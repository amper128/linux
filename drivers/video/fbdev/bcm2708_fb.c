/*
 *  linux/drivers/video/bcm2708_fb.c
 *
 * Copyright (C) 2010 Broadcom
 * Copyright (C) 2018 Raspberry Pi (Trading) Ltd
 *
 * This file is subject to the terms and conditions of the GNU General Public
 * License.  See the file COPYING in the main directory of this archive
 * for more details.
 *
 * Broadcom simple framebuffer driver
 *
 * This file is derived from cirrusfb.c
 * Copyright 1999-2001 Jeff Garzik <jgarzik@pobox.com>
 *
 */

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/slab.h>
#include <linux/mm.h>
#include <linux/fb.h>
#include <linux/init.h>
#include <linux/interrupt.h>
#include <linux/ioport.h>
#include <linux/list.h>
#include <linux/platform_data/dma-bcm2708.h>
#include <linux/platform_device.h>
#include <linux/clk.h>
#include <linux/printk.h>
#include <linux/console.h>
#include <linux/debugfs.h>
#include <asm/sizes.h>
#include <linux/uaccess.h>
#include <linux/io.h>
#include <linux/dma-mapping.h>
#include <linux/cred.h>
#include <soc/bcm2835/raspberrypi-firmware.h>

//#define BCM2708_FB_DEBUG
#define MODULE_NAME "bcm2708_fb"


#ifdef BCM2708_FB_DEBUG
#define print_debug(fmt, ...) pr_debug("%s:%s:%d: "fmt, \
			MODULE_NAME, __func__, __LINE__, ##__VA_ARGS__)
#else
#define print_debug(fmt, ...)
#endif

/* This is limited to 16 characters when displayed by X startup */
static const char *bcm2708_name = "BCM2708 FB";

#define DRIVER_NAME "bcm2708_fb"

static int fbwidth = 800;	/* module parameter */
static int fbheight = 480;	/* module parameter */
static int fbdepth = 32;	/* module parameter */
static int fbswap;		/* module parameter */

static u32 dma_busy_wait_threshold = 1<<15;
module_param(dma_busy_wait_threshold, int, 0644);
MODULE_PARM_DESC(dma_busy_wait_threshold, "Busy-wait for DMA completion below this area");

struct fb_alloc_tags {
	struct rpi_firmware_property_tag_header tag1;
	u32 xres, yres;
	struct rpi_firmware_property_tag_header tag2;
	u32 xres_virtual, yres_virtual;
	struct rpi_firmware_property_tag_header tag3;
	u32 bpp;
	struct rpi_firmware_property_tag_header tag4;
	u32 xoffset, yoffset;
	struct rpi_firmware_property_tag_header tag5;
	u32 base, screen_size;
	struct rpi_firmware_property_tag_header tag6;
	u32 pitch;
};

struct bcm2708_fb_stats {
	struct debugfs_regset32 regset;
	u32 dma_copies;
	u32 dma_irqs;
};

struct vc4_display_settings_t {
	u32 display_num;
	u32 width;
	u32 height;
	u32 pitch;
	u32 depth;
	u32 virtual_width;
	u32 virtual_height;
	u32 virtual_width_offset;
	u32 virtual_height_offset;
	unsigned long fb_bus_address;
};

/* One static instance of the DMA parameters, since they are used by all
 * bcm FB's
 */
static int dma_chan;
static int dma_irq;
static void __iomem *dma_chan_base;
static wait_queue_head_t dma_waitq;
static struct bcm2708_fb_stats dma_stats;
static void *cb_base;	/* DMA control blocks */
static dma_addr_t cb_handle;

/* Reference count of number of FB's created. Needed so we can remove
 * debugfs root on last FB remove
 */
static int instance_count;

struct bcm2708_fb {
	struct fb_info fb;
	struct platform_device *dev;
	struct rpi_firmware *fw;
	u32 cmap[16];
	u32 gpu_cmap[256];
	struct dentry *debugfs_dir;
	struct dentry *debugfs_subdir;
	unsigned long fb_bus_address;
	struct { u32 base, length; } gpu;
	struct vc4_display_settings_t display_settings;
	struct debugfs_regset32 screeninfo_regset;
};

#define to_bcm2708(info)	container_of(info, struct bcm2708_fb, fb)

static void bcm2708_fb_debugfs_deinit(struct bcm2708_fb *fb)
{
	debugfs_remove_recursive(fb->debugfs_subdir);
	fb->debugfs_subdir = NULL;

	instance_count--;

	if (!instance_count) {
		debugfs_remove_recursive(fb->debugfs_dir);
		fb->debugfs_dir = NULL;
	}
}

static int bcm2708_fb_debugfs_init(struct bcm2708_fb *fb)
{
char buf[3];

	static struct debugfs_reg32 stats_registers[] = {
	{"dma_copies", offsetof(struct bcm2708_fb_stats, dma_copies)},
	{"dma_irqs",   offsetof(struct bcm2708_fb_stats, dma_irqs)},
	};

	static struct debugfs_reg32 screeninfo[] = {
	{"width",	 offsetof(struct fb_var_screeninfo, xres)},
	{"height",	 offsetof(struct fb_var_screeninfo, yres)},
	{"bpp",		 offsetof(struct fb_var_screeninfo, bits_per_pixel)},
	{"xres_virtual", offsetof(struct fb_var_screeninfo, xres_virtual)},
	{"yres_virtual", offsetof(struct fb_var_screeninfo, yres_virtual)},
	{"xoffset",	 offsetof(struct fb_var_screeninfo, xoffset)},
	{"yoffset",	 offsetof(struct fb_var_screeninfo, yoffset)},
	};

	fb->debugfs_dir = debugfs_lookup(DRIVER_NAME, NULL);

	if (!fb->debugfs_dir)
		fb->debugfs_dir = debugfs_create_dir(DRIVER_NAME, NULL);

	if (!fb->debugfs_dir) {
		dev_warn(fb->fb.dev, "%s: could not create debugfs folder\n",
			__func__);
		return -EFAULT;
	}

	snprintf(buf, sizeof(buf), "%d", fb->display_settings.display_num);

	fb->debugfs_subdir = debugfs_create_dir(buf, fb->debugfs_dir);

	if (!fb->debugfs_subdir) {
		dev_warn(fb->fb.dev, "%s: could not create debugfs entry %d\n",
			__func__, fb->display_settings.display_num);
		return -EFAULT;
	}

	dma_stats.regset.regs = stats_registers;
	dma_stats.regset.nregs = ARRAY_SIZE(stats_registers);
	dma_stats.regset.base = &dma_stats;

	if (!debugfs_create_regset32(
		"dma_stats", 0444, fb->debugfs_subdir, &dma_stats.regset)) {
		dev_warn(fb->fb.dev, "%s: could not create statistics registers\n",
			__func__);
		goto fail;
	}

	fb->screeninfo_regset.regs = screeninfo;
	fb->screeninfo_regset.nregs = ARRAY_SIZE(screeninfo);
	fb->screeninfo_regset.base = &fb->fb.var;

	if (!debugfs_create_regset32(
		"screeninfo", 0444, fb->debugfs_subdir,
		&fb->screeninfo_regset)) {
		dev_warn(fb->fb.dev,
			"%s: could not create dimensions registers\n",
			__func__);
		goto fail;
	}

	instance_count++;

	return 0;

fail:
	bcm2708_fb_debugfs_deinit(fb);
	return -EFAULT;
}

static int bcm2708_fb_set_bitfields(struct fb_var_screeninfo *var)
{
	int ret = 0;

	memset(&var->transp, 0, sizeof(var->transp));

	var->red.msb_right = 0;
	var->green.msb_right = 0;
	var->blue.msb_right = 0;

	switch (var->bits_per_pixel) {
	case 1:
	case 2:
	case 4:
	case 8:
		var->red.length = var->bits_per_pixel;
		var->red.offset = 0;
		var->green.length = var->bits_per_pixel;
		var->green.offset = 0;
		var->blue.length = var->bits_per_pixel;
		var->blue.offset = 0;
		break;
	case 16:
		var->red.length = 5;
		var->blue.length = 5;
		/*
		 * Green length can be 5 or 6 depending whether
		 * we're operating in RGB555 or RGB565 mode.
		 */
		if (var->green.length != 5 && var->green.length != 6)
			var->green.length = 6;
		break;
	case 24:
		var->red.length = 8;
		var->blue.length = 8;
		var->green.length = 8;
		break;
	case 32:
		var->red.length = 8;
		var->green.length = 8;
		var->blue.length = 8;
		var->transp.length = 8;
		break;
	default:
		ret = -EINVAL;
		break;
	}

	/*
	 * >= 16bpp displays have separate colour component bitfields
	 * encoded in the pixel data.  Calculate their position from
	 * the bitfield length defined above.
	 */
	if (ret == 0 && var->bits_per_pixel >= 24 && fbswap) {
		var->blue.offset = 0;
		var->green.offset = var->blue.offset + var->blue.length;
		var->red.offset = var->green.offset + var->green.length;
		var->transp.offset = var->red.offset + var->red.length;
	} else if (ret == 0 && var->bits_per_pixel >= 24) {
		var->red.offset = 0;
		var->green.offset = var->red.offset + var->red.length;
		var->blue.offset = var->green.offset + var->green.length;
		var->transp.offset = var->blue.offset + var->blue.length;
	} else if (ret == 0 && var->bits_per_pixel >= 16) {
		var->blue.offset = 0;
		var->green.offset = var->blue.offset + var->blue.length;
		var->red.offset = var->green.offset + var->green.length;
		var->transp.offset = var->red.offset + var->red.length;
	}

	return ret;
}

static int bcm2708_fb_check_var(struct fb_var_screeninfo *var,
				struct fb_info *info)
{
	/* info input, var output */
	print_debug("%s(%p) %dx%d (%dx%d), %d, %d\n",
		__func__,
		info,
		info->var.xres, info->var.yres, info->var.xres_virtual,
		info->var.yres_virtual, (int)info->screen_size,
		info->var.bits_per_pixel);
	print_debug("%s(%p) %dx%d (%dx%d), %d\n", __func__, var,
		var->xres, var->yres, var->xres_virtual, var->yres_virtual,
		var->bits_per_pixel);

	if (!var->bits_per_pixel)
		var->bits_per_pixel = 16;

	if (bcm2708_fb_set_bitfields(var) != 0) {
		pr_err("%s: invalid bits_per_pixel %d\n", __func__,
		     var->bits_per_pixel);
		return -EINVAL;
	}


	if (var->xres_virtual < var->xres)
		var->xres_virtual = var->xres;
	/* use highest possible virtual resolution */
	if (var->yres_virtual == -1) {
		var->yres_virtual = 480;

		pr_err("%s: virtual resolution set to maximum of %dx%d\n",
		     __func__, var->xres_virtual, var->yres_virtual);
	}
	if (var->yres_virtual < var->yres)
		var->yres_virtual = var->yres;

	if (var->xoffset < 0)
		var->xoffset = 0;
	if (var->yoffset < 0)
		var->yoffset = 0;

	/* truncate xoffset and yoffset to maximum if too high */
	if (var->xoffset > var->xres_virtual - var->xres)
		var->xoffset = var->xres_virtual - var->xres - 1;
	if (var->yoffset > var->yres_virtual - var->yres)
		var->yoffset = var->yres_virtual - var->yres - 1;

	return 0;
}

static int bcm2708_fb_set_par(struct fb_info *info)
{
	static int old_firmware;

	struct bcm2708_fb *fb = to_bcm2708(info);
	struct fb_alloc_tags fbinfo = {
		.tag1 = { RPI_FIRMWARE_FRAMEBUFFER_SET_PHYSICAL_WIDTH_HEIGHT,
			8, 0, },
			.xres = info->var.xres,
			.yres = info->var.yres,
		.tag2 = { RPI_FIRMWARE_FRAMEBUFFER_SET_VIRTUAL_WIDTH_HEIGHT,
			8, 0, },
			.xres_virtual = info->var.xres_virtual,
			.yres_virtual = info->var.yres_virtual,
		.tag3 = { RPI_FIRMWARE_FRAMEBUFFER_SET_DEPTH, 4, 0 },
			.bpp = info->var.bits_per_pixel,
		.tag4 = { RPI_FIRMWARE_FRAMEBUFFER_SET_VIRTUAL_OFFSET, 8, 0 },
			.xoffset = info->var.xoffset,
			.yoffset = info->var.yoffset,
		.tag5 = { RPI_FIRMWARE_FRAMEBUFFER_ALLOCATE, 8, 0 },
			.base = 0,
			.screen_size = 0,
		.tag6 = { RPI_FIRMWARE_FRAMEBUFFER_GET_PITCH, 4, 0 },
			.pitch = 0,
	};
	int ret;
	u32 value = fb->display_settings.display_num;

	print_debug("%s(%p) %dx%d (%dx%d), %d, %d (display %d)\n", __func__, info,
		info->var.xres, info->var.yres, info->var.xres_virtual,
		info->var.yres_virtual, (int)info->screen_size,
		info->var.bits_per_pixel, value);

	/* Need to set the display number to act on first
	 * Cannot do it in the tag list because on older firmware the call
	 * will fail and stop the rest of the list being executed.
	 * We can ignore this call failing as the default at other end is 0
	 * Dont make the call if we suspect old firmware (ie call failed before)
	 */
	if (!old_firmware && rpi_firmware_property(fb->fw,
				RPI_FIRMWARE_FRAMEBUFFER_SET_DISPLAY_NUM,
				&value, sizeof(value)))	{
		dev_warn_once(info->device,
			"Set display number call failed. Old GPU firmware?");
		old_firmware = 1;
	}

	ret = rpi_firmware_property_list(fb->fw, &fbinfo, sizeof(fbinfo));
	if (ret) {
		dev_err(info->device,
			"Failed to allocate GPU framebuffer (%d)\n", ret);
		return ret;
	}

	if (info->var.bits_per_pixel <= 8)
		fb->fb.fix.visual = FB_VISUAL_PSEUDOCOLOR;
	else
		fb->fb.fix.visual = FB_VISUAL_TRUECOLOR;

	fb->fb.fix.line_length = fbinfo.pitch;
	fbinfo.base |= 0x40000000;
	fb->fb_bus_address = fbinfo.base;
	fbinfo.base &= ~0xc0000000;
	fb->fb.fix.smem_start = fbinfo.base;
	fb->fb.fix.smem_len = fbinfo.pitch * fbinfo.yres_virtual;
	fb->fb.screen_size = fbinfo.screen_size;
	if (fb->fb.screen_base)
		iounmap(fb->fb.screen_base);
	fb->fb.screen_base = ioremap_wc(fbinfo.base, fb->fb.screen_size);
	if (!fb->fb.screen_base) {
		/* the console may currently be locked */
		console_trylock();
		console_unlock();
		dev_err(info->device, "Failed to set screen_base\n");
		return -ENOMEM;
	}

	print_debug(
	  "%s: start = %p,%p width=%d, height=%d, bpp=%d, pitch=%d size=%d\n",
	  __func__,
	  (void *)fb->fb.screen_base, (void *)fb->fb_bus_address,
	  fbinfo.xres, fbinfo.yres, fbinfo.bpp,
	  fbinfo.pitch, (int)fb->fb.screen_size);

	return 0;
}

static inline u32 convert_bitfield(int val, struct fb_bitfield *bf)
{
	unsigned int mask = (1 << bf->length) - 1;

	return (val >> (16 - bf->length) & mask) << bf->offset;
}


static int bcm2708_fb_setcolreg(unsigned int regno, unsigned int red,
				unsigned int green, unsigned int blue,
				unsigned int transp, struct fb_info *info)
{
	struct bcm2708_fb *fb = to_bcm2708(info);

	if (fb->fb.var.bits_per_pixel <= 8) {
		if (regno < 256) {
			/* blue [23:16], green [15:8], red [7:0] */
			fb->gpu_cmap[regno] = ((red   >> 8) & 0xff) << 0 |
					((green >> 8) & 0xff) << 8 |
					((blue  >> 8) & 0xff) << 16;
		}
		/* Hack: we need to tell GPU the palette has changed, but
		 * currently bcm2708_fb_set_par takes noticeable time when
		 * called for every (256) colour
		 * So just call it for what looks like the last colour in a
		 * list for now.
		 */
		if (regno == 15 || regno == 255) {
			struct packet {
				u32 offset;
				u32 length;
				u32 cmap[256];
			} *packet;
			int ret;
			u32 dispnum = fb->display_settings.display_num;

			packet = kmalloc(sizeof(*packet), GFP_KERNEL);
			if (!packet)
				return -ENOMEM;
			packet->offset = 0;
			packet->length = regno + 1;
			memcpy(packet->cmap, fb->gpu_cmap,
				sizeof(packet->cmap));

			if (rpi_firmware_property(fb->fw,
						RPI_FIRMWARE_FRAMEBUFFER_SET_DISPLAY_NUM,
						&dispnum, sizeof(dispnum)))	{
				dev_warn_once(info->device,
					"Set display number call failed. Old GPU firmware?");
			}

			ret = rpi_firmware_property(fb->fw,
					RPI_FIRMWARE_FRAMEBUFFER_SET_PALETTE,
					packet,
					(2 + packet->length) * sizeof(u32));
			if (ret || packet->offset)
				dev_err(info->device,
					"Failed to set palette (%d,%u)\n",
					ret, packet->offset);
			kfree(packet);
		}
	} else if (regno < 16) {
		fb->cmap[regno] = convert_bitfield(transp, &fb->fb.var.transp) |
		convert_bitfield(blue, &fb->fb.var.blue) |
		convert_bitfield(green, &fb->fb.var.green) |
		convert_bitfield(red, &fb->fb.var.red);
	}
	return regno > 255;
}

static int bcm2708_fb_blank(int blank_mode, struct fb_info *info)
{
	struct bcm2708_fb *fb = to_bcm2708(info);
	u32 value;
	int ret;
	u32 dispnum = fb->display_settings.display_num;

	switch (blank_mode) {
	case FB_BLANK_UNBLANK:
		value = 0;
		break;
	case FB_BLANK_NORMAL:
	case FB_BLANK_VSYNC_SUSPEND:
	case FB_BLANK_HSYNC_SUSPEND:
	case FB_BLANK_POWERDOWN:
		value = 1;
		break;
	default:
		return -EINVAL;
	}

	if (rpi_firmware_property(fb->fw,
				RPI_FIRMWARE_FRAMEBUFFER_SET_DISPLAY_NUM,
				&dispnum, sizeof(dispnum))) {
		dev_warn_once(info->device,
			"Set display number call failed. Old GPU firmware?");
	}

	ret = rpi_firmware_property(fb->fw, RPI_FIRMWARE_FRAMEBUFFER_BLANK,
				&value, sizeof(value));

	if (ret)
		dev_err(info->device, "%s(%d) failed: %d\n", __func__,
			blank_mode, ret);

	return ret;
}

static int bcm2708_fb_pan_display(struct fb_var_screeninfo *var,
				  struct fb_info *info)
{
	s32 result;

	info->var.xoffset = var->xoffset;
	info->var.yoffset = var->yoffset;
	result = bcm2708_fb_set_par(info);
	if (result != 0)
		pr_err("%s(%d,%d) returns=%d\n", __func__,
			var->xoffset, var->yoffset, result);
	return result;
}

static void dma_memcpy(struct bcm2708_fb *fb, dma_addr_t dst, dma_addr_t src,
		       int size)
{
	struct bcm2708_dma_cb *cb = cb_base;
	int burst_size = (dma_chan == 0) ? 8 : 2;

	cb->info = BCM2708_DMA_BURST(burst_size) | BCM2708_DMA_S_WIDTH |
		BCM2708_DMA_S_INC | BCM2708_DMA_D_WIDTH |
		BCM2708_DMA_D_INC;
	cb->dst = dst;
	cb->src = src;
	cb->length = size;
	cb->stride = 0;
	cb->pad[0] = 0;
	cb->pad[1] = 0;
	cb->next = 0;

	/* Wait for DMA to become free in case being used by another FB. */
	while (bcm_dma_is_busy(dma_chan_base)) {
		wait_event_interruptible(
			dma_waitq,
			!bcm_dma_is_busy(dma_chan_base));
	}

	if (size < dma_busy_wait_threshold) {
		bcm_dma_start(dma_chan_base, cb_handle);
		bcm_dma_wait_idle(dma_chan_base);
	} else {
		void __iomem *local_dma_chan = dma_chan_base;

		cb->info |= BCM2708_DMA_INT_EN;
		bcm_dma_start(dma_chan_base, cb_handle);
		while (bcm_dma_is_busy(local_dma_chan)) {
			wait_event_interruptible(
				dma_waitq,
				!bcm_dma_is_busy(local_dma_chan));
		}
		dma_stats.dma_irqs++;
	}
	dma_stats.dma_copies++;
}

/* address with no aliases */
#define INTALIAS_NORMAL(x) ((x)&~0xc0000000)
/* cache coherent but non-allocating in L1 and L2 */
#define INTALIAS_L1L2_NONALLOCATING(x) (((x)&~0xc0000000)|0x80000000)

static long vc_mem_copy(struct bcm2708_fb *fb, unsigned long arg)
{
	struct fb_dmacopy ioparam;
	size_t size = PAGE_SIZE;
	u32 *buf = NULL;
	dma_addr_t bus_addr;
	long rc = 0;
	size_t offset;

	/* restrict this to root user */
	if (!uid_eq(current_euid(), GLOBAL_ROOT_UID)) {
		rc = -EFAULT;
		goto out;
	}

	/* Get the parameter data.
	 */
	if (copy_from_user
	(&ioparam, (void *)arg, sizeof(ioparam)) != 0) {
		pr_err("[%s]: failed to copy-from-user\n",
				__func__);
		rc = -EFAULT;
		goto out;
	}

	if (fb->gpu.base == 0 || fb->gpu.length == 0) {
		pr_err("[%s]: Unable to determine gpu memory (%x,%x)\n",
			__func__, fb->gpu.base, fb->gpu.length);
		return -EFAULT;
	}

	if (INTALIAS_NORMAL(ioparam.src) < fb->gpu.base ||
		INTALIAS_NORMAL(ioparam.src) >= fb->gpu.base + fb->gpu.length) {
		pr_err("[%s]: Invalid memory access %x (%x-%x)", __func__,
			INTALIAS_NORMAL(ioparam.src), fb->gpu.base,
			fb->gpu.base + fb->gpu.length);
		return -EFAULT;
	}

	buf = dma_alloc_coherent(fb->fb.device, PAGE_ALIGN(size), &bus_addr,
				GFP_ATOMIC);
	if (!buf) {
		pr_err("[%s]: failed to dma_alloc_coherent(%d)\n",
				__func__, size);
		rc = -ENOMEM;
		goto out;
	}

	for (offset = 0; offset < ioparam.length; offset += size) {
		size_t remaining = ioparam.length - offset;
		size_t s = min(size, remaining);
		unsigned char *p = (unsigned char *)ioparam.src + offset;
		unsigned char *q = (unsigned char *)ioparam.dst + offset;

		dma_memcpy(fb, bus_addr,
			   INTALIAS_L1L2_NONALLOCATING((dma_addr_t)p), size);
		if (copy_to_user(q, buf, s) != 0) {
			pr_err("[%s]: failed to copy-to-user\n",
					__func__);
			rc = -EFAULT;
			goto out;
		}
	}
out:
	if (buf)
		dma_free_coherent(fb->fb.device, PAGE_ALIGN(size), buf,
				  bus_addr);
	return rc;
}

static int bcm2708_ioctl(struct fb_info *info, unsigned int cmd,
			 unsigned long arg)
{
	struct bcm2708_fb *fb = to_bcm2708(info);
	u32 dummy = 0;
	int ret;
	u32 dispnum = fb->display_settings.display_num;

	switch (cmd) {
	case FBIO_WAITFORVSYNC:
	{
		if (rpi_firmware_property(fb->fw,
					RPI_FIRMWARE_FRAMEBUFFER_SET_DISPLAY_NUM,
					&dispnum , sizeof(dispnum ))) {
			dev_warn_once(info->device,
				"Set display number call failed. Old GPU firmware?");
		}

		ret = rpi_firmware_property(fb->fw,
					RPI_FIRMWARE_FRAMEBUFFER_SET_VSYNC,
					&dummy, sizeof(dummy));
		break;
	}
	case FBIODMACOPY:
		ret = vc_mem_copy(fb, arg);
		break;
	default:
		dev_dbg(info->device, "Unknown ioctl 0x%x\n", cmd);
		return -ENOTTY;
	}

	if (ret)
		dev_err(info->device, "ioctl 0x%x failed (%d)\n", cmd, ret);

	return ret;
}
static void bcm2708_fb_fillrect(struct fb_info *info,
				const struct fb_fillrect *rect)
{
	cfb_fillrect(info, rect);
}

/* A helper function for configuring dma control block */
static void set_dma_cb(struct bcm2708_dma_cb *cb,
		int        burst_size,
		dma_addr_t dst,
		int        dst_stride,
		dma_addr_t src,
		int        src_stride,
		int        w,
		int        h)
{
	cb->info = BCM2708_DMA_BURST(burst_size) | BCM2708_DMA_S_WIDTH |
		BCM2708_DMA_S_INC | BCM2708_DMA_D_WIDTH |
		BCM2708_DMA_D_INC | BCM2708_DMA_TDMODE;
	cb->dst = dst;
	cb->src = src;
	/*
	 * This is not really obvious from the DMA documentation,
	 * but the top 16 bits must be programmmed to "height -1"
	 * and not "height" in 2D mode.
	 */
	cb->length = ((h - 1) << 16) | w;
	cb->stride = ((dst_stride - w) << 16) | (u16)(src_stride - w);
	cb->pad[0] = 0;
	cb->pad[1] = 0;
}

static void bcm2708_fb_copyarea(struct fb_info *info,
				const struct fb_copyarea *region)
{
	struct bcm2708_fb *fb = to_bcm2708(info);
	struct bcm2708_dma_cb *cb = cb_base;
	int bytes_per_pixel = (info->var.bits_per_pixel + 7) >> 3;

	/* Channel 0 supports larger bursts and is a bit faster */
	int burst_size = (dma_chan == 0) ? 8 : 2;
	int pixels = region->width * region->height;

	/* If DMA is currently in use (ie being used on another FB), then
	 * rather than wait for it to finish, just use the cfb_copyarea
	 */
	if (bcm_dma_is_busy(dma_chan_base) ||
	    bytes_per_pixel > 4 ||
	    info->var.xres * info->var.yres > 1920 * 1200 ||
	    region->width <= 0 || region->width > info->var.xres ||
	    region->height <= 0 || region->height > info->var.yres ||
	    region->sx < 0 || region->sx >= info->var.xres ||
	    region->sy < 0 || region->sy >= info->var.yres ||
	    region->dx < 0 || region->dx >= info->var.xres ||
	    region->dy < 0 || region->dy >= info->var.yres ||
	    region->sx + region->width > info->var.xres ||
	    region->dx + region->width > info->var.xres ||
	    region->sy + region->height > info->var.yres ||
	    region->dy + region->height > info->var.yres) {

		cfb_copyarea(info, region);
		return;
	}

	if (region->dy == region->sy && region->dx > region->sx) {
		/*
		 * A difficult case of overlapped copy. Because DMA can't
		 * copy individual scanlines in backwards direction, we need
		 * two-pass processing. We do it by programming a chain of dma
		 * control blocks in the first 16K part of the buffer and use
		 * the remaining 48K as the intermediate temporary scratch
		 * buffer. The buffer size is sufficient to handle up to
		 * 1920x1200 resolution at 32bpp pixel depth.
		 */
		int y;
		dma_addr_t control_block_pa = cb_handle;
		dma_addr_t scratchbuf = cb_handle + 16 * 1024;
		int scanline_size = bytes_per_pixel * region->width;
		int scanlines_per_cb = (64 * 1024 - 16 * 1024) / scanline_size;

		for (y = 0; y < region->height; y += scanlines_per_cb) {
			dma_addr_t src =
				fb->fb_bus_address +
				bytes_per_pixel * region->sx +
				(region->sy + y) * fb->fb.fix.line_length;
			dma_addr_t dst =
				fb->fb_bus_address +
				bytes_per_pixel * region->dx +
				(region->dy + y) * fb->fb.fix.line_length;

			if (region->height - y < scanlines_per_cb)
				scanlines_per_cb = region->height - y;

			set_dma_cb(cb, burst_size, scratchbuf, scanline_size,
				src, fb->fb.fix.line_length,
				scanline_size, scanlines_per_cb);
			control_block_pa += sizeof(struct bcm2708_dma_cb);
			cb->next = control_block_pa;
			cb++;

			set_dma_cb(cb, burst_size, dst, fb->fb.fix.line_length,
				scratchbuf, scanline_size,
				scanline_size, scanlines_per_cb);
			control_block_pa += sizeof(struct bcm2708_dma_cb);
			cb->next = control_block_pa;
			cb++;
		}
		/* move the pointer back to the last dma control block */
		cb--;
	} else {
		/* A single dma control block is enough. */
		int sy, dy, stride;

		if (region->dy <= region->sy) {
			/* processing from top to bottom */
			dy = region->dy;
			sy = region->sy;
			stride = fb->fb.fix.line_length;
		} else {
			/* processing from bottom to top */
			dy = region->dy + region->height - 1;
			sy = region->sy + region->height - 1;
			stride = -fb->fb.fix.line_length;
		}
		set_dma_cb(cb, burst_size,
			fb->fb_bus_address + dy * fb->fb.fix.line_length +
						bytes_per_pixel * region->dx,
			stride,
			fb->fb_bus_address + sy * fb->fb.fix.line_length +
						bytes_per_pixel * region->sx,
			stride,
			region->width * bytes_per_pixel,
			region->height);
	}

	/* end of dma control blocks chain */
	cb->next = 0;

	if (pixels < dma_busy_wait_threshold) {
		bcm_dma_start(dma_chan_base, cb_handle);
		bcm_dma_wait_idle(dma_chan_base);
	} else {
		void __iomem *local_dma_chan = dma_chan_base;

		cb->info |= BCM2708_DMA_INT_EN;
		bcm_dma_start(dma_chan_base, cb_handle);
		while (bcm_dma_is_busy(local_dma_chan)) {
			wait_event_interruptible(
				dma_waitq,
				!bcm_dma_is_busy(local_dma_chan));
		}
		dma_stats.dma_irqs++;
	}
	dma_stats.dma_copies++;
}

static void bcm2708_fb_imageblit(struct fb_info *info,
				const struct fb_image *image)
{
	cfb_imageblit(info, image);
}

static irqreturn_t bcm2708_fb_dma_irq(int irq, void *cxt)
{
//	struct bcm2708_fb *fb = cxt;

	/* FIXME: should read status register to check if this is
	 * actually interrupting us or not, in case this interrupt
	 * ever becomes shared amongst several DMA channels
	 *
	 * readl(dma_chan_base + BCM2708_DMA_CS) & BCM2708_DMA_IRQ;
	 */

	/* acknowledge the interrupt */
	writel(BCM2708_DMA_INT, dma_chan_base + BCM2708_DMA_CS);

	wake_up(&dma_waitq);
	return IRQ_HANDLED;
}

static struct fb_ops bcm2708_fb_ops = {
	.owner = THIS_MODULE,
	.fb_check_var = bcm2708_fb_check_var,
	.fb_set_par = bcm2708_fb_set_par,
	.fb_setcolreg = bcm2708_fb_setcolreg,
	.fb_blank = bcm2708_fb_blank,
	.fb_fillrect = bcm2708_fb_fillrect,
	.fb_copyarea = bcm2708_fb_copyarea,
	.fb_imageblit = bcm2708_fb_imageblit,
	.fb_pan_display = bcm2708_fb_pan_display,
	.fb_ioctl = bcm2708_ioctl,
};

static int bcm2708_fb_register(struct bcm2708_fb *fb)
{
	int ret;

	fb->fb.fbops = &bcm2708_fb_ops;
	fb->fb.flags = FBINFO_FLAG_DEFAULT | FBINFO_HWACCEL_COPYAREA;
	fb->fb.pseudo_palette = fb->cmap;

	strncpy(fb->fb.fix.id, bcm2708_name, sizeof(fb->fb.fix.id));
	fb->fb.fix.type = FB_TYPE_PACKED_PIXELS;
	fb->fb.fix.type_aux = 0;
	fb->fb.fix.xpanstep = 1;
	fb->fb.fix.ypanstep = 1;
	fb->fb.fix.ywrapstep = 0;
	fb->fb.fix.accel = FB_ACCEL_NONE;

	/* If we have data from the VC4 on FB's, use that, otherwise use the
	 * module parameters
	 */
	if (fb->display_settings.width) {
		fb->fb.var.xres = fb->display_settings.width;
		fb->fb.var.yres = fb->display_settings.height;
		fb->fb.var.xres_virtual = fb->fb.var.xres;
		fb->fb.var.yres_virtual = fb->fb.var.yres;
		fb->fb.var.bits_per_pixel = fb->display_settings.depth;
	} else {
		fb->fb.var.xres = fbwidth;
		fb->fb.var.yres = fbheight;
		fb->fb.var.xres_virtual = fbwidth;
		fb->fb.var.yres_virtual = fbheight;
		fb->fb.var.bits_per_pixel = fbdepth;
	}

	fb->fb.var.vmode = FB_VMODE_NONINTERLACED;
	fb->fb.var.activate = FB_ACTIVATE_NOW;
	fb->fb.var.nonstd = 0;
	fb->fb.var.height = -1;		/* height of picture in mm    */
	fb->fb.var.width = -1;		/* width of picture in mm    */
	fb->fb.var.accel_flags = 0;

	fb->fb.monspecs.hfmin = 0;
	fb->fb.monspecs.hfmax = 100000;
	fb->fb.monspecs.vfmin = 0;
	fb->fb.monspecs.vfmax = 400;
	fb->fb.monspecs.dclkmin = 1000000;
	fb->fb.monspecs.dclkmax = 100000000;

	bcm2708_fb_set_bitfields(&fb->fb.var);
	init_waitqueue_head(&dma_waitq);

	/*
	 * Allocate colourmap.
	 */
	fb_set_var(&fb->fb, &fb->fb.var);

	ret = bcm2708_fb_set_par(&fb->fb);

	if (ret)
		return ret;

	print_debug("BCM2708FB: registering framebuffer for display %d (%dx%d@%d) (swap R/B%d)\n",
		    fb->display_settings.display_num,
		    fb->fb.var.xres,
		    fb->fb.var.yres,
		    fb->fb.var.bits_per_pixel,
		    fbswap);

	ret = register_framebuffer(&fb->fb);

	if (ret == 0)
		goto out;

	dev_warn(fb->fb.dev, "Unable to register framebuffer (%d)\n", ret);
out:
	return ret;
}

static int bcm2708_fb_probe(struct platform_device *dev)
{
	struct device_node *fw_np;
	struct rpi_firmware *fw;
	struct bcm2708_fb *fb;
	int ret, i, old = 0;
	u32 num_displays;

	fw_np = of_parse_phandle(dev->dev.of_node, "firmware", 0);
/* Remove comment when booting without Device Tree is no longer supported
 *	if (!fw_np) {
 *		dev_err(&dev->dev, "Missing firmware node\n");
 *		return -ENOENT;
 *	}
 */
	fw = rpi_firmware_get(fw_np);
	if (!fw)
		return -EPROBE_DEFER;

	ret = rpi_firmware_property(fw,
			RPI_FIRMWARE_FRAMEBUFFER_GET_NUM_DISPLAYS,
			&num_displays, sizeof(u32));

	/* If we fail to get the number of displays, or it returns 0, then
	 * assume old firmware that doesnt have the mailbox call, so just
	 * just set one display
	 */
	if (ret || num_displays == 0) {
		num_displays = 1;
		old = 1;
		dev_err(&dev->dev,
			"Unable to determine number of FB's. Assuming 1\n");
		print_debug("Unable to determine number of FB's. Assuming 1\n");
		ret = 0;
	}

	/* Set up the DMA information. Note we have just one set of DMA for
	 * all the FB's, and simply ensure that the DMA only gets called
	 * by one at a time
	 */
	if (!cb_base) {
		cb_base = dma_alloc_writecombine(&dev->dev, SZ_64K,
					&cb_handle, GFP_KERNEL);
		if (!cb_base) {
			dev_err(&dev->dev, "cannot allocate DMA CBs\n");
			ret = -ENOMEM;
			goto free_fb;
		}

		dev_info(&dev->dev,
			"allocated DMA memory %08x\n",
			cb_handle);

		ret = bcm_dma_chan_alloc(BCM_DMA_FEATURE_BULK,
					 &dma_chan_base, &dma_irq);
		if (ret < 0) {
			dev_err(&dev->dev, "couldn't allocate a DMA channel\n");
			goto free_cb;
		}
		dma_chan = ret;

		ret = request_irq(dma_irq, bcm2708_fb_dma_irq,
				  0, "bcm2708_fb dma", fb);
		if (ret) {
			dev_err(&dev->dev,
				"failed to request DMA irq\n");
			goto free_dma_chan;
		}
		dev_info(&dev->dev,
			"allocated DMA channel %d @ %p\n",
			dma_chan, dma_chan_base);
	}

	for (i = 0; i < num_displays && ret == 0; i++) {
		fb = kzalloc(sizeof(struct bcm2708_fb), GFP_KERNEL);

		if (!fb) {
			ret = -ENOMEM;
			goto free_region;
		}

		fb->fw = fw;
		fb->display_settings.display_num = i;

		bcm2708_fb_debugfs_init(fb);

		fb->dev = dev;
		fb->fb.device = &dev->dev;
		/* failure here isn't fatal, but we'll fail in vc_mem_copy
		 * if fb->gpu is not valid
		 */
		rpi_firmware_property(fb->fw,
				RPI_FIRMWARE_GET_VC_MEMORY,
				&fb->gpu, sizeof(fb->gpu));

		if (old) {
			memset(&fb->display_settings, 0,
				sizeof(fb->display_settings));
		} else {
			fb->display_settings.display_num = i;
			ret = rpi_firmware_property(fb->fw,
				RPI_FIRMWARE_FRAMEBUFFER_GET_DISPLAY_SETTINGS,
				&fb->display_settings,
				GET_DISPLAY_SETTINGS_PAYLOAD_SIZE);
		}

		ret = bcm2708_fb_register(fb);

		if (ret == 0) {
			platform_set_drvdata(dev, fb);
			print_debug("Registered framebuffer for display %d, size %d,%d\n",
					fb->display_settings.display_num,
					fb->fb.var.xres,
					fb->fb.var.yres);
		} else
			goto free_dma_chan; // TODO. Better cleanup
	}

	return ret;

free_dma_chan:
	bcm_dma_chan_free(dma_chan);
free_cb:
	dma_free_writecombine(&dev->dev, SZ_64K, cb_base, cb_handle);
free_fb:
	kfree(fb);
free_region:
	dev_err(&dev->dev, "probe failed, err %d\n", ret);

	return ret;
}

static int bcm2708_fb_remove(struct platform_device *dev)
{
	struct bcm2708_fb *fb = platform_get_drvdata(dev);

	platform_set_drvdata(dev, NULL);

	if (fb->fb.screen_base)
		iounmap(fb->fb.screen_base);
	unregister_framebuffer(&fb->fb);

	dma_free_writecombine(&dev->dev, SZ_64K, cb_base, cb_handle);
	bcm_dma_chan_free(dma_chan);

	bcm2708_fb_debugfs_deinit(fb);

	free_irq(dma_irq, fb);

	kfree(fb);

	return 0;
}

static const struct of_device_id bcm2708_fb_of_match_table[] = {
	{ .compatible = "brcm,bcm2708-fb", },
	{},
};
MODULE_DEVICE_TABLE(of, bcm2708_fb_of_match_table);

static struct platform_driver bcm2708_fb_driver = {
	.probe = bcm2708_fb_probe,
	.remove = bcm2708_fb_remove,
	.driver = {
		.name = DRIVER_NAME,
		.owner = THIS_MODULE,
		.of_match_table = bcm2708_fb_of_match_table,
		},
};

static int __init bcm2708_fb_init(void)
{
	return platform_driver_register(&bcm2708_fb_driver);
}

module_init(bcm2708_fb_init);

static void __exit bcm2708_fb_exit(void)
{
	platform_driver_unregister(&bcm2708_fb_driver);
}

module_exit(bcm2708_fb_exit);

module_param(fbwidth, int, 0644);
module_param(fbheight, int, 0644);
module_param(fbdepth, int, 0644);
module_param(fbswap, int, 0644);

MODULE_DESCRIPTION("BCM2708 framebuffer driver");
MODULE_LICENSE("GPL");

MODULE_PARM_DESC(fbwidth, "Width of ARM Framebuffer");
MODULE_PARM_DESC(fbheight, "Height of ARM Framebuffer");
MODULE_PARM_DESC(fbdepth, "Bit depth of ARM Framebuffer");
MODULE_PARM_DESC(fbswap, "Swap order of red and blue in 24 and 32 bit modes");
