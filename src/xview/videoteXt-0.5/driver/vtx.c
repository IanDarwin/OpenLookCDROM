/*
 *  vtx.c
 *
 *  Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 *
 *  This is a loadable character-device-driver for videotext-interfaces
 *  (aka teletext). Please check the Makefile for a list of supported
 *  interfaces.
 */

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#define __KERNEL__
#define MODULE
#include <linux/autoconf.h>
#include <linux/module.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/errno.h>
#include <linux/delay.h>
#include <asm/io.h>
#include <asm/segment.h>
#include <stdarg.h>
#include "vtx.h"

#if (defined IF_HOMEBREW + defined IF_CT + defined IF_CTSERIAL + defined IF_VTX2000 \
    + defined IF_PCVT7000 + defined IF_SATCOM) != 1
#error You must choose exactly one interface in the Makefile
#endif

#define VTX_VER_MAJ 1
#define VTX_VER_MIN 4
#define VTX_NAME "videotext"

static int disp_mode = DISPOFF;
static const int disp_modes[8][3] = {
  { 0x46, 0x03, 0x03 },	/* DISPOFF */
  { 0x46, 0xcc, 0xcc },	/* DISPNORM */
  { 0x44, 0x0f, 0x0f },	/* DISPTRANS */
  { 0x46, 0xcc, 0x46 },	/* DISPINS */
  { 0x44, 0x03, 0x03 },	/* DISPOFF, interlaced */
  { 0x44, 0xcc, 0xcc },	/* DISPNORM, interlaced */
  { 0x44, 0x0f, 0x0f },	/* DISPTRANS, interlaced */
  { 0x44, 0xcc, 0x46 }	/* DISPINS, interlaced */
};
static const int io_delay[2][3] = {
  { 3, 5, 7 },		/* Slow speed */
  { 2, 3, 5 },		/* Fast speed */
};
#define UDELAY(val) udelay(io_delay[!slow_if][val])
#define RESCHED \
        do { \
          if (need_resched) \
            schedule(); \
        } while (0)

/* I²C write/read-address of SAA 5246 */
#define CCTWR 34
#define CCTRD 35
#define NUMPAGES 4
#define NOACK_REPEAT 10
#define CLEAR_DELAY 5
#define I2C_TIMEOUT 300				/* open/close/SDA-check timeout in jiffies */
#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

static int is_searching[NUMPAGES];

/* These variables are settable when loading the driver (with modutils-1.1.67 & up) */
int io_base = VTX_DEFAULT_IO_BASE;
int major = VTX_DEFAULT_MAJOR;
#ifdef VTX_CHIPSET_IS_DAMN_SLOW
int slow_if = 1;
#else
int slow_if = 0;
#endif


#ifdef DEBUG_VTX
int debug = 0;
#define RETURN(val, str) \
        do { \
          if (debug > 1 || (debug == 1 && val)) { \
            printk("vtx: " str "\n"); \
          } \
          return (val); \
        } while (0)
#define RETURNx1(val, str, arg1) \
        do { \
          if (debug > 1 || (debug == 1 && val)) { \
            printk("vtx: " str "\n", arg1); \
          } \
          return (val); \
        } while (0)
#define NOTIFY(level, str) \
        do { \
          if (debug >= level) { \
            printk("vtx: " str "\n"); \
          } \
        } while (0)
#else
#define RETURN(val, str) return (val)
#define RETURNx1(val, str, arg1) return (val)
#define NOTIFY(level, str) 
#endif


#if defined IF_HOMEBREW || defined IF_PCVT7000
#define CCT_TYPE SAA5243
static int portval;
#endif

#ifdef IF_CT
#define CCT_TYPE SAA5246
#endif

#if defined IF_CTSERIAL || defined IF_VTX2000 || defined IF_SATCOM
#define CCT_TYPE SAA5246
static int portval;
#endif



/* Macros for setting/checking SDA/SCL lines:
 * set_sda(val): Set SDA-line to val (0/1)
 * set_scl(val): Set SCL-line to val (0/1)
 * get_sda(): Read state of SDA-line (0/1)
 * is_busy(), i2c_ctrl(val): Required only for i2c_reserve/release() (IF_HOMEBREW)
 */
#ifdef IF_HOMEBREW
#define set_sda(val) outb(portval = ((portval & ~1) | ((val) ? 0 : 1)), io_base)
#define set_scl(val) outb(portval = ((portval & ~2) | ((val) ? 0 : 2)), io_base)
#define get_sda() ((inb(io_base + 1) & 0x40) ? 0 : 1)
#define is_busy() ((inb(io_base + 1) & 0x10) ? 1 : 0)
#define i2c_ctrl(val) outb((inb(io_base + 2) & ~2) | (val ? 0 : 2), io_base + 2)
#endif

#ifdef IF_CT
#define set_sda(val) inb(io_base + (val ? 2 : 3))
#define set_scl(val) inb(io_base + (val ? 0 : 1))
#define get_sda() (inb(io_base + 2) & 1)
#endif

#ifdef IF_CTSERIAL
#define set_sda(val) outb(portval = ((portval & ~1) | ((val) ? 0 : 1)), io_base + 4)
#define set_scl(val) outb(portval = ((portval & ~2) | ((val) ? 0 : 2)), io_base + 4)
#define get_sda() ((inb(io_base + 6) & 0x80) ? 0 : 1)
#endif

#ifdef IF_VTX2000
#define set_sda(val) outb(portval = ((portval & ~2) | ((val) ? 0 : 2)), io_base + 1)
#define set_scl(val) outb(portval = ((portval & ~1) | ((val) ? 0 : 1)), io_base + 1)
#define get_sda() ((inb(io_base + 1) & 8) ? 1 : 0)
#endif

#ifdef IF_PCVT7000
#define set_sda(val) outb(portval = ((portval & ~1) | ((val) ? 1 : 0)), io_base)
#define set_scl(val) outb(portval = ((portval & ~2) | ((val) ? 2 : 0)), io_base)
#define get_sda() (inb(io_base) & 1)
#endif

#ifdef IF_SATCOM
#define set_sda(val) outb(portval = ((portval & ~1) | ((val) ? 1 : 0)), io_base + 4)
#define set_scl(val) outb(portval = ((portval & ~2) | ((val) ? 0 : 2)), io_base + 4)
#define get_sda() ((inb(io_base + 6) & 0x80) ? 0 : 1)
#endif


/* Reserve I²C-Bus. This routine is required only if you don't have a standalone VTX-decoder
 * but one that is built for example into a VCR. Then we have to disconnect the
 * VTX-decoder from the rest of the bus here.
 * Returns -1 if bus is busy all the time (or if it doesn't exist), 0 otherwise
 */
static int
reserve_bus(void) {
#ifdef IF_HOMEBREW
  int start;

  set_sda(1);
  set_scl(0);
  UDELAY(1);
  i2c_ctrl(0);
  start = jiffies;
  while ((jiffies < start + I2C_TIMEOUT) && !is_busy())		/* Wait for falling edge on */
    RESCHED;							/* busy line. This may take some */
  while ((jiffies < start + I2C_TIMEOUT) && is_busy())		/* seconds (depending on the */
    RESCHED;							/* timeout), so we better call */
  if (jiffies >= start + I2C_TIMEOUT)				/* the scheduler from time to */
    return -1;							/* time */
  i2c_ctrl(1);
  UDELAY(1);
  set_scl(1);
  UDELAY(0);
#endif
  return 0;
}


/* Release I²C-Bus. See comment for i2c_reserve() above
 * Returns -1 if bus is busy all the time (or if it doesn't exist), 0 otherwise
 */
static int
release_bus(void) {
#ifdef IF_HOMEBREW
  int start;

  set_sda(1);
  set_scl(1);
  i2c_ctrl(0);
  start = jiffies;
  while ((jiffies < start + I2C_TIMEOUT) && !is_busy())		/* Wait for falling edge on */
    RESCHED;							/* busy line. This may take some */
  while ((jiffies < start + I2C_TIMEOUT) && is_busy())		/* seconds (depending on the */
    RESCHED;							/* timeout), so we better call */
  if (jiffies >= start + I2C_TIMEOUT)				/* the scheduler from time to */
    return -1;							/* time */
  i2c_ctrl(1);
  UDELAY(0);
#endif
  return 0;
}


/* The following routines are not hardware-dependent and should be the same for all possible
 * interfaces
 */

/* Send I²C-start-sequence
 */
static void
i2c_start(void) {
  set_sda(1); set_scl(1); UDELAY(1);
  set_sda(0); UDELAY(1);
  set_scl(0); UDELAY(0);
}


/* Send I²C-end-sequence
 */
static void
i2c_end(void) {
  set_scl(0); set_sda(0); UDELAY(1);
  set_scl(1); UDELAY(1);
  set_sda(1); UDELAY(0);
}


/* Send val on I²C-bus. i2c_start must have been called before!
 * Returns -1 if I²C-device didn't send acknowledge, 0 otherwise
 */
static int
i2c_sendbyte(int val) {
  int count, ack;

  for (count = 7; count >= 0; count--) {
    set_scl(0);
    set_sda((val >> count) & 1);
    UDELAY(1);
    set_scl(1);
    UDELAY(1);
    set_scl(0);
    UDELAY(0);
  }
  set_sda(1);
  UDELAY(0);
  set_scl(1);
  UDELAY(1);
  ack = !get_sda();
  set_scl(0);
  UDELAY(0);
  return (ack ? 0 : -1);
}


/* Receive byte from I²C-bus. Send acknowledge afterwards if ack != 0
 * Returns byte received
 */
static int
i2c_getbyte(int ack) {
  int count, val;

  set_scl(0);
  set_sda(1);
  UDELAY(1);
  for (val = count = 0; count <= 7; count++) {
    set_scl(1);
    UDELAY(2);
    val = (val << 1) + get_sda();
    set_scl(0);
    UDELAY(1);
  }
  if (ack) {
    set_sda(0);
    UDELAY(1);
    set_scl(1);
    UDELAY(2);
    set_scl(0);
    UDELAY(0);
  } else {
    set_scl(1);
    UDELAY(2);
    set_scl(0);
    UDELAY(0);
  }
  return val;
}


/* Send arbitrary number of bytes to I²C-bus. Start & stop handshaking is done by this routine.
 * adr should be address of I²C-device, varargs-list of values to send must be terminated by -1
 * Returns -1 if I²C-device didn't send acknowledge, 0 otherwise
 */
static int
i2c_senddata(int adr, ...) {
  int val, loop;
  va_list argp;

  for (loop = 0; loop <= NOACK_REPEAT; loop++) {
    i2c_start();
    if (i2c_sendbyte(adr) < 0)
      goto loopend;
    va_start(argp, adr);
    while (!((val = va_arg(argp, int)) & ~0xff))
      if (i2c_sendbyte(val) < 0)
        goto loopend;
    va_end(argp);
    i2c_end();
    return 0;
loopend:
    i2c_end();
  }
  va_end(argp);
  return -1;
}


/* Send count number of bytes from buffer buf to I²C-device adr. Start & stop handshaking is
 * done by this routine. If get_fs is TRUE, data is read from user-space by get_fs_byte.
 * Returns -1 if I²C-device didn't send acknowledge, 0 otherwise
 */
static int
i2c_sendbuf(int adr, int reg, int count, byte_t *buf, int get_fs) {
  int pos, loop, val;

  for (loop = 0; loop <= NOACK_REPEAT; loop++) {
    i2c_start();
    if (i2c_sendbyte(adr))
      goto loopend;
    if (i2c_sendbyte(reg))
      goto loopend;
    for (pos = 0; pos < count; pos++) {
      if (get_fs)
        val = get_fs_byte(buf + pos);
      else
        val = buf[pos];
      if (i2c_sendbyte(val))
        goto loopend;
      RESCHED;
    }
    i2c_end();
    return 0;
loopend:
    i2c_end();
  }
  return -1;
}


/* Get count number of bytes from I²C-device at address adr, store them in buf. Start & stop
 * handshaking is done by this routine, ack will be sent after the last byte to inhibit further
 * sending of data. If put_fs is TRUE, data is written to user-space by put_fs_byte
 * Returns -1 if I²C-device didn't send acknowledge, 0 otherwise
 */
static int
i2c_getdata(int adr, int count, byte_t *buf, int put_fs) {
  int pos, loop;

  for (loop = 0; loop <= NOACK_REPEAT; loop++) {
    i2c_start();
    if (i2c_sendbyte(adr)) {
      i2c_end();
      continue;
    }
    for (pos = 0; pos < count; pos++) {
      if (put_fs)
        put_fs_byte(i2c_getbyte(pos < count - 1), buf + pos);
      else
        buf[pos] = i2c_getbyte(pos < count - 1);
      RESCHED;
    }
    i2c_end();
    return 0;
  }
  return -1;
}


/* Standard character-device-driver functions
 */

static int
vtx_ioctl(struct inode *inode, struct file *file, unsigned int cmd, unsigned long arg) {
  int err;

  NOTIFY(2, "ioctl");
  if (!arg)
    RETURN(-EINVAL, "ioctl: EINVAL (arg is NULL)");
  switch(cmd) {
    case VTXIOCGETINFO: {
      vtx_info_t info;
      
      info.version_major = VTX_VER_MAJ;
      info.version_minor = VTX_VER_MIN;
      info.numpages = NUMPAGES;
      info.cct_type = CCT_TYPE;
      if ((err = verify_area(VERIFY_WRITE, (void*)arg, sizeof(vtx_info_t))))
        RETURN(err, "VTXIOCGETINFO: EFAULT");
      memcpy_tofs((void*)arg, &info, sizeof(vtx_info_t));
      RETURN(0, "VTXIOCGETINFO: OK");
    }

    case VTXIOCCLRPAGE: {
      vtx_pagereq_t req;
      int start;
      
      if ((err = verify_area(VERIFY_READ, (void*)arg, sizeof(vtx_pagereq_t))))
        RETURN(err, "VTXIOCCLRPAGE: EFAULT");
      memcpy_fromfs(&req, (void*)arg, sizeof(vtx_pagereq_t));
      if (req.pgbuf < 0 || req.pgbuf >= NUMPAGES)
        RETURN(-EINVAL, "VTXIOCCLRPAGE: EINVAL");
      if (i2c_senddata(CCTWR, 8, req.pgbuf | 8, -1))
        RETURN(-EIO, "VTXIOCCLRPAGE: EIO (1)");
      start = jiffies;
      while (jiffies < start + CLEAR_DELAY)
        RESCHED;
      RETURN(0, "VTXIOCCLRPAGE: OK");
    }

    case VTXIOCCLRFOUND: {
      vtx_pagereq_t req;
      byte_t vtxflags;
      
      if ((err = verify_area(VERIFY_READ, (void*)arg, sizeof(vtx_pagereq_t))))
        RETURN(err, "VTXIOCCLRFOUND: EFAULT");
      memcpy_fromfs(&req, (void*)arg, sizeof(vtx_pagereq_t));
      if (req.pgbuf < 0 || req.pgbuf >= NUMPAGES)
        RETURN(-EINVAL, "VTXIOCCLRFOUND: EINVAL");
      if (i2c_senddata(CCTWR, 8, req.pgbuf, 25, 8, -1) ||
          i2c_getdata(CCTRD, 1, &vtxflags, FALSE))
        RETURN(-EIO, "VTXIOCCLRFOUND: EIO (1)");
      vtxflags |= 0x10;
      if (i2c_senddata(CCTWR, 8, req.pgbuf, 25, 8, vtxflags, -1))
        RETURN(-EIO, "VTXIOCCLRFOUND: EIO (2)");
      RETURN(0, "VTXIOCCLRFOUND: OK");
    }

    case VTXIOCPAGEREQ: {
      vtx_pagereq_t req;
      
      if ((err = verify_area(VERIFY_READ, (void*)arg, sizeof(vtx_pagereq_t))))
        RETURN(err, "VTXIOCPAGEREQ: EFAULT");
      memcpy_fromfs(&req, (void*)arg, sizeof(vtx_pagereq_t));
      if (!(req.pagemask & PGMASK_PAGE))
        req.page = 0;
      if (!(req.pagemask & PGMASK_HOUR))
        req.hour = 0;
      if (!(req.pagemask & PGMASK_MINUTE))
        req.minute = 0;
      if (req.page < 0 || req.page > 0x8ff)
        RETURN(-EINVAL, "VTXIOCPAGEREQ: EINVAL (1)");
      req.page &= 0x7ff;
      if (req.hour < 0 || req.hour > 0x3f || req.minute < 0 || req.minute > 0x7f ||
          req.pagemask >= PGMASK_MAX || req.pgbuf < 0 || req.pgbuf >= NUMPAGES)
        RETURN(-EINVAL, "VTXIOCPAGEREQ: EINVAL (2)");
      if (i2c_senddata(CCTWR, 2, req.pgbuf << 4,
          (req.pagemask & PG_HUND ? 0x10 : 0) | (req.page / 0x100) | 8,
          (req.pagemask & PG_TEN ? 0x10 : 0) | ((req.page / 0x10) & 0xf),
          (req.pagemask & PG_UNIT ? 0x10 : 0) | (req.page & 0xf),
          (req.pagemask & HR_TEN ? 0x10 : 0) | (req.hour / 0x10),
          (req.pagemask & HR_UNIT ? 0x10 : 0) | (req.hour & 0xf),
          (req.pagemask & MIN_TEN ? 0x10 : 0) | (req.minute / 0x10),
          (req.pagemask & MIN_UNIT ? 0x10 : 0) | (req.minute & 0xf), -1))
        RETURN(-EIO, "VTXIOCPAGEREQ: EIO");
      is_searching[req.pgbuf] = TRUE;
      RETURN(0, "VTXIOCPAGEREQ: OK");
    }

    case VTXIOCGETSTAT: {
      vtx_pagereq_t req;
      byte_t infobits[10];
      vtx_pageinfo_t info;
      int a;

      if ((err = verify_area(VERIFY_READ, (void*)arg, sizeof(vtx_pagereq_t))))
        RETURN(err, "VTXIOCGETSTAT: EFAULT (read)");
      memcpy_fromfs(&req, (void*)arg, sizeof(vtx_pagereq_t));
      if (req.pgbuf < 0 || req.pgbuf >= NUMPAGES)
        RETURN(-EINVAL, "VTXIOCGETSTAT: EINVAL");
      if (i2c_senddata(CCTWR, 8, req.pgbuf, 25, 0, -1) ||
          i2c_getdata(CCTRD, 10, infobits, FALSE))
        RETURN(-EIO, "VTXIOCGETSTAT: EIO");
      info.pagenum = ((infobits[8] << 8) & 0x700) | ((infobits[1] << 4) & 0xf0) |
          (infobits[0] & 0x0f);
      if (info.pagenum < 0x100)
        info.pagenum += 0x800;
      info.hour = ((infobits[5] << 4) & 0x30) | (infobits[4] & 0x0f);
      info.minute = ((infobits[3] << 4) & 0x70) | (infobits[2] & 0x0f);
      info.charset = ((infobits[7] >> 1) & 7);
      info.delete = !!(infobits[3] & 8);
      info.headline = !!(infobits[5] & 4);
      info.subtitle = !!(infobits[5] & 8);
      info.supp_header = !!(infobits[6] & 1);
      info.update = !!(infobits[6] & 2);
      info.inter_seq = !!(infobits[6] & 4);
      info.dis_disp = !!(infobits[6] & 8);
      info.serial = !!(infobits[7] & 1);
      info.notfound = !!(infobits[8] & 0x10);
      info.pblf = !!(infobits[9] & 0x20);
      info.hamming = 0;
      for (a = 0; a <= 7; a++) {
        if (infobits[a] & 0xf0) {
          info.hamming = 1;
          break;
        }
      }
      if ((err = verify_area(VERIFY_WRITE, req.buffer, sizeof(vtx_pageinfo_t))))
        RETURN(err, "VTXIOCGETSTAT: EFAULT (write)");
      memcpy_tofs(req.buffer, &info, sizeof(vtx_pageinfo_t));
      if (!info.hamming && !info.notfound) {
        is_searching[req.pgbuf] = FALSE;
      }
      RETURN(0, "VTXIOCGETSTAT: OK");
    }

    case VTXIOCGETPAGE: {
      vtx_pagereq_t req;
      int start, end;

      if ((err = verify_area(VERIFY_READ, (void*)arg, sizeof(vtx_pagereq_t))))
        RETURN(err, "VTXIOCGETPAGE: EFAULT (read)");
      memcpy_fromfs(&req, (void*)arg, sizeof(vtx_pagereq_t));
      if (req.pgbuf < 0 || req.pgbuf >= NUMPAGES ||
          req.start > req.end || req.end >= VTX_PAGESIZE)
        RETURN(-EINVAL, "VTXIOCGETPAGE: EINVAL");
      if ((err = verify_area(VERIFY_WRITE, req.buffer, req.end - req.start + 1)))
        RETURN(err, "VTXIOCGETPAGE: EFAULT (write)");
      if (i2c_senddata(CCTWR, 8, req.pgbuf, req.start / 40, req.start % 40, -1) ||
          i2c_getdata(CCTRD, req.end - req.start + 1, req.buffer, TRUE))
        RETURN(-EIO, "VTXIOCGETPAGE: EIO (page)");
      /* Always get the time from buffer 4, since this stupid SAA524? only updates the
       * currently displayed buffer...
       */
      if (req.start <= 39 && req.end >= 32) {
        start = (req.start <= 32 ? 32 : req.start);
        end = (req.end >= 39 ? 39 : req.end);
        if (i2c_senddata(CCTWR, 8, 4, 0, start, -1) ||
            i2c_getdata(CCTRD, end - start + 1, req.buffer + start - req.start, TRUE))
          RETURN(-EIO, "VTXIOCGETPAGE: EIO (time)");
      }
      /* Insert the header from buffer 4 only, if DAU is still searching for a page */
      if (req.start <= 31 && req.end >= 7 && is_searching[req.pgbuf]) {
        start = (req.start <= 7 ? 7 : req.start);
        end = (req.end >= 31 ? 31 : req.end);
        if (i2c_senddata(CCTWR, 8, 4, 0, start, -1) ||
            i2c_getdata(CCTRD, end - start + 1, req.buffer + start - req.start, TRUE))
          RETURN(-EIO, "VTXIOCGETPAGE: EIO (header)");
      }
      RETURN(0, "VTXIOCGETPAGE: OK");
    }

    case VTXIOCSTOPDAU: {
      vtx_pagereq_t req;
      
      if ((err = verify_area(VERIFY_READ, (void*)arg, sizeof(vtx_pagereq_t))))
        RETURN(err, "VTXIOCSTOPDAU: EFAULT");
      memcpy_fromfs(&req, (void*)arg, sizeof(vtx_pagereq_t));
      if (req.pgbuf < 0 || req.pgbuf >= NUMPAGES)
        RETURN(-EINVAL, "VTXIOCSTOPDAU: EINVAL");
      if (i2c_senddata(CCTWR, 2, req.pgbuf << 4, 0, -1))
        RETURN(-EIO, "VTXIOCSTOPDAU: EIO");
      RETURN(0, "VTXIOCSTOPDAU: OK");
    }

    case VTXIOCPUTPAGE: {
      vtx_pagereq_t req;
      int pos, end;

      if ((err = verify_area(VERIFY_READ, (void*)arg, sizeof(vtx_pagereq_t))))
        RETURN(err, "VTXIOCPUTPAGE: EFAULT (1)");
      memcpy_fromfs(&req, (void*)arg, sizeof(vtx_pagereq_t));
      if (req.start > req.end || req.end >= VTX_PAGESIZE)
        RETURN(-EINVAL, "VTXIOCPUTPAGE: EINVAL");
      if ((err = verify_area(VERIFY_READ, req.buffer, req.end - req.start + 1)))
        RETURN(err, "VTXIOCPUTPAGE: EFAULT (2)");
      /* Suppressing the display of the current time/page in the first line is impossible, so we
       * have to disable this line completely. Due to this, the whole page gets shifted down one
       * line and the (normally unsused) status-line will be used for the last videotext-line.
       * Accesses that exceed logical line 22 have to be split up due to the wrap-around after
       * screen-line 23.
       * Also, double-height attributes in screen-line 23 will be ignored by the 524? and some
       * garbage that was left in the following line would become visible in this case. Because
       * of this we have to write an empty screen-line 24 in that case. BTW, this only works if
       * logical line 22 is written completely.
       */
      if (req.start / 40 < 23 && req.end / 40 >= 23) {
        end = 23 * 40 - 1;
      } else {
        end = req.end;
      }
      if (i2c_senddata(CCTWR, 8, 4, req.start / 40 + 1, req.start % 40, -1) ||
          i2c_sendbuf(CCTWR, 11, end - req.start + 1, req.buffer, TRUE))
        RETURN(-EIO, "VTXIOCPUTPAGE: EIO (3)");
      if (req.end != end) {
        if (i2c_senddata(CCTWR, 8, 4, 24, 0, -1))
          RETURN(-EIO, "VTXIOCPUTPAGE: EIO (4)");
        if (req.start <= 22 * 40 && req.end >= 23 * 40 - 1) {
          for (pos = 22 * 40 - req.start; pos < 23 * 40 - req.start; pos++) {
            if (get_fs_byte(req.buffer + pos) == 0x0d) {
              if (i2c_senddata(CCTWR, 11,
                  ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
                  ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
                  -1))
                RETURN(-EIO, "VTXIOCPUTPAGE: EIO (5)");
              RETURN(0, "VTXIOCPUTPAGE: OK");
            }
          }
        }
        if (i2c_sendbuf(CCTWR, 11, req.end - 23 * 40 + 1, req.buffer + 23 * 40, TRUE))
          RETURN(-EIO, "VTXIOCPUTPAGE: EIO (6)");
      }
      RETURN(0, "VTXIOCPUTPAGE: OK");
    }

    case VTXIOCSETDISP: {
      vtx_pagereq_t req;

      if ((err = verify_area(VERIFY_READ, (void*)arg, sizeof(vtx_pagereq_t))))
        RETURN(err, "VTXIOCSETDISP: EFAULT");
      memcpy_fromfs(&req, (void*)arg, sizeof(vtx_pagereq_t));
      if (req.page < DISPOFF || req.page > DISPINS + INTERLACE_OFFSET)
        RETURN(-EINVAL, "VTXIOCSETDISP: EINVAL");
      if (i2c_senddata(CCTWR, 1, disp_modes[req.page][0], -1) ||
          i2c_senddata(CCTWR, 5, disp_modes[req.page][1], disp_modes[req.page][2], -1))
        RETURN(-EIO, "VTXIOCSETDISP: EIO");
      RETURN(0, "VTXIOCSETDISP: OK");
    }

    case VTXIOCPUTSTAT: {
      vtx_pagereq_t req;
      vtx_pageinfo_t info;

      if ((err = verify_area(VERIFY_READ, (void*)arg, sizeof(vtx_pagereq_t))))
        RETURN(err, "VTXIOCPUTSTAT: EFAULT (read req)");
      memcpy_fromfs(&req, (void*)arg, sizeof(vtx_pagereq_t));
      if ((err = verify_area(VERIFY_READ, req.buffer, sizeof(vtx_pageinfo_t))))
        RETURN(err, "VTXIOCPUTSTAT: EFAULT (read info)");
      memcpy_fromfs(&info, req.buffer, sizeof(vtx_pageinfo_t));
      if (i2c_senddata(CCTWR, 8, 4, 25, 5,
          (!!info.headline << 2) | (!!info.subtitle << 3),
          1 | (!!info.dis_disp << 3),		/* Always suppress header */
          (info.charset & 7) << 1,
          -1))
        RETURN(-EIO, "VTXIOCPUTSTAT: EIO");
      RETURN(0, "VTXIOCPUTSTAT: OK");
    }

    default:
      RETURN(-EINVAL, "ioctl: EINVAL (unknown command)");
  }
}


static int
vtx_open(struct inode *inode, struct file *file) {
  int start, pgbuf;
  byte_t checkval;

  NOTIFY(2, "open");
  if (MINOR(inode->i_rdev))
    RETURN(-ENODEV, "open: ENODEV");
  if (MOD_IN_USE)
    RETURN(-EBUSY, "open: EBUSY (already in use)");
  MOD_INC_USE_COUNT;
  if (reserve_bus()) {
    MOD_DEC_USE_COUNT;
    RETURN(-EBUSY, "open: EBUSY (reserve_bus)");
  }

  /* Check if SDA-input-line is working (we would probably never notice this otherwise)
   */
  i2c_end();
  UDELAY(1);
  start = jiffies;
  while (!get_sda()) {
    if (jiffies >= start + I2C_TIMEOUT) {
      MOD_DEC_USE_COUNT;
      RETURN(-EIO, "open: EIO (check_sda)");
    }
    RESCHED;
  }

  if (i2c_senddata(CCTWR, 0, 0, -1) ||		/* Select R11 */
						/* Turn off parity checks (we do this ourselves) */
      i2c_senddata(CCTWR, 1, disp_modes[disp_mode][0], 0, -1) ||
						/* Display TV-picture */
      i2c_senddata(CCTWR, 4, 4, disp_modes[disp_mode][1], disp_modes[disp_mode][2], 7, -1) ||
						/* Set display to page 4, suppress header */
      i2c_senddata(CCTWR, 8, 4, 25, 6, 1, -1)) {
    MOD_DEC_USE_COUNT;
    RETURN(-EIO, "open: EIO (init)");
  }
  for (pgbuf = 0; pgbuf <= 7; pgbuf++) {	/* Stop all DAUs, clear all buffers */
    if (pgbuf < NUMPAGES) {
      if (i2c_senddata(CCTWR, 2, pgbuf << 4, 0, -1)) {
        MOD_DEC_USE_COUNT;
        RETURN(-EIO, "open: EIO (stop)");
      }
      is_searching[pgbuf] = FALSE;
    }
    if (i2c_senddata(CCTWR, 8, pgbuf | 8)) {
      MOD_DEC_USE_COUNT;
      RETURN(-EIO, "open: EIO (clear)");
    }
    start = jiffies;
    while (jiffies < start + CLEAR_DELAY)
      RESCHED;
  }
  
  /* Check all page-buffers (must be filled with 0x20, since we cleared them recently)
   */
  for (pgbuf = 0; pgbuf <= 7; pgbuf++) {
    if (i2c_senddata(CCTWR, 8, pgbuf, 1, 0, -1) ||
        i2c_getdata(CCTRD, 1, &checkval, FALSE)) {
      MOD_DEC_USE_COUNT;
      RETURN(-EIO, "open: EIO (getdata)");
    }
    if (checkval != ' ') {
      MOD_DEC_USE_COUNT;
      RETURNx1(-EIO, "open: EIO (check_ram, buf %d)", pgbuf);
    }
  }
  
  RETURN(0, "open: OK");
}


static void
vtx_release(struct inode *inode, struct file *file) {
  NOTIFY(2, "release");
  i2c_senddata(CCTWR, 1, 0x20, -1);	/* Turn off CCT */
  i2c_senddata(CCTWR, 5, 3, 3, -1);	/* Turn off TV-display */
  release_bus();
  MOD_DEC_USE_COUNT;
}


static struct file_operations vtx_fops = {
  NULL,		/* lseek */
  NULL,		/* read */
  NULL,		/* write */
  NULL,		/* readdir */
  NULL,		/* select */
  vtx_ioctl,
  NULL,		/* mmap */
  vtx_open,
  vtx_release
};


/* Routines for loadable modules
 */
char kernel_version[] = KERNEL_VERSION;

int
init_module(void) {
  printk("videotext driver ("
#if defined IF_CT
      "c't-interface"
#elif defined IF_CTSERIAL
      "serial c't-interface"
#elif defined IF_VTX2000
      "VTX2000/VD3000-interface"
#elif defined IF_PCVT7000
      "ELV PC-VT 7000-interface"
#elif defined IF_SATCOM
      "SATCOM-interface"
#elif defined IF_HOMEBREW
      "homebrew interface"
#endif
      " at 0x%x)  version %d.%d\n", io_base, VTX_VER_MAJ, VTX_VER_MIN);
  if(register_chrdev(major, VTX_NAME, &vtx_fops)) {
    printk("vtx: Error: Cannot register major number %d\n", major);
    return -EIO;
  }
  return 0;
}


void
cleanup_module(void) {
  printk("removing videotext driver\n");
  if(MOD_IN_USE)
    printk("vtx: Warning: Device is busy, delaying driver removal\n");
  if(unregister_chrdev(major, VTX_NAME))
    printk("vtx: Error: unregister_chrdev() failed\n");
}
