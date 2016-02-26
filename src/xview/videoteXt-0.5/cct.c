/*
 * cct.c: Routines to communicate with VTX-decoder SAA 5246 & compatibles (eg. 5243) via
 *        I²C-routines
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <linux/vtx.h>
#include "cct.h"


char *cct_device = "/dev/vtx";

static int vtxfd = -1;



/* Open device, initialize SAA 5246 & internal tables; this also checks, if the driver-version
 * is compatible (you have to pass the expected major-version and the lowest possible minor-
 * version); it also fills in the vtx_info_t-struct pointed to by prg_info
 * Return CCTOK if success, CCTEOPEN if device isn't ready, CCTERR if I/O-error occured,
 * CCTEVERSION if driver-version isn't compatible
 */
int
cct_open(int major, int minor, vtx_info_t *prg_info) {
  vtx_info_t *info, tmp_info;
  
  if ((vtxfd = open(cct_device, O_RDONLY)) < 0)
    return CCTEOPEN;
  info = (prg_info != NULL ? prg_info : &tmp_info);
  if (ioctl(vtxfd, VTXIOCGETINFO, info) < 0)
    return CCTERR;
  if (info->version_major != major || info->version_minor < minor) {
    while (close(vtxfd) < 0 && errno == EINTR);
    return CCTEVERSION;
  }
  return CCTOK;
}


/* Turn off SAA 5246, close device
 */
void
cct_close(void) {
  if (vtxfd == -1)
    return;
  while (close(vtxfd) < 0 && errno == EINTR);
}


/* Clear page-buffer pgbuf in SAA 5246 & wait.
 * Return CCTOK if success, CCTEINVAL if pgbuf is out of range, CCTERR if I/O-error occured
 */
int
cct_clearpgbuf(int pgbuf) {
  vtx_pagereq_t pagereq;
  
  if (vtxfd == -1)
    return CCTENOTOPEN;
  pagereq.pgbuf = pgbuf;
  if (ioctl(vtxfd, VTXIOCCLRPAGE, &pagereq) < 0) {
    if (errno == EINVAL)
      return CCTEINVAL;
    return CCTERR;
  }
  return CCTOK;
}


/* Check if SAA 5246 already found a page in pgbuf. This only checks if the beginning of a page
 * was found, NOT the whole page!
 * Return CCTOK if page was found, CCTNOTFOUND if no page was found, CCTEINVAL if pgbuf is out
 * of range, CCTERR if I/O-error occured
 */
int
cct_checkpage(int pgbuf) {
  vtx_pagereq_t pagereq;
  vtx_pageinfo_t pageinfo;
  
  if (vtxfd == -1)
    return CCTENOTOPEN;
  pagereq.pgbuf = pgbuf;
  pagereq.buffer = &pageinfo;
  if (ioctl(vtxfd, VTXIOCGETSTAT, &pagereq) < 0) {
    if (errno == EINVAL)
      return CCTEINVAL;
    return CCTERR;
  }
  if (pageinfo.notfound || pageinfo.hamming)
    return CCTNOTFOUND;
  return CCTOK;
}


/* Stop data acquisition unit for pgbuf.
 * Return CCTOK if success, CCTEINVAL if pgbuf is out of range, CCTERR if I/O-error occured
 */
int
cct_stop_dau(int pgbuf) {
  vtx_pagereq_t pagereq;
  
  if (vtxfd == -1)
    return CCTENOTOPEN;
  pagereq.pgbuf = pgbuf;
  if (ioctl(vtxfd, VTXIOCSTOPDAU, &pagereq) < 0) {
    if (errno == EINVAL)
      return CCTEINVAL;
    return CCTERR;
  }
  return CCTOK;
}


/* Reset bits in pgbuf used to check if a page was already found
 * Return CCTOK if success, CCTEINVAL if pgbuf is out of range, CCTERR if I/O-error occured
 */
int
cct_reset_pgfound(int pgbuf) {
  vtx_pagereq_t pagereq;
  
  if (vtxfd == -1)
    return CCTENOTOPEN;
  pagereq.pgbuf = pgbuf;
  if (ioctl(vtxfd, VTXIOCCLRFOUND, &pagereq) < 0) {
    if (errno == EINVAL)
      return CCTEINVAL;
    return CCTERR;
  }
  return CCTOK;
}


/* Search for page page/hour/minute (hexadecimal !!!), only regard digits with corresponding bits
 * in pagemask set (constants defined in <linux/vtx.h>), store page in page-buffer pgbuf
 * Return CCTOK if success, CCTEINVAL if page, hour, minute or pgbuf is out of range, CCTERR if
 * I/O-error occured
 */
int
cct_searchpage(int page, int hour, int minute, int pagemask, int pgbuf) {
  vtx_pagereq_t pagereq;
  
  if (vtxfd == -1)
    return CCTENOTOPEN;
  pagereq.page = page;
  pagereq.hour = hour;
  pagereq.minute = minute;
  pagereq.pagemask = pagemask;
  pagereq.pgbuf = pgbuf;
  if (ioctl(vtxfd, VTXIOCPAGEREQ, &pagereq) < 0) {
    if (errno == EINVAL)
      return CCTEINVAL;
    return CCTERR;
  }
  return CCTOK;
}


/* Get page from page-buffer pgbuf of SAA 5246 (first char x1/y1, last char x2/y2), store it in
 * buffer (= ptr to array of byte_t's)
 * Return CCTOK if success, CCTEINVAL if corner-coordinates or pgbuf is invalid, CCTERR if
 * I/O-error occured
 */
int
cct_getpage(int pgbuf, int x1, int y1, int x2, int y2, byte_t *buffer, vtx_pageinfo_t *info) {
  vtx_pagereq_t pagereq;

  if (vtxfd == -1)
    return CCTENOTOPEN;
  pagereq.pgbuf = pgbuf;
  pagereq.start = y1 * 40 + x1;
  pagereq.end = y2 * 40 + x2;
  if (info) {
    pagereq.buffer = info;
    if (ioctl(vtxfd, VTXIOCGETSTAT, &pagereq) < 0) {
      if (errno == EINVAL)
        return CCTEINVAL;
      return CCTERR;
    }
  }
  if (buffer) {
    pagereq.buffer = buffer;
    if (ioctl(vtxfd, VTXIOCGETPAGE, &pagereq) < 0) {
      if (errno == EINVAL)
        return CCTEINVAL;
      return CCTERR;
    }
  }
  return CCTOK;
}


/* Display page in buffer (first char x1/y1, last char x2/y2) on TV-Screen.
 * Return CCTOK if success, CCTEINVAL if corner-coordinates or are invalid, CCTERR if
 * I/O-error occured
 */
int
cct_putpage(int x1, int y1, int x2, int y2, const byte_t *buffer, const vtx_pageinfo_t *info) {
  vtx_pagereq_t pagereq;

  if (vtxfd == -1)
    return CCTENOTOPEN;
  if (info) {
    pagereq.buffer = (vtx_pageinfo_t*)info;
    if (ioctl(vtxfd, VTXIOCPUTSTAT, &pagereq) < 0) {
      if (errno == EINVAL)
        return CCTEINVAL;
      return CCTERR;
    }
  }
  if (buffer) {
    pagereq.start = y1 * 40 + x1;
    pagereq.end = y2 * 40 + x2;
    pagereq.buffer = (byte_t*)buffer;
    if (ioctl(vtxfd, VTXIOCPUTPAGE, &pagereq) < 0) {
      if (errno == EINVAL)
        return CCTEINVAL;
      return CCTERR;
    }
  }
  return CCTOK;
}


/* Set mode of video-output of SAA5246 (for displaying pages on TV-screen).
 * Return CCTOK if success of CCTERR if I/O-error occured.
 */
int
cct_set_display(vtxdisp_t disp) {
  vtx_pagereq_t pagereq;

  if (vtxfd == -1)
    return CCTENOTOPEN;
  pagereq.page = disp;
  if (ioctl(vtxfd, VTXIOCSETDISP, &pagereq) < 0) {
    if (errno == EINVAL)
      return CCTEINVAL;
    return CCTERR;
  }
  return CCTOK;
}
