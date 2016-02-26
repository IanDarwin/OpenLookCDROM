#ifndef _VTX_H
#define _VTX_H

/*
 *  vtx.h
 *
 *  Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 *
 */


typedef unsigned char byte_t;


/* videotext ioctls
 */
#define VTXIOCGETINFO	0x7101	/* get version of driver & capabilities of vtx-chipset */
#define VTXIOCCLRPAGE	0x7102	/* clear page-buffer */
#define VTXIOCCLRFOUND	0x7103	/* clear bits indicating that page was found */
#define VTXIOCPAGEREQ	0x7104	/* search for page */
#define VTXIOCGETSTAT   0x7105	/* get status of page-buffer */
#define VTXIOCGETPAGE   0x7106  /* get contents of page-buffer */
#define VTXIOCSTOPDAU   0x7107  /* stop data acquisition unit */
#define VTXIOCPUTPAGE   0x7108  /* display page on TV-screen */
#define VTXIOCSETDISP   0x7109  /* set TV-mode */
#define VTXIOCPUTSTAT   0x710a  /* set status of TV-output-buffer */


/* definitions for VTXIOCGETINFO
 */
#define SAA5243 0
#define SAA5246 1

typedef struct {
  int version_major, version_minor;	/* version of driver; if version_major changes, driver */
					/* is not backward compatible!!! CHECK THIS!!! */  
  int numpages;				/* number of page-buffers of vtx-chipset */
  int cct_type;				/* type of vtx-chipset (SAA5243 or SAA5246) */
} vtx_info_t;


/* definitions for VTXIOC{CLRPAGE,CLRFOUND,PAGEREQ,GETSTAT,GETPAGE,STOPDAU,PUTPAGE,SETDISP}
 */
#define MIN_UNIT   (1<<0)
#define MIN_TEN    (1<<1)
#define HR_UNIT    (1<<2)
#define HR_TEN     (1<<3)
#define PG_UNIT    (1<<4)
#define PG_TEN     (1<<5)
#define PG_HUND    (1<<6)
#define PGMASK_MAX (1<<7)
#define PGMASK_PAGE (PG_HUND | PG_TEN | PG_UNIT)
#define PGMASK_HOUR (HR_TEN | HR_UNIT)
#define PGMASK_MINUTE (MIN_TEN | MIN_UNIT)

typedef struct {
  int page;			/* number of requested page (hexadecimal) */
  int hour;			/* requested hour (hexadecimal) */
  int minute;			/* requested minute (hexadecimal) */
  int pagemask;			/* mask defining which values of the above are set */
  int pgbuf;			/* buffer where page will be stored */
  int start;			/* start of requested part of page */
  int end;			/* end of requested part of page */
  void* buffer;			/* pointer to beginning of destination buffer */
} vtx_pagereq_t;


/* definitions for VTXIOC{GETSTAT,PUTSTAT}
 */
#define VTX_PAGESIZE 960

typedef struct {
  int pagenum;			/* number of page (hexadecimal) */
  int hour;			/* hour (hexadecimal) */
  int minute;			/* minute (hexadecimal) */
  int charset;			/* national charset */
  unsigned delete : 1;		/* delete page (C4) */
  unsigned headline : 1;	/* insert headline (C5) */
  unsigned subtitle : 1;	/* insert subtitle (C6) */
  unsigned supp_header : 1;	/* suppress header (C7) */
  unsigned update : 1;		/* update page (C8) */
  unsigned inter_seq : 1;	/* interrupted sequence (C9) */
  unsigned dis_disp : 1;	/* disable/suppress display (C10) */
  unsigned serial : 1;		/* serial mode (C11) */
  unsigned notfound : 1;	/* /FOUND */
  unsigned pblf : 1;		/* PBLF */
  unsigned hamming : 1;		/* hamming-error occured */
} vtx_pageinfo_t;


/* definitions for VTXIOCSETDISP
 */
typedef enum { DISPOFF, DISPNORM, DISPTRANS, DISPINS, INTERLACE_OFFSET } vtxdisp_t;


#endif /* _VTX_H */
