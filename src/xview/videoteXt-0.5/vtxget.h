#ifndef _VTXGET_H
#define _VTXGET_H

/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */


#include "vtxdecode.h"


typedef enum { FMT_ISO, FMT_ANSI, FMT_TEXT, FMT_VTX, FMT_PS, FMT_IPS, FMT_CPS } ofmt_t;


extern int page_timeout, show_hidden, show_header, loop_forever;
extern ofmt_t ofmt;
extern vtx_info_t vtx_info;
extern vtxpage_t page;
extern char *outdir, *progname, *fname_prefix;

#endif /* _VTXGET_H */
