#ifndef _VTXTOOLS_H
#define _VTXTOOLS_H

/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */


#include <linux/vtx.h>


int vtx_chkparity(byte_t *val);
byte_t vtx_mkparity(byte_t val);
int vtx_chkhamming(byte_t val);
int inc_vtxpage(int page);
int dec_vtxpage(int page);
int vtx_hex2dec(int pgnum);
int vtx_dec2hex(int pgnum);
int vtx_chkpgnum(int pgnum, int hex_ok);

#endif /* _VTXTOOLS_H */
