/*

Copyright (c) 1990 - 1994  FUJITSU LIMITED
Copyright (c) 1990 - 1991  PFU Limited

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE FUJITSU LIMITED AND PFU LIMITED BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the FUJITSU LIMITED and
PFU Limited shall not be used in advertising or otherwise to promote
the sale, use or other dealings in this Software without prior written
authorization from the FUJITSU LIMITED and PFU Limited.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
                               fujiwara@a80.tech.yk.fujitsu.co.jp

*/

#ifndef _CMODSTR_H_
#define _CMODSTR_H_

#ident "@(#)cmodstr.h	1.5 4/3/91"

/*
 * Protocol requests constants and alignment values
 * These would really be in Change Modifiers's X.h and Xproto.h equivalents
 */

#include "cmod.h"

#if defined(__STDC__) && !defined(UNIXCPP)
#define ControlModifiersGetReq(name, req, info) GetReq (name, req); \
	req->reqType = info->codes->major_opcode; \
	req->cmodReqType = X_##name;
#else
#define ControlModifiersGetReq(name, req, info) GetReq (name, req); \
	req->reqType = info->codes->major_opcode; \
	req->cmodReqType = X_/**/name;
#endif

typedef struct {
    BYTE    type;
    BYTE    unused;
    CARD16  sequenceNumber B16;
    CARD32  change B32;			/* change state mask */
    CARD32  state B32;			/* current states */
} xControlModifiersEvent;

typedef struct {
    CARD8	reqType;		/* always codes->major_opcode */
    CARD8	cmodReqType;		/* always X_ControlModifiersGetVersion */
    CARD16	length B16;
} xControlModifiersGetVersionReq;
#define sz_xControlModifiersGetVersionReq	4

typedef struct {
    BYTE	type;			/* X_Reply */
    CARD8	unused;			/* not used */
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD8	majorVersion;		/* major version of CMOD protocol */
    CARD8	minorVersion;		/* minor version of CMOD protocol */
    CARD16	pad1 B32;
    CARD32	validReport B32;	/* valid bits of to report */
    CARD32	validChange B32;	/* valid bits of to change */
    CARD32	pad2 B32;
    CARD32	pad3 B32;
    CARD32	pad4 B32;
} xControlModifiersGetVersionReply;
#define sz_xControlModifiersGetVersionReply	32

typedef struct {
    CARD8	reqType;		/* always codes->major_opcode */
    CARD8	cmodReqType;		/* always X_ControlModifiersSetMask */
    CARD16	length B16;
    Window	window;
    CARD32	mask B32;
    CARD32	pad1;
    CARD32	pad2;
    CARD32	pad3;
    CARD32	pad4;
    CARD32	pad5;
} xControlModifiersSetMaskReq;
#define sz_xControlModifiersSetMaskReq	32

typedef struct {
    CARD8	reqType;		/* always codes->major_opcode */
    CARD8	cmodReqType;		/* always X_ControlModifiersGetState */
    CARD16	length B16;
    Window	window;
} xControlModifiersGetMaskReq;
#define sz_xControlModifiersGetMaskReq	8

typedef struct {
    BYTE	type;			/* X_Reply */
    CARD8	unused;			/* not used */
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD32	mask B32;		/* current state */
    CARD32	unused1 B32;
    CARD32	unused2 B32;
    CARD32	unused3 B32;
    CARD32	unused4 B32;
    CARD32	unused5 B32;
} xControlModifiersGetMaskReply;
#define sz_xControlModifiersGetMaskReply	32

typedef struct {
    CARD8	reqType;		/* always codes->major_opcode */
    CARD8	cmodReqType;		/* always X_ControlModifiersGetState */
    CARD16	length B16;
} xControlModifiersGetStateReq;
#define sz_xControlModifiersGetStateReq	4

typedef struct {
    BYTE	type;			/* X_Reply */
    CARD8	unused;			/* not used */
    CARD16	sequenceNumber B16;
    CARD32	length B32;
    CARD32	state B32;		/* current state */
    CARD32	unused1 B32;
    CARD32	unused2 B32;
    CARD32	unused3 B32;
    CARD32	unused4 B32;
    CARD32	unused5 B32;
} xControlModifiersGetStateReply;
#define sz_xControlModifiersGetStateReply	32

typedef struct {
    CARD8	reqType;		/* always codes->major_opcode */
    CARD8	cmodReqType;		/* always X_ControlModifiersChangeState */
    CARD16	length B16;
    CARD32	change B32;		/* mask */
    CARD32	state B32;		/* state */
    CARD32	unused1 B32;
    CARD32	unused2 B32;
    CARD32	unused3 B32;
    CARD32	unused4 B32;
    CARD32	unused5 B32;
} xControlModifiersChangeStateReq;
#define sz_xControlModifiersChangeStateReq	32

typedef struct {
    CARD8	reqType;		/* always codes->major_opcode */
    CARD8	cmodReqType;		/* always X_ControlModifiersGetVersion */
    CARD16	length B16;
} xControlModifiersGrabReq;
#define sz_xControlModifiersGrabReq	4

typedef struct {
    CARD8	reqType;		/* always codes->major_opcode */
    CARD8	cmodReqType;		/* always X_ControlModifiersGetVersion */
    CARD16	length B16;
} xControlModifiersUngrabReq;
#define sz_xControlModifiersUngrabReq	4

#endif /* _CMODSTR_H_ */
