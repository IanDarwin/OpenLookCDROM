/* $XimpImplementGroup: XimImSw.h, v 1.1 94/05/31 21:16:02 $ */
/* $XConsortium: XimImSw.h,v 1.3 94/03/31 22:01:03 rws Exp $ */
/******************************************************************

                Copyright 1992, 1993 by FUJITSU LIMITED

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of FUJITSU LIMITED
not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.
FUJITSU LIMITED makes no representations about the suitability of
this software for any purpose. 
It is provided "as is" without express or implied warranty.

FUJITSU LIMITED DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
EVENT SHALL FUJITSU LIMITED BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
                               fujiwara@a80.tech.yk.fujitsu.co.jp

******************************************************************/
/*

Copyright (c) 1992 - 1994  FUJITSU LIMITED

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
IN NO EVENT SHALL THE FUJITSU LIMITED BE LIABLE FOR ANY CLAIM, DAMAGES
OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the FUJITSU LIMITED shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the FUJITSU LIMITED.

  Author: Takashi Fujiwara     FUJITSU LIMITED 

*/

#ifndef _XIMIMSW_H
#define _XIMIMSW_H

typedef struct {
    Bool	(*checkprocessing)(
#if NeedNestedPrototypes
	Xim   im
#endif
		);
    Bool	(*im_open)(
#if NeedNestedPrototypes
	Xim   im
#endif
		);
    void	(*im_free)(
#if NeedNestedPrototypes
	Xim   im
#endif
		);
} XimImsportSW;

typedef struct {
    XIM	(*im_open)(
#if NeedNestedPrototypes
	XLCd lcd, Display *dpy, XrmDatabase rdb, char *res_name, char *res_class
#endif
		);
} XimImsportProtocolSW;

extern XimImsportSW _XimImSportRec[];
extern XimImsportProtocolSW _XimImSportProtocolRec[];

#endif /* _XIMIMSW_H */
