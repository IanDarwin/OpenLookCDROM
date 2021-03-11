/* $XimpImplementGroup: XimpConv.c, v 1.1 94/05/31 21:16:03 $ */
/* $XConsortium: Ximpint.c,v 1.5 92/10/19 19:27:06 rws Exp $ */
/******************************************************************

              Copyright 1991, 1992 by Fuji Xerox Co.,Ltd.

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Fuji Xerox Co.,Ltd.
not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.
Fuji Xerox Co.,Ltd. makes no representations about the suitability of
this software for any purpose.
It is provided "as is" without express or implied warranty.

FUJI XEROX CO.,LTD. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
IN NO EVENT SHALL FUJI XEROX CO.,LTD. BE LIABLE FOR ANY SPECIAL, INDIRECT
OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH
THE USE OR PERFORMANCE OF THIS SOFTWARE.

  Auther: Kazunori Nishihara,  Fuji Xerox Co.,Ltd.
                               kaz@ssdev.ksp.fujixerox.co.jp

******************************************************************/
/*

Copyright (c) 1994  FUJITSU LIMITED

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

  Auther  : Takashi Fujiwara   FUJITSU LIMITED 

*/

#define NEED_EVENTS
#include "Xlibint.h"
#include "Xutil.h"
#include "Xlcint.h"

#include "XimpIm.h"

#ifndef MAXINT
#define MAXINT          (~((unsigned int)1 << (8 * sizeof(int)) - 1))
#endif /* !MAXINT */

Public int
_Ximp_ctstombs(im, from, from_len, to, to_len, state)
    Ximp_XIM	 im;
    char	*from;
    int		 from_len;
    char	*to;
    int		 to_len;
    Status	*state;
{
    XlcConv	 conv = im->ximp_impart->ctom_conv;
    int		 from_left;
    int		 to_left;
    int		 from_savelen;
    int		 to_savelen;
    int		 from_cnvlen;
    int		 to_cnvlen;
    char	*from_buf;
    char	*to_buf;
    Status	 tmp_state;

    if (!state)
	state = &tmp_state;

    if (!conv || !from || !from_len) {
	*state = XLookupNone;
	return 0;
    }

    if (to && to_len) {
	from_left = from_len;
	to_left = to_len - 1;
	from_cnvlen = 0;
	to_cnvlen = 0;
	for (;;) {
	    from_savelen = from_left;
	    to_savelen = to_left;
	    from_buf = &from[from_cnvlen];
	    to_buf = &to[to_cnvlen];
	    if (_XlcConvert(conv, (XPointer *)&from_buf, &from_left,
				 (XPointer *)&to_buf, &to_left, NULL, 0) < 0) {
		*state = XLookupNone;
		return 0;
	    }
	    from_cnvlen += (from_savelen - from_left);
	    to_cnvlen += (to_savelen - to_left);
	    if (from_left == 0) {
		if (to_cnvlen > 0) {
		    to[to_cnvlen] = '\0';
		    *state = XLookupChars;
		} else {
		    *state = XLookupNone;
		}
		return to_cnvlen;
	    }
	    if (to_left == 0)
		break;
	}
    }

    from_left = from_len;
    from_cnvlen = 0;
    to_cnvlen = 0;
    to_buf = NULL;
    for (;;) {
	from_savelen = from_left;
	to_left = MAXINT;
	from_buf = &from[from_cnvlen];
	if (_XlcConvert(conv, (XPointer *)&from_buf, &from_left,
				 (XPointer *)&to_buf, &to_left, NULL, 0) < 0) {
	    *state = XLookupNone;
	    return 0;
	}
	from_cnvlen += (from_savelen - from_left);
	to_cnvlen += (MAXINT - to_left);
	if (from_left == 0) {
	    if (to_cnvlen > 0)
		*state = XBufferOverflow;
	    else
		*state = XLookupNone;
	    break;
	}
    }
    return to_cnvlen;
}

Public int
_Ximp_ctstowcs(im, from, from_len, to, to_len, state)
    Ximp_XIM	 im;
    char	*from;
    int		 from_len;
    wchar_t	*to;
    int		 to_len;
    Status	*state;
{
    XlcConv	 conv = im->ximp_impart->ctow_conv;
    int		 from_left;
    int		 to_left;
    int		 from_savelen;
    int		 to_savelen;
    int		 from_cnvlen;
    int		 to_cnvlen;
    char	*from_buf;
    wchar_t	*to_buf;
    Status	 tmp_state;

    if (!state)
	state = &tmp_state;

    if (!conv || !from || !from_len) {
	*state = XLookupNone;
	return 0;
    }

    if (to && to_len) {
	from_left = from_len;
	to_left = to_len - 1;
	from_cnvlen = 0;
	to_cnvlen = 0;
	for (;;) {
	    from_savelen = from_left;
	    to_savelen = to_left;
	    from_buf = &from[from_cnvlen];
	    to_buf = &to[to_cnvlen];
	    if (_XlcConvert(conv, (XPointer *)&from_buf, &from_left,
				 (XPointer *)&to_buf, &to_left, NULL, 0) < 0) {
		*state = XLookupNone;
		return 0;
	    }
	    from_cnvlen += (from_savelen - from_left);
	    to_cnvlen += (to_savelen - to_left);
	    if (from_left == 0) {
		if (to_cnvlen > 0) {
		    to[to_cnvlen] = (wchar_t)'\0';
		    *state = XLookupChars;
		} else {
		    *state = XLookupNone;
		}
		return to_cnvlen;
	    }
	    if (to_left == 0)
		break;
	}
    }
		
    from_left = from_len;
    from_cnvlen = 0;
    to_cnvlen = 0;
    to_buf = (wchar_t *)NULL;
    for (;;) {
	from_savelen = from_left;
	to_left = MAXINT;
	from_buf = &from[from_cnvlen];
	if (_XlcConvert(conv, (XPointer *)&from_buf, &from_left,
				 (XPointer *)&to_buf, &to_left, NULL, 0) < 0) {
	    *state = XLookupNone;
	    return 0;
	}
	from_cnvlen += (from_savelen - from_left);
	to_cnvlen += (MAXINT - to_left);
	if (from_left == 0) {
	    if (to_cnvlen > 0)
		*state = XBufferOverflow;
	    else
		*state = XLookupNone;
	    break;
	}
    }
    return to_cnvlen;
}

Public int
_Ximp_mbs_charlen(im, mbstr, mbstr_len)
    Ximp_XIM 	 im;
    char	*mbstr;
    int		 mbstr_len;
{
    return _Ximp_ctstombs(im, mbstr, mbstr_len,
					(char *)NULL, 0, (Status *)NULL);
}

#define BUF_SIZE (20)
static unsigned char look[BUF_SIZE] = {0};	/* Clean up bss */

Public int
_Ximp_LookupMBText(ic, event, buffer, nbytes, keysym, status)
    Ximp_XIC ic;
    XKeyEvent *event;
    unsigned char *buffer;
    int nbytes;
    KeySym *keysym;
    XComposeStatus *status;
{
    Ximp_XIM im = (Ximp_XIM)ic->core.im;
    int count;
    KeySym symbol;

    count = im->methods->lookup_string(event, (char *)buffer,
						nbytes, &symbol, status);
    if (keysym) *keysym = symbol;
    if ((nbytes == 0) || (symbol == NoSymbol)) {
	return(count);
    }
    if (count == 0) {
	count = _XimConvertCharCode((char *)buffer, nbytes, symbol,
						im->ximp_impart->ctom_conv);
    } else if ((count != 1) || (buffer[0] >= 0x80)) { /* not ASCII Encoding */
	bcopy(buffer, look, count);
	if ((count = _Ximp_ctstombs(im,
		 (char *)look, count, (char *)buffer, count, NULL)) < 0) {
	    count = 0;
	}
    }
    return(count);
}

Public int
_Ximp_LookupWCText(ic, event, buffer, nbytes, keysym, status)
    Ximp_XIC ic;
    XKeyEvent *event;
    wchar_t *buffer;
    int nbytes;
    KeySym *keysym;
    XComposeStatus *status;
{
    Ximp_XIM im = (Ximp_XIM)ic->core.im;
    int count;
    KeySym symbol;

    count = im->methods->lookup_string(event, (char *)buffer, nbytes,
							&symbol, status);
    if (keysym) *keysym = symbol;
    if ((nbytes == 0) || (symbol == NoSymbol)) {
	return(count);
    }
    if (count == 0) { /* Not ISO 8859-1 Encoding */
	count = _XimConvertCharCode((char *)buffer, nbytes, symbol,
						im->ximp_impart->ctow_conv);
    } else if ((count != 1) || (*(char *)buffer >= 0x80)) { /* not ASCII Encoding */
	bcopy(buffer, look, count);
	if ((count = _Ximp_ctstowcs(im,
			  (char *)look, count, buffer, count, NULL)) < 0) {
	    count = 0;
	}
    }
    else { /* ASCII Encoding */
	buffer[0] = *(char *)buffer;
    }
    return(count);
}
