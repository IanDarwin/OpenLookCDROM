XCOMM!/bin/sh
XCOMM Copyright 1992 NEC Corporation, Tokyo, Japan.
XCOMM
XCOMM Permission to use, copy, modify, distribute and sell this software
XCOMM and its documentation for any purpose is hereby granted without
XCOMM fee, provided that the above copyright notice appear in all copies
XCOMM and that both that copyright notice and this permission notice
XCOMM appear in supporting documentation, and that the name of NEC
XCOMM Corporation not be used in advertising or publicity pertaining to
XCOMM distribution of the software without specific, written prior
XCOMM permission.  NEC Corporation makes no representations about the
XCOMM suitability of this software for any purpose.  It is provided "as
XCOMM is" without express or implied warranty.
XCOMM
XCOMM NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
XCOMM INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
XCOMM NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
XCOMM CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
XCOMM USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
XCOMM OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
XCOMM PERFORMANCE OF THIS SOFTWARE. 
XCOMM

XCOMM $Id: dpbindic.cpp,v 4.3 1994/04/20 03:47:19 kon Exp $
#include "cannaconf.h"
PATH=CANNABINDIR:$PATH:/bin:/usr/bin:/etc:/usr/etc:/usr/nec/bin:/usr/ucb
export PATH

echo dpxdic $* 1>&2
dpxdic $*
