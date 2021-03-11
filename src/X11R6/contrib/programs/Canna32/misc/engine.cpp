XCOMM Copyright 1994 NEC Corporation, Tokyo, Japan.
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
XCOMM Configuration file for kana-kanji conversion engines.
XCOMM
XCOMM for Canna version 2.2
XCOMM $Id: engine.cpp,v 1.7 1994/03/24 06:11:06 kon Exp $

cannaserver LIBCANNADIR/libRKC    # Canna engine
irohaserver LIBCANNADIR/libRKC    # Canna engine (same as above)
jserver     LIBCANNADIR/libRKWnn  # Wnn4 engine
sj3serv     LIBCANNADIR/libRKSj3  # SJ3 engine
