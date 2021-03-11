/* Copyright 1992 NEC Corporation, Tokyo, Japan.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of NEC
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  NEC Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
 * NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
 * OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE. 
 */

/* sccs_id[]="%Z% NEC UNIX( PC-UX/EWS-UX ) %M% %R%.%L% %E% %U%"; */
/* $Id: IRwproto.h,v 2.4 1994/03/01 12:04:38 kon Exp $ */

typedef struct _wReq0{		
    int Type ;
    int namelen ;
    char *name ;
} wReq0 ;

typedef struct _wReq1{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
} wReq1 ;

typedef struct _wReq2{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
} wReq2 ;

typedef struct _wReq3{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    unsigned short buflen ;
} wReq3 ;

typedef struct _wReq4{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    short begin ;
    short end ;
    unsigned short yomilen ;
    Ushort *yomi ;
} wReq4 ;

typedef struct _wReq5{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    unsigned short size ;
    int mode ;
} wReq5 ;

typedef struct _wReq6{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    short number ;
    unsigned short buflen ;
} wReq6 ;

typedef struct _wReq7{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    short number ;
    short yomilen ;
} wReq7 ;

typedef struct _wReq8{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    short curbun ;
    short curkouho ;
    unsigned short size ;
} wReq8 ;

typedef struct _wReq9{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    short number ;
    short kouho ;
    short max ;
} wReq9 ;

typedef struct _wReq10{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    short number ;
    int mode ;
    short *kouho ;
} wReq10 ;

typedef struct _wReq11{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    short curbun ;
    Ushort *yomi ;
} wReq11 ;

typedef struct _wReq12{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    char *dicname ;
    Ushort *datainfo ;
} wReq12 ;

typedef struct _wReq13{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    char *dicname ;
    Ushort *yomi ;
    unsigned short yomilen ;
    unsigned short kouhosize ;
    unsigned short hinshisize ;
} wReq13 ;

typedef struct _wReq14{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    int mode ;
    short context ;
    Ushort *yomi ;
} wReq14 ;

typedef struct _wReq15{
    BYTE type;
    BYTE none;
    Ushort datalen ;
    int mode ;
    short context ;
    char *dicname ;
} wReq15 ;

typedef struct _wReq17{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    char *dicname ;
    char mode ;
} wReq17 ;

typedef struct _wReq18{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    char *data ;
    unsigned short size ;
} wReq18 ;

typedef struct _wReq20{
    BYTE type;
    BYTE none;
    unsigned short datalen ;
    short context ;
    int command ;
    int bufsize ;
    char *buf ;
} wReq20 ;

typedef struct _wReq21{
    BYTE type;
    BYTE none;
    Ushort datalen;
    int mode;
    short context;
    char *dirname;
    char *srcdic;
    char *dstdic;
} wReq21 ;

typedef union _IRwReq {
    wReq0 type0 ;
    wReq1 type1 ;
    wReq2 type2 ;
    wReq3 type3 ;
    wReq4 type4;
    wReq5 type5;
    wReq6 type6 ;
    wReq7 type7 ;
    wReq8 type8;
    wReq9 type9;
    wReq10 type10 ;
    wReq11 type11;
    wReq12 type12 ;
    wReq13 type13;
    wReq14 type14 ;
    wReq15 type15 ;
    wReq17 type17;
#ifdef EXTENSION
    wReq18 type18;
#endif /* EXTENSION */
    wReq20 type20;
    wReq21 type21;
    BYTE Buffer[4];
} IRwReq ;

