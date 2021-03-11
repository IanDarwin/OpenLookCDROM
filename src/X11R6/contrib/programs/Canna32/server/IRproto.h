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

/* @(#) NEC UNIX( PC-UX/EWS-UX ) IRproto.h 2.1 91/10/17 13:40:04 */
/* $Id: IRproto.h,v 2.4 1994/02/22 11:12:52 kon Exp $ */

#define RETURN_VERSION_ERROR_STAT   -2

#define OWNER		"bin"
#define EXIT		999

typedef struct _Req0{
    int Type ;
} Req0 ;

typedef struct _Req1{
    int Type ;
    int context ;
} Req1 ;

typedef struct _Req2{		
    int Type ;
    int namelen ;
    char *name ;
} Req2 ;

typedef struct _Req3{
    int Type ;
    int context ;
    int number ;
} Req3 ;

typedef struct _Req4{
    int Type ;
    int context ;
    int number ;
    int *kouho ;
} Req4 ;

typedef struct _Req5{
    int Type ;
    int context ;
    int number ;
    int datalen ;
} Req5 ;

typedef struct _Req6{
    int Type ;
    int context ;
    int mode ;
    int datalen ;
    char *data ;
} Req6 ;

typedef struct _Req7{
    int Type ;
    int context ;
    int diclen ;
    char *dicname ;
    int datalen ;
    char *datainfo ;
} Req7 ;

typedef struct _Req8{
    int Type ;
    int context ;
    int datalen ;
    char *data ;
    int mode ;
 } Req8 ;

typedef struct _Req9{
    int Type ;
    int context ;
    int number ;
    int datalen ;
    char *data ;
} Req9 ;

typedef struct _Req10{
    int Type ;
    int context ;
    int diclen ;
    char *dicname ;
    int textdiclen ;
    char *textdicname ;
    int mode ;
} Req10 ;

typedef struct _Req11{
    int Type ;
    int context ;
    int number ;
    int kouho ;
    int max ;
} Req11 ;

typedef struct _Req12{
    int Type ;
    int datalen ;
    char *data ;
    int number ;
} Req12 ;

typedef union _IRReq {
    Req0 Request0 ;
    Req1 Request1 ;
    Req2 Request2 ;
    Req3 Request3 ;
    Req4 Request4 ;
    Req5 Request5 ;
    Req6 Request6 ;
    Req7 Request7 ;
    Req8 Request8 ;
    Req9 Request9 ;
    Req10 Request10;
    Req11 Request11;
    Req12 Request12;
    BYTE Buffer[4];
} IRReq ;

#define     ACK_BUFSIZE   2048

typedef struct _Ack0 {
    int stat ;
} Ack0 ;

typedef struct _Ack1 {
    int stat ;
    int len ;
    char data[ ACK_BUFSIZE ] ;
} Ack1 ;

typedef struct _Ack2 {
    int stat ;
    int len ;
    int info[ ACK_BUFSIZE/sizeof( int ) ] ;
} Ack2 ;

typedef union _IRAck {
    Ack0 Anck0 ;
    Ack1 Anck1 ;
    Ack2 Anck2 ;

    char SendAckBuffer[ ACK_BUFSIZE + sizeof( int )*2 ] ;
} IRAck ;	
