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

/* sccs_id[]="@(#) NEC UNIX( PC-UX/EWS-UX ) rkc.h 2.4 91/11/11 12:01:18"; */
/* #ident	"@(#) NEC/V(386) R3.0B rkc.h 5.9 90/03/26 21:04:36" */
/* $Id: rkc.h,v 2.9 1994/04/21 02:31:21 kon Exp $ */

#ifdef ENGINE_SWITCH
#include "RKrename.h"
#endif

#if defined(SYSV) || defined(SVR4) || __STDC__
# if defined(SYSV) || defined(SVR4)
#  include <memory.h>
# endif
# ifndef bzero
#  define bzero(buf, size) memset((char *)(buf), 0x00, (size))
# endif
# ifndef bcopy
#  define bcopy(src, dst, size) memcpy((char *)(dst), (char *)(src), (size))
# endif
#endif

/* 文節情報レコード
 *
 */

#define MAX_HOSTNAME	256

typedef struct _RkcBun {
    unsigned short  *kanji  ;	/* 第一候補または全候補列 */
    short	    curcand ;	/* カレント漢字候補番号 */
    short	    maxcand ;	/* 漢字候補総数 */
    short	    flags   ;	/* フラグ */
#define NOTHING_KOUHO	 0x00
#define FIRST_KOUHO	0x01	   /* kanjiは先頭候補のみ */
#define NUMBER_KOUHO	0x02	   /* kanjiは候補一覧のポインタ */
} RkcBun ;			   /* この場合、curcandは0,maxcandは1 */


/*
 *  クライアントコンテクストレコ−ド
 *
 */
typedef struct _RkcContext {
    short	    server ;  /* サ−バ・コンテクスト番号 */
    short	    client ;  /* クライアント・コンテクスト番号 */
    RkcBun	    *bun   ;  /* 文節情報レコード配列へのポインタ */
    unsigned short *Fkouho ; /* 第一候補列へのポインタ */
    short	    curbun ;  /* カレント文節番号 */
    short	    maxbun ;  /* 文節総数 */
    short	    bgnflag ; /* RkBgnBunのフラグ */
    unsigned short *lastyomi;
    short	    maxyomi;
} RkcContext ;

extern int ushort2euc(), euc2ushort(), ushort2wchar(), wchar2ushort(),
    wcharstrlen(), ushortstrlen(), ushortstrcpy() ;

#if __STDC__
#include <stdlib.h>
#define pro(x) x
#else
extern char *malloc(), *realloc(), *calloc();
extern void free();
#define pro(x) ()
#endif

struct rkcproto {
  int (*initialize) pro((char *));
  int (*finalize) pro((void));
  int (*close_context) pro((RkcContext *));
  int (*create_context) pro((void));
  int (*duplicate_context) pro((RkcContext *));
  int (*dictionary_list) pro((RkcContext *, char *, int));
  int (*define_dic) pro((RkcContext *, char *, Ushort *));
  int (*delete_dic) pro((RkcContext *, char *, Ushort *));
  int (*mount_dictionary) pro((RkcContext *, char *, int));
  int (*remount_dictionary) pro((RkcContext *, char *, int));
  int (*umount_dictionary) pro((RkcContext *, char *));
  int (*mount_list) pro((RkcContext *, char *, int));
  int (*convert) pro((RkcContext *, Ushort *, int, int));
  int (*convert_end) pro((RkcContext *, int));
  int (*get_kanji_list) pro((RkcContext *));
  int (*get_stat) pro((RkcContext *, RkStat *));
  int (*resize) pro((RkcContext *, int));
  int (*store_yomi) pro((RkcContext *, Ushort *, int));
  int (*get_yomi) pro((RkcContext *, Ushort *));
  int (*get_lex) pro((RkcContext *, int, RkLex *));
  int (*autoconv) pro((RkcContext *, int, int));
  int (*subst_yomi) pro((RkcContext *, int, int, int, Ushort *, int));
  int (*flush_yomi) pro((RkcContext *));
  int (*get_last_yomi) pro((RkcContext *, Ushort *, int));
  int (*remove_bun) pro((RkcContext *, int));
  int (*get_simple_kanji)
    pro((RkcContext *, char *, Ushort *, int, Ushort *, int, Ushort *, int));
  int (*query_dic) pro((RkcContext *, char *, char *, struct DicInfo *));
  int (*get_hinshi) pro((RkcContext *, Ushort *, int));
  int (*store_range) pro((RkcContext *, Ushort *, int));
  int (*set_locale) pro((RkcContext *, char *));
  int (*set_app_name) pro((RkcContext *, char *));
  int (*notice_group_name) pro((RkcContext *, char *));
  int (*through) pro((RkcContext *, int, char *, int, int));
#ifdef EXTENSION
  int (*list_dictionary) pro((RkcContext *, char *, char *, int));
  int (*create_dictionary) pro((RkcContext *, char *, int));
  int (*remove_dictionary) pro((RkcContext *, char *, int));
  int (*rename_dictionary) pro((RkcContext *, char *, char *, int));
  int (*get_text_dictionary)
    pro((RkcContext *, char *, char *, Ushort *, int));
  int (*sync) pro((RkcContext *, char *));
  int (*chmod_dic) pro((RkcContext *, char *, int));
  int (*copy_dictionary) pro((RkcContext *, char *, char *, char *, int));
#endif /* EXTENSION */
};

/* BASIC TYPE:
 *	subete no data ha MSB first(Motorolla order) de tenkai sareru
 *		unsigned char	w
 *		unsigned short	wx
 *		unsigned long	wxyz
 */	
#define LOMASK(x)	((x)&255)
#define	LTOL4(l, l4)	{\
	(l4)[0] = LOMASK((l)>>24); (l4)[1] = LOMASK((l)>>16);\
	(l4)[2] = LOMASK((l)>> 8); (l4)[3] = LOMASK((l));\
}
#define	LTOL3(l, l3)	{\
(l3)[0] = LOMASK((l)>>16); (l3)[1] = LOMASK((l)>> 8); (l3)[2] = LOMASK((l));\
}
#define	STOS2(s, s2)	{\
	(s2)[0] = LOMASK((s)>> 8); (s2)[1] = LOMASK((s));\
}

#define RK_LINE_BMAX 1024 /* これは RKintern.h のと同じ値でなければならない */

#if 0
#define I16toI32(x) (((x) & 0x8000) ? ((x) | 0xffff8000) : (x))
#endif
#define I16toI32(x) (x)
#define I8toI32(x) (((x) & 0x80) ? ((x) | 0xffffff80) : (x))

#ifndef YES
#define YES 1
#endif
#ifndef NO
#define NO  0
#endif

#define SIZEOFSHORT 2 /* for protocol */
#define SIZEOFLONG  4 /* for protocol */

#define MAX_CX 100
