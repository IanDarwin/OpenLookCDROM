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
/* $Id: IR.h,v 5.16 1994/04/19 11:37:59 kon Exp $ */

#include "cannaconf.h"
#define EXTENSION
#define DEBUG

#if defined(SYSV) || defined(SVR4) || __STDC__
# if defined(SYSV) || defined(SVR4)
#  include    <memory.h>
# endif
# ifndef bcopy
#  define bcopy(src, dst, size) memcpy((char *)(dst), (char *)(src), (size))
# endif
# ifndef bzero
#  define bzero(buf, size) memset((char *)(buf), 0x00, (size))
# endif
#endif

#ifdef pcux

#include     <sys/kdef.h>

#else /* pcux */

#include    <sys/param.h>

#if __STDC__
#include <stdlib.h>
#include <time.h>
#include <string.h>
#else
extern char *malloc(), *realloc(), *calloc();
extern void free();
extern char *ctime(), *strtok();
#endif

#ifdef WCHAR_T
#undef WCHAR_T
#define WCHAR_was_defined
#endif
#ifdef _WCHAR_T
#undef _WCHAR_T
#define WCHAR_was_defined
#endif
#ifdef _WCHAR_T_
#undef _WCHAR_T_
#define WCHAR_was_defined
#endif
#ifdef __WCHAR_T
#undef __WCHAR_T
#define WCHAR_was_defined
#endif
#ifdef _GCC_WCHAR_T
#undef _GCC_WCHAR_T
#define WCHAR_was_defined
#endif

#include    "protodefs.h"
#include    "canna/RK.h"
#include    "IRproto.h"
#include    "IRwproto.h"

#if __STDC__ && defined(WCHAR_was_defined)
/* RK.h の wchar_t 関連の関数のプロトタイプ宣言の代わりにここで宣言する */
extern void	RkwFinalize(void);
extern int	RkwInitialize(char *),
		RkwCreateContext(void),
		RkwCloseContext(int),
		RkwDuplicateContext(int), 
		RkwSetDicPath(int, char *),
		RkwGetDirList(int, char *,int),
		RkwGetDicList(int, char *,int),
		RkwMountDic(int, char *, int),
		RkwUnmountDic(int, char *),
		RkwRemountDic(int, char *, int),
		RkwGetMountList(int, char *, int),
		RkwDefineDic(int, char *, Ushort *),
		RkwDeleteDic(int, char *, Ushort *),
		RkwBgnBun(int, Ushort *, int, int),
		RkwEndBun(int, int),
		RkwGoTo(int, int),
		RkwLeft(int),
		RkwRight(int),
		RkwXfer(int, int),
		RkwNfer(int),
		RkwNext(int),
		RkwPrev(int),
		RkwResize(int, int),
		RkwEnlarge(int),
		RkwShorten(int),
		RkwSubstYomi(int, int, int, Ushort *, int),
		RkwStoreYomi(int, Ushort *, int),
		RkwGetLastYomi(int, Ushort *, int),
		RkwFlushYomi(int),
		RkwRemoveBun(int, int),
		RkwGetStat(int, RkStat *),
		RkwGetYomi(int, Ushort *, int),
		RkwGetKanji(int, Ushort *, int),
		RkwGetKanjiList(int, Ushort *, int),
		RkwGetLex(int, RkLex *, int),
		RkwCvtHira(Ushort *, int, Ushort *, int),
		RkwCvtKana(Ushort *, int, Ushort *, int),
		RkwCvtHan(Ushort *, int, Ushort *, int),
		RkwCvtZen(Ushort *, int, Ushort *, int),
		RkwCvtEuc(Ushort *, int, Ushort *, int);

/* struct DicInfo *RkwQueryDic(int, Ushort *, struct DicInfo *); */
#endif

#ifdef DEBUG
#define ir_debug( cannadebug )	 cannadebug
#else
#define ir_debug( cannadebug )	
#endif

#define DDPATH              "canna"
#define DDUSER              "user"
#define DDGROUP             "group"
#define DDPATHLEN           (sizeof(DDPATH) - 1)
#define DDUSERLEN           (sizeof(DDUSER) - 1)
#define DDGROUPLEN          (sizeof(DDGROUP) - 1)

#define DATE_LENGH	    29
#define GETDATE 	    1
#define CONNECT 	    2
#define SETTIME 	    3
#define GETTIME 	    4

#define N_INIT_CONTEXTS	    8 /* クライアントは８個ぐらいしか使わんだろう */
#define N_ADD_CONTEXTS	    4

/* ユーザのクライアント管理テーブル */
typedef struct _Client {
    int 	id ;			     /* ソケット番号 */
    int 	usr_no ;		     /* ユーザ管理番号 */
    short 	version_hi ;		     /* protocol major version */
    short 	version_lo ;		     /* protocol miner version */
    long	used_time ;		     /* ユーザ消費時間 */
    long	idle_date ;		     /* アイドル時間 */
    long	connect_date ;		     /* コネクトした時間 */
    char	*username ;		     /* ユーザ名  */
    char	*groupname;		     /* グループ名  */
    char	*hostname ;		     /* ホスト名  */
    unsigned long	hostaddr;	     /* ホストアドレス */
    int 	pcount[ W_MAXREQUESTNO ] ;   /* プロトコルカウント */
    int		*context_flag;               /* コンテクスト管理フラグ */
    int		cfsize, ncon;		     /* 上のテーブルの大きさ管理 */
    char	*clientname ;		     /* クライアント名  */
} ClientRec ;			

typedef struct _UserTabl {
    char *username ;
    int count ;
} UsrTabl ;

typedef struct _ClientStat {
    int 	id ;			     /* ソケット番号 */
    int 	usr_no ;		     /* ユーザ管理番号 */
    long	used_time ;		     /* ユーザ消費時間 */
    long	idle_date ;		     /* アイドル時間 */
    long	connect_date ;		     /* コネクトした時間 */
    int 	pcount[ OLD_MAXREQUESTNO ] ; /* プロトコルカウント */
    char	username[ NAME_LENGTH+1] ;   /* ユーザ名  */
    char	hostname[ HOST_NAME ] ;      /* ホスト名  */
    char	context_flag[ OLD_MAX_CX ] ;	 /* コンテクスト管理フラグ */
} ClientStatRec ;		

#define IR_NO_ADDRESS   ((unsigned long)-1)
#define IR_UNIX_ADDRESS 0

typedef struct _AccessControlList {
    struct _AccessControlList  *prev ;
    struct _AccessControlList  *next ;
    char *hostname ;
    char *usernames ;
    int  usercnt ;
    unsigned long hostaddr;		/* host address */
} ACLRec ;

typedef struct _Client *ClientPtr ;
typedef struct _ClientStat *ClientStatPtr ;
typedef struct _UserTabl *UserTblPtr ;
typedef struct _AccessControlList *ACLPtr ;

#define LOCAL_BUFSIZE		2048
/*
#define BUFWATERMARK		8192	
#define MAXBUFSIZE (1 << 18)
*/

/* long が 64 ビットだったりするとまずい */
#define LENTODATA( len, data ) { \
				long work ; \
				work = htonl( (len) ) ; \
				bcopy( (char *)&work, (char *)(data), sizeof( long ) ) ; \
				}

/* long が 64 ビットだったりするとまずい */
#define DATATOLEN( data, len ) { \
				long work ; \
				bcopy( (char *)(data), (char *)&work, sizeof( long ) ) ; \
				len = ntohl( (work) ) ; \
				}

#endif /* pcux */

#define INITSOCKS 32
#define ADDSOCKS 32
/* checkohmori */

extern unsigned long mskcnt, connow_socks;

#define BITMASK(i) ((mskcnt == 1) ? (1 << (i)) : (1 << ((i) & 31)))
#define MASKIDX(i) ((mskcnt == 1) ? 0 : ((i) >> 5))

#define MASKWORD(buf, i) buf[MASKIDX(i)]
#define BITSET(buf, i) MASKWORD(buf, i) |= BITMASK(i)
#define BITCLEAR(buf, i) MASKWORD(buf, i) &= ~BITMASK(i)
#define GETBIT(buf, i) (MASKWORD(buf, i) & BITMASK(i))

#define COPYBITS(src, dst) \
  bcopy((caddr_t)src, (caddr_t)dst, mskcnt * sizeof(long))
#define CLEARBITS(buf) bzero((caddr_t)buf, mskcnt * sizeof(long))
#define MASKANDSETBITS(dst, b1, b2)  \
		      { int cri;			\
			for (cri=0; cri<mskcnt; cri++)	\
			  dst[cri] = (b1[cri] & b2[cri]); }
#define ORBITS(dst, b1, b2)  \
		      { int cri;			\
		      for (cri=0; cri<mskcnt; cri++)	\
			  dst[cri] = (b1[cri] | b2[cri]); }
#define UNSETBITS(dst, b1) \
		      { int cri;			\
		      for (cri=0; cri<mskcnt; cri++)	\
			  dst[cri] &= ~b1[cri];  }


