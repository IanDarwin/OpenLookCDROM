/* Copyright 1993 NEC Corporation, Tokyo, Japan.
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

/* $Id: RK.h,v 2.17 1994/04/21 02:34:24 kon Exp $ */
#ifndef		_RK_h
#define		_RK_h

#ifndef _WCHAR_T
# if defined(WCHAR_T) || defined(_WCHAR_T_) || defined(__WCHAR_T) \
  || defined(_GCC_WCHAR_T)
#  define _WCHAR_T
# endif
#endif

typedef	struct {
   int		ylen;		/* yomigana no nagasa (in byte) */ 
   int		klen;		/* kanji no nagasa (in byte) */
   int		rownum;		/* row number */
   int		colnum;		/* column number */
   int		dicnum;		/* dic number */
}		RkLex;

typedef	struct {
   int		bunnum;		/* bunsetsu bangou */
   int		candnum;	/* kouho bangou */
   int		maxcand;  	/* sou kouho suu */
   int		diccand;	/* jisho ni aru kouho suu */
   int		ylen;		/* yomigana no nagasa (in byte) */ 
   int		klen;		/* kanji no nagasa (in byte) */
   int		tlen;		/* tango no kosuu */
}		RkStat;

struct DicInfo {
    unsigned char	*di_dic;
    unsigned char	*di_file;
    int			di_kind;
    int			di_form;
    unsigned		di_count;
    int			di_mode;
    long		di_time;
};

/* romaji/kanakanji henkan code */
#define	RK_XFERBITS	4	/* bit-field width */
#define	RK_XFERMASK	((1<<RK_XFERBITS)-1)
#define	RK_NFER		0	/* muhenkan */
#define	RK_XFER		1	/* hiragana henkan */
#define	RK_HFER		2	/* hankaku henkan */
#define	RK_KFER		3	/* katakana henkan */
#define	RK_ZFER		4	/* zenkaku  henkan */

#define	RK_CTRLHENKAN		0xf
#define	RK_HENKANMODE(flags)	(((flags)<<RK_XFERBITS)|RK_CTRLHENKAN)

#define RK_TANBUN		0x01
#define RK_MAKE_WORD		0x02
#define RK_MAKE_EISUUJI		0x04
#define RK_MAKE_KANSUUJI	0x08

/* RkRxDic
 *	romaji/kana henkan jisho 
 */
struct RkRxDic	{
    int                 dic;		/* dictionary version: see below */
    unsigned char	*nr_string;	/* romaji/kana taiou hyou */
    int			nr_strsz;	/* taiou hyou no size */
    unsigned char	**nr_keyaddr;	/* romaji key no kaishi iti */
    int			nr_nkey;	/* romaji/kana taiou suu */
    unsigned char       *nr_bchars;     /* backtrack no trigger moji */
    unsigned char       *nr_brules;     /* backtrack no kanouseino aru rule */
};

#define RX_KPDIC 0 /* new format dictionary */
#define RX_RXDIC 1 /* old format dictionary */

/* kanakanji henkan */

/* romaji hennkan code */
#define	RK_FLUSH	0x8000	/* flush */
#define	RK_SOKON	0x4000	/* sokuon shori */
#define RK_IGNORECASE	0x2000  /* ignore case */

#define	RK_BIN		0
#define	RK_TXT		0x01

#define	RK_MWD	        0
#define	RK_SWD		1
#define	RK_PRE		2
#define	RK_SUC		3

#define KYOUSEI		0x01		/* jisho_overwrite_mode */

#define	Rk_MWD		0x80		/* jiritsugo_jisho */
#define	Rk_SWD		0x40		/* fuzokugo_jisho */
#define	Rk_PRE		0x20		/* settougo_jisho */
#define	Rk_SUC		0x10		/* setsubigo_jisho */

/* permission for RkwChmod() */
#define RK_ENABLE_READ   0x01
#define RK_DISABLE_READ  0x02
#define RK_ENABLE_WRITE  0x04
#define RK_DISABLE_WRITE 0x08
/* chmod for directories */
#define RK_USR_DIR       0x3000
#define RK_GRP_DIR       0x1000
#define RK_SYS_DIR       0x2000
#define RK_DIRECTORY     (RK_USR_DIR | RK_GRP_DIR | RK_SYS_DIR)
/* chmod for dictionaries */
#define RK_USR_DIC       0	/* specify user dic */
#define RK_GRP_DIC       0x4000	/* specify group dic */
#define RK_SYS_DIC       0x8000	/* specify system dic */

#define PL_DIC		 0x0100
#define PL_ALLOW	 0x0200
#define PL_INHIBIT	 0x0400
#define PL_FORCE	 0x0800

#define	NOENT	-2	/* No such file or directory		*/
#define	IO	-5	/* I/O error				*/
#define	NOTALC	-6	/* Cann't alloc. 			*/
#define	BADF	-9	/* irregal argument			*/
#define	BADDR	-10	/* irregal dics.dir	 		*/
#define	ACCES	-13	/* Permission denied 			*/
#define	NOMOUNT	-15	/* cannot mount				*/
#define	MOUNT	-16	/* file already mounted			*/
#define	EXIST	-17	/* file already exits			*/
#define	INVAL	-22	/* irregal argument			*/
#define	TXTBSY	-26	/* text file busy			*/
#define BADARG	-99	/* Bad Argment				*/
#define BADCONT -100	/* Bad Context				*/

/* kanakanji henkan */

#if __STDC__ || defined( __cplusplus )
#ifdef __cplusplus
extern "C" {
#endif
#if defined(_WCHAR_T) && !defined(RK_INTERNAL)
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
		RkwSync(int, char *),
		RkwGetMountList(int, char *, int),
		RkwDefineDic(int, char *, wchar_t *),
		RkwDeleteDic(int, char *, wchar_t *),
		RkwBgnBun(int, wchar_t *, int, int),
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
		RkwSubstYomi(int, int, int, wchar_t *, int),
		RkwStoreYomi(int, wchar_t *, int),
		RkwGetLastYomi(int, wchar_t *, int),
		RkwFlushYomi(int),
		RkwRemoveBun(int, int),
		RkwGetStat(int, RkStat *),
		RkwGetYomi(int, wchar_t *, int),
		RkwGetHinshi(int, wchar_t *, int),
		RkwGetKanji(int, wchar_t *, int),
		RkwGetKanjiList(int, wchar_t *, int),
		RkwGetLex(int, RkLex *, int),
		RkwCvtHira(wchar_t *, int, wchar_t *, int),
		RkwCvtKana(wchar_t *, int, wchar_t *, int),
		RkwCvtHan(wchar_t *, int, wchar_t *, int),
		RkwCvtZen(wchar_t *, int, wchar_t *, int),
		RkwCvtEuc(wchar_t *, int, wchar_t *, int),
                RkwQueryDic(int, char *, char *, struct DicInfo *);
extern void	RkwCloseRoma(struct RkRxDic *);
struct RkRxDic *RkwOpenRoma(unsigned char *);
#endif

extern void	RkFinalize(void);
extern int	RkInitialize(char *),
		RkCreateContext(void),
    		RkCloseContext(int),
    		RkDuplicateContext(int),
		RkSetDicPath(int, char *),
		RkGetDirList(int, char *,int),
		RkGetDicList(int, char *,int),
		RkMountDic(int, char *, int),
		RkUnmountDic(int, char *),
		RkRemountDic(int, char *, int),
		RkSync(int, char *),
		RkGetMountList(int, char *, int),
		RkDefineDic(int, char *, char *),
		RkDeleteDic(int, char *, char *),
		RkBgnBun(int, char *, int, int),
		RkEndBun(int, int),
		RkGoTo(int, int),
		RkLeft(int),
		RkRight(int),
		RkXfer(int, int),
		RkNfer(int),
		RkNext(int),
		RkPrev(int),
		RkResize(int, int),
		RkEnlarge(int),
		RkShorten(int),
		RkSubstYomi(int, int, int, char *, int),
		RkStoreYomi(int, char *, int),
		RkGetLastYomi(int, char *, int),
		RkFlushYomi(int),
		RkRemoveBun(int, int),
		RkGetStat(int, RkStat *),
		RkGetYomi(int, unsigned char *, int),
		RkGetHinshi(int, unsigned char *, int),
		RkGetKanji(int, unsigned char *, int),
		RkGetKanjiList(int, unsigned char *, int),
		RkGetLex(int, RkLex *, int),
		RkCvtHira(unsigned char *, int, unsigned char *, int),
		RkCvtKana(unsigned char *, int, unsigned char *, int),
		RkCvtHan(unsigned char *, int, unsigned char *, int),
		RkCvtZen(unsigned char *, int, unsigned char *, int),
		RkCvtEuc(unsigned char *, int, unsigned char *, int),
                RkQueryDic(int, char *, char *, struct DicInfo *);
#ifdef __cplusplus
}
#endif
#else
int		RkwInitialize(), RkInitialize();
void		RkwFinalize(), RkFinalize();
int		RkwCreateContext(), RkwCloseContext(), RkwDuplicateContext();
int		RkCreateContext(), RkCloseContext(), RkDuplicateContext();

/* dictionary search path */
int		RkwSetDicPath(), RkSetDicPath();
int		RkwGetDirList(), RkGetDirList();
int		RkwGetDicList(), RkGetDicList();
/* mount control */
int		RkwMountDic(), RkwUnmountDic(), RkwRemountDic();
int		RkMountDic(), RkUnmountDic(),  RkRemountDic();
int		RkwGetMountList(), RkGetMountList();
int             RkwSync(), RkSync();
/* special effects */
int		RkwDefineDic(), RkDefineDic();
int		RkwDeleteDic(), RkDeleteDic();

/* henkan */
int		RkwBgnBun(), RkBgnBun();
int		RkwEndBun(), RkEndBun();

int		RkwGoTo(), RkwLeft(), RkwRight();
int		RkGoTo(), RkLeft(), RkRight();
int		RkwXfer(), RkwNfer(), RkwNext(), RkwPrev();
int		RkXfer(), RkNfer(), RkNext(), RkPrev();
int		RkwResize(), RkwEnlarge(), RkwShorten();
int		RkResize(), RkEnlarge(), RkShorten();
int		RkwSubstYomi(), RkSubstYomi();
int		RkwStoreYomi(), RkSubstYomi();
int		RkwGetLastYomi(), RkGetLastYomi();
int		RkwFlushYomi(), RkFlushYomi();
int		RkwRemoveBun(), RkRemoveBun();

/* queries */
int		RkwGetStat();
int		RkGetStat();

int		RkwGetYomi(), RkGetYomi();
int		RkwGetHinshi(), RkGetHinshi();
int		RkwGetKanji(), RkGetKanji();
int		RkwGetKanjiList(), RkGetKanjiList();
int		RkwGetLex(), RkGetLex();
int		RkwCvtHira(), RkwCvtKana(), RkwCvtHan(),
		RkwCvtZen(), RkwCvtEuc();
int		RkCvtHira(), RkCvtKana(), RkCvtHan(), RkCvtZen(), RkCvtEuc();
int             RkQueryDic(), RkwQueryDic();
extern void	RkwCloseRoma();
struct RkRxDic *RkwOpenRoma();
#endif /*  __STDC__ || defined( __cplusplus ) */

#ifdef ENGINE_SWITCH
struct rkfuncs {
#if __STDC__ || defined( __cplusplus )
  int (*GetProtocolVersion)(int *, int *);
  char *(*GetServerName)(void);
  int (*GetServerVersion)(int *, int *);
  int (*Initialize)(char *);
  void (*Finalize)(void);
  int (*CreateContext)(void);
  int (*DuplicateContext)(int);
  int (*CloseContext)(int);
  int (*SetDicPath)(int, char *);
  int (*CreateDic)(int, unsigned char *, int);
  int (*SyncDic)(int, char *);
  int (*GetDicList)(int, char *, int);
  int (*GetMountList)(int, char *, int);
  int (*MountDic)(int, char *, int);
  int (*RemountDic)(int, char *, int);
  int (*UnmountDic)(int, char *);
  int (*DefineDic)(int, char *, wchar_t *);
  int (*DeleteDic)(int, char *, wchar_t *);
  int (*GetHinshi)(int, wchar_t *, int);
  int (*GetKanji)(int, wchar_t *, int);
  int (*GetYomi)(int, wchar_t *, int);
  int (*GetLex)(int, RkLex *, int);
  int (*GetStat)(int, RkStat *);
  int (*GetKanjiList)(int, wchar_t *, int);
  int (*FlushYomi)(int);
  int (*GetLastYomi)(int, wchar_t *, int);
  int (*RemoveBun)(int, int);
  int (*SubstYomi)(int, int, int, wchar_t *, int);
  int (*BgnBun)(int, wchar_t *, int, int);
  int (*EndBun)(int, int);
  int (*GoTo)(int, int);
  int (*Left)(int);
  int (*Right)(int);
  int (*Next)(int);
  int (*Prev)(int);
  int (*Nfer)(int);
  int (*Xfer)(int, int);
  int (*Resize)(int, int);
  int (*Enlarge)(int);
  int (*Shorten)(int);
  int (*StoreYomi)(int, wchar_t *, int);
  int (*SetAppName)(int char *);
#else /* !(__STDC__ || defined( __cplusplus )) */
  int (*GetProtocolVersion)();
  char *(*GetServerName)();
  int (*GetServerVersion)();
  int (*Initialize)();
  void (*Finalize)();
  int (*CreateContext)();
  int (*DuplicateContext)();
  int (*CloseContext)();
  int (*SetDicPath)();
  int (*CreateDic)();
  int (*SyncDic)();
  int (*GetDicList)();
  int (*GetMountList)();
  int (*MountDic)();
  int (*RemountDic)();
  int (*UnmountDic)();
  int (*DefineDic)();
  int (*DeleteDic)();
  int (*GetHinshi)();
  int (*GetKanji)();
  int (*GetYomi)();
  int (*GetLex)();
  int (*GetStat)();
  int (*GetKanjiList)();
  int (*FlushYomi)();
  int (*GetLastYomi)();
  int (*RemoveBun)();
  int (*SubstYomi)();
  int (*BgnBun)();
  int (*EndBun)();
  int (*GoTo)();
  int (*Left)();
  int (*Right)();
  int (*Next)();
  int (*Prev)();
  int (*Nfer)();
  int (*Xfer)();
  int (*Resize)();
  int (*Enlarge)();
  int (*Shorten)();
  int (*StoreYomi)();
  int (*SetAppName)();
#endif /* !(__STDC__ || defined( __cplusplus )) */
};
#endif /* ENGINE_SWITCH */

#endif	/* _RK_h */
/* don't add stuff after this line */
