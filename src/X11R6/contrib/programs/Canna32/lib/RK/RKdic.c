/* Copyright 1994 NEC Corporation, Tokyo, Japan.
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

#if !defined(lint) && !defined(__CODECENTER__)
static char rcsid[]="@(#) 102.1 $Id: RKdic.c,v 2.38 1994/06/01 06:54:16 misao Exp $";
#endif
/*LINTLIBRARY*/

#include "RKintern.h"

#if defined(USG) || defined(SYSV) || defined(SVR4)
#include <string.h>
#else
#include <strings.h>
#endif

#define dm_td		dm_extdata.ptr
#define cx_gwt		cx_extdata.ptr
#define Is_Gwt_CTX(cx)

#define ND2RK(s)	((0x80 >> (int)(s)) & 0xff)
#define	STRCMP(d, s)	strcmp((char *)(d), (char *)(s))

#define FREQ_TEMPLATE	"freq%d.fq"
#define USER_TEMPLATE	"user%d.t"
#define PERM_TEMPLATE	"bin%d.d"
#define DEFAULT_PERMISSION	"w"

/* locatepath -- 辞書サーチパスを mode に応じて張る

   return value:
       0: 成功
   ACCES: エラー(グループを指定したのに DDPATH に存在しない)
 */

static int
locatepath(userDDP, ddpath, mode)
struct DD *userDDP[], *ddpath[];
int mode;
{
  /* find dictionary under system and user/group directory */
  if (mode & RK_GRP_DIC) {
    if (ddpath[1] && ddpath[2]) {
      /* グループ辞書とシステム辞書がちゃんとあれば */
      userDDP[0] = ddpath[1];
    }
    else {
      return ACCES;
    }
  }
  else { /* ユーザ辞書 */
    userDDP[0] = ddpath[0];
  }
  userDDP[1] = (struct DD*)0;
  return 0;
}


/* int
 * RkwCreateDic(cx_num, dicname, mode)
 *
 * 引き数
 *         int            cx_num    コンテクストナンバー
 *	   unsigned char  *dicname  辞書へのポインタ
 *	   int            mode      辞書の種類と強制モードのOR
 *             辞書の種類 
 *                 #define	Rk_MWD		0x80
 *                 #define	Rk_SWD		0x40
 *                 #define	Rk_PRE		0x20
 *                 #define	Rk_SUC		0x10
 *             強制モード
 *                 #define KYOUSEI	        0x01
 *                 強制しない場合           0x00
#define PL_DIC		(0x0100)
#define PL_ALLOW	(PL_DIC << 1)
#define PL_INHIBIT	(PL_DIC << 2)
#define PL_FORCE	(PL_DIC << 3)
 *
 * リターン値
 *             成功した場合                                 0
 *             成功した場合(上書きした場合)                 1
 *             アロケーションに失敗した場合                -6  NOTALC
 *             辞書がバイナリ辞書であった場合              -9    BADF
 *             dics.dirに異常があった場合                 -10   BADDR
 *             GetDicFilename返り値が-1の場合           -13   ACCES
 *             MakeDicFileに失敗した場合                  -13   ACCES
 *             CreatDicに失敗した場合                     -13   ACCES
 *             辞書がマウント中であった場合               -16   MOUNT
 *             辞書がすでにある場合(強制でない場合)       -17   EXIST
 *             辞書が使用中であった場合                   -26  TXTBSY
 *             modeが異常値であった場合                   -99  BADARG
 *             コンテクスト構造体が存在しない場合        -100 BADCONT
 */

int
RkwCreateDic(cx_num, dicname, mode)
int cx_num;
unsigned char *dicname;
int     mode;
{
  struct RkParam	*sx = RkGetSystem();
  struct RkContext	*cx = RkGetContext(cx_num);
  struct DM             *sm, *um, *tm;
  int			type;
  struct DD		*userDDP[2], *systemDDP[2];
  char            	spec[RK_LINE_BMAX];
  char	      		*filename, extent[5];
  
  if(!dicname || !dicname[0])
    return ACCES;
  if (strlen((char *)dicname) >= (unsigned)RK_NICK_BMAX) {
    return INVAL;
  }
  if ( !cx || !cx->ddpath || !cx->ddpath[0] )
      return BADCONT;
  if ( !sx || !sx->ddpath || !sx->ddpath[0] )
      return BADCONT;
  if ( cx->ddpath[0] == sx->ddpath[0] )
      return BADCONT;

  if (locatepath(userDDP, cx->ddpath, mode) < 0) {
    return ACCES;
  }
  if (!(userDDP[0]->dd_flags & DD_WRITEOK)) {
    return ACCES;
  }

  systemDDP[0] = sx->ddpath[0];
  systemDDP[1] = 0;

  type = (mode & PL_DIC) ? DF_FREQDIC : DF_TEMPDIC;
/* find dictionary in current mount list */
  sm = _RkSearchDDQ(systemDDP, (char *)dicname, type);
  um = _RkSearchDDQ(userDDP, (char *)dicname, type);

  if (um && !(mode & KYOUSEI)) {
    return EXIST;
  }

  if (mode & PL_DIC) {
    if (!sm) {
      if(_RkSearchDDQ(systemDDP, (char *)dicname, DF_TEMPDIC))
	return BADF;
      return NOENT;
    }
    if (!um) {
      struct DM	*dm;
      
      if (!(filename = _RkCreateUniquePath(userDDP[0], FREQ_TEMPLATE)))
	return ACCES;
      (void)sprintf(spec, "%s(%s) -%s--%s-\n",
		    filename, sm->dm_dicname, sm->dm_nickname,
		    DEFAULT_PERMISSION);
      if (!DMcheck(spec, dicname))
	return(NOENT);
      if (!(dm = DMcreate(userDDP[0], spec)))
	return NOTALC;
      if (copyFile(sm, dm))
	return ACCES;
      else {
	return 0;
      }
    } else {
      if (!(ND2RK(um->dm_class) & mode))
	return INVAL;
      if ( um->dm_rcount > 0 ) 
	return TXTBSY;
      if ( !um->dm_file )
	return BADCONT;	/* INVAL SHOULD BE REPLACED... MAKO 1225 */

      if(_RkRealizeDF(um->dm_file)) /* これいらないんじゃない？ kon 1993.11 */
	return ACCES;
      if ( copyFile(sm, um) )
	   return ACCES;
      else
           return 1;
    };
  } else {
    /*    um = _RkSearchDDQ(userDDP, (char *)dicname, DF_TEMPDIC);*/
    tm = _RkSearchDDP(userDDP, (char *)dicname);
    if (tm != um)
      return BADF;
    if (!um) {
      if (!(filename = _RkCreateUniquePath(userDDP[0], USER_TEMPLATE)))
	return ACCES;
      if (mode & Rk_MWD) {
	  (void)strcpy(extent, "mwd");
      } else if (mode & Rk_SWD) {
	  (void)strcpy(extent, "swd");
      } else if (mode & Rk_PRE) {
	  (void)strcpy(extent, "pre");
      } else if (mode & Rk_SUC) {
	  (void)strcpy(extent, "suc");
      } else {
	  /* return INVAL;	*/
	  (void)strcpy(extent, "mwd");
      };
      (void)sprintf(spec, "%s(.%s) -%s--%s-\n", filename, extent, dicname,
		    DEFAULT_PERMISSION);
      if (!DMcheck(spec, dicname))
	return(NOENT);
      if (!DMcreate(userDDP[0], spec))
	return NOTALC;
      _RkRealizeDD(userDDP[0]);
      return 0;
    } else {
      sprintf(spec, "%s(%s) -%s--%s%s-\n",
	      um->dm_file->df_link, um->dm_dicname, um->dm_nickname,
	      (um->dm_flags & DM_READOK) ? "r" : "",
	      (um->dm_flags & DM_WRITEOK) ? "w" : "");
      if(_RkRealizeDF(um->dm_file))
	return ACCES;
      return 1;	/* backward compatiblity ... 1224 Xmas */
    };
  }
}

copyFile(src, dst)
struct DM	*src;
struct DM	*dst;
{
  struct DF	*srcF = src->dm_file;
  struct DD	*srcD = srcF->df_direct;
  struct DF	*dstF = dst->dm_file;
  struct DD	*dstD = dstF->df_direct;
  char		*srcN, *dstN;
  int		srcFd, dstFd;
  int		n;
  int		ecount = 0;

  srcN = _RkCreatePath(srcD, srcF->df_link);
  if (srcN) {
    srcFd = open(srcN, 0);
    (void)free(srcN);
    if (srcFd >= 0) {
      dstN = _RkCreatePath(dstD, dstF->df_link);
      if (dstN) {
	dstFd = creat(dstN, 0666);
	(void)free(dstN);
	if (dstFd >= 0) {
	  char b[RK_BUFFER_SIZE];

	  _RkRealizeDD(dstD);

	  while ((n = read(srcFd, b, sizeof(b))) > 0) { /* do copy */
	    if ( write(dstFd, b, n) != n ) {
	      ecount++;
	      break;
	    }
	  }
	  if (close(dstFd) < 0 || n < 0) {
	    ecount++;
	  }
	}
      }
      close(srcFd);
    }
  }
  return ecount ? -1 : 0;
}

/*
 * RkwListDic(cx_num, dirname, buf, size)
 * int  cx_num;             コンテクストナンバー
 * unsigned char *dirname;  辞書リストを出力したいディレクトリ名
 * unsigned char *buf;      辞書リストが返ってくるバッファ
 * int  size;               バッファのサイズ
 *
 * リターン値               
 *             成功した場合                      辞書の数
 *             コンテクストナンバーが負の場合          BADCONT
 *             RkwCreateContextに失敗した場合           BADCONT
 *             RkwSetDicPathに失敗した場合              NOTALC
 */
RkwListDic( cx_num, dirname, buf, size )
int  cx_num;
unsigned char *dirname;
unsigned char *buf;
int  size;
{
  int dicscnt;
  int new_cx_num;

  if(!dirname || !strlen((char *)dirname))
    return 0;
  if (cx_num < 0)
    return BADCONT;
  if((new_cx_num = RkwCreateContext()) < 0)
    return BADCONT;
  if (RkwSetDicPath(new_cx_num, (char *)dirname) == -1) {
    RkwCloseContext(new_cx_num);
    return NOTALC;
  }
  dicscnt = RkwGetDicList(new_cx_num, (char *)buf, size);
  (void)RkwCloseContext(new_cx_num);
  return (dicscnt);
}

/* int
 * RkwRemoveDic(cx_num, dicname, mode)
 *
 * 指定されたコンテクストに指定された辞書が存在すれば
 * その辞書を削除する。
 *
 * 引き数
 *             int            cx_num     コンテクストナンバー
 *             unsigned char  *dicname   辞書名
 *
 * リターン値
 *             成功した場合                             0
 *             辞書が存在しない場合                    -2   NOENT
 *             辞書がバイナリ辞書であった場合          -9    BADF
 *             RemoveDicの返り値が-1の場合            -13   ACCES
 *             マウントしていた場合                   -26  TXTBSY
 *             コンテクスト構造体が存在しない場合    -100 BADCONT
 */
int
RkwRemoveDic(cx_num, dicname, mode)
int cx_num;
unsigned char *dicname;
int mode;
{
  struct RkContext	*cx = RkGetContext(cx_num);
/*  struct RkParam	*sx = RkGetSystem();	*/
  struct DD		*userDDP[2];
  struct DM             *dm;
  char			*path;
  int res;

  if(!dicname)
    return NOENT;
  if ( !cx || !cx->ddpath || !cx->ddpath[0] )
      return BADCONT;

  if (locatepath(userDDP, cx->ddpath, mode) < 0) {
    return ACCES;
  }

  /* find dictionary in current mount list */
  dm = _RkSearchDDP(userDDP, (char *)dicname);
  if (!dm || ((mode & PL_DIC) && dm->dm_file->df_type != DF_FREQDIC)) {
    return NOENT;
  }
  if ( dm->dm_rcount > 0 ) 
	return TXTBSY;
  if ( !dm->dm_file ) /* ? */
	return BADCONT;
  if (!(dm->dm_file->df_direct->dd_flags & DD_WRITEOK) ||
      (!(dm->dm_flags & DM_WRITEOK) && !(mode & KYOUSEI))) {
    return ACCES;
  }
  if (!(path = _RkMakePath(dm->dm_file)))
	return NOTALC;
  res = unlink(path);
  (void)free(path);  
  if(res)
    return ACCES;
  (void)_RkRealizeDD(dm->dm_file->df_direct);
  DMremove(dm);
  return 0;
}

/* int
 * RkwRenameDic(cx_num, oldnick, newnick, mode)
 *
 * 指定されたコンテクストに指定された辞書が存在すれば
 * その辞書の名前を変更する。
 *
 * 引き数
 *          int            cx_num           コンテクストナンバー
 *          unsigned char  *oldnick        変更元辞書名
 *          unsigned char  *newnick        変更先辞書名
 *          int            mode             強制モード
 *
 * 返り値 (RKdic.h参照)
 *          成功した場合                             0
 *          oldnickが存在しない場合                -2     NOENT
 *          RemoveDicの返り値が-1の場合             -2     NOENT
 *          辞書がバイナリ辞書であった場合          -9      BADF
 *          RenameDicFileの返り値が-1の場合        -13     ACCES
 *          newnickが存在する場合                 -17     EXIST
 *          oldnickをマウントしていた場合         -26    TXTBSY
 *          newnickをマウントしていた場合         -26    TXTBSY
 *          コンテクスト構造体が存在しない場合    -100   BADCONT
 */
int
RkwRenameDic(cx_num, old, new, mode)
  int cx_num;
  unsigned char *old;
  unsigned char *new;
  int mode;
{
  struct RkContext	*cx = RkGetContext(cx_num);
  struct DD		*userDDP[2], *dd;
  struct DM		*dm1, *dm2;
  char            	spec[RK_LINE_BMAX];
  char			*path;
  
  if(!old || !*old)
    return NOENT;
  if(!new || !*new)
    return ACCES;
  if (!cx || !cx->ddpath || !cx->ddpath[0])
    return BADCONT;
  if (strlen((char *)new) >= (unsigned)RK_NICK_BMAX) {
    return INVAL;
  }

  if (locatepath(userDDP, cx->ddpath, mode) < 0) {
    return ACCES;
  }

  dm1 = _RkSearchDDP(userDDP, (char *)old);
  if (!dm1) {
    return NOENT;
  }

  dd = dm1->dm_file->df_direct;
  if (!(dd->dd_flags & DD_WRITEOK)) {
    return ACCES;
  }

  dm2 = _RkSearchDDP(userDDP, (char *)new);

  if (dm1->dm_rcount > 0) 
    return TXTBSY;
  if (dm2) { /* 新しい名前が、既に辞書として存在すれば */
    if (dm2->dm_rcount > 0) 
      return TXTBSY;
    if (!(mode & KYOUSEI))
      return EXIST;
    if (!(path = _RkMakePath(dm2->dm_file)))
      return NOTALC;
    (void)unlink(path);
    (void)free(path);
    DMremove(dm2);
    DMrename(dm1, new);
    (void)_RkRealizeDD(dd);
    return 1;
  } else {
    (void)sprintf(spec, "%s(.%s) -%s--%s%s-\n", "tmp.t", "mwd", new,
		  (dm1->dm_flags & DM_READOK) ? "r" : "",
		  (dm1->dm_flags & DM_WRITEOK) ? "w" : "");
    if (!DMcheck(spec, new))
      return NOENT; /* なんなんだか良く分からない (1993.11 今) */
    /* ためしにやってみているのかな？ (1993.11 今) */
    DMrename(dm1, new);
    (void)_RkRealizeDD(dd);
    return 0;
  }
}

/* int
 * RkwCopyDic(cx, dir, from, to, mode)
 *
 * 辞書をコピーする。
 *
 * 引き数
 *          int            cx              コンテクストナンバー
 *          char           *dir	           ディレクトリ名
 *          char           *from           コピー元辞書名
 *          char           *to             コピー先辞書名
 *          int            mode            モード
 *
 * 返り値 (RKdic.h参照)
 *          成功した場合                           0
 *          oldnickが存在しない場合               -2     NOENT
 *          RemoveDicの返り値が-1の場合           -2     NOENT
 *          ディレクトリの指定がおかしい場合      -9     BADF
 *          RenameDicFileの返り値が-1の場合       -13    ACCES
 *          メモリが足りなかった場合                     NOTALC
 *          辞書名が長すぎる場合                         INVAL
 *          newnickが存在する場合                 -17    EXIST
 *          oldnickをマウントしていた場合         -26    TXTBSY
 *          newnickをマウントしていた場合         -26    TXTBSY
 *          コンテクスト構造体が存在しない場合    -100   BADCONT
 */

int
RkwCopyDic(co, dir, from, to, mode)
int co;
char *dir, *from, *to;
int mode;
{
  struct RkContext	*cx = RkGetContext(co);
  struct DD		*userDDP[2];
  struct DM		*dm1, *dm2;
  char			*path, *perm = DEFAULT_PERMISSION;
  char *myddname;
  int res, v, con;
  
  if (!dir || !*dir) {
    return BADF;
  }
  if (!from || !*from)
    return NOENT;
  if (!to || !*to)
    return ACCES;
  if (strlen((char *)to) >= (unsigned)RK_NICK_BMAX) {
    return INVAL;
  }
  if (!cx || !cx->ddpath || !cx->ddpath[0])
    return BADCONT;

  res = BADCONT;
  con = RkwCreateContext();
  if (con >= 0) {
    int n = 2; /* for system dic */
    switch (mode & (RK_GRP_DIC | RK_SYS_DIC)) {
    case RK_GRP_DIC:
      n = 1; /* for group dic */
    case RK_SYS_DIC:
      if (!cx->ddpath[2]) {
	return BADCONT;
      }
      myddname = cx->ddpath[n]->dd_name;
      break;
    default:
      myddname = cx->ddpath[0]->dd_name;
      break;
    }

    res = NOTALC;
    path = malloc(strlen(dir) + 1 + strlen(myddname) + 1);
    if (path) {
      strcpy(path, dir);
      strcat(path, ":");
      strcat(path, myddname);

      res = NOTALC;
      v = RkwSetDicPath(con, path);
      (void)free(path);
      if (v >= 0) {
	struct RkContext *cy = RkGetContext(con);

	res = ACCES;
	if (cy->ddpath[1]->dd_flags & DD_WRITEOK) {
	  userDDP[0] = cy->ddpath[0];
	  userDDP[1] = (struct DD *)0;

	  res = NOENT;
	  dm1 = _RkSearchDDP(userDDP, from);
	  if (dm1) {
	    int type = dm1->dm_file->df_type;

	    res = BADF;
	    if (type != DF_RUCDIC) {
	      userDDP[0] = cy->ddpath[1];
	      userDDP[1] = (struct DD *)0;

	      dm2 = _RkSearchDDP(userDDP, to);
	      if (dm2) { /* to があって、強制モードなら消す */
		if (dm2->dm_rcount > 0) {
		  res = TXTBSY;
		  goto newdicUsed;
		}
		if (!(mode & KYOUSEI)) {
		  res = EXIST;
		  goto newdicUsed;
		}
		if (!(path = _RkMakePath(dm2->dm_file))) {
		  res = NOTALC;
		  goto newdicUsed;
		}
		(void)unlink(path);
		(void)free(path);
		switch (dm2->dm_flags & (DM_READOK | DM_WRITEOK)) {
		case (DM_READOK | DM_WRITEOK):
		  perm = "rw";
		  break;
		case DM_READOK:
		  perm = "r";
		  break;
		case DM_WRITEOK:
		  perm = "w";
		  break;
		default:
		  perm = "";
		  break;
		}
		DMremove(dm2);
	      }

	      { /* いよいよ辞書を作る */
		char *template, *filename;

		RkwSync(co, from); /* sometimes, this failes to an error */
		template =
		  (type == DF_FREQDIC) ? FREQ_TEMPLATE :
		    (type == DF_TEMPDIC) ? USER_TEMPLATE :
		      PERM_TEMPLATE;

		res = ACCES;
		filename = _RkCreateUniquePath(userDDP[0], template);
		if (filename) {
		  char spec[RK_LINE_BMAX];

		  (void)sprintf(spec, "%s(%s) -%s--%s-\n",
				filename, dm1->dm_dicname, to, perm);
		  res = NOTALC;
		  dm2 = DMcreate(userDDP[0], spec);
		  if (dm2) {
		    res = ACCES;
		    if (copyFile(dm1, dm2) == 0) {
		      (void)_RkRealizeDD(userDDP[0]);
		      res = 0;
		    }
		    else {
		      DMremove(dm2);
		    }
		  }
		}
	      }
	    }
	  }
	newdicUsed:;
	}
      }
    }
    RkwCloseContext(con);
  }
  return res;
}

/* int
 * RkwChmodDic(cx_num, dicname, mode)
 *
 * 辞書のモードを変更する。
 *
 * 引き数
 *          int   cx_num           コンテクスト
 *          char  dicname          辞書名
 *          int   mode             モード
 *
 * 返り値 (RKdic.h参照)
 *          成功した場合                             0
 *          dicnameが存在しない場合                 -2     NOENT
 *          DMchmod の返り値が-1の場合             -13     ACCES
 *          コンテクスト構造体が存在しない場合    -100   BADCONT
 */
int
RkwChmodDic(cx_num, dicname, mode)
int cx_num;
char *dicname;
int mode;
{
  struct RkContext	*cx = RkGetContext(cx_num);
  struct DD		*dd, *userDDP[2];
  struct DM		*dm;
  int res;
  unsigned dirmode;

  res = BADCONT;
  if (cx && cx->ddpath && cx->ddpath[0]) {
    dirmode = mode & RK_DIRECTORY;
    if (dirmode != 0) { /* ディレクトリ */
      switch (dirmode) {
      case RK_SYS_DIR:
	dd = (struct DD *)0; /* or SX.ddpath[0] */
	break;
      case RK_GRP_DIR:
	if (cx->ddpath[1] && cx->ddpath[2]) {
	  dd = cx->ddpath[1];
	}
	break;
      default: /* RK_USR_DIR */
	dd = cx->ddpath[0];
	break;
      }
      res = dd ? DDchmod(dd, mode) : ACCES;
    }
    else { /* ファイル */
      res = ACCES;
      if (locatepath(userDDP, cx->ddpath, mode) == 0) {
	res = NOENT;
	if(dicname && *dicname) {
	  dm = _RkSearchDDP(userDDP, dicname);
	  res = NOENT;
	  if (dm) {
	    struct DD *dd = dm->dm_file->df_direct;

	    res = DMchmod(dm, mode);
	    if (res >= 0) {
	      (void)_RkRealizeDD(dd);
	    }
	    else {
	      res = ACCES;
	    }
	  }
	}
      }
    }
  }
  return res;
}

/*
 * GetLine(cx, gram, tdp, line)
 * struct RkContext            *cx
 * struct RkKxGram	*gram
 * struct TD            *tdp
 * Wchar	*line
 *
 * 返り値 成功  0
 *        失敗 -1
 */
static struct td_n_tupple *
pushTdn(cx, tdp)
struct RkContext *cx;
struct TD *tdp;
{
  struct td_n_tupple	*new;
  struct _rec		*gwt;
  if (!cx || !(gwt = (struct _rec *)cx->cx_gwt) ||
      !(new = (struct td_n_tupple *)malloc(sizeof(struct td_n_tupple)))) {
    return (struct td_n_tupple *)0;
  }
  new->td = (char *)tdp;
  new->n = 0;
  new->next = (struct td_n_tupple *)gwt->tdn;
  gwt->tdn = (struct td_n_tupple *)new;
  return new;
}

void
freeTdn(cx)  /* tdn を next をたどりながら free する */
struct RkContext *cx;
{
  struct td_n_tupple *work;
  struct _rec	*gwt = (struct _rec *)cx->cx_gwt;
  if (gwt) {
    while((work = gwt->tdn) != (struct td_n_tupple *)0) {
      gwt->tdn = work->next;
      (void)free((char *)work);
    };
  };
}

static void
popTdn(cx)
struct RkContext *cx;
{
  struct td_n_tupple *work;
  struct _rec	*gwt = (struct _rec *)cx->cx_gwt;
  work = gwt->tdn;
  if (work) {
    gwt->tdn = work->next;
    (void)free(work);
  }
}

static
GetLine(cx, gram, tdp, line, size)
     struct RkContext	*cx;
     struct RkKxGram	*gram;
     struct TD		*tdp;
     Wchar		*line;
     int		size;
{
  struct TD	*vtd;
  struct TN	*vtn;
  struct _rec	*gwt = (struct _rec *)cx->cx_gwt;
  
  if (tdp) {
    if (gwt->tdn)
      freeTdn(cx);
    if(!pushTdn(cx, tdp))
      return NOTALC;
  }
  while (gwt->tdn && gwt->tdn->n >= ((struct TD *)gwt->tdn->td)->td_n)
    popTdn(cx);
  if (gwt->tdn == (struct td_n_tupple *)0)
    return -1;
  vtd = (struct TD *)gwt->tdn->td;
  vtn = vtd->td_node + gwt->tdn->n;
  while ( !IsWordNode(vtn) ) {
    gwt->tdn->n++;
    if(!pushTdn(cx, vtn->tn_tree))
      return NOTALC;
    vtd = (struct TD *)gwt->tdn->td;
    vtn = vtd->td_node;
  }
  if (RkUparseWrec(gram, vtn->tn_word->word, line, size, vtn->tn_word->lucks)) {
    gwt->tdn->n++;
    return 0;
  } else
    return -1;
}

/*
 * RkwGetWordTextDic(cx_num, dirname, dicname, info, infolen)
 *
 * int            cx_num      コンテクストNO
 * unsigned char  *dirname    ディレクトリ名
 * unsigned char  *dicname    辞書名
 * unsigned char  *info       バッファ
 * int            infolen     バッファの長さ
 *
 * 返り値 : 実際にinfoに入った長さ
 *          最後まで読んでいたら          ０を返す
 *          RkwCreateContextに失敗した     BADCONT
 *          RkwDuplicateContextに失敗した  BADCONT
 *          RkGetContextに失敗した        BADCONT
 *          RkwSetDicPathに失敗した        NOTALC
 *          RkwMountDicに失敗した          NOENT
 *          SearchUDDPに失敗した          NOENT
 *          バイナリ辞書だった                          -9   BADF
 *          dics.dirに異常があった場合                 -10   BADDR
 */
int
RkwGetWordTextDic(cx_num, dirname, dicname, info, infolen)
     int		cx_num;
     unsigned char	*dirname;
     unsigned char	*dicname;
     Wchar		*info ;
     int		infolen ;
{
  struct RkContext *new_cx, *cx;
  struct DM *dm;
  int new_cx_num;
  struct TD *initial_td;
  unsigned size;
  unsigned char *buff = 0;
  struct _rec	*gwt;

  if (!dicname || !dirname || !info || !(cx = RkGetContext(cx_num)) ||
      !(gwt = (struct _rec *)cx->cx_gwt))
    return BADCONT;
  if(dicname[0] != '\0') {
    size = strlen((char *)dicname) + 1;
    if (!(buff = (unsigned char *)malloc(size)))
      return (NOTALC);
    (void)strcpy((char *)buff, (char *)dicname);
    if(dirname[0] != '\0') {
      if((new_cx_num = RkwCreateContext()) < 0) {
	(void)free((char *)buff);
	return BADCONT;
      }
      if(RkwSetDicPath(new_cx_num, (char *)dirname) < 0) {
	RkwCloseContext(new_cx_num);
	(void)free((char *)buff);
	return NOTALC;
      }
    } else {
      if ((new_cx_num = RkwDuplicateContext(cx_num)) < 0) {
	(void)free((char *)buff);
	return BADCONT;
      }
    }
    if (!(new_cx = RkGetContext(new_cx_num))) {
      if(dirname[0] != '\0') {
	RkwCloseContext(new_cx_num);
	(void)free((char *)buff);
	return BADCONT;
      }
    }
    if (gwt->gwt_cx >= 0) {
      RkwCloseContext(gwt->gwt_cx);
      gwt->gwt_cx = -1;
    }
    
    if(!STRCMP(dirname, SYSTEM_DDHOME_NAME)) {
      if (!(dm = _RkSearchDDP(new_cx->ddpath, (char *)dicname))) {
	if (dirname[0] != '\0') {
	  RkwCloseContext(new_cx_num);
	}
	(void)free((char *)buff);
	return NOENT;
      }
    } else {
      if (!(dm = _RkSearchUDDP(new_cx->ddpath, dicname))) {
	if(dirname[0] != '\0') {
	  RkwCloseContext(new_cx_num);
	}
	(void)free((char *)buff);
	return NOENT;
      }
    }
    if (DM2TYPE(dm) != DF_TEMPDIC ) {
      if(dirname[0] != '\0') {
	RkwCloseContext(new_cx_num);
      }
      (void)free((char *)buff);
      return BADF;
    }
    if(RkwMountDic(new_cx_num, (char *)dicname,0) == -1) {
      RkwCloseContext(new_cx_num);
      (void)free((char *)buff);
      return NOMOUNT;
    }

    if (!_RkSearchDDP(new_cx->ddpath, (char *)dicname)) {
      RkwCloseContext(new_cx_num);
      (void)free((char *)buff);
      return BADDR;
    }
    gwt->gwt_cx = new_cx_num;
    if (gwt->gwt_dicname)
      (void)free((char *)gwt->gwt_dicname);
    gwt->gwt_dicname = buff;
    initial_td = (struct TD *)dm->dm_td;
  }
  else {
    if ((new_cx_num = gwt->gwt_cx) < 0
	|| !(new_cx = RkGetContext(new_cx_num))) {
      if (gwt->gwt_dicname)
	(void)free((char *)gwt->gwt_dicname);
      gwt->gwt_dicname = (unsigned char *)0;
      return BADCONT;
    }
    initial_td = (struct TD *)0;
  }
  if (GetLine(new_cx, cx->gram->gramdic, (struct TD *)initial_td,
	      info, infolen) < 0) {
    RkwUnmountDic(new_cx_num, (char *)gwt->gwt_dicname);
    RkwCloseContext(new_cx_num);
    gwt->gwt_cx = -1;
    return 0;
  }
  infolen = uslen((Wchar *)info);
  return infolen;
}
