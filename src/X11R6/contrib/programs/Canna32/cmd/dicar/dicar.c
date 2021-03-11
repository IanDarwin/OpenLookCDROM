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

#ifndef lint
static char rcsid[]="@(#) 102.1 $Id: dicar.c,v 1.29 1994/01/28 08:37:36 kon Exp $";
#endif

/*
 *	辞書アーカイバ:
 *
 *	dicar -t bindic			[原子辞書の表示]
 *	dicar -x bindic [atmdic ...]	[原子辞書のとりだし]
 *	dicar -r bindic1 bindic2 ...	[辞書のマージ、bindic2をbindic1に]
 *	dicar -d bindic atmdic ...	[原子辞書の削除]
 *
 */

#include <stdio.h>
#include <time.h>		/* 時間をとってくるため */
#include <fcntl.h>
#include <sys/types.h>
#include "RKintern.h"

static char *program;

/* 共通に利用する関数 */

static void
usage()
{
  (void)fprintf( stderr,"usage: %s -t filename\n\
       %s -x filename [dicname ...]\n\
       %s -r filename1 filename2 ...\n\
       %s -d filename dicname ...\n",
		program, program, program, program);
  exit(1);
}

/* 出力状況のためのメッセージ */
static void
actmessage(msg, name)
char *msg;
char *name;
{
  (void)printf("%s - %s\n", msg, name);
}

static char *
basename(name)
char *name;
{
  char *s = name + strlen(name);
  if (*--s == '/') *s = (char)0;
  while (s-- >= name) {
    if (*s == '/') return ++s;
  }
  return name;
}

/* cfuncdef

   getatomicname -- HD 構造体から辞書名を取り出して、name に書き込む。

 */

static void
getatomicname(hd, name)
struct HD *hd;
char *name;
{
  int len;

  len = strlen((char *)hd->data[HD_DMNM].ptr);
  (void)strcpy(name, (char *)hd->data[HD_DMNM].ptr);
  name[len++] = '.';
  name[len++] = 'd';
  name[len++] = '\0';
} 

/* ヘッダ情報を書き出す */
static void
PrintHeader(hd)
struct HD *hd;
{
  char		*date;
  time_t	tloc;

  if (hd->flag[HD_TIME] && hd->flag[HD_REC] && hd->flag[HD_CAN]) {
    tloc = hd->data[HD_TIME].uvar;
    date = ctime( &tloc );
    date[strlen(date)-1] = '\0';

    (void)printf("%s.d [%s] = %d + %d\n",
		 hd->data[HD_DMNM].ptr, date,
		 hd->data[HD_CAN].uvar, hd->data[HD_REC].uvar);
  }
}

static int
openForRead(name)
char *name;
{
  int newfd;

  if ((newfd = open(name, O_RDONLY)) < 0) {
    (void)fprintf(stderr, "%s: %s cannot read.\n", program, name);
  }
  return newfd;
}

static int
openForWrite(name)
char *name;
{
  int newfd;

  if ((newfd = open(name, O_WRONLY | O_CREAT | O_TRUNC, 0666)) < 0) {
    (void)fprintf(stderr, "%s: %s cannot create\n", program, name);
  }
  return newfd;
}

static void
closeForWrite(fd, name)
int fd;
char *name;
{
  if (close(fd) < 0) {
    (void)fprintf(stderr, "%s: write failed for %s\n", program, name);
  }
}

/* src の内容を dst に siz 分コピーする */
static void
copyfile(src, dst, siz)
int src, dst;
unsigned siz;
{
  char *buf;

  if (!(buf = (char *)malloc((unsigned)siz))) {
    (void)fprintf(stderr, "%s: cannot malloc %ld siz.\n", program, siz);
  }
  else {
    (void)read(src, buf, siz);
    if (write(dst, buf, siz) < 0) {
      (void)fprintf(stderr, "%s: write failed.\n", program);
    }
    free(buf);
  }
}

/* 以下は、個々の作業用の関数 */

/* -t: traceDic
 *      バイナリ辞書内の子辞書名とサイズを表示する
 */
static void
traceDic( fd )
int fd;
{
  struct HD hd;
  char key[ND_HDRSIZ];
  long offset = 0;

  while(_RkReadHeader(fd, &hd, offset) >= 0) {
    if (!hd.flag[HD_DMNM] /* 名前 */ || !hd.flag[HD_SIZ] /* サイズ */) {
      _RkClearHeader(&hd);
      continue;
    }

    getatomicname(&hd, key);
    offset += hd.data[HD_SIZ].uvar;
    PrintHeader(&hd);
    _RkClearHeader(&hd);
  }
  _RkClearHeader(&hd);
}


/* -x: xgetDic
 *
 *      bindic 内の原子辞書を取り出す。
 *
 *      xgetDic 原子辞書名を取り出す。名前が指定されればそれだけ取り出す。
 */


static void
xgetDic(fd, fname)
int fd;
char *fname; /* テキスト辞書名に .d がついたもの */
{
  struct HD hd;
  int dst;
  unsigned dmsize;
  char key[ND_HDRSIZ];
  long offset = 0;

  while (_RkReadHeader(fd, &hd, offset) >= 0) {
    if (!hd.flag[HD_DMNM] /* 名前 */ || !hd.flag[HD_SIZ] /* サイズ */) {
      _RkClearHeader(&hd);
      continue;
    }

    getatomicname(&hd, key);
    dmsize = hd.data[HD_SIZ].uvar;
    _RkClearHeader(&hd);

    if (!fname || !strcmp(fname, key)) { /* 原子辞書が同じ時 */
      if ((dst = openForWrite(key)) >= 0) {
	actmessage("x", key); /* 出力メッセージ */
	(void)lseek(fd, offset, 0);
	copyfile(fd, dst, dmsize);
	closeForWrite(dst, key);
      }
      if (fname) {
	return;
      }
    }
    offset += dmsize;
  }
  _RkClearHeader(&hd);
  if (fname) { /* 指定された辞書が見つからなかった */
    (void)fprintf(stderr, "%s: %s not found\n", program, fname);
  }
}

struct wordrec {
  char key[ND_HDRSIZ];
  long offset;
  unsigned size;
  int flag;
};

/* -r: remakedic
 *	bindic2中の子辞書をbindic1に追加する。
 *	もし、bindic1中に同じ子辞書がある場合は、上書きする。
 */
/* 子辞書の一覧を作成する */

static int
getchild(fd, childdic)
int fd;
struct wordrec	*childdic;
{
  struct HD hd;
  long offset = 0;
  int i, j, num = 0;

  while(_RkReadHeader(fd, &hd, offset) >= 0) {
    if (!hd.flag[HD_DMNM] /* 名前 */ || !hd.flag[HD_SIZ] /* サイズ */) {
      _RkClearHeader(&hd);
      continue;
    }

    (void)strcpy(childdic[num].key, (char *)hd.data[HD_DMNM].ptr);
    childdic[num].offset = offset;
    childdic[num].size = hd.data[HD_SIZ].uvar;
    childdic[num].flag = 1;
    num++;
    offset += hd.data[HD_SIZ].uvar;
    _RkClearHeader(&hd);
  }
  _RkClearHeader(&hd);

  for (i = 0 ; i < num ; i++) { /* 重複している辞書名には印を付ける */
    for (j = i + 1 ; j < num ; j++) {
      if (!strcmp(childdic[i].key, childdic[j].key)) {
	childdic[i].flag = 0;
	break;
      }
    }
  }
  return num;
}

/* cfuncdef

   remakeDic -- 辞書(src と atm)をマージして、新しく作る。

   src が -1 の時は atm リストを cat して新しく作る。

 */

static void
remakeDic(src, atm, dst)
int src, atm, dst;
{
  struct HD hd;
  struct wordrec childdic[256]; /* 子辞書の情報 */
  char key[ND_HDRSIZ];
  int num, i;
  long offset = 0;
  unsigned dmsize;
  
  num = getchild(atm, childdic); /* 子辞書情報の構造体の作成 */

  if (src >= 0) {
    while (_RkReadHeader(src, &hd, offset) >= 0) {
      if (!hd.flag[HD_DMNM] /* 名前 */ || !hd.flag[HD_SIZ] /* サイズ */) {
	_RkClearHeader(&hd);
	continue;
      }

      dmsize = hd.data[HD_SIZ].uvar;

      for (i = 0 ; i < num ; i++) {
	if (childdic[i].flag &&
	    !strcmp(childdic[i].key, (char *)hd.data[HD_DMNM].ptr)) {
	  (void)strcpy(key, childdic[i].key);
	  (void)strcat(key, ".d");
	  actmessage("r", key); /* 出力メッセージ */
	  (void)lseek(atm, childdic[i].offset, 0);
	  copyfile(atm, dst, childdic[i].size);
	  childdic[i].flag = 0;
	  break;
	}
      }
      _RkClearHeader(&hd);

      if (i == num) { /* リプレース指定されていなければ */
	(void)lseek(src, offset, 0);
	copyfile(src, dst, dmsize);
      }
      offset += dmsize;
    }
    _RkClearHeader(&hd);
  }

  for (i = 0 ; i < num ; i++) {
    if (childdic[i].flag) {
      (void)strcpy(key, childdic[i].key);
      (void)strcat(key, ".d");
      actmessage("a", key); /* 出力メッセージ */
      (void)lseek(atm, childdic[i].offset, 0);
      copyfile(atm, dst, childdic[i].size);
    }
  }
}


/* -d: deleteDic
 *	bindic1 中の子辞書 bindic2 を削除する。
 */

static void
deleteDic(src, dst, fname)
int src, dst;
char	*fname;
{
  struct HD hd;
  char key[ND_HDRSIZ];
  long offset = 0;
  unsigned dmsize;
  int deleted = 0;

  while (_RkReadHeader(src, &hd, offset) >= 0) {
    if (!hd.flag[HD_DMNM] /* 名前 */ || !hd.flag[HD_SIZ] /* サイズ */) {
      _RkClearHeader(&hd);
      continue;
    }

    getatomicname(&hd, key);
    dmsize = hd.data[HD_SIZ].uvar;
    _RkClearHeader(&hd);

    if (strcmp(fname, key)) { /* 指定原子辞書以外はコピー */
      (void)lseek(src, offset, 0);
      copyfile(src, dst, dmsize);
    }
    else {
      actmessage("d", fname);	/* 出力メッセージ */
      deleted = 1;
    }
    offset += dmsize;
  }
  _RkClearHeader(&hd);
  if (!deleted) {
    (void)fprintf(stderr, "%s: %s not found\n", program, fname);
  }
}

static void
doTrace(argn, args)
int argn;
char **args;
{
  int fd;

  if (argn < 3) usage();

  if ((fd = openForRead(args[2])) >= 0) {
    traceDic(fd);
    (void)close(fd);
  }
}

static void
doExtract(argn, args)
int argn;
char **args;
{
  int fd, i;

  if ((fd = openForRead(args[2])) >= 0) {
    if (argn == 3)
      xgetDic(fd, (char *)0);
    else {
      for (i = 3 ; i < argn ; i++) {
	xgetDic(fd, basename(args[i]));
      }
    }
    (void)close(fd);
  }
}

static void
doReplace(argn, args)
int argn;
char **args;
{
  int src, dst, atm, i;
  char fname[ND_HDRSIZ];

  for (i = 3 ; i < argn ; i++) {
    if ((src = openForRead(args[2])) >= 0) {
      if ((atm = openForRead(args[i])) >= 0) {
	(void)strcpy(fname, "#");
	(void)strcat(fname, basename(args[2]));
	if ((dst = openForWrite(fname)) >= 0) {
	  remakeDic(src, atm, dst);
	  closeForWrite(dst, fname);
	  (void)rename(fname, args[2]); /* バックファイルを戻す */
	}
	(void)close(atm);
      }
      (void)close(src);
    }
  }
}

static void
doCreate(argn, args)
int argn;
char **args;
{
  int dst, atm, i;
  char fname[ND_HDRSIZ];

  (void)strcpy(fname, "#");
  (void)strcat(fname, basename(args[2]));
  if ((dst = openForWrite(fname)) >= 0) {
    for (i = 3 ; i < argn ; i++) {
      if ((atm = openForRead(args[i])) >= 0) {
	remakeDic(-1, atm, dst);
	(void)close(atm);
      }
    }
    closeForWrite(dst, fname);
    (void)rename(fname, args[2]); /* バックファイルを戻す */
  }
}

doDelete(argn, args)
int argn;
char **args;
{
  int src, dst, i;
  char fname[ND_HDRSIZ];

  for (i = 3 ; i < argn ; i++) {
    if ((src = openForRead(args[2])) >= 0) {
      (void)strcpy(fname, "#");
      (void)strcat(fname, basename(args[2]));
      if ((dst = openForWrite(fname)) >= 0) {
	deleteDic(src, dst, args[i]);
	closeForWrite(dst, args[i]);
	(void)rename(fname, args[2]);
      }
      (void)close(src);
    }
  }
}

main(argn, args)
int	argn;
char	**args;
{
  char *opchar;

  program = basename(args[0]);

  if (argn < 3) usage();

  opchar = args[1] + (args[1][0] == '-');
  if (opchar[1]) usage();
  
  switch (opchar[0]) {
  case 'c': doCreate(argn, args); break;
  case 'd': doDelete(argn, args); break;
  case 'r': doReplace(argn, args); break;
  case 't': doTrace(argn, args); break;
  case 'x': doExtract(argn, args); break;
  default: usage();
  }
  exit(0);
}
