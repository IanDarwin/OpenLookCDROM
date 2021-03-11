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

#if !defined(lint) && !defined(__CODECENTER__)
static char rcs_id[] = "@(#) 102.1 $Id: ulmount.c,v 5.15 1994/04/18 12:07:47 kon Exp $";
#endif

#include	<errno.h>
#include 	"canna.h"

extern int  errno;

extern int uiUtilIchiranTooSmall();
extern int kojin;

/* cfunc mountContext
 *
 * mountContext
 *
 */
static mountContext
newMountContext()
{
  mountContext mcxt;

  if((mcxt = (mountContext)calloc(1, sizeof(mountContextRec))) == NULL) {
    jrKanjiError = "malloc (newMountContext) できませんでした";
    return(0);
  }
  mcxt->id = MOUNT_CONTEXT;

  return mcxt;
}

static void
freeMountContext(mc)
mountContext mc;
{
  if (mc) {
    if (mc->mountList) {
      if (*(mc->mountList)) {
	free(*(mc->mountList));
      }
      free(mc->mountList);
    }
    if (mc->mountOldStatus) {
      free(mc->mountOldStatus);
    }
    if (mc->mountNewStatus) {
      free(mc->mountNewStatus);
    }
    free(mc);
  }
}

/*
 * 候補一覧行を作る
 */
static
getMountContext(d)
uiContext d;
{
  mountContext mc;
  int retval = 0;

  if(pushCallback(d, d->modec, 0, 0, 0, 0) == 0) {
    jrKanjiError = "malloc (pushCallback) できませんでした";
    return(NG);
  }
  
  if((mc = newMountContext()) == NULL) {
    popCallback(d);
    return(NG);
  }
  mc->majorMode = d->majorMode;
  mc->next = d->modec;
  d->modec = (mode_context)mc;

  mc->prevMode = d->current_mode;

  return(retval);
}

static void
popMountMode(d)
uiContext d;
{
  mountContext mc = (mountContext)d->modec;

  d->modec = mc->next;
  d->current_mode = mc->prevMode;
  freeMountContext(mc);
}

static struct dicname *
findDic(s)
char *s;
{
  extern struct dicname *kanjidicnames;
  struct dicname *dp;

  for (dp = kanjidicnames ; dp ; dp = dp->next) {
    if (!strcmp(s, dp->name)) {
      return dp;
    }
  }
  return (struct dicname *)0;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * 辞書のマウント／アンマウント                                              *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
uuMountExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  mountContext mc;
  int i, nmount = 0;
  extern defaultContext;
  struct dicname *dp;

  killmenu(d);
  popCallback(d); /* OnOff をポップ */

  if(defaultContext == -1) {
    if((KanjiInit() != 0) || (defaultContext == -1)) {
      jrKanjiError = "かな漢字変換サーバと通信できません";
      popMountMode(d);
      popCallback(d);
      return(GLineNGReturn(d));
    }
  }

  mc = (mountContext)d->modec;
  for(i=0; mc->mountList[i]; i++) {
    if(mc->mountOldStatus[i] != mc->mountNewStatus[i]) {
      if(mc->mountNewStatus[i]) {
	/* マウントする */
	nmount++;
	if((retval = RkwMountDic(defaultContext, (char *)mc->mountList[i],
			    kojin ? PL_ALLOW : PL_INHIBIT)) == NG) {
	  if (errno == EPIPE) {
	    jrKanjiPipeError();
	  }
	  MBstowcs(d->genbuf, "辞書のマウントに失敗しました", 512);
	}
	else if(d->contextCache != -1 &&
	  (retval = RkwMountDic(d->contextCache, (char *)mc->mountList[i],
			    kojin ? PL_ALLOW : PL_INHIBIT)) == NG) {
	  if (errno == EPIPE) {
	    jrKanjiPipeError();
	  }
	  MBstowcs(d->genbuf, "辞書のマウントに失敗しました", 512);
	}
	else { /* 成功 */
	  dp = findDic((char *)mc->mountList[i]);
	  if (!dp) {
	    dp = (struct dicname *)malloc(sizeof(struct dicname));
	    if (dp) {
	      dp->name = malloc(strlen((char *)mc->mountList[i]) + 1);
	      if (dp->name) {
		/* マウントしたやつはリストにつなぐ */
		strcpy(dp->name, (char *)mc->mountList[i]);
		dp->dictype = DIC_PLAIN;
		/* dp->dicflag = DIC_NOT_MOUNTED; will be rewritten below */
		dp->next = kanjidicnames;
		kanjidicnames = dp;
	      }
	      else { /* malloc failed */
		free((char *)dp);
		dp = (struct dicname *)0;
	      }
	    }
	  }
	  if (dp) {
	    dp->dicflag = DIC_MOUNTED;
	  }
	}
      } else {
	/* アンマウントする */
	nmount++;
	if((retval = RkwUnmountDic(defaultContext, (char *)mc->mountList[i]))
	   == NG) {
	  if (errno == EPIPE) {
	    jrKanjiPipeError();
	  }
	  MBstowcs(d->genbuf, "辞書のアンマウントに失敗しました", 512);
	}
	else if(d->contextCache != -1 &&
	  (retval = RkwUnmountDic(d->contextCache, (char *)mc->mountList[i]))
		== NG) {
	  if (errno == EPIPE) {
	    jrKanjiPipeError();
	  }
	  MBstowcs(d->genbuf, "辞書のアンマウントに失敗しました", 512);
	}
	else {
	  dp = findDic((char *)mc->mountList[i]);
	  if (dp) { /* かならず以下を通るはず */
	    dp->dicflag = DIC_NOT_MOUNTED;
	  }
	}
      }
    }
  }

  if(nmount)
    makeAllContextToBeClosed(1);

  if(retval != NG)
    MBstowcs(d->genbuf, "辞書のマウント／アンマウントを行いました", 512);
  else
    MBstowcs(d->genbuf, "辞書のマウント／アンマウントに失敗しました", 512);
  makeGLineMessage(d, d->genbuf, WStrlen(d->genbuf));

  popMountMode(d);
  popCallback(d);
  currentModeInfo(d);

  return(0);
}

static
uuMountQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* OnOff をポップ */

  popMountMode(d);
  popCallback(d);
  currentModeInfo(d);

  return prevMenuIfExist(d);
}

/*
 * dicLbuf                dicLp       soldp   snewp
 * ┌─────────┐ ┌───┐  ┌─┐  ┌─┐
 * │iroha\@fuzokugo\@k│ │*iroha│  │1 │  │1 │
 * │atakana\@satoko\@s│ │*fuzo │  │1 │  │1 │
 * │oft\@\@...        │ │*kata │  │0 │  │0 │
 * │                  │ │  :   │  │: │  │: │
 * └─────────┘ └───┘  └─┘  └─┘
 * dicMbuf                dicMp
 * ┌─────────┐ ┌───┐
 * │iroha\@fuzokugo\@s│ │*iroha│
 * │atoko\@\@...      │ │*fuzo │
 * │                  │ │*sato │
 * │                  │ │  :   │
 * └─────────┘ └───┘
 */
static
getDicList(d)
uiContext d;
{
  mountContext mc = (mountContext)d->modec;
  char *dicLbuf, dicMbuf[ROMEBUFSIZE];
  char **dicLp, *dicMp[ROMEBUFSIZE/2];
  char *wptr, **Lp, **Mp;
  BYTE *sop, *snp, *soldp, *snewp;
  int dicLc, dicMc, i;
  extern defaultContext;

  if((dicLbuf = malloc(ROMEBUFSIZE)) == NULL) {
    jrKanjiError = "malloc (getDicList) できませんでした";
    return(NG);
  }
  if(defaultContext == -1) {
    if((KanjiInit() != 0) || (defaultContext == -1)) {
      jrKanjiError = "かな漢字変換サーバと通信できません";
      free(dicLbuf);
      return(NG);
    }
  }
  if((dicLc = RkwGetDicList(defaultContext, (char *)dicLbuf, ROMEBUFSIZE))
     < 0) {
    if(errno == EPIPE)
      jrKanjiPipeError();
    jrKanjiError = "マウント可能な辞書の取り出しに失敗しました";
    free(dicLbuf);
    return(NG);
  }
  if (dicLc == 0) {
    jrKanjiError = "マウント可能な辞書が存在しません";
    free(dicLbuf);
    return NG;
  }
  if((dicLp = (char **)calloc(dicLc + 1, sizeof(char *))) == NULL) {
    jrKanjiError = "malloc (getDicList) できませんでした";
    free(dicLbuf);
    return(NG);
  }
  if((soldp = (BYTE *)malloc(dicLc + 1)) == NULL) {
    jrKanjiError = "malloc (getDicList) できませんでした";
    free(dicLbuf);
    free(dicLp);
    return(NG);
  }
  if((snewp = (BYTE *)malloc(dicLc + 1)) == NULL) {
    jrKanjiError = "malloc (getDicList) できませんでした";
    free(dicLbuf);
    free(dicLp);
    free((char *)soldp);
    return(NG);
  }
  for(i = 0, wptr = dicLbuf; i < dicLc; i++) { /* buf を作る */
    dicLp[i] = wptr;
    while(*wptr++)
      /* EMPTY */
      ; /* NULL までスキップし、NULL の次までポインタを進める */
  }
  dicLp[i] = NULL;

  if(defaultContext == -1) {
    if((KanjiInit() != 0) || (defaultContext == -1)) {
      jrKanjiError = "かな漢字変換サーバと通信できません";
      free(dicLbuf);
      free(dicLp);
      free((char *)soldp);
      return(NG);
    }
  }
  if((dicMc = RkwGetMountList(defaultContext, (char *)dicMbuf, ROMEBUFSIZE)) <
     0) {
    if(errno == EPIPE)
      jrKanjiPipeError();
    jrKanjiError = "マウントしている辞書の取り出しに失敗しました";
    free(dicLbuf);
    free(dicLp);
    free((char *)soldp);
    free((char *)snewp);
    return(NG);
  }

  for(i = 0, wptr = dicMbuf ; i < dicMc ; i++) { /* buf を作る */
    dicMp[i] = wptr;
    while (*wptr++)
      /* EMPTY */
      ; /* NULL までスキップし、NULL の次までポインタを進める */
  }
  dicMp[i] = NULL;

  for(i=0, sop=soldp, snp=snewp; i<dicLc; i++, sop++, snp++) {
    *sop = 0;
    *snp = 0;
  }
  for(Lp=dicLp, sop=soldp, snp=snewp; *Lp; Lp++, sop++, snp++) {
    for(Mp=dicMp; *Mp; Mp++) {
      if(!strcmp((char *)*Lp, (char *)*Mp)) {
	*sop = *snp = 1;
	break;
      }
    }
  }
  mc->mountList = dicLp;
  mc->mountOldStatus = soldp;
  mc->mountNewStatus = snewp;

  return(dicLc);
}

dicMount(d)
uiContext d;
{
  ichiranContext oc;
  mountContext mc;
  int retval = 0, currentkouho = 0, nelem;
  wchar_t *xxxx[100];

  d->status = 0;

  if((retval = getMountContext(d)) == NG) {
    killmenu(d);
    return(GLineNGReturn(d));
  }

  /* 辞書リストとマウント／アンマウントの状態を montContext にとってくる */
  if((nelem = getDicList(d)) == NG) {
    popMountMode(d);
    popCallback(d);
    killmenu(d);
    return(GLineNGReturn(d));
  }

  mc = (mountContext)d->modec;
#ifdef DEBUG
  if(iroha_debug) {
    int i;

    printf("<★mount>\n");
    for(i= 0; mc->mountList[i]; i++)
      printf("[%s][%x][%x]\n", mc->mountList[i],
	     mc->mountOldStatus[i], mc->mountNewStatus[i]);
    printf("\n");
  }
#endif

  /* selectOnOff を呼ぶための準備 */
  mc->curIkouho = currentkouho = 0;

  setWStrings(xxxx, mc->mountList, 0);
  if((retval = selectOnOff(d, xxxx, &mc->curIkouho, nelem,
		 BANGOMAX, currentkouho, mc->mountOldStatus,
		 0, uuMountExitCatch,
		 uuMountQuitCatch, uiUtilIchiranTooSmall)) == NG) {
    popMountMode(d);
    popCallback(d);
    killmenu(d);
    return(GLineNGReturn(d));
  }

  oc = (ichiranContext)d->modec;
  oc->majorMode = CANNA_MODE_ExtendMode;
  oc->minorMode = CANNA_MODE_MountDicMode;
  currentModeInfo(d);

  /* 候補一覧行が狭くて候補一覧が出せない */
  if(oc->tooSmall) {
    wchar_t p[512];


    ichiranFin(d);
    popCallback(d); /* OnOff をポップ */
    popMountMode(d);
    popCallback(d);
    currentModeInfo(d);
    MBstowcs(p ,"辞書一覧用の幅が狭いので辞書マウント／アンマウントできません",512);
    makeGLineMessage(d, p, WStrlen(p));
    killmenu(d);
    return(0);
  }

  makeGlineStatus(d);
  /* d->status = ICHIRAN_EVERYTIME; */

  return(retval);
}
