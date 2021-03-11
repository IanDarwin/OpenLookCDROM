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
static char rcs_id[] = "@(#) 102.1 $Id: ulkigo.c,v 5.18 1994/04/18 12:10:22 kon Exp $";
#endif

#include	<errno.h>
#include "canna.h"

extern int makeGlineStatus();
extern int uiUtilIchiranTooSmall();

static
char *srussia_data[] = {"§¡", "§¢", "§£", "§¤", "§¥", "§¦", "§§", "§¨",
			"§©", "§ª", "§«", "§¬", "§­", "§®", "§¯", "§°", 
			"§±", "§²", "§³", "§´", "§µ", "§¶", "§·", "§¸",
			"§¹", "§º", "§»", "§¼", "§½", "§¾", "§¿", "§À", 
			"§Á", "§Ñ", "§Ò", "§Ó", "§Ô", "§Õ", "§Ö", "§×",
			"§Ø", "§Ù", "§Ú", "§Û", "§Ü", "§Ý", "§Þ", "§ß",
			"§à", "§á", "§â", "§ã", "§ä", "§å", "§æ", "§ç",
			"§è", "§é", "§ê", "§ë", "§ì", "§í", "§î", "§ï",
			"§ð", "§ñ", 
		      };

#define	UURD_SZ	(sizeof(srussia_data) / sizeof(char *))

static
char *sgreek_data[] =  { "¦¡", "¦¢", "¦£", "¦¤", "¦¥", "¦¦", "¦§", "¦¨",
                        "¦©", "¦ª", "¦«", "¦¬", "¦­", "¦®", "¦¯", "¦°",
			"¦±", "¦²", "¦³", "¦´", "¦µ", "¦¶", "¦·", "¦¸",
		        "¦Á", "¦Â", "¦Ã", "¦Ä", "¦Å", "¦Æ", "¦Ç", "¦È",
		        "¦É", "¦Ê", "¦Ë", "¦Ì", "¦Í", "¦Î", "¦Ï", "¦Ð",
			"¦Ñ", "¦Ò", "¦Ó", "¦Ô", "¦Õ", "¦Ö", "¦×", "¦Ø",
		      };

#define	UUGD_SZ	(sizeof(sgreek_data) / sizeof(char *))

static wchar_t *russia_data[UURD_SZ];
static wchar_t *greek_data[UUGD_SZ];

int
initUlKigoTable()
{
  setWStrings(russia_data, srussia_data, UURD_SZ);
  setWStrings(greek_data, sgreek_data, UUGD_SZ);
}

#ifdef pcux_r32
static
char *skeisen_data[] =  { "¬¤", "¬¥", "¬¦", "¬§", "¬¨", "¬©", "¬ª", "¬«",
			 "¬¬", "¬­", "¬®", "¬¯", "¬°", "¬±", "¬²", "¬³",
			 "¬´", "¬µ", "¬¶", "¬·", "¬¸", "¬¹", "¬º", "¬»",
			 "¬¼", "¬½", "¬¾", "¬¿", "¬À", "¬Á", "¬Â", "¬Ã",
			 "¬Ä", "¬Å", "¬Æ", "¬Ç", "¬È", "¬É", "¬Ê", "¬Ë",
			 "¬Ì", "¬Í", "¬Î", "¬Ï", "¬Ð", "¬Ñ", "¬Ò", "¬Ó",
			 "¬Ô", "¬Õ", "¬Ö", "¬×", "¬Ø", "¬Ù", "¬Ú", "¬Û",
			 "¬Ü", "¬Ý", "¬Þ", "¬ß", "¬à", "¬á", "¬â", "¬ã",
			 "¬ä", "¬å", "¬æ", "¬ç", "¬è", "¬é", "¬ê", "¬ë",
			 "¬ì", "¬í", "¬î", "¬ï",
		       };
#else /* EWS-UX/V */
static
char *skeisen_data[] =  { "¨¡", "¨¢", "¨£", "¨¤", "¨¥", "¨¦", "¨§", "¨¨",
			 "¨©", "¨ª", "¨«", "¨¬", "¨­", "¨®", "¨¯", "¨°",
			 "¨±", "¨²", "¨³", "¨´", "¨µ", "¨¶", "¨·", "¨¸",
			 "¨¹", "¨º", "¨»", "¨¼", "¨½", "¨¾", "¨¿", "¨À",
 		       };
#endif

#define UUKD_SZ (sizeof(skeisen_data) / sizeof(char *))

static wchar_t *keisen_data[UUKD_SZ];

int
initUlKeisenTable()
{
  setWStrings(keisen_data, skeisen_data, UUKD_SZ);
}

static
uuKigoExitDo(d, retval)
uiContext d;
int retval;
{
  popForIchiranMode(d);
  popCallback(d);
  retval = YomiExit(d, retval);
  currentModeInfo(d);

  killmenu(d);

  return(retval);
}

static
uuKigoRExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  forichiranContext fc;

  popCallback(d); /* °ìÍ÷¤ò pop */

  fc = (forichiranContext)d->modec;
  d->currussia = fc->curIkouho;

  return(uuKigoExitDo(d, retval));
}

static
uuKigoGExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  forichiranContext fc;

  popCallback(d); /* °ìÍ÷¤ò pop */

  fc = (forichiranContext)d->modec;
  d->curgreek = fc->curIkouho;

  return(uuKigoExitDo(d, retval));
}

static
uuKigoKExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  forichiranContext fc;

  popCallback(d); /* °ìÍ÷¤ò pop */

  fc = (forichiranContext)d->modec;
  d->curkeisen = fc->curIkouho;

  return(uuKigoExitDo(d, retval));
}

uuKigoGeneralExitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  forichiranContext fc;

  popCallback(d); /* °ìÍ÷¤ò pop */

  fc = (forichiranContext)d->modec;
  if (fc->prevcurp) {
    *(fc->prevcurp) = fc->curIkouho;
  }

  return(uuKigoExitDo(d, retval));
}

static
uuKigoQuitCatch(d, retval, env)
uiContext d;
int retval;
mode_context env;
/* ARGSUSED */
{
  popCallback(d); /* °ìÍ÷¤ò pop */

  popForIchiranMode(d);
  popCallback(d);
  currentModeInfo(d);

  return prevMenuIfExist(d);
}

extern int quickly_escape;

uuKigoMake(d, allkouho, size, cur, mode, exitfunc, posp)
uiContext d;
wchar_t **allkouho;
int size, *posp;
char cur, mode;
int (*exitfunc)();
{
  forichiranContext fc;
  ichiranContext ic;
  unsigned char inhibit = 0;
  int retval = 0;

  d->status = 0;

  if((retval = getForIchiranContext(d)) == NG) {
    return(GLineNGReturn(d));
  }
  fc = (forichiranContext)d->modec;

  /* selectOne ¤ò¸Æ¤Ö¤¿¤á¤Î½àÈ÷ */
  fc->allkouho = allkouho;
  fc->curIkouho = 0;
  fc->prevcurp = posp;
  inhibit |= (unsigned char)NUMBERING;

  if((retval = selectOne(d, fc->allkouho, &fc->curIkouho, size,
		 KIGOBANGOMAX, inhibit, 0, WITH_LIST_CALLBACK,
		 0, exitfunc,
		 uuKigoQuitCatch, uiUtilIchiranTooSmall)) == NG) {
    return(GLineNGReturnFI(d));
  }

  ic = (ichiranContext)d->modec;
  ic->minorMode = mode;
  ic->flags |= quickly_escape ? 0 : ICHIRAN_STAY_LONG;
  currentModeInfo(d);

  *(ic->curIkouho) = (int)cur;

  /* ¸õÊä°ìÍ÷¹Ô¤¬¶¹¤¯¤Æ¸õÊä°ìÍ÷¤¬½Ð¤»¤Ê¤¤ */
  if(ic->tooSmall) {
    d->status = AUX_CALLBACK;
    return(retval);
  }

  if ( !(ic->flags & ICHIRAN_ALLOW_CALLBACK) ) {
    makeGlineStatus(d);
  }

  /* d->status = ICHIRAN_EVERYTIME; */

  return(retval);
}

#if 0
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * µ­¹æ°ìÍ÷                                                                  *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static
kigoZenpan(d)
uiContext	d;
{
  if(makeKigoIchiran(d, CANNA_MODE_ExtendMode) == NG) /* 0 ¤Ï³ÈÄ¥¤Îµ­¹æ°ìÍ÷ */
    return(GLineNGReturn(d));
  else
    return(0);
}
#endif

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * ¥í¥·¥¢Ê¸»ú¤ÎÆþÎÏ                                                          *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

kigoRussia(d)
uiContext d;
{
  return(uuKigoMake(d, (wchar_t **)russia_data, UURD_SZ,
           d->currussia, CANNA_MODE_RussianMode, uuKigoRExitCatch, (int *)0));
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * ¥®¥ê¥·¥ãÊ¸»ú¤ÎÆþÎÏ                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

kigoGreek(d)
uiContext d;
{
  return(uuKigoMake(d, (wchar_t **)greek_data, UUGD_SZ,
           d->curgreek, CANNA_MODE_GreekMode, uuKigoGExitCatch, (int *)0));
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * ·ÓÀþ¤ÎÆþÎÏ                                                                *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

kigoKeisen(d)
uiContext d;
{
  return(uuKigoMake(d, (wchar_t **)keisen_data, UUKD_SZ,
           d->curkeisen, CANNA_MODE_LineMode, uuKigoKExitCatch, (int *)0));
}
