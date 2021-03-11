/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/
/* 
 *  [ ctwm ]
 *
 *  Copyright 1992 Claude Lecommandeur.
 *            
 * Permission to use, copy, modify  and distribute this software  [ctwm] and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above  copyright notice appear  in all copies and that both that
 * copyright notice and this permission notice appear in supporting documen-
 * tation, and that the name of  Claude Lecommandeur not be used in adverti-
 * sing or  publicity  pertaining to  distribution of  the software  without
 * specific, written prior permission. Claude Lecommandeur make no represen-
 * tations  about the suitability  of this software  for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * Claude Lecommandeur DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL  IMPLIED WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO
 * EVENT SHALL  Claude Lecommandeur  BE LIABLE FOR ANY SPECIAL,  INDIRECT OR
 * CONSEQUENTIAL  DAMAGES OR ANY  DAMAGES WHATSOEVER  RESULTING FROM LOSS OF
 * USE, DATA  OR PROFITS,  WHETHER IN AN ACTION  OF CONTRACT,  NEGLIGENCE OR
 * OTHER  TORTIOUS ACTION,  ARISING OUT OF OR IN  CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Claude Lecommandeur [ lecom@sic.epfl.ch ][ April 1992 ]
 */


/***********************************************************************
 *
 * $XConsortium: parse.c,v 1.52 91/07/12 09:59:37 dave Exp $
 *
 * parse the .twmrc file
 *
 * 17-Nov-87 Thomas E. LaStrange       File created
 * 10-Oct-90 David M. Sternlicht       Storing saved colors on root
 *
 * Do the necessary modification to be integrated in ctwm.
 * Can no longer be used for the standard twm.
 *
 * 22-April-92 Claude Lecommandeur.
 *
 ***********************************************************************/

#include <stdio.h>
#if defined(sony_news)
#  include <ctype.h>
#endif
#ifdef VMS
#include <ctype.h>
#include <decw$include/Xos.h>
#include <X11Xmu/CharSet.h>
#else
#include <X11/Xos.h>
#include <X11/Xmu/CharSet.h>
#endif
#include "twm.h"
#include "screen.h"
#include "menus.h"
#include "util.h"
#include "gram.h"
#include "parse.h"
#ifdef VMS
#include <decw$include/Xatom.h> 
#else
#include <X11/Xatom.h> 
#endif

/* For m4... */
#ifdef USEM4
#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#endif

#ifndef SYSTEM_INIT_FILE
#ifdef VMS
#define SYSTEM_INIT_FILE "DECW$SYSTEM_DEFAULTS:SYSTEM.CTWMRC"
#else
#define SYSTEM_INIT_FILE "/usr/lib/X11/twm/system.twmrc"
#endif
#endif
#define BUF_LEN 300

static int ParseJustification ();
static int ParseRandomPlacement ();

static FILE *twmrc;
static int ptr = 0;
static int len = 0;
static char buff[BUF_LEN+1];
static char overflowbuff[20];		/* really only need one */
static int overflowlen;
static char **stringListSource, *currentString;
static int ParseUsePPosition();
#ifdef USEM4
static FILE *start_m4();
static char *m4_defs();
#endif

extern int yylineno;
extern int mods;

int ConstrainedMoveTime = 400;		/* milliseconds, event times */

int RaiseDelay = 0;			/* msec, for AutoRaise */

static int twmFileInput(), twmStringListInput();
#ifdef USEM4
static int  m4twmFileInput ();
#endif
void twmUnput();
int (*twmInputFunc)();

extern char *defTwmrc[];		/* default bindings */

#ifdef SOUNDS
extern int set_sound_host();
#endif

/***********************************************************************
 *
 *  Procedure:
 *	ParseTwmrc - parse the .twmrc file
 *
 *  Inputs:
 *	filename  - the filename to parse.  A NULL indicates $HOME/.twmrc
 *
 ***********************************************************************
 */

static int doparse (ifunc, srctypename, srcname)
    int (*ifunc)();
    char *srctypename;
    char *srcname;
{
    mods = 0;
    ptr = 0;
    len = 0;
    yylineno = 1;
    ParseError = FALSE;
    twmInputFunc = ifunc;
    overflowlen = 0;

    yyparse();

    if (ParseError) {
	fprintf (stderr, "%s:  errors found in twm %s",
		 ProgramName, srctypename);
	if (srcname) fprintf (stderr, " \"%s\"", srcname);
	fprintf (stderr, "\n");
    }
    return (ParseError ? 0 : 1);
}


int ParseTwmrc (filename)
    char *filename;
{
    int i;
    char *home = NULL;
    int homelen = 0;
    char *cp = NULL;
    char tmpfilename[257];
#ifdef USEM4
    static FILE *raw;
    extern int GoThroughM4;
#endif

    /*
     * Check for the twmrc file in the following order:
     *       Unix                  |   VMS
     *   0.  -f filename.#         | -f filename_#
     *   1.  -f filename           | -f filename
     *   2.  .ctwmrc.#             | ctwm.rc_#
     *   3.  .ctwmrc               | ctwm.rc
     *   4.  .twmrc.#              | twm.rc_#
     *   5.  .twmrc                | twm.rc
     *   6.  system.ctwmrc         | system.ctwmrc
     */
    for (twmrc = NULL, i = 0; !twmrc && i < 7; i++) {
	switch (i) {
#ifdef VMS
	  case 0:
	    if (filename != NULL)  {
	       cp = tmpfilename;
	       (void) sprintf (tmpfilename, "%s_%d", filename, Scr->screen);
	    } else
	       cp = filename;
	    break;

	  case 1:
	    cp = filename;
	    break;

	  case 2:
	    if (!filename) {
		home = getenv ("DECW$USER_DEFAULTS");
		if (home) {
		    homelen = strlen (home);
		    cp = tmpfilename;
		    (void) sprintf (tmpfilename, "%sctwm.rc_%d",
				    home, Scr->screen);
		    break;
		}
	    }
	    continue;

	  case 3:
	    if (home) {
		tmpfilename[homelen + 7] = '\0';
	    }
	    break;

	  case 4:
	    if (!filename) {
		home = getenv ("DECW$USER_DEFAULTS");
		if (home) {
		    homelen = strlen (home);
		    cp = tmpfilename;
		    (void) sprintf (tmpfilename, "%stwm.rc_%d",
				    home, Scr->screen);
		    break;
		}
	    }
	    continue;

	  case 5:
	    if (home) {
		tmpfilename[homelen + 6] = '\0';
	    }
	    break;
#else
	  case 0:			/* -f filename.# */
	    if (filename) {
		cp = tmpfilename;
		(void) sprintf (tmpfilename, "%s.%d", filename, Scr->screen);
	    }
	    else cp = filename;
	    break;

	  case 1:			/* -f filename */
	    cp = filename;
	    break;

	  case 2:			/* ~/.ctwmrc.screennum */
	    if (!filename) {
		home = getenv ("HOME");
		if (home) {
		    homelen = strlen (home);
		    cp = tmpfilename;
		    (void) sprintf (tmpfilename, "%s/.ctwmrc.%d",
				    home, Scr->screen);
		    break;
		}
	    }
	    continue;

	  case 3:			/* ~/.ctwmrc */
	    if (home) {
		tmpfilename[homelen + 8] = '\0';
	    }
	    break;

	  case 4:			/* ~/.twmrc.screennum */
	    if (!filename) {
		home = getenv ("HOME");
		if (home) {
		    homelen = strlen (home);
		    cp = tmpfilename;
		    (void) sprintf (tmpfilename, "%s/.twmrc.%d",
				    home, Scr->screen);
		    break;
		}
	    }
	    continue;

	  case 5:			/* ~/.twmrc */
	    if (home) {
		tmpfilename[homelen + 7] = '\0'; /* C.L. */
	    }
	    break;
#endif

	  case 6:			/* system.twmrc */
	    cp = SYSTEM_INIT_FILE;
	    break;
	}

	if (cp) {
            twmrc = fopen (cp, "r");
#ifdef USEM4
            raw = twmrc;
#endif

        }
    }

#ifdef USEM4
    if (raw) {
#else
    if (twmrc) {
#endif
	int status;

	if (filename && strncmp (cp, filename, strlen (filename))) {
	    fprintf (stderr,
		     "%s:  unable to open twmrc file %s, using %s instead\n",
		     ProgramName, filename, cp);
	}
#ifdef USEM4
	if (GoThroughM4) twmrc = start_m4(raw);
	status = doparse (m4twmFileInput, "file", cp);
	wait (0);
	fclose (twmrc);
        fclose (raw);
#else
	status = doparse (twmFileInput, "file", cp);
	fclose (twmrc);
#endif
	return status;
    } else {
	if (filename) {
	    fprintf (stderr,
	"%s:  unable to open twmrc file %s, using built-in defaults instead\n",
		     ProgramName, filename);
	}
	return ParseStringList (defTwmrc);
    }
}

int ParseStringList (sl)
    char **sl;
{
    stringListSource = sl;
    currentString = *sl;
    return doparse (twmStringListInput, "string list", (char *)NULL);
}


/***********************************************************************
 *
 *  Procedure:
 *	twmFileInput - redefinition of the lex input routine for file input
 *
 *  Returned Value:
 *	the next input character
 *
 ***********************************************************************
 */


#ifndef USEM4

/* This has Tom's include() funtionality.  This is utterly useless if you
 * can use m4 for the same thing.               Chris P. Ross */

#define MAX_INCLUDES 10

static struct incl {
     FILE *fp;
     char *name;
     int lineno;
} rc_includes[MAX_INCLUDES];
static int include_file = 0;


static int twmFileInput()
{
    if (overflowlen) return (int) overflowbuff[--overflowlen];

    while (ptr == len)
    {
        while (include_file) {
            if (fgets(buff, BUF_LEN, rc_includes[include_file].fp) == NULL) {
                free(rc_includes[include_file].name);
                fclose(rc_includes[include_file].fp);
                yylineno = rc_includes[include_file--].lineno;
            } else
                break;
        }

        if (!include_file)
	if (fgets(buff, BUF_LEN, twmrc) == NULL)
	    return 0;
	yylineno++;

        if (strncmp(buff, "include", 7) == 0) {
             /* Whoops, an include file! */
             char *p = buff + 7, *q;
             FILE *fp;

             while (isspace(*p)) p++;
             for (q = p; *q && !isspace(*q); q++)
                  continue;
             *q = 0;

             if ((fp = fopen(p, "r")) == NULL) {
                  fprintf(stderr, "%s: Unable to open included init file %s\n",
                          ProgramName, p);
                  continue;
             }
             if (++include_file >= MAX_INCLUDES) {
                  fprintf(stderr, "%s: init file includes nested too deep\n",
                          ProgramName);
                  continue;
             }
             rc_includes[include_file].fp = fp;
             rc_includes[include_file].lineno = yylineno;
             yylineno = 0;
             rc_includes[include_file].name = malloc(strlen(p)+1);
             strcpy(rc_includes[include_file].name, p);
             continue;
        }
	ptr = 0;
	len = strlen(buff);
    }
    return ((int)buff[ptr++]);
}
#else /* USEM4 */
/* If you're going to use m4, use this version instead.  Much simpler.
 * m4 ism's credit to Josh Osborne (stripes) */

static int m4twmFileInput()
{
        if (overflowlen) return((int) overflowbuff[--overflowlen]);

        while (ptr == len) {
                if (fgets(buff, BUF_LEN, twmrc) == NULL) return(0);

                yylineno++;

                ptr = 0;
                len = strlen(buff);
        }
        return ((int)buff[ptr++]);
}
#endif /* USEM4 */


static int twmStringListInput()
{
    if (overflowlen) return (int) overflowbuff[--overflowlen];

    /*
     * return the character currently pointed to
     */
    if (currentString) {
	unsigned int c = (unsigned int) *currentString++;

	if (c) return c;		/* if non-nul char */
	currentString = *++stringListSource;  /* advance to next bol */
	return '\n';			/* but say that we hit last eol */
    }
    return 0;				/* eof */
}


/***********************************************************************
 *
 *  Procedure:
 *	twmUnput - redefinition of the lex unput routine
 *
 *  Inputs:
 *	c	- the character to push back onto the input stream
 *
 ***********************************************************************
 */

void twmUnput (c)
    int c;
{
    if (overflowlen < sizeof overflowbuff) {
	overflowbuff[overflowlen++] = (char) c;
    } else {
	twmrc_error_prefix ();
	fprintf (stderr, "unable to unput character (%c)\n",
		 c);
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	TwmOutput - redefinition of the lex output routine
 *
 *  Inputs:
 *	c	- the character to print
 *
 ***********************************************************************
 */

void
TwmOutput(c)
{
    putchar(c);
}


/**********************************************************************
 *
 *  Parsing table and routines
 * 
 ***********************************************************************/

typedef struct _TwmKeyword {
    char *name;
    int value;
    int subnum;
} TwmKeyword;

#define kw0_NoDefaults			1
#define kw0_AutoRelativeResize		2
#define kw0_ForceIcons			3
#define kw0_NoIconManagers		4
#define kw0_InterpolateMenuColors	6
#define kw0_NoVersion			7
#define kw0_SortIconManager		8
#define kw0_NoGrabServer		9
#define kw0_NoMenuShadows		10
#define kw0_NoRaiseOnMove		11
#define kw0_NoRaiseOnResize		12
#define kw0_NoRaiseOnDeiconify		13
#define kw0_DontMoveOff			14
#define kw0_NoBackingStore		15
#define kw0_NoSaveUnders		16
#define kw0_RestartPreviousState	17
#define kw0_ClientBorderWidth		18
#define kw0_NoTitleFocus		19
#define kw0_DecorateTransients		21
#define kw0_ShowIconManager		22
#define kw0_NoCaseSensitive		23
#define kw0_NoRaiseOnWarp		24
#define kw0_WarpUnmapped		25
#define kw0_ShowWorkspaceManager	27
#define kw0_StartInMapState		28
#define kw0_NoShowOccupyAll		29
#define kw0_AutoOccupy			30
#define kw0_TransientHasOccupation	31
#define kw0_DontPaintRootWindow		32
#define kw0_Use3DMenus			33
#define kw0_Use3DTitles			34
#define kw0_Use3DIconManagers		35
#define kw0_Use3DBorders		36
#define kw0_SunkFocusWindowTitle	37
#define kw0_BeNiceToColormap		38
#define kw0_SmartIconify		39
#define kw0_WarpRingOnScreen		40
#define kw0_NoIconManagerFocus		41
#define kw0_StayUpMenus			42
#define kw0_ClickToFocus		43
#define kw0_BorderResizeCursors		44

#define kws_UsePPosition		1
#define kws_IconFont			2
#define kws_ResizeFont			3
#define kws_MenuFont			4
#define kws_TitleFont			5
#define kws_IconManagerFont		6
#define kws_UnknownIcon			7
#define kws_IconDirectory		8
#define kws_MaxWindowSize		9
#define kws_PixmapDirectory		10
#define kws_RandomPlacement		11
#define kws_IconJustification		12
#define kws_TitleJustification		13
#define kws_IconRegionJustification	14
#ifdef SOUNDS
#define kws_SoundHost			15
#endif

#define kwn_ConstrainedMoveTime		1
#define kwn_MoveDelta			2
#define kwn_XorValue			3
#define kwn_FramePadding		4
#define kwn_TitlePadding		5
#define kwn_ButtonIndent		6
#define kwn_BorderWidth			7
#define kwn_IconBorderWidth		8
#define kwn_TitleButtonBorderWidth	9
#define kwn_RaiseDelay			10
#define kwn_TransientOnTop		11
#define kwn_OpaqueMoveThreshold		12
#define kwn_OpaqueResizeThreshold	13
#define kwn_WMgrVertButtonIndent	14
#define kwn_WMgrHorizButtonIndent	15
#define kwn_ClearShadowContrast		16
#define kwn_DarkShadowContrast		17
#define kwn_WMgrButtonShadowDepth	18
#define kwn_MaxIconTitleWidth		19
#define kwn_AnimationSpeed		20
#define kwn_ThreeDBorderWidth		21
#define kwn_MoveOffResistance		22

#define kwcl_BorderColor		1
#define kwcl_IconManagerHighlight	2
#define kwcl_BorderTileForeground	3
#define kwcl_BorderTileBackground	4
#define kwcl_TitleForeground		5
#define kwcl_TitleBackground		6
#define kwcl_IconForeground		7
#define kwcl_IconBackground		8
#define kwcl_IconBorderColor		9
#define kwcl_IconManagerForeground	10
#define kwcl_IconManagerBackground	11
#define kwcl_MapWindowBackground	12
#define kwcl_MapWindowForeground	13

#define kwc_DefaultForeground		1
#define kwc_DefaultBackground		2
#define kwc_MenuForeground		3
#define kwc_MenuBackground		4
#define kwc_MenuTitleForeground		5
#define kwc_MenuTitleBackground		6
#define kwc_MenuShadowColor		7

/*
 * The following is sorted alphabetically according to name (which must be
 * in lowercase and only contain the letters a-z).  It is fed to a binary
 * search to parse keywords.
 */
static TwmKeyword keytable[] = { 
    { "all",			ALL, 0 },
    { "animationspeed",		NKEYWORD, kwn_AnimationSpeed },
    { "autooccupy",		KEYWORD, kw0_AutoOccupy },
    { "autoraise",		AUTO_RAISE, 0 },
    { "autorelativeresize",	KEYWORD, kw0_AutoRelativeResize },
    { "benicetocolormap",	KEYWORD, kw0_BeNiceToColormap },
    { "bordercolor",		CLKEYWORD, kwcl_BorderColor },
    { "borderresizecursors",	KEYWORD, kw0_BorderResizeCursors },
    { "bordertilebackground",	CLKEYWORD, kwcl_BorderTileBackground },
    { "bordertileforeground",	CLKEYWORD, kwcl_BorderTileForeground },
    { "borderwidth",		NKEYWORD, kwn_BorderWidth },
    { "button",			BUTTON, 0 },
    { "buttonindent",		NKEYWORD, kwn_ButtonIndent },
    { "c",			CONTROL, 0 },
    { "center",			JKEYWORD, J_CENTER },
    { "changeworkspacefunction", CHANGE_WORKSPACE_FUNCTION, 0 },
    { "clearshadowcontrast",	NKEYWORD, kwn_ClearShadowContrast },
    { "clicktofocus",		KEYWORD, kw0_ClickToFocus },
    { "clientborderwidth",	KEYWORD, kw0_ClientBorderWidth },
    { "color",			COLOR, 0 },
    { "constrainedmovetime",	NKEYWORD, kwn_ConstrainedMoveTime },
    { "control",		CONTROL, 0 },
    { "cursors",		CURSORS, 0 },
    { "darkshadowcontrast",	NKEYWORD, kwn_DarkShadowContrast },
    { "decoratetransients",	KEYWORD, kw0_DecorateTransients },
    { "defaultbackground",	CKEYWORD, kwc_DefaultBackground },
    { "defaultforeground",	CKEYWORD, kwc_DefaultForeground },
    { "defaultfunction",	DEFAULT_FUNCTION, 0 },
    { "deiconifyfunction",	DEICONIFY_FUNCTION, 0 },
    { "destroy",		KILL, 0 },
    { "donticonifybyunmapping",	DONT_ICONIFY_BY_UNMAPPING, 0 },
    { "dontmoveoff",		KEYWORD, kw0_DontMoveOff },
    { "dontpaintrootwindow",	KEYWORD, kw0_DontPaintRootWindow },
    { "dontsqueezetitle",	DONT_SQUEEZE_TITLE, 0 },
    { "east",			DKEYWORD, D_EAST },
    { "f",			FRAME, 0 },
    { "f.adoptwindow",		FKEYWORD, F_ADOPTWINDOW },
    { "f.autoraise",		FKEYWORD, F_AUTORAISE },
    { "f.backiconmgr",		FKEYWORD, F_BACKICONMGR },
    { "f.beep",			FKEYWORD, F_BEEP },
    { "f.bottomzoom",		FKEYWORD, F_BOTTOMZOOM },
    { "f.circledown",		FKEYWORD, F_CIRCLEDOWN },
    { "f.circleup",		FKEYWORD, F_CIRCLEUP },
    { "f.colormap",		FSKEYWORD, F_COLORMAP },
    { "f.cut",			FSKEYWORD, F_CUT },
    { "f.cutfile",		FKEYWORD, F_CUTFILE },
    { "f.deiconify",		FKEYWORD, F_DEICONIFY },
    { "f.delete",		FKEYWORD, F_DELETE },
    { "f.deltastop",		FKEYWORD, F_DELTASTOP },
    { "f.destroy",		FKEYWORD, F_DESTROY },
    { "f.downiconmgr",		FKEYWORD, F_DOWNICONMGR },
    { "f.exec",			FSKEYWORD, F_EXEC },
    { "f.file",			FSKEYWORD, F_FILE },
    { "f.focus",		FKEYWORD, F_FOCUS },
    { "f.forcemove",		FKEYWORD, F_FORCEMOVE },
    { "f.forwiconmgr",		FKEYWORD, F_FORWICONMGR },
    { "f.fullzoom",		FKEYWORD, F_FULLZOOM },
    { "f.function",		FSKEYWORD, F_FUNCTION },
    { "f.gotoworkspace",	FSKEYWORD, F_GOTOWORKSPACE },
    { "f.hbzoom",		FKEYWORD, F_BOTTOMZOOM },
    { "f.hideiconmgr",		FKEYWORD, F_HIDELIST },
    { "f.hideworkspacemgr",	FKEYWORD, F_HIDEWORKMGR },
    { "f.horizoom",		FKEYWORD, F_HORIZOOM },
    { "f.htzoom",		FKEYWORD, F_TOPZOOM },
    { "f.hzoom",		FKEYWORD, F_HORIZOOM },
    { "f.iconify",		FKEYWORD, F_ICONIFY },
    { "f.identify",		FKEYWORD, F_IDENTIFY },
    { "f.lefticonmgr",		FKEYWORD, F_LEFTICONMGR },
    { "f.leftzoom",		FKEYWORD, F_LEFTZOOM },
    { "f.lower",		FKEYWORD, F_LOWER },
    { "f.menu",			FSKEYWORD, F_MENU },
    { "f.move",			FKEYWORD, F_MOVE },
    { "f.movemenu",		FKEYWORD, F_MOVEMENU },
    { "f.nexticonmgr",		FKEYWORD, F_NEXTICONMGR },
    { "f.nextworkspace",	FKEYWORD, F_NEXTWORKSPACE },
    { "f.nop",			FKEYWORD, F_NOP },
    { "f.occupy",		FKEYWORD, F_OCCUPY },
    { "f.occupyall",		FKEYWORD, F_OCCUPYALL },
    { "f.pin",			FKEYWORD, F_PIN },
    { "f.previconmgr",		FKEYWORD, F_PREVICONMGR },
    { "f.prevworkspace",	FKEYWORD, F_PREVWORKSPACE },
    { "f.quit",			FKEYWORD, F_QUIT },
    { "f.raise",		FKEYWORD, F_RAISE },
    { "f.raiselower",		FKEYWORD, F_RAISELOWER },
    { "f.refresh",		FKEYWORD, F_REFRESH },
#ifdef SOUNDS
    { "f.rereadsounds",		FKEYWORD, F_REREADSOUNDS },
#endif
    { "f.resize",		FKEYWORD, F_RESIZE },
    { "f.restart",		FKEYWORD, F_RESTART },
    { "f.righticonmgr",		FKEYWORD, F_RIGHTICONMGR },
    { "f.rightzoom",		FKEYWORD, F_RIGHTZOOM },
    { "f.saveyourself",		FKEYWORD, F_SAVEYOURSELF },
    { "f.separator",		FKEYWORD, F_SEPARATOR },
    { "f.setbuttonsstate",	FKEYWORD, F_SETBUTTONSTATE },
    { "f.setmapstate",		FKEYWORD, F_SETMAPSTATE },
    { "f.showiconmgr",		FKEYWORD, F_SHOWLIST },
    { "f.showworkspacemgr",	FKEYWORD, F_SHOWWORKMGR },
    { "f.slowdownanimation",	FKEYWORD, F_SLOWDOWNANIMATION },
    { "f.sorticonmgr",		FKEYWORD, F_SORTICONMGR },
    { "f.source",		FSKEYWORD, F_BEEP },  /* XXX - don't work */
    { "f.speedupanimation",	FKEYWORD, F_SPEEDUPANIMATION },
    { "f.startanimation",	FKEYWORD, F_STARTANIMATION },
    { "f.stopanimation",	FKEYWORD, F_STOPANIMATION },
    { "f.title",		FKEYWORD, F_TITLE },
#ifdef SOUNDS
    { "f.togglesound",		FKEYWORD, F_TOGGLESOUND },
#endif
    { "f.togglestate",		FKEYWORD, F_TOGGLESTATE },
    { "f.topzoom",		FKEYWORD, F_TOPZOOM },
    { "f.twmrc",		FKEYWORD, F_RESTART },
    { "f.unfocus",		FKEYWORD, F_UNFOCUS },
    { "f.upiconmgr",		FKEYWORD, F_UPICONMGR },
    { "f.vanish",		FKEYWORD, F_VANISH },
    { "f.version",		FKEYWORD, F_VERSION },
    { "f.vlzoom",		FKEYWORD, F_LEFTZOOM },
    { "f.vrzoom",		FKEYWORD, F_RIGHTZOOM },
    { "f.test",			FKEYWORD, F_TESTFUNC },
    { "f.warphere",		FSKEYWORD, F_WARPHERE },
    { "f.warpring",		FSKEYWORD, F_WARPRING },
    { "f.warpto",		FSKEYWORD, F_WARPTO },
    { "f.warptoiconmgr",	FSKEYWORD, F_WARPTOICONMGR },
    { "f.warptoscreen",		FSKEYWORD, F_WARPTOSCREEN },
    { "f.winrefresh",		FKEYWORD, F_WINREFRESH },
    { "f.zoom",			FKEYWORD, F_ZOOM },
    { "forceicons",		KEYWORD, kw0_ForceIcons },
    { "frame",			FRAME, 0 },
    { "framepadding",		NKEYWORD, kwn_FramePadding },
    { "function",		FUNCTION, 0 },
    { "i",			ICON, 0 },
    { "icon",			ICON, 0 },
    { "iconbackground",		CLKEYWORD, kwcl_IconBackground },
    { "iconbordercolor",	CLKEYWORD, kwcl_IconBorderColor },
    { "iconborderwidth",	NKEYWORD, kwn_IconBorderWidth },
    { "icondirectory",		SKEYWORD, kws_IconDirectory },
    { "iconfont",		SKEYWORD, kws_IconFont },
    { "iconforeground",		CLKEYWORD, kwcl_IconForeground },
    { "iconifybyunmapping",	ICONIFY_BY_UNMAPPING, 0 },
    { "iconifyfunction",	ICONIFY_FUNCTION, 0 },
    { "iconjustification",	SKEYWORD, kws_IconJustification },
    { "iconmanagerbackground",	CLKEYWORD, kwcl_IconManagerBackground },
    { "iconmanagerdontshow",	ICONMGR_NOSHOW, 0 },
    { "iconmanagerfont",	SKEYWORD, kws_IconManagerFont },
    { "iconmanagerforeground",	CLKEYWORD, kwcl_IconManagerForeground },
    { "iconmanagergeometry",	ICONMGR_GEOMETRY, 0 },
    { "iconmanagerhighlight",	CLKEYWORD, kwcl_IconManagerHighlight },
    { "iconmanagers",		ICONMGRS, 0 },
    { "iconmanagershow",	ICONMGR_SHOW, 0 },
    { "iconmgr",		ICONMGR, 0 },
    { "iconregion",		ICON_REGION, 0 },
    { "iconregionjustification",SKEYWORD, kws_IconRegionJustification },
    { "icons",			ICONS, 0 },
    { "interpolatemenucolors",	KEYWORD, kw0_InterpolateMenuColors },
    { "l",			LOCK, 0 },
    { "left",			JKEYWORD, J_LEFT },
    { "lefttitlebutton",	LEFT_TITLEBUTTON, 0 },
    { "lock",			LOCK, 0 },
    { "m",			META, 0 },
    { "maketitle",		MAKE_TITLE, 0 },
    { "mapwindowbackground",	CLKEYWORD, kwcl_MapWindowBackground },
    { "mapwindowcurrentworkspace", MAPWINDOWCURRENTWORKSPACE, 0},
    { "mapwindowdefaultworkspace", MAPWINDOWDEFAULTWORKSPACE, 0},
    { "mapwindowforeground",	CLKEYWORD, kwcl_MapWindowForeground },
    { "maxicontitlewidth",	NKEYWORD, kwn_MaxIconTitleWidth },
    { "maxwindowsize",		SKEYWORD, kws_MaxWindowSize },
    { "menu",			MENU, 0 },
    { "menubackground",		CKEYWORD, kwc_MenuBackground },
    { "menufont",		SKEYWORD, kws_MenuFont },
    { "menuforeground",		CKEYWORD, kwc_MenuForeground },
    { "menushadowcolor",	CKEYWORD, kwc_MenuShadowColor },
    { "menutitlebackground",	CKEYWORD, kwc_MenuTitleBackground },
    { "menutitleforeground",	CKEYWORD, kwc_MenuTitleForeground },
    { "meta",			META, 0 },
    { "mod",			META, 0 },  /* fake it */
    { "monochrome",		MONOCHROME, 0 },
    { "move",			MOVE, 0 },
    { "movedelta",		NKEYWORD, kwn_MoveDelta },
    { "moveoffresistance",	NKEYWORD, kwn_MoveOffResistance },
    { "nobackingstore",		KEYWORD, kw0_NoBackingStore },
    { "noborder",		NO_BORDER, 0 },
    { "nocasesensitive",	KEYWORD, kw0_NoCaseSensitive },
    { "nodefaults",		KEYWORD, kw0_NoDefaults },
    { "nograbserver",		KEYWORD, kw0_NoGrabServer },
    { "nohighlight",		NO_HILITE, 0 },
    { "noiconmanagerfocus",	KEYWORD, kw0_NoIconManagerFocus },
    { "noiconmanagers",		KEYWORD, kw0_NoIconManagers },
    { "noicontitle",		NO_ICON_TITLE, 0  },
    { "nomenushadows",		KEYWORD, kw0_NoMenuShadows },
    { "noopaquemove",		NOOPAQUEMOVE, 0 },
    { "noopaqueresize",		NOOPAQUERESIZE, 0 },
    { "noraiseondeiconify",	KEYWORD, kw0_NoRaiseOnDeiconify },
    { "noraiseonmove",		KEYWORD, kw0_NoRaiseOnMove },
    { "noraiseonresize",	KEYWORD, kw0_NoRaiseOnResize },
    { "noraiseonwarp",		KEYWORD, kw0_NoRaiseOnWarp },
    { "north",			DKEYWORD, D_NORTH },
    { "nosaveunders",		KEYWORD, kw0_NoSaveUnders },
    { "noshowoccupyall",	KEYWORD, kw0_NoShowOccupyAll },
    { "nostackmode",		NO_STACKMODE, 0 },
    { "notitle",		NO_TITLE, 0 },
    { "notitlefocus",		KEYWORD, kw0_NoTitleFocus },
    { "notitlehighlight",	NO_TITLE_HILITE, 0 },
    { "noversion",		KEYWORD, kw0_NoVersion },
    { "occupy",			OCCUPYLIST, 0 },
    { "occupyall",		OCCUPYALL, 0 },
    { "opaquemove",		OPAQUEMOVE, 0 },
    { "opaquemovethreshold",	NKEYWORD, kwn_OpaqueMoveThreshold },
    { "opaqueresize",		OPAQUERESIZE, 0 },
    { "opaqueresizethreshold",	NKEYWORD, kwn_OpaqueResizeThreshold },
    { "pixmapdirectory",	SKEYWORD, kws_PixmapDirectory },
    { "pixmaps",		PIXMAPS, 0 },
    { "r",			ROOT, 0 },
    { "raisedelay",		NKEYWORD, kwn_RaiseDelay },
    { "randomplacement",	SKEYWORD, kws_RandomPlacement },
    { "resize",			RESIZE, 0 },
    { "resizefont",		SKEYWORD, kws_ResizeFont },
    { "restartpreviousstate",	KEYWORD, kw0_RestartPreviousState },
    { "right",			JKEYWORD, J_RIGHT },
    { "righttitlebutton",	RIGHT_TITLEBUTTON, 0 },
    { "root",			ROOT, 0 },
    { "s",			SHIFT, 0 },
    { "savecolor",              SAVECOLOR, 0},
    { "select",			SELECT, 0 },
    { "shift",			SHIFT, 0 },
    { "showiconmanager",	KEYWORD, kw0_ShowIconManager },
    { "showworkspacemanager",	KEYWORD, kw0_ShowWorkspaceManager },
    { "smarticonify",		KEYWORD, kw0_SmartIconify },
    { "sorticonmanager",	KEYWORD, kw0_SortIconManager },
#ifdef SOUNDS
    { "soundhost",		SKEYWORD, kws_SoundHost },
#endif
    { "south",			DKEYWORD, D_SOUTH },
    { "squeezetitle",		SQUEEZE_TITLE, 0 },
    { "starticonified",		START_ICONIFIED, 0 },
    { "startinmapstate",	KEYWORD, kw0_StartInMapState },
    { "stayupmenus",		KEYWORD, kw0_StayUpMenus },
    { "sunkfocuswindowtitle",	KEYWORD, kw0_SunkFocusWindowTitle },
    { "t",			TITLE, 0 },
    { "threedborderwidth",	NKEYWORD, kwn_ThreeDBorderWidth },
    { "title",			TITLE, 0 },
    { "titlebackground",	CLKEYWORD, kwcl_TitleBackground },
    { "titlebuttonborderwidth",	NKEYWORD, kwn_TitleButtonBorderWidth },
    { "titlefont",		SKEYWORD, kws_TitleFont },
    { "titleforeground",	CLKEYWORD, kwcl_TitleForeground },
    { "titlehighlight",		TITLE_HILITE, 0 },
    { "titlejustification",	SKEYWORD, kws_TitleJustification },
    { "titlepadding",		NKEYWORD, kwn_TitlePadding },
    { "transienthasoccupation",	KEYWORD, kw0_TransientHasOccupation },
    { "transientontop",		NKEYWORD, kwn_TransientOnTop },
    { "unknownicon",		SKEYWORD, kws_UnknownIcon },
    { "usepposition",		SKEYWORD, kws_UsePPosition },
    { "usethreedborders",	KEYWORD, kw0_Use3DBorders },
    { "usethreediconmanagers",	KEYWORD, kw0_Use3DIconManagers },
    { "usethreedmenus",		KEYWORD, kw0_Use3DMenus },
    { "usethreedtitles",	KEYWORD, kw0_Use3DTitles },
    { "w",			WINDOW, 0 },
    { "wait",			WAITC, 0 },
    { "warpcursor",		WARP_CURSOR, 0 },
    { "warpringonscreen",	KEYWORD, kw0_WarpRingOnScreen },
    { "warpunmapped",		KEYWORD, kw0_WarpUnmapped },
    { "west",			DKEYWORD, D_WEST },
    { "window",			WINDOW, 0 },
    { "windowfunction",		WINDOW_FUNCTION, 0 },
    { "windowring",		WINDOW_RING, 0 },
    { "wmgrbuttonshadowdepth",	NKEYWORD, kwn_WMgrButtonShadowDepth },
    { "wmgrhorizbuttonindent",	NKEYWORD, kwn_WMgrHorizButtonIndent },
    { "wmgrvertbuttonindent",	NKEYWORD, kwn_WMgrVertButtonIndent },
    { "workspace", 		WORKSPACE, 0 },
    { "workspacemanagergeometry", WORKSPCMGR_GEOMETRY, 0 },
    { "workspaces",             WORKSPACES, 0},
    { "xorvalue",		NKEYWORD, kwn_XorValue },
    { "xpmicondirectory",	SKEYWORD, kws_PixmapDirectory },
    { "zoom",			ZOOM, 0 },
};

static int numkeywords = (sizeof(keytable)/sizeof(keytable[0]));

int parse_keyword (s, nump)
    char *s;
    int *nump;
{
    register int lower = 0, upper = numkeywords - 1;

    XmuCopyISOLatin1Lowered (s, s);
    while (lower <= upper) {
        int middle = (lower + upper) / 2;
	TwmKeyword *p = &keytable[middle];
        int res = strcmp (p->name, s);

        if (res < 0) {
            lower = middle + 1;
        } else if (res == 0) {
	    *nump = p->subnum;
            return p->value;
        } else {
            upper = middle - 1;
        }
    }
    return ERRORTOKEN;
}



/*
 * action routines called by grammar
 */

int do_single_keyword (keyword)
    int keyword;
{
    switch (keyword) {
      case kw0_NoDefaults:
	Scr->NoDefaults = TRUE;
	return 1;

      case kw0_AutoRelativeResize:
	Scr->AutoRelativeResize = TRUE;
	return 1;

      case kw0_ForceIcons:
	if (Scr->FirstTime) Scr->ForceIcon = TRUE;
	return 1;

      case kw0_NoIconManagers:
	Scr->NoIconManagers = TRUE;
	return 1;

      case kw0_InterpolateMenuColors:
	if (Scr->FirstTime) Scr->InterpolateMenuColors = TRUE;
	return 1;

      case kw0_NoVersion:
	/* obsolete */
	return 1;

      case kw0_SortIconManager:
	if (Scr->FirstTime) Scr->SortIconMgr = TRUE;
	return 1;

      case kw0_NoGrabServer:
	Scr->NoGrabServer = TRUE;
	return 1;

      case kw0_NoMenuShadows:
	if (Scr->FirstTime) Scr->Shadow = FALSE;
	return 1;

      case kw0_NoRaiseOnMove:
	if (Scr->FirstTime) Scr->NoRaiseMove = TRUE;
	return 1;

      case kw0_NoRaiseOnResize:
	if (Scr->FirstTime) Scr->NoRaiseResize = TRUE;
	return 1;

      case kw0_NoRaiseOnDeiconify:
	if (Scr->FirstTime) Scr->NoRaiseDeicon = TRUE;
	return 1;

      case kw0_DontMoveOff:
	Scr->DontMoveOff = TRUE;
	return 1;

      case kw0_NoBackingStore:
	Scr->BackingStore = FALSE;
	return 1;

      case kw0_NoSaveUnders:
	Scr->SaveUnder = FALSE;
	return 1;

      case kw0_RestartPreviousState:
	RestartPreviousState = True;
	return 1;

      case kw0_ClientBorderWidth:
	if (Scr->FirstTime) Scr->ClientBorderWidth = TRUE;
	return 1;

      case kw0_NoTitleFocus:
	Scr->TitleFocus = TRUE /*FALSE*/;
	return 1;

      case kw0_DecorateTransients:
	Scr->DecorateTransients = TRUE;
	return 1;

      case kw0_ShowIconManager:
	Scr->ShowIconManager = TRUE;
	return 1;

      case kw0_ShowWorkspaceManager:
	Scr->ShowWorkspaceManager = TRUE;
	return 1;

      case kw0_StartInMapState:
	Scr->workSpaceMgr.workspaceWindow.state = MAPSTATE;
	return 1;

      case kw0_NoShowOccupyAll:
	Scr->workSpaceMgr.workspaceWindow.noshowoccupyall = TRUE;
	return 1;

      case kw0_AutoOccupy:
	Scr->AutoOccupy = TRUE;
	return 1;

      case kw0_TransientHasOccupation:
	Scr->TransientHasOccupation = TRUE;
	return 1;

      case kw0_DontPaintRootWindow:
	Scr->DontPaintRootWindow = TRUE;
	return 1;

      case kw0_Use3DBorders:
	Scr->use3Dborders = TRUE;
	return 1;

      case kw0_Use3DIconManagers:
	Scr->use3Diconmanagers = TRUE;
	return 1;

      case kw0_Use3DMenus:
	Scr->use3Dmenus = TRUE;
	return 1;

      case kw0_Use3DTitles:
	Scr->use3Dtitles = TRUE;
	return 1;

      case kw0_SunkFocusWindowTitle:
	Scr->SunkFocusWindowTitle = TRUE;
	return 1;

      case kw0_BeNiceToColormap:
	Scr->BeNiceToColormap = TRUE;
	return 1;

      case kw0_SmartIconify:
	Scr->SmartIconify = TRUE;
	return 1;

      case kw0_BorderResizeCursors:
	Scr->BorderCursors = TRUE;
	return 1;

      case kw0_NoCaseSensitive:
	Scr->CaseSensitive = FALSE;
	return 1;

      case kw0_NoRaiseOnWarp:
	Scr->NoRaiseWarp = TRUE;
	return 1;

      case kw0_WarpUnmapped:
	Scr->WarpUnmapped = TRUE;
	return 1;

      case kw0_WarpRingOnScreen:
	Scr->WarpRingAnyWhere = FALSE;
	return 1;

      case kw0_NoIconManagerFocus:
	Scr->IconManagerFocus = FALSE;
	return 1;

      case kw0_StayUpMenus:
	Scr->StayUpMenus = TRUE;
	return 1;

      case kw0_ClickToFocus:
	Scr->ClickToFocus = TRUE;
	return 1;
    }

    return 0;
}


int do_string_keyword (keyword, s)
    int keyword;
    char *s;
{
    switch (keyword) {
       case kws_RandomPlacement:
 	{
 	    int rp = ParseRandomPlacement (s);
 	    if (rp < 0) {
 		twmrc_error_prefix();
 		fprintf (stderr,
 			 "ignoring invalid RandomPlacement argument \"%s\"\n", s);
 	    } else {
 		Scr->RandomPlacement = rp;
 	    }
 	    return 1;
 	}
      case kws_UsePPosition:
	{ 
	    int ppos = ParseUsePPosition (s);
	    if (ppos < 0) {
		twmrc_error_prefix();
		fprintf (stderr,
			 "ignoring invalid UsePPosition argument \"%s\"\n", s);
	    } else {
		Scr->UsePPosition = ppos;
	    }
	    return 1;
	}

      case kws_IconFont:
	if (!Scr->HaveFonts) Scr->IconFont.name = s;
	return 1;

      case kws_ResizeFont:
	if (!Scr->HaveFonts) Scr->SizeFont.name = s;
	return 1;

      case kws_MenuFont:
	if (!Scr->HaveFonts) Scr->MenuFont.name = s;
	return 1;

      case kws_TitleFont:
	if (!Scr->HaveFonts) Scr->TitleBarFont.name = s;
	return 1;

      case kws_IconManagerFont:
	if (!Scr->HaveFonts) Scr->IconManagerFont.name = s;
	return 1;

      case kws_UnknownIcon:
	if (Scr->FirstTime) GetUnknownIcon (s);
	return 1;

      case kws_IconDirectory:
	if (Scr->FirstTime) Scr->IconDirectory = ExpandFilename (s);
	return 1;

      case kws_PixmapDirectory:
	if (Scr->FirstTime) Scr->PixmapDirectory = ExpandFilename (s);
	return 1;

      case kws_MaxWindowSize:
	JunkMask = XParseGeometry (s, &JunkX, &JunkY, &JunkWidth, &JunkHeight);
	if ((JunkMask & (WidthValue | HeightValue)) != 
	    (WidthValue | HeightValue)) {
	    twmrc_error_prefix();
	    fprintf (stderr, "bad MaxWindowSize \"%s\"\n", s);
	    return 0;
	}
	if (JunkWidth <= 0 || JunkHeight <= 0) {
	    twmrc_error_prefix();
	    fprintf (stderr, "MaxWindowSize \"%s\" must be positive\n", s);
	    return 0;
	}
	Scr->MaxWindowWidth = JunkWidth;
	Scr->MaxWindowHeight = JunkHeight;
	return 1;

      case kws_IconJustification:
	{
	    int just = ParseJustification (s);

 	    if ((just < 0) || (just == J_BORDER)) {
 		twmrc_error_prefix();
 		fprintf (stderr,
 			 "ignoring invalid IconJustification argument \"%s\"\n", s);
 	    } else {
 		Scr->IconJustification = just;
 	    }
 	    return 1;
 	}
      case kws_IconRegionJustification:
	{
	    int just = ParseJustification (s);

 	    if (just < 0) {
 		twmrc_error_prefix();
 		fprintf (stderr,
 			 "ignoring invalid IconRegionJustification argument \"%s\"\n", s);
 	    } else {
 		Scr->IconRegionJustification = just;
 	    }
 	    return 1;
 	}
      case kws_TitleJustification:
	{
	    int just = ParseJustification (s);

 	    if ((just < 0) || (just == J_BORDER)) {
 		twmrc_error_prefix();
 		fprintf (stderr,
 			 "ignoring invalid TitleJustification argument \"%s\"\n", s);
 	    } else {
 		Scr->TitleJustification = just;
 	    }
 	    return 1;
 	}
#ifdef SOUNDS
      case kws_SoundHost:
        if (Scr->FirstTime) set_sound_host(s);
        return 1;
#endif
		
    }
    return 0;
}


int do_number_keyword (keyword, num)
    int keyword;
    int num;
{
    switch (keyword) {
      case kwn_ConstrainedMoveTime:
	ConstrainedMoveTime = num;
	return 1;

      case kwn_MoveDelta:
	Scr->MoveDelta = num;
	return 1;

      case kwn_MoveOffResistance:
	Scr->MoveOffResistance = num;
	return 1;

      case kwn_XorValue:
	if (Scr->FirstTime) Scr->XORvalue = num;
	return 1;

      case kwn_FramePadding:
	if (Scr->FirstTime) Scr->FramePadding = num;
	return 1;

      case kwn_TitlePadding:
	if (Scr->FirstTime) {
	    Scr->TitlePadding = num;
	}
	return 1;

      case kwn_ButtonIndent:
	if (Scr->FirstTime) Scr->ButtonIndent = num;
	return 1;

      case kwn_ThreeDBorderWidth:
	if (Scr->FirstTime) Scr->ThreeDBorderWidth = num;
	return 1;

      case kwn_BorderWidth:
	if (Scr->FirstTime) Scr->BorderWidth = num;
	return 1;

      case kwn_IconBorderWidth:
	if (Scr->FirstTime) Scr->IconBorderWidth = num;
	return 1;

      case kwn_TitleButtonBorderWidth:
	if (Scr->FirstTime) Scr->TBInfo.border = num;
	return 1;

      case kwn_RaiseDelay:
	RaiseDelay = num;
	return 1;

      case kwn_TransientOnTop:
	if (Scr->FirstTime) Scr->TransientOnTop = num;
	return 1;

      case kwn_OpaqueMoveThreshold:
	if (Scr->FirstTime) Scr->OpaqueMoveThreshold = num;
	return 1;

      case kwn_OpaqueResizeThreshold:
	if (Scr->FirstTime) Scr->OpaqueResizeThreshold = num;
	return 1;

      case kwn_WMgrVertButtonIndent:
	if (Scr->FirstTime) Scr->WMgrVertButtonIndent = num;
	if (Scr->WMgrVertButtonIndent < 0) Scr->WMgrVertButtonIndent = 0;
	return 1;

      case kwn_WMgrHorizButtonIndent:
	if (Scr->FirstTime) Scr->WMgrHorizButtonIndent = num;
	if (Scr->WMgrHorizButtonIndent < 0) Scr->WMgrHorizButtonIndent = 0;
	return 1;

      case kwn_WMgrButtonShadowDepth:
	if (Scr->FirstTime) Scr->WMgrButtonShadowDepth = num;
	if (Scr->WMgrButtonShadowDepth < 0) Scr->WMgrButtonShadowDepth = 2;
	return 1;

      case kwn_MaxIconTitleWidth:
	if (Scr->FirstTime) Scr->MaxIconTitleWidth = num;
	return 1;

      case kwn_ClearShadowContrast:
	if (Scr->FirstTime) Scr->ClearShadowContrast = num;
	if (Scr->ClearShadowContrast <   0) Scr->ClearShadowContrast =   0;
	if (Scr->ClearShadowContrast > 100) Scr->ClearShadowContrast = 100;
	return 1;

      case kwn_DarkShadowContrast:
	if (Scr->FirstTime) Scr->DarkShadowContrast = num;
	if (Scr->DarkShadowContrast <   0) Scr->DarkShadowContrast =   0;
	if (Scr->DarkShadowContrast > 100) Scr->DarkShadowContrast = 100;
	return 1;

      case kwn_AnimationSpeed:
	if (num < 0) num = 0;
	SetAnimationSpeed (num);
	return 1;
    }

    return 0;
}

name_list **do_colorlist_keyword (keyword, colormode, s)
    int keyword;
    int colormode;
    char *s;
{
    switch (keyword) {
      case kwcl_BorderColor:
	GetColor (colormode, &Scr->BorderColorC.back, s);
	return &Scr->BorderColorL;

      case kwcl_IconManagerHighlight:
	GetColor (colormode, &Scr->IconManagerHighlight, s);
	return &Scr->IconManagerHighlightL;

      case kwcl_BorderTileForeground:
	GetColor (colormode, &Scr->BorderTileC.fore, s);
	return &Scr->BorderTileForegroundL;

      case kwcl_BorderTileBackground:
	GetColor (colormode, &Scr->BorderTileC.back, s);
	return &Scr->BorderTileBackgroundL;

      case kwcl_TitleForeground:
	GetColor (colormode, &Scr->TitleC.fore, s);
	return &Scr->TitleForegroundL;

      case kwcl_TitleBackground:
	GetColor (colormode, &Scr->TitleC.back, s);
	return &Scr->TitleBackgroundL;

      case kwcl_IconForeground:
	GetColor (colormode, &Scr->IconC.fore, s);
	return &Scr->IconForegroundL;

      case kwcl_IconBackground:
	GetColor (colormode, &Scr->IconC.back, s);
	return &Scr->IconBackgroundL;

      case kwcl_IconBorderColor:
	GetColor (colormode, &Scr->IconBorderColor, s);
	return &Scr->IconBorderColorL;

      case kwcl_IconManagerForeground:
	GetColor (colormode, &Scr->IconManagerC.fore, s);
	return &Scr->IconManagerFL;

      case kwcl_IconManagerBackground:
	GetColor (colormode, &Scr->IconManagerC.back, s);
	return &Scr->IconManagerBL;

      case kwcl_MapWindowBackground:
	GetColor (colormode, &Scr->workSpaceMgr.workspaceWindow.windowcp.back, s);
	return &Scr->workSpaceMgr.workspaceWindow.windowBackgroundL;

      case kwcl_MapWindowForeground:
	GetColor (colormode, &Scr->workSpaceMgr.workspaceWindow.windowcp.fore, s);
	return &Scr->workSpaceMgr.workspaceWindow.windowForegroundL;
    }
    return NULL;
}

int do_color_keyword (keyword, colormode, s)
    int keyword;
    int colormode;
    char *s;
{
    switch (keyword) {
      case kwc_DefaultForeground:
	GetColor (colormode, &Scr->DefaultC.fore, s);
	return 1;

      case kwc_DefaultBackground:
	GetColor (colormode, &Scr->DefaultC.back, s);
	return 1;

      case kwc_MenuForeground:
	GetColor (colormode, &Scr->MenuC.fore, s);
	return 1;

      case kwc_MenuBackground:
	GetColor (colormode, &Scr->MenuC.back, s);
	return 1;

      case kwc_MenuTitleForeground:
	GetColor (colormode, &Scr->MenuTitleC.fore, s);
	return 1;

      case kwc_MenuTitleBackground:
	GetColor (colormode, &Scr->MenuTitleC.back, s);
	return 1;

      case kwc_MenuShadowColor:
	GetColor (colormode, &Scr->MenuShadowColor, s);
	return 1;

    }

    return 0;
}

/*
 * put_pixel_on_root() Save a pixel value in twm root window color property.
 */
put_pixel_on_root(pixel)                                 
    Pixel pixel;                                         
{                                                        
  int           i, addPixel = 1;
  Atom          pixelAtom, retAtom;	                 
  int           retFormat;
  unsigned long nPixels, retAfter;                     
  Pixel        *retProp;
  pixelAtom = XInternAtom(dpy, "_MIT_PRIORITY_COLORS", True);        
  XGetWindowProperty(dpy, Scr->Root, pixelAtom, 0, 8192, 
		     False, XA_CARDINAL, &retAtom,       
		     &retFormat, &nPixels, &retAfter,    
		     (unsigned char **)&retProp);

  for (i=0; i< nPixels; i++)                             
      if (pixel == retProp[i]) addPixel = 0;             
                                                         
  if (addPixel)                                          
      XChangeProperty (dpy, Scr->Root, _XA_MIT_PRIORITY_COLORS,
		       XA_CARDINAL, 32, PropModeAppend,  
		       (unsigned char *)&pixel, 1);                       
}                                                        

/*
 * do_string_savecolor() save a color from a string in the twmrc file.
 */
int do_string_savecolor(colormode, s)
     int colormode;
     char *s;
{
  Pixel p;
  GetColor(colormode, &p, s);
  put_pixel_on_root(p);
}

/*
 * do_var_savecolor() save a color from a var in the twmrc file.
 */
typedef struct _cnode {int i; struct _cnode *next;} Cnode, *Cptr;
Cptr chead = NULL;

int do_var_savecolor(key)
int key;
{
  Cptr cptrav, cpnew;
  if (!chead) {
    chead = (Cptr)malloc(sizeof(Cnode));
    chead->i = key; chead->next = NULL;
  }
  else {
    cptrav = chead;
    while (cptrav->next != NULL) { cptrav = cptrav->next; }
    cpnew = (Cptr)malloc(sizeof(Cnode));
    cpnew->i = key; cpnew->next = NULL; cptrav->next = cpnew;
  }
}

/*
 * assign_var_savecolor() traverse the var save color list placeing the pixels
 *                        in the root window property.
 */
void assign_var_savecolor()
{
  Cptr cp = chead;
  while (cp != NULL) {
    switch (cp->i) {
    case kwcl_BorderColor:
      put_pixel_on_root(Scr->BorderColorC.back);
      break;
    case kwcl_IconManagerHighlight:
      put_pixel_on_root(Scr->IconManagerHighlight);
      break;
    case kwcl_BorderTileForeground:
      put_pixel_on_root(Scr->BorderTileC.fore);
      break;
    case kwcl_BorderTileBackground:
      put_pixel_on_root(Scr->BorderTileC.back);
      break;
    case kwcl_TitleForeground:
      put_pixel_on_root(Scr->TitleC.fore);
      break;
    case kwcl_TitleBackground:
      put_pixel_on_root(Scr->TitleC.back);
      break;
    case kwcl_IconForeground:
      put_pixel_on_root(Scr->IconC.fore);
      break;
    case kwcl_IconBackground:
      put_pixel_on_root(Scr->IconC.back);
      break;
    case kwcl_IconBorderColor:
      put_pixel_on_root(Scr->IconBorderColor);
      break;
    case kwcl_IconManagerForeground:
      put_pixel_on_root(Scr->IconManagerC.fore);
      break;
    case kwcl_IconManagerBackground:
      put_pixel_on_root(Scr->IconManagerC.back);
      break;
    case kwcl_MapWindowForeground:
      put_pixel_on_root(Scr->workSpaceMgr.workspaceWindow.windowcp.fore);
      break;
    case kwcl_MapWindowBackground:
      put_pixel_on_root(Scr->workSpaceMgr.workspaceWindow.windowcp.back);
      break;
    }
    cp = cp->next;
  }
  if (chead) {
    free(chead);
    chead = NULL;
  }
}

static int ParseRandomPlacement (s)
    register char *s;
{
    if (strlen (s) == 0) return RP_ALL;
    if (strcmp (s, "default") == 0) return RP_ALL;
    XmuCopyISOLatin1Lowered (s, s);

    if (strcmp (s,      "off") == 0) return RP_OFF;
    if (strcmp (s,       "on") == 0) return RP_ALL;
    if (strcmp (s,      "all") == 0) return RP_ALL;
    if (strcmp (s, "unmapped") == 0) return RP_UNMAPPED;
    return (-1);
}

static int ParseJustification (s)
    register char *s;
{
    if (strlen (s) == 0) return (-1);
    if (strcmp (s, "default") == 0) return J_CENTER;
    XmuCopyISOLatin1Lowered (s, s);

    if (strcmp (s, "default") == 0) return J_CENTER;
    if (strcmp (s,    "left") == 0) return J_LEFT;
    if (strcmp (s,  "center") == 0) return J_CENTER;
    if (strcmp (s,   "right") == 0) return J_RIGHT;
    if (strcmp (s,  "border") == 0) return J_BORDER;
    return (-1);
}

static int ParseUsePPosition (s)
    register char *s;
{
    if (strlen (s) == 0) return (-1);
    if (strcmp (s,  "default") == 0) return PPOS_OFF;
    XmuCopyISOLatin1Lowered (s, s);

    if (strcmp (s,  "default") == 0) return PPOS_OFF;
    if (strcmp (s,      "off") == 0) return PPOS_OFF;
    if (strcmp (s,       "on") == 0) return PPOS_ON;
    if (strcmp (s, "non-zero") == 0) return PPOS_NON_ZERO;
    if (strcmp (s,  "nonzero") == 0) return PPOS_NON_ZERO;
    return (-1);
}


do_squeeze_entry (list, name, justify, num, denom)
    name_list **list;			/* squeeze or dont-squeeze list */
    char *name;				/* window name */
    int justify;			/* left, center, right */
    int num;				/* signed num */
    int denom;				/* 0 or indicates fraction denom */
{
    int absnum = (num < 0 ? -num : num);

    if (denom < 0) {
	twmrc_error_prefix();
	fprintf (stderr, "negative SqueezeTitle denominator %d\n", denom);
	return (1);
    }
    if (absnum > denom && denom != 0) {
	twmrc_error_prefix();
	fprintf (stderr, "SqueezeTitle fraction %d/%d outside window\n",
		 num, denom);
	return (1);
    }
    if (denom == 1) {
	twmrc_error_prefix();
	fprintf (stderr, "useless SqueezeTitle faction %d/%d, assuming 0/0\n",
		 num, denom);
	num = 0;
	denom = 0;
    }

    if (HasShape) {
	SqueezeInfo *sinfo;
	sinfo = (SqueezeInfo *) malloc (sizeof(SqueezeInfo));

	if (!sinfo) {
	    twmrc_error_prefix();
	    fprintf (stderr, "unable to allocate %d bytes for squeeze info\n",
		     sizeof(SqueezeInfo));
	    return (1);
	}
	sinfo->justify = justify;
	sinfo->num = num;
	sinfo->denom = denom;
	AddToList (list, name, (char *) sinfo);
    }
    return (0);
}

#ifdef USEM4

static FILE *start_m4(fraw)
FILE *fraw;
{
        int fno;
        int fids[2];
        int fres;               /* Fork result */

        fno = fileno(fraw);
        /* if (-1 == fcntl(fno, F_SETFD, 0)) perror("fcntl()"); */
        pipe(fids);
        fres = fork();
        if (fres < 0) {
                perror("Fork for m4 failed");
                exit(23);
        }
        if (fres == 0) {
                extern Display *dpy;
                extern char *display_name;
                char *tmp_file;

                /* Child */
                close(0);                       /* stdin */
                close(1);                       /* stdout */
                dup2(fno, 0);           /* stdin = fraw */
                dup2(fids[1], 1);       /* stdout = pipe to parent */
                /* get_defs("m4", dpy, display_name) */
                tmp_file = m4_defs(dpy, display_name);
                execlp("m4", "m4", tmp_file, "-", NULL);
                /* If we get here we are screwed... */
                perror("Can't execlp() m4");
                exit(124);
        }
        /* Parent */
        close(fids[1]);
        return ((FILE*)fdopen(fids[0], "r"));
}

/* Code taken and munged from xrdb.c */
#define MAXHOSTNAME 255
#define Resolution(pixels, mm)  ((((pixels) * 100000 / (mm)) + 50) / 100)
#define EXTRA   12

static char *MkDef(name, def)
char *name, *def;
{
        static char *cp = NULL;
        static int maxsize = 0;
        int n, nl;

	if (def == NULL) return ("");     /* XXX JWS: prevent segfaults */
        /* The char * storage only lasts for 1 call... */
        if ((n = EXTRA + ((nl = strlen(name)) +  strlen(def))) > maxsize) {
		maxsize = n;
                if (cp) free(cp);

                cp = malloc(n);
        }
        /* Otherwise cp is aready big 'nuf */
        if (cp == NULL) {
                fprintf(stderr, "Can't get %d bytes for arg parm\n", n);
                exit(468);
        }
        strcpy(cp, "define(");
        strcpy(cp+7, name);
        *(cp + nl + 7) = ',';
        *(cp + nl + 8) = ' ';
        strcpy(cp + nl + 9, def);
        strcat(cp + nl + 9, ")\n");

        return(cp);
}

static char *MkQte(name, def)
char *name, *def;
{
        char *cp, *cp2;

        cp = malloc(2 + strlen(def));
        if (cp == NULL) {
                fprintf(stderr, "Can't get %d bytes for arg parm\n", 2 + strlen(
def));
                exit(469);
        }
        *cp = '\"';
        strcpy(cp + 1, def);
        strcat(cp, "\"");
        cp2 = MkDef(name, cp);
        free(cp);               /* Not really needed, but good habits die hard..
. */
        return(cp2);
}

static char *MkNum(name, def)
char *name;
int def;
{
        char num[20];

        sprintf(num, "%d", def);
        return(MkDef(name, num));
}

#ifdef NOSTEMP
int mkstemp(str)
char *str;
{
        int fd;

        mktemp(str);
        fd = creat(str, 0744);
        if (fd == -1) perror("mkstemp's creat");
        return(fd);
}
#endif

static char *m4_defs(display, host)
Display *display;
char *host;
{
        extern int KeepTmpFile;
        int i;
        Screen *screen;
        Visual *visual;
        char client[MAXHOSTNAME], server[MAXHOSTNAME], *colon;
        struct hostent *hostname;
        char *vc;               /* Visual Class */
        static char tmp_name[] = "/tmp/twmrcXXXXXX";
        int fd;
        FILE *tmpf;
	char *user;

        fd = mkstemp(tmp_name);		/* I *hope* mkstemp exists, because */
					/* I tried to find the "portable" */
					/* mktmp... */
        if (fd < 0) {
                perror("mkstemp failed in m4_defs");
                exit(377);
        }
        tmpf = (FILE*) fdopen(fd, "w+");
        XmuGetHostname(client, MAXHOSTNAME);
        hostname = gethostbyname(client);
        strcpy(server, XDisplayName(host));
        colon = strchr(server, ':');
        if (colon != NULL) *colon = '\0';
        if ((server[0] == '\0') || (!strcmp(server, "unix")))
                strcpy(server, client); /* must be connected to :0 or unix:0 */
        /* The machine running the X server */
        fputs(MkDef("SERVERHOST", server), tmpf);
        /* The machine running the window manager process */
        fputs(MkDef("CLIENTHOST", client), tmpf);
        if (hostname)
                fputs(MkDef("HOSTNAME", hostname->h_name), tmpf);
        else
                fputs(MkDef("HOSTNAME", client), tmpf);

	if (!(user=getenv("USER")) && !(user=getenv("LOGNAME"))) user = "unknown";
        fputs(MkDef("USER", user), tmpf);
        fputs(MkDef("HOME", getenv("HOME")), tmpf);
        fputs(MkNum("VERSION", ProtocolVersion(display)), tmpf);
        fputs(MkNum("REVISION", ProtocolRevision(display)), tmpf);
        fputs(MkDef("VENDOR", ServerVendor(display)), tmpf);
        fputs(MkNum("RELEASE", VendorRelease(display)), tmpf);
        screen = ScreenOfDisplay(display, Scr->screen);
        visual = DefaultVisualOfScreen(screen);
        fputs(MkNum("WIDTH", screen->width), tmpf);
        fputs(MkNum("HEIGHT", screen->height), tmpf);
        fputs(MkNum("X_RESOLUTION",Resolution(screen->width,screen->mwidth)), tmpf);
        fputs(MkNum("Y_RESOLUTION",Resolution(screen->height,screen->mheight)),tmpf);
        fputs(MkNum("PLANES",DisplayPlanes(display, Scr->screen)), tmpf);
        fputs(MkNum("BITS_PER_RGB", visual->bits_per_rgb), tmpf);
        fputs(MkDef("TWM_TYPE", "ctwm"), tmpf);
        switch(visual->class) {
                case(StaticGray):       vc = "StaticGray";      break;
                case(GrayScale):        vc = "GrayScale";       break;
                case(StaticColor):      vc = "StaticColor";     break;
                case(PseudoColor):      vc = "PseudoColor";     break;
                case(TrueColor):        vc = "TrueColor";       break;
                case(DirectColor):      vc = "DirectColor";     break;
                default:                vc = "NonStandard";     break;
        }
        fputs(MkDef("CLASS", vc), tmpf);
        if (visual->class != StaticGray && visual->class != GrayScale) {
                fputs(MkDef("COLOR", "Yes"), tmpf);
        } else {
                fputs(MkDef("COLOR", "No"), tmpf);
        }
#ifdef XPM
	fputs(MkDef("XPM", "Yes"), tmpf);
#endif
        if (KeepTmpFile) {
                fprintf(stderr, "Left file: %s\n", tmp_name);
        } else {
                fprintf(tmpf, "syscmd(/bin/rm %s)\n", tmp_name);
        }
        fclose(tmpf);
        return(tmp_name);
}
#endif /* USEM4 */
