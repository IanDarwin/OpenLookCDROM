/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

/* $ACIS */

 

/*---------------------------------------------------------------------------*/
/*	MODULE: helpsys.h						     */
/*		All structures and constants for the help system.	     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/*				VERSION NUMBER				     */
/*---------------------------------------------------------------------------*/

#define MAJOR_VERSION	7
#define MINOR_VERSION	2

/*---------------------------------------------------------------------------*/
/*				USEFUL CONSTANTS			     */
/*---------------------------------------------------------------------------*/

/* 
 * menu mask bits 
 */
#define MENU_Always		0      /* show this item always */
#define MENU_Never		(1<<31)	/* never show this item  */

#define MENU_ToggleOverHide	(1<<0) /* show Hide Overviews? */
#define MENU_ToggleOverShow	(1<<1) /* show Show Overviews? */

#define MENU_ToggleListHide	(1<<2) /* show Hide Programs List*/
#define MENU_ToggleListShow	(1<<3) /* show Show Programs List */

#define	MENU_ToggleHistHide	(1<<4) /* show Hide History Panel? */
#define	MENU_ToggleHistShow	(1<<5) /* show Show History Panel? */

#define	MENU_TogglePanelHide	(1<<6) /* show Hide Help Panels? */
#define	MENU_TogglePanelShow	(1<<7) /* show Show help Panels? */

#define	MENU_ToggleSizeShrink	(1<<8) /* show Shrink Program List? */
#define	MENU_ToggleSizeExpand	(1<<9) /* show Expand Program List? */

#define	MENU_ToggleFilterShow	(1<<10) /* show Filter Panel? */
#define MENU_ToggleReFilterShow	(1<<11)	/* show ReFilter Panel? */
#define	MENU_ToggleRestoreShow	(1<<12) /* show Restore Panel? */

#define	MENU_SwitchMoreMenu	(1<<13) /* show Show More Doc? */
#define	MENU_SwitchTutorialMenu	(1<<14) /* show Show Tutorial? */
#define	MENU_SwitchChangesMenu	(1<<15) /* show Show Changes? */
#define MENU_SwitchCopy		(1<<16) /* show Copy? */
#define	MENU_SwitchDeleteMenu	(1<<17)	/* show Delete This Window? */

/*
 * small macro to show an error message with a 'continue' button
 */

/* space for the default dialog box answer */
static long answer;

/* default choice for the dialog box */
static char *defchoices[] =
	{"Continue", (char *)NULL};

#define ERRORBOX(vw, msg) (message_MultipleChoiceQuestion(vw, 0, msg, 0,\
							 &answer, defchoices, NULL))

/* defines for GetHelpOn */
#define help_NEW	TRUE	/* is this the first time through for this entry */
#define help_OLD	FALSE	/* is the continuing to get help on a topic */

#define help_HIST_NOADD	0	/* don't add to the history list */
#define help_HIST_NAME	1	/* add the exact help topic name to history */
#define help_HIST_TAIL	2	/* add the tail of the help file name to history */

#define help_USE_OLD_TITLE	3 /* use last state of (more) */

#define help_ON		1<<0	/* menurock for Help On... */
#define help_SEL	1<<1	/* menurock for Help On Selected */
#define help_NEW_WIN	1<<3	/* menurock &'d in for New Window Help On */

/* some menu and key rock defines */
#define help_HIDE_PANEL		0
#define help_SHOW_PANEL 	1

#define help_SHRINK		0
#define help_EXPAND		1

#define help_SHOW_OVER		(1<<0)
#define help_HIDE_OVER		(1<<1)

#define help_SHOW_LIST		(1<<2)
#define help_HIDE_LIST		(1<<3)

#define help_HIDE_HIST		(1<<4)
#define help_SHOW_HIST		(1<<5)

#define help_IGNORE_TOGGLE	0
#define help_ALWAYS_TOGGLE	(1<<15)

/* textview procedure rocks */
#define help_SEARCH		0
#define help_RSEARCH		1
#define help_SEARCH_AGAIN	2
#define help_PAGE_DOWN		3
#define help_PAGE_UP		4
#define	help_COPY_REGION	5
#define	help_SET_PRINTER	6

/* history and bookmark constants */
#define help_HE_HISTORY		1
#define help_HE_BOOKMARK	2

/* filter options */
#define	help_FILTER_FILTER	1 /* filtering first time */
#define	help_FILTER_REFILTER	2 /* refiltering panel */
#define	help_FILTER_RESTORE	3 /* restoring panel */

#define help_MAXPANEL		5000 /* maximum size of the list panel */


/*---------------------------------------------------------------------------*/
/*			     PROGRAM INTERNALS				     */
/*---------------------------------------------------------------------------*/

#define HELP_MAX_ERR_LENGTH	127 /* maximum error message length */

/* 
 * useful structures 
 */

struct cache {			/* state holder */
    char name[HNSIZE];		/* original topic requested */
    long flags;			/* menu flags and state */

    struct view *view;		/* view on help file */
    struct scroll *scroll;	/* scrollbar for help file */
    struct dataobject *data;	/* data (text) for help file */
    struct helpFile *all;	/* current list of help files */
    struct helpFile *cur;	/* pointer into list of all files */
    struct menulist *menus;	/* menu structure */

    int histat;			/* history status */
    char histent[HNSIZE];	/* entry to be made in history */
    struct panel_Entry *lastHist; /* the last hit history item */
    struct panel *hist;		/* pointer to this window history panel */
    
};

struct helpDir {
    struct helpDir *next;
    char *dirName;
};

struct helpFile {
    struct helpFile *next;      /* next pointer */
    char *fileName;             /* the file name before the final dot */
    int extension;		/* the file's numeric (!) extension,
				   e.g. 1 for foo.1v */
    char *extPtr;               /* pointer into fileName to extension, if any*/
    int metric;			/* smaller the metric, the earlier we show
				   the file */
};

struct helpAlias {
    struct helpAlias *next;
    char *alias;
    char *original;
};

struct programItem {
    struct programItem *next;
    char *name;
};


struct history_entry {
    char *fname;
    long pos;
    long top;
    long sellen;
};

struct self_help {
    struct self_help *next;
    struct help *this;
};

    
