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

/* $ACIS$ */

 


/*---------------------------------------------------------------------------*/
/*	MODULE: config.c						     */
/*		Contains all site-dependent filenames and paths to other     */
/*		modules and programs used within the help system.	     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/*				CONSTANT DEFINES			     */
/*---------------------------------------------------------------------------*/

/*
 * Entry in AndrewSetup indicating where the help index is to be found.
 * If that doesn't work, use the DEFAULT_ directory, relative to ANDREWDIR
 */
#define SETUP_INDEXDIR		"HelpIndexDir"
#define DEFAULT_INDEXDIR	"/lib/help.index"

/*
 * Entry in AndrewSetup indicating where to look for the panel setup files.
 * If that doesn't work, use the DEFAULT_ directory, relative to ANDREWDIR
 */
#define SETUP_LIBDIR		"HelpLib"
#define DEFAULT_LIBDIR		"/lib"

/*
 * Entry in AndrewSetup indicating where to look for help files.
 * If that doesn't work, use the DEFAULT_ directory, relative to ANDREWDIR
 */
#define SETUP_HELPDIR		"HelpDir"
#define	DEFAULT_HELPDIR		"/help"

/*
 * Entry in AndrewSetup indicating where to look for "change files".
 * If that doesn't work, use the DEFAULT_ directory, relative to LOCALDIR
 */
#define SETUP_CHANGEDIR 	"HelpChangeDir"
#define DEFAULT_CHANGEDIR	"/help"

/*
 * Entry in AndrewSetup indicating where to look for "tutorial files".
 * If that doesn't work, use the DEFAULT_ directory, relative to ANDREWDIR
 */
#define SETUP_TUTORIALDIR 	"HelpTutorialDir"
#define DEFAULT_TUTORIALDIR	"/help"

/*
 * Entry in AndrewSetup indicating where to look for "alias files".
 * If that doesn't work, use the DEFAULT_ directory, relative to ANDREWDIR
 */
#define	SETUP_ALIASDIR		"HelpAliasDir"
#define DEFAULT_ALIASDIR	"/help"

/*
 * Entry in AndrewSetup indicating where to look for "panels files".
 * If that doesn't work, use the DEFAULT_ directory, relative to LIBDIR (above)
 */
#define	SETUP_PANELSDIR		"HelpPanelsDir"
#define DEFAULT_PANELSDIR	"/help"

/*
 * Entry in AndrewSetup indicating where to store "missing files".
 * If that doesn't work, use the DEFAULT_ directory, relative to ANDREWDIR
 */
#define	SETUP_MISSINGDIR	"HelpMissingDir"
#define DEFAULT_MISSINGDIR	"/help"

/*
 * location of the alias file, relative to the Help ALIASDIR
 */
#define ALIASFILE "/help.aliases"

/*
 * location of the default overviews list file, relative to the Help PANELSDIR
 */
#define OVERVIEWFILE "/help.overviews"

/* 
 * the location of the default help topics list file, relative to the Help PANELSDIR
 */
#define PROGRAMFILE "/help.programs"

/* 
 * where to store the "missing file x" notices, relative to the Help MISSINGDIR
 */
#define MISSINGDIR 	"/HelpFlaws"

/*
 * array of possible subdirectories of MANDIR, ie man1, mann
 */
#define MANSUBS "12345678nolpx"

/* 
 * command exec'd when "send comment on help" is chosen 
 */
#define COMMENTCMD	"sendmessage"

/*
 * Entry in AndrewSetup indicating where to send the comment
 * If that doesn't work, use the DEFAULT_ entry
 */
#define SETUP_GRIPE_ADDRESS	"HelpSendCommentOnAddress"
#define DEFAULT_GRIPE_ADDRESS	"HelpComments+@andrew.cmu.edu"

/* 
 * command exec'd when a manpage is shown on a dumb terminal
 * 1st arg is the file, 2nd is the pager program 
 */
#define ROFFCMD		"nroff -man %s | %s"

/* 
 * command exec'd when an ATK document is shown on a dumb terminal
 * same args as above 
 */
#define ATKROFFCMD	"ezprint -t %s | nroff -man | %s"

/*
 * command exec'd when a troff file is printed. arg is the filename
 */
#ifdef CMU_ENV
#define ROFFPRINTCMD	"eqn -Tpostscript %s | troff -man -Tpostscript - | print -Tdvi"
#else
#define ROFFPRINTCMD	"eqn -Tdvi %s | troff -man -Tdvi - | print -Tdvi"
#endif /* CMU_ENV */

/*
 * command exec'd when an ATK file is printed. arg is the filename
 */
#define ATKPRINTCMD	"ezprint %s"

/*
 * NULL terminated list of relatively important filename extensions.
 * Files with these extensions will be shown before man pages, etc,
 * with the same base filename.
 */
#define FILE_EXTS {".help", ".concept", ".overview", NULL}

/*
 * NULL terminated lists of filename extensions that will be shown
 * in the overview and program panels if help is compiled outside
 * the CMU environment (CMU_ENV is not defined)
 */
#define OVERVIEW_EXTS {".overview", ".concept", ".glossary", NULL}
#define PROGRAM_EXTS {".help", ".intro", NULL}

/*
 * Filename extension used to indicate a "changes" file.  A menu
 * item to show the "changes" file foe that topic will show up if a
 *  file with the topic name followed by this extension exists in the CHANGEDIR
 */
#define CHANGE_EXT ".chng"

/*
 * Filename extnsion used to indicate a "tutorial" file.  Same
 * semantics as for CHANGE_EXT
 */
#define TUTORIAL_EXT ".tutr"

/* 
 * NULL terminated list of roff macro files to be used when processing
 * roff files (man pages, etc). If the file name doesn't start with '/' it
 * will be acessed relative to ANDREWDIR 
 */
#define MACFILES {TMACMANFILE, "lib/tmac/helpmac.an", NULL}

/*
 * environment variable that holds an alternate terminal pager program 
 */
#define PAGERVAR "PAGER"

/*
 * the default dumb-terminal pager program 
 */
#define DEFAULTPAGER "more -d"

/*
 * the default help file under a window manager when no files are specified
 */
#define WMDEFAULTFILE "tour"

/*
 * the default help file on a terminal when  no files are specified
 */
#define NONWMDEFAULTFILE "tour"

/*
 * maximum length of a help topic
 */
#define HNSIZE		100

/*
 * Maximum number of tutorial dirs allowed
 */
#define	MAX_TUTORIAL_DIRS   25

/*
 * how many items in the history panel
 */
#define HISTORYSIZE	25

/*
 * socket used for communication with existing intstance of help, if the
 * getservbyname call fails
 */
#define HELPSOCK	2016

/* 
 * horizontal %age given to the history and list panels 
 */
#define MAINPCT            25

/*
 * vertical %age given to the two listing panels
 */
#define PANELPCT	   50

/*
 * number of pixels given to labels in a panel
 */
#define LABPCT		   35


/*
 * Keybindings
 */

#define HELP_KEY_TOGGLE_PANELS		"\030p"		/* ^X-p	 */
#define HELP_KEY_TOGGLE_OVER		NULL		/*	 */
#define HELP_KEY_TOGGLE_PROGRAMS	"\030e"		/* ^X-e  */
#define HELP_KEY_TOGGLE_HIST 		"\030t"		/* ^X-t  */
#define HELP_KEY_SHOW_MORE		"\030\016"	/* ^X-^N */
#define HELP_KEY_SHOW_TUTORIAL		NULL		/*  	 */
#define HELP_KEY_SHOW_CHANGES		"\030x"		/* ^X-x  */
#define HELP_KEY_COPY_REGION		"\033w"		/* ESC-w */
#define HELP_KEY_HELP_ON_SELECTED	NULL		/*  	 */
#define HELP_KEY_HELP_ON		"\015"		/* ^M 	 */
#define HELP_KEY_HELP_ON_2		"\030\026"	/* ^X^V  */
#define HELP_KEY_SEND_COMMENT		"\030s"		/* ^X-s  */
#define HELP_KEY_SET_PRINTER		NULL		/*  	 */
#define HELP_KEY_PRINT			NULL		/* ^X-p  */
#define HELP_KEY_SEARCH			NULL		/* 	 */
#define HELP_KEY_RSEARCH		NULL		/* 	 */
#define HELP_KEY_SEARCH_AGAIN		NULL		/* 	 */
#define HELP_KEY_SEARCH_OVERVIEWS	NULL 		/*	 */
#define HELP_KEY_SEARCH_PROGRAMS	NULL 		/*	 */
#define HELP_KEY_NEXT_SCREEN		" "		/* space */
#define HELP_KEY_NEXT_SCREEN2		"v"		/* v 	 */
#define HELP_KEY_PREV_SCREEN		"b"		/* b	 */
#define HELP_KEY_ADD_SEARCH_DIR		NULL		/*	 */
#define HELP_KEY_DELETE_WINDOW		"\030\004"	/* ^X-^D */
#define HELP_KEY_WINDOW_SPLIT		"\0302"		/* ^X-2  */
#define HELP_KEY_ADD_BOOKMARK		"\030\002"	/* ^X^B	 */
#define	HELP_KEY_QUIT			"\030\003"	/* ^X^C  */
