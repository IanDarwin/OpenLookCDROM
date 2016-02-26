/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: defs.h,v 1.20 89/12/07 16:15:28 kit Exp $
 * $Athena: defs.h,v 4.8 89/01/06 15:56:19 kit Exp $
 *
 * Copyright 1987, 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:    Chris D. Peterson, MIT Project Athena
 * Created:   October 22, 1987
 */

#ifndef HELPFILE
#define HELPFILE "/usr/lib/X11/xman.help" /* name of the default helpfile. */ 
#endif

/* The default cursors */

#define XMAN_CURSOR "left_ptr"		/* Top level cursor. */
#define HELP_CURSOR "left_ptr"	        /* The help cursor. */
#define MANPAGE_CURSOR "left_ptr"	/* The manpage cursor. */
#define SEARCH_ENTRY_CURSOR "question_arrow"	/* The search text widget
						   cursor. */
#define DIRECTORY_NORMAL "fixed" /* The default dir font */

#define OPTION_MENU "optionMenu" /* Name of the Option Menu. */
#define SECTION_MENU "sectionMenu" /* Name of the Section Menu. */

#define HELP_BUTTON "helpButton" /* Name of top help button */
#define QUIT_BUTTON "quitButton" /* Name of top quit button */
#define MANPAGE_BUTTON "manpageButton" /* Name of top manpage button */

#define TOPBOXNAME  "topBox"	/* Name of the Top Box. */
#define MANNAME "manualBrowser"	/* name for each manual page widget. */
#define SEARCHNAME "search" /* The name for the search widget. */
#define HELPNAME  "help"	/* The name of the help widget. */
#define DIRECTORY_NAME "directory" /* name of the directory widget. */
#define MANUALPAGE "manualPage"	/* name of the Scrollbyline widget that
				 contains the man page. */
#define DIALOG         "dialog"

/* Names of the menu buttons */

#define NUM_OPTIONS 9		/* Number of menu options. */

#define DIRECTORY      "displayDirectory"
#define MANPAGE        "displayManualPage"
#define HELP           "help"
#define SEARCH         "search"
#define BOTH_SCREENS   "showBothScreens"
#define REMOVE_MANPAGE "removeThisManpage"
#define OPEN_MANPAGE   "openNewManpage"
#define SHOW_VERSION   "showVersion"
#define QUIT           "quit"

/* definitions of string to use for show both and show one. */

#define SHOW_BOTH "Show Both Screens"
#define SHOW_ONE "Show One Screen"

/* 
 * Things will not look right if you change these names to make 
 * MANUALSEARCH longer APROPOSSEARCH, see search.c for details.
 */

#define MANUALSEARCH "manualPage"
#define APROPOSSEARCH "apropos"
#define CANCEL "cancel"

#define MANUAL 0
#define APROPOS 1

#define NO_SECTION_DEFAULTS ("no default sections")

/*
 * The command filters for the manual and apropos searches.
 */

#if ( defined(hpux) || defined(macII) || defined(CRAY) )
#  define NO_MANPATH_SUPPORT
#endif

#ifdef NO_MANPATH_SUPPORT
#  define APROPOS_FORMAT ("man -k %s | pr -h Apropos >> %s")
#else
#  define APROPOS_FORMAT ("man -M %s -k %s | pr -h Apropos > %s")
#endif

#if defined( ultrix )
#define FORMAT "| nroff -man"             /* The format command. */
#else
#define FORMAT "| neqn | nroff -man"      /* The format command. */
#define PFORMAT "| eqn | troff -man -t | lpr -t"
#endif

#define TBL "tbl"

#define DEFAULT_WIDTH 500	/* The default width of xman. */
#define SECTALLOC  8		/* The number of entries allocated
				   at a time for the manual structures. */
#define ENTRYALLOC 100		/* The number of entries allocated
				   at a time for a section. */

#define INITIAL_DIR 0		/* The Initial Directory displayed. */

#define	NUMMANPAGES	5	/* Number of manual page displays. */
/*
 * Names of the man and cat dirs. 
 */

#define MAN "man"

#if ( defined(macII) || defined(CRAY) )
/*
 * The Apple and Cray folks put the preformatted pages in the "man" 
 * directories, what a bunch of BOZOs. 
 */
#define CATC MAN		
#else
#define CATC "cat"
#endif

/*
 * The directories to search we are making the assumption that the manual
 * directories are more complete that the cat directories, but you can
 * change it if you like. 
 */

#if ( defined(UTEK) || defined(apollo) )
#  define SEARCHDIR  CATC
#  define LSEARCHDIR LCAT
#else
#  define SEARCHDIR  MAN
#  define LSEARCHDIR LMAN
#endif

/*
 * I set umask so that when the user copies a file into the main
 * manual page tree s/he will not protect it so that other users cannot
 * see it.
 */

#define COPY "umask 0; cp"		/* The unix copy command.  */

#define MANDESC "mandesc"	/* Name of the mandesc files.  */

/*
 * The default manual page directory. 
 *
 * The MANPATH enviornment variable will override this.
 */

#ifdef macII
#  define MANDIR "/usr/catman/u_man:/usr/catman/a_man"	
#else
#  define MANDIR "/usr/man"
#endif

#define INDENT 15
#define TYP20STR "MMMMMMMMMMMMMMMMMMMM"

#define FILE_SAVE "yes"
#define CANCEL_FILE_SAVE "no"
#define MANTEMP "/tmp/xmanXXXXXX"

/*
 * Compression Definitions.
 */

#if defined( macII )
#  define COMPRESSION_EXTENSION   "z"
#  define UNCOMPRESS_FORMAT       "pcat %s > %s"
#  define NO_COMPRESS		/* mac can't handle using pack as a filter and
				   xman needs it to be done that way. */
#else 
#  if defined ( UTEK )
#    define COMPRESSION_EXTENSION "C"
#    define UNCOMPRESS_FORMAT     "ccat < %s > %s"
#    define COMPRESS              "compact"
#  else
#    define COMPRESSION_EXTENSION "Z"
#    define UNCOMPRESS_FORMAT     "zcat < %s > %s"
#    define COMPRESS              "compress"
#  endif /* UTEK */
#endif /* macII */


/*
 * Macro Definitions.
 */

#define streq(a, b)        ( strcmp((a), (b)) == 0 )

/* 
 * Function definitions moved to man.h
 */
