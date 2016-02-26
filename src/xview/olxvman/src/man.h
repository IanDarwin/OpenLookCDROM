/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: man.h,v 1.19 89/12/15 21:06:54 kit Exp $
 * $Athena: man.h,v 4.6 89/01/06 12:17:38 kit Exp $
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

#include "version.h"
#include "defs.h"

typedef void (*fcall)();	/* function pointer typedef */

typedef olxvman_OLXVMan_objects OLXVManObjs;
typedef olxvman_SearchPopup_objects SearchPopupObjs;
typedef olxvman_ManualPagePopup_objects ManualPageObjs;

typedef struct _ManPageWidgets {
  Xv_opaque manpage;		/* The manual page window (scrolled) */
  Xv_opaque directory;		/* The widget in which all directories will
				   appear. */
} ManPageWidgets;

/*
 * The manual sections and entries
 */

typedef struct tManual {
  char * blabel;		/* The button label. */
  char ** entries; 		/* The individual man page file names. */
  int nentries;			/* how many (TOTAL)*/
  int nalloc;			/* how much space allocated */
} Manual;
   
/* psuedo Globals that are specific to each manpage created. */

typedef struct _ManpageGlobals{
  int current_directory;	/* The directory currently being shown 
				   on this manpage. */
  Bool dir_shown,		/* True if the directory is then current
				   visable screen */
    both_shown;			/* If true then both the manpage and
				   the directory are to be shown.*/
  Xv_opaque label,		/* The label widget at the top of the page. */
    save,			/* The "would you like to save?" widget. */
    search,			/* The search widget popup. */
    help_button,		/* The help button. */
    option_menu,		/* The option menu. */
    text;			/* text widget containing search string. */

  char manpage_title[80];       /* The label to use for the current manpage. */

  char save_file[80];		/* the name of the file to save fomatted 
				   page into. */
  char tmpfile[80];		/* the name of the file to copy the formatted
				   page from. */
  Bool compress;		/* Compress file on save? */
  char ** section_name;		/* The name of each of the sections */

  ManPageWidgets manpagewidgets; /* The manpage widgets. */

  /* Things to remember when cleaning up when killing manpage. */

  Xv_opaque This_Manpage;	/* a pointer to the root of
				   this manpage. */

} ManpageGlobals;

/************************************************************
 *
 * Function Defintions 
 * 
 ************************************************************/

/*
 * This is easier than trying to find all calls to StrAlloc().
 */

#define StrAlloc(ptr) strdup(ptr)

/* Standard library function definitions. */

char * mktemp(), * getenv(), * malloc(), * realloc();
void exit();

char * CreateManpageName();

/* handler.c */

void DirectoryHandler(), PopUpMenu(), SaveCallback(), OptionCallback();
void Popup(),ManpageButtonPress(), GotoManpage(), DirPopupCallback();

/* Action Routines. */

void GotoPage(), PopupHelp(), PopupSearch(), Quit(), SaveFormattedPage();
void CreateNewManpage(), RemoveThisManpage(), Search(), ShowVersion();

/* help.c */

Bool MakeHelpWidget(), OpenHelpfile();

/* main.c */

void main();

/* man.c */

int Man();

/* misc.c */

void PrintError(),PrintWarning(), ChangeLabel(), OpenFile();
void RemovePixmaps(),PositionCenter(),AddCursor(),ParseEntry();
FILE *FindManualFile(),*Format(), *OpenEntryFile();
void PrintFormat ();
char *OlRealloc ();

/* pages.c */

Bool InitManpage();
void PrintManpage();
Bool Boldify();

/* search */

void MakeSearchWidget();
FILE * DoSearch();

/* tkfunctions.c */

int Width(), Height(), BorderWidth();
Xv_opaque PopupChild(), Child();
char * Name();
Bool MakeLong();
