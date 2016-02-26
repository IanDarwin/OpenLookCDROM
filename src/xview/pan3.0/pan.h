/*
Post A Note V3.0
Copyright (c) 1993, Jeffrey W. Bailey
All rights reserved.

Permission is granted to distribute this program in exact, complete
source form, which includes this copyright notice, as long as no fee
other than media and distribution cost is charged.

This program may not be used in whole, or in part, in any other manner
without prior written permission from the author.

This program may not be distributed in modified form without prior
written permission from the author.  In other words, patches may be
distributed, but modified source may not be distributed.

If there are any questions, comments or suggestions, the author may be
contacted at:

    jeff@rd1.interlan.com

    or

    Jeffrey Bailey
    Racal-Datacom, Inc.
    Mail Stop E-110
    1601 N. Harrison Parkway
    Sunrise, FL  33323-2899
*/

#ifndef  PAN

#include <stdio.h>

#ifdef HAS_STDLIB
#include <stdlib.h>
#else
extern char *malloc();
extern char *getenv();
#endif /* HAS_STDLIB */

#include <sys/types.h>

#ifndef BSD
#include <time.h>
#include <dirent.h>
#else
#include <sys/dir.h>
#define dirent direct
#endif /* BSD */

#include <sys/file.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/openmenu.h>
#include <xview/textsw.h>
#include <xview/svrimage.h>
#include <xview/icon.h>
#include <xview/notice.h>
#include <xview/defaults.h>
#include <xview/seln.h>

#define  PAN

/* parser stuff */
struct ps_component
    {
    struct ps_component *ps_prev; /* Pointer to previous node */
    struct ps_component *ps_next; /* Pointer to next node */
    char *ps_text;                /* Pointer to token text */
    char ps_start_delim;          /* Character delimiter for start of token */
    char ps_end_delim;            /* Character delimiter for end of token */
    };
extern struct ps_component *parse_string();


/* Linked List stuff */

struct LLM_node   {
                  struct LLM_node *Next;
                  struct LLM_node *Prev;
                  };

struct LLM_root   {
                  int              Init;
                  struct LLM_node *First;
                  struct LLM_node *Last;
                  struct LLM_node *Current;
                  unsigned int     Size;
                  };

typedef struct LLM_root LLM_root;

extern char *LLM_add();
extern char *LLM_first();
extern char *LLM_insert();
extern char *LLM_last();
extern char *LLM_next();
extern char *LLM_previous();
extern char *LLM_current();

/* Pan stuff */
struct resource {
    char *a;
    char *b;
};

#ifdef public
struct resource resources [] = {
    {"pan.initialDirectory",	"pan.initialdirectory"},
    {"pan.noticeBeep",		"pan.noticebeep"},
    {"pan.confirmDestroy",	"pan.confirmdestroy"},
    {"pan.printCommand",	"pan.printcommand"},
    {"pan.iconTransparent",	"pan.icontransparent"},
    {"pan.folderOrder",	        "pan.folderorder"},
    {"pan.defaultSearch",	"pan.defaultsearch"},
    {"pan.folderInTitle",	"pan.folderintitle"},
    {"pan.textFont",		"pan.textfont"},
    {"pan.windowMax",		"pan.windowmax"},
    {"pan.actionDefault",	"pan.actiondefault"},
    {"pan.menuTextLength",	"pan.menutextlength"},
    {"pan.setWidth",		"pan.setwidth"},
    {"pan.logging",		"pan.logging"},
    {"pan.topMargin",		"pan.topmargin"},
    {"pan.bottomMargin",	"pan.bottommargin"},
    {"pan.leftMargin",		"pan.leftmargin"},
    {"pan.rightMargin",		"pan.rightmargin"},
    {"pan.checkInterval",	"pan.checkinterval"},
    {"pan.searchMenu",		"pan.searchmenu"},
    {"pan.noteWidth",		"pan.notewidth"},
    {"pan.noteHeight",		"pan.noteheight"}
};

unsigned short mainicon [] = {
#include "panmain.icon"
};

unsigned short mainiconmask [] = {
#include "panmain.mask.icon"
};

unsigned short myicon [] = {
#include "pan.icon"
};

unsigned short myiconmask [] = {
#include "pan.mask.icon"
};

#else /* ifdef public */

extern struct resource resources [];
extern unsigned short mainicon [];
extern unsigned short mainiconmask [];
extern unsigned short myicon [];
extern unsigned short myiconmask [];

#endif /* ifdef public */

/* Index number into resources array */
#define RES_IDIR          0
#define RES_NBEEP         1
#define RES_CDESTROY      2
#define RES_PCOMMAND      3
#define RES_ITRANSPARENT  4
#define RES_FORDER        5
#define RES_DSEARCH       6
#define RES_FINTITLE      7
#define RES_TEXTFONT      8
#define RES_WINDOWMAX     9
#define RES_ACTIONDEF     10
#define RES_MENUTEXTLEN   11
#define RES_SETWIDTH      12
#define RES_LOGGING       13
#define RES_TOPMARGIN     14
#define RES_BOTTOMMARGIN  15
#define RES_LEFTMARGIN    16
#define RES_RIGHTMARGIN   17
#define RES_CHECKINTERVAL 18
#define RES_SEARCHMENU    19
#define RES_NOTEWIDTH     20
#define RES_NOTEHEIGHT    21

/* Resource default values (IDIR default is built dynamically) */
#define RESDEF_NBEEP		TRUE
#define RESDEF_CDESTROY		FALSE
#define RESDEF_PCOMMAND		"/usr/ucb/lpr $FILE"
#define RESDEF_ITRANSPARENT	FALSE
#define RESDEF_FORDER      	"Miscellaneous"
#define RESDEF_DSEARCH      	""
#define RESDEF_FINTITLE     	FALSE
#define RESDEF_TEXTFONT     	NULL
#define RESDEF_WINDOWMAX     	20
#define RESDEF_ACTIONDEF    	CHOICE_FOLDER
#define RESDEF_MENUTEXTLEN	20
#define RESDEF_SETWIDTH		80
#define RESDEF_LOGGING		TRUE
#define RESDEF_TOPMARGIN	25
#define RESDEF_BOTTOMMARGIN	5
#define RESDEF_LEFTMARGIN	5
#define RESDEF_RIGHTMARGIN	5
#define RESDEF_CHECKINTERVAL	60
#define RESDEF_SEARCHMENU	"."
#define RESDEF_NOTEWIDTH	-1
#define RESDEF_NOTEHEIGHT	-1

#ifndef public
#define public extern
#endif

#define NOTEDIR ".pan"
#define NOTENAM "Note_%d_%d"
#define CTRLNAM "PanCtrl"
#define CTRLLCK "PanCtrl.lock"
#define ERRLNAM "PanCtrl.log"
#define PIDNAM  "PID"

#define DEFWIDTH  260
#define DEFHEIGHT 200
#define DEFSPACING 30
#define DEFPANELSPACING 10

/*
    Most of the following size parameters have become estimates since I
    implemented the new window creation algorithm
*/

#define MAINWIDTH  200
#define MAINHEIGHT 90

#define FLDRWIDTH  350
#define FLDRHEIGHT 100
#define FLDRSPACING 30

#define DESTROYWIDTH  220
#define DESTROYHEIGHT 280
#define DESTROYSPACING 30

#define SEARCHWIDTH  230
#define SEARCHHEIGHT 230
#define SEARCHSPACING 30

#define MOVEWIDTH  600
#define MOVEHEIGHT 250
#define MOVESPACING 30

/* Empirically determined scrollbar width.  How can you really find out? */
#define SCROLLWIDTH 35

#define FORCE   1
#define NOFORCE 0

#define NOTITLE "Untitled"

#define MAXTITLELEN     100
#define MAXTITLEDISLEN  8
#define MAXSEARCHLEN    MAXTITLELEN
#define MAXSEARCHDISLEN 12
#define MAXBUFLEN       1024

#define MAXSUBDIR      20  /* Max. len of sub dir name */
#define MAXNAME        256 /* Max. note file name len */

#define COL_ROLLOVER 10
#define COL_MAXCOL    6

#define CHOICE_ALL       "All"
#define CHOICE_FROM_LIST "From List"

#define CHOICE_QUIT    "Quit"
#define CHOICE_MOVE    "Move Note..."
#define CHOICE_FOLDER  "Create Folder..."
#define CHOICE_DESTROY "Destroy Folder..."
#define CHOICE_PRINT   "Print Note"

#define ERRONDISPLAY 1
#define ERRINLOG     0

public Frame  main_frame;
public Panel  main_panel;
public Menu   main_newnote;
public Menu   main_expose;
public Menu   main_print;

public Frame      folder_frame;
public Panel_item folder_item;
public Menu_item  folder_mitem;

public Frame      move_frame;
public Panel      move_panel;
public Menu_item  move_mitem;

public Frame      destroy_frame;
public Panel_item destroy_item;
public Menu_item  destroy_mitem;

public Frame      search_frame;
public Panel_item search_item;
public Panel_item search_button;

public int move_up, destroy_up, search_up;

enum NoteState {
    Visible = 0,
    Hidden  = 1,
    Veiled  = 2
};
typedef enum NoteState NoteState;

struct Note {
    Rect       rect;
    NoteState  state;
    char       ntitle[MAXTITLELEN + 1];
    int        mapped;
    Frame      frame;
    Panel      panel;
    Textsw     textsw;
    Panel_item title;
    Panel_item hide;
    Panel_item action;
    Menu       actionmenu;
    Panel_item cdate;
    Panel_item ctime;
    Icon       icon;
#ifdef PAN_DND
    Xv_opaque  drag_obj;
    Xv_opaque  drag_tgt;
    int        got_itms;
    Xv_opaque  sel_itm1;
    Xv_opaque  sel_itm2;
    Xv_opaque  sel_itm3;
#endif
    long       crttime;
    long       touched;
    char       basename[MAXNAME];
    struct SubDir *sp;
};

struct SubDir {
    char           subdir[MAXSUBDIR];
   struct LLM_root note_rt;
};

struct FreeWin {
    Frame      frame;
    Panel      panel;
    Textsw     textsw;
    Panel_item title;
    Panel_item hide;
    Panel_item action;
    Menu       actionmenu;
    Panel_item cdate;
    Panel_item ctime;
    Icon       icon;
#ifdef PAN_DND
    Xv_opaque  drag_obj;
    Xv_opaque  drag_tgt;
    int        got_itms;
    Xv_opaque  sel_itm1;
    Xv_opaque  sel_itm2;
    Xv_opaque  sel_itm3;
#endif
};

public int  confirmdestroy;
public int  noticenobeep;
public int  icontransparent;
public char folderorder[2048];
public int  fintitle;
public char textfont[2048];
public int  windowmax;
public char actiondef[2048];
public int  menutextlen;
public int  setwidth;
public char widthtext[20];
public int  logging;
public int  topmargin;
public int  bottommargin;
public int  leftmargin;
public int  rightmargin;
public int  checkinterval;
public char searchmenu[2048];
public int  notewidth;
public int  noteheight;

public int  notecount;

public int  debug_on;

public char note_dir [MAXBUFLEN];
public char myname [MAXBUFLEN];

extern int  cleanup(), hideall(), exposeall(), hidenote(), noteaction();
extern int  dismissed(), frameexit(), dragdrop(), reseticon(), newtitle();
extern int  newnote(), exposemenu(), actionmenu(), newfolder(), folderdone();
extern int  notesearch(), movenote(), destroyfolder(), createfolder();
extern int  search_menu_proc();
extern Menu gen_exposemenu(), gen_prtmenu();
extern Notify_value child_death();
extern Notify_value check_ctl_file();
extern char *add_sorted();

#ifdef PAN_DND
extern drag_proc();
#endif

public struct LLM_root subdir_rt;
public struct LLM_root freewin_rt;

#endif /* PAN */
