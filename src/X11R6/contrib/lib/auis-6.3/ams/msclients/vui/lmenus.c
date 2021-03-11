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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/lmenus.c,v 2.10 1992/12/15 21:23:32 rr2b R6tape $";
#endif

/* 
 *      C Lotus-style menus package
 *	Extracted from code by Mark Chance
 */


#include <andrewos.h>
#include <vui.h>
#include <ctype.h>
#include <panel.h>
#include <lmenus.h>
#include <keycodes.h>

extern unsigned char opt_row;
PRIVATE opt_cols[MAXOPTS] = { 0, 8, 16, 24, 32, 40, 48, 56, 64, 72 };
int current_option = -1;
MENU_TREE *current_menu_tree = (MENU_TREE *)NIL;

PRIVATE int MenuCursorHome(), MenuCursorLeft(), MenuCursorRight();
PRIVATE int MenuReturnKey(),  MenuEscapeKey(), MenuAnyKey();
PRIVATE int MenuRedrawScreen();

PRIVATE MKEYTAB MenuKeys[] = {
    KEYCODE_LEFT,	MenuCursorLeft,       /* Left              */
    KEYCODE_RIGHT,	MenuCursorRight,      /* Right             */
    KEYCODE_TAB,	MenuCursorRight,      /* Tab               */
    KEYCODE_SHIFT_TAB,  MenuCursorLeft,       /* Shift Tab         */
    KEYCODE_HOME,	MenuCursorHome,       /* Ctrl-home         */
    KEYCODE_RETURN,	MenuReturnKey,        /* Enter             */
    KEYCODE_FAKERETURN,	MenuReturnKey,        /* Enter             */
    KEYCODE_ESCAPE,	MenuEscapeKey,        /* Esc         */
    KEYCODE_REDRAW,	MenuRedrawScreen,     /* ^L */
    MATCHANY,		MenuAnyKey,
    0,			NULL
};

InitMenus(menu_opt)
MENU_OPTS *menu_opt;
{
    while (menu_opt->Option) {
	(menu_opt->prompt).plen=strlen( (menu_opt->prompt).pdata);
	menu_opt++;
    }
}

PRIVATE int MenuCursorHome(current_option, maxoption)
int current_option, maxoption;
{
    return(0);
}

PRIVATE int MenuCursorLeft(current_option, maxoption)
int current_option, maxoption;
{
    return((current_option>0)?current_option-1:maxoption);
}

PRIVATE int MenuCursorRight(current_option, maxoption)
int current_option, maxoption;
{
    return((current_option==maxoption)?0:current_option+1);
}

PRIVATE int MenuReturnKey(current_option, maxoption)
int current_option, maxoption;
{
    return(RETURN_KEY_HIT);
}

PRIVATE int MenuEscapeKey(current_option, maxoption)
int current_option, maxoption;
{
    return(ESCAPE_KEY_HIT);
}

PRIVATE int MenuRedrawScreen(current_option)
int current_option;
{
    extern FIELD *RedrawScreen();
    FIELD *boguscurfield = NULL;

    boguscurfield = RedrawScreen(boguscurfield);
    return(current_option);
}

PRIVATE MENU_OPTS *PANEL_BaseMenu;

PRIVATE int MenuAnyKey(current_option, maxoption, ch, menu_tree)
int current_option, maxoption, ch;
MENU_TREE menu_tree[];
{
    int i;

    ch = islower(ch) ? toupper(ch) : ch;
    for (i=0; i<=maxoption; i++) {
	current_option = ((current_option+1) % (maxoption+1));
	if (ch == *(PANEL_BaseMenu[menu_tree[current_option].this].Option)) break;
    }
    return(current_option);
}

SetBaseMenu(menu)
MENU_OPTS *menu;
{
    PANEL_BaseMenu=menu;
}

ShowMenu(menu_tree, current_option)
MENU_TREE menu_tree[];
int current_option;
{
    int i, rc = 0;
    char *opt_text, end_menu;
    ClearLine(opt_row);
    end_menu=FALSE;
    /*  ShowString("\0", opt_row, 0, opt_cols[0], RVIDEO); */
    for  (i=0; i<MAXOPTS; i++ ) {
	if (!end_menu) {
	    if (menu_tree[i].this >= 0) {
		opt_text=PANEL_BaseMenu[menu_tree[i].this].Option;
	    } else {
		rc=i-1;
		end_menu=TRUE;
		opt_text="\0";
	    }
	} else opt_text="\0";
	if (i==current_option) {
	    /* * ShowString("\0", opt_row, opt_cols[i], OPTWIDTH, RVIDEO);  ** */
	    ShowString(opt_text, opt_row, opt_cols[i], strlen(opt_text), RVIDEO);
	} else
	    ShowString(opt_text, opt_row, opt_cols[i], OPTWIDTH, HILITE);
    }
    ShowPrompt(&PANEL_BaseMenu[menu_tree[current_option].this].prompt);
    if (!end_menu) rc=i-1;  /* There were all 10 menu things. */
    return(rc);
}

EraseMenuCursor()
{
    char *opt_text;
    if (current_option==-1 || current_menu_tree==(MENU_TREE *)NIL) return;
    opt_text=PANEL_BaseMenu[current_menu_tree[current_option].this].Option;
    ShowString(opt_text, opt_row, opt_cols[current_option],
		strlen(opt_text), HILITE);
    UpdateScreen();
}

ShowMenuCursor()
{
    char *opt_text;
    if (current_option==-1 || current_menu_tree==(MENU_TREE *)NIL) return;
    opt_text=PANEL_BaseMenu[current_menu_tree[current_option].this].Option;
    ShowString(opt_text, opt_row, opt_cols[current_option],
		strlen(opt_text), RVIDEO);
    UpdateScreen();
}


MenuInput (menu_tree, keytable, last_opt_p)
MENU_TREE menu_tree[];
MKEYTAB *keytable;
int *last_opt_p;
{
    int skflag, maxopts, new_option = 0 ;
    register int i, ch;
    MKEYTAB *k;

    current_option = (last_opt_p)?*last_opt_p:0;
    current_menu_tree = menu_tree;
    maxopts=ShowMenu(menu_tree, current_option);

    while (TRUE) {
	ch = KeyIn();
	k = keytable;
	skflag = FALSE;

	for (i=0;;) {
	    if (k[i].scan_code == 0) {  /* Go through both key tables */
		if (!skflag) {
		    k = MenuKeys;   /* Finished user's so do ours */
		    skflag = TRUE;
		    i = 0;
		}
		else {
		    ErrorBeep ();
		    break;
		}
	    }

	    if ((ch == k[i].scan_code) || (k[i].scan_code == MATCHANY)) {
		ClearError ();
		new_option = (*(k[i].keyhandler))(current_option, maxopts, ch & 0x7F, menu_tree);
		if (new_option==REDRAW_KEY_HIT) {
		    ShowPrompt(&PANEL_BaseMenu[menu_tree[current_option].this].prompt);
		    maxopts=ShowMenu(menu_tree, current_option);
		    break;
		}
		if (new_option==ESCAPE_KEY_HIT) {
		    EraseMenuCursor();
		    return(ESCAPE_KEY_HIT);
		}
		if (new_option==RETURN_KEY_HIT) {
		    if (last_opt_p) *last_opt_p = current_option;
		    if ((MENU_TREE *)NIL==menu_tree[current_option].submenu) {
			EraseMenuCursor();
			return(menu_tree[current_option].this);
		    } else {  /* Process sub-menu */
			int new_last = 0;
			new_option=MenuInput(menu_tree[current_option].submenu, keytable, &new_last);
			if (new_option==ESCAPE_KEY_HIT) {
			    current_option = (last_opt_p)?*last_opt_p:0;
			    current_menu_tree = menu_tree;
			    maxopts=ShowMenu(menu_tree, current_option);
			    break;
			} else return(new_option);
		    }
		}
		if (new_option != current_option) {
		    EraseMenuCursor();
		    ShowPrompt(&PANEL_BaseMenu[menu_tree[new_option].this].prompt);
		    current_option=new_option;
		    ShowMenuCursor();
		}
		break;
	    }
	    else i++;
	}
    }
}

ClearPrompt()
{
    ClearLine(opt_row);
}
