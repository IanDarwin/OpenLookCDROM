/* $Author: rr2b $ */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *  For full copyright information see:'mit-copyright.h'     *
 *************************************************************/

#include <mit-copyright.h>

struct readWindow {
    struct buffer *data;
    struct im *im;
    struct frame *fr;
};

union windows {
    struct papers *paperswin;
    struct turnin *turninwin;
    struct pickup *pickupwin;
};

struct windowlist {
    struct windowlist *next;
    union windows *window;
    char *course;
};

class eos : view {
classprocedures:
  InitializeObject() returns boolean;
  FinalizeObject();
  InitializeClass() returns boolean;
overrides:
  LinkTree(struct view *parent);
  FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);
  Update();
  Hit(enum view_MouseAction action, long x, long y, long clicks) returns struct view *;
  PostMenus(struct menulist *menulist);
  PostKeyState(struct keystate *ks);
  ReceiveInputFocus(); 
methods:
  SetBuffer(char *filename, long flags);
  NameBuffer() returns char *;
  SetTitle(char *string);
  SetCourse(char *name);
  SetFontDisplay();
  SetFontDefault();
macromethods:
  SetProgram(char *name) (strcpy((self)->program, name))
data:
  struct lpair         *head, *screen;
  struct buffer        *editregion;
  struct frame         *frame;
  struct label         *title;
  struct labelview     *titleV;
  struct lpair         *buttons;
  struct windowlist    *handouts;
  struct windowlist    *grades;
  struct windowlist    *pickups;
  struct windowlist    *exchanges;
  struct windowlist    *turnins;
  struct help          *helpwindow;
  struct menulist      *menus;
  struct keystate      *keys;
  char *course;
  char *program;
  int dialogpri;
  int menuflags;
  boolean gradingflag;
  struct atom *paperatom;   /* used by grade for naming the 'paper' property of a buffer's data object */
  struct readWindow *displaywindow;
};

