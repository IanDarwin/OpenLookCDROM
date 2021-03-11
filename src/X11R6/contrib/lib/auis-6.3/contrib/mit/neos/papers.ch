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
#include "eos_structs.h"

#define DELETED 1

enum papers_DisplayType { papers_SIDE, papers_SIDESUBMIT, papers_ALTSIDE, papers_PLAIN, papers_SIDESUBDISP };
enum papers_Types { papersHandouts, papersGrading, papersExchange, papersNotKnown };
enum papers_Toggles { papers_OLD, papers_NEW };

class papers : view {
classprocedures:
  InitializeClass() returns boolean;
  InitializeObject(struct thisobject *self) returns boolean;
  FinalizeObject(struct thisobject *self);
overrides:
  LinkTree(struct view *parent);
  FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);
  Update();
  Hit(enum view_MouseAction action, long x, long y, long clicks) returns struct view *;
  PostMenus(struct menulist *menus);
  ReceiveInputFocus();
methods:
  ListHandouts() returns int;
  ListGrade() returns int;
  ListExchange() returns int;
  SetTitle(char *string);
  SetDisplay(boolean buttons, enum papers_Types windowtype);
  GradingListType(enum papers_Toggles type);
  SetDefault();
macromethods:
  SetParent(struct eos *parent) ((self)->daddy = parent)
  SetCourse(char *name) (strcpy((self)->course, name))
  SetGrading() ((self)->menuflags |= MENUS_instructor)
data:
  struct lpair *main, *screen;
  struct lpair *submitscreen, *display;
  struct lpair *buttons;
  struct lpair *altbuttons;
  struct lpair *xtrabuttons;
  struct newbuttonview *toggle;
  struct cursor *maincursor;
  struct text *textobj;
  struct scroll *scroll;
  struct textview *textv;
  struct label *title;
  struct labelview *titleV;
  struct menulist *menus;
  struct style *style;
  struct eos *daddy;
  int menuflags;
  long downdot;
  int markcount;
  char *course;
  enum papers_DisplayType wantbuttons;
  Paperlist_res *list;
  struct paperPositions *Positions;
  enum papers_Types thiswindow;
  enum papers_Toggles toggled;
  boolean IDoNotExist;
  struct lpair *subdispbuttons;
  int assignment;
  char *student;
};
