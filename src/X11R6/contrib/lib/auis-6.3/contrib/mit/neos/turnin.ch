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

class turnin : view {
overrides:
  Hit(enum view_MouseAction action, long x, long y, long clicks) returns struct view *;
  PostKeyState(struct keystate *ks);
  PostMenus(struct menulist *ml);
  LinkTree(struct view *v);
  FullUpdate(enum view_UpdateType type, long t, long l, long h, long w);
  ReceiveInputFocus();
classprocedures:
  InitializeClass() returns boolean;
  InitializeObject(struct thisobject *self) returns boolean;
  FinalizeObject(struct thisobject *self);
methods:
  SetTitle(char *string);
  GoForIt();
macromethods:
  SetParent(struct eos *dad) ((self)->daddy = dad)
  SetCourse(char *name) (strcpy((self)->course, name))
data:
  struct label *title;
  struct labelview *titleV;
  struct lpair *topscreen;
  struct lpair *userarea;
  struct lpair *questions;
  struct lpair *buttons;
  struct lpair *whole;
  struct value *onoff;
  struct onoffV *onoffv;
  struct pushbutton *okb;
  struct pushbutton *cancelb;
  struct newbuttonview *okv;
  struct newbuttonview *cancelv;
  struct strinput *name;
  struct strinput *number;
  struct eos *daddy;
  struct keystate *kstate;
  struct menulist *menus;
  char *course;
  boolean turninfromfile;
  boolean IDoNotExist;
  boolean ResourcesPosted;
};
