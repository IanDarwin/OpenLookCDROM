/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

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


/* Changed June 1990 njw */
/* A simple string input view, with a fenced prompt */

class strinput : view {
classprocedures:
  InitializeObject(struct thisobject *self) returns boolean;
  FinalizeObject(struct thisobject *self);
  InitializeClass() returns boolean;
overrides:
  FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);
  Hit(enum view_mouseAction action, long x, long y, long clicks) returns struct view *;
  ReceiveInputFocus();
  LinkTree(struct view *parent);
methods:
  GetInput(int length) returns char *;
  SetInput(char *string);
  ClearInput();
  SetPrompt(char *string);
  HaveYouGotTheFocus() returns boolean;
macromethods:
  IsLinked() ((self)->header.view.parent && (self)->header.view.imPtr)
data:
  struct text *textobj;
  struct textview *textv;
  char *prompt;
};
