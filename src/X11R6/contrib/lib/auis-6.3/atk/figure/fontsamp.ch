/* fontsamp.ch - font sample view */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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

  $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/fontsamp.ch,v 1.2 1992/12/14 20:44:19 rr2b R6tape $
*/

class fontsample [fontsamp] : view {

    classprocedures:
      InitializeClass() returns boolean;
      InitializeObject(struct fontsample *self) returns boolean;
      FinalizeObject(struct fontsample *self);

    overrides:
      FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);
      Update();
      Hit(enum view_MouseAction action, long x, long y, long n)	returns struct view *;
      ObservedChanged(struct fontsel *fontsel, long status);

    methods:
      SetString(char *val);
      GetFontDesc() returns struct fontdesc *;

    macromethods:
      GetString()  ((self)->teststring)

    data:
      char *teststring;
      boolean dirty;
      struct fontdesc *fdesc;
};
