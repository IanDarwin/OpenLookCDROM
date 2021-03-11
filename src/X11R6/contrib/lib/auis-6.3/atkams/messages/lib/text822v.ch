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


class t822view[text822v]: messages {
    overrides:
      PostKeyState(struct keystate *keystate);
      PostMenus(struct menulist *ml);
      DeleteApplicationLayer(struct view *scrollbar);
      SetDataObject(struct dataobject *do);
      ObservedChanged(struct text *changed, long value);
    methods:
      ShowHelp(char *text);
      SetCaptions(struct captions *cap);
      NewCaptionsInNewWindow() returns struct captions *;
      GetCaptions() returns struct captions *;
    classprocedures:
      InitializeClass() returns boolean;
      InitializeObject(struct textview *self) returns boolean;
      FinalizeObject(struct captions *self);
    data:
      struct captions *mycaps;
      struct keystate *myks;
      struct menulist *myml;
      boolean PriorReadOnliness;	/* for ESC-~ */
};
