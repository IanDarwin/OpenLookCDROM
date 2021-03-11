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


 

#define WHATIAM_UNDEFINED -1
#define WHATIAM_FOLDERS 0
#define WHATIAM_CAPTIONS 1
#define WHATIAM_BODIES 3

class messages: textview[textv] {
    classprocedures:
      InitializeObject(struct messwind *self) returns boolean;
      InitializeClass() returns boolean;
      FinalizeObject(struct messwind *self);
    overrides:
      PostKeyState(struct keystate *keystate);
      PostMenus(struct menulist *ml);
      ObservedChanged(struct observable *changed, long change);
    methods:
      SetWhatIAm(int WhatIAm);
      ResetFileIntoMenus();
      AppendOneMessageToFile(int cuid, char *fname, int DoRaw) returns int;
    data:
      struct menulist *mymenulist, *mypermmenulist, *fileintomenulist, *expandedmenuslist;
      struct keystate *mykeys, *mypermkeys;
      int WhatIAm;
      struct foldertreev *folderTree;
      struct frame *folderFrame;
};

