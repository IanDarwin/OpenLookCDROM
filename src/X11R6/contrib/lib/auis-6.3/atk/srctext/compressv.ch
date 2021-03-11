/* File compressv.ch created by R S Kemmetmueller
 
   compressv: a view to display a box where the hidden text lies. */
/* Copyright 1992, 1994 Carnegie Mellon University and IBM. All rights reserved.
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

class compressv: view {

  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct compressv *self) returns boolean;
    FinalizeObject(struct compressv *self);

  overrides:
    DesiredSize(long width,long height,enum view_DSpass pass,long *dWidth,long *dheight) returns enum view_DSattributes;
    FullUpdate(enum view_UpdateType type,long left,long top,long width,long right);
    GetOrigin(long width, long height, long *originX, long *originY);
    Hit(enum view_MouseAction action,long x,long y,long numberOfClicks) returns struct view *;
    LinkTree(struct view *parent);
    ObservedChanged(struct observable *changed,long value);
    Print(FILE *file,char *processor,char *finalFormat,boolean topLevel);
    ReceiveInputFocus();
    Update();

  methods:
    BoxText() returns char *;

  data:
    struct cursor *cursor;
    struct text *parenttext;
    struct textview *parentview;
};
