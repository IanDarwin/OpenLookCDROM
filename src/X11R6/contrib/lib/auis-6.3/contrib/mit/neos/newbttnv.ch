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
 *   For full copyright information see:'mit-copyright.h'     *
 *  Adapted from ATK pushbuttonview by Nick Williams, July 1989
 *************************************************************/

#include <mit-copyright.h>

class newbuttonview[newbttnv]: view {
  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct newbuttonview *self) returns boolean;
    FinalizeObject(struct newbuttonview *self);
  overrides:	
    DesiredSize(long width, long height, enum view_DSpass pass, long * desired_width, long * desired_height) returns enum view_DSattributes;
    GetOrigin(long width, long height, long * originX, long * originY);
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);
    Update();
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns   struct view *;
  methods:
    Enable(boolean want_to_enable);
    IsEnabled() returns boolean;
  data:
    short lit;
    short depth;
    char *bfont;
    int bfontsize;
    struct cursor *cursor;
    boolean enabled;
};

