/* Copyright 1992 by Carnegie Mellon University All rights Reserved.
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
* $ */

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/phelpv.ch,v 1.3 1992/12/14 20:51:24 rr2b R6tape $ */

class phelpv : textview [textv] {
classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct prefval *self) returns boolean;
    FinalizeObject(struct prefval *self);
overrides:
    SetDotPosition(long pos);
    Hit(enum view_MouseAction action, long x, long y, long clicks) returns struct view *;
methods:
macros:
macromethods:
    SetPrefs(struct prefs *p) ((self)->prefs=(p))
    GetPrefs() ((self)->prefs)
data:
    struct prefs *prefs;
};
