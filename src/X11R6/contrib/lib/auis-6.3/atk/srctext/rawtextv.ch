/* File rawtextv.ch created by R S Kemmetmueller

   rawtextv.ch: Text View subclass with overstrike mode.
*/

/* Copyright 1988,1994 Carnegie Mellon University and IBM. All rights reserved.
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
class rawtextview[rawtextv]: textview[textv] {
 
  overrides:
    PostMenus(struct menulist *menulist);
    ReceiveInputFocus();

  methods:

  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct srctextview *self) returns boolean;
    FinalizeObject(struct srctextview *self);

  macromethods:

  data:
    struct keystate *raw_state;
    struct menulist *raw_menus;    
};
