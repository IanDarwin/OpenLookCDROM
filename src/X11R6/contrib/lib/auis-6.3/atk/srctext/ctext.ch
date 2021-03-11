/* File ctext.ch created by R L Quinn
 
   ctext.ch: Text subclass specialized for dealing with C code. */
/* Copyright 1988, 1994 Carnegie Mellon University and IBM. All rights reserved.
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
/* kinds of styles that are used in hash table */
#define KEYWRD 1     /* keywords */
#define PREPRC 2     /* preprocessor directives */

class ctext: srctext {

  overrides:
    Indentation(long pos) returns int;
    IsTokenChar(char ch) returns boolean;
    Quoted(long pos) returns boolean;
    RedoStyles();
    SetAttributes(struct attributes *atts);
    SetupStyles();

  methods:
    BackwardSkipDelimited(long pos,char begin,char end) returns long;
    BackwardSkipJunk(long pos) returns long;
    CheckPreproc(long start) returns long;
    MaybeBackwardSkipCppLines(long pos) returns long;
    UnstyledCommentStart(long pos) returns long;

  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct ctext *self) returns boolean;
 
  macromethods:

  data:
    /* Variables used to control the indenting style. */
    int braceIndent, switchLabelUndent, switchLevelIndent;
    boolean outdentPreproc;
};
