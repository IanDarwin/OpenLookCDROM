/* File cpptext.ch created by R L Quinn
 
   cpptext.ch: Text subclass specialized for dealing with C++ code.  */

/* Copyright 1990, 1994 Carnegie Mellon University and IBM. All rights reserved.
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
/*ctext KEYWRD 1     /* keywords */
/*ctext PREPRC 2     /* preprocessor directives */
#define CLASS  3     /* class definition */

/* bit values used for indentation identification */
#define	CLASS_BIT	1   /* bit 0 */
#define	ACSCTRL_BIT	2   /* bit 1 */
#define TEMPLATE_BIT	4   /* bit 2 */
/*note: CLASS_BIT keywords do NOT get wrapped with kindStyle[CLASS], and ACSCTRL_BIT isn't used anyway, because ctext and cpptext still use the old match() method*/

class cpptext: ctext {

  overrides:
    BackwardCheckLabel(long pos);
    BreakLine(struct mark *endofline);
    CheckWord(long i,long end) returns long;
    Indentation(long pos) returns int;
    RedoStyles();
    ReflowComment(long pos) returns boolean;
    SetAttributes(struct attributes *atts);
    SetupStyles();

  methods:

  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct cpptext *self) returns boolean;
 
  macromethods:

  data:
    boolean inClassDef;
    int acsctrlOutdent;
};
