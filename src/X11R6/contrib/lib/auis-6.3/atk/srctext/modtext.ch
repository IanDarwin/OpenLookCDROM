/* File modtext.ch created by R L Quinn

   modtext.ch: Text subclass specialized for dealing with Modula-X code. */
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
#define PROCEDURE_VAL 201

/* kinds of styles that are used in hash table */    /*RSK90add*/
#define KEYWRD 1    /* keywords */
#define IDNTFR 2    /* identifiers (reserved(M3) or predefined(M2)) */
#define PRAGMA 3    /* M3 only, put here for convenience of ReverseBalance, Indentation, etc */
#define PREPRC 4    /* M2 only, put here for convenience and reuse */

class modtext: srctext {

  overrides:
    BackwardCheckWord(long pos,long first);
    CheckComment(long start) returns long;
    CheckWord(long i,long end) returns long;
    Indentation(long pos) returns int;
    IsTokenChar(char ch) returns boolean;
    Keywordify(char *buff, boolean checkforceupper) returns char *;
    RedoStyles();
    ReverseBalance(long pos) returns long;
    SetAttributes(struct attributes *atts);
    SetupStyles();

  methods:
    IsTokenOrPeriod(char ch) returns boolean;

  classprocedures:
    InitializeObject(struct modtext *self) returns boolean;

  macromethods:

  data:
    boolean preprocessor; /* TRUE if the language has a C-style preprocessor */
    boolean outdentPreproc;
};
