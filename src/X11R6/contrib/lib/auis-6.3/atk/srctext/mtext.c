/* File mtext.c created by R L Quinn
  
   mtext, a Modula-2 mode for ATK. */
/* Copyright 1988, 1994 Carnegie Mellon University and IBM.  All rights reserved.
 $Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
 */

#ifndef NORCSID
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/mtext.c,v 1.5 1994/02/22 20:14:18 rr2b Exp $";
#endif


#include <andrewos.h>
#include <class.h>

#include <stylesht.ih>
#include <style.ih>
#include <fontdesc.ih>
#include <toolcnt.h>    /*RSK90add*/

#include "srctext.ih"
#include "mtext.eh"

static Dict *words[TABLESIZE];

static void SetupStyles(self)
struct mtext *self;
{
    if ((self->header.srctext.kindStyle[IDNTFR]= stylesheet_Find(self->header.text.styleSheet, "predefined")) == NULL) {
	self->header.srctext.kindStyle[IDNTFR]= style_New();
	style_SetName(self->header.srctext.kindStyle[IDNTFR], "predefined");
	style_AddNewFontFace(self->header.srctext.kindStyle[IDNTFR], fontdesc_Bold | fontdesc_Italic);
	stylesheet_Add(self->header.text.styleSheet, self->header.srctext.kindStyle[IDNTFR]);
    }
    self->header.srctext.kindStyle[PRAGMA]= NULL;
    if ((self->header.srctext.kindStyle[PREPRC]= stylesheet_Find(self->header.text.styleSheet, "preproc")) == NULL) {
	self->header.srctext.kindStyle[PREPRC]= style_New();
	style_SetName(self->header.srctext.kindStyle[PREPRC], "preproc");
	style_AddNewFontFace(self->header.srctext.kindStyle[PREPRC], fontdesc_Italic);
	stylesheet_Add(self->header.text.styleSheet, self->header.srctext.kindStyle[PREPRC]);
    }
}

void mtext__SetupStyles(self)
struct mtext *self;
{
    super_SetupStyles(self);
    SetupStyles(self);
}

boolean mtext__InitializeClass(classID)
struct classheader *classID;
{
    static Dict mkeywords[]={
/* meanings of bit values:
  bit#0   (1) indent lines and match with an END (e.g.BEGIN,IF,etc)
  bit#1   (2) outdent this line (e.g.END)
  bit#2   (4) used to match ELSE and ELSIF with IF
  bit#3   (8) used to align CONST,TYPE,VAR,PROCEDURE,and BEGIN
  bit#4  (16) match END with BEGIN,IF,etc
  bit#5  (32) completely outdents MODULE and INTERFACE
  bit#6  (64) indent lines, but don't match with an END (e.g.PROCEDURE)
  bit#7 (128) used for lining up PROCEDURE arguments or something
*/
    /* Keywords */
	{"AND",0,KEYWRD},
	{"ARRAY",0,KEYWRD},
	{"BEGIN",9,KEYWRD},
	{"BY",0,KEYWRD},
	{"CASE",1,KEYWRD},/*RSKmod: change 65 to 1*/
	{"CONST",73,KEYWRD},
	{"DEFINITION",0,KEYWRD},
	{"DIV",0,KEYWRD},/*RSKadd*/
	{"DO",1,KEYWRD},
	{"ELSE",4,KEYWRD},
	{"ELSIF",4,KEYWRD},
	{"END",18,KEYWRD},
	{"EXIT",0,KEYWRD},/*RSKadd*/
	{"EXPORT",0,KEYWRD},
	{"FOR",0,KEYWRD},
	{"FROM",0,KEYWRD},
	{"IF",1,KEYWRD},
	{"IMPLEMENTATION",0,KEYWRD},
	{"IMPORT",0,KEYWRD},
	{"IN",0,KEYWRD},/*RSKadd*/
	{"LOOP",1,KEYWRD},/*RSKadd*/
	{"MOD",0,KEYWRD},
	{"MODULE",96,KEYWRD},
	{"NOT",0,KEYWRD},
	{"OF",0,KEYWRD},
	{"OR",0,KEYWRD},
	{"POINTER",0,KEYWRD},
	{"PROCEDURE",PROCEDURE_VAL,KEYWRD},
	{"QUALIFIED",0,KEYWRD},
	{"REF",0,KEYWRD},/*RLQ addition per Orgass & Seurer */
	{"RECORD",1,KEYWRD},
	{"REPEAT",1,KEYWRD},
	{"RETURN",0,KEYWRD},
	{"SET",0,KEYWRD},
	{"THEN",0,KEYWRD},
	{"TO",0,KEYWRD},
	{"TYPE",73,KEYWRD},
	{"UNTIL",18,KEYWRD},
	{"VAR",73,KEYWRD},
	{"WHILE",0,KEYWRD},
	{"WITH",0,KEYWRD},
	{"|",4,KEYWRD},/*this is to align |'s with case statements; note that [Tab] key should be pressed after [|] so that cases line up properly*/
    /* Predefined Identifiers */
	{"ABS",0,IDNTFR},
	{"ADR",0,IDNTFR},
	{"BITSET",0,IDNTFR},
	{"BOOLEAN",0,IDNTFR},
	{"CAP",0,IDNTFR},
	{"CARDINAL",0,IDNTFR},
	{"CHAR",0,IDNTFR},
	{"CHR",0,IDNTFR},
	{"DEC",0,IDNTFR},
	{"DISPOSE",0,IDNTFR},
	{"EXCL",0,IDNTFR},
	{"FALSE",0,IDNTFR},
	{"FLOAT",0,IDNTFR},
	{"HALT",0,IDNTFR},
	{"HIGH",0,IDNTFR},
	{"INC",0,IDNTFR},
	{"INCL",0,IDNTFR},
	{"INTEGER",0,IDNTFR},
	{"LONGINT",0,IDNTFR},/*RSKadd*/
	{"LONGREAL",0,IDNTFR},/*RSKadd*/
	{"MAX",0,IDNTFR},/*RSKadd*/
	{"MIN",0,IDNTFR},/*RSKadd*/
	{"NEW",0,IDNTFR},
	{"NIL",0,IDNTFR},/*RSKadd*/
	{"ODD",0,IDNTFR},
	{"ORD",0,IDNTFR},
	{"PROC",0,IDNTFR},
	{"REAL",0,IDNTFR},
	{"SIZE",0,IDNTFR},
	{"TRUE",0,IDNTFR},
	{"TRUNC",0,IDNTFR},
	{"TSIZE",0,IDNTFR},
	{"VAL",0,IDNTFR},
	{NULL,0,IDNTFR} };
    srctext_BuildTable("mtext",words,mkeywords);
    return TRUE;
}

boolean mtext__InitializeObject(classID, self)
struct classheader *classID;
struct mtext *self;
{
    self->header.srctext.words= (Dict **)words;
    self->header.modtext.preprocessor= TRUE;
    SetupStyles(self); /*RSK92add*/
    ToolCount("EditViews-mtext",NULL);    /*RSK90add*/
    return TRUE;
}
