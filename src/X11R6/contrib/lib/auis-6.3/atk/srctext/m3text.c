/* File m3text.c created by R L Quinn
  
   m3text, a Modula-3 mode for ATK. */

/* Copyright 1994 Carnegie Mellon University and IBM. All rights reserved.
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/m3text.c,v 1.5 1994/02/22 20:14:18 rr2b Exp $";
#endif

#include <andrewos.h>
#include <class.h>

#include <stylesht.ih>
#include <style.ih>
#include <fontdesc.ih>
#include <toolcnt.h>    /*RSK90add*/

#include "srctext.ih"
#include "m3text.eh"

static Dict *words[TABLESIZE];

/* modpragma should be functionally the same as modtext's CheckComment method */
static long modpragma(self, start)
struct m3text *self;
long start;
{
    int prev, c=0, pragmas=1;
    long end=start+1, len=m3text_GetLength(self);

    while (end<len) {
	prev= c;
	c= m3text_GetChar(self, ++end);
	switch(c) {
	    case '*':
		if(prev=='<') pragmas++;
		break;
	    case '>':
		if(prev=='*') pragmas--;
		break;
	}
	if (pragmas<1) {
	    m3text_WrapStyle(self, start,end-start+1, self->header.srctext.kindStyle[PRAGMA], FALSE,FALSE);
	    break;
	}
    }
    return end;
}

/* like modtext_RedoStyles, but with ctext's \' and \" recognition */
void m3text__RedoStyles(self)
struct m3text *self;
{
    long posn, len = m3text_GetLength(self);
    int prev = 0, c = '\n'; /* c is initialized to a newline so the start of the file looks like the start of line. */
    boolean escape= FALSE;
    m3text_RemoveStyles(self); /* Remove the old styles, but leave the root environment in place. */
    for (posn=0; posn<len-1; posn++) {
	prev = c;
	posn=m3text_SkipWhitespace(self,posn,len);
	c = m3text_GetChar(self, posn);
	if(escape)
	    escape=FALSE;
	else
	    switch (c) {
		case '\n':
		    break;
		case '\\':
		    escape=TRUE;
		    break;
		case '*':
		    if (prev == '(')
			posn= m3text_CheckComment(self, posn-1);
		    else if (prev == '<') 
			posn= modpragma(self, posn-1);
		    break;
		case '\'':
		case '\"':
		    posn= m3text_CheckString(self,posn);
		    break;
		default:
		    if (m3text_IsTokenChar(self,c))
			posn= m3text_CheckWord(self,posn,len);
		    break;
	    }
    }
}

/* returns true iff the character at pos is quoted (ie. "\"). Takes into account the slash being quoted. (ie "\\"). */ /*RSK92mod*/
boolean m3text__Quoted(self, pos)
struct m3text *self;
long pos;
{
    boolean retval = FALSE;
    while (pos-->0 && m3text_GetChar(self,pos)=='\\')
	retval= !retval;
    return retval;
}

static void SetupStyles(self)
struct m3text *self;
{
    if ((self->header.srctext.kindStyle[IDNTFR]= stylesheet_Find(self->header.text.styleSheet, "identifier")) == NULL) {
	self->header.srctext.kindStyle[IDNTFR]= style_New();
	style_SetName(self->header.srctext.kindStyle[IDNTFR], "identifier");
	style_AddNewFontFace(self->header.srctext.kindStyle[IDNTFR], fontdesc_Bold | fontdesc_Italic);
	stylesheet_Add(self->header.text.styleSheet, self->header.srctext.kindStyle[IDNTFR]);
    }
    if ((self->header.srctext.kindStyle[PRAGMA]= stylesheet_Find(self->header.text.styleSheet, "pragma")) == NULL) {
	self->header.srctext.kindStyle[PRAGMA]= style_New();
	style_SetName(self->header.srctext.kindStyle[PRAGMA], "pragma");
	style_AddNewFontFace(self->header.srctext.kindStyle[PRAGMA], fontdesc_Italic);
	style_SetFontSize(self->header.srctext.kindStyle[PRAGMA], style_PreviousFontSize, 2);
	stylesheet_Add(self->header.text.styleSheet, self->header.srctext.kindStyle[PRAGMA]);
    }
}

void m3text__SetupStyles(self)
struct m3text *self;
{
    super_SetupStyles(self);
    SetupStyles(self);
}

boolean m3text__InitializeClass(classID)
struct classheader *classID;
{
    static Dict m3keywords[]={
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
	{"ANY",0,KEYWRD}, /*RSK92add, per D.Orgass (ref. Nelson book)*/
	{"ARRAY",0,KEYWRD},
	{"BEGIN",9,KEYWRD},
	{"BITS",0,KEYWRD},/*added*/
	{"BRANDED",0,KEYWRD},/*RLQ added*/
	{"BY",0,KEYWRD},
	{"CASE",1,KEYWRD},/*change 65 to 1; free up bit#6 for future use*/
	{"CONST",73,KEYWRD},/*change 9 to 73*/
	{"DIV",0,KEYWRD},/*added*/
	{"DO",1,KEYWRD},/*change 0 to 1 so FOR can be used with BITS without indenting*/
	{"ELSE",4,KEYWRD},
	{"ELSIF",4,KEYWRD},/*change 36 to 4; use bit#5 for INTERFACE/MODULE*/
	{"END",18,KEYWRD},/*change 50 to 18; see ELSIF above*/
	{"EVAL",0,KEYWRD},/*added*/
	{"EXCEPT",4,KEYWRD},/*added*/
	{"EXCEPTION",73,KEYWRD},/*added*/
	{"EXIT",0,KEYWRD},/*added*/
	{"EXPORTS",0,KEYWRD},/*added the "S"*/
	{"FINALLY",4,KEYWRD},/*added*/
	{"FOR",0,KEYWRD},/*changed 1 to 0, indent DO instead*/
	{"FROM",0,KEYWRD},
	{"IF",1,KEYWRD},
	{"IMPORT",0,KEYWRD},
	{"IN",0,KEYWRD},/*added*/
	{"INTERFACE",33,KEYWRD},/*changed 0 to 33*/
	{"LOCK",0,KEYWRD},/*added, note that indentation is taken care of by DO*/
	{"LOOP",1,KEYWRD},/*added*/
	{"METHODS",4,KEYWRD},/*added*/
	{"MOD",0,KEYWRD},
	{"MODULE",96,KEYWRD},/*changed 8 to 96*/
	{"NOT",0,KEYWRD},
	{"OBJECT",1,KEYWRD},/*added*/
	{"OF",0,KEYWRD},
	{"OR",0,KEYWRD},
	{"OVERRIDES",4,KEYWRD}, /*RSK92add, per D.Orgass (ref. Nelson book). Treat same as METHODS for indentation*/
	{"PROCEDURE",PROCEDURE_VAL,KEYWRD},/*change 136 to PROCEDURE_VAL(=200)*/
	{"RAISE",0,KEYWRD},/*added*/
	{"RAISES",0,KEYWRD},/*added*/
	{"READONLY",0,KEYWRD},/*added*/
	{"RECORD",1,KEYWRD},
	{"REF",0,KEYWRD},/*added*/
	{"REPEAT",1,KEYWRD},
	{"RETURN",0,KEYWRD},
	{"REVEAL",73,KEYWRD}, /* RLQ added */
	{"ROOT",0,KEYWRD}, /* RLQ added */
	{"SET",0,KEYWRD},
	{"THEN",0,KEYWRD},
	{"TO",0,KEYWRD},
	{"TRY",1,KEYWRD},/*added*/
	{"TYPE",73,KEYWRD},/*change 9 to 73*/
	{"TYPECASE",1,KEYWRD},/*added*/
	{"UNSAFE",0,KEYWRD},/*added*/
	{"UNTIL",18,KEYWRD},/*change 50 to 18, see ELSIF*/
	{"UNTRACED",0,KEYWRD},/*added*/
	{"VALUE",0,KEYWRD},/*added*/
	{"VAR",73,KEYWRD},/*change 9 to 73*/
	{"WHILE",0,KEYWRD},/*change 1 to 0, let DO take care of indentation*/
	{"WITH",0,KEYWRD},/*same as WHILE*/
	{"|",4,KEYWRD},/*this is to align |'s with case statements; note that [Tab] key should be pressed after [|] so that cases line up properly*/
    /* Reserved Identifiers *//*RSKadds*/
	{"ABS",0,IDNTFR},
	{"ADDRESS",0,IDNTFR},
	{"ADR",0,IDNTFR},
	{"ADRSIZE",0,IDNTFR},
	{"BITSIZE",0,IDNTFR},
	{"BOOLEAN",0,IDNTFR},
	{"BYTESIZE",0,IDNTFR},
	{"CARDINAL",0,IDNTFR},
	{"CEILING",0,IDNTFR},
	{"CHAR",0,IDNTFR},
	{"DEC",0,IDNTFR},
	{"DISPOSE",0,IDNTFR},
	{"FALSE",0,IDNTFR},
	{"FIRST",0,IDNTFR},
	{"FLOAT",0,IDNTFR},
	{"FLOOR",0,IDNTFR},
	{"INC",0,IDNTFR},
	{"INTEGER",0,IDNTFR},
	{"ISTYPE",0,IDNTFR},
	{"LAST",0,IDNTFR},
	{"LONGFLOAT",0,IDNTFR},
	{"LONGREAL",0,IDNTFR},
	{"LOOPHOLE",0,IDNTFR},
	{"MAX",0,IDNTFR},
	{"MIN",0,IDNTFR},
	{"MUTEX",0,IDNTFR},
	{"NARROW",0,IDNTFR},
	{"NEW",0,IDNTFR},
	{"NIL",0,IDNTFR},
	{"NULL",0,IDNTFR},
	{"NUMBER",0,IDNTFR},
	{"ORD",0,IDNTFR},
	{"REAL",0,IDNTFR},
	{"REFANY",0,IDNTFR},
	{"ROUND",0,IDNTFR},
	{"SUBARRAY",0,IDNTFR},
	{"TEXT",0,IDNTFR},
	{"TRUE",0,IDNTFR},
	{"TRUNC",0,IDNTFR},
	{"TYPECODE",0,IDNTFR},
	{"VAL",0,IDNTFR},
	{NULL,0,0} };
    srctext_BuildTable("m3text",words,m3keywords);
    return TRUE;
}

boolean m3text__InitializeObject(classID, self)
struct classheader *classID;
struct m3text *self;
{
    self->header.srctext.words= (Dict **)words;
    SetupStyles(self); /*RSK92add*/
    ToolCount("EditViews-m3text",NULL);    /*RSK90add*/
    return TRUE;
}
