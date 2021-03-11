/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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




/*
    $Log: nessmark.ch,v $
*Revision 1.10  1993/05/04  01:23:55  susan
*RCS Tree Split
*
*Revision 1.9.1.1  1993/02/02  03:02:55  rr2b
*new R6tape branch
*
*Revision 1.9  1992/12/14  20:49:20  rr2b
*disclaimerization
*
Revision 1.8  1992/11/26  02:39:52  wjh
converted CorrectGetChar to GetUnsignedChar
moved ExtendShortSign to interp.h
remove xgetchar.h; use simpletext_GetUnsignedChar
nessrun timing messages go to stderr
replaced curNess with curComp
replaced genPush/genPop with struct compilation
created compile.c to handle compilation
moved scope routines to compile.c
converted from lex to tlex
convert to use lexan_ParseNumber
truncated logs to 1992 only
use bison and gentlex instead of yacc and lexdef/lex

.

Revision 1.7  92/06/05  16:39:31  rr2b
added test for a null text before performing
operation referencing it.

Revision 1.6  1991/09/12  19:44:10  bobg
Update copyright notice

Revision 1.5  1989/06/01  15:58:17  wjh
campus release version

Revision 1.2  88/12/07  22:43:44  wjh

9 Nov
implemented access level control
skip first line of script if starts with # (for shell script)
changed so the name of all initialization functions is init()
added ness-load
moved execution of init() from compilation to first execution
deferred compilation to first FullUpdate time

22 Nov
proc table calls now work correctly with type-free procs  (the first arg can be anything)
added "cheat_" functions which will remain undocumented
changed inset() to a function
fixed some bugs in initial access

25 November
added long strings
added Next Error menu option
made replace() work correctly in all cases
added class() and new()

29 Nov
added ^<upper-case> and \e as characters in strings
added nextn() and length()

6 Dec
added functions: parseint(), parsereal(), firstobject(), whereitwas(), replacewithobject(), addstyles(), nextstylegroup(), enclosingstylegroup(), clearstyles(), hasstyles()
catch bus and segmentation errors


Revision 1.1  88/10/21  10:59:54  wjh
Initial revision

 * Creation 88/03/24 14:56:00 wjh
 * Created by WJHansen following definitions in 
 *	"A Practical Algebra for Substring Expressions"
*/

typedef long sysMarkIndex;
 
class  nessmark  :  mark

{
overrides:

	UpdateMarks(/* struct nessmark *self, */ long pos, long size);
		/* update mark locations for a change to base of size at pos.
			'size' may be negative for deletion. */

methods:

	SetText(/* struct nessmark *self, */ struct simpletext *text);
		/* changes to refer to 'text'.  Updates links in chain from text. */

	Set(/* struct nessmark *self, */ struct simpletext *text, long pos, long length);
		/* sets the mark to the indicated position and length in the given text */
	MakeConst(/* struct nessmark *self, */ char * cx);
		/* sets the mark to refer to the constant */
	Next(/* struct nessmark *self */);
		/* modifies the mark to point to the character after the former value */
	Start(/* struct nessmark *self */);
		/* modifies the mark to refer to the start of the former value */
	Base(/* struct nessmark *self */);
		/* modifies the mark to refer to the entire text */
	Extent(/* struct nessmark *self, */ struct nessmark *tail);
		/* modifies the mark to extend from its former start to the end of tail */
	Replace(/* struct nessmark *self, */ struct nessmark *replacement);
		/* replaces the contents of the mark with the replacement text */

	Equal(/* struct nessmark *self, */ struct nessmark *comparand) returns boolean;
		/* compares the text denoted by the two marks */
	IsEmpty(/* struct nessmark *self */) returns boolean;
		/* tests the text of self to see if it has any characters */

	Length(/* struct nessmark *self */) returns long;
		/* returns the number of characters in the mark */
	NextN(/* struct nessmark *self, */ long n);
		/* apply next 'n' times */

	SetFrom(/* struct nessmark *self, */ struct nessmark *src);
		/* copies src marker to self  (they share text) */
	ToC(/* struct nessmark *self */) returns unsigned char *;
		/* mallocs a string and copies mark into it.  The caller must free the value. */
	
macromethods:


	GetText(/* struct nessmark *self */)   \
		((struct simpletext *)(mark_GetObject((struct mark *)self)))

	/* DetachFromText and AttachToText are intended for use when moving
	   a mark, as on a stack.  The text is not deleted even if it has
	   no marks after the DetachFromText.  To get rid of a mark, 
	   nessmark_Destroy should be used instead.  To change the text
	   associated with a mark, use SetText instead. */
	DetachFromText(/* struct nessmark *self */) if (nessmark_GetText(self)) { \
		simpletext_RemoveMark(nessmark_GetText(self), self);  \
		mark_SetObject((struct mark *)self, NULL);  \
	}
		/* XXX dirty:  Should have an AddMark method in simpletext.H */
	AttachToText(/* struct nessmark *self,  struct simpletext * */  text) { \
		mark_SetObject((struct mark *)self, (struct simpletext *)text);  \
		mark_SetNext((struct mark *)(self), ((struct simpletext *)text)->markList);  \
		((struct simpletext *)text)->markList = (struct mark *)(self);  \
	}
	InitFrom(/* struct nessmark *self,  struct nessmark * */  src) {*(self) = *(src);  \
		nessmark_AttachToText(self, nessmark_GetText(self));}
		/* copies src marker to self  (they share text);
		   self did not previously have a marker value */

classprocedures:

	InitializeClass(/* struct classhdr *ClassID*/) returns boolean;
	InitializeObject(/* struct classhdr *ClassID, */ struct nessmark *self) returns boolean;
	FinalizeObject(/* struct classhdr *ClassID, */ struct nessmark *self);

	/* Warning:  If a nessmark is attached to a text that has no other marks, 
	the text will be deleted when the nessmark is Destroyed  XXX */

data:
};

