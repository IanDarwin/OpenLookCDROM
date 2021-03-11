/* Copyright 1992 Andrew Toolkit Consortium, Carnegie Mellon University */

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







class nessmv : nessview [nessv] {
overrides:
	PostMenus (struct menulist *menulist);

classprocedures:

	InitializeClass() returns boolean;
	InitializeObject(struct nessview *self) returns boolean;
	FinalizeObject(struct nessview *self);

data:
	struct menulist *ml;
};

/*
 *    $Log: nessmv.ch,v $
*Revision 1.4  1993/05/04  01:23:55  susan
*RCS Tree Split
*
*Revision 1.3.1.1  1993/02/02  03:06:51  rr2b
*new R6tape branch
*
*Revision 1.3  1992/12/14  20:50:00  rr2b
*disclaimerization
*
# Revision 1.2  1992/11/26  02:42:25  wjh
# converted CorrectGetChar to GetUnsignedChar
# moved ExtendShortSign to interp.h
# remove xgetchar.h; use simpletext_GetUnsignedChar
# nessrun timing messages go to stderr
# replaced curNess with curComp
# replaced genPush/genPop with struct compilation
# created compile.c to handle compilation
# moved scope routines to compile.c
# converted from lex to tlex
# convert to use lexan_ParseNumber
# truncated logs to 1992 only
# use bison and gentlex instead of yacc and lexdef/lex
#
# .
#
# Revision 1.1  92/06/05  17:28:27  rr2b
# Initial revision
# 
 */

