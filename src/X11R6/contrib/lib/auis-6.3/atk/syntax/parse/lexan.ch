
/* lexan.ch - headers for lexan class */

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
	Copyright Carnegie Mellon University 1992 - All rights reserved
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
	lexan is a class for lexical analysis
	It is very bare bones and should be subclassed for real applications.

	Its default action is to return EOF from NextToken.
*/

class lexan {

methods:

	NextToken(/* struct lexan *self, */ void *pyylval) returns int;
		/* get the next token from the lexan, set *pyylval to the
		value returned by a hook routine and return the token number */

macromethods:
	SetTranslator(struct parse *p)	(self->translator = (p))
	TranslateTokenNumber(int t) \
		parse_TranslateTokenNumber(self->translator, t)

	/* if a subclass of lexan generates yacc token numbers, they must
	  be translated for use with parse.  class sss would do this by calling
		sss_TranslateTokenNnumber(self, yaccnumber)
	  sss must #include <parse.ih>
	*/

classprocedures:

	InitializeClass() returns boolean;
	InitializeObject(struct parse *self) returns boolean;
	FinalizeObject(struct parse *self);

	ParseNumber(char *buf, long *plen, long *intval, double *dvlval)
			returns int;
		/* parses a number from buf and sets *plen to length found
		sets *intval to integer value and
			if syntax is for a real, sets *dblval to real value
		returns 1 if syntactically an integer, 2 for a double,
			and 0 for a syntax error */

	TransEscape(char *buf, int *plen) returns int;
		/* buf holds a character sequence (at least three chars)
		  that occurred after a backslash in a string.
		  The translation is returned as an int.
		  The number of characters used is returned in *plen.
			  (plen may be NULL)
		  The translations are a superset of C:
		      escape seq      :  translation
		      --------------- :  ------------
		      \\ \' \" \b \t  :  as in C
		      \n \v \f \r     :  as in C
		      \ddd	      :  octal digits, as in C
		      \?	      :  DEL  or  \177
		      \e	      :  ESC  or  ctl-[ or \033
		      \^@	      :  NUL  or  \000
		      \^a ... \^z     :  ctl-a ... ctl-z  or  \001 ... \032
		      \^[  \^\	\^]   :  \033  \034  \035
		      \^^  \^_	      :  \036  \037
		*/
data:

	struct parse *translator; /* home of function to translate tokens */
};
