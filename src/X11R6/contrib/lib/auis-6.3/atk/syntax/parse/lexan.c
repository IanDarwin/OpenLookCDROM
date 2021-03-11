/*	lexan.c: superclass for lexical analyzers  */

/*
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
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/parse/RCS/lexan.c,v 1.11 1993/12/15 20:58:04 susan Exp $";
#endif

/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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

#include <andrewos.h>
#include <ctype.h>
#include <lexan.eh>

	boolean
lexan__InitializeClass(ClassID)
	struct classheader *ClassID;
{
	return TRUE;
}

/* lexan__InitializeObject(ClassID, self)
*/
	boolean
lexan__InitializeObject(ClassID, self)
	struct classheader *ClassID;
	struct lexan *self;
{
	return TRUE;
}

/* lexan__FinalizeObject(ClassID, self)
*/
	void
lexan__FinalizeObject(ClassID, self)
	struct classheader *ClassID;
	struct lexan *self;
{
}


/* lexan__TransEscape(buf, plen)
	translate the characters in buf as though they follow backslash
	in a C string
	store the number of characters used in *plen
	return the character value computed (type is int)

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
		      \o	      :  other characters, unchanged
	if no character follows the \, return \ and length of zero
*/
	int
lexan__TransEscape(ClassID, buf, plen)
	struct classheader *ClassID;
	char *buf;
	int *plen;
{
	static char esctab[]
=   "r\rn\nf\ft\tb\bv\v\"\"\'\'\\\\?\177e\033E\033R\rN\nF\fT\tB\bV\v";
	char *cx;
	int val, len;

	if (*buf == '\0') {
		len = 0;
		val = '\\';
	}
	else if (isdigit(*buf)) {
		/* parse digit string */
		len = 0;
		val = 0;
		cx = buf;
		while (isdigit(*cx) && len < 3)  {
			val = 8 * val + (*cx++ - '0');
			len++;
		}
	}
	else if (*buf == '^') {
		/* extended syntax for control-x */
		if (*(buf+1) == '\0') {
			val = '^';
			len = 1;
		}
		val = '\037' & *(buf+1);
		len = 2;
	}
	else {
		len = 1;
		for (cx = esctab ; *cx && *cx != *buf; cx +=2) {}
		val = (*cx) ? *(cx+1) : *buf;
	}
	if (plen) *plen = len;
	return val;
}


/* translate all characters to those of interest for numbers:
	0..9 a..f  +  ,  -  .	*/
static char xlate[] = {   16, 20, 17, 18, 20,	    /*	+  ,  -  .  /  */
		0, 1, 2, 3, 4, 5, 6, 7, 8, 9,	  /* 0 ... 9 */
		20, 20, 20, 20, 20, 20, 	  /* :	;  <  =  >  ? */
		20, 10, 11, 12, 13, 14, 15, 20,   /* @	A ... G */
		20, 20, 20, 20, 20, 20, 20, 20,   /* H ... O */
		20, 20, 20, 20, 20, 20, 20, 20,   /* P ... W */
		19, 20, 20, 20, 20, 20, 20, 20,   /* X ... */
		20, 10, 11, 12, 13, 14, 15, 20,   /* `	a ... g */
		20, 20, 20, 20, 20, 20, 20, 20,   /* h ... o */
		20, 20, 20, 20, 20, 20, 20, 20, 19,  /* p ... x */
};

/* for each of the 21 kinds of character that may appear in or after numbers,
	and for each current state, specify the new state
	initial state is 8.  final state is 9 or 10.
*/
static char newstate [9][21] = {
/*	   char:   0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f, +, -, ., x other */
/* 0 leading 0 */{ 0, 2, 2, 2, 2, 2, 2, 2, 7, 7,10,10,10,10,10,10,10,10, 4, 3,10  },
/* 1 decimal */  { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,10,10,10,10, 5,10,10,10, 4,10,10  },
/* 2 octal */	 { 2, 2, 2, 2, 2, 2, 2, 2, 7, 7,10,10,10,10, 7,10,10,10, 7, 9,10  },
/* 3 hex */	 { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,10,10, 7, 9,10  },
/* 4 after . */  { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,10,10,10,10, 5,10,10,10, 9, 9,10  },
/* 5 after e */  { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9  },
/* 6 exponent */ { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,10,10,10,10, 9,10,10,10, 9, 9,10  },
/* 7 has error */{ 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 9, 9, 9, 9, 9, 9, 9, 9, 7, 9, 9  },
/* 8 initial */  { 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 4, 9, 9  }
/* 9 error halt */
/*10 accept */
};


/* lexan_ParseNumber(buf, plen, intval, realval)
		parses a number from buf and sets *plen to length found
		first character in buf must be legal to start an integer
		sets *intval to integer value and
			if syntax is for a real, sets *dblval to real value
		returns 1 if syntactically an integer, 2 for a double,
			and 0 for a syntax error

	An integer is a string of digits
		or 0x followed by a string of hexadecimal digits
		or a character within apostrophes, possibly \-escaped
	A real has one of the following formats
		.ddd
		ddd.
		ddd.ddd
		.dddEpddd
		ddd.Epddd
		ddd.dddEpddd
		dddEpddd
	where
		ddd is a digit sequence  (one or more digits)
		p is empty or + or -
		E may be 'e' or 'E'  and means the exponent
*/
	int
lexan__ParseNumber(ClassID, buf, plen, intval, dblval)
	struct classheader *ClassID;
	char *buf;
	long *plen, *intval;
	double *dblval;
{
	long val, len;
	char oldstate, currstate;
	register int x;
	register char *bx;
	int success;

	if (*buf == '\'') {
		/* process quoted character */
		val = buf[1];
		if (val == '\\') {
			val = lexan_TransEscape(buf+2, &len);
			len += 3;  /* two apostrophes and the backslash */
		}
		else len = 3;
		if (plen) *plen = len;
		if (intval) *intval = val;
		return (buf[len-1] == '\'') ? 1 : 0;
	}

	len = 0;
	val = 0;
	currstate = 8;
	bx = buf;
	x = *bx++;
	while (TRUE) {
		x = (x < '+' || x > 'x')  ?  20  :  xlate[x - '+'];
		oldstate = currstate;
		currstate = newstate[oldstate][x];
		if (currstate > 8) break;

		/* accumulate value */
		switch(currstate) {
		case 1: /* accumulate decimal value */
			val = val * 10 + x;
			break;
		case 2: /* accumulate octal value */
			val = val * 8 + x;
			break;
		case 3: /* accumulate hexadecimal value */
			if (x != 19)
				val = val * 16 + x;
			break;
		}
		x = *bx++;
	}
	len = bx - buf - 1 - ((currstate == 9) ? 1 : 0);
	if (plen)
		*plen = len;	/* send back len */

	/* convert */
	if (currstate == 9)
		/* error */
		success = 0;
	else if (oldstate >= 4 && oldstate <= 6) {
		/* is real value */
		extern double atof();
		double dval;

		success = 2;
		/* convert number */
		dval = atof(buf);
		if (dblval)
			*dblval = dval;
		if (intval)
			*intval = dval + 0.5;	     /* round to integer */
	}
	else {
		/* is integer */
		success = 1;
		if (intval)
			*intval = val;
		if (dblval)
			*dblval = val;
	}
	return success;
}


	int
lexan__NextToken(self, pyylval)
	register struct tlex *self;
	void **pyylval;
{
	/* use grammar ../bison/foo.y and token stream: ?/ab?/?/ */
	static short dummy[] = {3, 6, 4, 5, 3, 6, 3, 6, 0};
	static int dummyinx = 0;
	return (dummyinx < sizeof(dummy)/sizeof(short)) 
			? dummy[dummyinx++]
			: 0;
}

