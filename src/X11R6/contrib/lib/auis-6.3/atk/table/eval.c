/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/table/RCS/eval.c,v 1.9 1992/12/15 21:43:33 rr2b R6tape $";
#endif


 

/* eval.c - parse and evaluate expressions in table */


#include <andrewos.h>
#include <stdio.h>
#include <setjmp.h>
#include <signal.h>
#include <ctype.h>

#include <class.h>
#include <table.ih>

#if !POSIX_ENV
extern char * malloc();
#endif
extern double pow();

#define CASEBIT 040

#define ADDOP	000001
#define MULOP   000002
#define EXPOP   000004
#define LBRACK  000010
#define RBRACK  000020
#define COMMA   000040
#define LPAREN  000100
#define RPAREN  000200
#define UPPER   000400
#define LOWER	000400
#define PERCENT 001000
#define DIGIT   002000
#define BLANK   004000
#define CMPOP	010000
#define DOT	020000
#define COLON	040000

short   charclass[128] =
{
    0, 0, 0, 0, 0, 0, 0, 0,
    0, BLANK, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    BLANK, 0, 0, 0, 0, PERCENT, 0, 0,
    LPAREN, RPAREN, MULOP, ADDOP, COMMA, ADDOP, DOT, MULOP,
    DIGIT, DIGIT, DIGIT, DIGIT, DIGIT, DIGIT, DIGIT, DIGIT,
    DIGIT, DIGIT, COLON, 0, CMPOP, CMPOP, CMPOP, 0,
    0, UPPER, UPPER, UPPER, UPPER, UPPER, UPPER, UPPER,
    UPPER, UPPER, UPPER, UPPER, UPPER, UPPER, UPPER, UPPER,
    UPPER, UPPER, UPPER, UPPER, UPPER, UPPER, UPPER, UPPER,
    UPPER, UPPER, UPPER, LBRACK, 0, RBRACK, EXPOP, 0,
    0, LOWER, LOWER, LOWER, LOWER, LOWER, LOWER, LOWER,
    LOWER, LOWER, LOWER, LOWER, LOWER, LOWER, LOWER, LOWER,
    LOWER, LOWER, LOWER, LOWER, LOWER, LOWER, LOWER, LOWER,
    LOWER, LOWER, LOWER, 0, 0, 0, 0, 0
};

#define skipb(x) while(charclass[*(x)]&BLANK) (x)++

#define getop(x,class) (charclass[*(x)]&(class)?*(x)++:0)

static void expr();
static double relexpr(), aexpr(), term(), factor(), atom(), funcall();

struct jbstruct {
    jmp_buf env;
    extended_double *result;
};
static struct jbstruct *jbs = NULL;

syntaxError ()
{
    MakeBogus(jbs -> result, "SYNTAX!");
    longjmp (jbs -> env, 1);
}

#if defined(_ANSI_C_SOURCE) && !defined(_NO_PROTO)
void Exception (int sig)
#else
Exception ()
#endif
{
    MakeBogus(jbs -> result, "ARITH!");
    longjmp (jbs -> env, 1);
}

static double standardize(x)
extended_double *x;
{
    if (!IsStandard(x)) {
	*(jbs -> result) = *x;
	longjmp (jbs -> env, 1);
    }
    return StandardValue(x);
}

void  eval (T, result, r, c, input)
register struct table * T;
extended_double *result;
int     r, c;
char   *input;
{
    char *saveinput = input;

    if (!*input) {
	MakeBogus(result, "NULL!");
	return;
    }
    if (*input == '\'' || *input == '\"' || *input == '^') {
	MakeBogus(result, "VALUE!");
	return;
    }
    expr (T, result, &input, r, c);
    if (IsStandard(result)) {
	skipb(input);
	if (*input)
	    MakeBogus(result, "SYNTAX!");   /* extra stuff after expression */
    }
    if (IsBogus(result)) {
	int d = trydate (saveinput);
	if (d)
	    MakeStandard(result, (double) d);
    }
}

static void expr (T, result, inptr, r, c)
register struct table * T;
extended_double *result;
char **inptr;
int     r, c;
{
    struct jbstruct new_jbs, *old_jbs;
#if defined(_ANSI_C_SOURCE) && !defined(_NO_PROTO)
    void (*oldsig) (int sig);
#else
    int (*oldsig) ();
#endif

    old_jbs = jbs;
    jbs = &new_jbs;
    jbs -> result = result;
    oldsig = signal(SIGFPE, Exception);
    if (!setjmp(jbs -> env)) {
	MakeStandard(result, relexpr(T, inptr, r, c));
    }
    jbs = old_jbs;
    signal (SIGFPE, oldsig);
}

static double relexpr(T, inptr, r, c)
register struct table *T;
char **inptr;
int     r, c;
{
    int     op, op2;
    double  x, y;

    x = aexpr (T, inptr, r, c);
    skipb(*inptr);
    while (op = getop (*inptr, CMPOP)) {
	op2 = getop (*inptr, CMPOP);
	y = aexpr (T, inptr, r, c);
	skipb(*inptr);
	if (!op2)
	    if (op == '=')
		x = (double) (x == y);
	    else if (op == '>')
		x = (double) (x > y);
	    else if (op == '<')
		x = (double) (x < y);
	    else
		syntaxError(/*"<=> expected" */);
	else if (op == '<' && op2 == '>')
	    x = (double) (x != y);
	else if (op2 == '=')
	    if (op == '>')
		x = (double) (x >= y);
	    else if (op == '<')
		x = (double) (x <= y);
	    else
		syntaxError(/*"<> expected" */);
	else
	    syntaxError(/*"<=> expected" */);
    }
    return x;
}

static double aexpr (T, inptr, r, c)
register struct table *T;
char **inptr;
int     r, c;
{
    int     op;
    double  x, y;

    x = term (T, inptr, r, c);
    skipb(*inptr);
    while (op = getop (*inptr, ADDOP)) {
	y = term (T, inptr, r, c);
	if (op == '+')
	    x += y;
	else
	    x -= y;
	skipb(*inptr);
    }
    return x;
}

static double term (T, inptr, r, c)
register struct table * T;
char **inptr;
int     r, c;
{
    int     op;
    double  x, y;

    x = factor (T, inptr, r, c);
    skipb(*inptr);
    while (op = getop (*inptr, MULOP)) {
	y = factor (T, inptr, r, c);
	if (op == '*')
	    x *= y;
	else
	    x /= y;
	skipb(*inptr);
    }
    return x;
}

static double factor (T, inptr, r, c)
register struct table * T;
char **inptr;
int     r, c;
{
    int     op;
    double x, y;

    skipb(*inptr);
    if (op = getop (*inptr, ADDOP)) {
	skipb(*inptr);
	x = factor (T, inptr, r, c);
	if (op == '-') {
	    x = -x;
	}
    }
    else
	x = atom (T, inptr, r, c);
    skipb(*inptr);
    if (op = getop (*inptr, EXPOP)) {
	y = factor (T, inptr, r, c);
	x = pow(x, y);
	skipb(*inptr);
    }
    return x;
}

static cellref (T, inptr, rr, cc, r, c)
register struct table * T;
char **inptr;
int     rr, cc;
double *r, *c;
{
    *r = relexpr(T, inptr, rr, cc);
    skipb(*inptr);
    if (!getop (*inptr, COMMA))
	syntaxError(/*", expected" */);
    *c = relexpr(T, inptr, rr, cc);
    skipb(*inptr);
    if (!getop (*inptr, RBRACK))
	syntaxError(/*"] expected" */);
}

#define DigitToDouble(c) ((double) (c - '0'))

static double atom (T, inptr, rr, cc)
register struct table * T;
char **inptr;
int     rr, cc;
{
    int    c;
    double x = 0, y = 0;
    char *p;
    int h;
    int count;
    int length;
    extended_double args[100];
    double r0, c0, r1, c1;
    char *isave;

    skipb(*inptr);
    if (getop (*inptr, LBRACK)) {
	cellref (T, inptr, rr, cc, &r0, &c0);
	rcref (T, &args[0], (int) r0, (int) c0, 1);
	x = standardize(&args[0]);

    } else if (getop (*inptr, LPAREN)) {
	x = relexpr(T, inptr, rr, cc);
	skipb(*inptr);
	if (!getop (*inptr, RPAREN))
	    syntaxError(/*") expected" */);

    } else if (c = getop (*inptr, UPPER | LOWER)) {
	p = *inptr - 1;
	h = c & ~CASEBIT;
	count = 0;
	while (c = getop (*inptr, UPPER | LOWER | DIGIT))
	    h = (h * 65599 + (c & ~CASEBIT)) & 0xffff;
	length = *inptr - p;
	skipb(*inptr);
	if (getop (*inptr, LPAREN)) {
	    do {
		if (getop (*inptr, LBRACK)) {
		    isave = *inptr - 1;
		    cellref (T, inptr, rr, cc, &r0, &c0);

		    if (getop (*inptr, COLON)) {
			if (!getop (*inptr, LBRACK))
			    syntaxError(/*"[ expected" */);
			cellref (T, inptr, rr, cc, &r1, &c1);
			if (r0 > r1) { x = r0; r0 = r1; r1 = x; }
			if (c0 > c1) { x = c0; c0 = c1; c1 = x; }
			MakeBogus(&args[count++], "range");
			MakeStandard(&args[count++], r0);
			MakeStandard(&args[count++], c0);
			MakeStandard(&args[count++], r1);
			MakeStandard(&args[count++], c1);
		    } else /* not COLON */ {
			*inptr = isave;
			expr (T, &args[count++], inptr, rr, cc);
		    }
		} else { /* not LBRACK */
		    expr (T, &args[count++], inptr, rr, cc);
		}
		skipb(*inptr);
	    } while (getop (*inptr, COMMA));
	    skipb(*inptr);
	    if (!getop (*inptr, RPAREN))
		syntaxError(/*") expected" */ );
	} /* if LPAREN */
	x = funcall (T, rr, cc, p, length, h, args, count);

    } else if (c = getop (*inptr, DIGIT)) {
	x = DigitToDouble (c);
	while (c = getop (*inptr, DIGIT))
	    x = x * 10.0 + DigitToDouble (c);
	if (c = getop (*inptr, DOT)) {
	    y = 1.0;
	    while (c = getop (*inptr, DIGIT))
		x += (y *= 0.1) * DigitToDouble (c);
	}
	if (getop (*inptr, PERCENT))
	    x *= 0.01;

    } else if (c = getop (*inptr, DOT)) {
	x = 0.0;
	y = 1.0;
	if (!(c = getop (*inptr, DIGIT)))
	    syntaxError(/*"Decimal point without number" */ );
	x += (y *= 0.1) * DigitToDouble (c);
	while (c = getop (*inptr, DIGIT))
	    x += (y *= 0.1) * DigitToDouble (c);
	if (getop (*inptr, PERCENT))
	    x *= 0.01;
    } else
	syntaxError(/*"number expected" */);
    return x;
}

struct fun {
    char   *name;
    short   length,
	    argc;
    double (*f)();
    struct fun *next;
};

#define HASHMASK 63		/* must be power of 2 minus 1 */
static int  initdone = 0;
static struct fun  *htable[HASHMASK + 1];
static struct fun   sentinal;

void enterfun (name, fptr, argc)
char     *name;
double (*fptr)();
int     argc;
{
    struct fun *p, **q;
    char    c;
    int     h = 0;
    p = (struct fun *) malloc (sizeof (struct fun));
    if (p == NULL) {
	printf("Out of memory for function pointer\n");
	exit(4);
    }
    p -> name = name;
    p -> length = strlen (name);
    p -> argc = argc;
    p -> f = fptr;
    while (c = *name++)
	h = (h * 65599 + (c & ~CASEBIT)) & 0xffff;
    q = htable + (h & HASHMASK);
    while ((*q) != &sentinal)
	q = &((*q) -> next);
    *q = p;
    p -> next = &sentinal;
}

inithash () {
    int     i;
    for (i = 0; i <= HASHMASK; i++)
	htable[i] = &sentinal;
    enterfuns ();
    initdone = 1;
}

static double funcall (T, rr, cc, name, length, h, args, argc)
register struct table * T;
int     rr, cc;
char   *name;
extended_double *args;
int     argc,
	h,
	length;
{
    struct fun *p;
    extended_double myresult;

    if (!initdone)
	inithash ();

    sentinal.name = name;
    sentinal.length = length;
    for (p = htable[h & HASHMASK];; p = p -> next) {
	char   *s, *t;
	if (p -> length != length)
	    continue;
	for (s = p -> name, t = name; *s; s++, t++)
	    if (!(*s == *t || isupper (*t) && *s == tolower (*t)))
		goto NEXT;
	break;
NEXT: 	;
    }

    if (p == &sentinal)
	syntaxError(/*"Unknown name" */);

    if (p -> argc >= 0 && argc != p -> argc)
	syntaxError(/*"Wrong # of parameters" */);

    switch (p -> argc) {
	case -1:
	    ((void (*)())(p -> f))(T, &myresult, rr, cc, argc, args);
	    return standardize(&myresult);;
	case 0:
	    return (p -> f)();
	case 1:
	    return (p -> f)(standardize(&args[0]));
	case 2:
	    return (p -> f)(standardize(&args[0]), standardize(&args[1]));
	case 3:
	    return (p -> f)(standardize(&args[0]), standardize(&args[1]), standardize(&args[2]));
	default:
	    syntaxError(/*"Unimplemented function case" */);
    }
}

char   *translate (input, translaterc, ms)
char   *input;
int     (*translaterc) ();
struct movetrstate * ms;
{
    int     any = 0;
    int     absr,
	    absc;
    char    output[1000];
    char   *ip = input,
	   *op = output;

    for (;;) {
	int     multr, offsetr = 0, multc, offsetc = 0;
	if (*ip == '[') {
	    char   *s = ip;
	    s++;
	    skipb(s);

	    if (*s == 'r') {
		s++;
		skipb(s);
		if (*s == '+')
		    multr = 1;
		else if (*s == '-')
		    multr = -1;
		else if (*s == ',')
		    multr = 0;
		else
		    goto NOMATCH;
		s++;
		absr = 0;
	    } else if (charclass[*s] & DIGIT) {
		multr = 1;
		absr = 1;
	    } else
		goto NOMATCH;
	    if (multr) {
		skipb(s);
		while (charclass[*s] & DIGIT)
		    offsetr = offsetr * 10 + (*s++) - '0';
		if (!offsetr)
		    goto NOMATCH;
		skipb(s);
		if (*s != ',')
		    goto NOMATCH;
		s++;
	    }
	    skipb(s);
	    if (*s == 'c') {
		s++;
		skipb(s);
		if (*s == '+')
		    multc = 1;
		else if (*s == '-')
		    multc = -1;
		else if (*s == ']')
		    multc = 0;
		else
		    goto NOMATCH;
		s++;
		absc = 0;
	    }
	    else
		if (charclass[*s] & DIGIT) {
		    multc = 1;
		    absc = 1;
		}
		else
		    goto NOMATCH;
	    if (multc) {
		skipb(s);
		while (charclass[*s] & DIGIT)
		    offsetc = offsetc * 10 + (*s++) - '0';
		if (!offsetc)
		    goto NOMATCH;
		skipb(s);
		if (*s != ']')
		    goto NOMATCH;
		s++;
	    }
	    offsetr *= multr;
	    offsetc *= multc;
	    if (translaterc (&offsetr, &offsetc, absr, absc, ms)) {
		char    rbuf[30],
			cbuf[30];
		*rbuf = 0;
		*cbuf = 0;
		if (absr)
		    sprintf (rbuf, "%d", offsetr);
		else
		    if (offsetr)
			sprintf (rbuf, "r%s%d", offsetr > 0 ? "+" : "", offsetr);
		    else
			sprintf (rbuf, "r");
		if (absc)
		    sprintf (cbuf, "%d", offsetc);
		else
		    if (offsetc)
			sprintf (cbuf, "c%s%d", offsetc > 0 ? "+" : "", offsetc);
		    else
			sprintf (cbuf, "c");
		sprintf (op, "[%s,%s]", rbuf, cbuf);
	    }
	    else
		sprintf (op, "error");
	    while (*op)
		op++;
	    ip = s;
	    any++;
	    continue;
	}
NOMATCH: 
	if (!(*op++ = *ip++))
	    break;;
    }

    if (any)
	return output;
    else
	return input;
}

static char *monthname[] = {
    NULL, "january", "february", "march", "april", "may", "june",
    "july", "august", "september", "october", "november", "december"
};

static int trydate (input)
char   *input;
{
    int     day = 0, month = 0, year = 0;
    int     i;
    char    c;
    char    name[20];
    char   *p = name;
    skipb(input);
    while (c = getop (input, DIGIT))
	day = day * 10 + c - '0';
    skipb(input);
    while (p - name < 18 && (*p = getop (input, UPPER | LOWER))) {
	if (isupper (*p))
	    *p = tolower (*p);
	p++;
    }
    *p = 0;
    for (i = 1; i <= 12; i++) {
	if (!strncmp (name, monthname[i], p - name))
	    if (month)
		return 0;
	    else
		month = i;
    }
    skipb(input);
    while (c = getop (input, DIGIT))
	year = year * 10 + c - '0';
    skipb(input);
    if (*input)
	return 0;
    if (!year)
	return 0;
    if (year < 100)
	year += 1900;
    if (!day)
	day = 1;
    if (!month)
	month = 1;
    return idate (year, month, day);
}
