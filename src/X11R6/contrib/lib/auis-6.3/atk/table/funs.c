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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/table/RCS/funs.c,v 1.9 1992/12/15 21:43:33 rr2b R6tape $";
#endif






/*
 * %%%% the need to undefine _C_func appears to be a bug in AIX...pgc
 */
#include <andrewos.h> /* sys/time.h */
#if SY_AIX221
#ifdef _C_func
#undef _C_func
#endif /* ifdef _C_func */
#endif /* if SY_AIX221 */
#include <math.h>
#ifdef hpux
#define random rand
#endif /* hpux */

#include <class.h>

#define AUXMODULE
#include <table.eh>

#define e_TRUE 1.0
#define e_FALSE 0.0

extern int daysinmonth[];
extern char *fcvt();

int isrange (x)
extended_double *x;
{
    return (IsBogus(x) && strcmp (ExtractBogus(x), "range") == 0);
}

static void getrow (T, result, r, c, argc, argv)
register struct table * T;
register extended_double *result;
int     r, c;
int     argc;
extended_double *argv;
{
    if (argc != 0)
	MakeBogus(result, "No args expected");
    else
	MakeStandard(result, (double) (r + 1));
}

static void getcol (T, result, r, c, argc, argv)
register struct table * T;
register extended_double *result;
int     r, c;
int     argc;
extended_double *argv;
{
    if (argc != 0)
	MakeBogus(result, "No args expected");
    else
	MakeStandard(result, (double) (c + 1));
}

static void fsum (T, result, rr, cc, argc, argv)
register struct table * T;
register extended_double *result;
int     rr, cc;
int     argc;
extended_double *argv;
{
    double  x = 0.0;
    extended_double *p = argv;

    while (p - argv < argc)
	if (isrange(p)) {
	    int     r, c;
	    struct chunk chunk;
	    p++;
	    chunk.TopRow = (int) (StandardValue(p++));
	    chunk.LeftCol = (int) (StandardValue(p++));
	    chunk.BotRow = (int) (StandardValue(p++));
	    chunk.RightCol = (int) (StandardValue(p++));
	    rangeLimit (T, &chunk);
	    for (r = chunk.TopRow; r <= chunk.BotRow; r++)
		for (c = chunk.LeftCol; c <= chunk.RightCol; c++) {
		    if (!table_IsJoinedToAnother(T, r-1, c-1)) {
			rcref (T, result, r, c, 0);
			if (!IsStandard(result))
			    return;
			x += StandardValue(result);
		    }
		}
	}
	else {
	    if (!IsStandard(p)) {
		*result = *p;
		return;
	    }
	    x += StandardValue(p);
	    p++;
	}
    MakeStandard(result, x);
}

static void fcount (T, result, rr, cc, argc, argv)
register struct table * T;
register extended_double *result;
int     rr, cc;
int     argc;
extended_double *argv;
{
    int     x = 0;
    extended_double *p = argv;

    while (p - argv < argc)
	if (isrange(p)) {
	    int     r, c;
	    struct chunk chunk;
	    p++;
	    chunk.TopRow = (int) (StandardValue(p++));
	    chunk.LeftCol = (int) (StandardValue(p++));
	    chunk.BotRow = (int) (StandardValue(p++));
	    chunk.RightCol = (int) (StandardValue(p++));
	    rangeLimit (T, &chunk);
	    for (r = chunk.TopRow; r <= chunk.BotRow; r++) {
		for (c = chunk.LeftCol; c <= chunk.RightCol; c++) {
		    if (!table_IsJoinedToAnother(T, r-1, c-1) && table_GetCell(T, r-1, c-1)->celltype == table_ValCell)
			x++;
		}
	    }
	}
	else {
	    if (IsStandard(p))
		x++;
	    p++;
	}
    MakeStandard(result, (double) (x));
}

static void fmax (T, result, rr, cc, argc, argv)
register struct table * T;
register extended_double *result;
int     rr,
        cc;
int     argc;
extended_double *argv;
{
    double  x = 0;
    int     any = 0;
    extended_double *p = argv;

    while (p - argv < argc)
	if (isrange(p)) {
	    int     r, c;
	    struct chunk chunk;
	    p++;
	    chunk.TopRow = (int) (StandardValue(p++));
	    chunk.LeftCol = (int) (StandardValue(p++));
	    chunk.BotRow = (int) (StandardValue(p++));
	    chunk.RightCol = (int) (StandardValue(p++));
	    rangeLimit (T, &chunk);
	    for (r = chunk.TopRow; r <= chunk.BotRow; r++)
		for (c = chunk.LeftCol; c <= chunk.RightCol; c++) {
		    if (!table_IsJoinedToAnother(T, r-1, c-1)) {
			rcref (T, result, r, c, 0);
			if (!IsStandard(result))
			    return;
			if (any++)
			    x = StandardValue(result) > x ? StandardValue(result) : x;
			else
			    x = StandardValue(result);
		    }
		}
	}
	else {
	    if (!IsStandard(p)) {
		*result = *p;
		return;
	    }
	    if (any++)
		x = StandardValue(p) > x ? StandardValue(p) : x;
	    else
		x = StandardValue(p);
	    p++;
	}
    if (any)
	MakeStandard(result, x);
    else
	eval(T, result, rr, cc, "-1/0");
}

static void fmin (T, result, rr, cc, argc, argv)
register struct table * T;
register extended_double *result;
int     rr,
        cc;
int     argc;
extended_double *argv;
{
    double  x = 0;
    int     any = 0;
    extended_double *p = argv;

    while (p - argv < argc)
	if (isrange(p)) {
	    int     r, c;
	    struct chunk chunk;
	    p++;
	    chunk.TopRow = (int) (StandardValue(p++));
	    chunk.LeftCol = (int) (StandardValue(p++));
	    chunk.BotRow = (int) (StandardValue(p++));
	    chunk.RightCol = (int) (StandardValue(p++));
	    rangeLimit (T, &chunk);
	    for (r = chunk.TopRow; r <= chunk.BotRow; r++)
		for (c = chunk.LeftCol; c <= chunk.RightCol; c++) {
		    if (!table_IsJoinedToAnother(T, r-1, c-1)) {
			rcref (T, result, r, c, 0);
			if (!IsStandard(result))
			    return;
			if (any++)
			    x = StandardValue(result) < x ? StandardValue(result) : x;
			else
			    x = StandardValue(result);
		    }
		}
	}
	else {
	    if (!IsStandard(p)) {
		*result = *p;
		return;
	    }
	    if (any++)
		x = StandardValue(p) < x ? StandardValue(p) : x;
	    else
		x = StandardValue(p);
	    p++;
	}
    if (any)
	MakeStandard(result, x);
    else
	eval(T, result, rr, cc, "1/0");
}

static void vlookup (T, result, rr, cc, argc, argv)
register struct table * T;
register extended_double *result;
int     rr, cc;
int     argc;
extended_double *argv;
{
    double  x;
    extended_double *p = argv;
    int     r;
    struct chunk chunk;
    if (argc != 6 || !isrange(&argv[1]))
	syntaxError ("bad arguments");
    x = StandardValue(p++);
    p++;
    chunk.TopRow = (int) (StandardValue(p++));
    chunk.LeftCol = (int) (StandardValue(p++));
    chunk.BotRow = (int) (StandardValue(p++));
    chunk.RightCol = (int) (StandardValue(p++));
    rangeLimit (T, &chunk);
    for (r = chunk.BotRow; r >= chunk.TopRow; r--) {
	if (!table_IsJoinedToAnother(T, r-1, chunk.LeftCol-1)) {
	    if (table_IsJoinedToAnother(T, r-1, chunk.RightCol-1)) {
		MakeBogus(result, "LOOKUP!");
		return;
	    }
	    rcref (T, result, r, chunk.LeftCol, 0);
	    if (!IsStandard(result))
		return;
	    if (StandardValue(result) <= x) {
		rcref (T, result, r, chunk.RightCol, 1);
		return;
	    }
	}
    }
    MakeBogus(result, "LOOKUP!");
}

static double   iffer (x, y, z)
double  x, y, z;
{
    return x ? y : z;
}

static double   false ()
{
    return e_FALSE;
}

static double true ()
{
    return e_TRUE;
}

static double frand ()
{
    return (double) (random () & (0x1000000 - 1)) / (float) 0x1000000;
}

static double fnot (x)
double  x;
{
    return x ? e_FALSE : e_TRUE;
}

static double fand (x, y)
double  x, y;
{
    return (x != 0 && y != 0);
}

static double orf (x, y)
double  x, y;
{
    return (x != 0 || y != 0);
}

static void fiserr (T, result, rr, cc, argc, argv)
register struct table * T;
register extended_double *result;
int     rr, cc;
int     argc;
extended_double *argv;
{
    int decpt, sign;

    if (argc != 1) {
	MakeBogus(result, "Wrong # of parameters");
	return;
    }
    if (IsBogus(&argv[0])) {
	MakeStandard(result, e_TRUE);
	return;
    }
    /* fcvt returns digits for normal values, "INF" "-INF" or "NAN()" */
    if (*fcvt(StandardValue(&argv[0]), 10, &decpt, &sign) == 'N')
	MakeStandard(result, e_TRUE);
    else
	MakeStandard(result, e_FALSE);
}

static void fisinf (T, result, rr, cc, argc, argv)
register struct table * T;
register extended_double *result;
int     rr, cc;
int     argc;
extended_double *argv;
{
    int decpt, sign;
    char *cvtbuff;

    if (argc != 1) {
	MakeBogus(result, "Wrong # of parameters");
	return;
    }
    if (IsBogus(&argv[0])) {
	MakeStandard(result, e_FALSE);
	return;
    }
    /* fcvt returns digits for normal values, "INF" "-INF" or "NAN()" */
    cvtbuff = fcvt(StandardValue(&argv[0]), 10, &decpt, &sign);
    if (*cvtbuff >= '0' && *cvtbuff <= '9' || *cvtbuff == 'N')
	MakeStandard(result, e_FALSE);
    else
	MakeStandard(result, e_TRUE);
}

int     idate (y, m, d)
int     y,
        m,
        d;
{
    int     leapyear = 0;
    int     ans,
            i;
    y -= 1900;
    m--;
    d--;
    if (y < 0 || y > 199)
	return 0;
    if (m < 0 || m >= 12)
	return 0;
    i = y >> 2;
    ans = i * 1461;
    i += i;
    y = y - (i + i);
    leapyear = (!y);
    if (y) {
	ans += 366;
	while (--y)
	    ans += 365;
    }
    if (d < 0 || d >= daysinmonth[m] + (m==1 ? leapyear : 0)) {
	return 0;
    }
    while (m--)
	ans += daysinmonth[m] + (m==1 ? leapyear : 0);
    ans += d;
    return ans + (ans < 59);
}

static void fdate (T, result, rr, cc, argc, argv)
register struct table * T;
register extended_double *result;
int     rr, cc;
int     argc;
extended_double *argv;
{
    int     y, m, d;
    int     leapyear = 0;
    int     ans, i;

    if (argc != 3) {
	MakeBogus(result, "Wrong # of parameters");
	return;
    }
    if (!IsStandard(&argv[0])) {
	*result = argv[0];
	return;
    }
    if (!IsStandard(&argv[1])) {
	*result = argv[1];
	return;
    }
    if (!IsStandard(&argv[2])) {
	*result = argv[2];
	return;
    }

    y = ((int) (StandardValue(&argv[0]) + 0.5)) - 1900;
    m = ((int) (StandardValue(&argv[1]) + 0.5)) - 1;
    d = ((int) (StandardValue(&argv[2]) + 0.5)) - 1;
    
    if (y < 0 || y > 199) {
	MakeBogus(result, "year out of range");
	return;
    }
    if (m < 0 || m >= 12) {
	MakeBogus(result, "month out of range");
	return;
    }
    i = y >> 2;
    ans = i * 1461;
    i += i;
    y = y - (i + i);
    leapyear = (!y);
    if (y) {
	ans += 366;
	while (--y)
	    ans += 365;
    }
    if (d < 0 || d >= daysinmonth[m] + (m == 1 ? leapyear : 0)) {
	MakeBogus(result, "day out of range");
	return;
    }
    while (m--)
	ans += daysinmonth[m] + (m == 1 ? leapyear : 0);
    ans += d;
    MakeStandard(result, (double) (ans + (ans < 59)));
}

static double   fday (fdate)
double  fdate;
{
    int     date;
    int     m;

    date = (int) (fdate + 0.5);
    date--;
    if (date > 58)
	date++;
    date = date % 1461;
    if (date == 59)
	return 29.0;
    else {
	if (date > 59)
	    date--;
	date = date % 365;
	for (m = 0; date >= 0; m++)
	    date -= daysinmonth[m];
	return (double) (date + daysinmonth[m - 1] + 1);
    }
}

static double   fmonth (fdate)
double  fdate;
{
    int     date;
    int     m;

    date = (int) (fdate + 0.5);
    date--;
    if (date > 58)
	date++;
    date = date % 1461;
    if (date == 59)
	return 2.0;
    else{
	if (date > 59)
	    date--;
	date = date % 365;
	for (m = 0; date >= 0; m++)
	    date -= daysinmonth[m];
	return (double) (m);
    }
}

static double fyear (fdate)
double  fdate;
{
    int     date;
    int     y;

    date = (int) (fdate + 0.5);
    date--;
    if (date > 58)
	date++;
    y = date / 1461;
    date -= y * 1461;
    y = y + y;
    y = y + y;
    if (date > 59)
	date--;
    y += date / 365;
    return (double) (y + 1900);
}

static void errorfunc (T, result, rr, cc, argc, argv)
register struct table * T;
register extended_double *result;
int     rr, cc;
int     argc;
extended_double *argv;

{
    MakeBogus(result, "ERROR!");
}

static double   fmodulo (x, y)
double  x, y;
{
    return x - y * floor (x / y);
}

static double   fpi ()
{
    return 3.141592653589794;
}

static double fround (x, yy)
double  x, yy;
{
    int     y;
    double  p;

    y = ((int) (yy + 1000.5)) - 1000;
    for (p = 1.0; y > 0; y--)
	p *= 10.0;
    for (; y < 0; y++)
	p /= 10.0;
    return floor (x * p + 0.5) / p;
}

static double   today ()
{
    struct timeval  tv;

    gettimeofday (&tv, 0);
    return (double) (25568 + tv.tv_sec / 86400);
}

enterfuns () {
    enterfun("r", getrow, -1);
    enterfun("c", getcol, -1);
    enterfun("abs", fabs, 1);
    enterfun("floor", floor, 1);
    enterfun("ceil", ceil, 1);
    enterfun("exp", exp, 1);
    enterfun("ln", log, 1);
    enterfun("log", log10, 1);
    enterfun("sqrt", sqrt, 1);
    enterfun("sin", sin, 1);
    enterfun("cos", cos, 1);
    enterfun("asin", asin, 1);
    enterfun("acos", acos, 1);
    enterfun("atan", atan, 1);
    enterfun("atan2", atan2, 2);
    enterfun("if", iffer, 3);
    enterfun("false", false, 0);
    enterfun("true", true, 0);
    enterfun("rand", frand, 0);
    enterfun("not", fnot, 1);
    enterfun("iserr", fiserr, -1);
    enterfun("isinf", fisinf, -1);
    enterfun("and", fand, 2);
    enterfun("or", orf, 2);
    enterfun("date", fdate, -1);
    enterfun("day", fday, 1);
    enterfun("month", fmonth, 1);
    enterfun("year", fyear, 1);
    enterfun("error", errorfunc, -1);
    enterfun("mod", fmodulo, 2);
    enterfun("pi", fpi, 0);
    enterfun("round", fround, 2);
    enterfun("today", today, 0);
    enterfun("sum", fsum, -1);
    enterfun("count", fcount, -1);
    enterfun("max", fmax, -1);
    enterfun("min", fmin, -1);
    enterfun("vlookup", vlookup, -1);
}
