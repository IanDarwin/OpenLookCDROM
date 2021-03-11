/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/real.c,v 1.17 1993/05/04 01:23:55 susan Exp $";
#endif




#include <andrewos.h>	
#include <class.h>		/* for TRUE, FALSE */
#include <math.h>
#include <error.h>
#include <compdefs.h>
#include <envt.h>

/* 
	Real function comments from /usr/man/man3/math.3m

	acos(x)		sin.3m		inverse trigonometric functions
	acosh(x)		asinh.3m		inverse hyperbolic functions
	asin(x)		sin.3m		inverse trigonometric functions
	asinh(x)		asinh.3m		inverse hyperbolic functions
	atan(x)		sin.3m		inverse trigonometric functions
	atanh(x)		asinh.3m		inverse hyperbolic functions
	atan2(x, y)	sin.3m		inverse trigonometric functions
	-- cabs(z)	hypot.3m		complex absolute value (not implemented)
	cbrt(x)		sqrt.3m		cube root
	ceil(x) => int	floor.3m		interger no less than
	cos(x)		sin.3m		trigonometric function
	cosh(x)		sinh.3m		hyperbolic function
	erf(x)		erf.3m		error function
	erfc(x)		erf.3m		complementary error function
	exp(x)		exp.3m		exponential
	expm1(x)	exp.3m		exp(x) - 1
	fabs(x)		floor.3m		absolute value
	floor(x) => int	floor.3m		integer no greater than
	hypot(x, y)	hypot.3m		Euclidean distance
	j0(x)		j0.3m		bessel function
	j1(x)		j0.3m		bessel function
	jn(n, x)		j0.3m		bessel function
	lgamma(x)	lgamma.3m	log gamma function
	log(x)		exp.3m		natural logarithm
	log10(x)		exp.3m		logarithm to base 10
	log1p(x)		exp.3m		log(1+x)
	pow(x, y)		exp.3m		exponential x**y
	sin(x)		sin.3m		trigonometric function
	sinh(x)		sinh.3m		hyperbolic functions
	sqrt(x)		sqrt.3m		square root
	tan(x)		sin.3m		trigonometric function
	tanh(x)		sinh.3m		hyperbolic function
	y0(x)		j0.3m		bessel function
	y1(x)		j0.3m		bessel function
	yn(n, x)		j0.3m		bessel function

	round(x) => int
	float1(i) => real
	float2(x, i) => x, real

*/

/* single argument real functions */

	void
realUnary(op, iar)
	char op;
	unsigned char *iar;
{
	union stackelement *NSP = NSPstore;
	if (NSP->d.hdr != dblHdr) 
		RunError(": arg is not a real value (uninitialized ?)", iar);
	switch(op) {
		case 'a':	NSP->d.v = acos(NSP->d.v);   break;
		case 'c':	NSP->d.v = asin(NSP->d.v);   break;
		case 'e':	NSP->d.v = atan(NSP->d.v);   break;
		case 'i':	NSP->d.v = cos(NSP->d.v);   break;
		case 'j':	NSP->d.v = cosh(NSP->d.v);   break;
		case 'k':	NSP->d.v = erf(NSP->d.v);   break;
		case 'l':	NSP->d.v = erfc(NSP->d.v);   break;
		case 'm':	NSP->d.v = exp(NSP->d.v);   break;
		case 'o':	NSP->d.v = fabs(NSP->d.v);   break;
		case 'r':	NSP->d.v = j0(NSP->d.v);   break;
		case 's':	NSP->d.v = j1(NSP->d.v);   break;
		case 'u':	NSP->d.v = log(NSP->d.v);   break;
		case 'v':	NSP->d.v = log10(NSP->d.v);   break;
		case 'y':	NSP->d.v = sin(NSP->d.v);   break;
		case 'z':	NSP->d.v = sinh(NSP->d.v);   break;
		case 'A':	NSP->d.v = sqrt(NSP->d.v);   break;
		case 'B':	NSP->d.v = tan(NSP->d.v);   break;
		case 'C':	NSP->d.v = tanh(NSP->d.v);   break;
		case 'D':	NSP->d.v = y0(NSP->d.v);   break;
		case 'E':	NSP->d.v = y1(NSP->d.v);   break;
		case '_': NSP->d.v = -(NSP->d.v);   break;

#if (! SY_U5x && ! SY_AIXx)
#if !defined(VAX_ENV) && !defined(PMAX_ENV)
		case 't':	NSP->d.v = lgamma(NSP->d.v);  break;
#endif /* !defined(VAX_ENV) && !defined(PMAX_ENV) */
		case 'b':	NSP->d.v = acosh(NSP->d.v);   break;
		case 'd':	NSP->d.v = asinh(NSP->d.v);   break;
		case 'f':	NSP->d.v = atanh(NSP->d.v);   break;
		case 'g':	NSP->d.v = cbrt(NSP->d.v);    break;
		case 'n':	NSP->d.v = expm1(NSP->d.v);   break;
		case 'w':	NSP->d.v = log1p(NSP->d.v);   break;
#endif /* (!SYSV && !AIX) */
		default:
			RunError(":unimplemented operation requested", iar);
	}
}

	void
realOther(op, iar)
	char op;
	unsigned char *iar;
{
	union stackelement *NSP = NSPstore;
	double x;
	long l;
	register struct dblstkelt *left ;

	/* check arguments */
	switch (op) {
	case '+':
	case '-':
	case '*':
	case '/':
	case 'a':
	case 'b':
	case 'x':	/* pow(x, y) NSP->d.v = pow(NSP->d.v);   break; */
		/* binary real operations */
		if (NSP->d.hdr != dblHdr)
			RunError(":right operand is not a real value", iar);
		left = &(&(NSP->d))[1];
		if (left->hdr != dblHdr)
			RunError(":left operand is not a real value", iar);
		x = NSP->d.v;
		NSPopSpace(dblstkelt);
		break;
	case  'c':
	case  'd':
	case  'e':
		/* real to integer */
		if (NSP->d.hdr != dblHdr)
			RunError(":operand is not a real value", iar);
		x = NSP->d.v;
		NSPopSpace(dblstkelt);
		NSPushSpace(longstkelt);
		NSP->l.hdr = longHdr;
		break;
	case  'f':
	case  'g':
		/* real to boolean */
		if (NSP->d.hdr != dblHdr)
			RunError(":operand is not a real value", iar);
		x = NSP->d.v;
		NSPopSpace(dblstkelt);
		NSPushSpace(boolstkelt);
		NSP->b.hdr = boolHdr;
		break;
	case 'j':
		/* integer to real */
		if (NSP->l.hdr != longHdr)
			RunError(":operand is not an integer value", iar);
		l = NSP->l.v;
		NSPopSpace(longstkelt);
		NSPushSpace(dblstkelt);
		NSP->d.hdr = dblHdr;
		break;
	case 'k':		/* jn(n, x) */
	case 'l':		/* yn(n, x) */
		/* (integer, real) => real */
		left = &(&(NSP->d))[1];
		if (left->hdr != longHdr)
			RunError(":left operand is not an integer value", iar);
		if (NSP->d.hdr != dblHdr)
			RunError(":right operand is not a real value", iar);
		x = NSP->d.v;
		l = ((struct longstkelt *)left)->v;
		NSPopSpace(longstkelt);  /* pop dbl, pop long, push dbl */
		NSP->d.hdr = dblHdr;
		break;
	}

	/* at this point the operands are in approriate places (x, l, top of stack) and 
		the top stack element is prepared to be the result */
	switch (op) {
	/* binary real operations */
	case '+':
		NSP->d.v = NSP->d.v + x;  break;
	case '-':
		NSP->d.v = NSP->d.v - x;  break;
	case '*':
		NSP->d.v = NSP->d.v * x;  break;
	case '/':
		NSP->d.v = NSP->d.v / x;  break;
	case 'a':
		NSP->d.v = atan2(NSP->d.v, x);  break;
	case 'b':
		NSP->d.v = hypot(NSP->d.v, x);  break;
	case 'x':
		NSP->d.v = pow(NSP->d.v, x);  break;

	/* real to integer */
	case 'c':		/* round(x) */
		NSP->l.v = floor(x+0.5);  break;
	case 'd':
		NSP->l.v = floor(x);  break;
	case 'e':
		NSP->l.v = ceil(x);  break;

	/* real to boolean */
#if !defined(VAX_ENV) && !defined(PMAX_ENV)
#if (! SY_U5x && ! SY_AIXx)
	case 'f':
		NSP->b.v = (isnan(x) == 1) ? TRUE : FALSE;   break;

	case 'g':
#ifdef IBM032_ENV
		NSP->b.v = (finite(x) == 1) ? TRUE : FALSE;   break;
#else /* IBM032_ENV */
		/* isinf() is defined on SUN.  Let's assume it works for all
			other than IBM032, VAX, and HPUX */
		NSP->b.v = (! isinf(x) == 1) ? TRUE : FALSE;   break;
#endif /* IBM032_ENV */

#endif /* (!SYSV && !AIX) */
#endif /* !defined(VAX_ENV) && !defined(PMAX_ENV) */


	/* integer to real */
	case 'j':		/* float(x) */
		NSP->d.v = l;  break;

	/* (integer, real) => real */
	case 'k':
		NSP->d.v = jn(l, x);  break;
	case 'l':
		NSP->d.v = yn(l, x);  break;
	default:
		RunError(":unimplemented operation requested", iar);
	}
}
