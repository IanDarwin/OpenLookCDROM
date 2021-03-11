/* -*-C-*-
*******************************************************************************
*
* File:         xlmath2.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlmath2.c,v 2.5 1994/06/06 15:59:21 npm Exp $
* Description:  commonmath - xlisp math functions modified and augmented to
*		correspond more closely to Common Lisp standard. Must define
*		"COMPLX" in order to get this file, else you get xlmath.c.
*		Mods to xlmath.c by Luke Tierney from XLISP-STAT 2.0.
* Author:	David Michael Betz. WINTERP portions by Niels Mayer;
*               XLISP-PLUS by Tom Almy with contributions from Johnny
*               Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*               Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:03 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, Niels Mayer, Luke Tierney and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Enterprise Integration
* Technologies, Hewlett-Packard Company, Niels Mayer, Luke Tierney and David
* Betz make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
* LUKE TIERNEY AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD
* COMPANY, NIELS MAYER, LUKE TIERNEY NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlmath2.c,v 2.5 1994/06/06 15:59:21 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include "xlisp.h"
#include <math.h>

#ifdef COMPLX
/* These definititions used instead of those in XLMATH.C */
/* Somewhat cleaned by by Tom Almy */

/* external variables */
extern LVAL true;

#define IN 0
#define FL 1
#define CI 2
#define CF 3

typedef struct {
  int mode;
  FIXTYPE val, crval, cival;
  FLOTYPE fval, cfrval, cfival;
} Number;

#ifdef WINTERP
#undef Complex			/* "Complex" define'd as "0" in <X11/X.h> */
#endif /* WINTERP */

typedef struct {
  double real, imag;
} Complex;


#ifdef ANSI /* local declarations */
LOCAL void NEAR checkizero(FIXTYPE iarg); /* NPM: changed this to LOCAL */
LOCAL void NEAR checkfzero(FLOTYPE farg); /* NPM: changed this to LOCAL */
LOCAL void NEAR badiop(void);	/* NPM: changed this to LOCAL */
LOCAL void NEAR badfop(void);	/* NPM: changed this to LOCAL */
LOCAL void NEAR badcop(void);	/* NPM: changed this to LOCAL */
LOCAL void NEAR badcomplex(LVAL c); /* NPM: changed this to LOCAL */
LOCAL void NEAR badarg(LVAL arg); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR realpart(LVAL x); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR imagpart(LVAL x); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR makecomplex(LVAL x);	/* NPM: changed this to LOCAL */
LOCAL double NEAR modulus(Complex c); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR cart2complex(double real, double imag); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR cvcomplex(Complex c); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR polar2complex(double mod, double phi); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR csqrt(Complex c); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR cexp(Complex c); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR clog(Complex c); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR cmul(Complex c1, Complex c2); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR cexpt(Complex c1, Complex c2); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR cadd(Complex c1, Complex c2); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR csub(Complex c1, Complex c2); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR cdiv(Complex c1, Complex c2); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR csin(Complex c); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR ccos(Complex c); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR ctan(Complex c); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR casin(Complex c); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR cacos(Complex c); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR catan(Complex c); /* NPM: changed this to LOCAL */
LOCAL Complex NEAR catan2(Complex n, Complex d); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR readnumber(Number *num); /* NPM: changed this to LOCAL */
LOCAL void NEAR setmode(Number *x, int mode); /* NPM: changed this to LOCAL */
LOCAL void NEAR matchmodes(Number *x, Number *y); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR lispnumber(Number *x); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR binary(int which); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR logbinary(int which); /* NPM: changed this to LOCAL */
LOCAL void NEAR get_mod_arg(FLOTYPE *fval, int *mode); /* NPM: changed this to LOCAL */
LOCAL FIXTYPE NEAR xget_gcd(FIXTYPE n, FIXTYPE m); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR unary(int which); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR unary2(int which); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR predicate(int fcn); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR compare(int fcn); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR ccompare(int which); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR xsgetreal(void); /* NPM: changed this to LOCAL */
LOCAL double NEAR logarithm(FLOTYPE x, FLOTYPE base, int base_supplied); /* NPM: changed this to LOCAL */
#endif /* ANSI */


/* Error checking and messages */

/* checkizero - check for integer division by zero */
LOCAL VOID NEAR checkizero(iarg)
  FIXTYPE iarg;
{
  if (iarg == 0)
  xlfail("illegal zero argument");
}

/* checkfzero - check for floating point division by zero or log of zero */
LOCAL VOID NEAR checkfzero(farg)
  FLOTYPE farg;
{
  if (farg == 0.0)
  xlfail("illegal zero argument");
}

/* badiop - bad integer operation */
LOCAL VOID NEAR badiop()
{
  xlfail("bad integer operation");
}

/* badfop - bad floating point operation */
LOCAL VOID NEAR badfop()
{
  xlfail("bad floating point operation");
}

/* badcop - bad complex number operation */
LOCAL VOID NEAR badcop()
{
  xlfail("bad complex number operation");
}

/* badcomplex - bad complex number */
LOCAL VOID NEAR badcomplex(c)
	LVAL c;
{
  xlerror("not a valid complex number", c);
}

/* badarg -- bad argument type */
LOCAL VOID NEAR badarg(arg)
	LVAL arg;
{
  xlerror("bad argument type", arg);
}


/* complex - Complex number functions  */

/*TAA MOD--do inline with libm call*/
#define phase(c) ((c).imag==0.0 && (c).real == 0.0 ? 0 : atan2((c).imag,(c).real))

LOCAL LVAL NEAR realpart(x)
	LVAL x;
{
  if (! complexp(x)) badcomplex(x);
  return(getelement(x, 0));
}

LOCAL LVAL NEAR imagpart(x)
	LVAL x;
{
  if (! complexp(x)) badcomplex(x);
  return(getelement(x, 1));
}

LOCAL Complex NEAR makecomplex(x)
	LVAL x;
{
  Complex c;

  if (numberp(x)) {
    c.real = makefloat(x);
    c.imag = 0.0;
  }
  else if (complexp(x)) {
    c.real = makefloat(realpart(x));
    c.imag = makefloat(imagpart(x));
  }
  else xlerror("not a number", x);
  return(c);
}

LOCAL double NEAR modulus(c)
	Complex c;
{
  return(sqrt(c.real * c.real + c.imag * c.imag));
}

LOCAL Complex NEAR cart2complex(real, imag)
	double real, imag;
{
  Complex val;
  val.real = real;
  val.imag = imag;
  return(val);
}

LOCAL LVAL NEAR cvcomplex(c)
	Complex c;
{
  return(newdcomplex(c.real, c.imag));
}

LOCAL Complex NEAR polar2complex(mod, phi)
	double mod, phi;
{
  Complex val;
  double cs, sn;

  if (phi == 0) {
    cs = 1.0;
    sn = 0.0;
  }
  else if (phi == PI / 2) {
    cs = 0.0;
    sn = 1.0;
  }
  else if (phi == PI) {
    cs = -1.0;
    sn = 0.0;
  }
  else if (phi == -PI / 2) {
    cs = 0.0;
    sn = -1.0;
  }
  else {
    cs = cos(phi);
    sn = sin(phi);
  }
  val.real = mod * cs;
  val.imag = mod * sn;
  return(val);
}

LOCAL Complex NEAR csqrt(c)
	Complex c;
{
  return(polar2complex(sqrt(modulus(c)), phase(c) / 2));
}

LOCAL Complex NEAR cexp(c)
	Complex c;
{
  return(polar2complex(exp(c.real), c.imag));
}

LOCAL Complex NEAR clog(c)
	Complex c;
{
  double mod;

  mod = modulus(c);
  checkfzero(mod);
  return(cart2complex(log(mod), phase(c)));
}

LOCAL Complex NEAR cmul(c1, c2)
	Complex c1, c2;
{
#if 0	/* This is rediculous, says TAA */
	/* why pay the cost for the two conversions? */
  double m1, m2, p1, p2;

  m1 = modulus(c1);
  p1 = phase(c1);
  m2 = modulus(c2);
  p2 = phase(c2);
  return(polar2complex(m1 * m2, p1 + p2));
#else
    Complex val;

    val.real = c1.real * c2.real - c1.imag * c2.imag;
    val.imag = c1.imag * c2.real + c1.real * c2.imag;
    return val;
#endif
}

LOCAL Complex NEAR cexpt(cb, cp)
	Complex cb, cp;
{
  if (modulus(cp) == 0.0) return(cart2complex(1.0, 0.0));
  else	return(cexp(cmul(clog(cb), cp)));
}

LOCAL Complex NEAR cadd(c1, c2)
	Complex c1, c2;
{
  return(cart2complex(c1.real + c2.real, c1.imag + c2.imag));
}

LOCAL Complex NEAR csub(c1, c2)
	Complex c1, c2;
{
  return(cart2complex(c1.real - c2.real, c1.imag - c2.imag));
}

LOCAL Complex NEAR cdiv(c1, c2)
	Complex c1, c2;
{
#if 0
  double m1, m2, p1, p2;

  m1 = modulus(c1);
  p1 = phase(c1);
  m2 = modulus(c2);
  p2 = phase(c2);
  checkfzero(m2);
  return(polar2complex(m1 / m2, p1 - p2));
#else /* replace with code that is faster */
    double ratio, temp;

    if (fabs(c2.real) > fabs(c2.imag)) {
	ratio = c2.imag / c2.real;
	temp = c2.real + ratio*c2.imag;
	return	cart2complex((c1.real + c1.imag*ratio)/temp,
			     (c1.imag - c1.real*ratio)/temp);
    }
    else {
	checkfzero(c2.imag);
	ratio = c2.real / c2.imag;
	temp = c2.imag + ratio*c2.real;
	return	cart2complex ((c1.real*ratio + c1.imag)/temp,
			      (c1.imag*ratio - c1.real)/temp);
    }
#endif
}

LOCAL Complex NEAR csin(c)
	Complex c;
{
  Complex x1, x2, val;

  x1 = cart2complex(-c.imag, c.real);
  x2 = cart2complex(c.imag, -c.real);
  val = csub(cexp(x1), cexp(x2));
  return(cart2complex(val.imag / 2.0, -val.real / 2.0));
}

LOCAL Complex NEAR ccos(c)
	Complex c;
{
  Complex x1, x2, val;

  x1 = cart2complex(-c.imag, c.real);
  x2 = cart2complex(c.imag, -c.real);
  val = cadd(cexp(x1), cexp(x2));
  return(cart2complex(val.real / 2.0, val.imag / 2.0));
}

LOCAL Complex NEAR ctan(c)
	Complex c;
{
  Complex e1, e2, val;

  e1 = cexp(cart2complex(-c.imag, c.real));
  e2 = cexp(cart2complex(c.imag, -c.real));
  val = cdiv(csub(e1, e2), cadd(e1, e2));
  return(cart2complex(val.imag, -val.real));
}

LOCAL Complex NEAR casin(c)
	Complex c;
{
  Complex sx, ix, val;

  sx = cmul(c, c);
  sx = csqrt(cart2complex(1.0 - sx.real, - sx.imag));
  ix = cart2complex(-c.imag, c.real);
  val = clog(cadd(ix, sx));
  return(cart2complex(val.imag, -val.real));
}

LOCAL Complex NEAR cacos(c)
	Complex c;
{
  Complex sx, val;

  sx = cmul(c, c);
  sx = csqrt(cart2complex(1.0 - sx.real, - sx.imag));
  sx = cart2complex(-sx.imag, sx.real);
  val = clog(cadd(c, sx));
  return(cart2complex(val.imag, -val.real));
}

LOCAL Complex NEAR catan(c)
	Complex c;
{
#if 0	/* This has been redefined in Jan 1989 */
  Complex sx, ix, val, one;

  sx = cmul(c, c);
  sx = cart2complex(1.0 + sx.real, sx.imag);
  one = cart2complex(1.0, 0.0);
  sx = csqrt(cdiv(one, sx));
  ix = cadd(one, cart2complex(-c.imag, c.real));
  val = clog(cmul(ix, sx));
  return(cart2complex(val.imag, -val.real));
#else
  Complex sx, ix;

  sx = clog(cart2complex(1.0-c.imag, c.real));
  ix = clog(cart2complex(1.0+c.imag, -c.real));
  sx.real -= ix.real;	/* complex addition w.o. subroutine call */
  sx.imag -= ix.imag;
  return cdiv(sx,cart2complex(0.0, 2.0));
#endif
}

LOCAL Complex NEAR catan2(n,d)	/* by Tom Almy, and a kludge at that */

Complex n, d;
{
    double tmp;

    tmp = modulus(d);
    if (tmp == 0 || modulus(n)/tmp > 1e50 ) { /* worst case */
	tmp = phase(n) - phase(d);
	if (tmp < -PI) tmp += 2.0*PI;
	else if (tmp > PI) tmp -= 2.0*PI;
	return cart2complex(fabs(tmp) > PI/2.0 ? -PI/2.0 : PI/2.0 ,0.0);
    }
    n = cdiv(n,d);  /* best case */
    return (catan(n));
}

/* Helper functions */

LOCAL LVAL NEAR readnumber(num)
	Number *num;
{
  LVAL arg = xlgetarg(), real, imag;

  if (fixp(arg)) {
    num->mode = IN;
    num->val = getfixnum(arg);
  }
  else if (floatp(arg)) {
    num->mode = FL;
    num->fval = getflonum(arg);
  }
  else if (complexp(arg)) {
    real = realpart(arg);
    imag = imagpart(arg);
    if (fixp(real)) {
      num->mode = CI;
      num->crval = getfixnum(real);
      num->cival = getfixnum(imag);
    }
    else {
      num->mode = CF;
      num->cfrval = makefloat(real);
      num->cfival = makefloat(imag);
    }
  }
  else xlerror("not a number", arg);
  return(arg);
}

LOCAL VOID NEAR setmode(x, mode)
	Number *x;
	int mode;
{
  switch (mode) {
  case FL:
    if (x->mode != IN) return;
    x->mode = mode;
    x->fval = x->val;
    break;
  case CI:
    if (x->mode != IN) return;
    x->mode = mode;
    x->crval = x->val;
    x->cival = 0;
    break;
  case CF:
    switch (x->mode) {
    case IN:
      x->mode = mode;
      x->cfrval = x->val;
      x->cfival = 0.0;
      break;
    case FL:
      x->mode = mode;
      x->cfrval = x->fval;
      x->cfival = 0.0;
      break;
    case CI:
      x->mode = mode;
      x->cfrval = x->crval;
      x->cfival = x->cival;
      break;
    }
    break;
  }
}

LOCAL VOID NEAR matchmodes(x, y)
	Number *x, *y;
{
  int mode = x->mode;
  switch (mode) {
  case IN: mode = y->mode; break;
  case FL: if (y->mode == CI || y->mode == CF) mode = CF; break;
  case CI: if (y->mode == FL || y->mode == CF) mode = CF; break;
  case CF: break;
  }
  if (x->mode != mode) setmode(x, mode);
  if (y->mode != mode) setmode(y, mode);
}

LOCAL LVAL NEAR lispnumber(x)
	Number *x;
{
  switch (x->mode) {
  case IN: return(cvfixnum(x->val));
  case FL: return(cvflonum(x->fval));
  case CI: return(newicomplex(x->crval, x->cival));
  case CF: return(newdcomplex(x->cfrval, x->cfival));
  }
  return NIL; /* avoid warning messages */
}

LOCAL LVAL NEAR binary(which)
	int which;
{
  LVAL larg;
  Number val, arg;
  FIXTYPE rtemp, itemp;
  FLOTYPE frtemp, fitemp;

  if (xlargc == 1 && (which == '-' || which == '/')) {
    val.mode = IN;
    switch (which) {
    case '-': val.val = 0; break;
    case '/': val.val = 1; break;
    }
  }
  else larg = readnumber(&val);
  while (moreargs()) {
    larg = readnumber(&arg);
    matchmodes(&val, &arg);
    switch (which) {
    case '+':
      switch (val.mode) {
      case IN: val.val	 += arg.val;  break;
      case FL: val.fval	 += arg.fval; break;
      case CI: val.crval += arg.crval;	 val.cival += arg.cival;   break;
      case CF: val.cfrval += arg.cfrval; val.cfival += arg.cfival; break;
      }
      break;
    case '-':
      switch (val.mode) {
      case IN: val.val	 -= arg.val;  break;
      case FL: val.fval	 -= arg.fval; break;
      case CI: val.crval -= arg.crval;	 val.cival -= arg.cival;   break;
      case CF: val.cfrval -= arg.cfrval; val.cfival -= arg.cfival; break;
      }
      break;
    case '*':
      switch (val.mode) {
      case IN: val.val	 *= arg.val;  break;
      case FL: val.fval	 *= arg.fval; break;
      case CI:
	rtemp = val.crval * arg.crval - val.cival * arg.cival;
	itemp = val.cival * arg.crval + val.crval * arg.cival;
	val.crval = rtemp; val.cival = itemp;
	break;
      case CF:
	frtemp = val.cfrval * arg.cfrval - val.cfival * arg.cfival;
	fitemp = val.cfival * arg.cfrval + val.cfrval * arg.cfival;
	val.cfrval = frtemp; val.cfival = fitemp;
	break;
      }
      break;
    case '/':
      switch (val.mode) {
      case IN:
	checkizero(arg.val);
	if (val.val % arg.val == 0) {
	  val.val /= arg.val;
	  break;
	}
	else {
	  setmode(&val, FL);
	  setmode(&arg, FL);
	}
	/* drop through */
      case FL:
	checkfzero(arg.fval);
	val.fval /= arg.fval;
	break;
      case CI:
	setmode(&val, CF);
	setmode(&arg, CF);
	/* drop through */
      case CF:
#if 0	/* we can do better */
    { double magn;
	magn = arg.cfrval * arg.cfrval + arg.cfival * arg.cfival;
	checkfzero(magn);
	frtemp = (val.cfrval * arg.cfrval + val.cfival * arg.cfival) / magn;
	fitemp = (val.cfival * arg.cfrval - val.cfrval * arg.cfival) / magn;
	val.cfrval = frtemp; val.cfival = fitemp;
	break;
    }
#else
    { double ratio,temp;
	if (fabs(arg.cfrval) > fabs(arg.cfival)) {
	    ratio = arg.cfival / arg.cfrval;
	    temp = arg.cfrval + ratio*arg.cfival;
	    frtemp = (val.cfrval + val.cfival*ratio)/temp;
	    fitemp = (val.cfival - val.cfrval*ratio)/temp;
	}
	else {
	    checkfzero(arg.cfival);
	    ratio = arg.cfrval / arg.cfival;
	    temp = arg.cfival + ratio*arg.cfrval;
	    frtemp = (val.cfrval*ratio + val.cfival)/temp;
	    fitemp = (val.cfival*ratio - val.cfrval)/temp;
	}
	val.cfrval = frtemp; val.cfival = fitemp;
	break;
    }
#endif
      }
      break;
    case 'M':
      switch (val.mode) {
      case IN: val.val	= (val.val > arg.val)	? val.val  : arg.val;  break;
      case FL: val.fval = (val.fval > arg.fval) ? val.fval : arg.fval; break;
      default: badarg(larg);
      }
      break;
    case 'm':
      switch (val.mode) {
      case IN: val.val	= (val.val < arg.val)	? val.val  : arg.val;  break;
      case FL: val.fval = (val.fval < arg.fval) ? val.fval : arg.fval; break;
      default: badarg(larg);
      }
      break;
    }
  }
  return(lispnumber(&val));
}


/* This has been completely rewritten by Tom Almy to handle floating point
   arguments */

LVAL xrem()
{
    Number num, div;
    double ftemp;

    readnumber(&num);
    readnumber(&div);
    xllastarg();

    matchmodes(&num, &div);

    switch (num.mode) {
    case IN:	checkizero(div.val);
		return (cvfixnum((FIXTYPE) num.val % div.val));
    case FL:	checkfzero(div.fval);
		modf(num.fval / div.fval, &ftemp);
		return (cvflonum((FLOTYPE)(num.fval - ftemp*div.fval)));
    }
    badcop();
    return NIL; /* fool compiler into not giving warning */
}



LOCAL LVAL NEAR logbinary(which)
	int which;
{
  FIXTYPE val, arg; /* TAA Mod -- was int */

  switch (which) {
  case '&': val = -1; break;
  case '|': val =  0; break;
  case '^': val =  0; break;
  }
  while (moreargs()) {
    arg = getfixnum(xlgafixnum());
    switch (which) {
    case '&': val &= arg; break;
    case '|': val |= arg; break;
    case '^': val ^= arg; break;
    }
  }
  return(cvfixnum((FIXTYPE) val));
}

/* binary functions */
/* TAA fix allowing (+) */
LVAL xadd()    { return (moreargs()?binary('+'):cvfixnum((FIXTYPE)0)); }
LVAL xsub()    { return (binary('-')); } /* - */
/* TAA fix allowing (*) */
LVAL xmul()    { return (moreargs()?binary('*'):cvfixnum((FIXTYPE)1)); }
LVAL xdiv()    { return (binary('/')); } /* / */
LVAL xmin()    { return (binary('m')); } /* min */
LVAL xmax()    { return (binary('M')); } /* max */
LVAL xlogand() { return (logbinary('&')); } /* logand */
LVAL xlogior() { return (logbinary('|')); } /* logior */
LVAL xlogxor() { return (logbinary('^')); } /* logxor */

LOCAL VOID NEAR get_mod_arg(fval, mode)
	FLOTYPE *fval;
	int *mode;
{
  LVAL arg;

  arg = xlgetarg();
  if (fixp(arg)) {
    *fval = getfixnum(arg);
    *mode = IN;
  }
  else if (floatp(arg)) {
    *fval = getflonum(arg);
    *mode = FL;
  }
  else xlerror("bad argument type", arg);
}


LVAL xmod()
{
  int mode1, mode2;
  FLOTYPE fval1, fval2, fres;

  get_mod_arg(&fval1, &mode1);
  get_mod_arg(&fval2, &mode2);
  xllastarg();

  fres = fval1 - fval2 * floor(fval1 / fval2);
  return((mode1 == IN && mode2 == IN) ? cvfixnum((FIXTYPE) fres)
				      : cvflonum((FLOTYPE) fres));
}


LVAL xexpt()
{
  LVAL base, power;
  int bsign, psign;
  FIXTYPE b, p, val;
  FLOTYPE fb, fp, fval;

  base = xlgetarg();
  power = xlgetarg();
  xllastarg();

  if (fixp(base) && fixp(power)) {
    b = getfixnum(base);
    p = getfixnum(power);
    if (p == 0) return(cvfixnum((FIXTYPE) 1));
    if (b == 0 && p > 0) return(cvfixnum((FIXTYPE) 0));
    checkizero(b);
    bsign = (b > 0) ? 1 : -1;
    psign = (p > 0) ? 1 : -1;
    b = (b > 0) ? b : -b;
    p = (p > 0) ? p : -p;
    fval = floor(pow((double) b, (double) p) + 0.1); /* to get integer right */
    if (bsign == -1 && p % 2 == 1) fval = -fval;
    if (psign == 1) {
      val = fval;
      if (val == fval) return(cvfixnum((FIXTYPE) val));
      else return(cvflonum((FLOTYPE) fval));	/* to handle precision for large results */
    }
    else {
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
    }
  }
  else if (floatp(base) && fixp(power)) {
    fb = getflonum(base);
    p = getfixnum(power);
    if (p == 0) return(cvflonum((FLOTYPE) 1.0)); /* TAA MOD - used to return
				    fixnum 1, but CL says should be flonum */
    if (fb == 0.0 && p > 0) return(cvflonum((FLOTYPE) 0.0));
    checkfzero(fb);
    bsign = (fb > 0) ? 1 : -1;
    psign = (p > 0) ? 1 : -1;
    fb = (fb > 0) ? fb : -fb;
    p = (p > 0) ? p : -p;
    fval = pow((double) fb, (double) p);
    if (bsign == -1 && p % 2 == 1) fval = -fval;
    if (psign == 1) return(cvflonum((FLOTYPE) fval));
    else {
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
    }
  }
  else if ((fixp(base) || floatp(base)) && floatp(power)) {
    fb = makefloat(base);
    fp = getflonum(power);
    if (fp == 0.0) return(cvflonum((FLOTYPE) 1.0));
    if (fb == 0.0 && fp > 0.0) return(cvflonum((FLOTYPE) 0.0));
    if (fb < 0.0)
      return(cvcomplex(cexpt(makecomplex(base), makecomplex(power))));
    psign = (fp > 0) ? 1 : -1;
    fb = (fb > 0) ? fb : -fb;
    fp = (fp > 0) ? fp : -fp;
    fval = pow((double) fb, (double) fp);
    if (psign == 1) return(cvflonum((FLOTYPE) fval));
    else {
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
    }
  }
  else if (complexp(base) || complexp(power))
    return(cvcomplex(cexpt(makecomplex(base), makecomplex(power))));
  else xlfail("bad argument type(s)");
  return NIL; /* avoid compiler warnings */
}

/* arc tangent -- Tom Almy */
LVAL xatan()
{
    Number numer, denom;
    LVAL lnum, ldenom;
    Complex cnum, cdenom;

    lnum = readnumber(&numer);

    if (moreargs()) {
	ldenom = readnumber(&denom);
	xllastarg();
	matchmodes(&numer, &denom);

	switch (numer.mode) {
	    case IN:
		numer.fval = numer.val; denom.fval = denom.val;
	    case FL:
		return (cvflonum((FLOTYPE)atan2(numer.fval, denom.fval)));
	    default: /* complex */
		cnum = makecomplex(lnum);
		cdenom = makecomplex(ldenom);
		return (cvcomplex(catan2(cnum,cdenom)));
	}
    }
    else {
	switch (numer.mode) {
	    case IN:
		numer.fval = numer.val;
	    case FL:
		return (cvflonum((FLOTYPE)atan(numer.fval)));
	    default: /* complex */
		cnum = makecomplex(lnum);
		return (cvcomplex(catan(cnum)));
	}
    }
}



/* two argument logarithm */
LOCAL double NEAR logarithm(x, base, base_supplied)
     FLOTYPE x, base;
     int base_supplied;
{
  double lbase;
  if (x <= 0.0) xlfail("logarithm of a nonpositive number");
  if (base_supplied) {
    if (base <= 0.0) xlfail("logarithm to a nonpositive base");
    else {
      lbase = log(base);
      if (lbase == 0.0) xlfail("logarith to a unit base");
      else return((log(x)/lbase));
    }
  }
  return (log(x));
}

LVAL xlog()
{
  LVAL arg, base;
  int base_supplied = FALSE;
  double fx, fb;

  arg = xlgetarg();
  if (moreargs()) {
    base_supplied = TRUE;
    base = xlgetarg();
  }
  if (base_supplied) {
    if (numberp(arg) && numberp(base)) {
      fx = makefloat(arg);
      fb = makefloat(base);
      if (fx <= 0.0 || fb <= 0.0)
	return(cvcomplex(cdiv(clog(makecomplex(arg)), clog(makecomplex(base)))));
      else return(cvflonum((FLOTYPE) logarithm(fx, fb, TRUE)));
    }
    else if ((numberp(arg) && complexp(base))
	     || (complexp(arg) && numberp(base))
	     || (complexp(arg) && complexp(base)))
      return(cvcomplex(cdiv(clog(makecomplex(arg)), clog(makecomplex(base)))));
    else xlfail("bad argument type(s)");
  }
  else {
    if (numberp(arg)) {
      fx = makefloat(arg);
      if (fx <= 0.0) return(cvcomplex(clog(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) logarithm(fx, 0.0, FALSE)));
    }
    else if (complexp(arg))
      return(cvcomplex(clog(makecomplex(arg))));
    else xlfail("bad argument type(s)");
  }
  return NIL; /* avoid compiler warnings */
}

/* TAA Mod to return FIXTYPE */
LOCAL FIXTYPE NEAR xget_gcd(n,m)		 /* euclid's algorith */
FIXTYPE n, m;
{
   FIXTYPE r;

   for (;;) {
      r = m % n;
      if (r == (FIXTYPE) 0)
	break;
      m = n;
      n = r;
   }

   return (n);
}

/* xgcd - greatest common divisor */
LVAL xgcd()
{
  FIXTYPE m,n;
  LVAL arg;

  if (!moreargs())		    /* check for identity case */
    return (cvfixnum((FIXTYPE)0));
  arg = xlgafixnum();
  n = getfixnum(arg);
  if (n < (FIXTYPE)0) n = -n;		/* absolute value */
  while (moreargs()) {
    arg = xlgafixnum();
    m = getfixnum(arg);
    if (m == 0 || n == 0) xlfail("zero argument");
    if (m < (FIXTYPE)0) m = -m;	    /* absolute value */

    n = xget_gcd(n,m);
  }
  return (cvfixnum(n));
}

LVAL xlcm()			    /* added by kcw */
{
  LVAL arg;
  FIXTYPE n, m, t, g;

  arg = xlgafixnum();
  n = getfixnum(arg);
  if (!moreargs())  {
     if (n < (FIXTYPE) 0) n = -n;
     return (cvfixnum(n));
  }

  while (moreargs())  {
     arg = xlgafixnum();
     m = getfixnum(arg);
     if ((n == (FIXTYPE) 0) || (m == (FIXTYPE) 0))
	return(cvfixnum(0));

     t = n * m;
     g = xget_gcd(n,m);

     n = (FIXTYPE) t / g;
  }

  if (n < (FIXTYPE) 0) n = -n;

  return (cvfixnum(n));
}

#ifndef RANDOM
LOCAL long rseed=1L;
#endif

/* unary - handle unary operations */
LOCAL LVAL NEAR unary(which)
	int which;
{
  FLOTYPE fval;
  FIXTYPE ival;
  Complex cval;
  LVAL arg, real, imag;
  int mode;

  /* get the argument */
  arg = xlgetarg();
  if (which == 'F' && moreargs()) { /*TAA MOD to eliminate compiler warnings*/
      if (floatp(*xlargv)) xlargc--;
      else xlbadtype(*xlargv);
  }
  xllastarg();

  /* check its type */
  if (fixp(arg)) {
    ival = getfixnum(arg);
    mode = IN;
  }
  else if (floatp(arg)) {
    fval = getflonum(arg);
    mode = FL;
  }
  else if (complexp(arg)) {
    cval = makecomplex(arg);
    real = realpart(arg);
    imag = imagpart(arg);
    if (fixp(realpart(arg))) mode = CI;
    else mode = CF;
  }
  else xlerror("not a number", arg);

  switch (which) {
  case '~':
    if (mode == IN) return(cvfixnum((FIXTYPE) ~ival));
    else badiop();
    break;
  case 'A':
    switch (mode) {
    case IN: return(cvfixnum((FIXTYPE) (ival < 0   ? -ival : ival)));
    case FL: return(cvflonum((FLOTYPE) (fval < 0.0 ? -fval : fval)));
    case CI:
    case CF: return(cvflonum((FLOTYPE) modulus(cval)));
    }
    break;
  case '+':
    switch (mode) {
    case IN: return(cvfixnum((FIXTYPE) ival + 1));
    case FL: return(cvflonum((FLOTYPE) fval + 1.0));
    case CI: return(newicomplex(getfixnum(real) + 1, getfixnum(imag)));
    case CF: return(newdcomplex(getflonum(real) + 1.0, getflonum(imag)));
    }
    break;
  case '-':
    switch (mode) {
    case IN: return(cvfixnum((FIXTYPE) ival - 1));
    case FL: return(cvflonum((FLOTYPE) fval - 1.0));
    case CI: return(newicomplex(getfixnum(real) - 1, getfixnum(imag)));
    case CF: return(newdcomplex(getflonum(real) - 1.0, getflonum(imag)));
    }
    break;
  case 'S':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) sin((double) ival)));
    case FL: return(cvflonum((FLOTYPE) sin((double) fval)));
    case CI:
    case CF: return(cvcomplex(csin(cval)));
    }
  case 'C':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) cos((double) ival)));
    case FL: return(cvflonum((FLOTYPE) cos((double) fval)));
    case CI:
    case CF: return(cvcomplex(ccos(cval)));
    }
  case 'T':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) tan((double) ival)));
    case FL: return(cvflonum((FLOTYPE) tan((double) fval)));
    case CI:
    case CF: return(cvcomplex(ctan(cval)));
    }
  case 'E':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) exp((double) ival)));
    case FL: return(cvflonum((FLOTYPE) exp((double) fval)));
	case CI:
	case CF: return(cvcomplex(cexp(cval)));
	}
	break;
  case 'R':
    switch (mode) {
    case IN:
      if (ival < 0) return(cvcomplex(csqrt(makecomplex(arg))));
	  else return(cvflonum((FLOTYPE) sqrt((double) ival)));
	case FL:
      if (fval < 0) return(cvcomplex(csqrt(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) sqrt(fval)));
    case CI:
    case CF: return(cvcomplex(csqrt(cval)));
    }
    break;
  case 'F':
    switch (mode) {
    case IN: return (cvflonum((FLOTYPE) ival));
    case FL: return (cvflonum((FLOTYPE) fval));
	default: badcop();
	}
	break;
#ifndef RANDOM
  case '?':
    switch (mode) {
    case IN: return (cvfixnum((FIXTYPE)(rseed=osrand(rseed)) % ival));
    case FL: badfop();
	default: badcop();
	}
	break;
#endif
  case 's':
    switch (mode) {
    case IN:
      fval = ival;
      /* drop through */
    case FL:
      if (fval > 1.0 || fval < -1.0)
	return(cvcomplex(casin(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) asin(fval)));
	case CI:
	case CF: return(cvcomplex(casin(cval)));
	}
	break;
  case 'c':
    switch (mode) {
    case IN:
      fval = ival;
      /* drop through */
    case FL:
      if (fval > 1.0 || fval < -1.0)
	return(cvcomplex(cacos(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) acos(fval)));
	case CI:
	case CF: return(cvcomplex(cacos(cval)));
	}
	break;
  case 'P':
    switch (mode) {
	case IN: return(cvflonum((FLOTYPE) (ival >= 0) ? 0.0 : PI));
	case FL: return(cvflonum((FLOTYPE) (fval >= 0.0) ? 0.0 : PI));
	case CI:
	case CF: return(cvflonum((FLOTYPE) phase(cval)));
	}
	break;
  default: xlfail("unsupported operation");
  }
	return NIL; /* avoid compiler warning */
}

LOCAL LVAL NEAR unary2(which)	/* handle truncate, floor, ceiling, and round */
			    /* 1 or two arguments */
			    /* By Tom Almy */
int which;
{
    Number numer, denom;
    LVAL lval;

    lval = readnumber(&numer);

    if (moreargs()) {	/* two argument version */
	readnumber(&denom);
	xllastarg();
	matchmodes(&numer, &denom);

	switch (numer.mode) {
	    case IN:
		checkizero(denom.val);
		numer.fval = numer.val / (double)denom.val;
		break;
	    case FL:
		checkfzero(denom.fval);
		numer.fval /= denom.fval;
		break;
	    default: badcop();
	}
    }
    else { /* single argument version */
	switch (numer.mode) {
	    case IN: return lval;   /* no-operation */
	    case FL: break;	    /* continue */
	    default: badcop();
	}
    }
    switch (which)  { /* now do it! */
	case 'I': modf(numer.fval,&numer.fval); break;
	case '_': numer.fval = floor(numer.fval); break;
	case '^': numer.fval = ceil(numer.fval); break;
	case 'r': numer.fval = floor(numer.fval + 0.5); break;
    }
    if (fabs(numer.fval) > (double)MAXFIX)
	return cvflonum((FLOTYPE)numer.fval);
    return cvfixnum((FIXTYPE)numer.fval);
}

/* unary functions */
LVAL xlognot() { return (unary('~')); } /* lognot */
LVAL xabs()    { return (unary('A')); } /* abs */
LVAL xadd1()   { return (unary('+')); } /* 1+ */
LVAL xsub1()   { return (unary('-')); } /* 1- */
LVAL xsin()    { return (unary('S')); } /* sin */
LVAL xcos()    { return (unary('C')); } /* cos */
LVAL xtan()    { return (unary('T')); } /* tan */
LVAL xexp()    { return (unary('E')); } /* exp */
LVAL xsqrt()   { return (unary('R')); } /* sqrt */
LVAL xfloat()  { return (unary('F')); } /* float */
#ifndef RANDOM
LVAL xrand()   { return (unary('?')); } /* random */
#endif
LVAL xasin()   { return (unary('s')); } /* asin */
LVAL xacos()   { return (unary('c')); } /* acos */
LVAL xphase()  { return (unary('P')); } /* phase */
LVAL xfix()    { return (unary2('I')); } /* truncate */
LVAL xfloor()  { return (unary2('_')); } /* floor */
LVAL xceil()   { return (unary2('^')); } /* ceiling */
LVAL xround()  { return (unary2('r')); } /* round */


/* predicate - handle a predicate function */
LOCAL LVAL NEAR predicate(fcn)
  int fcn;
{
  FLOTYPE fval;
  FIXTYPE ival;
  LVAL arg;

  /* get the argument */
  arg = xlgetarg();
  xllastarg();

  /* check the argument type */
  if (fixp(arg)) {
    ival = getfixnum(arg);
    switch (fcn) {
    case '-': ival = (ival < 0); break;
    case 'Z': ival = (ival == 0); break;
    case '+': ival = (ival > 0); break;
    case 'E': ival = ((ival & 1) == 0); break;
    case 'O': ival = ((ival & 1) != 0); break;
    default:  badiop();
    }
  }
  else if (floatp(arg)) {
    fval = getflonum(arg);
    switch (fcn) {
    case '-': ival = (fval < 0); break;
    case 'Z': ival = (fval == 0); break;
    case '+': ival = (fval > 0); break;
    default:  badfop();
    }
  }
  else
    xlerror("bad argument type",arg);

  /* return the result value */
  return (ival ? true : NIL);
}

/* unary predicates */
LVAL xminusp() { return (predicate('-')); } /* minusp */
LVAL xzerop()  { return (predicate('Z')); } /* zerop */
LVAL xplusp()  { return (predicate('+')); } /* plusp */
LVAL xevenp()  { return (predicate('E')); } /* evenp */
LVAL xoddp()   { return (predicate('O')); } /* oddp */


/* compare - common compare function */
LOCAL LVAL NEAR compare(fcn)
  int fcn;
{
    FIXTYPE icmp,ival,iarg;
    FLOTYPE fcmp,fval,farg;
    LVAL arg;
    int mode;

    /* get the first argument */
    arg = xlgetarg();

    /* set the type of the first argument */
    if (fixp(arg)) {
	ival = getfixnum(arg);
	mode = 'I';
    }
    else if (floatp(arg)) {
	fval = getflonum(arg);
	mode = 'F';
    }
    else
	xlerror("bad argument type",arg);

    /* handle each remaining argument */
    for (icmp = TRUE; icmp && moreargs(); ival = iarg, fval = farg) {

	/* get the next argument */
	arg = xlgetarg();

	/* check its type */
	if (fixp(arg)) {
	    switch (mode) {
	    case 'I':
		iarg = getfixnum(arg);
		break;
	    case 'F':
		farg = (FLOTYPE)getfixnum(arg);
		break;
	    }
	}
	else if (floatp(arg)) {
	    switch (mode) {
	    case 'I':
		fval = (FLOTYPE)ival;
		farg = getflonum(arg);
		mode = 'F';
		break;
	    case 'F':
		farg = getflonum(arg);
		break;
	    }
	}
	else
	    xlerror("bad argument type",arg);

	/* compute result of the compare */
	switch (mode) {
	case 'I':
	    icmp = ival - iarg;
	    switch (fcn) {
	    case '<':	icmp = (icmp < 0); break;
	    case 'L':	icmp = (icmp <= 0); break;
	    case '=':	icmp = (icmp == 0); break;
	    case '#':	icmp = (icmp != 0); break;
	    case 'G':	icmp = (icmp >= 0); break;
	    case '>':	icmp = (icmp > 0); break;
	    }
	    break;
	case 'F':
	    fcmp = fval - farg;
	    switch (fcn) {
	    case '<':	icmp = (fcmp < 0.0); break;
	    case 'L':	icmp = (fcmp <= 0.0); break;
	    case '=':	icmp = (fcmp == 0.0); break;
	    case '#':	icmp = (fcmp != 0.0); break;
	    case 'G':	icmp = (fcmp >= 0.0); break;
	    case '>':	icmp = (fcmp > 0.0); break;
	    }
	    break;
	}
    }

    /* return the result */
    return (icmp ? true : NIL);
}

LOCAL LVAL NEAR ccompare(which)
	int which;
{
  Number val, arg;
  int icmp;

  switch (which) {
  case '=': icmp = TRUE;  break;
  case '#': icmp = FALSE; break;
  }
  /*larg =*/ readnumber(&val);
  while (moreargs()) {
    /*larg =*/ readnumber(&arg);
    matchmodes(&val, &arg);
    switch (which) {
    case '=':
      switch (val.mode) {
      case IN: icmp = icmp && val.val  == arg.val;  break;
      case FL: icmp = icmp && val.fval == arg.fval; break;
      case CI: icmp = icmp && val.crval == arg.crval && val.cival == arg.cival; break;
      case CF: icmp = icmp && val.cfrval == arg.cfrval && val.cfival == arg.cfival; break;
      }
      break;
    case '#':
      switch (val.mode) {
      case IN: icmp = icmp || val.val  != arg.val;  break;
      case FL: icmp = icmp || val.fval != arg.fval; break;
      case CI: icmp = icmp || val.crval != arg.crval || val.cival != arg.cival; break;
      case CF: icmp = icmp || val.cfrval != arg.cfrval || val.cfival != arg.cfival; break;
      }
      break;
    }
  }
  return((icmp) ? true : NIL);
}

/* comparison functions */
LVAL xlss() { return (compare('<'));  } /* < */
LVAL xleq() { return (compare('L'));  } /* <= */
LVAL xequ() { return (ccompare('=')); } /* = */
LVAL xneq() { return (ccompare('#')); } /* /= */
LVAL xgeq() { return (compare('G'));  } /* >= */
LVAL xgtr() { return (compare('>'));  } /* > */


/***********************************************************************/
/**								      **/
/**			Complex Number Functions		      **/
/**								      **/
/***********************************************************************/

LOCAL LVAL NEAR xsgetreal()
{
  LVAL arg = xlgetarg();
  if (! numberp(arg)) xlerror("not a real number", arg);
  return(arg);
}

LVAL xcomplex() /* TAA MOD so (complex 12.0) => #c(12.0 0.0) as required
		    by CL. */
{
  LVAL real, imag;

  real = xsgetreal();
  if (! moreargs())
      return (ntype(real)==FIXNUM ? real :
				    newcomplex(real,cvflonum((FLOTYPE)0.0)));
  else {
    imag = xsgetreal();
    return(newcomplex(real, imag));
  }
}

LVAL xconjugate()
{
  LVAL arg = xlgetarg();

  xllastarg();
  if (numberp(arg)) return(arg);
  if (fixp(realpart(arg)) && fixp(imagpart(arg)))
    return(newicomplex(getfixnum(realpart(arg)), -getfixnum(imagpart(arg))));
  else
    return(newdcomplex(makefloat(realpart(arg)), -makefloat(imagpart(arg))));
}

LVAL xrealpart()
{
  LVAL arg = xlgetarg();

  xllastarg();
  if (fixp(arg) || floatp(arg)) return(arg);
  else return(realpart(arg));
}

LVAL ximagpart()
{
  LVAL arg = xlgetarg();

  xllastarg();
  if (fixp(arg)) return(cvfixnum((FIXTYPE) 0));
  else if (floatp(arg)) return(cvflonum((FLOTYPE) 0.0));
  else return(imagpart(arg));
}

#endif /* COMPLX */
