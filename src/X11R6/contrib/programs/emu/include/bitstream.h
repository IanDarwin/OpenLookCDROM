#ifndef	_BITSTREAM_H_INCLUDE
#define	_BITSTREAM_H_INCLUDE

/* bitstream.h,v 1.3 1994/06/02 20:06:39 me Exp */

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * This code is derived from software contributed by
 * Paul Vixie.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/*
 * bitstring.h - bit string manipulation macros
 *
 * vix 26feb87 [written]
 * vix 03mar87 [fixed stupid bug in setall/clearall]
 * vix 25mar87 [last-minute cleanup before mod.sources gets it]
 *
 * bitstream.h,v
 * Revision 1.3  1994/06/02  20:06:39  me
 * Like I said, new copyright
 *
 *
 * Revision 1.1.1.1  1994/05/22  11:22:39  me
 * Initial import into CVS
 */

/*
 * there is something like this in 4.3, but that's licensed source code that
 * I'd rather not depend on, so I'll reinvent the wheel (incompatibly).
 */

/*
 * except for the number of bits per int, and the other constants, this should
 * port painlessly just about anywhere.  please #ifdef any changes with your
 * compiler-induced constants (check the CC man page, it'll be something like
 * 'vax' or 'mc68000' or 'sun' or some such).  also please mail any changes
 * back to me (ucbvax!dual!ptsfa!vixie!paul) for inclusion in future releases.
 */

/* (macros used internally) */

/* how many int's in a string of N bits? */
#define	_bit_size(N) (((N) / _bit_intsiz) + (((N) % _bit_intsiz) ? 1 : 0))

/* which int of the string is bit N in?	 */
#define	_bit_intn(N) ((N) / _bit_intsiz)

/* mask for bit N in it's int */
#define	_bit_mask(N) (1 << ((N) % _bit_intsiz))

/* (macros used externally) */

/* declare (create) Name as a string of N bits */
#define	bit_decl(Name, N)	_bit_type Name[_bit_size(N)]

/* declare (reference) Name as a bit string */
#define	bit_ref(Name)		_bit_type *Name

/*
 * allocate a dynamic bitstring, thanks to:
 *	lef@nlm.nih.gov.arpa
 *	L Fitzpatrick
 *	National Library of Medicine
 *	Bethesda, MD 10894
 */
#define bit_new(N) \
     (_bit_type *) malloc((unsigned)_bit_size(N) * sizeof(_bit_type));

/* is bit N of string Name set? */
#define	bit_test(Name, N)	((Name)[_bit_intn(N)] & _bit_mask(N))

/* set bit N of string Name */
#define	bit_set(Name, N)	{ (Name)[_bit_intn(N)] |= _bit_mask(N); }

/* clear bit N of string Name */
#define	bit_clear(Name, N)	{ (Name)[_bit_intn(N)] &= ~_bit_mask(N); }

/* set bits 0..N in string Name */
#define	bit_setall(Name, N) \
	{	register _bit_i; \
		for (_bit_i = _bit_size(N)-1; _bit_i >= 0; _bit_i--) \
			Name[_bit_i]=_bit_1s; \
	}

/* clear bits 0..N in string Name */
#define	bit_clearall(Name, N) \
	{	register _bit_i; \
		for (_bit_i = _bit_size(N)-1; _bit_i >= 0; _bit_i--) \
			Name[_bit_i]=_bit_0s; \
	}

#endif	/* _BITSTREAM_H_INCLUDE */
