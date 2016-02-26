/* $Header: draw.h,v 1.5 88/12/02 10:43:28 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
/*
 * $Header: draw.h,v 1.5 88/12/02 10:43:28 bvs Exp $
 *
 * NewsDraw:  A NeWS based Graphical Object Editor
 * By Bruce Schwartz
 *
 * Thanks to Charlie Burns for figuring out how to get crude
 * objects and inheiritance out of C.  By putting most structure
 * definitions in the .c files, we have truely opaque objects.
 * In item.c/go.h/gopvt.h, we have simple inheritance.
 * Cheesy but effective!
 */
/*
	%
	% This file is a product of Sun Microsystems, Inc. and is provided for
	% unrestricted use provided that this legend is included on all tape
	% media and as a part of the software program in whole or part.
	% Users may copy, modify or distribute this file at will.
	%
	% THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
	% WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
	% PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
	%
	% This file is provided with no support and without any obligation on the
	% part of Sun Microsystems, Inc. to assist in its use, correction,
	% modification or enhancement.
	%
	% SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
	% INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
	% OR ANY PART THEREOF.
	%
	% In no event will Sun Microsystems, Inc. be liable for any lost revenue
	% or profits or other special, indirect and consequential damages, even
	% if Sun has been advised of the possibility of such damages.
	%
	% Sun Microsystems, Inc.
	% 2550 Garcia Avenue
	% Mountain View, California  94043
	%
	% Copyright (C) 1988 by Sun Microsystems. All rights reserved.
*/

#ifndef DRAW_H
#define DRAW_H

#define TRUE 1
#define FALSE 0

/* the modes -- some have graphical objects */
#define STRETCH	0
#define ROTATE	1
#define BRUSH	2
#define LINE	3
#define RECT	4
#define CIRC	5
#define OVAL	6
#define TEXT	7
#define POLY	8
#define MODEMAX	10

/* event handlers */
typedef struct {
	void *(*handleselect)();	/* select button */
	void *(*handleadjust)();	/* adjust button */
	void *(*handlekey)();		/* keystroke     */
	void *(*handleprop)();		/* property list change */
	void *(*handlereply)();		/* server reply  */
	void *(*handledamage)();	/* damage event  */
	void *(*handlebegin)();		/* begin mode    */
	void *(*handleend)();		/* end mode      */
} EHPROCS;

EHPROCS ehprocs[MODEMAX];

#endif
