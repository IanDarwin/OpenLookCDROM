/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.
 * Users may copy, modify or distribute this file at will.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 * Modifications to the original Sun Microsystems, Inc. source code
 * made by the Grasshopper Group are in the Public Domain.
 *
 * Extensions to this file by Eric Messick of the Grasshopper Group.
 *
 * Grasshopper Group
 * 212 Clayton St
 * San Francisco, CA 94117
 *
 */

/*
 * "@(#)screen.h 9.4 88/01/19
 * "@(#)$Header: /it/grass/gterm/RCS/screen.h,v 2.2 1991/04/23 06:53:12 hugh Grass2 $
 *
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */



struct line {
    u_short	length;		/* chars zero through length are meaningful */
    u_short	buffer_length;	/* body has this many chars allocated */
    u_short	changeposition;	/* we've scribbled on stuff right of this pos */
    u_short	end_of_changes;	/* we haven't scribbled past this point */
    int		usedtobe;	/* this line was scrolled from line usedtobe */
    int		flags;		/* attributes of this line */
    char *	body;		/* chars to display */
    u_char *	prop;		/* how to display them (attributes) */
};

/* values for flags */
#define LINE_WRAPPED	1	/* newline at end of line caused by autowrap */

struct line **screen, **lines;

struct pair {
    short	x, y;
};

struct range {
    struct pair	first, last_plus_one;
};

/*
 * Values for struct line -> prop
 */
#define	InsertMode	0x0100
#define AutoMarginMode	0x0200
#define IgnoreNewlineAfterWrapMode 0x0400
#define WrapJustHappenedMode	0x0800
#define	ReverseVideoMode	0x0001
#define	UnderlineMode	0x0002
#define	BoldMode	0x0004
#define	BlinkMode	0x0008
#define StandOutMode	ReverseVideoMode
#define	Attributes	(ReverseVideoMode|UnderlineMode|BoldMode|BlinkMode)

#define PrimSelMode	0x0010
#define SecnSelMode	0x0020
#define ReverseVideoLook	(ReverseVideoMode|PrimSelMode)
#define UnderlineLook		(UnderlineMode|SecnSelMode)
#define MaxCharsPerLine	2048
#define MaxLinesPerScreen 2048

#define bugprintf(fmt, a1, a2, a3) \
	{ \
		char errors[1024]; \
		\
		sprintf(errors, fmt, a1, a2, a3); \
		PopMsg(errors); \
		FlushPostScript(); \
	}
