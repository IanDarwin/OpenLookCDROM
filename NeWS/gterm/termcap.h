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
 * "@(#)termcap.h 9.4 88/01/19 SMI
 * "@(#)$Header: /it/grass/gterm/RCS/termcap.h,v 2.1 1991/04/23 06:52:50 hugh Grass2 $
 *
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */



enum tct { string, num, bool};

struct tcap {
    char *	t_key;		/* Capability name */
    enum tct	t_type;		/* Capability type */
    int		(*t_op)(/* struct tcap * */);
    int		(*t_in)(/* struct tcap * */);
    char *	t_text;		/* Capability text */
    char *	t_deftext;	/* Default text */
    u_short	t_size;		/* Length of t_text */
    u_short	t_index;	/* Posn. in t_text */
    int		t_param;	/* parameter value for %match */
    u_short	t_matched;	/* Length of matched string */
    u_short	t_x;		/* Coordinate for cm= etc. */
    u_short	t_xi;		/* Offset to subtract from t_x */
    u_short	t_xilim;	/* If t_x >= t_xilim subtract t_xi */
    u_short	t_y;
    u_short	t_yi;
    u_short	t_yilim;
    unsigned	t_pc_r	: 1;	/* %r present */
    unsigned	t_pc_n	: 1;	/* %n present */
    unsigned	t_pc_B	: 1;	/* %B present */
    unsigned	t_pc_D	: 1;	/* %D present */
    unsigned	t_2nd	: 1;	/* On second coord */
};
