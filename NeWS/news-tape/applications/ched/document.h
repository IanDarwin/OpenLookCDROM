/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
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
 */

/* %Z%%M% %I% %E% SMI	 */

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

/*-
	data types for document representation

	document.h, Sun Apr 19 11:39:23 1987

		James Gosling,
		Sun Microsystems
 */

#ifndef FILE
#include <stdio.h>
#endif

/* A string of bytes.  It supports insertion, deletion and reference */
struct bytestring {
    char       *bytes;		/* The beginning of the buffer */
    int         bufsize;	/* The number of bytes in the buffer */
    int         size;		/* the number of bytes in the string
				 * (bufsize-size is the number of spare bytes) */
    int         firstsize;	/* The number of bytes in the first part. The
				 * buffer is split into two parts, firstsize
				 * bytes at the beginning of the buffer,
				 * size-firstsize at the end, with
				 * bufsize-size spare in the middle */
    char       *secondpart;	/* == bytes+bufsize-size, to speed up
				 * references in the second part */
};

/* Get byte n from bytestring b */
#define GetByte(b, n) (*((n) < (b)->firstsize ? (b)->bytes+(n) : (b)->secondpart+(n)))

/*
 * A marker for a place in a bytestring.  A marker marks a position a span
 * that starts from there
 */
struct marker {
    struct marker *next,
               *prev;		/* the marker chain */
    struct document *document;	/* the marked document */
    int         pos;		/* the starting byte position */
    int         span;		/* the number of bytes marked */
    unsigned int modified:1;	/* true if some text in the spanned region was
				 * modified */
    unsigned int posmodified:1;	/* true iff the position of the marker was
				 * modified, but not its contents */
    unsigned int rightside:1;	/* If bytes are inserted exactly at the
				 * marker, and rightside!=0, the marker stays
				 * on the right side of them */
};

struct bytestring *bs_create();
struct marker *mk_create( /* buffer, pos, span */ );

#define mk_movemark(m, POS, SPAN) ((m)->pos = (POS), (m)->span = (SPAN))
#define mk_resetmark(m) ((m)->modified = 0, (m)->posmodified = 0)

#ifndef DEBUG
#define assert(b) 0
#else
#define assert(b) (!(b) ? (fprintf(stderr, "Assertion failed: b, at line %d in %s\n", __LINE__, __FILE__),abort(0)) : 0)
#endif

/* A reference to a style from a document */
struct styleref {		/* BOGUS! */
    int         pos;		/* The position in the document of the
				 * reference */
    int         span;		/* The size of the reference */
    short       parent;		/* The smallest styleref that encloses this
				 * one */
    struct style *style;	/* The style referred to */
};

struct style {
    struct style *next;		/* The rest of this style */
    short       which;		/* What in the formatter paramater state this
				 * modifies */
    short       how;		/* How it modifies that state (M_REPLACE,
				 * M_ADD, M_OR) */
    int         value;		/* The value used in the modification */
};

/* A few global styles */
struct style style_bold;
struct style style_italic;
struct style style_superscript;
struct style style_subscript;
struct style style_smaller;
struct style style_larger;

/* A snapshot of the values of the various formatter parameters */
struct formatter_info {
    int         parameters[10];
};

/* Style modification types */
#define M_REPLACE 0
#define M_ADD 1
#define M_OR 2
#define M_ADDPSREL 3

/* Well known formatter parameters */
#define F_TYPE 0		/* Object type */
#define F_FONT 1
#define F_FACECODE 2
#define F_POINTSIZE 3
#define F_BASELINE 4
#define F_LEFTMARGIN 5
#define F_RIGHTMARGIN 6
#define F_FIRSTMARGIN 7
#define F_WIDTH 8		/* set iff the object has a fixed size */
#define F_HEIGHT 9


/* Well known object types */
#define FT_TEXT 0
#define FT_ILLUSTRATION 1

/* Well known facecodes */
#define FC_BOLD 1
#define FC_ITALIC 2
#define FC_OBLIQUE 4
#define FC_SHADOW 8




/* A singing & dancing document */
struct document {
    struct bytestring *data;	/* The data data part of the document */
    struct bytestring *stylerefs;	/* The array of style sheet references */
    short       nstylerefs;	/* the number of style references, ==
				 * stylerefs.size/sizeof(struct styleref) */
    unsigned    modified:1;	/* true iff this document has been modified */
    short       firstshifted;	/* First style whose "pos" needs to be shifted */
    long       shiftdistance;	/* The distance by which it and all further style
				 * refs need to be shifted */
    struct marker *marks;	/* The markers that refer to this document */
    struct marker *caret;	/* The caret/selection for this document */
    struct docview *views;	/* The views on this document */
};

#define d_setcaret(d, pos, span) mk_movemark((d)->caret, pos, span)
#define d_caret(d) ((d)->caret->pos)
#define d_styleref(d,n) ((struct styleref *)&GetByte((d)->stylerefs,n*sizeof(struct styleref)))
#define d_validatestylepos(d,n) (n>=d->firstshifted ? applyshift(d, n) : 0)
struct document *d_create();


struct spoint {			/* a coordinate pair */
    short       x,
                y;
};

typedef long fixed;		/* 32 bit fixed point number with 16 bits of
				 * fraction */
#define fixedi(i) (fixed)((i)*(1<<16))	/* convert int to fixed */
#define floorfr(fr) ((fr)>>16)	/* convert fixed to int by flooring */
#define floatfr(f) ((double)f / (1<<16))	/* convert fixed to float */
#define FIXED_HUGE 0x7FFFFFFF	/* the largest possible fixed point value */

struct fpoint {
    fixed       x,
                y;
};

/* extra state information used by the formatter */
struct formatter_state {
    fixed       x0;		/* The x position of the string currently
				 * being assembled */
    fixed       x,
                y;		/* The current coordinate */
    fixed       spshim;		/* The size of the shim being added to spaces */
    int         posatlastbreak;	/* The position in the document at the last
				 * word break */
    fixed       xatlastbreak;	/* The x coordinate at the last word break */
    int         nsp;		/* The number of spaces in the line */
    struct docview *v;		/* The document being looked at */
};


struct lineinfo {		/* information about one line visible in a
				 * view */
    struct marker *text;	/* The text in the line */
    int         linelength;	/* The number of characters in the line */
    struct formatter_info fi;	/* properties at the beginning of the line */
    struct formatter_state fs;	/* state information at the beginning of the
				 * line */
    fixed       cx0,
                cx1;		/* The first and last x coordinates on this
				 * line, for use in drawing selections */
};

/* Caret position information */
struct caretcoord {
    struct fpoint p;		/* X & Y coordinates */
    struct lineinfo *where;	/* The relevent line */
};

/*
 * A view on a document
 */
struct docview {
    struct document *doc;	/* the document being viewed */
    struct docview *docnext,
               *docprev;	/* the set of views on this document  */
    struct docview *allnext,
               *allprev;	/* the set of all views */
    struct marker *top;		/* the top of the view */
    struct lineinfo *lineinfo;	/* the start point of each line */
    struct spoint size;		/* the viewport size */
    struct caretcoord cstart,	/* The caret position */
                cend;
    unsigned char viewport;	/* a window index */
    unsigned char nlines;	/* number of lines in the view */
    unsigned    caretup:1;	/* True iff the caret in this document is up */
    unsigned    widecaret:1;	/* True iff the caret spans several characters */
    unsigned    modified:1;	/* true iff the document has been modified
				 * since the image of the view was last
				 * updated */
    unsigned    mapped:1;	/* true iff this view is mapped onto a screen
				 * image */
};

struct docview *viewroot;	/* the set of all views on documents */
struct docview *vw_create( /* document */ );
struct docview *viewports[256];	/* viewport->docview map */

struct font {
    struct font *next;		/* Fonts are in a linked list */
    char       *name;		/* Family name */
    char        size;		/* size in points */
    char        bbheight;	/* Height from highest ascender to lowest
				 * descender */
    char        descent;	/* distance from lowest descender to baseline */
    unsigned short nchars,	/* The number of characters in the font */
                fontindex;	/* The magic token by which the server knows
				 * this font */
    fixed       width[1];	/* The array of widths */
};

struct font *ft_create( /* fn, size */ );

struct font *defaultfont;	/* for now, this is the only font */
struct formatter_info defaultinfo;
