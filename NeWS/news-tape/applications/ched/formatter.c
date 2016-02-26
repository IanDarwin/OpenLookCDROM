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

#ifndef lint
static char sccsid[] = "%Z%%M% %I% %E% Copyright 1985 Sun Micro";
#endif

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

/*-
	The meat of the formatter

	formatter.c, Tue Apr 21 09:06:06 1987

		James Gosling,
		Sun Microsystems
 */

#include "document.h"
#include "psinter.h"

/*
 * The central part of the formatter is the routine scan_and_apply.  It scans
 * a line of the document, applies formatting directives, and calls routines
 * at key points in the line: when a substring is fully assembled; when the
 * end of the line is reached; and to deal with eraseing rectangles.  Which
 * routines get called is determined by a set of procedure pointers passed in
 * to scan_and_apply
 */

struct formatter_ops {		/* The set of procedure passed to
				 * scan_and_apply */
    int         (*handlestring) ();
    int         (*handleeol) ();
    int         (*eraserect) ();
};

static      dotpos0,		/* The starting character position of "dot" */
            dotpos1;		/* The ending character position of "dot" */

struct formatter_ops locate_ops;/* The procedures to call when locating the
				 * mouse position in the document */
struct formatter_ops measure_ops;	/* " when measuring & laying out a
					 * line */
struct formatter_ops output_ops;/* " when outputting a measured line */

/*
 * CaretCheck is called by scan_and_apply to see if either end of "dot" is at
 * the current character position
 */
#define CaretCheck() { \
    if (pos == dotpos0) {  \
	register struct docview *v = start->fs.v;  \
	v->cstart.p.x = start->fs.x;  \
	v->cstart.p.y = start->fs.y;  \
	v->cstart.where = start; \
    } \
    if (pos == dotpos1) {  \
	register struct docview *v = start->fs.v;  \
	v->cend.p.x = start->fs.x;  \
	v->cend.p.y = start->fs.y;  \
	v->cend.where = start; \
    } \
}

scan_and_apply(start, fi, fo, xlimit)
    struct lineinfo *start;
    register struct formatter_info *fi;
    register struct formatter_ops *fo;
    fixed       xlimit;
{
    register struct bytestring *b = start->text->document->data;
    register    pos = start->text->pos;
    int         plimit = pos + start->linelength;
    char        buf[300];
    register    bufptr = 0;
    start->fs.x = fixedi(fi->parameters[F_LEFTMARGIN]);
    if (fo == &output_ops)
	start->cx0 = start->fs.x;
    start->fs.nsp = 0;
    if (plimit > b->size)
	plimit = b->size;
    start->fs.x0 = start->fs.x;
    while (pos < plimit) {
	register long c = GetByte(b, pos);
	CaretCheck();
	if (c <= 040) {
	    start->fs.posatlastbreak = pos;
	    start->fs.xatlastbreak = start->fs.x;
	    if (c == '\n') {
		pos++;
		CaretCheck();
		break;
	    }
	    start->fs.nsp++;
	    buf[bufptr] = ' ';
	    c = start->fs.x + ((struct font *) fi->parameters[F_FONT])->width[' '] + start->fs.spshim;
	}
	else {
	    buf[bufptr] = c;
	    c = start->fs.x + ((struct font *) fi->parameters[F_FONT])->width[c];
	}
	if (c > xlimit) {
	    if (fo == &locate_ops && (c + start->fs.x) >> 1 < xlimit && pos + 1 < plimit)
		pos++;
	    break;
	}
	start->fs.x = c;
	bufptr++;
	pos++;
    }
    CaretCheck();
    (*fo->handlestring) (&start->fs, fi, buf, bufptr);
    pos = (*fo->handleeol) (&start->fs, fi, start, pos);
    if (fo == &output_ops)
	start->cx1 = start->fs.x;
    return pos;
}

null_proc()
{
}

null_handleeol(fs, fi, start, pos)
    register struct formatter_state *fs;
    register struct formatter_info *fi;
    register struct lineinfo *start;
    register    pos;
{
    return pos;
}


/*
 * End-of-line handler when measuring & laying out a line.  It's major task is
 * trimming trailing spaces & determining how much to add to the width of each
 * space character (fs->spshim)
 */
measure_handleeol(fs, fi, start, pos)
    register struct formatter_state *fs;
    register struct formatter_info *fi;
    register struct lineinfo *start;
    register    pos;
{
    register struct bytestring *b;
    register    c;
    start->text->span = pos - start->text->pos;
    b = start->text->document->data;
    if (fs->nsp > 0) {
	register    err = fixedi(fs->v->size.x - fi->parameters[F_RIGHTMARGIN])
	- fs->xatlastbreak;
	pos = fs->posatlastbreak;
	if (GetByte(b, pos) == '\n')
	    err = 0;
	else
	    while (pos > 0 && (c = GetByte(b, pos - 1)) == ' ')
		if (c == ' ') {
		    pos--;
		    fs->nsp--;
		    err += ((struct font *) fi->parameters[F_FONT])->width[' '];
		}
		else {
		    err = 0;
		    break;
		}
	start->linelength = pos - start->text->pos;
	if (fs->nsp > 1)
	    fs->spshim = err / (short) (fs->nsp - 1);
	pos = fs->posatlastbreak;
    }
    else
	start->linelength = pos - start->text->pos;
    c = -1;
    while (pos <= b->size && (c = GetByte(b, pos)) == ' ')
	pos++;
    if (c == '\n')
	pos++;
    return pos;
}

static struct formatter_ops measure_ops = {
    null_proc,
    measure_handleeol,
    null_proc,
};


/* Output a completely assembled substring of the current line */
output_handlestring(fs, fi, buf, n)
    register struct formatter_state *fs;
    register struct formatter_info *fi;
    char       *buf;
{
    while (1) {
	if (n <= 0)
	    return;
	if (buf[n - 1] != ' ')
	    break;
	n--;
    }
    ps_frmoveto(fs->x0, fs->y);
    ps_usefont(((struct font *) fi->parameters[F_FONT])->fontindex);
    if (fs->spshim == 0)
	ps_cshow(buf, n);
    else
	ps_cwidthshow(fs->spshim, buf, n);
}

output_eraserect(x, y, w, h)
{
    ps_eraserect(x, y, w, h);
}

static struct formatter_ops output_ops = {
    output_handlestring,
    null_handleeol,
    output_eraserect,
};

static struct formatter_ops null_ops = {
    null_proc,
    null_handleeol,
    null_proc,
};

static struct formatter_ops locate_ops = {
    null_proc,
    null_handleeol,
    null_proc,
};


/*
 * Format one line of the document: first measure it, then clear its
 * background rectangle, then draw it
 */
formatline(start, y, v, fi, ops)
    register struct lineinfo *start;
    register struct docview *v;
    fixed       y;
    struct formatter_info *fi;
    struct formatter_ops *ops;
{
    int         next;
    start->fi = *fi;
    start->fs.v = v;
    start->fs.y = y;
    start->fs.spshim = 0;
    start->text->span = 1 << 30;
    start->linelength = 1 << 30;
    next = scan_and_apply(start, fi, &measure_ops,
		fixedi(start->fs.v->size.x - fi->parameters[F_RIGHTMARGIN]));
    start->fs.y = y;
    if (start->cx0 < start->cx1) {
	register struct font *font;
	font = (struct font *) fi->parameters[F_FONT];
	(*ops->eraserect) (start->cx0 - fixedi(2),
			   start->fs.y - fixedi(font->descent),
			   start->cx1 + fixedi(2),
			   fixedi(font->bbheight));
    }
    *fi = start->fi;
    scan_and_apply(start, fi, ops, FIXED_HUGE);
    return next;
}

/* Draw or undraw "dot" */
togglecaret(v)
    register struct docview *v;
{
    v->caretup = !v->caretup;
    if (v->cstart.where)
	if (!v->widecaret)	/* "dot" has zero width and is between two
				 * characters */
	    ps_frdrawcaret(v->cstart.p.x, v->cstart.p.y - fixedi(3));
	else {			/* "dot" is wide and selects a group of
				 * characters: underline them */
	    ps_startselect();
	    assert(v->cstart.p.y >= v->cend.p.y);
	    if (v->cstart.p.y == v->cend.p.y) {
		ps_frmoveto(v->cstart.p.x, v->cstart.p.y);
		ps_frlineto(v->cend.p.x, v->cend.p.y);
	    }
	    else {
		register struct lineinfo *l = v->cstart.where;
		ps_frmoveto(v->cstart.p.x, v->cstart.p.y);
		ps_frlineto(l->cx1, v->cstart.p.y);
		while (++l < v->cend.where) {
		    ps_frmoveto(l->cx0, l->fs.y);
		    ps_frlineto(l->cx1, l->fs.y);
		}
		ps_frmoveto(l->cx0, v->cend.p.y);
		ps_frlineto(v->cend.p.x, v->cend.p.y);
	    }
	    ps_endselect();
	}
}

/*
 * Format all the lines of a document visible in a view.  Each view has an
 * array of lineinfo structs that describe the visible lines.  A line is
 * redraw if the contents of the line has changed or if a change in a previous
 * line has caused some ripple-through.
 */
formatview(v, fi)
    register struct docview *v;
    register struct formatter_info *fi;
{
    int         pos = v->top->pos;
    register struct marker *m;
    register struct lineinfo *lni;
    register    ln,
                y;
    y = fixedi(v->size.y - fi->parameters[F_POINTSIZE]);
    ln = 0;
    ps_usecanvas(v->viewport);
    if (v->caretup)
	togglecaret(v);
    dotpos0 = v->doc->caret->pos;
    dotpos1 = v->doc->caret->span + dotpos0;
    while (y >= 0) {
	if (v->nlines <= ln) {	/* Entend the line table since the view has
				 * grown */
	    register    sz;
	    if (v->nlines == 0) {
		v->nlines = 10;
		sz = v->nlines * sizeof(struct lineinfo);
		v->lineinfo = (struct lineinfo *) malloc(sz);
	    }
	    else {
		register    nnl = ln * 3 / 2;
		v->lineinfo = (struct lineinfo *) realloc(v->lineinfo,
					      nnl * sizeof(struct lineinfo));
		sz = (nnl - v->nlines) * sizeof(struct lineinfo);
		v->nlines = nnl;
	    }
	    bzero(&v->lineinfo[ln], sz);
	}
	lni = &v->lineinfo[ln];
	if ((m = lni->text) == 0)
	    m = lni->text = mk_create(v->doc, -1, 0);
	if (m->modified		/* the line has changed */
		|| m->pos != pos/* a change has rippled through */
	    ) {
	    m->pos = pos;
	    pos = formatline(lni, y, v, fi, &output_ops);
	    m->modified = 0;
	}
	else {
	    /* Don't have to redraw this line! */
	    if (v->nlines > ln + 1 && lni[1].text)
		if ((pos <= dotpos0 && dotpos0 <= pos + m->span)
			|| (pos <= dotpos1 && dotpos1 <= pos + m->span))
		    /*
		     * dot is in this line, measure it anyway so that the
		     * caret is handled properly
		     */
		    pos = formatline(lni, y, v, fi, &null_ops);
		else		/* Leave this line entirely alone! */
		    pos = lni[1].text->pos;
	    else
		pos = m->pos + m->span, y = -1;
	}
	y -= fixedi(fi->parameters[F_POINTSIZE]);
	ln++;
    }
    if (v->top->pos >= dotpos1)
	v->cstart.where = 0;
    else if (v->top->pos >= dotpos0) {
	lni = &v->lineinfo[0];
	v->cstart.p.x = lni->cx0;
	v->cstart.p.y = lni->fs.y;
	v->cstart.where = lni;
    }
    if (pos <= dotpos0)
	v->cstart.where = 0;
    else if (pos <= dotpos1 && (lni = &v->lineinfo[v->nlines - 1])->text) {
	v->cend.p.x = lni->cx1;
	v->cend.p.y = lni->fs.y;
	v->cend.where = lni;
    }
    dotpos0 = -1;
    dotpos1 = -1;
    v->widecaret = v->doc->caret->span > 0;
    togglecaret(v);
    v->modified = 0;
}

/* Set flags in a view so that it gets entirely redrawn */
damageview(v)
    register struct docview *v;
{
    register    ln;
    for (ln = 0; ln < v->nlines; ln++) {
	register struct marker *m = v->lineinfo[ln].text;
	if (m)
	    m->modified = 1;
    }
    v->modified = 1;
}

locate_mouse(v, x, y)		/* Locate a mouse hit in a view */
    register struct docview *v;
    fixed       x,
                y;
{
    register struct lineinfo *ln = v->lineinfo;
    register    n = v->nlines;
    struct formatter_info fi;
    while (--n >= 0 && ln->fs.y > y)
	ln++;
    if (ln->text == 0)
	return v->doc->data->size;
    fi = ln->fi;
    return scan_and_apply(ln, &fi, &locate_ops, x);
}

/* scroll the document so that character pos is at coordinate y in the window */
scroll_to(v, pos, ytarget)
    register struct docview *v;
    register    pos;
    fixed       ytarget;
{
    int         lastpos;
    int         lasty = 0;
    struct formatter_info *fi = &defaultinfo;
    register struct bytestring *b = v->doc->data;
    if (pos < 0)
	pos = 0;
    if (pos > b->size)
	pos = b->size;
    lastpos = pos;
    while (1) {
	if (pos > 0)
	    do
		pos--;
	    while (pos > 0 && GetByte(b, pos - 1) != '\n');
	{
	    struct scrollinfo {
		int         pos;
		int         y;
	    };
	    static struct scrollinfo *info;
	    static int  infosize;
	    register    ln = 0,
	                fpos = pos;
	    fixed       y = 0;
	    while (fpos < lastpos) {
		static struct lineinfo lni;
		if (infosize <= ln) {	/* Entend the line table since the
					 * view has grown */
		    register    sz;
		    if (infosize == 0) {
			infosize = 10;
			info = (struct scrollinfo *) malloc(infosize * sizeof(struct scrollinfo));
		    }
		    else {
			infosize = ln * 3 / 2;
			info = (struct scrollinfo *) realloc(info,
				       infosize * sizeof(struct scrollinfo));
		    }
		}
		if (lni.text == 0)
		    lni.text = mk_create(v->doc, -1, 0);
		lni.text->pos = fpos;
		info[ln].pos = fpos;
		info[ln].y = y;
		fpos = formatline(&lni, y, v, fi, &null_ops);
		y += fixedi(fi->parameters[F_POINTSIZE]);
		ln++;
	    }
	    if (y + lasty >= ytarget) {
		while (ln > 0) {
		    ln--;
		    if (y - info[ln].y + lasty >= ytarget)
			break;
		    fpos = info[ln].pos;
		}
		settoppos(v, fpos);
		return;
	    }
	    lasty += y;
	    lastpos = pos;
	}
	if (pos <= 0) {
	    settoppos(v, 0);
	    return;
	}
    }
}
