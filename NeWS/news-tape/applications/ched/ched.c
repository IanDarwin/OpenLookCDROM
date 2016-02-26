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
	A cheap editor, for a simple NeWS demo

	ched.c, Mon Apr 20 10:42:38 1987

		James Gosling,
		Sun Microsystems
 */

#include "document.h"
#include "psinter.h"
#include <sys/ioctl.h>

struct style style_bold = {0, F_FACECODE, M_OR, FC_BOLD};
struct style style_italic = {0, F_FACECODE, M_OR, FC_ITALIC};
struct style style_smaller = {0, F_POINTSIZE, M_ADDPSREL, -fixedi(1) / 5};
struct style style_bigger = {0, F_POINTSIZE, M_ADDPSREL, fixedi(1) / 5};
struct style style_superscript = {&style_smaller, F_BASELINE, M_ADDPSREL, fixedi(1) / 2};
struct style style_subscript = {&style_larger, F_BASELINE, M_ADDPSREL, -fixedi(1) / 2};

main(argc, argv)
    char      **argv;
{
    char       *docname = "TEST";
    char       *fontname = "Times-Roman";
    char       *fontsize = "12";
    int         seenlook = 0;
    /*- int         sync_count = 0;	/* The number of characters processed since
				     * the last update.  Used for syncronizing
				     * type-ahead. */
    register struct document *doc;
    register struct docview *vw;
    currentcanvas = -1;
    currentfont = -1;
    while (--argc > 0)
	if ((++argv)[0][0] == '-')
	    switch (argv[0][1]) {
	    case 'f':
		fontname = &argv[0][2];
		break;
	    case 's':
		fontsize = &argv[0][2];
		break;
	    default:
		fprintf(stderr, "Illegal switch: %s\n", argv[0]);
		exit(1);
	    }
	else
	    docname = argv[0];
    doc = d_create();
    {
	int         fd = open(docname, 0);
	if (fd < 0) {
	    fprintf(stderr, "Can't read %s\n", docname);
	    exit(1);
	}
	d_readfile(doc, fd);
	vw = vw_create(doc);
	d_setcaret(doc, 0, 0);
	close(fd);
    }
    ps_open_PostScript(0);
    ps_startup();
    vw_createwindowforview(vw, docname);
    defaultfont = ft_create(fontname, atoi(fontsize));
    defaultinfo.parameters[F_TYPE] = FT_TEXT;
    defaultinfo.parameters[F_FONT] = (int) defaultfont;
    defaultinfo.parameters[F_FACECODE] = 0;
    defaultinfo.parameters[F_POINTSIZE] = 16;
    defaultinfo.parameters[F_BASELINE] = 0;
    defaultinfo.parameters[F_LEFTMARGIN] = 10;
    defaultinfo.parameters[F_RIGHTMARGIN] = 10;
    defaultinfo.parameters[F_FIRSTMARGIN] = 0;
    defaultinfo.parameters[F_WIDTH] = 0;
    defaultinfo.parameters[F_HEIGHT] = 0;
    while (!psio_error(PostScriptInput) && !psio_eof(PostScriptInput)) {
	int         window,
	            width,
	            height,
	            ch;
	int         nread,
	            x,
	            y;
	if (ioctl(psio_fileno(PostScriptInput), FIONREAD, &nread) < 0)
	    break;
	if (nread)
	    putchar('f');
	if (psio_availinputbytes(PostScriptInput) > 0)
	    putchar('b');
	nread += psio_availinputbytes(PostScriptInput);
	if (nread <= 0)
	    for (vw = viewroot; vw; vw = vw->allnext)
		if (vw->modified)
		    formatview(vw, &defaultinfo);
	if (ps_redraw(&window, &width, &height)) {
	    register struct lineinfo *ln;
	    ps_startredraw(window);
	    vw = viewports[window];
	    if (vw->nlines > 0)
		for (ln = vw->lineinfo + vw->nlines; --ln >= vw->lineinfo;)
		    ln->cx0 = ln->cx1 = 0;
	    currentcanvas = -1;
	    currentfont = -1;
	    vw->size.x = width;
	    vw->size.y = height;
	    vw->caretup = 0;
	    damageview(vw);
	}
	else if (ps_mouse(&window, &ch, &x, &y)) {
	    int         pos;
	    static      pos0;
	    register struct marker *caret;
	    vw = viewports[window];
	    caret = vw->doc->caret;
	    pos = locate_mouse(vw, fixedi(x), fixedi(y));
	    if (ch == 0) {
		mk_movemark(caret, pos, 0);
		pos0 = pos;
	    }
	    else {
		if (ch == 2) {
		    pos0 = caret->pos;
		    if (pos < pos0 + (caret->span >> 1))
			pos0 += caret->span;
		}
		if (pos < pos0)
		    mk_movemark(caret, pos, pos0 - pos);
		else
		    mk_movemark(caret, pos0, pos - pos0);
	    }
	    vw->modified = 1;
	}
	else if (ps_thumb(&window, &height))
	    thumb(viewports[window], height);
	else if (ps_scroll(&window, &height))
	    scroll(viewports[window], fixedi(height));
	else if (ps_keyboard(&window, &ch)) {
	    register struct marker *caret;
	    vw = viewports[window];
	    caret = vw->doc->caret;
	    /*-	    sync_count++; */
	    if (seenlook) {
		register struct style *nst = 0;
		switch (ch & 037) {
		case 'b' & 037:
		    nst = &style_bold;
		    break;
		case 'd' & 037:
		    nst = &style_subscript;
		    break;
		case 'i' & 037:
		    nst = &style_italic;
		    break;
		case 'l' & 037:
		    nst = &style_larger;
		    break;
		case 'p' & 037:
		    print_styles(vw->doc);
		    break;
		case 's' & 037:
		    nst = &style_smaller;
		    break;
		case 'u' & 037:
		    nst = &style_superscript;
		    break;
		}
		if (nst && caret->span > 0)
		    style_apply(vw->doc, caret->pos, caret->span, nst);
	    }
	    else
		switch (ch) {
		case '\r':
		    ch = '\n';
		default:
		    if (ch >= ' ' || ch == '\n') {
			char        str[1];
			str[0] = ch;
			if (caret->span > 0)
			    d_deletebytes(vw->doc, caret->pos, caret->span);
			d_insertbytes(vw->doc, caret->pos, str, 1);
		    };
		    break;
		case 0177:
		case '\b':
		    if (caret->span > 0)
			d_deletebytes(vw->doc, caret->pos, caret->span);
		    else
			d_deletebytes(vw->doc, caret->pos, -1);
		    break;
		case 'd' & 037:
		    if (caret->span > 0)
			d_deletebytes(vw->doc, caret->pos, caret->span);
		    else
			d_deletebytes(vw->doc, caret->pos, 1);
		    break;
		case 'a' & 037:
		    mk_movemark(caret, 0, 0);
		    settoppos(vw, 0);
		    break;
		case 'b' & 037:
		    if (caret->pos > 0)
			caret->pos--;
		    break;
		case 'c' & 037:
		    exit(0);
		case 'e' & 037:
		    mk_movemark(caret, vw->doc->data->size, 0);
		    break;
		case 'f' & 037:
		    if (caret->pos < vw->doc->data->size)
			caret->pos++;
		case 'l' & 037:
		    seenlook = 2;
		    break;
		}
	    if (!seenlook)
		caret->span = 0;
	    else
		seenlook--;
	    vw->modified = 1;
	}
	else {
	    printf("Bogus token\n");
	    printf("text=\"%.*s\"\n", PostScriptInput->cnt > 40 ? 40 : PostScriptInput->cnt,
		   PostScriptInput->ptr);
	    abort(0);
	}
    }
}
