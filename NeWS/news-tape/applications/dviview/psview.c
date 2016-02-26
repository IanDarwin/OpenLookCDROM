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
static char sccsid[] = "@(#)psview.c 9.3 88/01/18 Copyright 1985 Sun Micro";
#endif

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

/*-
	PostScript previewer

	psview.c, Wed Jun  3 10:39:11 1987

 */

#include "psview.h"
#include <stdio.h>
#include <sys/types.h>
#ifdef REF
#include <ref/config.h>
#endif
#include <sys/stat.h>
#include "psio.h"

static	int        *pagestart;		/* Array of page starting positions */
static	int         psize;		/* size of page array */
static	int         npages;		/* number of pages */
static	int         CurrentPage;
static	int         EndProlog;
static	int         Trailer;
static	int         verbose;
static	int         box = 1;
static	int         recolor;
static	int         ditroff = 0;
static	int         dvips = 0;
static	int         direction;

struct point {
    short       x,
                y;
};

static	struct point size = {612, 792};
static	struct point center = {612 / 2, 792 / 2};

static	char       *ProgramName;
static	char	   *input_str;

struct color {
    short       v[3];
    short       rgb;
};

static	struct color FGBG[2] = {0, 0, 0, -1, 256, 256, 256, -1};


parse_color(s, c)
    register char *s;
    register struct color *c;
{
    register    n = 0;
    register    index = 0;
    register    validn = 0;
    register    ndot = 0;
    bzero(c, sizeof *c);
    while (1)
	if ('0' <= *s && *s <= '9') {
	    n = n * 10 + *s++ - '0';
	    validn++;
	    ndot++;
	}
	else if (*s == '.') {
	    ndot = 0;
	    s++;
	}
	else {
	    if (index < 3 && validn) {
		n = n * 256;
		while (--ndot >= 0)
		    n = n / 10;
		if (n > 256)
		    n = 256;
		if (n < 0)
		    n = 0;
		c->v[index] = n;
		n = 0;
		ndot = 0;
		validn = 0;
	    }
	    switch (*s++) {
	    case 0:
		return 1;
	    default:
	error_return:
		c->rgb = -1;
		return 0;
	    case ',':
		if (index < 2)
		    index++;
		else
		    goto error_return;
		break;
	    case 'r':
		index = 0;
		c->rgb = 1;
		break;
	    case 'g':
		index = 1;
		c->rgb = 1;
		break;
	    case 'b':
		index = 2;
		break;
	    case 'h':
		index = 0;
		break;
	    case 's':
		index = 1;
		break;
	    }
	}
}

parse_dims(s)
    register char *s;
{
    float       vals[4];
    register float t;
    if (sscanf(s, "%f,%f-%f,%f", vals, vals + 1, vals + 2, vals + 3) != 4)
	return 0;
    if (vals[0] > vals[2]) {
	t = vals[2];
	vals[2] = vals[0];
	vals[0] = t;
    }
    if (vals[1] > vals[3]) {
	t = vals[3];
	vals[3] = vals[1];
	vals[1] = t;
    }
    size.x = (vals[2] - vals[0]) * 72;
    size.y = (vals[3] - vals[1]) * 72;
    center.x = (vals[2] + vals[0]) * 36;
    center.y = (vals[3] + vals[1]) * 36;
    return 1;
}

/* I added these 		*/
make_MAX()

{
	input_str = "/MAX  ";
	psio_fprintf(PostScript, "%s", input_str);
	psio_fprintf(PostScript, "%d", npages);
	input_str = "  def  ";
	psio_fprintf(PostScript, "%s", input_str);
}
make_MIN_MAX()
{
	input_str = "/MAX  ";
	psio_fprintf(PostScript, "%s", input_str);
	input_str = "  2  ";
	psio_fprintf(PostScript, "%s", input_str);
	input_str = "  def  ";
	psio_fprintf(PostScript, "%s", input_str);
}

/*
 * Scan the .ps file looking for the structureing comments that should be in
 * it.  We check for %%EndProlog, %%Page: and %%Trailer.
 */
MakePageTable(f)
    register FILE *f;
{
    char        line[1000];
    EndProlog = -1;
    Trailer = -1;
    npages = 0;

    fgets(line, sizeof line, f );
        if ((line[0] != '%') && (line[1] != '!'))
        {
            printf("Non-standard PostScript file. Expecting '%%%!'.\n");
            exit(f);
        } 
    while (fgets(line, sizeof line, f))
	if (line[0] == '%')
	  {
	    if (line[1] == '%')
		switch (line[2]) {
/*-		case 'E':
		    if (strncmp(line + 3, "ndProlog", 8) == 0)
			EndProlog = ftell(f);
		    break; */
		case 'P':
		    if ((strncmp(line + 3, "age", 3) == 0) &&
			(line[6] != 's')) {
			if (npages >= psize) {
			    if (psize)
				pagestart = (int *) realloc(pagestart, (psize = npages * 3 / 2) * sizeof(int));
			    else
				pagestart = (int *) malloc((psize = 2) * sizeof(int));
			}
			pagestart[npages++] = ftell(f);
		    }
		    break;
		case 'T':
		    if (strncmp(line + 3, "railer", 6) == 0)
			Trailer = ftell(f);
		    break;
		}
	    else if (line[1] == ' ' && line[2] == 'l' && strncmp(line, "% lib/psdit.pro", 15) == 0)
	        ditroff = 1;
            else if (line[1] == ' ' && line[3] == 'D' && strncmp(line, "%  Dvips.pro", 12) == 0)
	        dvips = 1;
	  }
    if (npages > 0) {
	if (EndProlog < 0 || pagestart[0] < EndProlog)
	    EndProlog = pagestart[0];
    }
    else
	EndProlog = 0;
    if (Trailer < 0)
	Trailer = ftell(f);
    if (npages + 2 >= psize) {
	if (psize)
	    pagestart = (int *) realloc(pagestart, (psize = npages + 10) * sizeof(int));
	else
	    pagestart = (int *) malloc((psize = 2) * sizeof(int));
    }
    if (npages == 0)
	pagestart[npages++] = 0;
    pagestart[npages] = Trailer;
    if (verbose)
	printf("%d Pages\n", npages);
}

PumpBytes(f, start, end)
    register FILE *f;
    register    start,
                end;
{
    register    c;
    fseek(f, start, 0);
    while (start < end) {
	psio_putc(getc(f), PostScript);
	start++;
    }
    psio_putc(' ', PostScript);
}

GotoPage(f, n)
    FILE       *f;
{
    static      notfirst;

    /* convert page number from 1 base to 0 base */
    if (n >= npages)
	n = npages - 1;
    if (n < 0)
	n = 0;
    if (n == CurrentPage)
	return;
    ps_startpage();
    PumpBytes(f, pagestart[n], pagestart[n + 1]); 
    ps_endpage();
/*    if (notfirst)*/
        ps_damageall();
    notfirst = 1;

    CurrentPage = n;
}

/* Preview a file, it assumes that the file object is seekable */
previewfile(f, name)
    register FILE *f;
    char       *name;
{
    int	    page_selection;
    MakePageTable(f);
    if (ps_open_PostScript() == 0) {
	fprintf(stderr, "%s: Can't contact the NeWS server\n", ProgramName);
	exit(1);
    }
    ps_initialize(size.x, size.y, center.x, center.y);
    if (!box)
	ps_nobox();
    CurrentPage = -1;
    if (recolor)
	ps_redefine_colors();
    {
	register struct color *v;
	for (v = FGBG; v < &FGBG[sizeof FGBG / sizeof FGBG[0]]; v++)
	    if (v->rgb >= 0) {
		if (v->rgb)
		    ps_rgbcolor(v->v[0], v->v[1], v->v[2]);
		else
		    ps_hsbcolor(v->v[0], v->v[1], v->v[2]);
		if (v == FGBG)
		    ps_defFG();
		else
		    ps_defBG();
	    }
    }
    if (npages < 2) 
        make_MIN_MAX();
    if (npages >= 2) 
        make_MAX();

    if (EndProlog > 0 && !dvips) {
        ps_startprolog();
        PumpBytes(f, 0, EndProlog);
        ps_endprolog();
    }
    {
	char        buf[100];
#ifndef SYSVREF
	extern char *rindex();
#endif
	register char *p = rindex(name, '/');
	register char *dest;
	if (p == 0)
	    p = name;
	else
	    p++;
	for (dest = buf; *p && *p != '.'; )
	    *dest++ = *p++;
	*dest++ = 0;
	ps_setupwindow(buf[0] ? buf : "Preview");
    }
    if (EndProlog > 0 && dvips) {
        ps_dvips_fix();
        ps_startprolog();
        PumpBytes(f, 0, EndProlog);
        ps_endprolog();
    }
    if (ditroff)
	ps_ditroff_fix(); 
    ps_prolog_done();
    GotoPage(f, 0);
    while (!psio_error(PostScriptInput) && !psio_eof(PostScriptInput))
	if (get_page_selection(&page_selection)) 
		GotoPage(f, page_selection - 1); 		
	else if (get_exit()) 
		break;
}

main(argc, argv)
    char      **argv;
{
    char       *filename = 0;	/* The file to be previewed */
    char       *tempname = 0;	/* The name of a temporary file, if we created
				 * one */
    char       asize;           /* the size of A? paper */
    short      temp;            /* for swapping sizes */

    ProgramName = argv[0];
/*-    if (argv[1] == NULL) { 
	   printf("%s: Requires PostScript filename.\n",ProgramName );
           exit(0); 
    } */
    while (--argc > 0)
	if ((++argv)[0][0] == '-')
	    switch (argv[0][1]) {
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':
		if (!parse_dims(&argv[0][1])) {
		    fprintf(stderr, "%s: Bad coordinate syntax `%s'; should be `x,y-x,y'\n",
			    ProgramName, argv[0]);
		    exit(-1);
		}
		break;
	    case 'a':
	    case 'A':
		asize = argv[0][2];
		if (asize < '0' || asize > '5') {
		    fprintf(stderr, "%s: Bad page size `%s'\n", ProgramName, argv[0]);
		    exit(-1);
		}
		size.x = 2381;
		size.y = 3368;
		while (asize-- > '0') {
		    short temp;
		    temp = size.y;
		    size.y = size.x;
		    size.x = temp / 2;
		}
		switch (argv[0][3]) {
		case 'L':
		case 'l':
		    temp = size.x; size.x = size.y; size.y = temp;
		    break;
		case 'P':
		case 'p':
		case '\0':
		    break;
		default:
		    fprintf(stderr, "%s: Bad page orientation `%s'\n", ProgramName, argv[0]);
		    exit(-1);
		}
		center.x = size.x / 2;
		center.y = size.y / 2;
		break;
	    case 'b':
		if (!parse_color(&argv[0][2], &FGBG[1])) {
		    fprintf(stderr, "%s: Bad color syntax `%s'\n", ProgramName, argv[0]);
		    exit(-1);
		}
		recolor++;
		break;
	    case 'B':
		box = !box;
		break;
	    case 'c':
		{
		    static struct color dfc[2] = {256, 256, 256, 1, 0, 0, 256, 1};
		    bcopy(dfc, FGBG, sizeof FGBG);
		    recolor++;
		}
		break;
	    case 'f':
		if (!parse_color(&argv[0][2], &FGBG[0])) {
		    fprintf(stderr, "%s: Bad color syntax `%s'\n", ProgramName, argv[0]);
		    exit(-1);
		}
		recolor++;
		break;
	    case 'l':		/* landscape slide size */
		size.y = 7 * 72;
		size.x = 11 * 72;
		center.y = 7 * 72 / 2 + (11 - 7) * 72;
		center.x = size.x / 2;
		break;
	    case 's':		/* slide size */
		size.y = 6.8 * 72;
		center.y = 6.8 * 72 / 2 + (11 - 6.8) * 72;
		break;
	    case 'S':		/* slide size */
		size.y = 5.7 * 72;
		center.y = 5.7 * 72 / 2 + (11 - 5.7) * 72;
		break;
	    case 'v':
		verbose++;
		break;
	    default:
		fprintf(stderr, "%s: Illegal switch `%s'\n", ProgramName, argv[0]);
		exit(-1);
		break;
	    }
	else {
	    if (filename)
		fprintf(stderr, "%s: Ignoring `%s'\n", ProgramName, argv[0]);
	    else
		filename = argv[0];
	}
    if (filename == 0) {
	struct stat st;
	if (fstat(fileno(stdin), &st) < 0) {
	    perror(ProgramName);
	    exit(-1);
	}
	if ((st.st_mode & S_IFMT) != S_IFREG) {	/* Copy the file */
	    register    fd;
	    register    n;
	    char        buffer[16 * 1024];
	    tempname = (char *) mktemp("/tmp/ps.XXXXXX");
	    fd = creat(tempname, 0700);
	    if (fd < 0) {
		fprintf(stderr, "%s: Can't create temp file `%s'\n", ProgramName, tempname);
		exit(-1);
	    }
	    while ((n = read(fileno(stdin), buffer, sizeof buffer)) > 0)
		write(fd, buffer, n);
	    if (n < 0) {
		perror(ProgramName);
		exit(-1);
	    }
	    close(fd);
	    filename = tempname;
	}
    }
    if (filename == 0)
	previewfile(stdin, "stdin");
    else {
	register FILE *f = fopen(filename, "r");
	if (f == 0)
	    fprintf(stderr, "%s: Can't open %s\n", ProgramName, filename);
	else {
	    previewfile(f, filename);
	    fclose(f);
	}
    }
    if (tempname)
	unlink(tempname);
}

