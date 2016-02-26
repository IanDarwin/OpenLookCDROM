/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or 
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

#ifndef lint
static char sccsid[] = "@(#)psview.c 9.4 88/01/19 Copyright 1985 Sun Micro";
static char RCSid[] = "@(#)$Header: paper.c,v 1.6 88/09/14 15:16:56 eric Exp $";
#endif

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

/*-
	PostScript previewer

	psview.c, Wed Jun  3 10:39:11 1987

 */

#include "paper.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "psio.h"

char errors[1024];

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
static	int         direction;

struct point {
	int x, y;
};

static	struct point size = {612, 792};			/*   { 8.5 * 72, 11.0 * 72 }  */
static	struct point center = {612 / 2, 792 / 2};

static	char       *ProgramName;
static	char	   *input_str;

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
            sprintf(errors, "Non-standard PostScript file. Expecting '%%%!'.\n");
            Fatal(errors);
        } 
    while (fgets(line, sizeof line, f))
	if (line[0] == '%')
	    if (line[1] == '%')
		switch (line[2]) {
/*-		case 'E':
		    if (strncmp(line + 3, "ndProlog", 8) == 0)
			EndProlog = ftell(f);
		    break; */
		case 'P':
		    if (strncmp(line + 3, "age:", 4) == 0) {
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
    /* convert page number from 1 base to 0 base */
    if (n >= npages)
	n = npages - 1;
    if (n < 0)
	n = 0;
    ps_startpage();
    PumpBytes(f, pagestart[n], pagestart[n + 1]); 
    ps_endpage();
    CurrentPage = n;
}

/* Preview a file, it assumes that the file object is seekable */
previewfile(f, name)
    register FILE *f;
    char       *name;
{
    int	    page_selection;

    MakePageTable(f);
    CurrentPage = -1;
    ps_startprolog();
    if (EndProlog > 0)
	PumpBytes(f, 0, EndProlog);
    ps_endprolog();
    if (ditroff)
	ps_ditroff_fix(); 
    set_MAX(npages);
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
    while (!psio_error(PostScriptInput) && !psio_eof(PostScriptInput))
	if (get_page_selection(&page_selection)) {
		GotoPage(f, page_selection - 1);
		ps_paintimage();
		}
	else if (get_exit()) 
		break;
}

main(argc, argv)
    char      **argv;
{
    char       *filename = 0;	/* The file to be previewed */
    char       *tempname = 0;	/* The name of a temporary file, if we created
				 * one */
    ProgramName = argv[0];
    if (ps_open_PostScript() == 0) {
	fprintf(stderr, "%s: Can't contact the NeWS server\n", ProgramName);
	exit(1);
    }
    if (argv[1] == NULL) { 
	   sprintf(errors, "%s: Requires PostScript filename.\n",ProgramName );
           Fatal(errors);
    }
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
		    sprintf(errors, "%s: Bad coordinate syntax `%s'; should be `-x,y-x,y'\n",
			    ProgramName, argv[0]);
		    Fatal(errors);
		}
		break;
	    case 'B':
		box = !box;
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
		sprintf(errors, "%s: Illegal switch `%s'\n", ProgramName, argv[0]);
		Fatal(errors);
		break;
	    }
	else {
	    if (filename) {
		sprintf(errors, "%s: Ignoring `%s'\n", ProgramName, argv[0]);
		popstring(errors); psio_flush(PostScript);
		}
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
		sprintf(errors, "%s: Can't create temp file `%s'\n", ProgramName, tempname);
		Fatal(errors);
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
    if (isatty(0) && fork()) exit(0);
    PSDefs();
    ps_initialize(size.x, size.y, center.x, center.y);
    if (!box)
	ps_nobox();
    if (filename == 0)
	previewfile(stdin, "stdin");
    else {
	register FILE *f = fopen(filename, "r");
	if (f == 0) {
	    sprintf(errors, "%s: Can't open %s\n", ProgramName, filename);
	    popstring(errors); psio_flush(PostScript);
	    }
	else {
	    previewfile(f, filename);
	    fclose(f);
	}
    }
    if (tempname)
	unlink(tempname);
}

Fatal(s)
char *s;
{
	popstring(s);
	psio_flush(PostScript);
	exit(-1);
}

