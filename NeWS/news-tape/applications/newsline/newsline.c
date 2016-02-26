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
*/
/*
 * Copyright (c) 1985, 1988 by Sun Microsystems, Inc.
 */

/*-
	PostScript previewer

	psview.c, Wed Jun  3 10:39:11 1987

	bvs - changed to newsline.c Nov 1988

 */

#include "newsline.h"
#include <stdio.h>
#include <sys/types.h>
#ifdef REF
#include <ref/config.h>
#endif
#include <sys/stat.h>
#include "psio.h"

FILE *previewfile();

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
static	FILE        *pfile;

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

SetMax(max)
int max;
{
	ps_setminmax(1, max);
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
    static      notfirst;

    /* convert page number from 1 base to 0 base */
    if (n >= npages)
		n = npages - 1;
    if (n < 0)
		n = 0;
    if (n == CurrentPage)
		return;
    ps_startpage(pagestart[n + 1] - pagestart[n]);
    PumpBytes(f, pagestart[n], pagestart[n + 1]); 
    ps_endpage();

	ps_damageall();
    notfirst = 1;

    CurrentPage = n;
}

/* Preview a file, it assumes that the file object is seekable */
FILE *previewfile(filename, name)
    char       *filename;
    char       *name;
{
    FILE *f = fopen(filename, "r");
	if(f == NULL)
	  {
		fprintf(stderr, "Couldn't open file '%s'\n", filename);
		return;
	  }
	
    MakePageTable(f);
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

    if (EndProlog > 0) {
		ps_startprolog(EndProlog);
		PumpBytes(f, 0, EndProlog);
		ps_endprolog();
    }
    if (ditroff)
		ps_ditroff_fix(); 
    SetMax((npages < 2) ? 2 : npages);

    GotoPage(f, 0);
	{
		char        buf[100];
#		ifndef SYSVREF
		extern char *rindex();
#		endif
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
	return(f);
}

NeWSInit()
{
	char buf[256];
    if (ps_open_PostScript() == 0) {
		fprintf(stderr,
			"%s: Can't contact the NeWS server\n", ProgramName);
		exit(1);
		}
	getwd(buf);
    ps_initialize(buf);
}

NeWSEventLoop()
{
    char filename[256];
    char name[256];
    int	 page_selection;
    
    while (!psio_error(PostScriptInput) && !psio_eof(PostScriptInput))
		if (get_page_selection(&page_selection)) 
		{
			/* fprintf(stderr, "goto '%d'\n", page_selection - 1); */
			if(pfile) GotoPage(pfile, page_selection - 1); 		
		}
		else if (get_newfile(filename, name))
		{
			/* fprintf(stderr, "preview '%s'\n", filename); */
			if(pfile) fclose(pfile);
			pfile = previewfile(filename, name);
		}
		else if (get_exit()) 
		{
			break;
		}
}

main(argc, argv)
    char      **argv;
{
    char *filename = 0;	/* The file to be previewed */
    char *tempname = 0;	/* The name of a temporary file */
    ProgramName = argv[0];
    while (--argc > 0)
	if ((++argv)[0][0] == '-')
	    switch (argv[0][1]) {
	    	case 'b':
				if (!parse_color(&argv[0][2], &FGBG[1])) {
		    		fprintf(stderr,
						"%s: Bad color syntax `%s'\n",
						ProgramName, argv[0]);
		    		exit(-1);
				}
				recolor++;
				break;
	    	case 'B':
				box = !box;
				break;
			case 'c':
				{
					static struct color dfc[2] =
						{256, 256, 256, 1, 0, 0, 256, 1};
					bcopy(dfc, FGBG, sizeof FGBG);
					recolor++;
				}
				break;
			case 'f':
				if (!parse_color(&argv[0][2], &FGBG[0])) {
					fprintf(stderr,
						"%s: Bad color syntax `%s'\n",
						ProgramName, argv[0]);
					exit(-1);
				}
				recolor++;
				break;
	    	case 'v':
				verbose++;
				break;
	    	default:
				fprintf(stderr,
					"%s: Illegal switch `%s'\n",
					ProgramName, argv[0]);
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
	   printf("%s: Requires PostScript filename.\n",ProgramName );
           /* exit(0);  */
    }

	NeWSInit();
	if(filename != 0)
		{ pfile = previewfile(filename, filename); }
	NeWSEventLoop();

    if (tempname)
		unlink(tempname);
}

