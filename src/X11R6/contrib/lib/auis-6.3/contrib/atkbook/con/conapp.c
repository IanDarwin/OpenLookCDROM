static char *conapp_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/con/RCS/conapp.c,v 1.1 1992/10/06 22:08:31 susan R6tape $";

/* **************************************************** *\
Copyright 1989 Nathaniel S. Borenstein
Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear in
supporting documentation, and that the name of 
Nathaniel S. Borenstein not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission. 

Nathaniel S. Borenstein DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
Nathaniel S. Borenstein BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
\* ***************************************************** */
#include <conapp.eh>
#include <im.ih>
#include <flexpair.ih>
#include <flexpairview.ih>
#include <frame.ih>
#include <buffer.ih>
#include <sys/file.h> /* for access() call */
#include <errno.h>

extern char *malloc(), *index();

boolean conapp__InitializeObject(c, self)
struct classheader *c;
struct conapp *self;
{
    self->desiredheight = 100;
    self->desiredwidth = 400;
    self->filenamesallocated = 5;
    self->filenames = (char **) malloc(self->filenamesallocated * sizeof (char *));
    if (!self->filenames) return(FALSE);
    self->filenamesused = 0;
    conapp_SetMajorVersion(self, 1);
    conapp_SetMinorVersion(self, 0);
    return(TRUE);
}

/* This routine could be easily modified to do a path search for the named console file */

static boolean FindFile(fname, fpp)
char *fname;
FILE **fpp;
{
    if (fname == NULL) {
	*fpp = NULL;
	return(TRUE);
    }
    *fpp = fopen(fname, "r");
    if (*fpp) return(TRUE);
    if (errno != ENOENT) return(FALSE);
    *fpp = fopen(fname, "w");
    if (!*fpp) return(FALSE);
    fclose(*fpp);
    *fpp = NULL;
    return(TRUE);
}

boolean conapp__Start(self)
struct conapp *self;
{
    struct im *im;
    struct flexpair *cp;
    struct flexpairview *cpv;
    struct frame *f;
    struct buffer *b;
    int i;
    FILE *fp;
    char LineBuf[250], *s, *t, bufname[25];

    im_SetProgramName("console");
    im_SetPreferedDimensions(0,0, self->desiredwidth, self->desiredheight);
    if (self->filenamesused <= 0) {
	self->filenamesused = 1;
	self->filenames[0] = NULL;
    }
    for (i=0; i<self->filenamesused; ++i) {
	im = im_Create(NULL);
	cp = flexpair_New();
	if (!FindFile(self->filenames[i], &fp)) {
	    printf("There is no such file as %s.\n", self->filenames[i]);
	    return(FALSE);
	}
	if (fp) {
	    if (fgets(LineBuf, sizeof(LineBuf)-1, fp) == NULL) {
		printf("The file %s is empty.\n", self->filenames[i]);
		fclose(fp);
		return(FALSE);
	    }
	    if (strncmp(LineBuf, "\\begindata{flexpair,", 19)) {
		printf("The file %s is not an ATK console file.\n", self->filenames[i]);
		fclose(fp);
		return(FALSE);
	    }
	    s = &LineBuf[19];
	    t = index(LineBuf, '}');
	    if (t) *t = '\0';
	    if (flexpair_Read(cp, fp, atoi(s)) != dataobject_NOREADERROR) {
		printf("Error reading data from file %s\n", self->filenames[i]);
		fclose(fp);
		return(FALSE);
	    }
	    fclose(fp);
	}
	cpv = flexpairview_New();
	sprintf(bufname, "console-%d", i);
	b = buffer_Create(bufname, self->filenames[i], "flexpair", cp);
	f = frame_Create(b);
	flexpairview_SetDataObject(cpv, cp);
	frame_SetCommandEnable(f, TRUE);
	frame_SetView(f, cpv);
	im_SetView(im, f);
	/*    frame_HideMessageLine(f); */  /* There should be such a method */
	flexpairview_WantInputFocus(cpv, cpv);
    }
    return(TRUE);
}

boolean conapp__ParseArgs(self, argc, argv)
struct conapp *self;
int argc;
char **argv;
{
    int i;
    for (i=1; i<argc; ++i) { /* skip argv[0] */
	if (argv[i][0] == '-') {
	    switch(argv[i][1]) {
		case 'H':
		    if (argv[i][2]) {
			self->desiredheight = atoi(&argv[i][2]);
		    } else if (++i < argc) {
			self->desiredheight = atoi(argv[i]);
		    } else {
			printf("No Height argument specified!\n");
			return(FALSE);
		    }
		    break;
		case 'W':
		    if (argv[i][2]) {
			self->desiredwidth = atoi(&argv[i][2]);
		    } else if (++i < argc) {
			self->desiredwidth = atoi(argv[i]);
		    } else {
			printf("No Width argument specified!\n");
			return(FALSE);
		    }
		    break;
		default:
		    printf("Unrecognized option switch %c\n", argv[i][1]);
		    return(FALSE);
	    }
	} else {
	    if (self->filenamesused >= self->filenamesallocated) {
		self->filenamesallocated += 5;
		self->filenames = (char **) realloc(self->filenames,(self->filenamesallocated * sizeof (char *)));
		if (!self->filenames) return(FALSE);
	    }
	    self->filenames[self->filenamesused] = malloc(1+strlen(argv[i]));
	    if (!self->filenames[self->filenamesused]) return(FALSE);
	    strcpy(self->filenames[self->filenamesused], argv[i]);
	    ++self->filenamesused;
	}
    }
    return(TRUE);
}

