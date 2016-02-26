/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <ctype.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <NeWS/psio.h>

#if defined(sun3)
#include "icecps-sun3.h"
#elif defined(sun4)
#include "icecps-sun4.h"
#endif

#include "Text.h"

static char *textptr;
static char sourcebuf[TEXT_MAXLINELEN+1];
static FILE *sourcefp;
static int sourceline;

extern "C" {
int		pprintf( ... );
int		ps_checkfor(PSFILE *, int, int);
int		ps_currenttag(PSFILE *, int, int *);
int		pscanf( ... );
int		psio_flush(PSFILE *);
int		psio_flushbuf(char, PSFILE *);
int		psio_write(char *, int, int, PSFILE *);
char *		strcat(char *, char *);
char *		strcpy(char *, char *);
int		strlen(char *);
}

extern FILE *	doc_open(char *, char *);

Text::Text()
{
	source= TEXT_USERINPUT;
	filename= (char *) 0;
	text= (char *) 0;
	font= TEXT_GLOBALFONT;
	fontname= (char *) 0;
	fontsz= TEXT_GLOBALFONTSZ;
	size= 12.;
	lead= 0.;
	justify= TEXT_FLUSHLEFT;
	path= (Path *) 0;
	pathoffset= 0.;
	letterspace= 0.;
	foreground= TEXT_GLOBALFG;
	redfg= greenfg= bluefg= 0;
	backgroundmode= TEXT_TRANSPARENTBGM;
	background= TEXT_GLOBALBG;
	redbg= greenbg= bluebg= 255;
	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_TEXT);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(0);
}

Text::Text(Dlnk **listhd, char *nm, int seq)
{
	if (listhd == (Dlnk **) 0)
		;
	else if (*listhd == (Dlnk *) 0)
		*listhd= (Dlnk *) this;
	else
		link(*listhd, DLNK_TAIL);

	(void) setname(nm);

	source= TEXT_USERINPUT;
	filename= (char *) 0;
	text= (char *) 0;
	font= TEXT_GLOBALFONT;
	fontname= (char *) 0;
	fontsz= TEXT_GLOBALFONTSZ;
	size= 12.;
	lead= 0.;
	justify= TEXT_FLUSHLEFT;
	path= (Path *) 0;
	pathoffset= 0.;
	letterspace= 0.;
	foreground= TEXT_GLOBALFG;
	redfg= greenfg= bluefg= 0;
	backgroundmode= TEXT_TRANSPARENTBGM;
	background= TEXT_GLOBALBG;
	redbg= greenbg= bluebg= 255;
	(void) settype(GROBJ_INTOBJ);
	(void) setintobjtype(INTOBJ_TEXT);
	setscale((float) 1., (float) 1.);
	setrotation((float) 0.);
	setsequence(seq);
	sortsequence((Grobj **) listhd);
}

int
Text::setsource(int s)
{
	switch (s) {
	case TEXT_USERINPUT:
		delete filename;
		filename= (char *) 0;
		source= s;
		return GROBJ_SUCCESS;
	case TEXT_FILEINPUT:
		delete text;
		text= (char *) 0;
		source= s;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Text::settext(char *t)
{
	delete text;
	textlen= textbufsz= 0;
	if (t == (char *) 0)
		text= (char *) 0;
	else if (strlen(t) == 0)
		text= (char *) 0;
	else {
		if ((text= new char[strlen(t)+1]) == (char *) 0)
			return GROBJ_FAILURE;
		(void) strcpy(text, t);
		textlen= strlen(t);
		textbufsz= textlen+1;
	}
	return GROBJ_SUCCESS;
}

int
Text::setfilename(char *f)
{
	delete filename;
	if (f == (char *) 0)
		filename= (char *) 0;
	else if (strlen(f) == 0)
		filename= (char *) 0;
	else {
		if ((filename= new char[strlen(f)+1]) == (char *) 0)
			return GROBJ_FAILURE;
		(void) strcpy(filename, f);
	}
	return GROBJ_SUCCESS;
}

int
Text::setfont(int f, char *n)
{
	char *oldfontname;

	switch (f) {
	case TEXT_GLOBALFONT:
	case TEXT_OTHERFONT:
		break;
	default:
		return GROBJ_FAILURE;
	}

	oldfontname= fontname;
	if (n == (char *) 0)
		fontname= (char *) 0;
	else if (strlen(n) == 0)
		fontname= (char *) 0;
	else {
		if ((fontname= new char[strlen(n)+1]) == (char *) 0) {
			fontname= oldfontname;
			return GROBJ_FAILURE;
		}
		(void) strcpy(fontname, n);
	}
	font= f;
	delete oldfontname;
	return GROBJ_SUCCESS;
}

int
Text::setfontsize(int fs, float s, float l)
{
	switch (fs) {
	case TEXT_GLOBALFONTSZ:
	case TEXT_OTHERFONTSZ:
		fontsz= fs;
		size= s;
		lead= l;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Text::setjustify(int j)
{
	switch (j) {
	case TEXT_FLUSHLEFT:
	case TEXT_FLUSHRIGHT:
	case TEXT_CENTER:
	case TEXT_JUSTIFY:
	case TEXT_PATH:
		justify= j;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Text::setpath(Path *p, float po, float l)
{
	if (po < 0.)
		return GROBJ_FAILURE;

	if ((path != (Path *) 0) && (path != p))
		(void) path->setreferences(PATH_REFDECR);
	if ((p != (Path *) 0) && (path != p))
		(void) p->setreferences(PATH_REFINCR);

	path= p;
	pathoffset= po;
	letterspace= l;
	return GROBJ_SUCCESS;
}

int
Text::setforeground(int fg, unsigned char r, unsigned char g, unsigned char b)
{
	switch (fg) {
	case TEXT_BLACKFG:
		foreground= fg;
		redfg= greenfg= bluefg= 0;
		return GROBJ_SUCCESS;
	case TEXT_WHITEFG:
		foreground= fg;
		redfg= greenfg= bluefg= 255;
		return GROBJ_SUCCESS;
	case TEXT_GLOBALFG:
	case TEXT_OTHERFG:
		foreground= fg;
		redfg= r;
		greenfg= g;
		bluefg= b;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Text::setbackground(int mode, int bg, unsigned char r, unsigned char g, unsigned char b)
{
	switch (mode) {
	case TEXT_TRANSPARENTBGM:
	case TEXT_OPAQUEBGM:
		break;
	default:
		return GROBJ_FAILURE;
	}

	switch (bg) {
	case TEXT_WHITEBG:
		backgroundmode= mode;
		background= bg;
		redbg= greenbg= bluebg= 255;
		return GROBJ_SUCCESS;
	case TEXT_BLACKBG:
		backgroundmode= mode;
		background= bg;
		redbg= greenbg= bluebg= 0;
		return GROBJ_SUCCESS;
	case TEXT_GLOBALBG:
	case TEXT_OTHERBG:
		backgroundmode= mode;
		background= bg;
		redbg= r;
		greenbg= g;
		bluebg= b;
		return GROBJ_SUCCESS;
	default:
		return GROBJ_FAILURE;
	}
}

int
Text::draw(FILE *outfp)
{
	int tag, rendererr, linelen;
	int x, y, flags;
	float fx, fy, rot, hs, vs;
	float r, g, b;
	float blankwidth;
	float boxwidth, boxheight;
	float strwidth, maxstrwidth;
	char psbuf[128];
	Path *cpth;

	if ((source == TEXT_USERINPUT) && (text == (char *) 0))
		return GROBJ_SUCCESS;
	if (fontname == (char *) 0)
		return GROBJ_FAILURE;

	if (opensource() != GROBJ_SUCCESS)
		return GROBJ_FAILURE;

	if (outfp == (FILE *) 0) {

		/* discard any unread NeWS tags */
		while (ps_check_PostScript_event() == 1)
			ps_read_PostScript_event(&tag);

		news_traperrors();
	}
	else
		fprintf(outfp, "save\n");

	if ((cpth= getclip()) != (Path *) 0) {
		flags= PATH_DRAWCLOSE | PATH_DRAWCLIP;
		cpth->draw(flags, outfp);
	}

	switch (justify) {
	case TEXT_FLUSHLEFT:
	case TEXT_FLUSHRIGHT:
	case TEXT_CENTER:
	case TEXT_JUSTIFY:
		/* shift origin to object location */
		getloc(&fx, &fy, &x, &y);
		fx*= 72.;
		fy*= 72.;
		if ((fx != 0.) || (fy != 0.)) {

			/* NeWS server limitation --
			   the real magic number seems to be 0.03125,
			   but let's not push our luck */
			if ((fx >= 0.) && (fx < 0.05))
				fx= 0.05;
			else if ((fx <= 0.) && (fx > -0.05))
				fx= -0.05;
			if ((fy >= 0.) && (fy < 0.05))
				fy= 0.05;
			else if ((fy <= 0.) && (fy > -0.05))
				fy= -0.05;

			if (outfp == (FILE *) 0)
				news_translate(fx, fy);
			else
				fprintf(outfp, "%6.4f %6.4f translate\n", fx, fy);
		}

		/* rotate as necessary */
		rot= getrotation();
		if (outfp == (FILE *) 0)
			news_rotate(rot);
		else
			fprintf(outfp, "%6.4f rotate\n", rot);

		/* scale appropriately */
		getscale(&hs, &vs);
		if (outfp == (FILE *) 0)
			news_scale(hs, vs);
		else
			fprintf(outfp, "%6.4f %6.4f scale\n", hs, vs);
		break;

	case TEXT_PATH:
		getscale(&hs, &vs);
		if (path == (Path *) 0)
			return GROBJ_FAILURE;
		break;
	}

	/* access font */
	if (outfp == (FILE *) 0)
		news_font(fontname, size);
	else
		fprintf(outfp, "/%s findfont %4.2f scalefont setfont\n", fontname, size);

	/* determine maximum line width */
	if ((justify != TEXT_PATH) &&
	    ((backgroundmode == TEXT_OPAQUEBGM) || (justify != TEXT_FLUSHLEFT))) {
		if (outfp == (FILE *) 0) {
			maxstrwidth= 0.;
			while (getsourceline(outfp) > 0) {
				news_strwidth(sourcebuf, &strwidth);
				if (maxstrwidth < strwidth)
					maxstrwidth= strwidth;
			}
			news_strwidth(" ", &blankwidth);
			boxwidth= maxstrwidth+(2.*blankwidth);
		}
		else {
			fprintf(outfp, "/ICEmaxstrwidth 0 def\n");
			while (getsourceline(outfp) > 0) {
				fprintf(outfp, "/ICEcurrstrwidth (%s) stringwidth pop def\n", sourcebuf);
				fprintf(outfp, "ICEcurrstrwidth ICEmaxstrwidth gt { /ICEmaxstrwidth ICEcurrstrwidth def } if\n");
			}
			fprintf(outfp, "/ICEblankwidth ( ) stringwidth pop def\n");
			fprintf(outfp, "/ICEboxwidth 2 ICEblankwidth mul ICEmaxstrwidth add def\n");
		}
	}

	/* fill background if opaque */
	if ((justify != TEXT_PATH) && (backgroundmode == TEXT_OPAQUEBGM)) {

		/* set background color */
		r= ((float) redbg)/255.;
		g= ((float) greenbg)/255.;
		b= ((float) bluebg)/255.;
		if (outfp == (FILE *) 0) {
			news_setrgbcolor(r, g, b);
			switch (justify) {
			case TEXT_FLUSHLEFT:
			case TEXT_JUSTIFY:
				fx= -blankwidth;
				break;
			case TEXT_FLUSHRIGHT:
				fx= -maxstrwidth-blankwidth;
				break;
			case TEXT_CENTER:
				fx= (-maxstrwidth/2.)-blankwidth;
				break;
			}
			if (lead >= -size) {
				fy= -((sourceline-1)*(size+lead))-(size/3.);
				boxheight= -fy+size;
			}
			else {
				fy= -(size/3.);
				boxheight= -((sourceline-1)*(size+lead))+size+(size/3.);
			}
			news_fillrectangle(fx, fy, boxwidth, boxheight);
		}
		else {
			fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", r, g, b);
			switch (justify) {
			case TEXT_FLUSHLEFT:
			case TEXT_JUSTIFY:
				fprintf(outfp, "/ICEfx ICEblankwidth neg def\n");
				break;
			case TEXT_FLUSHRIGHT:
				fprintf(outfp, "/ICEfx ICEmaxstrwidth neg ICEblankwidth sub def\n");
				break;
			case TEXT_CENTER:
				fprintf(outfp, "/ICEfx ICEmaxstrwidth neg 2 div ICEblankwidth sub def\n");
				break;
			}
			if (lead >= -size) {
				fy= -((sourceline-1)*(size+lead))-(size/3.);
				fprintf(outfp, "/ICEfy %6.4f def\n", fy);
				boxheight= -fy+size;
				fprintf(outfp, "/ICEboxheight %6.4f def\n", boxheight);
			}
			else {
				fy= -(size/3.);
				fprintf(outfp, "/ICEfy %6.4f def\n", fy);
				boxheight= -((sourceline-1)*(size+lead))+size+(size/3.);
				fprintf(outfp, "/ICEboxheight %6.4f def\n", boxheight);
			}
			fprintf(outfp, "newpath\n");
			fprintf(outfp, "ICEfx ICEfy moveto\n");
			fprintf(outfp, "ICEboxwidth 0 rlineto\n");
			fprintf(outfp, "0 ICEboxheight rlineto\n");
			fprintf(outfp, "ICEboxwidth neg 0 rlineto\n");
			fprintf(outfp, "closepath fill\n");
		}
	}

	/* foreground color */
	r= ((float) redfg)/255.;
	g= ((float) greenfg)/255.;
	b= ((float) bluefg)/255.;
	if (outfp == (FILE *) 0)
		news_setrgbcolor(r, g, b);
	else
		fprintf(outfp, "%4.2f %4.2f %4.2f setrgbcolor\n", r, g, b);

	rendererr= 0;
	if (resetsource() != GROBJ_SUCCESS) {
		closesource();
		if (outfp == (FILE *) 0) {
			ps_grestore();
			news_syncserver();
		}
		else
			fprintf(outfp, "restore\n");
		return GROBJ_FAILURE;
	}

	switch (justify) {
	case TEXT_FLUSHLEFT:
	case TEXT_FLUSHRIGHT:
	case TEXT_CENTER:
	case TEXT_JUSTIFY:
		if (outfp == (FILE *) 0) {
			for (fy= 0.; (linelen= getsourceline(outfp)) > 0; fy-= size+lead) {
				int i, nblanks;
				float blankadj;

				if ((linelen == 1) && (sourcebuf[0] == ' '))
					continue;

				switch (justify) {
				case TEXT_FLUSHLEFT:
					news_show(0., fy, sourcebuf);
					break;
				case TEXT_FLUSHRIGHT:
					news_strwidth(sourcebuf, &strwidth);
					fx= -strwidth;
					news_show(fx, fy, sourcebuf);
					break;
				case TEXT_CENTER:
					news_strwidth(sourcebuf, &strwidth);
					fx= -strwidth/2.;
					news_show(fx, fy, sourcebuf);
					break;
				case TEXT_JUSTIFY:
					news_strwidth(sourcebuf, &strwidth);
					for (i= nblanks= 0; i < linelen; i++) {
						if (sourcebuf[i] == ' ')
							nblanks++;
					}
					if ((strwidth == maxstrwidth) ||
				    	(nblanks == 0) ||
				    	(strwidth < maxstrwidth/2.))
						news_show(0., fy, sourcebuf);
					else {
						blankadj= (maxstrwidth-strwidth)/nblanks;

						/* NeWS server limitation --
						   the real magic number seems
						   to be 0.03125, but let's
						   not push our luck */
						if (blankadj < 0.05)
							blankadj= 0.05;

						news_widthshow(0., fy, blankadj, sourcebuf);
					}
					break;
				}
			}
		}
		else {
			for (fy= 0.; (linelen= getsourceline(outfp)) > 0; fy-= size+lead) {
				int i, nblanks;

				if ((linelen == 1) && (sourcebuf[0] == ' '))
					continue;

				switch (justify) {
				case TEXT_FLUSHLEFT:
					fprintf(outfp, "0 %6.4f moveto\n", fy);
					fprintf(outfp, "(%s) show\n", sourcebuf);
					break;
				case TEXT_FLUSHRIGHT:
					fprintf(outfp, "/ICEcurrstrwidth (%s) stringwidth pop def\n", sourcebuf);
					fprintf(outfp, "ICEcurrstrwidth neg %6.4f moveto\n", fy);
					fprintf(outfp, "(%s) show\n", sourcebuf);
					break;
				case TEXT_CENTER:
					fprintf(outfp, "/ICEcurrstrwidth (%s) stringwidth pop def\n", sourcebuf);
					fprintf(outfp, "ICEcurrstrwidth 2 div neg %6.4f moveto\n", fy);
					fprintf(outfp, "(%s) show\n", sourcebuf);
					break;
				case TEXT_JUSTIFY:
					fprintf(outfp, "/ICEcurrstrwidth (%s) stringwidth pop def\n", sourcebuf);
					for (i= nblanks= 0; i < linelen; i++) {
						if (sourcebuf[i] == ' ')
							nblanks++;
					}
					if (nblanks == 0) {
						fprintf(outfp, "0 %6.4f moveto\n", fy);
						fprintf(outfp, "(%s) show\n", sourcebuf);
					}
					else {
						fprintf(outfp, "ICEcurrstrwidth ICEmaxstrwidth eq\n");
						fprintf(outfp, "ICEcurrstrwidth ICEmaxstrwidth 2 div lt or\n{\n");
						fprintf(outfp, "\t0 %6.4f moveto\n", fy);
						fprintf(outfp, "\t(%s) show\n}\n{\n", sourcebuf);
						fprintf(outfp, "\t/ICEblankadj ICEmaxstrwidth ICEcurrstrwidth sub %d div def\n", nblanks);

						/* NeWS server limitation --
						   the real magic number
						   seems to be 0.03125,
						   but let's not push our luck */
						fprintf(outfp, "\tICEblankadj 0.05 lt {\n");
						fprintf(outfp, "\t\t/ICEblankadj 0.05 def } if\n");
						fprintf(outfp, "\t0 %6.4f moveto\n", fy);
						fprintf(outfp, "\tICEblankadj 0 8#040 (%s) widthshow\n} ifelse\n", sourcebuf);
					}
					break;
				}
			}
		}
		if (linelen < 0)
			rendererr= 1;
		break;

	case TEXT_PATH:
		if (path->getclosure() == PATH_CLOSED)
			flags= PATH_DRAWCLOSE | PATH_DRAWNONEWPATH;
		else
			flags= PATH_DRAWNONEWPATH;
		path->draw(flags, outfp);
		(void) strcpy(sourcebuf, "ICEPathTextDict begin\n(");
		if (fillpathbuf(sourcebuf+strlen("ICEPathTextDict begin\n("), TEXT_MAXLINELEN-100) < 1) {
			closesource();
			if (outfp == (FILE *) 0) {
				ps_grestore();
				news_syncserver();
			}
			else
				fprintf(outfp, "restore\n");
			return GROBJ_FAILURE;
		}
		(void) sprintf(psbuf, ") %4.2f %4.2f %4.2f %4.2f PathText\nend\n", pathoffset, letterspace, hs, vs);
		(void) strcat(sourcebuf, psbuf);
		if (outfp == (FILE *) 0)
			psio_write(sourcebuf, 1, strlen(sourcebuf), PostScript);
		else
			fprintf(outfp, "%s", sourcebuf);
		break;
	}

	if (outfp == (FILE *) 0) {
		(void) strcpy(psbuf, " false 1 setfileinputtoken stop\n");
		psio_write(psbuf, 1, strlen(psbuf), PostScript);
		psio_flush(PostScript);
	}

	closesource();

	if (outfp == (FILE *) 0) {
		news_syncserver();

		while (ps_check_PostScript_event() == 1) {
			ps_read_PostScript_event(&tag);
			if (tag == RENDERERR_TAG)
				rendererr= 1;
		}
	}
	else
		fprintf(outfp, "restore\n");

	if (rendererr)
		return GROBJ_FAILURE;
	else
		return GROBJ_SUCCESS;
}

int
Text::opensource()
{
	switch (source) {
	case TEXT_USERINPUT:
		if (text == (char *) 0)
			return GROBJ_FAILURE;
		if (strlen(text) == 0)
			return GROBJ_FAILURE;
		textptr= text;
		break;
	case TEXT_FILEINPUT:
		if (filename == (char *) 0)
			return GROBJ_FAILURE;
		if (strlen(filename) == 0)
			return GROBJ_FAILURE;
		if ((sourcefp= doc_open(filename, "r")) == (FILE *) 0)
			return GROBJ_FAILURE;
		break;
	}
	sourceline= 0;
	return GROBJ_SUCCESS;
}

int
Text::getsourceline(FILE *outfp)
{
	int len, byte, endfound;

	len= 0;
	switch (source) {
	case TEXT_USERINPUT:
		if (*textptr == '\0')
			return len;
		for ( ; len < TEXT_MAXLINELEN; textptr++) {
			if ((*textptr == '\n') ||
			    (*textptr == '\0'))
				break;

			if (*textptr == '\t') {
				int nblanks;

				nblanks= 8-(len%8);
				for ( ; (nblanks > 0) && (len < TEXT_MAXLINELEN); nblanks--)
					sourcebuf[len++]= ' ';
			}
			else if ((outfp != (FILE *) 0) && (*textptr == '\\')) {
				if (len+1 < TEXT_MAXLINELEN) {
					sourcebuf[len++]= '\\';
					sourcebuf[len++]= '\\';
				}
				else {
					sourcebuf[len]= '\0';
					break;
				}
			}
			else if ((outfp != (FILE *) 0) && (*textptr == '(')) {
				if (len+1 < TEXT_MAXLINELEN) {
					sourcebuf[len++]= '\\';
					sourcebuf[len++]= '(';
				}
				else {
					sourcebuf[len]= '\0';
					break;
				}
			}
			else if ((outfp != (FILE *) 0) && (*textptr == ')')) {
				if (len+1 < TEXT_MAXLINELEN) {
					sourcebuf[len++]= '\\';
					sourcebuf[len++]= ')';
				}
				else {
					sourcebuf[len]= '\0';
					break;
				}
			}
			else
				sourcebuf[len++]= *textptr;
		}
		sourcebuf[len]= '\0';

		/* skip to beginning of next line */
		if (*textptr == '\n')
			textptr++;
		else if (*textptr != '\0') {
			while ((*textptr != '\n') && (*textptr != '\0'))
				textptr++;
			if (*textptr == '\n')
				textptr++;
		}
		break;
	case TEXT_FILEINPUT:
		for (endfound= 0; len < TEXT_MAXLINELEN; ) {
			if ((byte= getc(sourcefp)) == EOF) {
				endfound= 1;
				if (feof(sourcefp))
					break;
				else
					return -1;
			}
			if (byte == '\t') {
				int nblanks;

				nblanks= 8-(len%8);
				for ( ; (nblanks > 0) && (len < TEXT_MAXLINELEN); nblanks--)
					sourcebuf[len++]= ' ';
			}
			else if (byte == '\n') {
				endfound= 1;
				if (len == 0)
					sourcebuf[len++]= ' ';
				break;
			}
			else if (byte == '\0') {
				endfound= 1;
				break;
			}
			else if ((outfp != (FILE *) 0) && (byte == '\\')) {
				if (len+1 < TEXT_MAXLINELEN) {
					sourcebuf[len++]= '\\';
					sourcebuf[len++]= '\\';
				}
				else {
					sourcebuf[len]= '\0';
					break;
				}
			}
			else if ((outfp != (FILE *) 0) && (byte == '(')) {
				if (len+1 < TEXT_MAXLINELEN) {
					sourcebuf[len++]= '\\';
					sourcebuf[len++]= '(';
				}
				else {
					sourcebuf[len]= '\0';
					break;
				}
			}
			else if ((outfp != (FILE *) 0) && (byte == ')')) {
				if (len+1 < TEXT_MAXLINELEN) {
					sourcebuf[len++]= '\\';
					sourcebuf[len++]= ')';
				}
				else {
					sourcebuf[len]= '\0';
					break;
				}
			}
			else
				sourcebuf[len++]= (char) byte;

		}
		if (len == 0)
			return len;
		sourcebuf[len]= '\0';

		/* skip to beginning of next line */
		if (!endfound) {
			while ((byte= getc(sourcefp)) != EOF) {
				if (byte == '\n')
					break;
				else if (byte == '\0')
					break;
			}
			if ((byte == EOF) && !feof(sourcefp))
				return -1;
		}
		break;
	}
	sourceline++;
	return len;
}

int
Text::fillpathbuf(char *buf, int maxlen)
{
	int len, byte;

	len= 0;
	while (len < maxlen) {
		if (source == TEXT_USERINPUT)
			byte= (int) *textptr++;
		else {
			if ((byte= getc(sourcefp)) == EOF) {
				if (feof(sourcefp))
					break;
				else
					return -1;
			}
		}

		if (byte == '\0')
			break;
		else if (byte == '\n')
			buf[len++]= ' ';
		else if (byte == '\t') {
			int nblanks;

			nblanks= 8-(len%8);
			for ( ; (nblanks > 0) && (len < maxlen); nblanks--)
				buf[len++]= ' ';
		}
		else if (!isprint(byte))
			buf[len++]= ' ';
		else if (byte == '\\') {
			if (len+1 < maxlen) {
				buf[len++]= '\\';
				buf[len++]= '\\';
			}
			else {
				buf[len]= '\0';
				break;
			}
		}
		else if (byte == '(') {
			if (len+1 < maxlen) {
				buf[len++]= '\\';
				buf[len++]= '(';
			}
			else {
				buf[len]= '\0';
				break;
			}
		}
		else if (byte == ')') {
			if (len+1 < maxlen) {
				buf[len++]= '\\';
				buf[len++]= ')';
			}
			else {
				buf[len]= '\0';
				break;
			}
		}
		else
			buf[len++]= byte;
	}
	buf[len]= '\0';
	return len;
}

int
Text::resetsource()
{
	switch (source) {
	case TEXT_USERINPUT:
		textptr= text;
		break;
	case TEXT_FILEINPUT:
		if (fseek(sourcefp, (long) 0, 0) != 0)
			return GROBJ_FAILURE;
		break;
	}
	sourceline= 0;
	return GROBJ_SUCCESS;
}

void
Text::closesource()
{
	if (source == TEXT_FILEINPUT)
		(void) fclose(sourcefp);
	return;
}

int
Text::icedump(FILE *fp)
{
	float fx, fy, r, h, v;
	int ix, iy, dtk;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;

	if (fp == (FILE *) 0)
		return GROBJ_FAILURE;

	fprintf(fp, "%%%%ICE-Txt: Begin %s\n", getname());

	getloc(&fx, &fy, &ix, &iy);
	fprintf(fp, "%%%%ICE-Txt: Loc %6.4f %6.4f\n", fx, fy);

	r= getrotation();
	getscale(&h, &v);
	fprintf(fp, "%%%%ICE-Txt: Trans %6.4f %6.4f %6.4f\n", r, h, v);

	if (source == TEXT_USERINPUT)
		fprintf(fp, "%%%%ICE-Txt: Text %s\n", text);
	else
		fprintf(fp, "%%%%ICE-Txt: Filenm %s\n", filename);

	fprintf(fp, "%%%%ICE-Txt: Font %1d %s\n", font, fontname);

	fprintf(fp, "%%%%ICE-Txt: Size %1d %4.2f %4.2f\n", fontsz, size, lead);

	fprintf(fp, "%%%%ICE-Txt: Just %1d\n", justify);

	if (path != (Path *) 0)
		fprintf(fp, "%%%%ICE-Txt: Pathnm %s\n", path->getname());
	fprintf(fp, "%%%%ICE-Txt: Path %f %f\n", pathoffset, letterspace);

	fprintf(fp, "%%%%ICE-Txt: FG %1d %1d %1d %1d\n", foreground, (int) redfg, (int) greenfg, (int) bluefg);
	fprintf(fp, "%%%%ICE-Txt: BG %1d %1d %1d %1d %1d\n", backgroundmode, background, (int) redbg, (int) greenbg, (int) bluebg);

	if ((pth= getclip()) != (Path *) 0)
		fprintf(fp, "%%%%ICE-Txt: Clip %s\n", pth->getname());

	getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	fprintf(fp, "%%%%ICE-Txt: DTK %1d %1d %1d %1d\n", dtk, (int) rdtk, (int) gdtk, (int) bdtk);

	fprintf(fp, "%%%%ICE-Txt: Seq %1d\n", getsequence());

	fprintf(fp, "%%%%ICE-Txt: IOT %1d\n", getiotag());

	fprintf(fp, "%%%%ICE-Txt: End\n");
	return GROBJ_SUCCESS;
}

Text::~Text()
{
	Path *cpth;

	if ((cpth= getclip()) != (Path *) 0)
		(void) cpth->setreferences(PATH_REFDECR);
	if (path != (Path *) 0)
		(void) path->setreferences(PATH_REFDECR);
	delete text;
	delete filename;
	delete fontname;
}
