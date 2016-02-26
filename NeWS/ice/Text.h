/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __TEXT_CLASS__
#define __TEXT_CLASS__

#include "Grobj.h"
#include "Intobj.h"
#include "Path.h"

#define TEXT_MAXLINELEN		(8192)

/* these must begin at 0! */
#define TEXT_USERINPUT		(0)
#define TEXT_FILEINPUT		(1)

/* these must begin at 0! */
#define TEXT_GLOBALFONT		(0)
#define TEXT_OTHERFONT		(1)

/* these must begin at 0! */
#define TEXT_GLOBALFONTSZ	(0)
#define TEXT_OTHERFONTSZ	(1)

/* these must begin at 0! */
#define TEXT_FLUSHLEFT		(0)
#define TEXT_FLUSHRIGHT		(1)
#define TEXT_CENTER		(2)
#define TEXT_JUSTIFY		(3)
#define TEXT_PATH		(4)

/* these must begin at 0! */
#define TEXT_GLOBALFG		(0)
#define TEXT_BLACKFG		(1)
#define TEXT_WHITEFG		(2)
#define TEXT_OTHERFG		(3)

/* these must begin at 0! */
#define TEXT_TRANSPARENTBGM	(0)
#define TEXT_OPAQUEBGM		(1)

/* these must begin at 0! */
#define TEXT_GLOBALBG		(0)
#define TEXT_WHITEBG		(1)
#define TEXT_BLACKBG		(2)
#define TEXT_OTHERBG		(3)

class Text : public Intobj {

	int source;
	char *filename;
	char *text;
	int font;
	char *fontname;
	int fontsz;
	float size;
	float lead;
	int justify;
	Path *path;
	float pathoffset;
	float letterspace;
	int foreground;
	unsigned char redfg, greenfg, bluefg;
	int backgroundmode;
	int background;
	unsigned char redbg, greenbg, bluebg;
	int textlen, textbufsz;

	int opensource();
	int getsourceline(FILE *);
	int fillpathbuf(char *, int);
	int resetsource();
	void closesource();

public:
	/* constructors */
	Text();
	Text(Dlnk **, char *, int);

	int getsource()				{ return source; }
	int setsource(int);
	char *gettext()				{ return text; }
	int settext(char *);
	char *getfilename()			{ return filename; }
	int setfilename(char *);
	void getfont(int *f, char **n)		{ *f= font; *n= fontname; }
	int setfont(int, char *);
	void getfontsize(int *fs, float *s, float *l)
						{ *fs= fontsz;
						  *s= size; *l= lead; }
	int setfontsize(int, float, float);
	int getjustify()			{ return justify; }
	int setjustify(int);
	void getpath(Path **p, float *po, float *l)
						{ *p= path;
						  *po= pathoffset; *l= letterspace; }
	int setpath(Path *, float, float);
	void getforeground(int *fg,
			   unsigned char *r, unsigned char *g, unsigned char *b)
						{ *fg= foreground; *r= redfg;
						  *g= greenfg; *b= bluefg; }
	int setforeground(int, unsigned char, unsigned char, unsigned char);
	void getbackground(int *bgmode, int *bg,
			   unsigned char *r, unsigned char *g, unsigned char *b)
						{ *bgmode= backgroundmode;
						  *bg= background; *r= redbg;
						  *g= greenbg; *b= bluebg; }
	int setbackground(int, int, unsigned char, unsigned char, unsigned char);

	int draw(FILE *);
	int icedump(FILE *);

	/* destructor */
	~Text();
};

#endif __TEXT_CLASS__
