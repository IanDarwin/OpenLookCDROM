/*
 * The routines in this file provide support for ANSI style terminals
 * over a serial line. The serial I/O services are provided by routines in
 * "termio.c". It compiles into nothing if not an ANSI device.
 */

#define	termdef	1			/* don't define "term" external */

#include        <stdio.h>
#include	"estruct.h"
#include        "edef.h"

#if     ANSI

#if	AMIGA
#define NROW    23                      /* Screen size.                 */
#define NCOL    77                      /* Edit if you want to.         */
#else
#define NROW    25                      /* Screen size.                 */
#define NCOL    80                      /* Edit if you want to.         */
#endif
#define	NPAUSE	100			/* # times thru update to pause */
#define	MARGIN	8			/* size of minimim margin and	*/
#define	SCRSIZ	64			/* scroll size for extended lines */
#define BEL     0x07                    /* BEL character.               */
#define ESC     0x1B                    /* ESC character.               */

extern  int     ttopen();               /* Forward references.          */
extern  int     ttgetc();
extern  int     ttputc();
extern  int     ttflush();
extern  int     ttclose();
extern  int     ansimove();
extern  int     ansieeol();
extern  int     ansieeop();
extern  int     ansibeep();
extern  int     ansiopen();
extern	int	ansirev();
extern	int	ansiclose();
extern	int	ansikopen();
extern	int	ansikclose();
extern	int	ansicres();

#if	COLOR
extern	int	ansifcol();
extern	int	ansibcol();

int	cfcolor = -1;		/* current forground color */
int	cbcolor = -1;		/* current background color */

#if	AMIGA
/* apperently the AMIGA does not follow the ANSI standards as
   regards to colors....maybe because of the default pallette
   settings?
*/

int coltran[8] = {2, 3, 5, 7, 0, 4, 6, 1};	/* color translation table */
#endif
#endif

/*
 * Standard terminal interface dispatch table. Most of the fields point into
 * "termio" code.
 */
TERM    term    = {
	NROW-1,
        NROW-1,
        NCOL,
        NCOL,
	MARGIN,
	SCRSIZ,
	NPAUSE,
        ansiopen,
        ansiclose,
	ansikopen,
	ansikclose,
        ttgetc,
        ttputc,
        ttflush,
        ansimove,
        ansieeol,
        ansieeop,
        ansibeep,
	ansirev,
	ansicres
#if	COLOR
	, ansifcol,
	ansibcol
#endif
};

#if	COLOR
ansifcol(color)		/* set the current output color */

int color;	/* color to set */

{
	if (color == cfcolor)
		return;
	ttputc(ESC);
	ttputc('[');
#if	AMIGA
	ansiparm(coltran[color]+30);
#else
	ansiparm(color+30);
#endif
	ttputc('m');
	cfcolor = color;
}

ansibcol(color)		/* set the current background color */

int color;	/* color to set */

{
	if (color == cbcolor)
		return;
	ttputc(ESC);
	ttputc('[');
#if	AMIGA
	ansiparm(coltran[color]+40);
#else
	ansiparm(color+40);
#endif
	ttputc('m');
        cbcolor = color;
}
#endif

ansimove(row, col)
{
        ttputc(ESC);
        ttputc('[');
        ansiparm(row+1);
        ttputc(';');
        ansiparm(col+1);
        ttputc('H');
}

ansieeol()
{
        ttputc(ESC);
        ttputc('[');
        ttputc('K');
}

ansieeop()
{
#if	COLOR
	ansifcol(gfcolor);
	ansibcol(gbcolor);
#endif
        ttputc(ESC);
        ttputc('[');
        ttputc('J');
}

ansirev(state)		/* change reverse video state */

int state;	/* TRUE = reverse, FALSE = normal */

{
#if	COLOR
	int ftmp, btmp;		/* temporaries for colors */
#endif

	ttputc(ESC);
	ttputc('[');
	ttputc(state ? '7': '0');
	ttputc('m');
#if	COLOR
	if (state == FALSE) {
		ftmp = cfcolor;
		btmp = cbcolor;
		cfcolor = -1;
		cbcolor = -1;
		ansifcol(ftmp);
		ansibcol(btmp);
	}
#endif
}

ansicres()	/* change screen resolution */

{
	return(TRUE);
}

spal(dummy)		/* change pallette settings */

{
	/* none for now */
}

ansibeep()
{
        ttputc(BEL);
        ttflush();
}

ansiparm(n)
register int    n;
{
        register int q,r;

        q = n/10;
        if (q != 0) {
		r = q/10;
		if (r != 0) {
			ttputc((r%10)+'0');
		}
		ttputc((q%10) + '0');
        }
        ttputc((n%10) + '0');
}

ansiopen()
{
#if     V7 | USG | BSD
        register char *cp;
        char *getenv();

        if ((cp = getenv("TERM")) == NULL) {
                puts("Shell variable TERM not defined!");
                exit(1);
        }
        if (strcmp(cp, "vt100") != 0) {
                puts("Terminal type not 'vt100'!");
                exit(1);
        }
#endif
	strcpy(sres, "NORMAL");
	revexist = TRUE;
        ttopen();
}

ansiclose()

{
#if	COLOR
	ansifcol(7);
	ansibcol(0);
#endif
	ttclose();
}

ansikopen()	/* open the keyboard (a noop here) */

{
}

ansikclose()	/* close the keyboard (a noop here) */

{
}

#if	FLABEL
fnclabel(f, n)		/* label a function key */

int f,n;	/* default flag, numeric argument [unused] */

{
	/* on machines with no function keys...don't bother */
	return(TRUE);
}
#endif
#else
ansihello()
{
}
#endif
