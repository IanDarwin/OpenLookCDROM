/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/eq/RCS/draweqv.c,v 2.14 1993/12/15 23:25:49 gk5g Exp $";
#endif


 

/*
 * draweqv.c
 * This module implements the eq drawing routines.
 */


#include <andrewos.h>
#include <class.h>
#include <eq.ih>
#include <exfont.h>
#include <fontdesc.ih>
#include <graphic.ih>
#include <view.ih>
#include <rectlist.ih>
#define AUXMODULE 1
#include <eqv.eh>

static struct graphic *pat;

/*
paren around one of the sums is too big
sup for ordinary parens is too high;  don't always center
compare 1/n with n/n
extra space after n in 1/n?
belows don't seem to add to size!
aboves don't add size properly!
not enough space after sin, sum
*/


/*
 * group	"{" [ align ] [ eqstyle ] item ... "}"
 * align	[ "hlist" ] | "cpile" | "lpile" | "rpile"
 * eqstyle	"d_eqstyle" | "t_eqstyle" | "s_eqstyle" | "ss_eqstyle"
 * item		atom [ "sup" item ] [ "sub" item ] ...
 * atom		group | simple | extendable
 * simple	"a" | "alpha" | "log" | "sum" | "+" | ...
 * extendable	"(" | ")" | "over" | ...
 *
 * The *reference point* for a group is the point wrt which (the reference
 * points of) all items of the group are positioned.  The *origin* of a group
 * is a point on the baseline for aligning items in an hlist group.  The
 * offset from the origin to the reference point of a group f is left in
 * f->pos after f is formatted.
 *
 * For hlist ref.pt. = origin of first item = origin of the hlist
 * For lpile ref.pt. = top left of bounding box
 * For cpile ref.pt. = top center of bounding box
 * For rpile ref.pt. = top right of bounding box 
 *
 * The origin of a simple atom is the font-defined origin of the first
 * character in the string for the atom, and is coincident with the
 * reference point.
 *
 * Dimensions are relative to the reference point.
 * sup and sub positons are positions of origin of sup or sub.
 * above position is center of bottom of above
 * below position is center of top of below
 */

/*
 * Reference point.
 * Origin.
 * Baseline.
 * Axis.
 */



/*
 * eqstyles
 */

enum eqstyle enscript[] = { S_EQSTYLE, S_EQSTYLE, SS_EQSTYLE, SS_EQSTYLE };
enum eqstyle enpile[] = { T_EQSTYLE, S_EQSTYLE, SS_EQSTYLE, SS_EQSTYLE };


/*
 * Equations
 */

struct pos zero = { 0, 0 };
struct pos small = { -10000, -10000 };
struct pos large = { 10000, 10000 };


/*
 * Knuth's spacing table, The TeXbook p. 170.
 * We map Knuth's space classifications to enum space
 * as described below.
 */

enum space spacing[(int)MAX_genre][(int)MAX_genre] = {
    /*		ORD	OP	BIN	REL	OPEN	CLOSE	PUNCT	INNER */
    /* ORD */	NOSPC,	THIN,	CMED,	CTHICK,	NOSPC,	NOSPC,	NOSPC,	CTHIN,
    /* OP */	THIN,	THIN,	EQBAD,	CTHICK,	NOSPC,	NOSPC,	NOSPC,	CTHIN,
    /* BIN */	CMED,	CMED,	EQBAD,	EQBAD,	CMED,	EQBAD,	EQBAD,	CMED,
    /* REL */	CTHICK,	CTHICK,	EQBAD,	NOSPC,	CTHICK,	NOSPC,	NOSPC,	CTHICK,
    /* OPEN */	NOSPC,	NOSPC,	EQBAD,	NOSPC,	NOSPC,	NOSPC,	NOSPC,	NOSPC,
    /* CLOSE */	NOSPC,	THIN,	CMED,	CTHICK,	NOSPC,	NOSPC,	NOSPC,	CTHIN,
    /* PUNCT */	CTHIN,	CTHIN,	EQBAD,	CTHICK,	CTHIN,	CTHIN,	CTHIN,	CTHIN,
    /* INNER */	CTHIN,	THIN,	CMED,	CTHICK,	CTHIN,	NOSPC,	CTHIN,	CTHIN
};

/* Horizontal spacing.  Usage: KNUTH_SPACE(left, right, eqstyle) */

#define KNUTH_SPACE(left, right, eqstyle) h_space \
    [(int)spacing[(int)((left)->symbol->genre)][(int)((right)->symbol->genre)]] \
    [(int)(eqstyle)]

int h_space[(int)MAX_space][(int)MAX_eqstyle] = {
    /*				d	t	s	ss */
    /* EQBAD, */			0,	0,	0,	0,	
    /* NOSPC */			0,	0,	0,	0,	
    /* CTHIN */			3,	2,	0,	0,
    /* THIN */			3,	2,	1,	1,
    /* CMED */			5,	3,	0,	0,
    /* MED */			5,	3,	2,	2,
    /* CTHICK */		7,	4,	0,	0,
    /* THICK */			7,	4,	3,	3
};

/* Vertical spacing.  Usage: v_thin_space[eqstyle] */
int v_thin_space[(int)MAX_eqstyle] = { 2, 2, 1, 1 };
int v_thick_space[(int)MAX_eqstyle] = { 10, 3, 1, 1 };

long exHeightTable[] = {
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
     -2,  -4,  -5,  -8, -13,  -2,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,  10,   9,   9,
      0,   0,   0,   0,   0,  10,   9,   9,
      0,   0,   0,   0,   0,  10,   9,   9,
      0,   0,   0,   0,   0,  10,   9,   9,
      0,   0,   0,   0,   0,  10,   9,   9,
      0,   0,   0,   0,   0,  10,   9,   9,
      0,   0,   0,   0,   0,   8,   9,   7,
      0,   0,   0,   0,   0,   8,   9,   7,
     18,   9,   3,   0,  18,   9,   3,   0,
    -20, -10,  -5,  -1,   0,   0,   0,   0,
     16,   8,   4,   1,  16,   8,   4,   1,
      0,   0,   0,   0,   0,   0,   0,   0
};
#ifdef NOTUSED
static void ZeroSpacing() {  /* for debugging */
    int i, j;
    for (i=0; i<(int)MAX_eqstyle; i++) {
	v_thin_space[i] = v_thick_space[i] = 0;
	for (j=0; j<(int)MAX_space; j++)
	    h_space[j][i] = 0;
    }
}
#endif /* NOTUSED ? */

/*
 * Fonts
 */

struct {
    char *fontfamily;
    long fontstyle;
    long fontsize;
    struct fontdesc *font;
} fonts [MAX_simple][MAX_eqstyle] = {
    /* D_EQSTYLE		T_EQSTYLE            S_EQSTYLE            SS_EQSTYLE */
    /* ITALIC */
    "Andy",fontdesc_Italic,12,0, "Andy",fontdesc_Italic,12,0, "Andy",fontdesc_Italic,10,0, "Andy",fontdesc_Italic,8,0,
    /* ROMAN */
    "Andy",fontdesc_Plain,12,0, "Andy",fontdesc_Plain,12,0, "Andy",fontdesc_Plain,10,0, "Andy",fontdesc_Plain,8,0,
    /* BOLD */
    "Andy",fontdesc_Bold,12,0, "Andy",fontdesc_Bold,12,0, "Andy",fontdesc_Bold,10,0, "Andy",fontdesc_Bold,8,0,
    /* SYMBOL */
    "AndySymbol",fontdesc_Plain,12,0, "AndySymbol",fontdesc_Plain,12,0, "AndySymbol",fontdesc_Plain,10,0, "AndySymbol",fontdesc_Plain,8,0,
    /* SYMBOLA */
    "AndySymbolA",fontdesc_Plain,12,0, "AndySymbolA",fontdesc_Plain,12,0, "AndySymbolA",fontdesc_Plain,10,0, "AndySymbolA",fontdesc_Plain,8,0,
    /* SYM */
    "Sym",fontdesc_Bold,12,0, "Sym",fontdesc_Plain,12,0, "Sym",fontdesc_Plain,10,0, "Sym",fontdesc_Plain,8,0

};
    
#define FONT(t,s) (fonts[(int)t][(int)s].font ? \
    fonts[(int)t][(int)s].font : \
    (fonts[(int)t][(int)s].font = fontdesc_Create(fonts[(int)t][(int)s].fontfamily, fonts[(int)t][(int)s].fontstyle, fonts[(int)t][(int)s].fontsize)))

int eqstyle_sup[MAX_eqstyle];			/* y superscript positions */
int eqstyle_top[MAX_eqstyle];			/* top of most characters */
int eqstyle_big_sup[MAX_eqstyle];		/* offset for sup for big ops */
int eqstyle_sub[MAX_eqstyle];			/* y subscript positions */
int eqstyle_axis[MAX_eqstyle];			/* y axis positions */
int eqstyle_min_paren[MAX_eqstyle] = { 2, 2, 1, 0 };/* smallest paren per eqstyle */
int have_eqstyle[MAX_eqstyle] = { 0, 0, 0, 0 };	/* have above been computed? */

static void InitFont(self, eqstyle)
struct eqview *self;
enum eqstyle eqstyle;
{
    struct fontdesc_charInfo info;

    struct fontdesc *font = FONT(ITALIC,eqstyle);
    struct fontdesc *tempfont = FONT(ITALIC,enscript[(int)eqstyle]);
    struct fontdesc *symfont = FONT(SYMBOL,eqstyle);

#define SCRIPT_EXTRA 1

    /* baseline of sub is wrt 'p' */
    fontdesc_CharSummary(font, eqview_GetDrawable(self), 'p', &info);
    eqstyle_sub[(int)eqstyle] = info.height - info.yOriginOffset - 1 + SCRIPT_EXTRA;
    
    /* eqstyle top */
    fontdesc_CharSummary(font, eqview_GetDrawable(self), 'b', &info);
    eqstyle_top[(int)eqstyle] = -info.yOriginOffset;

    /* distance from top of char to sup baseline ... */
    fontdesc_CharSummary(tempfont, eqview_GetDrawable(self), 'x', &info);
    eqstyle_big_sup[(int)eqstyle] = info.yOriginOffset;

    /* ... wrt eqstyle_top in most cases */
    eqstyle_sup[(int)eqstyle] = eqstyle_top[(int)eqstyle] + eqstyle_big_sup[(int)eqstyle] - SCRIPT_EXTRA;

    /* axis */
    fontdesc_CharSummary(symfont, eqview_GetDrawable(self), '-', &info);
    eqstyle_axis[(int)eqstyle] = - info.yOriginOffset;

    have_eqstyle[(int)eqstyle] = 1;
}


/*
 * Find the dimensions of a simple atom
 */

static void eqview_FormatSimple(self, f, eqstyle)
struct eqview *self;
struct formula *f;			/* printable equation */
enum eqstyle eqstyle;			/* in what eqstyle */
{
    struct fontdesc *font = FONT(f->symbol->what, eqstyle);
    struct fontdesc_charInfo info;
    char *s;
    int i, x = 0;

    if (f->symbol->type!=SIMPLE)
	abort();

    f->min = large,  f->max = small;
    f->font = FONT(f->symbol->what, eqstyle);

    for (s=f->symbol->string; *s; s++) {
	fontdesc_CharSummary(font, eqview_GetDrawable(self), *s, &info);
	if ((i = x - info.xOriginOffset) < f->min.x)
	    f->min.x = i;
	if ((i += info.width - 1) > f->max.x)
	    f->max.x = i;
	if ((i = - info.yOriginOffset) < f->min.y)
	    f->min.y = i;
	if ((i += info.height - 1) > f->max.y)
	    f->max.y = i;
	x += info.xSpacing;
    }

    if (!have_eqstyle[(int)eqstyle])
	InitFont(self, eqstyle);

    f->kern = f->max.x - f->min.x - x + 2;
    f->sup_y = eqstyle_sup[(int)eqstyle];
    f->sub_y = eqstyle_sub[(int)eqstyle];

    /* big ops in sym12b */
    if (f->sub_y < f->max.y) {
	f->sup_y = eqstyle_big_sup[(int)eqstyle] + f->min.y - SCRIPT_EXTRA;
	f->sub_y = f->max.y + SCRIPT_EXTRA;
    }

}

/*
 * Generate extender of type 'code' and size 'size' into 'string'
 */

struct {
    char *fontfamily;
    long fontstyle;
    long fontsize;
    struct fontdesc *font;
} exfont [1] = {
    "ex",fontdesc_Plain,12,NIL
};

#define EXFONT(n) (exfont[n].font ? exfont[(int)n].font \
    : (exfont[(int)n].font = fontdesc_Create(exfont[(int)n].fontfamily, exfont[(int)n].fontstyle, exfont[(int)n].fontsize)))


long eqview_Extender(self, s, stringp, code, size)
struct eqview *self;
char *s;
char **stringp;
long code, size;
{
    int i, left=size;
    for (i=0; i<EX_NEXTENDERS; i++) {
	int c = code+i;
	int n;
	struct fontdesc_charInfo info;

	fontdesc_CharSummary(EXFONT(0), eqview_GetDrawable(self), c, &info);
	n = MAX(ABS(info.xSpacing), ABS(exHeightTable[c]));
	if (n)
	    while (left>=n) {
		*s++ = c;
		left -= n;
	    }
    }
    *s = 0;
    if (stringp)
	*stringp = s;
    return size - left;
}


/*
 * Zero size
 */

static void eqview_Zero(f)
struct formula *f;
{
    /* f->pos = f->min = f->max = zero has bugs on rt */
    f->pos = zero;
    f->min = zero;
    f->max = zero;
    f->sup_y = f->sub_y = f->kern = 0;
}


/*
 * Find the dimensions of an extendable atom
 */

char *eqview_Extendable(self, f, leftf, rightf, eqstyle, ext1, ext2, hang, one_part_extender)
struct eqview *self;
struct formula *f, *leftf, *rightf;
enum eqstyle eqstyle;
char ext1, ext2;
long hang;		/* whether to hang (root) or center (paren) */
long one_part_extender;	/* whether extender has only one part */
{
    struct fontdesc_charInfo info;
    struct FontSummary *summary;

    int c = f->symbol->what;
    int i, j, x, y, h_left_top, h_left_bot, h_right_top, h_right_bot,
	h_left, h_right, height;
    char *s = f->string;
    int actual;

    if (!have_eqstyle[(int)eqstyle])
	InitFont(self, eqstyle);

    /* calculate height */
    h_left_top
	= (leftf? eqstyle_axis[(int)eqstyle] - (leftf->pos.y + leftf->min.y) : 0);
    h_left_bot
	= (leftf? (leftf->pos.y + leftf->max.y) - eqstyle_axis[(int)eqstyle] : 0);
    h_left = h_left_top + h_left_bot + 1;
    h_right_top
	= (rightf? eqstyle_axis[(int)eqstyle] - (rightf->pos.y + rightf->min.y) : 0);
    h_right_bot
	= (rightf? (rightf->pos.y + rightf->max.y) - eqstyle_axis[(int)eqstyle] : 0);
    h_right = h_right_top + h_right_bot + 1;
    height = MAX(h_left, h_right);

    /* look at the fixed parens */
    for (i=eqstyle_min_paren[(int)eqstyle]; i<EX_NPARENS-3; i++) {
	fontdesc_CharSummary(EXFONT(0), eqview_GetDrawable(self), c+i, &info);
	if (info.height >= height)
	    break;
    }

    if (i<EX_NPARENS-3) {
	/* use a fixed paren */
	f->pos.x = 0;
	f->min.x = - info.xOriginOffset;
	f->max.x = f->min.x + info.width - 1;
	f->min.y = - info.yOriginOffset;
	f->max.y = f->min.y + info.height - 1;
	if (hang && rightf)
	    f->pos.y = rightf->pos.y + rightf->min.y - f->min.y;
	else
	    f->pos.y = eqstyle_axis[(int)eqstyle] - (f->min.y+f->max.y)/2; 
	f->sup_y = eqstyle_sup[(int)eqstyle] - f->pos.y;
	f->sub_y = eqstyle_sub[(int)eqstyle] - f->pos.y;
	if (f->sub_y < f->max.y) {
	    f->sup_y = eqstyle_big_sup[(int)eqstyle] + f->min.y - SCRIPT_EXTRA;
	    f->sub_y = f->max.y + SCRIPT_EXTRA;
	}
	f->kern = 0;
	*s++ = c+i;
	*s = 0;
    } else {
	/* use an extendable paren */

	/* subtract height of fixed part */
	for (i=EX_NPARENS-3; i<EX_NPARENS; i++) {
	    fontdesc_CharSummary(EXFONT(0), eqview_GetDrawable(self), c+i, &info);
	    height -= info.height;
	}

	/* build string, compute rest of height */
	*s++ = c+EX_NPARENS-3;
	actual = eqview_Extender(self, s, &s, ext1,
	    (one_part_extender? height : height/2));
	*s++ = c+EX_NPARENS-2;
	actual = eqview_Extender(self, s, &s, ext2, height-actual);
	*s++ = c+EX_NPARENS-1;
	*s = 0;

	/* compute x dimensions */
	f->max.y = -1,  f->min.y = 0,  y = 0;
	f->max.x = -1,  f->min.x = 0,  x = 0;
	for (s=f->string;  *s;  s++) {
	    struct fontdesc *tempfont = EXFONT(0);
	    
	    fontdesc_CharSummary(tempfont, eqview_GetDrawable(self), *s, &info);
	    if ((j = x - info.xOriginOffset) < f->min.x)
		f->min.x = j;
	    if ((j += info.width-1) > f->max.x)
		f->max.x = j;
	    x += info.xSpacing;
	    if ((j = y - info.yOriginOffset) < f->min.y)
		f->min.y = j;
	    if ((j += info.height-1) > f->max.y)
		f->max.y = j;
	    y += exHeightTable[*s];
	}

	/* center adjacent items */

	summary = fontdesc_FontSummary(FONT(ITALIC, eqstyle), eqview_GetDrawable(self));

	if (leftf && ABS(h_left_top-h_left_bot) > 2*summary->maxHeight)
	    leftf->pos.y += (h_left_top-h_left_bot)/2;
	if (rightf && ABS(h_right_top-h_right_bot) > 2*summary->maxHeight)
	    rightf->pos.y += (h_right_top-h_right_bot)/2;

	/* other stuff */
	f->pos.x = 0;
	if (hang && rightf)
	    f->pos.y = rightf->pos.y + rightf->min.y - f->min.y;
	else
	    f->pos.y = eqstyle_axis[(int)eqstyle] - (f->min.y+f->max.y)/2;
	f->sup_y = eqstyle_big_sup[(int)eqstyle] + f->min.y - SCRIPT_EXTRA;
	f->sub_y = f->max.y + SCRIPT_EXTRA;
	f->kern = 0;
    }

    f->verticalExtend = TRUE;
    return s;
}


static void eqview_FormatExtendable(self, leftf, f, rightf, eqstyle)
struct eqview *self;
struct formula *leftf, *f, *rightf;
enum eqstyle eqstyle;
{
    int lsize, rsize;
    char *s;

    if (f->symbol->type!=EXTEND)
	abort();

    eqview_Zero(f);
    f->font = EXFONT(0);
    f->verticalExtend = FALSE;

#define EXSTRINGSIZE 100
#define H_EXTRA 1	/* extra horizontal between all boxes */
#define V_EXTRA 0	/* extra vertical between all boxes */

    /* if (!f->string) how gets zeroed? */
	f->string = (char *) malloc(EXSTRINGSIZE);

    switch (f->symbol->what) {
    case EX_BAR:
	lsize = (leftf? leftf->max.x - leftf->min.x : 0) + 1 - 2*H_EXTRA;
	rsize = (rightf? rightf->max.x - rightf->min.x : 0) + 1 - 2*H_EXTRA;
	f->min.x = 0;
	f->max.x = eqview_Extender(self, f->string, NIL, EX_HORIZ1, MAX(lsize,rsize)) - 1;
        break;
    case EX_OVER:
	lsize = (leftf? leftf->max.x - leftf->min.x : 0) + 1;
	rsize = (rightf? rightf->max.x - rightf->min.x : 0) + 1;
	f->min.x = 0;
	f->max.x = eqview_Extender(self, f->string, NIL, EX_HORIZ1, MAX(lsize,rsize)) - 1;
	break;	
    case EX_VINCULUM:
	lsize = (leftf? leftf->max.x - leftf->min.x : 0) + 1 + 2*H_EXTRA;
	rsize = (rightf? rightf->max.x - rightf->min.x : 0) + 1 + 2*H_EXTRA;
	f->min.x = 2*H_EXTRA /* a lie */;
	f->max.x = eqview_Extender(self, f->string, NIL, EX_HORIZ1, MAX(lsize,rsize)) - 1;
	break;
    case EX_CTHIN:
    case EX_THIN:
    case EX_CMED:
    case EX_MED:
    case EX_CTHICK:
    case EX_THICK:
	f->max.x = h_space[f->symbol->what-EX_SPACE][(int)eqstyle];
	break;
    case EX_LPAREN:
    case EX_LSQUARE:
    case EX_LBRACE:
	eqview_Extendable(self, f, NIL, rightf, eqstyle, EX_VERT1, EX_VERT1, 0, 0);
	break;
    case EX_RPAREN:
    case EX_RSQUARE:
    case EX_RBRACE:
	eqview_Extendable(self, f, leftf, NIL, eqstyle, EX_VERT1, EX_VERT1, 0, 0);
	break;
    case EX_LANGLE:
	eqview_Extendable(self, f, NIL, rightf, eqstyle, EX_ANGLE_DL, EX_ANGLE_DR, 0, 0);
	break;
    case EX_RANGLE:
	eqview_Extendable(self, f, leftf, NIL, eqstyle, EX_ANGLE_DR, EX_ANGLE_DL, 0, 0);
	break;
    case EX_SLASH:
	eqview_Extendable(self, f, leftf, rightf, eqstyle, EX_SLASH_DL, EX_SLASH_DL, 0, 1);
	break;
    case EX_ROOT:
	rightf->min.y = MIN(rightf->min.y, eqstyle_top[(int)eqstyle])
	    - v_thin_space[(int)eqstyle] - 1;
	rightf->max.x += h_space[(int)CTHIN][(int)eqstyle];
	s = eqview_Extendable(self, f, NIL, rightf, eqstyle, EX_ROOT_EXT, EX_ROOT_EXT, 1, 1);
	rsize = rightf->max.x - rightf->min.x + 1
	    + KNUTH_SPACE(f, rightf, eqstyle);
	eqview_Extender(self, s, NIL, EX_HORIZ1, rsize);
	break;
    }
}



static void eqview_MinMax(f)
struct formula *f;
{
    f->pos = zero;
    f->min = large;
    f->max = small;
    f->sup_y = f->sub_y = f->kern = 0;
}



/*
 * Format a group.  Calculate dimensions.
 * Return a pointer to the end of the group.
 */

#define f2sub (f2script[(int)SUB])
#define f2sup (f2script[(int)SUP])
#define f2above (f2script[(int)ABOVE])
#define f2below (f2script[(int)BELOW])

static struct formula *eqview_FormatGroup(self, eqptr, f, eqstyle)
struct eqview *self;
struct eq *eqptr;
struct formula *f;
enum eqstyle eqstyle;
{
    enum align current_alignment = HLIST;
    enum eqstyle current_eqstyle = eqstyle;
    struct formula *f1, *f2, *f3;
    struct formula *firstf, *lastf = NULL;
    struct formula *f2script[MAX_script], *nextf = NULL;
    int i;
    int group_axis_y = 0, pile_axis_y = 0;

    struct formula *eqview_Format();


    /* initialize */
    f1 = NIL,  f2 = f,  f3 = eq_NextFormula(eqptr, f);
    f->min = f->max = zero;
    f->sup_y = f->sub_y = f->kern = 0;

    /* alignment op */
    if (f3->symbol->type==ALIGN) {
	if (!have_eqstyle[(int)current_eqstyle])
	    InitFont(self, current_eqstyle);
	group_axis_y = eqstyle_axis[(int)current_eqstyle];
	pile_axis_y = 0;
	current_alignment = (enum align) f3->symbol->what;
	current_eqstyle = enpile[(int)current_eqstyle];
	f3 = eq_NextFormula(eqptr, f3);
    }

    /* eqstyle op */
    if (f3->symbol->type==EQSTYLE) {
	current_eqstyle = (enum eqstyle) f3->symbol->what;
	f3 = eq_NextFormula(eqptr, f3);
    }

    /* initialize */
    eqview_MinMax(f2);
    firstf = f3; /* ? */

    while (f2) {

	/* f2 scripts (f2s) */
	for (i=0; i<(int)MAX_script; i++)
	    f2script[i] = NIL;
	while (f3 && f3->symbol->type==SCRIPT) {
	    f2script[f3->symbol->what] = eq_NextFormula(eqptr, f3);
	    f3 = eq_NextFormula(eqptr, eqview_Format(self, eqptr, f2, eq_NextFormula(eqptr, f3), NIL, enscript[(int) current_eqstyle]));
	}

	/* f3 dimensions if possible, and nextf */
	if (f3) switch (f3->symbol->type) {
	case END:
	    eqview_Zero(f3);
	    nextf = NIL;
	    break;
	case BEGIN:
	case SIMPLE:
	    nextf = eq_NextFormula(eqptr, eqview_Format(self, eqptr, NIL, f3, NIL, current_eqstyle));
	    break;
	case EXTEND:
	    eqview_Zero(f3);  /* for now */
	    nextf = eq_NextFormula(eqptr, f3);
	    break;
	default:
	    abort();
	}

	/* ref to f2 above assume f2 not extendable */
	/* ref to f3 below assume f3 not extendable */

	/* f2 dimensions if necessary */
	if (f2->symbol->type==EXTEND)
	    eqview_Format(self, eqptr, f1, f2, f3, current_eqstyle);

	/* script position (f2script[]->pos) wrt f2 pos (f2->pos) */
	if (f2sup) {
	    f2sup->pos.x = f2->max.x - f2sup->min.x + 1 + H_EXTRA;
	    f2sup->pos.y = f2->sup_y;
	}
	if (f2sub) {
	    f2sub->pos.x = f2->max.x - f2sub->min.x + 1 - f2->kern + H_EXTRA;
	    f2sub->pos.y = f2->sub_y;
	}
	if (f2above) {
	    f2above->pos.x = (f2->min.x + f2->kern + f2->max.x)/2
		- (f2above->max.x + f2above->min.x)/2;
	    f2above->pos.y = f2->min.y - f2above->max.y - 1
		- V_EXTRA - v_thin_space[(int)eqstyle];
	}
	if (f2below) {
	    f2below->pos.x = (f2->min.x + f2->max.x - f2->kern)/2
		- (f2below->max.x + f2below->min.x)/2;
	    f2below->pos.y = f2->max.y - f2below->min.y + 1
		+ V_EXTRA + v_thin_space[(int)eqstyle];
	}

	/* f2 dimensions as modified by scripts */
	for (i=0; i<(int)MAX_script; i++) {
	    int j;
	    if (f2script[i]) {
		if ((j=f2script[i]->pos.x+f2script[i]->max.x) > f2->max.x)
		    f2->max.x = j;
		if ((j=f2script[i]->pos.x+f2script[i]->min.x) < f2->min.x)
		    f2->min.x = j;
		if ((j=f2script[i]->pos.y+f2script[i]->max.y) > f2->max.y)
		    f2->max.y = j;
		if ((j=f2script[i]->pos.y+f2script[i]->min.y) < f2->min.y)
		    f2->min.y = j;
	    }
	}

	/* f2 position, and pile_axis_y */
	if (f1)	switch (current_alignment) {
	case HLIST:
	    if (f1==f)  /* first; min size for empty group would go here */
		f2->pos.x = - f2->min.x;
	    else if (f3!=NIL)  /* middle */
		f2->pos.x = f1->pos.x + f1->max.x - f2->min.x + 1 + H_EXTRA
		    + KNUTH_SPACE(f1, f2, eqstyle);
	    else  /* last */
		f2->pos.x = f1->pos.x + f1->max.x - f2->min.x;
	    f2->pos.y += 0;
	    break;
	case LPILE:
	    f2->pos.x = -(f2->min.x);
	    goto pile;
	case CPILE:
	    f2->pos.x = -((f2->max.x + f2->min.x)/2);
	    goto pile;
	case RPILE:
	    f2->pos.x = -(f2->max.x);
	    goto pile;
	pile:
	    if (f1==f)  /* first */
		f2->pos.y = -f2->min.y;
	    else if (f3!=NIL)  /* middle */
		f2->pos.y = f1->pos.y + f1->max.y - f2->min.y + 1 + V_EXTRA +
		    ((f2->symbol->type==EXTEND || f1->symbol->type==EXTEND) ?
			v_thin_space[(int)current_eqstyle] :
			v_thick_space[(int)current_eqstyle]);
	    else  /* last */
		f2->pos.y = f1->pos.y + f1->max.y - f2->min.y;
	    if (f2->symbol->type==EXTEND)
		pile_axis_y = f2->pos.y;
	}

	/* script positions (f2script[]->pos) relative to group */
	for (i=0; i<(int)MAX_script; i++) {
	    if (f2script[i]) {
		f2script[i]->pos.x += f2->pos.x;
		f2script[i]->pos.y += f2->pos.y;
	    }
	}

	/* our dimensions */
	if ((i=f2->pos.y+f2->min.y) < f->min.y) f->min.y = i;
	if ((i=f2->pos.y+f2->max.y) > f->max.y) f->max.y = i;
	if ((i=f2->pos.x+f2->min.x) < f->min.x) f->min.x = i;
	if ((i=f2->pos.x+f2->max.x) > f->max.x) f->max.x = i;
	if ((i=f2->pos.y+f2->sup_y) < f->sup_y) f->sup_y = i;
	if ((i=f2->pos.y+f2->sub_y) > f->sub_y) f->sub_y = i;

	/* shift */
	lastf = f1;
	f1 = f2,  f2 = f3,  f3 = nextf;
    }

    f->kern = (firstf->kern + lastf->kern)/2;  /* ? */
    if (current_alignment != HLIST) { /* i.e., pile */
	if (pile_axis_y == 0)
	    pile_axis_y = (f->max.y+f->min.y)/2;
	f->pos.y = group_axis_y - pile_axis_y;
    }

    return f1;
}


/*
 * Format an atom:  assign values to f.
 * Return pointer to last formula of group.
 */

/* static */struct formula *eqview_Format(self, eqptr, leftf, f, rightf, eqstyle)
struct eqview *self;
struct eq *eqptr;
struct formula *leftf, *f, *rightf;
enum eqstyle eqstyle;
{
    f->pos = zero;   /* ? */

    switch (f->symbol->type) {
    case BEGIN:
	return eqview_FormatGroup(self, eqptr, f, eqstyle);
    case SIMPLE:
	eqview_FormatSimple(self, f, eqstyle);
	return f;
    case EXTEND:
	eqview_FormatExtendable(self, leftf, f, rightf, eqstyle);
	return f;
    default:
	abort();
    }
}

/*
 * Calculate some user interface stuff.
 *
 * In the following, H means has a hot spot,
 * spot, T means transparent, e.g. can cut across, can select across.
 *
 * x sup { ... }  { cpile { ... } over { ... } }  root { ... }    ( { ... } )
 * H  .  .     H  H   .   .     H  .   .     H .    H  .     H    H .     H .
 *                D                            D       D            D     D
 *                                                                  T     T
 *
 * { x y } above bar
 * H H H H   .    .
 */

struct formula *eqview__Draw(self, eqptr, f, x, y)
struct eqview *self;
struct eq *eqptr;
struct formula *f;
long x, y;
{
    struct formula *prev = 0, *this = f, *next = 0, *begin = 0;
    int in_align = 0;

    if (this->symbol->type==BEGIN) {
	x += this->pos.x,  y += this->pos.y;
	begin = this;
	this = eq_NextFormula(eqptr, this);
    }

    while (1) {
	this->hot.y = y;
	if (prev)
	    this->hot.x = x + (prev->pos.x+prev->max.x
		+ this->pos.x+this->min.x + 1) / 2;
	else
	    this->hot.x = x - 1;
	next = this;
	this->deletable = FALSE;
	this->transparent = FALSE;
	this->has_hot_spot = FALSE;
	switch (this->symbol->type) {
	    case BEGIN:
		if (prev && prev->symbol->genre == OPEN) {
		    this->deletable = TRUE;
		    if (prev->symbol != root)
			this->transparent = TRUE;
		}
		next = eqview_Draw(self, eqptr, this, x, y);
		next->has_hot_spot = !this->has_hot_spot;
		next->transparent = this->transparent;
		if (this->deletable && ! (prev && prev->symbol == root))
		    next->deletable = TRUE;
		break;
	    case END:
		this->hot.x = x + this->pos.x + 3;
		return this;
	    case SIMPLE:
		eqview_SetFont(self, this->font);
		eqview_MoveTo(self, x + this->pos.x, y + this->pos.y);
		eqview_DrawString(self, this->symbol->string, view_ATLEFT|view_ATBASELINE);
		if (!in_align && !(prev && prev->symbol->type==SCRIPT))
		    this->has_hot_spot = TRUE;
		break;
	    case EXTEND:
		eqview_SetFont(self, this->font);
		eqview_MoveTo(self, x + this->pos.x, y + this->pos.y);
		if (this->verticalExtend)  {
		    register char *s = this->string;
		    register long xPos = x + this->pos.x;
		    register long yPos = y + this->pos.y;
		    struct fontdesc_charInfo info;

		    while (*s != '\0')  {
			eqview_DrawText(self, s, 1, view_ATLEFT|view_ATBASELINE);
			fontdesc_CharSummary(this->font, eqview_GetDrawable(self), *s, &info);
			xPos += info.xSpacing;
			yPos += exHeightTable[*s];
			eqview_MoveTo(self, xPos, yPos);
			s++;
		    }
		}
		else  {
		    eqview_DrawString(self, this->string, view_ATLEFT|view_ATBASELINE);
		}
		if (!in_align		/* close parens do not get hot spots */
		  && !(this->symbol->genre==CLOSE && prev
		    && prev->symbol->type==BEGIN && prev->transparent)
		  && !(prev && prev->symbol->type==SCRIPT))
		    this->has_hot_spot = TRUE;
		break;
	    case ALIGN:
		in_align = 1;
		begin->deletable = TRUE;
		begin->has_hot_spot = TRUE;	/* assumes contained in hlist */
		begin->transparent = FALSE;
		break;
	    case SCRIPT:
	    case EQSTYLE:
		break;
	    default:
		abort();
	}
	prev = this;
	this = eq_NextFormula(eqptr, next);
    }
}
	

/*
 * Find a mouse click
 */

long eqview__Find(self, eqptr, mx, my, restrict)
struct eqview *self;
struct eq *eqptr;
long mx, my, restrict;
{
    register int i;
    long n = eq_Size(eqptr);
    int nearest_i = -1, nearest_distance = 1000000;

    if (restrict) do
	restrict = eq_FindBeginGroup(eqptr, restrict);
    while (eq_Access(eqptr, restrict)->transparent);

    for (i=restrict+1; i<n; i++) {
	register struct formula *f = eq_Access(eqptr, i);
	if (f->has_hot_spot) {
	    register int
		dx = mx - f->hot.x,
		dy = my - f->hot.y,
		distance = ABS(dx) + ABS(dy) /*dx*dx + dy*dy*/;
	    if (distance < nearest_distance) {
		nearest_distance = distance;
		nearest_i = i;
	    }
	}
	if (restrict && !f->transparent)
	    if (f->symbol->type == BEGIN)
		i = eq_FindEndGroup(eqptr, i+1);
	    else if (f->symbol->type == END)
		break;
    }
    return nearest_i;
}


/*
 * Manage the caret
 */

static int min_x, min_y, max_x, max_y;

static eqview_Box(eqptr, pos, start, stop, x, y)
struct eq *eqptr;
long pos, start, stop, x, y;
{
    register int i;
    for (i=start; i<stop; i++) {
	register struct formula *f = eq_Access(eqptr, i);
	switch (f->symbol->type) {
	    case BEGIN:
		i = eqview_Box(eqptr, pos, i+1, stop, x+f->pos.x, y+f->pos.y);
		break;
	    case SIMPLE:
	    case EXTEND:
		if (i >= pos) {
		    register int xx = x+f->pos.x,  yy = y+f->pos.y;
		    if (xx+f->min.x < min_x) min_x = xx+f->min.x;
		    if (xx+f->max.x > max_x) max_x = xx+f->max.x;
		    if (yy+f->min.y < min_y) min_y = yy+f->min.y;
		    if (yy+f->max.y > max_y) max_y = yy+f->max.y;
		}
		break;
	    case END:
		return i;
	}
    }
    return stop;
}


void eqview__CalculateCaret(self)
struct eqview *self;
{
    int pos, len;
    struct eq *eqptr = Eq(self);
    struct formula *f;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);

    if (!self->hasinputfocus) {
	self->selection_width = self->selection_height = -1;
    } else if (len==0) {
	f = eq_Access(eqptr, pos);
	self->caret_x = f->hot.x;
	self->caret_y = f->hot.y;
	self->selection_width = 0;
	self->selection_height = 0;
    } else {
	min_x=10000,  min_y=10000,  max_x=0,  max_y=0;
	eqview_Box(eqptr, pos, 0, pos+len, self->off_x, self->off_y);
	self->caret_x = min_x-1;
	self->caret_y = min_y-1;
	self->selection_width = max_x-min_x+3;
	self->selection_height = max_y-min_y+3;
    }	
}


void eqview__DrawCaret(self)
struct eqview *self;
{
    struct rectangle rect;

    if (self->selection_width < 0 || self->selection_height < 0)
	return;
    else if (self->selection_width==0 || self->selection_height==0) {
	static struct fontdesc *icon12;
	if (!icon12)
	    icon12 = fontdesc_Create("icon", fontdesc_Plain, 12);
	eqview_SetTransferMode(self, graphic_INVERT);
	eqview_SetFont(self, icon12);
	eqview_MoveTo(self, self->caret_x, self->caret_y);
	eqview_DrawString(self,"|",view_NOMOVEMENT);
    } else {
	eqview_SetTransferMode(self, graphic_INVERT);
	rectangle_SetRectSize(&rect, self->caret_x, self->caret_y, self->selection_width, self->selection_height);
	pat = eqview_WhitePattern(self);
	eqview_FillRect(self, &rect, pat);
    }
}
