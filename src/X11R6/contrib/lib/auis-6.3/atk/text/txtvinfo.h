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


 

#include <txtstvec.h>

/* Redisplay structure giving font and position info in a large buffer. */

enum li_Type {
    li_Plain,
    li_Expose,
    li_Octal,
    li_View
};

struct viewitem {
    struct view *view;          /* Pointer to view object */
    short width, height;        /* Space allocated in text */
};

struct textitem {
    short rSpaces;              /* # rubber spaces in item */
    short lineBufPos;		/* Start pos in line buffer */
    struct fontdesc *font;	/* Font of any text in item */
    short *fontWidths;		/* Cached ptr to font widths tbl */
    long styleFlags;		/* style_Underline, etc., ORed */
    short hasTab;               /* flag */
    char *color;		/* color for text */
};

struct lineitem  {
    long docPos;		/* Start pos in document */
    short scripting;		/* Displacement added to y */
    short xPos;			/* X pos of the line start */
    long lineBelow;
    long lineAbove;
    long textBelow;
    enum li_Type type;          /* Specifies union member */
    union {
        struct viewitem viewitem;
        struct textitem textitem;
    } itemdata;
};

#define vi_view itemdata.viewitem.view
#define vi_width itemdata.viewitem.width
#define vi_height itemdata.viewitem.height

#define ti_rSpaces itemdata.textitem.rSpaces
#define ti_lineBufPos itemdata.textitem.lineBufPos
#define ti_font itemdata.textitem.font
#define ti_fontWidths itemdata.textitem.fontWidths
#define ti_styleFlags itemdata.textitem.styleFlags
#define ti_hasTab itemdata.textitem.hasTab
#define ti_color itemdata.textitem.color

#define	NTITEMS	60  /* Max env changes per line */
#define	LBSIZE 512  /* Max total chars in all line items */

/*
 * Variables representing the window manager state.  They are used
 * as global implicit parameters within the real-time justification code
 * which must run extremely fast.
 */

struct formattinginfo  {
    long lineLength;		/* Length (in chars) of screen line */
    long markLength;		/* # things considered in choosing line */
    long totalWidth;		/* Total width of text in screen line */
    long rubberSpaces;		/* Total # expandable spaces on line */
    struct text_statevector sv;	/* Current state vector */
    short *myWidths;		/* Width table for current font */
    boolean pBreak;		/* True if char is first in a paragraph */
    boolean endOfPara;		/* True if line is last line in a para. */
    int	below;			/* Below of line including spacing/spread */
    int lineBelow;		/* Below of tallest font/view on line */
    int textBelow;		/* Below of tallest font on line */
    int lineAbove;		/* Above of tallest font on line */
    int spaceBump;		/* Extra space added per rubber space */
    int	lineBP;			/* Pointer into line buffer array */
    unsigned char lineBuffer[LBSIZE];	/* Line buffer array */
    int	lineIP;			/* Pointer into lineitem array */
    struct lineitem *clp;	/* Pointer to lineitems[lineip-1] */
    struct lineitem lineItems[NTITEMS];	/* Line item array */
    int	xDim;			/* X dimension to draw in */
    int locateX;		/* Locatehit stores best x pos here */
    int cursorY;
    int	bLM;			/* Left margin at beg. of line */
    enum style_Justification just;	/* Just. mode at beg. of line */
    int bIndent;		/* Indentation at beg. of line */
    int	sawTab;			/* True if line contains a tab (breaks justifier) */
    int	inContinueMode;
    struct view *foundView;
    long predrawnEnd;
    struct lineitem *itemAtDot;
    long lastFontPos;
    boolean continued;		/* True if line extends past the end of page */
};

/* defines used by charType in txtvcmds.c */
#define	WHITESPACE	0
#define	WORD		1
#define	SPECIAL		2

