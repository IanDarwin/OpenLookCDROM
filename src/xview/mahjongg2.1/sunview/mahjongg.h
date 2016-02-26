/*
 *	Copyright 1988, Mark Holm
 *			Exceptions
 *
 *	Acknowledgments to Dorothy Robinson for her artistic
 *	 abilities in drawing the icons and to Jim Batch for
 *	 technical support and graphical concepts (which I abandoned in favor
 *       of the easy way out).
 *
 *	Permission is given to copy and distribute for non-profit purposes.
 *
 */

#ifndef lint
static char *rcsh = "$header$ Copyright 1988 Mark A. Holm";
#endif !lint

/*
 * $log$
 */

#define RANDOM(x)	((random() >> 8) % x)

#define boolean int
#define TRUE 1
#define FALSE 0
#define MAX_COLORS		7	/* maximum colors in color map excludingblack */

/* extra window definitions */
#define BORDER		5	/* border width between panels */

/* define tile playing area offsets from frame edges */
#define X_LOC 30
#define Y_LOC 40

#define W_BASE_TILE     58
#define H_BASE_TILE     59
#define S_TILE_SHADOW	6
#define B_TILE_SHADOW	5
#define WIDE_MAX_TILES  15
#define HIGH_MAX_TILES  8
#define PLAY_X_MAX	((W_BASE_TILE * WIDE_MAX_TILES) + S_TILE_SHADOW + (2 * X_LOC))
#define PLAY_Y_MAX	((H_BASE_TILE * HIGH_MAX_TILES)  + B_TILE_SHADOW + (2 * Y_LOC))
#define MESS_X_MAX	(PLAY_X_MAX)
#define MESS_Y_MAX	(H_BASE_TILE * 3)
#define FRAME_X_MAX	(PLAY_X_MAX )
#define FRAME_Y_MAX	(PLAY_Y_MAX + MESS_Y_MAX + BORDER)

/* define rows and cols */

#define ROW1	(Y_LOC)
#define ROW2	(Y_LOC + H_BASE_TILE)
#define ROW3	(Y_LOC + (H_BASE_TILE * 2))
#define ROW4	(Y_LOC + (H_BASE_TILE * 3))
#define ROW4pt5	(Y_LOC + (H_BASE_TILE * 3) + (H_BASE_TILE / 2))
#define ROW5	(Y_LOC + (H_BASE_TILE * 4))
#define ROW6	(Y_LOC + (H_BASE_TILE * 5))
#define ROW7	(Y_LOC + (H_BASE_TILE * 6))
#define ROW8	(Y_LOC + (H_BASE_TILE * 7))

#define COL1	(X_LOC)
#define COL2	(X_LOC + W_BASE_TILE)
#define COL3	(X_LOC + (W_BASE_TILE * 2))
#define COL4	(X_LOC + (W_BASE_TILE * 3))
#define COL5	(X_LOC + (W_BASE_TILE * 4))
#define COL6	(X_LOC + (W_BASE_TILE * 5))
#define COL7	(X_LOC + (W_BASE_TILE * 6))
#define COL7pt5	(X_LOC + (W_BASE_TILE * 6) + (W_BASE_TILE / 2))
#define COL8	(X_LOC + (W_BASE_TILE * 7))
#define COL9	(X_LOC + (W_BASE_TILE * 8))
#define COL10	(X_LOC + (W_BASE_TILE * 9))
#define COL11	(X_LOC + (W_BASE_TILE * 10))
#define COL12	(X_LOC + (W_BASE_TILE * 11))
#define COL13	(X_LOC + (W_BASE_TILE * 12))
#define COL14	(X_LOC + (W_BASE_TILE * 13))
#define COL15	(X_LOC + (W_BASE_TILE * 14))

/* Tile structure */

typedef struct tile {
    Pixrect 	 *image;
    int		 value;
    boolean	 left_free;
    boolean	 right_free;
    boolean	 top_free;
    int		 left_next[2];
    int		 right_next[2];
    int		 covered[4];
    boolean	 removed;
} Tiles;

typedef struct sel { 
        boolean		filled;
        Panel_item      item;
        Event           event; 
} Selected;

/* global externals */

/* color closed icon image */

extern Pixrect		cicon_image;

/* black and white number tiles */

extern Pixrect		NUM0;
extern Pixrect		NUM1;
extern Pixrect		NUM2;
extern Pixrect		NUM3;
extern Pixrect		NUM4;
extern Pixrect		NUM5;
extern Pixrect		NUM6;
extern Pixrect		NUM7;
extern Pixrect		NUM8;
extern Pixrect		NUM9;

/* color number tiles */

extern Pixrect		cNUM0;
extern Pixrect		cNUM1;
extern Pixrect		cNUM2;
extern Pixrect		cNUM3;
extern Pixrect		cNUM4;
extern Pixrect		cNUM5;
extern Pixrect		cNUM6;
extern Pixrect		cNUM7;
extern Pixrect		cNUM8;
extern Pixrect		cNUM9;

/* black and white playing tiles */

extern Pixrect		BLANK;
extern Pixrect		DOT1;
extern Pixrect		DOT2;
extern Pixrect		DOT3;
extern Pixrect		DOT4;
extern Pixrect		DOT5;
extern Pixrect		DOT6;
extern Pixrect		DOT7;
extern Pixrect		DOT8;
extern Pixrect		DOT9;
extern Pixrect		BAM1;
extern Pixrect		BAM2;
extern Pixrect		BAM3;
extern Pixrect		BAM4;
extern Pixrect		BAM5;
extern Pixrect		BAM6;
extern Pixrect		BAM7;
extern Pixrect		BAM8;
extern Pixrect		BAM9;
extern Pixrect		CHA1;
extern Pixrect		CHA2;
extern Pixrect		CHA3;
extern Pixrect		CHA4;
extern Pixrect		CHA5;
extern Pixrect		CHA6;
extern Pixrect		CHA7;
extern Pixrect		CHA8;
extern Pixrect		CHA9;
extern Pixrect		GRED;
extern Pixrect		REDD;
extern Pixrect		WHTD;
extern Pixrect		EAST;
extern Pixrect		WEST;
extern Pixrect		SOUT;
extern Pixrect		NORT;
extern Pixrect		AUT;
extern Pixrect		SUM;
extern Pixrect		SPR;
extern Pixrect		WIN;
extern Pixrect		ORC;
extern Pixrect		MUM;
extern Pixrect		BAM;
extern Pixrect		PLM;

/* color playing tiles */

extern Pixrect		cBLANK;
extern Pixrect		cDOT1;
extern Pixrect		cDOT2;
extern Pixrect		cDOT3;
extern Pixrect		cDOT4;
extern Pixrect		cDOT5;
extern Pixrect		cDOT6;
extern Pixrect		cDOT7;
extern Pixrect		cDOT8;
extern Pixrect		cDOT9;
extern Pixrect		cBAM1;
extern Pixrect		cBAM2;
extern Pixrect		cBAM3;
extern Pixrect		cBAM4;
extern Pixrect		cBAM5;
extern Pixrect		cBAM6;
extern Pixrect		cBAM7;
extern Pixrect		cBAM8;
extern Pixrect		cBAM9;
extern Pixrect		cCHA1;
extern Pixrect		cCHA2;
extern Pixrect		cCHA3;
extern Pixrect		cCHA4;
extern Pixrect		cCHA5;
extern Pixrect		cCHA6;
extern Pixrect		cCHA7;
extern Pixrect		cCHA8;
extern Pixrect		cCHA9;
extern Pixrect		cGRED;
extern Pixrect		cREDD;
extern Pixrect		cWHTD;
extern Pixrect		cEAST;
extern Pixrect		cWEST;
extern Pixrect		cSOUT;
extern Pixrect		cNORT;
extern Pixrect		cAUT;
extern Pixrect		cSUM;
extern Pixrect		cSPR;
extern Pixrect		cWIN;
extern Pixrect		cORC;
extern Pixrect		cMUM;
extern Pixrect		cBAM;
extern Pixrect		cPLM;

/* cursor images */

extern Pixrect		stick;
extern Pixrect		waiting;
extern Pixrect		confirm;
