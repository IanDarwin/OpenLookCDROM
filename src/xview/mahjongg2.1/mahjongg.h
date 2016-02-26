/*
 * XView port of mahjongg by Stan K. Tazuma, 8/91-9/91
 * Copyright 1991.
 *
 * $Header: /home/sirius/skt/cmds/src/sun/mj/RCS/mahjongg.h,v 2.4 91/12/22 17:12:52 skt Exp $
 *
 */

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

#define RANDOM(x)	((random() >> 8) % x)

typedef unsigned char boolean;
#define TRUE		1
#define FALSE		0

/* extra window definitions */
#define BORDER		2	/* border width between panel and canvas */
			/* weird behavior when BORDER set to 1 : the
			 * width of the canvas window gets reduced (usually
			 * but not always--each invocation different)
			 * by about 1/3rd.  Don't know why. */

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

typedef unsigned char Tile_val;
/* DONT_CARE is a value assigned to a Tile_val variable that is a non-valid
 * tile identifier */
#define DONT_CARE	255

typedef struct tile {
    char	image;		/* numeric index of image into the array of
				 * images */

    short	x_loc;		/* X coord of upper left corner */
    short	y_loc;		/* Y coord of upper left corner */
    Tile_val	value;		/* numeric id of tile */
    boolean	left_free;	/* is left side of tile free? */
    boolean	right_free;	/* is right side of tile free? */
    boolean	top_free;	/* is top side of tile free? */
    Tile_val	left_next[2];	/* if !left_free, there are 1 or 2 tiles
				 * to the left of the tile, with their id's
				 * listed here */
    Tile_val	right_next[2];	/* if !right_free, there are 1 or 2 tiles
				 * to the right of the tile, with their id's
				 * listed here */
    Tile_val	covered[4];	/* if !top_free, there are 1 or 4 tiles
				 * below the tile, id's are listed here */
    boolean	removed;	/* is tile removed from play area? */
    boolean	image_fixed;	/* was the tile image fixed up to clean up the
				 * junk? */
    boolean	top_covered;	/* in a visual sense, is the tile completely
				 * (not just partially) covered by another
				 * tile? (some tiles have tiles above them
				 * that completely obscure and hide the tile;
				 * other tiles are only partially obscured) */
} Tile;

typedef struct sel { 
    boolean	in_preview_mode;	/* is selected tile
					 * visually inverted? */
    Tile	*tileptr;		/* ptr to a Tile, so we can use its
					 * Pixrect and x_loc, y_loc location */
    Server_image si_before_preview;	/* before doing a preview (inverting)
					 * save the pixrect here so that if
					 * a cancel preview is done, the
					 * cancel can be quick */
} Selected;

/*
 * The next few macros are used to extract from Pixrect's the pointers
 * to actual image data, so that I can use or modify the data.
 * mpr_d() comes from <pixrect/memvar.h>.  Each of the macros takes
 * a pointer to a memory pixrect as the parameter.
 */
#define PIXRECT_IMAGE_DATA_PTR(mpr)	(mpr_d(mpr)->md_image)
#define PIXRECT_IMAGE_DEPTH(mpr)	((mpr)->pr_depth)
/* PIXRECT_IMAGE_SIZE() gets the image size in numbers of bytes */
#define PIXRECT_IMAGE_SIZE(mpr)		\
	((((mpr)->pr_width * (mpr)->pr_height) * PIXRECT_IMAGE_DEPTH(mpr))/8)


/* global externals */

/* color closed icon image */

extern Pixrect		cicon_image;

/* black and white closed icon image */
extern Pixrect		icon_image_pr;

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

/*
 * Indexes into Tile_icon_mpr[] and Tile_icon_si[].  These defines map
 * directly to the case labels used in the original switch statement
 * in mahjongg.c
 */
#define DOT1_I	0
#define DOT2_I	1
#define DOT3_I	2
#define DOT4_I	3
#define DOT5_I	4
#define DOT6_I	5
#define DOT7_I	6
#define DOT8_I	7
#define DOT9_I	8
#define BAM1_I	9
#define BAM2_I	10
#define BAM3_I	11
#define BAM4_I	12
#define BAM5_I	13
#define BAM6_I	14
#define BAM7_I	15
#define BAM8_I	16
#define BAM9_I	17
#define CHA1_I	18
#define CHA2_I	19
#define CHA3_I	20
#define CHA4_I	21
#define CHA5_I	22
#define CHA6_I	23
#define CHA7_I	24
#define CHA8_I	25
#define CHA9_I	26
#define GRED_I	27
#define REDD_I	28
#define WHTD_I	29
#define EAST_I	30
#define WEST_I	31
#define SOUT_I	32
#define NORT_I	33
#define AUT_I	34
#define SUM_I	35
#define SPR_I	36
#define WIN_I	37
#define ORC_I	38
#define MUM_I	39
#define BAM_I	40
#define PLM_I	41

#define BLANK_I	42
