/*
 * $RCSfile: ParserDefs.h,v $
 *
 * (c) Copyright 1992-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

#ifndef _PARSER_DEFS_H
#define _PARSER_DEFS_H

/***************************************************************
**
**			CONSTANT DEFINITIONS 			
**
***************************************************************/


/* Max length of the linked list of objects (loop detector) */
#define	MAX_OBJECTS	1024 * 8	 /* 8K is 2 * (ColorTut + 1k) */ 

/* Define types of graphic objects */
#define PATH_TYPE_FILL	 	0
#define PATH_TYPE_STROKE	1
#define PATH_TYPE_CLIP		2
#define PATH_TYPE_INITCLIP	3

/* Define the color spaces */
#define COLOR_MODEL_GRAY		0
#define COLOR_MODEL_RGB			1

/* Define the rendering methods */
#define METHOD_LEVEL1	0
#define METHOD_UPATHS	1
#define METHOD_UCACHE	2

/* Define types of input files */
#define PS_NOT_KNOWN	0
#define PS_DISTILLERY	1

/* Define userpath buffer constants */
#define PTS_UPATH_BUFFER	2000
#define OPS_UPATH_BUFFER	1001

/***************************************************************
**
**			TYPEDEFS AND DATA STRUCTURE DEFINITIONS
**
***************************************************************/

typedef enum {
    UNDEFINED,
    INTEGER,
    REAL
} Element;

typedef struct {
    Element	type;
    union {
	int	integer;
	float	real;
    } element;
} Any;

typedef struct {
    float x, y;
} Point;

typedef struct {
        Point       ll;
        Point       ur;
} BBox;

typedef struct {
    unsigned char	path_type;
    unsigned char	color_type;
    float		gray;
    float		red;
    float		green;
    float		blue;
    float		linewidth;
    float		miterlimit;
    unsigned char	linejoin;
    unsigned char	linecap;
} GraphicParams;

typedef struct {
    float	*pts;
    char	*ops;
    float	bbox[4];
    int		num_pts;
    int		num_ops;
} UserPath;

typedef struct _Graphic {
    struct _Graphic	*next;
    GraphicParams	parms;
    UserPath		path;
} Graphic;

typedef struct {
    Graphic	*qHead;
    Graphic	*qTail;
    int		objNum;
    BBox	bounds;
} Page;

#endif /* _PARSER_DEFS_H  -- Add nothing below this line! */
