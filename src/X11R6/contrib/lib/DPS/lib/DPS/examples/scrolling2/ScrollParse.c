/*
 * $RCSfile: ScrollParse.c,v $
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

#include "Scroll.h"
#include <DPS/dpsexcept.h>
#include <X11/Xos.h>
#include <stdlib.h>

/***************************************************************
**
** 		LOCAL DATA DECLARATIONS
**
***************************************************************/

#define STACKSIZE	256	/* Maximum stack size. */
#define ERR_MSG_LEN	80	/* Size of the error msg buffer */

static UserPath	CurrentPath;
static Page	CurrentPage;

static char		*dp;			/* input data pointer */
static int		stackdepth;		/* number of elements */
static Any		*stacktop;		/* ptr to top of stack */
static Any		*stack;			/* ptr to bottom of stack */
static Point		currentpoint; 
static Point		startpoint;
static Boolean		nocurrentpoint; 
static BBox		pathbbox;
static GraphicParams	gparms;

static char		*errmsg;
static Boolean		error;

/* Parsing routines */

static void i_int(), i_real(), i_string(), i_literal(), i_name(), i_array(),
	i_f(), i_s(), i_clip(), i_T(), i_A(), i_W(), i_AW(), i_m(), i_lineto(),
	i_L(), i_r(), i_R(), i_l(), i_x(), i_y(), i_X(), i_Y(), i_c(), i_cp(),
	i_w(), i_g(), i_j(), i_d(), i_miter(), i_cap(), i_RGB(), i_F(),
	i_FF(), i_DF(), i_MF(), i_IMASK(), i_IMAGE(), i_BPAGE(), i_EPAGE(),
	i_REMAP(), i_RECODE(), i_initclip();

#define	NUM_PROCS	43	/* Size of the procs [] function array */

typedef void (*VoidProc) ();

static VoidProc procs[128] = {
/* 00 */ 	i_int, i_real,  i_string, i_literal, i_name, i_array, i_f, i_s,
/* 08 */	i_clip, i_T, i_A, i_W, i_AW, i_m, i_lineto, i_L,
/* 16 */	i_r, i_R, i_l, i_x, i_y, i_X, i_Y, i_c,
/* 24 */	i_cp, i_w, i_g, i_j, i_d, i_miter, i_cap, i_RGB,
/* 32 */	i_F, i_FF, i_DF, i_MF, i_IMASK, i_IMAGE, i_BPAGE, i_EPAGE,
/* 40 */	i_REMAP, i_RECODE, i_initclip
};

extern char yytext[];
extern int yyleng;

/* Concatenate the string to the generic parser error message and display */

static void showerror(string)
    String string;
{
    XmString err, msg;

    msg = XmStringCreateSimple(string);
    err = XmStringConcat(AppData.parserErrorMessage, msg);
    XmStringFree(msg);
    putUpInfoDialog(err);
    XmStringFree(err);
} /* end showerror() */

/* Set error flag and show error message */

static void errorlog(pMsg, pFlag)
    char	*pMsg; 
    Boolean	*pFlag;
{
    *pFlag = TRUE;	
    errmsg = pMsg;
    showerror(errmsg);
} /* end errorlog() */

/* Construct error message and show it */

static void errordisplay(token)
    int	token;
{
    static char	errbuff[ERR_MSG_LEN];

    sprintf(errbuff, "Token=%d, ", token);
    strcat(errbuff, errmsg);
    showerror(errbuff);
} /* end errordisplay() */

/* Malloc routine that puts up error message if malloc fails */

char *mymalloc(size)
    unsigned int size;
{
    char *ch = (char *) malloc(size);
    if (ch == NULL) putUpInfoDialog(AppData.noMemoryMessage);
    return ch;
} /* end mymalloc() */

/* Reads the entire contents of the file into memory */

unsigned long readFileIntoBuffer(f, buff)
    FILE *f;
    char **buff;
{
    char		*cp; 
    unsigned long	fsize, count; 
    struct stat		fdata;

    /* Init the buff pointer to NULL */
    *buff = NULL;

    /* Get the file length and allocate a (maybe huge) buffer */
    fstat (fileno(f), &fdata);
    fsize = (unsigned long) fdata.st_size;

    /* Don't use XtMalloc since we want to let this fail */
    cp = (char *) mymalloc(fsize + 2);

    if (cp == NULL) count = 0;
    else {
	count = fread(cp, 1, fsize, f);
	if (count != fsize) {
	    putUpInfoDialog(AppData.badReadMessage);
	    free(cp);
	    return 0;
	}
	
	/* Null-terminate the buffer and save control information */
	*(cp + count) = '\n';
	*(cp + count + 1) = '\0';
	*buff = cp;
    }
    return count;
} /* end of readFileIntoBuffer() */

/* Returns the first character of the next line after cp */

static char *skipToLineEnd(cp)
    char *cp;
{
    cp = index(cp,'\n'); cp++;
    return cp;
} /* end of skipToLineEnd () */

/* Returns the first nonspace character after a colon */

static char *skipToField (cp)
    char *cp;
{
    cp = index(cp,':'); cp++;
    if (*cp == ' ') {
	rindex(cp, ' ');
	cp++;
    }
    return cp;
} /* end of skipToField () */

/* Checks for the valid PostScript header */

static Boolean checkForPS(data)
    char **data; 
{
    Boolean	valid = FALSE;

    if (!strncmp(*data, "%!PS-Adobe-", 11)) valid = TRUE;

    *data = skipToLineEnd(*data);
    return valid;
} /* end of checkForPS () */

/* Checks if the file was created by the distillery */

static int checkFileType(s0, s1, s2, s3)
    char *s0, *s1, *s2, *s3;
{
    int	type = PS_NOT_KNOWN;
	
    if (s0 && *s0 && s1 && *s1 && s2 && *s2 && s3 && *s3) {
	if (!strncmp(s3, "still.ps", 8)) type = PS_DISTILLERY;
    }
    return type;
} /* end of checkFileType () */

/* Checks header comments for creator */

static void checkHeader(dataptr, creator)
    char	**dataptr; 
    int  	*creator;
{
    Boolean	done = FALSE;
    char	*cp;
    char	str0[40], str1[40], str2[40], str3[40];

    *creator = PS_NOT_KNOWN;

    cp = *dataptr;
    while (*cp && !done) {
	/* Skip any lines that aren't comments */
	if (strncmp(cp, "%%", 2) == 0) {
	    cp += 2;
			
	    /* Look for end-of-comments comments */
	    if (strncmp(cp, "EndComments", 11) == 0) done = TRUE;

	    /* Look for the Creator field */
	    else if (strncmp(cp, "Creator:", 8) == 0) {
		cp = skipToField (cp);
		sscanf(cp, "%s%s%s%s", str0, str1, str2, str3);
		*creator = checkFileType(str0, str1, str2, str3);
	    }
	}	
	cp = skipToLineEnd(cp);
    }

    *dataptr = cp;
    return;
} /* end of checkHeader () */

/* Return next character in the buffer */

char igetc ()
{
    return *dp++;
}

/* Initialize the graphics parameters */

void initGraphicParms(ptr)
    GraphicParams *ptr;
{
    if (ptr) {
	ptr->path_type = PATH_TYPE_STROKE;
	ptr->color_type = COLOR_MODEL_GRAY;
	ptr->gray =  0.0;
	ptr->red = ptr->green = ptr->blue = 0.0;
	ptr->linewidth = 1;
	ptr->miterlimit = 10;
	ptr->linejoin = ptr->linecap = 0;
    }
} /* end of initGraphicParms () */

/* Reset stored path structure */

static void resetPathStruct()
{
    CurrentPath.num_pts = 0;
    CurrentPath.num_ops = 0;
} /* end resetPathStruct() */

/* One-time parser initialization */

static void parseInitialize()
{
    unsigned int size = STACKSIZE * sizeof(Any);

    if (CurrentPath.pts == NULL) {
	CurrentPath.pts = (float *) XtCalloc(PTS_UPATH_BUFFER, sizeof(float));
	CurrentPath.ops = (char *) XtCalloc(OPS_UPATH_BUFFER, sizeof(char));
    }
    resetPathStruct();
    CurrentPage.qHead = CurrentPage.qTail = NULL;
    CurrentPage.objNum = 0;
    CurrentPage.bounds.ll.x = CurrentPage.bounds.ll.y = 9999;
    CurrentPage.bounds.ur.x = CurrentPage.bounds.ur.y = -9999;
    initGraphicParms(&gparms);
	
    if (stack == NULL) stack = (Any *) XtMalloc((Cardinal) size);
    nocurrentpoint = TRUE;
    stackdepth = 0;
    stacktop = stack;
    error = FALSE;
}

/* Parse the buffer */

Page *doParse(buffer)
    char **buffer;
{
    Boolean 	parse_err = FALSE; 
    Boolean	skip = TRUE; 
    Boolean	done = FALSE;
    int		token;
	
    dp = *buffer;
    parseInitialize();

    /* Initially we're skipping lines.  %%EndPageSetup signals to stop doing
       that and start parsing them.  %%Trailer says we're done */

    while (*dp != 0  && !done)	{
	if (*dp == '\n') dp++;
	else if (!strncmp(dp, "%%", 2)) {
	    dp += 2;
	    if (!strncmp(dp, "Trailer", 7)) done = TRUE;
	    else if (!strncmp(dp, "EndPageSetup", 12)) skip = FALSE;
	    else if (!strncmp(dp, "PageTrailer", 11)) skip = TRUE;

	    dp = index(dp,'\n'); dp++;

	} else if (skip) {
	    dp = index(dp,'\n'); dp++;
	} else {
	    /* Get and parse a token */
	    token = yylex();
	    while (token && !parse_err) {
		if (token <= NUM_PROCS) {
		    (*procs[token-1])();
		    parse_err = error;
		} else errorlog("unrecognized token", &parse_err);

		if (!parse_err) token = yylex ();
		else errordisplay(token);
	    }
	}
    }
	
    *buffer = dp;
    return &CurrentPage;
} /* end doParse() */

/* Parse the data buffer */

static Page *parseDataBuffer(buffer)
    char *buffer;
{
    int		creator;
    Page	*p;

    /* Check the buffer for PS-identifying DSC or EPSF comments */
    if (!checkForPS (&buffer)) {
	putUpInfoDialog(AppData.badFileMessage);
	return NULL;
    }
	
    /* Check the prologue for Creator comments */
    checkHeader(&buffer, &creator);

    /* If Creator isn't a magic name, the input isn't distilled */
    if (creator == PS_NOT_KNOWN) {
	putUpInfoDialog(AppData.badFileMessage);
	return NULL;
    }

    /*
    ** Having finished with prologue, parse the actual PS program; 
    ** this will also fill the bounding box with the actual 
    ** image dimensions, derived from the bounds of each user path.
    */
    p = doParse(&buffer);

    if (p == NULL) return NULL;

    return p;
} /* end of parseDataBuffer () */

/* Free all objects in a page */

static void freeObjects(p)
    Page *p;
{
    Graphic *g, *nextg;

    for (g = p->qHead; g != NULL; g = nextg) {
	if (g->parms.path_type != PATH_TYPE_INITCLIP) {
	    XtFree((XtPointer) g->path.pts);
	    XtFree((XtPointer) g->path.ops);
	}
	nextg = g->next;
	XtFree((XtPointer) g);
    }
} /* end freeObjects() */
 
/* Parse a file by reading it into a buffer and calling parse */

Boolean parseFile(f)
    FILE *f;
{
    unsigned long bufferSize;
    char *buffer;
    Page *p;

    bufferSize = readFileIntoBuffer(f, &buffer);

    if (bufferSize != 0) {
	p = parseDataBuffer(buffer);
	free(buffer);

	if (p != NULL) {
	    freeObjects(&AppData.picture);
	    AppData.picture = *p;
	    return True;
	}
    }

    return False;
} /* end of parseFile() */

/***************************************************************
**
** 		TOKEN PARSING SUBROUTINES
**
***************************************************************/

/* Pop and discard stack */

static void pop()
{
    if (!error) {
	if (stackdepth > 0) {
	    stackdepth--;
	    stacktop--;
	} else errorlog("stack underflow", &error);
    }
}

/* Clear stack */

static void clear()
{
    while (stackdepth > 0) pop();
}

/* Pop and return integer */

static void popint(aval)
    int *aval;
{
    if (!error) {
	if (stackdepth > 0) {
	    if (stacktop->type == INTEGER) {
		*aval = stacktop->element.integer;
		stackdepth--;
		stacktop--;
	    } else errorlog("typecheck", &error);
	} else errorlog("stack underflow", &error);
    }
}

/* Pop and return real */

static void popreal(aval)
    float *aval;
{
    if (!error) {
	if (stackdepth > 0) {
	    if (stacktop->type == REAL || stacktop->type == INTEGER) {
		if (stacktop->type == REAL) {
		    *aval = stacktop->element.real;
		} else *aval = (float) stacktop->element.integer;
		stackdepth--;
		stacktop--;
	    } else errorlog("typecheck", &error);
	} else errorlog("stack underflow", &error);
    }
}

/* Push an integer on the stack */

static void pushint(aval)
    int	aval;
{
    if (!error)	{
	if (stackdepth < STACKSIZE) {
	    stackdepth++;
	    stacktop++;
	    stacktop->type = INTEGER;
	    stacktop->element.integer = aval;
	} else errorlog("stack overflow", &error);
    }
}

/* Push a real on the stack */

static void pushreal(aval)
    float aval;
{
    if (!error) {
	if (stackdepth < STACKSIZE) {
	    stackdepth++;
	    stacktop++;
	    stacktop->type = REAL;
	    stacktop->element.real = aval;
	} else errorlog("stack overflow", &error);
    }
}

/* Roll stack, like PS roll operator */

static void roll(n, j)
    int	n;
    int	j;
{
    int	a, b;
    Any	temp;

    if (!error) {
	if (stackdepth - n >= 0) {
	    for (a = j; a > 0; a--) {
		temp = *stacktop;
		for (b = 1; b < n; b++) {
		    stack [stackdepth - (b-1)] = stack [stackdepth - b];
		}
		stack[stackdepth - (n - 1)] = temp;
	    }
	} else errorlog("stack underflow", &error);
    }
}

/* Check points against path bounding box and expand if necessary */

static void checkBounds(pts, num_pts, relative)
    Point	*pts; 
    int 	num_pts; 
    Boolean	relative;
{
    int		i;
    Point	pt;

    if (nocurrentpoint && num_pts > 0) {
	pathbbox.ll.x = pathbbox.ur.x = pts->x;
	pathbbox.ll.y = pathbbox.ur.y = pts->y;
    }

    for (i = 0; i < num_pts; i++) {
	pt = pts[i];	
	if (relative) {
	    pt.x += currentpoint.x;
	    pt.y += currentpoint.y;
	}
		
	if (pt.x < pathbbox.ll.x ) pathbbox.ll.x = pt.x;
	else if (pt.x > pathbbox.ur.x) pathbbox.ur.x = pt.x;
	
	if (pt.y < pathbbox.ll.y) pathbbox.ll.y = pt.y;
	else if (pt.y > pathbbox.ur.y) pathbbox.ur.y = pt.y;
    }
}

/* Add an object to the page */

void addObjToPage(pPage, pObj)
    Page	*pPage;
    Graphic	*pObj;
{
    Graphic	*pTmp;

    /* Handle special case of null queue pointers */ 
    if (pPage->qTail == NULL) {
	/* first obj */
	pPage->qHead = pPage->qTail = pObj;
	pObj->next =  NULL;
	pPage->objNum = 1;

    } else { /* just add it to the end of the list */
	pTmp = pPage->qTail;
	pObj->next = NULL;
	pTmp->next = pPage->qTail = pObj;
	pPage->objNum += 1;
    }
}

/* Inset bounding box by deltas */

void insetBox(pBox, deltaX, deltaY)
    BBox	*pBox;
    float	deltaX;
    float	deltaY;
{
    pBox->ll.x += deltaX;
    pBox->ll.y += deltaY;
    pBox->ur.x -= deltaX;
    pBox->ur.y -= deltaY;
}

/* Copy user path to the object */

void installUPathAndBounds(pObj, pPath, pBBox)
    Graphic 	*pObj; 
    UserPath	*pPath; 
    BBox 	*pBBox;
{
    int	i;

    /* Allocate space for the operator and operand arrays */
    pObj->path.pts = (float *) XtMalloc(pPath->num_pts * sizeof(float));
    pObj->path.ops = (char *) XtMalloc(pPath->num_ops + 1);

    /* Insert the bounding box and copy in the other operands */
    pObj->path.bbox[0] = pBBox->ll.x;
    pObj->path.bbox[1] = pBBox->ll.y;
    pObj->path.bbox[2] = pBBox->ur.x;
    pObj->path.bbox[3] = pBBox->ur.y; 

    for (i = 0; i < pPath->num_pts; i++) pObj->path.pts[i] = pPath->pts[i];

    pObj->path.num_pts = pPath->num_pts;

    /* Insert dps_ucache cmd and copy in the other operators */
    pObj->path.ops[0] = dps_ucache;
    memmove(pObj->path.ops+1, pPath->ops, pPath->num_ops);
    pObj->path.num_ops = pPath->num_ops + 1;
} /* end of installUPathAndBounds () */

/* Update the page's bounding box */

void updatePageBBox(pPage, pBBox)
    Page	*pPage;
    BBox	*pBBox;
{
    /* Test for extension leftwards */
    if (pBBox->ll.x < pPage->bounds.ll.x) pPage->bounds.ll.x = pBBox->ll.x;

    /* Test for extension downwards */
    if (pBBox->ll.y < pPage->bounds.ll.y) pPage->bounds.ll.y = pBBox->ll.y;

    /* Test for extension rightwards */
    if (pBBox->ur.x > pPage->bounds.ur.x) pPage->bounds.ur.x = pBBox->ur.x;

    /* Test for extension upwards */
    if (pBBox->ur.y > pPage->bounds.ur.y) pPage->bounds.ur.y = pBBox->ur.y;
}

/* Add a path to the page */

static void insertPath(path_type)
    int	path_type;
{
    Graphic	*pObj;
    BBox	bbox;

    if (!error) {
	if (CurrentPath.num_ops > 0 || path_type == PATH_TYPE_INITCLIP) {
	    bbox = pathbbox;

	    /* If stroking, increase bounding box by half the stroke width */
	    if (path_type == PATH_TYPE_STROKE) {
		insetBox (&bbox, -gparms.linewidth/2, -gparms.linewidth/2);
	    }

	    pObj = (Graphic *) XtMalloc(sizeof (Graphic));
	    gparms.path_type = (unsigned char) path_type;
	    pObj->parms = gparms;

	    if (path_type != PATH_TYPE_INITCLIP) {
		installUPathAndBounds(pObj, &CurrentPath, &bbox);
		updatePageBBox(&CurrentPage, &bbox);
	    }

	    addObjToPage(&CurrentPage, pObj);
	}

	resetPathStruct();
	nocurrentpoint = TRUE;
    }
}

/* Reset graphics state at end of page */

static void insertPage()
{	
    if (!error) {
	resetPathStruct();
	nocurrentpoint = TRUE;
    }
}

/* Add coordinates and an operator to the current path */

static void insertPathCoord(ptr_pts, num_pts, dps_op)
    Point 	*ptr_pts; 
    int		num_pts; 
    char	dps_op; 
{
    int	i;

    for (i = 0;  i < num_pts; i++, ptr_pts++) {
	CurrentPath.pts[CurrentPath.num_pts++] = ptr_pts->x;
	CurrentPath.pts[CurrentPath.num_pts++] = ptr_pts->y;
    }

    CurrentPath.ops[CurrentPath.num_ops++] = dps_op;		
}

/* Pop operands from the stack and add an operator to the path */

static void insertPathOp(dps_op, num_args)
    int	dps_op;
    int	num_args;
{
    Boolean	relative_op = TRUE; 
    Boolean	start_path = FALSE;	/* Whether op starts a path segment */
    int		i, num_pts;
    Point	pts[3];

    if (!error)	{
	switch (dps_op) {
	    case dps_rmoveto:
	        start_path = TRUE;
		break;
	    case dps_moveto:
		start_path = TRUE;
		relative_op = FALSE;
		break;
	    case dps_lineto:
	    case dps_curveto:
	    case dps_closepath:
		relative_op = FALSE;
		break;
	    }

	if ((relative_op || dps_op == dps_closepath) && nocurrentpoint) {
	    errorlog("no currentpoint", &error);
	} else {
	    /* Take pts off stack and put (back to front) into pts array */
	    num_pts = num_args/2;
	    for (i = num_pts - 1;  i >= 0;  i--) {
		popreal(&pts[i].y);
		popreal(&pts[i].x);
	    }

	    if (!error) {
		/*  Path from end to start and check bounds. */	
		checkBounds(pts, num_pts, relative_op);
		insertPathCoord(pts, num_pts, dps_op);

		if (relative_op) {
		    currentpoint.x += pts[num_pts-1].x;
		    currentpoint.y += pts[num_pts-1].y;
		} else if (dps_op == dps_closepath) currentpoint = startpoint;
		else currentpoint = pts[num_pts-1];
				
		if (start_path) startpoint = currentpoint;

		nocurrentpoint = FALSE;
	    }
	}
    }
}

/* Utilities to set graphics state parameters */

static void setgray(fval)
    float fval;
{
    gparms.gray = fval;
    gparms.color_type = COLOR_MODEL_GRAY;
}

static void setlinewidth(fval)
    float fval;
{
    gparms.linewidth = fval;
}

static void setlinejoin(ival)
    int	ival;
{
    gparms.linejoin = (unsigned char) ival;
}

static void setmiterlimit(fval)
    float fval;
{
    gparms.miterlimit = fval;
}

static void setlinecap (ival)
    int ival;
{
    gparms.linecap = (unsigned char) ival;
}

static void setrgbcolor(r, g, b)
    float r, g, b;
{
    gparms.red = r;
    gparms.green = g;
    gparms.blue = b;
    gparms.color_type = COLOR_MODEL_RGB;
}

/***************************************************************
**
** 		FIRST-LEVEL TOKEN PARSING FUNCTIONS
**
***************************************************************/

/* integer */

static void i_int()
{
    int	avalue;

    sscanf(yytext,"%d", &avalue);
    pushint(avalue);
}

/* real number */

static void i_real()
{
    float avalue;

    sscanf(yytext,"%f", &avalue);
    pushreal(avalue);
}

/* literal -- skip it */

static void i_literal()
{
}

/* name -- skip it */

static void i_name()
{
}

/* array -- skip it */

static void i_array()
{
}

/* string -- skip it */

static void i_string()
{
}

/* f (fill) operator */

static void i_f()
{
    insertPath(PATH_TYPE_FILL);
}

/* s (stroke) operator */

static void i_s()
{
    insertPath(PATH_TYPE_STROKE);
}

/* clip operator */

static void i_clip()
{
    insertPath(PATH_TYPE_CLIP);
}

/* T (text) operator -- unimplemented */

static void i_T()
{
    clear();
}

/* A (ashow) operator -- unimplemented */

static void i_A()
{
    clear();
}

/* W (widthshow) operator -- unimplemented */

static void i_W()
{
    clear();
}

/* AW (awidthshow) operator -- unimplemented */

static void i_AW()
{
    clear();
}

/* m (moveto) operator */

static void i_m()
{
    insertPathOp(dps_moveto, 2);
}

/* lineto operator */

static void i_lineto()
{
    insertPathOp (dps_lineto, 2);
}

/* L (multiple lineto) operator */

static void i_L()
{
    int i, index;

    popint(&index);	/* number of lines */
    for (i = index; i > 0; i--) insertPathOp(dps_lineto,2);
}

/* r (rlineto) operator */

static void i_r()
{
    insertPathOp(dps_rlineto, 2);
}

/* R (multiple rlineto) operator */

static void i_R()
{
    int i, index;

    popint (&index);
    for (i = index; i > 0; i--) insertPathOp(dps_rlineto, 2);
}

/* l (moveto-lineto) operator */

static void i_l()
{
    insertPathOp(dps_moveto, 2);
    insertPathOp(dps_lineto, 2);
    insertPath(PATH_TYPE_STROKE);
}

/* x (horizontal rlineto) operator */

static void i_x()
{
    pushreal(0.0);
    insertPathOp(dps_rlineto, 2);
}

/* y (vertical rlineto) operator */

static void i_y()
{
    pushreal(0.0);
    roll(2, 1);
    insertPathOp(dps_rlineto, 2);
}

/* X (moveto-horizontal rlineto) operator */

static void i_X()
{
    insertPathOp(dps_moveto, 2);
    pushreal(0.0);
    insertPathOp(dps_rlineto, 2);
    insertPath(PATH_TYPE_STROKE);
}

/* Y (moveto-vertical rlineto) operator */

static void i_Y()
{
    insertPathOp(dps_moveto, 2);
    pushreal(0.0);
    roll(2, 1);
    insertPathOp(dps_rlineto, 2);
    insertPath(PATH_TYPE_STROKE);
}

/* c (curveto) operator */

static void i_c()
{
    insertPathOp (dps_curveto, 6);
}

/* cp (closepath) operator */

static void i_cp()
{
    insertPathOp (dps_closepath, 0);
}

/* w (setlinewidth) operator */

static void i_w()
{
    float avalue;
	
    popreal(&avalue);
    setlinewidth(avalue);
}

/* g (setgray) operator */

static void i_g()
{
    float avalue;
	
    popreal(&avalue);
    setgray(avalue);
}

/* j (setlinejoin) operator */

static void i_j()
{
    int	avalue;
	
    popint(&avalue);
    setlinejoin(avalue);
}

/* d (setdash) operator -- unimplemented */

static void i_d()
{
    clear();
}

/* setmiterlimit operator */

static void i_miter()
{
    float avalue;
	
    popreal(&avalue);
    setmiterlimit(avalue);
}

/* setlinecap operator */

static void i_cap()
{
    float	data;
    unsigned 	parm;		
	
    popreal(&data);
    parm = (unsigned) floor(data);
    setlinecap(parm);
}

/* RGB (setrgbcolor) operator */

static void i_RGB()
{
    float		r, g, b;
	
    popreal(&b);
    popreal(&g);
    popreal(&r);
    setrgbcolor (r, g, b);
}

/* F (setfont) operator -- unimplemented */

static void i_F()
{
    /*  setfont not implemented */
    clear();
}

/* MF (makefont) operator -- unimplemented */

static void i_MF()
{
    clear();
}

/* FF (setfont) operator -- unimplemented */

static void i_FF()
{
    clear();
}
/* DF (definefont) operator -- unimplemented */
static void i_DF()
{
    clear();
}

/* IMASK (imagemask) operator -- unimplemented */

static void i_IMASK()
{
    clear();
}

/* IMAGE (image) operator -- unimplemented */
static void i_IMAGE()
{
    clear();
}

/* BPAGE (begin page) operator -- unimplemented */

static void i_BPAGE()
{
    pop();
}

/* EPAGE (end page) operator -- unimplemented */

static void i_EPAGE()
{
    pop();
    insertPage();
}

/* REMAP (font remapping) operator -- unimplemented */

static void i_REMAP()
{
    clear();
}

/* RECODE (font reencoding) operator -- unimplemented */

static void i_RECODE()
{
    clear();
}

/* initclip operator */

static void i_initclip()
{
    insertPath(PATH_TYPE_INITCLIP);
}

/***************************************************************
**
** 		DISTILLERY FUNCTIONS
**
***************************************************************/

/* Special DPS text handler procedure that saves the output of the
   DPSContext to a file. */

static FILE *outfile;
static Boolean writeErr;

/* ARGSUSED */

void saveTextProc (ctxt, buff, count)
    DPSContext	ctxt;
    char	*buff;
    unsigned long count;
{
    unsigned long	wrote;

    if (writeErr || buff == NULL || count <= 0) return;

    wrote = fwrite(buff, 1, count, outfile);
    if (wrote != count) {
	putUpInfoDialog(AppData.badWriteMessage);
	writeErr = True;
    }
} /* end of saveTextProc () */

/* Special DPS text handler procedure that throws away the text
   output of the DPSContext. */

/* ARGSUSED */

void tossTextProc(ctxt, buff, count)
    DPSContext	ctxt;
    char	*buff;
    long	count;
{
} /* end of tossTextProc () */

/* Convert a file into its distilled form */

void distillFile(f, name)
    FILE *f;
    String name;
{
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
    FILE 		*dfile;
    int 		len; 
    char 		*cp;
    char		*dname;
    char		nbuff [MAXPATHLEN]; 
    Boolean		errflag;
    static DPSContext	ctxt;
    static char		startCmd[] = "resyncstart clear cleardictstack\n";

    /* Create a context for the distiller.  Don't user our main
       one to protect from possible distillery bugs */

    if (ctxt == NULL) {
	ctxt = XDPSCreateSimpleContext(XtDisplay(AppData.time),
				       None, None, 0, 0,
				       DPSDefaultTextBackstop,
				       DPSDefaultErrorProc, NULL);

	if (ctxt == NULL) {
	    putUpInfoDialog(AppData.noDistillContextMessage);
	    return;
	}

	/* resync and init the context */
	DPSWritePostScript(ctxt, startCmd, strlen(startCmd));
    }
	
    DPSSetTextProc(ctxt, tossTextProc);

    /* Read the distillery program into a buffer */
    
    dname = getenv("POSTSCRIPT_DISTILLERY");
    if (dname == NULL) dname = "still.ps";
    dfile = fopen(dname, "r");
    if (dfile == NULL) {
	putUpInfoDialog(AppData.noDistillFileMessage);
	return;
    }
    if ((len = readFileIntoBuffer (dfile, &cp)) == 0) {
	fclose(dfile);
	putUpInfoDialog(AppData.noMemoryMessage);
	return;
    }
    fclose(dfile);
	
    /* Load the distiller program into the context */
    errflag = FALSE;
DURING
    DPSWritePostScript(ctxt, cp, len + 1);
HANDLER
    DPSResetContext(ctxt);
    errflag = TRUE;
END_HANDLER

    free(cp);
    if (errflag) {
	putUpInfoDialog(AppData.distillErrorMessage);
	return;
    }

    /* Change the source filename into the target filename */
    strcpy(nbuff, name);
    cp = rindex(nbuff, '.');
    if (cp != NULL) *cp = '\0';
    strcat(nbuff, ".dst");

    /* Load the input file into a buffer */
    if ((len = readFileIntoBuffer(f, &cp)) == 0) {
	putUpInfoDialog(AppData.noMemoryMessage);
	return;
    }

    /* Open the output file for writing */
    if ((outfile = fopen(nbuff, "w")) == NULL) {
	putUpInfoDialog(AppData.noOutputFileMessage);
	return;
    }

    /* Blast the input file at the context */

    DPSPrintf(ctxt, "distill\n");
    writeErr = False;
    DPSSetTextProc(ctxt, saveTextProc);
    DPSWaitContext(ctxt);

DURING
    DPSWritePostScript (ctxt, cp, len + 1);
HANDLER
    DPSResetContext(ctxt);
    errflag = TRUE;
END_HANDLER

    free(cp);
    DPSWaitContext(ctxt);
    DPSSetTextProc(ctxt, tossTextProc);
    fclose(outfile); 

    if (errflag) putUpInfoDialog(AppData.distillErrorMessage);
    else putUpInfoDialog(AppData.distillCompleteMessage);
} /* end of distillFile() */

