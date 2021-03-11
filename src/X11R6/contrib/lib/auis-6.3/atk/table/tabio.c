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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/table/RCS/tabio.c,v 1.24 1994/04/15 18:37:52 rr2b Exp $";
#endif


 

/* tabio.c - input/output for table */

#include <class.h>

#include <dataobj.ih>

#include <util.h>

#include <ctype.h>

#define AUXMODULE
#include <table.eh>

static boolean debug=FALSE;

#ifndef _IBMR2
extern char * malloc();
extern char * realloc();
#endif


/* read and write ASCII representation */

/*  * format for ASCII representation
 *
 * Cells are separated by tabs
 * Rows are separated by newlines
 *
 * Each cell has the following elements in order:
 *
 *
 *  1.  Color of line to left of cell
 *
 *    = if glued to cell at left (in which case nothing below appears)
 *    | if edge to left of cell is visible
 *    Either of these is followed by a space.  This is important because
 *    it distinguishes this from interspersed horizontal color lines (see below).
 *
 *
 *  2.  Cell format and precision (if not default values)
 *
 *   {<format><precision> <L>}
 *
 *
 *  3.  Special case for currency-formatted cells:
 *
 *   "$" if dollar format
 *
 *
 *  4.  Contents of cell: one of:
 *
 *     null cell:  zero or more blanks
 *       (exactly one blank is recommended, so that null cells at the end of the row are unambiguous)
 *     expressions to be calculated: =expression (= is optional if unambiguous without it)
 *     direct character strings: "string or 'string or ^string (' is optional if string starts with a letter)
 *     imbedded objects: \datatype  (in message buffer - data stream in file)
 *
 *
 * If horizontal lines are visible they are specified by additional
 * lines interspersed with the rows, containing:
 *
 *    - if the horizontal line between cells is visible
 *    = if the cells are glued together
 *    tab after each cell
 *
 * If row and/or column data is included it appears at the beginning:
 *
 *   \row t1 t2 ... tn
 *   \col t1 t2 ... tn
 *
 *   where ti is the thickness of row i.
 */

/* write thickness data */

static void WriteThickness(f, tag, slice, first, last, def)
FILE *f;
char *tag;
struct slice * slice;
int first, last;
{
    int i;
    int alldef;

    alldef = 1;
    for (i = first; i <= last; i++)
	if (slice[i].thickness != def) {
	    alldef = 0;
	    break;
	}
    if (alldef == 0) {
	fprintf(f, "\\%s", tag);
	for (i = first; i <= last; i++)
	    fprintf(f, " %d", slice[i].thickness);
	putc('\n', f);
    }
}

/* write color information */

static void WriteAboveColor(T, f, r, first, last)
register struct table * T;
FILE *f;
int r;
int first, last;
{
    int c, clast;

    for (c = first, clast = -1; c <= last; c++)
	if (table_ColorAbove(T, r, c) == BLACK
	    || table_IsJoinedAbove(T, r, c))
	    clast = c;
    for (c = first; c <= clast; c++) {
	if (table_ColorAbove(T, r, c) == BLACK)
	    putc('-', f);
	else if (table_IsJoinedAbove(T, r, c))
	    putc('=', f);
	putc((c < clast) ? '\t' : '\n', f);
    }
}

/* write string */

#define WriteString(x2,cp)\
{\
char *x=x2;\
    for (; *x; x++) {\
	if (*x == '\n') {\
	    ENSURESIZE(2);\
	    *cp++ = '\\', *cp++ = 'n';\
	}\
	else if (*x == '\t') {\
	    ENSURESIZE(2);\
	    *cp++ = '\\', *cp++ = 't';\
	}\
	else if (*x == '\\') {\
	    ENSURESIZE(2);\
	    *cp++ = '\\', *cp++ = *x;\
	}\
	else {\
	    ENSURESIZE(1);\
	    *cp++ = *x;\
	}\
    }\
}

#define ENSURESIZE(x)  do { if((pos+=(x))>=size) { *buff=realloc(*buff, size+=100+(x)); cp=(*buff)+pos-(x); if(*buff==NULL) { *buff=NewString("OUT OF MEMORY!!!"); return; } } } while (0)

/* write contents of one cell */

void WriteCell (T, f, cell, buff, level)
register struct table * T;
FILE *f;
struct cell * cell;
char **buff;
int level;
{
    int size=1000, pos=0;
    char *cp;
    cp = *buff =(char *)malloc(size);

    if (debug) {
	int n, r, c;
	n = cell - T->cells;
	r = n / table_NumberOfColumns(T) + 1;
	c = n % table_NumberOfColumns(T) + 1;
	printf("WriteCell(,, [%d,%d],, %d)\n", r, c, level);
    }

    /* output format and precision and lock */

    if ((cell->format != CURRENCYFORMAT
	&& cell->format != GENERALFORMAT)
	|| cell->precision != table_DefaultPrecision
	|| cell->lock) {
	ENSURESIZE(2);
	*cp++ = '{';
        *cp++ = cell->format;
	if (cell->precision != table_DefaultPrecision) {
	    char sbuf[16];
	    sprintf(sbuf, "%d", cell->precision);
	    ENSURESIZE(strlen(sbuf));
	    strcpy(cp, sbuf);
	    while (*cp) cp++;
	}
	if (cell->lock) {
	    ENSURESIZE(2);
	    *cp++ = ' ';
	    *cp++ = 'L';
	}
	ENSURESIZE(2);
	*cp++ = '}';
	*cp++ = ' ';
    }

    /* special case for % and $ */

    if (cell->format == CURRENCYFORMAT) {
	ENSURESIZE(1);
	*cp++ = cell->format;
    }

    /* output formula or text */

    switch (cell->celltype) {
	case table_EmptyCell:
	    ENSURESIZE(1);
	    *cp++ = ' ';
	    break;
	case table_TextCell:
	    WriteString(cell->interior.TextCell.textstring, cp);
	    if (debug)
		printf("text string is %s\n", buff);
	    break;
	case table_ValCell:
	    if (!isdigit(*cell->interior.ValCell.formula)) {
		ENSURESIZE(1);
		*cp++ = '=';
	    }
	    WriteString(cell->interior.ValCell.formula, cp);
	    break;
	case table_ImbeddedObject:
	    if (!(cell->interior.ImbeddedObject.data)) {
		static char tp[] = "MISSING DATA OBJECT!";
		ENSURESIZE(sizeof(tp)-1);
		strcpy(cp, tp);
		cp+=sizeof(tp)-1;
	    }
	    else if (f) {
		if (debug)
		    printf("Writing out %s\n", class_GetTypeName(cell->interior.ImbeddedObject.data));
		dataobject_Write (cell->interior.ImbeddedObject.data, f, super_GetWriteID(T), level + 1);
	    }
	    else {
		char *tp = class_GetTypeName(cell->interior.ImbeddedObject.data);
		ENSURESIZE(1+strlen(tp));
		*cp++ = '\\';
		while (*tp)
		    *cp++ = *tp++;
	    }
	    break;
    }
    ENSURESIZE(1);
    *cp = '\0';
}

/* write subrectangle */

void WriteASCII (T, f, chunk, level)
register struct table * T;
FILE *f;
Chunk chunk;
int level;
{
    int r, c;
    int fr = max(0, chunk->TopRow);
    int lr = chunk->BotRow;
    int fc = max(0, chunk->LeftCol);
    int lc = chunk->RightCol;

    if (debug)
	printf("WriteASCII(,,[%d..%d, %d..%d], %d)\n", fr+1, lr+1, fc+1, lc+1, level);
    if (chunk->LeftCol < 0)
	WriteThickness(f, "rows", T->row, fr, lr, TABLE_DEFAULT_ROW_THICKNESS);
    if (chunk->TopRow < 0)
	WriteThickness(f, "cols", T->col, fc, lc, TABLE_DEFAULT_COLUMN_THICKNESS);

    for (r = fr; r <= lr; r++ ) {

	WriteAboveColor(T, f, r, fc, lc);

	for (c = fc; c <= lc; c++) {
	    char *buff;

	    if (table_IsJoinedToLeft(T, r, c)) {
		putc('=', f);
		putc(' ', f);
	    }
	    else if (table_ColorToLeft(T, r, c) == BLACK) {
		putc('|', f);
		putc(' ', f);
	    }

	    if (!table_IsJoinedToAnother(T, r, c)) {
		WriteCell(T, f, table_GetCell(T, r, c), &buff, level);
		fprintf(f, "%s", buff);
		free(buff);
	    }

	    if (c < lc) putc('\t', f);
	}
	if (table_ColorToLeft(T, r, lc + 1) == BLACK) {
	    putc('\t', f);
	    putc('|', f);
	    putc(' ', f);
	}
	putc('\n', f);
    }

    WriteAboveColor(T, f, lr + 1, fc, lc);
}

/* refill buffer if necessary */

/* if f is NULL, we are simply scanning a line buffer with cp; buff and cl are meaningless */
/* in any case, the buffer is terminated with a null */

#define advance(f, buff, cp, cl) {if(*cp) cp++; if (*cp == '\0') refill(f, buff, &cp, cl);}

/* read an input line */

/* by reading starting at buff+1 we leave one character worth of putback */

static void refill(f, buff, cpp, cl)
FILE *f;
char buff[];
char **cpp;
char *cl;
{
    char *cp;
    int inc = 0;

    if (f != NULL) {
	for (*cpp = cp = buff+1; (inc = getc(f)) > 0; ) {
	    *cp++ = inc;
	    if (inc == '\n')
		break;
	    if (cp >= cl - 1) {
		break;
	    }
	}
	*cp = '\0';
	if (debug)
	    printf("Input line:  \'%s\' inc=%d\n", buff+1, inc);
    }
}

/* read end of line and refill buffer */

static void SkipRest(f, buff, cpp, cl)
FILE *f;
char buff[];
char **cpp;
char *cl;
{
    char *cp = *cpp;

    if (*cp != '\n' && *cp != '\0') {
	printf("Ignored extra text at end of line:  '");
	while (*cp != '\n' && *cp != '\0') {
	    printf("%c", *cp);
	    advance(f, buff, cp, cl);
	}
	printf("'\n");
    }
    advance(f, buff, cp, cl);
    *cpp = cp;
}

/* read thickness vector */

static int ReadSlice(f, buff, cpp, cl, sp)
FILE *f;
char buff[];
char **cpp;
char *cl;
struct slice **sp;
{
    int i, t;
    char *cp = *cpp;

    *sp = (struct slice *) malloc (sizeof (struct slice));
    if (*sp == NULL) {
	printf("Out of memory reading table (ReadSlice)\n");
	return -1;
    }
    for (i = 0; *cp != '\0' && *cp != '\n'; i++) {
	while (*cp == ' ')
	    advance(f, buff, cp, cl);
        if (*cp < '0' || *cp > '9') {
	    *cpp = cp;
            return 0;
	}
	*sp = (struct slice *) realloc( (char *) *sp, (i + 1) * sizeof (struct slice));
	if (*sp == NULL) {
	    printf("Out of memory reading table (ReadSlice)\n");
	    return -1;
	}
        for (t = 0; *cp >= '0' && *cp <= '9'; ) {
	    t = t * 10 + (*cp - '0');
	    advance(f, buff, cp, cl);
	}
        (*sp)[i].thickness = t;
	while (*cp == ' ')
	    advance(f, buff, cp, cl);
    }
    *cpp = cp;
    return i;
}

/* read color information */

static int ReadLeftColor(T, f, buff, cpp, cl, r, c)
struct table *T;
FILE *f;
char buff[];
char **cpp;
char *cl;
int r, c;
{
    int color;
    char lastc;
    char *cp = *cpp;
    int n;

    color = GHOST;

    n = 0;
    if (*cp == '=' || *cp == '|') {
	lastc = *cp;
	advance(f, buff, cp, cl);
	cp--;
	*cp = lastc;
	if (*cp == '=' && *(cp+1) == ' ') {
	    color = JOINED;
	    advance(f, buff, cp, cl);
	    n++;
	}
	else if (*cp == '|' && *(cp+1) == ' ') {
	    color = BLACK;
	    advance(f, buff, cp, cl);
	    n++;
	}
	if (*cp == ' ') {
	    advance(f, buff, cp, cl);
	    n++;
	}
    }

    table_SetLeftColor(T, r, c, color);
    *cpp = cp;
    return n;
}

/* read horizontal colors and return true if there were some */

static int ReadAboveColor(T, f, buff, cpp, cl, r)
register struct table * T;
FILE *f;
char buff[];
char **cpp;
char *cl;
int r;
{
    int c;
    int cmax;
    char lastc;
    char *cp = *cpp;
    char *x;

    /* see if this is an "above color" line
	this code only looks at the first bufferful,
	which could be a problem (unlikely) */

    for (c = 0, cmax = -1, x = cp; *x != '\0' && *x != '\n'; ) {
	if (*x == '-' || *x == '=') {
	    cmax = c;
	    lastc = *x;
	    while (*x == lastc) x++;
	}
	if (*x == '\t') x++;
	else if (*x != '\0' && *x != '\n')
	    return 0;
    }
    if (cmax < 0)
	return 0;
    
    if (c >= table_NumberOfColumns(T))
	table_ChangeSize (T, table_NumberOfRows(T), c + 1);
    if (r > table_NumberOfRows(T))
	table_ChangeSize (T, r, table_NumberOfColumns(T));

    for (c = 0; *cp != '\0' && *cp != '\n'; ) {
	if (*cp == '-' || *cp == '=') {
	    if (c >= table_NumberOfColumns(T))
		table_ChangeSize (T, table_NumberOfRows(T), c + 1);
	    table_SetAboveColor(T, r, c, (*cp == '-') ? BLACK : JOINED);
	    lastc = *cp;
	    while (*cp == lastc)
		advance(f, buff, cp, cl);
	}
	if (*cp == '\t') {
	    advance(f, buff, cp, cl);
	    c++;
	}
    }
    *cpp = cp;
    return 1;
}

/* read string */

static void ReadString(f, buff, cpp, cl, result)
FILE *f;
char buff[];
char **cpp;
char *cl;
char **result;
{
    char *x;
    int k;
    int n;
    char *cp = *cpp;
    char *y;

    y = NULL;
    n = k = 0;
    for (;;) {
	/* prescan to determine size and get space with few realloc calls */
	for (x = cp; *x != '\0' && *x != '\n' && *x != '\t'; x++, n++)
	    if (*x == '\\')
		x++;
	if (y == NULL)
	    *result = y = malloc(n + 1);
	else
	    *result = y = realloc(y, n + 1);
	if (y == NULL) {
	    printf("Out of space reading table (ReadString)\n");
	    *cpp = x;
	    return;
	}
	/* copy into allocated space */
	while (k < n) {
	    if (*cp == '\\') {
		advance(f, buff, cp, cl);
		switch (*cp) {
		    case 'n':
			y[k++] = '\n';
			break;
		    case 't':
			y[k++] = '\t';
			break;
		    default:
			y[k++] = *cp;
		}
	    } else
		y[k++] = *cp;
	    advance(f, buff, cp, cl);
	}
	/* stop at tab or newline */
	if (*cp == '\0' || *cp == '\n' || *cp == '\t')
	    break;
    }
    y[k] = '\0';
    *cpp = cp;
    return;
}

/* read a cell */

void ReadCell(T, f, buff, cpp, cl, cell)
register struct table *T;
FILE *f;
char *buff;
char **cpp;
char *cl;
struct cell *cell;
{
    char *cp = *cpp;

    DestroyCell(T, cell);

    /* get format, precision, and lock */

    while (*cp == ' ')
	advance(f, buff, cp, cl);
    if (*cp == '{') {
	advance(f, buff, cp, cl);
	if (*cp != ' ') {
	    cell->format = GENERALFORMAT;
	    cell->precision = table_DefaultPrecision;
	    if (*cp != '}' && (*cp < '0' || *cp > '9')) {
		    cell->format = *cp;
		    advance(f, buff, cp, cl);
	    }
	    if (*cp >= '0' && *cp <= '9') {
		int precision;
		for (precision = 0; *cp >= '0' && *cp <= '9'; ) {
		    precision = (precision * 10) + (*cp - '0');
		    advance(f, buff, cp, cl);
		}
		cell->precision = precision;
	    }
	}
	while (*cp == ' ')
	    advance(f, buff, cp, cl);
	if (*cp == 'L')
	    cell->lock = 1;
	while (*cp != '\0' && *cp != '\n' && *cp != '}' && *cp != '\t')
	    advance(f, buff, cp, cl);
	if (*cp == '}')
	    advance(f, buff, cp, cl);
    }

    /* special case for $ */

    while (*cp == ' ') 
	advance(f, buff, cp, cl);
    if (*cp == '$') {
	cell->format = CURRENCYFORMAT;
	advance(f, buff, cp, cl);
    }

    /* get string */

    if (*cp == '\'' || *cp == '^' || *cp == '\"' || isalpha(*cp)) {
	cell->celltype = table_TextCell;
	ReadString(f, buff, &cp, cl, &(cell->interior.TextCell.textstring));
	if (cell->interior.TextCell.textstring == NULL)
	    cell->celltype = table_EmptyCell;
    }

    /* get imbedded object from buffer */

    else if (*cp == '\\' && f == NULL) {
	/* leave it null */
	while (*cp != '\0' && *cp != '\n' && *cp != '\t')
	    advance(f, buff, cp, cl);
    }

    /* get imbedded object */

    else if (strncmp(cp, "\\begindata{", 11) == 0) {
	char dataname[256];
	register char *np;
	long uniqueID;

	for (cp += 11, np = dataname; np < dataname + 255 && *cp != '\0' && *cp != ','; ) {
	    *np++ = *cp;
	    advance(f, buff, cp, cl);
	}
	*np = '\0';
	if (*cp == ',')
	    advance(f, buff, cp, cl);
	while (*cp == ' ')
	    advance(f, buff, cp, cl);
	uniqueID = 0;
	while (*cp >= '0' && *cp <= '9') {
	    uniqueID = uniqueID * 10 + (*cp - '0');
	    advance(f, buff, cp, cl);
	}
	while (*cp == ' ')
	    advance(f, buff, cp, cl);
	if (*cp == '}') {
	    advance(f, buff, cp, cl);
	    while (*cp == ' ')
		advance(f, buff, cp, cl);
	}
	if (*cp != '\0' && *cp != '\n') {
	    struct dataobject *u=(struct dataobject *)class_NewObject("unknown");
	    fprintf(stderr, "Wrong begindata format: %s\n", buff);
	    fprintf(stderr, "Skipping the '%s' inset, it had a \\begindata with no trailing newline.\n", dataname);
	    if(u==NULL) {
		fprintf(stderr, "Even the 'unknown' inset couldn't be created.\n");
	    } else {
		dataobject_Read(u, f, uniqueID);
		dataobject_Destroy(u);
	    }
	}	    
	else {
	    struct dataobject *newobject = (struct dataobject *) class_NewObject(dataname);
	    if (!newobject) {
		fprintf(stderr, "Could not create '%s' inset.\n", dataname);
		fprintf(stderr, "Trying for the 'unknown' inset.\n");
		newobject=(struct dataobject *)class_NewObject("unknown");
		if(newobject==NULL) {
		    fprintf(stderr, "Even the 'unknown' inset couldn't be created.\n");
		} else fprintf(stderr, "Made an 'unknown' inset.\n");
	    }
	    if(newobject) {
		cell->celltype = table_ImbeddedObject;
		cell->interior.ImbeddedObject.data = newobject;
		cell->interior.ImbeddedObject.views = NULL;
		newobject->id = uniqueID;
		
		dataobject_AddObserver(newobject,T);
		
		if (f) {
		    dataobject_Read(newobject, f, uniqueID);
		    if (debug)
			printf("Additional ");
		    refill(f, buff, &cp, cl);
		}
	    }
	}
    }

    /* get formula */

    else if (*cp != '\0' && *cp != '\n' && *cp != '\t') {
	if (*cp == '=')
	    advance(f, buff, cp, cl);
	cell->celltype = table_ValCell;
	ReadString(f, buff, &cp, cl, &(cell->interior.ValCell.formula));
	cell->lastcalc = 0;
	if (cell->interior.ValCell.formula == NULL)
	    cell->celltype = table_EmptyCell;
    }

    /* skip trailing spaces */
    while (*cp == ' ')
	advance(f, buff, cp, cl);

    *cpp = cp;
    return;
}

/* read subrectangle */

struct table * ReadASCII (T, f)
register struct table * T;
FILE *f;
{
    char buff[1000];
    char *cp;
    char *cl;
    int r, c, n;
    struct slice * s;

    /* need 1x1 for ChangeSize to allocate the edge array */

    table_ChangeSize (T, 0, 0);
    table_ChangeSize (T, 1, 1);

    T->everythingChanged = T->cellChanged = ++(T->timeStamper);

    /* read first line of input */

    cl = buff + sizeof buff;
    refill(f, buff, &cp, cl);
    
    if (strncmp(cp, "\\begindata{table,", 17) == 0) {
	if (debug)
	printf("*** Skipped begindata - if you see this, bug Andy Palay ***\n");
	refill(f, buff, &cp, cl);
    }
    if (*cp == '\n') {
	if (debug)
	    printf("Skipped empty input line at beginning of table!\n");
	refill(f, buff, &cp, cl);
    }

    /* read thickness information */

    if (strncmp(cp, "\\rows", 5) == 0) {
	if (debug)
	    printf("Got rows\n");
	cp += 5;
	n = ReadSlice(f, buff, &cp, cl, &s);
	if (n > 0) {
	    if (n > table_NumberOfRows(T))
		table_ChangeSize (T, n, table_NumberOfColumns(T));
	    for (r = 0; r < n; r++)
		T->row[r].thickness = s[r].thickness;
	}
	if (s)
	    free (s);
	s = NULL;
	SkipRest(f, buff, &cp, cl);
    }

    if (strncmp(cp, "\\cols", 5) == 0) {
	if (debug)
	    printf("Got cols\n");
	cp += 5;
	n = ReadSlice(f, buff, &cp, cl, &s);
	if (n > 0) {
	    if (n > table_NumberOfColumns(T))
		table_ChangeSize (T, table_NumberOfRows(T), n);
	    for (c = 0; c < n; c++)
		T->col[c].thickness = s[c].thickness;
	}
	if (s)
	    free (s);
	s = NULL;
	SkipRest(f, buff, &cp, cl);
    }

    for (r = -1; *cp != '\0';  ) {

	if (ReadAboveColor(T, f, buff, &cp, cl, r + 1))
	    SkipRest(f, buff, &cp, cl);
	if (*cp == '\0') {
	    if (debug)
		printf("End of input\n");
	    break;
	}

	if (strncmp(cp, "\\enddata{", 8) == 0) 
	    break;

	if (++r >= table_NumberOfRows(T))
	    table_ChangeSize (T, r + 1, table_NumberOfColumns(T));

	if (debug)
	    printf("*** row %d *** for \'%s\'\n", r + 1, cp);

	for (c = -1;;) {

	    n = ReadLeftColor(T, f, buff, &cp, cl, r, c+1);
	    if ((*cp == '\0' || *cp == '\n') && n > 0)
		break;

	    if (++c >= table_NumberOfColumns(T))
		table_ChangeSize (T, table_NumberOfRows(T), c + 1);
	    
	    ReadCell(T, f, buff, &cp, cl, table_GetCell(T, r, c));

	    if (*cp != '\t')
		break;
	    advance(f, buff, cp, cl);
	}

	SkipRest(f, buff, &cp, cl);
    }

    return T;
}
