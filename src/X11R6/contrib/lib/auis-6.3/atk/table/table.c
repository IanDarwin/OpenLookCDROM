/*LIBS: -lm
*/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/table/RCS/table.c,v 1.24 1993/08/18 20:22:06 rr2b Exp $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

 

#include <andrewos.h>
#include <class.h>
#include <ctype.h>
#include <dataobj.ih>
#include <view.ih>
#include <graphic.ih>
#include <fontdesc.ih>

#include <table.eh>

#ifndef _IBMR2
extern char * malloc ();
extern char * realloc ();
#endif /* _IBMR2 */

extern double atof();

struct table * ReadASCII ();

/* globals for entire package */

struct table * table_List;	/* root of list of all tables */

char table_debug;			/* nonzero if debugging */
int table_DefaultPrecision;		/* precision when none provided */

/* initialize entire class */

boolean table__InitializeClass(classID)
    struct classheader *classID;
{
    if (table_debug)
	printf("table_InitializeClass()\n");

    table_DefaultPrecision = 2;
    table_debug = 0;
    return TRUE;
}

/* return corresponding view name */

char * table__ViewName (T)
register struct table * T;
{
    return "spread";
}

/* table data structure creation */

/* Initialize new data table */

boolean table__InitializeObject (classID, T)
struct classheader *classID;
register struct table * T;
{
    static int uniquifier = 0;
    char buff[20];

    if (table_debug)
	printf("table_InitializeObject()\n");

    T->tablename = NULL;
    T->NextTable = NULL;
    T->rows = T->cols = 0;
    T->row = T->col = NULL;
    T->cells = NULL;
    T->leftedge = NULL;
    T->aboveedge = NULL;
    T->edgeChanged = T->cellChanged = T->fileWritten = 0;
    T->everythingChanged = T->timeStamper = 1;
    T->inGetModified = FALSE;

    /* the following is unnecessary for internal tables */

    table_ChangeSize (T, 10, 5);
    sprintf(buff, "Table%d", ++uniquifier);
    table_SetName (T, buff);
    return TRUE;
}


void IgnoreObserved(T)
struct table *T;
{
    long r,c;
    struct cell *cell;
    for (r = 0; r < table_NumberOfRows(T); r++) {
	for (c = 0; c < table_NumberOfColumns(T); c++) {
	    cell = table_GetCell(T, r, c);
	    if (cell->celltype == table_ImbeddedObject && cell->interior.ImbeddedObject.data!=NULL)
		dataobject_RemoveObserver( cell->interior.ImbeddedObject.data,T);
	}
    }
}
    
/* tear down a table */

void table__FinalizeObject (classID, T)
struct classheader *classID;
register struct table * T;
{
    register struct table * S;
    
    if (table_debug)
	printf("table_FinalizeObject(%s)\n", table_Name(T));
    
    table_NotifyObservers(T,observable_OBJECTDESTROYED);
    IgnoreObserved(T);
    
    if (T->tablename) {
	if (T == table_List)
	    table_List = T->NextTable;
	else {
	    for (S = table_List; S->NextTable != T; S = S->NextTable) ;
	    S->NextTable = T->NextTable;
	}
	free (T->tablename);
	T->tablename = NULL;
    }
    table_ChangeSize (T, 0, 0);
}

void table__ObservedChanged(T, changed, value)
struct table *T;
struct observable *changed;
long value;
{
    T->cellChanged = ++(T->timeStamper);
    table_NotifyObservers(T, 0);
    table_SetModified(T);
}

/* toggle table_debugging flag */

void table__ToggleDebug (T)
{
    if (table_debug) {
	printf("Table debugging off\n");
	table_debug = 0;
    } else {
	table_debug = 1;
	printf("Table debugging on - ESC-ESC turns it off\n");
    }
}

/* find a table by name */

struct table *table__FindName (T, name)
register struct table *T;
char * name;
{
    if (table_debug)  {
	printf("table_FindName(%s, %s)\n", table_Name(T), name);
    }

    for (T = table_List; T; T = T->NextTable)
	if ((T->tablename != NULL) && strcmp (name, T->tablename) == 0)
	    break;
    return T;
}

/* rename a table (may return pointer to pre-existing table) */

struct table * table__SetName (T, name)
register struct table * T;
char * name;
{
    register struct table * S;

    if (table_debug)
	printf("table_SetName(%s, %s)\n", table_Name(T), name);
    
    S = table_FindName (T, name);
    if (S)
    	return S;

    if (T->tablename)
	free (T->tablename);
    else {
	T->NextTable = table_List;
	table_List = T;
    }
    T->tablename = malloc (strlen (name) + 1);
    if (T->tablename == NULL)
	printf("Out of memory for table name\n");
    else
	strcpy (T->tablename, name);
    return T;
}

/* reallocate including provisions for null pointers */

static char * myrealloc (s, n)
char * s;
int n;
{
    char *news;

    if (s == NULL && n <= 0)
	return NULL;
    else if (s == NULL)
	news = malloc (n);
    else if (n <= 0) {
	free (s);
	return NULL;
    }
    else
	news = realloc (s, n);

    if (news == NULL) {
	printf("Out of memory for new table\n");
	exit(4);
    }
    return news;
}

/* change dimensions of existing table */

void table__ChangeSize (T, nrows, ncols)
register struct table * T;
int nrows, ncols;
{
    int     r, c;
    struct cell *newcells, *p, *q;
    char *oldleft, *oldabove, *eleft, *eabove, *fleft, *fabove;
    int oldrows, oldcols;

    if (table_debug)
	printf("table_ChangeSize(%s, %d, %d)\n", table_Name(T), nrows, ncols);

    /* the row and col arrays are one-dimensional, so realloc works */

    T->row = (struct slice   *) myrealloc ((char *)T->row, nrows * sizeof (struct slice));
    for (r = table_NumberOfRows(T); r < nrows; r++)
	T->row[r].thickness = TABLE_DEFAULT_ROW_THICKNESS;

    T->col = (struct slice   *) myrealloc ((char *)T->col, ncols * sizeof (struct slice));
    for (c = (table_NumberOfColumns(T) >= 0 ? table_NumberOfColumns(T) : 0); c < ncols; c++)
	T->col[c].thickness = TABLE_DEFAULT_COLUMN_THICKNESS;

    /* reallocate and copy cell array */

    for (r = nrows, p = table_GetCell(T, nrows, 0); r < table_NumberOfRows(T); r++)
	for (c = 0; c < table_NumberOfColumns(T); c++, p++)
	    DestroyCell (T, p);

    if (ncols == table_NumberOfColumns(T))
	p = q = newcells = (struct cell *) myrealloc ( (char *)T->cells,
	    nrows * ncols * sizeof (struct cell));
    else {
	p = T->cells;
	q = newcells = (struct cell *) myrealloc ( (char *)NULL,
	    nrows * ncols * sizeof (struct cell));
    }

    for (r = 0; r < nrows && r < table_NumberOfRows(T); r++) {
	for (c = 0; c < ncols && c < table_NumberOfColumns(T); c++, p++, q++)
	    if (p != q)
		bcopy ((char *)p, (char *)q, sizeof(struct cell));
	for ( ; c < ncols; c++, q++) 
	    CreateCell (T, q, (struct cell *) NULL);
	for ( ; c < table_NumberOfColumns(T); c++, p++)
	    DestroyCell (T, p);
    }
    for ( ; r < nrows; r++)
	for (c = 0; c < ncols; c++, q++)
	    CreateCell (T, q, (struct cell *) NULL);

    if (ncols != table_NumberOfColumns(T))
	T->cells = (struct cell *)myrealloc ((char *)T->cells, 0);
    T->cells = newcells;


    /* reallocate and copy edges */

    oldrows = table_NumberOfRows(T);
    oldcols = table_NumberOfColumns(T);
    T->rows = nrows;
    table_NumberOfColumns(T) = ncols;
    if (ncols == oldcols) {
	oldleft = oldabove = NULL;
	eleft = fleft = T->leftedge = myrealloc (T->leftedge,
	    adjustbyone(nrows) * adjustbyone(ncols) * sizeof (char));
	eabove = fabove = T->aboveedge = myrealloc (T->aboveedge,
	    adjustbyone(nrows) * adjustbyone(ncols) * sizeof (char));
    }
    else {
	eleft = oldleft = T->leftedge;
	eabove = oldabove = T->aboveedge;
	fleft = T->leftedge = myrealloc ( (char *) NULL,
	    adjustbyone(nrows) * adjustbyone(ncols) * sizeof (char));
	fabove = T->aboveedge = myrealloc ( (char *) NULL,
	    adjustbyone(nrows) * adjustbyone(ncols) * sizeof (char));
    }

    for (r = 0; r < adjustbyone(nrows) && r < adjustbyone(oldrows); r++) {
	for (c = 0; c < adjustbyone(ncols) && c < adjustbyone(oldcols); c++) {
	    *fleft++ = *eleft++;
	    *fabove++ = *eabove++;
	}
	for ( ; c < adjustbyone(ncols); c++) {
	    *fleft++ = *fabove++ = GHOST;
	}
	for ( ; c < adjustbyone(oldcols); c++, eleft++, eabove++)
	    /* do nothing */ ;
    }
    for ( ; r < adjustbyone(nrows); r++)
	for (c = 0; c < adjustbyone(ncols); c++) {
	    *fleft++ = *fabove++ = GHOST;
	}

    if (oldleft != NULL)
	oldleft = myrealloc (oldleft, 0);
    if (oldabove != NULL)
	oldabove = myrealloc (oldabove, 0);

    table_StampEverything(T);
    table_NotifyObservers(T, 0);
    table_SetModified(T);
}

/* extract chunk of table (destroys original) */

struct table * table__ExtractData (T, chunk)
register struct table * T;
Chunk chunk;
{
    register struct table * S = table_New ();
    int r0, c0, nrows, ncols;
    int     r, c;

    if (table_debug)
	printf("table_ExtractData(%s, )\n", table_Name(T));

    r0 = max (chunk->TopRow, 0);
    c0 = max (chunk->LeftCol, 0);
    nrows = chunk->BotRow + 1 - r0;
    ncols = chunk->RightCol + 1 - c0;

    table_ChangeSize (S, nrows, ncols);

    for (r = 0; r < table_NumberOfRows(S); r++)
	S->row[r].thickness = T->row[r + r0].thickness;

    for (c = 0; c < table_NumberOfColumns(S); c++)
	S->col[c].thickness = T->col[c + c0].thickness;

    for (r = 0; r < table_NumberOfRows(S); r++) {
	for (c = 0; c < table_NumberOfColumns(S); c++) {
	    struct cell *q = table_GetCell(S, r, c), *p = table_GetCell(T, r + r0, c + c0);
	    DestroyCell (S, q);
	    *q = *p;
	    CreateCell (T, p, (struct cell *) NULL);
	}
    }

    for (r = 0; r < adjustbyone(table_NumberOfRows(S)); r++) {
	for (c = 0; c < adjustbyone(table_NumberOfColumns(S)); c++) {
	    table_SetLeftColor(S, r, c, table_ColorToLeft(T, r+r0, c+c0));
	    table_SetAboveColor(S, r, c, table_ColorAbove(T, r+r0, c+c0));
	}
    }
    T->cellChanged = ++(T->timeStamper);
    table_NotifyObservers(T, 0);
    table_SetModified(T);
    return S;
}

/* insert chunk of table */

void table__InsertData (S, T, chunk)
register struct table * S;
register struct table * T;
Chunk chunk;
{
    int r0, c0;
    int r1, c1;
    int     r, c;
    int nrows, ncols;
    struct cell *cell;
    int iscombined;
    int newcolor;

    if (table_debug)
	printf("table_InsertData(%s, %s, )\n", table_Name(S), table_Name(T));

    /* convenient variables for size and location of chunk */

    r0 = max (chunk->TopRow, 0);
    c0 = max (chunk->LeftCol, 0);
    nrows = chunk->BotRow + 1 - r0;
    ncols = chunk->RightCol + 1 - c0;
    r1 = table_NumberOfRows(T);
    c1 = table_NumberOfColumns(T);

    /* if pasting into a combined cell, paste only into main cell */

    iscombined = 1;
    for (r = 0; r < nrows && iscombined == 1; r++) {
	for (c = 0; c < ncols && iscombined == 1; c++) {
	    if (r > 0 && !table_IsJoinedAbove(S, r+r0, c+c0) || c > 0 && !table_IsJoinedToLeft(S, r+r0, c+c0))
		iscombined = 0;
	}
    }

    /* replace thickness data only if entire row/column involved */

    if (chunk->LeftCol < 0)
	for (r = 0; r < nrows; r++)
	    S->row[r + r0].thickness = T->row[r % r1].thickness;
    if (chunk->TopRow < 0)
	for (c = 0; c < ncols; c++)
	    S->col[c + c0].thickness = T->col[c % c1].thickness;

    for (r = 0; r < nrows; r++) {
	for (c = 0; c < ncols; c++) {

	    /* replace contents of cells */

	    cell = table_GetCell(S, r + r0, c + c0);
	    DestroyCell (S, cell);
	    CreateCell (S, cell, table_GetCell(T, r % r1, c % c1));
	    
	   if(cell->celltype==table_ImbeddedObject)
	       dataobject_RemoveObserver( cell->interior.ImbeddedObject.data,T);
	   
	    if (iscombined != 0) break;

	    /* replace interior boundaries */

	    if (c > 0) {
		newcolor = table_ColorToLeft(T, r%r1, c%c1);
		if ((c%c1) == 0 && newcolor == GHOST)
		    newcolor = table_ColorToLeft(T, r%r1, c1);
		table_SetLeftColor(S, r+r0, c+c0, newcolor);
	    }
	    if (r > 0) {
		newcolor = table_ColorAbove(T, r%r1, c%c1);
		if ((r%r1) == 0 && newcolor == GHOST)
		    newcolor = table_ColorAbove(T, r1, c%c1);
		table_SetAboveColor(S, r+r0, c+c0, newcolor);
	    }
	}
	if (iscombined != 0) break;
    }

    /* replace exterior boundaries */

    for (c = 0; c < ncols; c++) {
	if (table_ColorAbove(S, r0, c+c0) == GHOST)
	    table_SetAboveColor(S, r0, c+c0, table_ColorAbove(T, 0, c % c1));
	if (table_ColorAbove(S, nrows+r0, c+c0) == GHOST)
	    table_SetAboveColor(S, nrows+r0, c+c0, table_ColorAbove(T, (nrows - 1) % r1 + 1, c % c1));
    }
    for (r = 0; r < nrows; r++) {
	if (table_ColorToLeft(S, r+r0, c0) == GHOST)
	    table_SetLeftColor(S, r+r0, c0, table_ColorToLeft(T, r % r1, 0));
	if (table_ColorToLeft(S, r+r0, ncols+c0) == GHOST)
	    table_SetLeftColor(S, r+r0, ncols+c0, table_ColorToLeft(T, r % r1, (ncols - 1) % c1 + 1));
    }

    /* tell the world */

    table_StampEverything(S);
    table_NotifyObservers(S, 0);
    table_SetModified(T);
}

/* write table to file */

long table__Write (T, f, writeID, level)
register struct table * T;
FILE * f;
long writeID;
int level;
{
    struct chunk chunk;

    if (table_debug)
	printf("table_Write(%s,, %ld, %d)\n", table_Name(T), writeID, level);

    if (getDataObject(T).writeID != writeID) {
	getDataObject(T).writeID = writeID;
	fprintf (f, "\\begindata{%s,%ld}\n", class_GetTypeName(T), table_GetID(T));
	chunk.LeftCol = -1;
	chunk.RightCol = table_NumberOfColumns(T) - 1;
	chunk.TopRow = -1;
	chunk.BotRow = table_NumberOfRows(T) - 1;
	WriteASCII (T, f, &chunk, level);
	fprintf (f, "\\enddata{%s,%ld}\n", class_GetTypeName(T), table_GetID(T));
    }
    return table_GetID(T);
}

/* read table from file */

long table__Read (T, f, id)
register struct table * T;
FILE * f;
long id;
{
    if (table_debug)
	printf("table_Read(%s,, %d)\n", table_Name(T), id);

    table_SetID(T, table_UniqueID(T));
    table_SetModified(T);
    ReadASCII (T, f);

    T->fileWritten = T->timeStamper;
    table_NotifyObservers(T, observable_OBJECTCHANGED);
    return dataobject_NOREADERROR;
}

/* write subrectangle */

void table__WriteASCII (T, f, chunk)
register struct table * T;
FILE *f;
Chunk chunk;
{
    if (table_debug)
	printf("table_WriteASCII(%s)\n", table_Name(T));

    WriteASCII(T, f, chunk, 0);
}

/* read subrectangle */

struct table * table__ReadASCII (T, f)
register struct table * T;
FILE *f;
{
    if (table_debug)
	printf("table_ReadASCII(%s, , %d, %d)\n", table_Name(T));

    return ReadASCII(T, f);
}

/* format cell contents for external display */

void table__FormatCell (T, cell, buff)
register struct table * T;
struct cell * cell;
char **buff;
{
    if (table_debug)
	printf("table_FormatCell(%s, , )\n", table_Name(T));

    WriteCell (T, NULL, cell, buff);
}

/* parse external cell contents */

void table__ParseCell(T, cell, buff)
register struct table * T;
struct cell * cell;
char *buff;
{
    char *cp = buff;

    ReadCell (T, NULL, buff, &cp, buff+1000, cell);
    T->cellChanged = ++(T->timeStamper);
    table_NotifyObservers(T, 0);
    table_SetModified(T);
}


/* create and copy a cell */


static  CreateCell (T, newcell, oldcell)
register struct table *T;
struct cell *newcell, *oldcell;
{
    if (oldcell) {
	newcell->format = oldcell->format;
	newcell->lock = oldcell->lock;
	newcell->precision = oldcell->precision;
	newcell->celltype = oldcell->celltype;
	switch (newcell->celltype) {
	    case table_TextCell:
		newcell->interior.TextCell.textstring = (char *) malloc (strlen (oldcell->interior.TextCell.textstring) + 1);
		if (newcell->interior.TextCell.textstring == NULL) {
		    newcell->celltype = table_EmptyCell;
		    break;
		}
		strcpy (newcell->interior.TextCell.textstring, oldcell->interior.TextCell.textstring);
		newcell->lastcalc = ++(T->timeStamper);
		break;
	    case table_ValCell:
		newcell->interior.ValCell.formula = (char *) malloc (strlen (oldcell->interior.ValCell.formula) + 1);
		if (newcell->interior.ValCell.formula == NULL) {
		    newcell->celltype = table_EmptyCell;
		    break;
		}
		strcpy (newcell->interior.ValCell.formula, oldcell->interior.ValCell.formula);
		newcell->interior.ValCell.value = oldcell->interior.ValCell.value;
		newcell->lastcalc = 0;
		break;
	    case table_ImbeddedObject:
		newcell->interior.ImbeddedObject.data = oldcell->interior.ImbeddedObject.data;
		newcell->interior.ImbeddedObject.views = oldcell->interior.ImbeddedObject.views;
		oldcell->celltype = table_EmptyCell;
		
		dataobject_AddObserver(newcell->interior.ImbeddedObject.data, T);
		
	    default:
		newcell->lastcalc = ++(T->timeStamper);
	}
    }
    else {
	newcell->format = GENERALFORMAT;
	newcell->lock = 0;
	newcell->precision = table_DefaultPrecision;
	newcell->lastcalc = ++(T->timeStamper);
	newcell->celltype = table_EmptyCell;
    }
}

void RemoveCellView(T,c,v)
struct table *T;
struct cell *c;
struct view *v;
{
    struct viewlist *vl=c->interior.ImbeddedObject.views;
    if(vl==NULL) return;
    if(vl->child==v) {
	c->interior.ImbeddedObject.views=vl->next;
	if(vl->next==NULL) {
	    CreateCell(T,c,NULL);
	}
	free(vl);
    } else {
	struct viewlist *last=vl;
	while(vl=vl->next) {
	    if(vl->child==v) {
		last->next=vl->next;
		free(vl);
		return;
	    } else last=vl;
	}
    }
}

void table__RemoveViewFromTable(T,v)
struct table *T;
struct view *v;
{
    long r,c;
    struct cell *cell;
    for (r = 0; r < table_NumberOfRows(T); r++) {
	for (c = 0; c < table_NumberOfColumns(T); c++) {
	    cell = table_GetCell(T, r, c);
	    if (cell->celltype == table_ImbeddedObject)
		RemoveCellView(T,cell,v);
	}
    }
    T->cellChanged = ++(T->timeStamper);
    table_NotifyObservers(T, 0);
    table_SetModified(T);
}

/* delete a cell */

DestroyCell(T, oldcell)
register struct table *T;
struct cell *oldcell;
{
    struct viewlist *vl;

    switch (oldcell->celltype) {
	case table_TextCell:
	    free(oldcell->interior.TextCell.textstring);
	    break;
	case table_ValCell:
	    free(oldcell->interior.ValCell.formula);
	    break;
	case table_ImbeddedObject:
	   
	    if (oldcell->interior.ImbeddedObject.data) {
		dataobject_Destroy(oldcell->interior.ImbeddedObject.data);
		oldcell->interior.ImbeddedObject.data = NULL;
	    }
	    while (vl = oldcell->interior.ImbeddedObject.views) {
		if (table_debug)
		    printf("**MISUSE** removing imbedded view ref by %x\n", vl);
		    view_UnlinkTree(vl->child);
		    view_Destroy(vl->child);
		/* end of misuse */
		oldcell->interior.ImbeddedObject.views = vl->next;
		    free((char *) vl);
	    }
	    if (table_debug)
		printf("imbedded remove complete\n");
	    
	    break;
    }
    oldcell->format = GENERALFORMAT;
    oldcell->lock = 0;
    oldcell->precision = table_DefaultPrecision;
    oldcell->celltype = table_EmptyCell;
    oldcell->lastcalc = ++(T->timeStamper);
}

void table__ChangeThickness (T, dim, i, thickness)
register struct table * T;
Dimension dim;
int     i, thickness;
{
    struct slice * slice;

    if (table_debug)
	printf("table_ChangeThickness(%s, %d, %d, %d)\n", table_Name(T), (int)dim, i, thickness);

    if (dim == ROWS) {
	slice = T->row;
	if (thickness <= 0)
	    thickness = TABLE_DEFAULT_ROW_THICKNESS;
    }
    else {
	slice = T->col;
	if (thickness < 10)
	    thickness = 10;
    }

    slice[i].thickness = thickness;

    table_StampEverything(T);
    table_NotifyObservers(T, 0);
    table_SetModified(T);
}

/* enlarge chunk so it doesn't include only part of a taped cell */

void table__FindBoundary (T, chunk)
register struct table * T;
Chunk chunk;
{
    int r, c;
    int ok;

    if (table_debug)
	printf("table_FindBoundary(%s, )\n", table_Name(T));

    for (; ;) {
	ok = 1;
	for (r = max (0, chunk->TopRow);
		chunk->LeftCol >= 0 && r <= chunk->BotRow; r++) {
	    while (table_IsJoinedToLeft(T, r, chunk->LeftCol) && chunk->LeftCol > 0) {
		chunk->LeftCol--;
		ok = 0;
	    }
	    while (table_IsJoinedToLeft(T, r, chunk->RightCol + 1)  && chunk->RightCol < table_NumberOfColumns(T) - 1) {
		chunk->RightCol++;
		ok = 0;
	    }
	}
	for (c = max (0, chunk->LeftCol);
		chunk->TopRow >= 0 && c <= chunk->RightCol; c++) {
	    while (table_IsJoinedAbove(T, chunk->TopRow, c) && chunk->TopRow > 0) {
		chunk->TopRow--;
		ok = 0;
	    }
	    while (table_IsJoinedAbove(T, chunk->BotRow + 1, c) && chunk->BotRow < table_NumberOfRows(T) - 1) {
		chunk->BotRow++;
		ok = 0;
	    }
	}
	if (ok) break;
    }
}

void table__SetInterior (T, chunk, color)
register struct table * T;
Chunk chunk;
Color color;
{
    int r, c;

    if (table_debug)
	printf("table_SetInterior(%s, , %d)\n", table_Name(T), (int)color);

    table_FindBoundary(T, chunk);
    T->cellChanged = table_StampEverything(T);

    for (r = chunk->TopRow + 1; r <= chunk->BotRow; r++) {
   	for (c = max(chunk->LeftCol, 0); c <= chunk->RightCol; c++) {
	    if (c > chunk->LeftCol && table_ColorToLeft(T, r, c) != color) {
		struct cell * cell = table_GetCell(T, r, c);
		if (cell->celltype != table_ValCell)
		    cell->lastcalc = T->cellChanged;
		else
		    cell->lastcalc = 0;
	    }
	    table_SetAboveColor(T, r, c, color);
	}
    }
    for (r = max(chunk->TopRow, 0); r <= chunk->BotRow; r++) {
   	for (c = chunk->LeftCol + 1; c <= chunk->RightCol; c++) {
	    if (c > chunk->LeftCol && table_ColorToLeft(T, r, c) != color) {
		struct cell * cell = table_GetCell(T, r, c);
		if (cell->celltype != table_ValCell)
		    cell->lastcalc = T->cellChanged;
		else
		    cell->lastcalc = 0;
	    }
	    table_SetLeftColor(T, r, c, color);
	}
    }
    table_NotifyObservers(T, 0);
    table_SetModified(T);
}


void table__SetBoundary (T, chunk, color)
register struct table * T;
Chunk chunk;
Color color;
{
    int r, c;

    if (table_debug)
	printf("table_SetBoundary(%s, , %d)\n", table_Name(T), (int)color);

    table_FindBoundary (T, chunk);

    T->edgeChanged = T->cellChanged = ++(T->timeStamper);
    for (r = max (chunk->TopRow, 0); r <= chunk->BotRow; r++) {
	table_SetLeftColor(T, r, chunk->LeftCol, color);
	table_SetLeftColor(T, r, chunk->RightCol+1, color);
    }
   for (c = max (chunk->LeftCol, 0); c <= chunk->RightCol; c++) {
	table_SetAboveColor(T, chunk->TopRow, c, color);
	table_SetAboveColor(T, chunk->BotRow+1, c, color);
    }
    T->edgeChanged = ++(T->timeStamper);
    table_NotifyObservers(T, 0);
    table_SetModified(T);
}

void rangeLimit (T, chunk)
register struct table * T;
Chunk chunk;
{
    if (chunk->TopRow < 1) chunk->TopRow = 1;
    if (chunk->BotRow > table_NumberOfRows(T)) chunk->BotRow = table_NumberOfRows(T);
    if (chunk->LeftCol < 1) chunk->LeftCol = 1;
    if (chunk->RightCol > table_NumberOfColumns(T)) chunk->RightCol = table_NumberOfColumns(T);
}

/* re-evaluate one cell */

static char *circ = "CIRC!";

void table__ReEval (T, r, c)
register struct table * T;
int     r, c;
{
    struct cell * cell = table_GetCell(T, r, c);

    if (cell->lastcalc >= T->cellChanged)
	return;
    if (cell->celltype != table_ValCell)
	return;
    cell->lastcalc = T->cellChanged;
    MakeBogus(&(cell->interior.ValCell.value), circ);
    eval (T, &(cell->interior.ValCell.value), r, c, cell->interior.ValCell.formula);
    cell->lastcalc = T->cellChanged;
    return;
}

void rcref (T, result, r, c, iftaped)
register struct table * T;
register extended_double *result;
int     r, c, iftaped;
{
    struct cell * cell;

    r--, c--;
    if (r >= 0 && r < table_NumberOfRows(T) && c >= 0 && c < table_NumberOfColumns(T)) {
	for (;;) {
	    if (table_IsJoinedToLeft(T, r, c))
		c--;
	    else if (table_IsJoinedAbove(T, r, c))
		r--;
	    else
		break;
	    if (!iftaped) {
		MakeBogus(result, "CELL!");
		return;
	    }
	}
	cell = table_GetCell(T, r, c);
	
	switch(cell->celltype) {
	    case table_ValCell:
		table_ReEval (T, r, c);
		if (IsBogus(&(cell->interior.ValCell.value)) && ExtractBogus(&(cell->interior.ValCell.value)) != circ) {
		    MakeBogus(result, "REF!");
		}
		else {
		    *result = cell->interior.ValCell.value;
		}
		break;
	    case table_EmptyCell:
		MakeStandard(result, (double)0.0);
		break;
	    case table_ImbeddedObject:
		{
		struct atom *rock1=atom_Intern("rock1");
		struct atom *string=atom_Intern("string");
		struct atom *value=atom_Intern("value");
		long rock;
		char *rockp;
		if(dataobject_Get(cell->interior.ImbeddedObject.data,value, &string,&rockp)) MakeStandard(result,atof(rockp));
		else
		    if( !dataobject_Get( cell->interior.ImbeddedObject.data, value, &rock1, &rock)) MakeBogus(result,"NOVAL!");
		    else MakeStandard(result,(double)rock);
		break;
		}
	    default:	
		MakeBogus(result, "NOVAL!");
	}
    }
    else {
	MakeBogus(result, "CELL!");
    }
}
#define inside(r,c,ch) ( (r)>=((ch)->TopRow) && (r)<=((ch)->BotRow) && (c)>=((ch)->LeftCol) && (c) <= ((ch)->RightCol) )

struct movetrstate {
    int myrow, mycol;
    Chunk left, moved;
};
/* 
static movetr (r, c, absr, absc, ms)
int  *r, *c, absr, absc;
struct movetrstate *ms;
{
    int     refr = (ms->myrow) * (!absr) + *r, refc = (ms->mycol) * (!absc) + *c;
    if (inside ((ms->myrow), (ms->mycol), (ms->moved)))
	if (inside (refr, refc, (ms->moved))) {
	    if (absr)
		(*r) += ms->moved->TopRow - ms->left->TopRow;
	    if (absc)
		(*c) += ms->moved->LeftCol - ms->left->LeftCol;
	}
	else {
	    if (!absr)
		(*r) -= (ms->moved->TopRow - ms->left->TopRow);
	    if (!absc)
		(*c) -= (ms->moved->LeftCol - ms->left->LeftCol);
	    if (inside ((ms->myrow) * (!absr) + *r, (ms->mycol) * (!absc) + *c, (ms->moved)))
		return 0;
	}
    else
	if (inside(refr, refc, (ms->left)))
	    (*r) += (ms->moved->TopRow - ms->left->TopRow),
	    (*c) += (ms->moved->LeftCol - ms->left->LeftCol);
	else
	    if (inside (refr, refc, (ms->moved)))
		return 0;
    return 1;
}
 */

void table__SetFormat (T, ch, chunk)
register struct table * T;
char ch;
Chunk chunk;
{
    int     r, c;
    struct cell *cell;

    if (table_debug)
	printf("table_SetFormat(%s, %c)\n", table_Name(T), ch);

    for (r = max (chunk->TopRow, 0); r <= chunk->BotRow; r++) {
	for (c = max (chunk->LeftCol, 0); c <= chunk->RightCol; c++) {
	    if (!table_IsJoinedToAnother(T, r, c)) {
		cell = table_GetCell(T, r, c);
		if (!(cell->lock)) {
		    cell->format = ch;
		}
	    }
	}
    }
    T->cellChanged = ++(T->timeStamper);
    table_NotifyObservers(T, 0);
    table_SetModified(T);
}

void table__SetPrecision (T, prec, chunk)
register struct table * T;
int prec;
Chunk chunk;
{
    int     r, c;
    struct cell *cell;

    if (table_debug)
	printf("table_SetPrecision(%s, %d)\n", table_Name(T), prec);

    for (r = max (chunk->TopRow, 0); r <= chunk->BotRow; r++) {
	for (c = max (chunk->LeftCol, 0); c <= chunk->RightCol; c++) {
	    if (!table_IsJoinedToAnother(T, r, c)) {
		cell = table_GetCell(T, r, c);
		if (!(cell->lock)) {
		    cell->precision = prec;
		}
	    }
	}
    }
    T->cellChanged = ++(T->timeStamper);
    table_NotifyObservers(T, 0);
    table_SetModified(T);
}

void table__Imbed (T, name, chunk)
register struct table * T;
char *name;
Chunk chunk;
{
    struct dataobject *newobject;
    int r, c;
    struct cell *cell;

    if (table_debug)
	printf("table_Imbed(%s, %s, )\n", table_Name(T), name);

    table_StampEverything(T);
    for (r = max (chunk->TopRow, 0); r <= chunk->BotRow; r++) {
	for (c = max (chunk->LeftCol, 0); c <= chunk->RightCol; c++) {
	    cell = table_GetCell(T, r, c);
	    if (!table_IsJoinedToAnother(T, r, c) && !(cell->lock)) {
		DestroyCell(T, cell);
		newobject = (struct dataobject *) class_NewObject(name);
		if (newobject) {
		    newobject->id = (long) newobject;
		    cell->celltype = table_ImbeddedObject;
		    cell->interior.ImbeddedObject.data = newobject;
		    cell->interior.ImbeddedObject.views = NULL;
		    cell->lastcalc = T->cellChanged;
		    
		    dataobject_AddObserver(newobject,T);
		}
	    }
	}
    }
    table_NotifyObservers(T, 0);
    table_SetModified(T);
}

void table__Lock (T, ch, chunk)
register struct table * T;
char ch;
Chunk chunk;
{
    int     r,  c;

    if (table_debug)
	printf("table_Lock(%s, %c, )\n", table_Name(T), ch);

    for (r = max (chunk->TopRow, 0); r <= chunk->BotRow; r++)
	for (c = max (chunk->LeftCol, 0); c <= chunk->RightCol; c++)
	    if (!table_IsJoinedToAnother(T, r, c))
		table_GetCell(T, r, c)->lock = ch;
    T->cellChanged = ++(T->timeStamper);
    table_NotifyObservers(T, 0);
    table_SetModified(T);
}

/* format date */

static char   *monthname[] = {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
};

int     daysinmonth[] = {
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

void table__FormatDate (T, fdate, buf, format)
register struct table * T;
double  fdate;
char   *buf;
char    format;
{
    int     date = (int) (fdate + 0.5);
    int     y, m, d;

    if (date <= 0 || date > 73050)
	strcpy (buf, "DATE!");
    date = date - 1 - 59 + 1461;
    if (date < 1461)
	date--;
    y = date / 1461;
    date -= y * 1461;
    y *= 4;
    d = date / 365;
    if (d >= 4) d = 3;
    date -= d * 365;
    y += d + 1900 - 4;
    for (m = 2; m != 1 && date >= daysinmonth[m]; ) {
	date -= (daysinmonth[m++]);
	if (m >= 12) {
	    m = 0;
	    y += 1;
	}
    }
    d = date + 1;
    if (format == DDMMMYYYYFORMAT)
	sprintf (buf, "%2d %3.3s %d", d, monthname[m], y);
    else if (format == MMMYYYYFORMAT)
	sprintf (buf, "%3.3s %d", monthname[m], y);
    else if (format == DDMMMFORMAT)
	sprintf (buf, "%2d %3.3s", d, monthname[m]);
}

/* construct an extended floating value */

MakeStandard(x, value)
extended_double *x;
double value;
{
    ExtendedType(x) = extended_STANDARD;
    StandardValue(x) = value;
}

/* construct a extended floating "bogus" value */

MakeBogus(x, message)
extended_double *x;
char *message;
{
    ExtendedType(x) = extended_BOGUS;
    ExtractBogus(x) = message;
}

/* check to see if modified */

long table__GetModified(self)
struct table *self;
{
    int r, c;
    struct cell *cell;
    long rc, cc;

    rc = super_GetModified(self);
    if (!self->inGetModified) {
	self->inGetModified = TRUE;
	for (r = 0; r < self->rows; r++) {
	    for (c = 0; c < self->cols; c++) {
		cell = table_GetCell(self, r, c);
		if (cell->celltype == table_ImbeddedObject && cell->interior.ImbeddedObject.data != NULL) {
		    cc = dataobject_GetModified(cell->interior.ImbeddedObject.data);
		    if (cc > rc) rc = cc;
		}
	    }
	}
	self->inGetModified = FALSE;
    }

    if (table_debug)
	printf("table_GetModified = %d\n", rc);

    return rc;
}
