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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/table/RCS/menu.c,v 1.14 1993/12/06 23:55:39 gk5g Exp $";
#endif


 

/* menu.c - menu operations for table */

#include <class.h>
FILE * popen ();

#include <im.ih>
#include <view.ih>
#include <dataobj.ih>
#include <bind.ih>
#include <table.ih>

#define AUXMODULE
#include <spread.eh>

extern struct view *spread_FindSubview();

void m_rename(V, ch)
register struct spread * V;
char ch;
{
    char buff[257];

    if (!k_AskUser (V, "New name?:  ", table_Name(MyTable(V)), buff, sizeof buff))
	return;
    if (*buff && strcmp(buff, "(null)") != 0) {
	struct table * S = table_SetName (MyTable(V), buff);
	if (S != MyTable(V)) {
	    if (!k_WantToDiscard (V))
		return;
	    /* MyTable(V) = S; */
	    V->lastTime = -1;
	}
	/* im_SetTitle (getIM(V), safetn); */
    }
}

void m_changedim(V)
register struct spread *V;
{
    char buff[257];
    int nrows, ncols;

    if (!k_AskUser (V, "Number of rows?:  ", "10", buff, sizeof buff) || (sscanf(buff, "%d", &nrows) != 1))
	return;
    if (!k_AskUser (V, "Number of cols?:  ", "5" /*table_Name(MyTable(V))*/, buff, sizeof buff) || (sscanf(buff, "%d", &ncols) != 1))
	return;
    if (nrows > 0 && ncols > 0) {
	struct slice *slice;
	int i = 0;
	int thickness = 92;
	table_ChangeSize(MyTable(V), nrows, ncols);
	slice = (MyTable(V))->col;
	thickness = 460/ncols;
	while (i < ncols) {
		slice[i].thickness = thickness;
		i++;
	}
	table_StampEverything(MyTable(V));
	table_NotifyObservers(MyTable(V), 0);
	table_SetModified(MyTable(V));
    }
}

void m_drawboxes(V, ch)
register struct spread * V;
char ch;
{
    int r, c;
    struct chunk chunk;

    MyTable(V)->edgeChanged = ++(MyTable(V)->timeStamper);
    MyTable(V)->cellChanged = ++(MyTable(V)->timeStamper);

    chunk.RightCol = 0;
    chunk.BotRow = 0;
    while (chunk.RightCol < MyTable(V)->cols) {
        chunk.BotRow = 0;
        while (chunk.BotRow < MyTable(V)->rows) {
            for (r = max (chunk.TopRow, 0); r <= chunk.BotRow; r++) {
                table_SetLeftColor(MyTable(V), r, chunk.LeftCol, BLACK);
                table_SetLeftColor(MyTable(V), r, chunk.RightCol+1, BLACK);
            }
            for (c = max (chunk.LeftCol, 0); c <= chunk.RightCol; c++) {
                table_SetAboveColor(MyTable(V), chunk.TopRow, c, BLACK);
                table_SetAboveColor(MyTable(V), chunk.BotRow+1, c, BLACK);
            }
            chunk.BotRow++;
        }
        chunk.RightCol++;
    }
    MyTable(V)->edgeChanged = ++(MyTable(V)->timeStamper);
    table_NotifyObservers(MyTable(V), 0);
    table_SetModified(MyTable(V));
}

void m_eraseboxes(V, ch)
register struct spread * V;
char ch;
{
    int r, c;
    struct chunk chunk;

    MyTable(V)->edgeChanged = ++(MyTable(V)->timeStamper);
    MyTable(V)->cellChanged = ++(MyTable(V)->timeStamper);

    chunk.RightCol = 0;
    chunk.BotRow = 0;
    while (chunk.RightCol < MyTable(V)->cols) {
        chunk.BotRow = 0;
        while (chunk.BotRow < MyTable(V)->rows) {
            for (r = max (chunk.TopRow, 0); r <= chunk.BotRow; r++) {
                table_SetLeftColor(MyTable(V), r, chunk.LeftCol, GHOST);
                table_SetLeftColor(MyTable(V), r, chunk.RightCol+1, GHOST);
            }
            for (c = max (chunk.LeftCol, 0); c <= chunk.RightCol; c++) {
                table_SetAboveColor(MyTable(V), chunk.TopRow, c, GHOST);
                table_SetAboveColor(MyTable(V), chunk.BotRow+1, c, GHOST);
            }
            chunk.BotRow++;
        }
        chunk.RightCol++;
    }
    MyTable(V)->edgeChanged = ++(MyTable(V)->timeStamper);
    table_NotifyObservers(MyTable(V), 0);
    table_SetModified(MyTable(V));
}

void m_drawalledges(V, ch)
register struct spread * V;
char ch;
{
    if (max(0, V->selection.TopRow) > V->selection.BotRow && max(0, V->selection.LeftCol) > V->selection.RightCol)
	k_TellUser(V, "Please select a region to draw edges");
    else {
        int r, c;
        struct chunk chunk;

        MyTable(V)->edgeChanged = ++(MyTable(V)->timeStamper);
        MyTable(V)->cellChanged = ++(MyTable(V)->timeStamper);

        chunk.LeftCol = V->selection.LeftCol;
        chunk.TopRow = V->selection.TopRow;
        chunk.RightCol = V->selection.LeftCol;
        chunk.BotRow = V->selection.TopRow;
/*
printf("draw(LeftCol=%d, RightCol=%d, TopRow=%d, BotRow=%d)\n",
V->selection.LeftCol,V->selection.RightCol,V->selection.TopRow,V->selection.BotRow);
*/
        while (chunk.RightCol <= V->selection.RightCol) {
            chunk.BotRow = V->selection.TopRow;
            while (chunk.BotRow <= V->selection.BotRow) {
                for (r = max (chunk.TopRow, 0); r <= chunk.BotRow; r++) {
                    table_SetLeftColor(MyTable(V), r, chunk.LeftCol, BLACK);
                    table_SetLeftColor(MyTable(V), r, chunk.RightCol+1, BLACK);
                }
                for (c = max (chunk.LeftCol, 0); c <= chunk.RightCol; c++) {
                    table_SetAboveColor(MyTable(V), chunk.TopRow, c, BLACK);
                    table_SetAboveColor(MyTable(V), chunk.BotRow+1, c, BLACK);
                }
                chunk.BotRow++;
            }
            chunk.RightCol++;
        }
        MyTable(V)->edgeChanged = ++(MyTable(V)->timeStamper);
        table_NotifyObservers(MyTable(V), 0);
        table_SetModified(MyTable(V));
    }
}

void m_erasealledges(V, ch)
register struct spread * V;
char ch;
{
    if (max(0, V->selection.TopRow) > V->selection.BotRow && max(0, V->selection.LeftCol) > V->selection.RightCol)
	k_TellUser(V, "Please select a region to draw edges");
    else {
        int r, c;
        struct chunk chunk;

        MyTable(V)->edgeChanged = ++(MyTable(V)->timeStamper);
        MyTable(V)->cellChanged = ++(MyTable(V)->timeStamper);

        chunk.LeftCol = V->selection.LeftCol;
        chunk.TopRow = V->selection.TopRow;
        chunk.RightCol = V->selection.LeftCol;
        chunk.BotRow = V->selection.TopRow;
/*
printf("erase(LeftCol=%d, RightCol=%d, TopRow=%d, BotRow=%d)\n",
V->selection.LeftCol,V->selection.RightCol,V->selection.TopRow,V->selection.BotRow);
*/
        while (chunk.RightCol <= V->selection.RightCol) {
            chunk.BotRow = V->selection.TopRow;
            while (chunk.BotRow <= V->selection.BotRow) {
                for (r = max (chunk.TopRow, 0); r <= chunk.BotRow; r++) {
                    table_SetLeftColor(MyTable(V), r, chunk.LeftCol, GHOST);
                    table_SetLeftColor(MyTable(V), r, chunk.RightCol+1, GHOST);
                }
                for (c = max (chunk.LeftCol, 0); c <= chunk.RightCol; c++) {
                    table_SetAboveColor(MyTable(V), chunk.TopRow, c, GHOST);
                    table_SetAboveColor(MyTable(V), chunk.BotRow+1, c, GHOST);
                }
                chunk.BotRow++;
            }
            chunk.RightCol++;
        }
        MyTable(V)->edgeChanged = ++(MyTable(V)->timeStamper);
        table_NotifyObservers(MyTable(V), 0);
        table_SetModified(MyTable(V));
    }
}

static char *newext(filename, extension)	/* returns "mumble.x" */
char *filename;			/* "mumble.y" or something */
char *extension;		/* ".x" */
{
    char *cp, *suffixp;
    static char newname[257];
    char *safefn;

    safefn = (filename == NULL ? "" : filename);
    strncpy(newname, safefn, (sizeof newname) - 1);
    newname[(sizeof newname) - 1] = '\0';
    for (cp = newname, suffixp = NULL; *cp; cp++)
	switch (*cp) {
	case '.':
	    suffixp = cp;
	    break;
	case '/':
	    suffixp = NULL;
	    break;
	}
    if (suffixp)
	cp = suffixp;
    strncpy(cp, extension, (sizeof newname) - 1 - (cp - newname));
    return newname;
}

void m_writeTroff(V, ch)
register struct spread * V;
char ch;
{
    char buf[300];
    char fname[300];
    FILE * f;

    k_AskUser(V, "Enter file name: ",  newext (table_Name(MyTable(V)), ".trf"), fname, 300);
    f = fopen (fname, "w");
    if (f) {
	WriteTroff(V, f, "troff", "PostScript", 1);
	fclose (f);
	sprintf(buf, "Wrote %s", fname);
	k_TellUser(V, buf);
    }
}

void m_write(V, ch)
register struct spread * V;
char ch;
{
    char buf[300];
    char fname[300];
    FILE * f;
    struct chunk chunk;

    k_AskUser(V, "Enter file name: ", newext (table_Name(MyTable(V)), ".table"), fname, 300);
    f = fopen (fname, "w");
    if (f) {
	chunk.LeftCol = -1;
	chunk.RightCol = table_NumberOfColumns(MyTable(V)) - 1;
	chunk.TopRow = -1;
	chunk.BotRow = table_NumberOfRows(MyTable(V)) - 1;
	table_WriteASCII (MyTable(V), f, &chunk);
	fclose (f);
	sprintf(buf, "Wrote %s", fname);
	k_TellUser(V, buf);
    }
}

void m_read(V, ch)
register struct spread * V;
char ch;
{
    char buf[300];
    char fname[300];
    FILE * f;

    k_AskUser(V, "Enter file name: ", newext (table_Name(MyTable(V)), ".table"), fname, 300);
    f = fopen (fname, "r");
    if (f) {
	table_ReadASCII (MyTable(V), f);
	fclose (f);
	sprintf(buf, "Read %s", fname);
	k_TellUser(V, buf);
    }
}

void m_cut(V, ch)
register struct spread * V;
char ch;
{
    if (V->selection.TopRow > V->selection.BotRow || V->selection.LeftCol > V->selection.RightCol)
	k_TellUser (V, "Please select region to cut");
    else {
	FILE * writeFile = im_ToCutBuffer(getIM(V));
	
	struct table *T;
	
	/* write to cutbuffer */

	table_WriteASCII(MyTable(V), writeFile, &(V->selection));
	im_CloseToCutBuffer(getIM(V), writeFile);

	T=table_ExtractData(MyTable(V), &(V->selection));
	table_AddObserver(T,V);
	table_Destroy(T);

	/* delete entire rows */

	if (V->selection.LeftCol < 0 && V->selection.RightCol == table_NumberOfColumns(MyTable(V)) - 1) {
	    struct chunk remnant;
	    int n0 = V->selection.TopRow;
	    int n1 = V->selection.BotRow + 1;
	    int n2 = table_NumberOfRows(MyTable(V));
	    if (n0 < 0) n0 = 0;
	    if (n1 < 0) n1 = 0;
	    remnant.LeftCol = -1;
	    remnant.RightCol = table_NumberOfColumns(MyTable(V)) - 1;
	    if (n0 < n1 && n1 < n2) {
		struct table * Q;
		remnant.TopRow = n1;
		remnant.BotRow = n2 - 1;
		Q = table_ExtractData (MyTable(V), &remnant);
		
		table_AddObserver(Q,V);
		
		remnant.TopRow -= (n1 - n0);
		remnant.BotRow -= (n1 - n0);
		table_InsertData (MyTable(V), Q, &remnant);
		table_Destroy(Q);
	    }
	    table_ChangeSize(MyTable(V), table_NumberOfRows(MyTable(V)) - (n1 - n0), table_NumberOfColumns(MyTable(V)));
	    if (table_NumberOfRows(MyTable(V)) <= 0)
		table_ChangeSize(MyTable(V), n0 = 10, table_NumberOfColumns(MyTable(V)));
	    remnant.TopRow = V->selection.TopRow;
	    remnant.BotRow = n0 - 1;
	    SetCurrentCell (V, &remnant);
	}

	/* delete entire columns */

	if (V->selection.TopRow < 0 && V->selection.BotRow == table_NumberOfRows(MyTable(V)) - 1) {
	    struct chunk remnant;
	    int n0 = V->selection.LeftCol;
	    int n1 = V->selection.RightCol + 1;
	    int n2 = table_NumberOfColumns(MyTable(V));
	    if (n0 < 0) n0 = 0;
	    if (n1 < 0) n1 = 0;
	    remnant.TopRow = -1;
	    remnant.BotRow = table_NumberOfRows(MyTable(V)) - 1;
	    if (n0 < n1 && n1 < n2) {
		struct table * Q;
		remnant.LeftCol = n1;
		remnant.RightCol = n2 - 1;
		Q = table_ExtractData (MyTable(V), &remnant);
		
		table_AddObserver(Q,V);
		
		remnant.LeftCol -= (n1 - n0);
		remnant.RightCol -= (n1 - n0);
		table_InsertData (MyTable(V), Q, &remnant);
		table_Destroy(Q);
	    }
	    table_ChangeSize(MyTable(V), table_NumberOfRows(MyTable(V)), table_NumberOfColumns(MyTable(V)) - (n1 - n0));
	    if (table_NumberOfColumns(MyTable(V)) <= 0)
		table_ChangeSize(MyTable(V), table_NumberOfRows(MyTable(V)), n0 = 5);
	    remnant.LeftCol = V->selection.LeftCol;
	    remnant.RightCol = n0 - 1;
	    SetCurrentCell (V, &remnant);
	}
    }
    view_WantNewSize(getView(V).parent, &getView(V));
}

void m_copy(V, ch)
register struct spread * V;
char ch;
{
    if (V->selection.TopRow > V->selection.BotRow || V->selection.LeftCol > V->selection.RightCol)
	k_TellUser (V, "Please select region to copy");
    else {
	FILE * writeFile = im_ToCutBuffer(getIM(V));

	table_WriteASCII(MyTable(V), writeFile, &(V->selection));
	im_CloseToCutBuffer(getIM(V), writeFile);
    }
}

void m_paste(V, ch)
register struct spread * V;
char ch;
{
    if ((V->selection.TopRow >= 0 || V->selection.BotRow < table_NumberOfRows(MyTable(V))-1) && (V->selection.LeftCol >= 0 || V->selection.RightCol < table_NumberOfColumns(MyTable(V))-1) && (V->selection.TopRow > V->selection.BotRow || V->selection.LeftCol > V->selection.RightCol))
	k_TellUser (V, "Please select region to paste into");
    else {
	struct table * S;
	FILE * readFile = im_FromCutBuffer(getIM(V));

	/* read from cutbuffer */

	S = table_New();
	table_ReadASCII(S, readFile);
	im_CloseFromCutBuffer(getIM(V), readFile);

	/* insert entire rows */

	if (V->selection.LeftCol < 0 && V->selection.RightCol == table_NumberOfColumns(MyTable(V)) - 1) {
	    struct chunk newselection;

	    AddRows(V, V->selection.TopRow, table_NumberOfRows(S));
	    newselection.LeftCol = -1;
	    newselection.RightCol = table_NumberOfColumns(MyTable(V)) - 1;
	    newselection.TopRow = V->selection.TopRow;
	    newselection.BotRow = V->selection.TopRow + table_NumberOfRows(S) - 1;
	    SetCurrentCell (V, &newselection);
	    }

	/* insert entire columns */

	if (V->selection.TopRow < 0 && V->selection.BotRow == table_NumberOfRows(MyTable(V)) - 1) {
	    struct chunk newselection;

	    AddCols(V, V->selection.LeftCol, table_NumberOfColumns(S));
	    newselection.TopRow = -1;
	    newselection.BotRow = table_NumberOfRows(MyTable(V)) - 1;
	    newselection.LeftCol = V->selection.LeftCol;
	    newselection.RightCol = V->selection.LeftCol + table_NumberOfColumns(S) - 1;
	    SetCurrentCell (V, &newselection);
	}

	/* paste in data read */

	table_InsertData (MyTable(V), S, &(V->selection));
	table_Destroy (S); 
    }
    view_WantNewSize(getView(V).parent, &getView(V));
}

void m_combine(V, ch)
register struct spread * V;
char ch;
{
    struct chunk chunk;

    if (max(0, V->selection.TopRow) > V->selection.BotRow || max(0, V->selection.LeftCol) > V->selection.RightCol)
	k_TellUser(V, "Please select a region to combine");
    else {
	table_SetInterior (MyTable(V), &(V->selection), JOINED);
	CopyChunk (&chunk, &(V->selection));
	SetCurrentCell (V, &chunk);
    }
}

void m_separate(V, ch)
register struct spread * V;
char ch;
{
    struct chunk chunk;

    if (max(0, V->selection.TopRow) > V->selection.BotRow || max(0, V->selection.LeftCol) > V->selection.RightCol)
	k_TellUser(V, "Please select a region to separate");
    else {
	table_SetInterior (MyTable(V), &(V->selection), GHOST);
	CopyChunk (&chunk, &(V->selection));
	SetCurrentCell (V, &chunk);
    }
}

void m_drawedges(V, ch)
register struct spread * V;
char ch;
{
    if (max(0, V->selection.TopRow) > V->selection.BotRow && max(0, V->selection.LeftCol) > V->selection.RightCol)
	k_TellUser(V, "Please select a region to draw edges");
    else
	table_SetBoundary (MyTable(V), &(V->selection), BLACK);
}

void m_eraseedges(V, ch)
register struct spread * V;
char ch;
{
    if (max(0, V->selection.TopRow) > V->selection.BotRow && max(0, V->selection.LeftCol) > V->selection.RightCol)
	k_TellUser(V, "Please select a region to erase edges");
    else
	table_SetBoundary (MyTable(V), &(V->selection), GHOST);
}

/* process formatting menu hit */

void m_format (V, ch)
register struct spread * V;
char    ch;
{
    table_SetFormat (MyTable(V), ch, &(V->selection));
}

/* process precision request */

void m_precision (V, ch)
register struct spread * V;
char    ch;
{
    char parambuff[100];
    int param;
    
    if (!k_AskUser(V, "Enter precision: ", "", parambuff, sizeof parambuff))
	return;

    if (sscanf(parambuff, "%d", &param) == 1)
	table_SetPrecision (MyTable(V), (param >= 0 ? param : 0), &(V->selection));
}

static objecttest(V, name, desiredname)
register struct spread *V;
char *name,*desiredname;
{
    if(class_Load(name) == NULL){
        char foo[640];
        sprintf(foo,"Can't load %s",name);
         k_TellUser(V, foo);
        return(FALSE);
    }
    if(! class_IsTypeByName(name, desiredname)){
        char foo[640];
        sprintf(foo,"%s is not a %s",name,desiredname);
         k_TellUser(V, foo);
        return(FALSE);
    }
    return(TRUE);
}

/* process request for imbedded object */

void m_imbed (V, ch)
register struct spread * V;
char    ch;
{
    char parambuff[100];
    struct cell * hitcell;
    struct view * child;
    
    parambuff[0] = '\0';
    if (!k_AskUser(V, "Data object to enter here (text): ", "", parambuff, sizeof parambuff))
	return;
    if (!*parambuff)
	strcpy(parambuff, "text");
    if (objecttest(V, parambuff, "dataobject") == FALSE)
	return;

    table_Imbed(MyTable(V), parambuff, &(V->selection));

    if (V->anchor.TopRow >= 0 && V->anchor.LeftCol >= 0) {
	hitcell = table_GetCell(MyTable(V), V->anchor.TopRow, V->anchor.LeftCol);
	if (hitcell->celltype == table_ImbeddedObject) {
	    if ((child = spread_FindSubview(V, hitcell)))
		view_WantInputFocus(child, child);
	}
    }
}

/* compute row heights automatically */

void m_resetheights (V, ch)
register struct spread * V;
char    ch;
{
    int r;
    struct table *T = MyTable(V);
    
    for (r = max (V->selection.TopRow, 0); r <= V->selection.BotRow; r++) {
	table_ChangeThickness(T, ROWS, r, 0);
    }
}

/* lock or unlock cells */

void m_lock (V, ch)
register struct spread * V;
char    ch;
{
    table_Lock (MyTable(V), ch, &(V->selection));
}

static struct bind_Description menutable[] = {

    {"table-cut", "\027", 0, "Cut~10", 0, 0, m_cut, "Save and erase cells"},
    {"table-copy", "\033w", 0, "Copy~11", 0, 0, m_copy, "Copy cells to cutbuffer"},
    {"table-paste", "\031", 0, "Paste~12", 0, 0, m_paste, "Copy cutbuffer to cells"},
    {"table-write", "/fw", 0, "Write table~80", 0, 0, m_write, "Write table file"},
    {"table-read", "/fr", 0, "Read table~81", 0, 0, m_read, "Read table file"},
    {"table-writeTroff", NULL, 0, "Write Troff~82", 0, 0, m_writeTroff, "Write .trf file"},
    {"table-rename", NULL, 0, "Rename~84", 0, 0, m_rename, "Rename table"},
    {"table-changedim", NULL, 0, "Change Rows & Cols~85", 0, 0, m_changedim, "Fix number of rows and cols in table"},
    {"table-drawboxes", NULL, 0, "Draw All boxes~86", 0, 0, m_drawboxes, "Draw all boxes in table"},
    {"table-eraseboxes", NULL, 0, "Erase All boxes~87", 0, 0, m_eraseboxes, "Erase all boxes in table"},
    {"table-combine", "/rc", 0, "Cells~1,Combine~10", 0, 0, m_combine, "Combine several cells into one",NULL},
    {"table-separate", "/rs", 0, "Cells~1,Separate~11", 0, 0, m_separate, "Separate combined cells"},
    {"table-drawedges", "/rbd", 0, "Cells~1,Draw box~20", 0, 0, m_drawedges, "Draw box around cells"},
    {"table-eraseedges", "/rbe", 0, "Cells~1,Erase box~21", 0, 0, m_eraseedges, "Erase box around cells"},
    {"table-drawalledges", "/rad", 0, "Cells~1,Draw boxes~22", 0, 0, m_drawalledges, "Draw all boxes around cells"},
    {"table-erasealledges", "/rae", 0, "Cells~1,Erase boxes~23", 0, 0, m_erasealledges, "Erase all boxes around cells"},
    {"table-lock", "/rl", TRUE, "Cells~1,Lock~30", TRUE, 0, m_lock, "Protect cells against modification"},
    {"table-unlock", "/ru", FALSE, "Cells~1,Unlock~31", FALSE, 0, m_lock, "Allow cells to be modified"},
    {"table-imbed", "\033\t", 0, "Cells~1,Imbed~40", 0, 0, m_imbed, "Place BE2 object in cell"},
    {"table-reset-height", 0, 0, "Cells~1,Reset Heights~41", 0, 0, m_resetheights, "Comput row heights automatically"},

    {"table-general-format", "/rfg", GENERALFORMAT, "Number Format~2,general~10", GENERALFORMAT, 0, m_format, "general number format"},
    {"table-currency-format", "/rfc", CURRENCYFORMAT, "Number Format~2,Dollar~11", CURRENCYFORMAT, 0, m_format, "Dollar sign before number"},
    {"table-percent-format", "/rfp", PERCENTFORMAT, "Number Format~2,Percent~12", PERCENTFORMAT, 0, m_format, "Multiply by 100 and display %"},
    {"table-exponential-format", "/rfe", EXPFORMAT, "Number Format~2,Exp~13", EXPFORMAT, 0, m_format, "Exponential format (not implemented)"},
    {"table-fixed-format", "/rff", FIXEDFORMAT, "Number Format~2,Fixed~14", FIXEDFORMAT, 0, m_format, "Always display decimal places"},
    {"table-hbar-format", "/rfh", HORIZONTALBARFORMAT, "Number Format~2,H-Bar~15", HORIZONTALBARFORMAT, 0, m_format, "Display as horizontal bar"},
    {"table-vbar-format", "/rfv", VERTICALBARFORMAT, "Number Format~2,V-Bar~16", VERTICALBARFORMAT, 0, m_format, "Display as vertical bar"},
    {"table-precision", "/rp", 0, "Number Format~2,Precision~20", 0, 0, m_precision, "Set number of decimal places"},

    {"table-day-month-year", "/rfda", DDMMMYYYYFORMAT, "Date Format~3,19 Jun 1970~10", DDMMMYYYYFORMAT, 0, m_format, "Display day,  month, and year"},
    {"table-month-year", "/rfdb", MMMYYYYFORMAT, "Date Format~3,Jun 1970~11", MMMYYYYFORMAT, 0, m_format, "Display month and year"},
    {"table-day-month", "/rfdc", DDMMMFORMAT, "Date Format~3,19 Jun~12", DDMMMFORMAT, 0, m_format, "Display day and month"},

    {NULL, NULL, 0, NULL, 0, NULL, NULL}
};

DefineMenus (mainmenus, mainmap, classinfo)
struct menulist *mainmenus;
struct keymap * mainmap;
struct spread_classinfo *classinfo;
{
    bind_BindList(menutable, mainmap, mainmenus, classinfo);
}
