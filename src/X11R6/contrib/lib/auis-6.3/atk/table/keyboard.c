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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/table/RCS/keyboard.c,v 1.14 1993/12/16 18:10:48 rr2b Exp $";
#endif


 

/* keyboard.c - keyboard input for table */

#include <class.h>

#include <bind.ih>
#include <proctbl.ih>
#include <keymap.ih>
#include <view.ih>
#include <message.ih>
#include <table.ih>

#define AUXMODULE
#include <spread.eh>

/* Cancel old input and set new message buffer state */

static boolean debug=FALSE;

void k_SetMessageState (V, newstate)
register struct spread * V;
int newstate;
{
    if (V->bufferstatus != BUFFEREMPTY) {
	if (debug)
	    printf("Cancelled input request, status %d, new %d\n", V->bufferstatus, newstate);
	message_CancelQuestion (&getView(V));
    }
    if (V->bufferstatus != newstate) {
	V->bufferstatus = newstate;
    }
}

/* message to user */

void k_TellUser (V, s)
register struct spread * V;
char   *s;
{
    k_SetMessageState (V, BUFFERHASMESSAGE);
    message_DisplayString (&getView(V), 0, s);
}

/* ask for and read keyboard input */

k_AskUser (V, prompt, def, buff, n)
register struct spread * V;
char prompt[];
char def[];
char buff[];
int n;
{
    int notOK;

    k_SetMessageState (V, BUFFERHASPARAM);
    notOK = message_AskForString(&getView(V), 0, prompt, def, buff, n);
    V->bufferstatus = BUFFEREMPTY;
    if (debug)
	printf("k_Askuser got status %d, message %s\n", notOK, buff);
    if (notOK)
	k_TellUser (V, "Cancelled!");
    return !notOK;
}

/* Are you sure? */

k_AreYouSure (V, why)
register struct spread * V;
char *why;
{
    char buff[10];

    if (!k_AskUser (V, why, "", buff, sizeof buff))
	return 0;
    return (buff[0] == 'y' || buff[0] == 'Y');
}

/* Verify that user wants to discard changes */

k_WantToDiscard (V)
register struct spread * V;
{
    if (table_WriteTimestamp(MyTable(V)) < table_CellsTimestamp(MyTable(V))
     || table_WriteTimestamp(MyTable(V)) < table_EdgesTimestamp(MyTable(V)))
	return k_AreYouSure(V, "Table modified; exit anyway? [no]:  ");
    return 1;
}

#ifdef NOTUSED
static void k_exit(V, ch)
register struct spread * V;
char ch;
{
    if (k_WantToDiscard (V))
	exit (0);
}
#endif /* NOTUSED */
/* add rows to table */

AddRows(V, after, count)
register struct spread * V;
int after, count;
{
    if (after < 0) after = 0;
    table_ChangeSize (MyTable(V), table_NumberOfRows(MyTable(V)) + count, table_NumberOfColumns(MyTable(V)));
    if (0 < count && after + count < table_NumberOfRows(MyTable(V))) {
	struct table * Q;
	struct chunk remnant;

	remnant.LeftCol = -1;
	remnant.RightCol = table_NumberOfColumns(MyTable(V)) - 1;
	remnant.TopRow = after;
	remnant.BotRow = table_NumberOfRows(MyTable(V)) - count - 1;
	Q = table_ExtractData (MyTable(V), &remnant);
	
	remnant.TopRow += count;
	remnant.BotRow += count;
	table_InsertData (MyTable(V), Q, &remnant);
	table_Destroy(Q);
    }
    view_WantNewSize(getView(V).parent, &getView(V));
}

/* add columns to table */

AddCols(V, after, count)
register struct spread * V;
int after, count;
{
    if (after < 0) after = 0;
    table_ChangeSize (MyTable(V), table_NumberOfRows(MyTable(V)), table_NumberOfColumns(MyTable(V)) + count);
    if (0 < count && after + count < table_NumberOfColumns(MyTable(V))) {
	struct table * Q;
	struct chunk remnant;

	remnant.TopRow = -1;
	remnant.BotRow = table_NumberOfRows(MyTable(V)) - 1;
	remnant.LeftCol = after;
	remnant.RightCol = table_NumberOfColumns(MyTable(V)) - count - 1;
	Q = table_ExtractData (MyTable(V), &remnant);
	
	remnant.LeftCol += count;
	remnant.RightCol += count;
	table_InsertData (MyTable(V), Q, &remnant);
	table_Destroy(Q);
    }
    view_WantNewSize(getView(V).parent, &getView(V));
}

/* verify there is a selection to enter data into */

static int k_CheckSelection (V)
register struct spread * V;
{
    struct chunk newselection;

    if (V->selection.BotRow < 0 || V->selection.RightCol < 0) {
	k_TellUser (V, "Please select a cell");
	return 0;
    }

    if (V->selection.BotRow < V->selection.TopRow) {
	AddRows(V, V->selection.TopRow, 1);
	CopyChunk(&newselection, &(V->selection));
	newselection.BotRow = newselection.TopRow;
	SetCurrentCell (V, &newselection);
    }

    if (V->selection.RightCol < V->selection.LeftCol) {
	AddCols(V, V->selection.LeftCol, 1);
	CopyChunk(&newselection, &(V->selection));
	newselection.RightCol = newselection.LeftCol;
	SetCurrentCell (V, &newselection);
    }

    return 1;
}

/* Read new formula for cell */

static k_ReadFormula (V, startstring)
register struct spread * V;
char *startstring;
{
    char   keybuff[1000];
    struct cell * cell;
    int r, c;
    int notOK;

    if (k_CheckSelection(V) == 0)
	return 0;

    for (r = max(0, V->selection.TopRow); r <= V->selection.BotRow; r++) {
	for (c = max(0, V->selection.LeftCol); c <= V->selection.RightCol; c++) {
	    if (!table_IsJoinedToAnother(MyTable(V), r, c)) {
		if (table_GetCell (MyTable(V), r, c)->lock) {
		    k_TellUser (V, "A cell is locked.");
		    return 0;
		}
	    }
	}
    }

    k_SetMessageState (V, BUFFERHASINPUT);
    notOK = message_AskForString (&getView(V), 0, "", startstring, keybuff, sizeof keybuff);
    if (debug)
	printf("k_ReadFormula got status %d, string %s\n", notOK, keybuff);
    V->bufferstatus = BUFFEREMPTY;
    if (notOK) {
	k_TellUser(V, "Input canceled!");
	return 0;
    }

    for (r = max(0, V->selection.TopRow); r <= V->selection.BotRow; r++) {
	for (c = max(0, V->selection.LeftCol); c <= V->selection.RightCol; c++) {
	    if (!table_IsJoinedToAnother(MyTable(V), r, c)) {
		cell = table_GetCell (MyTable(V), r, c);
		table_ParseCell (MyTable(V), cell, keybuff);
	    }
	}
    }
    TellFormula (V);
    return 1;
}

static void k_enterchar(V, ch)
register struct spread * V;
char ch;
{
    char   *startstring=NULL;
    register char *cp=NULL;

    if (ch != '$' && ch != '{') {
	GetFormula (V, &(V->anchor), &startstring);
	if(startstring==NULL) goto outofmemory;
	cp=startstring;
	if (*cp == '{') {
	    while (*cp && *cp++ != '}') ;
	}
	else if (*cp == '$')
	    cp++;
    }
    if(cp==NULL) {
	startstring=(char *)malloc(1000);
	if(startstring==NULL) goto outofmemory;
	cp=startstring;
    }
    *cp++ = ch;
    *cp++ = '\0';
    k_ReadFormula (V, startstring);
    free(startstring);
    return;
    outofmemory:
      message_DisplayString(V, 0, "Out of memory!");
    return;
      
}

static void k_backspace(V, ch)
register struct spread * V;
char ch;
{
    char *keybuff=NULL;

    GetFormula (V, &(V->selection), &keybuff);
    k_ReadFormula (V, keybuff);
    free(keybuff);
}

static void k_tab(V, ch)
register struct spread * V;
char ch;
{
    struct chunk newselection;

    k_CheckSelection(V);

    CopyChunk(&newselection, &(V->selection));
    newselection.LeftCol = ++newselection.RightCol;
    if (newselection.RightCol >= table_NumberOfColumns(MyTable(V)))
	newselection.RightCol = table_NumberOfColumns(MyTable(V)) - 1;
    SetCurrentCell (V, &newselection);
}

static void k_newline(V, ch)
register struct spread * V;
char ch;
{
    struct chunk newselection;

    k_CheckSelection(V);

    CopyChunk(&newselection, &(V->selection));
    newselection.TopRow = ++newselection.BotRow;
    if (newselection.BotRow >= table_NumberOfRows(MyTable(V)))
	newselection.BotRow = table_NumberOfRows(MyTable(V)) - 1;
    newselection.LeftCol = newselection.RightCol = 0;
    SetCurrentCell (V, &newselection);
}

static void k_killbuff (V, ch)
register struct spread * V;
char ch;
{
    k_SetMessageState (V, BUFFEREMPTY);
}

static void k_rightarrow (V, ch)
register struct spread * V;
char ch;
{
    struct chunk chunk;

    k_killbuff (V, ch);
    CopyChunk(&chunk, &(V->anchor));
    if (chunk.RightCol < table_NumberOfColumns(MyTable(V)) - 1) {
	if (chunk.TopRow < 0)
	    chunk.TopRow = 0;
	chunk.LeftCol = ++chunk.RightCol;
	chunk.BotRow = chunk.TopRow;
    }
    SetCurrentCell (V, &chunk);
}

static void k_leftarrow (V, ch)
register struct spread * V;
char ch;
{
    struct chunk chunk;

    k_killbuff (V, ch);
    CopyChunk(&chunk, &(V->anchor));
    if (chunk.TopRow < 0)
	chunk.TopRow = 0;
    if (--chunk.LeftCol < 0)
	chunk.LeftCol = 0;
    chunk.RightCol = chunk.LeftCol;
    chunk.BotRow = chunk.TopRow;
    SetCurrentCell (V, &chunk);
}

static void k_downarrow (V, ch)
register struct spread * V;
char ch;
{
    struct chunk chunk;

    k_killbuff (V, ch);
    CopyChunk(&chunk, &(V->anchor));
    if (chunk.BotRow < table_NumberOfRows(MyTable(V)) - 1) {
	if (chunk.LeftCol < 0)
	    chunk.LeftCol = 0;
	chunk.TopRow = ++chunk.BotRow;
	chunk.RightCol = chunk.LeftCol;
    }
    SetCurrentCell (V, &chunk);
}

static void k_uparrow (V, ch)
register struct spread * V;
char ch;
{
    struct chunk chunk;

    k_killbuff (V, ch);
    CopyChunk(&chunk, &(V->anchor));
    if (--chunk.TopRow < 0)
	chunk.TopRow = 0;
    if (chunk.LeftCol < 0)
	chunk.LeftCol = 0;
    chunk.BotRow = chunk.TopRow;
    chunk.RightCol = chunk.LeftCol;
    SetCurrentCell (V, &chunk);
}

static void k_home (V, ch)
register struct spread * V;
char ch;
{
    struct chunk chunk;

    k_killbuff (V, ch);
    CopyChunk (&chunk, &(V->anchor));
    chunk.LeftCol = chunk.RightCol = 0;
    if (chunk.BotRow < 0)
	chunk.BotRow = chunk.TopRow = 0;
    SetCurrentCell (V, &chunk);
}

static void k_endline (V, ch)
register struct spread * V;
char ch;
{
    struct chunk chunk;

    k_killbuff (V, ch);
    CopyChunk (&chunk, &(V->anchor));
    chunk.LeftCol = chunk.RightCol = table_NumberOfColumns(MyTable(V)) - 1;
    if (chunk.BotRow < 0)
	chunk.BotRow = chunk.TopRow = 0;
	SetCurrentCell (V, &chunk);
}

static void k_top (V, ch)
register struct spread * V;
char ch;
{
    struct chunk chunk;

    k_killbuff (V, ch);
    CopyChunk (&chunk, &(V->anchor));
    chunk.TopRow = chunk.BotRow = 0;
    chunk.LeftCol = chunk.RightCol = 0;
    SetCurrentCell (V, &chunk);
}

static void k_bottom (V, ch)
register struct spread * V;
char ch;
{
    struct chunk chunk;

    k_killbuff (V, ch);
    CopyChunk (&chunk, &(V->anchor));
    chunk.TopRow = chunk.BotRow = table_NumberOfRows(MyTable(V)) - 1;
    chunk.LeftCol = chunk.RightCol = table_NumberOfColumns(MyTable(V)) - 1;
    SetCurrentCell (V, &chunk);
}

/* toggle debug */

static void k_debug (V, ch)
register struct spread * V;
char ch;
{

/*
    the rather peculiar if statement below is designed to work properly
    in either the case that table and spread are loaded separately or
    loaded together (ie two or one copies of 'debug'
*/
    if (debug) {
	table_ToggleDebug(MyTable(V));
	debug = 0;
    } else {
	table_ToggleDebug(MyTable(V));
	debug = 1;
    }
}

static struct bind_Description keytable[] = {

    {"table-begin-row", "\001", 0, NULL, 0, 0, k_home, "Beginning of row"},
    {"table-left", "\002", 0, NULL, 0, 0, k_leftarrow, "Left one column"},
    {"table-end-row", "\005", 0, NULL, 0, 0, k_endline, "End of row"},
    {"table-right", "\006", 0, NULL, 0, 0, k_rightarrow, "Right one column"},
    {"table-down", "\016", 0, NULL, 0, 0, k_downarrow, "Down one row"},
    {"table-up", "\020", 0, NULL, 0, 0, k_uparrow, "Up one row"},

    {"table-backspace", "\010", 0, NULL, 0, 0, k_backspace, "Backspace"},
    {"table-tab", "\011", 0, NULL, 0, 0, k_tab, "Tab"},
    {"table-newline", "\012", 0, NULL, 0, 0, k_newline, "New line"},
    {"table-return", "\015", 0, NULL, 0, 0, k_newline, "Carriage return"},
    {"table-DELchar", "\177", 0, NULL, 0, 0, k_backspace, "DEL"},

    {"table-erase", "\025", 0, NULL, 0, 0, k_killbuff, "Erase"},

    {"table-up", "\033A", 0, NULL, 0, 0, k_uparrow, "Up"},
    {"table-down", "\033B", 0, NULL, 0, 0, k_downarrow, "Down"},
    {"table-right", "\033C", 0, NULL, 0, 0, k_rightarrow, "Right"},
    {"table-left", "\033D", 0, NULL, 0, 0, k_leftarrow, "Left"},
    {"table-end-row", "\033J", 0, NULL, 0, 0, k_endline, "End"},
    {"table-begin-row", "\033H", 0, NULL, 0, 0, k_home, "home"},
    {"table-top", "\033<", 0, NULL, 0, 0, k_top, "Top"},
    {"table-bottom", "\033>", 0, NULL, 0, 0, k_bottom, "Bottom"},

    {"table-toggle-debug", "\033\033", 0, NULL, 0, 0, k_debug, "Toggle Debug"},
    {NULL, NULL, 0, NULL, 0, NULL, NULL}
};

void k_DefineKeys (mainmap, classinfo)
struct keymap * mainmap;
struct spread_classinfo *classinfo;
{
    char ch;
    struct proctable_Entry *tempProc;

    tempProc = proctable_DefineProc("self_insert", k_enterchar, classinfo, NULL, "Enter character");
    for (ch = ' '; ch < 127; ch++) {		/* self-insert */
	if (ch != '/') {
	    char foo[2];
	    foo[0] = ch;
	    foo[1] = '\0';
	    keymap_BindToKey (mainmap, foo, tempProc, ch);
	}
    }

    bind_BindList(keytable, mainmap, NULL, classinfo);

}
