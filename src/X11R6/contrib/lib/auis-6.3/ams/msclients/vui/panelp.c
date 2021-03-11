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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/panelp.c,v 1.25 1994/03/29 04:08:20 rr2b Exp $";
#endif

/* 
 *      C Panel Handling Package
 *	Machine-independent routines
 *
 *
 *      The normal sequence of the panel handler calls is
 *
 *	1.  InitPanels	    (once)
 *	2.  SaveVideoEnv    (once)
 *	    Do one or more times:
 *	    3.	DrawPanel   (as needed)
 *	    4.	GetKey	    (once)
 *	    Enddo
 *	5.  RestoreVideoEnv (once)
 *
 *      The keytab provided to GetKeys is presumed to contain an exit key.
 *
 */


#include <andrewos.h>
#include <vui.h>
#include <ctype.h>
#include <panel.h>
#include <keycodes.h>
#include <setjmp.h>
#include <vuidebug.h>

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

extern int Interactive;

unsigned char msg_row = 23;
unsigned char error_row = 24;
unsigned char last_row = 24;
unsigned char past_col = 80;
/* global */ unsigned char opt_row = 22;
/* global */ unsigned char bar_row = 21;
PRIVATE FIELD *current_field = (FIELD *)NIL;

unsigned char cursor_row;       /* Cursor screen row         */
unsigned char cursor_col;       /* Cursor screen col         */
PRIVATE unsigned int  cursor_xtent;     /* Cursor field extent       */
PRIVATE unsigned int  cursor_offset;    /* Cursor posn within extent */
PRIVATE unsigned int  cursor_index;     /* Cursor posn within data   */

unsigned char isrtmode;

unsigned char Mono_AttrTable[]  = {
    0x07,0x0F, 0x70, 0x8F, 0x70, 0x07,
    0x07, 0x0F, 0x78, 0x07, 0x00, 0x70,
    0x01, 0x09
};

/* **** Used by PC not RT, so ignore compiler warnings */
unsigned char Color_AttrTable[] = {
    0x0B, 0x0E, 0x4F, 0x84, 0x4F, 0x0C,
    0x0A, 0x0D, 0x4E, 0x17, 0x00, 0x4F,
    0x0B, 0x0E
};
unsigned char BW_AttrTable[]    = {
    0x07, 0x0F, 0x70, 0x8F, 0x0F, 0x07,
    0x07, 0x0F, 0x78, 0x07, 0x00, 0x70,
    0x07, 0x0F
};
unsigned char EGAco_AttrTable[] = {
    0x0B, 0x0E, 0x4F, 0x84, 0x4F, 0x0C,
    0x0A, 0x0D, 0x4E, 0x17, 0x00, 0x4F,
    0x01, 0x09
};
unsigned char Termcap_AttrTable[]={
    0, 0, 1, 1, 1, 0,
    0, 1, 1, 0, 0, 1,
    1, 1
};
unsigned char *AttrTablePtr = Mono_AttrTable;
/* **** Used by PC not RT, so ignore compiler warnings */

PRIVATE FIELD *CursorLeft(), *CursorRight(), *CursorUp(); 
PRIVATE FIELD *CursorDown(), *RightField(), *LeftField();
PRIVATE FIELD *ReturnKey(), *IsrtToggle(), *DeleteChar();
PRIVATE FIELD *homeCursor(), *Backspace(), *EndCursor();
PRIVATE FIELD *EraseEOF(), *DataInput();
FIELD *RedrawScreen(), *SuperHomeKey();

PRIVATE KEYTAB StandardKeys[] = {
    KEYCODE_LEFT,	CursorLeft,       /* Left              */
    KEYCODE_RIGHT,	CursorRight,      /* Right             */
    KEYCODE_UP,		CursorUp,         /* Up                */
    KEYCODE_DOWN,	CursorDown,       /* Down              */
    KEYCODE_TAB,	RightField,       /* Tab               */
    KEYCODE_CTRL_TAB,	RightField,       /* Ctrl-Right        */
    KEYCODE_SHIFT_TAB,  LeftField,        /* Shift Tab         */
    KEYCODE_CTRL_LEFT,	LeftField,        /* Ctrl-Left         */
    KEYCODE_RETURN,	ReturnKey,        /* Enter             */
    KEYCODE_FAKERETURN,	ReturnKey,        /* Enter             */
    KEYCODE_INSERT,	IsrtToggle,       /* Ins               */
    KEYCODE_DELETE,	DeleteChar,       /* Del               */
    KEYCODE_HOME,	homeCursor,       /* home              */
    KEYCODE_F3,		SuperHomeKey,	  /* lets you go into a protected field */
    KEYCODE_END,	EndCursor,        /* End               */
    KEYCODE_BACKSPACE,  Backspace,        /* Backspace         */
    KEYCODE_DEL,	Backspace,        /* DEL               */
    KEYCODE_CTRL_END,	EraseEOF,         /* Ctrl-End          */
    KEYCODE_REDRAW,	RedrawScreen,				    /* mas V1.3 */
    MATCHANY,		DataInput,        /* Everything else   */
    0,			NULL
};

PRIVATE FIELD *LeftField (curfield)
FIELD *curfield;
{
    return (curfield->fleft);
}

PRIVATE FIELD *RightField (curfield)
FIELD *curfield;
{
    if (curfield->fprotecthandler) {
	FIELD *tmpfield;
	while ((tmpfield = CursorDown(curfield)) == curfield) {
	    if (curfield->flocation[cursor_xtent].xprt > 1)
		return(curfield);
	}
	return(tmpfield);
    }
    return (curfield->fright);
}

PRIVATE FIELD *CursorDown (curfield)
FIELD *curfield;
{
    register XTENT *x;

    x = curfield->flocation;
    if (x[cursor_xtent+1].xlen) {
	register int i;
	int new_offset, len, extendx;
	char *data;

	len = x[cursor_xtent++].xlen - cursor_offset;
	cursor_row = x[cursor_xtent].xrow;
	if (x[cursor_xtent].xprt)
	    new_offset = x[cursor_xtent].xprt;
	else {
	    if (cursor_offset >= x[cursor_xtent].xlen)
		new_offset = x[cursor_xtent].xlen - 1;
	    else
		new_offset = cursor_offset;
	}

	cursor_col = x[cursor_xtent].xcol + new_offset;
	cursor_index += (len + new_offset);
	cursor_offset = new_offset;
	    
	data = curfield->fdata;
	extendx = FALSE;
	for (i=0; i<=cursor_index; i++) {
	    if (data[i] == 0) extendx = TRUE;
	    if (extendx) data[i] = ' ';
	}
	if (extendx) data[cursor_index+1] = 0;
	SetCursorPosn();
	if (curfield->fprotecthandler) homeCursor(curfield);
	return (curfield);
    }

    if (curfield->ftype == FTYPE_SEL)
	curfield->fattr = NORMAL;

    return (curfield->fdown);
}

PRIVATE FIELD *CursorUp (curfield)
FIELD *curfield;
{

    if (cursor_xtent) {
	XTENT *x;
	int new_offset;

	cursor_xtent--;
	x = curfield->flocation;
	cursor_row = x[cursor_xtent].xrow;

	if (cursor_offset < x[cursor_xtent].xprt)  /* Are we inside a protected field? */
	    new_offset = x[cursor_xtent].xprt;
	else {
	    if (cursor_offset >= x[cursor_xtent].xlen)
		new_offset = x[cursor_xtent].xlen - 1;
	    else
		new_offset = cursor_offset;
	}

	cursor_col = x[cursor_xtent].xcol + new_offset;
	cursor_index -= (cursor_offset + (x[cursor_xtent].xlen - new_offset));
	cursor_offset = new_offset;

	SetCursorPosn();
	if (curfield->fprotecthandler) homeCursor(curfield);
	return (curfield);
    }

    if (curfield->ftype == FTYPE_SEL) curfield->fattr = NORMAL;
    return (curfield->fup);
}

PRIVATE FIELD *HandleEndofPage(curfield)
FIELD *curfield;
{
    if (curfield->ftype == FTYPE_SCR && curfield->fexithandler) {
	if (curfield->fprotecthandler) { /* is there protected field? */
	    EndCursor(curfield);
	    if (curfield->fdata[cursor_index] != ',' ||
		(curfield->fdata[cursor_index] == ' ' &&
		 curfield->fdata[cursor_index-1] != ',')) {
		homeCursor(curfield);
		return(CursorDown(curfield)); /* just go to next header or body */
	    }
	}
	(*(curfield->fexithandler)) (curfield);
	if (curfield->fprotecthandler)
	    (*(curfield->fprotecthandler)) (curfield);
	return(curfield);
    }
 
    return (curfield->freturn);
}

PRIVATE FIELD *ReturnKey (curfield)     /* Move down and to leftmost field */
FIELD *curfield;
{
    Boolean  TempInsert, Redisplay;

    if (!curfield->flocation[cursor_xtent+1].xlen) /* Are we at the end of page */
	return(HandleEndofPage(curfield));

    Redisplay  = FALSE;
    TempInsert = FALSE;

    if (curfield->fprotecthandler && isrtmode != INSERT) { /* is there protected field? */
	EndCursor(curfield);
	if (curfield->fdata[cursor_index] == ',' ||
	    (curfield->fdata[cursor_index] == ' ' &&
	     curfield->fdata[cursor_index-1] == ',')) {
	    TempInsert = TRUE; /* Make it look like insert mode */
	}
	else {
	    homeCursor(curfield);
	    return(CursorDown(curfield)); /* just go to next field */
	}
    }

    if ((isrtmode == INSERT) || (TempInsert)) {
	if (ShiftDown(curfield) != 0) return(curfield);
	Redisplay = TRUE;
    }

    if (curfield->fmodifyhandler)
	(*(curfield->fmodifyhandler)) (curfield);

    if (curfield->fprotecthandler)
	(*(curfield->fprotecthandler)) (curfield);

    if (Redisplay) {
	CursorOff();
	RefreshField((unsigned char)
		     ((current_field->fattr == INVISI) ? RINVIS : current_field->fattr));
	CursorOn();
    }

    homeCursor(curfield);
    return(CursorDown(curfield));
}

PRIVATE FIELD *CursorLeft (curfield)
FIELD *curfield;
{
    XTENT *x;

    if (curfield->ftype == FTYPE_SEL)
	return (LeftField (curfield));

    x = curfield->flocation;

    if (cursor_offset - x[cursor_xtent].xprt > 0) {
	cursor_col--;
	cursor_offset--;
	cursor_index--;
    }
    else
	if (!cursor_xtent)
	    return (LeftField (curfield));
	else {
	    cursor_xtent--;
	    cursor_offset = x[cursor_xtent].xprt;
	    cursor_row = x[cursor_xtent].xrow;
	    cursor_col = x[cursor_xtent].xcol + cursor_offset;
	    cursor_index -= (x[cursor_xtent+1].xprt + 
			     x[cursor_xtent].xlen - x[cursor_xtent].xprt);
	    curfield = EndCursor(curfield);
	}
    SetCursorPosn();
    return (curfield);
}

PRIVATE FIELD *CursorRight (curfield)
FIELD *curfield;
{
    XTENT *x;
    register char *data;

    if (curfield->ftype == FTYPE_SEL)
	return (RightField (curfield));

    x = curfield->flocation;
    if (cursor_offset+1 >= x[cursor_xtent].xlen) {
	if (x[cursor_xtent+1].xlen > 0) {
	    cursor_xtent++;
	    cursor_row = x[cursor_xtent].xrow;
	    cursor_col = x[cursor_xtent].xcol + x[cursor_xtent].xprt;
	    cursor_offset = x[cursor_xtent].xprt;
	    cursor_index += cursor_offset + 1;
	}
	else
	    return (RightField (curfield));
    }
    else {
	cursor_offset++;
	cursor_col++;
	cursor_index++;
    }

    data = (curfield->fdata) + cursor_index;    /* If we move right, past */
    if (*(data-1) == 0) {                       /* the end of a string,   */
	*(data-1) = ' ';                        /* we add a tailing blank */
	*data = 0;                              /* and advance the string */
    }                                       /* terminator.            */
    SetCursorPosn();
    return (curfield);
}

PRIVATE FIELD *DeleteChar (curfield)
FIELD *curfield;
{
    XTENT *x;
    register int len, row, col;
    char *data;
    unsigned char cursor_attr;

    x = curfield->flocation;
    if (curfield->ftype == FTYPE_YN) {
	ErrorBeep ();
	return (curfield);
    }

    cursor_attr = (current_field->fattr == INVISI ? RINVIS : current_field->fattr);

    data = curfield->fdata + cursor_index;
    len = x[cursor_xtent].xlen - cursor_offset - 1;

    while (*data && len--) {   /* Delete a char */
	*data = *(data+1);
	data++;
    }
    if (*data) *data=' ';

    if (curfield->fmodifyhandler)
	(*(curfield->fmodifyhandler)) (curfield);

    CursorOff ();
    len = x[cursor_xtent].xlen - cursor_offset;
    row = x[cursor_xtent].xrow;
    col = x[cursor_xtent].xcol + cursor_offset;
    ShowString(curfield->fdata+cursor_index, row, col, len,
		cursor_attr);
    CursorOn ();
    return (curfield);
}

PRIVATE FIELD *EndCursor (curfield)
FIELD *curfield;
{
    register XTENT *x;
    register char *data;
    register int i;
    int last_nonblank;

    x = curfield->flocation;  /* Find the beginning of the current xtent */
    debug((1,"EndCursor, index=%d, offset=%d\n", cursor_index, cursor_offset));
    data=curfield->fdata+cursor_index-cursor_offset;
    last_nonblank=0;

    for (i=1; i<=x[cursor_xtent].xlen; i++) {
	if (!*data) break;     /* End of string */
	if (*data != ' ') last_nonblank=i;
	data++;
    }
    if (last_nonblank==x[cursor_xtent].xlen) last_nonblank--;
    if (last_nonblank < x[cursor_xtent].xprt) last_nonblank = x[cursor_xtent].xprt;
    cursor_index = cursor_index - cursor_offset + last_nonblank;
    cursor_offset = last_nonblank;
    debug((1,"Resetting to index=%d, offset=%d\n", cursor_index, cursor_offset));

    cursor_row = x[cursor_xtent].xrow;
    cursor_col = x[cursor_xtent].xcol + cursor_offset;
    SetCursorPosn();
    return (curfield);
}

PRIVATE FIELD *Backspace (curfield)
FIELD *curfield;
{
    XTENT *x;
    int   i,j, len, deleting_line = 0;
    char *data, *data2;
    unsigned char old_cr, old_cc, cursor_attr;

    if ((cursor_offset == 0) && (cursor_xtent == 0)) {
	ErrorBeep ();
	return (curfield);
    }

    if ((cursor_offset == 1) && (curfield->flocation[cursor_xtent].xprt == 1)) {
	/* if they really want to go over a protected field, let them */
	curfield->flocation[cursor_xtent].xprt = 0;
    }

    if (cursor_offset==0) { /* Move the current line up to the previous line */
	data2 = curfield->fdata+cursor_index;
	CursorUp(curfield);
	EndCursor(curfield);
	DeleteChar(curfield); /* just in case the 80th column is full */
	data = curfield->fdata+cursor_index;
	if (*data != ' ') return(curfield);
	if (cursor_offset == 0) deleting_line++;
	/* Copy current extent & Put blanks in balance of current extent */
	/* first figure out how many characters to copy */

	x = curfield->flocation;
	for (len=0; len <x[cursor_xtent+1].xlen; len++)
	    if (!*(data2+len)) break;

	if (len)
	    memcpy_preserve_overlap(data, data2, len);
	for (i=cursor_offset+len; i<x[cursor_xtent+1].xlen; i++) {
	    if (*(data2+i)) *(data2+i) = ' ';
	    else break;
	}

	if (curfield->fmodifyhandler) (*(curfield->fmodifyhandler))(curfield);

	if (deleting_line) { /* push everything up in this screen */
	    i = strlen(data2+x[cursor_xtent+1].xlen);
	    memcpy_preserve_overlap(data2, data2+x[cursor_xtent+1].xlen, i);
	    for (j=0; j<x[cursor_xtent+1].xlen; j++) {
		if (*(data2+i+j)) *(data2+i+j) = ' ';
		else break;
	    }
	    if (curfield->fshrinkhandler) (*(curfield->fshrinkhandler)) (curfield);
	    if (curfield->fprotecthandler) (*(curfield->fprotecthandler)) (curfield);
	}


	CursorOff();
	cursor_attr = (current_field->fattr == INVISI ? RINVIS : current_field->fattr);

	if (deleting_line) {
	    RefreshField(cursor_attr);
	}
	else {
	    ShowString(data, x[cursor_xtent].xrow, x[cursor_xtent].xcol+cursor_offset, 
		       x[cursor_xtent].xlen-cursor_offset, cursor_attr);
	    if (len)
		ShowString(data2, x[cursor_xtent+1].xrow, x[cursor_xtent+1].xcol,
			   x[cursor_xtent+1].xlen, cursor_attr);
	}

	CursorOn();
	if (deleting_line) {
	    EndCursor(curfield);
	    homeCursor(curfield);
	}
	SetCursorPosn();
	return(curfield);
    }

    old_cr = cursor_row;
    old_cc = cursor_col;
    CursorLeft (curfield);
    if ((old_cr == cursor_row) && (old_cc == cursor_col))
	return (curfield);

    if (isrtmode != INSERT)
	DeleteChar (curfield);
    else {
	isrtmode = REPLACE;
	if (HandleChar (curfield, ' ', FALSE) != 0)
	    CursorRight (curfield);
	isrtmode = INSERT;
    }

    return (curfield);
}

PRIVATE FIELD *EraseEOF (curfield)
FIELD *curfield;
{
    XTENT *x;
    register int i;
    int totallen, row, col,movelinesup = TRUE;
    char *data;
    unsigned char cursor_attr;

    debug((1,"Ending line at index=%d, offset=%d, xtent=%d\n",
	    cursor_index, cursor_offset, cursor_xtent));
    x = curfield->flocation;
    if (curfield->ftype == FTYPE_YN || curfield->ftype == FTYPE_SEL) {
	ErrorBeep ();
	return (curfield);
    }

    cursor_attr = (current_field->fattr == INVISI ? RINVIS : current_field->fattr);
    data=curfield->fdata+cursor_index-cursor_offset;
    totallen=strlen(data);
    data += cursor_offset;
    debug((1,"We show %d characters in this extent\n", totallen));
    if (totallen<x[cursor_xtent].xlen) /* we know end of string in this xtnt */
	*data = '\0';
    else {
	debug((1,"Blanking out %d - %d = %d chars.\n",x[cursor_xtent].xlen,
	       cursor_offset, x[cursor_xtent].xlen - cursor_offset));
	for (i=cursor_offset; i<x[cursor_xtent].xlen; i++) {
	    if (!*data) break;
	    if (*data != ' ') movelinesup = FALSE;
	    *data++ = ' ';
	}
    }
    if (curfield->fmodifyhandler)
	(*(curfield->fmodifyhandler)) (curfield);

    CursorOff();
    row = x[cursor_xtent].xrow;
    col = x[cursor_xtent].xcol + cursor_offset;
    ShowString(curfield->fdata+cursor_index, row, col,
		x[cursor_xtent].xlen - cursor_offset, cursor_attr);
    CursorOn();

    if (movelinesup)
	DeleteCurrentLine(curfield);

    return (curfield);
}

PRIVATE int DeleteCurrentLine(curfield)
FIELD *curfield;
{
    XTENT *x;
    unsigned char cursor_attr;

    if ((cursor_offset != 0) || (!curfield->fshrinkhandler)) return 0;

    x = curfield->flocation;
    cursor_attr = (current_field->fattr == INVISI ? RINVIS : current_field->fattr);

    if (x[cursor_xtent+1].xlen) { /* are we at the end of page? */
	x[cursor_xtent+1].xprt = 0;
	curfield = CursorDown(curfield);
	curfield = Backspace(curfield);
    } 
    else { 
	(*(curfield->fshrinkhandler)) (curfield);
	ShowString(curfield->fdata+cursor_index,x[cursor_xtent].xrow,
		   x[cursor_xtent].xcol + cursor_offset, x[cursor_xtent].xlen, cursor_attr);

	if (curfield->fprotecthandler) {
	    (*(curfield->fprotecthandler)) (curfield);
	    RefreshField(cursor_attr);
	}
    }
    return 0;
}

PRIVATE int IntegerCheck (data, chr, offset)
char *data, chr;
int offset;
{
    register int i, j;
    int df = FALSE, ib = FALSE;
    int len;
    char curchar;

    len = strlen (data);
    if ((len == offset) || (isrtmode == INSERT))
	len++;                          /* new char will extend data */

    for (i=j=0; i<len; i++) {
	curchar = (i == offset ? chr : data[i]);
	if (i == offset) {
	    curchar = chr;
	    if (isrtmode != INSERT)
		j++;
	}
	else
	    curchar = data[j++];

	if ((curchar >= '0') && (curchar <= '9')) {
	    df = TRUE;
	    if (ib)
		return (2);             /* Imbedded blank error */
	}
	else if (curchar == ' ') {
	    if (df)
		ib = TRUE;
	}
	else
	    return (1);                 /* Non-numeric char */
    }
    return (0);
}

PRIVATE PRMPT SelectMsg[] = {
    49, "(Use the TAB key (-->) to select the current item)"	    /* special chars gone */
};

PRIVATE PRMPT OnlyDigits[] = {
    54, "(Only numeric characters may be entered in this field)"
};

PRIVATE PRMPT NoImbeddedBlanks[] = {
    47, "(Imbedded blanks are not allowed in this field)"
};

PRIVATE PRMPT YesOrNo[] = {
    32, "(Type Y for \"yes\" or N for \"no\")"
};

PRIVATE unsigned char ValidateData (curfield, chr)
FIELD *curfield;
unsigned char chr;
{
    register int i;
    int result, lhe, rhs, rhnb, lhnb;

    switch (curfield->ftype) {
	case FTYPE_YN:
	    chr = toupper (chr);
	    if ((chr != 'Y') && (chr != 'N')) {
		ShowError (YesOrNo);
		return (0);
	    }
	    break;

	case FTYPE_INT:
	    result = IntegerCheck (curfield->fdata, chr, cursor_offset);
	    if (result == 1) {
		ShowError (OnlyDigits);
		return (0);
	    }
	    else if (result == 2) {
		ShowError (NoImbeddedBlanks);
		return (0);
	    }
	    break;

	case FTYPE_SCR:
	case FTYPE_ASC:
	    if ((chr < 0x20) || (chr > 0x7E)) {
		ErrorBeep ();
		return (0);
	    }
	    break;

	case FTYPE_SEL:
	    ShowError (SelectMsg);
	    return (0);
	    /** break; *** avoid compiler warnings */

	case FTYPE_WRD:
	    if (isrtmode == INSERT) {
		if (cursor_offset == 0) {
		    lhe = 0;                /* lhe = "left hand end" */
		    rhs = 0;                /* rhs = "right hand start" */
		}
		else {
		    lhe = cursor_offset-1;
		    rhs = cursor_offset;
		}
	    }
	    else
		if (cursor_offset == 0) {
		    lhe = 0;
		    rhs = (curfield->fdata[0] ? 1 : 0);
		}
		else {
		    lhe = cursor_offset-1;
		    rhs = (curfield->fdata[cursor_offset] ? cursor_offset+1 :
			   cursor_offset);
		}

	    lhnb = FALSE;                   /* lhnb = "left hand not blank" */
	    if (lhe != 0)
		for (i=0; i<=lhe; i++)
		    if (curfield->fdata[i] != ' ') {
			lhnb = TRUE;
			break;
		    }
	    rhnb = FALSE;                   /* rhnb = "right hand not blank" */
	    for (i=rhs; curfield->fdata[i]; i++)
		if (curfield->fdata[i] != ' ') {
		    rhnb = TRUE;
		    break;
		}
	    if ((rhnb && lhnb && (chr == ' ')) ||
		((chr != ' ') &&
		 ((lhnb && (curfield->fdata[lhe] == ' ')) ||
		  (rhnb && (curfield->fdata[rhs] == ' '))))) {
		ShowError (NoImbeddedBlanks);
		return (0);
	    }
	    break;

	default:
	    break;
    }

    return (chr);
}

PRIVATE int ShiftDown (curfield)
FIELD *curfield;
{
    XTENT *x;
    register int i;
    int new_offset, totallen, last_xtent;
    char *data, *data2;
/*    unsigned char cursor_attr; */

    if (curfield == (FIELD *)NULL) return(-1);

    x = curfield->flocation;

/*    cursor_attr = (current_field->fattr == INVISI ? RINVIS : current_field->fattr); */

    /* ****  Check to see if the last line == blanks **** */

    totallen = strlen(curfield->fdata);

    debug((1,"The total string length is %d\n",totallen));
    debug((1,"We are at %d in this extent (%d in the data)\n", cursor_offset, cursor_index));
    last_xtent = cursor_xtent;
    new_offset = cursor_index - cursor_offset + x[cursor_xtent].xlen;
    debug((1,"The current extent is %d and the end of it is at %d\n",
	    last_xtent, new_offset));
    while (x[last_xtent+1].xlen && new_offset<totallen)
	new_offset += x[++last_xtent].xlen;

    debug((1,"The last extent is %d and the end of it is at %d\n", last_xtent, new_offset));

    if (x[last_xtent+1].xlen==0) {
	/* beginning of last extent */
	data = curfield->fdata + new_offset - x[last_xtent].xlen;
	for ( i=0; i<x[last_xtent].xlen; i++) {
	    if (!*data) break;
	    else if (*data != ' ') {
		if ((curfield->fexpandhandler) &&
		    (((*(curfield->fexpandhandler)) (curfield)) == 0))
		    break;
		ErrorBeep();
		return(-1);
	    } else data++;
	}
	new_offset -= x[last_xtent--].xlen;
	debug((1,"Last of field, so backing up to xtent	%d, offset %d\n",
	       last_xtent, new_offset));
    }

    debug((1,"So now we know there is room for a new line.\n"));

    for (i=last_xtent; i>cursor_xtent; i--) {
	debug((1,"We are moving	%d bytes from %d to %d\n", x[i].xlen,
	       new_offset-x[i].xlen, new_offset));
	memcpy_preserve_overlap(curfield->fdata+new_offset,
				curfield->fdata+new_offset-x[i].xlen, x[i].xlen);
	new_offset -= x[i].xlen;
    }

    data = curfield->fdata + cursor_index; /* set to cursor location */
    data2= curfield->fdata + new_offset;   /* set to next line */
    debug((1,"Now we copy from %d to %d\n",cursor_offset,x[cursor_xtent].xlen));
    debug((1,"or origin data, from %d to %d\n", cursor_index, new_offset));

    for (i=cursor_offset; i<x[cursor_xtent].xlen; i++) {
	*data2++ = *data;
	*data++ = ' ';
    }

    debug((1,"Now we blank from	%d to %d\n", x[cursor_xtent].xlen-cursor_offset,    x[cursor_xtent+1].xlen));

    for (i=x[cursor_xtent].xlen-cursor_offset; i<x[cursor_xtent+1].xlen; i++)
	*data2++ = ' ';
    
    data2=curfield->fdata+cursor_index-cursor_offset;

    return(0);
}

PRIVATE int ShiftRight (curfield)
FIELD *curfield;
{
    XTENT *x;
    register int i;
    int len, totallen;
    char *data, *idata;
    unsigned char cursor_attr, extendx, noblanks, tmprow, tmpcol;

    x = curfield->flocation;
    if (curfield->ftype == FTYPE_YN) {
	return (0);
    }

    totallen=strlen(curfield->fdata);
    cursor_attr = (current_field->fattr == INVISI ? RINVIS : current_field->fattr);
    len = cursor_index - cursor_offset;
    data = curfield->fdata + len;
    idata=data;                 /* Remember where we were        */
    i = cursor_xtent;
    extendx=FALSE;
    noblanks=TRUE;
    while (x[i].xlen) {         /* Now go through the rest to find 2 ' 's */
	len+=x[i].xlen;
	if (len>totallen) {     /* Did we go past the end of the string?  */
	    extendx=TRUE;
	    break;
	}
	data += x[i++].xlen;
	if (*(data-1)==' ' && *(data-2)==' ') {
	    noblanks=FALSE;
	    break;
	}
	if ((x[i].xlen) && (x[i].xprt > 1)) {
	    ErrorBeep();
	    return(-1);
	}
    }
    if (noblanks && x[i].xlen==0) {  /* We ran out of extents        */
	ErrorBeep ();
	return (-1);
    }

    idata += cursor_offset;     /* Add how far into the extent we were */
    if (extendx) {
	data=curfield->fdata+totallen;
	*(data+1) = '\0';
    } else {
	/* Find the last non-blank */
	data -= 2;
	while (*data==' ' && data>idata) data--;
	data++;                     /* Leave ptr at first blank       */
    }
    len=data-idata;
    memcpy_preserve_overlap (idata+1, idata, len);    /* This handles overlapped copy! */

    tmprow = cursor_row;    /* mas V1.3 : this is a hack to get around */
    tmpcol = cursor_col;    /* andrew\CursorOff changing cursor_row&col */
    CursorOff ();
    i = cursor_xtent;
    totallen = x[i].xlen - cursor_offset - 1;
    ShowString(idata+1, tmprow, tmpcol+1, totallen, cursor_attr);
    idata+=totallen+1;
    if (len > totallen) {
	len -= totallen;
	while (x[i++].xlen) {
	    ShowString(idata, x[i].xrow, x[i].xcol, x[i].xlen, cursor_attr);
	    if (len < x[i].xlen)
		break;
	    len -= x[i].xlen;
	    idata += x[i].xlen;
	}
    }

    CursorOn ();
    return (0);
}

PRIVATE FIELD *DataInput (curfield, chr)
FIELD *curfield;
unsigned char chr;
{
    HandleChar (curfield, chr, TRUE);
    return (curfield);
}

PRIVATE int HandleChar (curfield, chr, advance)
FIELD *curfield;
unsigned char chr;
int advance;
{
    XTENT *x;
    char *data;
    unsigned char cursor_attr;

    if ((chr = ValidateData (curfield, chr)) ==  0)
	return (-1);

    if (isrtmode == INSERT)
	if (ShiftRight (curfield) != 0)
	    return (-1);

    x = curfield->flocation;
    cursor_attr = (current_field->fattr == INVISI ? RINVIS : current_field->fattr);
    data = (curfield->fdata) + cursor_index;

    if (*data)                      /* If we're writing past the end of   */
	*data = chr;                /* a string, be sure to propagate the */
    else {                          /* terminating zero!                  */
	*data = chr;
	*(data+1) = 0;
    }

    if (curfield->fmodifyhandler)
	(*(curfield->fmodifyhandler)) (curfield);

    if ((curfield->fattr != INVISI) && (curfield->fattr != RINVIS)) {
	ShowString(data, cursor_row, cursor_col, 1, cursor_attr);
    }

    if (advance) {
	if ((cursor_xtent == 0) && (x[0].xlen == 1) && (x[1].xlen == 0)) {
	    /* do nothing */
	}
	else {
	    if (CursorRight (curfield) != curfield)
		ErrorBeep ();
	    else {
	    }
	}
    }

    return (0);
}

PRIVATE FIELD *homeCursor (curfield)
FIELD *curfield;
{
    register XTENT *x;

    if (curfield) {
	x=curfield->flocation;
	cursor_row = x[cursor_xtent].xrow;
	cursor_col = x[cursor_xtent].xcol + x[cursor_xtent].xprt;
	cursor_index = cursor_index - cursor_offset + x[cursor_xtent].xprt;
	cursor_offset = x[cursor_xtent].xprt;
	SetCursorPosn();
    }
    return (curfield);
}

PRIVATE FIELD *SuperHomeKey (curfield)
FIELD *curfield;
{
    /* since headers have protected fields, if a user wants to get rid of it,
	they should be able to move inside that protected field.  This routine
	invoked by F3 or <Esc>3 will zero out the xprt field and then calls
	homeCursor() */

    if (curfield && curfield->fprotecthandler) { /* message headers */
	curfield->flocation[cursor_xtent].xprt = 0;
	homeCursor(curfield);
    }

    return(curfield);
}

PRIVATE FIELD *IsrtToggle (curfield)
FIELD *curfield;
{
    if (curfield->ftype != FTYPE_SEL) {
	isrtmode ^= INSERT;
	CursorOn ();
    }
    return (curfield);
}

ClearError ()
{
    ClearLine(error_row);
}

/* 
  *  This procedure fills in the length fields in panel and field
  *  arrays to agree with the corresponding constant strings.
  *  Some minimal consistency checking is also performed.
  *
  */

InitPanels (p, f)
PANEL *p[];
FIELD *f[];
{
    register int i, j;

    if (p != NULL)
	for (j=0; p[j]; j++)
	    for (i=0; p[j][i].pdata; i++)
		if (p[j][i].plen == 0)
		    p[j][i].plen = strlen (p[j][i].pdata);
    if (f != NULL)
	for (j=0; f[j]; j++)
	    for (i=0; f[j][i].freturn; i++) {
		if (f[j][i].ftype == FTYPE_YN)
		    if ((f[j][i].flocation[0].xlen != 1) ||
			(f[j][i].flocation[1].xlen != 0)) {
			printf ("Field array %d, item %d,", j+1, i);
			printf (" - length must be 1 for YN field\n");
			exit (99);
		    }
		if ((f[j][i].fprompt) && (f[j][i].fprompt->plen == 0))
		    f[j][i].fprompt->plen = strlen (f[j][i].fprompt->pdata);
	    }
}

ShowCursor ()
{
    unsigned char cursor_attr;
    XTENT *x;

    if (current_field == (FIELD *)NIL) return;
    x = current_field->flocation;
    cursor_row = x->xrow;
    cursor_col = x->xcol + x->xprt;
    cursor_xtent = 0;
    cursor_offset = cursor_index = x->xprt;
    cursor_attr = (current_field->fattr == INVISI ? RINVIS : current_field->fattr);

    RefreshField(cursor_attr);

    if (current_field->ftype != FTYPE_SEL)
	CursorOn ();
}

EraseCursor ()
{
    CursorOff ();
    RefreshField((unsigned char)NORMAL);
}

RefreshField(attr)
unsigned char attr;
{
    register int i = 0;
    char *data, *enddata;
    XTENT *x;

    if (current_field == (FIELD *)NIL) return;
    data = current_field->fdata;
    enddata = data;
    while (*enddata) enddata++; /* Find the end of the string */
    x = current_field->flocation;

    while (x[i].xlen) {
	if (x[i].xprt) {
	    ShowString(data, x[i].xrow, x[i].xcol, x[i].xprt,HILITE);
	    data += x[i].xprt;
	    if (data > enddata) *data = '\0';
	}
	ShowString (data, x[i].xrow, x[i].xcol+x[i].xprt, x[i].xlen-x[i].xprt, attr);
	data += (x[i].xlen - x[i].xprt);
	if (data>enddata) *data='\0';  /* Show blanks past the end */
	i++;
    }
}

GetCursorPosition(row,col)
int *row,*col;
{
    *row = (int)cursor_row;
    *col = (int)cursor_col;
}

PositionCursor(row,col)
int row,col;
{
    if (current_field == (FIELD *)NULL)
	return;

    while(cursor_row < row)
	current_field = CursorDown(current_field);

    while(cursor_col < col)
	current_field = CursorRight(current_field);
}

DrawPanel (panel, inputfield, clearp)
PANEL *panel;
FIELD *inputfield;
int clearp;
{
    register int i = 0;

    CursorOff ();
    if (clearp) ClearScreen();
    if (panel) {
	while (panel[i].plen) {
	    ShowString (panel[i].pdata, panel[i].prow, panel[i].pcol,
			panel[i].plen, panel[i].pattr);
	    i++;
	}
    }

    if (inputfield) {
	current_field = inputfield;
	if (inputfield->fentryhandler)
	    (*(inputfield->fentryhandler)) (inputfield);
	if (inputfield->fprompt)
	    ShowPrompt (inputfield->fprompt);
	ShowCursor ();
    }
    else
	if (current_field && current_field->ftype != FTYPE_SEL)
	    CursorOn ();
}

ShowPrompt (msg)
PRMPT *msg;
{
    CursorOff ();
    ClearLine(msg_row);
    ShowString (msg->pdata, msg_row, 0, msg->plen, HILITC);
    if (current_field && current_field->ftype != FTYPE_SEL)
	CursorOn ();
}

FIELD *GetCurrentField ()
{
    return (current_field);
}

SetCurrentField (newfield, prompt)
FIELD *newfield;
int prompt;
{
    if (current_field) {
	if (current_field->fexithandler && current_field->ftype != FTYPE_SCR) {
	    if (current_field->ftype != FTYPE_SEL)
		(*(current_field->fexithandler)) (current_field);
	    else {
		if (current_field->fmodifyhandler)
		    (*(current_field->fmodifyhandler)) (current_field);
		if (current_field->fexithandler != newfield->fexithandler)
		    (*(current_field->fexithandler)) (current_field);
	    }
	}
	EraseCursor ();
    }
    current_field = newfield;
    if ((FIELD *)NIL==current_field) return;
    ShowCursor ();
    if (newfield->fentryhandler)
	(*(newfield->fentryhandler)) (newfield);
    if ((prompt) && (newfield->fprompt))
	ShowPrompt (newfield->fprompt);
}

RestoreCurrentField (curfield)
FIELD *curfield;
{
    XTENT *x;

    current_field = curfield;
    if (current_field == (FIELD *)NIL) return;
    x = current_field->flocation;
    cursor_row = x->xrow;
    cursor_col = x->xcol;
    cursor_xtent = cursor_offset = cursor_index = 0;
}

GetKey (keytable, standard_also)
KEYTAB *keytable;
char standard_also;
{
    int skflag;
    register int i, ch;
    KEYTAB *k;
    FIELD *new_field = NULL;

    while (TRUE) {
	ch = KeyIn();
	debug((1, "KeyIn: %d\n", ch));
	k = keytable;
	skflag = !standard_also;

	for (i=0;;) {
	    if (k[i].scan_code == 0) {
		if (!skflag) {
		    k = StandardKeys;
		    skflag = TRUE;
		    i = 0;
		}
		else {
		    ErrorBeep ();
		    break;
		}
	    }

	    if ((ch == k[i].scan_code) || (k[i].scan_code == MATCHANY)) {
		ClearError ();
		new_field = (*(k[i].keyhandler)) (current_field, (unsigned char) ch);
		if (new_field != current_field)
		    SetCurrentField (new_field, PANEL_PROMPT);
		break;
	    }
	    else i++;
	}
    }
}

jmp_buf inppanel;

FIELD *InpEscapeKey (curfield)
FIELD *curfield;
{
    longjmp (inppanel, -1);
    return (FIELD *)NULL; /* avoid compiler warnings */
}

FIELD *InpReturnKey (curfield)
FIELD *curfield;
{
    longjmp (inppanel, -2);
    return (FIELD *)NULL; /* avoid compiler warnings */

}

extern FIELD *HelpKey2(), *DebugKey2();

KEYTAB ee_keys [] = {
    KEYCODE_ESCAPE,	InpEscapeKey,       /* Esc         */
    KEYCODE_RETURN,	InpReturnKey,       /* Enter       */
    KEYCODE_FAKERETURN,	InpReturnKey,       /* Enter       */
    KEYCODE_F1,		HelpKey2,           /* F1          */
#ifdef DEBUG
    KEYCODE_ALT_F1,	DebugKey2,          /* Alt-F1      */
#endif /* DEBUG */
    0,			0
};


int GetFromUser(row, col, length, deflt, field_type, return_str)
int row, col, length;
char *deflt, field_type, *return_str;
{
    XTENT v_temp_x[2];
    FIELD v_temp_f[2], *v_temp_fa[2];
    FIELD *hold_current;
    int rc;

    v_temp_f[0].freturn=v_temp_f[0].fup=v_temp_f[0].fdown=v_temp_f[0].fleft=
      v_temp_f[0].fright = v_temp_f;
    v_temp_f[0].flocation = v_temp_x;
    v_temp_f[0].fattr = RVIDEO;
    v_temp_f[0].fdata = return_str;
    v_temp_f[0].fprompt = (PRMPT *)NIL;
    v_temp_f[0].fmodifyhandler = v_temp_f[0].fentryhandler = (int (*)())NULL;
    v_temp_f[0].fexithandler = v_temp_f[0].fprotecthandler = (int (*)())NULL;
    v_temp_f[0].fshrinkhandler = v_temp_f[0].fexpandhandler = (int (*)())NULL;
    v_temp_f[1].freturn=v_temp_f[1].fup=v_temp_f[1].fdown=v_temp_f[1].fleft=
      v_temp_f[1].fright = (FIELD *)NIL;
    v_temp_f[1].flocation = (XTENT *)NIL;
    v_temp_f[1].fattr = '\0';
    v_temp_f[1].fdata = NIL;
    v_temp_f[1].fprompt = (PRMPT *)NIL;
    v_temp_f[1].fmodifyhandler = v_temp_f[1].fentryhandler = (int (*)())NULL;
    v_temp_f[1].fexithandler = v_temp_f[1].fprotecthandler =(int (*)())NULL;
    v_temp_f[1].fshrinkhandler = v_temp_f[1].fexpandhandler = (int (*)())NULL;
    v_temp_fa[0] = v_temp_f;  v_temp_fa[1] = (FIELD *)NIL;
    v_temp_f->ftype = field_type;
    v_temp_x[0].xrow = row;
    v_temp_x[0].xcol = col;
    v_temp_x[0].xlen = length;
    v_temp_x[0].xprt = 0;
    v_temp_x[1].xrow = v_temp_x[1].xcol = v_temp_x[1].xlen = v_temp_x[1].xprt = 0;
    if (deflt != NIL) strcpy(return_str, deflt); else *return_str = '\0';
    InitPanels((PANEL **)NULL, v_temp_fa);
    hold_current = GetCurrentField();
    rc=setjmp(inppanel);
    if (rc==-1) {
	*return_str = '\0';    /* "" since escape was hit */
	SetCurrentField(hold_current, PANEL_NOPROMPT);
	return(-1);
    }
    if (rc!=0) {
	strip(return_str);
	SetCurrentField(hold_current, PANEL_NOPROMPT);
	return(0);
    }
    SetCurrentField(v_temp_f, PANEL_NOPROMPT);
    GetKey(ee_keys, TRUE);
    debug((1, "GetFromUser got '%s'\n", return_str));
}

strip (str)
char *str;
{
    register int i, j;

    i = 0;
    while (str[i]==' ') i++;
    for (j=0; (str[j] = str[i]); j++, i++) ;
    j--;
    while (j>=0 && (str[j]==' '))
	str[j--] = 0;
}

int GetNUser( row, col, length, min, max, val_out)
int row, col, length, min, max, *val_out;
{
    int rc;
    char str_out[80], *msgp;
    PRMPT temp;
    while (1) {
	rc=GetFromUser(row, col, length, NIL, FTYPE_INT, str_out);
	if (rc!=0) return(rc);
	if (*str_out == '\0') /* the user didn't enter anything */
	    return(-1);
	rc=atoi(str_out);
	if (rc>=min && rc <=max) {
	    (*val_out)=rc;
	    return(0);
	}
	msgp=NIL;
	if (rc<min) {
	    msgp="The value you entered is too small.";
	} else if (rc>max) {
	    msgp="The value you entered is too large.";
	}
	if (msgp!=NIL) {
	    temp.plen=strlen(msgp);
	    temp.pdata=msgp;
	    ShowError(&temp);
	}
    }
}

ConfirmUser(msg)
char *msg;
{
    if (Interactive) {
	char str_out[2];
	ClearLine(opt_row);
	ShowString(msg,opt_row,0,strlen(msg), HILITE);
	GetFromUser(opt_row, strlen(msg), 0, NIL, FTYPE_SEL, str_out);
	ClearError();
	cursor_row = error_row;
	cursor_col = 0;
    } else {
	int c;
	printf(msg); fflush(stdout);
	while ((c = getchar()) != EOF && c != '\n');
    }
}

char *GetSUser(msg, prompt, deflt, str_out)
char *msg, *prompt, *deflt, *str_out;
{
    PRMPT prompt_str;
    int rc;

    ClearError();
    ClearLine(opt_row);
    ShowString(msg,opt_row,0,strlen(msg), HILITE);
    if (prompt != NIL) {
	prompt_str.plen = strlen(prompt);
	prompt_str.pdata = prompt;
	ShowPrompt(&prompt_str);
    }
    rc = GetFromUser(opt_row, strlen(msg), 80-strlen(msg), deflt, FTYPE_ASC, str_out);
    if (rc == -1) return(NIL);
    return(str_out);
}

GetStringFromUser(prompt, output, length, IsPassword)
char *prompt, *output;
int length;
int IsPassword;
{
    if (Interactive) {
	GetSUser(prompt, NIL, NIL, output);
    } else {
	int     c, lim;
	printf("%s? ", prompt); fflush(stdout);
	if(!IsPassword) {
	    output[0] = '\0';
	    for (lim = 0; lim < length && (c = getchar()) != EOF && c != '\n'; ++lim) {
		output[lim] = (char) c;
	    }
	    output[lim] = 0;
	    if (c == EOF) clearerr(stdin);
	} else {
	    extern char *getpass();
	    char *pass=getpass("");
	    if(pass==NULL || strlen(pass)>length-1) return -1;
	    strcpy(output, pass);
	    while(*pass) *pass++='\0';
	}
    }
}

char GetBooleanNotInteractive(msg, deflt)
char *msg, deflt;
{
    char result;

    printf("%s%s ", msg, (*(msg+strlen(msg)-1)=='?' ? "" : "?"));
    fflush(stdout);
    result = getchar();
    return (result=='y' || result=='Y');
}

char GetBooleanFromUser(msg, deflt)
char *msg, deflt;
{
    char result[2];
    int len, freeit = FALSE;

    if (!Interactive)
	return(GetBooleanNotInteractive(msg, deflt));

    ClearLine(opt_row);
    ClearLine(opt_row+1);
    len=strlen(msg);
    if (*(msg+len-1) != '?') {
	char *newmsg;
	freeit = TRUE;
	newmsg=malloc(len+2);
	strcpy(newmsg, msg);
	strcat(newmsg, "?");
	msg = newmsg;
    }
    ShowString(msg,opt_row,0,strlen(msg), HILITE);
    while(1) {
	GetFromUser(opt_row, 1+strlen(msg), 1, deflt? "Y":"N", FTYPE_YN, result);
	if (*result == 'Y' || *result == 'y' || *result == 'n' || *result == 'N')
	    break;
	ClearError();
	ShowMsg(YesOrNo);
    }
    if (freeit) free(msg);
    return (*result=='y' || *result=='Y');
}

ShowMsg (msg)
PRMPT *msg;
{
    static char start_error[] = {">>>"};
    static char stop_error[]  = {"<<<"};
    unsigned char col;

    ClearError ();
    CursorOff ();
    ShowString (start_error, error_row, col = ((past_col-10-(msg->plen)) >> 1),
		 sizeof (start_error), BLINKH);
    ShowString (msg->pdata, error_row, col += 4, msg->plen, HILITC);
    ShowString (stop_error, error_row, col+(msg->plen)+1,
		 sizeof (stop_error), BLINKH);
    if (current_field && current_field->ftype != FTYPE_SEL)
	CursorOn ();
    vuirefresh();
}
