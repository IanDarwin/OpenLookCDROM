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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/eq/RCS/eqvcmds.c,v 2.13 1992/12/15 21:32:53 rr2b R6tape $";
#endif


 

/*
 * eqvcmds.c
 * This module handles the view commands for eq.
 */


#include <class.h>
#define AUXMODULE 1
#include <eqv.eh>

#include <eq.ih>
#include <dataobj.ih>
#include <proctbl.ih>
#include <keymap.ih>
#include <menulist.ih>
#include <bind.ih>
#include <im.ih>
#include <view.ih>
#include <message.ih>
#include <stdio.h>

#define MAXFILENAME 400

FILE *popen();

char *line = "{ zilch ^}";

char* eqview_cut_prefix = "{ lpile d_eqstyle { ";
char* eqview_cut_suffix = "} } ";

/*
 * Call this routine when you have changed something in
 * the data structure and you want it to get updated on
 * the screen eventually via the update mechanism.
 */

void eqview__Changed(self, changed)
struct eqview *self;
enum changed changed;
{
    if ((int)changed > (int)self->changed)
	self->changed = changed;
}

/*
 * Self-insert, basically
 */

void eqview_Default(self, c)
struct eqview *self;
char c;
{
    static char s[2] = " ";
    long pos, len, added;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);

    if (len > 0) {
	pos += len;
	eqview_SetDotPosition(self, pos);
	eqview_SetDotLength(self, 0);
    }
    s[0] = c;
    added = eq_InsertTokensCarefully(Eq(self), pos, s);
    eqview_SetDotPosition(self, pos+added);
    eqview_Changed(self, EQVIEW_eq);
}

/*
 * Parse-based output
 */

void eqview_WriteC(self)
struct eqview *self;
{
    eq_Parse(Eq(self), stdout, 'c');
}

void eqview_WriteEqn(self)
struct eqview *self;
{
    eq_Parse(Eq(self), stdout, 'e');
}

void eqview_WriteTroff(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    FILE *file = popen("eqn", "w");
    eq_Parse(eqptr, file, 'e');
    pclose(file);
}

void eqview_WriteDvi(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    FILE *file = popen("eqn | troff", "w");
    eq_Parse(eqptr, file, 'e');
    pclose(file);
}

/*
void eqview_PreviewMe(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    FILE *file;

    message_DisplayString(self, 0, "Processing preview request.");
    file = popen("eqn | troff | preview", "w");
    eq_Parse(eqptr, file, 'e');
    pclose(file);
    message_DisplayString(self, 0, "Preview window should appear soon.");
}
*/
/*
void eqview_PrintMe(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    FILE *file;

    message_DisplayString(self, 0, "Processing print request.");
    file = popen("eqn | troff | print -Tdvi", "w");
    eq_Parse(eqptr, file, 'e');
    pclose(file);
    message_DisplayString(self, 0, "Print request submitted; watch console for results.");
}
*/
/*
void eqview_WriteOutFile(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    char name[MAXFILENAME], out[500];
    long code;
    FILE *file;

    code = message_AskForString(self, 0, "Write to file: ", 0, name, sizeof(name));
    if (code < 0) {
	message_DisplayString(self, 0, "Punt!");
	return;
    }
    if (strlen(name) == 0) {
	message_DisplayString(self, 0, "No name specified.");
	return;
    }

    if (file = fopen(name, "w")) {
	eq_Write(eqptr, file, 0L, 1);
	if (self->filename)
	    self->filename = (char *) realloc(self->filename, 1 + sizeof(name));
	else
	    self->filename = (char *) malloc(1 + sizeof(name));
	strcpy(self->filename, name);
	message_DisplayString(self, 0, "Wrote file.");
    }
    else {
	sprintf(out, "Couldn't write to file %s.", name);
	message_DisplayString(self, 0, out);
    }
    fclose(file);
}
*/
/*
void eqview_Save(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    char out[500];
    FILE *file;

    if (self->filename == NULL)
	eqview_WriteOutFile(self);

    else {
	if (file = fopen(self->filename, "w")) {
	    eq_Write(eqptr, file, 0L, 1);
	    message_DisplayString(self, 0, "Wrote file.");
	}
	else {
	    sprintf(out, "Couldn't write to file %s.", self->filename);
	    message_DisplayString(self, 0, out);
	}
	fclose(file);
    }
}
*/
/*
void eqview_ReadInFile(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    char name[MAXFILENAME], out[500];
    long code;
    FILE *file;

    code = message_AskForString(self, 0, "Read file: ", 0, name, sizeof(name));
    if (code < 0) {
	message_DisplayString(self, 0, "Punt!");
	return;
    }
    if (strlen(name) == 0) {
	message_DisplayString(self, 0, "No name specified.");
	return;
    }

    if (file = fopen(name, "r")) {
	eq_Erase(eqptr);
	eq_Read(eqptr, file, 0);
	fclose(file);
	eqview_SetDotPosition(self, 4);
	if (self->filename)
	    self->filename = (char *) realloc(self->filename, 1 + sizeof(name));
	else
	    self->filename = (char *) malloc(1 + sizeof(name));
	strcpy(self->filename, name);
	eqview_Changed(self, EQVIEW_everything);
	message_DisplayString(self, 0, "Done.");
    }
    else {
	sprintf(out, "Couldn't read file %s.", name);
	message_DisplayString(self, 0, out);
    }
}
*/
/*
 * Keyboard commands
 */

/*
 * Move right in a group to position x.
 * i points to a begin group.
 */

long eqview_MoveRight(self, eqptr, i, x)
struct eqview *self;
struct eq *eqptr;
long i, x;
{
    register int n = eq_FindEndGroup(eqptr, i+1), closest = 0, distance = 1000000, j;
    for (j = i+1;  j<=n;  j++) {
	register struct formula *f = eq_Access(eqptr, j);
	if (f->symbol->type == ALIGN)
	    j = eq_FindEndGroup(eqptr, j);
	else if (f->symbol->type==SCRIPT && eq_Access(eqptr,j+1)->symbol->type==BEGIN)
	    j = eq_FindEndGroup(eqptr, j+2);
	else if (f->has_hot_spot) {
	    register int dx = f->hot.x - x;
	    dx = ABS(dx);
	    if (dx < distance) {
		closest = j;
		distance = dx;
	    }
	}
    }
    return closest;
}

void eqview_MoveForward(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    long n = eq_Size(eqptr), i, pos, len;
    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    for (i = pos+len+1;  i<n;  i++) {
	struct formula *f = eq_Access(eqptr, i);
	if (f->has_hot_spot)
	    break;
	else if (f->symbol->type == ALIGN)
	    i = eq_FindEndGroup(eqptr, i);
	else if (f->symbol->type==SCRIPT && eq_Access(eqptr,i+1)->symbol->type==BEGIN)
	    i = eq_FindEndGroup(eqptr, i+2);
	else if (f->symbol->type == BEGIN) {
	    int j = eq_FindBeginGroup(eqptr, i);
	    if (j!=0 && eq_Access(eqptr, j+1)->symbol->type == ALIGN)
		i = eq_FindEndGroup(eqptr, i);
	}
    }
    if (i < n) {
	eqview_SetDotPosition(self, i);
	eqview_SetDotLength(self, 0);
    }
    else {
	eqview_SetDotPosition(self, pos+len);
	eqview_SetDotLength(self, 0);
    }

    eqview_Changed(self, EQVIEW_caret);
}

void eqview_MoveBackward(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    int i, pos, len;
    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    for (i = pos+len-1;  i>0;  i--) {
	struct formula *f = eq_Access(eqptr, i);
	if (f->symbol->type == END) {
	    int j = eq_FindBeginGroup(eqptr, i);
	    if (eq_Access(eqptr, j+1)->symbol->type == ALIGN)
		i = j+1;
	    else if (eq_Access(eqptr, j-1)->symbol->type == SCRIPT)
		i = j-1;
	    else if (f->has_hot_spot)
		break;
	} else if (f->has_hot_spot) {
	    break;
	} else if (f->symbol->type == BEGIN) {
	    int j = eq_FindBeginGroup(eqptr, i);
	    if (j!=0 && eq_Access(eqptr, j+1)->symbol->type == ALIGN)
		i = j+1;
	}
    }
    if (i > 0) {
	eqview_SetDotPosition(self, i);
	eqview_SetDotLength(self, 0);
    }
    else {
	eqview_SetDotPosition(self, pos+len);
	eqview_SetDotLength(self, 0);
    }

    eqview_Changed(self, EQVIEW_caret);
}

void eqview_MoveToBeginning(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    int pos, len;
    /*eqview_MoveBackward(eqptr);*/
    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);

    if (len==0) {
	do
	    pos = eq_FindBeginGroup(eqptr, pos);
	while (eq_Access(eqptr, pos)->transparent);
	pos += 1;
	while (!eq_Access(eqptr, pos) -> has_hot_spot)
	    pos++;
    }
    eqview_SetDotPosition(self, pos);
    eqview_SetDotLength(self, 0);

    eqview_Changed(self, EQVIEW_caret);
}

void eqview_MoveToEnd(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    int pos, len;
    /*eqview_MoveForward(eqptr);*/
    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    if (len==0) {
	pos = eq_FindEndGroup(eqptr, pos);
	while (eq_Access(eqptr, pos)->transparent)
	    pos = eq_FindEndGroup(eqptr, pos+1);
    } else
	pos += len;
    eqview_SetDotPosition(self, pos);
    eqview_SetDotLength(self, 0);

    eqview_Changed(self, EQVIEW_caret);
}

void eqview_DeleteBackward(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    int pos, len, start, stop;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    eqview_SetDotPosition(self, pos+=len);
    eqview_SetDotLength(self, 0);

    start = pos-1,  stop = pos;

    switch (eq_Access(eqptr, start)->symbol->type) {
    case BEGIN:
	if (!eq_Access(eqptr, start)->deletable)
	    return;
	break;
    case END:
	if (eq_Access(eqptr, start)->deletable)
	    start = eq_FindBeginGroup(eqptr, start);
	else
	    return;
	break;
    }
    while (!eq_Access(eqptr, start)->has_hot_spot)
	start -= 1;

    start = eq_DeleteCarefully(eqptr, start, stop);
    if (start)
	eqview_SetDotPosition(self, start);

    eqview_Changed(self, EQVIEW_eq);
}

void eqview_DeleteForward(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    int pos, len, start, stop;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    eqview_SetDotPosition(self, pos+=len);
    eqview_SetDotLength(self, 0);

    start = pos,  stop = pos+1;

    switch (eq_Access(eqptr, stop-1)->symbol->type) {
    case END:
	if (!eq_Access(eqptr, stop-1)->deletable)
	    return;
	break;
    case BEGIN:
	if (eq_Access(eqptr, stop-1)->deletable)
	    stop = eq_FindEndGroup(eqptr, stop)+1;
	else
	    return;
	break;
    }
    while (!eq_Access(eqptr, stop)->has_hot_spot)
	stop += 1;

    /* Inserting a zilch may have changed start */
    start = eq_DeleteCarefully(eqptr, start, stop);
    eqview_SetDotPosition(self, start);	/* in case a zilch was inserted */
    eqview_SetDotLength(self, 0);

    eqview_Changed(self, EQVIEW_eq);
}

void eqview_CR(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    long i, pos, len, added, n = eq_Size(eqptr);;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    i = eq_FindEndGroup(eqptr, pos+len) + 1;
    while (i<n && !eq_Access(eqptr, i) -> has_hot_spot)
	i += 1;
    if (i<n) {
	eqview_SetDotPosition(self, i);
	eqview_SetDotLength(self, 0);
    }
    else if (eq_Access(eqptr, 1)->symbol->type == ALIGN) {
	added = eq_InsertTokens(eqptr, n-1, line);
	eqview_SetDotPosition(self, pos+len+added);
	eqview_SetDotLength(self, 0);
    }

    eqview_Changed(self, EQVIEW_eq);
}

void eqview_MoveUp(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    long i;
    int pos, len;
    struct formula *start;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    i = pos+len;
    start = eq_Access(eqptr, i);

    /* are we in front of a pile? */
    if (eq_Access(eqptr, i)->symbol->type == BEGIN
      && eq_Access(eqptr, i+1)->symbol->type == ALIGN)
    {
	while (!eq_Access(eqptr, ++i) -> has_hot_spot) ;
	eqview_SetDotPosition(self, i);
	eqview_SetDotLength(self, 0);
	return;
    }

    /* are we in front of a scripted atom? */
    else if (eq_Access(eqptr, i+1)->symbol->type == SCRIPT) {
	/* move to top script of following object */
	if (eq_Access(eqptr, i+3)->hot.y < start->hot.y) {
	    while(!eq_Access(eqptr, ++i)->has_hot_spot) ;
	    eqview_SetDotPosition(self, i);
	    eqview_SetDotLength(self, 0);
	    return;
	}
    }

    /* no script or pile to move into; find a higher sibling */
    while ( (i=eq_FindBeginGroup(eqptr, i)) != 0) {
#ifdef notdef
	/* is our parent higher? */
	if (eq_Access(eqptr, i)->hot.y < start->hot.y)
	    break;
#endif /* notdef */
	if (eq_Access(eqptr, i-1)->symbol->type == SCRIPT) {
	    /* find sibling script */
	    if (eq_Access(eqptr, i-2)->symbol->type == END) {
		i = eq_FindBeginGroup(eqptr, i-2);
		if (eq_Access(eqptr, i-1)->symbol->type == SCRIPT)
		    break;
	    }
	} else {
	    /* find sibling in pile */
	    enum type t;
	    while ((t=eq_Access(eqptr, --i)->symbol->type) != END && t != BEGIN) ;
	    if (t==END) {
		i = eq_FindBeginGroup(eqptr, i);
		if (eq_Access(eqptr, i-1)->symbol->type != SCRIPT
		  && eq_Access(eqptr, i)->hot.y < start->hot.y)
		    break;
	    } else
		i += 1;
	}
    }
    if (i != 0) {
	eqview_SetDotPosition(self, eqview_MoveRight(self, eqptr, i, start->hot.x));
	eqview_SetDotLength(self, 0);
    }

    eqview_Changed(self, EQVIEW_caret);
}

void eqview_MoveDown(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    long i;
    int pos, len, j;
    struct formula *start;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    i = pos+len;
    start = eq_Access(eqptr, i);

    /* are we in front of a pile? */
    if (eq_Access(eqptr, i)->symbol->type == BEGIN
      && eq_Access(eqptr, i+1)->symbol->type == ALIGN)
    {
	/* find last of pile */
	enum type t;
	i = eq_FindEndGroup(eqptr, i+1);
	while ((t=eq_Access(eqptr, --i)->symbol->type) != BEGIN && t != END) ;
	if (t==END) {
	    i = eq_FindBeginGroup(eqptr, i);
	    while (!eq_Access(eqptr, ++i)->has_hot_spot) ;
	    eqview_SetDotPosition(self, i);
	    eqview_SetDotLength(self, 0);
	    return;
	}
    }

    /* are we in front of a scripted atom? */	
    else if (eq_Access(eqptr, i+1)->symbol->type == SCRIPT) {
	/* find last script */
	int j = i;
	do {
	    i = j;
	    j = eq_FindEndGroup(eqptr, i+3);
	} while (eq_Access(eqptr, j+1)->symbol->type == SCRIPT);
	if (eq_Access(eqptr, i+3)->hot.y > start->hot.y) {
	    while(!eq_Access(eqptr, ++i)->has_hot_spot) ;
	    eqview_SetDotPosition(self, i);
	    eqview_SetDotLength(self, 0);
	    return;
	}
    }

    /* find a lower sibling */
    j = i;
    while ( (j=eq_FindBeginGroup(eqptr, j)) != 0) {
	i = eq_FindEndGroup(eqptr, i);
#ifdef notdef
	/* is our parent lower? */
	if (eq_Access(eqptr, j)->hot.y > start->hot.y) {
	    i = j;
	    break;
	}
#endif /* notdef */
	if (eq_Access(eqptr, j-1)->symbol->type == SCRIPT) {
	    /* find sibling script */
	    if (eq_Access(eqptr, i+1)->symbol->type == SCRIPT) {
		i = i+2;
		break;
	    } else
		i += 1;
	} else {
	    /* find sibling in pile */
	    enum type t;
	    while ((t=eq_Access(eqptr, ++i)->symbol->type) != BEGIN && t != END) ;
	    if (t==BEGIN) {
		if (eq_Access(eqptr, i-1)->symbol->type != SCRIPT
		  && eq_Access(eqptr, i)->hot.y < start->hot.y)
		    break;
	    }
	}
    }
    if (j != 0) {
	eqview_SetDotPosition(self, eqview_MoveRight(self, eqptr, i, start->hot.x));
	eqview_SetDotLength(self, 0);
    }
    eqview_Changed(self, EQVIEW_caret);
}

/*
 * Diacritical marks
 */

void eqview_Bar(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    long pos, len, start, stop;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    if (len==0) {
	stop = pos;
	eqview_MoveBackward(self /*eqptr*/);
	start = eqview_GetDotPosition(self);
    } else {
	start = pos;
	stop = pos+len;
    }
    eqview_SetDotPosition(self, stop);
    eqview_SetDotLength(self, 0);
    eq_InsertTokens(eqptr, stop, "} above bar");
    eq_InsertTokens(eqptr, start, "{ord");

    eqview_Changed(self, EQVIEW_eq);
}

void eqview_Dot(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    long n, pos, len, added = 0;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    eqview_SetDotLength(self, 0);
    eqview_SetDotPosition(self, pos = (pos + len));

    if ((n = eq_DoScript(eqptr, pos, ABOVE, "above { dot }")) == pos) {
	message_DisplayString(self, 0, "Bad command.");
	return;
    }
    else if (n == pos+3) {
	eqview_SetDotPosition(self, n+1);
    }
    else {
	added = eq_InsertTokensCarefully(eqptr, n, "dot");
	eqview_SetDotPosition(self, n+added+1);
    }

    eqview_Changed(self, EQVIEW_eq);
}

void eqview_Prime(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    long n, pos, len, added = 0;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    eqview_SetDotLength(self, 0);
    eqview_SetDotPosition(self, pos = (pos + len));

    if ((n=eq_DoScript(eqptr, pos, SUP, "sup { ' }")) == pos) {
	message_DisplayString(self, 0, "Bad command.");
	return;
    }
    else if (n == pos+3) {
	eqview_SetDotPosition(self, n+1);
    }
    else {
	added = eq_InsertTokensCarefully(eqptr, n, "'");
	eqview_SetDotPosition(self, n+added+1);
    }

    eqview_Changed(self, EQVIEW_eq);
}

/*
 * Parens
 */

#ifdef notdef
void eqview_Open(self, c)
struct eqview *self;
char c;
{
    register struct eq *eqptr = Eq(self);
    long pos, len, added;
    char buf[20];

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    eqview_SetDotPosition(self, pos+len);
    eqview_SetDotLength(self, 0);
    if (c=='{')
	buf = "lbrace";
    sprintf(buf, "%c", c);
    added = eq_InsertTokensCarefully(eqptr, pos+len, buf);
    eqview_SetDotPosition(self, pos+added);

    eqview_Changed(self, EQVIEW_eq);
}
#endif /* notdef */

void eqview_Close(self, c)
struct eqview *self;
char c;
{
    register struct eq *eqptr = Eq(self);
    long pos, len, added, level = 0, i, matched = 0;
    struct formula *f;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    pos += len;

    for (i=pos-1; i>=0 && level>=0; i--) {
	f = eq_Access(eqptr, i);
	if (f->symbol->type == END)
	    i = eq_FindBeginGroup(eqptr, i);
	else if (f->symbol->type == BEGIN)
	    break;
	else if (f->symbol->genre == CLOSE)
	    level++;
	else if (f->symbol->genre == OPEN && f->symbol != root) {	
	    if (level != 0)
		level--;
	    else {
		matched = 1;
		break;
	    }
#ifdef notdef
		f = eq_Access(eqptr, i+1);	/* XXX - following necessary? */
		if (! (f->symbol->type==BEGIN && f->transparent) )
		    break;
#endif /* notdef */
	}
    }
    if (matched) {
	char buf[20];
	if (c=='}')
	    sprintf(buf, "} rbrace");
	else
	    sprintf(buf, "} %c", c);
	eq_InsertTokens(eqptr, pos, buf);
	eq_InsertTokens(eqptr, i+1, "{");
	eqview_SetDotPosition(self, i);
	eqview_SetDotLength(self, pos-i+3);
    } else {
	char buf[20];
	if (c=='}')
	    sprintf(buf, "rbrace");
	else
	    sprintf(buf, "%c", c);
	if (len>0) {
	    eqview_SetDotPosition(self, pos);
	    eqview_SetDotLength(self, 0);
	}
	added = eq_InsertTokensCarefully(eqptr, pos, buf);
	eqview_SetDotPosition(self, pos+added);
    }

    eqview_Changed(self, EQVIEW_eq);
}

/*
 * Cut, copy, paste.
 *
 * The cut buffer is set up to contain an autonomous equation
 * in case we are pasting into something other than an eq
 * object.  For this we use eqview_cut_prefix before and
 * eqview_cut_suffix after the sub-equation we put into 
 * the cut buffer.
 */

void eqview_Cut(self)
struct eqview *self;
{
    struct eq *eqptr = Eq(self);
    long pos, len;
    FILE *cutFile;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);

    cutFile = im_ToCutBuffer(view_GetIM((struct view *) self));
    fprintf(cutFile, "\\begindata{%s,%d}\n", class_GetTypeName(eqptr), dataobject_UniqueID(self));
    fprintf(cutFile, "%s", eqview_cut_prefix);
    eqptr->header.dataobject.writeID = im_GetWriteID();
    eq_WriteFILE(eqptr, cutFile, pos, pos+len, ' ');
    fprintf(cutFile, "%s", eqview_cut_suffix);
    fprintf(cutFile, "\\enddata{%s,%d}\n", class_GetTypeName(eqptr), dataobject_UniqueID(self));
    im_CloseToCutBuffer(view_GetIM((struct view *) self), cutFile);
    eq_DeleteCarefully(eqptr, pos, pos+len);
    eqview_SetDotLength(self, 0);

    eqview_Changed(self, EQVIEW_eq);
}

void eqview_Copy(self)
struct eqview *self;
{
    struct eq *eqptr = Eq(self);
    long pos, len;
    FILE *cutFile;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);

    cutFile = im_ToCutBuffer(view_GetIM((struct view *) self));
    fprintf(cutFile, "\\begindata{%s,%d}\n", class_GetTypeName(eqptr), dataobject_UniqueID(self));
    fprintf(cutFile, "%s", eqview_cut_prefix);
    eqptr->header.dataobject.writeID = im_GetWriteID();
    eq_WriteFILE(eqptr, cutFile, pos, pos+len, ' ');
    fprintf(cutFile, "%s", eqview_cut_suffix);
    fprintf(cutFile, "\\enddata{%s,%d}\n", class_GetTypeName(eqptr), dataobject_UniqueID(self));
    im_CloseToCutBuffer(view_GetIM((struct view *) self), cutFile);
}

/*
 * Paste takes account of zilches.
 * Caution: this is a near copy of eq_InsertTokensCarefully,
 * but the two are inconsistent wrt scripted zilches.
 */

void eqview_Paste(self)
struct eqview *self;
{
    struct eq *eqptr = Eq(self);
    register int i;
    long pos, len;
    FILE *pasteFile;
    long ct;
    struct formula *f;

    i = 0;
    ct = im_Argument(self->header.view.imPtr);

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    pos += len;
    len = 0;

    if ((f = eq_Access(eqptr, pos)) != NULL && f->symbol == zilch)
	eq_Delete(eqptr, pos);
    if ((f = eq_Access(eqptr, pos-1)) != NULL && f->symbol ==  zilch)
	eq_Delete(eqptr, --pos);

    while(i < ct) {
	char s[256], name[64];
	long inid;

	pasteFile = im_FromCutBuffer(view_GetIM((struct view *) self));
	while (fgets(s, 256, pasteFile) != NULL) {
	    if (*s == '\\') {
		if (strncmp(s, "\\enddata{", 9) == 0) {
		    sscanf(s, "\\enddata{%s,%ld}\n", name, &inid);
		    break;
		}
		if (strncmp(s, "\\begindata{", 11) == 0)
		    sscanf(s, "\\begindata{%s,%ld}\n", name, &inid);
	    }
	    len = eq_ReadFILE(eqptr, pasteFile, pos);
	    break;
	}
	i++;
	im_CloseFromCutBuffer(view_GetIM((struct view *) self), pasteFile);
    }


    /* The cut buffer is set up to contain an autonomous equation
      in case we are pasting into something other than an eq
      object.

      When we paste into eq, we remove the additional crud:
      "{ lpile d_eqstyle { " before and "} } " after. */

    /* first confirm the crud is there */
    if ((len >= 5) &&
	 (eq_Access(eqptr, pos)->symbol->type == BEGIN) &&
	 (eq_Access(eqptr, pos + 1)->symbol->type == ALIGN) &&
	 (eq_Access(eqptr, pos + 2)->symbol->type == EQSTYLE) &&
	 (eq_Access(eqptr, pos + 3)->symbol->type == BEGIN)) {
	for (i = 3; i >= 0; i --) {
	    eq_Delete(eqptr, pos + i);
	    len -= 1;
	}
    }

    /* eliminate unmatched groups */
    i = eq_FindEndGroup(eqptr, pos);
    while (i < pos+len) {
	eq_Delete(eqptr, i);
	len -= 1;
	i = eq_FindEndGroup(eqptr, i);
    }
    i = eq_FindBeginGroup(eqptr, pos+len);
    while (i >= pos) {
	eq_Delete(eqptr, i);
	len -= 1;
	i = eq_FindBeginGroup(eqptr, i);
    }
    eqview_SetDotPosition(self, pos);
    eqview_SetDotLength(self, len);

    eqview_Changed(self, EQVIEW_eq);
}

/*
 * Perhaps this should go away
 */

void eqview_Exit()
{
    exit(0);
}

/*
 * For debugging
 */

void eqview_DumpAndWrite(self)
struct eqview *self;
{
    FILE *file;
    struct eq *eqptr = Eq(self);

    if (file = fopen("new.eq", "w"))
	eq_Write(eqptr, file, 0L, 1);
    else
	message_DisplayString(self, 0, "Couldn't write to file new.eq.");
    fclose(file);
    eq_Dump(eqptr, "/tmp/eq");
}

/*
 * Online documentation
 */

void eqview_doc()
{
    printf("not yet\n");
}

/*
 * Special symbols prefixed with ESC
 */

/*
 * Called as a result of eqview_Special
 */

void eqview_DoSpecial(self, s)
struct eqview *self;
char *s;
{
    long pos, len, added;

    if (strcmp(s, "}") == 0)
	eqview_Close(self, '}');
    else {
	pos = eqview_GetDotPosition(self);
	len = eqview_GetDotLength(self);

	pos += len;
	if (len>0) {
	    eqview_SetDotPosition(self, pos);
	    eqview_SetDotLength(self, 0);
	}

	if (strcmp(s, "bar") == 0)
	    eqview_Bar(self);
	else if (strcmp(s, "dot") == 0)
	    eqview_Dot(self);
	else if (strcmp(s, "prime") == 0)
	    eqview_Prime(self);
	else {
	    if (strcmp(s, "{") == 0)
		s = "lbrace";
	    added = eq_InsertTokensCarefully(Eq(self), pos, s);
	    eqview_SetDotPosition(self, pos+added);
	    eqview_Changed(self, EQVIEW_eq);
	}
    }
}

void eqview_Special(self, c)
struct eqview *self;
char c;
{
    char name[100];
    long code;

    code = message_AskForString(self, 0, "Symbol? ", 0, name, sizeof(name));
    if (code < 0) {
	message_DisplayString(self, 0, "Punt!");
	return;
    }
    if (strlen(name) == 0) {
	message_DisplayString(self, 0, "No name specified.");
	return;
    }
    eqview_DoSpecial(self, name);
    message_DisplayString(self, 0, "Done.");
}

/*
void eqview_Punt(self)
struct eqview *self;
{
    message_DisplayString(self, 0, "Bad command.");
    return;
}
*/

/*
 * Scripts
 */

void eqview_SuperScript(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    long pos, len, n;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    eqview_SetDotLength(self, 0);
    eqview_SetDotPosition(self, pos = (pos + len));

    if (n = eq_DoScript(eqptr, pos, SUP, "sup { zilch ^}"))
	eqview_SetDotPosition(self, n);
    eqview_Changed(self, EQVIEW_eq);
}

void eqview_SubScript(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    long pos, len, n;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    eqview_SetDotLength(self, 0);
    eqview_SetDotPosition(self, pos = (pos + len));

    if (n = eq_DoScript(eqptr, pos, SUB, "sub { zilch ^}"))
	eqview_SetDotPosition(self, n);
    eqview_Changed(self, EQVIEW_eq);
}

void eqview_AboveScript(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    long pos, len, n;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    eqview_SetDotLength(self, 0);
    eqview_SetDotPosition(self, pos = (pos + len));

    if (n = eq_DoScript(eqptr, pos, ABOVE, "above { zilch ^}"))
	eqview_SetDotPosition(self, n);
    eqview_Changed(self, EQVIEW_eq);
}

void eqview_BelowScript(self)
struct eqview *self;
{
    register struct eq *eqptr = Eq(self);
    long pos, len, n;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);
    eqview_SetDotLength(self, 0);
    eqview_SetDotPosition(self, pos = (pos + len));

    if (n = eq_DoScript(eqptr, pos, BELOW, "below { zilch ^}"))
	eqview_SetDotPosition(self, n);
    eqview_Changed(self, EQVIEW_eq);
}

/*
 * Special symbols that require a string of symbols
 * There should be a third parameter to keymap-called routines!
 */

void eqview_String(self, s)
struct eqview *self;
char *s;
{
    long pos, len, added;

    pos = eqview_GetDotPosition(self);
    len = eqview_GetDotLength(self);

    if (len > 0) {
	pos += len;
	eqview_SetDotPosition(self, pos);
	eqview_SetDotLength(self, 0);
    }

    added = eq_InsertTokensCarefully(Eq(self), pos, s);
    eqview_SetDotPosition(self, pos+added);
    eqview_Changed(self, EQVIEW_eq);
}

void eqview_Root(self)
struct eqview *self;
{
    long pos;

    eqview_String(self, "root { ^zilch }");
    pos = eqview_GetDotPosition(self);
    eqview_SetDotPosition(self, pos-2);
}

void eqview_Fraction(self)
struct eqview *self;
{
    long pos;

    eqview_String(self, "{ cpile { zilch ^} over { zilch } }");
    pos = eqview_GetDotPosition(self);
    eqview_SetDotPosition(self, pos-7);
}

void eqview_lbrace(self)
struct eqview *self;
{
    eqview_String(self, "lbrace");
}

static struct bind_Description eqviewBindings[]={
    /* FILE menu card */
/*    {"eqview-write-out-file", "\030\027",0,"File,Save As~21",0,0,eqview_WriteOutFile, "Write equation to specified file."}, */

/*    {"eqview-read-in-file", "\030\022",0,"File,Read File~31",0,0,eqview_ReadInFile, "Read in specified file."}, */

    {"eqview-write-c", "\032c",0,"File,C~41",0,0,eqview_WriteC, "Write equation in C format."},
    {"eqview-write-eqn", "\032e",0,"File,EQN~42",0,0,eqview_WriteEqn, "Write equation in EQN format."},
    {"eqview-write-troff", "\032t",0,"File,TROFF~43",0,0,eqview_WriteTroff, "Write equation in troff format."},
    {"eqview-write-dvi", "\032d",0,"File,DVI~44",0,0,eqview_WriteDvi, "Write equation in DVI format."},

/*    {"eqview-preview-me", "\032v",0,"File,Preview~51",0,0,eqview_PreviewMe, "Preview equation."}, */
/*    {"eqview-print-me", "\032p",0,"File,Print~52",0,0,eqview_PrintMe, "Print equation."}, */

    /* FORMULA menu card */
    {"eqview-root", "\022",0,"Formulae,Root~11",0,0,eqview_Root, "Insert a root."},
    {"eqview-root", "\033root\n",0,0},

    {"eqview-fraction", "/",0,"Formulae,Fraction~21",0,0,eqview_Fraction,"Insert a fraction."},

    {"eqview-superscript", "^",0,"Formulae,Superscript~31",0,0,eqview_SuperScript, "Insert a superscript."},
    {"eqview-subscript", "_",0,"Formulae,Subscript~32",0,0,eqview_SubScript, "Insert a subscript."},

    {"eqview-above-script", "!",0,"Formulae,Above~41",0,0,eqview_AboveScript, "Insert an abovescript."},
    {"eqview-below-script", "#",0,"Formulae,Below~42",0,0,eqview_BelowScript, "Insert a belowscript."},

    {"eqview-special", "\033a",0,"Formulae,Insert Symbol...~51",0,0,eqview_Special, "Special symbol (dialog)."},

    /* FRONT menu card */
    {"eqview-cut", "\027",0,NULL,0,0,eqview_Cut, "Cut~11"},
    {"eqview-copy", "\033w",0,NULL,0,0,eqview_Copy, "Copy~12"},
    {"eqview-paste", "\031",0,"Paste~10",0,0,eqview_Paste, "Paste~13"},

/*    {"eqview-save", "\030\023",0,"Save~31",0,0,eqview_Save, "Save equation in default file."}, */

/*    {"eqview-exit", "\030\003",0,"Quit~50",0,0,eqview_Exit, "Exit eq."}, */
/*    {"eqview-exit", EOF,0,0}, */

    /* Debugging commands */
    {"eqview-dump-and-write", "\030\030",0,NULL,0,0,eqview_DumpAndWrite, "Dump equation and write it to tmp."},

    /* Special commands */
/*    {"eqview-punt", "\007",0,NULL,0,0,eqview_Punt, "Abort current command."}, */
/*    {"eqview-symbol-documentation", "\023",0,"List symbols~30",0,0,eqview_doc, "Documentation for symbols."}, */

    /* Positioning commands */
    {"eqview-move-forward", "\006",0,NULL,0,0,eqview_MoveForward, "Move forward."},
    {"eqview-move-backward", "\002",0,NULL,0,0,eqview_MoveBackward, "Move backward."},
    {"eqview-move-to-beginning", "\001",0,NULL,0,0,eqview_MoveToBeginning, "Move to the beginning."},
    {"eqview-move-to-end", "\005",0,NULL,0,0,eqview_MoveToEnd, "Move to the end."},
    {"eqview-move-up", "\020",0,NULL,0,0,eqview_MoveUp, "Move up."},
    {"eqview-move-down", "\016",0,NULL,0,0,eqview_MoveDown, "Move down"},
    {"eqview-carriage-return", "\015",0,NULL,0,0,eqview_CR, "Handle carriage returns."},

    /* Other commands */
    {"eqview-delete-forward", "\004",0,NULL,0,0,eqview_DeleteForward, "Delete forward from cursor."},
    {"eqview-delete-backward", "\010",0,NULL,0,0,eqview_DeleteBackward, "Delete backward from cursor."},
    {"eqview-delete-backward", "\177",0},
    /*{"eqview-undelete", "\025",0,NULL,0,0,eqview_UnDelete, "Undelete."}, */

    {"eqview-left-brace", "{",0,NULL,0,0,eqview_lbrace, "Insert a left brace."},
    {"eqview-close-paren", ")",')',NULL,0,0,eqview_Close, "Insert a close paren."},
    {"eqview-close-bracket", "]",']',NULL,0,0,eqview_Close, "Insert a close bracket."},
    {"eqview-close-brace", "}",'}',NULL,0,0,eqview_Close, "Insert a close brace."},

    {"eqview-prime", "\'",0,NULL,0,0,eqview_Prime,"Insert a prime."}, 
    NULL
};

static struct bind_Description eqviewCutBindings[]={
    /* FILE menu card */
/*    {"eqview-write-out-file", NULL,0,"File,Save As~21",0,0}, */

    {"eqview-read-in-file", NULL,0,"File,Read File~31",0,0},

    {"eqview-write-c", NULL,0,"File,C~41",0,0},
    {"eqview-write-eqn", NULL,0,"File,EQN~42",0,0},
    {"eqview-write-troff", NULL,0,"File,TROFF~43",0,0},
    {"eqview-write-dvi", NULL,0,"File,DVI~44",0,0},

/*    {"eqview-preview-me", NULL,0,"File,Preview~51",0,0}, */
/*    {"eqview-print-me", NULL,0,"File,Print~52",0,0}, */

    /* FRONT menu card */
    {"eqview-cut", NULL,0,"Cut~10",0,0},
    {"eqview-copy", NULL,0,"Copy~11",0,0},

/*    {"eqview-save", NULL,0,"Save~31",0,0}, */

/*    {"eqview-exit", NULL,0,"Quit~50",0,0}, */

    NULL
};

struct keymap *eqview_InitKeyMap(classInfo, eqviewMenus, eqviewCutMenus)
struct eqview_classinfo *classInfo;
struct menulist **eqviewMenus, **eqviewCutMenus;
{
    struct keymap *keymap = keymap_New();
    char str[2];
    register int i;
    struct proctable_Entry *def;

    if (eqviewMenus != NULL)
	*eqviewMenus = menulist_New();
    if(eqviewCutMenus!=NULL)
	*eqviewCutMenus=menulist_New();

    def=proctable_DefineProc("eqview-default", eqview_Default, classInfo, NULL, "Insert a character.");

    str[0] = ' ';
    str[1] = '\0';

    for (i = 32; i<127; i++) {
	keymap_BindToKey(keymap, str, def, i);
	str[0]++;
    }
    bind_BindList(eqviewBindings, keymap, *eqviewMenus, classInfo);
    bind_BindList(eqviewCutBindings, NULL, *eqviewCutMenus, classInfo);

    return keymap;
}
