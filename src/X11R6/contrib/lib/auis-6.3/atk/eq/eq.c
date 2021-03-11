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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/eq/RCS/eq.c,v 2.10 1992/12/15 21:32:53 rr2b R6tape $";
#endif


 

/*
 * eq.c
 * This module handles the eq data object.
 */


#include <class.h>
#include "eq.eh"

#include <dataobj.ih>
#include <mark.ih>

#define SIZE 10

static char *init_string = "{ lpile d_eqstyle { zilch ^} }";

/* Create a new equation */
boolean eq__InitializeObject(classID, self)
struct classheader *classID;
struct eq *self;
{
    register struct formula *f = (struct formula *) malloc(SIZE*sizeof(struct formula));

    self->f = f;
    self->p1 = 0;
    self->gap = SIZE;
    self->p2 = 0;
    self->markList = NULL;
    self->fence = eq_CreateMark(self, 0, 0);
    mark_SetStyle(self->fence, TRUE, FALSE);
    eq_InsertTokensCarefully(self, 0, init_string);

    return TRUE;
}

void eq__FinalizeObject(classID, self)
struct classheader *classID;
struct eq *self;
{
    struct mark *mark;

    free(self->f);
    if (self->markList != NULL)  {
	for (mark = self->markList; mark != NULL; mark = mark_GetNext(mark))
	    mark_SetObjectFree(mark, TRUE);
    }
    mark_Destroy(self->fence);
}

/* Insert a formula in an equation at position n  */
void eq__Insert(self, pos, f)
struct eq *self;
long pos;
struct formula *f;
{
    if (pos > self->p1 + self->p2)
	pos = self->p1 + self->p2;

    if (self->gap == 0) {
	int size = (self->p1 + self->gap + self->p2) * 3 / 2;
	self->f = (struct formula *) realloc(self->f, size*sizeof(struct formula));
	self->p1 = self->p1 + self->p2;
	self->p2 = 0;
	self->gap = size - self->p1;
    }

    if (self->p1 != pos) {
	register int i, gap = self->gap;
	register struct formula *newf = self->f;
	for (i=self->p1-1; i>=pos; i--)
	    newf[i+gap] = newf[i];
	for (i=self->p1; i<pos; i++)
	    newf[i] = newf[i+gap];
	self->p2 = self->p2 + self->p1 - pos;
	self->p1 = pos;
    }

    self->f[pos] = *f;
    self->p1 += 1;
    self->gap -= 1;
    mark_UpdateMarks(self->markList, pos , 1);
}

/* Insert tokens */

long eq__InsertTokens(self, pos, s)
struct eq *self;
long pos;
char *s;
{
    char buf[100], *p;
    int inserted = 0;
    static struct formula f;

    while (*s) {
	while (*s==' ')
	    s++;
	p = buf;
	while (*s!=' ' && *s!='\0')
	    *p++ = *s++;
	if (p != buf) {
	    *p = '\0';
	    p = buf;
	    if (p[0]=='#' && p[1]!='\0') {
		p += 1;
	    }
	    else if (p[0]=='^' && p[1]!='\0')
		p += 1;
	    f.symbol = eq_Lookup(p);
	    if (!f.symbol) {
		fprintf(stderr, "Unknown symbol '%s'\n", p);
		return 0;
	    }
	    else {
		eq_Insert(self, (pos + inserted), &f);
		inserted += 1;
	    }
	}
    }

    return inserted;
}

/*
 * Take account of zilches when inserting.
 * Caution: a near copy of this is in eqview_Paste, but
 * the two are inconsistent wrt scripted zilches.
 */

long eq__InsertTokensCarefully(self, pos, s)
struct eq *self;
long pos;
char *s;
{
    long evenup = 0, inserted = 0;
    short zilch_removed = 0;
    struct formula *f;

    if ((f = eq_Access(self, pos)) != NULL && f->symbol == zilch) {
	eq_Delete(self, pos);
	zilch_removed = 1;
    }
    if ((f = eq_Access(self, pos-1)) != NULL && f->symbol == zilch) {
	eq_Delete(self, --pos);
	zilch_removed = 1;
	evenup = 1;
    }
/* the following may not handle replacing a scripted zilch properly */
    if (!(inserted = eq_InsertTokens(self, pos, s)) && (zilch_removed))
	inserted = eq_InsertTokens(self, pos, "zilch");

    return (inserted-evenup);
}

/* Delete formula at position n from an equation */
void eq__Delete(self, pos)
struct eq *self;
long pos;
{
    if (pos == self->p1 - 1) {
	self->p1 -= 1;
    } else if (pos == self->p1) {
	self->p2 -= 1;
    } else {
	register int i, gap = self->gap;
	register struct formula *f = self->f;
	for (i=self->p1-1; i>=pos; i--)
	    f[i+gap] = f[i];
	for (i=self->p1; i<pos; i++)
	    f[i] = f[i+gap];
	self->p2 = self->p2 + self->p1 - pos - 1;
	self->p1 = pos;
    }

    self->gap += 1;
    mark_UpdateMarks(self->markList, pos , -1);
}

/*
 * Delete an item but preserve structure
 */

long eq__DeleteCarefully(self, start, stop)
struct eq *self;
long start, stop;
{
    long was_zilch;
    register int i, j;
    enum type type;
    struct formula *f;

    /* remember whether we deleted a zilch */
    f = eq_Access(self, stop-1);
    was_zilch = (f != NULL && f->symbol == zilch);

    /* eliminate unmatched groups */
    i = start;
    j = eq_FindEndGroup(self, start);
    while (j < stop) {
	i = eq_FindBeginGroup(self, i);
	eq_Delete(self, j);
	eq_Delete(self, i);
	start -= 1,  stop -= 2,  j -= 1;
	j = eq_FindEndGroup(self, j);
    }
    i = eq_FindBeginGroup(self, stop);
    j = stop;
    while (i >= start) {
	j = eq_FindEndGroup(self, j);
	eq_Delete(self, j);
	i = eq_FindBeginGroup(self, i);
    }

    /* do the delete */
    for (i=start; i<stop; i++)
	eq_Delete(self, start);

    /* did we just delete the last item, a zilch, in a group? */
    if (eq_Access(self, start)->symbol->type==END
      && ((type=eq_Access(self, start-1)->symbol->type)==BEGIN
      || type==ALIGN || type==EQSTYLE)   &&  was_zilch) {
	/* find an enclosing group to delete */
	register int i, b=start, e;
	while (eq_Access(self, --b)->symbol->type != BEGIN) ;
	while (b!=0 && !eq_Access(self, b)->has_hot_spot
	  && eq_Access(self, b-1)->symbol->type!=SCRIPT)
	    b = eq_FindBeginGroup(self, b);
	if (b!=0) {
	    e = eq_FindEndGroup(self, b+1);
	    for (i=b+1;  i<e;  i++) {
		register struct formula *f = eq_Access(self, i);
		if (f->symbol->type!=END && f->has_hot_spot
		  && f->symbol != zilch)
		    break;
	    }
	    if (i==e) {
		start = b,  stop = e+1;
		if (eq_Access(self, b-1)->symbol->type==SCRIPT)
		    start -= 1;
		for (i=start;  i<stop;  i++)
		    eq_Delete(self, start);
	    }
	}
    }
    
    /* Check again */
    if (eq_Access(self, start)->symbol->type==END
      && ((type=eq_Access(self, start-1)->symbol->type)==BEGIN
      || type==ALIGN || type==EQSTYLE)) {
	i = eq_FindBeginGroup(self, start);
	if (!eq_Access(self, i)->deletable)
	    eq_InsertTokens(self, start, "zilch");
    }
    return start;
}

/*
 * DoScript.
 * Looks for specified script; if found, sets dot to end of
 * script and returns position of end of script.  If not found,
 * inserts string and returns new position.
 */

long eq__DoScript(self, pos, script, string)
struct eq *self;
long pos;
enum script script;
char *string;
{
    long i, added, found = -1;

    for (i=pos-1; ; i--) {
	struct formula *f = eq_Access(self, i);
	if (f->symbol == zilch)
	    return pos;
	if (f->symbol->type==END)
	    i = eq_FindBeginGroup(self, i);
	else if (f->symbol->type==SCRIPT) {
	    if (((enum script)f->symbol->what)==script) {
		found = i;
		break;
	    }
	}
	else if (f->symbol->type==BEGIN) {
	    return pos; 
	}
	else {
	    break;
	}
    }
    if (found >= 0) {
	return (eq_FindEndGroup(self, found));
    } else {
	added = eq_InsertTokens(self, pos, string);
	return pos+added-1;
    }
}

struct mark *eq__CreateMark(self, pos, length)
struct eq *self;
long pos;
long length;  {
    struct mark *mark;
    
    mark = mark_New();
    mark_SetPos(mark, pos);
    mark_SetLength(mark, length);
    mark_SetNext(mark, self->markList);
    mark_SetObject(mark, self);
    self->markList = mark;
    return mark;
}

void eq__RemoveMark(self, mark)
struct eq *self;
struct mark *mark;  {
    struct mark *mp;
    struct mark *tp;

    if (mark == NULL) return;
    
    for (mp = self->markList, tp = NULL; mp != NULL && mp != mark; tp = mp, mp = mark_GetNext(mp));
    
    if (mp != NULL)  {
	if (tp == NULL)  {
/* 	    First element on the list
 */	    
	    self->markList = mark_GetNext(mp);
	}
	else  {
	    mark_SetNext(tp, mark_GetNext(mp));
	}
    }
}

#ifdef notyet
/* Maintain the current cursor geometry */
void eq_SetCursor(self, left, top, width, height)
struct eq *self;
long left, top, width, height;
{
    self->cursor.left = left;
    self->cursor.top = top;
    self->cursor.width = width;
    self->cursor.height = height;
}

void eq_GetCursor(self, leftp, topp, widthp, heightp)
struct eq *self;
long *leftp, *topp, *widthp, *heightp;
{
    *leftp = self->cursor.left;
    *topp = self->cursor.top;
    *widthp = self->cursor.width;
    *heightp = self->cursor.height;
}
#endif /* notyet */

/* Forget a whole equation */
void eq__Erase(self)
struct eq *self;
{
    self->gap = self->p1 + self->gap + self->p2;
    self->p1 = 0;
    self->p2 = 0;
}

/* Get formula n from an equation */
struct formula *eq__Access(self, n)
struct eq *self;
long n;
{
    if (n < 0) return 0;
    if (n < self->p1)
	return &(self->f[n]);
    else if (n < self->p1+self->p2)
	return &(self->f[n+self->gap]);
    else
	return 0;
}

/* Determine the number of formulas in an equation */
long eq__Size(self)
struct eq *self;
{
    return self->p1 + self->p2;
}

/* Given an equation and a formula, return the next formula. */
struct formula *eq__NextFormula(self, f)
struct eq *self;
struct formula *f;
{
    register int n, p1 = self->p1, gap = self->gap, p2 = self->p2;

    f += 1;
    n = f - self->f;
    if (n == p1)
	return f + gap;
    else if (n < p1 + gap + p2)
	return f;
    else
	return 0;
}

/* Convert to printable representation.  Returns length in bytes, including trailing null. */
long eq__GetTokens(self, startp, stop, string, size)
struct eq *self;
long *startp, stop;
char *string;
long size;
{
    register char *s = string;
    register int i, len;
    for (i= *startp; i<stop; i++) {
	register struct formula *f = eq_Access(self, i);
	len = strlen(f->symbol->name) + 1;
	if (len > size)
	    break;
	sprintf(s, "%s ", f->symbol->name);
	s += len,  size -= len;
    }
    *startp = i;
    if (s>string)
	s[-1] = '\0';
    else
	*s++ = '\0';
    return s - string;
}

/*
 * Find end of group i is in.    { . . . }
 * x's at right find the }.	   x x x x
 */

long eq__FindEndGroup(self, i)
struct eq *self;
long i;
{
    register int j, level = 0;
    for (j=i; ; j++) {
	switch (eq_Access(self, j)->symbol->type) {
	case BEGIN:
	    level++;
	    break;
	case END:
	    if (level==0)
		return j;
	    level--;
	    break;
	}
    }
}


/*
 * Find beginning of group i is in.	{ . . . }
 * x's at right find the {.		  x x x x
 */

long eq__FindBeginGroup(self, i)
struct eq *self;
long i;
{
    register int j, level = 0;
    for (j=i-1; ; j--) {
	switch (eq_Access(self, j)->symbol->type) {
	case END:
	    level++;
	    break;
	case BEGIN:
	    if (level==0)
		return j;
	    level--;
	    break;
	}
    }
}


/*
 * Find siblings.  Assumes i points to a begin group.
 * Returns pointer to a begin group.
 */

long eq__FindLeftSibling(self, i)
struct eq *self;
long i;
{
    register int j;
    if (i==0)
	return -1;
    for (j=i-1; ; j--) {
	switch (eq_Access(self, j)->symbol->type) {
	case END:
	    return eq_FindBeginGroup(self, j);
	case BEGIN:
	    return -1;
	}
    }
}

long eq__FindRightSibling(self, i)
struct eq *self;
long i;
{
    register int j;
    if (i==0)
	return -1;
    for (j=eq_FindEndGroup(self, i+1)+1; ; j++) {
	switch (eq_Access(self, j)->symbol->type) {
	case END:
	    return -1;
	case BEGIN:
	    return j;
	}
    }
}

/* Write to a file */
void eq__WriteFILE(self, f, start, stop, sep)
struct eq *self;
FILE *f;
long start, stop;
char sep;
{
    while (start<stop) {
	char buf[4000];
	int n = eq_GetTokens(self, &start, stop, buf, sizeof(buf));
	if (n<=0)
	    break;
	buf[n-1] = sep;
	fwrite(buf, 1, n, f);
    }
}

/* Read from a FILE */
long eq__ReadFILE(self, file, start)
struct eq *self;
FILE *file;
long start;
{
    char word[100];
    long n = start, i, c;
    char name[64];
    long inid;

    c = getc(file);
    while (c != EOF && c != '\0') {
	char *ptr = word;

	while (c != ' ' && c != '\t' && c != '\n' && c != EOF) {
	    *ptr++ = c;
	    c = getc(file);
	}
	*ptr++ = '\0';
	if (c == EOF)
	    break;
	if (strncmp(word, "\\begindata{", 11) == 0) {
	    sscanf(word, "\\begindata{%s,%ld}", name, &inid);
	    if (strcmp(name, "eq") == 0)
		self->header.dataobject.id = inid;
	    else
		break;
	}
	if (strncmp(word, "\\enddata{", 9) == 0) {
	    sscanf(word, "\\enddata{%s,%ld}", name, &inid);
	    break;
	}
	if (word[0] != '\0') {
	    i = eq_InsertTokens(self, n, word);
	    if (i==0) {
		fprintf(stderr, "Unknown symbol '%s'\n", word);
		break;
	    }
	    n += i;
	}
	while (c == ' ' || c == '\t' || c == '\n') 
	    c = getc(file);
    }

    return n-start;
}

long eq__Read(self, file, id)
struct eq *self;
FILE *file;
long id;
{
    eq_Erase(self);
    if (id != 0L)
	self->header.dataobject.id = dataobject_UniqueID(self);
    self->header.dataobject.modified = 0;
    eq_ReadFILE(self, file, 0);
    return dataobject_NOREADERROR; /* probably should get a status value from eq_ReadFILE */
}

long eq__Write(self, file, writeid, level)
struct eq *self;
FILE *file;
long writeid;
long level;
{
    if ((self->header.dataobject.writeID != writeid)) {
	self->header.dataobject.writeID = writeid;
	fprintf(file, "\\begindata{%s,%ld}\n", class_GetTypeName(self), dataobject_UniqueID(self)); 
	eq_WriteFILE(self, file, 0, eq_Size(self), ' ');
	fprintf(file, "\\enddata{%s,%ld}\n", class_GetTypeName(self), dataobject_UniqueID(self));
    }
    return dataobject_UniqueID(self);
}

/* Dump info */
void eq__Dump(self, name)
struct eq *self;
char *name;
{
    FILE *file = NULL;
    int n = eq_Size(self), i;

    if (name != NULL && name[0] != '\0')
	file = fopen(name, "w");
    else
	file = stdout;
    if (file==NULL) {
	perror(name);
	exit(-1);
    }

    for (i=0; i<n; i++) {
	struct formula *f = eq_Access(self, i);
	fprintf(file, "%s pos (%d,%d) hot (%d,%d) %c%c%c min (%d,%d) max (%d,%d) ",
	    f->symbol->name,
	    f->pos.x, f->pos.y,
	    f->hot.x, f->hot.y,
	    (f->has_hot_spot? 'H': 'x'),
	    (f->transparent? 'T': 'x'),
	    (f->deletable? 'D': 'x'),
	    f->min.x, f->min.y,
	    f->max.x, f->max.y
	);
	fprintf(file, "sup_y %d, sub_y %d, kern %d\n",
	    f->sup_y,
	    f->sub_y,
	    f->kern
	);
	fflush(file);
    }
    fprintf(file, "\n");

    if (file != stdout)
	fclose(file);
}

boolean eq__InitializeClass(classID)
struct classheader *classID;
{
    if (zilch == NULL)
	zilch = eq_Lookup("zilch");
    if (root == NULL)
	root = eq_Lookup("root");

    return TRUE;
}
