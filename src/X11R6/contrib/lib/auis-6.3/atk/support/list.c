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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/list.c,v 1.10 1992/12/15 21:41:42 rr2b R6tape $";
#endif

/* $ACIS$ */

 

/*
 * Generic Linked List / Stack / Queue datatype
 *
 * Although char *'s are used for data, pointers to any type
 * of object or structure may be used.  Minimal casting may
 * be required by some compilers.
 */

#include <class.h>
#include "list.eh"

#define new() \
  (struct list_Entry *) malloc(sizeof (struct list_Entry))

/*
 * Class procedures
 */

boolean list__InitializeObject(classID, self)
struct classheader *classID;
struct list *self;
{
    self->head = NULL;
    self->tail = NULL;
    self->size = 0;

    return TRUE;
}

void list__FinalizeObject(classID, self)
struct classheader *classID;
struct list *self;
{
    list_Clear(self);
}

static int CopyEntry(value, dst)
char *value;
struct list *dst;
{
    list_InsertEnd(dst, value);

    return TRUE;
}

void list__Merge(classID, dst, src)
struct classheader *classID;
struct list *dst, *src;
{
    list_Enumerate(src, CopyEntry, (char *) dst);
}

void list__Append(classID, dst, src)
struct classheader *classID;
struct list *dst, *src;
{
    if (dst->size == 0) {
        dst->head = src->head;
        dst->tail = src->tail;
        dst->size = src->size;
    } else {
        dst->tail->next = src->head;
        dst->size += src->size;
    }

    src->head = NULL;
    src->tail = NULL;
    src->size = 0;
}

/*
 * Methods
 */

void list__InsertFront(self, data)
struct list *self;
char *data;
{
    register struct list_Entry *p;

    p = new();
    p->data = data;
    p->next = self->head;
    self->head = p;
    self->size++;
}

void list__InsertEnd(self, data)
struct list *self;
char *data;
{
    register struct list_Entry *p;

    p = new();
    p->data = data;
    p->next = NULL;

    if (self->size == 0)
        self->head = p;
    else
        self->tail->next = p;
    self->tail = p;
    self->size++;
}

boolean list__InsertUnique(self, data)
struct list *self;
char *data;
{
    if (list_Member(self, data))
        return FALSE;

    list_InsertEnd(self, data);
    return TRUE;
}

/* Compare takes two arguments, data1 and data2. */
/* It should return a positive number if data1 is greater than */
/* data2, a negative number if data1 is less than data2, */
/* or zero if they are equal. */

void list__InsertSorted(self, data, compare)
struct list *self;
char *data;
procedure compare;
{
    register struct list_Entry *n, *p, **pp;

    n = new();
    n->data = data;

    pp = &self->head;
    for (p = *pp; p != NULL &&
      (*compare)(data, p->data) > 0; p = *pp)
        pp = &p->next;

    if (p == NULL)
        self->tail = n;

    n->next = *pp;
    *pp = n;

    self->size++;
}

char *list__RemoveFront(self)
struct list *self;
{
    register struct list_Entry *p = self->head;
    register char *data;

    if (self->size == 0)
	return NULL;

    if (--self->size == 0)
        self->head = self->tail = NULL;
    else
        self->head = p->next;

    data = p->data;
    free(p);
    return data;
}

/*
 * Find and delete
 */

boolean list__Delete(self, data)
struct list *self;
char *data;
{
    register struct list_Entry *p, **pp, *lp=NULL;

    pp = &self->head;
    for (p = *pp; p != NULL && p->data != data; lp=p, p = *pp)
        pp = &p->next;

    if (p == NULL)
        return FALSE;

    if (self->tail == p)
        self->tail = lp;

    *pp = p->next;
    free(p);

    self->size--;
    return TRUE;
}

boolean list__Member(self, data)
struct list *self;
char *data;
{
    register struct list_Entry *p;

    for (p = self->head; p != NULL; p = p->next)
        if (p->data == data)
              return TRUE;

    return FALSE;
}

/*
 * Simple sort, using qsort, wind out this list into
 * an array, sort it and make the list again.
 * Compare routine is same as that taken by InsertSorted.
 */

struct arg { char **list; int ind;};

static boolean MoveNew(data, ap)
char *data;
struct arg *ap;
{
    ap->list[ap->ind]=data;
    ap->ind++;
    return TRUE;
}

static procedure tcompare=NULL;

static int rcompare(d1, d2)
char **d1;
char **d2;
{
    procedure lcompare=tcompare;
    int result=tcompare?tcompare(*d1, *d2):0;
    tcompare=lcompare;
    return result;
}

boolean list__Sort(self, compare)
struct list *self;
procedure compare;
{
    struct arg a;
    int i;
    /* Create new list, inserting old elements in sorted order */

    if(self->size<=0) return FALSE;
    
    a.list = (char **)malloc(sizeof(char *)*self->size);
    tcompare = compare;
    a.ind=0;

    if(a.list == NULL) return FALSE;
    
    list_Enumerate(self, MoveNew, (char *) &a);

    qsort(a.list, self->size, sizeof(char *), rcompare);

    list_Clear(self);

    for(i=0;i<a.ind;i++) list_InsertEnd(self, a.list[i]);

    free(a.list);
    
    return TRUE;
}

/*
 * Enumerate runs proc on each entry in a list.
 * If proc returns FALSE, the enumeration terminates
 * and the piece of data responsible is returned.
 * Otherwise, the enumeration completes and NULL is returned.
 */

char *list__Enumerate(self, proc, rock)
struct list *self;
procedure proc;
char *rock;
{
    register struct list_Entry *p;

    for (p = self->head; p != NULL; p = p->next)
        if ((*proc)(p->data, rock) == FALSE)
            return p->data;

    return NULL;
}

void list__Clear(self)
struct list *self;
{
    register struct list_Entry *p, *n;

    for (p = self->head; p != NULL; p = n) {
	n = p->next;
        free(p);
    }

    self->head = NULL;
    self->tail = NULL;
    self->size = 0;
}
