/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/

/* $ACIS$ */

 

/*
 * Generic Linked List / Stack / Queue datatype
 *
 * Although char *'s are used here, pointers to any type of object
 * or structure may be used with minimal casting.
 */

struct list_Entry {
    struct list_Entry *next;
    char *data;
};

class list {
classprocedures:
    InitializeObject(struct list *self) returns boolean;
    FinalizeObject(struct list *self);

    /* Copies dst to the end of src.   Src is left alone. */
    Merge(struct list *dst, struct list *src);

    /* Tacks src list onto the end of dst. Src is emptied. */
    Append(struct list *dst, struct list *src);

methods:
    InsertFront(char *data);
    InsertEnd(char *data);
    InsertUnique(char *data) returns boolean;
    InsertSorted(char *data, procedure compare);

    RemoveFront() returns char *;

    Delete(char *data) returns boolean;
    Member(char *data) returns boolean;

    Sort(procedure greater) returns boolean;

    /* Enumerate proc receives (data, rock).  If the */
    /* proc returns FALSE, enumerate terminates and */
    /* returns the piece of data responsible. */

    Enumerate(procedure proc, char *rock) returns char *;

    Clear();

macromethods:
    Enqueue(char *data) (list_InsertEnd(self, data))
    Dequeue() (list_RemoveFront(self))

    Push(char *data) (list_InsertFront(self, data))
    Pop() (list_RemoveFront(self))

    Size() ((self)->size)

    First() ((self)->size == 0 ? NULL : (self)->head->data)
    Last() ((self)->size == 0 ? NULL : (self)->tail->data)

    /* Way to traverse the list from head to tail: */
    /* Start and Advance return NULL at end of list. */
    /* Data returns the data at the current position, NULL at end */

    Start() ((self)->trav = (self)->head)
    Advance() ((self)->trav == NULL ? NULL : \
              ((self)->trav = (self)->trav->next))
    Data() ((self)->trav == NULL ? NULL : \
              (self)->trav->data)

data:
    struct list_Entry *head;
    struct list_Entry *tail;
    long size;

    struct list_Entry *trav;
};
