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


 

/* List object
 * for Graph Editor
 */

struct glistelt {
    char * this;
    struct glistelt *next;
};

class glist {

classprocedures:
    Create(procedure Destroy) returns struct glist *;
    FinalizeObject(struct glist *self);
    InitializeObject(struct glist *self) returns boolean;    
    Copy(struct glist *dest, struct glist *source);
methods:
    Insert(char * element) returns boolean;
    Push(char *element) returns boolean;
    Pop() returns char *;
    InsertSorted(char *element, procedure greater) returns boolean;
    Sort(procedure greater) returns boolean;
    InsertUnique(char * element) returns boolean;
    Delete(char * element, boolean destroy) returns boolean;
    Find(procedure filter, char * rock) returns char *; /* filter is called: filter(char * element) */
    Enumerate(void (*proc)(), unsigned long rock);
    Contains(char * element) returns boolean;
    Clear(boolean destroy);
macromethods:
    First() ((self)->head->this)
    Last() ((self)->tail->this)
    Size() ((self)->size)
data:
    struct glistelt *head;
    struct glistelt *tail;
    long size;
    procedure DestroyProc;
};
