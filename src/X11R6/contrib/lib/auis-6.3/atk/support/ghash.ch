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


 

/* A hash table
 *
 */

#define ghash_BUCKETS 32

class ghash {

classprocedures:
    InitializeObject(struct ghash *self) returns boolean;
    FinalizeObject(struct ghash *self);
    InitializeClass() returns boolean;
methods:
    Lookup(char *key) returns char *;
    LookupKey(char *key) returns char *;
    Store(char *key, char *value) returns boolean;
    Delete(char *key) returns char *;
    Rename(char *key, char *new) returns char *;
    Clear();
    Enumerate(procedure proc, long rock) returns char *;
    Debug();
macromethods:
    SetHash(fn) (self->hash = ((procedure)fn))
    SetFreeVal(fn) (self->freeVal = ((procedure)fn))
    SetFreeKey(fn) (self->freeKey = ((procedure)fn))
    SetCopyKey(fn) (self->copyKey = ((procedure)fn))
    SetCopyVal(fn) (self->copyVal = ((procedure)fn))
    SetCompareKey(fn) (self->compKey = ((procedure)fn))
    GetFreeVal() (self->freeVal)
    GetFreeKey() (self->freeKey)
    GetCopyKey() ((char *(*)())self->copyKey)
    GetCopyVal() ((char *(*)())self->copyVal)
    GetCompareKey() ((int (*)())self->compKey)

data:
    struct glist *buckets[ghash_BUCKETS];
    procedure hash;
    procedure freeVal, freeKey;
    procedure copyVal, copyKey;
    procedure compKey;
};
