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


 

#define mark_VERSION 1

class mark {
methods:
    SetStyle(boolean beginning, boolean ending);
    UpdateMarks(long pos, long size);
classprocedures:
    NewWithStyle(boolean beginning, boolean ending) returns struct mark *;
    Allocate() returns struct mark *;
    Deallocate(struct mark *self);

macromethods:

	GetObject()	((self)->object)
	GetPos() 	((self)->pos)
	GetLength()	((self)->length)
	GetEndPos()	((self)->pos + (self)->length)
	IncludeBeginning()	((self)->includeBeginning)
	IncludeEnding()	((self)->includeEnding)
	GetModified()	((self)->modified)
	ObjectFree()	((self)->objectFree)
	GetNext()	((self)->next)

	SetObject(obj)	((self)->object = (struct basicobject *)(obj))
	SetPos(position)	((self)->pos = position)
	SetLength(len)	((self)->length = len)
	SetModified(val)	((self)->modified = val)
	SetObjectFree(v)	((self)->objectFree = v)
	SetNext(n)	((self)->next = n)

data:
    struct basicobject *object;
    struct mark *next;
    long pos;
    long length;
    char modified;
    char objectFree;
    char includeBeginning;
    char includeEnding;
};
