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


 

#define tree23int_VERSION 1

class tree23int[tree23] {
methods:
    AddIn(long offset, struct tree23int *newnode);
    Apply(procedure proc);
    Filter(long offset, struct tree23int *left, struct tree23int *right, long lowval, long highval, procedure proc, char *procdata);
    FindL(long offset, long key) returns long;
    FindR(long offset, long key) returns long;
    Free();
    Merge(struct tree23int *ancestor, long offset, procedure proc, char *procdata);
    Update(long pos, long size);
    Enumerate(procedure proc, char *procdata) returns long;
    NumberOfLeaves() returns long; 
    Dump(long offset);
    Delete() returns struct tree23int *;
    Eval() returns long;
    Insert(long key, long data) returns struct tree23int *;
    Jam(struct tree23int *newnode);
    Remove(struct tree23int *child) returns struct tree23int *;
    Twiddle(struct tree23int *child);
    GetLeftMostNode() returns struct tree23int *;
    GetNextNode(struct tree23int *node) returns struct tree23int *;
    GetRightMostNode() returns struct tree23int *;
    GetPreviousNode(struct tree23int *node) returns struct tree23int *;
macromethods:
    GetData() (self->data)
classprocedures:
    Allocate() returns struct tree23int *;
    Deallocate(struct tree23int *self);
data:
    struct tree23int *parent;/* 	Parent of the node */
    long bump;			/* Amount to add to parents value to get key */
    long data;			/* The real data */
    boolean leaf;		/* True if the node is a leaf. */
    short nKids;
    struct tree23int *kid[3];
};

