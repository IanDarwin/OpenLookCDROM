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


 

#define nestedmark_VERSION 1

/* FreeTree: recursively frees the tree starting at self.
Add: adds in a mark in the nested mark tree headed by self. Returns new mark if completed.
Delete: removes the nested mark from the tree it is in.
Update: change the position of the marks where length items are
	added/delete.
GetInnerMost: returns the innermost nested mark surrounding the item at pos.
GetEnclosing: returns the innermost nested mark paying attention to the includebeginning and includeending flags.
Eval: returns the position assocated with the nested mark.
GetCommonParent: finds the common parent in the tree of self and nmark.
SetLength: sets the length of the nested mark.
GetNextChange: returns the next change fof the mark passed in by self and the pos.  If they are the same values as the last call to GetInnerMost then the value computed by GetInnerMost is used.
Distance: returns the distance from self to nmark.  If nmark is above self the value returned is positive - If self is above nmark the value returned is negative.  If they are equal the value returned is zero and if they are not directly related the value returned is nestedmark_UNRELATED.
SetStyle: Sets the includebegiing and includeending flags.
NewButSimilar: returns a new mark with the include{Ending,Beginning} fields copied from the old one.  This allows subclasses (i.e., environment) to copy relevant fields when we want a "copy" of a mark.
Split: Splits the mark given (and any children) in  two at the given point (relative the beginning of the mark), and returns the right half (the left half is the old mark, modified).
SetGlobalIsolation: If argument is True, Updates will behave as though all individual styles are False.  Returns prior value of GlobalStyle.
 */
class nestedmark[nstdmark] {
macromethods:
    GetLength() (self->length)
    GetParent() (self->parent)
methods:
    FreeTree();
    Add(long pos, long length) returns struct thisobject *;
    Delete();
    Update(long pos, long length);
    GetInnerMost(long pos) returns struct thisobject *;
    GetEnclosing(long pos) returns struct thisobject *;
    Eval() returns long;
    GetCommonParent(struct thisobject *nmark) returns struct thisobject *;
    SetLength(long length);
    GetNextChange(long pos) returns long;
    Distance(struct thisobject *nmark) returns long;
    SetStyle(boolean includebeginning, boolean includeending);
    GetChild(long position) returns struct thisobject *;
    GetNextChild(struct thisobject *current, long pos) returns struct thisobject *;
    GetPreviousChild(struct thisobject *current, long pos) returns struct thisobject *;
    NumberOfChildren() returns long;
    NewButSimilar() returns struct thisobject *;
    Split(long rpos) returns struct thisobject *;
classprocedures:
    SetGlobalIsolation(boolean dontextend) returns boolean;
data:
    struct tree23int *children;		/* The children of this mark */
    struct tree23int *position;		/* This mark in the superior 23-tree */
    long length;			/* Length of this mark */
    struct nestedmark *parent;		/* enclosing mark */
    boolean includeEnding;
    boolean includeBeginning;
};

#define nestedmark_UNRELATED -999999999
