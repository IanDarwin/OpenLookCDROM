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


 

#define environment_VERSION 1

union environmentcontents {
    struct style *style;
    struct viewref *viewref;
};

enum environmenttype {
    environment_None,
    environment_Style,
    environment_View,
    environment_Any
};

class environment[envrment] : nestedmark[nstdmark]  {
methods:
/* the next two are obsolete. Use WrapStyle, WrapView, InsertStyle, or
 * InsertView instead.
 */
    Wrap(long pos, long length, enum environmenttype type, union environmentcontents data) returns struct thisobject *;
    Insert(long rpos, enum environmenttype type, union environmentcontents data, boolean doinsert) returns struct thisobject *;
    WrapStyle(long pos, long length, struct style *style) returns struct thisobject *;
    WrapView(long pos, long length, struct viewref *viewref) returns struct thisobject *;
    InsertStyle(long rpos, struct style *style, boolean doinsert) returns struct thisobject *;
    InsertView(long rpos, struct viewref *viewref, boolean doinsert) returns struct thisobject *;
    Remove(long pos, long length, enum environmenttype type, boolean deleteall) returns boolean;
overrides:
    NewButSimilar() returns struct environment *;
classprocedures:
    InitializeObject(struct environment *self) returns boolean;
    FinalizeObject(struct environment *self);
    GetRootEnvironment() returns struct environment *;
    Allocate() returns struct environment *;
    Deallocate(struct environment *self);
data:
    enum environmenttype type;
    union environmentcontents data;
};
