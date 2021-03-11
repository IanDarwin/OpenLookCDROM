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


#define icon_TitleChanged 34935	/* random integers... */
#define icon_SizeChanged 1894
#define icon_ChildChanged 2435
#define icon_ChangedSomehow 90235

class icon : dataobject[dataobj] {
classprocedures:
  InitializeObject(struct thisobject *self) returns boolean;
  InitializeClass() returns boolean;
overrides:
    Read (FILE *file, long id) returns long;
    Write (FILE *file, long writeid, int level) returns long;
methods:
    SetSize(long w,long h);
    GetSize(long * w, long * h);
    SetChild(struct dataobject * child);
    GetChild() returns struct dataobject *;
    SetTitle(char * title);
    GetTitle() returns char *;
data:
    struct dataobject * child;
    long width, height;
    char * title;
};
