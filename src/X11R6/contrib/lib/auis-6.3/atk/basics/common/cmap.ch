/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
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

class colormap [cmap] : dataobject [dataobj] {

classprocedures:
  InitializeClass() returns boolean;
  InitializeObject( struct colormap *self ) returns boolean;
  FinalizeObject( struct colormap *self ) returns void;
overrides:
  ViewName() returns char *;
methods:
  SetColor(struct color *color, boolean needpixel) returns int;
  LookupColor(char *name, unsigned int red, unsigned int green, unsigned int blue, boolean needpixel) returns struct color *;
  Copy(struct colormap *source) returns int;
  Merge(struct colormap *other) returns int;
  AllocColor(char *name, unsigned int red, unsigned int green, unsigned int blue, boolean needpixel) returns struct color *;
  ChangeColor(struct color *c) returns int;
  SetSize( int size) returns int;
  Clear();
  DestroyColor(struct color *c);
macromethods:
  Size() ((self)->size)
  Used() ((self)->used)
  NthColor(n) ((self)->colors[n])
data:
  int size, used;
  struct color **colors;
};

