/* fontsel.ch - font selection inset dataobject */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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
  $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/fontsel.ch,v 1.2 1992/12/14 20:45:16 rr2b R6tape $
*/

#define	fontsel_DATACHANGED	(1)

#define	fontsel_Style	(0)
#define	fontsel_Size	(1)
#define	fontsel_Family	(2)

#define	fontsel_default_Style	(0)
#define	fontsel_default_Size	(12)
#define	fontsel_default_Family	("andy")

class fontsel : dataobject [dataobj] {

    classprocedures:
      InitializeClass() returns boolean;
      InitializeObject(struct fontsel *self) returns boolean;
      FinalizeObject(struct fontsel *self);

    methods:
      SetStyle(long mask);
      SetSize(short val);
      SetFamily(char *val);

    macromethods:
      GetStyle()  ((self)->style)
      GetSize()  ((self)->size)
      GetFamily()  ((self)->family)
      IsActive(val)  ((self)->active & ((unsigned long)1<<(val)))
      UnsetStyle()  (fontsel_SetStyle((self), fontsel_default_Style), (self)->active &= ~((unsigned long)1<<fontsel_Style))
      UnsetSize()  (fontsel_SetSize((self), fontsel_default_Size), (self)->active &= ~((unsigned long)1<<fontsel_Size))
      UnsetFamily()  (fontsel_SetFamily((self), fontsel_default_Family), (self)->active &= ~((unsigned long)1<<fontsel_Family))

    data:
      long style;
      short size;
      char *family;

      long active;
};
