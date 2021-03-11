/* figattr.ch - attributes for fig objects */
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

  $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figattr.ch,v 1.2 1992/12/14 20:45:16 rr2b R6tape $
*/

#define	figattr_MaskAll	    (0x00ff) /* binary 11111111 */
#define figattr_NumAttributes (8)

#define	figattr_Shade	    (0)
#define	figattr_Color	    (1)
#define	figattr_LineWidth   (2)
#define	figattr_RRectCorner (3)
#define	figattr_FontSize    (4)
#define	figattr_FontStyle   (5)
#define	figattr_FontFamily  (6)
#define	figattr_TextPos	    (7)

/* definitions:
Shade: long: value from 0 (white) to 8 (black). -1 means clear. 
LineWidth: long: 0 for hairline (always drawn at 1/72"); positive values for positive widths. The width in fig coords is val*figview_FigUPerPix.
RRectCorner: long: 0 for none; positive values for positive widths. The width in fig coords is val*figview_FigUPerPix.
Color: char *: lowercase string containing an X (or whatever) color name.
TextPos: long: figattr_PosLeft, figattr_PosCenter, figattr_PosRight 
*/

#define figattr_ShadeClear  (-1)
/* denominator for view_GrayPattern() calls */
#define figattr_ShadeDenom  (8)

/* values for the TextPos attribute */
#define	figattr_PosCenter   (0x00)
#define	figattr_PosLeft	    (0x01)
#define	figattr_PosRight    (0x02)
/*  I have no intention of supporting vertical positioning, but if anybody does, here are some values. OR them with the first three to create the TextPos value. Note that old TextPos values will be forwards-compatible.
#define	figattr_PosVCenter  (0x00)
#define	figattr_PosTop	    (0x10)
#define	figattr_PosBottom   (0x20)
*/

class figattr : dataobject [dataobj] {

    classprocedures:

      InitializeObject(struct figattr *self) returns boolean;
      FinalizeObject(struct figattr *self);

    overrides:
      Write(FILE *fp, long writeid, int level) returns long;
      Read(FILE *file, long id) returns long;

    methods:
      CopySelf() returns struct figattr *;
      CopyData(struct figattr *src, unsigned long mask);

    macromethods:
      SetActive(att, val)  ((self)->active = ((val) ? ((self)->active | ((unsigned long)1<<(att))) : ((self)->active & ~((unsigned long)1<<(att)))))
      IsActive(att)	((self)->active & ((unsigned long)1<<(att)))

      SetShadeVal(val)  ((self)->shade = (val))
      GetShadeVal()  ((self)->shade)
      SetShade(val)  ((self)->shade = (val), (self)->active |= ((unsigned long)1<<figattr_Shade))
      GetShade(def)  (figattr_IsActive((self), figattr_Shade) ? ((self)->shade) : ((def)->shade))

      SetLineWidthVal(val)  ((self)->linewidth = (val))
      GetLineWidthVal()  ((self)->linewidth)
      SetLineWidth(val)  ((self)->linewidth = (val), (self)->active |= ((unsigned long)1<<figattr_LineWidth))
      GetLineWidth(def)  (figattr_IsActive((self), figattr_LineWidth) ? ((self)->linewidth) : ((def)->linewidth))

      SetRRectCornerVal(val)  ((self)->rrectcorner = (val))
      GetRRectCornerVal()  ((self)->rrectcorner)
      SetRRectCorner(val)  ((self)->rrectcorner = (val), (self)->active |= ((unsigned long)1<<figattr_RRectCorner))
      GetRRectCorner(def)  (figattr_IsActive((self), figattr_RRectCorner) ? ((self)->rrectcorner) : ((def)->rrectcorner))

      SetColorVal(val)  (((self)->color ? (free((self)->color), 1) : 0), (self)->color = (char *)malloc(1+strlen(val)), strcpy((self)->color, (val)))
      GetColorVal()  ((self)->color)
      SetColor(val)  (figattr_SetColorVal(self, val), (self)->active |= ((unsigned long)1<<figattr_Color))
      GetColor(def)  (figattr_IsActive((self), figattr_Color) ? ((self)->color) : ((def)->color))

      SetFontFamilyVal(val)  (((self)->fontfamily ? (free((self)->fontfamily), 1) : 0), (self)->fontfamily = (char *)malloc(1+strlen(val)), strcpy((self)->fontfamily, (val)))
      GetFontFamilyVal()  ((self)->fontfamily)
      SetFontFamily(val)  (figattr_SetFontFamilyVal(self, val), (self)->active |= ((unsigned long)1<<figattr_FontFamily))
      GetFontFamily(def)  (figattr_IsActive((self), figattr_FontFamily) ? ((self)->fontfamily) : ((def)->fontfamily))

      SetFontSizeVal(val)  ((self)->fontsize = (val))
      GetFontSizeVal()  ((self)->fontsize)
      SetFontSize(val)  ((self)->fontsize = (val), (self)->active |= ((unsigned long)1<<figattr_FontSize))
      GetFontSize(def)  (figattr_IsActive((self), figattr_FontSize) ? ((self)->fontsize) : ((def)->fontsize))

      SetFontStyleVal(val)  ((self)->fontstyle = (val))
      GetFontStyleVal()  ((self)->fontstyle)
      SetFontStyle(val)  ((self)->fontstyle = (val), (self)->active |= ((unsigned long)1<<figattr_FontStyle))
      GetFontStyle(def)  (figattr_IsActive((self), figattr_FontStyle) ? ((self)->fontstyle) : ((def)->fontstyle))

      SetTextPosVal(val)  ((self)->textpos = (val))
      GetTextPosVal()  ((self)->textpos)
      SetTextPos(val)  ((self)->textpos = (val), (self)->active |= ((unsigned long)1<<figattr_TextPos))
      GetTextPos(def)  (figattr_IsActive((self), figattr_TextPos) ? ((self)->textpos) : ((def)->textpos))

    data:
      unsigned long active;  /* bits as defined above */

      long shade; 
      long linewidth;
      char *color;
      long rrectcorner;
      long fontsize;
      long fontstyle;
      char *fontfamily;
      long textpos;
};
