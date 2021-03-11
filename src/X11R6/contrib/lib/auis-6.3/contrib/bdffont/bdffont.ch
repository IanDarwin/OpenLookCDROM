/* bdffont.ch - font editor for bdf format fonts */

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

/*
	Copyright Carnegie Mellon University 1991, 1992 - All rights reserved
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

#include <units.h>

/* value for NotifyObservers when bdffont__Write is called */
#define bdffont_Writing  4

#define bdffont_WidthInBytes(w) (((w) + 7) / 8)

/* Face codes */
#define bdffont_Bold		(0x1)
#define bdffont_Italic		(0x2)
#define bdffont_FixedWidth	(0x4)
#define bdffont_Shadowed	(0x8)

struct bdffont_fontchar
    {
	char name[16];
	int encoding;
	long swx, swy;
	long dwx, dwy;
	long bbw, bbh, bbx, bby;
	long attributes;
	unsigned char *bitmap;	/* NULL means not active */
    };

#define bdffont_Alignment (16)

#define bdffont_IsActive(fc) ((fc)->bitmap != NULL)

#define bdffont_GetCharEncoding(fc) ((fc)->encoding)
#define bdffont_GetCharName(fc) ((fc)->name)
#define bdffont_SetCharName(fc, n) strncpy((fc)->name, n, sizeof((fc)->name))
#define bdffont_GetSWidth(fc, x, y) ((*(x) = (fc)->swx), (*(y) = (fc)->swy))
#define bdffont_SetSWidth(fc, x, y) (((fc)->swx = (long) (x)), ((fc)->swy = (long) (y)))
#define bdffont_GetDWidth(fc, x, y) ((*(x) = (fc)->dwx), (*(y) = (fc)->dwy))
#define bdffont_SetDWidth(fc, x, y) (((fc)->dwx = (long) (x)), ((fc)->dwy = (long) (y)))
#define bdffont_GetOrigin(fc, x, y) ((*(x) = (fc)->bbx), (*(y) = (fc)->bby))
#define bdffont_SetOrigin(fc, x, y) (((fc)->bbx = (long) (x)), ((fc)->bby = (long) (y)))
#define bdffont_GetExtent(fc, w, h) ((*(w) = (fc)->bbw), (*(h) = (fc)->bbh))
#define bdffont_SetExtent(fc, w, h) (((fc)->bbw = (long) (w)), ((fc)->bbh = (long) (h)))
#define bdffont_GetAttrs(fc) ((fc)->attributes)
#define bdffont_SetAttrs(fc, p) ((fc)->attributes = (long) (p))
#define bdffont_GetBitmap(fc) ((fc)->bitmap)
#define bdffont_SetBitmap(fc, bm) ((fc)->bitmap = (unsigned char *) (bm))

#define bdffont_AlignedWidthInBytes(fc) \
	(bdffont_WidthInBytes(bdffont_Alignment) * \
		(((fc)->bbw + bdffont_Alignment - 1) / bdffont_Alignment))
#define bdffont_BitmapSize(fc) ((fc)->bbh * bdffont_AlignedWidthInBytes(fc))

class bdffont : dataobject[dataobj] {
    classprocedures:
	InitializeObject(struct bdffong *self) returns boolean;
	FinalizeObject(struct bdffont *self);
	CreateNewFont(long pts, long resx, long resy) returns struct bdffont *;
    overrides:
	Read(FILE *file, long id) returns long;
	Write(FILE *file, long writeid, int level) returns long;
    methods:
	SetFontName(char *fn);
	SetFontFamily(char *fn);
	GetBoundingBox(long *w, long *h, long *x, long *y);
	SetBoundingBox(long w, long h, long x, long y);
	GetResolution(long *rx, long *ry);
	SetResolution(long rx, long ry);
	SetCharDWidth(int which, long x, long y);
	SetCharExtent(int which, long w, long h);
	SetCharOrigin(int which, long x, long y);
	GetDefaultChar() returns int;
	SetDefaultExtent(long w, long h);
	GetDefaultExtent(long *w, long *h);
	SetDefaultOrigin(long x, long y);
	GetDefaultOrigin(long *x, long *y);
	SetDefaultDelta(long dx, long dy);
	GetDefaultDelta(long *dx, long *dy);
    macromethods:
	GetFontName() ((self)->fontname)
	GetFontFamily() ((self)->fontfamily)
	GetPointSize() ((self)->pointsize)
	SetPointSize(long s) ((self)->pointsize = (s))
	GetFontWeight() ((self)->fontweight)
	SetFontWeight(long w) ((self)->fontweight = (w))
	GetFontFace() ((self)->fontface)
	SetFontFace(long f) ((self)->fontface = (f))
	GetFontAscent() ((self)->ascent)
	SetFontAscent(long a) ((self)->ascent = (a))
	GetFontDescent() ((self)->descent)
	SetFontDescent(long d) ((self)->descent = (d))
	SetDefaultChar(int c) ((self)->defaultchar = (long) (c))
	GetActiveDefns() ((self)->activedefns)
	ModifyActiveDefnsBy(long count) ((self)->activedefns += (count))
	GetDefinition(int encoding) (&(self)->defns[encoding])
	ComputeFontSize() (((double) ((self)->pointsize * (self)->resy)) / units_POINTSperINCH)
    data:
	struct tlex *lex;
	char *version;
	char *comments;
	char *fontname;
	long pointsize;
	long resx;		/* RESOLUTION_X */
	long resy;		/* RESOLUTION_Y */
	long bbw, bbh, bbx, bby;
	long proplength;
	char *properties;	/* known properties ... */
	char *fontfamily;	/* FAMILY */
	long fontweight;	/* WEIGHT */
	long fontface;		/* WEIGHT_NAME */
	long ascent;		/* FONT_ASCENT */
	long descent;		/* FONT_DESCENT */
	long defaultw;		/* DEFAULT_WIDTH */
	long defaulth;		/* DEFAULT_HEIGHT */
	long defaultx;		/* DEFAULT_X */
	long defaulty;		/* DEFAULT_Y */
	long defaultdx;		/* DEFAULT_DX */
	long defaultdy;		/* DEFAULT_DY */
	long defaultchar;	/* DEFAULT_CHAR */
	long activedefns;
	struct bdffont_fontchar *defns;
	long defns_size;
	long lastcharloaded;
};
