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


 



#define fontdesc_All	-1
#define fontdesc_Plain	0
#define fontdesc_Bold	1
#define fontdesc_Italic	2
#define fontdesc_Shadow	4
#define fontdesc_Fixed	8
#define fontdesc_Outline 16
#define fontdesc_Thin	32
#define fontdesc_Black	64
#define fontdesc_Medium	128
#define fontdesc_Heavy	256
#define fontdesc_Condense 512

#define fontdesc_NumIcons 256


/* FaceCode flags - normally found in font.h for WM fonts*/
#define BoldFace 1
#define ItalicFace 2
#define ShadowFace 4
#define FixedWidthFace 010

struct fontnamedesc  {
    struct fontnamedesc *next;	/* next guy in list of all fonts */
    struct fontdesc *fontList;	/* list of all fonts in this family */
    char *name;			/* family name: use = for equality check */
};

struct FontSummary {
    short maxWidth;	/* the max char width over the whole font */
    short maxHeight;	/* the max above+below for the whole font */
    short newlineHeight;/* recommended newline distance for the font */
    short maxSpacing;	/* the max spacing in x direction */
    short maxBelow;	/* the max below for the whole font */
    short maxLeft;	/* the max to the left of the origin */
};

struct fontdesc_charInfo {
    short width;			/* Width of bounding box */
    short height;			/* Height of bounding box */
    short xOriginOffset;	        /* X offset from left of bb */
    short yOriginOffset;		/* Y offset from top of bb */
    short xSpacing;		/* hor dist from this origin to next*/
    short ySpacing;		/* vert dist from this origin to next */
};


class fontdesc {
methods:
    GetFontFamily() returns char *;
    GetFontFamilyDesc() returns struct fontnamedesc *;
    GetFontSize() returns long;
    GetFontStyle() returns long;

    CvtCharToGraphic(struct graphic *gr, char SpecialChar) returns struct graphic *;
    GetRealFontDesc(struct graphic *gr) returns struct font *;

    StringSize(struct graphic *gr, char * string, long * XWidth, long * YWidth) returns long;
    StringBoundingBox(struct graphic *graphic, char *string, int *width, int *height) returns long;
    TextSize(struct graphic *gr, char * text, long TextLength, long *XWidth, long *YWidth)
		returns long;
    WidthTable(struct graphic *gr) returns  short*; /* actually an array of short's */
    HeightTable(struct graphic *gr) returns short *; /* actually an array of short's */
    FontSummary(struct graphic *gr) returns struct FontSummary *;
    CharSummary(struct graphic *gr, char LookUpChar, struct fontdesc_charInfo * returendInfo);
macromethods:
    SetCharValid(char c) (self->charValid[(c) >> 3] |= (1 << ((c) & 7)))
    ClearCharValid(char c) (self->charValid[(c) >> 3] &= ~(1 << ((c) & 7)))
    CharValid(char c) (self->charValid[(c) >> 3] & (1 << ((c) & 7)))
classprocedures:
    Create(char * FontName, long FontStyle, long FontSize) returns struct fontdesc *;
    GetFontNameDesc(char * FontName) returns struct fontnamedesc *;
    ExplodeFontName(char *fontName, char *familyName, long bufSize, long *fontStyle, long *fontSize) returns boolean;
    InitializeObject(struct fontdesc *self) returns boolean;
    FinalizeObject(struct fontdesc * FontDescObject);
    Allocate() returns struct fontdesc *; 
    Deallocate(struct fontdesc *self);
data:
    struct fontnamedesc  *FontName;
    long   FontStyles;
    struct FontSummary summary;
    short *widthTable;		/* the full width table, if non-null */
    short *heightTable;		/* the full height table, if non-null*/
    short    FontSize;
    boolean DescValid;
    struct font * MachineDependentFontDescriptor;
    struct fontdesc *next;
    unsigned char charValid[fontdesc_NumIcons / 8];
};
