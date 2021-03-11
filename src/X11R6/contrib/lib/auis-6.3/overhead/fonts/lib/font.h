/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/


 


/* ***************************************************************** */

/*	font.h		wm font format definition */

/*	
	This header file defines the font representation for the
	window manager.
*/

/* ***************************************************************** */


/* ***************************************************************** */


/* the font.h file may be included several times; to prevent
   duplication, we process it only if FONTMAGIC is undefined */

#ifndef FONTMAGIC


/*
	A font is a named collection of 128 icons. Each icon
	is a collection of bits representing the image of a
	character in that font.  In addition to the icons, 
	various additional information is kept for a font.
	A font has a name with a FamilyName (such as TimesRoman),
	a height (such as 10 points), and a FaceCode (such
	as italic or bold).  Also, the largest values of
	icon properties are kept.  The icon properties
	include the size of the bounding box, the location
	of the origin, the spacing, and so on.

        So a font is:

		A magic number
		A font name
		Global Font Properties
		An array of 128 Icons


	Each icon has two parts: a Generic part and a
	Specific part.  The theory is that a Generic icon
	may have several different specific representations:
	such as a Bitmap, Vector, Outline, and so on. (Currently
	we only support Bitmaps).  Each of would be different
	ways of representing the same icon, and so certain
	properties of the character are representation-independent:
	such as its origin and spacing.  The Specific part would
	include the type of representation and the specific
	icon for that representation.

	To avoid forward structure definitions, the more detailed
	portions are defined first: 
		vectors (SVector), 
		'struct icon' (and its generic and specific parts),
		'struct FontName', 
   		 and finally 'struct font'.
*/
	
#define CharsPerFont 128


/* ************************************************************ */
/*								*/
/*    To allow eventually for rotated fonts, all values are
	stored as vectors, each with an x and y component	*/
/*								*/
/* ************************************************************ */

struct SVector {		/* Short Vector */
    		  short x, y;
		};

/**************************************************\
* 						   *
* An icon is a patch that can be drawn.  It has a  *
* distinguished point called its origin.	   *
* 						   *
* 	           North			   *
*         |---------WtoE----------->|		   *
*      -  +-------------------------+		   *
*      |  |          /             /|		   *
*      |  |         /             / |		   *
*      |  |        /             /  |		   *
*      |  |       /             /   |		   *
*      N  |      /             /    |	East	   *
*      t  |     /             /     |		   *
*      o  |    /             /      |		   *
*      S  |---*-------------/-------|		   *
*      |  |  / Origin      /        |		   *
*      |  | /             /         |		   *
*      V  |/             /          |		   *
*      -  +-------------------------+		   *
*         <---|---Spacing-->|			   *
*           ^Wbase				   *
* 	        South				   *
* 						   *
\**************************************************/


/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

struct icon {		/* An icon is made up of a generic and a
			   specific part.  The address of each
			   is "Offset" bytes from the "icon"
			   structure */
    short   OffsetToGeneric;
    short   OffsetToSpecific;
};



/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* Given a pointer to an icon, 
	GenericPart returns a pointer to its IconGenericPart 
	SpecificPart returns a pointer to its BitmapIconSpecificPart */

#define GenericPart(icon) ((struct IconGenericPart *) \
		 (((int) (icon)) + (icon) -> OffsetToGeneric))
#define SpecificPart(icon) ((struct BitmapIconSpecificPart *) \
		 (((int) (icon)) + (icon)->OffsetToSpecific))

/* Given a character and a font,
	 GenericPartOfChar returns the corresponding IconGenericPart
	 SpecificPartOfChar returns the corresponding BitmapIconSpecificPart */
#define GenericPartOfChar(f,c) GenericPart(&((f)->chars[c]))
#define SpecificPartOfChar(f,c) SpecificPart(&((f)->chars[c]))



/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

struct IconGenericPart {	/* information relating to this icon
				   that is of general interest */
    struct SVector  Spacing;	/* The vector which when added to the
				   origin of this character will give 
				   the origin of the next character to
				   follow it */
    struct SVector  NWtoOrigin;	/* Vector from the origin to the North
				   West corner of the characters
				   bounding box */
    struct SVector  NtoS;	/* North to south vector */
    struct SVector  WtoE;	/* West to East vector */
    struct SVector  Wbase;	/* Vector from the origin to the West
				   edge parallel to the baseline */
};

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* Possible icon types: */
#define AssortedIcon 0		/* Not used in icons, only in fonts: the
				   icons have an assortment of types */
#define BitmapIcon 1		/* The icon is represented by a bitmap */
#define VectorIcon 2		/* The icon is represented as vectors */


/* ************************************************************ */


struct BitmapIconSpecificPart {	/* information relating to an icon that
				   is necessary only if you intend to
				   draw it */
    char    type;		/* The type of representation used for
				   this icon.  (= BitmapIcon) */
    unsigned char   rows,	/* rows and columns in this bitmap */
                    cols;
    char    orow,		/* row and column of the origin */
            ocol;		/* Note that these are signed--use BUG macro! */
    unsigned short  bits[1];	/* The bitmap for this icon */
};


/* romp does not sign extend characters; this macro sign extends them */
#ifdef ibm032
#define BUG(x) ((((int)(x))<<24)>>24)
#else /* ibm032 */
#define BUG(x) (x)
#endif /* ibm032 */


/*
   Although the bits[] array is given as 1, it is as large as
   needed.  The size is determined by orows and ocol.  The bits
   are packed by into 16 bit shorts, padded with zeros to make
   each row a multiple of 16 bits.  Thus the number of shorts needed
   for the bits is (orow + 15) / 16 * ocol.

   The packing is unusual: Each row is padded out as needed.  The
   first 16 bits of the top row are stored first; then the first
   16 bits of the next row, and so on, until the first 16 bits 
   of all rows are stored.  Then the second 16-bits of the top
   row are stored (after the first 16 bits of the bottom row).
   Thus, for the a 20 x 4 bit icon, we would have 8 shorts as

		1  5
		2  6
		3  7
		4  8
*/



/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* A font name description block.  These are used in 
   font definitions and in font directories */

#define FontFamilyNameSize 16   /* length of FamilyName */
struct FontName {
    char    FamilyName[FontFamilyNameSize];	/* eg. "TimesRoman" */
    short   rotation;		/* The rotation of this font (degrees;
				   positive =>clockwise) */
    char    height;		/* font height in points */
    char    FaceCode;		/* "Italic", "Bold" or "Bold Italic" */
};

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* FaceCode flags */
#define BoldFace 1
#define ItalicFace 2
#define ShadowFace 4
#define FixedWidthFace 010


/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */


/* A font.  This structure is at the front of every font file.  The icon
   generic and specific parts follow.  They are pointed to by offsets in
   the icon structures */

struct font {
    short   magic;		/* used to detect invalid font files */
    short   NonSpecificLength;	/* number of bytes in the font and
				   generic parts */
    struct FontName fn;		/* The name of this font */
    struct SVector  NWtoOrigin;	/* These are "maximal" versions of the
				   variables by the same names in each
				   constituent icon */
    struct SVector  NtoS;	/* North to South */
    struct SVector  WtoE;	/* West to East */
    struct SVector  Wbase;	/* From the origin along the baseline to
				   the West edge */
    struct SVector  newline;	/* The appropriate "newline" vector, its
				   just NtoS with an appropriate fudge
				   factor added */
    char    type;		/* The type of representation used for
				   the icons within this font.  If all
				   icons within the font share the same
				   type, then type is that type,
				   otherwise it is "AssortedIcon" */
    short  NIcons;		/* The number of icons actually in this
				   font.  The constant "CharsPerFont"
				   doesn't actually restrict the size
				   of the following array; it's used 
				   to specify the local common case */

    struct icon    chars[CharsPerFont];

   /* at the end of the font structure come 
      the icon structure for each character */
};

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

struct FontSet *getpfont();	/* get font given name to parse */
struct icon *geticon();		/* get an icon given a font and a slot */
int LastX, LastY;		/* coordinates following the end of
				   the last string drawn */

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* The value of font->magic is set to FONTMAGIC.  This is used to
   check that a file does indeed contain a font */
#define FONTMAGIC 0x1fd

#endif /* FONTMAGIC */
