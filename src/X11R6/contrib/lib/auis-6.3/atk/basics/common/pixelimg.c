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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/pixelimg.c,v 2.15 1992/12/15 21:27:56 rr2b R6tape $";
#endif


 


/* pixelimg.c 

	Object to store pixel images.

*/


#include <class.h>
#include <andrewos.h>
#include <pixelimg.eh>

#define BUFBYTES	600	/* enough for 4792 bits */

#define WHITEBYTE	0
#define BLACKBYTE	0xFF
#define WHITEHALFWORD  ((WHITEBYTE <<8) | WHITEBYTE)
#define BLACKHALFWORD  ((BLACKBYTE <<8) | BLACKBYTE)

static bitmask[8] = {0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01};

/* pixelimage__ClipRange(ClassID, tstart, tlength, start, length)
  'Start' . . . 'start'+'length' denotes a range of integer values.  
  The function clips the values of 'tstart' and 'tlength' to that portion
  within the range denoted by 'start' and 'length'.  
  Modifies tstart and tlengthso start <= tstart, tlength >= 0, and
  tstart+tlength <= start+length.  Ensures that tstart+tlength retains 
  its value if that value is between start and start+length.
*/

void pixelimage__ClipRange(ClassID, tstart, tlength, start, length)
struct classheader *ClassID;
long *tstart, *tlength;
register long start, length;
{
    register long s = *tstart, l = *tlength;
    if (s < start) l -= start - s,  s = start;
    if (l < 0)  l = 0;
    if (s > start+length) s = start+length, l = 0;
    else if (s+l > start+length) l = start+length - s;
    *tstart = s;
    *tlength = l;
}

/* ClipChange(self, sub)
  clips 'sub' to lie within the pixels for self.
      sets the ChangedRect to include the space denoted by 'sub'
      note that width or height may be set to a negative value
      Returns TRUE if the width or height is set negative
*/

static boolean ClipChange(self, sub)
struct pixelimage *self;
struct rectangle *sub;
{
    struct rectangle R;
    rectangle_SetRectSize(&R, 0, 0, self->pixelsPerRow, self->numRows);
    rectangle_IntersectRect(sub, sub, &R);
    if (rectangle_IsEmptyRect(sub)) 
	return TRUE;
    if (rectangle_IsEmptyRect(&self->ChangedRect))
	self->ChangedRect = *sub;	/* copy whole struct */
    else
	rectangle_UnionRect(&self->ChangedRect, &self->ChangedRect, sub);
    return FALSE;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	SubRaster Access
 *		entry points:
 *			pixelimage__GetRow
 *			pixelimage__SetRow
 *			pixelimage_GetColumn
 *			pixelimage_SetColumn
 *	The non-raster source or destination must be short-word aligned.
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* GetRow(self, x, y, length, dest)
  performs the inner processing for pixelimage_GetRow
      assumes the arguments are valid
*/

static long GetRow(self, x, y, length, dest)
struct pixelimage *self;
long x, y, length;
register unsigned char *dest;
{
    register unsigned char *src;  /* where to fetch next source halfword */
    register unsigned char *dend;  /* last dest halfword to fill */
    register unsigned long v;	/* shift register to hold halfwords */
    register long shift;		/* amount to shift v to the right */

    /* set 'dend' to addr of last byte to store */
    dend = dest + ((length-1)/8);

    /* set src to addr of byte containing first bit */
    src = (self->bits + y * self->RowWidth) + (x/8);
    /* amount to shift v to the RIGHT.
      The source will wind up in the leftmost halfword of v.  The
      computed shift will move the first bit of the desired value
      to the top bit of the rightmost halfword of v.  */
    shift = 8 - (x % 8);

    if (shift < 8) {
	/* load first halfword into v */
	v = *src++;
	while (dest <= dend) {
	    /* get next 8 bits into shift register */
	    v = (v<<8) | (*src++);
	    /* shift to align with dest and store 8 bits */
	    *dest++ = (v >> shift);
	}
    }
    else	/* src aligned on halfword,  copy without shifting */
	bcopy(src, dest, (dend-dest+1));

    if ((length % 8) != 0) {
	/* pad last byte with WHITE 
	 mask selects bits after src 
	 length	 mask
	 1	 0x7F
	 2	 0x3F
	 . . . 
	 7	 0x1  */
	*dend = (*dend & ~((1 << (8-(length % 8))) - 1));
    }
    return length;
}



/* pixelimage__GetRow(self, x, y, length, dest)

  Copies into 'dest' the 'length' bits of 'self' from row 'y'
  starting at bit 'x'.
  Bits in the result are shifted so the bit at (x,y) is the
  leftmost bit in 'dest' and others follow in byte order.
  'Dest' must begin on a halfword boundary.
  The length of 'dest' must be at least 2 * ((length+15) % 16) bytes.
  (I.e., a multiple of two bytes at least as big as length/8.)
  Return value: The number of bits stored in 'dest'.  This will
  be less than 'length' if x+length is greater than the
      length of a row in 'self'.
      It will return 0 if y is outside the bounds of the image.
	  If 'dest' is not on a halfword boundary, returns -1;
*/
long pixelimage__GetRow(self, x, y, length, dest)
struct pixelimage *self;
long x, y, length;
register unsigned short *dest;
{
    if (((unsigned long)dest)&1)
	/* dest not aligned */
	return -1;
    pixelimage_ClipRange(&x, &length, 0, self->pixelsPerRow);
    if (length == 0 || y < 0 || y >= self->numRows) 
	return 0;

    return GetRow(self, x, y, length, (unsigned char *)dest);
}



/* SetRow(self, x, y, length, src)

  Internal version of pixelimage_SetRow.
  Does not check parameters or set self->ChangedRect

  Copies 'length' bits of 'src' into the 'y'th row of 'self'
  starting at bit 'x'.
  Returns the number of bits stored.
*/

static long SetRow(self, x, y, length, src)
struct pixelimage *self;
long x, y, length;
register unsigned char *src;
{
    register unsigned char *dest;  /* where to store next halfword */
    register unsigned char *send;  /* addr of halfword with last src bit */
    register unsigned long v = 0;	/* shift register to hold halfwords */
    register long shift;		/* amount to shift v to the right */
    long bitsleft;		/* number of bits used from *send */

    /* set 'send' to addr of the byte containing the last src bit */
    send = src + ((length-1) / 8);
    /* and set 'bitsleft' to the number of bits needed from *send   */
    bitsleft = ((length-1) % 8) + 1;

    /* set 'dest' to addr of byte containing first bit */
    dest = (self->bits + y * self->RowWidth) + (x / 8);
    /* amount to shift v to the RIGHT 
      Suppose x%7 is t.  Then each word stored must
      have t bits from the existing dest or prior source byte
      and 8-t bits of the new source.  The shift must be t bits to 
      align the new contents of v with the destination byte */
    shift = (x % 8);

    if (shift != 0) {
	/* load first dest halfword into v and shift to retain only 
	 the bits which preceding the first source byte. */
	v = *dest >> (8 - shift);

	while (src < send) {
	    /* get next 8 bits of src into shift register */
	    v = (v<<8) | (*src++);
	    /* shift to align with dest and store 8 bits */
	    *dest++ = (v >> shift);
	}
    }
    else {
	/* shift is zero, copy src directly to dest */
	bcopy(src, dest, (send-src));
	dest += send - src;
    }

    /* now append to dest the 'shift' remaining bits from *(send-1)
      and the 'bitsleft' bits from *send */
    /* first, get *send and align v with dest */
    v = ((v << 8) | *send) << (8 - shift);
    /* now update 'bitsleft' to include the bits from *(send-1) */
    bitsleft += shift;
    if (bitsleft > 8) {
	/* more than an entire byte left, store a byte */
	*dest++ = v >> 8;
	bitsleft -= 8;
    }
    else 
	/* get remaining bits to lower halfword */
	v >>= 8;
    if (bitsleft > 0) {
	/* make mask to combine dest and src
	 mask has (8-bitsleft) ones at right of word */
	unsigned long mask = (1 << (8 - bitsleft)) - 1;
	*dest = (v & ~mask) | (*dest & mask);
    }
    return length;
}


/* pixelimage__SetRow(self, x, y, length, src)

  Copies 'length' bits of 'src' into the 'y'th row of 'self'
  starting at bit 'x'.
  'Src' must begin on a halfword boundary.
  Returns the number of bits stored.  If the parameters are outside
  the defined part of the bitarray, returns zero.
  If the src is not halfword aligned, returns -1;
*/

long pixelimage__SetRow(self, x, y, length, src)
struct pixelimage *self;
long x, y, length;
register unsigned short *src;
{
    struct rectangle R;
    if (((unsigned long)src) & 1)
	return -1;
    rectangle_SetRectSize(&R, x, y, length, 1);
    if (self->ReadOnly || ClipChange(self, &R)) return 0;

    return SetRow(self, rectangle_Left(&R), 
		   rectangle_Top(&R), 
		   rectangle_Width(&R), (unsigned char *)src);
}

/* GetColumn(self, x, y, length, dest)
  Internal version of pixelimage_GetColumn.
  Does not check parameters.

  Copies into 'dest' the 'length' bits of 'self' from column 'x'
  starting at bit 'y'.
  Bits in the result are shifted so the bit at (x,y) is the
  leftmost bit in 'dest' and others follow in ascending order.
  Return value: The number of bits stored in 'dest'.
*/

static long GetColumn(self, x, y, length, dest)
struct pixelimage *self;
long x, y, length;
register unsigned char *dest;
{
    register unsigned char *src;		/* where to fetch next source byte */
    register unsigned char *send;	/* stop just before fetching this */
    register unsigned char v;	/* shift register to hold halfwords */
    register unsigned char vbit;		/* which bit to set in v */
    register unsigned char colbit;		/* which bit to examine from src */
    register long W = self->RowWidth;

    src = self->bits + y * W + (x >> 3);	/* first byte */
    send = src + length * W;			/* after last byte */
    colbit = 1 << (7 - (x & 0x7));		/* set then not altered */

    vbit = 0x80;
    v = 0;
    while (src < send) {
	/* for each src bit,  if it is one, set bit in v */
	if ((*src & colbit))
	    v |= vbit;
	src += W;
	vbit >>= 1;
	/* if vbit is zero, we have put 8 bits in v,  store in dest */
	if (vbit == 0) {
	    *dest++ = v;
	    vbit = 0x80;
	    v = 0;
	}
    }
    if (vbit < 0x80) {
	/* pad last halfword with WHITE 
	 vbit is where next bit would go
	 make it and the bits to its right WHITE */

	*dest = v & ~((vbit<<1) - 1);
    }
    return length;
}

/* pixelimage__GetColumn(self, x, y, length, dest)

  Copies into 'dest' the 'length' bits of 'self' from column 'x'
  starting at bit 'y'.
  Bits in the result are shifted so the bit at (x,y) is the
  leftmost bit in 'dest' and others follow in ascending order.
  'Dest' must begin on a halfword boundary.
  The length of 'dest' must be at least 2 * ((length+15) / 16) bytes.
  (I.e., a multiple of two bytes at least as big as length/8.)
  Return value: The number of bits stored in 'dest'.  This will
  be less than 'length' if y+length is greater than the
      height of a column in 'self'.
      If 'dest' is not on a halfword boundary, returns -1;
*/

long pixelimage__GetColumn(self, x, y, length, dest)
struct pixelimage *self;
long x, y, length;
register unsigned short *dest;
{
    if (((unsigned long)dest)&1)
	/* dest not aligned */
	return -1;
    pixelimage_ClipRange(&y, &length, 0, self->numRows);
    if (length == 0 || x < 0 || x >= self->pixelsPerRow) 
	return 0;

    return GetColumn(self, x, y, length, (unsigned char *)dest);
}



/* SetColumn(self, x, y, length, src)
  Internal version of pixelimage_SetColumn
  Does not check parameters
  Does not set ChangedRect

  Copies 'length' bits from 'src' into column 'x' of 'self'
  starting at bit 'y'.
  Bits in the result are shifted so the bit at (x,y) is the
  leftmost bit from 'src' and others follow in sequential order.
  Return value: The number of bits stored from 'src'.
*/

static long SetColumn(self, x, y, length, src)
struct pixelimage *self;
long x, y, length;
register unsigned char *src;
{
    register unsigned char *dest;	/* byte to receive incoming bit */
    register unsigned char vbit;	/* which bit to test in v */
    register unsigned char colbit; 	/* which bit to change in *dest */
    register unsigned char v = 0;	/* shift register to hold halfwords from src */
    register long W = self->RowWidth;
    register unsigned char *dend;	/* row after last row to receive a byte */

    dest = self->bits + y * W + (x/8);
    dend = dest + W * length;
    colbit = 0x80 >> (x & 0x7);		/* (never changes) */
    vbit = 0;				/* scans across v */
    while (dest < dend) {
	if (vbit == 0) {
	    v =	*src++;		    /* fetch next src byte */
	    vbit = 0x80;    	    /* set to examine its first bit */
	}
	/* check bit from src */
	if ((v & vbit))
	    *dest |= colbit;	/* store a one */
	else
	    *dest &= ~colbit;	/* clear to zero */
	vbit >>= 1;	/* examine next bit */
	dest += W;	/* and set to store in next row */
    }
    return length;
}


/* pixelimage__SetColumn(self, x, y, length, src)

  Copies 'length' bits from 'src' into column 'x' of 'self'
  starting at bit 'y'.
  Bits in the result are shifted so the bit at (x,y) is the
  leftmost bit from 'src' and others follow in sequential order.
  'src' must begin on a halfword boundary.
  The length of 'src' must be at least 2 * ((length+15) % 16) bytes.
  (I.e., a multiple of two bytes at least as big as length/8.)
  Return value: The number of bits stored from 'src'.  This will
  be less than 'length' if y+length is greater than the
      height of a column in 'self'.
      If 'src' is not on a halfword boundary, returns -1;
*/

long pixelimage__SetColumn(self, x, y, length, src)
struct pixelimage *self;
long x, y, length;
register unsigned short *src;
{
    struct rectangle R;
    if (((unsigned long)src) & 1)
	return -1;
    rectangle_SetRectSize(&R, x, y, 1, length);
    if (self->ReadOnly || ClipChange(self, &R)) return 0;

    return SetColumn(self, rectangle_Left(&R), 
		      rectangle_Top(&R), 
		      rectangle_Height(&R), (unsigned char *)src);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	SubRaster Operations
 *		entry points:
 *			pixelimage_PaintSubraster
 *			pixelimage_InvertSubraster
 *			pixelimage_MirrorLRSubraster
 *			pixelimage_MirrorUDSubraster
 *			pixelimage_RotateSubraster
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* pixelimage__PaintSubraster(self, sub, byte)
  sets the indicated Subraster to the color given by 'byte', which is
  WHITEBYTE or BLACKBYTE 
  (I suppose one could do eight bit wide patterns with this routine,
	but I wouldn't count on it.)
*/

void pixelimage__PaintSubraster(self, sub, byte)
struct pixelimage *self;
struct rectangle *sub;
long byte;
{
    unsigned short buffer[BUFBYTES>>1];
    register unsigned char *bx, *bend;
    register long row, rowlimit;
    register long x, width;
    if (self->ReadOnly || ClipChange(self, sub)) return;

    rowlimit = rectangle_Top(sub)+rectangle_Height(sub);
    x = rectangle_Left(sub);
    width = rectangle_Width(sub);
    for (bx = (unsigned char *)buffer, bend = bx + ((width+7)>>3); 
	  bx < bend; ) *bx++ = byte;
    for (row = rectangle_Top(sub); row < rowlimit; row++)
	SetRow(self, x, row, width, (unsigned char *)buffer);
}


/* pixelimage__InvertSubraster(self, sub)
  change ones to zeros and zeros to ones in indicated subraster 
*/

void pixelimage__InvertSubraster(self, sub)
register struct pixelimage *self;
struct rectangle *sub;
{
    unsigned short buffer[BUFBYTES>>1];
    register unsigned char *bx, *bend;
    register long row, rowlimit;
    register long x, width;
    if (self->ReadOnly || ClipChange(self, sub)) return;

    rowlimit = rectangle_Top(sub)+rectangle_Height(sub);
    x = rectangle_Left(sub);
    width = rectangle_Width(sub);
    bend = ((unsigned char *)buffer) + ((width+7)>>3);
    for (row = rectangle_Top(sub); row < rowlimit; row++) {
	GetRow(self, x, row, width, (unsigned char *)buffer);
	for (bx = (unsigned char *)buffer; bx < bend; bx++) *bx = ~ *bx;
	SetRow(self, x, row, width, (unsigned char *)buffer);
    }
}


static unsigned char GrayScalePatterns[2][15] ={
    {0x80, 0x80, 0x82, 0x88, 0x92, 0x92, 0x92, 0xAA,
    0x6D, 0x6D, 0x6D, 0x77, 0x7D, 0x7F, 0x7F},
    {0x00, 0x04, 0x10, 0x22, 0x24, 0x49, 0x55, 0x55,
    0xAA, 0xB6, 0xDB, 0xDD, 0xEF, 0xFB, 0xFF}
};

void pixelimage__GraySubraster(self, sub, level)
struct pixelimage *self;
struct rectangle *sub;
long level;
{
    unsigned short line0[BUFBYTES>>1], line1[BUFBYTES>>1];
    unsigned char byte0, byte1;
    register unsigned char *bx, *bend;
    register long row;
    long rowlimit;
    register long evenrowlimit;
    register long x, width;
    long shift;

    if (self->ReadOnly || ClipChange(self, sub)) return;

    level--;
    if (level < 0) level = 0;
    if (level > 14) level = 14;
    byte0 = GrayScalePatterns[0][level];
    byte1 = GrayScalePatterns[1][level];

    x = rectangle_Left(sub);
    row = rectangle_Top(sub);
    width = rectangle_Width(sub);
    rowlimit = row + rectangle_Height(sub);
    evenrowlimit = rowlimit - (rowlimit % 2);

    shift = x % 8;
    byte0 = (((byte0<<shift) | (byte0>>(8 - shift))) & 0xff);
    byte1 = (((byte1<<shift) | (byte1>>(8 - shift))) & 0xff);
    for (bx = (unsigned char *)line0, bend = bx + ((width+7)>>3); 
	  bx < bend; ) *bx++ = byte0;
    for (bx = (unsigned char *)line1, bend = bx + ((width+7)>>3); 
	  bx < bend; ) *bx++ = byte1;

    if ((row % 2) == 1)
	SetRow(self, x, row++, width, (unsigned char *)line1);
    for (; row < evenrowlimit;) {
	SetRow(self, x, row++, width, (unsigned char *)line0);
	SetRow(self, x, row++, width, (unsigned char *)line1); }
    if (evenrowlimit != rowlimit)
	SetRow(self, x, rowlimit - 1, width, (unsigned char *)line0);
}


/* pixelimage__MirrorLRSubraster(self, sub)
  reflect the subraster about its vertical axis 
*/
void pixelimage__MirrorLRSubraster(self, sub)
register struct pixelimage *self;
struct rectangle *sub;
{
    unsigned short bufferleft[BUFBYTES>>1], bufferright[BUFBYTES>>1];
    register long colleft, colright;
    long x, y, width, height;
    if (self->ReadOnly || ClipChange(self, sub)) return;

    rectangle_GetRectSize(sub, &x, &y, &width, &height);
    /* colleft and colright start on either side of the middle and move outward
      until colleft is at x */
    for (colleft = x + (width>>1) - 1,  colright = colleft + 1 + (width&1); 
	  colleft >= x; colleft--, colright++)  {
	/* interchange colleft and colright */
	GetColumn(self, colleft,   y, height, (unsigned char *)bufferleft);
	GetColumn(self, colright, y, height, (unsigned char *)bufferright);
	SetColumn (self, colleft,   y, height, (unsigned char *)bufferright);
	SetColumn (self, colright, y, height, (unsigned char *)bufferleft);
    }
}


/* pixelimage__MirrorUDSubraster(self, sub)
  reflect the subraster about its horizontal axis 
*/
void pixelimage__MirrorUDSubraster(self, sub)
struct pixelimage *self;
struct rectangle *sub;
{
    unsigned short buffer1[BUFBYTES>>1], buffer2[BUFBYTES>>1];
    long row1, row2;
    long x, y, width, height;
    if (self->ReadOnly || ClipChange(self, sub)) return;

    rectangle_GetRectSize(sub, &x, &y, &width, &height);
    /* row1 and row2 start on either side of the middle and move outward
      until row1 is at y */
    for (row1 = y+(height>>1)-1, row2 = row1+1+(height&1); 
	  row1 >= y; row1--, row2++) {
	/* interchange row1 and row2 */
	GetRow(self, x, row1, width, (unsigned char *)buffer1);
	GetRow(self, x, row2, width, (unsigned char *)buffer2);
	SetRow (self, x, row1, width, (unsigned char *)buffer2);
	SetRow (self, x, row2, width, (unsigned char *)buffer1);
    }
}


/* pixelimage__GetRotatedSubraster(self, sub, target)
  copies the subraster to the target raster, interchanging rows and columns.
  If the target is too small in either dimension, it is Resized. 
*/
void pixelimage__GetRotatedSubraster(self, sub, target)
register struct pixelimage *self;
struct rectangle *sub;
struct pixelimage *target;
{
    register long col;
    register long W;
    register unsigned char *dest;
    long x, y, width, height;

    rectangle_GetRectSize(sub, &x, &y, &width, &height);
    pixelimage_ClipRange(&x, &width, 0, self->pixelsPerRow);
    pixelimage_ClipRange(&y, &height, 0, self->numRows);

    if (pixelimage_GetWidth(target) < height  ||  pixelimage_GetHeight(target) < width)
	pixelimage_Resize(target, height, width);
    /* copy each column of self into a row of target 
      the order of doing col's is chosen to ensure that image is not reflected 
	it does x+width-1 ... x, making the test of the value before the decrement */
    W = target->RowWidth;
    dest = target->bits;
    for (col = x+width; col-- > x; ) {
	GetColumn(self, col, y, height, dest);
	dest += W;
    }
}

/* start added by pasieka */

/* Scales the subraster by scaling factors and puts result into
  target. Target is resized as needed. Can copy form self to self.
*/
void pixelimage__GetScaledSubraster(self, sub, NewW, NewH, target)
struct pixelimage *self;
struct rectangle *sub;
long NewW, NewH;
struct pixelimage *target;
{
    unsigned short bitbuf[BUFBYTES>>1];
    long x, y, width, height;

    struct rectangle R;

    /* Reduce sub to fit in self. This should not affect self->ChangedRect. 
      Used to be done by ClipChange(self, sub), which is wrong. */
    rectangle_SetRectSize(&R, 0, 0, self->pixelsPerRow, self->numRows);
    rectangle_IntersectRect(sub, sub, &R);

    rectangle_GetRectSize(sub, &x, &y, &width, &height);

    if (target->bits == NULL)
	pixelimage_Resize(target,
			  ((width > NewW) ? width : NewW),
			  ((height > NewH) ? height : NewH));
    else {
	long TargetW = pixelimage_GetWidth(target);
	long TargetH = pixelimage_GetHeight(target);
	long w = (width > NewW) ? width : NewW;
	long h = (height > NewH) ? height : NewH;
	/* Resize so that the target is at least the size of the selection within the source. */
	if (TargetW < w)
	    if (TargetH < h)
		pixelimage_Resize(target, w, h);
	    else pixelimage_Resize(target, w, TargetH);
	else if (TargetH < h)
	    pixelimage_Resize(target, TargetW, h);
    }

    /* if self is equal to target and the scaled image will overlap the source, copy the unscaled version of the subraster to translate the origin to (0,0) first and then scale. Note: this could be more clever but why bother? */
    if ((self == target) &&
	 ( (NewW > x) && (x != 0) && (NewH > y) && (y != 0) ) ) {
	long desty;
	/* copy whole thing -- no scaling */
	/* note that this assumes row major storing of the image. */
	for (desty = 0; desty < height; desty++) {
	    GetRow(self, x, y + desty, width, (unsigned char *)bitbuf);
	    SetRow(target, 0, desty, width, (unsigned char *)bitbuf); }
	x = 0;
	y = 0; }

    if (width==NewW && height==NewH) {
	long desty;
	/* copy whole thing -- no scaling */
	/* note that this assumes row major storing of the image. */
	for (desty = 0; desty < height; desty++) {
	    GetRow(self, x, y + desty, width, (unsigned char *)bitbuf);
	    SetRow(target, 0, desty, width,(unsigned char *) bitbuf); } }

    /* Scale the width first (if needed). */
    if (width != NewW) {
	float inc, currentfloat;
	long last = -1;		/* the last column to be copied into bitbuf. */
	long currentlong;		/* the current column to be copied. */
	long destx;			/* where to copy the currentlong column. */

	if ((inc = (float)width/NewW) > 1) {
	    /* Shrink Width by copying from left to right. */
	    for (destx = 0, currentlong = currentfloat = 0;
		 destx < NewW;
		 destx++, currentlong = currentfloat += inc) {
		if (currentlong != last) {
		    GetColumn(self, x + currentlong, y, height, (unsigned char *)bitbuf);
		    last = currentlong; }
		SetColumn(target, destx, 0, height, (unsigned char *)bitbuf); } }
	else {
	    /* Expand Width by copying from right to left. */
	    for (destx = NewW - 1, currentlong = currentfloat = inc * (NewW - 1);
		 destx >= 0;
		 destx--, currentlong = currentfloat -= inc) {
		if (currentlong != last) {
		    GetColumn(self, x + currentlong, y, height, (unsigned char *)bitbuf);
		    last = currentlong; }
		SetColumn(target, destx, 0, height, (unsigned char *)bitbuf); } }
    }

    /* Scale the height second (if needed). */
    if (height!=NewH) {
	struct pixelimage *source;
	float inc, currentfloat;
	long last = -1;		/* the last row to be copied into bitbuf. */
	long currentlong;		/* the current row to be copied. */
	long desty;			/* where to copy the currentlong row. */

	/* If width was not scaled then height will be scaled from the source. */
	if (width==NewW)
	    source = self;
	else {
	    source = target;
	    x = 0;
	    y = 0; }

	if ((inc = (float)height/NewH) > 1) {
	    /* Shrink the height by copying from the top down. */
	    for (desty = 0, currentlong = currentfloat = 0;
		 desty < NewH;
		 desty++, currentlong = currentfloat += inc) {
		if (currentlong != last) {
		    GetRow(source, x, y + currentlong, NewW, (unsigned char *)bitbuf);
		    last = currentlong; }
		SetRow(target, 0, desty, NewW, (unsigned char *)bitbuf); } }
	else {
	    for (desty = NewH - 1, currentlong = currentfloat = inc * (NewH - 1);
		 desty >= 0;
		 desty--, currentlong = currentfloat -= inc) {
		if (currentlong != last) {
		    GetRow(source, x, y + currentlong, NewW, (unsigned char *)bitbuf);
		    last = currentlong; }
		SetRow(target, 0, desty, NewW, (unsigned char *)bitbuf); } } }

    pixelimage_Resize(target, NewW, NewH);
}

/* Blit the subraster given within the source into self starting at
  the (x, y) coordinates (as an offset from the origin of self) using
  the function given.
*/
void pixelimage__BlitSubraster(self, x, y, source, sub, function)
struct pixelimage *self;
long x, y;
struct pixelimage *source;
struct rectangle *sub;
int function;
{
    struct rectangle R;
    long width, height;
    long locy, subx, suby;
    register long bx, bend;
    /* now we need two buffers that are halfword-aligned but accessible as char * arrays. Thus, a cheap hack. */
    unsigned short bitbuf[BUFBYTES>>1];
    unsigned short desbuf[BUFBYTES>>1];
#define bitbbuf ((unsigned char *)bitbuf)
#define desbbuf ((unsigned char *)desbuf)

    width = rectangle_Width(sub);
    height = rectangle_Height(sub);

    rectangle_SetRectSize(&R, x, y, width, height);
    ClipChange(self, &R);

    width = rectangle_Width(&R);
    height = rectangle_Height(&R);
    bend = (width+7)>>3; /* width of rectangle in bytes */

/* Someone changed this. Pasieka's changing it back.
    for (locy = 0; locy < height; locy++) {
	GetRow(source, 0, locy, width, bitbuf);
	SetRow(self, x, y + locy, width, bitbuf); }
*/
    switch (function & 0xF) {
	case 0x0: /* white (0) */
	    for (bx=0; bx<bend; bx++)
		bitbbuf[bx] = 0;
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0x1: /* inverted or (~(src|dest))*/
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		GetRow(self, x, y + locy, width, desbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = ~(bitbbuf[bx] | desbbuf[bx]);
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0x2: /* ((~src)&dest) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		GetRow(self, x, y + locy, width, desbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = (~bitbbuf[bx]) & desbbuf[bx];
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0x3: /* inverted copy (~src) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = ~(bitbbuf[bx]);
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0x4: /* (src&(~dest)) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		GetRow(self, x, y + locy, width, desbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = bitbbuf[bx] & (~desbbuf[bx]);
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0x5: /* invert (~dest) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(self, x, y + locy, width, bitbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = ~(bitbbuf[bx]);
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0x6: /* xor (src^dest) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		GetRow(self, x, y + locy, width, desbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = bitbbuf[bx] ^ desbbuf[bx];
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0x7: /* inverted and (~(src&dest)) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		GetRow(self, x, y + locy, width, desbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = ~(bitbbuf[bx] & desbbuf[bx]);
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0x8: /* and (src&dest) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		GetRow(self, x, y + locy, width, desbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = bitbbuf[bx] & desbbuf[bx];
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0x9: /* equiv (~(src^dest)) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		GetRow(self, x, y + locy, width, desbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = ~(bitbbuf[bx] ^ desbbuf[bx]);
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0xA: /* noop (dest) */
	    break;
	case 0xB: /* ((~src)|dest) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		GetRow(self, x, y + locy, width, desbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = (~bitbbuf[bx]) | desbbuf[bx];
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0xC: /* copy (src) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0xD: /* (src|(~dest)) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		GetRow(self, x, y + locy, width, desbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = bitbbuf[bx] | (~desbbuf[bx]);
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0xE: /* or (src|dest) */
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		GetRow(source, subx, suby, width, bitbbuf);
		GetRow(self, x, y + locy, width, desbbuf);
		for (bx=0; bx<bend; bx++)
		    bitbbuf[bx] = bitbbuf[bx] | desbbuf[bx];
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
	case 0xF: /* black (1) */
	    for (bx=0; bx<bend; bx++)
		bitbbuf[bx] = (~((unsigned char)0));
	    for (locy = 0, subx = rectangle_Left(sub), suby = rectangle_Top(sub);
		 locy < height; locy++, suby++) {
		SetRow(self, x, y + locy, width, bitbbuf);
	    }
	    break;
    }
}

/* end added by pasieka */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	Object Functions
 *		entry points:
 *			pixelimage__InitializeObject
 *			pixelimage__FinalizeObject
 *			pixelimage__Create
 *			pixelimage__NotifyObservers
 *			pixelimage__Resize
 *			pixelimage__Clone
 *			pixelimage__Clear
 *			pixelimage__GetPixel
 *			pixelimage__SetPixel
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/* pixelimage__InitializeObject(classID, self)
  Initializes the fields of a new object.
  NO bit_array is created.
  Returns TRUE always because there is 
  no memory allocation to fail.
*/
boolean pixelimage__InitializeObject(classID, self)
struct classheader	*classID;
register struct pixelimage  *self;
{
    self->pixelsPerRow = 0;
    self->RowWidth = 0;
    self->numRows = 0;
    self->ReadOnly = FALSE;
    self->bits = NULL;
    self->Resized = FALSE;
    rectangle_EmptyRect(&self->ChangedRect);

    return(TRUE);
}

/* pixelimage__FinalizeObject(ClassID,  self)
  does processing for the middle of pixelimage_Destroy.
      Frees the 'bits', if any.
*/
void pixelimage__FinalizeObject(ClassID,  self)
struct classheader *ClassID;
register struct pixelimage *self;
{
    if (self->bits != NULL)
	free (self->bits);
}


/* pixelimage__Create(ClassID, width, height)
  create a pixelimage object with space for a raster
      of the given 'width' and 'height'
      The raster is NOT initialized to any particular value. 
      Returns the new pixelimage object. 
*/
struct pixelimage *pixelimage__Create(ClassID, width, height)
struct classheader *ClassID;
long width, height;
{
    struct pixelimage *ras = pixelimage_New();
    ras->pixelsPerRow = width;
    ras->numRows = height;
    /* round RowWidth so rows are a multiple of short words */
    ras->RowWidth = ((width+15) >> 4) << 1;
    ras->bits = (unsigned char *)malloc(height * ras->RowWidth);
    return ras;
}

/* pixelimage__NotifyObservers(self, status)
  Informs all observers that a change has occurred.
  The fields ChangedRect and Resized describe the changes.

  The client must call NotifyObservers;  it will clear out ChangedRect.
*/
void pixelimage__NotifyObservers(self, status)
register struct pixelimage *self;
long status;
{
    super_NotifyObservers(self, status);
    rectangle_EmptyRect(&self->ChangedRect);
    self->Resized = FALSE;
}


/* pixelimage__Resize(self, width, height)
  Change the width and height of an image.  
  The upper left corners of the old and new images
  are aligned, with truncation or padding with WHITE. 
*/
void pixelimage__Resize(self, width, height)
register struct pixelimage *self;
long width, height;
{
    long w, h, i;
    unsigned short trow[BUFBYTES>>1];
    struct pixelimage *new;

    if (self->ReadOnly) return;
    if (self->bits == NULL)
	self->pixelsPerRow = self->numRows = 0;
    if (width == self->pixelsPerRow  && height == self->numRows)
	return;

    new = pixelimage_Create(width, height);
    if (width > self->pixelsPerRow || height > self->numRows)
	pixelimage_Clear(new);
    w = (width > self->pixelsPerRow) ? self->pixelsPerRow : width;
    h = (height > self->numRows) ? self->numRows : height;
    for (i = 0; i < h; i++) {
	GetRow(self, 0, i, w, (unsigned char *)trow);
	SetRow(new, 0, i, w, (unsigned char *)trow);
    }
    if (self->bits != NULL)
	free (self->bits);
    self->bits = new->bits;
    new->bits = NULL;
    self->pixelsPerRow = new->pixelsPerRow;
    self->numRows = new->numRows;
    self->RowWidth = new->RowWidth;
    pixelimage_Destroy(new);

    rectangle_EmptyRect(&self->ChangedRect);
    self->Resized = TRUE;
}

/* pixelimage__Clone(self)
  make a new pixelimage with all fields the same
  (both point to same bits array)
*/
struct pixelimage *pixelimage__Clone(self)
register struct pixelimage *self;
{
    struct pixelimage *new = (struct pixelimage *) class_NewObject(class_GetTypeName(self));
    new->numRows = self->numRows;
    new->pixelsPerRow = self->pixelsPerRow;
    new->RowWidth = self->RowWidth;
    new->bits = self->bits;
    return new;
}


/* pixelimage__Clear(self)
  Sets all the raster image to WHITE 
*/
void pixelimage__Clear(self)
register struct pixelimage *self;
{
    register unsigned char *rowx;
    register long W = self->RowWidth;

    if (self->bits == NULL)
	return;
    if (self->ReadOnly) return;
    {
	/* clear out the first row of bitarray */
	register unsigned short *hwx = (unsigned short *)self->bits;
	register unsigned short *hwend = hwx + (W>>1);
	while (hwx < hwend)
	    *hwx++ = WHITEHALFWORD;
    }
    /* copy first row to the others */
    for (rowx = self->bits + W * (self->numRows - 1);
	  rowx > self->bits;
	  rowx -= W)
	bcopy (self->bits, rowx, W);

    rectangle_SetRectSize(&self->ChangedRect, 0, 0,
			   self->pixelsPerRow, self->numRows);
}


long pixelimage__GetPixel(self, x, y)
register struct pixelimage *self;
register long x, y;
{
    register unsigned char *bytePtr = (self->bits) + (self->RowWidth*y) + (x >> 3);
    return (*bytePtr & bitmask[x & 0x7]) ? 1 : 0;
}

void pixelimage__SetPixel(self, x, y, pixelValue)
register struct pixelimage *self;
register long x, y, pixelValue;
{
    struct rectangle R;
    register unsigned char *bytePtr;
    if (self->ReadOnly) return;

    rectangle_SetRectSize(&R, x, y, 1, 1);
    if (ClipChange(self, &R)) return;

    bytePtr = (self->bits) + (self->RowWidth*rectangle_Top(&R)) 
      + (rectangle_Left(&R) >> 3);
    if ((pixelValue & 0x1))
	*bytePtr |= bitmask[x & 0x7];
    else
	*bytePtr &= ~bitmask[x & 0x7];
}
