/* $XConsortium: CribBoard.h,v 1.5 90/12/19 18:46:00 converse Exp $ */

/* Copyright	Massachusetts Institute of Technology	1987, 1988
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#ifndef _CribBoard_h
#define _CribBoard_h

/****************************************************************
 *
 * CribBoard widget
 *
 ****************************************************************/

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

*/

/* define any special resource names here that are not in <X11/StringDefs.h> */

#define XtNpegColor "pegColor"
#define XtNholeColor "holeColor"
#define XtNpegSize "pegSize"
#define XtCPegSize "PegSize"
#define XtNholeSize "holeSize"
#define XtCHoleSize "HoleSize"
#define XtNnumPegs "numPegs"
#define XtCNumPegs "NumPegs"
#define XtNgroupSpace "groupSpace"
#define XtCGroupSpace "GroupSpace"
#define XtNrowSpace "rowSpace"
#define XtCRowSpace "RowSpace"
#define XtNnumRows "numRows"
#define XtCNumRows "NumRows"
#define XtNnumCols "numCols"
#define XtCNumCols "NumCols"

#define CribBoardUnset   (-1)

/* declare specific CribBoardWidget class and instance datatypes */

typedef struct _CribBoardClassRec*  CribBoardWidgetClass;
typedef struct _CribBoardRec*	    CribBoardWidget;

/* declare the class constant */

extern WidgetClass cribBoardWidgetClass;

#endif /* _CribBoard_h */
