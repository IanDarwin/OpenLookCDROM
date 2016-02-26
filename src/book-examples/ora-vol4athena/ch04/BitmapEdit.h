/*
 * Copyright 1989 O'Reilly and Associates, Inc.

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */


#ifndef _ORABitmapEdit_h
#define _ORABitmapEdit_h

/*
 * BitmapEdit Widget public include file
 */

/*
 * This include not needed unless the application includes Intrinsic.h
 * after this file.   Anyway, it doesn't hurt.
 */
#include <X11/Core.h>

/* Resources:
 * Name		     Class		RepType		Default Value
 * ----		     -----		-------		-------------
 * (from RectObj)
 * ancestorSensitive
 * x		    Position		Int		0
 * y		    Position		Int		0
 * width	Dimension	Dimension	0
 * height	Dimension	Dimension	0
 * borderWidth	BorderWidth	Int
 * sensitive	Sensitive
 *
 * (from WindowObj)
 * screen		    Screen		Pointer		XtCopyScreen
 * depth		    Depth		Int		XtCopyFromParent
 * colormap	    Colormap	        Pointer	        XtCopyFromParent
 * background	    Background		Pixel		White
 * backgroundPixmap    Pixmap		Pixmap		XtUnspecifiedPixmap
 * borderColor	    BorderColor		Pixel		Black
 * borderPixmap	    BorderPixmap	Pixmap		XtUnspecifiedPixmap
 * mappedWhenManaged   MappedWhenManaged	Boolean		True
 * translations
 * accelerators
 *
 * (from Core)
 * none
 *
 * (from BitmapEdit)
 * foregroundPixel    Foreground		Pixel		Black
 * backgroundPixel    Background		Pixel		White
 * callback	    Callback		Callback	NULL
 * cellSize	    CellSize		Int		30
 * pixmapWidth	    PixmapWidth		Int		32
 * pixmapHeight	    PixmapHeight	Int		32
 */

/* 
 * This public structure is used as call_data to the callback.
 * It passes the x, y position of the cell toggled (in units of
 * cells, not pixels) and a mode flag that indicates whether the
 * cell was turned on (1) or off (0).
 */
typedef struct {
	int mode;
	int newx;
	int newy;
} BitmapEditPointInfo;

#define XtNcellSizeInPixels "cellSizeInPixels"
#define XtNpixmapWidthInCells "pixmapWidthInCells"
#define XtNpixmapHeightInCells "pixmapHeightInCells"
#define XtNcurX "curX"
#define XtNcurY "curY"
#define XtNcellArray "cellArray"
#define XtNshowEntireBitmap "showEntireBitmap"
 
#define XtCCellSizeInPixels "CellSizeInPixels"
#define XtCPixmapWidthInCells "PixmapWidthInCells"
#define XtCPixmapHeightInCells "PixmapHeightInCells"
#define XtCCurX "CurX"
#define XtCCurY "CurY"
#define XtCCellArray "CellArray"
#define XtCShowEntireBitmap "ShowEntireBitmap"

extern char *BitmapEditGetArrayString(); /* w */
    /* Widget w; */

/* Class record constants */

extern WidgetClass bitmapEditWidgetClass;

typedef struct _BitmapEditClassRec *BitmapEditWidgetClass;
typedef struct _BitmapEditRec      *BitmapEditWidget;

#endif /* _ORABitmapEdit_h */
/* DON'T ADD STUFF AFTER THIS #endif */
