/*
 * Copyright (C) 1990-1991 by Adobe Systems Incorporated.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems
 * Incorporated not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY FITNESS FOR A PARTICULAR PURPOSE AND
 * NON-INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * PostScript, Display PostScript, and Adobe are trademarks of Adobe Systems
 * Incorporated registered in the U.S.A. and other countries.
 *
 * Author: Adobe Systems Incorporated
 */

#include "wonderland.h"

#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/Scale.h>
#include <Xm/Label.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>

static void init_element(e)
    Element *e;
{
    e->red = INIT_RED;
    e->green = INIT_GREEN;
    e->blue = INIT_BLUE;
    e->background = INIT_BACK;
    e->size = INIT_SCALE;
    e->angle = INIT_ROTATE;
    e->buffered = True;
    e->drawingArea = NULL;
    e->pixmap = None;
    e->gstate = 0;
}

static void set_up_panel(parent, topLabelName, midLabelName, imagePanel, which)
    Widget parent;
    String topLabelName, midLabelName;
    Boolean imagePanel;
    Element *which;
{
    Widget rowcol, frame, w;

    init_element(which);

    frame = XtVaCreateManagedWidget("frame", xmFrameWidgetClass, parent,
				    NULL);
    rowcol = XtVaCreateManagedWidget("innerRowCol", xmRowColumnWidgetClass,
				     frame, NULL);
    w = XtVaCreateManagedWidget(topLabelName, xmLabelWidgetClass,
				rowcol, NULL);
    which->drawingArea = XtVaCreateManagedWidget("drawing",
						 xmDrawingAreaWidgetClass,
						 rowcol, NULL);
    XtAddCallback(which->drawingArea, XmNexposeCallback, init_drawing, which);
    w = XtVaCreateManagedWidget(midLabelName, xmLabelWidgetClass,
				rowcol, NULL);

    w = XtVaCreateManagedWidget("redSlider", xmScaleWidgetClass,
				rowcol, NULL);
    XtAddCallback(w, XmNdragCallback, red_moved, which);
    XtAddCallback(w, XmNvalueChangedCallback, red_moved, which);

    w = XtVaCreateManagedWidget("greenSlider", xmScaleWidgetClass,
				rowcol, NULL);
    XtAddCallback(w, XmNdragCallback, green_moved, which);
    XtAddCallback(w, XmNvalueChangedCallback, green_moved, which);
    
    w = XtVaCreateManagedWidget("blueSlider", xmScaleWidgetClass,
				rowcol, NULL);
    XtAddCallback(w, XmNdragCallback, blue_moved, which);
    XtAddCallback(w, XmNvalueChangedCallback, blue_moved, which);

    w = XtVaCreateManagedWidget("backSlider", xmScaleWidgetClass,
				rowcol, NULL);
    XtAddCallback(w, XmNdragCallback, back_moved, which);
    XtAddCallback(w, XmNvalueChangedCallback, back_moved, which);

    w = XtVaCreateManagedWidget("sizeSlider", xmScaleWidgetClass,
				rowcol, NULL);
    XtAddCallback(w, XmNdragCallback, size_moved, which);
    XtAddCallback(w, XmNvalueChangedCallback, size_moved, which);

    w = XtVaCreateManagedWidget("angleSlider", xmScaleWidgetClass,
				rowcol, NULL);
    XtAddCallback(w, XmNdragCallback, angle_moved, which);
    XtAddCallback(w, XmNvalueChangedCallback, angle_moved, which);
    
    if (imagePanel) {
	Widget h;

	h = XtVaCreateManagedWidget("toggleBox", xmRowColumnWidgetClass,
				     rowcol, NULL);
	w = XtVaCreateManagedWidget("emptyLabel", xmLabelWidgetClass,
				    h, NULL);
	w = XtVaCreateManagedWidget("toggleLabel", xmLabelWidgetClass,
				    h, NULL);
	w = XtVaCreateManagedWidget("toggleButton", xmToggleButtonWidgetClass,
				    h, NULL);
	XtAddCallback(w, XmNvalueChangedCallback, on_off, which);
	w = XtVaCreateManagedWidget("creditLabel", xmLabelWidgetClass,
				    rowcol, NULL);
    }
}

static void set_up_labels(parent)
    Widget parent;
{
    Widget rowcol, top, bottom, w;

    rowcol = XtVaCreateManagedWidget("labelRowCol", xmRowColumnWidgetClass,
				     parent, NULL);
    top = XtVaCreateManagedWidget("labelTopRowCol", xmRowColumnWidgetClass,
				  rowcol, NULL);
    w = XtVaCreateManagedWidget("emptyLabel", xmLabelWidgetClass,
				top, NULL);
    w = XtVaCreateManagedWidget("emptyDrawing", xmDrawingAreaWidgetClass,
				top, XtNmappedWhenManaged, False, NULL);
    w = XtVaCreateManagedWidget("emptyLabel", xmLabelWidgetClass,
				top, NULL);
    bottom = XtVaCreateManagedWidget("labelBottomRowCol",
				     xmRowColumnWidgetClass, rowcol, NULL);
    w = XtVaCreateManagedWidget("redLabel", xmLabelWidgetClass,
				bottom, NULL);
    w = XtVaCreateManagedWidget("greenLabel", xmLabelWidgetClass,
				bottom, NULL);
    w = XtVaCreateManagedWidget("blueLabel", xmLabelWidgetClass,
				bottom, NULL);
    w = XtVaCreateManagedWidget("backLabel", xmLabelWidgetClass,
				bottom, NULL);
    w = XtVaCreateManagedWidget("sizeLabel", xmLabelWidgetClass,
				bottom, NULL);
    w = XtVaCreateManagedWidget("angleLabel", xmLabelWidgetClass,
				bottom, NULL);
}

void set_up_window(shell)
    Widget shell;
{
    Widget rowcol;

    rowcol = XtVaCreateManagedWidget("outerRowCol", xmRowColumnWidgetClass,
				     shell, NULL);

    set_up_labels(rowcol);
    set_up_panel(rowcol, "squareLabel", "emptyLabel", False, &square);
    set_up_panel(rowcol, "curveLabel", "emptyLabel", False, &curve);
    set_up_panel(rowcol, "textLabel", "emptyLabel", False, &text);
    set_up_panel(rowcol, "imageLabel", "middleLabel", True, &image);
}
