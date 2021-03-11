/*
 * LineChart chart display widget
 *
 * Copyright 1989, PCS Computer Systeme GmbH, West Germany
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of PCS not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  PCS makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * PCS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL PCS
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Original Author:  Emanuel Jay Berkenbilt, MIT Project Athena
 * Author:           Thomas A. Baghli, PCS Computer Systeme GmbH, West Germany
 *                   tom@meepmeep.pcs.com
 */

#ifndef _XtLineChart_h
#define _XtLineChart_h

/***********************************************************************
 *
 * LineChart Widget
 *
 ***********************************************************************/

/* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		White
 border		     BorderColor	Pixel		Black
 borderWidth	     BorderWidth	Dimension	1
 font		     Font		XFontStruct*	XtDefaultFont
 foreground	     Foreground		Pixel		Black
 height		     Height		Dimension	200
 reverseVideo	     ReverseVideo	Boolean		False
 stepSize	     StepSize		int		1
 update		     Interval		int		5 (seconds)
 width		     Width		Dimension	200
 x		     Position		Position	0
 y		     Position		Position	0
*/

#define XtNgetValue "getValue"
#define XtNstepSize "stepSize"
#ifndef XtNupdate
#define XtNupdate   "update"
#endif
#define XtNminValue "minValue"
#define XtNmaxValue "maxValue"

#define XtCStepSize "StepSize"

typedef struct _LineChartRec *LineChartWidget;
typedef struct _LineChartClassRec *LineChartWidgetClass;

extern WidgetClass lineChartWidgetClass;

#endif
