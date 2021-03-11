/**
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 *
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:	Greg Renda <greg@ncd.com>
 * 		Network Computing Devices, Inc.
 * 		350 North Bernardo Ave.
 * 		Mountain View, CA  94043
 *
 * $NCDId: @(#)Graph.h,v 1.4 1994/04/07 18:18:28 greg Exp $
 */

#ifndef _Graph_h
#define _Graph_h

/****************************************************************
 *
 * Graph widget
 *
 ****************************************************************/

/** Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 foreground	     Foreground		Pixel		XtDefaultForeground
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

#define XtCData			"Data"
#define XtCNumSamples		"NumSamples"
#define XtCNumTracks		"NumTracks"
#define XtCStart		"Start"
#define XtCEnd			"End"
#define XtCLeftMarker		"LeftMarker"
#define XtCRightMarker		"RightMarker"
#define XtCGraphColor		"GraphColor"
#define XtCMarkerColor		"MarkerColor"
#define XtCPositionColor	"PositionColor"

#define XtNdata			"data"
#define XtNnumSamples		"numSamples"
#define XtNnumTracks		"numTracks"
#define XtNstart		"start"
#define XtNend			"end"
#define XtNleftMarker		"leftMarker"
#define XtNrightMarker		"rightMarker"
#define XtNgraphColor		"graphColor"
#define XtNmarkerColor		"markerColor"
#define XtNpositionColor	"positionColor"
#define XtNposition		"position"
#define XtNleftProc		"leftProc"
#define XtNrightProc		"rightProc"

/* declare specific GraphWidget class and instance datatypes */

typedef struct _GraphClassRec *GraphWidgetClass;
typedef struct _GraphRec *GraphWidget;

/* declare the class constant */

extern WidgetClass graphWidgetClass;

typedef short GraphDataType;

#define GraphLeftMarker		0
#define GraphRightMarker 	1

void            GraphSetPosition(), GraphRedraw();

#endif						/* _Graph_h */
