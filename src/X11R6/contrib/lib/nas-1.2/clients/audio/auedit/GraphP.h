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
 * $NCDId: @(#)GraphP.h,v 1.3 1993/05/15 01:16:12 greg Exp $
 */

#ifndef _GraphP_h
#define _GraphP_h

#include "Graph.h"
/* include superclass private header file */
#include <X11/CoreP.h>

/* define unique representation types not found in <X11/StringDefs.h> */

#define NO_GCS 		0
#define GRAPH_GC 	(1 << 0)
#define MARKER_GC 	(1 << 1)
#define POSITION_GC 	(1 << 2)
#define ALL_GCS 	(GRAPH_GC | MARKER_GC | POSITION_GC)

typedef struct
{
    int             empty;
}               GraphClassPart;

typedef struct _GraphClassRec
{
    CoreClassPart   core_class;
    GraphClassPart  graph_class;
}               GraphClassRec;

extern GraphClassRec graphClassRec;

typedef struct
{
    /* resources */
    Pixel           graphColor,
                    positionColor,
                    markerColor;
    GraphDataType  *data;
    XtCallbackList  leftProc,
                    rightProc;
    int             numSamples,
                    start,
                    end,
                    position,
                    numTracks,
                    leftMarker,
                    rightMarker;

    /* private state */
    GC              graphGC,			/* graphics context for
						 * foreground */
                    positionGC,
                    markerGC;
    int             marker,
                    leftMarkerX,
                    rightMarkerX;
    float           hscale,
                    vscale;
}               GraphPart;

typedef struct _GraphRec
{
    CorePart        core;
    GraphPart       graph;
}               GraphRec;

#endif						/* _GraphP_h */
