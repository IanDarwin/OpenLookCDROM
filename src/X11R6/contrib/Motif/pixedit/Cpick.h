/*
 * Cpick.h - Color picker widget for Motif Toolkit
 * 
 * Author:	Mike Yang (mikey@sgi.com)
 *		Silicon Graphics, Inc.
 * Date:	Mon Jul 29 1991
 * Copyright (c) 1994 Mike Yang
 */

#ifndef _Cpick_h
#define _Cpick_h

#include <Xm/Xm.h>

/* yuck... hardware dependency */
#define MAXPIXELS 256

#define COLORLESSPIXELS 1
#define NECESSARYPIXELS (COLORLESSPIXELS+6)
#define HNEARRATIO 1
#define VNEARRATIO 1

typedef struct _CpickRec *CpickWidget;
typedef struct _CpickClassRec *CpickWidgetClass;

#define XmNselectProc	"selectproc"
#define XmNokProc	"okproc"
#define XmNhelpProc	"helpproc"
#define XmNchangeProc	"changeproc"
#define XmNrestoreProc	"restoreproc"
#define XmNallocated	"allocated"
#define XmNcmap		"cmap"
#define XmNselectLabel	"selectlabel"
#define XmNcancelLabel	"cancellabel"
#define XmNrestoreLabel	"restorelabel"
#define XmNokLabel	"oklabel"
#define XmNhelpLabel	"helplabel"
#define XmNnearPixels	"nearpixels"
#define XmNuseColors	"usecolors"

#define XmCAllocated	"Allocated"
#define XmCCmap		"Cmap"
#define XmCUsecolors	"Usecolors"

#define XmRXColor	"xcolor"
#ifndef XmRColormap
#define XmRColormap	"colormap"
#endif

extern WidgetClass cpickWidgetClass;

extern Widget CpickGetBoxFrame(Widget w);

#endif /* _Cpick_h */
