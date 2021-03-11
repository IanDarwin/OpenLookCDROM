/*
 * CpickP.h - Color picker widget for Motif Toolkit
 * 
 * Author:	Mike Yang (mikey@sgi.com)
 *		Silicon Graphics, Inc.
 * Date:	Mon Jul 29 1991
 * Copyright (c) 1994 Mike Yang
 */

#ifndef _CpickP_h
#define _CpickP_h

#include "Cpick.h"
#include <Xm/ManagerP.h>

#define R 0
#define G 1
#define B 2
#define H 3
#define S 4
#define V 5
#define C 6
#define M 7
#define Y 8
#define NUM Y+1

#define NARROW 0
#define WIDE 1
#define RANGE 2

#define MAXNAME 80

typedef struct {
  XtCallbackList selectProc;
  XtCallbackList okProc;
  XtCallbackList helpProc;
  XtCallbackList changeProc;
  XtCallbackList restoreProc;
  XColor *allocated;
  String selectlabel, cancellabel, restorelabel, oklabel, helplabel;
  Dimension nearpixels;
  Boolean usecolors;
  Colormap cmap;
  Widget tlevel, scaleSets, names[NUM], scales[NUM], labels[NUM], boxButtons;
  Widget hexText, paletteButton, matchButton, box, commandBox, nlevel;
  Widget select0, cancel, restore, ok, help, mlabel, mframe, bframe;
  Widget bcontainer;
  char mnames[MAXPIXELS][MAXNAME];
  int values[NUM], inc, mdist[MAXPIXELS];
  Boolean keep, matched;
  int wide;
  XColor oldvalue, nearcells[MAXPIXELS];
  Dimension oldHeight;
} CpickPart;

typedef struct _CpickRec {
  CorePart core;
  CompositePart composite;
  ConstraintPart constraint;
  XmManagerPart manager;
  CpickPart cpick;
} CpickRec;

typedef struct _CpickClass {
  int make_compiler_happy;
} CpickClassPart;

typedef struct _CpickClassRec {
  CoreClassPart core_class;
  CompositeClassPart composite_class;
  ConstraintClassPart constraint_class;
  XmManagerClassPart manager_class;
  CpickClassPart cpick_class;
} CpickClassRec;

#endif /* _CpickP_h */
