/* $Id: CanvasSheP.h,v 1.3 1991/03/22 13:15:11 ishisone Rel $ */
/*
 * Copyright (c) 1990  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

#ifndef _CanvasShellP_h
#define _CanvasShellP_h

#include "AdoptedShP.h"
#include "CanvasShel.h"

typedef struct {
    int empty;
} CanvasShellClassPart;

typedef struct _CanvasShellClassRec {
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ShellClassPart	shell_class;
    OverrideShellClassPart	override_shell_class;
    AdoptedShellClassPart	adoptedshell_class;
    CanvasShellClassPart	canvasshell_class;
} CanvasShellClassRec;

extern CanvasShellClassRec canvasShellClassRec;

typedef struct {
    Cursor		cursor;
    XtCallbackList	exposecallback;
    XtCallbackList	resizecallback;
} CanvasShellPart;

typedef struct _CanvasShellRec {
    CorePart		core;
    CompositePart	composite;
    ShellPart		shell;
    OverrideShellPart	override;
    AdoptedShellPart	adoptedshell;
    CanvasShellPart	canvasshell;
} CanvasShellRec;
#endif
