/* $Id: AdoptedShP.h,v 1.3 1991/03/22 11:41:38 ishisone Rel $ */
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

#ifndef _AdoptedShellP_h
#define _AdoptedShellP_h

#include <X11/ShellP.h>
#include "AdoptedShe.h"

typedef struct {
    int empty;
} AdoptedShellClassPart;

typedef struct _AdoptedShellClassRec {
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ShellClassPart	shell_class;
    OverrideShellClassPart	override_shell_class;
    AdoptedShellClassPart	adoptedshell_class;
} AdoptedShellClassRec;

extern AdoptedShellClassRec adoptedShellClassRec;

typedef struct {
    Window	parent;
    Boolean	disable_geometry;	/* disable geometry management */
    /* private */
    Boolean	colormap_specified;
} AdoptedShellPart;

typedef struct _AdoptedShellRec {
    CorePart		core;
    CompositePart	composite;
    ShellPart		shell;
    OverrideShellPart	override;
    AdoptedShellPart	adoptedshell;
} AdoptedShellRec;
#endif
