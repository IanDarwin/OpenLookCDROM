/* $Id: AdoptedShe.h,v 1.1 1990/11/09 11:16:51 ishisone Rel $ */
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

#ifndef _AdoptedShell_h
#define _AdoptedShell_h

/* AdoptedShell widget public header file */

#define XtNparentWindow "parentWindow"
#define XtCParentWindow "ParentWindow"
#define XtNdisableGeometryManagement "disableGeometryManagement"
#define XtCDisableGeometryManagement "DisableGeometryManagement"

typedef struct _AdoptedShellClassRec *AdoptedShellWidgetClass;
typedef struct _AdoptedShellRec *AdoptedShellWidget;

extern WidgetClass adoptedShellWidgetClass;
#endif
