/* $Id: ICLabel.h,v 1.3 1991/03/22 17:46:57 ishisone Rel $ */
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

#ifndef _ICLabel_h
#define _ICLabel_h

/* ICLabel widget public header file */
#include "ICtypes.h"

#define XtNcursor "cursor"
#define XtCSpacing "Spacing"
#define XtNhorizontalSpacing "horizontalSpacing"
#define XtNverticalSpacing "verticalSpacing"

typedef struct _ICLabelClassRec*	ICLabelWidgetClass;
typedef struct _ICLabelRec*		ICLabelWidget;

extern WidgetClass icLabelWidgetClass;

/*
 * public function:
 *	void ICLRecomputeSize(Widget widget)
 *		recompute size and make resize request.
 *		useful when changing child (convDisplayObject) attributes
 */
extern void ICLRecomputeSize(
#if NeedFunctionPrototypes
	Widget	/* widget */
#endif
);

#endif
