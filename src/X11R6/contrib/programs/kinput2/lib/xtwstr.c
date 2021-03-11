/*
 *	xtwstr.c
 */

/*
 * Copyright (c) 1989  Software Research Associates, Inc.
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
 *		ishisone@sra.co.jp
 */

#ifndef lint
static char	*rcsid = "$Id: xtwstr.c,v 2.3 1991/10/02 04:27:04 ishisone Rel $";
#endif

#include <X11/Intrinsic.h>
#include "WStr.h"
#include "XWStr.h"

#define IS2B(f)	(((f)->max_byte1 > 0) || ((f)->max_char_or_byte2 > 255))

XWSGC
XtWSGetGCSet(w, mask, values, fontG0, fontG1, fontG2, fontG3)
Widget w;
unsigned long mask;
XGCValues *values;
XFontStruct *fontG0;
XFontStruct *fontG1;
XFontStruct *fontG2;
XFontStruct *fontG3;
{
	XGCValues	gcval;
	XWSGC		gcset;
	int		i;

	gcset = (XWSGC)XtMalloc(sizeof(XWSGCSet));
	gcset->fe[0].font = fontG0;
	gcset->fe[1].font = fontG1;
	gcset->fe[2].font = fontG2;
	gcset->fe[3].font = fontG3;

	gcval = *values;
	mask |= GCFont;
	for (i = 0; i < 4; i++) {
		if (gcset->fe[i].font != NULL) {
			gcval.font = (gcset->fe[i].font)->fid;
			gcset->fe[i].gc = XtGetGC(w, mask, &gcval);
			gcset->fe[i].flag = GCCREAT;
			if (IS2B(gcset->fe[i].font))
				gcset->fe[i].flag |= TWOB;
		} else {
			gcset->fe[i].gc = NULL;
		}
	}

	return gcset;
}

void
XtWSDestroyGCSet(gcset)
XWSGC gcset;
{
	int	i;
	int	flag;

	for (i = 0; i < 4; i++) {
		if (gcset->fe[i].gc == NULL)
			continue;
		flag = gcset->fe[i].flag;
		if (flag & GCCREAT)
			XtDestroyGC(gcset->fe[i].gc);
		/* can't free XFontStruct data allocated by XWSSetGCSet()
		 * because I can't figure out which display is used.
		 * if (flag & FONTQUERY)
		 *	XFreeFont(???, gcset->fe[i].font);
		 */
	}
	XtFree((char *)gcset);
}

void
XtWSReleaseGCSet(w, gcset)
Widget w;
XWSGC gcset;
{
	int	i;
	int	flag;

	for (i = 0; i < 4; i++) {
		if (gcset->fe[i].gc == NULL)
			continue;
		flag = gcset->fe[i].flag;
		if (flag & GCCREAT)
			XtReleaseGC(w, gcset->fe[i].gc);
		if (flag & FONTQUERY)
			XFreeFont(XtDisplay(w), gcset->fe[i].font);
	}
	XtFree((char *)gcset);
}
