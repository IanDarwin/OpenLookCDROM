/* $XConsortium: ThermoP.h,v 1.6 91/03/13 20:12:07 rws Exp $ */

/* Copyright	Massachusetts Institute of Technology	1987, 1988
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#ifndef _ThermoP_h
#define _ThermoP_h

#include "Thermo.h"
/* include superclass private header file */
#include <X11/Xaw/SimpleP.h>

/* define unique representation types not found in <X11/StringDefs.h> */

#define XtRThermoResource "ThermoResource"

typedef struct {
    int empty;
} ThermoClassPart;

typedef struct _ThermoClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    ThermoClassPart	thermo_class;
} ThermoClassRec;

extern ThermoClassRec thermoClassRec;

typedef struct {
    /* resources */
    XFontStruct	    *font;
    unsigned long   mercuryColor;
    unsigned long   textColor;
    unsigned long   tickColor;
    int		    current;
    int		    minimum;
    int		    maximum;
    int		    reqThickness;
    int		    reqMinorStart;
    int		    reqMajorStart;
    int		    reqMinorStep;
    int		    reqMajorStep;
    Dimension	    reqStartPad;
    Dimension	    reqEndPad;
    Dimension	    reqMajorTickLen;
    Dimension	    reqMinorTickLen;
    Boolean	    vertical;
    /* private state */
    int		    thickness;
    int		    minorStart;
    int		    majorStart;
    int		    minorStep;
    int		    majorStep;
    Dimension	    startPad;
    Dimension	    endPad;
    Dimension	    majorTickLen;
    Dimension	    minorTickLen;
    Dimension	    textWidth;
    GC		    mercuryGC;
    GC		    textGC;
    GC		    tickGC;
} ThermoPart;


typedef struct _ThermoRec {
    CorePart		core;
    SimplePart		simple;
    ThermoPart		thermo;
} ThermoRec;

#endif /* _ThermoP_h */
