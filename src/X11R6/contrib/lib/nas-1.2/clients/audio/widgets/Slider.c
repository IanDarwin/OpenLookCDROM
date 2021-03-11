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
 * $NCDId: @(#)Slider.c,v 1.5 1994/05/27 02:37:56 greg Exp $
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Scrollbar.h>
#include "SliderP.h"

static XtResource resources[] = {
    {
	XtNlabel, XtCLabel, XtRString, sizeof(String),
	XtOffsetOf(SliderRec, slider.label), XtRString, NULL
    },
    {
	XtNvalue, XtCValue, XtRInt, sizeof(int),
	XtOffsetOf(SliderRec, slider.value), XtRInt, NULL
    },
    {
	XtNmin, XtCMin, XtRInt, sizeof(int),
	XtOffsetOf(SliderRec, slider.min), XtRInt, NULL
    },
    {
	XtNmax, XtCMax, XtRInt, sizeof(int),
	XtOffsetOf(SliderRec, slider.max), XtRInt, NULL
    },
    {
	XtNcallback, XtCCallback, XtRCallback, sizeof(XtPointer),
	XtOffsetOf(SliderRec, slider.callbacks), XtRCallback, (XtPointer) NULL
    },
};

static void     Initialize();
static Boolean  SetValues();

SliderClassRec  sliderClassRec =
{
    {						/* core_class fields */
	 /* superclass         */ (WidgetClass) & formClassRec,
	 /* class_name         */ "Slider",
	 /* widget_size        */ sizeof(SliderRec),
	 /* class_initialize   */ NULL,
	 /* class_part init    */ NULL,
	 /* class_inited       */ FALSE,
	 /* initialize         */ Initialize,
	 /* initialize_hook    */ NULL,
	 /* realize            */ XtInheritRealize,
	 /* actions            */ NULL,
	 /* num_actions        */ 0,
	 /* resources          */ resources,
	 /* num_resources      */ XtNumber(resources),
	 /* xrm_class          */ NULLQUARK,
	 /* compress_motion    */ TRUE,
	 /* compress_exposure  */ TRUE,
	 /* compress_enterleave */ TRUE,
	 /* visible_interest   */ FALSE,
	 /* destroy            */ NULL,
	 /* resize             */ XtInheritResize,
	 /* expose             */ XtInheritExpose,
	 /* set_values         */ SetValues,
	 /* set_values_hook    */ NULL,
	 /* set_values_almost  */ XtInheritSetValuesAlmost,
	 /* get_values_hook    */ NULL,
	 /* accept_focus       */ NULL,
	 /* version            */ XtVersion,
	 /* callback_private   */ NULL,
	 /* tm_table           */ NULL,
	 /* query_geometry     */ XtInheritQueryGeometry,
	 /* display_accelerator */ XtInheritDisplayAccelerator,
	 /* extension          */ NULL
    },
    {						/* composite_class fields */
	 /* geometry_manager   */ XtInheritGeometryManager,
	 /* change_managed     */ XtInheritChangeManaged,
	 /* insert_child       */ XtInheritInsertChild,
	 /* delete_child       */ XtInheritDeleteChild,
	 /* extension          */ NULL
    },
    {						/* constraint_class fields */
	 /* subresourses       */ NULL,
	 /* subresource_count  */ 0,
	 /* constraint_size    */ sizeof(SliderConstraintsRec),
	 /* initialize         */ NULL,
	 /* destroy            */ NULL,
	 /* set_values         */ NULL,
	 /* extension          */ NULL
    },
    {						/* form_class fields */
	 /* layout             */ XtInheritLayout
    },
    {						/* slider_class fields */
	 /* empty              */ 0
    }
};

WidgetClass     sliderWidgetClass = (WidgetClass) & sliderClassRec;

static void
setValue(w, value, setThumb, force)
SliderWidget    w;
int             value;
Boolean         setThumb,
                force;
{
    if (value < w->slider.min)
	value = w->slider.min;
    else if (value > w->slider.max)
	value = w->slider.max;

    if (value != w->slider.value || force)
    {
	char            buf[50];

	w->slider.value = value;

	if (w->slider.labelW && w->slider.label)
	{
	    sprintf(buf, w->slider.label, value);
	    XtVaSetValues(w->slider.labelW, XtNlabel, buf, NULL);
	}

	if (setThumb)
	    XawScrollbarSetThumb(w->slider.scrollbarW,
			     (float) w->slider.value / w->slider.max, -1.0);

	XtCallCallbacks((Widget) w, XtNcallback, (XtPointer) w->slider.value);
    }
}

static void
scroll(w, swp, positionp)
Widget          w;
XtPointer       swp,
                positionp;
{
    SliderWidget    sw = (SliderWidget) swp;
    int             position = (int) positionp;

    setValue(sw, sw->slider.value + (position > 0 ? -1 : 1), True, False);
}

static void
jump(w, swp, percentp)
Widget          w;
XtPointer       swp,
                percentp;
{
    SliderWidget    sw = (SliderWidget) swp;
    float          *percent = (float *) percentp;

    setValue(sw, (int) (*percent * sw->slider.max), False, False);
}

static void
Initialize(request, new, args, num_args)
Widget          request,
                new;
ArgList         args;
Cardinal       *num_args;
{
    SliderWidget    w = (SliderWidget) new;

    XtVaSetValues((Widget) w, XtNborderWidth, 0, XtNdefaultDistance, 0, NULL);

    if (w->slider.label)
    {
	w->slider.labelW =
	    XtCreateManagedWidget("label", labelWidgetClass, (Widget) w, NULL, 0);
	XtVaSetValues(w->slider.labelW,
		      XtNborderWidth, 0,
		      XtNhorizDistance, 0,
		      XtNleft, XtChainLeft,
		      XtNright, XtChainLeft,
		      NULL);
    }

    w->slider.scrollbarW =
	XtCreateManagedWidget("scrollbar", scrollbarWidgetClass, (Widget) w,
			      NULL, 0);
    XtVaSetValues(w->slider.scrollbarW,
		  XtNwidth, 100, XtNheight, 10,
		  XtNorientation, XtorientHorizontal,
		  XtNleft, XtChainLeft,
		  XtNright, XtChainRight,
		  XtNresizable, True,
		  XtNhorizDistance, 4,
		  XtNvertDistance, 4,
		  NULL);

    if (w->slider.labelW && w->slider.label)
	XtVaSetValues(w->slider.scrollbarW,
		      XtNfromHoriz, w->slider.labelW, NULL);

    XtAddCallback(w->slider.scrollbarW, XtNscrollProc, scroll, w);
    XtAddCallback(w->slider.scrollbarW, XtNjumpProc, jump, w);
    setValue(w, w->slider.value, True, True);
}

static Boolean
SetValues(current, request, new, in_args, in_num_args)
Widget          current,
                request,
                new;
ArgList         in_args;
Cardinal       *in_num_args;
{
    SliderWidget    w = (SliderWidget) new,
                    old = (SliderWidget) current;

    if (w->slider.value != old->slider.value)
	setValue(w, w->slider.value, True, True);

    if (w->core.sensitive != old->core.sensitive)
    {
	XtVaSetValues(w->slider.labelW, XtNsensitive, w->core.sensitive,
		      NULL);
	XtVaSetValues(w->slider.scrollbarW, XtNsensitive, w->core.sensitive,
		      NULL);
    }

    return False;
}
