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
 * $NCDId: @(#)SliderP.h,v 1.1 1994/02/09 01:47:38 greg Exp $
 */

#ifndef _SliderP_h
#define _SliderP_h

#include <X11/Xaw/FormP.h>
#include "Slider.h"

typedef struct
{
    int             empty;
}               SliderClassPart;

typedef struct _SliderClassRec
{
    CoreClassPart   core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    FormClassPart   form_class;
    SliderClassPart slider_class;
}               SliderClassRec;

extern SliderClassRec sliderClassRec;

typedef struct _SliderPart
{
    /* resources */
    String          label;
    int             value,
                    min,
                    max;
    XtCallbackList  callbacks;

    /* private data */
    Widget          labelW,
                    scrollbarW;
}               SliderPart;

typedef struct _SliderRec
{
    CorePart        core;
    CompositePart   composite;
    ConstraintPart  constraint;
    FormPart        form;
    SliderPart      slider;
}               SliderRec;

typedef struct
{
    int             empty;
}               SliderConstraintsPart;

typedef struct _SliderConstraintsRec
{
    FormConstraintsPart form;
    SliderConstraintsPart slider;
}               SliderConstraintsRec, *SliderConstraints;

#endif						/* _SliderP_h */
