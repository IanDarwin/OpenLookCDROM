/* $XConsortium: sin_val.c,v 5.5 94/04/17 20:42:07 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * Default device specific routines for input.
 */

#include "phg.h"
#include "ws_type.h"
#include "sin.h"
#include "sin_priv.h"
#include <X11/Shell.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Label.h>

static void
set_float_arg( args, num_args, arg, val_p )
    Arg		*args;
    Cardinal	*num_args;
    String	arg;
    float	*val_p;
{
    if (sizeof(float) > sizeof(XtArgVal)) {
	XtSetArg(args[*num_args], arg, (XtArgVal)val_p); (*num_args)++;
    } else {
	XtArgVal	*arg_val = (XtArgVal *)val_p;
	XtSetArg(args[*num_args], arg, *arg_val); (*num_args)++;
    }
}

static void
valuator_jump( w, client_data, call_data )
    Widget	w;
    XtPointer	client_data;
    XtPointer	call_data;
{
    Arg				args[2];
    Cardinal			num_args = 0;
    char			buf[256];
    float			percent;
    Sin_input_device		*device = (Sin_input_device *)client_data;
    Sin_valuator_device_data	*data = &device->data.valuator;

    percent = *((float *) call_data);
    data->value = data->low + percent * (data->high - data->low);
    if ( device->item_handle.valuator.value ) {
	sprintf( buf, data->format, data->value );
	XtSetArg(args[num_args], XtNlabel, (XtArgVal)buf); num_args++;
	XtSetValues( device->item_handle.valuator.value, (ArgList)args,
	    num_args );
    }

    if ( device->mode == SIN_EVENT ) {
	unsigned	status;
	status = phg_sin_ws_enque_events( 1, &device );
	if ( SIN_EVENT_NOT_ENQUED(status) )
	    XBell( XtDisplay(w), 0 );
    }
    /* The action proc takes care of REQUEST.  No additional action needed
     * for SAMPLE.
     */
}

static void
enable_valuator( device )
    Sin_input_device	*device;
{
    Arg				args[20];
    Cardinal			num_args = 0;
    char			buf[256];
    float			init_value;
    Widget			w;
    Sin_valuator_device_data	*data = &device->data.valuator;
    Widget			parent = device->ws->shell;

    /* Create the containing shell. */
    sprintf( buf, "valuator%d", device->num );
    device->item_handle.valuator.shell =
	XtVaCreatePopupShell( buf, applicationShellWidgetClass,
	    parent,
	    /* Don`t override the resource database for these.
	    XtNwidth, (XtArgVal)SIN_EA_WIDTH(&device->echo_area),
	    XtNheight, (XtArgVal)SIN_EA_HEIGHT(&device->echo_area),
	    XtNx, (XtArgVal)SIN_EA_X(&device->echo_area),
	    XtNy, (XtArgVal)SIN_EA_Y(&device->echo_area),
	    */
	    NULL );

    /* Create the containing pane. */
    device->item_handle.valuator.pane =
	XtVaCreateManagedWidget( "pane", panedWidgetClass,
	    device->item_handle.valuator.shell,
	    NULL );

    /* Create the label. */
    if ( data->label && data->label[0] ) {
	device->item_handle.valuator.label =
	    XtVaCreateManagedWidget( "label", labelWidgetClass,
		device->item_handle.valuator.pane,
		XtNlabel, (XtArgVal)data->label,
		NULL );
    }

    /* Create the value readout. */
    if ( device->echo_sw && data->format && data->format[0] ) {
	sprintf( buf, data->format, data->value );
	device->item_handle.valuator.value =
	    XtVaCreateManagedWidget( "readout", labelWidgetClass,
		device->item_handle.valuator.pane,
		XtNlabel, (XtArgVal)buf,
		NULL );
    }

    /* Create the low range limit label. */
    if ( data->low_label && data->low_label[0] ) {
	sprintf( buf, data->low_label, data->low );
	device->item_handle.valuator.low =
	    XtVaCreateManagedWidget( "low_label", labelWidgetClass,
		device->item_handle.valuator.pane,
		XtNlabel, (XtArgVal)buf,
		NULL );
    }

    /* Create the scroll bar. */
    num_args = 0;
    init_value = (data->init_value - data->low) / (data->high - data->low);
    set_float_arg( args, &num_args, XtNtopOfThumb, &init_value );
    w = device->item_handle.valuator.scrollbar =
	XtCreateManagedWidget( "scrollbar", scrollbarWidgetClass,
	    device->item_handle.valuator.pane, args, num_args );
    XtAddCallback( device->item_handle.valuator.scrollbar,
	XtNjumpProc, valuator_jump, (XtPointer)device);

    /* Create the high range limit label. */
    if ( data->high_label && data->high_label[0] ) {
	sprintf( buf, data->high_label, data->high );
	device->item_handle.valuator.high =
	    XtVaCreateManagedWidget( "high_label", labelWidgetClass,
		device->item_handle.valuator.pane,
		XtNlabel, (XtArgVal)buf,
		NULL );
    }

    XtPopup( device->item_handle.valuator.shell, XtGrabNone );
    if ( device->mode == SIN_REQUEST_PENDING )
	XSaveContext( XtDisplay(w), XtWindow(w), phg_sin_device_context_id,
	    (caddr_t)device );
}

static void
disable_valuator( device )
    Sin_input_device	*device;
{
    Widget	w = device->item_handle.valuator.scrollbar;

    XDeleteContext( XtDisplay(w), XtWindow(w), phg_sin_device_context_id );
    XtDestroyWidget( device->item_handle.valuator.shell );
}

void
phg_sin_dev_boot_valuator( dev )
    Sin_input_device	*dev;
{
    switch ( dev->data.valuator.type ) {
	case WST_VAL_TYPE_SLIDER:
	    dev->dev_ops.reset = NULL;
	    dev->dev_ops.sample = NULL;
	    dev->dev_ops.resize = NULL;
	    dev->dev_ops.repaint = NULL;
	    dev->dev_ops.destroy = NULL;
	    dev->dev_ops.create = NULL;
	    dev->dev_ops.init = NULL;
	    dev->dev_ops.enable = enable_valuator;
	    dev->dev_ops.disable = disable_valuator;
	    break;
    }
}
