/*
 *      olmotif.c
 *      Motif demo program that sets certain Open
 *	Look Window Manager (olwm) properties.
 *
 *	To be simple, written for Motif 1.0
 *	and the version of olwm that came on
 *	the MIT R4 tape (a very old version of
 *	olwm).
 *
 *	Copyright 1991 by Eric F. Johnson and Kevin Reichard,
 *	All Rights Reserved.
 *
 */

#include  <stdio.h>
#include  <X11/Intrinsic.h>
#include  <X11/StringDefs.h>
#include  <X11/Xatom.h> 
#include  <Xm/Xm.h>
#include  <Xm/PushB.h>


/*
 *	Global Atom ID
 */
Atom	ol_pin_state;	/* _OL_PIN_STATE */


void event_handler( widget, client_data, event )

Widget		widget;
caddr_t		client_data;
XPropertyEvent	*event;

/*
 *	Event handler for PropertyNotify events.
 */

{	/* event_handler */
	int		status;
	int		*data;
	Atom		actual_type;
	int		actual_format;
        unsigned long   number_items;
        unsigned long   bytesafter;

	/*
	 * Check if its our property.
	 */
	if ( ( event->state == PropertyNewValue ) &&
		( event->atom == ol_pin_state ) )
		{
		/*
		 * If so, read in values.
		 */
		status = XGetWindowProperty( XtDisplay( widget ),
				XtWindow( widget ), 
				ol_pin_state, 0, 1,
        			False, 
				XA_INTEGER, 
				&actual_type, 
				&actual_format, 
				&number_items,
        			&bytesafter, 
				&data );

		if ( status == Success )
			{
			if ( *data == 1 )
				{
				printf( "The pin is in\n" );
				}
			else
				{
				printf( "The pin is out\n" );
				}

			XFree( data );
			}
		}

}	/* event_handler */

void quit_callback( widget, client_data, call_data )

Widget  widget;
caddr_t client_data;
caddr_t call_data;

/*
 *      Callback function to quit program.
 */

{       /* quit_callback */

        exit( 0 );

}       /* quit_callback */

main( argc, argv )

int	argc;
char	*argv[];

{	/* main */
	Widget		parent, quit_widget;
	Arg		args[10];
	int		n;

        parent = XtInitialize( argv[0],
                        "Olmotif", NULL,
                        0, &argc, argv );

        n = 0;
        XtSetArg( args[n], XmNallowShellResize, True ); n++;

        /*
         * We don't want to map our top-level window
         * until we are done setting property values
         * for olwm.
         */
        XtSetArg( args[n], XmNmappedWhenManaged, False ); n++;

        XtSetValues( parent, args, n );

	n = 0;
	quit_widget = XmCreatePushButton( parent, "QuitProgram", args, n );

	XtManageChild( quit_widget );

        /*
         * Set up a callback function to be
         * called whenever the push button 
         * is "activated".
         */
        XtAddCallback( quit_widget, XmNactivateCallback,
                quit_callback, (caddr_t) NULL );

        /*
         * Realize the widget tree so that all the
         * windows are created.
         */
        XtRealizeWidget( parent );

	/*
	 * Now, set up olwm properties.
	 * If we aren't running under olwm, these
	 * extra properties really won't matter.
 	 */
	set_olwm_props( XtDisplay( parent ), XtWindow( parent ) );


	/*
	 * When we're done, map our parent widget
	 */
	XtMapWidget( parent );

	/*
	 * Create global pin-state atom
	 */
        ol_pin_state    = XInternAtom( XtDisplay( parent ), 
				"_OL_PIN_STATE", False );

	/*
	 * Create an event handler to capture PropertyNotify
	 * events. Note that this may interfere with other
	 * PropertyNotify events that Motif may want to handle.
	 */
	XtAddEventHandler( parent, PropertyChangeMask, False,
		event_handler, (caddr_t) NULL );

        XtMainLoop();

}	/* main */

set_olwm_props( display, window )

Display *display;
Window  window;

/*
 *      Sets window manager properties for olwm, the Open Look
 *      window manager. You should call this before the window
 *      is mapped.
 */

{       /* set_olwm_props */
        Atom    ol_decor_del;           /* _OL_DECOR_DEL */
        Atom    ol_decor_resize;        /* _OL_DECOR_RESIZE */
        Atom    ol_decor_close;         /* _OL_DECOR_CLOSE */
        Atom    ol_decor_pin;           /* _OL_DECOR_PIN */
        Atom    ol_decor_add;           /* _OL_DECOR_ADD */
        Atom	attributes[20];

        /*
         * First, create Atoms. We should check for
         * errors here.
         */
        ol_decor_add    = XInternAtom( display, "_OL_DECOR_ADD", False );
        ol_decor_pin    = XInternAtom( display, "_OL_DECOR_PIN", False );
        ol_decor_del    = XInternAtom( display, "_OL_DECOR_DEL", False );
        ol_decor_resize = XInternAtom( display, "_OL_DECOR_RESIZE", False );
        ol_decor_close  = XInternAtom( display, "_OL_DECOR_CLOSE", False );

        /*
         * Now, remove the resize corners and close box from our window
         */
        attributes[0] = ol_decor_resize;
        attributes[1] = ol_decor_close;

        XChangeProperty( display, window,
                ol_decor_del,
                XA_ATOM,
                32,
                PropModeAppend,
                attributes,
                2 );

	/*
	 * Add a pushpin. Who says Motif doesn't have
	 * pushpins? Normally, though, you'll want to
	 * place pushpins on pop-up windows like dialogs and
	 * menus, rather than on the top-level window.
	 */
        XChangeProperty( display, window,
                ol_decor_add,
                XA_ATOM,
                32,
                PropModeAppend,
                &ol_decor_pin,
                1 );

}       /* set_olwm_props */


/*
 *	end of file
 */
