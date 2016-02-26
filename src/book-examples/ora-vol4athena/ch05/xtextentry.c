/*
 * xtextentry.c - simplified single-line text entry
 *
 *    The X Consortium, and any party obtaining a copy of these files from
 *    the X Consortium, directly or indirectly, is granted, free of charge, a
 *    full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 *    nonexclusive right and license to deal in this software and
 *    documentation files (the "Software"), including without limitation the
 *    rights to use, copy, modify, merge, publish, distribute, sublicense,
 *    and/or sell copies of the Software, and to permit persons who receive
 *    copies from any such party to do so.  This license includes without
 *    limitation a license to do the foregoing actions under any patents of
 *    the party supplying this software to the X Consortium.
 */

/*
 * Include files required for all Toolkit programs
 */
#include <X11/Intrinsic.h>     /* Intrinsics Definitions*/
#include <X11/StringDefs.h>    /* Standard Name-String definitions*/

/*
 * Public include file for widgets we actually use in this file.
 */
#include <X11/Xaw/AsciiText.h>  
#include <X11/Xaw/Form.h>  
#include <X11/Xaw/Box.h>  
#include <X11/Shell.h>  
#include <X11/Xaw/Label.h>  
#include <X11/Xaw/Command.h> 
#include <X11/Xaw/Dialog.h>    

Widget address1, address2, pshell, pressme, quit;

/*
 * quit button callback function
 */
/*ARGSUSED*/
void Quit(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
        exit(0);
}

/*
 * address button
 */
/*ARGSUSED*/
void PopupDialog(w, client_data, call_data)
Widget w;
XtPointer client_data; /* cast to topLevel */
XtPointer call_data;
{
        Widget topLevel = (Widget) client_data;
        Position x, y;
        Dimension width, height;

        /*
         * get the coordinates of the middle of topLevel widget.
         */
        XtVaGetValues(topLevel,
                        XtNwidth, &width,
                        XtNheight, &height,
                        NULL);
	/*
         * translate coordinates in application top-level window
         * into coordinates from root window origin.
         */
        XtTranslateCoords(topLevel,                /* Widget */
                (Position) width/2,        /* x */
                (Position) height/2,       /* y */
                &x, &y);          /* coords on root window */

        /* move popup shell to this position (it's not visible yet) */
        XtVaSetValues(pshell,
                        XtNx, x,
                        XtNy, y,
                        NULL);

        /*
         * Indicate to user that no other application functions are
         * valid while dialog is popped up...
         */
        XtSetSensitive(pressme, FALSE);
        XtSetSensitive(quit, FALSE);

        XtPopup(pshell, XtGrabNonexclusive);
}


/* OK button */
/* ARGSUSED */
void DialogDone(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
	Widget dialog = (Widget) client_data;
	String string1;
	String string2;

	XtPopdown(pshell);

	XtSetSensitive(pressme, TRUE);
	XtSetSensitive(quit, TRUE);

	XtVaGetValues(address1, XtNstring, &string1, NULL);
	XtVaGetValues(address2, XtNstring, &string2, NULL);

	printf("User's address is:\n%s\n%s\n", string1, string2);
}

/* Cancel button */
/* ARGSUSED */
void DialogCancel(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
	XtPopdown(pshell);
	XtSetSensitive(pressme, TRUE);
	XtSetSensitive(quit, TRUE);
}

/* ARGSUSED */
void ReturnAction1(w,event,params,num_params)
	Widget w;
	XEvent *event;         /* unused */
	String *params;         /* unused */
	Cardinal *num_params;   /* unused */
{
	/* For return in first field. */
	/* Should move pointer and focus to next field. */
}

/* ARGSUSED */
void ReturnAction2(w,event,params,num_params)
	Widget w;
	XEvent *event;         /* unused */
	String *params;         /* unused */
	Cardinal *num_params;   /* unused */
{
	/* For return in first field. */
	String string1;
	String string2;

	XtPopdown(pshell);

	XtVaGetValues(address1, XtNstring, &string1, NULL);
	XtVaGetValues(address2, XtNstring, &string2, NULL);

	printf("User's address is:\n%s\n%s\n", string1, string2);

	DialogCancel(w, NULL, NULL);
}

main(argc, argv)
int argc;
char **argv;
{
	XtAppContext app_context;
	Widget topLevel, box, form, dialogDone, dialogCancel, label;
	XtTranslations TextFieldTranslations;
	static XtActionsRec actions[] = {
		{ "ReturnAction1", ReturnAction1 },
		{ "ReturnAction2", ReturnAction2 },
	};

        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    	topLevel = XtVaAppInitialize(
        	&app_context,       /* Application context */
        	"XTextEntry",         /* Application class */
        	NULL, 0,            /* command line option list */
        	&argc, argv,        /* command line args */
        	NULL,               /* for missing app-defaults file */
        	NULL);              /* terminate varargs list */

	box = XtVaCreateManagedWidget(
		"box",			/* arbitrary widget name */
		boxWidgetClass,	/* widget class from Label.h */
		topLevel,			/* parent widget */
        	NULL);              /* terminate varargs list */

	pressme = XtVaCreateManagedWidget(
		"pressme",			/* arbitrary widget name */
		commandWidgetClass,	/* widget class from Label.h */
		box,			/* parent widget */
       		NULL);              /* terminate varargs list */

	quit = XtVaCreateManagedWidget(
		"quit",			/* arbitrary widget name */
		commandWidgetClass,	/* widget class from Label.h */
		box,			/* parent widget */
       		NULL);              /* terminate varargs list */

	pshell = XtVaCreateManagedWidget(
		"pshell", transientShellWidgetClass, topLevel,		
		XtNallowShellResize, True,
		XtNinput, True,
		XtNtransientFor, topLevel,
       		NULL);

	form = XtVaCreateManagedWidget(
		"form", formWidgetClass, pshell,		
       		NULL);

	label = XtVaCreateManagedWidget(
		"label", labelWidgetClass, form,		
		XtNborderWidth, 0,
       		NULL);

	address1 = XtVaCreateManagedWidget(
		"address1", asciiTextWidgetClass, form,		
		XtNeditType, XawtextEdit,
		XtNresizable, True,
		XtNresize, XawtextResizeBoth,
		XtNfromVert, label,
		XtNright, XawChainRight,
		XtNleft, XawChainLeft,
       		NULL);

	address2 = XtVaCreateManagedWidget(
		"address2", asciiTextWidgetClass, form,		
		XtNeditType, XawtextEdit,
		XtNresizable, True,
		XtNresize, XawtextResizeBoth,
		XtNfromVert, address1,
		XtNright, XawChainRight,
		XtNleft, XawChainLeft,
       		NULL);

	dialogDone = XtVaCreateManagedWidget(
		"dialogDone", commandWidgetClass, form,		
		XtNfromVert, address2,
       		NULL);

	dialogCancel = XtVaCreateManagedWidget(
		"dialogCancel", commandWidgetClass, form,		
		XtNfromVert, address2,
		XtNfromHoriz, dialogDone,
       		NULL);

	XtAddCallback(dialogDone, XtNcallback, DialogDone, (XtPointer) 0);
	XtAddCallback(dialogCancel, XtNcallback, DialogCancel, (XtPointer) 0);

	TextFieldTranslations = XtParseTranslationTable
		("<Key>Return: ReturnAction1()\n\
			Ctrl<Key>R: no-op(RingBell)\n\
			Ctrl<Key>S: no-op(RingBell)\n");

	XtOverrideTranslations(address1, TextFieldTranslations);

	TextFieldTranslations = XtParseTranslationTable
		("<Key>Return: ReturnAction2()\n\
			Ctrl<Key>R: no-op(RingBell)\n\
			Ctrl<Key>S: no-op(RingBell)\n");

	XtOverrideTranslations(address2, TextFieldTranslations);

	XtAppAddActions(app_context, actions, XtNumber(actions));

        XtAddCallback(quit, XtNcallback, Quit, 0);

        XtAddCallback(pressme, XtNcallback, PopupDialog, topLevel);

	/*
	 *  Create windows for widgets and map them.
	 */
	XtRealizeWidget(topLevel);

	/*
	 *  Loop for events.
	 */
	XtAppMainLoop(app_context);
}
