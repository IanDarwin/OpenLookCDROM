/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 *
 *   The X Consortium, and any party obtaining a copy of these files from
 *   the X Consortium, directly or indirectly, is granted, free of charge, a
 *   full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 *   nonexclusive right and license to deal in this software and
 *   documentation files (the "Software"), including without limitation the
 *   rights to use, copy, modify, merge, publish, distribute, sublicense,
 *   and/or sell copies of the Software, and to permit persons who receive
 *   copies from any such party to do so.  This license includes without
 *   limitation a license to do the foregoing actions under any patents of
 *   the party supplying this software to the X Consortium.
 */

/* text_form.c -- demonstrate how attachments work in Form widgets
 * by creating a text-entry form type application.
 */

#include <Xm/LabelG.h>
#include <Xm/Text.h>
#include <Xm/Form.h>

char *prompts[] = {
    "Name:", "Phone:", "Address:",
    "City:", "State:", "Zip Code:",
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, mainform, subform, label, text;
    XtAppContext app;
    char buf[32];
    int i;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    mainform = XtVaCreateWidget ("mainform",
        xmFormWidgetClass, toplevel,
        NULL);

    for (i = 0; i < XtNumber (prompts); i++) {
        subform = XtVaCreateWidget ("subform",
            xmFormWidgetClass,   mainform,
            /* first one should be attached for form */
            XmNtopAttachment,    i ? XmATTACH_WIDGET : XmATTACH_FORM,
            /* others are attached to the previous subform */
            XmNtopWidget,        subform,
            XmNleftAttachment,   XmATTACH_FORM,
            XmNrightAttachment,  XmATTACH_FORM,
            NULL);
	/* Note that the label here contains a colon from the prompts
         * array above.  This makes it impossible for external resources
         * to be set on these widgets.  Here, that is intentional, but
         * be careful in the general case.
         */
        label = XtVaCreateManagedWidget (prompts[i],
            xmLabelGadgetClass,  subform,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_FORM,
            XmNalignment,        XmALIGNMENT_BEGINNING,
            NULL);
        sprintf (buf, "text_%d", i);
        text = XtVaCreateManagedWidget (buf,
            xmTextWidgetClass,   subform,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNrightAttachment,  XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_WIDGET,
            XmNleftWidget,       label,
            NULL);
        XtManageChild (subform);
    }
    /* Now that all the forms are added, manage the main form */
    XtManageChild (mainform);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
