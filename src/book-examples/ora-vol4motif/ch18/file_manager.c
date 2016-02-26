/* Written by Paula Ferguson.  
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

/* file_manager.c -- displays all of the files in the current directory
 * and creates a drag source for each file.  The user can drag the 
 * contents of the file to another application that understands  
 * dropping file data.  Demonstrates creating a drag source, creating
 * drag icons, and handling data conversion.
 */
#include <Xm/Screen.h>
#include <Xm/ScrolledW.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/AtomMgr.h>
#include <Xm/DragDrop.h>
#include <X11/Xos.h>
#include <stdio.h>
#include <sys/stat.h>

typedef struct {
    char    *file_name;
    Boolean is_directory; 
} FileInfo;

/* global variable -- arbitrarily limit number of files to 256 */
FileInfo                files[256];

void                    StartDrag();

/* translations and actions.  Pressing mouse button 2 calls 
 * StartDrag to start a drag transaction */

static char dragTranslations[] = 
     "#override <Btn2Down>: StartDrag()";

static XtActionsRec dragActions[] = 
     { {"StartDrag", (XtActionProc) StartDrag} };

main (argc, argv)
int argc;
char *argv[];
{
    Arg                     args[10];
    int                     num_files, n, i = 0;
    Widget                  toplevel, sw, panel, form;
    Display                *dpy;
    Atom                    FILE_CONTENTS, FILE_NAME, DIRECTORY;
    XtAppContext            app;
    XtTranslations          parsed_trans;
    char                   *p, *buf[256];
    FILE                   *pp, *popen();
    struct stat             s_buf;
    Pixmap                  file, dir;
    Pixel                   fg, bg;
    
    XtSetLanguageProc (NULL, NULL, NULL);
    
    toplevel = XtAppInitialize (&app, "Demos", NULL, 0, &argc, argv, 
        NULL, NULL, 0);

    /* intern the Atoms for data targets */
    dpy = XtDisplay (toplevel);
    FILE_CONTENTS = XmInternAtom (dpy, "FILE_CONTENTS", False);
    FILE_NAME = XmInternAtom (dpy, "FILE_NAME", False);
    DIRECTORY = XmInternAtom (dpy, "DIRECTORY", False);

    /* use popen to get the files in the directory */
    sprintf (buf, "/bin/ls .");
    if (!(pp = popen (buf, "r"))) {
        perror (buf);
        exit (1);
    }
    /* read output from popen -- store filename and type */
    while (fgets (buf, sizeof (buf), pp) && (i < 256)) {
        if (p = index (buf, '\n'))
            *p = 0;
        if (stat (buf, &s_buf) == -1)
            continue;
        else if ((s_buf.st_mode &S_IFMT) == S_IFDIR)
            files[i].is_directory = True;
        else if (!(s_buf.st_mode & S_IFREG))
            continue;
        else
            files[i].is_directory = False;
        files[i].file_name = XtNewString (buf);
        i++;
    }
    pclose (pp);
    num_files = i;

    /* create a scrolled window to contain the file labels */
    sw = XtVaCreateManagedWidget ("sw", 
        xmScrolledWindowWidgetClass, toplevel,
        XmNwidth, 200,
        XmNheight, 300,
        XmNscrollingPolicy, XmAUTOMATIC,
        NULL);

    panel = XtVaCreateWidget ("panel", xmRowColumnWidgetClass, sw, NULL);

    /* get foreground and background colors and create label pixmaps */
    XtVaGetValues (panel,
        XmNforeground, &fg,
        XmNbackground, &bg,
        NULL);
    file = XmGetPixmap (XtScreen (panel), "file.xbm", fg, bg);
    dir = XmGetPixmap (XtScreen (panel), "dir.xbm", fg, bg);
    if (file == XmUNSPECIFIED_PIXMAP || dir == XmUNSPECIFIED_PIXMAP) {
        puts ("Couldn't load pixmaps");
        exit (1);
    }

    parsed_trans = XtParseTranslationTable (dragTranslations); 
    XtAppAddActions (app, dragActions, XtNumber (dragActions));

    /* create image and filename Labels for each file */
    for (i = 0; i < num_files; i++) {
        form = XtVaCreateWidget ("form", xmFormWidgetClass, panel, NULL);
        XtVaCreateManagedWidget ("type", xmLabelWidgetClass, form,
            /* specify translation for drag and index into file array */
            XmNtranslations, parsed_trans,
            XmNuserData, i,
            XmNlabelType, XmPIXMAP,
            XmNlabelPixmap, files[i].is_directory ? dir : file,
            XmNtopAttachment, XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNleftAttachment, XmATTACH_FORM,
            XmNrightAttachment, XmATTACH_POSITION,
            XmNrightPosition, 25,
            NULL);
        XtVaCreateManagedWidget (files[i].file_name,
            xmLabelWidgetClass, form,
            XmNalignment, XmALIGNMENT_BEGINNING,
            XmNtopAttachment, XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNrightAttachment, XmATTACH_FORM,
            XmNleftAttachment, XmATTACH_POSITION,
            XmNleftPosition, 25,
            NULL);
        XtManageChild (form);
    }
 
    XtManageChild (panel);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* StartDrag() -- action routine called by the initiator when a drag starts 
 * (in this case, when mouse button 2 is pressed).  It starts 
 * the drag processing and establishes a drag context. 
 */
void 
StartDrag(widget, event, params, num_params)
Widget  widget;
XEvent  *event;
String *params;
Cardinal *num_params;
{
    Arg             args[10];
    int             n, i;
    Display        *dpy;
    Atom            FILE_CONTENTS, FILE_NAME, DIRECTORY;
    Atom            exportList[2];
    Widget          drag_icon, dc;
    Pixel           fg, bg;
    Pixmap          icon, iconmask;
    XtPointer       ptr;
    Boolean         ConvertProc();
    void            DragDropFinish();

    /* intern the Atoms for data targets */
    dpy = XtDisplay (widget);
    FILE_CONTENTS = XmInternAtom (dpy, "FILE_CONTENTS", False);
    FILE_NAME = XmInternAtom (dpy, "FILE_NAME", False);
    DIRECTORY = XmInternAtom (dpy, "DIRECTORY", False);

    /* get background and foreground colors and fetch index into file 
     * array from XmNuserData.
     */
    XtVaGetValues (widget, 
        XmNbackground, &bg,
        XmNforeground, &fg,
        XmNuserData,  &ptr,
        NULL);

    /* create pixmaps for drag icon -- either file or directory */
    i = (int) ptr;
    if (files[i].is_directory) {
        icon = XmGetPixmapByDepth (XtScreen (widget), "dir.xbm", 1, 0, 1);
        iconmask = XmGetPixmapByDepth (XtScreen (widget), "dirmask.xbm", 
            1, 0, 1);
    }
    else {
        icon = XmGetPixmapByDepth (XtScreen (widget), "file.xbm", 1, 0, 1);
        iconmask = XmGetPixmapByDepth (XtScreen (widget), "filemask.xbm", 
            1, 0, 1);
    }
    if (icon == XmUNSPECIFIED_PIXMAP || iconmask == XmUNSPECIFIED_PIXMAP) {
        puts ("Couldn't load pixmaps");
        exit (1);
    }

    n = 0;
    XtSetArg (args[n], XmNpixmap, icon); n++;
    XtSetArg (args[n], XmNmask, iconmask); n++;
    drag_icon = XmCreateDragIcon (widget, "drag_icon", args, n);
    
    /* specify resources for DragContext for the transfer */
    n = 0;
    XtSetArg (args[n], XmNblendModel, XmBLEND_JUST_SOURCE); n++;
    XtSetArg (args[n], XmNcursorBackground, bg); n++;
    XtSetArg (args[n], XmNcursorForeground, fg); n++;
    XtSetArg (args[n], XmNsourceCursorIcon, drag_icon); n++; 
    /* establish the list of valid target types */
    if (files[i].is_directory) {
        exportList[0] = DIRECTORY;
        XtSetArg (args[n], XmNexportTargets, exportList); n++;
        XtSetArg (args[n], XmNnumExportTargets, 1); n++;
    }
    else {
        exportList[0] = FILE_CONTENTS;
        exportList[1] = FILE_NAME;
        XtSetArg (args[n], XmNexportTargets, exportList); n++;
        XtSetArg (args[n], XmNnumExportTargets, 2); n++;
    }
    XtSetArg (args[n], XmNdragOperations, XmDROP_COPY); n++;
    XtSetArg (args[n], XmNconvertProc, ConvertProc); n++;
    XtSetArg (args[n], XmNclientData, widget); n++;

    /* start the drag and register a callback to clean up when done */
    dc = XmDragStart (widget, event, args, n);
    XtAddCallback (dc, XmNdragDropFinishCallback, DragDropFinish, NULL);
}

/* ConvertProc() -- convert the file data to the format requested
 * by the drop site.
 */
Boolean 
ConvertProc(widget, selection, target, type_return, value_return, 
        length_return, format_return)
Widget              widget;
Atom                *selection;
Atom                *target;
Atom                *type_return;
XtPointer           *value_return;
unsigned long       *length_return;
int                 *format_return;
{
    Display    *dpy;
    Atom        FILE_CONTENTS, FILE_NAME, MOTIF_DROP;
    XtPointer   ptr;
    Widget      label;
    int         i;
    char       *text;
    struct stat s_buf;
    FILE       *fp;
    long        length;
    String      str;

    /* intern the Atoms for data targets */
    dpy = XtDisplay (widget);
    FILE_CONTENTS = XmInternAtom (dpy, "FILE_CONTENTS", False);
    FILE_NAME = XmInternAtom (dpy, "FILE_NAME", False);
    MOTIF_DROP = XmInternAtom (dpy, "_MOTIF_DROP", False);

    /* check if we are dealing with a drop */
    if (*selection != MOTIF_DROP)
        return False;

    /* get the drag source widget */
    XtVaGetValues (widget, XmNclientData, &ptr, NULL);
    label = (Widget) ptr;
    
    if (label == NULL)
        return False;

    /* get the index into the file array from XmNuserData from the
     * drag source widget.
     */
    XtVaGetValues (label, XmNuserData, &ptr, NULL);
    i = (int) ptr;
    
    /* this routine processes only file contents and file name */
    if (*target == FILE_CONTENTS) {
        /* get the contents of the file */
        if (stat (files[i].file_name, &s_buf) == -1 ||
                (s_buf.st_mode & S_IFMT) != S_IFREG ||
                !(fp = fopen (files[i].file_name, "r"))) 
            return False;

        length = s_buf.st_size;
        if (!(text = XtMalloc ((unsigned) (length + 1))))
            return False;
        else if (fread (text, sizeof (char), length, fp) != length)
            return False;
        else
            text[length] = 0;
        fclose (fp);

        /* format the value for transfer */
        *type_return = FILE_CONTENTS;
        *value_return = (XtPointer) text;
        *length_return = length;
        *format_return = 8;
        return True;
    }
    else if (*target == FILE_NAME) {
        str = XtNewString (files[i].file_name);
        
        /* format the value for transfer */
        *type_return = FILE_NAME;
        *value_return = (XtPointer) str;
        *length_return = strlen (str) + 1;
        *format_return = 8;
        return True;
    }
    else
        return False;
}

/* DragDropFinish() -- clean up after a drag and drop transfer.
 */
void
DragDropFinish (widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    Widget      source_icon = NULL;

    XtVaGetValues (widget, XmNsourceCursorIcon, &source_icon, NULL);

    if (source_icon)
        XtDestroyWidget (source_icon);
}
