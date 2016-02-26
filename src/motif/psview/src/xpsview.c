/*
 * An OSF/MOTIF X Interface to PsView
 *
 * Author:        François Bourdoncle
 * Organization:  DIGITAL Paris Research Laboratory
 * Email:         bourdoncle@prl.dec.com
 */

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#include <unistd.h>
#include <sys/wait.h>

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/CascadeBG.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/FileSB.h>
#include <Xm/Separator.h>
#include <Xm/SelectioB.h>
#include "version.h"

#define null '\0'
#define BUFLEN 1000

typedef enum {FullPage = 0, FullWidth = 1,
		FullHeight = 2, FixedScale = 3, Undefined = 4} Mode;

static Mode CurrentMode = Undefined;
static float CurrentScale = 0;
static int CurrentPage;
static float CurrentAngle;
static char CurrentMessage[100]  = "";
static char CurrentTitle[PATH_MAX]  = "";
static Bool CurrentWatchProgress;
static Bool CurrentBitmapImages;
static Bool CurrentAutoUpdate;
static Bool CurrentNext;
static Bool CurrentPrevious;
static Bool CurrentRevert;
static Bool CurrentPrint;
static int  CurrentSmoothScrolling;
static char *FileName = NULL;
static char buffer[BUFLEN];
static char FullFileName[PATH_MAX] = "";
static XmString ModeString[5];
static Window window = (Window) 0;
static Display *dpy;
static Widget ApplicationWidget, FormWidget, FileWidget, QuitWidget, 
  RevertWidget, MessageWidget, PreviousWidget, NextWidget, LastPageWidget,
  FirstPageWidget, AngleWidget, ScaleWidget, PageWidget, ModeWidget,
  ZoomWidget, AutoUpdateWidget, WatchProgressWidget, SmoothScrollingWidget,
  OpenWidget, CloseWidget, SaveWidget, SeparatorWidget, BitmapImagesWidget,
  AutoUpdateOption, BitmapImagesOption, WatchProgressOption, SavePixmapWidget,
  SmoothScrollingOption, PresentationOption, PrintWidget, UndoWidget,
  HelpWidget, PrintDialogWidget = (Widget) 0, PrintPage1Widget,
  PrintPage2Widget, LoadWidget = (Widget) 0, DPSWidget = (Widget) 0;

static XmString BitmapImagesString, AutoUpdateString,
  WatchProgressString, SmoothScrollingString;
static int Argc;
static char **Argv;
static Bool
  Exit = False,
  FirstTime = True,
  FirstPage = True,
  FirstScale = True,
  FirstAngle = True,
  FirstMode = True,
  FirstWatchProgress = True,
  FirstBitmapImages = True,
  FirstSmoothScrolling = True,
  FirstAutoUpdate = True,
  FirstRevert = True,
  FirstPrint = True,
  FirstNext = True,
  FirstPrevious = True;
static char *BoundingBox = "";
static int MinKeyCode, MaxKeyCode;

char *PredefinedFormats[] = {
  "Letter",
  "Tabloid",
  "Ledger",
  "Legal",
  "Statement",
  "Executive",
  "A3",
  "A4",
  "A5",
  "B4",
  "B5",
  "Folio",
  "Quarto",
  "10x14"};

/*
 * Prototypes
 */

static int BadWindowHandler (Display *dpy, XErrorEvent *error);
static int Child (void);
static int OptionValue (char *name, int def);
static void AddBBox (Widget popup, char *label, char *bbox);
static void AddBBoxes (Widget popup, char *bbox);
static void AddDefaultBBoxes (Widget popup);
static void AddUserBBoxes (Widget popup, char *class);
static void Alarm (void);
static void ArgsCallback (Widget widget,
			  XtPointer client_data, XtPointer call_data);
static void Beep (int n);
static void BoundingBoxCallback (Widget widget,
				 XtPointer client_data, XtPointer call_data);
static void ExposeCallback (Widget widget,
			    XtPointer client_data, XtPointer call_data);
static void FileCallback (Widget widget,
			  XtPointer client_data, XtPointer call_data);
static void KeyCallback (Widget widget,
			 XtPointer client_data, XtPointer call_data);
static void ModeCallback (Widget widget,
			  XtPointer client_data, XtPointer call_data);
static void OpenFile (char *file);
static void PrintCallback (Widget widget,
			   XtPointer client_data, XtPointer call_data);
static void PrintDialogCallback (Widget widget,
				 XtPointer client_data, XtPointer call_data);
static void Quit (void);
static void ResizeCallback (Widget widget,
			    XtPointer client_data, XtPointer call_data);
static void SelectionCallback (Widget widget,
			       XtPointer client_data, XtPointer call_data);
static void SendArgument (char *arg);
static void SendKey (char *s);
static void SetAngle (char *title);
static void SetAutoUpdate (int a);
static void SetMessage(char *title);
static void SetTitle(char *title);
static void SetWMName(char *name);
static void SetBitmapImages (int a);
static void SetButtons (int state);
static void SetMode (Mode mode);
static void SetNext (int a);
static void SetPage (char *title);
static void SetPrevious (int a);
static void SetPrint (int a);
static void SetRevert (int a);
static void SetScale (char *title);
static void SetSmoothScrolling (int s);
static void SetWatchProgress (int a);
static void SmoothCallback (Widget widget,
			    XtPointer client_data, XtPointer call_data);
static void Strip (char *buffer);
static void ToLower (char *to, int len, char *from);
static void ToggleCallback (Widget widget,
			    XtPointer client_data, XtPointer call_data);
static void WaitProc (Opaque closure, XtIntervalId *id);
int main (int argc, char *argv[]);

/*
 * Code
 */

static void ToLower(char *to, int len, char *from)
{
  int i=0;
  
  while (i++ < len && *from != null)
    if (isupper(*from))
      *to++ = tolower(*from++);
    else
      *to++ = *from++;
  *to = null;
}

static void Strip(char *buffer)
{
  int i;

  for (i = strlen(buffer) - 1 ; i > 0  && buffer[i] == '0'; i--);

  if (buffer[i] == '.')
    buffer[i] = null;
  else
    buffer[i + 1] = null;
}

static Bool OptionValue(char *name, Bool def)
{
  char *opt;
  
  if ((opt = XGetDefault(dpy, "PsView", name)) == NULL)
    return def;
  else {
    ToLower(buffer, BUFLEN, opt);
    
    return (strcmp(buffer, "yes") == 0 ||
	    strcmp(buffer, "on") == 0 ||
	    strcmp(buffer, "true") == 0);
  }
}

static Bool Child()
{
  Window w, dps;
  Window *ch;
  unsigned int n;

  /*
   * Check to see if there is a child window
   */
  
  if (DPSWidget != (Widget) 0 &&
      XQueryTree(dpy, XtWindow(DPSWidget), &w, &w, &ch, &n) != 0 &&
      n > 0){

    /*
     * If yes, set "window" equal to this window
     */
    
    window = ch[0];
    
    XFree((char*) ch);
    
    return True;
    
  } else {

    /*
     * Otherwise, if there was an opened file, then clear everything
     */
    
    if (window != (Window) 0) {

      XmTextFieldSetString(PageWidget, ""); 
      XmTextFieldSetString(ScaleWidget, "");
      XmTextFieldSetString(AngleWidget, "");

      SetMode(Undefined);

      SetButtons(False);
      
      FirstTime = True,
      FirstPage = True,
      FirstScale = True,
      FirstAngle = True,
      FirstMode = True,
      FirstWatchProgress = True,
      FirstBitmapImages = True,
      FirstAutoUpdate = True;
      FirstSmoothScrolling = True;
      FirstRevert = True;
      FirstPrint = True;
      FirstNext = True;
      FirstPrevious = True;

      SetWMName(NULL);
    }

    /*
     * If exit was enforced, then exit.
     */
    
    if (Exit)
      Quit();
    else
      window = (Window) 0;
    
    return False;
  }
}

static void Beep(int n)
{
  while (n-- > 0)
    XBell(XtDisplay(ApplicationWidget), 0);
}

/*
 * Send a command to the PsView window
 */

static void SendKey(char *s)
{
  KeySym keysym;
  KeyCode keycode;
  XEvent e;
  int i, mask = 0;
  
  if (Child()) {
    
    if (s[0] == '^' && s[1] != null) {
      mask = ControlMask;
      s++;
    } else
      mask = 0;

    if (s[1] == null)
      keysym = (KeySym) (s[0]);
    else if ((keysym = XStringToKeysym(s)) == NoSymbol)
      return;
    
    if ((keycode = XKeysymToKeycode(dpy, keysym)) != 0)  {

      for (i = 0 ; i < MaxKeyCode ; i++)
	if (XKeycodeToKeysym (dpy, keycode, i) == keysym)
	  break;
      
      if (i == 1)
	mask = mask | ShiftMask;
      
      e.xkey.window = window;
      e.xkey.display = dpy;
      e.xkey.send_event = True;
      e.xkey.keycode = keycode;
      e.xkey.state = mask;
      
      e.type = KeyPress;
      XSendEvent(dpy, window, False, 0, &e);
      
      e.type = KeyRelease;
      XSendEvent(dpy, window, False, 0, &e);
    }
  }
}

static void SendArgument(char *arg)
{
  int i, len;
  char letter[2];

  letter[1] = null;
  
  while (*arg != null) {
    *letter = *arg++;
    SendKey(letter);
  }
}

static void OpenFile(char *file)
{
  char **argv;
  int i;
  String string = NULL;

  if ((argv = (char**) XtMalloc(sizeof(char*) * (Argc + 14))) != NULL) {

    argv[0] = "psview";
    
    for (i = 1 ; i < Argc ; i++)
      argv[i] = Argv[i];

    if (file != NULL) {
      argv[i++] = file;
      argv[i++] = XmToggleButtonGetState(AutoUpdateOption) ?
	"-u+" : "-u-";
      argv[i++] = XmToggleButtonGetState(BitmapImagesOption) ?
	"-i+" : "-i-";
      argv[i++] = XmToggleButtonGetState(WatchProgressOption) ?
	"-W+" : "-W-";
      if (!XmToggleButtonGetState(WatchProgressOption))
	argv[i++] = XmToggleButtonGetState(SmoothScrollingOption) ?
	  "-L+" : "-L-";
      argv[i++] = XmToggleButtonGetState(PresentationOption) ?
	"-P+" : "-P-";
      string = "";
      argv[i++] = "-B";
      argv[i++] = BoundingBox;
    }
    argv[i++] = "-#";
    (void) sprintf(buffer, "%lu", XtWindow(DPSWidget));
    argv[i++] = buffer;
    argv[i++] = "-d";
    argv[i++] = DisplayString(XtDisplay(DPSWidget));
    argv[i++] = "-Q+";
    argv[i] = NULL;
    
    window = (Window) 0;
    
    if (vfork() == 0) {
      execvp("psview", argv);
      _exit(EXIT_FAILURE);
    }

    XtFree((char*) argv);
    if (string != NULL)
      XtFree(string);

  }
}

static void Quit()
{
  XtDestroyWidget(ApplicationWidget);
  exit(EXIT_SUCCESS);
}

/*
 * Callbacks
 */

static void KeyCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  SendKey((char*) client_data);
}

static void ToggleCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  if (widget == WatchProgressWidget) {
    SendKey("=");
    CurrentWatchProgress = ! CurrentWatchProgress;
  } else if (widget == AutoUpdateWidget) {
    SendKey("u");
    CurrentAutoUpdate = ! CurrentAutoUpdate;
  } else if (widget == SmoothScrollingWidget) {
    SendKey("L");
    CurrentSmoothScrolling = CurrentSmoothScrolling == 1 ? 0 : 1;
  } else if (widget == BitmapImagesWidget) {
    SendKey("i");
    CurrentBitmapImages = ! CurrentBitmapImages;
  } 
}

static void PrintDialogCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  XmFileSelectionBoxCallbackStruct *data;
  XmString string;
  XmStringContext context;
  XmStringCharSet charset;
  XmStringDirection direction;
  Boolean separator;
  char *p1, *p2, *command;
  int page1, page2;
  
  XtUnmanageChild(PrintDialogWidget);

  if (client_data && Child() &&
      (p1 = XmTextFieldGetString(PrintPage1Widget)) != NULL &&
      (p2 = XmTextFieldGetString(PrintPage2Widget)) != NULL &&
      (data = (XmFileSelectionBoxCallbackStruct*) call_data) != NULL &&
      (string = data->value) != NULL &&
      XmStringInitContext(&context, string) &&
      XmStringGetNextSegment(context, &command,
			     &charset, &direction, &separator)) {
      
    /*
     * We send:
     *
     *     "^P" <page1> "-" <page2> ":" <print command> "Escape"
     *
     * to PsView
     */
    
    SendKey("^P");
    (void) sprintf(buffer, "%d-%d:", atoi(p1), atoi(p2));
    SendArgument(buffer);
    SendArgument(command);
    SendKey("Escape");

    XtFree(p1);
    XtFree(p2);
  }
}

static void PrintCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  Widget work, form, label, command;
  char *print_command;
  
  if (PrintDialogWidget == (Widget) 0) {
    
    PrintDialogWidget = XmCreatePromptDialog(ApplicationWidget,
					     "print_dialog", NULL, 0);
    XtAddCallback(PrintDialogWidget,
		  XmNokCallback, PrintDialogCallback, (XtPointer) True);
    
    XtAddCallback(PrintDialogWidget,
		  XmNcancelCallback, PrintDialogCallback, (XtPointer) False);

    if ((print_command = XGetDefault(dpy, "PsView", "printCommand")) == NULL)
      print_command = "lpr %s";
    
    XtVaSetValues(PrintDialogWidget,
		  XmNselectionLabelString,
		  XmStringCreateSimple("Print command:"),
		  XmNtextString,
		  XmStringCreateSimple(print_command),
		  NULL);
    
    XtUnmanageChild(XmFileSelectionBoxGetChild(PrintDialogWidget,
					       XmDIALOG_HELP_BUTTON));
    
    work = XmCreateWorkArea(PrintDialogWidget, "pages", NULL, 0);

    XtVaSetValues(work, XmNtraversalOn, False, NULL);
    
    form = XmCreateForm(work, "print_form", NULL, 0);
    
    label =
      XtVaCreateManagedWidget("label",
			      xmLabelWidgetClass, form,
			      XmNleftAttachment, XmATTACH_FORM,
			      XmNleftOffset, 5,
			      XmNtopAttachment, XmATTACH_FORM,
			      XmNtopWidget, FileWidget,
			      XmNtopOffset, 5,
			      XmNlabelString,
			      XmStringCreateSimple("First page:"),
			      NULL);
    
    PrintPage1Widget =
      XtVaCreateManagedWidget("from",
			      xmTextFieldWidgetClass, form,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, label,
			      XmNleftOffset, 5,
			      XmNtopAttachment, XmATTACH_FORM,
			      XmNtopOffset, 5,
			      XmNeditable, True,
			      XmNmarginHeight, 2,
			      XmNmarginWidth, 4,
			      XmNhighlightThickness, 0,
			      XmNcursorPositionVisible, True,
			      NULL);
    
    label =
      XtVaCreateManagedWidget("label",
			      xmLabelWidgetClass, form,
			      XmNleftAttachment, XmATTACH_FORM,
			      XmNleftOffset, 5,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, PrintPage1Widget,
			      XmNtopOffset, 5,
			      XmNlabelString,
			      XmStringCreateSimple("Last page:"),
			      NULL);
    
    PrintPage2Widget =
      XtVaCreateManagedWidget("to",
			      xmTextFieldWidgetClass, form,
			      XmNleftAttachment, XmATTACH_WIDGET,
			      XmNleftWidget, label,
			      XmNleftOffset, 5,
			      XmNtopAttachment, XmATTACH_WIDGET,
			      XmNtopWidget, PrintPage1Widget,
			      XmNtopOffset, 5,
			      XmNeditable, True,
			      XmNmarginHeight, 2,
			      XmNmarginWidth, 4,
			      XmNhighlightThickness, 0,
			      XmNcursorPositionVisible, True,
			      NULL);

    XtManageChild(form);
    XtManageChild(work);

    XtManageChild(PrintDialogWidget);
    XStoreName(dpy, XtWindow(XtParent(PrintDialogWidget)), "Print Dialog");
    
  }
  
  (void) sprintf(buffer, "%d", CurrentPage);
  XmTextFieldSetString(PrintPage1Widget, buffer);
  XmTextFieldSetString(PrintPage2Widget, buffer);
  
  XtManageChild(PrintDialogWidget);
}

static void ArgsCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  char *arg;
  
  if ((arg = XmTextFieldGetString(widget)) != NULL) {
    
    if (arg[0] != null) {
      
      SendArgument(arg);
      
      if (widget == PageWidget) 
	SendKey("g");
      else if (widget == ScaleWidget)
	SendKey("s");
      else if (widget == AngleWidget)
      SendKey("R");
    }

    XtFree(arg);
    
  }
}

static void ResizeCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  XWindowAttributes att;

  if (Child()) {
    XGetWindowAttributes(dpy, XtWindow(DPSWidget), &att);
    XResizeWindow(dpy, window, att.width, att.height);
  }
}

static void ExposeCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  static Bool first = True;
  Bool load;
  int argc;
  char **argv;
    
  if (!first)
    return;

  first = False;
  load = False;
  
  argc = Argc - 1;
  argv = Argv + 1;
  
  while (argc > 0)
    if (argv[0][0] == '-' && argv[0][1] != null)
      switch (argv[0][1]) {
      case 'v':
      case 'C':
      case 'L':
      case 'G':
      case 'i':
      case 'P':
      case 'W':
      case 'U':
      case 'u':
      case 'f':
      case 'w':
      case 'h':
      case 'q':
	argc--;
	argv++;
	break;
      case 'g':
      case 'p':
      case 'B':
      case 's':
      case 't':
      case 'd':
      case 'R':
      case '#':
      case 'I':
      case 'e':
	  argc -= 2;
	  argv += 2;
	break;
      case 'b':
	argc -= 5;
	argv += 5;
	break;
      case 'S':
	argc -= 2;
	argv += 2;
	load = True;
	break;
      case 'H':
	load = True;
	argc--;
	argv++;
	break;
      default:
	argc--;
	argv++;
	break;
      } else {
	load = True;
	argc--;
	argv++;
      }
  
  if (load)
    OpenFile(NULL);
  
}

static void ModeCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  switch ((int)(client_data)) {
  case 0:
    SendKey("f");
    break;
  case 1:
    SendKey("w");
    break;
  case 2:
    SendKey("h");
    break;
  case 3:
    SendKey("s");
    break;
  }
}

static void SmoothCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  if (widget == WatchProgressOption) {
    if (XmToggleButtonGetState(WatchProgressOption)) {
      XmToggleButtonSetState(SmoothScrollingOption, False, False);
      XmToggleButtonSetState(PresentationOption, False, False);
    }
  } else if (XmToggleButtonGetState(widget))
    XmToggleButtonSetState(WatchProgressOption, False, False);
  
}

static void SelectionCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  XmFileSelectionBoxCallbackStruct *data;
  XmString string;
  XmStringContext context;
  XmStringCharSet charset;
  XmStringDirection direction;
  Boolean separator;

  XtUnmanageChild(LoadWidget);

  if ((int) client_data) {
    data = (XmFileSelectionBoxCallbackStruct*) call_data;
    string = data->value;
    
    if (XmStringInitContext(&context, string) &&
	XmStringGetNextSegment(context, &FileName,
			       &charset, &direction, &separator))
      
      SendKey("q");
    
  }
}

static void BoundingBoxCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  BoundingBox = (char*) client_data;
}

static void AddBBox(Widget popup, char *label, char *bbox)
{
  Widget w;
  
  w = XmCreatePushButton(popup, label, NULL, 0);
  XtAddCallback(w, XmNactivateCallback, BoundingBoxCallback, (XtPointer) bbox);
  XtVaSetValues(w, XmNalignment, XmALIGNMENT_CENTER, NULL);
  XtManageChild(w);
}

static void AddBBoxes(Widget popup, char *bbox)
{
  char *b, *next;
  Widget w;

  while (*bbox != null) {
    while (*bbox != null && *bbox != '(') 
      bbox++;
    if (*bbox == '(') {
      bbox++;
      for (b = bbox ; *b != null && *b != ')' ; b++);
      if (*b == null)
	next = b;
      else
	next = b + 1;
      *b = null;

      AddBBox(popup, bbox, bbox);
    
      bbox = next;
    }
  }
}
      
static void AddUserBBoxes(Widget popup, char *class)
{
  char *bbox, *tmp;
  
  if ((bbox = XGetDefault(dpy, class, "boundingBox")) != NULL &&
      (tmp = XtMalloc(strlen(bbox) + 1)) != NULL) {
    
    bbox = strcpy(tmp, bbox);
    AddBBoxes(popup, bbox);
  }
}

static void AddDefaultBBoxes(Widget popup)
{
  int i;
  
  for (i = 0 ;
       i < sizeof(PredefinedFormats)/sizeof(*PredefinedFormats) ;
       i++) {
    AddBBox(popup, PredefinedFormats[i], PredefinedFormats[i]);
  }
}

static void FileCallback(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  
  Widget work, option, frame, popup;
  
  switch ((int) client_data) {
  case 0:
    if (LoadWidget == (Widget) 0) {
      
      LoadWidget =
	XmCreateFileSelectionDialog(ApplicationWidget, "load", NULL, 0);

      XtAddCallback(LoadWidget,
		    XmNokCallback, SelectionCallback, (XtPointer) True);

      XtAddCallback(LoadWidget,
		    XmNcancelCallback, SelectionCallback, (XtPointer) False);
      
      XtVaSetValues(LoadWidget,
		    XmNselectionLabelString,
		    XmStringCreateSimple("File"),
		    XmNdirSpec,
		    XmStringCreateSimple(FullFileName),
		    NULL);
    
      XtUnmanageChild(XmFileSelectionBoxGetChild(LoadWidget,
						 XmDIALOG_HELP_BUTTON));

      frame = XmCreateFrame(LoadWidget, "frame", NULL, 0);
      
      work = XmCreateWorkArea(frame, "options", NULL, 0);

      XtVaSetValues(work, XmNtraversalOn, False, NULL);

      AutoUpdateOption =
	XmCreateToggleButton(work, "autoupdate", NULL, 0);

      BitmapImagesOption =
	XmCreateToggleButton(work, "images", NULL, 0);

      WatchProgressOption =
	XmCreateToggleButton(work, "watchprogress", NULL, 0);

      SmoothScrollingOption =
	XmCreateToggleButton(work, "smoothscrolling", NULL, 0);

      PresentationOption =
	XmCreateToggleButton(work, "presentation", NULL, 0);

      XtVaSetValues(AutoUpdateOption,
		    XmNlabelString, AutoUpdateString,
		    XmNset, OptionValue("autoUpdate", True),
		    NULL);
      XtVaSetValues(BitmapImagesOption,
		    XmNlabelString, BitmapImagesString,
		    XmNset, OptionValue("bitmapImages", True),
		    NULL);
      XtVaSetValues(WatchProgressOption,
		    XmNlabelString, WatchProgressString,
		    XmNset, OptionValue("watchProgress", True),
		    NULL);
      XtVaSetValues(SmoothScrollingOption,
		    XmNlabelString, SmoothScrollingString,
		    XmNset,
		    !OptionValue("watchProgress", True) &&
		    OptionValue("smoothScrolling", True),
		    NULL);
      XtVaSetValues(PresentationOption,
		    XmNlabelString, XmStringCreateSimple("Presentation Mode"),
		    XmNset, False,
		    NULL);

      XtAddCallback(WatchProgressOption,
		    XmNvalueChangedCallback, SmoothCallback,
		    (XtPointer) 0);

      XtAddCallback(SmoothScrollingOption,
		    XmNvalueChangedCallback, SmoothCallback,
		    (XtPointer) 0);

      XtAddCallback(PresentationOption,
		    XmNvalueChangedCallback, SmoothCallback,
		    (XtPointer) 0);
      
      option =
	XmVaCreateSimpleOptionMenu(work, "bbox",
				   XmStringCreateSimple("Bounding Box: "),
				   (KeySym) 0,
				   0, BoundingBoxCallback,
				   NULL);
      
      /*
       * BBox style names
       */
      
      XtVaGetValues(XmOptionButtonGadget(option), XmNsubMenuId, &popup, NULL);

      AddBBox(popup, "   Document's default   ", "");

      AddUserBBoxes(popup, "PsView");
      AddDefaultBBoxes(popup);

      XtManageChild(frame);
      XtManageChild(option);

      XtManageChild(BitmapImagesOption);
      XtManageChild(AutoUpdateOption);
      XtManageChild(WatchProgressOption);
      XtManageChild(SmoothScrollingOption);
      XtManageChild(PresentationOption);
      XtManageChild(work);
    }

    XtManageChild(LoadWidget);
    
    XStoreName(dpy, XtWindow(XtParent(LoadWidget)), "File Selection Dialog");
    
    break;
  case 1:
    SendKey("q");
    break;
  case 2:
    SendKey("S");
    break;
  case 3:
    SendKey("^s");
    break;
  case 4:
    system("psview -H &");
    break;
  case 5:
    if (Child()) {
      SendKey("q");
      Exit = True;
    } else
      Quit();
    break;
  }
}

static void SetButtons(Bool state)
{
  XmString s;
  
  XtVaSetValues(PageWidget, XmNeditable, state, NULL);
  XtVaSetValues(ScaleWidget, XmNeditable, state, NULL);
  XtVaSetValues(AngleWidget, XmNeditable, state, NULL);
  XtSetSensitive(PreviousWidget, state);
  XtSetSensitive(NextWidget, state);
  XtSetSensitive(FirstPageWidget, state);
  XtSetSensitive(LastPageWidget, state);
  XtSetSensitive(ZoomWidget, state);
  XtSetSensitive(PrintWidget, state);
  XtSetSensitive(RevertWidget, state);
  XtSetSensitive(UndoWidget, state);
  XtSetSensitive(SaveWidget, state);
  XtSetSensitive(SavePixmapWidget, state);
  XtSetSensitive(CloseWidget, state);
  XtSetSensitive(AutoUpdateWidget, state);
  XtSetSensitive(SmoothScrollingWidget, state);
  XtSetSensitive(WatchProgressWidget, state);
  XtSetSensitive(BitmapImagesWidget, state);
  XtSetSensitive(ModeWidget, state);

  if (!state) {
    
    XmToggleButtonSetState(WatchProgressWidget, False, False);
    XmToggleButtonSetState(BitmapImagesWidget, False, False);
    XmToggleButtonSetState(AutoUpdateWidget, False, False);
    XmToggleButtonSetState(SmoothScrollingWidget, False, False);

    s = XmStringCreateSimple(" ");
    XtVaSetValues(MessageWidget, XmNlabelString, s, NULL);
    XmStringFree(s);

  }

}


static void SetMode(Mode mode)
{

  if (!FirstMode && CurrentMode == mode)
    return;

  CurrentMode = mode;
  
  XtVaSetValues(ModeWidget, XmNlabelString, ModeString[mode], NULL);

  FirstMode = False;
  
}

static void SetScale(char *title)
{
  float s;
  char *scale;

  if ((scale = strstr(title, "<s")) != NULL) {
    s = atof(scale + 2);
    if (FirstScale || s != CurrentScale) {
      CurrentScale = s;
      (void) sprintf(buffer, "%.3f", CurrentScale);
      Strip(buffer);
      XmTextFieldSetString(ScaleWidget, buffer);

      FirstScale = False;
    }
  }
}

static void SetAngle(char *title)
{
  float a;
  char *angle;

  if ((angle = strstr(title, "<R")) != NULL) {
    a = atof(angle + 2);
    if (FirstAngle || a != CurrentAngle) {
      CurrentAngle = a;
      (void) sprintf(buffer, "%.2f", CurrentAngle);
      Strip(buffer);
      XmTextFieldSetString(AngleWidget, buffer);

      FirstAngle = False;
    }
  }
}

static void SetPage(char *title)
{
  int p;
  char *page;

  if ((page = strstr(title, "<P")) != NULL) {
    p = atoi(page + 2);
    if (FirstAngle || p != CurrentPage) {
      CurrentPage = p;
      (void) sprintf(buffer, "%d", CurrentPage);
      XmTextFieldSetString(PageWidget, buffer);

      FirstPage = False;
    }
  }
}

static void SetRevert(Bool a)
{
  if (FirstRevert || a != CurrentRevert) {
    CurrentRevert = a;
    XtSetSensitive(RevertWidget, CurrentRevert);
    XtSetSensitive(UndoWidget, CurrentRevert);
    FirstRevert = False;
  }
}

static void SetPrint(Bool a)
{
  if (FirstPrint || a != CurrentPrint) {
    CurrentPrint = a;
    XtSetSensitive(PrintWidget, CurrentPrint);
    FirstPrint = False;
  }
}

static void SetNext(Bool a)
{
  if (FirstNext || a != CurrentNext) {
    CurrentNext = a;
    XtSetSensitive(NextWidget, CurrentNext);
    FirstNext = False;
  }
}

static void SetPrevious(Bool a)
{
  if (FirstPrevious || a != CurrentPrevious) {
    CurrentPrevious = a;
    XtSetSensitive(PreviousWidget, CurrentPrevious);
    FirstPrevious = False;
  }
}

static void SetAutoUpdate(Bool a)
{
  if (FirstAutoUpdate || a != CurrentAutoUpdate) {
    CurrentAutoUpdate = a;
    XmToggleButtonSetState(AutoUpdateWidget, CurrentAutoUpdate, False);
    FirstAutoUpdate = False;
  }
}

static void SetWatchProgress(Bool a)
{
  if (FirstWatchProgress || a != CurrentWatchProgress) {
    CurrentWatchProgress = a;
    XmToggleButtonSetState(WatchProgressWidget, CurrentWatchProgress, False);
    FirstWatchProgress = False;
  }
}

static void SetBitmapImages(Bool a)
{
  if (FirstBitmapImages || a != CurrentBitmapImages) {
    CurrentBitmapImages = a;
    XmToggleButtonSetState(BitmapImagesWidget, CurrentBitmapImages, False);
    FirstBitmapImages = False;
  }
}

static void SetSmoothScrolling(int s)
{
  if (FirstSmoothScrolling || s != CurrentSmoothScrolling) {
    CurrentSmoothScrolling = s;
    if (s == -1) {
      XtSetSensitive(SmoothScrollingWidget, False);
      XmToggleButtonSetState(SmoothScrollingWidget, False, False);
    } else {
      XtSetSensitive(SmoothScrollingWidget, True);
      XmToggleButtonSetState(SmoothScrollingWidget, (Bool) s, False);
    }

    FirstSmoothScrolling = False;
  }
}

static void SetMessage(char *title)
{
  char *message;
  XmString s;
  int i;
  
  if ((message = strstr(title, "<m")) != NULL) {

    message += 2;

    for (i = 0 ;
	 i < BUFLEN && *message != null &&
	 (message[0] != '>' || message[1] != null); )
      buffer[i++] = *message++;

    buffer[i] = null;
    
  } else
    
    buffer[0] = null;
  
  if (strcmp(buffer, CurrentMessage) != 0) {

    (void) strcpy(CurrentMessage, buffer);

    s = XmStringCreateSimple(buffer[0] == null ? " " : buffer);
    XtVaSetValues(MessageWidget, XmNlabelString, s, NULL);
    XmStringFree(s);

    XFlush(dpy);
  }
}

static void SetWMName(char *name)
{
  char *list;
  XTextProperty prop;

  if (name == NULL)
    (void) sprintf(CurrentTitle, "xpsview %s", VERSION);
  else
    (void) strcpy(CurrentTitle, name);
  
  list = CurrentTitle;
  
  if (XStringListToTextProperty(&list, 1, &prop)) {
    XSetWMName(dpy, XtWindow(ApplicationWidget), &prop);
    XFree((char*) prop.value);
  }

  if (name == NULL) {
    (void) sprintf(buffer, "xpsview");
    list = buffer;
  } else if ((list = strrchr(list, '/')) != NULL)
    list++;
  else
    list = CurrentTitle;
  
  if (XStringListToTextProperty(&list, 1, &prop)) {
    XSetWMIconName(dpy, XtWindow(ApplicationWidget), &prop);
    XFree((char*) prop.value);
  }
}

static void SetTitle(char *title)
{
  char *message;
  int i;
  
  if ((message = strstr(title, "<f/")) != NULL) {
    
    message += 2;
    
    for (i = 0 ; i < BUFLEN && *message != null && *message != '>' ; )
      FullFileName[i++] = *message++;

    FullFileName[i] = null;

    if (strcmp(FullFileName, CurrentTitle) != 0)
      SetWMName(FullFileName);
    
  }
}

static void Alarm()
{
  XtAddTimeOut((unsigned long) 150, WaitProc, NULL);
}

static void WaitProc(Opaque closure, XtIntervalId *id)
{
  char *title;
  int status;

  /*
   * To avoid <exiting> processes
   */
  
  (void) waitpid(-1, &status, WNOHANG | WUNTRACED);
  
  /*
   * If there is a window, get its title and retrieve the current status
   */
  
  if (Child()) {
    
    if (XFetchName(dpy, window, &title) != NULL && title != NULL) {

      if (FirstTime)
	SetButtons(True);

      FirstTime = False;
      
      if (strstr(title, "<fp>"))
	SetMode(FullPage);
      else if (strstr(title, "<fw>"))
	SetMode(FullWidth);
      else if (strstr(title, "<fh>"))
	SetMode(FullHeight);
      else if (strstr(title, "<fs>"))
	SetMode(FixedScale);
      
      SetScale(title);
      SetPage(title);
      SetAngle(title);
      SetMessage(title);
      SetTitle(title);
      
      if (strstr(title, "<U+>"))
	SetAutoUpdate(True);
      else if (strstr(title, "<U->"))
	SetAutoUpdate(False);
      
      if (strstr(title, "<W+>"))
	SetWatchProgress(True);
      else if (strstr(title, "<W->"))
	SetWatchProgress(False);
      
      if (strstr(title, "<I+>"))
	SetBitmapImages(True);
      else if (strstr(title, "<I->"))
	SetBitmapImages(False);
      
      if (strstr(title, "<S+>"))
	SetSmoothScrolling(1);
      else if (strstr(title, "<S->"))
	SetSmoothScrolling(0);
      else if (strstr(title, "<S0>"))
	SetSmoothScrolling(-1);

      if (strstr(title, "<n+>"))
	SetNext(True);
      else if (strstr(title, "<n->"))
	SetNext(False);
      
      if (strstr(title, "<p+>"))
	SetPrevious(True);
      else if (strstr(title, "<p->"))
	SetPrevious(False);
      
      if (strstr(title, "<r+>"))
	SetRevert(True);
      else if (strstr(title, "<r->"))
	SetRevert(False);

      if (strstr(title, "<L+>"))
	SetPrint(True);
      else if (strstr(title, "<L->"))
	SetPrint(False);

      XFlush(dpy);
      XFree(title);

    }
    
  } else if (FileName != NULL) {

    OpenFile(FileName);
    FileName = NULL;
    
  }
  
  Alarm();
}

static int BadWindowHandler(Display *dpy, XErrorEvent *error)
{
  if (error->error_code != BadWindow &&
      error->error_code != BadPixmap &&
      error->error_code != BadDrawable) {
    XGetErrorText(dpy, error->error_code, buffer, BUFLEN);
    (void) fprintf(stderr, "X Error : %s\n", buffer);
    exit(EXIT_FAILURE);
  }
}

main(argc, argv)
     int argc;
     char *argv[];
{
  Widget PageLabelWidget, ScaleLabelWidget, ModeLabelWidget,
  AngleLabelWidget, LabelWidget, FrameWidget, MenuWidget;
  
  XtInitialize(argv[0], "XPsView", NULL, 0, &argc, argv);

  ApplicationWidget =
    XtCreateApplicationShell(argv[0],
			     applicationShellWidgetClass,
			     NULL, 0);

  FormWidget =
    XtVaCreateManagedWidget("form",
			    xmFormWidgetClass, ApplicationWidget,
			    NULL);
  
  FileWidget =
    XmVaCreateSimpleMenuBar(FormWidget, "mbar",
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNtopOffset, 5,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNleftOffset, 5,
			    XmNmarginHeight, 0,
			    XmNmarginWidth, 0,
			    XmNtraversalOn, False,
			    NULL);

  XtManageChild(FileWidget);

  XtVaCreateManagedWidget("cascade",
			  xmCascadeButtonWidgetClass,
			  FileWidget,
			  XmNlabelString,
			  XmStringCreateSimple("File"),
			  XmNmarginWidth, 6,
			  XmNmarginHeight, 0,
			  XmNmarginLeft, 0,
			  XmNmarginRight, 0,
			  XmNmarginTop, 0,
			  XmNhighlightThickness, 0,
			  NULL);
  
  MenuWidget =
    XmVaCreateSimplePulldownMenu(FileWidget, "file", 0, NULL,
				 XmNtraversalOn, False,
				 XmNsaveUnder, True,
				 NULL);
  
  OpenWidget = XmCreatePushButton(MenuWidget, "open", NULL, 0);
  XtVaSetValues(OpenWidget,
		XmNlabelString, XmStringCreateSimple("Open..."),
		NULL);
  XtAddCallback(OpenWidget,
		XmNactivateCallback, FileCallback, (XtPointer) 0);
  XtManageChild(OpenWidget);

  SaveWidget = XmCreatePushButton(MenuWidget, "save", NULL, 0);
  XtVaSetValues(SaveWidget,
		XmNlabelString, XmStringCreateSimple("Save BBox"),
		NULL);
  XtAddCallback(SaveWidget,
		XmNactivateCallback, FileCallback, (XtPointer) 2);
  XtManageChild(SaveWidget);

  SavePixmapWidget = XmCreatePushButton(MenuWidget, "save_pixmap", NULL, 0);
  XtVaSetValues(SavePixmapWidget,
		XmNlabelString, XmStringCreateSimple("Save Pixmap"),
		NULL);
  XtAddCallback(SavePixmapWidget,
		XmNactivateCallback, FileCallback, (XtPointer) 3);
  XtManageChild(SavePixmapWidget);

  PrintWidget = XmCreatePushButton(MenuWidget, "save_pixmap", NULL, 0);
  XtVaSetValues(PrintWidget,
		XmNlabelString, XmStringCreateSimple("Print..."),
		NULL);
  XtAddCallback(PrintWidget,
		XmNactivateCallback, PrintCallback, (XtPointer) 0);
  XtManageChild(PrintWidget);

  CloseWidget = XmCreatePushButton(MenuWidget, "close", NULL, 0);
  XtVaSetValues(CloseWidget,
		XmNlabelString, XmStringCreateSimple("Close"),
		NULL);
  XtAddCallback(CloseWidget,
		XmNactivateCallback, FileCallback, (XtPointer) 1);
  XtManageChild(CloseWidget);

  SeparatorWidget = XmCreateSeparator(MenuWidget, "separator", NULL, 0);
  XtManageChild(SeparatorWidget);
  
  AutoUpdateWidget = XmCreateToggleButton(MenuWidget, "update", NULL, 0);
  AutoUpdateString = XmStringCreateSimple("Auto Update");
  XtVaSetValues(AutoUpdateWidget,
		XmNlabelString, AutoUpdateString,
		NULL);
  XtAddCallback(AutoUpdateWidget,
		XmNvalueChangedCallback, ToggleCallback, NULL);
  XtManageChild(AutoUpdateWidget);

  BitmapImagesWidget = XmCreateToggleButton(MenuWidget, "bitmap", NULL, 0);
  BitmapImagesString = XmStringCreateSimple("Bitmap Images");
  XtVaSetValues(BitmapImagesWidget,
		XmNlabelString, BitmapImagesString,
		NULL);
  XtAddCallback(BitmapImagesWidget,
		XmNvalueChangedCallback, ToggleCallback, NULL);
  XtManageChild(BitmapImagesWidget);

  WatchProgressWidget = XmCreateToggleButton(MenuWidget, "progress", NULL, 0);
  WatchProgressString = XmStringCreateSimple("Watch Progress");
  XtVaSetValues(WatchProgressWidget,
		XmNlabelString, WatchProgressString,
		NULL);
  XtAddCallback(WatchProgressWidget,
		XmNvalueChangedCallback, ToggleCallback, NULL);
  XtManageChild(WatchProgressWidget);

  SmoothScrollingWidget = XmCreateToggleButton(MenuWidget, "large", NULL, 0);
  SmoothScrollingString = XmStringCreateSimple("Smooth Scrolling");
  XtVaSetValues(SmoothScrollingWidget,
		XmNlabelString, SmoothScrollingString,
		NULL);
  XtAddCallback(SmoothScrollingWidget,
		XmNvalueChangedCallback, ToggleCallback, NULL);
  XtManageChild(SmoothScrollingWidget);

  SeparatorWidget = XmCreateSeparator(MenuWidget, "separator", NULL, 0);
  XtManageChild(SeparatorWidget);

  HelpWidget = XmCreatePushButton(MenuWidget, "help", NULL, 0);
  XtVaSetValues(HelpWidget,
		XmNlabelString, XmStringCreateSimple("Help..."),
		NULL);
  XtAddCallback(HelpWidget, XmNactivateCallback,
		FileCallback, (XtPointer) 4);
  XtManageChild(HelpWidget);
  
  SeparatorWidget = XmCreateSeparator(MenuWidget, "separator", NULL, 0);
  XtManageChild(SeparatorWidget);

  QuitWidget = XmCreatePushButton(MenuWidget, "quit", NULL, 0);
  XtVaSetValues(QuitWidget,
		XmNlabelString, XmStringCreateSimple("Quit"),
		NULL);
  XtAddCallback(QuitWidget, XmNactivateCallback, FileCallback, (XtPointer) 5);
  XtManageChild(QuitWidget);
  
  PreviousWidget =
    XtVaCreateManagedWidget("previous",
			    xmPushButtonWidgetClass, FormWidget,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, FileWidget,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNlabelString, XmStringCreateSimple(" << "),
			    XmNleftOffset, 8,
			    XmNtopOffset, 5,
			    XmNtraversalOn, False,
			    XmNhighlightThickness, 0,
			    NULL);
  
  XtAddCallback(PreviousWidget,
		XmNactivateCallback, KeyCallback, (XtPointer) "p");
  
  NextWidget =
    XtVaCreateManagedWidget("next",
			    xmPushButtonWidgetClass, FormWidget,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, PreviousWidget,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNlabelString, XmStringCreateSimple(" >> "),
			    XmNleftOffset, 8,
			    XmNtopOffset, 5,
			    XmNtraversalOn, False,
			    XmNhighlightThickness, 0,
			    NULL);
  
  XtAddCallback(NextWidget,
		XmNactivateCallback, KeyCallback, (XtPointer) "n");

  FirstPageWidget =
    XtVaCreateManagedWidget("first",
			    xmPushButtonWidgetClass, FormWidget,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, NextWidget,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNlabelString,
			    XmStringCreateSimple(" First "),
			    XmNleftOffset, 8,
			    XmNtopOffset, 5,
			    XmNtraversalOn, False,
			    XmNhighlightThickness, 0,
			    NULL);
  
  XtAddCallback(FirstPageWidget,
		XmNactivateCallback, KeyCallback, (XtPointer) "1");

  XtAddCallback(FirstPageWidget,
		XmNactivateCallback, KeyCallback, (XtPointer) "g");

  LastPageWidget =
    XtVaCreateManagedWidget("last",
			    xmPushButtonWidgetClass, FormWidget,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, FirstPageWidget,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNlabelString,
			    XmStringCreateSimple(" Last "),
			    XmNleftOffset, 8,
			    XmNtopOffset, 5,
			    XmNtraversalOn, False,
			    XmNhighlightThickness, 0,
			    NULL);
  
  XtAddCallback(LastPageWidget,
		XmNactivateCallback, KeyCallback, (XtPointer) "g");

  ZoomWidget =
    XtVaCreateManagedWidget("zoom",
			    xmPushButtonWidgetClass, FormWidget,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, LastPageWidget,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNlabelString, XmStringCreateSimple(" Zoom "),
			    XmNleftOffset, 8,
			    XmNtopOffset, 5,
			    XmNtraversalOn, False,
			    XmNhighlightThickness, 0,
			    NULL);

  XtAddCallback(ZoomWidget,
		XmNactivateCallback, KeyCallback, (XtPointer) "z");
  
  UndoWidget =
    XtVaCreateManagedWidget("undo",
			    xmPushButtonWidgetClass, FormWidget,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, ZoomWidget,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNlabelString,
			    XmStringCreateSimple(" Revert "),
			    XmNleftOffset, 8,
			    XmNtopOffset, 5,
			    XmNtraversalOn, False,
			    XmNhighlightThickness, 0,
			    NULL);
  
  XtAddCallback(UndoWidget,
		XmNactivateCallback, KeyCallback, (XtPointer) "^r");
  
  RevertWidget =
    XtVaCreateManagedWidget("revert",
			    xmPushButtonWidgetClass, FormWidget,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNleftWidget, UndoWidget,
			    XmNlabelString,
			    XmStringCreateSimple(" Undo "),
			    XmNleftOffset, 8,
			    XmNtopOffset, 5,
			    XmNtraversalOn, False,
			    XmNhighlightThickness, 0,
			    NULL);
  
  XtAddCallback(RevertWidget,
		XmNactivateCallback, KeyCallback, (XtPointer) "r");
  
  MessageWidget =
    XtVaCreateManagedWidget("message",
                            xmLabelWidgetClass, FormWidget,
                            XmNleftAttachment, XmATTACH_WIDGET,
                            XmNleftWidget, RevertWidget,
                            XmNleftOffset, 5,
                            XmNrightAttachment, XmATTACH_FORM,
                            XmNrightOffset, 5,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNtopOffset, 8,
			    XmNrecomputeSize, True,
                            XmNlabelString, XmStringCreateSimple(" "),
			    XmNalignment, XmALIGNMENT_BEGINNING,
                            NULL);
			       
  PageLabelWidget =
    XtVaCreateManagedWidget("page",
                            xmLabelWidgetClass, FormWidget,
                            XmNleftAttachment, XmATTACH_FORM,
                            XmNleftOffset, 5,
                            XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, FileWidget,
                            XmNtopOffset, 5,
                            XmNlabelString, XmStringCreateSimple("Page:"),
                            NULL);
			       
  PageWidget =
    XtVaCreateManagedWidget("page",
			    xmTextFieldWidgetClass, FormWidget,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, PageLabelWidget,
			    XmNleftOffset, 5,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopOffset, 5,
			    XmNtopWidget, FileWidget,
			    XmNeditable, True,
			    XmNmarginHeight, 2,
			    XmNmarginWidth, 4,
			    XmNhighlightThickness, 0,
			    XmNcolumns, 3,
			    XmNcursorPositionVisible, False,
			    NULL);
  
  XtAddCallback(PageWidget, XmNactivateCallback, ArgsCallback, NULL);

  ScaleLabelWidget =
    XtVaCreateManagedWidget("scale",
                            xmLabelWidgetClass, FormWidget,
                            XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, PageWidget,
                            XmNleftOffset, 5,
                            XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, FileWidget,
                            XmNtopOffset, 5,
                            XmNlabelString, XmStringCreateSimple("Scale:"),
                            NULL);
			       
  ScaleWidget =
    XtVaCreateManagedWidget("scale",
			    xmTextFieldWidgetClass, FormWidget,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, ScaleLabelWidget,
			    XmNleftOffset, 5,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, FileWidget,
			    XmNtopOffset, 5,
			    XmNeditable, True,
			    XmNmarginHeight, 2,
			    XmNmarginWidth, 4,
			    XmNhighlightThickness, 0,
			    XmNcolumns, 5,
			    XmNcursorPositionVisible, False,
			    NULL);

  XtAddCallback(ScaleWidget, XmNactivateCallback, ArgsCallback, NULL);

  AngleLabelWidget =
    XtVaCreateManagedWidget("angle",
                            xmLabelWidgetClass, FormWidget,
                            XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, ScaleWidget,
                            XmNleftOffset, 5,
                            XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, FileWidget,
                            XmNtopOffset, 5,
                            XmNlabelString,
			    XmStringCreateSimple("Angle:"),
                            NULL);
			       
  AngleWidget =
    XtVaCreateManagedWidget("angle",
			    xmTextFieldWidgetClass, FormWidget,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, AngleLabelWidget,
			    XmNleftOffset, 5,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, FileWidget,
			    XmNtopOffset, 5,
			    XmNeditable, True,
			    XmNmarginHeight, 2,
			    XmNmarginWidth, 4,
			    XmNhighlightThickness, 0,
			    XmNcursorPositionVisible, False,
			    XmNcolumns, 4,
			    NULL);
  
  XtAddCallback(AngleWidget, XmNactivateCallback, ArgsCallback, NULL);

  ModeLabelWidget =
    XtVaCreateManagedWidget("mode_label",
                            xmLabelWidgetClass, FormWidget,
                            XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, AngleWidget,
                            XmNleftOffset, 5,
                            XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, FileWidget,
                            XmNtopOffset, 5,
                            XmNlabelString,
			    XmStringCreateSimple("Mode:"),
                            NULL);
			       
  ModeString[0] = XmStringCreateSimple("Full Page   ");
  ModeString[1] = XmStringCreateSimple("Full Width  ");
  ModeString[2] = XmStringCreateSimple("Full Height ");
  ModeString[3] = XmStringCreateSimple("Fixed Scale");
  ModeString[4] = XmStringCreateSimple("                 ");

  MenuWidget =
    XmVaCreateSimpleMenuBar(FormWidget, "mode",
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, ModeLabelWidget,
			    XmNleftOffset, 5,
                            XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, FileWidget,
                            XmNtopOffset, 5,
			    XmNmarginHeight, 0,
			    XmNmarginWidth, 0,
			    XmNtraversalOn, False,
			    NULL);

  XtManageChild(MenuWidget);

  ModeWidget =
    XtVaCreateManagedWidget("mode",
			    xmCascadeButtonWidgetClass, MenuWidget,
			    XmNlabelString, ModeString[Undefined],
			    XmNmarginWidth, 6,
			    XmNmarginHeight, 0,
			    XmNmarginLeft, 0,
			    XmNmarginRight, 0,
			    XmNmarginTop, 0,
			    XmNhighlightThickness, 0,
			    XmNrecomputeSize, True,
			    NULL);

  XmVaCreateSimplePulldownMenu(MenuWidget, "mode", 0, ModeCallback,
			       XmVaPUSHBUTTON,
			       ModeString[0],
			       NULL, NULL, NULL,
			       XmVaPUSHBUTTON,
			       ModeString[1],
			       NULL, NULL, NULL,
			       XmVaPUSHBUTTON,
			       ModeString[2],
			       NULL, NULL, NULL,
			       XmVaPUSHBUTTON,
			       ModeString[3],
			       NULL, NULL, NULL,
			       XmNtraversalOn, False,
			       NULL);

  FrameWidget =
    XtVaCreateManagedWidget("frame",
                            xmFrameWidgetClass, FormWidget,
                            XmNshadowType, XmSHADOW_IN,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNleftOffset, 5,
			    XmNrightAttachment, XmATTACH_FORM,
			    XmNrightOffset, 5,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, PageWidget,
			    XmNtopOffset, 5,
			    XmNbottomAttachment, XmATTACH_FORM,
			    XmNbottomOffset, 5,
			    XmNtraversalOn, False,
			    XmNhighlightThickness, 0,
                            NULL);

  DPSWidget =
    XtVaCreateManagedWidget("psview",
			    xmDrawingAreaWidgetClass, FrameWidget,
			    XmNwidth, 660,
			    XmNheight, 850,
			    NULL);

  XtAddCallback(DPSWidget, XmNresizeCallback, ResizeCallback, NULL);
  XtAddCallback(DPSWidget, XmNexposeCallback, ExposeCallback, NULL);
  
  /*
   * Set environment variables
   */
  
  dpy = XtDisplay(ApplicationWidget);

  XDisplayKeycodes(dpy, &MinKeyCode, &MaxKeyCode);
  
  Argc = argc;
  Argv = argv;

  /*
   * Wake up every tenth of second
   */
  
  Alarm();

  /*
   * Ignore "bad window" errors
   */

  XSetErrorHandler(BadWindowHandler);

  /*
   * Realize & loop
   */

  SetButtons(False);
  
  XtRealizeWidget(ApplicationWidget);

  SetWMName(NULL);
  
  XtMainLoop();
}


