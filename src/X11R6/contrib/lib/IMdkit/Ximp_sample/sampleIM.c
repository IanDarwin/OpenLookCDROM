/******************************************************************
 
              Copyright 1994 by Sun Microsystems, Inc.
              Copyright 1994 by Hewlett-Packard Company
 
Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Sun Microsystems, Inc.
and Hewlett-Packard not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior permission.
Sun Microsystems, Inc. and Hewlett-Packard make no representations about
the suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
 
SUN MICROSYSTEMS INC. AND HEWLETT-PACKARD COMPANY DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
SUN MICROSYSTEMS, INC. AND HEWLETT-PACKARD COMPANY BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 
  Author: Hiromu Inukai (inukai@Japan.Sun.COM) Sun Microsystems, Inc.
          Hidetoshi Tajima(tajima@kobe.hp.com) Hewlett-Packard Company.
 
******************************************************************/
#include <stdio.h>
#include <locale.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Ximd/IMdkit.h>
#include <X11/Ximd/XimpData.h>

#define DEFAULT_IMNAME "sampleIM"
#define DEFAULT_LOCALE "ja_JP"

/* flags for debugging */
long filter_mask = KeyPressMask;
static Bool conv_state = False;

/* Supported Inputstyles */
static XIMStyle Styles[] = {
    XIMPreeditCallbacks|XIMStatusCallbacks,
    XIMPreeditPosition|XIMStatusArea,
    XIMPreeditPosition|XIMStatusNothing,
    XIMPreeditArea|XIMStatusArea,
    XIMPreeditNothing|XIMStatusNothing,
    0
};

/* Trigger Keys List */
static XIMTriggerKey Trigger_Keys[] = {
    {XK_space, ShiftMask, ShiftMask},
    {0L, 0L, 0L}
};

/* Conversion Keys List */
static XIMTriggerKey Conversion_Keys[] = {
    {XK_k, ControlMask, ControlMask},
    {0L, 0L, 0L}
};

/* Forward Keys List */
static XIMTriggerKey Forward_Keys[] = {
    {XK_Return, 0, 0},
    {XK_Tab, 0, 0},
    {0L, 0L, 0L}
};

MyGetICValuesHandler(ims, call_data)
XIMS ims;
XIMPICValuesStruct *call_data;
{
    GetIC(call_data);
    return True;
}

MySetICValuesHandler(ims, call_data)
XIMS ims;
XIMPICValuesStruct *call_data;
{
    SetIC(call_data);
    return True;
}

MyResetEventHandler(ims, call_data)
XIMS ims;
XIMPResetStruct *call_data;
{
    XTextProperty tp;
    Display *display = ims->core.display;
    char *text = "IC をリセットしました。";

    setlocale(LC_CTYPE, "");
    XmbTextListToTextProperty(display, (char **)&text, 1,
			      XCompoundTextStyle, &tp);
    call_data->ctext = (char *)tp.value;
}

MyCreateICHandler(ims, call_data)
XIMS ims;
XIMPICValuesStruct *call_data;
{
    static unsigned long ximp_type = 0;
    ximp_type = call_data->ximp_type_mask;
    CreateIC(call_data);
    return True;
}

MyBeginHandler(ims, call_data)
XIMS ims;
IMPProtocol *call_data;
{
    conv_state = True;
}

MyEndHandler(ims, call_data)
XIMS ims;
IMPProtocol *call_data;
{
    conv_state = False;
}

MyExtensionHandler(ims, call_data)
XIMS ims;
IMPProtocol *call_data;
{
    Display	*display = ((XIMPCore)ims->protocol)->display;
    XIMPExtensionStruct *extension =
		(XIMPExtensionStruct*)&(call_data->extension);
    switch (extension->ext_type) {
      case EXT_STATUS:
	break;
      case EXT_BACKFRONT:
	break;
      case EXT_CONV:
	if (extension->todo.conversion.operation) { /* Conversion set */
	    conv_state = (Bool)extension->todo.conversion.mode;
	} else { /* Conversion get */
	    extension->todo.conversion.mode = conv_state;

	}
    }
}

MyProtoHandler(ims, call_data)
XIMS ims;
IMPProtocol *call_data;
{
    switch (call_data->type) {
      case XIMP_CREATE4:
	MyCreateICHandler(ims, call_data);
	break;
      case XIMP_REG_KEY_PRESSED4:
      case XIMP_BEGIN3:
	MyBeginHandler(ims, call_data);
	break;
      case XIMP_DESTROY4:
	break;
      case XIMP_SETVALUE4:
	MySetICValuesHandler(ims, call_data);
	break;
      case XIMP_GETVALUE4:
	MyGetICValuesHandler(ims, call_data);
	break;
      case XIMP_KEYPRESS4:
      case XIMP_KEYPRESS3:
	MyForwardEventHandler(ims, call_data);
	break;
      case XIMP_RESET4:
	MyResetEventHandler(ims, call_data);
	break;
      case XIMP_SETFOCUS4:
      case XIMP_UNSETFOCUS4:
	break;
      case XIMP_CREATE3:
	MyCreateICHandler(ims, call_data);
	break;
      case XIMP_DESTROY3:
	break;
      case XIMP_SETVALUE3:
	MySetICValuesHandler(ims, call_data);
	break;
      case XIMP_GETVALUE3:
	MyGetICValuesHandler(ims, call_data);
	break;
      case XIMP_RESET3:
	MyResetEventHandler(ims, call_data);
	break;
      case XIMP_SETFOCUS3:
      case XIMP_UNSETFOCUS3:
	break;
      case XIMP_EXTENSION4:
      case XIMP_EXTENSION3:
	MyExtensionHandler(ims, call_data);
	break;
      case XIMP_END3:
	MyEndHandler(ims, call_data);
    }
}

static Bool
IsMatchKeys(display, keycode, state, trigger)
Display *display;
unsigned int keycode;
unsigned int state;
XIMTriggerKey *trigger;
{
    int i;
    int key_count;
    int modifier;
    int modifier_mask;
    KeySym keysym = XKeycodeToKeysym(display, keycode, 0);

    for (i = 0; trigger[i].keysym != 0; i++);
    key_count = i;

    for (i = 0; i < key_count; i++, trigger++) {
	modifier      = trigger->modifier;
	modifier_mask = trigger->modifier_mask;
	if (((KeySym)trigger->keysym == keysym)
	    && ((state & modifier_mask) == modifier))
	  return True;
    }
    return False;
}

ProcessKey(display, keycode, state)
Display *display;
unsigned int keycode;
unsigned int state;
{
    XKeyEvent *kev;
    int count;

    printf("Keycode %d with state %d is consumed in IMserver\n",
	   keycode, state);
}

MyForwardEventHandler(ims, call_data)
XIMS ims;
XIMPKeyEventStruct *call_data;
{
    XIMPCore core = (XIMPCore)ims->protocol;
    Display *display = core->display;

    if (IsMatchKeys(display, call_data->keycode, call_data->state,
		    Trigger_Keys)) {
	return IMPreeditEnd(ims, (XPointer)call_data);
    }
    if (IsMatchKeys(display, call_data->keycode, call_data->state,
		    Conversion_Keys)) {
	XTextProperty tp;
	char *text = "これは IM からの確定文字列です。";
	char lang[20];
	setlocale(LC_CTYPE, "");
	XmbTextListToTextProperty(display, (char **)&text, 1,
				  XCompoundTextStyle, &tp);
	((XIMPCommitStringStruct*)call_data)->ctext = (char *)tp.value;
	IMCommitString(ims, (XPointer)call_data);
    }
    else if (IsMatchKeys(display, call_data->keycode, call_data->state,
			 Forward_Keys)) {
	IMForwardEvent(ims, (XPointer)call_data);
    } else {
	ProcessKey(display, call_data->keycode, call_data->state);
    }
    return True;
}

void MyXEventHandler(im_window, event)
Window im_window;
XEvent *event;
{
    switch (event->type) {
      case DestroyNotify:
	break;
    case ButtonPress:
      switch (event->xbutton.button) {
	case Button3:
	  if (event->xbutton.window == im_window)
	    goto Exit;
	  break;
      }
      default:
	break;
    }
    return;
  Exit:
    XDestroyWindow(event->xbutton.display, im_window);
    exit(0);
}

unsigned long	XimpType[] = {
    XIMP_BE_TYPE1,
    XIMP_FE_TYPE1,
    XIMP_BE_TYPE2,
    XIMP_FE_TYPE2,
    XIMP_FE_TYPE3,
    XIMP_SYNC_BE_TYPE1,
    XIMP_SYNC_BE_TYPE2,
    0
};

char *ximp_version = NULL;

main(argc, argv)
int argc;
char **argv;
{
    char *display_name = NULL;
    Display *dpy;
    char *imname = NULL;
    XIMS ims;
    XIMStyles *input_styles, *styles2;
    XIMTriggerKeys *on_keys, *trigger2;
    Window im_window;
    register int i;
    XVaNestedList *nest_list;
    XVaNestedList *ext_list;
    long back_front = 0;
    XIMPTypeRec *ximp_type;

    for (i = 1; i < argc; i++) {
	if (!strcmp(argv[i], "-name")) {
	    imname = argv[++i];
	} else if (!strcmp(argv[i], "-display")) {
	    display_name = argv[++i];
	} else if (!strcmp(argv[i], "-kl")) {
	    filter_mask = (KeyPressMask|KeyReleaseMask);
	} else if (!strcmp(argv[i], "-old")) {
	    ximp_version = "3.5";
	}
    }
    if (!imname) imname = DEFAULT_IMNAME;

    if (!ximp_version) ximp_version = "4.0";

    if ((dpy = XOpenDisplay(display_name)) == NULL) {
	fprintf(stderr, "Can't Open Display: %s\n", display_name);
	exit(1);
    }
    im_window = XCreateSimpleWindow(dpy, DefaultRootWindow(dpy),
				    0, 0, 1, 1, 1, 0, 0);
    if (im_window == (Window)NULL) {
	fprintf(stderr, "Can't Create Window\n");
	exit(1);
    }
    XStoreName(dpy, im_window, "Sample Input Method Server");

    if ((input_styles = (XIMStyles *)malloc(sizeof(XIMStyles))) == NULL) {
	fprintf(stderr, "Can't allocate\n");
	exit(1);
    }
    input_styles->count_styles = sizeof(Styles)/sizeof(XIMStyle) - 1;
    input_styles->supported_styles = Styles;

    if ((ximp_type = (XIMPTypeRec *)malloc(sizeof(XIMPTypeRec))) == NULL) {
	fprintf(stderr, "Can't allocate\n");
	exit(1);
    }
    ximp_type->num_of_types = sizeof(XimpType)/ sizeof(XIMPTypeRec) - 1;
    ximp_type->types = XimpType;

    if ((on_keys = (XIMTriggerKeys *)
	 malloc(sizeof(XIMTriggerKeys))) == NULL) {
	fprintf(stderr, "Can't allocate\n");
	exit(1);
    }
    on_keys->count_keys = sizeof(Trigger_Keys)/sizeof(XIMTriggerKey) - 1;
    on_keys->keylist = Trigger_Keys;

    ext_list = (XVaNestedList *)XVaCreateNestedList( 0,
				    XIMPExtStatusWin, True,
				    XIMPExtBackFront, 1,
				    XIMPExtConversion, True,
				    NULL);

    nest_list = (XVaNestedList *)XVaCreateNestedList( 0,
				     XIMPVersion, ximp_version,
 				     XIMPServerVersion, "1.0",
				     XIMPType, ximp_type,
				     XIMPExtension, ext_list,
 				     XIMPVendorName, "sample",
				     NULL);
    ims = IMOpenIM(dpy,
		   IMModifiers, "XIMP",
		   IMServerWindow, im_window,
		   IMServerName, imname,
		   IMLocale, DEFAULT_LOCALE,
		   IMInputStyles, input_styles,
		   IMProtocolDepend, nest_list,
		   IMOnKeysList, on_keys,
		   IMOffKeysList, on_keys,
		   NULL);
    if (ims == (XIMS)NULL) {
	fprintf(stderr, "Can't Open Input Method Service:\n");
	fprintf(stderr, "\tInput Method Name :%s\n", imname);
	exit(1);
    }
    IMSetIMValues(ims,
		  IMProtocolHandler, MyProtoHandler,
		  IMFilterEventMask, filter_mask,
		  NULL);

    ext_list = (XVaNestedList *)XVaCreateNestedList( 0,
				    XIMPExtBackFront, &back_front,
				    NULL);

    nest_list = (XVaNestedList *)XVaCreateNestedList( 0,
				     XIMPExtension, ext_list,
				     NULL);
    IMGetIMValues(ims,
		  IMInputStyles, &styles2,
		  IMOnKeysList, &trigger2,
		  IMOffKeysList, &trigger2,
		  IMProtocolDepend, nest_list,
		  NULL);

    XSelectInput(dpy, im_window, StructureNotifyMask|ButtonPressMask);
    XMapWindow(dpy, im_window);

    for (;;) {
	XEvent event;
	XNextEvent(dpy, &event);
	if (XFilterEvent(&event, None) == True)
	  continue;
	MyXEventHandler(im_window, &event);
    }
}
