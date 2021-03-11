/*
 * $header$
 *
 * Copyright 1989 Massachusetts Institute of Technology
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
 */

#include <stdio.h>
#include <AF/AFlib.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Clock.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Logo.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/StripChart.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Cardinals.h>

/*
 * Display related stuff.
 */
String fallback_resources[] = { 
    "*input:			True",
    "*Form*resizable:		True",
    "*Box*allowResize:		True",
    "*scrollbar*orientation:	horizontal",
    "*scrollbar*length:		100",
    "*background:		LightYellow",
    "*foreground: 		SteelBlue",
    "*showGrip:			off",
    "*Text*preferredPaneSize:	200", 
    "*Text*editType:		edit",
    "*Text*scrollVertical:	whenNeeded",
    "*Text*scrollHorizontal:	whenNeeded",
    "*Text*autoFill:		off",
    NULL,
};


/*
 * Audio Device information.
 */
typedef struct {
	int	id;
	AC	ac;
	AFDeviceDescriptor	*ad;
	AMask	imask;
	AMask	imask_old;
	AMask	omask;
	AMask	omask_old;
	int	igain;
	int	igain_max;
	int	igain_min;
	int	ogain;
	int	ogain_max;
	int	ogain_min;
} devStuff;

#define	MAXRB 	16
#define	MAXSB 	16

/*
 * Widget closure.
 */
typedef struct {
	devStuff *dPtr;
	Widget	scrollbar;
	Widget  label;
	float	pos;
	float	pos_old;
} myScrollBar;

#define	RBDisabled	False
#define	RBEnabled	True

typedef struct {
	devStuff *dPtr;
	Widget	button;
	int	num;				/* Bit position in mask */
	int	group;
} myRadioButton;

int 		nSB=0;
int 		nRB=0;
myRadioButton	rb[MAXRB];
myScrollBar	sb[MAXSB];

char	*devLabel[4]={
	"Phone",
	"Local",
	"Left",
	"Right"
};

devStuff	devInfo[4];
AFAudioConn 	*aud;
int 		numDevices=0;
int		gid=0;
AC		phoneAC;
int		hs;
int		loop;
Widget 		hookw;

extern Widget 	text;

/*
 * forwards.
 */
static void Destroyed(), Quit(), Finish();
static void ChangeInputSelect();
static void ChangeOutputSelect();
static void Syntax();
static void CreateInputForm();
static void CreateOutputForm();
static void CreateHookSwitch();
static void Hs();
static void Scrolled();
static void ThumbedInput(); 
static void ThumbedOutput(); 
static void CreateDeviceForm();
static void CreateDevicesForm();
static void CreatePassThrough();
static void PassThrough();

extern void	AFeventHandler();
extern void	AFInitEvent();

int
main(argc,argv)
int argc;
char **argv;
{
    Widget toplevel, outer, quit;
    XtAppContext app_con;
    int afd;

    /* Audio Connection */
    aud = AFOpenAudioConn("");
    if (aud == NULL) {
	    fprintf(stderr,"can't open connection\n");
	    exit(1);
    }
    /* 
     * For the moment, on a LoFi, only display Phone and Local
     * control panels.
     */
    numDevices = ANumberOfAudioDevices(aud);
    numDevices = (numDevices > 2 ? 2 : numDevices);

    toplevel = XtAppInitialize(&app_con, "Xwidgets", NULL, ZERO,
			       &argc, argv, fallback_resources, NULL, ZERO);

    if (argc != 1) 
	Syntax(app_con, argv[0]);

    XtAddCallback( toplevel, XtNdestroyCallback, Destroyed, NULL );
    outer = XtCreateManagedWidget( "paned", panedWidgetClass, toplevel,
				  NULL, ZERO);
    quit = XtCreateManagedWidget( "quit", commandWidgetClass, outer, 
				 NULL, ZERO);
    XtAddCallback( quit, XtNcallback, Quit, (XtPointer) toplevel);

    CreateHookSwitch(outer);
    CreatePassThrough(outer);
    CreateDevicesForm(outer);

    AFInitEvent(aud, outer);

    afd = AConnectionNumber(aud);
    (void) XtAppAddInput(app_con, afd, (XtPointer) XtInputReadMask, AFeventHandler, 
		(XtPointer) &aud);
    XtRealizeWidget(toplevel);
    XtAppMainLoop(app_con);

    return (0);
}


static void
CreateHookSwitch(parent)
Widget parent;
{
    Arg args[8];
    char buf[256];
    int j,val;

    phoneAC = AFCreatePhoneAC(aud, 0, NULL);
    if (phoneAC != NULL) {
	AFQueryPhone(phoneAC, &hs, &loop);
	sprintf(buf,"%s",(hs==OnHook ? "OnHook" : "OffHook"));
        j = 0;
	val = ( hs == OffHook ? RBEnabled : RBDisabled);
        XtSetArg(args[j], XtNstate, val); ++j;
	hookw = XtCreateManagedWidget(buf, toggleWidgetClass, parent, 
				     args, j);
	XtAddCallback(hookw, XtNcallback, Hs, (XtPointer) hookw);
    }

}

/* ARGSUSED */
static void 
Hs(widget, closure, callData)
Widget widget;
XtPointer closure, callData;
{
    Arg args[8];
    char buf[256];

    hs = ~hs & 1;
    AFHookSwitch(phoneAC, hs);
    AFFlush(aud);

    sprintf(buf,"%s",(hs==OnHook ? "OnHook" : "OffHook"));
    XtSetArg( args[0], XtNlabel, buf );
    XtSetValues(hookw, args, ONE);
}

static ABool ptm;
static Widget ptw;
static void
CreatePassThrough(parent)
Widget parent;
{
    Arg args[8];
    char buf[256];
    int j,val;

    if (phoneAC != NULL) {
	AFEnablePassThrough(phoneAC, (ABool)0, &ptm, &ptm);
	sprintf(buf,"%s",(ptm==0 ? "Pass Through Off" : "Pass Through On"));
        j = 0;
	val = ( ptm == 1 ? RBEnabled : RBDisabled);
        XtSetArg(args[j], XtNstate, val); ++j;
	ptw = XtCreateManagedWidget(buf, toggleWidgetClass, parent, 
				     args, j);
	XtAddCallback(ptw, XtNcallback, PassThrough, (XtPointer) ptw);
    }

}

/* ARGSUSED */
static void 
PassThrough(widget, closure, callData)
Widget widget;
XtPointer closure, callData;
{
    Arg args[8];
    char buf[256];

    ptm = ~ptm & 1;

    if (ptm) 
    	AFEnablePassThrough(phoneAC, (ABool)1, &ptm, &ptm);
    else
    	AFDisablePassThrough(phoneAC, (ABool)1, &ptm, &ptm);

    AFFlush(aud);

    sprintf(buf,"%s",(ptm==0 ? "Pass Through Off" : "Pass Through On"));
    XtSetArg( args[0], XtNlabel, buf );
    XtSetValues(ptw, args, ONE);
}


static void
CreateDevicesForm(parent)
Widget parent;
{
    int noutputs, ninputs;
    int i;

    for(i=0;i<numDevices;++i){
	AMask old,new;
	devInfo[i].ad = AAudioDeviceDescriptor(aud, i);
        ninputs = devInfo[i].ad->numberOfInputs;
        noutputs = devInfo[i].ad->numberOfOutputs;
	devInfo[i].id = i;
	devInfo[i].ac = AFCreateAC(aud, i, 0, NULL);

        AFEnableInput(devInfo[i].ac, 0, &old, &new);
	devInfo[i].imask = new;
	devInfo[i].imask_old = old;

        AFEnableOutput(devInfo[i].ac, 0, &devInfo[i].omask, &devInfo[i].omask);
	devInfo[i].omask_old = devInfo[i].omask;
	devInfo[i].igain = AFQueryInputGain(devInfo[i].ac, 
		&devInfo[i].igain_min, 
		&devInfo[i].igain_max);
	devInfo[i].ogain = AFQueryOutputGain(devInfo[i].ac, 
		&devInfo[i].ogain_min, 
		&devInfo[i].ogain_max);

    	(void) XtCreateManagedWidget( devLabel[i], labelWidgetClass, parent,
				  NULL, ZERO);
	CreateDeviceForm(parent, i, ninputs, noutputs);
    }
}

static void
CreateDeviceForm(parent, index, nin, nout)
Widget parent;
int 	index;
int	nin;
int	nout;
{
    Widget box;
    devStuff	*devPtr= &devInfo[index];

    box = XtCreateManagedWidget( "box", boxWidgetClass, parent,
				  NULL, ZERO);

    CreateInputForm(box, devPtr, nin);
    CreateOutputForm(box, devPtr, nout);
}

static void
CreateInputForm(parent, devPtr, nin)
Widget parent;
devStuff	*devPtr;
int	nin;
{
    Widget form;
    Widget scrollbar, label;
    int i,j, val;
    char buf[256];
    Arg args[8];
    float top;

    sb[nSB].dPtr = devPtr;

    form = XtCreateManagedWidget( "form", formWidgetClass, parent, NULL, ZERO);

    if (((float)(sb[nSB].dPtr->igain_max) == (float)(sb[nSB].dPtr->igain_min)))
	top = 0;
    else
	top = ((float)(sb[nSB].dPtr->igain) - (float)(sb[nSB].dPtr->igain_min))/ 
         ((float)(sb[nSB].dPtr->igain_max) - (float)(sb[nSB].dPtr->igain_min));

    sprintf(buf,"%3d",sb[nSB].dPtr->igain);
    label = XtCreateManagedWidget(buf, labelWidgetClass, form, 
				  NULL, ZERO);

    sb[nSB].label = label;

    XtSetArg(args[0], XtNfromHoriz, label);
    scrollbar = XtCreateManagedWidget("scrollbar", scrollbarWidgetClass, form,
				      args, ONE);

    XtAddCallback(scrollbar, XtNjumpProc, ThumbedInput, (XtPointer) &sb[nSB]);

    sb[nSB].scrollbar = scrollbar;
    XawScrollbarSetThumb(scrollbar, top, -1.0);

    ++nSB;

    XtSetArg(args[0], XtNfromHoriz, scrollbar);
    for(i=0;i<nin;i++){
	Widget trb;

	rb[nRB].dPtr = devPtr;
	rb[nRB].num = i;
	rb[nRB].group = gid;

	if(i!=0) XtSetArg(args[0], XtNfromHoriz, rb[nRB-1].button);
	sprintf(buf,"%2d",i);
	j = 1;
	val = ( ((rb[nRB].dPtr->imask>>i) & 1) == 1 ? RBEnabled : RBDisabled);
        XtSetArg(args[j], XtNstate, val); ++j;
	rb[nRB].button = XtCreateManagedWidget(buf, toggleWidgetClass, 
		form, args, j);
	XtAddCallback( rb[nRB].button, XtNcallback, ChangeInputSelect, 
		(XtPointer) &rb[nRB] );

	trb = (i == 0 ? (Widget) NULL : rb[nRB-i].button);
	XawToggleChangeRadioGroup(rb[nRB].button, trb);
	++nRB;
    }
    ++gid;

    XtSetArg(args[0], XtNfromHoriz, rb[nRB-1].button);
    label = XtCreateManagedWidget( "Input", labelWidgetClass, form,
				  args, ONE);

}

/* ARGSUSED */
static void 
ChangeInputSelect(widget, closure, callData)
Widget widget;
XtPointer closure, callData;
{
   AMask newstate; AMask oldstate;
   AMask enmask, dismask;

   myRadioButton *rbp=(myRadioButton *)closure;

   rbp->dPtr->imask_old = rbp->dPtr->imask;
   rbp->dPtr->imask ^= (1<<rbp->num);
   enmask = rbp->dPtr->imask_old ^ rbp->dPtr->imask;  /* What changed */
   enmask &= rbp->dPtr->imask; /* newly enabled only */
   if(enmask!=0){
	   AFEnableInput(rbp->dPtr->ac, enmask, &oldstate, &newstate);
   }
   dismask = rbp->dPtr->imask_old ^ rbp->dPtr->imask;  /* What changed */
   dismask &= ~rbp->dPtr->imask; /* newly enabled only */
   if(dismask != 0){
	   AFDisableInput(rbp->dPtr->ac, dismask, &oldstate, &newstate);
   }
}


/* ARGSUSED */
static void 
ThumbedInput(widget, sb_ptr, top_ptr)
Widget widget;
XtPointer sb_ptr, top_ptr;	
{
    float top = *((float *) top_ptr);
    Widget label = (Widget) ((myScrollBar *)sb_ptr)->label;
    myScrollBar *sbp = (myScrollBar *)sb_ptr;
    Arg args[1];
    char message[BUFSIZ];
    int gain;

    sbp->pos_old = sbp->pos; 
    sbp->pos = top;

   /* Convert a position between 0 and 1.0 to a gain in min to max range */
   gain = (int)(sbp->pos * (sbp->dPtr->igain_max - sbp->dPtr->igain_min) + 
		sbp->dPtr->igain_min);
   AFSetInputGain(sbp->dPtr->ac, gain);
   AFFlush(aud);

    sprintf( message, "%3d", gain);
    XtSetArg( args[0], XtNlabel, message );
    XtSetValues( label, args, ONE );
}


static void
CreateOutputForm(parent, devPtr, nout)
Widget parent;
devStuff	*devPtr;
int	nout;
{
    Widget form;
    Widget scrollbar, label;
    int i,j, val;
    char buf[256];
    Arg args[8];
    float top;

    sb[nSB].dPtr = devPtr;

    form = XtCreateManagedWidget( "form", formWidgetClass, parent, NULL, ZERO);

    if(((float)(sb[nSB].dPtr->ogain_max) == (float)(sb[nSB].dPtr->ogain_min)))
	top = 0;
    else
	top = ((float)(sb[nSB].dPtr->ogain) - (float)(sb[nSB].dPtr->ogain_min))/ 
         ((float)(sb[nSB].dPtr->ogain_max) - (float)(sb[nSB].dPtr->ogain_min));
    sprintf(buf,"%3d",sb[nSB].dPtr->ogain);
    label = XtCreateManagedWidget(buf, labelWidgetClass, form, 
				  NULL, ZERO);
    sb[nSB].label = label;

    XtSetArg(args[0], XtNfromHoriz, label);
    scrollbar = XtCreateManagedWidget("scrollbar", scrollbarWidgetClass, form,
				      args, ONE);

    XtAddCallback(scrollbar, XtNjumpProc, ThumbedOutput, (XtPointer) &sb[nSB]);
    sb[nSB].scrollbar = scrollbar;
    XawScrollbarSetThumb(scrollbar, top, -1.0);

    ++nSB;

    XtSetArg(args[0], XtNfromHoriz, scrollbar);
    for(i=0;i<nout;i++){
	rb[nRB].dPtr = devPtr;
	rb[nRB].num = i;
	rb[nRB].group = gid;

	if (i!=0) XtSetArg(args[0], XtNfromHoriz, rb[nRB-1].button);
	sprintf(buf,"%2d",i);
	j = 1;
	val = ( ((rb[nRB].dPtr->omask>>i) & 1) == 1 ? RBEnabled : RBDisabled);
        XtSetArg(args[j], XtNstate, val); ++j;

	rb[nRB].button = XtCreateManagedWidget(buf, toggleWidgetClass, 
		form, args, j);
	XtAddCallback( rb[nRB].button, XtNcallback, ChangeOutputSelect, 
		(XtPointer) &rb[nRB] );

	++nRB;
    }
    ++gid;
    XtSetArg(args[0], XtNfromHoriz, rb[nRB-1].button);
    label = XtCreateManagedWidget( "Output", labelWidgetClass, form,
				  args, ONE);
}

/* ARGSUSED */
static void 
ChangeOutputSelect(widget, closure, callData)
Widget widget;
XtPointer closure, callData;
{
   AMask newstate; AMask oldstate;
   AMask enmask, dismask;
   myRadioButton *rbp=(myRadioButton *)closure;
	
   rbp->dPtr->omask_old = rbp->dPtr->omask;
   rbp->dPtr->omask ^= (1<<rbp->num);
   enmask = rbp->dPtr->omask_old ^ rbp->dPtr->omask;  /* What changed */
   enmask &= rbp->dPtr->omask; /* newly enabled only */
   if(enmask!=0){
	   AFEnableOutput(rbp->dPtr->ac, enmask, &oldstate, &newstate);
   }
   dismask = rbp->dPtr->omask_old ^ rbp->dPtr->omask;  /* What changed */
   dismask &= ~rbp->dPtr->omask; /* newly enabled only */
   if(dismask != 0){
	   AFDisableOutput(rbp->dPtr->ac, dismask, &oldstate, &newstate);
   }
}

/* ARGSUSED */
static void 
ThumbedOutput(widget, sb_ptr, top_ptr)
Widget widget;
XtPointer sb_ptr, top_ptr;	
{
    float top = *((float *) top_ptr);
    Widget label = (Widget) ((myScrollBar *)sb_ptr)->label;
    myScrollBar *sbp = (myScrollBar *)sb_ptr;
    Arg args[1];
    char message[BUFSIZ];
    int gain;

    sbp->pos_old = sbp->pos; 
    sbp->pos = top;

   gain = (int)(sbp->pos * (sbp->dPtr->ogain_max - sbp->dPtr->ogain_min) + 
		sbp->dPtr->ogain_min);
   AFSetOutputGain(sbp->dPtr->ac, gain);
   AFFlush(aud);

    sprintf( message, "%3d", gain);
    XtSetArg( args[0], XtNlabel, message );
    XtSetValues( label, args, ONE );
}


/* ARGSUSED */
static void
Finish(client_data, id)
XtPointer client_data;
XtIntervalId id;
{
    exit(0);
}

/* ARGSUSED */
static void 
Destroyed(widget, closure, callData)
Widget widget;
XtPointer closure, callData;		
{
    XtAppAddTimeOut(XtWidgetToApplicationContext(widget), 1000, 
		(XtTimerCallbackProc)Finish, (XtPointer) NULL);
}

static void 
Syntax(app_con, call)
XtAppContext app_con;
char *call;
{
    XtDestroyApplicationContext(app_con);
    fprintf(stderr, "Usage: %s\n", call);
    exit(1);
}

/* ARGSUSED */
static void 
Quit(widget,closure,callData)
Widget widget;
XtPointer closure, callData;
{
    XtDestroyWidget((Widget)closure);
}


void
fatal(s)
char *s;
{
   fprintf(stderr, "%s\n",s);
   exit(1);
}
