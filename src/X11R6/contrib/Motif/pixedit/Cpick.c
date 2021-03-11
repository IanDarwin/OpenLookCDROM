/**********************************************************************
 Copyright (c) 1994 Mike Yang

 The X Consortium, and any party obtaining a copy of these files from
 the X Consortium, directly or indirectly, is granted, free of charge, a
 full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 nonexclusive right and license to deal in this software and
 documentation files (the "Software"), including without limitation the
 rights to use, copy, modify, merge, publish, distribute, sublicense,
 and/or sell copies of the Software, and to permit persons who receive
 copies from any such party to do so.  This license includes without
 limitation a license to do the foregoing actions under any patents of
 the party supplying this software to the X Consortium.
**********************************************************************/

/*
 * Cpick.c - Color picker widget for Motif Toolkit
 * 
 * Author:	Mike Yang (mikey@sgi.com)
 *		Silicon Graphics, Inc.
 * Date:	Mon Jul 29 1991
 */

#include <stdio.h>
#include <math.h>
#include <X11/Xos.h>
#include "CpickP.h"
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Scale.h>
#include <Xm/Frame.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/SelectioB.h>
#include "color.h"
#include <X11/Shell.h>

/* #define BUBBLE_MATCH	/* Uses a visible bubble sort when matching colors */

#define INCREMENT 10
#define WIDENEAR 8192
#define NARROWNEAR 768
#define MAXIMUM 65536
#define MINIMUM 0
#define MBASE 256
#define RATIO (MAXIMUM/MBASE)
#define FBASE 1000
#define NULLSTR " "

#ifndef RGBFILE
#define RGBFILE "/usr/lib/X11/rgb.txt"
#endif

/* These should be resources */
#define BORDER_MARGIN		20
#define INTRA_SCALE_SPACING	5
#define SCALE_SPACING		5
#define SCALE_SET_SPACING	20
#define INTER_SPACING		10
#define SCALE_WIDTH		100
#define BOX_MARGIN		10
#define BOX_SHADOW_THICKNESS	4
#define BUTTON_SPACING		4
#define WIDE_STRING		"Wide"
#define RANGE_STRING		"Range"
#define NARROW_STRING		"Narrow"
#define MATCH_STRING		"Match"

#define Offset(field) XtOffset(CpickWidget, field)

static XtResource resources[] = {
  {XmNhelpProc, XmCCallback, XmRCallback, sizeof(XtPointer),
     Offset(cpick.helpProc), XmRCallback, NULL},
  {XmNokProc, XmCCallback, XmRCallback, sizeof(XtPointer),
     Offset(cpick.okProc), XmRCallback, NULL},
  {XmNselectProc, XmCCallback, XmRCallback, sizeof(XtPointer),
     Offset(cpick.selectProc), XmRCallback, NULL},
  {XmNchangeProc, XmCCallback, XmRCallback, sizeof(XtPointer),
     Offset(cpick.changeProc), XmRCallback, NULL},
  {XmNrestoreProc, XmCCallback, XmRCallback, sizeof(XtPointer),
     Offset(cpick.restoreProc), XmRCallback, NULL},
  {XmNallocated, XmCAllocated, XmRXColor, sizeof(XtPointer),
     Offset(cpick.allocated), XmRXColor, NULL},
  {XmNcmap, XmCCmap, XmRColormap, sizeof(Colormap),
     Offset(cpick.cmap), XmRColormap, NULL},
  {XmNselectLabel, XmCLabel, XmRString, sizeof(String),
     Offset(cpick.selectlabel), XmRString, "Select"},
  {XmNcancelLabel, XmCLabel, XmRString, sizeof(String),
     Offset(cpick.cancellabel), XmRString, "Cancel"},
  {XmNrestoreLabel, XmCLabel, XmRString, sizeof(String),
     Offset(cpick.restorelabel), XmRString, "Restore"},
  {XmNhelpLabel, XmCLabel, XmRString, sizeof(String),
     Offset(cpick.helplabel), XmRString, "Help"},
  {XmNokLabel, XmCLabel, XmRString, sizeof(String),
     Offset(cpick.oklabel), XmRString, "OK"},
  {XmNnearPixels, XmCDimension, XmRDimension, sizeof(Dimension),
     Offset(cpick.nearpixels), XmRString, "64"},
  {XmNuseColors, XmCUsecolors, XmRBoolean, sizeof(Boolean),
     Offset(cpick.usecolors), XmRString, "False"},
};

static int formPositions[] = {
  0, 30,
  35, 65,
  70, 100,
};

static void ClassInitialize();
static void Initialize();
static void Realize();
static void Resize();
static void Redisplay();
static Boolean SetValues();
static XtGeometryResult GeometryManager();
static void ChangeManaged();

static int changeLabel();
static int doNew();
static int doExpose();
static int changePalette();
static int undomatch();
static int matchPalette();
static void setMlabelStr();

static void Notify();

CpickClassRec cpickClassRec = {
  {
    (WidgetClass) &xmManagerClassRec,	/* superclass		  */	
    "Cpick",				/* class_name		  */
    sizeof(CpickRec),			/* size			  */
    NULL,				/* class_initialize	  */
    NULL,				/* class_part_initialize  */
    FALSE,				/* class_inited		  */
    Initialize,				/* initialize		  */
    NULL,				/* initialize_hook	  */
    Realize,				/* realize		  */
    NULL,				/* actions		  */
    0,					/* num_actions		  */
    resources,				/* resources		  */
    XtNumber(resources),		/* resource_count	  */
    NULLQUARK,				/* xrm_class		  */
    TRUE,				/* compress_motion	  */
    TRUE,				/* compress_exposure	  */
    TRUE,				/* compress_enterleave    */
    FALSE,				/* visible_interest	  */
    NULL,				/* destroy		  */
    Resize,				/* resize		  */
    XtInheritExpose,			/* expose		  */
    SetValues,				/* set_values		  */
    NULL,				/* set_values_hook	  */
    XtInheritSetValuesAlmost,		/* set_values_almost	  */
    NULL,				/* get_values_hook	  */
    NULL,				/* accept_focus		  */
    XtVersion,				/* version		  */
    NULL,				/* callback_private	  */
    NULL,				/* tm_table		  */
    NULL,				/* query_geometry	  */
    XtInheritDisplayAccelerator,	/* display_accelerator	  */
    NULL				/* extension		  */
  },{
/* composite_class fields */
    /* geometry_handler   */    GeometryManager,
    /* change_managed     */    ChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	   */   NULL
  },{
/* constraint class record */
    /* no additional resources  */  NULL,
    /* num additional resources */  0,
    /* size of constraint rec   */  0,
    /* constraint_initialize    */  NULL,
    /* constraint_destroy       */  NULL,
    /* constraint_setvalue      */  NULL,
    /* extension                */  NULL,
  },{
/* manager class record */
    XtInheritTranslations,	 /* default translations */
    NULL,               	 /* syn_resources          */
    0,				 /* num_syn_resources      */
    NULL,                        /* syn_cont_resources     */
    0,                           /* num_syn_cont_resources */
    XmInheritParentProcess,	 /* parent_process */
    NULL,                        /* extension              */
  },{
/* Cpick class fields */
    /* empty		  */	0,
  }
};

WidgetClass cpickWidgetClass = (WidgetClass)&cpickClassRec;

typedef unsigned long (*PixProc)();

typedef struct {
  int ih, iw, x0, y0, rownum;
  PixProc pix;
  CpickWidget cw;
} vals_cpick;

static vals_cpick nval;

String colors[] = {"red", "green", "blue",
		     "gold", "khaki", "wheat",
		     "cyan", "magenta", "yellow"};

static char STRING[] = "rgbhsvcmy";

static Arg args[20];
static int count;

static Position getP(w, s)
Widget w;
String s;
{
  Position result;
  int v;

  count = 0;
  XtSetArg(args[count], s, (XtArgVal) &result);  count++;
  XtGetValues(w, args, count);
  v = result;

  return(v);
}

static Dimension getD(w, s)
Widget w;
String s;
{
  Dimension result;
  int v;

  count = 0;
  XtSetArg(args[count], s, (XtArgVal) &result);  count++;
  XtGetValues(w, args, count);
  v = result;

  return(v);
}

static SetScaleValue(CpickWidget cw, int which, int value)
{
  XmScaleSetValue(cw->cpick.scales[which], value);
  changeLabel(cw, which);
}

static SetScaleFloatValue(CpickWidget cw, int which, int value)
{
  XmScaleSetValue(cw->cpick.scales[which], (int) ((FBASE*value)/MBASE));
  changeLabel(cw, which);
}

static XtGeometryResult GeometryManager(w, request, reply)
    Widget w;
    XtWidgetGeometry *request;
    XtWidgetGeometry *reply;    /* RETURN */
{
  return XtGeometryYes;
}

static void ChangeManaged(cw)
CpickWidget cw;
{
  XtMakeResizeRequest((Widget) cw, getD(cw->cpick.tlevel, XmNwidth),
		      getD(cw->cpick.tlevel, XmNheight),
		      NULL, NULL);
}

static void Realize( gw, valueMask, attributes )
   Widget gw;
   XtValueMask *valueMask;
   XSetWindowAttributes *attributes;
{
  CpickWidget cw = (CpickWidget) gw;
  Dimension newHeight;

  XtCreateWindow( gw, InputOutput, (Visual *)CopyFromParent,
		 *valueMask, attributes );
  XtRealizeWidget(cw->cpick.tlevel);

  count = 0;
  XtSetArg(args[count], XmNheight, &newHeight);  count++;
  XtGetValues(cw->cpick.hexText, args, count);
  if (newHeight > cw->cpick.oldHeight) {
    count = 0;
    XtSetArg(args[count], XmNmarginHeight,
	     (newHeight-cw->cpick.oldHeight)/2);  count++;
    XtSetValues(cw->cpick.hexText, args, count);
  }
}

static void Resize( gw )
   Widget gw;
{
  CpickWidget cw = (CpickWidget) gw;

  XClearWindow( XtDisplay(gw), XtWindow(gw) );
  XtResizeWidget(cw->cpick.tlevel, cw->core.width,
		 cw->core.height, (Dimension) 1);
  XClearWindow(XtDisplay(cw->cpick.nlevel), XtWindow(cw->cpick.nlevel) );
  doExpose(cw);
}


static Boolean SetValues( current, request, desired )
   Widget current,		/* what I am */
          request,		/* what he wants me to be */
          desired;		/* what I will become */
{
  CpickWidget sw = (CpickWidget) desired;
  Boolean redraw = TRUE; /* be stupid for now */
  
  sw->cpick.oldvalue = *(sw->cpick.allocated);
  doNew(sw);
  return(redraw);
}

static updateBox(cw)
CpickWidget cw;
{
  XStoreColor(XtDisplay(cw),
	      cw->cpick.cmap,
	      cw->cpick.allocated);
}

void CenterWidget(parent, child)
Widget parent, child;
{
  Position x, y;

  x = (getD(parent, XmNwidth) - getD(child, XmNwidth)) / 2;
  y = (getD(parent, XmNheight) - getD(child, XmNheight)) / 2;
  if (x < 0) x = 0;
  XtMoveWidget(child, x, y);
}

static char hexDigit(v)
int v;
{
  if (v < 10)
    return '0'+v;
  else
    return 'a'+(v-10);
}

static int unhexChar(ch)
char ch;
{
  if (ch >= '0' && ch <= '9')
    return (ch-'0');
  else if (ch >= 'a' && ch <= 'f')
    return (ch-'a'+10);
  else if (ch >= 'A' && ch <= 'F')
    return (ch-'A'+10);
  else
    return -1;
}

static Boolean hexChars(s)
char *s;
{
  int each;

  for (each=0; each<strlen(s); each++) {
    if (unhexChar(s[each]) == -1)
      return(FALSE);
  }
  return(TRUE);
}

static createHex(r, g, b, s)
int r, g, b;
char s[];
{
  sprintf(s, "#%c%c%c%c%c%c",
	  hexDigit(256*r/MBASE/16),
	  hexDigit(256*r/MBASE % 16),
	  hexDigit(256*g/MBASE/16),
	  hexDigit(256*g/MBASE % 16),
	  hexDigit(256*b/MBASE/16),
	  hexDigit(256*b/MBASE % 16));
}

static changeRGB(cw, which)
CpickWidget cw;
int which;
{
  RGB rgb;
  HSV hsv;
  CMY cmy;
  char str[13];

  if (cw->cpick.matched) {
    setMlabelStr(cw, NULLSTR);
  }
  if (which < H) {
    rgb.r = cw->cpick.values[R]*RATIO;
    rgb.g = cw->cpick.values[G]*RATIO;
    rgb.b = cw->cpick.values[B]*RATIO;
    hsv = RGBToHSV(rgb);
    cw->cpick.values[H] = (int) (hsv.h*(MBASE-1));
    cw->cpick.values[S] = (int) (hsv.s*(MBASE-1));
    cw->cpick.values[V] = (int) (hsv.v*(MBASE-1));
    SetScaleFloatValue(cw, H, cw->cpick.values[H]);
    SetScaleFloatValue(cw, S, cw->cpick.values[S]);
    SetScaleFloatValue(cw, V, cw->cpick.values[V]);
    cmy = RGBToCMY(rgb);
    cw->cpick.values[C] = cmy.c*RATIO/MAXIMUM;
    cw->cpick.values[M] = cmy.m*RATIO/MAXIMUM;
    cw->cpick.values[Y] = cmy.y*RATIO/MAXIMUM;
    SetScaleValue(cw, C, cw->cpick.values[C]);
    SetScaleValue(cw, M, cw->cpick.values[M]);
    SetScaleValue(cw, Y, cw->cpick.values[Y]);
  } else if (which < C) {
    hsv.h = cw->cpick.values[H]/(float) (MBASE-1);
    hsv.s = cw->cpick.values[S]/(float) (MBASE-1);
    hsv.v = cw->cpick.values[V]/(float) (MBASE-1);
    rgb = HSVToRGB(hsv);
    cw->cpick.values[R] = rgb.r*RATIO/MAXIMUM;
    cw->cpick.values[G] = rgb.g*RATIO/MAXIMUM;
    cw->cpick.values[B] = rgb.b*RATIO/MAXIMUM;
    SetScaleValue(cw, R, cw->cpick.values[R]);
    SetScaleValue(cw, G, cw->cpick.values[G]);
    SetScaleValue(cw, B, cw->cpick.values[B]);
    cmy = RGBToCMY(rgb);
    cw->cpick.values[C] = cmy.c*RATIO/MAXIMUM;
    cw->cpick.values[M] = cmy.m*RATIO/MAXIMUM;
    cw->cpick.values[Y] = cmy.y*RATIO/MAXIMUM;
    SetScaleValue(cw, C, cw->cpick.values[C]);
    SetScaleValue(cw, M, cw->cpick.values[M]);
    SetScaleValue(cw, Y, cw->cpick.values[Y]);
  } else {
    cmy.c = cw->cpick.values[C]*RATIO;
    cmy.m = cw->cpick.values[M]*RATIO;
    cmy.y = cw->cpick.values[Y]*RATIO;
    rgb = CMYToRGB(cmy);
    cw->cpick.values[R] = rgb.r*RATIO/MAXIMUM;
    cw->cpick.values[G] = rgb.g*RATIO/MAXIMUM;
    cw->cpick.values[B] = rgb.b*RATIO/MAXIMUM;
    SetScaleValue(cw, R, cw->cpick.values[R]);
    SetScaleValue(cw, G, cw->cpick.values[G]);
    SetScaleValue(cw, B, cw->cpick.values[B]);
    hsv = RGBToHSV(rgb);
    cw->cpick.values[H] = (int) (hsv.h*(MBASE-1));
    cw->cpick.values[S] = (int) (hsv.s*(MBASE-1));
    cw->cpick.values[V] = (int) (hsv.v*(MBASE-1));
    SetScaleFloatValue(cw, H, cw->cpick.values[H]);
    SetScaleFloatValue(cw, S, cw->cpick.values[S]);
    SetScaleFloatValue(cw, V, cw->cpick.values[V]);
  }
  createHex(cw->cpick.values[R], cw->cpick.values[G], cw->cpick.values[B],
	    str);
  XmTextFieldSetString(cw->cpick.hexText, str);
  cw->cpick.allocated->red = cw->cpick.values[R]*RATIO;
  cw->cpick.allocated->green = cw->cpick.values[G]*RATIO;
  cw->cpick.allocated->blue = cw->cpick.values[B]*RATIO;
  changePalette(cw, FALSE);
  if (XtIsRealized((Widget) cw) && cw->cpick.changeProc) {
    XtCallCallbacks((Widget) cw, XmNchangeProc,
		    (XtPointer) (cw->cpick.allocated));
  }
}

static update(cmd, client_data, call_data)
     Widget cmd;
     XtPointer client_data;
     XtPointer call_data;
{
  int which;
  CpickWidget cw = (CpickWidget) client_data;
  XmScaleCallbackStruct *cb = (XmScaleCallbackStruct *) call_data;

  cw = (CpickWidget) client_data;

  for (which=0; which<NUM; which++) {
    if (cw->cpick.scales[which] == cmd) {
      if (which >= H && which <= V) {
	cw->cpick.values[which] = (cb->value*MBASE)/FBASE;
      } else {
	cw->cpick.values[which] = cb->value;
      }
      changeLabel(cw, which);
      break;
    }
  }

  changeRGB(cw, which);
  updateBox(cw);
}

static newRGB(cw, r, g, b)
CpickWidget cw;
unsigned short r, g, b;
{
  cw->cpick.values[R] = r/RATIO;
  cw->cpick.values[G] = g/RATIO;
  cw->cpick.values[B] = b/RATIO;
  SetScaleValue(cw, R, cw->cpick.values[R]);
  SetScaleValue(cw, G, cw->cpick.values[G]);
  SetScaleValue(cw, B, cw->cpick.values[B]);
  changeRGB(cw, R);
  updateBox(cw);
}
			      
static changeLabel(cw, which)
CpickWidget cw;
int which;
{
  char str[10];
  XmString xs;

  if (which < H || which > V) {
    sprintf(str, "%-5d", cw->cpick.values[which]);
  } else {
    sprintf(str, "%-5.3f", cw->cpick.values[which]/(float) (MBASE-1));
  }
  count = 0;
  xs = XmStringCreateSimple(str);
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetValues(cw->cpick.labels[which], args, count);
}

static createScales(cw)
CpickWidget cw;
{
  int each;
  Widget scaleSet, scale;
  XColor xc, ignore;
  XmString xs, empty, lstring;
  char str[5];
  Dimension height = 0, maxWidth = 0, width;

  empty = XmStringCreateSimple(NULLSTR);
  lstring = XmStringCreateSimple("0.000");

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNtopOffset, BORDER_MARGIN);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftOffset, BORDER_MARGIN);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomOffset, BORDER_MARGIN);  count++;
  cw->cpick.scaleSets = XmCreateForm(cw->cpick.tlevel, "scaleSets",
				     args, count);
  XtManageChild(cw->cpick.scaleSets);

  for (each=0; each<NUM; each++) {
    if (each % 3 == 0) {
      count = 0;
      XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
      XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
      XtSetArg(args[count], XmNtopAttachment, XmATTACH_POSITION);  count++;
      XtSetArg(args[count], XmNtopPosition, formPositions[(each/3)*2]);
        count++;
      XtSetArg(args[count], XmNbottomAttachment, XmATTACH_POSITION);  count++;
      XtSetArg(args[count], XmNbottomPosition, formPositions[(each/3)*2+1]);
	count++;
      scaleSet = XmCreateForm(cw->cpick.scaleSets, "scaleSet",
				   args, count);
      XtManageChild(scaleSet);
    }

    count = 0;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_POSITION);  count++;
    XtSetArg(args[count], XmNtopPosition, (each%3)*33);  count++;
    XtSetArg(args[count], XmNbottomAttachment, XmATTACH_POSITION);  count++;
    XtSetArg(args[count], XmNbottomPosition, 33+(each%3)*33);  count++;
    scale = XmCreateForm(scaleSet, "scaleGroup", args, count);
    XtManageChild(scale);
    
    count = 0;
    sprintf(str, "%c", STRING[each]);
    xs = XmStringCreateSimple(str);
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNmarginWidth, 0);  count++;
    XtSetArg(args[count], XmNmarginHeight, 0);  count++;
    XtSetArg(args[count], XmNrecomputeSize, False);  count++;
    XtSetArg(args[count], XmNlabelString, xs);  count++;
    if (cw->cpick.usecolors && ((each < H) || (each > V))) {
      XAllocNamedColor(XtDisplay(cw),
		       cw->cpick.cmap,
		       colors[each], &xc, &ignore);
      XtSetArg(args[count], XmNforeground, xc.pixel);  count++;
      cw->cpick.names[each] = XmCreateLabel(scale, "name", args, count);
    } else {
      cw->cpick.names[each] = XmCreateLabelGadget(scale, "name", args, count);
    }
    XtManageChild(cw->cpick.names[each]); 
    XmStringFree(xs);
    if (!height) {
      count = 0;
      XtSetArg(args[count], XmNheight, &height);  count++;
      XtGetValues(cw->cpick.names[each], args, count);
    }
    count = 0;
    XtSetArg(args[count], XmNwidth, &width);  count++;
    XtGetValues(cw->cpick.names[each], args, count);
    if (width > maxWidth) {
      maxWidth = width;
    }

    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
    XtSetArg(args[count], XmNleftWidget, cw->cpick.names[each]);  count++;
    XtSetArg(args[count], XmNleftOffset, INTRA_SCALE_SPACING);  count++;
    XtSetArg(args[count], XmNwidth, SCALE_WIDTH);  count++;
    XtSetArg(args[count], XmNshowValue, False);  count++;
    XtSetArg(args[count], XmNorientation, XmHORIZONTAL);  count++;
    XtSetArg(args[count], XmNtitleString, empty);  count++;
    XtSetArg(args[count], XmNheight, height);  count++;
    XtSetArg(args[count], XmNminimum, 0);  count++;
    if (each < H || each > V) {
      XtSetArg(args[count], XmNmaximum, MBASE-1);  count++;
    } else {
      XtSetArg(args[count], XmNmaximum, FBASE-1);  count++;
      XtSetArg(args[count], XmNdecimalPoints, 3);  count++;
    }
    cw->cpick.scales[each] = XmCreateScale(scale, "scale", args, count);
    XtManageChild(cw->cpick.scales[each]);
    XtAddCallback(cw->cpick.scales[each], XmNvalueChangedCallback,
		  (XtCallbackProc) update, (XtPointer) cw);
    XtAddCallback(cw->cpick.scales[each], XmNdragCallback,
		  (XtCallbackProc) update, (XtPointer) cw);
    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
    XtSetArg(args[count], XmNleftWidget, cw->cpick.scales[each]);  count++;
    XtSetArg(args[count], XmNleftOffset, INTRA_SCALE_SPACING);  count++;
    XtSetArg(args[count], XmNlabelString, lstring);  count++;
    XtSetArg(args[count], XmNrecomputeSize, False);  count++;
    cw->cpick.labels[each] = XmCreateLabelGadget(scale, "label", args, count);
    XtManageChild(cw->cpick.labels[each]);
  }

  count = 0;
  XtSetArg(args[count], XmNwidth, maxWidth);  count++;
  for (each=0; each<NUM; each++) {
    XtSetValues(cw->cpick.names[each], args, count);
  }
  XmStringFree(empty);
  XmStringFree(lstring);
}

static void dohex(cmd, client_data, call_data)
     Widget cmd;
     XtPointer client_data;
     XtPointer call_data;
{
  CpickWidget cw = (CpickWidget) client_data;
  char *response, *str;

  response = str = XmTextGetString(cw->cpick.hexText);
  if (response[0] == '#') {
    response++;
  }
  if ((strlen(response) != 6 && strlen(response) != 3)
      || !hexChars(response)) {
    setMlabelStr(cw, "Illegal hex string.");
    XBell(XtDisplay(cw), 0);
    cw->cpick.matched = True;
  } else if (strlen(response) == 6) {
    cw->cpick.allocated->red =
      (unhexChar(response[0])*16+unhexChar(response[1]))*RATIO;
    cw->cpick.allocated->green =
      (unhexChar(response[2])*16+unhexChar(response[3]))*RATIO;
    cw->cpick.allocated->blue =
      (unhexChar(response[4])*16+unhexChar(response[5]))*RATIO;
    cw->cpick.keep = FALSE;
    doNew(cw);
  } else if (strlen(response) == 3) {
    cw->cpick.allocated->red = unhexChar(response[0])*16*RATIO;
    cw->cpick.allocated->green = unhexChar(response[1])*16*RATIO;
    cw->cpick.allocated->blue = unhexChar(response[2])*16*RATIO;
    cw->cpick.keep = FALSE;
    doNew(cw);
  }
  XtFree(str);
}

static dopalette(cmd, client_data, call_data)
     Widget cmd;
     XtPointer client_data;
     XtPointer call_data;
{
  CpickWidget cw = (CpickWidget) client_data;
  XmString xs;

  cw->cpick.wide = (cw->cpick.wide+1) % (RANGE+1);
  if (cw->cpick.wide == WIDE) {
    xs = XmStringCreateSimple(WIDE_STRING);
  } else if (cw->cpick.wide == RANGE) {
    xs = XmStringCreateSimple(RANGE_STRING);
  } else {
    xs = XmStringCreateSimple(NARROW_STRING);
  }
  count = 0;
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetValues(cw->cpick.paletteButton, args, count);
  XmStringFree(xs);
  changePalette(cw, TRUE);
}

static domatch(cmd, client_data, call_data)
     Widget cmd;
     XtPointer client_data;
     XtPointer call_data;
{
  CpickWidget cw = (CpickWidget) client_data;

  cw->cpick.matched = TRUE;
  matchPalette(cw);
}

static createBox(cw)
CpickWidget cw;
{
  Widget match;
  XmString xs;

  count = 0;
  XtSetArg(args[count], XmNadjustMargin, False);  count++;
  XtSetArg(args[count], XmNmarginWidth, 0);  count++;
  XtSetArg(args[count], XmNmarginHeight, 0);  count++;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNtopOffset, BORDER_MARGIN);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightOffset, BORDER_MARGIN);  count++;
  XtSetArg(args[count], XmNorientation, XmVERTICAL);  count++;
  XtSetArg(args[count], XmNpacking, XmPACK_COLUMN);  count++;
  XtSetArg(args[count], XmNentryAlignment, XmALIGNMENT_CENTER);  count++;
  XtSetArg(args[count], XmNspacing, BUTTON_SPACING);  count++;
  cw->cpick.boxButtons = XmCreateRowColumn(cw->cpick.tlevel, "boxButtons",
					   args, count);
  XtManageChild(cw->cpick.boxButtons);
  
  count = 0;
  XtSetArg(args[count], XmNcolumns, 8);  count++;
  cw->cpick.hexText = XmCreateTextField(cw->cpick.boxButtons, "hexText",
					args, count);
  XtManageChild(cw->cpick.hexText);
  XtAddCallback(cw->cpick.hexText, XmNactivateCallback,
		(XtCallbackProc) dohex, (XtPointer) cw);
  count = 0;
  XtSetArg(args[count], XmNheight, &cw->cpick.oldHeight);  count++;
  XtGetValues(cw->cpick.hexText, args, count);

  count = 0;
  xs = XmStringCreateSimple(RANGE_STRING);
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetArg(args[count], XmNrecomputeSize, False);  count++;
  XtSetArg(args[count], XmNmarginLeft, 0);  count++;
  XtSetArg(args[count], XmNmarginRight, 0);  count++;
  XtSetArg(args[count], XmNmarginTop, 0);  count++;
  XtSetArg(args[count], XmNmarginBottom, 0);  count++;
  cw->cpick.paletteButton = XmCreatePushButton(cw->cpick.boxButtons,
					       "paletteButton", args, count);
  XtManageChild(cw->cpick.paletteButton);
  XmStringFree(xs);
  XtAddCallback(cw->cpick.paletteButton, XmNactivateCallback,
		(XtCallbackProc) dopalette, (XtPointer) cw);

  count = 0;
  xs = XmStringCreateSimple(MATCH_STRING);
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetArg(args[count], XmNrecomputeSize, False);  count++;
  XtSetArg(args[count], XmNmarginLeft, 0);  count++;
  XtSetArg(args[count], XmNmarginRight, 0);  count++;
  XtSetArg(args[count], XmNmarginTop, 0);  count++;
  XtSetArg(args[count], XmNmarginBottom, 0);  count++;
  cw->cpick.matchButton = XmCreatePushButton(cw->cpick.boxButtons,
					     "matchButton", args, count);
  XtManageChild(cw->cpick.matchButton);
  XmStringFree(xs);
  XtAddCallback(cw->cpick.matchButton, XmNactivateCallback,
		(XtCallbackProc) domatch, (XtPointer) cw);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNtopOffset, BORDER_MARGIN);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNleftWidget, cw->cpick.scaleSets);  count++;
  XtSetArg(args[count], XmNleftOffset, INTER_SPACING);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNrightWidget, cw->cpick.boxButtons);  count++;
  XtSetArg(args[count], XmNrightOffset, INTER_SPACING);  count++;
  XtSetArg(args[count], XmNshadowType, XmSHADOW_IN);  count++;
  XtSetArg(args[count], XmNmarginWidth, BOX_MARGIN);  count++;
  XtSetArg(args[count], XmNmarginHeight, BOX_MARGIN);  count++;
  cw->cpick.bcontainer = XmCreateFrame(cw->cpick.tlevel, "bcontainer",
				       args, count);
  XtManageChild(cw->cpick.bcontainer);

  count = 0;
  XtSetArg(args[count], XmNshadowType, XmSHADOW_OUT);  count++;
  XtSetArg(args[count], XmNshadowThickness, BOX_SHADOW_THICKNESS);  count++;
  cw->cpick.bframe = XmCreateFrame(cw->cpick.bcontainer, "bframe",
				   args, count);
  XtManageChild(cw->cpick.bframe);

  count = 0;
  cw->cpick.box = XmCreateDrawingArea(cw->cpick.bframe, "box", args, count);
  XtManageChild(cw->cpick.box);
}

static doNew(cw)
CpickWidget cw;
{
  newRGB(cw,
	 cw->cpick.allocated->red,
	 cw->cpick.allocated->green,
	 cw->cpick.allocated->blue);
  if (cw->cpick.box) {
    count = 0;
    XtSetArg(args[count], XmNbackground, cw->cpick.allocated->pixel);  count++;
    XtSetValues(cw->cpick.box, args, count);
  }
}

static nButtonHandler(w, val, event)
Widget w;
vals_cpick *val;
XButtonEvent *event;
{
  int newpix, v;
  XColor xc;
  CpickWidget cw = val->cw;

  if (!val->ih || !val->iw) {
    return;
  }

  v = (event->x-val->x0)/val->iw + ((event->y-val->y0)/val->ih)*val->rownum;
  newpix = val->pix(cw, v);
  if (newpix != MAXIMUM) {
    xc.pixel = newpix;
    XQueryColor(XtDisplay(cw), cw->cpick.cmap, &xc);
    cw->cpick.allocated->red = xc.red;
    cw->cpick.allocated->green = xc.green;
    cw->cpick.allocated->blue = xc.blue;
    cw->cpick.keep = TRUE;
    doNew(cw);
    if (cw->cpick.matched) {
      setMlabelStr(cw, cw->cpick.mnames[v]);
    }
  }
}

static unsigned long nPixel(cw, each)
CpickWidget cw;
int each;
{
  if (each < cw->cpick.nearpixels)
    return cw->cpick.nearcells[each].pixel;
  else
    return MAXIMUM;
}

static createGrid(wid, num, x0, y0, h, w, proc, val, pix)
Widget wid;
int num;
Position x0, y0;
Dimension h, w;
XtEventHandler proc;
vals_cpick *val;
PixProc pix;
{
  int each, x, y, hs, ws;
  double hsize, wsize;
  GC gc;
  XGCValues gcv;
  unsigned long black;

  val->pix = pix;

  hsize = sqrt((double) num*VNEARRATIO/HNEARRATIO);
  wsize = ceil(hsize*HNEARRATIO/VNEARRATIO);
  hsize = ceil(hsize);

  val->ih = (int) ((h-1)/hsize);
  val->iw = (int) ((w-1)/wsize);
  val->rownum = (int) wsize;
  hs = (int) hsize*val->ih+1;
  ws = (int) wsize*val->iw+1;

/*
  XtMoveWidget(wid, (Position) x0, (Position) y0);
  XtResizeWidget(wid, w, h, (Dimension) 1);
*/

  val->y0 = (h-hs)/2;
  val->x0 = (w-ws)/2;

  gcv.function = GXcopy;
  gcv.fill_style = FillSolid;
  black = XBlackPixel(XtDisplay(wid),
		      XDefaultScreen(XtDisplay(wid)));
  gcv.background = black;
  gc = XCreateGC(XtDisplay(wid), XtWindow(wid),
		 GCBackground | GCFunction | GCFillStyle, &gcv);

  XSync(XtDisplay(wid),0);
  for (each=0; each<num; each++) {
    x = (each % val->rownum)*val->iw;
    y = (each/val->rownum)*val->ih;
    XSetForeground(XtDisplay(wid), gc, pix(val->cw, each));
    XFillRectangle(XtDisplay(wid), XtWindow(wid), gc, val->x0+x, val->y0+y,
		   val->iw, val->ih);
    XSetForeground(XtDisplay(wid), gc, black);
    XDrawRectangle(XtDisplay(wid), XtWindow(wid), gc, val->x0+x, val->y0+y,
		   val->iw, val->ih);
  }

  XtAddEventHandler(wid, ButtonPressMask, FALSE, (XtEventHandler) proc,
		    (XtPointer) val);
  XFreeGC(XtDisplay(wid), gc);
}

static doExpose(cw)
CpickWidget cw;
{
  nval.cw = cw;
  createGrid(cw->cpick.nlevel, (int) cw->cpick.nearpixels,
	     getP(cw->cpick.nlevel, XmNx),
	     getP(cw->cpick.nlevel, XmNy),
	     getD(cw->cpick.nlevel, XmNheight),
	     getD(cw->cpick.nlevel, XmNwidth),
	     nButtonHandler, &nval, nPixel);
}

static nlevel_expose(w, client_data, call_data)
     Widget w;
     XtPointer client_data;
     XtPointer call_data;
{
  XEvent ev;
  CpickWidget cw = (CpickWidget) client_data;

  XFlush(XtDisplay(w));
  while (XCheckWindowEvent(XtDisplay(w), XtWindow(w), ExposureMask, &ev)) {
    /* eat it */
  }
  
  doExpose(cw);
}

static doselect(cmd, client_data, call_data)
     Widget cmd;
     XtPointer client_data;
     XtPointer call_data;
{
  CpickWidget cw = (CpickWidget) client_data;

  undomatch(cw);
  if (cw->cpick.selectProc) {
    XtCallCallbacks((Widget) cw, XmNselectProc,
		    (XtPointer)(cw->cpick.allocated));
  }
}

static docancel(cmd, client_data, call_data)
     Widget cmd;
     XtPointer client_data;
     XtPointer call_data;
{
  CpickWidget cw = (CpickWidget) client_data;

  *(cw->cpick.allocated) = cw->cpick.oldvalue;
  doNew(cw);
}

static dorestore(cmd, client_data, call_data)
     Widget cmd;
     XtPointer client_data;
     XtPointer call_data;
{
  CpickWidget cw = (CpickWidget) client_data;

  if (cw->cpick.restoreProc) {
    XtCallCallbacks((Widget) cw, XmNrestoreProc,
		    (XtPointer)(cw->cpick.allocated));
  }
  changePalette(cw, TRUE);
}

static dook(cmd, client_data, call_data)
     Widget cmd;
     XtPointer client_data;
     XtPointer call_data;
{
  CpickWidget cw = (CpickWidget) client_data;

  if (cw->cpick.okProc) {
    XtCallCallbacks((Widget) cw, XmNokProc,
		    (XtPointer)(cw->cpick.allocated));
  }
}

static dohelp(cmd, client_data, call_data)
     Widget cmd;
     XtPointer client_data;
     XtPointer call_data;
{
  CpickWidget cw = (CpickWidget) client_data;

  if (cw->cpick.helpProc) {
    XtCallCallbacks((Widget) cw, XmNhelpProc,
		    (XtPointer)(cw->cpick.allocated));
  }
}

static createCommands(cw)
CpickWidget cw;
{
  Widget buffer;
  XmString xs;

  count = 0;
  XtSetArg(args[count], XmNadjustMargin, False);  count++;
  XtSetArg(args[count], XmNmarginWidth, 0);  count++;
  XtSetArg(args[count], XmNmarginHeight, 0);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightOffset, BORDER_MARGIN);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomOffset, BORDER_MARGIN);  count++;
  XtSetArg(args[count], XmNorientation, XmHORIZONTAL);  count++;
  XtSetArg(args[count], XmNpacking, XmPACK_COLUMN);  count++;
  XtSetArg(args[count], XmNentryAlignment, XmALIGNMENT_CENTER);  count++;
  XtSetArg(args[count], XmNspacing, BUTTON_SPACING);  count++;
  cw->cpick.commandBox = XmCreateRowColumn(cw->cpick.tlevel, "commandBox",
					   args, count);
  XtManageChild(cw->cpick.commandBox);

  count = 0;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNleftWidget, cw->cpick.scaleSets);  count++;
  XtSetArg(args[count], XmNleftOffset, INTER_SPACING);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNrightWidget, cw->cpick.commandBox);  count++;
  XtSetArg(args[count], XmNwidth, 1);  count++;
  buffer = XmCreateDrawingArea(cw->cpick.tlevel, "commandBuffer", args, count);
  XtManageChild(buffer);

  if (cw->cpick.selectlabel) {
    count = 0;
    xs = XmStringCreateSimple(cw->cpick.selectlabel);
    XtSetArg(args[count], XmNlabelString, xs);  count++;
    XtSetArg(args[count], XmNmarginLeft, 0);  count++;
    XtSetArg(args[count], XmNmarginRight, 0);  count++;
    XtSetArg(args[count], XmNmarginTop, 0);  count++;
    XtSetArg(args[count], XmNmarginBottom, 0);  count++;
    cw->cpick.select0 = XmCreatePushButton(cw->cpick.commandBox, "select",
					   args, count);
    XtManageChild(cw->cpick.select0);
    XtAddCallback(cw->cpick.select0, XmNactivateCallback,
		  (XtCallbackProc) doselect, (XtPointer) cw);
    XmStringFree(xs);
  }
  if (cw->cpick.cancellabel) {
    count = 0;
    xs = XmStringCreateSimple(cw->cpick.cancellabel);
    XtSetArg(args[count], XmNlabelString, xs);  count++;
    XtSetArg(args[count], XmNmarginLeft, 0);  count++;
    XtSetArg(args[count], XmNmarginRight, 0);  count++;
    XtSetArg(args[count], XmNmarginTop, 0);  count++;
    XtSetArg(args[count], XmNmarginBottom, 0);  count++;
    cw->cpick.cancel = XmCreatePushButton(cw->cpick.commandBox, "cancel",
					  args, count);
    XtManageChild(cw->cpick.cancel);
    XtAddCallback(cw->cpick.cancel, XmNactivateCallback,
		  (XtCallbackProc) docancel, (XtPointer) cw);
    XmStringFree(xs);
  }
  if (cw->cpick.restorelabel) {
    count = 0;
    xs = XmStringCreateSimple(cw->cpick.restorelabel);
    XtSetArg(args[count], XmNlabelString, xs);  count++;
    XtSetArg(args[count], XmNmarginLeft, 0);  count++;
    XtSetArg(args[count], XmNmarginRight, 0);  count++;
    XtSetArg(args[count], XmNmarginTop, 0);  count++;
    XtSetArg(args[count], XmNmarginBottom, 0);  count++;
    cw->cpick.restore = XmCreatePushButton(cw->cpick.commandBox, "restore",
					   args, count);
    XtManageChild(cw->cpick.restore);
    XtAddCallback(cw->cpick.restore, XmNactivateCallback,
		  (XtCallbackProc) dorestore, (XtPointer) cw);
    XmStringFree(xs);
  }
  if (cw->cpick.oklabel) {
    count = 0;
    xs = XmStringCreateSimple(cw->cpick.oklabel);
    XtSetArg(args[count], XmNlabelString, xs);  count++;
    XtSetArg(args[count], XmNmarginLeft, 0);  count++;
    XtSetArg(args[count], XmNmarginRight, 0);  count++;
    XtSetArg(args[count], XmNmarginTop, 0);  count++;
    XtSetArg(args[count], XmNmarginBottom, 0);  count++;
    cw->cpick.ok = XmCreatePushButton(cw->cpick.commandBox, "ok",
				      args, count);
    XtManageChild(cw->cpick.ok);
    XtAddCallback(cw->cpick.ok, XmNactivateCallback,
		  (XtCallbackProc) dook, (XtPointer) cw);
    XmStringFree(xs);
  }
  if (cw->cpick.helplabel) {
    count = 0;
    xs = XmStringCreateSimple(cw->cpick.helplabel);
    XtSetArg(args[count], XmNlabelString, xs);  count++;
    XtSetArg(args[count], XmNmarginLeft, 0);  count++;
    XtSetArg(args[count], XmNmarginRight, 0);  count++;
    XtSetArg(args[count], XmNmarginTop, 0);  count++;
    XtSetArg(args[count], XmNmarginBottom, 0);  count++;
    cw->cpick.help = XmCreatePushButton(cw->cpick.commandBox, "help",
				      args, count);
    XtManageChild(cw->cpick.help);
    XtAddCallback(cw->cpick.help, XmNactivateCallback,
		  (XtCallbackProc) dohelp, (XtPointer) cw);
    XmStringFree(xs);
  }

  count = 0;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNleftWidget, cw->cpick.scaleSets);  count++;
  XtSetArg(args[count], XmNleftOffset, INTER_SPACING);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightOffset, BORDER_MARGIN);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNbottomWidget, cw->cpick.commandBox);  count++;
  XtSetArg(args[count], XmNbottomOffset, INTER_SPACING);  count++;
  cw->cpick.mframe = XmCreateFrame(cw->cpick.tlevel, "mframe", args, count);
  XtManageChild(cw->cpick.mframe);

  count = 0;
  xs = XmStringCreateSimple(NULLSTR);
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetArg(args[count], XmNalignment, XmALIGNMENT_CENTER);  count++;
  XtSetArg(args[count], XmNrecomputeSize, False);  count++;
  cw->cpick.mlabel = XmCreateLabel(cw->cpick.mframe, "mlabel", args, count);
  XtManageChild(cw->cpick.mlabel);
  XmStringFree(xs);
  cw->cpick.matched = FALSE;
}

static initPalette(cw)
CpickWidget cw;
{
  unsigned long cells[MAXPIXELS];
  int each;

  if (!XAllocColorCells(XtDisplay(cw), cw->cpick.cmap, FALSE, NULL, 0,
			cells, (int) cw->cpick.nearpixels)) {
    fprintf(stderr, "Cpick: Can't allocate %d cells for palette.\n", (int) cw->cpick.nearpixels);
    exit(1);
  }
  for (each=0; each<(int) cw->cpick.nearpixels; each++) {
    cw->cpick.nearcells[each].pixel = cells[each];
    cw->cpick.nearcells[each].flags = DoRed | DoGreen | DoBlue;
  }
  changePalette(cw, TRUE);
}

static changePalette(cw, new)
CpickWidget cw;
Boolean new;
{
  int each, each2, r, g, b, nr, ng, nb, eachr, eachg, eachb;
  int delta, halfr, halfg, halfb, near;
  double hueinc;
  RGB rgb;
  HSV hsv;
  int mods[MAXPIXELS][3], nummods;

  if (new) {
    undomatch(cw);
  }
  if (cw->cpick.keep) {
    cw->cpick.keep = FALSE;
  } else {
    if (cw->cpick.wide == NARROW) {
      halfr = halfg = halfb = (int) cw->cpick.nearpixels/2;
      r = cw->cpick.allocated->red;
      g = cw->cpick.allocated->green;
      b = cw->cpick.allocated->blue;

      for (each=0; each<(int) cw->cpick.nearpixels/2; each++) {
	if (r-halfr*NARROWNEAR < 0)
	  halfr--;
	if (r+((int) cw->cpick.nearpixels-1-halfr)*NARROWNEAR >= MAXIMUM)
	  halfr++;
	if (g-halfg*NARROWNEAR < 0)
	  halfg--;
	if (g+((int) cw->cpick.nearpixels-1-halfg)*NARROWNEAR >= MAXIMUM)
	  halfg++;
	if (b-halfb*NARROWNEAR < 0)
	  halfb--;
	if (b+((int) cw->cpick.nearpixels-1-halfb)*NARROWNEAR >= MAXIMUM)
	  halfb++;
      }

      halfb = halfg = halfr = (halfr+halfg+halfb)/3;
      for (each=0; each<(int) cw->cpick.nearpixels; each++) {
	nr = r+(each-halfr)*NARROWNEAR;
	ng = g+(each-halfg)*NARROWNEAR;
	nb =  b+(each-halfb)*NARROWNEAR;
	if (nr < 0)
	  nr = 0;
	if (ng < 0)
	  ng = 0;
	if (nb < 0)
	  nb = 0;
	if (nr >= MAXIMUM)
	  nr = MAXIMUM-1;
	if (ng >= MAXIMUM)
	  ng = MAXIMUM-1;
	if (nb >= MAXIMUM)
	  nb = MAXIMUM-1;
	cw->cpick.nearcells[each].red = nr;
	cw->cpick.nearcells[each].green = ng;
	cw->cpick.nearcells[each].blue = nb;
      }
      XStoreColors(XtDisplay(cw), cw->cpick.cmap, cw->cpick.nearcells,
		   (int) cw->cpick.nearpixels);
    } else if (cw->cpick.wide == WIDE) {
      near = WIDENEAR;
      delta = (int) exp(log((double) cw->cpick.nearpixels)/3.0);
      halfr = halfg = halfb = delta/2;

      r = cw->cpick.allocated->red;
      g = cw->cpick.allocated->green;
      b = cw->cpick.allocated->blue;

      for (each=0; each<delta/2; each++) {
	if (r-halfr*near < 0)
	  halfr--;
	if (r+(delta-1-halfr)*near >= MAXIMUM)
	  halfr++;
	if (g-halfg*near < 0)
	  halfg--;
	if (g+(delta-1-halfg)*near >= MAXIMUM)
	  halfg++;
	if (b-halfb*near < 0)
	  halfb--;
	if (b+(delta-1-halfb)*near >= MAXIMUM)
	  halfb++;
      }

      nummods = 0;
      for (eachb=0; eachb<delta; eachb++) {
	for (eachg=0; eachg<delta; eachg++) {
	  for (eachr=0; eachr<(int) cw->cpick.nearpixels/(delta*delta); eachr++) {
	    mods[nummods][R] = eachr-halfr;
	    mods[nummods][G] = eachg-halfg;
	    mods[nummods][B] = eachb-halfb;
	    nummods++;
	  }
	}
      }
      for (each=0; each<nummods-1; each++) {
	for (each2=each+1; each2<nummods; each2++) {
	  if (mods[each][R]+mods[each][G]+mods[each][B] >
	      mods[each2][R]+mods[each2][G]+mods[each2][B]) {
	    eachr = mods[each][R];
	    eachg = mods[each][G];
	    eachb = mods[each][B];
	    mods[each][R] = mods[each2][R];
	    mods[each][G] = mods[each2][G];
	    mods[each][B] = mods[each2][B];
	    mods[each2][R] = eachr;
	    mods[each2][G] = eachg;
	    mods[each2][B] = eachb;
	  }
	}
      }

      for (each=0; each<nummods; each++) {
	cw->cpick.nearcells[each].red = r+mods[each][R]*near;
	cw->cpick.nearcells[each].green = g+mods[each][G]*near;
	cw->cpick.nearcells[each].blue = b+mods[each][B]*near;
      }
      XStoreColors(XtDisplay(cw), cw->cpick.cmap, cw->cpick.nearcells,
		   (int) cw->cpick.nearpixels);
    } else if (new) {
      hueinc = 1.0/cw->cpick.nearpixels;
      hsv.s = hsv.v = 1.0;
      hsv.h = 0.0;
      for (each=0; each<(int) cw->cpick.nearpixels; each++) {
	rgb = HSVToRGB(hsv);
	cw->cpick.nearcells[each].red = rgb.r;
	cw->cpick.nearcells[each].green = rgb.g;
	cw->cpick.nearcells[each].blue = rgb.b;
	hsv.h = hsv.h + hueinc;
      }
      XStoreColors(XtDisplay(cw), cw->cpick.cmap, cw->cpick.nearcells,
		   (int) cw->cpick.nearpixels);
    }
  }
}

static undomatch(cw)
CpickWidget cw;
{
  XmString xs;

  if (cw->cpick.matched) {
    cw->cpick.matched = FALSE;
    setMlabelStr(cw, NULLSTR);
  }
}

static matchPalette(cw)
CpickWidget cw;
{
  int each, each2, ct, r, g, b, nr, ng, nb, cr, cg, cb, d;
  FILE *fd;
  char name[MAXNAME], *sname;

  for (each=0; each<MAXPIXELS; each++) {
    strcpy(cw->cpick.mnames[each], NULLSTR);
    cw->cpick.mdist[each] = MBASE*MBASE*3;
  }
  cr = cw->cpick.allocated->red/RATIO;
  cg = cw->cpick.allocated->green/RATIO;
  cb = cw->cpick.allocated->blue/RATIO;
  fd = fopen(RGBFILE, "r");
  if (fd == NULL || feof(fd)) {
    undomatch(cw);
  } else {
    ct = 0;
    r = g = b = -1;
    while (!feof(fd)) {
      fscanf(fd, "%d %d %d", &nr, &ng, &nb);
      fgets(name, MAXNAME, fd);
      name[strlen(name)-1] = '\0';
      sname = name;
      while (sname[0] == ' ' || sname[0] == '\t') {
	sname++;
      }
      if (nr != r || ng != g || nb != b) {
	r = nr;
	g = ng;
	b = nb;
	d = (nr-cr)*(nr-cr)+(ng-cg)*(ng-cg)+(nb-cb)*(nb-cb);
	for (each = 0; each<ct; each++) {
	  if (d < cw->cpick.mdist[each]) {
	    for (each2 = cw->cpick.nearpixels-1; each2>= each; each2--) {
	      cw->cpick.nearcells[each2+1].red =
		cw->cpick.nearcells[each2].red;
	      cw->cpick.nearcells[each2+1].green =
		cw->cpick.nearcells[each2].green;
	      cw->cpick.nearcells[each2+1].blue =
		cw->cpick.nearcells[each2].blue;
	      cw->cpick.mdist[each2+1] = cw->cpick.mdist[each2];
	      strcpy(cw->cpick.mnames[each2+1], cw->cpick.mnames[each2]);
	    }
	    cw->cpick.nearcells[each].red = nr*RATIO;
	    cw->cpick.nearcells[each].green = ng*RATIO;
	    cw->cpick.nearcells[each].blue = nb*RATIO;
	    cw->cpick.mdist[each] = d;
	    strcpy(cw->cpick.mnames[each], sname);
	    if (ct < cw->cpick.nearpixels) {
	      ct++;
	    }
#ifdef BUBBLE_MATCH
	    XStoreColors(XtDisplay(cw), cw->cpick.cmap,
			 &(cw->cpick.nearcells[each]),
			 (int) (cw->cpick.nearpixels-each));
#endif
	    break;
	  }
	}
	if (ct < cw->cpick.nearpixels) {
	  for (each2 = cw->cpick.nearpixels-1; each2>= ct; each2--) {
	    cw->cpick.nearcells[each2+1].red =
	      cw->cpick.nearcells[each2].red;
	    cw->cpick.nearcells[each2+1].green =
	      cw->cpick.nearcells[each2].green;
	    cw->cpick.nearcells[each2+1].blue =
	      cw->cpick.nearcells[each2].blue;
	    cw->cpick.mdist[each2+1] = cw->cpick.mdist[each2];
	    strcpy(cw->cpick.mnames[each2+1], cw->cpick.mnames[each2]);
	  }
	  cw->cpick.nearcells[ct].red = nr*RATIO;
	  cw->cpick.nearcells[ct].green = ng*RATIO;
	  cw->cpick.nearcells[ct].blue = nb*RATIO;
	  cw->cpick.mdist[ct] = d;
	  strcpy(cw->cpick.mnames[ct], sname);
#ifdef BUBBLE_MATCH
	  XStoreColors(XtDisplay(cw), cw->cpick.cmap,
		       &(cw->cpick.nearcells[ct]),
		       (int) (cw->cpick.nearpixels-ct));
#endif
	  ct++;
	}	  
      }
    }
    if (ct != 0 &&
	cw->cpick.nearcells[0].red == cw->cpick.allocated->red &&
	cw->cpick.nearcells[0].green == cw->cpick.allocated->green &&
	cw->cpick.nearcells[0].blue == cw->cpick.allocated->blue) {
      setMlabelStr(cw, cw->cpick.mnames[0]);
    } else { 
      setMlabelStr(cw, NULLSTR);
    }
    fclose(fd);
#ifndef BUBBLE_MATCH
    XStoreColors(XtDisplay(cw), cw->cpick.cmap,
		 cw->cpick.nearcells,
		 (int) cw->cpick.nearpixels);
#endif
  }
}

static void setMlabelStr(cw, str)
CpickWidget cw;
char *str;
{
  XmString xs;

  xs = XmStringCreateSimple(str);
  count = 0;
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetValues(cw->cpick.mlabel, args, count);
  XmStringFree(xs);
}

static createPalette(cw)
CpickWidget cw;
{
  Pixel bg;

  count = 0;
  XtSetArg(args[count], XmNbackground, &bg);  count++;
  XtGetValues((Widget) cw, args, count);

  count = 0;
  XtSetArg(args[count], XmNbackground, bg);  count++;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNtopWidget, cw->cpick.boxButtons);  count++;
  XtSetArg(args[count], XmNtopOffset, INTER_SPACING); count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNleftWidget, cw->cpick.scaleSets);  count++;
  XtSetArg(args[count], XmNleftOffset, INTER_SPACING); count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightOffset, BORDER_MARGIN); count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNbottomWidget, cw->cpick.mframe);  count++;
  XtSetArg(args[count], XmNbottomOffset, INTER_SPACING);  count++;
  XtSetArg(args[count], XmNheight, 80);  count++;
  cw->cpick.nlevel = XmCreateDrawingArea(cw->cpick.tlevel, "nlevel",
					 args, count);
  XtManageChild(cw->cpick.nlevel);
  XtAddCallback(cw->cpick.nlevel, XmNexposeCallback,
		(XtCallbackProc) nlevel_expose, (XtPointer) cw);
  cw->cpick.wide = RANGE;
  cw->cpick.keep = FALSE;
  cw->cpick.matched = FALSE;

  count = 0;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNbottomWidget, cw->cpick.nlevel);  count++;
  XtSetArg(args[count], XmNbottomOffset, INTER_SPACING);  count++;
  XtSetValues(cw->cpick.bcontainer, args, count);
}

static void Initialize( request, new )
   Widget request;		/* what the client asked for */
   Widget new;			/* what we're going to give him */
{
  int each, depth;

  CpickWidget cw = (CpickWidget) new;

  if (!cw->cpick.cmap) {
    fprintf(stderr, "Cpick: Must set XmNcmap resource.\n");
    exit(1);
  } else if (cw->cpick.allocated == NULL) {
    fprintf(stderr, "Cpick: Must set XmNallocated resource.\n");
    exit(1);
  }

  count = 0;
  cw->cpick.tlevel = XmCreateForm(new, "form", args, count);
  XtManageChild(cw->cpick.tlevel);

  depth = XDisplayPlanes(XtDisplay(cw),
			 XDefaultScreen(XtDisplay(cw)));
  cw->cpick.inc = INCREMENT;
  if (!cw->cpick.inc)
    cw->cpick.inc = 1;

  createScales(cw);
  createBox(cw);
  createCommands(cw);
  createPalette(cw);

  initPalette(cw);

  doNew(cw);

  if (cw->core.width == 0) {
    cw->core.width = 10;
  }
  if (cw->core.height == 0) {
    cw->core.height = 10;
  }
}

/**********************************************************************/

Widget
CpickGetBoxFrame(Widget w)
{
  CpickWidget cw = (CpickWidget) w;

  return cw->cpick.bframe;
}
