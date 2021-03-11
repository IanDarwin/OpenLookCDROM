/* -*-C-*-
********************************************************************************
*
* File:         Graph.c
* RCS:          $Header: /users/npm/src/winterp/src-server/widgets/RCS/Graph.c,v 2.1 1994/06/06 15:48:07 npm Exp $
* Description:  Implementation file for a Motif Graph Widget
* Author:       Doug Young dyoung@wpd.sgi.com, Luis Miguel, (luis@postgres.berkeley.edu)
* Created:      
* Modified:     Sun May 29 22:50:53 1994 (Niels Mayer) npm@indeed
* Language:     C
*
* Copyright (c) 1988 by Hewlett-Packard Company
* 
* Permission to use, copy, modify, and distribute this software 
* and its documentation for any purpose and without fee is hereby 
* granted, provided that the above copyright notice appear in all 
* copies and that both that copyright notice and this permission 
* notice appear in supporting documentation, and that the name of 
* Hewlett-Packard not be used in advertising or publicity pertaining 
* to distribution of the software without specific, written prior 
* permission.
*
********************************************************************************
*/

/*
 * This is a Motif 1.1 version of the XmGraph widget. Ported to Motif 1.1
 * by Doug Young, ( dyoung@wpd.sgi.com)
 */

/* CYY modified 2/1/91 for  R3 and R4 compatible and ANSI_C fixes*/

/* TQP modified 12/16/91.  Merge CYY's version with CLL's C++ Developer
       group's version of the XmGraph.  This version has the ANSI_C
       compatibility of CYY's version, plus the graph layout and some bug
       fixes from CLL's C++ Developer Group. */

/* CYY modified 9/19/92 for R5 and SESD merge */
#define SESD 1

static char rcs_identity[] = "XmGraph Version 2.1 @(#)$Header: /users/npm/src/winterp/src-server/widgets/RCS/Graph.c,v 2.1 1994/06/06 15:48:07 npm Exp $";

#define CLL_ADDITION 0


#if SESD
/* On big graphs the increment is not useful.  Maybe someday someone should
 make a resource of this. */
#define SCROLLBAR_INCREMENT 20
#endif

#ifndef _HPUX_SOURCE
#define _HPUX_SOURCE
#endif

#include	<stdio.h>
#include 	<Xm/XmP.h>
#include 	"math.h"

#if ((defined (XtSpecificationRelease)) && (XtSpecificationRelease >= 5))
#define _R5_
#else
#undef _R5_
#endif

#if ((defined (XtSpecificationRelease)) && (XtSpecificationRelease >= 4))
#define _R4_
#else
#undef _R4_
#endif

#if (XmREVISION > 1)
#define RX XtX
#define RY XtY
#define RWidth XtWidth
#define RHeight XtHeight
#define RBorder XtBorderWidth
#include <Xm/GadgetP.h>
#endif

#include	<Xm/PushBG.h>
#include	<Xm/PushB.h>
#include        <Xm/BulletinBP.h>
#include        <Xm/ScrolledW.h>
#include	"Graph.h"
#include	"GraphP.h"
#include	"Arc.h"
#include	"ArcP.h"
#ifdef WINTERP
#include        <X11/cursorfont.h>
#else /* !defined(WINTERP) -- from original Interex XmGraph -- iworks.ecn.uiowa.edu:comp.hp/GUI_classic/XmGraph.R5.tar.Z */
#include        "X11/cursorfont.h"
#endif /* WINTERP */


#define XmBeingDestroyed(widget)  (((Object)(widget))->object.being_destroyed)
/*
 * Child and Parent cursors used by Graph
 */

#define c_width 16
#define c_height 16
#define c_x_hot 1
#define c_y_hot 14
static char c_bits[] = {
    0x00, 0x00, 0xc0, 0x07, 0x30, 0x18, 0x08, 0x20, 0x88, 0x23, 0xc4, 0x44,
    0xc4, 0x40, 0xc4, 0x40, 0xc4, 0x40, 0xc4, 0x44, 0x88, 0x23, 0x08, 0x20,
    0x3a, 0x18, 0xc6, 0x07, 0x0e, 0x00, 0x00, 0x00};

#define c_mask_width 16
#define c_mask_height 16
static char c_mask_bits[] = {
    0xe0, 0x0f, 0xf8, 0x3f, 0xfc, 0x7f, 0xfc, 0x7f, 0xfe, 0xff, 0xfe, 0xff,
    0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff, 0xff, 0x7f,
    0xff, 0x7f, 0xff, 0x3f, 0xff, 0x0f, 0x1f, 0x00};

#define p_width 16
#define p_height 16
#define p_x_hot 1
#define p_y_hot 14
static char p_bits[] = {
    0x00, 0x00, 0xc0, 0x07, 0x30, 0x18, 0x08, 0x20, 0xc8, 0x27, 0xc4, 0x4c,
    0xc4, 0x4c, 0xc4, 0x4c, 0xc4, 0x47, 0xc4, 0x40, 0xc8, 0x20, 0x08, 0x20,
    0x3a, 0x18, 0xc6, 0x07, 0x0e, 0x00, 0x00, 0x00};

#define p_mask_width 16
#define p_mask_height 16
static char p_mask_bits[] = {
    0xe0, 0x0f, 0xf8, 0x3f, 0xfc, 0x7f, 0xfc, 0x7f, 0xfe, 0xff, 0xfe, 0xff,
    0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff, 0xff, 0x7f,
    0xff, 0x7f, 0xff, 0x3f, 0xff, 0x0f, 0x1f, 0x00};


#define PtrCursor(m)		(m -> graph.ptr_cursor)
#define MotionCursor(m)	        (m -> graph.motion_cursor)
#define IndicateCursor(m)	(m -> graph.indicate_cursor)
#define HighRightCursor(m)	(m -> graph.high_right_cursor)
#define HighLeftCursor(m)	(m -> graph.high_left_cursor)
#define LowRightCursor(m)	(m -> graph.low_right_cursor)
#define LowLeftCursor(m)	(m -> graph.low_left_cursor)
#define IndicateChildCursor(m)	(m -> graph.indicate_child_cursor)
#define IndicateParentCursor(m)	(m -> graph.indicate_parent_cursor)

#define GRAPH_STRING_MESSAGE "Invalid XmNdefaultLabel - must be a compound string "
#define DEFAULT_LABEL        "Node"

static int      resource_0_int      = 0;

/*
 *  Dimensions of default node created by AddNode
 */

Dimension     nn_width, nn_height;

/*
 *  To have graph away from edge of window
 */

#define VERT_LEFT_DIST  40
#define HOR_TOP_DIST    40

/*
 *  Array of lines to be used to send XDrawLine requests in "batch"
 */

/* CYY comment:
 *  using globals like this would make it not re-entrant !
 */
#define MAX_LINES 200

struct  {
    int       n;
    XSegment  lines [MAX_LINES];
} line_list;

/*
 * Array of rectangles to be used to send XDrawRectangle requests in "batch"
 */

#define MAX_RECTANGLES 200

struct  {
    int         n;
    XRectangle  rect [MAX_RECTANGLES];
} rectangle_list;

/*
 * List of Nodes to be used by Subgraph manipulation functions.
 *  Two purposes:
 *    1) efficiency (we only traverse subgraph once),
 *    2) Avoid visiting nodes
 *  more than once.
 */

static NodeList  subgraph_nodes;

/* 910318 mohammad:  Set up the widgetLocation structure to temporarily store 
 * the nodes in the graph while doing the layout.  If none of the nodes falls
 * outside the virtual graph space, then display them.  Otherwise, change
 * the sibling and/or child spacings and try to re-layout the graph.
 * If we get to a spacing, either child or sibling, of 1 then just display
 * what we can. 
 */
struct widgetLocation{
  Widget w;
  int    x;
  int    y;
  int    bottom;
  int    right;
  Boolean off_of_graph;
};

static struct widgetLocation *p_widgetLocationArray = NULL;
static struct widgetLocation *p_currentPositionInLocationArray = NULL;

static Boolean BAD_X_RESOLUTION = FALSE;
static Boolean BAD_Y_RESOLUTION = FALSE;

/* 910318 mohammad:  The maximum value that can be assigned to the Position
 * datatype in the Graph.  It is slightly less than the maximum value that can
 * be can assigned to Position because we do not want to get too close to the
 * edge of the graph because of problems that can be encountered when additional
 * space is allocated by the graph.  Otherwise, the value could be 32767.
 * 910324 coha:  Try to reduce the problems that can occur at edges by reducing
 * the maximum size of the graph that we will actually construct.  There is
 * a problem with both the cursor grab and the scrollbars when the graph is
 * near the limit of Position.
 */
#define MAX_VALUE_FOR_POSITION 32000
#define HALF_MAX_VALUE_FOR_POSITION 16000

#define MIN_VALUE_FOR_CHILD_OR_SIBLING_SPACING 1

/* END of changes from 910318 */


#ifndef _R4_
#define XtPointer caddr_t
#endif

/* 
 * Procedure declarations used before definition
 */
#ifndef _NO_PROTO

static void updateNodeStruct(Widget w);
static Boolean nodesConnected(NodePtr, NodePtr);

static Boolean 	NodeListMember (NodeList *, NodePtr );
static void 	_RefreshArcs (XmGraphWidget, Region);
static void 	AdjustSize    (XmGraphWidget);
static void 	ArcListInsert (ArcList *, XmArcWidget);
static void  	CancelAdd(XmGraphWidget, XEvent *);
static void 	CancelArcsWithSubgraph (XmGraphWidget, NodePtr);
static Cursor 	CreateCursor(Widget, String, String,
		  		 int, int, int, int);
static Widget  	DefaultNodeWidget(Widget, int, int);
static XtGeometryResult GeometryManager(Widget,
					XtWidgetGeometry*,
					XtWidgetGeometry*);
static void	MakeEditable(Widget);
static void	MakeReadOnly(Widget);
static void 	MoveMultiple (XmGraphWidget , XEvent *);
static XtGeometryResult QueryGeometry (Widget,
					XtWidgetGeometry*,
					XtWidgetGeometry*);
static void 	ResizeClipArea (Widget, XtPointer, XtPointer);
static void 	SelectArcsWithSubgraph (Widget, NodePtr);
static void 	SelectMultipleForMotion (XmGraphWidget , XEvent *);
static void	EndMultipleMotion(XmGraphWidget , XEvent *);
static void GetSubgraphNodeList (NodePtr );
static void DoRemoveArc 	(XmGraphWidget , XmArcWidget );
static void ArcListFree 	(ArcList *);
static void ArcListRemove 	(ArcList *, XmArcWidget );
static void MoveArcsWithMultipleNodes (XmGraphWidget, NodePtr);
static void AddRectToList (XmGraphWidget , 
			   Position , Position ,
			   Dimension , Dimension );
static void DrawRectList 	(XmGraphWidget );
static void AddLineToList 	(XmGraphWidget ,
			   	  int , int , int , int );
static void DrawLineList 	(XmGraphWidget );
static void NodeListInsertNoDuplicates (NodeList *, NodePtr);
static void NodeListInsert 	(NodeList *, NodePtr );
static void NodeListFree 	(NodeList *);
static void InitNode 		(NodePtr );
static void  IndicateRegion	(XmGraphWidget, XEvent *);
static void  SweepOutRegion	(XmGraphWidget, XEvent *);
static void  SelectRegion	(XmGraphWidget, XEvent *);
static void UpdateArcs		(Widget , Boolean );
static void RemoveFromSiblings  (XmArcWidget );
static void _RefreshGadgets	(XmGraphWidget ,Region );
static void FreeNode 		(NodePtr );
static XmArcWidget CreateArcInternal (Widget , String, 
				      NodePtr , NodePtr ,
				      ArgList , 
				      Cardinal );
static void Initialize (Widget , Widget );
static void ClassInitialize (void);
static void Destroy 		(XmGraphWidget);
static void Resize 		(XmGraphWidget);
static Boolean SetValues	(Widget , Widget , Widget);
static void ChangeManaged 	(XmGraphWidget );
static void DoLayout 		(XmGraphWidget );
static void Redisplay 		(XmGraphWidget , XEvent *, Region );
static void InsertChild 	(Widget );
static void SelectArcsWithNode 	(XmGraphWidget, NodePtr);
static void MoveArcsWithNode 	(XmGraphWidget , NodePtr);
static void Select		(XmGraphWidget , XEvent *);
static void NodeListRemove 	(NodeList *, NodePtr);
static void DeleteChild		(Widget );
static void ConstraintInitialize(Widget , Widget );
static void ConstraintDestroy	(Widget ) ;
static Region VisibleGraphRegion(XmGraphWidget );

static void HandleButtonPress 	(Widget , XEvent *);
static void HandleButtonMotion 	(Widget , XEvent *);
static void HandleButtonRelease (Widget , XEvent *);
static void HandleCreateNotify  (Widget , XtPointer , XEvent *, Boolean *);
static void XmDispatchArcInput   (Widget , XEvent *,Mask);
static Boolean ArcVisibleInGraph (XmGraphWidget, XmArcWidget);
static void _XmFromSiblingPixels (Widget , int , XtArgVal *);
static void _XmFromChildPixels   (Widget , int , XtArgVal *);
#ifdef _R4_
XmImportOperator _XmToChildPixels (Widget, int, XtArgVal *);
XmImportOperator _XmToSiblingPixels (Widget, int, XtArgVal *);
#endif

/*****************************************************************************
 * NEW STUFF
 ******************************************************************************/


static void	_XmCvtStringToArcDirection (XrmValuePtr, Cardinal *,
					  XrmValue *, XrmValue *);
static void	_XmCvtStringToArcDrawMode (XrmValuePtr, Cardinal *,
					  XrmValue *, XrmValue *);
static void	_XmCvtStringToLineStyle (XrmValuePtr, Cardinal *,
					  XrmValue *, XrmValue *);
static void	_XmCvtStringToCapStyle (XrmValuePtr, Cardinal *,
					  XrmValue *, XrmValue *);
static void	_XmCvtStringToAutoLayoutType (XrmValuePtr, Cardinal *,
					  XrmValue *, XrmValue *);
static void	AddIndicate(XmGraphWidget, XEvent *);
static void	AddSelect(XmGraphWidget, XEvent *);
static void	Cancel(XmGraphWidget, XEvent *);
static void	CancelSubtree(XmGraphWidget, XEvent *);
static void	EndAddNode(XmGraphWidget, XEvent *);
static void	EndButtonAction(XmGraphWidget, XEvent *);
static void	EndMotion(XmGraphWidget, XEvent *);
static void	HandleDoubleClick(XmGraphWidget, XEvent *);
static void	HandleMotion(XmGraphWidget, XEvent *);
static void	Indicate(Widget, XEvent *);
static void	IndicateSubtree(Widget, XEvent *);
static Boolean	IsDoubleClick(XmGraphWidget, XEvent *);
static void	Motion(XmGraphWidget, XEvent *);
static void	PositionNewNode(XmGraphWidget, XEvent *);
static void	SelectForMotion(Widget, XEvent *);
static void	SelectSubtree(XmGraphWidget, XEvent *);
static void	StartAddNode(XmGraphWidget, XEvent *);
static void	toAllLowerCase(String);





#else

static void updateNodeStruct();
static Boolean nodesConnected();
static Boolean 	NodeListMember ();



static void  	_RefreshArcs();
static void 	_RefreshGadgets();
static void 	_XmFromChildPixels();
static void 	_XmFromSiblingPixels();
static void 	AddLineToList();
static void 	AddRectToList( );
static void	AdjustSize();
static void 	ArcListFree();
static void 	ArcListInsert();
static void 	ArcListRemove();
static Boolean 	ArcVisibleInGraph();
static void 	CancelAdd();
static void	CancelArcsWithSubgraph();
static void 	ChangeManaged();
static void 	ClassInitialize();
static void 	ConstraintInitialize();
static void 	ConstraintDestroy() ;
static XmArcWidget CreateArcInternal();
static Cursor 	CreateCursor();
static Widget	DefaultNodeWidget();
static void 	DeleteChild();
static void 	Destroy();
static void	DoLayout();
static void	DoRemoveArc();
static void	DrawRectList();
static void	DrawLineList();
static void 	EndMultipleMotion();
static void	FreeNode();
static XtGeometryResult GeometryManager();
static void	GetSubgraphNodeList();
static void	HandleButtonPress();
static void	HandleButtonMotion();
static void	HandleButtonRelease();
static void	HandleCreateNotify();
static void	InitNode();
static void	Initialize();
static void 	IndicateRegion();
static void 	InsertChild();
static void     MakeEditable();
static void 	MakeReadOnly();
static void 	MoveArcsWithMultipleNodes();
static void 	MoveArcsWithNode();
static void 	MoveMultiple();
static void 	NodeListInsertNoDuplicates();
static void 	NodeListInsert();
static void 	NodeListFree();
static void 	NodeListRemove();
static XtGeometryResult QueryGeometry();
static void 	Redisplay();
static void 	RemoveFromSiblings();
static void	Resize();
static void	ResizeClipArea();
static void	Select();
static void	SelectArcsWithNode();
static void 	SelectArcsWithSubgraph();
static void 	SelectMultipleForMotion();
static void	SelectRegion();
static Boolean	SetValues();
static void	SweepOutRegion();
static void	UpdateArcs();
static Region	VisibleGraphRegion();
static void	XmDispatchArcInput();

#ifdef _R4_
XmImportOperator _XmToChildPixels ();
XmImportOperator _XmToSiblingPixels ();
#endif
/*****************************************************************************
 * NEW STUFF
 *****************************************************************************/

static void	_XmCvtStringToArcDirection ();
static void	_XmCvtStringToArcDrawMode ();
static void	_XmCvtStringToLineStyle ();
static void	_XmCvtStringToCapStyle ();
static void	_XmCvtStringToAutoLayoutType ();
static void	AddIndicate();
static void	AddSelect();
static void	Cancel();
static void	CancelSubtree();
static void	EndAddNode();
static void	EndButtonAction();
static void	EndMotion();
static void	HandleDoubleClick();
static void	HandleMotion();
static void	Indicate();
static void	IndicateSubtree();
static Boolean	IsDoubleClick();
static void	Motion();
static void	PositionNewNode();
static void	SelectForMotion();
static void	SelectSubtree();
static void	StartAddNode();
static void	toAllLowerCase();



#endif







#define PointInRect(a, b, rect) ((a) <= ((rect)->x + (rect)->width) && (a) >= (rect)->x && (b) <= ((rect)->y + (rect)->height) && (b) >= (rect)->y)

#define ZERO_RANKING(a)  ((a)->arc.siblings == NULL ||  \
			  (graph->graph.siblings_visible != TRUE) || \
			  ((a)->arc.siblings->arcs[0]) == (XmArcWidget)((a)->core.self))

static ArcList return_widget_list;


/*************************************<->*************************************
 *
 *   Default translations: Used by the widget to bind events occurring in it
 *   -----------	   to predefined actions. The application can redefine
 *			   which events get bound, but the list of actions to
 * 			   handle these events is hard-coded.
 *************************************<->***********************************/


static XtAccelerators defaultAcceleratorsParsed;

#if SESD
#ifdef WINTERP
/*
 * NPM: replaced all "None" descriptions in translations below with less
 * restrictive version. On many systems, "None" is equivalent to
 * ~Shift~Ctrl~Lock~Mod1~Mod2~Mod3~Mod4~Mod5~Button1~Button2~Button3~Button4~Button5
 * which causes those translations not to work when "caps lock" "num lock"
 * "scroll lock" are set. By not specifying ~Lock~Mod2~Mod3~Mod4~Mod5, one prevents 
 * overly restrictive translation specs, which will allow the bindings to work
 * even when caps-lock and other modifiers are set.
 */
static char defaultTranslations[] =
    "<EnterWindow>:	 Enter() \n\
   <FocusIn>:	         FocusIn() \n\
   <FocusOut>:	         FocusOut() \n\
   <Key>F1:	         Help()\n\
   ~Shift~Ctrl~Mod1~Button2~Button3~Button4~Button5<Btn1Down>:	Indicate() Arm()\n\
   ~Shift Ctrl~Mod1~Button2~Button3~Button4~Button5<Btn1Down>:	AddIndicate()\n\
    Shift~Ctrl~Mod1~Button2~Button3~Button4~Button5<Btn1Down>:	IndicateSubtree()\n\
   ~Shift Ctrl~Mod1~Button1~Button3~Button4~Button5<Btn2Down>:	StartAddNode()\n\
   ~Shift~Ctrl~Mod1~Button1~Button3~Button4~Button5<Btn2Down>:  SelectForMotion()\n\
   <Btn1Motion>:         HandleMotion() EnterLeaveMotion()\n\
   <Btn2Motion>:         HandleMotion() \n\
   ~Shift~Ctrl~Mod1~Button2~Button3~Button4~Button5<Btn1Up>:	EndButtonAction() Activate()\n\
    Shift~Ctrl~Mod1~Button2~Button3~Button4~Button5<Btn1Up>:	EndButtonAction()\n\
   ~Shift Ctrl~Mod1~Button2~Button3~Button4~Button5<Btn1Up>:	EndButtonAction()\n\
   ~Shift~Ctrl~Mod1~Button1~Button3~Button4~Button5<Btn2Up>:	EndButtonAction()\n\
    Shift~Ctrl~Mod1~Button1~Button3~Button4~Button5<Btn2Up>:	EndButtonAction()\n\
   ~Shift Ctrl~Mod1~Button1~Button3~Button4~Button5<Btn2Up>:	EndButtonAction()";
#else /* !defined(WINTERP) -- from original Interex XmGraph -- iworks.ecn.uiowa.edu:comp.hp/GUI_classic/XmGraph.R5.tar.Z */
static char defaultTranslations[] =
    "<EnterWindow>:	 Enter() \n\
   <FocusIn>:	         FocusIn() \n\
   <FocusOut>:	         FocusOut() \n\
   <Key>F1:	         Help()\n\
   None<Btn1Down>:       Indicate() Arm()\n\
   Ctrl<Btn1Down>:       AddIndicate()\n\
   Shift<Btn1Down>:      IndicateSubtree()\n\
   Ctrl<Btn2Down>:       StartAddNode()\n\
   None<Btn2Down>:       SelectForMotion()\n\
   <Btn1Motion>:         HandleMotion() EnterLeaveMotion()\n\
   <Btn2Motion>:         HandleMotion() \n\
   None<Btn1Up>:         EndButtonAction() Activate()\n\
   Shift<Btn1Up>:        EndButtonAction()\n\
   Ctrl<Btn1Up>:         EndButtonAction()\n\
   None<Btn2Up>:         EndButtonAction()\n\
   Shift<Btn2Up>:        EndButtonAction()\n\
   Ctrl<Btn2Up>:         EndButtonAction()";
#endif /* WINTERP */
#else
static char defaultTranslations[] =
    "<EnterWindow>:	 Enter() \n\
   <FocusIn>:	         FocusIn() \n\
   <FocusOut>:	         FocusOut() \n\
   <Key>F1:	         Help()\n\
   None<Btn1Down>:       Indicate() Arm()\n\
   Shift<Btn1Down>:      AddIndicate()\n\
   Ctrl<Btn1Down>:       IndicateSubtree()\n\
   Ctrl<Btn2Down>:       StartAddNode()\n\
   None<Btn2Down>:       SelectForMotion()\n\
   <Btn1Motion>:         HandleMotion() EnterLeaveMotion()\n\
   <Btn2Motion>:         HandleMotion() \n\
   None<Btn1Up>:         EndButtonAction() Activate()\n\
   Shift<Btn1Up>:        EndButtonAction()\n\
   Ctrl<Btn1Up>:         EndButtonAction()\n\
   None<Btn2Up>:         EndButtonAction()\n\
   Shift<Btn2Up>:        EndButtonAction()\n\
   Ctrl<Btn2Up>:         EndButtonAction()";
#endif


static char defaultAccelerators[] =
    "\043override\n\
	<Key>F1:	Help()";


/*************************************<->*************************************
 *
 *   Default actions: Matches the actions mentioned in the default Translations
 *   -----------      list to actual procedures within the widget.
 *
 *************************************<->***********************************/


static XtActionsRec actionsList[] = {
{ "Enter",	           (XtActionProc) _XmManagerEnter },
{ "FocusIn",	           (XtActionProc) _XmManagerFocusIn },
{ "FocusOut",	           (XtActionProc) _XmManagerFocusOut },
{ "Arm",	           (XtActionProc) HandleButtonPress  },
{ "Activate",	           (XtActionProc) HandleButtonRelease },
#ifdef _R4_
{ "Help",	           (XtActionProc) _XmManagerHelp },
#else
{ "Help",                (XtActionProc) _XmBulletinBoardHelp },
#endif
{ "EnterLeaveMotion",	   (XtActionProc) HandleButtonMotion },
{ "Indicate",            (XtActionProc) Indicate       },
{ "AddIndicate",         (XtActionProc) AddIndicate        },
{ "IndicateSubtree",     (XtActionProc) IndicateSubtree   },
{ "SelectForMotion",     (XtActionProc) SelectForMotion    },
{ "StartAddNode",        (XtActionProc) StartAddNode    },
{ "HandleMotion",        (XtActionProc) HandleMotion   },
{ "EndButtonAction",     (XtActionProc) EndButtonAction   },
};


/*************************************<->*************************************
 *
 *   Resources: Provides default settings for the new resources, defined
 *   ---------  in GraphP.h, which can set by applications.
 *
 *************************************<->**************************************
 */

static XtResource resources[] = {

{XmNptrCursor, XmCCursor, XmRCursor, sizeof(Cursor),
     XtOffset(XmGraphWidget, graph.ptr_cursor), XmRString, (caddr_t) "left_ptr"},

{XmNmotionCursor, XmCCursor, XmRCursor, sizeof(Cursor),
     XtOffset(XmGraphWidget, graph.motion_cursor), XmRString, (caddr_t) "fleur"},

{XmNindicateCursor, XmCCursor, XmRCursor, sizeof(Cursor),
     XtOffset(XmGraphWidget, graph.indicate_cursor), XmRString, (caddr_t) "crosshair"},

{XmNindicateChildCursor, XmCCursor, XmRCursor, sizeof(Cursor),
     XtOffset(XmGraphWidget, graph.indicate_child_cursor), XmRImmediate, (caddr_t) None},

{XmNindicateParentCursor, XmCCursor, XmRCursor, sizeof(Cursor),
     XtOffset(XmGraphWidget, graph.indicate_parent_cursor), XmRImmediate, (caddr_t) None},

{ XmNdoubleClickInterval, XmCDoubleClickInterval, XmRInt, sizeof(int),
      XtOffset(XmGraphWidget, graph.double_click_interval), XmRImmediate,  (caddr_t) 250  },

{ XmNallowMultipleSelections, XmCAllowMultipleSelections, XmRBoolean, sizeof(Boolean),
      XtOffset(XmGraphWidget, graph.allow_multiple_selections), XmRString,  "True"  },

{XmNeditable, XmCEditable, XmRBoolean, sizeof (Boolean),
     XtOffset(XmGraphWidget, graph.edit_mode), XmRString, "False" },
 
{XmNshowCrossingArcs, XmCShowCrossingArcs, XmRBoolean, sizeof (Boolean),
     XtOffset(XmGraphWidget, graph.show_crossing_arcs), XmRString, "True" },
 
{XmNmovableNodes, XmCMovableNodes, XmRBoolean, sizeof (Boolean),
     XtOffset(XmGraphWidget, graph.nodes_movable), XmRString, "TRUE" },
 
{XmNautoLayoutMode, XmCAutoLayoutMode, XmRAutoLayoutType, sizeof (autoLayoutType),
     XtOffset(XmGraphWidget, graph.auto_layout_mode), XmRString, "never"},
 
{XmNreLayout, XmCReLayout, XmRBoolean, sizeof (Boolean),
     XtOffset(XmGraphWidget, graph.re_layout), XmRString, "False" },
 
{XmNreorient, XmCReorient, XmRBoolean, sizeof (Boolean),
     XtOffset(XmGraphWidget, graph.re_orient), XmRString, "False" },
 
{XmNtwinsVisible, XmCTwinsVisible, XmRBoolean, sizeof (Boolean),
     XtOffset(XmGraphWidget, graph.siblings_visible), XmRString, "True" },
 
{ XmNorientation, XmCOrientation, XmROrientation, sizeof(unsigned char),
      XtOffset(XmGraphWidget, graph.direction),  XmRImmediate, (caddr_t) XmHORIZONTAL},
 
{XmNarcDrawMode,  XmCArcDrawMode, XmRArcDrawMode, sizeof(unsigned char),
     XtOffset(XmGraphWidget, graph.arc_draw_mode), XmRString, "position_relative"},

{XmNchildSpacing, XmCChildSpacing, XmRInt, sizeof(int),
     XtOffset(XmGraphWidget, graph.child_spacing), XmRString, "40"},
 
{XmNsiblingSpacing, XmCSiblingSpacing, XmRInt, sizeof(int),
     XtOffset(XmGraphWidget, graph.sibling_spacing), XmRString, "30"},
 
{XmNnewArcCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.new_arc), XmRPointer, (caddr_t) NULL},
 
{XmNnewNodeCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.new_node), XmRPointer, (caddr_t) NULL},
 
{XmNnodeMovedCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.node_moved), XmRPointer, (caddr_t) NULL},
 
{XmNarcMovedCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.arc_moved), XmRPointer, (caddr_t) NULL},
 
{XmNlayoutProc, XmCLayoutProc, XmRPointer, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.layout_proc), XmRImmediate, (caddr_t) DoLayout},
 
{XmNdefaultActionCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.double_click), XmRPointer, (caddr_t) NULL},
 
{XmNselectNodeCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.select_node), XmRPointer, (caddr_t) NULL},
 
{XmNdeselectCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.deselect), XmRPointer, (caddr_t) NULL},
 
{XmNselectArcCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.select_arc), XmRPointer, (caddr_t) NULL},
 
{XmNselectSubgraphCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.select_subgraph), XmRPointer, (caddr_t) NULL},
 
{XmNdefaultNodeClass, XmCDefaultNodeClass, XmRInt, sizeof (int),
     XtOffset(XmGraphWidget, graph.default_widget_class), XmRInt, (caddr_t) &resource_0_int},
 
{XmNinitializeDataCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.constraint_init), XmRPointer, (caddr_t) NULL},
 
{XmNfreeDataCallback, XmCCallback, XmRCallback, sizeof(caddr_t),
     XtOffset(XmGraphWidget, graph.constraint_free), XmRPointer, (caddr_t) NULL},
 
{XmNsnapGridOn, XmCSnapGridOn, XmRBoolean, sizeof (Boolean),
     XtOffset(XmGraphWidget, graph.snap_on), XmRImmediate, (caddr_t) FALSE },
 
{XmNsnapGridSize, XmCSnapGridSize, XtRCardinal, sizeof (Cardinal),
     XtOffset(XmGraphWidget, graph.snap_grid_size), XmRImmediate, (caddr_t) 10 },
 
{XmNinteractiveArcDirection, XmCInteractiveArcDirection, XmRArcDirection, sizeof (unsigned char),
     XtOffset(XmGraphWidget, graph.arc_style), XmRImmediate, (caddr_t) XmUNDIRECTED },
 
};

/*************************************<->*************************************
 *
 *
 * 
 *
 *************************************<->***********************************/

#ifdef _R4_
static XmSyntheticResource get_resources[] = {
{ XmNchildSpacing, 
      sizeof (int),
      XtOffset (XmGraphWidget, graph.child_spacing),
      _XmFromChildPixels,
      (XmImportProc) _XmToChildPixels },

{ XmNsiblingSpacing,
      sizeof (int),
      XtOffset (XmGraphWidget, graph.sibling_spacing),
      _XmFromSiblingPixels,
      (XmImportProc) _XmToSiblingPixels}
};
#else
static XmGetValueResource get_resources[] = {
{ XmNchildSpacing,
      sizeof (short),
      XtOffset (XmGraphWidget, graph.child_spacing),
      (XmGetValuesProc) _XmFromChildPixels },
 
{ XmNsiblingSpacing,
      sizeof (short),
      XtOffset (XmGraphWidget, graph.sibling_spacing),
      (XmGetValuesProc) _XmFromSiblingPixels },
};
#endif


/*************************************<->*************************************
 *
 *   The Class Record
 *   -----------
 *
 *************************************<->***********************************/

externaldef(xmgraphclassrec) XmGraphClassRec xmGraphClassRec = {
{
    /* core_class fields      */
    (WidgetClass) &xmManagerClassRec,  /* superclass        */
    "XmGraph",		                /* class_name         */
    sizeof(XmGraphRec),                 /* widget_size        */ 
    (XtProc) ClassInitialize,           /* class_initialize   */ 
    NULL,                               /* class_part_init    */
    FALSE,    				/* class_inited       */
    (XtInitProc) Initialize,		/* initialize         */   
    NULL,    				/* initialize_hook    */
    XtInheritRealize, 			/* realize            */   
    actionsList,    			/* actions            */   
    XtNumber(actionsList),    		/* num_actions	      */
    resources,    			/* resources          */   
    XtNumber(resources),		/* num_resources      */   
    NULLQUARK,    			/* xrm_class          */   
    TRUE,				/* compress_motion    */
#ifdef _R4_
    XtExposeCompressMaximal,		/* compress_exposure  */
#else
    TRUE,				/* compress_exposure  */
#endif
    TRUE,    				/* compress_enterleave*/
    TRUE,    				/* visible_interest   */
    (XtWidgetProc) Destroy,    		/* destroy            */ 
    (XtWidgetProc) Resize,		/* resize             */
    (XtExposeProc) Redisplay,           /* expose             */
    (XtSetValuesFunc) SetValues,	/* set_values         */
    NULL,    				/* set_values_hook    */
    (XtAlmostProc) XtInheritSetValuesAlmost,/* set_values_almost  */ 
    NULL,    				/* get_values_hook    */
    NULL,    				/* accept_focus       */
    XtVersion,    			/* version            */
    NULL,    				/* callback_private   */
    defaultTranslations,    		/* tm_table           */
    (XtGeometryHandler)QueryGeometry,	/* query_geometry     */
    NULL,	                        /* disp accelerator   */
    NULL,                        	/* extension          */
},
{
    /* composite_class fields */
    (XtGeometryHandler)GeometryManager, /* geometry_manager   */
    (XtWidgetProc) ChangeManaged,    	 /* change_managed     */
    (XtWidgetProc) InsertChild,	         /* insert_child       */
    (XtWidgetProc) DeleteChild,          /* delete_child       */
    NULL,				 /* extension          */
},
{ 
    /* constraint_class fields */
    NULL,                                 /* subresourses       */   
    0,                                    /* subresource_count  */
    sizeof(XmGraphConstraintsRec),        /* constraint_size    */ 
    (XtInitProc) ConstraintInitialize,  /* initialize         */   
    (XtWidgetProc) ConstraintDestroy,     /* destroy            */   
    NULL,				  /* set_values         */ 
    NULL,                                 /* extension          */
},
{   /* manager_class fields   */
#ifdef _R4_
    XtInheritTranslations,        /* translations      	  */
#else
    (XtTranslations) _XtInherit,        /* translations      	  */
#endif
    get_resources,			/* get resources      	  */
    XtNumber(get_resources),   		/* num get_resources 	  */
    NULL,				/* get_cont_resources     */
    0,					/* num_get_cont_resources */
#ifdef _R4_
    XmInheritParentProcess,               /* parent_process         */
#endif
    NULL, 				/* extension              */
}, 
{
    /* GRAPH class fields */
    NULL,    				 /* empty	       */
}
};

externaldef(xmgraphwidgetclass) 
WidgetClass xmGraphWidgetClass = (WidgetClass) &xmGraphClassRec;


/*************************************<->*************************************
 *
 *   GeometryManager. Just allow all resize requests from nodes. Must sometimes
 *   -----------      move arcs to compensate for node motion or size changes.
 *
 *************************************<->***********************************/

static XtGeometryResult GeometryManager
#ifndef _NO_PROTO
  (Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
#else
  (w, request, reply)
	Widget	         w;
	XtWidgetGeometry	*request;
	XtWidgetGeometry	*reply;
#endif
{
    Widget graphW = XtParent(w);
    XmGraphWidget graph = (XmGraphWidget) graphW;
    if(((request->request_mode) & XtCWQueryOnly) == NULL) /* CYY  */
    { if (request->request_mode)
      {
	NodePtr    node = NODEPTR(w);
	Position   x, y;
	Dimension  width, height, border_width;


	if (node && XtIsRealized(graph))
	{
	    XmArcWidgetList  arcs;
	    Widget arcW;
	    XmArcWidget arc;
#if SESD
	    Region      region = VisibleGraphRegion(graph);
#else
	    Region      region = XCreateRegion();
#endif
	    int         n_arcs, i;

	    _InitArcList(graph);  /* clear line list */
	    graph->graph.batch_drawing_mode = TRUE;

	    /* Erase arcs from old position */
	    arcs = node->from_arcs.arcs;
	    n_arcs = node->from_arcs.n_arcs;
	    for (i = 0; i < n_arcs; i++) 
#if SESD
		_EraseArc (arcs[i]);
#else
	    {
		arc =  arcs[i];

		if (arc->arc.region && arc->arc.visible) 
		    region = _AddRegionToRegion (region, arc->arc.region);

		_EraseArc (arc);
	    }
#endif

	    arcs = node->to_arcs.arcs; n_arcs = node->to_arcs.n_arcs;
	    for (i = 0; i < n_arcs; i++) 
#if SESD
		_EraseArc (arcs[i]);
#else
	    { 
		arc =  arcs[i];

		if (arc->arc.region && arc->arc.visible) 
		    region = _AddRegionToRegion(region, arc->arc.region);
		_EraseArc (arc);
	    }
#endif
	    _InitArcList(graph);  /* clear line list */
	    graph->graph.batch_drawing_mode = FALSE;

	    x      = RX(w);
	    y      = RY(w);
	    width  = RWidth(w);
	    height = RHeight(w);
	    border_width = RBorder(w);

	    if (request->request_mode & CWX)
		x = (Position) request->x;
	    if (request->request_mode & CWY)
		y = (Position) request->y;
	    if (request->request_mode & CWWidth)
		width = (Dimension) request->width;
	    if (request->request_mode & CWHeight)
		height = (Dimension) request->height;
	    if (request->request_mode & CWBorderWidth)
		border_width = (Dimension) request->border_width;

#if (XmREVISION==1)
	    _XmConfigureObject ((RectObj)w, x, y, width, height, border_width);
#else
	    _XmConfigureObject (w, x, y, width, height, border_width);
#endif

	    UpdateArcs(w, FALSE);

	    _InitArcList(graph);  /* clear line list */
	    graph->graph.batch_drawing_mode = TRUE;

	    arcs = node->from_arcs.arcs; n_arcs = node->from_arcs.n_arcs;

	    { 
	      for (i = 0; i < n_arcs; i++) 
	      { arc = arcs[i];
		arcW = (Widget) arc;
		arcW->core.visible = ArcVisibleInGraph(graph, arc);
#if SESD
		if (graph->core.visible) 
#endif
		(* arc->core.widget_class->core_class.expose) (arcW, NULL, NULL);
	      }

	      arcs = node->to_arcs.arcs; n_arcs = node->to_arcs.n_arcs;
	      for (i = 0; i < n_arcs; i++) 
	      { arc = arcs[i];
		arcW =(Widget) arc;
		arcW->core.visible = ArcVisibleInGraph(graph, arc);
#if SESD
		if (graph->core.visible) 
#endif
		(* arcW->core.widget_class->core_class.expose) (arcW, NULL, NULL);
	      }
	    }

	    /* 
	     *  Refresh arcs and nodes underneath 
	     */
	    _RefreshGadgets(graph, region);

	    _InitArcList(graph);  /* clear line list */
	    graph->graph.batch_drawing_mode = FALSE;

	    _RefreshArcs (graph, region);

#if SESD
	    if (graph->core.visible)
	      AdjustSize(graph);
	    else
	      graph->graph.deferedAdjustSize = TRUE; 
#else
	    AdjustSize(graph); 
#endif
	    XDestroyRegion(region);
	    return (XtGeometryDone);
	}
      }
    }
    return (XtGeometryYes);
}

/*************************************<->*************************************
 *
 *   ConstraintInitialize. Allocate a node struct for all non-arcs and zero it.
 *   -----------
 *
 *************************************<->***********************************/

static void ConstraintInitialize
#ifndef _NO_PROTO
  (Widget request, Widget new)
#else
  (request, new)
Widget request, new;
#endif
{
    XmGraphConstraintsRec * wconst = CONSTRAINTREC(new);
    Widget parent = XtParent(new);

#ifdef _R4_
    if(XmIsArc(new) || !XtIsRectObj(new))
#else
    if(XmIsArc(new) || !XtIsRectObject(new))
#endif
	return;


    wconst->node =  (NodePtr) XtMalloc (sizeof (Node));
    wconst->node->widget = new;
    wconst->delta_x =  wconst->delta_y = 0;
    wconst->old_x = wconst->old_y = 0;
    wconst->vel_x =  wconst->vel_y =  0.0;
    wconst->pos_x =  RX(new);
    wconst->pos_y =  RY(new);
    wconst->pos_fixed =  FALSE;
    InitNode (wconst->node);

    if(XtCallbackHasSome == XtHasCallbacks(parent, XmNinitializeDataCallback))
	XtCallCallbacks(parent, XmNinitializeDataCallback, new);
}

/*************************************<->*************************************
 *
 *   ConstraintDestroy. Free the node structure. By this time
 *   -----------
 *
 *************************************<->***********************************/

static void ConstraintDestroy
#ifndef _NO_PROTO
  (Widget w) 
#else
  (w) Widget w;
#endif
{ 
    Widget graphW =  XtParent(w);
    XmGraphWidget graph = (XmGraphWidget) graphW;

    if (XmIsArc(w))  
    {
	graph->graph.current_arc = NULL;
    }
    else 
    {
	NodePtr   node;

#ifdef _R4_
	if (!XtIsRectObj(w))
#else
	if (!XtIsRectObject(w))
#endif
	    return;
	node  = NODEPTR (w);

	if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNfreeDataCallback))	
	    XtCallCallbacks(graphW, XmNfreeDataCallback, w);

	graph->graph.current_node = NULL;

	if(node) 
	{
	    FreeNode (node);
#ifdef _R4_
	    if( XtIsWidget(w))
#else
	    if( XtIsWindowObject(w) || XtIsCompositeObject(w) )
#endif
		((XmGraphConstraintsRec *) w->core.constraints)->node = NULL;
	    else
		((XmGraphConstraintsRec *) ((Object) w)->object.constraints)->node = NULL;
	}
    }
}


/*************************************<->*************************************
 *
 *   AdjustSize:   Figure out the extent of all children and ask to grow to
 *   -----------   the same size + 50 pixels each way. Kludgy, but it allows
 *                 the widget to expand as nodes are interactively moved or
 *		   added.
 *
 * 910405 coha:  There is a problem with all of the comparisons for width
 * and height in Motif 1.1 because the data type has been changed to unsigned
 * short.  This affects the compares that are done for ABS and MAX because
 * they will be done as unsigned compares.  The values returned by RWidth
 * and RHeight are also changed to unsigned.
 *
 * 911202 Thuan: Because of the reason above, another macro called
 * ABS_DIM_DIFF is added to obtain the ABS value from 2 Dimension arguments.
 * Using this allow us to avoid the type-casting to integer and back 
 * (to avoid negative number being very large positive number in UNSIGNED 
 * data type)
 *************************************<->***********************************/
#define ABS(a) ((a) < 0 ? -(a) : (a))
#define ABS_DIM_DIFF(a,b) ((a>b)? a-b: b-a)  /* 12/02/91 CYY */

static void AdjustSize 
#ifndef _NO_PROTO
  (XmGraphWidget graph)
#else
  (graph) XmGraphWidget graph;
#endif
{ Widget graphW =  (Widget) graph;
    int              i;
    Dimension min_width = 0, min_height = 0;  /* 12/02/91 CYY */
    Dimension        replyWidth, replyHeight;
    XtGeometryResult result;
    int neg_y, neg_x;

    graph->graph.deferedAdjustSize = FALSE; 

    /*
      First find out if any child has a negative coord. and correct situation if needed.
      */
    neg_x = neg_y = 0;

    for (i = 0; i < graph->composite.num_children; i++) 
    {
	Widget child = graph->composite.children[i];

	if(child == graph->graph.root->widget) 
	    continue;

	if(RX(child) < neg_x)
	    neg_x = RX(child);

	if(RY(child) < neg_y)
	    neg_y = RY(child);
    }

    if(neg_x < 0 || neg_y < 0) 
	XmGraphMoveAll(graphW, ABS(neg_x), ABS(neg_y));

    /*
     * Get extent of all children  
     */  

    for (i = 0; i < graph->composite.num_children; i++) 
    {
	Widget child = graph->composite.children[i];

	if(child == graph->graph.root->widget) 
	    continue;

	if (XtIsManaged (child)) 
	{
	    min_width =  MAX (min_width,   RX(child) + RWidth(child)  + RBorder(child) * 2);
	    min_height = MAX (min_height,  RY(child) + RHeight(child) + RBorder(child) * 2);
	}
    }

    /* The if(graph->graph.is_scrolled) statement is pulled out of the if to
       insure the initial width is correct.  The graph should always be at least
       as large as the clip window.  SESD
    */
    if(graph->graph.is_scrolled) 
	{
	    min_width  = MAX(min_width, graph->graph.clip_width);
	    min_height = MAX(min_height, graph->graph.clip_height);
	}
    /*  If we aren't at least that big, ask to grow    */

#if SESD
    if (min_width > RWidth(graph) || min_height > RHeight(graph) ||
        min_width +50 < RWidth(graph) || min_height+50 < RHeight(graph)) 
#else
    if (min_width > RWidth(graph) || min_height > RHeight(graph)) 
#endif
    {
	result = XtMakeResizeRequest (graphW, 
				      min_width,
				      min_height,
				      &replyWidth, 
				      &replyHeight);
	/*  accept any compromise  */

	if (result == XtGeometryAlmost) 
	    XtMakeResizeRequest (graphW, replyWidth, replyHeight, NULL, NULL);
    }

    else if (min_width  + 50 < RWidth(graph) || min_height +  50 < RHeight(graph)) 
    {
	result = XtMakeResizeRequest (graphW, min_width, min_height, &replyWidth, &replyHeight);

	if (result == XtGeometryAlmost) 
	    XtMakeResizeRequest (graphW, replyWidth, replyHeight, NULL, NULL);
#if SESD
	if(graph->graph.is_scrolled)
	  {   Arg       wargs[10];
	      Widget    hscroll, vscroll;
	      int       value;
	      int       slider_size;
	      int       increment;
	      int       page_increment;
      
	      /* Get the scrollbars from the graph. */
	      Widget swindow = XtParent(XtParent(graph));
	      XtSetArg(wargs[0], XmNverticalScrollBar, &vscroll);
	      XtSetArg(wargs[1], XmNhorizontalScrollBar, &hscroll);
	      XtGetValues(swindow, wargs, 2);
      
	      /* Now find out where the widget is located. */
	      if(vscroll)
	      {
		  XmScrollBarGetValues (vscroll, &value, 
					&slider_size, &increment, &page_increment);
		  XmScrollBarSetValues (vscroll, value, slider_size, SCROLLBAR_INCREMENT, page_increment, TRUE);
	      }
	      if(hscroll)
	      {
		  XmScrollBarGetValues (hscroll, &value, 
					&slider_size, &increment, &page_increment);
		  XmScrollBarSetValues (hscroll, value, slider_size, SCROLLBAR_INCREMENT, page_increment, TRUE);
	      }
	  }

	{ /* recompute core.visible for all arcs */
	  XmArcWidgetList arcs = graph->graph.arcs;
	  Widget arcW;
	  XmArcWidget arc;
	  int n_arcs = graph->graph.n_arcs;
	  int i;

	  for (i = 0; i < n_arcs; i++) 
	    { arc = arcs[i];
	      arcW = (Widget) arc;
	      arcW->core.visible = ArcVisibleInGraph(graph, arc);

	      if(arcW->core.visible)
		{ if (graph->core.visible) 
		  (* arcW->core.widget_class->core_class.expose) (arcW, NULL, NULL);
		}
	      else
		  FreeArcRegions(arc);
	    }
	}
#endif
    }
}

/*************************************<->*************************************
 *
 * XmGraphMoveAll: Move the entire graph by (dx, dy)
 *   -----------
 *
 *************************************<->***********************************/

void XmGraphMoveAll
#ifndef _NO_PROTO
  (Widget graphW, int delta_x, int delta_y)
#else
  (graphW, delta_x, delta_y)
       Widget graphW; 
       int    delta_x, delta_y;
#endif
{ XmGraphWidget graph = (XmGraphWidget) graphW;
    XmArcWidgetList   arcs;
    XmArcWidget  arc;
    Widget arcW;
    int          n_arcs, i;

    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = TRUE;

    arcs = graph->graph.arcs; 
    n_arcs = graph->graph.n_arcs;
    for (i = 0; i < n_arcs; i++) 
    {
	arc = (XmArcWidget) arcs[i];
	_EraseArc (arc);
    }

    for (i = 0; i < graph->composite.num_children; i++) 
    {
	Widget child = graph->composite.children[i];
#if (XmREVISION==1)
	_XmMoveObject((RectObj)child, RX(child) + delta_x, RY(child) + delta_y);
#else
	_XmMoveObject(child, RX(child) + delta_x, RY(child) + delta_y);
#endif

	UpdateArcs(child, FALSE);
    }

    arcs = graph->graph.arcs;
    n_arcs = graph->graph.n_arcs;
    { 
      for (i = 0; i < n_arcs; i++) 
      { arc = arcs[i];
	arcW = (Widget) arc;
	arcW->core.visible = ArcVisibleInGraph(graph, arc);

	if(arcW->core.visible) 
	{ if(graph->core.visible) 
	    (* arcW->core.widget_class->core_class.expose) (arcW, NULL, NULL);
        }
	else 
	    FreeArcRegions(arc);
      }
    }
    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = FALSE;

}


/*************************************<->*************************************
 *
 *   QueryGeometry. Try be be as big as the extent of all our children.
 *   -----------
 *
 *************************************<->***********************************/

static XtGeometryResult QueryGeometry 
#ifndef _NO_PROTO
  (Widget graphW, XtWidgetGeometry *intended, XtWidgetGeometry *desired)
#else
  (graphW, intended, desired) Widget graphW;
		XtWidgetGeometry	*intended, *desired;
#endif
{ XmGraphWidget graph = (XmGraphWidget) graphW;

    int  i, min_width = 0, min_height = 0;

    if (intended->request_mode & ~ (CWWidth | CWHeight))
	return (XtGeometryYes);

    if ( (intended->request_mode & CWWidth) &&
	(intended->request_mode & ~CWHeight))
	min_width = intended->width;

    if ( (intended->request_mode & CWHeight) &&
	(intended->request_mode & ~CWWidth))
	min_height = intended->height;

    /*  Get extent of all children  */
    for (i = 0; i < graph->composite.num_children; i++) 
    {
	Widget child = graph->composite.children[i];

	if (XtIsManaged (child)) {
	    min_width = MAX (min_width,  RX(child) + RWidth(child) + RBorder(child) * 2);
	    min_height = MAX (min_height,  RY(child) + RHeight(child) + RBorder(child) * 2);
	}
    }

    desired->request_mode = 0;
    desired->width = MAX(RWidth(graph), min_width);
    desired->height = MAX(RHeight(graph), min_height);

    if (!intended->request_mode) 
    {
	desired->request_mode |= CWWidth;
	desired->request_mode |= CWHeight;
	return (XtGeometryAlmost);
    }

    if ( (intended->request_mode & (CWWidth | CWHeight)) && 
	((min_width != intended->width) || 
	 (min_height != intended->height))) 
    {
	desired->request_mode |= CWWidth;
	desired->request_mode |= CWHeight;
	return (XtGeometryAlmost);
    }

    if ( (intended->request_mode & CWWidth) && 
	(intended->request_mode & ~CWHeight))
    {
	desired->request_mode |= CWHeight;
	return (XtGeometryAlmost);
    }

    if ( (intended->request_mode & CWHeight) &&
	(intended->request_mode & ~CWWidth)) 
    {
	desired->request_mode |= CWWidth;
	return (XtGeometryAlmost);
    }

    return (XtGeometryYes);
}

/*************************************<->*************************************
 *
 *   ChangeManaged:   Do the layout and redraw all arcs. Called by
 *                    XtRealizeWidget, after the nodes and arcs are inserted.
 *  		      Also called after each XtDestroyWidget call, as well
 *                    as when a new child is managed/unmanaged.
 *************************************<->***********************************/

static void ChangeManaged 
#ifndef _NO_PROTO
  (XmGraphWidget graph)
#else
  (graph) XmGraphWidget graph;
#endif
{ Widget graphW= (Widget) graph;
    int i;

    if (graph->core.visible)
      graph->graph.deferedChangeManaged = FALSE;
    else
      { graph->graph.deferedChangeManaged = TRUE;
        return;
      }


    if(graph->graph.auto_layout_mode == XmALWAYS)
    {
	graph->graph.layed_out = FALSE;
	XmGraphLayout(graphW);
    }

    if (graph->graph.edit_mode ) 
	for (i = 0 ; i < graph->composite.num_children; i++) 
	    MakeEditable(graph->composite.children[i]);
    else
	for (i = 0 ; i < graph->composite.num_children; i++) 
	    MakeReadOnly(graph->composite.children[i]);


    if (XtIsRealized (graph)) 
    {
	int         n_arcs = graph->graph.n_arcs;
	XmArcWidget arc;
	Widget arcW;
	Widget      to, from;
	Region graph_region = VisibleGraphRegion(graph);
	/* erase arcs whose nodes are not managed */

	_InitArcList(graph);  /* clear line list */
	graph->graph.batch_drawing_mode = TRUE;

	for (i = 0; i < n_arcs; i++) 
	{
	    arc =  graph->graph.arcs[i];
	    arcW = (Widget) arc;
	    from = arc->arc.from;
	    to   = arc->arc.to;

	    if( from == NULL || to == NULL )
		continue;

	    if (arc->core.visible && arc->arc.visible && 
		!(arc->core.managed && XtIsManaged(from) && XtIsManaged(to)) &&
		(XmIsGadget(to) || XmIsGadget(from) || 
		(to->core.mapped_when_managed  && 
		from->core.mapped_when_managed)))
	    {
		_EraseArc (arc);
	    }
	    else if (!arc->arc.visible && arc->core.visible &&
		arc->core.managed && XtIsManaged(from) && 
		XtIsManaged(to) &&
		(XmIsGadget(to) || XmIsGadget(from) ||
		(to->core.mapped_when_managed  && 
		from->core.mapped_when_managed)))
	    {
		if(arc->core.being_destroyed ||
		   XmBeingDestroyed(from) ||  
                   XmBeingDestroyed(to))
		    continue;

#if SESD
	      if (graph->core.visible)
#endif
		(* arc->core.widget_class->core_class.expose) 
                  (arcW, NULL, graph_region);
	    }
	}
	_InitArcList(graph);  /* clear line list */
	graph->graph.batch_drawing_mode = FALSE;
	XDestroyRegion (graph_region);
#if SESD
	AdjustSize(graph);
#endif
    }
}


/****************************************************************
 *
 * _RefreshArcs: Redisplay those arcs under given region.
 *
 *****************************************************************/
static void _RefreshArcs 
#ifndef _NO_PROTO
  (XmGraphWidget graph, Region region)
#else
  (graph, region)
XmGraphWidget	graph;
Region             region;
#endif
{
    int          i, n_arcs = graph->graph.n_arcs;
    XmArcWidget  arc;

    if(!XtIsRealized(graph) || !graph->core.visible)
	return;

    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = TRUE;

    if(region)
    {   Widget arcW;
	XRectangle rr;
	XClipBox(region, &rr);

	for (i = 0; i < n_arcs; i++)
	{
	    arc =  graph->graph.arcs[i];
	    arcW = (Widget) arc;

	    if(arc->arc.visible && arc->core.visible && 
	       (region != arc->arc.region) && _ArcInRect(arc, &rr))
	(* arc->core.widget_class->core_class.expose) (arcW, NULL, region);
	}
    }
    else
    {   /* If no region, draw everything */
      Widget arcW;

	for (i = 0; i < n_arcs; i++) 
	{
	    arc =  graph->graph.arcs[i];
	    arcW = (Widget) arc;

	    if(arc->arc.visible && arc->core.visible)
	    /* Arcs that aren't currently drawn, don't need refreshed */
		(* arc->core.widget_class->core_class.expose)(arcW, NULL, NULL);
	}
    } 
    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = FALSE;

}

/*************************************<->*************************************
 *
 *   Redisplay:  Redraw all arcs and gadgets. 
 *   --------
 *************************************<->***********************************/

static void Redisplay 
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event, Region region)
#else
  (graph, event, region)
XmGraphWidget 		graph;
XEvent		*event;
Region		region;
#endif
{ Widget	graphW = (Widget)	graph;
  int i;
  int n_arcs = graph->graph.n_arcs;
  Widget arcW;

    if (graph->core.visible) 
     {
#if SESD
       	if (graph->graph.deferedChangeManaged)
	{ graph->graph.deferedChangeManaged = FALSE;
          graph->graph.deferedAdjustSize = FALSE;
          ChangeManaged(graph);
        }
       if (graph->graph.deferedAdjustSize)
        { graph->graph.deferedAdjustSize = FALSE;
          AdjustSize(graph);
        }


#endif
	if (!region) 
	{
	    XClearWindow (XtDisplay (graph), XtWindow (graph)); 

	    if (event)
		_XmRedisplayGadgets (graphW, event, NULL);

	    _InitArcList(graph);  /* clear line list */
	    graph->graph.batch_drawing_mode = TRUE;

	    for (i = 0; i < n_arcs; i++) 
	    {
		arcW = (Widget)graph->graph.arcs[i];

		if(arcW->core.managed && arcW->core.visible)
	    (* arcW->core.widget_class->core_class.expose)(arcW, event, NULL);
	    }
	    graph->graph.batch_drawing_mode = FALSE;
	    _InitArcList(graph);  /* clear line list */
	}
	else
	{
	    _RefreshGadgets (graph, region);

	    _InitArcList(graph);  /* clear line list */
	    graph->graph.batch_drawing_mode = TRUE;

	    for (i = 0; i < n_arcs; i++) 
	    {
		arcW = (Widget)  graph->graph.arcs[i];

		if(arcW->core.managed && arcW->core.visible)
                (* arcW->core.widget_class->core_class.expose) (arcW, event, region);
	    }

	    graph->graph.batch_drawing_mode = FALSE;
	    _InitArcList(graph);  /* clear line list */
	}
    }
}


/*************************************<->*************************************
 *
 *   SetValues: 
 *   -----------
 *
 *************************************<->***********************************/

static Boolean SetValues 
#ifndef _NO_PROTO
  (Widget current_w, Widget request_w, Widget new_w)
#else
  (current_w, request_w, new_w) Widget current_w, request_w, new_w;
#endif
{ XmGraphWidget current = (XmGraphWidget) current_w, 
		new = (XmGraphWidget) new_w;
    int        redraw = FALSE, i;
    XGCValues  values;
    XtGCMask   valueMask;
    Boolean    needs_relayout = FALSE;

    if(PtrCursor(new) != PtrCursor(current))
    {
	if(XtIsRealized(new))
	    XDefineCursor (XtDisplay (new), XtWindow (new),  PtrCursor (new));

	if(PtrCursor (current))
	    XFreeCursor(XtDisplay(current), PtrCursor (current));
    }

    if(MotionCursor(current) && MotionCursor(new) != MotionCursor(current))
    {
	XFreeCursor(XtDisplay(current), MotionCursor (current));
    }

    if(IndicateCursor(current) && IndicateCursor(new) != IndicateCursor(current))
    {
	XFreeCursor(XtDisplay(current), IndicateCursor (current));
    }

    if(IndicateChildCursor(current) && IndicateChildCursor(new) != IndicateChildCursor(current))
    {
	XFreeCursor(XtDisplay(current), IndicateChildCursor (current));
    }

    if(IndicateParentCursor(current) && IndicateParentCursor(new) != IndicateParentCursor(current))
    {
	XFreeCursor(XtDisplay(current), IndicateParentCursor (current));
    }


    /*
     * Check anything that would effect the screen resolution independence.
     */
    if ((new->manager.unit_type != current->manager.unit_type ||
	 new->graph.direction != current->graph.direction ||
	 new->graph.re_orient != current->graph.re_orient)  &&
	new->graph.child_spacing == current->graph.child_spacing &&
	new->graph.sibling_spacing == current->graph.sibling_spacing)
    { 
	/* Only if the child spacing is the same */

#ifdef _R4_
	if(new->graph.direction == XmHORIZONTAL)
	{
	    new->graph.child_spacing = (int)
		XmCvtToHorizontalPixels(XtScreen(new), 
					new->graph.original_child_spacing,
					new->manager.unit_type);
	    new->graph.sibling_spacing = (int)
		XmCvtToVerticalPixels(XtScreen(new), 
				      new->graph.original_sibling_spacing,
				      new->manager.unit_type);
	}

	else if(new->graph.direction == XmVERTICAL)
	{
	    new->graph.child_spacing = (int)
		XmCvtToVerticalPixels(XtScreen(new), 
				      new->graph.original_child_spacing,
				      new->manager.unit_type);
	    new->graph.sibling_spacing = (int)
		XmCvtToHorizontalPixels(XtScreen(new), 
					new->graph.original_sibling_spacing,
					new->manager.unit_type);
	}
#else
      if(new->graph.direction == XmHORIZONTAL)
	{
	  new->graph.child_spacing = (int)
	    _XmToHorizontalPixels (new, new->manager.unit_type,
				   new->graph.original_child_spacing);

	  new->graph.sibling_spacing = (int)
	    _XmToVerticalPixels (new, new->manager.unit_type,
				 new->graph.original_sibling_spacing);
	}

      else if(new->graph.direction == XmVERTICAL)
	{
	  new->graph.child_spacing = (int)
	    _XmToVerticalPixels (new, new->manager.unit_type,
				 new->graph.original_child_spacing);

	  new->graph.sibling_spacing = (int)
	    _XmToHorizontalPixels (new, new->manager.unit_type,
				   new->graph.original_sibling_spacing);
	}
#endif
	needs_relayout = TRUE;
	redraw = TRUE;
    }
    /*
     * If the child or sibling spacing has changed, recompute the
     * resolution independence stuff.
     */
    if (new->graph.child_spacing != current->graph.child_spacing ||
	new->graph.sibling_spacing != current->graph.sibling_spacing)
    {

	/* Save the unconverted spacing for future use */

	new->graph.original_sibling_spacing =new->graph.sibling_spacing;  
	new->graph.original_child_spacing =new->graph.child_spacing; 

#ifdef _R4_
	if(new->graph.direction == XmHORIZONTAL)
	{
	    new->graph.child_spacing = (int)
		XmCvtToHorizontalPixels(XtScreen(new), 
					new->graph.original_child_spacing,
					new->manager.unit_type);

	    new->graph.sibling_spacing = (int)
		XmCvtToVerticalPixels(XtScreen(new), 
				      new->graph.original_sibling_spacing,
				      new->manager.unit_type);
	}

	else if(new->graph.direction == XmVERTICAL)
	{
	    new->graph.child_spacing = (int)
		XmCvtToVerticalPixels(XtScreen(new), 
				      new->graph.original_child_spacing,
				      new->manager.unit_type);

	    new->graph.sibling_spacing = (int)
		XmCvtToHorizontalPixels(XtScreen(new), 
					new->graph.original_sibling_spacing,
					new->manager.unit_type);

	}
#else
      if(new->graph.direction == XmHORIZONTAL)
	{
	    new->graph.child_spacing = (int)
		_XmToHorizontalPixels(new, 
					new->manager.unit_type,
					new->graph.original_child_spacing);

	    new->graph.sibling_spacing = (int)
		_XmToVerticalPixels(new, 
				      new->manager.unit_type,
				      new->graph.original_sibling_spacing);
	}

	else if(new->graph.direction == XmVERTICAL)
	{
	    new->graph.child_spacing = (int)
		_XmToVerticalPixels(new, 
				      new->manager.unit_type,
				      new->graph.original_child_spacing);

	    new->graph.sibling_spacing = (int)
		_XmToHorizontalPixels(new, 
					new->manager.unit_type,
					new->graph.original_sibling_spacing);

	}
#endif
	needs_relayout = TRUE;
	redraw = TRUE;
    }

    /* 
      CHeck If the direction has changed.
      */
    if (new->graph.direction != current->graph.direction) 
    {
	needs_relayout = TRUE;
	redraw = TRUE;
    }


    if (new->graph.arc_draw_mode != current->graph.arc_draw_mode) 
    {

	for (i = 0; i< new->composite.num_children; i++)
	    UpdateArcs(new->composite.children[i], FALSE);   

	redraw = TRUE;
    }


    if (new->graph.auto_layout_mode != current->graph.auto_layout_mode) 
    {
	if (new->graph.auto_layout_mode != XmNEVER) 
	{
	    needs_relayout = TRUE;
	    redraw = TRUE;
	}
    }
    /*
     * Always do a relayout when this resource is set.
     */
    if (new->graph.re_layout != current->graph.re_layout) 
    {
	needs_relayout = TRUE;
	redraw = TRUE;
	new->graph.re_layout = FALSE;
    }

    if (new->graph.siblings_visible != current->graph.siblings_visible) 
    {
	redraw = TRUE;
    }

    if (new->graph.re_orient != current->graph.re_orient) 
    {
	if (current->graph.direction == XmHORIZONTAL)
	    new->graph.direction = XmVERTICAL;
	else 
	    new->graph.direction = XmHORIZONTAL;

	needs_relayout = TRUE;
	redraw = TRUE;

	new->graph.re_orient = FALSE;
    }

    if (new->manager.foreground != current->manager.foreground ||
	new->core.background_pixel != current->core.background_pixel) 
    {
	valueMask = GCForeground | GCBackground;
	values.foreground = new->manager.foreground;
	values.background = new->core.background_pixel;
	XtReleaseGC(current_w, current->graph.gc);
	new->graph.gc = XtGetGC ((Widget) new, valueMask, &values);
    }

    if (new->core.background_pixel != current->core.background_pixel) 
    {
	int n_arcs = new->graph.n_arcs;
	XmArcWidget  arc;

	values.background = new->core.background_pixel;

	/* Adjust the XOR graphic Context */
	valueMask |= GCForeground | GCFunction | GCLineStyle | GCSubwindowMode;
	values.foreground =  new->manager.foreground ^ new->core.background_pixel;
	values.function = GXxor;
	values.line_style = LineOnOffDash;

	values.subwindow_mode = IncludeInferiors;
	XtReleaseGC(current_w, current->graph.xorgc);
	new->graph.xorgc = XtGetGC ((Widget) new, valueMask, &values);  

	/* Adjust the CLEAR graphic Context */
	valueMask = GCForeground | GCBackground | GCFunction;
	values.function = GXcopy;
	values.foreground = new->core.background_pixel;
	values.background = new->core.background_pixel;
	XtReleaseGC(current_w, current->graph.cleargc);
	new->graph.cleargc = XtGetGC((Widget) new, valueMask, &values);

	/* Now adjust the background pixel of all the arcs */
	valueMask = GCForeground | GCBackground;
	for (i = 0; i < n_arcs; i++) 
	{
	    arc =  new->graph.arcs[i];
	    values.foreground = arc->arc.foreground;
	    values.background = new->core.background_pixel; 
	    XtReleaseGC(current_w, arc->arc.gc);
	    arc->arc.gc = XtGetGC((Widget) arc, valueMask, &values);
	    if (!arc->arc.highlight) 
	    {
		arc->arc.current_gc = arc->arc.gc;
	    }
	}
	redraw = TRUE; 
    }

    /* intercept button pushes on node widgets */

    if ( new->graph.edit_mode &&  !current->graph.edit_mode ) 
	for (i = 0 ; i < new->composite.num_children; i++) 
	    MakeEditable(new->composite.children[i]);


    /* restore node widget's event mask */

    if (!new->graph.edit_mode && current->graph.edit_mode)
	for (i = 0 ; i < current->composite.num_children; i++) 
	    MakeReadOnly(new->composite.children[i]);

    if(needs_relayout) 
    { XmGraphLayout(new_w);
      return FALSE;
    }
    else if (new->graph.show_crossing_arcs != current->graph.show_crossing_arcs) 
    { Widget arcW;
      Boolean visible;
      XmArcWidget arc;
	for (i = 0; i < new->graph.n_arcs; i++) 
	{ arc =  new->graph.arcs[i]; 
	   arcW   = (Widget) arc;
	    visible = ArcVisibleInGraph(new, arc);

	    if(visible && !arcW->core.visible) 
	    {
		arcW->core.visible = TRUE;

		if(!redraw && XtIsRealized(new))
		    (* arcW->core.widget_class->core_class.expose) (arcW, NULL, NULL);
	    }
	    else  if (!visible && arcW->core.visible) 
	    {
		arcW->core.visible = FALSE;
		if(XtIsRealized(new))
		    _EraseArc(arc);
		redraw = TRUE;
	    }
	}
    }

    return redraw;
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void MakeEditable
#ifndef _NO_PROTO
  (Widget w)
#else
  (w) Widget w; 
#endif
{  

    Mask                  vMask = CWDontPropagate;
    XWindowAttributes     get_attr;
    XSetWindowAttributes  set_attr;
    Display              *display;
    XmGraphConstraintsRec *wconst;

    if(!w) 
	return;

    if((XtClass(w) != xmArcWidgetClass) &&  (!XmIsGadget(w))) 
    {
        display = XtDisplay (w);
        wconst = CONSTRAINTREC(w);

	if (XtIsRealized(w)) 
	{
	    wconst->old_mask = XtBuildEventMask(w);
	    /* This prevents w from getting button events */
	    XSelectInput(display, XtWindow(w), 
			 wconst->old_mask & ~(KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask));
	    /* make sure events are passed on to Graph widget */
	    XGetWindowAttributes (display, XtWindow(w),
				  &get_attr);
	    set_attr.do_not_propagate_mask = 
		get_attr.do_not_propagate_mask 
		    & ~(KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
	    XChangeWindowAttributes (display, XtWindow (w),
				     vMask, &set_attr);
	}
    }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void MakeReadOnly
#ifndef _NO_PROTO
  (Widget w)
#else
  (w) Widget w; 
#endif
{  
    Mask                  vMask = CWDontPropagate;
    XWindowAttributes     get_attr;
    XSetWindowAttributes  set_attr;
    Display              *display = XtDisplay (w);

    if ((XtClass(w) != xmArcWidgetClass) &&  (!XmIsGadget(w))) 
    {
	if (XtIsRealized (w)) 
	{
	    XmGraphConstraintsRec *wconst = CONSTRAINTREC(w);

	    wconst->old_mask = XtBuildEventMask(w);
	    XSelectInput(display, XtWindow(w), wconst->old_mask);
	    /* restore do_not_propagate mask */
	    XGetWindowAttributes (display, XtWindow(w),
				  &get_attr);
	    /* ButtonMotionMask is not in original mask */
	    set_attr.do_not_propagate_mask = 
		get_attr.do_not_propagate_mask &
		    (KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
	    XChangeWindowAttributes (display, XtWindow (w),
				     vMask, &set_attr);
	}
    }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void Resize 
#ifndef _NO_PROTO
  (XmGraphWidget w)
#else
  (w) XmGraphWidget w;
#endif
{

    if(w->graph.is_scrolled) 
    {
	Arg wargs[2];
	Widget clip;

	if(XtIsSubclass(XtParent(w), xmScrolledWindowWidgetClass))
	{
	    XtSetArg(wargs[0], XmNclipWindow, &clip);
	    XtGetValues(XtParent(w), wargs, 1);
	}
	else
	    clip = XtParent(w);

	if (!clip) 
	{
	    w->graph.clip_width  = XtWidth(w);
	    w->graph.clip_height = XtHeight(w);
	}
	else
	{
	    w->graph.clip_width  = RWidth(clip);
	    w->graph.clip_height = RHeight(clip);
	}
    }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void destroy_arc_callback
#ifndef _NO_PROTO
  (Widget w, XtPointer client_data, XtPointer call_data)
#else
  (w, client_data, call_data) Widget w; XtPointer client_data , call_data;
#endif
{
    DoRemoveArc ((XmGraphWidget) XtParent(w), (XmArcWidget) w);
}

static void destroy_node_callback
#ifndef _NO_PROTO
  (Widget w, XtPointer client_data, XtPointer call_data)
#else
  (w, client_data, call_data)
Widget w;
XtPointer client_data , call_data;
#endif
{
    XmGraphWidget graph = (XmGraphWidget) client_data;
    int i;
    NodePtr   node = NODEPTR (w);
    NodePtr     from_node ;
    NodePtr     to_node ;
    Widget arcW;
    XmArcWidget arc ;

    if(!node) return;

    /* 
     * remove all from_arcs, to_arcs,
     * and node from its parents and kids lists 
     */

    for (i = 0 ; i < node->from_arcs.n_arcs; i++) 
    {
	arc      = node->from_arcs.arcs[i];
	arcW 	= (Widget) arc;
	from_node = NODEPTR (arc->arc.from);
	to_node   = NODEPTR (arc->arc.to);

	if (to_node && from_node && to_node->tree_parent == from_node) 
	{
	    NodeListRemove (&(from_node->tree_kids), to_node);
	    to_node->tree_parent = NULL;
	}

	if(to_node && from_node) 
	{
	    NodeListRemove (&(to_node->parents), from_node);
	    NodeListRemove (&(from_node->kids), to_node);
	}

	XtDestroyWidget(arcW);

	node->from_arcs.arcs[i] = NULL;
	arc->arc.from = NULL;
    }

    node->from_arcs.n_arcs = 0;

    for (i = 0 ; i < node->to_arcs.n_arcs; i++) 
    {
	XmArcWidget arc       = (XmArcWidget) node->to_arcs.arcs[i];
	NodePtr     from_node = NODEPTR (arc->arc.from);
	NodePtr     to_node   = NODEPTR (arc->arc.to);

	arcW                  = (Widget) arc ;
	
	if (to_node && from_node && to_node->tree_parent == from_node) 
	{
	    NodeListRemove (&(from_node->tree_kids), to_node);
	    to_node->tree_parent = NULL;
	}

	if(to_node && from_node) 
	{
	    NodeListRemove (&(to_node->parents), from_node);
	    NodeListRemove (&(from_node->kids), to_node);  /* Added */
	}

	XtDestroyWidget(arcW);

	node->to_arcs.arcs[i] = NULL;
	arc->arc.to = NULL;
    }

    node->to_arcs.n_arcs = 0;

    /* In case node is a member of these lists */

    NodeListRemove (&(graph->graph.selected_nodes), node);
    NodeListRemove (&(graph->graph.user_roots), node);
    node->widget = NULL;
    node->tree_parent = NULL;

}


/*************************************<->*************************************
 *
 *   InsertChild:  Insert Arcs in the arc list. Node widgets are inserted into
 *   -----------   node Data Structure and made part of graph.
 *
 *************************************<->***********************************/

static void InsertChild 
#ifndef _NO_PROTO
  (Widget w)
#else
  (w) Widget	w;
#endif
{
    register Cardinal	     position;
    register Cardinal        i;
    register XmGraphWidget   graph = (XmGraphWidget) XtParent(w);
    register WidgetList      children;

    if (XtClass(w) == xmArcWidgetClass) 
    { XtAddCallback(w, XmNdestroyCallback, 
		      destroy_arc_callback, graph); 

	children = (WidgetList)graph->graph.arcs;
	position = graph->graph.n_arcs;
	if (graph->graph.n_arcs == graph->graph.n_arc_slots) 
	{
	    /* Allocate more space */
	    graph->graph.n_arc_slots +=  (graph->graph.n_arc_slots / 2) + 2;
	    children = (WidgetList) XtRealloc((caddr_t) children,
		       (unsigned) (graph->graph.n_arc_slots) * sizeof(Widget));
	    graph->graph.arcs = (XmArcWidgetList) children;
	}

	/* Ripple arcs up one space from "position" */
	for (i = graph->graph.n_arcs; i > position; i--) 
	{
	    children[i] = children[i-1];
	}
	children[position] =  w;
	graph->graph.n_arcs++;

	/* All geometry is handled directly by graph widget */
	XtSetMappedWhenManaged (w, FALSE);
    }
    else 
    { 
#ifdef _R4_
	if (!XtIsRectObj(w))
#else
	if (!XtIsRectObject(w))
#endif
	    return;

	XtAddCallback(w, XmNdestroyCallback, 
		       destroy_node_callback, graph); 

	/* we have a node widget !! */

	if ( graph->graph.edit_mode ) 
	    MakeEditable(w);
	else
	    MakeReadOnly(w);

	children = graph->composite.children;

	if (graph->composite.insert_position != NULL)
	    position = (* (graph->composite.insert_position)) (w);
	else
	    position = graph->composite.num_children;

	if (graph->composite.num_children == graph->composite.num_slots) 
	{
	    /* Allocate more space */
	    graph->composite.num_slots +=  (graph->composite.num_slots / 2) + 2;
	    graph->composite.children = children = 
		(WidgetList) XtRealloc((caddr_t) children,
				       (unsigned) (graph->composite.num_slots) * sizeof(Widget));
	}

	/* Ripple children up one space from "position" */

	for (i = graph->composite.num_children; i > position; i--) 
	{
	    children[i] = children[i-1];
	}

	children[position] = w;
	graph->graph.n_nodes = ++graph->composite.num_children;

	/* make a new node, with this node widget */

    }
}

static void DeselectAllNodes
#ifndef _NO_PROTO
  (Widget graphW) 
#else
  (graphW) Widget graphW;
#endif
{ XmGraphWidget graph= (XmGraphWidget) graphW;
    int i;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("DeselectAllNodes requires an XmGraph widget");
	return;
    }

    for(i=0; i<  graph->graph.selected_nodes.n_nodes; i++) 
    {
	_XmUnhighlightBorder(graph->graph.selected_nodes.nodes[i]->widget);
	graph->graph.selected_nodes.nodes[i] = NULL;
    }

    graph->graph.selected_nodes.n_nodes = 0;
}

static void DeselectAllArcs
#ifndef _NO_PROTO
  (Widget graphW) 
#else
  (graphW) Widget graphW;
#endif
{ XmGraphWidget graph= (XmGraphWidget) graphW;
 
    int i;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass))  
    {
	XtWarning("DeselectAllArcs requires an XmGraph widget");
	return;
    }
    for(i=0; i < graph->graph.selected_arcs.n_arcs; i++) 
    {
	_XmUnhighlightArc(graph->graph.selected_arcs.arcs[i]);
	graph->graph.selected_arcs.arcs[i] = NULL;
    }

    graph->graph.selected_arcs.n_arcs = 0;
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void  HandleMotion
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{
    switch(graph->graph.current_action) {
    case  NODE_INDICATED:
    case  NODE_INDICATED_PENDING_CANCEL:
    case  ARC_INDICATED:
    case  ARC_INDICATED_PENDING_CANCEL:
	Cancel(graph, event);
	break;
    case REGION_INDICATED:
	SweepOutRegion(graph, event);
	break;
    case NODES_INDICATED:
    case ARCS_INDICATED:
    case NODES_INDICATED_PENDING_CANCEL:
    case ARCS_INDICATED_PENDING_CANCEL:
	CancelAdd(graph, event);
	break;
    case NODE_SELECTED_FOR_MOTION:
    case ARC_SELECTED_FOR_MOTION:
    case MULTIPLE_NODES_SELECTED_FOR_MOTION:
	Motion(graph, event);
	break;
    case ADDING_ARC:
    case ADDING_ARC_IN_PARENT:
    case ADDING_NODE:
	PositionNewNode(graph, event);
	break;
    case SUBGRAPH_INDICATED:
    case SUBGRAPH_INDICATED_PENDING_CANCEL:
	CancelSubtree(graph, event);
	break;
    case NODE_SELECTED:
    case NODES_SELECTED:
    case ARC_SELECTED:
    case ARCS_SELECTED:
	break;
    }
}


static void  EndButtonAction
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{

    XDefineCursor (XtDisplay (graph), XtWindow (graph),  PtrCursor (graph));

    switch(graph->graph.current_action) {
    case  NODE_INDICATED:
    case  ARC_INDICATED:
    case  POSSIBLE_DOUBLE_CLICK:
	Select(graph, event);
	break;

    case NODES_INDICATED:
    case ARCS_INDICATED:
	AddSelect(graph, event);
	break;

    case REGION_INDICATED:
	SelectRegion(graph, event);
	break;

    case NODE_SELECTED_FOR_MOTION:
    case ARC_SELECTED_FOR_MOTION:
    case MULTIPLE_NODES_SELECTED_FOR_MOTION:
	EndMotion(graph, event);
	break;

    case ADDING_ARC:
    case ADDING_NODE:
	EndAddNode(graph, event);
	break;

    case SUBGRAPH_INDICATED:
	SelectSubtree(graph, event);
	break;

    case NODE_INDICATED_PENDING_CANCEL:
    case ARC_INDICATED_PENDING_CANCEL:
    case NODES_INDICATED_PENDING_CANCEL:
    case ARCS_INDICATED_PENDING_CANCEL:
    case SUBGRAPH_INDICATED_PENDING_CANCEL:
    case NODE_SELECTED:
    case NODES_SELECTED:
    case ARC_SELECTED:
    case ARCS_SELECTED:
	graph->graph.current_action = NORMAL;
	break;
    }
}

/******************************************************************************
 *  Routines for Indicating/Cancel/Selecting a node or arc
 *  Indicate(), Cancel(), Select()
 *
 ******************************************************************************/

static void  Indicate
#ifndef _NO_PROTO
  (Widget graphW, XEvent *event)
#else
  (graphW, event) Widget graphW; XEvent *event;
#endif
{ XmGraphWidget graph= (XmGraphWidget) graphW;

    Widget w;

    if ( graph->graph.edit_mode) 
    {
	if(IsDoubleClick(graph, event)) 
	{
	    graph->graph.current_action = NORMAL;
	    HandleDoubleClick(graph, event);
	    return;
	}


	if ( event->xbutton.subwindow ||
#if (XmREVISION==1)
	    (w =  (Widget) _XmInputInGadget ((CompositeWidget)graph, 
					     event->xbutton.x,
					     event->xbutton.y)) != NULL)
#else
	    (w =  (Widget) _XmInputInGadget ((Widget)graph, 
					     event->xbutton.x,
					     event->xbutton.y)) != NULL)
#endif
	{ 
	    if (event->xbutton.subwindow)
		w = XtWindowToWidget (XtDisplay(graph),
				      event->xbutton.subwindow);

	    if(XmGraphIsSelectedNode(graphW, w)) 
	    {
		graph->graph.current_action = POSSIBLE_DOUBLE_CLICK;
/* 
### possible change (hjs)
### so key events can be passed to the selected node(s)
*/
                   graph->graph.indicated_widget = w;
                /* graph->graph.indicated_widget = NULL; */
		return;
	    }

	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  IndicateCursor (graph));

	    graph->graph.indicated_widget = w;

	    graph->graph.current_action = NODE_INDICATED;

	    _XmHighlightBorder(w);

	}
	else if( (w =  XmGraphInputOverArc(graphW, event->xbutton.x, 
						   event->xbutton.y)) != NULL) 
	{ XmArcWidget arc = (XmArcWidget) w;


	    if(XmGraphIsSelectedArc(graphW, w)) {
		graph->graph.current_action = POSSIBLE_DOUBLE_CLICK;
/* ### possible change (hjs) */
/* ### so key events can be passed to the selected arc(s) */
                graph->graph.indicated_widget = w;
                /* graph->graph.indicated_widget = NULL; */

		return;
	    }

	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  IndicateCursor (graph));

	    graph->graph.indicated_widget = w;

	    graph->graph.current_action = ARC_INDICATED;

	    _XmHighlightArc (arc);
	}
	else 
	{
	    XmGraphCallbackStruct    cb;

	    graph->graph.current_action = NORMAL;
	    graph->graph.indicated_widget = NULL;

	    if(graph->graph.selected_arcs.n_arcs || graph->graph.selected_nodes.n_nodes) 
	    {
		if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNdeselectCallback))
		{
		    cb.event  = event;
		    cb.reason = XmCR_DESELECT;
		    cb.widget = NULL;
		    cb.interactive = TRUE;

		    cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

		    cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

		    XtCallCallbacks (graphW, XmNdeselectCallback, &cb);

		}

		DeselectAllNodes(graphW); 
		DeselectAllArcs(graphW);

	    }
#if 0
/* CYY, this is fixed both both SESD and HJS */
/* ####### - hjs - add double-click check for this event ###### */
/* double click on the graph (not the nodes) */
           if(IsDoubleClick(graph, event)) 
	   { graph->graph.current_action = NORMAL;
              HandleDoubleClick(graph, event);
	      return;
           }
#endif
/* ####### - hjs - add check for control key in open region ###### */
           else if (event->type & KeyPress) 
	   { XmGraphCallbackStruct    cb;
              graph->graph.current_action = NORMAL;
              graph->graph.indicated_widget = NULL;
              if(XtCallbackHasSome == XtHasCallbacks(graphW,
                                                     XmNdefaultActionCallback))
              { cb.event  = event;
                cb.reason = XmCR_RELEASE;
                cb.widget = NULL;
                cb.interactive = TRUE;
                cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);
                cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);
                XtCallCallbacks (graphW, XmNdefaultActionCallback, &cb);
              }
	      return;
           }
           else
/* ####### - end additions hjs ###### */

	    IndicateRegion(graph, event);

	}
    }

}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void  Cancel
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent       *event;
#endif
{  Widget graphW = (Widget) graph;
   Widget w;
   Widget oldwidget  = graph->graph.indicated_widget;

    if ( graph->graph.edit_mode) 
    {

	if(!graph->graph.indicated_widget)
	    return;

	if ((event->xbutton.subwindow  &&  (w = XtWindowToWidget (XtDisplay(graph),
								  event->xbutton.subwindow)) != NULL)  ||
#if (XmREVISION==1)
	    ((w =  (Widget) _XmInputInGadget ((CompositeWidget)graph,
					      event->xbutton.x, 
					      event->xbutton.y)) != NULL) ||
#else
	    ((w =  (Widget) _XmInputInGadget ((Widget)graph,
					      event->xbutton.x, 
					      event->xbutton.y)) != NULL) ||
#endif
	    ((w =  XmGraphInputOverArc(graphW, event->xbutton.x, 
						event->xbutton.y)) != NULL))
	{ 
	    /*
	     * OK, we have a widget. Now is it the one we previously indicated, or not?
	     */

	    if(oldwidget != NULL && w != oldwidget)   /* If no */
	    {
		if(graph->graph.current_action == NODE_INDICATED)
		    _XmUnhighlightBorder(oldwidget);

		else if(graph->graph.current_action == ARC_INDICATED)
		    _XmUnhighlightArc ((XmArcWidget)oldwidget);

		if(XmIsArc(oldwidget))
		    graph->graph.current_action = ARC_INDICATED_PENDING_CANCEL;
		else
		    graph->graph.current_action = NODE_INDICATED_PENDING_CANCEL;

	    }
	    else if( w == oldwidget)  /* Else we are in the original indicated widget */
	    {

		if(graph->graph.current_action == NODE_INDICATED_PENDING_CANCEL)
		{
		    _XmHighlightBorder(oldwidget);
		    graph->graph.current_action = NODE_INDICATED;
		}
		else if(graph->graph.current_action == ARC_INDICATED_PENDING_CANCEL)
		{
		    _XmHighlightArc ((XmArcWidget)oldwidget);
		    graph->graph.current_action = ARC_INDICATED;
		}
	    }
	}
	else        /* We must not be over any widget - has to be a pending cancel action*/
	{

	    if(graph->graph.current_action == NODE_INDICATED ||
	       graph->graph.current_action == ARC_INDICATED)
	    {

		if(XmIsArc(oldwidget))
		    _XmUnhighlightArc ((XmArcWidget)oldwidget);
		else 
		    _XmUnhighlightBorder(oldwidget);

		if(XmIsArc(oldwidget))
		    graph->graph.current_action = ARC_INDICATED_PENDING_CANCEL;
		else
		    graph->graph.current_action = NODE_INDICATED_PENDING_CANCEL;
	    }
	}
    }  
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void  CancelAdd
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{ Widget graphW =(Widget)graph;
   Widget w;
   Widget oldwidget  = graph->graph.indicated_widget;

    if ( graph->graph.edit_mode) 
    {

	if(!graph->graph.indicated_widget)
	    return;

	if (event->xbutton.subwindow ||
#if (XmREVISION==1)
	    (w =  (Widget) _XmInputInGadget ((CompositeWidget)graph, 
					     event->xbutton.x, 
					     event->xbutton.y)) != NULL)
#else
	    (w =  (Widget) _XmInputInGadget ((Widget)graph, 
					     event->xbutton.x, 
					     event->xbutton.y)) != NULL)
#endif
	{ 
	    if (event->xbutton.subwindow)
		w = XtWindowToWidget (XtDisplay(graph),
				      event->xbutton.subwindow);

	    if(oldwidget != NULL && w != oldwidget) {

		if(graph->graph.current_action == NODES_INDICATED)
		    _XmUnhighlightBorder(oldwidget);
		else if(graph->graph.current_action == ARCS_INDICATED)
		    _XmUnhighlightArc ((XmArcWidget)oldwidget);

		if(XmIsArc(oldwidget))
		    graph->graph.current_action = ARCS_INDICATED_PENDING_CANCEL;
		else
		    graph->graph.current_action = NODES_INDICATED_PENDING_CANCEL;

	    }
	    else if( w == oldwidget)
	    {

		if(graph->graph.current_action == NODES_INDICATED_PENDING_CANCEL)
		    _XmHighlightBorder(oldwidget);

		graph->graph.current_action = NODES_INDICATED;

	    }
	}
	else 
	    if( (w =  XmGraphInputOverArc(graphW, event->xbutton.x,
						  event->xbutton.y)) != NULL) 
	    {

		if(oldwidget != NULL && w != oldwidget) 
		{

		    if(graph->graph.current_action == ARCS_INDICATED)
			_XmUnhighlightArc ((XmArcWidget)oldwidget);
		    else if(graph->graph.current_action == NODES_INDICATED)
			_XmUnhighlightBorder(oldwidget);


		    if(XmIsArc(oldwidget))
			graph->graph.current_action = ARCS_INDICATED_PENDING_CANCEL;
		    else
			graph->graph.current_action = NODES_INDICATED_PENDING_CANCEL;

		}
		else if( w == oldwidget)
		{

		    if(graph->graph.current_action == ARCS_INDICATED_PENDING_CANCEL)
			_XmHighlightArc ((XmArcWidget)oldwidget);

		    graph->graph.current_action = ARCS_INDICATED;
		}

	    }
	    else 
	    {

		if(graph->graph.current_action == NODES_INDICATED ||
		   graph->graph.current_action == ARCS_INDICATED)

		    if(oldwidget && XmIsArc(oldwidget))
			_XmUnhighlightArc ((XmArcWidget)oldwidget);
		    else
			_XmUnhighlightBorder(oldwidget);

		if(XmIsArc(oldwidget))
		    graph->graph.current_action = ARCS_INDICATED_PENDING_CANCEL;
		else
		    graph->graph.current_action = NODES_INDICATED_PENDING_CANCEL;
	    }
    }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void    Select
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{ Widget graphW =(Widget)graph;

    Widget w;
    XmGraphCallbackStruct    cb;

    if ( graph->graph.edit_mode) 
    {

#if !SESD
	if(IsDoubleClick(graph, event)) 
	{
	    graph->graph.current_action = NORMAL;
	    HandleDoubleClick(graph, event);
	    return;
	}
#endif

	if(!graph->graph.indicated_widget) 
	{
	    graph->graph.current_action = NORMAL;
	    return;
	}

	if (event->xbutton.subwindow ||
#if (XmREVISION==1)
	    (w =  (Widget) _XmInputInGadget ((CompositeWidget)graph,
					     event->xbutton.x,
					     event->xbutton.y)) != NULL)
#else
	    (w =  (Widget) _XmInputInGadget ((Widget)graph,
					     event->xbutton.x,
					     event->xbutton.y)) != NULL)
#endif
	{ 
	    if (event->xbutton.subwindow)
		w = XtWindowToWidget (XtDisplay(graph),
				      event->xbutton.subwindow);

	    if(graph->graph.indicated_widget == w && graph->graph.current_action == NODE_INDICATED) 
	    {

		if(graph->graph.selected_arcs.n_arcs || graph->graph.selected_nodes.n_nodes) 
		{
		    if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNdeselectCallback))
		    {
			cb.event  = event;
			cb.reason = XmCR_DESELECT;
			cb.widget = NULL;
			cb.interactive = TRUE;

			cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

			cb.selected_arcs  =  XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

			XtCallCallbacks (graphW, XmNdeselectCallback, &cb);

		    }
		    DeselectAllNodes(graphW); 
		    DeselectAllArcs(graphW);
		}

		XmGraphSelectNode(graphW, w);

		graph->graph.indicated_widget = NULL;
		graph->graph.current_action = NODE_SELECTED;

		if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNselectNodeCallback))
		{
		    cb.event  = event;
		    cb.reason = XmCR_SELECT_NODE;
		    cb.widget = w;
		    cb.interactive = TRUE;

		    cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

		    cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

		    XtCallCallbacks (graphW, XmNselectNodeCallback, &cb);

		}
	    }
/* #################### addition - hjs -
  issue the select callback of the node even if previously selected */
            else
            if(graph->graph.indicated_widget == w &&
               graph->graph.current_action == POSSIBLE_DOUBLE_CLICK)
            {
                graph->graph.indicated_widget = NULL;
                graph->graph.current_action = NODE_SELECTED;

                if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNselectNodeCallback))
                { cb.event  = event;
                  cb.reason = XmCR_SELECT_NODE;
                  cb.widget = w;
                  cb.interactive = TRUE;

                  cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

                  cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);
                  XtCallCallbacks (graphW, XmNselectNodeCallback, &cb);

                }
            }
/* #################### end addition - hjs */
	}
	else 
	    if( (w =  XmGraphInputOverArc(graphW, event->xbutton.x,
						  event->xbutton.y)) != NULL) 
	    {
		if(graph->graph.indicated_widget == w &&  graph->graph.current_action == ARC_INDICATED) 
		{
		    if(graph->graph.selected_arcs.n_arcs || graph->graph.selected_nodes.n_nodes) 
		    {
			if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNdeselectCallback))
			{
			    cb.event  = event;
			    cb.reason = XmCR_DESELECT;
			    cb.widget = NULL;
			    cb.interactive = TRUE;

			    cb.selected_widgets  =   XmGraphGetSelectedNodes (graphW,  &cb.num_selected_widgets);

			    cb.selected_arcs  =  XmGraphGetSelectedArcs (graphW,  &cb.num_selected_arcs);

			    XtCallCallbacks (graphW, XmNdeselectCallback, &cb);

			}
			DeselectAllNodes(graphW);
			DeselectAllArcs(graphW);

		    }

		    XmGraphSelectArc(graphW, w);

		    graph->graph.indicated_widget = NULL;
		    graph->graph.current_action = ARC_SELECTED;

		    if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNselectArcCallback))
		    {
			cb.event  = event;
			cb.reason = XmCR_SELECT_ARC;
			cb.widget = (Widget) w;
			cb.interactive = TRUE;
			cb.selected_widgets = NULL;
			cb.num_selected_widgets = 0;

			cb.selected_widgets  =  XmGraphGetSelectedNodes (graphW,  &cb.num_selected_widgets);

			cb.selected_arcs  =  XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

			XtCallCallbacks (graphW, XmNselectArcCallback, &cb);

		    }
		}
/* #################### addition - hjs -
  issue the select callback of the arc even if previously selected */
               else
               if(graph->graph.indicated_widget == w &&
                  graph->graph.current_action == POSSIBLE_DOUBLE_CLICK)
               {
                  graph->graph.indicated_widget = NULL;
                  graph->graph.current_action = ARC_SELECTED;

                  if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNselectArcCallback))
                  {
                    cb.event  = event;
                    cb.reason = XmCR_SELECT_ARC;
                    cb.widget = (Widget)w;
                    cb.interactive = TRUE;
                    cb.selected_widgets = NULL;
                    cb.num_selected_widgets = 0;
                    cb.selected_widgets = XmGraphGetSelectedNodes(graphW, &cb.num_selected_widgets);
                    cb.selected_arcs = XmGraphGetSelectedArcs(graphW, &cb.num_selected_arcs);
                    XtCallCallbacks (graphW, XmNselectArcCallback, &cb);

                  }
               }
/* #################### end addition - hjs */
	    }
	graph->graph.current_action = NORMAL;
    }
}

/*******************************************************************************
 *  Routines for handling Double Clicks
 *  IsDoubleClick() and HandleDoubleClcik
 *
 ******************************************************************************/

static Boolean IsDoubleClick
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent * event;
#endif
{
    if(graph->graph.last_button_time && 
       (event->xbutton.time - graph->graph.last_button_time) < graph->graph.double_click_interval) 
    {
	graph->graph.last_button_time = 0;
	return TRUE;
    }
    graph->graph.last_button_time = event->xbutton.time;
  
    return FALSE;
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void    HandleDoubleClick
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{ Widget graphW =(Widget)graph;
    Widget w;
    XmGraphCallbackStruct    cb;

    if(XtCallbackHasSome != XtHasCallbacks(graphW, XmNdefaultActionCallback))
	return;

    if (event->xbutton.subwindow)
    { 
	w = XtWindowToWidget (XtDisplay(graph),
			      event->xbutton.subwindow);

	graph->graph.indicated_widget = NULL;

	cb.event  = event;
	cb.reason = XmCR_NODE_DOUBLE_CLICK;
	cb.widget = w;
	cb.interactive = TRUE;
	cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

	cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

	XtCallCallbacks (graphW, XmNdefaultActionCallback, &cb);

    } else
#if (XmREVISION==1)
    if( (w =  (Widget) _XmInputInGadget ((CompositeWidget)graph,
					       event->xbutton.x,
					       event->xbutton.y)) != NULL) 
#else
    if( (w =  (Widget) _XmInputInGadget ((Widget)graph,
					       event->xbutton.x,
					       event->xbutton.y)) != NULL) 
#endif
    {

	graph->graph.indicated_widget = NULL;

	cb.event  = event;
	cb.reason = XmCR_NODE_DOUBLE_CLICK;
	cb.widget = w;
	cb.interactive = TRUE;
	cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

	cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

	XtCallCallbacks (graphW, XmNdefaultActionCallback, &cb);

    }
    else 
	if( (w =  XmGraphInputOverArc(graphW, event->xbutton.x,
					      event->xbutton.y)) != NULL) 
	{
	    graph->graph.indicated_widget = NULL;

	    cb.event  = event;
	    cb.reason = XmCR_ARC_DOUBLE_CLICK;
	    cb.widget = w;
	    cb.interactive = TRUE;
	    cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

	    cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

	    XtCallCallbacks (graphW, XmNdefaultActionCallback, &cb);

	}
	else 
	{
	    graph->graph.indicated_widget = NULL;

	    cb.event  = event;
	    cb.reason = XmCR_DOUBLE_CLICK;
	    cb.widget = w;
	    cb.interactive = TRUE;
	    cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

	    cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);


	    XtCallCallbacks (graphW, XmNdefaultActionCallback, &cb);
	}

}

/*******************************************************************************
 *  Routines for indicating/cancelling/selecting multiple discontiguous nodes and arcs
 *  AddIndicate(), AddSelect()
 *
 ******************************************************************************/

static void  AddIndicate
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{ Widget graphW = (Widget) graph;
    Widget w;

    if ( graph->graph.edit_mode && graph->graph.allow_multiple_selections) 
    {

	if (event->xbutton.subwindow ||
#if (XmREVISION==1)
	    (w =  (Widget) _XmInputInGadget ((CompositeWidget)graph,
					     event->xbutton.x, 
					     event->xbutton.y)) != NULL)
#else
	    (w =  (Widget) _XmInputInGadget ((Widget)graph,
					     event->xbutton.x, 
					     event->xbutton.y)) != NULL)
#endif
	{ 
	    if (event->xbutton.subwindow)
		w = XtWindowToWidget (XtDisplay(graph),
				      event->xbutton.subwindow);

	/* Make Addselect toggle a selection instead WJS 920721 */
	    if(XmGraphIsSelectedNode(graphW, w)) {
#if SESD
		graph->graph.current_action = NORMAL;
		XmGraphUnselectNode(graphW, w);
#else
		graph->graph.current_action = POSSIBLE_DOUBLE_CLICK;
#endif
		graph->graph.indicated_widget = NULL;
		return;
	    }

	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  IndicateCursor (graph));

	    graph->graph.indicated_widget = w;
	    graph->graph.current_action = NODES_INDICATED;
	    _XmHighlightBorder(w);

	}
	else 
	    if( (w =  XmGraphInputOverArc(graphW, event->xbutton.x,
						  event->xbutton.y)) != NULL) 
	    {
#if SESD
              /* Make Addselect toggle a selection instead WJS 920721 */
	       if(XmGraphIsSelectedArc(graphW, w)) 
		 { graph->graph.current_action = NORMAL;
		   XmGraphUnselectArc(graphW, w);
		   graph->graph.indicated_widget = NULL;
		   return;
		 }
#endif
		XDefineCursor (XtDisplay (graph), XtWindow (graph),  IndicateCursor (graph));
		graph->graph.indicated_widget = w;
		graph->graph.current_action = ARCS_INDICATED;
		_XmHighlightArc ((XmArcWidget)w);
	    }
    }
}
/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void    AddSelect
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{ Widget graphW =(Widget) graph;

    Widget w;
    XmGraphCallbackStruct cb;
    if ( graph->graph.edit_mode) {

	if(!graph->graph.indicated_widget)
	    return;

	if (event->xbutton.subwindow ||
#if (XmREVISION==1)
	    (w =  (Widget) _XmInputInGadget ((CompositeWidget)graph,
					     event->xbutton.x,
					     event->xbutton.y)) != NULL)
#else
	    (w =  (Widget) _XmInputInGadget ((Widget)graph,
					     event->xbutton.x,
					     event->xbutton.y)) != NULL)
#endif
	{ 

	    if (event->xbutton.subwindow)
		w = XtWindowToWidget (XtDisplay(graph),
				      event->xbutton.subwindow);

	    if(graph->graph.indicated_widget == w &&  graph->graph.current_action != NORMAL) 
	    {

		XmGraphSelectNode(graphW, w);

		graph->graph.current_action = NODES_SELECTED;

		if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNselectNodeCallback))
		{
		    cb.selected_widgets  = XmGraphGetSelectedNodes (graphW,  &cb.num_selected_widgets);

		    cb.selected_arcs  =   XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

		    cb.event  = event;
		    cb.reason = XmCR_SELECT_NODE;
		    cb.widget = w;
		    cb.interactive = TRUE;

		    XtCallCallbacks (graphW, XmNselectNodeCallback, &cb);

		}
	    }
	    graph->graph.indicated_widget = NULL;

	}
	else 
	    if( (w =  XmGraphInputOverArc(graphW, event->xbutton.x,
						  event->xbutton.y)) != NULL) 
	    {
		if(graph->graph.indicated_widget == w &&  graph->graph.current_action != NORMAL) 
		{
		    XmGraphSelectArc(graphW, w);

		    graph->graph.current_action = ARCS_SELECTED;

		    if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNselectNodeCallback))
		    {
			cb.event  = event;
			cb.reason = XmCR_SELECT_ARC;
			cb.widget = w;
			cb.interactive = TRUE;

			cb.selected_widgets  =  XmGraphGetSelectedNodes (graphW,  &cb.num_selected_widgets);

			cb.selected_arcs  =  XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

			XtCallCallbacks (graphW, XmNselectNodeCallback, &cb);

		    }
		}
		graph->graph.indicated_widget = NULL;
	    }
    }
}

/*******************************************************************************
 *  Routines for indicating/cancelling/selecting subtrees
 *  IndicateSubtree(), CancelSubtree(), SelectSubtree()
 *
 ******************************************************************************/

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void IndicateSubtree
#ifndef _NO_PROTO
  (Widget graphW, XEvent *event)
#else
  (graphW, event) Widget graphW; XEvent *event;
#endif
{ XmGraphWidget graph=(XmGraphWidget)graphW;
    NodePtr      node;
    int          i, n;
    NList        nodes;
    Widget       w = NULL;

    graph->graph.current_node = NULL;
    graph->graph.current_arc = NULL;
    graph->graph.current_subgraph = NULL;
    subgraph_nodes.n_nodes = 0;

    if ( graph->graph.edit_mode && graph->graph.allow_multiple_selections) {

	if (event->xbutton.subwindow || 
#if (XmREVISION==1)
	    (w = (Widget) _XmInputInGadget ((CompositeWidget)graph,
					    event->xbutton.x, 
					    event->xbutton.y)) != NULL) 
#else
	    (w = (Widget) _XmInputInGadget ((Widget)graph,
					    event->xbutton.x, 
					    event->xbutton.y)) != NULL) 
#endif
	{
	    if(event->xbutton.subwindow)
		w =  XtWindowToWidget (XtDisplay(graph),
				       event->xbutton.subwindow);

#if !SESD
 /*  Deleted so that it is possible to select a sub tree even when root node
     is already selected.  This seems to work WJS 920721.
  */
#else
	    if(XmGraphIsSelectedNode(graphW, w)) 
	    {
		graph->graph.current_action = NORMAL;
		graph->graph.indicated_widget = NULL;
		return;
	    }
#endif

	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  IndicateCursor (graph));

	    graph->graph.current_action = SUBGRAPH_INDICATED;

	    graph->graph.indicated_widget = w;
	    node = graph->graph.current_subgraph = NODEPTR(w);

	    GetSubgraphNodeList (node);

	    n = subgraph_nodes.n_nodes; nodes = subgraph_nodes.nodes;
	    XmGraphSelectNode(graphW, w);
	    SelectArcsWithSubgraph (graphW, node);

	    for (i = 0; i < n; i++) 
	    {
		XmGraphSelectNode(graphW, nodes[i]->widget);
		SelectArcsWithSubgraph (graphW, nodes[i]); 
	    }
	    graph->graph.current_action = SUBGRAPH_INDICATED;
	}
    }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void SelectArcsWithSubgraph 
#ifndef _NO_PROTO
  (Widget graphW, NodePtr node)
#else
  (graphW, node) Widget  graphW; NodePtr node;
#endif
{ 
    int         i, n_arcs = node->from_arcs.n_arcs, rank;
    XmArcWidget arc;
    Widget arcW;
    XmArcWidgetList  arcs = node->from_arcs.arcs;
    Widget      from, to;

    for (i = 0; i < n_arcs; i++) 
    {
	arc =arcs[i];
	arcW = (Widget) arc;
	rank = _sibling_rank (arc);
	from = arc->arc.from;
	to = arc->arc.to;
	if ((from != to) && XtIsManaged(to) && (rank == 0) && 
	    XtIsManaged(from)) 
	{
	    if(XmIsGadget (to) || XmIsGadget(from) || 
	       (to->core.mapped_when_managed && from->core.mapped_when_managed))
		XmGraphSelectArc(graphW, arcW);
	}
    }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void  CancelSubtree
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{ Widget graphW = (Widget) graph;

    int     i, n;
    NList   nodes;
    Widget  w = NULL;
    Widget  oldwidget  = graph->graph.indicated_widget;


    if ( graph->graph.edit_mode) 
    {
	if(!graph->graph.indicated_widget) 
	    return;

	if (event->xbutton.subwindow ||
#if (XmREVISION==1)
	    (w = (Widget) _XmInputInGadget ((CompositeWidget)graph,
					    event->xbutton.x, 
					    event->xbutton.y)) != NULL)
#else
	    (w = (Widget) _XmInputInGadget ((Widget)graph,
					    event->xbutton.x, 
					    event->xbutton.y)) != NULL)
#endif
	{ 
	    if(event->xbutton.subwindow)
		w = XtWindowToWidget (XtDisplay(graph),
				      event->xbutton.subwindow);

	    if(oldwidget != NULL && w != oldwidget && 
	       graph->graph.current_subgraph) 
	    {
		graph->graph.current_action = SUBGRAPH_INDICATED_PENDING_CANCEL;
		n = subgraph_nodes.n_nodes; nodes = subgraph_nodes.nodes;
		XmGraphUnselectNode(graphW, oldwidget);
		CancelArcsWithSubgraph (graph, NODEPTR(oldwidget));
		for (i = 0; i < n; i++) {
		    XmGraphUnselectNode(graphW, nodes[i]->widget);
		    CancelArcsWithSubgraph (graph, nodes[i]);
		}
	    }
	    else 
	    {
		graph->graph.current_action = SUBGRAPH_INDICATED;
		n = subgraph_nodes.n_nodes; nodes = subgraph_nodes.nodes;
		XmGraphSelectNode(graphW, oldwidget);
		SelectArcsWithSubgraph (graphW, NODEPTR(oldwidget));

		for (i = 0; i < n; i++)
		{
		    XmGraphSelectNode(graphW, nodes[i]->widget);
		    SelectArcsWithSubgraph (graphW, nodes[i]); 
		}
	    }
	}
	else  if (graph->graph.current_action == SUBGRAPH_INDICATED)
	{
	    n = subgraph_nodes.n_nodes; nodes = subgraph_nodes.nodes;
	    XmGraphUnselectNode(graphW, oldwidget);
	    CancelArcsWithSubgraph (graph, NODEPTR(oldwidget));
	    for (i = 0; i < n; i++) {
		XmGraphUnselectNode(graphW, nodes[i]->widget);
		CancelArcsWithSubgraph (graph, nodes[i]);
	    }
	}
    }
}
/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void CancelArcsWithSubgraph 
#ifndef _NO_PROTO
  (XmGraphWidget graph, NodePtr node)
#else
  (graph, node) XmGraphWidget   graph; NodePtr  node;
#endif
{   Widget graphW = (Widget) graph;
    int         i, n_arcs = node->from_arcs.n_arcs, rank;
    XmArcWidget arc;
    Widget arcW;
    XmArcWidgetList  arcs = node->from_arcs.arcs;
    Widget      from, to;

    for (i = 0; i < n_arcs; i++) 
    {
	arc  = arcs[i];
	arcW  = (Widget) arc;
	rank = _sibling_rank (arc);
	from = arc->arc.from;
	to   = arc->arc.to;
	if ((from != to) && XtIsManaged(to) && (rank == 0) && 
	    XtIsManaged(from)) 
	{
	    if(XmIsGadget (to) || XmIsGadget(from) || 
	       (to->core.mapped_when_managed && from->core.mapped_when_managed))
		XmGraphUnselectArc(graphW, arcW);
	}
    }
}
/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/


static void   SelectSubtree
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{ Widget graphW =(Widget)graph;


    int     i, n;
    NList   nodes;

    Widget  w;
    XmGraphCallbackStruct    cb;

    if ( graph->graph.edit_mode) {

	if (event->xbutton.subwindow ||
#if (XmREVISION==1)
	    (w =  (Widget) _XmInputInGadget ((CompositeWidget)graph,
						event->xbutton.x, 
					     	event->xbutton.y)) != NULL)
#else
	    (w =  (Widget) _XmInputInGadget ((Widget)graph,
						event->xbutton.x, 
					     	event->xbutton.y)) != NULL)
#endif
	{ 
	    if(event->xbutton.subwindow)
		w = XtWindowToWidget (XtDisplay(graph),
				      event->xbutton.subwindow);

	    if(graph->graph.indicated_widget == w) 
	    {

		if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNdeselectCallback))
		{
		    cb.event  = event;
		    cb.reason = XmCR_DESELECT;
		    cb.widget = NULL;
		    cb.interactive = TRUE;

		    cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

		    cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

		    XtCallCallbacks (graphW, XmNdeselectCallback, &cb);

		}
		DeselectAllNodes(graphW); 
		DeselectAllArcs(graphW);

		n = subgraph_nodes.n_nodes; nodes = subgraph_nodes.nodes;
		XmGraphSelectNode(graphW, w);
		SelectArcsWithSubgraph (graphW, NODEPTR(w)); 

		for (i = 0; i < n; i++) 
		{
		    XmGraphSelectNode(graphW, nodes[i]->widget);
		    SelectArcsWithSubgraph (graphW, nodes[i]); 
		}

		if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNselectSubgraphCallback))
		{
		    cb.event  = event;
		    cb.reason = XmCR_SELECT_SUBGRAPH;
		    cb.widget = w;
		    cb.interactive = TRUE;

		    cb.selected_widgets  =  XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

		    cb.selected_arcs  = XmGraphGetSelectedArcs (graphW,  &cb.num_selected_arcs);

		    XtCallCallbacks (graphW, XmNselectSubgraphCallback, &cb);
		}
	    }

	}
	graph->graph.current_node     = NULL;
	graph->graph.current_arc      = NULL;
	graph->graph.indicated_widget = NULL;
	graph->graph.current_subgraph = NULL;
	graph->graph.current_action =  NORMAL;
	subgraph_nodes.n_nodes = 0;
    }
}


/*******************************************************************************
 *  Routines for Repositioning individual nodes and arcs
 *  SelectForMotion(), Motion(), EndMotion()
 *
 ******************************************************************************/

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void   SelectForMotion
#ifndef _NO_PROTO
  (Widget graphW, XEvent *event)
#else
  (graphW, event) Widget graphW; XEvent *event;
#endif
{ XmGraphWidget graph= (XmGraphWidget) graphW;
    Widget      w = NULL;
    Widget arcW;
    XmArcWidget arc;
    int x = event->xbutton.x, y = event->xbutton.y;


    if ( graph->graph.edit_mode) {

	if (event->xbutton.subwindow ||
#if (XmREVISION==1)
	    (w = (Widget) _XmInputInGadget ((CompositeWidget)graph,
						event->xbutton.x, 
					    event->xbutton.y)) != NULL)
#else
	    (w = (Widget) _XmInputInGadget ((Widget)graph,
						event->xbutton.x, 
					    event->xbutton.y)) != NULL)
#endif
	{ 
	    if (!graph->graph.nodes_movable)
		return;

	    if (event->xbutton.subwindow)
		w = XtWindowToWidget (XtDisplay(graph),
				      event->xbutton.subwindow);


	    if(XmGraphIsSelectedNode (graphW, w) && graph->graph.selected_nodes.n_nodes > 1) {
		SelectMultipleForMotion(graph, event);
		return;
	    }

	    graph->graph.delta_x = event->xbutton.x - RX(w);
	    graph->graph.delta_y = event->xbutton.y - RY(w);
	    graph->graph.start_x = RX(w);
	    graph->graph.start_y = RY(w);

	    graph->graph.current_node = NODEPTR (w);

	    AddRectToList (graph,
			   graph->graph.start_x, graph->graph.start_y,
			   RWidth(w) - 1, RHeight(w) - 1);

	    SelectArcsWithNode (graph, graph->graph.current_node);
	    DrawRectList (graph);
	    DrawLineList (graph);

	    graph->graph.current_action = NODE_SELECTED_FOR_MOTION;
	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  MotionCursor (graph));
	}
	else if( (arcW =  XmGraphInputOverArc(graphW, event->xbutton.x,
						     event->xbutton.y)) != NULL) 
	{ 
#ifdef __sun
          /* 911104 mohammad - This variable is used for a workaround ahead.
                               See below for furthur details. */
	  XGCValues values;
#endif
	int d1, d2;
	  arc = (XmArcWidget) arcW;

	    /* d1 == distance to from node */

	    d1 = (arc->arc.from_x - x) * (arc->arc.from_x - x) + 
		 (arc->arc.from_y - y) * (arc->arc.from_y - y);

	    /* d2 == distance to to_node */

	    d2 = (arc->arc.to_x - x) *  (arc->arc.to_x - x) +  
		 (arc->arc.to_y - y) * (arc->arc.to_y - y);

	    if (d1 > d2) 
	    { /* change from <= so as to grab "other" node */
		graph->graph.start_x = arc->arc.from_x;
		graph->graph.start_y = arc->arc.from_y;
		XDefineCursor (XtDisplay (graph), XtWindow (graph),
			       IndicateChildCursor (graph));

	    } 
	    else 
	    {
		graph->graph.start_x = arc->arc.to_x;
		graph->graph.start_y = arc->arc.to_y;
		XDefineCursor (XtDisplay (graph), XtWindow (graph),
			       IndicateParentCursor (graph));
	    }

	    graph->graph.end_x  =  x;
	    graph->graph.end_y  =  y;

#ifdef __sun
          /* 911104 mohammad - This is a workaround for the current problem
                               with the Ghost arc bug CLLlg01539. When trying
                               to add a new Arc, there are residual traces
                               of the arc around. */
	  values.line_width = 2;
	  XChangeGC(XtDisplay(graph), graph->graph.xorgc, GCLineWidth, &values);

#endif          
	    XDrawLine (XtDisplay(graph), XtWindow(graph), 
		       graph->graph.xorgc, 
		       graph->graph.start_x,
		       graph->graph.start_y,
		       graph->graph.end_x,
		       graph->graph.end_y);

	    graph->graph.current_arc = arcW;

	    graph->graph.current_action = ARC_SELECTED_FOR_MOTION;
	}
    }

}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void   Motion
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{
    NodePtr  node = graph->graph.current_node;
    Widget   w    = (node ? node->widget : NULL);

    if ( graph->graph.edit_mode) 
    {

	if(graph->graph.current_action == MULTIPLE_NODES_SELECTED_FOR_MOTION) 
	{
	    MoveMultiple (graph, event);
	    return;
	}

	if (w && !XmIsArc(w))
	{ 

	    AddRectToList (graph,
			   graph->graph.start_x, graph->graph.start_y,
			   RWidth(w) - 1 , RHeight(w) - 1);

	    MoveArcsWithNode (graph, node);
	    DrawRectList (graph);
	    DrawLineList (graph);

	    graph->graph.start_x = event->xbutton.x - graph->graph.delta_x;
	    graph->graph.start_y = event->xbutton.y - graph->graph.delta_y;

	    /* draw new lines */
	    AddRectToList (graph,
			   graph->graph.start_x, graph->graph.start_y,
			   RWidth(w) - 1, RHeight (w) - 1);


	    MoveArcsWithNode (graph, node);
	    DrawRectList (graph);
	    DrawLineList (graph);

	}
	else if (graph->graph.current_arc)
	{
	    XDrawLine (XtDisplay(graph), XtWindow(graph), 
		       graph->graph.xorgc, 
		       graph->graph.start_x,
		       graph->graph.start_y,
		       graph->graph.end_x,
		       graph->graph.end_y);

	    graph->graph.end_x  =  event->xbutton.x; 
	    graph->graph.end_y =  event->xbutton.y;
	    /* now draw new line */
	    XDrawLine (XtDisplay(graph), XtWindow(graph), 
		       graph->graph.xorgc, 
		       graph->graph.start_x,
		       graph->graph.start_y,
		       graph->graph.end_x,
		       graph->graph.end_y);
	}
    }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void  EndMotion
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{ Widget graphW =(Widget) graph;

    if ( graph->graph.edit_mode) 
    {
#if SESD
	Region        region = VisibleGraphRegion(graph);  /* OK, freed */
#else
	Region        region = XCreateRegion();  /* OK, freed */
#endif
	int           i, n_arcs;
	NodePtr       node = graph->graph.current_node;
	Widget        w = (node ? node->widget : NULL);
	XmArcWidgetList    arcs;
	XmArcWidget   arc;
	Widget        arcW;
	XmGraphCallbackStruct    cb;

	if(graph->graph.current_action == MULTIPLE_NODES_SELECTED_FOR_MOTION) 
	{
	    EndMultipleMotion (graph, event);
	    return;
	}

	if (w && !XmIsArc(w))
	{ 
	    AddRectToList (graph,
			   graph->graph.start_x, graph->graph.start_y,
			   RWidth(w) - 1, RHeight(w) - 1);


	    MoveArcsWithNode (graph, node);

	    DrawRectList (graph);
	    DrawLineList (graph);

	    /* Erase arcs from old position */

	    _InitArcList(graph);  /* clear line list */
	    graph->graph.batch_drawing_mode = TRUE;

	    arcs = node->from_arcs.arcs;
	    n_arcs = node->from_arcs.n_arcs;
	    for (i = 0; i < n_arcs; i++) 
#if SESD
		_EraseArc (arcs[i]);
#else
	    {
		arc =  arcs[i];

		if (arc->arc.region && arc->arc.visible)
		    region = _AddRegionToRegion (region, arc->arc.region);

		_EraseArc (arc);
	    }
#endif

	    arcs = node->to_arcs.arcs; n_arcs = node->to_arcs.n_arcs;
	    for (i = 0; i < n_arcs; i++) 
#if SESD
		_EraseArc (arcs[i]);
#else
	    {
		arc =  arcs[i];

		if (arc->arc.region && arc->arc.visible)
		    region = _AddRegionToRegion (region, arc->arc.region);

		_EraseArc (arc);
	    }
#endif
#if (XmREVISION==1)
	    _XmMoveObject ((RectObj)w, 
			   graph->graph.start_x,
			   graph->graph.start_y); 
#else
	    _XmMoveObject (w, 
			   graph->graph.start_x,
			   graph->graph.start_y); 
#endif

	    _InitArcList(graph);  /* clear line list */
	    graph->graph.batch_drawing_mode = FALSE;

	    UpdateArcs(w, FALSE);

	    /* Redisplay arc in new position */

	    _InitArcList(graph);  /* clear line list */
	    graph->graph.batch_drawing_mode = TRUE;

	    arcs = node->from_arcs.arcs;
	    n_arcs = node->from_arcs.n_arcs;

	    for (i = 0; i < n_arcs; i++) 
	    {
		arc = arcs[i];
		arcW = (Widget) arc;
		arcW->core.visible = ArcVisibleInGraph(graph, arc);

		if(arcW->core.visible)
		    (* arcW->core.widget_class->core_class.expose) (arcW, NULL, NULL);
		else if (!arcW->core.visible)
		    FreeArcRegions(arc);
	    }

	    arcs = node->to_arcs.arcs; n_arcs = node->to_arcs.n_arcs;
	    for (i = 0; i < n_arcs; i++) 
	    {
		arc =  arcs[i];
		arcW = (Widget) arc;
		arcW->core.visible = ArcVisibleInGraph(graph, arc);

		if(arcW->core.visible)
		    (* arcW->core.widget_class->core_class.expose) (arcW, NULL, NULL);
		else if (!arcW->core.visible)
		    FreeArcRegions(arc);
	    }

	    /* refresh those underneath */

	    _RefreshGadgets(graph, region);

	    _InitArcList(graph);  /* clear line list */
	    graph->graph.batch_drawing_mode = FALSE;

	    _RefreshArcs (graph, region); 

	    AdjustSize(graph);  

	    graph->graph.current_node = NULL;
	    graph->graph.current_action = NODE_INDICATED;

	    if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNnodeMovedCallback))	
	    {
		cb.event  = event;
		cb.reason = XmCR_NODE_MOVED;
		cb.widget = w;
		cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

		cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

		cb.interactive = TRUE;
		XtCallCallbacks (graphW, XmNnodeMovedCallback, &cb);
	    }
  	    updateNodeStruct(w);

	}
	else  if ((arcW =  graph->graph.current_arc) != NULL)
	{
	    Widget    from, to;
	    arc = (XmArcWidget) arcW;

	    if(event->xbutton.subwindow)
		w = XtWindowToWidget (XtDisplay(graph), 
				      event->xbutton.subwindow);
	    else
#if (XmREVISION==1)
		w = (Widget) _XmInputInGadget ((CompositeWidget)graph, 
							event->xbutton.x,
					       event->xbutton.y);
#else
		w = (Widget) _XmInputInGadget ((Widget)graph, 
							event->xbutton.x,
					       event->xbutton.y);
#endif

	    XDrawLine (XtDisplay(graph), XtWindow(graph), 
		       graph->graph.xorgc, 
		       graph->graph.start_x,
		       graph->graph.start_y,
		       graph->graph.end_x,
		       graph->graph.end_y);

	    graph->graph.current_arc = NULL;

	    if(!w) return;

	    if ((arc->arc.to_x == graph->graph.start_x) &&
		(arc->arc.to_y == graph->graph.start_y)) 
	    {
		/* We are changing from_node */
		from = w;
		to = arc->arc.to ;
	    } 
	    else
	    { /* We are changing to_node */
		from = arc->arc.from;
		to = w;
	    }

	    if ((arc->arc.from == from) && (arc->arc.to == to))
		/* do nothing */
		return;

	    if(XtCallbackHasSome == XtHasCallbacks(arcW, XmNarcEditedCallback))	
	    {
		cb.event  = event;
		cb.reason = XmCR_ARC_EDITED;
		cb.widget = arcW;
		cb.old_from = (Widget) arc->arc.from;
		cb.old_to   = (Widget) arc->arc.to;
		cb.new_from = (Widget) from;
		cb.new_to   = (Widget) to;
		cb.doit                 = TRUE;
		cb.interactive          = TRUE; 
		cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

		cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

		XtCallCallbacks (arcW, XmNarcEditedCallback, &cb); 

		if(!cb.doit)
		    return;
	    }

	    if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNarcMovedCallback))
	    {
		cb.event  = event;
		cb.reason = XmCR_ARC_MOVED;
		cb.widget = arcW;
		cb.selected_widgets     = NULL;
		cb.num_selected_widgets = 0;
		cb.old_from             = (Widget) arc->arc.from;
		cb.old_to               = (Widget) arc->arc.to;
		cb.new_from             = (Widget) from;
		cb.new_to               = (Widget) to;
		cb.doit                 = TRUE;
		cb.interactive          = TRUE;  
		cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

		cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

		XtCallCallbacks (graphW, XmNarcMovedCallback, &cb); 

		if(!cb.doit)
		    return;
	    }

	    XmGraphMoveArc (graphW, arcW, from, to);

	    graph->graph.current_action = ARC_INDICATED;

	}
	XDestroyRegion(region);
    }
}

/*******************************************************************************
 *  Routines for Repositioning multiple nodes
 *  SelectMultipleForMotion(), MultipleMotion(), EndMultipleMotion()
 *
 ******************************************************************************/

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void SelectMultipleForMotion 
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget  graph; XEvent        *event;
#endif
{ 
    int      i;

    graph->graph.current_action = MULTIPLE_NODES_SELECTED_FOR_MOTION;

    XDefineCursor (XtDisplay (graph), XtWindow (graph),  MotionCursor (graph));

    graph->graph.current_node     = NULL;
    graph->graph.current_arc      = NULL;
    graph->graph.current_subgraph = NULL;

    graph->graph.start_x = event->xbutton.x;
    graph->graph.start_y = event->xbutton.y;

    graph->graph.delta_x = 0;
    graph->graph.delta_y = 0;

    for (i = 0; i < graph->graph.selected_nodes.n_nodes; i++) 
    {
	NodePtr node = graph->graph.selected_nodes.nodes[i];
	Widget  w    = node->widget;
	XmGraphConstraintPtr wconst = CONSTRAINTREC(w);

	wconst->delta_x = event->xbutton.x - RX(node->widget);
	wconst->delta_y = event->xbutton.y - RY(node->widget);

	wconst->old_x = RX(w);
	wconst->old_y = RY(w);

	AddRectToList (graph, RX(w), RY(w),  RWidth(w), RHeight(w));
	SelectArcsWithNode (graph, node);

    }

    DrawRectList (graph);
    DrawLineList (graph); 
}

static void MoveMultiple 
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget  graph; XEvent        *event;
#endif
{ 
    int      i;

    for (i = 0; i < graph->graph.selected_nodes.n_nodes; i++) 
    {
	NodePtr node = graph->graph.selected_nodes.nodes[i];
	Widget  w    = node->widget;
	AddRectToList (graph, RX(w), RY(w),  RWidth(w), RHeight(w));
	MoveArcsWithMultipleNodes (graph, node);
    }

    DrawRectList (graph);
    DrawLineList (graph);

    for (i = 0; i < graph->graph.selected_nodes.n_nodes; i++) 
    {
	NodePtr node = graph->graph.selected_nodes.nodes[i];
	Widget  w    = node->widget;
	XmGraphConstraintsRec * wconst = CONSTRAINTREC(w);

	RX(w)   =  event->xbutton.x - wconst->delta_x;
	RY(w)   =  event->xbutton.y - wconst->delta_y;
    }

    graph->graph.start_x = event->xbutton.x - graph->graph.delta_x;
    graph->graph.start_y = event->xbutton.y - graph->graph.delta_y;

    for (i = 0; i < graph->graph.selected_nodes.n_nodes; i++) 
    {
	NodePtr node = graph->graph.selected_nodes.nodes[i];
	Widget  w    = node->widget;

	AddRectToList (graph, RX(w), RY(w),  RWidth(w), RHeight(w));
	MoveArcsWithMultipleNodes (graph, node);
    }

    DrawRectList (graph);
    DrawLineList (graph);

}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/


/*************************************<->*************************************
 *  EndMultipleMotion
 *   
 *   
 *
 *************************************<->***********************************/


static void  EndMultipleMotion
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{   Widget graphW=(Widget)graph;
    int           j, i, n_arcs;
#if SESD
    Region        region = VisibleGraphRegion(graph);
#else
    Region        region = XCreateRegion();
#endif
    XmArcWidget   arc;
    XmGraphCallbackStruct  cb;

    for (i = 0; i < graph->graph.selected_nodes.n_nodes; i++) 
    {
	NodePtr node = graph->graph.selected_nodes.nodes[i];
	Widget  w    = node->widget;
	AddRectToList (graph, RX(w), RY(w),  RWidth(w), RHeight(w));
	MoveArcsWithMultipleNodes (graph, node);
    }

    DrawRectList (graph);
    DrawLineList (graph); 

    for (i = 0; i < graph->graph.selected_nodes.n_nodes; i++) 
    {
	NodePtr node = graph->graph.selected_nodes.nodes[i];
	Widget w = node->widget;
	XmGraphConstraintPtr wconst = CONSTRAINTREC(w);

	RX(w) =  wconst->old_x; 
	RY(w) =  wconst->old_y; 
    }

    _InitArcList(graph);
    graph->graph.batch_drawing_mode = TRUE;

    for (j = 0; j < graph->graph.selected_nodes.n_nodes; j++) 
    {
	NodePtr   node = graph->graph.selected_nodes.nodes[j];

	n_arcs = node->from_arcs.n_arcs;
	for (i = 0; i < n_arcs; i++) 
	{
	    arc = (XmArcWidget) node->from_arcs.arcs[i];
#if !SESD

	    if (arc->arc.region && arc->arc.visible) 
		region = _AddRegionToRegion (region, arc->arc.region);
#endif
	    _EraseArc (arc);
	}

	n_arcs = node->to_arcs.n_arcs;
	for (i = 0; i < n_arcs; i++) 
	{
	    arc = (XmArcWidget) node->to_arcs.arcs[i];

#if !SESD
	    if (arc->arc.region && arc->arc.visible)
		region = _AddRegionToRegion (region, arc->arc.region);
#endif
	    _EraseArc (arc);
	}
    }

    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = FALSE;

    for (j = 0; j < graph->graph.selected_nodes.n_nodes; j++) 
    {
	NodePtr   node = graph->graph.selected_nodes.nodes[j];
	Widget   w    = node->widget;
	XmGraphConstraintsRec * wconst = CONSTRAINTREC(w);

#if (XmREVISION==1)
	_XmMoveObject ((RectObj)w, graph->graph.start_x - wconst->delta_x ,  
		       graph->graph.start_y - wconst->delta_y);
#else
	_XmMoveObject (w, graph->graph.start_x - wconst->delta_x ,  
		       graph->graph.start_y - wconst->delta_y);
#endif


	UpdateArcs(w, FALSE);
    }

    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = TRUE;

    for (j = 0; j < graph->graph.selected_nodes.n_nodes; j++) 
    {
	NodePtr   node = graph->graph.selected_nodes.nodes[j];
	Widget    arcW;

	n_arcs = node->from_arcs.n_arcs;
	for (i = 0; i < n_arcs; i++) 
	{
	    arc = node->from_arcs.arcs[i];
	    arcW = (Widget) arc;
	    arc->core.visible = ArcVisibleInGraph(graph, arc);

	    if(arc->core.visible)
		(* arc->core.widget_class->core_class.expose) (arcW, NULL, NULL);
	    else if (!arc->core.visible)
		FreeArcRegions(arc);
	}

	n_arcs = node->to_arcs.n_arcs;
	for (i = 0; i < n_arcs; i++) 
	{
	    arc = node->to_arcs.arcs[i];
	    arcW = (Widget) arc;
	    arcW->core.visible = ArcVisibleInGraph(graph, arc);

	    if(arcW->core.visible)
		(* arcW->core.widget_class->core_class.expose) (arcW, NULL, NULL);
	    else if (!arcW->core.visible)
		FreeArcRegions(arc);
	}
    }


    graph->graph.current_node   = NULL;
    graph->graph.current_action = NODE_INDICATED;

    AdjustSize(graph); 

    _RefreshGadgets(graph, region);

    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = FALSE;

    _RefreshArcs (graph, region);  

    XDestroyRegion(region);

    if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNnodeMovedCallback))
    {
	cb.event                = event;
	cb.reason               = XmCR_NODES_MOVED;
	cb.interactive          = TRUE;

	cb.selected_widgets = XmGraphGetSelectedNodes (graphW,  &cb.num_selected_widgets);

	cb.selected_arcs =  XmGraphGetSelectedArcs (graphW,  &cb.num_selected_arcs);

	cb.widget   = (( 0 < cb.num_selected_widgets) ? cb.selected_widgets[0] : NULL);

	XtCallCallbacks (graphW, XmNnodeMovedCallback, &cb);
    }	
}

/**************************************************************************************
 *  Routines for Adding a New Node or Arc
 * StartAddNodeOrArc(), PositionNewNodeOrArc(), EndAddNodeOrArc()
 *
 *************************************************************************************/


/*************************************<->*************************************
 * StartAddNode
 *   
 *   
 *
 *************************************<->***********************************/

static void  StartAddNode
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{

    Widget w;

    if ( graph->graph.edit_mode) {


	if (event->xbutton.subwindow)
	{
	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  IndicateParentCursor (graph));

	    w = XtWindowToWidget(XtDisplay(graph),
				 event->xbutton.subwindow);

	    graph->graph.current_node = NODEPTR (w);

	    graph->graph.end_x = graph->graph.start_x = RX(w) + RWidth(w) / 2;
	    graph->graph.end_y = graph->graph.start_y = RY(w) + RHeight(w) / 2;


	    /* Now, draw the line */

	    XDrawLine (XtDisplay(graph), XtWindow(graph), 
		       graph->graph.xorgc, 
		       graph->graph.start_x,
		       graph->graph.start_y,
		       graph->graph.end_x,
		       graph->graph.end_y);

	    graph->graph.current_action = ADDING_ARC_IN_PARENT;
	} else
#if (XmREVISION==1)
	if( (w =  (Widget) _XmInputInGadget ((CompositeWidget)graph,
						event->xbutton.x,
						   event->xbutton.y)) != NULL) 
#else
	if( (w =  (Widget) _XmInputInGadget ((Widget)graph,
						event->xbutton.x,
						   event->xbutton.y)) != NULL) 
#endif
	{
	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  IndicateParentCursor (graph));

	    graph->graph.current_node = NODEPTR (w);

	    graph->graph.end_x = graph->graph.start_x = RX(w) + RWidth(w) / 2;
	    graph->graph.end_y = graph->graph.start_y = RY(w) + RHeight(w) / 2;


	    /* Now, draw the line */

	    XDrawLine (XtDisplay(graph), XtWindow(graph), 
		       graph->graph.xorgc, 
		       graph->graph.start_x,
		       graph->graph.start_y,
		       graph->graph.end_x,
		       graph->graph.end_y);
	    graph->graph.current_action = ADDING_ARC_IN_PARENT;

	}
	else  
	{
	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  MotionCursor (graph));

	    graph->graph.start_x = event->xbutton.x;
	    graph->graph.start_y = event->xbutton.y;
	    /* Draw first outline */
	    XDrawRectangle (XtDisplay(graph), XtWindow(graph), 
			    graph->graph.xorgc, 
			    graph->graph.start_x,
			    graph->graph.start_y,
			    nn_width, nn_height);

	    graph->graph.current_action = ADDING_NODE;
	}
    }
}

/*************************************<->*************************************
 * PositionNewNode
 *   
 *   
 *
 *************************************<->***********************************/

static void   PositionNewNode
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{
    if ( graph->graph.edit_mode) {

	if (graph->graph.current_action == ADDING_NODE)
	{ 
	    XDrawRectangle (XtDisplay(graph), XtWindow(graph), 
			    graph->graph.xorgc, 
			    graph->graph.start_x,
			    graph->graph.start_y,
			    nn_width, nn_height);

	    graph->graph.start_x  =  event->xbutton.x;
	    graph->graph.start_y  =  event->xbutton.y;

	    XDrawRectangle (XtDisplay(graph), XtWindow(graph), 
			    graph->graph.xorgc, 
			    graph->graph.start_x,
			    graph->graph.start_y,
			    nn_width, nn_height);

	}

	else  if (graph->graph.current_action == ADDING_ARC ||
		  graph->graph.current_action == ADDING_ARC_IN_PARENT)

	{

	    if(graph->graph.current_action == ADDING_ARC_IN_PARENT)
	    {
		Widget w;

		if (event->xbutton.subwindow)
		    w = XtWindowToWidget(XtDisplay(graph),
					 event->xbutton.subwindow);
		else
#if (XmREVISION==1)
		    w =  (Widget) _XmInputInGadget ((CompositeWidget)graph,
							event->xbutton.x, 
						    event->xbutton.y);
#else
		    w =  (Widget) _XmInputInGadget ((Widget)graph,
							event->xbutton.x, 
						    event->xbutton.y);
#endif

		if(!w)
		{
		    XDefineCursor (XtDisplay (graph), XtWindow (graph),  
				   IndicateChildCursor (graph));
		    graph->graph.current_action = ADDING_ARC;
		}
	    }

	    XDrawLine (XtDisplay(graph), XtWindow(graph), 
		       graph->graph.xorgc, 
		       graph->graph.start_x,
		       graph->graph.start_y,
		       graph->graph.end_x,
		       graph->graph.end_y);

	    graph->graph.end_x =  event->xbutton.x; 
	    graph->graph.end_y =  event->xbutton.y;
	    /* now draw new line */
	    XDrawLine (XtDisplay(graph), XtWindow(graph), 
		       graph->graph.xorgc, 
		       graph->graph.start_x,
		       graph->graph.start_y,
		       graph->graph.end_x,
		       graph->graph.end_y);

	}
    }
}

/*************************************<->*************************************
 * EndAddNode
 *   
 *   
 *
 *************************************<->***********************************/

static void EndAddNode
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{ Widget graphW = (Widget)graph;

    Widget w;
    XmGadget g;
    XmGraphCallbackStruct    cb;

    if ( graph->graph.edit_mode) {

	if (graph->graph.current_action == ADDING_NODE)
	{
	    autoLayoutType old_layout_type = graph->graph.auto_layout_mode;

	    switch(graph->graph.auto_layout_mode)
	    {
	    case XmNODES_ONLY:
		graph->graph.auto_layout_mode = XmALWAYS;
		break;
	    case XmARCS_ONLY:
		graph->graph.auto_layout_mode = XmNEVER;
		break;
	    case XmPARTIAL:
		graph->graph.auto_layout_mode = XmNEVER;
		break;
	    case XmALWAYS:
	    case XmNEVER:
	    default:
		break;
	    }

	    graph->graph.current_action = NORMAL;

	    /* Erase last line */

	    XDrawRectangle (XtDisplay(graph), XtWindow(graph), 
			    graph->graph.xorgc, 
			    graph->graph.start_x,
			    graph->graph.start_y,
			    nn_width, nn_height);

	    /* Create and display new node */

	    w = DefaultNodeWidget (graphW, event->xbutton.x, event->xbutton.y);

	    if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNnewNodeCallback))
	    {
		cb.event  = event;
		cb.reason = XmCR_NEW_NODE;
		cb.widget = w;
		cb.selected_widgets = NULL;
		cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

		cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);
		cb.interactive = TRUE;
		cb.doit = TRUE;
		XtCallCallbacks(graphW, XmNnewNodeCallback, &cb);

		if (!cb.doit) 
		{
		    XtDestroyWidget((Widget) w);

		    graph->graph.auto_layout_mode = old_layout_type;

		    return;
		}
	    }

	    XtManageChild (w);

	    graph->graph.auto_layout_mode = old_layout_type;

	}
	else if (graph->graph.current_action == ADDING_ARC)
	{
	    Arg arglist[10];

	    autoLayoutType old_layout_type = graph->graph.auto_layout_mode;

	    switch(graph->graph.auto_layout_mode)
	    {
	    case XmNODES_ONLY:
		graph->graph.auto_layout_mode = XmNEVER;
		break;
	    case XmARCS_ONLY:
		graph->graph.auto_layout_mode = XmALWAYS;
		break;
	    case XmPARTIAL:
		graph->graph.auto_layout_mode = XmNEVER;
		break;
	    case XmALWAYS:
	    case XmNEVER:
	    default:
		break;
	    }

	    graph->graph.current_action = NORMAL;

	    XDrawLine (XtDisplay(graph), XtWindow(graph), 
		       graph->graph.xorgc, 
		       graph->graph.start_x,
		       graph->graph.start_y,
		       graph->graph.end_x,
		       graph->graph.end_y);

	    /* update coords */

	    graph->graph.end_x  =  event->xbutton.x;
	    graph->graph.end_y  =  event->xbutton.y;

	    if (event->xbutton.subwindow  || 
#if (XmREVISION==1)
		(g = _XmInputInGadget ((CompositeWidget)graph,
					event->xbutton.x, 
				       event->xbutton.y)) != NULL)
#else
		(g = _XmInputInGadget ((Widget)graph,
					event->xbutton.x, 
				       event->xbutton.y)) != NULL)
#endif
	    { /* We are on a node widget */
		int n = 0;
		XmArcWidget new;
		Widget new_w;
		NodePtr fnode = graph->graph.current_node;
		NodePtr tnode;

		graph->graph.current_node = NULL;

		if(event->xbutton.subwindow)
		    tnode = NODEPTR(XtWindowToWidget(XtDisplay (graph), event->xbutton.subwindow));
		else
		    tnode = NODEPTR((Widget) g);

		XtSetArg (arglist [n], XmNarcDirection, graph->graph.arc_style); n++;

		new = CreateArcInternal (graphW, NULL, fnode, tnode, arglist, n);
		new_w = (Widget) new;
		new->arc.up_to_date = FALSE;
		new->core.visible = TRUE;
		/* Update node's child and parent lists */

		NodeListInsert (&(fnode->kids), tnode);
		NodeListInsert (&(fnode->tree_kids), tnode);
		NodeListInsert (&(tnode->parents), fnode);

		if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNnewArcCallback))
		{
		    cb.event            = event;
		    cb.reason           = XmCR_NEW_ARC;
		    cb.widget           = new_w;
		    cb.new_from = (Widget) fnode->widget;
		    cb.new_to   = (Widget) tnode->widget;
		    cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

		    cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

		    cb.doit             = TRUE;
		    cb.interactive      = TRUE;
		    XtCallCallbacks (graphW, XmNnewArcCallback, &cb);

		    if(!cb.doit) 
		    {
			XtDestroyWidget( new_w);

			graph->graph.auto_layout_mode = old_layout_type;

			return;
		    }
		}
		/* Now display */

		XtManageChild(new_w);

		graph->graph.auto_layout_mode = old_layout_type;

		if(graph->graph.auto_layout_mode == XmPARTIAL)
		{
		    Widget poppa = fnode->widget;
		    XmGraphRelaySubgraph (graphW, poppa);
		}

	    }
	}

    }
}

/*************************************<->*************************************
 * GetSubgraphNodeList
 *   
 *   
 *
 *************************************<->***********************************/

/*  The GetSubgraphNodeList function used to have a "visited" node list
    as a parameter.  It was used to prevent an infinite loop when finding
    a subgraph with a cycle.  Unfortunatly GetSubgraphNodeList could still
    require an exponential amount of time to identify a sub-graph.  The
    new version below only requires n^2 time to identify a sub-graph.
    It could easily be made linear by using the visited field in the
    node record.
    SESD.
*/
static void GetSubgraphNodeList 
#ifndef _NO_PROTO
  (NodePtr node)
#else
  (node) NodePtr      node;
#endif
{ 
    NList      nodes = (node ? node->kids.nodes : NULL);
    int        i, n_nodes = (node ? node->kids.n_nodes : NULL);
    Widget     w = (node ? node->widget : NULL);

    /*
     * Insert those not yet in subgraph, mapped, and managed 
     */
    if (w && XtIsManaged(w) && 
	(XmIsGadget(w) || w->core.mapped_when_managed) &&
	 !NodeListMember(&subgraph_nodes, node))
	{
	    node->visited = True;
	    NodeListInsert (&(subgraph_nodes), node);
	    for (i = 0; i < n_nodes; i++) 
		GetSubgraphNodeList (nodes[i]);
	}
}


/**********
 *
 * FreeNode:
 * 
 **********/
static void FreeNode 
#ifndef _NO_PROTO
  (NodePtr node)
#else
  (node) NodePtr  node;
#endif
{
    /* Free node lists */
    NodeListFree (&(node->tree_kids));
    NodeListFree (&(node->parents));
    NodeListFree (&(node->kids));

    /* Free arc lists */
    ArcListFree (&(node->from_arcs));
    ArcListFree (&(node->to_arcs));

    /* Free the node structure */
    XtFree ((String) node);
}

/**********
 *
 * Destroy: frees up space consumed by graph and node structures, destroys
 *          all its arcs.
 * 
 ***********/
static void Destroy 
#ifndef _NO_PROTO
  (XmGraphWidget graph)
#else
  (graph) XmGraphWidget graph;
#endif
{ Widget graphW =(Widget) graph;

    XmGraphDestroyAllArcs (graphW);

    /* free the arc lists */
    ArcListFree (&(graph->graph.selected_arcs));

    ArcListFree (&return_widget_list);

    /* free the node lists */
    NodeListFree (&(graph->graph.selected_nodes));
    NodeListFree (&(graph->graph.user_roots));

    /* release graphic contexts */
    XtReleaseGC (graphW, graph->graph.gc);
    XtReleaseGC (graphW, graph->graph.xorgc);
    XtReleaseGC (graphW, graph->graph.cleargc);

    /* get rid of callbacks */
    XtRemoveAllCallbacks (graphW, XmNnewArcCallback);
    XtRemoveAllCallbacks (graphW, XmNnewNodeCallback);
    XtRemoveAllCallbacks (graphW, XmNnodeMovedCallback);
    XtRemoveAllCallbacks (graphW, XmNarcMovedCallback);
    XtRemoveAllCallbacks (graphW, XmNdefaultActionCallback);
    XtRemoveAllCallbacks (graphW, XmNselectNodeCallback);
    XtRemoveAllCallbacks (graphW, XmNdeselectCallback);
    XtRemoveAllCallbacks (graphW, XmNselectArcCallback);
    XtRemoveAllCallbacks (graphW, XmNselectSubgraphCallback);
}

/*************************************<->*************************************
 *
 * SetToNodeTreeParentToNullIfNotOnListOfFromNodesTreeKids
 *
 * Function designed to be certain that we do not set the tree_parent field
 * of the to_node (type _node) to NULL if the to_node is still on the
 * nlist.  Apparently, identical items can be added to a list.  When the
 * node is removed as a child then the corresponding field in the child's
 * record indicating the value of the parent should also be reset.  However,
 * if the child is listed twice as a child of the parent then the field in
 * the child should not be reset when only one of the entries in the list is
 * removed.
 *
 * 910314 coha:  creation
 *************************************<->***********************************/
#ifdef THIS_NAME_IS_TOO_LONG /* 12/16/91 I shorten it but keep original */
static void SetToNodeTreeParentToNullIfNotOnListOfFromNodesTreeKids
                                                               (nlist, to_node)
#endif /* THIS_NAME_IS_TOO_LONG */

static void SetToNodeTreeParent 
#ifndef _NO_PROTO
  (NodeList *nlist, NodePtr to_node)
#else
  (nlist, to_node) NodeList  *nlist; NodePtr   to_node;
#endif
{
    int    i, n_nodes;
    NList  nodes_list;

    /* Check to be sure nlist is not null. */
    if ( nlist != NULL )
    {
        n_nodes = nlist->n_nodes;
        nodes_list = nlist->nodes;
  
        /* Check to be sure nodes_list is not null. */
        if ( nodes_list != NULL )
        {
            /* Search for the to_node in the list of from node's tree kids */
            for (i = 0; (i < n_nodes) && (nodes_list[i] != to_node); i++);

            /* Only assign a null if either:
             *   1.  There are no nodes on the list.  ( i == 0; n_nodes == 0 )
             *       This implies that to_node was just removed from nlist.
             *   2.  The node is not on the list.  ( i++ until i == n_nodes )
             * If either the node is not on the list or there are no nodes on
             * the list then the value of i should be equal to n_nodes.
             */
            if (i == n_nodes)
              to_node->tree_parent = NULL;
        }
    }
}


/*************************************<->*************************************
 * DoRemoveArc
 *  
 *  
 *
 *************************************<->***********************************/

static void DoRemoveArc 
#ifndef _NO_PROTO
  (XmGraphWidget graph, XmArcWidget arc)
#else
  (graph, arc) XmGraphWidget  graph; XmArcWidget    arc;
#endif
{
    if (arc)
    {
	if(!graph->core.being_destroyed)
	{
	    int         i, n_siblings = 0;
	    int         rank   = _sibling_rank (arc);
	    WidgetList  list;
#if SESD
	    Region      region = VisibleGraphRegion(graph);
#else
	    Region      region = XCreateRegion();
#endif

	    _InitArcList(graph);  
	    graph->graph.batch_drawing_mode = TRUE;

	    _EraseArc (arc);

#if !SESD
	    region = _AddRegionToRegion (region, arc->arc.region);
#endif

	    if (arc->arc.siblings) 
	    {
		n_siblings = arc->arc.siblings->n_arcs;

		list = (WidgetList) XtMalloc(sizeof(Widget) * arc->arc.siblings->n_arcs);

		for (i = (rank + 1); i < arc->arc.siblings->n_arcs; i++)
		{
		    XmArcWidget arc_widget = (XmArcWidget) arc->arc.siblings->arcs[i];
		    list [i] = (Widget) arc_widget;

		    arc_widget->arc.up_to_date = FALSE;   /* ADDED */

		    _EraseArc (arc_widget);
		    region = _AddRegionToRegion (region, arc_widget->arc.region);
		}

		RemoveFromSiblings (arc);

		/* 
		 * reorder the old siblings 
		 */

		for (i = (rank + 1); i < n_siblings; i++) 
		    (* list[i]->core.widget_class->core_class.expose) (list[i], NULL, NULL);


		XtFree((char *) list);
	    }


	    _RefreshGadgets(graph, region);

	    _InitArcList(graph); 
	    graph->graph.batch_drawing_mode = FALSE;

	    _RefreshArcs (graph, region);

	    XDestroyRegion(region);
	}

	ArcListRemove (&(graph->graph.selected_arcs), arc);

	if(arc->arc.from && arc->arc.to )  
	{
	    NodePtr  from_node = NODEPTR (arc->arc.from);
	    NodePtr  to_node = NODEPTR (arc->arc.to);

	    /* 
	     * remove to_node from from_node's kid list 
	     */ 

	    NodeListRemove (&(from_node->kids), to_node);

	    /* 
	     * remove to_node from from_node's tree_kids list
	     */

	    if (to_node->tree_parent == from_node) 
	      {
		NodeListRemove (&(from_node->tree_kids), to_node);

	        /* 910314 coha:  Replace the following call:
        	 *   to_node->tree_parent = NULL;
                 * with the call to check to be certain that another entry of
                 * the to_node is not still on the list:
                 */
                SetToNodeTreeParent (&(from_node->tree_kids), to_node);
	    }

	    /* 
	     * remove from_node from to_node's parent list 
	     */

	    NodeListRemove (&(to_node->parents), from_node);

	    /* 
	     *  remove arc from from_node's from_arc's list 
	     */
	}
	if(arc->arc.from) 
	{
	    NodePtr  from_node = NODEPTR (arc->arc.from);

	    /* remove arc from to_node's to_arc's list */

	    ArcListRemove (&(from_node->from_arcs), arc);
	}
	if(arc->arc.to) 
	{
	    NodePtr  to_node = NODEPTR (arc->arc.to);

	    ArcListRemove (&(to_node->to_arcs), arc);

	}
    }
}

/**********
 *
 * ArcListFree: Free up all its space.
 *
 * 910612 coha:  Dan Myers found a problem that occurred because the value
 * of alist->arcs was not reset to NULL.  The symptom of the problem is that
 * selection followed by a close and a re-open, followed by another selection
 * will cause an abort of the application.  The abort is reported to occur
 * because of an attempt to realloc space referenced by the old value of 
 * alist->arcs.
 ***********/ 
static void ArcListFree 
#ifndef _NO_PROTO
  (ArcList *alist)
#else
  (alist) ArcList    *alist;
#endif
{
    if (alist->n_slots > 0)
	XtFree ((char *) alist->arcs);
    alist->n_arcs = alist->n_slots = 0;
    alist->arcs = NULL;                 /* 910612 coha:  added */
}

/**********
 *
 * ArcListInsert: inserts arc at end of alist.
 *
 ***********/ 
static void ArcListInsert 
#ifndef _NO_PROTO
  (ArcList *alist, XmArcWidget arc)
#else
  (alist, arc) ArcList  *alist; XmArcWidget   arc;
#endif
{ 

    int position = alist->n_arcs;
    XmArcWidgetList  arcs = alist->arcs;

    if (position == alist->n_slots) {

	/* Allocate more space */

	alist->n_slots += (alist->n_slots / 2) + 2;
	alist->arcs = arcs =
	    (XmArcWidgetList) XtRealloc ((caddr_t) arcs,
				    (unsigned) (alist->n_slots) * sizeof(Widget));
    }

    /* ripple arcs up one space from position */

    /*  for (i = alist->n_arcs; i > position; i--) {  How could this ever happen?
	arcs [i] = arcs [i-1];
	} */
    arcs [position] = arc;
    alist->n_arcs++;
}

/**********
 *
 * ArcListInsertNoDuplicates: Only inserts if arc is not already in alist.
 *
 ***********/ 
static Boolean ArcListInsertNoDuplicates 
#ifndef _NO_PROTO
  (ArcList *alist, XmArcWidget arc)
#else
  (alist, arc) ArcList  *alist; XmArcWidget   arc;
#endif
{
    int i, n_arcs = alist->n_arcs;
    XmArcWidgetList  arcs = alist->arcs;

    /* look for arc */

#if 0 /* CYY */
    if(arcs)
    for (i = 0; 
	 (arcs [i] != arc) && (i < n_arcs);)
    { ++i;  /* to make saber_c happy, no empty for loop */
    }
    if (!arcs || i == n_arcs || arcs[i] != arc)
	ArcListInsert (alist, arc);
#else
    if(arcs)
    { for (i = 0;  i< n_arcs; ++i)
      { if (arcs [i] == arc) return FALSE;
      }
    }
    ArcListInsert (alist, arc);
    return TRUE;
#endif
}

/**********
 *
 * ArcListRemove: 
 *
 ***********/ 
static void ArcListRemove 
#ifndef _NO_PROTO
  (ArcList *alist, XmArcWidget arc)
#else
  (alist, arc) ArcList  *alist; XmArcWidget    arc;
#endif
{
    XmArcWidgetList  arcs = alist->arcs;
    int         n_arcs = alist->n_arcs, i;

    if (n_arcs > 0) {
	/* find the arc */
	for (i = 0; (arcs [i] != arc) && (i < n_arcs); )
	{ i++; /* to make saber_c happy, no empty for loop */
	}
	/* remove it and move all arcs above it one space down */

	if (arcs [i] == arc) {
	    alist->n_arcs = --n_arcs;
	    while (i < n_arcs) {
		arcs [i] = arcs [i+1];
		i++;
	    }
	    arcs [n_arcs] = NULL;
	}
    }
}

/***********
 *
 * MakeSiblings: arc1 and arc2 are siblings if 
 *               ((arc1.from == arc2.from) &&
 *               (arc2.to == arc1.to)).
 *               Make both arcs point to the same sibling list.
 *               Insert new_arc in sibling list.
 *               If an arc has no siblings his sibling list
 *               will be NULL. Otherwise, it will contain
 *               itself and its siblings.
 *
 ***********/
static void MakeSiblings 
#ifndef _NO_PROTO
  (XmArcWidget sibling, XmArcWidget new_arc)
#else
  (sibling, new_arc) XmArcWidget  sibling, new_arc;
#endif
{
    /* Note that we use arc->core.self in the sibling list because
       if we simply use arc we run into problems when Redisplay is
       called from the Arc SetValues procedure (has to do with the
       fact that "current" and "new" are diff addresses) */

    if (sibling->arc.siblings == NULL) 
    {
	sibling->arc.siblings = (ArcList *) XtMalloc (sizeof (ArcList));
	sibling->arc.siblings->n_slots = 0;
	sibling->arc.siblings->n_arcs = 0;
	sibling->arc.siblings->arcs = NULL;
	ArcListInsert (sibling->arc.siblings, (XmArcWidget)sibling->core.self);
    }
    new_arc->arc.up_to_date = FALSE;   /* ADDED */
    sibling->arc.up_to_date = FALSE;   /* ADDED */
    ArcListInsertNoDuplicates (sibling->arc.siblings, (XmArcWidget)new_arc->core.self);
    new_arc->arc.siblings = sibling->arc.siblings;
}

/*********
 *
 * CheckForSiblings: Checks if arc has any siblings.
 *                   If not, do nothing. Otherwise, insert in sibling
 *                   list, make it point to this list.
 *
 *********/

static void CheckForSiblings 
#ifndef _NO_PROTO
  (XmArcWidget arc)
#else
  (arc) XmArcWidget      arc;
#endif
{
    XmArcWidget   next_arc;
    NodePtr     from = NODEPTR (arc->arc.from),
    to = NODEPTR (arc->arc.to);
    int         i, n_arcs;
    XmArcWidgetList  arcs;
    Boolean     done = False;

    arcs = from->from_arcs.arcs;
    n_arcs = from->from_arcs.n_arcs;

    for (i = 0; i < n_arcs; i++) 
    {
	next_arc = (XmArcWidget) arcs[i];
	if ((next_arc->arc.to == to->widget) &&
	    (next_arc != arc)) 
	{ /* found a sibling */
	    MakeSiblings (next_arc, arc); 
	    done = True;
	    break;
	}
    }
    /* This loop detects "reverse" siblings */
    if (!done) 
    {
	arcs = to->from_arcs.arcs;
	n_arcs = to->from_arcs.n_arcs;

	for (i = 0; i < n_arcs; i++) 
	{
	    next_arc = (XmArcWidget) arcs[i];
	    if ((next_arc->arc.to == from->widget) &&
		(next_arc != arc)) 
	    { /* a reverse sibling */
		MakeSiblings (next_arc, arc);
		break;
	    }
	}
    }
}

/***********
 *
 * RemoveFromSiblings: If sibling list is NULL do nothing.
 *                     If there are 2 siblings, arc is one of them,
 *                     therefore recycle list and make it NULL for both members.
 *                     Otherwise simply remove arc from the list.
 *
 ***********/

static void RemoveFromSiblings 
#ifndef _NO_PROTO
  (XmArcWidget arc)
#else
  (arc) XmArcWidget  arc;
#endif
{
    ArcList    *siblings = arc->arc.siblings;
    int         i;

    if (siblings != NULL) {
	if (siblings->n_arcs == 2) 
	{
	    for (i = 0; i < 2; i++) 
		((XmArcWidget) (siblings->arcs[i]))->arc.siblings = NULL;
	    XtFree ((char *) siblings->arcs);
	    XtFree ((char *) siblings);
	} 
	else 
	{
	    ArcListRemove (siblings, (XmArcWidget)arc->core.self);
	}
	arc->arc.siblings = NULL;
	arc->arc.up_to_date = FALSE;   /* ADDED */
    }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static XmArcWidget CreateArcInternal 
#ifndef _NO_PROTO
  (Widget graphW, char *name, 
				      NodePtr from, NodePtr to,
				      ArgList arglist, 
				      Cardinal nargs)
#else
  (graphW, name, from, to, arglist, nargs) Widget   graphW;
	char *name; NodePtr from, to; 
	ArgList  arglist; Cardinal  nargs;
#endif
{ Widget arcW = XtCreateWidget (name, xmArcWidgetClass, graphW,arglist, nargs); 
  XmArcWidget  arc = (XmArcWidget) arcW;

    /* Do not use XtSetValues for these because it will cause a Redisplay
       which might be fatal */

    arc->arc.from = from->widget;
    arc->arc.to = to->widget;

    /* check for siblings before inserting in arc lists */

    CheckForSiblings (arc);

    ArcListInsert (&(from->from_arcs), arc);
    ArcListInsert (&(to->to_arcs), arc); 

    return arc;
}

void _SetupArcInternal 
#ifndef _NO_PROTO
  (XmArcWidget arc)
#else
  (arc) XmArcWidget     arc;
#endif
{ NodePtr from = NODEPTR(arc->arc.from), to = NODEPTR(arc->arc.to);

    /* 
     * check for siblings before inserting in arc lists 
     */

    CheckForSiblings (arc);

    ArcListInsert (&(from->from_arcs), arc);
    ArcListInsert (&(to->to_arcs), arc); 

    /* 
     * Update  child and parent lists 
     */

    NodeListInsert (&(from->kids), to);
    NodeListInsert (&(from->tree_kids), to);
    NodeListInsert (&(to->parents), from);

}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void SelectArcsWithNode 
#ifndef _NO_PROTO
  (XmGraphWidget graph, NodePtr node)
#else
  (graph, node) XmGraphWidget graph; NodePtr       node;
#endif
{   int           i, n_arcs = node->from_arcs.n_arcs, rank;
    XmArcWidget   arc;
    XmArcWidgetList    arcs = node->from_arcs.arcs;
    Widget        from, to;

    for (i = 0; i < n_arcs; i++) 
    {
	arc  = (XmArcWidget) arcs[i];
	rank = _sibling_rank (arc);
	from = arc->arc.from;
	to   = arc->arc.to;
	arc->core.visible = TRUE;   /* Make sure it can be redrawn */
	if ((from != to) && XtIsManaged(to) && (rank == 0) && XtIsManaged(from))
        if(XmIsGadget(to) || XmIsGadget(from) || 
           (to->core.mapped_when_managed && from->core.mapped_when_managed))
		AddLineToList (graph, arc->arc.from_x, arc->arc.from_y,
			       arc->arc.to_x, arc->arc.to_y);
    }

    arcs = node->to_arcs.arcs; n_arcs = node->to_arcs.n_arcs;
    for (i = 0; i < n_arcs; i++) 
    {
	arc  = (XmArcWidget) arcs[i];
	rank = _sibling_rank (arc);
	from = arc->arc.from;
	to   = arc->arc.to;
	arc->core.visible = TRUE;   /* Make sure it can be redrawn */
	if ((from != to) && XtIsManaged(to) && (rank == 0) && XtIsManaged(from))
	    if(XmIsGadget(to) || XmIsGadget(from) || 
	       (to->core.mapped_when_managed && from->core.mapped_when_managed))
		AddLineToList (graph, arc->arc.from_x, arc->arc.from_y,
			       arc->arc.to_x, arc->arc.to_y);
    }
}



/*************************************<->*************************************
 * MoveArcsWithNode
 *   
 *   
 *
 *************************************<->***********************************/

static void MoveArcsWithNode 
#ifndef _NO_PROTO
  (XmGraphWidget graph, NodePtr node)
#else
  (graph, node) XmGraphWidget   graph; NodePtr         node;
#endif
{
    float         from_x, from_y, to_x, to_y;
    int           i, n_arcs = node->from_arcs.n_arcs, rank;
    XmArcWidget   arc;
    XmArcWidgetList    arcs = node->from_arcs.arcs;
    Widget        from, to;

    for (i = 0; i < n_arcs; i++) 
    {
	arc  = (XmArcWidget) arcs[i];
	rank = _sibling_rank (arc);
	from = arc->arc.from;
	to   = arc->arc.to;


	if ((from != to) && XtIsManaged(to) && (rank == 0) && XtIsManaged(from))

	    if(XmIsGadget (to) || XmIsGadget(from) || 
	       (to->core.mapped_when_managed && from->core.mapped_when_managed))
	    { Dimension bwTo = XtBorderWidth(to) + XtBorderWidth(to);
	      Dimension bwFrom = XtBorderWidth(from) + XtBorderWidth(from);
		_GetPoints (graph, graph->graph.start_x, graph->graph.start_y,
			    (int) RWidth(from)+bwFrom, 
			    (int) RHeight(from)+bwFrom, 
			    (int) RX(to),       (int) RY(to),
			    (int) RWidth(to)+bwTo,   
			    (int) RHeight(to)+bwTo,
			    &from_x, &from_y, &to_x, &to_y);

		AddLineToList (graph, (int) from_x, (int) from_y,
			       (int) to_x, (int) to_y);
	    }
    }

    arcs = node->to_arcs.arcs; n_arcs = node->to_arcs.n_arcs;
    for (i = 0; i < n_arcs; i++) 
    {
	arc  = (XmArcWidget) arcs[i];
	rank = _sibling_rank (arc);
	from = arc->arc.from;
	to   = arc->arc.to;
	arc->core.visible = TRUE;  /* make it displayable */

	if ((from != to) && XtIsManaged(to) && (rank == 0) &&  XtIsManaged(from))
	    if(XmIsGadget (to) || XmIsGadget(from) || 
	       (to->core.mapped_when_managed && from->core.mapped_when_managed))
	    { Dimension bwTo = XtBorderWidth(to) + XtBorderWidth(to);
	      Dimension bwFrom = XtBorderWidth(from) + XtBorderWidth(from);
		_GetPoints (graph, RX(from), RY(from), 
			    RWidth(from)+bwFrom, RHeight(from)+bwFrom,
			    graph->graph.start_x, 
			    graph->graph.start_y,
			    RWidth(to)+bwTo, 
			    RHeight(to)+bwTo,
			    &from_x, &from_y, &to_x, &to_y);


		AddLineToList (graph, (int) from_x, (int) from_y,
			       (int) to_x, (int) to_y);
	    }
    }
}

/*************************************<->*************************************
 * MoveArcsWithMultipleNodes
 *   
 *   
 *
 *************************************<->***********************************/

static void MoveArcsWithMultipleNodes 
#ifndef _NO_PROTO
  (XmGraphWidget graph, NodePtr node)
#else
  (graph, node) XmGraphWidget   graph; NodePtr         node;
#endif
{ Widget   graphW = (Widget)  graph;
    float         from_x, from_y, to_x, to_y;
    int           i, n_arcs = node->from_arcs.n_arcs, rank;
    XmArcWidget   arc;
    XmArcWidgetList    arcs = node->from_arcs.arcs;
    Widget        from, to;

    for (i = 0; i < n_arcs; i++) 
    {
	arc  = (XmArcWidget) arcs[i];
	rank = _sibling_rank (arc);
	from = arc->arc.from;
	to   = arc->arc.to;

	if( XmGraphIsSelectedNode(graphW, to) &&
	    XmGraphIsSelectedNode(graphW, from))
	    continue;

	if (((from != to) && XtIsManaged(to) && (rank == 0) && XtIsManaged(from)) &&
            (XmIsGadget (to) || XmIsGadget(from) || 
	     (to->core.mapped_when_managed && from->core.mapped_when_managed)))
	    { Dimension bwTo = XtBorderWidth(to) + XtBorderWidth(to);
	      Dimension bwFrom = XtBorderWidth(from) + XtBorderWidth(from);
		_GetPoints (graph, RX(from), RY(from),
			    RWidth(from)+bwFrom, RHeight(from)+bwFrom, 
			    RX(to), RY(to),
			    RWidth(to)+bwTo, RHeight(to)+bwTo,
			    &from_x, &from_y, &to_x, &to_y);

		AddLineToList (graph, (int) from_x, (int) from_y,
			       (int) to_x, (int) to_y);
	    }
    }

    arcs = node->to_arcs.arcs; n_arcs = node->to_arcs.n_arcs;
    for (i = 0; i < n_arcs; i++) 
    {
	arc  = (XmArcWidget) arcs[i];
	rank = _sibling_rank (arc);
	from = arc->arc.from;
	to   = arc->arc.to;
	arc->core.visible = TRUE;  /* make it displayable */

	if ((from != to) && XtIsManaged(to) && (rank == 0) &&  XtIsManaged(from))
	    if(XmIsGadget (to) || XmIsGadget(from) || 
	       (to->core.mapped_when_managed && from->core.mapped_when_managed))
	    { Dimension bwTo = XtBorderWidth(to) + XtBorderWidth(to);
	      Dimension bwFrom = XtBorderWidth(from) + XtBorderWidth(from);
	_GetPoints (graph, RX(from), RY(from), 
			RWidth(from)+bwFrom, RHeight(from)+bwFrom,
			    RX(to), RY(to),
			    RWidth(to)+bwTo, RHeight(to)+bwTo,
			    &from_x, &from_y, &to_x, &to_y);

		AddLineToList (graph, (int) from_x, (int) from_y,
			       (int) to_x, (int) to_y);
	    }
    }
}


/*************************************<->*************************************
 *
 *  ClassInitialize: Called once only, before first instance is created.
 *   ----------
 *
 *************************************<->***********************************/

static void ClassInitialize 
#ifndef _NO_PROTO
  (void)
#else
  ()
#endif
{
    XtAddConverter (XmRString, XmRArcDirection, _XmCvtStringToArcDirection, NULL, 0);
    XtAddConverter (XmRString, XmRLineStyle,    _XmCvtStringToLineStyle, NULL, 0);
    XtAddConverter (XmRString, XmRCapStyle,     _XmCvtStringToCapStyle, NULL, 0);
    XtAddConverter (XmRString, XmRArcDrawMode,  _XmCvtStringToArcDrawMode, NULL,0);
    XtAddConverter (XmRString, XmRAutoLayoutType,  _XmCvtStringToAutoLayoutType, NULL,0);


    nn_width = 60;
    nn_height = 30;
    line_list.n = 0;
    rectangle_list.n = 0;
    subgraph_nodes.n_nodes = 0;
    subgraph_nodes.n_slots = 0;
    subgraph_nodes.nodes = NULL;

    return_widget_list.n_arcs = return_widget_list.n_slots =  0;
    return_widget_list.arcs = NULL;

#ifndef _R4_
    _XmInitializeGetValuesResources (get_resources, XtNumber(get_resources));
#endif
    defaultAcceleratorsParsed = XtParseAcceleratorTable (defaultAccelerators);
}

/*************************************<->*************************************
 *
 * ResizeClipArea   
 *
 * 910405 coha:  There is a problem with all of the comparisons for width
 * and height in Motif 1.1 because the data type has been changed to unsigned
 * short.  This affects the compares that are done for ABS and MAX because
 * they will be done as unsigned compares.  The values returned by RWidth
 * and RHeight are also changed to unsigned.
 *************************************<->***********************************/

static void ResizeClipArea 
#ifndef _NO_PROTO
  (Widget clip, XtPointer client_data, XtPointer call_data)
#else
  (clip, client_data, call_data) Widget      clip;
	XtPointer   client_data,   call_data;
#endif
{
    Widget graphW = (Widget) client_data;
    XmGraphWidget   graph = (XmGraphWidget)  graphW;
#if 0
    Arg wargs[10];
    XmGraphCallbackStruct    cb;
#endif
    int i;
    Dimension oldWidth, oldHeight;

    oldWidth = graph->graph.clip_width;
    oldHeight = graph->graph.clip_height;

    graph->graph.clip_width  = RWidth(clip);
    graph->graph.clip_height = RHeight(clip);

/* HORRIBLE HACK ALERT:
    If we call adjust size everytime the clip window changes size, we get into 
a recursive loop as the cip window resizes in responce to scrollbars popping 
up and down.  So, only readjust things if the net change in size is greater than
 the width of the scrollbar area in either direction.
*/

/* CYY: what happens when we have ABS(-ve number) ? *****************
*********************************************************************
*  Take care of this by the macro ABS_DIM_DIFF as define above
*********************************************************************/
    
/* The "||" in the statement below was a "&&".  The AND prevents the graph from
   having its size adjusted unless the graph grows in BOTH directions.  SESD */
    if((ABS_DIM_DIFF(oldWidth, graph->graph.clip_width) > 
	ABS_DIM_DIFF(RWidth(XtParent(clip)), RWidth(clip))) ||
       (ABS_DIM_DIFF(oldHeight, graph->graph.clip_height) >  
	ABS_DIM_DIFF(RHeight(XtParent(clip)) , RHeight(clip))))
      AdjustSize(graph);

    XmUpdateDisplay(graphW);

#if 0
    if(RWidth(graph) < graph->graph.clip_width || 
       RHeight(graph) < graph->graph.clip_height) 
    {
	XtSetArg(wargs[0], XmNwidth, MAX(RWidth(graph), graph->graph.clip_width));
	XtSetArg(wargs[1], XmNheight, MAX(RHeight(graph), graph->graph.clip_height));
	XtSetValues(graphW, wargs, 2);
    }
#endif

    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = TRUE;

    {	Boolean visible;
	Widget  arcW;
	XmArcWidget  arc;

        for (i = 0; i < graph->graph.n_arcs; i++) 
       { arc =  graph->graph.arcs[i]; 
	 arcW =  (Widget) arc;

	visible = ArcVisibleInGraph(graph, arc);

	if(visible && !arc->core.visible) 
	{
	    arc->core.visible = TRUE;
	    (* arc->core.widget_class->core_class.expose) (arcW, NULL, NULL);
	}
	else if(!visible && arc->core.visible) 
	{
	    _EraseArc(arc);
	    FreeArcRegions(arc);
	    arc->core.visible = FALSE;
	}
      }
    }

    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = FALSE;

#if 0
    AdjustSize(graph);   /* Needed to update scrollbars */
#endif
}



static void GraphScrolled 
#ifndef _NO_PROTO
  ( Widget scrollbar, XtPointer graphPtr, XtPointer call_data)
#else
  (scrollbar, graphPtr, call_data)
	Widget scrollbar;
        XtPointer graphPtr;
        XtPointer call_data;
#endif
{
    int i;
    Boolean any_erased = FALSE;
    XmGraphWidget graph = (XmGraphWidget) graphPtr;
#if SESD
    Region region = VisibleGraphRegion(graph);
#else
    Region region = XCreateRegion();
#endif

    XFlush(XtDisplay(graph));

    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = TRUE;

    { Widget arcW;
      XmArcWidget  arc;
      Boolean      visible;

      for (i = 0; i < graph->graph.n_arcs; i++) 
      {
	arc =  graph->graph.arcs[i]; 
	arcW = (Widget) arc;
	visible  = ArcVisibleInGraph(graph, arc);

	if(visible && !arc->core.visible) 
	{
	    arc->core.visible = TRUE;
	    (* arc->core.widget_class->core_class.expose) (arcW, NULL, NULL);
	}
	else if (!visible && arc->core.visible) 
	{
	    any_erased = TRUE;
	    _EraseArc(arc);
#if !SESD
	    region = _AddRegionToRegion (region, arc->arc.region);
#endif
	    FreeArcRegions(arc);
	    arc->core.visible = FALSE;
	}
    }
    }

    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = FALSE;

    if(!graph->graph.show_crossing_arcs && any_erased)
    {
	_RefreshGadgets(graph, region);

	_RefreshArcs (graph, region);
    }

    XDestroyRegion(region);
}

/*************************************<->*************************************
 *
 *   Initialize: Called at instance creation time.
 *    ---------
 *
 *************************************<->***********************************/

static void Initialize 
#ifndef _NO_PROTO
  (Widget request_w, Widget new_w)
#else
  (request_w, new_w) Widget request_w, new_w;
#endif
{ XmGraphWidget request=(XmGraphWidget) request_w;
  XmGraphWidget newGraph=(XmGraphWidget) new_w;

    XGCValues   values;
    XtGCMask    valueMask;
    int         i;
    Arg         wargs [10];

    newGraph->graph.is_scrolled      = FALSE;
    newGraph->graph.last_button_time = 0;

    if(XtIsSubclass(XtParent(newGraph), xmScrolledWindowWidgetClass))
	/* Really weird, but it appears for now, 
           that the ScrolledWidget has not yet "reparented" the Graph widget.
	   So we do indeed want the parent here, not the grandparent */
    {
	Widget clip, hscroll, vscroll;

	newGraph->graph.is_scrolled = TRUE;

	XtSetArg(wargs[0], XmNclipWindow, &clip);
	XtSetArg(wargs[1], XmNverticalScrollBar, &vscroll);
	XtSetArg(wargs[2], XmNhorizontalScrollBar, &hscroll);
	XtGetValues(XtParent(newGraph), wargs, 3);

	if(clip)
	    XtAddCallback(clip, XmNresizeCallback, 
		       ResizeClipArea, newGraph);

	if(hscroll) 
	{
	    XtAddCallback(hscroll, XmNvalueChangedCallback, 
		       GraphScrolled, newGraph);
	    XtAddCallback(hscroll, XmNincrementCallback, 
		       GraphScrolled, newGraph);
	    XtAddCallback(hscroll, XmNdecrementCallback, 
		       GraphScrolled, newGraph);
	    XtAddCallback(hscroll, XmNpageIncrementCallback, 
		       GraphScrolled, newGraph);
	    XtAddCallback(hscroll, XmNpageDecrementCallback, 
		       GraphScrolled, newGraph);
	    XtAddCallback(hscroll, XmNdragCallback, 
		       GraphScrolled, newGraph);
#if SESD
	    XtSetArg(wargs[0], XmNincrement, SCROLLBAR_INCREMENT);
            XtSetValues(hscroll, wargs, 1);
#endif
	}
	if(vscroll) 
	{
	    XtAddCallback(vscroll, XmNvalueChangedCallback, 
		       GraphScrolled, newGraph);
	    XtAddCallback(vscroll, XmNincrementCallback, 
		       GraphScrolled, newGraph);
	    XtAddCallback(vscroll, XmNdecrementCallback, 
		       GraphScrolled, newGraph);
	    XtAddCallback(vscroll, XmNpageIncrementCallback, 
		       GraphScrolled, newGraph);
	    XtAddCallback(vscroll, XmNpageDecrementCallback, 
		       GraphScrolled, newGraph);
	    XtAddCallback(vscroll, XmNdragCallback, 
		       GraphScrolled, newGraph);
#if SESD
	    XtSetArg(wargs[0], XmNincrement, SCROLLBAR_INCREMENT);
            XtSetValues(hscroll, wargs, 1);
#endif
	}
    }

    XtAddEventHandler(new_w, SubstructureNotifyMask, FALSE, HandleCreateNotify, NULL);

#if 0
    PtrCursor (newGraph)      = XCreateFontCursor (XtDisplay (newGraph), XC_left_ptr);
    MotionCursor (newGraph)   = XCreateFontCursor (XtDisplay (newGraph), XC_fleur);
    IndicateCursor (newGraph) = XCreateFontCursor (XtDisplay (newGraph), XC_crosshair);
#endif
    HighRightCursor(newGraph) = XCreateFontCursor (XtDisplay (newGraph), XC_top_right_corner);
    HighLeftCursor (newGraph) = XCreateFontCursor (XtDisplay (newGraph), XC_top_left_corner);
    LowRightCursor(newGraph)  = XCreateFontCursor (XtDisplay (newGraph), XC_bottom_right_corner);
    LowLeftCursor (newGraph)  = XCreateFontCursor (XtDisplay (newGraph), XC_bottom_left_corner);

    if(!IndicateChildCursor(newGraph))
	IndicateChildCursor(newGraph)=CreateCursor(new_w, c_bits, c_mask_bits,
						 c_width, c_height,
						 c_x_hot, c_y_hot);
    if(!IndicateParentCursor(newGraph))
	IndicateParentCursor(newGraph)=CreateCursor (new_w, p_bits, p_mask_bits,
						  p_width, p_height, 
						  p_x_hot, p_y_hot);

    if(!newGraph->graph.default_widget_class)
	newGraph->graph.default_widget_class = xmPushButtonGadgetClass;

    newGraph->graph.original_sibling_spacing =newGraph->graph.sibling_spacing;  /* Save the unconverted spacing */
    newGraph->graph.original_child_spacing =newGraph->graph.child_spacing; 

    if (newGraph->manager.unit_type != XmPIXELS)
    {

#ifdef _R4_
	if(newGraph->graph.direction == XmHORIZONTAL)
	{
	    newGraph->graph.child_spacing = (int)
		XmCvtToHorizontalPixels(XtScreen(newGraph), 
					newGraph->graph.original_child_spacing,
					newGraph->manager.unit_type);

	    newGraph->graph.sibling_spacing = (int)
		XmCvtToVerticalPixels(XtScreen(newGraph), 
				      newGraph->graph.original_sibling_spacing,
				      newGraph->manager.unit_type);
	}

	else if(newGraph->graph.direction == XmVERTICAL)
	{
	    newGraph->graph.child_spacing = (int)
		XmCvtToVerticalPixels(XtScreen(newGraph), 
				      newGraph->graph.original_child_spacing,
				      newGraph->manager.unit_type);


	    newGraph->graph.sibling_spacing = (int)
		XmCvtToHorizontalPixels(XtScreen(newGraph), 
					newGraph->graph.original_sibling_spacing,
					newGraph->manager.unit_type);

	}
#else
	if(newGraph->graph.direction == XmHORIZONTAL)
	{
	    newGraph->graph.child_spacing = (int)
		_XmToHorizontalPixels(newGraph, 
					newGraph->manager.unit_type,
					newGraph->graph.original_child_spacing);

	    newGraph->graph.sibling_spacing = (int)
		_XmToVerticalPixels(newGraph, 
				      newGraph->manager.unit_type,
				      newGraph->graph.original_sibling_spacing);
	}

	else if(newGraph->graph.direction == XmVERTICAL)
	{
	    newGraph->graph.child_spacing = (int)
		_XmToVerticalPixels(newGraph, 
				      newGraph->manager.unit_type,
				      newGraph->graph.original_child_spacing);


	    newGraph->graph.sibling_spacing = (int)
		_XmToHorizontalPixels(newGraph, 
					newGraph->manager.unit_type,
					newGraph->graph.original_sibling_spacing);

	}
#endif
    }


    if (XtWidth(request) == 0)
	newGraph->core.width = 50;
    if (XtHeight(request) == 0)
	newGraph->core.height = 50;

    if (request->graph.auto_layout_mode != XmNEVER)
    {
	newGraph->graph.layed_out = False;
    } 
    else
    {
	newGraph->graph.layed_out = True;
    }

    if (request->graph.siblings_visible != False)
	newGraph->graph.siblings_visible = True;
    else
	newGraph->graph.siblings_visible = False;

    { Widget dummy;
      i = 0;
      XtSetArg(wargs[i], XmNwidth,  nn_width); i++; 
      XtSetArg(wargs[i], XmNheight, nn_height); i++; 
      XtSetArg(wargs[i], XmNmappedWhenManaged, FALSE); i++; 

      dummy = XtCreateWidget("dummy", xmPushButtonWidgetClass, new_w, wargs, i);

      newGraph->graph.root = NODEPTR(dummy);

    }
    newGraph->graph.current_action = NORMAL;
    newGraph->graph.last_button_time = 0;

    newGraph->graph.arcs         = NULL;
    newGraph->graph.current_arc  = NULL;
    newGraph->graph.current_node = NULL;
    newGraph->graph.n_arc_slots  = 0;
    newGraph->graph.n_arcs  = 0;
    newGraph->graph.n_nodes = 0;
    newGraph->graph.start_x = 0;
    newGraph->graph.start_y = 0;
    newGraph->graph.end_x   = 0;
    newGraph->graph.end_y   = 0;
    newGraph->graph.user_roots.nodes   = NULL;
    newGraph->graph.user_roots.n_nodes = 0;
    newGraph->graph.user_roots.n_slots = 0;
    newGraph->graph.selected_nodes.nodes   = NULL;
    newGraph->graph.selected_nodes.n_nodes = 0;
    newGraph->graph.selected_nodes.n_slots = 0;
    newGraph->graph.selected_arcs.arcs     = NULL;
    newGraph->graph.selected_arcs.n_arcs   = 0;
    newGraph->graph.selected_arcs.n_slots  = 0;
    newGraph->graph.current_arc      = NULL;
    newGraph->graph.highlighted_arc  = NULL;
    newGraph->graph.indicated_widget = NULL;
    newGraph->graph.current_subgraph = NULL;
    newGraph->graph.current_node     = NULL;

#if SESD
    newGraph->graph.deferedChangeManaged = FALSE;
#endif

/* 
 * Set the graphicContext 
 */

    valueMask         = GCForeground | GCBackground;
    values.foreground = newGraph->manager.foreground;
    values.background = newGraph->core.background_pixel;
    newGraph->graph.gc = XtGetGC ((Widget) newGraph, valueMask, &values);  

/* 
 * Set the XOR graphic Context 
 */

    valueMask |= GCForeground | GCFunction | GCLineStyle | GCSubwindowMode;
    values.foreground     = newGraph->manager.foreground ^ newGraph->core.background_pixel;
    values.function       = GXxor;
    values.line_style = LineOnOffDash;

    values.subwindow_mode = IncludeInferiors;
    newGraph->graph.xorgc = XtGetGC ((Widget) newGraph, valueMask, &values);  

/* 
 * Set the CLEAR graphic Context 
 */

    valueMask = GCForeground | GCBackground | GCFunction;
    values.function    = GXcopy;
    values.foreground  = newGraph->core.background_pixel;
    values.background  = newGraph->core.background_pixel;
    newGraph->graph.cleargc = XtGetGC((Widget) newGraph, valueMask, &values);

    if (newGraph->core.accelerators == NULL)
    {
    newGraph->core.accelerators = defaultAcceleratorsParsed;
    newGraph->manager.accelerator_widget = new_w;
    } 
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void HandleCreateNotify 
#ifndef _NO_PROTO
  (Widget graphW, XtPointer client_data, XEvent *event, Boolean *cont)
#else
  (graphW, client_data, event, cont)
	Widget          graphW;
	XtPointer              client_data;
	XEvent                 *event;
	Boolean                *cont;
#endif
{ XmGraphWidget graph = (XmGraphWidget) graphW;
    if(event->type == CreateNotify) 
    {
	Widget w = XtWindowToWidget(XtDisplay(graph), event->xcreatewindow.window);

	if ( graph->graph.edit_mode ) 
	    MakeEditable(w);
	else 
	    MakeReadOnly(w);
    }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void HandleButtonPress 
#ifndef _NO_PROTO
  (Widget graphW, XEvent *event)
#else
  (graphW, event) Widget graphW; XEvent  *event;
#endif
{ XmGraphWidget      graph=(XmGraphWidget)      graphW;
    Widget w;

    if ( !graph->graph.edit_mode)

#if (XmREVISION==1)
	if( (w =  (Widget) _XmInputInGadget ((CompositeWidget)graph,
						event->xbutton.x,
					     event->xbutton.y)) != NULL) 
#else
	if( (w =  (Widget) _XmInputInGadget ((Widget)graph,
						event->xbutton.x,
					     event->xbutton.y)) != NULL) 
#endif
	{
#if (XmREVISION==1)
	    _XmDispatchGadgetInput ((XmGadget)w, event, XmARM_EVENT);
#else
	    _XmDispatchGadgetInput (w, event, XmARM_EVENT);
#endif
	    graph->manager.selected_gadget = (XmGadget) w;
	}
	else 
	    if( (w =  XmGraphInputOverArc(graphW, event->xbutton.x,
						  event->xbutton.y)) != NULL) 
	    {
		XmDispatchArcInput (w, event, XmARM_EVENT);
		graph->manager.selected_gadget = (XmGadget) w;
	    }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void HandleButtonMotion 
#ifndef _NO_PROTO
  (Widget graphW, XEvent *ev)
#else
  (graphW, ev) Widget graphW; XEvent    *ev;
#endif
{ XmGraphWidget graph=( XmGraphWidget) graphW;
  XPointerMovedEvent    *event = (XPointerMovedEvent *)ev;
    XmArcWidget arc;
    XmArcWidget oldArc;

    if ( graph->graph.edit_mode)
	return;

    if (event->subwindow != 0)
	return;


    arc = (XmArcWidget) XmGraphInputOverArc (graphW, event->x, event->y);
    oldArc = (XmArcWidget) graph->graph.highlighted_arc;

    if(!graph->manager.selected_gadget || 
       (arc && arc != (XmArcWidget) graph->manager.selected_gadget))
	return;


    /*  Dispatch motion events to the child  */

    if (oldArc != NULL && arc != oldArc)
    {
	XmDispatchArcInput ((Widget)oldArc, ev, XmLEAVE_EVENT);

	graph->graph.highlighted_arc = NULL;
    }  

    if (arc != NULL && arc != oldArc)
    {
	XmDispatchArcInput ((Widget)arc, ev, XmENTER_EVENT);
	graph->graph.highlighted_arc = (Widget) arc;
    }
}

static void HandleButtonRelease
#ifndef _NO_PROTO
  (Widget graphW, XEvent *event)
#else
  (graphW, event) Widget          graphW; XEvent *event;
#endif
{ XmGraphWidget graph = (XmGraphWidget) graphW;
    if ( !graph->graph.edit_mode)
	if ( (graph->manager.selected_gadget != NULL) &&
	     (!XmIsArc((Widget)(graph->manager.selected_gadget))))
	{
#if (XmREVISION==1)
	    _XmDispatchGadgetInput (graph->manager.selected_gadget, event,
			 XmACTIVATE_EVENT);
#else
	    _XmDispatchGadgetInput ((Widget)graph->manager.selected_gadget,
		event, XmACTIVATE_EVENT);
#endif


	    graph->manager.selected_gadget = NULL;
	}
	else 
	    if ( (graph->manager.selected_gadget != NULL) && 
		 (XmIsArc((Widget)(graph->manager.selected_gadget))) )
	    {
		XmDispatchArcInput ((Widget) graph->manager.selected_gadget, event, XmACTIVATE_EVENT);

		graph->manager.selected_gadget = NULL;
		graph->graph.highlighted_arc = (Widget) NULL;
	    }
}

/*
 * DefaultNodeWidget creates the widget to insert as a new node.
 */
static Widget 	DefaultNodeWidget
#ifndef _NO_PROTO
  (Widget graphW, int x, int y)
#else
  (graphW, x, y) Widget   graphW; int             x, y;
#endif
{ XmGraphWidget   graph = (XmGraphWidget)   graphW;
  Widget w;
  Arg    wargs[10];
  int    n = 0;

    /* 
     * Note that w is not yet managed
     */

    w = XtCreateWidget(DEFAULT_LABEL, graph->graph.default_widget_class, graphW, wargs, n);

#if (XmREVISION==1)
    _XmMoveObject((RectObj)w, x, y);
#else
    _XmMoveObject(w, x, y);
#endif

    UpdateArcs(w, FALSE);

    nn_width  = RWidth(w);
    nn_height = RHeight(w);
    return w;
}

/*************************************************************************
 *
 * Layout algorithm routines.
 * Description:  Layout Algorithm for GRAPH Widget
 * 
 **************************************************************************/

static Boolean NodeListMember 
#ifndef _NO_PROTO
  (NodeList *n_list, NodePtr node)
#else
  (n_list, node) NodeList    *n_list; NodePtr      node;
#endif
{
    NList  nodes = n_list->nodes;
    int    i, n_nodes = n_list->n_nodes;

#if 0
    /* look for node */
    for (i = 0; (i < n_nodes) && (nodes [i] != node);)
    { i++; /* keep saber_c happy about empty for loop */
    }

    if(n_nodes <= i)/* went off end of list  SESD (used to read n_nodes==0)*/
	return(FALSE);

    return (nodes [i] == node);
#endif
    for (i = 0; (i < n_nodes) ;i++)
    { if (nodes [i] == node) return TRUE;
    }
    return FALSE;
}


/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void updateNodeDepth 
#ifndef _NO_PROTO
  (NodePtr node)
#else
  (node) NodePtr node;
#endif
{
    NList nodes = node->tree_kids.nodes;
    int   n = node->tree_kids.n_nodes, i;

    node->level++;
    for (i = 0; i < n; i++) 
         if(!XmBeingDestroyed(nodes[i]->widget))
	    updateNodeDepth (nodes[i]);
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void updateNodeParent 
#ifndef _NO_PROTO
  (NodePtr node, NodePtr new_parent)
#else
  (node, new_parent) NodePtr  node, new_parent;
#endif
{
    NodePtr  old_parent = node->tree_parent;

    NodeListRemove (&(old_parent->tree_kids), node);
    node->tree_parent = new_parent;
}

/*
 * DoConvertToTree: actual conversion from a GRAPH to a Tree.
 */
static void doConvertToTree 
#ifndef _NO_PROTO
  (NodeList *visited, NodePtr node, NodePtr parent, int level)
#else
  (visited, node, parent, level) NodeList  *visited;
	NodePtr   node, parent; int       level;
#endif
{
    NList      nodes = node->kids.nodes;
    int        n = node->kids.n_nodes, i;

    node->tree_parent = parent;
    node->level = level;


    if (!NodeListMember (visited, node) && !XmBeingDestroyed(node->widget)) 
    {
	NodeListInsert (visited, node);
	for (i = 0; i < n; i++) 
	{

	    if(XmBeingDestroyed(nodes[i]->widget))
		continue;

	    if (nodes[i]->level < LARGE_NUM)
	    {  /* already visited */
		NodeListRemove (&(node->tree_kids), nodes[i]);
	    } 
	    else  if ((nodes[i]->level <= node->level)
		      && (node != nodes[i]->tree_parent)) 
	    {
		/* make me the tree_parent of this kid */
		updateNodeDepth (nodes[i]);
		updateNodeParent (nodes[i], node);
	    } 
	    else
	    {  /* not visited yet */
		doConvertToTree (visited, nodes[i], node, level+1);
	    }
	}
	NodeListRemove (visited, node);
    }
}

/*************************************<->*************************************
 * convertToTree
 *   
 *   
 *
 *************************************<->***********************************/

static void convertToTree 
#ifndef _NO_PROTO
  (XmGraphWidget graph)
#else
  (graph) XmGraphWidget graph;
#endif
{
    NodeList  visited;

    visited.n_nodes = visited.n_slots = 0;
    visited.nodes = NULL;

    doConvertToTree (&visited, graph->graph.root, (NodePtr) NULL, 0);

    NodeListFree (&visited);
}

/*************************************<->*************************************
 * LayoutWidgets
 *   
 * 910318 mohammad:  All the nodes are inside the virtual graph space.  
 * Position them on the screen in the correct spot and draw the relevant
 * arcs.
 *************************************<->***********************************/

static void LayoutWidgets
#ifndef _NO_PROTO
  (void)
#else
  ()
#endif
{
    /* First, go back to the last node. */
    p_currentPositionInLocationArray--;

    /* Check to be certain that there was more than just the dummy root node. */
    if ( p_widgetLocationArray == p_currentPositionInLocationArray )
        return;


    do
    {
        /* This works because the first time, we skip the dummy root node. */
        p_currentPositionInLocationArray--;

#ifdef TRACING
        printf("LayoutWidgets: widget,x,y: %x %d %d\n",
                                         p_currentPositionInLocationArray,
                                         p_currentPositionInLocationArray->x, 
                                         p_currentPositionInLocationArray->y);
#endif
	/* 12/17/91 TQP:  w must be casted to type RectObj */
#if (XmREVISION==1)
        _XmMoveObject( (RectObj)p_currentPositionInLocationArray->w, 
		       p_currentPositionInLocationArray->x, 
		       p_currentPositionInLocationArray->y); 
#else
        _XmMoveObject( p_currentPositionInLocationArray->w, 
		       p_currentPositionInLocationArray->x, 
		       p_currentPositionInLocationArray->y); 
#endif

        UpdateArcs(p_currentPositionInLocationArray->w, FALSE);
    }
    while( p_widgetLocationArray != p_currentPositionInLocationArray );

}

/*************************************<->*************************************
 * LayoutWidgetsWhenSomeAreOutOfBounds
 *   
 * 910318 mohammad:  All the nodes are NOT inside the virtual graph space.  
 * Position them on the screen in the correct spot and draw the relevant
 * arcs.
 * 910324 coha:  Try to make each of the nodes visible.  
 *************************************<->***********************************/
#define DISPLACEMENT_INCREMENT 15

static void LayoutWidgetsWhenSomeAreOutOfBounds
#ifndef _NO_PROTO
  (void)
#else
  ()
#endif
{
int new_x;
int new_y;
int x_displacement = DISPLACEMENT_INCREMENT;
int y_displacement = DISPLACEMENT_INCREMENT;

    /* First, go back to the last node. */
    p_currentPositionInLocationArray--;

    /* Check to be certain that there was more than just the dummy root node. */
    if ( p_widgetLocationArray == p_currentPositionInLocationArray )
        return;

    do
    {
        /* This works because the first time, we skip the dummy root node. */
        p_currentPositionInLocationArray--;

        if ( ! p_currentPositionInLocationArray->off_of_graph ) 
        {    
/*
 #ifdef TRACING
         printf("LayoutWidgetsWhenSomeAreOutOfBounds - NOT: x,y: %d %d\n",
	         p_currentPositionInLocationArray->x,
                 p_currentPositionInLocationArray->y);
#endif
*/
	  /* 12/17/91 TQP: w must be casted to type RecObj */
#if (XmREVISION==1)
	  _XmMoveObject((RectObj)p_currentPositionInLocationArray->w, 
                           p_currentPositionInLocationArray->x, 
                           p_currentPositionInLocationArray->y);
#else
	  _XmMoveObject(p_currentPositionInLocationArray->w, 
                           p_currentPositionInLocationArray->x, 
                           p_currentPositionInLocationArray->y);
#endif

        }
        else 
        {
            /* Reset each of the displacement values if either has become 
             * too large.
             */
            if ( x_displacement > HALF_MAX_VALUE_FOR_POSITION )
                x_displacement = DISPLACEMENT_INCREMENT;
            if ( y_displacement > HALF_MAX_VALUE_FOR_POSITION )
                y_displacement = DISPLACEMENT_INCREMENT;

            /* Try to position the node so that it can be seen in the graph.
             * The strategy is to move the node back onto the graph and then
             * move it over an additional amount so that all of the nodes are
             * not simply piled up on top of each other on the edge.  In order
             * to make the graph more readable and to avoid a problem that 
             * occurs when too many nodes are piled up on top of one another,
             * also move the node in the other direction.
             * We have three cases to handle: 
             *     node is off in X and Y direction
             *     node is off in X direction
             *     node is off in Y direction
             */
            if ( ( p_currentPositionInLocationArray->right > 
                       MAX_VALUE_FOR_POSITION ) &&
                 ( p_currentPositionInLocationArray->bottom > 
                       MAX_VALUE_FOR_POSITION )
               )
            {
                new_x = MAX_VALUE_FOR_POSITION - 
                              ( p_currentPositionInLocationArray->right - 
                                p_currentPositionInLocationArray->x 
                              ) -
                              x_displacement;
                x_displacement = x_displacement + DISPLACEMENT_INCREMENT;

                new_y = MAX_VALUE_FOR_POSITION - 
                              ( p_currentPositionInLocationArray->bottom -
                                p_currentPositionInLocationArray->y
                              ) -
                              y_displacement;
                y_displacement = y_displacement + DISPLACEMENT_INCREMENT;
            }
            else    /* the node is only off of the graph in one direction */
            {
                if ( p_currentPositionInLocationArray->right > 
                         MAX_VALUE_FOR_POSITION )             /* X direction */
                {
                    new_x = MAX_VALUE_FOR_POSITION - 
                                ( p_currentPositionInLocationArray->right - 
                                  p_currentPositionInLocationArray->x 
                                ) -
                                x_displacement;
                    x_displacement = x_displacement + DISPLACEMENT_INCREMENT;

                    /* Calculate the position for the new_y.  We know at this
                     * point that it is not > MAX_VALUE_FOR_POSITION, so just
                     * calculate a new value, moving it toward the center of
                     * the graph.
                     */
                    if ( ( p_currentPositionInLocationArray->y ) < 
                               HALF_MAX_VALUE_FOR_POSITION )
                        new_y = p_currentPositionInLocationArray->y + 
                                    y_displacement;
                    else
                        new_y = p_currentPositionInLocationArray->y - 
                                    y_displacement;
                    y_displacement = y_displacement + DISPLACEMENT_INCREMENT;
                }
                else    /* Y direction */
                {
                    /* We know that ( p_currentPositionInLocationArray->bottom >
                     * MAX_VALUE_FOR_POSITION )
                     */
                    new_y = MAX_VALUE_FOR_POSITION - 
                                ( p_currentPositionInLocationArray->bottom - 
                                  p_currentPositionInLocationArray->y 
                                ) -
                                y_displacement;
                    y_displacement = y_displacement + DISPLACEMENT_INCREMENT;

                    /* Calculate the position for the new_x.  We know at this
                     * point that it is not > MAX_VALUE_FOR_POSITION, so just
                     * calculate a new value, moving it toward the center of
                     * the graph.
                     */
                    if ( ( p_currentPositionInLocationArray->x ) < 
                               HALF_MAX_VALUE_FOR_POSITION )
                        new_x = p_currentPositionInLocationArray->x +
                                    x_displacement;
                    else
                        new_x = p_currentPositionInLocationArray->x -
                                    x_displacement;
                    x_displacement = x_displacement + DISPLACEMENT_INCREMENT;
                }
            }

#ifdef TRACING
            printf("LayoutWidgetsWhenSomeAreOutOfBounds - IS: x,y: %d %d\n",
                    new_x, new_y);
#endif
	    /* 12/17/91 TQP: w must be casted to type RecObj */
#if (XmREVISION==1)
            _XmMoveObject((RectObj)p_currentPositionInLocationArray->w, new_x, new_y);
#else
            _XmMoveObject(p_currentPositionInLocationArray->w, new_x, new_y);
#endif

        }

        UpdateArcs(p_currentPositionInLocationArray->w, FALSE);
    }
    while( p_widgetLocationArray != p_currentPositionInLocationArray );
}

/*************************************<->*************************************
 * UpdateSiblingAndChildSpacing
 *   
 * 910323 coha:  We need to do slightly more than just assign the new value 
 * of a sibling or child spacing.  
 *************************************<->***********************************/

static void UpdateSiblingAndChildSpacing
#ifndef _NO_PROTO
  (XmGraphWidget graph, int sibling_spacing, int child_spacing)
#else
  ( graph, sibling_spacing, child_spacing)
	XmGraphWidget graph; int sibling_spacing; int child_spacing;
#endif
{
    /* Save the unconverted spacing for future use */

    graph->graph.original_sibling_spacing = sibling_spacing;
    graph->graph.original_child_spacing = child_spacing;

    if ( graph->graph.direction == XmHORIZONTAL )
    {
        graph->graph.child_spacing = (int)
            XmCvtToHorizontalPixels( XtScreen(graph),
                                     graph->graph.original_child_spacing,
                                     graph->manager.unit_type );

        graph->graph.sibling_spacing = (int)
            XmCvtToVerticalPixels( XtScreen(graph),
                                   graph->graph.original_sibling_spacing,
                                   graph->manager.unit_type );
    }

    else /* if ( graph->graph.direction == XmVERTICAL ) */
    {
        graph->graph.child_spacing = (int)
            XmCvtToVerticalPixels( XtScreen(graph),
                                   graph->graph.original_child_spacing,
                                   graph->manager.unit_type );

        graph->graph.sibling_spacing = (int)
            XmCvtToHorizontalPixels( XtScreen(graph),
                                     graph->graph.original_sibling_spacing,
                                     graph->manager.unit_type );
    }
}


/*************************************<->*************************************
 * PerformLayout
 *   
 * 910318 mohammad:  Make sure all the nodes are placed within the virtual
 * graph space.
 * 910324 coha:  When the child or sibling spacing is changed, we need to
 * update graph's data structures.  Otherwise, everytime that we do a relayout
 * on a large graph we will start back at the original values of sibling and
 * child spacing.  Also, we need to do a conversion of the new value from an 
 * integer to a number of pixels.
 *************************************<->***********************************/

static void PerformLayout
#ifndef _NO_PROTO
  (XmGraphWidget graph, NodePtr graph_root,
	  int (*layout_function)(), int number_of_nodes)
#else
  ( graph, graph_root, layout_function, number_of_nodes )
	XmGraphWidget graph;
	NodePtr graph_root;
	int (*layout_function)();
	int number_of_nodes;
#endif
{

    Boolean spacing_reduced_to_minimum = FALSE;
    Boolean update_sibling_and_child_spacing = FALSE;
    int initial_x;
    int initial_y;
    int graph_child_spacing = graph->graph.child_spacing;
    int graph_sibling_spacing = graph->graph.sibling_spacing;

    /* Create a list to store all of the widget locations. */
    p_widgetLocationArray = (struct widgetLocation *)
            XtMalloc( sizeof(struct widgetLocation) * (number_of_nodes + 1) );

    if ( p_widgetLocationArray == NULL )
    {
        /* The following is a problem that should be reported back to the 
         * caller of the graph widget.
         */
        printf("Graph Error:  CALL TO MALLOC FAILED:  OUT OF MEMORY!\n");
        return;
    }

#ifdef TRACING
    printf("PerformLayout entry:  Spacing:  child - %d, sibling - %d\n", 
        graph->graph.child_spacing, graph->graph.sibling_spacing);
#endif

    /* Check the child and sibling spacing to be certain that we do
     * not immediately cause an overflow.  This can be caused by a zoom
     * that increases child or sibling spacing beyond the maximum allowed.
     * Also, check to be certain that the values are not negative because
     * as of 910324, the result could be an x or y position that is negative
     * for the location of a node.  This can result in a X server abort.
     */
    if ( graph_child_spacing < 1 )
    {
        graph_child_spacing = MIN_VALUE_FOR_CHILD_OR_SIBLING_SPACING;
        update_sibling_and_child_spacing = TRUE;
    }
    else if ( graph_child_spacing > MAX_VALUE_FOR_POSITION ) 
    {
        graph_child_spacing = MAX_VALUE_FOR_POSITION;
        update_sibling_and_child_spacing = TRUE;
    }

    if ( graph_sibling_spacing < 1 )
    {
        graph_sibling_spacing = MIN_VALUE_FOR_CHILD_OR_SIBLING_SPACING;
        update_sibling_and_child_spacing = TRUE;
    }
    else if ( graph_sibling_spacing > MAX_VALUE_FOR_POSITION ) 
    {
        graph_sibling_spacing = MAX_VALUE_FOR_POSITION;
        update_sibling_and_child_spacing = TRUE;
    }

    do 
    { 
        /* Initialize the pointer that we will use to add nodes to our    
         * data structure.
         */
        p_currentPositionInLocationArray = p_widgetLocationArray;

        BAD_X_RESOLUTION = BAD_Y_RESOLUTION = FALSE;

        if ( update_sibling_and_child_spacing ) 
        {
            UpdateSiblingAndChildSpacing( graph, graph_sibling_spacing, 
                                                 graph_child_spacing );
            update_sibling_and_child_spacing = FALSE;
        }

#ifdef TRACING
        printf(
          "PerformLayout in Top of loop:  Spacing:  child - %d, sibling - %d\n",
	      graph->graph.child_spacing, graph->graph.sibling_spacing );
#endif

        /* Calculation of the initial_x and initial_y had to be moved
         * inside the loop so that it can be re-calculated when the child
         * or sibling spacing changes.
         * HORIZONTAL:  each tree root node is displayed along the left border 
         *                of the graph
         *              sibling spacing is the y direction
         *              child spacing is the x direction
         * VERTICAL:  each tree root node is displayed across the top of the
         *              graph
         *            sibling spacing is the x direction
         *            child spacing is the y direction
         */
        if (graph->graph.direction == XmHORIZONTAL)  
        {
            initial_x = VERT_LEFT_DIST - graph->graph.child_spacing 
                                       - RWidth(graph_root->widget);
            initial_y = HOR_TOP_DIST;
        }
        else
        {
            initial_x = VERT_LEFT_DIST;
            initial_y = HOR_TOP_DIST - graph->graph.sibling_spacing 
                                     - RHeight(graph_root->widget);
        }

        (*layout_function)(graph, graph_root, initial_x, initial_y);
         
        /* If a node has been placed outside of the maximum virtual space then
         * reduce the child spacing by 1/2.  If the spacing has already been
         * reduced to 1 then it cannot be reduced any more.  Do the layout
         * with overlays.
	 */
        if (BAD_X_RESOLUTION)
        {
            if ( ( ( graph->graph.direction == XmHORIZONTAL ) &&  
	           ( graph->graph.child_spacing == 1 ) 
                 ) ||
                 ( ( graph->graph.direction == XmVERTICAL ) &&  
	           ( graph->graph.sibling_spacing == 1 ) 
                 )
               )
                spacing_reduced_to_minimum = TRUE;
            else
            {
                if (graph->graph.direction == XmHORIZONTAL)  
	            graph_child_spacing = graph->graph.child_spacing / 2;
                else
	            graph_sibling_spacing = graph->graph.sibling_spacing / 2;

                update_sibling_and_child_spacing = TRUE;
            }
        }

        /* Do the same with the Y direction as we just did with the X.  However,
         * we want to be certain that we reset the boolean flag that will tell
         * us whether we have reduced the spacing to a minimum if we are still
         * able to reduce the Y spacing further.
	 */
        if ( BAD_Y_RESOLUTION )
        {
            if ( ( ( graph->graph.direction == XmHORIZONTAL ) &&  
	           ( graph->graph.sibling_spacing == 1 ) 
                 ) ||
                 ( ( graph->graph.direction == XmVERTICAL ) &&  
	           ( graph->graph.child_spacing == 1 ) 
                 )
               )
            {
                /* The spacing has not been reduced to a minimum if the code
                 * to handle the X dimension has just reduced the X spacing.
                 */
                if ( ! update_sibling_and_child_spacing ) 
                    spacing_reduced_to_minimum = TRUE;
            }
            else
            {
                spacing_reduced_to_minimum = FALSE;

                if (graph->graph.direction == XmHORIZONTAL)  
	            graph_sibling_spacing = graph->graph.sibling_spacing / 2;
                else
	            graph_child_spacing = graph->graph.child_spacing / 2;

                update_sibling_and_child_spacing = TRUE;
            }
        }
       
#ifdef TRACING
        printf(
         "PerformLayout in tail of loop:  Spacing:  child - %d, sibling - %d\n",
	  graph_child_spacing, graph_sibling_spacing );
#endif
       
    } while( ! spacing_reduced_to_minimum && 
             ( BAD_X_RESOLUTION || BAD_Y_RESOLUTION ) 
           );

    if ( ! spacing_reduced_to_minimum )
        LayoutWidgets();
    else
        LayoutWidgetsWhenSomeAreOutOfBounds();

    /* clean up the temporary list of widgets */
    XtFree( (char*) p_widgetLocationArray );

}


/*************************************<->*************************************
 * AddWidgetInfoToListOfWidgets
 *   
 * 910318 mohammad:  Check the position of x and y.  If either one falls
 * outside the virtual graph space, it assigns FALSE to a BAD_*_RESOLUTION flag.
 *************************************<->***********************************/

static void AddWidgetInfoToListOfWidgets
#ifndef _NO_PROTO
  (Widget w, int left, int right, int top, int bottom)
#else
  (w, left, right, top, bottom) Widget w; int left, right, top, bottom;
#endif
{

    /* Assign the values of the fields to the widgetLocation structure. */
    p_currentPositionInLocationArray->w = w;  
    p_currentPositionInLocationArray->x = left;  
    p_currentPositionInLocationArray->y = top;
    p_currentPositionInLocationArray->off_of_graph = FALSE;

    /* Only fill in the bottom and right positions if we will need to know
     * the positions when we have reached the minimum child or sibling spacing.
     */
    if (right >= MAX_VALUE_FOR_POSITION) 
    {
        BAD_X_RESOLUTION = TRUE;
        p_currentPositionInLocationArray->off_of_graph = TRUE;
        p_currentPositionInLocationArray->bottom = bottom;
        p_currentPositionInLocationArray->right = right;
    }
  
    if (bottom >= MAX_VALUE_FOR_POSITION)
    {
        BAD_Y_RESOLUTION = TRUE;
        if ( ! p_currentPositionInLocationArray->off_of_graph )
        {
            p_currentPositionInLocationArray->off_of_graph = TRUE;
            p_currentPositionInLocationArray->bottom = bottom;
            p_currentPositionInLocationArray->right = right;
        }
    }

#ifdef TRACING
  if ((right >= MAX_VALUE_FOR_POSITION) || (bottom >= MAX_VALUE_FOR_POSITION)) 
      printf("*********** next position is not on graph ************* \n");
  printf("AddWidgetInfoToListOfWidgets- widget, right, bottom: %x %d %d\n", 
                 p_currentPositionInLocationArray, right, bottom);
#endif

    p_currentPositionInLocationArray++;
}

/*************************************<->*************************************
 * fastHorizontalLayout
 *
 * HORIZONTAL:  The root nodes for the trees are displayed along the left 
 *              border of the graph and each tree is laid out horizontally.
 *                                                      
 *                                                     x0  40  60
 *                                                  y
 *                                                  0   +-----------
 *                                                      |
 *              sibling spacing is the y direction 40   |   sib-->child
 *              child spacing is the x direction        |
 *                                                 60   |   sib
 *                                                      |
 *
 * 910328 coha:  Cloned from the horizontalLayout routine in an attempt to 
 * speed up the average layout.
 *************************************<->***********************************/
static Boolean NodeOutOfBounds;

static int fastHorizontalLayout 
#ifndef _NO_PROTO
  (XmGraphWidget graph, NodePtr node,
				 int left, int top)
#else
  (graph, node, left, top) XmGraphWidget graph; NodePtr       node;
		int           left, top;
#endif
{
  int       childTop   = top;
  Widget    w          = node->widget;
  int       childLeft  = left + RWidth(w) + 2*RBorder(w) +
                                                 graph->graph.child_spacing;
  int       bottom     = top;
  int       n          = node->tree_kids.n_nodes;
  NList     nodes      = node->tree_kids.nodes;
  int       i;
  
  /* Just force a return if called when a node is already out of bounds. */
  if ( NodeOutOfBounds )
     return 0;
   
  if(XmBeingDestroyed(w)) return bottom;
    
  for (i = 0; i < n; i++) {
    if (nodes[i]->tree_parent == node && !XmBeingDestroyed(nodes[i]->widget)) {
      bottom   = fastHorizontalLayout (graph, nodes[i], childLeft, childTop);
      childTop = bottom + graph->graph.sibling_spacing;
    }
  }
    
  bottom = MAX( (int) bottom, (int)(top + RHeight(w) + 2 * RBorder(w)) );
 
  /* Calculate y top and assign it to the variable named top.  This is    */
  /* is the y coordinate for the widget.                                  */
  top = (top + bottom)/2 - ( (int)(RHeight(w)) )/2;

#ifdef TRACING
  printf("fastHorizontalLayout - IS: x,y: %d %d\n", left, top );
#endif

  /* Avoid laying out the pseudo-root node if it is out of bounds.        */
  /* WARNING:  This needs to be done first because of a comparison in the */
  /* next if statement that is unsigned.                                  */
  if ( ( left < 0 ) || ( top < 0 ) )
    return 0;

  if ( ( NodeOutOfBounds ) ||
       ( (int)( left + RWidth(w) + 2*RBorder(w) ) >=   /* x right */
           (int) MAX_VALUE_FOR_POSITION ) ||
       ( (int) bottom >=                               /* y bottom */
           (int) MAX_VALUE_FOR_POSITION )
     ) {
#ifdef TRACING
    printf("fastHorizontalLayout - NOT x,y: %d %d\n", left, top );
#endif
    NodeOutOfBounds = TRUE;
    return 0;
  }

  /* 12/17/91 TQP: w must be casted to type RecObj */
#if (XmREVISION==1)
  _XmMoveObject( (RectObj)w,  left,  top );
#else
  _XmMoveObject( w,  left,  top );
#endif

  UpdateArcs( w, FALSE );

  return bottom;
}

/*************************************<->*************************************
 * horizontalLayout
 *
 * HORIZONTAL:  The root nodes for the trees are displayed along the left 
 *              border of the graph and each tree is laid out horizontally.
 *                                                      
 *                                                     x0  40  60
 *                                                  y
 *                                                  0   +-----------
 *                                                      |
 *              sibling spacing is the y direction 40   |   sib-->child
 *              child spacing is the x direction        |
 *                                                 60   |   sib
 *                                                      |
 *************************************<->***********************************/

static int horizontalLayout 
#ifndef _NO_PROTO
  (XmGraphWidget graph, NodePtr node, int left, int top)
#else
  (graph, node, left, top) XmGraphWidget graph; NodePtr node; int left, top;
#endif
{
    int       childTop   = top;
    Widget    w          = node->widget;
    int       childLeft  = left + RWidth(w) + 2 * RBorder(w) + graph->graph.child_spacing;
    int       bottom     = top;
    int       n          = node->tree_kids.n_nodes;
    NList     nodes      = node->tree_kids.nodes;
    int       i;

    if(XmBeingDestroyed(w))
	return bottom;

    for (i = 0; i < n; i++) 
    {
	if (nodes[i]->tree_parent == node && !XmBeingDestroyed(nodes[i]->widget)) 
	{
	    bottom   = horizontalLayout (graph, nodes[i], childLeft, childTop);
	    childTop = bottom + graph->graph.sibling_spacing;
	}
    }

    bottom = MAX (bottom, top + RHeight(w) + 2 * RBorder(w));
#if (XmREVISION==1)
    _XmMoveObject ((RectObj)w, left, (top + bottom)/2 - RHeight(w) / 2);
#else
    _XmMoveObject (w, left, (top + bottom)/2 - RHeight(w) / 2);
#endif

/* CYY Added */
    updateNodeStruct(w);

    UpdateArcs(w, FALSE);
    return bottom;
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static int verticalLayout 
#ifndef _NO_PROTO
  (XmGraphWidget graph, NodePtr node, int left, int top)
#else
  (graph, node, left, top) XmGraphWidget graph; NodePtr node; int left, top;
#endif
{
    Widget  w          = node->widget;
    int     childTop   = top + RHeight(w) + 2 * RBorder(w) + graph->graph.child_spacing;
    int     childLeft  = left;
    int     right      = left;
    int     n          = node->tree_kids.n_nodes;
    NList   nodes      = node->tree_kids.nodes;
    int     i;


    if(XmBeingDestroyed(w))
	return right;

    for (i = 0; i < n; i++) 
    {
	if (nodes[i]->tree_parent == node && !XmBeingDestroyed(nodes[i]->widget))
	{
	    right = verticalLayout (graph, nodes[i], childLeft, childTop);
	    childLeft = right + graph->graph.sibling_spacing;
	}
    }

    right = MAX (right, left + RWidth(w) + 2 * RBorder(w));
#if (XmREVISION==1)
    _XmMoveObject ((RectObj)w, (left + right) / 2 - RWidth(w) / 2, top);
#else
    _XmMoveObject (w, (left + right) / 2 - RWidth(w) / 2, top);
#endif

/* CYY Added */
    updateNodeStruct(w);
    UpdateArcs(w, FALSE);
    return right;
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void MarkSubgraphVisited 
#ifndef _NO_PROTO
  (NodePtr node)
#else
  (node) NodePtr      node;
#endif
{
    NList      nodes = node->kids.nodes;
    int        i, n_nodes = node->kids.n_nodes;

    if (node && !node->visited) 
    {
	node->visited = TRUE;
	for (i = 0; i < n_nodes; i++) 
	    MarkSubgraphVisited(nodes[i]);
    }
}

/*************************************<->*************************************
 *
 *   DoLayout:       Toplevel interface to layout algorithm
 *   -----------
 *
 *************************************<->***********************************/

static void DoLayout 
#ifndef _NO_PROTO
  (XmGraphWidget graph)
#else
  (graph) XmGraphWidget graph;
#endif
{ 
  if (!graph->graph.layed_out) {
    NodePtr    next, dummy; 
    NodeList   roots;  /* list of nodes that act as roots of subgraphs */
    int        i, nkids;
    NList      kids = NULL;
    /* 9103xx: CLL C++ dev.  added to support layout of graph */
    int        total_number_of_nodes_to_layout = 0;  
    int        estimated_size_of_graph;
	
    for (i = 0; i < graph->composite.num_children; i++) {
      Widget   w;
      NodePtr  next;

      w = graph->composite.children[i];

      if(!XmBeingDestroyed(w)) {
	next              = NODEPTR (w);
	next->visited     = FALSE;
	next->tree_parent = NULL;
	next->level       = LARGE_NUM;
      }
    }

    graph->graph.layed_out = TRUE;

    dummy = graph->graph.root;
    InitNode (dummy);

    roots.n_nodes = roots.n_slots = 0; roots.nodes = NULL;
    /* Insert nodes in user_roots */
    for (i = 0; i < graph->graph.user_roots.n_nodes; i++) {
      /*
       * BUG alert!!!!!!!!!
       */
      next = graph->graph.user_roots.nodes[i];
      NodeListInsertNoDuplicates (&roots, next);
      MarkSubgraphVisited (next);
    } 

    /* 
     * Insert orphan nodes 
     */

    for (i = 0; i < graph->composite.num_children; i++) {
      Widget child = graph->composite.children[i];

      if(!XmBeingDestroyed(child)) {
	next = NODEPTR (child);

	if ((next->parents.n_nodes == 0) && (next != dummy)) {
	  NodeListInsertNoDuplicates (&roots, next);
	  MarkSubgraphVisited (next);
	}
      }
    } 

    /* All unmarked nodes belong to sub-graphs with cycles.
       We capture them here, and arbitrarily choose a root for
       each sub-graph  
       */

    for (i = 0; i < graph->composite.num_children; i++) {
      Widget child = graph->composite.children[i];

      if(!XmBeingDestroyed(child)) {
	next = NODEPTR(child);

	if (!next->visited && (next != dummy)) {
	  NodeListInsertNoDuplicates (&roots, next);
	  MarkSubgraphVisited (next);
	}
      }
    } 

    nkids = roots.n_nodes; kids = roots.nodes;

    for (i = 0; i < nkids; i++) {
      if(!XmBeingDestroyed(kids[i]->widget)) {
	NodeListInsert (&(kids[i]->parents), dummy);
	NodeListInsert (&(dummy->kids), kids[i]);
	NodeListInsert (&(dummy->tree_kids), kids[i]);
      }
    }

    convertToTree (graph);  

#if CLL_ADDITION

    /* 910318 mohammad:  PerformLayout takes either of the two actual layout 
     * functions as an argument, horizontalLayout or verticalLayout.
     *
     * 910328 coha:  Try to improve the performance for the average case.
     * Unfortunately, we can only estimate the size of the graph.  First,
     * assume that a node is out of bounds.  Only reset the value of
     * NodeOutOfBounds immediately before we make the call to
     * fastHorizontalLayout.
     */    
    NodeOutOfBounds = TRUE;

    /* remember to include the root node */
    total_number_of_nodes_to_layout = XmGraphNumNodes((Widget)graph) + 1;

    /* Only try the fast layout if the graph is being laid out along the  */
    /* left margin, horizontally, and the sibling spacing is reasonable.  */
    /* This will also prevent an overflow from occurring when we try to   */
    /* do the calculation of the graph size.                              */
    if ( ( total_number_of_nodes_to_layout > 1 ) &&
         ( graph->graph.direction == XmHORIZONTAL ) &&
         ( graph->graph.sibling_spacing > 0 ) &&
         ( graph->graph.sibling_spacing < 300 ) &&
         ( total_number_of_nodes_to_layout < 1500 ) 
       ) {

      /* do NOT use the root node as the size of a node in the graph      */
      /* because the size of the root node is the size of the graph       */
      /* and not the size of a node in the graph.  Also, do NOT execute   */
      /* this code if the graph does not have any nodes because the       */
      /* following assignment will not be valid.                          */
      Widget temp_widget = graph->graph.root->tree_kids.nodes[0]->widget;

      estimated_size_of_graph = HOR_TOP_DIST +
                                  total_number_of_nodes_to_layout * 
                                    ( RHeight(temp_widget) + 
                                      2*RBorder(temp_widget) +
                                      graph->graph.sibling_spacing 
                                    );

#ifdef TRACING
  printf("DoLayout: total, height, border, sibling:  %d %d %d %d\n",
               total_number_of_nodes_to_layout,
               RHeight(temp_widget),
               2*RBorder(temp_widget),
               graph->graph.sibling_spacing );
#endif

      if ( estimated_size_of_graph < MAX_VALUE_FOR_POSITION ) {

        /* Set the global that will be used to determine whether a node   */
        /* is attempted to be placed out of the maximum bounds.           */
        NodeOutOfBounds = FALSE;

        fastHorizontalLayout( graph, 
                              graph->graph.root,
                              VERT_LEFT_DIST - graph->graph.child_spacing - 
                                                   RWidth(dummy->widget),
                              HOR_TOP_DIST);
      }
    }

    /* If a node is out of bounds then use the more tedious and memory    */
    /* consuming method of laying out the graph.                          */

    if ( ( total_number_of_nodes_to_layout > 1 ) &&
         ( NodeOutOfBounds ) 
       ) {
      if (graph->graph.direction == XmHORIZONTAL)
        PerformLayout( graph, dummy, horizontalLayout, 
                           total_number_of_nodes_to_layout );
      else
        PerformLayout( graph, dummy, verticalLayout,
                           total_number_of_nodes_to_layout );
    }
    /* END of new code added in 9103xx */	
    
#else /*  no CLL_ADDITION: here is the original code */
    if (graph->graph.direction == XmHORIZONTAL) 
      {
	horizontalLayout(graph, graph->graph.root, 
			 VERT_LEFT_DIST - graph->graph.child_spacing - RWidth(dummy->widget), 
			 HOR_TOP_DIST);
      }
    else 
      {
	verticalLayout (graph, graph->graph.root, VERT_LEFT_DIST,  HOR_TOP_DIST - graph->graph.sibling_spacing - RHeight(dummy->widget));
      }
#endif /* CLL_ADDITION */

    for (i = 0; i < nkids; i++) {

      if(!XmBeingDestroyed(kids[i]->widget)) {
	NodeListRemove (&(kids[i]->parents), dummy);
	kids[i]->tree_parent = NULL;
	NodeListRemove (&(dummy->kids), kids[i]);
	NodeListRemove (&(dummy->tree_kids), kids[i]);
      }
    }

    if (kids) {
      for(i=0;i<nkids;i++)
	kids[i] = NULL;
      XtFree ((char *) kids);
    }

    NodeListFree (&(dummy->tree_kids));
    NodeListFree (&(dummy->parents));
    NodeListFree (&(dummy->kids));

    AdjustSize (graph);

  }

}

/********
 * XmReLayGraph: Will call the layout algorithm on graph.
 *********/
void XmGraphLayout 
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget graphW;
#endif
{ XmGraphWidget graph = (XmGraphWidget) graphW;
    int      i;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphLayout requires an XmGraph widget");
	return;
    }

    graph->graph.layed_out = FALSE;

    if(XtIsRealized(graphW)) 
	XClearArea(XtDisplay(graphW), XtWindow(graphW), 0, 0, 0, 0, TRUE);

    (*(graph->graph.layout_proc)) (graph);

    if(XtIsRealized(graphW))
    { XmArcWidget arc;
	for (i = 0; i < graph->graph.n_arcs; i++) 
	{
	    arc   = graph->graph.arcs[i]; 
	    arc->core.visible = ArcVisibleInGraph(graph, arc);

	    if(arc->core.visible)
		ComputeRegionsForArc(arc);
	    else
		FreeArcRegions(arc);
	}
    }
}

static Boolean StringsAreEqual 
#ifndef _NO_PROTO
  (register char *in_str, register char *test_str)
#else
  (in_str, test_str)
	register char * in_str;
	register char * test_str;
#endif

{
   register int i;
   register int j;
   i = *in_str;
   if (((in_str[0] == 'X') || (in_str[0] == 'x')) &&
       ((in_str[1] == 'M') || (in_str[1] == 'm')))
        in_str +=2;

   for (;;)
   {
      i = *in_str;
      j = *test_str;

      if (isupper (i)) i = tolower (i);
      if (i != j) return (False);
      if (i == 0) return (True);

      in_str++;
      test_str++;
   }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void _XmCvtStringToArcDirection 
#ifndef _NO_PROTO
  (XrmValuePtr args, Cardinal *num_args, XrmValue *from_val, XrmValue *to_val)
#else
  (args, num_args, from_val, to_val)
		XrmValuePtr args; Cardinal   *num_args;
		XrmValue   *from_val; XrmValue   *to_val;
#endif

{
    char * in_str = XtNewString(from_val->addr);
    static unsigned char i;

    toAllLowerCase(in_str);
    to_val->size = sizeof (unsigned char);
    to_val->addr = (caddr_t) &i;

    if (StringsAreEqual (in_str, "directed")) 
	i = XmDIRECTED;
    else if (StringsAreEqual (in_str, "bidirected")) 
	i = XmBIDIRECTED;
    else if (StringsAreEqual (in_str, "undirected")) 
	i = XmUNDIRECTED;
    else {
	to_val->size = 0;
	to_val->addr = NULL;
	XtStringConversionWarning ((char *)from_val->addr, XmRArcDirection);
    }
    XtFree((char *) in_str);
}


static void _XmCvtStringToAutoLayoutType 
#ifndef _NO_PROTO
  (XrmValuePtr args, Cardinal *num_args, XrmValue *from_val, XrmValue *to_val)
#else
  (args, num_args, from_val, to_val)
	XrmValuePtr args; Cardinal   *num_args;
	XrmValue   *from_val; XrmValue   *to_val;
#endif
{
    char * in_str = XtNewString(from_val->addr);
    static unsigned char i;

    toAllLowerCase(in_str);
    to_val->size = sizeof (unsigned char);
    to_val->addr = (caddr_t) &i;

    if (StringsAreEqual (in_str, "never")) 
	i = XmNEVER;
    else if (StringsAreEqual (in_str, "false")) 
	i = XmNEVER;
    else if (StringsAreEqual (in_str, "always")) 
	i = XmALWAYS;
    else if (StringsAreEqual (in_str, "true")) 
	i = XmALWAYS;
    else if (StringsAreEqual (in_str, "arcs_only")) 
	i = XmARCS_ONLY;
    else if (StringsAreEqual (in_str, "nodes_only")) 
	i = XmNODES_ONLY;
    else if (StringsAreEqual (in_str, "partial")) 
	i = XmPARTIAL;
    else {
	to_val->size = 0;
	to_val->addr = NULL;
	XtStringConversionWarning ((char *)from_val->addr, XmRArcDirection);
    }
    XtFree((char *) in_str);
}


/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void _XmFromChildPixels 
#ifndef _NO_PROTO
  (Widget widget, int resource, XtArgVal *value)
#else
  (widget, resource, value) Widget widget; int resource; XtArgVal * value;
#endif
{ XmGraphWidget graph = (XmGraphWidget) widget;
    if(graph->graph.direction == XmVERTICAL)
	_XmFromHorizontalPixels (widget, resource, value);
    else
	_XmFromVerticalPixels (widget, resource, value);  
}

#ifdef _R4_
XmImportOperator _XmToChildPixels 
#ifndef _NO_PROTO
  (Widget widget, int offset, XtArgVal *value)
#else
  (widget, offset, value) Widget widget; int offset; XtArgVal * value;
#endif
{ XmGraphWidget graph = (XmGraphWidget) widget;
    if(graph->graph.direction == XmVERTICAL)
	return _XmToHorizontalPixels (widget, offset, value);
    else
	return _XmToVerticalPixels (widget, offset, value);  
}
#endif

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void _XmFromSiblingPixels 
#ifndef _NO_PROTO
  (Widget widget, int resource, XtArgVal *value)
#else
  (widget, resource, value) Widget widget; int resource; XtArgVal * value;
#endif
{ XmGraphWidget graph = (XmGraphWidget) widget;

    if(graph->graph.direction == XmHORIZONTAL)
	_XmFromHorizontalPixels (widget, resource, value);
    else
	_XmFromVerticalPixels (widget, resource, value);
}


#ifdef _R4_
XmImportOperator _XmToSiblingPixels 
#ifndef _NO_PROTO
  (Widget widget, int offset, XtArgVal *value)
#else
  (widget, offset, value) Widget widget; int offset; XtArgVal * value;
#endif

{ XmGraphWidget graph = (XmGraphWidget) widget;
    if(graph->graph.direction == XmHORIZONTAL)
	return _XmToHorizontalPixels (widget, offset, value);
    else
	return _XmToVerticalPixels (widget, offset, value);  
}
#endif

static void toAllLowerCase
#ifndef _NO_PROTO
  (register char *p) 
#else
  (p) register char *p;
#endif
{ 
    while(*p) {
      *p = tolower(*p);
      p++;               /* 910314 coha:  change from *p++ = tolower(*p);  */
                         /* because the time of incrementation is          */
                         /* ambiguous and thus does not work with all      */
                         /* machines.                                      */
    }
}


/****
 *
 * Conversion from string to line style of arcs
 *
 ********/

static void _XmCvtStringToLineStyle 
#ifndef _NO_PROTO
  (XrmValuePtr args, Cardinal *num_args, XrmValue *from_val, XrmValue *to_val)
#else
  (args, num_args, from_val, to_val)
	XrmValuePtr args; Cardinal * num_args;
	XrmValue * from_val; XrmValue * to_val;
#endif

{ char *in_str = XtNewString(from_val->addr);
  static unsigned char i;
  
  toAllLowerCase(in_str);
  
  
  to_val->size = sizeof (unsigned char);
  to_val->addr = (caddr_t) &i;
  
  if (StringsAreEqual (in_str, "linesolid")) 
      i = XmLineSolid;
  else if (StringsAreEqual (in_str, "lineonoffdash")) 
      i = XmLineOnOffDash;
  else if (StringsAreEqual (in_str, "linedoubledash")) 
      i = XmLineDoubleDash;
  else {
      to_val->size = 0;
      to_val->addr = NULL;
      XtStringConversionWarning ((char *)from_val->addr, XmRLineStyle);
  }
  XtFree ((char *)  in_str);
}


/****
 *
 * Conversion from String to Cap Style for arc lines 
 *
 ********/

static void _XmCvtStringToCapStyle 
#ifndef _NO_PROTO
  (XrmValuePtr args, Cardinal *num_args, XrmValue *from_val, XrmValue *to_val)
#else
  (args, num_args, from_val, to_val)
	XrmValuePtr args; Cardinal *num_args;
	XrmValue *from_val; XrmValue *to_val;
#endif

{ char * in_str = XtNewString(from_val->addr);
  static unsigned char i;
  
  toAllLowerCase(in_str);
  
  to_val->size = sizeof (unsigned char);
  to_val->addr = (caddr_t) &i;
  
  if (StringsAreEqual (in_str, "capnotlast")) 
      i = XmCapNotLast;
  else if (StringsAreEqual (in_str, "capbutt")) 
      i = XmCapButt;
  else if (StringsAreEqual (in_str, "capround")) 
      i = XmCapRound;
  else if (StringsAreEqual (in_str, "capprojecting")) 
      i = XmCapProjecting;
  else {
      to_val->size = 0;
      to_val->addr = NULL;
      XtStringConversionWarning ((char *)from_val->addr, XmRCapStyle);
  }
  XtFree ((char *) in_str);
}

/****
 *
 * Conversion from String to ArcDrawMode
 *
 ********/

static void _XmCvtStringToArcDrawMode 
#ifndef _NO_PROTO
  (XrmValuePtr args, Cardinal *num_args, XrmValue *from_val, XrmValue *to_val)
#else
  (args, num_args, from_val, to_val)
	XrmValuePtr args; Cardinal * num_args;
	XrmValue * from_val; XrmValue * to_val;
#endif

{ char * in_str = XtNewString(from_val->addr);
  static unsigned char i;
  
  toAllLowerCase(in_str);
  
  to_val->size = sizeof (unsigned char);
  to_val->addr = (caddr_t) &i;
  
  if (StringsAreEqual (in_str, "position_fixed")) 
      i = XmPOSITION_FIXED;
  else if (StringsAreEqual (in_str, "position_relative")) 
      i = XmPOSITION_RELATIVE;
  else {
      to_val->size = 0;
      to_val->addr = NULL;
      XtStringConversionWarning ((char *)from_val->addr, XmRArcDrawMode);
  }
  XtFree ((char *) in_str);
}


static void XmDispatchArcInput 
#ifndef _NO_PROTO
  (Widget w, XEvent *event,Mask  mask)
#else
  (w, event, mask) Widget  w; XEvent *event; Mask    mask;
#endif
{
    if (w && XtIsSensitive (w) && XtIsManaged (w))
	(*(((XmArcWidgetClass) (w->core.widget_class))->arc_class.input_dispatch)) (w, event, mask);
}


/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void DeleteChild
#ifndef _NO_PROTO
  (Widget w)
#else
  (w) Widget	w;
#endif
{
    register Cardinal	     position;
    register Cardinal	     i;
    register XmGraphWidget   graph = (XmGraphWidget) XtParent(w);

    if (XtIsSubclass(w, xmArcWidgetClass)) 
    {

	for (position = 0; position < graph->graph.n_arcs; position++) 
	{
	    if (graph->graph.arcs[position] == (XmArcWidget)(w)) {
		break;
	    }
	}
	if (position == graph->graph.n_arcs) return;

	/* Ripple children down one space from "position" */
	graph->graph.n_arcs--;
	for (i = position; i < graph->graph.n_arcs; i++) 
	{
	    graph->graph.arcs[i] = graph->graph.arcs[i+1];
	}
    }
    else 
    {

#ifdef _R4_
	if (!XtIsRectObj(w))
#else
	if (!XtIsRectObject(w))
#endif
	    return;

	for (position = 0; position < graph->composite.num_children; position++) 
	{
	    if (graph->composite.children[position] == w) {
		break;
	    }
	}
	if (position == graph->composite.num_children) return;

	/* Ripple children down one space from "position" */
	graph->composite.num_children--;
	for (i = position; i < graph->composite.num_children; i++) 
	{
	    graph->composite.children[i] = graph->composite.children[i+1];
	}
    }
}


/*************************************<->*************************************
 *
 *   Functions to add lines and rectangles to "draw" list, and to
 *             draw the lines and rectangles in the "draw"  lists.
 *
 *************************************<->***********************************/

static void AddRectToList 
#ifndef _NO_PROTO
  (XmGraphWidget graph,Position x,Position y,Dimension width,Dimension height)
#else
  (graph, x, y, width, height) XmGraphWidget graph;
		Position x, y; Dimension width, height;
#endif
{
    int pos = rectangle_list.n;

    if (MAX_RECTANGLES == (1 + pos)) 
    {
	XDrawRectangles (XtDisplay (graph), XtWindow (graph), graph->graph.xorgc,
			 rectangle_list.rect, MAX_RECTANGLES);
	pos = rectangle_list.n = 0;
    }

    rectangle_list.rect [pos].x = x;
    rectangle_list.rect [pos].y = y;
    rectangle_list.rect [pos].height = height;
    rectangle_list.rect [pos].width = width;
    rectangle_list.n++;
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void DrawRectList 
#ifndef _NO_PROTO
  (XmGraphWidget graph)
#else
  (graph) XmGraphWidget graph;
#endif
{
    if (rectangle_list.n > 0) 
    { XDrawRectangles (XtDisplay (graph), XtWindow (graph), graph->graph.xorgc,
			 rectangle_list.rect, rectangle_list.n);
	rectangle_list.n = 0;
    }
}
 
 
/*************************************<->*************************************
 *
 *  Draws the dashed XOR box for area selection.  Tests are added to insure
 *  the width and height are positive.
 *							WJS 5/22/92
 *************************************<->***********************************/

static void DrawSelectBox
#ifndef _NO_PROTO
  (XmGraphWidget graph, Position x, Position y, int dx, int dy)
#else
  (graph, x, y, dx, dy) XmGraphWidget graph; Position x, y; int dx, dy;
#endif
{
   if (dx < 0) { x += dx; dx = -dx; }
   if (dy < 0) { y += dy; dy = -dy; }
   
   XDrawRectangle(XtDisplay (graph), XtWindow (graph), graph->graph.xorgc,
			 x, y, dx, dy);
}


/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static Boolean LineAlreadyInList
#ifndef _NO_PROTO
  (XmGraphWidget graph, int x1, int y1, int x2, int y2)
#else
  (graph, x1, y1, x2, y2) XmGraphWidget graph; int x1, y1, x2, y2;
#endif
{
    int i;

    int pos = line_list.n - 1;

    int maxx = MAX(x1, x2);
    int minx = MIN(x1, x2);
    int maxy = (maxx == x1) ? y1 : y2;
    int miny = (minx == x1) ? y1 : y2;

    for(i=0; i < pos; i++) 
    {
	if(line_list.lines [i].x1 == minx && 
	   line_list.lines [i].x2 == maxx &&
	   line_list.lines [i].y1 == miny && 
	   line_list.lines [i].y2 == maxy)
	    return TRUE;
    }
    return FALSE;
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void AddLineToList 
#ifndef _NO_PROTO
  (XmGraphWidget graph, int x1, int y1, int x2, int y2)
#else
  (graph, x1, y1, x2, y2) XmGraphWidget graph; int x1, y1, x2, y2;
#endif
{
    int pos = line_list.n;

    if(LineAlreadyInList(graph, x1, y1, x2, y2))
	return;

    if (MAX_LINES == (1 + pos)) 
    {
	XDrawSegments (XtDisplay (graph), XtWindow (graph), graph->graph.xorgc,
		       line_list.lines, MAX_LINES);
	pos = line_list.n = 0;
    }

    line_list.lines [pos].x1 = MIN(x1, x2);
    line_list.lines [pos].y1 = (line_list.lines [pos].x1 == x1) ? y1 : y2;
    line_list.lines [pos].x2 = MAX(x1, x2);
    line_list.lines [pos].y2 = (line_list.lines [pos].x2 == x2) ? y2 : y1;
    line_list.n ++;
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void DrawLineList 
#ifndef _NO_PROTO
  (XmGraphWidget graph)
#else
  (graph) XmGraphWidget graph;
#endif
{
    if (line_list.n > 0) 
    {
	XDrawSegments (XtDisplay (graph), XtWindow (graph), graph->graph.xorgc,
		       line_list.lines, line_list.n);
	line_list.n = 0;
    }
}



/*************************************<->*************************************
 *
 *   Node Manipulation Routines
 *   --------------------------
 *
 *************************************<->***********************************/


/* 
 * Inserts node at end of nlist. 
 */

static void NodeListInsert 
#ifndef _NO_PROTO
  (NodeList *nlist, NodePtr node)
#else
  (nlist, node) NodeList    *nlist; NodePtr	 node;
#endif
{
    int    position = nlist->n_nodes;
    int    i;

    if (position == nlist->n_slots) 
    {
	/* Allocate more space */

	nlist->n_slots += (nlist->n_slots / 2) + 2;
	nlist->nodes = 
	    (NList) XtRealloc ((caddr_t) nlist->nodes,
			       (unsigned) (nlist->n_slots) * sizeof (NodePtr));
	for(i=position;i<nlist->n_slots;i++)
	    nlist->nodes[i] = NULL;
    }
    nlist->nodes [position] = node;
    nlist->n_nodes++;
}

/* Inserts node at end of nlist. Does not allow duplicates */

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

static void NodeListInsertNoDuplicates 
#ifndef _NO_PROTO
  (NodeList *nlist, NodePtr node)
#else
  (nlist, node) NodeList    *nlist; NodePtr	 node;
#endif
{
    int    i, n_nodes = nlist->n_nodes;
    NList  nodes = nlist->nodes;

    /* look for node, unless nodes is null */
    if(nodes)
	for (i = 0; (i < n_nodes) && (nodes [i] != node); )
	{ ++i; /* to make saber_c happy about empty for loop */
	}

    /* 
     * If nodes is null, NodeListInsert will create a list.
     * Otherwise, it will insert it in the list unless it is already there.
     */

    if (!nodes || i == n_nodes ||  nodes [i] != node)
	NodeListInsert (nlist, node);
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

/* If node is a member of nlist, it is removed */
static void NodeListRemove 
#ifndef _NO_PROTO
  (NodeList *nlist, NodePtr node)
#else
  (nlist, node) NodeList    *nlist; NodePtr	node;
#endif
{

    int         n_nodes = nlist->n_nodes, i;

    if (n_nodes > 0) 
    {
	/* find the node */

	for (i = 0; i < n_nodes; i++)
	    if(nlist->nodes[i] == node)
	    {
		nlist->n_nodes = --n_nodes;
		while (i < n_nodes) {
		    nlist->nodes [i] = nlist->nodes [i+1];
		    i++;
		}
		nlist->nodes [n_nodes] = NULL;
		return;
	    }
    }
}

/*********************************************************
 *
 * NodeListFree: Free up all its space.
 *
 ***********************************************************/ 

static void NodeListFree 
#ifndef _NO_PROTO
  (NodeList *nlist)
#else
  (nlist) NodeList    *nlist;
#endif
{
    if ((nlist->nodes != NULL) && (nlist->n_slots > 0))
	XtFree ((char *) nlist->nodes);

    nlist->nodes = NULL;
    nlist->n_slots = 0;
}

/*******************************************************
 *
 * InitNode: initialize the node structure fields
 *
 ********************************************************/
static void InitNode 
#ifndef _NO_PROTO
  (NodePtr node)
#else
  (node) NodePtr  node;
#endif
{
    node->visited = FALSE;
    node->tree_parent = NULL;
    node->level = LARGE_NUM; /* any large number will do */
    node->tree_kids.nodes = NULL; 
    node->tree_kids.n_nodes = 0;
    node->tree_kids.n_slots = 0;
    node->parents.nodes = NULL; 
    node->parents.n_nodes = 0;
    node->parents.n_slots = 0;
    node->kids.nodes = NULL;
    node->kids.n_nodes = 0;
    node->kids.n_slots = 0;
    node->from_arcs.arcs = NULL;
    node->from_arcs.n_arcs = 0;
    node->from_arcs.n_slots = 0;
    node->to_arcs.arcs = NULL;
    node->to_arcs.n_arcs = 0;
    node->to_arcs.n_slots = 0;
}

/*
 * Return true if the arc intersects the given rectangle. Can't be sure of boundary cases without
 * doing fullblown clipping, so err on the safe side. If arc cannot be contained or cross rect, return FALSE
 * otherwise return TRUE.
 */

Boolean  _ArcInRect
#ifndef _NO_PROTO
  (XmArcWidget arc, XRectangle *rect)
#else
  (arc, rect) XmArcWidget arc; XRectangle *rect;
#endif
{ int x1, x2, y1, y2;

  if (arc->arc.to == arc->arc.from) /* */
	return TRUE;

  x1 = MIN(arc->arc.from_x,  arc->arc.to_x-5);
  x2 = MAX(arc->arc.to_x+5,    arc->arc.from_x) + arc->arc.width;
  y1 = MIN(arc->arc.from_y,  arc->arc.to_y-5);
  y2 = MAX(arc->arc.to_y+5,    arc->arc.from_y) + arc->arc.width;
      /* +/- 5 is to allow for arrow heads */
#if SESD
     if (arc->arc.map_name)
      { if (x2-x1 < arc->arc.labelwidth)
	   { x2 = (x2+x1+arc->arc.labelwidth)/2;
	     x1 = x2 - arc->arc.labelwidth;
	   }
        if (y2-y1 < arc->arc.labelheight)
	   { y2 = (y2+y1+arc->arc.labelheight)/2;
	     y1 = y2 - arc->arc.labelheight;
	   }
      }

    if (x1 <= (int)(rect->x + rect->width) &&
	y1 <= (int)(rect->y + rect->height) &&
        x2 >= (int)(rect->x) && y2 >= (int)(rect->y))
#else
    if( x1 < rect->x + rect->width && y1 < rect->y + rect->height && 
	x2 > rect->x && y2 > rect->y)
#endif
	return TRUE;
    return FALSE;
}

/*
 * Return TRUE if neither of the endpoints of the arc are contained within the rectangle.
 */

static Boolean  _ArcJustCrossing 
#ifndef _NO_PROTO
  (XmArcWidget arc, XRectangle *rect)
#else
  (arc, rect) XmArcWidget arc; XRectangle *rect;
#endif
{
    if(!(PointInRect(arc->arc.from_x, arc->arc.from_y, rect) || PointInRect(arc->arc.to_x, arc->arc.to_y, rect)))
	return TRUE;
    return FALSE;
}

static Boolean ArcVisibleInGraph
#ifndef _NO_PROTO
  (XmGraphWidget graph, XmArcWidget arc)
#else
  (graph, arc) XmGraphWidget graph; XmArcWidget arc;
#endif
{ if(XmBeingDestroyed(arc))
	return FALSE;

    if(graph->graph.is_scrolled )
    {
	XRectangle rect;
	Widget to = arc->arc.to;
	Widget from = arc->arc.from;

	rect.x      = -graph->core.x;
	rect.y      = -graph->core.y;
	rect.width  = graph->graph.clip_width;
	rect.height = graph->graph.clip_height;

	if(!arc->arc.up_to_date) 
	{ float   x1, x2, y1, y2;
	     Dimension bwTo = XtBorderWidth(to) + XtBorderWidth(to);
	      Dimension bwFrom = XtBorderWidth(from) + XtBorderWidth(from);
	    _GetPoints (graph, RX(from), RY(from),
			RWidth(from)+bwFrom, RHeight(from)+bwFrom,
			RX(to), RY(to), 
			RWidth(to)+bwTo, RHeight(to)+bwTo,
			&x1, &y1, &x2, &y2);
	    /*
	     * Update widget.
	     */
	    arc->arc.from_x =  (int)  x1;
	    arc->arc.to_x   =  (int)  x2;
	    arc->arc.from_y =  (int)  y1;
	    arc->arc.to_y   =  (int)  y2;

	    arc->arc.up_to_date = TRUE;
	    FreeArcRegions (arc);
	}

	if ((!graph->graph.show_crossing_arcs && _ArcJustCrossing (arc, &rect)))
	    return FALSE;
	else if (!_ArcInRect(arc, &rect) )        /* If it doesn't intersect at all, stop here, return FALSE */
	    return FALSE;
	else 
	{  int rStatus;
	    if(!arc->arc.region) 
		ComputeRegionsForArc ((XmArcWidget)arc);
#if SESD
	    rStatus = XRectInRegion (arc->arc.region,
				     (int)-graph->core.x,
				     (int)-graph->core.y, 
				     (int)graph->graph.clip_width,
				     (int)graph->graph.clip_height);
	    FreeArcRegions(arc);
	    return (rStatus != RectangleOut);
#else

	    if (RectangleOut == XRectInRegion (arc->arc.region,
					       (int) -graph->core.x, (int) -graph->core.y, 
					       (int) graph->graph.clip_width, (int) graph->graph.clip_height))
		return FALSE;
	    else

		return TRUE;
#endif
	}
    }
    else                                    /* If we aren't in a scrolled window, the arc has to be visible */
#if SESD
    {
	FreeArcRegions(arc);
  	return TRUE;
    }
#else
	return TRUE;
#endif
}

static Region VisibleGraphRegion
#ifndef _NO_PROTO
  (XmGraphWidget graph)
#else
  (graph) XmGraphWidget graph;
#endif
{

#if SESD
    Region region;
#else
    Region region = XCreateRegion();
#endif
    XPoint points[4];

    if(graph->graph.is_scrolled )
    {
	points[0].x = -graph->core.x;
	points[0].y = -graph->core.y;
	points[1].x = graph->graph.clip_width - graph->core.x;
	points[1].y = - graph->core.y;
	points[2].x = graph->graph.clip_width - graph->core.x;
	points[2].y = graph->graph.clip_height - graph->core.y;
	points[3].x = -graph->core.x;
	points[3].y = graph->graph.clip_height - graph->core.y;
	region = XPolygonRegion (points, 4, EvenOddRule);
    }
    else 
    {
	points[0].x = 0;
	points[0].y = 0;
	points[1].x = XtWidth(graph);
	points[1].y = 0;
	points[2].x = XtWidth(graph);
	points[2].y = XtHeight(graph);
	points[3].x = 0;
	points[3].y = XtHeight(graph);
	region = XPolygonRegion (points, 4, EvenOddRule);
    }
    return region;
}


static void UpdateArcs
#ifndef _NO_PROTO
  (Widget w, Boolean uptodate)
#else
  (w, uptodate) Widget  w; Boolean uptodate;
#endif
{
    NodePtr       node = NODEPTR(w);
    XmArcWidgetList    arcs;
    XmArcWidget   arc;
    int           i, n_arcs;

    arcs = node->from_arcs.arcs; 
    n_arcs = node->from_arcs.n_arcs;
    for (i = 0; i < n_arcs; i++) 
    {
	arc = (XmArcWidget) arcs[i];
	arc->arc.up_to_date = uptodate;
    }

    arcs = node->to_arcs.arcs;
    n_arcs = node->to_arcs.n_arcs;
    for (i = 0; i < n_arcs; i++) 
    {
	arc = (XmArcWidget) arcs[i];
	arc->arc.up_to_date = uptodate;
    }
}

static void _RefreshGadgets
#ifndef _NO_PROTO
  (XmGraphWidget graph,Region region)
#else
  (graph,region) XmGraphWidget graph; Region region;
#endif
{
    XEvent event;
    XRectangle rect;
    int i;

    if(graph->core.visible &&
       !graph->core.being_destroyed && 
       XtIsRealized(graph)) 
    {
	event.xany.type = Expose;
	event.xany.display = XtDisplay(graph);
	event.xexpose.window = XtWindow(graph);

	XClipBox(region, &rect);

	event.xexpose.x = rect.x;
	event.xexpose.y = rect.y;
	event.xexpose.width = rect.width;
	event.xexpose.height = rect.height;

	if(graph->graph.is_scrolled )
	{
	    XRectangle rect;

	    rect.x      = -graph->core.x;
	    rect.y      = -graph->core.y;
	    rect.width  = graph->graph.clip_width;
	    rect.height = graph->graph.clip_height;

	    for (i = 1; i < graph->composite.num_children; i++)
	    {
		Widget  child = graph->composite.children[i];
		if (XmIsGadget(child) && XtIsManaged(child))
		{
		    if(rect.x < RX(child) + RWidth(child) &&
		       rect.y < RY(child) + RHeight(child) &&
		       rect.x + rect.width >  RX(child) &&
		       rect.y + rect.height> RY(child) &&
		       event.xexpose.x < RX(child) + RWidth(child) &&
		       event.xexpose.y < RY(child) + RHeight(child) &&
		       event.xexpose.x + event.xexpose.width >  RX(child) &&
		       event.xexpose.y + event.xexpose.height> RY(child))
		    {
			if (XRectInRegion (region, RX(child), RY(child),
					   RWidth(child), RHeight(child)))
			{
			    XmGadget g = (XmGadget) child;
			    if (g->object.widget_class->core_class.expose)
				(*(g->object.widget_class->core_class.expose)) (child, &event, region);
			}
		    }
		}
	    }
	}
	else
	{
	    for (i = 1; i < graph->composite.num_children; i++)
	    {
		Widget  child = graph->composite.children[i];
		if (XmIsGadget(child) && XtIsManaged(child))
		{
		    if (XRectInRegion (region, RX(child), RY(child),
				       RWidth(child), RHeight(child)))
		    {
			XmGadget g = (XmGadget) child;
			if (g->object.widget_class->core_class.expose)
			    (*(g->object.widget_class->core_class.expose)) (child, &event, region);
		    }
		}
	    }
	}
    }
}


static void  IndicateRegion
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{
    if ( graph->graph.edit_mode && graph->graph.allow_multiple_selections) 
    {

	XDefineCursor (XtDisplay (graph), XtWindow (graph),  HighLeftCursor (graph));

	graph->graph.current_action = REGION_INDICATED;
	graph->graph.delta_x = 0;
	graph->graph.delta_y = 0;
	graph->graph.start_x = event->xbutton.x;
	graph->graph.start_y = event->xbutton.y;

#if SESD
	DrawSelectBox(graph, graph->graph.start_x, graph->graph.start_y,
	                     graph->graph.delta_x, graph->graph.delta_y);
#else
	AddRectToList (graph,
		       graph->graph.start_x, graph->graph.start_y,
		       graph->graph.delta_x, graph->graph.delta_y);

	DrawRectList (graph);
#endif
    }
}


static void  SweepOutRegion
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{

    if ( graph->graph.edit_mode && graph->graph.allow_multiple_selections) 
    {
#if SESD
	DrawSelectBox(graph, graph->graph.start_x, graph->graph.start_y,
	                     graph->graph.delta_x, graph->graph.delta_y);

	graph->graph.delta_x = event->xbutton.x - graph->graph.start_x;
	graph->graph.delta_y = event->xbutton.y - graph->graph.start_y;
#endif

	if(0 < graph->graph.delta_x && 0 < graph->graph.delta_y)
	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  LowRightCursor (graph));
	else if (0 < graph->graph.delta_x)
	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  HighRightCursor (graph));
	else if (0 < graph->graph.delta_y)
	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  LowLeftCursor (graph));
	else
	    XDefineCursor (XtDisplay (graph), XtWindow (graph),  HighLeftCursor (graph));


#if SESD
	DrawSelectBox(graph, graph->graph.start_x, graph->graph.start_y,
	                     graph->graph.delta_x, graph->graph.delta_y);
#else
	AddRectToList (graph,
		       graph->graph.start_x, graph->graph.start_y,
		       graph->graph.delta_x, graph->graph.delta_y);

	DrawRectList (graph);

	graph->graph.delta_x = event->xbutton.x - graph->graph.start_x;
	graph->graph.delta_y = event->xbutton.y - graph->graph.start_y;

	AddRectToList (graph,
		       graph->graph.start_x, graph->graph.start_y,
		       graph->graph.delta_x, graph->graph.delta_y);

	DrawRectList (graph);
#endif
    }
}

static void  SelectRegion
#ifndef _NO_PROTO
  (XmGraphWidget graph, XEvent *event)
#else
  (graph, event) XmGraphWidget graph; XEvent *event;
#endif
{ Widget graphW=(Widget) graph;
    int i;
    Region region;
    XPoint points[4];
    XRectangle rect;
    if ( graph->graph.edit_mode && graph->graph.allow_multiple_selections) 
    {

	XDefineCursor (XtDisplay (graph), XtWindow (graph),  PtrCursor (graph));

	graph->graph.current_action = NORMAL;
	graph->graph.indicated_widget = NULL;

#if SESD
	DrawSelectBox(graph, graph->graph.start_x, graph->graph.start_y,
	                     graph->graph.delta_x, graph->graph.delta_y);
#else
	AddRectToList (graph,
		       graph->graph.start_x, graph->graph.start_y,
		       graph->graph.delta_x, graph->graph.delta_y);

	DrawRectList (graph);
#endif

	graph->graph.delta_x = event->xbutton.x - graph->graph.start_x;
	graph->graph.delta_y = event->xbutton.y - graph->graph.start_y;

	if(graph->graph.delta_x  < 0) 
	{
	    graph->graph.start_x += graph->graph.delta_x;
	    graph->graph.delta_x = - graph->graph.delta_x;
	}

	if(graph->graph.delta_y  < 0) 
	{
	    graph->graph.start_y += graph->graph.delta_y;
	    graph->graph.delta_y = - graph->graph.delta_y;
	}

	rect.x = points[0].x = graph->graph.start_x;
	rect.y = points[0].y = graph->graph.start_y;
	points[1].x = graph->graph.start_x + graph->graph.delta_x;
	points[1].y = graph->graph.start_y;
	points[2].x = graph->graph.start_x + graph->graph.delta_x;
	points[2].y = graph->graph.start_y + graph->graph.delta_y;
	points[3].x = graph->graph.start_x;
	points[3].y = graph->graph.start_y + graph->graph.delta_y;

	region = XPolygonRegion (points, 4, EvenOddRule);
	rect.width = graph->graph.delta_x;
	rect.height = graph->graph.delta_y;

	{ Widget childW;
	  XmArcWidget child;
	  for (i = 0 ; i < graph->graph.n_arcs ; i++) 
	  { 
	    child = graph->graph.arcs[i];
	    childW = (Widget) child;
#if SESD
	    if (child->arc.visible) 
#else
	    if (child->arc.visible && child->arc.region) 
#endif
	    {
		if(PointInRect(child->arc.from_x, child->arc.from_y, &rect) &&
		   PointInRect(child->arc.to_x, child->arc.to_y, &rect))
		    XmGraphSelectArc(graphW, childW);
	    }
	  }

	  for (i = 0 ; i < graph->composite.num_children ; i++) 
	  { childW = graph->composite.children[i];

	    if (RectangleIn == XRectInRegion (region, 
					      RX(childW), RY(childW),
					      RWidth(childW), RHeight(childW)))
	    XmGraphSelectNode(graphW, childW);
	  }
	}

	XDestroyRegion(region);

	if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNselectNodeCallback))
	{
	    XmGraphCallbackStruct    cb;

	    cb.event  = event;

	    cb.interactive = TRUE;

	    cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

	    cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);

	    if(cb.num_selected_widgets && cb.num_selected_arcs) 
	    {
		cb.reason = XmCR_SELECT_ARCS_AND_NODES;
		cb.widget = cb.selected_widgets[0];
		XtCallCallbacks (graphW, XmNselectNodeCallback, &cb);
		cb.widget = cb.selected_arcs[0];
		XtCallCallbacks (graphW, XmNselectArcCallback, &cb);
	    }
	    else if(cb.num_selected_widgets == 1) 
	    {
		cb.reason = XmCR_SELECT_NODE;
		cb.widget = cb.selected_widgets[0];
		XtCallCallbacks (graphW, XmNselectNodeCallback, &cb);
	    }
	    else if(cb.num_selected_widgets) 
	    {
		cb.reason = XmCR_SELECT_NODES;
		cb.widget = cb.selected_widgets[0];
		XtCallCallbacks (graphW, XmNselectNodeCallback, &cb);
	    }
	    else if(cb.num_selected_arcs == 1) 
	    {
		cb.reason = XmCR_SELECT_ARC;
		cb.widget = cb.selected_arcs[0];
		XtCallCallbacks (graphW, XmNselectArcCallback, &cb);
	    }
	    else if(cb.num_selected_widgets) 
	    {
		cb.reason = XmCR_SELECT_ARCS;
		cb.widget = cb.selected_arcs[0];
		XtCallCallbacks (graphW, XmNselectArcCallback, &cb);
	    }
	}
    }
}


static Cursor CreateCursor
#ifndef _NO_PROTO
  (Widget graph, char *bm, char *mask, 
			   int width, int height, int x_hot, int y_hot)
#else
  (graph, bm, mask, width, height, x_hot, y_hot)
	Widget graph; char * bm, * mask; int width, height, x_hot, y_hot;
#endif
{
    Display *dpy = XtDisplay(graph);
    int screen   = DefaultScreen(dpy);

    Window root  = RootWindow(dpy, screen);
    Pixmap s     = XCreateBitmapFromData(dpy, root, bm, width, height);
    Pixmap m     = XCreateBitmapFromData(dpy, root, mask, width, height);
    Colormap cmap = DefaultColormap(dpy, screen);
    XColor  black,  white;
    Cursor  cursor;

    black.pixel = BlackPixel(dpy, screen);
    XQueryColor(dpy, cmap, &black);

    white.pixel = WhitePixel(dpy, screen);
    XQueryColor(dpy, cmap, &white);

    cursor = XCreatePixmapCursor(dpy, s, m, &black, &white, x_hot, y_hot);

    XFreePixmap(dpy, s);
    XFreePixmap(dpy, m);

    return cursor;
}


/********
 *
 * RelaySubgraph: Will call the layout algorithm on the subgraph
 *                rooted at w. w must be a node widget.
 *                w will preserve its current position.
 *********/

void XmGraphRelaySubgraph 
#ifndef _NO_PROTO
  (Widget graphW, Widget w)
#else
  (graphW, w) Widget       graphW, w;
#endif
{ XmGraphWidget graph= (XmGraphWidget) graphW;

    NodePtr  node = NODEPTR(w);
    Position delta_x, delta_y;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) {
	XtWarning("XmGraphRelaySubgraph requires an XmGraph widget");
	return;
    }

    if (node) {
	int         i, j, n_nodes, n_arcs,
	x = RX(w),
	y = RY(w);
	XmArcWidgetList  arcs;
	NList       nodes;
	NodePtr     next,
	dummy = graph->graph.root;  /* saves old root */
	Arg         wargs [2];

	graph->graph.layed_out = FALSE;

	/* CYY added; have to reset to zero */
	subgraph_nodes.n_nodes=0;
	/* 
	 * get list of nodes in subgraph 
	 */
	GetSubgraphNodeList (node);

	n_nodes = subgraph_nodes.n_nodes;
	nodes   = subgraph_nodes.nodes;

	/* 
	 * Erase arcs 
	 */
	for (i = 0; i < n_nodes; i++) 
	{
	    n_arcs = nodes[i]->to_arcs.n_arcs;
	    arcs   = nodes[i]->to_arcs.arcs;
	    for (j = 0; j < n_arcs; j++) 
		_EraseArc ((XmArcWidget) arcs [j]);
	}

	/*
	 * prepare nodes for relaying 
	 */
	for (i = 0; i < n_nodes; i++) 
	{
	    next = nodes [i];
	    next->visited     = FALSE;
	    next->tree_parent = NULL;
	    next->level       = LARGE_NUM;
	    next->tree_kids.n_nodes = 0;

	    XtSetArg (wargs[0], XmNmappedWhenManaged, FALSE);
	    XtSetValues (next->widget, wargs, 1); 

	    for (j = 0; j < next->kids.n_nodes; j++)
		NodeListInsert (&(next->tree_kids), next->kids.nodes[j]);
	}


	graph->graph.root = node;

	/*
	 * unmap root of subtree temporarily
	 */
	XtSetArg (wargs[0], XmNmappedWhenManaged, FALSE);
	XtSetValues (w, wargs, 1);

	convertToTree (graph);

	if (graph->graph.direction == XmHORIZONTAL) 
	    horizontalLayout (graph, node, x , y);
	else
	    verticalLayout (graph, node, x, y);

	/* 
	 * restore old root 
	 */
	graph->graph.root = dummy;


	delta_x = (graph->graph.direction == XmVERTICAL) ? (RX(w) - x) : 0;
	delta_y = (graph->graph.direction == XmHORIZONTAL)   ? (RY(w) - y) : 0;

	for (i = 0; i < n_nodes; i++) 
	{
	    Widget child = nodes [i]->widget;

#if (XmREVISION==1)
	    _XmMoveObject ((RectObj)child, RX(child) - delta_x, RY(child) - delta_y); 
#else
	    _XmMoveObject (child, RX(child) - delta_x, RY(child) - delta_y); 
#endif


	    XtSetArg (wargs[0], XmNmappedWhenManaged, TRUE);
	    XtSetValues (child, wargs, 1); 
	}

	UpdateArcs(w, FALSE);

	XtSetArg (wargs[0], XmNmappedWhenManaged, TRUE);
	XtSetValues (w, wargs, 1);

	/* 
	 * Redisplay arcs 
	 */

	_InitArcList(graph);  /* clear line list */
	graph->graph.batch_drawing_mode = TRUE;

	for (i = 0; i < n_nodes; i++) 
	{
	    n_arcs = nodes[i]->to_arcs.n_arcs; arcs = nodes[i]->to_arcs.arcs;

	    for (j = 0; j < n_arcs; j++)
		(* arcs[j]->core.widget_class->core_class.expose) 
		     ((Widget)arcs[j], NULL, NULL);
	}

	_InitArcList(graph);  /* clear line list */
	graph->graph.batch_drawing_mode = FALSE;

	/*
	 *  Clean up subgraph_list for next time 
	 */

	for (i = 0; i < n_nodes; i++) 
	    nodes [i] = NULL;
	subgraph_nodes.n_nodes = 0;

	AdjustSize (graph);
    }
}



/*********
 *
 * GetRoots: Inserts all nodes in user_roots in nodes.
 *           nodes must be of adequate size.
 *
 *********/
WidgetList XmGraphGetRoots 
#ifndef _NO_PROTO
  (Widget graphW, int *num_nodes)
#else
  (graphW, num_nodes) Widget graphW; int     *num_nodes;
#endif
{ XmGraphWidget  graph=(XmGraphWidget)  graphW;
    int i;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGetRoots requires an XmGraph widget");
	return NULL;
    }

    if(!graph->graph.user_roots.n_nodes)
    {
	*num_nodes = 0;
	return NULL;
    }

    *num_nodes = graph->graph.user_roots.n_nodes;

    return_widget_list.n_arcs = 0;

    for(i=0;i<graph->graph.user_roots.n_nodes; i++)
    {
	/* NodeListInsert ? */
	ArcListInsert(&return_widget_list, 
		 	(XmArcWidget) graph->graph.user_roots.nodes[i]->widget);
    }

    return (WidgetList) return_widget_list.arcs;
    /* what has  this function  got to do with Arcs? */

}

/********
 *
 * InsertRoots: Inserts given nodes in the user_roots list
 *
 *********/
void XmGraphInsertRoots 
#ifndef _NO_PROTO
  (Widget graphW, WidgetList nodes, int n_nodes)
#else
  (graphW, nodes, n_nodes) Widget  graphW; WidgetList nodes; int n_nodes;
#endif
{ XmGraphWidget  graph = ( XmGraphWidget)  graphW;
    NodePtr next;
    int     i;

    for (i = 0; i < n_nodes; i++) 
    {
	next = NODEPTR(nodes [i]);
	NodeListInsertNoDuplicates (&(graph->graph.user_roots), next);
    }
}

/*********
 *
 * RemoveRoots: Removes given nodes from the user_roots list
 *
 *********/
void XmGraphRemoveRoots 
#ifndef _NO_PROTO
  (Widget graphW, WidgetList nodes, int n_nodes)
#else
  (graphW, nodes, n_nodes) Widget  graphW; WidgetList nodes; int n_nodes;
#endif
{ XmGraphWidget  graph = (XmGraphWidget)  graphW;
  NodePtr next;
  int     i;

  for (i = 0; i < n_nodes; i++) 
  { next = NODEPTR (nodes [i]);
    NodeListRemove (&(graph->graph.user_roots), next);
  }
}

/*********
 *
 * NumSelectedNodes: returns the total number of selected node
 *                   widgets in graph.
 *
 *********/
int XmGraphNumSelectedNodes 
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget  graphW;
#endif
{ XmGraphWidget  graph = (XmGraphWidget)  graphW;
  return graph->graph.selected_nodes.n_nodes;
}

/*********
 *
 * GetSelectedNodes: Returns all selected nodes in selected_nodes in nodes.
 *                   nodes must be of adequate size.
 *
 *********/

WidgetList XmGraphGetSelectedNodes
#ifndef _NO_PROTO
  (Widget graphW, int *num)
#else
  (graphW, num) Widget graphW; int           *num;
#endif
{ XmGraphWidget  graph= (XmGraphWidget) graphW;
    int i;
    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphGetSelectedNodes requires an XmGraph widget");
	return NULL;
    }

    *num =  graph->graph.selected_nodes.n_nodes;

    if(!*num)
	return NULL;

    return_widget_list.n_arcs = 0;

    for(i=0;i<graph->graph.selected_nodes.n_nodes; i++)
    {
	ArcListInsert(&return_widget_list, 
		     (XmArcWidget)  graph->graph.selected_nodes.nodes[i]->widget);
    }

    return (WidgetList)return_widget_list.arcs;

}


/**********
 *
 * DestroyAllArcs:
 * 
 ***********/
void XmGraphDestroyAllArcs
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget graphW;
#endif
{ XmGraphWidget    graph= (XmGraphWidget)    graphW;
    int i, n_arcs;
    XmArcWidgetList arcs;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphDestroyAllArcs requires an XmGraph widget");
	return;
    }

    n_arcs = graph->graph.n_arcs;

    arcs = (XmArcWidgetList) XtMalloc( sizeof(Widget) * n_arcs);

    for (i = 0; i < n_arcs; i++)
	arcs[i] = graph->graph.arcs[i];

    for (i = 0; i < n_arcs; i++)
    {
	XtDestroyWidget((Widget)arcs[i]);
    }

    XtFree( (char *)arcs);

    if(graph->graph.arcs) XtFree ((char *) graph->graph.arcs);

    graph->graph.arcs = NULL;
    graph->graph.n_arcs = 0;
    graph->graph.n_arc_slots = 0;
}

/**********
 *
 * DestroyAllNodes:
 * 
 ***********/

void XmGraphDestroyAllNodes
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget    graphW;
#endif
{ XmGraphWidget    graph= (XmGraphWidget)    graphW;
    int i, n_nodes;
    WidgetList nodes;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphDestroyAllNodes requires an XmGraph widget");
	return;
    }

    /*
     * We have to make a copy of the list of widgets to be destroyed, because
     * the DeleteChild method munges the list, creating all kinds of problems.
     */

    n_nodes = graph->composite.num_children;

    nodes = (WidgetList) XtMalloc(sizeof(Widget) * n_nodes);

    for (i = 0; i < n_nodes; i++)
	nodes[i] = (Widget) graph->composite.children[i];

    for (i = 0; i < n_nodes; i++)
    {
	if(graph->composite.children[i] != graph->graph.root->widget)
	    XtDestroyWidget(nodes[i]);
    }

    XtFree( (char *)nodes);
}



/*************************************<->*************************************
 * XmGraphRemoveArcBetweenNodes
 *  
 *  
 *
 *************************************<->***********************************/

void XmGraphRemoveArcBetweenNodes
#ifndef _NO_PROTO
  (Widget graphW, Widget widget1, Widget widget2)
#else
  (graphW, widget1, widget2) Widget graphW; Widget widget1, widget2;
#endif
{
    int i,j;
    NodePtr      node1, node2;
    XmArcWidgetList  list1, list2;
    int n_arcs1, n_arcs2;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphRemoveArcBetweenNodes requires an XmGraph widget");
	return;
    }

    node1 = NODEPTR(widget1);
    node2 = NODEPTR(widget2);

    list1 = node1->from_arcs.arcs;
    n_arcs1 = node1->from_arcs.n_arcs;

    list2 = node2->to_arcs.arcs;
    n_arcs2 = node2->to_arcs.n_arcs;


    for (i = 0; i < n_arcs1; i++)
	for (j = 0 ; j < n_arcs2; j++)
	    if(list1[i] == list2[j])  
		XtDestroyWidget ((Widget)list1[i]);
}



/********************************************************************
 *
 * CONVENIENCE FUNCTIONS. USED BY APPLICATIONS.
 *
 *********************************************************************/


/*************************************<->*************************************
 *
 * XmGraphGetArcsBetweenNodes
 *
 * Inserts into the arcs widget list all arcs connecting from and to widgets.
 *
 * The space allocated for "arcs" must be large enough to hold the arcs.
 *
 * 900719 coha:  NOTE:  This routine returns a pointer to space allocated
 * in the heap.  When you are finished using the list of arcs, use XtFree
 * to reclaim the space.
 *************************************<->***********************************/
WidgetList XmGraphGetArcsBetweenNodes 
#ifndef _NO_PROTO
  (Widget graphW, Widget from, Widget to,
					int *num_arcs)
#else
  (graphW, from, to, num_arcs) Widget graphW; Widget from, to; int *num_arcs;
#endif
{ NodePtr fnode = NODEPTR(from);
  NodePtr tnode = NODEPTR(to);
  int     i, j = 0;
  ArcList f_arcs;
  XmArcWidgetList     arcs;  

    /*
     * First count the number of arcs we have.
     */
    j = 0;

    if (fnode && tnode) 
    {
	f_arcs = fnode->from_arcs;
	for (i = 0; i < f_arcs.n_arcs ; i++)
	{
	    if (( (f_arcs.arcs[i]))->arc.to == to) 
	    {
		j++;
	    }
	}
    }

    if (!j) 
    {
	*num_arcs = 0;
	return NULL;
    }

    arcs = (XmArcWidgetList) XtMalloc(sizeof(XmArcWidget) * j);

    /* 900719 coha:  add reinitialization of j to zero for second loop.  */
    j = 0;

    if (fnode && tnode) 
    {
	f_arcs = fnode->from_arcs;
	for (i = 0; i < f_arcs.n_arcs ; i++)
	{
	    if (((XmArcWidget) (f_arcs.arcs[i]))->arc.to == to) 
	    {
		arcs [j] = f_arcs.arcs[i];
		j++;
	    }
	}
    }

    *num_arcs = j;
    return (WidgetList) arcs;

}

/**********
 *
 * CreateArc: creates and returns a new arc widget.
 *
 **********/
Widget  XmCreateAttachedArc 
#ifndef _NO_PROTO
  (Widget graph, char *name, Widget from, Widget to, 
			    ArgList args, Cardinal n_args)
#else
  (graph, name, from, to, args, n_args)
	Widget  graph; char    *name;
	Widget  from; Widget  to; ArgList args; unsigned int     n_args;
#endif
{
    Widget arc;
    Arg    wargs[2];
    int    n = 0;

    XtSetArg(wargs[n], XmNto, to); n++;
    XtSetArg(wargs[n], XmNfrom, from); n++;

    arc =  XtCreateWidget (name, xmArcWidgetClass, graph, wargs, n);

    XtSetValues(arc, args, n_args);

    return arc;
}


/**********
 *
 * MoveNode: Moves node to given coords.
 *
 **********/
Boolean  XmGraphMoveNode 
#ifndef _NO_PROTO
  (Widget graphW, Widget w, Position x, Position y)
#else
  (graphW, w, x, y) Widget  graphW; Widget         w; Position       x, y;
#endif
{ XmGraphWidget  graph=(XmGraphWidget)  graphW;
  NodePtr  node = NODEPTR(w);

  if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
  {
	XtWarning("XmGraphMoveNode requires an XmGraph widget");
	return FALSE;
  }

    if (node) 
    {
	XmArcWidgetList  arcs;
	XmArcWidget   arc;
	Widget   arcW;
#if SESD
	Region      region = VisibleGraphRegion(graph);
#else
	Region      region = XCreateRegion();
#endif
	int         n_arcs, i;

	_InitArcList(graph);  /* clear line list */
	graph->graph.batch_drawing_mode = TRUE;

	/* 
	 * Erase arcs from old position 
	 */

	arcs = node->from_arcs.arcs; n_arcs = node->from_arcs.n_arcs;
	for (i = 0; i < n_arcs; i++) 
#if SESD
	    _EraseArc (arcs[i]);
#else
	{
	    arc =  arcs[i];

	    if (arc->arc.region && arc->arc.visible)
		region = _AddRegionToRegion (region, arc->arc.region);

	    _EraseArc (arc);
	}
#endif

	arcs = node->to_arcs.arcs;
	n_arcs = node->to_arcs.n_arcs;
	for (i = 0; i < n_arcs; i++) 
#if SESD
	    _EraseArc (arcs[i]);
#else
	{
	    arc =  arcs[i];
	    if (arc->arc.region && arc->arc.visible) 
		region = _AddRegionToRegion (region, arc->arc.region);

	    _EraseArc (arc);
	}
#endif

	_InitArcList(graph);  /* clear line list */
	graph->graph.batch_drawing_mode = FALSE;

#if (XmREVISION==1)
	_XmMoveObject ((RectObj)w,  x,  y);
#else
	_XmMoveObject (w,  x,  y);
#endif
	UpdateArcs(w, FALSE);

	/* 
	 * Redisplay arcs in new position 
	 */

	_InitArcList(graph);  /* clear line list */
	graph->graph.batch_drawing_mode = TRUE;

	arcs = node->from_arcs.arcs;
	n_arcs = node->from_arcs.n_arcs;
	for (i = 0; i < n_arcs; i++) 
	{
	    arc = arcs[i];
	    arcW =  (Widget)arc;

	    arcW->core.visible = ArcVisibleInGraph(graph, arc);

	    if(arcW->core.visible) 
		(* arcW->core.widget_class->core_class.expose) (arcW, NULL, NULL);
	    else 
		FreeArcRegions(arc);
	}

	arcs = node->to_arcs.arcs; n_arcs = node->to_arcs.n_arcs;
	for (i = 0; i < n_arcs; i++) 
	{ arc = arcs[i];
 	  arcW = (Widget)arc;

	    arc->core.visible = ArcVisibleInGraph(graph, arc);

	    if(arc->core.visible) 
		(* arc->core.widget_class->core_class.expose) (arcW, NULL, NULL);
	    else
		FreeArcRegions(arc);
	}

	/*  refreshes those underneath */
	_RefreshGadgets(graph, region);

	_InitArcList(graph);  /* clear line list */
	graph->graph.batch_drawing_mode = FALSE;

	_RefreshArcs (graph, region);

	XDestroyRegion(region);

	return TRUE;
    }
    return FALSE;
}

/**********
 *
 * MoveArc: Resets arc to and from to the node widgets supplied.
 *
 **********/
Boolean  XmGraphMoveArc 
#ifndef _NO_PROTO
  (Widget graphW, Widget arcW, Widget from, Widget to)
#else
  (graphW, arcW, from, to) Widget  graphW; Widget  arcW; Widget from, to;
#endif
{ XmGraphWidget  graph=(XmGraphWidget)  graphW;
  XmArcWidget    arc  =(XmArcWidget)    arcW;

    int       i, n_siblings = 0, rank = _sibling_rank (arc);
    NodePtr   old_node, to_node, from_node;
    XmArcWidgetList    list = NULL;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning(" XmGraphMoveArc requires an XmGraph widget");
	return FALSE;
    }

    if ((arc->arc.from == from) && (arc->arc.to == to))
	/* do nothing */
	return FALSE;

    if(arc->arc.from == NULL || arc->arc.to == NULL) 
    {
	return TRUE;
    }

    arc->core.visible = TRUE;  /* make sure it is displayable */


    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = TRUE;  

    if (arc->arc.siblings) 
    {
	n_siblings = arc->arc.siblings->n_arcs;
	list = (XmArcWidgetList) XtMalloc(sizeof(Widget) * n_siblings);
	for (i = (rank + 1); i < arc->arc.siblings->n_arcs; i++)
	{
	    list [i] = arc->arc.siblings->arcs[i];
	    _EraseArc (list[i]);
	}
    }
    /* erase old arc */
    _EraseArc (arc);


    RemoveFromSiblings (arc);

    /* 
     * reorder the old siblings 
     */

    if(list) 
    {
	for (i = (rank + 1); i < n_siblings; i++) 
	    (* list[i]->core.widget_class->core_class.expose) 
		((Widget)(list[i]), NULL, NULL);
	XtFree((char *) list);
    }


    /*
     *  refresh arcs that were underneath 
     */
    _RefreshGadgets(graph, arc->arc.region);  

    _InitArcList(graph);  /* clear line list */
    graph->graph.batch_drawing_mode = FALSE;


    _RefreshArcs (graph, arc->arc.region);


    if (arc->arc.from != from) 
    {
	/* We are changing from_node */
	old_node = NODEPTR((Widget) arc->arc.from);
	arc->arc.from = from; 

	from_node = NODEPTR(arc->arc.from);
	to_node = NODEPTR(arc->arc.to);
	NodeListRemove (&(old_node->kids), to_node);
	NodeListRemove (&(old_node->tree_kids), to_node);
	ArcListRemove  (&(old_node->from_arcs), arc);
	NodeListRemove (&(to_node->parents), old_node);
	ArcListInsert  (&(from_node->from_arcs), arc);

    }
    if (arc->arc.to != to) 
    {
	/* We are changing to_node */
	old_node = NODEPTR(arc->arc.to);
	arc->arc.to = to;
	from_node = NODEPTR(arc->arc.from);
	to_node = NODEPTR(arc->arc.to);

	NodeListRemove (&(old_node->parents), from_node);
	ArcListRemove (&(old_node->to_arcs), arc);
	NodeListRemove (&(from_node->tree_kids), old_node);
	NodeListRemove (&(from_node->kids), old_node);
	ArcListInsert (&(to_node->to_arcs), arc);

    }

    NodeListInsert (&(from_node->kids), to_node);
    NodeListInsert (&(from_node->tree_kids), to_node);
    NodeListInsert (&(to_node->parents), from_node);
    CheckForSiblings (arc);
    UpdateArcs(to, FALSE); 
    UpdateArcs(from, FALSE);  
    /* finally draw it */

    if(graph->graph.auto_layout_mode == XmPARTIAL ||
       graph->graph.auto_layout_mode  == XmARCS_ONLY ||
       graph->graph.auto_layout_mode  == XmALWAYS)
    {
	Widget poppa = from_node->widget;
	XmGraphRelaySubgraph (graphW, poppa);
    }
    else
	(* arc->core.widget_class->core_class.expose)  (arcW, NULL, NULL);

    return TRUE;
}

/**********
 *
 * CreateGraph: Creates and returns a new graph widget instance.
 *
 **********/
Widget XmCreateGraph 
#ifndef _NO_PROTO
  (Widget parent, String name, 
			ArgList arglist, Cardinal argcount)
#else
  (parent, name, arglist, argcount) Widget    parent; String    name;
		ArgList   arglist; Cardinal  argcount;
#endif
{
    return XtCreateWidget (name, xmGraphWidgetClass, parent, arglist, argcount);
}

/*********
 *
 * CreateManagedGraph: Creates and returns a new managed
 *                     graph widget instance.
 *
 *********/
Widget XmCreateManagedGraph 
#ifndef _NO_PROTO
  (Widget parent, String  name, 
			     ArgList arglist, Cardinal argcount)
#else
  (parent, name, arglist, argcount) Widget    parent; String    name;
		ArgList   arglist; Cardinal  argcount;
#endif
{
    return XtCreateManagedWidget (name, xmGraphWidgetClass, parent, arglist, argcount);
}

/********
 *
 * NumNodeArcs: Assigns to n_from and n_to the number of
 *          from and to arcs respectively.
 ********/
XmGraphNumNodeArcs 
#ifndef _NO_PROTO
  (Widget graphW, Widget w, int *n_from, int *n_to)
#else
  (graphW, w, n_from, n_to) Widget   graphW; Widget w; int *n_from, *n_to;
#endif
{ 
    NodePtr  node = NODEPTR(w);

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning(" XmGraphNumNodeArcs requires an XmGraph widget");
	return;
    }

    if (node) 
    {
	*n_from = node->from_arcs.n_arcs;
	*n_to = node->to_arcs.n_arcs;
    }
}

/********
 *
 * GetNodeArcs: 
 *
 ********/

void XmGraphGetNodeArcs 
#ifndef _NO_PROTO
  (Widget graphW, Widget w, 
			 WidgetList *from, WidgetList *to, 
			 int *n_from, int *n_to)
#else
  (graphW, w, from, to, n_from, n_to) Widget  graphW; Widget w;
	WidgetList *from, *to; int *n_from, *n_to;
#endif
{ NodePtr node = NODEPTR (w);

  if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
  { XtWarning(" XmGraphGetNodeArcs requires an XmGraph widget");
	return;
  }

  *n_from = node->from_arcs.n_arcs;
  *n_to = node->to_arcs.n_arcs;

  if (node) 
  { *from = (WidgetList) node->from_arcs.arcs;
    *to   = (WidgetList) node->to_arcs.arcs;
  }
}

/********
 *
 * GetArcNodes: Returns "from" and "to" pointing to the from and to
 *              widgets of arc.
 *
 *********/
void XmGraphGetArcNodes 
#ifndef _NO_PROTO
  (Widget graphW, Widget arcW, Widget *from, Widget *to)
#else
  (graphW, arcW, from, to) Widget  graphW; Widget  arcW; Widget  *from, *to;
#endif
{ XmArcWidget arc = (XmArcWidget) arcW;

    *from = arc->arc.from;
    *to = arc->arc.to;
}

/*********
 *
 * XmGraphSelectArc: Inserts first n_arcs arc widgets in arcs into
 *             the selected_arcs list.
 *   
 **********/
void XmGraphSelectArc 
#ifndef _NO_PROTO
  (Widget graphW, Widget arcW)
#else
  (graphW, arcW) Widget  graphW; Widget         arcW;
#endif
{ XmGraphWidget  graph=( XmGraphWidget)  graphW;
  XmArcWidget arc = (XmArcWidget) arcW;

  if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
  { XtWarning("XmGraphSelectArc requires an XmGraph widget");
    return;
  }

  ArcListInsertNoDuplicates (&(graph->graph.selected_arcs), arc);
  _XmHighlightArc (arc);
}

/*********
 *
 * SelectArcs: Inserts first n_arcs arc widgets in arcs into
 *             the selected_arcs list.
 *   
 **********/
void XmGraphSelectArcs 
#ifndef _NO_PROTO
  (Widget graphW, WidgetList arcsW, int n_arcs)
#else
  (graphW, arcsW, n_arcs) Widget graphW; WidgetList arcsW; int n_arcs;
#endif
{ XmGraphWidget  graph= (XmGraphWidget)  graphW;
  XmArcWidgetList arcs= (XmArcWidgetList)  arcsW;
  int i;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphSelectArcs requires an XmGraph widget");
	return;
    }

    for (i = 0; i < n_arcs; i++) 
    {
	_XmHighlightArc (arcs[i]);
	ArcListInsertNoDuplicates (&(graph->graph.selected_arcs), arcs[i]);
    }
}


/**********
 *
 * XmGraphUnselectArc: Remove given arcs from arc_selected list.
 *
 **********/
void XmGraphUnselectArc 
#ifndef _NO_PROTO
  (Widget graphW, Widget arcW)
#else
  (graphW, arcW) Widget  graphW; Widget  arcW;
#endif
{ XmGraphWidget  graph=( XmGraphWidget)  graphW;
  XmArcWidget arc = (XmArcWidget) arcW;
    if (!XtIsSubclass(graphW,xmGraphWidgetClass))  
    {
	XtWarning("XmGraphUnselectArc requires an XmGraph widget");
	return;
    }

    _XmUnhighlightArc(arc);
    ArcListRemove (&(graph->graph.selected_arcs), arc);
}

/**********
 *
 * UnSelectArcs: Remove given arcs from arc_selected list.
 *
 **********/
void XmGraphUnselectArcs 
#ifndef _NO_PROTO
  (Widget graphW, WidgetList arcsW, int n_arcs)
#else
  (graphW, arcsW, n_arcs) Widget  graphW; WidgetList arcsW; int n_arcs;
#endif
{ XmGraphWidget  graph=( XmGraphWidget ) graphW;
  XmArcWidgetList arcs = (XmArcWidgetList) arcsW;
    int i;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphUnselectArcs requires an XmGraph widget");
	return;
    }

    for (i = 0; i < n_arcs; i++) 
    {
	_XmUnhighlightArc(arcs[i]);
	ArcListRemove (&(graph->graph.selected_arcs), arcs[i]);
    }
}

/*********
 *
 * NumSelectedArcs: returns the number of selected arcs in graph.
 *
 **********/
int XmGraphNumSelectedArcs 
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget  graphW;
#endif
{ XmGraphWidget  graph=( XmGraphWidget ) graphW;
    if (!XtIsSubclass(graphW,xmGraphWidgetClass))  
    {
	XtWarning("XmGraphNumSelectedArcs requires an XmGraph widget");
	return 0;
    }

    return graph->graph.selected_arcs.n_arcs;
}

/*********
 *
 * GetSelectedArcs: Inserts all arcs in selected_arcs in arcs.
 *                   arcs must be of adequate size.
 *********/
WidgetList XmGraphGetSelectedArcs 
#ifndef _NO_PROTO
  (Widget graphW, int *num_arcs)
#else
  (graphW, num_arcs) Widget  graphW; int *num_arcs;
#endif
{ XmGraphWidget  graph=( XmGraphWidget ) graphW;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphGetSelectedArcs requires an XmGraph widget");
	return NULL;
    }

    *num_arcs = graph->graph.selected_arcs.n_arcs;

    if(!*num_arcs)
	return NULL;

    return (WidgetList) graph->graph.selected_arcs.arcs;
}

/*********
 *
 * IsSelectedArc: 
 *
 *
 *********/
Boolean XmGraphIsSelectedArc 
#ifndef _NO_PROTO
  (Widget graphW, Widget arcW)
#else
  (graphW, arcW) Widget  graphW; Widget  arcW;
#endif
{
XmGraphWidget  graph=( XmGraphWidget ) graphW;
    int i, n;
    XmArcWidgetList  s_arcs;
    XmArcWidget  arc = (XmArcWidget) arcW;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphIsSelectedArc requires an XmGraph widget");
	return 0;
    }

    n = graph->graph.selected_arcs.n_arcs;
    s_arcs = graph->graph.selected_arcs.arcs;

    for (i = 0; i < n; i++)
	if(arc == s_arcs[i])
	    return TRUE;
    return FALSE;
}


/*********
 *
 * XmGraphNumArcs: returns the total number of arc widgets in graph.
 *
 *********/
int XmGraphNumArcs 
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget  graphW;
#endif
{ XmGraphWidget  graph=( XmGraphWidget ) graphW;
    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphNumArcs requires an XmGraph widget");
	return 0;
    }

    return graph->graph.n_arcs;
}

/*********
 *
 * XmGrapGethArcs: 
 *
 *********/

WidgetList XmGraphGetArcs 
#ifndef _NO_PROTO
  (Widget graphW, int *num_arcs)
#else
  (graphW, num_arcs) Widget  graphW; int *num_arcs;
#endif
{ XmGraphWidget  graph=( XmGraphWidget ) graphW;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphGetArcs requires an XmGraph widget");
	return NULL;
    }

    if(!graph->graph.n_arcs) 
    {
	*num_arcs = 0;
	return NULL;
    }
    *num_arcs = graph->graph.n_arcs;

    return (WidgetList) graph->graph.arcs;
}


/*********
 *
 * NumRoots: returns the total number of user set root
 *           widgets in graph.
 *********/
int XmGraphNumRoots 
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget  graphW;
#endif
{ XmGraphWidget graph = (XmGraphWidget) graphW;
    return graph->graph.user_roots.n_nodes;
}


/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

void XmGraphDestroySelectedArcsOrNodes 
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget  graphW;
#endif
{ XmGraphWidget graph = (XmGraphWidget) graphW;
    int i, n_nodes = graph->graph.selected_nodes.n_nodes;
    NList  s_nodes = graph->graph.selected_nodes.nodes;
    int n_arcs;
    XmArcWidgetList  s_arcs;

    for (i = 0; i < n_nodes; i++)
	XtDestroyWidget(((NodePtr) s_nodes [i])->widget);

    n_arcs = graph->graph.selected_arcs.n_arcs;
    s_arcs = graph->graph.selected_arcs.arcs;

    for (i = 0; i < n_arcs; i++)
	if(!s_arcs[i]->core.being_destroyed)
	    XtDestroyWidget((Widget)(s_arcs [i]));

}

/*********
 *
 * IsSelectedNode: 
 *
 *
 *********/
Boolean XmGraphIsSelectedNode 
#ifndef _NO_PROTO
  (Widget graphW, Widget node)
#else
  (graphW, node) Widget  graphW; Widget         node;
#endif
{
XmGraphWidget  graph=(XmGraphWidget) graphW;
    int i, n = graph->graph.selected_nodes.n_nodes;
    NList  s_nodes = graph->graph.selected_nodes.nodes;

    for (i = 0; i < n; i++)
	if(node == ((NodePtr) s_nodes[i])->widget)
	    return TRUE;
    return FALSE;
}

/********
 *
 * SelectNodes: Inserts given nodes in the selected_nodes list
 *
 ********/
void XmGraphSelectNodes 
#ifndef _NO_PROTO
  (Widget graphW, WidgetList nodes, int n_nodes)
#else
  (graphW, nodes, n_nodes) Widget  graphW; WidgetList nodes; int n_nodes;
#endif
{ XmGraphWidget  graph=(XmGraphWidget)  graphW;
    NodePtr next;
    int     i;

    for (i = 0; i < n_nodes; i++) {
	next = NODEPTR (nodes [i]);
#if SESD
	if (next == graph->graph.root) return;
#endif
	_XmHighlightBorder(nodes[i]);
	NodeListInsertNoDuplicates (&(graph->graph.selected_nodes), next);
    }
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

void XmGraphSelectNode 
#ifndef _NO_PROTO
  (Widget graphW, Widget node_widget)
#else
  (graphW, node_widget) Widget  graphW; Widget    node_widget;
#endif
{ XmGraphWidget  graph=(XmGraphWidget)  graphW;

  NodePtr next = NODEPTR(node_widget);
  _XmHighlightBorder(node_widget);
  NodeListInsertNoDuplicates (&(graph->graph.selected_nodes), next);
}

/********
 *
 * UnSelectNodes: Removes given nodes from the selected_nodes list
 *
 ********/
void XmGraphUnselectNodes 
#ifndef _NO_PROTO
  (Widget graphW, WidgetList nodes, int n_nodes)
#else
  (graphW, nodes, n_nodes) Widget  graphW; WidgetList nodes; int n_nodes;
#endif
{
XmGraphWidget  graph = (XmGraphWidget) graphW;
    NodePtr next;
    int     i;

    for (i = 0; i < n_nodes; i++) {
	next = NODEPTR (nodes [i]);
	_XmUnhighlightBorder(nodes[i]);
	NodeListRemove (&(graph->graph.selected_nodes), next);
    }
}

/********
 *
 * UnSelectNodes: Removes given nodes from the selected_nodes list
 *
 ********/
void XmGraphUnselectNode 
#ifndef _NO_PROTO
  (Widget graphW, Widget node)
#else
  (graphW, node) Widget  graphW; Widget    node;
#endif
{
XmGraphWidget  graph = (XmGraphWidget) graphW;
    NodePtr next;


    _XmUnhighlightBorder(node); 
    next = NODEPTR (node);
    NodeListRemove (&(graph->graph.selected_nodes), next);
}

/*********
 *
 * NumGraphNodes: returns the total number of node widgets in graph.
 *
 *********/
int XmGraphNumNodes 
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget  graphW;
#endif
{ XmGraphWidget graph = (XmGraphWidget) graphW;
    /* minus 1 because we have "dummy" */
  return (graph->composite.num_children - 1);
}

/*********
 *
 * XmGraphGetNodes:
 *
 *********/
WidgetList XmGraphGetNodes 
#ifndef _NO_PROTO
  (Widget graphW, int *n_nodes)
#else
  (graphW, n_nodes) Widget  graphW; int           *n_nodes;
#endif
{ XmGraphWidget  graph=( XmGraphWidget ) graphW;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
    {
	XtWarning("XmGraphGetNodes requires an XmGraph widget");
	return NULL;
    }

    if( graph->composite.num_children <= 1)
    {
	*n_nodes = 0;
	return NULL;
    }

    *n_nodes =  graph->composite.num_children - 1;

    return &graph->composite.children[1];

}



static void DestroySWCallback 
#ifndef _NO_PROTO
  ( Widget w, XtPointer client_data, XtPointer call_data)
#else
  (w, client_data, call_data) Widget w; XtPointer client_data,call_data;
#endif
{
    Widget clientWidget = (Widget) client_data;
    Widget clipWidget;
    Arg wargs[2];

    XtSetArg(wargs[0], XmNclipWindow, &clipWidget);
    XtGetValues(clientWidget, wargs, 1);

    if(clipWidget)
	XtDestroyWidget (clipWidget);

    XtDestroyWidget (clientWidget);
}

/*************************************<->*************************************
 *
 *   
 *   
 *
 *************************************<->***********************************/

Widget XmCreateScrolledGraph
#ifndef _NO_PROTO
  (Widget parent, String name, 
			     ArgList arglist, Cardinal argcount)
#else
  (parent, name, arglist, argcount)
	Widget    parent; String    name; ArgList   arglist; Cardinal  argcount;
#endif
{
    Widget swindow;
    Widget sgraph;
    Arg    args[10]; 
    int    n;
    char   *s;

    s = XtMalloc(strlen(name) + 3);  /* Name + NULL + "SW" */
    strcpy(s, name);
    strcat(s, "SW");

    /* 
     *     Create Scrolled Window.
     */

    n = 0;
    XtSetArg (args[n],XmNscrollingPolicy, XmAUTOMATIC); n++;
    XtSetArg(args[n], XmNvisualPolicy, (XtArgVal) XmCONSTANT); n++;

    swindow = XtCreateManagedWidget(s, xmScrolledWindowWidgetClass,
				    parent, args, n);

    /*
     *   Create Graph.
     */

    sgraph = XtCreateWidget(name, xmGraphWidgetClass, swindow, arglist, argcount);

    /*
     * Add callback to destroy ScrolledWindow parent.
     */
    XtAddCallback (sgraph, XmNdestroyCallback, 
	       DestroySWCallback, swindow);

    /*
     * Return Graph.
     */

    XtFree((char *) s);

    return (sgraph);
}


/*******************
 *
 * returns the arc under the point (x,y).
 * Logic: 1. check only those arcs that could be 
 *           visible (in the visible area of the window)
 *        2. Check only those arcs whose extents encompass the point
 *        3. After weeding out above, make a region for the arc only 
 *           if it doesn't already have one and check the point against the region.
 *
 **************/

Widget XmGraphInputOverArc
#ifndef _NO_PROTO
  (Widget graphW,int x,int y) 
#else
  (graphW,x,y) Widget  graphW; int x,y;
#endif
{ XmGraphWidget  graph=( XmGraphWidget ) graphW;
  XmArcWidget child;
    int i;

    if (!XtIsSubclass(graphW,xmGraphWidgetClass)) {
	XtWarning("XmGraphInputOverArc requires an XmGraph widget");
	return (Widget) NULL;
    }

    for (i = 0 ; i < graph->graph.n_arcs ; i++) 
    { 
	child =  graph->graph.arcs[i];

	if(child->core.visible) 
	{
	    XRectangle rect;

	    rect.x     = MIN(child->arc.from_x,  child->arc.to_x)   - child->arc.delta;
	    rect.width = ABS(child->arc.to_x   - child->arc.from_x) + 2 * child->arc.delta;
	    rect.y     = MIN(child->arc.from_y,  child->arc.to_y)   - child->arc.delta;
	    rect.height= ABS(child->arc.to_y   - child->arc.from_y) + 2 * child->arc.delta; 

	    if((child->arc.label && child->arc.map_name) || 
	       PointInRect(x, y, &rect) || 
	       child->arc.to == child->arc.from || 
	       !ZERO_RANKING(child))
	    {
		if(!child->arc.region) 
		    ComputeRegionsForArc(child);

		if(XPointInRegion(child->arc.region, x, y)) 
		    return (Widget) child;
	    }
	}
    }
    return (Widget) NULL;
}


Boolean  XmGraphIsPointInArc
#ifndef _NO_PROTO
  (Widget arcW,int x,int y) 
#else
  (arcW,x,y) Widget  arcW;
int x,y;
#endif
{ XmArcWidget  arc = (XmArcWidget)  arcW;
  XmGraphWidget graph = (XmGraphWidget) XtParent(arc);
  XRectangle rect;
   
    rect.x     = MIN(arc->arc.from_x,  arc->arc.to_x)   - arc->arc.delta; 
    rect.width = ABS(arc->arc.to_x   - arc->arc.from_x) + 2 * arc->arc.delta;
    rect.y     = MIN(arc->arc.from_y,  arc->arc.to_y)   - arc->arc.delta; 
    rect.height= ABS(arc->arc.to_y   - arc->arc.from_y) + 2 * arc->arc.delta; 

    if(PointInRect(x, y, &rect) || arc->arc.from != arc->arc.to ||
       !ZERO_RANKING(arc) ||
       (arc->arc.label && arc->arc.map_name))
    {

	if(!arc->arc.region) 
	    ComputeRegionsForArc((XmArcWidget)arc);

	return XPointInRegion(arc->arc.region, x, y);
    }
    return FALSE;
}


/**********
 *
 * CreateArcWidget: creates and returns a new arc widget.
 *             the Motif Widget functional interface way
 *
 **********/
Widget  XmCreateArc 
#ifndef _NO_PROTO
  (Widget graph, String name, ArgList args, Cardinal n_args)
#else
  (graph, name,  args, n_args) Widget  graph; String   name;
	ArgList args; Cardinal     n_args;
#endif
{ 
    Widget w =  XtCreateWidget (name, xmArcWidgetClass, graph, args, n_args);
    return w;
}



void XmGraphCenterAroundWidget
#ifndef _NO_PROTO
  (Widget graphW, Widget w)
#else
  (graphW, w) Widget graphW; Widget w;
#endif
{ XmGraphWidget graph = (XmGraphWidget) graphW;
    if(graph->graph.is_scrolled)
    {
	static XmScrollBarCallbackStruct cdata =  { XmCR_DRAG, NULL, 0, NULL};

	Arg       wargs[10];
	Widget    hscroll, vscroll;
	int       value;
	int       slider_size;
	int       increment;
	int       page_increment, max, min;
	Position  x, y;
	Dimension parent_width, parent_height, widget_width, widget_height;

	/* 
	 * Get the scrollbars from the graph.
	 */
	Widget swindow = XtParent(XtParent(graph));

	XtSetArg(wargs[0], XmNverticalScrollBar, &vscroll);
	XtSetArg(wargs[1], XmNhorizontalScrollBar, &hscroll);
	XtGetValues(swindow, wargs, 2);

	/* 
	 * Now find out where the widget is located.
	 */
#ifdef _R4_
	x = RX(w);
        y = RY(w);
        widget_width = RWidth(w);
        widget_height = RHeight(w);
#else
	XtSetArg(wargs[0], XmNx, &x);
	XtSetArg(wargs[1], XmNy, &y);
	XtSetArg(wargs[2], XmNwidth, &widget_width);
	XtSetArg(wargs[3], XmNheight, &widget_height);
	XtGetValues(w, wargs, 4);
#endif

	/*
	 * Compute the upper left corner that will place
	 * the widget in the middle of the screen.
	 * xloc = xwidget + width_widget / 2 -  width_of_scrolled_window /2
	 */

	x = x + widget_width / 2;
	y = y + widget_height / 2;

#ifdef _R4_
	parent_width = RWidth(XtParent(graph));
        parent_height = RHeight(XtParent(graph));
#else
	XtSetArg(wargs[0], XmNwidth, &parent_width);
	XtSetArg(wargs[1], XmNheight, &parent_height);
	XtGetValues(XtParent(XtParent(graph)), wargs, 2);
#endif
	/*
	 * Get current scrollbar info
	 */
	if(vscroll)
	{
	    XmScrollBarGetValues (vscroll, &value, 
				  &slider_size, &increment, &page_increment);
	    XtSetArg(wargs[0], XmNmaximum, &max);
	    XtSetArg(wargs[1], XmNminimum, &min);
	    XtGetValues(vscroll, wargs, 2);

	    y = MAX(min, ((int) (y - parent_height / 2)));

	    /*
	     * Adjust request to be less than scrollbar max if necessary.
	     */
	    y = MIN(y, max - slider_size);

#if SESD
	    XmScrollBarSetValues (vscroll, y, slider_size, SCROLLBAR_INCREMENT, page_increment, TRUE);
#else
	    XmScrollBarSetValues (vscroll, y, slider_size, increment, page_increment, TRUE);
#endif
	    /*
	     * Hack due to Motif deficiency.
	     */
	    cdata.value = y;
	    XtCallCallbacks(vscroll, XmNdragCallback, &cdata);
	}
	/*
	 * Do all the same stuff for x direction
	 */
	if(hscroll)
	{
	    XmScrollBarGetValues (hscroll, &value, 
				  &slider_size, &increment, &page_increment);
	    XtSetArg(wargs[0], XmNmaximum, &max);
	    XtSetArg(wargs[1], XmNminimum, &min);
	    XtGetValues(hscroll, wargs, 2);

	    x = MAX(min, ((int) (x - parent_width / 2)));
	    x = MIN(x, max - slider_size);

#if SESD
	    XmScrollBarSetValues (hscroll, x, slider_size, SCROLLBAR_INCREMENT, page_increment, TRUE);
#else
	    XmScrollBarSetValues (hscroll, x, slider_size, increment, page_increment, TRUE);
#endif

	    cdata.value = x;
	    XtCallCallbacks(hscroll, XmNdragCallback, &cdata);
	}
    }
}


#ifdef notdef

Widget XmGraphDummyRoot
#ifndef _NO_PROTO
  (XmGraphWidget w)
#else
  (w) XmGraphWidget w;
#endif
{
    return w->graph.root->widget;
}


int N_SUB_NODES
#ifndef _NO_PROTO
  (Widget w)
#else
  (w) Widget w;
#endif
{
    NodePtr node = NODEPTR(w);

    if(!node) 
	return 0;

    return node->from_arcs.n_arcs;
}

int N_SUPER_NODES
#ifndef _NO_PROTO
  (Widget w)
#else
  (w) Widget w;
#endif
{
    NodePtr node = NODEPTR(w);

    if(!node) 
	return 0;

    return node->to_arcs.n_arcs;
}


Widget NTH_CHILD
#ifndef _NO_PROTO
  (Widget w, int n)
#else
  (w, n) Widget w; int n;
#endif
{
    NodePtr node = NODEPTR(w);
    XmArcWidget arc;

    if(!node) 
	return NULL;

    arc = (XmArcWidget) node->to_arcs.arcs[n];

    return arc->arc.to;
}
#endif /* notdef */

/*
 * Adds a new node positioned in the center of the graph
 */

void XmGraphAddNode
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget graphW;
#endif
{ XmGraphWidget graph=(XmGraphWidget) graphW;
    int x, y;
    Widget w;
    XmGraphCallbackStruct    cb;

    autoLayoutType old_layout_type = graph->graph.auto_layout_mode;

    if(graph->graph.is_scrolled) 
    {
	x = -graph->core.x + graph->graph.clip_width / 2 - nn_width / 2;
	y = -graph->core.y + graph->graph.clip_height /2 - nn_height / 2 ;
    }
    else
    {
	x = XtWidth(graph) / 2 - nn_width / 2;
	y = XtHeight(graph) / 2 - nn_height / 2;
    }

    switch(graph->graph.auto_layout_mode)
    {
    case XmNODES_ONLY:
	graph->graph.auto_layout_mode = XmALWAYS;
	break;
    case XmARCS_ONLY:
    case XmPARTIAL:
	graph->graph.auto_layout_mode = XmNEVER;
	break;
    case XmALWAYS:
    case XmNEVER:
    default:
	break;
    }

    w = DefaultNodeWidget (graphW, x, y);

    if(XtCallbackHasSome == XtHasCallbacks(graphW, XmNnewNodeCallback))
    {
	cb.event  = NULL;
	cb.reason = XmCR_NEW_NODE;
	cb.widget = w;
	cb.selected_widgets = NULL;
	cb.selected_widgets = XmGraphGetSelectedNodes (graphW, &cb.num_selected_widgets);

	cb.selected_arcs = XmGraphGetSelectedArcs (graphW, &cb.num_selected_arcs);
	cb.interactive = TRUE;
	cb.doit = TRUE;
	XtCallCallbacks(graphW, XmNnewNodeCallback, &cb);

	if (!cb.doit) 
	{
	    XtDestroyWidget((Widget) w);

	    graph->graph.auto_layout_mode = old_layout_type;

	    return;
	}
    }

    XtManageChild (w);

    graph->graph.auto_layout_mode = old_layout_type;

}


void XmGraphCenterAtPos
#ifndef _NO_PROTO
  (Widget widget, int x, int y)
#else
  (widget, x, y) Widget widget; int x, y;
#endif
{ XmGraphWidget graph = (XmGraphWidget)widget;
    if(graph->graph.is_scrolled)
    {
	static XmScrollBarCallbackStruct cdata =  { XmCR_DRAG, NULL, 0, NULL};

	Arg       wargs[10];
	Widget    hscroll, vscroll;
	int       value;
	int       slider_size;
	int       increment;
	int       page_increment, max, min;

	/* 
	 * Get the scrollbars from the graph.
	 */
	Widget swindow = XtParent(XtParent(graph));

	XtSetArg(wargs[0], XmNverticalScrollBar, &vscroll);
	XtSetArg(wargs[1], XmNhorizontalScrollBar, &hscroll);
	XtGetValues(swindow, wargs, 2);

	/*
	 * Get current scrollbar info
	 */
	if(vscroll)
	{
	    XmScrollBarGetValues (vscroll, &value, 
				  &slider_size, &increment, &page_increment);
	    XtSetArg(wargs[0], XmNmaximum, &max);
	    XtSetArg(wargs[1], XmNminimum, &min);
	    XtGetValues(vscroll, wargs, 2);

	    y =  ((max - min) - slider_size) * y / (float) XtHeight(graph);

	    y = MAX(y, min);
	    y = MIN(y, max - slider_size);


	    XmScrollBarSetValues (vscroll, y, slider_size, increment, page_increment, TRUE);
	    /*
	     * Hack due to Motif deficiency.
	     */
	    cdata.value = y;
	    XtCallCallbacks(vscroll, XmNdragCallback, &cdata);
	}
	/*
	 * Do all the same stuff for x direction
	 */
	if(hscroll)
	{
	    XmScrollBarGetValues (hscroll, &value, 
				  &slider_size, &increment, &page_increment);
	    XtSetArg(wargs[0], XmNmaximum, &max);
	    XtSetArg(wargs[1], XmNminimum, &min);
	    XtGetValues(hscroll, wargs, 2);

	    x =  ((max - min) - slider_size) * x / (float) XtWidth(graph);


	    x = MAX(x, min);
	    x = MIN(x, max - slider_size);

#if SESD
	    XmScrollBarSetValues (hscroll, x, slider_size, SCROLLBAR_INCREMENT, page_increment, TRUE);
#else
	    XmScrollBarSetValues (hscroll, x, slider_size, increment, page_increment, TRUE);
#endif

	    cdata.value = x;
	    XtCallCallbacks(hscroll, XmNdragCallback, &cdata);
	}
    }
}

/*
 * Crude approximation of a zoom function. Widgets don't scale, making this
 * so so at best. Attempt to keep the current center of the graph in place.
 * **** Not quite working yet ****
 */
void XmGraphZoom
#ifndef _NO_PROTO
  (Widget graphW, float factor)
#else
  (graphW, factor) Widget graphW; float factor;
#endif
{  XmGraphWidget graph = (XmGraphWidget) graphW;
    Arg wargs[2];
    int cs, ss;
    int middle_x, middle_y;
    float xper, yper;
    int limit = 4096; /* 910318 mohammad - changed this to a variable to 
                                           experiment with some other values.
					   2 ** 14 = 16384 
					   2 ** 10 = 4096 
                       */
    
#ifdef TRACING
    printf("ZOOM: FACTOR|%f -- CHILD SPACE|%d -- SIBLING SPACE|%d\n", 
	   factor, graph->graph.child_spacing, graph->graph.sibling_spacing);
#endif


    if(graph->graph.child_spacing > limit && factor > 0)  /* Crude check to prevent rediculous zooming */
	return;

    if(graph->graph.sibling_spacing > limit && factor > 0)
	return;

    if(graph->graph.is_scrolled)
    {
	middle_x = - graph->core.x + graph->graph.clip_width / 2;
	middle_y = - graph->core.y + graph->graph.clip_height / 2;
    }
    else
    {
	middle_x =  RWidth(graph) / 2;
	middle_y =  RHeight(graph) / 2;
    }

    xper = (float) middle_x / RWidth(graph);
    yper = (float) middle_y / RHeight(graph);

    if(factor < 0) 
    {
	cs  = graph->graph.child_spacing / ABS(factor);
	ss  = graph->graph.sibling_spacing / ABS(factor);
    }
    else
    {
	cs  = graph->graph.child_spacing * ABS(factor);
	ss  = graph->graph.sibling_spacing * ABS(factor);
    }


    if (cs == 0)
	cs = 1;

    if(ss == 0)
	ss = 1;

    XtSetArg (wargs[0], XmNchildSpacing,   cs);
    XtSetArg (wargs[1], XmNsiblingSpacing, ss);
    XtSetValues (graphW, wargs, 2);

    XmGraphCenterAtPos(graphW, (int) (xper * RWidth(graph)), (int) (yper * RHeight(graph)));

}

void XmGraphUnmap
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget graphW;
#endif
{   XmGraphWidget graph = (XmGraphWidget) graphW;
    XtUnmapWidget(graph);
    graph->core.visible = 0;
}

 
void XmGraphMap
#ifndef _NO_PROTO
  (Widget graphW)
#else
  (graphW) Widget graphW;
#endif
{   XmGraphWidget graph = (XmGraphWidget) graphW;
    XtMapWidget(graph);
    graph->core.visible = 1;
    
    if (graph->graph.deferedChangeManaged)
	{ graph->graph.deferedChangeManaged = FALSE;
	  graph->graph.deferedAdjustSize = FALSE;
	  ChangeManaged(graph);
	}
    if (graph->graph.deferedAdjustSize)
	{ graph->graph.deferedAdjustSize = FALSE;
	  AdjustSize(graph);
	}
}



int XmGraphGetGridSize
#ifndef _NO_PROTO
  (Widget w)
#else
  (w)
  Widget w;
#endif
{ XmGraphWidget graphW = (XmGraphWidget) w;

  if (!XtIsSubclass(w,xmGraphWidgetClass)) 
  { XtWarning("XmGraphGetGridSize requires an XmGraph widget");
    return NULL;
  }

  return  graphW->graph.snap_grid_size;
}


void XmGraphSetGridSize
#ifndef _NO_PROTO
  (Widget w, Cardinal size)
#else
  (w,size) Widget w; Cardinal size;
#endif
{ XmGraphWidget graphW = (XmGraphWidget) w;

  if (!XtIsSubclass(w,xmGraphWidgetClass)) 
  { XtWarning("XmGraphSetGridSize requires an XmGraph widget");
    return ;
  }

  if (size)
    graphW->graph.snap_grid_size = size;
}


static void updateNodeStruct
#ifndef _NO_PROTO
  (Widget w)
#else
  (w) Widget w;
#endif
{ XmGraphConstraintsRec * node1;

  node1 = CONSTRAINTREC(w);
  node1->pos_x = RX(w);
  node1->pos_y = RY(w);
}


static Boolean nodesConnected
#ifndef _NO_PROTO
  (NodePtr n1 ,NodePtr n2)
#else
  (n1,n2)NodePtr n1 ;NodePtr n2;
#endif
{ Widget w2,w1;
  XmArcWidgetList  arcs;
  XmArcWidget arc;
  int n_arcs,i;
  NodePtr  connected_node; 

  w1 = n1->widget;
  w2 = n2->widget;

  arcs = n1->from_arcs.arcs;
  n_arcs = n1->from_arcs.n_arcs;
  for (i = 0; i < n_arcs; i++)
  { arc =  arcs[i];
    connected_node   = NODEPTR (arc->arc.to);
    if (connected_node->widget == w2) 
    { return TRUE;
    }
  }
  arcs = n1->to_arcs.arcs;
  n_arcs = n1->to_arcs.n_arcs;
  for (i = 0; i < n_arcs; i++)
  { arc =  arcs[i];
    connected_node = NODEPTR (arc->arc.from);
    if (connected_node->widget == w2) 
    { return TRUE;
    }
  }
  return FALSE;
}



void XmGraphFixNodePos
#ifndef _NO_PROTO
(Widget w, Widget ww, Boolean fixed)
#else
(w,ww,fixed) Widget w,ww; Boolean fixed;
#endif
{ XmGraphConstraintsRec * node1;

  node1 = CONSTRAINTREC(ww);
  node1->pos_fixed = fixed;
}

void XmGraphFixAllNodePos
#ifndef _NO_PROTO
(Widget w, Boolean fixed)
#else
(w,fixed) Widget w; Boolean fixed;
#endif
{ Widget *wlist;
  int i,no;
  XmGraphWidget graphW = (XmGraphWidget)w;

  if (!XtIsSubclass(w,xmGraphWidgetClass)) 
  { XtWarning("XmFixAllNodePos requires an XmGraph widget");
    return ;
  }


  no = graphW->composite.num_children - 1;
  wlist = &(graphW->composite.children[1]);
  for(i=0;i<no;i++)
  { XmGraphFixNodePos(w, *wlist++, fixed);
  }
}

void XmGraphFixSelectedNodePos
#ifndef _NO_PROTO
(Widget w, Boolean fixed)
#else
(w,fixed) Widget w; Boolean fixed;
#endif
{ Widget *wlist;
  int i,no;

  wlist = XmGraphGetSelectedNodes (w, &no);
  for(i=0;i<no;i++)
  { XmGraphFixNodePos(w, *wlist++, fixed);
  }
}

void XmGraphSetSnapGrid
#ifndef _NO_PROTO
(Widget w, Boolean onOff)
#else
(w,onOff) Widget w; Boolean onOff;
#endif
{ XmGraphWidget graphW = (XmGraphWidget) w;

  if (!XtIsSubclass(w,xmGraphWidgetClass)) 
  { XtWarning("XmGraphSetSnapGrid requires an XmGraph widget");
    return ;
  }

  graphW->graph.snap_on = onOff;
#ifdef TRY_SPRING
  if (onOff) XmGraphUpdateSpringLayout(w);
#endif
}


static void  getConnectedNodesInArcList
#ifndef _NO_PROTO
( Widget  w)
#else
(w) Widget w;
#endif
{ XmArcWidgetList  arcs;
  XmArcWidget arc;
  NodePtr connected_node;
  XmGraphConstraintsRec *node1 = CONSTRAINTREC(w);
  NodePtr n1 = node1->node;
  int i,n_arcs;

  arcs = n1->from_arcs.arcs;
  n_arcs = n1->from_arcs.n_arcs;
  for (i = 0; i < n_arcs; i++)
  { arc =  arcs[i];
    connected_node   = NODEPTR (arc->arc.to);
    if (ArcListInsertNoDuplicates(&return_widget_list, 
	     (XmArcWidget)  connected_node->widget))
    { getConnectedNodesInArcList(connected_node->widget);
    }

  }

  arcs = n1->to_arcs.arcs;
  n_arcs = n1->to_arcs.n_arcs;
  for (i = 0; i < n_arcs; i++)
  { arc =  arcs[i];
    connected_node = NODEPTR (arc->arc.from);
    if (ArcListInsertNoDuplicates(&return_widget_list, 
	     (XmArcWidget)  connected_node->widget))
    { getConnectedNodesInArcList(connected_node->widget);
    }
  }
}

/*********
 *
 * XmGraphGetConnectedNodes: 
		Returns all nodes connected to a specific node.
 *
 *********/

WidgetList XmGraphGetConnectedNodes
#ifndef _NO_PROTO
  (Widget graphW, Widget w, int *num)
#else
  (graphW, w, num) Widget graphW; int           *num;
#endif
{ XmGraphWidget  graph= (XmGraphWidget) graphW;
  
  if (!XtIsSubclass(graphW,xmGraphWidgetClass)) 
  { XtWarning("XmGraphGetConnectedNodes requires an XmGraph widget");
    return NULL;
  }

  return_widget_list.n_arcs = 0;

  ArcListInsertNoDuplicates(&return_widget_list, (XmArcWidget)  w);

  getConnectedNodesInArcList(w);

  *num = return_widget_list.n_arcs ;
  /*
  { int i,size;
    Widget *ww;
    size = *num;
    ww = (WidgetList)return_widget_list.arcs;
    for (i=0;i< size;i++)
    { fprintf(stderr,"%d: %s \n", i, XtName(*ww++));
    }
  }
  */
  return (WidgetList)return_widget_list.arcs;
}

static NodeList  cluster_nodes;
static Boolean sameCluster 
#ifndef _NO_PROTO
  (NodePtr node1, Widget w1)
#else
  (node1,w1) NodePtr      node1; Widget w1;
#endif
{ NList      nodes = (node1 ? node1->kids.nodes : NULL);
  int        i, n_nodes = (node1 ? node1->kids.n_nodes : 0);
  Widget     w = (node1 ? node1->widget : NULL);

    /*
     * Insert those not yet in subgraph, mapped, and managed 
     */
   if (w && XtIsManaged(w) && 
	(XmIsGadget(w) || w->core.mapped_when_managed) &&
	 !NodeListMember(&cluster_nodes, node1))
  { /* node->visited = True; */
    NodeListInsert (&(cluster_nodes), node1);
    for (i = 0; i < n_nodes; i++) 
    { if ((nodes[i])-> widget != w1) 
      { if (sameCluster (nodes[i],w1)) return TRUE;
      } else 
      { return TRUE;
      }
    }
    nodes = (node1 ? node1->parents.nodes : NULL);
    n_nodes = (node1 ? node1->parents.n_nodes : 0);
    for (i = 0; i < n_nodes; i++) 
    { if ((nodes[i])-> widget != w1) 
      { if (sameCluster (nodes[i],w1)) return TRUE;
      } else 
      { return TRUE;
      }
    }
  }
  return FALSE;
}




void XmGraphCenterAll
#ifndef _NO_PROTO
  (Widget w)
#else
  (w)
Widget w;
#endif
{ /* calculate the Center of Gravity of the nodes and then move the graph 
     there */
  Widget *wlist;
  Widget wi,arcW;
  XmGraphWidget graphW = (XmGraphWidget) w;
  int i,size,n_arcs;
  int totalX=0, totalY=0;
  int deltaX=0, deltaY=0;
  XmArcWidgetList  arcs;
  XmArcWidget   arc;

  if (!XtIsSubclass(w,xmGraphWidgetClass)) 
  { XtWarning("XmGraphCenterAll requires an XmGraph widget");
    return ;
  }
  size = graphW->composite.num_children - 1;
  wlist = &(graphW->composite.children[1]);
  for (i=0;i<size;i++)
  { wi = *wlist++;
    totalX += RX(wi);
    totalY += RY(wi);
  }
  totalX /= size;
  totalY /= size;
  if(graphW->graph.is_scrolled) 
  { deltaX = XtWidth(XtParent(XtParent(wi)))/2 - totalX;
    deltaY = XtWidth(XtParent(XtParent(wi)))/2 - totalY;
  } else
  { deltaX = XtWidth(wi)/2 - totalX;
    deltaY = XtWidth(wi)/2 - totalY;
  }
  /*
  fprintf(stderr,"move all %d %d \n", deltaX, deltaY);
  */
  if ((deltaX == 0) && (deltaY == 0)) return;

  wlist = &(graphW->composite.children[1]);

  _InitArcList(graphW);  /* clear line list */
  graphW->graph.batch_drawing_mode = TRUE;

  for (i=0;i<size;i++)
  { wi = *wlist++;

    XmGraphMoveNode(w,wi, RX(wi) + deltaX, RY(wi) + deltaY); 
    updateNodeStruct(wi);
  }

  _InitArcList(graphW);  /* clear line list */
  graphW->graph.batch_drawing_mode = FALSE;
}

#ifdef TRY_SPRING

#define MULTIPLIER 4
static double SPRING_LENGTH= 100.0;
static double CHARGE_EFFECTIVE_LENGTH= 0.0;
static double SPRING_CONST= .1;
static double CHARGE_CONST= -10000.0;
static double LOSS_CONSTANT= 0.2;

void XmGraphUpdateSpringLayout
#ifndef _NO_PROTO
  (Widget w)
#else
  (w) Widget w;
#endif
{ int size,i,j;
  int grid_size, half_grid_size;
  Widget *wlist;
  Widget wi;
  XmGraphConstraintsRec * node1;
  XmGraphWidget graphW = (XmGraphWidget) w;
  int x,y;

  if (!XtIsSubclass(w,xmGraphWidgetClass)) 
  { XtWarning("XmGraphUpdateSpringLayout requires an XmGraph widget");
    return ;
  }


  grid_size = graphW->graph.snap_grid_size ;
  half_grid_size = grid_size/2;

  graphW->graph.batch_drawing_mode = TRUE;
  size = graphW->composite.num_children - 1;
  wlist = &(graphW->composite.children[1]);
  for (i=0;i<size;i++)
  { wi = *(wlist+i);
    if (XmIsGadget(wi) || XtIsWidget(wi))
    { node1 = CONSTRAINTREC(wi);
      if ((node1 != NULL) &&  !(node1->pos_fixed))
      { if (graphW->graph.snap_on)
	{ x = (((int)node1->pos_x + half_grid_size)/ grid_size )* grid_size;
          y = (((int)node1->pos_y + half_grid_size)/ grid_size )* grid_size;
          node1->pos_y = y;
          node1->pos_x = x;
	} else
	{ x = (int)node1->pos_x ;
          y = (int)node1->pos_y ;
	}
        XmGraphMoveNode (w,  wi, x,y);
      }
    }
  }
  graphW->graph.batch_drawing_mode = FALSE;
}

void XmGraphSpringLayout
#ifndef _NO_PROTO
  (Widget w,Widget ww)
#else
  (w, ww) Widget w, ww;
#endif
{ XmGraphWidget graphW = (XmGraphWidget) w;
  int size;
  Widget *wlist;

  if (!XtIsSubclass(w,xmGraphWidgetClass)) 
  { XtWarning("XmGraphSpringLayout requires an XmGraph widget");
    return ;
  }

  if (ww == NULL)
  { size = graphW->composite.num_children - 1;
    wlist = &(graphW->composite.children[1]);
  } else
  { wlist =  XmGraphGetConnectedNodes(w, ww, &size);
  }
  XmGraphSpringLayoutList(w,wlist,size);
}

void XmGraphSpringLayoutList
#ifndef _NO_PROTO
  (Widget w,Widget *wlist,int size)
#else
  (w,wlist,size) Widget w;Widget *wlist;int size;
#endif
{ int i,j;
  Widget wi,wj;
  XmGraphConstraintsRec * node1, *node2;
  double deltaX,deltaY, d_sq, d_cube, dist;
  double ffi,ffx,ffy;
  static Boolean beenHere  = FALSE;

  if (!beenHere)
  { CHARGE_EFFECTIVE_LENGTH = MULTIPLIER * SPRING_LENGTH;
    beenHere  = TRUE;
  }
  for (i=0;i<size;i++)
  { wi = *(wlist+i);
    if (XmIsGadget(wi) || XtIsWidget(wi))
    { node1 = CONSTRAINTREC(wi);
      if ((node1 != NULL) )
      { for (j=i+1; j<size;j++)
        { wj = *(wlist+j);
          node2 = CONSTRAINTREC(wj);
	  deltaX = node2->pos_x - node1->pos_x;
	  d_sq = deltaX * deltaX;
          deltaY = node2->pos_y - node1->pos_y;
	  d_sq += deltaY * deltaY;
	  if (d_sq < 2.0)
	  { d_sq = 2.0;
	    deltaX = 1.0;
	    deltaY = 1.0;
	  }
	  dist = sqrt(d_sq);
	  d_cube = d_sq * dist;

/* spring interaction */
/* this should only be bewtween connected nodes */
	  if (nodesConnected(node1->node, node2->node))
	  { ffi = (dist - SPRING_LENGTH) * SPRING_CONST / dist;
	    ffx = ffi * (deltaX );
	    node1->vel_x += ffx;
	    node2->vel_x -= ffx;

	    ffy = ffi * deltaY;
	    node1->vel_y += ffy;
	    node2->vel_y -= ffy;

          }
          cluster_nodes.n_nodes = 0;
	  /* this is VERY inefficient 
	  if (sameCluster(node1->node, node2->node->widget))
	  */
/* charge interaction */
          if (dist < CHARGE_EFFECTIVE_LENGTH) 
	  { ffi = CHARGE_CONST /d_cube;

	    ffx = ffi * deltaX;
	    node1->vel_x += ffx;
	    node2->vel_x -= ffx;

	    ffy = ffi * deltaY;
	    node1->vel_y += ffy;
	    node2->vel_y -= ffy;
	  }
	  /*
	  else
	  { fprintf(stderr,"not same cluster %s %s\n", XtName(node1->node->widget), XtName(node2->node->widget));
	  }
	  */
        }

	/*
        if (!(node1->pos_fixed) && (connectedToOther(node1->node)))
	*/
/*
        if (!(node1->pos_fixed) && 
		!((node1->node->from_arcs.n_arcs !=0)|| 
		  (node1->node->to_arcs.n_arcs !=0)))
*/
        if (!(node1->pos_fixed)
   && /* there is an arc connected to this node */
		((node1->node->from_arcs.n_arcs !=0)|| 
		  (node1->node->to_arcs.n_arcs !=0)))
	{ 
#define MAX_VEL 100.0
	  if (node1->vel_y > MAX_VEL) node1->vel_y = MAX_VEL;
	  else
	  if (node1->vel_y < -MAX_VEL) node1->vel_y = -MAX_VEL;

	  if (node1->vel_x > MAX_VEL) node1->vel_x = MAX_VEL;
	  else
	  if (node1->vel_x < -MAX_VEL) node1->vel_x = -MAX_VEL;

	  node1->pos_x  = node1->pos_x + node1->vel_x + ((int)(node1->pos_x))%2;
          node1->pos_y  = node1->pos_y + node1->vel_y;


          node1->vel_x *= LOSS_CONSTANT;
          node1->vel_y *= LOSS_CONSTANT;
	}
      }
    }
  }
}

void XmGraphGetConsts
#ifndef _NO_PROTO
( Widget w,double *len, double *springK, double *chargeK, double *loss)
#else
(w,len,springK,chargeK,loss)
   Widget w;double *len, *springK, *chargeK, *loss;
#endif
{ 
  
  *len = SPRING_LENGTH;
  *springK = SPRING_CONST;
  *chargeK = CHARGE_CONST;
  *loss = LOSS_CONSTANT;
  /* a factor between 0.01 and 0.999) */
}


void XmGraphSetConsts
#ifndef _NO_PROTO
(Widget w,double len, double springK, double chargeK, double loss)
#else
(w,len,springK,chargeK,loss)
   Widget w;double len, springK, chargeK, loss;
#endif
{ 
  
  SPRING_LENGTH= (len < 0)? 100: len;
  CHARGE_EFFECTIVE_LENGTH= MULTIPLIER * len;
  SPRING_CONST= (springK < 0)? .1 : springK;
  CHARGE_CONST= (chargeK > 0)? -100.0 : chargeK;
  LOSS_CONSTANT= (loss < 0.01)? 0.01 : (loss > 0.999) ? 0.999: loss;
  /* a factor between 0.01 and 0.999) */
}
#endif /* TRY_SPRING */


/* returns the current one to you */
Cursor  XmGraphSetCursor
#ifndef _NO_PROTO
  (Widget widget, Cursor cursor)
#else
  (widget, cursor) Widget widget; Cursor cursor;
#endif
{ XmGraphWidget graph = (XmGraphWidget) widget;
  Cursor old;

  if (!XtIsSubclass(widget,xmGraphWidgetClass))  return NULL;


  old = PtrCursor (graph);
  PtrCursor (graph) = cursor;
  XDefineCursor (XtDisplay (widget), XtWindow (widget),  cursor);
  return old;
}
