/* -*-C-*-
********************************************************************************
*
* File:         Graph.h
* RCS:          $Header: /users/npm/src/winterp/src-server/widgets/RCS/Graph.h,v 2.1 1994/06/06 15:48:05 npm Exp $
* Description:  Public Header file for a Motif Graph Widget
* Author:       Doug Young dyoung@wpd.sgi.com, Luis Miguel, (luis@postgres.berkeley.edu)
* Created:      
* Modified:     Sun May 29 22:57:06 1994 (Niels Mayer) npm@indeed
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

#ifndef _XmGraph_h
#define _XmGraph_h

/* Class record constant */

externalref WidgetClass xmGraphWidgetClass;
typedef struct _XmGraphClassRec		*XmGraphWidgetClass;
typedef struct _XmGraphRec		*XmGraphWidget;


#include <Xm/Xm.h>

#ifndef XmIsGraph
#define XmIsGraph (w)	XtIsSubclass(w, xmGraphWidgetClass)
#endif


/******************* CONVENIENCE FUNCTIONS **********************/

/* creates and returns an instance of the graph widget */

#ifdef __cplusplus                    /* do not leave open across includes */
extern "C" {                                  /* for C++ V2.0 */
#endif

#ifndef _NO_PROTO

#ifdef TRY_SPRING
extern void	XmGraphUpdateSpringLayout (Widget w);
extern void	XmGraphSpringLayout (Widget w,Widget ww);
extern void	XmGraphSpringLayoutList (Widget w,Widget *wlist,int size);
#endif /* TRY_SPRING */

extern void	XmGraphFixAllNodePos (Widget , Boolean );

extern void	XmGraphCenterAll(Widget);
extern void	XmGraphCenterAtPos(Widget,int,int);
extern void	XmGraphFixNodePos (Widget , Widget , Boolean );
extern void	XmGraphFixSelectedNodePos (Widget , Boolean );
extern WidgetList XmGraphGetConnectedNodes(Widget , Widget , int *);
extern void	XmGraphGetConsts (Widget ,double *len, double *springK,
				double *chargeK, double *loss);
extern int	XmGraphGetGridSize(Widget);
extern void	XmGraphSetSnapGrid (Widget , Boolean );
extern void	XmGraphSetGridSize(Widget,Cardinal);
extern void	XmGraphSetConsts (Widget,double len, double springK,
				double chargeK, double loss);

#else

#ifdef TRY_SPRING
extern void	XmGraphUpdateSpringLayout ();
extern void	XmGraphSpringLayout ();
extern void	XmGraphSpringLayoutList ();
#endif /* TRY_SPRING */

extern void	XmGraphFixAllNodePos ();
extern void	XmGraphCenterAll();
extern void	XmGraphCenterAtPos();
extern void	XmGraphFixNodePos ();
extern void	XmGraphFixSelectedNodePos ();
extern WidgetList XmGraphGetConnectedNodes();
extern void	XmGraphGetConsts ();
extern int	XmGraphGetGridSize();
extern void	XmGraphSetSnapGrid ();
extern void	XmGraphSetGridSize();
extern void	XmGraphSetConsts ();
#endif

#ifndef _NO_PROTO

extern Widget	XmCreateGraph(Widget,String,ArgList,Cardinal);
extern Widget	XmCreateManagedGraph (Widget,String,ArgList,Cardinal);
extern Widget	XmCreateScrolledGraph(Widget,String,ArgList,Cardinal);

extern  void       XmGraphAddNode(Widget);
extern  void       XmGraphCenterAroundWidget(Widget, Widget);
extern  void       XmGraphDestroyAllArcs(Widget);
extern  void       XmGraphDestroyAllNodes(Widget);
extern  void       XmGraphDestroySelectedArcsOrNodes(Widget);
extern  void       XmGraphGetArcNodes(Widget, Widget, Widget *, Widget *);
extern  WidgetList XmGraphGetArcsBetweenNodes(Widget, Widget, Widget, int *);
extern  WidgetList XmGraphGetArcs(Widget, int *);
extern  void	   XmGraphGetNodeArcs(Widget, Widget,
				   WidgetList *, WidgetList *, int *, int *);
extern  WidgetList XmGraphGetNodes(Widget, int *);
extern  WidgetList XmGraphGetRoots(Widget, int *);
extern  WidgetList XmGraphGetSelectedArcs(Widget,int *);
extern  WidgetList XmGraphGetSelectedNodes(Widget,int *);
extern  Widget     XmGraphInputOverArc(Widget, int, int);
extern  void       XmGraphInsertRoots(Widget, WidgetList, int);
extern  Boolean    XmGraphIsPointInArc(Widget, int, int);
extern  Boolean    XmGraphIsSelectedArc(Widget, Widget);
extern  Boolean    XmGraphIsSelectedNode(Widget, Widget);
extern  void       XmGraphLayout(Widget);
extern  void	   XmGraphMoveAll(Widget, int, int);
extern  Boolean    XmGraphMoveArc(Widget, Widget, Widget, Widget);
extern  Boolean    XmGraphMoveNode(Widget,Widget, Position, Position);
extern  int        XmGraphNumArcs(Widget);
extern  int        XmGraphNumNodes(Widget);
extern  int        XmGraphNumNodeArcs(Widget, Widget, int * , int *);
extern  int        XmGraphNumRoots(Widget);
extern  int        XmGraphNumSelectedArcs(Widget);
extern  int        XmGraphNumSelectedNodes(Widget);
extern  void       XmGraphRelaySubgraph(Widget, Widget);
extern  void       XmGraphRemoveArcBetweenNodes(Widget, Widget, Widget);
extern  void       XmGraphRemoveRoots(Widget, WidgetList, int);
extern  void       XmGraphSelectNode(Widget,Widget);
extern  void       XmGraphSelectNodes(Widget, WidgetList, int);
extern  void       XmGraphSelectArc(Widget, Widget);
extern  void       XmGraphSelectArcs(Widget, WidgetList, int);
extern  void       XmGraphUnselectArc(Widget,Widget);
extern  void       XmGraphUnselectArcs(Widget, WidgetList, int);
extern  void       XmGraphUnselectNode(Widget,Widget);
extern  void       XmGraphUnselectNodes(Widget,WidgetList, int);
extern  void	   XmGraphUnmap(Widget);
extern  void	   XmGraphMap(Widget);

#else    /* _NO_PROTO */

extern Widget XmCreateGraph();
extern Widget XmCreateManagedGraph ();
extern Widget XmCreateScrolledGraph();
extern Widget XmCreateArc ();
extern Widget XmCreateAttachedArc ();

extern  void    XmGraphGetArcNodes();
extern  void    XmGraphGetNodeArcs();
extern  Widget  XmGraphInputOverArc();
extern  void    XmGraphInsertRoots();
extern  Boolean XmGraphIsSelectedArc();
extern  Boolean XmGraphMoveArc();
extern  Boolean XmGraphMoveNode();
extern  void    XmGraphSelectNode();
extern  void    XmGraphSelectNodes();
extern  void    XmGraphSelectArc();
extern  void    XmGraphSelectArcs();

extern  void	XmGraphCenterAroundWidget();
extern  void	XmGraphDestroyAllArcs();
extern  void	XmGraphDestroyAllNodes();
extern  void	XmGraphDestroySelectedArcsOrNodes();
extern  WidgetList	XmGraphGetArcsBetweenNodes();
extern  WidgetList	XmGraphGetArcs();
extern  WidgetList	XmGraphGetNodes();
extern  WidgetList	XmGraphGetRoots();
extern  WidgetList	XmGraphGetSelectedArcs();
extern  WidgetList	XmGraphGetSelectedNodes();
extern  Boolean XmGraphIsPointInArc();
extern  Boolean XmGraphIsSelectedNode();
extern  int	XmGraphNumArcs();
extern  int	XmGraphNumNodes();
extern  int	XmGraphNumNodeArcs();
extern  int	XmGraphNumRoots();
extern  int	XmGraphNumSelectedArcs();
extern  int	XmGraphNumSelectedNodes();
extern  void	XmGraphMoveAll();
extern  void	XmGraphLayout();
extern  void	XmGraphRelaySubgraph();
extern  void	XmGraphRemoveArcBetweenNodes();
extern  void	XmGraphRemoveRoots();
extern  void	XmGraphUnselectArc();
extern  void	XmGraphUnselectArcs();
extern  void	XmGraphUnselectNode();
extern  void	XmGraphUnselectNodes();
extern  void	XmGraphAddNode();
extern  void	XmGraphUnmap();
extern  void	XmGraphMap();
#endif /*_NO_PROTO */

#ifdef __cplusplus
}                                             /* for C++ V2.0 */
#endif

#define XmNsnapGridSize     "snapGridSize"
#define XmCSnapGridSize     "SnapGridSize"

#define XmNsnapGridOn     "snapGridOn"
#define XmCSnapGridOn     "SnapGridOn"

#define XmNtwinsVisible     "twinsVisible"
#define XmCTwinsVisible     "TwinsVisible"

#define XmNarcDrawMode      "arcDrawMode"
#define XmCArcDrawMode      "ArcDrawMode"


#define XmNautoLayoutMode   "autoLayoutMode"
#define XmCAutoLayoutMode   "AutoLayoutMode"

#define XmNreLayout	    "reLayout"
#define XmCReLayout	    "ReLayout"

#define XmNreorient         "reorient"
#define XmCReorient         "Reorient"

#define XmNchildSpacing     "childSpacing"
#define XmCChildSpacing     "ChildSpacing"

#define XmNsiblingSpacing   "siblingSpacing"
#define XmCSiblingSpacing   "SiblingSpacing"

#define XmNsaveGraph       "saveGraph"
#define XmCSaveGraph       "SaveGraph"

#define XmNsaveFileName     "saveFileName"
#define XmCSaveFileName     "SaveFileName"

	/* these have class XmCCallback  */

#define XmNnewArcCallback           "newArcCallback"

#define XmNnewNodeCallback	    "newNodeCallback"
#define XmNallowMultipleSelections  "allowMultipleSelections"
#define  XmCAllowMultipleSelections "AllowMultipleSelections"

#define XmNnodeMovedCallback	    "nodeMovedCallback"

#define XmNarcMovedCallback	    "arcMovedCallback"

#define XmNselectNodeCallback	    "selectNodeCallback"

#define XmNselectArcCallback	    "selectArcCallback"

#define XmNdeselectCallback	    "deselectCallback"

#define XmNselectSubgraphCallback   "selectSubgraphCallback"

#define XmNdeleteNodeCallback	    "deleteNodeCallback"

#define XmNdeleteArcCallback	    "deleteArcCallback"

#define XmNdefaultNodeClass "defaultNodeClass"

#define XmCDefaultNodeClass "DefaultNodeClass"

#define XmNdefaultLabel     "defaultLabel"

#define XmRArcDrawMode      "ArcDrawMode"

#define XmRAutoLayoutType     "AutoLayoutType"

#define XmNinteractiveArcDirection     "interactiveArcDirection"

#define XmCInteractiveArcDirection     "InteractiveArcDirection"

#define XmNmovableNodes   "movableNodes"
#define XmCMovableNodes    "MovableNodes"

#define XmNshowCrossingArcs  "showCrossingArcs"
#define XmCShowCrossingArcs  "ShowCrossingArcs"

#define XmNptrCursor  "ptrCursor"
#define XmNmotionCursor  "motionCursor"
#define XmNindicateCursor  "indicateCursor"
#define XmNindicateChildCursor  "indicateChildCursor"
#define XmNindicateParentCursor "indicateParentCursor"

#define XmNinitializeDataCallback  "initializeDataCallback"
#define XmNfreeDataCallback  "freeDataCallback"

#define XmNlayoutProc  "layoutProc"
#define XmCLayoutProc  "LayoutProc"


/****************************************************
 * Callback reasons
 *****************************************************/
#ifdef WINTERP
/*
 * NPM: note that Motif 1.2 defines new callback reasons up to
 * XmCR_OBSCURED_TRAVERSAL==46. Therefore, we can't use
 * case below with "#define XmCR_NEW_ARC 41..." due to 
 * conflicting callback reason id's. For WINTERP, we start
 * with id==60, giving some room for growth with Motif 1.3, etc.
 */
enum {
  XmCR_NEW_ARC = 60, XmCR_NEW_NODE, XmCR_NODE_MOVED,
  XmCR_ARC_MOVED, XmCR_SUBGRAPH_MOVED, XmCR_ARC_EDITED,
  XmCR_SELECT_NODE, XmCR_SELECT_ARC, XmCR_SELECT_SUBGRAPH,
  XmCR_DELETE_NODE, XmCR_DELETE_ARC,
#ifndef XmCR_SELECT
  XmCR_SELECT,
#endif /* XmCR_SELECT */
  XmCR_RELEASE, XmCR_NODE_DOUBLE_CLICK, XmCR_ARC_DOUBLE_CLICK,
  XmCR_DOUBLE_CLICK, XmCR_DESELECT_ARC, XmCR_DESELECT_NODE,
  XmCR_DESELECT, XmCR_NODES_MOVED, XmCR_SELECT_NODES,
  XmCR_SELECT_ARCS, XmCR_SELECT_ARCS_AND_NODES
  };
#else /* !defined(WINTERP) -- from original Interex XmGraph -- iworks.ecn.uiowa.edu:comp.hp/GUI_classic/XmGraph.R5.tar.Z */
#define XmCR_NEW_ARC			41
#define XmCR_NEW_NODE                   42
#define XmCR_NODE_MOVED			43
#define XmCR_ARC_MOVED			44
#define XmCR_SUBGRAPH_MOVED		45
#define XmCR_ARC_EDITED			46
#define XmCR_SELECT_NODE		47
#define XmCR_SELECT_ARC			48
#define XmCR_SELECT_SUBGRAPH		49
#define XmCR_DELETE_NODE		50
#define XmCR_DELETE_ARC			51

#ifndef XmCR_SELECT
#define XmCR_SELECT			52
#endif

#define XmCR_RELEASE			53
#define XmCR_NODE_DOUBLE_CLICK		54
#define XmCR_ARC_DOUBLE_CLICK		55
#define XmCR_DOUBLE_CLICK		56
#define XmCR_DESELECT_ARC		57
#define XmCR_DESELECT_NODE		58
#define XmCR_DESELECT		        59
#define XmCR_NODES_MOVED		60
#define XmCR_SELECT_NODES		61
#define XmCR_SELECT_ARCS		62
#define XmCR_SELECT_ARCS_AND_NODES		63
#endif /* WINTERP */

#define XmPOSITION_FIXED                 0
#define XmPOSITION_RELATIVE              1

typedef struct {
    int            reason;
    XEvent        *event;
    Boolean        interactive;
    WidgetList     selected_widgets;
    int            num_selected_widgets;
    WidgetList     selected_arcs;
    int            num_selected_arcs;
    Widget         widget;
    Widget         old_to;    /* Used for move and edit arc callbacks */
    Widget         old_from;
    Widget         new_to;
    Widget         new_from;
    Boolean        doit;
} XmGraphCallbackStruct;

typedef enum  {XmNEVER, XmALWAYS, XmARCS_ONLY, XmNODES_ONLY, XmPARTIAL} autoLayoutType;

#endif /* _XmGraph_h */


/* DO NOT ADD ANYTHING AFTER THIS #endif */



