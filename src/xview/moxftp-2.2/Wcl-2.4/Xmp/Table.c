/*LINTLIBRARY*/
/*
 * SCCS_data:    @(#) Table.c	1.17 92/12/09 14:20:59
 *
 * XmpTable -	Forms-based composite widget/geometry manager derived from
 *		Motif manager widgets.  Class heirarchy:
 *			Core
 *			Composite
 *			Constraint
 *			XmManager
 *			XmBulletinBoard
 *			XmpTable
 *
 * Originally implemented by:
 *	David Harrison
 *	University of California, Berkeley
 *	1989
 *
 * Many bug fixes and enhancements provided by
 *	marbru@auto-trol.com	Martin Brunecky
 *	nazgul@alphalpha.com	Kee Hinckley
 *	pastor@PRC.Unisys.COM	Jon A. Pastor
 *
 * Completely re-implemented by:
 *	David.Smyth@SniAp.MchP.SNI.De
 */

/*
Edit History

25Oct92		david	Geometry management re-work
01Feb92		david	Re-Implementation

*/

#include <X11/Xmp/COPY>
#include <X11/IntrinsicP.h>

#ifdef sun
#include <X11/ObjectP.h>        /* why don't they just use X from mit!?! */
#include <X11/RectObjP.h>
#endif

#include <X11/StringDefs.h>
#include <Xm/DialogS.h>

#include <X11/Xmp/TableP.h>

/* For HP platforms
**==================**
*/
#ifndef XtInheritFocusMovedProc
#define XtInheritFocusMovedProc XmInheritFocusMovedProc
#endif  /* !XtInheritFocusMovedProc */

/* For R4 and DECwindows
**=======================**
*/
#ifdef USE_XtResizeWidget
#define _XmResizeObject(c,w,h,b) XtResizeWidget(c,w,h,b)
#endif
#ifdef USE_XtMoveWidget
#define _XmMoveObject(w,x,y) XtMoveWidget(w,x,y)
#endif

/* For backward compatibility with old Xt releases
**=================================================**
*/
#ifndef XtIsWidget
#ifdef XtSpecificationRelease
#define XtIsWidget(obj) XtIsSubclass(obj,(WidgetClass)coreWidgetClass)
#else
#define XtIsWidget(obj) XtIsSubclass(obj,(WidgetClass)widgetClass)
#endif
#endif

#ifndef XtSpecificationRelease
#if NeedFunctionPrototypes
typedef void*   XtPointer;
#else
typedef char*   XtPointer;
#endif
#endif


/* Resources
**===========**
*/
#ifdef XtOffsetOf
#define OFFSET(field) XtOffsetOf(XmpTableRec,table.field)
#else
#define OFFSET(field) XtOffset(XmpTableWidget,table.field)
#endif

static XtResource resources[] = {
 { XtNdefaultOptions, XtCDefaultOptions, XtRXmpTableOpts, sizeof(XmpTableOpts),
   OFFSET(default_options), XtRImmediate, (XtPointer)0 },
 { XtNlayout, XtCLayout, XtRXmpTableLoc, sizeof(XmpTableLoc),
   OFFSET(default_layout), XtRXmpTableLoc, (XtPointer)0 },
 { XtNforceShrink, XtCForceShrink, XtRBoolean, sizeof(Boolean),
   OFFSET(force_shrink), XtRImmediate, (XtPointer)True },
 { XtNshrinkSimple, XtCShrinkSimple, XtRBoolean, sizeof(Boolean),	/*OBS*/
   OFFSET(shrink_simple), XtRImmediate, (XtPointer)True },		/*OBS*/
 { XtNcolumnSpacing, XtCSpacing, XtRInt, sizeof(int),
   OFFSET(col_spacing), XtRImmediate, (XtPointer)0 },
 { XtNrowSpacing, XtCSpacing, XtRInt, sizeof(int),
   OFFSET(row_spacing), XtRImmediate, (XtPointer)0 },
};

#undef OFFSET

/* Core Class Methods
**====================**
*/
static void		XmpTableClassInitialize ();
static void		XmpTableInitialize _((	Widget, Widget,
						ArgList, Cardinal*	));
static void		XmpTableDestroy _((	Widget			));
static void		XmpTableResize _((	Widget			));
#ifdef XtSpecificationRelease
static Boolean		XmpTableSetValues _((	Widget, Widget, Widget,
						ArgList, Cardinal*	));
#else
static Boolean		XmpTableSetValues _((	Widget, Widget, Widget  ));
#endif
static XtGeometryResult	XmpTableQueryGeometry _(( Widget,
						XtWidgetGeometry*,
						XtWidgetGeometry*	));

/* Composite class methods
**=========================**
*/
static XtGeometryResult	XmpTableGeometryManager _(( Widget /*child*/,
						XtWidgetGeometry*,
						XtWidgetGeometry*	));
static void		XmpTableChangeManaged _(( Widget		));




XmpTableClassRec xmpTableClassRec = {
  { /* core_class fields		*/
    /* superclass		*/	(WidgetClass)&xmBulletinBoardClassRec,
    /* class_name			*/	"XmpTable",
    /* widget_size			*/	sizeof(XmpTableRec),
    /* class_initialize  		*/	XmpTableClassInitialize,
    /* class_part_initialize		*/	NULL,
    /* class_inited			*/	FALSE,
    /* initialize			*/	XmpTableInitialize,
    /* initialize_hook			*/	NULL,
    /* realize				*/	XtInheritRealize,
    /* actions				*/	NULL,
    /* num_actions			*/	0,
    /* resources			*/	resources,
    /* num_resources			*/	XtNumber(resources),
    /* xrm_class			*/	NULLQUARK,
    /* compress_motion			*/	False,
    /* compress_exposure		*/	TRUE,
    /* compress_enterleave		*/	False,
    /* visible_interest			*/	FALSE,
    /* destroy  			*/	XmpTableDestroy,
    /* resize				*/	XmpTableResize,
    /* expose				*/	XtInheritExpose,
    /* set_values			*/	XmpTableSetValues,
    /* set_values_hook			*/	NULL,
    /* set_values_almost		*/	XtInheritSetValuesAlmost,
    /* get_values_hook			*/	NULL,
    /* accept_focus			*/	NULL,
    /* version				*/	XtVersion,
    /* callback_private			*/	NULL,
    /* tm_table				*/	XtInheritTranslations,
    /* query_geometry			*/	XmpTableQueryGeometry,
    /* display_accelerator		*/	NULL,
    /* extension			*/	NULL
  },
  { /* composite_class fields		*/
    /* geometry_manager  		*/	XmpTableGeometryManager,
    /* change_managed			*/	XmpTableChangeManaged,
    /* insert_child			*/	XtInheritInsertChild,
    /* delete_child			*/	XtInheritDeleteChild,
    /* extension			*/	NULL
  },
  { /* constraint_class fields  	*/
    /* resources			*/	NULL,
    /* num_resources			*/	0,
    /* constraint_size  		*/	0,
    /* initialize			*/	NULL,
    /* destroy  			*/	NULL,
    /* set_values			*/	NULL,
    /* extension			*/	NULL
  },
  { /* manager_class fields		*/
#if XmVersion > 1000
    /* translations			*/	XtInheritTranslations,
    /* syn_resources			*/	NULL,
    /* num_syn_resources		*/	0,
    /* syn_cont_resources		*/	NULL,
    /* num_syn_cont_resources		*/	0,
    /* parent_process			*/	XmInheritParentProcess,
#else
    /* translations			*/	(XtTranslations) _XtInherit,
    /* get_resources			*/	NULL,
    /* num_get_resources		*/	0,
    /* get_constraint_resources  	*/	NULL,
    /* num_get_constraint_resources	*/	0,
#endif
    /* extension			*/	NULL
  },
  { /* bulletin_board_class fields	*/
    /* always_install_accelerators	*/	False,
#if XmVersion > 1000
    /* geo_matrix_create		*/	NULL,
    /* focus_moved_proc  		*/	XtInheritFocusMovedProc,
#endif
    /* extension			*/	NULL
  },
  { /* table_class fields		*/
    /* extension			*/	NULL
  }
};

WidgetClass xmpTableWidgetClass = (WidgetClass) &xmpTableClassRec;


/* Converters
**============**
*/

/* Convert String to TableOpts
**=============================**
   Converts a string representation into a TableOpts, which is small enough
   to fit entirely into the to->addr.
*/
/*ARGSUSED*/
void XmpCvtStrToXmpTableOpts( args, num_args, from, to )
    XrmValue* args;		/* Arguments to converter */
    Cardinal* num_args;		/* Number of arguments    */
    XrmValue* from;		/* From type              */
    XrmValue* to;		/* To type                */
{
    static XmpTableOpts opts;

    if (*num_args != 0) {
	XtErrorMsg("XmpCvtStrToXmpTableOpts", "wrongParameters",
		   "XtToolkitError",
		   "String to options takes no additional arguments",
		   (String*)0, (Cardinal*)0);
    }

    opts = XmpTableOptsParse( (String) from->addr );

    if (opts == (XmpTableOpts)0)
	XtStringConversionWarning( (String) from->addr, XtRXmpTableOpts );

    to->addr = (caddr_t) &opts;
    to->size = sizeof( XmpTableOpts );
}

/* Convert String to TableLocRec Array
**=====================================**
   Converts a string representation into an array of TableLocRec structures
   (ie., TableLocRec*).  This XtCalloc'd array is kept by the resource
   database: a COPY must be made by the widget.
*/
/*ARGSUSED*/
void XmpCvtStrToXmpTableLoc( args, num_args, from, to )
    XrmValue* args;		/* Arguments to converter */
    Cardinal* num_args;		/* Number of arguments    */
    XrmValue* from;		/* From type              */
    XrmValue* to;		/* To type                */
{
    static XmpTableLoc defLocs;

    if (*num_args != 0) {
	XtErrorMsg("XmpCvtStrToXmpTableLoc", "wrongParameters",
		   "XtToolkitError",
		   "String to layout takes no additional arguments",
		   (String*)0, (Cardinal*)0);
    }

    defLocs = XmpTableLocParse( (String) from->addr );

    if (defLocs == (XmpTableLoc)0)
	XtStringConversionWarning( (String) from->addr, XtRXmpTableLoc );

    to->addr = (caddr_t) &defLocs;
    to->size = sizeof(caddr_t);
}


/* Initialization Methods
**========================**
   Note that no class part initialization is needed, as there are
   no inherited methods (yet?).
*/

static void XmpTableClassInitialize()
{
    XtAddConverter(	XtRString, XtRXmpTableOpts,
			XmpCvtStrToXmpTableOpts, (XtConvertArgList)0, 0 );
    XtAddConverter(	XtRString, XtRXmpTableLoc,
			XmpCvtStrToXmpTableLoc, (XtConvertArgList)0, 0 );
}

/*ARGSUSED*/
static void XmpTableInitialize( requestWidget, newWidget, args, num_args )
    Widget	requestWidget;		/* as already set by Xt		*/
    Widget	newWidget;		/* set up by this method	*/
    ArgList	args;
    Cardinal*	num_args;
{
    XmpTableWidget tw = (XmpTableWidget) newWidget;

    /* Copy resource values specified by pointer
    */
    tw->table.default_layout = XmpTableLocCopy(tw->table.default_layout);

    /* Initialize internally computed members
    */
    tw->table.real_layout = (XmpTableLoc)0;
    tw->table.num_cols    = 0;
    tw->table.cols        = (XmpTableVector)0;
    tw->table.num_rows    = 0;
    tw->table.rows        = (XmpTableVector)0;

    tw->table.resize_status      = RSinit;
    tw->table.in_changed_managed = False;

    tw->table.requesting_resize = False;
    tw->table.requesting_width  = (Dimension)0;
    tw->table.requesting_height = (Dimension)0;

    tw->table.resize_child        = (Widget)0;
    tw->table.resize_mode         = (XtGeometryMask)0;
    tw->table.resize_width        = (Dimension)0;
    tw->table.resize_height       = (Dimension)0;
    tw->table.resize_border_width = (Dimension)0;

    tw->table.approved_child        = (Widget)0;
    tw->table.approved_mode         = (XtGeometryMask)0;
    tw->table.approved_width        = (Dimension)0;
    tw->table.approved_height       = (Dimension)0;
    tw->table.approved_border_width = (Dimension)0;
    tw->table.approved_cols         = (XmpTableVector)0;
    tw->table.approved_rows         = (XmpTableVector)0;

    tw->table.current_cols          = (XmpTableVector)0;
    tw->table.current_rows          = (XmpTableVector)0;

    tw->table.query_mode            = (XtGeometryMask)0;
    tw->table.query_width           = (Dimension)0;
    tw->table.query_height          = (Dimension)0;

    tw->table.resize_table_to_size_pre_approved_by_parent = False;
}
 


/* Destroy Method
**================**
   Free any instance data allocated for the TablePart members.  
*/
static void XmpTableDestroy( w )
    Widget w;			/* Widget to destroy */
{
    XmpTableWidget tw = (XmpTableWidget) w;

    XmpTableLocFree(    tw->table.default_layout );
    XmpTableLocFree(    tw->table.real_layout );
    XmpTableVectorFree( tw->table.cols );
    XmpTableVectorFree( tw->table.rows );
    XmpTableVectorFree( tw->table.approved_cols );
    XmpTableVectorFree( tw->table.approved_rows );
    XmpTableVectorFree( tw->table.current_cols );
    XmpTableVectorFree( tw->table.current_rows );
}



/* Geometry Management and Negotiation
**=====================================**
   The following methods are involved in geometry management and
   negotiation:

   TableResize() method:  The parent can issue a resize demand by
   invoking TableResize() via XtResizeWidget().  This can be due to the
   parent being resized from above (perhaps the user changed the shell
   size via the window manager) or it may be due to the Table itself
   asking to be made a different size for any of several reasons.

   TableSetValues() method:  Geometry management gets involved when a
   change occurs to any Table resource, or any of several superclass
   resources (width, height, margin_width).

   TableQueryGeometry() method:  A parent of a Table asks the table for
   its preferred geometry using this method, invoked via XtQueryGeometry().
   Usually generates a proposed layout which is valid only if the next
   gemoetry management and negotiation method invoked is TableQueryGeometry()
   for the same child asking for approved sizes.  All other geometry
   management and negotiation methods must clear the proposed layout.

   TableGeometryManager() method:  This method is invoked when a child
   wishes to become a different size.

   TableChangeManaged() method: this method is invoked when the set of
   managed children changes.  This triggers the initial layout of the
   Table widget, and causes the existing layout to be re-computed.
*/

/* Recompute Layout Due To Parent Demand
**=======================================**
   This gets called when a parent tells the table it must re-size.  We
   certainly have real_layout, rows, cols, already done (or null if no
   managed children).

   If we are being resized due to the initial resize request from a child,
   then we will have alot of the work done IFF the size the table is now
   the size the parent *said* is would change the table to become.  We
   cannot really trust this, because all this geometry negotiation stuff
   is so goddamn complex that one is amazed when anything actually DOES work...

   Therefore, if the table is being resized, and the size is as we expect,
   then we just resize and the optimizations work as planned.  This means
   we can use the proposed column and row vectors which have already been
   adjusted to fit the pre-approved size, but we still size and position
   all the children, and redisplay.

   If we are being resized for any other reason, or if we are being resized
   to a size different from that previously approved by this tables's parent,
   then we need to flush the proposed layout stuff, and do a full resize:
   adjust the cols and rows to fit the table, size and position all the
   children, and redisplay.

   Cause the background of the Table widget to be re-displayed.
*/

static void XmpTableResize( w )
    Widget w;
{
    XmpTableWidget tw = (XmpTableWidget)w;

    if ( tw->table.resize_table_to_size_pre_approved_by_parent
      && tw->core.width  == tw->table.query_width
      && tw->core.height == tw->table.query_height )
    {
	XmpTableResizeLayout( tw );
    }
    else
    {
	tw->table.resize_table_to_size_pre_approved_by_parent = False;
	XmpTableForgetProposedLayout( tw );
	XmpTableResizeLayout(         tw );
    }
}



/* Set Values
**============**
   This method gets called via XtSetValues().  If any table members or
   margin width have been changed, then geometry management must be done.

   We do not have to worry about geometry changes (core.width etc) because
   Xt takes care of invoking TableResize directly.
*/

#ifdef XtSpecificationRelease
/*ARGSUSED*/
static Boolean XmpTableSetValues(currentWidget, ignoreRequestWidget, newWidget,
				 ignored, notUsed )
    Widget	currentWidget;		/* Before call to XtSetValues */
    Widget	ignoreRequestWidget;	/* After call to XtSetValues  */
    Widget	newWidget;		/* Final version of widget    */
    ArgList	ignored;
    Cardinal*	notUsed;
#else
static Boolean XmpTableSetValues(currentWidget, ignoreRequestWidget, newWidget)
    Widget	currentWidget;		/* Before call to XtSetValues */
    Widget	ignoreRequestWidget;	/* After call to XtSetValues  */
    Widget	newWidget;		/* Final version of widget    */
#endif
{
    XmpTableWidget current = (XmpTableWidget) currentWidget;
    XmpTableWidget new     = (XmpTableWidget) newWidget;

    if ( current->table.force_shrink	!= new->table.force_shrink
      || current->table.col_spacing	!= new->table.col_spacing
      || current->table.row_spacing	!= new->table.row_spacing
      || current->table.default_options	!= new->table.default_options
      || current->bulletin_board.margin_width
					!= new->bulletin_board.margin_width
      || current->bulletin_board.margin_height
					!= new->bulletin_board.margin_height)
    {
	/* We need to do some re-layout
	*/
	XmpTableForgetProposedLayout( current );

	if ( current->table.default_layout != new->table.default_layout )
	{
	    /* values set by pointers require special handling:
	     * free old value, copy and alloc new value.
	     */
	    XmpTableLocFree( current->table.default_layout );
	    new->table.default_layout = XmpTableLocCopy(
						new->table.default_layout );
	    /* We need to do a complete recomputation and placement
	    */
	    XmpTableNewLayout( new );
	}
	else
	{
	    /* we only need to change some things in the existing real_layout
	    */
	    XmpTableRecomputeLayout( new );
	}
	/* Let Xt know exposure is needed
	*/
	return True;
    }
    /* No exposure needed due to Table resources
    */
    return False;
}



/* Provide Preferred Geometry To Parent
**======================================**
   If the parent asks if the table can grow, the answer is always yes.  If
   the parent asks for the table to shrink to a size smaller than the
   preferred width or height, then the answer is almost, with the preferred
   width and height provided.
*/
static XtGeometryResult XmpTableQueryGeometry(w, request, geo_return)
    Widget w;				/* XmpTable widget	*/
    XtWidgetGeometry *request;		/* Parent intended size	*/
    XtWidgetGeometry *geo_return;	/* preferred size	*/
{
    XmpTableWidget tw = (XmpTableWidget) w;
    int pref;

    /* First check for queries which would not result in a resize.
     * According to the spec, "No" means "Don't bother."
     */
    if (  request->request_mode & CWWidth
     &&   request->width  == tw->core.width
     &&   request->request_mode & CWHeight
     &&   request->height == tw->core.height )
	return XtGeometryNo;
    if (  request->request_mode & CWWidth
     &&   request->width == tw->core.width )
	return XtGeometryNo;
    if (  request->request_mode & CWHeight
     &&   request->height == tw->core.height )
	return XtGeometryNo;

    /* Note that this table may be in the process of asking the parent for a
     * resize, and the parent may (from within the parent's GeometryManager
     * method) ask the table for its preferred size.  TablePreferredWidth()
     * and TablePreferredHeight() can tell this is happening, because
     * TableMakeResizeRequest() sets the flag tw->table.requesting_resize.
     * In such a case, the preferred size returned is the size the table is
     * asking to become.
     */
    if ( request->request_mode == (XtGeometryMask)0 )
    {
	/* Parent is asking for all preferred dimensions
	*/
	geo_return->request_mode = CWWidth|CWHeight;
	geo_return->width  = XmpTablePreferredWidth(  tw );
	geo_return->height = XmpTablePreferredHeight( tw );
    }
    if ( request->request_mode & CWWidth )
    {
	pref = XmpTablePreferredWidth( tw );
	if ( request->width < (Dimension)pref )
	{
	    geo_return->width = (Dimension)pref;
	    geo_return->request_mode |= CWWidth;
	}
	else
	{
	    geo_return->width = request->width;
	}
    }
    if ( request->request_mode & CWHeight )
    {
	pref = XmpTablePreferredHeight( tw );
	if ( request->height < (Dimension)pref )
	{
	    geo_return->height = (Dimension)pref;
	    geo_return->request_mode |= CWHeight;
	}
	else
	{
	    geo_return->height = request->height;
	}
    }

    /* XtGeometryNo means already at preferred (minimum) size (dont't bother)
    */
    if ( geo_return->width  == tw->core.width
     &&  geo_return->height == tw->core.height )
	return XtGeometryNo;

    /* XtGeometryAlmost means has a preferred size different from current size
    */
    if ( geo_return->request_mode & (CWWidth|CWHeight) )
	return XtGeometryAlmost;

    /* XtGeometryYes is only returned if request is to change geometry in ways
     * which do not matter to the table layout: x&y position, border_width,
     * sibling, and stack_mode.
     */
    return XtGeometryYes;
}



/* Handle Geometry Requests from Children
**========================================**
   Only called by Xt if the child is managed.  Therefore, we can trust that
   the table already has valid row and column vectors.

   Position changes are always rejected.

   Size changes are processed as followed: We store the child and the child's
   requested size in the Table.  We also save the current row and column
   vectors.  Then we "pretend" that we are going to accept the requested
   child size: create new row and column vectors, make them fit the Table's
   width and height.  Then, instead of changing the geometries of all the
   children, we see what the size of the requesting child would be.  If (by
   some miracle) the resulatant size is the requested size, then we really
   do the layout.  In reality, the size will almost ALWAYS be different from
   the requested, so we return XtGeometryAlmost, save the newly computed
   row and column vectors, and remember the approved size of the child.

   In most cases, the child will simply immediately re-invoke this method,
   passing the approved sizes.  We can then use the pre-computed column and
   row vectors, and set the geometries of all the children (including the
   requesting child) and return XtGeometryDone (XtGeometryYes).

   If, however, ANY other geometry method is invoked (due to set values,
   change managed, or resize command from parent) then the pre-computed
   vectors are invalid, and the approved size is invalid.
*/

static XtGeometryResult XmpTableGeometryManager( child, request, reply )
    Widget child;		/* Widget                    */
    XtWidgetGeometry *request;	/* Requested geometry change */
    XtWidgetGeometry *reply;	/* Actual reply to request   */
{
    Widget		parent	= child->core.parent;
    XmpTableWidget	tw	= (XmpTableWidget) parent;
    Dimension		width, height, border_width;

    if ( !parent || !XmpIsTable( parent ) )
	XtErrorMsg("XmpTableGeometryManager", "badParent", "XtToolkitError",
		   "Parent of widget is not an XmpTableWidget",
		   (String*)0, (Cardinal*)0);

    /* If request is only for geometry things ignored by table, do nothing.
    */
    if ( 0 == request->request_mode & (CWWidth|CWHeight|CWBorderWidth))
	return XtGeometryNo;

    /* Get the relevent dimensions of child: request or current.
    */
    if ( request->request_mode & CWWidth )
	width = request->width;
    else
	width = child->core.width;

    if ( request->request_mode & CWHeight )
	height = request->height;
    else
	height = child->core.height;

    if ( request->request_mode & CWBorderWidth )
	border_width = request->border_width;
    else
	border_width = child->core.border_width;

    /* We can use the pre-computed proposed layout ONLY if the same child is
     * requesting pre-approved sizes.  Note: approved_mode does NOT have
     * XtCWQueryOnly set!
     */
    if ( tw->table.approved_child        == child
      && tw->table.approved_mode         == request->request_mode
      && tw->table.approved_width        == width 
      && tw->table.approved_height       == height 
      && tw->table.approved_border_width == border_width )
    {
	/* Request is exactly as approved.
	*/
	XmpTableUseProposedLayout( tw );
	return XtGeometryDone;
    }

    /* No pre-approved layout, or something is different. Need to
     * re-compute proposed layout.
     */
    XmpTableForgetProposedLayout( tw );

    /* Pretend we re-size the child, and go through table layout logic. Then
     * look at the resulting size of the child, and that will be the size we
     * approve for the child.  If by some fluke the approved size is the size
     * requested, then we do the change, otherwise return Almost.
     */
    tw->table.resize_child        = child;
    tw->table.resize_mode         = request->request_mode;
    tw->table.resize_width        = width;
    tw->table.resize_height       = height;
    tw->table.resize_border_width = border_width;

    XmpTableNewProposedLayout( tw );

    XmpTableForgetResizeChild( tw );

    reply->request_mode = tw->table.approved_mode;
    reply->width        = tw->table.approved_width;
    reply->height       = tw->table.approved_height;
    reply->border_width = tw->table.approved_border_width;

    /* Note: TableNewProposedLayout() will only set CWWidth or CWHeight or
     * CWBorderWidth into approved_mode.  Therefore, if request_mode has
     * XtCWQueryOnly or any of the flags ignored by table (like CWStackMode)
     * then approved_mode will be different from request_mode
     */
    if ( ( !(request->request_mode & CWWidth)
        ||  (tw->table.approved_width == width) )
      && ( !(request->request_mode & CWHeight)
        ||  (tw->table.approved_height == height) )
      && ( !(request->request_mode & CWBorderWidth)
        ||  (tw->table.approved_border_width == border_width) ) )
    {
	/* Everything which is asked to change matched the approved changes.
	*/
	if ( (tw->table.approved_mode|XtCWQueryOnly) == request->request_mode )
	{
	    /* child only queried: we would grant child's request exactly
	    */
	    return XtGeometryYes;
	}
	else if ( tw->table.approved_mode == request->request_mode )
	{
	    /* Request is exactly as approved.
	    */
	    XmpTableUseProposedLayout( tw );
	    return XtGeometryDone;
	}
    }

    if ( tw->table.approved_mode
      && ( tw->table.approved_width        != child->core.width
        || tw->table.approved_height       != child->core.height
        || tw->table.approved_border_width != child->core.border_width ) )
    {
	/* Something is approved, and some approved size is different from
	 * existing size, so some of the child's geometry would change.
	 * approved_mode as provided by TableNewProposedLayout() already
	 * reflects those fields which would actually change.
	 */
	return XtGeometryAlmost;
    }
    else
    {
	/* Approved size is exactly the same as current size.
	*/
	return XtGeometryNo;
    }
}

/* Handle Increase or Decrease in Managed Children
**=================================================**
   Called when a child or when children are managed or unmanaged via
   XtManageChild(), XtUnmanageChild() etc.
*/
static void XmpTableChangeManaged( w )
    Widget w;
{
    XmpTableWidget tw = (XmpTableWidget)w;

    tw->table.in_changed_managed = True;

    XmpTableNewLayout( tw );

    tw->table.in_changed_managed = False;
}

/*===========================**
** End Of Xt Invoked Methods **
**===========================*/

/* Internal Table Methods
**========================**
   There are these ways the Table may need to be recomputed:
 o ChangedManage: Compute new layout, ask parent for new size.
 o SetValues: Compute new layout or recompute layout, ask parent for new size.
 o Child change: Recompute layout, ask parent for new size.
 o Initial GeometryRequest from child: Compute proposed layout.
 o Approved GeometryRequest from child: Use proposed layout, ask parent for
   new size.
 o Resize Method: Command to change to specific size from above.
*/

void XmpTableNewLayout( tw )
    XmpTableWidget tw;
{
    XmpTableNewRealLayout(  tw );
    XmpTableNewColsAndRows( tw );
    XmpTableRequestResize(  tw );
}

void XmpTableRecomputeLayout( tw )
    XmpTableWidget tw;
{
    XmpTableNewColsAndRows( tw );
    XmpTableRequestResize(  tw );
}

void XmpTableNewProposedLayout( tw )
    XmpTableWidget tw;
{
    XmpTableProposedColsAndRows(    tw );
    XmpTableQueryParentForResize(   tw );    /* query only, no resize */
    XmpTableMakeColsFitQueryWidth(  tw );
    XmpTableMakeRowsFitQueryHeight( tw );
    XmpTableGetProposedChildSize(   tw );
    XmpTableSaveProposedLayout(     tw );
}

void XmpTableUseProposedLayout( tw )
    XmpTableWidget tw;
{
    XmpTableGetProposedLayout( tw );

    tw->table.resize_table_to_size_pre_approved_by_parent = True;
    XmpTableRequestResize( tw );
    tw->table.resize_table_to_size_pre_approved_by_parent = False;
}
    
/* Called due to TableRequestResize()
*/
void XmpTableResizeLayout( tw )
    XmpTableWidget tw;
{
    if ( !tw->table.resize_table_to_size_pre_approved_by_parent )
    {
	XmpTableMakeColsFitWidth(  tw );
	XmpTableMakeRowsFitHeight( tw );
    }

    XmpTableSetGeometryOfChildren( tw );

    /* I have to do this in case someone is using those goddamn gadgets.
     * Normally, a manager should not need to redisplay itself, right!?!
     */
    if ( XtIsRealized( (Widget)tw ) )
    {
	XClearArea( XtDisplay( (Widget)tw ), XtWindow( (Widget)tw ),
		    0, 0, 0, 0,		/* clears entire window		*/
		    True );		/* we need Expose events	*/
    }

    tw->table.resize_status = RSdone;
}


/* Proposed Layout Methods
**=========================**
   A resize request from a child is virtually NEVER granted straight away:
   Instead, a proposed layout must be computed, and the resultant size of
   the child is then returned.  If the child can accept this size (nearly
   always yes in actual existing widgets), then it immediately returns these
   approved sizes, and the table can detect this and use the proposed layout.

   If any other geometry method is invoked in the meantime, the proposed
   layout must be cleared and forgotten.
*/

void XmpTableSaveProposedLayout( tw )
    XmpTableWidget tw;
{
    tw->table.approved_cols = tw->table.cols;
    tw->table.approved_rows = tw->table.rows;

    tw->table.cols = tw->table.current_cols;
    tw->table.rows = tw->table.current_rows;
}

void XmpTableGetProposedLayout( tw )
    XmpTableWidget tw;
{
    XmpTableVectorFree( tw->table.cols );
    XmpTableVectorFree( tw->table.rows );

    tw->table.cols = tw->table.approved_cols;
    tw->table.rows = tw->table.approved_rows;

    tw->table.approved_child        = (Widget)0;
    tw->table.approved_mode         = (XtGeometryMask)0;
    tw->table.approved_width        = (Dimension)0;
    tw->table.approved_height       = (Dimension)0;
    tw->table.approved_border_width = (Dimension)0;
    tw->table.approved_cols         = (XmpTableVector)0;
    tw->table.approved_rows         = (XmpTableVector)0;
}

void XmpTableForgetProposedLayout( tw )
    XmpTableWidget tw;
{
    XmpTableVectorFree( tw->table.approved_cols );
    XmpTableVectorFree( tw->table.approved_rows );

    tw->table.approved_child        = (Widget)0;
    tw->table.approved_mode         = (XtGeometryMask)0;
    tw->table.approved_width        = (Dimension)0;
    tw->table.approved_height       = (Dimension)0;
    tw->table.approved_border_width = (Dimension)0;
    tw->table.approved_cols         = (XmpTableVector)0;
    tw->table.approved_rows         = (XmpTableVector)0;
    tw->table.current_cols          = (XmpTableVector)0;
    tw->table.current_rows          = (XmpTableVector)0;
    tw->table.query_mode            = (XtGeometryMask)0;
    tw->table.query_width           = (Dimension)0;
    tw->table.query_height          = (Dimension)0;
    tw->table.resize_table_to_size_pre_approved_by_parent = False;
}

void XmpTableForgetResizeChild( tw )
    XmpTableWidget tw;
{
    tw->table.resize_child        = (Widget)0;
    tw->table.resize_mode         = (XtGeometryMask)0;
    tw->table.resize_width        = (Dimension)0;
    tw->table.resize_height       = (Dimension)0;
    tw->table.resize_border_width = (Dimension)0;
}


/* Build a new real_layout for all managed children.
**==================================================**
   Each location is a function of the real_layout, default_layout, 
   default_options, and automatic positioning.

   The list of managed children in traversed: if a managed child already
   appears in the current real_layout, that layout is copied.  Otherwise,
   location data comes from default_layout, default_options, and automatic
   positioning members of the parent table widget.

   Since both the default_layout and real_layout are changed by TablePosition(),
   TableResize(), TableOptions(), and TableConfig(), changes to the children
   remain in effect when the table layout is re-computed, and when children
   become managed and unmanaged multiple times.  However, if the default_layout
   is changed by a XtSetValues, all the positioning stuff in default_layout is
   lost.  OK, since both are done by the client program: if the programmer wants
   to change the layout, the programmer will also need to reposition children.
*/

void XmpTableNewRealLayout( tw )
    XmpTableWidget tw;
{
    int		num_children	= tw->composite.num_children;
    WidgetList	children	= tw->composite.children;
    XmpTableLoc	result		= XmpTableLocNew( num_children );
    XmpTableLoc	loc		= result;
    XmpTableLoc	found;

    int		child	= 0;	/* index into list of all children */

    for ( ;  child < num_children  ;  child++ )
    {
	Widget w = children[child];

	if ( XtIsManaged( w ) )
	{
	    if ( found = XmpTableLocFind( tw->table.real_layout, w ) )
	    {
		/* This widget was in previous layout, copy all fields
		 */
		*loc = *found;
	    }
	    else if ( found = XmpTableLocFind( tw->table.default_layout, w ) )
	    {
		/* This child has been laid out before, so copy everything.
		 */
		*loc = *found;
	    }
	    else if ( found = XmpTableLocFindDefault( tw->table.default_layout,
							w ) )
	    {
		/* Never laid out this child, but default layout provides
		 * some information.  Copy everything, fill in the blanks
		 * (col,row,col_span,row_span already have defaults).
		 * No problem if tw->table.default_options is zero.
		 */
		*loc = *found;
		loc->w = w;
		loc->orig_width        = w->core.width;
		loc->orig_height       = w->core.height;
		loc->orig_border_width = w->core.border_width;
		if ( !loc->options )
		    loc->options = tw->table.default_options;

		XmpTableAppendToDefaultLayout( tw, loc );
	    }
	    else
	    {
		/* Never laid out this child, not in default layout.  Fill
		 * in everything with default values.
		 */
		loc->w = w;
		loc->w_quark = w->core.xrm_name;
		loc->col = loc->row = 0;
		loc->col_span = loc->row_span = 1;
		loc->orig_width        = w->core.width;
		loc->orig_height       = w->core.height;
		loc->orig_border_width = w->core.border_width;
		if ( !loc->options )
		    loc->options = tw->table.default_options;

		XmpTableAppendToDefaultLayout( tw, loc );
	    }
	    loc++;	/* loc only incremented for MANAGED children */
	}
    }
    XmpTableLocFree( tw->table.real_layout );
    tw->table.real_layout = result;
}

/* Append loc to default_layout
**==============================**
*/
void XmpTableAppendToDefaultLayout( tw, loc )
    XmpTableWidget tw;
    XmpTableLoc    loc;
{
    int inx;

    tw->table.default_layout = XmpTableLocGrow( tw->table.default_layout );
    inx = XmpTableLocLen( tw->table.default_layout );
    tw->table.default_layout[inx] = *loc;
}

/* Create New Cols and Rows Vectors
**==================================**
   This must be done whenever a new real_layout is created, or when
   the existing real_layout has been changed.
*/
void XmpTableNewColsAndRows( tw )
    XmpTableWidget tw;
{
    XmpTableVectorFree( tw->table.cols );
    XmpTableVectorFree( tw->table.rows );

    tw->table.num_cols = XmpTableLocNumCols( tw->table.real_layout );
    tw->table.num_rows = XmpTableLocNumRows( tw->table.real_layout );

    if ( tw->table.num_cols && tw->table.num_rows )
    {
	tw->table.cols = XmpTableVectorNew( tw->table.num_cols, tw, DO_COL );
	tw->table.rows = XmpTableVectorNew( tw->table.num_rows, tw, DO_ROW );
    }
    else
    {
	tw->table.num_cols = tw->table.num_rows = 0;
	tw->table.cols     = tw->table.rows     = (XmpTableVector)0;
    }
}

/* Create New Cols and Rows Vectors for Proposed Layout
**======================================================**
   Save the "real" valid vectors for later.  Note that we CERTAINLY have
   rows and columns, because this is only invoked due to a child's request
   to resize, and only managed children will be processed, and the existence
   of managed children implies the existence of non-null row and column
   vectors.
*/
void XmpTableProposedColsAndRows( tw )
    XmpTableWidget tw;
{
    tw->table.current_cols = tw->table.cols;
    tw->table.current_rows = tw->table.rows;

    tw->table.cols = XmpTableVectorNew( tw->table.num_cols, tw, DO_COL );
    tw->table.rows = XmpTableVectorNew( tw->table.num_rows, tw, DO_ROW );
}

/* Adjust rows and columns to fit
**================================**
   These procedures are called when the Table's parent changes the size of the
   Table.  The TableRecomputeLayout() procedure computes the preferred size of
   the table based on the layout and the children, with the assumption that the
   table could be any size.  Now, we have a specific size, so we will need to
   adjust everything to fit.

   If the new size is larger, then its easy: just expand the space available to
   each row and/or column, and change the geometries of all the children.

   If the new size is smaller and force_shrink is true (the new default), then
   adjust all children to fit the new size.

   If the new size is smaller and force_shrink is false then we must do
   something like a better behaved version of the old behavior:  Shrink
   to the preferred size but no smaller.
*/
void XmpTableMakeColsFitWidth( tw )
    XmpTableWidget tw;
{
    XmpTableFitThis( tw, DO_COL, tw->core.width );
}

void XmpTableMakeColsFitQueryWidth( tw )
    XmpTableWidget tw;
{
    XmpTableFitThis( tw, DO_COL, tw->table.query_width );
}

void XmpTableMakeRowsFitHeight( tw )
    XmpTableWidget tw;
{
    XmpTableFitThis( tw, DO_ROW, tw->core.height );
}

void XmpTableMakeRowsFitQueryHeight( tw )
    XmpTableWidget tw;
{
    XmpTableFitThis( tw, DO_ROW, tw->table.query_height );
}

void XmpTableFitThis( tw, do_col, to_fit )
    XmpTableWidget tw;
    int            do_col, to_fit;
{
    int change, current, prefer, num;
    XmpTableVector vec;

    if ( do_col )
    {
	vec = tw->table.cols;
	num = tw->table.num_cols;
    }
    else
    {
	vec = tw->table.rows;
	num = tw->table.num_rows;
    }
    current = XmpTableVectorTotalSize(     vec, num, tw, do_col );
    prefer  = XmpTableVectorPreferredSize( vec, num, tw, do_col );

    if ( to_fit < prefer  &&  tw->table.force_shrink == False )
    {
	/* Smallest size is preferred size.  Excess clipped.
	*/
	change = prefer - current;
    }
    else
    {
	change = to_fit - current;
    }

    if ( change != 0 )
	XmpTableVectorAdjust( vec, num, change );
}

/* Determine Preferred (Minimum) Size of Table
**=============================================**
*/
int XmpTablePreferredWidth( tw )
    XmpTableWidget tw;
{
    if ( tw->table.requesting_resize )
    {
	return tw->table.requesting_width;
    }
    else
    {
	XmpTableVector	vec = tw->table.cols;
	int		num = tw->table.num_cols;

	return XmpTableVectorPreferredSize( vec, num, tw, DO_COL );
    }
}

int XmpTablePreferredHeight( tw )
    XmpTableWidget tw;
{
    if ( tw->table.requesting_resize )
    {
	return tw->table.requesting_height;
    }
    else
    {
	XmpTableVector	vec = tw->table.rows;
	int		num = tw->table.num_rows;

	return XmpTableVectorPreferredSize( vec, num, tw, DO_ROW );
    }
}

/* Request Resize from Parent
**============================**
   This procedure gets called by other Table methods when the Table instance
   wants to grow or shrink.  Since we cannot yet tell if the desired size is OK,
   the children of the Table have NOT been sized or positioned: this is only
   done by the TableResize method.

   Here is when the wonders of Xt Geometry Management come into play.  We cannot
   tell a priori what the hell is going to happen here.  We can ask the parent
   to allow the table to resize based on the computed width and height of the
   cols and rows.

   If the parent says yes, then TableResize may, or then again, may not, have
   been called.  Since we don't know, we must keep a bogus little flag in the
   instance to indicate what really happened.
*/
void XmpTableRequestResize( tw )
    XmpTableWidget tw;
{
    XtGeometryResult	result;
    XtWidgetGeometry	desired, approved;
   
    /* If this is RSdone after the call to XtMakeResizeRequest(), then we know
     * that TableResize() has been invoked.  Otherwise, we must invoke
     * TableResize() directly.
     */
    tw->table.resize_status = RSdueToRequest;

    /* We may be requesting a resize after a child asked for a resize.
     * In this case, we already have a size pre-approved by the parent.
     * Otherwise, we want to become our preferred size.
     */
    if ( tw->table.resize_table_to_size_pre_approved_by_parent )
    {
	desired.width        = tw->table.query_width;
	desired.height       = tw->table.query_height;
	desired.request_mode = tw->table.query_mode;
    }
    else
    {
	desired.width        = XmpTablePreferredWidth(  tw );
	desired.height       = XmpTablePreferredHeight( tw );
	desired.request_mode = (XtGeometryMask)0;
	if ( desired.width != tw->core.width )
	    desired.request_mode |= CWWidth;
	if ( desired.width != tw->core.height )
	    desired.request_mode |= CWHeight;
    }

    /* XtMakeResizeRequest() asks the parent to allow this table to resize.
     * The parent, a Composite widget, will often need to query all of its
     * children (including this table) to see what sizes they want to be.
     * Therefore, there is a very good chance that the QueryGeometry method of
     * this table widget will be invoked in a few microseconds, and this table
     * will need to compute its desired size.  So, for efficiency we remember
     * our desired size.
     */
    tw->table.requesting_resize = True;
    tw->table.requesting_width  = desired.width;
    tw->table.requesting_height = desired.height;

    /* Careful!  If table is not managed, Xt always returns XtGeometryNo,
     * which is bogus: we CAN assume we can resize if unmanaged.  If we
     * really cannot, then the parent can force us to re-size when the
     * table becomes managed.
     */
    if ( XtIsManaged( (Widget)tw ) )
	result = XtMakeGeometryRequest( (Widget)tw, &desired, &approved );
    else
	result = XtGeometryYes;

    /* Nothing special to do if XtGeometryYes or XtGeometryNo
    */
    if ( result == XtGeometryAlmost )
    {
	/* Now the desired size is the size our parent will allow.
	*/
	result = XtMakeGeometryRequest( (Widget)tw, &approved, &approved );
    }

    tw->table.requesting_resize = False;

    /* No matter what the outcome, the Table must be "resized", as this is
     * where the table looks at its actual width/height and sizes and positions
     * the children widgets.
     */
    if ( tw->table.resize_status == RSdueToRequest )
	XmpTableResize( (Widget)tw );
}

/* Query parent for hypothetical resize.
**======================================**
   This is called when a child has asked to be resized, and such a request,
   if granted, would cause the table to ask its parent for a resize.

   We ask the parent to tell us what size the parent would resize the table,
   if the table really asked for a resize to its "proposed" size.

   Note that we DO NOT ask the parent to resize us, because we are not
   COMPLETELY certain the child will want the size we will propose for it:
   the child may withdraw its resize.

   Unfortunately, the GeometryManager method of the parent may not in fact
   treat this as a simple query, but it may treat it as a request, and the
   table may well have its size changed by a call to its Resize method!  Yes,
   this is a common deficiency in GeometryManager methods of composite widgets.
   It is VERY difficult to get all this &$%?%$!?! right.
*/

void XmpTableQueryParentForResize( tw )
    XmpTableWidget tw;
{
    XtGeometryResult	result;
    XtWidgetGeometry	desired, query;
   
    /* If this is RSdone after the call to XtMakeGeometryRequest(), then we
     * know that TableResize() has been invoked - UNFORTUNATELY!
     */
    tw->table.resize_status = RSdueToRequest;

    /* Query only for width and height
    */
    desired.request_mode = (XtCWQueryOnly|CWWidth|CWHeight);

    /* Desired dimensions reflect change due to resize_child's resize request
    */
    desired.width  = XmpTablePreferredWidth(  tw );
    desired.height = XmpTablePreferredHeight( tw );

    /* XtMakeGeometryRequest() asks the parent for the result of a hypothetical
     * resize request from the table.  The parent, a Composite widget, will
     * often need to query all of its children (including this table) to see
     * what sizes they want to be.  Therefore, there is a very good chance that
     * the QueryGeometry method of this table widget will be invoked in a few
     * microseconds, and this table will need to compute its desired size.  So,
     * for efficiency we remember our desired size.
     */
    tw->table.requesting_resize = True;
    tw->table.requesting_width  = desired.width;
    tw->table.requesting_height = desired.height;

    /* Careful!  If table is not managed, Xt always returns XtGeometryNo,
     * which is bogus: we CAN assume we can resize if unmanaged.  If we
     * really cannot, then the parent can force us to re-size when the
     * table becomes managed.
     */
    if ( XtIsManaged( (Widget)tw ) )
	result = XtMakeGeometryRequest( (Widget)tw, &desired, &query );
    else
	result = XtGeometryYes;

    tw->table.requesting_resize = False;

    switch ( result )
    {
    case XtGeometryYes:
	tw->table.query_mode   = CWWidth | CWHeight;
	tw->table.query_width  = desired.width;
	tw->table.query_height = desired.height;
	break;
    case XtGeometryAlmost:
	tw->table.query_mode   = ( query.request_mode & CWWidth ? CWWidth : 0 )
				|( query.request_mode & CWHeight? CWWidth : 0 );
	tw->table.query_width  = query.width;
	tw->table.query_height = query.height;
	break;
    case XtGeometryNo:
    default:
	/* Cannot resize.  XtMakeResizeRequest does nothing (returns Yes) if
	 * sizes are equal to current.
	 */
	tw->table.query_mode   = (XtGeometryMask)0;
	tw->table.query_width  = tw->core.width;
	tw->table.query_height = tw->core.height;
	break;
    }

    if ( result == XtGeometryYes && tw->table.resize_status != RSdueToRequest )
    {
	/* Oh Shit!  The parent did not notice this was just a QUERY, and
	 * it went ahead and did the change.  Oh Shit!
	 */
	Widget parent = XtParent( (Widget)tw );
	char* class = parent->core.widget_class->core_class.class_name;
	String args[3];
	Cardinal two = 2;
	args[0] = XtName(parent); args[1] = class; args[2] = NULL;	
	XtWarningMsg( "brokenParent", "XmpTableQueryParentForResize",
		"Widget Library Error",
		"Widget %s of class %s\nhas a GeometryManager method which ignores XtCWQueryOnly!",
		args, &two );
	/* Already resized. XtMakeResizeRequest does nothing (returns Yes) if
	 * sizes are equal to current.
	 */
	tw->table.query_mode = (XtGeometryMask)0;
	tw->table.query_width = tw->core.width;
	tw->table.query_height = tw->core.height;
    }
}

/* Get Approved Size of Child
**============================**
   A child has asked to be resized.  We need to compute the size which
   the table would make the child if the table itself was resized (from
   above).  Assume we will grant the border width request UNLESS the
   width or height goes to zero or negative.

   The child is managed (or else it could not make the resize request),
   so the child is in the real_layout.
*/
void XmpTableGetProposedChildSize( tw )
    XmpTableWidget	tw;
{
    int		width, height, bw, x, y;
    XmpTableLoc loc = tw->table.real_layout;

    while ( loc->w && loc->w != tw->table.resize_child )
	++loc;

    if ( loc->w != tw->table.resize_child )
    {
	String args[3];
	Cardinal two = 2;
	args[0] = XtName(tw->table.resize_child);
	args[1] = XtName((Widget)tw);
	args[2] = NULL;
	XtWarningMsg( "unmanagedChildMadeResizeRequest",
		      "XmpTableGetProposedChildSize",
		      "Xt Implementation Bug",
		      "XtMakeGeometryRequest passed request from unmanaged child %s to table %s\n",
		      args, &two );

	XmpTableForgetProposedLayout( tw );
	return;
    }

    /* This loc is for the child requesting the resize.
     *
     * First, try to use all the dimensions: width, height, requested border.
     * If the width and height are not minimums, we are done.
     */
    width = height = 0;			/* need to compute these */
    bw = tw->table.resize_border_width; /* assume we grant this  */

    XmpTableComputeChildSize( tw, loc, &width, &height, &bw, &x, &y );

    if ( 1 < width &&  1 < height )
    {
	XmpTableApproveGeometryChanges( tw, loc->w, width, height, bw );
	return;
    }

    /* Try again, this time using the current border width
    */
    width = height = 0;
    bw = loc->w->core.border_width;

    XmpTableComputeChildSize( tw, loc, &width, &height, &bw, &x, &y );

    if ( width  < 1 ) width  = 1;
    if ( height < 1 ) height = 1;
    XmpTableApproveGeometryChanges( tw, loc->w, width, height, bw );
}

void XmpTableApproveGeometryChanges( tw, child, width, height, bw )
    XmpTableWidget	tw;
    Widget		child;
    int			width, height, bw;
{
    tw->table.approved_child = child;
    tw->table.approved_mode  = (XtGeometryMask)0;

    if ( tw->table.resize_mode&CWBorderWidth && bw != child->core.border_width )
	tw->table.approved_mode |= CWBorderWidth;

    if ( tw->table.resize_mode&CWWidth && width != child->core.width )
	tw->table.approved_mode |= CWWidth;

    if ( tw->table.resize_mode&CWHeight && height != child->core.height )
	tw->table.approved_mode |= CWHeight;

    tw->table.approved_border_width = bw;
    tw->table.approved_width        = width;
    tw->table.approved_height       = height;
}

/* Set Geometry Of Children
**==========================**
   Children are placed according to the real_layout, cols, rows, and
   row and column spacing.
*/
void XmpTableSetGeometryOfChildren( tw )
    XmpTableWidget tw;
{
    XmpTableLoc	loc;
    int		shadow = tw->manager.shadow_thickness;

    if ( tw->table.real_layout	== (XmpTableLoc)0
      || tw->table.cols		== (XmpTableVector)0
      || tw->table.num_cols	== 0
      || tw->table.rows		== (XmpTableVector)0
      || tw->table.num_rows	== 0 )
	return;

    XmpTableVectorComputeOffsets( tw->table.cols, tw->table.num_cols,
                                  tw->bulletin_board.margin_width + shadow,
                                  tw->table.col_spacing );

    XmpTableVectorComputeOffsets( tw->table.rows, tw->table.num_rows,
                                  tw->bulletin_board.margin_height + shadow,
                                  tw->table.row_spacing );

    for ( loc = tw->table.real_layout  ;  loc->w  ;  loc++ )
    {
	int width, height, bw, x, y;

	/* This can be invoked due to a child size change request, in which
	 * case the child has not yet been changed in size, but this size
	 * has been guaranteed to be OK.
	 */
	if ( loc->w == tw->table.approved_child )
	{
	    width  = tw->table.approved_width;
	    height = tw->table.approved_height;
	    bw     = tw->table.approved_border_width;
	}
	else
	{
	    width  = 0;
	    height = 0;
	    bw     = loc->w->core.border_width;
	}

	XmpTableComputeChildSize( tw, loc, &width, &height, &bw, &x, &y );

	if ( width  != loc->w->core.width
	  || height != loc->w->core.height
	  || bw     != loc->w->core.border_width )
	    _XmResizeObject( loc->w, width, height, bw );

	if ( x != loc->w->core.x 
	  || y != loc->w->core.y )
	    _XmMoveObject( loc->w, x, y );
    }
}

void XmpTableComputeChildSize( tw, loc, wP, hP, bwP, xP, yP )
    XmpTableWidget	tw;
    XmpTableLoc		loc;
    int			*wP, *hP;		/* in-out, USE if != 0 */
    int			*bwP;			/* in  */
    int			*xP, *yP;		/* out */
{
    int cell_x, cell_y, cell_w, cell_h;
    int new_x,  new_y,  new_w,  new_h, prefer;
    int total_w, total_h;
    int pad, i;
    int init_w = *wP;	/* non-zero means use it */
    int init_h = *hP;	/* non-zero means use it */
    int bw     = *bwP;

    /* Upper left corner of where we will place the widget
    */
    cell_x = tw->table.cols[ loc->col ].offset;
    cell_y = tw->table.rows[ loc->row ].offset;

    /* cell width and height may well span cols and rows and spacing
    */
    pad = tw->table.col_spacing;
    cell_w = -pad;
    for ( i = 0  ;  i < loc->col_span  ;  i++ )
	cell_w += tw->table.cols[ loc->col + i ].value + pad;

    pad = tw->table.row_spacing;
    cell_h = -pad;
    for ( i = 0  ;  i < loc->row_span  ;  i++ )
	cell_h += tw->table.rows[ loc->row + i ].value + pad;

    /* If the width or height was given, then use it.  This given dimension
     * has already been approved by the table in geometry negotiation (by
     * the GeometryManager method).
     *
     * Otherwise, If size growth is prevented due to TBL_SM_(WIDTH | HEIGHT),
     * then use the lesser of the cell size or the preferred size.
     *
     * Otherwise, use the cell size.
     */
    if (init_w)
    {
	new_w = init_w;
    }
    else if (loc->options & TBL_SM_WIDTH
          && cell_w > (prefer = XmpTableLocPreferredWidth( loc, tw )) )
    {
	new_w = prefer - 2 * bw;
    }
    else
    {
	new_w = cell_w - 2 * bw;
    }

    if (init_h)
    {
	new_h = init_h;
    }
    else if (loc->options & TBL_SM_HEIGHT
          && cell_h > (prefer = XmpTableLocPreferredHeight( loc, tw )) )
    {
	new_h = prefer - 2 * bw;
    }
    else
    {
	new_h = cell_h - 2 * bw;
    }

    /* Be certain that the size does not go to zero, or negative!
    */
    if ( new_w <= 0 ) new_w = 1;	/* value ready for XtResize */
    if ( new_h <= 0 ) new_h = 1;	/* value ready for XtResize */

    total_w = new_w + 2 * bw;
    total_h = new_h + 2 * bw;

    if ( loc->options & TBL_LEFT )
	new_x = cell_x;				/* left justify in cell   */
    else if ( loc->options & TBL_RIGHT )
	new_x = cell_x + cell_w - total_w;	/* right justify in cell  */
    else
	new_x = cell_x + (cell_w - total_w)/2;	/* center in cell         */

    if ( loc->options & TBL_TOP )
	new_y = cell_y;				/* top justify in cell    */
    else if ( loc->options & TBL_BOTTOM )
	new_y = cell_y + cell_h - total_h;	/* bottom justify in cell */
    else
	new_y = cell_y + (cell_h - new_h)/2;	/* center in cell         */

    *wP = new_w;
    *hP = new_h;
    *xP = new_x;
    *yP = new_y;
}


/* TableOpts methods
**===================**
*/

XmpTableOpts XmpTableOptsParse( optString )
    String optString;
{
    XmpTableOpts opt = 0;

    for ( ;  *optString;  optString++) {
	switch (*optString)
	{
	case 'l':	opt |= TBL_LEFT;	break;
	case 'r':	opt |= TBL_RIGHT;	break;
	case 't':	opt |= TBL_TOP;		break;
	case 'b':	opt |= TBL_BOTTOM;	break;
	case 'w':	opt |= TBL_LK_WIDTH;	break;
	case 'h':	opt |= TBL_LK_HEIGHT;	break;
	case 'W':	opt |= TBL_SM_WIDTH;	break;
	case 'H':	opt |= TBL_SM_HEIGHT;
	default:	break;
	}
    }
    return opt;
}


/* Client Utility Functions
**==========================**
The following functions are used to reconfigure children of Table widgets
*/

/* Position Child in Col,Row of Table
**====================================**
*/
#define CHG_POS  0x1
#define CHG_SPAN 0x2
#define CHG_OPTS 0x4
#define CHG_ALL  0x7

static void Change( loc, what, col, row, col_span, row_span, opts )
    XmpTableLoc  loc;			/* specific loc to change	*/
    int          what;			/* What to change		*/
    int          col, row;		/* New position in table	*/
    int          col_span, row_span;	/* New spans of child in table	*/
    XmpTableOpts opts;  		/* New size/justification opts	*/
{
    if ( what & CHG_POS )
    {
	if ( 0 <= col ) loc->col = col;
	if ( 0 <= row ) loc->row = row;
    }
    if ( what & CHG_SPAN )
    {
	if ( 1 <= col_span ) loc->col_span = col_span;
	if ( 1 <= row_span ) loc->row_span = row_span;
    }
    if ( what & CHG_OPTS )
    {
	loc->options = opts;
    }
}

static void XmpTableChildChange(child, what, col, row, col_span, row_span, opts)
    Widget       child;			/* Child widget to change	*/
    int          what;			/* What to change		*/
    int          col, row;		/* New position in table	*/
    int          col_span, row_span;	/* New spans of child in table	*/
    XmpTableOpts opts;  		/* New size/justification opts	*/
{
    if ( !XmpIsTable( child->core.parent ) )
    {
	Cardinal one = 1;
	char* name = XtName( child );
	XtWarningMsg( "notChildOfTable", "XmpTableChildChange", "XmpLibError",
		"Widget %s is not a child of an XmpTable widget.", &name, &one);
	return;
    }
    else
    {
	XmpTableWidget	tw   = (XmpTableWidget)child->core.parent;
	XmpTableLoc	def  = XmpTableLocFind( tw->table.default_layout,child);
	XmpTableLoc	real = XmpTableLocFind( tw->table.real_layout,   child);

	if ( def == (XmpTableLoc)0 )
	{
	    /* Never laid out this child before.  
	    */
	    static XmpTableLocRec	nullRec;
	    XmpTableLocRec		newRec;
	    XmpTableLoc			initDef;

	    newRec = nullRec;
	    def = &newRec;

	    /* Find the initial default for this name, copy what we can.
	    */
	    initDef = XmpTableLocFindDefault( tw->table.default_layout, child );
	    if ( initDef != (XmpTableLoc)0 )
		*def = *initDef;

	    /* Set up default fields.
	    */
	    def->w_quark = child->core.xrm_name;
	    def->w	 = child;
	    if ( def->col_span <= 0 ) def->col_span = 1;
	    if ( def->row_span <= 0 ) def->row_span = 1;
	    if ( def->options  == 0 ) def->options  = tw->table.default_options;
	    def->orig_width        = child->core.width;
	    def->orig_height       = child->core.height;
	    def->orig_border_width = child->core.border_width;

	    /* Append to default_layout, then get pointer to that loc
	    */
	    XmpTableAppendToDefaultLayout( tw, def );
	    def = XmpTableLocFind( tw->table.default_layout,child );
	}

	/* Change loc in default_layout to reflect widget position etc.
	*/
	Change( def, what, col, row, col_span, row_span, opts );
	
	if ( real != (XmpTableLoc)0 )
	{
	    /* In real_layout: Change child's actual position/span/opt
	    */
	    Change( real, what, col, row, col_span, row_span, opts );

	    /* We do not have to create a new real_layout.  Also, we know
	    ** child is managed, since it is in the real_layout.  Therefore:
	    */
	    XmpTableRecomputeLayout( tw );
	}
	/* If the child is not managed, no re-layout needs to be done: it
	** will be done when the child becomes managed (see ChangeManaged)
	*/
    }
}

/* Change Position of Child
**==========================**
*/
void XmpTableChildPosition( child, col, row )
    Widget child;			/* Child widget to move		*/
    int    col, row;			/* New position in table	*/
{
    XmpTableChildChange( child, CHG_POS, col, row, 0, 0, 0);
}

/* Change Size (Span) of Child
**=============================**
*/
void XmpTableChildResize( child, col_span, row_span)
    Widget child;			/* Child widget to resize	*/
    int    col_span, row_span;		/* New widget span		*/
{
    XmpTableChildChange( child, CHG_SPAN, 0, 0, col_span, row_span, 0);
}

void XmpTableChildOptions( child, opts )
    Widget	 child;		/* Child widget to get new options	*/
    XmpTableOpts opts;  	/* New option mask			*/
{
    XmpTableChildChange( child, CHG_OPTS, 0, 0, 0, 0, opts );
}

void XmpTableChildConfig( child, col, row, col_span, row_span, opts )
    Widget child;		/* Child widget to change	*/
    int col, row;		/* New position in table	*/
    int col_span, row_span;	/* New spans of child in table	*/
    XmpTableOpts opts;  	/* New size/justification opts	*/
{
    XmpTableChildChange(child, CHG_ALL , col, row, col_span, row_span, opts);
}

/* Constructors
**==============**
*/

Widget XmpCreateTable( parent, name, arglist, argcount )
    Widget   parent;
    char*    name;
    ArgList  arglist;
    Cardinal argcount;
{
   return XtCreateWidget(name, xmpTableWidgetClass, parent, arglist, argcount);
}

/* Table Dialog Constructor 
** Date: Fri, 8 Feb 91 12:23:39 EST
** From: pastor@PRC.Unisys.COM (Jon A. Pastor)
*/

#ifndef DIALOG_SUFFIX
#define DIALOG_SUFFIX "_popup"
#define DIALOG_SUFFIX_SIZE strlen(DIALOG_SUFFIX)
#endif

/* Destroy parent dialog shell when the child is destroyed.
*/
/*ARGSUSED*/
static void XmpDestroyParentCallback( w, ignored, unused )
    Widget w;
    XtPointer ignored, unused;
{
    XtDestroyWidget( XtParent( w ) );
}

Widget XmpCreateTableDialog( parent, name, arglist, argcount )
    Widget   parent;
    char*    name;
    ArgList  arglist;
    Cardinal argcount;
{
    int    i;
    Arg    shellArgs[2];
    Widget tableShell, table;
    char*  dsName;

    /* Fabricate a name for the dialog shell using Motif 1.1 naming
    */
    dsName = (char*)XtCalloc( strlen(name)+DIALOG_SUFFIX_SIZE+1, sizeof(char) );
    strcpy( dsName, name ); strcat( dsName, DIALOG_SUFFIX );

    /* Create a Motif Dialog Shell widget
    */
    i = 0;
    XtSetArg( shellArgs[i], XtNallowShellResize, True); i++;
    tableShell = XmCreateDialogShell( parent, dsName, shellArgs, i );
    XtFree( dsName );

    /* Create the Table widget
    */
    table = XtCreateWidget( name, xmpTableWidgetClass, tableShell, 
			    arglist, argcount );
    XtAddCallback( table, XtNdestroyCallback,
		   XmpDestroyParentCallback, (XtPointer)0);

    return table;
}

Widget XmpCreateTableTransient( parent, name, arglist, argcount )
    Widget   parent;
    char*    name;
    ArgList  arglist;
    Cardinal argcount;
{
    char*  dsName;
    int    i;
    Arg    shellArgs[2];
    Widget tableShell, table;

    /* Fabricate a name for the dialog shell using Motif 1.1 naming
    */
    dsName = (char*)XtCalloc( strlen(name)+DIALOG_SUFFIX_SIZE+1, sizeof(char) );
    strcpy( dsName, name ); strcat( dsName, DIALOG_SUFFIX );

    /* Create a Transient Shell widget
    */
    i = 0;
    XtSetArg( shellArgs[i], XtNallowShellResize, True); i++;
#ifdef XtTransientForBugIsFixed
#ifdef XtSpecificationRelease
    XtSetArg( shellArgs[i], XtNtransientFor, parent); i++;
#endif
#endif
    tableShell = XtCreatePopupShell( dsName, transientShellWidgetClass, 
					parent, shellArgs, i );
    XtFree( dsName );

    /* Create the Table widget
    */
    table = XtCreateWidget( name, xmpTableWidgetClass, tableShell, 
			    arglist, argcount );
    XtManageChild( table );
    XtAddCallback( table, XtNdestroyCallback,
		   XmpDestroyParentCallback, (XtPointer)0);

    return table;
}
