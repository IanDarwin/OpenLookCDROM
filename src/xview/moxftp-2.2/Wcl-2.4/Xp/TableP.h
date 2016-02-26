#ifndef _XpTableP_h
#define _XpTableP_h
#include <X11/Xp/COPY>

/*
 * SCCS_data:    @(#) TableP.h	1.7 92/11/06 13:46:48
 *
 * XpTable -	Forms-based composite widget/geometry manager.
 *		Class heirarchy:
 *			Core
 *			Composite
 *			XpTable
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
 *	David.Smyth@SniAP.MchP.SNI.De
 */

#include <X11/Xp/Table.h>
#include <X11/Shell.h>

#ifdef __cplusplus	/* for C++ V2.0 */
extern "C" {
#endif

/* Method definitions
**====================**
   No new methods.  I can't imagine how anyone is going to sub-class
   THIS Widget, as the coupling between the behavior and the instance
   members is rather intense!
*/

/* Table Class Part: Information kept in class record
**====================================================**
*/

typedef struct _XpTableClassPart {
    caddr_t			extension;
} XpTableClassPart;

/* Class hierarchy
**=================**
*/

typedef struct _XpTableClassRec {
    CoreClassPart		core_class;
    CompositeClassPart		composite_class;
    XpTableClassPart		table_class;
} XpTableClassRec;

extern XpTableClassRec xpTableClassRec;


/* Private data types
**====================**
   All of these structures are often used as elements of null terminated arrays.
*/

/* Table Location structs.
**========================**
   These are used to describe each widget location, in both the
   default_layout and the real_layout.  In the default_layout, the
   location structs contain a quark, in the real_layout they contain the
   actual widget and the orig* fields are set.
*/
typedef struct _XpTableLoc {
    XrmQuark		w_quark;		/* Widget name quark	*/
    Widget		w;			/* actual widget	*/
    int			col, row;		/* Position in table	*/
    int			col_span, row_span;	/* Positions spanned	*/
    int			orig_width, orig_height;/* Childs orig size ...	*/
    int			orig_border_width;	/* ... and border width	*/
    XpTableOpts  	options;		/* Child layout options	*/
} XpTableLocRec;				/* 	*XpTableLoc	*/

/* TableLoc Methods
**==================**
*/
#define TABLE XpTableWidget

extern XpTableLoc XpTableLocNew            _(( int   /*count*/  ));
extern XpTableLoc XpTableLocParse          _(( char* /*layout*/ ));
extern XpTableLoc XpTableLocGrow           _(( XpTableLoc ));
extern XpTableLoc XpTableLocCopy           _(( XpTableLoc ));
extern XpTableLoc XpTableLocFind           _(( XpTableLoc, Widget ));
extern XpTableLoc XpTableLocFindDefault    _(( XpTableLoc, Widget ));
extern XpTableLoc XpTableLocFindAtPosition _(( XpTableLoc, int, int ));
extern int  XpTableLocLen                  _(( XpTableLoc ));
extern int  XpTableLocPreferredWidth       _(( XpTableLoc, TABLE ));
extern int  XpTableLocPreferredHeight      _(( XpTableLoc, TABLE ));
extern int  XpTableLocNumCols              _(( XpTableLoc ));
extern int  XpTableLocNumRows              _(( XpTableLoc ));
extern int  XpTableLocCompareColSpan       _(( XpTableLoc, XpTableLoc ));
extern int  XpTableLocCompareRowSpan       _(( XpTableLoc, XpTableLoc ));
extern void XpTableLocFree                 _(( XpTableLoc ));

/* Table Vector Structs
**======================**
   A table has two of these vectors: one for columns, and one for rows.
*/
typedef int XpTableVectorOpts;
#define	TBL_VEC_MINIMIZE	0x01
#define	TBL_VEC_LOCK		0x02
#define	TBL_VEC_NOGROW		(TBL_VEC_MINIMIZE | TBL_VEC_LOCK)

typedef struct _XpTableVector {
    XpTableVectorOpts   options;	/* Apply to entire col or row	*/
    int			value;		/* width of col, hieght of row	*/
    int			pref_value;	/* minimum or preferred value	*/
    int			offset;		/* of upper left corner of cell	*/
} XpTableVectorRec, *XpTableVector;

/* TableVector Methods
**=====================**
*/
#define DO_COL (int)1
#define DO_ROW (int)0

extern XpTableVector XpTableVectorNew   _(( int, TABLE, int ));
extern void XpTableVectorFree           _(( XpTableVector ));
extern void XpTableVectorMinimize       _(( XpTableVector, int, TABLE, int));
extern int  XpTableVectorTotalSize      _(( XpTableVector, int, TABLE, int));
extern int  XpTableVectorPreferredSize  _(( XpTableVector, int, TABLE, int));
extern void XpTableVectorAdjust         _(( XpTableVector, int, int ));
extern void XpTableVectorComputeOffsets _(( XpTableVector, int, int, int ));

#undef TABLE

typedef enum _ResizeStatus { RSinit, RSdone, RSdueToRequest } ResizeStatus;

/* Table Part: Information kept in instance record
**=================================================**
*/

typedef struct _XpTablePart {
    /* controlling members, set by SetValues or from resource database
    */
    Boolean		force_shrink;	/* Shrink smaller than pref'd	*/
    Boolean		shrink_simple;	/* obsolete: will be removed	*/
    int  		margin_width;	/* to left and right of kids	*/
    int  		margin_height;	/* above and below table kids	*/
    int			col_spacing;	/* Space between columns	*/
    int			row_spacing;	/* Space between rows		*/
    XpTableOpts  	default_options;/* Default child layout options	*/
    XpTableLoc  	default_layout;	/* Layout spec (orig from xrdb)	*/

    /* internally computed members
    */
    XpTableLoc  	real_layout;	/* Computed current layout	*/
    int			num_cols;	/* Number of columns		*/
    XpTableVector	cols;		/* Widths and opts of each col	*/
    int			num_rows;	/* Number of rows		*/
    XpTableVector	rows;		/* Heights and opts of each row	*/

    /* State indications
    */
    ResizeStatus	resize_status;		/* if Resize was invoked*/
    Boolean		in_changed_managed;	/* for preferred sizes	*/

    /* Makes geometry management more efficient
    */
    Boolean		requesting_resize;
    Dimension		requesting_width, requesting_height;

    /* Geometry data to support child resize request:
     * resize_*		These fields are the values the child is asking for.
     * approved_*	These fields are computed by table and returned to
     *			the child.
     * current_*	These fields are placeholders to save the current
     *			vectors when computing the proposed vectors.
     * query_*		These fields are for the table, obtained by a query
     *			to the table's parent.
     */
    Widget		resize_child;
    XtGeometryMask	resize_mode;
    Dimension		resize_width, resize_height, resize_border_width;
    Widget		approved_child;
    XtGeometryMask	approved_mode;
    Dimension		approved_width, approved_height, approved_border_width;
    XpTableVector	approved_cols, approved_rows;
    XpTableVector	current_cols, current_rows;
    XtGeometryMask	query_mode;
    Dimension		query_width, query_height;
    Boolean		resize_table_to_size_pre_approved_by_parent;

} XpTablePart;

/* Instance hierarchy
**====================**
*/

typedef struct _XpTableRec {
    CorePart            core;
    CompositePart       composite;
    XpTablePart         table;
} XpTableRec;

/* Geometry Management Support Methods
**=====================================**
*/
extern void XpTableNewLayout              _(( XpTableWidget ));
extern void XpTableRecomputeLayout        _(( XpTableWidget ));
extern void XpTableNewProposedLayout      _(( XpTableWidget ));
extern void XpTableUseProposedLayout      _(( XpTableWidget ));
extern void XpTableResizeLayout           _(( XpTableWidget ));

extern void XpTableSaveProposedLayout     _(( XpTableWidget ));
extern void XpTableGetProposedLayout      _(( XpTableWidget ));
extern void XpTableForgetProposedLayout   _(( XpTableWidget ));
extern void XpTableForgetResizeChild      _(( XpTableWidget ));
extern void XpTableNewRealLayout          _(( XpTableWidget ));
extern void XpTableAppendToDefaultLayout  _(( XpTableWidget, XpTableLoc ));
extern void XpTableNewColsAndRows         _(( XpTableWidget ));
extern void XpTableProposedColsAndRows    _(( XpTableWidget ));
extern void XpTableMakeColsFitWidth       _(( XpTableWidget ));
extern void XpTableMakeColsFitQueryWidth  _(( XpTableWidget ));
extern void XpTableMakeRowsFitHeight      _(( XpTableWidget ));
extern void XpTableMakeRowsFitQueryHeight _(( XpTableWidget ));
extern void XpTableFitThis                _(( XpTableWidget, int, int ));
extern int  XpTablePreferredWidth         _(( XpTableWidget ));
extern int  XpTablePreferredHeight        _(( XpTableWidget ));
extern void XpTableRequestResize          _(( XpTableWidget ));
extern void XpTableQueryParentForResize   _(( XpTableWidget ));
extern void XpTableGetProposedChildSize   _(( XpTableWidget ));
extern void XpTableApproveGeometryChanges _(( XpTableWidget, Widget,
                                              int, int, int ));
extern void XpTableSetGeometryOfChildren  _(( XpTableWidget ));
extern void XpTableComputeChildSize       _(( XpTableWidget, XpTableLoc,
                                              int*, int*, int*, int*, int* ));

#ifdef __cplusplus	/* for C++ V2.0 */
}
#endif

#endif /* _XpTableP_h */
