#ifndef _XmpTableP_h
#define _XmpTableP_h
#include <X11/Xmp/COPY>

/*
 * SCCS_data:    @(#) TableP.h	1.9 92/11/04 15:39:34
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
 *	David.Smyth@SniAP.MchP.SNI.De
 */

#include <X11/Xmp/Table.h>
#include <Xm/XmP.h>
#include <Xm/BulletinBP.h>

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

typedef struct _XmpTableClassPart {
    caddr_t			extension;
} XmpTableClassPart;

/* Class hierarchy
**=================**
*/

typedef struct _XmpTableClassRec {
    CoreClassPart		core_class;
    CompositeClassPart		composite_class;
    ConstraintClassPart  	constraint_class;
    XmManagerClassPart  	manager_class;
    XmBulletinBoardClassPart	bulletin_class;
    XmpTableClassPart		table_class;
} XmpTableClassRec;

extern XmpTableClassRec xmpTableClassRec;


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
typedef struct _XmpTableLoc {
    XrmQuark		w_quark;		/* Widget name quark	*/
    Widget		w;			/* actual widget	*/
    int			col, row;		/* Position in table	*/
    int			col_span, row_span;	/* Positions spanned	*/
    int			orig_width, orig_height;/* Childs orig size ...	*/
    int			orig_border_width;	/* ... and border width	*/
    XmpTableOpts	options;		/* Child layout options	*/
} XmpTableLocRec;				/* 	*XmpTableLoc	*/

/* TableLoc Methods
**==================**
*/
#define TABLE XmpTableWidget

extern XmpTableLoc XmpTableLocNew            _(( int   /*count*/  ));
extern XmpTableLoc XmpTableLocParse          _(( char* /*layout*/ ));
extern XmpTableLoc XmpTableLocGrow           _(( XmpTableLoc ));
extern XmpTableLoc XmpTableLocCopy           _(( XmpTableLoc ));
extern XmpTableLoc XmpTableLocFind           _(( XmpTableLoc, Widget ));
extern XmpTableLoc XmpTableLocFindDefault    _(( XmpTableLoc, Widget ));
extern XmpTableLoc XmpTableLocFindAtPosition _(( XmpTableLoc, int, int ));
extern int  XmpTableLocLen                   _(( XmpTableLoc ));
extern int  XmpTableLocPreferredWidth        _(( XmpTableLoc, TABLE ));
extern int  XmpTableLocPreferredHeight       _(( XmpTableLoc, TABLE ));
extern int  XmpTableLocNumCols               _(( XmpTableLoc ));
extern int  XmpTableLocNumRows               _(( XmpTableLoc ));
extern int  XmpTableLocCompareColSpan        _(( XmpTableLoc, XmpTableLoc ));
extern int  XmpTableLocCompareRowSpan        _(( XmpTableLoc, XmpTableLoc ));
extern void XmpTableLocFree                  _(( XmpTableLoc ));

/* Table Vector Structs
**======================**
   A table has two of these vectors: one for columns, and one for rows.
*/
typedef int XmpTableVectorOpts;
#define	TBL_VEC_MINIMIZE	0x01
#define	TBL_VEC_LOCK		0x02
#define	TBL_VEC_NOGROW		(TBL_VEC_MINIMIZE | TBL_VEC_LOCK)

typedef struct _XmpTableVector {
    XmpTableVectorOpts  options;	/* Apply to entire col or row	*/
    int			value;		/* width of col, hieght of row	*/
    int			pref_value;	/* minimum or preferred value	*/
    int			offset;		/* of upper left corner of cell	*/
} XmpTableVectorRec, *XmpTableVector;

/* TableVector Methods
**=====================**
*/
#define DO_COL (int)1
#define DO_ROW (int)0

extern XmpTableVector XmpTableVectorNew  _(( int, TABLE, int ));
extern void XmpTableVectorFree           _(( XmpTableVector ));
extern void XmpTableVectorMinimize       _(( XmpTableVector, int, TABLE, int));
extern int  XmpTableVectorTotalSize      _(( XmpTableVector, int, TABLE, int));
extern int  XmpTableVectorPreferredSize  _(( XmpTableVector, int, TABLE, int));
extern void XmpTableVectorAdjust         _(( XmpTableVector, int, int ));
extern void XmpTableVectorComputeOffsets _(( XmpTableVector, int, int, int ));

#undef TABLE

typedef enum _ResizeStatus { RSinit, RSdone, RSdueToRequest } ResizeStatus;

/* Table Part: Information kept in instance record
**=================================================**
*/

typedef struct _XmpTablePart {
    /* controlling members, set by SetValues or from resource database
    */
    Boolean		force_shrink;	/* Shrink smaller than pref'd	*/
    Boolean		shrink_simple;	/* obsolete: will be removed	*/
    int			col_spacing;	/* Space between columns	*/
    int			row_spacing;	/* Space between rows		*/
    XmpTableOpts	default_options;/* Default child layout options	*/
    XmpTableLoc  	default_layout;	/* Layout spec (orig from xrdb)	*/

    /* internally computed members
    */
    XmpTableLoc  	real_layout;	/* Computed current layout	*/
    int			num_cols;	/* Number of columns		*/
    XmpTableVector	cols;		/* Widths and opts of each col	*/
    int			num_rows;	/* Number of rows		*/
    XmpTableVector	rows;		/* Heights and opts of each row	*/

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
    XmpTableVector	approved_cols, approved_rows;
    XmpTableVector	current_cols, current_rows;
    XtGeometryMask	query_mode;
    Dimension		query_width, query_height;
    Boolean		resize_table_to_size_pre_approved_by_parent;

} XmpTablePart;

/* Instance hierarchy
**====================**
*/

typedef struct _XmpTableRec {
    CorePart            core;
    CompositePart       composite;
    ConstraintPart      constraint;
    XmManagerPart       manager;
    XmBulletinBoardPart bulletin_board;
    XmpTablePart        table;
} XmpTableRec;

/* Geometry Management Support Methods
**=====================================**
*/
extern void XmpTableNewLayout              _(( XmpTableWidget ));
extern void XmpTableRecomputeLayout        _(( XmpTableWidget ));
extern void XmpTableNewProposedLayout      _(( XmpTableWidget ));
extern void XmpTableUseProposedLayout      _(( XmpTableWidget ));
extern void XmpTableResizeLayout           _(( XmpTableWidget ));

extern void XmpTableSaveProposedLayout     _(( XmpTableWidget ));
extern void XmpTableGetProposedLayout      _(( XmpTableWidget ));
extern void XmpTableForgetProposedLayout   _(( XmpTableWidget ));
extern void XmpTableForgetResizeChild      _(( XmpTableWidget ));
extern void XmpTableNewRealLayout          _(( XmpTableWidget ));
extern void XmpTableAppendToDefaultLayout  _(( XmpTableWidget, XmpTableLoc ));
extern void XmpTableNewColsAndRows         _(( XmpTableWidget ));
extern void XmpTableProposedColsAndRows    _(( XmpTableWidget ));
extern void XmpTableMakeColsFitWidth       _(( XmpTableWidget ));
extern void XmpTableMakeColsFitQueryWidth  _(( XmpTableWidget ));
extern void XmpTableMakeRowsFitHeight      _(( XmpTableWidget ));
extern void XmpTableMakeRowsFitQueryHeight _(( XmpTableWidget ));
extern void XmpTableFitThis                _(( XmpTableWidget, int, int ));
extern int  XmpTablePreferredWidth         _(( XmpTableWidget ));
extern int  XmpTablePreferredHeight        _(( XmpTableWidget ));
extern void XmpTableRequestResize          _(( XmpTableWidget ));
extern void XmpTableQueryParentForResize   _(( XmpTableWidget ));
extern void XmpTableGetProposedChildSize   _(( XmpTableWidget ));
extern void XmpTableApproveGeometryChanges _(( XmpTableWidget, Widget,
                                               int, int, int ));
extern void XmpTableSetGeometryOfChildren  _(( XmpTableWidget ));
extern void XmpTableComputeChildSize       _(( XmpTableWidget, XmpTableLoc,
                                               int*, int*, int*, int*, int* ));

#ifdef __cplusplus	/* for C++ V2.0 */
}
#endif

#endif /* _XmpTableP_h */
