#ifndef _XmpTable_h
#define _XmpTable_h
#include <X11/Xmp/COPY>

#ifdef __cplusplus	/* for C++ V2.0 */
extern "C" {
#endif

/*
 * SCCS_data: @(#) Table.h 1.9 92/10/28 07:50:35
 *
 * XmpTable - Forms-based composite widget/geometry manager
 *            Subclassed from Motif manager widgets
 *
 * Original Author:
 *	David Harrison
 *	University of California, Berkeley
 *	1989
 *
 * Re-Implementation:
 *	David E. Smyth		David.Smyth@SniAp.MchP.SNI.De
 *	1992
 *
 * This file contains the XmpTable public declarations.
 */
 
/*
 * XmpTable Widget Parameters
 *
 * Name                    Class                   RepType       Default Value
 *
 * layout                  Layout                  XmpTableLoc   NULL
 * defaultOptions          DefaultOptions          XmpTableOpts  NULL
 * forceShrink             ForceShrink             Boolean       True
 * shrinkSimple            ShrinkSimple            Boolean       True
 * columnSpacing           Spacing                 int           0
 * rowSpacing              Spacing                 int           0
 *
 * Inheritace Heirarchy (therefore see man pages for these widget types
 * for additional resources):
 * Core, Composite, Constraint (although no constraints are defined!),
 * XmManager, XmBulletinBoard, XmpTable.
 */

#define XtNlayout			"layout"
#define XtNdefaultOptions		"defaultOptions"
#define XtNshrinkSimple			"shrinkSimple"
#define XtNforceShrink			"forceShrink"
#define XtNcolumnSpacing        	"columnSpacing"
#define XtNrowSpacing           	"rowSpacing"

#define XtCLayout			"Layout"
#define XtCDefaultOptions		"DefaultOptions"
#define XtCForceShrink			"ForceShrink"
#define XtCShrinkSimple			"ShrinkSimple"
#ifndef XtCSpacing
#define XtCSpacing			"Spacing"
#endif

#define XtRXmpTableLoc		"XmpTableLoc"
#define XtRXmpTableOpts		"XmpTableOpts"

/*
 * Option masks
 */
#define TBL_LEFT	(1<<0)
#define TBL_RIGHT	(1<<1)
#define TBL_TOP		(1<<2)
#define TBL_BOTTOM	(1<<3)
#define TBL_SM_WIDTH	(1<<4)
#define TBL_SM_HEIGHT	(1<<5)	
#define TBL_LK_WIDTH	(1<<6)
#define TBL_LK_HEIGHT	(1<<7)

#define TBL_DEF_OPT	-1

typedef int XmpTableOpts;

/*
 * Opaque class and instance records
 */

typedef struct _XmpTableLoc		*XmpTableLoc;
typedef struct _XmpTableClassRec	*XmpTableWidgetClass;
typedef struct _XmpTableRec		*XmpTableWidget;

extern WidgetClass xmpTableWidgetClass;

#define XmpIsTable(w) XtIsSubclass(w,xmpTableWidgetClass)

/******************************************************************************
** Macros for ANSI and K&R Function Decls
******************************************************************************/

#ifndef NeedFunctionPrototypes
#if defined(FUNCPROTO) || defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus)
#define NeedFunctionPrototypes 1
#else
#define NeedFunctionPrototypes 0
#endif /* __STDC__ */
#endif /* NeedFunctionPrototypes */

#ifndef _
/* Macro for ANSI or K&R external declarations.  Declare them like this:
**
**      int foo _(( int, MapAg ));
**
** DO NOT forget whitespace before the '_' !!
*/
#if NeedFunctionPrototypes
#define _(a) a          /* ANSI results in: int foo ( int, MapAg ); */
#else
#define _(a) ()         /* K&R  results in: int foo ();                       */
#endif
#endif

/******************************************************************************
** XmpTable Public Functions
******************************************************************************/

/* Support for new XmpTable data types
*/
extern XmpTableLoc  XmpTableLocParse  _(( char*		/*layout*/	));
extern void         XmpTableLocFree   _(( XmpTableLoc	/*to_free*/	));
extern XmpTableOpts XmpTableOptsParse _(( char*		/*opt_string*/	));

extern void XmpCvtStrToXmpTableOpts _((	XrmValue*, Cardinal*,
					XrmValue*, XrmValue* ));
extern void XmpCvtStrToXmpTableLoc  _((	XrmValue*, Cardinal*,
					XrmValue*, XrmValue* ));

/* Support for configuring children of an XmpTable
*/
extern void XmpTableChildPosition _((	Widget		/*child*/,
					int		/*col*/,
					int		/*row*/		));

extern void XmpTableChildResize	_((	Widget		/*child*/,
					int		/*col_span*/,
					int		/*row_span*/	));

extern void XmpTableChildOptions _((	Widget		/*child*/,
					XmpTableOpts	/*opts*/	));

extern void XmpTableChildConfig	_((	Widget		/*child*/,
					int		/*col*/,
					int		/*row*/,
					int		/*col_span*/,
					int		/*row_span*/,
					XmpTableOpts	/*opts*/	));

/* Constructors
*/
extern Widget XmpCreateTable	_((	Widget		/*parent*/,
					char*		/*name*/,
					ArgList		/*args*/,
					Cardinal	/*numArgs*/	));

extern Widget XmpCreateTableDialog _((	Widget		/*parent*/,
					char*		/*name*/,
					ArgList		/*args*/,
					Cardinal	/*numArgs*/	));

extern Widget XmpCreateTableTransient _(( Widget	/*parent*/,
					char*		/*name*/,
					ArgList		/*args*/,
					Cardinal	/*numArgs*/	));

#ifdef __cplusplus	/* for C++ V2.0 */
}
#endif

#endif /* _XmpTable_h */

