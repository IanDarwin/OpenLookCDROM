#ifndef _XpTable_h
#define _XpTable_h
#include <X11/Xp/COPY>

#ifdef __cplusplus	/* for C++ V2.0 */
extern "C" {
#endif

/*
 * SCCS_data: @(#) Table.h 1.5 92/10/28 07:53:52
 *
 * XpTable - Forms-based composite widget/geometry manager
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
 * This file contains the XpTable public declarations.
 */
 

/*
 * XpTable Widget Parameters
 *
 * Name			Class		RepType		Default Value
 *
 * layout		Layout		XpTableLoc	NULL
 * defaultOptions	DefaultOptions	XpTableOpts	NULL
 * forceShrink		ForceShrink	Boolean		True
 * shrinkSimple		ShrinkSimple	Boolean		True
 * marginWidth		Margins		int		0
 * marginHeight		Margins		int		0
 * columnSpacing	Spacing		int		0
 * rowSpacing		Spacing		int		0
 *
 * Inheritace Heirarchy (therefore see man pages for these widget types
 * for additional resources):
 * Core, Composite, XpTable.
 */

#define XtNlayout		"layout"
#define XtNdefaultOptions	"defaultOptions"
#define XtNshrinkSimple		"shrinkSimple"
#define XtNforceShrink		"forceShrink"
#define XtNmarginWidth		"marginWidth"
#define XtNmarginHeight		"marginHeight"
#define XtNcolumnSpacing        "columnSpacing"
#define XtNrowSpacing           "rowSpacing"

#define XtCLayout		"Layout"
#define XtCDefaultOptions	"DefaultOptions"
#define XtCForceShrink		"ForceShrink"
#define XtCShrinkSimple		"ShrinkSimple"
#define XtCMargins		"Margins"
#define XtCSpacing		"Spacing"

#define XtRXpTableLoc		"XpTableLoc"
#define XtRXpTableOpts		"XpTableOpts"

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

typedef int XpTableOpts;

/*
 * Opaque class and instance records
 */

typedef struct _XpTableLoc	*XpTableLoc;
typedef struct _XpTableClassRec	*XpTableWidgetClass;
typedef struct _XpTableRec	*XpTableWidget;

extern WidgetClass xpTableWidgetClass;

#define XpIsTable(w) XtIsSubclass(w,xpTableWidgetClass)

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
#define _(a) a          /* ANSI results in: int foo ( int, MapAg );	*/
#else
#define _(a) ()         /* K&R  results in: int foo ();			*/
#endif
#endif

/******************************************************************************
** XpTable Public Functions
******************************************************************************/

/* Support for new XpTable data types
*/
extern XpTableLoc  XpTableLocParse  _((	char*		/*layout*/	));
extern void        XpTableLocFree   _((	XpTableLoc	/*to_free*/	));
extern XpTableOpts XpTableOptsParse _(( char*		/*opt_string*/	));

extern void XpCvtStrToXpTableOpts _((	XrmValue*, Cardinal*,
					XrmValue*, XrmValue* ));
extern void XpCvtStrToXpTableLoc _((	XrmValue*, Cardinal*,
					XrmValue*, XrmValue* ));

/* Support for configuring children of an XpTable
*/
extern void XpTableChildPosition _((	Widget		/*child*/,
					int		/*col*/,
					int		/*row*/		));

extern void XpTableChildResize	_((	Widget		/*child*/,
					int		/*col_span*/,
					int		/*row_span*/	));


extern void XpTableChildOptions _((	Widget		/*child*/,
					XpTableOpts	/*opts*/	));

extern void XpTableChildConfig	_((	Widget		/*child*/,
					int		/*col*/,
					int		/*row*/,
					int		/*col_span*/,
					int		/*row_span*/,
					XpTableOpts	/*opts*/	));

/* Constructors
*/
extern Widget XpCreateTable	_((	Widget		/*parent*/,
					char*		/*name*/,
					ArgList		/*args*/,
					Cardinal	/*numArgs*/	));

extern Widget XpCreateTableDialog _((	Widget		/*parent*/,
					char*		/*name*/,
					ArgList		/*args*/,
					Cardinal	/*numArgs*/	));

extern Widget XpCreateTableTransient _(( Widget		/*parent*/,
					char*		/*name*/,
					ArgList		/*args*/,
					Cardinal	/*numArgs*/	));

#ifdef __cplusplus	/* for C++ V2.0 */
}
#endif

#endif /* _XpTable_h */

