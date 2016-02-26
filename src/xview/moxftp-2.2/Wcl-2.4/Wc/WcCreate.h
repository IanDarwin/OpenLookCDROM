#ifndef _WcCreate_h
#define _WcCreate_h
#include <X11/Wc/COPY>
#include <stdio.h>

/*
* SCCS_data: @(#) WcCreate.h 1.11 92/10/28 07:49:02
*
* Widget Creation Library - WcCreate.h
*
* Everything declared in this file is the public definition of Wcl version 2.
* No interfaces will be deleted or changed until Wcl 3, but things might be
* added.
*
*******************************************************************************
*/

/****************************************************************************** 
*  Applications should append the WCL_XRM_OPTIONS macro to their
*  XrmOptionDescRec declaration so a user can specify Wcl specific options
*  easily from the command line.
*
*  Applications should also always call WcAppClass( argc, argv ) to
*  get the application class name from the command line, which can
*  then be passed to XtInitialize().  WcAppClass() looks for
*  -name <appName> in the argv, or else it capitalizes the first letter
*  of argv[0] (or first two if the first is already capitalized).
*
*  Therefore, a typical Wcl based application should look something like this:
******************************************************************************
    #include <X11/Wc/WcCreate.h>

    static XrmOptionDescRec options[] = {
	{ ... other application specific options ... },
	WCL_XRM_OPTIONS
    };

    main( argc, argv )
	int    argc;
	char** argv;
    {
	Widget appShell = XtInitialize(
		argv[0],
		WcAppClass( argc, argv ),
		options, XtNumber(options),
		&argc, argv );

	... register widgets, callbacks, and actions ...

	if ( WcWidgetCreation ( appShell ) )
	    exit(1);

	XtRealize( appShell );
	XtMainLoop();
    }
******************************************************************************
*/

#define WCL_XRM_OPTIONS \
    { "-ResFile",	"*wclInitResFile",	XrmoptionSepArg, NULL }, \
    { "-rf",		"*wclInitResFile",	XrmoptionSepArg, NULL }, \
    { "-trrf",		"*wclTraceResFiles",	XrmoptionNoArg,  "on" }, \
    { "-Trace",		"*wcTrace",		XrmoptionNoArg,  "on" }, \
    { "-tr",		"*wcTrace",		XrmoptionNoArg,  "on" }, \
    { "-trtd",		"*wclTraceTemplateDef",	XrmoptionNoArg,	 "on" }, \
    { "-trtx",		"*wcTraceTemplate",	XrmoptionNoArg,	 "on" }, \
    { "-Warnings",	"*wclVerboseWarnings",	XrmoptionNoArg,  "on" }

/******************************************************************************
** Resources Fetched By Wcl
******************************************************************************
*  Resources beginning with "wcl" (class names beginning with "Wcl") apply
*  to an application instance.  Such resources are fetched by
*  WcInitialize() as resources of the initial top level shell.  These
*  resources are never fetched again during the life of an application.
*
*  WcWidgetCreation() calls WcInitialize() before creating any widgets.
*
*  Application Wide Resources:
******************************************************************************
* WclResFiles			List of files to be loaded into resource db.
* WclInitResFile		ONLY set by command line.
* WclTraceResFiles		Trace loading of files by Wcl to stderr.
* WclErrorDatabaseFile		File with overrides for Wcl warning messages.
* WclWidgetResourceFiles	Files with widget resources not reported by
*				XtGetConstraintResourceList() nor by
*				XtGetResourceList(). ** not yet implemented **
* WclTemplateFiles		List of files with templates.
* WclTraceTemplateDef		Trace definition of templates to stderr.
* WclVerboseWarnings		Some warnings are supressed normally, but this
*				resource causes Wcl to act like ANSI C and
*				give somewhat informative but usually annoying
*				messages.
* WclDynamicLibs		List of full pathnames of libraries which
*				can be named using "-lName" shorthand in
*				WcDynamicAction(), WcDynamicCallback(), and in
*				the new (Wcl 2.02) callback and method syntax.
* WclSlowTightNames		Default: False.  Causes WcFullNameToWidget()
*				to use the same algorithm as used by the Athena
*				string-to-widget converter for widget names
*				starting with `*' - specifically, the reference
*				widget is moved up in the widget tree, parent
*				by parent, with a call to XtNameToWidget()
*				at each parent, until the top of the tree is
*				reached.  This is slower than the default
*				algorithm which tries the reference widget,
*				then the root of the widget tree containing
*				the reference widget.
*/
#define WcNwclResFiles			"wclResFiles"
#define WcNwclInitResFile		"wclInitResFile"
#define WcNwclTraceResFiles		"wclTraceResFiles"
#define WcNwclErrorDatabaseFile		"wclErrorDatabaseFile"
#define WcNwclWidgetResourceFiles	"wclWidgetResourceFiles"
#define WcNwclTemplateFiles		"wclTemplateFiles"
#define WcNwclTraceTemplateDef		"wclTraceTemplateDef"
#define WcNwclVerboseWarnings		"wclVerboseWarnings"
#define WcNwclDynamicLibs		"wclDynamicLibs"
#define WcNwclSlowTightNames		"wclSlowTightNames"

#define WcCWclResFiles			"WclResFiles"
#define WcCWclInitResFile		"WclInitResFile"
#define WcCWclTraceResFiles		"WclTraceResFiles"
#define WcCWclErrorDatabaseFile		"WclErrorDatabaseFile"
#define WcCWclWidgetResourceFiles	"WclWidgetResourceFiles"
#define WcCWclTemplateFiles		"WclTemplateFiles"
#define WcCWclTraceTemplateDef		"WclTraceTemplateDef"
#define WcCWclVerboseWarnings		"WclVerboseWarnings"
#define WcCWclDynamicLibs		"WclDynamicLibs"
#define WcCWclSlowTightNames		"WclSlowTightNames"

/* Widget Creation Time Resources - Before Instance Creation:
******************************************************************************
* These resources are fetched before each widget instance is created by
* the Wcl widget creation recursion.  Since they are fetched before the
* class of the widget is truly known, they apply to an instance (or
* template) name, not to a widget class name.
*
* NOTE: The Wcl 1.x instance creation time resource "wcResFile" is no
* longer supported.  Its use was dangerous.  Change all occurances of
* "wcResFile" to use the new "wclResFiles" 
*/

#define WcNwcPreCreateDumpResources	"wcPreCreateDumpResources"
#define WcNwcTemplate			"wcTemplate"
#define WcNwcTraceTemplate		"wcTraceTemplate"
#define WcNwcPostTemplateDumpResources	"wcPostTemplateDumpResources"
#define WcNwcCreate			"wcCreate"
#define WcNwcClassName			"wcClassName"		/* OBSOLETE */
#define WcNwcClass			"wcClass"		/* OBSOLETE */
#define WcNwcConstructor		"wcConstructor"		/* OBSOLETE */

#define WcCWcDumpResources		"WcDumpResources"
#define WcCWcTemplate			"WcTemplate"
#define WcCWcTraceTemplate		"WcTraceTemplate"
#define WcCWcCreate			"WcCreate"
#define WcCWcClassName			"WcClassName"		/* OBSOLETE */
#define WcCWcClass			"WcClass"		/* OBSOLETE */
#define WcCWcConstructor		"WcConstructor"		/* OBSOLETE */

/* Widget Creation Time Resources - After Instance Creation:
******************************************************************************
* These resources are fetched after each widget instance is created by
* the Wcl widget creation recursion.  The class of the widget is then
* known, so these resources may be specified on a widget class basis.
*
* If a constructor was used to create the instance, and this constructor
* creates additional parents of the instance (e.g., XmCreateMessageDialog
* creates a dialog shell before creating the XmMessageBox), then these
* resources MUST be preceeded by either an "*" or the name of the
* automatically created parent (e.g., childName_popup in the case of
* XmCreateMessageDialog).
*/

#define WcNwcTrace			"wcTrace"
#define WcNwcPostCreateDumpResources	"wcPostCreateDumpResources"
#define WcNwcCallback			"wcCallback"
#define WcNwcPopups			"wcPopups"
#define WcNwcChildren			"wcChildren"
#define WcNwcManaged			"wcManaged"
#define WcNwcAfterPopups		"wcAfterPopups"
#define WcNwcAfterChildren		"wcAfterChildren"
#define WcNwcAfterManageChildren	"wcAfterManageChildren"

#define WcCWcTrace			"WcTrace"
#define WcCWcCallback			"WcCallback"
#define WcCWcPopups			"WcPopups"
#define WcCWcChildren			"WcChildren"
#define WcCWcManaged			"WcManaged"
#define WcCWcAfterPopups		"WcAfterPopups"
#define WcCWcAfterChildren		"WcAfterChildren"
#define WcCWcAfterManageChildren	"WcAfterManageChildren"

/* Resource Types for Wcl Resources
*/
#define WcRDynamicLibs			"DynamicLibs"
#define WcRQuarkRec			"QuarkRec"
#define WcRQuark			"Quark"

/******************************************************************************
** Macros for ANSI and K&R Function Decls
******************************************************************************/

#ifndef NeedFunctionPrototypes
#if defined(FUNCPROTO) || defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus)
#define NeedFunctionPrototypes 1
#include <stdarg.h>
#define Va_start(a,b) va_start(a,b)
#else
#define NeedFunctionPrototypes 0
#include <varargs.h>
#define Va_start(a,b) va_start(a)
#endif /* __STDC__ */
#endif /* NeedFunctionPrototypes */

/* Macro for ANSI or K&R external declarations.  Declare them like this:
**
**      int foo _(( int, MapAg ));
**
***************** DO NOT forget whitespace before the '_' !! *****************
*/
#if NeedFunctionPrototypes
#define _(a) a          /* ANSI results in: int foo ( int, MapAg ); */
#else
#define _(a) ()         /* K&R  results in: int foo ();                       */
#endif

/******************************************************************************
** Wcl Public Functions
******************************************************************************/

#ifdef __cplusplus	/* for C++ V2.0 */
extern "C" {
#endif

/* -- Wcl requires these XtR4 declarations: ********************************/
#ifndef XtSpecificationRelease
#ifdef __STDC__
typedef void*	XtPointer;
#else
typedef char*	XtPointer;
#endif
#ifndef XtName
extern char* XtName _(( Widget ));	/* from ./XtName.c if not in libXt */
#endif
#endif	/* !XtSpecificationRelease *****************************************/

/* -- Function Pointer Typedefs
*/
typedef Widget (*WcWidgetConstructor) _((Widget,char*,ArgList,Cardinal));

/*====================================================================
 * Widget class, constructor, and callback proc registration routines:
 * Registrations are kept separate for each application context.
 * The names are used in resource files as RHS (values).
 * Multiple names can be registered for any widget class, callback, etc.
 */

/* -- Register name of widget class pointer, so name can be used as value of
 *    wcCreate resources.  You might want to register mySpecialWidgetClass as
 *    both "mySpecialWidgetClass" and "MySpecial", the first being the C name
 *    for the widget class pointer, the second being the widget class name
 *    as stored in the widget's class record.
 */
extern void WcRegisterClassPtr _((
		XtAppContext,
		char*,		/* name of class pointer, used for wcCreate */ 
		WidgetClass
));

extern void WcRegisterClassName _(( /* OBSOLETE - use WcRegisterClassPtr */
		XtAppContext,
		char*,		/* name of class pointer, used for wcCreate */
		WidgetClass
));

/* -- Register name of widget constructor function, so name can be used as
 *    value of wcCreate resources.
 */
extern void WcRegisterConstructor _((
		XtAppContext,
		char*,		/* name of constructor, used for wcCreate */
		WcWidgetConstructor
));

/* -- Register the name of XtCallbackProcs so they may be named as callback
 *    resources for widgets.  The registration-time client data is NOT very
 *    useful: it only gets passed to the callback if there is nothing inside
 *    of (the optional) parenthesis following the callback name in the resource
 *    value specification.  Normally, you should use a mapping agent (see
 *    MapAg.h) or the X Context Manager (see XFindContext, XSaveContext) to
 *    obtain arbitrary data related to widgets.  Or, use object-oriented
 *    design and use the Wcl Method concept...
 */
extern void WcRegisterCallback _((
		XtAppContext,
		char*,		/* callback proc name for resource value */
		XtCallbackProc,	
		XtPointer	/* not very useful... I always use NULL */
));

/* -- Provides symmetry with WcRegisterCallback().  It is more efficient
 *    to declare your own XtActionsRec[] and call XtAppAddActions().
 */
extern void WcRegisterAction _((
		XtAppContext,
		char*,		/* action proc name for translations */
		XtActionProc
));

/* -- If a callback proc cannot be found given a name specified in a resource
 *    value, then the "UndefinedCallback" is invoked.  The client data is the
 *    name and arguments (more-or-less) as specified in the resource value.
 *    You can provide your own callback which will then be invoked instead of
 *    the default "UndefinedCallback".  The return value is the address of the
 *    previous "UndefinedCallback".
 */
extern XtCallbackProc WcSetUndefinedCallback	 _(( XtCallbackProc ));

/*=============================================================================
 * Method Support:  Callback resource value specifications can now name methods
 * instead of only simple callbacks.  A method specification in a callback
 * resource value looks like this:
 *
 *	<class_name>::<method_name>(<opt_args>)
 *
 * The callback method gets a pointer to a special struct as the client data.
 * The struct has three elements: a pointer to the "object" of the appropriate
 * class, an arbitrary pointer which is provided at registration time, and the
 * characters inside of parenthesis in the resource value (leading and trailing
 * whitespace stripped, at least a pointer to a null character, never a null
 * pointer).
 *
 * Note that this makes the registration-time client data much more useful than
 * the original Wcl callback registration mechanisms: the original callback
 * mechanism only provides the registration-time client data if there is nothing
 * between parens in the resource file.  The new method mechanism ALWAYS
 * provides the registration-time client data.
 *
 * Class names CANNOT begin with the two characters "-l" in order to avoid
 * ambiguities with the Dynamic Linking Support (see below).
 *
 * Wcl uses late binding to invoke methods.  At invocation time, Wcl resolves
 * the method address, and it attempts to find an object of the appropriate
 * class.  The run-time overhead is kept low: about 20 lines of C are executed
 * for typical method invocations.  
 *
 * Wcl provides a default mechanism for finding object which is baed on the
 * following observations:  Often, an object in the application is mapped 1:1
 * with some shell or manager widget, and the children of that shell or manager
 * widget provide ways to display or set attributes on the object, and invoke
 * methods on the object.  Imagine a dialog box for a text editor which is used
 * to display and manipulate paragraph properties.  The application changes the
 * "Paragraph" object related to the dialog as the insertion point moves from
 * paragraph to paragraph.  Pressing a button labeled "Right Justify" may
 * invoke the RightJustify method of the current paragraph.  The resource
 * specification would look like this:
 *
 *	*parProp*rightJustify.callback:	Paragraph::RightJustify()
 *
 * The default object resolution mechanism uses WcAttachThisToWidget() and
 * WcDetachThisFromWidget() to allow the application to set and change the
 * objects related to widgets in the interface.
 *
 * An application can get the address of the default object resolution function
 * and provide its own which replaces or envelopes the default resolution logic.
 * Use WcSetFindObjectFunc() to get the old resolution function and provide a
 * replacement which will be called to get the object pointer.
 *
 * NOTE: it is very possible that no object of the appropriate type is found
 * by an object resolution procedure.  Wcl can optionally issue a warning
 * message if this occurs, BUT THE METHOD IS INVOKED ANYWAY!  This means that
 * methods MUST detect NULL object pointers and handle them reasonably.
 *
 * Some methods do not need object pointers: object constructors are a common
 * and useful example.
 */
extern void WcRegisterMethod _((
		XtAppContext,
		char*,		/* object class name - CANNOT begin with "-l" */
		char*,		/* method name */
		XtCallbackProc,	/* method is still an XtCallbackProc */
		XtPointer	/* registered client data - can be useful */
));

/* -- A pointer to this struct is passed as client data to methods.  This struct
 *    may become longer in future releases.  The object pointer my be NULL!  The
 *    method must detect NULL object pointers.
 */
typedef struct _WcMethodDataRec {

    XtPointer	object;		/* pointer to application obj or NULL */
    XtPointer	closure;	/* client data registered with method name */
    char*	args;		/* from between parens in resource value */

} WcMethodDataRec, *WcMethodData;

/* -- Set the function used to find an object of a particular class from
 *    a widget invoking a callback.  The default function provided by Wcl
 *    searches up the widget tree, looking for objects attached to widgets.
 *    Some applications may want to use different approaches to finding an
 *    object, such as using a server, loading objects from files, collections,
 *    MappingAgents, etc...
 *    The previous function is returned.
 */
typedef XtPointer (*WcFindObjectFunc) _((
		Widget,		/* widget invoking the callback */
		XrmQuark	/* quarkified case sensitive class name */
));

extern WcFindObjectFunc WcSetFindObjectFunc _((
		WcFindObjectFunc	/* application provided function ptr */
));

/* -- Set the object pointer related to a widget.  These may become useless
 *    if you use WcSetFindObjectFunc...
 */
extern void WcAttachThisToWidget _((
		XtPointer,	/* "this" pointer, pointer to application obj */
		char*,		/* object class name - CANNOT begin with "-l" */
		Widget		/* object will be `attached' to the widget */
));

/* -- Un-set the object pointer related to a widget.
 *    The "this" pointer is unused: actually, any object of the indicated
 *    class is detached from the widget.
 */
extern void WcDetachThisFromWidget _((
		XtPointer,	/* "this" pointer, pointer to application obj */
		char*,		/* object class name - CANNOT begin with "-l" */
		Widget		/* any object of "class" will be detached */
));

/*============================================================================
 * Dynamic Linking Support:  Callback resource value specifications can now
 * name dynamic libraries which can be used to resolve callback procedure
 * addresses.  Both "old-fashioned" Wcl style callbacks and "new-fangled"
 * Wcl style methods can be bound using this mechanism.  The resolution of
 * addresses is done once, before the first invocation of a callback or a
 * method.  Callback resource specification which use dynamic libraries look
 * like this:
 *
 *	-l<library_abbreviation>::<callback_name>(<opt_args>)
 * or	-l<library_abbreviation>::<class_name>::<method_name>(<opt_args>)
 *
 * Note that the initial "-l" of library specifications eliminated any
 * ambiguity between library names and class names used for methods.
 *
 * The library abbreviation is the same as that used by the "ld" command:
 * the library /usr/X11R5/lib/libXop.so.2.2 can be named by "-lXop".
 *
 * Libraries must be registered with Wcl before they are needed (before the
 * first callback invokation which names the library).  Libraries may be
 * registered using the function below, and by using the application-wide
 * resource "wclDynamicLibs" which is fetched and evaluated during Wcl
 * initialization.  Multiple libraries can be registered at once: each full
 * pathname is separated by whitespace and/or a comma.
 */
extern void WcRegisterDynamicLibs _((
		XtAppContext,
		char*		/* full path names of dynamic libraries */
));

/*============================================================================
 * Initialization and initial widget tree creation routines:  WcInitialize()
 * must be called before any widgets are created by Wcl.  Many applications
 * simply use the function WcWidgetCreation() to initialize Wcl and to create
 * the initial non-empty widget tree.  WcWidgetCreation complains and returns
 * 1 if no popups or children of the application shell could be created.  If
 * your application does not have an initial non-empty widget tree, then you
 * should invoke WcInitialize() and then dynamically create the widget trees
 * as needed.
 */
extern int  WcWidgetCreation _((	/* 1 if failed, 0 if OK */
		Widget 			/* appShell from XtInitialize */
));

extern void WcInitialize _((		/* called by WcWidgetCreation */
		Widget			/* appShell from XtInitialize */
));

/*============================================================================
 * Create Widgets Dynamically:  Each of these functions can create an entire
 * widget tree starting with the named child.  Functions which create a single
 * direct descendent child return the widget id of the new child.  Functions
 * which may create multiple direct descendents return void.
 *
 * Child names are always single elements (like "newChild", not like
 * "somechild*aDistantDescendant").  Names of children are passed as a single
 * null terminated string, with whitespace and/or a comma separating each
 * child name.
 *
 * The type of the child as well as other Wcl creation-time resources of the
 * child are fetched and evaluated.  Therefore, wcPopups and wcChildren
 * resources of the child widget can cause arbitrarily complex widget trees to
 * be created.  However, NONE of the parent's resources are re-evaluated.
 * Specifically, this means the Wcl creation-time callbacks of the parent are
 * NOT invoked.
 * WcCreateChild():		Child is NOT managed.
 * WcCreateChildFromTemplate():	Child is NOT managed.
 * WcCreateNamedPopups():	Children are NOT popped up.
 * WcCreatePopup():		Child is NOT popped up.
 * WcCreatePopupFromTemplate():	Child is NOT popped up.
 * WcCreateRoot():		Shell is realized.
 */ 

/* -- Each child of the parent may be managed as controlled by child's
 *    wcManaged resource.
 */
extern void WcCreateNamedChildren _((
		Widget,		/* parent widget */
		char*		/* names of children to be created */
));

/* -- Child is NOT managed.
*/
extern Widget WcCreateChild _((
		Widget,		/* parent widget */
		char*		/* name of child to be created */
));

/* -- Child is NOT managed.  Template applied before creation.
*/
extern Widget WcCreateChildFromTemplate _((
		Widget,		/* parent widget */
		char*,		/* name of child to be created */
		char*		/* template name */
));

/* -- Children are NOT popped up.
*/
extern void WcCreateNamedPopups _((
		Widget,		/* parent widget */
		char*		/* names of popup children to be created */
));

/* -- Child is NOT popped up.
*/
extern Widget WcCreatePopup _((
		Widget,		/* parent widget */
		char*		/* name of popup child to be created */
));

/* -- Child is NOT popped up.  Template applied before creation.
*/
extern Widget WcCreatePopupFromTemplate _((
		Widget,		/* parent widget */
		char*,		/* name of popup child to be created */
		char*		/* template name */
));

/* -- Creates an application shell.  Therefore, this MUST be created
 *    on a new display connection to avoid problems.  The shell is realized,
 *    which generally causes it to appear.
 */
extern Widget WcCreateRoot _((
		Display*,	/* A different connection than other roots! */
		char*		/* name of shell to be created */
));

/*===========================================================================
 * Resource Database Dumping Capabilities - require X11R5
 * These allow you to dump portions of the resource database into stdio files.
 */

/* -- Dump resources which apply to a widget before it is created, note that
 *    we do not know the class name yet, so no resources which are terminally
 *    named by a widget class name (like *XpTable.defaultOptions) will be
 *    found.
 */
extern void WcPreCreateDumpResources _((
		Widget,		/* parent widget */
		char*,		/* name of (usually) not-yet-created child */
		FILE*		/* stderr, stdout, or opened with fopen() */
));

/* -- Dump all resources which apply to a widget after the widget has been
 *    created.  The class is now known, the final full path of the child is
 *    known, and templates have been applied.  The resources shown will be
 *    the same which the widget sees and responds to in its core.Initialize()
 *    method.
 */
extern void WcPostCreateDumpResources _((
		Widget,		/* the widget */
		FILE*		/* stderr, stdout, or opened with fopen() */
));

/*===========================================================================
 * Widget name routines
 */

/* -- Find named widget starting from reference widget.
 *    WcFullNameToWidget examines the first few characters of the `name'
 *    argument in order to determine where the search will begin.
 *
 *    The `name' can begin with one or more of the relative prefix operators
 *    which are evaluated from left-to-right:
 *
 *	this	must be first, means "the reference widget";
 *	^	means "parent";
 *	~	means "closest shell ancestor";
 *	.	ignored, can be used stylistically.
 *
 *    Following the relative pre-fix operators can be a normal widget path
 *    name which is simply passed to XtNameToWidget().  Therefore, the rest
 *    of the path can include "*", widget instance names and widget class
 *    names, but of course the last component must be an instance name.
 *
 *    If the name begins with `*' as the first character, then the root
 *    widget starting from the reference widget is used as the reference
 *    widget.  This is very different from XtNameToWidget!  Use "this*foo"
 *    if you want to get the same widget which XtNameToWidget(this,"*foo")
 *    would find.
 */
extern Widget WcFullNameToWidget _((
		Widget,		/* reference widget */
		char*		/* relative path name to another widget */
));

/* -- This is a bug-fixed version of XtNameToWidget().  Pre-X11R4 and early
 *    X11R4 versions of XtNameToWidget dump core when a Gadget is encountered.
 *    This version is the late Rr and R5 version which can deal properly with
 *    Gadgets.
 */
extern Widget WcChildNameToWidget _((
		Widget,		/* starting widget */
		char*		/* path to child widget */
));

/* -- Returns the full path name of a widget starting from its root
 *    widget.  The string is XtAlloc'd, so use XtFree() when done.
 */
extern char* WcWidgetToFullName _(( Widget ));

/*  -- Print the widget tree starting from this widget
*/
extern void WcPrintTree  _(( Widget ));

/*============================================================================
 * Parsing and various string utilities.
 */

/* -- Obtain application class from the argv passed to main()
 *    The caller must free return values with XtFree().
 */
extern char* WcAppClass _((
		int,		/* argc passed to main() */
		char**		/* argv passed to main() */
));

/* -- Convert an application name into an application class name using
 *    the X convention (capitalizes first letter, or first two if first
 *    is already capitalized)
 */
extern char* WcAppNameToAppClass _((
		char*		/* application name (usually argv[0]) */
));

/* -- Returns the character pointer argument advanced past whitespace,
 *    and '\0' at end of string.
 */
extern char* WcSkipWhitespace		_(( char* ));

/* -- Returns the character pointer argument advanced past whitespace and
 *    optional comma.  Commas can be used to indicate null fields.  If a
 *    field is null, the comma is returned.  Returns '\0' at end of string.
 */
extern char* WcSkipWhitespace_Comma	_(( char* ));	/* optional comma   */

/* -- WcCleanName is ugly, but effective.  The first argument is the string to
 *    find a name surrounded by whitespace.  The second is a buffer provided
 *    by the caller into which WcCleanName builds a null terminated string.
 *    The return value points at the next character WcCleanName should process
 *    if continuing to get names from the same initial string.
 */
extern char* WcCleanName _((
		char*,		/* string with a bunch of words */
		char*		/* pre-allocd char array to be filled in */
));

/*  -- get quark of lower case copy of string - the incoming arg is NOT changed
*/
extern XrmQuark WcStringToQuark _((
			char*	/* copied, flattened, then quarkified */
));

extern XrmQuark WcSubStringToQuark _((
			char*,	/* ptr to starting character in an array */
			char*	/* ptr to ending character in same array */
));

/*  -- get case-sensitive quark of sub-string (without flattening case)
*/
extern XrmQuark WcCSSubStringToQuark _((
			char*,	/* ptr to starting character in an array */
			char*	/* ptr to ending character in same array */
));

/*  -- Various safe wrappers around string functions
*/
extern char* WcStrStr  _(( char*, char* ));	/* find str1 in str2	*/
extern char* WcStrCpy  _(( char*, char* ));		/* safe strcpy	*/
extern char* WcStrCat  _(( char*, char* ));		/* safe strcat	*/
extern int   WcStrCmp  _(( char*, char* ));		/* safe strcmp	*/
extern int   WcStrCmpN _(( char*, char*, int ));	/* safe strncmp	*/

#define WcNonNull(str) (int)((str) && *(str))
#define WcNull(str)  (int)(!((str) && *(str)))
#define WcStrLen(str) (int)(((str) && *(str))?strlen(str):0)
#define WcStrEq(s1,s2) (int)(!WcStrCmp((s1),(s2)))
#define WcStrEqN(s1,s2,n) (int)(!WcStrCmpN((s1),(s2),(n)))

/* -- This is used to determine the number of `%s' replacement characters in
 *    a string which is intended to be passed to printf() as a format
 *    specification.  It returns 0 if something other than strings are expected
 *    by the format specification.  It also returns 0 if a string format
 *    specification contains a '*' which means the width or precision is
 *    parametric.  Therefore, a positive return value means printf() can be
 *    safely called with the returned number of strings as arguments following
 *    the format.
 */
extern int   WcPrintfFormatStrings _(( char* ));

/*============================================================================
 * Set resource values on widgets
 * These procedures make it easier to use strings as the base protocol for 
 * communicating with widgets.  This means you can use the same strings to
 * dynamically change widget resources as you do to initialize them in the
 * resource files.  The widget provided converters are used to convert the
 * strings into the types required by the widget.
 */

/*  -- Set resource values on widgets.  The string is of the format:
 *	<set_string>	::=	<res_spec_list>
 *			|	<res_spec>
 *
 *	<res_spec_list>	::=	( <res_spec> )
 *			|	<res_spec_list> ( <res_spec> )
 *
 *	<res_spec>	::=	<targetName>.<resName>: <resValue>
 *			|	<targetName>.<resName>(<resType>): <resValue>
 *
 *	<targetName>	::=	<empty>
 *			|	this
 *			|	<relative_opt><widget_path>
 *
 * If there are multiple resource specs, then each must be enclosed
 * in parens.
 *
 * In other words, a resource spec looks just like a line in a resource file,
 * except that it may include an optional resource type specification in
 * parens, and the name of the targetWidget may be 'this' or a relative path
 * as well as the standard wildcarded paths.  An empty targetName is the
 * same as 'this'.
 *
 * The special resource value of "this" means "this widget."  Typically,
 * using "this" as the resource value is used to set the "XmNdefaultButton"
 * resource on a XmbulletinBoard, "menuBar", "workArea", etc on XmMainWindows,
 * the subMenuId resource on menuBar cascade buttons, and so on.
 *
 * The targetName must resolve to a specific widget: therefore the last
 * component of the targetName must be the name of a Widget instance, and
 * the targetName MUST be followed by a '.' and NOT a '*'.
 *
 * Caution: only use resource_type when you REALLY must, as this is not safe.
 * With sub-resources you will need it.
 */
extern void WcSetValue _((
		Widget,		/* refernce widget, not necessarly altered! */
		char*		/* <set_string> as defined above */
));

/* -- Set a single resource on a single widget
*/
extern void WcSetValueFromString _((
		Widget,		/* widget to change */
		char*,		/* name of resource (like XtNforground) */
		char*		/* value of resource (like "red") */
));

/* -- Set a single resource on a single widget.  Use only if the widget
 *    does not include the resource in its class resources rec, nor in its
 *    constraint resources.  With sub-resources you will need it.
 *    You can try to get the type with WcGetResourceType(), and if you get
 *    NULL, then the widget is mis-implemented (or its a sub-resource) and
 *    you will need to somehow figure out the appropriate type, like XtRWidget.
 */
extern void WcSetValueFromStringAndType _((
		Widget,		/* widget to change */
		char*,		/* resource name (like "fontList") */
		char*,		/* resource value (like "*times*--14-*") */
		char*		/* resource type (like "FontList") */
));

/* -- Determine the type of the named resource.  Checks both normal widget
 *    resources and constraint resources which may apply to an instance.
 *    XtFree the return value.  Returns NULL if the widget pretends it does
 *    not have this resource (which is generally a widget bug, and usually
 *    implies the resource is a sub-resource).
 */
extern char* WcGetResourceType _((
		Widget,		/* Check resource types for this instance */
		char*		/* return type of this resource, or NULL */
));

/* -- Set translations on a widget based on a translation string which includes
 *    #augment, #replace, or #override directives (default to #replace).
 */
extern void WcSetTranslations _((
		Widget,		/* set translations on this widget */
		char*		/* string describing translations */
));

/* -- Load a resource file (file is loaded once only per display), returns
 *    TRUE if loaded.  Note that in general you must be careful when using
 *    this function.  If you load resources which override existing values,
 *    the overriden values are free'd.  If someone is trying to use resource
 *    values in-place (which they should not, but...) then you may eventually
 *    get a core dump.  Or maybe not.  Wcl uses this function, but only during
 *    Wcl initialization.  At this time, there are few users of the database,
 *    and those users (applicationShellWidgetClass, Wcl, Xt) do not use
 *    in-place resource values, only copies.  Be careful!
 */
extern int WcLoadResourceFile _((
		Widget,		/* for Display* and  XtAppContext */
		char*		/* name of file to load */
));

/*============================================================================
 * Callback And Action Support
 * Applications can export their methods to the widgets in the form of
 * XtCallbackProcs or XtActionProcs.  Wcl and the XtTranslationManager provide
 * the argument(s) between parentheses as character strings to the methods.
 * Callback procs receive the string between parenthesis as a single string,
 * actions receive the same string broken up into words.  If the method needs
 * to parse the arguments, one or the other form may be more convenient.
 *
 * For example, WcUnmanage() requires no real parsing if implemented as an
 * XtActionProc as it only uses the arguments as individual words.  However,
 * WcSetValue() must do more sophisticated parsing, and so it is easier to
 * implement as a callback.  Wcl implements each of the 30 odd conveneince
 * procedures as either callbacks or actions (choosing the form which is most
 * convenient for implementation) and then it simply invokes the convenient
 * form from the less convenient form: WcManageCB() invokes WcManageACT(),
 * whereas WcSetValuesACT() invokes WcSetValuesCB().
 */

/* -- Invoke an XtActionProc easily from an XtCallbackProc
*/
extern void WcInvokeAction _((
		XtActionProc,	/* invoke this action proc */
		Widget,		/* the widget the callback proc received */
		char*		/* the client data string Wcl gave the CB */
));

extern void WcInvokeNamedAction _((
		char*,		/* Name of action proc to invoke (needs R4) */
		Widget,		/* the widget the callback proc received */
		char*		/* the client data string Wcl gave the CB */
));

/* -- Invoke an XtCallbackProc easily from an XtActionProc
*/
extern void WcInvokeCallback _((
		XtCallbackProc,	/* invoke this callback proc */
		Widget,		/* the widget the action proc received */
		char**,		/* the "params" recvd from XtTranslationMgr */
		Cardinal*	/* "num_params" recvd from XtTranslationMgr */
));

/*============================================================================
 * Convenience Callbacks and Actions provided by Wcl
 *
 * In all cases, unless explicitly stated, the names of widgets taken
 * as arguments to the following callbacks and actions are resolved using
 * WcFullNameToWidget(), and therefore the names can always be relative
 * widget path names.  The "reference widget" is the widget which invoked
 * the callback or action proc.
 *
 * Also, each of these callbacks and actions are registered with Wcl with
 * the "CB" or "ACT" suffixes and without.  Therefore, in resource files you
 * can name WcManageCB() with either WcManage or WcManageCB, and WcManageACT()
 * with either WcManage or WcManageACT.
 */

/* -- WcManage( widgetName [, widgetName] ... )
 *    Manages each of the named widgets.  Uses XtManageChildren as much
 *    as possible.
 */
extern void WcManageCB  _(( Widget, XtPointer, XtPointer ));
extern void WcManageACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcUnmanage( widgetName [, widgetName] ... )
 *    Unmanages each of the named widgets.
 */
extern void WcUnmanageCB  _(( Widget, XtPointer, XtPointer ));
extern void WcUnmanageACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcManageChildren( parentWidgetName, child [, child] ... )
 *    The parentWidgetName is resolved using WcFullNameToWidget().  However,
 *    Each of the children must be single component names of direct descendents
 *    of the parent widget.  No wildcards or relative names are allowed for the
 *    children names.
 */
extern void WcManageChildrenCB  _(( Widget, XtPointer, XtPointer ));
extern void WcManageChildrenACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcUnmanageChildren( parentWidgetName, child [, child] ... )
 *    See WcManageChildren for comments about the arguments.
 */
extern void WcUnmanageChildrenCB  _(( Widget, XtPointer, XtPointer ));
extern void WcUnmanageChildrenACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcDestroy( widgetName [, widgetName] ... )
 *    Calls XtDestroyWidget with each of the named widgets.  The 2-phase
 *    destroy mechanism makes this safe.
 */
extern void WcDestroyCB  _(( Widget, XtPointer, XtPointer ));
extern void WcDestroyACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcSetSensitive( widgetName [, widgetName] ... )
 *    Makes each named widget sensitive by calling XtSetSensitive.  Note that
 *    gadgets cannot be made insensitive: a warning message is generated.
 */
extern void WcSetSensitiveCB  _(( Widget, XtPointer, XtPointer ));
extern void WcSetSensitiveACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcSetInsensitive( widgetName [, widgetName] ... )
 *    See WcSetSensitive for comments about the arguments.
 */
extern void WcSetInsensitiveCB  _(( Widget, XtPointer, XtPointer ));
extern void WcSetInsensitiveACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcPopup( shellWidgetName [, shellWidgetName] ... )
 *    Named widgets must be shells.  This invokes XtPopup(), passing XtGrabNone
 */
extern void WcPopupCB  _(( Widget, XtPointer, XtPointer ));
extern void WcPopupACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcPopupGrab( shellWidgetName [, shellWidgetName] ... )
 *    Requires shell widgets.  This invokes XtPopup(), passing XtGrabExclusive
 */
extern void WcPopupGrabCB  _(( Widget, XtPointer, XtPointer ));
extern void WcPopupGrabACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcPopdown( shellWidgetName [, shellWidgetName] ... )
 *    The named widgets must be shell widgets.  Each shell is popped-down
 *    via a call to XtPopdown.  This releases any grabs by the shells.
 */
extern void WcPopdownCB  _(( Widget, XtPointer, XtPointer ));
extern void WcPopdownACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcMap( widgetName [, widgetName] ... )
 *    Causes each of the named widgets to be mapped via XtMapWidget()
 */
extern void WcMapCB  _(( Widget, XtPointer, XtPointer ));
extern void WcMapACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcUnmap( widgetName [, widgetName] ... )
 *    Causes each of the named widgets to be un-mapped via XtUnmapWidget()
 */
extern void WcUnmapCB  _(( Widget, XtPointer, XtPointer ));
extern void WcUnmapACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcInstallAccelerators( dest, src [, src] ... )
 *	dest  - (root of tree) widget that can activate accelerators,
 *	src   - widget which provides functionality,
 *		and defines the accel translation (accelerators resource).
 */
extern void WcInstallAcceleratorsCB  _(( Widget, XtPointer, XtPointer ));
extern void WcInstallAcceleratorsACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcInstallAllAccelerators( dest, src )
 *	dest  - (root of tree) widget that can activate accelerators,
 *	src   - root of widget tree, each widget can provide functionality,
 *		and can define an accel translation (accelerators resource).
 */
extern void WcInstallAllAcceleratorsCB  _((Widget, XtPointer, XtPointer ));
extern void WcInstallAllAcceleratorsACT _((Widget, XEvent*, char**, Cardinal*));

/* -- WcCreateRoot( shell [on: display] [shell [on: display]] ... )
 *    Creates a new application shell.  Although the display name is optional,
 *    it is effectively mandatory because Xt does not fully define behavior
 *    when there are multiple applicationShells on the same display connection.
 *    The name of the shell must be a single component name: it will have no
 *    parent.
 */
extern void WcCreateRootCB  _(( Widget, XtPointer, XtPointer ));
extern void WcCreateRootACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcSpawn( <cmd_arguments> )
 *    Forks and execvp a new process.  The <cmd_arguments> are passed to the
 *    main() of the new process as argc and argv.  The command line is NOT
 *    a shell command line.  See WcSystem().
 */
extern void WcSpawnCB  _(( Widget, XtPointer, XtPointer ));
extern void WcSpawnACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcLoadResourceFile( file [, file] )
 *    Be careful when you use this!!   See the comments for the function
 *    of the same name.
 */
extern void WcLoadResourceFileCB  _(( Widget, XtPointer, XtPointer ));
extern void WcLoadResourceFileACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcPrintTree( widgetName [, widgetName] )
 *    Prints out widget tree starting with named widget(s).
 */
extern void WcPrintTreeCB  _(( Widget, XtPointer, XtPointer ));       
extern void WcPrintTreeACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcDumpResources( widgetName [, widgetName] )
 *    Print out resources under a widget instance.
 */
extern void WcDumpResourcesCB  _(( Widget, XtPointer, XtPointer ));
extern void WcDumpResourcesACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcExit( [exitVal] )
 *    Exit the application, returning the integer equivalent of exitVal.
 */
extern void WcExitCB  _(( Widget, XtPointer, XtPointer ));
extern void WcExitACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcCreateChildren( parentWidgetName, child [,child] ... )
 *    The parent name can be a relative name, but each of the children are
 *    named with a single component name.  This procedure is equivalent to
 *    providing a wcChildren resource on the parent widget, except that
 *    the parent's creation-time resources are not evaluated: i.e., the
 *    creation-time callbacks of the parent are NOT invoked.
 */
extern void WcCreateChildrenCB  _(( Widget, XtPointer, XtPointer ));
extern void WcCreateChildrenACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcCreatePopups( parentWidgetName, child [,child] ... )
 *    The parent name can be a relative name, but each of the children are
 *    named with a single component name.  This procedure is equivalent to
 *    providing a wcPopup resource on the parent widget, except that
 *    the parent's creation-time resources are not evaluated: i.e., the
 *    creation-time callbacks of the parent are NOT invoked.
 */
extern void WcCreatePopupsCB  _(( Widget, XtPointer, XtPointer ));
extern void WcCreatePopupsACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcPositionTransient()
 *    Position a TransientShell in middle of parent widget.  This callback
 *    is useful as a popupCallback or as an action triggered by the <MapNotify>
 *    event.
 */
extern void WcPositionTransientCB  _(( Widget, XtPointer, XtPointer ));
extern void WcPositionTransientACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcSetValue( <set_string> )
 *    This callback and action are simple wrappers around WcSetValue().
 *    See the comments above for the sytax of the <set_string> argument.
 */
extern void WcSetValueCB  _(( Widget, XtPointer, XtPointer ));
extern void WcSetValueACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcTrace( annotation )
 *    Prints to stderr the wiget pathname of the invoking wiget, and an
 *    optional annotation.
 */
extern void WcTraceCB  _(( Widget, XtPointer, XtPointer ));
extern void WcTraceACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcSystem( shellCmdString )
 *    Invokes system(), passing the shellCmdString.  Note this does NOT
 *    fork, so it will block unless you append a `&' at the end of the
 *    shellCmdString.
 */
extern void WcSystemCB  _(( Widget, XtPointer, XtPointer ));
extern void WcSystemACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcAddCallbacks( widgetName callbackName CbProc(args) [CbProc(args) ...] )
 *    This is the socially acceptable way to add callback procedures to
 *    callback resources of widgets.  You generally should not use WcSetValue().
 *    You must specify the callback list name (like popupCallback) as well
 *    as one or more callback procedure names.  The args for each callback
 *    procedure are optional.  The syntax for the callback names are discussed
 *    under "Method Support" and "Dynamic Library Support" above.
 */
extern void WcAddCallbacksCB  _(( Widget, XtPointer, XtPointer ));
extern void WcAddCallbacksACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcRemoveCallbacks( widName callbackName CbProc(args) [CbProc(args) ...] )
 *    This is the socially acceptable way to remove callback procedures from
 *    callback resources of widgets.  
 *    You must specify the callback list name (like popupCallback) as well
 *    as one or more callback procedure names.  The args for each callback
 *    procedure must be the same (except for leading and trailing whitespace).
 *    I.e., you must use the same internal whitespace, commas, and
 *    capitailization as when you set the callback resource value.
 */
extern void WcRemoveCallbacksCB  _(( Widget, XtPointer, XtPointer ));
extern void WcRemoveCallbacksACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcOnceOnly( callbackName CbProc( args ) [CbProc( args ) ...] )
 *    Note that we must have the callback list name!
 *    This allows the list of callbacks to each be invoked exactly once, the
 *    first time the callbacks on the callback list is traversed by the widget.
 *    The callbacks are invoked in left to right order.  Note that this is
 *    NOT true for callbacks in other contexts!  Xt does not guarantee any
 *    callback procedure invocation order.  However, since WcOnceOnlyCB is
 *    the callback invoked from the callback list from Xt, and each CbProc()
 *    is then invoked by WcOnceOnly(), the order can be guaranteed.
 */
extern void WcOnceOnlyCB  _(( Widget, XtPointer, XtPointer ));
extern void WcOnceOnlyACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcTranslations( widgetName translationString )
 *    Augment or override translations on the named widget.
 */
extern void WcTranslationsCB  _(( Widget, XtPointer, XtPointer ));
extern void WcTranslationsACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcDynamicCallback(  sharedLibrary nameOfXtCallbackProc([optArgs]) )
 *    This is really made obsolete by the new (Wcl 2.02) callback naming syntax.
 *    The sharedLibrary argument must be a full pathname, a path starting from
 *    ~, or a library abbreviation for libraries registered with the
 *    WclDynamicLibs resource or the WcRegisterDynamicLibs() function.  See the
 *    discussion under "Dynamic Library Support" above.
 */
extern void WcDynamicCallbackCB  _(( Widget, XtPointer, XtPointer ));
extern void WcDynamicCallbackACT _(( Widget, XEvent*, char**, Cardinal* ));

/* -- WcDynamicAction( sharedLibrary nameOfXtActionProc([optArgs]) )
 *    This is not yet obsolete.  Doing late binding of actions is not so
 *    simple...
 *    ~, or a library abbreviation for libraries registered with the
 *    WclDynamicLibs resource or the WcRegisterDynamicLibs() function.  See the
 *    discussion under "Dynamic Library Support" above.
 */
extern void WcDynamicActionCB  _(( Widget, XtPointer, XtPointer ));
extern void WcDynamicActionACT _(( Widget, XEvent*, char**, Cardinal* ));


/* -- For backward compatibility, these functions do nothing at all
*/
extern void WcAllowDuplicateRegistration	 _(( int ));
extern void WcAllowDuplicateClassPtrReg 	 _(( int ));
extern void WcAllowDuplicateClassNameReg	 _(( int ));
extern void WcAllowDuplicateConstructorReg	 _(( int ));
extern void WcAllowDuplicateCallbackReg		 _(( int ));


#ifdef __cplusplus	/* for C++ V2.0 */
}
#endif

#endif /* _WcCreate_h */
