#include <X11/Wc/COPY>

/*
* SCCS_data: @(#) WcTemplate.c 1.6 92/10/12 08:29:40
*
* Widget Creation Library - WcTemplate.c
*
* The capabilities provided by the functions in this file absolutely
* require X11R5 Xlib and Xt.  If XtSpecificationRelease is less than
* 5, then these functions are no-ops.
*
* This module contains the functions used to apply templates to widgets
* before they are created.  The templates can be considered "constructors"
* which are specified in resource files.  They can also be used to apply
* any sort of resource settings to portions of a widget interface, not
* simply to control the creation of widgets.
*
*******************************************************************************
* Create Widgets using Templates obtained from XrmDatabase
*******************************************************************************

    Templates are specified in resource files very much like any widget
    resources.  Each template must be in a separate resource files: the
    name of the file is the name of the template.

    Template resources are applied to some widget "node" - that is, if
    a widget named "App.this.is.the.widget" has a template appiled to it,
    all of the widget resources in the template file have the pathname
    of the widget pre-pended.  An example can best illustrate this.  First,
    a template file, lets say it is named "PT_ColumnLable" looks like this:

	.wcCreate:              XmpTable
	.wcChildren:            filler label
	.layout:                filler 0 0 ; label 0 1 hH
	.borderWidth:           0
	*WcCreate:              XmLabel
	*filler.labelString:

    Here is how this template is used:

	App.wclTemplateFiles:	PT_ColumnLabel
	...
	*IIIt.wcTemplate:               PT_ColumnLabel
	*IIIt.label.labelString:        IIIb

    The full path name of the widget "*IIIt" gets pre-pended to each of the
    resource specifications of the template.  If the full path name of the
    widget is "App.main.table.IIIt" then each resource is pre-pended with
    "*main*table*IIIt" before being merged into the resource database used
    by Xt and Wcl during widget creation.

    The templates available to an application are named in a Wcl library
    resource value "wclTemplateFiles".  Each template file is loaded during
    Wcl initialization before any widgets are created by Wcl.  Each file is
    loaded into a completely separate resource database, separate from each
    other template resource database and separate from the one used by Xt
    to provide resources to widgets during creation.

    As widgets are being created, WcCreate() applys templates during the
    pre-creation resource fetch.  This occurs before wcCreate, wcClass,
    wcClassName, and wcConstructor are checked.  Template resources are
    merged into the database used by Xt when WcCreate (actually,
    WcxGetPreRes()) invokes WcApplyTemplate().

    XrmEnumerateDatabase() pulls out each resource specification in the
    template database, and passes it to WcxCopyEntry(), which pre-pends the
    loose bound path name of the widget about to be created, and copies the
    resource values into a new, very small resource database.

    The new resource database is then merged with the existing (primary)
    resource database on the widget's screen by calling 

	XrmCombineDatabase( temp, existing, False)

    The merge augments, does not overwrite the existing resource database.
    This allows all template resources to be tailored on an instance by
    instance basis.  Since the full path loose bindings for resources is in
    fact pretty darn specific, it may seem awkward to override template
    resources.  However, simply put a `?' in front of any resource
    specification and it will be certain to override the template
    specification.

-- Implementation Notes:

    This mechanism is rather different from the template mechanism
    implemented by Wcl version 1.06.  This implementation is compact and
    fast with very large resource databases.

    The 1.06 implementation had two performance problems:  First, the
    original mechanism caused every single value in the entire resource
    database to be examined by the CopyEntry() procedure, which was very
    slow on large interfaces, and large interfaces are when templates are
    most useful. Second, the original scheme allowed strings to be
    examined, and %p and %n to be replaces by the parent name and child
    name respectively.  It seems that this was not necessary: relative
    naming using ^ and . can do the same thing.  Since every single
    value in the template database IS a string (remember, it was loaded
    from a file), supporting this feature caused every single resource
    value to be examined byte-by-byte.  This was slow.

    This scheme uses a totally separate database for specifying templates,
    which makes is easier to use wildcarding the templates without
    hammering the overall resource database, thereby making templates
    more "portable" between applications.

PROBLEM:
    It is easy to make templates which do not work as desired!  Loose
    and tight binding really do not cause resources in templates to be
    differentiated.  

    Any duplicate path ignoring bindings (dots or stars) have indeterminate
    precedence in templates.  In other words, .wcCreate does not
    necessarily override *wcCreate.  You can (and must!) use resource
    classes instead of resource names to get loose bindings in templates.

    In WcApplyTemplate, XrmEnumerateDatabase is used to pull each of the
    template resource values from the individual template database.
    XrmEnumerateDatabase does not guarantee any order of enumeration.
    WcApplyTemplate puts the template resource values into a new temporary
    database using the only mechanism provided by Xrm: XrmQPutResource.
    Unfortunately, XrmQPutResource does NOT check the bindings before
    writing the value: hence, whichever template resource is enumerated
    last wins, regardless of the bindings.

    Luckily, WcApplyTemplate can use XrmCombineDatabase to load all of the
    template resources from the temporary database into the database used
    by Xt (and Wcl to create).  We cannot go directly from the template
    databases to the final databases because we must first pre-fix each
    template database with the path of the widget using the template.

    Two possible solutions: an Xrm lint which detects this sort of problem,
    or to enhance WcxCopyEntry so it is much, much smarter (and slower!).
    I prefer the lint approach.  Note that the lint must know the
    difference between a template file and a regular file to catch this
    problem.
*/

#include <X11/IntrinsicP.h>

#ifdef sun
#include <X11/ObjectP.h>	/* why don't they just use X from mit!?! */
#include <X11/RectObjP.h>
#endif

#include <X11/Wc/WcCreateP.h>

#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4

#include <X11/Xlibint.h>	/* needed to use templates (Xfree) */

#define MAXDBDEPTH 100		/* From X11R5 mit/lib/X/Xrm.c */

static XrmQuark WcQString;

/*  -- Mapping Agent: Map Template Names to Template XrmDatabase
*******************************************************************************
*/
static MapAgRec temAgentRec = MapAg_STATIC;
       MapAg    temAgent    = &temAgentRec;

#define WcMapTemplateFileLoaded( name ) \
	MapAg_Define( temAgent, NULL, name, NULL, 1 )

#define WcMapTemplateFileAlreadyLoaded( name ) \
	(int)MapAg_Find( temAgent, NULL, name, NULL )

#define WcMapTemplateNameToDatabaseDefine( name, db ) \
	MapAg_Define( temAgent, name, NULL, NULL, db )

#define WcMapTemplateNameToDatabase( name ) \
	(XrmDatabase)MapAg_Find( temAgent, name, NULL, NULL )

/*  -- Print Database Contents
*******************************************************************************
*/

typedef struct _DumpEntryRec {
    FILE*	stream;
    int		count;
} DumpEntryRec, *DumpEntry;

/*ARGSUSED*/
static Bool WcxDumpEntry(db, bindings, quarks, type, value, data)
    XrmDatabase*	db;
    XrmBindingList	bindings;
    XrmQuarkList	quarks;
    XrmRepresentation*	type;
    XrmValuePtr		value;
    XPointer		data;
{
    DumpEntry	dumpEntry = (DumpEntry)data;
    int		i;

    dumpEntry->count++;

    for ( i = 0  ;  quarks[i] != NULLQUARK  ;  i++ )
    {
	fprintf( dumpEntry->stream,"%s%s",
		(bindings[i] == XrmBindTightly ? "." : "*"),
		XrmQuarkToString(quarks[i]) );
    }

    /* Note: cannot be certain that value->addr is meaningful
    ** if value->size is zero.
    */
    if ( WcQString == *type )
    {
	if ( value->size )
	    fprintf( dumpEntry->stream, ":\t%s\n", (char*)value->addr );
	else
	    fprintf( dumpEntry->stream, ":\t(nil)\n" );
    }
    else
    {
	/* The following logic could use #if instead of if, but that is too
	 * ugly, and a decent compiler will not generate unreachable code.
	 */
	String typeString = XrmRepresentationToString(*type);

	if ( 0 == value->size || (XPointer)0 == value->addr )
	    fprintf( dumpEntry->stream, "(%s):\t(nil)\n",
			typeString );
	else if ( sizeof(XPointer) == sizeof(long int) )
	    fprintf( dumpEntry->stream, "(%s):\t0x%lX points to %d bytes\n",
			typeString, (long int)(value->addr), value->size );
	else if ( sizeof(XPointer) == sizeof(short int) )
	    fprintf( dumpEntry->stream, "(%s):\t0x%hX points to %d bytes\n",
			typeString, (int)(value->addr), value->size );
	else
	    fprintf( dumpEntry->stream, "(%s):\t0x%X points to %d bytes\n",
			typeString, (int)(value->addr), value->size );
    }

    return False;
}

/*  -- Print out all resources which apply to a widget.
*******************************************************************************
    Useful for debugging...
*/
void WcPreCreateDumpResources( parent, name, stream )
    Widget	parent;
    char*	name;
    FILE*	stream;
{
    Widget		w;
    int			depth;
    XrmQuark		nameQuarks[101];	/* widget path name quarks  */
    XrmQuark		classQuarks[101];	/* widget path class quarks */
    DumpEntryRec	dumpEntryRec;
    String		fullName = WcWidgetToFullName( parent );

    /*
     * Build quark lists of widget names and class names up the widget instance
     * tree up to the top level (shell) widget.
     */
    for ( w = parent, depth = 1  ;  (Widget)0 != w  ;  depth++ )
	w = XtParent(w);

    nameQuarks[depth] = classQuarks[depth] = NULLQUARK;
    depth--;
    nameQuarks[depth]  = XrmStringToQuark( name );
    classQuarks[depth] = XrmStringToQuark( "WeDoNotKnowTheClassYet" );

    for ( w = parent, depth--  ;  0 <= depth  ;  --depth )
    {
	nameQuarks[ depth] = w->core.xrm_name;
	classQuarks[depth] = w->core.widget_class->core_class.xrm_class;
	w = XtParent(w);
    }

    dumpEntryRec.stream = stream;
    dumpEntryRec.count  = 0;

    fprintf( stream, "==== Wcl: PreCreate Resources For Widget %s.%s:\n",
			fullName,name );
    XrmEnumerateDatabase( XtScreenDatabase( XtScreenOfObject(parent) ), 
			nameQuarks, classQuarks,
			XrmEnumOneLevel,
			WcxDumpEntry, (XPointer)&dumpEntryRec );
    fprintf( stream, "==== Wcl: Total Number of Resources: %d\n", 
			dumpEntryRec.count );
    XtFree( fullName );

#ifdef DEBUG
    {
	XrmQuark empty = NULLQUARK;
	DumpEntryRec dumpEntryRec;
	dumpEntryRec.stream = stderr;
	dumpEntryRec.count  = 0;
	fprintf(stderr,"======= Wcl: PreCreate All Resources\n");
	XrmEnumerateDatabase( XtScreenDatabase( XtScreenOfObject(parent) ),
			&empty, &empty,
			XrmEnumAllLevels,
			WcxDumpEntry, (XPointer)&dumpEntryRec );
    }
#endif
}

void WcPostCreateDumpResources( widget, stream )
    Widget	widget;
    FILE*	stream;
{
    Widget		w;
    int			depth;
    XrmQuark		nameQuarks[101];	/* widget path name quarks  */
	    XrmQuark		classQuarks[101];	/* widget path class quarks */
	    DumpEntryRec	dumpEntryRec;
	    String		fullName  = WcWidgetToFullName( widget );

    /*
     * Build quark lists of widget names and class names up the widget instance
     * tree up to the top level (shell) widget.
     */
    for ( w = widget, depth = 0  ;  (Widget)0 != w  ;  depth++ )
	w = XtParent(w);

    nameQuarks[depth] = classQuarks[depth] = NULLQUARK;

    for ( w = widget, depth--  ;  0 <= depth  ;  --depth )
    {
	nameQuarks[ depth] = w->core.xrm_name;
	classQuarks[depth] = w->core.widget_class->core_class.xrm_class;
	w = XtParent(w);
    }

    dumpEntryRec.stream = stream;
    dumpEntryRec.count  = 0;

    fprintf( stream, "==== Wcl PostCreate Resources: Widget %s of class %s:\n",
			fullName, 
			widget->core.widget_class->core_class.class_name );
    XrmEnumerateDatabase( XtScreenDatabase( XtScreenOfObject(widget) ), 
			nameQuarks, classQuarks,
			XrmEnumOneLevel,
			WcxDumpEntry, (XPointer)&dumpEntryRec );
    fprintf( stream, "==== Wcl: Total Number of Resources: %d\n", 
			dumpEntryRec.count );
    XtFree( fullName );
#ifdef DEBUG
    {
	XrmQuark empty = NULLQUARK;
	DumpEntryRec dumpEntryRec;
	dumpEntryRec.stream = stderr;
	dumpEntryRec.count  = 0;
	fprintf(stderr,"======= Wcl: PostCreate All Resources\n");
	XrmEnumerateDatabase( XtScreenDatabase( XtScreenOfObject(widget) ),
			&empty, &empty,
			XrmEnumAllLevels,
			WcxDumpEntry, (XPointer)&dumpEntryRec );
    }
#endif
}

/*  -- Load Template Database
*******************************************************************************
    Invoked during initialization, AFTER WcWARN has been initialized.
*/
/*ARGSUSED*/
void WcTemplateInitialize( app, wcl )
    XtAppContext	app;
    WclRecPtr		wcl;
{
    char  cleanName[MAX_XRMSTRING];
    char* next = WcCleanName( wcl->templateFiles, cleanName );

    WcQString = XrmPermStringToQuark( "String" );
    while ( WcNonNull( cleanName ) )
    {
	XrmQuark fileQuark = XrmStringToQuark( cleanName );

	if ( ! WcMapTemplateFileAlreadyLoaded( fileQuark ) )
	{
	    XrmQuark	templateQuark;
	    String	templateName, tp;
	    XrmDatabase	templateDb = XrmGetFileDatabase( cleanName );

	    WcMapTemplateFileLoaded( fileQuark );

	    /* template name is last component of template file name
	    */
	    tp = cleanName;
	    while( *tp )
		++tp;
	    while ( cleanName < tp && *tp != '/' )
		--tp;
	    if ( *tp == '/' )
		++tp;
	    templateName = tp;

	    if ( *templateName == '\0' )
		/* '/' terminated: let the load fail to annunciate error
		*/
		templateName = cleanName;

	    templateQuark = XrmStringToQuark( templateName );
	    WcMapTemplateNameToDatabaseDefine( templateQuark, templateDb );

	    if ( (XrmDatabase)0 == templateDb )
	    {
		WcWARN1( (Widget)0, "WcTemplateInitialize", "couldNotLoad",
		    "WcTemplateInitialize(): could not load template file %s\n",
		    cleanName );
	    }
	    else if ( wcl->traceTemplateDef )
	    {
		XrmQuark empty = NULLQUARK;
		DumpEntryRec dumpEntryRec;
		dumpEntryRec.stream = stderr;
		dumpEntryRec.count  = 0;
		fprintf(stderr,
			"==== Wcl Template Definition: %s from %s\n",
			templateName, cleanName);
		XrmEnumerateDatabase(	templateDb, &empty, &empty,
					XrmEnumAllLevels,
					WcxDumpEntry, (XtPointer)&dumpEntryRec);
		fprintf(stderr,
			"==== Wcl Template Defined: %s has %d resources.\n",
			templateName, dumpEntryRec.count );
	    }
	}
	next = WcSkipWhitespace_Comma( next );
	next = WcCleanName( next, cleanName );
    }
}

/*  -- Used by WcApplyTemplate(), XrmEnumerateDatabase() and WcxCopyEntry()
*******************************************************************************
*/
typedef struct _CopyData {
    int			count;		/* # times WcxCopyEntry called	*/
    int			trace;		/* enable expansion tracing	*/
    XrmQuark		template;	/* template we are fetching	*/
    int			targetLevel;	/* num of widgets above target	*/
    XrmBinding		bindings[101];	/* target path bindings		*/
    XrmQuark		quarks[101];	/* target path name quarks	*/
    XrmDatabase		db;		/* template resources only	*/
} CopyDataRec, *CopyData;

/*  -- Copy Database Entry
*******************************************************************************
   This function is called by XrmEnumerateDatabase() for each resource database
   entry found beneath the template name.

   WcxCopyEntry always returns False, which causes XrmEnumerateDatabase()
   to continue until done.
*/
/*ARGSUSED*/
static int WcxCopyEntry(db, bindings, quarks, type, value, data)
    XrmDatabase*	db;
    XrmBindingList	bindings;
    XrmQuarkList	quarks;
    XrmRepresentation*	type;
    XrmValue*		value;
    XPointer		data;
{
    CopyData	cd = (CopyData)data;
    int		ofTarget, ofTemplate;

    cd->count++;

    /*
     * This resource value goes in new database at location named by:
     *		*the*full*path*of*parent*child<b0><q0><b1><q1>...
     * We append all the bindings and quarks we got from the database.  We
     * re-use the bindings and quarks in the CopyData struct, the initial
     * bindings and quarks (from root thru to child) were already set up.
     */
    ofTarget   = cd->targetLevel;
    ofTemplate = 0;
    while (  quarks[ofTemplate] && ofTarget < MAXDBDEPTH  )
    {
	cd->bindings[ ofTarget ] = bindings[ ofTemplate ];
	cd->quarks[   ofTarget ] = quarks[   ofTemplate ];
	++ofTarget;
	++ofTemplate;
    }
    cd->quarks[ ofTarget ] = NULLQUARK;

    /* Load resource into target database
    */
    XrmQPutResource ( &cd->db, cd->bindings, cd->quarks, *type, value );

    if ( cd->trace )
    {
	XrmRepresentation typeRtn;
	XrmValue	  valueRtn;
	int i;
	for ( i = 0 ; cd->quarks[i] ; i++ )
	    fprintf(stderr,"%s%s",
			(cd->bindings[i] == XrmBindTightly ? "." : "*"),
			XrmQuarkToString(cd->quarks[i]) );
	XrmQGetResource( cd->db, cd->quarks, cd->quarks, &typeRtn, &valueRtn );
	fprintf(stderr,":  %s\n",(char*)valueRtn.addr);
    }

    /* false: keep enumerating until no more resources under template name
    */
    return 0;
}

/*  -- Apply template to child
*******************************************************************************
    The parent widget must exist.  We initially assume the template
    does in fact exist (this is certainly the correct and normal case).

    A template specification simply looks like a specification for some
    widget tree rooted at a top level shell, the template name looking
    identical to a top level shell name.

    Returns 0 if template not found, 1 if template found and loaded.
*/
int WcApplyTemplate( template, pw, name, trace )
    XrmQuark	template;
    Widget	pw;
    char*	name;
    int		trace;
{
    CopyDataRec		copyDataRec;
    Widget		w;
    int			depth;
    XrmQuark		templateNameQuarkList[2];
    XrmDatabase		dbUsedByXt;
    XrmDatabase		templateDb = WcMapTemplateNameToDatabase( template );

    if ( (XrmDatabase)0 == templateDb )
	return 0;

    /* Template we are getting resources for
    */
    copyDataRec.template = template;

    /* Initially there are no template resources
    */
    copyDataRec.count = 0;

    copyDataRec.trace = trace;

    /*
     * Build a quark and binding list from the root widget to the parent.
     * Drop the first component and make all pre-fix components loose, so it
     * is easy to override template values in resource files.  The last
     * binding absolutely must always be loose so XmCreate*Dialog style
     * constructors work.  Result:
     * 			*the*full*path*of*parent*child 
     * depth starts at 1: one for child, another for each widget from
     * parent to root.
     * Recursion here would be more elegant, but slower...
     */
    for ( w = pw, depth = 1  ;  (Widget)0 != w  ;  depth++ )
	w = XtParent(w);

    copyDataRec.targetLevel = depth;

    copyDataRec.bindings[depth] = copyDataRec.quarks[depth] = NULLQUARK;
    --depth;

    copyDataRec.bindings[depth] = XrmBindLoosely;
    copyDataRec.quarks[  depth] = XrmStringToQuark( name );
    --depth;

    for ( w = pw  ;  (Widget)0 != w  ;  w = XtParent(w) )
    {
	copyDataRec.bindings[depth] = XrmBindLoosely;
	copyDataRec.quarks[  depth] = w->core.xrm_name;
	--depth;
    }

    /* Create a database, single useless entry
    */
    copyDataRec.db = XrmGetStringDatabase ( "_scratch: scratch" );

    /* No prefixes assumed in template database
    */
    templateNameQuarkList[0] = NULLQUARK;

    if ( trace )
	fprintf(stderr,
		"==== Wcl Template Expansion: %s\n",
		XrmQuarkToString(template) );

    /* Go through all template database entries under the template name.
    */
    XrmEnumerateDatabase( templateDb,
			templateNameQuarkList, templateNameQuarkList,
			XrmEnumAllLevels,
			WcxCopyEntry, (XPointer)&copyDataRec );

    if ( trace )
	fprintf(stderr,
		"==== Wcl Template Expanded: %d template resources from %s\n",
		copyDataRec.count, XrmQuarkToString( template ) );

    /* Check to see if any resources were found under the template name.
    */
    if ( copyDataRec.count == 0 )
    {
	Xfree((char*)copyDataRec.db);
	return 0;
    }

    /*
     * WcxCopyEntry() has filled in a database with all template resources,
     * some of which may have been edited slightly.  Merge this new database
     * into primary db used by Xt.  Template resources DO NOT override
     * specific resources of template instance.  XrmCombineDatabase()
     * frees the temporary db.
     */
    dbUsedByXt = XtScreenDatabase( XtScreenOfObject( pw ) );

#ifdef DEBUG
    if ( trace )
    {
	XrmQuark empty = NULLQUARK;
	DumpEntryRec dumpEntryRec;
	dumpEntryRec.stream = stderr;
	dumpEntryRec.count  = 0;
	fprintf(stderr,"======= WcApplyTemplate: dbUsedByXt before Combine\n");
	XrmEnumerateDatabase( dbUsedByXt, 
			&empty, &empty,
			XrmEnumAllLevels,
			WcxDumpEntry, (XPointer)&dumpEntryRec );
    }
#endif

    XrmCombineDatabase( copyDataRec.db, &dbUsedByXt, False );

#ifdef DEBUG
    if ( trace )
    {
	XrmQuark empty = NULLQUARK;
	DumpEntryRec dumpEntryRec;
	dumpEntryRec.stream = stderr;
	dumpEntryRec.count  = 0;
	fprintf(stderr,"======= WcApplyTemplate: dbUsedByXt after Combine\n");
	XrmEnumerateDatabase( dbUsedByXt, 
			&empty, &empty,
			XrmEnumAllLevels,
			WcxDumpEntry, (XPointer)&dumpEntryRec );
	fprintf(stderr,"======= WcApplyTemplate to %s finished.\n",name);
    }
#endif

    return 1;
}

#else  /* pre X11R5, no-op implementations */

/*ARGSUSED*/
void WcPreCreateDumpResources( parent, name, stream )
    Widget	parent;
    char*	name;
    FILE*	stream;
{
    return;
}

/*ARGSUSED*/
void WcPostCreateDumpResources( widget, stream )
    Widget	widget;
    FILE*	stream;
{
    return;
}

/*ARGSUSED*/
void WcTemplateInitialize( app, wcl )
    XtAppContext	app;
    WclRecPtr		wcl;
{
    return;
}

/*ARGSUSED*/
int WcApplyTemplate( template, pw, name, trace )
    XrmQuark	template;
    Widget	pw;
    char*	name;
    int		trace;
{
    return 1;
}

#endif /* requires X11R5 */
