#include <X11/Wc/COPY_X>

/*
* SCCS_data: @(#) XtName.c 1.4 92/03/18 11:02:30
*
* NOTE: Some old versions of Xt (including X11R4 before several patches)
* do not handle Gadgets, and many do not find names with wildcards.
*
* Below is the code extracted from the X11R5 distribution, with very
* minor changes to make it independent from the rest of the R5 Intrinsics.
*
* differences:	changed name from XtNameToWidget to WcChildNameToWidget
*		Deleted gratuitous double function decl
*/

#include <X11/IntrinsicP.h>
#include <X11/ObjectP.h>		/* some machines need this */
#include <X11/StringDefs.h>

#include <X11/Wc/WcCreateP.h>

/*************** Begin source from X11R5 Xtos.h ***************/

#ifndef ALLOCATE_LOCAL
#define ALLOCATE_LOCAL(size) XtMalloc((unsigned long)(size))
#define DEALLOCATE_LOCAL(ptr) XtFree((caddr_t)(ptr))
#endif /* ALLOCATE_LOCAL */

/*************** End source from X11R5 Xtos.h ***************/

#define _XtAllocError		XtError

/*************** Begin source from X11R5 lib/Xt/Intrinsics.c ***************/
/* ---------------- XtNameToWidget ----------------- */

static Widget NameListToWidget();

typedef Widget (*NameMatchProc)();

static Widget MatchExactChildren(names, bindings, children, num,
	in_depth, out_depth, found_depth)
    XrmNameList     names;
    XrmBindingList  bindings;
    register WidgetList children;
    register int num;
    int in_depth, *out_depth, *found_depth;
{
    register Cardinal   i;
    register XrmName    name = *names;
    Widget w, result = NULL;
    int d, min = 10000;

    for (i = 0; i < num; i++) {
	if (name == children[i]->core.xrm_name) {
	    w = NameListToWidget(children[i], &names[1], &bindings[1],
		    in_depth+1, &d, found_depth);
	    if (w != NULL && d < min) {result = w; min = d;}
	}
    }
    *out_depth = min;
    return result;
}

static Widget MatchWildChildren(names, bindings, children, num,
	in_depth, out_depth, found_depth)
    XrmNameList     names;
    XrmBindingList  bindings;
    register WidgetList children;
    register int num;
    int in_depth, *out_depth, *found_depth;
{
    register Cardinal   i;
    Widget w, result = NULL;
    int d, min = 10000;

    for (i = 0; i < num; i++) {
	w = NameListToWidget(children[i], names, bindings,
		in_depth+1, &d, found_depth);
	if (w != NULL && d < min) {result = w; min = d;}
    }
    *out_depth = min;
    return result;
}

static Widget SearchChildren(root, names, bindings, matchproc,
	in_depth, out_depth, found_depth)
    Widget root;
    XrmNameList     names;
    XrmBindingList  bindings;
    NameMatchProc matchproc;
    int in_depth, *out_depth, *found_depth;
{
    Widget w1, w2;
    int d1, d2;

    if (XtIsComposite(root)) {
	w1 = (*matchproc)(names, bindings,
		((CompositeWidget) root)->composite.children,
		((CompositeWidget) root)->composite.num_children,
		in_depth, &d1, found_depth);
    } else d1 = 10000;
    w2 = (*matchproc)(names, bindings, root->core.popup_list,
	    root->core.num_popups, in_depth, &d2, found_depth);
    *out_depth = (d1 < d2 ? d1 : d2);
    return (d1 < d2 ? w1 : w2);
}

static Widget NameListToWidget(root, names, bindings,
	in_depth, out_depth, found_depth)
    register Widget root;
    XrmNameList     names;
    XrmBindingList  bindings;
    int in_depth, *out_depth, *found_depth;
{
    Widget w1, w2;
    int d1, d2;

    if (in_depth >= *found_depth) {
	*out_depth = 10000;
	return NULL;
    }

    if (names[0] == NULLQUARK) {
	*out_depth = *found_depth = in_depth;
	return root;
    }

    if (! XtIsWidget(root)) {
	*out_depth = 10000;
	return NULL;
    }

    if (*bindings == XrmBindTightly) {
	return SearchChildren(root, names, bindings, MatchExactChildren,
		in_depth, out_depth, found_depth);

    } else {	/* XrmBindLoosely */
	w1 = SearchChildren(root, names, bindings, MatchExactChildren,
		in_depth, &d1, found_depth);
	w2 = SearchChildren(root, names, bindings, MatchWildChildren,
		in_depth, &d2, found_depth);
	*out_depth = (d1 < d2 ? d1 : d2);
	return (d1 < d2 ? w1 : w2);
    }
} /* NameListToWidget */

Widget WcChildNameToWidget(root, name)
    Widget root;
    String name;
{
    XrmName *names;
    XrmBinding *bindings;
    int len, depth, found = 10000;
    Widget result;

    len = strlen(name);
    if (len == 0) return NULL;

    names = (XrmName *) ALLOCATE_LOCAL((unsigned) (len+1) * sizeof(XrmName));
    bindings = (XrmBinding *)
	ALLOCATE_LOCAL((unsigned) (len+1) * sizeof(XrmBinding));
    if (names == NULL || bindings == NULL) _XtAllocError(NULL);

    XrmStringToBindingQuarkList(name, bindings, names);
    if (names[0] == NULLQUARK) {
	DEALLOCATE_LOCAL((char *) bindings);
	DEALLOCATE_LOCAL((char *) names);
	return NULL;
    }

    result = NameListToWidget(root, names, bindings, 0, &depth, &found);

    DEALLOCATE_LOCAL((char *) bindings);
    DEALLOCATE_LOCAL((char *) names);
    return result;
} /* XtNameToWidget */

