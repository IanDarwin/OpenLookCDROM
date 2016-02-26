#include <X11/Wc/COPY_X>
/*
    SCCS_data: @(#) Xt4GetResL.c 1.2 92/03/18 11:02:28

    This file is subject to the same copyright notice as the X11R4
    distribution, as the contents are basically taken directly from
    that distribution.

    The following is the XtGetConstraintResourceList function from
    the R4 Intrinsics.  This function is not provided by the Motif
    1.0 Intrinsics.  Only change: ConstraintClassFlag was changed to
    _XtConstraintBit.

    This file normally does not get included into the Wc library.  Only
    if the Widget Creation Library is intended to be used with Motif 1.0
    (which is generally a bad idea - get Motif 1.1!!!!!!!), this file
    should be compiled and either added to the library libWc, or 
    added to the list of object files during linking, placed between
    libWc and libXtm.

    Or better yet, use Motif 1.1 so you can use the X11R4 Intrinsics!!!
*/

#include <X11/IntrinsicP.h>
/*************** Begin source from X11R4 GetResList.c ***************/

static Boolean ClassIsSubclassOf(class, superclass)
    WidgetClass class, superclass;
{
    for (; class != NULL; class = class->core_class.superclass) {
	if (class == superclass) return True;
    }
    return False;
}

void XtGetConstraintResourceList(widget_class, resources, num_resources)
	WidgetClass widget_class;
	XtResourceList *resources;
	Cardinal *num_resources;
{
	int size;
	register int i, dest = 0;
	register XtResourceList *list, dlist;
	ConstraintWidgetClass class = (ConstraintWidgetClass)widget_class;

	if (   (class->core_class.class_inited &&
		!(class->core_class.class_inited & _XtConstraintBit)) /* DES */
	    || (!class->core_class.class_inited &&
		!ClassIsSubclassOf(widget_class, constraintWidgetClass))
	    || class->constraint_class.num_resources == 0) {

	    *resources = NULL;
	    *num_resources = 0;
	    return;
	}

	size = class->constraint_class.num_resources * sizeof(XtResource);
	*resources = (XtResourceList) XtMalloc((unsigned) size);

	if (!class->core_class.class_inited) {
	    /* Easy case */

	    bcopy((char *)class->constraint_class.resources,
		    (char *) *resources, size);
	    *num_resources = class->constraint_class.num_resources;
	    return;
	}

	/* Nope, it's the hard case */

	list = (XtResourceList *) class->constraint_class.resources;
	dlist = *resources;
	for (i = 0; i < class->constraint_class.num_resources; i++) {
	    if (list[i] != NULL) {
		dlist[dest].resource_name = (String)
			XrmQuarkToString((XrmQuark) list[i]->resource_name);
		dlist[dest].resource_class = (String) 
			XrmQuarkToString((XrmQuark) list[i]->resource_class);
		dlist[dest].resource_type = (String)
			XrmQuarkToString((XrmQuark) list[i]->resource_type);
		dlist[dest].resource_size = list[i]->resource_size;
		dlist[dest].resource_offset = -(list[i]->resource_offset + 1);
		dlist[dest].default_type = (String)
			XrmQuarkToString((XrmQuark) list[i]->default_type);
		dlist[dest].default_addr = list[i]->default_addr;
		dest++;
	    }
	}
	*num_resources = dest;
}

/*************** End source from X11R4 GetResList.c ***************/
