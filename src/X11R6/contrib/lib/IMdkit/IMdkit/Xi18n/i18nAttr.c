/******************************************************************

         Copyright 1993, 1994 by Hewlett-Packard Company

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose without fee is hereby granted,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Hewlett-Packard not
be used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
Hewlett-Packard Company makes no representations about the suitability
of this software for any purpose.
It is provided "as is" without express or implied warranty.

HEWLETT-PACKARD COMPANY DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
IN NO EVENT SHALL HEWLETT-PACKARD COMPANY BE LIABLE FOR ANY SPECIAL,
INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

Author:
    Hidetoshi Tajima	Hewlett-Packard Company.
			(tajima@kobe.hp.com)
******************************************************************/
#include <X11/Xlib.h>
#include "IMdkit.h"
#include "Xi18n.h"

typedef struct {
    char *name;
    CARD16 type;
} IMListOfAttr;

typedef struct {
    char *name;
    CARD8 major_opcode;
    CARD8 minor_opcode;
} IMExtList;

IMListOfAttr Default_IMattr[] = {
    {XNQueryInputStyle, XimType_XIMStyles},
    {(char *)NULL, (CARD16)0}
};

IMListOfAttr Default_ICattr[] = {
    {XNInputStyle, XimType_CARD32},
    {XNClientWindow, XimType_Window},
    {XNFocusWindow, XimType_Window},
    {XNFilterEvents, XimType_CARD32},
    {XNPreeditAttributes, XimType_NEST},
    {XNStatusAttributes, XimType_NEST},
    {XNFontSet, XimType_XFontSet},
    {XNArea, XimType_XRectangle},
    {XNAreaNeeded, XimType_XRectangle},
    {XNColormap, XimType_CARD32},
    {XNStdColormap, XimType_CARD32},
    {XNForeground, XimType_CARD32},
    {XNBackground, XimType_CARD32},
    {XNBackgroundPixmap, XimType_CARD32},
    {XNSpotLocation, XimType_XPoint},
    {XNLineSpace, XimType_CARD32},
    {XNPreeditState, XimType_CARD32},
    {XNSeparatorofNestedList, XimType_SeparatorOfNestedList},
    {(char *)NULL, (CARD16)NULL}
};

IMExtList Default_Extension[] = {
    {"XIM_EXT_MOVE", XIM_EXTENSION, XIM_EXT_MOVE},
    {"XIM_EXT_SET_EVENT_MASK", XIM_EXTENSION, XIM_EXT_SET_EVENT_MASK},
    {"XIM_EXT_FORWARD_KEYEVENT", XIM_EXTENSION, XIM_EXT_FORWARD_KEYEVENT},
    {(char *)NULL, (CARD8)NULL, (CARD8)NULL}
};

static void
#if NeedFunctionPrototypes
CountAttrList(IMListOfAttr *attr, int *total_count)
#else
CountAttrList(attr, total_count)
IMListOfAttr *attr;
int *total_count;
#endif
{
    *total_count = 0;

    while (attr->name != NULL) {
	attr++;
	++(*total_count);
    }
}

static XIMAttr *
#if NeedFunctionPrototypes
CreateAttrList(Xi18n i18n_core, IMListOfAttr *attr, int *total_count)
#else
CreateAttrList(i18n_core, attr, total_count)
Xi18n	i18n_core;
IMListOfAttr *attr;
int *total_count;
#endif
{
    XIMAttr *args, *p;
    unsigned int buf_size;

    CountAttrList(attr, total_count);

    buf_size = (unsigned)(*total_count + 1) * sizeof(XIMAttr);
    args = (XIMAttr *)malloc(buf_size);
    if (!args) return (XIMAttr *)NULL;

    memset(args, 0, buf_size);

    for (p = args; attr->name != NULL; attr++, p++) {
	p->name = attr->name;
	p->length = strlen(attr->name);
	p->type = (CARD16)attr->type;
	p->attribute_id = XrmStringToQuark(p->name);
	if (!strcmp(p->name, XNPreeditAttributes))
	  i18n_core->address.preeditAttr_id = p->attribute_id;
	else if (!strcmp(p->name, XNStatusAttributes))
	  i18n_core->address.statusAttr_id = p->attribute_id;
	else if (!strcmp(p->name, XNSeparatorofNestedList))
	  i18n_core->address.separatorAttr_id = p->attribute_id;
    }
    p->name = (char*)NULL;

    return args;
}

void
#if NeedFunctionPrototypes
_Xi18nInitAttrList(Xi18n i18n_core)
#else
_Xi18nInitAttrList(i18n_core)
Xi18n i18n_core;
#endif
{
    XIMAttr *args;
    int	total_count;

    /* init IMAttr list */
    if (i18n_core->address.xim_attr)
      XFree((char *)i18n_core->address.xim_attr);
    args = CreateAttrList(i18n_core, Default_IMattr, &total_count);

    i18n_core->address.im_attr_num = total_count;
    i18n_core->address.xim_attr = (XIMAttr *)args;

    /* init ICAttr list */
    if (i18n_core->address.xic_attr)
      XFree((char *)i18n_core->address.xic_attr);
    args = CreateAttrList(i18n_core, Default_ICattr, &total_count);

    i18n_core->address.ic_attr_num = total_count;
    i18n_core->address.xic_attr = (XICAttr *)args;

    return;
}

void
#if NeedFunctionPrototypes
_Xi18nInitExtension(Xi18n i18n_core)
#else
_Xi18nInitExtension(i18n_core)
Xi18n i18n_core;
#endif
{
    register int i;
    IMExtList *extensions = (IMExtList*)Default_Extension;
    XIMExt *ext_list = (XIMExt*)i18n_core->address.extension;
    
    for (i = 0; extensions->name != NULL; i++, ext_list++, extensions++) {
	ext_list->major_opcode = extensions->major_opcode;
	ext_list->minor_opcode = extensions->minor_opcode;
	ext_list->name = extensions->name;
	ext_list->length = strlen(ext_list->name);
    }
    i18n_core->address.ext_num = i;
    return;
}
