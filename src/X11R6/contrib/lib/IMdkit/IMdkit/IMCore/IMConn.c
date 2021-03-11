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
#if NeedVarargsPrototypes
# include <stdarg.h>
# define Va_start(a,b) va_start(a,b)
#else
# include <varargs.h>
# define Va_start(a,b) va_start(a)
#endif

static void
#if NeedVarargsPrototypes
_IMCountVaList(va_list var, int *total_count)
#else
_IMCountVaList(var, total_count)
    va_list var;
    int *total_count;
#endif
{
    char *attr;

    *total_count = 0;

    for (attr = va_arg(var, char*); attr; attr = va_arg(var, char*)) {
	va_arg(var, XIMArg*);
	++(*total_count);
    }
}

static void
#if NeedVarargsPrototypes
_IMVaToNestedList(va_list var, int max_count, XIMArg **args_return)
#else
_IMVaToNestedList(var, max_count, args_return)
    va_list var;
    int max_count;
    XIMArg **args_return;
#endif
{
    XIMArg *args;
    char   *attr;

    if (max_count <= 0) {
	*args_return = (XIMArg *)NULL;
	return;
    }

    args = (XIMArg *)malloc((unsigned)(max_count + 1) * sizeof(XIMArg));
    *args_return = args;
    if (!args) return;

    for (attr = va_arg(var, char*); attr; attr = va_arg(var, char*)) {
	args->name = attr;
	args->value = va_arg(var, XPointer);
	args++;
    }
    args->name = (char*)NULL;
}

static char *
#if NeedFunctionPrototypes
_FindModifiers(XIMArg *args)
#else
_FindModifiers(args)
XIMArg *args;
#endif
{
    char *modifiers;

    while (args->name) {
	if (!strcmp(args->name, IMModifiers)) {
	    modifiers = args->value;
	    return modifiers;
	} else {
	    args++;
	}
    }
    return NULL;
}

XIMS
#if NeedFunctionPrototypes
_GetIMS(char *modifiers)
#else
_GetIMS(modifiers)
char *modifiers;
#endif
{
    XIMS ims;
    extern IMMethodsRec Xi18n_im_methods;
#ifdef Use_Ximp
    extern IMMethodsRec Ximp_im_methods;
#endif

    if ((ims = (XIMS)malloc(sizeof(XIMProtocolRec))) == (XIMS)NULL) {
	return((XIMS)NULL);
    }
    memset(ims, 0, sizeof(XIMProtocolRec));

    if (!*modifiers || !modifiers || !strcmp(modifiers, "Xi18n")) {
	ims->methods = &Xi18n_im_methods;
#ifdef Use_Ximp
    } else if (!strcmp(modifiers, "XIMP")) {
	ims->methods = &Ximp_im_methods;
#endif
    } else {
	XFree(ims);
	return (XIMS)NULL;
    }
    return ims;
}

#if NeedVarargsPrototypes
XIMS
IMOpenIM(Display *display, ...)
#else
XIMS
IMOpenIM(display, va_alist)
Display *display;
va_dcl
#endif
{
    va_list var;
    int total_count;
    XIMArg *args;
    XIMS ims;
    char *modifiers;
    Status ret;

    Va_start(var, display);
    _IMCountVaList(var, &total_count);
    va_end(var);

    Va_start(var, display);
    _IMVaToNestedList(var, total_count, &args);
    va_end(var);

    modifiers = _FindModifiers(args);

    ims = _GetIMS(modifiers);
    if (ims == (XIMS)NULL) return (XIMS)NULL;

    ims->core.display = display;

    ims->protocol = (*ims->methods->setup)(display, args);
    if (ims->protocol == (void *)NULL) {
	XFree(ims);
	return (XIMS)NULL;
    }
    ret = (ims->methods->openIM)(ims);
    if (ret == False) {
	XFree(ims);
	return (XIMS)NULL;
    }
    return (XIMS)ims;
}

Status
#if NeedFunctionPrototypes
IMCloseIM(XIMS ims)
#else
IMCloseIM(ims)
XIMS ims;
#endif
{
    (ims->methods->closeIM)(ims);
    XFree(ims);
    return True;
}
