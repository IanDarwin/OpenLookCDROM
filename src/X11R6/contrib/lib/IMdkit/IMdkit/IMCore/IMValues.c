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

char *
#if NeedVarargsPrototypes
IMGetIMValues(XIMS ims, ...)
#else
IMGetIMValues(ims, va_alist)
XIMS ims;
va_dcl
#endif
{
    va_list var;
    int total_count;
    XIMArg *args;
    char *ret;

    Va_start(var, ims);
    _IMCountVaList(var, &total_count);
    va_end(var);

    Va_start(var, ims);
    _IMVaToNestedList(var, total_count, &args);
    va_end(var);

    ret = (*ims->methods->getIMValues)(ims, args);

    if (args) XFree((char *)args);
    return ret;
}

char *
#if NeedVarargsPrototypes
IMSetIMValues(XIMS ims, ...)
#else
IMSetIMValues(ims, va_alist)
XIMS ims;
va_dcl
#endif
{
    va_list var;
    int total_count;
    XIMArg *args;
    char *ret;  

    Va_start(var, ims);
    _IMCountVaList(var, &total_count);
    va_end(var);

    Va_start(var, ims);
    _IMVaToNestedList(var, total_count, &args);
    va_end(var);

    ret = (*ims->methods->setIMValues)(ims, args);

    if (args) XFree((char *)args);

    return ret;
}
