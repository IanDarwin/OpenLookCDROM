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

/* Public Function */
void
#if NeedFunctionPrototypes
IMForwardEvent(XIMS ims, XPointer call_data)
#else
IMForwardEvent(ims, call_data)
XIMS ims;
XPointer call_data;
#endif
{
    (ims->methods->forwardEvent)(ims, call_data);
    return;
}

void
#if NeedFunctionPrototypes
IMCommitString(XIMS ims, XPointer call_data)
#else
IMCommitString(ims, call_data)
XIMS ims;
XPointer call_data;
#endif
{
    (ims->methods->commitString)(ims, call_data);

    return;
}

int
#if NeedFunctionPrototypes
IMCallCallback(XIMS ims, XPointer call_data)
#else
IMCallCallback(ims, call_data)
XIMS ims;
XPointer call_data;
#endif
{
    return (ims->methods->callCallback)(ims, call_data);
}

int
#if NeedFunctionPrototypes
IMPreeditStart(XIMS ims, XPointer call_data)
#else
IMPreeditStart(ims, call_data)
XIMS ims;
XPointer call_data;
#endif
{
    return (ims->methods->preeditStart)(ims, call_data);
}

int
#if NeedFunctionPrototypes
IMPreeditEnd(XIMS ims, XPointer call_data)
#else
IMPreeditEnd(ims, call_data)
XIMS ims;
XPointer call_data;
#endif
{
    return (ims->methods->preeditEnd)(ims, call_data);
}
