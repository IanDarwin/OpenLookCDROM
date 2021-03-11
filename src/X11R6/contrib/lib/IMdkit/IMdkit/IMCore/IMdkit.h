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
#ifndef _IMdkit_h
#define _IMdkit_h

#include <X11/Xmd.h>

/* IM Attributes Name */
#define IMModifiers		"modifiers"
#define IMServerWindow		"serverWindow"
#define IMServerName		"serverName"
#define IMServerTransport	"serverTransport"
#define IMLocale		"locale"
#define IMInputStyles		"inputStyles"
#define IMProtocolHandler	"protocolHandler"
#define IMOnKeysList		"onKeysList"
#define IMOffKeysList		"offKeysList"
#define IMEncodingList		"encodingList"
#define IMFilterEventMask	"filterEventMask"
#define IMProtocolDepend	"protocolDepend"

/* Masks for IM Attributes Name */
#define I18N_IMSERVER_WIN	0x0001 /* IMServerWindow */
#define I18N_IM_NAME		0x0002 /* IMServerName */
#define I18N_IM_LOCALE		0x0004 /* IMLocale */
#define I18N_IM_ADDRESS		0x0008 /* IMServerTransport */
#define I18N_INPUT_STYLES	0x0010 /* IMInputStyles */
#define I18N_ON_KEYS		0x0020 /* IMOnKeysList */
#define I18N_OFF_KEYS		0x0040 /* IMOffKeysList */
#define I18N_IM_HANDLER		0x0080 /* IMProtocolHander */
#define I18N_ENCODINGS		0x0100 /* IMEncodingList */
#define I18N_FILTERMASK		0x0200 /* IMFilterEventMask */
#define I18N_PROTO_DEPEND	0x0400 /* IMProtoDepend */

typedef struct {
    char	*name;
    XPointer	value;
} XIMArg;

typedef struct {
    CARD32	keysym;
    CARD32	modifier;
    CARD32	modifier_mask;
} XIMTriggerKey;

typedef struct {
    unsigned short count_keys;
    XIMTriggerKey *keylist;
} XIMTriggerKeys;

typedef char *XIMEncoding;

typedef struct {
    unsigned short count_encodings;
    XIMEncoding *supported_encodings;
} XIMEncodings;

typedef struct {
    void*	(*setup)();
    Status	(*openIM)();
    Status	(*closeIM)();
    char*	(*setIMValues)();
    char*	(*getIMValues)();
    Status	(*forwardEvent)();
    Status	(*commitString)();
    int		(*callCallback)();
    int		(*preeditStart)();
    int		(*preeditEnd)();
} IMMethodsRec, *IMMethods;

typedef struct _XIMS *XIMS;

typedef struct {
    Display	*display;
    int		screen;
} IMCoreRec, *IMCore;

typedef struct _XIMS {
    IMMethods	methods;
    IMCoreRec	core;
    void	*protocol;
} XIMProtocolRec;

/* 
 * X function declarations.
 */
extern XIMS IMOpenIM(
#if NeedFunctionPrototypes
    Display * /* display */,
    ...
#endif
);

extern Status IMCloseIM(
#if NeedFunctionPrototypes
    XIMS
#endif
);

extern char* IMSetIMValues(
#if NeedFunctionPrototypes
    XIMS,
    ...
#endif
);

extern char* IMGetIMValues(
#if NeedFunctionPrototypes
    XIMS,
    ...
#endif
);

void IMForwardEvent(
#if NeedFunctionPrototypes
    XIMS, XPointer
#endif
);

void IMCommitString(
#if NeedFunctionPrototypes
    XIMS, XPointer
#endif
);

int IMCallCallback(
#if NeedFunctionPrototypes
    XIMS, XPointer
#endif
);

int IMPreeditEnd(
#if NeedFunctionPrototypes
    XIMS, XPointer
#endif
);
#endif /* IMdkit_h */
