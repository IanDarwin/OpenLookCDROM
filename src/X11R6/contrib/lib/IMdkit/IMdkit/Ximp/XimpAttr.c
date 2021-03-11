/* Copyright 1994 by Sun Microsystems, Inc. */
/* @(#)XimpAttr.c	1.8 94/02/16 */
/******************************************************************
 
              Copyright 1994 by Sun Microsystems, Inc.
              Copyright 1994 by Hewlett-Packard Company
 
Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Sun Microsystems, Inc.
and Hewlett-Packard not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior permission.
Sun Microsystems, Inc. and Hewlett-Packard make no representations about
the suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
 
SUN MICROSYSTEMS INC. AND HEWLETT-PACKARD COMPANY DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
SUN MICROSYSTEMS, INC. AND HEWLETT-PACKARD COMPANY BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 
  Author: Hiromu Inukai (inukai@Japan.Sun.COM) Sun Microsystems, Inc.
          Hidetoshi Tajima(tajima@kobe.hp.com) Hewlett-Packard Company.
 
******************************************************************/

#ifndef	_XIMPATTR_C_
#define	_XIMPATTR_C_
#include <X11/Xatom.h>
#include "XimpData.h"

#define PREEDIT_MASK4 (XIMP_PRE_AREA_MASK4|XIMP_PRE_AREANEED_MASK4|\
		       XIMP_PRE_COLORMAP_MASK4|XIMP_PRE_STD_COLORMAP_MASK4|\
		       XIMP_PRE_FG_MASK4|XIMP_PRE_BG_MASK4|\
		       XIMP_PRE_BGPIXMAP_MASK4|XIMP_PRE_LINESP_MASK4|\
		       XIMP_PRE_CURSOR_MASK4|XIMP_PRE_SPOTL_MASK4)

#define STATUS_MASK4  (XIMP_STS_AREA_MASK4|XIMP_STS_AREANEED_MASK4|\
		       XIMP_STS_COLORMAP_MASK4|XIMP_STS_STD_COLORMAP_MASK4|\
		       XIMP_STS_FG_MASK4|XIMP_STS_BG_MASK4|\
		       XIMP_STS_BGPIXMAP_MASK4|XIMP_STS_LINESP_MASK4|\
		       XIMP_STS_CURSOR_MASK4|XIMP_STS_WINDOW_MASK4)

#define PREEDIT_MASK3 (XIMP_PRE_AREA_MASK3|XIMP_PRE_AREANEED_MASK3|\
		       XIMP_PRE_COLORMAP_MASK3|XIMP_PRE_FG_MASK3|\
		       XIMP_PRE_BG_MASK3|XIMP_PRE_BGPIXMAP_MASK3|\
		       XIMP_PRE_LINESP_MASK3|XIMP_PRE_CURSOR_MASK3|\
		       XIMP_PRE_SPOTL_MASK3)

#define STATUS_MASK3  (XIMP_STS_AREA_MASK3|XIMP_STS_AREANEED_MASK3|\
		       XIMP_STS_COLORMAP_MASK3|XIMP_STS_FG_MASK3|\
		       XIMP_STS_BG_MASK3|XIMP_STS_BGPIXMAP_MASK3|\
		       XIMP_STS_LINESP_MASK3|XIMP_STS_CURSOR_MASK3|\
		       XIMP_STS_WINDOW_MASK3)
static Bool
#if NeedFunctionPrototypes
readProperty(Display *dpy, Window win, Atom prop, Atom type, int format,
	     unsigned char **datapp, unsigned long *lenp)
#else
readProperty(dpy, win, prop, type, format, datapp, lenp)
Display *dpy;
Window win;
Atom prop;
Atom type;
int format;
unsigned char **datapp;
unsigned long *lenp;
#endif
{
    Atom realtype;
    int realformat;
    unsigned long bytesafter;

    *datapp = NULL;
    if (XGetWindowProperty(dpy, win, prop, 0L, 1000000L, True, type,
			   &realtype, &realformat, lenp,
			   &bytesafter, datapp) != Success)
      return False;
    if (realtype == None) {
	return False;
    } else if (realtype != type) {
	return False;
    } else if (realformat != format) {
	if (*datapp != NULL) XFree((char *)*datapp);
	*datapp = NULL;
	return False;
    }
    return True;
}

static Bool
#if NeedFunctionPrototypes
writeProperty(Display *dpy, Window win, Atom prop, Atom type, int format,
	     unsigned char *datap, unsigned long len)
#else
writeProperty(dpy, win, prop, type, format, datap, len)
Display *dpy;
Window win;
Atom prop;
Atom type;
int format;
unsigned char *datap;
unsigned long len;
#endif
{
    (void)XChangeProperty(dpy, win, prop, type, format,
			  PropModeReplace, datap, len);
    /* always return True */
    return True;
}

static Bool
#if NeedFunctionPrototypes
getFocusProperty(XIMPCore core, XIMPICValuesStruct *values)
#else
getFocusProperty(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    unsigned char *data;
    unsigned long len;

    if (!readProperty(core->display, values->client_win,
		      core->atoms.focus,
		      XA_WINDOW, 32,
		      &data, &len)) {
	return False;
    } else if (len != 1) {
	XFree((char *)data);
	return False;
    }
    values->focus_win = *(Window *)data;
    XFree((char *)data);
    return True;
}

static Bool
#if NeedFunctionPrototypes
getPreeditFontProperty(XIMPCore core, XIMPICValuesStruct *values)
#else
getPreeditFontProperty(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    char *data;
    unsigned long len;

    if (!readProperty(core->display, values->client_win,
		      core->atoms.preedit_font,
		      XA_STRING, 8,
		      (unsigned char **)&data, &len)) {
	return False;
    }
    values->pre_font = (char*)data;
    return True;
}

static Bool
#if NeedFunctionPrototypes
getStatusFontProperty(XIMPCore core, XIMPICValuesStruct *values)
#else
getStatusFontProperty(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    char *data;
    unsigned long len;

    if (!readProperty(core->display, values->client_win,
		      core->atoms.status_font,
		      XA_STRING, 8,
		      (unsigned char **)&data, &len)) {
	return False;
    }
    values->sts_font = (char *)data;
    return True;
}

static Bool
#if NeedFunctionPrototypes
getServerType4(XIMPCore core, XIMPICValuesStruct *values)
#else
getServerType4(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    char *data;
    unsigned long len;

    if (!readProperty(core->display, values->client_win,
		      core->atoms.supported_types,
		      core->atoms.supported_types, 32,
		      (unsigned char **)&data, &len)) {
	return False;
    }
    values->ximp_type_mask = *(unsigned long*)data;
    XFree((char *)data);
    return True;
}

static Bool
#if NeedFunctionPrototypes
getPreeditProperty4(XIMPCore core, XIMPICValuesStruct *values)
#else
getPreeditProperty4(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    char *data;
    unsigned long len;

    if (!readProperty(core->display, values->client_win,
		      core->atoms.preedit,
		      core->atoms.preedit, 32,
		      (unsigned char **)&data, &len)) {
	return False;
    }
    values->pre_values = (Ximp_PreeditPropRec4*)data;
    return True;
}

static Bool
#if NeedFunctionPrototypes
getStatusProperty4(XIMPCore core, XIMPICValuesStruct *values)
#else
getStatusProperty4(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    char *data;
    unsigned long len;

    if (!readProperty(core->display, values->client_win,
		      core->atoms.status,
		      core->atoms.status, 32,
		      (unsigned char **)&data, &len)) {
	return False;
    }
    values->sts_values = (Ximp_StatusPropRec4*)data;
    return True;
}

static Bool
#if NeedFunctionPrototypes
getPreeditProperty3(XIMPCore core, XIMPICValuesStruct *values)
#else
getPreeditProperty3(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    Ximp_PreeditPropRec3 *preedit_bc;
    Ximp_PreeditPropRec4 *pre_values =
      (Ximp_PreeditPropRec4 *)values->pre_values;
    unsigned long len;

    if (!readProperty(core->display, values->client_win,
		      core->atoms.preedit,
		      core->atoms.preedit, 32,
		      (unsigned char **)&preedit_bc, &len)) {
	return False;
    }
    pre_values->Area         = preedit_bc->Area;
    pre_values->AreaNeeded   = preedit_bc->AreaNeeded;
    pre_values->SpotLocation = preedit_bc->SpotLocation;
    pre_values->Colormap     = preedit_bc->Colormap;
    pre_values->Foreground   = preedit_bc->Foreground;
    pre_values->Background   = preedit_bc->Background;
    pre_values->Bg_Pixmap    = preedit_bc->Bg_Pixmap;
    pre_values->LineSpacing  = preedit_bc->LineSpacing;
    pre_values->Cursor       = preedit_bc->Cursor;
    return True;
}

static Bool
#if NeedFunctionPrototypes
getStatusProperty3(XIMPCore core, XIMPICValuesStruct *values)
#else
getStatusProperty3(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    Ximp_StatusPropRec3 *status_bc;
    Ximp_StatusPropRec4 *sts_values =
      (Ximp_StatusPropRec4 *)values->sts_values;
    unsigned long len;

    if (!readProperty(core->display, values->client_win,
		      core->atoms.status,
		      core->atoms.status, 32,
		      (unsigned char **)&status_bc, &len)) {
	return False;
    }
    sts_values->Area        = status_bc->Area;
    sts_values->AreaNeeded  = status_bc->AreaNeeded;
    sts_values->Colormap    = status_bc->Colormap;
    sts_values->Foreground  = status_bc->Foreground;
    sts_values->Background  = status_bc->Background;
    sts_values->Bg_Pixmap   = status_bc->Bg_Pixmap;
    sts_values->LineSpacing = status_bc->LineSpacing;
    sts_values->Cursor      = status_bc->Cursor;
    sts_values->window      = status_bc->window;
    return True;
}

static Bool
#if NeedFunctionPrototypes
setFocusProperty(XIMPCore core, XIMPICValuesStruct *values)
#else
setFocusProperty(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    return writeProperty(core->display, values->client_win,
			 core->atoms.focus,
			 XA_WINDOW, 32,
			 (unsigned char *)&values->focus_win, 1);
}

static Bool
#if NeedFunctionPrototypes
setPreeditFontProperty(XIMPCore core, XIMPICValuesStruct *values)
#else
setPreeditFontProperty(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    return writeProperty(core->display, values->client_win,
			 core->atoms.preedit_font,
			 XA_STRING, 8,
			 (unsigned char *)values->pre_font,
			 (CARD32)strlen(values->pre_font));
}

static Bool
#if NeedFunctionPrototypes
setStatusFontProperty(XIMPCore core, XIMPICValuesStruct *values)
#else
setStatusFontProperty(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    return writeProperty(core->display, values->client_win,
			 core->atoms.status_font,
			 XA_STRING, 8,
			 (unsigned char *)values->sts_font,
			 (CARD32)strlen(values->sts_font));
}

static Bool
#if NeedFunctionPrototypes
setServerType4(XIMPCore core, XIMPICValuesStruct *values)
#else
setServerType4(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    return writeProperty(core->display, values->client_win,
			 core->atoms.supported_types,
			 core->atoms.supported_types,
			 32,
			 (unsigned char *)&values->ximp_type_mask, 1);
}

static Bool
#if NeedFunctionPrototypes
setPreeditProperty4(XIMPCore core, XIMPICValuesStruct *values)
#else
setPreeditProperty4(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    return writeProperty(core->display, values->client_win,
			 core->atoms.preedit,
			 core->atoms.preedit,
			 32,
			 (unsigned char *)values->pre_values,
			 XIMP_PREEDIT_MAX_LONG4);
}

static Bool
#if NeedFunctionPrototypes
setStatusProperty4(XIMPCore core, XIMPICValuesStruct *values)
#else
setStatusProperty4(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    return writeProperty(core->display, values->client_win,
			 core->atoms.status,
			 core->atoms.status,
			 32,
			 (unsigned char *)values->sts_values,
			 XIMP_STATUS_MAX_LONG4);
}

static Bool
#if NeedFunctionPrototypes
setPreeditProperty3(XIMPCore core, XIMPICValuesStruct *values)
#else
setPreeditProperty3(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    Ximp_PreeditPropRec3 preedit_bc;
    Ximp_PreeditPropRec4 *pre_values = 
      (Ximp_PreeditPropRec4*)values->pre_values;

    memset(&preedit_bc, 0, sizeof(Ximp_PreeditPropRec3));

    preedit_bc.Area		= pre_values->Area;
    preedit_bc.AreaNeeded	= pre_values->AreaNeeded;
    preedit_bc.SpotLocation	= pre_values->SpotLocation;
    preedit_bc.Colormap		= pre_values->Colormap;
    preedit_bc.Foreground	= pre_values->Foreground;
    preedit_bc.Background	= pre_values->Background;
    preedit_bc.Bg_Pixmap	= pre_values->Bg_Pixmap;
    preedit_bc.LineSpacing	= pre_values->LineSpacing;
    preedit_bc.Cursor		= pre_values->Cursor;

    return writeProperty(core->display, values->client_win,
			 core->atoms.preedit,
			 core->atoms.preedit,
			 32,
			 (unsigned char *)&preedit_bc,
			 XIMP_PREEDIT_MAX_LONG3);
}

static Bool
#if NeedFunctionPrototypes
setStatusProperty3(XIMPCore core, XIMPICValuesStruct *values)
#else
setStatusProperty3(core, values)
XIMPCore core;
XIMPICValuesStruct *values;
#endif
{
    Ximp_StatusPropRec3 status_bc;
    Ximp_StatusPropRec4 *sts_values = 
      (Ximp_StatusPropRec4*)values->sts_values;

    memset(&status_bc, 0, sizeof(Ximp_StatusPropRec3));

    status_bc.Area		= sts_values->Area;
    status_bc.AreaNeeded	= sts_values->AreaNeeded;
    status_bc.Colormap		= sts_values->Colormap;
    status_bc.Foreground	= sts_values->Foreground;
    status_bc.Background	= sts_values->Background;
    status_bc.Bg_Pixmap		= sts_values->Bg_Pixmap;
    status_bc.LineSpacing	= sts_values->LineSpacing;
    status_bc.Cursor		= sts_values->Cursor;
    status_bc.window		= sts_values->window;

    return writeProperty(core->display, values->client_win,
			 core->atoms.status,
			 core->atoms.status,
			 32,
			 (unsigned char *)&status_bc,
			 XIMP_STATUS_MAX_LONG3);
}

/* Public functions */
Bool
#if NeedFunctionPrototypes
_XimpGetProperty(XIMPCore core, XimpClient *client,
		 XIMPICValuesStruct *values)
#else
_XimpGetProperty(core, client, values)
XIMPCore core;
XimpClient *client;
XIMPICValuesStruct *values;
#endif
{
    long mask = values->attr_mask;

    if (IS_VERSION_40(client)) {
	if (mask & XIMP_FOCUS_WIN_MASK4) {
	    if (getFocusProperty(core, values))
	      values->attr_mask |= XIMP_FOCUS_WIN_MASK4;
	    else
	      return False;
	}
	if (mask & XIMP_PRE_FONT_MASK4) {
	    if (getPreeditFontProperty(core, values))
	      values->attr_mask |= XIMP_PRE_FONT_MASK4;
	    else
	      return False;
	}
	if (mask & XIMP_STS_FONT_MASK4) {
	    if (getStatusFontProperty(core, values))
	      values->attr_mask |= XIMP_STS_FONT_MASK4;
	    else
	      return False;
	}
	if (mask & PREEDIT_MASK4) {
	    if (getPreeditProperty4(core, values))
	      values->attr_mask |= mask & PREEDIT_MASK4;
	    else
	      return False;
	}
	if (mask & STATUS_MASK4) {
	    if (getStatusProperty4(core, values))
	      values->attr_mask |= mask & STATUS_MASK4;
	    else
	      return False;
	}
	if (mask & XIMP_SERVERTYPE_MASK4) {
	    if (getServerType4(core, values))
	      values->attr_mask |= mask & XIMP_SERVERTYPE_MASK4;
	    else
	      return False;
	}
    } else {
	if (mask & XIMP_FOCUS_WIN_MASK3) {
	    if (getFocusProperty(core, values))
	      values->attr_mask |= XIMP_FOCUS_WIN_MASK3;
	    else
	      return False;
	}
	if (mask & XIMP_PRE_FONT_MASK3) {
	    if (getPreeditFontProperty(core, values))
	      values->attr_mask |= XIMP_PRE_FONT_MASK3;
	    else
	      return False;
	}
	if (mask & XIMP_STS_FONT_MASK3) {
	    if (getStatusFontProperty(core, values))
	      values->attr_mask |= XIMP_STS_FONT_MASK3;
	    else
	      return False;
	}
	if (mask & PREEDIT_MASK3) {
	    if (getPreeditProperty3(core, values))
	      values->attr_mask |= mask & PREEDIT_MASK3;
	    else
	      return False;
	}
	if (mask & STATUS_MASK3) {
	    if (getStatusProperty3(core, values))
	      values->attr_mask |= mask & STATUS_MASK3;
	    else
	      return False;
	}
    }
    return True;
}

Bool
#if NeedFunctionPrototypes
_XimpSetProperty(XIMPCore core, XimpClient *client,
		 XIMPICValuesStruct *values)
#else
_XimpSetProperty(core, client, values)
XIMPCore core;
XimpClient *client;
XIMPICValuesStruct *values;
#endif
{
    long mask = values->attr_mask;

    if (IS_VERSION_40(client)) {
	if (mask & XIMP_FOCUS_WIN_MASK4) {
	    if (setFocusProperty(core, values))
	      values->attr_mask |= XIMP_FOCUS_WIN_MASK4;
	    else
	      return False;
	}
	if (mask & XIMP_PRE_FONT_MASK4) {
	    if (setPreeditFontProperty(core, values))
	      values->attr_mask |= XIMP_PRE_FONT_MASK4;
	    else
	      return False;
	}
	if (mask & XIMP_STS_FONT_MASK4) {
	    if (setStatusFontProperty(core, values))
	      values->attr_mask |= XIMP_STS_FONT_MASK4;
	    else
	      return False;
	}
	if (mask & PREEDIT_MASK4) {
	    if (setPreeditProperty4(core, values))
	      values->attr_mask |= mask & PREEDIT_MASK4;
	    else
	      return False;
	}
	if (mask & STATUS_MASK4) {
	    if (setStatusProperty4(core, values))
	      values->attr_mask |= mask & STATUS_MASK4;
	    else
	      return False;
	}
	if (mask & XIMP_SERVERTYPE_MASK4) {
	    if (setServerType4(core, values))
	      values->attr_mask |= mask & XIMP_SERVERTYPE_MASK4;
	    else
	      return False;
	}
    } else {
	if (mask & XIMP_FOCUS_WIN_MASK3) {
	    if (setFocusProperty(core, values))
	      values->attr_mask |= XIMP_FOCUS_WIN_MASK3;
	    else
	      return False;
	}
	if (mask & XIMP_PRE_FONT_MASK3) {
	    if (setPreeditFontProperty(core, values))
	      values->attr_mask |= XIMP_PRE_FONT_MASK3;
	    else
	      return False;
	}
	if (mask & XIMP_STS_FONT_MASK3) {
	    if (setStatusFontProperty(core, values))
	      values->attr_mask |= XIMP_STS_FONT_MASK3;
	    else
	      return False;
	}
	if (mask & PREEDIT_MASK3) {
	    if (setPreeditProperty3(core, values))
	      values->attr_mask |= mask & PREEDIT_MASK3;
	    else
	      return False;
	}
	if (mask & STATUS_MASK3) {
	    if (setStatusProperty3(core, values))
	      values->attr_mask |= mask & STATUS_MASK3;
	    else
	      return False;
	}
    }
    return True;
}
#endif	/* _XIMPATTR_C_ */
