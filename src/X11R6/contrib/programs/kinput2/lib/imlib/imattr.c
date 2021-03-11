#ifndef lint
static char *rcsid = "$Id: imattr.c,v 1.7 1994/06/02 02:21:14 ishisone Exp $";
#endif
/*
 * Copyright (c) 1994  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

#include "im.h"

#ifndef XNSeparatorofNestedList
#define XNSeparatorofNestedList "separatorofNesttedList"
#endif

#define PAD4(n)	((((n) + 3) / 4) * 4)

/*
 * List of supported input styles.
 */

typedef struct {
    XIMStyle xim_style;		/* X11R5 spec. */
    int conversion_style;	/* kinput2 spec. */
} InputStyle;

static InputStyle styles[] = {
    { XIMPreeditPosition|XIMStatusArea, IMSTYLE_OVER_THE_SPOT },
    { XIMPreeditPosition|XIMStatusNothing, IMSTYLE_OVER_THE_SPOT },
    { XIMPreeditArea|XIMStatusArea, IMSTYLE_OFF_THE_SPOT },
    { XIMPreeditNothing|XIMStatusNothing, IMSTYLE_SEPARATE },
    { 0 },
};

#define NEST_NONE	0
#define NEST_PREEDIT	1
#define NEST_STATUS	2

#define CHECK_ICATTR_SIZE(validsize, code) \
	if (len != validsize) { badSizeError(icp, code); return -1; }

#undef OP_C
#undef OP_S
#undef OP_G

#define OP_C	1	/* Create */
#define OP_S	2	/* SetValues */
#define OP_G	4	/* GetValues */

typedef struct {
    char *name;		/* attribute name */
    int type;		/* type of attribute value */
    int valid_ops;	/* valid operations for this attribute */
    int (*set_proc) _Pt_((IMIM *, char *, int));
    int (*get_proc) _Pt_((IMIM *, unsigned int, int));
} IMAttribute;

typedef struct {
    char *name;		/* attribute name */
    int type;		/* type of attribute value */
    int valid_ops;	/* valid operations for this attribute */
    int (*set_proc) _Pt_((IMIC *, char *, int, int, int, int));
    int (*get_proc) _Pt_((IMIC *, unsigned int, int, int, char *, int));
} ICAttribute;


/*
 * IM attributes
 */

static int getQueryInputStyle _Pt_((IMIM *imp, unsigned int id, int offset));

static IMAttribute imAttributes[] = {
    { XNQueryInputStyle, TYPE_XIM_STYLES, OP_G,
	  NULL, getQueryInputStyle },
};

static int numImAttributes = XtNumber(imAttributes);


/*
 * IC attributes
 */

static int setInputStyle _Pt_((IMIC *, char *, int, int, int, int));
static int setClientWindow _Pt_((IMIC *, char *, int, int, int, int));
static int setFocusWindow _Pt_((IMIC *, char *, int, int, int, int));
static int setPreeditAttributes _Pt_((IMIC *, char *, int, int, int, int));
static int setStatusAttributes _Pt_((IMIC *, char *, int, int, int, int));
static int setArea _Pt_((IMIC *, char *, int, int, int, int));
static int setAreaNeeded _Pt_((IMIC *, char *, int, int, int, int));
static int setForeground _Pt_((IMIC *, char *, int, int, int, int));
static int setBackground _Pt_((IMIC *, char *, int, int, int, int));
static int setColormap _Pt_((IMIC *, char *, int, int, int, int));
static int setBgPixmap _Pt_((IMIC *, char *, int, int, int, int));
static int setLineSpace _Pt_((IMIC *, char *, int, int, int, int));
static int setCursor _Pt_((IMIC *, char *, int, int, int, int));
static int setSpotLocation _Pt_((IMIC *, char *, int, int, int, int));
static int setStdColormap _Pt_((IMIC *, char *, int, int, int, int));
static int setFontSet _Pt_((IMIC *, char *, int, int, int, int));

static int getPreeditAttributes _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getStatusAttributes _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getInputStyle _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getClientWindow _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getFocusWindow _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getArea _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getAreaNeeded _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getSpotLocation _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getColormap _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getStdColormap _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getForeground _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getBackground _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getBgPixmap _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getFontSet _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getLineSpace _Pt_((IMIC *, unsigned int, int, int, char *, int));
static int getCursor _Pt_((IMIC *, unsigned int, int, int, char *, int));

static ICAttribute icAttributes[] = {
    { XNInputStyle, TYPE_CARD32, OP_C|OP_G,
	  setInputStyle, getInputStyle },
    { XNClientWindow, TYPE_WINDOW, OP_C|OP_G,
	  setClientWindow, getClientWindow },
    { XNFocusWindow, TYPE_WINDOW, OP_C|OP_S|OP_G,
	  setFocusWindow, getFocusWindow },
    { XNPreeditAttributes, TYPE_NESTED_LIST, OP_C|OP_S|OP_G,
	  setPreeditAttributes, getPreeditAttributes },
    { XNStatusAttributes, TYPE_NESTED_LIST, OP_C|OP_S|OP_G,
	  setStatusAttributes, getStatusAttributes },
    { XNArea, TYPE_XRECTANGLE, OP_C|OP_S|OP_G,
	  setArea, getArea },
    { XNAreaNeeded, TYPE_XRECTANGLE, OP_C|OP_S|OP_G,
	  setAreaNeeded, getAreaNeeded },
    { XNSpotLocation, TYPE_XPOINT, OP_C|OP_S|OP_G,
	  setSpotLocation, getSpotLocation },
    { XNColormap, TYPE_CARD32, OP_C|OP_S|OP_G,
	  setColormap, getColormap },
    { XNStdColormap, TYPE_CARD32, OP_C|OP_S|OP_G,
	  setStdColormap, getStdColormap },
    { XNForeground, TYPE_CARD32, OP_C|OP_S|OP_G,
	  setForeground, getForeground },
    { XNBackground, TYPE_CARD32, OP_C|OP_S|OP_G,
	  setBackground, getBackground },
    { XNBackgroundPixmap, TYPE_CARD32, OP_C|OP_S|OP_G,
	  setBgPixmap, getBgPixmap },
    { XNFontSet, TYPE_XFONTSET, OP_C|OP_S|OP_G,
	  setFontSet, getFontSet },
    { XNLineSpace, TYPE_CARD16, OP_C|OP_S|OP_G,	  /* should be TYPE_INT16 */
	  setLineSpace, getLineSpace },
    { XNCursor, TYPE_CARD32, OP_C|OP_S|OP_G,
	  setCursor, getCursor },
    { XNSeparatorofNestedList, TYPE_SEPARATOR, OP_G,
	  NULL, NULL },
};

static int numIcAttributes = XtNumber(icAttributes);


static unsigned int getC16 _Pt_((char *data, int order));
static int getI16 _Pt_((char *data, int order));
static unsigned long getC32 _Pt_((char *data, int order));
static int validateClientWindow _Pt_((IMIC *icp));
static int validateFocusWindow _Pt_((IMIC *icp));
static void badSizeError _Pt_((IMIC *icp, int code));
static void unnestedError _Pt_((IMIC *icp));
static IMPSAttributes *getPSPtr _Pt_((IMIC *icp, int type));
static int getIMValues _Pt_((IMIM *imp, char *data, int len, int offset));
static int getICValues _Pt_((IMIC *icp, char *data, int len, int nest,
			     int offset, int *sepp));
static int setICValues _Pt_((IMIC *icp, char *data, int len,
			     int major, int op));
static int getPSAttributes _Pt_((IMIC *icp, unsigned int id, int nest,
				 int offset, char *data, int len));
static void changeFonts _Pt_((IMIC *icp));
static void fillCommonDefault _Pt_((IMIC *icp, unsigned long mask));
static int getNaturalLineSpace _Pt_((IMIC *icp));
static void fillPSDefault _Pt_((IMIC *icp, int type, unsigned long mask));
static int validateCommonAttr _Pt_((IMIC *icp, int checkonly));
static int validatePSAttr _Pt_((IMIC *icp, int type, int checkonly));
static void changeConversionAttributes _Pt_((IMIC *icp));
static void computeAreaNeeded _Pt_((IMIC *icp));
static void computeAreaForQuery _Pt_((IMIC *icp));


/*
 * Functions reading out numbers from byte buffer
 */

static unsigned int
getC16(data, order)
char *data;
int order;
{
    unsigned char *p = (unsigned char *)data;
    unsigned int x;

    x = (order == ORDER_BIG) ? ((p[0] << 8) | p[1]) : (p[0] | p[1] << 8);
    return x;
}

static int
getI16(data, order)
char *data;
int order;
{
    unsigned char *p = (unsigned char *)data;
    long l;

    l = (order == ORDER_BIG) ? ((p[0] << 8) | p[1]) : (p[0] | p[1] << 8);
    return (l < 32768) ? (int)l : (int)(l - 65536L);
}

static unsigned long
getC32(data, order)
char *data;
int order;
{
    unsigned char *p = (unsigned char *)data;
    unsigned long x;

    if (order == ORDER_BIG) {
	x = (p[0]<<24) | (p[1]<<16) | (p[2]<<8) | p[3];
    } else {
	x = p[0] | (p[1]<<8) | (p[2]<<16) | (p[3]<<24);
    }
    return x;
}


/*
 * Functions that check the validity of resources.
 */

static int
validateClientWindow(icp)
IMIC *icp;
{
    return IMValidateWindow(XtDisplay(icp->im->connection->proto_widget),
			    icp->common_attr.client, &icp->client_profile);
}

static int
validateFocusWindow(icp)
IMIC *icp;
{
    IMCommonAttributes *ap = &icp->common_attr;

    /*
     * This function assumes that the client window has already
     * been validated.
     */
    if ((ap->set_mask & ATTR_MASK_CLIENT) && ap->focus == ap->client) {
	icp->focus_profile = icp->client_profile;
	return 1;
    } else {
	return IMValidateWindow(XtDisplay(icp->im->connection->proto_widget),
				icp->common_attr.focus, &icp->focus_profile);
    }
}


/*
 * Functions submit errors
 */

static void
badSizeError(icp, code)
IMIC *icp;
int code;
{
    DPRINT(("bad size error for IC #%d\n", icp->id));
    IMSendError(icp->im->connection, code, icp->im->id, icp->id,
		"invalid size of attribute value");
}

static void
unnestedError(icp)
IMIC *icp;
{
    DPRINT(("unnested error for IC #%d\n", icp->id));
    IMSendError(icp->im->connection, IMBadSomething, icp->im->id, icp->id,
		"either preedit or status specification required");
}


/*
 * Functions getting IM attributes
 */

static IMPSAttributes *
getPSPtr(icp, type)
IMIC *icp;
int type;
{
    switch (type) {
    case NEST_PREEDIT: return &icp->preedit_attr;
    case NEST_STATUS: return &icp->status_attr;
    default: return NULL;
    }
}

static int
getIMValues(imp, data, len, offset)
IMIM *imp;
char *data;
int len;
int offset;		/* request offset */
{
    unsigned int id;		/* attribute ID */
    IMAttribute *attrp;
    IMConnection *conn = imp->connection;
    int byte_order = conn->byte_order;

    while (len >= 2) {
	id = getC16(data, byte_order);
	data += 2;
	len -= 2;

	if (id > numImAttributes) {
	    /* invalid attribute ID */
	    IMCancelRequest(conn, offset);
	    IMSendError(conn, IMBadSomething, imp->id, 0,
			"invalid IM attribute ID");
	    return -1;
	}

	attrp = &imAttributes[id];
	if (!(attrp->valid_ops & OP_G)) {
	    IMCancelRequest(conn, offset);
	    IMSendError(conn, IMBadSomething, imp->id, 0,
			"invalid operation (IMGetValues) for this attribute");
	    return -1;
	}

	if ((*attrp->get_proc)(imp, id, offset) < 0) return -1;
    }
    return 0;
}

/* ARGSUSED */
static int
getQueryInputStyle(imp, id, offset)
IMIM *imp;
unsigned int id;
int offset;
{
    IMConnection *conn = imp->connection;
    unsigned int num_styles, num_bytes;
    InputStyle *stp;

    TRACE(("imlib:getQueryInputStyle()\n"));

    for (num_styles = 0, stp = styles; stp->xim_style != 0; stp++) {
	num_styles++;
    }
    num_bytes = num_styles * 4 + 4;
    IMPutC16(conn, id);
    IMPutC16(conn, num_bytes);
    IMPutC16(conn, num_styles);
    IMPutC16(conn, 0);
    for (stp = styles; stp->xim_style != 0; stp++) {
	IMPutC32(conn, stp->xim_style);
    }
    return 0;
}


/*
 * Functions setting IC attributes
 */

static int
setICValues(icp, data, len, nest, op)
IMIC *icp;
char *data;
int len;
int nest;
int op;
{
    IMConnection *conn = icp->im->connection;
    unsigned int imid = icp->im->id;
    unsigned int icid = icp->id;
    unsigned int id;
    unsigned int value_len;
    unsigned int attr_len;
    char *value;
    ICAttribute *attrp;
    int byte_order = icp->im->connection->byte_order;

    TRACE(("imlib:setICValues()\n"));

    while (len > 0) {
	if (len < 4) {
	    DPRINT(("attribute data length < 4\n"));
	    IMSendError(conn, IMBadSomething, imid, icid, "Bad attribute data");
	    return -1;
	}
	id = getC16(data, byte_order);
	value_len = getC16(data + 2, byte_order);
	attr_len = PAD4(4 + value_len);

	if (attr_len > len) {
	    DPRINT(("attribute data length > request length\n"));
	    IMSendError(conn, IMBadSomething, imid, icid,
			"Bad attribute length");
	    return -1;
	}
	value = data + 4;

	if (id > numIcAttributes) {
	    DPRINT(("invalid IC attribute ID %d\n", id));
	    IMSendError(conn, IMBadSomething, imid, icid,
			"invalid IC attribute ID");
	    return -1;
	}
	attrp = &icAttributes[id];
	if (!(attrp->valid_ops & op)) {
	    DPRINT(("invalid operation (%s) for IC attr %d\n",
		    op == OP_C ? "create" : "set", id));
	    IMSendError(conn, IMBadSomething, imid, icid,
			"invalid operation for this attribute");
	    return -1;
	}

	/*
	 * Call attribute set procedure.
	 */
	if ((*attrp->set_proc)(icp, value, (int)value_len, byte_order, nest, op) < 0) {
	    /*
	     * Error has occured.  The set procedure has already sent
	     * appropriate error message, so just return here.
	     */
	    return -1;
	}

	data += attr_len;
	len -= attr_len;
    }
    return 0;
}

/* ARGSUSED */
static int
setInputStyle(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    TRACE(("imlib:setInputStyle()\n"));

    CHECK_ICATTR_SIZE(4, IMBadStyle);

    /*
     * InputStyle must be set with CreateIC.
     */
    if (op != OP_C) {
	DPRINT(("trying to change input style through SetICValues\n"));
	IMSendError(icp->im->connection, IMBadStyle,
		    icp->im->id, icp->id,
		    "InputStyle cannot be changed by SetICValues");
	return -1;
    }

    icp->common_attr.input_style = (XIMStyle)getC32(value, order);
    icp->common_attr.set_mask |= ATTR_MASK_INPUT_STYLE;
    icp->common_attr.change_mask |= ATTR_MASK_INPUT_STYLE;
    TRACE(("\tinput style: %ld\n", icp->common_attr.input_style));
    return 0;
}

/* ARGSUSED */
static int
setClientWindow(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    TRACE(("imlib:setClientWindow()\n"));

    CHECK_ICATTR_SIZE(4, IMBadClientWindow);

    /*
     * ClientWindow cannot be changed.
     */
    if (icp->common_attr.set_mask & ATTR_MASK_CLIENT) {
	DPRINT(("client window already specified\n"));
	IMSendError(icp->im->connection, IMBadClientWindow,
		    icp->im->id, icp->id, "ClientWindow already set");
	return -1;
    }

    icp->common_attr.client = (Window)getC32(value, order);
    TRACE(("\tclient window: %08lx\n", icp->common_attr.client));

    icp->common_attr.set_mask |= ATTR_MASK_CLIENT;
    icp->common_attr.change_mask |= ATTR_MASK_CLIENT;

    return 0;
}

/* ARGSUSED */
static int
setFocusWindow(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    Window focus;

    TRACE(("imlib:setFocusWindow()\n"));

    CHECK_ICATTR_SIZE(4, IMBadFocusWindow);

    focus = (Window)getC32(value, order);
    TRACE(("\tfocus window: %08lx\n", focus));

    if (!(icp->common_attr.set_mask & ATTR_MASK_FOCUS) ||
	focus != icp->common_attr.focus) {
	icp->common_attr.change_mask |= ATTR_MASK_FOCUS;
    }
    icp->common_attr.focus = focus;
    icp->common_attr.set_mask |= ATTR_MASK_FOCUS;
    return 0;
}

/* ARGSUSED */
static int
setPreeditAttributes(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    TRACE(("imlib:setPreeditAttributes()\n"));
    return setICValues(icp, value, len, NEST_PREEDIT, op);
}

/* ARGSUSED */
static int
setStatusAttributes(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    TRACE(("imlib:setStatusAttributes()\n"));
    return setICValues(icp, value, len, NEST_STATUS, op);
}

/* ARGSUSED */
static int
setArea(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    IMPSAttributes *ap;
    XRectangle area;

    TRACE(("imlib:setArea()\n"));

    CHECK_ICATTR_SIZE(8, IMBadArea);

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	unnestedError(icp);
	return -1;
    }

    area.x = getI16(value, order);
    area.y = getI16(value + 2, order);
    area.width = getC16(value + 4, order);
    area.height = getC16(value + 6, order);
    TRACE(("\tarea: %d, %d, %d, %d\n",
	   area.x, area.y, area.width, area.height));

    if (!(ap->set_mask & ATTR_MASK_AREA) ||
	area.x != ap->area.x ||
	area.y != ap->area.y ||
	area.width != ap->area.width ||
	area.height != ap->area.height) {
	ap->change_mask |= ATTR_MASK_AREA;
    }

    ap->area.x = area.x;
    ap->area.y = area.y;
    ap->area.width = area.width;
    ap->area.height = area.height;
    ap->set_mask |= ATTR_MASK_AREA;

    return 0;
}

/* ARGSUSED */
static int
setAreaNeeded(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    IMPSAttributes *ap;
    XRectangle area;

    TRACE(("imlib:setAreaNeeded()\n"));

    CHECK_ICATTR_SIZE(8, IMBadArea);

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	unnestedError(icp);
	return -1;
    }

    area.width = getC16(value + 4, order);
    area.height = getC16(value + 6, order);
    TRACE(("\tarea needed: %d, %d\n", area.width, area.height));

    if (!(ap->set_mask & ATTR_MASK_AREA_NEEDED) ||
	area.width != ap->area_needed.width ||
	area.height != ap->area_needed.height) {
	ap->change_mask |= ATTR_MASK_AREA_NEEDED;
    }

    ap->area_needed.width = area.width;
    ap->area_needed.height = area.height;
    ap->set_mask |= ATTR_MASK_AREA_NEEDED;

    return 0;
}

/* ARGSUSED */
static int
setForeground(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    IMPSAttributes *ap;
    Pixel fore;

    TRACE(("imlib:setForeground()\n"));

    CHECK_ICATTR_SIZE(4, IMBadForeground);

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	unnestedError(icp);
	return -1;
    }

    fore = getC32(value, order);
    TRACE(("\tforeground: %ld\n", fore));

    if (!(ap->set_mask & ATTR_MASK_FOREGROUND) || fore != ap->foreground) {
	ap->change_mask |= ATTR_MASK_FOREGROUND;
    }
    ap->foreground = fore;
    ap->set_mask |= ATTR_MASK_FOREGROUND;
    return 0;
}

/* ARGSUSED */
static int
setBackground(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    IMPSAttributes *ap;
    Pixel back;

    TRACE(("imlib:setBackground()\n"));

    CHECK_ICATTR_SIZE(4, IMBadBackground);

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	unnestedError(icp);
	return -1;
    }

    back = getC32(value, order);
    TRACE(("\tbackground: %ld\n", back));

    if (!(ap->set_mask & ATTR_MASK_BACKGROUND) || back != ap->background) {
	ap->change_mask |= ATTR_MASK_BACKGROUND;
    }
    ap->background = back;
    ap->set_mask |= ATTR_MASK_BACKGROUND;
    return 0;
}

/* ARGSUSED */
static int
setColormap(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    IMPSAttributes *ap;
    Colormap cmap;

    TRACE(("imlib:setColormap()\n"));

    CHECK_ICATTR_SIZE(4, IMBadColormap);

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	unnestedError(icp);
	return -1;
    }

    cmap = getC32(value, order);
    TRACE(("\tcolormap: %08lx\n", cmap));

    if (!(ap->set_mask & ATTR_MASK_COLORMAP) || cmap != ap->colormap) {
	ap->change_mask |= ATTR_MASK_COLORMAP;
    }
    ap->colormap = cmap;
    ap->set_mask |= ATTR_MASK_COLORMAP;
    return 0;
}

/* ARGSUSED */
static int
setBgPixmap(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    IMPSAttributes *ap;
    Pixmap pixmap;

    TRACE(("imlib:setBgPixmap()\n"));

    CHECK_ICATTR_SIZE(4, IMBadPixmap);

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	unnestedError(icp);
	return -1;
    }

    pixmap = getC32(value, order);
    TRACE(("\tbackground pixmap: %08lx\n", pixmap));

    if (!(ap->set_mask & ATTR_MASK_BG_PIXMAP) || pixmap != ap->bg_pixmap) {
	ap->change_mask |= ATTR_MASK_BG_PIXMAP;
    }

    ap->bg_pixmap = pixmap;
    ap->set_mask |= ATTR_MASK_BG_PIXMAP;

    return 0;
}

/* ARGSUSED */
static int
setLineSpace(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    IMPSAttributes *ap;
    int line_space;

    TRACE(("imlib:setLineSpace()\n"));

    CHECK_ICATTR_SIZE(4, IMBadSomething);

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	unnestedError(icp);
	return -1;
    }

    line_space = getI16(value, order);	/* ??? linespacing is 'int' */
    TRACE(("\tline space: %d\n", line_space));

    if (!(ap->set_mask & ATTR_MASK_LINESPACE) ||
	line_space != ap->line_space) {
	ap->change_mask |= ATTR_MASK_LINESPACE;
    }
    ap->line_space = line_space;
    ap->set_mask |= ATTR_MASK_LINESPACE;
    return 0;
}

/* ARGSUSED */
static int
setCursor(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    IMPSAttributes *ap;
    Cursor cursor;

    TRACE(("imlib:setCursor()\n"));

    CHECK_ICATTR_SIZE(4, IMBadCursor);

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	unnestedError(icp);
	return -1;
    }

    cursor = getC32(value, order);
    TRACE(("\tcursor: %08lx\n", cursor));

    if (!(ap->set_mask & ATTR_MASK_CURSOR) || cursor != ap->cursor) {
	ap->change_mask |= ATTR_MASK_CURSOR;
    }
    ap->cursor = cursor;
    ap->set_mask |= ATTR_MASK_CURSOR;
    return 0;
}

/* ARGSUSED */
static int
setSpotLocation(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    IMPSAttributes *ap;
    XPoint spot;

    TRACE(("imlib:setSpotLocation()\n"));

    CHECK_ICATTR_SIZE(4, IMBadSpotLocation);

    if (nest == NEST_STATUS) {
	DPRINT(("spot location specified in a status attribute list\n"));
	IMSendError(icp->im->connection, IMBadSpotLocation,
		    icp->im->id, icp->id,
		    "spot location isn't a status attribute");
	return -1;
    }

    ap = &icp->preedit_attr;

    spot.x = getI16(value, order);
    spot.y = getI16(value + 2, order);
    TRACE(("\tspot location: %d, %d\n", spot.x, spot.y));

    if (!(ap->set_mask & ATTR_MASK_SPOT_LOCATION) ||
	spot.x != ap->spot_location.x || spot.y != ap->spot_location.y) {
	ap->change_mask |= ATTR_MASK_SPOT_LOCATION;
    }
    ap->spot_location.x = spot.x;
    ap->spot_location.y = spot.y;
    ap->set_mask |= ATTR_MASK_SPOT_LOCATION;
    return 0;
}

/* ARGSUSED */
static int
setStdColormap(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    IMPSAttributes *ap;
    Atom colormap_name;
    XStandardColormap *stdcolormaps;
    Widget w = icp->im->connection->proto_widget;
    Display *dpy = XtDisplay(w);
    int ncolormaps;
    Window root;
    XAEHandle h;
    int status;

    TRACE(("imlib:setStdColormap()\n"));

    CHECK_ICATTR_SIZE(4, IMBadAtom);

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	unnestedError(icp);
	return -1;
    }

    colormap_name = getC32(value, order);
    if (icp->common_attr.set_mask & ATTR_MASK_CLIENT) {
	root = icp->client_profile.root;
    } else if (icp->common_attr.set_mask & ATTR_MASK_FOCUS) {
	root = icp->focus_profile.root;
    } else {
	/*
	 * Client has not specified client window yet.
	 * Reading standard colormap property should been deffered
	 * until the window is set, but for now...
	 */
	DDPRINT(2, ("std colormap specified, leaving client window unspecified\n"));
	root = RootWindowOfScreen(XtScreen(w));
    }

    h = XAESetIgnoreErrors(dpy);
    status = XGetRGBColormaps(dpy, root,
			      &stdcolormaps, &ncolormaps, colormap_name);
    XAEUnset(h);
    if (!status || ncolormaps < 0) {
	DPRINT(("can't get standard colormap (%ld)\n", colormap_name));
	IMSendError(icp->im->connection, IMBadName, icp->im->id, icp->id,
		"invalid standard colormap name");
	return -1;
    }

    if (!(ap->set_mask & ATTR_MASK_STD_COLORMAP) ||
	colormap_name != ap->std_colormap) {
	ap->change_mask |= ATTR_MASK_STD_COLORMAP;
    }
    ap->std_colormap = colormap_name;
    ap->colormap = stdcolormaps[0].colormap;
    TRACE(("\tstandard colormap: %ld (colormap=%08lx)\n",
	   colormap_name, ap->colormap));

    ap->set_mask |= ATTR_MASK_STD_COLORMAP | ATTR_MASK_COLORMAP;
    XFree((char *)stdcolormaps);
    return 0;
}

/* ARGSUSED */
static int
setFontSet(icp, value, len, order, nest, op)
IMIC *icp;
char *value;
int len;
int order;
int nest;
int op;
{
    IMPSAttributes *ap;
    unsigned int name_list_len;
    char *name_list;

    TRACE(("imlib:setFontSet()\n"));

    if (len < 2) {
	badSizeError(icp, IMBadName);
	return -1;
    }
    name_list_len = getC16(value, order);
    if (2 + name_list_len > len) {
	badSizeError(icp, IMBadName);
	return -1;
    }

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	unnestedError(icp);
	return -1;
    }

    name_list = XtMalloc(name_list_len + 1);
    bcopy(value + 2, name_list, name_list_len);
    name_list[name_list_len] = '\0';
    TRACE(("\tfontset: %s\n", name_list));

    if (ap->set_mask & ATTR_MASK_FONT_SET) {
	if (!strcmp(name_list, ap->font_set)) {
	    XtFree(name_list);
	} else {
	    ap->change_mask |= ATTR_MASK_FONT_SET;
	    if (ap->font_set != IMDefaultFontSet(icp->im)) {
		XtFree(ap->font_set);
	    }
	    ap->font_set = name_list;
	}
    } else {
	ap->font_set = name_list;
	ap->set_mask |= ATTR_MASK_FONT_SET;
	ap->change_mask |= ATTR_MASK_FONT_SET;
    }
    return 0;
}


/*
 * Functions getting IC attributes
 */

static int
getICValues(icp, data, len, nest, offset, sepp)
IMIC *icp;
char *data;
int len;
int nest;		/* NEST_NONE, NEST_PREEDIT or NEST_STATUS */
int offset;		/* request offset */
int *sepp;		/* Out: true if ended with a nested list separator */
{
    unsigned int id;		/* attribute ID */
    ICAttribute *attrp;
    IMConnection *conn = icp->im->connection;
    int byte_order = conn->byte_order;
    char *org_data = data;
    int r;

    TRACE(("imlib:getICValues()\n"));

    while (len >= 2) {
	id = getC16(data, byte_order);
	data += 2;
	len -= 2;

	if (id > numIcAttributes) {
	    /* invalid attribute ID */
	    DPRINT(("invalid IC attribute ID (%d) specified\n", id));
	    IMCancelRequest(conn, offset);
	    IMSendError(conn, IMBadSomething, icp->im->id, icp->id,
			"invalid IC attribute ID");
	    return -1;
	}

	attrp = &icAttributes[id];
	if (attrp->type == TYPE_SEPARATOR) {
	    /* nested list separator */
	    *sepp = 1;
	    return data - org_data;
	}

	if (!(attrp->valid_ops & OP_G)) {
	    DPRINT(("invalid operation (get) for IC attr %d\n", id));
	    IMCancelRequest(conn, offset);
	    IMSendError(conn, IMBadSomething, icp->im->id, icp->id,
			"invalid operation (ICGetValues) for this attribute");
	    return -1;
	}

	r = (*attrp->get_proc)(icp, id, nest, offset, data, len);
	/*
	 * The return value of get_proc is usually 0, indicating success.
	 * If it is less than 0, there are some errors.
	 * If it is greater than 0,
	 */
	if (r < 0) return -1;

	data += r;		/* r is extra offset */
	len -= r;
    }
    *sepp = 0;
    return data - org_data;
}

/* ARGSUSED */
static int
getPSAttributes(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    unsigned int length;
    int length_offset;
    int attr_offset;
    int nested_separator;
    int r;

    IMPutC16(conn, id);

    length_offset = IMWritePos(conn);
    IMPutC16(conn, 0);		/* dummy -- overwritten afterwards */

    attr_offset = IMWritePos(conn);

    r = getICValues(icp, data, len, nest, offset, &nested_separator);
    if (r < 0) return -1;
    if (!nested_separator) {
	/* there's no nested list separator */
	DPRINT(("nested list doesn't end with separator\n"));
	/*
	 * X11R6 Xlib sends nested attribute list which has no
	 * separator at its end.  In order to accommodate to it,
	 * don't send error for that.
	 */
#ifdef notdef
	IMCancelRequest(conn, offset);
	IMSendError(conn, IMBadSomething, icp->im->id, icp->id,
		    "corrupted nested list");
	return -1;
#endif
    }

    /*
     * Nested list is written on the output buffer.
     * Calculate the length of the list.
     */
    length = IMWritePos(conn) - attr_offset;

    /* rewrite attribute length field */
    IMRewriteC16(conn, length_offset, length);
    IMPutPad(conn);

    return r;
}

/* ARGSUSED */
static int
getPreeditAttributes(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;		/* unused */
int offset;
char *data;
int len;
{
    TRACE(("imlib:getPreeditAttributes()\n"));
    return getPSAttributes(icp, id, NEST_PREEDIT, offset, data, len);
}

/* ARGSUSED */
static int
getStatusAttributes(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;		/* unused */
int offset;
char *data;
int len;
{
    TRACE(("imlib:getStatusAttributes()\n"));
    return getPSAttributes(icp, id, NEST_STATUS, offset, data, len);
}

/* ARGSUSED */
static int
getInputStyle(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;

    TRACE(("imlib:getInputStyle()\n"));

    /*
     * Input style must have been specified, (and validated)
     * at IC creation.  No need for checking.
     */
    IMPutC16(conn, id);		/* attribute ID */
    IMPutC16(conn, 4);		/* value length */
    IMPutC32(conn, icp->common_attr.input_style);
    return 0;
}

/* ARGSUSED */
static int
getClientWindow(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;

    TRACE(("imlib:getClientWindow()\n"));

    if (icp->common_attr.set_mask & ATTR_MASK_CLIENT) {
	IMPutC16(conn, id);		/* attribute ID */
	IMPutC16(conn, 4);		/* value length */
	IMPutC32(conn, icp->common_attr.client);
	return 0;
    } else {
	/* no default is available */
	DPRINT(("getClientWindow without setting client window previously\n"));
	IMCancelRequest(conn, offset);
	IMSendError(conn, IMBadClientWindow, icp->im->id, icp->id,
		    "client window not specified yet");
	return -1;
    }
}

/* ARGSUSED */
static int
getFocusWindow(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;

    TRACE(("imlib:getFocusWindow()\n"));

    if (!(icp->common_attr.set_mask & ATTR_MASK_FOCUS)) {
	/* fill default value */
	fillCommonDefault(icp, (unsigned long)ATTR_MASK_FOCUS);
    }

    if (icp->common_attr.set_mask & ATTR_MASK_FOCUS) {
	IMPutC16(conn, id);		/* attribute ID */
	IMPutC16(conn, 4);		/* value length */
	IMPutC32(conn, icp->common_attr.focus);
	return 0;
    } else {
	/*
	 * Couldn't get the default value.  That is, neither
	 * focus window nor client window is specified yet.
	 */
	DPRINT(("getFocusWindow without setting focus/client window previously\n"));
	IMCancelRequest(conn, offset);
	IMSendError(conn, IMBadFocusWindow, icp->im->id, icp->id,
		    "neither of client/focus window not specified yet");
	return -1;
    }
}

/* ARGSUSED */
static int
getArea(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    IMPSAttributes *ap;

    TRACE(("imlib:getArea()\n"));

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	IMCancelRequest(conn, offset);
	unnestedError(icp);
	return -1;
    }

    if (!(ap->set_mask & ATTR_MASK_AREA)) {
	fillPSDefault(icp, nest, (unsigned long)ATTR_MASK_AREA);
    }

    IMPutC16(conn, id);		/* attribute ID */
    IMPutC16(conn, 8);		/* value length */
    IMPutI16(conn, ap->area.x);
    IMPutI16(conn, ap->area.y);
    IMPutC16(conn, ap->area.width);
    IMPutC16(conn, ap->area.height);
    return 0;
}

/* ARGSUSED */
static int
getAreaNeeded(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    IMPSAttributes *ap;

    TRACE(("imlib:getAreaNeeded()\n"));

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	IMCancelRequest(conn, offset);
	unnestedError(icp);
	return -1;
    }

    if (!(ap->set_mask & ATTR_MASK_AREA_NEEDED)) {
	fillPSDefault(icp, nest, (unsigned long)ATTR_MASK_AREA_NEEDED);	/* ??? */
    }

    TRACE(("\tarea needed: %d, %d, %d, %d\n",
	   ap->area_needed.x, ap->area_needed.y,
	   ap->area_needed.width, ap->area_needed.height));

    IMPutC16(conn, id);		/* attribute ID */
    IMPutC16(conn, 8);		/* value length */
    IMPutI16(conn, ap->area_needed.x);
    IMPutI16(conn, ap->area_needed.y);
    IMPutC16(conn, ap->area_needed.width);
    IMPutC16(conn, ap->area_needed.height);
    return 0;
}

/* ARGSUSED */
static int
getSpotLocation(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    IMPSAttributes *ap = &icp->preedit_attr;

    TRACE(("imlib:getSpotLocation()\n"));

    if (nest == NEST_STATUS) {
	IMCancelRequest(conn, offset);
	IMSendError(conn, IMBadSomething, icp->im->id, icp->id,
		    "spot location isn't a status attribute");
	return -1;
    }

    if (!(ap->set_mask & ATTR_MASK_SPOT_LOCATION)) {
	fillPSDefault(icp, NEST_PREEDIT,
		      (unsigned long)ATTR_MASK_SPOT_LOCATION);
    }

    IMPutC16(conn, id);		/* attribute ID */
    IMPutC16(conn, 4);		/* value length */
    IMPutI16(conn, ap->spot_location.x);
    IMPutI16(conn, ap->spot_location.y);
    return 0;
}

/* ARGSUSED */
static int
getColormap(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    IMPSAttributes *ap;

    TRACE(("imlib:getColormap()\n"));

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	IMCancelRequest(conn, offset);
	unnestedError(icp);
	return -1;
    }

    if (!(ap->set_mask & ATTR_MASK_COLORMAP)) {
	fillPSDefault(icp, nest, (unsigned long)ATTR_MASK_COLORMAP);
    }

    IMPutC16(conn, id);		/* attribute ID */
    IMPutC16(conn, 4);		/* value length */
    IMPutC32(conn, ap->colormap);
    return 0;
}

/* ARGSUSED */
static int
getStdColormap(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    IMPSAttributes *ap;
    Atom colormap_name;

    TRACE(("imlib:getStdColormap()\n"));

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	IMCancelRequest(conn, offset);
	unnestedError(icp);
	return -1;
    }

    if (ap->set_mask & ATTR_MASK_STD_COLORMAP) {
	colormap_name = ap->std_colormap;
    } else {
	/* what to do? */
	colormap_name = None;
	DPRINT(("client asks standard colormap, but not specified\n"));
    }

    IMPutC16(conn, id);		/* attribute ID */
    IMPutC16(conn, 4);		/* value length */
    IMPutC32(conn, colormap_name);
    return 0;
}

/* ARGSUSED */
static int
getForeground(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    IMPSAttributes *ap;

    TRACE(("imlib:getForeground()\n"));

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	IMCancelRequest(conn, offset);
	unnestedError(icp);
	return -1;
    }

    if (!(ap->set_mask & ATTR_MASK_FOREGROUND)) {
	fillPSDefault(icp, nest, (unsigned long)ATTR_MASK_FOREGROUND);
    }

    IMPutC16(conn, id);		/* attribute ID */
    IMPutC16(conn, 4);		/* value length */
    IMPutC32(conn, ap->foreground);
    return 0;
}

/* ARGSUSED */
static int
getBackground(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    IMPSAttributes *ap;

    TRACE(("imlib:getBackground()\n"));

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	IMCancelRequest(conn, offset);
	unnestedError(icp);
	return -1;
    }

    if (!(ap->set_mask & ATTR_MASK_BACKGROUND)) {
	fillPSDefault(icp, nest, (unsigned long)ATTR_MASK_BACKGROUND);
    }

    IMPutC16(conn, id);		/* attribute ID */
    IMPutC16(conn, 4);		/* value length */
    IMPutC32(conn, ap->background);
    return 0;
}

/* ARGSUSED */
static int
getBgPixmap(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    IMPSAttributes *ap;

    TRACE(("imlib:getBgPixmap()\n"));

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	IMCancelRequest(conn, offset);
	unnestedError(icp);
	return -1;
    }

    if (!(ap->set_mask & ATTR_MASK_BG_PIXMAP)) {
	fillPSDefault(icp, nest, (unsigned long)ATTR_MASK_BG_PIXMAP);
    }

    IMPutC16(conn, id);		/* attribute ID */
    IMPutC16(conn, 4);		/* value length */
    IMPutC32(conn, ap->bg_pixmap);
    return 0;
}

/* ARGSUSED */
static int
getFontSet(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    IMPSAttributes *ap;
    int name_len;

    TRACE(("imlib:getFontSet()\n"));

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	IMCancelRequest(conn, offset);
	unnestedError(icp);
	return -1;
    }

    if (!(ap->set_mask & ATTR_MASK_FONT_SET)) {
	fillPSDefault(icp, nest, (unsigned long)ATTR_MASK_FONT_SET);
    }

    name_len = strlen(ap->font_set);

    IMPutC16(conn, id);		/* attribute ID */

    IMPutC16(conn, (unsigned int)name_len);	/* value length */
    IMPutString(conn, ap->font_set, name_len);
    IMPutPad(conn);
    return 0;
}

/* ARGSUSED */
static int
getLineSpace(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    IMPSAttributes *ap;

    TRACE(("imlib:getLineSpace()\n"));

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	IMCancelRequest(conn, offset);
	unnestedError(icp);
	return -1;
    }

    if (!(ap->set_mask & ATTR_MASK_LINESPACE)) {
	fillPSDefault(icp, nest, (unsigned long)ATTR_MASK_LINESPACE);
    }

    IMPutC16(conn, id);		/* attribute ID */
    IMPutC16(conn, 4);	/* value length */
    IMPutC32(conn, (unsigned long)ap->line_space);
    return 0;
}

/* ARGSUSED */
static int
getCursor(icp, id, nest, offset, data, len)
IMIC *icp;
unsigned int id;
int nest;
int offset;
char *data;
int len;
{
    IMConnection *conn = icp->im->connection;
    IMPSAttributes *ap;

    TRACE(("imlib:getCursor()\n"));

    if ((ap = getPSPtr(icp, nest)) == NULL) {
	IMCancelRequest(conn, offset);
	unnestedError(icp);
	return -1;
    }

    if (!(ap->set_mask & ATTR_MASK_CURSOR)) {
	fillPSDefault(icp, nest, (unsigned long)ATTR_MASK_CURSOR);
    }

    IMPutC16(conn, id);		/* attribute ID */
    IMPutC16(conn, 4);		/* value length */
    IMPutC32(conn, ap->cursor);
    return 0;
}

static void
changeFonts(icp)
IMIC *icp;
{
    FontBank bank = IMFontBank(icp->im);

    TRACE(("imlib:changeFonts()\n"));

    /*
     * Unload previous fonts.
     */
    if (icp->num_fonts > 0) {
	FontBankFreeFonts(bank, icp->fonts, icp->num_fonts);
    }

    /*
     * Load new fonts and store them in the IC structure.
     */
    icp->fonts = FontBankGet(bank, icp->preedit_attr.font_set,
			     &icp->num_fonts);
}


/*
 * Functions computing default attribute values
 */

static void
fillCommonDefault(icp, mask)
IMIC *icp;
unsigned long mask;
{
    IMCommonAttributes *ap = &icp->common_attr;

    TRACE(("imlib:fillCommonDefault()\n"));

    /*
     * Don't bother with the attributes which have been set.
     */
    mask &= ~ap->set_mask;

    /*
     * The only attribute that have default value is FocusWindow.
     */
    if (mask & ATTR_MASK_FOCUS) {
	/* if ClientWindow is not set... no way */
	if (mask & ATTR_MASK_CLIENT) {
	    ap->focus = ap->client;
	    ap->set_mask |= ATTR_MASK_FOCUS;
	    icp->focus_profile = icp->client_profile;
	    TRACE(("\tdefault focus window: %08lx\n", ap->focus));
	}
    }
}

static int
getNaturalLineSpace(icp)
IMIC *icp;
{
    XFontStruct **fonts;
    int num_fonts;
    int max_ascent = 0, max_descent = 0;
    int i;

    changeFonts(icp);
    fonts = icp->fonts;
    num_fonts = icp->num_fonts;

    for (i = 0; i < num_fonts; i++) {
	XFontStruct *font = fonts[i];
	if (max_ascent < font->ascent) max_ascent = font->ascent;
	if (max_descent < font->descent) max_descent = font->descent;
    }

    if (max_ascent + max_descent < MIN_LINE_SPACING) {
	return MIN_LINE_SPACING;
    } else {
	return max_ascent + max_descent;
    }
}

static void
fillPSDefault(icp, type, mask)
IMIC *icp;
int type;	/* NEST_PREEDIT or NEST_STATUS */
unsigned long mask;
{
    IMPSAttributes *ap;
    IMConnection *conn = icp->im->connection;
    Widget pw = conn->proto_widget;
    int preedit;
#ifdef DEBUG
    char *typename = (type == NEST_PREEDIT) ? "preedit" : "status";
#endif

    TRACE(("imlib:fillPSDefault(%s)\n", typename));

    preedit = (type == NEST_PREEDIT);
    ap = preedit ? &icp->preedit_attr : &icp->status_attr;

    /*
     * Don't bother with the attributes which have been set.
     * But area_needed needs to be computed each time (to get
     * correct X and Y coordinates).
     */
    mask &= ~ap->set_mask | ATTR_MASK_AREA_NEEDED;

    if (mask & ATTR_MASK_AREA) {
	computeAreaForQuery(icp);
	ap->set_mask |= ATTR_MASK_AREA;
	DDPRINT(5, ("\tdefault %s area: %d,%d,%d,%d\n", typename,
		    ap->area.x, ap->area.y, ap->area.width, ap->area.height));
    }
    if (mask & ATTR_MASK_FOREGROUND) {
	ap->foreground = IMDefaultForeground(pw);
	ap->set_mask |= ATTR_MASK_FOREGROUND;
	DDPRINT(5, ("\tdefault %s foreground: %ld\n",
		    typename, ap->foreground));
    }
    if (mask & ATTR_MASK_BACKGROUND) {
	ap->background = IMDefaultBackground(pw);
	ap->set_mask |= ATTR_MASK_BACKGROUND;
	DDPRINT(5, ("\tdefault %s background: %ld\n",
		    typename, ap->background));
    }
    if (mask & ATTR_MASK_COLORMAP) {
	ap->colormap = pw->core.colormap;
	ap->set_mask |= ATTR_MASK_COLORMAP;
	DDPRINT(5, ("\tdefault %s colormap: %08lx\n",
		    typename, ap->colormap));
    }
    if (mask & ATTR_MASK_STD_COLORMAP) {
	/* you can't fill default. what to do? */
	DDPRINT(5, ("\tdefault %s std colormap: N/A\n", typename));
    }
    if (mask & ATTR_MASK_BG_PIXMAP) {
	ap->bg_pixmap = None;
	ap->set_mask |= ATTR_MASK_BG_PIXMAP;
	DDPRINT(5, ("\tdefault %s background pixmap: None\n", typename));
    }
    if (mask & ATTR_MASK_LINESPACE) {
	if (!(ap->set_mask & ATTR_MASK_FONT_SET)) {
	    fillPSDefault(icp, type, (unsigned long)ATTR_MASK_FONT_SET);
	}
	ap->line_space = getNaturalLineSpace(icp);
	ap->set_mask |= ATTR_MASK_LINESPACE;
	DDPRINT(5, ("\tdefault line space: %d\n", ap->line_space));
    }
    if (mask & ATTR_MASK_CURSOR) {
	ap->cursor = None;
	ap->set_mask |= ATTR_MASK_CURSOR;
	DDPRINT(5, ("\tdefault %s cursor: None\n", typename));
    }
    if (mask & ATTR_MASK_AREA_NEEDED) {
	computeAreaNeeded(icp);
	DDPRINT(5, ("\t%s area_needed: %d,%d,%d,%d\n", typename,
		    ap->area_needed.x, ap->area_needed.y,
		    ap->area_needed.width, ap->area_needed.height));
    }
    if (mask & ATTR_MASK_FONT_SET) {
	ap->font_set = IMDefaultFontSet(icp->im);
	ap->set_mask |= ATTR_MASK_FONT_SET;
	DDPRINT(5, ("\tdefault %s fontset: %s\n", typename, ap->font_set));
    }
    if (mask & ATTR_MASK_SPOT_LOCATION) {
	ap->spot_location.x = ap->spot_location.y = 0;
	ap->set_mask |= ATTR_MASK_SPOT_LOCATION;
	DDPRINT(5, ("\tdefault spot location: %d, %d\n",
		    ap->spot_location.x, ap->spot_location.y));
    }
}

/*
 * Function validating attribute values
 */

static int
validateCommonAttr(icp, checkonly)
IMIC *icp;
int checkonly;
{
    IMCommonAttributes *ap = &icp->common_attr;
    IMConnection *conn = icp->im->connection;
    unsigned long mask = ap->change_mask;
    int ret = 0;

    TRACE(("imlib:validateCommonAttr()\n"));

    mask &= ap->set_mask;

#define SENDERROR(code, msg) \
    if (!checkonly && ret == 0) { \
	IMSendError(conn, code, icp->im->id, icp->id, msg); ret = -1; \
    }

    if (mask & ATTR_MASK_INPUT_STYLE) {
	XIMStyle xstyle = icp->common_attr.input_style;
	InputStyle *isp = styles;

	while (isp->xim_style != 0) {
	    if (isp->xim_style == xstyle) break;
	    isp++;
	}
	if (isp->xim_style == 0) {
	    DPRINT(("unsupported input style\n"));
	    SENDERROR(IMBadStyle, "unsupported input style");
	} else {
	    icp->style = isp->conversion_style;
	}
    }
    if (mask & ATTR_MASK_CLIENT) {
	if (!validateClientWindow(icp)) {
	    DPRINT(("invalid client window ID\n"));
	    SENDERROR(IMBadClientWindow, "invalid client window ID");
	}
    }
    if (mask & ATTR_MASK_FOCUS) {
	if (!validateFocusWindow(icp)) {
	    DPRINT(("invalid focus window ID\n"));
	    SENDERROR(IMBadFocusWindow, "invalid focus window ID");
	}
    }

    return ret;
#undef SENDERROR
}

static int
validatePSAttr(icp, type, checkonly)
IMIC *icp;
int type;
int checkonly;
{
    /*
     * Check validity of preedit/status attribute values.
     * 'type' is either NEST_PREEDIT or NEST_STATUS, indicating
     * whether preedit or status attributes are to be checked.
     * 'mask' is the attribute mask to be checked.
     * If all the attributes are valid, this function return 0.
     * Otherwise it issues an error message for the first invalid
     * value detected, and returns -1.
     */
    IMPSAttributes *ap;
    IMConnection *conn = icp->im->connection;
    unsigned long mask;
    int preedit;
    int ret = 0;

    TRACE(("imlib:validatePSAttr()\n"));

    preedit = (type == NEST_PREEDIT);
    ap = preedit ? &icp->preedit_attr : &icp->status_attr;

    /* do not check unset attributes */
    mask = ap->change_mask & ap->set_mask;

#define SENDERROR(code, msg) \
    if (!checkonly && ret == 0) { \
	IMSendError(conn, code, icp->im->id, icp->id, msg); ret = -1; \
    }

    if (mask & ATTR_MASK_AREA) {
	if (ap->area.width == 0 || ap->area.height == 0) {
	    ap->set_mask &= ~ATTR_MASK_AREA;
	    DPRINT(("zero area width/height\n"));
	    SENDERROR(IMBadArea, "invalid area width/height");
	}
    }

#ifdef notdef
    if (mask & ATTR_MASK_COLORMAP) {
    }

    if (mask & ATTR_MASK_BG_PIXMAP) {
    }
#endif

    if (mask & ATTR_MASK_LINESPACE) {
	if (ap->line_space < MIN_LINE_SPACING) {
	    ap->set_mask &= ~ATTR_MASK_LINESPACE;
	    /*
	     * we don't send error message in this case, because
	     * there exist some applications which send invalid line
	     * spacing and we don't want to break them.
	     */
	    DPRINT(("line space too small %d\n", ap->line_space));
	}
    }

#ifdef notdef
    if (mask & ATTR_MASK_CURSOR) {
	/* How should we check it? */
    }
#endif

    if (mask & ATTR_MASK_AREA_NEEDED) {
    }
#ifdef notdef
    if (mask & ATTR_MASK_FONT_SET) {
    }
    if (mask & ATTR_MASK_SPOT_LOCATION) {
    }
#endif

    return ret;
#undef SENDERROR
}


/*
 * Functions to extract necessary attributes and make conversion argument
 */

static void
changeConversionAttributes(icp)
IMIC *icp;
{
    unsigned long mask;
    ConversionAttributes attr;

    TRACE(("imlib:changeConversionAttributes()\n"));
    mask = IMMakeConvAttributes(icp, &attr);
    CControlChangeAttributes(icp->conversion, mask, &attr);
}

static void
computeAreaNeeded(icp)
IMIC *icp;
{
    IMPSAttributes *pattr = &icp->preedit_attr;
    IMPSAttributes *sattr = &icp->status_attr;
    IMWindowProfile *cpr = &icp->client_profile;
    int width, height;
    int max_width, max_height;
    int default_status_width;
    int font_height;

    TRACE(("computeAreaNeeded()\n"));

    if (icp->style == IMSTYLE_SEPARATE) return;

    /*
     * Get the current dimension of the client window.
     */
    (void)validateClientWindow(icp);

    /*
     * Compute the dimensions of the status region.
     */
    fillPSDefault(icp, NEST_STATUS, (unsigned long)ATTR_MASK_LINESPACE);
    font_height = sattr->line_space + 2;
    max_width = max_height = 0;
    if (sattr->set_mask & ATTR_MASK_AREA_NEEDED) {
	max_width = sattr->area_needed.width;
	max_height = sattr->area_needed.height;
	TRACE(("\tstatus areaNeeded was: (%d,%d)\n", max_width, max_height));
    }

    default_status_width = IMStatusWidth(icp->im->connection->proto_widget);
    if (default_status_width > 0) {
	width = default_status_width;
    } else {
	width = cpr->width / 5;		/* wild guess */
	if (width < font_height * 3) {
	    width = font_height * 3;	/* another wild guess */
	}
    }
    height = font_height;
    
    if (max_width > 0 && width > max_width) width = max_width;
    if (max_height > 0 && height > max_height) height = max_height;
    if (width < MIN_AREA_WIDTH) width = MIN_AREA_WIDTH;
    if (height < MIN_AREA_HEIGHT) height = MIN_AREA_HEIGHT;
    
    sattr->area_needed.x = 0;
    sattr->area_needed.y = cpr->height - height;
    sattr->area_needed.width = width;
    sattr->area_needed.height = height;
    TRACE(("\tstatus areaNeeded is now: (%d, %d, %d, %d)\n",
	    sattr->area_needed.x, sattr->area_needed.y, width, height));

    /*
     * Compute the dimensions of the pre-edit region.
     */
    if (icp->style != IMSTYLE_OFF_THE_SPOT) return;

    fillPSDefault(icp, NEST_PREEDIT, (unsigned long)ATTR_MASK_LINESPACE);
    font_height = pattr->line_space + 2;
    max_width = max_height = 0;
    if (pattr->set_mask & ATTR_MASK_AREA_NEEDED) {
	max_width = pattr->area_needed.width;
	max_height = pattr->area_needed.height;
	TRACE(("\tpreedit areaNeeded was: (%d,%d)\n",
		max_width, max_height));
    }

    width = cpr->width - sattr->area_needed.width;
    height = font_height;

    if (max_width > 0 && width > max_width) width = max_width;
    if (max_height > 0 && height > max_height) height = max_height;
    if (width < MIN_AREA_WIDTH) width = MIN_AREA_WIDTH;
    if (height < MIN_AREA_HEIGHT) height = MIN_AREA_HEIGHT;

    pattr->area_needed.x = sattr->area_needed.width;
    pattr->area_needed.y = cpr->height - height;
    pattr->area_needed.width = width;
    pattr->area_needed.height = height;
    TRACE(("\tpreedit areaNeeded is now: (%d, %d, %d, %d)\n",
	    pattr->area_needed.x, pattr->area_needed.y, width, height));
}

static void
computeAreaForQuery(icp)
IMIC *icp;
{
    IMPSAttributes *pattr = &icp->preedit_attr;
    IMPSAttributes *sattr = &icp->status_attr;

    TRACE(("computeAreaForQuery()\n"));

    if (icp->style == IMSTYLE_SEPARATE) return;

    computeAreaNeeded(icp);

    if (!(pattr->set_mask & ATTR_MASK_AREA)) {
	if (icp->style == IMSTYLE_OVER_THE_SPOT) {
	    IMWindowProfile *fpr = &icp->focus_profile;

	    pattr->area.x = 0;
	    pattr->area.y = 0;
	    pattr->area.width = fpr->width;
	    pattr->area.height = fpr->height;
	} else {	/* IMSTYLE_OFF_THE_SPOT */
	    pattr->area = pattr->area_needed;
	}
    }

    if (!(sattr->set_mask & ATTR_MASK_AREA)) {
	sattr->area = sattr->area_needed;
    }
}


/*
 * Public functions
 */

/*- IMPutIMAttrList: write list of supported IM attributes to output buffer -*/
void
IMPutIMAttrList(imp)
IMIM *imp;
{
    IMConnection *conn = imp->connection;
    IMAttribute *iap;
    int offset, list_start, list_end;
    int n;

    TRACE(("IMPutIMAttrList()\n"));

    offset = IMWritePos(conn);
    IMPutC16(conn, 0);		/* dummy. overwritten afterwards */

    list_start = IMWritePos(conn);
    for (n = 0, iap = imAttributes; n < numImAttributes; n++, iap++) {
	int length;

	IMPutC16(conn, (unsigned int)n);
	IMPutC16(conn, (unsigned int)iap->type);
	length = strlen(iap->name);
	IMPutC16(conn, (unsigned int)length);
	IMPutString(conn, iap->name, length);
	IMPutPad(conn);
    }
    list_end = IMWritePos(conn);
    IMRewriteC16(conn, offset, (unsigned int)(list_end - list_start));
}

/*- IMPutICAttrList: write list of supported IC attributes to output buffer -*/
void
IMPutICAttrList(imp)
IMIM *imp;
{
    IMConnection *conn = imp->connection;
    ICAttribute *iap;
    int offset, list_start, list_end;
    int n;

    TRACE(("IMPutICAttrList()\n"));

    offset = IMWritePos(conn);
    IMPutC16(conn, 0);		/* dummy. overwritten afterwards */
    IMPutC16(conn, 0);		/* unused */

    list_start = IMWritePos(conn);
    for (n = 0, iap = icAttributes; n < numIcAttributes; n++, iap++) {
	int length;

	IMPutC16(conn, (unsigned int)n);
	IMPutC16(conn, (unsigned int)iap->type);
	length = strlen(iap->name);
	IMPutC16(conn, (unsigned int)length);
	IMPutString(conn, iap->name, length);
	IMPutPad(conn);
    }
    list_end = IMWritePos(conn);
    IMRewriteC16(conn, offset, (unsigned int)(list_end - list_start));
}

/* ARGSUSED */
int
IMSetIMValues(imp, data, len, major)
IMIM *imp;
char *data;
int len;
int major;
{
    TRACE(("IMSetIMValues(): not supported yet\n"));
    /* not supported yet */

    IMSendError(imp->connection, IMBadProtocol, imp->id, 0,
		"this protocol is not supported yet");
    return -1;
}

int
IMGetIMValues(imp, data, len, offset)
IMIM *imp;
char *data;
int len;
int offset;		/* request offset */
{
    IMConnection *conn = imp->connection;
    int pos, list_start, list_end;

    TRACE(("IMGetIMValues()\n"));

    pos = IMWritePos(conn);
    IMPutC16(conn, 0);		/* length of the list. to be overwritten. */

    list_start = IMWritePos(conn);

    if (getIMValues(imp, data, len, offset) < 0) return -1;

    list_end = IMWritePos(conn);
    IMRewriteC16(conn, pos, (unsigned int)(list_end - list_start));
    return 0;
}

int
IMSetICValues(icp, data, len, major)
IMIC *icp;
char *data;
int len;
int major;
{
    int r1, r2, r3;

    TRACE(("IMSetICValues()\n"));

    /* clear change mask */
    icp->common_attr.change_mask = 0;
    icp->preedit_attr.change_mask = 0;
    icp->status_attr.change_mask = 0;

    /* read the specified data and set attributes */
    r1 = setICValues(icp, data, len, 0,
		     (major == XIM_CREATE_IC) ? OP_C : OP_S);

    /* validate attributes */
    r2 = IMValidateICAttributes(icp, (r1 < 0));

    /*
     * If the operation is CREATE_IC, input style attribute must be set.
     */
    if (major == XIM_CREATE_IC &&
	!(icp->common_attr.set_mask & ATTR_MASK_INPUT_STYLE) &&
	r1 == 0 && r2 == 0) {
	DPRINT(("input style not specified by CreateIC\n"));
	IMSendError(icp->im->connection, IMBadSomething, icp->im->id, icp->id,
		    "inputStyle resource must be set");
	r3 = -1;
    } else {
	r3 = 0;
    }

    /* if conversion is taking place... */
    if (icp->state & IC_CONVERTING) {
	changeConversionAttributes(icp);
    }

    return (r1 < 0 || r2 < 0 || r3 < 0) ? -1 : 0;
}

int
IMGetICValues(icp, data, len, offset)
IMIC *icp;
char *data;
int len;
int offset;		/* request offset */
{
    int nested_separator;
    int r;
    IMConnection *conn = icp->im->connection;
    int pos, list_start, list_end;

    TRACE(("IMGetICValues()\n"));

    pos = IMWritePos(conn);
    IMPutC16(conn, 0);		/* dummy. overwritten afterwards */
    IMPutC16(conn, 0);		/* unused */

    list_start = IMWritePos(conn);

    r = getICValues(icp, data, len, NEST_NONE, offset, &nested_separator);
    if (r < 0) return -1;

    list_end = IMWritePos(conn);
    IMRewriteC16(conn, pos, (unsigned int)(list_end - list_start));

    if (nested_separator) {
	/*
	 * There must have been some unbalanced NestedListSeparator.
	 */
	DPRINT(("getICvalues: unmatched separator\n"));
	IMCancelRequest(icp->im->connection, offset);
	IMSendError(icp->im->connection, IMBadSomething, icp->im->id, icp->id,
		    "corrupted nested list");
	return -1;
    }
    return 0;
}

void
IMFillDefault(icp, common_mask, preedit_mask, status_mask)
IMIC *icp;
unsigned long common_mask;
unsigned long preedit_mask;
unsigned long status_mask;
{
    TRACE(("IMFillDefault()\n"));

    if (common_mask != 0) fillCommonDefault(icp, common_mask);
    if (preedit_mask != 0) fillPSDefault(icp, NEST_PREEDIT, preedit_mask);
    if (status_mask != 0) fillPSDefault(icp, NEST_STATUS, status_mask);
}

int
IMValidateWindow(dpy, win, profilep)
Display *dpy;
Window win;
IMWindowProfile *profilep;
{
    Window root;
    int x, y;
    unsigned int width, height, border, depth;
    XAEHandle h;
    int s;

    TRACE(("IMValidateWindow(win: %08lx)\n", win));

    h = XAESetIgnoreErrors(dpy);
    s = XGetGeometry(dpy, win, &root, &x, &y,
		     &width, &height, &border, &depth);
    XAEUnset(h);

    if (profilep != NULL && s) {
	profilep->width = width;
	profilep->height = width;
	profilep->root = root;
    }
    return s;
}

int
IMValidateICAttributes(icp, checkonly)
IMIC *icp;
int checkonly;
{
    int error_occured;
    int r;

    TRACE(("IMValidateICAttributes()\n"));

    /*
     * Be careful not to send multiple error messages.
     */

    error_occured = 0;

    r = validateCommonAttr(icp, checkonly);
    if (r < 0) error_occured = 1;

    r = validatePSAttr(icp, NEST_PREEDIT, (checkonly && error_occured));
    if (r < 0) error_occured = 1;

    r = validatePSAttr(icp, NEST_STATUS, (checkonly && error_occured));
    if (r < 0) error_occured = 1;

    return error_occured ? -1 : 0;
}

void
IMFreeICAttributes(icp)
IMIC *icp;
{
    IMPSAttributes *pattr = &icp->preedit_attr;
    IMPSAttributes *sattr = &icp->status_attr;
    FontBank bank = IMFontBank(icp->im);

    TRACE(("IMFreeICAttributes()\n"));

    if (pattr->set_mask & ATTR_MASK_FONT_SET) XtFree(pattr->font_set);
    if (sattr->set_mask & ATTR_MASK_FONT_SET) XtFree(sattr->font_set);

    if (icp->num_fonts > 0) {
	FontBankFreeFonts(bank, icp->fonts, icp->num_fonts);
    }
}

unsigned long
IMMakeConvAttributes(icp, attr)
IMIC *icp;
ConversionAttributes *attr;
{
    IMCommonAttributes *cattr;
    IMPSAttributes *pattr, *sattr;
    unsigned long cmask;		/* changed attributes */
    unsigned long mask;			/* attributes to be set */

    TRACE(("IMMakeConvAttributes()\n"));
    mask = 0L;

    cattr = &icp->common_attr;
    pattr = &icp->preedit_attr;
    sattr = &icp->status_attr;

    /*
     * Check changes of common attributes.
     */
    cmask = cattr->change_mask;

    /* focus window */
    if (cmask & ATTR_MASK_FOCUS) {
	attr->focuswindow = cattr->focus;
	mask |= CAFocusWindow;
    }

    /*
     * Check changes of preedit attributes.
     */
    cmask = pattr->change_mask;

    /* client area */
    if (cmask & ATTR_MASK_AREA) {
	attr->clientarea.x = pattr->area.x;
	attr->clientarea.y = pattr->area.y;
	attr->clientarea.width = pattr->area.width;
	attr->clientarea.height = pattr->area.height;
	mask |= CAClientArea;
    }

    /* foreground/background */
    if (cmask & ATTR_MASK_FOREGROUND) {
	attr->foreground = pattr->foreground;
	mask |= CAColor;
    }
    if (cmask & ATTR_MASK_BACKGROUND) {
	attr->background = pattr->background;
	mask |= CAColor;
    }

    /* colormap */
    if (cmask & ATTR_MASK_COLORMAP) {
	attr->colormap = pattr->colormap;
	mask |= CAColormap;
    }

    /* background pixmap */
    if (cmask & ATTR_MASK_BG_PIXMAP) {
	attr->background_pixmap = pattr->bg_pixmap;
	mask |= CABackgroundPixmap;
    }

    /* line spacing */
    if (cmask & ATTR_MASK_LINESPACE) {
	attr->linespacing = pattr->line_space;
	mask |= CALineSpacing;
    }

    /* cursor */
    if (cmask & ATTR_MASK_CURSOR) {
	attr->cursor = pattr->cursor;
	mask |= CACursor;
    }

    /* font */
    if (cmask & ATTR_MASK_FONT_SET) {
	changeFonts(icp);
	attr->fonts = icp->fonts;
	attr->num_fonts = icp->num_fonts;
	mask |= CAFonts;
    }

    /* spot location */
    if ((cmask & ATTR_MASK_SPOT_LOCATION) &&
	icp->style == IMSTYLE_OVER_THE_SPOT) {
	attr->spotx = pattr->spot_location.x;
	attr->spoty = pattr->spot_location.y;
	mask |= CASpotLocation;
    }

    /*
     * Check changes of status attributes.
     */
    cmask = sattr->change_mask;

    /* status area */
    if (cmask & ATTR_MASK_AREA) {
	attr->statusarea.x = sattr->area.x;
	attr->statusarea.y = sattr->area.y;
	attr->statusarea.width = sattr->area.width;
	attr->statusarea.height = sattr->area.height;
	mask |= CAStatusArea;
    }

    return mask;
}

void
IMMoveLocation(icp, x, y)
IMIC *icp;
int x;
int y;
{
    TRACE(("IMMoveLocation()\n"));

    icp->preedit_attr.spot_location.x = x;
    icp->preedit_attr.spot_location.y = y;
    icp->preedit_attr.set_mask |= ATTR_MASK_SPOT_LOCATION;
    icp->preedit_attr.change_mask = ATTR_MASK_SPOT_LOCATION;
    if (icp->state & IC_CONVERTING) {
	changeConversionAttributes(icp);
    }
}
