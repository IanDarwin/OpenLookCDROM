/* textobject.c - Implements the TextType for draw
 *
 * (c) Copyright 1990-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include "objects.h"
#include "textmgr.h"
#include <math.h>
#include <DPS/FontSB.h>

typedef struct {
    GenericObject base;	/* Must be first!	*/
    char *font;
    float size;
    char *text;
    float bbox[4];
} TextObject;

#define DefaultFontSize		24
static char *defaultFont = "Helvetica";
static Widget fontPanel;

static char *currentFont;
static float currentSize;

/* ARGSUSED */

void CheckFont(widget, clientData, callData)
    Widget widget;
    XtPointer clientData, callData;
{
    FSBValidateCallbackRec *cb = (FSBValidateCallbackRec *) callData;

    if (cb->family_selection != FSBOne && cb->face_selection != FSBOne &&
        cb->size_selection != FSBOne && cb->name_selection != FSBOne) {
        cb->doit = FALSE;
    }
}

/* ARGSUSED */

void SetFont(widget, clientData, callData)
    Widget widget;
    XtPointer clientData, callData;
{
    FSBCallbackRec *cb = (FSBCallbackRec *) callData;

    (void)strcpy(currentFont, cb->name);
    currentSize = cb->size;
}

Widget CreateFontPanel(parent)
    Widget parent;
{
    Widget shell;

    shell = XtCreatePopupShell("Fonts", transientShellWidgetClass,
			       parent, (ArgList) NULL, 0);

    fontPanel = XtCreateManagedWidget("fontPanel", fontSelectionBoxWidgetClass,
				      shell, (ArgList) NULL, 0);
    XtAddCallback(fontPanel, XtNvalidateCallback, CheckFont, (XtPointer) NULL);
    XtAddCallback(fontPanel, XtNokCallback, SetFont, (XtPointer) NULL);
    XtAddCallback(fontPanel, XtNapplyCallback, SetFont, (XtPointer) NULL);

    FSBSetFontName(fontPanel, defaultFont, False);
    FSBSetFontSize(fontPanel, (double) DefaultFontSize , False);

    currentFont = XtMalloc(256);
    strcpy(currentFont, defaultFont);
    currentSize = DefaultFontSize;

    return fontPanel;
}

static void Transform(x, y, ox, oy, theta, rx, ry)
    float x, y, ox, oy, theta;
    float *rx, *ry;
{
    float r = sqrt((y - oy) * (y - oy) + (x - ox) * (x - ox));

    theta += atan2(y - oy, x - ox);

    *rx = r * cos(theta) + ox;
    *ry = r * sin(theta) + oy;
}

static void CheckCorner(cx, cy, ox, oy, theta, minx, miny, maxx, maxy)
    float cx, cy, ox, oy, theta;
    float *minx, *miny, *maxx, *maxy;
{
    float x, y;

    Transform(cx, cy, ox, oy, theta, &x, &y);
    if (x < *minx) *minx = x;
    if (y < *miny) *miny = y;
    if (x > *maxx) *maxx = x;
    if (y > *maxy) *maxy = y;
}

static void ComputeTextBBox(gen, bbox)
    GenericObject *gen;
    register float *bbox;
{
    TextObject *obj = (TextObject *) gen;
    float minx, miny, maxx, maxy;
    float theta;

    if (HEIGHT(obj->bbox) == 0 && WIDTH(obj->bbox) == 0) {
	float dummy;
	FSBGetTextDimensions(fontPanel, obj->text, obj->font,
			     obj->size, obj->base.x1, obj->base.y1,
			     &dummy, &dummy,
			     &X(obj->bbox), &WIDTH(obj->bbox),
			     &HEIGHT(obj->bbox), &Y(obj->bbox));
	WIDTH(obj->bbox) -= X(obj->bbox);
	HEIGHT(obj->bbox) -= Y(obj->bbox);
    }

    if (gen->x1 == gen->x2 && gen->y1 == gen->y2) theta = 0;
    else theta = atan2(gen->y2 - gen->y1, gen->x2 - gen->x1);

    Transform(X(obj->bbox), Y(obj->bbox), gen->x1, gen->y1,
	      theta, &minx, &miny);
    maxx = minx;
    maxy = miny;
    CheckCorner(X(obj->bbox), TOP(obj->bbox),
		gen->x1, gen->y1, theta, &minx, &miny, &maxx, &maxy);
    CheckCorner(RIGHT(obj->bbox), Y(obj->bbox),
		gen->x1, gen->y1, theta, &minx, &miny, &maxx, &maxy);
    CheckCorner(RIGHT(obj->bbox), TOP(obj->bbox),
		gen->x1, gen->y1, theta, &minx, &miny, &maxx, &maxy);

    X(bbox) = minx - 1;
    Y(bbox) = miny - 1;
    WIDTH(bbox) = maxx - minx + 2;
    HEIGHT(bbox) = maxy - miny + 2;
}
  
GenericObject *CreateTextObject(type)
    ObjectType type;
{
    TextObject *obj = (TextObject *)malloc(sizeof(TextObject));

    obj->font = XtNewString(currentFont);
    obj->size = currentSize;
    obj->text = GetTextValue();
    if (obj->text == NULL || *obj->text == '\0') {
	obj->text = XtNewString("Surprise!");
    }

    HEIGHT(obj->bbox) = WIDTH(obj->bbox) = 0;

    obj->base.objectType = type;
    return (GenericObject *)obj;
}

void DrawTextObject(base)
    GenericObject *base;
{
    TextObject *obj = (TextObject *) base;

    if (base->y2 == base->y1 && base->x2 == base->x1) base->x2 = base->x1 + 1;

    PSWDrawText(base->x1, base->y1, base->x2 - base->x1, base->y2 - base->y1,
		obj->font, obj->text, obj->size);
}

void InitTextObject()
{
    objDescriptors[TextType].type = TextType;
    objDescriptors[TextType].label = "text";
    objDescriptors[TextType].computeBBox = ComputeTextBBox;
    objDescriptors[TextType].createObject = CreateTextObject;
    objDescriptors[TextType].drawObject = DrawTextObject;
}
