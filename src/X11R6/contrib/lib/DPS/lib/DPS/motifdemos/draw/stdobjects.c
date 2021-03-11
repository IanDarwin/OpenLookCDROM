/* stdobjects.c - Manages several simple object types
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
#include "objects.h"
#include "stdwraps.h"

#define	CountStdTypes	5

#undef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#undef ABS
#define ABS(a) ((a) < 0 ? -(a) : (a))

static void (*drawFunctions[CountStdTypes])();

static void ComputeBBoxForStdObject(obj, bbox)
    register GenericObject *obj;
    register float *bbox;
{
    X(bbox) = MIN(obj->x1, obj->x2) - 1;
    Y(bbox) = MIN(obj->y1, obj->y2) - 1;

    WIDTH(bbox) = ABS(obj->x1 - obj->x2) + 2;
    HEIGHT(bbox) = ABS(obj->y1 - obj->y2) + 2;
}

static void ComputeBBoxForCircle(obj, bbox)
    register GenericObject *obj;
    register float *bbox;
{
    float dx = ABS(obj->x1 - obj->x2);
    float dy = ABS(obj->y1 - obj->y2);

    X(bbox) = obj->x1 - dx - 1;
    Y(bbox) = obj->y1 - dy - 1;

    WIDTH(bbox) = dx * 2 + 2;
    HEIGHT(bbox) = dy * 2 + 2;
}

void DrawStdObject(obj)
    GenericObject *obj;
{
    (*(drawFunctions[obj->objectType]))(obj->x1, obj->y1, obj->x2, obj->y2);
}

GenericObject *CreateStdObject(type)
    ObjectType type;
{
    GenericObject *obj = (GenericObject *)malloc(sizeof(GenericObject));
    
    obj->objectType = type;
    return(obj);
}

void InitStdObjects()
{
    register ObjTypeDesc *objs;
  
    drawFunctions[0] = PSWDrawLine;
    drawFunctions[1] = PSWDrawRect;
    drawFunctions[2] = PSWDrawCircle;
    drawFunctions[3] = PSWDrawFilledRect;
    drawFunctions[4] = PSWDrawFilledCircle;
  
    objs = objDescriptors;

    /* Fill in LineType Descriptor...		*/
    objs[LineType].type = LineType;
    objs[LineType].label = "line";
    objs[LineType].computeBBox = ComputeBBoxForStdObject;
    objs[LineType].createObject = CreateStdObject;
    objs[LineType].drawObject = DrawStdObject;

    /* Fill in RectType Descriptor...		*/
    objs[RectType].type = RectType;
    objs[RectType].label = "rectangle";
    objs[RectType].computeBBox = ComputeBBoxForStdObject;
    objs[RectType].createObject = CreateStdObject;
    objs[RectType].drawObject = DrawStdObject;

    /* Fill in CircleType Descriptor...		*/
    objs[CircleType].type = CircleType;
    objs[CircleType].label = "circle";
    objs[CircleType].computeBBox = ComputeBBoxForCircle;
    objs[CircleType].createObject = CreateStdObject;
    objs[CircleType].drawObject = DrawStdObject;

    /* Fill in FilledRectType Descriptor...	*/
    objs[FilledRectType].type = FilledRectType;
    objs[FilledRectType].label = "filledRectangle";
    objs[FilledRectType].computeBBox = ComputeBBoxForStdObject;
    objs[FilledRectType].createObject = CreateStdObject;
    objs[FilledRectType].drawObject = DrawStdObject;

    /* Fill in FilledCircleType Descriptor...	*/
    objs[FilledCircleType].type = FilledCircleType;
    objs[FilledCircleType].label = "filledCircle";
    objs[FilledCircleType].computeBBox = ComputeBBoxForCircle;
    objs[FilledCircleType].createObject = CreateStdObject;
    objs[FilledCircleType].drawObject = DrawStdObject;
}
