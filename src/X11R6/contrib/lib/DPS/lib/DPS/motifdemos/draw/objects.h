/* objects.h - Interface for object implementors
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

#ifndef	OBJECTS_H
#define	OBJECTS_H

#include <X11/Xlib.h>

typedef int ObjectType;

/* Values of ObjectType...	*/
#define	LineType		0
#define	RectType		1
#define CircleType		2
#define	FilledRectType		3
#define	FilledCircleType	4
#define	TextType		5
#define	CountTypes		6

/* This struct defines a single graphics object	*/
typedef struct _t_GenericObject {
    int objectType;
    float x1, y1, x2, y2;
    float red, green, blue;
    struct _t_GenericObject *next;
    struct _t_GenericObject *prev;
    float bbox[4];
} GenericObject;

#define X(bbox) ((bbox)[0])
#define Y(bbox) ((bbox)[1])
#define WIDTH(bbox) ((bbox)[2])
#define HEIGHT(bbox) ((bbox)[3])
#define RIGHT(bbox) ((bbox)[0] + (bbox)[2])
#define TOP(bbox) ((bbox)[1] + (bbox)[3])

typedef struct _t_ObjTypeDesc {
    ObjectType type;
    char *label;
    void (*computeBBox)();
    GenericObject *(*createObject)();
    void (*drawObject)();
} ObjTypeDesc;
  
extern void InitStdObjects();
extern void InitTextObject();
extern ObjTypeDesc objDescriptors[CountTypes];

#endif	/* OBJECTS_H */
