/*
 * Copyright (C) 1990-1991 by Adobe Systems Incorporated.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems
 * Incorporated not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY FITNESS FOR A PARTICULAR PURPOSE AND
 * NON-INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * PostScript, Display PostScript, and Adobe are trademarks of Adobe Systems
 * Incorporated registered in the U.S.A. and other countries.
 *
 * Author: Adobe Systems Incorporated
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <DPS/psops.h>
#include <DPS/dpsXclient.h>
#include <DPS/dpsXshare.h>
#include <Xm/Scale.h>
#include <Xm/ToggleB.h>

#include "wwraps.h"

typedef struct {
    int red;
    int green;
    int blue;
    int background;
    int size;
    int angle;
    Boolean buffered;
    Widget drawingArea;
    Pixmap pixmap;
    Dimension width;
    Dimension height;
    DPSGState gstate;
    DPSGState windowGstate;
} Element;

extern Element square, curve, text, image;

typedef struct {
  char *imageFileName;
  char *font;
  char *fallbackImage;
  Boolean debug;
} AppData;

extern AppData appData;
extern Display *dpy;

#define INIT_RED 1000
#define INIT_GREEN 1000
#define INIT_BLUE 1000
#define INIT_BACK 500
#define INIT_SCALE	100
#define INIT_ROTATE  0

extern void init_drawing(), red_moved(), blue_moved(), green_moved(),
	back_moved(), size_moved(), angle_moved(), on_off(), set_up_window();
