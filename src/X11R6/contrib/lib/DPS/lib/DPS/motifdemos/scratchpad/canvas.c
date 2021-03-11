/* canvas.c
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

/* C headers */
#include <stdio.h>
#include <math.h>

/* Xlib library headers */
#include <X11/keysym.h>
#include <X11/cursorfont.h>

/* Xt toolkit headers */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

/* Motif widget headers */
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/ToggleBG.h>
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#include <Xm/FileSB.h>

/* DPS library headers */
#include <DPS/dpsXclient.h>
#include <DPS/dpsops.h>
#include <DPS/psops.h>

#define __CANVAS__
#include "canvas.h"
#include "globals.h"
#include "list.h"
#include "wraps.h"
#undef __CANVAS__

typedef struct GraphicsObjectRec {
int x, y;               /* x & y position (pixmap coordinates) */
    float psX, psY;         /* x & y position (PostScript coord.)  */
    int width, height;      /* width and height of the object      */
} GraphicsObject;

static struct GraphicsModeRec {
    int outline;                /* outline or solid text?    */
    int anchor;                 /* is anchor set?            */
    GraphicsFunction function;  /* which function to execute */
} mode;
    
static struct GraphicsBufferRec {
    Window window;              /* window id of the DrawingArea widget */
    Pixmap pixmap;              /* pixmap used for storing window data */
    Widget horizSB, vertSB;     /* scrollbars controlling x & y offset */
    int xoffs, yoffs;           /* x & y offset for canvas into pixmap */
    XRectangle rect;            /* window rectangle                    */
    Dimension wwidth, wheight;  /* width and height of the window      */
    Dimension pwidth, pheight;  /* width and height of the pixmap      */
    int textanchor;             /* x position for carriage returns     */
} gb;

static int charcount = 0;           /* the number of characters entered */
static GraphicsObject textcursor;   /* the text cursor object           */
static GraphicsObject anchor;       /* the graphics anchor object       */
static FILE *psout;                 /* where to send print output       */
static GC clipGC;                   /* gc to use for clipping           */
static Region emptyRegion;          /* empty region for clipping        */
static Region clipRegion;           /* clip region for clipping         */
static boolean deleteAllFirst;

static struct CursorsRec { 
    Cursor select;         /* hand cursor */
    Cursor resize;         /* resize cursor */
    Cursor rotate;         /* rotate cursor */
    Cursor delete;         /* delete cursor */
    Cursor slant;          /* slant  cursor */
    Cursor anchor;         /* anchor cursor */
    Cursor reset;         /* anchor cursor */
} cursors;

#define ANCHOR_SIZE 2               /* size of an anchor arm      */
#define DOUBLE_CLICK_TIME 250       /* delay time in milliseconds */

#define minmax(x,y,min,max) \
    ((x < y? (min=x)&&(max=y): (max=x)&&(min=y)))

#define isasciikeysym(keysym) \
    (((keysym >= XK_KP_Space) && (keysym <= XK_KP_9)) \
    || ((keysym >= XK_space) && (keysym <= XK_asciitilde)))

#define isasciimodifier(keysym) \
    ((keysym >= XK_Shift_L) && (keysym <= XK_Hyper_R))

#define DoubleClick(event, time) \
    (event->xbutton.time-time<DOUBLE_CLICK_TIME ? \
          (time = event->xbutton.time) : !(time = event->xbutton.time))

#define ConvertXtoPS(xX, xY, psX, psY) \
    convertDevicePoint((float)(xX), (float)(xY), &(psX), &(psY));

#define ConvertPStoX(psX, psY, xX, xY)                                        \
    { float x, y;                                                             \
    convertUserPoint(psX, psY, &x, &y);                                       \
    xX = (int)(x); xY= (int)(y); }

#define DrawTextCursor(dpy, drawable, gc)                                     \
    XDrawLine(dpy, drawable, gc, textcursor.x - gb.xoffs - textcursor.width,  \
          textcursor.y - gb.yoffs, textcursor.x - gb.xoffs +                  \
          textcursor.width, textcursor.y - gb.yoffs);                         \
    XDrawLine(dpy, drawable, gc, textcursor.x - gb.xoffs - textcursor.width,  \
          textcursor.y - gb.yoffs - textcursor.height, textcursor.x -         \
          gb.xoffs + textcursor.width, textcursor.y - gb.yoffs -              \
          textcursor.height);                                                 \
    XDrawLine(dpy, drawable, gc, textcursor.x-gb.xoffs, textcursor.y -        \
          gb.yoffs- textcursor.height, textcursor.x-gb.xoffs,                 \
          textcursor.y-gb.yoffs);

#define EraseTextCursor(dpy, src, dest, gc)                                   \
    XCopyArea(dpy, src, dest, gc, textcursor.x-textcursor.width,              \
          textcursor.y-textcursor.height, (textcursor.width*2)+1,             \
          textcursor.height+1, textcursor.x-textcursor.width-gb.xoffs,        \
          textcursor.y-textcursor.height-gb.yoffs);

#define DrawAnchor(dpy, drawable, gc)                                         \
    XDrawLine(dpy, drawable, gc,                                              \
          anchor.x-ANCHOR_SIZE-gb.xoffs, anchor.y-ANCHOR_SIZE-gb.yoffs,       \
          anchor.x+ANCHOR_SIZE-gb.xoffs, anchor.y+ANCHOR_SIZE-gb.yoffs);      \
    XDrawLine(dpy, drawable, gc,                                              \
          anchor.x-ANCHOR_SIZE-gb.xoffs, anchor.y+ANCHOR_SIZE-gb.yoffs,       \
          anchor.x+ANCHOR_SIZE-gb.xoffs, anchor.y-ANCHOR_SIZE-gb.yoffs);

#define EraseAnchor(dpy, src, dest, gc)                                       \
    XCopyArea(dpy, src, dest, gc, anchor.x-ANCHOR_SIZE, anchor.y-             \
          ANCHOR_SIZE, (2*ANCHOR_SIZE)+1, (2*ANCHOR_SIZE)+1,                  \
          anchor.x-ANCHOR_SIZE-gb.xoffs, anchor.y-ANCHOR_SIZE-gb.yoffs);

#define CopyBBox(dpy, src, dest, gc, bbox)                                    \
    XCopyArea(dpy, src, dest, gc, (int)bbox.llx, (int)bbox.ury,               \
          (int)(bbox.urx - bbox.llx)+2, (int)(bbox.lly - bbox.ury)+2,         \
          ((int)bbox.llx) - gb.xoffs, ((int)bbox.ury) - gb.yoffs);

#define CopyRegion(dpy, gc, region)                                           \
    XOffsetRegion(region, -gb.xoffs, -gb.yoffs);                              \
    XSetRegion(dpy, gc, region);                                              \
    XCopyArea(dpy, gb.pixmap, gb.window, gc, gb.xoffs,                        \
          gb.yoffs, gb.wwidth, gb.wheight, 0, 0);                             \
    XIntersectRegion(region, emptyRegion, region);

#define EraseRectangle(dpy, src, dest, gc, minX, minY, maxX, maxY)            \
    XCopyArea(dpy, src, dest, gc, minX, minY, maxX - minX, 1,                 \
          minX - gb.xoffs, minY - gb.yoffs); XCopyArea(dpy, src, dest, gc,    \
          minX, maxY, maxX - minX + 1, 1, minX - gb.xoffs, maxY - gb.yoffs);  \
    XCopyArea(dpy, src, dest, gc, minX, minY, 1, maxY - minY, minX -          \
          gb.xoffs, minY - gb.yoffs); XCopyArea(dpy, src, dest, gc,           \
          maxX, minY, 1, maxY - minY + 1, maxX - gb.xoffs, minY - gb.yoffs);

#define DeleteSelected(region)                                                \
    if (deleteSelected(&region)) {                                            \
        DPSWaitContext(DPSGetCurrentContext());                               \
        CopyRegion(dpy, clipGC, region);                                      \
        DrawAnchor(dpy, gb.window, clipGC);                                   \
        DrawTextCursor(dpy, gb.window, clipGC);                               \
    }

#define UnselectAll(region)                                                   \
    if (unselectAll(&region)) {                                               \
        DPSWaitContext(DPSGetCurrentContext());                               \
        CopyRegion(dpy, clipGC, region);                                      \
        DrawAnchor(dpy, gb.window, clipGC);                                   \
        DrawTextCursor(dpy, gb.window, clipGC);                               \
    }
                              
#define SelectMoreElements(x, y, region)                                      \
    if (constructiveSelectElements(x, y, x, y, &region)) {                    \
        DPSWaitContext(DPSGetCurrentContext());                               \
        CopyRegion(dpy, clipGC, region);                                      \
        DrawAnchor(dpy, gb.window, clipGC);                                   \
        DrawTextCursor(dpy, gb.window, clipGC);                               \
    }

int GetCanvasHeight ()
{
    return (gb.pheight);
}

void ChangeMode (widget, new_mode, cb)
    Widget widget;
    GraphicsFunction new_mode;
    XmToggleButtonCallbackStruct *cb;
{
    mode.anchor = False;
    EraseAnchor(dpy, gb.pixmap, gb.window, defaultGC);

    switch (new_mode) {
      case Mode_Outline:
          mode.outline = cb->set;
      break;
      case Mode_Select:
          mode.function = new_mode;
          XDefineCursor(dpy, gb.window, cursors.select);
          anchor.x = anchor.y = -10;
      break;
      case Mode_Delete:
          mode.function = new_mode;
          XDefineCursor(dpy, gb.window, cursors.delete);
          anchor.x = anchor.y = -10;
      break;
      case Mode_Reset:
          mode.function = new_mode;
          XDefineCursor(dpy, gb.window, cursors.reset);
          anchor.x = anchor.y = -10;
      break;
      default:
          mode.function = new_mode;
          XDefineCursor(dpy, gb.window, cursors.anchor);
    }
}

void SetCursorSize (psWidth, psHeight)
    float psWidth;
    float psHeight;
{
    int xWidth, xHeight;

    ConvertPStoX(psWidth, psHeight, xWidth, xHeight);
    xHeight -= gb.pheight;

    if ((gb.pixmap)&&(gb.window)) {
        EraseTextCursor(dpy, gb.pixmap, gb.window, defaultGC);
        DrawAnchor(dpy, gb.window, defaultGC);
    }

    textcursor.width = xWidth/6;
    textcursor.height = -xHeight;

    if (gb.window) {
        DrawTextCursor(dpy, gb.window, defaultGC);
    }
}

void appendMode (button, client_data, call_data)
    Widget button;
    caddr_t client_data;
    caddr_t call_data;
{
  deleteAllFirst = FALSE;
}

void loadMode (button, client_data, call_data)
    Widget button;
    caddr_t client_data;
    caddr_t call_data;
{
  deleteAllFirst = TRUE;
}

void selectAllCallback (button, client_data, call_data)
    Widget button;
    caddr_t client_data;
    caddr_t call_data;
{
  if (selectAll(&clipRegion)) {
    DPSWaitContext(DPSGetCurrentContext());
    CopyRegion(dpy, clipGC, clipRegion);
    DrawAnchor(dpy, gb.window, clipGC);
    DrawTextCursor(dpy, gb.window, clipGC);
  }
}  

void updateSelectedFont(fontName, fontSize, fontBBox)
     char *fontName;
     float fontSize;
     bboxStruct *fontBBox;
{
  if (changeSelectedFont(fontName, fontSize, fontBBox, &clipRegion)) {
    DPSWaitContext(DPSGetCurrentContext());
    CopyRegion(dpy, clipGC, clipRegion);
    DrawAnchor(dpy, gb.window, clipGC);
    DrawTextCursor(dpy, gb.window, clipGC);
  }
}

void kernLetters (button, client_data, call_data)
    Widget button;
    int client_data;
    caddr_t call_data;
{
  int xoff, yoff;

  switch (client_data) {
  case UP:
    xoff = 0; yoff = -1;
    break;
  case LEFT:
    xoff = -1; yoff = 0;
    break;
  case RIGHT:
    xoff = 1; yoff = 0;
    break;
  case DOWN:
    xoff = 0; yoff = 1;
    break;
  case UP_LEFT:
    xoff = -1; yoff = -1;
    break;
  case UP_RIGHT:
    xoff = 1; yoff = -1;
    break;
  case DOWN_LEFT:
    xoff = -1; yoff = 1;
    break;
  case DOWN_RIGHT:
    xoff = 1; yoff = 1;
    break;
  }

  if (moveSelectedByPixels(xoff, yoff, &clipRegion)) {
    DPSWaitContext(DPSGetCurrentContext());
    CopyRegion(dpy, clipGC, clipRegion);
    DrawAnchor(dpy, gb.window, clipGC);
    DrawTextCursor(dpy, gb.window, clipGC);
  }
}      

void LoadAIFile (dialog, client_data, call_data)
    Widget dialog;
    caddr_t client_data;
    caddr_t call_data;
{
    Widget text;
    char *fileName;

    text = XmFileSelectionBoxGetChild(dialog, XmDIALOG_TEXT);
    fileName = XmTextGetString(text);
    if(fileName != '\0') {
      if(deleteAllFirst)
	deleteAll(NULL);
      loadFile(fileName);
    }
    free(fileName);
    DPSWaitContext(DPSGetCurrentContext());
    XCopyArea(dpy, gb.pixmap, gb.window, defaultGC, gb.xoffs,
	      gb.yoffs, gb.wwidth, gb.wheight, 0, 0);
    return;
}

void SaveAIFile (dialog, client_data, call_data)
    Widget dialog;
    caddr_t client_data;
    caddr_t call_data;
{
    Widget text;
    char *fileName;

    text = XmSelectionBoxGetChild(dialog, XmDIALOG_TEXT);
    fileName = XmTextGetString(text);
    if(fileName != '\0')
      saveFile(fileName);
    free(fileName);
    return;
}

static unsigned GetColorByName (name)
    char *name;
{
    Colormap cmap;
    XColor color;

    cmap = DefaultColormap(dpy, DefaultScreen(dpy));

    return((XDisplayCells(dpy, DefaultScreen(dpy)) > 2) &&
          XParseColor(dpy, cmap, name, &color) &&
          XAllocColor(dpy, cmap, &color)
          ? color.pixel : BlackPixel(dpy, DefaultScreen(dpy)));
}

static Boolean AdjustOffset (position, offset, wsize, psize)
    int *position;
    int *offset;
    int wsize;
    int psize;
{
    Boolean adjusted = False;

    if (*position > wsize) {
        *offset += *position - wsize;
        if (*offset+wsize > psize) *offset = psize - wsize;
        *position = wsize;
        adjusted = True;
    } else if (*position < 0) {
        *offset += *position;
        if (*offset < 0) *offset = 0;
        *position = 0;
        adjusted = True;
    }

    return(adjusted);
}

static void PrintProc (ctxt, buf, count)
    DPSContext ctxt;
    char *buf;
    unsigned long count;
{
    fwrite(buf, sizeof(char), count, psout);
}

void PrintPage (selectionBox, printerButton, call_data)
    Widget selectionBox;
    Widget printerButton;
    caddr_t call_data;
{
    Arg arg;
    char *filename, *printer, command[256];
    DPSContext printctxt;
    XmString xmString;
    char **fontList;
    int i;
    int numFonts = 0;
    int currentLength = 0;

    XtSetArg(arg, XmNtextString, &xmString);
    XtGetValues(selectionBox, &arg, 1);

    if (XmToggleButtonGadgetGetState(printerButton)) {
        filename = tmpnam(NULL);
    } else {
        XmStringGetLtoR(xmString, XmSTRING_DEFAULT_CHARSET, &filename);
    }

    if ((psout = fopen(filename, "w")) == NULL) {
        fprintf(stderr, "sorry, couldn't open file %s\n", filename);
        return;
    }

    fprintf(psout, "%%!PS-Adobe-2.0 EPSF-2.0\n");
    getCurrentFontList(&fontList, &numFonts);
    if(numFonts > 0) {
      fprintf(psout, "%%%%DocumentNeededFonts: ");
      for(i=0; i < numFonts; i++) {
	currentLength += strlen(fontList[i]);
	if(currentLength > 70) {
	  currentLength = 0;
	  fprintf(psout, "\n%%%%+ ");
	}
	fprintf(psout, "%s ", fontList[i]);
	free(fontList[i]);
      }
      fprintf(psout, "\n");
      free(fontList);
    }
    fprintf(psout, "%%%%BoundingBox: 0 0 612 792\n");
    fprintf(psout, "%%%%EndComments\n\n");
    fprintf(psout, "/selectfont {exch findfont exch scalefont setfont} def\n");
    fprintf(psout, "/defineusername {pop pop} def\n%%%%EndProlog\n\n");
    fprintf(psout, "%%%%Page: 1\n");

    printctxt = DPSCreateTextContext(PrintProc, DPSDefaultErrorProc);
    DPSChainContext(DPSGetCurrentContext(), printctxt);

    redrawElements(0, 0, gb.pwidth, gb.pheight);

    DPSUnchainContext(printctxt);
    DPSDestroyContext(printctxt);

    fprintf(psout, "\nshowpage\n");
    fprintf(psout, "%%%%Trailer\n");
    fclose(psout);

    if (XmToggleButtonGadgetGetState(printerButton)) {
        /* this is an ugly way to do this, but    */
        /* I'll clean it up later if I have time. */
        XmStringGetLtoR(xmString, XmSTRING_DEFAULT_CHARSET, &printer);
        strcpy(command, "lpr -P");
        strcat(command, printer);
        strcat(command, " ");
        strcat(command, filename);
        fprintf(stdout, "%s\n", command);
        (void) system(command);
	unlink(filename);
    }
}

void ErasePage (widget, client_data, call_data)
    Widget widget;
    caddr_t client_data;
    caddr_t call_data;
{
    if (deleteAll(&clipRegion)) {
        DPSWaitContext(DPSGetCurrentContext());
        CopyRegion(dpy, clipGC, clipRegion);
        DrawAnchor(dpy, gb.window, clipGC);
        DrawTextCursor(dpy, gb.window, clipGC);
    }
}

static void MouseEventHandler (widget, client_data, event)
    Widget widget;
    caddr_t client_data;
    XEvent *event;
{
    Arg arg;
    int x, y;
    static Time time;
    static Boolean selectbox = True;

    static struct {
        int minX, minY;         /* min (x,y) of selection box */
        int maxX, maxY;         /* max (x,y) of selection box */
        int anchorX, anchorY;   /* current graphics anchor    */
        int controlX, controlY; /* graphics control point     */
    } box;

    switch (event->type) {
      case ButtonPress:
          selectbox = False;
          box.controlX = event->xbutton.x + gb.xoffs;
          box.controlY = event->xbutton.y + gb.yoffs;
          if ((event->xbutton.state&ControlMask) 
                || (mode.function == Mode_Select)) {
              XDefineCursor(dpy, gb.window, cursors.select);
              if ((DoubleClick(event, time))
                    &&(event->xbutton.button == Button1)) {
                  charcount = 0;
                  EraseTextCursor(dpy, gb.pixmap, gb.window, defaultGC);
                  DrawAnchor(dpy, gb.window, defaultGC);
                  textcursor.x = gb.xoffs + event->xbutton.x;
                  textcursor.y = gb.yoffs + event->xbutton.y;
                  gb.textanchor = textcursor.x;
                  DrawTextCursor(dpy, gb.window, defaultGC);
                  ConvertXtoPS(textcursor.x, textcursor.y, 
                        textcursor.psX, textcursor.psY);
	      } else {
		  box.anchorX=box.minX=box.maxX= event->xbutton.x+gb.xoffs;
		  box.anchorY=box.minY=box.maxY= event->xbutton.y+gb.yoffs;
		  switch (constructiveSelectElements(
			box.minX, box.minY, box.maxX, box.maxY, &clipRegion)) {
		    case SELECTED_ALREADY:
			selectbox = False;
		    break;
		    case SELECTED_NEW:
			selectbox = False;
                        switch (event->xbutton.button) {
                          case Button1:
                              UnselectAll(clipRegion);
                              SelectMoreElements(box.anchorX, box.anchorY, 
                                    clipRegion);
                          break;
                          case Button3:
                              SelectMoreElements(box.anchorX, box.anchorY, 
                                    clipRegion);
                          break;
                        }
		    break;
		    case SELECTED_MISS:
			selectbox = True;
                        if (event->xbutton.button == Button1) {
			    UnselectAll(clipRegion);
                        }
		    break;
		  }
	      }
	  } else {
	      x = event->xbutton.x + gb.xoffs;
	      y = event->xbutton.y + gb.yoffs;
	      switch (mode.function) {
		case Mode_Reset:
		    if (resetSelectedAlignment(x, y, &clipRegion)) {
                        DPSWaitContext(DPSGetCurrentContext());
                        CopyRegion(dpy, clipGC, clipRegion);
                        DrawAnchor(dpy, gb.window, clipGC);
                        DrawTextCursor(dpy, gb.window, clipGC);
		    }
		break;
		case Mode_Delete:
		    switch (constructiveSelectElements(x, y, x, y, 
                          &clipRegion)) {
		      case SELECTED_ALREADY:
			  selectbox = False;
		      break;   
		      case SELECTED_NEW:
			  selectbox = False;
                          UnselectAll(clipRegion);
                          SelectMoreElements(x, y, clipRegion);
		      break;
		      case SELECTED_MISS:
			  selectbox = True;
			  UnselectAll(clipRegion);
			  box.anchorX = event->xbutton.x + gb.xoffs;
			  box.anchorY = event->xbutton.y + gb.yoffs;
			  box.minX = box.maxX = box.anchorX;
			  box.minY = box.maxY = box.anchorY;
		      break;
		    }
		break;
		case Mode_Slant:
		    EraseAnchor(dpy, gb.pixmap, gb.window, defaultGC);
		    if (mode.anchor) {
			(void) beginSlant(x, y);
			mode.anchor = False;
			XDefineCursor(dpy, gb.window, cursors.slant);
		    } else {
			anchor.x = event->xbutton.x + gb.xoffs;
			anchor.y = event->xbutton.y + gb.yoffs;
			DrawAnchor(dpy, gb.window, defaultGC);
			if (anchorSlant(anchor.x, anchor.y)) {
                                mode.anchor = True;
                            }
		    }
		break;
		case Mode_Resize:
		    EraseAnchor(dpy, gb.pixmap, gb.window, defaultGC);
		    if (mode.anchor) {
			(void) beginScale(x, y);
			mode.anchor = False;
			XDefineCursor(dpy, gb.window, cursors.resize);
		    } else {
			anchor.x = event->xbutton.x + gb.xoffs;
                            anchor.y = event->xbutton.y + gb.yoffs;
                            DrawAnchor(dpy, gb.window, defaultGC);
                            if (anchorScale(anchor.x, anchor.y)) {
                                mode.anchor = True;
                            }
		    }
		break;
		case Mode_Rotate:
		    EraseAnchor(dpy, gb.pixmap, gb.window, defaultGC);
		    if (mode.anchor) {
			(void) beginRotate(x, y);
			mode.anchor = False;
			XDefineCursor(dpy, gb.window, cursors.rotate);
		    } else {
			anchor.x = event->xbutton.x + gb.xoffs;
			anchor.y = event->xbutton.y + gb.yoffs;
			DrawAnchor(dpy, gb.window, defaultGC);
			if (anchorRotate(anchor.x, anchor.y)) {
			    mode.anchor = True;
			}
		    }
		break;
	      }
	  }
          /* throw away all other buttonpress events */
          while (XCheckTypedEvent(dpy, ButtonPress, event));
      break;
      case MotionNotify:
          while (XCheckTypedEvent(dpy, MotionNotify, event));
          switch (event->xmotion.state) {
            case Button1Mask:
            case Button1Mask|ControlMask:
                if (!selectbox) {
                    x = event->xmotion.x + gb.xoffs;
                    y = event->xmotion.y + gb.yoffs;
                    if (AdjustOffset(&(event->xmotion.x), &(gb.xoffs), 
                          gb.wwidth, gb.pwidth)) {
                        XWarpPointer(dpy, None, gb.window, 0, 0, 0, 0, 
                              event->xmotion.x, event->xmotion.y);
                        XtSetArg(arg, XmNvalue, gb.xoffs);
                        XtSetValues(gb.horizSB, &arg, 1);
                        XCopyArea(dpy, gb.pixmap, gb.window, defaultGC, 
                              gb.xoffs, gb.yoffs, gb.wwidth, gb.wheight, 0, 0);
                    }
                    if (AdjustOffset(&(event->xmotion.y), &(gb.yoffs),
                          gb.wheight, gb.pheight)) {
                        XWarpPointer(dpy, None, gb.window, 0, 0, 0, 0, 
                              event->xmotion.x, event->xmotion.y);
                        XtSetArg(arg, XmNvalue, gb.yoffs);
                        XtSetValues(gb.vertSB, &arg, 1);
                        XCopyArea(dpy, gb.pixmap, gb.window, defaultGC, 
                              gb.xoffs, gb.yoffs, gb.wwidth, gb.wheight, 0, 0);
                    }
                    DrawAnchor(dpy, gb.window, defaultGC);
		    DrawTextCursor(dpy, gb.window, defaultGC);
                    if ((event->xmotion.state&ControlMask)
                          || (mode.function == Mode_Select)) {
                        if (moveSelectedByPixels(x-box.controlX, 
                              y- box.controlY, &clipRegion)) {
                            DPSWaitContext(DPSGetCurrentContext());
                            CopyRegion(dpy, clipGC, clipRegion);
                            DrawAnchor(dpy, gb.window, clipGC);
                            DrawTextCursor(dpy, gb.window, clipGC);
                        }
			box.controlX = x;
			box.controlY = y;
                    } else switch (mode.function) {
		      case Mode_Rotate:
                          if ((!mode.anchor) &&
                                (doRotate(x, y, &clipRegion))) {
                              DPSWaitContext(DPSGetCurrentContext());
                              CopyRegion(dpy, clipGC, clipRegion);
                              DrawAnchor(dpy, gb.window, clipGC);
                              DrawTextCursor(dpy, gb.window, clipGC);
                          }
                      break;
                      case Mode_Resize:
                          if ((!mode.anchor) &&
                              (doScale(x, y, &clipRegion))) {
                              DPSWaitContext(DPSGetCurrentContext());
                              CopyRegion(dpy, clipGC, clipRegion);
                              DrawAnchor(dpy, gb.window, clipGC);
                              DrawTextCursor(dpy, gb.window, clipGC);
                          }
		      break;
		      case Mode_Slant:
			  if ((!mode.anchor) && 
                              (doSlant(x, y, &clipRegion))) {
                              DPSWaitContext(DPSGetCurrentContext());
                              CopyRegion(dpy, clipGC, clipRegion);
                              DrawAnchor(dpy, gb.window, clipGC);
                              DrawTextCursor(dpy, gb.window, clipGC);
			  }
		      break;
		    }
                break; /* don't fall through */
                }
            /* fall through to default */
            default:
                if ((!(event->xbutton.state&Button2Mask)) &&
                      ((event->xbutton.state&ControlMask) 
                      || (mode.function == Mode_Select) || (selectbox))) {
                    if (AdjustOffset(&(event->xmotion.x), &(gb.xoffs), 
                          gb.wwidth, gb.pwidth)) {
                        XWarpPointer(dpy, None, gb.window, 0, 0, 0, 0, 
                              event->xmotion.x, event->xmotion.y);
                        XtSetArg(arg, XmNvalue, gb.xoffs);
                        XtSetValues(gb.horizSB, &arg, 1);
                        XCopyArea(dpy, gb.pixmap, gb.window, defaultGC, 
                              gb.xoffs, gb.yoffs, gb.wwidth, gb.wheight, 0, 0);
                    }
                    if (AdjustOffset(&(event->xmotion.y), &(gb.yoffs),
                          gb.wheight, gb.pheight)) {
                        XWarpPointer(dpy, None, gb.window, 0, 0, 0, 0, 
                              event->xmotion.x, event->xmotion.y);
                        XtSetArg(arg, XmNvalue, gb.yoffs);
                        XtSetValues(gb.vertSB, &arg, 1);
                        XCopyArea(dpy, gb.pixmap, gb.window, defaultGC, 
                              gb.xoffs, gb.yoffs, gb.wwidth, gb.wheight, 0, 0);
                    }
                    EraseRectangle(dpy, gb.pixmap, gb.window, defaultGC,
                          box.minX, box.minY, box.maxX, box.maxY);
                    DrawAnchor(dpy, gb.window, defaultGC);
                    DrawTextCursor(dpy, gb.window, defaultGC);
                    minmax(event->xmotion.x+gb.xoffs, box.anchorX, 
                          box.minX, box.maxX);
                    minmax(event->xmotion.y+gb.yoffs, box.anchorY, 
                          box.minY, box.maxY);
                    XDrawRectangle(dpy, gb.window, defaultGC,
                          box.minX - gb.xoffs, box.minY - gb.yoffs, 
                          box.maxX - box.minX, box.maxY - box.minY);
                    switch (event->xmotion.state) {
                      case Button1Mask:
                      case Button1Mask|ControlMask:
                          if (destructiveSelectElements(box.minX, box.minY, 
                                box.maxX, box.maxY, &clipRegion)) {
                              DPSWaitContext(DPSGetCurrentContext());
                              CopyRegion(dpy, clipGC, clipRegion);
                              DrawAnchor(dpy, gb.window, clipGC);
                              DrawTextCursor(dpy, gb.window, clipGC);
                          }
                      break;
                      case Button3Mask:
                      case Button3Mask|ControlMask:
                          if (constructiveSelectElements(box.minX, box.minY, 
                                box.maxX, box.maxY, &clipRegion)) {
                              DPSWaitContext(DPSGetCurrentContext());
                              CopyRegion(dpy, clipGC, clipRegion);
                              DrawAnchor(dpy, gb.window, clipGC);
                              DrawTextCursor(dpy, gb.window, clipGC);
                          }
                      break;
                    }
                    XDrawRectangle(dpy, gb.window, defaultGC,
                          box.minX - gb.xoffs, box.minY - gb.yoffs, 
                          box.maxX - box.minX, box.maxY - box.minY);
                }
            break;
          }
      break;
      case ButtonRelease:
          EraseRectangle(dpy, gb.pixmap, gb.window, defaultGC,
                box.minX, box.minY, box.maxX, box.maxY);
          DrawAnchor(dpy, gb.window, defaultGC);
          DrawTextCursor(dpy, gb.window, defaultGC);
          if (event->xbutton.button == Button1) switch (mode.function) {
                case Mode_Delete:
                    if ((selectbox)) {
                        if (!(event->xbutton.state&ControlMask)) {
                            DeleteSelected(clipRegion);
                        }
                    } else switch (constructiveSelectElements(
                          event->xbutton.x+gb.xoffs, 
                          event->xbutton.y+gb.yoffs,
                          event->xbutton.x+gb.xoffs, 
                          event->xbutton.y+gb.yoffs, &clipRegion)) {
                        case SELECTED_MISS:
                            UnselectAll(clipRegion);
                        break;
                        default:
                            if (!(event->xbutton.state&ControlMask)) {
                                DeleteSelected(clipRegion);
                            }
                    }
                break;
          }
          if (mode.anchor) switch (mode.function) {
            case Mode_Resize:
                XDefineCursor(dpy, gb.window, cursors.resize);
            break;
            case Mode_Rotate:
                XDefineCursor(dpy, gb.window, cursors.rotate);
            break;
            case Mode_Slant:
                XDefineCursor(dpy, gb.window, cursors.slant);
            break;
          } else if ((mode.function != Mode_Select)) {
              if (mode.function == Mode_Delete) {
                  XDefineCursor(dpy, gb.window, cursors.delete);
              } else XDefineCursor(dpy, gb.window, cursors.anchor);
          }
      break;
    }
}

static void TextEventHandler (widget, client_data, event)
    Widget widget;
    caddr_t client_data;
    XEvent *event;
{
    KeySym keysym;
    bboxStruct fontBBox;
    char key, fontName[256];
    float tmpX, tmpY, fontSize;
    int returnVal;

    CurrentFontInfo(fontName, &fontSize, &fontBBox);

    switch (event->type) {
      case KeyPress:
          do {
              (void) XLookupString((XKeyEvent*)event, &key, 1, &keysym, NULL);
              switch (keysym) {
                case XK_Return:
                case XK_KP_Enter:
                    EraseTextCursor(dpy, gb.pixmap, gb.window, defaultGC);
                    DrawAnchor(dpy, gb.window, defaultGC);
		    addElement(' ', fontName, fontSize,
			       mode.outline, &fontBBox, &clipRegion,
			       &(textcursor.psX), &(textcursor.psY));
                    textcursor.y += textcursor.height;
                    textcursor.x = gb.textanchor;
                    ConvertXtoPS(textcursor.x, textcursor.y,
                          textcursor.psX, textcursor.psY);
                    DrawTextCursor(dpy, gb.window, defaultGC);
		    charcount++;
                break;
                case XK_BackSpace:
                case XK_Delete:
                    if (charcount > 0) {
		          returnVal = deleteLastElement(&(textcursor.psX), 
				     &(textcursor.psY), &clipRegion);
			  if(returnVal == 1) {
			    DPSWaitContext(DPSGetCurrentContext());
			    EraseTextCursor(dpy, gb.pixmap, gb.window, 
					    defaultGC);
			    DrawAnchor(dpy, gb.window, defaultGC);
			    ConvertPStoX(textcursor.psX, textcursor.psY,
					 textcursor.x, textcursor.y);
			    DrawTextCursor(dpy, gb.window, defaultGC);
			    CopyRegion(dpy, clipGC, clipRegion);
			    DrawTextCursor(dpy, gb.window, defaultGC);
			  }
			  else if(returnVal == -1) {
			    EraseTextCursor(dpy, gb.pixmap, gb.window, 
					    defaultGC);
			    DrawAnchor(dpy, gb.window, defaultGC);
			    ConvertPStoX(textcursor.psX, textcursor.psY,
					 textcursor.x, textcursor.y);
			    DrawTextCursor(dpy, gb.window, defaultGC);
			  }			    
                        charcount--;
                    } else XBell(dpy,100);
                break;
                default:
                    if (isasciikeysym(keysym)) {
                        if (addElement(key, fontName, fontSize,
                              mode.outline, &fontBBox, &clipRegion,
                              &(textcursor.psX), &(textcursor.psY))) {
                            DPSWaitContext(DPSGetCurrentContext());
                            CopyRegion(dpy, clipGC, clipRegion);
                        }
                        EraseTextCursor(dpy, gb.pixmap, gb.window, defaultGC); 
                        DrawAnchor(dpy, gb.window, defaultGC);
                        ConvertPStoX(textcursor.psX, textcursor.psY,
                              textcursor.x, textcursor.y);
                        DrawTextCursor(dpy, gb.window, defaultGC);
                        charcount++;
                    } else if (!isasciimodifier(keysym)) XBell(dpy,100);
              }
          } while (XCheckTypedEvent(dpy, KeyPress, event));
      break;
    }
}

static void ChangeOffset (widget, orientation, cb)
    Widget widget;
    unsigned int orientation;
    XmScrollBarCallbackStruct *cb;
{
    switch (orientation) {
      case XmHORIZONTAL:
          gb.xoffs = cb->value;
      break;
      case XmVERTICAL:
          gb.yoffs = cb->value;
      break;
    }

    XCopyArea(dpy, gb.pixmap, gb.window, defaultGC, 
          gb.xoffs, gb.yoffs, gb.wwidth, gb.wheight, 0, 0);
    DrawTextCursor(dpy, gb.window, defaultGC);
    DrawAnchor(dpy, gb.window, defaultGC);
    XFlush(dpy);
}

static void RefreshCanvas (widget, client_data, cb)
    Widget widget;
    caddr_t client_data;
    XmDrawingAreaCallbackStruct *cb;
{
    XRectangle rectangle;

    do {
        rectangle.x = (short) cb->event->xexpose.x;
        rectangle.y = (short) cb->event->xexpose.y;
        rectangle.width = (unsigned short) cb->event->xexpose.width;
        rectangle.height = (unsigned short) cb->event->xexpose.height;
        XUnionRectWithRegion(&rectangle, clipRegion, clipRegion);
    } while (XCheckTypedWindowEvent(dpy, gb.window, Expose, cb->event));

    XSetRegion(dpy, clipGC, clipRegion);
    XCopyArea(dpy, gb.pixmap, gb.window, clipGC,
        gb.xoffs, gb.yoffs, gb.wwidth, gb.wheight, 0, 0);
    DrawAnchor(dpy, gb.window, clipGC);
    DrawTextCursor(dpy, gb.window, clipGC);
}
    
void SetGraphicsWindow (canvas)
    Widget canvas;
{
    Arg args[2];

    gb.window = XtWindow(canvas);

    if (mode.function == Mode_Select) {
        XDefineCursor(dpy, gb.window, cursors.select);
    }

    XtSetArg(args[0], XmNwidth, &gb.wwidth);
    XtSetArg(args[1], XmNheight, &gb.wheight);
    XtGetValues(canvas, args, 2);

    XtSetArg(args[0], XmNsliderSize, gb.wwidth);
    XtSetArg(args[1], XmNincrement, gb.wwidth/4);
    XtSetValues(gb.horizSB, args, 2);

    XtSetArg(args[0], XmNsliderSize, gb.wheight);
    XtSetArg(args[1], XmNincrement, gb.wheight/4);
    XtSetValues(gb.vertSB, args, 2);
}

static void ResizeCanvas (canvas, client_data, cb)
    Widget canvas;
    caddr_t client_data;
    XmDrawingAreaCallbackStruct *cb;
{
    Arg args[5];
    Pixmap pixmap;
    int width, height;

    if (cb->window) SetGraphicsWindow(canvas);
}

Widget CreateCanvas (parent, name, wargs, wcount)
    Widget parent;
    char *name;
    Arg *wargs;
    int wcount;
{
    Arg args[5];
    DPSContext ctxt;
    Widget scrollWin, canvas;
    long mask;
    int i;

    gb.xoffs = gb.yoffs = 0;
    gb.pwidth = WidthOfScreen(screen);
    gb.pheight = HeightOfScreen(screen);

    gb.pixmap = XCreatePixmap(dpy, RootWindowOfScreen(screen),
          gb.pwidth, gb.pheight, DefaultDepthOfScreen(screen));
    XSetForeground(dpy, defaultGC, WhitePixelOfScreen(screen));
    XFillRectangle(dpy, gb.pixmap, defaultGC, 0, 0, gb.pwidth, gb.pheight);
    XSetForeground(dpy, defaultGC, BlackPixelOfScreen(screen));

    scrollWin = XmCreateScrolledWindow(parent, "scrollWin", wargs, wcount);
    XtManageChild(scrollWin);

    i=0; XtSetArg(args[i], XmNvalue, gb.xoffs);
    i++; XtSetArg(args[i], XmNminimum, 0);
    i++; XtSetArg(args[i], XmNmaximum, gb.pwidth);
    i++; XtSetArg(args[i], XmNorientation, XmHORIZONTAL);
    i++; gb.horizSB = XmCreateScrollBar(scrollWin, "horizSB", args, i);
    XtManageChild(gb.horizSB);

    XtAddCallback(gb.horizSB,XmNdragCallback, (XtCallbackProc) ChangeOffset,
		  (XtPointer) XmHORIZONTAL);
    XtAddCallback(gb.horizSB,XmNvalueChangedCallback,
		  (XtCallbackProc) ChangeOffset, (XtPointer) XmHORIZONTAL);

    i=0; XtSetArg(args[i], XmNvalue, gb.yoffs);
    i++; XtSetArg(args[i], XmNminimum, 0);
    i++; XtSetArg(args[i], XmNmaximum, gb.pheight);
    i++; XtSetArg(args[i], XmNorientation, XmVERTICAL);
    i++; gb.vertSB = XmCreateScrollBar(scrollWin, "vertSB", args, i);
    XtManageChild(gb.vertSB);

    XtAddCallback(gb.vertSB, XmNdragCallback, (XtCallbackProc) ChangeOffset,
		  (XtPointer) XmVERTICAL);
    XtAddCallback(gb.vertSB, XmNvalueChangedCallback,
		  (XtCallbackProc) ChangeOffset, (XtPointer) XmVERTICAL);

    canvas = XmCreateDrawingArea(scrollWin, name, args, i);

    XtAddCallback(canvas, XmNresizeCallback, (XtCallbackProc) ResizeCanvas,
		  NULL);
    XtAddCallback(canvas, XmNexposeCallback, (XtCallbackProc) RefreshCanvas,
		  NULL);

    mask=ButtonPressMask|ButtonReleaseMask|ButtonMotionMask;
    XtAddEventHandler(canvas, mask, False, (XtEventHandler) MouseEventHandler,
		      NULL);

    mask=KeyPressMask|KeyReleaseMask;
    XtAddEventHandler(canvas, mask, False, (XtEventHandler) TextEventHandler,
		      NULL);

    XmScrolledWindowSetAreas(scrollWin, gb.horizSB, gb.vertSB, canvas);

    ctxt = DPSGetCurrentContext();
    DPSWsetXgcdrawable(ctxt, XGContextFromGC(defaultGC),
		       gb.pixmap, 0, gb.pheight);
    DPSinitmatrix(ctxt);

    matrixInit();

    mode.outline = False;
    mode.function = Mode_Select;

    cursors.select = XCreateFontCursor(dpy, XC_hand2);
    cursors.resize = XCreateFontCursor(dpy, XC_sizing);
    cursors.rotate = XCreateFontCursor(dpy, XC_exchange);
    cursors.delete = XCreateFontCursor(dpy, XC_pirate);
    cursors.slant  = XCreateFontCursor(dpy, XC_diamond_cross);
    cursors.reset = XCreateFontCursor(dpy, XC_double_arrow);
    cursors.anchor = XCreateFontCursor(dpy, XC_crosshair);
    anchor.x = anchor.y = -10; /* draw off screen */
    textcursor.x = textcursor.y = -10; /* draw off screen */

    clipGC = XCreateGC(dpy, RootWindowOfScreen(screen), 0, NULL);
    XSetForeground(dpy, defaultGC, BlackPixelOfScreen(screen));
    XSetBackground(dpy, defaultGC, WhitePixelOfScreen(screen));

    emptyRegion = XCreateRegion();
    clipRegion = XCreateRegion();

    return(canvas);
}
