/* canvas.h
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

typedef enum {
    Mode_Select, Mode_Delete, Mode_Resize,
    Mode_Rotate, Mode_Slant, Mode_Outline, Mode_Reset
} GraphicsFunction;

/* global functions and variables */
#ifndef __CANVAS__
    extern void ChangeMode(/* Widget, GraphicsFunction, caddr_t */);
    extern void SetScrollBarSliders (/* int, int */);
    extern Widget CreateCanvas (/* Widget, char*, Arg*, int */);
    extern int GetCanvasHeight (/* void */);
    extern void SetCursorSize (/* width, height */);
    extern void PrintPage (/*widget, caddr_t, caddr_t */);
    extern void ErasePage (/*widget, caddr_t, caddr_t */);
    extern void SetGraphicsWindow (/* widget */);
    extern void appendMode(/*widget, caddr_t, caddr_t */);
    extern void loadMode(/*widget, caddr_t, caddr_t */);
    extern void selectAllCallback(/*widget, caddr_t, caddr_t */);
    extern void kernLetters(/*widget, caddr_t, caddr_t */);
    extern void LoadAIFile (/*widget, caddr_t, caddr_t */);
    extern void SaveAIFile (/*widget, caddr_t, caddr_t */);
#endif
