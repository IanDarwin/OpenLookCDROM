#ifndef _TERMCANVAS_H
#define _TERMCANVAS_H

/* TermCanvas.h,v 1.3 1994/06/02 10:58:15 me Exp */

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/*
 * TermCanvas public include file.
 *
 * Author: Michael Elbel
 * Date: March 20th, 1990.
 * Description: This file contains all the public includes for the
 *		TermCanvas widget.
 *
 * Revision History:
 *
 * TermCanvas.h,v
 * Revision 1.3  1994/06/02  10:58:15  me
 * Changed float interfaced functions to double
 *
 * Revision 1.2  1994/05/24  19:55:26  me
 * New Copyright
 * fixed bug where the text cursor dissappeared upon a move_abs
 *
 * Revision 1.1.1.1  1994/05/22  11:22:41  me
 * Initial import into CVS
 *
 */

/****************************************************************
 *
 * TermCanvas widget
 *
 ****************************************************************/

/* Resources:
 * 
 * Name		     Class		RepType		Default Value
 * ----		     -----		-------		-------------
 * lines             Lines              Cardinal        24 (read only)
 * columns           Columns            Cardinal        80 (read only)
 * cellWidth	     CellWidth		Cardinal	0 (read only)
 * cellHeigth	     CellHeight		Cardinal	0 (read only)
 * font	     	     Font		FontStruct	XtDefaultFont
 * boldFont	     BoldFont		FontStruct	derived from font
 * IF DOUBLE_FONTS is DEFINED
 * dWideFont	     DWideFont		FontStruct	derived from font
 * dWHighFont	     DWHighFont		FontStruct	derived from font
 * dWideBFont	     DWideBFont		FontStruct	derived from font
 * dWHighBFont	     DWHighBFont	FontStruct	derived from font
 * ENDIF DOUBLE_FONTS is DEFINED
 * underlineWidth    UnderlineWidth	Cardinal	1
 * foreground	     Foreground		String		black
 * background	     Background		String		white
 * cursorFg	     CursorFg		String          black
 * cursorBg	     CursorBg		String          white
 * cursorHeight      CursorHeight       Cardinal        0
 * cursorWidth       CursorWidth        Cardinal        0
 * cursorBlinking    CursorBlinking     Boolean         True
 * blinkInterval     BlinkInterval      Cardinal        500 msec
 * blinkWOFocus      BlinkWOFocus       Boolean         False
 * textBlinkInterval TextBlinkInterval  Cardinal	500 msec
 * wrapAround	     WrapAround         Boolean         True
 * insertMode	     InsertMode		Boolean		False
 * bellVolume	     BellVolume		Cardinal	0 %
 * defTabWidth	     DefTabWidth	Cardinal	8
 * termType	     TermType		String		NULL
 * setSize	     SetSize		Pointer		NULL
 * output	     Output		Pointer		NULL
 * notifyFirstMap    NotifyFirstMap	Pointer		NULL
 * selectionInverse  SelectionInverse	Boolean		False
 * pointerShape	     PointerShape	Cursor		xterm
 * pointerColor	     PointerColor	String		foreground
 * reverseVideo      ReverseVideo	Boolean		False
 * saveLines	     SaveLines		Cardinal	64 or 'lines',
 * 							whichever is larger
 * adjustScrollBar   AdjustScrollBar	Pointer		NULL
 * multiClickTime    MultiClickTime	Cardinal	300 msec
 * jumpScrollLines   JumpScrollLines	Cardinal	10
 */

/* resource types */
#define XpNlines		"lines"
#define XpNcolumns		"columns"
#define XpNcellWidth		"cellWidth"
#define XpNcellHeight		"cellHeight"
#define XpNboldFont		"boldFont"
#ifdef DOUBLE_FONTS
#define XpNdWideFont		"dWideFont"
#define XpNdWHighFont		"dWHighFont"
#define XpNdWideBFont		"dWideBFont"
#define XpNdWHighBFont		"dWHighBFont"
#endif /* DOUBLE_FONTS */
#define XpNunderlineWidth	"underlineWidth"
#define XpNcursorFg		"cursorFg"
#define XpNcursorBg		"cursorBg"
#define XpNcursorHeight		"cursorHeight"
#define XpNcursorWidth		"cursorWidth"
#define XpNcursorBlinking	"cursorBlinking"
#define XpNblinkInterval	"blinkInterval"
#define XpNblinkWOFocus		"blinkWOFocus"
#define XpNtextBlinkInterval	"textBlinkInterval"
#define XpNwrapAround		"wrapAround"
#define XpNinsertMode		"insertMode"
#define XpNbellVolume		"bellVolume"
#define XpNdefTabWidth		"defTabWidth"
#define XpNdefaultTerm		"vt100c"
#define XpNtermType		"termType"
#define XpNsetSize		"setSize"
#define XpNoutput		"output"
#define XpNnotifyFirstMap	"notifyFirstMap"
#define XpNcomBlock		"comBlock"
#define XpNselectionInverse	"selectionInverse"
#define XpNpointerShape		"pointerShape"
#define XpNpointerColor		"pointerColor"
#define XpNsaveLines		"saveLines"
#define XpNadjustScrollBar	"adjustSBar"
#define XpNmultiClickTime	"multiClickTime"
#define XpNjumpScrollLines	"jumpScrollLines"

/* class types */
#define XpCLines		"Lines"
#define XpCColumns		"Columns"
#define XpCCellWidth		"CellWidth"
#define XpCCellHeight		"CellHeight"
#define XpCBoldFont		"BoldFont"
#ifdef DOUBLE_FONTS
#define XpCDWideFont		"DWideFont"
#define XpCDWHighFont		"DWHighFont"
#define XpCDWideBFont		"DWideBFont"
#define XpCDWHighBFont		"DWHighBFont"
#endif /* DOUBLE_FONTS */
#define XpCUnderlineWidth	"UnderlineWidth"
#define XpCCursorFg		"CursorFg"
#define XpCCursorBg		"CursorBg"
#define XpCCursorHeight		"CursorHeight"
#define XpCCursorWidth		"CursorWidth"
#define XpCCursorBlinking	"CursorBlinking"
#define XpCBlinkInterval	"BlinkInterval"
#define XpCBlinkWOFocus		"BlinkWOFocus"
#define XpCTextBlinkInterval	"TextBlinkInterval"
#define XpCWrapAround		"WrapAround"
#define XpCInsertMode		"InsertMode"
#define XpCBellVolume		"BellVolume"
#define XpCDefTabWidth		"DefTabWidth"
#define XpCTermType		"TermType"
#define XpCSetSize		"SetSize"
#define XpCOutput		"Output"
#define XpCNotifyFirstMap	"NotifyFirstMap"
#define XpCComBlock		"ComBlock"
#define XpCSelectionInverse	"SelectionInverse"
#define XpCPointerShape		"PointerShape"
#define XpCPointerColor		"PointerColor"
#define XpCSaveLines		"SaveLines"
#define XpCAdjustScrollBar	"AdjustSBar"
#define XpCMultiClickTime	"MultiClickTime"
#define XpCJumpScrollLines	"JumpScrollLines"

/* declare specific TermCanvasWidget class and instance datatypes */

typedef struct _TermCanvasClassRec*	TermCanvasWidgetClass;
typedef struct _TermCanvasRec*		TermCanvasWidget;

/* The type of function to pass us for the scroll procedure hook */
typedef void (*ScrollFuncPtr)(Widget, double, double);

/* declare the class constant */
extern WidgetClass termCanvasWidgetClass;

#endif /* _TermCanvas_h */
