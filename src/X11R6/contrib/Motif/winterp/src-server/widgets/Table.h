/* -*-C-*-
********************************************************************************
*
* File:         Table.h
* RCS:          $Header: /users/npm/src/winterp/src-server/widgets/RCS/Table.h,v 2.5 1994/06/06 15:47:58 npm Exp $
* Description:  Table - Forms-based composite widget/geometry manager for the
*		X Toolkit. This file contains the Table public declarations.
* Author:       David Harrison, University of California, Berkeley
* Created:      1989
* Modified:     Sun Jun  5 04:15:21 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Table widget by David Harrison, University of California, Berkeley.
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, Niels Mayer, and David Harrison not be used in
* advertising or publicity pertaining to distribution of the software without
* specific, written prior permission. Enterprise Integration Technologies, 
* Hewlett-Packard Company, Niels Mayer, and David Harrison makes no
* representations about the suitability of this software for any purpose. It
* is provided "as is" without express or implied warranty.
*
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
* AND DAVID HARRISON DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
* INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
* SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS
* MAYER, AND DAVID HARRISON BE LIABLE FOR ANY SPECIAL, INDIRECT OR
* CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
* DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
* TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
********************************************************************************
*/

/*  -- The following defines external storage class for data DEFINITION and
    -- REFERENCE. The VAX LINKER strictly demands clear distinction between
    -- data definition for external reference, and reference to external data. 
*/
#ifdef VAX
#define EXTERNDEF globaldef
#define EXTERNREF globalref
#else
#define EXTERNDEF
#define EXTERNREF extern
#endif

 
#ifndef _Table_h
#define _Table_h

#include <Xm/Xm.h>
/*
 * Table Widget Parameters
 *
 * Name			Class		RepType		Default Value
 *
 * background		Background	Pixel		XtDefaultBackground
 * borderColor		BorderColor	Pixel		XtDefaultForeground
 * borderWidth		BorderWidth	Dimension	0
 * x			Position	Position	0
 * y			Position	Position	0
 * width		Width		Dimension	(computed)
 * height		Height		Dimension	(computed)
 * mappedWhenManaged	MappedWhenManaged Boolean	True
 * sensitive		Sensitive	Boolean		True
 * layout		Layout		String		NULL
 * marginHeight 	Height		Dimension	0
 * marginWidth  	Width		Dimension	0
 * verticalSpacing	Spacing		Dimension	0
 * horizontalSpacing	Spacing		Dimension	0
 */

#define XmNlayout		"layout"
#define XmCLayout		"Layout"
#define XmRLayout		"Layout"

#define XmNdefaultOptions	"defaultOptions"
#define XmCOptions		"Options"
#define XmROptions		"Options"

#ifndef WINTERP
#define XtNcolumnSpacing        "columnSpacing"
#define XtNrowSpacing           "rowSpacing"
#ifndef XtCSpacing
#define XtCSpacing		"Spacing"
#endif
#endif


/*
 * Option masks
 */
#define TBL_LEFT	(1<<0)
#define TBL_RIGHT	(1<<1)
#define TBL_TOP		(1<<2)
#define TBL_BOTTOM	(1<<3)
#define TBL_SM_WIDTH	(1<<4)
#define TBL_SM_HEIGHT	(1<<5)	
#define TBL_LK_WIDTH	(1<<6)
#define TBL_LK_HEIGHT	(1<<7)

#define TBL_DEF_OPT	-1

typedef int XtTblMask;

/*
 * Opaque class and instance records
 */

typedef struct _TableClassRec	*TableWidgetClass;
typedef struct _TableRec	*TableWidget;

#ifndef _Table_c
EXTERNREF WidgetClass tableWidgetClass;
#endif

/*
 * Public access routines
 */

#ifdef WINTERP
void TblClassInitialize();
#endif /* WINTERP */

extern caddr_t XtTblParseLayout();
  /* String layout; */

extern void XtTblPosition();
  /* 
   * Widget w;
   * Position col, row;
   */

extern void XtTblResize();
  /*
   * Widget w;
   * Dimension h_span, v_span;
   */

extern void XtTblOptions();
  /*
   * Widget w;
   * XtTblMask opt;
   */

extern void XtTblConfig();
  /* 
   * Widget w;
   * Position col, row;
   * Dimension h_span, v_span;
   * XtTblMask opt;
   */

extern Widget XtCreateTable();
/*
Widget parent;
char * name;
ArgList arglist;
Cardinal argcount;
*/
 
#endif /* _Table_h */
