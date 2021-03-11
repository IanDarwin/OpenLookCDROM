/* -*-C-*-
********************************************************************************
*
* File:         TableP.h
* RCS:          $Header: /users/npm/src/winterp/src-server/widgets/RCS/TableP.h,v 2.5 1994/06/06 15:47:58 npm Exp $
* Description:  Table - Forms-based composite widget/geometry manager for the
*		X Toolkit. This file contains the Table private declarations.
* Author:       David Harrison, University of California, Berkeley
* Created:      1989
* Modified:     Sun Jun  5 04:15:16 1994 (Niels Mayer) npm@indeed
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

#ifndef _TableP_h
#define _TableP_h

#include "Table.h"
#include <Xm/XmP.h>
#include <Xm/BulletinBP.h>

/*
 * Local definitions
 */

typedef void (*XtTblRProc)();
  /*
   * Widget table;
   * Widget subwidget;
   * Position r, c;
   * Dimension hspan, vspan;
   * XtTblMask options;
   */

typedef Boolean (*XtTblLProc)();
   /*
    * Widget w;
    * Position *r, *c;
    * Dimension *hspan, *vspan;
    * XtTblMask *options;
    */

typedef struct _TableLocTbl *TableLocTblPtr;
   /*
    * Opaque reference to actual widget location table
    * defined in Table.c
    */

typedef struct _TableDefLoc *TableDefLocPtr;
   /*
    * Opaque reference to default widget location table defined
    * in Table.c.
    */    

typedef struct _TableVector *TableVecPtr;
   /*
    * Opaque reference to vectors used for giving size of
    * each row and column.
    */

typedef enum _TableVecState { INVALID, MINIMUM } TableVecState;

/*
 * Information kept in class record
 */

typedef struct {
    XtTblRProc position_child;	/* Register location of some child widget  */
    XtTblLProc find_child;	/* Return information about a child widget */
} TableClassPart;

/*
 * Class hierarchy
 */

typedef struct _TableClassRec {
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ConstraintClassPart	constraint_class;
    XmManagerClassPart	manager_class;
    XmBulletinBoardClassPart bulletin_class;
    TableClassPart	table_class;
} TableClassRec;

#ifndef _Table_c
EXTERNREF TableClassRec tableClassRec;
#endif

/*
 * Information in instance record
 */

typedef struct _TablePart {
    Dimension		int_width;   /* Inner horizontal padding          */
    Dimension		int_height;  /* Inner vertical padding            */
    Dimension		row_spacing; /* Space between rows                */
    Dimension		col_spacing; /* Space between columns             */
    XtTblMask		def_options; /* Default layout options            */
    TableDefLocPtr	init_layout; /* Initial layout spec from resource */
    TableDefLocPtr	layout_db;   /* Merged table                      */
    TableLocTblPtr	real_layout; /* Actual current layout information */
    TableVecState	vec_state;   /* Current state of vectors          */
    Cardinal		num_rows;    /* Number of rows                    */
    TableVecPtr		rows;	     /* Heights of each row               */
    Cardinal		num_cols;    /* Number of columns                 */
    TableVecPtr		cols;	     /* Widths of each column             */
    Cardinal		vec_height;  /* Sum of current rows               */
    Cardinal		vec_width;   /* Sum of current columns            */
} TablePart;

/*
 * Instance hierarchy
 */

typedef struct _TableRec {
    CorePart		core;
    CompositePart	composite;
    ConstraintPart		constraint;
    XmManagerPart		manager;
    XmBulletinBoardPart	bulletin_board;
    TablePart		table;
} TableRec;

#endif /* _TableP_h */
