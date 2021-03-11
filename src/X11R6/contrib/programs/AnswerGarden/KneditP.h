
/* KneditP.h */

/***********************************************************
  -- Copyright (c) 1994 Regents of the University of California.
  -- All rights reserved.
  --
  -- This software was developed by the Answer Garden project
  -- at the University of California, Irvine.
  --
  -- Redistribution and use in source and binary forms are permitted
  -- provided that the above copyright notice and this paragraph are
  -- duplicated in all such forms and that any documentation,
  -- advertising materials, and other materials related to such
  -- distribution and use acknowledge that the software was developed
  -- by the University of California, Irvine.  The name of the
  -- University may not be used to endorse or promote products derived
  -- from this software without specific prior written permission.
  -- THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
  -- IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
  -- WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
  
  -- Answer Garden is a trademark of the Regents of the University of
  -- California.  All rights reserved.


******************************************************************/

/* 
 * KneditP.h - Private definitions for Knedit widget
 * 
 */

#ifndef _KneditP_h
#define _KneditP_h

/***********************************************************************
 *
 * Knedit Widget Private Data
 *
 ***********************************************************************/

#include "Knedit.h"
#include "LayoutP.h"

/*** These should move somewhere else so they don't
     bother a subclasser. ***/
#define MAXBUF 256
#define MAXCHAR   2048  /* make so it's alloc'ed or a resource */

/* New fields for the Knedit widget class record */
typedef struct {int makes_compiler_happy;} KneditClassPart;

/* Full class record declaration */
typedef struct _KneditClassRec {
    CoreClassPart	core_class;
    CompositeClassPart  composite_class;
    LayoutClassPart	layout_class;
    KneditClassPart     knedit_class;
} KneditClassRec;

extern KneditClassRec kneditClassRec;

/* New fields for the Knedit widget record */


typedef struct {
  /* resources */
    Position left_margin;
    Position right_margin;
    Position top_margin;
    Dimension line_height; /* height of each line including leading */
    Dimension separator_height; /* distance between paragraphs */
    Dimension v_space; /* extra spacing for buttons */
    XFontStruct *font;
    XFontStruct *bold_font;
    XFontStruct *italic_font;
    String filename;
    Pixel foreground;
    XtCallbackList internal_button_callback;
    XtCallbackList edit_callback;
    Cursor cursor;
    Dimension total_height;
    Boolean force_size;
    Boolean edit_mode;    
    String knedit_node_name;
    XtCallbackList dynamic_callback;
    Widget func_obj;

  /* private */
    GC normal_gc;
    GC italic_gc;
    GC bold_gc;
    int when_flag;
    Boolean error_flag;
    String textbuffer;
    XtPointer first_passback;

} KneditPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _KneditRec {
    CorePart	    core;
    CompositePart   composite;
    LayoutPart 	    layout;
    KneditPart      knedit;
} KneditRec;

#endif /* _KneditP_h */

