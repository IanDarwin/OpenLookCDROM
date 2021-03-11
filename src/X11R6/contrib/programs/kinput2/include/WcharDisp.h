/* $Id: WcharDisp.h,v 1.4 1991/09/17 10:08:27 ishisone Rel $ */
/*
 * Copyright (c) 1990  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

#ifndef _WcharDisplay_h
#define _WcharDisplay_h

/* WcharDisplay and JpWcharDisplay object public header file */

/*
  WcharDisplay new resources:

  name		class		type		default		access
  ----------------------------------------------------------------------------
  fontG0	Font		FontStruct	XtDefaultFont	CSG
  fontG1	Font		FontStruct	NULL		CSG
  fontG2	Font		FontStruct	NULL		CSG
  fontG3	Font		FontStruct	NULL		CSG
  fontMapping	FontMapping	FontMappng	GL/GL/GL/GL	CSG


  JpWcharDisplay new resources:

  name		class		type		default	access	note
  ----------------------------------------------------------------------------
  font		Font		FontStruct	*1	CSG	*4
  kanjiFont	KanjiFont	FontStruct	*2	CSG	*5
  kanaFont	KanaFont	FontStruct	*3	CSG	*6

  *1) "-Misc-Fixed-Medium-R-*--14-*-*-*-C-*-ISO8859-1"
  *2) "-Misc-Fixed-Medium-R-*--14-*-*-*-C-*-JISX0208.1983-0"
  *3) "-Misc-Fixed-Medium-R-*--14-*-*-*-C-*-JISX0201.1976-0"

  note: *4) this resource overrides superclass's fontG0 resource
	*5) this resource overrides superclass's fontG1 resource
	*6) this resource overrides superclass's fontG2 resource


  JpWcharDisplay override resource:

  name		class		type		default	access	note
  ----------------------------------------------------------------------------
  fontMapping	FontMapping	FontMappng	GL/GL/GR/GL	CSG

*/

#include "ConvDisp.h"
#include "WStr.h"

#define XtNfontG0 "fontG0"
#define XtNfontG1 "fontG1"
#define XtNfontG2 "fontG2"
#define XtNfontG3 "fontG3"

#define XtNfontMapping "fontMapping"
#define XtCFontMapping "FontMapping"

#define XtNkanjiFont "kanjiFont"
#define XtCKanjiFont "KanjiFont"
#define XtNkanaFont "kanaFont"
#define XtCKanaFont "KanaFont"

typedef struct _WcharDisplayClassRec	*WcharDisplayObjectClass;
typedef struct _WcharDisplayRec		*WcharDisplayObject;

extern WidgetClass	wcharDisplayObjectClass;

typedef struct _JpWcharDisplayClassRec	*JpWcharDisplayObjectClass;
typedef struct _JpWcharDisplayRec	*JpWcharDisplayObject;

extern WidgetClass	jpWcharDisplayObjectClass;

#endif /* _WcharDisplay_h */

