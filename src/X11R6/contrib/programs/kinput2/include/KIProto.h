/* $Id: KIProto.h,v 1.3 1991/08/22 06:01:57 ishisone Rel $ */
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

#ifndef _KinputProtocol_h
#define _KinputProtocol_h

#include "ICtypes.h"

/*
  KinputProtocol new resources:

  name			class			type	default		access
  ----------------------------------------------------------------------------
  language		Language		String	NULL		CG
  inputObjectClass	Class			Pointer	NULL		CG
  displayObjectClass	Class			Pointer	NULL		CG
  backwardCompatible	BackwardCompatible	Boolean	False		CG
  xlcConversionStartKey	XlcConversionStartKey	String	NULL		CG

*/

#define XtNlanguage "language"
#define XtCLanguage "Language"

#define XtNinputObjectClass "inputObjectClass"
#define XtNdisplayObjectClass "displayObjectClass"
#define XtCClass "Class"

#define XtNbackwardCompatible "backwardCompatible"
#define XtCBackwardCompatible "BackwardCompatible"

#define XtNxlcConversionStartKey "xlcConversionStartKey"
#define XtCXlcConversionStartKey "XlcConversionStartKey"

typedef struct _KinputProtocolClassRec*	KinputProtocolWidgetClass;
typedef struct _KinputProtocolRec*	KinputProtocolWidget;

extern WidgetClass kinputProtocolWidgetClass;

#endif
