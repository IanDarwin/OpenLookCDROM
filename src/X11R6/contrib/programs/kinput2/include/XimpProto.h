/* $Id: XimpProto.h,v 1.5 1993/09/07 06:33:46 ishisone Rel $ */
/*
 * Copyright (c) 1991  Software Research Associates, Inc.
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

#ifndef _XimpProtocol_h
#define _XimpProtocol_h

#include "ICtypes.h"

/*
  XimpProtocol new resources:

  name			class			type	default		access
  ----------------------------------------------------------------------------
  localeName		LocaleName		String	NULL		CG
  serverName		ServerName		String	"kinput2"	CG
  inputObjectClass	Class			Pointer	NULL		CG
  displayObjectClass	Class			Pointer	NULL		CG
  defaultFontList	FontList		String	NULL		CG
  foreground		Foreground		Pixel	DefaultForeground
									CG
  forceDefaultServer	ForceDefaultServer	Boolean False		CG

*/

#define XtNlocaleName "localeName"
#define XtCLocaleName "LocaleName"

#define XtNserverName "serverName"
#define XtCServerName "ServerName"

#define XtNinputObjectClass "inputObjectClass"
#define XtNdisplayObjectClass "displayObjectClass"
#define XtCClass "Class"

#define XtNdefaultFontList "defaultFontList"
#define XtCFontList "FontList"

#define XtNforceDefaultServer "forceDefaultServer"
#define XtCForceDefaultServer "ForceDefaultServer"

#define XtNconversionStartKeys "conversionStartKeys"
#define XtCConversionStartKeys "ConversionStartKeys"

#define XtNstatusWidth "statusWidth"
#define XtCStatusWidth "StatusWidth"

typedef struct _XimpProtocolClassRec*	XimpProtocolWidgetClass;
typedef struct _XimpProtocolRec*	XimpProtocolWidget;

extern WidgetClass ximpProtocolWidgetClass;

#endif
