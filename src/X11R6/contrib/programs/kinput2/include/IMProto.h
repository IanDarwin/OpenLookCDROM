/* $Id: IMProto.h,v 1.3 1994/05/31 07:54:39 ishisone Rel $ */
/*
 * Copyright (c) 1994  Software Research Associates, Inc.
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

#ifndef _IMProtocol_h
#define _IMProtocol_h

#include "ICtypes.h"

/*
  IMProtocol new resources:

  name			class			type	default		access
  ----------------------------------------------------------------------------
  serverName		ServerName		String	"kinput2"	CG
  language		Language		String	NULL		CG
  locales		Locales			String	NULL		CG
  inputObjectClass	Class			Pointer	NULL		CG
  displayObjectClass	Class			Pointer	NULL		CG
  defaultFontList	FontList		String	NULL		CG
  foreground		Foreground		Pixel	DefaultForeground
									CG
  transports		Transports		String	"tcp,unix,x"	CG
*/

#define XtNserverName "serverName"
#define XtCServerName "ServerName"

#define XtNlanguage "language"
#define XtCLanguage "Language"

#define XtNlocales "locales"
#define XtCLocales "Locales"

#define XtNinputObjectClass "inputObjectClass"
#define XtNdisplayObjectClass "displayObjectClass"
#define XtCClass "Class"

#define XtNdefaultFontList "defaultFontList"
#define XtCFontList "FontList"

#define XtNconversionStartKeys "conversionStartKeys"
#define XtCConversionStartKeys "ConversionStartKeys"

#define XtNstatusWidth "statusWidth"
#define XtCStatusWidth "StatusWidth"

#define XtNtransports "transports"
#define XtCTransports "Transports"

typedef struct _IMProtocolClassRec*	IMProtocolWidgetClass;
typedef struct _IMProtocolRec*		IMProtocolWidget;

extern WidgetClass imProtocolWidgetClass;

#endif /* _IMProtocol_h */
