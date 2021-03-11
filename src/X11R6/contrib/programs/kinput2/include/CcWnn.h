/* $Id: CcWnn.h,v 1.4 1991/09/30 10:54:58 ishisone Rel $ */
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

#ifndef _CcWnn_h
#define _CcWnn_h

#include "InputConv.h"

/*
  CcWnn new resources:

  name			class		type		default		access
  ----------------------------------------------------------------------------
  confirmFunc		Function	Pointer		NULL		CG
  confirmData		ConfirmData	Ponter		NULL		CG
  jserver		Jserver		String		*1		CG
  jserver2nd		Jserver		String		NULL		CG
  wnnEnvName		WnnEnvName	String		*2		CG
  wnnEnvrc		WnnEnvrc	String		*3		CG
  wnnOverrideEnv	WnnOverrideEnv	Boolean		False		CG
  wnnEnv		WnnEnv		WnnEnv		*4		CG
  ccdef			Ccdef		String		"ccdef.kinput"	CG
  ccRule		CcRule		CcRule		*4		CG
  saveInterval		SaveInterval	Int		0 (*5)		CG

  note:	*1) if not specified, use value of an environment variable "JSERVER"
	*2) if not specified, use user's loginname
	*3) if not specified, use value of an environment variable "WNNENVRC"
	*4) if not specified, create internally
	*5) 0 means files are never saved
*/

#define XtNconfirmFunc	"confirmFunc"
#define XtNconfirmData	"confirmData"
#define XtCConfirmData	"ConfirmData"
#define XtNjserver	"jserver"
#define XtNjserver2nd	"jserver2nd"
#define XtCJserver	"Jserver"
#define XtNwnnEnvname	"wnnEnvname"
#define XtCWnnEnvname	"WnnEnvname"
#define XtNwnnEnvrc	"wnnEnvrc"
#define XtCWnnEnvrc	"WnnEnvrc"
#define XtNwnnOverrideEnv	"wnnOverrideEnv"
#define XtCWnnOverrideEnv	"WnnOverrideEnv"
#define XtNccdef	"ccdef"
#define XtCCcdef	"Ccdef"
#define XtNwnnEnv	"wnnEnv"
#define XtCWnnEnv	"WnnEnv"
#define XtRWnnEnv	"WnnEnv"
#define XtNccRule	"ccRule"
#define XtCCcRule	"CcRule"
#define XtRCcRule	"CcRule"
#define XtNsaveInterval	"saveInterval"
#define XtCSaveInterval	"SaveInterval"

#define DEF_CCDEF_FILE	"ccdef.kinput"	/* for backward compatibility */

typedef struct _CcWnnClassRec	*CcWnnObjectClass;
typedef struct _CcWnnRec	*CcWnnObject;

extern WidgetClass	ccWnnObjectClass;

#endif /* _CcWnn_h */

