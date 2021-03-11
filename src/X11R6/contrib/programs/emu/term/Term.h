#ifndef _TERM_H
#define _TERM_H

/* Term.h,v 1.2 1994/05/27 06:20:51 me Exp */

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
 * Term widget public include file.
 *
 * Author: Jordan K. Hubbard
 * Date: June 6th, 1990.
 * Description: Everything a client of the term widget is going to need.
 *
 * Revision History:
 *
 * Term.h,v
 * Revision 1.2  1994/05/27  06:20:51  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:44  me
 * Initial import into CVS
 * 
 * Revision 1.6  91/09/30  21:41:59  jkh
 * Checkin prior to DECWRL merge.
 * 
 * Revision 1.4  90/11/13  14:57:43  jkh
 * Double file descriptor version.
 */

/* Resources:
 * 
 * Name			Class		RepType		Default Value
 * ----			-----		-------		-------------
 * command		Command		String		XpNdefaultCommand
 * commandArgs		CommandArgs	Pointer		NULL
 * termType		TermType	String		XpNdefaultTermType
 * canvas		Canvas		Widget		NULL
 * readSize		ReadSize	Int		XpNdefaultReadSize
 * iopRequest		IopRequest	Callback	NULL
 * layoutProc		LayoutProc	Callback	NULL
 * childDeath		ChildDeath	Callback	NULL
 * inParser		InParser	Callback	NULL
 * outParser		OutParser	Callback	NULL
 * utmpInhibit		UtmpInhibit	Boolean		FALSE
 * loginShell		LoginShell	Boolean		FALSE
 * rops			Rops		String		NULL
 *
 */

/* resource types */
#define XpNcommand		"command"
#define XpNcommandArgs		"commandArgs"
#define XpNtermType		"termType"
#define XpNcanvas		"canvas"
#define XpNiopRequestProc	"iopRequestProc"
#define XpNlayoutProc		"layoutProc"
#define XpNprocessDeath		"processDeath"
#define XpNreadSize		"readSize"
#define XpNinParser		"inParser"
#define XpNoutParser		"outParser"
#define XpNutmpInhibit		"utmpInhibit"
#define XpNloginShell		"loginShell"
#define XpNrops			"rops"

/* class types */
#define XpCCommand		"Command"
#define XpCCommandArgs		"CommandArgs"
#define XpCTermType		"TermType"
#define XpCCanvas		"Canvas"
#define XpCIopRequestProc	"IopRequestProc"
#define XpCLayoutProc		"LayoutProc"
#define XpCProcessDeath		"ProcessDeath"
#define XpCReadSize		"ReadSize"
#define XpCInParser		"InParser"
#define XpCOutParser		"OutParser"
#define XpCUtmpInhibit		"UtmpInhibit"
#define XpCLoginShell		"LoginShell"
#define XpCRops			"Rops"

/* Defaults */
#define XpNdefaultCommand	"/bin/sh"
#define XpNdefaultReadSize	64
#define XpNdefaultTermType	"vt220"

/* declare specific TermWidget class and instance datatypes */

typedef struct _TermClassRec	*TermWidgetClass;
typedef struct _TermRec		*TermWidget;

/* declare the class constant */
extern WidgetClass termWidgetClass;

/* public procedures */
extern Boolean XpTermDoEmulation(TermWidget, String);
extern void XpTermDoRop(TermWidget, int);
extern void XpTermDispatchRequest(TermWidget, int);
/* extern int XpTermCanvasDispatch(Widget, ComBlockPtr, Boolean); */
extern void XpTermSetRegister(TermWidget, unsigned char, int, caddr_t);
extern void XpTermInsertText(TermWidget, String, int);
extern caddr_t XpTermGetRegister(TermWidget, unsigned char, int *);
extern caddr_t XpTermComBlock(TermWidget);	/* real value is opaque */

#endif /* _TERM_H */
