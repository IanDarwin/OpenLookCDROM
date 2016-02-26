/*
 * @(#)guide_actions.h	2.12 91/10/15 Copyright 1989 Sun Microsystems.
 *
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */


/*
 * The set of "core" whens that guide handles.
 */
#define MOUSE_SELECT	"Select"
#define MOUSE_ADJUST	"Adjust"
#define MOUSE_MENU	"Menu"
#define ENTER		"Enter"
#define EXIT		"Exit"
#define	NOTIFY		"Notify"
#define ANYEVENT 	"AnyEvent"
#define REPAINT		"Repaint"
#define RESIZE		"Resize"
#define DONE		"Done"
#define DOUBLECLICK	"DoubleClick"
#define	KEYPRESS	"KeyPress"
#define	DROPPED_UPON	"DroppedUpon"
#define	DRAGGED_FROM	"DraggedFrom"

/*
 * The set of "core" actions that guide handles.
 */
#define ATTACHMENU	"Attach"
#define CALLFUNCTION	"CallFunction"
#define DISABLE		"Disable"
#define DESELECT	"Deselect"
#define ENABLE		"Enable"
#define EXECUTECODE	"ExecuteCode"
#define GETLABEL	"GetLabel"
#define GETLEFTFOOTER	"GetLeftFooter"
#define GETRIGHTFOOTER	"GetRightFooter"
#define GETVALUENUMBER	"GetValueNumber"
#define GETVALUESTRING	"GetValueString"
#define HIDE		"Hide"
#define LOADTEXTFILE	"LoadTextFile"
#define SETLABEL	"SetLabel"
#define SETLEFTFOOTER	"SetLeftFooter"
#define SETRIGHTFOOTER	"SetRightFooter"
#define SETSELECTED	"SetSelected"
#define SETVALUENUMBER	"SetValueNumber"
#define SETVALUESTRING	"SetValueString"
#define SHOW		"Show"


EXTERN_FUNCTION( G_MSG_STRUCT	*G_all_default_events,		(_VOID_) );
EXTERN_FUNCTION( G_MSG_STRUCT	*G_all_default_messages,	(_VOID_) );
EXTERN_FUNCTION( G_TYPES	*G_all_default_receivers,	(_VOID_) );
EXTERN_FUNCTION( G_MSG_STRUCT	*G_default_events,		(G_TYPES) );
EXTERN_FUNCTION( G_MSG_STRUCT	*G_default_messages,		(G_TYPES) );
EXTERN_FUNCTION( G_TYPES	*G_default_receivers,		(G_TYPES) );
