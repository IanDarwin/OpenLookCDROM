/*
 *
 * 	funcs.h 
 * 	prototypes des fonctions du toolkit
 *
 * 	Modification :  23/04/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute and sell this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appears in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Bruno RIVAS not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  Bruno RIVAS makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Bruno RIVAS disclaims all warranties with regard to this software,
 * including all implied warranties of merchantability and fitness,
 * in no event shall Bruno RIVAS be liable for any special,
 * indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits,
 * whether in an action of contract, negligence or other tortious
 * action, arising out of or in connection with the use 
 * or performance of this software.
 *
 *
 *	Bruno RIVAS 
 *      IMAN Development Toolkit version 1.2
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */




#ifndef _IMAN_TK_FUNCS_H
#define _IMAN_TK_FUNCS_H


#if NeedFunctionPrototypes




/****************************************************************************
 *									    *
 *				      TK				    *
 *									    *
 ****************************************************************************/



extern int tk_GetConnection(
	TkDisplay *
);


extern int tk_RefreshColors(
	TkDisplay *
);


extern unsigned long tk_GetDoubleClickSpeed();


extern TkDisplay *tk_OpenSession(
	char *,
	int ,
	char **
);

int _TK_CopyColors(
	WidgetColors*,
	WidgetColors *
);

extern int tk_SetSystemColors(
	TkDisplay *,
	unsigned int,
	WidgetColors *,
	Bool
);

extern int tk_CloseSession(
	TkDisplay *
);


extern int tk_LoadSystemColorsFromFile(
	TkDisplay *,
	char *
);


extern int help_GetConnection(
	TkDisplay *
);

extern int mGetHsVersion(
	TkDisplay *
);


extern int mGetHsRelease(
	TkDisplay *
);


extern char *mGetHsComment(
	TkDisplay *
);


extern Window mGetHsMainWindow(
	TkDisplay *
);


extern Bool mIsHsActive(
	TkDisplay *
);



extern Display *mGetDisplay(
	TkDisplay *
);


extern char *mGetVendorString(
	TkDisplay *
);


extern int mGetTkVersion(
	TkDisplay *
);


extern int mGetTkRelease(
	TkDisplay *
);


extern char *mGetTkComment(
	TkDisplay *
);


extern int mGetWmVersion(
	TkDisplay *
);


extern int mGetWmRelease(
	TkDisplay *
);


extern char *mGetWmComment(
	TkDisplay *
);


extern Window mGetWmMainWindow(
	TkDisplay *
);


extern Bool mIsWmActive(
	TkDisplay *
);


extern int mGetDefaultDepth(
	TkDisplay *
);



extern int tk_GetWidgetEvents(
	TkDisplay *,
	TkEvent *,
	int 
);


/****************************************************************************
 *									    *
 *				     WIDGETS				    *
 *									    *
 ****************************************************************************/


extern WidgetID *wid_GetSubWidgets(
	TkDisplay *,
	WidgetID,
	unsigned int *
);



extern WidgetID wid_Create(
	TkDisplay *,
	int , int, 
	Window , Window, 
	int , int,
	unsigned int, unsigned int,
	WidgetAttributes *,
	int 
);



extern int wid_Destroy(
	TkDisplay *,
	WidgetID
);


extern int WID_Add(
	TkDisplay *,
	unsigned int
);


extern int WID_Initialize(
	TkDisplay *,
	unsigned int 
);


extern int WID_Remove(
	TkDisplay *,
	unsigned int 
);


extern int WID_Free(
	TkDisplay *
);


extern int wid_Map(
	TkDisplay *,
	WidgetID
);


extern int wid_Unmap(
	TkDisplay *,
	WidgetID
);


extern int WID_Refresh(
	TkDisplay *,
	Window 
);


extern int wid_Refresh(
	TkDisplay *,
	WidgetID
);


extern int WID_SeekNextFocus(
	TkDisplay *,
	Window , Window, Window,
	int 
);


extern int WID_SeekPreviousFocus(
	TkDisplay *,
	Window , Window, Window,
	int 
);


extern int _WID_Tempo(
	TkDisplay *
);



extern Bool WID_IsSubwidget(
	TkDisplay *,
	WidgetID , WidgetID
);


extern int WID_GetWindowType(
	TkDisplay *,
	Window 
);


extern int WID_GetWindowNumber(
	TkDisplay *,
	Window
);


extern int WID_GetUnusedNumber(
	TkDisplay *
);


extern int wid_GetState(
	TkDisplay *,
	WidgetID 
);


extern int wid_GetText(
	TkDisplay *,
	WidgetID ,
	unsigned char **
);


extern int wid_GetPosition(
	TkDisplay *,
	WidgetID 
);


extern int wid_GetAttributes(
	TkDisplay *,
	WidgetID,
	WidgetAttributes *
);


extern int wid_GetSelectedItems(
	TkDisplay *,
	WidgetID,
	unsigned int **,
	int *
);


extern int wid_GetNumItems(
	TkDisplay *,
	WidgetID 
);


extern int wid_GetItem(
	TkDisplay *,
	WidgetID ,
	unsigned int ,
	WidgetItem *
);


extern int wid_GetGeometry(
	TkDisplay *,
	WidgetID,
	Window *, Window *,
	int *, int *,
	unsigned int *, unsigned int *
);



extern int wid_SetFreeze(
	TkDisplay *,
	Window,
	Bool 
);


extern int WID_Freeze(
	TkDisplay *,
	Window 
);


extern int WID_Unfreeze(
	TkDisplay *,
	Window 
);


extern int WID_SetOverride(
	TkDisplay *,
	Bool
);


extern int wid_SetState(
	TkDisplay *,
	WidgetID ,
	unsigned int 
);


extern int wid_SetTextDecoration(
	TkDisplay *,
	WidgetID ,
	WidgetTextDecoration *,
	Bool 
);


extern int wid_SetPixmapDecoration(
	TkDisplay *,
	WidgetID ,
	WidgetPixmapDecoration *,
	Bool 
);


extern int wid_SetPixmapGrayedDecoration(
	TkDisplay *,
	WidgetID ,
	WidgetPixmapDecoration *,
	Bool 
);


extern int wid_SetAttributes(
	TkDisplay *,
	WidgetID ,
	WidgetAttributes *,
	Bool 
);


extern int wid_GiveFocus(
	TkDisplay *,
	WidgetID 
);


extern int wid_Configure(
	TkDisplay *,
	WidgetID ,
	int , int,
	unsigned int , unsigned int,
	unsigned long 
);


extern int wid_SetPosition(
	TkDisplay *,
	WidgetID,
	int 
);


extern int wid_SetPrivateColors(
	TkDisplay *,
	WidgetID,
	WidgetColors *,
	Bool
);


extern int wid_SetCursor(
	TkDisplay *,
	WidgetID,
	Cursor
);


extern int wid_SetColormap(
	TkDisplay *,
	WidgetID ,
	Colormap
);


extern int _WID_RefreshClass(
	TkDisplay *,
	unsigned int
);




/****************************************************************************
 *									    *
 *				     BUTTON				    *
 *									    *
 ****************************************************************************/


extern int BN_Create( 
	TkDisplay *, 
	int	,
	Window 	,
	Window	,
	int , int , int , int,
	char *,
	int ,
	unsigned int	
);



extern int bn_CreatePushButton( 
	TkDisplay *	,
	ButtonID 	,
	Window , Window ,
	int , int , 
	unsigned int , unsigned int ,
	char *,
	int , int
);




extern int bn_CreateRepeatButton(
	TkDisplay *,
	ButtonID ,
	Window , Window,
	int , int ,
	unsigned int , unsigned int ,
	char *,
	int , int
);



extern int bn_CreatePopupButton(
	TkDisplay *,
	ButtonID ,
	Window , Window,
	int , int ,
	unsigned int , unsigned int ,
	char *,
	int , int
);



extern int bn_CreateCrossButton(
	TkDisplay *,
	ButtonID ,
	Window , Window,
	int , int ,
	unsigned int , unsigned int ,
	char *,
	int
);



extern int bn_CreateRadioButton(
	TkDisplay *,
	ButtonID ,
	Window , Window,
	int , int ,
	unsigned int , unsigned int ,
	char *,
	int 
);



extern int bn_CreateCheckButton(
	TkDisplay *,
	ButtonID ,
	Window , Window,
	int , int ,
	unsigned int , unsigned int ,
	char *,
	int 
);


extern int bn_CreateThumbButton(
	TkDisplay *,
	ButtonID ,
	Window , Window,
	int , int ,
	unsigned int , unsigned int ,
	int 
);



extern int bn_CreateScrollButton(
	TkDisplay *,
	ButtonID ,
	Window , Window,
	int , int ,
	unsigned int , unsigned int ,
	int , int
);



extern int bn_CreateComboButton(
	TkDisplay *,
	ButtonID ,
	Window , Window,
	int , int ,
	unsigned int , unsigned int ,
	int 
);


extern int bn_CreatePopupRadioButton(
	TkDisplay *,
	ButtonID ,
	Window , Window,
	int , int ,
	unsigned int , unsigned int ,
	char *,
	int 
);


extern int BN_Destroy(
	TkDisplay *,
	ButtonID 
);



extern int BN_Map(
	TkDisplay *,
	ButtonID 
);



extern int BN_Unmap(
	TkDisplay *,
	ButtonID 
);



extern int bn_DrawThumbButton(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawComboButton(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawScrollButton(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawRepeatButton(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawPopupButton(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawPushButton(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawThumbButton(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawCrossButton(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawCrossFocus(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawRadioButton(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawRadioFocus(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawCheckButton(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawCheckFocus(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawPopupRadioButton(
	TkDisplay *,
	ButtonID
);


extern int bn_DrawPopupRadioFocus(
	TkDisplay *,
	ButtonID
);



extern int bn_ComboButtonEvents(
	TkDisplay *,
	ButtonID ,
	TkEvent *
);


extern int bn_PushButtonEvents(
	TkDisplay *,
	ButtonID ,
	TkEvent *
);


extern int bn_ComboCrossEvents(
	TkDisplay *,
	ButtonID ,
	TkEvent *
);


extern int bn_RadioButtonEvents(
	TkDisplay *,
	ButtonID ,
	TkEvent *
);


extern int bn_PopupRadioButtonEvents(
	TkDisplay *,
	ButtonID ,
	TkEvent *
);


extern int bn_CheckButtonEvents(
	TkDisplay *,
	ButtonID ,
	TkEvent *
);


extern int bn_PopupButtonEvents(
	TkDisplay *,
	ButtonID ,
	TkEvent *
);


extern int bn_ScrollButtonEvents(
	TkDisplay *,
	ButtonID ,
	TkEvent *
);


extern int bn_RepeatButtonEvents(
	TkDisplay *,
	ButtonID ,
	TkEvent *
);


extern int bn_ThumbButtonEvents(
	TkDisplay *,
	ButtonID ,
	TkEvent *
);


extern int bn_KeyFocus(
	TkDisplay *,
	Window ,
	int ,
	XEvent *
);


extern Bool bn_CheckRepeatPush(
	Display *,
	XEvent *,
	char *
);



extern int BN_GetState(
	TkDisplay *,
 	ButtonID
);


extern char *BN_GetText(
	TkDisplay *,
	ButtonID
);


extern int BN_Block(
	TkDisplay *,
	ButtonID
);


extern int BN_Unblock(
	TkDisplay *,
	ButtonID 
);


extern int BN_Push(
	TkDisplay *,
	ButtonID
);


extern int BN_Unpush(
	TkDisplay *,
	ButtonID 
);


extern int BN_Gray(
	TkDisplay *,
	ButtonID
);


extern int BN_Ungray(
	TkDisplay *,
	ButtonID
);


extern int BN_SetLighting(
	TkDisplay *,
	ButtonID ,
	int , 
	Bool
);


extern int BN_SetText(
	TkDisplay *,
	ButtonID ,
	char *,
	unsigned char,
	int, int, int, 
	Bool 
);



extern int BN_SetPixmap(
	TkDisplay *,
	ButtonID ,
	Pixmap , Pixmap,
	int, int, int, int, int, int,
	Bool 
);



extern int BN_SetPixmapGrayed(
	TkDisplay *,
	ButtonID ,
	Pixmap , Pixmap,
	int, int, int, int, int, int,
	Bool 
);


extern int BN_DeleteText(
	TkDisplay *,
	ButtonID,
	Bool 
);


extern int BN_DeletePixmap(
	TkDisplay *,
	ButtonID ,
	Bool
);


extern int BN_DeletePixmapGrayed(
	TkDisplay *,
	ButtonID,
	Bool
);


extern int BN_SetFont(
	TkDisplay *,
	ButtonID,
	XFontStruct *,
	Bool 
);


extern int BN_Configure(
	TkDisplay *,
	ButtonID ,
	int , int,
	unsigned int , unsigned int
);



extern int BN_Resize(
	TkDisplay *,
	ButtonID ,
	unsigned int , unsigned int
);	


extern int BN_Move(
	TkDisplay *,
	ButtonID,
	int , int
);


extern int BN_GiveFocus(
	TkDisplay *,
	ButtonID 
);


extern int BN_SetNeverfocusFlag(
	TkDisplay *,
	ButtonID,
	Bool
);


extern int BN_SetCrossType(
	TkDisplay *,
	ButtonID,
	int ,
	Bool 
);




/****************************************************************************
 *									    *
 *				     ITEMS				    *
 *									    *
 ****************************************************************************/


extern int item_GetState(
	TkDisplay *,
	WidgetID ,
	int ,
	unsigned int *,
	int *
);


extern int item_GetText(
	TkDisplay *,
	WidgetID ,
	int,
	unsigned char **
);


extern int item_Add(
	TkDisplay *,
	WidgetID, WidgetID,
	int , int,
	char *,
	unsigned int ,
	XFontStruct *,
	unsigned int,
	Bool 
);


extern int item_Move(
	TkDisplay *,
	WidgetID ,
	int , int,
	Bool 
);


extern int item_Delete(
	TkDisplay *,
	WidgetID ,
	int ,
	Bool 
);


extern int item_DeleteAll(
	TkDisplay *,
	WidgetID
);


extern int item_SetState(
	TkDisplay *,
	WidgetID,
	int,
	unsigned int,
	int
);


extern int item_SetTextDecoration(
	TkDisplay *,
	WidgetID,
	int ,
	ItemTextDecoration *,
	Bool 
);


extern int item_SetPixmapDecoration(
	TkDisplay *,
	WidgetID,
	int,
	ItemPixmapDecoration *,
	Bool 
);


extern int item_SetPrecedency(
	TkDisplay *,
	WidgetID,
	int,
	unsigned int,
	Bool
);



/****************************************************************************
 *									    *
 *				    WINDOWS				    *
 *									    *
 ****************************************************************************/




extern Window win_Create(
	TkDisplay *,
	Window , Window,
	unsigned long , unsigned long, 
	int , int,
	unsigned int, unsigned int, unsigned int, unsigned int, 
	Visual *,
	unsigned long,
	XSetWindowAttributes *,
	unsigned long 
);


extern Window win_Destroy(
	TkDisplay *,
	Window
);


extern int win_Map(
	TkDisplay *,
	Window 
);


extern int win_MapRaised(
	TkDisplay *,
	Window 
);


extern int win_Unmap(
	TkDisplay *,
	Window 
);


extern Window win_GetIconWindow(
	TkDisplay *,
	Window
);


extern long win_GetType(
	TkDisplay *,
	Window
);


extern long win_GetState(
	TkDisplay *,
	Window
);


extern long win_GetAttributes(
	TkDisplay *,
	Window
);


extern int win_GetGeometry(
	TkDisplay *,
	Window, Window *,
	int *, int *,
	unsigned int *, unsigned int *
);


extern long win_GetMDWid(
	TkDisplay *,
	Window
);


extern int win_SetTitleName(
	TkDisplay *,
	Window ,
	char *
);


extern int win_SetGroupLeader(
	TkDisplay *,
	Window , Window
);


extern int win_SetTransientFor(
	TkDisplay *,
	Window , Window
);


extern int win_SetToplevel(
	TkDisplay *,
	Window , Window
);


extern int win_Reparent(
	TkDisplay *,
	Window , Window, Window,
	int , int
);


extern int win_SetIconWindow(
	TkDisplay *, 
	Window, Window
);


extern int win_SetIconPixmap(
	TkDisplay *,
	Window,
	Pixmap 
);


extern int win_SetIconPixmapMask(
	TkDisplay *,
	Window,
	Pixmap
);


extern int win_SetIconName(
	TkDisplay *,
	Window,
	char *
);


extern int win_Resize(
	TkDisplay *,
	Window,
	unsigned int, unsigned int
);


extern int win_SetState(
	TkDisplay *,
	Window,
	unsigned int
);


extern int win_Zoom(
	TkDisplay *,
	Window
);


extern int win_Unzoom(
	TkDisplay *,
	Window
);


extern int win_Iconify(
	TkDisplay *,
	Window 
);


extern int win_Uniconify(
	TkDisplay *,
	Window
);


extern int win_SetWMProtocols(
	TkDisplay *,
	Window,
	Atom *,
	unsigned int
);


extern int win_SetHelpDatabook(
	TkDisplay *,
	Window,
	unsigned char *
);


extern int win_SetHelpTopic(
	TkDisplay *,
	Window,
	int
);


extern int win_RequestHelp(
	TkDisplay *,
	Window
);



extern int win_SetZoomHints(
	TkDisplay *,
	Window,
	XSizeHints *
);


extern int win_SetNormalHints(
	TkDisplay *,
	Window,
	XSizeHints *
);






/****************************************************************************
 *									    *
 *				SCROLLBAR				    *
 *									    *
 ****************************************************************************/



extern SB_Create( 
	TkDisplay *,
	ScrollbarID,
	int,
	Window , Window,
	int, int,
	unsigned int, unsigned int,
	int, int, int,
	unsigned int
);


extern int SB_Destroy(
	TkDisplay *,
	ScrollbarID 
);


extern int SB_Map(
	TkDisplay *,
	ScrollbarID
);


extern int SB_Unmap(
	TkDisplay *,
	ScrollbarID
);


extern int SB_MapThumb(
	TkDisplay *,
	ScrollbarID
);


extern int SB_UnmapThumb(
	TkDisplay *,
	ScrollbarID 
);


extern int sb_ScrollbarEvents(
	TkDisplay *,
	ScrollbarID,
	TkEvent *
);


extern int SB_GetPosition(
	TkDisplay *,
	ScrollbarID
);


extern int SB_GetState(
	TkDisplay *,
	ScrollbarID 
);


extern int SB_GetRange(
	TkDisplay *,
	ScrollbarID 
);


extern int SB_GetPagerange(
	TkDisplay *,
	ScrollbarID 
);


extern int SB_GetThumbsize(
	TkDisplay *,
	ScrollbarID
);


extern int SB_SetPosition(
	TkDisplay *,
	ScrollbarID,
	int
);


extern int SB_SetRange(
	TkDisplay *,
	ScrollbarID,
	int
);


extern int SB_SetPagerange(
	TkDisplay *,
	ScrollbarID,
	int
);


extern int SB_SetThumbsize(
	TkDisplay *,
	ScrollbarID,
	int
);


extern int SB_Gray(
	TkDisplay *,
	ScrollbarID
);


extern int SB_Ungray(
	TkDisplay *,
	ScrollbarID
);


extern int SB_Configure(
	TkDisplay *,
	ScrollbarID,
	int, int,
	unsigned int, unsigned int
);




/****************************************************************************
 *									    *
 *					LIST				    *
 *									    *
 ****************************************************************************/



extern int LS_Create(
	TkDisplay *,
	ListID,
	Window , Window,
	int , int, 
	unsigned int, unsigned int,
	int , int, int, int, int, 
	XFontStruct *,
	Bool
);


extern int LS_Destroy(
	TkDisplay *,
	ListID
);


extern int LS_Map(
	TkDisplay *,
	ListID 
);


extern int LS_Unmap(
	TkDisplay *,
	ListID
);


extern int ls_DrawList(
	TkDisplay *,
	ListID
);


extern int ls_DrawItem(
	TkDisplay *,
	ListID,
	unsigned int
);


extern int ls_ListEvents(
	TkDisplay *,
	ListID ,
	TkEvent *
);


extern int LS_IsAnySelected(
	TkDisplay *,
	ListID 
);


extern Bool LS_IsItemSelected(
	TkDisplay *,
	ListID,
	int
);


extern int LS_GetItemSelected(
	TkDisplay *,
	ListID
);


extern int LS_GetState(
	TkDisplay *,
	ListID
);


extern int LS_GetNumitems(
	TkDisplay *,
	ListID
);


extern int LS_GetItemState(
	TkDisplay *,
	ListID,
	int
);


extern int LS_GetItem(
	TkDisplay *,
	ListID,
	int,
	ListItem *
);


extern int LS_GiveFocus(
	TkDisplay *,
	ListID
);


extern int LS_SetList(
	TkDisplay *,
	ListID,
	ListItem *,
	int, int
);


extern int LS_AddItem(
	TkDisplay *,
	ListID,
	int,
	int,
	Bool
);


extern int LS_DeleteItem(
	TkDisplay *,
	ListID,
	int,
	Bool
);


extern LS_Clear(
	TkDisplay *,
	ListID
);


extern int LS_MoveItem(
	TkDisplay *,
	ListID,
	int, int,
	Bool
);


extern LS_AddFillItem(
	TkDisplay *,
	ListID,
	int, int,
	char *,
	XFontStruct *,
	Pixmap, Pixmap,
	unsigned int, unsigned int, unsigned int,
	int, int,
	Bool
);


extern LS_SetItemText(
	TkDisplay *,
	ListID,
	int,
	char *,
	XFontStruct *,
	Bool
);


extern LS_SetItemPixmap(
	TkDisplay *,
	ListID,
	int,
	Pixmap, Pixmap,
	unsigned int, unsigned int, unsigned int,
	Bool
);


extern LS_SetItemPixmapGrayed(
	TkDisplay *,
	ListID,
	int,
	Pixmap, Pixmap,
	Bool
);


extern int LS_SetPrecedency(
	TkDisplay *,
	ListID,
	int, int,
	Bool
);


extern int LS_SetFont(
	TkDisplay *,
	ListID,
	XFontStruct *,
	Bool
);


extern int LS_Gray(
	TkDisplay *,
	ListID
);


extern int LS_Ungray(
	TkDisplay *,
	ListID
);


extern int LS_GrayItem(
	TkDisplay *,
	ListID,
	int
);


extern int LS_UngrayItem(
	TkDisplay *,
	ListID,
	int
);


extern int LS_DeleteItemPixmap(
	TkDisplay *,
	ListID,
	int,
	Bool
);


extern int LS_DeleteItemPixmapGrayed(
	TkDisplay *,
	ListID,
	int,
	Bool 
);


extern int LS_DeleteItemtext(
	TkDisplay *,
	ListID,
	int,
	Bool
);


extern int LS_AllowMultipleSelection(
	TkDisplay *,
	ListID
);


extern int LS_ForbidMultipleSelection(
	TkDisplay *,
	ListID 
);


extern int LS_SetActiveItem(
	TkDisplay *,
	ListID,
	int
);


extern int LS_SetPosition(
	TkDisplay *,
	ListID,
	int
);


extern int LS_SelectItem(
	TkDisplay *,
	ListID,
	int
);


extern int LS_UnselectItem(
	TkDisplay *,
	ListID,
	int
);


extern int LS_AutoSetParams(
	TkDisplay *,
	ListID,
	Bool
);


extern int LS_Configure(
	TkDisplay *,
	ListID,
	int, int,
	unsigned int, unsigned int
);


extern int _LS_DrawFocus(
	TkDisplay *,
	ListID ,
	int
);






/****************************************************************************
 *									    *
 *					EDIT				    *
 *									    *
 ****************************************************************************/


extern int _ED_DrawFocus(
	TkDisplay *,
	EditID,
	int
);



extern int ED_Create( 
	TkDisplay *,
	EditID,
	Window, Window,
	int, int,
	unsigned int, unsigned int,
	char *,
	Bool,
	unsigned int, unsigned int
);


extern int ED_Destroy(
	TkDisplay *,
	EditID
);


extern ED_Map(
	TkDisplay *,
	EditID
);


extern ED_Unmap(
	TkDisplay *,
	EditID
);


extern ed_DrawEdit(
	TkDisplay *,
	EditID
);


extern ed_DrawLetter(
	TkDisplay *,
	EditID,
	int
);


extern ed_DrawCursor(
	TkDisplay *,
	EditID
);


extern ed_DrawText(
	TkDisplay *,
	EditID,
	int, int
);


extern ed_MoveText(
	TkDisplay *,
	EditID,
	int, int
);


extern int ed_EditEvents(
	TkDisplay *,
	EditID,
	XEvent *
);


extern int ED_GetPosition(
	TkDisplay *,
	EditID
);


extern int ED_GetState(
	TkDisplay *,
	EditID
);


extern char *ED_GetText(
	TkDisplay *,
	EditID
);


extern int ED_GetSelection(
	TkDisplay *,
	EditID,
	unsigned int *, unsigned int *
);


extern int ED_SetCursorPosition(
	TkDisplay *,
	EditID,
	int,
	Bool
);


extern int ED_GiveFocus(
	TkDisplay *,
	EditID
);


extern int ED_SetSelection(
	TkDisplay *,
	EditID,
	int, int,
	Bool
);


extern int ED_SetText(
	TkDisplay *,
	EditID,
	char *,
	Bool
);


extern int ED_Gray(
	TkDisplay *,
	EditID
);


extern int ED_Ungray(
	TkDisplay *,
	EditID
);


extern int ED_Configure(
	TkDisplay *,
	EditID,
	int, int,
	unsigned int, unsigned int
);


extern int ED_Move(
	TkDisplay *,
	EditID,
	int, int
);


extern int ED_Resize(
	TkDisplay *,
	EditID,
	unsigned int, unsigned int
);


extern int ED_SetFont(
	TkDisplay *,
	EditID,
	XFontStruct *,
	Bool
);




/****************************************************************************
 *									    *
 *				      COMBO				    *
 *									    *
 ****************************************************************************/



extern int CB_Create(
	TkDisplay *,
	ComboID,
	Window, Window,
	int, int,
	unsigned int, unsigned int,
	int, int, int,
	XFontStruct *,
	int
);


extern int CB_Destroy(
	TkDisplay *,
	ComboID
);


extern int CB_Map(
	TkDisplay *,
	ComboID
);


extern int CB_Unmap(
	TkDisplay *,
	ComboID
);


int cb_ComboEvents(
	TkDisplay *,
	ComboID,
	TkEvent *
);


extern int CB_GetItemSelected(
	TkDisplay *,
	ComboID
);


extern Bool CB_IsAnySelected(
	TkDisplay *,
	ComboID
);


extern Bool CB_IsItemSelected(
	TkDisplay *,
	ComboID,
	int
);


extern int CB_GetNumitems(
	TkDisplay *,
	ComboID
);


extern int CB_GetItemState(
	TkDisplay *,
	ComboID,
	int
);


extern int CB_GetItem(
	TkDisplay *,
	ComboID,
	int,
	ListItem *
);


extern int CB_GetState(
	TkDisplay *,
	ComboID
);


extern int CB_GiveFocus(
	TkDisplay *,
	ComboID
);


extern int CB_AddItem(
	TkDisplay *,
	ComboID,
	int, int,
	Bool
);


extern int CB_DeleteItem(
	TkDisplay *,
	ComboID,
	int,
	Bool
);


extern int CB_ClearList(
	TkDisplay *,
	ComboID
);


extern int CB_MoveItem(
	TkDisplay *,
	ComboID,
	int, int,
	Bool
);


extern int CB_AddFillItem(
	TkDisplay *,
	ComboID,
	int, int,
	char *,
	XFontStruct *,
	Pixmap, Pixmap,
	unsigned int, unsigned int, unsigned int,
	int, int,
	Bool
);


extern int CB_SetItemText(
	TkDisplay *,
	ComboID, 
	int,
	char *,
	XFontStruct *,
	Bool
);


extern int CB_SetItemPixmap(
	TkDisplay *,
	ComboID,
	int,
	Pixmap, Pixmap,
	unsigned int, unsigned int, unsigned int,
	Bool
);


extern int CB_SetItemPixmapGrayed(
	TkDisplay *,
	ComboID,
	int,
	Pixmap, Pixmap,
	Bool
);


extern int CB_SetPrecedency(
	TkDisplay *,
	ComboID,
	int, int,
	Bool
);


extern int CB_Gray(
	TkDisplay *,
	ComboID
);


extern int CB_Ungray(
	TkDisplay *,
	ComboID
);


extern int CB_GrayItem(
	TkDisplay *,
	ComboID,
	int
);


extern int CB_UngrayItem(
	TkDisplay *,
	ComboID,
	int
);


extern int CB_DeleteItemPixmap(
	TkDisplay *,
	ComboID,
	int,
	Bool
);


extern int CB_DeleteItemPixmapGrayed(
	TkDisplay *,
	ComboID,
	int,
	Bool
);


extern int CB_DeleteItemtext(
	TkDisplay *,
	ComboID,
	int,
	Bool
);


extern int CB_AllowMultipleSelection(
	TkDisplay *,
	ComboID
);


extern int CB_ForbidMultipleSelection(
	TkDisplay *,
	ComboID
);


extern int CB_SetActiveItem(
	TkDisplay *,
	ComboID,
	int
);


extern int CB_SelectItem(
	TkDisplay *,
	ComboID,
	int
);


extern int CB_UnselectItem(
	TkDisplay *,
	ComboID,
	int
);


extern int CB_Configure(
	TkDisplay *,
	ComboID,
	int, int,
	unsigned int, unsigned int
);


extern int CB_Resize(
	TkDisplay *,
	ComboID,
	unsigned int, unsigned int
);


extern int CB_Move(
	TkDisplay *,
	ComboID,
	int, int
);



/****************************************************************************
 *									    *
 *				      MENU				    *
 *									    *
 ****************************************************************************/


extern int MN_CreateFloating(
	TkDisplay *,
	MenuID,
	Window,
	short,
	MenuID,
	int, int,
	char *,
	XFontStruct *,
	int
);


extern int MN_CreateBar(
	TkDisplay *,
	MenuID,
	Window,
	int
);


extern int MN_Destroy(
	TkDisplay *,
	MenuID
);


extern int MN_Map(
	TkDisplay *,
	MenuID,
	int, int
);


extern int mn_Map(
	TkDisplay *,
	MenuID,
	int, int
);


extern int MN_Unmap(
	TkDisplay *,
	MenuID
);


extern int mn_DrawFloatingMenu(
	TkDisplay *,
	MenuID
);


extern int mn_DrawFloatingItem(
	TkDisplay *,
	MenuID,
	int
);


extern int mn_DrawMenuBar(
	TkDisplay *,
	MenuID
);


extern int mn_DrawMenuBarItem(
	TkDisplay *,
	MenuID,
	int
);


extern long mn_GetRootMenu(
	TkDisplay *,
	MenuID
);


extern int mn_FloatingMenuEvents(
	TkDisplay *,
	MenuID,
	TkEvent *
);


extern int mn_MenuBarEvents(
	TkDisplay *,
	MenuID,
	TkEvent *

);


extern int mn_CloseSubmenus(
	TkDisplay *,
	MenuID, MenuID
);


extern int mn_CloseMenu(
	TkDisplay *,
	MenuID
);


extern int mn_CloseParentMenus(
	TkDisplay *,
	MenuID
);


extern Bool mn_IsSubmenu(
	TkDisplay *,
	MenuID,
	Window
);


extern int mn_GiveFocusToWidget(
	TkDisplay *,
	MenuID
);


extern int mn_GetSelectedItem(
	TkDisplay *,
	MenuID,
	int, int
);


extern int MN_GetItemSelectedNumber(
	TkDisplay *,
	MenuID
);


extern int MN_GetItemSelected(
	TkDisplay *,
	MenuID
);


extern int MN_GetState(
	TkDisplay *,
	MenuID
);


extern int MN_GetItemState(
	TkDisplay *,
	MenuID,
	int
);


extern int MN_GetNumitems(
	TkDisplay *,
	MenuID
);


extern int mn_GiveFocus(
	TkDisplay *,
	MenuID
);


extern int MN_AddFillItem(
	TkDisplay *,
	MenuID, MenuID,
	int, int, 
	char *,
	int, 
	XFontStruct *,
	int,
	Bool
);


extern int MN_DeleteItem(
	TkDisplay *,
	MenuID,
	int,
	Bool
);


extern MN_Clear(
	TkDisplay *,
	MenuID
);


extern int MN_MoveItem(
	TkDisplay *,
	MenuID,
	int, int,
	Bool
);


extern int MN_SetItemText(
	TkDisplay *,
	MenuID,
	int,
	char *,
	int,
	XFontStruct *,
	Bool
);


extern int MN_SetItemPixmap(
	TkDisplay *,
	MenuID,
	int,
	Pixmap, Pixmap,
	unsigned int, unsigned int, unsigned int,
	Bool
);


extern int MN_SetItemPixmapGrayed(
	TkDisplay *,
	MenuID,
	int,
	Pixmap, Pixmap,
	Bool
);


extern int MN_SetPrecedency(
	TkDisplay *,
	MenuID,
	int, int,
	Bool
);


extern int MN_SetFont(
	TkDisplay *,
	MenuID,
	XFontStruct *,
	Bool
);


extern int MN_AutoSetDimensions(
	TkDisplay *,
	MenuID,
	Bool
);


extern int MN_GrayItem(
	TkDisplay *,
	MenuID,
	int
);


extern int MN_UngrayItem(
	TkDisplay *,
	MenuID,
	int
);


extern int MN_CheckItem(
	TkDisplay *,
	MenuID,
	int
);


extern int MN_UncheckItem(
	TkDisplay *,
	MenuID,
	int
);


extern int MN_Gray(
	TkDisplay *,
	MenuID
);


extern int MN_Ungray(
	TkDisplay *,
	MenuID
);


int MN_Move(
	TkDisplay *,
	MenuID,
	int, int
);



#endif

#endif


