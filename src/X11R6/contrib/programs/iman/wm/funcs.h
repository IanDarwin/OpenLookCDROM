/*
 *
 * 	funcs.h
 * 	prototypes de fonctions
 *
 * 	Modification :  21/01/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute and sell this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
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
 *      IMAN Window Manager version 1.2
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */



#ifndef IMAN_WM_FUNCS_H
#define IMAN_WM_FUNCS_H



#if NeedFunctionPrototypes



/****************************************************************************
 *
 *					ABOUT
 *
 ****************************************************************************/


extern int AB_Draw();

extern void AB_Events();

extern void AB_Init();


/****************************************************************************
 *
 *				       CLIPBOARD
 *
 ****************************************************************************/


extern int CLIP_Draw();

extern int CLIP_Events();

extern int CLIP_GetSelectionPixmap(
	Window,
	Atom
);


extern int CLIP_Init();

extern int CLIP_PrintScreen();

extern int CLIP_PrintWindow(
	int
);

extern int CLIP_PrintClientWindow(
	int
);



/****************************************************************************
 *
 *				        COLORS
 *
 ****************************************************************************/


extern int CL_DrawDesktop(int);

extern int CL_DrawWindow(int);

extern int CL_DrawDialog(int);

extern int CL_DrawIcon(int);

extern int CL_DrawButton(int);

extern int CL_DrawEdit(int);

extern int CL_DrawScroll(int);

extern int CL_DrawList(int);

extern int CL_DrawMenu(int);

extern int CL_Events();

extern void CL_Close();

extern int CL_GetPreferences();

extern void CL_Init();

extern void CL_SetupDesk();

extern void CL_SetupWin();

extern void CL_SetupDialog();

extern void CL_SetupIcon();

extern int CL_GetIndex(char *);

extern void CL_SetupButton();

extern void CL_SetupScroll();

extern void CL_SetupEdit();

extern void CL_SetupList();

extern void CL_SetupMenu();

extern int CL_SavePreferences();

extern Bool CL_IsUsed(unsigned long);


/****************************************************************************
 *
 *				        COLORS
 *
 ****************************************************************************/




extern void DSK_Draw();

extern int DSK_Events();

extern void DSK_MotifEvents();

extern void DSK_DefaulticonEvents();

extern void DSK_ScreensaverEvents();

extern void DSK_GetPreferences();

extern void DSK_Init();

extern int DSK_SetMotif(unsigned int);

extern int DSK_SetDefaulticon(unsigned int);

extern void DSK_SavePreferences();



/****************************************************************************
 *
 *				         END
 *
 ****************************************************************************/


extern void END_Draw();

extern void END_Map(Window);

extern void END_Events();

extern void END_Init();



/****************************************************************************
 *
 *				       EVENTS
 *
 ****************************************************************************/


extern void WM_WidgetsEvents();

extern void WM_XlibEvents();



/****************************************************************************
 *
 *				        KILL
 *
 ****************************************************************************/


extern void KILL_Draw();

extern void KILL_Map(Window);

extern void KILL_Events();

extern void KILL_Init();



/****************************************************************************
 *
 *				        PID
 *
 ****************************************************************************/


extern int PID_Map();

extern int PID_Unmap();

extern void PID_Events();

extern int PID_GetMainWindows();

extern void PID_GetProcesses();

extern void PID_Init();


/****************************************************************************
 *
 *				        PID
 *
 ****************************************************************************/


extern int SET_Draw();

extern void SET_Events();

extern void SET_GetPreferences();

extern void SET_Init();

extern int SET_SavePreferences();


/****************************************************************************
 *
 *				      WINDOWS
 *
 ****************************************************************************/




extern int WM_AddWindows(unsigned int);

extern int WM_InitWindow(int, int, int);

extern int WM_ReInitWindow(unsigned int);

extern int WM_InitNormal(int);

extern int WM_InitIcon(int);

extern int WM_InitZoom(int);

extern int WM_InitState(int);

extern int WM_InitIdentity(int);

extern int WM_InitGroup(int);

extern int WM_InitTransient(int);

extern int WM_InitFreezing(int);

extern void WM_CheckFreeWindows();

extern void WM_VerifyWindows();

extern int WIN_DrawTitle(int);

extern int WIN_DrawBorder(
	Window,
	unsigned int
);

extern int WIN_DrawActive(int);

extern int WIN_DrawUnactive(int);

extern int WIN_DrawIconTitle(int);

extern int WIN_DrawIconBorder(int);

extern int WIN_DrawIconPixmap(int);

extern int WM_SendError(
	unsigned int,
	Window
);

extern int WM_Usage(
	int,
	char **
);

extern long WM_GetWindowClass(Window);

extern long WM_GetWindowType(Window);

extern long WM_GetWindowClassWithoutProperty(Window);

extern long WM_GetWindowTypeWithoutProperty(Window);

extern long WM_GetWindowAttributes(Window);

extern Bool IsWindow(Window);

extern Bool IsTitleBar(Window);

extern Bool IsBorderBox(Window);

extern Bool IsIconWindow(Window);

extern Bool IsAnyOnTop();

extern int WM_GetNumber(Window);

extern int WM_GetWindowNumber(Window);

extern int WM_GetOnTop();

extern int WIN_GetOnTop();

extern int WM_GetUnusedWindow(int);

extern int numFreeWindows();

extern int numFreeDialogs();

extern int WIN_GetAddDimensions(int);

extern int WIN_GetProtocols(int);

extern int WIN_AddToGroup(int);

extern int WIN_UnlinkGroup(unsigned int, Bool);

extern int WIN_RemoveFromGroup(unsigned int, Bool);

extern int WIN_Iconify(unsigned int);

extern int WIN_Uniconify(unsigned int);

extern int WM_InitResources();

extern void WM_EndOfSession();

extern int ErrorHandler(
	Display *,
	XErrorEvent *
);

extern void main(
	int,
	char **
);

extern int WIN_MapRaised(int);

extern int WIN_Map(int);

extern int WIN_Unmap(int);

extern int WIN_UnmapAll(int);

extern int WM_FindTopestWindow();

extern int WM_FindToplevelFocus();

extern int WIN_GetSubmembers(
	unsigned int,
	int **,
	unsigned int *, unsigned int *,
	int
);

extern int OrderSubmembers(
	int **,
	unsigned int *, unsigned int *
);


extern int WIN_VerifyTree(int);

extern int ICN_MapRaised(int);

extern void WM_UnmapSystemWindows();

extern int WIN_ReparentClient(
	unsigned int,
	Window
);

extern int WIN_ResizeClient(
	unsigned int,
	unsigned int, unsigned int
);

extern int WIN_Resize(
	unsigned int,
	unsigned int, unsigned int
);

extern int WIN_GiveFocus(unsigned int);

extern int WIN_SetClientType(
	unsigned int,
	long
);

extern int WIN_SetClientAttributes(
	unsigned int,
	long
);

extern int WIN_SetClientState(
	unsigned int,
	long
);

extern int WIN_SetColormap(unsigned int);


extern void WIN_SetTitleName(int);

extern void WIN_SetIconName(int);

extern void WIN_SetIconPixmap(
	int,
	Pixmap,
	Bool
);

extern void WIN_SetIconPixmapMask(
	int,
	Pixmap,
	Bool
);

extern void WIN_SetIconWindow(
	int,
	Window
);

extern void WIN_SetWMHints(int);

extern void WIN_SetGroupLeader(
	int,
	Window
);

extern int WIN_SetTransientLeader(int);

extern void WIN_SetState(int, int);

extern void WM_FreezeAll();

extern void WM_UnfreezeAll();

extern int WIN_Zoom(unsigned int);

extern int WIN_Unzoom(unsigned int);

extern unsigned char mAdaptColor(unsigned char);


#endif


#endif


