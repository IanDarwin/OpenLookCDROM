/*
 *
 * 	funcs.h
 * 	Functions prototypes
 *
 * 	Modification :  24/04/94
 *
 *	Copyright (c) 1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
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
 *      IMAN Help Server version 1.0
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */



#ifndef _IMAN_HS_FUNCS_H
#define _IMAN_HS_FUNCS_H


extern int XLIBErrorHandler(
	Display *,
	XErrorEvent *
);

extern int HS_AddSessions(unsigned int);

extern int HS_InitSession(unsigned int);

extern int HS_UnuseSession(unsigned int);

extern int HS_ResizeMain(unsigned int, unsigned int, unsigned int);

extern int HS_ResizeTopics(unsigned int, unsigned int, unsigned int);

extern int HS_GetWindowSessionNumber(Window);

extern int HS_ResizeGlossary(unsigned int, unsigned int, unsigned int);

extern int HS_GetWidgetSessionNumber(WidgetID);

extern int HS_FreeSessions();

extern int HS_UseSession(unsigned int);

extern char *HS_GetDatabook(Window);

extern int HS_GetTopic(Window);


/**** Drawing functions ****/

extern int HS_DrawError(unsigned int,unsigned int);

extern int HS_MapError(unsigned int);

extern int HS_UnmapError(unsigned int);

extern int HS_DrawWarning(unsigned int,unsigned int);

extern int HS_MapWarning(unsigned int);

extern int HS_UnmapWarning(unsigned int);

extern int HS_MapOpen(unsigned int);

extern int HS_UnmapOpen(unsigned int);

extern int HS_Draw(unsigned int);



extern int HS_GetFilesList(unsigned int, char *);

extern void HS_Exit();

extern int EV_Xlib();

extern int EV_Widgets();

extern Bool IsHelpFile(char *);

extern int HS_OpenHelpFile(unsigned int, char *);

extern int HS_GetHelpFileFormat(unsigned int);

extern int HS_OpenHelpName(unsigned int, char *);

extern int HS_FreeSessionData(unsigned int);

extern char *HS_ReadNextLine(unsigned int);

extern char *HS_ReadNextLineWithBlanks(unsigned int);


/*** Resources ***/

extern int HS_FreeResources(unsigned int);

extern int HS_GetUnusedResource(unsigned int);

extern int HS_InitResource(unsigned int, unsigned int);

extern int HS_UseResource(unsigned int, unsigned int);

extern int HS_UnuseResource(unsigned int, unsigned int);

extern int HS_VerifyResource(unsigned int, unsigned int);

extern XFontStruct *HS_LoadFont(unsigned int, unsigned int);

extern int HS_UnloadFont(unsigned int, unsigned int);

extern int HS_LoadColor(unsigned int, unsigned int);

extern int HS_UnloadColor(unsigned int, unsigned int);

extern int HS_LoadBitmap(unsigned int, unsigned int);

extern int HS_UnloadBitmap(unsigned int, unsigned int);

extern int ReadAndCreateBitmap(unsigned int, unsigned int);

extern int HS_LoadPixmap(unsigned int, unsigned int);

extern int HS_UnloadPixmap(unsigned int, unsigned int);

extern int ReadAndCreatePixmap(unsigned int, unsigned int);

extern int HS_GetResourceIndexFromNumber(unsigned int, unsigned int);



/*** Topics ***/

extern int HS_FreeTopics(unsigned int);

extern int HS_GetUnusedTopic(unsigned int);

extern int HS_InitTopic(unsigned int, unsigned int);

extern int HS_UseTopic(unsigned int, unsigned int);

extern int HS_UnuseTopic(unsigned int, unsigned int);

extern int HS_VerifyTopic(unsigned int,unsigned int);

extern int HS_UnloadTopic(unsigned int, unsigned int);

extern int HS_LoadTopic(unsigned int, unsigned int);



/*** Items ***/

extern int HS_FreeItems(unsigned int,unsigned int);

extern int HS_GetUnusedItem(unsigned int,unsigned int);

extern int HS_InitItem(unsigned int, unsigned int,unsigned int);

extern int HS_UseItem(unsigned int, unsigned int,unsigned int);

extern int HS_UnuseItem(unsigned int, unsigned int,unsigned int);

extern int HS_VerifyItem(unsigned int, unsigned int,unsigned int);

extern int HS_VerifyTopicJumps(unsigned int,unsigned int);

extern int HS_LoadItems(unsigned int, unsigned int);

extern int HS_GetAllItemsDimensions(unsigned int);

extern int HS_GetItemsDimensions(unsigned int, unsigned int);

extern char *HS_LowerText(char *);



/*** Lines ***/

extern int HS_GetNumWords(char *);

extern HSWord *HS_GetWords(
	char *,
	unsigned int
);

extern int HS_GetLinesDimensions(unsigned int);

extern int HS_GetTotalHeight(unsigned int);



#endif


