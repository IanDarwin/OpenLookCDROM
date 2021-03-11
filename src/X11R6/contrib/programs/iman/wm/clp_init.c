/*
 *
 * 	clp_init.c
 * 	initialisation du clipboard
 *
 * 	Modification :  03/04/93
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



#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"










/*
 *
 * Boucle principale
 *
 *
 */

int CLIP_Init()
{
  WidgetAttributes wid_attributes;
 WidgetTextDecoration wid_text;
 WidgetPixmapDecoration wid_pixmap;
 ItemPixmapDecoration item_pixmap;
 ItemTextDecoration item_text;

 XSetWindowAttributes attrib;
 XGCValues xgcvalues;
 XWMHints wmhints;

 unsigned long ptr;
 int mask;
 int ret;
 int i, j, k, w, h;
 unsigned long win_type;
 Pixmap pix;
 char *str_type, *sptr;




 		/*** Creation de la fenetre du clipboard */

 /*fprintf(stderr,"CLIP init\n");*/


 attrib.background_pixel=tk_display->win_colors.bg;
 attrib.border_pixel=tk_display->win_colors.border_inactive;
 attrib.cursor=tk_display->cursors.normal;
 attrib.event_mask=ExposureMask;
 attrib.bit_gravity=StaticGravity;
 mask=CWBackPixel|CWBorderPixel|CWCursor|CWEventMask|CWBitGravity;

 wm_clipboard_window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),10,10,200,200,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 XStoreName(tk_display->display,wm_clipboard_window,"Clipboard");
 XSetIconName(tk_display->display,wm_clipboard_window,"Clip");

 ptr=TOP_LEVEL_WINDOW;
 XChangeProperty(tk_display->display,wm_clipboard_window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeReplace,(char *)&ptr,1);
 ptr=TitleBar+Border+CloseBox+ZoomBox+IconifyBox;
 XChangeProperty(tk_display->display,wm_clipboard_window, tk_display->atoms._IMAN_WM_TYPE,XA_INTEGER,32, PropModeAppend,(char *)&ptr,1);


 mask=GCForeground|GCBackground;
 xgcvalues.foreground=BlackPixel(tk_display->display,tk_display->screen);
 xgcvalues.background=tk_display->win_colors.bg;
 clipboard_gc=XCreateGC(tk_display->display,wm_clipboard_window, mask, &xgcvalues);
 clip_info.pixmap=0;


 clip_info.isOwner=False;
 clip_info.owner=XGetSelectionOwner(tk_display->display,XA_CLIPBOARD);
 clip_info.selection=XA_CLIPBOARD;
 clip_info.target=0;
 clip_info.time=0;
 clip_info.write_number=0;

 if(clip_info.owner!=None)
   XConvertSelection(tk_display->display,XA_CLIPBOARD,XA_PIXMAP,tk_display->atoms._IMAN_WM_DATA,wm_clipboard_window,CurrentTime);


 return 0;
}




