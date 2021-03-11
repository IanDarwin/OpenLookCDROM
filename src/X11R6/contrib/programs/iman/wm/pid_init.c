/*
 *
 * 	pid_init.c
 * 	initialisation de la fenetre de processus
 *
 * 	Modification :  02/05/94
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

#include "bm/focus.bm"
#include "bm/close.bm"
#include "bm/kill.bm"
#include "bm/zoomer.bm"
#include "bm/iconify.bm"

#include "iman.h"









/*
 *
 * Boucle principale
 *
 *
 */

void PID_Init()
{
 WidgetAttributes wid_attributes;
 WidgetTextDecoration wid_text;
 WidgetPixmapDecoration wid_pixmap;

 XSetWindowAttributes attrib;
 XGCValues xgcvalues;
 XWMHints wmhints;

 unsigned long ptr;
 int mask;
 int ret;
 int i, j, k, w;
 unsigned long win_type;





	     /*** Creation de la fenetre de configuration ***/

 attrib.background_pixel=tk_display->dlg_colors.bg;
 attrib.border_pixel=tk_display->dlg_colors.border_inactive;
 attrib.cursor=tk_display->cursors.normal;
 attrib.event_mask=ExposureMask;
 mask=CWBackPixel|CWBorderPixel|CWCursor|CWEventMask;
																											
 wm_process_window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),(DisplayWidth(tk_display->display,0)-PROCESS_BOX_WIDTH-20)/2,(DisplayHeight(tk_display->display,0)-PROCESS_BOX_HEIGHT-30)/2,PROCESS_BOX_WIDTH,PROCESS_BOX_HEIGHT,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 XStoreName(tk_display->display,wm_process_window,"Process");

 mask=GCForeground|GCBackground;
 xgcvalues.foreground=BlackPixel(tk_display->display,tk_display->screen);
 xgcvalues.background=tk_display->dlg_colors.bg;
 process_gc=XCreateGC(tk_display->display,wm_process_window,mask,&xgcvalues);

 ptr=DIALOG_BOX;
 XChangeProperty(tk_display->display,wm_process_window,tk_display->atoms._IMAN_WM_TYPE,XA_INTEGER,32, PropModeReplace,(char *)&ptr,1);
 ptr=TitleBar+Border+CloseBox;
 XChangeProperty(tk_display->display,wm_process_window,tk_display->atoms._IMAN_WM_TYPE,XA_INTEGER,32, PropModeAppend,(char *)&ptr,1);




		    /*** Creation des widgets ***/

 wid_attributes.mask=SALighting|SANeverFocus;
 wid_attributes.lighting=True;
 wid_attributes.neverFocus=True;
 bn_process_pid=wid_Create(tk_display,WI_BUTTON,BN_POPUPBUTTON,wm_process_window,wm_process_window,5,5,20,20,&wid_attributes,Unpushed);
 bn_process_win=wid_Create(tk_display,WI_BUTTON,BN_POPUPBUTTON,wm_process_window,wm_process_window,30,5,20,20,&wid_attributes,Unpushed);
 bn_process_focus=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_process_window,wm_process_window,186,5,25,25,&wid_attributes,Unpushed);
 bn_process_close=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_process_window,wm_process_window,217,5,25,25,&wid_attributes,Unpushed);
 bn_process_kill=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_process_window,wm_process_window,248,5,25,25,&wid_attributes,Unpushed);
 bn_process_iconify=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_process_window,wm_process_window,279,5,25,25,&wid_attributes,Unpushed);
 bn_process_zoom=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_process_window,wm_process_window,310,5,25,25,&wid_attributes,Unpushed);				

 wid_attributes.multipleSelection=False;
 wid_attributes.itemheight=14;
 wid_attributes.htype=SB_LEFTALIGN;
 wid_attributes.vtype=SB_BOTTOMALIGN;
 wid_attributes.border=True;
 wid_attributes.mask=SAMultipleSelection|SAItemHeight|SAHVType|SABorder;
 ls_process_win=wid_Create(tk_display,WI_LIST,LS_SIMPLE,wm_process_window,wm_process_window,5,38,PROCESS_BOX_WIDTH-13,PROCESS_BOX_HEIGHT-45,&wid_attributes,0);



			/*** Modifications ***/

 pix_focus=XCreateBitmapFromData(tk_display->display,wm_process_window,focus_bits,focus_width,focus_height);
 pix_close=XCreateBitmapFromData(tk_display->display,wm_process_window,close_bits,close_width,close_height);
 pix_kill=XCreateBitmapFromData(tk_display->display,wm_process_window,kill_bits,kill_width,kill_height);
 pix_zoom=XCreateBitmapFromData(tk_display->display,wm_process_window,zoomer_bits,zoomer_width,zoomer_height);
 pix_iconify=XCreateBitmapFromData(tk_display->display,wm_process_window,iconify_bits,iconify_width,iconify_height);


 wid_pixmap.mask=SPPixmap|SPGravity|SPWidth|SPHeight|SPDepth;
 wid_pixmap.pixmap=pix_focus;
 wid_pixmap.width=focus_width;
 wid_pixmap.height=focus_height;
 wid_pixmap.depth=1;
 wid_pixmap.gravity=CenterBitmap;
 wid_SetPixmapDecoration(tk_display,bn_process_focus,&wid_pixmap,True);

 wid_pixmap.pixmap=pix_close;
 wid_pixmap.width=close_width;
 wid_pixmap.height=close_height;
 wid_SetPixmapDecoration(tk_display,bn_process_close,&wid_pixmap,True);

 wid_pixmap.pixmap=pix_kill;
 wid_pixmap.width=kill_width;
 wid_pixmap.height=kill_height;
 wid_SetPixmapDecoration(tk_display,bn_process_kill,&wid_pixmap,True);

 wid_pixmap.pixmap=pix_zoom;
 wid_pixmap.width=zoomer_width;
 wid_pixmap.height=zoomer_height;
 wid_SetPixmapDecoration(tk_display,bn_process_zoom,&wid_pixmap,True);

 wid_pixmap.pixmap=pix_iconify;
 wid_pixmap.width=iconify_width;
 wid_pixmap.height=iconify_height;
 wid_SetPixmapDecoration(tk_display,bn_process_iconify,&wid_pixmap,True);


			/**** Affichage ****/

/* wid_Map(tk_display,bn_process_pid);
 wid_Map(tk_display,bn_process_win);*/

 wid_Map(tk_display,bn_process_close);
 wid_Map(tk_display,bn_process_kill);
 wid_Map(tk_display,bn_process_zoom);
 wid_Map(tk_display,bn_process_iconify);
 wid_Map(tk_display,bn_process_focus);
 wid_Map(tk_display,ls_process_win);


}







