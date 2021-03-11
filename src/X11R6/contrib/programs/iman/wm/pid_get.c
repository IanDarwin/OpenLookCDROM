/*
 *
 * 	pid_get.c  
 * 	informations sur la fenetre de processus
 *
 * 	Modification :  18/11/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
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









int PID_GetMainWindows()
{
 int i;
 ItemPixmapDecoration item_pixmap;
 WidgetAttributes wid_attributes;

 item_DeleteAll(tk_display,ls_process_win);

 for(i=0;i<maxwindows;i++)
 if(windows[i].isUsed==True && (windows[i].state.isMapped==True||windows[i].state.isIconic==True) && i!=wm_process_index && i!=wm_end_index && i!=wm_kill_index && WIN_VerifyTree(i)==0)
 {
   item_pixmap.mask=SPPixmap|SPWidth|SPHeight|SPDepth;
   item_pixmap.width=13;
   item_pixmap.height=13;
   item_pixmap.depth=1;
   if(windows[i].state.isIconic==True) 
     item_pixmap.pixmap=tk_display->pixmaps.iconify;
   else if(windows[i].state.isZoomed==True) 
     item_pixmap.pixmap=tk_display->pixmaps.zoom;
   else item_pixmap.pixmap=pix_motifs[0];
    
   item_Add(tk_display,ls_process_win,0,END,TextFlag,windows[i].title_name,0,0,0,True);
   item_SetPixmapDecoration(tk_display,ls_process_win,END,&item_pixmap,True);
   item_SetPrecedency(tk_display,ls_process_win,END,PixmapFlag,True);
 }


 if(wid_GetNumItems(tk_display,ls_process_win)>0)
 {
   wid_attributes.mask=SAPosition;
   wid_attributes.position=START;
   wid_SetAttributes(tk_display,ls_process_win,&wid_attributes,False);
 }
 wid_Refresh(tk_display,ls_process_win);
 return 0;
}







void PID_GetProcesses()
{

  fprintf(stderr,"PID_GetProcesses injoignable\n");

}



