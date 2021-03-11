/*
 *
 * 	dsk_set.c
 * 	modification du bureau
 *
 * 	Modification :  16/12/93
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



#define NEED_XRM_RESOURCES


#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"




int DSK_SetMotif(pixnumber)
unsigned int pixnumber;
{
 int i, j;
 int ret;
 int x, y, w, h, border, depth;
 Window root;

/* fprintf(stderr,"Set desktop=%d\n",pixnumber);*/

 i=pixnumber;
 XSetForeground(tk_display->display,desktop_gc,wm_info.desk_fg); 
 XSetBackground(tk_display->display,desktop_gc,wm_info.desk_bg); 

 XSetStipple(tk_display->display,desktop_gc,pix_motifs[i]);
 XSetFillStyle(tk_display->display,desktop_gc,FillOpaqueStippled);
 /*ret=XQueryBestStipple(tk_display->display,RootWindow(tk_display->display,tk_display->screen),16,16,&w,&h);
 if(ret!=0) XFillRectangle(tk_display->display,wm_info.desk_pixmap,desktop_gc,0,0,w,h);
 else XFillRectangle(tk_display->display,wm_info.desk_pixmap,desktop_gc,0,0,16,16);*/
 
 XFreePixmap(tk_display->display,wm_info.desk_pixmap);
 ret=XGetGeometry(tk_display->display,pix_motifs[i],&root,&x,&y,&w,&h,&border,&depth);
 if(ret==1)
   wm_info.desk_pixmap=XCreatePixmap(tk_display->display,root,w,h,tk_display->depth);
 else wm_info.desk_pixmap=XCreatePixmap(tk_display->display,wm_desktop_window,16,16,tk_display->depth);
 XFillRectangle(tk_display->display,wm_info.desk_pixmap,desktop_gc,0,0,w,h);

 XSetStipple(tk_display->display,desktop_gc,None);
 XSetFillStyle(tk_display->display,desktop_gc,FillSolid);

 XSetWindowBackgroundPixmap(tk_display->display,RootWindow(tk_display->display,tk_display->screen),wm_info.desk_pixmap);
 XClearWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen));

}




int DSK_SetDefaulticon(pixnumber)
unsigned int pixnumber;
{
 int i, j;
 int ret;
 int x, y, w, h, border, depth, root;

 if(pixnumber==0)
 {
   for(i=0;i<maxwindows;i++)
   if(windows[i].isUsed==True&&windows[i].state.isIconic==True&&windows[i].icon.hasIconWindow==False&&windows[i].icon.pixmap==0)
     XClearWindow(tk_display->display,windows[i].icon.draw_area);
   return 0;
 }
 else if(pixnumber==1)
 {
   for(i=0;i<maxwindows;i++)
   if(windows[i].isUsed==True&&windows[i].state.isIconic==True&&windows[i].icon.hasIconWindow==False&&windows[i].icon.pixmap==0)
   {
     XClearWindow(tk_display->display,windows[i].icon.draw_area);
     XSetBackground(tk_display->display,icon_gc,tk_display->icn_colors.icn_draw_bg);
     XSetForeground(tk_display->display,icon_gc,tk_display->icn_colors.icn_draw);
     XCopyPlane(tk_display->display,pix_motifs[11],windows[i].icon.draw_area,icon_gc,0,0,64,64,0,0,1);
   }
   return 0;
 }
 return -1;
}





void DSK_SavePreferences()
{
 char str[20];


 sprintf(str,"%d",wm_info.dsk_motif);
 XrmPutStringResource(&xrdb,desktop_text[0],str);

 sprintf(str,"%d",wm_info.dsk_defaulticon);
 XrmPutStringResource(&xrdb,desktop_text[1],str);

 sprintf(str,"%d",wm_info.dsk_screensaver);
 XrmPutStringResource(&xrdb,desktop_text[2],str);

 sprintf(str,"%d",wm_info.dsk_screensaver_time);
 XrmPutStringResource(&xrdb,desktop_text[3],str);

 sprintf(str,"%d",wm_info.dsk_paper);
 XrmPutStringResource(&xrdb,desktop_text[4],str);

 sprintf(str,"%d",wm_info.dsk_paper_drawing);
 XrmPutStringResource(&xrdb,desktop_text[5],str);

#ifdef DESQVIEW_X_SERVER
 XrmPutFileDatabase(xrdb,"/dvx/iman.rc");
#else
 XrmPutFileDatabase(xrdb,xrm_file);
#endif

}



