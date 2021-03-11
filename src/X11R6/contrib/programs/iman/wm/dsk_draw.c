/*
 *
 * 	dsk_draw.c  
 * 	affichage du bureau
 *
 * 	Modification :  29/03/94
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


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"





void DSK_Draw()
{
 int i, w;
 char *str;

 if(tk_event.event.xexpose.window==desktop_motif_window)
 {
				  
  XSetForeground(tk_display->display,desktop_gc,tk_display->dlg_colors.text);
  XSetBackground(tk_display->display,desktop_gc,tk_display->dlg_colors.bg);
  XSetFont(tk_display->display,desktop_gc,tk_display->fonts.helvetica12->fid);

  XDrawRectangle(tk_display->display,desktop_motif_window,desktop_gc,0,7,239,72);
			
  str="Motif";
  w=strlen(str);
  XDrawImageString(tk_display->display,desktop_motif_window,desktop_gc,8,10,str,w);
  str="Name:";
  w=strlen(str);
  XDrawImageString(tk_display->display,desktop_motif_window,desktop_gc,12,35,str,w);
 }

 else if(tk_event.event.xexpose.window==desktop_defaulticon_window)
 {
				  
  XSetForeground(tk_display->display,desktop_gc,tk_display->dlg_colors.text);
  XSetBackground(tk_display->display,desktop_gc,tk_display->dlg_colors.bg);
  XSetFont(tk_display->display,desktop_gc,tk_display->fonts.helvetica12->fid);
  XDrawRectangle(tk_display->display,desktop_defaulticon_window,desktop_gc,0,7,239,42);
  str="Default icon";
  w=strlen(str);
  XDrawImageString(tk_display->display,desktop_defaulticon_window,desktop_gc,10,12,str,w);
  str="Name:";
  w=strlen(str);
  XDrawImageString(tk_display->display,desktop_defaulticon_window,desktop_gc,12,35,str,w);				  
 }

 else if(tk_event.event.xexpose.window==desktop_screensaver_window)
 {
				  
  XSetForeground(tk_display->display,desktop_gc,tk_display->dlg_colors.text);
  XSetBackground(tk_display->display,desktop_gc,tk_display->dlg_colors.bg);
  XSetFont(tk_display->display,desktop_gc,tk_display->fonts.helvetica12->fid);

  XDrawRectangle(tk_display->display,desktop_screensaver_window,desktop_gc,0,7,369,82);

  str="Screen saver";
  w=strlen(str);
  XDrawImageString(tk_display->display,desktop_screensaver_window,desktop_gc,10,12,str,w);
  str="Name:";
  w=strlen(str);
  XDrawImageString(tk_display->display,desktop_screensaver_window,desktop_gc,112,35,str,w);
  str="Delay:";
  w=strlen(str);
  XDrawImageString(tk_display->display,desktop_screensaver_window,desktop_gc,112,65,str,w);
  str="minutes";
  w=strlen(str);
  XDrawImageString(tk_display->display,desktop_screensaver_window,desktop_gc,258,65,str,w);

 }
 else if(tk_event.event.xexpose.window==desktop_paper_window)
 {
				  
  XSetForeground(tk_display->display,desktop_gc,tk_display->dlg_colors.text);
  XSetBackground(tk_display->display,desktop_gc,tk_display->dlg_colors.bg);
  XSetFont(tk_display->display,desktop_gc,tk_display->fonts.helvetica12->fid);

  XDrawRectangle(tk_display->display,desktop_paper_window,desktop_gc,0,7,279,72);
			
  str="Desktop image";
  w=strlen(str);
  XDrawImageString(tk_display->display,desktop_paper_window,desktop_gc,10,12,str,w);
  str="Image:";
  w=strlen(str);
  XDrawImageString(tk_display->display,desktop_paper_window,desktop_gc,12,35,str,w);

 }


}



