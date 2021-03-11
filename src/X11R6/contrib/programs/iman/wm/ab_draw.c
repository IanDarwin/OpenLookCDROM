/*
 *
 * 	ab_draw.c  
 * 	
 *
 * 	Modification :  24/04/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
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





int AB_Draw()
{
 int w;
 char *str;


 if(tk_event.event.xexpose.count==0)
 {
  XSetForeground(tk_display->display,about_gc,tk_display->dlg_colors.text);
  XSetBackground(tk_display->display,about_gc,tk_display->dlg_colors.bg);
  XSetFont(tk_display->display,about_gc,wm_fonts.times_big->fid);
  w=strlen("Iman");
  XDrawImageString(tk_display->display,wm_about_window,about_gc,(360-XTextWidth(wm_fonts.times_big,"Iman",4))/2,35,"Iman",w);

  XSetFont(tk_display->display,about_gc,tk_display->fonts.helvetica12->fid);
  str="Iman - Window Manager";
  w=strlen(str);
  XDrawImageString(tk_display->display,wm_about_window,about_gc,60,75,str,w);
  str="Version 1.2  for UNIX";
#ifdef UNIX386_X_SERVER
  str="Version 1.2  for UNIX/386";
#endif
#ifdef GENERIC_X_SERVER
  str="Version 1.2  for UNIX";
#endif
#ifdef LINUX386_X_SERVER
  str="Version 1.2  for LINUX/386";
#endif
#ifdef DESQVIEW_X_SERVER
  str="Version 1.2  for DesqVIEW/X";
#endif
  w=strlen(str);
  XDrawImageString(tk_display->display,wm_about_window,about_gc,60,90,str,w);
  str="Copyright (c) 1993,1994 by Bruno RIVAS";
  w=strlen(str);
  XDrawImageString(tk_display->display,wm_about_window,about_gc,60,105,str,w);


  XDrawLine(tk_display->display,wm_about_window,about_gc,50,118,310,118);
  XDrawLine(tk_display->display,wm_about_window,about_gc,50,118,310,118);
 
  str="To contact the author :";
  w=strlen(str);
  XDrawImageString(tk_display->display,wm_about_window,about_gc,60,140,str,w);

  str="Bruno RIVAS";
  w=strlen(str);
  XDrawImageString(tk_display->display,wm_about_window,about_gc,80,162,str,w);
  str="30, avenue Marie";
  w=strlen(str);
  XDrawImageString(tk_display->display,wm_about_window,about_gc,80,177,str,w);
  str="93250 Villemomble, France";
  w=strlen(str);
  XDrawImageString(tk_display->display,wm_about_window,about_gc,80,192,str,w);

  str="Tel :   33 (1) 49.35.97.27";
  w=strlen(str);
  XDrawImageString(tk_display->display,wm_about_window,about_gc,80,216,str,w);
  str="Fax :  33 (1) 48.94.27.91";
  w=strlen(str);
  XDrawImageString(tk_display->display,wm_about_window,about_gc,80,231,str,w);
  str="Email : rivas@email.teaser.com";
  w=strlen(str);
  XDrawImageString(tk_display->display,wm_about_window,about_gc,80,246,str,w); 


  XDrawLine(tk_display->display,wm_about_window,about_gc,50,265,310,265);
  XDrawLine(tk_display->display,wm_about_window,about_gc,50,265,310,265);
 
 /* str="This software is copyrighted.";
  w=strlen(str);
  XDrawImageString(tk_display->display,wm_about_window,about_gc,60,290,str,w);*/
 }


}







