/*
 *
 * 	set_get.c
 * 	informations sur la fentre de parametrage
 *
 * 	Modification :  26/01/94
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








void SET_GetPreferences()
{
 int i, j, k;
 char *str_type, *sptr;
 char str[30];
			/*** Options pour wm_info ***/



 if(xrdb!=NULL) for(i=0;i<6;i++)
 {
  j=0;
  j=XrmGetResource(xrdb,options_text[i],NULL,&str_type,&xrmvalue);
  if(i==0 && j==1)
  {
    sptr=xrmvalue.addr;
    if(strcmp(sptr,"OFF")==0||strcmp(sptr,"Off")==0) wm_info.set_decoration=False;
    else wm_info.set_decoration=True;
  }
  else wm_info.set_decoration=True;

  if(i==1 && j==1)
  {
    sptr=xrmvalue.addr;
    if(strcmp(sptr,"OFF")==0||strcmp(sptr,"Off")==0) wm_info.set_groups=False;
    else wm_info.set_groups=True;
  }
  else wm_info.set_groups=True;

  if(i==2 && j==1)
  {
    sptr=xrmvalue.addr;
    if(strcmp(sptr,"OFF")==0||strcmp(sptr,"Off")==0) 
    {
	wm_info.set_icons=False;
	for(k=0;k<maxwindows;k++)
	{
	  if(windows[k].isUsed==True&&windows[k].state.isIconic==True&&(windows[k].attributes&GroupMember)!=GroupMember)
	    XUnmapWindow(tk_display->display,windows[k].icon.window);
	}
    }
    else wm_info.set_icons=True;

  }
  else wm_info.set_icons=True;

  if(i==3 && j==1)
  {
    sptr=xrmvalue.addr;
    if(strcmp(sptr,"ON")==0||strcmp(sptr,"On")==0)
    { 
	wm_info.set_icontitle=True;
	for(k=0;k<maxwindows;k++)
	if(windows[k].class==TOP_LEVEL_WINDOW)
	{
		XMapRaised(tk_display->display,windows[k].icon.title);
		XResizeWindow(tk_display->display,windows[k].icon.window,ICON_DRAW_AREA_WIDTH+4,ICON_DRAW_AREA_HEIGHT+4+14);
		XMoveWindow(tk_display->display,windows[k].icon.draw_area,2,2+14); 
		windows[k].icon.width=ICON_DRAW_AREA_WIDTH+4;
		windows[k].icon.height=ICON_DRAW_AREA_HEIGHT+4+14;
	}
    }
    else
    { 
	wm_info.set_icontitle=False;
	for(k=0;k<maxwindows;k++)
	if(windows[k].class==TOP_LEVEL_WINDOW)
	{
	  	XUnmapWindow(tk_display->display,windows[k].icon.title);
		XResizeWindow(tk_display->display,windows[k].icon.window,ICON_DRAW_AREA_WIDTH+4,ICON_DRAW_AREA_HEIGHT+4);
		XMoveWindow(tk_display->display,windows[k].icon.draw_area,2,2); 
		if(windows[k].icon.hasIconWindow==True) XMoveWindow(tk_display->display,windows[k].icon.clientwindow,2,2); 
		windows[k].icon.width=ICON_DRAW_AREA_WIDTH+4;
		windows[k].icon.height=ICON_DRAW_AREA_HEIGHT+4;
		if(windows[k].state.isIconic==True) XClearArea(tk_display->display,windows[k].icon.window,0,0,100,100,True);
	}
    }
  }
  else wm_info.set_icontitle=True;

  if(i==4 && j==1)
  {
    sptr=xrmvalue.addr;
    if(strcmp(sptr,"OFF")==0||strcmp(sptr,"Off")==0) wm_info.set_helpactive=False;
    else wm_info.set_helpactive=True;
  }
  else wm_info.set_helpactive=False;

  if(i==5 && j==1)
  {
    sptr=xrmvalue.addr;
    if(strcmp(sptr,"OFF")==0||strcmp(sptr,"Off")==0) wm_info.set_debug=False;
    else wm_info.set_debug=True;
  }
  else wm_info.set_debug=False;

  if(wm_action.type==InitAction)
    SET_SavePreferences();
 }
 else
 {
#ifdef DEBUG
   fprintf(stderr,"xrdb==0\n");
#endif

   wm_info.set_decoration=True;
   wm_info.set_groups=True;
   wm_info.set_icons=True;
   wm_info.set_icontitle=True;
   wm_info.set_helpactive=False;
   wm_info.set_debug=False;
   SET_SavePreferences();
 }
}





