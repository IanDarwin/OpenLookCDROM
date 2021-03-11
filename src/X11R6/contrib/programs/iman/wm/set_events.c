/*
 *
 * 	set_events.c
 * 	evenements de la fentre de parametrage
 *
 * 	Modification :  22/11/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify distribute, and sell this software and its
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
#include <X11/keysym.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"






/*
 *
 * Traitement des evenements
 *
 *
 */

void SET_Events()
{
 unsigned long ptr;
 int mask,state;
 int ret;
 int i, j, k, w;



 switch(tk_event.ev_type)
 {

   case BN_KEYUNKNOWN :
	if(XKeycodeToKeysym(tk_display->display,tk_event.event.xkey.keycode,0)==XK_Escape)
	  goto SETUP_CANCEL;	 

	break;


   case BN_RELEASED :

	 if(tk_event.button==bn_setup_cancel)
	 {
	SETUP_CANCEL:
	   XUnmapWindow(tk_display->display,windows[wm_setup_index].mainwindow);
	   XUnmapWindow(tk_display->display,wm_setup_window);
 
	   if (wm_info.set_decoration==True) wid_SetState(tk_display,bn_setup_decoration,Pushed+Grayed);
	   else wid_SetState(tk_display,bn_setup_decoration,Unpushed+Grayed);

	   if (wm_info.set_groups==True) wid_SetState(tk_display,bn_setup_groups,Pushed); 
	   else wid_SetState(tk_display,bn_setup_groups,Grayed);

	   if (wm_info.set_icons==True) wid_SetState(tk_display,bn_setup_icons,Pushed);
	   else wid_SetState(tk_display,bn_setup_icons,Unpushed);

	   if (wm_info.set_icontitle==True) wid_SetState(tk_display,bn_setup_icontitle,Pushed);
	   else wid_SetState(tk_display,bn_setup_icontitle,Unpushed);

	   if (wm_info.set_helpactive==True) wid_SetState(tk_display,bn_setup_helpactive,Pushed+Grayed);
	   else wid_SetState(tk_display,bn_setup_helpactive,Unpushed+Grayed);

	   if (wm_info.set_debug==True) wid_SetState(tk_display,bn_setup_debug,Pushed+Grayed);
	   else wid_SetState(tk_display,bn_setup_debug,Unpushed+Grayed);
 	}

	else if(tk_event.button==bn_setup_ok)
	{
	   XUnmapWindow(tk_display->display,windows[wm_setup_index].mainwindow);
	   XUnmapWindow(tk_display->display,wm_setup_window);
 
	   state=wid_GetState(tk_display,bn_setup_decoration);
	   if((state&Pushed)==Pushed)
	   {
	     if(wm_info.set_decoration==False)
	       wm_info.set_decoration=True;
	   }
	   else wm_info.set_decoration=False;

	   state=wid_GetState(tk_display,bn_setup_groups);
	   if((state&Pushed)==Pushed)
	   {
	     if(wm_info.set_groups==False)
	       wm_info.set_groups=True;
	   }
	   else wm_info.set_groups=False;

	   state=wid_GetState(tk_display,bn_setup_icons);
	   if((state&Pushed)==Pushed)
	   {
	     if(wm_info.set_icons==False)
	     {
	    	for(i=0;i<maxwindows;i++)
		{
		  if(windows[i].isUsed==True&&windows[i].state.isIconic==True&&(windows[i].attributes&GroupMember)!=GroupMember&&windows[i].class==TOP_LEVEL_WINDOW)
		    XMapWindow(tk_display->display,windows[i].icon.window);
		}
	    	wm_info.set_icons=True;
	     }
	     wm_info.set_icons=True;
	   }
	   else{ 
	     if(wm_info.set_icons==True)
	     {
	    	for(i=0;i<maxwindows;i++)
		{
		  if(windows[i].isUsed==True&&windows[i].state.isIconic==True&&(windows[i].attributes&GroupMember)!=GroupMember&&windows[i].class==TOP_LEVEL_WINDOW)
		    XUnmapWindow(tk_display->display,windows[i].icon.window);
		}
	    	wm_info.set_icons=False;
	     }
	     wm_info.set_icons=False;
	   }

	   state=wid_GetState(tk_display,bn_setup_icontitle);
	   if((state&Pushed)==Pushed)
	   {
	     for(i=0;i<maxwindows;i++)
	     if(windows[i].class==TOP_LEVEL_WINDOW)
	     {
	      	XMapRaised(tk_display->display,windows[i].icon.title);
		XResizeWindow(tk_display->display,windows[i].icon.window,ICON_DRAW_AREA_WIDTH+4,ICON_DRAW_AREA_HEIGHT+4+14);
		XMoveWindow(tk_display->display,windows[i].icon.draw_area,2,2+14); 
		windows[i].icon.width=ICON_DRAW_AREA_WIDTH+4;
		windows[i].icon.height=ICON_DRAW_AREA_HEIGHT+4+14;
	     }
	     wm_info.set_icontitle=True;
	   }
	   else{ 
	     for(i=0;i<maxwindows;i++)
	     if(windows[i].class==TOP_LEVEL_WINDOW)
	     {
	       	XUnmapWindow(tk_display->display,windows[i].icon.title);
		XResizeWindow(tk_display->display,windows[i].icon.window,ICON_DRAW_AREA_WIDTH+4,ICON_DRAW_AREA_HEIGHT+4);
		XMoveWindow(tk_display->display,windows[i].icon.draw_area,2,2); 
		windows[i].icon.width=ICON_DRAW_AREA_WIDTH+4;
		windows[i].icon.height=ICON_DRAW_AREA_HEIGHT+4;
		if(windows[i].state.isIconic==True) XClearArea(tk_display->display,windows[i].icon.window,0,0,100,100,True);
	     }
	     wm_info.set_icontitle=False;
	   }
	   state=wid_GetState(tk_display,bn_setup_helpactive);
	   if((state&Pushed)==Pushed)
	     wm_info.set_helpactive=True;
	   else wm_info.set_helpactive=False;

	   state=wid_GetState(tk_display,bn_setup_debug);
	   if((state&Pushed)==Pushed)
	     wm_info.set_debug=True;
	   else wm_info.set_debug=False;

	   SET_SavePreferences();

	}
	break;


 }

}

