/*
 *
 * 	dsk_events.c  
 * 	evenements du bureau
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





int DSK_Events()
{
 XEvent send_event;
 Window restack[2];
 WidgetAttributes wid_attributes;
 WidgetTextDecoration wid_text;
 unsigned char *text;
 int i;

  switch(tk_event.ev_type)
  {
        case BN_PUSHED :

		if(tk_event.button==bn_paper_center)
		{
		  wm_info.dsk_paper_drawing=1;
		  wid_SetState(tk_display,bn_paper_mos,Unpushed); 
		}
		else if(tk_event.button==bn_paper_mos)
		{
		  wm_info.dsk_paper_drawing=0;
		  wid_SetState(tk_display,bn_paper_center,Unpushed); 
		}
		return 0;
		break;


	case BN_RELEASED :

		if(tk_event.button==bn_desktop_colors)
		{
#ifdef DEBUG
		  fprintf(stderr,"Couleurs en route\n");
#endif			  
		  i=wm_colors_index;
		  windows[i].state.isInitialized=True; 
		  windows[i].state.isMapped=True;
		  windows[i].state.isOnTop=True;

		  WID_Freeze(tk_display,wm_desktop_window);
		  /*
		  wid_attributes.mask=SAPosition;
		  wid_attributes.position=0;
		  wid_SetAttributes(tk_display,ls_colors_items,&wid_attributes,True);
		  */
		  WIN_MapRaised(wm_colors_index);
		  windows[wm_desktop_index].state.isFrozen=True;
		  windows[wm_desktop_index].state.isMapped=True;
		  for(i=1;i<9;i++)
		    wid_SetState(tk_display,bn_colors_widgets[i],Unpushed);
		  wid_SetState(tk_display,bn_colors_widgets[0],Pushed+Blocked);
		  wid_SetState(tk_display,bn_colors_map,Grayed);
		  cl_index.current_widget=0;
		  cl_index.current_item=-1;
		  CL_GetPreferences();
		  CL_SetupDesk();
		  wid_GiveFocus(tk_display,bn_colors_cancel);
		  wid_SetPosition(tk_display,ls_colors_items,0);
		  wid_SetPosition(tk_display,ls_colors_rgb_names,cl_index.desk_fg);
		  XSetWindowBackground(tk_display->display,color_rgb,wm_info.desk_fg);

		  XSetWindowBackground(tk_display->display,color_button,cl_manager.win_colors.bg);
		  XSetWindowBackground(tk_display->display,color_scroll,cl_manager.win_colors.bg);
		  XSetWindowBackground(tk_display->display,color_edit,cl_manager.win_colors.bg);
		  XSetWindowBackground(tk_display->display,color_list,cl_manager.win_colors.bg);
		  XSetWindowBackground(tk_display->display,color_menu,cl_manager.win_colors.bg);

		  return 0;
		}

		else if(tk_event.button==bn_desktop_clipboard)
		{
		  i=wm_clipboard_index;
		  windows[i].state.isInitialized=True; 
		  windows[i].state.isMapped=True;
		  windows[i].state.isIconic=False;
		  windows[i].state.isOnTop=False;
	
		  WIN_DrawUnactive(wm_clipboard_index);
		  XMapWindow(tk_display->display,wm_clipboard_window);
		  XMapWindow(tk_display->display,windows[wm_clipboard_index].mainwindow);
		  XUnmapWindow(tk_display->display,windows[i].icon.window);
		  restack[0]=windows[wm_desktop_index].mainwindow;
		  restack[1]=windows[wm_clipboard_index].mainwindow;
		  XRestackWindows(tk_display->display,restack,2);
		}

		else if(tk_event.button==bn_desktop_cancel)
		{
	DESKTOP_CANCEL:
		  DSK_GetPreferences();
		  XUnmapWindow(tk_display->display,wm_desktop_window);
		  send_event.type=UnmapNotify;
		  send_event.xunmap.window=wm_desktop_window;
		  send_event.xunmap.type=UnmapNotify;
		  XSendEvent(tk_display->display,wm_desktop_window,False,0,&send_event);
		  return 0;
		}

		else if(tk_event.button==bn_desktop_ok)
		{
		  wid_GetText(tk_display,ed_desktop_screensaver,(unsigned char **)&text);
		  if(strlen(text)>0)
		  {
		    wm_info.dsk_screensaver_time=atoi(text);
		    if(wm_info.dsk_screensaver_time<=0)
		    {
			wm_info.dsk_screensaver_time=10;
			wid_text.mask=STText|STFont|STKey;
		       	wid_text.text="10";
 		  	wid_text.key=0;
 			wid_text.font=tk_display->fonts.f8_13;
			if(wm_info.dsk_screensaver==1)
			  XSetScreenSaver(tk_display->display,wm_info.dsk_screensaver_time*60,0,DefaultBlanking,DefaultExposures);
		      	wid_SetTextDecoration(tk_display,ed_desktop_screensaver,&wid_text,False);
		     }
		     else if(wm_info.dsk_screensaver==1)
		       XSetScreenSaver(tk_display->display,wm_info.dsk_screensaver_time*60,0,DefaultBlanking,DefaultExposures);
		     free(text);
		   }
		   else
		   { 
			wm_info.dsk_screensaver_time=10;
			wid_text.mask=STText|STFont|STKey;
		       	wid_text.text="10";
 		  	wid_text.key=0;
 			wid_text.font=tk_display->fonts.f8_13;
			if(wm_info.dsk_screensaver==1)
			  XSetScreenSaver(tk_display->display,wm_info.dsk_screensaver_time*60,0,DefaultBlanking,DefaultExposures);
		      	wid_SetTextDecoration(tk_display,ed_desktop_screensaver,&wid_text,False);
		   }
		   DSK_SavePreferences();
		   XUnmapWindow(tk_display->display,wm_desktop_window);
		   send_event.type=UnmapNotify;
		   send_event.xunmap.window=wm_desktop_window;
		   send_event.xunmap.type=UnmapNotify;
		   XSendEvent(tk_display->display,wm_desktop_window,False,0,&send_event);
		   return 0;
		 }

	   	 else if(tk_event.button==bn_paper_center)
		 {
		   wm_info.dsk_paper_drawing=1;
	   	   wid_SetState(tk_display,bn_paper_mos,Pushed);
		   return 0;
		 }

	   	 else if(tk_event.button==bn_paper_mos)
		 {
	   	   wid_SetState(tk_display,bn_paper_center,Pushed);
		   wm_info.dsk_paper_drawing=0;
		   return 0;
		 }

		 else if(tk_event.button==bn_desktop_about)
		 {
			i=wm_about_index;
			windows[i].state.isInitialized=True; 
			windows[i].state.isMapped=True;
			windows[i].state.isOnTop=True;

			wm_action.window=0;
			wm_action.type=NoAction;
			WID_Freeze(tk_display,wm_desktop_window);
			WIN_MapRaised(wm_about_index);
			wid_GiveFocus(tk_display,bn_about_ok);
		  	windows[wm_desktop_index].state.isFrozen=True;
			windows[wm_desktop_index].state.isMapped=True;
			return 0;
		 }		
		 return 0;
		 break;



	case BN_KEYUNKNOWN :

		if(tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Escape))
		  goto DESKTOP_CANCEL;

		break;



	case CB_KEYUNKNOWN:
		if(tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Escape))
		  goto DESKTOP_CANCEL;

		break;
	


  }	
}






void DSK_MotifEvents()
{
 XEvent send_event;
 int i, j, x, y, ret;
 unsigned int w,h;
 ListItem *sel;
 Window root;
 unsigned int depth, border;

 switch(tk_event.ev_type)
 {
	case CB_PROPOSITION:
	case CB_VALIDATION:

		i=wid_GetPosition(tk_display,cb_desktop_motif);
		if(i<0) i=0;
		if(i>=numpixmotifs) i=0;
		if(wm_info.dsk_paper==0) DSK_SetMotif(i);
		wm_info.dsk_motif=i;
		break;


	case CB_KEYUNKNOWN:
		if(tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Escape))
		{
		  DSK_GetPreferences();
		  XUnmapWindow(tk_display->display,wm_desktop_window);
		  send_event.type=UnmapNotify;
		  send_event.xunmap.window=wm_desktop_window;
		  send_event.xunmap.type=UnmapNotify;
		  XSendEvent(tk_display->display,wm_desktop_window,False,0,&send_event);
		}
		break;

 }

}





void DSK_DefaulticonEvents()
{
 XEvent send_event;
 int i, j, x, y, ret;
 unsigned int w,h;
 ListItem *sel;
 Window root;
 unsigned int depth, border;

 switch(tk_event.ev_type)
 {
	case CB_PROPOSITION:
	case CB_VALIDATION:

		i=wid_GetPosition(tk_display,cb_desktop_defaulticon);
		if(i<0) i=0;
		if(i>=2) i=0;
		DSK_SetDefaulticon(i);
		wm_info.dsk_defaulticon=i;
		break;


	case CB_KEYUNKNOWN:
		if(tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Escape))
		{
		  DSK_GetPreferences();
		  XUnmapWindow(tk_display->display,wm_desktop_window);
		  send_event.type=UnmapNotify;
		  send_event.xunmap.window=wm_desktop_window;
		  send_event.xunmap.type=UnmapNotify;
		  XSendEvent(tk_display->display,wm_desktop_window,False,0,&send_event);
		}
		break;

 }

}





void DSK_ScreensaverEvents()
{
 XEvent send_event;
 int i, j, x, y, ret;
 unsigned int w,h;
 ListItem *sel;
 WidgetTextDecoration wid_text;
 Window root;
 unsigned int depth, border;
 unsigned char *text;

 switch(tk_event.ev_type)
 {

	case BN_RELEASED:

		if(tk_event.button==bn_desktop_screensaver_test)
		{
		  if(wm_info.dsk_screensaver==1)
		    XActivateScreenSaver(tk_display->display);
		}
		break;


	case ED_VALIDATION :

		if(tk_event.edit==ed_desktop_screensaver)
		{
  SET_SCREEN_SAVER:
		  wid_GetText(tk_display,ed_desktop_screensaver,(unsigned char **)&text);
		  if(strlen(text)>0)
		  {
		    wm_info.dsk_screensaver_time=atoi(text);
		    if(wm_info.dsk_screensaver_time<=0)
		    {
			wm_info.dsk_screensaver_time=10;
			wid_text.mask=STText|STFont|STKey;
		       	wid_text.text="10";
 		  	wid_text.key=0;
 			wid_text.font=tk_display->fonts.f8_13;
			if(wm_info.dsk_screensaver==1)
			  XSetScreenSaver(tk_display->display,wm_info.dsk_screensaver_time*60,0,DefaultBlanking,DefaultExposures);
		      	wid_SetTextDecoration(tk_display,ed_desktop_screensaver,&wid_text,False);
		     }
		     else if(wm_info.dsk_screensaver==1)
		       XSetScreenSaver(tk_display->display,wm_info.dsk_screensaver_time*60,0,DefaultBlanking,DefaultExposures);
		     free(text);
		   }
		   else
		   { 
			wm_info.dsk_screensaver_time=10;
			wid_text.mask=STText|STFont|STKey;
		       	wid_text.text="10";
 		  	wid_text.key=0;
 			wid_text.font=tk_display->fonts.f8_13;
			if(wm_info.dsk_screensaver==1)
			  XSetScreenSaver(tk_display->display,wm_info.dsk_screensaver_time*60,0,DefaultBlanking,DefaultExposures);
		      	wid_SetTextDecoration(tk_display,ed_desktop_screensaver,&wid_text,False);
		   }		  

		}
		break;

	case ED_KEYUNKNOWN:
		if(tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Escape))
		  goto DESKTOP_CANCEL;
		break;


	case CB_PROPOSITION:
	case CB_VALIDATION:

		i=wid_GetPosition(tk_display,cb_desktop_screensaver);
		if(i<0) i=0;
		if(i>=2) i=0;
		wm_info.dsk_screensaver=i;
		if(wm_info.dsk_screensaver==0)
		{
		  XSetScreenSaver(tk_display->display,0,0,DefaultBlanking,DefaultExposures);
		  wid_SetState(tk_display,bn_desktop_screensaver_test,Grayed);
		  wid_SetState(tk_display,bn_desktop_screensaver_install,Grayed);
		  wid_SetState(tk_display,ed_desktop_screensaver,Grayed);
		}
		if(wm_info.dsk_screensaver==1)
		{
		  /*XSetScreenSaver(tk_display->display,wm_info.dsk_screensaver_time*60,0,DefaultBlanking,DefaultExposures);*/
		  wid_SetState(tk_display,bn_desktop_screensaver_test,Ungrayed);
		  wid_SetState(tk_display,bn_desktop_screensaver_install,Ungrayed);
		  wid_SetState(tk_display,ed_desktop_screensaver,Ungrayed);
		  goto SET_SCREEN_SAVER;
		}
		break;


	case CB_KEYUNKNOWN:
		if(tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Escape))
		{
	     DESKTOP_CANCEL:
		  DSK_GetPreferences();
		  XUnmapWindow(tk_display->display,wm_desktop_window);
		  send_event.type=UnmapNotify;
		  send_event.xunmap.window=wm_desktop_window;
		  send_event.xunmap.type=UnmapNotify;
		  XSendEvent(tk_display->display,wm_desktop_window,False,0,&send_event);
		}
		break;

 }

}






