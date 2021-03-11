/*
 *
 * 	ev_widgets.c  
 * 	gestion des evenements des widgets
 *
 * 	Modification :  16/01/94
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
 *      IMAN Window Manager version 1.1.a
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





void WM_WidgetsEvents()
{
 ItemPixmapDecoration item_pixmap;
 ListStruct *list;

 XSetWindowAttributes attrib;
 XWindowAttributes xwa;
 Window qt_root, qt_parent, *qt_children;
 Window tc_child;
 XGCValues xgcvalues;
 XEvent send_event, eventbis;

 unsigned int qt_numchildren;
 int mask;
 int ret;
 int i, j, k, w;
 unsigned long win_type;




  switch(tk_event.ev_type){


        case JUSTFOCUS: /*fprintf(stderr,"Just Focus\n");*/
			if(tk_event.ev_widget==WI_BUTTON&&tk_event.button==bn_colors_ok||tk_event.button==bn_colors_cancel||tk_event.button==bn_colors_map||tk_event.button==bn_colors_help)
			  CL_Events();
			for(i=0;i<10;i++)
			  if(tk_event.ev_widget==WI_BUTTON&&tk_event.button==bn_colors_widgets[i])
			    CL_Events();
			if(tk_event.ev_widget==WI_LIST&&tk_event.list==ls_colors_items)
			  CL_Events();
			break;

	case BN_PUSHED:

		if(wm_action.type!=KillProcessAction)
		{
		  i=0;
		  while(tk_event.button!=windows[i].bn_close && tk_event.button!=windows[i].bn_iconify && tk_event.button!=windows[i].bn_zoom && i<maxwindows)
		    i++;

		  if(i<maxwindows&&windows[i].isUsed==False)
		  {
			XUnmapWindow(tk_display->display,windows[i].mainwindow);
			WM_VerifyWindows();
			goto BN_RELEASED_END;
		  }

		  if(i<maxwindows&&windows[i].state.isOnTop==False)
		  {
		    if(i!=wm_desktop_index&&i!=wm_colors_index&&i!=wm_process_index&&i!=wm_setup_index)
		      WM_UnmapSystemWindows();
		    if(windows[i].isUsed==True && windows[i].state.isMapped==True&&windows[i].state.isOnTop==False) 
		      WIN_MapRaised(i);
		    else if(windows[i].isUsed==True && windows[i].state.isMapped==True&&windows[i].state.isOnTop==True) 
		      WIN_Map(i);
		  }
		  else
		  {

		    if(tk_event.button==bn_paper_center||tk_event.button==bn_paper_mos)
		      DSK_Events();

		    else if(tk_event.button==bn_colors_ok||tk_event.button==bn_colors_cancel||tk_event.button==bn_colors_map||tk_event.button==bn_colors_help)
		      CL_Events();

		    else for(i=0;i<10;i++)
		      if(tk_event.ev_widget==WI_BUTTON&&tk_event.button==bn_colors_widgets[i])
			CL_Events();

		  }
		}

		break;



	case BN_RELEASED:

		if(tk_event.ev_widget==WI_BUTTON)
		{
		 i=0;
		 while(tk_event.button!=windows[i].bn_close && tk_event.button!=windows[i].bn_iconify && tk_event.button!=windows[i].bn_zoom && i<maxwindows)
		   i++;
		 if(i<maxwindows)
		 {
		   if(tk_event.button==windows[i].bn_zoom && windows[i].state.isZoomed==False && windows[i].state.isFrozen==False && windows[i].state.isMapped==True && wm_action.type!=KillProcessAction)
		   {
		     WIN_Zoom(i);
		     goto BN_RELEASED_END;
		   }
		   else if(tk_event.button==windows[i].bn_zoom && windows[i].state.isZoomed==True&&wm_action.type!=KillProcessAction&& windows[i].state.isMapped==True&&windows[i].state.isFrozen==False)
		   {
			/*windows[i].state=NormalState;
			windows[i].initialstate=windows[i].state;
			WIN_SetClientState(i,NormalState);*/
			WIN_Unzoom(i);
			goto BN_RELEASED_END;
		   }
		   else if(tk_event.button==windows[i].bn_zoom && wm_action.type!=KillProcessAction&& windows[i].state.isMapped==True&&windows[i].state.isFrozen==True)
		   {
			WIN_MapRaised(i);
			goto BN_RELEASED_END;
		   }
		   else if(tk_event.button==windows[i].bn_iconify && windows[i].state.isFrozen==False&&wm_action.type!=KillProcessAction&&windows[i].transient.isTransient==False)
		   {
			WIN_Iconify(i);
			WM_FindToplevelFocus();			
			goto BN_RELEASED_END;
		   }
		   else if(tk_event.button==windows[i].bn_iconify && windows[i].state.isFrozen==False&&wm_action.type!=KillProcessAction&&windows[i].transient.isTransient==True)
		   {
			WIN_MapRaised(i);
			goto BN_RELEASED_END;
		   }
		   else if(tk_event.button==windows[i].bn_close&& windows[i].state.isFrozen==False&&wm_action.type!=KillProcessAction)
		   {
			if(i==wm_clipboard_index)
			{
			  XUnmapWindow(tk_display->display,wm_clipboard_window); 
			  wid_SetState(tk_display,bn_desktop_clipboard,Unpushed+Ungrayed);
			  WIN_Unmap(wm_clipboard_index);
			  WM_FindToplevelFocus();
			}
			else if(windows[i].clientwindow==wm_colors_window)
			  CL_Close();			  

			else if(windows[i].clientwindow==wm_process_window)
			  XUnmapWindow(tk_display->display,wm_process_window);  
			else if(windows[i].clientwindow==wm_setup_window)
			{
			  tk_event.button=bn_setup_cancel;
			  SET_Events();
			} 
			else if(windows[i].clientwindow==wm_desktop_window)
			{
			  tk_event.button=bn_desktop_cancel;
			  DSK_Events();
			} 
			else
			{
			  send_event.xclient.message_type=tk_display->atoms.WM_PROTOCOLS;
			  send_event.type=ClientMessage;
			  send_event.xclient.window=windows[i].clientwindow;
			  send_event.xclient.format=32;
			  send_event.xclient.data.l[0]=tk_display->atoms.WM_DELETE_WINDOW;
			  ret=XSendEvent(tk_display->display,windows[i].clientwindow,False,0,&send_event);
			}
			goto BN_RELEASED_END;

		   }
		   else if(tk_event.button==windows[i].bn_close&& windows[i].state.isFrozen==True&&wm_action.type!=KillProcessAction)
		   {
			WIN_MapRaised(i);
			goto BN_RELEASED_END;
		   }
		   goto BN_RELEASED_END;

		 }
		 else{ 
			if(tk_event.button==bn_kill_ok&&wm_action.window!=wm_kill_window)
			  KILL_Events();
			else if(tk_event.button==bn_kill_cancel)
			  KILL_Events();

		        else if(tk_event.button==bn_paper_center||tk_event.button==bn_paper_mos||tk_event.button==bn_desktop_colors||tk_event.button==bn_desktop_about||tk_event.button==bn_desktop_cancel||tk_event.button==bn_desktop_ok||tk_event.button==bn_desktop_clipboard)
		          DSK_Events();

			else if(tk_event.button==bn_colors_cancel||tk_event.button==bn_colors_ok||tk_event.button==bn_colors_map||tk_event.button==bn_colors_help)
			  CL_Events();

			else if(tk_event.button==bn_end_cancel||tk_event.button==bn_end_ok)
			  END_Events();

			else if(tk_event.button==bn_about_ok)
			  AB_Events();  

			else if(tk_event.button==bn_process_kill||tk_event.button==bn_process_focus||tk_event.button==bn_process_close||tk_event.button==bn_process_iconify||tk_event.button==bn_process_zoom)
			  PID_Events();
			  
			else if(tk_event.button==bn_setup_cancel||tk_event.button==bn_setup_ok)
			  SET_Events();

			else if(tk_event.button==bn_desktop_screensaver_test||tk_event.button==bn_desktop_screensaver_install)
			  DSK_ScreensaverEvents();

			for(i=0;i<10;i++)
			  if(tk_event.ev_widget==WI_BUTTON&&tk_event.button==bn_colors_widgets[i])
			    CL_Events();




		   goto BN_RELEASED_END;
		 }
		 
		}

		BN_RELEASED_END:
		break;



	case BN_KEYUNKNOWN:

		if((tk_event.button==bn_kill_ok||tk_event.button==bn_kill_cancel)&&XKeycodeToKeysym(tk_display->display,tk_event.event.xkey.keycode,0)==XK_Escape)
		  KILL_Events();
		else if((tk_event.button==bn_end_ok||tk_event.button==bn_end_cancel)&&XKeycodeToKeysym(tk_display->display,tk_event.event.xkey.keycode,0)==XK_Escape)
		  END_Events();
		else if(tk_event.button==bn_colors_ok||tk_event.button==bn_colors_cancel||tk_event.button==bn_colors_map||tk_event.button==bn_colors_help)
		  CL_Events();
		else if(tk_event.button==bn_desktop_screensaver_test || tk_event.button==bn_desktop_screensaver_install || tk_event.button==bn_desktop_ok || tk_event.button==bn_desktop_cancel || tk_event.button==bn_desktop_about || tk_event.button==bn_desktop_help || tk_event.button==bn_desktop_clipboard || tk_event.button==bn_desktop_colors || tk_event.button==bn_paper_mos || tk_event.button==bn_paper_center)
		  DSK_Events();
		else if(tk_event.button==bn_setup_ok||tk_event.button==bn_setup_cancel||tk_event.button==bn_setup_groups||tk_event.button==bn_setup_icons||tk_event.button==bn_setup_icontitle)
		  SET_Events();
		for(i=0;i<10;i++)
		  if(tk_event.ev_widget==WI_BUTTON&&tk_event.button==bn_colors_widgets[i])
		    CL_Events();
		if(tk_event.button==bn_about_ok)
		  AB_Events();  

		break;


	case LS_CLICKED:
	case LS_RELEASED:

/****** A COMPLETER *****************************************/
		if(tk_event.list==ls_colors_items||tk_event.list==ls_colors_rgb_names)
		  CL_Events();
		break;



	case LS_VALIDATION:
	case LS_DOUBLECLICKED:
		
		if(tk_event.list==ls_process_win||tk_event.list==ls_process_pid)
		  PID_Events();
		else if(tk_event.list==ls_colors_items||tk_event.list==ls_colors_rgb_names)
		  CL_Events();
		
		LIST_VALIDATION_END:
		break;



	case LS_KEYUNKNOWN:

		if(tk_event.list==ls_process_win||tk_event.list==ls_process_pid)
		  PID_Events();
		else if(tk_event.list==ls_colors_items||tk_event.list==ls_colors_rgb_names)
		  CL_Events();
		break;



	case CB_VALIDATION :
	case CB_KEYUNKNOWN :
	case CB_PROPOSITION :

		/*fprintf(stderr,"CB event %d  %ld\n",tk_event.ev_type,tk_event.combo);*/
		if(tk_event.combo==cb_desktop_paper)
		  DSK_Events();
		if(tk_event.combo==cb_desktop_motif)
		  DSK_MotifEvents();
		else if(tk_event.combo==cb_desktop_defaulticon)
		  DSK_DefaulticonEvents();
		else if(tk_event.combo==cb_desktop_screensaver)
		  DSK_ScreensaverEvents();
		break;


	case ED_KEYUNKNOWN:
	case ED_VALIDATION:

		if(tk_event.edit==ed_desktop_screensaver)
		  DSK_ScreensaverEvents();
		break;


  }


}



