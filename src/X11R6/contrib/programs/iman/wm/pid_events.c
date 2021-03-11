/*
 *
 * 	pid_events.c  
 * 	evenements de la fenetre de processus
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
#include <X11/keysym.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "iman.h"





void PID_Events()
{
 WidgetItem wid_item;
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




  switch(tk_event.ev_type)
  {


	case BN_RELEASED:


			if(tk_event.button==bn_process_kill) 
			{
			  tk_event.list=ls_process_win;
			  goto DELETE_BEGIN;
			}

			else if(tk_event.button==bn_process_focus) 
			{
			  j=-1;
		    	  j=wid_GetPosition(tk_display,ls_process_win);
		    	  if(j==-1) goto LIST_VALIDATION_END;
/***  EN ATTENDANT QUELQUE CHOSE DE PLUS ELEGANT  ***/
			  list=tk_display->widgets[ls_process_win].list;

		    	  for(i=0;i<maxwindows;i++)
		    	  if(windows[i].isUsed==True && windows[i].state.isMapped==True&&windows[i].title_name==list->items[j].text&&windows[i].state.isIconic==False)
			  {
				PID_Unmap();
				WIN_MapRaised(i);
				goto BN_RELEASED_END;
			  }
			  else if(windows[i].isUsed==True && windows[i].state.isMapped==False && windows[i].state.isIconic==True&&windows[i].title_name==list->items[j].text)
			  {
				PID_Unmap();
				WIN_Uniconify(i);
				goto BN_RELEASED_END;
			  }

			}

			else if(tk_event.button==bn_process_close)
			{
			  j=-1;
		    	  j=wid_GetPosition(tk_display,ls_process_win);
/***  EN ATTENDANT QUELQUE CHOSE DE PLUS ELEGANT  ***/
			  list=tk_display->widgets[ls_process_win].list;
		    	  if(j==-1) goto LIST_VALIDATION_END;

		    	  for(i=0;i<maxwindows;i++)
			  {
		    	   if(windows[i].isUsed==True && windows[i].state.isMapped==True&&windows[i].title_name==list->items[j].text)
			   {
				if(i==wm_clipboard_index)
				{
				  XUnmapWindow(tk_display->display,wm_clipboard_window); 
			  	  wid_SetState(tk_display,bn_desktop_clipboard,Unpushed+Ungrayed);
				}
				else
				{
				  send_event.type=ClientMessage;
			  	  send_event.xclient.window=windows[i].clientwindow;
			  	  send_event.xclient.message_type=tk_display->atoms.WM_PROTOCOLS;
			  	  send_event.xclient.format=32;
				  send_event.xclient.data.l[0]=tk_display->atoms.WM_DELETE_WINDOW;
				  ret=XSendEvent(tk_display->display,windows[i].clientwindow,False,0,&send_event);
				  /*DLG_Unmap(wm_process_index);
				  XUnmapWindow(tk_display->display,wm_process_window);*/
				}
				goto BN_RELEASED_END;
			   }
			   else if(windows[i].isUsed==True && windows[i].state.isMapped==False&&windows[i].state.isIconic==True&&windows[i].title_name==list->items[j].text)
			   {
			  	send_event.type=ClientMessage;
			  	send_event.xclient.window=windows[i].clientwindow;
			  	send_event.xclient.message_type=tk_display->atoms.WM_PROTOCOLS;
			  	send_event.xclient.format=32;
				send_event.xclient.data.l[0]=tk_display->atoms.WM_DELETE_WINDOW;
				ret=XSendEvent(tk_display->display,windows[i].clientwindow,False,0,&send_event);
				goto BN_RELEASED_END;
			   }
			  }

			}


			else if(tk_event.button==bn_process_iconify)
			{
			  j=-1;
		    	  j=wid_GetPosition(tk_display,ls_process_win);
		    	  if(j==-1) goto BN_RELEASED_END;
/***  EN ATTENDANT QUELQUE CHOSE DE PLUS ELEGANT  ***/
			  list=tk_display->widgets[ls_process_win].list;
		    	  for(i=0;i<maxwindows;i++)
			  {																		
		    	   if(windows[i].isUsed==True && (windows[i].attributes&IconifyBox)==IconifyBox && windows[i].state.isMapped==True&&windows[i].title_name==list->items[j].text && windows[i].class==TOP_LEVEL_WINDOW&&windows[i].state.isFrozen==False&&windows[i].transient.isTransient==False)
			   {
				ret=WIN_Iconify(i);
				if(ret==0)
				{
				  WIN_DrawUnactive(i);
				  PID_GetMainWindows();
				  j=0;
				  k=wid_GetNumItems(tk_display,ls_process_win);
				  if(k==0) goto BN_RELEASED_END;
				  wid_GetItem(tk_display,ls_process_win,j,&wid_item);
				  while(windows[i].title_name!=wid_item.text && j<k) 
				  {
				    j++;
				    wid_GetItem(tk_display,ls_process_win,j,&wid_item);
				  }
				  if(j<k)
				    wid_SetPosition(tk_display,ls_process_win,j);
				  item_pixmap.mask=SPPixmap;
				  item_pixmap.pixmap=tk_display->pixmaps.iconify;
				  item_SetPixmapDecoration(tk_display,ls_process_win,j,&item_pixmap,False);
				}
				goto BN_RELEASED_END;
			   }															
			   else if(windows[i].isUsed==True && windows[i].state.isMapped==False&&windows[i].state.isIconic==True&&windows[i].title_name==list->items[j].text&&windows[i].class==TOP_LEVEL_WINDOW&&windows[i].state.isFrozen==False)
			   {
				PID_Unmap();
				item_pixmap.mask=SPPixmap;
				if(windows[i].state.isZoomed==False)
				  item_pixmap.pixmap=pix_motifs[0];
				else item_pixmap.pixmap=tk_display->pixmaps.zoom;
				item_SetPixmapDecoration(tk_display,ls_process_win,j,&item_pixmap,False);
				WIN_Uniconify(i);

				j=0;
				k=wid_GetNumItems(tk_display,ls_process_win);
				if(k==0) goto BN_RELEASED_END;
				wid_GetItem(tk_display,ls_process_win,j,&wid_item);
				while(windows[i].title_name!=wid_item.text && j<k) 
				{
				    j++;
				    wid_GetItem(tk_display,ls_process_win,j,&wid_item);
				}
			  	if(j<k)
				  wid_SetPosition(tk_display,ls_process_win,j);
				goto BN_RELEASED_END;
			   }
			  }
			}


			else if(tk_event.button==bn_process_zoom)
			{
			  j=-1;
		    	  j=wid_GetPosition(tk_display,ls_process_win);
/***  EN ATTENDANT QUELQUE CHOSE DE PLUS ELEGANT  ***/
			  list=tk_display->widgets[ls_process_win].list;

		    	  if(j==-1) goto BN_RELEASED_END;
		    	  for(i=0;i<maxwindows;i++)							
		    	  if(windows[i].isUsed==True&&(windows[i].attributes&ZoomBox)==ZoomBox&&windows[i].state.isMapped==True&&windows[i].title_name==list->items[j].text&&windows[i].state.isZoomed==False&&windows[i].state.isIconic==False&&windows[i].state.isFrozen==False&&windows[i].class==TOP_LEVEL_WINDOW)
			  {
				item_pixmap.mask=SPPixmap;
				item_pixmap.pixmap=tk_display->pixmaps.zoom;
				item_SetPixmapDecoration(tk_display,ls_process_win,j,&item_pixmap,False);
				WIN_Zoom(i);				
				goto BN_RELEASED_END;
			  }
			  else if(windows[i].isUsed==True&&(windows[i].attributes&ZoomBox)==ZoomBox&&windows[i].state.isMapped==True&&windows[i].title_name==list->items[j].text&&windows[i].state.isZoomed==True&&windows[i].state.isIconic==False&&windows[i].state.isFrozen==False&&windows[i].class==TOP_LEVEL_WINDOW)
			  {
				/*windows[i].state=NormalState;
				windows[i].initialstate=windows[i].state;
				WIN_SetClientState(i,NormalState);*/
				item_pixmap.mask=SPPixmap;
				item_pixmap.pixmap=pix_motifs[0];
				item_SetPixmapDecoration(tk_display,ls_process_win,j,&item_pixmap,False);
				WIN_Unzoom(i);
				goto BN_RELEASED_END;
			  }
			  fprintf(stderr,"Fenetre non conforme\n");

			}



			BN_RELEASED_END:
			break;




	case LS_VALIDATION:
	case LS_DOUBLECLICKED:
		
		if(tk_event.list==ls_process_win)
		{
		  j=-1;
		  j=wid_GetPosition(tk_display,ls_process_win);
/***  EN ATTENDANT QUELQUE CHOSE DE PLUS ELEGANT  ***/
		  list=tk_display->widgets[ls_process_win].list;
		  if(j==-1) goto LIST_VALIDATION_END;
		  for(i=0;i<maxwindows;i++)
		  {
		    if(windows[i].isUsed==True && windows[i].state.isMapped==True&&windows[i].title_name==list->items[j].text)
		    {
			WIN_Unmap(wm_process_index);
			WIN_MapRaised(i);
			goto LIST_VALIDATION_END;
		    }
		    else if(windows[i].isUsed==True && windows[i].state.isMapped==False&&windows[i].state.isIconic==True&windows[i].title_name==list->items[j].text)
		    {
			WIN_Unmap(wm_process_index);
			WIN_Uniconify(i);
			goto LIST_VALIDATION_END;
		    }

		  }
		}
		else if(tk_event.list==ls_process_pid)
		{
#ifdef DEBUG
		  fprintf(stderr,"PID unvalide\n");
#endif
		}
		
		LIST_VALIDATION_END:
		break;



	case LS_KEYUNKNOWN:

		if((tk_event.list==ls_process_win||tk_event.list==ls_process_pid))
		{
#ifdef DEBUG
		  fprintf(stderr,"LS_KEYUNKNOWN: code=%d  sym=%d\n",tk_event.event.xkey.keycode,XLookupKeysym(&tk_event.event.xkey,0));
#endif
		  if(XKeycodeToKeysym(tk_display->display,tk_event.event.xkey.keycode,0)==XK_Escape&&(tk_event.event.xkey.state&Mod1Mask)!=Mod1Mask&&(tk_event.event.xkey.state&ControlMask)!=ControlMask)
		    XUnmapWindow(tk_display->display,wm_process_window);
													
		  else if(XKeycodeToKeysym(tk_display->display,tk_event.event.xkey.keycode,0)==XK_Delete&&(tk_event.event.xkey.state&Mod1Mask)!=Mod1Mask&&(tk_event.event.xkey.state&ControlMask)!=ControlMask)
		  {

		   DELETE_BEGIN:
		   if(tk_event.list==ls_process_win)
		   {
		    j=-1;
		    j=wid_GetPosition(tk_display,ls_process_win);
		    if(j==-1) goto LIST_VALIDATION_END;
/***  EN ATTENDANT QUELQUE CHOSE DE PLUS ELEGANT  ***/
		    list=tk_display->widgets[ls_process_win].list;
		    for(i=0;i<maxwindows;i++)
		    if(i!=wm_clipboard_index)
		    {
		      if(windows[i].isUsed==True && windows[i].state.isMapped==True&&windows[i].title_name==list->items[j].text)
		      	KILL_Map(windows[i].clientwindow);

		      else if(windows[i].isUsed==True && windows[i].state.isMapped==False&&windows[i].state.isIconic==True&&windows[i].title_name==list->items[j].text)
			KILL_Map(windows[i].clientwindow);
		      

		    }
		   }
#ifdef DEBUG
		   else fprintf(stderr,"Impossible de tuer un PID\n");
#endif
		  }

		}
		break;



  }


}



