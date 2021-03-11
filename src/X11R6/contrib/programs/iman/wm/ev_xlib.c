/*
 *
 * 	ev_xlib.c
 * 	gestion des evenements XLIB
 *
 * 	Modification :  30/01/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute and sell this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appears in all copies and that both that
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
#include <X11/keysym.h>


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "iman.h"



/*
#define DEBUG 1
*/






void WM_XlibEvents()
{
 XSetWindowAttributes attrib;
 XWindowAttributes xwa;
 XWindowChanges xwc;
 Window qt_root, qt_parent, *qt_children;
 Window tc_child;
 XGCValues xgcvalues;
 XEvent send_event, eventbis;

 unsigned int qt_numchildren;
 unsigned long ptr;
 int mask, ret;
 int i, j, k, w;
 long win_type, win_number;
 int tc_x, tc_y;
 int shape_event;
 XShapeEvent *shapev;
 XRectangle rectangle;
 int add_top, add_bottom, add_leftside, add_rightside;
 int map_flag=0;



  switch(tk_event.event.type)
  {


	case CreateNotify:

	      map_flag=0;
	    CREATE_START:
	      if(wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction) 
	      {
		WM_CheckFreeWindows();		
#ifdef DEBUG
		fprintf(stderr,"***** Create notify  %ld\n",tk_event.event.xcreatewindow.window);
#endif
		memset(&xwa,0,sizeof(XWindowAttributes));  

		XSync(tk_display->display,False);
		if(XCheckTypedWindowEvent(tk_display->display,tk_event.event.xcreatewindow.window,DestroyNotify,&send_event)==True)
 		  goto CREATE_END;
				
		win_type=win_GetType(tk_display,tk_event.event.xcreatewindow.window);
		if(win_type==-1||(win_type!=ICON_WINDOW&&win_type!=TITLE_BAR&&win_type!=BORDER_BOX&&win_type!=REPARENTING_DIALOG&&win_type!=REPARENTING_WINDOW&&win_type!=TOP_LEVEL_WINDOW&&win_type!=DIALOG_BOX)) 
		  win_type=TOP_LEVEL_WINDOW;
		ret=XGetWindowAttributes(tk_display->display,tk_event.event.xcreatewindow.window,&xwa);


		if(ret!=0&&xwa.override_redirect==False&&tk_event.event.xcreatewindow.override_redirect==False &&(win_type==TOP_LEVEL_WINDOW||win_type==-1||win_type==DIALOG_BOX||(win_type!=ICON_WINDOW&&win_type!=TITLE_BAR&&win_type!=BORDER_BOX&&win_type!=REPARENTING_DIALOG&&win_type!=REPARENTING_WINDOW)))
		{																								
		  j=WM_GetUnusedWindow(win_type);  
		  if(j<0 || j>=maxwindows || windows[j].isUsed==True)
		  {
		    fprintf(stderr,"Erreur de NUMERO\n");
		    goto CREATE_END;
		  }
		  ret=WIN_ReparentClient(j,tk_event.event.xcreatewindow.window);
		  
		  if(ret!=0)
		  {
		    /*fprintf(stderr,"ERREUR AU REPARENTAGE\n");*/
		    WM_ReInitWindow(j);
		    goto CREATE_END;
		  }
		  numwindows++;
		  windows[j].isUsed=True;
#ifdef DEBUG
		  fprintf(stderr,"Fenetre creee ret=%d\n",ret);
#endif
		  if(map_flag==1)
		  {
		    tk_event.event.type=MapRequest;
		    tk_event.event.xmaprequest.window=tk_event.event.xcreatewindow.window;
		  }
		  goto CREATE_END;
		}

		else if(ret!=0&&xwa.override_redirect==True&&win_type!=ICON_WINDOW&&win_type!=MN_FLOATING&&win_type!=MN_MENUBAR&&win_type!=TITLE_BAR)
 		{
#ifdef DEBUG
		  fprintf(stderr,"Override window\n");
#endif
		  /*XSelectInput(tk_display->display,tk_event.event.xcreatewindow.window,StructureNotifyMask|PropertyChangeMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask);*/
		  goto CREATE_END;
		}
		else if(ret!=0 && (win_type==MN_FLOATING||win_type==MN_MENUBAR))
		{
#ifdef DEBUG
		  fprintf(stderr,"MENU WINDOW !!!\n");
#endif
		  goto CREATE_END;
		}
#ifdef DEBUG
		else fprintf(stderr,"ERREUR CREATION DE FENETRE\n");
#endif
	      }

	CREATE_END:
	      XSync(tk_display->display,False);
	      WM_CheckFreeWindows();
	      if(map_flag==1)
	        goto MAP_REQUEST_BEGIN;
	      map_flag=0;
	      break;




	case DestroyNotify:
				
	      if(wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction)
	      {
	
		DESTROY_BEGINNING:
		
		win_number=WIN_GetNumber(tk_event.event.xdestroywindow.window);
#ifdef DEBUG
		fprintf(stderr,"Destroy window\n");
#endif	
		
		if(win_number>=0&&win_number<maxwindows)
		{

		  WM_ReInitWindow(win_number);
		  windows[win_number].isUsed=False;
		  numwindows--; 
		  goto DESTROY_SUITE;
		}
		else goto DESTROY_CLOSE;

				
		DESTROY_SUITE:

		WM_VerifyWindows();
 		XSync(tk_display->display,False);
		XFlush(tk_display->display);
		send_event.xdestroywindow.window=0;

 		if(XCheckTypedEvent(tk_display->display,DestroyNotify,&send_event)==True)
		{
		  if(tk_event.event.xdestroywindow.window==send_event.xdestroywindow.window)
		    goto DESTROY_SUITE;
		  else if(WM_GetWindowClassWithoutProperty(send_event.xdestroywindow.window)==-1)
		    goto DESTROY_SUITE;
		  tk_event.event.xdestroywindow.window=send_event.xdestroywindow.window;
 	 	  tk_event.event.type=DestroyNotify;
		  goto DESTROY_BEGINNING;
		}

		PID_GetMainWindows();
		WM_FindToplevelFocus();

	      DESTROY_CLOSE:
		WM_VerifyWindows();
		i=i;
	      }	
	      XSync(tk_display->display,False);
	      break;




	case MapRequest:

	      MAP_REQUEST_BEGIN:

#ifdef DEBUG
		fprintf(stderr,"***** Map request *********  %ld\n",tk_event.event.xmaprequest.window);
#endif
	      if(wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction)
	      {
#ifdef DEBUG
		fprintf(stderr,"Map event traitee\n");
#endif
		memset(&xwa,0,sizeof(XWindowAttributes));  
		XSync(tk_display->display,False);
		XFlush(tk_display->display);
		if(XCheckTypedWindowEvent(tk_display->display,tk_event.event.xmaprequest.window,DestroyNotify,&send_event)==True)
		{
		  tk_event.event.type=DestroyNotify;
		  tk_event.event.xdestroywindow.window=tk_event.event.xmaprequest.window;
		  goto DESTROY_BEGINNING;
		}
		XSync(tk_display->display,False);
		XFlush(tk_display->display);
		if(XCheckTypedWindowEvent(tk_display->display,tk_event.event.xmaprequest.window,UnmapNotify,&send_event)==True)
		{
		  tk_event.event.type=UnmapNotify;
		  tk_event.event.xunmap.window=tk_event.event.xmaprequest.window;
		  goto UNMAP_START;
		}
#ifdef DEBUG
		(void)fprintf(stderr,"numwindows:%d maxwindows:%d\n",numwindows,maxwindows);
#endif
		win_type=-1;
		win_type=WM_GetWindowClassWithoutProperty(tk_event.event.xmaprequest.window);
#ifdef DEBUG
		fprintf(stderr,"Map: type=%ld\n",win_type);
#endif

		if(win_type==TOP_LEVEL_WINDOW||win_type==DIALOG_BOX)
		{
		  i=-1;
		  i=WM_GetWindowNumber(tk_event.event.xmaprequest.window);
#ifdef DEBUG			
		  fprintf(stderr,"i=%d\n",i);	  
#endif
		  if(i>=0&&i<maxwindows)
		  { 

		    WM_UnmapSystemWindows();

		    if(windows[i].state.isInitialized==True)
		    {
		      ret=WIN_VerifyTree(i);
		      if((windows[i].attributes&GroupLeader)==GroupLeader)
		      	  windows[i].state.isWithdrawn=False;
		      if(ret==0&&windows[i].state.isIconic==False) ret=WIN_MapRaised(i);
		      else if(ret==0&&windows[i].state.isIconic==True) ret=WIN_Uniconify(i);
		      if(i==wm_desktop_index)
		        wid_GiveFocus(tk_display,bn_desktop_cancel);
		      else if(i==wm_setup_index)
		        wid_GiveFocus(tk_display,bn_setup_cancel);
		      XSync(tk_display->display,False);
		      XFlush(tk_display->display);

		    }
		    else
 		    {					
#ifdef DEBUG
		      fprintf(stderr,"Fenetre non-initialisee\n"); 
#endif
		      if(windows[i].state.initialstate==NormalState||windows[i].state.initialstate==DontCareState||windows[i].state.initialstate==InactiveState||(windows[i].state.initialstate==IconicState&&(windows[i].attributes&GroupMember)==GroupMember))
		      {
			windows[i].state.isInitialized=True; 
			windows[i].state.initialstate=NormalState;
			WIN_SetClientState(i,NormalState);
			ret=WIN_VerifyTree(i);
   		    	if((windows[i].attributes&GroupLeader)==GroupLeader)
		      	  windows[i].state.isWithdrawn=False;
			if(ret==0) ret=WIN_MapRaised(i);
			else
			  windows[i].state.isMapped=True;
		        if(i==wm_desktop_index)
		          wid_GiveFocus(tk_display,bn_desktop_cancel);
		      }
		      else if(windows[i].state.initialstate==IconicState&&(windows[i].attributes&GroupMember)!=GroupMember)
		      {
			windows[i].state.isInitialized=True; 
			windows[i].state.isMapped=True;
#ifdef DEBUG
			fprintf(stderr,"Iconic state\n");
#endif
			ret=WIN_VerifyTree(i);
			if(ret==0) ret=WIN_Iconify(i);
			else windows[i].state.isMapped=True;
			WIN_SetClientState(i,NormalState);
		      }
		      else if(windows[i].state.initialstate==ZoomState)
		      {
			windows[i].state.isInitialized=True; 
			windows[i].state.isMapped=True;
			windows[i].state.isZoomed=True;
			windows[i].state.initialstate=NormalState;
#ifdef DEBUG
			fprintf(stderr,"Zoom state\n");
#endif
			WIN_Zoom(i);
   		    	if((windows[i].attributes&GroupLeader)==GroupLeader)
		      	  windows[i].state.isWithdrawn=False;
			WIN_MapRaised(i);
		      }
		    }
		  }
		}

		else
		{ 
			memset(&xwa,0,sizeof(XWindowAttributes));  			
			ret=XGetWindowAttributes(tk_display->display,tk_event.event.xmaprequest.window,&xwa);
			if(ret==1 && xwa.override_redirect==False)
			  ret=XMapRaised(tk_display->display,tk_event.event.xmaprequest.window);
			/*fprintf(stderr,"Fin du map inconnu ret=%d\n",ret);*/
		}
	      }
	      MAP_REQUEST_END:
	      map_flag=0;
	      XSync(tk_display->display,False);
	      break;
			       




	case UnmapNotify:

	
	      if(wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction)
	      {
		map_flag=0;
		UNMAP_START:
#ifdef DEBUG
		fprintf(stderr,"Unmap event %ld\n",tk_event.event.xunmap.window);
#endif
		XSync(tk_display->display,False);
		XFlush(tk_display->display);
		win_type=-1;
		win_type=WM_GetWindowClassWithoutProperty(tk_event.event.xunmap.window);

		if(XCheckTypedWindowEvent(tk_display->display,tk_event.event.xunmap.window,DestroyNotify,&send_event)==True)
		{
#ifdef DEBUG
		  fprintf(stderr,"Unmap: destroy detecte\n");
#endif
		  if(win_type!=-1&&tk_event.event.xunmap.window==send_event.xdestroywindow.window)
		  {
		    tk_event.event.type=DestroyNotify;
		    tk_event.event.xdestroywindow.window=tk_event.event.xunmap.window;
		    goto DESTROY_BEGINNING;
		  }
		  else if(win_type!=-1) XPutBackEvent(tk_display->display,&send_event);
		}
		

		if(win_type==TOP_LEVEL_WINDOW||win_type==DIALOG_BOX)
		{
		  i=WM_GetWindowNumber(tk_event.event.xunmap.window);
 		  if(i>=0 && i<maxwindows&&windows[i].state.isMapped==True)
		  {
#ifdef DEBUG
		    fprintf(stderr,"WIN Unmap notify %ld\n",tk_event.event.xunmap.window);
#endif
		    WIN_Unmap(i);
   		    if((windows[i].attributes&GroupLeader)==GroupLeader)
		      windows[i].state.isWithdrawn=True;
		    if(i!=wm_end_index&&i!=wm_kill_index)
		      PID_GetMainWindows();
				    
 		    send_event=tk_event.event;
		    ret=True;
		    XSync(tk_display->display,False);
		    XFlush(tk_display->display);
		    while(windows[i].clientwindow==send_event.xunmap.window&&ret==True)
		      ret=XCheckTypedEvent(tk_display->display,UnmapNotify,&send_event);
		    if(ret==True) win_type=WM_GetWindowClass(send_event.xunmap.window);
		    else win_type=-1;
		    /*fprintf(stderr,"WIN Unmap continue %d\n",win_type);*/
		    if(ret==True&&windows[i].clientwindow!=send_event.xunmap.window&&(win_type==DIALOG_BOX||win_type==TOP_LEVEL_WINDOW))
		    {
			tk_event.event.xunmap.window=send_event.xunmap.window; 
			goto UNMAP_START;
		    }

		    WM_FindToplevelFocus();
 
		  }
				 
		}

	      }
	      XSync(tk_display->display,False);
	      break;



	case ResizeRequest:

#ifdef DEBUG
	      fprintf(stderr,"Resize request\n");
#endif
	      if(wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction)
	      {

		win_type=-1;
		win_type=WM_GetWindowClass(tk_event.event.xresizerequest.window);

		if(XCheckTypedWindowEvent(tk_display->display,tk_event.event.xresizerequest.window,DestroyNotify,&send_event)==True)
		{
		  tk_event.event.xdestroywindow.window=tk_event.event.xresizerequest.window;
		  goto DESTROY_BEGINNING;
		}
				
		if(win_type==TOP_LEVEL_WINDOW||win_type==DIALOG_BOX)
		{
		  i=0;
		  while(tk_event.event.xresizerequest.window!=windows[i].clientwindow && i>=0 && i<maxwindows)
		    i++;
		  /*fprintf(stderr,"WIN Resize Request  i:%d\n",i);*/
		  if(i>=0 && i<maxwindows)
		    WIN_ResizeClient(i,tk_event.event.xresizerequest.width,tk_event.event.xresizerequest.height);
		  
		}

		else XResizeWindow(tk_display->display,tk_event.event.xresizerequest.window,tk_event.event.xresizerequest.width,tk_event.event.xresizerequest.height);
	      }
	      XSync(tk_display->display,False);
	      break;



	case ConfigureRequest:
				
#ifdef DEBUG
	      fprintf(stderr,"Configure request %ld\n",tk_event.event.xconfigurerequest.window);
#endif
	      if(wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction)
	      {				
		XSync(tk_display->display,False);
		XFlush(tk_display->display);
		if(XCheckTypedWindowEvent(tk_display->display,tk_event.event.xconfigurerequest.window,DestroyNotify,&send_event)==True)
		{
#ifdef DEBUG
		  fprintf(stderr,"Configure Destroy\n");
#endif
		  tk_event.event.type=DestroyNotify;
		  tk_event.event.xdestroywindow.window=tk_event.event.xconfigurerequest.window;
		  goto DESTROY_BEGINNING;
		}
		if(XCheckTypedWindowEvent(tk_display->display,tk_event.event.xconfigurerequest.window,UnmapNotify,&send_event)==True)
		{
#ifdef DEBUG
		  fprintf(stderr,"Configure unmap\n");
#endif
		  tk_event.event.type=UnmapNotify;
		  tk_event.event.xunmap.window=tk_event.event.xconfigurerequest.window;
		  goto UNMAP_START;
		}
		win_type=-1;
		win_type=WM_GetWindowClassWithoutProperty(tk_event.event.xconfigurerequest.window);
		memset(&xwa,0,sizeof(XWindowAttributes));					



		if(win_type==TOP_LEVEL_WINDOW||win_type==DIALOG_BOX)
		{
		  i=-1;
		  i=WM_GetWindowNumber(tk_event.event.xconfigurerequest.window);

		  if(i>=0 && i<maxwindows && windows[i].isUsed==True)
		  {
		    	/*if(ret!=0&&xwa.x==tk_event.event.xconfigurerequest.x&&xwa.y==tk_event.event.xconfigurerequest.y&&xwa.width==tk_event.event.xconfigurerequest.width&&tk_event.event.xconfigurerequest.height==xwa.height&&windows[i].isOnTop==False)*/

			if((tk_event.event.xconfigurerequest.value_mask&CWStackMode)==CWStackMode&&tk_event.event.xconfigurerequest.detail==Above&&windows[i].state.isOnTop==False&&windows[i].state.isMapped==True)
			{
			  WM_UnmapSystemWindows();
		    	  WIN_MapRaised(i);
					 
		   	}
			if((tk_event.event.xconfigurerequest.value_mask&CWWidth)==CWWidth||(tk_event.event.xconfigurerequest.value_mask&CWHeight)==CWHeight)
			  WIN_ResizeClient(i,tk_event.event.xconfigurerequest.width,tk_event.event.xconfigurerequest.height);
			goto CONFIGURE_END;
		  }
#ifdef DEBUG
		  fprintf(stderr,"CONFIGURE REQUEST ERROR \n");
#endif
		  goto CONFIGURE_END;
		}
		else if(win_type==-1)
		{
		  XFlush(tk_display->display);
		  XSync(tk_display->display,False);
		  if(XCheckTypedWindowEvent(tk_display->display,tk_event.event.xconfigurerequest.window,UnmapNotify,&send_event)==True)
		  {
		    goto CONFIGURE_END;
		  }
#ifdef DEBUG
		  fprintf(stderr,"Configure request x:%d y:%d width:%d height:%d mask:%ld\n",tk_event.event.xconfigurerequest.x,tk_event.event.xconfigurerequest.y,tk_event.event.xconfigurerequest.width,tk_event.event.xconfigurerequest.height,tk_event.event.xconfigurerequest.value_mask);
		  fprintf(stderr,"Above : %d  detail: %d  CWStackMode:%d\n",Above,tk_event.event.xconfigurerequest.detail,CWStackMode);
#endif
		  xwc.x=tk_event.event.xconfigurerequest.x;
		  xwc.y=tk_event.event.xconfigurerequest.y;
		  xwc.width=tk_event.event.xconfigurerequest.width;
		  xwc.height=tk_event.event.xconfigurerequest.height;
		  xwc.sibling=tk_event.event.xconfigurerequest.above;
		  xwc.stack_mode=tk_event.event.xconfigurerequest.detail;
		  XConfigureWindow(tk_display->display,tk_event.event.xconfigurerequest.window,tk_event.event.xconfigurerequest.value_mask,&xwc);
		  /*XMoveResizeWindow(tk_display->display,tk_event.event.xconfigurerequest.window,tk_event.event.xconfigurerequest.x,tk_event.event.xconfigurerequest.y,tk_event.event.xconfigurerequest.width,tk_event.event.xconfigurerequest.height);*/
		  memset(&xwa,0,sizeof(XWindowAttributes));  
		  ret=XGetWindowAttributes(tk_display->display,tk_event.event.xconfigurerequest.window,&xwa);
		  if(ret==1 && xwa.override_redirect==False && xwa.map_state==IsViewable&&(tk_event.event.xconfigurerequest.value_mask&CWStackMode)==CWStackMode&&tk_event.event.xconfigurerequest.detail==Above)
		    XMapRaised(tk_display->display,tk_event.event.xconfigurerequest.window);
		}
	      }
	      CONFIGURE_END :
#ifdef DEBUG
	      fprintf(stderr,"Configure End\n");
#endif
	      XSync(tk_display->display,False);
	      break;



	case ReparentNotify: 
#ifdef DEBUG
			fprintf(stderr,"Reparent Notify  %ld  parent:%ld  root=%ld\n",tk_event.event.xreparent.window,tk_event.event.xreparent.parent,RootWindow(tk_display->display,tk_display->screen));
#endif
			XSync(tk_display->display,False);
			win_type=WM_GetWindowClassWithoutProperty(tk_event.event.xreparent.window);
			win_number=WM_GetWindowNumber(tk_event.event.xreparent.window);
			if(win_number==-1 && tk_event.event.xreparent.parent==RootWindow(tk_display->display,tk_display->screen))
			{
			  tk_event.event.type=CreateNotify;
			  tk_event.event.xcreatewindow.window=tk_event.event.xreparent.window;
			  tk_event.event.xcreatewindow.override_redirect=tk_event.event.xreparent.override_redirect;
			  tk_event.event.xcreatewindow.x=tk_event.event.xreparent.x;
			  tk_event.event.xcreatewindow.y=tk_event.event.xreparent.y;
			  ret=XGetWindowAttributes(tk_display->display,tk_event.event.xreparent.window,&xwa);
			  if(ret==1 && xwa.override_redirect==False)
			  {
			    if(xwa.map_state==IsViewable)
			      map_flag=1;
			    else map_flag=0;
			    goto CREATE_START;
			  }
			  
			}
#ifdef DEBUG
			fprintf(stderr,"Reparent END\n");
#endif
			map_flag=0;
	      		XSync(tk_display->display,False);
			break;


	case CirculateNotify: fprintf(stderr,"Circulate Notify\n");
			break;
	case CirculateRequest:fprintf(stderr,"Circulate request\n");
			break;
	case MapNotify: 
#ifdef DEBUG
			if(tk_event.event.xmap.override_redirect!=True)
			{
			  fprintf(stderr,"Map Notify");
			  i=WIN_GetNumber(tk_event.event.xmap.window);
			  if(i>=0 && i<maxwindows)
			    fprintf(stderr,"  i=%d  %s\n",i,windows[i].title_name);
			  else fprintf(stderr,"  i=%d ---\n",i);
			}
#endif
	      		XSync(tk_display->display,False);
			break;




	case ClientMessage:

		win_type=WM_GetWindowClassWithoutProperty(tk_event.event.xclient.window);
#ifdef DEBUG
		fprintf(stderr,"Message %d  CHANGE:%d  win_type:%d\n",tk_event.event.xclient.message_type,tk_display->atoms.WM_CHANGE_STATE,win_type);
#endif
		if((win_type==TOP_LEVEL_WINDOW||win_type==DIALOG_BOX||tk_event.event.xclient.window==RootWindow(tk_display->display,tk_display->screen))&&tk_event.event.xclient.message_type==tk_display->atoms.WM_CHANGE_STATE)
		{
#ifdef DEBUG
		  fprintf(stderr,"Etat en cours ...\n");
#endif
		  i=WIN_GetNumber(tk_event.event.xclient.window);
		  if(i<0||i>=maxwindows)
		    goto CLIENT_END;
		  WIN_SetState(i,tk_event.event.xclient.data.l[0]);
		}
		CLIENT_END:
	    	XSync(tk_display->display,False);
		break;


	case ColormapNotify :

		i=WIN_GetNumber(tk_event.event.xclient.window);
		if(i<0||i>=maxwindows)
		  goto COLORMAP_END;
		WIN_SetColormap(i);
		COLORMAP_END:
		XSync(tk_display->display,False);
		break;


	case PropertyNotify:						
				
		win_type=-1;
		win_type=WM_GetWindowClassWithoutProperty(tk_event.event.xproperty.window);
#ifdef DEBUG
		fprintf(stderr,"Property notify: %s\n",XGetAtomName(tk_display->display,tk_event.event.xproperty.atom));
#endif												
		if(tk_event.event.xproperty.atom==XA_WM_NAME&&wm_action.type!=EndOfAllAction&&tk_event.event.xproperty.state==PropertyNewValue)
		{
		  i=-1;
		  i=WM_GetWindowNumber(tk_event.event.xproperty.window);
		  if(i==-1) goto PROPERTY_END;
		  WIN_SetTitleName(i);
		  goto PROPERTY_END;	
		}
		else if(tk_event.event.xproperty.atom==XA_WM_ICON_NAME&&wm_action.type!=EndOfAllAction&&tk_event.event.xproperty.state==PropertyNewValue)
		{
		  if(win_type==TOP_LEVEL_WINDOW)
		  {
			i=-1;
			i=WM_GetWindowNumber(tk_event.event.xproperty.window);
			if(i==-1) goto PROPERTY_END;
			WIN_SetIconName(i);
		  }
#ifdef DEBUG
		  else fprintf(stderr,"ICONNAME: win_type inconnu\n");
#endif
		  goto PROPERTY_END;
		}
		else if(tk_event.event.xproperty.atom==XA_WM_HINTS&&wm_action.type!=EndOfAllAction&&tk_event.event.xproperty.state==PropertyNewValue)
		{
		  if(win_type==TOP_LEVEL_WINDOW||win_type==DIALOG_BOX)
		  {
			i=-1;
			i=WM_GetWindowNumber(tk_event.event.xproperty.window);
			if(i<0||i>=maxwindows) goto PROPERTY_END;
			WIN_SetWMHints(i);
		  }
#ifdef DEBUG
		  else fprintf(stderr,"WMHINTS: win_type inconnu\n");
#endif
		  goto PROPERTY_END;
		}
		else if(tk_event.event.xproperty.atom==XA_WM_TRANSIENT_FOR&&wm_action.type!=EndOfAllAction)
		{
		  i=-1;
		  i=WM_GetWindowNumber(tk_event.event.xproperty.window);
		  if(i==-1) goto PROPERTY_END;
		  WIN_SetTransientLeader(i);

		  goto PROPERTY_END;		  

		}

		/*else if(tk_event.event.xproperty.atom==XA_WM_COLORMAP&&wm_action.type!=EndOfAllAction)
		{
		  i=-1;
		  i=WM_GetWindowNumber(tk_event.event.xproperty.window);
		  if(i==-1) goto PROPERTY_END;

		  goto PROPERTY_END;		  

		}*/

#ifdef DEBUG
		else fprintf(stderr,"Property notify: %s\n",XGetAtomName(tk_display->display,tk_event.event.xproperty.atom));
#endif
		PROPERTY_END:
#ifdef DEBUG
		fprintf(stderr,"Property end\n");
#endif
	 	XSync(tk_display->display,False);
		break;





	case Expose:

		if(tk_event.event.xexpose.count==0)
		{
		 win_type=-1;
		 win_type=WM_GetWindowClassWithoutProperty(tk_event.event.xexpose.window);

		 XSync(tk_display->display,False);
		 XFlush(tk_display->display);
		 if(XCheckTypedWindowEvent(tk_display->display,tk_event.event.xexpose.window,DestroyNotify,&send_event)==True)
		 {
#ifdef DEBUG
		   fprintf(stderr,"Expose Destroy\n");
#endif
		   tk_event.event.type=DestroyNotify;
		   tk_event.event.xdestroywindow.window=tk_event.event.xexpose.window;
		   goto DESTROY_BEGINNING;
		 }
		 if(XCheckTypedWindowEvent(tk_display->display,tk_event.event.xexpose.window,UnmapNotify,&send_event)==True)
		 {
#ifdef DEBUG
		   fprintf(stderr,"Expose unmap\n");
#endif
		   tk_event.event.type=UnmapNotify;
		   tk_event.event.xunmap.window=tk_event.event.xexpose.window;
		   goto UNMAP_START;
		 }


		 if(win_type==TITLE_BAR)
		 {
#ifdef DEBUG
		   fprintf(stderr,"Expose titlebar\n");
#endif
		   i=0;
		   while(i<maxwindows && (tk_event.event.xexpose.window!=windows[i].titlebar ||windows[i].isUsed==False))
		     i++;
		   if(i>=0 && i<maxwindows)
		   {
		     WIN_DrawTitle(i);
		     goto EXPOSE_END;
		   }
		   goto EXPOSE_END;
		 }

		 else if(win_type==BORDER_BOX)
		 {
		   i=0;
		   while(tk_event.event.xexpose.window!=windows[i].topleftbox &&tk_event.event.xexpose.window!=windows[i].toprightbox &&tk_event.event.xexpose.window!=windows[i].bottomleftbox &&tk_event.event.xexpose.window!=windows[i].bottomrightbox &&tk_event.event.xexpose.window!=windows[i].rightbar &&tk_event.event.xexpose.window!=windows[i].leftbar &&tk_event.event.xexpose.window!=windows[i].topbar &&tk_event.event.xexpose.window!=windows[i].bottombar && i>=0 && i<maxwindows)
		     i++;
		   if(i>=0 && i<maxwindows&&windows[i].isUsed==True)
		   {
		     WIN_DrawBorder(tk_event.event.xexpose.window,i);
		     goto EXPOSE_END;
		   }
		   goto EXPOSE_END;
		 }


		 else if(win_type==ICON_WINDOW)
		 {
		   i=0;
		   while(tk_event.event.xexpose.window!=windows[i].icon.window&&tk_event.event.xexpose.window!=windows[i].icon.draw_area&&tk_event.event.xexpose.window!=windows[i].icon.title && i>=0 && i<maxwindows)
		     i++;
		   if(i>=0 && i<maxwindows)
		   {
		     if(tk_event.event.xexpose.window==windows[i].icon.draw_area&&windows[i].icon.pixmap!=0)
		       WIN_DrawIconPixmap(i);
		     else if(tk_event.event.xexpose.window==windows[i].icon.draw_area&&windows[i].icon.pixmap==0)
		     {
		       if(wm_info.dsk_defaulticon==1)
		       {
		 	 /*XClearWindow(tk_display->display,windows[i].icon.draw_area);*/
     			 XSetBackground(tk_display->display,icon_gc,tk_display->icn_colors.icn_draw_bg);
     			 XSetForeground(tk_display->display,icon_gc,tk_display->icn_colors.icn_draw);
     			 XCopyPlane(tk_display->display,pix_motifs[11],windows[i].icon.draw_area,icon_gc,0,0,64,64,0,0,1);
		       }
		     }
		     else if(tk_event.event.xexpose.window==windows[i].icon.title)
		       WIN_DrawIconTitle(i);
			    
		     else if(tk_event.event.xexpose.window==windows[i].icon.window||tk_event.event.xexpose.window==windows[i].icon.title)
		       WIN_DrawIconBorder(i);
    		   
		     goto EXPOSE_END;
		   }
		 }

		  if(tk_event.event.xexpose.window==wm_kill_window)
		  {
		    KILL_Draw();
		    goto EXPOSE_END;
		  }

		 
		  else if(tk_event.event.xexpose.window==wm_about_window)
		  { 
		    AB_Draw();
		    goto EXPOSE_END;
		  }


		  else if(tk_event.event.xexpose.window==wm_setup_window)
		  {
		    SET_Draw();
		    goto EXPOSE_END;
		  }
	
										
		  else if(tk_event.event.xexpose.window==desktop_motif_window||tk_event.event.xexpose.window==desktop_defaulticon_window||tk_event.event.xexpose.window==desktop_screensaver_window||tk_event.event.xexpose.window==desktop_paper_window)
		    DSK_Draw();
 
		  else if(tk_event.event.xexpose.window==color_desktop)
		    CL_DrawDesktop(-1);
		  else if(tk_event.event.xexpose.window==color_window)
		    CL_DrawWindow(-1);
		  else if(tk_event.event.xexpose.window==color_dialog)
		    CL_DrawDialog(-1);
		  else if(tk_event.event.xexpose.window==color_icon)
		    CL_DrawIcon(-1);
		  else if(tk_event.event.xexpose.window==color_button)
		    CL_DrawButton(-1);
		  else if(tk_event.event.xexpose.window==color_scroll)
		    CL_DrawScroll(-1);
		  else if(tk_event.event.xexpose.window==color_edit)
		    CL_DrawEdit(-1);
		  else if(tk_event.event.xexpose.window==color_list)
		    CL_DrawList(-1);
		  else if(tk_event.event.xexpose.window==color_menu)
		    CL_DrawMenu(-1);
		 
		  else if(tk_event.event.xexpose.window==wm_clipboard_window)
		    CLIP_Draw();
		  else if(tk_event.event.xexpose.window==wm_end_window)
		    END_Draw(); 
		  goto EXPOSE_END;
		}
		else if(tk_event.event.xexpose.window==wm_clipboard_window)
		   CLIP_Draw();

		EXPOSE_END:
	  	XSync(tk_display->display,False);
		break;



	case ButtonPress:

			      if(tk_event.event.xbutton.window==wm_main_window)
			      {
			         END_OF_WM:
				  WM_EndOfSession();
			      }

			      if(wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction)
			      {

				i=0;
				while(i<maxwindows && tk_event.event.xbutton.window!=windows[i].titlebar&&tk_event.event.xbutton.window!=windows[i].mainwindow&&tk_event.event.xbutton.window!=windows[i].clientwindow)
				  i++;
 				if(i<maxwindows&&windows[i].isUsed==True&&windows[i].state.isMapped==True&&windows[i].state.isFrozen==False&&wm_action.type==NoAction)
				{
				   if(windows[i].state.isZoomed==False && (windows[i].attributes&Unmoveable)!=Unmoveable&&tk_event.event.xbutton.button==Button1)
				   {
					wm_action.window=tk_event.event.xbutton.window;
					wm_action.type=MoveAction;
					wm_action.number=i; 
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xbutton.x,tk_event.event.xbutton.y,&tc_x,&tc_y,&tc_child);
					wm_action.window=tk_event.event.xbutton.window;
					wm_action.gap_x=tc_x-windows[i].normal.x;
					wm_action.gap_y=tc_y-windows[i].normal.y;
					wm_action.x=wm_action.start_x=windows[i].normal.x;
					wm_action.y=wm_action.start_y=windows[i].normal.y;
					wm_action.width=wm_action.start_width=windows[i].normal.width+windows[i].normal.border;
					wm_action.height=wm_action.start_height=windows[i].normal.height+windows[i].normal.border;
					if(i!=wm_setup_index&&i!=wm_desktop_index&&i!=wm_process_index&&i!=wm_about_index&&i!=wm_colors_index)
					  WM_UnmapSystemWindows();
					XGrabServer(tk_display->display);
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					goto BUTTON_PRESS_END;						
				   }
				   else if(windows[i].state.isZoomed==False && ((windows[i].attributes&Unmoveable)==Unmoveable||tk_event.event.xbutton.button!=Button1)&&windows[i].state.isOnTop==False)
				   {
					if(i!=wm_setup_index&&i!=wm_desktop_index&&i!=wm_process_index&&i!=wm_about_index&&i!=wm_colors_index)
				          WM_UnmapSystemWindows();
					wm_action.type=NoAction;
					WIN_MapRaised(i);
					goto BUTTON_PRESS_END;					
				   }
				   else if(windows[i].state.isZoomed==True && (windows[i].attributes&MoveableWhenZoomed)==MoveableWhenZoomed&&tk_event.event.xbutton.button==Button1)
				   {
					wm_action.type=NoAction;
#ifdef DEBUG
					fprintf(stderr,"Fenetre zoomee \n"); 
#endif
					if(i!=wm_setup_index&&i!=wm_desktop_index&&i!=wm_process_index&&i!=wm_about_index&&i!=wm_colors_index)
					  WM_UnmapSystemWindows();

					wm_action.window=tk_event.event.xbutton.window;
					wm_action.type=MoveZoomAction;
					wm_action.number=i; 
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xbutton.x,tk_event.event.xbutton.y,&tc_x,&tc_y,&tc_child);
					wm_action.window=tk_event.event.xbutton.window;
					wm_action.gap_x=tc_x-windows[i].zoom.x;
					wm_action.gap_y=tc_y-windows[i].zoom.y;
					wm_action.x=wm_action.start_x=windows[i].zoom.x;
					wm_action.y=wm_action.start_y=windows[i].zoom.y;
					wm_action.width=wm_action.start_width=windows[i].zoom.width+windows[i].zoom.border;
					wm_action.height=wm_action.start_height=windows[i].zoom.height+windows[i].zoom.border;
					 
					XGrabServer(tk_display->display);
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					goto BUTTON_PRESS_END;
				   }
				   else if(windows[i].state.isZoomed==True&&((/*tk_event.event.xbutton.button!=Button1&&*/windows[i].state.isOnTop==False)||((windows[i].attributes&MoveableWhenZoomed)==MoveableWhenZoomed&&tk_event.event.xbutton.button!=Button1)))
				   {
					wm_action.type=NoAction;
					if(i!=wm_setup_index&&i!=wm_desktop_index&&i!=wm_process_index&&i!=wm_about_index&&i!=wm_colors_index)
					  WM_UnmapSystemWindows();
					WIN_MapRaised(i);
				   }
				   goto BUTTON_PRESS_END;
				}
				else if(i<maxwindows&& windows[i].isUsed==True&&windows[i].state.isMapped==True&&windows[i].state.isOnTop==False&& windows[i].state.isFrozen==True&&wm_action.type==NoAction)
				{	
					if(i!=wm_setup_index&&i!=wm_desktop_index&&i!=wm_process_index&&i!=wm_about_index&&i!=wm_colors_index)
					  WM_UnmapSystemWindows();
					wm_action.type=NoAction;
					WIN_MapRaised(i);
					goto BUTTON_PRESS_END;
				}
				else if(i<maxwindows && windows[i].isUsed==False)
				{
				  XUnmapWindow(tk_display->display,windows[i].mainwindow);
				  WM_VerifyWindows();
				  goto BUTTON_PRESS_END;
				}

				i=0;
				while(tk_event.event.xbutton.window!=windows[i].topleftbox &&tk_event.event.xbutton.window!=windows[i].toprightbox &&tk_event.event.xbutton.window!=windows[i].bottomleftbox &&tk_event.event.xbutton.window!=windows[i].bottomrightbox &&tk_event.event.xbutton.window!=windows[i].rightbar &&tk_event.event.xbutton.window!=windows[i].leftbar &&tk_event.event.xbutton.window!=windows[i].topbar &&tk_event.event.xbutton.window!=windows[i].bottombar &&i<maxwindows)
				  i++;
				if(i<maxwindows&&windows[i].isUsed==True&&windows[i].state.isFrozen==False&&wm_action.type==NoAction&&tk_event.event.xbutton.button==Button1)
				{
					wm_action.window=tk_event.event.xbutton.window;
					
					if(wm_action.window==windows[i].topleftbox)
					  wm_action.type=ResizeByTopLeftAction;
					else if(wm_action.window==windows[i].toprightbox)
					  wm_action.type=ResizeByTopRightAction;
					else if(wm_action.window==windows[i].bottomleftbox)
					  wm_action.type=ResizeByBottomLeftAction;
					else if(wm_action.window==windows[i].bottomrightbox)
					  wm_action.type=ResizeByBottomRightAction;
					else if(wm_action.window==windows[i].topbar)
					  wm_action.type=ResizeByTopAction;
					else if(wm_action.window==windows[i].bottombar)
					  wm_action.type=ResizeByBottomAction;
					else if(wm_action.window==windows[i].leftbar)
					  wm_action.type=ResizeByLeftAction;
					else if(wm_action.window==windows[i].rightbar)
					  wm_action.type=ResizeByRightAction;

					wm_action.number=i; 
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xbutton.x,tk_event.event.xbutton.y,&tc_x,&tc_y,&tc_child);
					wm_action.window=tk_event.event.xbutton.window;
					wm_action.gap_x=tc_x-windows[i].normal.x;
					wm_action.gap_y=tc_y-windows[i].normal.y;
					wm_action.gap_width=windows[i].normal.width-tc_x+windows[i].normal.x;
					wm_action.gap_height=windows[i].normal.height-tc_y+windows[i].normal.y;
					wm_action.x=wm_action.start_x=windows[i].normal.x;
					wm_action.y=wm_action.start_y=windows[i].normal.y;
					wm_action.width=wm_action.start_width=windows[i].normal.width+windows[i].normal.border;
					wm_action.height=wm_action.start_height=windows[i].normal.height+windows[i].normal.border;
					if(i!=wm_setup_index&&i!=wm_desktop_index&&i!=wm_process_index&&i!=wm_about_index&&i!=wm_colors_index)
					  WM_UnmapSystemWindows();
					XGrabServer(tk_display->display);
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
				  	goto BUTTON_PRESS_END;
				 }
				 else if(i<maxwindows && windows[i].isUsed==True&&windows[i].state.isMapped==True&&windows[i].state.isOnTop==False&&(windows[i].state.isFrozen==True||tk_event.event.xbutton.button!=Button1)&&wm_action.type==NoAction)
				 {	
					wm_action.type=NoAction;
					if(i!=wm_setup_index&&i!=wm_desktop_index&&i!=wm_process_index&&i!=wm_about_index&&i!=wm_colors_index)
					  WM_UnmapSystemWindows();
					WIN_MapRaised(i);
					
					goto BUTTON_PRESS_END;
				 }
				 else if(i<maxwindows && windows[i].isUsed==False)
				 {
				   XUnmapWindow(tk_display->display,windows[i].mainwindow);
				   WM_VerifyWindows();
				   goto BUTTON_PRESS_END;
				 }


				 i=0;
				 while(tk_event.event.xbutton.window!=windows[i].icon.window &&tk_event.event.xbutton.window!=windows[i].icon.draw_area &&tk_event.event.xbutton.window!=windows[i].icon.clientwindow &&tk_event.event.xbutton.window!=windows[i].icon.title&&i<maxwindows)
				   i++;
				 if(i<maxwindows&&windows[i].state.isFrozen==False&&windows[i].isUsed==True&&wm_action.type==NoAction&&tk_event.event.xbutton.button==Button1)
				 {
					wm_action.type=MoveIconAction;
					wm_action.number=i;
#ifdef DEBUG
					fprintf(stderr,"icon.width:%d  icon.height:%d\n",windows[i].icon.width,windows[i].icon.height);
#endif
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xbutton.x,tk_event.event.xbutton.y,&tc_x,&tc_y,&tc_child);
					wm_action.window=tk_event.event.xbutton.window;
					wm_action.gap_x=tc_x-windows[i].icon.x;
					wm_action.gap_y=tc_y-windows[i].icon.y;
					wm_action.gap_width=windows[i].icon.width-tc_x+windows[i].icon.x;
					wm_action.gap_height=windows[i].icon.height-tc_y+windows[i].icon.y;
					wm_action.x=wm_action.start_x=windows[i].icon.x;
					wm_action.y=wm_action.start_y=windows[i].icon.y;
					wm_action.width=wm_action.start_width=windows[i].icon.width;
					wm_action.height=wm_action.start_height=windows[i].icon.height;
#ifdef DEBUG
					fprintf(stderr,"icon.width:%d  icon.height:%d\n",windows[i].icon.width,windows[i].icon.height);
#endif
					XGrabServer(tk_display->display);
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width+1,wm_action.height+1);

				  	goto BUTTON_PRESS_END;
				 }
				 else if(i<maxwindows&&windows[i].state.isFrozen==False&&windows[i].isUsed==True&&wm_action.type==NoAction&&tk_event.event.xbutton.button!=Button1)
				 {
					wm_action.type=NoAction;
					wm_action.number=0;
					ICN_MapRaised(i);

				  	goto BUTTON_PRESS_END;
				 }
				 else if(i<maxwindows && windows[i].isUsed==False)
				 {
				   XUnmapWindow(tk_display->display,windows[i].mainwindow);
				   WM_VerifyWindows();
				   goto BUTTON_PRESS_END;
				 }


				 else if(i>=maxwindows&&wm_action.type==NoAction) 
				 { 
#ifdef DEBUG
					fprintf(stderr,"Button Press: cas inconnu\n");
#endif					
					win_type=WM_GetWindowClass(tk_event.event.xbutton.window);
					if(win_type!=BORDER_BOX&&win_type!=ICON_WINDOW&&win_type!=TITLE_BAR&&win_type!=REPARENTING_WINDOW&&win_type!=REPARENTING_DIALOG) 
					  XMapRaised(tk_display->display,tk_event.event.xbutton.window);
					goto BUTTON_PRESS_END;
				 }


				BUTTON_PRESS_END:
#ifdef DEBUG
				fprintf(stderr,"Button press action:%d\n",wm_action.type);
#endif
				break;
			      }
			      XSync(tk_display->display,False);
			      break;



	case ButtonRelease:

	      		      if(wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction)
			      {
#ifdef DEBUG
				fprintf(stderr,"Button Release Action=%d  button=%d\n",wm_action.type,tk_event.event.xbutton.button);
#endif
				if(wm_action.type==MoveAction&&tk_event.event.xbutton.button==Button1)
				{
					wm_action.type=NoAction;
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
					windows[wm_action.number].normal.x=wm_action.x;
					windows[wm_action.number].normal.y=wm_action.y;
					XMoveWindow(tk_display->display,windows[wm_action.number].mainwindow,wm_action.x,wm_action.y);
					XUngrabServer(tk_display->display);
					i=wm_action.number;
					
					if(windows[wm_action.number].state.isOnTop==False) 
					  WIN_MapRaised(wm_action.number);	
					else WIN_Map(wm_action.number);
					wm_action.number=-1;
					XUngrabServer(tk_display->display);

		    	  		send_event.type=ConfigureNotify;
					send_event.xconfigure.type=ConfigureNotify;
		  	  		send_event.xconfigure.send_event=True;
		  	  		send_event.xconfigure.window=windows[i].clientwindow;
		  	  		send_event.xconfigure.event=windows[i].clientwindow;
		  	  		send_event.xconfigure.above=None;
		  	  		send_event.xconfigure.override_redirect=False;
		  	  		send_event.xconfigure.display=tk_display->display;

		  	  		if(windows[i].state.isZoomed==False)
		  	  		{
		  	  		  	send_event.xconfigure.x=windows[i].normal.x+windows[i].normal.add_leftside;
		  	  		  	send_event.xconfigure.y=windows[i].normal.y+windows[i].normal.add_top;		  	
		  	  		  	send_event.xconfigure.width=windows[i].normal.client_width;
		  	  		  	send_event.xconfigure.height=windows[i].normal.client_height;
						send_event.xconfigure.border_width=windows[i].normal.border/2;
			  		}
			  		else if(windows[i].state.isZoomed==True)
			  		{
		      				send_event.xconfigure.x=windows[i].zoom.x+windows[i].zoom.add_leftside;
		      				send_event.xconfigure.y=windows[i].zoom.y+windows[i].zoom.add_top;		  	
		      				send_event.xconfigure.width=windows[i].zoom.client_width;
		      				send_event.xconfigure.height=windows[i].zoom.client_height;
						send_event.xconfigure.border_width=windows[i].normal.border/2;
		 	  		}
					if(windows[i].clientwindow!=wm_kill_window&&windows[i].clientwindow!=wm_desktop_window&&windows[i].clientwindow!=wm_colors_window&&windows[i].clientwindow!=wm_process_window&&windows[i].clientwindow!=wm_about_window&&windows[i].clientwindow!=wm_setup_window&&windows[i].clientwindow!=wm_end_window && windows[i].clientwindow!=wm_clipboard_window)
		    	  		  XSendEvent(tk_display->display,windows[i].clientwindow,False,StructureNotifyMask,&send_event);

				}
				else if(wm_action.type==MoveZoomAction&&tk_event.event.xbutton.button==Button1)
				{
					wm_action.type=NoAction;
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
#ifdef DEBUG
					fprintf(stderr,"Move Zoom window  number=%d\n",wm_action.number);
#endif	
					windows[wm_action.number].zoom.x=wm_action.x;
					windows[wm_action.number].zoom.y=wm_action.y;
					XMoveWindow(tk_display->display,windows[wm_action.number].mainwindow,wm_action.x,wm_action.y);
					XUngrabServer(tk_display->display);
					
					i=wm_action.number;

					if(windows[i].state.isOnTop==False)
					  WIN_MapRaised(wm_action.number);	
					else WIN_Map(i);	
				
					wm_action.number=-1;
					XUngrabServer(tk_display->display);
		    	  		send_event.type=ConfigureNotify;
					send_event.xconfigure.type=ConfigureNotify;
		  	  		send_event.xconfigure.send_event=True;
		  	  		send_event.xconfigure.event=windows[i].clientwindow;
		  	  		send_event.xconfigure.above=None;
		  	  		send_event.xconfigure.override_redirect=False;
		  	  		send_event.xconfigure.display=tk_display->display;
		  	  		send_event.xconfigure.window=windows[i].clientwindow;
		      			send_event.xconfigure.x=windows[i].zoom.x+windows[i].zoom.add_leftside;
		      			send_event.xconfigure.y=windows[i].zoom.y+windows[i].normal.add_top;		  	
		      			send_event.xconfigure.width=windows[i].zoom.client_width;
		      			send_event.xconfigure.height=windows[i].zoom.client_height;
					send_event.xconfigure.border_width=windows[i].normal.border/2;
		 	  		
					if(windows[i].clientwindow!=wm_kill_window&&windows[i].clientwindow!=wm_desktop_window&&windows[i].clientwindow!=wm_colors_window&&windows[i].clientwindow!=wm_process_window&&windows[i].clientwindow!=wm_about_window&&windows[i].clientwindow!=wm_setup_window&&windows[i].clientwindow!=wm_end_window && windows[i].clientwindow!=wm_clipboard_window)
		    	  		  XSendEvent(tk_display->display,windows[i].clientwindow,False,StructureNotifyMask,&send_event);

				}

				else if(wm_action.type==ResizeByBottomRightAction||wm_action.type==ResizeByBottomLeftAction||wm_action.type==ResizeByTopRightAction||wm_action.type==ResizeByTopLeftAction||wm_action.type==ResizeByRightAction||wm_action.type==ResizeByBottomAction||wm_action.type==ResizeByLeftAction||wm_action.type==ResizeByTopAction&&tk_event.event.xbutton.button==Button1)
				{
					wm_action.type=NoAction;
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
					windows[wm_action.number].normal.width=wm_action.width-windows[wm_action.number].normal.border;
					windows[wm_action.number].normal.height=wm_action.height-windows[wm_action.number].normal.border;
					windows[wm_action.number].normal.x=wm_action.x;
					windows[wm_action.number].normal.y=wm_action.y;
					
					XUngrabServer(tk_display->display);
					XMoveResizeWindow(tk_display->display,windows[wm_action.number].mainwindow,wm_action.x,wm_action.y,wm_action.width-windows[wm_action.number].normal.border,wm_action.height-windows[wm_action.number].normal.border);
					WIN_Resize(wm_action.number,wm_action.width-windows[wm_action.number].normal.border,wm_action.height-windows[wm_action.number].normal.border);

					i=wm_action.number; 

					if(windows[wm_action.number].state.isOnTop==False) 
					  WIN_MapRaised(wm_action.number);	
					wm_action.number=-1;
					XUngrabServer(tk_display->display);

		    	  		send_event.type=ConfigureNotify;
					send_event.xconfigure.type=ConfigureNotify;
		  	  		send_event.xconfigure.send_event=True;
		  	  		send_event.xconfigure.event=windows[i].clientwindow;
		  	  		send_event.xconfigure.above=None;
		  	  		send_event.xconfigure.override_redirect=False;
		  	  		send_event.xconfigure.display=tk_display->display;
		  	  		send_event.xconfigure.window=windows[i].clientwindow;
		      			send_event.xconfigure.x=windows[i].normal.x+windows[i].normal.add_leftside;
		      			send_event.xconfigure.y=windows[i].normal.y+windows[i].normal.add_top;		  	
		      			send_event.xconfigure.width=windows[i].normal.client_width;
		      			send_event.xconfigure.height=windows[i].normal.client_height;
					send_event.xconfigure.border_width=windows[i].normal.border/2;
		 	  		
					if(windows[i].clientwindow!=wm_kill_window&&windows[i].clientwindow!=wm_desktop_window&&windows[i].clientwindow!=wm_colors_window&&windows[i].clientwindow!=wm_process_window&&windows[i].clientwindow!=wm_about_window&&windows[i].clientwindow!=wm_setup_window&&windows[i].clientwindow!=wm_end_window && windows[i].clientwindow!=wm_clipboard_window)
		    	  		  XSendEvent(tk_display->display,windows[i].clientwindow,False,StructureNotifyMask,&send_event);
					
				}
				else if(wm_action.type==MoveIconAction&&tk_event.event.xbutton.button==Button1) 	
				{
					wm_action.type=NoAction;
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width+1,wm_action.height+1);
					
					windows[wm_action.number].icon.x=wm_action.x;
					windows[wm_action.number].icon.y=wm_action.y;
					XMoveWindow(tk_display->display,windows[wm_action.number].icon.window,wm_action.x,wm_action.y);
					XUngrabServer(tk_display->display);
					/*fprintf(stderr,"x:%d y:%d move_x:%d move_y:%d\n",windows[i].x,windows[i].y,wm_action.x,wm_action.y);*/
					
					ICN_MapRaised(wm_action.number);
					ptr=0;
					while(ptr<tk_display->widget_double_click) ptr++;			 
			 
			 		if(XCheckTypedWindowEvent(tk_display->display,tk_event.event.xbutton.window,ButtonPress,&eventbis)==True && eventbis.xbutton.button==Button1&&windows[wm_action.number].state.isFrozen==False&&(windows[wm_action.number].attributes&Iconic)!=Iconic)
     		  			{   
					  i=wm_action.number;
					  if(i!=wm_setup_index&&i!=wm_desktop_index&&i!=wm_process_index&&i!=wm_about_index&&i!=wm_colors_index)
					    WM_UnmapSystemWindows();
					  WIN_Uniconify(i);
					  /**** VERIFIER LE 'i' ... ****/
					}
					wm_action.number=-1;
				}
				else if(wm_action.type==NoAction&&tk_event.event.xbutton.window==RootWindow(tk_display->display,tk_display->screen)) 
				{
					ptr=0;
					while(ptr<tk_display->widget_double_click) ptr++;			 
			 
			 		if(XCheckTypedWindowEvent(tk_display->display,RootWindow(tk_display->display,tk_display->screen),ButtonPress,&eventbis)==True)
     		  			{   
					  if(eventbis.xbutton.button==Button1&&windows[wm_process_index].state.isMapped==False)
					  {
						/*fprintf(stderr,"Requete pour la fenetre de processus\n");
						WM_UnmapSystemWindows();
						PID_GetMainWindows();
						tk_event.event.type=MapRequest;
						tk_event.event.xmaprequest.window=wm_process_window;
						goto MAP_REQUEST_BEGIN;*/
						PID_Map();
					  }
					  else if(eventbis.xbutton.button==Button2&&windows[wm_desktop_index].state.isMapped==False)
		  			  {   
						/*fprintf(stderr,"Requete pour la fenetre de bureau\n");*/
						WM_UnmapSystemWindows();
						tk_event.event.type=MapRequest;
						tk_event.event.xmaprequest.window=wm_desktop_window;
						goto MAP_REQUEST_BEGIN;
		  			  }
					  else if(eventbis.xbutton.button==Button3&&windows[wm_setup_index].state.isMapped==False)
		  			  {   
						/*fprintf(stderr,"Requete pour la fenetre de setup\n");*/
						WM_UnmapSystemWindows();
						tk_event.event.type=MapRequest;
						tk_event.event.xmaprequest.window=wm_setup_window;
						goto MAP_REQUEST_BEGIN;
		  			  }
					
					}
					else XUngrabServer(tk_display->display);
				}
			      }
			      XSync(tk_display->display,False);
			      break;


	case MotionNotify:

			      if(wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction)
			      {
				
				if(wm_action.type==MoveAction&&(tk_event.event.xmotion.state&Button1Mask)==Button1Mask)
				{
					
					send_event.xmotion.x=tk_event.event.xmotion.x;
				 	send_event.xmotion.y=tk_event.event.xmotion.y;
		 			send_event.xmotion.state=tk_event.event.xmotion.state;

		 			while(XCheckWindowEvent(tk_display->display,wm_action.window,Button1MotionMask,&eventbis)==True)
					  send_event=eventbis;

			 		tk_event.event.xmotion.x=send_event.xmotion.x;
		 	 		tk_event.event.xmotion.y=send_event.xmotion.y;
		 	 		tk_event.event.xmotion.state=send_event.xmotion.state;

					
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xmotion.x,tk_event.event.xmotion.y,&tc_x,&tc_y,&tc_child);
					wm_action.x=tc_x-wm_action.gap_x;
					wm_action.y=tc_y-wm_action.gap_y;
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
				}
				else if(wm_action.type==MoveZoomAction&&(tk_event.event.xmotion.state&Button1Mask)==Button1Mask)
				{
					
					send_event.xmotion.x=tk_event.event.xmotion.x;
				 	send_event.xmotion.y=tk_event.event.xmotion.y;
		 			send_event.xmotion.state=tk_event.event.xmotion.state;

		 			while(XCheckWindowEvent(tk_display->display,wm_action.window,Button1MotionMask,&eventbis)==True)
					  send_event=eventbis;

			 		tk_event.event.xmotion.x=send_event.xmotion.x;
		 	 		tk_event.event.xmotion.y=send_event.xmotion.y;
		 	 		tk_event.event.xmotion.state=send_event.xmotion.state;

					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xmotion.x,tk_event.event.xmotion.y,&tc_x,&tc_y,&tc_child);
					wm_action.x=tc_x-wm_action.gap_x;
					wm_action.y=tc_y-wm_action.gap_y;
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
				}
				else if(wm_action.type==ResizeByBottomRightAction&&(tk_event.event.xmotion.state&Button1Mask)==Button1Mask)
				{
					send_event.xmotion.x=tk_event.event.xmotion.x;
				 	send_event.xmotion.y=tk_event.event.xmotion.y;
		 			send_event.xmotion.state=tk_event.event.xmotion.state;

		 			while(XCheckWindowEvent(tk_display->display,wm_action.window,Button1MotionMask,&eventbis)==True)
					  send_event=eventbis;

			 		tk_event.event.xmotion.x=send_event.xmotion.x;
		 	 		tk_event.event.xmotion.y=send_event.xmotion.y;
		 	 		tk_event.event.xmotion.state=send_event.xmotion.state;
			 					 
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xmotion.x,tk_event.event.xmotion.y,&tc_x,&tc_y,&tc_child);
					wm_action.width=tc_x-wm_action.gap_x+wm_action.start_width-wm_action.start_x;
					wm_action.height=tc_y-wm_action.gap_y+wm_action.start_height-wm_action.start_y;

					if(wm_action.width>windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border)
					   wm_action.width=windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border;
					if(wm_action.width<windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border)
					   wm_action.width=windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border;
					if(wm_action.height>windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border)
					   wm_action.height=windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border;
					if(wm_action.height<windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border)
					   wm_action.height=windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border;

					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
				}

				else if(wm_action.type==ResizeByBottomLeftAction&&(tk_event.event.xmotion.state&Button1Mask)==Button1Mask)
				{
					send_event.xmotion.x=tk_event.event.xmotion.x;
				 	send_event.xmotion.y=tk_event.event.xmotion.y;
		 			send_event.xmotion.state=tk_event.event.xmotion.state;

		 			while(XCheckWindowEvent(tk_display->display,wm_action.window,Button1MotionMask,&eventbis)==True)
					  send_event=eventbis;

			 		tk_event.event.xmotion.x=send_event.xmotion.x;
		 	 		tk_event.event.xmotion.y=send_event.xmotion.y;
		 	 		tk_event.event.xmotion.state=send_event.xmotion.state;
			 					 
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xmotion.x,tk_event.event.xmotion.y,&tc_x,&tc_y,&tc_child);
					wm_action.x=tc_x-wm_action.gap_x;
					wm_action.width=wm_action.start_width+wm_action.start_x-tc_x+wm_action.gap_x;
					wm_action.height=tc_y-wm_action.gap_y+wm_action.start_height-wm_action.start_y;

					if(wm_action.width>windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border)
					{  wm_action.width=windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border;
					   wm_action.x=wm_action.start_x+wm_action.start_width-wm_action.width;
					}
					if(wm_action.width<windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border)
					{  wm_action.width=windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border;
					   wm_action.x=wm_action.start_x+wm_action.start_width-wm_action.width;
					}
					if(wm_action.height>windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border)
					   wm_action.height=windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border;
					if(wm_action.height<windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border)
					   wm_action.height=windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border;
					
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
				}

				else if(wm_action.type==ResizeByTopLeftAction&&(tk_event.event.xmotion.state&Button1Mask)==Button1Mask)
				{
					send_event.xmotion.x=tk_event.event.xmotion.x;
				 	send_event.xmotion.y=tk_event.event.xmotion.y;
		 			send_event.xmotion.state=tk_event.event.xmotion.state;

		 			while(XCheckWindowEvent(tk_display->display,wm_action.window,Button1MotionMask,&eventbis)==True)
					  send_event=eventbis;

			 		tk_event.event.xmotion.x=send_event.xmotion.x;
		 	 		tk_event.event.xmotion.y=send_event.xmotion.y;
		 	 		tk_event.event.xmotion.state=send_event.xmotion.state;
			 					 
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xmotion.x,tk_event.event.xmotion.y,&tc_x,&tc_y,&tc_child);
					wm_action.x=tc_x-wm_action.gap_x;
					wm_action.y=tc_y-wm_action.gap_y;
					wm_action.width=wm_action.start_width+wm_action.start_x-tc_x+wm_action.gap_x;
					wm_action.height=wm_action.gap_y+wm_action.start_height+wm_action.start_y-tc_y;

					if(wm_action.width>windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border)
					{  wm_action.width=windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border;
					   wm_action.x=wm_action.start_x+wm_action.start_width-wm_action.width;
					}
					if(wm_action.width<windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border)
					{  wm_action.width=windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border;
					   wm_action.x=wm_action.start_x+wm_action.start_width-wm_action.width;
					}
					if(wm_action.height>windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border)
					{  wm_action.height=windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border;
					   wm_action.y=wm_action.start_y+wm_action.start_height-wm_action.height;
					}
					if(wm_action.height<windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border)
					{  wm_action.height=windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border;
					   wm_action.y=wm_action.start_y+wm_action.start_height-wm_action.height;
					}
			
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
				}

				else if(wm_action.type==ResizeByTopRightAction&&(tk_event.event.xmotion.state&Button1Mask)==Button1Mask)
				{
					send_event.xmotion.x=tk_event.event.xmotion.x;
				 	send_event.xmotion.y=tk_event.event.xmotion.y;
		 			send_event.xmotion.state=tk_event.event.xmotion.state;

		 			while(XCheckWindowEvent(tk_display->display,wm_action.window,Button1MotionMask,&eventbis)==True)
					  send_event=eventbis;

			 		tk_event.event.xmotion.x=send_event.xmotion.x;
		 	 		tk_event.event.xmotion.y=send_event.xmotion.y;
		 	 		tk_event.event.xmotion.state=send_event.xmotion.state;
			 					 
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xmotion.x,tk_event.event.xmotion.y,&tc_x,&tc_y,&tc_child);
					wm_action.y=tc_y-wm_action.gap_y;
					wm_action.width=tc_x+wm_action.start_width-wm_action.gap_x-wm_action.start_x;
					wm_action.height=wm_action.gap_y+wm_action.start_height+wm_action.start_y-tc_y;

					if(wm_action.width>windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border)
					   wm_action.width=windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border;
					if(wm_action.width<windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border)
					   wm_action.width=windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border;
					if(wm_action.height>windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border)
					{  wm_action.height=windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border;
					   wm_action.y=wm_action.start_y+wm_action.start_height-wm_action.height;
					}
					if(wm_action.height<windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border)
					{  wm_action.height=windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border;
					   wm_action.y=wm_action.start_y+wm_action.start_height-wm_action.height;
					}
					
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
				}

				else if(wm_action.type==ResizeByTopAction&&(tk_event.event.xmotion.state&Button1Mask)==Button1Mask)
				{
					send_event.xmotion.x=tk_event.event.xmotion.x;
				 	send_event.xmotion.y=tk_event.event.xmotion.y;
		 			send_event.xmotion.state=tk_event.event.xmotion.state;

		 			while(XCheckWindowEvent(tk_display->display,wm_action.window,Button1MotionMask,&eventbis)==True)
					  send_event=eventbis;

			 		tk_event.event.xmotion.x=send_event.xmotion.x;
		 	 		tk_event.event.xmotion.y=send_event.xmotion.y;
		 	 		tk_event.event.xmotion.state=send_event.xmotion.state;
			 					 
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xmotion.x,tk_event.event.xmotion.y,&tc_x,&tc_y,&tc_child);
					wm_action.y=tc_y-wm_action.gap_y;
					wm_action.height=wm_action.gap_y+wm_action.start_height+wm_action.start_y-tc_y;
					
					if(wm_action.height>windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border)
					{  wm_action.height=windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border;
					   wm_action.y=wm_action.start_y+wm_action.start_height-wm_action.height;
					}
					if(wm_action.height<windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border)
					{  wm_action.height=windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border;
					   wm_action.y=wm_action.start_y+wm_action.start_height-wm_action.height;
					}
			
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
				}

				else if(wm_action.type==ResizeByBottomAction&&(tk_event.event.xmotion.state&Button1Mask)==Button1Mask)
				{
					send_event.xmotion.x=tk_event.event.xmotion.x;
				 	send_event.xmotion.y=tk_event.event.xmotion.y;
		 			send_event.xmotion.state=tk_event.event.xmotion.state;

		 			while(XCheckWindowEvent(tk_display->display,wm_action.window,Button1MotionMask,&eventbis)==True)
					  send_event=eventbis;

			 		tk_event.event.xmotion.x=send_event.xmotion.x;
		 	 		tk_event.event.xmotion.y=send_event.xmotion.y;
		 	 		tk_event.event.xmotion.state=send_event.xmotion.state;
			 					 
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xmotion.x,tk_event.event.xmotion.y,&tc_x,&tc_y,&tc_child);
					/*wm_action.height=tc_y+wm_action.gap_y-wm_action.start_height-wm_action.start_y;*/
					wm_action.height=tc_y-wm_action.gap_y+wm_action.start_height-wm_action.start_y;

					if(wm_action.height>windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border)
					   wm_action.height=windows[wm_action.number].normal.max_height+windows[wm_action.number].normal.border;
					if(wm_action.height<windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border)
					   wm_action.height=windows[wm_action.number].normal.min_height+windows[wm_action.number].normal.border;
					
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
				}

				else if(wm_action.type==ResizeByRightAction&&(tk_event.event.xmotion.state&Button1Mask)==Button1Mask)
				{
					send_event.xmotion.x=tk_event.event.xmotion.x;
				 	send_event.xmotion.y=tk_event.event.xmotion.y;
		 			send_event.xmotion.state=tk_event.event.xmotion.state;

		 			while(XCheckWindowEvent(tk_display->display,wm_action.window,Button1MotionMask,&eventbis)==True)
					  send_event=eventbis;

			 		tk_event.event.xmotion.x=send_event.xmotion.x;
		 	 		tk_event.event.xmotion.y=send_event.xmotion.y;
		 	 		tk_event.event.xmotion.state=send_event.xmotion.state;
			 					 
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
			
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xmotion.x,tk_event.event.xmotion.y,&tc_x,&tc_y,&tc_child);
					wm_action.width=tc_x+wm_action.start_width-wm_action.gap_x-wm_action.start_x;
					
					if(wm_action.width>windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border)
					   wm_action.width=windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border;
					if(wm_action.width<windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border)
					   wm_action.width=windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border;
										
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
				}

				else if(wm_action.type==ResizeByLeftAction&&(tk_event.event.xmotion.state&Button1Mask)==Button1Mask)
				{
					send_event.xmotion.x=tk_event.event.xmotion.x;
				 	send_event.xmotion.y=tk_event.event.xmotion.y;
		 			send_event.xmotion.state=tk_event.event.xmotion.state;

		 			while(XCheckWindowEvent(tk_display->display,wm_action.window,Button1MotionMask,&eventbis)==True)
					  send_event=eventbis;

			 		tk_event.event.xmotion.x=send_event.xmotion.x;
		 	 		tk_event.event.xmotion.y=send_event.xmotion.y;
		 	 		tk_event.event.xmotion.state=send_event.xmotion.state;
			 					 
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
					
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xmotion.x,tk_event.event.xmotion.y,&tc_x,&tc_y,&tc_child);
					wm_action.x=tc_x-wm_action.gap_x;
					wm_action.width=wm_action.start_width+wm_action.start_x-tc_x+wm_action.gap_x;
			
					if(wm_action.width>windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border)
					{  wm_action.width=windows[wm_action.number].normal.max_width+windows[wm_action.number].normal.border;
					   wm_action.x=wm_action.start_x+wm_action.start_width-wm_action.width;
					}
					if(wm_action.width<windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border)
					{  wm_action.width=windows[wm_action.number].normal.min_width+windows[wm_action.number].normal.border;
					   wm_action.x=wm_action.start_x+wm_action.start_width-wm_action.width;
					}
					
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width-1,wm_action.height-1);
				}
				if(wm_action.type==MoveIconAction&&(tk_event.event.xmotion.state&Button1Mask)==Button1Mask)
				{
					send_event.xmotion.x=tk_event.event.xmotion.x;
				 	send_event.xmotion.y=tk_event.event.xmotion.y;
		 			send_event.xmotion.state=tk_event.event.xmotion.state;

		 			while(XCheckWindowEvent(tk_display->display,wm_action.window,Button1MotionMask,&eventbis)==True)
					  send_event=eventbis;

			 		tk_event.event.xmotion.x=send_event.xmotion.x;
		 	 		tk_event.event.xmotion.y=send_event.xmotion.y;
		 	 		tk_event.event.xmotion.state=send_event.xmotion.state;

					
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width+1,wm_action.height+1);
					
					XTranslateCoordinates(tk_display->display,tk_event.event.xbutton.window,RootWindow(tk_display->display,tk_display->screen),tk_event.event.xmotion.x,tk_event.event.xmotion.y,&tc_x,&tc_y,&tc_child);
					wm_action.x=tc_x-wm_action.gap_x;
					wm_action.y=tc_y-wm_action.gap_y;
					
					XDrawRectangle(tk_display->display,RootWindow(tk_display->display,tk_display->screen),grab_gc,wm_action.x,wm_action.y,wm_action.width+1,wm_action.height+1);
				}
			      }
			      XSync(tk_display->display,False);
			      break;


			
	case KeyPress:
				
				win_type=-1;
				win_type=WM_GetWindowClass(tk_event.event.xkey.window);

				KEY_SUITE :

				if(XLookupKeysym(&tk_event.event.xkey,0)==XK_End&&(tk_event.event.xkey.state&(ControlMask|Mod1Mask))==(ControlMask|Mod1Mask))
				{
				  if(windows[wm_process_index].state.isMapped==True)
				   END_Map(wm_process_window);
				  else END_Map(tk_event.event.xkey.window);
				}
				else if(tk_event.event.xkey.window==wm_clipboard_window && XLookupKeysym(&tk_event.event.xkey,0)!=XK_Escape)
				  CLIP_Events();

#ifdef DEBUG
				else if(XLookupKeysym(&tk_event.event.xkey,0)==XK_End)
				  fprintf(stderr,"END recu: code:%d window:%ld  root:%ld  subwindow:%ld\n",tk_event.event.xkey.keycode,tk_event.event.xkey.window,tk_event.event.xkey.root,tk_event.event.xkey.subwindow);  
#endif				
				else if(win_type!=-1&&XLookupKeysym(&tk_event.event.xkey,0)==XK_Delete&&(tk_event.event.xkey.state&ControlMask)==ControlMask&&(tk_event.event.xkey.state&Mod1Mask)==Mod1Mask)
				{
#ifdef DEBUG
				 fprintf(stderr,"CTRL+ALT+DEL enclenche: code:%d window:%ld  root:%ld  subwindow:%ld\n",tk_event.event.xkey.keycode,tk_event.event.xkey.window,tk_event.event.xkey.root,tk_event.event.xkey.subwindow);
#endif	
				 XGetInputFocus(tk_display->display,&tc_child,&ret);
				 i=0;
				 while(windows[i].clientwindow!=wm_kill_window&&i<maxwindows)
				   i++;
				 if(i<maxwindows&&tc_child!=wm_kill_window/*&&tc_child!=bn_kill_ok.window&&tc_child!=bn_kill_cancel.window*/)
				   KILL_Map(tk_event.event.xkey.window);
				}

				else if(XLookupKeysym(&tk_event.event.xkey,0)==XK_Escape&&(tk_event.event.xkey.state&Mod1Mask)==Mod1Mask)
				{
#ifdef DEBUG
				  fprintf(stderr,"ALT+ESCAPE enclenche: code:%d window:%ld  root:%ld  subwindow:%ld\n",tk_event.event.xkey.keycode,tk_event.event.xkey.window,tk_event.event.xkey.root,tk_event.event.xkey.subwindow);
#endif
				  if(windows[wm_process_index].state.isMapped==False)
				    WM_UnmapSystemWindows();				 
				  tk_event.event.type=MapRequest;
				  tk_event.event.xmaprequest.window=windows[wm_desktop_index].clientwindow;
				  windows[wm_desktop_index].state.isMapped=True;
				  wid_GiveFocus(tk_display,bn_desktop_cancel);
				  goto MAP_REQUEST_BEGIN;
				}

				else if(win_type!=-1&&XLookupKeysym(&tk_event.event.xkey,0)==XK_Escape&&(tk_event.event.xkey.state&ControlMask)==ControlMask)
				{
#ifdef DEBUG
				  fprintf(stderr,"CTRL+ESCAPE enclenche: code:%d window:%ld  root:%ld  subwindow:%ld\n",tk_event.event.xkey.keycode,tk_event.event.xkey.window,tk_event.event.xkey.root,tk_event.event.xkey.subwindow);
#endif
				  /*if(windows[wm_process_index].state.isMapped==False)
				    WM_UnmapSystemWindows();
				  PID_GetMainWindows();
				  tk_event.event.type=MapRequest;
				  tk_event.event.xmaprequest.window=wm_process_window;*/
				  PID_Map();
				 
				  goto MAP_REQUEST_BEGIN;
				}

				else if(win_type!=-1&&XLookupKeysym(&tk_event.event.xkey,0)==XK_Escape&&(tk_event.event.xkey.state&ShiftMask)==ShiftMask)
				{
#ifdef DEBUG
				  fprintf(stderr,"SHIFT+ESCAPE enclenche: code:%d window:%ld  root:%ld  subwindow:%ld\n",tk_event.event.xkey.keycode,tk_event.event.xkey.window,tk_event.event.xkey.root,tk_event.event.xkey.subwindow);
#endif	
				  if(windows[wm_process_index].state.isMapped==False)
				    WM_UnmapSystemWindows();
				  tk_event.event.type=MapRequest;
				  tk_event.event.xmaprequest.window=wm_setup_window;
				  windows[wm_setup_index].state.isMapped=True;
				  goto MAP_REQUEST_BEGIN;
				}

				else if(win_type!=-1&&tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Break))
				{
#ifdef DEBUG
				  fprintf(stderr,"BREAK enclenchee: code:%d window:%ld  root:%ld  subwindow:%ld\n",tk_event.event.xkey.keycode,tk_event.event.xkey.window,tk_event.event.xkey.root,tk_event.event.xkey.subwindow);
#endif
				}

				else if(win_type!=-1&&XLookupKeysym(&tk_event.event.xkey,0)==XK_Print)
				{
				PRINT_SCREEN_START :
#ifdef DEBUG
				  fprintf(stderr,"PrintScreen demande: code:%d window:%ld  root:%ld  subwindow:%ld\n",tk_event.event.xkey.keycode,tk_event.event.xkey.window,tk_event.event.xkey.root,tk_event.event.xkey.subwindow);
#endif
				  if(tk_event.event.xkey.state==0)
				    CLIP_PrintScreen();
				  else if((tk_event.event.xkey.state&ControlMask)==ControlMask&&(tk_event.event.xkey.state&Mod1Mask)!=Mod1Mask&&(tk_event.event.xkey.state&ShiftMask)!=ShiftMask)
				    CLIP_PrintWindow(WIN_GetNumber(tk_event.event.xkey.window));
				  else if((tk_event.event.xkey.state&ShiftMask)==ShiftMask&&(tk_event.event.xkey.state&Mod1Mask)!=Mod1Mask&&(tk_event.event.xkey.state&ControlMask)!=ControlMask)
				    CLIP_PrintClientWindow(WIN_GetNumber(tk_event.event.xkey.window));
				  else if(((tk_event.event.xkey.state&(ControlMask|ShiftMask))==(ControlMask|ShiftMask) || (tk_event.event.xkey.state&(ControlMask|Mod1Mask))==(ControlMask|Mod1Mask)) && wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction)
				  {
#ifdef DEBUG
				    fprintf(stderr,"Presse papier demande\n");
#endif
				    WM_UnmapSystemWindows();
				    if(windows[wm_clipboard_index].state.isIconic==False)
				    {
				     	windows[wm_clipboard_index].state.isInitialized=True; 
		  		    	windows[wm_clipboard_index].state.isMapped=True;
				    	WIN_MapRaised(wm_clipboard_index);
				    }
				    else WIN_Uniconify(wm_clipboard_index);
				  }
				}
				else if(XLookupKeysym(&tk_event.event.xkey,0)==XK_Print)
				{
				 /*fprintf(stderr,"PrintScreen BIS demande: code:%d window:%ld  root:%ld  subwindow:%ld\n",tk_event.event.xkey.keycode,tk_event.event.xkey.window,tk_event.event.xkey.root,tk_event.event.xkey.subwindow);*/
				  if(tk_event.event.xkey.state==0)
				    CLIP_PrintScreen();
				  else if(((tk_event.event.xkey.state&(ControlMask|ShiftMask))==(ControlMask|ShiftMask) || (tk_event.event.xkey.state&(ControlMask|Mod1Mask))==(ControlMask|Mod1Mask)) && wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction)
				  {
#ifdef DEBUG
				    fprintf(stderr,"Presse papier demande\n");
#endif
				    WM_UnmapSystemWindows();
				    if(windows[wm_clipboard_index].state.isIconic==False)
				    {
				     	windows[wm_clipboard_index].state.isInitialized=True; 
		  		    	windows[wm_clipboard_index].state.isMapped=True;
				    	WIN_MapRaised(wm_clipboard_index);
				    }
				    else WIN_Uniconify(wm_clipboard_index);
				  }
				}

				XSync(tk_display->display,False);
				break;


	case KeyRelease :

			if(tk_event.event.xkey.window==RootWindow(tk_display->display,tk_display->screen))
			{
				  if(tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Escape)&&(tk_event.event.xkey.state&ControlMask)==ControlMask)
			  {
					/*WM_UnmapSystemWindows();
				 	PID_GetMainWindows();
				 	tk_event.event.type=MapRequest;
				 	tk_event.event.xmaprequest.window=wm_process_window;
				 	goto MAP_REQUEST_BEGIN;*/
					PID_Map();
			  }
			  else if(tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Escape)&&(tk_event.event.xkey.state&Mod1Mask)==Mod1Mask)
			  {
				 	/*fprintf(stderr,"ALT+ESCAPE enclenche: code:%d window:%ld  root:%ld  subwindow:%ld\n",tk_event.event.xkey.keycode,tk_event.event.xkey.window,tk_event.event.xkey.root,tk_event.event.xkey.subwindow);*/
					if(windows[wm_process_index].state.isMapped==False)
					  WM_UnmapSystemWindows();
				 	tk_event.event.type=MapRequest;
				 	tk_event.event.xmaprequest.window=windows[wm_desktop_index].clientwindow;
				 	windows[wm_desktop_index].state.isMapped=True;
				 	goto MAP_REQUEST_BEGIN;
			  }
			  else if(tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Escape)&&(tk_event.event.xkey.state&ShiftMask)==ShiftMask)
			  {
				  	/*fprintf(stderr,"SHIFT+ESCAPE enclenche: code:%d window:%ld  root:%ld  subwindow:%ld\n",tk_event.event.xkey.keycode,tk_event.event.xkey.window,tk_event.event.xkey.root,tk_event.event.xkey.subwindow);*/
				  	if(windows[wm_process_index].state.isMapped==False)
				  	  WM_UnmapSystemWindows();				  	
					tk_event.event.type=MapRequest;
				  	tk_event.event.xmaprequest.window=wm_setup_window;
				  	windows[wm_setup_index].state.isMapped=True;				 
				  	goto MAP_REQUEST_BEGIN;
			  }
			  else if(tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Menu))
			    goto PRINT_SCREEN_START;
				  
			}
			break;


	case SelectionClear  :
	case SelectionRequest:
	case SelectionNotify :  
			CLIP_Events();
	 		XSync(tk_display->display,False);
			break;
				

	case MappingNotify   :
	      		XSync(tk_display->display,False);
			XRefreshKeyboardMapping(&tk_event.event.xmapping);
	      		XSync(tk_display->display,False);
			break;





	default :

#ifdef HAS_SHAPE_EXTENSION_LIBRARY
			XShapeQueryExtension(tk_display->display,&shape_event,&i);
			if(tk_event.event.type==shape_event)
			{

				XSync(tk_display->display,False);
				XFlush(tk_display->display);
				send_event=tk_event.event;
				shapev=(XShapeEvent *)&tk_event.event;

		 		while(XCheckTypedWindowEvent(tk_display->display,shapev->window,shape_event,&eventbis)==True)
				{
				  XSync(tk_display->display,False);
				  XFlush(tk_display->display);
				  send_event=eventbis;
				}
			 	tk_event.event=send_event;

#ifdef DEBUG
				fprintf(stderr,"Shape Notify recu\n");
#endif
				shapev=(XShapeEvent *)&tk_event.event;
				i=WM_GetWindowNumber(shapev->window);
				if(i>=0&&shapev->kind==ShapeBounding&&shapev->shaped==False&&windows[i].state.isZoomed==False&&windows[i].identity.shape==True)
				{
	  			  XShapeCombineMask(tk_display->display,windows[i].mainwindow,ShapeClip,0,0,None,0);
	  			  XShapeCombineMask(tk_display->display,windows[i].mainwindow,ShapeBounding,0,0,None,0);
				  windows[i].identity.shape=False;
				  WIN_Resize(i,windows[i].normal.width,windows[i].normal.height);
				}
				else if(i>=0&&shapev->kind==ShapeBounding&&shapev->shaped==False&&windows[i].state.isZoomed==True&&windows[i].identity.shape==True)
				{
	  			  XShapeCombineMask(tk_display->display,windows[i].mainwindow,ShapeClip,0,0,None,0);
	  			  XShapeCombineMask(tk_display->display,windows[i].mainwindow,ShapeBounding,0,0,None,0);
				  windows[i].identity.shape=False;
				  WIN_Resize(i,windows[i].zoom.width,windows[i].zoom.height);				  
				}
				else if(i>=0&&shapev->kind==ShapeBounding&&shapev->shaped==True&&windows[i].state.isZoomed==False)
				{
	  			  rectangle.x=windows[i].normal.client_x;	  
	  			  rectangle.y=windows[i].normal.client_y;
	  			  rectangle.width=windows[i].normal.client_width;
	  			  rectangle.height=windows[i].normal.client_height;
	  			  /*XShapeCombineRectangles(tk_display->display,windows[i].mainwindow,ShapeBounding,0,0,&rectangle,1,ShapeSubtract,Unsorted);
	  			  XShapeCombineRectangles(tk_display->display,windows[i].mainwindow,ShapeClip,0,0,&rectangle,1,ShapeSubtract,Unsorted);
				  XShapeCombineShape(tk_display->display,windows[i].mainwindow,ShapeBounding,windows[i].normal.client_x,windows[i].normal.client_y,windows[i].clientwindow,ShapeBounding,ShapeUnion);
	   			  XShapeCombineShape(tk_display->display,windows[i].mainwindow,ShapeClip,windows[i].normal.client_x,windows[i].normal.client_y,windows[i].clientwindow,ShapeBounding,ShapeUnion);
				  */
				  windows[i].identity.shape=True;
				  WIN_Resize(i,windows[i].normal.width,windows[i].normal.height);
				}																			
				else if(i>=0&&shapev->kind==ShapeBounding&&shapev->shaped==True&&windows[i].state.isZoomed==True)
				{
	  			  rectangle.x=windows[i].zoom.client_x;	  
	  			  rectangle.y=windows[i].zoom.client_y;
	  			  rectangle.width=windows[i].zoom.client_width;
	  			  rectangle.height=windows[i].zoom.client_height;
	  			  /*XShapeCombineRectangles(tk_display->display,windows[i].mainwindow,ShapeBounding,0,0,&rectangle,1,ShapeSubtract,Unsorted);
	  			  XShapeCombineRectangles(tk_display->display,windows[i].mainwindow,ShapeClip,0,0,&rectangle,1,ShapeSubtract,Unsorted);
				  XShapeCombineShape(tk_display->display,windows[i].mainwindow,ShapeBounding,windows[i].zoom.client_x,windows[i].zoom.client_y,windows[i].clientwindow,ShapeBounding,ShapeUnion);
	   			  XShapeCombineShape(tk_display->display,windows[i].mainwindow,ShapeClip,windows[i].zoom.client_x,windows[i].zoom.client_y,windows[i].clientwindow,ShapeBounding,ShapeUnion);
				  */
				  windows[i].identity.shape=True;
				  WIN_Resize(i,windows[i].zoom.width,windows[i].zoom.height);				  
				}																			
	  			
			}
#endif
	      		XSync(tk_display->display,False);
			break;


		}


}






