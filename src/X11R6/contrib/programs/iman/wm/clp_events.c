/*
 *
 * 	clp_events.c
 * 	evenements du clipboard
 *
 * 	Modification :  18/11/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
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
#include <pwd.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>
#include <X11/xpm.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"










/*
 *
 * Boucle principale
 *
 *
 */

int CLIP_Events()
{
 XEvent send_event;
 Window root;
 XpmAttributes xpmattributes;

 unsigned long ptr;
 int mask;
 int ret;
 int i, j, k, x,y,w, h,depth,border;
 char *str;
 char file_name[150];
 Atom atoms[15];
 struct passwd *pw;
 char *sptr;

 switch(tk_event.ev_type)
 {

   case XLIBEVENT :


	switch(tk_event.event.type)
	{
	  case SelectionRequest:
#ifdef DEBUG		
	  	fprintf(stderr,"Selection request recue\n");
	  	if(tk_event.event.xselectionrequest.owner==wm_clipboard_window)
	  	  fprintf(stderr,"Owner: CLIPBOARD\n");
	  	else fprintf(stderr,"Owner: inconnu %ld\n",tk_event.event.xselectionrequest.owner);
	  	fprintf(stderr,"Requestor: %ld\n",tk_event.event.xselectionrequest.requestor);
	  	if(tk_event.event.xselectionrequest.selection==XA_CLIPBOARD)
	  	  fprintf(stderr,"Selection: XA_CLIPBOARD\n");
	  	else fprintf(stderr,"Selection: %d\n",tk_event.event.xselectionrequest.selection);
#endif
	  	send_event.type=SelectionNotify;
	  	send_event.xselection.type=SelectionNotify;
	  	send_event.xselection.requestor=tk_event.event.xselectionrequest.requestor;
	  	send_event.xselection.selection=tk_event.event.xselectionrequest.selection;
	  	send_event.xselection.target=tk_event.event.xselectionrequest.target;
	  	send_event.xselection.property=tk_event.event.xselectionrequest.property;
	  	send_event.xselection.time=tk_event.event.xselectionrequest.time;
	
		if(tk_event.event.xselectionrequest.target==XA_STRING&&tk_event.event.xselectionrequest.selection==XA_CLIPBOARD)
		{
#ifdef DEBUG
	    	  fprintf(stderr,"Target: XA_STRING\n");
#endif
	  	  str="IMAN 1.2";
	  	  XChangeProperty(tk_display->display,tk_event.event.xselectionrequest.requestor,tk_event.event.xselectionrequest.property,XA_STRING,8,PropModeReplace,str,strlen(str));
	  	  send_event.xselection.target=None;
	  	  XSendEvent(tk_display->display,tk_event.event.xselectionrequest.requestor,False,0,&send_event);
	  	  return 0;
	  	}
	  	else if(tk_event.event.xselectionrequest.target==XA_PIXMAP&&tk_event.event.xselectionrequest.selection==XA_CLIPBOARD)
 	  	{ 
#ifdef DEBUG
	  	  fprintf(stderr,"Target: XA_PIXMAP\n");
#endif
	  	  ptr=clip_info.pixmap;
	  	  XChangeProperty(tk_display->display,tk_event.event.xselectionrequest.requestor,tk_event.event.xselectionrequest.property,XA_PIXMAP,32,PropModeReplace,(char *)&ptr,1);
	  	  send_event.xselection.target=XA_PIXMAP;
	  	  XSendEvent(tk_display->display,tk_event.event.xselectionrequest.requestor,False,0,&send_event);
	  	  return 0;
	  	}
	  	else if(tk_event.event.xselectionrequest.target==XA_TARGETS)
 	  	{ 
#ifdef DEBUG
	  	  fprintf(stderr,"Target: XA_TARGETS\n");
#endif
	  	  atoms[0]=XA_TARGETS;
	  	  atoms[1]=XA_OWNER_OS;
	  	  atoms[2]=XA_FILE_NAME;
	  	  atoms[3]=XA_HOST_NAME;
	  	  atoms[4]=XA_USER;
	  	  atoms[5]=XA_PROCESS;
	  	  atoms[6]=XA_TASK;
	  	  atoms[7]=XA_CLIENT_WINDOW;
	  	  if(tk_event.event.xselectionrequest.selection==XA_CLIPBOARD&&clip_info.isOwner==True&&clip_info.target==XA_PIXMAP)
	  	  {
	  	    atoms[8]=XA_PIXMAP;
	  	    XChangeProperty(tk_display->display,tk_event.event.xselectionrequest.requestor,tk_event.event.xselectionrequest.property,XA_INTEGER,32,PropModeReplace,(char *)atoms,8);
	  	  }
	  	  else XChangeProperty(tk_display->display,tk_event.event.xselectionrequest.requestor,tk_event.event.xselectionrequest.property,XA_TARGETS,32,PropModeReplace,(char *)atoms,7);
	  	  
	  	  send_event.xselection.target=XA_TARGETS;
	  	  XSendEvent(tk_display->display,tk_event.event.xselectionrequest.requestor,False,0,&send_event);
	  	  return 0;
	  	}
	  	else if(tk_event.event.xselectionrequest.target==XA_OWNER_OS)
 	  	{ 
#ifdef DEBUG
	  	  fprintf(stderr,"Target: XA_OWNER_OS\n");
#endif
#ifdef DESQVIEW_X_SERVER
		  str="dos";
#else
	  	  str="unix";
#endif
	  	  XChangeProperty(tk_display->display,tk_event.event.xselectionrequest.requestor,tk_event.event.xselectionrequest.property,XA_STRING,8,PropModeReplace,str,strlen(str));
	  	  send_event.xselection.target=XA_OWNER_OS;
	  	  XSendEvent(tk_display->display,tk_event.event.xselectionrequest.requestor,False,0,&send_event);	    
	  	  return 0;
	  	}
	  	else if(tk_event.event.xselectionrequest.target==XA_FILE_NAME)
 	  	{ 
#ifdef DEBUG
	  	  fprintf(stderr,"Target: XA_FILE_NAME\n");
#endif
	  	  str="/usr/bin/iman";
	  	  XChangeProperty(tk_display->display,tk_event.event.xselectionrequest.requestor,tk_event.event.xselectionrequest.property,XA_STRING,8,PropModeReplace,str,strlen(str));
	  	  send_event.xselection.target=XA_FILE_NAME;
	  	  XSendEvent(tk_display->display,tk_event.event.xselectionrequest.requestor,False,0,&send_event);	    
	  	  return 0;
	  	}
	  	else if(tk_event.event.xselectionrequest.target==XA_HOST_NAME)
 	  	{ 
#ifdef DEBUG
	  	  fprintf(stderr,"Target: XA_HOST_NAME\n");
#endif
	  	  str=(char *)malloc(100);
	  	  gethostname(str,100);
	  	  XChangeProperty(tk_display->display,tk_event.event.xselectionrequest.requestor,tk_event.event.xselectionrequest.property,XA_STRING,8,PropModeReplace,str,strlen(str));
	  	  send_event.xselection.target=XA_HOST_NAME;
	  	  XSendEvent(tk_display->display,tk_event.event.xselectionrequest.requestor,False,0,&send_event);
	  	  free(str);
	  	  return 0;
	  	}
	  	else if(tk_event.event.xselectionrequest.target==XA_USER)
 	  	{ 
#ifdef DEBUG
	  	  fprintf(stderr,"Target: XA_USER\n");
#endif
	  	  pw=(struct passwd *)getpwuid(getuid());																				
#ifdef DESQVIEW_X_SERVER
		  sptr="Unknown";
#else
		  sptr=(char *)pw->pw_name;
#endif
		  XChangeProperty(tk_display->display,tk_event.event.xselectionrequest.requestor,tk_event.event.xselectionrequest.property,XA_STRING,8,PropModeReplace,sptr,strlen(sptr));
	  	  send_event.xselection.target=XA_USER;
	  	  XSendEvent(tk_display->display,tk_event.event.xselectionrequest.requestor,False,0,&send_event);
#ifndef DESQVIEW_X_SERVER
	  	  endpwent();   
#endif
	  	  return 0;
	  	}
	  	else if(tk_event.event.xselectionrequest.target==XA_PROCESS||tk_event.event.xselectionrequest.target==XA_TASK)
 	  	{ 
#ifdef DEBUG
	  	  fprintf(stderr,"Target: XA_PROCESS ou XA_TASK\n");
#endif
	  	  ptr=getpid();
	  	  XChangeProperty(tk_display->display,tk_event.event.xselectionrequest.requestor,tk_event.event.xselectionrequest.property,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
	  	  send_event.xselection.target=tk_event.event.xselectionrequest.target;
	  	  XSendEvent(tk_display->display,tk_event.event.xselectionrequest.requestor,False,0,&send_event);
	  	  return 0;
	  	}
	  	else if(tk_event.event.xselectionrequest.target==XA_CLIENT_WINDOW)
 	  	{ 
#ifdef DEBUG
	  	  fprintf(stderr,"Target: XA_CLIENT_WINDOW\n");
#endif
	  	  ptr=wm_clipboard_window;
	  	  XChangeProperty(tk_display->display,tk_event.event.xselectionrequest.requestor,tk_event.event.xselectionrequest.property,XA_WINDOW,32,PropModeReplace,(char *)&ptr,1);
	  	  send_event.xselection.target=tk_event.event.xselectionrequest.target;
	  	  XSendEvent(tk_display->display,tk_event.event.xselectionrequest.requestor,False,0,&send_event);
	  	  return 0;
	  	}
	  	else{ 
#ifdef DEBUG
	  	  fprintf(stderr,"Target inconnu: %d\n",tk_event.event.xselectionrequest.target);
#endif
	  	  send_event.xselection.target=None;
	  	  XSendEvent(tk_display->display,tk_event.event.xselectionrequest.requestor,False,0,&send_event);	    
	  	  return 0;
	  	}
		
		break;


	  case SelectionClear :
#ifdef DEBUG	
		fprintf(stderr,"Selection clear recue\n");
#endif
		if(tk_event.event.xselectionclear.selection==XA_CLIPBOARD&&clip_info.time<=tk_event.event.xselectionclear.time&&clip_info.isOwner==True)
	  	{
	  	  clip_info.time=tk_event.event.xselectionclear.time;
	  	  clip_info.isOwner=False;
	  	  clip_info.target=0;
	  	  if(clip_info.pixmap>0) XFreePixmap(tk_display->display,clip_info.pixmap);
	  	  clip_info.pixmap=0;
	  	  XClearWindow(tk_display->display,wm_clipboard_window);
	  	  clip_info.owner=XGetSelectionOwner(tk_display->display,XA_CLIPBOARD);
	  	  if(clip_info.owner!=None)
   	  	    XConvertSelection(tk_display->display,XA_CLIPBOARD,XA_PIXMAP,tk_display->atoms._IMAN_WM_DATA,wm_clipboard_window,clip_info.time);
	  	}
	  	return 0;
		break;	


	  case SelectionNotify:
#ifdef DEBUG	
	  	fprintf(stderr,"Selection notify recue\n");
#endif
	  	if(tk_event.event.xselection.selection==XA_CLIPBOARD&&clip_info.time<=tk_event.event.xselection.time&&clip_info.isOwner==False)
	  	{
	  	  if(clip_info.pixmap>0) XFreePixmap(tk_display->display,clip_info.pixmap);
	  	  clip_info.pixmap=0;
	  	  clip_info.depth=0;
	
		  if(tk_event.event.xselection.target==XA_PIXMAP)
		  {
#ifdef DEBUG
		   	fprintf(stderr,"Target: XA_PIXMAP\n");
#endif
			clip_info.pixmap=CLIP_GetSelectionPixmap(tk_event.event.xselection.requestor,tk_event.event.xselection.property);
			XClearWindow(tk_display->display,wm_clipboard_window);
			if(clip_info.pixmap>0)
			{
			  XGetGeometry(tk_display->display,clip_info.pixmap,&root,&x,&y,&w,&h,&border,&depth);
			  clip_info.depth=depth;
			  tk_event.event.type=Expose;
			  tk_event.event.xexpose.window=wm_clipboard_window;
			  tk_event.event.xexpose.x=0;
			  tk_event.event.xexpose.y=0;
			  tk_event.event.xexpose.width=w;
			  tk_event.event.xexpose.height=h;
			  if(windows[wm_clipboard_index].state.isMapped==True) CLIP_Draw();
			}
		  }
#ifdef DEBUG
		  else if(tk_event.event.xselection.target==XA_STRING)
		    fprintf(stderr,"Target: XA_STRING\n");
		  else fprintf(stderr,"Target: %ld\n",tk_event.event.xselection.target);
#endif
		  clip_info.time=tk_event.event.xselection.time;
		  clip_info.isOwner=False;
		  clip_info.target=tk_event.event.xselection.target;
		}
		else if(tk_event.event.xselection.selection!=XA_CLIPBOARD&&clip_info.time<=tk_event.event.xselection.time&&clip_info.isOwner==False)
		{
#ifdef DEBUG
		  fprintf(stderr,"Selection : %ld\n",tk_event.event.xselection.selection);
#endif
		}
		return 0;
		break;



	  case KeyPress :
	  case KeyRelease :
	 
#ifdef DEBUG
		fprintf(stderr,"CLIP: keypress recu\n");		
#endif
		if((tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_W)||tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_w))&&(tk_event.event.xkey.state&Mod1Mask)==Mod1Mask)
		{
		  if(clip_info.isOwner==True&&clip_info.target==XA_PIXMAP)
		  {
#ifdef DEBUG
		    fprintf(stderr,"Ecriture de fichier %d requise\n",clip_info.write_number);
#endif
		    sprintf(file_name,"%s/clip%d.xpm\0","/usr/lib/iman/clipboard",clip_info.write_number);
		    ret=XpmWriteFileFromPixmap(tk_display->display,file_name,clip_info.pixmap,None,NULL);
		    if(ret!=XpmSuccess)
		    { 
			fprintf(stderr,"Iman : I/O error %d\n",ret);
			return 0;
		    }
		    clip_info.write_number++;
		    return 0;
		  }
#ifdef DEBUG
		  else fprintf(stderr,"Aucune image a sauvegarder !\n");
#endif
		}
		return 0;
		break;

	}
	return 0;
	break;


   default :
	break;

 }

 return 0;
}




