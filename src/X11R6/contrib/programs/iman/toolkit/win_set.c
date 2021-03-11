/*
 *
 * 	win_set.c  
 * 	modification des fenetres
 *
 * 	Modification :  23/04/94
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
 *      IMAN Development Toolkit version 1.2
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */




#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <malloc.h>
#include <memory.h>

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"
#include "X11/Xatom.h"


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>





			  /***** Titre principal *****/


int win_SetTitleName(tk_display,window,title_name)
TkDisplay *tk_display;
Window window;
char *title_name;
{
 int ret;

 ret=XStoreName(tk_display->display,window,title_name);
 return ret;

}





			/*** Groupe et boite de dialogue ***/



int win_SetGroupLeader(tk_display,window,group_leader_ID)
TkDisplay *tk_display;
Window window, group_leader_ID;
{
 int ret;
 XWMHints *xwmhints;

#ifdef DEBUG
 fprintf(stderr,"GroupLeader bientot change\n");
#endif
 xwmhints=XGetWMHints(tk_display->display,window);
#ifdef DEBUG
 fprintf(stderr,"GroupLeader avance\n");
#endif


 if(xwmhints==NULL)
 {
#ifdef DEBUG
   fprintf(stderr,"xwmhints==NULL\n");
#endif
   xwmhints=(XWMHints *)malloc(sizeof(XWMHints));
   if(xwmhints==NULL)
   {
	fprintf(stderr,"Erreur d'allocation de memoire\n");
	tk_CloseSession(tk_display);
	exit(-1);
   }
   xwmhints->flags=0;
 }
 if((xwmhints->flags&WindowGroupHint)!=WindowGroupHint)
   xwmhints->flags=xwmhints->flags+WindowGroupHint;

 xwmhints->window_group=group_leader_ID;
 XSetWMHints(tk_display->display,window,xwmhints);
 XFree(xwmhints);
 return 0;

}





int win_SetTransientFor(tk_display,window,transient_leader_ID)
TkDisplay *tk_display;
Window window, transient_leader_ID;
{
 XSetTransientForHint(tk_display->display,window,transient_leader_ID);
 return 0;
}




int win_SetToplevel(tk_display,window,top_level)
TkDisplay *tk_display;
Window window, top_level;
{
 unsigned long ptr;


 ptr=top_level;
 XChangeProperty(tk_display->display,window,tk_display->atoms.WM_TOP_LEVEL,XA_DRAWABLE,32,PropModeReplace,(char *)&ptr,1);
 ptr=0;														
 XChangeProperty(tk_display->display,window,tk_display->atoms._IMAN_WM_FOCUS,XA_DRAWABLE,32,PropModeReplace,(char *)&ptr,1);

 return 0;
}






int win_Reparent(tk_display,window,parent,top_level,x,y)
TkDisplay *tk_display;
Window window, parent, top_level;
int x, y;
{

  XReparentWindow(tk_display->display,window,parent,x,y);
  return win_SetToplevel(tk_display,window,top_level);

}





				/*** Icone ***/


int win_SetIconWindow(tk_display,window,icon_window)
TkDisplay *tk_display;
Window window, icon_window;
{
 int ret;
 XWMHints *xwmhints;

 xwmhints=XGetWMHints(tk_display->display,window);

 if(xwmhints==NULL)
 {
   xwmhints=(XWMHints *)malloc(sizeof(XWMHints));
   if(xwmhints==NULL)
   {
	fprintf(stderr,"Erreur d'allocation de memoire\n");
	tk_CloseSession(tk_display);
	exit(-1);
   }
   xwmhints->flags=0;
 }
 if((xwmhints->flags&IconWindowHint)!=IconWindowHint)
   xwmhints->flags=xwmhints->flags+IconWindowHint;
 xwmhints->initial_state=IgnoreState;
 xwmhints->icon_window=icon_window;
 XSetWMHints(tk_display->display,window,xwmhints);
 XFree(xwmhints);
 return 0;
}




int win_SetIconPixmap(tk_display,window,icon_pixmap)
TkDisplay *tk_display;
Window window;
Pixmap icon_pixmap;
{
 int ret;
 XWMHints *xwmhints;

 xwmhints=XGetWMHints(tk_display->display,window);

 if(xwmhints==NULL)
 {
   /*fprintf(stderr,"Erreur: xwmhints==0\n");*/
   xwmhints=(XWMHints *)malloc(sizeof(XWMHints));
   if(xwmhints==NULL)
   {
	fprintf(stderr,"Erreur d'allocation de memoire\n");
	tk_CloseSession(tk_display);
	exit(-1);
   }
   xwmhints->flags=0;
 }
 if((xwmhints->flags&IconPixmapHint)!=IconPixmapHint)
   xwmhints->flags=xwmhints->flags+IconPixmapHint;
 xwmhints->initial_state=IgnoreState;
 xwmhints->icon_pixmap=icon_pixmap;
 XSetWMHints(tk_display->display,window,xwmhints);
 XFree(xwmhints);
 return 0;
}




int win_SetIconPixmapMask(tk_display,window,icon_pixmap_mask)
TkDisplay *tk_display;
Window window;
Pixmap icon_pixmap_mask;
{
 int ret;
 XWMHints *xwmhints;

 xwmhints=XGetWMHints(tk_display->display,window);

 if(xwmhints==NULL)
 {
   xwmhints=(XWMHints *)malloc(sizeof(XWMHints));
   if(xwmhints==NULL)
   {
	fprintf(stderr,"Erreur d'allocation de memoire\n");
	tk_CloseSession(tk_display);
	exit(-1);
   }
   xwmhints->flags=0;
 }
 if((xwmhints->flags&IconMaskHint)!=IconMaskHint)
   xwmhints->flags=xwmhints->flags+IconMaskHint;

 xwmhints->initial_state=IgnoreState;
 xwmhints->icon_mask=icon_pixmap_mask;
 XSetWMHints(tk_display->display,window,xwmhints);
 XFree(xwmhints);
 return 0;
}





int win_SetIconName(tk_display,window,icon_name)
TkDisplay *tk_display;
Window window;
char *icon_name;
{
 int ret;
 
 XSetIconName(tk_display->display,window,icon_name);
 return 0;
}





int win_Resize(tk_display,window,width,height)
TkDisplay *tk_display;
Window window;
unsigned int width, height;
{
 int ret;
 
 ret=XResizeWindow(tk_display->display,window,width,height);
 return ret;
}


 


int win_SetState(tk_display,window,state)
TkDisplay *tk_display;
Window window;
unsigned int state;
{
  int i;
  int ret;
  XEvent event;

  event.type=ClientMessage;
  event.xclient.type=ClientMessage;
  event.xclient.format=32;
  event.xclient.send_event=True;
  event.xclient.message_type=tk_display->atoms.WM_CHANGE_STATE;
  event.xclient.window=window;
  event.xclient.data.l[0]=state;
  event.xclient.display=tk_display->display;

/*  if(tk_GetConnection(tk_display)==True)
    XSendEvent(tk_display->display,tk_display->wm.main_window,False,0,&event);
  else*/ XSendEvent(tk_display->display,RootWindow(tk_display->display,tk_display->screen),False,SubstructureNotifyMask|SubstructureRedirectMask,&event);

  if(state==WithdrawnState)
  {
    win_Unmap(tk_display,window);
    event.type=UnmapNotify;
    event.xunmap.type=UnmapNotify;
    event.xunmap.send_event=True;
    event.xunmap.window=window;
    XSendEvent(tk_display->display,RootWindow(tk_display->display,tk_display->screen),False,SubstructureNotifyMask|SubstructureRedirectMask,&event);
  }

  return 0;
}




int win_Zoom(tk_display,window)
TkDisplay *tk_display;
Window window;
{
 return win_SetState(tk_display,window,ZoomState);
}




int win_Unzoom(tk_display,window)
TkDisplay *tk_display;
Window window;
{
 return win_SetState(tk_display,window,NormalState);
}





int win_Iconify(tk_display,window)
TkDisplay *tk_display;
Window window;
{
 return win_SetState(tk_display,window,IconicState);
}






int win_Uniconify(tk_display,window)
TkDisplay *tk_display;
Window window;
{
   win_MapRaised(tk_display,window);
}






int win_SetWMProtocols(tk_display,window,protocols,num)
TkDisplay *tk_display;
Window window;
Atom *protocols;
unsigned int num;
{
 int i;
 unsigned long prt;


 if(window>0 && num>0)
 {
   
   XDeleteProperty(tk_display->display,window,tk_display->atoms.WM_PROTOCOLS);
   for(i=0;i<num;i++)
   {
	prt=protocols[i];								
	XChangeProperty(tk_display->display,window,tk_display->atoms.WM_PROTOCOLS,XA_ATOM,32,PropModeAppend,(char *)&prt,1); 
   }
   return 0;
 }
 else return -1;

}






int win_SetHelpDatabook(tk_display,window,databook)
TkDisplay *tk_display;
Window window;
unsigned char *databook;
{

 if(window>0)
 {
   XDeleteProperty(tk_display->display,window,tk_display->atoms._IMAN_HS_DATABOOK);
   XChangeProperty(tk_display->display,window,tk_display->atoms._IMAN_HS_DATABOOK,XA_STRING,8,PropModeAppend,(char *)databook,strlen(databook)+1); 
   return 0;
 }
 else return -1;

}







int win_SetHelpTopic(tk_display,window,topic)
TkDisplay *tk_display;
Window window;
int topic;
{
 unsigned long prt;


 if(window>0)
 {
   prt=topic;
   XDeleteProperty(tk_display->display,window,tk_display->atoms._IMAN_HS_TOPIC);
   XChangeProperty(tk_display->display,window,tk_display->atoms._IMAN_HS_TOPIC,XA_INTEGER,32,PropModeAppend,(char *)&prt,1); 
   return 0;   
 }
 else return -1;

}






int win_RequestHelp(tk_display,window)
TkDisplay *tk_display;
Window window;
{
  XEvent send_event;
  unsigned long prt;


  if(window>0 && mIsHsActive(tk_display)==True)
  {
	send_event.type=ClientMessage;
	send_event.xclient.type=ClientMessage;
	send_event.xclient.send_event=True;
	send_event.xclient.message_type=tk_display->atoms._IMAN_HS_MESSAGES;
	send_event.xclient.window=window;
	send_event.xclient.format=32;
	send_event.xclient.data.l[0]=RequestingHelp;

	XSendEvent(tk_display->display,tk_display->hs.main_window,False,0,&send_event);
    	return 0;   
  }
  else return -1;

}






int win_SetZoomHints(tk_display,window,zhints)
TkDisplay *tk_display;
Window window;
XSizeHints *zhints;
{
 
  if(zhints==NULL)
    return -1;

  XSetZoomHints(tk_display->display,window,zhints);
  return 0;
}






int win_SetNormalHints(tk_display,window,nhints)
TkDisplay *tk_display;
Window window;
XSizeHints *nhints;
{
 
  if(nhints==NULL)
    return -1;

  XSetNormalHints(tk_display->display,window,nhints);
  return 0;
}








