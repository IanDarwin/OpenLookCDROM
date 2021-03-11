/*
 *
 * 	wm_set.c
 * 	modifications diverses
 *
 * 	Modification :  16/01/94
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

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"
#include "X11/Xatom.h"


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "iman.h"

/*
#define DEBUG
*/





int WIN_GiveFocus(number)
unsigned int number;
{
 XWindowAttributes xwa;
 Window focuswindow;
 Atom gp_actual_type;

 int gp_actual_format;
 unsigned long gp_nitems=0, gp_bytes_after;
 unsigned long *gp_prop;
 int ret;


 if(number>=0&&number<maxwindows)
 {
   															
   ret=XGetWindowProperty(tk_display->display, windows[number].clientwindow,tk_display->atoms._IMAN_WM_FOCUS,0,1, False, AnyPropertyType, &gp_actual_type, &gp_actual_format, & gp_nitems, &gp_bytes_after,(unsigned char **)&gp_prop);

   if(gp_nitems>0&&ret==Success)
   {
	focuswindow=(Window)(gp_prop[0]);
	memset(&xwa,0,sizeof(XWindowAttributes));					
 	ret=XGetWindowAttributes(tk_display->display,focuswindow,&xwa);
	if(ret!=0 && xwa.map_state==IsViewable) ret=XSetInputFocus(tk_display->display,focuswindow,RevertToPointerRoot,CurrentTime);
	else XSetInputFocus(tk_display->display,windows[number].clientwindow,RevertToPointerRoot,CurrentTime);
	XFree(gp_prop);
	XSync(tk_display->display,False);
 	return 0;

   }
   else 
   {
  	XSetInputFocus(tk_display->display,windows[number].clientwindow,RevertToPointerRoot,CurrentTime);
	XSync(tk_display->display,False);
	return 0;
   }
 }
 return -1;

}




int WIN_SetClientType(number, type)
unsigned int number;
long type;
{
 long ptr;
 int ret;

 if(number>=0&&number<maxwindows)
 {
   ptr=type;
   ret=0;
   ret=XChangeProperty(tk_display->display, windows[number].clientwindow, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeReplace,(char *)&ptr,1);
   if (ret==BadAlloc||ret==BadAtom||ret==BadMatch||ret==BadValue||ret==BadWindow)
     return -1;
   else return 0;
 }
 else return -1;
}




int WIN_SetColormap(number)
unsigned int number;
{
 long ptr;
 int ret;

 if(number>=0&&number<maxwindows)
 {
   if(tk_event.event.xcolormap.new==True)
   {
	/*fprintf(stderr,"Colormap new  %ld  new:%d  state:%d\n",tk_event.event.xcolormap.colormap,tk_event.event.xcolormap.new,tk_event.event.xcolormap.state);*/

	if(tk_event.event.xcolormap.colormap!=None)
	  windows[number].identity.colormap=tk_event.event.xcolormap.colormap;
	else
	{
	  /*fprintf(stderr," colormap=none\n");*/
	  windows[number].identity.colormap=default_colormap;
	}
	if(WM_GetOnTop()==number)
	  XInstallColormap(tk_display->display,windows[number].identity.colormap);
	else if(WM_GetOnTop()>=0) XInstallColormap(tk_display->display,windows[WM_GetOnTop()].identity.colormap);
	return 0;
   }
   else
   {
	/*fprintf(stderr,"Colormap installed or not  %ld new:%d  state:%d\n",tk_event.event.xcolormap.colormap,tk_event.event.xcolormap.new,tk_event.event.xcolormap.state);*/

	if(tk_event.event.xcolormap.colormap==None)
	{
	  /*fprintf(stderr," colormap=none 2\n");*/
	  windows[number].identity.colormap=default_colormap;
	  if(WM_GetOnTop()==number)
	    XInstallColormap(tk_display->display,windows[number].identity.colormap);
	  return 0;
	}
	else
	{
	  if(tk_event.event.xcolormap.state==ColormapInstalled && WM_GetOnTop()!=number)
	  {
	    if(WM_GetOnTop()>=0 && windows[WM_GetOnTop()].identity.colormap==tk_event.event.xcolormap.colormap)
		return 0;
	    XUninstallColormap(tk_display->display,windows[number].identity.colormap);
	    return 0;
	  }
	  else if(tk_event.event.xcolormap.state==ColormapInstalled && WM_GetOnTop()==number && windows[number].identity.colormap!=tk_event.event.xcolormap.colormap)
	  { 
		/*windows[number].identity.colormap=tk_event.event.xcolormap.colormap;*/	
		XUninstallColormap(tk_display->display,tk_event.event.xcolormap.colormap);
		XInstallColormap(tk_display->display,windows[number].identity.colormap);
	  }
	  else if(tk_event.event.xcolormap.state==ColormapUninstalled && WM_GetOnTop()==number)
	  {
	    if(windows[number].identity.colormap==tk_event.event.xcolormap.colormap)
	      XInstallColormap(tk_display->display,windows[number].identity.colormap);
	    return 0;
	  }

	  
	}
	return 0;
   }
 }
 else return -1;
}




int WIN_SetClientAttributes(number, attributes)
unsigned int number;
long attributes;
{
 long ptr;
 int ret=0;

 if(number>=0&&number<maxwindows)
 {
  ret=0;
  ptr=windows[number].class;
  XDeleteProperty(tk_display->display,windows[number].clientwindow,tk_display->atoms._IMAN_WM_TYPE);
  ret=XChangeProperty(tk_display->display, windows[number].clientwindow, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeReplace,(char *)&ptr,1);
  ptr=attributes;
  ret=XChangeProperty(tk_display->display, windows[number].clientwindow, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeAppend,(char *)&ptr,1);
  if (ret==BadAlloc||ret==BadAtom||ret==BadMatch||ret==BadValue||ret==BadWindow)
    return -1;
  else return 0;
 }
}




int WIN_SetClientState(number, state)
unsigned int number;
long state;
{
 unsigned long ptr[2];
 int ret=0;

 if(number>=0&&number<maxwindows)
 {
  /*fprintf(stderr,"Set State commence %d\n",state);*/
  ptr[0]=state;
  if(windows[number].icon.hasIconWindow==False) ptr[1]=windows[number].icon.draw_area;
  else ptr[1]=0;

  ret=XChangeProperty(tk_display->display,windows[number].clientwindow, tk_display->atoms.WM_STATE, XA_INTEGER,32, PropModeReplace,(char *)ptr,2);
  /*fprintf(stderr,"Set State fini %d\n",ret);*/
  
  if (ret==BadAlloc||ret==BadAtom||ret==BadMatch||ret==BadValue||ret==BadWindow)
    return -1;
  else return 0;
 }
 else return -1;
}





void WIN_SetTitleName(number)
int number;
{
 WidgetAttributes wid_attributes;
 int i,j, ret;

 if(number>=0 && number<maxwindows)
 {
   j=wid_GetPosition(tk_display,ls_process_win);
   i=number;
   if(windows[i].hasStoredName==True) XFree(windows[i].title_name);
   ret=XFetchName(tk_display->display,windows[i].clientwindow,&windows[i].title_name);
   if(ret==0||strlen(windows[i].title_name)==0||windows[i].title_name==NULL)
   { 
	windows[i].hasStoredName=False; 
	windows[i].title_name="(Pas de nom)";
   }
   else windows[i].hasStoredName=True;
   XClearWindow(tk_display->display,windows[i].titlebar);
   if(windows[number].state.isMapped==True) WIN_DrawTitle(i);
   PID_GetMainWindows();
   wid_attributes.mask=SAPosition;
   wid_attributes.position=j;
   wid_SetAttributes(tk_display,ls_process_win,&wid_attributes,False);
  
 }
}





void WIN_SetIconName(number)
int number;
{
 int i,j, ret;

 if(number>=0 && number<maxwindows)
 {
  i=number;
  if(windows[i].icon.hasStoredIconName==True) XFree(windows[i].icon.name);
  ret=XGetIconName(tk_display->display,windows[i].clientwindow,&windows[i].icon.name);
  if(ret==0||strlen(windows[i].icon.name)==0||windows[i].title_name==NULL)
  {
	windows[i].icon.hasStoredIconName=False; 
	windows[i].icon.name=windows[i].title_name;
  }
  else windows[i].icon.hasStoredIconName=True;
  XClearWindow(tk_display->display,windows[i].icon.title);
  if(windows[i].state.isIconic==True&&wm_info.set_icontitle==True&&WIN_VerifyTree(number)==0)
    WIN_DrawIconTitle(i);
 }
}






void WIN_SetIconPixmap(number,pixmap,delay)
int number;
Pixmap pixmap;
Bool delay;
{
 Window root;
 int x,y,border;

 /*fprintf(stderr,"Set iconpixmap\n");*/
 if(number>=0 && number<maxwindows && windows[number].isUsed==True&&windows[number].icon.pixmap!=pixmap)
 {
  /*fprintf(stderr,"Modification de l'ICON PIXMAP\n");*/
  windows[number].icon.pixmap=pixmap;
  XGetGeometry(tk_display->display,pixmap,&root,&x,&y,&windows[number].icon.pix_width,&windows[number].icon.pix_height,&border,&windows[number].icon.depth);

  if(windows[number].state.isIconic==True&&wm_info.set_icons==True&&delay==False)
  {
    XClearWindow(tk_display->display,windows[number].icon.draw_area);
    WIN_DrawIconPixmap(number);
  }
 }
}




void WIN_SetIconPixmapMask(number,pixmap,delay)
int number;
Pixmap pixmap;
Bool delay;
{
 if(number>=0 && number<maxwindows && windows[number].isUsed==True&&windows[number].icon.mask!=pixmap)
 {
  /*fprintf(stderr,"Modification de l'ICON PIXMAP mask\n");*/
  windows[number].icon.mask=pixmap;

  if(windows[number].state.isIconic==True&&wm_info.set_icons==True&&delay==False)
  {
    XClearWindow(tk_display->display,windows[number].icon.draw_area);
    WIN_DrawIconPixmap(number);
  }
 }
}





void WIN_SetIconWindow(number,window)
int number;
Window window;
{
 int i, j, mask, ret;
 unsigned long ptr;
 XWindowChanges xwc;


 /*fprintf(stderr,"Set iconwindow\n");*/
 if(number>=0 && number<maxwindows && windows[number].isUsed==True&&windows[number].icon.clientwindow!=window)
 switch(windows[number].class)
 {

   case TOP_LEVEL_WINDOW :

   	/*fprintf(stderr,"Modification de l'ICON window\n");*/

   	j=0;
   	while(windows[j].clientwindow!=window&&j<maxwindows)
   	  j++;
   	if(j<maxwindows)
   	{
   	  /*fprintf(stderr,"Deja reparentee\n");*/
   	  WM_ReInitWindow(j);
   	}
	
	
	   if(windows[number].icon.hasIconWindow==True)
	     XUnmapWindow(tk_display->display,windows[number].icon.clientwindow);
	
	   XReparentWindow(tk_display->display,window,windows[number].icon.draw_area,0,0);
	   XSelectInput(tk_display->display,window,ResizeRedirectMask);
	   XAddToSaveSet(tk_display->display,window);
	   xwc.border_width=0;
	   xwc.width=ICON_DRAW_AREA_WIDTH;
	   xwc.height=ICON_DRAW_AREA_HEIGHT;
	   mask=CWBorderWidth|CWWidth|CWHeight;
	   XConfigureWindow(tk_display->display,window,mask,&xwc);
	   windows[number].icon.hasIconWindow=True;
	   windows[number].icon.clientwindow=window;
	   ptr=ICON_WINDOW;
	   ret=XChangeProperty(tk_display->display,windows[number].icon.clientwindow, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeReplace,(char *)&ptr,1);
	
   
   	if(windows[number].state.isIconic==True&&wm_info.set_icons==True)
   	  XMapRaised(tk_display->display,window);
	
	if(window==0)
	  windows[number].icon.hasIconWindow=False;
	break;
	
 }

}








void WIN_SetWMHints(number)
int number;
{
 WidgetAttributes wid_attributes;
 XWMHints *xwmhints;
 int flags;

 if(number>=0 && number<maxwindows)
 {
   xwmhints=XGetWMHints(tk_display->display,windows[number].clientwindow);
   if(xwmhints!=NULL)
   {
     flags=xwmhints->flags;
#ifdef DEBUG
     fprintf(stderr,"Set WMHints enclenche  flags=%d\n",xwmhints->flags);
     fprintf(stderr,"PixmapHint:%d flags&PixmapHint=%d\n",IconPixmapHint,(flags&IconPixmapHint));
#endif     
     if(windows[number].class==TOP_LEVEL_WINDOW)
     {
       if((flags&IconPixmapHint)==IconPixmapHint&&xwmhints->icon_pixmap!=windows[number].icon.pixmap)
         WIN_SetIconPixmap(number,xwmhints->icon_pixmap,True);
       if((flags&IconMaskHint)==IconMaskHint&&xwmhints->icon_mask!=windows[number].icon.mask)
         WIN_SetIconPixmapMask(number,xwmhints->icon_mask,True);
       if((flags&IconWindowHint)==IconWindowHint&&xwmhints->icon_window!=windows[number].icon.clientwindow)
         WIN_SetIconWindow(number,xwmhints->icon_window);
     }
#ifdef DEBUG
     fprintf(stderr,"OK jusqu'ici  group:%ld\n",xwmhints->window_group);
#endif
     if((flags&WindowGroupHint)==WindowGroupHint&&xwmhints->window_group!=windows[number].group.leader)
       WIN_SetGroupLeader(number,xwmhints->window_group);
#ifdef DEBUG
     fprintf(stderr,"OK jusqu'ici 2\n");
#endif
     if(windows[number].class==TOP_LEVEL_WINDOW&&((flags&IconMaskHint)==IconMaskHint||(flags&IconPixmapHint)==IconPixmapHint))
     {
	XClearWindow(tk_display->display,windows[number].icon.draw_area);
  	WIN_DrawIconPixmap(number);
     }
     XFree(xwmhints);
   }
#ifdef DEBUG
   else fprintf(stderr,"WMHints==NULL\n");
#endif
 }
}





void WIN_SetGroupLeader(number,group)
int number;
Window group;
{
 int i, j, mask, ret;
 unsigned long ptr;

/*
 fprintf(stderr,"Set group\n");
*/

 if(number>=0 && number<maxwindows && windows[number].isUsed==True&&windows[number].group.leader!=group)
 {
   if(group<=0)
   {
#ifdef DEBUG
     fprintf(stderr,"Remove group\n");
#endif
     WIN_RemoveFromGroup(number,True);
   }
   else
   {
#ifdef DEBUG
        fprintf(stderr,"Add to group\n");
#endif
	WIN_RemoveFromGroup(number,True);
	windows[number].group.leader=group;
	windows[number].group.number=WM_GetWindowNumber(group);
	if((windows[number].attributes&GroupMember)!=GroupMember)
	  windows[number].attributes=windows[number].attributes+GroupMember;
#ifdef DEBUG
        fprintf(stderr,"Add to group bis  %d\n",windows[number].group.number);
#endif
	WIN_AddToGroup(number);
#ifdef DEBUG
	fprintf(stderr,"Goood !!!\n");
#endif
   }
   if(windows[wm_process_index].state.isMapped==True)
     PID_GetMainWindows();
 }
#ifdef DEBUG
 fprintf(stderr,"Set group end\n");
#endif
}





int WIN_SetTransientLeader(number)
int number;
{
 int i, j, mask, ret;
 Window transient;
 XEvent send_event;

 Bool oldtransient=False;
 int win_type, win_number;


#ifdef DEBUG
 fprintf(stderr,"Set transient\n");
#endif

 

 if(number>=0 && number<maxwindows && windows[number].isUsed==True)
 {

   oldtransient=windows[number].transient.isTransient;
   ret=XGetTransientForHint(tk_display->display,windows[number].clientwindow,&transient);
   if((windows[number].attributes&GroupMember)==GroupMember&&windows[number].group.number>=0)
     return -1;
#ifdef DEBUG
   fprintf(stderr,"Transient : %d\n",windows[number].transient.isTransient);
#endif
   if(windows[number].transient.isTransient==True)
   {
     if(ret!=0 && transient!=0)
     {
       win_type=WM_GetWindowTypeWithoutProperty(transient);
       if(win_type==-1)
         return -1;
       win_number=WM_GetWindowNumber(transient);
       if(win_number==windows[number].transient.number)
	 return -1;

     }

     send_event.type=ClientMessage;
     send_event.xclient.window=windows[windows[number].transient.number].clientwindow;
     send_event.xclient.message_type=tk_display->atoms._IMAN_WM_MESSAGES;
     send_event.xclient.data.l[0]=WmUnfreezeWidgets;
     send_event.xclient.format=32;
	
     if(windows[windows[number].transient.number].state.isFrozen==True&&windows[windows[number].transient.number].freezing.number==number)
     {
	  windows[windows[number].transient.number].state.isFrozen=False;
	  windows[windows[number].transient.number].freezing.number=-1; 
	  windows[windows[number].transient.number].freezing.leader=0;
	  XSendEvent(tk_display->display,windows[windows[number].transient.number].clientwindow,False,0,&send_event);
     }
     windows[number].transient.number=-1;
     windows[number].transient.leader=0;
     windows[number].transient.isTransient=False;

     if(windows[wm_process_index].state.isMapped==True)
       PID_GetMainWindows();
     
   }
   else
   {
	windows[number].transient.number=-1;
	windows[number].transient.leader=0;
	windows[number].transient.isTransient=False;
   }
#ifdef DEBUG
   fprintf(stderr,"Transient suite\n");
#endif

   if(ret!=0 && transient!=0 && windows[number].transient.isTransient==False)
   {
#ifdef DEBUG
     fprintf(stderr,"Attention ...\n");
#endif
     win_type=WM_GetWindowClassWithoutProperty(transient);
#ifdef DEBUG
     fprintf(stderr,"win_type=%d\n",win_type);
#endif
     if(win_type==-1)
       return -1;
#ifdef DEBUG
     fprintf(stderr,"win_type bis=%d\n",win_type);
#endif
     win_number=WM_GetWindowNumber(transient);
     if(win_number<0 || win_number>=maxwindows || windows[win_number].isUsed==False)
       return -1;
#ifdef DEBUG
     fprintf(stderr,"win_number=%d  isFrozen=%d\n",win_number,windows[win_number].state.isFrozen);
#endif									
     if(windows[win_number].state.isFrozen==False)
     {
	  windows[number].transient.number=win_number;
	  windows[number].transient.leader=transient;
	  windows[number].transient.isTransient=True;
	  
	  send_event.type=ClientMessage;
	  send_event.xclient.window=windows[win_number].clientwindow;
	  send_event.xclient.message_type=tk_display->atoms._IMAN_WM_MESSAGES;
	  send_event.xclient.data.l[0]=WmFreezeWidgets;
	  send_event.xclient.format=32;
#ifdef DEBUG
	  fprintf(stderr,"Avant map: %d\n",windows[number].state.isMapped);
#endif
	  if(windows[number].state.isMapped==True)
	  {
	    XSendEvent(tk_display->display,windows[win_number].clientwindow,False,0,&send_event);
	    windows[win_number].state.isFrozen=True;	  
	    windows[win_number].freezing.number=number;
	    windows[win_number].freezing.leader=windows[number].clientwindow;
	    if(WIN_VerifyTree(number)==0)
	      WIN_Map(number);
	    if(windows[wm_process_index].state.isMapped==True)
	      PID_GetMainWindows();
	  }
	  goto END_OF_PROC;
     }
     else
     {
	  WIN_UnmapAll(number);
	  windows[number].transient.number=win_number;
	  windows[number].transient.leader=transient;
	  windows[number].transient.isTransient=True;
	  	 
	  if(windows[wm_process_index].state.isMapped==True)
 	    PID_GetMainWindows();
	  goto END_OF_PROC;
     }
   }	
     
   else
   {
#ifdef DEBUG
	fprintf(stderr,"Aiieee ...  map:%d\n",windows[number].state.isMapped);
#endif
	  windows[number].transient.number=-1;
	  windows[number].transient.leader=0;
	  windows[number].transient.isTransient=False;
	  if(windows[number].state.isMapped==True&&wm_action.type!=ReparentingAction)
	    WIN_Map(number);
	  else if(wm_action.type!=ReparentingAction&&oldtransient==True)
	    WIN_UnmapAll(number);	  
#ifdef DEBUG
	fprintf(stderr,"C'est mal parti\n");
#endif
	  if(wm_action.type!=ReparentingAction&&wm_process_index>=0&&windows[wm_process_index].state.isMapped==True)
 	    PID_GetMainWindows();
	  goto END_OF_PROC;

   }

 }
END_OF_PROC:
 /*fprintf(stderr,"Transient end\n");*/
 return 0;  
}











void WIN_SetState(number,state)
int number,state;
{
 int i,j,ret;


 if(number>=0 && number<maxwindows&&windows[number].isUsed==True)
 {
#ifdef DEBUG
   fprintf(stderr,"Demande de changement de l'etat %d    actuel iconic:%d  zoom:%d\n",state,windows[number].state.isIconic,windows[number].state.isZoomed);
#endif
   if(state==NormalState)
   {
     if(windows[number].state.isZoomed==True)
	WIN_Unzoom(number);
     goto END_OF_PROC;
   }
   else if(state==IconicState&&windows[number].state.isIconic==False)
   {
     WIN_Iconify(number);
     goto END_OF_PROC;
   }
   else if(state==ZoomState&&windows[number].state.isZoomed==False)
   {
     WIN_Zoom(number);
     goto END_OF_PROC;
   }


 }
END_OF_PROC:
 i=i;

}







			/**** Fonctions communes */


void WM_FreezeAll()
{
 int i;
 
 if(numwindows>0) for(i=0;i<maxwindows;i++)
   if(i!=wm_kill_index) WID_Freeze(tk_display,windows[i].titlebar);  
 
}




void WM_UnfreezeAll()
{
 int i;


 if(numwindows>0) for(i=0;i<maxwindows;i++)
   WID_Unfreeze(tk_display,windows[i].titlebar);  
 
}






