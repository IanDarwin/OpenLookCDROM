/*
 *
 * 	wm_data.c 
 * 	gestion des donnes du WM
 *
 * 	Modification :  30/01/94
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
 * Cree et ajoute 'number' fenetres a la liste
 *
 */

int WM_AddWindows(number)
unsigned int number;
{
 int i, j;
 

 if(maxwindows>0) windows=(WindowStruct *)realloc(windows,sizeof(WindowStruct)*(maxwindows+number));  
 else windows=(WindowStruct *)malloc(sizeof(WindowStruct)*number);  
 if(windows==NULL)
 {
 	fprintf(stderr,"iman: Pas assez de memoire pour allouer des fenetres\n");
	tk_CloseSession(tk_display);
	exit(-1);
 }

 for(i=maxwindows;i<maxwindows+number;i++)
 {
   windows[i].class=-2;
   windows[i].isUsed=False;
 }

 maxwindows=maxwindows+number;
 return 0;

}






/*
 * Initialise une fenetre
 *
 */

int WM_InitWindow(number,class,type)
int number,class,type;
{
 WidgetAttributes wid_attributes;
 WidgetPixmapDecoration wid_pixmap;

 int i, j;
 int mask;
 XSetWindowAttributes attrib;
 unsigned long ptr;


 attrib.background_pixel=tk_display->win_colors.bg;
 attrib.border_pixel=tk_display->win_colors.border_inactive;
 attrib.cursor=tk_display->cursors.normal;
 attrib.event_mask=ExposureMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|SubstructureRedirectMask|SubstructureNotifyMask; 
 attrib.override_redirect=True;
 mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor|CWOverrideRedirect;


 if(number<maxwindows)
 switch(class)
 {

   case TOP_LEVEL_WINDOW :

	windows[number].class=TOP_LEVEL_WINDOW;
	windows[number].type=NormalWindow;
	windows[number].attributes=TitleBar+Border+IconifyBox+ZoomBox+CloseBox;
	windows[number].attributes=0;	

				/*** Fenetre principale ***/
	
	windows[number].mainwindow=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),10,10,100,100,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
	windows[number].clientwindow=0;
	windows[number].hasStoredName=False;
	windows[number].isUsed=False;

	ptr=REPARENTING_WINDOW;
	XChangeProperty(tk_display->display, windows[number].mainwindow,tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
	
	
	
				/*** Bars de la fenetre ***/
	
	attrib.event_mask=ExposureMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask; 
	attrib.cursor=tk_display->cursors.left_right;
	windows[number].leftbar=XCreateWindow(tk_display->display,windows[number].mainwindow,-1,-1,BORDERSIZE,100,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	windows[number].rightbar=XCreateWindow(tk_display->display,windows[number].mainwindow,95,-1,BORDERSIZE,100,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	attrib.cursor=tk_display->cursors.up_down;
	windows[number].topbar=XCreateWindow(tk_display->display,windows[number].mainwindow,-1,-1,100,BORDERSIZE,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	windows[number].bottombar=XCreateWindow(tk_display->display,windows[number].mainwindow,-1,95,100,BORDERSIZE,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	
	
	
				/*** Box de la fenetres ***/
	
	attrib.cursor=tk_display->cursors.top_left;
	windows[number].topleftbox=XCreateWindow(tk_display->display,windows[number].mainwindow,0,0,15,15,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	attrib.cursor=tk_display->cursors.top_right;
	windows[number].toprightbox=XCreateWindow(tk_display->display,windows[number].mainwindow,86,0,15,15,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	attrib.cursor=tk_display->cursors.bottom_left;
	windows[number].bottomleftbox=XCreateWindow(tk_display->display,windows[number].mainwindow,0,86,15,15,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	attrib.cursor=tk_display->cursors.bottom_right;
	windows[number].bottomrightbox=XCreateWindow(tk_display->display,windows[number].mainwindow,86,86,15,15,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	
	
	
				 /*** Titre et boutons ***/

	attrib.background_pixel=tk_display->win_colors.title_bg_inactive; 
	attrib.cursor=tk_display->cursors.normal;
	windows[number].titlebar=XCreateWindow(tk_display->display,windows[number].mainwindow,15,-1,68,TITLEBARHEIGHT,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);  
	
	wid_attributes.mask=SALighting|SANeverFocus;
	wid_attributes.lighting=True;
	wid_attributes.neverFocus=True;
	windows[number].bn_zoom=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,windows[number].titlebar,windows[number].titlebar,3,1,17,17,&wid_attributes,Unpushed);
	wid_pixmap.mask=SPWidth|SPHeight|SPDepth|SPPixmap|SPGravity;
	wid_pixmap.width=13;
	wid_pixmap.height=13;
	wid_pixmap.depth=1;
	wid_pixmap.gravity=CenterBitmap;
	wid_pixmap.pixmap=tk_display->pixmaps.zoom;
	wid_SetPixmapDecoration(tk_display,windows[number].bn_zoom,&wid_pixmap,True);
	
	
	windows[number].bn_iconify=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,windows[number].titlebar,windows[number].titlebar,25,1,17,17,&wid_attributes,Unpushed);
	wid_pixmap.pixmap=tk_display->pixmaps.iconify;
	wid_SetPixmapDecoration(tk_display,windows[number].bn_iconify,&wid_pixmap,True);
	
	windows[number].bn_close=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,windows[number].titlebar,windows[number].titlebar,46,1,17,17,&wid_attributes,Unpushed);
	wid_pixmap.pixmap=tk_display->pixmaps.close;
	wid_SetPixmapDecoration(tk_display,windows[number].bn_close,&wid_pixmap,True);
	
/*	XMapWindow(tk_display->display,windows[number].topbar);	
	XMapWindow(tk_display->display,windows[number].bottombar);
	XMapWindow(tk_display->display,windows[number].leftbar);
	XMapWindow(tk_display->display,windows[number].rightbar);

	XMapWindow(tk_display->display,windows[number].topleftbox);
	XMapWindow(tk_display->display,windows[number].toprightbox);
	XMapWindow(tk_display->display,windows[number].bottomleftbox);	
	XMapWindow(tk_display->display,windows[number].bottomrightbox);

	XMapWindow(tk_display->display,windows[number].titlebar);
	wid_Map(tk_display->display,windows[number].bn_close);
	wid_Map(tk_display->display,windows[number].bn_zoom);
	wid_Map(tk_display->display,windows[number].bn_iconify);
*/

				/**** Etats ****/

	WM_InitNormal(number);
	WM_InitZoom(number);
	WM_InitIcon(number);

				   /*** Divers ***/

	WM_InitState(number);
	WM_InitGroup(number);
	WM_InitIdentity(number);
	WM_InitTransient(number);
	WM_InitFreezing(number);

	break;


   case DIALOG_BOX :

	windows[number].class=DIALOG_BOX;
	windows[number].type=NormalWindow;
	windows[number].attributes=TitleBar+Border+CloseBox;
	windows[number].attributes=0;

 	windows[number].mainwindow=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),0,0,100,100,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
	windows[number].clientwindow=0;
	windows[number].hasStoredName=False;
	windows[number].isUsed=False;

 	ptr=REPARENTING_DIALOG;
 	XChangeProperty(tk_display->display,windows[number].mainwindow, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeReplace,(char *)&ptr,1);

			 /*** Titre et boutons ***/

 	attrib.border_pixel=tk_display->dlg_colors.bg;
 	attrib.background_pixel=tk_display->dlg_colors.title_bg_inactive; 
 	attrib.cursor=tk_display->cursors.normal;
 	windows[number].titlebar=XCreateWindow(tk_display->display,windows[number].mainwindow,0,0,98,TITLEBARHEIGHT,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);  


 	wid_attributes.mask=SALighting|SANeverFocus;
 	wid_attributes.lighting=True;
 	wid_attributes.neverFocus=True;
 	windows[number].bn_close=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,windows[number].titlebar,windows[number].titlebar,76,1,17,17,&wid_attributes,Unpushed);
 	wid_pixmap.mask=SPWidth|SPHeight|SPDepth|SPPixmap|SPGravity;
 	wid_pixmap.width=13;
 	wid_pixmap.height=13;
 	wid_pixmap.depth=1;
 	wid_pixmap.gravity=CenterBitmap;
 	wid_pixmap.pixmap=tk_display->pixmaps.close;
 	wid_SetPixmapDecoration(tk_display,windows[number].bn_close,&wid_pixmap,True);
	windows[number].bn_iconify=windows[number].bn_zoom=-1;

	
				/**** Etats ****/

	WM_InitNormal(number);
	WM_InitZoom(number);
	WM_InitIcon(number);

				   /*** Divers ***/

	WM_InitState(number);
	WM_InitGroup(number);
	WM_InitIdentity(number);
	WM_InitTransient(number);
	WM_InitFreezing(number);
	
	break;

 }	
 else return -1;
 return 0;

}






/*
 * Re-initialise une fenetre
 *
 */

int WM_ReInitWindow(number)
unsigned int number;
{
 int *children, *children_types;
 int numchildren, maxchildren;

 if(number>=0 && number<maxwindows)
 {

   XUnmapWindow(tk_display->display,windows[number].mainwindow);
   XUnmapWindow(tk_display->display,windows[number].icon.window);

   if((windows[number].attributes&GroupLeader)==GroupLeader)
   {
#ifdef DEBUG
     fprintf(stderr,"Avant unlinkgroup\n");
#endif
     WIN_UnlinkGroup(number,True);
#ifdef DEBUG
     fprintf(stderr,"Apres unlinkgroup\n");
#endif
   }

   if((windows[number].attributes&GroupMember)==GroupMember)
   {
#ifdef DEBUG
     fprintf(stderr,"Avant remove group\n");
#endif
     WIN_RemoveFromGroup(number,False);
#ifdef DEBUG
     fprintf(stderr,"Avant remove group\n");
#endif
   }

   if(windows[number].state.isFrozen==True)
   {
#ifdef DEBUG
     fprintf(stderr,"Avant FROZEN\n");
#endif
     windows[windows[number].freezing.number].transient.isTransient=False;
     windows[windows[number].freezing.number].transient.leader=0;
     windows[windows[number].freezing.number].transient.number=-1;
     if(windows[windows[number].freezing.number].state.isMapped==True)
       WIN_Map(windows[number].freezing.number);
#ifdef DEBUG
     fprintf(stderr,"Apres FROZEN\n");
#endif
   }

   if(windows[number].identity.maxprotocols>0&&windows[number].identity.protocols!=NULL)
   {
#ifdef DEBUG
        fprintf(stderr,"Avant PROTOCOLS\n");
#endif
	windows[number].identity.numprotocols=windows[number].identity.maxprotocols=0;
	free(windows[number].identity.protocols);
	windows[number].identity.protocols=(Atom *)NULL;
#ifdef DEBUG
     	fprintf(stderr,"Apres PROTOCOLS\n");
#endif
   }
#ifdef DEBUG
   fprintf(stderr,"Avant NAMES\n");
#endif
   if(windows[number].hasStoredName==True&&windows[number].title_name!=NULL) XFree(windows[number].title_name);
   windows[number].title_name=NULL;
#ifdef DEBUG
   fprintf(stderr,"Avant icon name\n");
#endif
   if(windows[number].class==TOP_LEVEL_WINDOW && windows[number].icon.hasStoredIconName==True&&windows[number].icon.name!=NULL) XFree(windows[number].icon.name);
   windows[number].icon.name=NULL;
#ifdef DEBUG
   fprintf(stderr,"Apres NAMES\n");
#endif

   windows[number].identity.shape=False;
#ifdef HAS_SHAPE_EXTENSION_LIBRARY
#ifdef DEBUG
   fprintf(stderr,"Avant SHAPE\n");
#endif
   XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,None,0);
   XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,None,0);
#ifdef DEBUG
   fprintf(stderr,"Apres SHAPE\n");
#endif
#endif
   windows[number].attributes=0;

   WM_InitState(number);
   WM_InitGroup(number);
   WM_InitIdentity(number);
   WM_InitTransient(number);
   WM_InitFreezing(number);


   windows[number].isUsed=False;
   return 0;
 }
 else return -1;
}





int WM_InitNormal(number)
int number;
{
 if(number<0 || number>=maxwindows)
   return -1;


 switch(windows[number].class)
 {

   case TOP_LEVEL_WINDOW :

 	windows[number].normal.x=0;
 	windows[number].normal.y=0;
 	windows[number].normal.width=100;
 	windows[number].normal.height=100;  
 	windows[number].normal.border=2;  

 	windows[number].normal.client_x=5;
 	windows[number].normal.client_y=28;
 	windows[number].normal.client_width=90;
 	windows[number].normal.client_height=67;  

 	windows[number].normal.max_width=MAXWIDTH;
 	windows[number].normal.max_height=MAXHEIGHT;  
 	windows[number].normal.min_width=MINWIDTH;
 	windows[number].normal.min_height=MINHEIGHT;  

	windows[number].normal.add_top=28;
	windows[number].normal.add_bottom=5;
	windows[number].normal.add_leftside=5;
	windows[number].normal.add_rightside=5;
	windows[number].title_name=NULL;

	break;


   case DIALOG_BOX :

 	windows[number].normal.x=0;
 	windows[number].normal.y=0;
 	windows[number].normal.width=100;
 	windows[number].normal.height=100;  
 	windows[number].normal.border=2;  

 	windows[number].normal.client_x=0;
 	windows[number].normal.client_y=TITLEBARHEIGHT+2;
 	windows[number].normal.client_width=100;
 	windows[number].normal.client_height=100-TITLEBARHEIGHT-2;  

 	windows[number].normal.max_width=MAXWIDTH;
 	windows[number].normal.max_height=MAXHEIGHT;  
 	windows[number].normal.min_width=MINWIDTH;
 	windows[number].normal.min_height=MINHEIGHT;  

	windows[number].normal.add_top=TITLEBARHEIGHT+2;
	windows[number].normal.add_bottom=0;
	windows[number].normal.add_leftside=0;
	windows[number].normal.add_rightside=0;

   	windows[number].title_name=NULL;
	break;
 }
 return 0;

}




int WM_InitIcon(number)
int number;
{
  XSetWindowAttributes attrib;
  unsigned long ptr;
  unsigned long mask; 
  

 if(number<0 || number>=maxwindows)
   return -1;


 switch(windows[number].class)
 {
   
   case TOP_LEVEL_WINDOW :

 	windows[number].icon.x=0;
 	windows[number].icon.y=0;
 	windows[number].icon.width=ICONWIDTH;
	if(wm_info.set_icontitle==True) 
 	  windows[number].icon.height=ICONHEIGHT;  	
	else windows[number].icon.height=ICONHEIGHT-14;  	

	windows[number].icon.pixmap=windows[number].icon.mask=0;
	windows[number].icon.depth=0;
	windows[number].icon.pix_x=0;
	windows[number].icon.pix_width=0;
	windows[number].icon.pix_height=0;

	windows[number].icon.hasIconWindow=False;
	windows[number].icon.hasStoredIconName=False;
	windows[number].icon.name=NULL;

	attrib.background_pixel=tk_display->icn_colors.icn_bg; 
	attrib.event_mask=KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|ExposureMask|SubstructureRedirectMask;
	attrib.cursor=tk_display->cursors.normal;
	attrib.override_redirect=True;
	mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor|CWOverrideRedirect;
	if(wm_info.set_icontitle==True) 
	  windows[number].icon.window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),10,10,68,82,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	else windows[number].icon.window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),10,10,68,68,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	
	attrib.event_mask=ExposureMask;
	attrib.background_pixel=tk_display->icn_colors.title_bg_active;
	windows[number].icon.title=XCreateWindow(tk_display->display,windows[number].icon.window,2,2,ICON_DRAW_AREA_WIDTH,14,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	if(wm_info.set_icontitle==True) XMapRaised(tk_display->display,windows[number].icon.title);

	attrib.background_pixel=tk_display->icn_colors.icn_draw_bg; 
	if(wm_info.set_icontitle==True) windows[number].icon.draw_area=XCreateWindow(tk_display->display,windows[number].icon.window,2,16,ICON_DRAW_AREA_WIDTH,ICON_DRAW_AREA_HEIGHT,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	else windows[number].icon.draw_area=XCreateWindow(tk_display->display,windows[number].icon.window,1,1,ICON_DRAW_AREA_WIDTH,ICON_DRAW_AREA_HEIGHT,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib); 
	XMapRaised(tk_display->display,windows[number].icon.draw_area);
	
	ptr=ICON_WINDOW;
	XChangeProperty(tk_display->display, windows[number].icon.window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeReplace,(char *)&ptr,1);
	XChangeProperty(tk_display->display, windows[number].icon.draw_area, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeReplace,(char *)&ptr,1);
	XChangeProperty(tk_display->display, windows[number].icon.title, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeReplace,(char *)&ptr,1);
	windows[number].clientwindow=0;

	break;
 }
 return 0;
}




int WM_InitZoom(number)
int number;
{
 if(number<0 || number>=maxwindows)
   return -1;

 switch(windows[number].class)
 {

   case TOP_LEVEL_WINDOW :

 	windows[number].zoom.x=-1;
 	windows[number].zoom.y=-1;
 	windows[number].zoom.width=ZOOMMAXWIDTH;
 	windows[number].zoom.height=ZOOMMAXHEIGHT;  
 	windows[number].zoom.border=2;  

 	windows[number].zoom.client_x=0;
 	windows[number].zoom.client_y=24;
 	windows[number].zoom.client_width=ZOOMMAXWIDTH;
 	windows[number].zoom.client_height=ZOOMMAXHEIGHT-23;  

 	windows[number].zoom.max_width=ZOOMMAXWIDTH;
 	windows[number].zoom.max_height=ZOOMMAXHEIGHT;  
 	windows[number].zoom.min_width=ZOOMMAXWIDTH;
 	windows[number].zoom.min_height=ZOOMMAXHEIGHT;  

	windows[number].zoom.add_top=23;
	windows[number].zoom.add_bottom=0;
	windows[number].zoom.add_leftside=0;
	windows[number].zoom.add_rightside=0;

	break;


   case DIALOG_BOX :
   default :

 	windows[number].zoom.x=0;
 	windows[number].zoom.y=0;
 	windows[number].zoom.width=0;
 	windows[number].zoom.height=0;  
 	windows[number].zoom.border=0;  

 	windows[number].zoom.client_x=0;
 	windows[number].zoom.client_y=0;
 	windows[number].zoom.client_width=0;
 	windows[number].zoom.client_height=0;  

 	windows[number].zoom.max_width=0;
 	windows[number].zoom.max_height=0;  
 	windows[number].zoom.min_width=0;
 	windows[number].zoom.min_height=0;  

	windows[number].zoom.add_top=0;
	windows[number].zoom.add_bottom=0;
	windows[number].zoom.add_leftside=0;
	windows[number].zoom.add_rightside=0;

	break;
 }
 return 0;

}




int WM_InitState(number)
int number;
{
 if(number<0 || number>=maxwindows)
   return -1;

 switch(windows[number].class)
 {

   case TOP_LEVEL_WINDOW : 
   default:

	windows[number].state.isFrozen=False;
	windows[number].state.isZoomed=False;
	windows[number].state.isIconic=False;
	windows[number].state.isWithdrawn=False;
	windows[number].state.isMapped=False;
	windows[number].state.isOnTop=False;
	windows[number].state.isInitialized=False;
	windows[number].state.initialstate=DontCareState;
    	break;

 }

 return 0;
}




int WM_InitIdentity(number)
int number;
{
 if(number<0 || number>=maxwindows)
   return -1;

 switch(windows[number].class)
 {

   case TOP_LEVEL_WINDOW : 
   default:
	windows[number].identity.input=False;
	windows[number].identity.shape=False;
	windows[number].identity.numprotocols=0;
	windows[number].identity.maxprotocols=0;
	windows[number].identity.protocols=(Atom *)NULL;
	windows[number].identity.colormap=default_colormap;
	break;
 }

 return 0;
}


int WM_InitGroup(number)
int number;
{
 if(number<0 || number>=maxwindows)
   return -1;

 switch(windows[number].class)
 {

   case TOP_LEVEL_WINDOW : 
   default:
	windows[number].group.leader=0;
	windows[number].group.number=-1;
	windows[number].group.nummembers=0;
	windows[number].group.maxmembers=0;

	break;
 }
 return 0;
}




int WM_InitTransient(number)
int number;
{
 if(number<0 || number>=maxwindows)
   return -1;

 switch(windows[number].class)
 {

   case TOP_LEVEL_WINDOW : 
   default:
	windows[number].transient.leader=0;
	windows[number].transient.number=-1;
	windows[number].transient.isTransient=False;

	break;
 }
 return 0;
}





int WM_InitFreezing(number)
int number;
{
 if(number<0 || number>=maxwindows)
   return -1;

 switch(windows[number].class)
 {

   case TOP_LEVEL_WINDOW : 
   default:
	windows[number].freezing.leader=0;
	windows[number].freezing.number=-1;

	break;
 }
 return 0;
}





void WM_CheckFreeWindows()
{
  int i, j;


  if(numwindows>=maxwindows||WM_GetUnusedWindow(TOP_LEVEL_WINDOW)==-1||WM_GetUnusedWindow(DIALOG_BOX)==-1) 
  {
    if(WM_GetUnusedWindow(TOP_LEVEL_WINDOW)==-1)
    {
	j=maxwindows;
    	WM_AddWindows(10);
	for(i=0;i<10;i++)
	  WM_InitWindow(j+i,TOP_LEVEL_WINDOW,NormalWindow);
    }
    if(WM_GetUnusedWindow(DIALOG_BOX)==-1)
    {
	j=maxwindows;
    	WM_AddWindows(10);
	  for(i=0;i<10;i++)
	WM_InitWindow(j+i,DIALOG_BOX,NormalWindow);
    }
    XSync(tk_display->display,False);
    XFlush(tk_display->display);
  }

}





void WM_VerifyWindows()
{
  int i, j;

  j=0;
#ifdef DEBUG
  fprintf(stderr,"Verifying windows ...\n");
#endif
  for(i=0;i<maxwindows;i++)
  if(windows[i].isUsed==False && (windows[i].state.isMapped==True||windows[i].state.isIconic==True))
  {
    j++;
    WM_ReInitWindow(i);
  }
#ifdef DEBUG
  fprintf(stderr,"Windows OK  errors=%d\n",j);
#endif
}





