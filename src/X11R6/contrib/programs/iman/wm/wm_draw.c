/*
 *
 * 	wm_draw.c
 * 	dessin du WM
 *
 * 	Modification :  24/04/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
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


#ifndef WM_DRAW_C
#define WM_DRAW_C


#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "wm_struct.h"

extern TkDisplay *tk_display;
extern WmInfo   wm_info;
extern WindowStruct *windows;
extern int numwindows, maxwindows;
extern GC grab_gc, title_gc, border_gc, icon_gc;






int WIN_DrawTitle(number)
int number;
{
 int i, j;
 int mask;
 int x=0;
 int width=0;
 XPoint clip[4];
 Region clipregion;



 switch(windows[number].class)
 {


   case TOP_LEVEL_WINDOW :
 	x=0;
 	if(windows[number].state.isZoomed==True)
 	  width=windows[number].zoom.client_width;
 	else width=windows[number].normal.client_width;
	

 	if((windows[number].attributes&ZoomBox)==ZoomBox) 
 	{ 
	  width-=22;
	  x+=22;
   	}
 	if((windows[number].attributes&IconifyBox)==IconifyBox)
  	{ 
	  width-=22;
    	  x+=22;
   	}
 	if((windows[number].attributes&ZoomBox)==ZoomBox || (windows[number].attributes&IconifyBox)==IconifyBox) 
  	{
  	  width-=2;
  	  x+=2;
   	}
 	if((windows[number].attributes&CloseBox)==CloseBox)
  	width-=22;
  

 	clip[0].x=x;
 	clip[0].y=0;
 	clip[1].x=x;
 	clip[1].y=TITLEBARHEIGHT;
 	clip[2].x=x+width;
 	clip[2].y=TITLEBARHEIGHT;
 	clip[3].x=x+width;
 	clip[3].y=0;

 	clipregion=XPolygonRegion(clip,4,WindingRule);
 	XSetRegion(tk_display->display,title_gc,clipregion);
 	XSetFont(tk_display->display,title_gc,tk_display->fonts.f8_13->fid);


 	if(windows[number].state.isOnTop==True && (windows[number].attributes&TitleBar)==TitleBar)
 	{
  	  XSetForeground(tk_display->display,title_gc,tk_display->win_colors.title_text_active);
	  XSetBackground(tk_display->display,title_gc,tk_display->win_colors.title_bg_active);

	  XDrawString(tk_display->display,windows[number].titlebar,title_gc,x+(width-XTextWidth(tk_display->fonts.f8_13,windows[number].title_name,strlen(windows[number].title_name)))/2,tk_display->fonts.f8_13->ascent+4,windows[number].title_name,strlen(windows[number].title_name));

 	}
 	else if(windows[number].state.isOnTop==False &&(windows[number].attributes&TitleBar)==TitleBar)
 	{
	  XSetForeground(tk_display->display,title_gc,tk_display->win_colors.title_text_inactive);
	  XSetBackground(tk_display->display,title_gc,tk_display->win_colors.title_bg_inactive);

	  XDrawString(tk_display->display,windows[number].titlebar,title_gc,x+(width-XTextWidth(tk_display->fonts.f8_13,windows[number].title_name,strlen(windows[number].title_name)))/2,tk_display->fonts.f8_13->ascent+4,windows[number].title_name,strlen(windows[number].title_name));
 	}

	/*XSetRegion(tk_display->display,title_gc,None);*/
 	XDestroyRegion(clipregion);

 	return 0;
	break;


    case DIALOG_BOX :

 	x=2;
 	if((windows[number].attributes&Border)==Border) width=windows[number].normal.width-4;
 	else width=windows[number].normal.width-2;

 
 	if((windows[number].attributes&CloseBox)==CloseBox)
 	 width-=22;

 	clip[0].x=x;
 	clip[0].y=0;
 	clip[1].x=x;
 	clip[1].y=TITLEBARHEIGHT;
 	clip[2].x=x+width;
 	clip[2].y=TITLEBARHEIGHT;
 	clip[3].x=x+width;
 	clip[3].y=0;

 	clipregion=XPolygonRegion(clip,4,WindingRule);
 	XSetRegion(tk_display->display,title_gc,clipregion);
 	XSetFont(tk_display->display,title_gc,tk_display->fonts.f8_13->fid);

 	/*fprintf(stderr,"Name: %s\n",windows[number].title_name);*/

 	if(windows[number].state.isOnTop==True && (windows[number].attributes&TitleBar)==TitleBar)
 	{
	  /*fprintf(stderr,"OOooop ...\n");*/
	  XSetForeground(tk_display->display,title_gc,tk_display->dlg_colors.title_text_active);
	  XSetBackground(tk_display->display,title_gc,tk_display->dlg_colors.title_bg_active);
	  XDrawImageString(tk_display->display,windows[number].titlebar,title_gc,x+(width-XTextWidth(tk_display->fonts.f8_13,windows[number].title_name,strlen(windows[number].title_name)))/2,tk_display->fonts.f8_13->ascent+4,windows[number].title_name,strlen(windows[number].title_name));
 	}
 	else if(windows[number].state.isOnTop==False &&(windows[number].attributes&TitleBar)==TitleBar)
 	{
	  /*fprintf(stderr,"OOooop 2...\n");*/
	  XSetForeground(tk_display->display,title_gc,tk_display->dlg_colors.title_text_inactive);
	  XSetBackground(tk_display->display,title_gc,tk_display->dlg_colors.title_bg_inactive);
	  XDrawString(tk_display->display,windows[number].titlebar,title_gc,x+(width-XTextWidth(tk_display->fonts.f8_13,windows[number].title_name,strlen(windows[number].title_name)))/2,tk_display->fonts.f8_13->ascent+4,windows[number].title_name,strlen(windows[number].title_name));
 	}

 	/*XSetRegion(tk_display->display,title_gc,None);*/
 	XDestroyRegion(clipregion);

 	return 0;
	break;

 }
}






int WIN_DrawBorder(windowbox,number)
Window windowbox;
unsigned int number;
{
 int w, h;




 if(wm_info.wm_style==FULLDECORATION)
 {
  w=windows[number].normal.width-32;
  h=windows[number].normal.height-32;
  
  if(windows[number].state.isOnTop==False)
    XSetWindowBorder(tk_display->display,windows[number].mainwindow,tk_display->win_colors.border_inactive);
  else XSetWindowBorder(tk_display->display,windows[number].mainwindow,tk_display->win_colors.border_active);

  if(windowbox==windows[number].topleftbox)
  {
   /*fprintf(stderr,"TopLeftBorder\n");*/

   XSetBackground(tk_display->display,border_gc,tk_display->win_colors.bg);
   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.shadow);
   XDrawRectangle(tk_display->display,windows[number].topleftbox,border_gc,0,0,14,14);   
   XDrawRectangle(tk_display->display,windows[number].topleftbox,border_gc,3,3,15,15);

   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.light);
   XDrawLine(tk_display->display,windows[number].topleftbox,border_gc,0,0,0,15);   
   XDrawLine(tk_display->display,windows[number].topleftbox,border_gc,0,0,15,0);

   if(windows[number].state.isOnTop==False) XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_inactive);
   else XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_active);
   XDrawLine(tk_display->display,windows[number].topleftbox,border_gc,4,4,4,17);
   XDrawLine(tk_display->display,windows[number].topleftbox,border_gc,4,4,17,4);
  }
  else if(windowbox==windows[number].bottomleftbox)
  {
   /*fprintf(stderr,"BottomLeftBorder\n");*/

   XSetBackground(tk_display->display,border_gc,tk_display->win_colors.bg);
   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.shadow);
   XDrawRectangle(tk_display->display,windows[number].bottomleftbox,border_gc,0,0,14,13);   
   XDrawRectangle(tk_display->display,windows[number].bottomleftbox,border_gc,3,0,14,10);

   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.light);
   XDrawLine(tk_display->display,windows[number].bottomleftbox,border_gc,0,0,0,15);   
   XDrawLine(tk_display->display,windows[number].bottomleftbox,border_gc,0,0,15,0);
   XDrawLine(tk_display->display,windows[number].bottomleftbox,border_gc,4,10,15,10);

   if(windows[number].state.isOnTop==False) XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_inactive);
   else XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_active);
   XDrawRectangle(tk_display->display,windows[number].bottomleftbox,border_gc,4,0,15,9);

  }

  else if(windowbox==windows[number].toprightbox)
  {
   /*fprintf(stderr,"TopRightBorder\n");*/

   XSetBackground(tk_display->display,border_gc,tk_display->win_colors.bg);
   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.shadow);
   XDrawRectangle(tk_display->display,windows[number].toprightbox,border_gc,0,0,13,14);   
   XDrawRectangle(tk_display->display,windows[number].toprightbox,border_gc,0,3,10,15);

   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.light);
   XDrawLine(tk_display->display,windows[number].toprightbox,border_gc,0,0,0,15);   
   XDrawLine(tk_display->display,windows[number].toprightbox,border_gc,0,0,14,0);
   XDrawLine(tk_display->display,windows[number].toprightbox,border_gc,10,4,10,17);

   if(windows[number].state.isOnTop==False) XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_inactive);
   else XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_active);
   XDrawLine(tk_display->display,windows[number].toprightbox,border_gc,0,4,9,4);
   XDrawLine(tk_display->display,windows[number].toprightbox,border_gc,9,4,9,17);
  }

  else if(windowbox==windows[number].bottomrightbox)
  {
   /*fprintf(stderr,"BottomRightBorder\n");*/

   XSetBackground(tk_display->display,border_gc,tk_display->win_colors.bg);
   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.light);
   XDrawRectangle(tk_display->display,windows[number].bottomrightbox,border_gc,0,0,14,14);   
   XDrawRectangle(tk_display->display,windows[number].bottomrightbox,border_gc,0,0,10,10);

   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.shadow);
   XDrawLine(tk_display->display,windows[number].bottomrightbox,border_gc,1,13,15,13);   
   XDrawLine(tk_display->display,windows[number].bottomrightbox,border_gc,13,1,13,15);

   if(windows[number].state.isOnTop==False) XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_inactive);
   else XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_active);
   XDrawRectangle(tk_display->display,windows[number].bottomrightbox,border_gc,0,0,9,9);
  }

  else if(windowbox==windows[number].topbar||windowbox==windows[number].bottombar)
  {
   XSetBackground(tk_display->display,border_gc,tk_display->win_colors.bg);
   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.shadow);
   XDrawRectangle(tk_display->display,windowbox,border_gc,16,0,w,3);   

   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.light);
   XDrawLine(tk_display->display,windowbox,border_gc,16,0,16,15);   
   XDrawLine(tk_display->display,windowbox,border_gc,16,0,16+w,0);

   if(windows[number].state.isOnTop==False) XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_inactive);
   else XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_active);
   XDrawRectangle(tk_display->display,windowbox,border_gc,15,-1,2+w,15);

  }

  else if(windowbox==windows[number].leftbar||windowbox==windows[number].rightbar)
  {
   XSetBackground(tk_display->display,border_gc,tk_display->win_colors.bg);
   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.shadow);
   XDrawRectangle(tk_display->display,windowbox,border_gc,0,16,3,h);   

   XSetForeground(tk_display->display,border_gc,tk_display->win_colors.light);
   XDrawLine(tk_display->display,windowbox,border_gc,0,16,15,16);   
   XDrawLine(tk_display->display,windowbox,border_gc,0,16,0,16+h);

   if(windows[number].state.isOnTop==False) XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_inactive);
   else XSetForeground(tk_display->display,border_gc,tk_display->win_colors.border_active);
   XDrawRectangle(tk_display->display,windowbox,border_gc,-1,15,17,2+h);

  }


 }
 else if(wm_info.wm_style==LIGHTDECORATION)
 {


 }
 return 0;

}







int WIN_DrawActive(number)
int number;
{
 if(number>=0 && number<maxwindows && windows[number].isUsed==True)
 switch(windows[number].class)
 {

  case TOP_LEVEL_WINDOW :

     XSetWindowBackground(tk_display->display,windows[number].titlebar,tk_display->win_colors.title_bg_active);
     if(windows[number].state.isZoomed==True)
       XClearArea(tk_display->display,windows[number].titlebar,0,0,windows[number].zoom.width+10,TITLEBARHEIGHT+2,False);
     else XClearArea(tk_display->display,windows[number].titlebar,0,0,windows[number].normal.width+10,TITLEBARHEIGHT+2,False);
     XSetWindowBorder(tk_display->display,windows[number].mainwindow,tk_display->win_colors.border_active);
     WIN_DrawTitle(number);
     break;

  case DIALOG_BOX :

     XSetWindowBackground(tk_display->display,windows[number].titlebar,tk_display->dlg_colors.title_bg_active);
     XClearArea(tk_display->display,windows[number].titlebar,0,0,windows[number].normal.width+10,TITLEBARHEIGHT+2,False);
     XSetWindowBorder(tk_display->display,windows[number].mainwindow,tk_display->dlg_colors.title_bg_active);
     WIN_DrawTitle(number);
     break;

 }
 return 0;
}







int WIN_DrawUnactive(number)
int number;
{
 if(number>=0 && number<maxwindows && windows[number].isUsed==True)
 switch(windows[number].class)
 {

   case TOP_LEVEL_WINDOW :

     	XSetWindowBackground(tk_display->display,windows[number].titlebar,tk_display->win_colors.title_bg_inactive);
     	if(windows[number].state.isZoomed==True) 
	  XClearArea(tk_display->display,windows[number].titlebar,0,0,windows[number].zoom.width+10,TITLEBARHEIGHT+2,False);
     	else XClearArea(tk_display->display,windows[number].titlebar,0,0,windows[number].normal.width+10,TITLEBARHEIGHT+2,False);
     	XSetWindowBorder(tk_display->display,windows[number].mainwindow,tk_display->win_colors.border_inactive);
     	WIN_DrawTitle(number);
     	break;

   case DIALOG_BOX :

	XSetWindowBackground(tk_display->display,windows[number].titlebar,tk_display->dlg_colors.title_bg_inactive);
	XClearArea(tk_display->display,windows[number].titlebar,0,0,windows[number].normal.width+10,TITLEBARHEIGHT+2,False);
	XSetWindowBorder(tk_display->display,windows[number].mainwindow,tk_display->dlg_colors.title_bg_inactive);
	WIN_DrawTitle(number);
     	break;

 }
 return 0;
}





int WIN_DrawIconTitle(number)
int number;
{
 int j, ret;

 if(number>=0 && number<maxwindows && windows[number].isUsed==True)
 {
#ifdef DEBUG
   fprintf(stderr,"Icon title expose\n");
#endif
   XClearWindow(tk_display->display,windows[number].icon.title);
   XSetForeground(tk_display->display,icon_gc,tk_display->icn_colors.title_text_active);
   XSetBackground(tk_display->display,icon_gc,tk_display->icn_colors.title_bg_active);
   XSetClipMask(tk_display->display,icon_gc,None);
   XSetClipOrigin(tk_display->display,icon_gc,0,0);
   XSetFont(tk_display->display,icon_gc,tk_display->fonts.f5_7->fid);
#ifdef DEBUG
   fprintf(stderr,"Icon_name= %s\n",windows[number].icon_name);   
#endif
   if(windows[number].icon.name!=(char *)NULL)
     XDrawString(tk_display->display,windows[number].icon.title,icon_gc,(ICON_DRAW_AREA_WIDTH-XTextWidth(tk_display->fonts.f5_7,windows[number].icon.name,strlen(windows[number].icon.name)))/2,10,windows[number].icon.name,strlen(windows[number].icon.name));
   return 0;
 }
 else return -1;
}





int WIN_DrawIconBorder(number)
int number;
{
 int j, ret;

 if(number>=0 && number<maxwindows && windows[number].isUsed==True)
 {
#ifdef DEBUG
   fprintf(stderr,"Icon border expose\n");
#endif
   XSetForeground(tk_display->display,icon_gc,tk_display->icn_colors.icn_light);
   XSetBackground(tk_display->display,icon_gc,tk_display->icn_colors.icn_bg);
   XSetClipMask(tk_display->display,icon_gc,None);
   XSetClipOrigin(tk_display->display,icon_gc,0,0);

   XDrawLine(tk_display->display,windows[number].icon.window,icon_gc,0,0,0,ICON_DRAW_AREA_HEIGHT*2);
   XDrawLine(tk_display->display,windows[number].icon.window,icon_gc,0,0,ICON_DRAW_AREA_WIDTH*2,0);
   XSetForeground(tk_display->display,icon_gc,tk_display->icn_colors.icn_shadow);
   XDrawLine(tk_display->display,windows[number].icon.window,icon_gc,1,windows[number].icon.height-1,ICON_DRAW_AREA_WIDTH*2,windows[number].icon.height-1);
   XDrawLine(tk_display->display,windows[number].icon.window,icon_gc,windows[number].icon.width-1,1,windows[number].icon.width-1,windows[number].icon.height);
/*   XSetForeground(tk_display->display,icon_gc,tk_display->icn_colors.title_text_inactive);
   XSetBackground(tk_display->display,icon_gc,tk_display->icn_colors.title_bg_inactive);
   XSetFont(tk_display->display,icon_gc,tk_display->fonts.f5_7->fid);
   XDrawString(tk_display->display,windows[number].icon_title,icon_gc,(ICON_DRAW_AREA_WIDTH-XTextWidth(tk_display->fonts.f5_7,windows[number].icon_name,strlen(windows[number].icon_name)))/2,11,windows[number].icon_name,strlen(windows[number].icon_name));
*/
#ifdef DEBUG
   fprintf(stderr,"Icon title fin\n");
#endif
   return 0;
 }
 else return -1;
}





int WIN_DrawIconPixmap(number)
int number;
{
 int i, j, ret;

 if(number>=0 && number<maxwindows && windows[number].isUsed==True)
 {
#ifdef DEBUG
   fprintf(stderr,"Icon pixmap expose\n");
#endif
   i=number;
   XSetForeground(tk_display->display,icon_gc,tk_display->icn_colors.icn_draw);
   XSetBackground(tk_display->display,icon_gc,tk_display->icn_colors.icn_draw_bg);
   if(windows[i].icon.mask!=0)
   {
	XSetClipMask(tk_display->display,icon_gc,windows[i].icon.mask);
	XSetClipOrigin(tk_display->display,icon_gc,(ICON_DRAW_AREA_WIDTH-windows[i].icon.pix_width)/2,(ICON_DRAW_AREA_HEIGHT-windows[i].icon.pix_height)/2);
   }
   else
   {
	XSetClipMask(tk_display->display,icon_gc,None);
	XSetClipOrigin(tk_display->display,icon_gc,0,0);
   }
   if(windows[i].icon.depth==1)
     XCopyPlane(tk_display->display,windows[i].icon.pixmap,windows[i].icon.draw_area,icon_gc,0,0,windows[i].icon.pix_width,windows[i].icon.pix_height,(ICON_DRAW_AREA_WIDTH-windows[i].icon.pix_width)/2,(ICON_DRAW_AREA_HEIGHT-windows[i].icon.pix_height)/2,1);
   else if(windows[i].icon.depth==tk_display->depth)
     XCopyArea(tk_display->display,windows[i].icon.pixmap,windows[i].icon.draw_area,icon_gc,0,0,windows[i].icon.pix_width,windows[i].icon.pix_height,(ICON_DRAW_AREA_WIDTH-windows[i].icon.pix_width)/2,(ICON_DRAW_AREA_HEIGHT-windows[i].icon.pix_height)/2);
#ifdef DEBUG
   fprintf(stderr,"Icon pixmap end\n");
#endif
   return 0;
  
 }
 else return -1;
}



#endif

