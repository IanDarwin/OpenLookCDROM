/*
 *
 * 	mn_draw.c  
 * 	affichage des menus
 *
 * 	Modification :  05/12/93
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
 *      IMAN Development Toolkit version 1.1.a
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

#include <X11/iman/widgets.h>




/*
 *
 * Affichage du menu
 *
 *
 */

int MN_Map(tk_display,menuid,xr,yr)
TkDisplay *tk_display;
MenuID menuid;
int xr, yr;
{
 MenuStruct *menu, *parentmenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


 menu->x=xr;
 menu->y=yr;

 menu->repeat=-1;
 menu->selected=True;
 
 if(menu->selecteditem<0||menu->selecteditem>=menu->numitems) menu->selecteditem=0;


 if(menu->type==MN_FLOATING&&menu->parency_class==0)
 {
   XMoveResizeWindow(tk_display->display,menu->window,xr,yr,menu->width,menu->height);
   XMapRaised(tk_display->display, menu->window); 
   XSync(tk_display->display,False);
   XResizeWindow(tk_display->display,menu->window,menu->width,menu->height);
 }
 else if(menu->type==MN_MENUBAR)
  XMapRaised(tk_display->display, menu->window); 
 return 0;
}





int mn_Map(tk_display,menuid,xr,yr)
TkDisplay *tk_display;
MenuID menuid;
int xr,yr;
{
 unsigned long mask;
 XSetWindowAttributes xswa;
 MenuStruct *menu, *parentmenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;

 menu->x=xr;
 menu->y=yr;

 menu->repeat=-1;
 menu->selected=True;
 
 if(menu->selecteditem<0||menu->selecteditem>=menu->numitems) menu->selecteditem=0;


 if(menu->type==MN_FLOATING)
 {
   /*fprintf(stderr,"Map: xr=%d yr=%d\n",xr,yr);*/



   xswa.override_redirect=False;
   mask=CWOverrideRedirect;
   if(tk_display->wm.active==True)
     XChangeWindowAttributes(tk_display->display,menu->window,mask,&xswa);

   XMoveResizeWindow(tk_display->display,menu->window,xr,yr,menu->width,menu->height);
   XMapRaised(tk_display->display, menu->window); 
   XSync(tk_display->display,False);

 }
 else if(menu->type==MN_MENUBAR)
  XMapRaised(tk_display->display, menu->window); 
 return 0;
}







/*
 *
 * Retire le menu de l'ecran
 *
 *
 */


int MN_Unmap(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 MenuStruct *menu, *parentmenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;

 mn_CloseMenu(tk_display,menuid);
 XUnmapWindow(tk_display->display, menu->window);  
 menu->selecteditem=-1;
 menu->selected=False;
 menu->hasFocus=NO;
 return 0;
}





int mn_DrawFloatingMenu(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 GC gc;   
 XGCValues xgcvalues;
 int mask;
 unsigned int h1, h2, i, j, ptr, start, h;
 unsigned int leng;
 MenuStruct *menu, *parentmenu;
 WidgetStruct *widptr;

 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;
 widptr=&tk_display->widgets[menuid];


if(menu->numitems>0)
{
 /*XResizeWindow(tk_display->display,menu->window,menu->width,menu->height);
 XSetWindowBorder(tk_display->display,menu->window,widptr->colors->text);*/
 mask=GCForeground|GCBackground;
 xgcvalues.foreground=widptr->colors->text;
 xgcvalues.background=widptr->colors->bg;
 gc=XCreateGC(tk_display->display, menu->window, mask, &xgcvalues);			
 XSetFont(tk_display->display,gc,menu->font->fid);


 j=MN_UMARGE;
 i=0;
 while (i<menu->numitems&&j+MN_UMARGE<=menu->height)
 {

  h=menu->items[i].height;
  if(h<10) h=10;
  
  if(menu->items[i].type==MN_HBAR) h=7;



   switch(menu->items[i].type){


	case MN_ITEM:
	case MN_SUBMENU:


	   if(menu->hasFocus==True && menu->selecteditem==i && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->selected);
		XSetBackground(tk_display->display,gc,widptr->colors->selected);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
		}
	   else if(menu->hasFocus==True && menu->selecteditem==i && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->selected);
		XSetBackground(tk_display->display,gc,widptr->colors->selected);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed_selected);
		}
	   else if(menu->hasFocus==True && menu->selecteditem!=i && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text);
		}
	   else if(menu->hasFocus==True && menu->selecteditem!=i && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		}
	   else if(menu->hasFocus==False && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text);
		}
	   else if(menu->hasFocus==False && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		}


	   
	  XSetFont(tk_display->display,gc,menu->items[i].font->fid);	  



	  if((menu->items[i].flags&TextFlag)==TextFlag)
	  {

	   h1=(menu->items[i].height-menu->items[i].font->ascent-menu->items[i].font->descent)/2;
	   if(h1<0) h1=0;
	   if(menu->items[i].text!=NULL&&strlen(menu->items[i].text)>0)
	     leng=XTextWidth(menu->items[i].font,menu->items[i].text,strlen(menu->items[i].text));
	   else leng=0;

	   if(menu->items[i].precedency==TextFlag)
	    {
											
		  XDrawString(tk_display->display,menu->window,gc,MN_LMARGE,j+h1+menu->items[i].font->ascent,menu->items[i].text,strlen(menu->items[i].text));
	    }
	   else if(menu->items[i].precedency==PixmapFlag)
	    {
		if((menu->items[i].flags&PixmapFlag)==PixmapFlag)
		  XDrawString(tk_display->display,menu->window,gc,MN_LMARGE+MN_INTERMARGE+menu->items[i].pix_width,j+h1+menu->items[i].font->ascent,menu->items[i].text,strlen(menu->items[i].text));
		else
		  XDrawString(tk_display->display,menu->window,gc,MN_LMARGE,j+h1+menu->items[i].font->ascent,menu->items[i].text,strlen(menu->items[i].text));
	    }
	   else XDrawString(tk_display->display,menu->window,gc,MN_LMARGE,j+h1+menu->items[i].font->ascent,menu->items[i].text,strlen(menu->items[i].text));
	  }



/*	  if(menu->items[i].key>0 && menu->items[i].key<=strlen(menu->items[i].text)&&(menu->items[i].flags&TextFlag)==TextFlag)
	  {
	    if(menu->items[i].text!=NULL&&strlen(menu->items[i].text)>0)	   
	      leng=XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key-1);
	    else leng=0;
  

	    if(menu->items[i].precedency==TextFlag)
	    {
		if(menu->items[i].text!=NULL&&strlen(menu->items[i].text)>0&&menu->items[i].key>0)
		{
		  XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2);
		  XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1);
                }
	    }
	    else if(menu->items[i].precedency==PixmapFlag)
	    {
		if((menu->items[i].flags&PixmapFlag)==PixmapFlag){
		  XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+MN_INTERMARGE+menu->items[i].pix_width+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2,MN_LMARGE+MN_INTERMARGE+menu->items[i].pix_width+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2);
		  XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+MN_INTERMARGE+menu->items[i].pix_width+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1,MN_LMARGE+MN_INTERMARGE+menu->items[i].pix_width+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1);
		  }
		else{
		  XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2);
		  XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1);
		 }
	    }
	    else{ XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2);
		 XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1);
		}
	  }
*/	

	  if((menu->items[i].state&Checked)==Checked)
	  {
		XDrawLine(tk_display->display,menu->window,gc,5,j+4+(menu->items[i].height-10)/2,7,j+8+(menu->items[i].height-10)/2);
		XDrawLine(tk_display->display,menu->window,gc,7,j+8+(menu->items[i].height-10)/2,13,j+2+(menu->items[i].height-10)/2);
		XDrawLine(tk_display->display,menu->window,gc,6,j+4+(menu->items[i].height-10)/2,8,j+8+(menu->items[i].height-10)/2);
		XDrawLine(tk_display->display,menu->window,gc,8,j+8+(menu->items[i].height-10)/2,14,j+2+(menu->items[i].height-10)/2);
	  }


	  if(menu->items[i].type==MN_SUBMENU)
	  {
		XCopyPlane(tk_display->display,tk_display->pixmaps.right,menu->window,gc,0,0,11,11,menu->width-15,j+(h-10)/2,1);
	  }


	  if((menu->items[i].flags&PixmapFlag)==PixmapFlag)
	  {
	
	   h2=(menu->items[i].height-menu->items[i].pix_height)/2;
	   if(h2<0) h2=0;
	   if(menu->items[i].text!=NULL&&strlen(menu->items[i].text)>0)	   
	     leng=XTextWidth(menu->items[i].font,menu->items[i].text,strlen(menu->items[i].text));
	   else leng=0;

	   if(menu->items[i].precedency==TextFlag||menu->items[i].precedency==0)
	    {
      
	       if((menu->items[i].flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag&&((menu->items[i].state&Grayed)==Grayed||(menu->state&Grayed)==Grayed))
		{
		 XSetClipMask(tk_display->display,gc,menu->items[i].pix_maskgrayed);
		 XSetClipOrigin(tk_display->display,gc,MN_LMARGE+MN_INTERMARGE+leng,j+h2);
		}
		else if((menu->items[i].flags&PixmapMaskFlag)==PixmapMaskFlag)
		{
		 XSetClipMask(tk_display->display,gc,menu->items[i].pix_mask);
		 XSetClipOrigin(tk_display->display,gc,MN_LMARGE+MN_INTERMARGE+leng,j+h2);
		}
	

	      if(((menu->items[i].state&Grayed)==Grayed||(menu->state&Grayed)==Grayed)&&(menu->items[i].flags&PixmapGrayedFlag)==PixmapGrayedFlag)
	      {
	       if(menu->items[i].pix_depth==1)
		XCopyPlane(tk_display->display,menu->items[i].pix_grayed,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE+MN_INTERMARGE+leng,j+h2,1);
	       else if(menu->items[i].pix_depth==tk_display->depth)
		XCopyArea(tk_display->display,menu->items[i].pix_grayed,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE+MN_INTERMARGE+leng,j+h2);
		}
	      else
	      {
	       if(menu->items[i].pix_depth==1)
		XCopyPlane(tk_display->display,menu->items[i].pixmap,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE+MN_INTERMARGE+leng,j+h2,1);
	       else if(menu->items[i].pix_depth==tk_display->depth)
		XCopyArea(tk_display->display,menu->items[i].pixmap,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE+MN_INTERMARGE+leng,j+h2);
		}

	      XSetClipMask(tk_display->display,gc,None);	
	    }

	   else if(menu->items[i].precedency==PixmapFlag)
	    {
	
	      if((menu->items[i].flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag&&((menu->items[i].state&Grayed)==Grayed||(menu->state&Grayed)==Grayed))
		{
		 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
		 XSetClipMask(tk_display->display,gc,menu->items[i].pix_mask);
		 XSetClipOrigin(tk_display->display,gc,MN_LMARGE,j+h2);
		}
	      else if((menu->items[i].flags&PixmapMaskFlag)==PixmapMaskFlag)
		{
		 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
		 XSetClipMask(tk_display->display,gc,menu->items[i].pix_mask);
		 XSetClipOrigin(tk_display->display,gc,MN_LMARGE,j+h2);
		}


	      if(((menu->items[i].state&Grayed)==Grayed||(menu->state&Grayed)==Grayed)&&(menu->items[i].flags&PixmapGrayedFlag)==PixmapGrayedFlag)
	      {
	      if(menu->items[i].pix_depth==1)
		XCopyPlane(tk_display->display,menu->items[i].pix_grayed,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE,j+h2,1);
	      else if(menu->items[i].pix_depth==tk_display->depth)											
		XCopyArea(tk_display->display,menu->items[i].pix_grayed,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE,j+h2);
		}
	      else {
	      if(menu->items[i].pix_depth==1)
		XCopyPlane(tk_display->display,menu->items[i].pixmap,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE,j+h2,1);
	      else if(menu->items[i].pix_depth==tk_display->depth)											
		XCopyArea(tk_display->display,menu->items[i].pixmap,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE,j+h2);
		}
	

	     XSetClipMask(tk_display->display,gc,None);	
	    }

  	   }
	  
	  break;


	case MN_HBAR:

		
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text);
		XDrawLine(tk_display->display,menu->window,gc,0,j+3,menu->width+1,j+3);
		XDrawLine(tk_display->display,menu->window,gc,0,j+4,menu->width+1,j+4);
				
		break;

	}
	j=j+h;
  	i++;

    }

    XFreeGC(tk_display->display, gc);  

 }

}





int mn_DrawFloatingItem(tk_display,menuid,number)
TkDisplay *tk_display;
MenuID menuid;
int number;
{
 GC gc;   
 XGCValues xgcvalues;
 int mask;
 unsigned int h1, h2, i, j, ptr, start, h;
 unsigned int leng;
 MenuStruct *menu, *parentmenu;
 WidgetStruct *widptr;

 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;
 widptr=&tk_display->widgets[menuid];

if(number>=0 && number<menu->numitems)
{

 mask=GCForeground|GCBackground;
 xgcvalues.foreground=widptr->colors->text;
 xgcvalues.background=widptr->colors->bg;
 gc=XCreateGC(tk_display->display, menu->window, mask, &xgcvalues);			
 XSetFont(tk_display->display,gc,menu->font->fid);


 j=MN_UMARGE;
 i=0;
 while (i<number&&j+MN_UMARGE<=menu->height)
 {
  h=menu->items[i].height;
  if(h<10) h=10;
  if(menu->items[i].type==MN_HBAR) h=7;
  j=j+h;
  i++;

 }
  i=number;
  h=menu->items[i].height;
  if(h<10) h=10;
  if(menu->items[i].type==MN_HBAR) h=7;




   switch(menu->items[i].type){


	case MN_ITEM:
	case MN_SUBMENU:

	   if(menu->hasFocus==True && menu->selecteditem==i && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->selected);
		XSetBackground(tk_display->display,gc,widptr->colors->selected);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
		}
	   else if(menu->hasFocus==True && menu->selecteditem==i && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->selected);
		XSetBackground(tk_display->display,gc,widptr->colors->selected);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed_selected);
		}
	   else if(menu->hasFocus==True && menu->selecteditem!=i && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text);
		}
	   else if(menu->hasFocus==True && menu->selecteditem!=i && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		}
	   else if(menu->hasFocus==False && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text);
		}
	   else if(menu->hasFocus==False && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		}

	   
	  XSetFont(tk_display->display,gc,menu->items[i].font->fid);	  

	  if((menu->items[i].flags&TextFlag)==TextFlag)
	  {

	   h1=(menu->items[i].height-menu->items[i].font->ascent-menu->items[i].font->descent)/2;
	   if(h1<0) h1=0;
	   if(menu->items[i].text!=NULL&&strlen(menu->items[i].text)>0)	   
	     leng=XTextWidth(menu->items[i].font,menu->items[i].text,strlen(menu->items[i].text));
	   else leng=0;

	   if(menu->items[i].precedency==TextFlag)
	    {
											
		  XDrawString(tk_display->display,menu->window,gc,MN_LMARGE,j+h1+menu->items[i].font->ascent,menu->items[i].text,strlen(menu->items[i].text));
	    }
	   else if(menu->items[i].precedency==PixmapFlag)
	    {
		if((menu->items[i].flags&PixmapFlag)==PixmapFlag)
		  XDrawString(tk_display->display,menu->window,gc,MN_LMARGE+MN_INTERMARGE+menu->items[i].pix_width,j+h1+menu->items[i].font->ascent,menu->items[i].text,strlen(menu->items[i].text));
		else
		  XDrawString(tk_display->display,menu->window,gc,MN_LMARGE,j+h1+menu->items[i].font->ascent,menu->items[i].text,strlen(menu->items[i].text));
	    }
	   else XDrawString(tk_display->display,menu->window,gc,MN_LMARGE,j+h1+menu->items[i].font->ascent,menu->items[i].text,strlen(menu->items[i].text));
	  }



/*	  if(menu->items[i].key>0 && menu->items[i].key<=strlen(menu->items[i].text)&&(menu->items[i].flags&TextFlag)==TextFlag)
	  {
	    leng=XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key-1);

	    if(menu->items[i].precedency==TextFlag)
	    {
		XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2);
		XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1);
                
	    }
	    else if(menu->items[i].precedency==PixmapFlag)
	    {
		if((menu->items[i].flags&PixmapFlag)==PixmapFlag){
		  XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+MN_INTERMARGE+menu->items[i].pix_width+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2,MN_LMARGE+MN_INTERMARGE+menu->items[i].pix_width+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2);
		  XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+MN_INTERMARGE+menu->items[i].pix_width+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1,MN_LMARGE+MN_INTERMARGE+menu->items[i].pix_width+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1);
		  }
		else{
		  XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2);
		  XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1);
		 }
	    }
	    else{ XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-2);
		 XDrawLine(tk_display->display,menu->window,gc,MN_LMARGE+leng,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1,MN_LMARGE+XTextWidth(menu->items[i].font,menu->items[i].text,menu->items[i].key)-1,j+h1+menu->items[i].font->ascent+menu->items[i].font->descent-1);
		}
	  }

*/

	  if((menu->items[i].state&Checked)==Checked)
	  {
		XDrawLine(tk_display->display,menu->window,gc,5,j+4+(menu->items[i].height-10)/2,7,j+8+(menu->items[i].height-10)/2);
		XDrawLine(tk_display->display,menu->window,gc,7,j+8+(menu->items[i].height-10)/2,13,j+2+(menu->items[i].height-10)/2);
		XDrawLine(tk_display->display,menu->window,gc,6,j+4+(menu->items[i].height-10)/2,8,j+8+(menu->items[i].height-10)/2);
		XDrawLine(tk_display->display,menu->window,gc,8,j+8+(menu->items[i].height-10)/2,14,j+2+(menu->items[i].height-10)/2);
	  }



	  if(menu->items[i].type==MN_SUBMENU)
	  {
		XCopyPlane(tk_display->display,tk_display->pixmaps.right,menu->window,gc,0,0,11,11,menu->width-15,j+(h-10)/2,1);
	  }


	  if((menu->items[i].flags&PixmapFlag)==PixmapFlag)
	  {
	
	   h2=(menu->items[i].height-menu->items[i].pix_height)/2;
	   if(h2<0) h2=0;
	   if(menu->items[i].text!=NULL&&strlen(menu->items[i].text)>0)	   
	     leng=XTextWidth(menu->items[i].font,menu->items[i].text,strlen(menu->items[i].text));
	   else leng=0;

	   if(menu->items[i].precedency==TextFlag||menu->items[i].precedency==0)
	    {
      
	       if((menu->items[i].flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag&&((menu->items[i].state&Grayed)==Grayed||(menu->state&Grayed)==Grayed))
		{
		 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
		 XSetClipMask(tk_display->display,gc,menu->items[i].pix_maskgrayed);
		 XSetClipOrigin(tk_display->display,gc,MN_LMARGE+MN_INTERMARGE+leng,j+h2);
		}
		else if((menu->items[i].flags&PixmapMaskFlag)==PixmapMaskFlag)
		{
		 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
		 XSetClipMask(tk_display->display,gc,menu->items[i].pix_mask);
		 XSetClipOrigin(tk_display->display,gc,MN_LMARGE+MN_INTERMARGE+leng,j+h2);
		}
	

	      if(((menu->items[i].state&Grayed)==Grayed||(menu->state&Grayed)==Grayed)&&(menu->items[i].flags&PixmapGrayedFlag)==PixmapGrayedFlag)
	      {
	       if(menu->items[i].pix_depth==1)
		XCopyPlane(tk_display->display,menu->items[i].pix_grayed,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE+MN_INTERMARGE+leng,j+h2,1);
	       else if(menu->items[i].pix_depth==tk_display->depth)
		XCopyArea(tk_display->display,menu->items[i].pix_grayed,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE+MN_INTERMARGE+leng,j+h2);
		}
	      else
	      {
	       if(menu->items[i].pix_depth==1)
		XCopyPlane(tk_display->display,menu->items[i].pixmap,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE+MN_INTERMARGE+leng,j+h2,1);
	       else if(menu->items[i].pix_depth==tk_display->depth)
		XCopyArea(tk_display->display,menu->items[i].pixmap,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE+MN_INTERMARGE+leng,j+h2);
		}

	      XSetClipMask(tk_display->display,gc,None);	
	    }

	   else if(menu->items[i].precedency==PixmapFlag)
	    {
	
	      if((menu->items[i].flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag&&((menu->items[i].state&Grayed)==Grayed||(menu->state&Grayed)==Grayed))
		{
		 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
		 XSetClipMask(tk_display->display,gc,menu->items[i].pix_mask);
		 XSetClipOrigin(tk_display->display,gc,MN_LMARGE,j+h2);
		}
	      else if((menu->items[i].flags&PixmapMaskFlag)==PixmapMaskFlag)
		{
		 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
		 XSetClipMask(tk_display->display,gc,menu->items[i].pix_mask);
		 XSetClipOrigin(tk_display->display,gc,MN_LMARGE,j+h2);
		}


	      if(((menu->items[i].state&Grayed)==Grayed||(menu->state&Grayed)==Grayed)&&(menu->items[i].flags&PixmapGrayedFlag)==PixmapGrayedFlag)
	      {
	      if(menu->items[i].pix_depth==1)
		XCopyPlane(tk_display->display,menu->items[i].pix_grayed,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE,j+h2,1);
	      else if(menu->items[i].pix_depth==tk_display->depth)											
		XCopyArea(tk_display->display,menu->items[i].pix_grayed,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE,j+h2);
		}
	      else {
	      if(menu->items[i].pix_depth==1)
		XCopyPlane(tk_display->display,menu->items[i].pixmap,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE,j+h2,1);
	      else if(menu->items[i].pix_depth==tk_display->depth)											
		XCopyArea(tk_display->display,menu->items[i].pixmap,menu->window,gc,0,0,menu->items[i].pix_width,menu->items[i].pix_height,MN_LMARGE,j+h2);
		}
	

	     XSetClipMask(tk_display->display,gc,None);	
	    }

  	   }
	  
	  break;


	case MN_HBAR:

		
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,0,j,menu->width,h);
		XSetForeground(tk_display->display,gc,widptr->colors->text);
		XDrawLine(tk_display->display,menu->window,gc,0,j+3,menu->width+1,j+3);
		XDrawLine(tk_display->display,menu->window,gc,0,j+4,menu->width+1,j+4);
				
		break;

	}

   XFreeGC(tk_display->display, gc);  
 }

}







int mn_DrawMenuBar(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 GC gc;   
 XGCValues xgcvalues;
 int mask;
 unsigned int h1, h2, i, j, ptr, start, h, w, w1, w2;
 unsigned int leng;
 MenuStruct *menu, *parentmenu;
 WidgetStruct *widptr;

 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;
 widptr=&tk_display->widgets[menuid];

if(menu->numitems>0)
{

 /*XResizeWindow(tk_display->display,menu->window,menu->width,menu->height);*/
 
 mask=GCForeground|GCBackground;
 xgcvalues.foreground=widptr->colors->text;
 xgcvalues.background=widptr->colors->bg;
 gc=XCreateGC(tk_display->display, menu->window, mask, &xgcvalues);			
 XSetFont(tk_display->display,gc,menu->font->fid);


 j=0-MN_INTERMARGE/2;
 i=0;
 while (i<menu->numitems&&j+MN_INTERMARGE<=menu->width)
 {

   w=menu->items[i].width;
   switch(menu->items[i].type){


	case MN_ITEM:
	case MN_SUBMENU:


	   if(menu->hasFocus==True && menu->selecteditem==i && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->selected);
		XSetBackground(tk_display->display,gc,widptr->colors->selected);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
		}
	   else if(menu->hasFocus==True && menu->selecteditem==i && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->selected);
		XSetBackground(tk_display->display,gc,widptr->colors->selected);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed_selected);
		}
	   else if(menu->hasFocus==True && menu->selecteditem!=i && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text);
		}
	   else if(menu->hasFocus==True && menu->selecteditem!=i && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		}
	   else if(menu->hasFocus==False && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text);
		}
	   else if(menu->hasFocus==False && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		}
	   

	  if((menu->items[i].flags&TextFlag)==TextFlag)
	  {
	   XDrawString(tk_display->display,menu->window,gc,j+(2*MN_INTERMARGE/2),MN_UMARGE+menu->font->ascent,menu->items[i].text,strlen(menu->items[i].text));
    	  }


/*	  if(menu->items[i].key>0 && menu->items[i].key<=strlen(menu->items[i].text)&&(menu->items[i].flags&TextFlag)==TextFlag)
	  {
	   
	   leng=XTextWidth(menu->font,menu->items[i].text,menu->items[i].key-1);

	   XDrawLine(tk_display->display,menu->window,gc,j+leng+(2*MN_INTERMARGE/2),MN_UMARGE+menu->font->ascent+menu->font->descent,j+XTextWidth(menu->font,menu->items[i].text,menu->items[i].key)-1+(2*MN_INTERMARGE/2),MN_UMARGE+menu->font->ascent+menu->font->descent);
	   XDrawLine(tk_display->display,menu->window,gc,j+leng+(2*MN_INTERMARGE/2),MN_UMARGE+menu->font->ascent+menu->font->descent-1,j+XTextWidth(menu->font,menu->items[i].text,menu->items[i].key)-1+(2*MN_INTERMARGE/2),MN_UMARGE+menu->font->ascent-1+menu->font->descent);

	  }
 */
	  break;



	case MN_VBAR:

		/* Indisponible pour l'instant */
		break;


	}
	j=j+w+2*MN_INTERMARGE;
	i++;

    }

  XFreeGC(tk_display->display, gc);  
 
 }
}






int mn_DrawMenuBarItem(tk_display,menuid,number)
TkDisplay *tk_display;
MenuID menuid;
int number;
{
 GC gc;   
 XGCValues xgcvalues;
 int mask;
 unsigned int h1, h2, i, j, ptr, start, h, w, w1, w2;
 unsigned int leng;
 MenuStruct *menu, *parentmenu;
 WidgetStruct *widptr;

 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;
 widptr=&tk_display->widgets[menuid];


if(number>=0 && number<menu->numitems)
{

 /*XResizeWindow(tk_display->display,menu->window,menu->width,menu->height);*/
 
 mask=GCForeground|GCBackground;
 xgcvalues.foreground=widptr->colors->text;
 xgcvalues.background=widptr->colors->bg;
 gc=XCreateGC(tk_display->display, menu->window, mask, &xgcvalues);			
 XSetFont(tk_display->display,gc,menu->font->fid);


 j=0-MN_INTERMARGE/2;
 i=0;
 while (i<number&&j+MN_INTERMARGE<=menu->width)
 {
  w=menu->items[i].width;
  j=j+w+2*MN_INTERMARGE;
  i++;
 }

 i=number;

   switch(menu->items[i].type){


	case MN_ITEM:
	case MN_SUBMENU:


	   if(menu->hasFocus==True && menu->selecteditem==i && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->selected);
		XSetBackground(tk_display->display,gc,widptr->colors->selected);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
		}
	   else if(menu->hasFocus==True && menu->selecteditem==i && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->selected);
		XSetBackground(tk_display->display,gc,widptr->colors->selected);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed_selected);
		}
	   else if(menu->hasFocus==True && menu->selecteditem!=i && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text);
		}
	   else if(menu->hasFocus==True && menu->selecteditem!=i && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		}
	   else if(menu->hasFocus==False && (menu->items[i].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text);
		}
	   else if(menu->hasFocus==False && ((menu->items[i].state&Grayed)==Grayed || (menu->state&Grayed)==Grayed)){
		XSetForeground(tk_display->display,gc,widptr->colors->bg);
		XSetBackground(tk_display->display,gc,widptr->colors->bg);
		XFillRectangle(tk_display->display,menu->window,gc,j,0,menu->items[i].width+2*MN_INTERMARGE,menu->height);
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		}
	   

	  if((menu->items[i].flags&TextFlag)==TextFlag)
	  {
	   XDrawString(tk_display->display,menu->window,gc,j+(2*MN_INTERMARGE/2),MN_UMARGE+menu->font->ascent,menu->items[i].text,strlen(menu->items[i].text));
    	  }

/*	  if(menu->items[i].key>0 && menu->items[i].key<=strlen(menu->items[i].text)&&(menu->items[i].flags&TextFlag)==TextFlag)
	  {
	   
	   leng=XTextWidth(menu->font,menu->items[i].text,menu->items[i].key-1);

	   XDrawLine(tk_display->display,menu->window,gc,j+leng+(2*MN_INTERMARGE/2),MN_UMARGE+menu->font->ascent+menu->font->descent,j+XTextWidth(menu->font,menu->items[i].text,menu->items[i].key)-1+(2*MN_INTERMARGE/2),MN_UMARGE+menu->font->ascent+menu->font->descent);
	   XDrawLine(tk_display->display,menu->window,gc,j+leng+(2*MN_INTERMARGE/2),MN_UMARGE+menu->font->ascent+menu->font->descent-1,j+XTextWidth(menu->font,menu->items[i].text,menu->items[i].key)-1+(2*MN_INTERMARGE/2),MN_UMARGE+menu->font->ascent-1+menu->font->descent);

	  }
*/ 
	  break;



	case MN_VBAR:

		/* Indisponible pour l'instant */
		break;


	}

   XFreeGC(tk_display->display, gc);  
 }

}

