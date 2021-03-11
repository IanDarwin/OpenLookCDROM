/*
 *
 * 	bn_draw.c  
 * 	affichage des boutons 
 *
 * 	Modification :  27/11/93
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>


#include <X11/iman/widgets.h>





			/******** Affichage du bouton ********/



int BN_Map(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;

 XMapWindow(tk_display->display, button->window);			
 return 0;
}





int BN_Unmap(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{ 
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;

 XUnmapWindow(tk_display->display, button->window);			 
 return 0;
}




			/******* Dessin du bouton *******/



int bn_DrawThumbButton(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
  if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
    return bn_DrawPushButton(tk_display,buttonid);
  else return -1;
}





int bn_DrawComboButton(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
  if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
    return bn_DrawPushButton(tk_display,buttonid);
  else return -1;
}





int bn_DrawScrollButton(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
  if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
    return bn_DrawPushButton(tk_display,buttonid);
  else return -1;
}





int bn_DrawRepeatButton(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
  if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
    return bn_DrawPushButton(tk_display,buttonid);
  else return -1;
}






int bn_DrawPopupButton(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
  if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
    return bn_DrawPushButton(tk_display,buttonid);
  else return -1;
}






int bn_DrawPushButton(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 GC gc;
 XGCValues xgcvalues;
 XRectangle rect;
 int mask;
 int gravity; 
 ButtonStruct *button;
 WidgetStruct *widptr;

#ifdef DEBUG
 fprintf(stderr,"Dessin d'un bouton\n");
#endif

 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];

 /*fprintf(stderr,"ID=%d\n",buttonid);*/


 if(button->type==BN_PUSHBUTTON || button->type==BN_REPEATBUTTON || button->type==BN_SCROLLBUTTON || button->type==BN_THUMBBUTTON || button->type==BN_POPUPBUTTON|| button->type==BN_COMBOBUTTON)
 {

   
   rect.x=rect.y=2;
   rect.width=button->width-4;
   rect.height=button->height-4;

   mask=GCForeground|GCBackground|GCClipXOrigin|GCClipYOrigin;
   xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
   xgcvalues.function=GXcopy;
   xgcvalues.clip_x_origin=(button->width-button->pix_width)/2;
   xgcvalues.clip_y_origin=(button->height-button->pix_height)/2;
   gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);
 
   if(button->hasFocus==True && button->neverFocus==NO) XSetWindowBorder(tk_display->display,button->window,widptr->colors->focus);
   else if(button->hasFocus==False && button->neverFocus==NO)
     XSetWindowBorder(tk_display->display,button->window,widptr->colors->nofocus);


   /*fprintf(stderr,"Button state:%d\n",button->state);*/

   switch(button->state&Pushed){


   case Unpushed:

 	XSetWindowBackground(tk_display->display, button->window, widptr->colors->bg);
	XClearWindow(tk_display->display,button->window); 

	XSetForeground(tk_display->display,gc,widptr->colors->light);
 	XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
        
 	XDrawLine(tk_display->display,button->window, gc, 0,0,button->width,0);
	XDrawLine(tk_display->display,button->window, gc, 0,1,button->width,1);
 	XDrawLine(tk_display->display,button->window, gc, 0,0,0,button->height);
 	XDrawLine(tk_display->display,button->window, gc, 1,0,1,button->height);

 	XSetForeground(tk_display->display, gc, widptr->colors->shadow);
	
 	XDrawLine(tk_display->display,button->window, gc, button->width-2,2,button->width-2,button->height);
 	XDrawLine(tk_display->display,button->window, gc, button->width-1,2,button->width-1,button->height);
 	XDrawLine(tk_display->display,button->window, gc, 2,button->height-1,button->width,button->height-1);
 	XDrawLine(tk_display->display,button->window, gc, 2,button->height-2,button->width,button->height-2);
 	XDrawPoint(tk_display->display,button->window, gc,1,button->height-1); 
 	XDrawPoint(tk_display->display,button->window,gc,button->width-1,1); 

        /*fprintf(stderr,"Unpushed flags=%d\n",button->flags);*/

	if((button->flags&PixmapFlag)==PixmapFlag||(button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
	{
	
	 gravity=CenterBitmap;
	 if ((button->state&Grayed)==Grayed&&(button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
	   gravity=button->pixgrayed_gravity;
         else gravity=button->pix_gravity;

	 switch (gravity)
	 {
	 
  	  case CenterBitmap:
	
		XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed) 
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		   XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		   if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag) 
		   { 
			XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);		  
		 	XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)/2,(button->height-button->pixgrayed_height)/2);
		   }
		   if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2,(button->height-button->pixgrayed_height)/2,1);
		   else if(button->pixgrayed_depth==tk_display->depth)
		     XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2,(button->height-button->pixgrayed_height)/2);
		 }
		 else
		 {
		   XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		   if((button->flags&PixmapMaskFlag)==PixmapMaskFlag) 	
		   { 
			XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2,(button->height-button->pix_height)/2);
		   }
		   if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,(button->height-button->pix_height)/2,1);
		   else if(button->pix_depth==tk_display->depth)
		     XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,(button->height-button->pix_height)/2);
		 }
		}

		else
		{
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag) 
		  { 	
			XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2,(button->height-button->pix_height)/2);
		  }
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,(button->height-button->pix_height)/2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,(button->height-button->pix_height)/2);
		}
		break;


	  case NorthWestBitmap:

		XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		  if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		  {
		    XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		    if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag) 
		    { 
			XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);		  
			XSetClipOrigin(tk_display->display,gc,2,2);
		    }
		    if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,2,2,1);
		    else if(button->pixgrayed_depth==tk_display->depth)
		      XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,2,2);
		  }
		  else
		  {
		    XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		    if((button->flags&PixmapMaskFlag)==PixmapMaskFlag) 
		    { 
			XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,2,2); 
		    }
		    if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,2,2,1);
		    else if(button->pix_depth==tk_display->depth)
		      XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,2,2);
		  }
		}

		else
		{
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag) 
		  { 
			XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,2,2);
		  }
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,2,2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,2,2);
		}

		break;


 	  case WestBitmap:

		XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if((button->state&Grayed)==Grayed)
		{
		  if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		  {
		    XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		    if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag) 
  		    { 
			XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,2,(button->height-button->pixgrayed_height)/2);
		    }
		    if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,2,(button->height-button->pixgrayed_height)/2,1);
		    else if(button->pix_depth==tk_display->depth)
		      XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,2,(button->height-button->pixgrayed_height)/2);
		  }
		  else
		  {
		    XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		    if((button->flags&PixmapMaskFlag)==PixmapMaskFlag) 
		    { 
			XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,2,(button->height-button->pix_height)/2);
		    }
		    if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,2,(button->height-button->pix_height)/2,1);
		    else if(button->pix_depth==tk_display->depth)
		      XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,2,(button->height-button->pix_height)/2);
		  }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,2,(button->height-button->pix_height)/2);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,2,(button->height-button->pix_height)/2,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,2,(button->height-button->pix_height)/2);
		}

		break;


	  case SouthWestBitmap:


		XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		  if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		  {
		    XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		    if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag) 
		    { 
			XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,2,(button->height-button->pixgrayed_height)-4);
		    }
		    if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,2,(button->height-button->pixgrayed_height)-4,1);
		    else if(button->pix_depth==tk_display->depth)
		      XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,2,(button->height-button->pixgrayed_height)-4);
		  }
		  else
		  {
		    XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		    if((button->flags&PixmapMaskFlag)==PixmapMaskFlag) 
		    { 
			XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,2,(button->height-button->pix_height)-4);
		    }
		    if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,2,(button->height-button->pix_height)-4,1);
		    else if(button->pix_depth==tk_display->depth)
		      XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,2,(button->height-button->pix_height)-4);
		  }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,2,(button->height-button->pix_height)-4);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,2,(button->height-button->pix_height)-4,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,2,(button->height-button->pix_height)-4);
		}

		break;


	  case NorthBitmap:

		
		XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);		  
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)/2,2);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2,2,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2,2);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2,2);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,2);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2,2);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,2,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,2);
		}

		break;


	  case SouthBitmap:


		XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)/2,(button->height-button->pixgrayed_height)-4);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2,(button->height-button->pixgrayed_height)-4,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2,(button->height-button->pixgrayed_height)-4);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2,(button->height-button->pix_height)-4);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,(button->height-button->pix_height)-4,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,(button->height-button->pix_height)-4);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2,(button->height-button->pix_height)-4);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,(button->height-button->pix_height)-4,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2,(button->height-button->pix_height)-4);
		}

		break;


	  case EastBitmap:


		XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)-4,(button->height-button->pixgrayed_height)/2);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-4,(button->height-button->pixgrayed_height)/2,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-4,(button->height-button->pixgrayed_height)/2);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-4,(button->height-button->pix_height)/2);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,(button->height-button->pix_height)/2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,(button->height-button->pix_height)/2);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-4,(button->height-button->pix_height)/2);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,(button->height-button->pix_height)/2,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,(button->height-button->pix_height)/2);
		}

		break;


  	  case NorthEastBitmap:


		XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)-4,2);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-4,2,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-4,2);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-4,2);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,2);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-4,2);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,2,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,2);
		}

		break;

		
	  case SouthEastBitmap:


		XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)-4,(button->height-button->pixgrayed_height)-4);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-4,(button->height-button->pixgrayed_height)-4,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-4,(button->height-button->pixgrayed_height)-4);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-4,(button->height-button->pix_height)-4);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,(button->height-button->pix_height)-4,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,(button->height-button->pix_height)-4);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-4,(button->height-button->pix_height)-4);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,(button->height-button->pix_height)-4,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-4,(button->height-button->pix_height)-4);
		}

		break;


	  case UserDefinedBitmap:


		XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,button->pixgrayed_x,button->pixgrayed_y);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,button->pixgrayed_x,button->pixgrayed_y,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,button->pixgrayed_x,button->pixgrayed_y);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,button->pix_x,button->pix_y);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,button->pix_x,button->pix_y,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,button->pix_x,button->pix_y);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,button->pix_x,button->pix_y);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,button->pix_x,button->pix_y,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,button->pix_x,button->pix_y);
		}

		break;


	  default: (void)fprintf(stderr,"Button: Gravity inconnue\n");
		break;

	   }
	 }

        /*fprintf(stderr,"Unpushed: text en vue\n");*/

	if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
	{

	 switch(button->txt_gravity){

	  case CenterText:

		 XSetFont(tk_display->display, gc,button->txt_font->fid);
		 XSetBackground(tk_display->display, gc,widptr->colors->bg); 
		 if ((button->state&Grayed)==Grayed){ XSetForeground(tk_display->display, gc, widptr->colors->text_grayed);
			/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
			XSetLineAttributes(tk_display->display,gc,8,LineOnOffDash,CapNotLast,JoinMiter);*/} 
		 else XSetForeground(tk_display->display, gc,widptr->colors->text); 
		 XSetClipRectangles(tk_display->display,gc,2,2,&rect,1,Unsorted);

#ifdef DEBUG
		 fprintf(stderr,"Avant\n");
#endif
		 XDrawString(tk_display->display, button->window, gc,button->width/2- XTextWidth(button->txt_font, button->text, strlen(button->text))/2, (button->height+button->txt_font->ascent)/2, button->text,strlen(button->text));
#ifdef DEBUG
		 fprintf(stderr,"Apres 1   key=%d\n",button->txt_key);
#endif
		 if(button->txt_key>1 && button->txt_key<=strlen(button->text))
			XDrawLine(tk_display->display, button->window, gc,button->width/2-XTextWidth(button->txt_font, button->text,strlen(button->text))/2+XTextWidth(button->txt_font, button->text,button->txt_key-1), (button->height+button->txt_font->ascent)/2+2,button->width/2- XTextWidth(button->txt_font, button->text, strlen(button->text))/2+XTextWidth(button->txt_font, button->text,button->txt_key)-1, (button->height+button->txt_font->ascent)/2+2);
		 else if(button->txt_key==1 && button->txt_key<=strlen(button->text))
			XDrawLine(tk_display->display, button->window, gc,button->width/2-XTextWidth(button->txt_font, button->text,strlen(button->text))/2, (button->height+button->txt_font->ascent)/2+2,button->width/2- XTextWidth(button->txt_font, button->text, strlen(button->text))/2+XTextWidth(button->txt_font, button->text,button->txt_key)-1, (button->height+button->txt_font->ascent)/2+2);
#ifdef DEBUG
		 fprintf(stderr,"Apres 2\n");
#endif
		 break;

	  case UserDefinedText:

		 XSetFont(tk_display->display, gc,button->txt_font->fid);
		 XSetBackground(tk_display->display, gc,widptr->colors->bg); 
		 if ((button->state&Grayed)==Grayed)
		 { 
			XSetForeground(tk_display->display, gc, widptr->colors->text_grayed);
			/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
			XSetLineAttributes(tk_display->display,gc,8,LineOnOffDash,CapNotLast,JoinMiter);*/
		 } 
		 else XSetForeground(tk_display->display, gc,widptr->colors->text); 
		 XSetClipRectangles(tk_display->display,gc,2,2,&rect,1,Unsorted);
		 XDrawString(tk_display->display, button->window, gc,button->txt_x,button->txt_y, button->text,strlen(button->text));
		 if(button->txt_key>1 && button->txt_key<=strlen(button->text))
		     	XDrawLine(tk_display->display, button->window,gc,button->txt_x+XTextWidth(button->txt_font,button->text,button->txt_key-1),button->txt_y+2,button->txt_x+XTextWidth(button->txt_font,button->text,button->txt_key)-1,button->txt_y+2); 
		 else if(button->txt_key==1 && button->txt_key<=strlen(button->text))
		     	XDrawLine(tk_display->display, button->window,gc,button->txt_x,button->txt_y+2,button->txt_x+XTextWidth(button->txt_font,button->text,button->txt_key)-1,button->txt_y+2); 
		 break;

	   }

	 }
#ifdef DEBUG
        fprintf(stderr,"Unpushed free GC\n");
#endif
 	XFreeGC(tk_display->display, gc);
	XSync(tk_display->display,False);
	break;





   case Pushed:

	if(button->lighting==0) XSetWindowBackground(tk_display->display, button->window, widptr->colors->bg);
	else XSetWindowBackground(tk_display->display, button->window, widptr->colors->light);
	XClearWindow(tk_display->display,button->window); 

	XSetForeground(tk_display->display,gc,widptr->colors->shadow);
	XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);

	XDrawLine(tk_display->display,button->window, gc, 0,0,button->width,0);
	XDrawLine(tk_display->display,button->window, gc, 0,1,button->width,1);
	XDrawLine(tk_display->display,button->window, gc, 0,1,0,button->height);
	XDrawLine(tk_display->display,button->window, gc, 1,1,1,button->height);

	XSetForeground(tk_display->display,gc,widptr->colors->light);

	/*XDrawLine(tk_display->display,button->window, gc, 2,button->height-1,button->width,button->height-1);
	XDrawLine(tk_display->display,button->window, gc, 2,button->height-2,button->width,button->height-2);
	XDrawLine(tk_display->display,button->window, gc, button->width-2,2,button->width-2,button->height);
	XDrawLine(tk_display->display,button->window, gc, button->width-1,2,button->width-1,button->height);
 	XDrawPoint(tk_display->display,button->window,gc,1,button->height-1); 
 	XDrawPoint(tk_display->display,button->window,gc,button->width-1,1); */


	gravity=CenterBitmap;
	if ((button->state&Grayed)==Grayed&&(button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
	  gravity=button->pixgrayed_gravity;
        else gravity=button->pix_gravity;

        /*fprintf(stderr,"Pushed flags=%d\n",button->flags);*/

	if((button->flags&PixmapFlag)==PixmapFlag||(button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) switch(gravity)
	{

  	  case CenterBitmap:


		if(button->lighting==0) XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		else XSetBackground(tk_display->display,gc,widptr->colors->light); 
		XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);

		if ((button->state&Grayed)==Grayed) 
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);		  
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)/2+2,(button->height-button->pixgrayed_height)/2+2);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2+2,(button->height-button->pixgrayed_height)/2+2,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2+2,(button->height-button->pixgrayed_height)/2+2);
		 }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)/2+2);}			
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)/2+2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)/2+2);
		 }
		}

		else
		{
		  XSetForeground(tk_display->display,gc,widptr->colors->text);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag) 
		  { 
			XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)/2+2);
		  }
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)/2+2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)/2+2);
		}

		break;


	  case NorthWestBitmap:


		if(button->lighting==0) XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetBackground(tk_display->display,gc,widptr->colors->light); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);		  
			XSetClipOrigin(tk_display->display,gc,4,4);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,4,4,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,4,4);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,4,4);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,4,4,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,4,4);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,4,4);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,4,4,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,4,4);
		}

		break;


	  case WestBitmap:


		if(button->lighting==0) XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		XSetBackground(tk_display->display,gc,widptr->colors->light); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,4,(button->height-button->pixgrayed_height)/2+2);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,4,(button->height-button->pixgrayed_height)/2+2,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,4,(button->height-button->pixgrayed_height)/2+2);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,4,(button->height-button->pix_height)/2+2);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,4,(button->height-button->pix_height)/2+2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,4,(button->height-button->pix_height)/2+2);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,4,(button->height-button->pix_height)/2+2);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,4,(button->height-button->pix_height)/2+2,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,4,(button->height-button->pix_height)/2+2);
		}

		break;


	   case SouthWestBitmap:


		if(button->lighting==0) XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		else XSetBackground(tk_display->display,gc,widptr->colors->light); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);

		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,4,(button->height-button->pixgrayed_height)-2);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,4,(button->height-button->pixgrayed_height)-2,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,4,(button->height-button->pixgrayed_height)-2);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,4,(button->height-button->pix_height)-2);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,4,(button->height-button->pix_height)-2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,4,(button->height-button->pix_height)-2);
		 }
		}

		else
		{
		
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,4,(button->height-button->pix_height)-2);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,4,(button->height-button->pix_height)-2,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,4,(button->height-button->pix_height)-2);
		}

		break;


	   case NorthBitmap:


		if(button->lighting==0) XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		else XSetBackground(tk_display->display,gc,widptr->colors->light); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);		  
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)/2+2,4);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2+2,4,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2+2,4);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2+2,4);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,4,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,4);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2+2,4);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,4,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,4);
		}

		break;


	   case SouthBitmap:


		if(button->lighting==0) XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		else XSetBackground(tk_display->display,gc,widptr->colors->light); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)/2+2,(button->height-button->pixgrayed_height)-2);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2+2,(button->height-button->pixgrayed_height)-2,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)/2+2,(button->height-button->pixgrayed_height)-2);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)-2);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)-2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)-2);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)-2);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)-2,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)/2+2,(button->height-button->pix_height)-2);
		}

		break;


  	  case EastBitmap:


		if(button->lighting==0) XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		else XSetBackground(tk_display->display,gc,widptr->colors->light); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)-2,(button->height-button->pixgrayed_height)/2+2);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-2,(button->height-button->pixgrayed_height)/2+2,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-2,(button->height-button->pixgrayed_height)/2+2);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-2,(button->height-button->pix_height)/2+2);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,(button->height-button->pix_height)/2+2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,(button->height-button->pix_height)/2+2);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-2,(button->height-button->pix_height)/2+2);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,(button->height-button->pix_height)/2+2,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,(button->height-button->pix_height)/2+2);
		}

		break;


  	  case NorthEastBitmap:


		if(button->lighting==0) XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		else XSetBackground(tk_display->display,gc,widptr->colors->light); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 /*fprintf(stderr,"Button Grayed\n");*/

		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)-2,4);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-2,4,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-2,4);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-2,4);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,4,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,4);
		 }
		}

		else
		{
		  /*fprintf(stderr,"Button Ungrayed\n");*/
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag) 
		  { 
			XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-2,4);
		  }
		  /*fprintf(stderr,"Pixmap Flag passe\n");*/
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,4,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,4);
		}

		break;

		
	  case SouthEastBitmap:


		if(button->lighting==0) XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		else XSetBackground(tk_display->display,gc,widptr->colors->light); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pixgrayed_width)-2,(button->height-button->pixgrayed_height)-2);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-2,(button->height-button->pixgrayed_height)-2,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,(button->width-button->pixgrayed_width)-2,(button->height-button->pixgrayed_height)-2);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-2,(button->height-button->pix_height)-2);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,(button->height-button->pix_height)-2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,(button->height-button->pix_height)-2);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,(button->width-button->pix_width)-2,(button->height-button->pix_height)-2);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,(button->height-button->pix_height)-2,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,(button->width-button->pix_width)-2,(button->height-button->pix_height)-2);
		}

		break;


	  case UserDefinedBitmap:


		if(button->lighting==0) XSetBackground(tk_display->display,gc,widptr->colors->bg); 
		else XSetBackground(tk_display->display,gc,widptr->colors->light); 
		XSetForeground(tk_display->display,gc,widptr->colors->text);


		if ((button->state&Grayed)==Grayed)
		{
		 if((button->flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag){ XSetClipMask(tk_display->display,gc,button->pixgrayed_mask);
			XSetClipOrigin(tk_display->display,gc,button->pixgrayed_x+2,button->pixgrayed_y+2);}
		  if (button->pixgrayed_depth==1) XCopyPlane(tk_display->display,button->pixmap_grayed,button->window,gc,0,0,button->pixgrayed_width,button->pixgrayed_height,button->pixgrayed_x+2,button->pixgrayed_y+2,1);
		  else if(button->pixgrayed_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap_grayed,button->window,gc, 0,0,button->pixgrayed_width,button->pixgrayed_height,button->pixgrayed_x+2,button->pixgrayed_y+2);
		  }
		 else
		 {
		  XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
		  if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,button->pix_x+2,button->pix_y+2);}
		  if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,button->pix_x+2,button->pix_y+2,1);
		  else if(button->pix_depth==tk_display->depth)
		    XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,button->pix_x+2,button->pix_y+2);
		 }
		}

		else
		{
		if((button->flags&PixmapMaskFlag)==PixmapMaskFlag){ XSetClipMask(tk_display->display,gc,button->pix_mask);
			XSetClipOrigin(tk_display->display,gc,button->pix_x+2,button->pix_y+2);}
		if (button->pix_depth==1) XCopyPlane(tk_display->display,button->pixmap,button->window,gc,0,0,button->pix_width,button->pix_height,button->pix_x+2,button->pix_y+2,1);
		else if(button->pix_depth==tk_display->depth)
		  XCopyArea(tk_display->display,button->pixmap,button->window,gc, 0,0,button->pix_width,button->pix_height,button->pix_x+2,button->pix_y+2);
		}

		break;

	 }


        /*fprintf(stderr,"Pushed: text en vue %d\n",(button->flags&TextFlag));*/

	if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
	{
	   /*fprintf(stderr,"Text detecte: %s\n",button->text);*/
	   switch(button->txt_gravity)
	   {

	    case CenterText:
	 
		 XSetFont(tk_display->display, gc,button->txt_font->fid);
		 if ((button->state&Grayed)==Grayed)
		 { 
			XSetForeground(tk_display->display, gc, widptr->colors->text_grayed);
			/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
			XSetLineAttributes(tk_display->display,gc,8,LineOnOffDash,CapNotLast,JoinMiter);*/
		 }
		 else XSetForeground(tk_display->display, gc, widptr->colors->text);
		 XSetClipRectangles(tk_display->display,gc,2,2,&rect, 1,Unsorted);
		 XDrawString(tk_display->display, button->window, gc,button->width/2- XTextWidth(button->txt_font, button->text, strlen(button->text))/2+2, (button->height+button->txt_font->ascent)/2+2, button->text,strlen(button->text));
		 if(button->txt_key>1 && button->txt_key<=strlen(button->text))
			XDrawLine(tk_display->display, button->window, gc,button->width/2-XTextWidth(button->txt_font, button->text,strlen(button->text))/2+XTextWidth(button->txt_font, button->text,button->txt_key-1)+2, (button->height+button->txt_font->ascent)/2+4,button->width/2- XTextWidth(button->txt_font, button->text, strlen(button->text))/2+XTextWidth(button->txt_font, button->text,button->txt_key)+1, (button->height+button->txt_font->ascent)/2+4);
		 else if(button->txt_key==1 && button->txt_key<=strlen(button->text))
			XDrawLine(tk_display->display, button->window, gc,button->width/2-XTextWidth(button->txt_font, button->text,strlen(button->text))/2+2, (button->height+button->txt_font->ascent)/2+4,button->width/2- XTextWidth(button->txt_font, button->text, strlen(button->text))/2+XTextWidth(button->txt_font, button->text,button->txt_key)+1, (button->height+button->txt_font->ascent)/2+4);
		 break;


	    case UserDefinedText:
	 
		 XSetFont(tk_display->display, gc,button->txt_font->fid);
		 /*XSetFillStyle(tk_display->display, gc, FillTiled);*/
		 if ((button->state&Grayed)==Grayed){ XSetForeground(tk_display->display, gc, widptr->colors->text_grayed);
			/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
			XSetLineAttributes(tk_display->display,gc,8,LineOnOffDash,CapNotLast,JoinMiter);*/}
		 else XSetForeground(tk_display->display, gc, widptr->colors->text);
		 XSetClipRectangles(tk_display->display,gc,2,2,&rect, 1,Unsorted);
		 XDrawString(tk_display->display, button->window, gc,button->txt_x+2, button->txt_y+2, button->text,strlen(button->text));
		 if(button->txt_key>1 && button->txt_key<=strlen(button->text))
		     	XDrawLine(tk_display->display, button->window,gc,button->txt_x+XTextWidth(button->txt_font,button->text,button->txt_key-1)+2,button->txt_y+4,button->txt_x+XTextWidth(button->txt_font,button->text,button->txt_key)+1,button->txt_y+4); 
		 if(button->txt_key==1 && button->txt_key<=strlen(button->text))
		     	XDrawLine(tk_display->display, button->window,gc,button->txt_x+2,button->txt_y+4,button->txt_x+XTextWidth(button->txt_font,button->text,button->txt_key)+1,button->txt_y+4); 
		 break;

	   }

	 }

        /*fprintf(stderr,"Pushed free GC\n");*/

	XFreeGC(tk_display->display, gc);
	XSync(tk_display->display,False);
	break;

   }

 }
 else fprintf(stderr,"Mauvais type de bouton\n");

}




			/******** CROSS BUTTON ********/





int bn_DrawCrossButton(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 GC gc;
 XGCValues xgcvalues;
 int mask;
 int pos;
 ButtonStruct *button;
 WidgetStruct *widptr;

 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];



 if (button->type==BN_CROSSBUTTON) 
 {
   pos=(button->txt_font->ascent+button->txt_font->descent+6)/2 - CROSSSIZE/2;
   if(pos<0) pos=0;

   mask=GCForeground|GCBackground;
   xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
   xgcvalues.function=GXxor;
   gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);

   switch(button->state&Pushed)
   {

     case Unpushed : 	

	 if(button->hasFocus==NO) XSetForeground(tk_display->display,gc, widptr->colors->nofocus);
	 else XSetForeground(tk_display->display,gc, widptr->colors->focus); 
	 XDrawRectangle(tk_display->display,button->window,gc,0,pos,CROSSSIZE,CROSSSIZE);
	 XSetForeground(tk_display->display,gc, widptr->colors->bg);
	 XFillRectangle(tk_display->display,button->window,gc,1,pos+1,CROSSSIZE-1,CROSSSIZE-1);

			
	 XSetForeground(tk_display->display,gc,widptr->colors->light);
	 XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);

	 XDrawLine(tk_display->display,button->window, gc,1,pos+1,CROSSSIZE-1,pos+1);
	 XDrawLine(tk_display->display,button->window,gc,1,pos+2,CROSSSIZE-1,pos+2); 
	 XDrawLine(tk_display->display,button->window, gc,1,pos+1,1,pos+CROSSSIZE-1);
	 XDrawLine(tk_display->display,button->window, gc,2,pos+1,2,pos+CROSSSIZE-1);
			
	 XSetForeground(tk_display->display, gc, widptr->colors->shadow);
	
	 XDrawLine(tk_display->display,button->window, gc, CROSSSIZE-1,pos+2,CROSSSIZE-1,pos+CROSSSIZE-1);
	 XDrawLine(tk_display->display,button->window, gc, CROSSSIZE-2,pos+3,CROSSSIZE-2,pos+CROSSSIZE-1);
	 XDrawLine(tk_display->display,button->window, gc, 2,pos+CROSSSIZE-1,CROSSSIZE-1,pos+CROSSSIZE-1);
	 XDrawLine(tk_display->display,button->window, gc, 3,pos+CROSSSIZE-2,CROSSSIZE-1,pos+CROSSSIZE-2);
	 XDrawPoint(tk_display->display,button->window, gc,2,pos+CROSSSIZE-1); 
	 XDrawPoint(tk_display->display,button->window,gc,CROSSSIZE-1,pos+2);

	 if(button->hasFocus==NO) {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+CROSSSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,32+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,32,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineOnOffDash,CapButt,JoinMiter);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled); */
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+8,pos*2+CROSSSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,8,pos*2+CROSSSIZE);
	 }

	 XSetFont(tk_display->display, gc,button->txt_font->fid);
	 if((button->state&Grayed)==Grayed) 
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->text_grayed);
		XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetLineAttributes(tk_display->display,gc,4,LineSolid,CapNotLast,JoinMiter);
	 }
	 else XSetForeground(tk_display->display, gc,widptr->colors->text);
	 if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
	   XDrawString(tk_display->display, button->window,gc,button->txt_x,button->txt_y,button->text,strlen(button->text));
	 
	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);
	 break;


     case Pushed: 

	 
	 if(button->hasFocus==NO) XSetForeground(tk_display->display,gc, widptr->colors->nofocus);
	 else  XSetForeground(tk_display->display,gc, widptr->colors->focus);
	 XDrawRectangle(tk_display->display,button->window,gc,0,pos+0,CROSSSIZE,CROSSSIZE);
	 XSetForeground(tk_display->display,gc, widptr->colors->light);
	 XFillRectangle(tk_display->display,button->window,gc,1,pos+1,CROSSSIZE-1,CROSSSIZE-1); 

	 XSetForeground(tk_display->display,gc,widptr->colors->shadow);
	 XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);

	 XDrawLine(tk_display->display,button->window, gc, 1,pos+1,CROSSSIZE-1,pos+1);
	 XDrawLine(tk_display->display,button->window, gc, 1,pos+2,CROSSSIZE-1,pos+2);
	 XDrawLine(tk_display->display,button->window, gc,2,pos+1,2,pos+CROSSSIZE-1); 
	 XDrawLine(tk_display->display,button->window, gc,1,pos+1,1,pos+CROSSSIZE-1); 
	 
	 if ((button->state&Grayed)!=Grayed) XSetForeground(tk_display->display,gc,widptr->colors->cross);
	 else { XSetForeground(tk_display->display,gc,widptr->colors->text_grayed); 
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled);*/
		} 
	
	 if(button->crosstype==BigCross){
	  XDrawLine(tk_display->display,button->window,gc,3,pos+3,CROSSSIZE-1,pos+CROSSSIZE-1); 
	  XDrawLine(tk_display->display,button->window,gc,4,pos+3,CROSSSIZE-1,pos+CROSSSIZE-2); 
	  XDrawLine(tk_display->display,button->window,gc,3,pos+4,CROSSSIZE-2,pos+CROSSSIZE-1); 

	  XDrawLine(tk_display->display,button->window,gc,3,pos+CROSSSIZE-1,CROSSSIZE-1,pos+3); 
	  XDrawLine(tk_display->display,button->window,gc,3,pos+CROSSSIZE-2,CROSSSIZE-2,pos+3); 
	  XDrawLine(tk_display->display,button->window,gc,4,pos+CROSSSIZE-1,CROSSSIZE-1,pos+4); 

	 }
	 else if(button->crosstype==CheckMark){
	  XDrawLine(tk_display->display,button->window,gc,5,pos+9,8,pos+CROSSSIZE-2); 
	  XDrawLine(tk_display->display,button->window,gc,6,pos+9,9,pos+CROSSSIZE-2); 
	  XDrawLine(tk_display->display,button->window,gc,8,pos+CROSSSIZE-3,CROSSSIZE-3,pos+4);
	  XDrawLine(tk_display->display,button->window,gc,9,pos+CROSSSIZE-3,CROSSSIZE-2,pos+4);
	 } 
	 else if(button->crosstype==LittleCross){
	  XDrawLine(tk_display->display,button->window,gc,6,pos+6,CROSSSIZE-3,pos+CROSSSIZE-3); 
	  XDrawLine(tk_display->display,button->window,gc,7,pos+6,CROSSSIZE-3,pos+CROSSSIZE-4); 
	  XDrawLine(tk_display->display,button->window,gc,6,pos+7,CROSSSIZE-4,pos+CROSSSIZE-3); 
	  XDrawLine(tk_display->display,button->window,gc,6,pos+CROSSSIZE-3,CROSSSIZE-3,pos+6); 
	  XDrawLine(tk_display->display,button->window,gc,6,pos+CROSSSIZE-4,CROSSSIZE-4,pos+6); 
	  XDrawLine(tk_display->display,button->window,gc,7,pos+CROSSSIZE-3,CROSSSIZE-3,pos+7); 
	 } 

	 if(button->hasFocus==NO) 
	 {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+CROSSSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,32+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,32,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineOnOffDash,CapButt,JoinMiter);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled); */
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+8,pos*2+CROSSSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,8,pos*2+CROSSSIZE);
	 }

	 XSetFont(tk_display->display, gc,button->txt_font->fid);
	 if((button->state&Grayed)==Grayed)
	 { 
		XSetForeground(tk_display->display, gc,widptr->colors->text_grayed);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled);*/
	 }
	 else XSetForeground(tk_display->display, gc,widptr->colors->text);
	 if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
	   XDrawString(tk_display->display, button->window,gc,button->txt_x,button->txt_y,button->text,strlen(button->text));

	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);	 
	 break;

    }
  }
  else (void)fprintf(stderr,"Erreur: Mauvais type de bouton");
  return 0;

}





int bn_DrawCrossFocus(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 GC gc;
 XGCValues xgcvalues;
 int mask;
 int pos;
 ButtonStruct *button;
 WidgetStruct *widptr;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];



 if (button->type==BN_CROSSBUTTON) 
 {
   pos=(button->txt_font->ascent+button->txt_font->descent+6)/2 - CROSSSIZE/2;
   if(pos<0) pos=0;

   mask=GCForeground|GCBackground;
   xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
   xgcvalues.function=GXxor;
   gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);

   switch(button->state&Pushed)
   {

     case Unpushed : 	

	 if(button->hasFocus==NO) XSetForeground(tk_display->display,gc, widptr->colors->nofocus);
	 else XSetForeground(tk_display->display,gc, widptr->colors->focus); 
	 XDrawRectangle(tk_display->display,button->window,gc,0,pos,CROSSSIZE,CROSSSIZE);


	 if(button->hasFocus==NO) 
	 {
		XClearArea(tk_display->display,button->window,23,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,23,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,23,pos*2+CROSSSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,32+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,32,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled); */
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+8,pos*2+CROSSSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,8,pos*2+CROSSSIZE);
	 }

	 
	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);
	 break;



     case Pushed: 

	 
	 if(button->hasFocus==NO) XSetForeground(tk_display->display,gc, widptr->colors->nofocus);
	 else  XSetForeground(tk_display->display,gc, widptr->colors->focus);
	 XDrawRectangle(tk_display->display,button->window,gc,0,pos+0,CROSSSIZE,CROSSSIZE);

	 if(button->hasFocus==NO) 
	 {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+CROSSSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,32+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,32,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled); */
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+8,pos*2+CROSSSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,8,pos*2+CROSSSIZE);
	 }

	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);
	 break;

    }
  }
  else (void)fprintf(stderr,"Erreur: Mauvais type de bouton");

 return 0;
}




			/******* RADIO BUTTON *******/



int bn_DrawRadioButton(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 GC gc;
 XGCValues xgcvalues;
 int mask;
 int pos;
 ButtonStruct *button;
 WidgetStruct *widptr;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];


 if (button->type==BN_RADIOBUTTON) 
 {

   pos=(button->txt_font->ascent+button->txt_font->descent+6)/2 - RADIOSIZE/2;
   if(pos<0) pos=0;

   mask=GCForeground|GCBackground;
   xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
   xgcvalues.function=GXxor;
   gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);

   switch(button->state&Pushed)
   {

    case Unpushed : 	

	 XClearArea(tk_display->display,button->window,3,pos+3,13,13,False);
	 XSetForeground(tk_display->display,gc,widptr->colors->radio_light);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.radio2);
	 XSetClipOrigin(tk_display->display,gc,-1,pos-1);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.radio2,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
	 XSetForeground(tk_display->display,gc,widptr->colors->shadow);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.radio1);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.radio1,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
	 
	 XFreeGC(tk_display->display, gc);
	 mask=GCForeground|GCBackground;
	 xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
	 xgcvalues.function=GXxor;
	 gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);

	 if(button->hasFocus==NO) 
	 {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+RADIOSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,29+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,29,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled); */
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+6,pos*2+RADIOSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,6,pos*2+RADIOSIZE);
	 }


	 XSetFont(tk_display->display, gc,button->txt_font->fid);
	 if((button->state&Grayed)==Grayed) 
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->text_grayed);
		XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetLineAttributes(tk_display->display,gc,4,LineSolid,CapNotLast,JoinMiter);
	 }
	 else XSetForeground(tk_display->display, gc,widptr->colors->text);
	 if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
	   XDrawString(tk_display->display, button->window,gc,button->txt_x,button->txt_y,button->text,strlen(button->text));
	 
	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);
	 break;



    case Pushed: 

	 XSetForeground(tk_display->display,gc,widptr->colors->radio_light);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.radio2);
	 XSetClipOrigin(tk_display->display,gc,-1,pos-1);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.radio2,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
	 XSetForeground(tk_display->display,gc,widptr->colors->shadow);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.radio1);
	 XSetClipOrigin(tk_display->display,gc,-1,pos-1);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.radio1,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
	 XSetForeground(tk_display->display,gc,widptr->colors->radio_bg);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.radio3);
	 XSetClipOrigin(tk_display->display,gc,-1,pos-1);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.radio3,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
 	 
	 XFreeGC(tk_display->display, gc);
	 mask=GCForeground|GCBackground;
	 xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
	 xgcvalues.function=GXxor;
	 gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);

	 if(button->hasFocus==NO) 
	 {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+RADIOSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,29+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,29,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled); */
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+6,pos*2+RADIOSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,6,pos*2+RADIOSIZE);
	 }

	 XSetFont(tk_display->display, gc,button->txt_font->fid);
	 if((button->state&Grayed)==Grayed)
	 { 
		XSetForeground(tk_display->display, gc,widptr->colors->text_grayed);
		XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled);
	 }
	 else XSetForeground(tk_display->display, gc,widptr->colors->text);
	 if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
	   XDrawString(tk_display->display, button->window,gc,button->txt_x,button->txt_y,button->text,strlen(button->text));
	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);	 
	 break;

    }
  }
  else (void)fprintf(stderr,"Erreur: Mauvais type de bouton");

 return 0;
}





int bn_DrawRadioFocus(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 GC gc;
 XGCValues xgcvalues;
 int mask;
 int pos;
 ButtonStruct *button;
 WidgetStruct *widptr; 


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];


 if (button->type==BN_RADIOBUTTON)
 { 

   pos=(button->txt_font->ascent+button->txt_font->descent+6)/2 - RADIOSIZE/2;
   if(pos<0) pos=0;

   mask=GCForeground|GCBackground;
   xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
   xgcvalues.function=GXxor;
   gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);


   switch(button->state&Pushed)
   {

     case Unpushed : 	


	 if(button->hasFocus==NO) 
	 {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+RADIOSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,29+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,29,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled); */
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+6,pos*2+RADIOSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,6,pos*2+RADIOSIZE);
	 }

	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);
	 break;



    case Pushed: 


	 if(button->hasFocus==NO) 
	 {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+RADIOSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,29+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,29,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled); */
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+6,pos*2+RADIOSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,6,pos*2+RADIOSIZE);
	 }

	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);
	 break;

      }
  }
  else (void)fprintf(stderr,"Erreur: Mauvais type de bouton");

 return 0;

}





			/**** CHECK BUTTON ****/


int bn_DrawCheckButton(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 GC gc;
 XGCValues xgcvalues;
 int mask;
 int pos;
 XPoint *ptr, point[5];
 ButtonStruct *button;
 WidgetStruct *widptr;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];



 if (button->type==BN_CHECKBUTTON) 
  {
   /*fprintf(stderr,"Draw CHECK en cours\n");*/
   if(button->txt_font->ascent+button->txt_font->descent+6>CHECKSIZE)
     pos=(button->txt_font->ascent+button->txt_font->descent+6)/2 - CHECKSIZE/2;
   else pos=0;
   if(pos<0) pos=0;

   if(button->txt_font->ascent+button->txt_font->descent+6>CHECKSIZE)
     button->txt_y = button->txt_font->ascent+3;
   else button->txt_y=button->txt_font->ascent+3+(CHECKSIZE-button->txt_font->ascent-button->txt_font->descent-6)/2;

   point[0].x=1;
   point[0].y=10+pos;
   point[1].x=10;
   point[1].y=1+pos;
   point[2].x=19;
   point[2].y=10+pos;
   point[3].x=10;
   point[3].y=19+pos;


   mask=GCForeground|GCBackground;
   xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
   xgcvalues.function=GXxor;
   gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);



   switch(button->state&Pushed){


    case Unpushed : 	

	 /*fprintf(stderr,"CAS Unpushed\n");*/
	 bn_DrawCheckFocus(tk_display,buttonid);
	 /*fprintf(stderr,"En route\n");*/
	 
	 XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
	 XSetForeground(tk_display->display,gc,widptr->colors->bg);
	 XSetBackground(tk_display->display,gc,widptr->colors->bg);
	 ptr=point;
	 XFillPolygon(tk_display->display,button->window,gc,ptr,4,Nonconvex,CoordModeOrigin); 

	 XSetForeground(tk_display->display,gc,widptr->colors->text);
	 XDrawLine(tk_display->display,button->window,gc,0,10+pos,10,0+pos);
	 XDrawLine(tk_display->display,button->window,gc,0,10+pos,10,20+pos);
	 XDrawLine(tk_display->display,button->window,gc,10,0+pos,20,10+pos);
	 XDrawLine(tk_display->display,button->window,gc,10,20+pos,20,10+pos);

	  
	 XSetForeground(tk_display->display,gc,widptr->colors->light);
	 XDrawLine(tk_display->display,button->window, gc, 1,10+pos,10,1+pos);
	 XDrawLine(tk_display->display,button->window, gc, 2,10+pos,10,2+pos);
	 XDrawLine(tk_display->display,button->window, gc, 2,11+pos,10,19+pos);
	 XDrawLine(tk_display->display,button->window, gc, 3,11+pos,10,18+pos);

			/********** Ombre **********/

	 XSetForeground(tk_display->display, gc, widptr->colors->shadow);
	 XDrawLine(tk_display->display,button->window, gc,11,2+pos,19,10+pos);
	 XDrawLine(tk_display->display,button->window, gc,10,2+pos,18,10+pos);
	 XDrawLine(tk_display->display,button->window, gc, 11,18+pos,19,10+pos);
	 XDrawLine(tk_display->display,button->window, gc, 11,17+pos,18,10+pos);	 

	 /*fprintf(stderr,"Texte en vue\n");*/
	 XSetFont(tk_display->display, gc,button->txt_font->fid);
	 if((button->state&Grayed)==Grayed) 
	 { 	
		XSetForeground(tk_display->display, gc,widptr->colors->text_grayed);
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
	 }
	 else XSetForeground(tk_display->display, gc,widptr->colors->text);
	 if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
	 {
	   /*fprintf(stderr,"Texte parti\n");*/
	   XDrawString(tk_display->display, button->window,gc,button->txt_x,button->txt_y,button->text,strlen(button->text));
	 }
	 /*fprintf(stderr,"Free GC\n");*/
	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);
	 break;



    case Pushed: 

	 /*fprintf(stderr,"CAS Pushed\n");*/
	 bn_DrawCheckFocus(tk_display,buttonid);
	 /*fprintf(stderr,"En route\n");*/
	 XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
	 XSetBackground(tk_display->display,gc,widptr->colors->light);
	 XSetForeground(tk_display->display,gc,widptr->colors->light);
	 ptr=point;
	 XFillPolygon(tk_display->display,button->window,gc,ptr,4,Nonconvex,CoordModeOrigin); 

	 XSetForeground(tk_display->display,gc,widptr->colors->text);
	 XDrawLine(tk_display->display,button->window,gc,0,10+pos,10,0+pos);
	 XDrawLine(tk_display->display,button->window,gc,0,10+pos,10,20+pos);
	 XDrawLine(tk_display->display,button->window,gc,10,0+pos,20,10+pos);
	 XDrawLine(tk_display->display,button->window,gc,10,20+pos,20,10+pos);
	  
	 XSetForeground(tk_display->display,gc,widptr->colors->shadow);
	 XDrawLine(tk_display->display,button->window, gc, 1,10+pos,10,1+pos);
	 XDrawLine(tk_display->display,button->window, gc, 2,10+pos,10,2+pos);
	 XDrawLine(tk_display->display,button->window, gc, 2,11+pos,10,19+pos);
	 XDrawLine(tk_display->display,button->window, gc, 3,11+pos,10,18+pos);	 	 


	 /*fprintf(stderr,"Texte en vue\n");*/
	 XSetFont(tk_display->display, gc,button->txt_font->fid);
	 if((button->state&Grayed)==Grayed)
	 { 	XSetForeground(tk_display->display, gc,widptr->colors->text);
		/*XSetStipple(tk_display->display,gc,graytile_pixmap);*/
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
	 }
	 else XSetForeground(tk_display->display, gc,widptr->colors->text);
	 if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
	 {
	   /*fprintf(stderr,"Texte parti\n");*/
	   XDrawString(tk_display->display, button->window,gc,button->txt_x,button->txt_y,button->text,strlen(button->text));
	}

	 /*fprintf(stderr,"Free GC\n");*/
	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);
	 break;

      }
  }
  else (void)fprintf(stderr,"Erreur: Mauvais type de bouton");

 return 0;

}





int bn_DrawCheckFocus(tk_display, buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 GC gc;
 XGCValues xgcvalues;
 int mask;
 int pos;
 ButtonStruct *button;
 WidgetStruct *widptr;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];


 if (button->type==BN_CHECKBUTTON) 
 {

   pos=(button->txt_font->ascent+button->txt_font->descent+6)/2 - CHECKSIZE/2;
   if(pos<0) pos=0;

   mask=GCForeground|GCBackground;
   xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
   xgcvalues.function=GXxor;
   gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);
 
   switch(button->state&Pushed)
   {

   case Unpushed : 	


	 if(button->hasFocus==NO) 
	 {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+CHECKSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,29+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,29,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES){
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+6,pos*2+CHECKSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,6,pos*2+CHECKSIZE);
	 }

	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);
	 break;



   case Pushed: 


	 if(button->hasFocus==NO) 
	 {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+CHECKSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,29+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,29,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+6,pos*2+CHECKSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,6,pos*2+CHECKSIZE);
	 }

	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);
	 break;

      }
  }
  else (void)fprintf(stderr,"Erreur: Mauvais type de bouton");

 return 0;

}






			/******* POPUP RADIO BUTTON *******/


int bn_DrawPopupRadioButton(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 GC gc;
 XGCValues xgcvalues;
 int mask;
 int pos;
 ButtonStruct *button;
 WidgetStruct *widptr;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];



 if (button->type==BN_POPUPRADIOBUTTON) 
 {
   /*fprintf(stderr,"Draw POPUPRADIO\n");*/
   pos=(button->txt_font->ascent+button->txt_font->descent+6)/2 - RADIOSIZE/2;
   if(pos<0) pos=0;

   mask=GCForeground|GCBackground;
   xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
   xgcvalues.function=GXxor;
   gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);

   /*fprintf(stderr,"state=%d\n",button->state&Pushed);*/
    
    switch(button->state&Pushed)
    {

    case Unpushed : 	

/*	 XClearArea(tk_display->display,button->window,3,pos+3,13,13,False);*/
	 XSetForeground(tk_display->display,gc,widptr->colors->text);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.popup1);
	 XSetClipOrigin(tk_display->display,gc,-1,pos-1);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.popup1,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
	 XSetForeground(tk_display->display,gc,widptr->colors->shadow);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.popup4);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.popup4,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
	 XSetForeground(tk_display->display,gc,widptr->colors->light);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.popup3);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.popup3,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
	 XSetForeground(tk_display->display,gc,widptr->colors->bg);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.popup2);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.popup2,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);

	 /*fprintf(stderr,"Mask passe\n");*/

	 XFreeGC(tk_display->display, gc);
	 mask=GCForeground|GCBackground;
	 xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
	 xgcvalues.function=GXxor;
	 gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);

	 /*fprintf(stderr,"GC passe\n");*/

	 if(button->hasFocus==NO) 
	 {
		/*fprintf(stderr,"hasFocus==NO\n");*/
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+RADIOSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,29+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,29,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		/*fprintf(stderr,"hasFocus==YES\n");*/
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+6,pos*2+RADIOSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,6,pos*2+RADIOSIZE);
	 }
	 /*fprintf(stderr,"Texte en vue\n");*/

	 XSetFont(tk_display->display, gc,button->txt_font->fid);	
	 if((button->state&Grayed)==Grayed) 
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->text_grayed);
		XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetLineAttributes(tk_display->display,gc,4,LineSolid,CapNotLast,JoinMiter);
	 }
	 else XSetForeground(tk_display->display, gc,widptr->colors->text);
	 if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
	   XDrawString(tk_display->display, button->window,gc,button->txt_x,button->txt_y,button->text,strlen(button->text));

	 /*fprintf(stderr,"Texte passe\n");*/
	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);
	 break;



    case Pushed: 

/*	 XClearArea(tk_display->display,button->window,3,pos+3,13,13,False);*/
	 XSetForeground(tk_display->display,gc,widptr->colors->text);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.popup1);
	 XSetClipOrigin(tk_display->display,gc,-1,pos-1);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.popup1,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
	 XSetForeground(tk_display->display,gc,widptr->colors->light);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.popup4);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.popup4,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
	 XSetForeground(tk_display->display,gc,widptr->colors->shadow);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.popup3);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.popup3,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
	 XSetForeground(tk_display->display,gc,widptr->colors->light);
	 XSetClipMask(tk_display->display,gc,tk_display->pixmaps.popup2);
	 XCopyPlane(tk_display->display,tk_display->pixmaps.popup2,button->window,gc,1,1,RADIOSIZE,RADIOSIZE,0,pos,1);
 	 
	 XFreeGC(tk_display->display, gc);
	 mask=GCForeground|GCBackground;
	 xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
	 xgcvalues.function=GXxor;
	 gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);

	 if(button->hasFocus==NO) {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+RADIOSIZE-2,button->width,button->height,False);				
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,29+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,29,0,button->width,button->height,False);
		}
	 else if(button->hasFocus==YES){
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled); */
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+6,pos*2+RADIOSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,6,pos*2+RADIOSIZE);
		}

	 XSetFont(tk_display->display, gc,button->txt_font->fid);
	 if((button->state&Grayed)==Grayed){ XSetForeground(tk_display->display, gc,widptr->colors->text_grayed);
		XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled);
		}
	 else XSetForeground(tk_display->display, gc,widptr->colors->text);
	 if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
	   XDrawString(tk_display->display, button->window,gc,button->txt_x,button->txt_y,button->text,strlen(button->text));

	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);	 
	 break;

      }
  }
  else (void)fprintf(stderr,"Erreur: Mauvais type de bouton");

}





int bn_DrawPopupRadioFocus(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 GC gc;
 XGCValues xgcvalues;
 int mask;
 int pos;
 ButtonStruct *button;
 WidgetStruct *widptr;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];


 if (button->type==BN_POPUPRADIOBUTTON)
 {

   pos=(button->txt_font->ascent+button->txt_font->descent+6)/2 - RADIOSIZE/2;
   if(pos<0) pos=0;

   mask=GCForeground|GCBackground;
   xgcvalues.foreground= xgcvalues.background=BlackPixel(tk_display->display, tk_display->screen);
   xgcvalues.function=GXxor;
   gc=XCreateGC(tk_display->display, button->window, mask, &xgcvalues);

 
   switch(button->state&Pushed)
   {


    case Unpushed : 	


	 if(button->hasFocus==NO) 
	 {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+RADIOSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,29+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,29,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled); */
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+6,pos*2+RADIOSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,6,pos*2+RADIOSIZE);
	 }

	 XFreeGC(tk_display->display, gc);
	 XSync(tk_display->display,False);

	 break;



    case Pushed: 


	 if(button->hasFocus==NO) 
	 {
		XClearArea(tk_display->display,button->window,24,0,button->width,2,False);
		XClearArea(tk_display->display,button->window,24,0,4,button->height,False);
		XClearArea(tk_display->display,button->window,24,pos*2+RADIOSIZE-2,button->width,button->height,False);
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XClearArea(tk_display->display,button->window,29+XTextWidth(button->txt_font,button->text,strlen(button->text)),0,button->width,button->height,False);
		else XClearArea(tk_display->display,button->window,29,0,button->width,button->height,False);
	 }
	 else if(button->hasFocus==YES)
	 {
		XSetForeground(tk_display->display, gc,widptr->colors->focus);
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapButt,JoinMiter);
		/*XSetStipple(tk_display->display,gc,tk_display->pixmaps.graytile);
		XSetFillStyle(tk_display->display,gc,FillStippled); */
		if((button->flags&TextFlag)==TextFlag&&button->text!=NULL&&strlen(button->text)>0)
		  XDrawRectangle(tk_display->display,button->window,gc,24,0,XTextWidth(button->txt_font,button->text,strlen(button->text))+6,pos*2+RADIOSIZE);
		else XDrawRectangle(tk_display->display,button->window,gc,24,0,6,pos*2+RADIOSIZE);
	 }

	 XFreeGC(tk_display->display, gc);
 	 XSync(tk_display->display,False);

	 break;

      }
  }
  else (void)fprintf(stderr,"Erreur: Mauvais type de bouton");


}




