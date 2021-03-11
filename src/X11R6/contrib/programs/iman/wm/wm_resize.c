/*
 *
 * 	wm_resize.c
 * 	redimensionnement des fenetres
 *
 * 	Modification :  19/12/93
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

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"






int WIN_ResizeClient(number,width,height)
unsigned int number;
unsigned int width, height;
{
 XWindowChanges xwc;
 int i,j;
 int mask;
 XRectangle rectangle;
 XEvent send_event;


 if(number<0 || number>=maxwindows || windows[number].isUsed==False)
   return -1;

 switch(windows[number].class)
 {

   case TOP_LEVEL_WINDOW :

		/*** Verifier les dimensions ***/
	WIN_GetAddDimensions(number);
	if(windows[number].state.isZoomed==False)
	{
  	  if(width+windows[number].normal.add_leftside+windows[number].normal.add_rightside>windows[number].normal.max_width) width=windows[number].normal.max_width-(windows[number].normal.add_leftside+windows[number].normal.add_rightside);
	  if(width+windows[number].normal.add_leftside+windows[number].normal.add_rightside<windows[number].normal.min_width) width=windows[number].normal.min_width-(windows[number].normal.add_leftside+windows[number].normal.add_rightside);
	  if(height+windows[number].normal.add_top+windows[number].normal.add_bottom>windows[number].normal.max_height) height=windows[number].normal.max_height-(windows[number].normal.add_top+windows[number].normal.add_bottom);
	  if(height+windows[number].normal.add_top+windows[number].normal.add_bottom<windows[number].normal.min_height) height=windows[number].normal.min_height-(windows[number].normal.add_top+windows[number].normal.add_bottom);

    	  windows[number].normal.client_y=windows[number].normal.add_top;
    	  windows[number].normal.client_x=windows[number].normal.add_leftside;
    	  windows[number].zoom.client_y=windows[number].zoom.add_top;
    	  windows[number].zoom.client_x=windows[number].zoom.add_leftside;

			/*** Remettre les bonnes dimensions ***/

	  windows[number].normal.client_width=width;
	  windows[number].normal.client_height=height; 
	  windows[number].normal.width=width+windows[number].normal.add_leftside+windows[number].normal.add_rightside;
	  windows[number].normal.height=height+windows[number].normal.add_top+windows[number].normal.add_bottom; 

	  xwc.width=windows[number].normal.width;
	  xwc.height=windows[number].normal.height;
	  xwc.border_width=windows[number].normal.border/2;
	  mask=CWWidth|CWHeight|CWBorderWidth;
	  XConfigureWindow(tk_display->display,windows[number].mainwindow,mask,&xwc);
	  xwc.y=windows[number].normal.client_y;
	  xwc.x=windows[number].normal.client_x;
	  xwc.width=windows[number].normal.client_width;
	  xwc.height=windows[number].normal.client_height;
	  mask=CWX|CWY|CWWidth|CWHeight;
  	  XConfigureWindow(tk_display->display,windows[number].clientwindow,mask,&xwc);  
	  

  	  xwc.width=windows[number].normal.width;
  	  mask=CWWidth;
  	  XConfigureWindow(tk_display->display,windows[number].topbar,mask,&xwc);
  	  xwc.y=windows[number].normal.height-windows[number].normal.add_bottom;
  	  mask=CWWidth|CWY;
  	  XConfigureWindow(tk_display->display,windows[number].bottombar,mask,&xwc);

	  xwc.height=windows[number].normal.height;
	  mask=CWHeight;
	  XConfigureWindow(tk_display->display,windows[number].leftbar,mask,&xwc);
	  xwc.x=windows[number].normal.width-windows[number].normal.add_rightside;
	  mask=CWHeight|CWX;
	  XConfigureWindow(tk_display->display,windows[number].rightbar,mask,&xwc);

  	  xwc.x=windows[number].normal.width-14;
  	  mask=CWX;
	  XConfigureWindow(tk_display->display,windows[number].toprightbox,mask,&xwc);
	  xwc.y=windows[number].normal.height-14;
	  mask=CWY;
	  XConfigureWindow(tk_display->display,windows[number].bottomleftbox,mask,&xwc);
	  xwc.x=windows[number].normal.width-14;
	  xwc.y=windows[number].normal.height-14;
	  mask=CWX|CWY;
	  XConfigureWindow(tk_display->display,windows[number].bottomrightbox,mask,&xwc);
 

	  if(windows[number].identity.shape==True&&(windows[number].attributes&Border)!=Border)
    	    xwc.width=windows[number].normal.client_width-2;
          else 
	    xwc.width=windows[number].normal.client_width;
    	  if(wm_info.wm_style==LIGHTDECORATION)
    	  {
	    xwc.x=-1;
	    xwc.y=-1;
    	  }
    	  else if(wm_info.wm_style==FULLDECORATION)
    	  {
  	    if((windows[number].attributes&Border)==Border) 
	    {	
	      xwc.x=BORDERSIZE;
	      xwc.y=BORDERSIZE;
	    }
	    else
	    {
	      if(windows[number].identity.shape==True)
	        xwc.y=xwc.x=0;
	      else
	        xwc.y=xwc.x=-1;
	    }
    	  }
	  mask=CWWidth|CWX|CWY;
	  XConfigureWindow(tk_display->display,windows[number].titlebar,mask,&xwc);

#ifdef HAS_SHAPE_EXTENSION_LIBRARY
	  if(windows[number].identity.shape==True)
	  {
#ifdef DEBUG
		fprintf(stderr,"Has Shape\n");
#endif
	  	XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,None,0);
	  	XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,None,0);
	  	rectangle.x=windows[number].normal.client_x;	  
	  	rectangle.y=windows[number].normal.client_y;
	  	rectangle.width=windows[number].normal.client_width;
	  	rectangle.height=windows[number].normal.client_height;
	  	XShapeCombineRectangles(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,&rectangle,1,ShapeSubtract,Unsorted);
	  	XShapeCombineRectangles(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,&rectangle,1,ShapeSubtract,Unsorted);
																							
	  	XShapeCombineShape(tk_display->display,windows[number].mainwindow,ShapeBounding,windows[number].normal.client_x,windows[number].normal.client_y,windows[number].clientwindow,ShapeBounding,ShapeUnion);
	   	XShapeCombineShape(tk_display->display,windows[number].mainwindow,ShapeClip,windows[number].normal.client_x,windows[number].normal.client_y,windows[number].clientwindow,ShapeBounding,ShapeUnion);
	  }
#else
	  fprintf(stderr,"Pas de shape extension\n");
#endif

	}
	else
	{
  	  if(width+windows[number].zoom.add_leftside+windows[number].zoom.add_rightside>windows[number].zoom.max_width) width=windows[number].zoom.max_width-(windows[number].zoom.add_leftside+windows[number].zoom.add_rightside);
	  if(width+windows[number].zoom.add_leftside+windows[number].zoom.add_rightside<windows[number].zoom.min_width) width=windows[number].zoom.min_width-(windows[number].zoom.add_leftside+windows[number].zoom.add_rightside);
	  if(height+windows[number].zoom.add_top+windows[number].zoom.add_bottom>windows[number].zoom.max_height) height=windows[number].zoom.max_height-(windows[number].zoom.add_bottom+windows[number].zoom.add_top);
	  if(height+windows[number].zoom.add_top+windows[number].zoom.add_bottom<windows[number].zoom.min_height) height=windows[number].zoom.min_height-(windows[number].zoom.add_bottom+windows[number].zoom.add_top);
																					
    	  windows[number].normal.client_y=windows[number].normal.add_top;
    	  windows[number].normal.client_x=windows[number].normal.add_leftside;
    	  windows[number].zoom.client_y=windows[number].zoom.add_top;
    	  windows[number].zoom.client_x=windows[number].zoom.add_leftside;

			/*** Remettre les bonnes dimensions ***/

	  windows[number].zoom.client_width=width;
	  windows[number].zoom.client_height=height; 
	  windows[number].zoom.width=width+windows[number].zoom.add_leftside+windows[number].zoom.add_rightside;
	  windows[number].zoom.height=height+windows[number].zoom.add_top+windows[number].zoom.add_bottom; 

	  xwc.width=windows[number].zoom.width;
	  xwc.height=windows[number].zoom.height;
	  xwc.border_width=windows[number].zoom.border/2;
	  mask=CWWidth|CWHeight|CWBorderWidth;
	  XConfigureWindow(tk_display->display,windows[number].mainwindow,mask,&xwc);
	  xwc.y=windows[number].zoom.client_y;
	  xwc.x=windows[number].zoom.client_x;
	  xwc.width=windows[number].zoom.client_width;
	  xwc.height=windows[number].zoom.client_height;
	  mask=CWX|CWY|CWWidth|CWHeight;
  	  XConfigureWindow(tk_display->display,windows[number].clientwindow,mask,&xwc);  

  	  xwc.width=windows[number].zoom.width;
	  if(windows[number].identity.shape==True)
	    xwc.width=windows[number].zoom.width-2;
	  if(windows[number].identity.shape==False)
	  {
	    xwc.x=windows[number].zoom.add_leftside-1;
	    xwc.y=windows[number].zoom.add_top-1;
	  }
	  else
	  {
	    xwc.x=windows[number].zoom.add_leftside;
	    xwc.y=windows[number].zoom.add_top;
	  }
	  mask=CWWidth;
	  XConfigureWindow(tk_display->display,windows[number].titlebar,mask,&xwc);

#ifdef HAS_SHAPE_EXTENSION_LIBRARY
	  if(windows[number].identity.shape==True)
	  {
	  	XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,None,0);
	  	XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,None,0);
	  	rectangle.x=windows[number].zoom.client_x;	  
	  	rectangle.y=windows[number].zoom.client_y;
	  	rectangle.width=windows[number].zoom.client_width;
	  	rectangle.height=windows[number].zoom.client_height;
	  	XShapeCombineRectangles(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,&rectangle,1,ShapeSubtract,Unsorted);
	  	XShapeCombineRectangles(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,&rectangle,1,ShapeSubtract,Unsorted);
																							
	  	XShapeCombineShape(tk_display->display,windows[number].mainwindow,ShapeBounding,windows[number].zoom.client_x,windows[number].zoom.client_y,windows[number].clientwindow,ShapeBounding,ShapeUnion);
	   	XShapeCombineShape(tk_display->display,windows[number].mainwindow,ShapeClip,windows[number].zoom.client_x,windows[number].zoom.client_y,windows[number].clientwindow,ShapeBounding,ShapeUnion);
	  }
#endif

	}

	xwc.x=width-22;
	if(windows[number].identity.shape==True&&windows[number].state.isZoomed==True)
  	  xwc.x=width-24;
	if(windows[number].identity.shape==True&&windows[number].state.isZoomed==False&&(windows[number].attributes&Border)!=Border)
  	  xwc.x=width-24;
  	mask=CWX;
  	wid_Configure(tk_display,windows[number].bn_close,xwc.x,0,0,0,CFX);
	if(windows[number].state.isOnTop==True)
	  WIN_DrawActive(number);
	else WIN_DrawUnactive(number);


        send_event.type=ConfigureNotify;
  	send_event.xconfigure.send_event=True;
	send_event.xconfigure.window=windows[number].clientwindow;
	if(windows[number].state.isZoomed==False)
	{
	      	send_event.xconfigure.x=windows[number].normal.x;
	      	send_event.xconfigure.y=windows[number].normal.y;		  	
	      	send_event.xconfigure.width=windows[number].normal.client_width;
	      	send_event.xconfigure.height=windows[number].normal.client_height;
		send_event.xconfigure.border_width=windows[number].normal.border/2;
	}
	if(windows[number].state.isZoomed==True)
	{
	      	send_event.xconfigure.x=windows[number].zoom.x;
	      	send_event.xconfigure.y=windows[number].zoom.y;		  	
	      	send_event.xconfigure.width=windows[number].zoom.client_width;
	      	send_event.xconfigure.height=windows[number].zoom.client_height;
		send_event.xconfigure.border_width=windows[number].normal.border/2;
	}
	if(windows[number].clientwindow!=wm_clipboard_window)
	  XSendEvent(tk_display->display,windows[number].clientwindow,False,0,&send_event);


 	return 0;
	break;



    case DIALOG_BOX :

	WIN_GetAddDimensions(number);
  	if(width+windows[number].normal.add_leftside+windows[number].normal.add_rightside>windows[number].normal.max_width) width=windows[number].normal.max_width-(windows[number].normal.add_leftside+windows[number].normal.add_rightside);
	if(width+windows[number].normal.add_leftside+windows[number].normal.add_rightside<windows[number].normal.min_width) width=windows[number].normal.min_width-(windows[number].normal.add_leftside+windows[number].normal.add_rightside);
	if(height+windows[number].normal.add_top+windows[number].normal.add_bottom>windows[number].normal.max_height) height=windows[number].normal.max_height-(windows[number].normal.add_top+windows[number].normal.add_bottom);
	if(height+windows[number].normal.add_top+windows[number].normal.add_bottom<windows[number].normal.min_height) height=windows[number].normal.min_height-(windows[number].normal.add_top+windows[number].normal.add_bottom);

			/*** Remettre les bonnes dimensions ***/

	windows[number].normal.client_width=width;
	windows[number].normal.client_height=height; 
	windows[number].normal.width=width+windows[number].normal.add_leftside+windows[number].normal.add_rightside;
	windows[number].normal.height=height+windows[number].normal.add_top+windows[number].normal.add_bottom; 

	XResizeWindow(tk_display->display,windows[number].mainwindow,windows[number].normal.width,windows[number].normal.height);
  	XResizeWindow(tk_display->display,windows[number].clientwindow,width,height);  


        xwc.width=windows[number].normal.width-2;
      	xwc.border_width=1;
  
  	mask=CWWidth|CWBorderWidth;
  	XConfigureWindow(tk_display->display,windows[number].titlebar,mask,&xwc);

  	xwc.x=width-22-2;
  	mask=CWX;
  	wid_Configure(tk_display,windows[number].bn_close,xwc.x,0,0,0,CFX);

#ifdef HAS_SHAPE_EXTENSION_LIBRARY
	  if(windows[number].identity.shape==True)
	  {
#ifdef DEBUG
		fprintf(stderr,"Has Shape\n");
#endif
	  	XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,None,0);
	  	XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,None,0);
	  	rectangle.x=windows[number].normal.client_x;	  
	  	rectangle.y=windows[number].normal.client_y;
	  	rectangle.width=windows[number].normal.client_width;
	  	rectangle.height=windows[number].normal.client_height;
	  	XShapeCombineRectangles(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,&rectangle,1,ShapeSubtract,Unsorted);
	  	XShapeCombineRectangles(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,&rectangle,1,ShapeSubtract,Unsorted);
																							
	  	XShapeCombineShape(tk_display->display,windows[number].mainwindow,ShapeBounding,windows[number].normal.client_x,windows[number].normal.client_y,windows[number].clientwindow,ShapeBounding,ShapeUnion);
	   	XShapeCombineShape(tk_display->display,windows[number].mainwindow,ShapeClip,windows[number].normal.client_x,windows[number].normal.client_y,windows[number].clientwindow,ShapeBounding,ShapeUnion);
	  }
#else
	fprintf(stderr,"Pas de shape extension\n");
#endif
	if(windows[number].state.isOnTop==True)
	  WIN_DrawActive(number);
	else WIN_DrawUnactive(number);

        send_event.type=ConfigureNotify;
  	send_event.xconfigure.send_event=True;
	send_event.xconfigure.window=windows[number].clientwindow;
	if(windows[number].state.isZoomed==False)
	{
	      	send_event.xconfigure.x=windows[number].normal.x;
	      	send_event.xconfigure.y=windows[number].normal.y;		  	
	      	send_event.xconfigure.width=windows[number].normal.client_width;
	      	send_event.xconfigure.height=windows[number].normal.client_height;
		send_event.xconfigure.border_width=windows[number].normal.border/2;
	}
	if(windows[number].state.isZoomed==True)
	{
	      	send_event.xconfigure.x=windows[number].zoom.x;
	      	send_event.xconfigure.y=windows[number].zoom.y;		  	
	      	send_event.xconfigure.width=windows[number].zoom.client_width;
	      	send_event.xconfigure.height=windows[number].zoom.client_height;
		send_event.xconfigure.border_width=windows[number].normal.border/2;
	}
	if(windows[number].clientwindow!=wm_kill_window&&windows[number].clientwindow!=wm_desktop_window&&windows[number].clientwindow!=wm_colors_window&&windows[number].clientwindow!=wm_process_window&&windows[number].clientwindow!=wm_about_window&&windows[number].clientwindow!=wm_setup_window&&windows[number].clientwindow!=wm_end_window) 	
	  XSendEvent(tk_display->display,windows[number].clientwindow,False,0,&send_event);


	return 0;
	break;

  }

}





int WIN_Resize(number,width,height)
unsigned int number;
unsigned int width, height;
{
 XWindowChanges xwc;
 int i,j;
 int mask;



  if(number<0 || number>=maxwindows || windows[number].isUsed==False)
    return -1;

  if(windows[number].state.isZoomed==False)
    return WIN_ResizeClient(number,width-windows[number].normal.add_leftside-windows[number].normal.add_rightside,height-windows[number].normal.add_top-windows[number].normal.add_bottom);
  else return WIN_ResizeClient(number,width-windows[number].zoom.add_leftside-windows[number].zoom.add_rightside,height-windows[number].zoom.add_top-windows[number].zoom.add_bottom);
}






