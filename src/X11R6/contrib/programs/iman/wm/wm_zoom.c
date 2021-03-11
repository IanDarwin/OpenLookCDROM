/*
 *
 * 	wm_zoom.c
 * 	zoom/unzoom des fenetres
 *
 * 	Modification :  11/11/93
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"






int WIN_Zoom(number)
unsigned int number;
{
  XWindowChanges xwc;
  XRectangle rectangle;
  int i,j;
  int mask;



  switch(windows[number].class)
  {

    case TOP_LEVEL_WINDOW :


			/*** Remettre les bonnes dimensions ***/
	
	WIN_GetAddDimensions(number);
    	windows[number].zoom.client_y=windows[number].zoom.add_top;
    	windows[number].zoom.client_x=windows[number].zoom.add_leftside;

	xwc.width=windows[number].zoom.width;
	if(windows[number].identity.shape==True)
  	  xwc.width=windows[number].zoom.width-2;
	
	if(windows[number].identity.shape==True)
	{
  	  xwc.x=0;
  	  xwc.y=0;
	}
	else if(windows[number].identity.shape==False)
	{
  	  xwc.x=-1;
  	  xwc.y=-1;
	}
  	mask=CWWidth|CWX|CWY;
  	XConfigureWindow(tk_display->display,windows[number].titlebar,mask,&xwc);

	xwc.x=windows[number].zoom.x;
	xwc.y=windows[number].zoom.y;
	xwc.width=windows[number].zoom.width;
	xwc.height=windows[number].zoom.height;
	xwc.border_width=windows[number].zoom.border/2;
	mask=CWWidth|CWX|CWY|CWHeight|CWBorderWidth;
  	XConfigureWindow(tk_display->display,windows[number].mainwindow,mask,&xwc);

	xwc.x=windows[number].zoom.client_x;
	xwc.y=windows[number].zoom.client_y;
	xwc.width=windows[number].zoom.client_width;
	xwc.height=windows[number].zoom.client_height;
	mask=CWWidth|CWX|CWY|CWHeight;
  	XConfigureWindow(tk_display->display,windows[number].clientwindow,mask,&xwc);  

	xwc.x=windows[number].zoom.width-22;
	if(windows[number].identity.shape==True) 
	  xwc.x=windows[number].zoom.width-24;
  	mask=CWX;
  	wid_Configure(tk_display,windows[number].bn_close,xwc.x,0,0,0,CFX);

  	XUnmapWindow(tk_display->display,windows[number].topbar);
  	XUnmapWindow(tk_display->display,windows[number].bottombar);
  	XUnmapWindow(tk_display->display,windows[number].leftbar);
  	XUnmapWindow(tk_display->display,windows[number].rightbar);

  	XUnmapWindow(tk_display->display,windows[number].topleftbox);
  	XUnmapWindow(tk_display->display,windows[number].toprightbox);
  	XUnmapWindow(tk_display->display,windows[number].bottomleftbox);
  	XUnmapWindow(tk_display->display,windows[number].bottomrightbox);

  	if(number==wm_clipboard_index)
  	{
  	  XClearWindow(tk_display->display,wm_clipboard_window); 
  	  tk_event.event.xexpose.x=tk_event.event.xexpose.y=0;
  	  tk_event.event.xexpose.width=windows[number].zoom.width;
  	  tk_event.event.xexpose.height=windows[number].zoom.height;
  	  CLIP_Draw();
  	}

 	windows[number].state.isZoomed=True;
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

 	WIN_SetClientState(number,ZoomState);
 	return 0;
	break;



   default :
	WIN_GetAddDimensions(number);
	return 0;
	break;

 }
}





int WIN_Unzoom(number)
unsigned int number;
{
 XWindowChanges xwc;
 XRectangle rectangle;
 int i,j;
 int mask;


 switch(windows[number].class)
 {

   case TOP_LEVEL_WINDOW :

	WIN_GetAddDimensions(number);
    	windows[number].normal.client_y=windows[number].normal.add_top;
    	windows[number].normal.client_x=windows[number].normal.add_leftside;
											
	xwc.x=windows[number].normal.x;
	xwc.y=windows[number].normal.y;
	xwc.width=windows[number].normal.width;
	xwc.height=windows[number].normal.height;
	xwc.border_width=windows[number].normal.border/2;
	mask=CWWidth|CWX|CWY|CWHeight|CWBorderWidth;
  	XConfigureWindow(tk_display->display,windows[number].mainwindow,mask,&xwc);
	xwc.x=windows[number].normal.client_x;
	xwc.y=windows[number].normal.client_y;
	xwc.width=windows[number].normal.client_width;
	xwc.height=windows[number].normal.client_height;
	xwc.border_width=windows[number].normal.border/2;
	mask=CWWidth|CWX|CWY|CWHeight;
  	XConfigureWindow(tk_display->display,windows[number].clientwindow,mask,&xwc);  

  	xwc.width=windows[number].normal.width;
  	mask=CWWidth;
  	XConfigureWindow(tk_display->display,windows[number].topbar,mask,&xwc);
  	xwc.y=windows[number].normal.height-5;
  	mask=CWWidth|CWY;
  	XConfigureWindow(tk_display->display,windows[number].bottombar,mask,&xwc);

  	xwc.height=windows[number].normal.height;
  	mask=CWHeight;
  	XConfigureWindow(tk_display->display,windows[number].leftbar,mask,&xwc);
  	xwc.x=windows[number].normal.width-5;
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
	{
	  xwc.width=windows[number].normal.client_width-2;
  	  xwc.x=windows[number].normal.client_x;
	}
	else
	{ 
	  xwc.x=windows[number].normal.client_x-1;
	  xwc.width=windows[number].normal.client_width;
	}
	if((windows[number].attributes&Border)==Border)	
	  xwc.y=BORDERSIZE;
	else
	{
	  if(windows[number].identity.shape==False)
	    xwc.y=-1;
	  else xwc.y=0;
	}
  	mask=CWWidth|CWX|CWY;
  	XConfigureWindow(tk_display->display,windows[number].titlebar,mask,&xwc);

	xwc.x=windows[number].normal.client_width-22;
	if(windows[number].identity.shape==True && (windows[number].attributes&Border)!=Border) 
	  xwc.x=windows[number].normal.client_width-24;
  	mask=CWX;
  	wid_Configure(tk_display,windows[number].bn_close,xwc.x,0,0,0,CFX);


  	if((windows[number].attributes&Border)==Border && wm_info.wm_style==FULLDECORATION)
  	{
 	  XMapWindow(tk_display->display,windows[number].topbar);
 	  XMapWindow(tk_display->display,windows[number].bottombar);
 	  XMapWindow(tk_display->display,windows[number].leftbar);
 	  XMapWindow(tk_display->display,windows[number].rightbar);

 	  XMapWindow(tk_display->display,windows[number].topleftbox);
 	  XMapWindow(tk_display->display,windows[number].toprightbox);
 	  XMapWindow(tk_display->display,windows[number].bottomleftbox);
 	  XMapWindow(tk_display->display,windows[number].bottomrightbox);
  	}

  	else if((windows[number].attributes&Border)==Border && wm_info.wm_style==LIGHTDECORATION)
  	{
  	  XMapWindow(tk_display->display,windows[number].bottombar);
 	  XMapWindow(tk_display->display,windows[number].bottomleftbox);
 	  XMapWindow(tk_display->display,windows[number].bottomrightbox);
  	}

  	if(number==wm_clipboard_index)
  	{
  	  XClearWindow(tk_display->display,wm_clipboard_window); 
  	  tk_event.event.xexpose.x=tk_event.event.xexpose.y=0;
  	  tk_event.event.xexpose.width=windows[number].zoom.client_width;
  	  tk_event.event.xexpose.height=windows[number].zoom.client_height;
  	  CLIP_Draw();
  	}

 	windows[number].state.isZoomed=False;
#ifdef HAS_SHAPE_EXTENSION_LIBRARY
	if(windows[number].identity.shape==True)
	{
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
#endif
 	WIN_SetClientState(number,NormalState);
 	return 0;
	break;


   default :
	WIN_GetAddDimensions(number);
	return 0;
	break;

  }

}



