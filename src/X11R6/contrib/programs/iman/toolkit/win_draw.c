/*
 *
 * 	win_draw.c  
 * 	affichage des fenetres
 *
 * 	Modification :  11/11/93
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



#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"

#include <X11/iman/widgets.h>






int win_Map(tk_display,window)
TkDisplay *tk_display;
Window window;
{
 XMapWindow(tk_display->display, window);
 return 0;
}





int win_MapRaised(tk_display,window)
TkDisplay *tk_display;
Window window;
{
 XMapRaised(tk_display->display,window);
 return 0;
}




int win_Unmap(tk_display,window)
TkDisplay *tk_display;
Window window;
{
  XEvent event;
  int mask;
  int i, j;
  

  XSync(tk_display->display,False);
  XFlush(tk_display->display);
  mask=KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|ExposureMask|FocusChangeMask;


  if(XPending(tk_display->display)>0)
  for(i=0;i<tk_display->maxwidgets;i++)
  {
    if(tk_display->widgets[i].isUsed==True)
    {
	switch(tk_display->widgets[i].class)
	{
	  case WI_BUTTON :
			if(tk_display->widgets[i].button->parent==window || tk_display->widgets[i].button->top_level==window)
			{
  			  /*fprintf(stderr,"Button\n");*/
			  while(XCheckWindowEvent(tk_display->display,tk_display->widgets[i].button->window,mask,&event)==True) j=j;
			}
			break;

	  case WI_EDIT :
			if(tk_display->widgets[i].edit->parent==window || tk_display->widgets[i].edit->top_level==window)
			{
  			  /*fprintf(stderr,"Edit\n");*/
			  while(XCheckWindowEvent(tk_display->display,tk_display->widgets[i].edit->window,mask,&event)==True) j=j;
			}
			break;

	  case WI_SCROLLBAR :
			if(tk_display->widgets[i].scroll->parent==window || tk_display->widgets[i].scroll->top_level==window)
			{
  			  /*fprintf(stderr,"Scroll\n");*/
			  while(XCheckWindowEvent(tk_display->display,tk_display->widgets[i].scroll->mainwindow,mask,&event)==True || XCheckWindowEvent(tk_display->display,tk_display->widgets[i].scroll->thumbwindow,mask,&event)==True) j=j;
			}
			break;

	  case WI_LIST :
			if(tk_display->widgets[i].list->parent==window || tk_display->widgets[i].list->top_level==window)
			{
  			  /*fprintf(stderr,"List\n");*/
			  while(XCheckWindowEvent(tk_display->display,tk_display->widgets[i].list->mainwindow,mask,&event)==True || XCheckWindowEvent(tk_display->display,tk_display->widgets[i].list->listwindow,mask,&event)==True) j=j;
			}
			break;

	  case WI_COMBO :
			if(tk_display->widgets[i].combo->parent==window || tk_display->widgets[i].combo->top_level==window)
			{
  			  /*fprintf(stderr,"Combo\n");*/
			  while(XCheckWindowEvent(tk_display->display,tk_display->widgets[i].combo->window,mask,&event)==True) j=j;
			}
			break;

	  case WI_MENU :
			if(tk_display->widgets[i].menu->parent==window || tk_display->widgets[i].menu->top_level==window)
			{
  			  /*fprintf(stderr,"Menu\n");*/
			  while(XCheckWindowEvent(tk_display->display,tk_display->widgets[i].menu->window,mask,&event)==True) j=j;
			}
			break;

	}
    }
  }
  /*fprintf(stderr,"A la fenetre\n");*/
  mask=KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|ExposureMask|FocusChangeMask;
  while(XCheckWindowEvent(tk_display->display,window,mask,&event)==True)   
    i=i;
    /*fprintf(stderr,"Event win\n");
  fprintf(stderr,"Fin de la fenetre\n");*/

  XUnmapWindow(tk_display->display,window);
  return 0;
}




