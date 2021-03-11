/*
 *
 * 	wm_get.c
 * 	informations diverses
 *
 * 	Modification :  11/11/93
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "iman.h"








long WM_GetWindowClass(window)
Window window;
{
 int i;

 for(i=0;i<maxwindows;i++)
 { 
   if(windows[i].class==TOP_LEVEL_WINDOW)
   {
     if(windows[i].mainwindow==window)
       return (long)REPARENTING_WINDOW;
     if(windows[i].clientwindow==window)
       return (long)TOP_LEVEL_WINDOW;
     if(windows[i].titlebar==window)
       return (long)TITLE_BAR;
     if(windows[i].leftbar==window||windows[i].rightbar==window||windows[i].topbar==window||windows[i].bottombar==window||windows[i].topleftbox==window||windows[i].toprightbox==window||windows[i].bottomleftbox==window||windows[i].bottomrightbox==window)
       return (long)BORDER_BOX;
     if(windows[i].icon.window==window||windows[i].icon.title==window||windows[i].icon.draw_area==window)
       return (long)ICON_WINDOW;
   }
   else if(windows[i].class==DIALOG_BOX)
   {
     if(windows[i].mainwindow==window)
       return (long)REPARENTING_WINDOW;
     if(windows[i].clientwindow==window)
       return (long)DIALOG_BOX;
     if(windows[i].titlebar==window)
       return (long)TITLE_BAR;
   }
 }
 return (long)win_GetType(tk_display,window);
}




long WM_GetWindowType(window)
Window window;
{
 return (long)WM_GetWindowClass(window);
}







long WM_GetWindowClassWithoutProperty(window)
Window window;
{
 int i;

 for(i=0;i<maxwindows;i++)
 { 
   if(windows[i].class==TOP_LEVEL_WINDOW)
   {
     if(windows[i].mainwindow==window)
       return (long)REPARENTING_WINDOW;
     if(windows[i].clientwindow==window)
       return (long)TOP_LEVEL_WINDOW;
     if(windows[i].titlebar==window)
       return (long)TITLE_BAR;
     if(windows[i].leftbar==window||windows[i].rightbar==window||windows[i].topbar==window||windows[i].bottombar==window||windows[i].topleftbox==window||windows[i].toprightbox==window||windows[i].bottomleftbox==window||windows[i].bottomrightbox==window)
       return (long)BORDER_BOX;
     if(windows[i].icon.window==window||windows[i].icon.title==window||windows[i].icon.draw_area==window)
       return (long)ICON_WINDOW;
   }
   else if(windows[i].class==DIALOG_BOX)
   {
     if(windows[i].mainwindow==window)
       return (long)REPARENTING_WINDOW;
     if(windows[i].clientwindow==window)
       return (long)DIALOG_BOX;
     if(windows[i].titlebar==window)
       return (long)TITLE_BAR;
   }
 }
 return -1;
}




long WM_GetWindowTypeWithoutProperty(window)
Window window;
{
  return WM_GetWindowClassWithoutProperty(window);
}



long WM_GetWindowAttributes(window)
Window window;
{
 int i;

 i=WM_GetWindowNumber(window);
 if(i>=0)
   return windows[i].attributes;
 else return win_GetAttributes(tk_display,window);
}





Bool IsWindow(window)
Window window;
{
 int i;

 for(i=0;i<maxwindows;i++)
   if(windows[i].clientwindow==window&&windows[i].isUsed==True)
     return True;
 return False;
}








Bool IsTitleBar(window)
Window window;
{
 int i;

 for(i=0;i<maxwindows;i++)
   if(windows[i].isUsed==True&&windows[i].titlebar==window)
     return True;
 return False;
}





Bool IsBorderBox(window)
Window window;
{
 int i;

 for(i=0;i<maxwindows;i++)													
   if(windows[i].class==TOP_LEVEL_WINDOW&&(windows[i].leftbar==window||windows[i].rightbar==window||windows[i].topbar==window||windows[i].bottombar==window||windows[i].topleftbox==window||windows[i].toprightbox==window||windows[i].bottomleftbox==window||windows[i].bottomrightbox==window))
     return True;
 return False;
}




Bool IsIconWindow(window)
Window window;
{
 int i;

 for(i=0;i<maxwindows;i++)													
 if(windows[i].class==TOP_LEVEL_WINDOW&&(windows[i].icon.window==window||windows[i].icon.title==window||windows[i].icon.draw_area==window))
   return True;
 return False;
}





Bool IsAnyOnTop()
{
 int i;

 for(i=0;i<maxwindows;i++)
 {
   if(windows[i].isUsed==True&&windows[i].state.isOnTop==True&&windows[i].state.isMapped==True)
   {
     if(WIN_VerifyTree(i)==0)
       return True;
     else windows[i].state.isOnTop=False;
   }
   else windows[i].state.isOnTop=False;
 }
#ifdef DEBUG
 fprintf(stderr,"isOnTop=False\n");
#endif
 return False;
}





int WM_GetNumber(window)
Window window;
{
 int i;

 for(i=0;i<maxwindows;i++)
   if(windows[i].isUsed==True&&windows[i].clientwindow==window) return i;
 return -1;

}



int WM_GetWindowNumber(window)
Window window;
{
 int i;

 for(i=0;i<maxwindows;i++)
   if(windows[i].isUsed==True&&windows[i].clientwindow==window) return i;
 return -1;

}



int WIN_GetNumber(window)
Window window;
{
 int i;

 for(i=0;i<maxwindows;i++)
   if(windows[i].isUsed==True&&windows[i].clientwindow==window) return i;
 return -1;

}





int WM_GetOnTop()
{
 int i;

 for(i=0;i<maxwindows;i++)
 {
   if(windows[i].isUsed==True && windows[i].state.isMapped==True && windows[i].state.isOnTop==True)
     return i;
 }
 return -1;
}




int WIN_GetOnTop()
{
 int i;

 for(i=0;i<maxwindows;i++)
 {
   if(windows[i].isUsed==True && windows[i].state.isMapped==True && windows[i].state.isOnTop==True)
     return i;
 }
 return -1;
}



int WM_GetUnusedWindow(class)
int class;
{
  int i;

  switch(class)
  {
    case TOP_LEVEL_WINDOW :
	for(i=0;i<maxwindows;i++)
  	if(windows[i].isUsed==False&&windows[i].class==TOP_LEVEL_WINDOW)
  	  return i;
	return -1;
	break;

    case DIALOG_BOX :	
  	for(i=0;i<maxwindows;i++)
  	if(windows[i].isUsed==False&&windows[i].class==DIALOG_BOX)
  	  return i;
	return -1;
	break;

    default : return -1;
  }
  
}




int numFreeWindows()
{
  int i,j;

  j=0;
  for(i=0;i<maxwindows;i++)
  if(windows[i].isUsed==False&&windows[i].class==TOP_LEVEL_WINDOW)
    j++;
  return j;
}




int numFreeDialogs()
{
  int i,j;

  j=0;
  for(i=0;i<maxwindows;i++)
  if(windows[i].isUsed==False&&windows[i].class==DIALOG_BOX)
    j++;
  return j;
}





int WIN_GetAddDimensions(number)
int number;
{
  int i, j;
  int add_leftside, add_rightside, add_top, add_bottom;
  int zadd_leftside, zadd_rightside, zadd_top, zadd_bottom;
  int ret;
  Bool bounding_shaped, clip_shaped;
  unsigned int w_bounding, h_bounding, w_clip, h_clip;
  int x_bounding, y_bounding, x_clip, y_clip;


  if(number<0 || number>=maxwindows || windows[number].isUsed==False)
    return -1;
#ifdef DEBUG
  fprintf(stderr,"WIN GetAddDimensions\n");
#endif
  add_leftside=0;
  add_top=0;
  add_bottom=0;
  add_rightside=0;
  zadd_leftside=0;
  zadd_top=0;
  zadd_bottom=0;
  zadd_rightside=0;

  switch(windows[number].class)
  {


    case TOP_LEVEL_WINDOW :

#ifdef HAS_SHAPE_EXTENSION_LIBRARY
	if(windows[number].identity.shape==True)
	{
#ifdef DEBUG
	  fprintf(stderr,"---------- Fenetre  %ld 0x%lx utilise SHAPE !!!\n",windows[number].clientwindow,windows[number].clientwindow);
#endif
	  windows[number].identity.shape=True;

    	  if((windows[number].attributes&Border)==Border)
	  {
	    	add_top=add_top+BORDERSIZE+1;  
		add_bottom=BORDERSIZE+1;
	 	add_leftside=BORDERSIZE+1;
		add_rightside=BORDERSIZE+1;		
		if((windows[number].attributes&TitleBar)==TitleBar) 
		{
		  add_top=add_top+TITLEBARHEIGHT+1; 	    
		  zadd_top=zadd_top+TITLEBARHEIGHT+1; 	    
		}
	    	windows[number].normal.border=2;
	  }
	  else
	  {
	    windows[number].normal.border=0;
	    if((windows[number].attributes&TitleBar)==TitleBar)
	    {
		 add_top=add_top+TITLEBARHEIGHT+2; 	    
		 zadd_top=zadd_top+TITLEBARHEIGHT+2; 	    
	    }
	  }
	  windows[number].zoom.border=0;
	}
	else
	{
    	  if((windows[number].attributes&Border)==Border)
	  {
	    	add_top=add_top+BORDERSIZE+1;  
		add_bottom=BORDERSIZE+1;
	 	add_leftside=BORDERSIZE+1;
		add_rightside=BORDERSIZE+1;
		if((windows[number].attributes&TitleBar)==TitleBar)
	     	{
		  add_top=add_top+TITLEBARHEIGHT+1; 	    
		  zadd_top=zadd_top+TITLEBARHEIGHT+1; 	    
	    	}
	  }
	  else
	  {
	    windows[number].normal.border=0;
	    if((windows[number].attributes&TitleBar)==TitleBar)
	    {
		 add_top=add_top+TITLEBARHEIGHT+1; 	    
		 zadd_top=zadd_top+TITLEBARHEIGHT+1; 	    
	    }
	  }
	  
	  windows[number].normal.border=2;
	  windows[number].zoom.border=2;
	}
#else
    	if((windows[number].attributes&Border)==Border)
	{
	  if(wm_info.wm_style==FULLDECORATION) 
	  {
	    	add_top=add_top+BORDERSIZE+1;  
		add_bottom=BORDERSIZE+1;
	 	add_leftside=BORDERSIZE+1;
		add_rightside=BORDERSIZE+1;
		if((windows[number].attributes&TitleBar)==TitleBar)
	     	{
		  add_top=add_top+TITLEBARHEIGHT+1; 	    
		  zadd_top=zadd_top+TITLEBARHEIGHT+1; 	    
	    	}
	  }
	  else
	  { 
	    add_bottom=BORDERSIZE+1;
	    if((windows[number].attributes&TitleBar)==TitleBar)
	    {
		  add_top=add_top+TITLEBARHEIGHT+1; 	    
		  zadd_top=zadd_top+TITLEBARHEIGHT+1; 	    
	    }
	  }
	}
	windows[number].normal.border=2;
	windows[number].zoom.border=2;

#endif

	windows[number].normal.add_top=add_top;
	windows[number].normal.add_bottom=add_bottom;
	windows[number].normal.add_leftside=add_leftside;
	windows[number].normal.add_rightside=add_rightside;
	windows[number].zoom.add_top=zadd_top;
	windows[number].zoom.add_bottom=zadd_bottom;
	windows[number].zoom.add_leftside=zadd_leftside;
	windows[number].zoom.add_rightside=zadd_rightside;	
	break;



    case DIALOG_BOX :


#ifdef HAS_SHAPE_EXTENSION_LIBRARY
	if(windows[number].identity.shape==True)
	{
	  if((windows[number].attributes&TitleBar)==TitleBar)
	  {
	    add_top=TITLEBARHEIGHT+2;
	    windows[number].normal.border=0;
	  }
	  else windows[number].normal.border=0;
	  if((windows[number].attributes&Border)==Border)
	    windows[number].normal.border=8;
	}
	else
	{
	  if((windows[number].attributes&TitleBar)==TitleBar)
	  {
	    add_top=TITLEBARHEIGHT+2;
	    windows[number].normal.border=2;
	  }
	  else windows[number].normal.border=2;
	  if((windows[number].attributes&Border)==Border)
	    windows[number].normal.border=8;
	}

#else
	if((windows[number].attributes&TitleBar)==TitleBar)
	{
	  add_top=TITLEBARHEIGHT+2;
	  windows[number].normal.border=2;
	}
	else windows[number].normal.border=2;
	if((windows[number].attributes&Border)==Border)
	  windows[number].normal.border=8;
#endif

	windows[number].normal.add_top=add_top;
	windows[number].normal.add_bottom=add_bottom;
	windows[number].normal.add_leftside=add_leftside;
	windows[number].normal.add_rightside=add_rightside;
	windows[number].zoom.add_top=0;
	windows[number].zoom.add_bottom=0;
	windows[number].zoom.add_leftside=0;
	windows[number].zoom.add_rightside=0;
#ifdef DEBUG
	fprintf(stderr,"add_top==%d \n",add_top);
#endif
	break;


  }
  return 0;
}





int WIN_GetProtocols(number)
int number;
{
  int i, j;
  Atom gp_actual_type;
  int gp_actual_format;
  unsigned long gp_nitems, gp_bytes_after;
  unsigned long *gp_prop;
  long ret;


  if(number<0 || number>=maxwindows || windows[number].isUsed==False)
    return -1;

   ret=XGetWindowProperty(tk_display->display,windows[number].clientwindow,tk_display->atoms.WM_PROTOCOLS,0,1, False,XA_ATOM,&gp_actual_type, &gp_actual_format, & gp_nitems, &gp_bytes_after,(unsigned char **)&gp_prop);
#ifdef DEBUG   
   fprintf(stderr,"%d protocols detectes\n",gp_nitems);
#endif
   if(ret==1) return -1;

   if(gp_nitems>0)
   {
     ret=gp_prop[0];
     windows[number].identity.protocols=(Atom *)malloc(sizeof(Atom)*gp_nitems+2);
     windows[number].identity.numprotocols=gp_nitems;
     windows[number].identity.maxprotocols=gp_nitems+2;
     
     if(windows[number].identity.protocols==NULL)
     {
       windows[number].identity.numprotocols=0;
       windows[number].identity.maxprotocols=0;
     }
     XFree(gp_prop);
     return 0;
   }
   else
   {
     windows[number].identity.numprotocols=0;
     windows[number].identity.maxprotocols=0;     
     windows[number].identity.protocols=(Atom *)NULL;     
     return -1;
   }

}




