/*
 *
 * 	clp_set.c
 * 	modification du clipboard
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











int CLIP_PrintScreen()
{
 XImage *image;
 int mask;
 int ret;
 int i,current;

 
 current=WM_GetOnTop();
 if(current<0 || current>=maxwindows)
   windows[wm_clipboard_index].identity.colormap=DefaultColormap(tk_display->display,tk_display->screen);
 else windows[wm_clipboard_index].identity.colormap=windows[current].identity.colormap;

#ifdef DEBUG
 fprintf(stderr,"Print de tout l'ecran\n");
#endif
 image=XGetImage(tk_display->display,RootWindow(tk_display->display,tk_display->screen),0,0,DisplayWidth(tk_display->display,tk_display->screen),DisplayHeight(tk_display->display,tk_display->screen),AllPlanes,ZPixmap);
 if(image==NULL) 
 {
	fprintf(stderr,"Erreur dans l'image\n");
	return -1;
 }
 if(clip_info.pixmap>0) XFreePixmap(tk_display->display,clip_info.pixmap);
#ifdef DEBUG
 else fprintf(stderr,"Pix_clipboard=0\n");
#endif
 clip_info.pixmap=XCreatePixmap(tk_display->display,wm_clipboard_window,DisplayWidth(tk_display->display,tk_display->screen),DisplayHeight(tk_display->display,tk_display->screen),tk_display->depth);
 clip_info.depth=tk_display->depth;

 XPutImage(tk_display->display,clip_info.pixmap,clipboard_gc,image,0,0,0,0,DisplayWidth(tk_display->display,tk_display->screen),DisplayHeight(tk_display->display,tk_display->screen));
 XDestroyImage(image);
 XClearWindow(tk_display->display,wm_clipboard_window);
 if(windows[wm_clipboard_index].state.isMapped==True)
   XCopyArea(tk_display->display,clip_info.pixmap,wm_clipboard_window,clipboard_gc,0,0,DisplayWidth(tk_display->display,tk_display->screen),DisplayHeight(tk_display->display,tk_display->screen),0,0);


 if(XGetSelectionOwner(tk_display->display,XA_CLIPBOARD)!=wm_clipboard_window)
   XSetSelectionOwner(tk_display->display,XA_CLIPBOARD,wm_clipboard_window,CurrentTime);
 XSync(tk_display->display,False);
 XFlush(tk_display->display);

 clip_info.isOwner=True;
 clip_info.target=XA_PIXMAP;
 clip_info.owner=wm_clipboard_window;

 return 0;
}






int CLIP_PrintWindow(number)
int number;
{
 XImage *image;
 int mask;
 int ret;
 int i, j;

 if(number>=0 && number<maxwindows && windows[number].isUsed==True)
 {

   windows[wm_clipboard_index].identity.colormap=windows[number].identity.colormap;
#ifdef DEBUG
   fprintf(stderr,"Print de la fenetre '%s'\n",windows[number].title_name);
#endif
   if(windows[number].state.isZoomed==False)
   {
     image=XGetImage(tk_display->display,RootWindow(tk_display->display,tk_display->screen),windows[number].normal.x,windows[number].normal.y,windows[number].normal.width+windows[number].normal.border,windows[number].normal.height+windows[number].normal.border,AllPlanes,ZPixmap);
     if(image==NULL) 
     {
	fprintf(stderr,"Erreur dans l'image\n");
	return -1;
     }
     if(clip_info.pixmap>0) XFreePixmap(tk_display->display,clip_info.pixmap);
#ifdef DEBUG
     else fprintf(stderr,"Pix_clipboard=0\n");
#endif
     clip_info.pixmap=XCreatePixmap(tk_display->display,wm_clipboard_window,windows[number].normal.width+windows[number].normal.border,windows[number].normal.height+windows[number].normal.border,tk_display->depth);
     clip_info.depth=tk_display->depth;
     
     XPutImage(tk_display->display,clip_info.pixmap,clipboard_gc,image,0,0,0,0,windows[number].normal.width+windows[number].normal.border,windows[number].normal.height+windows[number].normal.border);
     XDestroyImage(image);
     XClearWindow(tk_display->display,wm_clipboard_window);
     if(windows[wm_clipboard_index].state.isMapped==True)
       XCopyArea(tk_display->display,clip_info.pixmap,wm_clipboard_window,clipboard_gc,0,0,windows[number].normal.width+windows[number].normal.border,windows[number].normal.height+windows[number].normal.border,0,0);
   }
   else
   {
     image=XGetImage(tk_display->display,RootWindow(tk_display->display,tk_display->screen),windows[number].zoom.x,windows[number].zoom.y,windows[number].zoom.width+windows[number].zoom.border,windows[number].zoom.height+windows[number].zoom.border,AllPlanes,ZPixmap);
     if(image==NULL) 
     {
	fprintf(stderr,"Erreur dans l'image\n");
	return -1;
     }
     if(clip_info.pixmap>0) XFreePixmap(tk_display->display,clip_info.pixmap);
#ifdef DEBUG
     else fprintf(stderr,"Pix_clipboard=0\n");
#endif
     clip_info.pixmap=XCreatePixmap(tk_display->display,wm_clipboard_window,windows[number].zoom.width+windows[number].zoom.border,windows[number].zoom.height+windows[number].zoom.border,tk_display->depth);
     clip_info.depth=tk_display->depth;

     XPutImage(tk_display->display,clip_info.pixmap,clipboard_gc,image,0,0,0,0,windows[number].zoom.width+windows[number].zoom.border,windows[number].zoom.height+windows[number].zoom.border);
     XDestroyImage(image);
     XClearWindow(tk_display->display,wm_clipboard_window);
     if(windows[wm_clipboard_index].state.isMapped==True)
       XCopyArea(tk_display->display,clip_info.pixmap,wm_clipboard_window,clipboard_gc,0,0,windows[number].zoom.width+windows[number].zoom.border,windows[number].zoom.height+windows[number].zoom.border,0,0);
   }

   if(XGetSelectionOwner(tk_display->display,XA_CLIPBOARD)!=wm_clipboard_window)
     XSetSelectionOwner(tk_display->display,XA_CLIPBOARD,wm_clipboard_window,CurrentTime);
   XSync(tk_display->display,False);
   XFlush(tk_display->display);

   clip_info.isOwner=True;
   clip_info.target=XA_PIXMAP;
   clip_info.owner=wm_clipboard_window;
   return 0;
 }
 else return -1;
}










int CLIP_PrintClientWindow(number)
int number;
{
 XImage *image;
 Window root;
 int mask;
 int ret;
 int i, j,x,y,w,h,depth,border;

 if(number>=0 && number<maxwindows && windows[number].isUsed==True)
 {
   windows[wm_clipboard_index].identity.colormap=windows[number].identity.colormap;
#ifdef DEBUG
   fprintf(stderr,"Print de la fenetre CLIENTE '%s'\n",windows[number].title_name);
#endif
   XGetGeometry(tk_display->display,windows[number].clientwindow,&root,&x,&y,&w,&h,&border,&depth);
   XTranslateCoordinates(tk_display->display,windows[number].clientwindow,RootWindow(tk_display->display,tk_display->screen),0,0,&x,&y,&root);

   image=XGetImage(tk_display->display,RootWindow(tk_display->display,tk_display->screen),x,y,w,h,AllPlanes,ZPixmap);
   if(image==NULL) 
   {
	fprintf(stderr,"Erreur dans l'image\n");
	return -1;
   }
   if(clip_info.pixmap>0) XFreePixmap(tk_display->display,clip_info.pixmap);
#ifdef DEBUG
   else fprintf(stderr,"Pix_clipboard=0\n");
#endif
   clip_info.pixmap=XCreatePixmap(tk_display->display,wm_clipboard_window,w,h,tk_display->depth);
   clip_info.depth=tk_display->depth;

   XPutImage(tk_display->display,clip_info.pixmap,clipboard_gc,image,0,0,0,0,w,h);
   XDestroyImage(image);
   XClearWindow(tk_display->display,wm_clipboard_window);
   if(windows[wm_clipboard_index].state.isMapped==True)
     XCopyArea(tk_display->display,clip_info.pixmap,wm_clipboard_window,clipboard_gc,0,0,w,h,0,0);

   if(XGetSelectionOwner(tk_display->display,XA_CLIPBOARD)!=wm_clipboard_window)
     XSetSelectionOwner(tk_display->display,XA_CLIPBOARD,wm_clipboard_window,CurrentTime);
   XSync(tk_display->display,False);
   XFlush(tk_display->display);

   clip_info.isOwner=True;
   clip_info.target=XA_PIXMAP;
   clip_info.owner=wm_clipboard_window;
   
   return 0;
 }
 else return -1;
}







