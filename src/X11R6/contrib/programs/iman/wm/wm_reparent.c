/*
 *
 * 	wm_reparent.c  
 * 	reparentage des fenetres clientes
 *
 * 	Modification :  19/12/93
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
#include <X11/keysym.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "iman.h"








int WIN_ReparentClient(number,clientwindow)
unsigned int number;
Window clientwindow;
{
 XWindowAttributes xwa;
 XSetWindowAttributes xswa;
 XWindowChanges xwc;
 XWMHints *xwmhints;
 XSizeHints hints, zhints, ihints;
 XEvent return_event, send_event;
 XClassHint classhint;
 XRectangle rectangle;

 Atom gp_actual_type;
 int gp_actual_format;
 unsigned long gp_nitems, gp_bytes_after;
 unsigned char *gp_prop;

 Window gg_root, transient;
 int gg_x, gg_y;
 unsigned int gg_border;

 long ptr;
 int i, j, ret;
 int minor, major;
 int mask;
 long win_class;
 long win_attributes;
 int add_top, add_bottom, add_leftside,add_rightside;
 int zadd_top, zadd_bottom, zadd_leftside,zadd_rightside;


 Bool bounding_shaped, clip_shaped;
 unsigned int w_bounding, h_bounding, w_clip, h_clip;
 int x_bounding, y_bounding, x_clip, y_clip;



		/** S'assurer que la fenetre est toujours valide **/

#ifdef DEBUG
 fprintf(stderr,"Window ID: %ld  parent:%ld  number:%d\n",clientwindow,windows[number].mainwindow,number);
#endif

 XSync(tk_display->display,False);
 if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
  {
REPARENT_END:
   windows[number].isUsed=0;
   return -1;
  }



	/*** S'assurer que la fenetre n'est pas l'icone d'une autre ***/

 for(i=0;i<maxwindows;i++)
 if(windows[i].isUsed==True&&windows[i].icon.hasIconWindow==True&&windows[i].icon.clientwindow==clientwindow)
   goto REPARENT_END;




		/* Obtenir les attributs et preparer les donnees */

 memset(&xwa,0,sizeof(XWindowAttributes)); 
 memset(&xwc,0,sizeof(XWindowChanges)); 
 memset(&xwmhints,0,sizeof(XWMHints)); 
 memset(&hints,0,sizeof(XSizeHints)); 
 memset(&zhints,0,sizeof(XSizeHints)); 
 memset(&ihints,0,sizeof(XSizeHints)); 
 memset(&classhint,0,sizeof(XClassHint)); 


 ret=XGetWindowAttributes(tk_display->display,clientwindow,&xwa);
 if (ret==0)
   goto REPARENT_END;
 
 win_class=win_GetType(tk_display,clientwindow);
 win_attributes=win_GetAttributes(tk_display,clientwindow);

 if(win_attributes==-1)
 { 
	 win_attributes=TitleBar+CloseBox+IconifyBox+ZoomBox+Border;
#ifdef DEBUG
	 fprintf(stderr,"Win attributes == -1\n");
#endif
 }
 if(win_class==-1)
 { 
	 win_class=TOP_LEVEL_WINDOW;
#ifdef DEBUG
	 fprintf(stderr,"Win class == -1\n");
#endif
 }

#ifdef DEBUG
 fprintf(stderr,"Win reparent type:%d  attributes:%d\n",win_class,win_attributes);
#endif

 if(windows[number].class!=win_class)
   return -1;


 if(xwa.override_redirect==False)
 switch(win_class)
 {

   case TOP_LEVEL_WINDOW :

    	windows[number].type=NormalWindow;
    	windows[number].attributes=win_attributes;
    	windows[number].state.isInitialized=False;
	windows[number].isUsed=True;
	windows[number].clientwindow=clientwindow;


			/*** Nom de la fenetre ***/

    	ret=XFetchName(tk_display->display,clientwindow,&windows[number].title_name);
    	if(ret==0||windows[number].title_name==NULL)
    	{ 
		windows[number].hasStoredName=False;
	 	windows[number].title_name="(Pas de nom)";
/*		ret=XGetClassHint(tk_display->display,clientwindow,&classhint);
		if(ret!=0)
		{
		  windows[number].title_name=classhint.res_class;
		  windows[number].hasStoredName=True;
		}
*/
    	}
    	else windows[number].hasStoredName=True;
#ifdef DEBUG
	fprintf(stderr,"WIN :Fenetre %s\n",windows[number].title_name);
#endif

		/*** Configuration de la decoration ***/

    	add_leftside=0;
    	add_top=0;
	add_bottom=0;
	add_rightside=0;
	zadd_leftside=0;
    	zadd_top=0;
	zadd_bottom=0;
	zadd_rightside=0;

#ifdef HAS_SHAPE_EXTENSION_LIBRARY
	ret=XShapeQueryExtents(tk_display->display,windows[number].clientwindow,&bounding_shaped,&x_bounding,&y_bounding,&w_bounding,&h_bounding,&clip_shaped,&x_clip,&y_clip,&w_clip,&h_clip);
	if(ret!=0) XShapeSelectInput(tk_display->display,windows[number].clientwindow,ShapeNotifyMask);
	if(ret!=0&&(bounding_shaped==True||clip_shaped==True))
	  windows[number].identity.shape=True;
	else windows[number].identity.shape=False;
#else
	windows[number].identity.shape=False;
#endif
	WIN_GetAddDimensions(number);
	add_top=windows[number].normal.add_top;
	add_bottom=windows[number].normal.add_bottom;
	add_leftside=windows[number].normal.add_leftside;
	add_rightside=windows[number].normal.add_rightside;
	zadd_top=windows[number].zoom.add_top;
	zadd_bottom=windows[number].zoom.add_bottom;
	zadd_leftside=windows[number].zoom.add_leftside;
	zadd_rightside=windows[number].zoom.add_rightside;


#ifdef DEBUG
	fprintf(stderr,"add_top: %d\n",add_top);
    	fprintf(stderr,"Recherche de la taille\n");
#endif


		  /******** Taille de la fenetre *********/

    	if(xwa.width<MINWIDTH) xwa.width=MINWIDTH;
    	if(xwa.height<MINHEIGHT) xwa.height=MINHEIGHT;
	if(xwa.width>MAXWIDTH) xwa.width=MAXWIDTH;
    	if(xwa.height>MAXHEIGHT) xwa.height=MAXHEIGHT;
	

    	windows[number].clientwindow=clientwindow;
    	windows[number].isUsed=True;
    	windows[number].normal.x=xwa.x;
    	windows[number].normal.y=xwa.y;
    	windows[number].normal.width=xwa.width;
    	windows[number].normal.height=xwa.height;
        windows[number].state.isMapped=False;
    	windows[number].normal.client_y=add_top;
    	windows[number].normal.client_x=add_leftside;
    	windows[number].zoom.client_y=zadd_top;
    	windows[number].zoom.client_x=zadd_leftside;


    	XSync(tk_display->display,False);
    	if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
    	{
    	  windows[number].isUsed=False;
    	  return -1;
    	}

    	ret=XGetNormalHints(tk_display->display, clientwindow, &hints);
    	hints.x=xwa.x;
    	hints.y=xwa.y;
    	hints.width=xwa.width;
    	hints.height=xwa.height;
    	if((hints.flags&PPosition)!=PPosition)
      	hints.flags+=PPosition;
    	if((hints.flags&PSize)!=PSize)
      	  hints.flags+=PSize;


    	if(ret!=0&&(hints.flags&PPosition)==PPosition && hints.flags<=256 && hints.flags>=0)
    	{
    	  windows[number].normal.x=hints.x;
    	  windows[number].normal.y=hints.y;
	  windows[number].normal.client_x=add_leftside;
	  windows[number].normal.client_y=add_top;
    	}


    	if(ret!=0&&(hints.flags&PSize)==PSize && hints.flags<=256 && hints.flags>=0)
    	{
	  if(hints.width>=MINWIDTH-add_leftside-add_rightside) windows[number].normal.client_width=hints.width; 
   	  else windows[number].normal.client_width=MINWIDTH-add_leftside-add_rightside;
   	  if(hints.height>=MINHEIGHT-add_top-add_bottom) windows[number].normal.client_height=hints.height;
   	  else windows[number].normal.client_height=MINHEIGHT-add_top-add_bottom; 
	  windows[number].normal.width=windows[number].normal.client_width+add_leftside+add_rightside;
	  windows[number].normal.height=windows[number].normal.client_height+add_top+add_bottom;
    	}
	else
	{
	  if(hints.width>=MINWIDTH-add_leftside-add_rightside) windows[number].normal.client_width=hints.width; 
   	  else windows[number].normal.client_width=MINWIDTH-add_leftside-add_rightside;
   	  if(hints.height>=MINHEIGHT-add_top-add_bottom) windows[number].normal.client_height=hints.height;
   	  else windows[number].normal.client_height=MINHEIGHT-add_top-add_bottom; 
	  windows[number].normal.width=windows[number].normal.client_width+add_leftside+add_rightside;
	  windows[number].normal.height=windows[number].normal.client_height+add_top+add_bottom;
	}

    	if(ret!=0&&(hints.flags&PMinSize)==PMinSize && hints.flags<=256 && hints.flags>=0)
    	{
   	  if(hints.min_width+add_leftside+add_rightside>=MINWIDTH ) windows[number].normal.min_width=hints.min_width+add_leftside+add_rightside;
   	  else windows[number].normal.min_width=MINWIDTH;
   	  if(hints.min_height+add_top+add_bottom>=MINHEIGHT) windows[number].normal.min_height=hints.min_height+add_top+add_bottom;
   	  else windows[number].normal.min_height=MINHEIGHT;

   	  if(windows[number].normal.width<windows[number].normal.min_width) windows[number].normal.width=windows[number].normal.min_width;
   	  if(windows[number].normal.height<windows[number].normal.min_height) windows[number].normal.height=windows[number].normal.min_height;
    	}
    	else 
	{
    	  windows[number].normal.min_width=MINWIDTH;
    	  windows[number].normal.min_height=MINHEIGHT;
   	  if(windows[number].normal.width<windows[number].normal.min_width) windows[number].normal.width=windows[number].normal.min_width;
   	  if(windows[number].normal.height<windows[number].normal.min_height) windows[number].normal.height=windows[number].normal.min_height;
      	}

/**** VERIFIER QUE PMAXSIZE>PMINSIZE ****/

    	if(ret!=0&&(hints.flags&PMaxSize)==PMaxSize && hints.flags<=256 && hints.flags>=0)
    	{
   	  if(hints.max_width+add_leftside+add_rightside<=MAXWIDTH && hints.max_width>0) windows[number].normal.max_width=hints.max_width+add_leftside+add_rightside;
   	  else windows[number].normal.max_width=MAXWIDTH;
   	  if(hints.max_height+add_top+add_bottom<=MAXHEIGHT && hints.max_height>0) windows[number].normal.max_height=hints.max_height+add_top+add_bottom;
   	  else windows[number].normal.max_height=MAXHEIGHT;

     	  if(windows[number].normal.width>windows[number].normal.max_width) windows[number].normal.width=windows[number].normal.max_width;
     	  if(windows[number].normal.height>windows[number].normal.max_height) windows[number].normal.height=windows[number].normal.max_height;
    	}
    	else 
	{
   	  windows[number].normal.max_width=MAXWIDTH;
   	  windows[number].normal.max_height=MAXHEIGHT;

     	  if(windows[number].normal.width>windows[number].normal.max_width) windows[number].normal.width=windows[number].normal.max_width;
     	  if(windows[number].normal.height>windows[number].normal.max_height) windows[number].normal.height=windows[number].normal.max_height;
     	}

  	windows[number].normal.client_width=windows[number].normal.width-add_leftside-add_rightside;
	windows[number].normal.client_height=windows[number].normal.height-add_bottom-add_top;

    	xwa.width=windows[number].normal.client_width;
    	xwa.height=windows[number].normal.client_height;
  

#ifdef DEBUG
	fprintf(stderr,"add_top: %d\n",add_top);
    	fprintf(stderr,"Recherche du ZOOM\n");
#endif


			/***** Coordonnees en mode ZOOM *****/


    	XSync(tk_display->display,False);
    	if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
    	{
    	  windows[number].isUsed=0;
    	  return -1;
    	}

    	ret=XGetZoomHints(tk_display->display, clientwindow, &zhints);
    	if (ret==BadWindow) goto REPARENT_END;


    	if(ret!=0&&((zhints.flags&PPosition)==PPosition||(zhints.flags&USPosition)==USPosition) && zhints.flags<256 && zhints.flags>=0)
    	{
    	  windows[number].zoom.x=zhints.x;
    	  windows[number].zoom.y=zhints.y;
    	}
    	else 
    	{
    	  windows[number].zoom.x=-1;
   	  windows[number].zoom.y=-1;
     	}
	windows[number].zoom.client_x=zadd_leftside;
	windows[number].zoom.client_y=zadd_top;
	

    	if(ret!=0&&((zhints.flags&PSize)==PSize||(zhints.flags&USSize)==USSize) && zhints.flags<=256 && zhints.flags>=0)
    	{
   	  if(zhints.width+zadd_leftside+zadd_rightside>=MINWIDTH) windows[number].zoom.width=zhints.width+zadd_leftside+zadd_rightside;
	  else windows[number].zoom.width=MINWIDTH;
   	  if(zhints.height+zadd_top+zadd_bottom>=MINHEIGHT) windows[number].zoom.height=zhints.height+zadd_top+zadd_bottom;
	  else windows[number].zoom.height=MINHEIGHT;
	  windows[number].zoom.client_width=windows[number].zoom.width-zadd_leftside-zadd_rightside;
	  windows[number].zoom.client_height=windows[number].zoom.height-zadd_top-zadd_bottom;
    	}
	else
	{
	  windows[number].zoom.width=ZOOMMAXWIDTH;   
    	  windows[number].zoom.height=ZOOMMAXHEIGHT;     
	  windows[number].zoom.client_width=ZOOMMAXWIDTH-zadd_leftside-zadd_rightside;   
    	  windows[number].zoom.client_height=ZOOMMAXHEIGHT-zadd_top-zadd_bottom;     
	}

	if(ret!=0&&(zhints.flags&PMinSize)==PMinSize && zhints.flags<=256 && zhints.flags>=0)
    	{
   	  if(zhints.min_width+add_leftside+add_rightside>=MINWIDTH ) windows[number].zoom.min_width=zhints.min_width;
   	  else windows[number].zoom.min_width=MINWIDTH;
   	  if(zhints.min_height+add_top+add_bottom>=MINHEIGHT) windows[number].zoom.min_height=zhints.min_height;
   	  else windows[number].zoom.min_height=MINHEIGHT;

   	  if(windows[number].zoom.width<windows[number].zoom.min_width) windows[number].zoom.width=windows[number].zoom.min_width;
   	  if(windows[number].zoom.height<windows[number].zoom.min_height) windows[number].zoom.height=windows[number].zoom.min_height;
    	}
    	else 
	{
    	  windows[number].zoom.min_width=windows[number].zoom.width;
    	  windows[number].zoom.min_height=windows[number].zoom.height;
   	  if(windows[number].zoom.width<windows[number].zoom.min_width) windows[number].zoom.width=windows[number].zoom.min_width;
   	  if(windows[number].zoom.height<windows[number].zoom.min_height) windows[number].zoom.height=windows[number].zoom.min_height;
      	}    	


    	if(ret!=0&&(zhints.flags&PMaxSize)==PMaxSize && zhints.flags<=256 && zhints.flags>=0)
    	{
   	  if(zhints.max_width+zadd_leftside+zadd_rightside<ZOOMMAXWIDTH && zhints.max_width>0) windows[number].zoom.max_width=zhints.max_width;
   	  else windows[number].zoom.max_width=ZOOMMAXWIDTH;
   	  if(zhints.max_height+zadd_top+zadd_bottom<ZOOMMAXHEIGHT && zhints.max_height>0) windows[number].zoom.max_height=zhints.max_height;
   	  else windows[number].zoom.max_height=ZOOMMAXHEIGHT;

     	  if(windows[number].zoom.width>windows[number].zoom.max_width) windows[number].zoom.width=windows[number].zoom.max_width;
     	  if(windows[number].zoom.height>windows[number].zoom.max_height) windows[number].zoom.height=windows[number].zoom.max_height;
    	}
    	else 
	{
   	  windows[number].zoom.max_width=windows[number].zoom.width;
   	  windows[number].zoom.max_height=windows[number].zoom.height;

     	  if(windows[number].zoom.width>windows[number].zoom.max_width) windows[number].zoom.width=windows[number].zoom.max_width;
     	  if(windows[number].zoom.height>windows[number].zoom.max_height) windows[number].zoom.height=windows[number].zoom.max_height;
     	}

  	windows[number].zoom.client_width=windows[number].zoom.width-zadd_leftside-zadd_rightside;
	windows[number].zoom.client_height=windows[number].zoom.height-zadd_bottom-zadd_top;



			/***** Gestion de l'icone *****/

  
#ifdef DEBUG
	fprintf(stderr,"add_top: %d\n",add_top);
    	fprintf(stderr,"Recherche d'icone\n");
#endif
  
    	XSync(tk_display->display,False);
    	if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
    	{
   	  windows[number].isUsed=0;
   	  return -1;
    	}


    	xwmhints=XGetWMHints(tk_display->display, clientwindow);

    	if(xwmhints!=NULL)
    	{
	  if((xwmhints->flags&StateHint)==StateHint)
	    windows[number].state.initialstate=xwmhints->initial_state;
	  else windows[number].state.initialstate=NormalState;

	  if((xwmhints->flags&IconPixmapHint)==IconPixmapHint&&xwmhints!=NULL&&xwmhints->icon_pixmap!=0)
	  {
	    windows[number].icon.pixmap=xwmhints->icon_pixmap;
	    ret=XGetGeometry(tk_display->display,xwmhints->icon_pixmap,&gg_root,&gg_x,&gg_y,&windows[number].icon.pix_width,&windows[number].icon.pix_height,&gg_border,&windows[number].icon.depth);
	    if(ret==BadDrawable||ret==BadWindow)
	    {
		windows[number].icon.pixmap=0;
		windows[number].icon.pix_width=windows[number].icon.pix_height=0;
		windows[number].icon.depth=0;
	    }
	  }
	  else
	  {
		windows[number].icon.pixmap=0;
		windows[number].icon.pix_width=windows[number].icon.pix_height=0;
		windows[number].icon.depth=0;
	  }
	
	  if((xwmhints->flags&IconMaskHint)==IconMaskHint)
	    windows[number].icon.mask=xwmhints->icon_mask;
	  else windows[number].icon.mask=0;

	  if((xwmhints->flags&IconWindowHint)==IconWindowHint&&xwmhints!=NULL&&xwmhints->flags<=256)
	  {
#ifdef DEBUG
	      fprintf(stderr,"icon_window: %ld\n",xwmhints->icon_window);
#endif
	      XReparentWindow(tk_display->display,xwmhints->icon_window,windows[number].icon.draw_area,0,0);
	      XSelectInput(tk_display->display,xwmhints->icon_window,ResizeRedirectMask);
	      XAddToSaveSet(tk_display->display,xwmhints->icon_window);
	      xwc.border_width=0;
	      xwc.width=ICON_DRAW_AREA_WIDTH;
	      xwc.height=ICON_DRAW_AREA_HEIGHT;
	      mask=CWBorderWidth|CWWidth|CWHeight;
	      XConfigureWindow(tk_display->display,xwmhints->icon_window,mask,&xwc);
	      windows[number].icon.hasIconWindow=True;
	      windows[number].icon.clientwindow=xwmhints->icon_window;
	      ptr=ICON_WINDOW;
	      ret=XChangeProperty(tk_display->display,windows[number].icon.clientwindow,tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeReplace,(char *)&ptr,1);
	      if (ret==BadAlloc||ret==BadAtom||ret==BadMatch||ret==BadValue||ret==BadWindow) goto REPARENT_END;

		    	/**** CHERCHER SI DEJA REPARENTE PAR LE WM ****/
	      j=0;
	      while(windows[j].clientwindow!=xwmhints->icon_window&&j<maxwindows)
	       j++;
	      if(j<maxwindows)
	        WM_ReInitWindow(j);
	      ptr=ICON_WINDOW;
	        XChangeProperty(tk_display->display,xwmhints->icon_window,tk_display->atoms._IMAN_WM_TYPE,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
	
	  }
	  else
	  { 
		windows[number].icon.hasIconWindow=False;
		windows[number].icon.clientwindow=0;
	  }
		

	  if((xwmhints->flags&IconPositionHint)==IconPositionHint)
	  { 
		windows[number].icon.x=xwmhints->icon_x;
	      	windows[number].icon.y=xwmhints->icon_y;
	  }
	  else 
	  { 
		windows[number].icon.x=windows[number].normal.x;
	        windows[number].icon.y=windows[number].normal.y; 
	  }
	  XMoveWindow(tk_display->display,windows[number].icon.window,windows[number].icon.x,windows[number].icon.y);

	  XSync(tk_display->display,False);
	  if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
	  {
	    windows[number].isUsed=0;
	    return -1;
	  }

	  ret=XGetIconName(tk_display->display,clientwindow,&windows[number].icon.name);
	  if(ret==0||windows[number].icon.name==NULL)
	  { 
		windows[number].icon.name=windows[number].title_name;
		windows[number].icon.hasStoredIconName=False;
	  }
	  else windows[number].icon.hasStoredIconName=True;
     	}
     	else
     	{
	  windows[number].icon.name=windows[number].title_name;
	  windows[number].icon.hasStoredIconName=False;
	  windows[number].icon.pixmap=0;
	  windows[number].icon.pix_width=windows[number].icon.pix_height=0;
	  windows[number].icon.depth=0;
	  windows[number].icon.x=0;
	  windows[number].icon.y=0;
        }


#ifdef DEBUG
    	fprintf(stderr,"Recherche du groupe\n");
#endif

		    /***** Gestion du groupe *****/


	windows[number].attributes=win_attributes;
    	if(xwmhints!=NULL&&(xwmhints->flags&WindowGroupHint)==WindowGroupHint&&xwmhints->flags<=256&&xwmhints->window_group>0)
    	{
	  if((win_attributes&GroupLeader)==GroupLeader && (xwmhints->window_group==clientwindow||xwmhints->window_group<=0))
	  {
		windows[number].group.leader=0;
		windows[number].group.number=-1;
	  }
     	  else if((win_attributes&GroupLeader)!=GroupLeader && (xwmhints->window_group==clientwindow||xwmhints->window_group<=0))
	  {
     	  	windows[number].attributes=windows[number].attributes+GroupLeader;
	  	windows[number].group.leader=0;
		windows[number].group.number=-1;
	  }
	  else if((win_attributes&GroupMember)==GroupMember && xwmhints->window_group!=clientwindow&&xwmhints->window_group>0)
	    windows[number].group.leader=xwmhints->window_group;
     	  else if((win_attributes&GroupMember)!=GroupMember && xwmhints->window_group!=clientwindow&&xwmhints->window_group>0)
    	  {
     	  	windows[number].attributes=windows[number].attributes+GroupMember;
     	  	windows[number].group.leader=xwmhints->window_group;
 	  }

#ifdef DEBUG
    	  fprintf(stderr,"Group: %ld  attributs:%d  flags:%d\n",windows[number].group.leader,windows[number].attributes,xwmhints->flags);
#endif
     	  if((windows[number].attributes&GroupMember)==GroupMember)
     	  {
		WIN_AddToGroup(number);
#ifdef DEBUG
    	  	fprintf(stderr,"Add to group : %ld  attributs:%d  flags:%d\n",windows[number].group.leader,windows[number].attributes,xwmhints->flags);
#endif
		win_attributes=windows[number].attributes;
     	  }
#ifdef DEBUG
     	  fprintf(stderr,"Fin du Group\n");
#endif
     
    	}
    	else
	{
 	  windows[number].group.leader=0;
 	  windows[number].group.number=-1;
	}




		/**** gestion du transient ****/

#ifdef DEBUG
    	fprintf(stderr,"Transient\n");
#endif

	wm_action.type=ReparentingAction;
	WIN_SetTransientLeader(number);
	wm_action.type=NoAction;



		/**** gestion du colormap ****/


	
	if(xwa.colormap!=0)
	  windows[number].identity.colormap=xwa.colormap;
	else windows[number].identity.colormap=default_colormap;
#ifdef DEBUG
	fprintf(stderr,"WIN Colormap=%ld\n",windows[number].identity.colormap);
#endif

		/*** gestion des dimensions finales ***/

#ifdef DEBUG
	fprintf(stderr,"add_top: %d\n",add_top);
    	fprintf(stderr,"Dimensions finales\n");
#endif

    	windows[number].isUsed=True;
  
    	xwc.width=windows[number].normal.width;
    	xwc.height=windows[number].normal.height;
    	xwc.x=windows[number].normal.x;
    	xwc.y=windows[number].normal.y;
    	xwc.border_width=windows[number].normal.border/2;
    	mask=CWWidth|CWHeight|CWX|CWY|CWBorderWidth;


    	XSync(tk_display->display,False);
    	if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
    	{
      	  windows[number].isUsed=0;
     	  return -1;
    	}

	XConfigureWindow(tk_display->display,windows[number].mainwindow,mask,&xwc);
    	XSetWindowBackground(tk_display->display,windows[number].mainwindow,tk_display->win_colors.bg);
	XSetWindowBorder(tk_display->display,windows[number].mainwindow,tk_display->win_colors.border_inactive);

#ifdef HAS_SHAPE_EXTENSION_LIBRARY
	if(XShapeQueryVersion(tk_display->display,&major,&minor)!=0)
	{
	  XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,None,0);
	  XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,None,0);
	}

	if(windows[number].identity.shape==True)
	{
#ifdef DEBUG
	  fprintf(stderr,"client_x=%d  client_y=%d\n",windows[number].normal.client_x,windows[number].normal.client_y);
#endif
	  rectangle.x=windows[number].normal.client_x;	  
	  rectangle.y=windows[number].normal.client_y;
	  rectangle.width=windows[number].normal.client_width;
	  rectangle.height=windows[number].normal.client_height;
	  XShapeCombineRectangles(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,&rectangle,1,ShapeSubtract,Unsorted);
	  XShapeCombineRectangles(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,&rectangle,1,ShapeSubtract,Unsorted);
																							
	  XShapeCombineShape(tk_display->display,windows[number].mainwindow,ShapeBounding,windows[number].normal.client_x,windows[number].normal.client_y,clientwindow,ShapeBounding,ShapeUnion);
	  XShapeCombineShape(tk_display->display,windows[number].mainwindow,ShapeClip,windows[number].normal.client_x,windows[number].normal.client_y,clientwindow,ShapeBounding,ShapeUnion);

	}
	
#endif

    	xwc.width=windows[number].normal.client_width;
    	xwc.height=windows[number].normal.client_height;
    	xwc.border_width=0;
    	mask=CWWidth|CWHeight|CWBorderWidth;
    	XConfigureWindow(tk_display->display,windows[number].clientwindow,mask,&xwc);


    	XUnmapWindow(tk_display->display,windows[number].topbar);
    	xwc.width=windows[number].normal.width;
    	mask=CWWidth;
    	XConfigureWindow(tk_display->display,windows[number].topbar,mask,&xwc);
    	if((win_attributes&Border)==Border && wm_info.wm_style==FULLDECORATION) 
    	  XMapWindow(tk_display->display,windows[number].topbar);


    	XUnmapWindow(tk_display->display,windows[number].bottombar);
    	xwc.width=windows[number].normal.width;
    	xwc.y=windows[number].normal.height-windows[number].normal.add_bottom;
    	mask=CWWidth|CWY;
    	XConfigureWindow(tk_display->display,windows[number].bottombar,mask,&xwc);
    	if((win_attributes&Border)==Border) XMapWindow(tk_display->display,windows[number].bottombar);

    	XUnmapWindow(tk_display->display,windows[number].leftbar);
    	xwc.height=windows[number].normal.height;
    	mask=CWHeight;
    	XConfigureWindow(tk_display->display,windows[number].leftbar,mask,&xwc);
    	if((win_attributes&Border)==Border && wm_info.wm_style==FULLDECORATION) XMapWindow(tk_display->display,windows[number].leftbar);

    	XUnmapWindow(tk_display->display,windows[number].rightbar);
    	xwc.height=windows[number].normal.height;
    	xwc.x=windows[number].normal.width-windows[number].normal.add_bottom;
    	mask=CWHeight|CWX;
    	XConfigureWindow(tk_display->display,windows[number].rightbar,mask,&xwc);
    	if((win_attributes&Border)==Border && wm_info.wm_style==FULLDECORATION) XMapWindow(tk_display->display,windows[number].rightbar);

    	XUnmapWindow(tk_display->display,windows[number].toprightbox);
	XUnmapWindow(tk_display->display,windows[number].topleftbox);
    	if((win_attributes&Border)==Border && wm_info.wm_style==FULLDECORATION) XMapWindow(tk_display->display,windows[number].toprightbox);
    	xwc.x=windows[number].normal.width-14;
    	mask=CWX;
    	XConfigureWindow(tk_display->display,windows[number].toprightbox,mask,&xwc);
    	if((win_attributes&Border)==Border && wm_info.wm_style==FULLDECORATION) 
    	{
   	   XMapWindow(tk_display->display,windows[number].toprightbox);
   	   XMapWindow(tk_display->display,windows[number].topleftbox);
    	}

    	XUnmapWindow(tk_display->display,windows[number].bottomleftbox);
    	xwc.y=windows[number].normal.height-14;
    	mask=CWY;
    	XConfigureWindow(tk_display->display,windows[number].bottomleftbox,mask,&xwc);
    	if((win_attributes&Border)==Border) XMapWindow(tk_display->display,windows[number].bottomleftbox);

    	XUnmapWindow(tk_display->display,windows[number].bottomrightbox);
    	xwc.x=windows[number].normal.width-14;
    	xwc.y=windows[number].normal.height-14;
    	mask=CWX|CWY;
    	XConfigureWindow(tk_display->display,windows[number].bottomrightbox,mask,&xwc);
    	if((win_attributes&Border)==Border) XMapWindow(tk_display->display,windows[number].bottomrightbox);


    	XUnmapWindow(tk_display->display,windows[number].titlebar);
        if(windows[number].identity.shape==True&&(win_attributes&Border)!=Border)
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
  	  if((win_attributes&Border)==Border) 
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
    	if((win_attributes&TitleBar)==TitleBar) XMapWindow(tk_display->display,windows[number].titlebar);


#ifdef DEBUG
    	fprintf(stderr,"Widgets\n");
#endif

    	wid_Unmap(tk_display,windows[number].bn_zoom);
    	wid_Unmap(tk_display,windows[number].bn_iconify);
    	wid_Unmap(tk_display,windows[number].bn_close);


    	if((win_attributes&IconifyBox)==IconifyBox&&(win_attributes&ZoomBox)!=ZoomBox) 
     	  wid_Configure(tk_display,windows[number].bn_iconify,3,1,0,0,CFX|CFY);
    	else wid_Configure(tk_display,windows[number].bn_iconify,25,1,0,0,CFX|CFY);

	xwc.x=windows[number].normal.client_width-22;
	if(windows[number].identity.shape==True&&(win_attributes&Border)!=Border)
    	  xwc.x=windows[number].normal.client_width-24;
    	mask=CWX;
    	wid_Configure(tk_display,windows[number].bn_close,xwc.x,0,0,0,CFX);

												
	if((win_attributes&ZoomBox)==ZoomBox &&(win_attributes&TitleBar)==TitleBar) 
	  wid_Map(tk_display,windows[number].bn_zoom);
	if((win_attributes&IconifyBox)==IconifyBox && (win_attributes&TitleBar)==TitleBar) 
	  wid_Map(tk_display,windows[number].bn_iconify);
	if((win_attributes&CloseBox)==CloseBox && (win_attributes&TitleBar)==TitleBar) 
	  wid_Map(tk_display,windows[number].bn_close);


#ifdef DEBUG
    	fprintf(stderr,"Reparentage final\n");
#endif


	  		/******** Reparentage du client ********/

    	XSync(tk_display->display,False);
    	if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
    	{
   	  windows[number].isUsed=0;
   	  return -1;
    	}


    	if(wm_info.wm_style==FULLDECORATION)
    	{															
	    XReparentWindow(tk_display->display,clientwindow,windows[number].mainwindow,add_leftside,add_top);
    	}	
    	else if(wm_info.wm_style==LIGHTDECORATION)
    	{										
	  if((win_attributes&Border)== Border && (win_attributes&TitleBar)!=TitleBar) 
	    XReparentWindow(tk_display->display,clientwindow,windows[number].mainwindow,0,0);
	  else if((win_attributes&Border)!= Border && (win_attributes&TitleBar)==TitleBar) 
	    XReparentWindow(tk_display->display,clientwindow,windows[number].mainwindow,0,TITLEBARHEIGHT+1);  
	  else if((win_attributes&Border)== Border && (win_attributes&TitleBar)==TitleBar) 
	    XReparentWindow(tk_display->display,clientwindow,windows[number].mainwindow,0,TITLEBARHEIGHT+1);
	  else if((win_attributes&Border)!= Border && (win_attributes&TitleBar)!=TitleBar) 
	    XReparentWindow(tk_display->display,clientwindow,windows[number].mainwindow,0,0);
    	}


    	XSync(tk_display->display,False);
    	if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
    	{
    	  windows[number].isUsed=0;
    	  return -1;
    	}


    	XAddToSaveSet(tk_display->display,clientwindow);

    	xwc.border_width=0;
    	xwc.width=windows[number].normal.client_width;
    	xwc.height=windows[number].normal.client_height;
    	mask=CWBorderWidth|CWWidth|CWHeight;;
    	XConfigureWindow(tk_display->display,clientwindow,mask,&xwc);
    	XSetWindowBorder(tk_display->display,clientwindow,tk_display->win_colors.border_inactive);


    	windows[number].attributes=win_attributes;
    	WIN_SetClientAttributes(number,win_attributes);
    	WIN_SetClientState(number,DontCareState);

    
 	if(clientwindow==wm_clipboard_window)
	{
	  	wm_clipboard_index=number;
	  	windows[number].icon.name="Clip";
#ifdef DEBUG
	  	fprintf(stderr,"Clipboard detecte\n");
#endif
	}
	else XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Delete),ControlMask|Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Escape),Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Escape),ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
													
	  
	if(clientwindow!=wm_clipboard_window) 
	  XSelectInput(tk_display->display,clientwindow,StructureNotifyMask|PropertyChangeMask|ColormapChangeMask);
	else XSelectInput(tk_display->display,clientwindow,StructureNotifyMask|PropertyChangeMask|ColormapChangeMask|ExposureMask|KeyReleaseMask|KeyPressMask);
  

    	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Escape),ControlMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
    	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_End),ControlMask|Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
        if(XKeysymToKeycode(tk_display->display,XK_End)<=0)
	  fprintf(stderr,"!!! XK_End keycode=0\n !!!");
        if(XKeysymToKeycode(tk_display->display,XK_Delete)<=0)
	  fprintf(stderr,"!!! XK_Delete keycode=0\n !!!");

    	ret=XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Print),ControlMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
    	ret=XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Print),ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
    	ret=XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Print),ControlMask|ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
    	ret=XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Print),ControlMask|Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
    	ret=XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Print),0,clientwindow,False,GrabModeAsync,GrabModeAsync);


#ifdef DEBUG
    	fprintf(stderr,"Avant-fin\n");
#endif

    	xswa.save_under=xwa.save_under;
	xswa.win_gravity=NorthWestGravity;
    	XChangeWindowAttributes(tk_display->display,windows[number].mainwindow,CWSaveUnder|CWWinGravity,&xswa);

    	if(xwmhints!=NULL) XFree(xwmhints);



	send_event.type=ConfigureNotify;
	send_event.xconfigure.window=clientwindow;
	send_event.xconfigure.send_event=True;
	send_event.xconfigure.x=windows[number].normal.x;
	send_event.xconfigure.y=windows[number].normal.y;
	send_event.xconfigure.width=windows[number].normal.client_width;
	send_event.xconfigure.height=windows[number].normal.client_height;
	send_event.xconfigure.border_width=windows[number].normal.border/2;
	if(clientwindow!=wm_clipboard_window)
	  XSendEvent(tk_display->display,clientwindow,True,False,&send_event);

	WIN_GetProtocols(number);
 	ret=XGetWindowAttributes(tk_display->display,clientwindow,&xwa);
 	if (ret==0||(ret==1&&(xwa.your_event_mask==NoEventMask||xwa.your_event_mask==0)))
 	  goto REPARENT_END;
#ifdef DEBUG
	fprintf(stderr,"event mask = %d\n",xwa.your_event_mask);
	fprintf(stderr,"StructureNotifyMask=%d   &event_mask=%d\n",StructureNotifyMask,(StructureNotifyMask&xwa.your_event_mask));
	ret=XGetWindowAttributes(tk_display->display,windows[number].mainwindow,&xwa);
	fprintf(stderr,"ret=%d  SubstructureRedirectMask=%d  &event_mask=%d\n",ret,SubstructureRedirectMask,(SubstructureRedirectMask&xwa.your_event_mask));

    	fprintf(stderr,"WIN reparent fin number:%d\n",number);
	windows[number].clientwindow=clientwindow;
#endif
    	return 0;
	break;












    case DIALOG_BOX :	/****** BOITES DE DIALOGUE ******/

    	windows[number].type=NormalWindow;
    	windows[number].attributes=win_attributes;
    	windows[number].state.isInitialized=False;
	windows[number].isUsed=True;
	windows[number].clientwindow=clientwindow;

			/*** Nom de la fenetre ***/

    	ret=XFetchName(tk_display->display,clientwindow,&windows[number].title_name);
    	if(ret==0||windows[number].title_name==NULL)
    	{ 
		windows[number].hasStoredName=False;
	 	windows[number].title_name="(Pas de nom)";
/*		ret=XGetClassHint(tk_display->display,clientwindow,&classhint);
		if(ret!=0)
		{
		  windows[number].title_name=classhint.res_class;
		  windows[number].hasStoredName=True;
		}
*/
    	}
    	else windows[number].hasStoredName=True;
#ifdef DEBUG
	fprintf(stderr,"DIALOG :Fenetre %s\n",windows[number].title_name);
#endif

		/*** Configuration de la decoration ***/

    	add_leftside=0;
    	add_top=0;
	add_bottom=0;
	add_rightside=0;
	zadd_leftside=0;
    	zadd_top=0;
	zadd_bottom=0;
	zadd_rightside=0;

#ifdef HAS_SHAPE_EXTENSION_LIBRARY
	ret=XShapeQueryExtents(tk_display->display,windows[number].clientwindow,&bounding_shaped,&x_bounding,&y_bounding,&w_bounding,&h_bounding,&clip_shaped,&x_clip,&y_clip,&w_clip,&h_clip);
	if(ret!=0) XShapeSelectInput(tk_display->display,windows[number].clientwindow,ShapeNotifyMask);
	if(ret!=0&&(bounding_shaped==True||clip_shaped==True))
	  windows[number].identity.shape=True;
	else windows[number].identity.shape=False;
#else
	windows[number].identity.shape=False;
#endif

	WIN_GetAddDimensions(number);
	add_top=windows[number].normal.add_top;
	add_bottom=windows[number].normal.add_bottom;
	add_leftside=windows[number].normal.add_leftside;
	add_rightside=windows[number].normal.add_rightside;


#ifdef DEBUG
    	fprintf(stderr,"Recherche de la taille\n");
#endif


		  /******** Taille de la fenetre *********/

	
    	windows[number].normal.x=xwa.x;
    	windows[number].normal.y=xwa.y;
    	windows[number].normal.client_width=xwa.width;
    	windows[number].normal.client_height=xwa.height;
        windows[number].state.isMapped=False;


    	XSync(tk_display->display,False);
    	if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
    	{
    	  windows[number].isUsed=False;
    	  return -1;
    	}

    	ret=XGetNormalHints(tk_display->display, clientwindow, &hints);
    	hints.x=xwa.x;
    	hints.y=xwa.y;
    	hints.width=xwa.width;
    	hints.height=xwa.height;
    	if((hints.flags&PPosition)!=PPosition)
      	hints.flags+=PPosition;
    	if((hints.flags&PSize)!=PSize)
      	  hints.flags+=PSize;


    	if(ret!=0&&(hints.flags&PPosition)==PPosition && hints.flags<=256 && hints.flags>=0)
    	{
    	  windows[number].normal.x=hints.x;
    	  windows[number].normal.y=hints.y;
	  windows[number].normal.client_x=add_leftside;
	  windows[number].normal.client_y=add_top;
    	}


    	if(ret!=0&&(hints.flags&PSize)==PSize && hints.flags<=256 && hints.flags>=0)
    	{
	  if(hints.width>=MINWIDTH-add_leftside-add_rightside) windows[number].normal.client_width=hints.width; 
   	  else windows[number].normal.client_width=MINWIDTH-add_leftside-add_rightside;
   	  if(hints.height>=MINHEIGHT-add_top-add_bottom) windows[number].normal.client_height=hints.height;
   	  else windows[number].normal.client_height=MINHEIGHT-add_top-add_bottom; 
	  windows[number].normal.width=windows[number].normal.client_width+add_leftside+add_rightside;
	  windows[number].normal.height=windows[number].normal.client_height+add_top+add_bottom;
    	}
	else
	{
	  if(hints.width>=MINWIDTH-add_leftside-add_rightside) windows[number].normal.client_width=hints.width; 
   	  else windows[number].normal.client_width=MINWIDTH-add_leftside-add_rightside;
   	  if(hints.height>=MINHEIGHT-add_top-add_bottom) windows[number].normal.client_height=hints.height;
   	  else windows[number].normal.client_height=MINHEIGHT-add_top-add_bottom; 
	  windows[number].normal.width=windows[number].normal.client_width+add_leftside+add_rightside;
	  windows[number].normal.height=windows[number].normal.client_height+add_top+add_bottom;
	}

    	if(ret!=0&&(hints.flags&PMinSize)==PMinSize && hints.flags<=256 && hints.flags>0)
    	{
   	  if(hints.min_width+add_leftside+add_rightside>=MINWIDTH ) windows[number].normal.min_width=hints.min_width+add_leftside+add_rightside;
   	  else windows[number].normal.min_width=MINWIDTH;
   	  if(hints.min_height+add_top+add_bottom>=MINHEIGHT) windows[number].normal.min_height=hints.min_height+add_top+add_bottom;
   	  else windows[number].normal.min_height=MINHEIGHT;

   	  if(windows[number].normal.width<windows[number].normal.min_width) windows[number].normal.width=windows[number].normal.min_width;
   	  if(windows[number].normal.height<windows[number].normal.min_height) windows[number].normal.height=windows[number].normal.min_height;
    	}
    	else 
	{
    	  windows[number].normal.min_width=MINWIDTH;
    	  windows[number].normal.min_height=MINHEIGHT;
   	  if(windows[number].normal.width<windows[number].normal.min_width) windows[number].normal.width=windows[number].normal.min_width;
   	  if(windows[number].normal.height<windows[number].normal.min_height) windows[number].normal.height=windows[number].normal.min_height;
      	}

/**** VERIFIER QUE PMAXSIZE>PMINSIZE ****/

    	if(ret!=0&&(hints.flags&PMaxSize)==PMaxSize && hints.flags<=256 && hints.flags>0)
    	{
   	  if(hints.max_width+add_leftside+add_rightside<=MAXWIDTH && hints.max_width>0) windows[number].normal.max_width=hints.max_width+add_leftside+add_rightside;
   	  else windows[number].normal.max_width=MAXWIDTH;
   	  if(hints.max_height+add_top+add_bottom<=MAXHEIGHT && hints.max_height>0) windows[number].normal.max_height=hints.max_height+add_top+add_bottom;
   	  else windows[number].normal.max_height=MAXHEIGHT;

     	  if(windows[number].normal.width>windows[number].normal.max_width) windows[number].normal.width=windows[number].normal.max_width;
     	  if(windows[number].normal.height>windows[number].normal.max_height) windows[number].normal.height=windows[number].normal.max_height;
    	}
    	else 
	{
   	  windows[number].normal.max_width=MAXWIDTH;
   	  windows[number].normal.max_height=MAXHEIGHT;

     	  if(windows[number].normal.width>windows[number].normal.max_width) windows[number].normal.width=windows[number].normal.max_width;
     	  if(windows[number].normal.height>windows[number].normal.max_height) windows[number].normal.height=windows[number].normal.max_height;
     	}

  	windows[number].normal.client_width=windows[number].normal.width-add_leftside-add_rightside;
	windows[number].normal.client_height=windows[number].normal.height-add_bottom-add_top;
  	windows[number].normal.client_x=windows[number].normal.add_leftside;
	windows[number].normal.client_y=windows[number].normal.add_top;


     	windows[number].icon.name=windows[number].title_name;
	windows[number].icon.hasStoredIconName=False;
	windows[number].icon.pixmap=0;
	windows[number].icon.pix_width=windows[number].icon.pix_height=0;
	windows[number].icon.depth=0;
	windows[number].icon.x=0;
	windows[number].icon.y=0;
        
	
		    /**** gestion du transient ****/

	wm_action.type=ReparentingAction;
	WIN_SetTransientLeader(number);
	wm_action.type=NoAction;

		    /***** Gestion du groupe *****/

#ifdef DEBUG
    	fprintf(stderr,"Recherche du groupe\n");
#endif


    	if(xwmhints!=NULL&&(xwmhints->flags&WindowGroupHint)==WindowGroupHint&&xwmhints->flags<=256&&xwmhints->window_group>0&&windows[number].transient.isTransient==False)
    	{
	  if((win_attributes&GroupLeader)==GroupLeader && (xwmhints->window_group==clientwindow||xwmhints->window_group<=0))
	  {
		windows[number].group.leader=0;
		windows[number].group.number=-1;
	  }
     	  else if((win_attributes&GroupLeader)!=GroupLeader && (xwmhints->window_group==clientwindow||xwmhints->window_group<=0))
	  {
     	  	windows[number].attributes=windows[number].attributes+GroupLeader;
	  	windows[number].group.leader=0;
		windows[number].group.number=-1;
	  }
	  else if((win_attributes&GroupMember)==GroupMember && xwmhints->window_group!=clientwindow&&xwmhints->window_group>0)
	    windows[number].group.leader=xwmhints->window_group;
     	  else if((win_attributes&GroupMember)!=GroupMember && xwmhints->window_group!=clientwindow&&xwmhints->window_group>0)
    	  {
     	  	windows[number].attributes=windows[number].attributes+GroupMember;
     	  	windows[number].group.leader=xwmhints->window_group;
 	  }

#ifdef DEBUG
    	  fprintf(stderr,"Group: %ld  attribut:%d  flags:%d\n",windows[number].group,windows[number].attributes,xwmhints->flags);
#endif
     	  if((windows[number].attributes&GroupMember)==GroupMember)
     	  {
		WIN_AddToGroup(number);
		win_attributes=windows[number].attributes;
     	  }
#ifdef DEBUG
     	  fprintf(stderr,"Fin du Group\n");
#endif
     
    	}
    	else
	{
 	  windows[number].group.leader=0;
 	  windows[number].group.number=-1;
	}



		/**** gestion du colormap ****/


	
	if(xwa.colormap!=0)
	  windows[number].identity.colormap=xwa.colormap;
	else windows[number].identity.colormap=default_colormap;
#ifdef DEBUG
	fprintf(stderr,"DLG Colormap=%ld\n",windows[number].identity.colormap);
#endif

		/*** gestion des dimensions finales ***/

#ifdef DEBUG
    	fprintf(stderr,"Dimensions finales\n");
	fprintf(stderr,"add_top==%d \n",add_top);
#endif
    	windows[number].isUsed=True;
  
    	xwc.width=windows[number].normal.width;
    	xwc.height=windows[number].normal.height;
    	xwc.x=windows[number].normal.x;
    	xwc.y=windows[number].normal.y;
    	xwc.border_width=(windows[number].normal.border/2);
    	mask=CWWidth|CWHeight|CWX|CWY|CWBorderWidth;


    	XSync(tk_display->display,False);
    	if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
    	{
      	  windows[number].isUsed=0;
     	  return -1;
    	}


    	XConfigureWindow(tk_display->display,windows[number].mainwindow,mask,&xwc);
    	XSetWindowBackground(tk_display->display,windows[number].mainwindow,tk_display->dlg_colors.bg);
	XSetWindowBorder(tk_display->display,windows[number].mainwindow,tk_display->dlg_colors.border_inactive);


#ifdef HAS_SHAPE_EXTENSION_LIBRARY
	if(XShapeQueryVersion(tk_display->display,&major,&minor)!=0)
	{
	  XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,None,0);
	  XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,None,0);
	}

	if(windows[number].identity.shape==True)
	{
	  XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,None,0);
	  XShapeCombineMask(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,None,0);
#ifdef DEBUG
	  fprintf(stderr,"client_x=%d  client_y=%d\n",windows[number].normal.client_x,windows[number].normal.client_y);
	  fprintf(stderr,"client_width=%d  client_height=%d\n",windows[number].normal.client_width,windows[number].normal.client_height);
#endif
	  rectangle.x=windows[number].normal.client_x;	  
	  rectangle.y=windows[number].normal.client_y;
	  rectangle.width=windows[number].normal.client_width;
	  rectangle.height=windows[number].normal.client_height;
	  XShapeCombineRectangles(tk_display->display,windows[number].mainwindow,ShapeBounding,0,0,&rectangle,1,ShapeSubtract,Unsorted);
	  XShapeCombineRectangles(tk_display->display,windows[number].mainwindow,ShapeClip,0,0,&rectangle,1,ShapeSubtract,Unsorted);
																							
	  XShapeCombineShape(tk_display->display,windows[number].mainwindow,ShapeBounding,windows[number].normal.client_x,windows[number].normal.client_y,clientwindow,ShapeBounding,ShapeUnion);
	  XShapeCombineShape(tk_display->display,windows[number].mainwindow,ShapeClip,windows[number].normal.client_x,windows[number].normal.client_y,clientwindow,ShapeBounding,ShapeUnion);

	}
	
#endif


    	xwc.width=windows[number].normal.client_width;
    	xwc.height=windows[number].normal.client_height;
    	xwc.border_width=0;
    	mask=CWWidth|CWHeight|CWBorderWidth;
    	XConfigureWindow(tk_display->display,windows[number].clientwindow,mask,&xwc);

    	XUnmapWindow(tk_display->display,windows[number].titlebar);
    	xwc.width=windows[number].normal.width-2;
    	mask=CWWidth;
    	XConfigureWindow(tk_display->display,windows[number].titlebar,mask,&xwc);
    	if((win_attributes&TitleBar)==TitleBar) XMapWindow(tk_display->display,windows[number].titlebar);


#ifdef DEBUG
    	fprintf(stderr,"Widgets\n");
#endif

    	wid_Unmap(tk_display,windows[number].bn_close);
   	xwc.x=windows[number].normal.client_width-22-2;
    	mask=CWX;
    	wid_Configure(tk_display,windows[number].bn_close,xwc.x,0,0,0,CFX);

	if((win_attributes&CloseBox)==CloseBox && (win_attributes&TitleBar)==TitleBar) 
	  wid_Map(tk_display,windows[number].bn_close);



		/******** Reparentage du client ********/

#ifdef DEBUG
    	fprintf(stderr,"Reparentage final\n");
#endif

    	XSync(tk_display->display,False);
    	if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
    	{
   	  windows[number].isUsed=0;
   	  return -1;
    	}

    	if((win_attributes&TitleBar)!=TitleBar) XReparentWindow(tk_display->display,clientwindow,windows[number].mainwindow,0,0);
    	else if((win_attributes&TitleBar)==TitleBar) XReparentWindow(tk_display->display,clientwindow,windows[number].mainwindow,windows[number].normal.client_x,windows[number].normal.client_y);  


    	XSync(tk_display->display,False);
	XFlush(tk_display->display);
    	if(XCheckTypedWindowEvent(tk_display->display,clientwindow,DestroyNotify,&return_event)==True)
    	{
   	  windows[number].isUsed=0;
   	  return -1;
    	}


    	XAddToSaveSet(tk_display->display,clientwindow);

    	if(clientwindow==wm_kill_window||clientwindow==wm_desktop_window||clientwindow==wm_colors_window||clientwindow==wm_process_window||clientwindow==wm_about_window||clientwindow==wm_setup_window||clientwindow==wm_end_window)
    	{
    	  if(clientwindow==wm_desktop_window) wm_desktop_index=number;
    	  else if(clientwindow==wm_kill_window) wm_kill_index=number;
    	  else if(clientwindow==wm_colors_window) wm_colors_index=number;
    	  else if(clientwindow==wm_process_window) wm_process_index=number;
    	  else if(clientwindow==wm_about_window) wm_about_index=number;
    	  else if(clientwindow==wm_setup_window) wm_setup_index=number;
    	  else if(clientwindow==wm_end_window) wm_end_index=number;

	  XSelectInput(tk_display->display,clientwindow,ExposureMask|ColormapChangeMask);
    	}
    	else
	{
	    XSelectInput(tk_display->display,clientwindow,StructureNotifyMask|PropertyChangeMask|ColormapChangeMask);
    	}

    	xwc.border_width=0;
    	xwc.width=windows[number].normal.client_width;
    	xwc.height=windows[number].normal.client_height;
    	mask=CWBorderWidth|CWWidth|CWHeight;;
    	XConfigureWindow(tk_display->display,clientwindow,mask,&xwc);

    	windows[number].attributes=win_attributes;
    	WIN_SetClientAttributes(number,win_attributes);
    	WIN_SetClientState(number,DontCareState);


			/*** Grab des touches ***/

    	if(clientwindow!=wm_kill_window&&clientwindow!=wm_desktop_window&&clientwindow!=wm_colors_window&&clientwindow!=wm_process_window&&clientwindow!=wm_about_window&&clientwindow!=wm_setup_window&&clientwindow!=wm_end_window) 
    	{
		XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Delete),ControlMask|Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
		XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Escape),Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
		XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Escape),ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);

/*		if(strcmp(ServerVendor(tk_display->display),"DESQview/X (R), by Quarterdeck Office Systems")!=0)
		{
		  XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Break),0,clientwindow,False,GrabModeAsync,GrabModeAsync);
		  XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Menu),0,clientwindow,False,GrabModeAsync,GrabModeAsync);
		}*/

		/*XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Break),ControlMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
		XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Menu),ControlMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
		XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Break),ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
		XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Menu),ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
		XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Break),Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
		XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Menu),Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
		*/
    	}
    	if(clientwindow!=wm_process_window&&clientwindow!=wm_kill_window&&clientwindow!=wm_end_window) 
    	{
    	  	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Escape),ControlMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  	/*XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Break),ControlMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Menu),ControlMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Break),ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Menu),ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Break),Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Menu),Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
		*/
    	}
    	if(clientwindow==wm_process_window||clientwindow==wm_kill_window) 
    	{

	  	/*XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Break),ControlMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Menu),ControlMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Break),ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Menu),ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Break),Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	  	XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Menu),Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
      	  	*/
    	}

    	if(clientwindow!=wm_end_window&&clientwindow!=wm_kill_window) 
	{
    	  XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_End),ControlMask|Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
	}

    	ret=XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Print),ControlMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
    	ret=XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Print),ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
    	ret=XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Print),ControlMask|ShiftMask,clientwindow,False,GrabModeAsync,GrabModeAsync);
    	ret=XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Print),ControlMask|Mod1Mask,clientwindow,False,GrabModeAsync,GrabModeAsync);
    	ret=XGrabKey(tk_display->display,XKeysymToKeycode(tk_display->display,XK_Print),0,clientwindow,False,GrabModeAsync,GrabModeAsync);


        if(XKeysymToKeycode(tk_display->display,XK_End)<=0)
	  fprintf(stderr,"!!! XK_End keycode=0\n !!!");
        if(XKeysymToKeycode(tk_display->display,XK_Delete)<=0)
	  fprintf(stderr,"!!! XK_Delete keycode=0\n !!!");

    	xswa.save_under=xwa.save_under;
	xswa.win_gravity=NorthWestGravity;
    	XChangeWindowAttributes(tk_display->display,windows[number].mainwindow,CWSaveUnder|CWWinGravity,&xswa);

    	if(xwmhints!=NULL) XFree(xwmhints);

	send_event.type=ConfigureNotify;
	send_event.xconfigure.window=clientwindow;
	send_event.xconfigure.send_event=True;
	send_event.xconfigure.x=windows[number].normal.x;
	send_event.xconfigure.y=windows[number].normal.y;
	send_event.xconfigure.width=windows[number].normal.client_width;
	send_event.xconfigure.height=windows[number].normal.client_height;
	send_event.xconfigure.border_width=windows[number].normal.border/2;

    	if(clientwindow!=wm_kill_window&&clientwindow!=wm_desktop_window&&clientwindow!=wm_colors_window&&clientwindow!=wm_process_window&&clientwindow!=wm_about_window&&clientwindow!=wm_setup_window&&clientwindow!=wm_end_window) 	
	  XSendEvent(tk_display->display,clientwindow,True,False,&send_event);

	WIN_GetProtocols(number);
 	ret=XGetWindowAttributes(tk_display->display,clientwindow,&xwa);
 	if (ret==0||(ret==1&&(xwa.your_event_mask==NoEventMask||xwa.your_event_mask==0)))
 	  goto REPARENT_END;
#ifdef DEBUG
	fprintf(stderr,"event mask = %d\n",xwa.your_event_mask);
	fprintf(stderr,"StructureNotifyMask=%d   &event_mask=%d\n",StructureNotifyMask,(StructureNotifyMask&xwa.your_event_mask));
	ret=XGetWindowAttributes(tk_display->display,windows[number].mainwindow,&xwa);
	fprintf(stderr,"ret=%d  SubstructureRedirectMask=%d  &event_mask=%d\n",ret,SubstructureRedirectMask,(SubstructureRedirectMask&xwa.your_event_mask));
	fprintf(stderr,"DIALOG reparent fin  number:%d\n",number);
	windows[number].clientwindow=clientwindow;
#endif
	return 0;
	break;

 }
 else return -1;

}






/*

int WIN_VerifyLinks(number)
int number;
{
 int i, j;
 Window transient;
 XEvent send_event;


 if(number<0 || number>=maxwindows || windows[number].isUsed==False) 
   return -1;


 if(numwindows>0&&(windows[number].attributes&GroupLeader)==GroupLeader)
 for(i=0;i<maxwindows;i++) if(windows[i].isUsed==True&&(windows[i].attributes&GroupMember)==GroupMember&&windows[i].group==windows[number].clientwindow)
 {
   if(windows[number].numgroupmembers>0)
   {
     for(j=0;j<windows[number].numgroupmembers;j++)
     {
       if(windows[number].groupmembers[j]==i)
 	 goto VERIFY_SUITE;
     }
     WIN_AddToGroupMembers(i);
   }
   else WIN_AddToGroupMembers(i);
VERIFY_SUITE:
   j=j;
 }


 if(numwindows>0)
 for(i=0;i<maxwindows;i++) if(windows[i].isUsed==True&&windows[i].isTransient==True&&(windows[i].window_transient_type==-1||windows[i].window_transient_number==-1))
 {
   XGetTransientForHint(tk_display->display,windows[i].clientwindow,&transient);
   if(windows[number].clientwindow==transient)
   {
	windows[i].window_transient_type=TOP_LEVEL_WINDOW;
   	windows[i].window_transient_number=number;
   	windows[i].isTransient=True;	
 	if(windows[i].isMapped==True)
	{
	  windows[windows[i].window_transient_number].initialstate=windows[windows[i].window_transient_number].state;
	  windows[windows[i].window_transient_number].state=InactiveState;
	  WIN_SetClientState(windows[i].window_transient_number,InactiveState);
	  send_event.type=ClientMessage;
	  send_event.xclient.window=windows[windows[i].window_transient_number].clientwindow;
	  send_event.xclient.message_type=WmFreezeWidgets;
	  send_event.xclient.format=32;
	  XSendEvent(tk_display->display,windows[windows[i].window_transient_number].clientwindow,False,0,&send_event);
	  XSync(tk_display->display, False);
	}
	goto VERIFY_END;
   }
 }


VERIFY_END:
 return 0;
}




*/









