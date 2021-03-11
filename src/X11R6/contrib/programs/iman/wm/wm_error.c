/*
 *
 * 	wm_error.c
 * 	gestion des erreurs
 *
 * 	Modification :  13/05/94
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

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "iman.h"









int WM_SendError(error,problem_window)
unsigned int error;
Window problem_window;
{
 int w, ret;
 char *str;

 switch(error){


	case AnotherWMError:

		/*XGrabServer(tk_display->display);*/
		XMapWindow(tk_display->display,wm_error_window);		
		wid_Unmap(tk_display,bn_error_ok);
		wid_Map(tk_display,bn_error_cancel);
		wid_GiveFocus(tk_display,bn_error_cancel);
		while(1)
 		{
     		  tk_GetWidgetEvents(tk_display,&tk_event,False);
		  /*ret=XGrabPointer(tk_display->display,wm_error_window,True,0,GrabModeAsync,GrabModeAsync,wm_error_window,tk_display->cursors.normal,CurrentTime);
		  if(ret==GrabNotViewable||ret==AlreadyGrabbed||ret==GrabFrozen||ret==GrabInvalidTime)
		    fprintf(stderr,"Grab pointer error: %d\n",ret);*/
		  switch(tk_event.ev_type){

  		  	case XLIBEVENT: 

				switch(tk_event.event.type){

					case Expose:

						if(tk_event.event.xexpose.count==0&&tk_event.event.xexpose.window==wm_error_window)
						{		
						  /*XUngrabServer(tk_display->display);*/
						  XSetForeground(tk_display->display,error_gc,BlackPixel(tk_display->display,tk_display->screen));
						  XSetBackground(tk_display->display,error_gc,WhitePixel(tk_display->display,tk_display->screen));
						  w=strlen(str="Another WM is already running ! !");
						  XSetFont(tk_display->display,error_gc,tk_display->fonts.helvetica12->fid);
						  XDrawString(tk_display->display,wm_error_window,error_gc,(ERROR_BOX_WIDTH-XTextWidth(tk_display->fonts.helvetica12,"Un autre Window Manager est deja charge",w))/2,60,str,w);
						  w=strlen(str="Impossible to control the X server resources");
						  XDrawString(tk_display->display,wm_error_window,error_gc,(ERROR_BOX_WIDTH-XTextWidth(tk_display->fonts.helvetica12,"Impossible de controler les ressources",w))/2,90,str,w);
						  w=strlen(str=" ");
						  XDrawString(tk_display->display,wm_error_window,error_gc,(ERROR_BOX_WIDTH-XTextWidth(tk_display->fonts.helvetica12,"du serveur X11 ...",w))/2,106,str,w);
						  XSync(tk_display->display,False);
						  /*XGrabServer(tk_display->display);*/
						}
						else if(tk_event.event.xexpose.count==0&&tk_event.event.xexpose.window==wm_error_title)
						{		
						  /*XUngrabServer(tk_display->display);*/
						  XSetForeground(tk_display->display,error_gc,tk_display->dlg_colors.title_text_active);
						  XSetBackground(tk_display->display,error_gc,tk_display->dlg_colors.title_bg_active);
						  w=strlen(str="System error");
						  XSetFont(tk_display->display,error_gc,tk_display->fonts.helvetica12->fid);
						  XDrawString(tk_display->display,tk_event.event.xexpose.window,error_gc,(ERROR_BOX_WIDTH-2-XTextWidth(tk_display->fonts.helvetica12,"Erreur systeme",w))/2,TITLEBARHEIGHT-7,str,w);
						  XSync(tk_display->display,False);
						  /*XGrabServer(tk_display->display);*/
						}
						break;
				 }
				break;

			case BN_RELEASED:

				/*XUngrabServer(tk_display->display);
				XUngrabPointer(tk_display->display,CurrentTime);*/
				XSync(tk_display->display,False);
				if(tk_event.button==bn_error_ok){ 
					XUnmapWindow(tk_display->display,wm_error_window);
					return 1;}
				else if(tk_event.button==bn_error_cancel){ 
					XUnmapWindow(tk_display->display,wm_error_window);
					return 0;}
				break;
			}
		}
		break;


	case NoMemoryError :
		break;



 }

}








int WM_Usage(argc,argv)
int argc;
char **argv;
{

 fprintf(stderr,"Usage: iman [-options]\n\n");
 fprintf(stderr,"Options:  -display   X server to contact\n");
 /*fprintf(stderr,"          -mode      mode de synchronisation: sync/async\n");*/
 /*fprintf(stderr,"          -login     affichage de la fenetre de login\n");*/
 fprintf(stderr,"          -?, -help  displays this help\n");

}




