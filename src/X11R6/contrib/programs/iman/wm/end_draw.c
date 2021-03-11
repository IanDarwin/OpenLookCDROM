/*
 *
 * 	end_draw.c  
 * 	affichage de la fenetre de fermeture
 *
 * 	Modification :  18/04/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
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


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "iman.h"





void END_Draw()
{

 XGCValues xgcvalues;
 XEvent send_event;

 char *str; 
 unsigned long ptr;
 int mask, ret;
 int i, j, k, w;


 if(tk_event.event.xexpose.window==wm_end_window)
 {
 
   XSetForeground(tk_display->display,end_gc,tk_display->dlg_colors.text);
   XSetBackground(tk_display->display,end_gc,tk_display->dlg_colors.bg);
	  
   XSetFont(tk_display->display,end_gc,tk_display->fonts.helvetica12->fid);
   /*XUngrabServer(tk_display->display);*/

   /*str="Vous allez mettre fin a tous les services du";*/ 
   str="You are going to shut down all the services";
   w=strlen(str);
   XDrawString(tk_display->display,wm_end_window,end_gc,50,55,str,w);
 
   /*str="gestionnaire de fenetres IMAN.";*/
   str="offered by the IMAN window manager";
   w=strlen(str);
   XDrawString(tk_display->display,wm_end_window,end_gc,50,71,str,w);

   /*str="Le presse-papier, la gestion des processus,";*/
   str="The clipboard, the process administrator,";
   w=strlen(str);
   XDrawString(tk_display->display,wm_end_window,end_gc,50,103,str,w);
 
   /*str="l'administration des palettes de couleurs et";*/
   str="colormap and window management";
   w=strlen(str);
   XDrawString(tk_display->display,wm_end_window,end_gc,50,119,str,w);
 
   /*str="le reparentage des fenetres vont s'arreter";*/
   str="will be stopped.";
   w=strlen(str);
   XDrawString(tk_display->display,wm_end_window,end_gc,50,135,str,w);

   /*str="Confirmez en appuyant sur 'fin' ou annulez.";*/
   str="Press 'End' if you agree or cancel.";
   w=strlen(str);
   XDrawString(tk_display->display,wm_end_window,end_gc,50,165,str,w);



   /*XGrabServer(tk_display->display);*/
   
 }

}





void END_Map(clientwindow)
Window clientwindow;
{
 windows[wm_end_index].state.isInitialized=True; 
 WIN_MapRaised(wm_end_index);
 XGrabServer(tk_display->display);
 XSync(tk_display->display,False);
 windows[wm_end_index].state.isMapped=True;
 

 WID_Freeze(tk_display,wm_colors_window);
 WID_Freeze(tk_display,wm_process_window);
 WID_Freeze(tk_display,wm_about_window);
 WID_Freeze(tk_display,wm_desktop_window);
 WID_Freeze(tk_display,wm_clipboard_window);
 WID_Freeze(tk_display,wm_setup_window);
 WM_FreezeAll();

 wid_Map(tk_display,bn_end_cancel);
 wid_Map(tk_display,bn_end_ok);

 wid_GiveFocus(tk_display,bn_end_cancel);
 wm_action.type=EndOfAllAction; 		
 wm_action.window=clientwindow;
 wm_action.number=WIN_GetNumber(wm_action.window);

 XGrabServer(tk_display->display);
 wm_action.type=EndOfAllAction;
 XSync(tk_display->display,False);
}






