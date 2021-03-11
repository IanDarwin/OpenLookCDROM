/*
 *
 * 	kill_draw.c
 * 	affichage de la fenetre de destruction
 *
 * 	Modification :  02/05/94
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
#include "bm/warning.bm"



static char *fr_kill_text[]=
{
	"Vous avez utilise CTRL+ALT+DEL pour quitter",
	"un processus en cours d'execution.",
	"Terminer cette application peut rendre instable",
	"le serveur X et menacer les services du", 
	"gestionnaire de fenetres.",
	"Ne continuez qui si le processus est dangereux ou",
	"ne peut etre termine autrement."
};


static char *kill_text[]=
{
	"You pressed CTRL+ALT+DEL to kill",
	"a window running under X.",
	"Stopping this process might damage",
	"the X server and threaten the IMAN", 
	"window manager.",
	"Do *NOT* continue if the process is not dangerous",
	"or can be stopped another way."
};



void KILL_Draw()
{

 XGCValues xgcvalues;
 XEvent send_event;

 
 unsigned long ptr;
 int mask, ret;
 int i, j, k, w;


 if(tk_event.event.xexpose.window==wm_kill_window)
 {
 
   XSetForeground(tk_display->display,kill_gc,BlackPixel(tk_display->display,tk_display->screen));
   XSetBackground(tk_display->display,kill_gc,WhitePixel(tk_display->display,tk_display->screen));
	  
   XSetFont(tk_display->display,kill_gc,tk_display->fonts.helvetica12->fid);
   /*XUngrabServer(tk_display->display);*/

   w=strlen(kill_text[0]);
   XDrawString(tk_display->display,wm_kill_window,kill_gc,68,60,kill_text[0],w);
   w=strlen(kill_text[1]);
   XDrawString(tk_display->display,wm_kill_window,kill_gc,68,76,kill_text[1],w);
   w=strlen(kill_text[2]);
   XDrawString(tk_display->display,wm_kill_window,kill_gc,68,106,kill_text[2],w);
   w=strlen(kill_text[3]);
   XDrawString(tk_display->display,wm_kill_window,kill_gc,68,122,kill_text[3],w);
   w=strlen(kill_text[4]);
   XDrawString(tk_display->display,wm_kill_window,kill_gc,68,138,kill_text[4],w);
   w=strlen(kill_text[5]);
   XDrawString(tk_display->display,wm_kill_window,kill_gc,68,168,kill_text[5],w);
   w=strlen(kill_text[6]);
   XDrawString(tk_display->display,wm_kill_window,kill_gc,68,184,kill_text[6],w);


   /*XGrabServer(tk_display->display);*/
   
 }

}





void KILL_Map(clientwindow)
Window clientwindow;
{
 WIN_MapRaised(wm_kill_index);
 XGrabServer(tk_display->display);
 XSync(tk_display->display,False);
 windows[wm_kill_index].state.isInitialized=True; 
 windows[wm_kill_index].state.isMapped=True;

 if(windows[wm_process_index].state.isMapped==True||windows[wm_process_index].state.isOnTop==True)
   WID_Freeze(tk_display,wm_process_window);
 WM_FreezeAll();

 /*wid_Map(tk_display,bn_kill_cancel);
 wid_Map(tk_display,bn_kill_ok);*/

 wid_GiveFocus(tk_display,bn_kill_cancel);
 wm_action.type=KillProcessAction; 		
 wm_action.window=clientwindow;
 wm_action.number=WIN_GetNumber(wm_action.window);
 XGrabServer(tk_display->display);
 wm_action.type=KillProcessAction;
 XSync(tk_display->display,False);

}






