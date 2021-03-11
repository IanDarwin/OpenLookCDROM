/*
 *
 * 	sb_draw.c  
 * 	affichage des ascenseurs
 *
 * 	Modification :  27/11/93
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

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"

#include <X11/iman/widgets.h>





			/******** Affichage du scrollbar ********/



int SB_Map(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;


 XMapRaised(tk_display->display,scroll->mainwindow);			

 if(scroll->type!=SB_HTHUMB && scroll->type!=SB_VTHUMB) 
   BN_Map(tk_display,scroll->B1);

 if(scroll->type!=SB_HTHUMB && scroll->type!=SB_VTHUMB)
   BN_Map(tk_display,scroll->B2);

 XMapWindow(tk_display->display, scroll->thumbwindow);			
 BN_Map(tk_display,scroll->bn_thumb); 
 return 0;
}





int SB_Unmap(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{ 
 ScrollbarStruct *scroll;

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 XUnmapWindow(tk_display->display, scroll->mainwindow);			 
 return 0;
}





int SB_MapThumb(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 BN_Map(tk_display,scroll->bn_thumb); 
 return 0;
}





int SB_UnmapThumb(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{ 
 ScrollbarStruct *scroll;

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 BN_Unmap(tk_display,scroll->bn_thumb); 
 return 0;
}



