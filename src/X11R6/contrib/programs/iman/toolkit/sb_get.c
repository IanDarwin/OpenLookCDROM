/*
 *
 * 	sb_get.c  
 * 	informations sur les ascenseurs
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




int SB_GetPosition(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 return scroll->position;
}




int SB_GetState(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 return scroll->state;
}



int SB_GetRange(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 return scroll->range;
}




int SB_GetPagerange(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 return scroll->pagerange;
}




int SB_GetThumbsize(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 return scroll->thumbsize;
}




