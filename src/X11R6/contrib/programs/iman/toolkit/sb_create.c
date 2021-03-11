/*
 *
 * 	sb_create.c  
 * 	creation des ascenseurs
 *
 * 	Modification :  27/11/93
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
#include "X11/Xatom.h"

#include <X11/iman/widgets.h>





		     /******* Creation d'un scrollbar *******/


int SB_Create(tk_display,scrollid,type,parent,top_level,x,y,width,height,state,range,pagerange,thumbsize) 
TkDisplay *tk_display;
ScrollbarID scrollid;
Window parent, top_level;
int x, y, type, state,range,pagerange;
unsigned int width, height,thumbsize;
{
 XSetWindowAttributes attrib;
 ButtonID buttonid;
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 WidgetStruct *scrollptr;
 WidgetAttributes wid_attributes;
 unsigned long mask, ptr;



 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;
 scrollptr=&tk_display->widgets[scrollid];



 attrib.override_redirect=True;
 attrib.background_pixel=tk_display->sb_colors.bg;
 attrib.border_pixel=tk_display->sb_colors.nofocus;
 attrib.event_mask=ExposureMask|ButtonPressMask|ButtonReleaseMask|StructureNotifyMask;

 if(type==SB_TOPALIGN || type==SB_BOTTOMALIGN || type==SB_VTHUMB)
   scrollptr->identity.cursor=attrib.cursor=tk_display->cursors.sb_up;
 else if(type==SB_LEFTALIGN ||type==SB_RIGHTALIGN ||type==SB_HTHUMB) 
   scrollptr->identity.cursor=attrib.cursor=tk_display->cursors.sb_left;
 attrib.colormap=scrollptr->identity.colormap;
 mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor|CWOverrideRedirect|CWColormap;


 scroll->parent=parent;
 scroll->top_level=top_level;											
 scroll->mainwindow=XCreateWindow(tk_display->display,parent, x, y, width, height, 1,tk_display->depth, InputOutput, tk_display->widgets[scrollid].identity.visual, mask, &attrib);
 scroll->type=type;
 scroll->state=state;
 scroll->x = x;
 scroll->y = y;
 scroll->width = width;
 scroll->height = height;
 scroll->range=range;
 scroll->pagerange=pagerange;
 scroll->position=0;
 scroll->thumbsize=thumbsize;
 scroll->parency_class=0;
 scroll->parency_number=-1;
 scroll->wid_number=scrollid;

 attrib.event_mask=ButtonPressMask|ButtonReleaseMask|StructureNotifyMask; 


 wid_attributes.neverFocus=True;
 wid_attributes.lighting=False;
 wid_attributes.colormap=tk_display->widgets[scrollid].identity.colormap;
 wid_attributes.visual=tk_display->widgets[scrollid].identity.visual;
 wid_attributes.mask=SALighting+SANeverFocus+SADirection+SAColormap+SAVisual+SACursor; 


 switch(scroll->type){

   case SB_TOPALIGN:
 
	wid_attributes.cursor=tk_display->cursors.normal;
	scroll->thumbwindow=XCreateWindow(tk_display->display, scroll->mainwindow, 0, 36, width, height-36, 0,tk_display->depth, InputOutput,CopyFromParent,mask,&attrib);

	wid_attributes.direction=UP;
	scroll->B1=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,0,0,width-2,16,&wid_attributes,state);

	wid_attributes.direction=DOWN;
	scroll->B2=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,0,18,width-2,16,&wid_attributes,state);

	wid_attributes.cursor=tk_display->cursors.up_down;	scroll->bn_thumb=wid_Create(tk_display,WI_BUTTON,BN_THUMBBUTTON,scroll->thumbwindow,top_level,0,0,width-2,scroll->thumbsize,&wid_attributes,state);
	
	break;


   case SB_BOTTOMALIGN:

	wid_attributes.cursor=tk_display->cursors.normal;
	scroll->thumbwindow=XCreateWindow(tk_display->display, scroll->mainwindow, 0, 0, width, height-36, 0, tk_display->depth, InputOutput,CopyFromParent, mask, &attrib); 

	wid_attributes.direction=UP;
	scroll->B1=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,0,scroll->height-36,width-2,16,&wid_attributes,state);

	wid_attributes.direction=DOWN;
	scroll->B2=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,0,scroll->height-18,width-2,16,&wid_attributes,state);
	
	wid_attributes.cursor=tk_display->cursors.up_down;		scroll->bn_thumb=wid_Create(tk_display,WI_BUTTON,BN_THUMBBUTTON,scroll->thumbwindow,top_level,0,0,width-2,scroll->thumbsize,&wid_attributes,state);

	break;


 case SB_VTHUMB:

	wid_attributes.cursor=tk_display->cursors.normal;
        scroll->thumbwindow=XCreateWindow(tk_display->display, scroll->mainwindow, 0, 0, width, height, 0, tk_display->depth, InputOutput,CopyFromParent, mask, &attrib); 

	wid_attributes.direction=UP;
	scroll->B1=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,0,0,width-2,16,&wid_attributes,state);

	wid_attributes.direction=DOWN;
	scroll->B2=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,0,0,width-2,16,&wid_attributes,state);
	
	wid_attributes.cursor=tk_display->cursors.up_down;		scroll->bn_thumb=wid_Create(tk_display,WI_BUTTON,BN_THUMBBUTTON,scroll->thumbwindow,top_level,0,0,width-2,scroll->thumbsize,&wid_attributes,state);

	break;


 case SB_LEFTALIGN: 

 	wid_attributes.cursor=tk_display->cursors.normal;
	scroll->thumbwindow=XCreateWindow(tk_display->display, scroll->mainwindow, 38, 0, width-38, height, 0, tk_display->depth, InputOutput,CopyFromParent, mask, &attrib); 

	wid_attributes.direction=LEFT;
	scroll->B1=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,0,0,17,height-2,&wid_attributes,state);

	wid_attributes.direction=RIGHT;
	scroll->B2=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,19,0,17,height-2,&wid_attributes,state);

	wid_attributes.cursor=tk_display->cursors.left_right;	scroll->bn_thumb=wid_Create(tk_display,WI_BUTTON,BN_THUMBBUTTON,scroll->thumbwindow,top_level,0,0,scroll->thumbsize,height-2,&wid_attributes,state);
	break;


 case SB_RIGHTALIGN:

	wid_attributes.cursor=tk_display->cursors.normal;
	scroll->thumbwindow=XCreateWindow(tk_display->display, scroll->mainwindow, 0, 0, width-38, height, 0, tk_display->depth, InputOutput,CopyFromParent, mask, &attrib); 

	wid_attributes.direction=LEFT;
	scroll->B1=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,scroll->width-38,0,17,height-2,&wid_attributes,state);

	wid_attributes.direction=RIGHT;
	scroll->B2=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,scroll->width-19,0,17,height-2,&wid_attributes,state);

	wid_attributes.cursor=tk_display->cursors.left_right;		scroll->bn_thumb=wid_Create(tk_display,WI_BUTTON,BN_THUMBBUTTON,scroll->thumbwindow,top_level,0,0,scroll->thumbsize,height-2,&wid_attributes,state);

	break;

 case SB_HTHUMB:

	wid_attributes.cursor=tk_display->cursors.normal;
        scroll->thumbwindow=XCreateWindow(tk_display->display, scroll->mainwindow, 0, 0, width, height, 0, tk_display->depth, InputOutput,CopyFromParent, mask, &attrib); 

	wid_attributes.direction=LEFT;
	scroll->B1=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,0,0,width-2,19,&wid_attributes,state);

	wid_attributes.direction=RIGHT;
	scroll->B2=wid_Create(tk_display,WI_BUTTON,BN_SCROLLBUTTON,scroll->mainwindow,top_level,0,0,width-2,19,&wid_attributes,state);

	wid_attributes.cursor=tk_display->cursors.left_right;		scroll->bn_thumb=wid_Create(tk_display,WI_BUTTON,BN_THUMBBUTTON,scroll->thumbwindow,top_level,0,0,scroll->thumbsize,height-2,&wid_attributes,state);

	break;


 }

 XSetWindowBorder(tk_display->display,tk_display->widgets[scroll->B1].button->window,tk_display->sb_colors.nofocus);
 XSetWindowBorder(tk_display->display,tk_display->widgets[scroll->B2].button->window,tk_display->sb_colors.nofocus);
 XSetWindowBorder(tk_display->display,tk_display->widgets[scroll->bn_thumb].button->window,tk_display->sb_colors.nofocus);

 /*fprintf(stderr,"Creation: SB1=%d  SB2=%d  SB3=%d \n",scroll->B1, scroll->B2, scroll->bn_thumb);*/
 button=tk_display->widgets[scroll->B1].button;
 button->parency_class=WI_SCROLLBAR;
 button->parency_number=scrollid;
 button=tk_display->widgets[scroll->B2].button;
 button->parency_class=WI_SCROLLBAR;
 button->parency_number=scrollid;
 button=tk_display->widgets[scroll->bn_thumb].button;
 button->parency_class=WI_SCROLLBAR;
 button->parency_number=scrollid;


 ptr=top_level;
 XChangeProperty(tk_display->display,scroll->thumbwindow,tk_display->atoms.WM_TOP_LEVEL,XA_DRAWABLE,32,PropModeReplace,(char *)&ptr,1);
 ptr=top_level;
 XChangeProperty(tk_display->display,scroll->mainwindow,tk_display->atoms.WM_TOP_LEVEL,XA_DRAWABLE,32,PropModeReplace,(char *)&ptr,1);

}





			/********** Destruction d'un scrollbar *********/


int SB_Destroy(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;


 BN_Destroy(tk_display,scroll->B1);
 BN_Destroy(tk_display,scroll->B2);
 BN_Destroy(tk_display,scroll->bn_thumb);

 XDestroySubwindows(tk_display->display,scroll->mainwindow);
 XDestroyWindow(tk_display->display,scroll->mainwindow);

 WID_Remove(tk_display,scrollid);
 return 0;
}

