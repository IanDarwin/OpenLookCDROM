/*
 *
 * 	ls_create.c  
 * 	creation des listes
 *
 * 	Modification :  04/12/93
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





/*
 *
 * Creation d'une liste 
 *
 *
 */



int LS_Create(tk_display,listid,parent,top_level,x,y,width,height,type,state,htype,vtype,itemheight,font,border) 
TkDisplay *tk_display;
ListID listid;
Window parent,top_level;
int x, y;
unsigned int width, height;
int type, state, htype, vtype,itemheight;
XFontStruct *font;
Bool border;
{
 XSetWindowAttributes attrib;
 unsigned long mask;
 unsigned long ptr;
 ListStruct *list;
 ScrollbarStruct *scroll;
 ScrollbarID scrollid;
 WidgetAttributes wid_attributes;
 WidgetStruct *widptr;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;
 widptr=&tk_display->widgets[listid];

 widptr->type=type;
 widptr->number=listid; 
 list->parent=parent;
 list->top_level=top_level; 

 list->itemheight=itemheight;
 list->font=font; 

 list->type=type;
 list->state=state;


 list->maxheight=itemheight;
 list->numitems=list->maxitems=0;
 list->selected=False;
 list->selecteditem=0;
 if(border==0) list->flags=NoBorderFlag;
 else list->flags=0;

 list->topitem=0;
 list->hpos=0;

 attrib.event_mask=FocusChangeMask;
 attrib.background_pixel=tk_display->ls_colors.bg;
 attrib.cursor=tk_display->widgets[listid].identity.cursor;
 attrib.colormap=tk_display->widgets[listid].identity.colormap;
 attrib.bit_gravity=NorthWestGravity;
 attrib.override_redirect=True;
 mask=CWBackPixel|CWEventMask|CWCursor|CWBitGravity|CWOverrideRedirect|CWColormap;


 if(border==1) list->mainwindow=XCreateWindow(tk_display->display, parent,x,y,width,height,1,tk_display->depth, InputOutput, tk_display->widgets[listid].identity.visual, mask, &attrib);
 else list->mainwindow=XCreateWindow(tk_display->display, parent,x,y,width,height,0,tk_display->depth, InputOutput,tk_display->widgets[listid].identity.visual, mask, &attrib);
 XSetWindowBorder(tk_display->display,list->mainwindow,tk_display->ls_colors.text);


 list->x=x;
 list->y = y;
 list->width = width;
 list->height = height;
 list->maxwidth=list->width;

 list->parency_class=0;
 list->parency_number=-1;
 list->wid_number=listid;
 widptr->colors=&tk_display->ls_colors;
 wid_attributes.colormap=tk_display->widgets[listid].identity.colormap;
 wid_attributes.visual=tk_display->widgets[listid].identity.visual;


 switch(type){

	case LS_SIMPLE:  
			 htype=SB_HTHUMB;
			 vtype=SB_VTHUMB;
			 attrib.background_pixel=tk_display->ls_colors.bg;
			 attrib.event_mask=ExposureMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|FocusChangeMask;
			 
			 list->listwindow=XCreateWindow(tk_display->display, list->mainwindow,-1,-1,list->width,list->height,1,tk_display->depth, InputOutput,tk_display->widgets[listid].identity.visual, mask, &attrib);
			 
			 list->downitem=((list->height-LS_UMARGE)/list->itemheight)-1;
			 wid_attributes.range=0;
			 wid_attributes.pagerange=1;
			 wid_attributes.thumbsize=27;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAVisual+SAColormap;
			 list->SBH=wid_Create(tk_display,WI_SCROLLBAR,htype,list->mainwindow,top_level,-1,50000,width,19,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBH].scroll;
			 scroll->parency_class=WI_LIST;
			 scroll->parency_number=listid;

			 wid_attributes.range=0;
			 wid_attributes.pagerange=list->downitem+1;
			 wid_attributes.thumbsize=30;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAVisual+SAColormap;
		 	 list->SBV=wid_Create(tk_display,WI_SCROLLBAR,vtype,list->mainwindow,top_level,50000,-1,19,list->height,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBV].scroll;
			 scroll->parency_class=WI_LIST;
			 scroll->parency_number=listid;
			 return 0;
			 break;


	case LS_HSCROLL:if(htype==SB_LEFTALIGN || htype==SB_RIGHTALIGN || htype==SB_HTHUMB)
			{
			 vtype=SB_VTHUMB;
			 attrib.background_pixel=tk_display->ls_colors.bg;
			 attrib.event_mask=ExposureMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|FocusChangeMask;

			 list->listwindow=XCreateWindow(tk_display->display, list->mainwindow,-1,-1,width,height-20,1,tk_display->depth, InputOutput,tk_display->widgets[listid].identity.visual, mask, &attrib);
			 XSetWindowBorder(tk_display->display,list->listwindow,tk_display->ls_colors.text);
			 
			 list->downitem=((height-20-LS_UMARGE)/list->itemheight)-1;
			 wid_attributes.range=0;
			 wid_attributes.pagerange=1;
			 wid_attributes.thumbsize=27;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAVisual+SAColormap;
			 list->SBH=wid_Create(tk_display,WI_SCROLLBAR,htype,list->mainwindow,top_level,-1,list->height-20,width,19,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBH].scroll;
			 scroll->parency_class=WI_LIST;
			 scroll->parency_number=listid;

			 wid_attributes.range=0;
			 wid_attributes.pagerange=list->downitem+1;
			 wid_attributes.thumbsize=30;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAVisual+SAColormap;
		 	 list->SBV=wid_Create(tk_display,WI_SCROLLBAR,vtype,list->mainwindow,top_level,50000,-1,19,list->height,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBV].scroll;
			 scroll->parency_class=WI_LIST;
			 scroll->parency_number=listid;
/*
			 list->width = width;
			 list->height = height-20;
*/
			}
			else return -1;
			break;

	case LS_RIGHTVSCROLL:

			if(vtype==SB_TOPALIGN || vtype==SB_BOTTOMALIGN || vtype==SB_VTHUMB)
			{
			 htype=SB_HTHUMB;
			 list->maxwidth=list->width-20;
			 attrib.background_pixel=tk_display->ls_colors.bg;
			 attrib.event_mask=ExposureMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|FocusChangeMask;
			 list->listwindow=XCreateWindow(tk_display->display, list->mainwindow,-1,-1,list->width-20,list->height,1,tk_display->depth, InputOutput,tk_display->widgets[listid].identity.visual, mask, &attrib);
			 XSetWindowBorder(tk_display->display,list->listwindow,tk_display->ls_colors.text);

			 list->downitem=((height-LS_UMARGE)/list->itemheight)-1;

			 wid_attributes.range=0;
			 wid_attributes.pagerange=1;
			 wid_attributes.thumbsize=27;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAColormap+SAVisual;
			 list->SBH=wid_Create(tk_display,WI_SCROLLBAR,htype,list->mainwindow,top_level,-1,50000,width,19,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBH].scroll;
			 scroll->parency_class=WI_LIST;
			 scroll->parency_number=listid;

			 wid_attributes.range=0;
			 wid_attributes.pagerange=list->downitem+1;
			 wid_attributes.thumbsize=30;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAColormap+SAVisual;
		 	 list->SBV=wid_Create(tk_display,WI_SCROLLBAR,vtype,list->mainwindow,top_level,list->width-20,-1,19,list->height,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBV].scroll;
			 scroll->parency_class=WI_LIST;
			 scroll->parency_number=listid;
/*	
			 list->width = width-20;
			 list->height = height;
*/			 			 
			}
			else return -1;
			break;


	case LS_LEFTVSCROLL:

			if(vtype==SB_TOPALIGN || vtype==SB_BOTTOMALIGN || vtype==SB_VTHUMB)
			{
			 htype=SB_HTHUMB;
			 list->maxwidth=list->width-20;
			 attrib.background_pixel=tk_display->ls_colors.bg;
			 attrib.event_mask=ExposureMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|FocusChangeMask;
			 list->listwindow=XCreateWindow(tk_display->display, list->mainwindow,19,-1,list->width-20,list->height,1,tk_display->depth, InputOutput,tk_display->widgets[listid].identity.visual, mask, &attrib);

			 XSetWindowBorder(tk_display->display,list->listwindow,tk_display->ls_colors.text);

			 list->downitem=((height-LS_UMARGE)/list->itemheight)-1;
			 wid_attributes.range=0;
			 wid_attributes.pagerange=1;
			 wid_attributes.thumbsize=27;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAColormap+SAVisual;
			 list->SBH=wid_Create(tk_display,WI_SCROLLBAR,htype,list->mainwindow,top_level,-1,50000,width,19,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBH].scroll;
			 scroll->parency_class=WI_LIST;
			 scroll->parency_number=listid;

			 wid_attributes.range=0;
			 wid_attributes.pagerange=list->downitem+1;
			 wid_attributes.thumbsize=30;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAColormap+SAVisual;
			 list->SBV=wid_Create(tk_display,WI_SCROLLBAR,vtype,list->mainwindow,top_level,-1,-1,19,list->height,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBV].scroll;
			 scroll->parency_number=listid;
			 scroll->parency_class=WI_LIST;
/*
			 list->width = width-20;
			 list->height = height;
*/
			}
			else return -1;
			break;

	case LS_HRIGHTVSCROLL:
			
			if(vtype==SB_TOPALIGN || vtype==SB_BOTTOMALIGN || vtype==SB_VTHUMB)
			{
			 list->maxwidth=list->width-20;
			 attrib.background_pixel=tk_display->ls_colors.bg;
			 attrib.event_mask=ExposureMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|FocusChangeMask;
			 list->listwindow=XCreateWindow(tk_display->display, list->mainwindow,-1,-1,list->width-20,list->height-20,1,tk_display->depth, InputOutput,tk_display->widgets[listid].identity.visual, mask, &attrib);

			 XSetWindowBorder(tk_display->display,list->listwindow,tk_display->ls_colors.text);
			 
			 list->downitem=((height-20-LS_UMARGE)/list->itemheight)-1;
			 wid_attributes.range=0;
			 wid_attributes.pagerange=list->downitem+1;
			 wid_attributes.thumbsize=30;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAColormap+SAVisual;
			 list->SBV=wid_Create(tk_display,WI_SCROLLBAR,vtype,list->mainwindow,top_level,list->width-20,-1,19,list->height-20,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBV].scroll;
			 scroll->parency_number=listid;
			 scroll->parency_class=WI_LIST;

			 wid_attributes.range=0;
			 wid_attributes.pagerange=1;
			 wid_attributes.thumbsize=27;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAColormap+SAVisual;
			 list->SBH=wid_Create(tk_display,WI_SCROLLBAR,htype,list->mainwindow,top_level,-1,list->height-20,list->width,19,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBH].scroll;
			 scroll->parency_number=listid;
			 scroll->parency_class=WI_LIST;
/*
			 list->width = width-20;
			 list->height = height-20;
*/
			 
			}
			else return -1;
			break;


	case LS_HLEFTVSCROLL:
			
			if(vtype==SB_TOPALIGN || vtype==SB_BOTTOMALIGN || vtype==SB_VTHUMB)
			{
			 list->maxwidth=list->width-20;
			 attrib.background_pixel=tk_display->ls_colors.bg;
			 attrib.event_mask=ExposureMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|FocusChangeMask;
			 list->listwindow=XCreateWindow(tk_display->display, list->mainwindow,19,-1,list->width-20,list->height-20,1,tk_display->depth, InputOutput,tk_display->widgets[listid].identity.visual,mask, &attrib);

			 XSetWindowBorder(tk_display->display,list->listwindow,tk_display->ls_colors.text);

			 list->downitem=((height-20-LS_UMARGE)/list->itemheight)-1;
			 wid_attributes.range=0;
			 wid_attributes.pagerange=list->downitem+1;
			 wid_attributes.thumbsize=30;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAColormap+SAVisual;
			 list->SBV=wid_Create(tk_display,WI_SCROLLBAR,vtype,list->mainwindow,top_level,-1,-1,19,list->height-20,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBV].scroll;
			 scroll->parency_number=listid;
			 scroll->parency_class=WI_LIST;

			 wid_attributes.range=0;
			 wid_attributes.pagerange=1;
			 wid_attributes.thumbsize=27;
			 wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAColormap+SAVisual;
			 list->SBH=wid_Create(tk_display,WI_SCROLLBAR,htype,list->mainwindow,top_level,-1,list->height-20,list->width,19,&wid_attributes,Grayed);
			 scroll=tk_display->widgets[list->SBH].scroll;
			 scroll->parency_number=listid;
			 scroll->parency_class=WI_LIST;
/*
			 list->width = width-20;
			 list->height = height-20;
*/

			}
			else return -1;
			break;


	default: break;

 }


 ptr=top_level;
 XChangeProperty(tk_display->display,list->mainwindow,tk_display->atoms.WM_TOP_LEVEL,XA_DRAWABLE,32,PropModeReplace,(char *)&ptr,1);
 ptr=top_level;
 XChangeProperty(tk_display->display,list->listwindow,tk_display->atoms.WM_TOP_LEVEL,XA_DRAWABLE,32,PropModeReplace,(char *)&ptr,1);

/*
 ptr=LS_MAINWINDOW;
 XChangeProperty(tk_display->display, list->mainwindow, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32,PropModeReplace,&ptr,1);
 ptr=(long)(list);
 XChangeProperty(tk_display->display, list->mainwindow, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32,PropModeAppend,&ptr,1);

 ptr=LS_LISTWINDOW;
 XChangeProperty(tk_display->display, list->listwindow, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeReplace,&ptr,1);
*/

 return 0;
}






int LS_Destroy(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;

 
 SB_Destroy(tk_display,list->SBH);
 SB_Destroy(tk_display,list->SBV);

 if(list->maxitems>0) free(list->items);
 XDestroySubwindows(tk_display->display,list->mainwindow);
 XDestroyWindow(tk_display->display,list->mainwindow);
 WID_Remove(tk_display,listid);

 return 0;
}


