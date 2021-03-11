/*
 *
 * 	cb_create.c  
 * 	creation des boites Combo
 *
 * 	Modification :  05/12/93
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include <X11/iman/widgets.h>





/*
 *
 * Creation d'une "combo box" 
 *
 *
 */

int CB_Create(tk_display,comboid,parent,top_level,x,y,width,height,type,state,vtype,font,itemheight)
TkDisplay *tk_display;
ComboID comboid;
Window parent, top_level;
int x, y;
unsigned int width, height;
int type, state, vtype,itemheight;
XFontStruct *font;
{
 ComboStruct *combo;
 ListStruct *list;
 EditStruct *edit;
 ButtonStruct *button;
 WidgetAttributes wid_attributes;
 WidgetStruct *widptr;

 XSetWindowAttributes attrib;
 unsigned long mask;
 unsigned long ptr;
 int tc_x, tc_y;
 Window tc_child;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;
 widptr=&tk_display->widgets[comboid];

 widptr->class=WI_COMBO;
 widptr->type=type;
 widptr->colors=&tk_display->cb_colors;

 attrib.background_pixel=tk_display->ls_colors.bg;
 attrib.cursor=tk_display->widgets[comboid].identity.cursor;
 attrib.colormap=tk_display->widgets[comboid].identity.colormap;
 attrib.event_mask=KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|FocusChangeMask; 
 attrib.bit_gravity=NorthWestGravity;
 mask=CWBackPixel|CWEventMask|CWCursor|CWBitGravity|CWColormap;

 combo->window=XCreateWindow(tk_display->display, parent, x, y, width, itemheight+6, 1, tk_display->depth, InputOutput,tk_display->widgets[comboid].identity.visual, mask,&attrib);
 XSetWindowBorder(tk_display->display,combo->window,tk_display->ls_colors.text);



 combo->parent=parent;
 combo->top_level=top_level; 

 if(font!=NULL) 
   combo->font=font; 
 else combo->font=tk_display->fonts.helvetica12;
 combo->type=type;
 combo->state=state;
 combo->flags=0;
 combo->attribut=0;

 combo->x = x;
 combo->y = y;
 combo->width = width;
 combo->height = height;

 combo->parency_class=0;
 combo->parency_number=-1;
 combo->hasFocus=combo->isOpen=0;
 combo->wid_number=comboid;
 
 wid_attributes.visual=tk_display->widgets[comboid].identity.visual;
 wid_attributes.colormap=tk_display->widgets[comboid].identity.colormap;

 switch(type){

	case CB_LEFTEDITION:  
	case CB_LEFTNOEDITION:
			wid_attributes.mask=SAColormap+SAVisual;
			combo->button=wid_Create(tk_display,WI_BUTTON,BN_COMBOBUTTON,combo->window,top_level,-1,-1,19,itemheight+6,&wid_attributes,state);
			button=tk_display->widgets[combo->button].button;
			button->neverFocus=True;
			button->parency_class=WI_COMBO;
			button->parency_number=comboid;
			fprintf(stderr,"Button passe %d\n",combo->button);

			combo->edit=wid_Create(tk_display,WI_EDIT,ED_FULLSELECT,combo->window,top_level,19,-1,width-20,itemheight+6,&wid_attributes,state);
			edit=tk_display->widgets[combo->edit].edit; 
			ED_SetFont(tk_display,combo->edit,font,True);
			if(type==CB_LEFTNOEDITION) edit->neverFocus=True;
			edit->parency_class=WI_COMBO;
			edit->parency_number=comboid;
			fprintf(stderr,"Edit passe %d\n",combo->edit);

			wid_attributes.itemheight=itemheight;
			wid_attributes.vtype=vtype;
			wid_attributes.htype=SB_LEFTALIGN;
			wid_attributes.mask=SAItemHeight+SAHVType;
			XTranslateCoordinates(tk_display->display,parent,top_level,x,y+itemheight+6+1,&tc_x,&tc_y,&tc_child);
			combo->list=wid_Create(tk_display,WI_LIST,LS_SIMPLE,top_level,top_level,tc_x,tc_y,width,height-itemheight-6-1,&wid_attributes,state);
			fprintf(stderr,"List passe %d\n",combo->list);
			list=tk_display->widgets[combo->list].list; 
			list->parency_class=WI_COMBO;
			list->parency_number=comboid;

			break;



	case CB_SLEFTEDITION:  
	case CB_SLEFTNOEDITION:
			wid_attributes.mask=SAColormap+SAVisual;
			combo->button=wid_Create(tk_display,WI_BUTTON,BN_COMBOBUTTON,combo->window,top_level,-1,-1,19,itemheight+6,&wid_attributes,state);
			button=tk_display->widgets[combo->button].button;
			button->neverFocus=True;
			button->parency_class=WI_COMBO;
			button->parency_number=comboid;

			
			combo->edit=wid_Create(tk_display,WI_EDIT,ED_FULLSELECT,combo->window,top_level,19,-1,width-20,itemheight+6,&wid_attributes,state);
			edit=tk_display->widgets[combo->edit].edit;
			ED_SetFont(tk_display,combo->edit,font,True);
			if(type==CB_SLEFTNOEDITION) 
			  edit->neverFocus=True;
			edit->parency_class=WI_COMBO;
			edit->parency_number=comboid;

			wid_attributes.itemheight=itemheight;
			wid_attributes.vtype=vtype;
			wid_attributes.htype=SB_LEFTALIGN;
			wid_attributes.mask=SAItemHeight+SAHVType;
			XTranslateCoordinates(tk_display->display,parent,top_level,x,y+itemheight+6+1,&tc_x,&tc_y,&tc_child);
			combo->list=wid_Create(tk_display,WI_LIST,LS_LEFTVSCROLL,top_level,top_level,tc_x,tc_y,width,height-itemheight-6-1,&wid_attributes,state);
			list=tk_display->widgets[combo->list].list;
			list->parency_class=WI_COMBO;
			list->parency_number=comboid;

			break;

	case CB_RIGHTEDITION:  
	case CB_RIGHTNOEDITION:
			wid_attributes.mask=SAVisual+SAColormap;
			combo->button=wid_Create(tk_display,WI_BUTTON,BN_COMBOBUTTON,combo->window,top_level,width-20,-1,19,itemheight+6,&wid_attributes,state);
			button=tk_display->widgets[combo->button].button;
			button->neverFocus=True;
			button->parency_class=WI_COMBO;
			button->parency_number=comboid;

			combo->edit=wid_Create(tk_display,WI_EDIT,ED_FULLSELECT,combo->window,top_level,-1,-1,width-20,itemheight+6,&wid_attributes,state);
			edit=tk_display->widgets[combo->edit].edit;
			ED_SetFont(tk_display,combo->edit,font,True);
			if(type==CB_RIGHTNOEDITION) 
			  edit->neverFocus=True;
			edit->parency_class=WI_COMBO;
			edit->parency_number=comboid;

			wid_attributes.itemheight=itemheight;
			wid_attributes.vtype=vtype;
			wid_attributes.htype=SB_LEFTALIGN;
			wid_attributes.mask=SAItemHeight+SAHVType;
			XTranslateCoordinates(tk_display->display,parent,top_level,x,y+itemheight+6+1,&tc_x,&tc_y,&tc_child);
			combo->list=wid_Create(tk_display,WI_LIST,LS_SIMPLE,top_level,top_level,tc_x,tc_y,width,height-itemheight-6-1,&wid_attributes,state);
			list=tk_display->widgets[combo->list].list;
			list->parency_class=WI_COMBO;
			list->parency_number=comboid;
			break;
	
	case CB_SRIGHTEDITION:  
	case CB_SRIGHTNOEDITION:
			wid_attributes.mask=SAVisual+SAColormap;
			combo->button=wid_Create(tk_display,WI_BUTTON,BN_COMBOBUTTON,combo->window,top_level,width-20,-1,19,itemheight+6,&wid_attributes,state);
			button=tk_display->widgets[combo->button].button;
			button->neverFocus=True;
			button->parency_class=WI_COMBO;
			button->parency_number=comboid;

			combo->edit=wid_Create(tk_display,WI_EDIT,ED_FULLSELECT,combo->window,top_level,-1,-1,width-20,itemheight+6,&wid_attributes,state);
			edit=tk_display->widgets[combo->edit].edit;
			ED_SetFont(tk_display,combo->edit,font,True);
			if(type==CB_SRIGHTNOEDITION) edit->neverFocus=True;
			edit->parency_class=WI_COMBO;
			edit->parency_number=comboid;

			wid_attributes.itemheight=itemheight;
			wid_attributes.vtype=vtype;
			wid_attributes.htype=SB_LEFTALIGN;
			wid_attributes.mask=SAItemHeight+SAHVType;
			XTranslateCoordinates(tk_display->display,parent,top_level,x,y+itemheight+6+1,&tc_x,&tc_y,&tc_child);
			combo->list=wid_Create(tk_display,WI_LIST,LS_RIGHTVSCROLL,top_level,top_level,tc_x,tc_y,width,height-itemheight-6-1,&wid_attributes,state);
			list=tk_display->widgets[combo->list].list;
			list->parency_class=WI_COMBO;
			list->parency_number=comboid;
			break;


	default: break;

 }

 ptr=top_level;
 XChangeProperty(tk_display->display,combo->window,tk_display->atoms.WM_TOP_LEVEL,XA_DRAWABLE,32,PropModeReplace,(char *)&ptr,1);
 LS_ForbidMultipleSelection(tk_display,combo->list);
 return 0;

}








int CB_Destroy(tk_display,comboid)
TkDisplay *tk_display;
ComboID comboid;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;


 BN_Destroy(tk_display,combo->button);
 ED_Destroy(tk_display,combo->edit);
 LS_Destroy(tk_display,combo->list);

 XDestroySubwindows(tk_display->display,combo->window);
 XDestroyWindow(tk_display->display,combo->window);
 WID_Remove(tk_display,comboid);

 return 0;
}

