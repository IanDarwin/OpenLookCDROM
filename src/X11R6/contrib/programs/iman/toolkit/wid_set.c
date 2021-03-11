/*
 *
 * 	wid_set.c  
 * 	modification des widgets
 *
 * 	Modification :  30/01/94
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

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>





/* 
 *
 * Freeze tous les widgets
 *
 *
 */

int wid_SetFreeze(tk_display,window,mode)
TkDisplay *tk_display;
Window window;
Bool mode;
{

  if(mode==True)
    return WID_Freeze(tk_display,window);
  else return WID_Unfreeze(tk_display,window);
}





int WID_Freeze(tk_display, window)
TkDisplay *tk_display;
Window window;
{
 int i;

 
 for(i=0;i<tk_display->maxwidgets;i++)
 {
  if(tk_display->widgets[i].isUsed==True) switch(tk_display->widgets[i].class)
  {
    case WI_BUTTON: if(tk_display->widgets[i].button->top_level==window)
		    {
 		      if((tk_display->widgets[i].button->state&Frozen)!=Frozen)
			tk_display->widgets[i].button->state=tk_display->widgets[i].button->state+Frozen;
		    }
		    break;

    case WI_SCROLLBAR : if(tk_display->widgets[i].scroll->top_level==window)
 		    {
		      if((tk_display->widgets[i].scroll->state&Frozen)!=Frozen)
			tk_display->widgets[i].scroll->state=tk_display->widgets[i].scroll->state+Frozen;
		    }
		    break;

    case WI_EDIT : if(tk_display->widgets[i].edit->top_level==window)
 		   {
		      if((tk_display->widgets[i].edit->state&Frozen)!=Frozen)
			tk_display->widgets[i].edit->state=tk_display->widgets[i].edit->state+Frozen;
		   }
		   break;

    case WI_LIST : if(tk_display->widgets[i].list->top_level==window)
 		   {
		      if((tk_display->widgets[i].list->state&Frozen)!=Frozen)
			tk_display->widgets[i].list->state=tk_display->widgets[i].list->state+Frozen;
		   } 		   
		   break;

    case WI_COMBO : if(tk_display->widgets[i].combo->top_level==window)
 		    {
		      if((tk_display->widgets[i].combo->state&Frozen)!=Frozen)
			tk_display->widgets[i].combo->state=tk_display->widgets[i].combo->state+Frozen;
		    } 	
		    break;

    case WI_MENU : if(tk_display->widgets[i].menu->top_level==window)
 		   {
		      if((tk_display->widgets[i].menu->state&Frozen)!=Frozen)
			tk_display->widgets[i].menu->state=tk_display->widgets[i].menu->state+Frozen;
		   }
		   break;

  }
 }
 
 return -1;

}






/* 
 *
 * Unfreeze tous les widgets
 *
 *
 */

int WID_Unfreeze(tk_display, window)
TkDisplay *tk_display;
Window window;
{
 int i;

 
 for(i=0;i<tk_display->maxwidgets;i++)
 {
  if(tk_display->widgets[i].isUsed==True) switch(tk_display->widgets[i].class)
  {
    case WI_BUTTON: if(tk_display->widgets[i].button->top_level==window)
		    {
 		      if((tk_display->widgets[i].button->state&Frozen)==Frozen)
			tk_display->widgets[i].button->state=tk_display->widgets[i].button->state-Frozen;
		    }
		    break;

    case WI_SCROLLBAR : if(tk_display->widgets[i].scroll->top_level==window)
 		    {
		      if((tk_display->widgets[i].scroll->state&Frozen)==Frozen)
			tk_display->widgets[i].scroll->state=tk_display->widgets[i].scroll->state-Frozen;
		    }
		    break;

    case WI_EDIT : if(tk_display->widgets[i].edit->top_level==window)
 		   {
		      if((tk_display->widgets[i].edit->state&Frozen)==Frozen)
			tk_display->widgets[i].edit->state=tk_display->widgets[i].edit->state-Frozen;
		   }
		   break;

    case WI_LIST : if(tk_display->widgets[i].list->top_level==window)
 		   {
		      if((tk_display->widgets[i].list->state&Frozen)==Frozen)
			tk_display->widgets[i].list->state=tk_display->widgets[i].list->state-Frozen;
		   } 		   
		   break;

    case WI_COMBO : if(tk_display->widgets[i].combo->top_level==window)
 		    {
		      if((tk_display->widgets[i].combo->state&Frozen)==Frozen)
			tk_display->widgets[i].combo->state=tk_display->widgets[i].combo->state-Frozen;
		    } 	
		    break;

    case WI_MENU : if(tk_display->widgets[i].menu->top_level==window)
 		   {
		      if((tk_display->widgets[i].menu->state&Frozen)==Frozen)
			tk_display->widgets[i].menu->state=tk_display->widgets[i].menu->state-Frozen;
		   }
		   break;

  }
 }
 
 return -1;

}






int WID_SetOverride(tk_display,bool)
TkDisplay *tk_display;
Bool bool;
{
 int i;
 XSetWindowAttributes xswa;
 unsigned long mask;


 if(tk_display==NULL) return -1;

 xswa.override_redirect=bool;
 mask=CWOverrideRedirect;

 if(tk_display->maxwidgets>0) 
 for(i=0;i<tk_display->maxwidgets;i++)
 if(tk_display->widgets[i].isUsed==True && tk_display->widgets[i].class==WI_MENU && tk_display->widgets[i].type==MN_FLOATING)
 { 
     /*fprintf(stderr,"set override =%d  ",bool);*/
     XChangeWindowAttributes(tk_display->display,tk_display->widgets[i].menu->window,mask,&xswa);
 }
 return 0;

}







int wid_SetState(tk_display,widgetid,state)
TkDisplay *tk_display;
WidgetID widgetid;
unsigned int state;
{
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 EditStruct *edit;
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 unsigned int oldstate;


 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_BUTTON :
		
		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_BUTTON&&tk_display->widgets[widgetid].button!=NULL)
		  button=tk_display->widgets[widgetid].button;
		else return -1;

		oldstate=button->state;

 		if((state&Blocked)==Blocked && (button->state&Blocked)!=Blocked) 
		  button->state=button->state+Blocked;
		else if((state&Blocked)!=Blocked && (button->state&Blocked)==Blocked) 
  		  button->state=button->state-Blocked;

		if((state&Pushed)==Pushed && (button->state&Pushed)!=Pushed)
		  button->state=button->state+Pushed;
		else if((state&Pushed)!=Pushed && (button->state&Pushed)==Pushed)
		  button->state=button->state-Pushed;

 		if((state&Grayed)==Grayed && (button->state&Grayed)!=Grayed)
 		  button->state=button->state+Grayed;
		else if((state&Grayed)!=Grayed && (button->state&Grayed)==Grayed)
 		  button->state=button->state-Grayed;


		if((oldstate&Grayed)!=(button->state&Grayed)||(oldstate&Pushed)!=(button->state&Pushed))
		{
   		  if (button->type ==BN_PUSHBUTTON) 	   bn_DrawPushButton(tk_display,widgetid);
   		  else if(button->type == BN_REPEATBUTTON) bn_DrawRepeatButton(tk_display,widgetid);
   		  else if(button->type == BN_POPUPBUTTON)  bn_DrawPopupButton(tk_display,widgetid);
   		  else if(button->type == BN_CROSSBUTTON)  bn_DrawCrossButton(tk_display,widgetid);
   		  else if(button->type == BN_RADIOBUTTON)  bn_DrawRadioButton(tk_display,widgetid);
   		  else if(button->type == BN_POPUPRADIOBUTTON) bn_DrawPopupRadioButton(tk_display,widgetid); 
   		  else if(button->type == BN_SCROLLBUTTON) bn_DrawScrollButton(tk_display,widgetid);
   		  else if(button->type == BN_THUMBBUTTON)  bn_DrawThumbButton(tk_display,widgetid); 
   		  else if(button->type == BN_CHECKBUTTON)  bn_DrawCheckButton(tk_display,widgetid);
   		  else if(button->type == BN_COMBOBUTTON)  bn_DrawComboButton(tk_display,widgetid);
		}

 		return button->state;
		break;


	case WI_SCROLLBAR :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_SCROLLBAR&&tk_display->widgets[widgetid].scroll!=NULL)
		  scroll=tk_display->widgets[widgetid].scroll;
		else return -1;

		oldstate=scroll->state;

 		if((state&Grayed)==Grayed && (scroll->state&Grayed)!=Grayed)
 		  return SB_Gray(tk_display,widgetid);
		else if((state&Grayed)!=Grayed && (scroll->state&Grayed)==Grayed)
 		  return SB_Ungray(tk_display,widgetid);

		else return -1;
		break;


	case WI_EDIT :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_EDIT&&tk_display->widgets[widgetid].edit!=NULL)
		  edit=tk_display->widgets[widgetid].edit;
		else return -1;

		oldstate=edit->state;

 		if((state&Grayed)==Grayed && (edit->state&Grayed)!=Grayed)
 		  return ED_Gray(tk_display,widgetid);
		else if((state&Grayed)!=Grayed && (edit->state&Grayed)==Grayed)
 		  return ED_Ungray(tk_display,widgetid);

		else return -1;		
		break;


	case WI_LIST :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_LIST&&tk_display->widgets[widgetid].list!=NULL)
		  list=tk_display->widgets[widgetid].list;
		else return -1;

		oldstate=list->state;

 		if((state&Grayed)==Grayed && (list->state&Grayed)!=Grayed)
 		  return LS_Gray(tk_display,widgetid);
		else if((state&Grayed)!=Grayed && (list->state&Grayed)==Grayed)
 		  return LS_Ungray(tk_display,widgetid);

		else return -1;	
		break;


	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		  combo=tk_display->widgets[widgetid].combo;
		else return -1;

		oldstate=combo->state;

 		if((state&Grayed)==Grayed && (combo->state&Grayed)!=Grayed)
 		  return CB_Gray(tk_display,widgetid);
		else if((state&Grayed)!=Grayed && (combo->state&Grayed)==Grayed)
 		  return CB_Ungray(tk_display,widgetid);

		else return -1;	
		break;


	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		  menu=tk_display->widgets[widgetid].menu;
		else return -1;

		oldstate=menu->state;

 		if((state&Grayed)==Grayed && (menu->state&Grayed)!=Grayed)
 		  return MN_Gray(tk_display,widgetid);
		else if((state&Grayed)!=Grayed && (menu->state&Grayed)==Grayed)
 		  return MN_Ungray(tk_display,widgetid);

		else return -1;			
		break;

	default : return -1;
 		  break;

   }

 }
 else return -1;
}







int wid_SetTextDecoration(tk_display,widgetid,wid_text,delay)
TkDisplay *tk_display;
WidgetID widgetid;
WidgetTextDecoration *wid_text;
Bool delay;
{
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 EditStruct *edit;
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 char *text;
 XFontStruct *font;
 unsigned int key;
 unsigned int gravity;
 int x, y, ret;



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_BUTTON :
		
		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_BUTTON&&tk_display->widgets[widgetid].button!=NULL)
		{
		  button=tk_display->widgets[widgetid].button;
		  
		  if((wid_text->mask&STFont)==STFont) font=wid_text->font;
		  else font=button->txt_font;
		  BN_SetFont(tk_display,widgetid,font,True);


		  if((wid_text->mask&STText)==STText)
		  {
			if(wid_text->text==NULL)
			{
			  BN_DeleteText(tk_display,widgetid,delay);
			  return 0;
			} 
			else text=wid_text->text;
		  }
		  else text=button->text;

		  if((wid_text->mask&STKey)==STKey) key=wid_text->key;
		  else key=button->txt_key;

		  if((wid_text->mask&STGravity)==STGravity) gravity=wid_text->gravity;
		  else gravity=button->txt_gravity;	

		  if((wid_text->mask&STX)==STX) x=wid_text->x;
		  else x=button->txt_x;

		  if((wid_text->mask&STY)==STY) y=wid_text->y;
		  else y=button->txt_y;

		  return BN_SetText(tk_display,widgetid,text,key,gravity,x,y,delay);

		}
		else return -1;
		break;



	case WI_SCROLLBAR :

		return -1;
		break;



	case WI_EDIT :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_EDIT&&tk_display->widgets[widgetid].edit!=NULL)
		{
		  edit=tk_display->widgets[widgetid].edit;
		  
		  if((wid_text->mask&STFont)==STFont) font=wid_text->font;
		  else font=edit->font;
		  ret=ED_SetFont(tk_display,widgetid,font,True);


		  if((wid_text->mask&STText)==STText)
		  {
			if(wid_text->text==NULL)
			  return ED_SetText(tk_display,widgetid,"",delay);
			
			else text=wid_text->text;
			return ED_SetText(tk_display,widgetid,text,delay); 
		  }
		  else return ret;
		}
		else return -1;
		break;



	case WI_LIST :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_LIST&&tk_display->widgets[widgetid].list!=NULL)
		{  
		  list=tk_display->widgets[widgetid].list;

		  if((wid_text->mask&STFont)==STFont) font=wid_text->font;
		  else font=list->font;
		  return LS_SetFont(tk_display,widgetid,font,True);
		}
		else return -1;
		break;



	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		{
		  combo=tk_display->widgets[widgetid].combo;

		  if((wid_text->mask&STFont)==STFont)
		  { 
		    font=wid_text->font;
		    ED_SetFont(tk_display,combo->edit,font,True);
		    if((wid_text->mask&STText)==STText)
		    {
		      if(wid_text->text==NULL) ED_SetText(tk_display,combo->edit,"",delay);
		      else ED_SetText(tk_display,combo->edit,wid_text->text,delay);
		    }
		    return LS_SetFont(tk_display,combo->list,font,True);
		  }
		  else if((wid_text->mask&STText)==STText)
		  {
		    if(wid_text->text==NULL) return ED_SetText(tk_display,combo->edit,"",delay);
		    else return ED_SetText(tk_display,combo->edit,wid_text->text,delay);
		  }
		  else return 0;
		}
		else return -1;
		break;



	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		{
		  menu=tk_display->widgets[widgetid].menu;
		  if((wid_text->mask&STFont)==STFont)
		  { 
		    font=wid_text->font;
		    return MN_SetFont(tk_display,widgetid,font,True);
		  }  
		  else return 0;
		}
		else return -1;
		break;



	default : return -1;
 		  break;

   }

 }
 else return -1;

}







int wid_SetPixmapDecoration(tk_display,widgetid,wid_pixmap,delay)
TkDisplay *tk_display;
WidgetID widgetid;
WidgetPixmapDecoration *wid_pixmap;
Bool delay;
{
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 EditStruct *edit;
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 Pixmap pixmap, pixmap_mask;
 unsigned int depth;
 unsigned int gravity;
 int x, y, ret;
 unsigned int width, height;



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_BUTTON :
		
		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_BUTTON&&tk_display->widgets[widgetid].button!=NULL)
		{
		  button=tk_display->widgets[widgetid].button;
		  
		  if((wid_pixmap->mask&SPPixmap)==SPPixmap) pixmap=wid_pixmap->pixmap;
		  else pixmap=button->pixmap;
		  if(pixmap==(Pixmap)0)
		    return BN_DeletePixmap(tk_display,widgetid,delay);

		  
		  if((wid_pixmap->mask&SPPixmapMask)==SPPixmapMask) pixmap_mask=wid_pixmap->pixmap_mask;
		  else pixmap_mask=button->pix_mask;

		  if((wid_pixmap->mask&SPDepth)==SPDepth) depth=wid_pixmap->depth;
		  else depth=button->pix_depth;

		  if((wid_pixmap->mask&SPGravity)==SPGravity) gravity=wid_pixmap->gravity;
		  else gravity=button->pix_gravity;	

		  if((wid_pixmap->mask&SPX)==SPX) x=wid_pixmap->x;
		  else x=button->pix_x;

		  if((wid_pixmap->mask&SPY)==SPY) y=wid_pixmap->y;
		  else y=button->pix_y;

		  if((wid_pixmap->mask&SPWidth)==SPWidth) width=wid_pixmap->width;
		  else width=button->pix_width;

		  if((wid_pixmap->mask&SPHeight)==SPHeight) height=wid_pixmap->height;
		  else height=button->pix_height;

		  return BN_SetPixmap(tk_display,widgetid,pixmap,pixmap_mask,gravity,x,y,width,height,depth,delay);

		}
		else return -1;
		break;



	default : return -1;
 		  break;

   }

 }
 else return -1;

}







int wid_SetPixmapGrayedDecoration(tk_display,widgetid,wid_pixmap,delay)
TkDisplay *tk_display;
WidgetID widgetid;
WidgetPixmapDecoration *wid_pixmap;
Bool delay;
{
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 EditStruct *edit;
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 Pixmap pixmap, pixmap_mask;
 unsigned int depth;
 unsigned int gravity;
 int x, y, ret;
 unsigned int width, height;



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_BUTTON :
		
		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_BUTTON&&tk_display->widgets[widgetid].button!=NULL)
		{
		  button=tk_display->widgets[widgetid].button;
		  
		  if((wid_pixmap->mask&SPPixmap)==SPPixmap) pixmap=wid_pixmap->pixmap;
		  else pixmap=button->pixmap_grayed;
		  if(pixmap==(Pixmap)0)
		    return BN_DeletePixmapGrayed(tk_display,widgetid,delay);

		  if((wid_pixmap->mask&SPPixmapMask)==SPPixmapMask) pixmap_mask=wid_pixmap->pixmap_mask;
		  else pixmap_mask=button->pixgrayed_mask;

		  if((wid_pixmap->mask&SPDepth)==SPDepth) depth=wid_pixmap->depth;
		  else depth=button->pixgrayed_depth;

		  if((wid_pixmap->mask&SPGravity)==SPGravity) gravity=wid_pixmap->gravity;
		  else gravity=button->pixgrayed_gravity;	

		  if((wid_pixmap->mask&SPX)==SPX) x=wid_pixmap->x;
		  else x=button->pixgrayed_x;

		  if((wid_pixmap->mask&SPY)==SPY) y=wid_pixmap->y;
		  else y=button->pixgrayed_y;

		  if((wid_pixmap->mask&SPWidth)==SPWidth) width=wid_pixmap->width;
		  else width=button->pixgrayed_width;

		  if((wid_pixmap->mask&SPHeight)==SPHeight) height=wid_pixmap->height;
		  else height=button->pixgrayed_height;

		  return BN_SetPixmapGrayed(tk_display,widgetid,pixmap,pixmap_mask,gravity,x,y,width,height,depth,delay);

		}
		else return -1;
		break;



	default : return -1;
 		  break;

   }

 }
 else return -1;

}





int wid_SetAttributes(tk_display,widgetid,wid_attributes,delay)
TkDisplay *tk_display;
WidgetID widgetid;
WidgetAttributes *wid_attributes;
Bool delay;
{
 ButtonID buttonid;
 ScrollbarID scrollid;
 EditID editid;
 ListID listid;
 ComboID comboid;
 MenuID  menuid;

 int ret;

 int lighting;
 unsigned int range;
 unsigned int pagerange;
 unsigned int thumbsize;
 Bool neverFocus;
 Bool multipleSelection;
 unsigned int position;
 unsigned int crosstype;
 Colormap colormap;
 Cursor cursor;


 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   if((wid_attributes->mask&SAColormap)==SAColormap)
     wid_SetColormap(tk_display,widgetid,wid_attributes->colormap);
   if((wid_attributes->mask&SACursor)==SACursor)
     wid_SetCursor(tk_display,widgetid,wid_attributes->cursor);

   switch(tk_display->widgets[widgetid].class)
   {


	case WI_BUTTON :

	   buttonid=widgetid;
	   if(tk_display->widgets[buttonid].class==WI_BUTTON)
	   {

	     if((wid_attributes->mask&SALighting)==SALighting)
	     { 
		lighting=wid_attributes->lighting;
		BN_SetLighting(tk_display,buttonid,lighting,True);
	     }
	     if((wid_attributes->mask&SANeverFocus)==SANeverFocus) 
	     {
		neverFocus=wid_attributes->neverFocus;
		BN_SetNeverfocusFlag(tk_display,buttonid,neverFocus);
	     }
	     if((wid_attributes->mask&SACrossType)==SACrossType)
	     {
 		crosstype=wid_attributes->crosstype;
		return BN_SetCrossType(tk_display,buttonid,crosstype,delay);
	     }
	     return 0;
	   }
	   break;



	case WI_SCROLLBAR:

	   scrollid=widgetid;
	   if(tk_display->widgets[scrollid].class==WI_SCROLLBAR)
	   {

 	     if((wid_attributes->mask&SARange)==SARange)
	     { 
		range=wid_attributes->range;
		SB_SetRange(tk_display,scrollid,range);
	     }
  	     if((wid_attributes->mask&SAPagerange)==SAPagerange)
	     { 
		pagerange=wid_attributes->pagerange;
 		SB_SetPagerange(tk_display,scrollid,pagerange);
	     }
	     if((wid_attributes->mask&SAThumbsize)==SAThumbsize)
	     { 
		thumbsize=wid_attributes->thumbsize;
 		SB_SetThumbsize(tk_display,scrollid,thumbsize);
	     }
	     if((wid_attributes->mask&SAPosition)==SAPosition)
	     { 
		position=wid_attributes->position;
	     	return SB_SetPosition(tk_display,scrollid,position);
	     }
	     return 0;
	   }
	   break;



	case WI_EDIT :

	   editid=widgetid;
	   if(tk_display->widgets[editid].class==WI_EDIT)
	     return 0;
	   else return -1;
	   break;



	case WI_LIST :

	   listid=widgetid;
	   if(tk_display->widgets[listid].class==WI_LIST)
	   {
	     ret=0;
 	     if((wid_attributes->mask&SAPosition)==SAPosition)
	     { 
 		position=wid_attributes->position;
 		ret=LS_SetActiveItem(tk_display,listid,position);
	     }
	     if((wid_attributes->mask&SAMultipleSelection)==SAMultipleSelection)
	     {
 		multipleSelection=wid_attributes->multipleSelection;
 		if(multipleSelection==True) return LS_AllowMultipleSelection(tk_display,listid);
		else return LS_ForbidMultipleSelection(tk_display,listid);
	     }
	     return ret;
	   }
	   else return -1;
	   break;



	case WI_COMBO :

	   comboid=widgetid;
	   if(tk_display->widgets[comboid].class==WI_COMBO)
	   {
	     ret=0;
	     if((wid_attributes->mask&SAMultipleSelection)==SAMultipleSelection)
	     {
		multipleSelection=wid_attributes->multipleSelection;
		if(multipleSelection==True) ret=CB_AllowMultipleSelection(tk_display,comboid);
	   	else ret=CB_ForbidMultipleSelection(tk_display,comboid);
	     }
	     if((wid_attributes->mask&SAPosition)==SAPosition)
	     {
 		position=wid_attributes->position;
		/*fprintf(stderr,"Wid set atteint  position=%d\n",position);*/
 		return CB_SetActiveItem(tk_display,comboid,position);
	     }
	     return ret;
	   }
	   return -1;
	   break;


	case WI_MENU :

	   menuid=widgetid;
	   if(tk_display->widgets[menuid].class=WI_MENU)
	     return 0;
	   else return -1;
	   break;


	default: return -1;
		 break;

   }
   return 0;
 }
 /*fprintf(stderr,"Fin … -1\n");*/
 return -1;

}







int wid_GiveFocus(tk_display,widgetid)
TkDisplay *tk_display;
WidgetID widgetid;
{
 int ret;


 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {

   switch(tk_display->widgets[widgetid].class)
   {

	case WI_BUTTON :

	   return BN_GiveFocus(tk_display,widgetid);
	   break;

	case WI_SCROLLBAR:

	   return -1;
	   break;

	case WI_EDIT :

	   return ED_GiveFocus(tk_display,widgetid);
	   break;

	case WI_LIST :

	   return LS_GiveFocus(tk_display,widgetid);
	   break;

	case WI_COMBO :

	   return CB_GiveFocus(tk_display,widgetid);
	   break;

	case WI_MENU :

	   return 0;
	   break;

	default: return -1;
		 break;

   }
   return 0;
 }
 return -1;

}






int wid_Configure(tk_display,widgetid,x,y,width,height,mask)
TkDisplay *tk_display;
WidgetID widgetid;
int x,y;
unsigned int width,height;
unsigned long mask;
{
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 EditStruct *edit;
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 int ret;




 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_BUTTON :
		
		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_BUTTON&&tk_display->widgets[widgetid].button!=NULL)
		{
		  button=tk_display->widgets[widgetid].button;
		  
		  if((mask&CFX)!=CFX) x=button->x;
		  if((mask&CFY)!=CFY) y=button->y;
		  if((mask&CFWidth)!=CFWidth) width=button->width;
		  if((mask&CFHeight)!=CFHeight) height=button->height;	

		  if(((mask&CFX)==CFX||(mask&CFY)==CFY)&&((mask&CFWidth)==CFWidth||(mask&CFHeight)==CFHeight))
		  return BN_Configure(tk_display,widgetid,x,y,width,height);

		  else if((mask&CFX)==CFX||(mask&CFY)==CFY)
		  return BN_Move(tk_display,widgetid,x,y);

		  else if((mask&CFWidth)==CFWidth||(mask&CFHeight)==CFHeight)
		  return BN_Resize(tk_display,widgetid,width,height);

		}
		else return -1;
		break;



	case WI_SCROLLBAR :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_SCROLLBAR&&tk_display->widgets[widgetid].scroll!=NULL)
		{
		  scroll=tk_display->widgets[widgetid].scroll;

		  if((mask&CFX)!=CFX) x=scroll->x;
		  if((mask&CFY)!=CFY) y=scroll->y;
		  if((mask&CFWidth)!=CFWidth) width=scroll->width;
		  if((mask&CFHeight)!=CFHeight) height=scroll->height;	

		  return SB_Configure(tk_display,widgetid,x,y,width,height);

		}
		else return -1;
		break;



	case WI_EDIT :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_EDIT&&tk_display->widgets[widgetid].edit!=NULL)
		{
		  edit=tk_display->widgets[widgetid].edit;
		  
		  if((mask&CFX)!=CFX) x=edit->x;
		  if((mask&CFY)!=CFY) y=edit->y;
		  if((mask&CFWidth)!=CFWidth) width=edit->width;
		  if((mask&CFHeight)!=CFHeight) height=edit->height;	

		  if(((mask&CFX)==CFX||(mask&CFY)==CFY)&&((mask&CFWidth)==CFWidth||(mask&CFHeight)==CFHeight))
		  return ED_Configure(tk_display,widgetid,x,y,width,height);

		  else if((mask&CFX)==CFX||(mask&CFY)==CFY)
		  return ED_Move(tk_display,widgetid,x,y);

		  else if((mask&CFWidth)==CFWidth||(mask&CFHeight)==CFHeight)
		  return ED_Resize(tk_display,widgetid,width,height);
		  
		}
		else return -1;
		break;



	case WI_LIST :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_LIST&&tk_display->widgets[widgetid].list!=NULL)
		{  
		  list=tk_display->widgets[widgetid].list;
		
		  if((mask&CFX)!=CFX) x=list->x;
		  if((mask&CFY)!=CFY) y=list->y;
		  if((mask&CFWidth)!=CFWidth) width=list->width;
		  if((mask&CFHeight)!=CFHeight) height=list->height;	

		  return LS_Configure(tk_display,widgetid,x,y,width,height);

		}
		else return -1;
		break;



	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		{
		  combo=tk_display->widgets[widgetid].combo;

		  
		  if((mask&CFX)!=CFX) x=combo->x;
		  if((mask&CFY)!=CFY) y=combo->y;
		  if((mask&CFWidth)!=CFWidth) width=combo->width;
		  if((mask&CFHeight)!=CFHeight) height=combo->height;	

		  if(((mask&CFX)==CFX||(mask&CFY)==CFY)&&((mask&CFWidth)==CFWidth||(mask&CFHeight)==CFHeight))
		  return CB_Configure(tk_display,widgetid,x,y,width,height);

		  else if((mask&CFX)==CFX||(mask&CFY)==CFY)
		  return CB_Move(tk_display,widgetid,x,y);

		  else if((mask&CFWidth)==CFWidth||(mask&CFHeight)==CFHeight)
		  return CB_Resize(tk_display,widgetid,width,height);

		}
		else return -1;
		break;



	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		{
		  menu=tk_display->widgets[widgetid].menu;
		  if(menu->parency_class<=0 && menu->type!=MN_MENUBAR&&((mask&CFX)==CFX||(mask&CFY)==CFY)) 
		  {
			if((mask&CFX)!=CFX) x=menu->x;
			if((mask&CFY)!=CFY) y=menu->y;
			return MN_Move(tk_display,widgetid,x,y);
		  }
		  else return 0;
		}
		else return -1;
		break;


	default : return -1;
 		  break;

   }

 }
 else return -1;

}





int wid_SetPosition(tk_display,widgetid,position)
TkDisplay *tk_display;
WidgetID widgetid;
int position;
{
 ButtonID buttonid;
 ScrollbarID scrollid;
 EditID editid;
 ListID listid;
 ComboID comboid;
 MenuID  menuid;
 int ret;


 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_BUTTON :

	   return 0;
	   break;

	case WI_SCROLLBAR:

	   scrollid=widgetid;
	   if(tk_display->widgets[scrollid].class==WI_SCROLLBAR)
     	     return SB_SetPosition(tk_display,scrollid,position);
	   else return -1;  
	   break;

	case WI_EDIT :

	   editid=widgetid;
	   if(tk_display->widgets[editid].class==WI_EDIT)
	     return 0;
	   else return -1;
	   break;

	case WI_LIST :

	   listid=widgetid;
	   if(tk_display->widgets[listid].class==WI_LIST)
	     return LS_SetActiveItem(tk_display,listid,position);
	   else return -1;
	   break;

	case WI_COMBO :

	   comboid=widgetid;
	   if(tk_display->widgets[comboid].class==WI_COMBO)
	     return CB_SetActiveItem(tk_display,comboid,position);
  	   return -1;
	   break;

	case WI_MENU :

	   menuid=widgetid;
	   if(tk_display->widgets[menuid].class=WI_MENU)
	     return 0;
	   else return -1;
	   break;

	default: return -1;
		 break;

   }
   return 0;
 }
 return -1;

}




int wid_SetPrivateColors(tk_display,widgetid,colors,delay)
TkDisplay *tk_display;
WidgetID widgetid;
WidgetColors *colors;
Bool delay;
{
 int ret;
 WidgetStruct *widptr;
 WidgetStruct *comboptr;
 WidgetStruct *listptr;
 WidgetStruct *editptr;
 WidgetStruct *scrollptr;
 WidgetStruct *buttonptr;

 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   widptr=&tk_display->widgets[widgetid];
   switch(tk_display->widgets[widgetid].class)
   {
 
	case WI_BUTTON :

	   if(colors==(WidgetColors *)NULL)
	   {
		if(widptr->identity.usePrivateColors==True && widptr->colors!=(WidgetColors *)NULL)
		  free((char *)widptr->colors);
		widptr->colors=(WidgetColors *)&tk_display->bn_colors;
		widptr->identity.usePrivateColors=False;
	   }
	   else
	   {
		if(widptr->identity.usePrivateColors==False)
		  widptr->colors=(WidgetColors *)malloc(sizeof(WidgetColors));
		if(widptr->colors==(WidgetColors *)NULL)
		{
		  fprintf(stderr,"Malloc error. Exit. ");
		  exit(-1);
		}
		widptr->colors=memcpy(widptr->colors,colors,sizeof(WidgetColors));
		widptr->identity.usePrivateColors=True;
	   }	

	   if(widptr->button->parency_class==WI_COMBO)
	   {
	     comboptr=&tk_display->widgets[widptr->button->parency_number];
	     editptr=&tk_display->widgets[comboptr->combo->edit];
	     if(comboptr->combo->hasFocus==True)
	       XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].button->window,comboptr->colors->focus);
	     else 
	       XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].button->window,comboptr->colors->nofocus);
	   }
	   else if(widptr->button->parency_class==WI_SCROLLBAR)
	   {
	     scrollptr=&tk_display->widgets[widptr->button->parency_number];
	     
	     if(scrollptr->scroll->parency_class==WI_LIST)
	     {
	  	listptr=&tk_display->widgets[scrollptr->scroll->parency_number];
		if(listptr->list->parency_class==WI_COMBO)
	     	{
		  comboptr=&tk_display->widgets[listptr->list->parency_number];
	     	  if(comboptr->combo->hasFocus==True)
	     	    XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].button->window,comboptr->colors->focus);
	     	  else 
	       	    XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].button->window,comboptr->colors->nofocus); 
		}
		else
		{
		  if(listptr->list->hasFocus==True)
	       	    XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].button->window,listptr->colors->focus);
		  else XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].button->window,listptr->colors->nofocus);
		}
     	     } 
	     else
	       XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].button->window,scrollptr->colors->nofocus);
	   } 
	   else
	   {	   
	     if(widptr->button->hasFocus==True)
	       XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].button->window,widptr->colors->focus);
	     else 
	       XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].button->window,widptr->colors->nofocus);
	   } 
	   XSetWindowBackground(tk_display->display,tk_display->widgets[widgetid].button->window,widptr->colors->bg);
	   if(delay==False)
	     wid_Refresh(tk_display,widgetid);
	   return 0;
	   break;


	case WI_SCROLLBAR:

	   if(colors==(WidgetColors *)NULL)
	   {
		if(widptr->identity.usePrivateColors==True && widptr->colors!=(WidgetColors *)NULL)
		  free((char *)widptr->colors);
		widptr->colors=(WidgetColors *)&tk_display->sb_colors;
		widptr->identity.usePrivateColors=False;
	   }
	   else
	   {
		if(widptr->identity.usePrivateColors==False)
		  widptr->colors=(WidgetColors *)malloc(sizeof(WidgetColors));
		if(widptr->colors==(WidgetColors *)NULL)
		{
		  fprintf(stderr,"Malloc error. Exit. ");
		  exit(-1);
		}
		widptr->colors=memcpy(widptr->colors,colors,sizeof(WidgetColors));
		widptr->identity.usePrivateColors=True;
	   }	
	   
	   if(widptr->scroll->parency_class==WI_LIST)
	   {
		listptr=&tk_display->widgets[widptr->scroll->parency_number];
		if(listptr->list->parency_class==WI_COMBO)
	     	{
		  comboptr=&tk_display->widgets[listptr->list->parency_number];
	     	  if(comboptr->combo->hasFocus==True)
		  {
	     	    XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].button->window,comboptr->colors->focus);
	   	    buttonptr=&tk_display->widgets[widptr->scroll->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);
	   	    buttonptr=&tk_display->widgets[widptr->scroll->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);
	   	    buttonptr=&tk_display->widgets[widptr->scroll->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);
		  }
	     	  else
		  { 
	       	    XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].button->window,comboptr->colors->nofocus); 
	   	    buttonptr=&tk_display->widgets[widptr->scroll->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
	   	    buttonptr=&tk_display->widgets[widptr->scroll->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
	   	    buttonptr=&tk_display->widgets[widptr->scroll->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
		  }
		}
		else
		{
		  if(listptr->list->hasFocus==True)
		  {
	       	    XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].list->mainwindow,listptr->colors->focus);
	   	    buttonptr=&tk_display->widgets[widptr->scroll->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,listptr->colors->focus);
	   	    buttonptr=&tk_display->widgets[widptr->scroll->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,listptr->colors->focus);
	   	    buttonptr=&tk_display->widgets[widptr->scroll->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,listptr->colors->focus);
		  }
		  else 
		  {
		    XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].list->mainwindow,listptr->colors->nofocus);
	   	    buttonptr=&tk_display->widgets[widptr->scroll->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,listptr->colors->nofocus);
	   	    buttonptr=&tk_display->widgets[widptr->scroll->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,listptr->colors->nofocus);
	   	    buttonptr=&tk_display->widgets[widptr->scroll->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,listptr->colors->nofocus);
		  }
		}
	   }
	   else
	   {
	   	XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].scroll->mainwindow,widptr->colors->nofocus);

	   	buttonptr=&tk_display->widgets[widptr->scroll->B1];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
	   	buttonptr=&tk_display->widgets[widptr->scroll->B2];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
	   	buttonptr=&tk_display->widgets[widptr->scroll->bn_thumb];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
	   }

	   XSetWindowBackground(tk_display->display,tk_display->widgets[widgetid].scroll->mainwindow,widptr->colors->bg);
	   XSetWindowBackground(tk_display->display,tk_display->widgets[widgetid].scroll->thumbwindow,widptr->colors->bg);
	   if(delay==False)
	     wid_Refresh(tk_display,widgetid);

	   return 0;
	   break;


	case WI_EDIT :

	   if(colors==(WidgetColors *)NULL)
	   {
		if(widptr->identity.usePrivateColors==True && widptr->colors!=(WidgetColors *)NULL)
		  free((char *)widptr->colors);
		widptr->colors=(WidgetColors *)&tk_display->ed_colors;
		widptr->identity.usePrivateColors=False;
	   }
	   else
	   {
		if(widptr->identity.usePrivateColors==False)
		  widptr->colors=(WidgetColors *)malloc(sizeof(WidgetColors));
		if(widptr->colors==(WidgetColors *)NULL)
		{
		  fprintf(stderr,"Malloc error. Exit. ");
		  exit(-1);
		}
		widptr->colors=memcpy(widptr->colors,colors,sizeof(WidgetColors));
		widptr->identity.usePrivateColors=True;
	   }	
	   
	   if(widptr->edit->parency_class==WI_COMBO)
	   {
		comboptr=&tk_display->widgets[widptr->edit->parency_number];
		if(comboptr->combo->hasFocus==True)
	   	  XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].edit->window,comboptr->colors->focus);
		else	   
	   	  XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].edit->window,comboptr->colors->nofocus);
	   	XSetWindowBackground(tk_display->display,tk_display->widgets[widgetid].edit->window,widptr->colors->bg);
	   }
	   else
	   {	
		if(widptr->edit->hasFocus==True)
	   	  XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].edit->window,widptr->colors->focus);
		else	   
	   	  XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].edit->window,widptr->colors->nofocus);
	   	XSetWindowBackground(tk_display->display,tk_display->widgets[widgetid].edit->window,widptr->colors->bg);
	   	/*XClearWindow(tk_display->display,tk_display->widgets[widgetid].button->window);*/
	   }
	   if(delay==False)
	     wid_Refresh(tk_display,widgetid);

	   return 0;
	   break;


	case WI_LIST :

	   if(colors==(WidgetColors *)NULL)
	   {
		if(widptr->identity.usePrivateColors==True && widptr->colors!=(WidgetColors *)NULL)
		  free((char *)widptr->colors);
		widptr->colors=(WidgetColors *)&tk_display->ls_colors;
		widptr->identity.usePrivateColors=False;
	   }
	   else
	   {	/*fprintf(stderr,"private  ");*/
		if(widptr->identity.usePrivateColors==False)
		  widptr->colors=(WidgetColors *)malloc(sizeof(WidgetColors));
		if(widptr->colors==(WidgetColors *)NULL)
		{
		  fprintf(stderr,"Malloc error. Exit. ");
		  exit(-1);
		}
		widptr->colors=memcpy(widptr->colors,colors,sizeof(WidgetColors));
		widptr->identity.usePrivateColors=True;
	   }
	
	   if(widptr->list->parency_class==WI_COMBO)
	   {
		comboptr=&tk_display->widgets[widptr->list->parency_number];
		if(comboptr->combo->hasFocus==True)
		{
	   	  XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].list->mainwindow,comboptr->colors->focus);
	   	  XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].list->listwindow,comboptr->colors->focus);
		  scrollptr=&tk_display->widgets[widptr->list->SBV];
	   	  XSetWindowBorder(tk_display->display,scrollptr->scroll->mainwindow,comboptr->colors->focus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->B1];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->B2];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->bn_thumb];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);

		  scrollptr=&tk_display->widgets[widptr->list->SBH];
	   	  XSetWindowBorder(tk_display->display,scrollptr->scroll->mainwindow,comboptr->colors->focus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->B1];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->B2];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->bn_thumb];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);

		}
		else	 
		{ 
		  XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].list->mainwindow,comboptr->colors->nofocus); 
	   	  XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].list->listwindow,comboptr->colors->nofocus);
		  scrollptr=&tk_display->widgets[widptr->list->SBV];
	   	  XSetWindowBorder(tk_display->display,scrollptr->scroll->mainwindow,comboptr->colors->nofocus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->B1];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->B2];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->bn_thumb];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);

		  scrollptr=&tk_display->widgets[widptr->list->SBH];
	   	  XSetWindowBorder(tk_display->display,scrollptr->scroll->mainwindow,comboptr->colors->nofocus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->B1];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->B2];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
		  buttonptr=&tk_display->widgets[scrollptr->scroll->bn_thumb];
		  XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
		}
	   }
	   else
	   {
	     /*fprintf(stderr,"list cas 2   ");*/
	     if(widptr->list->hasFocus==True)
	     {
		/*fprintf(stderr,"... bis   ");*/
	     	XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].list->mainwindow,widptr->colors->focus);
	   	XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].list->listwindow,widptr->colors->focus);
      		scrollptr=&tk_display->widgets[widptr->list->SBV];
	   	XSetWindowBorder(tk_display->display,scrollptr->scroll->mainwindow,widptr->colors->focus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->B1];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->B2];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->bn_thumb];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);

		scrollptr=&tk_display->widgets[widptr->list->SBH];
	   	XSetWindowBorder(tk_display->display,scrollptr->scroll->mainwindow,widptr->colors->focus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->B1];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->B2];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->bn_thumb];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);
             }
	     else
	     {
	     	XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].list->mainwindow,widptr->colors->nofocus);
	   	XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].list->listwindow,widptr->colors->nofocus);
      		scrollptr=&tk_display->widgets[widptr->list->SBV];
	   	XSetWindowBorder(tk_display->display,scrollptr->scroll->mainwindow,widptr->colors->nofocus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->B1];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->B2];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->bn_thumb];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);

		scrollptr=&tk_display->widgets[widptr->list->SBH];
	   	XSetWindowBorder(tk_display->display,scrollptr->scroll->mainwindow,widptr->colors->nofocus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->B1];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->B2];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
		buttonptr=&tk_display->widgets[scrollptr->scroll->bn_thumb];
		XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
            
	     }
	   }	
	   XSetWindowBackground(tk_display->display,tk_display->widgets[widgetid].list->listwindow,widptr->colors->bg);
	   /*XClearWindow(tk_display->display,tk_display->widgets[widgetid].button->window);*/

	   if(delay==False)
	     wid_Refresh(tk_display,widgetid);
	   break;


	case WI_COMBO :

	   if(colors==(WidgetColors *)NULL)
	   {
		if(widptr->identity.usePrivateColors==True && widptr->colors!=(WidgetColors *)NULL)
		  free((char *)widptr->colors);
		widptr->colors=(WidgetColors *)&tk_display->cb_colors;
		widptr->identity.usePrivateColors=False;
	   }
	   else
	   {	/*fprintf(stderr,"private  ");*/
		if(widptr->identity.usePrivateColors==False)
		  widptr->colors=(WidgetColors *)malloc(sizeof(WidgetColors));
		if(widptr->colors==(WidgetColors *)NULL)
		{
		  fprintf(stderr,"Malloc error. Exit. ");
		  exit(-1);
		}
		widptr->colors=memcpy(widptr->colors,colors,sizeof(WidgetColors));
		widptr->identity.usePrivateColors=True;
	   }
	   if(widptr->menu->hasFocus==True)
	   {
	     XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].combo->window,widptr->colors->focus);
	     _LS_DrawFocus(tk_display,widptr->combo->list,ON);
	   }
	   else 
	   {
	     XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].combo->window,widptr->colors->nofocus);
	     _LS_DrawFocus(tk_display,widptr->combo->list,OFF);
	   }
	   if(delay==False)
	     wid_Refresh(tk_display,widgetid);
	   return 0;

	   break;


	case WI_MENU :

	   if(colors==(WidgetColors *)NULL)
	   {
		if(widptr->identity.usePrivateColors==True && widptr->colors!=(WidgetColors *)NULL)
		  free((char *)widptr->colors);
		widptr->colors=(WidgetColors *)&tk_display->mn_colors;
		widptr->identity.usePrivateColors=False;
	   }
	   else
	   {	/*fprintf(stderr,"private  ");*/
		if(widptr->identity.usePrivateColors==False)
		  widptr->colors=(WidgetColors *)malloc(sizeof(WidgetColors));
		if(widptr->colors==(WidgetColors *)NULL)
		{
		  fprintf(stderr,"Malloc error. Exit. ");
		  exit(-1);
		}
		widptr->colors=memcpy(widptr->colors,colors,sizeof(WidgetColors));
		widptr->identity.usePrivateColors=True;
	   }
	   if(widptr->menu->hasFocus==True)
	     XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].menu->window,widptr->colors->focus);
	   else XSetWindowBorder(tk_display->display,tk_display->widgets[widgetid].menu->window,widptr->colors->nofocus);
	   if(delay==False)
	     wid_Refresh(tk_display,widgetid);
	   return 0;
	   break;

	default: return -1;
		 break;

   }
   return 0;
 }
 return -1;

}




int wid_SetColormap(tk_display,widgetid,colormap)
TkDisplay *tk_display;
WidgetID widgetid;
Colormap colormap;
{
 WidgetStruct *widptr;
 
 int ret;


 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   widptr=&tk_display->widgets[widgetid];
   if(colormap==0) 
     colormap=DefaultColormap(tk_display->display,tk_display->screen);

   switch(tk_display->widgets[widgetid].class)
   {

	case WI_BUTTON :

	   XSetWindowColormap(tk_display->display,tk_display->widgets[widgetid].button->window,colormap);
	   widptr->identity.colormap=colormap;
	   return 0;
	   break;

	case WI_SCROLLBAR:

	   XSetWindowColormap(tk_display->display,tk_display->widgets[widgetid].scroll->mainwindow,colormap);
	   XSetWindowColormap(tk_display->display,tk_display->widgets[widgetid].scroll->thumbwindow,colormap);
	   wid_SetColormap(tk_display,widptr->scroll->B1,colormap);
	   wid_SetColormap(tk_display,widptr->scroll->B1,colormap);
	   wid_SetColormap(tk_display,widptr->scroll->bn_thumb,colormap);
	   widptr->identity.colormap=colormap;
	   return 0;  
	   break;

	case WI_EDIT :

	   XSetWindowColormap(tk_display->display,tk_display->widgets[widgetid].edit->window,colormap);
	   widptr->identity.colormap=colormap;
	   return 0;
	   break;

	case WI_LIST :

	   XSetWindowColormap(tk_display->display,tk_display->widgets[widgetid].list->mainwindow,colormap);
	   XSetWindowColormap(tk_display->display,tk_display->widgets[widgetid].list->listwindow,colormap);
	   widptr->identity.colormap=colormap;
	   wid_SetColormap(tk_display,widptr->list->SBH,colormap);
	   wid_SetColormap(tk_display,widptr->list->SBV,colormap);
	   return 0;
	   break;

	case WI_COMBO :

	   XSetWindowColormap(tk_display->display,tk_display->widgets[widgetid].combo->window,colormap);
	   widptr->identity.colormap=colormap;
	   wid_SetColormap(tk_display,widptr->combo->list,colormap);
	   wid_SetColormap(tk_display,widptr->combo->edit,colormap);
	   wid_SetColormap(tk_display,widptr->combo->button,colormap);
  	   return 0;
	   break;

	case WI_MENU :

	   XSetWindowColormap(tk_display->display,tk_display->widgets[widgetid].menu->window,colormap);
	   widptr->identity.colormap=colormap;
  	   return 0;
	   break;

	default: return -1;
		 break;

   }
   
 }
 return -1;

}



int wid_SetCursor(tk_display,widgetid,cursor)
TkDisplay *tk_display;
WidgetID widgetid;
Cursor cursor;
{
 WidgetStruct *widptr;
 int ret;


 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   widptr=&tk_display->widgets[widgetid];

   switch(tk_display->widgets[widgetid].class)
   {

	case WI_BUTTON :

	   if(cursor==0) 
 	     cursor=tk_display->cursors.normal;

	   XDefineCursor(tk_display->display,tk_display->widgets[widgetid].button->window,cursor);
	   widptr->identity.cursor=cursor;
	   return 0;
	   break;

	case WI_SCROLLBAR:

	   if(cursor==0 && (widptr->type==SB_TOPALIGN || widptr->type==SB_BOTTOMALIGN || widptr->type==SB_VTHUMB)) 
 	     cursor=tk_display->cursors.sb_up;
	   else if(cursor==0 && (widptr->type==SB_LEFTALIGN ||widptr->type==SB_RIGHTALIGN ||widptr->type==SB_HTHUMB)) 
 	     cursor=tk_display->cursors.sb_left;

	   XDefineCursor(tk_display->display,widptr->scroll->mainwindow,cursor);
	   XDefineCursor(tk_display->display,widptr->scroll->thumbwindow,cursor);
	   widptr->identity.cursor=cursor;
	   return 0;  
	   break;

	case WI_EDIT :

	   if(cursor==0) 
 	     cursor=tk_display->cursors.textedit;

	   XDefineCursor(tk_display->display,widptr->edit->window,cursor);
	   widptr->identity.cursor=cursor;
	   return 0;
	   break;

	case WI_LIST :

	   if(cursor==0) 
 	     cursor=tk_display->cursors.normal;

	   XDefineCursor(tk_display->display,widptr->list->mainwindow,cursor);
	   XDefineCursor(tk_display->display,widptr->list->listwindow,cursor);
	   widptr->identity.cursor=cursor;
	   return 0;
	   break;

	case WI_COMBO :

	   if(cursor==0) 
 	     cursor=tk_display->cursors.normal;

	   XDefineCursor(tk_display->display,widptr->combo->window,cursor);
	   widptr->identity.cursor=cursor;
  	   return 0;
	   break;

	case WI_MENU :

	   if(cursor==0) 
 	     cursor=tk_display->cursors.normal;

	   XDefineCursor(tk_display->display,widptr->menu->window,cursor);
	   widptr->identity.cursor=cursor;
  	   return 0;
	   break;

	default: return -1;
		 break;

   }
   
 }
 return -1;

}




int _WID_Tempo(tk_display)
TkDisplay *tk_display;
{
  unsigned long i;
  unsigned long tempo;

  tempo=tk_display->widget_double_click/10;
  i=0;

  while(i<tempo)
    i++;
  return 0;
}




