/*
 *
 * 	it_set.c
 * 	modification des elements des widgets
 *
 * 	Modification :  11/11/93
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
 *      IMAN Development Toolkit version 1.0.d
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








int item_Add(tk_display,widgetid,subwidgetid,position,type,text,key,font,state,delay)
TkDisplay *tk_display;
WidgetID widgetid, subwidgetid;
int position, type;
char *text;
unsigned int key, state;
XFontStruct *font;
Bool delay;
{
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 int ret;



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_SCROLLBAR :
	case WI_BUTTON :
	case WI_EDIT   :

		return -1;
		break;



	case WI_LIST :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_LIST&&tk_display->widgets[widgetid].list!=NULL)
		  return LS_AddFillItem(tk_display,widgetid,TextFlag,position,text,font,0,0,0,0,0,TextFlag,state,delay);
		else return -1;
		break;


	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		  return CB_AddFillItem(tk_display,widgetid,TextFlag,position,text,font,0,0,0,0,0,TextFlag,state,delay);
		else return -1;
		break;


	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		  return MN_AddFillItem(tk_display,widgetid,subwidgetid,type,position,text,key,font,state,delay);
		else return -2;
		break;


	default : return -1;
 		  break;

   }

 }
 else return -1;

}







int item_Move(tk_display,widgetid,number,position,delay)
TkDisplay *tk_display;
WidgetID widgetid;
int position, number;
Bool delay;
{
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 int ret;



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_SCROLLBAR :
	case WI_BUTTON :
	case WI_EDIT   :

		return -1;
		break;



	case WI_LIST :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_LIST&&tk_display->widgets[widgetid].list!=NULL)
		{
		  list=tk_display->widgets[widgetid].list;
		  if(number==END)
		    number=list->numitems-1;
		  if(position==END)
		    position=list->numitems-1;
		  return LS_MoveItem(tk_display,widgetid,number,position,delay);
		}
		else return -1;
		break;


	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		{
		  combo=tk_display->widgets[widgetid].combo;
  		  return item_Move(tk_display,combo->list,number,position,delay);
		}  
		else return -1;
		break;


	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		{
		  menu=tk_display->widgets[widgetid].menu;
		  if(number==END)
		    number=menu->numitems-1;
		  if(position==END)
		    position=menu->numitems-1;
		  return MN_MoveItem(tk_display,widgetid,number,position,delay);		  
		}
		else return -1;
		break;


	default : return -1;
 		  break;

   }

 }
 else return -1;

}







int item_Delete(tk_display,widgetid,number,delay)
TkDisplay *tk_display;
WidgetID widgetid;
int number;
Bool delay;
{
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 int ret;



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_SCROLLBAR :
	case WI_BUTTON :
	case WI_EDIT   :

		return -1;
		break;



	case WI_LIST :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_LIST&&tk_display->widgets[widgetid].list!=NULL)
		{
		  list=tk_display->widgets[widgetid].list;
		  if(number==END)
		    number=list->numitems-1;
		  return LS_DeleteItem(tk_display,widgetid,number,delay);
		}
		else return -1;
		break;


	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		{
		  combo=tk_display->widgets[widgetid].combo;
  		  return item_Delete(tk_display,combo->list,number,delay);
		}  
		else return -1;
		break;


	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		{
		  menu=tk_display->widgets[widgetid].menu;
		  if(number==END)
		    number=menu->numitems-1;
		  return MN_DeleteItem(tk_display,widgetid,number,delay);		  
		}
		else return -1;
		break;


	default : return -1;
 		  break;

   }

 }
 else return -1;

}







int item_DeleteAll(tk_display,widgetid)
TkDisplay *tk_display;
WidgetID widgetid;
{
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 int ret;



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_SCROLLBAR :
	case WI_BUTTON :
	case WI_EDIT   :

		return -1;
		break;



	case WI_LIST :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_LIST&&tk_display->widgets[widgetid].list!=NULL)
		{
		  list=tk_display->widgets[widgetid].list;
		  if(list->maxitems>0)
		    return LS_Clear(tk_display,widgetid);
		}
		else return -1;
		break;


	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		{
		  combo=tk_display->widgets[widgetid].combo;
  		  return item_DeleteAll(tk_display,combo->list);
		}  
		else return -1;
		break;


	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		{
		  menu=tk_display->widgets[widgetid].menu;
		  if(menu->maxitems>0)
		    return MN_Clear(tk_display,widgetid);		  
		}
		else return -1;
		break;


	default : return -1;
 		  break;

   }

 }
 else return -1;

}






int item_SetState(tk_display,widgetid,number,state,selected)
TkDisplay *tk_display;
WidgetID widgetid;
int number;
unsigned int state;
int selected;
{
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 int ret;

 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_SCROLLBAR :
	case WI_BUTTON :
	case WI_EDIT   :

		return -1;
		break;



	case WI_LIST :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_LIST&&tk_display->widgets[widgetid].list!=NULL)
		  list=tk_display->widgets[widgetid].list;
		else return -1;

		if(list->numitems>0 && number>=-1 && number<list->numitems)
		{
		  if(number==END) number=list->numitems-1;
		  ret=0;
 		  if((state&Grayed)==Grayed && (list->items[number].state&Grayed)!=Grayed)
 		    ret=LS_GrayItem(tk_display,widgetid,number);
		  else if((state&Grayed)!=Grayed && (list->items[number].state&Grayed)==Grayed)
 		    ret=LS_UngrayItem(tk_display,widgetid,number);
		  if(ret==-1) return -1;

		  if(selected==True && list->items[number].selected==False)
 		    ret=LS_SelectItem(tk_display,widgetid,number);
		  else if(selected==False && list->items[number].selected==True)
 		    ret=LS_UnselectItem(tk_display,widgetid,number);
		  if(ret==-1) return -1;
		  else return 0;
		}
		else return -1;	
		break;


	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		  combo=tk_display->widgets[widgetid].combo;
		else return -1;
		return item_SetState(tk_display,combo->list,number,state,selected);
		break;


	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		  menu=tk_display->widgets[widgetid].menu;
		else return -1;

		if(menu->numitems>0 && number>=-1 && number<menu->numitems)
		{
		  if(number==END) number=menu->numitems-1;
		  ret=0;
 		  if((state&Grayed)==Grayed && (menu->items[number].state&Grayed)!=Grayed)
 		    ret=MN_GrayItem(tk_display,widgetid,number);
		  else if((state&Grayed)!=Grayed && (menu->items[number].state&Grayed)==Grayed)
 		    ret=MN_UngrayItem(tk_display,widgetid,number);
		  if(ret==-1) return -1;

 		  if((state&Checked)==Checked && (menu->items[number].state&Checked)!=Checked)
 		    ret=MN_CheckItem(tk_display,widgetid,number);
		  else if((state&Checked)!=Checked && (menu->items[number].state&Checked)==Checked)
 		    ret=MN_UncheckItem(tk_display,widgetid,number);
		  if(ret==-1) return -1;
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






int item_SetTextDecoration(tk_display,widgetid,number,item_text,delay)
TkDisplay *tk_display;
WidgetID widgetid;
int number;
ItemTextDecoration *item_text;
Bool delay;
{
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 char *text;
 XFontStruct *font;
 unsigned int key;

 

 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_SCROLLBAR :
	case WI_BUTTON :
	case WI_EDIT   :

		return -1;
		break;



	case WI_LIST :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_LIST&&tk_display->widgets[widgetid].list!=NULL)
		  list=tk_display->widgets[widgetid].list;
		else return -1;

		if(list->numitems>0 && number>=-1 && number<list->numitems)
		{
		  if(number==END) number=list->numitems-1;
		  
		  if((item_text->mask&STText)==STText)
		  {
		    text=item_text->text;
		    if(text==NULL)
		    {
			if((list->items[number].flags&TextFlag)==TextFlag)
			  list->items[number].flags=list->items[number].flags-TextFlag;
			list->items[number].precedency=PixmapFlag;
		    }
		  }
	    	  else text=list->items[number].text;
		  if((item_text->mask&STFont)==STFont) font=item_text->font;
		  else font=list->items[number].font;

		  return LS_SetItemText(tk_display,widgetid,number,text,font,delay);
		}
		else return -1;	
		break;


	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		  combo=tk_display->widgets[widgetid].combo;
		else return -1;
		return item_SetTextDecoration(tk_display,combo->list,number,item_text,delay);
		break;


	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		  menu=tk_display->widgets[widgetid].menu;
		else return -1;

		if(menu->numitems>0 && number>=-1 && number<menu->numitems)
		{
		  if(number==END) number=menu->numitems-1;

		  if((item_text->mask&STText)==STText)
		  {
		    text=item_text->text;
		    if(text==NULL)
		    {
			if((menu->items[number].flags&TextFlag)==TextFlag)
			  menu->items[number].flags=menu->items[number].flags-TextFlag;
			menu->items[number].precedency=PixmapFlag;
		    }
		  }
		  else text=menu->items[number].text;
		  if((item_text->mask&STFont)==STFont) font=item_text->font;
		  else font=menu->items[number].font;
		  if((item_text->mask&STKey)==STKey) key=item_text->key;
		  else key=menu->items[number].key;


		  return MN_SetItemText(tk_display,widgetid,number,text,key,font,delay);
		}

		else return -1;			
		break;

	default : return -1;
 		  break;

   }

 }
 else return -1;
}





int item_SetPixmapDecoration(tk_display,widgetid,number,item_pixmap,delay)
TkDisplay *tk_display;
WidgetID widgetid;
int number;
ItemPixmapDecoration *item_pixmap;
Bool delay;
{
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 Pixmap pixmap, pixmap_mask;
 unsigned int depth;
 unsigned int width, height;


 

 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_SCROLLBAR :
	case WI_BUTTON :
	case WI_EDIT   :

		return -1;
		break;



	case WI_LIST :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_LIST&&tk_display->widgets[widgetid].list!=NULL)
		  list=tk_display->widgets[widgetid].list;
		else return -1;

		if(list->numitems>0 && number>=-1 && number<list->numitems)
		{
		  if(number==END) number=list->numitems-1;
		  
		  if((item_pixmap->mask&SPPixmap)==SPPixmap)
		  {
		    pixmap=item_pixmap->pixmap;
		    if(pixmap==0)
		    {
			if((list->items[number].flags&PixmapFlag)==PixmapFlag)
			  list->items[number].flags=list->items[number].flags-PixmapFlag;
			list->items[number].precedency=TextFlag;
		    }
		  }
		  else pixmap=list->items[number].pixmap;
		  if((item_pixmap->mask&SPPixmapMask)==SPPixmapMask) pixmap_mask=item_pixmap->pixmap_mask;
		  else pixmap_mask=list->items[number].pix_mask;
		  if((item_pixmap->mask&SPDepth)==SPDepth) depth=item_pixmap->depth;
		  else depth=list->items[number].pix_depth;
		  if((item_pixmap->mask&SPWidth)==SPWidth) width=item_pixmap->width;
		  else width=list->items[number].pix_width;
		  if((item_pixmap->mask&SPHeight)==SPHeight) height=item_pixmap->height;
		  else height=list->items[number].pix_height;

		  return LS_SetItemPixmap(tk_display,widgetid,number,pixmap,pixmap_mask,width,height,depth,delay);
		}
		else return -1;	
		break;


	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		  combo=tk_display->widgets[widgetid].combo;
		else return -1;
		return item_SetPixmapDecoration(tk_display,combo->list,number,item_pixmap,delay);
		break;


	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		  menu=tk_display->widgets[widgetid].menu;
		else return -1;

		if(menu->numitems>0 && number>=-1 && number<menu->numitems)
		{
		  if(number==END) number=menu->numitems-1;

		  if((item_pixmap->mask&SPPixmap)==SPPixmap)
		  {
		    pixmap=item_pixmap->pixmap;
		    if(pixmap==(Pixmap)0)
		    {
			if((menu->items[number].flags&PixmapFlag)==PixmapFlag)
			  menu->items[number].flags=menu->items[number].flags-PixmapFlag;
			menu->items[number].precedency=TextFlag;
		    }
		  }
		  else pixmap=menu->items[number].pixmap;
		  if((item_pixmap->mask&SPPixmapMask)==SPPixmapMask) pixmap_mask=item_pixmap->pixmap_mask;
		  else pixmap_mask=menu->items[number].pix_mask;
		  if((item_pixmap->mask&SPDepth)==SPDepth) depth=item_pixmap->depth;
		  else depth=menu->items[number].pix_depth;
		  if((item_pixmap->mask&SPWidth)==SPWidth) width=item_pixmap->width;
		  else width=menu->items[number].pix_width;
		  if((item_pixmap->mask&SPDepth)==SPDepth) height=item_pixmap->height;
		  else height=menu->items[number].pix_height;


		  return MN_SetItemPixmap(tk_display,widgetid,number,pixmap,pixmap_mask,width,height,depth,delay);
		}

		else return -1;			
		break;

	default : return -1;
 		  break;

   }

 }
 else return -1;
}






int item_SetPrecedency(tk_display,widgetid,number,precedency,delay)
TkDisplay *tk_display;
WidgetID widgetid;
int number;
unsigned int precedency;
Bool delay;
{
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;



 if(precedency!=TextFlag && precedency!=PixmapFlag)
   return -1;

 

 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {

	case WI_SCROLLBAR :
	case WI_BUTTON :
	case WI_EDIT   :

		return -1;
		break;



	case WI_LIST :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_LIST&&tk_display->widgets[widgetid].list!=NULL)
		  list=tk_display->widgets[widgetid].list;
		else return -1;

		if(list->numitems>0 && number>=-1 && number<list->numitems)
		{
		  if(number==END) number=list->numitems-1;
		  
		  return LS_SetPrecedency(tk_display,widgetid,number,precedency,delay);
		}
		else return -1;	
		break;


	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		  combo=tk_display->widgets[widgetid].combo;
		else return -1;
		return item_SetPrecedency(tk_display,combo->list,number,precedency,delay);
		break;


	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		  menu=tk_display->widgets[widgetid].menu;
		else return -1;

		if(menu->numitems>0 && number>=-1 && number<menu->numitems)
		{
		  if(number==END) number=menu->numitems-1;

		  return MN_SetPrecedency(tk_display,widgetid,number,precedency,delay);
		}

		else return -1;			
		break;

	default : return -1;
 		  break;

   }

 }
 else return -1;
}





