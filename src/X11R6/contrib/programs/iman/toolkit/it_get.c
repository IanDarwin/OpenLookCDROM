/*
 *
 * 	it_get.c  
 * 	informations sur les elements des widgets
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









int item_GetState(tk_display,widgetid,number,state,selected)
TkDisplay *tk_display;
WidgetID widgetid;
int number;
unsigned int *state;
int *selected;
{
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 

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
		  
 		  *state=list->items[number].state;
 		  *selected=list->items[number].selected;
		  return 0;
		}
		else return -1;	
		break;


	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		  combo=tk_display->widgets[widgetid].combo;
		else return -1;
		return item_GetState(tk_display,combo->list,number,state,selected);
		break;


	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		  menu=tk_display->widgets[widgetid].menu;
		else return -1;

		if(menu->numitems>0 && number>=-1 && number<menu->numitems)
		{
		  if(number==END) number=menu->numitems-1;

		  *state=menu->items[number].state;
 		  *selected=menu->items[number].selected;
		  return 0;
		}

		else return -1;			
		break;

	default : return -1;
 		  break;

   }

 }
 else return -1;
}






int item_GetText(tk_display,widgetid,number,text)
TkDisplay *tk_display;
WidgetID widgetid;
int number;
unsigned char **text;
{
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;

 

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

		  if(strlen(list->items[number].text)>0)
		  {
 		    *text=(unsigned char *)malloc(strlen(list->items[number].text)+1);
		    *text=(unsigned char *)memset(*text,0,strlen(list->items[number].text)+1);
		    memcpy(*text,list->items[number].text,strlen(list->items[number].text));
		    return 0;
 		  }
		  else *text=NULL;  
		  return 0;
		}
		else return -1;	
		break;


	case WI_COMBO :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_COMBO&&tk_display->widgets[widgetid].combo!=NULL)
		  combo=tk_display->widgets[widgetid].combo;
		else return -1;
		return item_GetText(tk_display,combo->list,number,text);
		break;


	case WI_MENU :

		if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].class==WI_MENU&&tk_display->widgets[widgetid].menu!=NULL)
		  menu=tk_display->widgets[widgetid].menu;
		else return -1;

		if(menu->numitems>0 && number>=-1 && number<menu->numitems)
		{
		  if(number==END) number=menu->numitems-1;

		  if(strlen(menu->items[number].text)>0)
		  {
 		    *text=(unsigned char *)malloc(strlen(menu->items[number].text)+1);
		    *text=(unsigned char *)memset(*text,0,strlen(menu->items[number].text)+1);
		    memcpy(*text,menu->items[number].text,strlen(menu->items[number].text));
		    return 0;
 		  }
		  else *text=NULL;  
		  return 0;
		}

		else return -1;			
		break;

	default : return -1;
 		  break;

   }

 }
 else return -1;
}







