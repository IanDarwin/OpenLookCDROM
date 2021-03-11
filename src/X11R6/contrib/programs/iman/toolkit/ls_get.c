/*
 *
 * 	ls_get.c  
 * 	informations sur les listes
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







/*
 *
 * Repond a la question: Est-ce qu'un element 
 * a deja ete selectionne ?
 *
 *
 */

int LS_IsAnySelected(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;

 return list->selected;
}




/*
 *
 * Repond a la question: Est-ce que cet element 
 * est selectionne ?
 *
 *
 */

Bool LS_IsItemSelected(tk_display,listid,number)
TkDisplay *tk_display;
ListID listid;
int number;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;

 if(number>=0 && number<list->numitems)
  return list->items[number].selected;
 else if(number==END && list->numitems>0)
  return list->items[list->numitems-1].selected;
 else return 0;
}




/*
 *
 * Obtient l'element courant selectionne
 *
 *
 */

int LS_GetItemSelected(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -2;

 if(list->selected==True) return list->selecteditem;
 else return -1;
}




/*
 *
 * Obtient l'etat de la liste
 *
 *
 */

int LS_GetState(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;

 return list->state;
}




/*
 *
 * Obtient le nombre d'elements
 *
 *
 */

int LS_GetNumitems(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;

 return list->numitems;
}



/*
 *
 * Obtient l'etat d'un element
 *
 */

int LS_GetItemState(tk_display,listid,number)
TkDisplay *tk_display;
ListID listid;
int number;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;

 if(number>=0 && number<list->numitems)
   return list->items[number].state;
 if(number==END && list->numitems>0)
   return list->items[list->numitems-1].state; 
 else return -1;
}




/*
 *
 * Obtient un element
 *
 */

int LS_GetItem(tk_display,listid,number,listitem)
TkDisplay *tk_display;
ListID listid;
int number;
ListItem *listitem;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;

 if(number>=0 && number<list->numitems)
  listitem=&list->items[number];
 else if(number==END&&list->numitems>0)
  listitem=&list->items[list->numitems-1];
 return 0;
}




