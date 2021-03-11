/*
 *
 * 	cb_get.c  
 * 	informations sur les boites combo
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

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"

#include <X11/iman/widgets.h>










int CB_GetItemSelected(tk_display,comboid)
TkDisplay *tk_display;
ComboID comboid;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;


 if(LS_IsAnySelected(tk_display,combo->list)==True) 
  return LS_GetItemSelected(tk_display,combo->list); 
 else return -1;
}





Bool CB_IsAnySelected(tk_display,comboid)
TkDisplay *tk_display;
ComboID comboid;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return (Bool)LS_IsAnySelected(tk_display,combo->list);
}




/*
 *
 * Repond a la question: Est-ce que cet element 
 * est selectionne ?
 *
 *
 */

Bool CB_IsItemSelected(tk_display,comboid,number)
TkDisplay *tk_display;
ComboID comboid;
int number;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_IsItemSelected(tk_display,combo->list,number);
}





int CB_GetNumitems(tk_display,comboid)
TkDisplay *tk_display;
ComboID comboid;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_GetNumitems(tk_display,combo->list);
}



/*
 *
 * Obtient l'etat d'un element
 *
 */

int CB_GetItemState(tk_display,comboid,number)
TkDisplay *tk_display;
ComboID comboid;
int number;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_GetItemState(tk_display,combo->list,number);
 
}




/*
 *
 * Obtient un element
 *
 */

int CB_GetItem(tk_display,comboid,number,listitem)
TkDisplay *tk_display;
ComboID comboid;
int number;
ListItem *listitem;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;


 return LS_GetItem(tk_display,combo->list,number,listitem);

}







int CB_GetState(tk_display,comboid)
TkDisplay *tk_display;
ComboID comboid;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return combo->state;
 
}

