/*
 *
 * 	cb_set.c  
 * 	modification des boites combo
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




/*
 *
 * Donne le focus a une liste
 *
 */

int CB_GiveFocus(tk_display,comboid)
TkDisplay *tk_display;
ComboID comboid;
{
 XWindowAttributes attrib;
 EditStruct *edit;
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 edit=tk_display->widgets[combo->edit].edit;
 attrib.map_state=IsUnviewable;
 XGetWindowAttributes(tk_display->display,edit->window,&attrib);			  

 
 if((combo->state&Grayed)!=Grayed&&attrib.map_state==IsViewable) 
  XSetInputFocus(tk_display->display,edit->window,RevertToPointerRoot,CurrentTime); 
 return 0;
}





/*
 *
 * Rajoute un element a la liste du "combo box"
 * 
 */

int CB_AddItem(tk_display,comboid,position,initialstate,delay)
TkDisplay *tk_display;
ComboID comboid;
int position;
int initialstate;
Bool delay;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_AddItem(tk_display,combo->list,position,initialstate,delay);
}





/*
 *
 * Retire un element a la liste "list"
 *
 */

int CB_DeleteItem(tk_display,comboid,position,delay)
TkDisplay *tk_display;
ComboID comboid;
int position;
Bool delay;
{
 ComboStruct *combo;
 ListStruct *list;

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;
 list=tk_display->widgets[combo->list].list;

 if(position==list->selecteditem&&list->selected==True)
 {
	ED_SetText(tk_display,combo->edit,"",delay);
	return LS_DeleteItem(tk_display,combo->list,position,delay);
 }
 else return LS_DeleteItem(tk_display,combo->list,position,delay);
}




/*
 *
 * Remet a 0 toute la combo, efface tous les elements
 * et libere la memoire allouee
 *
 */

int CB_ClearList(tk_display,comboid)
TkDisplay *tk_display;
ComboID comboid;
{
 int i;
 ComboStruct *combo;
 ListStruct *list;
 EditStruct *edit;

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;
 list=tk_display->widgets[combo->list].list;
 edit=tk_display->widgets[combo->edit].edit;

 LS_Clear(tk_display,combo->list);
 edit->sel1=edit->sel2=edit->position=0;
 for(i=0;i<152;i++)
  edit->text[i]=0;
 ed_DrawEdit(tk_display,combo->edit);
 return 0;
}





/*
 *
 * Deplace un element de la combo
 *
 *
 */

int CB_MoveItem(tk_display,comboid,number,newposition,delay)
TkDisplay *tk_display;
ComboID comboid;
int number, newposition;
Bool delay;
{
 ComboStruct *combo;

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_MoveItem(tk_display,combo->list,number,newposition,delay);
}





/*
 *
 * Ajoute et initialise un element de la combo
 *
 */

int CB_AddFillItem(tk_display,comboid,type,position,text,font,pixmap,pix_mask,pix_width,pix_height,pix_depth,precedency,initialstate,delay)
TkDisplay *tk_display;
ComboID comboid;
int type, position;
char *text;
XFontStruct *font;
Pixmap pixmap, pix_mask;
unsigned int pix_width, pix_height, pix_depth;
int precedency, initialstate;
Bool delay;
{
 ComboStruct *combo;

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_AddFillItem(tk_display,combo->list, type, position, text, font, pixmap, pix_mask,pix_width, pix_height, pix_depth, precedency, initialstate,delay);

}




/*
 *
 * Initialise le texte d'un element
 *
 */

int CB_SetItemText(tk_display,comboid,number,text,font,delay)
TkDisplay *tk_display;
ComboID comboid;
int number;
char *text;
XFontStruct *font;
Bool delay;
{
 ComboStruct *combo;
 ListStruct *list;

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;
 list=tk_display->widgets[combo->list].list;

 LS_SetItemText(tk_display,combo->list, number, text, font, delay);
 if(number==list->selecteditem&&list->selected==True)
   ED_SetText(tk_display,combo->edit,text,delay);
 return 0;
}




/*
 *
 * Initialise le pixmap d'un element
 *
 */

int CB_SetItemPixmap(tk_display,comboid,number,pixmap,pix_mask,pix_width,pix_height, pix_depth, delay)
TkDisplay *tk_display;
ComboID comboid;
int number;
Pixmap pixmap, pix_mask;
unsigned int pix_width, pix_height, pix_depth;
Bool delay;
{
 ComboStruct *combo;

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_SetItemPixmap(tk_display,combo->list, number, pixmap, pix_mask, pix_width, pix_height, pix_depth, delay);

}





/*
 *
 * Initialise le pixmap grayed d'un element
 *
 *
 */

int CB_SetItemPixmapGrayed(tk_display,comboid,number,pix_grayed,pix_maskgrayed,delay)
TkDisplay *tk_display;
ComboID comboid;
int number;
Pixmap pix_grayed, pix_maskgrayed;
Bool delay;
{
 ComboStruct *combo;

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_SetItemPixmapGrayed(tk_display,combo->list, number, pix_grayed, pix_maskgrayed,delay);

}




/*
 *
 * Change la precedence des composants d'un element
 *
 *
 */

int CB_SetPrecedency(tk_display,comboid,number,precedency,delay)
ComboID comboid;
TkDisplay *tk_display;
int number, precedency;
Bool delay;
{
 ComboStruct *combo;
 

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;
 

 return LS_SetPrecedency(tk_display,combo->list, number, precedency, delay);

}





/*
 *
 * Desactive la combo
 *
 *
 */

int CB_Gray(tk_display,comboid)
TkDisplay *tk_display;
ComboID comboid;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;


 if((combo->state&Grayed)!=Grayed){
   LS_Gray(tk_display,combo->list);
   ED_Gray(tk_display,combo->edit);
   BN_Gray(tk_display,combo->button);
   combo->state=combo->state+Grayed;
 }
 return 0;
}





/* 
 *
 * Reactive la combo
 *
 *
 */

int CB_Ungray(tk_display,comboid)
TkDisplay *tk_display;
ComboID comboid;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;


 if((combo->state&Grayed)==Grayed)
 {
   LS_Ungray(tk_display,combo->list);
   ED_Ungray(tk_display,combo->edit);
   BN_Ungray(tk_display,combo->button);
   combo->state=combo->state-Grayed;
 }
 return 0;
}





/*
 *
 * Desactive un element
 *
 *
 */

int CB_GrayItem(tk_display,comboid,number)
TkDisplay *tk_display;
ComboID comboid;
int number;
{
 ComboStruct *combo;
 

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;


 return LS_GrayItem(tk_display,combo->list, number);

}




/*
 *
 * Reactive un element
 *
 *
 */

int CB_UngrayItem(tk_display,comboid,number)
TkDisplay *tk_display;
ComboID comboid;
int number;
{
 ComboStruct *combo;
 

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;
 
 return LS_UngrayItem(tk_display,combo->list,number);

}




/*
 *
 * Retire le pixmap d'un element
 *
 *
 */

int CB_DeleteItemPixmap(tk_display,comboid,number,delay)
TkDisplay *tk_display;
ComboID comboid;
int number;
Bool delay;
{
 ComboStruct *combo;
 

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;
 
 return LS_DeleteItemPixmap(tk_display,combo->list,number,delay);

}




/*
 *
 * Retire le pixmap grayed d'un element
 *
 *
 */

int CB_DeleteItemPixmapGrayed(tk_display,comboid,number,delay)
TkDisplay *tk_display;
ComboID comboid;
int number;
Bool delay;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_DeleteItemPixmapGrayed(tk_display,combo->list,number,delay);
}




/*
 *
 * Retire le text d'un element
 *
 *
 */

int CB_DeleteItemtext(tk_display,comboid,number,delay)
TkDisplay *tk_display;
ComboID comboid;
int number;
Bool delay;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_DeleteItemtext(tk_display,combo->list,number,delay);

}





/*
 *
 * Autorise la selection multiple d'elements
 * (default=NO)
 *
 *
 */

int CB_AllowMultipleSelection(tk_display,comboid)
TkDisplay *tk_display;
ComboID comboid;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_AllowMultipleSelection(tk_display,combo->list);

}





/*
 *
 * Interdit la selection multiple d'elements
 * (default=NO)
 *
 *
 */

int CB_ForbidMultipleSelection(tk_display,comboid)
TkDisplay *tk_display;
ComboID comboid;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_ForbidMultipleSelection(tk_display,combo->list);

}





/*
 *
 * Change l'element courant
 *
 *
 */

int CB_SetActiveItem(tk_display,comboid,number)
TkDisplay *tk_display;
ComboID comboid;
int number;
{
 ComboStruct *combo;
 unsigned char *text;
 int ret;

 /*fprintf(stderr,"Combo SetActiveItem start\n");*/

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else{ 
	/*fprintf(stderr,"comboid=%d  maxwidgets=%d  class=%d combo=%ld\n",comboid,tk_display->maxwidgets,tk_display->widgets[comboid].class,tk_display->widgets[comboid].combo);*/
	return -1;
	}
 /*fprintf(stderr,"LS setactive\n");*/
 ret=LS_SetActiveItem(tk_display,combo->list,number);
 if(ret<0) return ret;
 /*fprintf(stderr,"item_GetText\n");*/
 item_GetText(tk_display,combo->list,number,(unsigned char **)&text);
 /*fprintf(stderr,"CB %s\n",text);*/
 return ED_SetText(tk_display,combo->edit,text,False); 
}




/*
 *
 * Selectionne un element
 *
 *
 */

int CB_SelectItem(tk_display,comboid,number)
TkDisplay *tk_display;
ComboID comboid;
int number;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_SelectItem(tk_display,combo->list,number);
}






/*
 *
 * Deselectionne un element
 *
 *
 */

int CB_UnselectItem(tk_display,comboid,number)
TkDisplay *tk_display;
ComboID comboid;
int number;
{
 ComboStruct *combo;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;

 return LS_UnselectItem(tk_display,combo->list,number);
}





/*
 *
 * Change la taille d'une boite Combo
 *
 *
 */



int CB_Configure(tk_display,comboid,x,y,width,height)
TkDisplay *tk_display;
ComboID comboid;
int x,y;
unsigned int width,height;
{
 int mask;
 XWindowChanges xwc;
 ComboStruct *combo;
 ListStruct *list;

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;
 list=tk_display->widgets[combo->list].list;


 xwc.x=combo->x=x;
 xwc.y=combo->y=y;
 if(width>=30) xwc.width=combo->width=width;
 else xwc.width=combo->width=30;
 if(height>=list->itemheight+10) xwc.height=combo->height=height;
 else xwc.height=combo->height=list->itemheight+10;
 mask=CWX|CWY|CWWidth;

 XConfigureWindow(tk_display->display,combo->window,mask,&xwc);

 switch(combo->type){

	case CB_LEFTNOEDITION:
	case CB_LEFTEDITION:
	case CB_SLEFTNOEDITION:
	case CB_SLEFTEDITION:
		
		ED_Configure(tk_display,combo->edit,19,-1,combo->width-20,list->itemheight+6);
		LS_Configure(tk_display,combo->list,combo->x,combo->y+list->itemheight+6+1,combo->width,combo->height-list->itemheight-6);
		break;

	case CB_RIGHTNOEDITION:
	case CB_RIGHTEDITION:
	case CB_SRIGHTNOEDITION:
	case CB_SRIGHTEDITION:

		BN_Configure(tk_display,combo->button,combo->width-20,-1,19,list->itemheight+6);
		ED_Configure(tk_display,combo->edit,-1,-1,combo->width-20,list->itemheight+6);
		LS_Configure(tk_display,combo->list,combo->x,combo->y+list->itemheight+6+1,combo->width,combo->height-list->itemheight-6);
		break;

   }

 return 0;

}





int CB_Resize(tk_display,comboid,width,height)
TkDisplay *tk_display;
ComboID comboid;
unsigned int width,height;
{
 ComboStruct *combo;
 ListStruct *list;


 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;
 list=tk_display->widgets[combo->list].list;


 if(width>=30) combo->width=width;
 else combo->width=30;
 if(height>=list->itemheight+10) combo->height=height;
 else combo->height=list->itemheight+10;
 

 XResizeWindow(tk_display->display,combo->window,combo->width, combo->height);

 switch(combo->type){

	case CB_LEFTNOEDITION:
	case CB_LEFTEDITION:
	case CB_SLEFTNOEDITION:
	case CB_SLEFTEDITION:
		
		ED_Configure(tk_display,combo->edit,19,-1,combo->width-20,list->itemheight+6);
		LS_Configure(tk_display,combo->list,combo->x,combo->y+list->itemheight+6+1,combo->width,combo->height-list->itemheight-6);
		return 0;
		break;

	case CB_RIGHTNOEDITION:
	case CB_RIGHTEDITION:
	case CB_SRIGHTNOEDITION:
	case CB_SRIGHTEDITION:

		BN_Configure(tk_display,combo->button,combo->width-20,-1,19,list->itemheight+6);
		ED_Configure(tk_display,combo->edit,-1,-1,combo->width-20,list->itemheight+6);
		LS_Configure(tk_display,combo->list,combo->x,combo->y+list->itemheight+6+1,combo->width,combo->height-list->itemheight-6);
		return 0;
		break;
	default : return -1;
		break;

   }

 return 0;

}




int CB_Move(tk_display,comboid,x,y)
TkDisplay *tk_display;
ComboID comboid;
int x, y;
{
 ComboStruct *combo;
 ListStruct *list;

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;
 list=tk_display->widgets[combo->list].list; 
 
 combo->x=x;
 combo->y=y;
  

 XMoveWindow(tk_display->display,combo->window,x,y);

 switch(combo->type){

	case CB_LEFTNOEDITION:
	case CB_LEFTEDITION:
	case CB_SLEFTNOEDITION:
	case CB_SLEFTEDITION:
		
		/*ED_Configure(tk_display,combo->edit,19,-1,combo->width-20,list->itemheight+6);
		*/
		LS_Configure(tk_display,combo->list,combo->x,combo->y+list->itemheight+6+1,combo->width,combo->height-list->itemheight-6);
		break;

	case CB_RIGHTNOEDITION:
	case CB_RIGHTEDITION:
	case CB_SRIGHTNOEDITION:
	case CB_SRIGHTEDITION:

		/*BN_Configure(tk_display,combo->button,combo->width-20,-1,19,list->itemheight+6);
		ED_Configure(tk_display,combo->edit,-1,-1,combo->width-20,list->itemheight+6);
		*/
		LS_Configure(tk_display,combo->list,combo->x,combo->y+list->itemheight+6+1,combo->width,combo->height-list->itemheight-6);
		break;

   }

 return 0;

}






