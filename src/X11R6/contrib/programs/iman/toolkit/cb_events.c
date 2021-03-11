/*
 *
 * 	cb_events.c  
 * 	evenements des boites combo
 *
 * 	Modification :  05/12/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy,  modify, distribute and sell this software and its
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
#include "X11/keysym.h"

#include <X11/iman/widgets.h>






/*
 *
 * Gestion des evenements d'une "combo box"
 *
 *
 */

int cb_ComboEvents(tk_display,comboid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ComboID comboid;
{
 int index;
 KeySym key;
 unsigned long ptr;
 unsigned char cp_int[5];
 int i,j,ret; 
 ComboStruct *combo;
 EditStruct *edit;
 ButtonStruct *button;
 ListStruct *list;

 if(comboid>=0 && comboid<tk_display->maxwidgets && tk_display->widgets[comboid].class==WI_COMBO&&tk_display->widgets[comboid].combo!=NULL)
   combo=tk_display->widgets[comboid].combo;
 else return -1;
 edit=tk_display->widgets[combo->edit].edit;
 button=tk_display->widgets[combo->button].button;
 list=tk_display->widgets[combo->list].list;

 switch(tk_event->ev_type){


	case JUSTFOCUS: 
	case BN_PUSHED:	
			if(combo->hasFocus==False&&(combo->state&Grayed)!=Grayed)
			{
			  combo->hasFocus=True;
			  XSetInputFocus(tk_display->display,edit->window,RevertToPointerRoot,CurrentTime); 
			}
			tk_event->ev_type=JUSTFOCUS;
			return JUSTFOCUS;
			break;

	case BN_RELEASED: /*fprintf(stderr,"Button released focus:%d  isOpen:%d\n",combo->hasFocus,combo->isOpen);*/

			if(combo->isOpen==False&&(combo->state&Grayed)!=Grayed)
			{
			  combo->isOpen=True;
			  
			  LS_Map(tk_display,combo->list);
			  /*XMoveResizeWindow(tk_display->display,combo->window,combo->x,combo->y,combo->width,combo->height);*/
			  XSetInputFocus(tk_display->display,list->listwindow,RevertToPointerRoot,CurrentTime);
			  memset(&tk_display->action,0,sizeof(ActionStruct));
			  tk_display->action.combo=tk_display->action.menu=-1;			  
			  if(combo->parency_class==0)
			  {
				tk_display->action.type=ComboSelectingAction; 
				tk_display->action.combo=comboid;
			  }
			
			}
			else if((combo->state&Grayed)!=Grayed)
			{
			  combo->isOpen=False;
			  XSetInputFocus(tk_display->display,edit->window,RevertToPointerRoot,CurrentTime); 
			  /*XMoveResizeWindow(tk_display->display,combo->window,combo->x,combo->y,combo->width,list->itemheight+5);*/
			  LS_Unmap(tk_display,combo->list);
			  memset(&tk_display->action,0,sizeof(ActionStruct));
			  tk_display->action.combo=tk_display->action.menu=-1;			  
			}
			tk_event->ev_type=0;
			return 0;
			break;

	case XLIBEVENT: switch(tk_event->event.type){

			case FocusOut:  
					/*fprintf(stderr,"Focus Out.. width:%d itemheight:%d  height:%d\n",combo->width,list->itemheight,combo->height);*/
					LS_Unmap(tk_display,combo->list);
					memset(&tk_display->action,0,sizeof(ActionStruct));
					tk_display->action.combo=tk_display->action.menu=-1;			  
					
					_LS_DrawFocus(tk_display,combo->list,OFF);
					combo->isOpen=False;
					combo->hasFocus=False;
					tk_event->ev_type=0;
					return 0;
					break;
			}
			tk_event->ev_type=0;
			return 0;
			break;

	case ED_KEYUNKNOWN: 
			index=0;
		        if((tk_event->event.xkey.state&1)==1) index=ShiftMapIndex+1;
			else if((tk_event->event.xkey.state&2)==2) index=LockMapIndex+1;
			else if((tk_event->event.xkey.state&4)==4) index=ControlMapIndex+1;
			else if((tk_event->event.xkey.state&8)==8) index=Mod1MapIndex+1;
			else if((tk_event->event.xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
			else if((tk_event->event.xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
			else if((tk_event->event.xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
			else if((tk_event->event.xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

			key=XLookupKeysym(&tk_event->event.xkey,index);

			switch(key){

				case XK_Tab: 
					
					if(combo->isOpen==True){
						if(index==0) LS_GiveFocus(tk_display,combo->list);
						else if(index==1) WID_SeekPreviousFocus(tk_display,combo->window,combo->parent,combo->top_level,UP+DOWN+INCRUST);
						}
					else{
						if(index==0) WID_SeekNextFocus(tk_display,combo->window,combo->parent,combo->top_level,UP+DOWN+INCRUST);
						else if(index==1) WID_SeekPreviousFocus(tk_display,combo->window,combo->parent,combo->top_level,UP+DOWN+INCRUST);
						}
					tk_event->ev_type=0;
					return 0;
					break;

				case XK_Down:

					if(combo->isOpen==True)
						LS_GiveFocus(tk_display,combo->list);
					else
					{
						combo->isOpen=True;
						LS_Map(tk_display,combo->list);
						LS_GiveFocus(tk_display,combo->list);
						memset(&tk_display->action,0,sizeof(ActionStruct));
						tk_display->action.combo=tk_display->action.menu=-1;			  
						if(combo->parency_class==0)
			  			{
							tk_display->action.type=ComboSelectingAction; 
							tk_display->action.combo=comboid;
						}
					}
					tk_event->ev_type=0;
					return 0;
					break;


				case XK_Up:
					
					if(combo->isOpen==True){
						combo->isOpen=False;
						LS_Unmap(tk_display,combo->list);
						memset(&tk_display->action,0,sizeof(ActionStruct));
						tk_display->action.combo=tk_display->action.menu=-1;			  
						}

					tk_event->ev_type=0;
					return 0;
					break;

				default: /*fprintf(stderr,"Key Unknown in edit/combo  index:%d\n",index);*/
					tk_event->ev_type=CB_KEYUNKNOWN;
					return CB_KEYUNKNOWN;
					break;

				}
			tk_event->ev_type=0;
			return 0;
			break;



	case LS_KEYUNKNOWN: index=0;
		        if((tk_event->event.xkey.state&1)==1) index=ShiftMapIndex+1;
			else if((tk_event->event.xkey.state&2)==2) index=LockMapIndex+1;
			else if((tk_event->event.xkey.state&4)==4) index=ControlMapIndex+1;
			else if((tk_event->event.xkey.state&8)==8) index=Mod1MapIndex+1;
			else if((tk_event->event.xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
			else if((tk_event->event.xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
			else if((tk_event->event.xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
			else if((tk_event->event.xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

			key=XLookupKeysym(&tk_event->event.xkey,index);

			switch(key){

				case XK_Tab: 
					
					if(index==1) ret=ED_GiveFocus(tk_display,combo->edit);
					else if(index==0){ 
						combo->isOpen=False;
						ret=WID_SeekNextFocus(tk_display,combo->window,combo->parent,combo->top_level,UP+DOWN+INCRUST);
						}
					fprintf(stderr,"Tab ret=%d\n",ret);
					tk_event->ev_type=0;
					return 0;
					break;


				case XK_Escape:
					
					if(combo->isOpen==True){
						combo->isOpen=False;
						ED_GiveFocus(tk_display,combo->edit);
						LS_Unmap(tk_display,combo->list);
						memset(&tk_display->action,0,sizeof(ActionStruct));
						tk_display->action.combo=tk_display->action.menu=-1;			  
						}

					return 0;
					break;

				default: /*fprintf(stderr,"Key Unknown in list/combo  index:%d\n",index);*/
					return CB_KEYUNKNOWN;
					break;

				}
			    tk_event->ev_type=0;
			    return 0;
			    break;


	case LS_DOUBLECLICKED:
	case LS_VALIDATION: 
			/*fprintf(stderr,"Validation recue\n");*/
			i=LS_GetItemSelected(tk_display,combo->list);
			if(i>=0)
			{
				ED_SetText(tk_display,combo->edit,list->items[i].text,YES);
				ED_SetSelection(tk_display,combo->edit,0,sizeof(list->items[i].text),YES);
				ED_GiveFocus(tk_display,combo->edit);
				/*fprintf(stderr,"Edit 1 passe\n");*/
				edit->sel1=edit->sel2=edit->length;
				ED_SetCursorPosition(tk_display,combo->edit,edit->length,False);
				/*fprintf(stderr,"Edit 2 passe\n");*/
				LS_Unmap(tk_display,combo->list); 
				/*fprintf(stderr,"List 1 passe\n");*/
				memset(&tk_display->action,0,sizeof(ActionStruct));
			  	tk_display->action.combo=tk_display->action.menu=-1;			  
				combo->isOpen=False;
				return CB_VALIDATION;
			}
			else return 0;
			break;

	case ED_VALIDATION:

			LS_Unmap(tk_display,combo->list); 
			memset(&tk_display->action,0,sizeof(ActionStruct));
			tk_display->action.combo=tk_display->action.menu=-1;			  
			combo->isOpen=False;
			return CB_PROPOSITION;
			break;


	default: tk_event->ev_type=0;
		 return 0;
		 break;

 }
 return 0;
}



