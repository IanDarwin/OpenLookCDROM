/*
 *
 * 	ev_loop.c  
 * 	boucle principale d'evenements
 *
 * 	Modification :  26/12/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appears in all copies and that both that
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
 *	Internet:       rivas@email.teaser.fr
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
#include <X11/iman/messages.h>



/*
 *
 * Gerer les evenements des widgets 
 *
 *
 */

int tk_GetWidgetEvents(tk_display,tk_event,dont_wait)
TkDisplay *tk_display;
TkEvent *tk_event;
int dont_wait;
{
 ButtonStruct *bn_ptr;
 ScrollbarStruct *sb_ptr;
 EditStruct *ed_ptr;
 ListStruct *ls_ptr;
 ComboStruct *cb_ptr;
 MenuStruct *mn_ptr;

 ButtonID buttonid;
 ScrollbarID scrollid;
 EditID editid;
 ListID listid;
 ComboID comboid;
 MenuID menuid;

 Bool bn_cont;
 XEvent event;
 Window win;
 int loop,emit, result, eventresult;
 int done;
 int wd_type, wid_number;
 int ev_ret;



 done=emit=0;
 tk_event->ev_type=NO_EVENT;
 
 
 if(dont_wait==True && XEventsQueued(tk_display->display,QueuedAfterFlush)==0)
 {
	tk_event->ev_type=NO_EVENT;
	return 0;
 }
 else {

 XNextEvent(tk_display->display,&event);

 tk_event->ev_type=0;
 tk_event->ev_widget=0;

 tk_event->button=-1;
 tk_event->scroll=-1;
 tk_event->edit=-1;
 tk_event->menu=-1;
 tk_event->combo=-1;


 tk_event->ev_type=NO_EVENT;

 switch(event.type){

	case Expose:	wd_type=0;
			wid_number=-1;
			emit=0;
			if(event.xexpose.count==0)
			  wd_type=WID_GetWindowType(tk_display, event.xexpose.window);
			switch(wd_type){

			    case WI_BUTTON: 
				buttonid=WID_GetWindowNumber(tk_display,event.xexpose.window);
#ifdef DEBUG
				fprintf(stderr,"BN draw id=%d  type=%d\n",buttonid,tk_display->widgets[buttonid].button->type);
#endif
				if(buttonid>=0&&buttonid<tk_display->maxwidgets) 
				  switch(tk_display->widgets[buttonid].button->type)	
				  {
					case BN_PUSHBUTTON : if(event.xexpose.count==0) bn_DrawPushButton(tk_display,buttonid); 
							   /*fprintf(stderr,"Retour de bn_DrawPush\n");*/
							   tk_event->ev_type=NO_EVENT;
							   break; 
					case BN_COMBOBUTTON : if(event.xexpose.count==0) bn_DrawComboButton(tk_display,buttonid); 
							   tk_event->ev_type=NO_EVENT;
							   break;
					case BN_POPUPBUTTON : if(event.xexpose.count==0) bn_DrawPopupButton(tk_display,buttonid); 
							   tk_event->ev_type=NO_EVENT;
							   break;
					case BN_REPEATBUTTON:if(event.xexpose.count==0) bn_DrawRepeatButton(tk_display,buttonid); 
							   tk_event->ev_type=NO_EVENT;
							   break;
	
					case BN_CROSSBUTTON: if(event.xexpose.count==0) bn_DrawCrossButton(tk_display,buttonid); 
							   tk_event->ev_type=NO_EVENT;
							   break;
					case BN_RADIOBUTTON: if(event.xexpose.count==0) bn_DrawRadioButton(tk_display,buttonid); 
							   tk_event->ev_type=NO_EVENT;
							   break; 
					case BN_POPUPRADIOBUTTON: if(event.xexpose.count==0) bn_DrawPopupRadioButton(tk_display,buttonid); 
							   tk_event->ev_type=NO_EVENT;
							   break;
					case BN_SCROLLBUTTON: if(event.xexpose.count==0) bn_DrawScrollButton(tk_display,buttonid); 
							   tk_event->ev_type=NO_EVENT;
							   break;
					case BN_THUMBBUTTON: if(event.xexpose.count==0) bn_DrawThumbButton(tk_display,buttonid); 
							   tk_event->ev_type=NO_EVENT;
							   break; 
					case BN_CHECKBUTTON: if(event.xexpose.count==0) bn_DrawCheckButton(tk_display,buttonid); 
							   tk_event->ev_type=NO_EVENT;
							   break;

				}
#ifdef DEBUG
				fprintf(stderr,"BN draw end\n");
#endif
				break;


			    case WI_EDIT:
				emit=0;
				editid=WID_GetWindowNumber(tk_display,event.xexpose.window);
#ifdef DEBUG
				fprintf(stderr,"ED draw id=%d  type=%d\n",editid,tk_display->widgets[editid].edit->type);
#endif
				if(editid>=0) 
				{
			    	  if(event.xexpose.count==0) ed_DrawEdit(tk_display,editid); 
				  tk_event->ev_type=NO_EVENT;
				  emit=0;
				  break;  
				}
#ifdef DEBUG
				fprintf(stderr,"ED draw \n");
#endif
				break;


			    case WI_LIST:
				emit=0;
				listid=WID_GetWindowNumber(tk_display,event.xexpose.window);
#ifdef DEBUG
				fprintf(stderr,"LS draw id=%d  type=%d\n",listid,tk_display->widgets[listid].list->type);
#endif
				if(listid>=0)	
				{
					if(event.xexpose.count==0) ls_DrawList(tk_display,listid); 
					tk_event->ev_type=NO_EVENT;
					emit=0;
				}
#ifdef DEBUG
				fprintf(stderr,"LS draw end\n");
#endif
				break;


			    case WI_MENU:
				emit=0;
				menuid=WID_GetWindowNumber(tk_display,event.xexpose.window);
#ifdef DEBUG
				fprintf(stderr,"MN draw id=%d  type=%d\n",menuid,tk_display->widgets[menuid].menu->type);
#endif
				if(menuid>=0) switch(tk_display->widgets[menuid].menu->type)	
				{
					case MN_FLOATING : 
							   if(event.xexpose.count==0)
							     mn_DrawFloatingMenu(tk_display,menuid);
							   tk_event->ev_type=NO_EVENT;
							   emit=0;
							   break;

					case MN_MENUBAR : 
							   if(event.xexpose.count==0)
							     mn_DrawMenuBar(tk_display,menuid);
							   tk_event->ev_type=NO_EVENT;
							   emit=0;
							   break;
				}
#ifdef DEBUG
				fprintf(stderr,"MN draw end\n");
#endif
				break;


			    default : emit=1;
				 tk_event->ev_type=XLIBEVENT;
				 tk_event->event=event;	

			}
			break;



	case EnterNotify:
	case LeaveNotify:
	case ConfigureNotify:
	case PropertyNotify:
	case MotionNotify :
	case KeyPress	  :
	case KeyRelease	  :
	case FocusOut     :
	case FocusIn      :
	case ButtonPress  :
	case ButtonRelease: 

			   wd_type=-1;
			   wid_number=-1;
			   if(event.type==ButtonPress||event.type==ButtonRelease){
				wd_type=WID_GetWindowType(tk_display, event.xbutton.window);
				win=event.xbutton.window;}
			    else if(event.type==KeyPress||event.type==KeyRelease) {
				wd_type=WID_GetWindowType(tk_display, event.xkey.window);
				win=event.xkey.window;}
			    else if(event.type==FocusIn||event.type==FocusOut){
				wd_type=WID_GetWindowType(tk_display, event.xfocus.window);
				win=event.xfocus.window;}
			    else if(event.type==MotionNotify){
				wd_type=WID_GetWindowType(tk_display, event.xmotion.window);
				win=event.xmotion.window;}
			    else if(event.type==PropertyNotify){
				wd_type=WID_GetWindowType(tk_display, event.xproperty.window);
				win=event.xproperty.window;}
			    else if(event.type==ConfigureNotify){
				wd_type=WID_GetWindowType(tk_display, event.xconfigure.window);
				win=event.xconfigure.window;}
			    else if(event.type==ClientMessage){
				wd_type=WID_GetWindowType(tk_display, event.xclient.window);
				win=event.xclient.window;}
			    else if(event.type==EnterNotify){
				wd_type=WID_GetWindowType(tk_display, event.xcrossing.window);
				win=event.xcrossing.window;}
			    else if(event.type==LeaveNotify){
				wd_type=WID_GetWindowType(tk_display, event.xcrossing.window);
				win=event.xcrossing.window;}

			  if(event.type==ButtonPress && tk_display->action.type==MenuSelectingAction&&tk_display->action.menu>=0&&wd_type!=WI_MENU)
			 	return 0;
			  if(event.type==ButtonPress && tk_display->action.type==MenuSelectingAction&&tk_display->action.menu>=0&&wd_type==WI_MENU&&event.xbutton.button!=Button1)
				return 0;
			  if(event.type==ButtonRelease && tk_display->action.type==MenuSelectingAction&&tk_display->action.menu>=0&&wd_type==WI_MENU&&event.xbutton.button!=Button1)
				return 0;
			  if(event.type==ButtonRelease && tk_display->action.type==MenuSelectingAction&&tk_display->action.menu>=0&wd_type==WI_MENU&&event.xbutton.button==Button1)
			  {
				wid_number=WID_GetWindowNumber(tk_display,win);
				if(wid_number>=0 && tk_display->widgets[wid_number].menu->type==MN_FLOATING&&tk_display->widgets[wid_number].menu->parency_class==0&&tk_display->widgets[tk_display->action.menu].menu->type==MN_MENUBAR)
				  goto MENU_CASE;
				if(wid_number>=0 && tk_display->widgets[wid_number].menu->type==MN_FLOATING&&tk_display->widgets[wid_number].menu->parency_class==0&&tk_display->widgets[tk_display->action.menu].menu->type==MN_FLOATING&&tk_display->action.menu!=wid_number)
				  goto MENU_CASE;
				if(wid_number>=0 && tk_display->widgets[wid_number].menu->type==MN_MENUBAR&&tk_display->widgets[tk_display->action.menu].menu->type==MN_FLOATING)
				  goto MENU_CASE;
			  }
			  if(event.type==ButtonRelease && tk_display->action.type==MenuSelectingAction&&tk_display->action.menu>=0&&wd_type!=WI_MENU&&event.xbutton.button==Button1)
			  {
				MENU_CASE:
			    	event.xbutton.window=tk_display->widgets[tk_display->action.menu].menu->window;
			    	event.xbutton.x=60000;
				event.xbutton.y=60000;
				wd_type=WI_MENU;
				win=event.xbutton.window;
				goto SWITCH_START;
			  }

			SWITCH_START:

			  switch(wd_type){

				case WI_BUTTON: 
					buttonid=WID_GetWindowNumber(tk_display,win);
					if(buttonid>=0) switch(tk_display->widgets[buttonid].button->type)	
					{


					 case BN_COMBOBUTTON:
					 case BN_PUSHBUTTON : 
						   bn_ptr=tk_display->widgets[buttonid].button;
						   tk_event->event=event;
						   if(bn_ptr->parency_class<=0)
						     ev_ret=bn_PushButtonEvents(tk_display,buttonid,tk_event); 
						   else if(bn_ptr->parency_class==WI_COMBO)
						     ev_ret=bn_ComboButtonEvents(tk_display,buttonid,tk_event); 

						   if(bn_ptr->parency_class<=0 && ev_ret!=0)
						   {
							emit=1;
							tk_event->ev_type=ev_ret; 
							tk_event->ev_widget=WI_BUTTON; 
							tk_event->button=buttonid;
							tk_event->event=event;
						   }
						   else if(bn_ptr->parency_class==WI_COMBO && ev_ret!=0)
						   {
							tk_event->ev_type=ev_ret; 
							tk_event->ev_widget=WI_BUTTON; 
							tk_event->button=buttonid;
							tk_event->event=event; 
							cb_ptr=(ComboStruct *)tk_display->widgets[bn_ptr->parency_number].combo;
							ev_ret=cb_ComboEvents(tk_display,bn_ptr->parency_number,tk_event);
							if(cb_ptr->parency_class<=0 && ev_ret!=0) { 
								emit=1;
								tk_event->ev_type=ev_ret; 
								tk_event->ev_widget=WI_COMBO; 
								tk_event->combo=bn_ptr->parency_number;
								tk_event->event=event;
								}
							else tk_event->ev_type=emit=0;
						   }
						   else ev_ret=emit=0;
						   break;

				 	 case BN_POPUPBUTTON : 
						   bn_ptr=(ButtonStruct *)tk_display->widgets[buttonid].button;
						   ev_ret=0;
						   tk_event->event=event;
						   ev_ret=bn_PopupButtonEvents(tk_display,buttonid,tk_event); 
						   /*(void)fprintf(stderr,"type=%d  parency=%d\n",ev_ret,bn_ptr->parency);*/
						   if(bn_ptr->parency_class<=0 && ev_ret!=0) { 
							emit=1;
							tk_event->ev_type=ev_ret; 
							tk_event->ev_widget=WI_BUTTON; 
							tk_event->button=buttonid;
							tk_event->event=event;
							if(ev_ret==BN_PUSHED && (bn_ptr->state&Pushed)!=Pushed){
								emit=0;
								tk_event->button=-1;
								tk_event->ev_type=0;}
							}
						   else emit=0;
						   break;

					 case BN_CROSSBUTTON: 
						   bn_ptr=(ButtonStruct *)tk_display->widgets[buttonid].button;
						   ev_ret=0;
						   tk_event->event=event;
						   ev_ret=bn_CrossButtonEvents(tk_display,buttonid,tk_event); 
						   if(bn_ptr->parency_class<=0 && ev_ret!=0) { 
							emit=1;
							tk_event->ev_type=ev_ret; 
							tk_event->ev_widget=WI_BUTTON; 
							tk_event->button=buttonid;
							tk_event->event=event;
							}
						   else emit=0;
						   break;

					 case BN_RADIOBUTTON: 
						   bn_ptr=(ButtonStruct *)tk_display->widgets[buttonid].button;
						   tk_event->event=event;
						   ev_ret=bn_RadioButtonEvents(tk_display,buttonid,tk_event); 
						   if(bn_ptr->parency_class<=0 && ev_ret!=0) { 
							emit=1;
							tk_event->ev_type=ev_ret; 
							tk_event->ev_widget=WI_BUTTON; 
							tk_event->button=buttonid;
							tk_event->event=event;
							}
						   else emit=0;
						   break;

					 case BN_POPUPRADIOBUTTON: 
						   bn_ptr=(ButtonStruct *)tk_display->widgets[buttonid].button;
						   tk_event->event=event;
						   ev_ret=bn_PopupRadioButtonEvents(tk_display,buttonid,tk_event); 
						   if(bn_ptr->parency_class<=0 && ev_ret!=0) 
						   { 
							emit=1;
							tk_event->ev_type=ev_ret; 
							tk_event->ev_widget=WI_BUTTON; 
							tk_event->button=buttonid;
							tk_event->event=event;
						   }
						   else emit=0;
						   break;

				
					 case BN_CHECKBUTTON: 
						   bn_ptr=(ButtonStruct *)tk_display->widgets[buttonid].button;
						   tk_event->event=event;
						   ev_ret=bn_CheckButtonEvents(tk_display,buttonid,tk_event); 
						   if(bn_ptr->parency_class<=0 && ev_ret!=0) { 
							emit=1;
							tk_event->ev_type=ev_ret; 
							tk_event->ev_widget=WI_BUTTON; 
							tk_event->button=buttonid;
							tk_event->event=event;
							}
						   else emit=0;
						   break;

				
					 case BN_REPEATBUTTON:
						   bn_ptr=(ButtonStruct *)tk_display->widgets[buttonid].button;
						   bn_cont=bn_ptr->continuity;
						   tk_event->event=event;
						   ev_ret=bn_RepeatButtonEvents(tk_display,buttonid,tk_event); 
						   /*(void)fprintf(stderr,"type=%d  parency=%d\n",ev_ret,bn_ptr->parency);*/
						   if(bn_ptr->parency_class<=0 && ev_ret!=0) {
							emit=1;
							tk_event->ev_type=ev_ret; 
							tk_event->ev_widget=WI_BUTTON; 
							tk_event->button=buttonid;
							tk_event->event=event; 		
							if((ev_ret==BN_RELEASED && bn_cont==NO) || (ev_ret==BN_RELEASED && (bn_ptr->state&Pushed)==Pushed) || (ev_ret==BN_RELEASED && (bn_ptr->state&Blocked)==Blocked)) {
							 tk_event->ev_type=0; 
							 emit=0;} 
							 if((ev_ret==BN_PUSHED && bn_ptr->continuity==NO) || (ev_ret==BN_PUSHED && (bn_ptr->state&Blocked)==Blocked) || (ev_ret==BN_PUSHED && (bn_ptr->state&Grayed)==Grayed)) {
							 tk_event->ev_type=0; 
							 emit=0;}
							}
						   
						   else emit=0;
						   break;
				
					 case BN_SCROLLBUTTON:
						   bn_ptr=(ButtonStruct *)tk_display->widgets[buttonid].button;
						   bn_cont=bn_ptr->continuity;
						   tk_event->event=event;
						   ev_ret=bn_ScrollButtonEvents(tk_display,buttonid,tk_event); 
						   /*(void)fprintf(stderr,"type=%d  parency=%d\n",ev_ret,bn_ptr->parency_number);*/
						   if(bn_ptr->parency_class<=0 && ev_ret!=0 && ev_ret!=JUSTFOCUS) {
							emit=1;
							tk_event->ev_type=ev_ret; 
							tk_event->ev_widget=WI_BUTTON; 
							tk_event->button=buttonid;
							tk_event->event=event; 		
							if((ev_ret==BN_RELEASED && bn_cont==NO) || (ev_ret==BN_RELEASED && (bn_ptr->state&Pushed)==Pushed) || (ev_ret==BN_RELEASED && (bn_ptr->state&Blocked)==Blocked)) {
							 tk_event->ev_type=0; 
							 emit=0;} 
							 if((ev_ret==BN_PUSHED && bn_ptr->continuity==NO) || (ev_ret==BN_PUSHED && (bn_ptr->state&Blocked)==Blocked) || (ev_ret==BN_PUSHED && (bn_ptr->state&Grayed)==Grayed)) {
							 tk_event->ev_type=0; 
							 emit=0;}
							}
						   else if(bn_ptr->parency_class==WI_SCROLLBAR && ev_ret!=0)
							{
							emit=1;
							tk_event->ev_type=ev_ret; 
							tk_event->ev_widget=WI_BUTTON; 
							tk_event->button=buttonid;
							tk_event->event=event; 		
							if((ev_ret==BN_RELEASED && bn_cont==NO) || (ev_ret==BN_RELEASED && (bn_ptr->state&Pushed)==Pushed) || (ev_ret==BN_RELEASED && (bn_ptr->state&Blocked)==Blocked)) {
							  tk_event->ev_type=0; 
							  emit=0;} 
							if((ev_ret==BN_PUSHED && bn_ptr->continuity==NO) || (ev_ret==BN_PUSHED && (bn_ptr->state&Blocked)==Blocked) || (ev_ret==BN_PUSHED && (bn_ptr->state&Grayed)==Grayed)) {
							  tk_event->ev_type=0; 
							  emit=0;} 
							if(emit==1){ 
							  /*fprintf(stderr,"Je vais obtenir le SB");
							  fprintf(stderr," %d\n",bn_ptr->parency_number);*/
							  sb_ptr=(ScrollbarStruct *)tk_display->widgets[bn_ptr->parency_number].scroll;
							  ev_ret=sb_ScrollbarEvents(tk_display,bn_ptr->parency_number,tk_event);  
							  tk_event->ev_type=ev_ret;
							  tk_event->ev_widget=WI_SCROLLBAR;
							  tk_event->scroll=bn_ptr->parency_number;
							  if(ev_ret==0) emit=0; 
							  else emit=1;
							if(emit==1 && sb_ptr->parency_class==WI_LIST)
							 {
							  ls_ptr=(ListStruct *)tk_display->widgets[sb_ptr->parency_number].list;
							  ev_ret=ls_ListEvents(tk_display,sb_ptr->parency_number,tk_event);
							  tk_event->ev_type=ev_ret;
							  tk_event->ev_widget=WI_LIST;
							  tk_event->list=sb_ptr->parency_number;
							  if(ev_ret==0) emit=0; 
							  else emit=1;
							  }
	
							 }
							}
						   else emit=0;
						   break;

					 case BN_THUMBBUTTON : 
						   bn_ptr=(ButtonStruct *)tk_display->widgets[buttonid].button;
						   tk_event->event=event;
						   ev_ret=bn_ThumbButtonEvents(tk_display,buttonid,tk_event); 
						   
						   if(bn_ptr->parency_class==WI_SCROLLBAR && ev_ret!=0) { 
							emit=1;
							tk_event->ev_type=ev_ret; 
							tk_event->ev_widget=WI_BUTTON; 
							tk_event->button=buttonid;
							tk_event->event=event;
							sb_ptr=(ScrollbarStruct *)tk_display->widgets[bn_ptr->parency_number].scroll;
							ev_ret=sb_ScrollbarEvents(tk_display,bn_ptr->parency_number,tk_event);  
							tk_event->ev_type=ev_ret;
							tk_event->ev_widget=WI_SCROLLBAR;
							tk_event->scroll=bn_ptr->parency_number;
							if(ev_ret==0) emit=0; 
							else emit=1;
							if(emit==1 && sb_ptr->parency_class==WI_LIST)
							 {
							  ls_ptr=(ListStruct *)tk_display->widgets[sb_ptr->parency_number].list;
							  ev_ret=ls_ListEvents(tk_display,sb_ptr->parency_number,tk_event);
							  tk_event->ev_type=ev_ret;
							  tk_event->ev_widget=WI_LIST;
							  tk_event->list=sb_ptr->parency_number;
							  if(ev_ret==0) emit=0; 
							  else emit=1;
							  }
							}
						   else emit=0;
						   break;
					}
					break;


				case WI_SCROLLBAR:
					scrollid=WID_GetWindowNumber(tk_display,win);
					if(scrollid>=0)	
					{
					   sb_ptr=(ScrollbarStruct *)tk_display->widgets[scrollid].scroll;
					   tk_event->ev_type=XLIBEVENT;
					   tk_event->ev_widget=0;
					   tk_event->event=event;
					   ev_ret=sb_ScrollbarEvents(tk_display,scrollid,tk_event);  
					   if(ev_ret!=0) { 
						emit=1;
						tk_event->event=event;
						tk_event->ev_type=ev_ret;
						tk_event->ev_widget=WI_SCROLLBAR;
						tk_event->scroll=scrollid;
						if(emit==1 && sb_ptr->parency_class==WI_LIST)
						 {
						  ls_ptr=(ListStruct *)tk_display->widgets[sb_ptr->parency_number].list;
						  ev_ret=ls_ListEvents(tk_display,sb_ptr->parency_number,tk_event);
						  tk_event->ev_type=ev_ret;
						  tk_event->ev_widget=WI_LIST;
						  tk_event->list=sb_ptr->parency_number;
						  if(ev_ret==0) emit=0; 
						  else emit=1;
						  }
					   }
					   else tk_event->ev_type=emit=0;
					   
					}
					break;


				case WI_EDIT:
					editid=WID_GetWindowNumber(tk_display,win);
					if(editid>=0)
					{
					   ed_ptr=(EditStruct *)tk_display->widgets[editid].edit;
					   ev_ret=ed_EditEvents(tk_display,editid,&event);  
					   if(ed_ptr->parency_class<=0 && ev_ret!=0) 
					   { 
						 emit=1;
						 tk_event->event=event;
						 tk_event->ev_type=ev_ret;
						 tk_event->ev_widget=WI_EDIT;
						 tk_event->edit=editid;
					   }
					   else if(ed_ptr->parency_class==WI_COMBO && ev_ret!=0)
					   {
						emit=1;
						tk_event->event=event;
						tk_event->ev_type=ev_ret;
						tk_event->ev_widget=WI_EDIT;
						tk_event->edit=editid;
						cb_ptr=(ComboStruct *)tk_display->widgets[ed_ptr->parency_number].combo;
						ev_ret=cb_ComboEvents(tk_display,ed_ptr->parency_number,tk_event);
						if(cb_ptr->parency_class==0 && ev_ret!=0) { 
							 emit=1;
							 tk_event->event=event;
							 tk_event->ev_type=ev_ret;
							 tk_event->ev_widget=WI_COMBO;
							 tk_event->combo=ed_ptr->parency_number;
							}
						else tk_event->ev_type=emit=0;
					   }
					   else tk_event->ev_type=emit=0; 
					 }
					 
					 break;


				case WI_LIST: 
					   listid=WID_GetWindowNumber(tk_display,win);
					   ls_ptr=(ListStruct *)tk_display->widgets[listid].list;
					   tk_event->event=event;
					   tk_event->ev_type=XLIBEVENT;
					   ev_ret=ls_ListEvents(tk_display,listid,tk_event);  
					   if(ls_ptr->parency_class<=0 && ev_ret!=0)
					   {
						emit=1;
						tk_event->event=event;
						tk_event->ev_type=ev_ret;
						tk_event->ev_widget=WI_LIST;
						tk_event->list=listid;
					   }
					   else if(ls_ptr->parency_class==WI_COMBO && ev_ret!=0)
					   {
						emit=1;
						tk_event->event=event;
						tk_event->ev_type=ev_ret;
						tk_event->ev_widget=WI_COMBO;
						tk_event->list=listid;
						cb_ptr=(ComboStruct *)tk_display->widgets[ls_ptr->parency_number].combo;
						ev_ret=cb_ComboEvents(tk_display,ls_ptr->parency_number,tk_event);
						tk_event->ev_type=ev_ret;
						tk_event->event=event;
						tk_event->combo=ls_ptr->parency_number;
						tk_event->ev_widget=WI_COMBO;
						/*if(cb_ptr->parency_class<=0 && ev_ret!=0) { 
							 emit=1;
							 tk_event->event=event;
							 tk_event->ev_type=ev_ret;
							 tk_event->ev_widget=WI_COMBO;
							}
						else tk_event->ev_type=emit=0;*/
					    }
					    else tk_event->ev_type=emit=0; 
					    break;


				case WI_MENU:

					menuid=WID_GetWindowNumber(tk_display,win);
					if(menuid>=0) switch(tk_display->widgets[menuid].menu->type)	
					{

					case MN_FLOATING:

						mn_ptr=(MenuStruct *)tk_display->widgets[menuid].menu;
						tk_event->event=event;
						tk_event->ev_type=XLIBEVENT;
						ev_ret=mn_FloatingMenuEvents(tk_display,menuid,tk_event);  
						
						BOUCLE1:

						if(ev_ret>0)
						{
							emit=1;
							tk_event->event=event;
							tk_event->ev_type=ev_ret;
							tk_event->ev_widget=WI_MENU;
							tk_event->menu=menuid;
						}
						else
						{
							tk_event->event=event;
							tk_event->ev_type=NO_EVENT;
							tk_event->ev_widget=0;
							tk_event->menu=-1;
							emit=0;
						}
						
						break;

					

					case MN_MENUBAR:

						mn_ptr=(MenuStruct *)tk_display->widgets[menuid].menu;
						tk_event->event=event;
						tk_event->ev_type=XLIBEVENT;
						ev_ret=mn_MenuBarEvents(tk_display,menuid,tk_event);  
						
						BOUCLE2:

						if(mn_ptr->parency_class==0 && ev_ret!=MN_KEYUNKNOWN&&ev_ret>0) { 
							 emit=1;
							 tk_event->event=event;
							 tk_event->ev_type=ev_ret;
							 tk_event->ev_widget=WI_MENU;
							 tk_event->menu=menuid;
							}
						
						else tk_event->ev_type=emit=0; 
						break;
					


					default:
						/*fprintf(stderr,"Menu inconnu\n");*/
						break;

					}
					else fprintf(stderr,"Ev_loop: WI_MENU inconnu\n");
					break;


				default : 

					 emit=1;
					 tk_event->ev_type=XLIBEVENT;
					 tk_event->event=event;	
					 break;
				}
			break;



	case ClientMessage: 	/*fprintf(stderr,"Client Message type:%ld  message:%ld\n",event.xclient.message_type,event.xclient.data.l[0]);*/

				if(event.xclient.message_type==tk_display->atoms._IMAN_WM_MESSAGES&&(event.xclient.data.l[0]==WmJustLoaded||event.xclient.data.l[0]==WmResetColors))
				{  
				  /*fprintf(stderr,"WmJustLoaded  \n");*/
				  tk_GetConnection(tk_display);
				  tk_RefreshColors(tk_display);
				  WID_Refresh(tk_display,event.xclient.window);
				  if(event.xclient.data.l[0]==WmJustLoaded)
				    WID_SetOverride(tk_display,False);
				}
				else if(event.xclient.message_type==tk_display->atoms._IMAN_WM_MESSAGES&&event.xclient.data.l[0]==WmFreezeWidgets)
				  WID_Freeze(tk_display,event.xclient.window);
				else if(event.xclient.message_type==tk_display->atoms._IMAN_WM_MESSAGES&&event.xclient.data.l[0]==WmUnfreezeWidgets)
				  WID_Unfreeze(tk_display,event.xclient.window); 
				else if(event.xclient.message_type==tk_display->atoms.WM_PROTOCOLS&&event.xclient.data.l[0]==tk_display->atoms.WM_SAVE_YOURSELF)
				{
				  tk_display->wm.active=False;
				  tk_display->wm.main_window=0;
				  WID_SetOverride(tk_display,True);
				}
				emit=1;
				tk_event->ev_type=XLIBEVENT;
				tk_event->event=event;	
				break;


	default : emit=1;
		  tk_event->ev_type=XLIBEVENT;
		  tk_event->event=event;
		  break;	




	}
	if(emit==1) return 1;
	else return 0;

      }
}


