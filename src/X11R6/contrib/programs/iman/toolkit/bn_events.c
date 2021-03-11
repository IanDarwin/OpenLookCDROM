/*
 *
 * 	bn_events.c  
 * 	gestion des evenements des boutons 
 *
 * 	Modification :  26/12/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute and sell this software and its
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
#include <X11/keysym.h>

#include <X11/iman/widgets.h>




			/****** Evenements des BN_PUSHBUTTONs *******/


int bn_ComboButtonEvents(tk_display,buttonid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ButtonID buttonid;
{
 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   return bn_PushButtonEvents(tk_display,buttonid,tk_event);
 else return -1;
}





int bn_PushButtonEvents(tk_display,buttonid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ButtonID buttonid;
{
 unsigned long ptr;
 XEvent event,send_event;
 KeySym key;
 int index;
 ButtonStruct *button;
 ComboStruct *combo;
 WidgetStruct *widptr;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];

 event=tk_event->event;

 switch(event.type){

	case ButtonPress:

	      if((button->state&Frozen)!=Frozen)
	      {
		if(button->hasFocus==NO)
		{
		 if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed && button->neverFocus==False) 
		 { 
		  button->hasFocus=YES;
		  XSetInputFocus(tk_display->display,button->window,RevertToPointerRoot,event.xbutton.time);
		  XSetWindowBorder(tk_display->display,button->window,widptr->colors->focus);

		  ptr=(unsigned long)(button->window);
		  XChangeProperty(tk_display->display,button->top_level,tk_display->atoms._IMAN_WM_FOCUS,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
		 }
		 XRaiseWindow(tk_display->display,button->top_level);
		}


		if(event.xbutton.button==Button1)
		{
		  if(tk_display->action.type==ComboSelectingAction) 
		  {
		    combo=tk_display->widgets[tk_display->action.combo].combo; 	
		    if(button->parency_class<=0||WID_IsSubwidget(tk_display,buttonid,tk_display->action.combo)==False)
		    {
		   	combo->isOpen=False;
		   	if(button->hasFocus==False) XSetInputFocus(tk_display->display,tk_display->widgets[combo->edit].edit->window,RevertToPointerRoot,CurrentTime); 
		   	LS_Unmap(tk_display,combo->list);
		   	memset(&tk_display->action,0,sizeof(ActionStruct));
		   	tk_display->action.combo=tk_display->action.menu=-1;
		    }		    
		  }    
				
		  button->continuity=YES;
		  if((button->state&Pushed)!=Pushed && (button->state&Grayed)!=Grayed && (button->state&Blocked)!=Blocked)
		  { 
		    BN_Push(tk_display,buttonid);
		    return BN_PUSHED; 
		  }
		  else if((button->state&Pushed)!=Pushed && (button->state&Grayed)!=Grayed && (button->state&Blocked)==Blocked)
		    return BN_PRESSED;
		  else if((button->state&Grayed)==Grayed)
		    return JUSTFOCUS;
		}
		else
		  return JUSTFOCUS;
		
	      }
	      else{
		XRaiseWindow(tk_display->display,button->top_level); 
 		return 0;
		}
	      break;


	case ButtonRelease:

		if(button->continuity==YES && event.xbutton.button==Button1&&(button->state&Frozen)!=Frozen)
		{
		  
		  button->continuity=NO;
		  if( event.xbutton.x>=0 && event.xbutton.y>=0 && event.xbutton.x<button->width && event.xbutton.y<button->height)
		  {
			if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed&& (button->state&Pushed)==Pushed){ 
			  BN_Unpush(tk_display,buttonid);
			  return BN_RELEASED;}
	
			else if((button->state&Blocked)==Blocked && (button->state&Grayed)!=Grayed)
			   return BN_UNPRESSED;
			
			else return 0;
		  }
		  else {
			if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed){ 
			  BN_Unpush(tk_display,buttonid);
			  return 0;}
	
			else return 0;
		  }

		}
		return 0;
		break;


	case KeyPress:

		index=0;
		if((event.xkey.state&1)==1) index=ShiftMapIndex+1;
		else if((event.xkey.state&2)==2) index=LockMapIndex+1;
		else if((event.xkey.state&4)==4) index=ControlMapIndex+1;
		else if((event.xkey.state&8)==8) index=Mod1MapIndex+1;
		else if((event.xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
		else if((event.xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
		else if((event.xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
		else if((event.xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

		key=XLookupKeysym(&event.xkey,index);
			
		if((button->state&Frozen)!=Frozen) 
		switch(key){

		  case XK_Tab: 
				break;

		  case XK_space:
		  case XK_Return: 
			VALID1:			
				if((button->state&Pushed)!=Pushed && (button->state&Grayed)!=Grayed && (button->state&Blocked)!=Blocked)
				  { BN_Push(tk_display,buttonid);
				    return BN_PUSHED; }
				 else if((button->state&Grayed)!=Grayed && (button->state&Blocked)==Blocked)
				    return BN_PRESSED;
								
				break;

		  default: 
			   if(button->txt_key>0 && key==button->text[button->txt_key-1]) goto VALID1;
			   else if(button->txt_key>0 && key+32==button->text[button->txt_key-1]) goto VALID1;
			   else if(button->txt_key>0 && key-32==button->text[button->txt_key-1]) goto VALID1;
			   /*else if(key>=32 && key<=175)  bn_KeyFocus(tk_display,button->top_level,key,&event);*/
		  	
			break;
		}
		return 0;
		break;


	case KeyRelease:

		index=0;
		if((event.xkey.state&1)==1) index=ShiftMapIndex+1;
		else if((event.xkey.state&2)==2) index=LockMapIndex+1;
		else if((event.xkey.state&4)==4) index=ControlMapIndex+1;
		else if((event.xkey.state&8)==8) index=Mod1MapIndex+1;
		else if((event.xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
		else if((event.xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
		else if((event.xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
		else if((event.xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

		key=XLookupKeysym(&event.xkey,index);
			
		if((button->state&Frozen)!=Frozen)
		switch(key){

		  case XK_Tab:  if(index==0) WID_SeekNextFocus(tk_display,button->window,button->parent,button->top_level,UP+DOWN+INCRUST);
				if(index==1) WID_SeekPreviousFocus(tk_display,button->window,button->parent,button->top_level,UP+DOWN+INCRUST);
				break;

		  case XK_space:
		  case XK_Return: 
				
			VALID2:	
				if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed){ 
			  	 BN_Unpush(tk_display,buttonid);
			 	 return BN_RELEASED;}
	
				else if((button->state&Blocked)==Blocked && (button->state&Grayed)!=Grayed)
			 	  return BN_UNPRESSED;
				else return 0;
		  				  		

				break;

		  default: if(button->txt_key>0 && key==button->text[button->txt_key-1]) goto VALID2;
			   else if(button->txt_key>0 && key+32==button->text[button->txt_key-1]) goto VALID2;
			   else if(button->txt_key>0 && key-32==button->text[button->txt_key-1]) goto VALID2;
			   else if(key>=32 && key<=175) bn_KeyFocus(tk_display,button->top_level,key,&event);
			   else if(event.xkey.keycode!=52&&event.xkey.keycode!=66&&event.xkey.keycode!=68&&event.xkey.keycode!=38&&event.xkey.keycode!=65&&event.xkey.keycode!=70&&event.xkey.keycode!=72&&event.xkey.keycode!=98)
				return BN_KEYUNKNOWN;
			   break;
		}
		return 0;
		break;



	case FocusIn:

		if(event.xfocus.detail==NotifyAncestor||event.xfocus.detail==NotifyInferior||event.xfocus.detail==NotifyNonlinear)
		{
		  button->hasFocus=YES;
		  if((button->state&Grayed)!=Grayed && button->neverFocus!=YES)
		  {
		  	XSetWindowBorder(tk_display->display,button->window,widptr->colors->focus);
		  	ptr=(unsigned long)(button->window);
		  XChangeProperty(tk_display->display,button->top_level,tk_display->atoms._IMAN_WM_FOCUS,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
		  }
		}
		return 0;
		break;

	case FocusOut:

		if(event.xfocus.detail==NotifyAncestor||event.xfocus.detail==NotifyInferior||event.xfocus.detail==NotifyNonlinear)
		{
		  button->hasFocus=NO;
		  XSetWindowBorder(tk_display->display,button->window,widptr->colors->nofocus);
		}
		return 0;
		break;


	default : return 0;
		break;

 }

}





int bn_CrossButtonEvents(tk_display,buttonid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ButtonID buttonid;
{
 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   return bn_PopupButtonEvents(tk_display,buttonid,tk_event);
 else return -1;
}






int bn_RadioButtonEvents(tk_display,buttonid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ButtonID buttonid;
{
 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   return  bn_PopupButtonEvents(tk_display,buttonid,tk_event);
 else return -1;
}




int bn_PopupRadioButtonEvents(tk_display,buttonid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ButtonID buttonid;
{
 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   return bn_PopupButtonEvents(tk_display,buttonid,tk_event);
 else return -1;
}



int bn_CheckButtonEvents(tk_display,buttonid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ButtonID buttonid;
{
 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   return bn_PopupButtonEvents(tk_display,buttonid,tk_event);
 else return -1;
}





int bn_PopupButtonEvents(tk_display,buttonid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ButtonID buttonid;
{
 unsigned long ptr;
 XEvent send_event, event;
 int index;
 KeySym key;
 ButtonStruct *button;
 ComboStruct *combo;
 WidgetStruct *widptr;

 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];

 event=tk_event->event;

 /*fprintf(stderr,"Button Event %d  state:%d\n",event.type,(button->state&Grayed));*/
 switch(event.type)
 {

	case ButtonPress:

	      if((button->state&Frozen)!=Frozen)
	      {
		if(button->hasFocus==NO)
		{
		 if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed && button->neverFocus!=YES) 
		 { 
		  button->hasFocus=YES;
		  XSetInputFocus(tk_display->display,button->window,RevertToPointerRoot,event.xbutton.time);
		  XSetWindowBorder(tk_display->display,button->window,widptr->colors->focus);

		  ptr=(unsigned long)(button->window);
		  XChangeProperty(tk_display->display,button->top_level,tk_display->atoms._IMAN_WM_FOCUS,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
		  if(button->type==BN_CROSSBUTTON) bn_DrawCrossFocus(tk_display,buttonid);
		  else if(button->type==BN_RADIOBUTTON) bn_DrawRadioFocus(tk_display,buttonid);
		  else if(button->type==BN_POPUPRADIOBUTTON) bn_DrawPopupRadioFocus(tk_display,buttonid);
		  else if(button->type==BN_CHECKBUTTON) bn_DrawCheckFocus(tk_display,buttonid);
		  }
		 XRaiseWindow(tk_display->display,button->top_level);
		}
		else if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed && button->neverFocus!=YES) 
		  XSetInputFocus(tk_display->display,button->window,RevertToPointerRoot,event.xbutton.time);


		if(event.xbutton.button==Button1)
		{
		  button->continuity=YES;

		  if(tk_display->action.type==ComboSelectingAction) 
		  {
		    combo=tk_display->widgets[tk_display->action.combo].combo; 	
		    if(button->parency_class<=0||WID_IsSubwidget(tk_display,buttonid,tk_display->action.combo)==False)
		    {
		   	combo->isOpen=False;
		   	if(button->hasFocus==False) XSetInputFocus(tk_display->display,tk_display->widgets[combo->edit].edit->window,RevertToPointerRoot,CurrentTime); 
		   	LS_Unmap(tk_display,combo->list);
		   	memset(&tk_display->action,0,sizeof(ActionStruct));
		   	tk_display->action.combo=tk_display->action.menu=-1;
		    }		    
		  }

		 if((button->state&Blocked)!=Blocked && (button->state&Pushed)!=Pushed && (button->state&Grayed)!=Grayed)
		  { BN_Push(tk_display,buttonid);
		    if((button->state&Stopped)!=Stopped) button->state=button->state+Stopped;
		    return JUSTFOCUS; }
		 else if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed ){
		    if((button->state&Stopped)==Stopped) button->state=button->state-Stopped;
		    return JUSTFOCUS;}
		 else if((button->state&Blocked)!=Blocked || (button->state&Grayed)==Grayed)
		    return JUSTFOCUS;
		 else return JUSTFOCUS;
		}
		else return JUSTFOCUS;
		
	       }
	      else{
		XRaiseWindow(tk_display->display,button->top_level); 
 		return 0;
		}
	      break;


	case ButtonRelease:

		if(button->continuity==YES && event.xbutton.button==Button1&&(button->state&Frozen)!=Frozen&&(button->state&Grayed)!=Grayed)
		{
		  
		  button->continuity=NO;
		  if( event.xbutton.x>=0 && event.xbutton.y>=0 && event.xbutton.x<button->width && event.xbutton.y<button->height)
		  {
			if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed && (button->state&Stopped)==Stopped)
			  return BN_PUSHED;
	
			else if((button->state&Blocked)!=Blocked && (button->state&Stopped)!=Stopped && (button->state&Grayed)!=Grayed)
			 {
			  BN_Unpush(tk_display,buttonid);
			  return BN_RELEASED;}

			if((button->state&Blocked)==Blocked && (button->state&Grayed)!=Grayed && (button->state&Stopped)==Stopped){ 
			  return BN_PRESSED;}

			else return 0;
		  }
		  else {
			if((button->state&Blocked)!=Blocked && (button->state&Stopped)!=Stopped && (button->state&Grayed)!=Grayed)
			{
			  if((button->state&Stopped)!=Stopped) button->state=button->state+Stopped;
			  return 0;
			}
			else if((button->state&Blocked)!=Blocked && (button->state&Stopped)==Stopped && (button->state&Grayed)!=Grayed)
			{
			  if((button->state&Stopped)==Stopped) button->state=button->state-Stopped;  
			  BN_Unpush(tk_display,buttonid);
			  return 0;
			}


			else return 0;
		  }

		}
		return 0;
		break;


	case KeyPress:

		index=0;
		if((event.xkey.state&1)==1) index=ShiftMapIndex+1;
		else if((event.xkey.state&2)==2) index=LockMapIndex+1;
		else if((event.xkey.state&4)==4) index=ControlMapIndex+1;
		else if((event.xkey.state&8)==8) index=Mod1MapIndex+1;
		else if((event.xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
		else if((event.xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
		else if((event.xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
		else if((event.xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

		key=XLookupKeysym(&event.xkey,index);
					
		if((button->state&Frozen)!=Frozen&&(button->state&Grayed)!=Grayed)
		switch(key){

		  case XK_space:
		  case XK_Return: 
			VALID1:	
				
				if((button->state&Pushed)!=Pushed && (button->state&Grayed)!=Grayed )
				  { BN_Push(tk_display,buttonid);
				    if((button->state&Stopped)!=Stopped) button->state=button->state+Stopped;
				    return 0; }
				 else if((button->state&Grayed)!=Grayed){
				    if((button->state&Stopped)==Stopped) button->state=button->state-Stopped;
				    return 0;}
								
				break;

		  default: 
			   if(button->type==BN_POPUPBUTTON && button->txt_key>0 && key==button->text[button->txt_key-1]) goto VALID1;
			   else if(button->type==BN_POPUPBUTTON && button->txt_key>0 && key+32==button->text[button->txt_key-1]) goto VALID1;
			   else if(button->type==BN_POPUPBUTTON && button->txt_key>0 && key-32==button->text[button->txt_key-1]) goto VALID1;
			   /*else if(key>=32 && key<=175)  bn_KeyFocus(tk_display,button->top_level,key,&event);*/
			   break;
		}
		return 0;
		break;


	case KeyRelease:

		index=0;
		if((event.xkey.state&1)==1) index=ShiftMapIndex+1;
		else if((event.xkey.state&2)==2) index=LockMapIndex+1;
		else if((event.xkey.state&4)==4) index=ControlMapIndex+1;
		else if((event.xkey.state&8)==8) index=Mod1MapIndex+1;
		else if((event.xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
		else if((event.xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
		else if((event.xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
		else if((event.xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

		key=XLookupKeysym(&event.xkey,index);
			
		if((button->state&Frozen)!=Frozen&&(button->state&Grayed)!=Grayed)
		switch(key){

		  case XK_Tab:  if(index==0) WID_SeekNextFocus(tk_display,button->window,button->parent,button->top_level,UP+DOWN+INCRUST);
				if(index==1) WID_SeekPreviousFocus(tk_display,button->window,button->parent,button->top_level,UP+DOWN+INCRUST);
				break;

		  case XK_space:
		  case XK_Return: 
				
			VALID2:	
					
				if((button->state&Stopped)==Stopped && (button->state&Grayed)!=Grayed){
				  return BN_PUSHED;}
	
				else if((button->state&Stopped)!=Stopped && (button->state&Grayed)!=Grayed){
				  BN_Unpush(tk_display,buttonid);
			 	  return BN_RELEASED;}

				else return 0;
		  		break;

		  default: if(button->type==BN_POPUPBUTTON && button->txt_key>0 && key==button->text[button->txt_key-1]) goto VALID2;
			   else if(button->type==BN_POPUPBUTTON && button->txt_key>0 && key+32==button->text[button->txt_key-1]) goto VALID2;
			   else if(button->type==BN_POPUPBUTTON && button->txt_key>0 && key-32==button->text[button->txt_key-1]) goto VALID2;
			   else if(event.xkey.send_event!=True && key>=32 && key<=175)  bn_KeyFocus(tk_display,button->top_level,key,&event);
			   else if(event.xkey.keycode!=52&&event.xkey.keycode!=66&&event.xkey.keycode!=68&&event.xkey.keycode!=38&&event.xkey.keycode!=65&&event.xkey.keycode!=70&&event.xkey.keycode!=72&&event.xkey.keycode!=98)
			     return BN_KEYUNKNOWN;
			   break;
		}
		return 0;
		break;


	case FocusIn:
		

		if((button->state&Grayed)==Grayed || button->neverFocus==True)
		  return 0;
		else if((event.xfocus.detail==NotifyAncestor||event.xfocus.detail==NotifyInferior||event.xfocus.detail==NotifyNonlinear)&&(button->state&Grayed)!=Grayed && button->neverFocus==False)
		{  
		   button->hasFocus=YES;
		   XSetWindowBorder(tk_display->display,button->window,widptr->colors->focus);
		   if(button->type==BN_CROSSBUTTON) bn_DrawCrossFocus(tk_display,buttonid);
		   else if(button->type==BN_RADIOBUTTON) bn_DrawRadioFocus(tk_display,buttonid);
		   else if(button->type==BN_CHECKBUTTON) bn_DrawCheckFocus(tk_display,buttonid);
		   else if(button->type==BN_POPUPRADIOBUTTON) bn_DrawPopupRadioFocus(tk_display,buttonid);
		   ptr=(long)(button->window);
		   XChangeProperty(tk_display->display,button->top_level,tk_display->atoms._IMAN_WM_FOCUS,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
		}
		return 0;
		break;

	case FocusOut:

		button->hasFocus=NO;
		if(event.xfocus.detail==NotifyAncestor||event.xfocus.detail==NotifyInferior||event.xfocus.detail==NotifyNonlinear)
		{
		   XSetWindowBorder(tk_display->display,button->window,widptr->colors->nofocus);
		    if(button->type==BN_CROSSBUTTON) bn_DrawCrossFocus(tk_display,buttonid);
		    else if(button->type==BN_RADIOBUTTON) bn_DrawRadioFocus(tk_display,buttonid);
		    else if(button->type==BN_CHECKBUTTON) bn_DrawCheckFocus(tk_display,buttonid);
		    else if(button->type==BN_POPUPRADIOBUTTON) bn_DrawPopupRadioFocus(tk_display,buttonid);
		}
		return 0;
		break;


	default : return 0;
		break;

 }

}







int bn_ScrollButtonEvents(tk_display,buttonid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ButtonID buttonid;
{
 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   return bn_RepeatButtonEvents(tk_display,buttonid,tk_event);
 else return -1;
}




int bn_RepeatButtonEvents(tk_display,buttonid,tk_event)
TkDisplay *tk_display;
ButtonID buttonid;
TkEvent *tk_event;
{

 int i, j; 
 int xp, yp, xr, yr, ret;
 unsigned long ptr;
 unsigned int xk;
 Window w, r;
 XEvent send_event, eventbis, event;
 int index;
 KeySym key;
 Bool bn_CheckRepeatPush();
 ButtonStruct *button;
 ComboStruct *combo;
 WidgetStruct *widptr;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];
 
 event=tk_event->event;

 switch(event.type){


	case ButtonPress:

	      if((button->state&Frozen)!=Frozen)
	      {
		if(button->hasFocus==NO)
		{
		 if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed && button->neverFocus!=YES)
		 {
		  XSetInputFocus(tk_display->display,button->window,RevertToPointerRoot,CurrentTime);

		  ptr=(unsigned long)(button->window);
		  XChangeProperty(tk_display->display,button->top_level,tk_display->atoms._IMAN_WM_FOCUS,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);

		  button->hasFocus=YES;
		  XSetWindowBorder(tk_display->display,button->window,widptr->colors->focus);
		 }

		 if(button->continuity==False) XRaiseWindow(tk_display->display,button->top_level);
		}


		if(event.xbutton.button==Button1)
		{
		 
		 button->continuity=YES;
			
		 if(tk_display->action.type==ComboSelectingAction) 
		 {
		    combo=tk_display->widgets[tk_display->action.combo].combo; 	
		    if(button->parency_class<=0||WID_IsSubwidget(tk_display,buttonid,tk_display->action.combo)==False)
		    {
		   	combo->isOpen=False;
		   	if(button->hasFocus==False) XSetInputFocus(tk_display->display,tk_display->widgets[combo->edit].edit->window,RevertToPointerRoot,CurrentTime); 
		   	LS_Unmap(tk_display,combo->list);
		   	memset(&tk_display->action,0,sizeof(ActionStruct));
		   	tk_display->action.combo=tk_display->action.menu=-1;
		    }		    
		 }


 		 if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed)
 		 {
    		  if((button->state&Pushed)!=Pushed) BN_Push(tk_display,buttonid);
		  if(event.xbutton.x>=0 && event.xbutton.x<button->width && event.xbutton.y>=0 && event.xbutton.y<button->height) 
		    ret=BN_PUSHED;
		  else ret=0;

   		  XQueryPointer(tk_display->display,button->window,&w,&r,&xr,&yr,&xp,&yp,&xk);
   		  event.xbutton.x=xp;
		  event.xbutton.y=yp;
		  event.xbutton.state=xk;
		
		  XSync(tk_display->display,False);
		  XFlush(tk_display->display);
		  if(button->repeat==0)
		  {
		    for(i=0;i<tk_display->widget_double_click/2;i++)
			j=i;
		    button->repeat=True;
		  }
		  XSync(tk_display->display,False);
		  XFlush(tk_display->display);
  		  if(XCheckTypedWindowEvent(tk_display->display,button->window,ButtonRelease,&eventbis)==True && eventbis.xbutton.button==Button1)
     		  {   
			eventbis.type=ButtonRelease;
			eventbis.xbutton.type=ButtonRelease;
			XPutBackEvent(tk_display->display,&eventbis);
			return ret;
		  }
   		  else{
			XPutBackEvent(tk_display->display,&event);
  			return ret;
		     }
   		 }

  		 else if((button->state&Grayed)!=Grayed) return BN_PRESSED;
		 else if((button->state&Grayed)==Grayed) return JUSTFOCUS;
  		}
		else{ 
			if(button->type!=BN_SCROLLBUTTON) return JUSTFOCUS;
			else return JUSTFOCUS;
		    }
	     }
	     else{
		XRaiseWindow(tk_display->display,button->top_level); 
 		return 0;
		}
	     break;


	case ButtonRelease:

		
		if(button->continuity==YES && event.xbutton.button==Button1&&(button->state&Frozen)!=Frozen)
		{
		  /*(void)fprintf(stderr,"YES et Button1\n");*/
		  button->continuity=NO;
		  button->repeat=False;
		  if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed){ 
			  BN_Unpush(tk_display,buttonid);
			  return BN_RELEASED;}
	
		  else if((button->state&Blocked)==Blocked && (button->state&Grayed)!=Grayed)
			   return BN_UNPRESSED;
		  else return 0;		  
		 }
		 else { /*(void)fprintf(stderr,"pas de YES:%d ou Button1\n",button->continuity);*/
		return 0;}
		break;


	case KeyPress:

		if(button->neverFocus==False&&(button->state&Frozen)!=Frozen){

		index=0;
		if((event.xkey.state&1)==1) index=ShiftMapIndex+1;
		else if((event.xkey.state&2)==2) index=LockMapIndex+1;
		else if((event.xkey.state&4)==4) index=ControlMapIndex+1;
		else if((event.xkey.state&8)==8) index=Mod1MapIndex+1;
		else if((event.xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
		else if((event.xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
		else if((event.xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
		else if((event.xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

		key=XLookupKeysym(&event.xkey,index);
			
		switch(key){


		  case XK_space:
		  case XK_Return: 
			VALID1:	button->continuity=YES;
				if((button->state&Pushed)!=Pushed && (button->state&Grayed)!=Grayed && (button->state&Blocked)!=Blocked)
				  { BN_Push(tk_display,buttonid);
				    return BN_PUSHED; }
				 else if((button->state&Grayed)!=Grayed && (button->state&Blocked)==Blocked)
				    return BN_PRESSED;
				break;



		  default: 
			   if(button->txt_key>0 && key==button->text[button->txt_key-1]) goto VALID1;
			   else if(button->txt_key>0 && key+32==button->text[button->txt_key-1]) goto VALID1;
			   else if(button->txt_key>0 && key-32==button->text[button->txt_key-1]) goto VALID1;
			   /*else if(key>=32 && key<=175)  bn_KeyFocus(tk_display,button->top_level,key,&event);*/
			   break; 

		  }
		}
		return 0;
		break;


	case KeyRelease:

		if(button->neverFocus==False&&(button->state&Frozen)!=Frozen){

		index=0;
		if((event.xkey.state&1)==1) index=ShiftMapIndex+1;
		else if((event.xkey.state&2)==2) index=LockMapIndex+1;
		else if((event.xkey.state&4)==4) index=ControlMapIndex+1;
		else if((event.xkey.state&8)==8) index=Mod1MapIndex+1;
		else if((event.xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
		else if((event.xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
		else if((event.xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
		else if((event.xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

		key=XLookupKeysym(&event.xkey,index);
			
		switch(key){

		  case XK_Tab:  if(index==0) WID_SeekNextFocus(tk_display,button->window,button->parent,button->top_level,UP+DOWN+INCRUST);
				if(index==1) WID_SeekPreviousFocus(tk_display,button->window,button->parent,button->top_level,UP+DOWN+INCRUST);
				break;

		  case XK_space:
		  case XK_Return: 
			VALID2:	button->continuity=NO;
				if((button->state&Blocked)!=Blocked && (button->state&Grayed)!=Grayed){ 
			  	 BN_Unpush(tk_display,buttonid);
			 	 return BN_RELEASED;}
	
				else if((button->state&Blocked)==Blocked && (button->state&Grayed)!=Grayed)
			 	  return BN_UNPRESSED;
				else return 0;

				break; 


		  default: if(button->txt_key>0 && key==button->text[button->txt_key-1]) goto VALID2;
			   else if(button->txt_key>0 && key+32==button->text[button->txt_key-1]) goto VALID2;
			   else if(button->txt_key>0 && key-32==button->text[button->txt_key-1]) goto VALID2;
			   else if(key>=32 && key<=175)  bn_KeyFocus(tk_display,button->top_level,key,&event);
			   else if(event.xkey.keycode!=52&&event.xkey.keycode!=66&&event.xkey.keycode!=68&&event.xkey.keycode!=38&&event.xkey.keycode!=65&&event.xkey.keycode!=70&&event.xkey.keycode!=72&&event.xkey.keycode!=98)
			     return BN_KEYUNKNOWN;
			   break; 

		  }
		}
		return 0;
		break;


	case FocusIn:

		button->hasFocus=YES;
		if((event.xfocus.detail==NotifyAncestor||event.xfocus.detail==NotifyInferior||event.xfocus.detail==NotifyNonlinear)&&(button->state&Grayed)!=Grayed && button->neverFocus!=YES) 
		{
		  XSetWindowBorder(tk_display->display,button->window,widptr->colors->focus);
		  ptr=(long)(button->window);
		  XChangeProperty(tk_display->display,button->top_level,tk_display->atoms._IMAN_WM_FOCUS,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
		}
		break;


	case FocusOut:

		button->hasFocus=NO;
		if((event.xfocus.detail==NotifyAncestor||event.xfocus.detail==NotifyInferior||event.xfocus.detail==NotifyNonlinear)&&button->neverFocus!=YES)
		  XSetWindowBorder(tk_display->display,button->window,widptr->colors->nofocus);

		break;


	default : break;


    }
 
}




int bn_ThumbButtonEvents(tk_display,buttonid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ButtonID buttonid;
{
 unsigned char cp_int[4];
 unsigned long ptr;
 XEvent send_event,eventbis,event;
 KeySym key;
 int index;
 ButtonStruct *button;
 ComboStruct *combo;
 WidgetStruct *widptr;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 widptr=&tk_display->widgets[buttonid];

 event=tk_event->event;

 switch(event.type){

	case ButtonPress:

	      if((button->state&Frozen)!=Frozen)
	      {
		if(button->hasFocus==NO) XRaiseWindow(tk_display->display,button->top_level);

		if(event.xbutton.button==Button1)
		{
		 button->continuity=YES;

		 if(tk_display->action.type==ComboSelectingAction) 
		 {
		    combo=tk_display->widgets[tk_display->action.combo].combo; 	
		    if(button->parency_class<=0||WID_IsSubwidget(tk_display,buttonid,tk_display->action.combo)==False)
		    {
		   	combo->isOpen=False;
		   	XSetInputFocus(tk_display->display,tk_display->widgets[combo->edit].edit->window,RevertToPointerRoot,CurrentTime); 
		   	LS_Unmap(tk_display,combo->list);
		   	memset(&tk_display->action,0,sizeof(ActionStruct));
		   	tk_display->action.combo=tk_display->action.menu=-1;
		    }		    
		  }

		 if((button->state&Grayed)!=Grayed)
		   return BN_PUSHED;
		 else if((button->state&Grayed)==Grayed)
		    return JUSTFOCUS;
		}
		return JUSTFOCUS;
	      }
	      else{
		XRaiseWindow(tk_display->display,button->top_level); 
 		return 0;
		}
	      break;


	case ButtonRelease:

		if(button->continuity==YES && event.xbutton.button==Button1)
		{
		  button->continuity=NO;
		  if((button->state&Grayed)!=Grayed)
		    return BN_RELEASED;
	 	  else return 0;
		}
		return 0;
		break;


	case MotionNotify:

		if(button->continuity==YES)
		{

  		  send_event.xmotion.time=event.xmotion.time;
		  send_event.xmotion.x=event.xmotion.x;
		  send_event.xmotion.y=event.xmotion.y;
		  send_event.xmotion.state=event.xmotion.state;

		  while(XCheckWindowEvent(tk_display->display,button->window,Button1MotionMask,&eventbis)==True)
			send_event=eventbis;
			 	
  		  /*fprintf(stderr,"x: %d  y: %d \n",send_event.xmotion.x,send_event.xmotion.y);*/

		  event.xmotion.time=send_event.xmotion.time;
		  event.xmotion.x=send_event.xmotion.x;
		  event.xmotion.y=send_event.xmotion.y;
		  event.xmotion.state=send_event.xmotion.state;

		  if((button->state&Grayed)!=Grayed)
		  {
		    /*fprintf(stderr,"Bouton deplace\n");*/
		    return BN_MOVED;
		  }
	 	  else return 0;
		}
		return 0;
		break;


	default : return XLIBEVENT;
		break;

 }

}







int bn_KeyFocus(tk_display,window,key,event)
TkDisplay *tk_display;
Window window;
int key;
XEvent *event;
{

 int seek_state;
 unsigned int i,j;
 Window qt_root, qt_parent, *qt_children,new_parent;
 unsigned int qt_numchildren;
 ButtonStruct *qt_button;
 XWindowAttributes qt_attrib;
 int number;



 XQueryTree(tk_display->display, window,&qt_root,&qt_parent,&qt_children,&qt_numchildren);
 
 i=seek_state=0;
 for(i=0;i<qt_numchildren;i++)
 if(seek_state==0)
 {
   switch(WID_GetWindowType(tk_display,qt_children[i]))
   {
    default:
    case 0 : if(bn_KeyFocus(tk_display,qt_children[i],key,event)==2) seek_state=2;
	     break;
    case WI_BUTTON:

   	     XGetWindowAttributes(tk_display->display,qt_children[i],&qt_attrib);
	     number=WID_GetWindowNumber(tk_display,qt_children[i]);
	     qt_button=(ButtonStruct *) tk_display->widgets[number].button;
	     /*fprintf(stderr,"Fenetre state:%d  text:%s\n",qt_button->state,qt_button->text);*/
	     if(qt_attrib.map_state==IsViewable && (qt_button->state&Grayed)!=Grayed && (qt_button->state&Blocked)!=Blocked && qt_button->neverFocus==False && qt_button->txt_key>0 && (key==qt_button->text[qt_button->txt_key-1]|| key==qt_button->text[qt_button->txt_key-1]-32|| key==qt_button->text[qt_button->txt_key-1]+32))
	     {  XSetInputFocus(tk_display->display,qt_button->window,RevertToPointerRoot,CurrentTime);
		/*XWindowEvent(tk_display->display,event->xkey.window,KeyReleaseMask,event);
	        event->xkey.window==qt_button->window;
		event->xkey.keycode=XKeysymToKeycode(tk_display->display,key); 
		event->type=KeyPress;
		event->xkey.send_event=True;
		XSendEvent(tk_display->display,qt_button->window,False,KeyPressMask,event);
		event->type=KeyRelease;
		XSendEvent(tk_display->display,qt_button->window,False,KeyReleaseMask,event);
		*/
		if(qt_numchildren>0) XFree(qt_children);
	        return 2;
		}
	     break;
   }

  }


 if(qt_numchildren>0) XFree(qt_children);
 return seek_state;

}







Bool bn_CheckRepeatPush(display,event,arg)
Display *display;
XEvent *event;
char *arg;
{

 if(event->type==KeyPress) return True;
 else return False;
}


