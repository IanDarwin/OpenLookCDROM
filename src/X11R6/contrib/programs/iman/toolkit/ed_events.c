/*
 *
 * 	ed_events.c  
 * 	evenements des zones d'edition
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

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"
#include "X11/Xatom.h"
#include "X11/keysym.h"

#include <X11/iman/widgets.h>






/*
 *
 * Gere les evenements des
 * zones d'edition
 *
 *
 */

int ed_EditEvents(tk_display,editid,event)
TkDisplay *tk_display;
EditID editid;
XEvent *event;
{
 int x1, x2, mask, index;
 Window if_win;
 int if_revert;
 XEvent eventbis,send_event;
 ListStruct *list;
 ComboStruct *combo;
 ButtonStruct *button;
 KeySym key;
 unsigned long ptr, width;
 int i,ret,oldp,olde; 
 EditStruct *edit;
 WidgetStruct *widptr;
 char str[2];


 str[0]=0;
 str[1]=0;
 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;
 widptr=&tk_display->widgets[editid];



 switch(event->type){

	case ButtonPress:
		
	      if((edit->state&Frozen)!=Frozen)
	      {		
		if(edit->hasFocus==NO)
		{
		 if((edit->state&Grayed)!=Grayed) 
		 { 
		   edit->hasFocus=True;
		   edit->continuity=1;
		   ptr=(long)(edit->window);
	  	   XChangeProperty(tk_display->display,edit->top_level,tk_display->atoms._IMAN_WM_FOCUS,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
		   XSetInputFocus(tk_display->display,edit->window,RevertToPointerRoot,event->xbutton.time);
		   _ED_DrawFocus(tk_display,editid,ON);
		 }
		 XMapRaised(tk_display->display,edit->top_level);
	        }

		if((edit->state&Grayed)!=Grayed&&edit->neverFocus==False) 
		{
		  oldp=edit->position;
		  ret=0;
		  if(event->xbutton.x<=edit->txt_x){ret=1; edit->position=0;}
		  if(ret==0) for(i=0;i<edit->length;i++)
		  {
			if(i>0&&edit->length>0)
			  width=XTextWidth(edit->font,edit->text,i);
			else width=0;
			if(event->xbutton.x>=width+edit->txt_x&&event->xbutton.x<=XTextWidth(edit->font,edit->text,i+1)+edit->txt_x)
			{
				edit->position=i;
				ret=1;
			}
		  }
		  if(ret==0) edit->position=edit->length;
		  ed_DrawLetter(tk_display,editid,oldp);
		

		  if((event->xbutton.button)==Button1)
		  {
			if(edit->sel1!=edit->sel2) 
			{
				oldp=edit->sel1;
				olde=edit->sel2;
				edit->sel1=edit->sel2=edit->position;
				ed_DrawText(tk_display,editid,oldp,olde);
				ed_DrawCursor(tk_display,editid);
			      }

			else{
				edit->sel1=edit->sel2=edit->position;
				ed_DrawLetter(tk_display,editid,edit->position);
				ed_DrawCursor(tk_display,editid);
			      }
			
		   }
		
		   edit->continuity=True;
		   edit->repeat=1;
		   return JUSTFOCUS;
		}
		return JUSTFOCUS;
	      }
	      else{
		XMapRaised(tk_display->display,edit->top_level); 
 		return 0;
		}
	      break;
		

	case MotionNotify:
			if((event->xmotion.state&Button1Mask)==Button1Mask && (edit->state&Grayed)!=Grayed&&edit->neverFocus==False&&edit->continuity==True)
			{
			   ret=0;
			   
			   send_event.xmotion.time=event->xmotion.time;
		 	   send_event.xmotion.x=event->xmotion.x;
		 	   send_event.xmotion.y=event->xmotion.y;
		 	   send_event.xmotion.state=event->xmotion.state;

		 	   while(XCheckWindowEvent(tk_display->display,edit->window,Button1MotionMask,&eventbis)==True)
				send_event=eventbis;

			   event->xmotion.time=send_event.xmotion.time;
			   event->xmotion.x=send_event.xmotion.x;
		 	   event->xmotion.y=send_event.xmotion.y;
		 	   event->xmotion.state=send_event.xmotion.state;


			   if(event->xmotion.x<=edit->txt_x){ret=1; x2=0;}
			   if(ret==0) for(i=0;i<edit->length;i++) 	
			   {
			     if(i>0&&edit->length>0)
			       width=XTextWidth(edit->font,edit->text,i);
			     else width=0;
			     if(event->xmotion.x>=width+edit->txt_x&&event->xmotion.x<=XTextWidth(edit->font,edit->text,i+1)+edit->txt_x)
			     {
				x2=i;
				ret=1;
			     }
			   }
			   if(ret==0) x2=edit->length;	


			   if(x2>edit->sel2 && x2>edit->sel1)
			   {
			     if(edit->position!=edit->length)
			     {
				oldp=edit->sel1;
				olde=edit->sel2;
				
				if(edit->repeat==2) edit->sel1=edit->sel2;
				

				if(x2<edit->length )
				{ 
				  edit->position=edit->sel2=x2;
				  if(edit->repeat==1) ed_DrawText(tk_display,editid,olde,edit->sel2);
				  else ed_DrawText(tk_display,editid,oldp,edit->sel2);
				}

				else if(x2>=edit->length)
				{
				  edit->sel2=edit->position=edit->length;
				  if(edit->repeat==1) ed_DrawText(tk_display,editid,olde,edit->sel2);
				  else ed_DrawText(tk_display,editid,oldp,edit->sel2);
				}
				edit->repeat=1;
			      }
			   }

			   else if(x2!=edit->sel2 && x2>edit->sel1 && edit->repeat<2)
			   {
				oldp=edit->sel1;
				olde=edit->sel2;

				if(x2<=edit->length ){ 
				edit->position=edit->sel2=x2;
				if(x2<olde) ed_DrawText(tk_display,editid,x2,olde);
				else ed_DrawText(tk_display,editid,olde,x2);
				}
				else if(x2>=edit->length && edit->sel2<edit->length){
				edit->sel2=edit->position=edit->length;
				ed_DrawText(tk_display,editid,olde,edit->length);
				}
				edit->repeat=1;
			    }

		           else if(x2!=edit->sel2 && x2>edit->sel1 && edit->repeat>=2)
			   {
				edit->repeat=2;
				oldp=edit->sel1;
				olde=edit->sel2;

				if(x2<edit->length)
				{
				  edit->position=edit->sel1=x2;
/*** MODIFIE ***/
				  ed_DrawText(tk_display,editid,oldp,edit->sel1);
				}
				else 
				{
				  edit->sel1=edit->sel2=edit->position=edit->length;
				  ed_DrawText(tk_display,editid,edit->sel1,edit->sel2);
				}
			      
			    }

			  else if((x2==edit->sel1&&edit->repeat==1)|| (edit->repeat==2 && x2==edit->sel2)) 
			  {

				oldp=edit->sel1;
				olde=edit->sel2;
				edit->position=edit->sel2=edit->sel1=x2;
				if(edit->repeat==2)
				  ed_DrawText(tk_display,editid,oldp,edit->position);
				else if(edit->repeat==1)
				  ed_DrawText(tk_display,editid,edit->position,olde);

				edit->repeat=0;
			    }

			   else if(x2<edit->sel2 && x2<edit->sel1 && edit->position>0)
				{
				oldp=edit->sel1;
				olde=edit->sel2;
				if(edit->repeat!=2) edit->sel2=edit->sel1; 

				if(x2>0){
				edit->position=edit->sel1=x2;
				if(edit->repeat<2) ed_DrawText(tk_display,editid,edit->sel1,olde);
				else ed_DrawText(tk_display,editid,edit->sel1,oldp);
				}
				else {
				edit->sel1=edit->position=0;
				if(edit->repeat!=2) ed_DrawText(tk_display,editid,0,olde);
				else if(oldp!=0) ed_DrawText(tk_display,editid,0,oldp);
				}
				edit->repeat=2;

			     }
			   /*(void)fprintf(stderr,"P1:%s  P2:%d\n",edit->sel1,edit->sel2);*/
			   }
  			   return 0;


	case ButtonRelease: edit->repeat=0;
			    edit->continuity=False;
			    return 0;
			    break;



	case KeyRelease: index=0;
			if((event->xkey.state&1)==1) index=ShiftMapIndex+1;
			else if((event->xkey.state&2)==2) index=LockMapIndex+1;
			else if((event->xkey.state&4)==4) index=ControlMapIndex+1;
			else if((event->xkey.state&8)==8) index=Mod1MapIndex+1;
			else if((event->xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
			else if((event->xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
			else if((event->xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
			else if((event->xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

			key=XLookupKeysym(&event->xkey,index);
			


			if(edit->sel1==edit->sel2) 
			{
			  switch(key){

			  case XK_Up:
			  case XK_Down: return ED_KEYUNKNOWN;
					break;


			  case XK_Insert: 
					edit->mode=INSERT; 
 					return 0;
					break;

			  case XK_Left: if(edit->position>0&&edit->neverFocus==False){
					edit->position--;
					ed_DrawLetter(tk_display,editid,edit->position+1); 
					/*ed_DrawLetter(tk_display,editid,edit->position);*/ 
					ed_DrawCursor(tk_display,editid);
					}
					return 0;
					break;

			  case XK_Right: 
					if(edit->position<strlen(edit->text)&&edit->neverFocus==False){ 
 					edit->position++;
					/*ed_DrawLetter(tk_display,editid,edit->position);*/
					ed_DrawLetter(tk_display,editid,edit->position-1);
					ed_DrawCursor(tk_display,editid);
					}
					return 0;
					break;

			  case XK_Home:	if(edit->neverFocus==False){
					oldp=edit->position;
					edit->position=0;
					ed_DrawLetter(tk_display,editid,oldp); 
					ed_DrawCursor(tk_display,editid);
					}
					return 0;
					break;

			  case XK_End:  if(edit->neverFocus==False){
					oldp=edit->position;
					edit->position=edit->length; 
					ed_DrawLetter(tk_display,editid,oldp);
					ed_DrawCursor(tk_display,editid);
					}
					return 0;
					break;
			
			  case XK_Delete:
					if(edit->position<strlen(edit->text)&&edit->neverFocus==False) 
					{
					 oldp=edit->position;
					 edit->length--;
					 if(oldp!=edit->length) ed_MoveText(tk_display,editid,oldp+1,edit->position);

					 for(x1=edit->position;x1<edit->length;x1++)
					   edit->text[x1]=edit->text[x1+1];
					 edit->text[edit->length]=0; 
					 if(oldp==edit->length){
					   ed_DrawLetter(tk_display,editid,oldp);}
					   ed_DrawCursor(tk_display,editid);
					}
					return 0;
					break;

			  case XK_Return: return ED_VALIDATION;
					break;
		
			  case XK_Tab:	if(edit->parency_class<=0){
					if(index==0) WID_SeekNextFocus(tk_display,edit->window,edit->parent,edit->top_level,UP+DOWN+INCRUST);
					if(index==1) WID_SeekPreviousFocus(tk_display,edit->window,edit->parent,edit->top_level,UP+DOWN+INCRUST);
					}
					else return ED_KEYUNKNOWN;
					break;


			  case XK_BackSpace :
					if(edit->position>0 && edit->length>0 && edit->neverFocus==False)
					{
					oldp=edit->position;
					edit->position--;
					edit->length--;
					if(edit->position!=edit->length) 
					  ed_MoveText(tk_display,editid,oldp,edit->position);
					for(x1=edit->position;x1<edit->length;x1++)
					  edit->text[x1]=edit->text[x1+1];
					edit->text[edit->length]=0; 
					
					if(edit->position==edit->length){
					 ed_DrawLetter(tk_display,editid,oldp);
					 ed_DrawLetter(tk_display,editid,edit->position);
					 ed_DrawCursor(tk_display,editid);}
					}
					return 0;
					break;


			default:if((key>=32 && key<=254) && key!=127 && edit->neverFocus==False) 
				{
				  if(edit->length>0)
			  	    width=XTextWidth(edit->font,edit->text,edit->length);
				  else width=0;
				  str[0]=(char)key;

				  if(edit->mode==DESTROY && edit->length<edit->maxlength)
				  {
					edit->text[edit->position]=(unsigned char)key;
					edit->position++;
					if(edit->position>edit->length ) edit->length++;
					edit->text[edit->length]=0;
					
					ed_DrawEdit(tk_display,editid);
					return 0;
				   }
				   else if(edit->mode==INSERT && XTextWidth(edit->font,str,1)+width+edit->txt_x<edit->width-edit->txt_x&& (event->xkey.state&ControlMask)!=ControlMask)
				   {	
					oldp=edit->position;
					
					if(edit->length>0&&edit->position<edit->length) for(x1=edit->length-1;x1>edit->position;x1--)
					  edit->text[x1+1]=edit->text[x1]; 
					edit->text[edit->position+1]=edit->text[edit->position];
					edit->length++;
					edit->text[edit->position]=(unsigned char)key;
					edit->position++;
					edit->text[edit->length]=0;
					
					if(edit->position!=edit->length){ 
					  ed_MoveText(tk_display,editid,oldp,edit->position);
					  ed_DrawLetter(tk_display,editid,oldp);
					 }
					else if(edit->position==edit->length){
					 ed_DrawLetter(tk_display,editid,oldp);
					 ed_DrawLetter(tk_display,editid,oldp+1);
					 ed_DrawCursor(tk_display,editid);	}
					return 0;
					
				 }
				 else if(edit->mode==INSERT && (event->xkey.state&ControlMask)==ControlMask/*&&(key==XK_P || key==XK_p)*/)
				 {
					fprintf(stderr,"Requete pur COLLER\n");
					
				 }
				}
				else {  if(event->xkey.keycode!=52&&event->xkey.keycode!=66&&event->xkey.keycode!=68&&event->xkey.keycode!=38&&event->xkey.keycode!=65)
					 if(event->xkey.keycode!=70&&event->xkey.keycode!=72&&event->xkey.keycode!=98)
					   if(key<32||key>254) return ED_KEYUNKNOWN;
					else return 0;
					}
				break;

			    }
			}

			else if(edit->sel1!=edit->sel2) 
			{
			  switch(key){


			  case XK_Up:
			  case XK_Down: return ED_KEYUNKNOWN;
				        /*{
					combo=(ComboStruct *)(tk_display->widgets[edit->parency_number].combo);
					if(combo->isOpen==False){ 
						combo->isOpen=True;
						LS_Map(tk_display,combo->list);
						}
				        }*/
				       break;

			  case XK_Insert: 
					 edit->mode=INSERT;
					 return 0;
					break;

			  case XK_Left: if(edit->mode==INSERT&&edit->neverFocus==False){
					oldp=edit->sel2;
					if(edit->sel1>0) edit->sel1--;
					edit->position=edit->sel2=edit->sel1;
					ed_DrawText(tk_display,editid,edit->position,oldp); 
					}
					return 0;
					break;

			  case XK_Right: 
					if(edit->mode==INSERT && edit->sel2<=edit->length&&edit->neverFocus==False){ 
					oldp=edit->sel1;
					if(edit->sel2<edit->length) edit->sel2++;
 					edit->position=edit->sel1=edit->sel2;
					ed_DrawText(tk_display,editid,oldp,edit->position);
					}
					return 0;
					break;

			  case XK_Home:	if(edit->mode==INSERT&&edit->neverFocus==False){
					oldp=edit->sel1;
					olde=edit->sel2;
					edit->sel1=edit->sel2=edit->position=0;
					ed_DrawText(tk_display,editid,oldp,olde); 
					ed_DrawCursor(tk_display,editid);
					}
					return 0;
					break;

			  case XK_End:  if(edit->mode==INSERT&&edit->neverFocus==False){
					oldp=edit->sel1;
					oldp=edit->sel2;
					edit->sel1=edit->sel2=edit->position=edit->length; 
					ed_DrawText(tk_display,editid,oldp,olde);
					ed_DrawCursor(tk_display,editid);
					}
					return 0;
					break;

			  case XK_BackSpace :
			  case XK_Delete:if(edit->mode==INSERT&&edit->neverFocus==False) 
					 {
					 
					 x2=edit->sel2;
					 x1=edit->sel1;
					 ed_MoveText(tk_display,editid,x2,x1);
					 while(x2<edit->length)
					   { edit->text[x1]=edit->text[x2]; 
					     x1++;
					     x2++; }
					 edit->length=edit->length-edit->sel2+edit->sel1;
					 edit->text[edit->length]=0; 
					 edit->position=edit->sel2=edit->sel1;
					 ed_DrawCursor(tk_display,editid);
					 }
					 return 0;
					 break;

			  case XK_Return: return ED_VALIDATION;

			  case XK_Tab:	if(edit->parency_class<=0){
					if(index==0) WID_SeekNextFocus(tk_display,edit->window,edit->parent,edit->top_level,UP+DOWN+INCRUST);
					if(index==1) WID_SeekPreviousFocus(tk_display,edit->window,edit->parent,edit->top_level,UP+DOWN+INCRUST);
					}
					else return ED_KEYUNKNOWN;
					break;



			  default:if((key>=32 && key<=254) && key!=127&&edit->neverFocus==False) 
				{
				 if(edit->mode==INSERT&&(event->xkey.state&ControlMask)!=ControlMask) 
					 {
					 
					 x2=edit->sel2;
					 x1=edit->sel1;
					 ed_MoveText(tk_display,editid,x2,x1+1);
					 while(x2<edit->length)
					   { edit->text[x1]=edit->text[x2]; 
					     x1++;
					     x2++; }
					 edit->length=edit->length-edit->sel2+edit->sel1;
					 edit->text[edit->length]=0; 
					 edit->position=edit->sel2=edit->sel1;

					 if(edit->length>0) for(x1=edit->length-1;x1>edit->position;x1--)
					  edit->text[x1+1]=edit->text[x1]; 
					 edit->text[edit->position+1]=edit->text[edit->position];
					 edit->length++;
					 edit->text[edit->position]=(unsigned char)key;
					 edit->position++;
					 edit->sel1=edit->sel2=edit->position;
					 edit->text[edit->length]=0;
					 if(edit->position>0) ed_DrawLetter(tk_display,editid,edit->position-1);
					 ed_DrawLetter(tk_display,editid,edit->position);
					 ed_DrawCursor(tk_display,editid);
					 return 0;
				   }
				 else if(edit->mode==INSERT && (event->xkey.state&ControlMask)==ControlMask)
				 {
				   if(key==XK_P || key==XK_p)
					fprintf(stderr,"Requete pour COLLER\n");
				   else if(key==XK_c || key==XK_C)
					fprintf(stderr,"Requete pour COPIER\n");
				   else if(key==XK_x || key==XK_X)
					fprintf(stderr,"Requete pour COUPER\n");
				   else fprintf(stderr,"EDIT inconnue\n");
				 }
				 return 0;
				}
				else {  if(event->xkey.keycode!=52&&event->xkey.keycode!=66&&event->xkey.keycode!=68&&event->xkey.keycode!=38&&event->xkey.keycode!=65)
					 if(event->xkey.keycode!=70&&event->xkey.keycode!=72&&event->xkey.keycode!=98)
					   if(key<32||key>254) return ED_KEYUNKNOWN;
					else return 0;
					}
				
				break;

			    }	
			}
			return 0;
			break;

	case FocusIn: 
			if(event->xfocus.detail==NotifyAncestor||event->xfocus.detail==NotifyInferior||event->xfocus.detail==NotifyNonlinear)
			{
			  ptr=(long)(edit->window);
			  XChangeProperty(tk_display->display,edit->top_level,tk_display->atoms._IMAN_WM_FOCUS,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
			  edit->hasFocus=True;
			  XAutoRepeatOn(tk_display->display);
			  _ED_DrawFocus(tk_display,editid,ON);

			  if(edit->type!=ED_NORMALEDIT&&edit->neverFocus==True)
			  {
				edit->position=edit->length;
				edit->sel1=0;
				edit->sel2=edit->length;
			  }
			  else edit->sel2=edit->sel1=edit->position;		      
			  ed_DrawEdit(tk_display,editid);
			}
			return 0;
			break;


	case FocusOut: 
			if(event->xfocus.detail==NotifyAncestor||event->xfocus.detail==NotifyInferior||event->xfocus.detail==NotifyNonlinear)
			{
			edit->hasFocus=False;
			ret=0;
			
			if(edit->parency_class==WI_COMBO)
			{
			  combo=(ComboStruct *)(tk_display->widgets[edit->parency_number].combo);
			  XGetInputFocus(tk_display->display,&if_win,&if_revert);
			  list=tk_display->widgets[combo->list].list;
			  if(list->listwindow!=if_win)
			  {
				_ED_DrawFocus(tk_display,editid,OFF);				
				combo->hasFocus=False;
				combo->isOpen=False;
				ret=XLIBEVENT;
				
			  }
			  else ret=0;
			}
			_ED_DrawFocus(tk_display,editid,OFF);	

			if(edit->type!=ED_NORMALEDIT)
			{
			  edit->sel1=edit->length;
			  edit->sel2=edit->length;
			  edit->position=edit->length;
			}
			else edit->sel2=edit->sel1=edit->position;

			ed_DrawEdit(tk_display,editid);
			return ret;
			}
			return 0;
			break;


 }
}






