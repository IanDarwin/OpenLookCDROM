/*
 *
 * 	mn_events.c  
 * 	evenements des menus
 *
 * 	Modification :  21/12/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify,distribute and sell this software and its
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

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"
#include "X11/Xatom.h"
#include "X11/keysym.h"

#include <X11/iman/widgets.h>






long mn_GetRootMenu(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 MenuStruct *mn_ptr, *menu;
 

 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


START_GET_PARENT_MENUS:

 if(menu->parency_class==WI_MENU && menu->prevmenu>=0)
 {
   mn_ptr=tk_display->widgets[menu->prevmenu].menu;
   if(mn_ptr->parency_class==WI_MENU && mn_ptr->prevmenu>=0)
   {
     menu=mn_ptr;
     goto START_GET_PARENT_MENUS;
   }
   else return (long)mn_ptr;
 }
 return (long)menu;

}





/*
 *
 * Gestion des evenements d'un menu deroulant
 *
 *
 */

int mn_FloatingMenuEvents(tk_display,menuid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
MenuID menuid;
{
 int mask, index;
 XGCValues xgcvalues;
 GC gc;
 KeySym key;
 unsigned long ptr;


 int i,j,ret; 
 int xp, yp, xr, yr;
 Window w, r;
 unsigned int xk;
 int clickpos,old,olds;
 XEvent send_event, eventbis;
 MenuStruct *mn_ptr, *menu, *prevmenu;
 ComboStruct *combo;
 WidgetStruct *widptr;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;
 widptr=&tk_display->widgets[menuid];
 if(menu->parency_class==WI_MENU) prevmenu=tk_display->widgets[menu->prevmenu].menu;
 else prevmenu=0;

 switch(tk_event->ev_type){



	case XLIBEVENT: switch(tk_event->event.type){


		case EnterNotify:
			if((tk_event->event.xcrossing.state&Button1Mask)==Button1Mask && (tk_event->event.xcrossing.detail==NotifyAncestor||tk_event->event.xcrossing.detail==NotifyInferior||tk_event->event.xcrossing.detail==NotifyNonlinear))
			if((menu->parency_class==WI_MENU&&prevmenu->items[prevmenu->selecteditem].type==MN_SUBMENU&&menuid==prevmenu->items[prevmenu->selecteditem].submenu)||(menu->parency_class==0&&menu->hasFocus==True))
			{

			  if(menu->hasFocus==NO)
			  {
			   if((menu->state&Grayed)!=Grayed) 
			   { 
			     menu->hasFocus=True;
			     /*XSetInputFocus(tk_display->display,menu->window,RevertToPointerRoot,tk_event->event.xcrossing.time);*/
			     XSetWindowBorder(tk_display->display,menu->window,widptr->colors->focus);
			     menu->selected=True;

			    }
			  }
			  else{ /*XSetInputFocus(tk_display->display,menu->window,RevertToPointerRoot,tk_event->event.xcrossing.time);*/
				XSetWindowBorder(tk_display->display,menu->window,widptr->colors->focus);
			      }


			  if((menu->state&Grayed)!=Grayed && (tk_event->event.xcrossing.state&Button1Mask)==Button1Mask&&menu->numitems>0)
			  {
			   menu->continuity=YES;

			   clickpos=-1;
			   clickpos=mn_GetSelectedItem(tk_display,menuid,tk_event->event.xcrossing.x,tk_event->event.xcrossing.y);

			   olds=menu->selecteditem;
			   menu->selecteditem=clickpos;

			   if(olds==clickpos && clickpos>=0 && clickpos<menu->numitems)
			   {
				
		   		/*mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem);*/
				if(menu->items[clickpos].type==MN_SUBMENU&&menu->items[clickpos].submenu>=0)
				{
				  mn_CloseSubmenus(tk_display,menuid,menu->items[clickpos].submenu);
				  mn_ptr=tk_display->widgets[menu->items[clickpos].submenu].menu;
			    	  mn_ptr->hasFocus=False;
			  	  mn_ptr->selected=NO;	
				  mn_ptr->continuity=False;
				  mn_DrawFloatingItem(tk_display,mn_ptr->wid_number,mn_ptr->selecteditem);
				  mn_ptr->selecteditem=0;
			  	  XSetWindowBorder(tk_display->display,mn_ptr->window,widptr->colors->text);
			  	  mn_CloseSubmenus(tk_display,mn_ptr->wid_number,-1);
				}
				else mn_CloseSubmenus(tk_display,menuid,-1);
			   }
			   else if(olds!=clickpos && menu->numitems>0) 
			   {
				mn_DrawFloatingItem(tk_display,menuid,olds);
				mn_CloseSubmenus(tk_display,menuid,-1);
							        
			   }
			 
			   menu->selected=True;			
			   mn_DrawFloatingItem(tk_display,menuid,clickpos);
			   /*XSetInputFocus(tk_display->display,menu->window,RevertToPointerRoot,tk_event->event.xcrossing.time);*/

			   /*XSync(tk_display->display,False);
			   fprintf(stderr,"Boucle en fin\n");*/

			   
			   if(menu->items[clickpos].type==MN_SUBMENU&&(menu->items[clickpos].state&Grayed)!=Grayed)
			   {
			     j=MN_UMARGE;
			     for(i=0;i<clickpos;i++)
			       j=j+menu->items[i].height;
			     ret=j;
			     XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->width-15,ret-3,&xr,&yr,&w);
			     mn_Map(tk_display,menu->items[clickpos].submenu,xr,yr);
			   }

			  }
			  /*fprintf(stderr,"Float: EnterNotify FIN %ld\n",tk_event->event.xcrossing.window);*/
			  return 0;
			}
			else if((tk_event->event.xcrossing.state&Button1Mask)!=Button1Mask && (tk_event->event.xcrossing.detail==NotifyAncestor||tk_event->event.xcrossing.detail==NotifyInferior||tk_event->event.xcrossing.detail==NotifyNonlinear))
			{
				menu->continuity=False;
			}
			break;


		case LeaveNotify:
			/*if(tk_event->event.xcrossing.detail==NotifyAncestor||tk_event->event.xcrossing.detail==NotifyInferior||tk_event->event.xcrossing.detail==NotifyNonlinear)
			  fprintf(stderr,"Float LeaveNotify %ld %d\n",tk_event->event.xcrossing.window,tk_event->event.xcrossing.detail);*/
			break;


		case ButtonPress:

			if(menu->hasFocus==NO)
			{
			 if((menu->state&Grayed)!=Grayed&&tk_event->event.xbutton.button==Button1) 
			 { 
			  menu->hasFocus=True;
			  menu->selected=True;
			  XSetWindowBorder(tk_display->display,menu->window,widptr->colors->focus);
			 }
			 XMapRaised(tk_display->display,menu->top_level);
			 XMapRaised(tk_display->display,menu->window);
			 XFlush(tk_display->display);
			 XSync(tk_display->display,False);
			}
			else{
			  XMapRaised(tk_display->display,menu->top_level);
			  XMapRaised(tk_display->display,menu->window);
			  XSetWindowBorder(tk_display->display,menu->window,widptr->colors->focus);
			  XFlush(tk_display->display);
			  XSync(tk_display->display,False);
			}

			if((menu->state&Grayed)!=Grayed && tk_event->event.xbutton.button==Button1&&menu->numitems>0)
			{
			 if(tk_display->action.type==ComboSelectingAction) 
			 {
			   
			   combo=tk_display->widgets[tk_display->action.combo].combo; 	
			   combo->isOpen=False;
			   XSetInputFocus(tk_display->display,tk_display->widgets[combo->edit].edit->window,RevertToPointerRoot,CurrentTime); 
			   LS_Unmap(tk_display,combo->list);
			   memset(&tk_display->action,0,sizeof(ActionStruct));
			   tk_display->action.combo=tk_display->action.menu=-1;
			 }  
			 

			 menu->continuity=YES;
			 clickpos=-1;
			 clickpos=mn_GetSelectedItem(tk_display,menuid,tk_event->event.xbutton.x,tk_event->event.xbutton.y);
			 
			 olds=menu->selecteditem;
			 menu->selecteditem=clickpos;

			 if(olds==clickpos && clickpos>=0 && clickpos<menu->numitems)
			 {
				
		   		mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem);
				if(menu->items[clickpos].type==MN_SUBMENU&&menu->items[clickpos].submenu>=0)
				{
				  mn_CloseSubmenus(tk_display,menuid,menu->items[clickpos].submenu);
				  mn_ptr=tk_display->widgets[menu->items[clickpos].submenu].menu;
			    	  mn_ptr->hasFocus=False;
			  	  mn_ptr->selected=NO;	
			  	  mn_DrawFloatingItem(tk_display,mn_ptr->wid_number,mn_ptr->selecteditem);
				  mn_ptr->selecteditem=0;
				  mn_ptr->continuity=False;
			  	  XSetWindowBorder(tk_display->display,mn_ptr->window,widptr->colors->text);
			  	  mn_CloseSubmenus(tk_display,mn_ptr->wid_number,-1);
				}
				else mn_CloseSubmenus(tk_display,menuid,-1);
			 }
			 else if(olds!=clickpos && menu->numitems>0) 
			 {
				mn_DrawFloatingItem(tk_display,menuid,olds);
				mn_CloseSubmenus(tk_display,menuid,-1);
				XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->width-15,ret-3,&xr,&yr,&w);
				if(menu->items[clickpos].type==MN_SUBMENU&&(menu->items[clickpos].state&Grayed)!=Grayed)
			 	  mn_Map(tk_display,menu->items[clickpos].submenu,xr,yr);
			 }
			 menu->selected=True;
			 mn_DrawFloatingItem(tk_display,menuid,clickpos);
			 if(menu->parency_class==0)
			 {
				tk_display->action.type=MenuSelectingAction; 
				tk_display->action.menu=menuid;
			 }
			}
			return 0;
			break;



		case ButtonRelease:

			/*fprintf(stderr,"Floating button release %d\n",menu->selecteditem);*/
			if(menu->selecteditem>=0&&menu->items[menu->selecteditem].type==MN_SUBMENU&&tk_event->event.xbutton.button==Button1)
			{
			  menu->continuity=NO;
			  clickpos=menu->selecteditem;

			  mn_ptr=(MenuStruct *)mn_GetRootMenu(tk_display,menuid);
			  if(mn_ptr!=(MenuStruct *)NULL)
			  {
			  	mn_ptr->hasFocus=False;
			  	mn_ptr->selected=NO;	
			  	mn_ptr->continuity=False;
				if(mn_ptr->selecteditem>=0&&mn_ptr->selecteditem<mn_ptr->numitems) 
			  	  mn_DrawMenuBarItem(tk_display,mn_ptr->wid_number,mn_ptr->selecteditem);
			  	mn_CloseSubmenus(tk_display,mn_ptr->wid_number,-1);
			  	XSetWindowBorder(tk_display->display,mn_ptr->window,widptr->colors->text);
			  	mn_ptr->selecteditem=mn_ptr->oldselecteditem=mn_ptr->repeat=-1;
			  }
			  menu->hasFocus=False;
			  menu->selected=NO;	
			  menu->continuity=False;
			  mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem);
			  mn_CloseMenu(tk_display,menuid);
			  menu->selecteditem=menu->oldselecteditem=menu->repeat=-1;
			  memset(&tk_display->action,0,sizeof(ActionStruct));
			  tk_display->action.combo=tk_display->action.menu=-1;
			  return NO_EVENT;
			 	
			}
			else if(menu->selecteditem>=0&&menu->items[menu->selecteditem].type!=MN_SUBMENU&&(menu->items[menu->selecteditem].state&Grayed)!=Grayed&&(menu->state&Grayed)!=Grayed&&tk_event->event.xbutton.x>=0&&tk_event->event.xbutton.x<=menu->width&&tk_event->event.xbutton.y>=0&&tk_event->event.xbutton.y<=menu->height&&tk_event->event.xbutton.button==Button1)
			{
			  clickpos=menu->selecteditem;
			  menu->continuity=NO;
			  clickpos=menu->oldselecteditem=menu->repeat=menu->selecteditem;
			  if(menu->items[menu->selecteditem].type==MN_SUBMENU) 
				mn_CloseMenu(tk_display,menu->items[menu->selecteditem].submenu);
			  mn_CloseMenu(tk_display,menuid);
			  menu->hasFocus=False;
			  mn_ptr=(MenuStruct *)mn_GetRootMenu(tk_display,menuid);

			  menu->hasFocus=False;
			  menu->selected=NO;	
			  menu->continuity=False;
			  mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem);

			  if(mn_ptr!=(MenuStruct *)NULL)
			  {
			  	mn_ptr->hasFocus=False;
			  	mn_ptr->selected=NO;	
			  	mn_ptr->continuity=False;
			   
			  	if(mn_ptr->selecteditem>=0&&mn_ptr->selecteditem<mn_ptr->numitems) 
			  	  mn_DrawMenuBarItem(tk_display,mn_ptr->wid_number,mn_ptr->selecteditem);
			  	mn_CloseSubmenus(tk_display,mn_ptr->wid_number,-1);
			  	XSetWindowBorder(tk_display->display,mn_ptr->window,widptr->colors->text);
			  	mn_ptr->hasFocus=False;
			  	mn_ptr->selected=NO;	
			  	mn_ptr->oldselecteditem=mn_ptr->selecteditem=mn_ptr->repeat=-1;
			  }

			   menu->repeat=menu->selecteditem=-1;
			  menu->oldselecteditem=clickpos;
			  memset(&tk_display->action,0,sizeof(ActionStruct));
			  tk_display->action.combo=tk_display->action.menu=-1;
			  if(clickpos>=0) return MN_SELECTED; 
			  else return 0;
			}
			else if(tk_event->event.xbutton.button==Button1)
			{
			  menu->continuity=NO;
			  menu->oldselecteditem=menu->repeat=menu->selecteditem;
			  if(menu->items[menu->selecteditem].type==MN_SUBMENU) 
				mn_CloseMenu(tk_display,menu->items[menu->selecteditem].submenu);
			  mn_CloseMenu(tk_display,menuid);
			  menu->hasFocus=False;
			  menu->selected=NO;	
			  menu->continuity=False;
			  mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem);
			  mn_ptr=(MenuStruct *)mn_GetRootMenu(tk_display,menuid);

			  if(mn_ptr!=(MenuStruct *)NULL)
			  {
			  	mn_ptr->hasFocus=False;
			  	mn_ptr->selected=NO;	
			  	mn_ptr->continuity=False;
			  	if(mn_ptr->selecteditem>=0&&mn_ptr->selecteditem<mn_ptr->numitems) 
			  	  mn_DrawMenuBarItem(tk_display,mn_ptr->wid_number,mn_ptr->selecteditem);
			  	mn_CloseSubmenus(tk_display,mn_ptr->wid_number,-1);
			  	XSetWindowBorder(tk_display->display,mn_ptr->window,widptr->colors->text);
			  	mn_ptr->hasFocus=False;
			  	mn_ptr->selecteditem=-1;	
			  }

			  menu->selecteditem=menu->repeat=-1;
			  memset(&tk_display->action,0,sizeof(ActionStruct));
			  tk_display->action.combo=tk_display->action.menu=-1;
			  return NO_EVENT;
			 
			}
			return 0;
			break;


		case MotionNotify:
			
			if(menu->continuity==True && (menu->state&Grayed)!=Grayed && (tk_event->event.xmotion.state&Button1Mask)==Button1Mask)
			if(tk_event->event.xmotion.x>=0 && tk_event->event.xmotion.x<menu->width &&tk_event->event.xmotion.y>=0 && tk_event->event.xmotion.y<menu->height)
			{
			 
			 menu->hasFocus=True;
			 menu->selected=True;
			 j=MN_UMARGE;
			 clickpos=-1;

			 send_event.xmotion.time=tk_event->event.xmotion.time;
		 	 send_event.xmotion.x=tk_event->event.xmotion.x;
		 	 send_event.xmotion.y=tk_event->event.xmotion.y;
		 	 send_event.xmotion.state=tk_event->event.xmotion.state;

		 	 while(XCheckWindowEvent(tk_display->display,menu->window,Button1MotionMask,&eventbis)==True)
				send_event=eventbis;

			 tk_event->event.xmotion.time=send_event.xmotion.time;
			 tk_event->event.xmotion.x=send_event.xmotion.x;
		 	 tk_event->event.xmotion.y=send_event.xmotion.y;
		 	 tk_event->event.xmotion.state=send_event.xmotion.state;
		

			 if(tk_event->event.xmotion.y<MN_UMARGE) clickpos=0;
			 else for(i=0;i<menu->numitems;i++)
			 {
			   if(tk_event->event.xmotion.y>=j&&tk_event->event.xmotion.y+menu->items[i].height>=tk_event->event.xmotion.y&& menu->items[i].type!=MN_HBAR)
			   {
			  	clickpos=i;	
				ret=j;
			   }
			   j=j+menu->items[i].height;
			 }
			 if(clickpos==-1) clickpos=menu->numitems-1;

			 olds=menu->selecteditem;
			 menu->selecteditem=clickpos;

			 if(menu->selected==True && olds!=clickpos){ 
				mn_DrawFloatingItem(tk_display,menuid,olds);
				if(menu->items[olds].type==MN_SUBMENU) mn_CloseMenu(tk_display,menu->items[olds].submenu);
				}
			 menu->selected=True;			
			 if(olds!=clickpos)
				mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem);
				
			 XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->width-15,ret-3,&xr,&yr,&w);
			 if(menu->items[clickpos].type==MN_SUBMENU&&(menu->items[clickpos].state&Grayed)!=Grayed) mn_Map(tk_display,menu->items[clickpos].submenu,xr,yr);
			}
			return 0;
			break;


		case KeyRelease: index=0;

			if((tk_event->event.xkey.state&1)==1) index=ShiftMapIndex+1;
			else if((tk_event->event.xkey.state&2)==2) index=LockMapIndex+1;
			else if((tk_event->event.xkey.state&4)==4) index=ControlMapIndex+1;
			else if((tk_event->event.xkey.state&8)==8) index=Mod1MapIndex+1;
			else if((tk_event->event.xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
			else if((tk_event->event.xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
			else if((tk_event->event.xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
			else if((tk_event->event.xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

			key=XLookupKeysym(&tk_event->event.xkey,index);

			if((tk_event->event.xkey.state&Button1Mask)!=Button1Mask)
			  menu->continuity=False;

			if(menu->continuity==False) switch(key){


			  case XK_Up:   i=menu->selecteditem-1;
					olds=menu->selecteditem;
					while(i>=0 && menu->items[i].type==MN_HBAR)
					  i--;
					
					if(i<0){ 
					  i=menu->numitems-1;
					  while(i>=0 && menu->items[i].type==MN_HBAR)
					    i--;
					  }
					menu->selecteditem=i;

					if(olds!=i){ mn_DrawFloatingItem(tk_display,menuid,olds);
						mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem); 
						if(menu->items[olds].type==MN_SUBMENU) MN_Unmap(tk_display,menu->items[olds].submenu);
						if(menu->items[i].type==MN_SUBMENU&&(menu->items[i].state&Grayed)!=Grayed){
							XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->width-15,menu->items[menu->selecteditem].y-7,&xr,&yr,&w);
							mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
							}
						}
					return 0;
					break;

			  case XK_Down: i=menu->selecteditem+1;
					olds=menu->selecteditem;
					while(i<menu->numitems && menu->items[i].type==MN_HBAR)
					 i++;
					
					if(i>=menu->numitems){ 
					  i=0;
					  while(i<menu->numitems && menu->items[i].type==MN_HBAR)
					    i++;
					  }
					menu->selecteditem=i;

					if(olds!=i){ mn_DrawFloatingItem(tk_display,menuid,olds);
						mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem); 
						if(menu->items[olds].type==MN_SUBMENU) MN_Unmap(tk_display,menu->items[olds].submenu);
						if(menu->items[i].type==MN_SUBMENU&&(menu->items[i].state&Grayed)!=Grayed){
							XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->width-15,menu->items[menu->selecteditem].y-7,&xr,&yr,&w);
							mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
							}
						}
					
					return 0;
					break;


			  case XK_Right: 
					
					if((menu->items[menu->selecteditem].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed && menu->items[menu->selecteditem].type==MN_SUBMENU)
					{
					  XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->width-15,menu->items[menu->selecteditem].y-7,&xr,&yr,&w);
					  mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
					  /*MN_GiveFocus(tk_display,menu->items[menu->selecteditem].submenu);*/
					  return 0;
					}
					else if(menu->parency_class==WI_MENU){ 
					  mn_ptr=(MenuStruct *)mn_GetRootMenu(tk_display,menuid);
					  /*MN_GiveFocus(tk_display,(MenuStruct *)mn_ptr);*/
					  prevmenu->continuity=False;
					  mn_ptr->continuity=False;
					  XSync(tk_display->display,False);
					  if(mn_ptr->type==MN_FLOATING)
					    return 0;
					  return MN_KEYUNKNOWN;
					}
					break;

			  case XK_Left:
					
					if(menu->parency_class==WI_MENU)
					{
					  /*MN_GiveFocus(tk_display,menu->prevmenu);*/
					  mn_CloseMenu(tk_display,menuid);
					  prevmenu->continuity=False;
					  XSync(tk_display->display,False);
					  if(prevmenu->type==MN_FLOATING)
					    return 0;
					  else return MN_KEYUNKNOWN;
					}
					else return 0;
					break;


			  case XK_Home:
			  case XK_Prior:
					i=0;
					olds=menu->selecteditem;
					while(i<menu->numitems && menu->items[i].type==MN_HBAR)
					 i++;
					
					menu->selecteditem=i;

					if(olds!=i){ mn_DrawFloatingItem(tk_display,menuid,olds);
						mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem); 
						if(menu->items[olds].type==MN_SUBMENU) MN_Unmap(tk_display,menu->items[olds].submenu);
						if(menu->items[i].type==MN_SUBMENU&&(menu->items[i].state&Grayed)!=Grayed){
							XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->width-15,menu->items[menu->selecteditem].y-7,&xr,&yr,&w);
							mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
							}
						}
					
					return 0;
					break;


			  case XK_End:
			  case XK_Next: i=menu->numitems-1;
					olds=menu->selecteditem;
					while(i>=0 && menu->items[i].type==MN_HBAR)
					 i--;
					
					menu->selecteditem=i;

					if(olds!=i){ mn_DrawFloatingItem(tk_display,menuid,olds);
						mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem); 
						if(menu->items[olds].type==MN_SUBMENU) MN_Unmap(tk_display,menu->items[olds].submenu);
						if(menu->items[i].type==MN_SUBMENU&&(menu->items[i].state&Grayed)!=Grayed){
							XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->width-15,menu->items[menu->selecteditem].y-7,&xr,&yr,&w);
							mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
							}
						}
					
					return 0;
					break;




			  case XK_space:
			  case XK_Return:

					VALID2:
					if((menu->items[menu->selecteditem].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed && menu->items[menu->selecteditem].type==MN_SUBMENU)
					{
					  XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->width-15,menu->items[menu->selecteditem].y-7,&xr,&yr,&w);
					  mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
					  /*MN_GiveFocus(tk_display,menu->items[menu->selecteditem].submenu);*/
					  return 0;
					}
					else if(menu->items[menu->selecteditem].type!=MN_SUBMENU&&(menu->items[menu->selecteditem].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed){
					  mn_CloseMenu(tk_display,menuid);
					  XSetWindowBorder(tk_display->display,menu->window,widptr->colors->text);
					  if(menu->parency_class==WI_MENU){
						eventbis.type=FocusOut;
					  	eventbis.xfocus.window=prevmenu->window;
			  			XPutBackEvent(tk_display->display,&eventbis);
						}
					  else{
						eventbis.type=FocusOut;
					  	eventbis.xfocus.window=menu->window;
			  			XPutBackEvent(tk_display->display,&eventbis);
						}
					  menu->hasFocus=False;
					  menu->repeat=menu->selecteditem;
					  /*mn_GiveFocusToWidget(tk_display,menuid);*/
					  return MN_SELECTED;
					  }
					else return 0;
					break;

			  case XK_Escape:
					if(menu->parency_class==WI_MENU)
					{
					  /*MN_GiveFocus(tk_display,menu->prevmenu);*/
					  mn_CloseMenu(tk_display,menuid);
					  prevmenu->continuity=False;
					  return 0;
					}
					else{
						mn_CloseMenu(tk_display,menuid); 
						return MN_ABORTED;
					}
					break;

			  default:	
					
					for(i=0;i<menu->numitems;i++) 
					{
					 if(menu->items[i].key>0 && (key==menu->items[i].text[menu->items[i].key-1]||key==menu->items[i].text[menu->items[i].key-1]+32||key==menu->items[i].text[menu->items[i].key-1]-32))
					  {
						/*fprintf(stderr,"key:%c %d   i:%d  text:%c\n",key,key,i,menu->items[i].text[menu->items[i].key-1]);*/
						olds=menu->selecteditem;
						menu->selecteditem=i;
						if(i!=olds){ mn_DrawFloatingItem(tk_display,menuid,i);
							     mn_DrawFloatingItem(tk_display,menuid,olds);}
						goto VALID2;
					   }
					}
					break;

								    
			}
			return 0;
			break;

/*		case FocusIn: 

			if(tk_event->event.xfocus.detail==NotifyAncestor||tk_event->event.xfocus.detail==NotifyInferior||tk_event->event.xfocus.detail==NotifyNonlinear)
			{
			 menu->hasFocus=True;
			 XAutoRepeatOn(tk_display->display);
			 if(menu->selected==True) mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem);
			 menu->selected=True;
			 XSetWindowBorder(tk_display->display,menu->window,widptr->colors->focus);
			}
			return 0;
			break;

		case FocusOut:

			ret=0;
			if(tk_event->event.xfocus.detail==NotifyAncestor||tk_event->event.xfocus.detail==NotifyInferior||tk_event->event.xfocus.detail==NotifyNonlinear)
			{

			FOCUS_OUT_START:
			 XGetInputFocus(tk_display->display,&r,&xr);
			 mn_ptr=(MenuStruct *)mn_GetRootMenu(tk_display,menuid);
			 if(mn_IsSubmenu(tk_display,mn_ptr->wid_number,r)==False)
			 {

			  mn_ptr->hasFocus=False;
			  mn_ptr->selected=NO;	
			  mn_ptr->continuity=False;
			  menu->hasFocus=False;
			  menu->selected=NO;	
			  menu->continuity=False;
			  mn_DrawFloatingItem(tk_display,menuid,menu->selecteditem);
			  if(mn_ptr->selecteditem>=0&&mn_ptr->selecteditem<mn_ptr->numitems) 
			    mn_DrawMenuBarItem(tk_display,mn_ptr->wid_number,mn_ptr->selecteditem);
			  mn_CloseSubmenus(tk_display,mn_ptr->wid_number,-1);
			  XSetWindowBorder(tk_display->display,mn_ptr->window,widptr->colors->text);
			  mn_ptr->hasFocus=False;
			  mn_ptr->selected=NO;	
			  mn_ptr->continuity=False;
			  return 0;

			 }
			}
			return 0;
			break;
*/
		case ConfigureNotify:
				/*fprintf(stderr,"Menu: Configure Notify\n");*/
				return 0;
				break;

		case PropertyNotify: /*fprintf(stderr,"Menu: Property Notify\n");*/
				return 0;
				break;

		default: return 0;
			 break;

		}
		return 0;
		break;


	default: return 0;
		 break;

 }
 return 0;

}






int mn_MenuBarEvents(tk_display,menuid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
MenuID menuid;
{

 int mask, index;
 XGCValues xgcvalues;
 GC gc;
 KeySym key;
 unsigned long ptr;

 int i,j,ret; 
 int xp, yp, xr, yr;
 Window w, r;
 unsigned int xk;
 int clickpos,old,olds;
 XEvent send_event, eventbis;
 XWindowAttributes attrib;
 ComboStruct *combo;
 MenuStruct *mn_ptr, *menu, *prevmenu;
 WidgetStruct *widptr;
 WidgetStruct *subwidptr;

 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;
 widptr=&tk_display->widgets[menuid];

 if(menu->parency_class==WI_MENU) prevmenu=tk_display->widgets[menu->prevmenu].menu;
 else prevmenu=0;


 switch(tk_event->ev_type){


	case XLIBEVENT: switch(tk_event->event.type){


		case LeaveNotify:
			/*if(tk_event->event.xcrossing.detail==NotifyAncestor||tk_event->event.xcrossing.detail==NotifyInferior||tk_event->event.xcrossing.detail==NotifyNonlinear)
			  fprintf(stderr,"Bar: LeaveNotify  %d\n",tk_event->event.xcrossing.detail);
			*/break;


		case EnterNotify:
			if((menu->state&Frozen)!=Frozen&&(tk_event->event.xcrossing.state&Button1Mask)==Button1Mask && (tk_event->event.xcrossing.detail==NotifyAncestor||tk_event->event.xcrossing.detail==NotifyInferior||tk_event->event.xcrossing.detail==NotifyNonlinear)&&menu->hasFocus==True)
			{
			  /*fprintf(stderr,"Bar: EnterNotify\n");*/

			  if(menu->hasFocus==NO)
			  {
			    if((menu->state&Grayed)!=Grayed) 
			    { 
			      menu->hasFocus=True;
			      /*XSetInputFocus(tk_display->display,menu->window,RevertToPointerRoot,tk_event->event.xcrossing.time);*/
			      XSetWindowBorder(tk_display->display,menu->window,widptr->colors->focus);
			      
			    }
			  }
			  

			  if((menu->state&Grayed)!=Grayed && (tk_event->event.xcrossing.state&Button1Mask)==Button1Mask&&menu->numitems>0)
			  {
			   menu->continuity=YES;
 
			   clickpos=-1;
			   clickpos=mn_GetSelectedItem(tk_display,menuid,tk_event->event.xcrossing.x,tk_event->event.xcrossing.y);
			   
			   olds=menu->selecteditem;
			   menu->selecteditem=clickpos;

			   if(olds==clickpos && clickpos>=0 && clickpos<menu->numitems)
			   {
				
		   		if(menu->items[clickpos].type==MN_SUBMENU&&menu->items[clickpos].submenu>=0)
				{
				  mn_CloseSubmenus(tk_display,menuid,menu->items[clickpos].submenu);
				  mn_ptr=tk_display->widgets[menu->items[clickpos].submenu].menu;
				  subwidptr=&tk_display->widgets[menu->items[clickpos].submenu];
			    	  mn_ptr->hasFocus=False;
			  	  mn_ptr->selected=NO;	
				  mn_ptr->continuity=False;
				  mn_DrawFloatingItem(tk_display,mn_ptr->wid_number,mn_ptr->selecteditem);
				  mn_ptr->selecteditem=0;
			  	  XSetWindowBorder(tk_display->display,mn_ptr->window,subwidptr->colors->text);
			  	  mn_CloseSubmenus(tk_display,mn_ptr->wid_number,-1);
				}
				else mn_CloseSubmenus(tk_display,menuid,-1);
			   }
			   else if(olds!=clickpos && menu->numitems>0) 
			   {
				mn_DrawMenuBarItem(tk_display,menuid,olds);
				mn_CloseSubmenus(tk_display,menuid,-1);
							        
			   }
			 
			   menu->selected=True;			
			   mn_DrawMenuBarItem(tk_display,menuid,clickpos);
			   /*XSetInputFocus(tk_display->display,menu->window,RevertToPointerRoot,tk_event->event.xcrossing.time);*/


			   XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->items[menu->selecteditem].x,menu->height,&xr,&yr,&w);
			   if(menu->items[clickpos].type==MN_SUBMENU&&(menu->items[clickpos].state&Grayed)!=Grayed)
			     mn_Map(tk_display,menu->items[clickpos].submenu,xr,yr);

			   
			  }
			}
			else if((tk_event->event.xcrossing.state&Button1Mask)!=Button1Mask && (tk_event->event.xcrossing.detail==NotifyAncestor||tk_event->event.xcrossing.detail==NotifyInferior||tk_event->event.xcrossing.detail==NotifyNonlinear))
			{
			 menu->continuity=False;

			}
			return 0;
			break;



		case ButtonPress:

		      if((menu->state&Frozen)!=Frozen)
		      {
			if(menu->hasFocus==NO)
			{
			 if((menu->state&Grayed)!=Grayed && tk_event->event.xbutton.button==Button1) 
			 { 
			  menu->hasFocus=True;
			  XSetWindowBorder(tk_display->display,menu->window,widptr->colors->focus);
			 }

			 XMapRaised(tk_display->display,menu->top_level);
			 XSync(tk_display->display,False);
			}
			else{
			  XSetWindowBorder(tk_display->display,menu->window,widptr->colors->focus);
			  XMapRaised(tk_display->display,menu->top_level); 
			}
			


			if((menu->state&Grayed)!=Grayed && tk_event->event.xbutton.button==Button1&&menu->numitems>0)
			{

			 if(tk_display->action.type==ComboSelectingAction) 
			 {
			   combo=tk_display->widgets[tk_display->action.combo].combo; 
			   combo->isOpen=False;
			   XSetInputFocus(tk_display->display,tk_display->widgets[combo->edit].edit->window,RevertToPointerRoot,CurrentTime); 
			   LS_Unmap(tk_display,combo->list);
			   memset(&tk_display->action,0,sizeof(ActionStruct));
			   tk_display->action.combo=tk_display->action.menu=-1;
			 }  
			 
			 menu->continuity=YES;
			 j=0-MN_INTERMARGE/2;
			 clickpos=-1;
			 if(tk_event->event.xbutton.x<MN_INTERMARGE) clickpos=0;
			 else if(menu->numitems>0) for(i=0;i<menu->numitems;i++)
			 {
			   if(tk_event->event.xbutton.x>=j&&tk_event->event.xbutton.x+menu->items[i].width>=tk_event->event.xbutton.x&& menu->items[i].type!=MN_VBAR)
			   { 
				clickpos=i;	
				ret=j;
			   }
			   j=j+menu->items[i].width+2*MN_INTERMARGE;
			 }
			 
			 if(clickpos==-1) clickpos=menu->numitems-1;
			 clickpos=mn_GetSelectedItem(tk_display,menuid,tk_event->event.xbutton.x,tk_event->event.xbutton.y);

			 olds=menu->selecteditem;
			 menu->selecteditem=clickpos;

			 if(menu->selected==True && olds==clickpos && clickpos>=0 && clickpos<menu->numitems)
			 {
								
		   		if(menu->items[clickpos].type==MN_SUBMENU&&menu->items[clickpos].submenu>=0)
				{
				  
				  mn_CloseSubmenus(tk_display,menuid,menu->items[clickpos].submenu);
				  mn_ptr=tk_display->widgets[menu->items[clickpos].submenu].menu;
				  subwidptr=&tk_display->widgets[menu->items[clickpos].submenu];
			    	  mn_ptr->hasFocus=False;
			  	  mn_ptr->selected=NO;	
				  mn_ptr->continuity=False;
				  mn_DrawFloatingItem(tk_display,mn_ptr->wid_number,mn_ptr->selecteditem);
				  mn_ptr->selecteditem=0;
			  	  XSetWindowBorder(tk_display->display,mn_ptr->window,subwidptr->colors->text);
			  	  mn_CloseSubmenus(tk_display,mn_ptr->wid_number,-1);
				}
				else mn_CloseSubmenus(tk_display,menuid,-1);
			  }
			  else if(olds!=clickpos && menu->numitems>0) 
			  {
				
				mn_DrawMenuBarItem(tk_display,menuid,olds);
				mn_CloseSubmenus(tk_display,menuid,-1);
							        
			  }
			  
			  menu->selected=True;			
			  mn_DrawMenuBarItem(tk_display,menuid,clickpos);
			  /*XSetInputFocus(tk_display->display,menu->window,RevertToPointerRoot,tk_event->event.xbutton.time);*/


			  XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->items[menu->selecteditem].x,menu->height,&xr,&yr,&w);
			  if(menu->items[clickpos].type==MN_SUBMENU&&(menu->items[clickpos].state&Grayed)!=Grayed)
			    mn_Map(tk_display,menu->items[clickpos].submenu,xr,yr);
			  			   
			  tk_display->action.type=MenuSelectingAction; 
			  tk_display->action.menu=menuid;
			}
		     	return 0;
		     }
		     else{ 
			XMapRaised(tk_display->display,menu->top_level);
			return 0;
			}
		     break;


		case ButtonRelease:

		     if((menu->state&Frozen)!=Frozen)
		     {
			/*fprintf(stderr,"Menubar button release\n"); */

			
			if(menu->selecteditem>=0&&menu->items[menu->selecteditem].type==MN_SUBMENU &&tk_event->event.xbutton.button==Button1)
			{
				menu->continuity=NO;
				clickpos=menu->selecteditem;
				menu->repeat=-1;
				menu->selecteditem=-1;
				menu->oldselecteditem=-1;
			  	menu->hasFocus=False;
				mn_CloseSubmenus(tk_display,menuid,-1);
				mn_DrawMenuBarItem(tk_display,menuid,clickpos);
				XSetWindowBorder(tk_display->display,menu->window,widptr->colors->text);

				/*XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->items[menu->selecteditem].x,menu->height,&xr,&yr,&w);
				mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
				XSync(tk_display->display,False);
				MN_GiveFocus(tk_display,menu->items[menu->selecteditem].submenu);*/
				memset(&tk_display->action,0,sizeof(ActionStruct));
			   	tk_display->action.combo=tk_display->action.menu=-1;
				return 0;
			}
			else if(menu->selecteditem>=0&&menu->items[menu->selecteditem].type!=MN_SUBMENU&&(menu->items[menu->selecteditem].state&Grayed)!=Grayed&&(menu->state&Grayed)!=Grayed&&tk_event->event.xbutton.x>=0&&tk_event->event.xbutton.x<=menu->width&&tk_event->event.xbutton.y>=0&&tk_event->event.xbutton.y<=menu->height&&tk_event->event.xbutton.button==Button1)
			{
			  clickpos=menu->selecteditem;
			  menu->hasFocus=False;
			  menu->continuity=NO;
			  mn_DrawMenuBarItem(tk_display,menuid,menu->selecteditem);
			  menu->repeat=menu->selecteditem;
			  menu->oldselecteditem=menu->selecteditem; 
			  menu->selecteditem=-1;
			  mn_CloseSubmenus(tk_display,menuid,-1);
			  XSetWindowBorder(tk_display->display,menu->window,widptr->colors->text);
			  memset(&tk_display->action,0,sizeof(ActionStruct));
			   tk_display->action.combo=tk_display->action.menu=-1;
			  if(clickpos>=0) return MN_SELECTED;
			  else return 0;
			}
			else if(tk_event->event.xbutton.button==Button1){
			  menu->continuity=NO;
			  menu->hasFocus=False;
			  menu->repeat=-1;
			  mn_DrawMenuBarItem(tk_display,menuid,menu->selecteditem);
			  menu->oldselecteditem=-1; 
			  menu->selecteditem=-1;
			  mn_CloseSubmenus(tk_display,menuid,-1);
			  XSetWindowBorder(tk_display->display,menu->window,widptr->colors->text);
			  memset(&tk_display->action,0,sizeof(ActionStruct));
			   tk_display->action.combo=tk_display->action.menu=-1;
			  return 0;
			}
			
		     }
		     else return 0;
		     break;


		case MotionNotify:

			
			if(menu->continuity==True && (menu->state&Grayed)!=Grayed && (tk_event->event.xmotion.state&Button1Mask)==Button1Mask)
			if(tk_event->event.xmotion.x>=0 && tk_event->event.xmotion.x<menu->width &&tk_event->event.xmotion.y>=0 && tk_event->event.xmotion.y<menu->height)
			{
			 
			 clickpos=-1;
			
			 XFlush(tk_display->display);
			 XSync(tk_display->display,False);
			 send_event.xmotion.time=tk_event->event.xmotion.time;
		 	 send_event.xmotion.x=tk_event->event.xmotion.x;
		 	 send_event.xmotion.y=tk_event->event.xmotion.y;
		 	 send_event.xmotion.state=tk_event->event.xmotion.state;

		 	 while(XCheckWindowEvent(tk_display->display,menu->window,Button1MotionMask,&eventbis)==True)
				send_event=eventbis;

			 tk_event->event.xmotion.time=send_event.xmotion.time;
			 tk_event->event.xmotion.x=send_event.xmotion.x;
		 	 tk_event->event.xmotion.y=send_event.xmotion.y;
		 	 tk_event->event.xmotion.state=send_event.xmotion.state;
		

			 clickpos=mn_GetSelectedItem(tk_display,menuid,tk_event->event.xmotion.x,tk_event->event.xmotion.y);
			 olds=menu->selecteditem;
			 menu->selecteditem=clickpos;

			 if(olds!=clickpos)
			 { 
				if(menu->selected==True)
				  mn_DrawMenuBarItem(tk_display,menuid,olds);
				mn_CloseSubmenus(tk_display,menuid,-1);
				mn_DrawMenuBarItem(tk_display,menuid,menu->selecteditem);
			 }
			 if(olds==clickpos && menu->items[clickpos].type==MN_SUBMENU)
			 {
			   if((menu->items[clickpos].state&Grayed)!=Grayed)
			     mn_CloseSubmenus(tk_display,menuid,menu->items[clickpos].submenu);
			   else mn_CloseSubmenus(tk_display,menuid,-1);
			 }
			 if(olds==clickpos && menu->items[clickpos].type!=MN_SUBMENU)
			   mn_CloseSubmenus(tk_display,menuid,-1);
			 menu->selected=True;				
				
			 
			 if(clickpos!=olds&&menu->items[clickpos].type==MN_SUBMENU&&(menu->items[clickpos].state&Grayed)!=Grayed)
			 {
			   XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->items[menu->selecteditem].x,menu->height,&xr,&yr,&w);
			   mn_CloseSubmenus(tk_display,menuid,-1);
			   mn_Map(tk_display,menu->items[clickpos].submenu,xr,yr);
			 }
			}
			return 0;
			break;


		case KeyRelease: index=0;

			KEY2:
			if((tk_event->event.xkey.state&1)==1) index=ShiftMapIndex+1;
			else if((tk_event->event.xkey.state&2)==2) index=LockMapIndex+1;
			else if((tk_event->event.xkey.state&4)==4) index=ControlMapIndex+1;
			else if((tk_event->event.xkey.state&8)==8) index=Mod1MapIndex+1;
			else if((tk_event->event.xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
			else if((tk_event->event.xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
			else if((tk_event->event.xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
			else if((tk_event->event.xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

			key=XLookupKeysym(&tk_event->event.xkey,index);
			
			if(menu->continuity==False) switch(key){


			  case XK_Left: 
					
					i=menu->selecteditem-1;
					olds=menu->selecteditem;
					while(i>=0 && menu->items[i].type==MN_VBAR)
					  i--;
					
					if(i<0) i=menu->numitems-1;
					menu->selecteditem=i;

					if(olds!=i){ mn_DrawMenuBarItem(tk_display,menuid,olds);
						mn_DrawMenuBarItem(tk_display,menuid,menu->selecteditem); 
						if(menu->items[olds].type==MN_SUBMENU) mn_CloseMenu(tk_display,menu->items[olds].submenu);
						if(menu->items[i].type==MN_SUBMENU){
							XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->items[menu->selecteditem].x,menu->height,&xr,&yr,&w);
							mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
							}
					XSync(tk_display->display,False);
						}
					return 0;
					break;

			  case XK_Right: 

					i=menu->selecteditem+1;
					olds=menu->selecteditem;
					while(i<menu->numitems && menu->items[i].type==MN_VBAR)
					 i++;
					
					if(i>=menu->numitems) i=0;
					menu->selecteditem=i;

					if(olds!=i){ mn_DrawMenuBarItem(tk_display,menuid,olds);
						mn_DrawMenuBarItem(tk_display,menuid,menu->selecteditem); 
						if(menu->items[olds].type==MN_SUBMENU) mn_CloseMenu(tk_display,menu->items[olds].submenu);
						if(menu->items[i].type==MN_SUBMENU){
							XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->items[menu->selecteditem].x,menu->height,&xr,&yr,&w);
							mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
							}
					XSync(tk_display->display,False);
						}
					return 0;
					break;

			  case XK_Down: 
					if((menu->items[menu->selecteditem].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed && menu->items[menu->selecteditem].type==MN_SUBMENU)
					{
					  XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->items[menu->selecteditem].x,menu->height,&xr,&yr,&w);
					  mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
					  /*MN_GiveFocus(tk_display,menu->items[menu->selecteditem].submenu);*/
					  return 0;
					}
					else return MN_KEYUNKNOWN;
					break;

			  case XK_Up:
					if(menu->parency_class==WI_MENU)
					{
					  /*MN_GiveFocus(tk_display,menu->prevmenu);*/
					  if(menu->items[menu->selecteditem].type==MN_SUBMENU) 
						mn_CloseMenu(tk_display,menu->items[menu->selecteditem].submenu);
					  if(prevmenu->type==MN_FLOATING)
					    return 0;
					  else return MN_KEYUNKNOWN;
					}
					else return MN_KEYUNKNOWN;
					break;


			  case XK_Home:
			  case XK_Prior:
					i=0;
					olds=menu->selecteditem;
					while(i<menu->numitems && menu->items[i].type==MN_HBAR)
					 i++;
					
					menu->selecteditem=i;

					if(olds!=i){ mn_DrawMenuBarItem(tk_display,menuid,olds);
						mn_DrawMenuBarItem(tk_display,menuid,menu->selecteditem); 
						if(menu->items[olds].type==MN_SUBMENU) mn_CloseMenu(tk_display,menu->items[olds].submenu);
						if(menu->items[i].type==MN_SUBMENU){
							XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->items[menu->selecteditem].x,menu->height,&xr,&yr,&w);
							mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
							}
						}
					
					return 0;
					break;


			  case XK_End:
			  case XK_Next: i=menu->numitems-1;
					olds=menu->selecteditem;
					while(i>=0 && menu->items[i].type==MN_HBAR)
					 i--;
					
					menu->selecteditem=i;

					if(olds!=i){ mn_DrawMenuBarItem(tk_display,menuid,olds);
						mn_DrawMenuBarItem(tk_display,menuid,menu->selecteditem); 
						if(menu->items[olds].type==MN_SUBMENU) mn_CloseMenu(tk_display,menu->items[olds].submenu);
						if(menu->items[i].type==MN_SUBMENU){
							XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->items[menu->selecteditem].x,menu->height,&xr,&yr,&w);
							mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
							}
						}
					
					return 0;
					break;




			  case XK_space:
			  case XK_Return:

					VALID2:
					olds=menu->selecteditem;
					if((menu->items[menu->selecteditem].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed && menu->items[menu->selecteditem].type==MN_SUBMENU)
					{
					  XTranslateCoordinates(tk_display->display,menu->window,RootWindow(tk_display->display,tk_display->screen),menu->items[menu->selecteditem].x,menu->height,&xr,&yr,&w);
					  mn_Map(tk_display,menu->items[menu->selecteditem].submenu,xr,yr);
					  /*MN_GiveFocus(tk_display,menu->items[menu->selecteditem].submenu);*/
					  return 0;
					}
					else if(menu->items[menu->selecteditem].type!=MN_SUBMENU&&(menu->items[menu->selecteditem].state&Grayed)!=Grayed && (menu->state&Grayed)!=Grayed)
					{
					  /*XGetWindowAttributes(tk_display->display,menu->window,&attrib);			  
					  if(attrib.map_state==IsViewable)
					    XSetInputFocus(tk_display->display,menu->parent,RevertToPointerRoot,CurrentTime);
					  else XSetInputFocus(tk_display->display,None,RevertToPointerRoot,CurrentTime);
					  */
					  XSetWindowBorder(tk_display->display,menu->window,widptr->colors->text);
					  menu->hasFocus=False;
					  menu->repeat=menu->selecteditem; 		  		  				  
					  /*mn_GiveFocusToWidget(tk_display,menu);*/
					  return MN_SELECTED;
					}
					else return 0;
					break;


			  case XK_Escape:

					if(menu->items[menu->selecteditem].type==MN_SUBMENU) 
					  mn_CloseMenu(tk_display,menu->items[menu->selecteditem].submenu);
					XGetWindowAttributes(tk_display->display,menu->window,&attrib);			  
					/*if(attrib.map_state==IsViewable)
					  XSetInputFocus(tk_display->display,menu->parent,RevertToPointerRoot,CurrentTime);
					else XSetInputFocus(tk_display->display,None,RevertToPointerRoot,CurrentTime);*/
					return MN_ABORTED;
					
					break;

			  default:	
					
					for(i=0;i<menu->numitems;i++) 
					{
					 if(menu->items[i].key>0 && (key==menu->items[i].text[menu->items[i].key-1]||key==menu->items[i].text[menu->items[i].key-1]+32||key==menu->items[i].text[menu->items[i].key-1]-32))
					  {
						/*fprintf(stderr,"key:%c %d   i:%d  text:%c\n",key,key,i,menu->items[i].text[menu->items[i].key-1]);*/
						olds=menu->selecteditem;
						menu->selecteditem=i;
						if(i!=olds){ mn_DrawMenuBarItem(tk_display,menuid,i);
							     mn_DrawMenuBarItem(tk_display,menuid,olds);}
						goto VALID2;
					   }
					}
					break;

								    
			}
			return 0;
			break;

/*
		case FocusIn: 

			if(tk_event->event.xfocus.detail==NotifyAncestor||tk_event->event.xfocus.detail==NotifyInferior||tk_event->event.xfocus.detail==NotifyNonlinear)
			{
			 menu->hasFocus=True;
			 XAutoRepeatOn(tk_display->display);
			 if(menu->selected==True) mn_DrawMenuBarItem(tk_display,menuid,menu->selecteditem);
			 menu->selected=True;
			 XSetWindowBorder(tk_display->display,menu->window,widptr->colors->focus);

			}
			return 0;
			break;


		case FocusOut:

			
			ret=0;
			if(tk_event->event.xfocus.detail==NotifyAncestor||tk_event->event.xfocus.detail==NotifyInferior||tk_event->event.xfocus.detail==NotifyNonlinear)
			{
			 XGetInputFocus(tk_display->display,&r,&xr); 
			 if(mn_IsSubmenu(tk_display,menuid,r)==False)
			 {
			  mn_CloseSubmenus(tk_display,menuid,-1);
			  XSetWindowBorder(tk_display->display,menu->window,widptr->colors->text);
			  menu->hasFocus=False;
			  menu->selected=NO;	
			  menu->continuity=False;
			  mn_DrawMenuBarItem(tk_display,menuid,menu->selecteditem); 
			  mn_DrawMenuBarItem(tk_display,menuid,0); 
			  menu->selected=NO;	
			  return 0;
			 }
			 return 0;
			}
			return 0;
			break;

*/

		case ConfigureNotify:
				/*fprintf(stderr,"Menu: Configure Notify\n");*/
				return 0;
				break;

		case PropertyNotify: /*fprintf(stderr,"Menu: Property Notify\n");*/
				return 0;
				break;

		default: return 0;
			 break;

		}
		return 0;
		break;


	default: return 0;
		 break;

 }
 return 0;

}






int mn_CloseSubmenus(tk_display,menuid,except_menuid)
TkDisplay *tk_display;
MenuID menuid, except_menuid;
{
 int i;
 unsigned long mask;
 MenuStruct *mn_ptr;
 XSetWindowAttributes xswa;
 MenuStruct *menu, *except_menu;
 WidgetStruct *widptr, *subwidptr;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;
 widptr=&tk_display->widgets[menuid];

 if(except_menuid>=0 && except_menuid<tk_display->maxwidgets && tk_display->widgets[except_menuid].class==WI_MENU&&tk_display->widgets[except_menuid].menu!=NULL)
   except_menu=tk_display->widgets[except_menuid].menu;
 else except_menu=NULL;


 if(menu->numitems>0) for(i=0;i<menu->numitems;i++)
 {
  											
  if(menu->items[i].type==MN_SUBMENU && menu->items[i].submenu>=0&&except_menuid!=menu->items[i].submenu)
  {
    mn_ptr=tk_display->widgets[menu->items[i].submenu].menu;
    subwidptr=&tk_display->widgets[menu->items[i].submenu];
    mn_CloseSubmenus(tk_display,mn_ptr->wid_number,-1);
    mn_ptr->hasFocus=False;
    mn_ptr->selected=False;
    mn_ptr->selecteditem=-1;
    mn_ptr->oldselecteditem=-1;
    mn_ptr->repeat=-1;
    mn_ptr->continuity=False;
    xswa.override_redirect=True;
    mask=CWOverrideRedirect;
    XChangeWindowAttributes(tk_display->display,mn_ptr->window,mask,&xswa);
    XSetWindowBorder(tk_display->display,mn_ptr->window,subwidptr->colors->text);    
    XUnmapWindow(tk_display->display,mn_ptr->window);
    XMoveWindow(tk_display->display,mn_ptr->window,50000,50000);
  }
 }

 return 0;

}






int mn_CloseMenu(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 unsigned long mask;
 XSetWindowAttributes xswa;
 MenuStruct *menu, *except_menu;
 WidgetStruct *widptr;

 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;
 widptr=&tk_display->widgets[menuid];

 mn_CloseSubmenus(tk_display,menuid,-1);
 
 menu->hasFocus=False;
 menu->selected=False;
 menu->continuity=False;
 menu->selecteditem=-1;
 menu->repeat=menu->oldselecteditem=-1;
 xswa.override_redirect=True;
 mask=CWOverrideRedirect;

 XChangeWindowAttributes(tk_display->display,menu->window,mask,&xswa);
 XSetWindowBorder(tk_display->display,menu->window,widptr->colors->text);   
 XUnmapWindow(tk_display->display,menu->window);
 XMoveWindow(tk_display->display,menu->window,50000,50000);
 return 0;

}






int mn_CloseParentMenus(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 MenuStruct *mn_ptr;
 int ret;
 MenuStruct *menu, *except_menu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;



START_PARENT_MENUS:

 if(menu->parency_class==WI_MENU && menu->prevmenu>=0)
 {
  mn_ptr=tk_display->widgets[menu->prevmenu].menu;
  mn_CloseSubmenus(tk_display,mn_ptr->wid_number,menuid);
  if(mn_ptr->parency_class==WI_MENU && mn_ptr->prevmenu>=0)
  {
    menu=mn_ptr;
    goto START_PARENT_MENUS;
  }
  return 0;
 }
 return -1;

}







Bool mn_IsSubmenu(tk_display,menuid,window)
TkDisplay *tk_display;
MenuID menuid;
Window window;
{
 MenuStruct *mn_ptr; 
 int i;
 int ret;
 MenuStruct *menu, *except_menu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


 if(menu->window==window) return True;
 if(menu->numitems>0) for(i=0;i<menu->numitems;i++)
 {
   ret=0;
   if(menu->items[i].type==MN_SUBMENU && menu->items[i].submenu>=0)
     ret=mn_IsSubmenu(tk_display,menu->items[i].submenu,window);
   if(ret==True) return True;
 }
 return False;
}





int mn_GiveFocusToWidget(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 XWindowAttributes xwa;
 Window focuswindow;
 Atom gp_actual_type;

 int gp_actual_format;
 unsigned long gp_nitems, gp_bytes_after;
 unsigned long *gp_prop;
 int ret;
 MenuStruct *menu, *except_menu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


  XGetWindowProperty(tk_display->display, menu->top_level,tk_display->atoms._IMAN_WM_FOCUS,0,2, False, AnyPropertyType, &gp_actual_type, &gp_actual_format, & gp_nitems, &gp_bytes_after,(unsigned char **)&gp_prop);
  
  if(gp_nitems>0)
  {
	focuswindow=(Window)gp_prop[0];
 	ret=XGetWindowAttributes(tk_display->display,focuswindow,&xwa);
	/*if(ret!=0 && xwa.map_state==IsViewable) XSetInputFocus(tk_display->display,focuswindow,RevertToPointerRoot,CurrentTime);*/
	XFree(gp_prop);
 	return 0;
  }
  else {
  	/*XSetInputFocus(tk_display->display,menu->top_level,RevertToPointerRoot,CurrentTime);*/
	return 0;
  }

}





int mn_GetSelectedItem(tk_display,menuid,x,y)
TkDisplay *tk_display;
MenuID menuid;
int x,y;
{
 int i, j, clickpos;
 MenuStruct *menu, *except_menu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


 clickpos=-1;
 /*fprintf(stderr,"Debut de GetSelected\n");*/

 if(menu->type==MN_MENUBAR)
 {
   j=0-MN_INTERMARGE/2;
   clickpos=-1;

   if(x<MN_INTERMARGE&&menu->numitems>0) 
   {
     for(i=0;i<menu->numitems;i++)
     if(menu->items[i].type!=MN_VBAR&&menu->items[i].type!=MN_HBAR) 
     {
	clickpos=i;
	goto END_OF_FUNCTION;
     }
     goto END_OF_FUNCTION;
   }
   else if(menu->numitems>0) for(i=0;i<menu->numitems;i++)
   {
     if(x>=j&&j+menu->items[i].width+2*MN_INTERMARGE>=x&& menu->items[i].type!=MN_VBAR&&menu->items[i].type!=MN_HBAR)
     { 
	clickpos=i;	
	goto END_OF_FUNCTION;
     }
     j=j+menu->items[i].width+2*MN_INTERMARGE;
   }
   goto END_OF_FUNCTION;
 }

 else if(menu->type==MN_FLOATING)
 {

   j=MN_UMARGE;
   clickpos=-1;

   if(y<MN_UMARGE&&menu->numitems>0) 
   {
     for(i=0;i<menu->numitems;i++)
     if(menu->items[i].type!=MN_VBAR&&menu->items[i].type!=MN_HBAR) 
     {
	clickpos=i;
	goto END_OF_FUNCTION;
     }
   }
   else if(menu->numitems>0) for(i=0;i<menu->numitems;i++)
   {
     if(y>=j&&j+menu->items[i].height>=y&& menu->items[i].type!=MN_HBAR)
     { 
	clickpos=i;	
	goto END_OF_FUNCTION;
     }
     j=j+menu->items[i].height;
   }

 }


 END_OF_FUNCTION:
 /*fprintf(stderr,"Get Return %d\n",clickpos);*/
  return clickpos;

}





