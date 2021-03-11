/*
 *
 * 	ls_events.c  
 * 	evenements des listes
 *
 * 	Modification :  02/01/94
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

#include <X11/iman/widgets.h>







/*
 *
 * Gestion des evenements d'une liste
 *
 *
 */

int ls_ListEvents(tk_display,listid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ListID listid;
{

 int x1, x2, mask, index;
 XGCValues xgcvalues;
 XEvent eventbis,send_event;
 GC gc;
 KeySym key;
 unsigned long ptr;
 int i,j,ret; 
 int xp, yp, xr, yr;
 Window w, r;
 unsigned int xk;
 int clickpos,old,olds;
 ComboStruct *combo;
 Window if_win;
 int if_revert;
 ListStruct *list;
 ScrollbarStruct *SBV, *SBH;
 EditStruct *edit;
 WidgetStruct *widptr;
 WidgetStruct *comboptr;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;
 widptr=&tk_display->widgets[listid];


 SBV=tk_display->widgets[list->SBV].scroll;
 SBH=tk_display->widgets[list->SBH].scroll;


 switch(tk_event->ev_type){


	case JUSTFOCUS:
			
			if(list->hasFocus==False)
			{
			 if((list->state&Grayed)!=Grayed&&list->numitems>0) 
			 { 
			   ptr=(unsigned long)list->listwindow;
			  XChangeProperty(tk_display->display,list->top_level,tk_display->atoms._IMAN_WM_FOCUS,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);

			   list->hasFocus=True;
			   XSetInputFocus(tk_display->display,list->listwindow,RevertToPointerRoot,CurrentTime);
			   list->hasFocus=True;
			   _LS_DrawFocus(tk_display,listid,ON);
			   if(list->selected==True) 
			     ls_DrawItem(tk_display,listid,list->selecteditem);
			  }
			  XMapRaised(tk_display->display,list->top_level); 
			  return JUSTFOCUS;
			}
			break;


	case SB_UP:   	/*fprintf(stderr,"List: topitem:%d  downitem:%d  numitems:%d\n",list->topitem,list->downitem,list->numitems);*/
		   	if(list->hasFocus==False&&list->numitems>0)
		    	{
			  if((list->state&Grayed)!=Grayed) 
			  { 
			    list->hasFocus=True;
			    XSetInputFocus(tk_display->display,list->listwindow,RevertToPointerRoot,CurrentTime);
			    if((list->flags&NoBorderFlag)!=NoBorderFlag)
			      _LS_DrawFocus(tk_display,listid,ON);
			    list->hasFocus=True;
			    if(list->selected==True) ls_DrawItem(tk_display,listid,list->selecteditem);
			  }
			  XMapRaised(tk_display->display,list->top_level);
			}

			if((list->state&Grayed)!=Grayed && list->topitem>0)
			{
				
				list->topitem--;

				mask=GCForeground|GCBackground;
				xgcvalues.foreground=widptr->colors->text;
 				xgcvalues.background=widptr->colors->bg;
 				gc=XCreateGC(tk_display->display, list->listwindow, mask, &xgcvalues);
				_WID_Tempo(tk_display);
				XCopyArea(tk_display->display,list->listwindow,list->listwindow,gc,0,LS_UMARGE,list->width,(list->downitem)*list->itemheight,0,LS_UMARGE+list->itemheight);
				XFreeGC(tk_display->display,gc);

				ls_DrawItem(tk_display,listid,list->topitem);
				return JUSTFOCUS;
			}
			/*fprintf(stderr,"List: topitem:%d  downitem:%d  numitems:%d\n",list->topitem,list->downitem,list->numitems);*/
			return JUSTFOCUS;
			break;


	case SB_DOWN: /*fprintf(stderr,"List: topitem:%d downitem:%d numitems:%d state:%d\n",list->topitem,list->downitem,list->numitems,list->state);*/
			if(list->hasFocus==False&&list->numitems>0)
			{
			 if((list->state&Grayed)!=Grayed) 
			 { 
			  list->hasFocus=True;
			  XSetInputFocus(tk_display->display,list->listwindow,RevertToPointerRoot,CurrentTime);
			  if((list->flags&NoBorderFlag)!=NoBorderFlag)
			    _LS_DrawFocus(tk_display,listid,ON);
			  list->hasFocus=True;
			  if(list->selected==True) ls_DrawItem(tk_display,listid,list->selecteditem);
			  }
			 XMapRaised(tk_display->display,list->top_level);
			}

			if((list->state&Grayed)!=Grayed && list->topitem+list->downitem<list->numitems-1)
			{
				list->topitem++;

				mask=GCForeground|GCBackground;
				xgcvalues.foreground=widptr->colors->text;
 				xgcvalues.background=widptr->colors->bg;
 				gc=XCreateGC(tk_display->display, list->listwindow, mask, &xgcvalues);
				_WID_Tempo(tk_display);
				XCopyArea(tk_display->display,list->listwindow,list->listwindow,gc,0,LS_UMARGE+list->itemheight,list->width,list->height-LS_UMARGE-list->itemheight,0,LS_UMARGE);
				XFreeGC(tk_display->display,gc);
				ls_DrawItem(tk_display,listid,list->topitem+list->downitem);
				return JUSTFOCUS;
			}
		
			return JUSTFOCUS;			
			break;


	case SB_PAGEDOWN:
	case SB_PAGEUP: /*fprintf(stderr,"List: topitem:%d downitem:%d numitems:%d\n state:%d",list->topitem,list->downitem,list->numitems,list->state);  
			fprintf(stderr,"SB page event\n");*/
			if(list->hasFocus==False&&list->numitems>0)
			{
			  if((list->state&Grayed)!=Grayed) 
			  { 
			   list->hasFocus=True;
			   XSetInputFocus(tk_display->display,list->listwindow,RevertToPointerRoot,CurrentTime);
			   if((list->flags&NoBorderFlag)!=NoBorderFlag)
			     _LS_DrawFocus(tk_display,listid,ON);
			   list->hasFocus=True;
			   if(list->selected==True) 
			     ls_DrawItem(tk_display,listid,list->selecteditem);
			  }
			  XMapRaised(tk_display->display,list->top_level);
			}
			_WID_Tempo(tk_display);
			_WID_Tempo(tk_display);
			if((list->state&Grayed)!=Grayed && list->topitem>=0&&list->numitems>0)
			{
				list->topitem=SBV->position;
				ls_DrawList(tk_display,listid);
				return JUSTFOCUS;
			 }
			return JUSTFOCUS;
			break;


	case SB_LEFT:
	case SB_RIGHT:
	case SB_PAGELEFT:
	case SB_PAGERIGHT:   

			/*fprintf(stderr,"List: topitem:%d  downitem:%d  numitems:%d\n",list->topitem,list->downitem,list->numitems);*/
			if(list->hasFocus==False&&list->numitems>0)
			{
			 if((list->state&Grayed)!=Grayed) 
			 { 
			  list->hasFocus=True;
			  XSetInputFocus(tk_display->display,list->listwindow,RevertToPointerRoot,CurrentTime);
			  if((list->flags&NoBorderFlag)!=NoBorderFlag)
			    _LS_DrawFocus(tk_display,listid,ON);
			  list->hasFocus=True;
			  if(list->selected==True) ls_DrawItem(tk_display,listid,list->selecteditem);
			 }
			 XMapRaised(tk_display->display,list->top_level);
			}

			if((list->state&Grayed)!=Grayed && list->topitem>=0)
			{
				_WID_Tempo(tk_display);
				olds=list->hpos;
				list->hpos=SBH->position;
				if(olds!=list->hpos) ls_DrawList(tk_display,listid);
				return JUSTFOCUS;
			 }
			/*else fprintf(stderr,"List: topitem:%d  downitem:%d  numitems:%d\n",list->topitem,list->downitem,list->numitems);*/
			return JUSTFOCUS;
			break;


	case SB_THUMBMOVED:  
			
			if(list->hasFocus==False&&list->numitems>0)
			{
			 if((list->state&Grayed)!=Grayed) 
			 { 
			  list->hasFocus=True;
			  XSetInputFocus(tk_display->display,list->listwindow,RevertToPointerRoot,CurrentTime);
			  if((list->flags&NoBorderFlag)!=NoBorderFlag)
			    _LS_DrawFocus(tk_display,listid,ON);
			  list->hasFocus=True;
			  if(list->selected==True) ls_DrawItem(tk_display,listid,list->selecteditem);
			  }
			 XMapRaised(tk_display->display,list->top_level);
			}


			if((list->state&Grayed)!=Grayed)
			{
			  _WID_Tempo(tk_display);
			  if(tk_event->scroll==list->SBV) 
			   {
				list->topitem=SBV->position;
				ls_DrawList(tk_display,listid);
				return 0;
			   }
			  else {
				list->hpos=SBH->position;
				ls_DrawList(tk_display,listid);
				return 0;
			   }
			}
			/*else fprintf(stderr,"List: topitem:%d  downitem:%d  numitems:%d\n",list->topitem,list->downitem,list->numitems);*/
			return 0;			
			break;


	case XLIBEVENT: switch(tk_event->event.type){


		case ButtonPress:

	      	if((list->state&Frozen)!=Frozen)
	      	{
			if(list->hasFocus==NO&&list->numitems>0)
			{
			 if((list->state&Grayed)!=Grayed) 
			 { 
			  ptr=(unsigned long)list->listwindow;
			  if(list->parency_class==0 )XChangeProperty(tk_display->display,list->top_level,tk_display->atoms._IMAN_WM_FOCUS,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);

			  list->hasFocus=True;
			  XSetInputFocus(tk_display->display,list->listwindow,RevertToPointerRoot,tk_event->event.xbutton.time);
			  if((list->flags&NoBorderFlag)!=NoBorderFlag)
			    _LS_DrawFocus(tk_display,listid,ON);
			 }
			 XMapRaised(tk_display->display,list->top_level);
			}
			else XMapRaised(tk_display->display,list->top_level);

			if((list->state&Grayed)!=Grayed && tk_event->event.xbutton.button==Button1)
			{
			 list->continuity=YES;
 			 clickpos=(tk_event->event.xbutton.y-LS_UMARGE)/list->itemheight;
			 if(clickpos>=0 && clickpos<=list->downitem && clickpos+list->topitem<list->numitems)
			  {
				olds=list->selecteditem;
				list->selecteditem=list->topitem+clickpos; 
				if(list->selected==True) ls_DrawItem(tk_display,listid,olds);
				list->selected=True;

				if(list->multipleSelection==True && (tk_event->event.xbutton.state&ControlMask)==ControlMask&&(list->items[clickpos+list->topitem].state&Grayed)!=Grayed)
				{
				  if(list->items[list->selecteditem].selected==True)
				   list->items[list->selecteditem].selected=False;
				  else list->items[list->selecteditem].selected=True;
				}
				else if((tk_event->event.xbutton.state&ShiftMask)==ShiftMask&&(list->items[clickpos+list->topitem].state&Grayed)!=Grayed)
				{
				  
				  for(j=0;j<list->numitems;j++)
				   if(list->items[j].selected==True) 
				   {
				    /*fprintf(stderr,"Item %d deselectionne\n",j);*/
				    list->items[j].selected=False;
				    if(j>=list->topitem&&j<=list->topitem+list->downitem) ls_DrawItem(tk_display,listid,j);
				   }
				}

				ls_DrawItem(tk_display,listid,list->selecteditem);
				
				if((list->items[clickpos+list->topitem].state&Grayed)!=Grayed) 
				  return LS_CLICKED;
				else return JUSTFOCUS;
			  }
			  /*else fprintf(stderr,"List: topitem:%d  downitem:%d  numitems:%d\n",list->topitem,list->downitem,list->numitems); */
			  return JUSTFOCUS;
			}
			else return JUSTFOCUS;
		    }
	         else{
		   XMapRaised(tk_display->display,list->top_level); 
 		   return 0;
		   }
		 break;




		case ButtonRelease:

			if(list->continuity==True && tk_event->event.xbutton.button==Button1)
			{
			  list->continuity=False;
			  ptr=0;

/* 85000 a 90000 clicks */
			  while(ptr<tk_display->widget_double_click) ptr++;			 
			 
			  if(XCheckTypedWindowEvent(tk_display->display,list->listwindow,ButtonPress,&eventbis)==True && eventbis.xbutton.button==Button1)
     		  	  {   
				 clickpos=(eventbis.xbutton.y-LS_UMARGE)/list->itemheight;
				 if(clickpos+list->topitem==list->selecteditem&&(list->items[clickpos+list->topitem].state&Grayed)!=Grayed) 
				   {    list->continuity=False;
					XCheckTypedWindowEvent(tk_display->display,list->listwindow,ButtonRelease,&eventbis);
					return LS_DOUBLECLICKED;}
				else{
				 XPutBackEvent(tk_display->display,&eventbis);
				 /*fprintf(stderr,"Clickpos different pour DOUBLECLICKED \n");*/
				 }
		  	  }
			  if(list->numitems>0 && list->selected==True && (list->items[list->selecteditem].state&Grayed)!=Grayed)
			    return LS_RELEASED;
			  else return 0;
			}
			else return 0;
			break;



		case MotionNotify:

			if(list->continuity==True && (list->state&Grayed)!=Grayed && (tk_event->event.xmotion.state&Button1Mask)==Button1Mask)
			{
			 olds=list->selecteditem;

			 send_event.xmotion.time=tk_event->event.xmotion.time;
		 	 send_event.xmotion.x=tk_event->event.xmotion.x;
		 	 send_event.xmotion.y=tk_event->event.xmotion.y;
		 	 send_event.xmotion.state=tk_event->event.xmotion.state;

		 	 while(XCheckWindowEvent(tk_display->display,list->listwindow,Button1MotionMask,&eventbis)==True)
				send_event=eventbis;

			 tk_event->event.xmotion.time=send_event.xmotion.time;
			 tk_event->event.xmotion.x=send_event.xmotion.x;
		 	 tk_event->event.xmotion.y=send_event.xmotion.y;
		 	 tk_event->event.xmotion.state=send_event.xmotion.state;


			 clickpos=(tk_event->event.xmotion.y-LS_UMARGE)/list->itemheight;
			 if(tk_event->event.xmotion.y<0) clickpos--;

			 if(clickpos+list->topitem!=olds && clickpos>=0 && clickpos<=list->downitem)
			  {
				olds=list->selecteditem;
				list->selecteditem=list->topitem+clickpos; 
				if(list->selecteditem>=list->numitems) list->selecteditem=list->numitems-1;

				if(list->multipleSelection==True && (tk_event->event.xmotion.state&ControlMask)==ControlMask&&(list->items[clickpos+list->topitem].state&Grayed)!=Grayed)
				{
				  if(list->items[list->selecteditem].selected==True)
				   list->items[list->selecteditem].selected=False;
				  else list->items[list->selecteditem].selected=True;
				}
				if(list->selecteditem!=olds) ls_DrawItem(tk_display,listid,olds);
				list->selected=True;
				if(list->selecteditem!=olds) ls_DrawItem(tk_display,listid,list->selecteditem);
				return LS_CLICKED;
			  }
			 else if(clickpos+list->topitem!=olds && clickpos<0 && list->topitem>0)
			  {				
				
				list->selecteditem=list->topitem-1; 
				ls_DrawItem(tk_display,listid,olds);
				list->topitem--;
				mask=GCForeground|GCBackground;
				xgcvalues.foreground=widptr->colors->text;
				xgcvalues.background=widptr->colors->bg;
				gc=XCreateGC(tk_display->display, list->listwindow, mask, &xgcvalues);
				_WID_Tempo(tk_display);
				XCopyArea(tk_display->display,list->listwindow,list->listwindow,gc,0,LS_UMARGE,list->width,(list->downitem)*list->itemheight,0,LS_UMARGE+list->itemheight);
				XFreeGC(tk_display->display,gc);

				if(list->multipleSelection==True && (tk_event->event.xmotion.state&ControlMask)==ControlMask&&(list->items[clickpos+list->topitem].state&Grayed)!=Grayed)
				{
				  if(list->items[list->selecteditem].selected==True)
				   list->items[list->selecteditem].selected=False;
				  else list->items[list->selecteditem].selected=True;
				}
				ls_DrawItem(tk_display,listid,list->selecteditem);

				if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
				 SB_SetPosition(tk_display,list->SBV,list->topitem);

				XQueryPointer(tk_display->display,list->listwindow,&w,&r,&xr,&yr,&xp,&yp,&xk);

		   		tk_event->event.xbutton.x=xp;
		  		tk_event->event.xbutton.y=yp;
		  		tk_event->event.xbutton.state=xk;
		
		  		XSync(tk_display->display,False);
  		  		if(XCheckTypedWindowEvent(tk_display->display,list->listwindow,ButtonRelease,&eventbis)==True && eventbis.xbutton.button==Button1)
     		  		{   
				 eventbis.type=ButtonRelease;
				 eventbis.xbutton.type=ButtonRelease;
				 XPutBackEvent(tk_display->display,&eventbis);
				 return LS_CLICKED;
		  		}
				else{
				 if(XCheckTypedWindowEvent(tk_display->display,list->listwindow,MotionNotify,&eventbis)==True && (eventbis.xmotion.state&Button1Mask)==Button1Mask)
				 {
				  XPutBackEvent(tk_display->display,&eventbis);
				 return LS_CLICKED;
				 }
				 else 
				  XPutBackEvent(tk_display->display,&tk_event->event);
  				 return LS_CLICKED;
		     		}
				
			  }
			 else if(clickpos+list->topitem!=olds && clickpos<0 && list->topitem<=0)
			  {				
				
				list->selecteditem=0; 
				if(olds!=list->selecteditem){

				if(list->multipleSelection==True && (tk_event->event.xmotion.state&ControlMask)==ControlMask&&(list->items[clickpos+list->topitem].state&Grayed)!=Grayed)
				{
				  if(list->items[list->selecteditem].selected==True)
				   list->items[list->selecteditem].selected=False;
				  else list->items[list->selecteditem].selected=True;
				}

				ls_DrawItem(tk_display,listid,olds);
				ls_DrawItem(tk_display,listid,list->selecteditem);
				return LS_CLICKED;}
				else return 0;
			  }
			 else if(clickpos+list->topitem!=olds && clickpos>list->downitem && list->topitem+list->downitem<list->numitems-1)
			  {				
				
				list->selecteditem=list->topitem+list->downitem+1; 
				ls_DrawItem(tk_display,listid,olds);
				list->topitem++;
				mask=GCForeground|GCBackground;
				xgcvalues.foreground=widptr->colors->text;
				xgcvalues.background=widptr->colors->bg;
				gc=XCreateGC(tk_display->display, list->listwindow, mask, &xgcvalues);
				_WID_Tempo(tk_display);
				XCopyArea(tk_display->display,list->listwindow,list->listwindow,gc,0,LS_UMARGE+list->itemheight,list->width,list->height-LS_UMARGE-list->itemheight,0,LS_UMARGE);
				XFreeGC(tk_display->display,gc);
				if(list->multipleSelection==True && (tk_event->event.xmotion.state&ControlMask)==ControlMask&&(list->items[clickpos+list->topitem].state&Grayed)!=Grayed)
				{
				  if(list->items[list->selecteditem].selected==True)
				   list->items[list->selecteditem].selected=False;
				  else list->items[list->selecteditem].selected=True;
				}
				ls_DrawItem(tk_display,listid,list->selecteditem);

				if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
				 SB_SetPosition(tk_display,list->SBV,list->topitem);

				XQueryPointer(tk_display->display,list->listwindow,&w,&r,&xr,&yr,&xp,&yp,&xk);

		   		tk_event->event.xbutton.x=xp;
		  		tk_event->event.xbutton.y=yp;
		  		tk_event->event.xbutton.state=xk;
		
		  		XSync(tk_display->display,False);
  		  		if(XCheckTypedWindowEvent(tk_display->display,list->listwindow,ButtonRelease,&eventbis)==True && eventbis.xbutton.button==Button1)
     		  		{   
				 eventbis.type=ButtonRelease;
				 eventbis.xbutton.type=ButtonRelease;
				 XPutBackEvent(tk_display->display,&eventbis);
				 return LS_CLICKED;
		  		}
   		  		else{
				 if(XCheckTypedWindowEvent(tk_display->display,list->listwindow,MotionNotify,&eventbis)==True && (eventbis.xmotion.state&Button1Mask)==Button1Mask)
				 {
				  XPutBackEvent(tk_display->display,&eventbis);
				 return LS_CLICKED;
				 }
				 else 
				  XPutBackEvent(tk_display->display,&tk_event->event);
  				 return LS_CLICKED;
		     		}

				
			  }
			 else if(clickpos+list->topitem!=olds && clickpos>list->downitem && list->topitem+list->downitem>=list->numitems-1)
			  {				
				
				list->selecteditem=list->numitems-1; 
				if(olds!=list->selecteditem){
				if(list->multipleSelection==True && (tk_event->event.xmotion.state&ControlMask)==ControlMask&&(list->items[clickpos+list->topitem].state&Grayed)!=Grayed)
				{
				  if(list->items[list->selecteditem].selected==True)
				   list->items[list->selecteditem].selected=False;
				  else list->items[list->selecteditem].selected=True;
				}
				ls_DrawItem(tk_display,listid,olds);
				ls_DrawItem(tk_display,listid,list->selecteditem);
				return LS_CLICKED;}
				else return 0;
			  }
			 else return 0;
			}
			else return 0;
			break;



		case KeyRelease: index=0;

			olds=list->selecteditem;
			
			/*send_event.xkey.time=tk_event->event.xkey.time;*/
		 	send_event.xkey.state=tk_event->event.xkey.state;
			send_event.xkey.keycode=tk_event->event.xkey.keycode;

		 	while(XCheckWindowEvent(tk_display->display,list->listwindow,KeyReleaseMask,&eventbis)==True)
				send_event=eventbis;

			/*tk_event->event.xkey.time=send_event.xkey.time;*/
			tk_event->event.xkey.state=send_event.xkey.state;			
			tk_event->event.xkey.keycode=send_event.xkey.keycode;
			

			if((tk_event->event.xkey.state&1)==1) index=ShiftMapIndex+1;
			else if((tk_event->event.xkey.state&2)==2) index=LockMapIndex+1;
			else if((tk_event->event.xkey.state&4)==4) index=ControlMapIndex+1;
			else if((tk_event->event.xkey.state&8)==8) index=Mod1MapIndex+1;
			else if((tk_event->event.xkey.state&Mod2Mask)==Mod2Mask) index=Mod2MapIndex+1;
			else if((tk_event->event.xkey.state&Mod3Mask)==Mod3Mask) index=Mod3MapIndex+1;
			else if((tk_event->event.xkey.state&Mod4Mask)==Mod4Mask) index=Mod4MapIndex+1;
			else if((tk_event->event.xkey.state&Mod5Mask)==Mod5Mask) index=Mod5MapIndex+1;

			key=XLookupKeysym(&tk_event->event.xkey,index);
			
			if(list->continuity==False) switch(key){


			  case XK_Right: 
					
					if(list->hpos<SBH->range)
					{
					  list->hpos++;
					  if(widptr->type==LS_HSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL)
					    SB_SetPosition(tk_display,list->SBH,list->hpos);
					  ls_DrawList(tk_display,listid);
					 }
					return 0;
					break;

			  case XK_Left: 
					 if(list->hpos>0)
					 {
					   list->hpos--;
					   if(widptr->type==LS_HSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL)
					     SB_SetPosition(tk_display,list->SBH,list->hpos);
					   ls_DrawList(tk_display,listid);
					 }
					
					return 0;
					break;

			  case XK_Home:	if(list->selected==False && list->numitems>0)
					{
						old=list->topitem;
						list->selected=True;
						list->selecteditem=0;
						list->topitem=0;
						if(old==0) ls_DrawItem(tk_display,listid,list->selecteditem);
						else ls_DrawList(tk_display,listid);
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
						}
					else if(list->selected==True && list->numitems>0)
					{
						old=list->topitem;
						olds=list->selecteditem;
						list->selecteditem=0;
						list->topitem=0;
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);

						if(old!=0) ls_DrawList(tk_display,listid);
						else{
						  ls_DrawItem(tk_display,listid,olds);
						  ls_DrawItem(tk_display,listid,0);
						}
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
					}
					else return 0;
					break;

			  case XK_End:  if(list->selected==False && list->numitems>0)
					{
						old=list->topitem;
						list->selected=True;
						list->selecteditem=0;
						list->topitem=0;
						ls_DrawList(tk_display,listid);
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
						}
					else if(list->selected==True && list->numitems>0)
					{
						old=list->topitem;
						olds=list->selecteditem;
						list->selecteditem=list->numitems-1;
						list->topitem=list->numitems-list->downitem-1;
						if(list->topitem<0) list->topitem=0;
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);
						if(old!=list->topitem) ls_DrawList(tk_display,listid);
						else{
						  ls_DrawItem(tk_display,listid,olds);
						  ls_DrawItem(tk_display,listid,list->selecteditem);
						}
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
					}
					else return 0;
					break;

			  case XK_Up: if(list->selected==False && list->numitems>0)
					{
						old=list->topitem;
						list->topitem=0;
						list->selected=True;
						list->selecteditem=0;

						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);

						if(old==0) ls_DrawItem(tk_display,listid,list->selecteditem);
						else ls_DrawList(tk_display,listid);
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
						}
					else if(list->selected==True && list->selecteditem>list->topitem && list->selecteditem>0&&list->selecteditem<=list->topitem+list->downitem)
					{
						list->selecteditem--;
						ls_DrawItem(tk_display,listid,list->selecteditem+1);
						ls_DrawItem(tk_display,listid,list->selecteditem);
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
					}
					else if(list->selected==True && list->selecteditem==list->topitem && list->selecteditem>0&&list->topitem>0&&list->selecteditem<=list->topitem+list->downitem)
					{
						list->selecteditem--;
						ls_DrawItem(tk_display,listid,list->selecteditem+1);
						list->topitem--;
						
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);
						
						mask=GCForeground|GCBackground;
						xgcvalues.foreground=widptr->colors->text;
 						xgcvalues.background=widptr->colors->bg;
 						gc=XCreateGC(tk_display->display, list->listwindow, mask, &xgcvalues);
						_WID_Tempo(tk_display);						XCopyArea(tk_display->display,list->listwindow,list->listwindow,gc,0,LS_UMARGE,list->width,(list->downitem)*list->itemheight,0,LS_UMARGE+list->itemheight);
						XFreeGC(tk_display->display,gc);
						ls_DrawItem(tk_display,listid,list->selecteditem);
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
					}
					else if(list->selected==True && list->selecteditem>list->topitem+list->downitem&&list->selecteditem<list->numitems)
					{
						list->selecteditem--;
						if(list->selecteditem+list->downitem<list->numitems)
						  list->topitem=list->selecteditem;
						else list->topitem=list->numitems-list->downitem-1;
						if(list->topitem<0) list->topitem=0;

						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);
						
						ls_DrawList(tk_display,listid);
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
					}
					else return 0;
					break;
			
			  case XK_Down: if(list->selected==False && list->numitems>0)
					{
						old=list->topitem;
						list->topitem=0;
						list->selected=True;
						list->selecteditem=0;
						
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);
						
						if(old==0) ls_DrawItem(tk_display,listid,list->selecteditem);
						else ls_DrawList(tk_display,listid);
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
					}
					else if(list->selected==True && list->selecteditem<list->topitem+list->downitem && list->selecteditem<list->numitems-1&&list->selecteditem>=list->topitem)
					{
						/*fprintf(stderr,"cas 1  ");*/
						list->selecteditem++;
						ls_DrawItem(tk_display,listid,list->selecteditem-1);
						ls_DrawItem(tk_display,listid,list->selecteditem);
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
					}
					else if(list->selected==True && list->selecteditem==list->topitem+list->downitem && list->selecteditem<list->numitems-1&&list->selecteditem>=list->topitem)
					{
						/*fprintf(stderr,"cas 2  ");*/
						list->selecteditem++;
						ls_DrawItem(tk_display,listid,list->selecteditem-1);
						list->topitem++;

						mask=GCForeground|GCBackground;
						xgcvalues.foreground=widptr->colors->text;
 						xgcvalues.background=widptr->colors->bg;
 						gc=XCreateGC(tk_display->display, list->listwindow, mask, &xgcvalues);			
						_WID_Tempo(tk_display);						XCopyArea(tk_display->display,list->listwindow,list->listwindow,gc,0,LS_UMARGE+list->itemheight,list->width,list->height-LS_UMARGE-list->itemheight,0,LS_UMARGE);
						XFreeGC(tk_display->display,gc);
						ls_DrawItem(tk_display,listid,list->selecteditem);
						
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);
						
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
					}
					else if(list->selected==True && list->selecteditem<list->numitems-1)
					{
						/*fprintf(stderr,"cas 3  ");*/
						list->selecteditem++;
						list->topitem=list->selecteditem;
						if(list->topitem+list->downitem>=list->numitems)
						  list->topitem=list->numitems-list->downitem-1;
						if(list->topitem<0)
						  list->topitem=0;
						ls_DrawList(tk_display,listid);
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);

						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
					}
					
					else return 0;
					break;

			  case XK_Prior: if(list->selected==False && list->numitems>0)
					{
						old=list->topitem;
						list->selected=True;
						list->selecteditem=0;
						list->topitem=list->selecteditem;

						_WID_Tempo(tk_display);
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						SB_SetPosition(tk_display,list->SBV,list->topitem);

						if(old==0) ls_DrawItem(tk_display,listid,list->selecteditem);
						else ls_DrawList(tk_display,listid);
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
						}
					else if(list->selected==True && list->numitems>0)
					{
						old=list->topitem;
						olds=list->selecteditem;
						list->selecteditem=list->selecteditem-list->downitem-1;
						if(list->selecteditem<0) list->selecteditem=0;
						list->topitem=list->selecteditem;

						_WID_Tempo(tk_display);
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						SB_SetPosition(tk_display,list->SBV,list->topitem);

						if(old!=list->topitem) ls_DrawList(tk_display,listid);
						else{
						  ls_DrawItem(tk_display,listid,olds);
						  ls_DrawItem(tk_display,listid,0);
						}
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
					}
					else return 0;
					break;

			  case XK_Next: if(list->selected==False && list->numitems>0)
					{
						old=list->topitem;
						list->selected=True;
						list->selecteditem=0;
						list->topitem=list->selecteditem;

						_WID_Tempo(tk_display);
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);
						
						if(old==0) ls_DrawItem(tk_display,listid,list->selecteditem);
						else ls_DrawList(tk_display,listid);
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
						}
					else if(list->selected==True && list->numitems>0)
					{
						old=list->topitem;
						olds=list->selecteditem;
						list->selecteditem=list->selecteditem+list->downitem+1;
						if(list->selecteditem>=list->numitems) list->selecteditem=list->numitems-1;
						list->topitem=list->selecteditem;

						if(list->topitem>list->numitems-1-list->downitem){ 
							list->topitem=list->numitems-1-list->downitem;
							if(list->topitem<0) list->topitem=0;
						 	}
						else list->topitem=list->selecteditem;

						_WID_Tempo(tk_display);
						if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL) 
						 SB_SetPosition(tk_display,list->SBV,list->topitem);

						if(old!=list->topitem) ls_DrawList(tk_display,listid);
						else
						{
						  ls_DrawItem(tk_display,listid,olds);
						  ls_DrawItem(tk_display,listid,list->selecteditem);
						}
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_CLICKED;
					}
					else return 0;
					break;


			  case XK_space:
			  case XK_Return: 
					if(list->numitems>0)
					{
						list->selected=True;
						if(list->selecteditem<0||list->selecteditem>=list->numitems)
							list->selecteditem=0;
						ls_DrawItem(tk_display,listid,list->selecteditem);
						if((list->items[list->selecteditem].state&Grayed)!=Grayed)
						  return LS_VALIDATION;
						else return 0;
					}
					else return 0;
					break;
		
			  case XK_Tab:	if(list->parency_class!=WI_COMBO){
					 /*fprintf(stderr,"index: %d\n",index);*/
					 if(index==0) WID_SeekNextFocus(tk_display,list->mainwindow,list->parent,list->top_level,UP+DOWN+INCRUST);
					 if(index==1) WID_SeekPreviousFocus(tk_display,list->mainwindow,list->parent,list->top_level,UP+DOWN+INCRUST);
					}
					else{ 
						if(index==0) LS_Unmap(tk_display,listid);
						return LS_KEYUNKNOWN;
					    }
					break;

			  default:  if(key>=32 && key<=175){ 
					bn_KeyFocus(tk_display,list->top_level,key,&tk_event->event);
					return 0;
					}
					else {  if(tk_event->event.xkey.keycode!=52&&tk_event->event.xkey.keycode!=66&&tk_event->event.xkey.keycode!=68&&tk_event->event.xkey.keycode!=38&&tk_event->event.xkey.keycode!=65)
					 if(tk_event->event.xkey.keycode!=70&&tk_event->event.xkey.keycode!=72&&tk_event->event.xkey.keycode!=98)
					   return LS_KEYUNKNOWN;
					else return 0;
					}
					break;
			    
			}
			return 0;
			break;



		case FocusIn: 

			if(tk_event->event.xfocus.detail==NotifyAncestor||tk_event->event.xfocus.detail==NotifyInferior||tk_event->event.xfocus.detail==NotifyNonlinear)
			{
			 ptr=(unsigned long)list->listwindow;
			 if(list->parency_class==0) XChangeProperty(tk_display->display,list->top_level,tk_display->atoms._IMAN_WM_FOCUS,XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);

			 list->hasFocus=True;
			 XAutoRepeatOn(tk_display->display);
			 if(list->selected==True) ls_DrawItem(tk_display,listid,list->selecteditem);
			 if((list->flags&NoBorderFlag)!=NoBorderFlag)
			   _LS_DrawFocus(tk_display,listid,ON);
			 /*if(list->parency_class<=0) ptr=(long)(list->listwindow);
			 else if(list->parency_class==WI_COMBO) 
			 {
				combo=(ComboStruct *)tk_display->widgets[list->parency_number].combo;
				ptr=(long)combo->edit->window;
			 }*/

			}
			return 0;
			break;

		case FocusOut:

			ret=0;
			if(tk_event->event.xfocus.detail==NotifyAncestor||tk_event->event.xfocus.detail==NotifyInferior||tk_event->event.xfocus.detail==NotifyNonlinear)
			{
			  if(list->parency_class!=WI_COMBO)
			  {
			 	list->hasFocus=False;
			 	if(list->selected==True) ls_DrawItem(tk_display,listid,list->selecteditem);
			 	  _LS_DrawFocus(tk_display,listid,OFF);
			 	list->hasFocus=False;
			  }
			  else 
			  {
			 	list->hasFocus=False;
			 	if(list->selected==True) ls_DrawItem(tk_display,listid,list->selecteditem);
			 	combo=(ComboStruct *)tk_display->widgets[list->parency_number].combo;
			 	edit=tk_display->widgets[combo->edit].edit;
			 	XGetInputFocus(tk_display->display,&if_win,&if_revert);
			 	if(edit->window!=if_win)
				{
			  	  ret=XLIBEVENT;
				  combo->isOpen=False;
			    	}
			 	else ret=0;
			  }
			}
			return ret;
			break;

		default: return 0;

		}
		return 0;
		break;


	default: return 0;

 }
 return 0;
}





