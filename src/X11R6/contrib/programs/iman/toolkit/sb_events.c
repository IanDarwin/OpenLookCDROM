/*
 *
 * 	sb_events.c  
 * 	evenements des ascenseurs
 *
 * 	Modification :  02/01/94
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
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

#include <X11/iman/widgets.h>






		  /****** Evenements des SCROLLBARs *******/


int sb_ScrollbarEvents(tk_display,scrollid,tk_event)
TkDisplay *tk_display;
TkEvent *tk_event;
ScrollbarID scrollid;
{
 int pos, ret, i;
 unsigned long ptr;
 XEvent send_event;
 KeySym key;
 int index;
 float fl;
 int mask, new;
 GC gc;
 XGCValues xgcvalues;
 XEvent ev;
 Window w, r;
 int xr, yr, xp, yp, xk;
 ScrollbarStruct *scroll;
 ButtonStruct *B1, *B2, *bn_thumb;
 ComboStruct *combo;
 WidgetStruct *widptr;


 /*fprintf(stderr,"SB event recu\n");*/

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;
 widptr=&tk_display->widgets[scrollid];

 B1=tk_display->widgets[scroll->B1].button;
 B2=tk_display->widgets[scroll->B2].button;
 bn_thumb=tk_display->widgets[scroll->bn_thumb].button;

 /*fprintf(stderr,"Event %d\n",tk_event->ev_type);*/

 switch(tk_event->ev_type){

	case JUSTFOCUS: return JUSTFOCUS;
			break;


	case BN_PUSHED :
			/*fprintf(stderr,"Scroll BN_PUSHED %d\n",tk_event->button);*/
			if(tk_event->button==scroll->B1 && scroll->position>0)
			{
			  scroll->oldposition=scroll->position;
			  pos=scroll->position;
			  SB_SetPosition(tk_display,scrollid,pos-1);
			  if(scroll->position!=pos){
				if(scroll->type==SB_TOPALIGN ||scroll->type==SB_BOTTOMALIGN) 
				   return SB_UP;
				if(scroll->type==SB_LEFTALIGN ||scroll->type==SB_RIGHTALIGN) 
				   return SB_LEFT;
				else return JUSTFOCUS;
			   }
			   else return JUSTFOCUS;
			}

			else if(tk_event->button==scroll->B2 && scroll->position<scroll->range)
			{
			  scroll->oldposition=scroll->position;
			  pos=scroll->position;
			  SB_SetPosition(tk_display,scrollid,pos+1);
			  if(scroll->position!=pos){
				if(scroll->type==SB_TOPALIGN ||scroll->type==SB_BOTTOMALIGN) 
				   return SB_DOWN;
				if(scroll->type==SB_LEFTALIGN ||scroll->type==SB_RIGHTALIGN) 
				   return SB_RIGHT;
				else return JUSTFOCUS;
			   }
			   else return JUSTFOCUS;
			}

			else if(tk_event->button==scroll->bn_thumb)
			{
			    mask=GCForeground|GCBackground|GCFunction|GCSubwindowMode;
			    xgcvalues.foreground=widptr->colors->nofocus;/**/
			    xgcvalues.background=widptr->colors->nofocus;
		  	    xgcvalues.function=GXxor;
			    xgcvalues.subwindow_mode=IncludeInferiors;
 			    gc=XCreateGC(tk_display->display,scroll->thumbwindow,mask,&xgcvalues);

			    if(scroll->type==SB_VTHUMB||scroll->type==SB_TOPALIGN||scroll->type==SB_BOTTOMALIGN)
				scroll->gap=tk_event->event.xbutton.y;
			    else scroll->gap=tk_event->event.xbutton.x;
			    

			    if(scroll->type==SB_VTHUMB||scroll->type==SB_TOPALIGN||scroll->type==SB_BOTTOMALIGN)
			    {
			    	XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,0,bn_thumb->y,bn_thumb->width+1,bn_thumb->height+1);
				scroll->oldposition=scroll->position;
				scroll->repeat=bn_thumb->y; 
			    }
			    else{ 
				XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,bn_thumb->x,0,bn_thumb->width+1,bn_thumb->height+1);
			    	scroll->oldposition=scroll->position;
				scroll->repeat=bn_thumb->x;
			    }
  			    XFreeGC(tk_display->display,gc);  
			    return JUSTFOCUS;
			}

			else return JUSTFOCUS;
			break;


	case BN_MOVED :
			mask=GCForeground|GCBackground|GCFunction|GCSubwindowMode;
			xgcvalues.foreground=widptr->colors->nofocus;/**/
			xgcvalues.background=widptr->colors->nofocus;
			xgcvalues.function=GXxor;
			xgcvalues.subwindow_mode=IncludeInferiors;
			gc=XCreateGC(tk_display->display,scroll->thumbwindow,mask,&xgcvalues);

			if(scroll->type==SB_TOPALIGN||scroll->type==SB_BOTTOMALIGN)
			{
			   ret=0;
			   new=bn_thumb->y+tk_event->event.xmotion.y-scroll->gap;

			   if(new!=scroll->repeat)
			   {
				XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,0,scroll->repeat,bn_thumb->width+1,bn_thumb->height+1);
				if(new<=scroll->height-36-2-scroll->thumbsize&&new>=0) 
				  scroll->repeat=new;
				else
				{
				  	if(new<0) new=scroll->repeat=0; 
					else if(new>scroll->height-36-2-scroll->thumbsize) 
					  new=scroll->repeat=scroll->height-36-2-scroll->thumbsize;
				}
				fl=(float)(((float)new*(float)(scroll->range))/(float)(scroll->height-36-2-scroll->thumbsize));
				if((int)fl<0) fl=0;
				else if((int)fl>scroll->range) fl=scroll->range;

				if(scroll->position!=(int)fl)
				{
				  if((int)fl<=scroll->range&&(int)fl>=0) scroll->position=(int)fl;
				  else if((int)fl<0) scroll->position=0;
				  else if((int)fl>scroll->range) scroll->position=scroll->range;
				  ret=SB_THUMBMOVED;
				}
				XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,0,scroll->repeat,bn_thumb->width+1,bn_thumb->height+1);
				return ret;
			    }
			    else return 0;
			}

			if(scroll->type==SB_VTHUMB)
			{
			   ret=0;
			   new=bn_thumb->y+tk_event->event.xmotion.y-scroll->gap;
			   if(new!=scroll->repeat)
			   {
				XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,0,scroll->repeat,bn_thumb->width+1,bn_thumb->height+1);
				
/****/				if(new<=scroll->height-3-scroll->thumbsize&&new>=0) 
				  scroll->repeat=new;
				else
				{
				  	if(new<0) new=scroll->repeat=0; 
				  	else if(new>scroll->height-2-scroll->thumbsize) new=scroll->repeat=scroll->height-2-scroll->thumbsize;
				}
				fl=(float)(((float)new*(float)(scroll->range))/(float)(scroll->height-2-scroll->thumbsize));
				if((int)fl<0) fl=0;
				else if((int)fl>scroll->range) fl=scroll->range;

				if(scroll->position!=(int)fl)
				{
				  if((int)fl<=scroll->range&&(int)fl>=0) scroll->position=(int)fl;
				  else if((int)fl<0) scroll->position=0;
				  else if((int)fl>scroll->range) scroll->position=scroll->range;
				  ret=SB_THUMBMOVED;
				}
				XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,0,scroll->repeat,bn_thumb->width+1,bn_thumb->height+1);
				return ret;
			    }
			    else return 0;
			}

			else if(scroll->type==SB_LEFTALIGN||scroll->type==SB_RIGHTALIGN)
			{
			   ret=0;
			   new=bn_thumb->x+tk_event->event.xmotion.x-scroll->gap;
			   if(new!=scroll->repeat)
			   {
				XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,scroll->repeat,0,bn_thumb->width+1,bn_thumb->height+1);
				
				if(new<=scroll->width-38-2-scroll->thumbsize&&new>=0) 
				  scroll->repeat=new;
				else
				{
				  	if(new<0) new=scroll->repeat=0; 
				  	else if(new>scroll->width-38-2-scroll->thumbsize) new=scroll->repeat=scroll->width-38-2-scroll->thumbsize;
				}
				fl=((float)new*(float)(scroll->range))/(float)(scroll->width-38-2-scroll->thumbsize);
				if((int)fl<0) fl=0;
				else if((int)fl>scroll->range) fl=scroll->range;
	
				if(scroll->position!=(int)fl)
				{
				  	if((int)fl<=scroll->range&&(int)fl>=0) scroll->position=(int)fl;
				  	else if((int)fl<0) scroll->position=0;
				  	else if((int)fl>scroll->range) scroll->position=scroll->range;
				  	ret=SB_THUMBMOVED;
				}
				XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,scroll->repeat,0,bn_thumb->width+1,bn_thumb->height+1);
				return ret;
			    }
			   else return 0;
			}


			else if(scroll->type==SB_HTHUMB){
			   ret=0;
			   new=bn_thumb->x+tk_event->event.xmotion.x-scroll->gap;
			   if(new!=scroll->repeat)
			   {
				XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,scroll->repeat,0,bn_thumb->width+1,bn_thumb->height+1);
				
				if(new<=scroll->width-2-scroll->thumbsize&&new>=0) 
				  scroll->repeat=new;
				else{
				  	if(new<0) new=scroll->repeat=0; 
				  	else if(new>scroll->width-2-scroll->thumbsize) new=scroll->repeat=scroll->width-2-scroll->thumbsize;
				}
				fl=((float)new*(float)(scroll->range))/(float)(scroll->width-2-scroll->thumbsize);
				if((int)fl<0) fl=0;
				else if((int)fl>scroll->range) fl=scroll->range;
	
				if(scroll->position!=(int)fl)
				{
				  if((int)fl<=scroll->range&&(int)fl>=0) scroll->position=(int)fl;
				  else if((int)fl<0) scroll->position=0;
				  else if((int)fl>scroll->range) scroll->position=scroll->range;
				  ret=SB_THUMBMOVED;
				}
				XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,scroll->repeat,0,bn_thumb->width+1,bn_thumb->height+1);
				return ret;
			    }
			   else return 0;
			}


   			XFreeGC(tk_display->display,gc);  
			break;


	case BN_RELEASED : 
			if(tk_event->button==scroll->bn_thumb)
			{
				ret=0;
			  	mask=GCForeground|GCBackground|GCFunction|GCSubwindowMode;
				xgcvalues.foreground=widptr->colors->nofocus;/**/
				xgcvalues.background=widptr->colors->nofocus;
				xgcvalues.function=GXxor;
				xgcvalues.subwindow_mode=IncludeInferiors;
				gc=XCreateGC(tk_display->display,scroll->thumbwindow,mask,&xgcvalues);

				if(scroll->type==SB_VTHUMB||scroll->type==SB_TOPALIGN||scroll->type==SB_BOTTOMALIGN)
				  XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,0,scroll->repeat,bn_thumb->width+1,bn_thumb->height+1);
				else XDrawRectangle(tk_display->display,scroll->thumbwindow,gc,scroll->repeat,0,bn_thumb->width+1,bn_thumb->height+1);

				if(scroll->position!=scroll->oldposition) ret=SB_STATUS;
				else ret=0;

				index=scroll->position;
				scroll->repeat=0;
				scroll->position=scroll->oldposition;
				SB_SetPosition(tk_display,scrollid,index);
				XFreeGC(tk_display->display,gc);  
				return ret;
			}
			else{
			 /*if(scroll->position!=scroll->oldposition) return SB_STATUS;
			 else*/ return SB_STATUS;
			}
			break;

	case XLIBEVENT :

	
	      if((scroll->state&Frozen)!=Frozen&&(tk_event->event.type==ButtonPress||tk_event->event.type==ButtonRelease))
	      {
			if(tk_event->event.type==ButtonPress&&(scroll->state&Grayed)!=Grayed&&tk_event->event.xbutton.button==Button1)
			{
			 
			 if(scroll->repeat==0) XMapRaised(tk_display->display,scroll->top_level);

			 if(tk_display->action.type==ComboSelectingAction) 
		 	 {
		    	   combo=tk_display->widgets[tk_display->action.combo].combo; 	
		    	   if(scroll->parency_class<=0||WID_IsSubwidget(tk_display,scrollid,tk_display->action.combo)==False)
		    	   {
		   		combo->isOpen=False;
		   		if(scroll->parency_class<=0) XSetInputFocus(tk_display->display,tk_display->widgets[combo->edit].edit->window,RevertToPointerRoot,CurrentTime); 
		   		LS_Unmap(tk_display,combo->list);
		   		memset(&tk_display->action,0,sizeof(ActionStruct));
		   		tk_display->action.combo=tk_display->action.menu=-1;
		    	   }		    
		  	 }
			 
			 XClearWindow(tk_display->display, scroll->thumbwindow);
			 
			 if(scroll->repeat==0)
			 for(i=0;i<tk_display->widget_double_click;i++)
			   i=i;

			 if(scroll->type==SB_TOPALIGN||scroll->type==SB_BOTTOMALIGN)
			 { 
			   if(scroll->repeat==0)
			   {
				if(tk_event->event.xbutton.y<bn_thumb->y && tk_event->event.xbutton.y>=0 && tk_event->event.xbutton.x<scroll->width && tk_event->event.xbutton.x>=0)
				  scroll->repeat=1;
				else scroll->repeat=2;
			   }
			   
			   mask=GCForeground|GCBackground;
			   xgcvalues.foreground=widptr->colors->light;/**/
			   xgcvalues.background=widptr->colors->bg;  
		  	   xgcvalues.function=GXxor;
     			   gc=XCreateGC(tk_display->display,scroll->thumbwindow,mask,&xgcvalues);

			   if(scroll->repeat==1&&tk_event->event.xbutton.y<bn_thumb->y && tk_event->event.xbutton.y>=0 &&tk_event->event.xbutton.x<scroll->width &&tk_event->event.xbutton.x>=0) 
			     XFillRectangle(tk_display->display,scroll->thumbwindow,gc,0,0,scroll->width,bn_thumb->y);

			   else if(scroll->repeat==2&&tk_event->event.xbutton.y>=bn_thumb->y+bn_thumb->height+1&&tk_event->event.xbutton.y<=scroll->height-36&&tk_event->event.xbutton.x<scroll->width&&tk_event->event.xbutton.x>=0) 
			     XFillRectangle(tk_display->display,scroll->thumbwindow,gc,0,bn_thumb->y,scroll->width,scroll->height);

			   else scroll->repeat=0;

     			   XFreeGC(tk_display->display,gc);

			   XQueryPointer(tk_display->display,scroll->thumbwindow,&w,&r,&xr,&yr,&xp,&yp,&xk);
     			   tk_event->event.xbutton.x=xp;
     			   tk_event->event.xbutton.y=yp;
		           tk_event->event.xbutton.state=xk;

			   ptr=0;
			   while(ptr<60000) ptr++;

     			   if (XCheckWindowEvent(tk_display->display,scroll->thumbwindow,ButtonReleaseMask,&ev)==True)
     			    {	
				tk_event->event.type=ButtonRelease;
				XPutBackEvent(tk_display->display,&tk_event->event);
				if(scroll->repeat==1) scroll->repeat=4; 	
				else if(scroll->repeat==2) scroll->repeat=5;
				else scroll->repeat=3;
			    }
			
			   if(scroll->repeat==3 || scroll->repeat==0) 
			   {
				return 0;
			   }
			   else
			   {
				if(scroll->repeat==4){
					SB_SetPosition(tk_display,scrollid,scroll->position-scroll->pagerange);
					return SB_PAGEUP;
					}
				else if(scroll->repeat==5){
					SB_SetPosition(tk_display,scrollid,scroll->position+scroll->pagerange);
					return SB_PAGEDOWN;
					}
				else if(scroll->repeat==1){
					SB_SetPosition(tk_display,scrollid,scroll->position-scroll->pagerange);
					tk_event->event.type=ButtonPress;
					XPutBackEvent(tk_display->display,&tk_event->event);
					return SB_PAGEUP;
					}
				else if(scroll->repeat==2){
					SB_SetPosition(tk_display,scrollid,scroll->position+scroll->pagerange);
					tk_event->event.type=ButtonPress;
					XPutBackEvent(tk_display->display,&tk_event->event);
					return SB_PAGEDOWN;
					}
			   }

			  }


			  else if(scroll->type==SB_VTHUMB)
			  { 
			   if(scroll->repeat==0){
				if(tk_event->event.xbutton.y<bn_thumb->y && tk_event->event.xbutton.y>=0 && tk_event->event.xbutton.x<scroll->width && tk_event->event.xbutton.x>=0)
				  scroll->repeat=1;
				else scroll->repeat=2;
			     }
			   
			   mask=GCForeground|GCBackground;
			   xgcvalues.foreground=widptr->colors->light;/**/
			   xgcvalues.background=widptr->colors->bg;
		  	   xgcvalues.function=GXxor;
     			   gc=XCreateGC(tk_display->display,scroll->thumbwindow,mask,&xgcvalues);

			   if(scroll->repeat==1&&tk_event->event.xbutton.y<bn_thumb->y && tk_event->event.xbutton.y>=0 &&tk_event->event.xbutton.x<scroll->width &&tk_event->event.xbutton.x>=0) 
			     XFillRectangle(tk_display->display,scroll->thumbwindow,gc,0,0,scroll->width,bn_thumb->y);

			   else if(scroll->repeat==2&&tk_event->event.xbutton.y>=bn_thumb->y+bn_thumb->height+1&&tk_event->event.xbutton.y<=scroll->height&&tk_event->event.xbutton.x<scroll->width&&tk_event->event.xbutton.x>=0) 
			     XFillRectangle(tk_display->display,scroll->thumbwindow,gc,0,bn_thumb->y,scroll->width,scroll->height);

			   else scroll->repeat=0;

     			   XFreeGC(tk_display->display,gc);

			   XQueryPointer(tk_display->display,scroll->thumbwindow,&w,&r,&xr,&yr,&xp,&yp,&xk);
     			   tk_event->event.xbutton.x=xp;
     			   tk_event->event.xbutton.y=yp;
		           tk_event->event.xbutton.state=xk;

     			   if (XCheckWindowEvent(tk_display->display,scroll->thumbwindow,ButtonReleaseMask,&ev)==True)
     			    {	
				tk_event->event.type=ButtonRelease;
				XPutBackEvent(tk_display->display,&tk_event->event);
				if(scroll->repeat==1) scroll->repeat=4; 	
				else if(scroll->repeat==2) scroll->repeat=5;
				else scroll->repeat=3;
			    }
			
			   if(scroll->repeat==3 || scroll->repeat==0) 
			   {
				return 0;
			   }
			   else
			   {
				if(scroll->repeat==4){
					SB_SetPosition(tk_display,scrollid,scroll->position-scroll->pagerange);
					return SB_PAGEUP;
					}
				else if(scroll->repeat==5){
					SB_SetPosition(tk_display,scrollid,scroll->position+scroll->pagerange);
					return SB_PAGEDOWN;
					}
				else if(scroll->repeat==1){
					SB_SetPosition(tk_display,scrollid,scroll->position-scroll->pagerange);
					tk_event->event.type=ButtonPress;
					XPutBackEvent(tk_display->display,&tk_event->event);
					return SB_PAGEUP;
					}
				else if(scroll->repeat==2){
					SB_SetPosition(tk_display,scrollid,scroll->position+scroll->pagerange);
					tk_event->event.type=ButtonPress;
					XPutBackEvent(tk_display->display,&tk_event->event);
					return SB_PAGEDOWN;
					}
			   }

			  }


			  else if(scroll->type==SB_LEFTALIGN||scroll->type==SB_RIGHTALIGN)
			  { 
			   if(scroll->repeat==0){
				if(tk_event->event.xbutton.x<bn_thumb->x && tk_event->event.xbutton.x>=0 && tk_event->event.xbutton.y<scroll->height && tk_event->event.xbutton.y>=0)
				  scroll->repeat=1;
				else scroll->repeat=2;
			     }
			   
			   mask=GCForeground|GCBackground;
			   xgcvalues.foreground=widptr->colors->light;/**/
			   xgcvalues.background=widptr->colors->bg;
		  	   xgcvalues.function=GXxor;
     			   gc=XCreateGC(tk_display->display,scroll->thumbwindow,mask,&xgcvalues);

			   if(scroll->repeat==1&&tk_event->event.xbutton.x<bn_thumb->x && tk_event->event.xbutton.x>=0 &&tk_event->event.xbutton.y<scroll->height &&tk_event->event.xbutton.y>=0) 
			     XFillRectangle(tk_display->display,scroll->thumbwindow,gc,0,0,bn_thumb->x,scroll->height);

			   else if(scroll->repeat==2&&tk_event->event.xbutton.x>=bn_thumb->x+bn_thumb->width+1&&tk_event->event.xbutton.x<=scroll->width-38&&tk_event->event.xbutton.y<scroll->height&&tk_event->event.xbutton.y>=0) 
			     XFillRectangle(tk_display->display,scroll->thumbwindow,gc,bn_thumb->x,0,scroll->width,scroll->height);

			   else scroll->repeat=0;

     			   XFreeGC(tk_display->display,gc);

			   XQueryPointer(tk_display->display,scroll->thumbwindow,&w,&r,&xr,&yr,&xp,&yp,&xk);
     			   tk_event->event.xbutton.x=xp;
     			   tk_event->event.xbutton.y=yp;
		           tk_event->event.xbutton.state=xk;

     			   if (XCheckWindowEvent(tk_display->display,scroll->thumbwindow,ButtonReleaseMask,&ev)==True)
     			    {	
				tk_event->event.type=ButtonRelease;
				XPutBackEvent(tk_display->display,&tk_event->event);
				if(scroll->repeat==1) scroll->repeat=4; 	
				else if(scroll->repeat==2) scroll->repeat=5;
				else scroll->repeat=3;
			    }
			
			   if(scroll->repeat==3 || scroll->repeat==0) 
			   {
				return 0;
			   }
			   else
			   {
				if(scroll->repeat==4){
					SB_SetPosition(tk_display,scrollid,scroll->position-scroll->pagerange);
					return SB_PAGELEFT;
					}
				else if(scroll->repeat==5){
					SB_SetPosition(tk_display,scrollid,scroll->position+scroll->pagerange);
					return SB_PAGERIGHT;
					}
				else if(scroll->repeat==1){
					SB_SetPosition(tk_display,scrollid,scroll->position-scroll->pagerange);
					tk_event->event.type=ButtonPress;
					XPutBackEvent(tk_display->display,&tk_event->event);
					return SB_PAGELEFT;
					}
				else if(scroll->repeat==2){
					SB_SetPosition(tk_display,scrollid,scroll->position+scroll->pagerange);
					tk_event->event.type=ButtonPress;
					XPutBackEvent(tk_display->display,&tk_event->event);
					return SB_PAGERIGHT;
					}
			   }

			  }


			  else if(scroll->type==SB_HTHUMB)
			  { 
			   if(scroll->repeat==0){
				if(tk_event->event.xbutton.x<bn_thumb->x && tk_event->event.xbutton.x>=0 && tk_event->event.xbutton.y<scroll->height && tk_event->event.xbutton.y>=0)
				  scroll->repeat=1;
				else scroll->repeat=2;
			     }
			   
			   mask=GCForeground|GCBackground;
			   xgcvalues.foreground=widptr->colors->light;/**/
			   xgcvalues.background=widptr->colors->bg;
		  	   xgcvalues.function=GXxor;
     			   gc=XCreateGC(tk_display->display,scroll->thumbwindow,mask,&xgcvalues);

			   if(scroll->repeat==1&&tk_event->event.xbutton.x<bn_thumb->x && tk_event->event.xbutton.x>=0 &&tk_event->event.xbutton.y<scroll->height &&tk_event->event.xbutton.y>=0) 
			     XFillRectangle(tk_display->display,scroll->thumbwindow,gc,0,0,bn_thumb->x,scroll->height);

			   else if(scroll->repeat==2&&tk_event->event.xbutton.x>=bn_thumb->x+bn_thumb->width+1&&tk_event->event.xbutton.x<=scroll->width-1&&tk_event->event.xbutton.y<scroll->height&&tk_event->event.xbutton.y>=0) 
			     XFillRectangle(tk_display->display,scroll->thumbwindow,gc,bn_thumb->x,0,scroll->width,scroll->height);

			   else scroll->repeat=0;

     			   XFreeGC(tk_display->display,gc);

			   XQueryPointer(tk_display->display,scroll->thumbwindow,&w,&r,&xr,&yr,&xp,&yp,&xk);
     			   tk_event->event.xbutton.x=xp;
     			   tk_event->event.xbutton.y=yp;
		           tk_event->event.xbutton.state=xk;

     			   if (XCheckWindowEvent(tk_display->display,scroll->thumbwindow,ButtonReleaseMask,&ev)==True)
     			    {	
				tk_event->event.type=ButtonRelease;
				XPutBackEvent(tk_display->display,&tk_event->event);
				if(scroll->repeat==1) scroll->repeat=4; 	
				else if(scroll->repeat==2) scroll->repeat=5;
				else scroll->repeat=3;
			    }
			
			   if(scroll->repeat==3 || scroll->repeat==0) 
			   {
				return 0;
			   }
			   else
			   {
				if(scroll->repeat==4){
					SB_SetPosition(tk_display,scrollid,scroll->position-scroll->pagerange);
					return SB_PAGELEFT;
					}
				else if(scroll->repeat==5){
					SB_SetPosition(tk_display,scrollid,scroll->position+scroll->pagerange);
					return SB_PAGERIGHT;
					}
				else if(scroll->repeat==1){
					SB_SetPosition(tk_display,scrollid,scroll->position-scroll->pagerange);
					tk_event->event.type=ButtonPress;
					XPutBackEvent(tk_display->display,&tk_event->event);
					return SB_PAGELEFT;
					}
				else if(scroll->repeat==2){
					SB_SetPosition(tk_display,scrollid,scroll->position+scroll->pagerange);
					tk_event->event.type=ButtonPress;
					XPutBackEvent(tk_display->display,&tk_event->event);
					return SB_PAGERIGHT;
					}
			   }

			  }
			  
			  else return 0;
			 
			}
			else if(tk_event->event.type==ButtonPress) 	
 			  XMapRaised(tk_display->display,scroll->top_level);

			else if(tk_event->event.type==ButtonRelease && tk_event->event.xbutton.window==scroll->thumbwindow&&(scroll->state&Grayed)!=Grayed&&scroll->repeat!=0)
			{
			 scroll->repeat=0;
			 XClearWindow(tk_display->display,tk_event->event.xbutton.window);
			 return SB_STATUS;
			}

			else return JUSTFOCUS;
		  }

		break;


	default : return 0;
			break;

 }

}


