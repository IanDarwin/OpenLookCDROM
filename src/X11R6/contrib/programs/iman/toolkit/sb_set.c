/*
 *
 * 	sb_set.c  
 * 	modification des ascenseurs
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
 *      IMAN Development Toolkit version 1.2
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






int SB_SetPosition(tk_display,scrollid,position)
TkDisplay *tk_display;
int position;
ScrollbarID scrollid;
{
 float fl;
 ScrollbarStruct *scroll;
 ButtonStruct *B1, *B2, *bn_thumb;


 /*fprintf(stderr,"SB Setposition %d  pos=%d\n",scrollid,position);*/

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 B1=tk_display->widgets[scroll->B1].button;
 B2=tk_display->widgets[scroll->B2].button;
 bn_thumb=tk_display->widgets[scroll->bn_thumb].button;



 switch(scroll->type)
 {
	case SB_TOPALIGN:
	case SB_BOTTOMALIGN:
	
		
		XClearWindow(tk_display->display, scroll->thumbwindow);
		if(position<=scroll->range && position>=0 && scroll->range>0)
		{
		 scroll->position=position;
		 fl=(float)((float)position/(float)(scroll->range))*(scroll->height-36-2-scroll->thumbsize);
		 bn_thumb->y=(int)fl;
		 XMoveWindow(tk_display->display,bn_thumb->window,0,bn_thumb->y);
		 
		}
		else if(position>scroll->range && scroll->range>0)
		{ 
		 position=scroll->position=scroll->range; 
		 fl=(float)((float)position/(float)(scroll->range))*(scroll->height-36-2-scroll->thumbsize);
		 bn_thumb->y=(int)fl;
		 XMoveWindow(tk_display->display,bn_thumb->window,0,bn_thumb->y);
		 
		}
		else if(position<0 && scroll->range>0) 
		{
		 position=scroll->position=0; 
		 fl=(float)((float)position/(float)(scroll->range))*(scroll->height-36-2-scroll->thumbsize);
		 bn_thumb->y=(int)fl;
		 XMoveWindow(tk_display->display,bn_thumb->window,0,bn_thumb->y);
		 
		}
		else if(scroll->range<=0)
		{
		 scroll->range=0;
		 scroll->position=0;
		 bn_thumb->y=0;
		 XMoveWindow(tk_display->display,bn_thumb->window,0,bn_thumb->y);
		}

		break;


	case SB_VTHUMB:
		
		XClearWindow(tk_display->display, scroll->thumbwindow);
		if(position<=scroll->range && position>=0/* && (bn_thumb->y+scroll->thumbsize)<=scroll->height-2*/ && scroll->range>0)
		{
		
		scroll->position=position;
		fl=(float)((float)position/(float)(scroll->range))*(scroll->height-2-scroll->thumbsize);
		bn_thumb->y=(int)fl;
		XMoveWindow(tk_display->display,bn_thumb->window,0,bn_thumb->y);
		
		}
		else if(position>scroll->range && scroll->range>0)
		{ 
		
		position=scroll->position=scroll->range; 
		fl=(float)((float)position/(float)(scroll->range))*(scroll->height-2-scroll->thumbsize);
		bn_thumb->y=(int)fl;
		XMoveWindow(tk_display->display,bn_thumb->window,0,bn_thumb->y);
		
		}
		else if(position<0 && scroll->range>0) 
		{
		
		position=scroll->position=0; 
		fl=(float)((float)position/(float)(scroll->range))*(scroll->height-2-scroll->thumbsize);
		bn_thumb->y=(int)fl;
		XMoveWindow(tk_display->display,bn_thumb->window,0,bn_thumb->y);
		}
		else if(scroll->range<=0)
		{
		 scroll->range=0;
		 scroll->position=0;
		 bn_thumb->y=0;
		 XMoveWindow(tk_display->display,bn_thumb->window,0,bn_thumb->y);
		}

		else fprintf(stderr,"VTHUMB: erreur\n");

		break;

	case SB_LEFTALIGN:
	case SB_RIGHTALIGN:
	
		XClearWindow(tk_display->display, scroll->thumbwindow);
		if(position<=scroll->range && position>=0 && scroll->range>0)
		{
		
		scroll->position=position;
		fl=(float)((float)position/(float)(scroll->range))*(scroll->width-38-2-scroll->thumbsize);
		bn_thumb->x=(int)fl;
		XMoveWindow(tk_display->display,bn_thumb->window,bn_thumb->x,0);
		
		}
		else if(position>scroll->range && scroll->range>0)
		{ 
		
		position=scroll->position=scroll->range; 
		fl=(float)((float)position/(float)(scroll->range))*(scroll->width-38-2-scroll->thumbsize);
		bn_thumb->x=(int)fl;
		XMoveWindow(tk_display->display,bn_thumb->window,bn_thumb->x,0);
		
		}
		else if(position<0 && scroll->range>0) 
		{
	
		position=scroll->position=0; 
		fl=(float)((float)position/(float)(scroll->range))*(scroll->width-38-2-scroll->thumbsize);
		bn_thumb->x=(int)fl;
		XMoveWindow(tk_display->display,bn_thumb->window,bn_thumb->x,0);
		}
		else if(scroll->range<=0)
		{
		 scroll->range=0;
		 scroll->position=0;
		 bn_thumb->x=0;
		 XMoveWindow(tk_display->display,bn_thumb->window,bn_thumb->x,0);
		}
		break;

	case SB_HTHUMB:
		
		XClearWindow(tk_display->display, scroll->thumbwindow);
		if(position<=scroll->range && position>=0/* && (bn_thumb->x+scroll->thumbsize)<=scroll->width-2*/ && scroll->range>0)
		{
		
		scroll->position=position;
		fl=(float)((float)position/(float)(scroll->range))*(scroll->width-2-scroll->thumbsize);
		bn_thumb->x=(int)fl;
		XMoveWindow(tk_display->display,bn_thumb->window,bn_thumb->x,0);
		
		}
		else if(position>scroll->range && scroll->range>0)
		{ 
		
		position=scroll->position=scroll->range; 
		fl=(float)((float)position/(float)(scroll->range))*(scroll->width-2-scroll->thumbsize);
		bn_thumb->x=(int)fl;
		XMoveWindow(tk_display->display,bn_thumb->window,bn_thumb->x,0);
		
		}
		else if(position<0 && scroll->range>0)
		{
		
		position=scroll->position=0; 
		fl=(float)((float)position/(float)(scroll->range))*(scroll->width-2-scroll->thumbsize);
		bn_thumb->x=(int)fl;
		XMoveWindow(tk_display->display,bn_thumb->window,bn_thumb->x,0);
		}
		else if(scroll->range<=0)
		{
		 scroll->range=0;
		 scroll->position=0;
		 bn_thumb->x=0;
		 XMoveWindow(tk_display->display,bn_thumb->window,bn_thumb->x,0);
		}
		else fprintf(stderr,"HThumb: erreur\n");		
		break;


 }
 /*fprintf(stderr,"SB setposition finie\n");*/
 return 0;
}





int SB_SetRange(tk_display,scrollid,range)
TkDisplay *tk_display;
int range;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;


 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;
 
 if(range>=0){
	scroll->range=range;
	SB_SetPosition(tk_display,scrollid,scroll->position);
	return 0;
	}
 else return -1;

}





int SB_SetPagerange(tk_display,scrollid,pagerange)
TkDisplay *tk_display;
int pagerange;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;


 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 if(pagerange>=0){
	scroll->pagerange=pagerange;
	return 0;
	}
 else return -1;

}





int SB_SetThumbsize(tk_display,scrollid,thumbsize)
TkDisplay *tk_display;
int thumbsize;
ScrollbarID scrollid;
{
 XWindowChanges xwchanges;
 int mask;
 ScrollbarStruct *scroll;
 ButtonStruct *B1, *B2, *bn_thumb;


 /*fprintf(stderr,"SB set thumb size start %d\n",scrollid);*/

 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 /*fprintf(stderr,"SB set thumb size SUITE \n");*/

 B1=tk_display->widgets[scroll->B1].button;
 B2=tk_display->widgets[scroll->B2].button;
 bn_thumb=tk_display->widgets[scroll->bn_thumb].button;

 
 if(thumbsize>=16){
	scroll->thumbsize=thumbsize;
	if(scroll->type==SB_VTHUMB||scroll->type==SB_TOPALIGN||scroll->type==SB_BOTTOMALIGN)
	{
	  BN_Configure(tk_display,scroll->bn_thumb,bn_thumb->x,bn_thumb->y,bn_thumb->width,thumbsize);
	  SB_SetPosition(tk_display,scrollid,scroll->position);	  
	  /*fprintf(stderr,"SB set thumb size FIN \n");*/
	  return 0;
	} 
	else{
	  BN_Configure(tk_display,scroll->bn_thumb,bn_thumb->x,bn_thumb->y,thumbsize,bn_thumb->height);
	  SB_SetPosition(tk_display,scrollid,scroll->position);
	  /*fprintf(stderr,"SB set thumb size FIN \n");*/
	  return 0;	
	}
  }
 else return -1;

}






int SB_Gray(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;


 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

 if((scroll->state&Grayed)!=Grayed) scroll->state=scroll->state+Grayed; 
 BN_Gray(tk_display,scroll->B1);
 BN_Gray(tk_display,scroll->B2);
 BN_Gray(tk_display,scroll->bn_thumb);
 return scroll->state;

}






int SB_Ungray(tk_display,scrollid)
TkDisplay *tk_display;
ScrollbarID scrollid;
{
 ScrollbarStruct *scroll;


 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;


 if((scroll->state&Grayed)==Grayed)
 {
   scroll->state=scroll->state-Grayed; 
   BN_Ungray(tk_display,scroll->B1);
   BN_Ungray(tk_display,scroll->B2);
   BN_Ungray(tk_display,scroll->bn_thumb);
 }
 return scroll->state;
}





int SB_Configure(tk_display,scrollid,x,y,width,height)
TkDisplay *tk_display;
ScrollbarID scrollid;
int x, y;
unsigned int width, height;
{
 int mask, oldw, oldh;
 XWindowChanges xwc;
 ScrollbarStruct *scroll;


 if(scrollid>=0 && scrollid<tk_display->maxwidgets && tk_display->widgets[scrollid].class==WI_SCROLLBAR&&tk_display->widgets[scrollid].scroll!=NULL)
   scroll=tk_display->widgets[scrollid].scroll;
 else return -1;

/*
 if(scroll->type!=SB_HTHUMB && scroll->type!=SB_VTHUMB) BN_Map(tk_display,scroll->B1);
 if(scroll->type!=SB_HTHUMB && scroll->type!=SB_VTHUMB) BN_Map(tk_display,scroll->B2);
 XMapWindow(tk_display->display, scroll->thumbwindow);			
 BN_Map(tk_display,scroll->bn_thumb); 
*/ 

 oldw=scroll->width;
 oldh=scroll->height;

 xwc.x=scroll->x=x;
 xwc.y=scroll->y=y;
 xwc.width=scroll->width=width;
 xwc.height=scroll->height=height;
 mask=CWX|CWY|CWWidth|CWHeight;

 XConfigureWindow(tk_display->display,scroll->mainwindow,mask,&xwc);
 
 switch(scroll->type){

	case SB_TOPALIGN:

		xwc.width=width;
		xwc.height=height-36;
		mask=CWWidth|CWHeight;
		XConfigureWindow(tk_display->display,scroll->thumbwindow,mask,&xwc);
		BN_Configure(tk_display,scroll->B1,0,0,width-2,16);
		BN_Configure(tk_display,scroll->B2,0,18,width-2,16);
		SB_SetPosition(tk_display,scrollid,scroll->position);
		
		break;

	case SB_BOTTOMALIGN:

		xwc.width=width;
		xwc.height=height-36;
		mask=CWWidth|CWHeight;
		XConfigureWindow(tk_display->display,scroll->thumbwindow,mask,&xwc);
		BN_Configure(tk_display,scroll->B1,0,height-36,width-2,16);
		BN_Configure(tk_display,scroll->B2,0,height-18,width-2,16);
		SB_SetPosition(tk_display,scrollid,scroll->position);
		
		break;

	case SB_VTHUMB:

		xwc.width=width;
		xwc.height=height;
		mask=CWWidth|CWHeight;
		/**  A REVOIR  **/
		XResizeWindow(tk_display->display,scroll->thumbwindow,width,height);
		SB_SetPosition(tk_display,scrollid,scroll->position);
		break;

	case SB_LEFTALIGN:

		xwc.width=width-38;
		xwc.height=height;
		mask=CWWidth|CWHeight;
		XConfigureWindow(tk_display->display,scroll->thumbwindow,mask,&xwc);
		BN_Configure(tk_display,scroll->B1,0,0,17,height-2); 
		BN_Configure(tk_display,scroll->B2,19,0,17,height-2);
		SB_SetPosition(tk_display,scrollid,scroll->position);

		break;

	case SB_RIGHTALIGN:

		xwc.width=width-38;
		xwc.height=height;
		mask=CWWidth|CWHeight;
		XConfigureWindow(tk_display->display,scroll->thumbwindow,mask,&xwc);
		BN_Configure(tk_display,scroll->B1,width-38,0,17,height-2);
		BN_Configure(tk_display,scroll->B2,width-19,0,17,height-2);
		/*BN_Configure(tk_display,scroll->bn_thumb,bn_thumb->x,0,scroll->thumbsize,height-2);
		SB_SetThumbsize(tk_display,scrollid,width*scroll->thumbsize/oldw);*/
		SB_SetPosition(tk_display,scrollid,scroll->position);
		
		break;

	case SB_HTHUMB:

		xwc.width=width;
		xwc.height=height;
		mask=CWWidth|CWHeight;
		XConfigureWindow(tk_display->display,scroll->thumbwindow,mask,&xwc);
		/*BN_Configure(tk_display,scroll->bn_thumb,bn_thumb->x,0,scroll->thumbsize,height-2);*/
		SB_SetPosition(tk_display,scrollid,scroll->position);
		/*SB_SetThumbsize(tk_display,scrollid,width*scroll->thumbsize/oldw);*/
		break;


 }
 return 0;
}



