/*
 *
 * 	bn_create.c  
 * 	creation des boutons 
 *
 * 	Modification :  26/12/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell  this software and its
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

#include <X11/iman/widgets.h>







			/**** Fonction generique ****/


ButtonID BN_Create(tk_display,type,wd_parent,wd_top_level,x,y,width,height,text,key,state)
TkDisplay *tk_display;
int type;
Window wd_parent, wd_top_level;
int x, y, width, height,key;
char *text;
unsigned int state;
{
 ButtonID button;


 switch(type){

	case BN_PUSHBUTTON: bn_CreatePushButton(tk_display, button, wd_parent , wd_top_level, x ,y ,width , height, text, key, state); 
				break;

	case BN_REPEATBUTTON: bn_CreateRepeatButton(tk_display, button, wd_parent ,wd_top_level,x ,y ,width , height, text, key, state); 
				break;

	case BN_CROSSBUTTON: bn_CreateCrossButton(tk_display, button, wd_parent, wd_top_level, x ,y , width, height, text,state); 
				break;

	case BN_POPUPBUTTON: bn_CreatePopupButton(tk_display, button, wd_parent ,wd_top_level,x ,y ,width , height, text, key, state);
				break;

	case BN_RADIOBUTTON: bn_CreateRadioButton(tk_display, button, wd_parent, wd_top_level, x ,y , width, height, text,state);
				break;

	case BN_CHECKBUTTON: bn_CreateCheckButton(tk_display,button,wd_parent, wd_top_level, x ,y , width, height, text,state);
				break;

	case BN_POPUPRADIOBUTTON: bn_CreatePopupRadioButton(tk_display,button,wd_parent, wd_top_level, x ,y , width, height, text,state);
				break;

	default: return -1;
		 break;

 }
 return button;
}




			/******* PUSH BUTTON *******/


int bn_CreatePushButton( tk_display, buttonid, wd_parent , wd_top_level, x ,y ,width , height, text, key, state) 
TkDisplay *tk_display;
ButtonID buttonid;
Window wd_parent, wd_top_level;
int x, y, key, state;
unsigned int width, height;
char *text;
{
 ButtonStruct *button;
 XSetWindowAttributes attrib;
 unsigned long mask;



 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
#ifdef DEBUG
 fprintf(stderr,"BN window bientot\n");
#endif
 attrib.background_pixel=tk_display->bn_colors.bg;
 attrib.border_pixel=tk_display->bn_colors.nofocus;
 attrib.cursor=tk_display->widgets[buttonid].identity.cursor;
 attrib.do_not_propagate_mask=FocusChangeMask;
 attrib.event_mask=ExposureMask | KeyPressMask | KeyReleaseMask |ButtonPressMask|ButtonReleaseMask | StructureNotifyMask |FocusChangeMask; 
 attrib.override_redirect=True;
 attrib.colormap=tk_display->widgets[buttonid].identity.colormap;
 mask=CWColormap| CWBackPixel|CWEventMask|CWBorderPixel|CWCursor|CWOverrideRedirect/*|CWDontPropagate*/;


 button->parent=wd_parent;
 button->top_level=wd_top_level;								
 button->window=XCreateWindow(tk_display->display, wd_parent, x, y, width, height, 1, tk_display->depth, InputOutput, tk_display->widgets[buttonid].identity.visual, mask, &attrib);
#ifdef DEBUG
 fprintf(stderr,"BN window creee %ld\n",button->window);
#endif
 button->type=BN_PUSHBUTTON;
 button->state=state;

 button->x = x;
 button->y = y;
 button->width = width;
 button->height = height;

 button->text=text;
 button->txt_key=key;
 button->txt_font=tk_display->fonts.helvetica12;
 button->txt_gravity=CenterText;

 button->flags=0;
 button->parency_class=0;
 button->parency_number=-1;
 button->neverFocus=False;
 button->hasFocus=False;
 button->wid_number=buttonid;
 button->repeat=button->debug=0;

 mask=button->top_level;
 XChangeProperty(tk_display->display,button->window,tk_display->atoms.WM_TOP_LEVEL,XA_DRAWABLE,32,PropModeReplace,(char *)&mask,1);

 return 0;
}



			/******* REPEAT BUTTON *******/


int bn_CreateRepeatButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height,text,key,state) 
TkDisplay *tk_display;
ButtonID buttonid;
Window wd_parent,wd_top_level;
int x, y, key, state;
unsigned int width, height;
char *text;
{
  int ret;
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 ret=bn_CreatePushButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height,text,key,state);
 if(ret==-1) return -1;
 button->type=BN_REPEATBUTTON;
 tk_display->widgets[buttonid].type=BN_REPEATBUTTON;


/* XSelectInput(tk_display->display,button->window,ExposureMask | KeyPressMask|ButtonPressMask|ButtonReleaseMask | StructureNotifyMask |FocusChangeMask); 
 
 ptr=BN_REPEATBUTTON;
 XChangeProperty(tk_display->display, button->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeReplace,&ptr,1);
 ptr=(long)(button);
 XChangeProperty(tk_display->display, button->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeAppend,&ptr,1);
*/
 return 0;  
}



			/******* POPUP BUTTON *******/


int bn_CreatePopupButton(tk_display, buttonid, wd_parent ,wd_top_level,x ,y ,width , height, text, key, state) 
TkDisplay *tk_display;
ButtonID buttonid;
Window wd_parent,wd_top_level;
int x, y, key, state;
unsigned int width, height;
char *text;
{
 int ret;
 int pop_state;
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 pop_state=state;
 if((state&Pushed)==Pushed && (state&Stopped)!=Stopped)
  pop_state=state+Stopped;
 if((state&Pushed)!=Pushed && (state&Stopped)==Stopped)
  pop_state=state-Stopped;

 ret=bn_CreatePushButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height,text,key,pop_state);
 if(ret==-1) return -1;
 button->type=BN_POPUPBUTTON;
 tk_display->widgets[buttonid].type=BN_POPUPBUTTON;

/* ptr=BN_POPUPBUTTON;
 XChangeProperty(tk_display->display, button->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeReplace,&ptr,1);
 ptr=(long)(button);
 XChangeProperty(tk_display->display, button->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeAppend,&ptr,1);
 */
 return 0;  
}




			/******* CROSS BUTTON *******/


int bn_CreateCrossButton(tk_display, buttonid, wd_parent, wd_top_level, x ,y , width, height, text,state) 
TkDisplay *tk_display;
ButtonID buttonid;
Window wd_parent, wd_top_level;
char *text;
int x, y, state;
unsigned int  width,height;
{
 XSetWindowAttributes attrib;
 unsigned long mask;
 int number;
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 
 attrib.background_pixmap=ParentRelative;
 attrib.cursor=tk_display->widgets[buttonid].identity.cursor;
 attrib.do_not_propagate_mask=FocusChangeMask;
 attrib.event_mask=ExposureMask|KeyPressMask| KeyReleaseMask |ButtonPressMask|ButtonReleaseMask|StructureNotifyMask|FocusChangeMask; 
 attrib.override_redirect=True;
 attrib.colormap=tk_display->widgets[buttonid].identity.colormap;
 mask=CWEventMask|CWCursor|CWBackPixmap|CWOverrideRedirect|CWColormap;


 button->parent=wd_parent;
 button->top_level=wd_top_level;										
 button->window=XCreateWindow(tk_display->display, wd_parent, x, y, width, height, 0, tk_display->depth, InputOutput, tk_display->widgets[buttonid].identity.visual, mask, &attrib);

 button->type=BN_CROSSBUTTON;
 button->state=state;
 button->x = x;
 button->y = y;
 button->width = width;
 button->height = height;
 button->txt_key=0;
 button->txt_font=tk_display->fonts.ega;
 button->txt_x = 28;
 button->txt_y = button->txt_font->ascent+3;
 button->text=text;
 button->flags=0;
 button->neverFocus=False; 
 button->crosstype=0;
 button->hasFocus=False;

 button->parency_class=0;
 button->parency_number=-1;
 button->wid_number=buttonid;

 mask=button->top_level;
 XChangeProperty(tk_display->display,button->window,tk_display->atoms.WM_TOP_LEVEL,XA_DRAWABLE,32,PropModeReplace,(char *)&mask,1);

 return 0;
}




			/******* RADIO BUTTON *******/


int bn_CreateRadioButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height,text,state) 
TkDisplay *tk_display;
ButtonID buttonid;
Window wd_parent, wd_top_level;
char *text;
int x, y, state;
unsigned int width,height;
{
 int ret;
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON && tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;
 

 ret=bn_CreateCrossButton(tk_display, buttonid, wd_parent, wd_top_level, x ,y , width, height, text,state); 
 if(ret==-1) return -1;
 button->type=BN_RADIOBUTTON;
 tk_display->widgets[buttonid].type=BN_RADIOBUTTON;

 return 0;

}





			/******* CHECK BUTTON *******/



int bn_CreateCheckButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height, text,state) 
TkDisplay *tk_display;
ButtonID buttonid;
Window wd_parent, wd_top_level;
char *text;
int x, y, state;
unsigned int width,height;
{
 int ret;
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 ret=bn_CreateCrossButton(tk_display,buttonid,wd_parent, wd_top_level, x ,y , width, height, text,state); 
 if(ret==-1) return -1;
 button->type=BN_CHECKBUTTON;
 tk_display->widgets[buttonid].type=BN_CHECKBUTTON; 

 return 0;

}




			/******* THUMB BUTTON *******/


int bn_CreateThumbButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height, state) 
TkDisplay *tk_display;
ButtonID buttonid;
Window wd_parent,wd_top_level;
int x, y, state;
unsigned int width, height;
{
 int ret;
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;

 ret=bn_CreatePushButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height,"",0,state);
 if(ret==-1) return -1;
 button->type=BN_THUMBBUTTON;
 tk_display->widgets[buttonid].type=BN_THUMBBUTTON; 

 button->neverFocus=True;
 XSelectInput(tk_display->display,button->window,ExposureMask|Button1MotionMask|ButtonPressMask|ButtonReleaseMask|StructureNotifyMask|FocusChangeMask); 
 BN_SetPixmap(tk_display,buttonid,tk_display->pixmaps.thumbM,tk_display->pixmaps.thumbMmask,CenterBitmap,3,3,11,11,DefaultDepth(tk_display->display,tk_display->screen),YES);
  
/*
 ptr=BN_THUMBBUTTON;
 XChangeProperty(tk_display->display, button->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeReplace,&ptr,1);
 ptr=(long)(button);
 XChangeProperty(tk_display->display, button->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeAppend,&ptr,1);
*/
 return 0;

}



			/******* SCROLL BUTTON *******/


int bn_CreateScrollButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height,direction,state) 
TkDisplay *tk_display;
ButtonID buttonid;
Window wd_parent,wd_top_level;
int x, y, state,direction;
unsigned int width, height;
{
 int ret;
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 ret=bn_CreatePushButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height,"",0,state);
 if(ret==-1) return -1;
 button->type=BN_SCROLLBUTTON;
 tk_display->widgets[buttonid].type=BN_SCROLLBUTTON; 
 button->neverFocus=True;
  
 if(direction==UP) BN_SetPixmap(tk_display,buttonid,tk_display->pixmaps.upM,tk_display->pixmaps.up,CenterBitmap,3,3,11,11,tk_display->depth,YES);
 if(direction==DOWN) BN_SetPixmap(tk_display,buttonid,tk_display->pixmaps.downM,tk_display->pixmaps.down,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 if(direction==RIGHT) BN_SetPixmap(tk_display,buttonid,tk_display->pixmaps.rightM,tk_display->pixmaps.right,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 if(direction==LEFT) BN_SetPixmap(tk_display,buttonid,tk_display->pixmaps.leftM,tk_display->pixmaps.left,CenterBitmap,3,3,11,10,tk_display->depth,YES);

 if(direction==UP) BN_SetPixmapGrayed(tk_display,buttonid,tk_display->pixmaps.upMgrayed,tk_display->pixmaps.upMmask,CenterBitmap,3,3,11,11,tk_display->depth,YES);
 if(direction==DOWN) BN_SetPixmapGrayed(tk_display,buttonid,tk_display->pixmaps.downMgrayed,tk_display->pixmaps.downMmask,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 if(direction==RIGHT) BN_SetPixmapGrayed(tk_display,buttonid,tk_display->pixmaps.rightMgrayed,tk_display->pixmaps.rightMmask,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 if(direction==LEFT) BN_SetPixmapGrayed(tk_display,buttonid,tk_display->pixmaps.leftMgrayed,tk_display->pixmaps.leftMmask,CenterBitmap,3,3,11,10,tk_display->depth,YES);

/*
 ptr=BN_SCROLLBUTTON;
 XChangeProperty(tk_display->display, button->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeReplace,&ptr,1);
 ptr=(long)(button);
 XChangeProperty(tk_display->display, button->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeAppend,&ptr,1);
*/
 return 0;

}




			/******* COMBO BUTTON *******/


int bn_CreateComboButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height, state) 
TkDisplay *tk_display;
ButtonID buttonid;
Window wd_parent,wd_top_level;
int x, y, state;
unsigned int width, height;
{
 int ret;
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 ret=bn_CreatePushButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height,"",0,state);
 if(ret==-1) return -1;
 button->type=BN_COMBOBUTTON;
 tk_display->widgets[buttonid].type=BN_COMBOBUTTON;
 button->neverFocus=True;
 XSelectInput(tk_display->display,button->window,ExposureMask|ButtonPressMask|ButtonReleaseMask|StructureNotifyMask|FocusChangeMask); 
 BN_SetPixmap(tk_display,buttonid,tk_display->pixmaps.comboM,tk_display->pixmaps.comboMmask,CenterBitmap,3,3,11,11,tk_display->depth,YES);
 BN_SetPixmapGrayed(tk_display,buttonid,tk_display->pixmaps.comboMgrayed,tk_display->pixmaps.comboMmask,CenterBitmap,3,3,11,11,tk_display->depth,YES);  

/*
 ptr=BN_PUSHBUTTON;
 XChangeProperty(tk_display->display, button->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeReplace,&ptr,1);
 ptr=(long)(button);
 XChangeProperty(tk_display->display, button->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeAppend,&ptr,1);
*/
 return 0;

}





			/******* POPUP RADIO BUTTON *******/


int bn_CreatePopupRadioButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width, height, text,state) 
TkDisplay *tk_display;
ButtonID buttonid;
Window wd_parent, wd_top_level;
char *text;
int x, y, state;
unsigned int width,height;
{
 int ret;
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;


 ret=bn_CreateCrossButton(tk_display,buttonid,wd_parent,wd_top_level,x,y,width,height, text,state); 
 if(ret==-1) return -1;
 button->type=BN_POPUPRADIOBUTTON;
 tk_display->widgets[buttonid].type=BN_POPUPRADIOBUTTON;
 return 0;

}



			/********** Destruction d'un bouton *********/


int BN_Destroy(tk_display,buttonid)
TkDisplay *tk_display;
ButtonID buttonid;
{
 ButtonStruct *button;


 if(buttonid>=0 && buttonid<tk_display->maxwidgets && tk_display->widgets[buttonid].class==WI_BUTTON&&tk_display->widgets[buttonid].button!=NULL)
   button=tk_display->widgets[buttonid].button;
 else return -1;

 XDestroyWindow(tk_display->display,button->window);
 return WID_Remove(tk_display,buttonid);

}



