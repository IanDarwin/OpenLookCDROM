/*
 *
 * 	ed_create.c  
 * 	creation des zones d'edition
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

#include <X11/iman/widgets.h>




		/********** Creation d'une zone edit *********/



int ED_Create(tk_display,editid,parent,top_level,x ,y ,width , height, text,border,type,state) 
TkDisplay *tk_display;
EditID editid;
Window parent,top_level;
int x, y;
unsigned int width, height;
char *text;
Bool border;
unsigned int type, state;
{
 XSetWindowAttributes attrib;
 unsigned long mask;
 unsigned long ptr;
 EditStruct *edit;
 WidgetStruct *widptr;

 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;
 widptr=&tk_display->widgets[editid];
 widptr->colors=&tk_display->ed_colors;
 widptr->type=type;
 widptr->class=WI_EDIT; 
 
 attrib.background_pixel=tk_display->ed_colors.bg;
 attrib.border_pixel=tk_display->ed_colors.nofocus;
 attrib.cursor=tk_display->widgets[editid].identity.cursor;
 attrib.colormap=tk_display->widgets[editid].identity.colormap;
 attrib.event_mask=ExposureMask | KeyPressMask| KeyReleaseMask| ButtonPressMask|ButtonReleaseMask | Button1MotionMask |FocusChangeMask; 
 mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor|CWColormap;				

 if(border==True) edit->window=XCreateWindow(tk_display->display, parent, x, y, width, height, 1,  tk_display->depth, InputOutput, tk_display->widgets[editid].identity.visual, mask, &attrib);
 else edit->window=XCreateWindow(tk_display->display, parent, x, y, width, height, 0,  tk_display->depth, InputOutput, DefaultVisual(tk_display->display,tk_display->screen), mask, &attrib);


 edit->parent=parent;
 edit->top_level=top_level;
 
 edit->type=type;
 edit->state=state;
 edit->x = x;
 edit->y = y;
 edit->width = width;
 edit->height = height;
 strcpy(edit->text,text); 

 edit->length=strlen(text);
 edit->font=tk_display->fonts.ega;
 edit->position=0;
 edit->neverFocus=False;

 edit->txt_y=4;
 edit->txt_x=5;

 edit->charwidth=8;
 edit->charheight=edit->font->ascent+edit->font->descent;;
 edit->mode=INSERT;

 edit->maxlength=(edit->width-2*edit->txt_x)/edit->charwidth;
 if(edit->maxlength>150) edit->maxlength=150;


 edit->parency_class=0;
 edit->parency_number=-1;
 edit->hasFocus=False;
 edit->wid_number=editid;

 ptr=top_level;
 XChangeProperty(tk_display->display,edit->window,tk_display->atoms.WM_TOP_LEVEL,XA_DRAWABLE,32,PropModeReplace,(char *)&ptr,1);

 return 0;
}







int ED_Destroy(tk_display,editid)
TkDisplay *tk_display;
EditID editid;
{
 EditStruct *edit;

 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;

 XDestroySubwindows(tk_display->display,edit->window);
 XDestroyWindow(tk_display->display,edit->window);
 WID_Remove(tk_display,editid);
 return 0;
}



