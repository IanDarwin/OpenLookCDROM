/*
 *
 * 	mn_create.c  
 * 	creation des menus
 *
 * 	Modification :  20/12/93
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




/*
 *
 * Creation d'un menu deroulant "flottant"
 *
 *
 */

int MN_CreateFloating(tk_display,menuid,top_level,parency,parentmenuid,x,y,title,defaultfont,state)
TkDisplay *tk_display;
MenuID menuid, parentmenuid;
Window top_level;
short parency;
int state;
int x,y;
char *title;
XFontStruct *defaultfont;
{
 XSetWindowAttributes attrib;
 unsigned int width,height;
 unsigned long mask;
 unsigned long ptr;
 int number;
 MenuStruct *menu, *parentmenu;
 WidgetStruct *widptr;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;
 widptr=&tk_display->widgets[menuid];

 widptr->class=WI_MENU;
 widptr->type=MN_FLOATING;
 widptr->colors=&tk_display->mn_colors;

 if(defaultfont!=(XFontStruct *)NULL) menu->font=defaultfont;
 else menu->font=tk_display->fonts.helvetica12;
 menu->title=title;
 menu->attribut=CloseWhenNoFocus;

 tk_GetConnection(tk_display);
 if(tk_display->wm.active==False) attrib.override_redirect=True;
 else attrib.override_redirect=False;
 attrib.save_under=True;
 attrib.background_pixel=tk_display->mn_colors.bg;
 attrib.cursor=tk_display->widgets[menuid].identity.cursor;
 attrib.colormap=tk_display->widgets[menuid].identity.colormap;

 attrib.event_mask=ExposureMask|KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|FocusChangeMask|StructureNotifyMask|PropertyChangeMask|EnterWindowMask|LeaveWindowMask|OwnerGrabButtonMask; 
 attrib.bit_gravity=NorthWestGravity;
 mask=CWBackPixel|CWEventMask|CWCursor|CWBitGravity|CWOverrideRedirect|CWSaveUnder|CWColormap;
 

 /*width=XTextWidth(menu->font,title,strlen(title))+MN_LMARGE;*/
 width=MN_LMARGE;
 height=2*MN_UMARGE;


 menu->window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen), x, y, width, height, 1, tk_display->depth, InputOutput,tk_display->widgets[menuid].identity.visual, mask,&attrib);
 XSetWindowBorder(tk_display->display,menu->window,tk_display->mn_colors.text);
 XStoreName(tk_display->display,menu->window,title);
 

 menu->top_level=top_level;
 menu->type=MN_FLOATING;
 menu->state=state;
 menu->flags=0;

 menu->x = x;
 menu->y = y;
 menu->width = width;
 menu->height=height;
 menu->maxheight=height;
 menu->maxwidth=0;
 menu->numitems=menu->maxitems=menu->selected=0;

 menu->prevmenu=0;
 menu->repeat=menu->continuity=menu->hasFocus=0;
 menu->selected=0;
 menu->selecteditem=menu->oldselecteditem=-1;
 menu->wid_number=menuid;

 number=menu->wid_number;
 if(parency==WI_MENU && parentmenuid>=0)
 {
  menu->attribut=CloseWhenNoFocus;
  menu->parency_class=WI_MENU; 
  menu->parency_number=parentmenuid;
 }


 ptr=MN_FLOATING;
 XChangeProperty(tk_display->display, menu->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
 ptr=top_level;
 XChangeProperty(tk_display->display,menu->window,tk_display->atoms.WM_TOP_LEVEL,XA_DRAWABLE,32,PropModeReplace,(char *)&ptr,1);

 return 0;

}






/*
 *
 * Creation d'une barre de menu
 *
 *
 */

int MN_CreateBar(tk_display,menuid,top_level,state)
TkDisplay *tk_display;
MenuID menuid;
Window top_level;
int state;
{
 XSetWindowAttributes attrib;
 XWindowAttributes xwa;
 unsigned int width,height;
 unsigned long mask;
 unsigned long ptr;
 int number;
 MenuStruct *menu, *parentmenu;
 WidgetStruct *widptr; 

 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;
 widptr=&tk_display->widgets[menuid];

 widptr->class=WI_MENU;
 widptr->type=MN_MENUBAR;
 widptr->colors=&tk_display->mn_colors;
 
 menu->font=tk_display->fonts.helvetica12;
 menu->title="Menu Bar";
 menu->attribut=CloseWhenNoFocus;
 menu->height=menu->font->ascent+menu->font->descent+2*MN_UMARGE;


 attrib.override_redirect=True;
 attrib.background_pixel=tk_display->mn_colors.bg;
 attrib.cursor=tk_display->widgets[menuid].identity.cursor;
 attrib.colormap=tk_display->widgets[menuid].identity.colormap;
 attrib.event_mask=ExposureMask|KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|Button1MotionMask|FocusChangeMask|StructureNotifyMask|PropertyChangeMask|EnterWindowMask|LeaveWindowMask|OwnerGrabButtonMask; 
 attrib.bit_gravity=NorthWestGravity;
 mask=CWBackPixel|CWEventMask|CWCursor|CWBitGravity|CWOverrideRedirect|CWColormap;
 

 XGetWindowAttributes(tk_display->display,top_level,&xwa);
 menu->window=XCreateWindow(tk_display->display,top_level,-1,-1,5000, menu->height, 1, tk_display->depth, InputOutput,tk_display->widgets[menuid].identity.visual, mask,&attrib);
 XSetWindowBorder(tk_display->display,menu->window,tk_display->mn_colors.text);
/* fprintf(stderr,"w: %d   h: %d\n",xwa.width,menu->height); */

 menu->top_level=top_level;
 menu->type=MN_MENUBAR;
 menu->state=state;
 menu->flags=0;

 menu->x = -1;
 menu->y = -1;
 menu->width = 5000 /*xwa.width*/;

 menu->maxheight=height;
 menu->maxwidth=xwa.width;
 menu->numitems=menu->maxitems=menu->selected=0;

 menu->repeat=menu->continuity=menu->hasFocus=0;

 number=menu->wid_number;
 menu->parency_class=0; 
 menu->parency_number=-1;
 menu->selected=0;
 menu->selecteditem=menu->oldselecteditem=-1;
 menu->wid_number=menuid;

 ptr=MN_MENUBAR;
 XChangeProperty(tk_display->display, menu->window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32,PropModeReplace,(char *)&ptr,1);
 ptr=top_level;
 XChangeProperty(tk_display->display,menu->window,tk_display->atoms.WM_TOP_LEVEL,XA_DRAWABLE,32,PropModeReplace,(char *)&ptr,1);

 return 0;
}






int MN_Destroy(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 MenuStruct *menu, *parentmenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;



 if(menu->maxitems>0) 
   free(menu->items);
 XDestroyWindow(tk_display->display,menu->window);
 WID_Remove(tk_display,menuid);
 return 0;
}



