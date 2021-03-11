/*
 *
 * 	win_create.c  
 * 	creation des fenetres
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

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"
#include "X11/Xatom.h"

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>






Window win_Create(tk_display,parent,top_level,class,attributes,x,y,width,height,depth,winclass,visual,mask,params,initialstate)
TkDisplay *tk_display;
Window parent,top_level;
unsigned long class, attributes,initialstate;
int x, y;
unsigned int width, height, depth,winclass;
Visual *visual;
unsigned long mask;
XSetWindowAttributes *params;
{

 unsigned long ptr;
 int ret, i;
 Window win;
 XWMHints wmhints;
 char *str;

 win=XCreateWindow(tk_display->display,parent,x,y,width,height,1,depth,winclass,visual,mask,params);
 if(win==0) return 0; 
 ptr=(unsigned long)class;
 ret=XChangeProperty(tk_display->display, win, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeReplace,(char *)&ptr,1);
 ptr=(unsigned long)attributes;
 ret=XChangeProperty(tk_display->display, win, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER, 32, PropModeAppend,(char *)&ptr,1);


 win_SetToplevel(tk_display,win,top_level);

 wmhints.initial_state=initialstate;
 wmhints.flags=StateHint;
 XSetWMHints(tk_display->display,win,&wmhints);


 for(i=0;i<tk_display->argc;i++)
   XChangeProperty(tk_display->display, win,XA_WM_COMMAND,XA_STRING,8,PropModeAppend,tk_display->argv[i],strlen(tk_display->argv[i]));
 str=(char *)malloc(100);
 gethostname(str,100);
 XChangeProperty(tk_display->display,win,XA_WM_CLIENT_MACHINE,XA_STRING,8,PropModeAppend,str,strlen(str)); 
 free(str);

 XSetCommand(tk_display->display,win,tk_display->argv,tk_display->argc);

 return win;

}





Window win_Destroy(tk_display,window)
TkDisplay *tk_display;
Window window;
{
 XDestroyWindow(tk_display->display,window);
 return 0;
}




