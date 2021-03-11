/*
 *
 * 	win_get.c  
 * 	informations sur les fenetres
 *
 * 	Modification :  24/04/93
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
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
#include "X11/Xatom.h"

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>






Window win_GetIconWindow(tk_display,window)
TkDisplay *tk_display;
Window window;
{
 Atom gp_actual_type;
 int gp_actual_format;
 long gp_nitems, gp_bytes_after;
 Window *gp_prop;
 Window ret;


 																
 ret=XGetWindowProperty(tk_display->display, window,tk_display->atoms.WM_STATE,0,2, False,AnyPropertyType, &gp_actual_type, &gp_actual_format, &gp_nitems, &gp_bytes_after,(unsigned char **)&gp_prop);
 if(ret==1) return 0;
 if(gp_nitems>=2){
  ret=gp_prop[1];
  /*(void)fprintf(stderr,"WM GetWindowState nitems=%d state=%ld \n",gp_nitems,gp_prop[0]);*/
  XFree(gp_prop);
  return ret;}

 else{
 /*(void)fprintf(stderr,"WM GetWindowState  nitems= 0 state=%d\n",gp_prop[0]);*/
  return 0;}
 
}





/* 
 *
 * Obtient le type de la fenetre
 *
 *
 */

long win_GetType(tk_display, window)
TkDisplay *tk_display;
Window window;
{

 Atom gp_actual_type;
 int gp_actual_format;
 unsigned long gp_nitems, gp_bytes_after;
 unsigned long *gp_prop;
 long ret;
 Window transient;

 																
 ret=XGetWindowProperty(tk_display->display, window,tk_display->atoms._IMAN_WM_TYPE,0,1, False, AnyPropertyType, &gp_actual_type, &gp_actual_format, & gp_nitems, &gp_bytes_after,(unsigned char **)&gp_prop);
 transient=0;
 if(ret==1) return -1;
 if(gp_nitems>0)
 {
   ret=gp_prop[0];
   /*(void)fprintf(stderr,"WM GetWindowType nitems=%d type=%ld\n",gp_nitems,gp_prop[0]);*/
   XFree(gp_prop);
   return ret;
 }

 else
 {
   ret=XGetTransientForHint(tk_display->display,window,&transient);
   if(ret==0||transient==0) return -1;
   else return DIALOG_BOX;
 }

}






/* 
 *
 * Obtient l'etat de la fenetre
 *
 *
 */

long win_GetState(tk_display, window)
TkDisplay *tk_display;
Window window;
{

 Atom gp_actual_type;
 int gp_actual_format;
 long gp_nitems, gp_bytes_after;
 long *gp_prop;
 long ret;


 																
 ret=XGetWindowProperty(tk_display->display, window,tk_display->atoms.WM_STATE,0,2, False,AnyPropertyType, &gp_actual_type, &gp_actual_format, &gp_nitems, &gp_bytes_after,(unsigned char **)&gp_prop);
 if(ret!=Success) return -1;

 if(gp_nitems>=1){
  ret=gp_prop[0];
  /*(void)fprintf(stderr,"WM GetWindowState nitems=%d state=%ld \n",gp_nitems,gp_prop[0]);*/
  XFree(gp_prop);
  return ret;}

 else{
 /*(void)fprintf(stderr,"WM GetWindowState  nitems= 0 state=%d\n",gp_prop[0]);*/
  return -1;}

}






/* 
 *
 * Obtient les attributs de la fenetre
 *
 *
 */

long win_GetAttributes(tk_display, window)
TkDisplay *tk_display;
Window window;
{

 Atom gp_actual_type;
 int gp_actual_format;
 long gp_nitems, gp_bytes_after;
 long *gp_prop;
 long ret;


 																
 ret=XGetWindowProperty(tk_display->display, window,tk_display->atoms._IMAN_WM_TYPE,0,2, False, AnyPropertyType, &gp_actual_type, &gp_actual_format, &gp_nitems, &gp_bytes_after,(unsigned char **)&gp_prop);
 if(ret!=Success) return -1;

 if(gp_nitems>=1){
  ret=gp_prop[1];
  /*(void)fprintf(stderr,"WM GetWindowAttributes nitems=%d attributes=%ld\n",gp_nitems,gp_prop[1]);*/
  XFree(gp_prop);
  return ret;}

 else{
/* (void)fprintf(stderr,"WM GetWindowType  nitem=0 type=%d\n",gp_prop[0]);*/
  return -1;}

}





int win_GetGeometry(tk_display,window,root,x,y,width,height)
TkDisplay *tk_display;
Window window,*root;
int *x, *y;
unsigned int *width, *height;
{
  unsigned int bw, depth, ret;

  ret=XGetGeometry(tk_display->display,window,root,x,y,width,height,&bw,&depth);
  if(ret==0)
   return -1;
  else return 0;
}





/*
 *
 * RESERVE A UNE VERSION ULTERIEURE. NE PAS UTILISER !
 *
 *
 */

long win_GetMDWid(tk_display,window)
TkDisplay *tk_display;
Window window;
{

 Atom gp_actual_type;
 int gp_actual_format;
 long gp_nitems, gp_bytes_after;
 unsigned long *gp_prop;
 long ret;


 																
 ret=XGetWindowProperty(tk_display->display, window,tk_display->atoms._IMAN_WM_MDW,0,2, False, AnyPropertyType, &gp_actual_type, &gp_actual_format, &gp_nitems, &gp_bytes_after,(unsigned char **)&gp_prop);
 if(ret!=Success) return 0;

 if(gp_nitems>=1){
  ret=gp_prop[0];
  XFree(gp_prop);
  return ret;}

 else{
  return 0;}

}



