/*
 *
 * 	wid_focus.c  
 * 	gestion de la main courante
 *
 * 	Modification :  26/12/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute and sell this software and its
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>




/*
 *
 * Cherche le prochain widget et lui donne 
 * le focus
 *
 *
 */

int WID_SeekNextFocus(tk_display,actual,parent,toplevel,seek_type)
TkDisplay *tk_display;
Window actual, parent,toplevel;
int seek_type;
{

 unsigned int i,j,k, actual_pos, seek_state;
 Window qt_root, qt_parent, *qt_children,new_parent;
 unsigned int qt_numchildren;
 ButtonStruct *qt_button;
 EditStruct *qt_edit;
 ListStruct *qt_list;
 ComboStruct *qt_combo;
 XWindowAttributes qt_attrib;
 int number;

#ifdef DEBUG
 fprintf(stderr,"WID seek next ...\n");
#endif


if(seek_type==DOWN || seek_type==DOWN+INCRUST )
{
 XQueryTree(tk_display->display, parent,&qt_root,&qt_parent,&qt_children,&qt_numchildren);
 i=0;
 actual_pos=0;
 if(actual!=0)
 {
    while(qt_children[i]!=actual && i<qt_numchildren) i++;
    actual_pos=i+1;
 }
 else actual_pos=0;

 seek_state=i=0;
 if(actual_pos<qt_numchildren)
 for(i=actual_pos;i<qt_numchildren;i++)
 if(seek_state==0)
 {
#ifdef DEBUG
   fprintf(stderr,"i=%d\n",i);
#endif
   switch(WID_GetWindowType(tk_display,qt_children[i]))
   {

    case WI_SCROLLBAR:
    case WI_MENU: break;

    case WI_COMBO:

	     XGetWindowAttributes(tk_display->display,qt_children[i],&qt_attrib);
	     number=WID_GetWindowNumber(tk_display,qt_children[i]);
	     qt_combo=(ComboStruct *)tk_display->widgets[number].combo;
	     if(qt_attrib.map_state==IsViewable && (qt_combo->state&Grayed)!=Grayed)
	     {
	       XSetInputFocus(tk_display->display,tk_display->widgets[qt_combo->edit].edit->window,RevertToPointerRoot,CurrentTime);
	       XFree(qt_children);
	       seek_state=2;
	     }
	     break;


    case WI_BUTTON:

	     XGetWindowAttributes(tk_display->display,qt_children[i],&qt_attrib);
	     number=WID_GetWindowNumber(tk_display,qt_children[i]);
	     qt_button=(ButtonStruct *)tk_display->widgets[number].button;
	     if(qt_attrib.map_state==IsViewable && (qt_button->state&Grayed)!=Grayed && qt_button->neverFocus==False&& (qt_button->state&Blocked)!=Blocked)
	     {
	       XSetInputFocus(tk_display->display,qt_children[i],RevertToPointerRoot,CurrentTime);
	       XFree(qt_children);
	       seek_state=2;
	     }
	     break;

 
    case WI_EDIT:

	     XGetWindowAttributes(tk_display->display,qt_children[i],&qt_attrib);
	     number=WID_GetWindowNumber(tk_display,qt_children[i]);
	     qt_edit=(EditStruct *)tk_display->widgets[number].edit;
	     if(qt_attrib.map_state==IsViewable && (qt_edit->state&Grayed)!=Grayed)
	     {
	       XSetInputFocus(tk_display->display,qt_children[i],RevertToPointerRoot,CurrentTime);
	       XFree(qt_children);
	       seek_state=2;
	     }
	     break;


   case WI_LIST:

	     XGetWindowAttributes(tk_display->display,qt_children[i],&qt_attrib);
	     number=WID_GetWindowNumber(tk_display,qt_children[i]);
	     qt_list=(ListStruct *)tk_display->widgets[number].list;
	     if(qt_attrib.map_state==IsViewable &&(qt_list->state&Grayed)!=Grayed &&qt_list->numitems>0)
	     { XSetInputFocus(tk_display->display,qt_list->listwindow,RevertToPointerRoot,CurrentTime);
	       XFree(qt_children);
	       seek_state=2;}
	     break;



    default:
    case 0 : if(WID_SeekNextFocus(tk_display,0,qt_children[i],toplevel,DOWN)==2) seek_state=2;
	     break;



   }
  }

 if(seek_state==0 && seek_type==UP+DOWN+INCRUST)
 {
   new_parent=qt_parent;
#ifdef DEBUG
   (void)fprintf(stderr,"root:%ld new_parent: %ld\n",qt_root,new_parent);
#endif
   if(qt_numchildren>0) XFree(qt_children);
   qt_numchildren=0;
   if(parent!=toplevel && qt_parent!=RootWindow(tk_display->display,tk_display->screen)) seek_state=WID_SeekNextFocus(tk_display,parent,qt_parent,toplevel,UP+DOWN+INCRUST);

 }

 if(seek_state==0)
 {
#ifdef DEBUG
   (void)fprintf(stderr,"State=0\n");
#endif
   if(qt_numchildren>0) XFree(qt_children);
   seek_state=WID_SeekNextFocus(tk_display,0,parent,toplevel,DOWN+INCRUST);
 }

 return seek_state;
 }

}





/*
 *
 * Cherche le precedant widget et lui donne 
 * le focus
 *
 *
 */

int WID_SeekPreviousFocus(tk_display,actual,parent,toplevel,seek_type)
TkDisplay *tk_display;
Window actual, parent,toplevel;
int seek_type;
{

 int i,j,k, actual_pos, seek_state;
 Window qt_root, qt_parent, *qt_children,new_parent;
 int qt_numchildren;
 ButtonStruct *qt_button;
 EditStruct *qt_edit;
 ListStruct *qt_list;
 ComboStruct *qt_combo;
 XWindowAttributes qt_attrib;
 int number;


if(seek_type==DOWN || seek_type==DOWN+INCRUST )
{
 XQueryTree(tk_display->display, parent,&qt_root,&qt_parent,&qt_children,&qt_numchildren);
 i=0;
 actual_pos=0;
 if(actual!=0)
 {
    while(qt_children[i]!=actual && i<qt_numchildren) i++;
    actual_pos=i-1;
 }
 else actual_pos=qt_numchildren-1;

 seek_state=i=0;
 if(actual_pos<qt_numchildren)
 for(i=actual_pos;i>=0;i--)
 if(seek_state==0 && i>=0)
 {
   switch(WID_GetWindowType(tk_display,qt_children[i]))
   {

    case WI_SCROLLBAR:
    case WI_MENU: break;

    case WI_COMBO:

	     XGetWindowAttributes(tk_display->display,qt_children[i],&qt_attrib);
	     number=WID_GetWindowNumber(tk_display,qt_children[i]);
	     qt_combo=(ComboStruct *)tk_display->widgets[number].combo;
	     if(qt_attrib.map_state==IsViewable && (qt_combo->state&Grayed)!=Grayed)
	     { XSetInputFocus(tk_display->display,tk_display->widgets[qt_combo->edit].edit->window,RevertToPointerRoot,CurrentTime);
	       XFree(qt_children);
	       seek_state=2;}
	     break;


     case WI_BUTTON:

	     XGetWindowAttributes(tk_display->display,qt_children[i],&qt_attrib);
	     number=WID_GetWindowNumber(tk_display,qt_children[i]);
	     qt_button=(ButtonStruct *)tk_display->widgets[number].button;
	     if(qt_attrib.map_state==IsViewable && (qt_button->state&Grayed)!=Grayed && qt_button->neverFocus==False&& (qt_button->state&Blocked)!=Blocked)
	     { XSetInputFocus(tk_display->display,qt_children[i],RevertToPointerRoot,CurrentTime);
	       seek_state=2;
	       XFree(qt_children);}
	     break;


    case WI_EDIT:

	     XGetWindowAttributes(tk_display->display,qt_children[i],&qt_attrib);
	     number=WID_GetWindowNumber(tk_display,qt_children[i]);
	     qt_edit=(EditStruct *)tk_display->widgets[number].edit;
	     if(qt_attrib.map_state==IsViewable && (qt_edit->state&Grayed)!=Grayed)
	     { XSetInputFocus(tk_display->display,qt_children[i],RevertToPointerRoot,CurrentTime);
	       seek_state=2;
	       XFree(qt_children);}
	     break;


    case WI_LIST:

	     XGetWindowAttributes(tk_display->display,qt_children[i],&qt_attrib);
	     number=WID_GetWindowNumber(tk_display,qt_children[i]);
	     qt_list=(ListStruct *)tk_display->widgets[number].list;
	     if(qt_attrib.map_state==IsViewable && (qt_list->state&Grayed)!=Grayed &&qt_list->numitems>0)
	     { XSetInputFocus(tk_display->display,qt_list->listwindow,RevertToPointerRoot,CurrentTime);
	       XFree(qt_children);
	       seek_state=2;}
	     break;


    default:
    case 0 : seek_state=WID_SeekPreviousFocus(tk_display,0,qt_children[i],toplevel,DOWN);
	     break;



   }
  
  if(seek_state==2) return 2;
  }


 if(seek_state==0 && seek_type==UP+DOWN+INCRUST)
 {
   new_parent=qt_parent;								
#ifdef DEBUG
   (void)fprintf(stderr,"root:%ld new_parent: %ld\n",qt_root,new_parent);
#endif
   if(qt_numchildren>0) XFree(qt_children);
   qt_numchildren=0;
   if(parent!=toplevel && qt_parent!=RootWindow(tk_display->display,tk_display->screen)) seek_state=WID_SeekPreviousFocus(tk_display,parent,qt_parent,toplevel,UP+DOWN+INCRUST);

 }

 if(seek_state==0)
 {
#ifdef DEBUG
   (void)fprintf(stderr,"State=0\n");
#endif
   if(qt_numchildren>0) XFree(qt_children);
   seek_state=WID_SeekPreviousFocus(tk_display,qt_numchildren,parent,toplevel,DOWN+INCRUST);
 }

 return seek_state;
 }

}


