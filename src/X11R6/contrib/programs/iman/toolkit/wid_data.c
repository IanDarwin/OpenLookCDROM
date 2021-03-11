/*
 *
 * 	wid_data.c  
 * 	gestion des donnees
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>




/* 
 *
 * Ajoute des widgets
 *
 *
 */

int WID_Add(tk_display,number)
TkDisplay *tk_display;
unsigned int number;
{
 int i;

 /*fprintf(stderr,"WID add:%d  to %d  num:%d\n",number,tk_display->maxwidgets,tk_display->numwidgets);*/

 if(number>0)
 {

  if(tk_display->maxwidgets<=0)
  {
   tk_display->widgets=(WidgetStruct *)malloc(sizeof(WidgetStruct)*number);
   if(tk_display->widgets==NULL){
	fprintf(stderr,"ERREUR: Pas assez de memoire\n");
	exit(-1);
	}
   tk_display->maxwidgets=number;
   for(i=0;i<number;i++)
     WID_Initialize(tk_display,i);
   return 0;
  }
  else{
   tk_display->widgets=(WidgetStruct *)realloc(tk_display->widgets,sizeof(WidgetStruct)*(number+tk_display->maxwidgets));
   if(tk_display->widgets==NULL){
	fprintf(stderr,"ERREUR: Pas assez de memoire\n");
	exit(-1);
	}
   tk_display->maxwidgets=tk_display->maxwidgets+number;
   for(i=0;i<number;i++)
     WID_Initialize(tk_display,tk_display->maxwidgets-number+i);
   
   return 0;
  }
 }
 return -1;
}





/*
 *
 * Initialise un widget
 *
 *
 */

int WID_Initialize(tk_display,number)
TkDisplay *tk_display;
unsigned int number;
{
 if(number>=0 && number<tk_display->maxwidgets)
 {
  tk_display->widgets[number].isUsed=False;
  tk_display->widgets[number].class=-1;
  tk_display->widgets[number].number=number;
  tk_display->widgets[number].button=NULL;
  tk_display->widgets[number].scroll=NULL;
  tk_display->widgets[number].edit=NULL;
  tk_display->widgets[number].list=NULL;
  tk_display->widgets[number].combo=NULL;
  tk_display->widgets[number].menu=NULL;
  return 0;
 }
 else return -1;

}





/*
 *
 * Libere un widget
 *
 *
 */

int WID_Remove(tk_display,number)
TkDisplay *tk_display;
unsigned int number;
{
 if(number>=0 && number<tk_display->maxwidgets && tk_display->widgets[number].isUsed==True)
 {
   if(tk_display->widgets[number].class==WI_BUTTON)
     free(tk_display->widgets[number].button);
   else if(tk_display->widgets[number].class==WI_SCROLLBAR)
     free(tk_display->widgets[number].scroll);
   else if(tk_display->widgets[number].class==WI_EDIT)
     free(tk_display->widgets[number].edit);
   else if(tk_display->widgets[number].class==WI_LIST)
     free(tk_display->widgets[number].list);
   else if(tk_display->widgets[number].class==WI_COMBO)
     free(tk_display->widgets[number].combo);
   else if(tk_display->widgets[number].class==WI_MENU)
     free(tk_display->widgets[number].menu);
   tk_display->numwidgets--;
   return WID_Initialize(tk_display,number);
 }
 else return -1;
}






/*
 *
 * Libere la memoire utilisee par tous 
 * les widgets
 *
 */

int WID_Free(tk_display)
TkDisplay *tk_display;
{
 int i;

 if(tk_display->maxwidgets>0)
 {
   for(i=0;i<tk_display->maxwidgets;i++)
   {
     if(tk_display->widgets[i].isUsed==True&&tk_display->widgets[i].class==WI_BUTTON)
       free(tk_display->widgets[i].button);
     else if(tk_display->widgets[i].isUsed==True&&tk_display->widgets[i].class==WI_SCROLLBAR)
       free(tk_display->widgets[i].scroll);
     else if(tk_display->widgets[i].isUsed==True&&tk_display->widgets[i].class==WI_EDIT)
       free(tk_display->widgets[i].edit);
     else if(tk_display->widgets[i].isUsed==True&&tk_display->widgets[i].class==WI_LIST)
       free(tk_display->widgets[i].list);
     else if(tk_display->widgets[i].isUsed==True&&tk_display->widgets[i].class==WI_COMBO)
       free(tk_display->widgets[i].combo);
     else if(tk_display->widgets[i].isUsed==True&&tk_display->widgets[i].class==WI_MENU)
       free(tk_display->widgets[i].menu);
   }
   tk_display->numwidgets=0;
   tk_display->maxwidgets=0;
   free(tk_display->widgets);
   return 0;
 }
 else return -1;
}




