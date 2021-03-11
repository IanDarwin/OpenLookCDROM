/*
 *
 * 	ed_get.c  
 * 	informations sur les zones d'edition
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
 *      IMAN Development Toolkit version 1.0.d
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





int ED_GetPosition(tk_display,editid)
TkDisplay *tk_display;
EditID editid;
{
 EditStruct *edit;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;
 return edit->position;

}




int ED_GetState(tk_display,editid)
TkDisplay *tk_display;
EditID editid;
{
 EditStruct *edit;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;
 return edit->state;

}






char *ED_GetText(tk_display,editid)
TkDisplay *tk_display;
EditID editid;
{
 EditStruct *edit;

 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return (char *)NULL;
 return (char *)edit->text;
}




int ED_GetSelection(tk_display,editid,i1,i2)
TkDisplay *tk_display;
EditID editid;
unsigned int *i1, *i2;
{
 EditStruct *edit;

 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;

 *i1=edit->sel1;
 *i2=edit->sel2;
 return 0;
}


