/*
 *
 * 	ed_set.c  
 * 	modification des zones d'edition
 *
 * 	Modification :  27/11/93
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

#include <X11/iman/widgets.h>






int ED_SetCursorPosition(tk_display,editid,position,delay)
TkDisplay *tk_display;
EditID editid;
int position;
Bool delay;
{
 EditStruct *edit;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;


 if(position>=0 && position<=edit->length){ 
	edit->position=position;
 	if(delay==False) ed_DrawEdit(tk_display,editid);
	return edit->position;}
 else return -1;

}




int ED_GiveFocus(tk_display,editid)
TkDisplay *tk_display;
EditID editid;
{
 XWindowAttributes attrib;
 EditStruct *edit;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;


 attrib.map_state=IsUnviewable;
 XGetWindowAttributes(tk_display->display,edit->window,&attrib);			  

 if((edit->state&Grayed)!=Grayed&&attrib.map_state==IsViewable)
 {
   XSetInputFocus(tk_display->display,edit->window,RevertToPointerRoot,CurrentTime); 
   return 0;
 }
 else return -1;

}




int ED_SetSelection(tk_display,editid,sel1,sel2,delay)
TkDisplay *tk_display;
EditID editid;
int sel1, sel2;
Bool delay;
{
 EditStruct *edit;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;


 if(sel1>=0 && sel1<=edit->length) edit->sel1=sel1;
 if(sel2>=0 && sel2<=edit->length && sel2>=edit->sel1) edit->sel2=sel2;
 if(delay==False) ed_DrawEdit(tk_display,editid);
 return 0;
}





int ED_SetText(tk_display,editid,text,delay)
TkDisplay *tk_display;
EditID editid;
char *text;
Bool delay;
{
 EditStruct *edit;
 int l;

 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;

 /*fprintf(stderr,"strlen\n");*/
 if(text!=NULL)
   l=strlen(text);
 else l=0;

 /*fprintf(stderr,"strcpy\n");*/
 if(text!=NULL&&l>0&&l<=150)
   strcpy(edit->text,text);
 else strcpy(edit->text,"");
 /*fprintf(stderr,"strlen bis\n");*/
 edit->length=l;
 if(edit->position>edit->length) edit->position=edit->length;
 edit->sel1=edit->sel2=edit->position;
 /*fprintf(stderr,"ed_DrawEdit\n");*/
 if(delay==False) ed_DrawEdit(tk_display,editid);
 /*fprintf(stderr,"PASSE !!!\n");*/
 return 0;
}





int ED_Gray(tk_display,editid)
TkDisplay *tk_display;
EditID editid;
{
 EditStruct *edit;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;

 if((edit->state&Grayed)!=Grayed) edit->state=edit->state+Grayed;
 return ed_DrawEdit(tk_display,editid);
 
}




int ED_Ungray(tk_display,editid)
TkDisplay *tk_display;
EditID editid;
{
 EditStruct *edit;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;

 if((edit->state&Grayed)==Grayed) edit->state=edit->state-Grayed;
 return ed_DrawEdit(tk_display,editid);
 
}





int ED_Configure(tk_display,editid,x,y,width,height)
TkDisplay *tk_display;
EditID editid;
int x, y;
unsigned int width, height;
{
 XWindowChanges xwc;
 int mask;
 EditStruct *edit;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;



 xwc.x=edit->x=x;
 xwc.y=edit->y=y;
 xwc.width=edit->width=width;
 xwc.height=edit->height=height;
 mask=CWX|CWY|CWWidth|CWHeight;

 XConfigureWindow(tk_display->display,edit->window,mask,&xwc);
 return ed_DrawEdit(tk_display,editid);
 
}





int ED_Move(tk_display,editid,x,y)
TkDisplay *tk_display;
EditID editid;
int x, y;
{
 EditStruct *edit;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;


 edit->x=x;
 edit->y=y;

 XMoveWindow(tk_display->display,edit->window,x,y);
 return 0;
}




int ED_Resize(tk_display,editid,width, height)
TkDisplay *tk_display;
EditID editid;
unsigned int width, height;
{
 EditStruct *edit;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;


 edit->width=width;
 edit->height=height;

 XResizeWindow(tk_display->display,edit->window,width,height);
 return 0;
}




int ED_SetFont(tk_display,editid,font,delay)
TkDisplay *tk_display;
EditID editid;
XFontStruct *font;
Bool delay;
{
 EditStruct *edit;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;

 edit->font=font;
 edit->charheight=font->ascent+font->descent;
 
 if(delay==False) ed_DrawEdit(tk_display,editid);
 return 0; 

}




