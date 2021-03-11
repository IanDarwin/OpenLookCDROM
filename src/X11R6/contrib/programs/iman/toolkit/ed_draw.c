/*
 *
 * 	ed_draw.c  
 * 	affichage des zones d'edition
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

#include <X11/iman/widgets.h>





		/******** Affichage de la zone edit ********/



ED_Map(tk_display, editid)
TkDisplay *tk_display;
EditID editid;
{
 EditStruct *edit;

 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;

 XMapWindow(tk_display->display, edit->window);			
}





ED_Unmap(tk_display, editid)
TkDisplay *tk_display;
EditID editid;
{
 EditStruct *edit;

 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;

 XUnmapWindow(tk_display->display, edit->window);			
}






ed_DrawEdit(tk_display,editid)
TkDisplay *tk_display;
EditID editid;
{
 GC gc;   
 XGCValues xgcvalues;
 int mask;
 unsigned int i, j, ptr, width, length;
 unsigned char str[5];
 EditStruct *edit;
 WidgetStruct *widptr;

 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;
 widptr=&tk_display->widgets[editid];

 mask=GCForeground|GCBackground;
 xgcvalues.foreground= widptr->colors->text; 
 xgcvalues.background= widptr->colors->selected;
 gc=XCreateGC(tk_display->display,edit->window,mask, &xgcvalues);
 i=j=0;
 
 if(edit->font==NULL) edit->font=tk_display->fonts.vga;
 XSetFont(tk_display->display, gc,edit->font->fid);


 /*fprintf(stderr,"En route  length=%d  %s\n",edit->length,edit->text);*/


 if(edit->length>0 && edit->text!=NULL)
 for(i=0;i<edit->length;i++)
 {
   /*fprintf(stderr,"i:%d\n",i);*/
   if(i>=edit->sel1 && i<edit->sel2 && edit->sel1!=edit->sel2)
   {
     if((edit->state&Grayed)!=Grayed)
     {
	XSetBackground(tk_display->display,gc,widptr->colors->selected);
	XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->selected);
     }
     else
     {
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->bg);
     }
     sprintf(str,"%c",(unsigned char)edit->text[i]);
     XDrawImageString(tk_display->display,edit->window,gc,edit->txt_x+j,edit->font->ascent+edit->txt_y,str,1);
   }
   else
   {
     if((edit->state&Grayed)!=Grayed)
     {
   	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->text);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->bg);
     }
     else
     {
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->bg);
     }
     sprintf(str,"%c",(unsigned char)edit->text[i]);
     XDrawImageString(tk_display->display,edit->window,gc,edit->txt_x+j,edit->font->ascent+edit->txt_y,str,1); 
   }
   if(edit->length>0 && edit->text!=NULL&&i+1>0 && i+1<=edit->length)   
     j=XTextWidth(edit->font,edit->text,i+1);
   else j=0;
 }

 /*fprintf(stderr,"ED boucle 1 finie\n");*/

 if(edit->length>0 && edit->text!=NULL&&edit->position>0&&edit->position<=edit->length)
   i=edit->txt_x+XTextWidth(edit->font,edit->text,edit->position);
 else i=edit->txt_x;

 if(edit->hasFocus==True)
 {
   /*fprintf(stderr,"ED focus trouve\n");*/
   XSetForeground(tk_display->display,gc,widptr->colors->text);
   if((edit->mode&INSERT)== INSERT)
   {
     if(edit->position>=edit->sel1 && edit->position<edit->sel2 &&edit->sel1!=edit->sel2) 
     {
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->bg); 
	if(edit->length>0 && edit->text!=NULL)
    	  XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
	else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
	XSetBackground(tk_display->display,gc,widptr->colors->selected);
	if(edit->type!=ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
	else XSetForeground(tk_display->display,gc,widptr->colors->selected);
	if(edit->length>0 && edit->text!=NULL&&edit->position>0&&edit->position<=152)
	  width=XTextWidth(edit->font,edit->text,edit->position); 
	else width=0;
	if(width+edit->txt_x<edit->width-edit->txt_x)
	  XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,&edit->text[edit->position],1);
	XSetForeground(tk_display->display,gc,widptr->colors->cursor);
        if(edit->neverFocus==False) XDrawLine(tk_display->display,edit->window,gc,i,edit->txt_y,i,edit->txt_y+edit->font->ascent+edit->font->descent-1);
     }
     else
     { 
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
    	XSetForeground(tk_display->display,gc,widptr->colors->bg);
	if(edit->length>0 && edit->text!=NULL)
	  XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
    	else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
	if(edit->type!=ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->text);
	else XSetForeground(tk_display->display,gc,widptr->colors->bg);
	if(edit->length>0 && edit->text!=NULL&&edit->position>0&&edit->position<=152)
	  width=XTextWidth(edit->font,edit->text,edit->position); 
	else width=0;
	if(width+edit->txt_x<edit->width-edit->txt_x)
	  XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,&edit->text[edit->position],1);
	XSetForeground(tk_display->display,gc,widptr->colors->cursor);
        if(edit->neverFocus==False) XDrawLine(tk_display->display,edit->window,gc,i,edit->txt_y,i,edit->txt_y+edit->font->ascent+edit->font->descent-1);
     }
   }
   else 
   {
     XSetForeground(tk_display->display,gc,widptr->colors->bg);
     XSetBackground(tk_display->display,gc,widptr->colors->bg);
     if(edit->length>0 && edit->text!=NULL)
       XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
     else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
     XSetForeground(tk_display->display,gc,widptr->colors->bg);
     XSetBackground(tk_display->display,gc,widptr->colors->text);
     if(edit->position>=0&&edit->position<edit->length)sprintf(str,"%c",edit->text[edit->position]);
     else sprintf(str," ");
     if(edit->position<edit->length) XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,str,1); 
   }
 }
 else
 {
    /*fprintf(stderr,"ED pas de focus\n");*/
    XSetForeground(tk_display->display,gc,widptr->colors->bg);
    XSetBackground(tk_display->display,gc,widptr->colors->bg);
    if(edit->length>0 && edit->text!=NULL)
      XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
    else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
 }
 XFreeGC(tk_display->display,gc);

}





/*
 *
 * Dessine une lettre du texte
 * de l'edit
 *
 *
 */

ed_DrawLetter(tk_display,editid,number)
TkDisplay *tk_display;
EditID editid;
int number;
{
 GC gc;   
 XGCValues xgcvalues;
 int mask;
 unsigned int i, j, ptr, width, length;
 unsigned char str[5];
 EditStruct *edit;
 WidgetStruct *widptr;

 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;
 widptr=&tk_display->widgets[editid];

 mask=GCForeground|GCBackground;
 xgcvalues.foreground= widptr->colors->text; 
 xgcvalues.background= widptr->colors->selected;
 gc=XCreateGC(tk_display->display, edit->window, mask, &xgcvalues);
 i=j=0;

 XSetFont(tk_display->display, gc,edit->font->fid);
 i=number;


 if(edit->length>0 && i>=0 && i<edit->length)
 {
   if(edit->length>0 && edit->text!=NULL&&i>0&&i<=edit->length)
     j=XTextWidth(edit->font,edit->text,i);
   else j=0;

   if(i>=edit->sel1 && i<edit->sel2 && edit->sel1!=edit->sel2)
   {
     if((edit->state&Grayed)!=Grayed)
     {
	XSetBackground(tk_display->display,gc,widptr->colors->selected);
	XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->selected);
     }
     else
     {
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->bg);
     }
     sprintf(str,"%c",(unsigned char)edit->text[i]);
     XDrawImageString(tk_display->display,edit->window,gc,edit->txt_x+j,edit->font->ascent+edit->txt_y,str,1);
   }
   else
   {
     if((edit->state&Grayed)!=Grayed)
     {
   	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->text);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->bg);
     }
     else
     {
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->bg);
     }
     sprintf(str,"%c",(unsigned char)edit->text[i]);
     XDrawImageString(tk_display->display,edit->window,gc,edit->txt_x+j,edit->font->ascent+edit->txt_y,str,1); 
   }
 }

 if(edit->length>0 && edit->text!=NULL&&edit->position>0 && edit->position<=edit->length)
   i=edit->txt_x+XTextWidth(edit->font,edit->text,edit->position);
 else i=edit->txt_x;

 if(edit->hasFocus==True)
 {
   XSetForeground(tk_display->display,gc,widptr->colors->text);
   if((edit->mode&INSERT)== INSERT)
   {
     if(edit->position>=edit->sel1 && edit->position<edit->sel2 &&edit->sel1!=edit->sel2) 
     {
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->bg); 
	if(edit->length>0 && edit->text!=NULL)
    	  XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
	else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
	XSetBackground(tk_display->display,gc,widptr->colors->selected);
	if(edit->type!=ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
	else XSetForeground(tk_display->display,gc,widptr->colors->selected);
	if(edit->length>0 && edit->text!=NULL&&edit->position>0)
	  width=XTextWidth(edit->font,edit->text,edit->position);
	else width=0;
        if(width+edit->txt_x<edit->width-edit->txt_x) 
	  XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,&edit->text[edit->position],1);
	XSetForeground(tk_display->display,gc,widptr->colors->cursor);
        if(edit->neverFocus==False) XDrawLine(tk_display->display,edit->window,gc,i,edit->txt_y,i,edit->txt_y+edit->font->ascent+edit->font->descent-1);
     }
     else
     { 
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
    	XSetForeground(tk_display->display,gc,widptr->colors->bg);
	if(edit->length>0 && edit->text!=NULL)
    	  XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
	else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
	if(edit->type!=ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->text);
	else XSetForeground(tk_display->display,gc,widptr->colors->bg);
	if(edit->length>0 && edit->text!=NULL&&edit->position>0)
	  width=XTextWidth(edit->font,edit->text,edit->position);
	else width=0;
        if(width+edit->txt_x<edit->width-edit->txt_x) 
	  XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,&edit->text[edit->position],1);
	XSetForeground(tk_display->display,gc,widptr->colors->cursor);
        if(edit->neverFocus==False) XDrawLine(tk_display->display,edit->window,gc,i,edit->txt_y,i,edit->txt_y+edit->font->ascent+edit->font->descent-1);
     }
   }
   else 
   {
     XSetForeground(tk_display->display,gc,widptr->colors->bg);
     XSetBackground(tk_display->display,gc,widptr->colors->bg);
     if(edit->length>0 && edit->text!=NULL)
       XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
     else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
     XSetForeground(tk_display->display,gc,widptr->colors->bg);
     XSetBackground(tk_display->display,gc,widptr->colors->text);
     if(edit->position>=0&&edit->position<edit->length) 
	sprintf(str,"%c",edit->text[edit->position]);
     else sprintf(str," ");
     if(edit->position<edit->length) XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,str,1); 
   }
 }
 else 
 {
    XSetForeground(tk_display->display,gc,widptr->colors->bg);
    XSetBackground(tk_display->display,gc,widptr->colors->bg);
    if(edit->length>0 && edit->text!=NULL)
      XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
    else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
 }

 XFreeGC(tk_display->display,gc);

}






ed_DrawCursor(tk_display,editid)
TkDisplay *tk_display;
EditID editid;
{
 GC gc;   
 XGCValues xgcvalues;
 int mask;
 unsigned int i, j, ptr, width, length;
 unsigned char str[5];
 EditStruct *edit;
 WidgetStruct *widptr;

 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;
 widptr=&tk_display->widgets[editid];


 mask=GCForeground|GCBackground;
 xgcvalues.foreground= widptr->colors->text; 
 xgcvalues.background= widptr->colors->selected;
 gc=XCreateGC(tk_display->display, edit->window, mask, &xgcvalues);
 i=j=0;

 XSetFont(tk_display->display, gc,edit->font->fid);

 if(edit->length>0 && edit->text!=NULL&&edit->position>0&&edit->position<=152)
   i=edit->txt_x+XTextWidth(edit->font,edit->text,edit->position);
 else i=edit->txt_x;

 if(edit->hasFocus==True){
  XSetForeground(tk_display->display,gc,widptr->colors->text);

  if((edit->mode&INSERT)== INSERT){


   if(edit->position>=edit->sel1 && edit->position<edit->sel2 &&edit->sel1!=edit->sel2) 
	{
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->bg); 
    	/*XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
	*/XSetBackground(tk_display->display,gc,widptr->colors->selected);
	if(edit->type!=ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
	else XSetForeground(tk_display->display,gc,widptr->colors->selected);
	if(edit->length>0 && edit->text!=NULL&&edit->position>0)
	  width=XTextWidth(edit->font,edit->text,edit->position);
	else width=0;
        if(width+edit->txt_x<edit->width-edit->txt_x) XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,&edit->text[edit->position],1);
	XSetForeground(tk_display->display,gc,widptr->colors->cursor);
        if(edit->neverFocus==False) XDrawLine(tk_display->display,edit->window,gc,i,edit->txt_y,i,edit->txt_y+edit->font->ascent+edit->font->descent-1);
	}
    else{ 
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
    	XSetForeground(tk_display->display,gc,widptr->colors->bg);
    	/*XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
	*/if(edit->type!=ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->text);
	else XSetForeground(tk_display->display,gc,widptr->colors->bg);
	if(edit->length>0 && edit->text!=NULL&&edit->position>0)
	  width=XTextWidth(edit->font,edit->text,edit->position);
	else width=0;
        if(width+edit->txt_x<edit->width-edit->txt_x) XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,&edit->text[edit->position],1);
	XSetForeground(tk_display->display,gc,widptr->colors->cursor);
        if(edit->neverFocus==False) XDrawLine(tk_display->display,edit->window,gc,i,edit->txt_y,i,edit->txt_y+edit->font->ascent+edit->font->descent-1);
	}
    }

    else {

    XSetForeground(tk_display->display,gc,widptr->colors->bg);
    XSetBackground(tk_display->display,gc,widptr->colors->bg);
    /*XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
    */XSetForeground(tk_display->display,gc,widptr->colors->bg);
    XSetBackground(tk_display->display,gc,widptr->colors->text);
    if(edit->position>=0&&edit->position<edit->length)sprintf(str,"%c",edit->text[edit->position]);
    else sprintf(str," ");
    if(edit->position<edit->length) XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,str,1); 
    
  }
 }
 else {

    XSetForeground(tk_display->display,gc,widptr->colors->bg);
    XSetBackground(tk_display->display,gc,widptr->colors->bg);
    if(edit->length>0 && edit->text!=NULL)
      XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
    else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
 }


 XFreeGC(tk_display->display,gc);

}






ed_DrawText(tk_display,editid,b1,b2)
TkDisplay *tk_display;
EditID editid;
int b1,b2;
{
 GC gc;   
 XGCValues xgcvalues;
 int mask;
 unsigned int i, j, ptr, width, length;
 unsigned char str[5];
 EditStruct *edit;
 WidgetStruct *widptr;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;
 widptr=&tk_display->widgets[editid];


 mask=GCForeground|GCBackground;
 xgcvalues.foreground= widptr->colors->text; 
 xgcvalues.background= widptr->colors->selected;
 gc=XCreateGC(tk_display->display, edit->window, mask, &xgcvalues);
 i=j=0;

 XSetFont(tk_display->display, gc,edit->font->fid);

 if(edit->length>0 && edit->text!=NULL&&b1>0)
   j=XTextWidth(edit->font,edit->text,b1);
 else j=0;

/* if(edit->type==ED_FULLSELECT) fprintf(stderr,"FULL de retour\n");*/


 for(i=b1;i<=b2 && i<edit->length;i++)
 {

 if(i>=edit->sel1 && i<edit->sel2 && edit->sel1!=edit->sel2)
  {
   if((edit->state&Grayed)!=Grayed){
	XSetBackground(tk_display->display,gc,widptr->colors->selected);
	XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->selected);
	}
   else{
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->bg);
   	}
   sprintf(str,"%c",(unsigned char)edit->text[i]);
   XDrawImageString(tk_display->display,edit->window,gc,edit->txt_x+j,edit->font->ascent+edit->txt_y,str,1);
  }
  else
  {
   if((edit->state&Grayed)!=Grayed){
   	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->text);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->bg);
	}
   else{
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
	if(edit->type==ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->bg);
	}
   sprintf(str,"%c",(unsigned char)edit->text[i]);
   XDrawImageString(tk_display->display,edit->window,gc,edit->txt_x+j,edit->font->ascent+edit->txt_y,str,1); 
   
  }
  if(edit->length>0 && edit->text!=NULL&&i+1>0)
    j=XTextWidth(edit->font,edit->text,i+1);
  else j=0;
 }

 if(edit->length>0 && edit->text!=NULL&&edit->position>0)
   i=edit->txt_x+XTextWidth(edit->font,edit->text,edit->position);
 else i=edit->txt_x;

 if(edit->hasFocus==True)
 {
   XSetForeground(tk_display->display,gc,widptr->colors->text);
   if((edit->mode&INSERT)== INSERT)
   {
     if(edit->position>=edit->sel1 && edit->position<edit->sel2 &&edit->sel1!=edit->sel2) 
     {
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XSetForeground(tk_display->display,gc,widptr->colors->bg); 
	if(edit->length>0 && edit->text!=NULL)
    	  XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
	else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
	XSetBackground(tk_display->display,gc,widptr->colors->selected);
	if(edit->type!=ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
	else XSetForeground(tk_display->display,gc,widptr->colors->selected);
	if(edit->length>0 && edit->text!=NULL&&edit->position>0)
	  width=XTextWidth(edit->font,edit->text,edit->position);
	else width=0;	
        if(width+edit->txt_x<edit->width-edit->txt_x) XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,&edit->text[edit->position],1);
	XSetForeground(tk_display->display,gc,widptr->colors->cursor);
        if(edit->neverFocus==False) XDrawLine(tk_display->display,edit->window,gc,i,edit->txt_y,i,edit->txt_y+edit->font->ascent+edit->font->descent-1);
      }
      else
      {   
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
    	XSetForeground(tk_display->display,gc,widptr->colors->bg);
	if(edit->length>0 && edit->text!=NULL)
    	  XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
	else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
	if(edit->type!=ED_SECRETEDIT) XSetForeground(tk_display->display,gc,widptr->colors->text);
	else XSetForeground(tk_display->display,gc,widptr->colors->bg);
	if(edit->length>0 && edit->text!=NULL&&edit->position>0)
	  width=XTextWidth(edit->font,edit->text,edit->position);
	else width=0;
        if(width+edit->txt_x<edit->width-edit->txt_x) 
	  XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,&edit->text[edit->position],1);
	XSetForeground(tk_display->display,gc,widptr->colors->cursor);
        if(edit->neverFocus==False) XDrawLine(tk_display->display,edit->window,gc,i,edit->txt_y,i,edit->txt_y+edit->font->ascent+edit->font->descent-1);
      }
    }
    else 
    {
      XSetForeground(tk_display->display,gc,widptr->colors->bg);
      XSetBackground(tk_display->display,gc,widptr->colors->bg);
      if(edit->length>0 && edit->text!=NULL)
        XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
      else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
      XSetForeground(tk_display->display,gc,widptr->colors->bg);
      XSetBackground(tk_display->display,gc,widptr->colors->text);
      if(edit->position>=0&&edit->position<edit->length)sprintf(str,"%c",edit->text[edit->position]);
      else sprintf(str," ");
      if(edit->position<edit->length) XDrawImageString(tk_display->display,edit->window,gc,i,edit->font->ascent+edit->txt_y,str,1); 
    }
 }
 else 
 {
    XSetForeground(tk_display->display,gc,widptr->colors->bg);
    XSetBackground(tk_display->display,gc,widptr->colors->bg);
    if(edit->length>0 && edit->text!=NULL)
      XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x+XTextWidth(edit->font,edit->text,edit->length),edit->txt_y,edit->width,100);
    else XFillRectangle(tk_display->display,edit->window,gc,edit->txt_x,edit->txt_y,edit->width,100);
 }
 XFreeGC(tk_display->display,gc);

}





ed_MoveText(tk_display,editid,b1,b2)
TkDisplay *tk_display;
EditID editid;
int b1,b2;
{
 GC gc;   
 XGCValues xgcvalues;
 int mask;
 unsigned int i, j, ptr, width, length;
 unsigned char str[5];
 EditStruct *edit;
 WidgetStruct *widptr;


 if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT&&tk_display->widgets[editid].edit!=NULL)
   edit=tk_display->widgets[editid].edit;
 else return -1;
 widptr=&tk_display->widgets[editid];


 mask=GCForeground|GCBackground;
 xgcvalues.foreground= widptr->colors->text; 
 xgcvalues.background= widptr->colors->selected;
 gc=XCreateGC(tk_display->display, edit->window, mask, &xgcvalues);
 i=j=0;

 XSetFont(tk_display->display, gc,edit->font->fid);
 if(edit->length>0 && edit->text!=NULL&&b1>0)
   i=edit->txt_x+XTextWidth(edit->font,edit->text,b1);
 else i=edit->txt_x;
 if(edit->length>0 && edit->text!=NULL&&b2>0)
   j=edit->txt_x+XTextWidth(edit->font,edit->text,b2);
 else j=edit->txt_x;

 XCopyArea(tk_display->display,edit->window,edit->window,gc,i,0,edit->width,edit->height,j,0);

 XFreeGC(tk_display->display,gc);

}



int _ED_DrawFocus(tk_display,editid,type)
TkDisplay *tk_display;
EditID editid;
int type;
{
  EditStruct *edit;
  ListID listid;
  WidgetStruct *widptr;
  WidgetStruct *comboptr;


  if(editid>=0 && editid<tk_display->maxwidgets && tk_display->widgets[editid].class==WI_EDIT && tk_display->widgets[editid].edit!=NULL)
    edit=tk_display->widgets[editid].edit;
  else return -1;
  widptr=&tk_display->widgets[editid];
 

  switch(type)
  {
	case ON :
  		if(edit->parency_class<=0)
  		{
  		  XSetWindowBorder(tk_display->display,edit->window,widptr->colors->focus);
  		}
  		else if(edit->parency_class==WI_COMBO) 
  		{
  		  comboptr=&tk_display->widgets[edit->parency_number];
		  listid=comboptr->combo->list;
		  _LS_DrawFocus(tk_display,listid,ON);
  		}
  		return 0;


	case OFF :  		
		if(edit->parency_class<=0)
  		{
  		  XSetWindowBorder(tk_display->display,edit->window,widptr->colors->nofocus);
  		}
  		else if(edit->parency_class==WI_COMBO) 
  		{
  		  comboptr=&tk_display->widgets[edit->parency_number];
		  listid=comboptr->combo->list;
		  _LS_DrawFocus(tk_display,listid,OFF);
  		}
		return 0;


	default :
		return -1;

  }

}



 

