/*
 *
 * 	ls_draw.c  
 * 	affichage des listes
 *
 * 	Modification :  04/12/93
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







/*
 *
 * Affichage de la liste 
 *
 *
 */

int LS_Map(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 XMapRaised(tk_display->display, list->mainwindow); 
 XMapRaised(tk_display->display, list->listwindow); 
 if(list->type==LS_HSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL)
   SB_Map(tk_display,list->SBH);
 if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
   SB_Map(tk_display,list->SBV);
 return 0;
}




/*
 *
 * Retire la liste de l'ecran
 *
 *
 */

int LS_Unmap(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 XUnmapWindow(tk_display->display, list->mainwindow);			
 XUnmapWindow(tk_display->display, list->listwindow);			

 if(list->type==LS_HSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL)
  SB_Unmap(tk_display,list->SBH);
 if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
  SB_Unmap(tk_display,list->SBV);
 return 0;
}




/*
 *
 * Dessine la liste en entier
 *
 *
 */

int ls_DrawList(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 GC gc;   
 XGCValues xgcvalues;
 int mask;
 unsigned int h1, h2, i, j, ptr, start,w;
 unsigned int leng;
 ListStruct *list;
 WidgetStruct *widptr;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;
 widptr=&tk_display->widgets[listid];


 i=start=list->topitem;
 mask=GCForeground|GCBackground;
 xgcvalues.foreground=widptr->colors->text;
 xgcvalues.background=widptr->colors->bg;
 gc=XCreateGC(tk_display->display, list->listwindow, mask, &xgcvalues);			
 XSetFont(tk_display->display,gc,list->font->fid);
 h1=list->font->ascent;
 h2=list->font->descent; 


 if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
  w=list->width-20;
 else w=list->width;


 while (i<=list->topitem+list->downitem && i<list->numitems)
 {


   if(list->hasFocus==True && list->selected==True && list->selecteditem==i && (list->items[i].state&Grayed)!=Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected);
	XSetBackground(tk_display->display,gc,widptr->colors->selected);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(i-start)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
	}
   else if(list->hasFocus==True && list->selected==True && list->selecteditem==i && (list->items[i].state&Grayed)==Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected);
	XSetBackground(tk_display->display,gc,widptr->colors->selected);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(i-start)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed_selected);
	}
   else if(list->hasFocus==True && list->selected==True && (list->items[i].selected==True) && (list->items[i].state&Grayed)!=Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected_inactive);
	XSetBackground(tk_display->display,gc,widptr->colors->selected_inactive);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(i-start)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_selected_inactive);
	}
   else if(list->hasFocus==True && list->selected==True && (list->items[i].selected==True) && (list->items[i].state&Grayed)==Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected_inactive);
	XSetBackground(tk_display->display,gc,widptr->colors->selected_inactive);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(i-start)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed_selected_inactive);
	}
   else if(list->hasFocus==False && list->selected==True&&(list->selecteditem==i||list->items[i].selected==True) && (list->items[i].state&Grayed)!=Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected_inactive);
	XSetBackground(tk_display->display,gc,widptr->colors->selected_inactive);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(i-start)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_selected_inactive);
	}
   else if(list->hasFocus==False && list->selected==True&&(list->selecteditem==i||list->items[i].selected==True) && (list->items[i].state&Grayed)==Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected_inactive);
	XSetBackground(tk_display->display,gc,widptr->colors->selected_inactive);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(i-start)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed_selected_inactive);
	}
   else if((list->state&Grayed)==Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->bg);
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(i-start)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
	}
   else if((list->selected!=True || list->selecteditem!=i) && (list->items[i].state&Grayed)==Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->bg);
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(i-start)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
	}
   else if((list->selected!=True || list->selecteditem!=i) && (list->items[i].state&Grayed)!=Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->bg);
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(i-start)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text);
	}
   

  if((list->items[i].flags&TextFlag)==TextFlag)
  {
      
   if(list->items[i].text!=NULL&&strlen(list->items[i].text)>0)
     leng=XTextWidth(list->font,list->items[i].text,strlen(list->items[i].text));
   else leng=0;

   if(list->items[i].precedency==TextFlag)
    {
	
	  XDrawString(tk_display->display,list->listwindow,gc,LS_LMARGE-(list->hpos*w),LS_UMARGE+(i-start)*list->itemheight+list->font->ascent+(list->itemheight-list->font->ascent-list->font->descent)/2,list->items[i].text,strlen(list->items[i].text));
    }
   else if(list->items[i].precedency==PixmapFlag)
    {
	if((list->items[i].flags&PixmapFlag)==PixmapFlag)
	  XDrawString(tk_display->display,list->listwindow,gc,LS_LMARGE+LS_INTERMARGE-(list->hpos*w)+list->items[i].pix_width,LS_UMARGE+(i-start)*list->itemheight+list->font->ascent+(list->itemheight-list->font->ascent-list->font->descent)/2,list->items[i].text,strlen(list->items[i].text));
	else
	  XDrawString(tk_display->display,list->listwindow,gc,LS_LMARGE-(list->hpos*w),LS_UMARGE+(i-start)*list->itemheight+list->font->ascent+(list->itemheight-list->font->ascent-list->font->descent)/2,list->items[i].text,strlen(list->items[i].text));
    }
    else XDrawString(tk_display->display,list->listwindow,gc,LS_LMARGE+LS_INTERMARGE-(list->hpos*w),LS_UMARGE+(i-start)*list->itemheight+list->font->ascent+(list->itemheight-list->font->ascent-list->font->descent)/2,list->items[i].text,strlen(list->items[i].text));
   }



  if((list->items[i].flags&PixmapFlag)==PixmapFlag)
  {
   if((list->items[i].flags&TextFlag)==TextFlag&&list->items[i].text!=NULL&&strlen(list->items[i].text)>0)
     leng=XTextWidth(list->font,list->items[i].text,strlen(list->items[i].text));
   else leng=0;

   if(list->items[i].precedency==TextFlag||list->items[i].precedency==0)
    {

      
       if((list->items[i].flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag&&((list->items[i].state&Grayed)==Grayed||(list->state&Grayed)==Grayed))
	{
	 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
	 XSetClipMask(tk_display->display,gc,list->items[i].pix_maskgrayed);
	 XSetClipOrigin(tk_display->display,gc,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight));
	}
	else if((list->items[i].flags&PixmapMaskFlag)==PixmapMaskFlag)
	{
	 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
	 XSetClipMask(tk_display->display,gc,list->items[i].pix_mask);
	 XSetClipOrigin(tk_display->display,gc,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight));
	}


      if(((list->items[i].state&Grayed)==Grayed||(list->state&Grayed)==Grayed)&&(list->items[i].flags&PixmapGrayedFlag)==PixmapGrayedFlag)
      {
       if(list->items[i].pix_depth==1)
	XCopyPlane(tk_display->display,list->items[i].pix_grayed,list->listwindow,gc,0,0,list->items[i].pix_width,list->items[i].pix_height,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight),1);
       else if(list->items[i].pix_depth==tk_display->depth)
	XCopyArea(tk_display->display,list->items[i].pix_grayed,list->listwindow,gc,0,0,list->items[i].pix_width,list->items[i].pix_height,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight));
	}
      else
      {
       if(list->items[i].pix_depth==1)
	XCopyPlane(tk_display->display,list->items[i].pixmap,list->listwindow,gc,0,0,list->items[i].pix_width,list->items[i].pix_height,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight),1);
       else if(list->items[i].pix_depth==tk_display->depth)
	XCopyArea(tk_display->display,list->items[i].pixmap,list->listwindow,gc,0,0,list->items[i].pix_width,list->items[i].pix_height,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight));
	}

      XSetClipMask(tk_display->display,gc,None);	
    }

   else if(list->items[i].precedency==PixmapFlag)
    {

      if((list->items[i].flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag&&((list->items[i].state&Grayed)==Grayed||(list->state&Grayed)==Grayed))
	{
	 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
	 XSetClipMask(tk_display->display,gc,list->items[i].pix_mask);
	 XSetClipOrigin(tk_display->display,gc,LS_LMARGE-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight));
	}
      else if((list->items[i].flags&PixmapMaskFlag)==PixmapMaskFlag)
	{
	 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
	 XSetClipMask(tk_display->display,gc,list->items[i].pix_mask);
	 XSetClipOrigin(tk_display->display,gc,LS_LMARGE-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight));
	}


      if(((list->items[i].state&Grayed)==Grayed||(list->state&Grayed)==Grayed)&&(list->items[i].flags&PixmapGrayedFlag)==PixmapGrayedFlag)
      {
      if(list->items[i].pix_depth==1)
	XCopyPlane(tk_display->display,list->items[i].pix_grayed,list->listwindow,gc,0,0,list->items[i].pix_width,list->items[i].pix_height,LS_LMARGE-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight),1);
      else if(list->items[i].pix_depth==tk_display->depth)											
	XCopyArea(tk_display->display,list->items[i].pix_grayed,list->listwindow,gc,0,0,list->items[i].pix_width,list->items[i].pix_height,LS_LMARGE-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight));
	}
      else {
      if(list->items[i].pix_depth==1)
	XCopyPlane(tk_display->display,list->items[i].pixmap,list->listwindow,gc,0,0,list->items[i].pix_width,list->items[i].pix_height,LS_LMARGE-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight),1);
      else if(list->items[i].pix_depth==tk_display->depth)											
	XCopyArea(tk_display->display,list->items[i].pixmap,list->listwindow,gc,0,0,list->items[i].pix_width,list->items[i].pix_height,LS_LMARGE-(list->hpos*w),LS_UMARGE+(i-list->topitem)*(list->itemheight));
	}


     XSetClipMask(tk_display->display,gc,None);	
    }
  
   }

  i++;

 }
 XSetForeground(tk_display->display,gc,widptr->colors->bg);
 XSetBackground(tk_display->display,gc,widptr->colors->bg);
 XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(i-start)*list->itemheight,list->width,list->itemheight);


 XFreeGC(tk_display->display, gc);  

}





/*
 *
 * Dessine un element de la liste 'list'
 *
 *
 */

int ls_DrawItem(tk_display,listid,number)
TkDisplay *tk_display;
ListID listid;
unsigned int number;
{
 GC gc;   
 XGCValues xgcvalues;
 int mask;
 unsigned int h1, h2,w;
 unsigned int leng;
 ListStruct *list;
 WidgetStruct *widptr;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;
 widptr=&tk_display->widgets[listid];


 if(list->numitems>0 && number>=list->topitem&&number<=list->topitem+list->downitem) 
 {

 mask=GCForeground|GCBackground;
 xgcvalues.foreground=widptr->colors->text;
 xgcvalues.background=widptr->colors->bg;
 gc=XCreateGC(tk_display->display, list->listwindow, mask, &xgcvalues);			
 XSetFont(tk_display->display,gc,list->font->fid);
 h1=list->font->ascent;
 h2=list->font->descent; 

 if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
  w=list->width-20;
 else w=list->width;


 if(list->hasFocus==True && list->selected==True && (list->selecteditem==number) && (list->items[number].state&Grayed)!=Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected);
	XSetBackground(tk_display->display,gc,widptr->colors->selected);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(number-list->topitem)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_selected);
	}
 else if(list->hasFocus==True && list->selected==True && (list->selecteditem==number) && (list->items[number].state&Grayed)==Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected);
	XSetBackground(tk_display->display,gc,widptr->colors->selected);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(number-list->topitem)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed_selected);
	}
 else if(list->hasFocus==True && list->selected==True && list->items[number].selected==True && (list->items[number].state&Grayed)!=Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected_inactive);
	XSetBackground(tk_display->display,gc,widptr->colors->selected_inactive);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(number-list->topitem)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_selected_inactive);
	}
 else if(list->hasFocus==True && list->selected==True && list->items[number].selected==True && (list->items[number].state&Grayed)==Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected_inactive);
	XSetBackground(tk_display->display,gc,widptr->colors->selected_inactive);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(number-list->topitem)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed_selected_inactive);
	}
 else if(list->hasFocus==False && list->selected==True&&(list->selecteditem==number||list->items[number].selected==True) && (list->items[number].state&Grayed)!=Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected_inactive);
	XSetBackground(tk_display->display,gc,widptr->colors->selected_inactive);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(number-list->topitem)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_selected_inactive);
	}
 else if(list->hasFocus==False && list->selected==True&&(list->selecteditem==number||list->items[number].selected==True) && (list->items[number].state&Grayed)==Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->selected_inactive);
	XSetBackground(tk_display->display,gc,widptr->colors->selected_inactive);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(number-list->topitem)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed_selected_inactive);
	}
 else if((list->state&Grayed)==Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->bg);
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(number-list->topitem)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
	}
 else if((list->selected!=True || list->selecteditem!=number) && (list->items[number].state&Grayed)==Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->bg);
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(number-list->topitem)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text_grayed);
	}
 else if((list->selected!=True || list->selecteditem!=number) && (list->items[number].state&Grayed)!=Grayed && (list->state&Grayed)!=Grayed){
	XSetForeground(tk_display->display,gc,widptr->colors->bg);
	XSetBackground(tk_display->display,gc,widptr->colors->bg);
	XFillRectangle(tk_display->display,list->listwindow,gc,0,LS_UMARGE+(number-list->topitem)*list->itemheight,list->width,list->itemheight);
	XSetForeground(tk_display->display,gc,widptr->colors->text);
	}



  if((list->items[number].flags&TextFlag)==TextFlag)
  {
    if(list->items[number].text!=NULL&&strlen(list->items[number].text)>0)
      leng=XTextWidth(list->font,list->items[number].text,strlen(list->items[number].text));
    else leng=0;

    if(list->items[number].precedency==TextFlag)
    {
	  XDrawString(tk_display->display,list->listwindow,gc,LS_LMARGE-(list->hpos*w),LS_UMARGE+(number-list->topitem)*list->itemheight+list->font->ascent+(list->itemheight-list->font->ascent-list->font->descent)/2,list->items[number].text,strlen(list->items[number].text));
    }
    else if(list->items[number].precedency==PixmapFlag)
    {
	if((list->items[number].flags&PixmapFlag)==PixmapFlag)
	  XDrawString(tk_display->display,list->listwindow,gc,LS_LMARGE+LS_INTERMARGE-(list->hpos*w)+list->items[number].pix_width,LS_UMARGE+(number-list->topitem)*list->itemheight+list->font->ascent+(list->itemheight-list->font->ascent-list->font->descent)/2,list->items[number].text,strlen(list->items[number].text));
	else
	  XDrawString(tk_display->display,list->listwindow,gc,LS_LMARGE-(list->hpos*w),LS_UMARGE+(number-list->topitem)*list->itemheight+list->font->ascent+(list->itemheight-list->font->ascent-list->font->descent)/2,list->items[number].text,strlen(list->items[number].text));
    }
    else XDrawString(tk_display->display,list->listwindow,gc,LS_LMARGE+LS_INTERMARGE-(list->hpos*w),LS_UMARGE+(number-list->topitem)*list->itemheight+list->font->ascent+(list->itemheight-list->font->ascent-list->font->descent)/2,list->items[number].text,strlen(list->items[number].text));
  }



  if((list->items[number].flags&PixmapFlag)==PixmapFlag)
  {
   if((list->items[number].flags&TextFlag)==TextFlag&&list->items[number].text!=NULL&&strlen(list->items[number].text)>0)
     leng=XTextWidth(list->font,list->items[number].text,strlen(list->items[number].text));
   else leng=0;

   if(list->items[number].precedency==TextFlag||list->items[number].precedency==0)
    {
       if((list->items[number].flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag&&((list->items[number].state&Grayed)==Grayed||(list->state&Grayed)==Grayed))
	{
	 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
	 XSetClipMask(tk_display->display,gc,list->items[number].pix_maskgrayed);
	 XSetClipOrigin(tk_display->display,gc,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight));
	}
	else if((list->items[number].flags&PixmapMaskFlag)==PixmapMaskFlag)
	{
	 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
	 XSetClipMask(tk_display->display,gc,list->items[number].pix_mask);
	 XSetClipOrigin(tk_display->display,gc,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight));
	}


      if(((list->items[number].state&Grayed)==Grayed||(list->state&Grayed)==Grayed)&&(list->items[number].flags&PixmapGrayedFlag)==PixmapGrayedFlag)
      {
       if(list->items[number].pix_depth==1)
	XCopyPlane(tk_display->display,list->items[number].pix_grayed,list->listwindow,gc,0,0,list->items[number].pix_width,list->items[number].pix_height,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight),1);
       else if(list->items[number].pix_depth==tk_display->depth)
	XCopyArea(tk_display->display,list->items[number].pix_grayed,list->listwindow,gc,0,0,list->items[number].pix_width,list->items[number].pix_height,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight));
	}
      else
      {
       if(list->items[number].pix_depth==1)
	XCopyPlane(tk_display->display,list->items[number].pixmap,list->listwindow,gc,0,0,list->items[number].pix_width,list->items[number].pix_height,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight),1);
       else if(list->items[number].pix_depth==tk_display->depth)
	XCopyArea(tk_display->display,list->items[number].pixmap,list->listwindow,gc,0,0,list->items[number].pix_width,list->items[number].pix_height,LS_LMARGE+LS_INTERMARGE+leng-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight));
	}

      XSetClipMask(tk_display->display,gc,None);	
    }

   else if(list->items[number].precedency==PixmapFlag)
    {

      if((list->items[number].flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag&&((list->items[number].state&Grayed)==Grayed||(list->state&Grayed)==Grayed))
	{
	 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
	 XSetClipMask(tk_display->display,gc,list->items[number].pix_mask);
	 XSetClipOrigin(tk_display->display,gc,LS_LMARGE-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight));
	}
      else if((list->items[number].flags&PixmapMaskFlag)==PixmapMaskFlag)
	{
	 /*fprintf(stderr,"Pixmap mask detecte pour %d\n",i);*/
	 XSetClipMask(tk_display->display,gc,list->items[number].pix_mask);
	 XSetClipOrigin(tk_display->display,gc,LS_LMARGE-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight));
	}


      if(((list->items[number].state&Grayed)==Grayed||(list->state&Grayed)==Grayed)&&(list->items[number].flags&PixmapGrayedFlag)==PixmapGrayedFlag)
      {
      if(list->items[number].pix_depth==1)
	XCopyPlane(tk_display->display,list->items[number].pix_grayed,list->listwindow,gc,0,0,list->items[number].pix_width,list->items[number].pix_height,LS_LMARGE-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight),1);
      else if(list->items[number].pix_depth==tk_display->depth)											
	XCopyArea(tk_display->display,list->items[number].pix_grayed,list->listwindow,gc,0,0,list->items[number].pix_width,list->items[number].pix_height,LS_LMARGE-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight));
	}
      else {
      if(list->items[number].pix_depth==1)
	XCopyPlane(tk_display->display,list->items[number].pixmap,list->listwindow,gc,0,0,list->items[number].pix_width,list->items[number].pix_height,LS_LMARGE-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight),1);
      else if(list->items[number].pix_depth==tk_display->depth)											
	XCopyArea(tk_display->display,list->items[number].pixmap,list->listwindow,gc,0,0,list->items[number].pix_width,list->items[number].pix_height,LS_LMARGE-(list->hpos*w),LS_UMARGE+(number-list->topitem)*(list->itemheight));
	}


     XSetClipMask(tk_display->display,gc,None);	
    }
       
   }
  

 XFreeGC(tk_display->display, gc);  

 }

}


int _LS_DrawFocus(tk_display,listid,type)
TkDisplay *tk_display;
ListID listid;
int type;
{
  ListStruct *list;
  ScrollbarStruct *SBV, *SBH;
  EditStruct *edit;
  ButtonStruct *button;
  WidgetStruct *widptr;
  WidgetStruct *comboptr;
  WidgetStruct *buttonptr;


  if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
    list=tk_display->widgets[listid].list;
  else return -1;
  widptr=&tk_display->widgets[listid];
 
  SBV=tk_display->widgets[list->SBV].scroll;
  SBH=tk_display->widgets[list->SBH].scroll;


  switch(type)
  {
	case ON :
  		if(list->parency_class<=0)
  		{
  		  XSetWindowBorder(tk_display->display,list->listwindow,widptr->colors->focus);
  		  XSetWindowBorder(tk_display->display,list->mainwindow,widptr->colors->focus);
  		  if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL)
		  {
		    XSetWindowBorder(tk_display->display,SBV->mainwindow,widptr->colors->focus);
		    /*buttonptr=&tk_display->widgets[SBV->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);
		    buttonptr=&tk_display->widgets[SBV->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);
		    buttonptr=&tk_display->widgets[SBV->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);*/
		  }
    		  if(widptr->type==LS_HSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL)
		  {
      		    XSetWindowBorder(tk_display->display,SBH->mainwindow,widptr->colors->focus);
		    /*buttonptr=&tk_display->widgets[SBH->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);
		    buttonptr=&tk_display->widgets[SBH->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);
		    buttonptr=&tk_display->widgets[SBH->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->focus);*/
		  }
  		}
  		else if(list->parency_class==WI_COMBO) 
  		{
  		  comboptr=&tk_display->widgets[list->parency_number];
		  XSetWindowBorder(tk_display->display,comboptr->combo->window,comboptr->colors->focus);
  		  XSetWindowBorder(tk_display->display,list->listwindow,comboptr->colors->focus);
  		  XSetWindowBorder(tk_display->display,list->mainwindow,comboptr->colors->focus);
  		  if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL)
  		  XSetWindowBorder(tk_display->display,tk_display->widgets[comboptr->combo->edit].edit->window,comboptr->colors->focus);
		  {
		    XSetWindowBorder(tk_display->display,SBV->mainwindow,comboptr->colors->focus);
		    /*buttonptr=&tk_display->widgets[SBV->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);
		    buttonptr=&tk_display->widgets[SBV->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);
		    buttonptr=&tk_display->widgets[SBV->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);*/
		  }
    		  if(widptr->type==LS_HSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL)
		  {
      		    XSetWindowBorder(tk_display->display,SBH->mainwindow,comboptr->colors->focus);
		    /*buttonptr=&tk_display->widgets[SBH->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);
		    buttonptr=&tk_display->widgets[SBH->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);
		    buttonptr=&tk_display->widgets[SBH->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->focus);*/
		  }
		  edit=tk_display->widgets[comboptr->combo->edit].edit;
		  XSetWindowBorder(tk_display->display,edit->window,comboptr->colors->focus);
		  button=tk_display->widgets[comboptr->combo->button].button;
		  XSetWindowBorder(tk_display->display,button->window,comboptr->colors->focus);
  		}
  		return 0;


	case OFF :  		
		if(list->parency_class<=0)
  		{
  		  XSetWindowBorder(tk_display->display,list->listwindow,widptr->colors->nofocus);
  		  XSetWindowBorder(tk_display->display,list->mainwindow,widptr->colors->nofocus);
  		  if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL)
		  {
		    XSetWindowBorder(tk_display->display,SBV->mainwindow,widptr->colors->nofocus);
		    /*buttonptr=&tk_display->widgets[SBV->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
		    buttonptr=&tk_display->widgets[SBV->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
		    buttonptr=&tk_display->widgets[SBV->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);*/
		  }
    		  if(widptr->type==LS_HSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL)
		  {
      		    XSetWindowBorder(tk_display->display,SBH->mainwindow,widptr->colors->nofocus);
		    /*buttonptr=&tk_display->widgets[SBH->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
		    buttonptr=&tk_display->widgets[SBH->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);
		    buttonptr=&tk_display->widgets[SBH->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,widptr->colors->nofocus);*/
		  }
  		}
  		else if(list->parency_class==WI_COMBO) 
  		{
  		  comboptr=&tk_display->widgets[list->parency_number];
		  XSetWindowBorder(tk_display->display,comboptr->combo->window,comboptr->colors->nofocus);
		  edit=tk_display->widgets[comboptr->combo->edit].edit;
  		  XSetWindowBorder(tk_display->display,list->listwindow,comboptr->colors->nofocus);
  		  XSetWindowBorder(tk_display->display,list->mainwindow,comboptr->colors->nofocus);
		  XSetWindowBorder(tk_display->display,edit->window,comboptr->colors->nofocus);
		  button=tk_display->widgets[comboptr->combo->button].button;
		  XSetWindowBorder(tk_display->display,button->window,comboptr->colors->nofocus);

  		  if(widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL)
  		  XSetWindowBorder(tk_display->display,tk_display->widgets[comboptr->combo->edit].edit->window,comboptr->colors->nofocus);
		  {
		    XSetWindowBorder(tk_display->display,SBV->mainwindow,comboptr->colors->nofocus);
		    /*buttonptr=&tk_display->widgets[SBV->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
		    buttonptr=&tk_display->widgets[SBV->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
		    buttonptr=&tk_display->widgets[SBV->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);*/
		  }
    		  if(widptr->type==LS_HSCROLL || widptr->type==LS_HLEFTVSCROLL || widptr->type==LS_HRIGHTVSCROLL)
		  {
      		    XSetWindowBorder(tk_display->display,SBH->mainwindow,comboptr->colors->nofocus);
		    /*buttonptr=&tk_display->widgets[SBH->B1];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
		    buttonptr=&tk_display->widgets[SBH->B2];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);
		    buttonptr=&tk_display->widgets[SBH->bn_thumb];
		    XSetWindowBorder(tk_display->display,buttonptr->button->window,comboptr->colors->nofocus);*/
		  }
  		}
		return 0;

	default :
		return -1;

  }

}






