/*
 *
 * 	ls_set.c  
 * 	modification des listes
 *
 * 	Modification :  30/01/94
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>

#include <X11/iman/widgets.h>






/*
 *
 * Donne le focus a une liste
 *
 */

int LS_GiveFocus(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 XWindowAttributes attrib;
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 attrib.map_state=IsUnviewable;
 XGetWindowAttributes(tk_display->display,list->listwindow,&attrib);			  
 
 if((list->state&Grayed)!=Grayed&&attrib.map_state==IsViewable) 
  XSetInputFocus(tk_display->display,list->listwindow,RevertToPointerRoot,CurrentTime); 
 return 0;
}






/*
 *
 * Change la liste courante: INUTILISE ET BOGUE !!!!
 *
 */

int LS_SetList(tk_display,listid,items,numitems,maxitems)
ListID listid;
TkDisplay *tk_display;
int numitems,maxitems;
ListItem *items;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;

 list->items=items;
 list->numitems=numitems;
 list->maxitems=maxitems;
 ls_DrawList(tk_display,listid);
 return 0;
}




/*
 *
 * Rajoute un element a la liste "list"
 * 
 */

int LS_AddItem(tk_display,listid,position,initialstate,delay)
TkDisplay *tk_display;
ListID listid;
int position;
int initialstate;
Bool delay;
{
 int pos;
 int i;
 ListStruct *list;
 ScrollbarStruct *SBV, *SBH;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;
 SBV=tk_display->widgets[list->SBV].scroll;
 SBH=tk_display->widgets[list->SBH].scroll;


 if(position==START && list->numitems<list->maxitems) 
 {
	pos=0;
	if (list->numitems>0) for(i=list->numitems-1;i>=0;i--) 
		list->items[i+1]=list->items[i];
	list->items[pos].flags=0;
	list->items[pos].selected=list->items[pos].precedency=0;
	list->numitems++;
  }

 else if(position==START && list->numitems==list->maxitems) {
	if(list->maxitems==0) list->items=(ListItem *)malloc((sizeof(ListItem))*(10));
	else list->items=(ListItem *)realloc((char *)list->items,(sizeof(ListItem))*(list->maxitems+10));
	if(list->items==0){ fprintf(stderr,"LIST: Plus assez de memoire !!!\n");
			    exit(0);}
	list->maxitems=list->maxitems+10;
	pos=0;
	if (list->numitems>0) for(i=list->numitems-1;i>=0;i--) 
		list->items[i+1]=list->items[i];
	list->items[pos].flags=0;
	list->items[pos].selected=list->items[pos].precedency=0;
	list->numitems++;
  }
 
 else if(position==END && list->numitems<list->maxitems)
  {
	pos=list->numitems;
	list->numitems++;
	
  }
  else  if(position==END && list->numitems==list->maxitems)
  {
	if(list->maxitems==0) list->items=(ListItem *)malloc((sizeof(ListItem))*(10));
	else list->items=(ListItem *)realloc((char *)list->items,(sizeof(ListItem))*(list->maxitems+10));
	if(list->items==0){ fprintf(stderr,"LIST: Plus assez de memoire !!!\n");
			    exit(0);}
	list->maxitems=list->maxitems+10;
	pos=list->numitems;
	list->items[pos].flags=0;
	list->items[pos].selected=list->items[pos].precedency=0;
	list->numitems++;
  }


 else if(position!=START && position!=END && position<=list->numitems && position>0 && list->numitems<list->maxitems)
  {
  	pos=position;
	for(i=list->numitems-1;i>=position;i--) 
		list->items[i+1]=list->items[i];
	list->items[pos].flags=0;
	list->items[pos].selected=list->items[pos].precedency=0;	
	list->numitems++;
  }
  else  if(position!=START && position!=END && position<=list->numitems && position>0 && list->numitems==list->maxitems)
  {
	if(list->maxitems==0) list->items=(ListItem *)malloc((sizeof(ListItem))*(10));
	else list->items=(ListItem *)realloc((char *)list->items,(sizeof(ListItem))*(list->maxitems+10));
	if(list->items==0){ fprintf(stderr,"LIST: Plus assez de memoire !!!\n");
			    exit(0);}
	list->maxitems=list->maxitems+10;
  	pos=position;
	for(i=list->numitems-1;i>=position;i--) 
		list->items[i+1]=list->items[i];
	list->items[pos].flags=0;
	list->items[pos].selected=list->items[pos].precedency=0;
	list->numitems++;

  }


 list->items[pos].state=initialstate;
 if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
   { SB_SetRange(tk_display,list->SBV,list->numitems-list->downitem-1);
     if(SBV->range>0) SB_Ungray(tk_display,list->SBV);
   }


 if(delay==False) return ls_DrawList(tk_display,listid);
 return 0;
 
}





/*
 *
 * Retire un element a la liste "list"
 *
 */

int LS_DeleteItem(tk_display,listid,position,delay)
TkDisplay *tk_display;
ListID listid;
int position;
Bool delay;
{
 int pos;
 int i;
 ListStruct *list;
 ScrollbarStruct *SBH, *SBV;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;
 SBV=tk_display->widgets[list->SBV].scroll;
 SBH=tk_display->widgets[list->SBH].scroll;


 if(position>=0 && position<list->numitems) 
  {
	pos=position;
	for(i=position+1;i<list->numitems;i++) 
		list->items[i-1]=list->items[i];
	list->numitems--;
  }

 else{ if(position==END)
  {
	list->numitems--;
  }
  else return -1;}



/* if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
  { SB_SetRange(tk_display,list->SBV,list->numitems-list->downitem-1);
    if(SBV->range<=0) SB_GrayScrollbar(tk_display,list->SBV);
   }*/

 list->hpos=0;
 LS_AutoSetParams(tk_display,listid,delay);

 if(list->selected==True){
  if(list->selecteditem>position) list->selecteditem--;
  else if(list->selecteditem==position){
    if(position-1>=0&&list->numitems>0) list->selecteditem=position-1;
    else if(list->numitems>0) list->selecteditem=0;
    else list->selected=False;
  }
 }

 if(delay==False||(list->topitem+list->downitem>=list->numitems)){
  if(list->topitem+list->downitem>=list->numitems&&list->topitem-1>=0)
   list->topitem--;
  /*if(list->topitem+list->downitem>=list->numitems&&list->topitem-1<0)
   XClearWindow(tk_display->display,list->listwindow);*/
  ls_DrawList(tk_display,listid);
 }
 return 0;

}




/*
 *
 * Remet a 0 toute la liste, efface tous les elements
 * et libere la memoire allouee
 *
 */

LS_Clear(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 if(list->maxitems>0)
  free(list->items);

 list->numitems=list->maxitems=0;
 list->selected=list->selecteditem=0;

 list->maxwidth=list->maxheight=0;
 list->topitem=list->hpos=0;

 if(list->type==LS_HSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL)
  { SB_SetRange(tk_display,list->SBH,0);
    SB_Gray(tk_display,list->SBH);}
 if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
  { SB_SetRange(tk_display,list->SBV,0);
    SB_Gray(tk_display,list->SBV);}

 XClearWindow(tk_display->display,list->listwindow);
 return 0;
}





/*
 *
 * Deplace un element de la liste
 *
 *
 */

int LS_MoveItem(tk_display,listid,number,newposition,delay)
TkDisplay *tk_display;
ListID listid;
int number, newposition;
Bool delay;
{
 ListItem dup;
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 if(number!=newposition)
 {
  dup=list->items[number];
  LS_DeleteItem(tk_display,listid,number,delay);
  LS_AddItem(tk_display,listid,newposition,dup.state,delay);
  if(newposition==END)
    list->items[LS_GetNumitems(tk_display,listid)-1]=dup;
  else if(newposition>=0 && newposition<list->numitems)
    list->items[newposition]=dup;

 }
 return 0;
}





/*
 *
 * Ajoute et initialise un element de la liste
 *
 */

LS_AddFillItem(tk_display,listid,type,position,text,font,pixmap,pix_mask,pix_width, pix_height, pix_depth, precedency, initialstate,delay)
TkDisplay *tk_display;
ListID listid;
int type, position;
char *text;
XFontStruct *font;
Pixmap pixmap, pix_mask;
unsigned int pix_width, pix_height, pix_depth;
int precedency, initialstate;
Bool delay;
{
 int pos;
 int i;
 ListStruct *list;
 ScrollbarStruct *SBV, *SBH;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;
 SBV=tk_display->widgets[list->SBV].scroll;
 SBH=tk_display->widgets[list->SBH].scroll;


 /*fprintf(stderr,"numitems:%d maxitems:%d position:%d\n",list->numitems,list->maxitems,position);
 if(list->numitems>0) for(i=0;i<list->numitems;i++)
   fprintf(stderr,"i:%d  text:%s\n",i,list->items[i].text);*/


 if(position==START && list->numitems<list->maxitems) 
 {
	pos=0;
	if (list->numitems>0) for(i=list->numitems-1;i>=0;i--) 
		list->items[i+1]=list->items[i];
	list->items[pos].flags=0;
	list->items[pos].selected=list->items[pos].precedency=0;
	list->numitems++;
  }

 else if(position==START && list->numitems==list->maxitems) {
	if(list->maxitems==0) list->items=(ListItem *)malloc((sizeof(ListItem))*(10));
	else list->items=(ListItem *)realloc((char *)list->items,(sizeof(ListItem))*(list->maxitems+10));
	if(list->items==NULL)
	{ 
	  fprintf(stderr,"LIST: Plus assez de memoire !!!\n");
	  exit(0);
	}
	list->maxitems=list->maxitems+10;
	pos=0;
	if (list->numitems>0) for(i=list->numitems-1;i>=0;i--) 
		list->items[i+1]=list->items[i];
	list->items[pos].flags=0;
	list->items[pos].selected=list->items[pos].precedency=0;
	list->numitems++;
  }
 
 else if(position==END && list->numitems<list->maxitems)
  {
	/*fprintf(stderr,"END: numitems!=maxitems\n");*/
	pos=list->numitems;
	list->items[pos].flags=0;
	list->items[pos].text=(char *)NULL;
	list->items[pos].precedency=0;
	list->items[pos].selected=False;
	list->numitems++;
	
  }
  else  if(position==END && list->numitems==list->maxitems)
  {
	/*fprintf(stderr,"END: numitems=maxitems\n");*/
	if(list->maxitems==0) list->items=(ListItem *)malloc((sizeof(ListItem))*(10));
	else list->items=(ListItem *)realloc((char *)list->items,(sizeof(ListItem))*(list->maxitems+10));
	if(list->items==NULL){ fprintf(stderr,"LIST: Plus assez de memoire !!!\n");
			    exit(0);}
	list->maxitems=list->maxitems+10;
	pos=list->numitems;
	list->items[pos].flags=0;
	list->items[pos].text=(char *)NULL;
	list->items[pos].selected=False;
	list->items[pos].precedency=0;
	list->numitems++;
  }


 else if(position!=START && position!=END && position<=list->numitems && position>0 && list->numitems<list->maxitems)
  {
	/*fprintf(stderr,"inconnu : numitems!=maxitems\n");*/
  	pos=position;
	for(i=list->numitems-1;i>=position;i--) 
		list->items[i+1]=list->items[i];
	list->items[pos].flags=0;
	list->items[pos].selected=list->items[pos].precedency=0;	
	list->numitems++;
  }
  else  if(position!=START && position!=END && position<=list->numitems && position>0 && list->numitems==list->maxitems)
  {
	if(list->maxitems==0) list->items=(ListItem *)malloc((sizeof(ListItem))*(10));
	else list->items=(ListItem *)realloc((char *)list->items,(sizeof(ListItem))*(list->maxitems+10));
	if(list->items==NULL){ fprintf(stderr,"LIST: Plus assez de memoire !!!\n");
			    exit(0);}
	list->maxitems=list->maxitems+10;
  	pos=position;
	for(i=list->numitems-1;i>=position;i--) 
		list->items[i+1]=list->items[i];
	list->items[pos].flags=0;
	list->items[pos].selected=list->items[pos].precedency=False;
	list->numitems++;

  }
  else fprintf(stderr,"LIST: Cas inconnu\n");

 /*fprintf(stderr,"Allocation passee\n");*/
 list->items[pos].state=initialstate;
 /*fprintf(stderr,"initialstate passee\n");*/

 if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
 { 
    SB_SetRange(tk_display,list->SBV,list->numitems-list->downitem-1);
    if(SBV->range>0) SB_Ungray(tk_display,list->SBV);
 }
 /*fprintf(stderr,"SB range passee\n");*/

 if((type&TextFlag)==TextFlag) LS_SetItemText(tk_display,listid,pos,text,font,delay);
 /*fprintf(stderr,"SetItemText passee\n");*/

 if((type&PixmapFlag)==PixmapFlag) LS_SetItemPixmap(tk_display,listid,pos,pixmap, pix_mask, pix_width, pix_height, pix_depth,delay);
 /*fprintf(stderr,"SetItemPixmap passee\n");*/
 LS_SetPrecedency(tk_display,listid,pos,precedency, delay);
 /*fprintf(stderr,"SetPrecedency passee\n"); */
 list->items[pos].selected=False;
 /*fprintf(stderr,"position:%d  text:%s\n",pos,list->items[pos].text);*/

}




/*
 *
 * Initialise le texte d'un element
 *
 */

LS_SetItemText(tk_display,listid,number,text,font,delay)
TkDisplay *tk_display;
ListID listid;
int number;
char *text;
XFontStruct *font;
Bool delay;
{
 int tw, mw, mh;
 int i, h;
 ListStruct *list;
 ScrollbarStruct *scroll, *SBV, *SBH;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;
 SBV=tk_display->widgets[list->SBV].scroll;
 SBH=tk_display->widgets[list->SBH].scroll;



if(number>=0 && number<list->numitems)
{  
 if((list->items[number].flags&TextFlag)!=TextFlag) list->items[number].flags=list->items[number].flags+TextFlag;
 list->items[number].text=text;
 /*fprintf(stderr,"Settext : %s \n",list->items[number].text);*/


 if(font!=NULL){
	 /*fprintf(stderr,"Police personnalisee\n");*/
	 list->items[number].font=font;
 	 if((list->items[number].flags&FontFlag)!=FontFlag) list->items[number].flags=list->items[number].flags+FontFlag;
	 font=list->font;
  	}
 else{ 
	list->items[number].font=list->font;
	font=list->font;
     }
 if((list->items[number].flags&TextFlag)==TextFlag&&text!=NULL&&strlen(text)>0)
   tw=XTextWidth(list->font,text,strlen(text));
 else tw=0;
 /*fprintf(stderr,"Fonte passe %ld\n",font);*/

 
 if((list->items[number].flags&PixmapFlag)==PixmapFlag)
 {
  mw=tw+LS_INTERMARGE+list->items[number].pix_width;
  if(font->ascent+font->descent>list->items[number].pix_height) mh=font->ascent+font->descent;
  else mh=list->items[number].pix_height;
 }
    else 
    {
      mw=tw;
      mh=font->ascent+font->descent;
    }
    /*fprintf(stderr,"Pixmap passe\n");*/


    if(mh>list->maxheight) list->maxheight=mh;
    if(mw>list->maxwidth) 	list->maxwidth=mw+LS_LMARGE;

 
    list->items[number].width=mw;
    list->items[number].height=mh;

    /*fprintf(stderr,"Maxis passes\n");*/


    SBH->range=(unsigned char)(list->maxwidth/(list->width+1));
    /*fprintf(stderr,"Hrange: %d  maxwidth: %d  width: %d\n ",(unsigned char)(list->maxwidth/(list->width+1)),list->maxwidth,list->width);*/

    if(list->type==LS_HSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL){
    {  
      /*fprintf(stderr,"Trange: %d  maxwidth: %d  width: %d\n ",(unsigned char)(list->maxwidth/(list->width+1)),list->maxwidth,list->width);*/
      SB_SetRange(tk_display,list->SBH,(unsigned int)(list->maxwidth/(list->width+1)));}
      if(SBH->range>0) SB_Ungray(tk_display,list->SBH);
    }    


    /*fprintf(stderr,"Autosetparams en route\n");*/
    LS_AutoSetParams(tk_display,listid,delay);

    if(delay==False) 
	ls_DrawItem(tk_display,listid,number);
	
    /*fprintf(stderr,"Settext : %s \n",list->items[number].text);*/

  }


}




/*
 *
 * Initialise le pixmap d'un element
 *
 */

LS_SetItemPixmap(tk_display,listid,number,pixmap,pix_mask,pix_width,pix_height,pix_depth, delay)
TkDisplay *tk_display;
ListID listid;
int number;
Pixmap pixmap, pix_mask;
unsigned int pix_width, pix_height, pix_depth;
Bool delay;
{ 
  int tw, mw, mh;
  int i, h;
  ListStruct *list;
  ScrollbarStruct *SBV, *SBH;


  if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
    list=tk_display->widgets[listid].list;
  else return -1;
  SBV=tk_display->widgets[list->SBV].scroll;
  SBH=tk_display->widgets[list->SBH].scroll;
 
  /*fprintf(stderr,"Settext : %s \n",list->items[number].text);*/

  if(number>=0 && number<list->numitems)
  { 

    if((list->items[number].flags&PixmapFlag)!=PixmapFlag && pixmap>0) list->items[number].flags=list->items[number].flags+PixmapFlag;
    if((list->items[number].flags&PixmapMaskFlag)!=PixmapMaskFlag && pix_mask>0) 	
      list->items[number].flags=list->items[number].flags+PixmapMaskFlag;


  if(pixmap>0)
  {
	list->items[number].pixmap=pixmap;
	list->items[number].pix_width=pix_width;
	list->items[number].pix_height=pix_height;
	list->items[number].pix_depth=pix_depth;
  }
  if(pix_mask>0) list->items[number].pix_mask=pix_mask;

 
 SBH->range=(unsigned char)(list->maxwidth/(list->width+1)); 
 if((list->items[number].flags&TextFlag)==TextFlag)
 {
  if((list->items[number].flags&TextFlag)==TextFlag&&list->items[number].text!=NULL && strlen(list->items[number].text)>0)
    tw=XTextWidth(list->font,list->items[number].text,strlen(list->items[number].text));
  else tw=0;
  mw=tw+LS_INTERMARGE+pix_width;
  if(list->items[number].font->ascent+list->items[number].font->descent>pix_height) mh=list->items[number].font->ascent+list->items[number].font->descent;
  else mh=pix_height;
 }

  else 
  {
    mw=pix_width;
    mh=pix_height;
  }

  /*fprintf(stderr,"Settext : %s \n",list->items[number].text);*/

  if(mh>list->maxheight) list->maxheight=mh;
  if(mw+LS_LMARGE>list->maxwidth) list->maxwidth=mw+LS_LMARGE; 

  list->items[number].width=mw;
  list->items[number].height=mh;


  if(list->type==LS_HSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL){
  { 
   /*fprintf(stderr,"Pix-range: %d  maxwidth: %d  width: %d\n ",(unsigned char)(list->maxwidth/(list->width+1)),list->maxwidth,list->width);*/
    SB_SetRange(tk_display,list->SBH,(unsigned char)(list->maxwidth/(list->width+1)));}
    if(SBH->range>0) SB_Ungray(tk_display,list->SBH);
  }    

 LS_AutoSetParams(tk_display,listid,delay);
 if(delay==False){

	ls_DrawItem(tk_display,listid,number);
	}

 }

}





/*
 *
 * Initialise le pixmap grayed d'un element
 *
 *
 */

LS_SetItemPixmapGrayed(tk_display,listid,number,pix_grayed,pix_maskgrayed,delay)
TkDisplay *tk_display;
ListID listid;
int number;
Pixmap pix_grayed, pix_maskgrayed;
Bool delay;
{ 
 int tw, mw, mh;
 int i, h;
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;



 if(number>=0 && number<list->numitems)
 { 

 if((list->items[number].flags&PixmapGrayedFlag)!=PixmapGrayedFlag && pix_grayed>0) list->items[number].flags=list->items[number].flags+PixmapGrayedFlag;
 if((list->items[number].flags&PixmapMaskGrayedFlag)!=PixmapMaskGrayedFlag && pix_maskgrayed>0) list->items[number].flags=list->items[number].flags+PixmapMaskGrayedFlag;


 if(pix_grayed>0)
	list->items[number].pix_grayed=pix_grayed;
 if(pix_maskgrayed>0) list->items[number].pix_maskgrayed=pix_maskgrayed;

 LS_AutoSetParams(tk_display,listid,delay); 

 if(delay==False)
   ls_DrawItem(tk_display,listid,number);
	
 }
 return 0;

}






/*
 *
 * Change la precedence des composants d'un element
 *
 *
 */

int LS_SetPrecedency(tk_display,listid,number,precedency,delay)
ListID listid;
TkDisplay *tk_display;
int number, precedency;
Bool delay;
{
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 if(number>=0 && number<list->numitems)
 {
   if((precedency==TextFlag && (list->items[number].flags&TextFlag)==TextFlag) || (precedency==PixmapFlag && (list->items[number].flags&PixmapFlag)==PixmapFlag))
     list->items[number].precedency=precedency;
   
   if(delay==False) 
     ls_DrawItem(tk_display,listid,number);
	
 }
 return 0;

}





int LS_SetFont(tk_display,listid,font,delay)
TkDisplay *tk_display;
ListID listid;
XFontStruct *font;
Bool delay;
{
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;

 if(font!=NULL) list->font=font;
 return LS_AutoSetParams(tk_display,listid,delay);

}





/*
 *
 * Desactive la liste
 *
 *
 */

int LS_Gray(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 if((list->state&Grayed)!=Grayed)
 {

  list->state=list->state+Grayed;

  if(list->type==LS_HSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL)
    SB_Gray(tk_display,list->SBH);
  if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
    SB_Gray(tk_display,list->SBV);

  ls_DrawList(tk_display,listid);

 }
 return 0;
}





/* 
 *
 * Reactive la liste
 *
 *
 */

int LS_Ungray(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 ListStruct *list;
 ScrollbarStruct *SBV, *SBH;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;
 SBV=tk_display->widgets[list->SBV].scroll;
 SBH=tk_display->widgets[list->SBH].scroll;

 if((list->state&Grayed)==Grayed)
 {

  list->state=list->state-Grayed;

  if(list->type==LS_HSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL)
    if(SBH->range>0) SB_Ungray(tk_display,list->SBH);
  if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
    if(SBV->range>0) SB_Ungray(tk_display,list->SBV);

  ls_DrawList(tk_display,listid);

 }
 return 0;
}





/*
 *
 * Desactive un element
 *
 *
 */

int LS_GrayItem(tk_display,listid,number)
TkDisplay *tk_display;
ListID listid;
int number;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 if(list->numitems>0 && number<list->numitems && number>=0)
 {
   if((list->items[number].state&Grayed)!=Grayed) list->items[number].state=list->items[number].state+Grayed;
   ls_DrawItem(tk_display,listid,number);
   return 0;
 }
 else if(list->numitems>0 && number==END)
 {
   number=list->numitems-1;
   if((list->items[number].state&Grayed)!=Grayed) list->items[number].state=list->items[number].state+Grayed;
   ls_DrawItem(tk_display,listid,number);
   return 0;
 }
 return -1;
}




/*
 *
 * Reactive un element
 *
 *
 */

int LS_UngrayItem(tk_display,listid,number)
TkDisplay *tk_display;
ListID listid;
int number;
{
 ListStruct *list;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 if(list->numitems>0 && number<list->numitems && number>=0){
	if((list->items[number].state&Grayed)==Grayed) list->items[number].state=list->items[number].state-Grayed;
  	ls_DrawItem(tk_display,listid,number);
 	}
  else if(list->numitems>0 && number==END){
	number=list->numitems-1;
	if((list->items[number].state&Grayed)==Grayed) list->items[number].state=list->items[number].state-Grayed;
  	ls_DrawItem(tk_display,listid,number);
 	}
 else return -1;
}




/*
 *
 * Retire le pixmap d'un element
 *
 *
 */

int LS_DeleteItemPixmap(tk_display,listid,number,delay)
TkDisplay *tk_display;
ListID listid;
int number;
Bool delay;
{
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 if(number>=0 && number<list->numitems)
 {
	if((list->items[number].flags&PixmapFlag)==PixmapFlag) 
	  list->items[number].flags=list->items[number].flags-PixmapFlag; 
	if((list->items[number].flags&PixmapMaskFlag)==PixmapMaskFlag) 
	  list->items[number].flags=list->items[number].flags-PixmapMaskFlag; 
	if(list->items[number].precedency==PixmapFlag) list->items[number].precedency=0;
	if((list->items[number].flags&TextFlag)==TextFlag&&list->items[number].text!=NULL && strlen(list->items[number].text)>0)
	  list->items[number].width=XTextWidth(list->font,list->items[number].text,strlen(list->items[number].text));
	else list->items[number].width=0;

	LS_AutoSetParams(tk_display,listid,delay);

	if(delay==False) ls_DrawItem(tk_display,listid,number);

 }

 else if(number==END&&list->numitems>0)
 {
	number=list->numitems-1;
	if((list->items[list->numitems-1].flags&PixmapFlag)==PixmapFlag) 
	  list->items[list->numitems-1].flags=list->items[list->numitems-1].flags-PixmapFlag; 
	if((list->items[list->numitems-1].flags&PixmapMaskFlag)==PixmapMaskFlag) 
	  list->items[list->numitems-1].flags=list->items[list->numitems-1].flags-PixmapMaskFlag; 
	if(list->items[number].precedency==PixmapFlag) list->items[number].precedency=0;
	if((list->items[number].flags&TextFlag)==TextFlag&&list->items[number].text!=NULL && strlen(list->items[number].text)>0)
	  list->items[number].width=XTextWidth(list->font,list->items[number].text,strlen(list->items[number].text));
	else list->items[number].width=0;
	LS_AutoSetParams(tk_display,listid,delay);

	if(delay==False) ls_DrawItem(tk_display,listid,list->numitems-1);

 }
 return 0;

}




/*
 *
 * Retire le pixmap grayed d'un element
 *
 *
 */

int LS_DeleteItemPixmapGrayed(tk_display,listid,number,delay)
TkDisplay *tk_display;
ListID listid;
int number;
Bool delay;
{
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 if(number>=0 && number<list->numitems)
 {
	if((list->items[number].flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
	  list->items[number].flags=list->items[number].flags-PixmapGrayedFlag; 
	if((list->items[number].flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag) 
	  list->items[number].flags=list->items[number].flags-PixmapMaskGrayedFlag; 

	if(delay==False) ls_DrawItem(tk_display,listid,number);

 }

 else if(number==END&&list->numitems>0)
 {
	if((list->items[list->numitems-1].flags&PixmapGrayedFlag)==PixmapGrayedFlag) 
	  list->items[list->numitems-1].flags=list->items[list->numitems-1].flags-PixmapGrayedFlag; 
	if((list->items[list->numitems-1].flags&PixmapMaskGrayedFlag)==PixmapMaskGrayedFlag) 
	  list->items[list->numitems-1].flags=list->items[list->numitems-1].flags-PixmapMaskGrayedFlag; 

	if(delay==False) ls_DrawItem(tk_display,listid,list->numitems-1);

 }
 return 0;

}




/*
 *
 * Retire le text d'un element
 *
 *
 */

int LS_DeleteItemtext(tk_display,listid,number,delay)
TkDisplay *tk_display;
ListID listid;
int number;
Bool delay;
{
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;



 if(number>=0 && number<list->numitems)
 {
	if((list->items[number].flags&TextFlag)==TextFlag) 
	  list->items[number].flags=list->items[number].flags-TextFlag; 
	if(list->items[number].precedency==TextFlag) list->items[number].precedency=0;
	if((list->items[number].flags&PixmapFlag)==PixmapFlag) 
	  list->items[number].width=list->items[number].pix_width; 	
	else list->items[number].width=0;

	LS_AutoSetParams(tk_display,listid,delay);

	if(delay==False) ls_DrawItem(tk_display,listid,number);

 }

 else if(number==END&&list->numitems>0)
 {
	number=list->numitems-1;
	if((list->items[list->numitems-1].flags&TextFlag)==TextFlag) 
	  list->items[list->numitems-1].flags=list->items[list->numitems-1].flags-TextFlag; 
	if(list->items[number].precedency==TextFlag) list->items[number].precedency=0;
	if((list->items[number].flags&PixmapFlag)==PixmapFlag) 
	  list->items[number].width=list->items[number].pix_width; 	
	else list->items[number].width=0;
	LS_AutoSetParams(tk_display,listid,delay);

	if(delay==False) ls_DrawItem(tk_display,listid,list->numitems-1);

 }
 return 0;

}





/*
 *
 * Autorise la selection multiple d'elements
 * (default=NO)
 *
 *
 */

int LS_AllowMultipleSelection(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


 if(list->multipleSelection==False)
   list->multipleSelection=True;
 return 0;

}





/*
 *
 * Interdit la selection multiple d'elements
 * (default=NO)
 *
 *
 */

int LS_ForbidMultipleSelection(tk_display,listid)
TkDisplay *tk_display;
ListID listid;
{
 int i;
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;



 if(list->multipleSelection==True)
 {
  list->multipleSelection=False;
  for(i=0;i<list->numitems;i++)
    list->items[i].selected=False;
  ls_DrawList(tk_display,listid);
 }
 return 0;

}





/*
 *
 * Change l'element courant
 *
 *
 */

int LS_SetActiveItem(tk_display,listid,number)
TkDisplay *tk_display;
ListID listid;
int number;
{
 int old, olds;
 ListStruct *list;
 TkEvent tk_event;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;


/* fprintf(stderr,"LS_SetActive %d\n",number);*/

 if(list->numitems>0 && number==0)
 {
   old=list->selecteditem;
   list->selecteditem=number;
   if(list->selected==True) ls_DrawItem(tk_display,listid,old);
   list->selected=True;
   ls_DrawItem(tk_display,listid,number);

   tk_event.ev_type=XLIBEVENT;
   tk_event.ev_widget=WI_LIST;
   tk_event.list=listid;
   tk_event.event.type=KeyRelease;
   /*fprintf(stderr,"Good\n");*/
   tk_event.event.xkey.keycode=XKeysymToKeycode(tk_display->display,XK_Home);
   tk_event.event.xkey.state=0;
   /*fprintf(stderr,"Good bis\n");*/
 
   old=list->topitem;
   olds=list->selecteditem;
   list->selected=True;
   list->selecteditem=0;
   list->topitem=0;
   if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
     SB_SetPosition(tk_display,list->SBV,list->topitem);
   if(old>0) ls_DrawList(tk_display,listid);
   else{
	  ls_DrawItem(tk_display,listid,olds);
	  ls_DrawItem(tk_display,listid,0);
   }
   /*fprintf(stderr,"Very Good\n");*/
   return 0;  
 }
 else if(list->numitems>0 && number>0 && number<list->numitems)
 {
  old=list->selecteditem;
  list->selecteditem=number;
  if(list->selected==True) ls_DrawItem(tk_display,listid,old);
  list->selected=True;
  ls_DrawItem(tk_display,listid,number);
  if(number<list->topitem  || number >list->topitem+list->downitem)
  {
    list->topitem=list->selecteditem;
    if(list->topitem+list->downitem>=list->numitems)
      list->topitem=list->numitems-list->downitem-1;
    if(list->topitem<0)
      list->topitem=0;
    if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
      SB_SetPosition(tk_display,list->SBV,list->topitem);
    ls_DrawList(tk_display,listid);
  }

  return 0;  
 }
 else if(number==END&&list->numitems>0)
 {
  old=list->selecteditem;
  list->selecteditem=list->numitems-1;
  if(list->selected==True) ls_DrawItem(tk_display,listid,old);
  list->selected=True;
  ls_DrawItem(tk_display,listid,list->numitems-1);
  if(number>=old+list->downitem)
  {
    list->topitem=list->numitems-list->downitem-1;
    if(list->topitem<0)
      list->topitem=0;
    if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
      SB_SetPosition(tk_display,list->SBV,list->topitem);
    ls_DrawList(tk_display,listid);
  }
 return 0;
 }
 else return -1;
}




int LS_SetPosition(tk_display,listid,number)
TkDisplay *tk_display;
ListID listid;
int number;
{
 int old;
 ListStruct *list;
 TkEvent tk_event;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;



 if(list->numitems>0 && number>=0 && number<list->numitems)
 {
  old=list->selecteditem;
  list->selecteditem=number;
  if(list->selected==True) ls_DrawItem(tk_display,listid,old);
  list->selected=True;
  ls_DrawItem(tk_display,listid,number);

  tk_event.ev_type=XLIBEVENT;
  tk_event.ev_widget=WI_LIST;
  tk_event.list=listid;
  tk_event.event.type=KeyRelease;
  /*fprintf(stderr,"Good\n");*/
  tk_event.event.xkey.keycode=XKeysymToKeycode(tk_display->display,XK_Home);
  tk_event.event.xkey.state=0;
  /*fprintf(stderr,"Good bis\n");*/
  ls_ListEvents(tk_display,listid,&tk_event);
  /*fprintf(stderr,"Very Good\n");*/
  return 0;  
 }
 else if(number==END&&list->numitems>0)
 {
  old=list->selecteditem;
  list->selecteditem=list->numitems-1;
  
  if(list->selected==True) ls_DrawItem(tk_display,listid,old);
  list->selected=True;
  ls_DrawItem(tk_display,listid,list->numitems-1);
  if(number>=old+list->downitem)
  {
    list->topitem=list->numitems-list->downitem-1;
    if(list->topitem<0)
      list->topitem=0;
    if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
      SB_SetPosition(tk_display,list->SBV,list->topitem);
    ls_DrawList(tk_display,listid);
  }
  return 0;
 }
 else return -1;
}




/*
 *
 * Selectionne un element
 *
 *
 */

int LS_SelectItem(tk_display,listid,number)
TkDisplay *tk_display;
ListID listid;
int number;
{
 int old;
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;

 
 if(list->numitems>0 && number>=0 && number<list->numitems&&list->multipleSelection==True)
 {
  list->items[number].selected=True;
  ls_DrawItem(tk_display,listid,number);
  return 0;  
 }
 else if(number==END&&list->numitems>0&&list->multipleSelection==True)
 {
  
  list->items[list->numitems-1].selected=True;
  ls_DrawItem(tk_display,listid,list->numitems-1);
  return 0;
 }
 else return -1;

}






/*
 *
 * Deselectionne un element
 *
 *
 */

int LS_UnselectItem(tk_display,listid,number)
TkDisplay *tk_display;
ListID listid;
int number;
{
 int old;
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;



 if(number>=0 && number<list->numitems)
 {
  list->items[number].selected=False;
  ls_DrawItem(tk_display,listid,number);
  return 0;  
 }
 else if(number==END&&list->numitems>0)
 {
  list->items[list->numitems-1].selected=False;
  ls_DrawItem(tk_display,listid,list->numitems-1);
  return 0;
 }
 else return -1;

}





int LS_AutoSetParams(tk_display,listid,delay)
TkDisplay *tk_display;
ListID listid;
Bool delay;
{
 int i, j , w, h;
 ListStruct *list;
 ScrollbarStruct *SBV, *SBH;

 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;
 SBV=tk_display->widgets[list->SBV].scroll;
 SBH=tk_display->widgets[list->SBH].scroll;


 list->maxwidth=0;
 for(i=0;i<list->numitems;i++)
 {
   
  if(list->items[i].width+LS_LMARGE>list->maxwidth) 
    list->maxwidth=list->items[i].width+LS_LMARGE; 

 }

 if(list->type==LS_HSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL){
  { 
    /*fprintf(stderr,"AutoSet: width=%d  maxwidth=%d  rapport=%d\n",list->width,list->maxwidth,(unsigned char)(list->maxwidth/(list->width+1)));*/

    if(list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL||list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL){
     SB_SetRange(tk_display,list->SBH,(unsigned char)(list->maxwidth/(list->width-20+1)));}
    else SB_SetRange(tk_display,list->SBH,(unsigned char)(list->maxwidth/(list->width+1)));}
    SB_SetPagerange(tk_display,list->SBH,1);
    if(SBH->range>0&&(list->state&Grayed)!=Grayed) SB_Ungray(tk_display,list->SBH);
    else { list->hpos=0;
	   SB_Gray(tk_display,list->SBH);}
   }    
 else SBH->range=(unsigned char)(list->maxwidth/(list->width-20+1));

 if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
   { 
     SB_SetRange(tk_display,list->SBV,list->numitems-list->downitem-1);
     SB_SetPagerange(tk_display,list->SBV,list->downitem+1);
     if(SBV->range>0&&(list->state&Grayed)!=Grayed) SB_Ungray(tk_display,list->SBV);
     else SB_Gray(tk_display,list->SBV);
   }
 return 0;
}






int LS_Configure(tk_display,listid,x,y,width,height)
TkDisplay *tk_display;
ListID listid;
int x, y;
unsigned int width, height;
{
 int mask;
 XWindowChanges xwc;
 ListStruct *list;


 if(listid>=0 && listid<tk_display->maxwidgets && tk_display->widgets[listid].class==WI_LIST&&tk_display->widgets[listid].list!=NULL)
   list=tk_display->widgets[listid].list;
 else return -1;



 xwc.x=list->x=x;
 xwc.y=list->y=y;
 xwc.width=list->width=width;
 xwc.height=list->height=height;
 mask=CWX|CWY|CWWidth|CWHeight;

 XConfigureWindow(tk_display->display,list->mainwindow,mask,&xwc);

 switch(list->type){

	case LS_SIMPLE:
		
		xwc.x=-1;
		xwc.y=-1;
		xwc.width=list->width;
		xwc.height=list->height;
		mask=CWWidth|CWHeight;
		XConfigureWindow(tk_display->display,list->listwindow,mask,&xwc);
		list->downitem=((list->height-LS_UMARGE)/list->itemheight)-1;
		break;

	case LS_HSCROLL:

		xwc.width=list->width;
		xwc.height=list->height-20;
		mask=CWWidth|CWHeight;
		XConfigureWindow(tk_display->display,list->listwindow,mask,&xwc);
		SB_Configure(tk_display,list->SBH,-1,list->height-20,width,19);
		list->downitem=((list->height-20-LS_UMARGE)/list->itemheight)-1;
		break;

	case LS_RIGHTVSCROLL:

		xwc.width=list->width-20;
		xwc.height=list->height;
		mask=CWWidth|CWHeight;
		XConfigureWindow(tk_display->display,list->listwindow,mask,&xwc);
		SB_Configure(tk_display,list->SBV,list->width-20,-1,19,list->height);
		list->downitem=((list->height-LS_UMARGE)/list->itemheight)-1;
		break;

	case LS_LEFTVSCROLL:

		xwc.width=list->width-20;
		xwc.height=list->height;
		mask=CWWidth|CWHeight;
		XConfigureWindow(tk_display->display,list->listwindow,mask,&xwc);
		SB_Configure(tk_display,list->SBV,-1,-1,19,list->height);
		list->downitem=((list->height-LS_UMARGE)/list->itemheight)-1;
		break;

	case LS_HRIGHTVSCROLL:

		xwc.width=list->width-20;
		xwc.height=list->height-20;
		mask=CWWidth|CWHeight;
		XConfigureWindow(tk_display->display,list->listwindow,mask,&xwc);
		SB_Configure(tk_display,list->SBV,list->width-20,-1,19,list->height-20);
		SB_Configure(tk_display,list->SBH,-1,list->height-20,list->width,19);
		list->downitem=((list->height-20-LS_UMARGE)/list->itemheight)-1;
		break;

	case LS_HLEFTVSCROLL:

		xwc.width=list->width-20;
		xwc.height=list->height-20;
		mask=CWWidth|CWHeight;
		XConfigureWindow(tk_display->display,list->listwindow,mask,&xwc);
		SB_Configure(tk_display,list->SBV,-1,-1,19,list->height-20);
		SB_Configure(tk_display,list->SBH,-1,list->height-20,list->width,19);
		list->downitem=((list->height-20-LS_UMARGE)/list->itemheight)-1;
		break;



 }

 LS_AutoSetParams(tk_display,listid,False);

 if(list->selected==True)
 {
  if(list->selecteditem+list->downitem<list->numitems)
   list->topitem=list->selecteditem;
  else list->topitem=list->numitems-list->downitem-1;
  if(list->topitem<0) list->topitem=0;
  if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
    SB_SetPosition(tk_display,list->SBV,list->topitem);
  ls_DrawList(tk_display,listid);
 }
 
 else if(list->topitem+list->downitem>=list->numitems)
 {
  list->topitem=list->numitems-list->downitem-1;
  if(list->topitem<0) list->topitem=0;
  if(list->type==LS_LEFTVSCROLL || list->type==LS_RIGHTVSCROLL || list->type==LS_HLEFTVSCROLL || list->type==LS_HRIGHTVSCROLL) 
    SB_SetPosition(tk_display,list->SBV,list->topitem);
  ls_DrawList(tk_display,listid);
 }

 return 0;

}



