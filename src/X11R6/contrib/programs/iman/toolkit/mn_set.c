/*
 *
 * 	mn_set.c  
 * 	modification des menus
 *
 * 	Modification :  22/12/93
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
 * Donne le focus a un menu
 *
 */

int mn_GiveFocus(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 XWindowAttributes attrib;
 MenuStruct *menu, *parentmenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


 if(menu->selecteditem<0) menu->selecteditem=0; 
 if(menu->selecteditem>=menu->numitems) menu->selecteditem=menu->numitems-1;
 XSync(tk_display->display,False);

 attrib.map_state=IsUnviewable;
 XGetWindowAttributes(tk_display->display,menu->window,&attrib);			  

 if((menu->state&Grayed)!=Grayed&&attrib.map_state==IsViewable) 
  XSetInputFocus(tk_display->display,menu->window,RevertToPointerRoot,CurrentTime); 
 return 0;
}





/*
 *
 * Rajoute un element a un menu
 * 
 */

int MN_AddFillItem(tk_display,menuid,submenuid,type,position,text,key,font,initialstate,delay)
TkDisplay *tk_display;
MenuID menuid, submenuid;
XFontStruct *font;
int type, position;
char *text;
int initialstate, key;
Bool delay;
{
  int pos;
  int i;
  char cp_int[5]; 
  long ptr;
  MenuStruct *menu, *submenu;


  if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
    menu=tk_display->widgets[menuid].menu;
  else return -1;

  if(submenuid>=0 && submenuid<tk_display->maxwidgets && type==MN_SUBMENU)
  {
    if(tk_display->widgets[submenuid].class==WI_MENU&&tk_display->widgets[submenuid].menu!=NULL)
      submenu=tk_display->widgets[submenuid].menu;
    else return -1;
  }


  /*fprintf(stderr,"numitems:%d maxitems:%d position:%d\n",menu->numitems,menu->maxitems,position);*/



  if(position==START && menu->numitems<menu->maxitems) 
  {
	pos=0;
	if (menu->numitems-1>0) for(i=menu->numitems;i>=0;i--) 
		menu->items[i+1]=menu->items[i];
	menu->items[pos].flags=0;
	menu->items[pos].selected=menu->items[pos].precedency=0;
	menu->numitems++;
   }

  else if(position==START && menu->numitems==menu->maxitems) 
  {
	if(menu->maxitems==0) menu->items=(MenuItem *)malloc((sizeof(MenuItem))*(10));
	else menu->items=(MenuItem *)realloc((char *)menu->items,(sizeof(MenuItem))*(menu->numitems+10));
	if(menu->items==0){ fprintf(stderr,"Menu: Plus assez de memoire !!!\n");
			    exit(0);}
	menu->maxitems=menu->maxitems+10;
	pos=0;
	if (menu->numitems-1>0) for(i=menu->numitems;i>=0;i--) 
		menu->items[i+1]=menu->items[i];
	menu->items[pos].flags=0;
	menu->items[pos].selected=menu->items[pos].precedency=0;
	menu->numitems++;
  }
 
  if(position==END && menu->numitems<menu->maxitems)
  {
	pos=menu->numitems;
	menu->items[pos].flags=0;
	menu->items[pos].precedency=0;
	menu->numitems++;
	
  }
  else  if(position==END && menu->numitems==menu->maxitems)
  {
	if(menu->maxitems==0) menu->items=(MenuItem *)malloc((sizeof(MenuItem))*(10));
	else menu->items=(MenuItem *)realloc((char *)menu->items,(sizeof(MenuItem))*(menu->numitems+10));
	if(menu->items==0){ fprintf(stderr,"Menu: Plus assez de memoire !!!\n");
			    exit(0);}
	menu->maxitems=menu->maxitems+10;
	pos=menu->numitems;
	menu->items[pos].flags=0;
	menu->items[pos].selected=menu->items[pos].precedency=0;
	menu->numitems++;
  }


 if(position!=START && position!=END && position<=menu->numitems && position>0 && menu->numitems<menu->maxitems)
  {
  	pos=position;
	for(i=menu->numitems-1;i>=position;i--) 
		menu->items[i+1]=menu->items[i];
	menu->items[pos].flags=0;
	menu->items[pos].selected=menu->items[pos].precedency=0;	
	menu->numitems++;
  }
  else  if(position!=START && position!=END && position<=menu->numitems && position>0 && menu->numitems==menu->maxitems)
  {
	if(menu->maxitems==0) menu->items=(MenuItem *)malloc((sizeof(MenuItem))*(10));
	else menu->items=(MenuItem *)realloc((char *)menu->items,(sizeof(MenuItem))*(menu->numitems+10));
	if(menu->items==0){ fprintf(stderr,"Menu: Plus assez de memoire !!!\n");
			    exit(0);}
	menu->maxitems=menu->maxitems+10;
  	pos=position;
	for(i=menu->numitems-1;i>=position;i--) 
		menu->items[i+1]=menu->items[i];
	menu->items[pos].flags=0;
	menu->items[pos].selected=menu->items[pos].precedency=0;
	menu->numitems++;

  }


 menu->items[pos].type=type;
 menu->items[pos].number=pos;
 menu->items[pos].font=menu->font;
 menu->items[pos].key=key;
 menu->items[pos].state=initialstate; 
 menu->items[pos].selected=False;
 if(font==(XFontStruct *)NULL) font=menu->font;


  switch(type)
  {


	case MN_ITEM:   MN_SetItemText(tk_display,menuid,pos,text,key,font,delay);
			MN_SetPrecedency(tk_display,menuid, pos, TextFlag, delay);
			break;

	case MN_SUBMENU: 
			menu->items[pos].submenu=submenuid;
			menu->items[pos].type=MN_SUBMENU;
			submenu->prevmenu=menuid;
			submenu->parency_class=WI_MENU;
			submenu->parency_number=menuid;
			submenu->attribut=CloseWhenNoFocus;

			MN_SetItemText(tk_display,menuid,pos,text,key,font,delay);
			MN_SetPrecedency(tk_display,menuid, pos, TextFlag, delay);
		
			break;

	case MN_VBAR:
	case MN_HBAR:   
			MN_AutoSetDimensions(tk_display,menuid,delay);
			break;

	default: 	fprintf(stderr,"Erreur: Mauvais type de MenuItem\n");
			return -1;
			break;  

  }
  return 0;
}




/*
 *
 * Retire un element du menu
 *
 */

int MN_DeleteItem(tk_display,menuid,position,delay)
TkDisplay *tk_display;
MenuID menuid;
int position;
Bool delay;
{
 int pos;
 int i;
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1; 


 if(position>=0 && position<menu->numitems) 
  {
	pos=position;
	for(i=position+1;i<menu->numitems;i++) 
		menu->items[i-1]=menu->items[i];
	menu->numitems--;
  }

 else{ if(position==END)
  {
	menu->numitems--;
  }
  else return -1;}


 if(menu->selected==True){
  if(menu->selecteditem>position) menu->selecteditem--;
  else if(menu->selecteditem==position) menu->selected=False;
 }

 

 if(delay==False){
  MN_AutoSetDimensions(tk_display,menuid,False);
  if(menu->type==MN_FLOATING) mn_DrawFloatingMenu(tk_display,menuid);
  else if(menu->type==MN_MENUBAR) mn_DrawMenuBar(tk_display,menuid);
 }
 else MN_AutoSetDimensions(tk_display,menuid,True);
}






MN_Clear(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 MenuStruct *menu;

 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


 if(menu->maxitems>0)
  free(menu->items);

 menu->numitems=menu->maxitems=0;
 menu->selected=0;
 menu->selecteditem=menu->oldselecteditem=-1;
 menu->prevmenu=0;
 menu->repeat=menu->continuity=menu->hasFocus=0;

 menu->maxwidth=MN_LMARGE;
 menu->maxheight=MN_UMARGE;

 XClearWindow(tk_display->display,menu->window);
 MN_AutoSetDimensions(tk_display,menuid,False);
 return 0;
}






int MN_MoveItem(tk_display,menuid,number,newposition,delay)
TkDisplay *tk_display;
MenuID menuid;
int number, newposition;
Bool delay;
{
 MenuItem dup;
 MenuStruct *menu;

 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


 if(number!=newposition)
 {
  dup=menu->items[number];
  MN_DeleteItem(tk_display,menuid,number,delay);
  MN_AddFillItem(tk_display,menuid,dup.submenu,dup.type,newposition,dup.text,dup.key,dup.font,dup.state,delay);
  if(newposition==END)
    menu->items[menu->numitems-1]=dup;
  else if(newposition>=0 && newposition<menu->numitems)
    menu->items[newposition]=dup;
 }
 return 0;
}






/*
 *
 * Initialise le texte d'un element
 *
 */

int MN_SetItemText(tk_display,menuid,number,text,key,font,delay)
TkDisplay *tk_display;
MenuID menuid;
int number;
char *text;
int key;
XFontStruct *font;
Bool delay;
{
 int tw, mw, mh;
 int i, h;
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


if(menu->type==MN_FLOATING && number>=0 && number<menu->numitems)
 {  
 

 if((menu->items[number].flags&TextFlag)!=TextFlag) menu->items[number].flags=menu->items[number].flags+TextFlag;

 menu->items[number].key=key;
 menu->items[number].text=text;
 if(font!=(XFontStruct *)NULL){
	 menu->items[number].font=font;
 	 if((menu->items[number].flags&FontFlag)!=FontFlag) menu->items[number].flags=menu->items[number].flags+FontFlag;
  	}
 else menu->items[number].font=menu->font;

 if((menu->items[number].flags&TextFlag)==TextFlag&&text!=NULL&&strlen(text)>0)
   tw=XTextWidth(font,text,strlen(text));
 else tw=0;
 
 if((menu->items[number].flags&PixmapFlag)==PixmapFlag)
 {
  mw=tw+MN_INTERMARGE+menu->items[number].pix_width;
  if(font->ascent+font->descent>menu->items[number].pix_height+4) mh=font->ascent+font->descent+2;
  else mh=menu->items[number].pix_height+4;
 }

 else 
  mw=tw;


 if(mw>menu->maxwidth){
 	menu->maxwidth=mw;
	if(mw+2*MN_LMARGE>menu->width) menu->width=mw+2*MN_LMARGE;
 	}

 if(mh<10) mh=10;
 menu->items[number].width=mw;
 menu->items[number].height=mh;

 MN_AutoSetDimensions(tk_display,menuid,delay);


 if(delay==False)
	mn_DrawFloatingMenu(tk_display,menuid);

 }

else if(menu->type==MN_MENUBAR && number>=0 && number<menu->numitems)
 {  

 if((menu->items[number].flags&TextFlag)!=TextFlag) menu->items[number].flags=menu->items[number].flags+TextFlag;

 menu->items[number].key=key;
 menu->items[number].text=text;
 menu->items[number].font=menu->font;
 if((menu->items[number].flags&TextFlag)==TextFlag&&text!=NULL&&strlen(text)>0)
   tw=XTextWidth(font,text,strlen(text));
 else tw=0;

 mw=tw;
 menu->items[number].width=mw;
 MN_AutoSetDimensions(tk_display,menuid,delay);


 if(delay==False) 
	mn_DrawFloatingMenu(tk_display,menuid);
	

 }
 return 0;

}





/*
 *
 * Initialise le pixmap d'un element
 *
 */

int MN_SetItemPixmap(tk_display,menuid,number,pixmap,pix_mask,pix_width,pix_height, pix_depth, delay)
TkDisplay *tk_display;
MenuID menuid;
int number;
Pixmap pixmap, pix_mask;
unsigned int pix_width, pix_height, pix_depth;
Bool delay;
{

 int tw, mw, mh;
 int i, h;
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


if(menu->type==MN_FLOATING)
{
 if(number>=0 && number<menu->numitems)
 { 

 if((menu->items[number].flags&PixmapFlag)!=PixmapFlag && pixmap>0) menu->items[number].flags=menu->items[number].flags+PixmapFlag;
 if((menu->items[number].flags&PixmapMaskFlag)!=PixmapMaskFlag && pix_mask>0) menu->items[number].flags=menu->items[number].flags+PixmapMaskFlag;


 if(pixmap>0){
	menu->items[number].pixmap=pixmap;
	menu->items[number].pix_width=pix_width;
	menu->items[number].pix_height=pix_height;
	menu->items[number].pix_depth=pix_depth;
       }
 if(pix_mask>0) menu->items[number].pix_mask=pix_mask;

 
  
 if((menu->items[number].flags&TextFlag)==TextFlag)
 {
  if((menu->items[number].flags&TextFlag)==TextFlag&&menu->items[number].text!=NULL&&strlen(menu->items[number].text)>0)
    tw=XTextWidth(menu->items[number].font,menu->items[number].text,strlen(menu->items[number].text));
  else tw=0;
  mw=tw+MN_INTERMARGE+pix_width;
  if(menu->items[number].font->ascent+menu->items[number].font->descent>pix_height+4) mh=menu->items[number].font->ascent+menu->items[number].font->descent+2;
  else mh=pix_height+4;
 }

 else {
  mw=pix_width;
  mh=pix_height+4;
 }


  if(mh<10) mh=10;
  if(mw>menu->maxwidth){
 	menu->maxwidth=mw;
	if(mw+2*MN_LMARGE>menu->width) menu->width=mw+2*MN_LMARGE;
 	}


 menu->items[number].width=mw;
 menu->items[number].height=mh;

 MN_AutoSetDimensions(tk_display,menuid,delay);


 if(delay==False) {
	mn_DrawFloatingMenu(tk_display,menuid);
	}

  }

 }
 return 0;
}





/*
 *
 * Initialise le pixmap grayed d'un element
 *
 *
 */

int MN_SetItemPixmapGrayed(tk_display,menuid,number, pix_grayed, pix_maskgrayed,delay)
TkDisplay *tk_display;
MenuID menuid;
int number;
Pixmap pix_grayed, pix_maskgrayed;
Bool delay;
{
 int tw, mw, mh;
 int i, h;
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


if(number>=0 && number<menu->numitems && menu->type==MN_FLOATING)
 { 

 if((menu->items[number].flags&PixmapGrayedFlag)!=PixmapGrayedFlag && pix_grayed>0) menu->items[number].flags=menu->items[number].flags+PixmapGrayedFlag;
 if((menu->items[number].flags&PixmapMaskGrayedFlag)!=PixmapMaskGrayedFlag && pix_maskgrayed>0) menu->items[number].flags=menu->items[number].flags+PixmapMaskGrayedFlag;


 if(pix_grayed>0)
	menu->items[number].pix_grayed=pix_grayed;
 if(pix_maskgrayed>0) menu->items[number].pix_maskgrayed=pix_maskgrayed;
 

 if(delay==False){
	mn_DrawFloatingMenu(tk_display,menuid);
	}

 }
 return 0;
}





/*
 *
 * Change la precedence des composants d'un element
 *
 *
 */

int MN_SetPrecedency(tk_display,menuid,number, precedency, delay)
MenuID menuid;
TkDisplay *tk_display;
int number, precedency;
Bool delay;
{
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


 if(number>=0 && number<menu->numitems && menu->type==MN_FLOATING)
  {
     if((precedency==TextFlag && (menu->items[number].flags&TextFlag)==TextFlag) || (precedency==PixmapFlag && (menu->items[number].flags&PixmapFlag)==PixmapFlag)){
 	menu->items[number].precedency=precedency;
   	}
 if(delay==False) {
	mn_DrawFloatingMenu(tk_display,menuid);
	}
  }
 return 0;

}






int MN_SetFont(tk_display,menuid,font,delay)
MenuID menuid;
TkDisplay *tk_display;
XFontStruct *font;
Bool delay;
{
 MenuStruct *menu;

 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;

 if(font>(XFontStruct *)NULL) menu->font=font;
 
 if(delay==False)
 {
   MN_AutoSetDimensions(tk_display,menuid,delay);
   if(menu->type==MN_FLOATING) return mn_DrawFloatingMenu(tk_display,menuid);
   if(menu->type==MN_MENUBAR) return mn_DrawMenuBar(tk_display,menuid);
 }
 else return MN_AutoSetDimensions(tk_display,menuid,delay);
}







int MN_AutoSetDimensions(tk_display,menuid,delay)
MenuID menuid;
TkDisplay *tk_display;
Bool delay;
{
 int i, j, h, w;
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;

 

if(menu->type==MN_FLOATING)
 {
  j=2*MN_UMARGE;
  for(i=0;i<menu->numitems;i++)
  switch(menu->items[i].type)
  {

	case MN_SUBMENU:
	case MN_ITEM:	h=0;
			if((menu->items[i].flags&TextFlag)==TextFlag) 
				h=menu->items[i].font->ascent+menu->items[i].font->descent+2;
			if((menu->items[i].flags&PixmapFlag)==PixmapFlag&&menu->items[i].pix_height+4>h) 
				h=menu->items[i].pix_height+4;
			if(j<10) j=10;
			menu->items[i].y=j-MN_UMARGE;
			menu->items[i].height=h;
			j=j+h;
			break;

	case MN_HBAR:
			h=7;
			menu->items[i].y=j-MN_UMARGE;
			menu->items[i].height=h;			
			j=j+h;
			break;

  }

  menu->height=j;

  /*if(delay==False)*/
   XResizeWindow(tk_display->display,menu->window,menu->width,menu->height);

 }
else if(menu->type==MN_MENUBAR)
 {
  j=0-MN_INTERMARGE/2;
  for(i=0;i<menu->numitems;i++)
  switch(menu->items[i].type)
  {

	case MN_SUBMENU:
	case MN_ITEM:	w=0;
			if((menu->items[i].flags&TextFlag)==TextFlag&&menu->items[i].text!=NULL&&strlen(menu->items[i].text)>0)
			  w=XTextWidth(menu->font,menu->items[i].text,strlen(menu->items[i].text));
			else w=0;
	
			if(w<10) w=10;
			if(j>0) menu->items[i].x=j;
			else menu->items[i].x=0;
			menu->items[i].width=w;
			j=j+w+2*MN_INTERMARGE;
			break;



  }


  /*if(delay==False)
   XMoveResizeWindow(tk_display->display,menu->window,menu->x,menu->y,menu->width,menu->height);*/

 }
return 0;

}






int MN_GrayItem(tk_display,menuid,number)
TkDisplay *tk_display;
MenuID menuid;
int number;
{
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


 if(number>=0 && number<menu->numitems && (menu->items[number].state&Grayed)!=Grayed)
 {
  menu->items[number].state=menu->items[number].state+Grayed;
  if(menu->items[number].type==MN_SUBMENU)
    MN_Gray(tk_display,menu->items[number].submenu);
  if(menu->type==MN_FLOATING) mn_DrawFloatingItem(tk_display,menuid,number);
  else if(menu->type==MN_MENUBAR) mn_DrawMenuBarItem(tk_display,menuid,number);
  else fprintf(stderr,"Type de menu inconnu\n");
  return 0;
 }
 return -1;
}




int MN_UngrayItem(tk_display,menuid,number)
TkDisplay *tk_display;
MenuID menuid;
int number;
{
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


 if(number>=0 && number<menu->numitems && (menu->items[number].state&Grayed)==Grayed)
 {
  menu->items[number].state=menu->items[number].state-Grayed;
  if(menu->items[number].type==MN_SUBMENU)
    MN_Ungray(tk_display,menu->items[number].submenu);
  if(menu->type==MN_FLOATING) mn_DrawFloatingItem(tk_display,menuid,number);
  else if(menu->type==MN_MENUBAR) mn_DrawMenuBarItem(tk_display,menuid,number);
  else fprintf(stderr,"Type de menu inconnu\n");    
 }
 return 0;
}





int MN_CheckItem(tk_display,menuid,number)
TkDisplay *tk_display;
MenuID menuid;
int number;
{
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;

 if(menu->type==MN_FLOATING && menu->numitems>0 && number>=0 && number<menu->numitems && (menu->items[number].state&Checked)!=Checked)
 {
   menu->items[number].state=menu->items[number].state+Checked;
   mn_DrawFloatingItem(tk_display,menuid,number);
   return 0;
 }
 else if(menu->type==MN_FLOATING && menu->numitems>0 && number==END && (menu->items[menu->numitems-1].state&Checked)!=Checked)
 {
   number=menu->numitems-1;
   menu->items[number].state=menu->items[number].state+Checked;
   mn_DrawFloatingItem(tk_display,menuid,number);
   return 0;
 }
 return -1;
}





	int MN_UncheckItem(tk_display,menuid,number)
TkDisplay *tk_display;
MenuID menuid;
int number;
{
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


 if(menu->type==MN_FLOATING && menu->numitems>0 && number>=0 && number<menu->numitems && (menu->items[number].state&Checked)==Checked)
 {
   menu->items[number].state=menu->items[number].state-Checked;
   mn_DrawFloatingItem(tk_display,menuid,number);
   return 0;
 }
 if(menu->type==MN_FLOATING && menu->numitems>0 && number==END && (menu->items[menu->numitems-1].state&Checked)==Checked)
 {
   number=menu->numitems-1;
   menu->items[number].state=menu->items[number].state-Checked;
   mn_DrawFloatingItem(tk_display,menuid,number);
   return 0;
 } 
 return -1;
}





int MN_Gray(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;


 if((menu->state&Grayed)!=Grayed)
 {
   menu->state=menu->state+Grayed;
   if(menu->type==MN_FLOATING) mn_DrawFloatingMenu(tk_display,menuid);
   else if(menu->type==MN_MENUBAR) mn_DrawMenuBar(tk_display,menuid);
 }
 return 0;
}





int MN_Ungray(tk_display,menuid)
TkDisplay *tk_display;
MenuID menuid;
{
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;

 if((menu->state&Grayed)==Grayed)
 {
   menu->state=menu->state-Grayed;
   if(menu->type==MN_FLOATING) mn_DrawFloatingMenu(tk_display,menuid);
   else if(menu->type==MN_MENUBAR) mn_DrawMenuBar(tk_display,menuid);
 }
 return 0;
}





int MN_Move(tk_display,menuid,x,y)
TkDisplay *tk_display;
MenuID menuid;
int x, y;
{
 MenuStruct *menu, *submenu;


 if(menuid>=0 && menuid<tk_display->maxwidgets && tk_display->widgets[menuid].class==WI_MENU&&tk_display->widgets[menuid].menu!=NULL)
   menu=tk_display->widgets[menuid].menu;
 else return -1;

 if(menu->type==MN_FLOATING&&menu->parency_class<=0)
   XMoveWindow(tk_display->display,menu->window,x,y);
 
 return 0;
}








