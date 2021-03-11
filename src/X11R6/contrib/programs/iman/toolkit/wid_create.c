/*
 *
 * 	wid_create.c  
 * 	creation de widgets generiques
 *
 * 	Modification :  20/12/93
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



/*
#define DEBUG
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





			/**** Fonction generique ****/


WidgetID wid_Create(tk_display,widget_class,widget_type,wnd_parent,wnd_top_level,x,y,width,height,wid_attributes,state)
TkDisplay *tk_display;
int widget_class, widget_type;
Window wnd_parent, wnd_top_level;
int x, y;
unsigned int width, height;
WidgetAttributes *wid_attributes;
int state;
{
 WidgetID widgetid;
 ButtonID buttonid;
 ScrollbarID scrollid;
 EditID editid;
 ListID listid;
 ComboID comboid;
 MenuID  menuid;

 unsigned long mask;
 int ret;
 Colormap colormap;
 Visual *visual;
 Cursor cursor;

 int lighting;
 int direction;
 unsigned int range;
 unsigned int pagerange;
 unsigned int thumbsize;
 Bool neverFocus;
 Bool multipleSelection;
 Bool border;
 int vtype, htype;
 unsigned int itemheight;
 unsigned int position;
 unsigned int crosstype;

 char *text="";
 int key=-1;





 widgetid=WID_GetUnusedNumber(tk_display);


 if((wid_attributes->mask&SALighting)==SALighting) lighting=wid_attributes->lighting;
 else lighting=False;
 if((wid_attributes->mask&SADirection)==SADirection) direction=wid_attributes->direction;
 else direction=0;
 if((wid_attributes->mask&SARange)==SARange) range=wid_attributes->range;
 else range=0;
 if((wid_attributes->mask&SAPagerange)==SAPagerange) pagerange=wid_attributes->pagerange;
 else pagerange=10;
 if((wid_attributes->mask&SAThumbsize)==SAThumbsize) thumbsize=wid_attributes->thumbsize;
 else thumbsize=30;
 if((wid_attributes->mask&SANeverFocus)==SANeverFocus) neverFocus=wid_attributes->neverFocus;
 else neverFocus=False;
 if((wid_attributes->mask&SAMultipleSelection)==SAMultipleSelection) multipleSelection=wid_attributes->multipleSelection;
 else multipleSelection=False;
 if((wid_attributes->mask&SABorder)==SABorder) border=wid_attributes->border;
 else border=True;
 if((wid_attributes->mask&SAHVType)==SAHVType)
 {
	  htype=wid_attributes->htype;
  	  vtype=wid_attributes->vtype;
 }
 else
 {
	  htype=SB_LEFTALIGN;
  	  vtype=SB_BOTTOMALIGN;
 }
 if((wid_attributes->mask&SAItemHeight)==SAItemHeight) itemheight=wid_attributes->itemheight;
 else itemheight=14;
 if((wid_attributes->mask&SAPosition)==SAPosition) position=wid_attributes->position;
 else position=-2;
 if((wid_attributes->mask&SACrossType)==SACrossType) crosstype=wid_attributes->crosstype;
 else crosstype=LittleCross;
 if((wid_attributes->mask&SAColormap)==SAColormap && wid_attributes->colormap>0) colormap=wid_attributes->colormap;
 else colormap=CopyFromParent;
 if((wid_attributes->mask&SAVisual)==SAVisual) visual=wid_attributes->visual;
 else visual=CopyFromParent;
 if((wid_attributes->mask&SACursor)==SACursor) cursor=wid_attributes->cursor;
 else cursor=tk_display->cursors.normal;




 if(widgetid>=0 && widgetid<tk_display->maxwidgets)
 {
   tk_display->widgets[widgetid].isUsed=True;
   tk_display->widgets[widgetid].number=widgetid;

   switch(widget_class)
   {


	case WI_BUTTON :
#ifdef DEBUG
	   fprintf(stderr,"BN create  %d\n",tk_display->numwidgets);
#endif
	   buttonid=widgetid;
	   tk_display->widgets[buttonid].class=WI_BUTTON;
	   tk_display->widgets[buttonid].type=widget_type; 
#ifdef DEBUG
	   fprintf(stderr,"BN avant malloc\n");
#endif
	   tk_display->widgets[buttonid].button=(ButtonStruct *)malloc(sizeof(ButtonStruct));
#ifdef DEBUG
	   fprintf(stderr,"BN apres malloc\n");
#endif
	   if(tk_display->widgets[buttonid].button==NULL)
	   {
		fprintf(stderr,"Plus de memoire\n");
		tk_display->widgets[widgetid].isUsed=False;
	     	tk_CloseSession(tk_display);
	     	/***** ERREUR DE MEMOIRE *****/
	     	exit(-1);
	   }
#ifdef DEBUG
	   fprintf(stderr,"BN avant memset\n");
#endif
	   tk_display->widgets[buttonid].button=(ButtonStruct *)memset(tk_display->widgets[buttonid].button,0,sizeof(ButtonStruct));
	   tk_display->numwidgets++;	  
	   tk_display->widgets[buttonid].identity.colormap=colormap;
	   tk_display->widgets[buttonid].identity.visual=visual;
	   tk_display->widgets[buttonid].identity.cursor=cursor;
	   tk_display->widgets[buttonid].colors=&tk_display->bn_colors;


#ifdef DEBUG
	   fprintf(stderr,"BN attention !!! %d\n", widget_type);
#endif

	   switch(widget_type){

		case BN_PUSHBUTTON: 	bn_CreatePushButton(tk_display,buttonid,wnd_parent , wnd_top_level, x ,y ,width , height,(char *)text, key, state); 
				    	break;

		case BN_REPEATBUTTON: 	bn_CreateRepeatButton(tk_display,buttonid,wnd_parent ,wnd_top_level,x ,y ,width , height,(char *)text, key, state); 
				      	break;

		case BN_CROSSBUTTON: 	bn_CreateCrossButton(tk_display,buttonid,wnd_parent, wnd_top_level, x ,y , width, height,(char *)text, state); 
				     	break;

		case BN_POPUPBUTTON: 	bn_CreatePopupButton(tk_display,buttonid,wnd_parent ,wnd_top_level,x ,y ,width , height, text, key, state);
				     	break;

		case BN_RADIOBUTTON: 	bn_CreateRadioButton(tk_display,buttonid,wnd_parent, wnd_top_level, x ,y , width, height, text,state);
				     	break;

		case BN_CHECKBUTTON: 	bn_CreateCheckButton(tk_display,buttonid,wnd_parent, wnd_top_level, x ,y , width, height, text,state);
				     	break;

		case BN_POPUPRADIOBUTTON: bn_CreatePopupRadioButton(tk_display,buttonid,wnd_parent,wnd_top_level,x,y,width,height,text,state);
					  break;

		case BN_SCROLLBUTTON: 	bn_CreateScrollButton(tk_display,buttonid,wnd_parent,wnd_top_level,x,y,width,height,direction,state);
					break;

		case BN_THUMBBUTTON: 	bn_CreateThumbButton(tk_display,buttonid,wnd_parent,wnd_top_level,x,y,width,height,state);
					break;

		case BN_COMBOBUTTON: 	bn_CreateComboButton(tk_display,buttonid,wnd_parent,wnd_top_level,x,y,width,height,state);
					break;

		default: WID_Remove(tk_display,buttonid);
			 return -1;
			 break;

 	   }
	   tk_display->widgets[buttonid].identity.usePrivateColors=False;
#ifdef DEBUG
  fprintf(stderr,"bn_Create passe\n");
#endif
	   BN_SetNeverfocusFlag(tk_display,buttonid,neverFocus);
#ifdef DEBUG
  fprintf(stderr,"neverFocus passe\n");
#endif
	   BN_SetLighting(tk_display,buttonid,lighting,True);
#ifdef DEBUG
  fprintf(stderr,"lighting passe\n");
#endif
	   BN_SetCrossType(tk_display,buttonid,crosstype,True);
#ifdef DEBUG
  fprintf(stderr,"cross passe\n");
  fprintf(stderr,"win: %ld\n",tk_display->widgets[buttonid].button->window);
#endif
	   break;



	case WI_SCROLLBAR:
#ifdef DEBUG
	   fprintf(stderr,"SB create  %d\n",tk_display->numwidgets);
#endif
	   scrollid=widgetid;
	   tk_display->widgets[scrollid].class=WI_SCROLLBAR;
	   tk_display->widgets[scrollid].type=widget_type;

	   tk_display->widgets[scrollid].scroll=(ScrollbarStruct *)malloc(sizeof(ScrollbarStruct));
	   if(tk_display->widgets[scrollid].scroll==NULL)
	   {
		fprintf(stderr,"Plus de memoire\n");
		tk_display->widgets[widgetid].isUsed=False;
	     	tk_CloseSession(tk_display);
	     	/***** ERREUR DE MEMOIRE *****/
	     	exit(-1);
	   }
	   tk_display->widgets[scrollid].scroll=(ScrollbarStruct *)memset(tk_display->widgets[scrollid].scroll,0,sizeof(ScrollbarStruct));
	   tk_display->numwidgets++;
	   tk_display->widgets[scrollid].identity.colormap=colormap;
	   tk_display->widgets[scrollid].identity.cursor=cursor;
	   tk_display->widgets[scrollid].identity.visual=visual;
	   tk_display->widgets[scrollid].identity.usePrivateColors=False;
	   tk_display->widgets[scrollid].colors=&tk_display->sb_colors;


	   ret=SB_Create(tk_display,scrollid,widget_type,wnd_parent,wnd_top_level,x,y,width,height,state,range,pagerange,thumbsize);
	   if(ret==-1)
	   {
		WID_Remove(tk_display,scrollid);
		return -1;
	   }
	   if(position>0) SB_SetPosition(tk_display,scrollid,position);
	   break;



	case WI_EDIT :
#ifdef DEBUG
	   fprintf(stderr,"ED create  %d\n",tk_display->numwidgets);
#endif
	   if((wid_attributes->mask&SACursor)==SACursor) cursor=wid_attributes->cursor;
	     else cursor=tk_display->cursors.textedit;

	   editid=widgetid;
	   tk_display->widgets[editid].class=WI_EDIT;
	   tk_display->widgets[editid].type=widget_type;

	   tk_display->widgets[editid].edit=(EditStruct *)malloc(sizeof(EditStruct));
	   if(tk_display->widgets[editid].edit==NULL)
	   {
		fprintf(stderr,"Plus de memoire\n");
		tk_display->widgets[widgetid].isUsed=False;
	     	tk_CloseSession(tk_display);
	     	/***** ERREUR DE MEMOIRE *****/
	     	exit(-1);
	   }
	   tk_display->widgets[editid].edit=(EditStruct *)memset(tk_display->widgets[editid].edit,0,sizeof(EditStruct));
	   tk_display->numwidgets++;	   
	   tk_display->widgets[editid].identity.colormap=colormap;
	   tk_display->widgets[editid].identity.visual=visual;
	   tk_display->widgets[editid].identity.usePrivateColors=False;
	   tk_display->widgets[editid].identity.cursor=cursor;
	   tk_display->widgets[editid].colors=&tk_display->ed_colors;


	   ret=ED_Create(tk_display,editid,wnd_parent,wnd_top_level,x,y,width,height,text,border,widget_type,state);
	   if(ret==-1)
	   {
		WID_Remove(tk_display,widgetid);
		return -1;
	   }
	   break;



	case WI_LIST :
#ifdef DEBUG
	   fprintf(stderr,"LS create  %d\n",tk_display->numwidgets);
#endif
	   listid=widgetid;
	   tk_display->widgets[listid].class=WI_LIST;
	   tk_display->widgets[listid].type=widget_type;
	   tk_display->widgets[listid].list=(ListStruct *)malloc(sizeof(ListStruct));
	   if(tk_display->widgets[listid].list==NULL)
	   {
		fprintf(stderr,"Plus de memoire\n");
		tk_display->widgets[widgetid].isUsed=False;
	     	tk_CloseSession(tk_display);
	     	/***** ERREUR DE MEMOIRE *****/
	     	exit(-1);
	   }
	   tk_display->widgets[listid].list=(ListStruct *)memset(tk_display->widgets[listid].list,0,sizeof(ListStruct));
	   tk_display->numwidgets++;
	   tk_display->widgets[listid].identity.colormap=colormap;
	   tk_display->widgets[listid].identity.visual=visual;
   	   tk_display->widgets[listid].identity.usePrivateColors=False;
	   tk_display->widgets[listid].identity.cursor=cursor;
	   tk_display->widgets[listid].colors=&tk_display->ls_colors;

	   ret=LS_Create(tk_display,listid,wnd_parent,wnd_top_level,x,y,width,height,widget_type,state,htype,vtype,itemheight,tk_display->fonts.helvetica12,border);
	   if(ret==-1)
	   {
		WID_Remove(tk_display,comboid);
		return -1;
	   }
	   if(multipleSelection==True) LS_AllowMultipleSelection(tk_display,listid);
	   else LS_ForbidMultipleSelection(tk_display,listid);
	   if(position>=-1) LS_SetActiveItem(tk_display,listid,position);
 	   break;



	case WI_COMBO :
#ifdef DEBUG
	   fprintf(stderr,"CB create  %d\n",tk_display->numwidgets);
#endif
	   comboid=widgetid;
	   tk_display->widgets[comboid].class=WI_COMBO;
	   tk_display->widgets[comboid].type=widget_type;
	   tk_display->widgets[comboid].combo=(ComboStruct *)malloc(sizeof(ComboStruct));
	   if(tk_display->widgets[comboid].combo==NULL)
	   {
	     	fprintf(stderr,"Plus de memoire\n");
		tk_display->widgets[widgetid].isUsed=False;
	     	tk_CloseSession(tk_display);
	     	/***** ERREUR DE MEMOIRE *****/
	     	exit(-1);
	   }
	   tk_display->widgets[comboid].combo=(ComboStruct *)memset(tk_display->widgets[comboid].combo,0,sizeof(ComboStruct));
	   tk_display->numwidgets++;
	   tk_display->widgets[comboid].identity.colormap=colormap;
	   tk_display->widgets[comboid].identity.visual=visual;
	   tk_display->widgets[comboid].identity.usePrivateColors=False;
	   tk_display->widgets[comboid].identity.cursor=cursor;
	   tk_display->widgets[comboid].colors=&tk_display->ls_colors;

	   ret=CB_Create(tk_display,comboid,wnd_parent,wnd_top_level,x,y,width,height,widget_type,state,vtype,tk_display->fonts.helvetica12,itemheight);
	   if(ret==-1)
	   {
		WID_Remove(tk_display,comboid);
		return -1;
	   }
	   if(multipleSelection==True) CB_AllowMultipleSelection(tk_display,comboid);
	   else CB_ForbidMultipleSelection(tk_display,comboid);
	   if(position>=-1) CB_SetActiveItem(tk_display,listid,position);
	   break;


	case WI_MENU :
#ifdef DEBUG
	   fprintf(stderr,"MN create  %d\n",tk_display->numwidgets);
#endif
	   menuid=widgetid;
	   tk_display->widgets[menuid].class=WI_MENU;
	   tk_display->widgets[menuid].type=widget_type;
	   tk_display->widgets[menuid].menu=(MenuStruct *)malloc(sizeof(MenuStruct));
	   if(tk_display->widgets[menuid].menu==NULL)
	   {
	     	fprintf(stderr,"Plus de memoire\n");
		tk_display->widgets[widgetid].isUsed=False;
	     	tk_CloseSession(tk_display);
	     	/***** ERREUR DE MEMOIRE *****/
	     	exit(-1);
	   }
	   tk_display->widgets[menuid].menu=(MenuStruct *)memset(tk_display->widgets[menuid].menu,0,sizeof(MenuStruct));
	   tk_display->numwidgets++;
	   tk_display->widgets[menuid].identity.colormap=colormap;
	   tk_display->widgets[menuid].identity.visual=visual;
	   tk_display->widgets[menuid].identity.usePrivateColors=False;
	   tk_display->widgets[menuid].identity.cursor=cursor;
	   tk_display->widgets[menuid].colors=&tk_display->mn_colors;

#ifdef DEBUG
	   fprintf(stderr,"MN avant 1 \n");
#endif

	   switch(widget_type)
	   {
		case MN_FLOATING:
			ret=MN_CreateFloating(tk_display,menuid,wnd_top_level,0,0,x,y,"",tk_display->fonts.helvetica12,state);
			break;
		case MN_MENUBAR:
			ret=MN_CreateBar(tk_display,menuid,wnd_top_level,state);
			break;
	   }
	   if(ret==-1)
	   {
		WID_Remove(tk_display,menuid);
		return -1;
	   }
	   break;


	default: WID_Remove(tk_display,widgetid);
		 return -1;
		 break;

   }
#ifdef DEBUG
   fprintf(stderr,"widget cree %d\n",tk_display->numwidgets);
#endif
   return widgetid;
 }
 fprintf(stderr,"Mauvaise fin\n");
 return -1;

}






int wid_Destroy(tk_display,widgetid)
TkDisplay *tk_display;
WidgetID widgetid;
{
 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   if(tk_display->widgets[widgetid].identity.usePrivateColors==True)
     free(tk_display->widgets[widgetid].colors);

   switch(tk_display->widgets[widgetid].class)
   {
	case WI_BUTTON :
		  return BN_Destroy(tk_display,widgetid);
		  break;

	case WI_SCROLLBAR :
		  return SB_Destroy(tk_display,widgetid);
		  break;

	case WI_EDIT :
		  return ED_Destroy(tk_display,widgetid);
		  break;

	case WI_LIST :
		  return LS_Destroy(tk_display,widgetid);
		  break;

	case WI_COMBO :
		  return CB_Destroy(tk_display,widgetid);
		  break;

	case WI_MENU :
		  return MN_Destroy(tk_display,widgetid);
		  break;

	default : return -1;
 		  break;

   }

 }
 else return -1;
}



