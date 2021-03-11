/*
 *
 * 	wid_get.c  
 * 	informations sur les widgets
 *
 * 	Modification :  09/12/93
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>



WidgetID *wid_GetSubWidgets(tk_display,widgetid,numsubwidgets)
TkDisplay *tk_display;
WidgetID widgetid;
unsigned int *numsubwidgets;
{
  WidgetID *subwidgetsid;
  WidgetStruct *widptr;
  int ret;


  if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
  {
    widptr=&tk_display->widgets[widgetid];

    switch(tk_display->widgets[widgetid].class)
    {

	case WI_MENU:
	case WI_EDIT:
	case WI_BUTTON :
	   (*numsubwidgets)=0;
	   return (WidgetID *)NULL;
	   break;


	case WI_SCROLLBAR:
	   if(widptr->type==SB_HTHUMB  || widptr->type==SB_VTHUMB)
    	   {
		(*numsubwidgets)=1;
		subwidgetsid=(WidgetID *)malloc(sizeof(WidgetID)*1);
		subwidgetsid[0]=widptr->scroll->bn_thumb;
		return (WidgetID *)subwidgetsid;
		break;
	   }
	   else
   	   {
		(*numsubwidgets)=3;
		subwidgetsid=(WidgetID *)malloc(sizeof(WidgetID)*3);
		subwidgetsid[0]=widptr->scroll->bn_thumb;
		subwidgetsid[1]=widptr->scroll->B1;
		subwidgetsid[2]=widptr->scroll->B2;
		return (WidgetID *)subwidgetsid;
		break;
	   }
	   break;


	case WI_LIST :
	   if(widptr->type==LS_SIMPLE)
    	   {
		(*numsubwidgets)=0;
		return (WidgetID *)NULL;
		break;
	   }
	   else if(widptr->type==LS_HSCROLL  || widptr->type==LS_LEFTVSCROLL || widptr->type==LS_RIGHTVSCROLL)
    	   {
		(*numsubwidgets)=1;
		subwidgetsid=(WidgetID *)malloc(sizeof(WidgetID)*1);
		subwidgetsid[0]=widptr->list->SBV;
		if(widptr->type==LS_HSCROLL) subwidgetsid[0]=widptr->list->SBH;
		return (WidgetID *)subwidgetsid;
		break;
	   }
	   else
   	   {
		(*numsubwidgets)=2;
		subwidgetsid=(WidgetID *)malloc(sizeof(WidgetID)*2);
		subwidgetsid[0]=widptr->list->SBH;
		subwidgetsid[1]=widptr->list->SBV;
		return (WidgetID *)subwidgetsid;
		break;
	   }
	   break;


	case WI_COMBO :
	   (*numsubwidgets)=3;
	   subwidgetsid=(WidgetID *)malloc(sizeof(WidgetID)*3);
	   subwidgetsid[0]=widptr->combo->button;
	   subwidgetsid[1]=widptr->combo->edit;
	   subwidgetsid[2]=widptr->combo->list;
	   return (WidgetID *)subwidgetsid;
	   break;


	default :
	   (*numsubwidgets)=0;
	   return (WidgetID *)NULL;
	   break;
 
    }
  }
  
  return (WidgetID *)NULL;
}





Bool WID_IsSubwidget(tk_display,wid1,wid2)
TkDisplay *tk_display;
WidgetID wid1, wid2;
{
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 EditStruct *edit;
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;


 ButtonID buttonid;
 ScrollbarID scrollid;
 EditID editid;
 ListID listid;
 ComboID comboid;
 MenuID  menuid;

 int ret;
 int i, j;


 if(wid1>=0 && wid1<tk_display->maxwidgets && tk_display->widgets[wid1].isUsed==True&&wid2>=0 && wid2<tk_display->maxwidgets && tk_display->widgets[wid2].isUsed==True)
 {

   switch(tk_display->widgets[wid1].class)
   {


	case WI_BUTTON :

	   buttonid=wid1;
	   if(tk_display->widgets[buttonid].class==WI_BUTTON)
	   {
	     	button=tk_display->widgets[buttonid].button; 

		if(button->parency_class<=0)
		  return False;
		else if(button->parency_class==WI_SCROLLBAR&&button->parency_number==wid2)
		  return True;
		else if(button->parency_class==WI_COMBO&&button->parency_number==wid2)
		  return True;
		else return WID_IsSubwidget(tk_display,button->parency_number,wid2);
	   }
	   else return False;
	   break;



	case WI_SCROLLBAR:

	   scrollid=wid1;
	   if(tk_display->widgets[scrollid].class==WI_SCROLLBAR)
	   {
		scroll=tk_display->widgets[scrollid].scroll;

		if(scroll->parency_class<=0)
		  return False;
		else if(scroll->parency_class==WI_LIST&&scroll->parency_number==wid2)
		  return True;
		else if(scroll->parency_class==WI_COMBO&&scroll->parency_number==wid2)
		  return True;
		else return WID_IsSubwidget(tk_display,scroll->parency_number,wid2);
	   }
	   else return False;
	   break;



	case WI_EDIT :

	   editid=wid1;
	   if(tk_display->widgets[editid].class==WI_EDIT)
	   {
		edit=tk_display->widgets[editid].edit;

		if(edit->parency_class<=0)
		  return False;
		else if(edit->parency_class==WI_COMBO&&edit->parency_number==wid2)
		  return True;
		else return WID_IsSubwidget(tk_display,edit->parency_number,wid2);
	   }
	   else return False;	   
	   break;



	case WI_LIST :

	   listid=wid1;
	   if(tk_display->widgets[listid].class==WI_LIST)
	   {
	     	list=tk_display->widgets[listid].list;

		if(list->parency_class<=0)
		  return False;
		else if(list->parency_class==WI_COMBO&&list->parency_number==wid2)
		  return True;
		else return WID_IsSubwidget(tk_display,list->parency_number,wid2);
	   }
	   else return False;
	   break;



	case WI_COMBO :
	case WI_MENU :

	   return False;
	   break;


	default: return False;
		 break;

   }
   return False;
 }
 fprintf(stderr,"Fin … -1\n");
 return False;

}




/* 
 *
 * Obtient le type de la fenetre
 *
 *
 */

int WID_GetWindowType(tk_display, window)
TkDisplay *tk_display;
Window window;
{
 int i;

 
 for(i=0;i<tk_display->maxwidgets;i++)
 {
  if(tk_display->widgets[i].isUsed==True) switch(tk_display->widgets[i].class)
  {
    case WI_BUTTON: if(tk_display->widgets[i].button->window==window)
 		      return WI_BUTTON;
		    break;

    case WI_SCROLLBAR : if(tk_display->widgets[i].scroll->mainwindow==window||tk_display->widgets[i].scroll->thumbwindow==window)
 		      return WI_SCROLLBAR;
		    break;

    case WI_EDIT : if(tk_display->widgets[i].edit->window==window)
 		      return WI_EDIT;
		    break;

    case WI_LIST : if(tk_display->widgets[i].list->mainwindow==window||tk_display->widgets[i].list->listwindow==window)
 		      return WI_LIST;
		    break;

    case WI_COMBO : if(tk_display->widgets[i].combo->window==window)
 		      return WI_COMBO;
		    break;

    case WI_MENU : if(tk_display->widgets[i].menu->window==window)
 		      return WI_MENU;
		    break;

  }
 }
 
 return -1;

}






/* 
 *
 * Obtient le numero du widget
 * correspondant a la fenetre
 *
 */

int WID_GetWindowNumber(tk_display, window)
TkDisplay *tk_display;
Window window;
{
 int i;

 
 for(i=0;i<tk_display->maxwidgets;i++)
 {
  if(tk_display->widgets[i].isUsed==True) switch(tk_display->widgets[i].class)
  {
    case WI_BUTTON: if(tk_display->widgets[i].button->window==window)
 		      return i;
		    break;

    case WI_SCROLLBAR : if(tk_display->widgets[i].scroll->mainwindow==window||tk_display->widgets[i].scroll->thumbwindow==window)
 		      return i;
		    break;

    case WI_EDIT : if(tk_display->widgets[i].edit->window==window)
 		      return i;
		    break;

    case WI_LIST : if(tk_display->widgets[i].list->mainwindow==window||tk_display->widgets[i].list->listwindow==window)
 		      return i;
		    break;

    case WI_COMBO : if(tk_display->widgets[i].combo->window==window)
 		      return i;
		    break;

    case WI_MENU : if(tk_display->widgets[i].menu->window==window)
 		      return i;
		    break;

  }
 }
 return -1;

}





/* 
 *
 * Obtient le numero du premier widget libre
 * 
 *
 */

int WID_GetUnusedNumber(tk_display)
TkDisplay *tk_display;
{
 int i;

 WID_GET_START:

 if(tk_display->numwidgets>=tk_display->maxwidgets)
   WID_Add(tk_display,20);
 
 for(i=0;i<tk_display->maxwidgets;i++)
   if(tk_display->widgets[i].isUsed==False) return i;
 
 fprintf(stderr,"Plus de widget libre num=%d  max=%d\n",tk_display->numwidgets,tk_display->maxwidgets);
 WID_Add(tk_display,20);
 goto WID_GET_START;

}






int wid_GetState(tk_display,widgetid)
TkDisplay *tk_display;
WidgetID widgetid;
{

 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {
	case WI_BUTTON :
		  return BN_GetState(tk_display,widgetid);
		  break;

	case WI_SCROLLBAR :
		  return SB_GetState(tk_display,widgetid);
		  break;

	case WI_EDIT :
		  return ED_GetState(tk_display,widgetid);
		  break;

	case WI_LIST :
		  return LS_GetState(tk_display,widgetid);
		  break;

	case WI_COMBO :
		  return CB_GetState(tk_display,widgetid);
		  break;

	case WI_MENU :
		  return MN_GetState(tk_display,widgetid);
		  break;

	default : return -1;
 		  break;

   }

 }
 else return -1;
}






int wid_GetText(tk_display,widgetid,text)
TkDisplay *tk_display;
WidgetID widgetid;
unsigned char **text;
{
 ButtonStruct *button;
 ComboStruct *combo;
 EditStruct *edit;


 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {
	case WI_BUTTON :
		  if(tk_display->widgets[widgetid].class==WI_BUTTON)
		  {
		    button=tk_display->widgets[widgetid].button;
		    if((button->flags&TextFlag)==TextFlag)
		    {
		    	*text=(unsigned char *)malloc(strlen(button->text+1));
			*text=(unsigned char *)memset(*text,0,strlen(button->text)+1);
			memcpy(*text,button->text,strlen(button->text));
			return 0;
		    }
		    *text=NULL;
		    return 0;
		  }
		  else return -1;
		  break;


	case WI_LIST :
	case WI_MENU :
	case WI_SCROLLBAR :
		  *text=NULL;
		  return 0;
		  break;


	case WI_EDIT :
		  if(tk_display->widgets[widgetid].class==WI_EDIT)
		  {
		    edit=tk_display->widgets[widgetid].edit;
		    *text=(unsigned char *)malloc(153);		  
		    *text=(unsigned char *)memset(*text,0,153);
		    memcpy(*text,edit->text,152);
		    return 0;
		  }
		  else return -1;
		  break;


	case WI_COMBO :
		  if(tk_display->widgets[widgetid].class==WI_COMBO)
		    combo=tk_display->widgets[widgetid].combo;
		  else return -1;

		  if(tk_display->widgets[combo->edit].class==WI_EDIT)
		  {
		    edit=tk_display->widgets[combo->edit].edit;
		    *text=(unsigned char *)malloc(153);		  
		    *text=(unsigned char *)memset(*text,0,153);
		    memcpy(*text,edit->text,152);
		    return 0;
		  }
		  else return -1;
		  break;


	default : return -1;
 		  break;

   }

 }
 else return -1;
}





int wid_GetPosition(tk_display,widgetid)
TkDisplay *tk_display;
WidgetID widgetid;
{

 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {
	case WI_BUTTON :
		  return -1;
		  break;

	case WI_SCROLLBAR :
		  return SB_GetPosition(tk_display,widgetid);
		  break;

	case WI_EDIT :
		  return -1;
		  break;

	case WI_LIST :
		  return LS_GetItemSelected(tk_display,widgetid);
		  break;

	case WI_COMBO :
		  return CB_GetItemSelected(tk_display,widgetid);
		  break;

	case WI_MENU :
		  return MN_GetItemSelected(tk_display,widgetid);
		  break;

	default : return -1;
 		  break;

   }

 }
 else return -1;
}





int wid_GetAttributes(tk_display,widgetid,wid_attributes)
TkDisplay *tk_display;
WidgetID widgetid;
WidgetAttributes *wid_attributes;
{
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 EditStruct *edit;
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;


 ButtonID buttonid;
 ScrollbarID scrollid;
 EditID editid;
 ListID listid;
 ComboID comboid;
 MenuID  menuid;

 int ret;



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {

   switch(tk_display->widgets[widgetid].class)
   {


	case WI_BUTTON :

	   buttonid=widgetid;
	   if(tk_display->widgets[buttonid].class==WI_BUTTON)
	   {
	     	button=tk_display->widgets[buttonid].button; 

		wid_attributes->lighting=button->lighting;
		wid_attributes->neverFocus=button->neverFocus;
		wid_attributes->crosstype=button->crosstype;
		wid_attributes->mask=SALighting|SANeverFocus|SACrossType;		
		return 0;
	   }
	   else return -1;
	   break;



	case WI_SCROLLBAR:

	   scrollid=widgetid;
	   if(tk_display->widgets[scrollid].class==WI_SCROLLBAR)
	   {
		scroll=tk_display->widgets[scrollid].scroll;

 	     	wid_attributes->range=scroll->range;
		wid_attributes->pagerange=scroll->pagerange;
 		wid_attributes->thumbsize=scroll->thumbsize;
	        wid_attributes->position=scroll->position;
		wid_attributes->mask=SARange|SAPagerange|SAThumbsize|SAPosition;
	        return 0;
	   }
	   else return -1;
	   break;



	case WI_EDIT :

	   editid=widgetid;
	   if(tk_display->widgets[editid].class==WI_EDIT)
	   {
	     wid_attributes->mask=0;
	     return 0;
	   }
	   else return -1;
	   break;



	case WI_LIST :

	   listid=widgetid;
	   if(tk_display->widgets[listid].class==WI_LIST)
	   {
	     	list=tk_display->widgets[listid].list;

 	     	wid_attributes->position=LS_GetItemSelected(tk_display,listid);
	     	wid_attributes->multipleSelection=list->multipleSelection;
 		wid_attributes->mask=SAPosition|SAMultipleSelection;
	     	return 0;
	   }
	   else return -1;
	   break;



	case WI_COMBO :

	   comboid=widgetid;
	   if(tk_display->widgets[comboid].class==WI_COMBO)
	   {
	     	combo=tk_display->widgets[comboid].combo;
		list=tk_display->widgets[combo->list].list;

		wid_attributes->multipleSelection=list->multipleSelection;
 		wid_attributes->position=CB_GetItemSelected(tk_display,listid);
		wid_attributes->mask=SAMultipleSelection|SAPosition;
		return 0;
	   }
	   return -1;
	   break;


	case WI_MENU :

	   menuid=widgetid;
	   if(tk_display->widgets[menuid].class==WI_MENU)
	   {
	     wid_attributes->mask=0;
	     return 0;
	   }
	   else return -1;
	   break;


	default: return -1;
		 break;

   }
   return 0;
 }
 fprintf(stderr,"Fin … -1\n");
 return -1;

}






int wid_GetSelectedItems(tk_display,widgetid,indexes,numitems)
TkDisplay *tk_display;
WidgetID widgetid;
unsigned int **indexes;
int *numitems;
{
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 EditStruct *edit;
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;


 ButtonID buttonid;
 ScrollbarID scrollid;
 EditID editid;
 ListID listid;
 ComboID comboid;
 MenuID  menuid;

 unsigned int numindexes, maxindexes;
 int i, j, ret;
 



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {

   switch(tk_display->widgets[widgetid].class)
   {


	case WI_BUTTON :

	   buttonid=widgetid;
	   if(tk_display->widgets[buttonid].class==WI_BUTTON)
	   {
	     	*numitems=(int)0; 
		*indexes=(unsigned int *)NULL;
		return 0;
	   }
	   else return -1;
	   break;



	case WI_SCROLLBAR:

	   scrollid=widgetid;
	   if(tk_display->widgets[scrollid].class==WI_SCROLLBAR)
	   {
	     	*numitems=(int)0; 
		*indexes=(unsigned int *)NULL;
	        return 0;
	   }
	   else return -1;
	   break;



	case WI_EDIT :

	   editid=widgetid;
	   if(tk_display->widgets[editid].class==WI_EDIT)
	   {
	     	*numitems=(int)0; 
		*indexes=(unsigned int *)NULL;
	     	return 0;
	   }
	   else return -1;
	   break;



	case WI_LIST :

	   listid=widgetid;
	   if(tk_display->widgets[listid].class==WI_LIST)
	   {
	     	list=tk_display->widgets[listid].list;
		numindexes=maxindexes=0;
	     	*numitems=(int)0; 
		*indexes=(unsigned int *)malloc(sizeof(unsigned int)*10);
		if(*indexes==NULL)
		{
		  fprintf(stderr,"Erreur: Pas assez de memoire\n");
		  return -1;
		}
		maxindexes=10;
		if(list->numitems>0 && list->selected==True)
		{
		  (*indexes)[0]=LS_GetItemSelected(tk_display,listid);
		  (*indexes)[2]=2;
		  numindexes=1;
		  for(i=0;i<list->numitems;i++)
		  {
		    /*fprintf(stderr,"\ni=%d  numindexes=%d",i,numindexes);*/
		    if(numindexes>=maxindexes)
		    {
			*indexes=(unsigned int *)realloc(*indexes,sizeof(unsigned int)*(maxindexes+10));
			maxindexes=maxindexes+10;
		    }
		    if(i!=(*indexes)[0]&&LS_IsItemSelected(tk_display,listid,i)==True)
		    {
			
			(*indexes)[numindexes]=i;
			numindexes=numindexes+1;
		    }
		  }
		  *numitems=numindexes;
		}
		else free(*indexes);
	     	return 0;
	   }
	   else return -1;
	   break;



	case WI_COMBO :

	   comboid=widgetid;
	   if(tk_display->widgets[comboid].class==WI_COMBO)
	   {
	     	combo=tk_display->widgets[comboid].combo;
		return wid_GetSelectedItems(tk_display,combo->list,indexes,numitems);
	   }
	   return -1;
	   break;


	case WI_MENU :

	   menuid=widgetid;
	   if(tk_display->widgets[menuid].class==WI_MENU)
	   {
	     menu=tk_display->widgets[menuid].menu;
	     *indexes=(unsigned int *)malloc(sizeof(unsigned int)); 
	     *numitems=1;
	     ret=MN_GetItemSelectedNumber(tk_display,menuid);
	     if(ret>=0 && ret<menu->numitems)
	     { 
		(*indexes)[0]=ret;
	     	return 0;
	     }
	     else
	     {
		*numitems=0;
		return 0;
	     }
	   }
	   else return -1;
	   break;


	default: return -1;
		 break;

   }
   return 0;
 }
 fprintf(stderr,"Fin … -1\n");
 return -1;

}





int wid_GetNumItems(tk_display,widgetid)
TkDisplay *tk_display;
WidgetID widgetid;
{
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 EditStruct *edit;
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;


 ButtonID buttonid;
 ScrollbarID scrollid;
 EditID editid;
 ListID listid;
 ComboID comboid;
 MenuID  menuid;

 unsigned int numindexes, maxindexes;
 int i, j, ret;
 



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {

   switch(tk_display->widgets[widgetid].class)
   {


	case WI_BUTTON :

	   buttonid=widgetid;
	   if(tk_display->widgets[buttonid].class==WI_BUTTON)
	     return 0;
	   else return -1;
	   break;



	case WI_SCROLLBAR:

	   scrollid=widgetid;
	   if(tk_display->widgets[scrollid].class==WI_SCROLLBAR)
	     return 0;
	   else return -1;
	   break;



	case WI_EDIT :

	   editid=widgetid;
	   if(tk_display->widgets[editid].class==WI_EDIT)
	     return 0;
	   else return -1;
	   break;



	case WI_LIST :

	   listid=widgetid;
	   if(tk_display->widgets[listid].class==WI_LIST)
	     return LS_GetNumitems(tk_display,listid);
	   else return -1;
	   break;



	case WI_COMBO :

	   comboid=widgetid;
	   if(tk_display->widgets[comboid].class==WI_COMBO)
	     return CB_GetNumitems(tk_display,comboid);
	   return -1;
	   break;


	case WI_MENU :

	   menuid=widgetid;
	   if(tk_display->widgets[menuid].class==WI_MENU)
	     return MN_GetNumitems(tk_display,menuid);
	   else return -1;
	   break;


	default: return -1;
		 break;

   }
   return 0;
 }
 fprintf(stderr,"Fin … -1\n");
 return -1;

}






int wid_GetItem(tk_display,widgetid,number,widgetitem)
TkDisplay *tk_display;
WidgetID widgetid;
unsigned int number;
WidgetItem *widgetitem;
{
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 EditStruct *edit;
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;


 ButtonID buttonid;
 ScrollbarID scrollid;
 EditID editid;
 ListID listid;
 ComboID comboid;
 MenuID  menuid;

 unsigned int numindexes, maxindexes;
 int i, j, ret;
 



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {

   switch(tk_display->widgets[widgetid].class)
   {


	case WI_BUTTON :

	   buttonid=widgetid;
	   if(tk_display->widgets[buttonid].class==WI_BUTTON)
	     return 0;
	   else return -1;
	   break;



	case WI_SCROLLBAR:

	   scrollid=widgetid;
	   if(tk_display->widgets[scrollid].class==WI_SCROLLBAR)
	     return 0;
	   else return -1;
	   break;



	case WI_EDIT :

	   editid=widgetid;
	   if(tk_display->widgets[editid].class==WI_EDIT)
	     return 0;
	   else return -1;
	   break;



	case WI_LIST :

	   listid=widgetid;
	   if(tk_display->widgets[listid].class==WI_LIST)
	   {
	     list=tk_display->widgets[listid].list;
	     if(number>=0 && number<list->numitems)
	     {
		widgetitem->number=number;
		widgetitem->flags=list->items[number].flags;
		widgetitem->type=0;
		widgetitem->state=list->items[number].state;
		widgetitem->precedency=list->items[number].precedency;
		widgetitem->x=list->items[number].x;
		widgetitem->y=list->items[number].y;
		widgetitem->width=list->items[number].width;
		widgetitem->height=list->items[number].height;
		widgetitem->text=list->items[number].text;
		widgetitem->font=list->items[number].font;
		widgetitem->pixmap=list->items[number].pixmap;
		widgetitem->pix_grayed=list->items[number].pix_grayed;
		widgetitem->pix_mask=list->items[number].pix_mask;
		widgetitem->pix_maskgrayed=list->items[number].pix_maskgrayed;
		widgetitem->pix_width=list->items[number].pix_width;
		widgetitem->pix_height=list->items[number].pix_height;
		widgetitem->pix_depth=list->items[number].pix_depth;
		widgetitem->selected=list->items[number].selected;
		widgetitem->submenu=0;
		return 0;
	     }
	     else return -1;
	   }
	   else return -1;
	   break;



	case WI_COMBO :

	   comboid=widgetid;
	   if(tk_display->widgets[comboid].class==WI_COMBO)
	     return wid_GetItem(tk_display,tk_display->widgets[comboid].combo->list,number,widgetitem);
	   return -1;
	   break;


	case WI_MENU :

	   menuid=widgetid;
	   if(tk_display->widgets[menuid].class==WI_MENU)
	   {
	     menu=tk_display->widgets[menuid].menu;
	     if(number>=0 && number<menu->numitems)
	     {
		widgetitem->number=number;
		widgetitem->flags=menu->items[number].flags;
		widgetitem->type=menu->items[number].type;
		widgetitem->state=menu->items[number].state;
		widgetitem->precedency=menu->items[number].precedency;
		widgetitem->x=menu->items[number].x;
		widgetitem->y=menu->items[number].y;
		widgetitem->width=menu->items[number].width;
		widgetitem->height=menu->items[number].height;
		widgetitem->text=menu->items[number].text;
		widgetitem->font=menu->items[number].font;
		widgetitem->pixmap=menu->items[number].pixmap;
		widgetitem->pix_grayed=menu->items[number].pix_grayed;
		widgetitem->pix_mask=menu->items[number].pix_mask;
		widgetitem->pix_maskgrayed=menu->items[number].pix_maskgrayed;
		widgetitem->pix_width=menu->items[number].pix_width;
		widgetitem->pix_height=menu->items[number].pix_height;
		widgetitem->pix_depth=menu->items[number].pix_depth;
		widgetitem->selected=menu->items[number].selected;
		widgetitem->submenu=menu->items[number].submenu;
		return 0;
	     }
	     else return -1;
	   }
	   else return -1;
	   break;


	default: return -1;
		 break;

   }
   return 0;
 }
 fprintf(stderr,"Fin … -1\n");
 return -1;

}




int wid_GetGeometry(tk_display,widgetid,parent,top_level,x,y,width,height)
TkDisplay *tk_display;
WidgetID widgetid;
Window *parent, *top_level;
int *x, *y;
unsigned int *width, *height;
{
 ButtonStruct *button;
 ScrollbarStruct *scroll;
 EditStruct *edit;
 ListStruct *list;
 ComboStruct *combo;
 MenuStruct *menu;


 ButtonID buttonid;
 ScrollbarID scrollid;
 EditID editid;
 ListID listid;
 ComboID comboid;
 MenuID  menuid;

 unsigned int numindexes, maxindexes;
 int i, j, ret;
 



 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {

   switch(tk_display->widgets[widgetid].class)
   {


	case WI_BUTTON :

	   buttonid=widgetid;
	   if(tk_display->widgets[buttonid].class==WI_BUTTON)
	   {
	     button=tk_display->widgets[buttonid].button;
	     *parent=button->parent;
	     *top_level=button->top_level;
	     *x=button->x;
	     *y=button->y;
	     *width=button->width;
	     *height=button->height;
	     return 0;
	   }
	   else return -1;
	   break;



	case WI_SCROLLBAR:

	   scrollid=widgetid;
	   if(tk_display->widgets[scrollid].class==WI_SCROLLBAR)
	   {
	     scroll=tk_display->widgets[scrollid].scroll;
	     *parent=scroll->parent;
	     *top_level=scroll->top_level;
	     *x=scroll->x;
	     *y=scroll->y;
	     *width=scroll->width;
	     *height=scroll->height;
	     return 0;
	   }
	   else return -1;
	   break;



	case WI_EDIT :

	   editid=widgetid;
	   if(tk_display->widgets[editid].class==WI_EDIT)
	   {
	     edit=tk_display->widgets[editid].edit;
	     *parent=edit->parent;
	     *top_level=edit->top_level;
	     *x=edit->x;
	     *y=edit->y;
	     *width=edit->width;
	     *height=edit->height;
	     return 0;
	   }
	   else return -1;
	   break;



	case WI_LIST :

	   listid=widgetid;
	   if(tk_display->widgets[listid].class==WI_LIST)
	   {
	     list=tk_display->widgets[listid].list;
	     *parent=list->parent;
	     *top_level=list->top_level;
	     *x=list->x;
	     *y=list->y;
	     *width=list->width;
	     *height=list->height;
	     return 0;
	   }
	   else return -1;
	   break;



	case WI_COMBO :

	   comboid=widgetid;
	   if(tk_display->widgets[comboid].class==WI_COMBO)
	   {
	     combo=tk_display->widgets[comboid].combo;
	     *parent=combo->parent;
	     *top_level=combo->top_level;
	     *x=combo->x;
	     *y=combo->y;
	     *width=combo->width;
	     *height=combo->height;
	     return 0;
	   }
	   return -1;
	   break;


	case WI_MENU :

	   menuid=widgetid;
	   if(tk_display->widgets[menuid].class==WI_MENU)
	   {
	     menu=tk_display->widgets[menuid].menu;
	     *parent=menu->parent;
	     *top_level=menu->top_level;
	     *x=menu->x;
	     *y=menu->y;
	     *width=menu->width;
	     *height=menu->height;
	     return 0;
	   }
	   else return -1;
	   break;


	default: return -1;
		 break;

   }
   return 0;
 }
 fprintf(stderr,"Fin … -1\n");
 return -1;
}



