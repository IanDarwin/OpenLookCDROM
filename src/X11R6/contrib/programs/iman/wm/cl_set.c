/*
 *
 * 	cl_set.c
 * 	modification des fenetres de couleur
 *
 * 	Modification :  31/01/94
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
 *      IMAN Window Manager version 1.2
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */



#define NEED_XRM_RESOURCES

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"


extern char *rgb_names[];







/*
 *
 * Boucle principale
 *
 *
 */

void CL_SetupDesk()
{
 int mask;
 int ret;
 int i, j, k, w, h;

 cl_index.current_widget=0;
 cl_index.current_item=-1;
 cl_index.current_color=-1;
 item_DeleteAll(tk_display,ls_colors_items);
 item_Add(tk_display,ls_colors_items,0,END,TextFlag,colors_desktop_text[0],0,0,0,True);
 item_Add(tk_display,ls_colors_items,0,END,TextFlag,colors_desktop_text[1],0,0,0,True);
 wid_Refresh(tk_display,ls_colors_items);
 wid_SetPosition(tk_display,ls_colors_rgb_names,cl_index.desk_fg);
 XSetWindowBackground(tk_display->display,color_rgb,cl_manager.desk_fg);
 XClearWindow(tk_display->display,color_rgb);


 XMapRaised(tk_display->display,color_desktop);
}



void CL_SetupWin()
{
 int ret;
 int i, j, k;
 char **str;

 cl_index.current_widget=1;
 cl_index.current_item=-1;
 cl_index.current_color=-1;
 item_DeleteAll(tk_display,ls_colors_items);
 if(LANGUAGE==0)
   str=(char **)us_window_colors_text;
 else str=(char **)fr_window_colors_text;
 for(i=0;i<(sizeof(WindowColors)/sizeof(unsigned long));i++)
   item_Add(tk_display,ls_colors_items,0,END,TextFlag,str[i],0,0,0,True);
 wid_Refresh(tk_display,ls_colors_items);
 wid_SetPosition(tk_display,ls_colors_rgb_names,cl_index.win_colors.bg);
 XSetWindowBackground(tk_display->display,color_rgb,cl_manager.win_colors.bg);
 XClearWindow(tk_display->display,color_rgb);

 XMapRaised(tk_display->display,color_window);
}




void CL_SetupDialog()
{
 int ret;
 int i, j, k;
 char **str;
 int gr; 

 cl_index.current_widget=2;
 cl_index.current_item=-1;
 cl_index.current_color=-1;
 item_DeleteAll(tk_display,ls_colors_items);
 if(LANGUAGE==0)
   str=(char **)us_window_colors_text;
 else str=(char **)fr_window_colors_text;
 for(i=0;i<(sizeof(WindowColors)/sizeof(unsigned long));i++)
 {   
     /*if(i==0 || i==1 || i==2|| i==3 || i==4)
	gr=0;
     else gr=Grayed;*/
     gr=0;
     if(i==5 || i==6)
	gr=Grayed;
     item_Add(tk_display,ls_colors_items,0,END,TextFlag,str[i],0,0,gr,True);
 }
 wid_Refresh(tk_display,ls_colors_items);
 wid_SetPosition(tk_display,ls_colors_rgb_names,cl_index.dlg_colors.bg);
 XSetWindowBackground(tk_display->display,color_rgb,cl_manager.dlg_colors.bg);
 XClearWindow(tk_display->display,color_rgb);

 XMapRaised(tk_display->display,color_dialog);
}




void CL_SetupIcon()
{
 int ret;
 int i, j, k;
 char **str;
 if(LANGUAGE==0)
   str=(char **)us_icon_colors_text;
 else str=(char **)fr_icon_colors_text;

 cl_index.current_widget=3;
 cl_index.current_item=-1;
 cl_index.current_color=-1;
 item_DeleteAll(tk_display,ls_colors_items);
 for(i=0;i<7;i++)
   item_Add(tk_display,ls_colors_items,0,END,TextFlag,str[i],0,0,0,True);
 wid_Refresh(tk_display,ls_colors_items);
 wid_SetPosition(tk_display,ls_colors_rgb_names,cl_index.icn_colors.icn_bg);
 XSetWindowBackground(tk_display->display,color_rgb,cl_manager.icn_colors.icn_bg);
 XClearWindow(tk_display->display,color_rgb);

 XMapRaised(tk_display->display,color_icon);
}



void CL_SetupButton()
{
 int ret;
 int i, j, k;
 char **str;
 int gr;

 cl_index.current_widget=4;
 cl_index.current_item=-1;
 cl_index.current_color=-1;
 item_DeleteAll(tk_display,ls_colors_items);
 if(LANGUAGE==0)
   str=(char **)us_widget_colors_text;
 else str=(char **)fr_widget_colors_text;
 for(i=0;i<(sizeof(WidgetColors)/sizeof(unsigned long));i++)
 {   
     if(i==0 || i==2 || i==3 || i==4 || i==5 || i==13 || i==14 || i==15 || i==16 || i==17 || i==18)
	gr=0;
     else gr=Grayed;
     item_Add(tk_display,ls_colors_items,0,END,TextFlag,str[i],0,0,gr,True);
 }
 wid_Refresh(tk_display,ls_colors_items);
 XSetWindowBackground(tk_display->display,color_rgb,cl_manager.bn_colors.bg);
 XClearWindow(tk_display->display,color_rgb);
 wid_SetPosition(tk_display,ls_colors_rgb_names,cl_index.bn_colors.bg);
 XMapRaised(tk_display->display,color_button);
}





void CL_SetupScroll()
{
 int ret;
 int i, j, k;
 int gr;
 char **str;

 cl_index.current_widget=5;
 cl_index.current_item=-1;
 cl_index.current_color=-1;
 item_DeleteAll(tk_display,ls_colors_items);
 if(LANGUAGE==0)
   str=(char **)us_widget_colors_text;
 else str=(char **)fr_widget_colors_text;
 for(i=0;i<(sizeof(WidgetColors)/sizeof(unsigned long));i++)
 {   
     if(i==0 || i==2)
	gr=0;
     else gr=Grayed;
     item_Add(tk_display,ls_colors_items,0,END,TextFlag,str[i],0,0,gr,True);
 }
 wid_Refresh(tk_display,ls_colors_items);
 XSetWindowBackground(tk_display->display,color_rgb,cl_manager.sb_colors.bg);
 XClearWindow(tk_display->display,color_rgb);
 wid_SetPosition(tk_display,ls_colors_rgb_names,cl_index.sb_colors.bg);

 XMapRaised(tk_display->display,color_scroll);
}



void CL_SetupEdit()
{
 int ret;
 int i, j, k;
 int gr;
 char **str;

 cl_index.current_widget=6;
 cl_index.current_item=-1;
 cl_index.current_color=-1;
 item_DeleteAll(tk_display,ls_colors_items);
 if(LANGUAGE==0)
   str=(char **)us_widget_colors_text;
 else str=(char **)fr_widget_colors_text;
 for(i=0;i<(sizeof(WidgetColors)/sizeof(unsigned long));i++)
 {   
     if(i==0 || i==4 || i==5 || i==6 || i==8 || i==12 || i==13 || i==14)
	gr=0;
     else gr=Grayed;
     item_Add(tk_display,ls_colors_items,0,END,TextFlag,str[i],0,0,gr,True);
 }
 wid_Refresh(tk_display,ls_colors_items);
 XSetWindowBackground(tk_display->display,color_rgb,cl_manager.ed_colors.bg);
 XClearWindow(tk_display->display,color_rgb);
 wid_SetPosition(tk_display,ls_colors_rgb_names,cl_index.ed_colors.bg);

 XMapRaised(tk_display->display,color_edit);
}




void CL_SetupList()
{
 int ret;
 int i, j, k;
 int gr;
 char **str;

 cl_index.current_widget=7;
 cl_index.current_item=-1;
 cl_index.current_color=-1;
 item_DeleteAll(tk_display,ls_colors_items);
 if(LANGUAGE==0)
   str=(char **)us_widget_colors_text;
 else str=(char **)fr_widget_colors_text;
 for(i=0;i<(sizeof(WidgetColors)/sizeof(unsigned long));i++)
 {   
     if(i==0 || i==4 || i==5 || i==6 || i==7 || i==8 || i==9 || i==10 || i==11 || i==11 || i==13|| i==14)
	gr=0;
     else gr=Grayed;
     item_Add(tk_display,ls_colors_items,0,END,TextFlag,str[i],0,0,gr,True);
 }
 wid_Refresh(tk_display,ls_colors_items);
 XSetWindowBackground(tk_display->display,color_rgb,cl_manager.ls_colors.bg);
 XClearWindow(tk_display->display,color_rgb);
 wid_SetPosition(tk_display,ls_colors_rgb_names,cl_index.ls_colors.bg);

 XMapRaised(tk_display->display,color_list);
}





void CL_SetupMenu()
{
 int ret;
 int i, j, k;
 int exp;
 int gr;
 char **str;

 cl_index.current_widget=9;
 cl_index.current_item=-1;
 cl_index.current_color=-1;
 item_DeleteAll(tk_display,ls_colors_items);
 if(LANGUAGE==0)
   str=(char **)us_widget_colors_text;
 else str=(char **)fr_widget_colors_text;
 for(i=0;i<(sizeof(WidgetColors)/sizeof(unsigned long));i++)
 {   
     if(i==0 || i==4 || i==5 || i==6 || i==7 || i==8 || i==9 || i==10 || i==11 || i==13 || i==14)
	gr=0;
     else gr=Grayed;
     item_Add(tk_display,ls_colors_items,0,END,TextFlag,str[i],0,0,gr,True);
 }

 wid_Refresh(tk_display,ls_colors_items);
 XSetWindowBackground(tk_display->display,color_rgb,cl_manager.mn_colors.bg);
 XClearWindow(tk_display->display,color_rgb);
 wid_SetPosition(tk_display,ls_colors_rgb_names,cl_index.mn_colors.bg);

 XMapRaised(tk_display->display,color_menu);
}



extern char *widgetitemname[];
extern char *windowitemname[];
extern char *iconitemname[];


int CL_SavePreferences()
{
 XEvent send_event;

 int exp; 
 unsigned long *version;
 int size;
 
 int ret;
 int i, j, k;
 char *str_type, *sptr, *str_class, *str;
 char text[10];
 unsigned char *widgetname, *windowname;
 unsigned long *colorsptr, *wincolorsptr;

 Status color;
 Bool delay=False;


 XGrabServer(tk_display->display);

 str=(char *)malloc(200);
 version=(unsigned long *)malloc(sizeof(long)*2);
 size=sizeof(WidgetColors)*6+sizeof(WindowColors)*2+sizeof(IconColors);

 version[0]=wm_info.wm_version;
 version[1]=wm_info.wm_release;

 if(version==NULL)
 {
   fprintf(stderr,"ERREUR: Pas assez de memoire disponible\n");
   tk_CloseSession(tk_display);
   exit(-1);
 }
 if(str==NULL)
 {
   fprintf(stderr,"ERREUR: Pas assez de memoire disponible\n");
   tk_CloseSession(tk_display);
   exit(-1);
 }

 memset(str,0,200);
 sprintf(str,"%s",rgb_names[cl_index.desk_bg]);
 XrmPutStringResource(&xrdb,resource_text[64],str);

 memset(str,0,200);
 sprintf(str,"%s",rgb_names[cl_index.desk_fg]);
 XrmPutStringResource(&xrdb,resource_text[65],str);
 wm_info.desk_bg=cl_manager.desk_bg;
 wm_info.desk_fg=cl_manager.desk_fg;


 DSK_SetMotif(wm_info.dsk_motif);


 XSetSelectionOwner(tk_display->display,tk_display->atoms._IMAN_WINDOW_MANAGER,None,CurrentTime);

 memcpy(&tk_display->win_colors,&cl_manager.win_colors,sizeof(WindowColors));
 memcpy(&tk_display->dlg_colors,&cl_manager.dlg_colors,sizeof(WindowColors));
 memcpy(&tk_display->icn_colors,&cl_manager.icn_colors,sizeof(IconColors));

 memcpy(&tk_display->bn_colors,&cl_manager.bn_colors,sizeof(WidgetColors));
 memcpy(&tk_display->sb_colors,&cl_manager.sb_colors,sizeof(WidgetColors));
 memcpy(&tk_display->ed_colors,&cl_manager.ed_colors,sizeof(WidgetColors));
 memcpy(&tk_display->ls_colors,&cl_manager.ls_colors,sizeof(WidgetColors));
 memcpy(&tk_display->cb_colors,&cl_manager.cb_colors,sizeof(WidgetColors));
 memcpy(&tk_display->mn_colors,&cl_manager.mn_colors,sizeof(WidgetColors));

 tk_SetSystemColors(tk_display,WI_BUTTON,&cl_manager.bn_colors,False);
 tk_SetSystemColors(tk_display,WI_EDIT,&cl_manager.ed_colors,False);
 tk_SetSystemColors(tk_display,WI_SCROLLBAR,&cl_manager.sb_colors,False);
 tk_SetSystemColors(tk_display,WI_LIST,&cl_manager.ls_colors,False);
 tk_SetSystemColors(tk_display,WI_COMBO,&cl_manager.cb_colors,False);
 tk_SetSystemColors(tk_display,WI_MENU,&cl_manager.mn_colors,False);

 XSetSelectionOwner(tk_display->display,tk_display->atoms._IMAN_WINDOW_MANAGER,wm_main_window,CurrentTime);

#ifdef DESQVIEW_X_SERVER
 XrmPutFileDatabase(xrdb,"/dvx/iman.rc");
#else
 XrmPutFileDatabase(xrdb,xrm_file);
#endif


  XDeleteProperty(tk_display->display,wm_main_window,tk_display->atoms._IMAN_WM_DATA);
  XChangeProperty(tk_display->display,wm_main_window, tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeReplace,(unsigned char *)version,2/**sizeof(unsigned long)*/);
  version[0]=sizeof(WidgetColors)/sizeof(unsigned long);
  version[1]=6;
  XDeleteProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen),tk_display->atoms._IMAN_WM_DATA);
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)version,2);
 
  version[0]=WI_BUTTON;
  version[1]=WI_SCROLLBAR;
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)version,2);
  version[0]=WI_EDIT;
  version[1]=WI_LIST;
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)version,2);
  version[0]=WI_COMBO;
  version[1]=WI_MENU;
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)version,2);

  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&cl_manager.bn_colors,sizeof(WidgetColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&cl_manager.sb_colors,sizeof(WidgetColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&cl_manager.ed_colors,sizeof(WidgetColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&cl_manager.ls_colors,sizeof(WidgetColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&cl_manager.cb_colors,sizeof(WidgetColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&cl_manager.mn_colors,sizeof(WidgetColors)/sizeof(unsigned long));


  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&cl_manager.win_colors,sizeof(WindowColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&cl_manager.dlg_colors,sizeof(WindowColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&cl_manager.icn_colors,sizeof(IconColors)/sizeof(unsigned long));


  str_class=(char *)NULL;
  XUngrabServer(tk_display->display);
  

  for(i=0;i<6;i++)
  {
    if(i==0) 
    {
	widgetname="button";
	colorsptr=(unsigned long *)&cl_index.bn_colors;
    }
    if(i==1)
    {
	widgetname="scrollbar";
 	colorsptr=(unsigned long *)&cl_index.sb_colors;
    }
    if(i==2) 
    {
	widgetname="edit";
  	colorsptr=(unsigned long *)&cl_index.ed_colors;
    }
    if(i==3) 
    {
	widgetname="list";
 	colorsptr=(unsigned long *)&cl_index.ls_colors;
    }
    if(i==4) 
    {
	widgetname="combo";
 	colorsptr=(unsigned long *)&cl_index.cb_colors;
    }
    if(i==5) 
    {
	widgetname="menu";
	colorsptr=(unsigned long *)&cl_index.mn_colors;
    }


    for(j=0;j<(sizeof(WidgetColors)/sizeof(unsigned long));j++)
    {
    	sprintf(str,"tk.colors.%s.%s",widgetname,widgetitemname[j]);
    	XrmPutStringResource(&xrdb,(char *)str,rgb_names[colorsptr[j]]);
    }
  }


  for(i=0;i<2;i++)
  {
    if(i==0) 
    {
	windowname="window";
	wincolorsptr=(unsigned long *)&cl_index.win_colors;
    }
    if(i==1)
    {
	windowname="dialog";
 	wincolorsptr=(unsigned long *)&cl_index.dlg_colors;
    } 

    for(j=0;j<(sizeof(WindowColors)/sizeof(unsigned long));j++)
    {
    	sprintf(str,"wm.colors.%s.%s",windowname,windowitemname[j]);
  	XrmPutStringResource(&xrdb,(char *)str,rgb_names[wincolorsptr[j]]);
    }

  }
  windowname="icon";
  wincolorsptr=(unsigned long *)&cl_index.icn_colors;
  for(j=0;j<(sizeof(IconColors)/sizeof(unsigned long))-2;j++)
  {
    	sprintf(str,"wm.colors.%s.%s",windowname,iconitemname[j]);
        XrmPutStringResource(&xrdb,(char *)str,rgb_names[wincolorsptr[j]]);
  }

  free(str); 


 send_event.type=ClientMessage;
 send_event.xclient.message_type=tk_display->atoms._IMAN_WM_MESSAGES;
 send_event.xclient.format=32;
 send_event.xclient.data.l[0]=WmResetColors;


 for(i=0;i<maxwindows;i++) 
 {
   if(windows[i].class==TOP_LEVEL_WINDOW)
   {
     XSetWindowBackground(tk_display->display,windows[i].mainwindow,tk_display->win_colors.bg);
     if(windows[i].isUsed==True&&windows[i].state.isOnTop==True)
       XSetWindowBackground(tk_display->display,windows[i].titlebar,tk_display->win_colors.title_bg_active);
     else
       XSetWindowBackground(tk_display->display,windows[i].titlebar,tk_display->win_colors.title_bg_inactive);
     XSetWindowBorder(tk_display->display,windows[i].titlebar,tk_display->win_colors.border_inactive);
   } 
   else if(windows[i].class==DIALOG_BOX)
   {
     XSetWindowBorder(tk_display->display,windows[i].titlebar,tk_display->dlg_colors.bg);
     if(windows[i].isUsed==True&&windows[i].state.isOnTop==True)
       XSetWindowBackground(tk_display->display,windows[i].titlebar,tk_display->dlg_colors.title_bg_active);
     else
       XSetWindowBackground(tk_display->display,windows[i].titlebar,tk_display->dlg_colors.title_bg_inactive);
   } 
   
   if(windows[i].class==TOP_LEVEL_WINDOW)
   {
     XSetWindowBackground(tk_display->display,windows[i].topleftbox,tk_display->win_colors.bg);
     XSetWindowBorder(tk_display->display,windows[i].topleftbox,tk_display->win_colors.border_inactive);
     XSetWindowBackground(tk_display->display,windows[i].toprightbox,tk_display->win_colors.bg);
     XSetWindowBorder(tk_display->display,windows[i].toprightbox,tk_display->win_colors.border_inactive);
     XSetWindowBackground(tk_display->display,windows[i].bottomleftbox,tk_display->win_colors.bg);
     XSetWindowBorder(tk_display->display,windows[i].bottomleftbox,tk_display->win_colors.border_inactive);
     XSetWindowBackground(tk_display->display,windows[i].bottomrightbox,tk_display->win_colors.bg);
     XSetWindowBorder(tk_display->display,windows[i].bottomrightbox,tk_display->win_colors.border_inactive);
     XSetWindowBackground(tk_display->display,windows[i].leftbar,tk_display->win_colors.bg);
     XSetWindowBorder(tk_display->display,windows[i].leftbar,tk_display->win_colors.border_inactive);
     XSetWindowBackground(tk_display->display,windows[i].rightbar,tk_display->win_colors.bg);
     XSetWindowBorder(tk_display->display,windows[i].rightbar,tk_display->win_colors.border_inactive);
     XSetWindowBackground(tk_display->display,windows[i].topbar,tk_display->win_colors.bg);
     XSetWindowBorder(tk_display->display,windows[i].topbar,tk_display->win_colors.border_inactive);
     XSetWindowBackground(tk_display->display,windows[i].bottombar,tk_display->win_colors.bg);
     XSetWindowBorder(tk_display->display,windows[i].bottombar,tk_display->win_colors.border_inactive);
   }
   /*WID_Refresh(tk_display,windows[i].titlebar);*/

   if(windows[i].isUsed==True)
     exp=True;
   else exp=False;

   XClearArea(tk_display->display,windows[i].mainwindow,0,0,10000,10000,exp);
   /*XClearArea(tk_display->display,windows[i].clientwindow,0,0,10000,10000,exp);*/
   XClearArea(tk_display->display,windows[i].titlebar,0,0,10000,10000,exp);
   if(windows[i].class==TOP_LEVEL_WINDOW)
   {
     XClearArea(tk_display->display,windows[i].topleftbox,0,0,10000,10000,exp);
     XClearArea(tk_display->display,windows[i].toprightbox,0,0,10000,10000,exp);
     XClearArea(tk_display->display,windows[i].bottomleftbox,0,0,10000,10000,exp);
     XClearArea(tk_display->display,windows[i].bottomrightbox,0,0,10000,10000,exp);
     XClearArea(tk_display->display,windows[i].leftbar,0,0,10000,10000,exp);
     XClearArea(tk_display->display,windows[i].rightbar,0,0,10000,10000,exp);
     XClearArea(tk_display->display,windows[i].topbar,0,0,10000,10000,exp);
     XClearArea(tk_display->display,windows[i].bottombar,0,0,10000,10000,exp);
   }
   if(windows[i].isUsed==True&&windows[i].state.isIconic==True)
     exp=True;
   else exp=False;

   if(windows[i].class==TOP_LEVEL_WINDOW)
   {
     XSetWindowBackground(tk_display->display,windows[i].icon.window,tk_display->icn_colors.icn_bg);
     XSetWindowBackground(tk_display->display,windows[i].icon.title,tk_display->icn_colors.title_bg_active);
     XSetWindowBackground(tk_display->display,windows[i].icon.draw_area,tk_display->icn_colors.icn_draw_bg);

     XClearArea(tk_display->display,windows[i].icon.window,0,0,10000,10000,exp);
     XClearArea(tk_display->display,windows[i].icon.title,0,0,10000,10000,exp);
     XClearArea(tk_display->display,windows[i].icon.draw_area,0,0,10000,10000,exp);
   }
   if(windows[i].isUsed==True && windows[i].clientwindow!=wm_error_window && windows[i].clientwindow!=wm_resize_window && windows[i].clientwindow!=wm_clipboard_window)
   {
     send_event.xclient.window=windows[i].clientwindow;
     XSendEvent(tk_display->display,windows[i].clientwindow,False,0,&send_event);
   }
 }



 XSetWindowBackground(tk_display->display,wm_about_window,tk_display->dlg_colors.bg);
 XSetWindowBackground(tk_display->display,wm_kill_window,tk_display->dlg_colors.bg);
 XSetWindowBackground(tk_display->display,wm_desktop_window,tk_display->dlg_colors.bg);
 XSetWindowBackground(tk_display->display,desktop_motif_window,tk_display->dlg_colors.bg);
 XSetWindowBackground(tk_display->display,desktop_screensaver_window,tk_display->dlg_colors.bg);
 XSetWindowBackground(tk_display->display,desktop_paper_window,tk_display->dlg_colors.bg);
 XSetWindowBackground(tk_display->display,desktop_defaulticon_window,tk_display->dlg_colors.bg);
 XSetWindowBackground(tk_display->display,wm_colors_window,tk_display->dlg_colors.bg);
 XSetWindowBackground(tk_display->display,wm_process_window,tk_display->dlg_colors.bg);
 XSetWindowBackground(tk_display->display,wm_setup_window,tk_display->dlg_colors.bg);
 XSetWindowBackground(tk_display->display,wm_end_window,tk_display->dlg_colors.bg);
 XSetWindowBackground(tk_display->display,wm_clipboard_window,tk_display->dlg_colors.bg);


 if(windows[wm_desktop_index].state.isMapped==True)
   exp=True;
 else exp=False;
 XClearArea(tk_display->display,wm_about_window,0,0,10000,10000,exp);
 XClearArea(tk_display->display,wm_desktop_window,0,0,10000,10000,exp);
 XClearArea(tk_display->display,wm_colors_window,0,0,10000,10000,exp);
 XClearArea(tk_display->display,desktop_motif_window,0,0,10000,10000,exp);
 XClearArea(tk_display->display,desktop_screensaver_window,0,0,10000,10000,exp);
 XClearArea(tk_display->display,desktop_paper_window,0,0,10000,10000,exp);
 XClearArea(tk_display->display,desktop_defaulticon_window,0,0,10000,10000,exp);

 XClearArea(tk_display->display,wm_process_window,0,0,10000,10000,False);
 XClearArea(tk_display->display,wm_setup_window,0,0,10000,10000,False);
 XClearArea(tk_display->display,wm_kill_window,0,0,10000,10000,False);
 XClearArea(tk_display->display,wm_end_window,0,0,10000,10000,False);
 XClearArea(tk_display->display,wm_clipboard_window,0,0,10000,10000,True);



 wid_Refresh(tk_display,bn_paper_center);
 wid_Refresh(tk_display,bn_paper_mos);


#ifdef DESQVIEW_X_SERVER
 XrmPutFileDatabase(xrdb,"/dvx/iman.rc");
#else
 XrmPutFileDatabase(xrdb,xrm_file);
#endif

 /*
 send_event.xclient.window=wm_desktop_window;
 XSendEvent(tk_display->display,wm_desktop_window,False,0,&send_event);
 */


 cl_index.current_color=-1;

 return 0;
}




