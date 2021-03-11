/*
 *
 * 	cl_get.c
 * 	informations sur les fenetres de couleur
 *
 * 	Modification :  29/01/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
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

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"
#include "wm_res.h"


extern int numrgbnames;
extern char *rgb_names[];

unsigned char *widgetitemname[]=
{
    	"bg","fg","light","shadow","text","text_grayed","selected","selected_inactive",
    	"text_selected","text_grayed_selected","text_selected_inactive","text_grayed_selected_inactive",
	"cursor","focus","nofocus","cross","check","radio_bg","radio_light"
}; 
unsigned char *windowitemname[]=
{
    	"bg","light","shadow","text","text_grayed",
    	"border_active","border_inactive","title_bg_active","title_bg_inactive",
	"title_text_active","title_text_inactive"
}; 
unsigned char *iconitemname[]=
{
    	"bg","light","shadow","pixmap_fg","pixmap_bg",
    	"title_bg_active","title_text_active","title_bg_inactive","title_text_inactive"
}; 



int CL_GetPreferences()
{
  int ret;
  int i, j, k;
  char *str_type, *sptr, *str_class, *str;
  char text[10];
  unsigned char *widgetname, *windowname;
  unsigned long *colorsptr, *wincolorsptr;

  Status color;
  Bool delay=False;



  str=(char *)malloc(200);
  if(str==(char *)NULL)
    return -3;


		/*** Options pour wm_info ***/

 str_class=(char *)NULL;
 cl_manager.bn_colors=tk_display->bn_colors;
 cl_manager.sb_colors=tk_display->sb_colors;
 cl_manager.ed_colors=tk_display->ed_colors;
 cl_manager.ls_colors=tk_display->ls_colors;
 cl_manager.cb_colors=tk_display->cb_colors;
 cl_manager.mn_colors=tk_display->mn_colors;
 cl_manager.win_colors=tk_display->win_colors;
 cl_manager.dlg_colors=tk_display->dlg_colors;
 cl_manager.icn_colors=tk_display->icn_colors;
 cl_manager.desk_bg=wm_info.desk_bg;
 cl_manager.desk_fg=wm_info.desk_fg;
 cl_index.bn_colors=tk_display->bn_colors;
 cl_index.sb_colors=tk_display->sb_colors;
 cl_index.ed_colors=tk_display->ed_colors;
 cl_index.ls_colors=tk_display->ls_colors;
 cl_index.cb_colors=tk_display->cb_colors;
 cl_index.mn_colors=tk_display->mn_colors;
 cl_index.win_colors=tk_display->win_colors;
 cl_index.dlg_colors=tk_display->dlg_colors;
 cl_index.icn_colors=tk_display->icn_colors;
 cl_index.desk_bg=wm_info.desk_bg;
 cl_index.desk_fg=wm_info.desk_fg;


  sptr="";
  j=0;
  if(xrdb!=NULL) j=XrmGetResource(xrdb,resource_text[64],NULL,&str_type,&xrmvalue); 
  if(xrdb!=NULL&&j==1)
  {
    sptr=xrmvalue.addr;
    cl_index.desk_bg=CL_GetIndex(sptr);
  }
  else
  {
    cl_index.desk_bg=CL_GetIndex("white");
  }
  sptr="";
  j=0;
  if(xrdb!=NULL) j=XrmGetResource(xrdb,resource_text[65],NULL,&str_type,&xrmvalue); 
  if(xrdb!=NULL&&j==1)
  {
    sptr=xrmvalue.addr;
    cl_index.desk_fg=CL_GetIndex(sptr);
  }
  else
  {
    cl_index.desk_bg=CL_GetIndex("black");
  }



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
    	k=XrmGetResource(xrdb,(char *)str,(char *)str,&str_type,&xrmvalue);
    	if(k==1)
    	{
	  sptr=xrmvalue.addr; 
    	  colorsptr[j]=CL_GetIndex(sptr);
    	}
	else
  	{
	    if(colorsptr[j]==WhitePixel(tk_display->display,tk_display->screen))
	      sptr="white";
	    else sptr="black";
    	    colorsptr[j]=CL_GetIndex(sptr);
	}

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
    	k=XrmGetResource(xrdb,(char *)str,(char *)str,&str_type,&xrmvalue);
    	if(k==1)
    	{
    	  sptr=xrmvalue.addr;
    	  wincolorsptr[j]=CL_GetIndex(sptr);
    	}
	else 
    	{
  	  if(wincolorsptr[j]==WhitePixel(tk_display->display,tk_display->screen))
	    sptr="white";
	  else sptr="black";
    	  wincolorsptr[j]=CL_GetIndex(sptr);
    	}

    }

  }
  windowname="icon";
  wincolorsptr=(unsigned long *)&cl_index.icn_colors;
  for(j=0;j<(sizeof(IconColors)/sizeof(unsigned long))-2;j++)
  {
    	sprintf(str,"wm.colors.%s.%s",windowname,iconitemname[j]);
    	k=XrmGetResource(xrdb,(char *)str,(char *)str,&str_type,&xrmvalue);
    	if(k==1)
    	{
    	  sptr=xrmvalue.addr;
    	  wincolorsptr[j]=CL_GetIndex(sptr);
    	}
	else 
    	{
  	  if(wincolorsptr[j]==WhitePixel(tk_display->display,tk_display->screen))
	    sptr="white";
	  else sptr="black";
    	  wincolorsptr[j]=CL_GetIndex(sptr);
    	}

  }

  free(str); 

  XSetWindowBackground(tk_display->display,color_desktop,wm_info.desk_bg);
  XSetWindowBackground(tk_display->display,color_window,wm_info.desk_bg);
  XSetWindowBackground(tk_display->display,color_icon,wm_info.desk_bg);
  XSetWindowBackground(tk_display->display,color_menu,wm_info.desk_bg);
						
  XSetWindowBackground(tk_display->display,color_button,cl_index.win_colors.bg);
  XSetWindowBackground(tk_display->display,color_edit,cl_index.win_colors.bg);
  XSetWindowBackground(tk_display->display,color_scroll,cl_index.win_colors.bg);
  XSetWindowBackground(tk_display->display,color_list,cl_index.win_colors.bg);

  return 0;

}



int CL_GetIndex(str)
char *str;
{
  int i;
   
  if(str==(char *)NULL)
    return -1;

  for(i=0;i<numrgbnames;i++)
  {
    if(strcmp(str,rgb_names[i])==0)
	return i;
  }
  return -1;
}




Bool CL_IsUsed(color)
unsigned long color;
{
  int i, j;
  unsigned long *colorsptr1, *colorsptr2;

  if(color==wm_info.desk_bg)
    return True;
  if(color==wm_info.desk_fg)
    return True;
  if(color==cl_manager.desk_bg && (cl_manager.current_widget!=0 || wid_GetPosition(tk_display,ls_colors_items)!=1))
    return True;
  if(color==cl_manager.desk_fg && (cl_manager.current_widget!=0 || wid_GetPosition(tk_display,ls_colors_items)!=0))
    return True;


  for(i=0;i<6;i++)
  {
    if(i==0) 
    {
	colorsptr1=(unsigned long *)&tk_display->bn_colors;
	colorsptr2=(unsigned long *)&cl_manager.bn_colors;
    }
    if(i==1)
    {
	colorsptr1=(unsigned long *)&tk_display->sb_colors;
 	colorsptr2=(unsigned long *)&cl_manager.sb_colors;
    }
    if(i==2) 
    {
	colorsptr1=(unsigned long *)&tk_display->ed_colors;
  	colorsptr2=(unsigned long *)&cl_manager.ed_colors;
    }
    if(i==3) 
    {
	colorsptr1=(unsigned long *)&tk_display->ls_colors;
 	colorsptr2=(unsigned long *)&cl_manager.ls_colors;
    }
    if(i==4) 
    {
	colorsptr1=(unsigned long *)&tk_display->cb_colors;
 	colorsptr2=(unsigned long *)&cl_manager.cb_colors;
    }
    if(i==5) 
    {
	colorsptr1=(unsigned long *)&tk_display->mn_colors;
	colorsptr2=(unsigned long *)&cl_manager.mn_colors;
    }


    for(j=0;j<(sizeof(WidgetColors)/sizeof(unsigned long));j++)
    {
      if(colorsptr1[j]==color && (i+4!=cl_manager.current_widget || j!=wid_GetPosition(tk_display,ls_colors_items)))
	return True;
      if(colorsptr2[j]==color && (i+4!=cl_manager.current_widget || j!=wid_GetPosition(tk_display,ls_colors_items)))
	return True;
    }
  }

  for(i=0;i<2;i++)
  {
    if(i==0) 
    {
	colorsptr1=(unsigned long *)&tk_display->win_colors;
	colorsptr2=(unsigned long *)&cl_manager.win_colors;
    }
    if(i==1)
    {
	colorsptr1=(unsigned long *)&tk_display->dlg_colors;
 	colorsptr2=(unsigned long *)&cl_manager.dlg_colors;
    } 

    for(j=0;j<(sizeof(WindowColors)/sizeof(unsigned long));j++)
    {
      if(colorsptr1[j]==color && (i+1!=cl_manager.current_widget || j!=wid_GetPosition(tk_display,ls_colors_items)))
	return True;
      if(colorsptr2[j]==color && (i+1!=cl_manager.current_widget || j!=wid_GetPosition(tk_display,ls_colors_items)))
	return True;

    }
  }
  return False;
}


