/*
 *
 * 	tk_get.c  
 * 	informations sur le toolkit et le wm
 *
 * 	Modification :  19/04/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute and sell this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appears in all copies and that both that
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
 *      IMAN Development Toolkit version 1.2
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
#include <X11/Xatom.h>
#include <X11/Xresource.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>









/*
 * Rafraichir/Obtenir la connection avec le WM 
 *
 */

int tk_GetConnection(tk_display)
TkDisplay *tk_display;
{

  int i;
  int ret=0;

  Atom gp_actual_type;
  int gp_actual_format;
  unsigned long gp_nitems, gp_bytes_after;
  unsigned long *gp_prop;
  char *str;

  /*fprintf(stderr," get ... ");*/

  if(tk_display->wm.comment!=NULL)
    XFree(tk_display->wm.comment);
  tk_display->wm.main_window=XGetSelectionOwner(tk_display->display,tk_display->atoms._IMAN_WINDOW_MANAGER);
  if(tk_display->wm.main_window==None)
  {
    tk_display->wm.active=False;
    tk_display->wm.version=0;
    tk_display->wm.release=0;
    tk_display->wm.comment=NULL;
    return False;   
  }
  tk_display->wm.active=True;
  ret=XGetWindowProperty(tk_display->display, tk_display->wm.main_window,tk_display->atoms._IMAN_WM_DATA,0,2,False,XA_INTEGER,&gp_actual_type, &gp_actual_format, &gp_nitems, &gp_bytes_after, (unsigned char **)&gp_prop);
  if(ret==0 && gp_nitems>=2)
  {	
    tk_display->wm.version=gp_prop[0];
    tk_display->wm.release=gp_prop[1];
    tk_display->wm.comment=NULL;
    XFetchName(tk_display->display,tk_display->wm.main_window,&str);
#ifdef DEBUG
    fprintf(stderr,"str=%s\n",str);
#endif
    tk_display->wm.comment=str;
  }
  else 
  {
    tk_display->wm.version=0;
    tk_display->wm.release=0;
    tk_display->wm.comment=NULL;
  }
  
  if(gp_nitems>0) XFree((char *)gp_prop);
#ifdef DEBUG
  (void)fprintf(stderr,"tk_GetConnection terminee  active=%d\n",tk_display->wm.active);
#endif
  return tk_display->wm.active;

}





/*
 * Obtenir les couleurs systemes du WM 
 *
 */

int tk_RefreshColors(tk_display)
TkDisplay *tk_display;
{
  int ret;
  Atom gp_actual_type;
  int gp_actual_format;
  unsigned long gp_nitems, gp_bytes_after;
  unsigned long *gp_prop;
  unsigned long widgetsize, numwidgets, *wid;
  int i, j, k;
  TkDisplay *tk_bis;
  unsigned long *widgetcolorsptr;
  unsigned long *windowcolorsptr;
  unsigned long *iconcolorsptr;
  XColor xcolor;


  /*fprintf(stderr,"Debut de refresh ...  ");*/
  if (XGetWindowProperty(tk_display->display, RootWindow(tk_display->display,tk_display->screen),tk_display->atoms._IMAN_WM_DATA,0,2,False,XA_INTEGER,&gp_actual_type,&gp_actual_format,&gp_nitems,&gp_bytes_after,(unsigned char**)&gp_prop)==1)
    return 0;

  tk_bis=(TkDisplay *)malloc(sizeof(TkDisplay));
  if(tk_bis==(TkDisplay *)NULL)
    return 0;
  else 
  {	
  	memcpy(tk_bis,tk_display,sizeof(TkDisplay));

	free(gp_prop);														
	ret=XGetWindowProperty(tk_display->display, RootWindow(tk_display->display,tk_display->screen),tk_display->atoms._IMAN_WM_DATA,0,2,False,XA_INTEGER,&gp_actual_type,&gp_actual_format,&gp_nitems,&gp_bytes_after,(unsigned char **)&gp_prop);
        if(ret==1) return 0;
	if(gp_nitems<2)
	  return 0;
	widgetsize=gp_prop[0];
	numwidgets=gp_prop[1];

	free(gp_prop);
	ret=XGetWindowProperty(tk_display->display, RootWindow(tk_display->display,tk_display->screen),tk_display->atoms._IMAN_WM_DATA,2,numwidgets,False,XA_INTEGER,&gp_actual_type,&gp_actual_format,&gp_nitems,&gp_bytes_after,(unsigned char **)&wid);
        if(ret==1) return 0;
	if(gp_nitems<numwidgets)
	  return 0;
	XSync(tk_display->display,tk_display->screen);

	/*fprintf(stderr,"widgetsize=%d   numwidgets=%d   ",widgetsize,numwidgets);*/

	for(i=0;i<numwidgets;i++)
	{
	  ret=XGetWindowProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen),tk_display->atoms._IMAN_WM_DATA,2+numwidgets+widgetsize*i,widgetsize,False,XA_INTEGER,&gp_actual_type,&gp_actual_format,&gp_nitems,&gp_bytes_after,(unsigned char **)&gp_prop);
          if(ret==1) return 0;
	  if(gp_nitems<widgetsize)
	    XSync(tk_display->display,tk_display->screen);
	  /*fprintf(stderr,"wid=%d  ",wid[i]);*/

	  if(wid[i]==WI_BUTTON)
	  {

			    	/* BUTTON COLORS */ 
		tk_display->bn_colors.bg=gp_prop[0];
 	 	tk_display->bn_colors.light=gp_prop[2];
	 	tk_display->bn_colors.shadow=gp_prop[3];
	 	tk_display->bn_colors.text=gp_prop[4];
		tk_display->bn_colors.text_grayed=gp_prop[5];
		tk_display->bn_colors.cursor=gp_prop[12];
		tk_display->bn_colors.focus=gp_prop[13];
		tk_display->bn_colors.nofocus=gp_prop[14];
	 	tk_display->bn_colors.cross=gp_prop[15];
	 	tk_display->bn_colors.check=gp_prop[16];
	 	tk_display->bn_colors.radio_bg=gp_prop[17];
	 	tk_display->bn_colors.radio_light=gp_prop[18];

		widgetcolorsptr=(unsigned long *)&tk_display->bn_colors;
		for(k=0;k<(sizeof(WidgetColors)/sizeof(unsigned long));k++)
		{
		   xcolor.pixel=widgetcolorsptr[k];
		   XQueryColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	  	   
		   XAllocColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	
		}
		
	  }	  
	  else if(wid[i]==WI_SCROLLBAR)
	  {
			 	/* SCROLLBAR COLORS */
	   	tk_display->sb_colors.bg=gp_prop[0];
	   	tk_display->sb_colors.light=gp_prop[2];
		tk_display->sb_colors.focus=gp_prop[13];
		tk_display->sb_colors.nofocus=gp_prop[14];

		widgetcolorsptr=(unsigned long *)&tk_display->sb_colors;
		for(k=0;k<(sizeof(WidgetColors)/sizeof(unsigned long));k++)
		{
		   xcolor.pixel=widgetcolorsptr[k];
		   XQueryColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	  	   
		   XAllocColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	
		}

	  }

	  else if(wid[i]==WI_COMBO)
	  {
			 	/* COMBO COLORS */;
		tk_display->cb_colors.focus=gp_prop[13];
		tk_display->cb_colors.nofocus=gp_prop[14];

		widgetcolorsptr=(unsigned long *)&tk_display->cb_colors;
		for(k=0;k<(sizeof(WidgetColors)/sizeof(unsigned long));k++)
		{
		   xcolor.pixel=widgetcolorsptr[k];
		   XQueryColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	  	   
		   XAllocColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	
		}
	  }

	  else if(wid[i]==WI_EDIT)
	  {
			      	/* EDIT COLORS */
	 	tk_display->ed_colors.bg=gp_prop[0];
	 	tk_display->ed_colors.text=gp_prop[4];
		tk_display->ed_colors.text_grayed=gp_prop[5];
	 	tk_display->ed_colors.selected=gp_prop[6];
	 	tk_display->ed_colors.text_selected=gp_prop[8];
	 	tk_display->ed_colors.text_grayed_selected=gp_prop[9];
	 	tk_display->ed_colors.cursor=gp_prop[12];
	 	tk_display->ed_colors.focus=gp_prop[13];
		tk_display->ed_colors.nofocus=gp_prop[14];

		widgetcolorsptr=(unsigned long *)&tk_display->ed_colors;
		for(k=0;k<(sizeof(WidgetColors)/sizeof(unsigned long));k++)
		{
		   xcolor.pixel=widgetcolorsptr[k];
		   XQueryColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	  	   
		   XAllocColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	
		}
	  }

	  else if(wid[i]==WI_LIST)
	  {
			   	/* LIST COLORS */
	 	tk_display->ls_colors.bg=gp_prop[0];
	 	tk_display->ls_colors.text=gp_prop[4];
	 	tk_display->ls_colors.text_grayed=gp_prop[5];
	 	tk_display->ls_colors.selected=gp_prop[6];
	 	tk_display->ls_colors.selected_inactive=gp_prop[7]; 
	 	tk_display->ls_colors.text_selected=gp_prop[8];
	 	tk_display->ls_colors.text_grayed_selected=gp_prop[9];
	 	tk_display->ls_colors.text_selected_inactive=gp_prop[10];
 		tk_display->ls_colors.text_grayed_selected_inactive=gp_prop[11];
		tk_display->ls_colors.focus=gp_prop[13];
	 	tk_display->ls_colors.nofocus=gp_prop[14];

		widgetcolorsptr=(unsigned long *)&tk_display->ls_colors;
		for(k=0;k<(sizeof(WidgetColors)/sizeof(unsigned long));k++)
		{
		   xcolor.pixel=widgetcolorsptr[k];
		   XQueryColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	  	   
		   XAllocColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	
		}

	  }

	  else if(wid[i]==WI_MENU)
	  {
			   	/* LIST COLORS */
	 	tk_display->mn_colors.bg=gp_prop[0];
	 	tk_display->mn_colors.text=gp_prop[4];
	 	tk_display->mn_colors.text_grayed=gp_prop[5];
	 	tk_display->mn_colors.selected=gp_prop[6];
	 	tk_display->mn_colors.selected_inactive=gp_prop[7]; 
	 	tk_display->mn_colors.text_selected=gp_prop[8];
	 	tk_display->mn_colors.text_grayed_selected=gp_prop[9];
	 	tk_display->mn_colors.text_selected_inactive=gp_prop[10];
 		tk_display->mn_colors.text_grayed_selected_inactive=gp_prop[11];
		tk_display->mn_colors.focus=gp_prop[13];
	 	tk_display->mn_colors.nofocus=gp_prop[14];

		widgetcolorsptr=(unsigned long *)&tk_display->mn_colors;
		for(k=0;k<(sizeof(WidgetColors)/sizeof(unsigned long));k++)
		{
		   xcolor.pixel=widgetcolorsptr[k];
		   XQueryColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	  	   
		   XAllocColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	
		}
	
	  }

	
 	 XFree((char *)gp_prop);
	}

	/*fprintf(stderr,"Widgets passes   ");*/

ret=XGetWindowProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen),tk_display->atoms._IMAN_WM_DATA,2+numwidgets+widgetsize*i,sizeof(WindowColors)*2+sizeof(IconColors),False,XA_INTEGER,&gp_actual_type,&gp_actual_format,&gp_nitems,&gp_bytes_after,(unsigned char **)&gp_prop);
          if(ret==1)
	  {
	    fprintf(stderr,"ret==1  ");
	    free((char *)tk_bis);
	    return 0;
	  }

	  if(gp_nitems<((sizeof(WindowColors))*2+sizeof(IconColors))/sizeof(unsigned long))
	  {
	    fprintf(stderr,"gp_nitems=%d  sizeof=%d",gp_nitems,((sizeof(WindowColors))*2+sizeof(IconColors)));
	    free((char *)tk_bis);
	    return 0;
	  }

	 /*fprintf(stderr,"avant WINDOW   ");*/

				/* WINDOW COLORS */
	 tk_display->win_colors.title_bg_active=gp_prop[7];	
	 tk_display->win_colors.title_bg_inactive=gp_prop[8];
	 tk_display->win_colors.title_text_active=gp_prop[9];
	 tk_display->win_colors.title_text_inactive=gp_prop[10];	
	 tk_display->win_colors.border_active=gp_prop[5];
 	 tk_display->win_colors.border_inactive=gp_prop[6];	
	 tk_display->win_colors.bg=gp_prop[0];
	 tk_display->win_colors.light=gp_prop[1];	
	 tk_display->win_colors.shadow=gp_prop[2];
	 tk_display->win_colors.text=gp_prop[3];	
	 tk_display->win_colors.text_grayed=gp_prop[4];

	 windowcolorsptr=(unsigned long *)&tk_display->win_colors;
	 for(k=0;k<(sizeof(WindowColors)/sizeof(unsigned long));k++)
	 {
		   xcolor.pixel=windowcolorsptr[k];
		   XQueryColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	  	   
		   XAllocColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	
	 }

	
				/* DIALOG COLORS */
	 tk_display->dlg_colors.title_bg_active=gp_prop[18];	
	 tk_display->dlg_colors.title_bg_inactive=gp_prop[19];
	 tk_display->dlg_colors.title_text_active=gp_prop[20];
	 tk_display->dlg_colors.title_text_inactive=gp_prop[21];	
	 tk_display->dlg_colors.border_active=gp_prop[16];
 	 tk_display->dlg_colors.border_inactive=gp_prop[17];	
	 tk_display->dlg_colors.bg=gp_prop[11];
	 tk_display->dlg_colors.light=gp_prop[12];	
	 tk_display->dlg_colors.shadow=gp_prop[13];
	 tk_display->dlg_colors.text=gp_prop[14];	
	 tk_display->dlg_colors.text_grayed=gp_prop[15];

	 windowcolorsptr=(unsigned long *)&tk_display->dlg_colors;
	 for(k=0;k<(sizeof(WindowColors)/sizeof(unsigned long));k++)
	 {
		   xcolor.pixel=windowcolorsptr[k];
		   XQueryColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	  	   
		   XAllocColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	
	 }

				/* ICON COLORS */
	 tk_display->icn_colors.icn_bg=gp_prop[22];	
	 tk_display->icn_colors.icn_light=gp_prop[23];
	 tk_display->icn_colors.icn_shadow=gp_prop[24];
	 tk_display->icn_colors.icn_draw=gp_prop[25]; 
	 tk_display->icn_colors.icn_draw_bg=gp_prop[26]; 
	 tk_display->icn_colors.title_bg_active=gp_prop[27]; 
	 tk_display->icn_colors.title_text_active=gp_prop[28]; 
	 tk_display->icn_colors.title_bg_inactive=gp_prop[29]; 
	 tk_display->icn_colors.title_text_inactive=gp_prop[30];
	
	 iconcolorsptr=(unsigned long *)&tk_display->icn_colors;
	 for(k=0;k<(sizeof(IconColors)/sizeof(unsigned long));k++)
	 {
		   xcolor.pixel=iconcolorsptr[k];
		   XQueryColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	  	   
		   XAllocColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor);	
	 }

	/*fprintf(stderr,"OK   ");*/

	XFree((char *)gp_prop);
	XSync(tk_display->display,False);

 	for(k=0;k<6;k++)
	{
	  ret=0;
	  if(k==0) widgetcolorsptr=(unsigned long *)&tk_bis->bn_colors;
	  else if(k==1) widgetcolorsptr=(unsigned long *)&tk_bis->sb_colors;
	  else if(k==2) widgetcolorsptr=(unsigned long *)&tk_bis->ed_colors;
	  else if(k==3) widgetcolorsptr=(unsigned long *)&tk_bis->ls_colors;
	  else if(k==4) widgetcolorsptr=(unsigned long *)&tk_bis->cb_colors;
	  else if(k==5) widgetcolorsptr=(unsigned long *)&tk_bis->mn_colors;

	  for(i=0;i<(sizeof(WidgetColors)/sizeof(unsigned long));i++)	
	  {
	    xcolor.pixel=(unsigned long)widgetcolorsptr[i];
	    if(IsColorUsed(tk_display,xcolor.pixel)==False)
	      XFreeColors(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor.pixel,1,1);    
	  }
	}
 	for(k=0;k<2;k++)
	{
	  ret=0;
	  if(k==0) windowcolorsptr=(unsigned long *)&tk_bis->win_colors;
	  else if(k==1) windowcolorsptr=(unsigned long *)&tk_bis->dlg_colors;

	  for(i=0;i<(sizeof(WindowColors)/sizeof(unsigned long));i++)	
	  {
	    xcolor.pixel=(unsigned long)windowcolorsptr[i];
	    if(IsColorUsed(tk_display,xcolor.pixel)==False)
	      XFreeColors(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor.pixel,1,1);    
	  }
	}
	iconcolorsptr=(unsigned long *)&tk_bis->icn_colors;
	
	for(i=0;i<(sizeof(IconColors)/sizeof(unsigned long));i++)	
	{
	    xcolor.pixel=(unsigned long)iconcolorsptr[i];
	    if(IsColorUsed(tk_display,xcolor.pixel)==False)
	      XFreeColors(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&xcolor.pixel,1,1);    
	}
	
	XSync(tk_display->display,False);



	free((char *)tk_bis);
	/*fprintf(stderr,"Fin de refresh colors   ");*/
	return 1;


     
  }

}




Bool IsColorUsed(tk_display,pixel)
TkDisplay *tk_display;
unsigned long pixel;
{

  int i, j, k; 
  unsigned long *widgetcolorsptr, *windowcolorsptr, *iconcolorsptr;


  for(k=0;k<6;k++)
  {
	  if(k==0) widgetcolorsptr=(unsigned long *)&tk_display->bn_colors;
	  else if(k==1) widgetcolorsptr=(unsigned long *)&tk_display->sb_colors;
	  else if(k==2) widgetcolorsptr=(unsigned long *)&tk_display->ed_colors;
	  else if(k==3) widgetcolorsptr=(unsigned long *)&tk_display->ls_colors;
	  else if(k==4) widgetcolorsptr=(unsigned long *)&tk_display->cb_colors;
	  else if(k==5) widgetcolorsptr=(unsigned long *)&tk_display->mn_colors;

	  for(i=0;i<(sizeof(WidgetColors)/sizeof(unsigned long));i++)	
	    if(widgetcolorsptr[i]==pixel) return True;
  }
 
  for(k=0;k<2;k++)
  {
	  if(k==0) windowcolorsptr=(unsigned long *)&tk_display->win_colors;
	  else if(k==1) windowcolorsptr=(unsigned long *)&tk_display->dlg_colors;

	  for(i=0;i<(sizeof(WindowColors)/sizeof(unsigned long));i++)	
	    if(windowcolorsptr[i]==pixel) return True;
  }
  iconcolorsptr=(unsigned long *)&tk_display->icn_colors;
 
  for(i=0;i<(sizeof(IconColors)/sizeof(unsigned long));i++)	
    if(iconcolorsptr[i]==pixel) return True;
 
  return False;
	
}




unsigned long tk_GetDoubleClickSpeed()
{
  int ret;
  time_t time1, time2;
  int i; 
  unsigned long j=100000;

  /*fprintf(stderr,"Getdouble  ");*/
PROC_START:
  time1=time(NULL);
  for(i=0;i<j;i++) i=i;
  time2=time(NULL);
  if(time2-time1<1)
  {
     j=j+100000;
     goto PROC_START;
  }
 
   return (j*5)/4;
  
}





/*
 * Loading the toolkit system colors from a resources file
 *
 */

int tk_LoadSystemColorsFromFile(tk_display,filename)
TkDisplay *tk_display;
char *filename;
{
  int i, j, k;
  unsigned long colorsptr[20], *wincolorsptr;
  WidgetColors wid_colors;
  XColor rgb, colorcell;
  XrmDatabase xrdb;
  XrmValue xrmvalue;
  unsigned char *str;
  char *str_type;
  unsigned char *widgetname, *windowname;
  unsigned char *itemname[]=
  {
    	"bg","fg","light","shadow","text","text_grayed","selected","selected_inactive",
    	"text_selected","text_grayed_selected","text_selected_inactive","text_grayed_selected_inactive",
	"cursor","focus","nofocus","cross","check","radio_bg","radio_light"
  }; 
  unsigned char *winitemname[]=
  {
    	"bg","light","shadow","text","text_grayed",
    	"border_active","border_inactive","title_bg_active","title_bg_inactive",
	"title_text_active","title_text_inactive"
  };
  unsigned char *iconitemname[]=
  {
    	"bg","light","shadow","pixmap_fg","pixmap_bg",
    	"title_bg_active","title_bg_inactive",
	"title_text_active","title_text_inactive"
  };

  Status color;
  Bool delay=False;

  if(tk_GetConnection(tk_display)==True)
    return -1;

  xrdb=XrmGetFileDatabase(filename);
  if(xrdb==NULL)
    return -2;

  str=(unsigned char *)malloc(200);
  if(str==(unsigned char *)NULL)
    return -3;


  for(i=0;i<6;i++)
  {
    if(i==0) 
    {
	widgetname="button";
	wid_colors=tk_display->bn_colors;
    }
    if(i==1)
    {
	widgetname="scrollbar";
 	wid_colors=tk_display->sb_colors;
    }
    if(i==2) 
    {
	widgetname="edit";
  	wid_colors=tk_display->ed_colors;
    }
    if(i==3) 
    {
	widgetname="list";
 	wid_colors=tk_display->ls_colors;
    }
    if(i==4) 
    {
	widgetname="combo";
 	wid_colors=tk_display->cb_colors;
    }
    if(i==5) 
    {
	widgetname="menu";
	wid_colors=tk_display->mn_colors;
    }
    memcpy((char *)colorsptr,(char *)&wid_colors,sizeof(WidgetColors));

    for(j=0;j<(sizeof(WidgetColors)/sizeof(unsigned long));j++)
    {

    	sprintf(str,"tk.colors.%s.%s",widgetname,itemname[j]);
    	k=XrmGetResource(xrdb,(char *)str,(char *)str,&str_type,&xrmvalue);
    	if(k==1)
    	{
    	  color=XAllocNamedColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),xrmvalue.addr,&colorcell,&rgb);
    	  if(color==1)
            colorsptr[j]=colorcell.pixel;
	  else
  	  {
	    /*fprintf(stderr,"unavailable: %s\n",str);*/
	    color=XAllocColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&rgb);
    	    if(color==1)
              colorsptr[j]=rgb.pixel;
	    /*else fprintf(stderr,"unavailable: %s\n",str);*/	    
	  }
    	}
	/*else fprintf(stderr,"error: %s\n",str);*/
    }
    tk_SetSystemColors(tk_display,i+1,(WidgetColors *)colorsptr,delay); 

  }


  for(i=0;i<2;i++)
  {
    if(i==0) 
    {
	windowname="window";
	wincolorsptr=(unsigned long *)&tk_display->win_colors;
    }
    if(i==1)
    {
	windowname="dialog";
 	wincolorsptr=(unsigned long *)&tk_display->dlg_colors;
    } 

    for(j=0;j<(sizeof(WindowColors)/sizeof(unsigned long));j++)
    {

    	sprintf(str,"wm.colors.%s.%s",windowname,winitemname[j]);
    	k=XrmGetResource(xrdb,(char *)str,(char *)str,&str_type,&xrmvalue);
    	if(k==1)
    	{
    	  color=XAllocNamedColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),xrmvalue.addr,&colorcell,&rgb);
    	  if(color==1)
            wincolorsptr[j]=colorcell.pixel;
    	}
	/*else fprintf(stderr,"error: %s\n",str);*/
    }

  }

  windowname="icon";
  wincolorsptr=(unsigned long *)&tk_display->icn_colors;
   
  for(j=0;j<(sizeof(IconColors)/sizeof(unsigned long));j++)
  {
    	sprintf(str,"wm.colors.%s.%s",windowname,iconitemname[j]);
    	k=XrmGetResource(xrdb,(char *)str,(char *)str,&str_type,&xrmvalue);
    	if(k==1)
    	{
    	  color=XAllocNamedColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),xrmvalue.addr,&colorcell,&rgb);
    	  if(color==1)
            wincolorsptr[j]=colorcell.pixel;
    	}
	/*else fprintf(stderr,"error: %s\n",str);*/
  }

  free(str); 

  return 0;
}


