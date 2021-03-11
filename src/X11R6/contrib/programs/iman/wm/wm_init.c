/*
 *
 * 	wm_init.c
 * 	initialisation du WM
 *
 * 	Modification :  02/05/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
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

/*
#define DEBUG
*/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>
#include <math.h>
#include <pwd.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>



#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "iman.h"









/*
 *
 * Initialisation des donnees partagees
 *
 *
 */

int WM_InitResources()
{
 int ret;
 int i, j, k, w;

 unsigned int qt_numchildren;
 Window qt_root, qt_parent, *qt_children;
 XEvent send_event;
 XColor colorcell_def, rgb_def;
 char *str_type, *sptr;
 unsigned long *colors;
 unsigned long *version; 
 struct passwd *pw;
 int open_file;
 int size;
 char str[30];



		/*** Pre-initialise les couleurs ***/

 version=(unsigned long *)malloc(sizeof(long)*2);
 size=sizeof(WidgetColors)*6+sizeof(WindowColors)*2+sizeof(IconColors);
 colors=(unsigned long *)malloc(size);

 version[0]=wm_info.wm_version;
 version[1]=wm_info.wm_release;
 /*fprintf(stderr,"IMAN %d.%d\n",tk_display->wm.version,tk_display->wm.release);*/

 if(colors==NULL||version==NULL)
 {
   fprintf(stderr,"ERREUR: Pas assez de memoire disponible\n");
   tk_CloseSession(tk_display);
   exit(-1);
 }



			/*** Lecture des preferences ***/

  xrdb=XrmGetFileDatabase(xrm_file);

  if(xrdb==NULL)
  {
    /*fprintf(stderr,"I/O error on %s\n",xrm_file);*/
#ifndef DESQVIEW_X_SERVER
    open_file=open(xrm_file,O_RDWR|O_CREAT|O_SYNC|O_TRUNC);
#else
    open_file=open(xrm_file,O_RDWR|O_CREAT|O_TRUNC);
#endif
    if(open_file==-1)   
    {
      fprintf(stderr,"IMAN: no resource file %s \n",xrm_file);
END_OF_ALL:
#ifndef DESQVIEW_X_SERVER
      XFree(xrm_file);
#endif
/**************  LIBERER TOUS LES ZONES DE MEMOIRE UTILISEES  ************/
      exit(-1);
    }
    strcpy(str,"wm.version: 1 \n");
    write(open_file,str,strlen(str));
    strcpy(str,"wm.release: 0 \n");
    write(open_file,str,strlen(str));
    chmod(xrm_file,0677);
    close(open_file);
    chmod(xrm_file,0677);
    xrdb=XrmGetFileDatabase(xrm_file);
    if(xrdb==NULL)
    {
      fprintf(stderr,"IMAN: Error creating file %s\n",xrm_file);
      goto END_OF_ALL;
    }
  }


#ifdef DESQVIEW_X_SERVER
  XrmPutFileDatabase(xrdb,"/dvx/iman.rc");
#else
  XrmPutFileDatabase(xrdb,xrm_file);
#endif
 


	   /*** Changer la propriete et prevenir les clients ***/

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

  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&tk_display->bn_colors,sizeof(WidgetColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&tk_display->sb_colors,sizeof(WidgetColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&tk_display->ed_colors,sizeof(WidgetColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&tk_display->ls_colors,sizeof(WidgetColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&tk_display->cb_colors,sizeof(WidgetColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&tk_display->mn_colors,sizeof(WidgetColors)/sizeof(unsigned long));


  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&tk_display->win_colors,sizeof(WindowColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&tk_display->dlg_colors,sizeof(WindowColors)/sizeof(unsigned long));
  XChangeProperty(tk_display->display,RootWindow(tk_display->display,tk_display->screen), tk_display->atoms._IMAN_WM_DATA,XA_INTEGER, 32, PropModeAppend,(unsigned char *)&tk_display->icn_colors,sizeof(IconColors)/sizeof(unsigned long));


 XQueryTree(tk_display->display,RootWindow(tk_display->display,tk_display->screen),&qt_root,&qt_parent,&qt_children,&qt_numchildren);
 send_event.type=ClientMessage;
 send_event.xclient.message_type=tk_display->atoms._IMAN_WM_MESSAGES;
 send_event.xclient.data.l[0]=WmJustLoaded;
 send_event.xclient.format=32;

 for(i=0;i<qt_numchildren;i++)																		
 if(qt_children[i]!=wm_main_window&&qt_children[i]!=wm_error_window && qt_children[i]!=wm_error_title && qt_children[i]!=wm_resize_window && qt_children[i]!=wm_kill_window && qt_children[i]!=wm_desktop_window && qt_children[i]!=wm_colors_window && qt_children[i]!=wm_process_window && qt_children[i]!=wm_about_window && qt_children[i]!=wm_setup_window && qt_children[i]!=wm_clipboard_window && qt_children[i]!=wm_end_window)
 {
	send_event.xclient.window=qt_children[i];
	XSendEvent(tk_display->display,qt_children[i],False,0,&send_event);
 }


  SET_GetPreferences();


			/*** Options pour wm_info ***/


 sptr="";
 j=0;
 if(xrdb!=NULL) j=XrmGetResource(xrdb,resource_text[64],NULL,&str_type,&xrmvalue); 
 if(xrdb!=NULL&&j==1)
 {
    sptr=xrmvalue.addr;
    wm_info.desk_bg=XAllocNamedColor(tk_display->display,default_colormap,sptr,&colorcell_def,&rgb_def);
    if(wm_info.desk_bg==0)
	wm_info.desk_bg=WhitePixel(tk_display->display,tk_display->screen);
    else wm_info.desk_bg=colorcell_def.pixel;
 }
 else wm_info.desk_bg=WhitePixel(tk_display->display,tk_display->screen);
 sprintf(str,"%s",sptr);
 XrmPutStringResource(&xrdb,resource_text[64],str);
 

 sptr="";
 j=0;
 if(xrdb!=NULL) j=XrmGetResource(xrdb,resource_text[65],NULL,&str_type,&xrmvalue); 
 if(xrdb!=NULL&&j==1)
 {
    sptr=xrmvalue.addr;
    wm_info.desk_fg=XAllocNamedColor(tk_display->display,default_colormap,sptr,&colorcell_def,&rgb_def);
    if(wm_info.desk_fg==0)
	wm_info.desk_fg=WhitePixel(tk_display->display,tk_display->screen);
    else wm_info.desk_fg=colorcell_def.pixel;
 }
 else wm_info.desk_fg=BlackPixel(tk_display->display,tk_display->screen);
 sprintf(str,"%s",sptr);
 XrmPutStringResource(&xrdb,resource_text[65],str);


 j=0;
 if(xrdb!=NULL) j=XrmGetResource(xrdb,desktop_text[6],NULL,&str_type,&xrmvalue); 
 if(xrdb!=NULL&&j==1)
 {
    sptr=xrmvalue.addr;
    LANGUAGE=atoi(sptr);
    if(LANGUAGE>=NUMLANGUAGES) LANGUAGE=0;
 }
 else LANGUAGE=0;
 sprintf(str,"%d",LANGUAGE);
 /*fprintf(stderr,"LANGUAGE=%d  ",LANGUAGE);*/
 XrmPutStringResource(&xrdb,desktop_text[6],str);


 if(xrdb!=NULL) for(i=0;i<6;i++)
 {
  j=0;
  j=XrmGetResource(xrdb,options_text[i],NULL,&str_type,&xrmvalue);
  

  if(i==0 && j==1)
  {
    sptr=xrmvalue.addr;
    if(sptr!=(char *)NULL && (strcmp(sptr,"OFF")==0||strcmp(sptr,"Off")==0)) wm_info.set_decoration=False;
    else wm_info.set_decoration=True;
  }
  else if(i==1 && j==1)
  {
    sptr=xrmvalue.addr;
    if(sptr!=(char *)NULL && (strcmp(sptr,"OFF")==0||strcmp(sptr,"Off")==0)) wm_info.set_groups=False;
    else wm_info.set_groups=True;
  }
  else if(i==2 && j==1)
  {
    sptr=xrmvalue.addr;
    if(sptr!=(char *)NULL && (strcmp(sptr,"OFF")==0||strcmp(sptr,"Off")==0)) wm_info.set_icons=False;
    else wm_info.set_icons=True;
  }
  else if(i==3 && j==1)
  {
    sptr=xrmvalue.addr;
    if(strcmp(sptr,"ON")==0||strcmp(sptr,"On")==0) wm_info.set_icontitle=True;
    else wm_info.set_icontitle=False;
  }
  else if(i==4 && j==1)
  {
    sptr=xrmvalue.addr;
    if(strcmp(sptr,"OFF")==0||strcmp(sptr,"Off")==0) wm_info.set_helpactive=False;
    else wm_info.set_helpactive=True;
  }
  else if(i==5 && j==1)
  {
    sptr=xrmvalue.addr;
    if(strcmp(sptr,"OFF")==0||strcmp(sptr,"Off")==0) wm_info.set_debug=False;
    else wm_info.set_debug=True;
  }

 }
 else
 {
#ifdef DEBUG
   fprintf(stderr,"xrdb==0\n");
#endif
   wm_info.set_decoration=True;
   wm_info.set_groups=True;
   wm_info.set_icons=True;
   wm_info.set_icontitle=True;
   wm_info.set_helpactive=True;
   wm_info.set_debug=True;

   sprintf(str,"%d",wm_info.set_decoration);
   XrmPutStringResource(&xrdb,options_text[0],str);
   sprintf(str,"%d",wm_info.set_groups);
   XrmPutStringResource(&xrdb,options_text[1],str);
   sprintf(str,"%d",wm_info.set_icons);
   XrmPutStringResource(&xrdb,options_text[2],str);
   sprintf(str,"%d",wm_info.set_icontitle);
   XrmPutStringResource(&xrdb,options_text[3],str);
   sprintf(str,"%d",wm_info.set_helpactive);
   XrmPutStringResource(&xrdb,options_text[4],str);
   sprintf(str,"%d",wm_info.set_debug);
   XrmPutStringResource(&xrdb,options_text[5],str);
 }

 free(colors);
 free(version);
 if(qt_numchildren>0) XFree(qt_children);

 XA_CLIPBOARD=XInternAtom(tk_display->display,"CLIPBOARD",False);
 XA_TARGETS=XInternAtom(tk_display->display,"TARGETS",False);
 XA_OWNER_OS=XInternAtom(tk_display->display,"OWNER_OS",False);
 XA_FILE_NAME=XInternAtom(tk_display->display,"FILE_NAME",False);
 XA_HOST_NAME=XInternAtom(tk_display->display,"HOST_NAME",False);
 XA_USER=XInternAtom(tk_display->display,"USER",False);
 XA_PROCESS=XInternAtom(tk_display->display,"PROCESS",False);
 XA_TASK=XInternAtom(tk_display->display,"TASK",False);
 XA_CLIENT_WINDOW=XInternAtom(tk_display->display,"CLIENT_WINDOW",False);

}





void WM_EndOfSession()
{
 XEvent send_event;
 XWMHints xwmhints;
 int i;
 struct passwd *pw;
 unsigned char str[100];

 cl_manager.bn_colors=tk_display->bn_colors;
 cl_manager.sb_colors=tk_display->sb_colors;
 cl_manager.ed_colors=tk_display->ed_colors;
 cl_manager.ls_colors=tk_display->ls_colors;
 cl_manager.cb_colors=tk_display->cb_colors;
 cl_manager.mn_colors=tk_display->mn_colors;
 cl_manager.win_colors=tk_display->win_colors;
 cl_manager.dlg_colors=tk_display->dlg_colors;
 cl_manager.icn_colors=tk_display->icn_colors;

 DSK_SavePreferences();
 SET_SavePreferences();


 send_event.type=ClientMessage;
 send_event.xclient.message_type=tk_display->atoms.WM_PROTOCOLS;
 send_event.xclient.data.l[0]=tk_display->atoms.WM_SAVE_YOURSELF;
 send_event.xclient.format=32;
 wm_info.set_groups=False;

 sprintf(str,"%d",LANGUAGE);
 XrmPutStringResource(&xrdb,desktop_text[6],str);


 for(i=0;i<maxwindows;i++)
 if(windows[i].isUsed==True)
 {
	xwmhints.flags=IconPositionHint;
	xwmhints.icon_x=windows[i].icon.x;
	xwmhints.icon_y=windows[i].icon.y;
	
	if(windows[i].icon.pixmap>0)
	{
	  xwmhints.icon_pixmap=windows[i].icon.pixmap;
	  xwmhints.flags=xwmhints.flags+IconPixmapHint;
	}
	else xwmhints.icon_pixmap=0;
	if(windows[i].icon.mask>0)
	{
	  xwmhints.icon_mask=windows[i].icon.mask;
	  xwmhints.flags=xwmhints.flags+IconMaskHint;
	}
	else xwmhints.icon_mask=0;
	if(windows[i].icon.hasIconWindow==True)
	{
	  xwmhints.icon_window=windows[i].icon.clientwindow;
	  xwmhints.flags=xwmhints.flags+IconWindowHint;
	}
	else xwmhints.icon_window=0;
	xwmhints.initial_state=windows[i].state.initialstate;
	xwmhints.flags=xwmhints.flags+StateHint;
	if((windows[i].attributes&GroupMember)==GroupMember&&windows[i].group.leader>0)
	{
	  xwmhints.flags=xwmhints.flags+WindowGroupHint;
	  xwmhints.window_group=windows[i].group.leader;
	}
	else xwmhints.window_group=0;
	
	XSetWMHints(tk_display->display,windows[i].clientwindow,&xwmhints);
	

	if((windows[i].attributes&GroupMember)!=GroupMember)
	{
	  send_event.xclient.window=windows[i].clientwindow;
	  /*XSendEvent(tk_display->display,windows[i].clientwindow,False,0,&send_event);*/
	}
	if(windows[i].state.isMapped==False&&windows[i].state.isIconic==False) 
	{
	  XRemoveFromSaveSet(tk_display->display,windows[i].clientwindow);
	  if(windows[i].state.isZoomed==True)
	    XReparentWindow(tk_display->display,windows[i].clientwindow,RootWindow(tk_display->display,tk_display->screen),windows[i].zoom.x,windows[i].zoom.y);
	  else
	    XReparentWindow(tk_display->display,windows[i].clientwindow,RootWindow(tk_display->display,tk_display->screen),windows[i].normal.x,windows[i].normal.y);

	}
	else if(windows[i].state.isIconic==False&&WIN_VerifyTree(i)!=0)
	{
	  XRemoveFromSaveSet(tk_display->display,windows[i].clientwindow);
	  if(windows[i].state.isZoomed==True)
	    XReparentWindow(tk_display->display,windows[i].clientwindow,RootWindow(tk_display->display,tk_display->screen),windows[i].zoom.x,windows[i].zoom.y);
	  else
	    XReparentWindow(tk_display->display,windows[i].clientwindow,RootWindow(tk_display->display,tk_display->screen),windows[i].normal.x,windows[i].normal.y);
	}
	if(windows[i].state.isZoomed==True)
	  XMoveResizeWindow(tk_display->display,windows[i].clientwindow,windows[i].normal.x,windows[i].normal.y,windows[i].normal.width,windows[i].normal.height);
	if(windows[i].clientwindow!=wm_kill_window&&windows[i].clientwindow!=wm_desktop_window&&windows[i].clientwindow!=wm_colors_window && windows[i].clientwindow!=wm_process_window && windows[i].clientwindow!=wm_about_window && windows[i].clientwindow!=wm_setup_window && windows[i].clientwindow!=wm_end_window && windows[i].clientwindow!=wm_clipboard_window) 
	  XMapWindow(tk_display->display,windows[i].clientwindow);
	else XUnmapWindow(tk_display->display,windows[i].clientwindow);
 }

#ifdef DEBUG
 fprintf(stderr,"XWMHints passes\n"); 
#endif
 for(i=0;i<maxwindows;i++)
 if(windows[i].isUsed==True)
 {
   if(windows[i].group.maxmembers>0&&windows[i].group.members!=NULL) free(windows[i].group.members);
   if(windows[i].hasStoredName>0&&windows[i].title_name!=NULL) XFree(windows[i].title_name);
   if(windows[i].class!=DIALOG_BOX && windows[i].icon.hasStoredIconName>0 && windows[i].icon.name!=NULL) XFree(windows[i].icon.name);
   if(windows[i].identity.maxprotocols>0&&windows[i].identity.protocols!=NULL) free(windows[i].identity.protocols);
 }


 XSelectInput(tk_display->display,RootWindow(tk_display->display,tk_display->screen),ButtonPressMask|ButtonReleaseMask|PropertyChangeMask);
#ifdef DEBUG
 fprintf(stderr,"Fenetres passees\n");
#endif
 if(wm_fonts.times_big!=NULL) XFreeFont(tk_display->display,wm_fonts.times_big);
 if(wm_fonts.venice_28!=NULL) XFreeFont(tk_display->display,wm_fonts.venice_28);

 XFreeGC(tk_display->display,grab_gc);
 XFreeGC(tk_display->display,title_gc);
 XFreeGC(tk_display->display,border_gc); 
 XFreeGC(tk_display->display,icon_gc);
 XFreeGC(tk_display->display,resize_gc);
 XFreeGC(tk_display->display,kill_gc);
 XFreeGC(tk_display->display,desktop_gc); 
 XFreeGC(tk_display->display,setup_gc);
 XFreeGC(tk_display->display,colors_gc);
 XFreeGC(tk_display->display,clipboard_gc);
 XFreeGC(tk_display->display,end_gc);
#ifdef DEBUG
 fprintf(stderr,"GC passes\n");
#endif
 XFreePixmap(tk_display->display,wm_pixmap.warning);
 XFreePixmap(tk_display->display,pix_focus);
 XFreePixmap(tk_display->display,pix_kill);
 XFreePixmap(tk_display->display,pix_close);
 XFreePixmap(tk_display->display,pix_zoom);
 XFreePixmap(tk_display->display,pix_iconify);
 if(clip_info.pixmap>0) XFreePixmap(tk_display->display,clip_info.pixmap);

 for(i=0;i<9;i++)
   XFreePixmap(tk_display->display,tk_display->widgets[bn_colors_widgets[i]].button->pixmap);
 for(i=0;i<numpixmotifs;i++)
   XFreePixmap(tk_display->display,pix_motifs[i]);
#ifdef DEBUG
 fprintf(stderr,"Pixmaps passed\n");
#endif
 wid_Destroy(tk_display,ls_process_win);
 wid_Destroy(tk_display,ls_process_pid);
 wid_Destroy(tk_display,ls_colors_items); 
 wid_Destroy(tk_display,ls_colors_rgb_names);

#ifdef DEBUG
 fprintf(stderr,"list passed\n");
#endif
 free(windows);
 wid_Destroy(tk_display,cb_desktop_motif);
 wid_Destroy(tk_display,cb_desktop_screensaver);
 wid_Destroy(tk_display,cb_desktop_paper);
 wid_Destroy(tk_display,cb_desktop_defaulticon);
#ifdef DEBUG
 fprintf(stderr,"CB passed\n");
#endif
#ifdef DESQVIEW_X_SERVER
 XrmPutFileDatabase(xrdb,"/dvx/iman.rc");
#else 
 pw=(struct passwd *)getpwuid(getuid());
 XrmPutFileDatabase(xrdb,xrm_file);
 XFree(xrm_file);
#endif

 tk_CloseSession(tk_display);
 exit(0);

}






unsigned char mAdaptColor(index)
unsigned char index;
{
  int i, j, k, borne;
  double f;




  if(tk_display->depth>=4&&tk_display->depth<8)
  {
    i=2;
    j=16;
    borne=128;
  }
  else if(tk_display->depth>=8&&tk_display->depth<10)
  {
    i=4;
    j=8;
    borne=64;
  }
  else if(tk_display->depth>=10&&tk_display->depth<12)
  {
    i=8;
    j=4;
    borne=32;
  }
  else if(tk_display->depth>=12&&tk_display->depth<14)
  {
    i=16;
    j=2;
    borne=16;
  }
  else if(tk_display->depth>=14)
  {
    i=32;
    j=1;
    borne=8;
    return index;
  }
  

  for(k=0;k<i;k++)
  {
    if(index<(k*borne) || index>=((k+1)*borne))
      goto ADAPT_SUITE;

    f=fmod((double)(index-(k*borne)),(double)j);
    
#ifdef DEBUG
    fprintf(stderr,"Dividing %d by %d = %d\n",index,j,(unsigned char)(f)+(k*borne));
#endif
    return (unsigned char)(f+k*borne);

ADAPT_SUITE:
    i=i;
  }
  return 0;
}






