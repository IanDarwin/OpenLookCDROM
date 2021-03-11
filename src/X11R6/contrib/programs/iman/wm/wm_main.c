/*
 *
 * 	wm_main.c
 * 	corps principal du WM
 *
 * 	Modification :  13/05/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
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



#define WM_MAIN_C
#define NEED_XRM_RESOURCES
#define WM_VERSION		1
#define WM_RELEASE		2



/*
#define DEBUG 1
*/


#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>
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
 * Gestionnaire d'erreurs XLIB
 *
 *
 */

int ErrorHandler(display,error)
Display *display;
XErrorEvent *error;
{
 char msg[80];


 /*XGetErrorText(display,error->error_code,msg,80);
 fprintf(stderr,"Error Handler:  %s %d\n",msg,error->error_code);*/
 return error->error_code;
}







/*
 *
 * Boucle principale
 *
 *
 */

void main(argc, argv)
int argc;
char **argv;
{
 WidgetAttributes wid_attributes;
 WidgetTextDecoration wid_text;
 ItemPixmapDecoration item_pixmap;
 ItemTextDecoration item_text;

 XIconSize size_list[2];
 XSetWindowAttributes attrib;
 XWindowAttributes xwa;
 Window qt_root, qt_parent, *qt_children;
 Window tc_child;
 XGCValues xgcvalues;
 XEvent send_event, eventbis;

 unsigned int qt_numchildren;
 unsigned char *colors;
 unsigned int numcolors;
 unsigned long ptr;
 char text[80];
 int mask;
 int ret, ret2;
 int i, j, k, w;
 long win_type, win_attributes;
 int tc_x, tc_y;
 Bool map_state; 
 struct passwd *pw;


			/*** Ouverture de la session ***/

 XSetErrorHandler(ErrorHandler);

 if(argc>0) for(i=1;i<argc;i++)
 if(strcmp(argv[1],"-?")==0 || strcmp(argv[1],"-help")==0)
 {
   WM_Usage(argc,argv);
   exit(0);
 }


 i=1;
 if(argc>=3) while(strcmp(argv[i],"-display")!=0 && i<argc)
  i++;
 if(i>=argc)
   tk_display=(TkDisplay *)tk_OpenSession("unix:0.0",argc,argv);
 else
 { 
   /*fprintf(stderr,"i=%d\n",i);*/
   if(i>0 && i<argc-1) tk_display=(TkDisplay *)tk_OpenSession(argv[i+1],argc,argv);
   else
   {
   	fprintf(stderr,"Error : No X server specified !\n");  
   	exit(-1);
   }
   if(tk_display==NULL)
   {
	fprintf(stderr,"Error : Connection to %s impossible\n",argv[i+1]);
	exit(-1);
   }
 }
 if(tk_display==NULL)
 {
	fprintf(stderr,"Error : Connection to the X server is impossible\n");
	exit(-1);
 }



#ifdef DESQVIEW_X_SERVER
  fprintf(stderr,"Iman connected\n");
#endif



		    /*** Obtention de quelques donnees ***/


  ZOOMMAXWIDTH=DisplayWidth(tk_display->display,tk_display->screen);
  ZOOMMAXHEIGHT=DisplayHeight(tk_display->display,tk_display->screen);
  MAXWIDTH=ZOOMMAXWIDTH;
  MAXHEIGHT=ZOOMMAXHEIGHT;

  XrmInitialize();
#ifdef DESQVIEW_X_SERVER
  xrm_file="/dvx/iman.rc";
#else
  pw=(struct passwd *)getpwuid(getuid());
  xrm_file=(char *)malloc(strlen(pw->pw_dir)+20);
  if(strcmp(pw->pw_dir,IMANRC_PATH)==0)
    sprintf(xrm_file,"%s%s",pw->pw_dir,".imanrc");
  else sprintf(xrm_file,"%s%s",pw->pw_dir,"/.imanrc");
  endpwent();   
#endif
  tk_LoadSystemColorsFromFile(tk_display,xrm_file);


		   /*** Creation de la fenetre d'erreurs ***/

  attrib.background_pixel=tk_display->dlg_colors.bg;
  attrib.border_pixel=tk_display->dlg_colors.title_bg_active;
  attrib.cursor=tk_display->cursors.normal;
  attrib.event_mask=ButtonPressMask|ButtonReleaseMask|ExposureMask|KeyPressMask|KeyReleaseMask;
  attrib.override_redirect=True;
  mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor|CWOverrideRedirect;
  wm_error_window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),(DisplayWidth(tk_display->display,0)-ERROR_BOX_WIDTH)/2,(DisplayHeight(tk_display->display,0)-ERROR_BOX_HEIGHT)/2,ERROR_BOX_WIDTH,ERROR_BOX_HEIGHT,4,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
  XStoreName(tk_display->display,wm_error_window,"ERROR");

  attrib.background_pixel=tk_display->dlg_colors.title_bg_active;
  attrib.border_pixel=tk_display->dlg_colors.title_text_active;
  attrib.cursor=tk_display->cursors.normal;
  attrib.event_mask=ExposureMask;
  attrib.override_redirect=True;
  wm_error_title=XCreateWindow(tk_display->display,wm_error_window,0,0,ERROR_BOX_WIDTH-2,TITLEBARHEIGHT,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);


  wid_attributes.mask=SALighting;
  wid_attributes.lighting=True;
  bn_error_ok=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_error_window,wm_error_window,ERROR_BOX_WIDTH-200,ERROR_BOX_HEIGHT-34,90,25,&wid_attributes,Unpushed);
  wid_text.mask=STText|STFont|STKey|STGravity;
  wid_text.font=tk_display->fonts.helvetica12;
  wid_text.text="Continue";
  wid_text.key=1;
  wid_text.gravity=CenterText;
  wid_SetTextDecoration(tk_display,bn_error_ok,&wid_text,True);

  bn_error_cancel=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_error_window,wm_error_window,ERROR_BOX_WIDTH-100,ERROR_BOX_HEIGHT-34,90,25,&wid_attributes,Unpushed);
  wid_text.text="Exit";
  wid_SetTextDecoration(tk_display,bn_error_cancel,&wid_text,True);

  wid_Map(tk_display,bn_error_ok);
  wid_Map(tk_display,bn_error_cancel);
  XMapWindow(tk_display->display,wm_error_title);

 mask=GCForeground|GCBackground;
 xgcvalues.foreground=BlackPixel(tk_display->display, tk_display->screen);
 xgcvalues.background=WhitePixel(tk_display->display, tk_display->screen);
 error_gc=XCreateGC(tk_display->display, wm_error_window, mask, &xgcvalues);

#ifdef DEBUG
  fprintf(stderr,"WM_Error created\n");
#endif


  		  /*** Verification de la presence d'un WM ***/


WM_CHECK:

#ifndef DESQVIEW_X_SERVER
  ret=EventMaskOfScreen(DefaultScreenOfDisplay(tk_display->display));
  if((ret&SubstructureRedirectMask)==SubstructureRedirectMask || (ret&ResizeRedirectMask)==ResizeRedirectMask)
  {
    fprintf(stderr,"Error : Another WM is running !\n");
    ret=WM_SendError(AnotherWMError,0);
    if(ret==0||ret==-1)
    {
	tk_CloseSession(tk_display);
	exit(-1);
    }
    else goto WM_CHECK;
  }
#endif

#ifdef DEBUG
  fprintf(stderr,"No WM detected\n");
#endif
  XSync(tk_display->display,False);
  XGrabServer(tk_display->display);


		/*** Initialisation d'infos diverses ***/

  wm_info.screen_depth=tk_display->depth;
  wm_info.screen_width=DisplayWidth(tk_display->display,tk_display->screen);
  wm_info.screen_height=DisplayHeight(tk_display->display,tk_display->screen);
  wm_info.remote=0;
  wm_action.type=NoAction;
  wm_info.wm_style=FULLDECORATION;
  tk_display->wm.version=wm_info.wm_version=WM_VERSION;
  tk_display->wm.release=wm_info.wm_release=WM_RELEASE;
  default_colormap=DefaultColormap(tk_display->display,tk_display->screen);


		      /*** Cree wm_main_window et des GC ***/

 attrib.background_pixel=tk_display->win_colors.bg;
 attrib.border_pixel=tk_display->win_colors.border_inactive;
 attrib.cursor=tk_display->cursors.normal;
 attrib.event_mask=ButtonPressMask|ButtonReleaseMask;
 attrib.override_redirect=True;
 mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor|CWOverrideRedirect;

 wm_main_window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),DisplayWidth(tk_display->display,0)-30,0,30,30,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 XStoreName(tk_display->display,wm_main_window,"IMAN Window Manager. Copyright (c) 1993,1994 Bruno RIVAS");
 

 mask=GCForeground|GCBackground;
 xgcvalues.foreground=BlackPixel(tk_display->display, tk_display->screen);
 xgcvalues.background=WhitePixel(tk_display->display, tk_display->screen);
 title_gc=XCreateGC(tk_display->display, wm_main_window, mask, &xgcvalues);
 border_gc=XCreateGC(tk_display->display, wm_main_window, mask, &xgcvalues);


 mask=GCForeground|GCBackground|GCFunction|GCSubwindowMode;
 xgcvalues.function=GXxor;
 xgcvalues.subwindow_mode=IncludeInferiors;
 xgcvalues.foreground=BlackPixel(tk_display->display, tk_display->screen);
 xgcvalues.background=WhitePixel(tk_display->display, tk_display->screen);
 grab_gc=XCreateGC(tk_display->display,RootWindow(tk_display->display,tk_display->screen), mask, &xgcvalues);

 mask=GCForeground|GCBackground;
 xgcvalues.foreground=BlackPixel(tk_display->display, tk_display->screen);
 xgcvalues.background=WhitePixel(tk_display->display, tk_display->screen);
 icon_gc=XCreateGC(tk_display->display, RootWindow(tk_display->display,tk_display->screen), mask, &xgcvalues);

#ifdef DEBUG
 fprintf(stderr,"Creation de WM_Main passee\n");
#endif

 
 size_list[0].min_width=16;
 size_list[0].min_height=16;
 size_list[0].max_width=16;
 size_list[0].max_height=16;
 size_list[0].width_inc=16;
 size_list[0].height_inc=16;

#ifdef DEBUG
 fprintf(stderr,"Step 1\n");
#endif 

XSetIconSizes(tk_display->display,RootWindow(tk_display->display,tk_display->screen),size_list,1);

#ifdef DEBUG
 fprintf(stderr,"Step 2\n");
#endif

		/*** Lectures des preferences de l'utilisateur ***/

  wm_action.type=InitAction;
  WM_InitResources();

#ifdef DEBUG
 fprintf(stderr,"Step 3\n");
#endif

  XSetSelectionOwner(tk_display->display,tk_display->atoms._IMAN_WINDOW_MANAGER,wm_main_window,CurrentTime);

#ifdef DEBUG
  fprintf(stderr,"WM_INITRESOURCES passed\n");
#endif


	     /*** Creation de fenetres diverses pour le WM ***/

  attrib.background_pixel=WhitePixel(tk_display->display,tk_display->screen);
  attrib.border_pixel=tk_display->win_colors.border_inactive;
  attrib.cursor=tk_display->cursors.normal;
  attrib.override_redirect=True;
  mask=CWBackPixel|CWBorderPixel|CWCursor|CWOverrideRedirect;

  wm_resize_window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),0,0,40,18,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
  XStoreName(tk_display->display,wm_resize_window,"WM Resize Window");

 mask=GCForeground|GCBackground;
 xgcvalues.foreground=WhitePixel(tk_display->display, tk_display->screen);
 xgcvalues.background=WhitePixel(tk_display->display, tk_display->screen);
 resize_gc=XCreateGC(tk_display->display,wm_resize_window, mask, &xgcvalues);


		   /*** Initialisation des fenetres ***/

 XQueryTree(tk_display->display,RootWindow(tk_display->display,tk_display->screen),&qt_root,&qt_parent,&qt_children,&qt_numchildren);
 XSync(tk_display->display,True);
 maxwindows=numwindows=0;


 WM_AddWindows(10);
 for(i=0;i<maxwindows;i++)
   WM_InitWindow(i,TOP_LEVEL_WINDOW,NormalWindow);
 WM_AddWindows(10);
 for(i=maxwindows;i<maxwindows+10;i++)
   WM_InitWindow(i,DIALOG_BOX,NormalWindow);

 XSync(tk_display->display,True);
 XFlush(tk_display->display);



		/*** Reparentage des fenetres clientes ***/

 
 

 XSync(tk_display->display,True);
#ifdef DEBUG
 fprintf(stderr,"Je vais reparenter les fenetres \n");
#endif

 wm_action.type=ReparentingAction;


REPARENT_START:
 if(qt_numchildren>0) for(i=0;i<qt_numchildren;i++)
REPARENT_START2:
 if(i<qt_numchildren&&qt_children[i]!=0&&qt_children[i]!=wm_main_window&&WM_GetWindowTypeWithoutProperty(qt_children[i])==-1)
 {
#ifdef DEBUG
   fprintf(stderr,"i=%d\n",i);
#endif
   WM_CheckFreeWindows();
   memset(&xwa,0,sizeof(XWindowAttributes));  

   XSync(tk_display->display,False);
   XFlush(tk_display->display);


   if(XCheckTypedWindowEvent(tk_display->display,qt_children[i],DestroyNotify,&send_event)==True)
   {
     qt_children[i]=0;
     i++;
     goto REPARENT_START2;
   }

#ifdef DEBUG
   fprintf(stderr,"Avant getType\n");
#endif
   win_type=win_GetType(tk_display,qt_children[i]);
   if(win_type==-1) win_type=TOP_LEVEL_WINDOW;
#ifdef DEBUG
   fprintf(stderr,"Apres getType  %d\n",win_type);
#endif

   ret=XGetWindowAttributes(tk_display->display,qt_children[i],&xwa);
   if(xwa.map_state==IsViewable) map_state=1;
   else map_state=0;

   if(ret==0)
   {
     qt_children[i]=0;
     i++;
     goto REPARENT_START2;
   }

   if(ret!=0&&xwa.override_redirect==False&&tk_event.event.xcreatewindow.override_redirect==False)
   {
	ret=j=0;
	j=WM_GetUnusedWindow(win_type);
#ifdef DEBUG
        fprintf(stderr,"Reparenting %d\n",i);
#endif
	ret=WIN_ReparentClient(j,qt_children[i]);
#ifdef DEBUG
	fprintf(stderr,"Classe:%d  Attribut:%d  Client:%ld  GroupMember:%d\n",windows[j].class,windows[j].attributes,windows[j].clientwindow,(windows[j].attributes)&GroupMember);
#endif
	if(ret!=0)
	{
	    WM_ReInitWindow(j);
	    qt_children[i]=0;
	    i++;
	    goto REPARENT_START2;
	}
	numwindows++;
	windows[j].isUsed=True;
	/*WIN_VerifyLinks(j);*/
	if(map_state==1&&ret!=-1)
	{
	  windows[j].state.isMapped=True;
          windows[j].state.isInitialized=True;
	  WIN_MapRaised(j);

	}
        else if(ret!=-1)
	{ 
	  windows[j].state.isMapped=False;
	  windows[j].state.initialstate=NormalState;
	  WIN_SetClientState(j,NormalState);
          windows[j].state.isInitialized=True;
	}

	send_event.type=ClientMessage;
  	send_event.xclient.send_event=True;
	send_event.xclient.format=32;
	send_event.xclient.window=windows[j].clientwindow;
      	send_event.xclient.message_type=tk_display->atoms._IMAN_WM_MESSAGES;
	send_event.xclient.data.l[0]=WmJustLoaded;		  	

	XSendEvent(tk_display->display,windows[j].clientwindow,False,0,&send_event);

    }

 }

  wm_action.type=NoAction;
  XSync(tk_display->display,False);
#ifdef DEBUG
  fprintf(stderr,"Reparentage termine\n");
#endif

  if(qt_numchildren>0) XFree(qt_children);



		/* Prise de controle de la fenetre racine */
																												
  XSelectInput(tk_display->display,RootWindow(tk_display->display,tk_display->screen),SubstructureNotifyMask|SubstructureRedirectMask|ButtonPressMask|StructureNotifyMask|ButtonReleaseMask|ResizeRedirectMask|PropertyChangeMask|KeyReleaseMask);
  mask=CWCursor;
  attrib.cursor=tk_display->cursors.normal;
  XChangeWindowAttributes(tk_display->display,RootWindow(tk_display->display,tk_display->screen),mask,&attrib);
  XSync(tk_display->display,False);
  XUngrabServer(tk_display->display);



			/***** Affichage du WM *****/

  /*XMapWindow(tk_display->display,wm_main_window);*/
 



	     /*** Creation de la fenetre de fin de processus ***/

  wm_action.type=InitAction;
#ifdef DEBUG
  fprintf(stderr,"KILL init\n");
#endif
  KILL_Init();


		/*** Creation de la gestion du bureau ***/
#ifdef DEBUG
  fprintf(stderr,"DSK init\n");
#endif
  DSK_Init();
#ifdef DEBUG
  fprintf(stderr,"PID init\n");
#endif
  PID_Init();
#ifdef DEBUG
  fprintf(stderr,"SET init\n");
#endif
  SET_Init();
  END_Init();

			/**** Boucle d'evenements ****/
#ifdef DEBUG 
 (void)fprintf(stderr,"Debut de la boucle numchildren:%d numwindows:%d\n",qt_numchildren,numwindows);
#endif
 wm_action.type=NoAction;
 while(1)
 {
 
#ifdef DEBUG
   fprintf(stderr,"Looking for events ...\n");
#endif

   XSync(tk_display->display,False); 
   tk_GetWidgetEvents(tk_display,&tk_event,False);
   XSync(tk_display->display,False); 
   XFlush(tk_display->display); 

#ifdef DEBUG
   fprintf(stderr,"  Event found  %d  %d\n",tk_event.ev_type,tk_event.event.type);
#endif

   if(tk_event.ev_type!=NO_EVENT) 
   switch(tk_event.ev_type)
   {

  	case XLIBEVENT: 

		WM_XlibEvents();
		break;


	default :

		WM_WidgetsEvents();
		break;

  }

 }

}



