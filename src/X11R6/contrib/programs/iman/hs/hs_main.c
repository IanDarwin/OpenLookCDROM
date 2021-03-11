/*
 *
 * 	hs_main.c
 * 	Main code for the help server
 *
 * 	Modification :  13/05/94
 *
 *	Copyright (c) 1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Please read COPYING and COPYING.LIB .
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
 *      IMAN Help Server version 1.0
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */


#ifndef _IMAN_HS_MAIN_C
#define _IMAN_HS_MAIN_C



#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>
#ifndef DESQVIEW_X_SERVER
#include <pwd.h>
#endif
#include <misc/file.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/xpm.h>


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "hs.h"

#include "bm/bug.bm"
#include "bm/question1.bm"
#include "bm/question2.bm"
#include "bm/warning1.bm"
#include "bm/warning2.bm"
#include "bm/warning3.bm"
#include "bm/stop1.bm"
#include "bm/stop2.bm"
#include "bm/stack.bm"
#include "bm/dir.bm"
#include "bm/file.bm"
#include "bm/none.bm"
#include "bm/next.bm"
#include "bm/previous.bm"
#include "bm/index.bm"
#include "bm/find.bm"
#include "bm/printer.bm"
#include "bm/glossary.bm"
#include "bm/help1.bm"
#include "bm/help2.bm"


#include "xpm/next.xpm"
#include "xpm/previous.xpm"
#include "xpm/index.xpm"
#include "xpm/find.xpm"
#include "xpm/printer.xpm"
#include "xpm/glossary.xpm"






/*
 *
 * Main loop
 *
 *
 */

int main(argc, argv)
int argc;
char **argv;
{
  WidgetAttributes wid_attributes;
  WidgetTextDecoration wid_text;
  ItemPixmapDecoration item_pixmap;
  ItemTextDecoration item_text;

  XSetWindowAttributes attrib;
  XWindowAttributes xwa;
  XGCValues xgcvalues;
  XEvent send_event, eventbis;
  XColor colorcell, rgb;
  XpmAttributes xpm_attributes;

  int mask;
  int ret;
  int i, j, k, w;
  int index;
#ifndef DESQVIEW_X_SERVER
  struct passwd *pw;
#endif





			/*** Connection to the X server ***/

 XSetErrorHandler(XLIBErrorHandler);

 if(argc>0) for(i=1;i<argc;i++)
 if(strcmp(argv[1],"-?")==0 || strcmp(argv[1],"-help")==0 || strcmp(argv[1],"-Help")==0 || strcmp(argv[1],"-h")==0)
 {
   HS_Usage();
   exit(0);
 }


 i=1;
 if(argc>=3) while(strcmp(argv[i],"-display")!=0 && i<argc)
  i++;
 else i=argc;
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
	fprintf(stderr,"Error : Connection to server %s is impossible\n",argv[i+1]);
	exit(-1);
   }
 }
 if(tk_display==NULL)
 {
	fprintf(stderr,"Error : Connection to server %s is impossible\n",argv[i+1]);
	exit(-1);
 }
 i=1;
 if(argc>=3) while(strcmp(argv[i],"-silent")!=0 && i<argc)
  i++;
 if(i<argc)
   mode=MSILENT;
 else mode=MACTIVE;


 /*fprintf(stderr,"WM version %d release %d\n",tk_display->wm.version,tk_display->wm.release);*/



  		  /*** Is another IMAN help server active ? ***/

  XSync(tk_display->display,False);
  XGrabServer(tk_display->display);
  if(help_GetConnection(tk_display)==True)
  {
    fprintf(stderr,"Error : Another IMAN Help Server is running .\n");
    tk_CloseSession(tk_display);
    exit(-1);
  }

  XrmInitialize();
#ifndef DESQVIEW_X_SERVER
  wm_file=(char *)malloc(strlen("/dev/iman.rc"));
  strcpy(wm_file,"/dev/iman.rc");
#else
  pw=(struct passwd *)getpwuid(getuid());
  wm_file=(char *)malloc(strlen(pw->pw_dir)+20);
  if(strcmp(pw->pw_dir,"/")==0)
    sprintf(wm_file,"%s%s",pw->pw_dir,".imanrc");
  else sprintf(wm_file,"%s%s",pw->pw_dir,"/.imanrc");
  endpwent();   
#endif
  ret=tk_LoadSystemColorsFromFile(tk_display,wm_file);
#ifndef DESQVIEW_X_SERVER 
 if(ret!=0)
 {
    free(wm_file);
    wm_file=(char *)malloc(strlen("/usr/lib/iman/.imanrc"));
    strcpy(wm_file,"/usr/lib/iman/.imanrc");
    tk_LoadSystemColorsFromFile(tk_display,wm_file);
 }
#endif
  dir_SetCurrent(HELP_DIRECTORY);


		  /*** We create the help server main window ***/

  attrib.background_pixel=tk_display->win_colors.bg;
  attrib.border_pixel=tk_display->win_colors.border_inactive;
  attrib.cursor=tk_display->cursors.normal;
  attrib.event_mask=ButtonPressMask|ButtonReleaseMask|KeymapStateMask;
  attrib.override_redirect=False;
  mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor|CWOverrideRedirect;

  hs_main_window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),30,30,30,30,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
  XStoreName(tk_display->display,hs_main_window,"Iman Help Server. Copyright (c) 1993,1994 Bruno RIVAS");

  mask=GCForeground|GCBackground|GCGraphicsExposures;
  xgcvalues.foreground=BlackPixel(tk_display->display, tk_display->screen);
  xgcvalues.background=WhitePixel(tk_display->display, tk_display->screen);
  xgcvalues.graphics_exposures=False;
  gc=XCreateGC(tk_display->display, hs_main_window, mask, &xgcvalues);
  

  XSetSelectionOwner(tk_display->display,tk_display->atoms._IMAN_HELP_SERVER,hs_main_window,CurrentTime);
  i=1;
  XChangeProperty(tk_display->display,hs_main_window,tk_display->atoms._IMAN_HELP_SERVER,XA_INTEGER,32,PropModeReplace,(unsigned char *)&i,1);
  i=0;
  XChangeProperty(tk_display->display,hs_main_window,tk_display->atoms._IMAN_HELP_SERVER,XA_INTEGER,32,PropModeAppend,(unsigned char *)&i,1);
  XUngrabServer(tk_display->display);
  XSync(tk_display->display,False);

  bm_bug=XCreateBitmapFromData(tk_display->display,hs_main_window,bug_bits,bug_width,bug_height);
  bm_warning1=XCreateBitmapFromData(tk_display->display,hs_main_window,warning1_bits,warning1_width,warning1_height);
  bm_warning2=XCreateBitmapFromData(tk_display->display,hs_main_window,warning2_bits,warning2_width,warning2_height);
  bm_warning3=XCreateBitmapFromData(tk_display->display,hs_main_window,warning3_bits,warning3_width,warning3_height);
  bm_question1=XCreateBitmapFromData(tk_display->display,hs_main_window,question1_bits,question1_width,question1_height);
  bm_question2=XCreateBitmapFromData(tk_display->display,hs_main_window,question2_bits,question2_width,question2_height);
  bm_stop1=XCreateBitmapFromData(tk_display->display,hs_main_window,stop1_bits,stop1_width,stop1_height);
  bm_stop2=XCreateBitmapFromData(tk_display->display,hs_main_window,stop2_bits,stop2_width,stop2_height);
  bm_stack=XCreateBitmapFromData(tk_display->display,hs_main_window,stack_bits,stack_width,stack_height);
  bm_dir=XCreateBitmapFromData(tk_display->display,hs_main_window,dir_bits,dir_width,dir_height);
  bm_file=XCreateBitmapFromData(tk_display->display,hs_main_window,file_bits,file_width,file_height);
  bm_none=XCreateBitmapFromData(tk_display->display,hs_main_window,none_bits,none_width,none_height);
  bm_help1=XCreateBitmapFromData(tk_display->display,hs_main_window,help1_bits,help1_width,help1_height);
  bm_help2=XCreateBitmapFromData(tk_display->display,hs_main_window,help2_bits,help2_width,help2_height);

  ret=XpmCreatePixmapFromData(tk_display->display,hs_main_window,xpm_next_bits,&xpm_next,&xpm_next_mask,&xpm_attributes);
  if(xpm_next_mask==0)
    xpm_next_mask=XCreateBitmapFromData(tk_display->display,hs_main_window,next_bits,next_width,next_height);
  ret=XpmCreatePixmapFromData(tk_display->display,hs_main_window,xpm_previous_bits,&xpm_previous,&xpm_previous_mask,&xpm_attributes);
  if(xpm_previous_mask==0)
    xpm_previous_mask=XCreateBitmapFromData(tk_display->display,hs_main_window,previous_bits,previous_width,previous_height);
  ret=XpmCreatePixmapFromData(tk_display->display,hs_main_window,xpm_index_bits,&xpm_index,&xpm_index_mask,&xpm_attributes);
  if(xpm_index_mask==0)
    xpm_index_mask=XCreateBitmapFromData(tk_display->display,hs_main_window,index_bits,index_width,index_height);
 ret=XpmCreatePixmapFromData(tk_display->display,hs_main_window,xpm_find_bits,&xpm_find,&xpm_find_mask,&xpm_attributes);
  if(xpm_find_mask==0)
    xpm_find_mask=XCreateBitmapFromData(tk_display->display,hs_main_window,find_bits,find_width,find_height);
  ret=XpmCreatePixmapFromData(tk_display->display,hs_main_window,xpm_printer_bits,&xpm_printer,&xpm_printer_mask,&xpm_attributes);
  if(xpm_printer_mask==0)
    xpm_printer_mask=XCreateBitmapFromData(tk_display->display,hs_main_window,printer_bits,printer_width,printer_height);
  ret=XpmCreatePixmapFromData(tk_display->display,hs_main_window,xpm_glossary_bits,&xpm_glossary,&xpm_glossary_mask,&xpm_attributes);
  if(xpm_glossary_mask==0)
    xpm_glossary_mask=XCreateBitmapFromData(tk_display->display,hs_main_window,glossary_bits,glossary_width,glossary_height);



		/*** We create help sessions to handle requests ***/

  numsessions=0;
  maxsessions=0;
  sessions=(HelpSession *)NULL;
  ret=HS_AddSessions(3);
  if(ret!=0)
  {
    fprintf(stderr,"Error : Memory allocation error\n");
    tk_CloseSession(tk_display);
    exit(-1);
  }
 


		/*** Lectures des preferences de l'utilisateur ***/

  ret=XAllocNamedColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),"red1",&colorcell,&rgb);
  red1=colorcell.pixel;
  ret=XAllocNamedColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),"yellow3",&colorcell,&rgb);
  yellow1=colorcell.pixel;
  ret=XAllocNamedColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),"DarkGoldenrod3",&colorcell,&rgb);
  topicjump=colorcell.pixel;
  ret=XAllocNamedColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),"IndianRed3",&colorcell,&rgb);
  glosdef=colorcell.pixel;


/*
  HS_InitResources();
#ifdef DEBUG
  fprintf(stderr,"HS_INITRESOURCES passee\n");
#endif
*/
  if(mode==MACTIVE)
  {
    HS_UseSession(0);
    win_MapRaised(tk_display,sessions[0].win_main);
  }
			/**** Boucle d'evenements ****/
#ifdef DEBUG 
 (void)fprintf(stderr,"Debut de la boucle numchildren:%d numwindows:%d\n",qt_numchildren,numwindows);
#endif

 while(1)
 {
 
#ifdef DEBUG
   fprintf(stderr,"Recherche d'evenement ");
#endif

   XSync(tk_display->display,False); 
   tk_GetWidgetEvents(tk_display,&tk_event,False);
   XSync(tk_display->display,False); 


#ifdef DEBUG
   fprintf(stderr,"  Evenement trouve  %d  %d\n",tk_event.ev_type,tk_event.event.type);
#endif

   if(tk_event.ev_type!=NO_EVENT) 
   switch(tk_event.ev_type)
   {

  	case XLIBEVENT: 
		EV_Xlib();
		break;


	default :
		EV_Widgets();
		break;

  }

 }

}




HS_Usage()
{
  fprintf(stderr,"Usage :  hs [-display [displayname]] [-silent] [-help]\n");
  fprintf(stderr,"         -display [displayname] : X server to contact\n");
  fprintf(stderr,"         -silent : runs the Help Server in background and waits for requests\n");
  fprintf(stderr,"         -help : displays this help\n");

}



#endif

