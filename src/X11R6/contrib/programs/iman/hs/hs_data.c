/*
 *
 * 	hs_data.c
 * 	Help structures management
 *
 * 	Modification :  30/04/94
 *
 *	Copyright (c) 1994 Bruno RIVAS
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


#ifndef _IMAN_HS_DATA_C
#define _IMAN_HS_DATA_C



#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>
#include <misc/file.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "hs.h"





int HS_AddSessions(number)
unsigned int number;
{
  unsigned int i;
  int ret;

  if(maxsessions==0)
  {
    sessions=(HelpSession *)malloc(sizeof(HelpSession)*number);
    if(sessions==(HelpSession *)NULL)
      return -1;
    for(i=0;i<number;i++)
    {
       ret=HS_InitSession(i);
       if(ret!=0)
	 return -1;
    }
    maxsessions=maxsessions+number;
    return 0;
  }
  else
  {
    sessions=(HelpSession *)realloc((char *)sessions,sizeof(HelpSession)*(number+maxsessions));
    if(sessions==(HelpSession *)NULL)
      return -1;
    for(i=0;i<number;i++)
    {
       ret=HS_InitSession(maxsessions+i);
       if(ret!=0)
	 return -1;
    }
    maxsessions=maxsessions+number;
    return 0;
  }
}



int HS_InitSession(number)
unsigned int number;
{
  WidgetAttributes wid_attributes;
  WidgetTextDecoration wid_text;
  WidgetPixmapDecoration wid_pixmap;
  ItemPixmapDecoration item_pixmap;
  ItemTextDecoration item_text;

  XSetWindowAttributes attrib;
  XWindowAttributes xwa;
  XGCValues xgcvalues;
  XEvent send_event, eventbis;
  XSizeHints hints;
  Atom protocols[3];

  int mask;
  int ret;
  int i, j, k, w;




  sessions[number].isUsed=False;


			     /*** Main Window ***/

  attrib.background_pixel=tk_display->win_colors.bg;
  attrib.border_pixel=tk_display->win_colors.text;
  attrib.cursor=tk_display->cursors.normal;
  attrib.event_mask=StructureNotifyMask;
  mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor;

  sessions[number].win_main=win_Create(tk_display,RootWindow(tk_display->display,tk_display->screen),RootWindow(tk_display->display,tk_display->screen),TOP_LEVEL_WINDOW,Border+TitleBar+CloseBox+IconifyBox+ZoomBox+GroupLeader,0,0,500,450,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib,NormalState);
  win_SetTitleName(tk_display,sessions[number].win_main,"Help");
  win_SetGroupLeader(tk_display,sessions[number].win_main,hs_main_window);
 
  protocols[0]=tk_display->atoms.WM_DELETE_WINDOW;
  protocols[1]=tk_display->atoms.WM_SAVE_YOURSELF;
  protocols[2]=tk_display->atoms._IMAN_WM_MESSAGES;
  win_SetWMProtocols(tk_display,sessions[number].win_main,protocols,3);		
  hints.flags=PMinSize|PSize|PPosition;
  hints.min_width=265;
  hints.min_height=230;
  hints.x=0;
  hints.y=0;
  hints.width=500;
  hints.height=450;
  win_SetNormalHints(tk_display,sessions[number].win_main,&hints);

  mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor|CWWinGravity;
  attrib.background_pixel=WhitePixel(tk_display->display,tk_display->screen);
  attrib.win_gravity=NorthWestGravity;
  attrib.event_mask=ButtonPressMask|ButtonReleaseMask|ExposureMask;
  sessions[number].win_draw=win_Create(tk_display,sessions[number].win_main,sessions[number].win_main,0,0,-1,60,479,387,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib,NormalState);
  sessions[number].width=479;
  sessions[number].height=387;
  win_MapRaised(tk_display,sessions[number].win_draw);	
  
  /*mask=GCForeground|GCBackground;
  xgcvalues.foreground=BlackPixel(tk_display->display, tk_display->screen);
  xgcvalues.background=WhitePixel(tk_display->display, tk_display->screen);
  sessions[number].gc=XCreateGC(tk_display->display, sessions[number].win_main, mask, &xgcvalues);*/

			/*** Main window widgets ***/
						
  wid_attributes.range=0;
  wid_attributes.pagerange=10;
  wid_attributes.thumbsize=20;
  wid_attributes.position=0;
  wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAPosition;
  sessions[number].sb_main=wid_Create(tk_display,WI_SCROLLBAR,SB_TOPALIGN,sessions[number].win_main,sessions[number].win_main,480,60,19,387,&wid_attributes,Grayed);  
  wid_Map(tk_display,sessions[number].sb_main);


  wid_attributes.mask=SALighting+SANeverFocus;
  wid_attributes.lighting=False;
  wid_attributes.neverFocus=True;
  sessions[number].bn_index=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,sessions[number].win_main,sessions[number].win_main,5,27,24,24,&wid_attributes,Grayed);
  wid_Map(tk_display,sessions[number].bn_index);
  if(xpm_index_mask!=0)
    wid_pixmap.mask=SPPixmap+SPPixmapMask+SPDepth+SPWidth+SPHeight+SPGravity;
  else wid_pixmap.mask=SPPixmap+SPDepth+SPWidth+SPHeight+SPGravity;
  wid_pixmap.pixmap=xpm_index;
  wid_pixmap.pixmap_mask=xpm_index_mask;
  wid_pixmap.depth=tk_display->depth;
  wid_pixmap.width=16;
  wid_pixmap.height=16;
  wid_pixmap.gravity=CenterBitmap;
  wid_SetPixmapDecoration(tk_display,sessions[number].bn_index,&wid_pixmap,True);


  sessions[number].bn_previous=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,sessions[number].win_main,sessions[number].win_main,35,27,24,24,&wid_attributes,Grayed);
  wid_Map(tk_display,sessions[number].bn_previous);
  if(xpm_previous_mask!=0)
    wid_pixmap.mask=SPPixmap+SPPixmapMask+SPDepth+SPWidth+SPHeight+SPGravity;
  else wid_pixmap.mask=SPPixmap+SPDepth+SPWidth+SPHeight+SPGravity;
  wid_pixmap.pixmap=xpm_previous;
  wid_pixmap.pixmap_mask=xpm_previous_mask;
  wid_pixmap.depth=tk_display->depth;
  wid_pixmap.width=16;
  wid_pixmap.height=16;
  wid_pixmap.gravity=CenterBitmap;
  wid_SetPixmapDecoration(tk_display,sessions[number].bn_previous,&wid_pixmap,True);
  sessions[number].bn_next=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,sessions[number].win_main,sessions[number].win_main,65,27,24,24,&wid_attributes,Grayed);
  wid_Map(tk_display,sessions[number].bn_next);
  if(xpm_next_mask!=0)
    wid_pixmap.mask=SPPixmap+SPPixmapMask+SPDepth+SPWidth+SPHeight+SPGravity;
  else wid_pixmap.mask=SPPixmap+SPDepth+SPWidth+SPHeight+SPGravity;
  wid_pixmap.pixmap=xpm_next;
  wid_pixmap.pixmap_mask=xpm_next_mask;
  wid_pixmap.depth=tk_display->depth;
  wid_pixmap.width=16;
  wid_pixmap.height=16;
  wid_pixmap.gravity=CenterBitmap;
  wid_SetPixmapDecoration(tk_display,sessions[number].bn_next,&wid_pixmap,True);

  sessions[number].bn_seek=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,sessions[number].win_main,sessions[number].win_main,95,27,24,24,&wid_attributes,Grayed);
  /*wid_Map(tk_display,sessions[number].bn_seek);*/
  if(xpm_find_mask!=0)
    wid_pixmap.mask=SPPixmap+SPPixmapMask+SPDepth+SPWidth+SPHeight+SPGravity;
  else wid_pixmap.mask=SPPixmap+SPDepth+SPWidth+SPHeight+SPGravity;
  wid_pixmap.pixmap=xpm_find;
  wid_pixmap.pixmap_mask=xpm_find_mask;
  wid_pixmap.depth=tk_display->depth;
  wid_pixmap.width=16;
  wid_pixmap.height=16;
  wid_pixmap.gravity=CenterBitmap;
  wid_SetPixmapDecoration(tk_display,sessions[number].bn_seek,&wid_pixmap,True);

  sessions[number].bn_history=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,sessions[number].win_main,sessions[number].win_main,125,27,24,24,&wid_attributes,Grayed);
  /*wid_Map(tk_display,sessions[number].bn_history);*/
  if(xpm_glossary_mask!=0)
    wid_pixmap.mask=SPPixmap+SPPixmapMask+SPDepth+SPWidth+SPHeight+SPGravity;
  else wid_pixmap.mask=SPPixmap+SPDepth+SPWidth+SPHeight+SPGravity;
  wid_pixmap.pixmap=xpm_glossary;
  wid_pixmap.pixmap_mask=xpm_glossary_mask;
  wid_pixmap.depth=tk_display->depth;
  wid_pixmap.width=16;
  wid_pixmap.height=16;
  wid_pixmap.gravity=CenterBitmap;
  wid_SetPixmapDecoration(tk_display,sessions[number].bn_history,&wid_pixmap,True);

  sessions[number].bn_print=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,sessions[number].win_main,sessions[number].win_main,155,27,24,24,&wid_attributes,Grayed);
  /*wid_Map(tk_display,sessions[number].bn_print);*/
  if(xpm_printer_mask!=0)
    wid_pixmap.mask=SPPixmap+SPPixmapMask+SPDepth+SPWidth+SPHeight+SPGravity;
  else wid_pixmap.mask=SPPixmap+SPDepth+SPWidth+SPHeight+SPGravity;
  wid_pixmap.pixmap=xpm_printer;
  wid_pixmap.pixmap_mask=xpm_printer_mask;
  wid_pixmap.depth=tk_display->depth;
  wid_pixmap.width=16;
  wid_pixmap.height=16;
  wid_pixmap.gravity=CenterBitmap;
  wid_SetPixmapDecoration(tk_display,sessions[number].bn_print,&wid_pixmap,True);


  wid_attributes.mask=0; 
  sessions[number].mn_main=wid_Create(tk_display,WI_MENU,MN_MENUBAR,sessions[number].win_main,sessions[number].win_main,0,0,2000,21,&wid_attributes,0);
  wid_Map(tk_display,sessions[number].mn_main);  
  sessions[number].mn_file=wid_Create(tk_display,WI_MENU,MN_FLOATING,sessions[number].win_main,sessions[number].win_main,0,0,600,20,&wid_attributes,0);
    sessions[number].mn_options=wid_Create(tk_display,WI_MENU,MN_FLOATING,sessions[number].win_main,sessions[number].win_main,0,0,600,20,&wid_attributes,0);
    sessions[number].mn_help=wid_Create(tk_display,WI_MENU,MN_FLOATING,sessions[number].win_main,sessions[number].win_main,0,0,600,20,&wid_attributes,0);

sessions[number].mn_window=wid_Create(tk_display,WI_MENU,MN_FLOATING,sessions[number].win_main,sessions[number].win_main,0,0,600,20,&wid_attributes,0);

  item_Add(tk_display,sessions[number].mn_file,0,START,MN_ITEM,"New",0,0,0,True);
  item_Add(tk_display,sessions[number].mn_file,0,END,MN_ITEM,"Open",0,0,0,True);
  /*item_Add(tk_display,sessions[number].mn_file,0,END,MN_HBAR,"",0,0,0,True);
  item_Add(tk_display,sessions[number].mn_file,0,END,MN_ITEM,"Print",0,0,0,True);*/
  item_Add(tk_display,sessions[number].mn_file,0,END,MN_HBAR,"",0,0,0,True);
  item_Add(tk_display,sessions[number].mn_file,0,END,MN_ITEM,"Close",0,0,0,True);
  item_Add(tk_display,sessions[number].mn_file,0,END,MN_ITEM,"Exit",0,0,0,True);

  item_Add(tk_display,sessions[number].mn_help,0,START,MN_ITEM,"About ...",0,0,0,True);
  /*item_Add(tk_display,sessions[number].mn_help,0,END,MN_ITEM,"Help on help",0,0,Grayed,True);*/

  item_Add(tk_display,sessions[number].mn_main,sessions[number].mn_file,END,MN_SUBMENU,"File",0,0,0,True);
  item_Add(tk_display,sessions[number].mn_main,sessions[number].mn_options,END,MN_SUBMENU,"Options",0,0,Grayed,True);
  item_Add(tk_display,sessions[number].mn_main,sessions[number].mn_window,END,MN_SUBMENU,"Window",0,0,0,True);
  item_Add(tk_display,sessions[number].mn_main,sessions[number].mn_help,END,MN_SUBMENU,"Help",0,0,0,True);

  item_Add(tk_display,sessions[number].mn_window,0,END,MN_ITEM,"Topics",0,0,0,True);
  item_Add(tk_display,sessions[number].mn_window,0,END,MN_ITEM,"Glossary",0,0,Grayed,True);
  /*item_Add(tk_display,sessions[number].mn_window,0,END,MN_ITEM,"Help Editor",0,0,Grayed,True);
  item_Add(tk_display,sessions[number].mn_window,0,END,MN_ITEM,"Help Compiler",0,0,Grayed,True);*/
 


			    /*** Topics window ***/


  attrib.background_pixel=tk_display->win_colors.bg;
  attrib.border_pixel=tk_display->win_colors.text;
  attrib.cursor=tk_display->cursors.normal;
  attrib.event_mask=StructureNotifyMask;
  mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor;

  sessions[number].win_topics=win_Create(tk_display,RootWindow(tk_display->display,tk_display->screen),RootWindow(tk_display->display,tk_display->screen),TOP_LEVEL_WINDOW,Border+TitleBar+CloseBox+ZoomBox+GroupMember+AlwaysOnTop,400,140,280,220,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib,NormalState);
  win_SetTitleName(tk_display,sessions[number].win_topics,"Topics");																									
 
  protocols[0]=tk_display->atoms.WM_DELETE_WINDOW;
  protocols[1]=tk_display->atoms.WM_SAVE_YOURSELF;
  protocols[2]=tk_display->atoms._IMAN_WM_MESSAGES;
  win_SetWMProtocols(tk_display,sessions[number].win_topics,protocols,3);
  win_SetGroupLeader(tk_display,sessions[number].win_topics,sessions[number].win_main);		
  hints.flags=PMinSize|PSize|PPosition;
  hints.min_width=250;
  hints.min_height=180;
  hints.x=400;
  hints.y=140;
  hints.width=280;
  hints.height=220;
  win_SetNormalHints(tk_display,sessions[number].win_topics,&hints);

						
  wid_attributes.vtype=SB_BOTTOMALIGN;
  wid_attributes.thumbsize=20;
  wid_attributes.htype=SB_RIGHTALIGN;
  wid_attributes.mask=SAThumbsize+SAHVType;
  sessions[number].ls_topics=wid_Create(tk_display,WI_LIST,LS_RIGHTVSCROLL,sessions[number].win_topics,sessions[number].win_topics,-1,-1,350,260,&wid_attributes,0);  
  wid_Map(tk_display,sessions[number].ls_topics);



			    /*** Glossary window ***/


  attrib.background_pixel=tk_display->win_colors.bg;
  attrib.border_pixel=tk_display->win_colors.text;
  attrib.cursor=tk_display->cursors.normal;
  attrib.event_mask=StructureNotifyMask;
  mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor;

  sessions[number].win_glossary=win_Create(tk_display,RootWindow(tk_display->display,tk_display->screen),RootWindow(tk_display->display,tk_display->screen),TOP_LEVEL_WINDOW,Border+TitleBar+CloseBox+ZoomBox+GroupMember+AlwaysOnTop,20,370,280,220,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib,NormalState);
  win_SetTitleName(tk_display,sessions[number].win_glossary,"Glossary");																									
 
  protocols[0]=tk_display->atoms.WM_DELETE_WINDOW;
  protocols[1]=tk_display->atoms.WM_SAVE_YOURSELF;
  protocols[2]=tk_display->atoms._IMAN_WM_MESSAGES;
  win_SetWMProtocols(tk_display,sessions[number].win_glossary,protocols,3);
  win_SetGroupLeader(tk_display,sessions[number].win_glossary,sessions[number].win_main);		
  hints.flags=PMinSize|PSize|PPosition;
  hints.min_width=250;
  hints.min_height=180;
  hints.x=20;
  hints.y=370;
  hints.width=280;
  hints.height=220;
  win_SetNormalHints(tk_display,sessions[number].win_glossary,&hints);

  wid_attributes.range=50;
  wid_attributes.pagerange=10;
  wid_attributes.thumbsize=20;
  wid_attributes.position=0;
  wid_attributes.mask=SARange+SAPagerange+SAThumbsize+SAPosition;
  
sessions[number].ls_glossary=wid_Create(tk_display,WI_LIST,LS_RIGHTVSCROLL,sessions[number].win_glossary,sessions[number].win_glossary,-1,-1,280,210,&wid_attributes,0); 					
  wid_Map(tk_display,sessions[number].ls_glossary);


			    /*** About dialog ***/


  attrib.background_pixel=tk_display->dlg_colors.bg;
  attrib.border_pixel=tk_display->dlg_colors.text;
  attrib.cursor=tk_display->cursors.normal;
  attrib.event_mask=ExposureMask;
  mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor;

  sessions[number].win_about=win_Create(tk_display,RootWindow(tk_display->display,tk_display->screen),RootWindow(tk_display->display,tk_display->screen),DIALOG_BOX,Border+TitleBar,(DisplayWidth(tk_display->display,tk_display->screen)-400)/2,(DisplayHeight(tk_display->display,tk_display->screen)-260)/2,400,260,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib,NormalState);
  win_SetTitleName(tk_display,sessions[number].win_about,"About");																									
 
  protocols[0]=tk_display->atoms.WM_DELETE_WINDOW;
  protocols[1]=tk_display->atoms.WM_SAVE_YOURSELF;
  protocols[2]=tk_display->atoms._IMAN_WM_MESSAGES;
  win_SetWMProtocols(tk_display,sessions[number].win_about,protocols,3);
  win_SetTransientFor(tk_display,sessions[number].win_about,sessions[number].win_main);		
  hints.flags=PMinSize|PMaxSize|PSize|PPosition;
  hints.min_width=400;
  hints.min_height=260;
  hints.max_width=400;
  hints.max_height=260;
  hints.x=20;
  hints.y=20;
  hints.width=400;
  hints.height=260;
  win_SetNormalHints(tk_display,sessions[number].win_about,&hints);

  wid_attributes.lighting=True;
  wid_attributes.mask=SALighting;
  
sessions[number].bn_about_ok=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,sessions[number].win_about,sessions[number].win_about,155,225,85,26,&wid_attributes,0); 					
  wid_Map(tk_display,sessions[number].bn_about_ok);

  wid_text.mask=STText|STFont|STKey|STGravity;
  wid_text.text="Validate";
  wid_text.key=1;
  wid_text.font=tk_display->fonts.helvetica12;
  wid_text.gravity=CenterText;
  wid_SetTextDecoration(tk_display,sessions[number].bn_about_ok,&wid_text,True);



			    /*** Error dialog ***/


  attrib.background_pixel=tk_display->dlg_colors.bg;
  attrib.border_pixel=tk_display->dlg_colors.text;
  attrib.cursor=tk_display->cursors.normal;
  attrib.event_mask=ExposureMask;
  mask=CWBackPixel|CWEventMask|CWBorderPixel|CWCursor;

  sessions[number].win_error=win_Create(tk_display,RootWindow(tk_display->display,tk_display->screen),RootWindow(tk_display->display,tk_display->screen),DIALOG_BOX,Border+TitleBar+CloseBox,(DisplayWidth(tk_display->display,tk_display->screen)-380)/2,(DisplayHeight(tk_display->display,tk_display->screen)-240)/2,380,240,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib,NormalState);
  win_SetTitleName(tk_display,sessions[number].win_error,"Error");																									
 
  protocols[0]=tk_display->atoms.WM_DELETE_WINDOW;
  protocols[1]=tk_display->atoms.WM_SAVE_YOURSELF;
  protocols[2]=tk_display->atoms._IMAN_WM_MESSAGES;
  win_SetWMProtocols(tk_display,sessions[number].win_error,protocols,3);
  win_SetTransientFor(tk_display,sessions[number].win_error,sessions[number].win_main);		
  hints.flags=PMinSize|PSize|PPosition;
  hints.min_width=300;
  hints.min_height=240;
  hints.x=20;
  hints.y=20;
  hints.width=380;
  hints.height=240;
  win_SetNormalHints(tk_display,sessions[number].win_error,&hints);

  wid_attributes.lighting=True;
  wid_attributes.mask=SALighting;
  
sessions[number].bn_error_ok=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,sessions[number].win_error,sessions[number].win_error,146,205,85,26,&wid_attributes,0); 					
  wid_Map(tk_display,sessions[number].bn_error_ok);

  wid_text.mask=STText|STFont|STKey|STGravity;
  wid_text.text="Validate";
  wid_text.key=1;
  wid_text.font=tk_display->fonts.helvetica12;
  wid_text.gravity=CenterText;
  wid_SetTextDecoration(tk_display,sessions[number].bn_error_ok,&wid_text,True);


			    /*** Warning dialog ***/

  sessions[number].win_warning=win_Create(tk_display,RootWindow(tk_display->display,tk_display->screen),RootWindow(tk_display->display,tk_display->screen),DIALOG_BOX,Border+TitleBar+CloseBox,(DisplayWidth(tk_display->display,tk_display->screen)-380)/2,(DisplayHeight(tk_display->display,tk_display->screen)-240)/2,380,240,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib,NormalState);
  win_SetTitleName(tk_display,sessions[number].win_warning,"Warning");																									
 
  protocols[0]=tk_display->atoms.WM_DELETE_WINDOW;
  protocols[1]=tk_display->atoms.WM_SAVE_YOURSELF;
  protocols[2]=tk_display->atoms._IMAN_WM_MESSAGES;
  win_SetWMProtocols(tk_display,sessions[number].win_warning,protocols,3);
  win_SetTransientFor(tk_display,sessions[number].win_warning,sessions[number].win_main);		
  hints.flags=PMinSize|PSize|PPosition;
  hints.min_width=300;
  hints.min_height=240;
  hints.x=20;
  hints.y=20;
  hints.width=380;
  hints.height=240;
  win_SetNormalHints(tk_display,sessions[number].win_error,&hints);

  wid_attributes.lighting=True;
  wid_attributes.mask=SALighting;
  
sessions[number].bn_warning_ok=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,sessions[number].win_warning,sessions[number].win_warning,146,205,85,26,&wid_attributes,0); 					
  wid_Map(tk_display,sessions[number].bn_warning_ok);

  wid_text.mask=STText|STFont|STKey|STGravity;
  wid_text.text="Validate";
  wid_text.key=1;
  wid_text.font=tk_display->fonts.helvetica12;
  wid_text.gravity=CenterText;
  wid_SetTextDecoration(tk_display,sessions[number].bn_warning_ok,&wid_text,True);


			    /*** Open dialog ***/

  sessions[number].win_open=win_Create(tk_display,RootWindow(tk_display->display,tk_display->screen),RootWindow(tk_display->display,tk_display->screen),DIALOG_BOX,Border+TitleBar+CloseBox,100,200,500,340,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib,NormalState);
  win_SetTitleName(tk_display,sessions[number].win_open,"Dir :");	
 
  protocols[0]=tk_display->atoms.WM_DELETE_WINDOW;
  protocols[1]=tk_display->atoms.WM_SAVE_YOURSELF;
  protocols[2]=tk_display->atoms._IMAN_WM_MESSAGES;
  win_SetWMProtocols(tk_display,sessions[number].win_open,protocols,3);
  win_SetTransientFor(tk_display,sessions[number].win_open,sessions[number].win_main);		
  hints.flags=PMinSize|PSize|PPosition;
  hints.min_width=500;
  hints.min_height=340;
  hints.x=100;
  hints.y=200;
  hints.width=500;
  hints.height=340;
  win_SetNormalHints(tk_display,sessions[number].win_open,&hints);

  wid_attributes.lighting=True;
  wid_attributes.mask=SALighting;
  
sessions[number].bn_open_ok=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,sessions[number].win_open,sessions[number].win_open,160,300,85,26,&wid_attributes,0); 					
  wid_Map(tk_display,sessions[number].bn_open_ok);
   
sessions[number].bn_open_cancel=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,sessions[number].win_open,sessions[number].win_open,255,300,85,26,&wid_attributes,0); 					
  wid_Map(tk_display,sessions[number].bn_open_cancel);
  
  wid_attributes.mask=0;
sessions[number].ed_open=wid_Create(tk_display,WI_EDIT,ED_FULLSELECT,sessions[number].win_open,sessions[number].win_open,10,11,480,25,&wid_attributes,0); 					
  wid_Map(tk_display,sessions[number].ed_open);
  wid_text.mask=STText|STFont;
  wid_text.text="*";
  wid_text.font=tk_display->fonts.helvetica12;
  wid_SetTextDecoration(tk_display,sessions[number].ed_open,&wid_text,True);


  wid_attributes.vtype=SB_BOTTOMALIGN;
  wid_attributes.htype=SB_RIGHTALIGN;
  wid_attributes.itemheight=16;
  wid_attributes.mask=SAHVType+SAItemHeight;

sessions[number].ls_open_dir=wid_Create(tk_display,WI_LIST,LS_RIGHTVSCROLL,sessions[number].win_open,sessions[number].win_open,10,53,235,230,&wid_attributes,0); 					
  wid_Map(tk_display,sessions[number].ls_open_dir);

sessions[number].ls_open_file=wid_Create(tk_display,WI_LIST,LS_RIGHTVSCROLL,sessions[number].win_open,sessions[number].win_open,255,53,235,230,&wid_attributes,0); 					
  wid_Map(tk_display,sessions[number].ls_open_file);


  wid_text.mask=STText|STFont|STKey|STGravity;
  wid_text.text="Open";
  wid_text.key=1;
  wid_text.font=tk_display->fonts.helvetica12;
  wid_text.gravity=CenterText;
  wid_SetTextDecoration(tk_display,sessions[number].bn_open_ok,&wid_text,True);

  wid_text.text="Cancel";
  wid_SetTextDecoration(tk_display,sessions[number].bn_open_cancel,&wid_text,True);
 

  sessions[number].file=-1;
  sessions[number].format=0;
  sessions[number].filename=(char *)NULL;
  sessions[number].dirname=(char *)NULL;
  sessions[number].current_dir=(char *)NULL;
  sessions[number].numfileinfos=0;
  sessions[number].fileinfos=(FileInfo **)0;
  sessions[number].version=-1;
  sessions[number].release=-1;
  sessions[number].help_name=(char *)NULL;
  sessions[number].copyright=(char *)NULL;
  sessions[number].vendor=(char *)NULL;
  sessions[number].index=0;
  sessions[number].year=0;
  sessions[number].month=0;
  sessions[number].day=0;
  sessions[number].numresources=0;
  sessions[number].loadedresources=0;
  sessions[number].maxresources=0;
  sessions[number].resources=(HelpResource *)NULL;
  sessions[number].numtopics=0;
  sessions[number].loadedtopics=0;
  sessions[number].maxtopics=0;
  sessions[number].topics=(HelpTopic *)NULL;
  sessions[number].current_topic=-1;
  sessions[number].current_item=0;
  sessions[number].current_line=0;
  sessions[number].inc=0;
  sessions[number].numlines=0;
  sessions[number].scroll_lines=0;
  sessions[number].maxlines=0;
  sessions[number].lines=(Lines *)NULL;
  sessions[number].position=0;
  sessions[number].showAllTopics=False;
  sessions[number].action=NoAction;
  sessions[number].owner=0;

  XSync(tk_display->display,False);
  return 0;
}





int HS_UnuseSession(number)
unsigned int number;
{
  if(number>=maxsessions)
    return -1;

  if(sessions[number].isUsed==False)
    return -2;

  if(sessions[number].file>=0)
  {
    close(sessions[number].file);
    if(sessions[number].stream!=(FILE *)NULL)
      fclose(sessions[number].stream);
    sessions[number].file=-1;
    sessions[number].stream=(FILE *)NULL;
  }
  sessions[number].format=0;
  sessions[number].version=-1;
  sessions[number].release=-1;
  sessions[number].index=0;
  sessions[number].year=0;
  sessions[number].month=0;
  sessions[number].day=0;

  if(sessions[number].help_name!=(char *)NULL)
  {
    free(sessions[number].help_name);
    sessions[number].help_name=(char *)NULL;
  }
  if(sessions[number].vendor!=(char *)NULL)
  {
    free(sessions[number].vendor);
    sessions[number].vendor=(char *)NULL;
  }
  if(sessions[number].copyright!=(char *)NULL)
  {
    free(sessions[number].copyright);
    sessions[number].copyright=(char *)NULL;
  }

  if(sessions[number].filename!=(char *)NULL)
  {
    free(sessions[number].filename);
    sessions[number].filename=(char *)NULL;
  }

  if(sessions[number].dirname!=(char *)NULL)
  {
    free(sessions[number].dirname);
    sessions[number].dirname=(char *)NULL;
  }

  if(sessions[number].current_dir!=(char *)NULL)
  {
    free(sessions[number].current_dir);
    sessions[number].current_dir=(char *)NULL;
  }

  sessions[number].numfileinfos=0;
  sessions[number].fileinfos=(FileInfo **)NULL;

  HS_FreeSessionData(number);


  win_Unmap(tk_display,sessions[number].win_main);
  win_Unmap(tk_display,sessions[number].win_topics);
  win_Unmap(tk_display,sessions[number].win_open);
  win_Unmap(tk_display,sessions[number].win_glossary);
  win_Unmap(tk_display,sessions[number].win_about);
  win_Unmap(tk_display,sessions[number].win_error);
  win_Unmap(tk_display,sessions[number].win_warning);


  XMoveResizeWindow(tk_display->display,sessions[number].win_main,0,0,500,450);
  XMoveWindow(tk_display->display,sessions[number].win_main,0,0);
  XMoveResizeWindow(tk_display->display,sessions[number].win_topics,400,140,280,220);
  XMoveResizeWindow(tk_display->display,sessions[number].win_glossary,20,370,280,220);
  XMoveResizeWindow(tk_display->display,sessions[number].win_about,(DisplayWidth(tk_display->display,tk_display->screen)-400)/2,(DisplayHeight(tk_display->display,tk_display->screen)-260)/2,400,260);
  XMoveResizeWindow(tk_display->display,sessions[number].win_open,100,200,500,340);
  XMoveResizeWindow(tk_display->display,sessions[number].win_error,(DisplayWidth(tk_display->display,tk_display->screen)-380)/2,(DisplayHeight(tk_display->display,tk_display->screen)-240)/2,380,240);
  XMoveResizeWindow(tk_display->display,sessions[number].win_warning,(DisplayWidth(tk_display->display,tk_display->screen)-380)/2,(DisplayHeight(tk_display->display,tk_display->screen)-240)/2,380,240);


  HS_ResizeMain(number,500,450);
  HS_ResizeTopics(number,280,220);
  HS_ResizeGlossary(number,280,220);
  
  /*if(sessions[number].owner>0)
  {
    fprintf(stderr," OWNER  ");
    HS_FreeSession(number);
    HS_InitSession(number);
    numsessions--;
    return 0;
  }*/

  sessions[number].current_topic=-1;
  sessions[number].current_item=0;
  sessions[number].current_line=0;
  sessions[number].numlines=0;
  sessions[number].scroll_lines=0;
  sessions[number].maxlines=0;
  sessions[number].lines=(Lines *)NULL;
  sessions[number].inc=0;
  sessions[number].position=0;
  sessions[number].isUsed=False;
  sessions[number].showAllTopics=False;
  sessions[number].action=NoAction;
  sessions[number].owner=0;
  numsessions--;

   return 0;
}




int HS_UseSession(number)
unsigned int number;
{
  if(sessions[number].isUsed==False)
  {
    numsessions++;
    sessions[number].current_dir=(char *)dir_GetCurrent();
  }
  sessions[number].isUsed=True;
  return 0;
}





int HS_GetUnusedSession()
{
  int i,ret;

  ret=0;
  if(numsessions>=maxsessions)
    ret=HS_AddSessions(2);
  if(ret<0) return -2;

  for(i=0;i<maxsessions;i++)
  if(sessions[i].isUsed==False) return i;
  return -1;
}




int HS_FreeSessionData(number)
unsigned int number;
{
  int i, j;
  int ret;
  WidgetAttributes wid_attributes;

  /*fprintf(stderr," Free Session DATA  ");*/


  if(sessions[number].numtopics>0)
    HS_FreeTopics(number);

  if(sessions[number].numresources>0)
    HS_FreeResources(number);
  if(sessions[number].numlines>0)
    HS_FreeLines(number);

  sessions[number].numresources=0;
  sessions[number].loadedresources=0;
  sessions[number].maxresources=0;
  sessions[number].numlines=0;
  sessions[number].maxlines=0;
  sessions[number].numtopics=0;
  sessions[number].maxtopics=0;

  sessions[number].resources=(HelpResource *)NULL;
  sessions[number].topics=(HelpTopic *)NULL;
  sessions[number].lines=(Lines *)NULL;


  item_DeleteAll(tk_display,sessions[number].ls_topics);
  item_DeleteAll(tk_display,sessions[number].ls_glossary);
  XClearWindow(tk_display->display,sessions[number].win_draw);
  wid_SetState(tk_display,sessions[number].bn_index,Grayed);
  wid_SetState(tk_display,sessions[number].bn_previous,Grayed);
  wid_SetState(tk_display,sessions[number].bn_next,Grayed);
  wid_SetState(tk_display,sessions[number].bn_seek,Grayed);
  wid_SetState(tk_display,sessions[number].bn_print,Grayed);
  wid_SetState(tk_display,sessions[number].bn_history,Grayed);
  wid_SetState(tk_display,sessions[number].sb_main,Grayed);
  wid_SetPosition(tk_display,sessions[number].sb_main,0);
  wid_attributes.range=0;
  wid_attributes.mask=SARange;
  wid_SetAttributes(tk_display,sessions[number].sb_main,&wid_attributes,False);

  sessions[number].current_topic=-1;
  sessions[number].current_item=0;
  sessions[number].current_line=0;
  sessions[number].numlines=0;
  sessions[number].scroll_lines=0;
  sessions[number].maxlines=0;
  sessions[number].lines=(Lines *)NULL;
  sessions[number].inc=0;
  sessions[number].position=0;
  sessions[number].action=NoAction;

  return 0;
}



int HS_FreeSession(number)
int number;
{
  int i;

  if(number>=0 && number<maxsessions)
  { 
    i=number;

    win_Unmap(tk_display,sessions[i].win_main);
    XSync(tk_display->display,True);
    wid_Destroy(tk_display,sessions[i].sb_main);
    wid_Destroy(tk_display,sessions[i].ls_glossary);
    wid_Destroy(tk_display,sessions[i].ls_topics);
    wid_Destroy(tk_display,sessions[i].ls_open_file);
    wid_Destroy(tk_display,sessions[i].ls_open_dir);
    wid_Destroy(tk_display,sessions[i].ed_open);

    wid_Destroy(tk_display,sessions[i].bn_error_ok);
    wid_Destroy(tk_display,sessions[i].bn_warning_ok);
    wid_Destroy(tk_display,sessions[i].bn_open_ok);
    wid_Destroy(tk_display,sessions[i].bn_open_cancel);
    wid_Destroy(tk_display,sessions[i].bn_index);
    wid_Destroy(tk_display,sessions[i].bn_previous);
    wid_Destroy(tk_display,sessions[i].bn_next);
    wid_Destroy(tk_display,sessions[i].bn_seek);
    wid_Destroy(tk_display,sessions[i].bn_print);
    wid_Destroy(tk_display,sessions[i].bn_history);
    wid_Destroy(tk_display,sessions[i].bn_about_ok);
    wid_Destroy(tk_display,sessions[i].mn_file);
    wid_Destroy(tk_display,sessions[i].mn_options);
    wid_Destroy(tk_display,sessions[i].mn_help);
    wid_Destroy(tk_display,sessions[i].mn_main);

    win_Destroy(tk_display,sessions[i].win_draw);
    win_Destroy(tk_display,sessions[i].win_main);
    win_Destroy(tk_display,sessions[i].win_topics);
    win_Destroy(tk_display,sessions[i].win_glossary);
    win_Destroy(tk_display,sessions[i].win_about);
    win_Destroy(tk_display,sessions[i].win_error);
    win_Destroy(tk_display,sessions[i].win_warning);
    win_Destroy(tk_display,sessions[i].win_open);

    return 0;
  }
  return -1;
}



int HS_FreeSessions()
{
  int i;

  if(maxsessions>0)
  for(i=0;i<maxsessions;i++)
  { 
    if(sessions[i].file!=-1)
      close(sessions[i].file);

    if(sessions[i].filename!=(char *)NULL)
      free(sessions[i].filename);
  
    if(sessions[i].dirname!=(char *)NULL)
      free(sessions[i].dirname);
  
    if(sessions[i].current_dir!=(char *)NULL)
      free(sessions[i].current_dir);
  
    HS_FreeSessionData(i);

    win_Unmap(tk_display,sessions[i].win_main);
    XSync(tk_display->display,True);
    wid_Destroy(tk_display,sessions[i].sb_main);
    wid_Destroy(tk_display,sessions[i].ls_glossary);
    wid_Destroy(tk_display,sessions[i].ls_topics);
    wid_Destroy(tk_display,sessions[i].ls_open_file);
    wid_Destroy(tk_display,sessions[i].ls_open_dir);
    wid_Destroy(tk_display,sessions[i].ed_open);

    wid_Destroy(tk_display,sessions[i].bn_error_ok);
    wid_Destroy(tk_display,sessions[i].bn_warning_ok);
    wid_Destroy(tk_display,sessions[i].bn_open_ok);
    wid_Destroy(tk_display,sessions[i].bn_open_cancel);
    wid_Destroy(tk_display,sessions[i].bn_index);
    wid_Destroy(tk_display,sessions[i].bn_previous);
    wid_Destroy(tk_display,sessions[i].bn_next);
    wid_Destroy(tk_display,sessions[i].bn_seek);
    wid_Destroy(tk_display,sessions[i].bn_print);
    wid_Destroy(tk_display,sessions[i].bn_history);
    wid_Destroy(tk_display,sessions[i].bn_about_ok);
    wid_Destroy(tk_display,sessions[i].mn_file);
    wid_Destroy(tk_display,sessions[i].mn_options);
    wid_Destroy(tk_display,sessions[i].mn_help);
    wid_Destroy(tk_display,sessions[i].mn_main);

    win_Destroy(tk_display,sessions[i].win_draw);
    win_Destroy(tk_display,sessions[i].win_main);
    win_Destroy(tk_display,sessions[i].win_topics);
    win_Destroy(tk_display,sessions[i].win_glossary);
    win_Destroy(tk_display,sessions[i].win_about);
    win_Destroy(tk_display,sessions[i].win_error);
    win_Destroy(tk_display,sessions[i].win_warning);
    win_Destroy(tk_display,sessions[i].win_open);

    return 0;
  }
  if(maxsessions>0)
    free((char *)sessions);
  return -1;
}



void HS_Exit()
{ 
  HS_FreeSessions();
  free((char *)sessions);

  XFreePixmap(tk_display->display,bm_bug);
  XFreePixmap(tk_display->display,bm_question1);
  XFreePixmap(tk_display->display,bm_question2);
  XFreePixmap(tk_display->display,bm_warning1);
  XFreePixmap(tk_display->display,bm_warning2);
  XFreePixmap(tk_display->display,bm_warning3);
  XFreePixmap(tk_display->display,bm_stop1);
  XFreePixmap(tk_display->display,bm_stop2);
  XFreePixmap(tk_display->display,bm_stack);
  XFreePixmap(tk_display->display,bm_dir);
  XFreePixmap(tk_display->display,bm_none);
  XFreePixmap(tk_display->display,bm_help1);
  XFreePixmap(tk_display->display,bm_help2);
  XFreePixmap(tk_display->display,xpm_index);
  XFreePixmap(tk_display->display,xpm_next);
  XFreePixmap(tk_display->display,xpm_previous);
  XFreePixmap(tk_display->display,xpm_index_mask);
  XFreePixmap(tk_display->display,xpm_previous_mask);
  XFreePixmap(tk_display->display,xpm_next_mask);
  XFreePixmap(tk_display->display,xpm_find);
  XFreePixmap(tk_display->display,xpm_printer);
  XFreePixmap(tk_display->display,xpm_glossary);
  XFreePixmap(tk_display->display,xpm_find_mask);
  XFreePixmap(tk_display->display,xpm_printer_mask);
  XFreePixmap(tk_display->display,xpm_glossary_mask);

  XFreeGC(tk_display->display,gc);
  tk_CloseSession(tk_display);
  exit(0);
}



#endif

