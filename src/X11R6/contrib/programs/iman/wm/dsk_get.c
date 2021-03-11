/*
 *
 * 	dsk_get.c
 * 	informations sur le bureau
 *
 * 	Modification :  11/11/93
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






void DSK_GetPreferences()
{
 WidgetTextDecoration wid_text;
 WidgetAttributes wid_attributes;
 char *str_type, *sptr, *str_class;
 int i, j, k, ret;
 char text[10];



		/*** Options pour wm_info ***/

 str_class=NULL;

 j=XrmGetResource(xrdb,desktop_text[0],str_class,&str_type,&xrmvalue); 
 if(j==True)
 {
    sptr=xrmvalue.addr;
    wm_info.dsk_motif=atoi(sptr);
    if(wm_info.dsk_motif>=numpixmotifs) wm_info.dsk_motif=0;
 }
 else wm_info.dsk_motif=0;
 wid_attributes.mask=SAPosition;
 wid_attributes.position=wm_info.dsk_motif;
 ret=wid_SetAttributes(tk_display,cb_desktop_motif,&wid_attributes,False);
 DSK_SetMotif(wm_info.dsk_motif);

 j=XrmGetResource(xrdb,desktop_text[1],str_class,&str_type,&xrmvalue); 
 if(j==True)
 {
    sptr=xrmvalue.addr;
    wm_info.dsk_defaulticon=atoi(sptr);
    if(wm_info.dsk_defaulticon>1) wm_info.dsk_defaulticon=0;
 }
 else wm_info.dsk_defaulticon=0;
 wid_attributes.position=wm_info.dsk_defaulticon;
 wid_SetAttributes(tk_display,cb_desktop_defaulticon,&wid_attributes,False);
 DSK_SetDefaulticon(wm_info.dsk_defaulticon);

 j=XrmGetResource(xrdb,desktop_text[2],str_class,&str_type,&xrmvalue); 
 if(j==True)
 {
    sptr=xrmvalue.addr;
    wm_info.dsk_screensaver=atoi(sptr);
    if(wm_info.dsk_screensaver>=2) wm_info.dsk_screensaver=0;
 }
 else wm_info.dsk_screensaver=0;
 wid_attributes.position=wm_info.dsk_screensaver;
 wid_SetAttributes(tk_display,cb_desktop_screensaver,&wid_attributes,False);



 j=XrmGetResource(xrdb,desktop_text[3],str_class,&str_type,&xrmvalue); 
 if(j==True)
 {
    sptr=xrmvalue.addr;
    wm_info.dsk_screensaver_time=atoi(sptr);
    if(wm_info.dsk_screensaver_time<=0) wm_info.dsk_screensaver_time=10;
 }
 else wm_info.dsk_screensaver_time=10;
 sprintf(text,"%d",wm_info.dsk_screensaver_time);
 wid_text.mask=STText;
 wid_text.text=text;
 wid_SetTextDecoration(tk_display,ed_desktop_screensaver,&wid_text,False);

 if(wm_info.dsk_screensaver==0)
 {
  XSetScreenSaver(tk_display->display,0,0,DefaultBlanking,DefaultExposures);
  wid_SetState(tk_display,bn_desktop_screensaver_test,Grayed);
  wid_SetState(tk_display,bn_desktop_screensaver_install,Grayed);
  wid_SetState(tk_display,ed_desktop_screensaver,Grayed);
 }
 if(wm_info.dsk_screensaver==1)
 {
  XSetScreenSaver(tk_display->display,wm_info.dsk_screensaver_time*60,0,DefaultBlanking,DefaultExposures);
  wid_SetState(tk_display,bn_desktop_screensaver_test,Ungrayed);
  wid_SetState(tk_display,bn_desktop_screensaver_install,Ungrayed);
  wid_SetState(tk_display,ed_desktop_screensaver,Ungrayed);
 }


 j=XrmGetResource(xrdb,desktop_text[4],str_class,&str_type,&xrmvalue); 
 if(j==True)
 {
    sptr=xrmvalue.addr;
    wm_info.dsk_paper=atoi(sptr);
    if(wm_info.dsk_paper>=1) wm_info.dsk_paper=0;
 }
 else wm_info.dsk_paper=0;
 wid_attributes.position=wm_info.dsk_paper;
 wid_SetAttributes(tk_display,cb_desktop_paper,&wid_attributes,False);

 j=XrmGetResource(xrdb,desktop_text[5],str_class,&str_type,&xrmvalue); 
 if(j==True)
 {
    sptr=xrmvalue.addr;
    wm_info.dsk_paper_drawing=atoi(sptr);
    if(wm_info.dsk_paper_drawing>1) wm_info.dsk_paper_drawing=0;
 }
 else wm_info.dsk_paper_drawing=0;
 if(wm_info.dsk_paper_drawing==0)
 {
   wid_SetState(tk_display,bn_paper_center,Unpushed);
   wid_SetState(tk_display,bn_paper_mos,Pushed);
 }
 else if(wm_info.dsk_paper_drawing==1)
 {
   wid_SetState(tk_display,bn_paper_center,Pushed);
   wid_SetState(tk_display,bn_paper_mos,Unpushed);
 }

 if(wm_action.type==InitAction)
   DSK_SavePreferences();

}





