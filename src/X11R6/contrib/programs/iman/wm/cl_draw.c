/*
 *
 * 	cl_draw.c
 * 	affichage des fenetres de couleur
 *
 * 	Modification :  27/03/94
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





#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>



#include "bm/warning.bm"
#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"





int CL_DrawDesktop(item)
int item;
{
 char *text;
 int w;


 if(item==-1||item==1)
 {
   XSetWindowBackground(tk_display->display,color_desktop,cl_manager.desk_bg);
   XSetWindowBackground(tk_display->display,color_icon,cl_manager.desk_bg);
   XSetWindowBackground(tk_display->display,color_window,cl_manager.desk_bg);
   XSetWindowBackground(tk_display->display,color_dialog,cl_manager.desk_bg);
   XSetWindowBackground(tk_display->display,color_menu,cl_manager.desk_bg);
   XSetForeground(tk_display->display,colors_gc,cl_manager.desk_bg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.desk_bg);
   XFillRectangle(tk_display->display,color_desktop,colors_gc,0,0,300,170);   
 }
 if(item==-1||item==0||item==1)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.desk_fg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.desk_fg);
   XSetFont(tk_display->display,colors_gc,wm_fonts.times_big->fid);
   text="Foreground";
   w=strlen(text);
   XDrawString(tk_display->display,color_desktop,colors_gc,(246-XTextWidth(wm_fonts.times_big,text,w))/2,85,text,w);
   /*XFillRectangle(tk_display->display,color_desktop,colors_gc,0,0,124,170);*/
 }

 return 0;

}




int CL_DrawWindow(item)
int item;
{


 if(item==-1||item==0)	/*** BG ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.win_colors.bg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.win_colors.bg);

   XSetWindowBackground(tk_display->display,color_button,cl_manager.win_colors.bg);
   XSetWindowBackground(tk_display->display,color_scroll,cl_manager.win_colors.bg);
   XSetWindowBackground(tk_display->display,color_list,cl_manager.win_colors.bg);
   XSetWindowBackground(tk_display->display,color_edit,cl_manager.win_colors.bg);

   XFillRectangle(tk_display->display,color_window,colors_gc,131,68,99,87); 
   XFillRectangle(tk_display->display,color_window,colors_gc,5,27,111,123);      

   XFillRectangle(tk_display->display,color_window,colors_gc,127,42,12,2);   
   XFillRectangle(tk_display->display,color_window,colors_gc,127,42,2,12);   
   XFillRectangle(tk_display->display,color_window,colors_gc,127,57,2,87);   
   XFillRectangle(tk_display->display,color_window,colors_gc,127,147,2,12);   
   XFillRectangle(tk_display->display,color_window,colors_gc,127,157,12,2);   
   XFillRectangle(tk_display->display,color_window,colors_gc,142,157,77,2);   
   XFillRectangle(tk_display->display,color_window,colors_gc,222,157,12,2);   
   XFillRectangle(tk_display->display,color_window,colors_gc,142,42,77,2);   
   XFillRectangle(tk_display->display,color_window,colors_gc,222,42,12,2);   
   XFillRectangle(tk_display->display,color_window,colors_gc,232,42,2,12);   
   XFillRectangle(tk_display->display,color_window,colors_gc,232,57,2,87);   
   XFillRectangle(tk_display->display,color_window,colors_gc,232,147,2,12);   
 }
 if(item==-1||item==0 || item==1)	/*** WINDOW LIGHT ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.win_colors.light);
   XSetBackground(tk_display->display,colors_gc,cl_manager.win_colors.light);

   XDrawLine(tk_display->display,color_window,colors_gc,126,41,139,41);  
   XDrawLine(tk_display->display,color_window,colors_gc,126,41,126,54);  
   XDrawLine(tk_display->display,color_window,colors_gc,126,56,129,56);
   XDrawLine(tk_display->display,color_window,colors_gc,126,56,126,144);
   XDrawLine(tk_display->display,color_window,colors_gc,126,146,129,146);
   XDrawLine(tk_display->display,color_window,colors_gc,126,146,126,159);
   XDrawLine(tk_display->display,color_window,colors_gc,130,156,139,156);
   XDrawLine(tk_display->display,color_window,colors_gc,129,45,129,156);

   XDrawLine(tk_display->display,color_window,colors_gc,141,41,219,41);  
   XDrawLine(tk_display->display,color_window,colors_gc,141,41,141,44);  
   XDrawLine(tk_display->display,color_window,colors_gc,221,41,234,41);  
   XDrawLine(tk_display->display,color_window,colors_gc,221,41,221,44);  
   XDrawLine(tk_display->display,color_window,colors_gc,231,45,231,54);  

   XDrawLine(tk_display->display,color_window,colors_gc,231,56,231,56);  
   XDrawLine(tk_display->display,color_window,colors_gc,231,56,231,144);  

   XDrawLine(tk_display->display,color_window,colors_gc,231,146,234,146);  
   XDrawLine(tk_display->display,color_window,colors_gc,231,146,231,156);  
   XDrawLine(tk_display->display,color_window,colors_gc,221,156,234,156);  
   XDrawLine(tk_display->display,color_window,colors_gc,221,156,221,159);  

   XDrawLine(tk_display->display,color_window,colors_gc,141,156,141,159);  
   XDrawLine(tk_display->display,color_window,colors_gc,141,156,219,156);  

   XFillRectangle(tk_display->display,color_window,colors_gc,15,130,90,2);  
   XFillRectangle(tk_display->display,color_window,colors_gc,105,52,2,79);  
 }

 if(item==-1||item==0||item==2)		/*** WINDOW SHADOW ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.win_colors.shadow);
   XSetBackground(tk_display->display,colors_gc,cl_manager.win_colors.shadow);

   XDrawLine(tk_display->display,color_window,colors_gc,139,42,139,44);  
   XDrawLine(tk_display->display,color_window,colors_gc,129,44,139,44);  
   XDrawLine(tk_display->display,color_window,colors_gc,129,44,139,44);  
   XDrawLine(tk_display->display,color_window,colors_gc,129,44,139,54);
   XDrawLine(tk_display->display,color_window,colors_gc,127,54,139,54);

   XDrawLine(tk_display->display,color_window,colors_gc,129,57,129,144);
   XDrawLine(tk_display->display,color_window,colors_gc,127,144,129,144);
   XDrawLine(tk_display->display,color_window,colors_gc,129,147,129,156);
   XDrawLine(tk_display->display,color_window,colors_gc,127,159,139,159);
   XDrawLine(tk_display->display,color_window,colors_gc,139,157,139,159);

   XDrawLine(tk_display->display,color_window,colors_gc,142,44,219,44);  
   XDrawLine(tk_display->display,color_window,colors_gc,219,42,219,44);  
   XDrawLine(tk_display->display,color_window,colors_gc,222,44,231,44);  
   XDrawLine(tk_display->display,color_window,colors_gc,232,54,234,54);  
   XDrawLine(tk_display->display,color_window,colors_gc,234,42,234,54);  

   XDrawLine(tk_display->display,color_window,colors_gc,142,159,219,159);  
   XDrawLine(tk_display->display,color_window,colors_gc,219,157,219,159);  

   XDrawLine(tk_display->display,color_window,colors_gc,234,57,234,144);  
   XDrawLine(tk_display->display,color_window,colors_gc,231,144,234,144);  

   XDrawLine(tk_display->display,color_window,colors_gc,234,147,234,159);  
   XDrawLine(tk_display->display,color_window,colors_gc,222,159,234,159);  

   XFillRectangle(tk_display->display,color_window,colors_gc,15,52,90,2);  
   XFillRectangle(tk_display->display,color_window,colors_gc,15,52,2,78);  

 }

 if(item==-1||item==5)		/*** BORDER ACTIVE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.win_colors.border_active);
   XSetBackground(tk_display->display,colors_gc,cl_manager.win_colors.border_active);
 
   XDrawRectangle(tk_display->display,color_window,colors_gc,125,40,110,120);   
   XDrawRectangle(tk_display->display,color_window,colors_gc,130,45,100,110);   
 
   XDrawLine(tk_display->display,color_window,colors_gc,125,55,130,55);
   XDrawLine(tk_display->display,color_window,colors_gc,125,145,130,145);
   XDrawLine(tk_display->display,color_window,colors_gc,140,40,140,45);
   XDrawLine(tk_display->display,color_window,colors_gc,220,40,220,45);
   XDrawLine(tk_display->display,color_window,colors_gc,230,55,235,55);
   XDrawLine(tk_display->display,color_window,colors_gc,230,145,235,145);
   XDrawLine(tk_display->display,color_window,colors_gc,140,155,140,160);
   XDrawLine(tk_display->display,color_window,colors_gc,220,155,220,160);
 }
 if(item==-1||item==6)		/*** BORDER INACTIVE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.win_colors.border_inactive);
   XSetBackground(tk_display->display,colors_gc,cl_manager.win_colors.border_inactive);
/*   XDrawLine(tk_display->display,color_window,colors_gc,0,0,120,0);   
   XDrawLine(tk_display->display,color_window,colors_gc,0,0,0,150);   
   XDrawLine(tk_display->display,color_window,colors_gc,0,150,29,150);   
   XDrawLine(tk_display->display,color_window,colors_gc,120,0,120,39);   */

   XDrawLine(tk_display->display,color_window,colors_gc,4,4,116,4);   
   XDrawLine(tk_display->display,color_window,colors_gc,4,4,4,150);   
   XDrawLine(tk_display->display,color_window,colors_gc,4,150,116,150);
   XDrawLine(tk_display->display,color_window,colors_gc,116,4,116,150);   
 }
 if(item==-1||item==7)	/*** TITLE BG ACTIVE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.win_colors.title_bg_active);
   XSetBackground(tk_display->display,colors_gc,cl_manager.win_colors.title_bg_active);
   XFillRectangle(tk_display->display,color_window,colors_gc,131,46,99,22);   
 }
 if(item==-1||item==8)	/*** TITLE BG UNACTIVE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.win_colors.title_bg_inactive);
   XSetBackground(tk_display->display,colors_gc,cl_manager.win_colors.title_bg_inactive);
   XFillRectangle(tk_display->display,color_window,colors_gc,5,5,111,22);   

   /*XFillRectangle(tk_display->display,color_window,colors_gc,130,15,116,4);   
   XFillRectangle(tk_display->display,color_window,colors_gc,130,15,4,25);   
   XFillRectangle(tk_display->display,color_window,colors_gc,242,15,4,135);   
   XFillRectangle(tk_display->display,color_window,colors_gc,141,146,134,4);   

   XFillRectangle(tk_display->display,color_window,colors_gc,135,20,106,20);   
   XFillRectangle(tk_display->display,color_window,colors_gc,141,40,100,2);   */
 }
 if(item==-1|| item==7|| item==9)	/*** TITLE TEXT ACTIVE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.win_colors.title_text_active);
   XSetBackground(tk_display->display,colors_gc,cl_manager.win_colors.title_bg_active);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawImageString(tk_display->display,color_window,colors_gc,162,61,"Focus",5);   
 }
 if(item==-1||item==8||item==10)	/*** TITLE TEXT UNACTIVE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.win_colors.title_text_inactive);
   XSetBackground(tk_display->display,colors_gc,cl_manager.win_colors.title_bg_inactive);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawImageString(tk_display->display,color_window,colors_gc,30,20,"No focus",8);    
 }
 if(item==-1||item==0||item==3)		/*** WINDOW TEXT ACTIVE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.win_colors.text);
   XSetBackground(tk_display->display,colors_gc,cl_manager.win_colors.text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.helvetica12->fid);
   XDrawString(tk_display->display,color_window,colors_gc,161,103,"Text",4);
 }
 if(item==-1||item==0||item==4)		/*** WINDOW TEXT GRISE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.win_colors.text_grayed);
   XSetBackground(tk_display->display,colors_gc,cl_manager.win_colors.text_grayed);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.helvetica12->fid);
   XDrawString(tk_display->display,color_window,colors_gc,141,125,"Text grayed",11);
 }

 return 0;
}




int CL_DrawDialog(item)
int item;
{

 if(item==-1||item==0)			    /*** BG ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.dlg_colors.bg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.dlg_colors.bg);
   XFillRectangle(tk_display->display,color_dialog,colors_gc,5,27,111,123);   
   XFillRectangle(tk_display->display,color_dialog,colors_gc,124,52,108,104);     
   XDrawRectangle(tk_display->display,color_dialog,colors_gc,124,29,107,24);
 }
 if(item==-1||item==0 || item==1)	/*** WINDOW LIGHT ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.dlg_colors.light);
   XSetBackground(tk_display->display,colors_gc,cl_manager.dlg_colors.light);
   XFillRectangle(tk_display->display,color_dialog,colors_gc,15,130,90,2);  
   XFillRectangle(tk_display->display,color_dialog,colors_gc,105,52,2,79);  
 }

 if(item==-1||item==0||item==2)		/*** WINDOW SHADOW ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.dlg_colors.shadow);
   XSetBackground(tk_display->display,colors_gc,cl_manager.dlg_colors.shadow);
   XFillRectangle(tk_display->display,color_dialog,colors_gc,15,52,90,2);  
   XFillRectangle(tk_display->display,color_dialog,colors_gc,15,52,2,78);  
 }
 if(item==-1||item==0||item==3)		/*** DIALOG TEXT ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.dlg_colors.text);
   XSetBackground(tk_display->display,colors_gc,cl_manager.dlg_colors.text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.helvetica12->fid);
   XDrawString(tk_display->display,color_dialog,colors_gc,163,100,"Text",4);
 }
 if(item==-1||item==0||item==4)		/*** DIALOG TEXT GRISE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.dlg_colors.text_grayed);
   XSetBackground(tk_display->display,colors_gc,cl_manager.dlg_colors.text_grayed);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.helvetica12->fid);
   XDrawString(tk_display->display,color_dialog,colors_gc,143,123,"Text grayed",11);
 }
 if(item==-1 || item==7)		/*** TITLE BG ACTIVE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.dlg_colors.title_bg_active);
   XSetBackground(tk_display->display,colors_gc,cl_manager.dlg_colors.title_bg_active);

   XFillRectangle(tk_display->display,color_dialog,colors_gc,120,25,116,4);   
   XFillRectangle(tk_display->display,color_dialog,colors_gc,120,25,4,135);   
   XFillRectangle(tk_display->display,color_dialog,colors_gc,232,25,4,135);   
   XFillRectangle(tk_display->display,color_dialog,colors_gc,120,156,116,4);   

   XFillRectangle(tk_display->display,color_dialog,colors_gc,125,30,106,22);      
 }
 if(item==-1||item==8)			/*** TITLE BG UNACTIVE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.dlg_colors.title_bg_inactive);
   XSetBackground(tk_display->display,colors_gc,cl_manager.dlg_colors.title_bg_inactive);
   XFillRectangle(tk_display->display,color_dialog,colors_gc,5,5,111,22);   
   XDrawRectangle(tk_display->display,color_dialog,colors_gc,4,4,112,146);
 }
 if(item==-1||item==7||item==9)		/*** TITLE TEXT ACTIVE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.dlg_colors.title_text_active);
   XSetBackground(tk_display->display,colors_gc,cl_manager.dlg_colors.title_bg_active);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_dialog,colors_gc,156,45,"Focus",5);   
 }
 if(item==-1||item==8||item==10)	/*** TITLE TEXT UNACTIVE ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.dlg_colors.title_text_inactive);
   XSetBackground(tk_display->display,colors_gc,cl_manager.dlg_colors.title_bg_inactive);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_dialog,colors_gc,30,20,"No focus",8);   
 }


 return 0;
}





int CL_DrawIcon(item)
int item;
{
 int w;
 char *text;


 if(item==-1||item==0)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.icn_colors.icn_bg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.icn_colors.icn_bg);
   XFillRectangle(tk_display->display,color_icon,colors_gc,60,41,127,4);
   XFillRectangle(tk_display->display,color_icon,colors_gc,60,41,13,108);
   XFillRectangle(tk_display->display,color_icon,colors_gc,174,41,13,108);
   XFillRectangle(tk_display->display,color_icon,colors_gc,60,145,127,4);
 }
 if(item==-1||item==1)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.icn_colors.icn_light);
   XSetBackground(tk_display->display,colors_gc,cl_manager.icn_colors.icn_light);

   XDrawLine(tk_display->display,color_icon,colors_gc,58,20,188,20);
   XDrawLine(tk_display->display,color_icon,colors_gc,58,21,187,21);
   XDrawLine(tk_display->display,color_icon,colors_gc,58,20,58,150);
   XDrawLine(tk_display->display,color_icon,colors_gc,59,20,59,149);
 }
 if(item==-1||item==2)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.icn_colors.icn_shadow);
   XSetBackground(tk_display->display,colors_gc,cl_manager.icn_colors.icn_shadow);

   XDrawLine(tk_display->display,color_icon,colors_gc,187,22,187,150);
   XDrawLine(tk_display->display,color_icon,colors_gc,188,21,188,150);
   XDrawLine(tk_display->display,color_icon,colors_gc,60,149,188,149);
   XDrawLine(tk_display->display,color_icon,colors_gc,59,150,188,150);
 }
 if(item==-1||item==4)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.icn_colors.icn_draw_bg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.icn_colors.icn_draw_bg);
   XFillRectangle(tk_display->display,color_icon,colors_gc,73,45,101,100);
 }
 if(item==-1||item==3||item==4)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.icn_colors.icn_draw);
   XSetBackground(tk_display->display,colors_gc,cl_manager.icn_colors.icn_draw_bg);
   XCopyPlane(tk_display->display,pix_motifs[11],color_icon,colors_gc,0,0,64,64,91,68,1);
 }
 if(item==-1||item==5)
 { 
   XSetForeground(tk_display->display,colors_gc,cl_manager.icn_colors.title_bg_active);
   XSetBackground(tk_display->display,colors_gc,cl_manager.icn_colors.title_bg_active);
   XFillRectangle(tk_display->display,color_icon,colors_gc,60,22,127,19);   
 }
 if(item==-1||item==5||item==6)
 { 
   XSetForeground(tk_display->display,colors_gc,cl_manager.icn_colors.title_text_active);
   XSetBackground(tk_display->display,colors_gc,cl_manager.icn_colors.title_text_active);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.helvetica12->fid);
   text="Title";
   w=strlen(text);
   XDrawString(tk_display->display,color_icon,colors_gc,60+(130-XTextWidth(tk_display->fonts.helvetica12,text,w))/2,36,text,w);      
 }
  return 0;
}





int CL_DrawButton(item)
int item;
{
  char *text;
  int w;

  if(item==-1||item==0)		/*** BG ***/
  {
    XSetClipMask(tk_display->display,colors_gc,None);
    XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.bg);
    XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.bg);
    XFillRectangle(tk_display->display,color_button,colors_gc,12,12,87,57);
  }
  if(item==-1||item==2)		/*** LIGHT ***/
  {
    XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.light);
    XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.light);
    XFillRectangle(tk_display->display,color_button,colors_gc,117,12,118,88);   
    XDrawLine(tk_display->display,color_button,colors_gc,10,10,10,70);       
    XDrawLine(tk_display->display,color_button,colors_gc,11,10,11,69);       
    XDrawLine(tk_display->display,color_button,colors_gc,10,10,100,10);       
    XDrawLine(tk_display->display,color_button,colors_gc,10,11,99,11);       
    XFillRectangle(tk_display->display,color_button,colors_gc,11,108+1,CROSSSIZE-1,CROSSSIZE-1); 
  }
  if(item==-1||item==1||item==3)		/*** SHADOW ***/
  {
    XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.shadow);
    XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.shadow);
    XFillRectangle(tk_display->display,color_button,colors_gc,115,10,120,2);   
    XFillRectangle(tk_display->display,color_button,colors_gc,115,10,2,90);   
    XDrawLine(tk_display->display,color_button,colors_gc,12,69,100,69);       
    XDrawLine(tk_display->display,color_button,colors_gc,11,70,100,70);       
    XDrawLine(tk_display->display,color_button,colors_gc,99,12,99,70);       
    XDrawLine(tk_display->display,color_button,colors_gc,100,11,100,70);       

    XDrawLine(tk_display->display,color_button,colors_gc,11,108+1,11+CROSSSIZE-1,108+1);
    XDrawLine(tk_display->display,color_button,colors_gc,11,108+2,11+CROSSSIZE-1,108+2);
    XDrawLine(tk_display->display,color_button,colors_gc,12,108+1,12,108+CROSSSIZE-1); 
    XDrawLine(tk_display->display,color_button,colors_gc,11,108+1,11,108+CROSSSIZE-1); 

    XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.shadow);
    XSetClipMask(tk_display->display,colors_gc,tk_display->pixmaps.radio1);
    XSetClipOrigin(tk_display->display,colors_gc,9,140-1);
    XCopyPlane(tk_display->display,tk_display->pixmaps.radio1,color_button,colors_gc,1,1,RADIOSIZE,RADIOSIZE,10,140,1);
    XSetClipMask(tk_display->display,colors_gc,None);
  }

  if(item==-1||item==2 ||item==1 ||item==4)	/*** TEXT ***/
  {
    XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.text);
    XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.text);
    /*XDrawRectangle(tk_display->display,color_button,colors_gc,9,9,92,62);*/
    XSetFont(tk_display->display,colors_gc,tk_display->fonts.helvetica12->fid);
    text="Normal";
    w=strlen(text);
    XDrawString(tk_display->display,color_button,colors_gc,117+(118-XTextWidth(tk_display->fonts.helvetica12,text,w))/2,59,text,w);
    text="Cross";
    w=strlen(text);
    XDrawString(tk_display->display,color_button,colors_gc,22+CROSSSIZE,122,text,w);
    text="Radio";
    w=strlen(text);
    XDrawString(tk_display->display,color_button,colors_gc,20+RADIOSIZE,154,text,w);
  }
  if(item==-1||item==0||item==5)	/*** TEXT GRISE ***/
  {
    XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.text_grayed);
    XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.text_grayed);
    XSetFont(tk_display->display,colors_gc,tk_display->fonts.helvetica12->fid);
    text="Grayed";
    w=strlen(text);
    XDrawString(tk_display->display,color_button,colors_gc,12+(86-XTextWidth(tk_display->fonts.helvetica12,text,w))/2,44,text,w);      
  }
  if(item==-1||item==13)		/*** FOCUS ***/
  {
    XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.focus);
    XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.focus);
    XDrawRectangle(tk_display->display,color_button,colors_gc,114,9,121,91);   
    XDrawRectangle(tk_display->display,color_button,colors_gc,10,108,CROSSSIZE,CROSSSIZE);    

    XDrawRectangle(tk_display->display,color_button,colors_gc,19+CROSSSIZE,106,XTextWidth(tk_display->fonts.helvetica12,"Cross",5)+5,CROSSSIZE+5);    
  } 
  if(item==-1||item==2 ||item==1 ||item==14)	/*** NOFOCUS ***/
  {
    XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.nofocus);
    XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.nofocus);
    XDrawRectangle(tk_display->display,color_button,colors_gc,9,9,92,62);
  }


  if(item==-1||item==1||item==15)	/*** CROSS ***/
  {
    XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.cross);
    XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.cross);

    XDrawLine(tk_display->display,color_button,colors_gc,16,108+6,10+CROSSSIZE-3,108+CROSSSIZE-3); 
    XDrawLine(tk_display->display,color_button,colors_gc,17,108+6,10+CROSSSIZE-3,108+CROSSSIZE-4); 
    XDrawLine(tk_display->display,color_button,colors_gc,16,108+7,10+CROSSSIZE-4,108+CROSSSIZE-3); 
    XDrawLine(tk_display->display,color_button,colors_gc,16,108+CROSSSIZE-3,10+CROSSSIZE-3,108+6); 
    XDrawLine(tk_display->display,color_button,colors_gc,16,108+CROSSSIZE-4,10+CROSSSIZE-4,108+6); 
    XDrawLine(tk_display->display,color_button,colors_gc,17,108+CROSSSIZE-3,10+CROSSSIZE-3,108+7);    
  }
  if(item==-1||item==17)
  {
    XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.radio_bg);
    XSetClipMask(tk_display->display,colors_gc,tk_display->pixmaps.radio3);
    XSetClipOrigin(tk_display->display,colors_gc,9,140-1);
    XCopyPlane(tk_display->display,tk_display->pixmaps.radio3,color_button,colors_gc,1,1,RADIOSIZE,RADIOSIZE,10,140,1);
    XSetClipMask(tk_display->display,colors_gc,None);
  }  
  if(item==-1||item==18)
  {
    XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.radio_light);
    XSetClipMask(tk_display->display,colors_gc,tk_display->pixmaps.radio2);
    XSetClipOrigin(tk_display->display,colors_gc,9,140-1);
    XCopyPlane(tk_display->display,tk_display->pixmaps.radio2,color_button,colors_gc,1,1,RADIOSIZE,RADIOSIZE,10,140,1);
    XSetClipMask(tk_display->display,colors_gc,None);
  }

  return 0;
}





int CL_DrawEdit(item)
int item;
{
 char *text;
 int w;

 if(item==-1||item==0)		/** BG **/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ed_colors.bg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ed_colors.bg);
   XFillRectangle(tk_display->display,color_edit,colors_gc,20,20,206,50);
   XFillRectangle(tk_display->display,color_edit,colors_gc,20,100,206,50);
 }
 if(item==-1||item==0||item==4)		/** TEXT **/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ed_colors.text);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ed_colors.text);
   text="Selec";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,wm_fonts.times_big->fid);   
   XDrawString(tk_display->display,color_edit,colors_gc,27,55,text,w);      
 }
 if(item==-1||item==0||item==6||item==8)	/** TEXT SELECTED **/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ed_colors.text_selected);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ed_colors.selected);
   text="ted";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,wm_fonts.times_big->fid);   
   XDrawImageString(tk_display->display,color_edit,colors_gc,27+XTextWidth(wm_fonts.times_big,"Selec",5),55,text,w);
 }
 if(item==-1||item==0||item==5)		/** TEXT GRAYED **/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ed_colors.text_grayed);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ed_colors.bg);
   text="Grayed";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,wm_fonts.times_big->fid);   
   XDrawString(tk_display->display,color_edit,colors_gc,27,135,text,w);      
 }
 if(item==-1||item==0||item==1||item==12)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ed_colors.cursor);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ed_colors.cursor);
   XFillRectangle(tk_display->display,color_edit,colors_gc,25+XTextWidth(wm_fonts.times_big,"S",1),27,2,36);
 }
 if(item==-1||item==0||item==13)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ed_colors.focus);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ed_colors.focus);
   XDrawRectangle(tk_display->display,color_edit,colors_gc,19,19,208,52);
 }
 if(item==-1||item==0||item==14)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ed_colors.nofocus);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ed_colors.nofocus);
   XDrawRectangle(tk_display->display,color_edit,colors_gc,19,99,208,52);
 }
 return 0;
}




int CL_DrawScroll(item)
int item;
{
 if(item==-1)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.text);
   XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.text);
   XDrawRectangle(tk_display->display,color_scroll,colors_gc,20,30,206,110);
   XDrawRectangle(tk_display->display,color_scroll,colors_gc,50,31,60,108);

   XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.bg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.bg);
   XFillRectangle(tk_display->display,color_scroll,colors_gc,53,34,56,104);

   XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.light);
   XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.light);
   XFillRectangle(tk_display->display,color_scroll,colors_gc,51,32,58,2);
   XFillRectangle(tk_display->display,color_scroll,colors_gc,51,32,2,106);

   XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.shadow);
   XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.shadow);
   XDrawLine(tk_display->display,color_scroll,colors_gc,53,137,109,137);
   XDrawLine(tk_display->display,color_scroll,colors_gc,52,138,109,138);
   XDrawLine(tk_display->display,color_scroll,colors_gc,108,34,108,138);
   XDrawLine(tk_display->display,color_scroll,colors_gc,109,33,109,138);
   XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.bg);
   XCopyPlane(tk_display->display,tk_display->pixmaps.thumb1,color_scroll,colors_gc,0,0,11,12,74,79,1);

   XSetForeground(tk_display->display,colors_gc,cl_manager.bn_colors.light);
   XSetBackground(tk_display->display,colors_gc,cl_manager.bn_colors.bg);
   XSetClipMask(tk_display->display,colors_gc,tk_display->pixmaps.thumb2);
   XSetClipOrigin(tk_display->display,colors_gc,74,79);
   XCopyPlane(tk_display->display,tk_display->pixmaps.thumb2,color_scroll,colors_gc,0,0,11,12,74,79,1);
   XSetClipMask(tk_display->display,colors_gc,None);
 }
 if(item==-1||item==0)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.sb_colors.bg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.sb_colors.bg);
   XFillRectangle(tk_display->display,color_scroll,colors_gc,21,31,29,109);
   XFillRectangle(tk_display->display,color_scroll,colors_gc,111,31,115,109);
 }
 if(item==-1||item==0||item==2)
 {
   /*XSetFunction(tk_display->display,colors_gc,GXcopy);
   XSetForeground(tk_display->display,colors_gc,cl_manager.sb_colors.bg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.sb_colors.bg);
   XDrawRectangle(tk_display->display,color_scroll,colors_gc,150,31,60,108);
   XSetFunction(tk_display->display,colors_gc,GXxor);*/
   XSetForeground(tk_display->display,colors_gc,cl_manager.sb_colors.light);
   XSetBackground(tk_display->display,colors_gc,cl_manager.sb_colors.light);
   XDrawRectangle(tk_display->display,color_scroll,colors_gc,150,31,60,108);
   XSetFunction(tk_display->display,colors_gc,GXcopy);
 }

 return 0;
}





int CL_DrawList(item)
int item;
{
 char *text;
 int w, i;

 if(item==-1||item==0)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ls_colors.bg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ls_colors.bg);
   XFillRectangle(tk_display->display,color_list,colors_gc,30,20,186,130);
 }
 if(item==-1||item==0||item==4)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ls_colors.text);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ls_colors.bg);
   text="Normal";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_list,colors_gc,34,23+13,text,w);
   XDrawString(tk_display->display,color_list,colors_gc,34,23+4*16+13,text,w);
 }
 if(item==-1||item==0||item==6)	  	/*** CURSOR_BAR ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ls_colors.selected);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ls_colors.selected);
   XFillRectangle(tk_display->display,color_list,colors_gc,30,39,188,16);
   XFillRectangle(tk_display->display,color_list,colors_gc,30,39+2*16,188,16);
 }
 if(item==-1||item==0||item==6||item==8)	/*** TEXT_BAR ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ls_colors.text_selected);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ls_colors.text_selected);
   text="Selected";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_list,colors_gc,34,39+12,text,w);
 }
 if(item==-1||item==0||item==5)		/*** TEXT_GRAYED ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ls_colors.text_grayed);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ls_colors.text_grayed);
   text="Grayed";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_list,colors_gc,34,55+12,text,w);
 }
 if(item==-1||item==0||item==6||item==9)	/*** TEXT_BAR_GRAYED ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ls_colors.text_grayed_selected);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ls_colors.text_grayed_selected);
   text="Grayed selected";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_list,colors_gc,34,71+12,text,w);
 }
 if(item==-1||item==0||item==7)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ls_colors.selected_inactive);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ls_colors.selected_inactive);
   XFillRectangle(tk_display->display,color_list,colors_gc,30,39+4*16,188,16);
   XFillRectangle(tk_display->display,color_list,colors_gc,30,39+5*16,188,16);
 }
 if(item==-1||item==0||item==10||item==7)	/*** TEXT_BAR_NOFOCUS ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ls_colors.text_selected_inactive);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ls_colors.text_selected_inactive);
   text="Normal no focus";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_list,colors_gc,34,71+2*16+12,text,w);
 }
 if(item==-1||item==0||item==7||item==11)	/*** TEXT_BAR_NOFOCUS ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ls_colors.text_grayed_selected_inactive);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ls_colors.text_grayed_selected_inactive);
   text="Grayed no focus";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_list,colors_gc,34,71+3*16+12,text,w);
 }
 if(item==-1||item==13)		/*** FOCUS ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.ls_colors.focus);
   XSetBackground(tk_display->display,colors_gc,cl_manager.ls_colors.focus);
   XDrawRectangle(tk_display->display,color_list,colors_gc,29,19,188,132);
 }

 return 0;
}






int CL_DrawMenu(item)
int item;
{
 char *text;
 int w, i;

 if(item==-1||item==0)		/**** BG ****/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.mn_colors.bg);
   XSetBackground(tk_display->display,colors_gc,cl_manager.mn_colors.bg);
   XFillRectangle(tk_display->display,color_menu,colors_gc,10,10,120,110);
   XFillRectangle(tk_display->display,color_menu,colors_gc,132,80,105,80);
 }
 if(item==-1||item==0||item==4)		/*** TEXT ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.mn_colors.text);
   XSetBackground(tk_display->display,colors_gc,cl_manager.mn_colors.bg);
   text="Normal";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_menu,colors_gc,23,12+13,text,w);
   XDrawString(tk_display->display,color_menu,colors_gc,23,12+2*15+13,text,w);
   XDrawString(tk_display->display,color_menu,colors_gc,23,12+4*15+13,text,w);
   XDrawString(tk_display->display,color_menu,colors_gc,23,12+6*15+13,text,w);
   XDrawString(tk_display->display,color_menu,colors_gc,145,82+13,text,w);
   XDrawString(tk_display->display,color_menu,colors_gc,145,82+4*15+13,text,w);
 }
 if(item==-1||item==0||item==6)	  	/*** CURSOR_BAR ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.mn_colors.selected);
   XSetBackground(tk_display->display,colors_gc,cl_manager.mn_colors.selected);
   XFillRectangle(tk_display->display,color_menu,colors_gc,10,28,120,15);
   XFillRectangle(tk_display->display,color_menu,colors_gc,10,28+4*15,120,15);
 }
 if(item==-1||item==0||item==6||item==8)	/*** TEXT_BAR ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.mn_colors.text_selected);
   XSetBackground(tk_display->display,colors_gc,cl_manager.mn_colors.text_selected);
   text="Selected";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_menu,colors_gc,23,27+13,text,w);
 }
 if(item==-1||item==0||item==5)		/*** TEXT_GRAYED ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.mn_colors.text_grayed);
   XSetBackground(tk_display->display,colors_gc,cl_manager.mn_colors.text_grayed);
   text="Grayed";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_menu,colors_gc,23,12+3*15+13,text,w);
   XDrawString(tk_display->display,color_menu,colors_gc,145,82+2*15+13,text,w);
 }
 if(item==-1||item==0||item==6||item==9)	/*** TEXT_BAR_GRAYED ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.mn_colors.text_grayed_selected);
   XSetBackground(tk_display->display,colors_gc,cl_manager.mn_colors.text_grayed_selected);
   text="Grayed selec.";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_menu,colors_gc,23,12+5*15+13,text,w);
 }
 if(item==-1||item==0||item==7)
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.mn_colors.selected_inactive);
   XSetBackground(tk_display->display,colors_gc,cl_manager.mn_colors.selected_inactive);
   XFillRectangle(tk_display->display,color_menu,colors_gc,132,83+15,105,15);
   XFillRectangle(tk_display->display,color_menu,colors_gc,132,83+3*15,105,15);
 }
 if(item==-1||item==0||item==7||item==10)	/*** TEXT_BAR_NOFOCUS ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.mn_colors.text_selected_inactive);
   XSetBackground(tk_display->display,colors_gc,cl_manager.mn_colors.text_selected_inactive);
   text="No focus";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_menu,colors_gc,145,82+1*15+13,text,w);
 }
 if(item==-1||item==0||item==7||item==11)	/*** TEXT_BARGRAYED_NOFOCUS ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.mn_colors.text_grayed_selected_inactive);
   XSetBackground(tk_display->display,colors_gc,cl_manager.mn_colors.text_grayed_selected_inactive);
   text="Grayed";
   w=strlen(text);
   XSetFont(tk_display->display,colors_gc,tk_display->fonts.f8_13->fid);
   XDrawString(tk_display->display,color_menu,colors_gc,145,82+3*15+13,text,w);
 }
 if(item==-1||item==13)		/*** FOCUS ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.mn_colors.focus);
   XSetBackground(tk_display->display,colors_gc,cl_manager.mn_colors.focus);
   XDrawRectangle(tk_display->display,color_menu,colors_gc,9,9,121,112);
 }
 if(item==-1||item==14)		/*** FOCUS ***/
 {
   XSetForeground(tk_display->display,colors_gc,cl_manager.mn_colors.nofocus);
   XSetBackground(tk_display->display,colors_gc,cl_manager.mn_colors.nofocus);
   XDrawRectangle(tk_display->display,color_menu,colors_gc,131,79,107,82);
 }


  return 0;
}



