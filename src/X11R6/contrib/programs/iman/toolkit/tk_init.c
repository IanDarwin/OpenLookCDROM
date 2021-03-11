/*
 *
 * 	tk_init.c  
 * 	initialisation du toolkit
 *
 * 	Modification :  02/05/94
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
 * Please read COPYING and COPYING.LIB .
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
#include <X11/cursorfont.h> 
#include <X11/Xatom.h>

#include <X11/iman/widgets.h>

#include "bm/close.bm"
#include "bm/iconify.bm"
#include "bm/zoom.bm"
#include "bm/up.bm"
#include "bm/down.bm"
#include "bm/left.bm"
#include "bm/right.bm"
#include "bm/cercle1.bm"
#include "bm/cercle2.bm"
#include "bm/cercle3.bm"
#include "bm/radio1.bm"
#include "bm/radio2.bm"
#include "bm/radio3.bm"
#include "bm/radio4.bm"
#include "bm/check1.bm"
#include "bm/check2.bm"
#include "bm/check3.bm"
#include "bm/check4.bm"
#include "bm/graytil.bm"
#include "bm/right4l.bm"
#include "bm/right4g.bm"
#include "bm/left4l.bm"
#include "bm/left4g.bm"
#include "bm/up4l.bm"
#include "bm/up4g.bm"
#include "bm/down4l.bm"
#include "bm/down4g.bm"
#include "bm/rightMmask.bm"
#include "bm/upMmask.bm"
#include "bm/leftMmask.bm"
#include "bm/downMmask.bm"
#include "bm/combo1.bm"
#include "bm/combo2.bm"
#include "bm/combo3.bm"
#include "bm/popup1.bm"
#include "bm/popup2.bm"
#include "bm/popup3.bm"
#include "bm/popup4.bm"






/*
 * Ouvrir/Fermer une session avec le Window Manager 
 *
 */

TkDisplay *tk_OpenSession(display_name,argc,argv)
char *display_name;
int argc;
char **argv;
{
 int ret;
 GC gc;
 XGCValues xgcvalues;
 XColor xcolor;
 Colormap cmap;
 int i,j, fn_cptr, mask;
 Pixmap right4l, right4g, left4l, left4g, up4l, up4g, down4l, down4g;
 XKeyboardState values;
 TkDisplay *tk_display;
 unsigned long black, white;
																											

 char *helv1="*-helvetica-bold-r-*-120-*";
 char *helv2="lutbs10";
 char *helv3="-*-helvetica-bold-r-*--*-120-75-75-*-*-*-*";

 char *times1="*-times-bold-r-*-120-*";
 char *times2="timb10";
 char *times3="-*-times-bold-r-*--12-120-75-75-*-*-*-*";




			/*** Initialisation ***/


 tk_display=(TkDisplay *)malloc(sizeof(TkDisplay));
 if(tk_display==NULL)
   return NULL;


 tk_display->display=XOpenDisplay(display_name);
 if (tk_display->display == NULL)
 {
   free(tk_display);
   return NULL;
 }

#ifdef DEBUG
 else (void)fprintf(stderr,"Connection a %s etablie\n",display_name);
#endif

 tk_display->screen=DefaultScreen(tk_display->display);
 tk_display->depth=DefaultDepth(tk_display->display,tk_display->screen);
 tk_display->argc=argc;
 tk_display->argv=argv;
 XSetCloseDownMode(tk_display->display,DestroyAll);



#ifdef DEBUG
 (void)fprintf(stderr,"WM_InitSession commencee\n");
#endif



				 /* ATOMS */

 tk_display->atoms._IMAN_WINDOW_MANAGER=XInternAtom(tk_display->display,"_IMAN_WINDOW_MANAGER",False);
 tk_display->atoms._IMAN_HELP_SERVER=XInternAtom(tk_display->display,"_IMAN_HELP_SERVER",False);
 tk_display->atoms._IMAN_HS_DATABOOK=XInternAtom(tk_display->display,"_IMAN_HS_DATABOOK",False);
 tk_display->atoms._IMAN_HS_TOPIC=XInternAtom(tk_display->display,"_IMAN_HS_TOPIC",False);
 tk_display->atoms._IMAN_HS_MESSAGES=XInternAtom(tk_display->display,"_IMAN_HS_MESSAGES",False);


 tk_display->atoms._IMAN_WM_TYPE=XInternAtom(tk_display->display,"_IMAN_WM_TYPE",False);
 tk_display->atoms._IMAN_WM_DATA=XInternAtom(tk_display->display,"_IMAN_WM_DATA",False);
 tk_display->atoms._IMAN_WM_FOCUS=XInternAtom(tk_display->display,"_IMAN_WM_FOCUS",False);
 tk_display->atoms._IMAN_WM_MDW=XInternAtom(tk_display->display,"_IMAN_WM_MDW",False);
 tk_display->atoms._IMAN_WM_MESSAGES=XInternAtom(tk_display->display,"_IMAN_WM_MESSAGES",False);


 tk_display->atoms._IMAN_DROP_SITES=XInternAtom(tk_display->display,"_IMAN_DROP_SITES",False);
 tk_display->atoms._IMAN_DROP_TARGETS=XInternAtom(tk_display->display,"_IMAN_DROP_TARGETS",False);
 tk_display->atoms._IMAN_DROP_ACTION=XInternAtom(tk_display->display,"_IMAN_DROP_ACTION",False);
 tk_display->atoms._IMAN_DROP_MESSAGES=XInternAtom(tk_display->display,"_IMAN_DROP_MESSAGES",False);


 tk_display->atoms.WM_STATE=XInternAtom(tk_display->display,"WM_STATE",False);
 tk_display->atoms.WM_COLORMAP_WINDOWS=XInternAtom(tk_display->display,"WM_COLORMAP_WINDOWS",False);
 tk_display->atoms.WM_PROTOCOLS=XInternAtom(tk_display->display,"WM_PROTOCOLS",False);
 tk_display->atoms.WM_CHANGE_STATE=XInternAtom(tk_display->display,"WM_CHANGE_STATE",False);
 tk_display->atoms.WM_TAKE_FOCUS=XInternAtom(tk_display->display,"WM_TAKE_FOCUS",False);
 tk_display->atoms.WM_SAVE_YOURSELF=XInternAtom(tk_display->display,"WM_SAVE_YOURSELF",False);
 tk_display->atoms.WM_DELETE_WINDOW=XInternAtom(tk_display->display,"WM_DELETE_WINDOW",False);
 tk_display->atoms.WM_TOP_LEVEL=XInternAtom(tk_display->display,"WM_TOP_LEVEL",False);


				/****  FONTS  ****/


tk_display->fonts.flag=0;
/*fprintf(stderr,"Version: %d  Revision: %d\n",ProtocolVersion(tk_display->display),ProtocolRevision(tk_display->display));
fprintf(stderr,"Vendor:  *%s* \n",ServerVendor(tk_display->display));*/


if(strcmp(ServerVendor(tk_display->display),"DESQview/X (R), by Quarterdeck Office Systems")!=0)
{

 /*fprintf(stderr,"Release <3\n");*/
/*
 *  police "ega"
 *
 */

 if((tk_display->fonts.ega= XLoadQueryFont(tk_display->display,"ega"))==NULL);
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police ega\n");*/
 else tk_display->fonts.flag++;


/*
 *  police "vga"
 *
 */

 if((tk_display->fonts.vga= XLoadQueryFont(tk_display->display,"vga"))==NULL)
 {
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police vga\n");*/
  if(tk_display->fonts.flag==1) tk_display->fonts.vga=tk_display->fonts.ega;
 }
 else{
  if(tk_display->fonts.flag==0) 
	tk_display->fonts.ega=tk_display->fonts.vga;
  tk_display->fonts.flag= tk_display->fonts.flag+2;
  }


/*
 *  police "8x13"
 *
 */

 if((tk_display->fonts.f8_13=XLoadQueryFont(tk_display->display,"8x13"))==NULL)
 {
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police 8x13\n");*/
  if((tk_display->fonts.flag&2)==2) tk_display->fonts.f8_13=tk_display->fonts.vga;
  else { 
    fprintf(stderr,"Error: no 8x13 font. Exiting ...\n"); 
    exit(-1);}
 }
 else{ 
  if((tk_display->fonts.flag&1)!=1)
    tk_display->fonts.ega=tk_display->fonts.f8_13;
  if((tk_display->fonts.flag&2)!=2)
    tk_display->fonts.vga=tk_display->fonts.f8_13;
  tk_display->fonts.flag= tk_display->fonts.flag+4;
  }


/*
 *  police "6x10"
 *
 */

 if((tk_display->fonts.f6_10=XLoadQueryFont(tk_display->display,"6x10"))==NULL)
 {
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police 6x10\n");*/
  tk_display->fonts.f6_10=tk_display->fonts.f8_13;
 }
 else tk_display->fonts.flag=tk_display->fonts.flag+8;


/*
 *  police "fixed"
 *
 */

 if((tk_display->fonts.fixed= XLoadQueryFont(tk_display->display,"fixed"))==NULL)
 {
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police fixed\n");*/
  tk_display->fonts.fixed=tk_display->fonts.ega;
 }
 else tk_display->fonts.flag=tk_display->fonts.flag+16;


/*
 * police "helvetica"
 *
 */

 if((tk_display->fonts.helvetica12= XLoadQueryFont(tk_display->display,helv3))==NULL)
 {
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police helvetica(12)\n");*/
  tk_display->fonts.helvetica12=tk_display->fonts.ega;
 }
 else tk_display->fonts.flag=tk_display->fonts.flag+32;


/*
 *  police "times"
 *
 */

 if((tk_display->fonts.times12= XLoadQueryFont(tk_display->display,times3))==NULL)
 {
  /*(void) fprintf(stderr,"Avertissement: Impossible d'utiliser la police times(12)\n");*/
  tk_display->fonts.times12=tk_display->fonts.vga;  
 }
 else tk_display->fonts.flag=tk_display->fonts.flag+64;


/*
 *  police "5x7"
 *
 */

 if((tk_display->fonts.f5_7= XLoadQueryFont(tk_display->display,"5x7"))==NULL)
 {
  /*(void) fprintf(stderr,"Avertissement: Impossible d'utiliser la police 5x7\n");*/
  tk_display->fonts.f5_7=tk_display->fonts.f6_10;  
 }
 else tk_display->fonts.flag=tk_display->fonts.flag+128;

}



else
{

/*
 *  police "pc8x14"
 *
 */

 if((tk_display->fonts.ega= XLoadQueryFont(tk_display->display,"pc8x14"))==NULL);
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police pc8x14\n");*/
 else tk_display->fonts.flag++;


/*
 *  police "pc8x16"
 *
 */

 if((tk_display->fonts.vga= XLoadQueryFont(tk_display->display,"pc8x16"))==NULL)
 {
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police pc8x16\n");*/
  if(tk_display->fonts.flag==1) tk_display->fonts.vga=tk_display->fonts.ega;
 }
 else{
  if(tk_display->fonts.flag==0) 
	tk_display->fonts.ega=tk_display->fonts.vga;
  tk_display->fonts.flag= tk_display->fonts.flag+2;
  }


/*
 *  police "8x13"
 *
 */

 if((tk_display->fonts.f8_13= XLoadQueryFont(tk_display->display,"8x13"))==NULL)
 {
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police 8x13\n");*/
  if((tk_display->fonts.flag&2)==2) tk_display->fonts.f8_13=tk_display->fonts.vga;
  else { 
    fprintf(stderr,"Error: No 8x13 font. Exiting ...\n"); 
    exit(-1);}
 }
 else{ 
  if((tk_display->fonts.flag&1)!=1)
    tk_display->fonts.ega=tk_display->fonts.f8_13;
  if((tk_display->fonts.flag&2)!=2)
    tk_display->fonts.vga=tk_display->fonts.f8_13;
  tk_display->fonts.flag= tk_display->fonts.flag+4;
  }


/*
 *  police "6x10"
 *
 */

 if((tk_display->fonts.f6_10= XLoadQueryFont(tk_display->display,"6x10"))==NULL)
 {
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police 6x10\n");*/
  tk_display->fonts.f6_10=tk_display->fonts.f8_13;
 }
 else tk_display->fonts.flag= tk_display->fonts.flag+8;


/*
 *  police "fixed"
 *
 */

 if((tk_display->fonts.fixed= XLoadQueryFont(tk_display->display,"fixed"))==NULL)
 {
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police fixed\n");*/
  tk_display->fonts.fixed=tk_display->fonts.ega;
 }
 else tk_display->fonts.flag= tk_display->fonts.flag+16;


/*
 * police "helvetica"
 *
 */

 if((tk_display->fonts.helvetica12= XLoadQueryFont(tk_display->display,helv2))==NULL)
 {
  /*fprintf(stderr,"Avertissement: Impossible d'utiliser la police helvetica(12)\n");*/
  tk_display->fonts.helvetica12=tk_display->fonts.ega;
 }
 else tk_display->fonts.flag=tk_display->fonts.flag+32;


/*
 *  police "times"
 *
 */

 if((tk_display->fonts.times12= XLoadQueryFont(tk_display->display,times2))==NULL)
 {
  /*(void) fprintf(stderr,"Avertissement: Impossible d'utiliser la police times(12)\n");*/
  tk_display->fonts.times12=tk_display->fonts.vga;  
 }
 else tk_display->fonts.flag=tk_display->fonts.flag+64;


/*
 *  police "5x8"
 *
 */

 if((tk_display->fonts.f5_7= XLoadQueryFont(tk_display->display,"5x8"))==NULL)
 {
  tk_display->fonts.f5_7=tk_display->fonts.f6_10;  
  /*(void) fprintf(stderr,"Avertissement: Impossible d'utiliser la police 5x8\n");*/
 }
 else tk_display->fonts.flag=tk_display->fonts.flag+128;

}


  /*fprintf(stderr,"Je vais creer les pixmaps\n");*/

				/**** CURSORS ****/
  tk_display->cursors.normal=XCreateFontCursor(tk_display->display,XC_left_ptr);
  tk_display->cursors.top_left=XCreateFontCursor(tk_display->display,XC_top_left_corner);
  tk_display->cursors.top_right=XCreateFontCursor(tk_display->display,XC_top_right_corner);
  tk_display->cursors.bottom_left=XCreateFontCursor(tk_display->display,XC_bottom_left_corner);
  tk_display->cursors.bottom_right=XCreateFontCursor(tk_display->display,XC_bottom_right_corner);
  tk_display->cursors.up_down=XCreateFontCursor(tk_display->display,XC_sb_v_double_arrow);
  tk_display->cursors.left_right=XCreateFontCursor(tk_display->display,XC_sb_h_double_arrow);
  tk_display->cursors.sb_up=XCreateFontCursor(tk_display->display,XC_sb_left_arrow);
  tk_display->cursors.sb_left=XCreateFontCursor(tk_display->display,XC_sb_up_arrow);
  tk_display->cursors.textedit=XCreateFontCursor(tk_display->display,XC_xterm); 


				/* PIXMAPS 1-plane */
  tk_display->pixmaps.iconify=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),iconify_bits,iconify_width,iconify_height);
  tk_display->pixmaps.zoom=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),zoom_bits,zoom_width,zoom_height); 
  tk_display->pixmaps.close=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),close_bits,close_width,close_height); 
  tk_display->pixmaps.up=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),up_bits,up_width,up_height);
  tk_display->pixmaps.down=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),down_bits,down_width,down_height); 
  tk_display->pixmaps.left=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),left_bits,left_width,left_height); 
  tk_display->pixmaps.right=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),right_bits,right_width,right_height);
  tk_display->pixmaps.thumb1=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),cercle1_bits,cercle1_width,cercle1_height);
  tk_display->pixmaps.thumb2=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),cercle2_bits,cercle2_width,cercle2_height); 
  tk_display->pixmaps.radio1=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),radio1_bits,radio1_width,radio1_height); 
  tk_display->pixmaps.radio2=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),radio2_bits,radio2_width,radio2_height);
  tk_display->pixmaps.radio3=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),radio3_bits,radio3_width,radio3_height);
  tk_display->pixmaps.radio4=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),radio4_bits,radio4_width,radio4_height);
  tk_display->pixmaps.graytile=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),graytil_bits,graytil_width,graytil_height);
  tk_display->pixmaps.combo2=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),combo3_bits,combo3_width,combo3_height); 
  tk_display->pixmaps.combo1=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),combo2_bits,combo2_width,combo2_height);
  tk_display->pixmaps.popup1=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),popup1_bits,popup1_width,popup1_height); 
  tk_display->pixmaps.popup2=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),popup2_bits,popup2_width,popup2_height);
  tk_display->pixmaps.popup3=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),popup3_bits,popup3_width,popup3_height);
  tk_display->pixmaps.popup4=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),popup4_bits,popup4_width,popup4_height);

 
  right4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),right4g_bits,right4g_width,right4g_height); 
  right4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),right4l_bits,right4l_width,right4l_height);
  left4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),left4l_bits,left4l_width,left4l_height);
  left4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),left4g_bits,left4g_width,left4g_height);
  up4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),up4g_bits,up4g_width,up4g_height); 
  up4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),up4l_bits,up4l_width,up4l_height);
  down4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),down4l_bits,down4l_width,down4l_height);
  down4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),down4g_bits,down4g_width,down4g_height);



  /*fprintf(stderr,"Je vais creer les couleurs\n");*/



			/* COLORS DATA BASE */


  black=BlackPixel(tk_display->display,tk_display->screen);
  white=WhitePixel(tk_display->display,tk_display->screen);
  cmap=DefaultColormap(tk_display->display, tk_display->screen);
  j=0;


  tk_display->wm.version=0;
  tk_display->wm.release=0;
  tk_display->wm.comment=NULL;
  tk_GetConnection(tk_display);
  help_GetConnection(tk_display);
 
  
  /*fprintf(stderr,"Couleurs par defaut\n");*/

			    	/* BUTTON COLORS */ 
  tk_display->bn_colors.light=white;
  tk_display->bn_colors.shadow=black;
  tk_display->bn_colors.bg=white;
  tk_display->bn_colors.text=black;
  tk_display->bn_colors.focus=black;
  tk_display->bn_colors.nofocus=black;
  tk_display->bn_colors.text_grayed=black;
  tk_display->bn_colors.cross=black;
  tk_display->bn_colors.check=black;
  tk_display->bn_colors.radio_bg=black;
  tk_display->bn_colors.radio_light=white;

			 	/* SCROLLBAR COLORS */
  tk_display->sb_colors.light=white;
  tk_display->sb_colors.bg=black;
  tk_display->sb_colors.focus=black;
  tk_display->sb_colors.nofocus=black;

			      	/* EDIT COLORS */
  tk_display->ed_colors.bg=white;
  tk_display->ed_colors.text=black;
  tk_display->ed_colors.selected=black;
  tk_display->ed_colors.text_selected=white;
  tk_display->ed_colors.text_grayed=black;
  tk_display->ed_colors.cursor=black;
  tk_display->ed_colors.focus=black;
  tk_display->ed_colors.nofocus=black;

			   	/* LIST COLORS */
  tk_display->ls_colors.bg=white;
  tk_display->ls_colors.selected=black;
  tk_display->ls_colors.text=black;
  tk_display->ls_colors.text_selected=white;
  tk_display->ls_colors.text_grayed=black;
  tk_display->ls_colors.text_grayed_selected=black; 
  tk_display->ls_colors.text_selected_inactive=black;
  tk_display->ls_colors.selected_inactive=white;
  tk_display->ls_colors.text_grayed_selected_inactive=black; 
  tk_display->ls_colors.focus=black;
  tk_display->ls_colors.nofocus=black;

				/* COMBO COLORS */
  tk_display->cb_colors.focus=black;
  tk_display->cb_colors.nofocus=black;

			   	/* MENU COLORS */
  tk_display->mn_colors.bg=white;
  tk_display->mn_colors.selected=black;
  tk_display->mn_colors.text=black;
  tk_display->mn_colors.text_selected=white;
  tk_display->mn_colors.text_grayed=black;
  tk_display->mn_colors.text_grayed_selected=black; 
  tk_display->mn_colors.text_selected_inactive=black;
  tk_display->mn_colors.selected_inactive=white;
  tk_display->mn_colors.text_grayed_selected_inactive=black; 
  tk_display->mn_colors.focus=black;
  tk_display->mn_colors.nofocus=black;

				/* WINDOW COLORS */
  tk_display->win_colors.title_bg_active=white;	
  tk_display->win_colors.title_bg_inactive=black;
  tk_display->win_colors.title_text_active=black;
  tk_display->win_colors.title_text_inactive=white;	
  tk_display->win_colors.border_active=black;
  tk_display->win_colors.border_inactive=black;	
  tk_display->win_colors.light=white;
  tk_display->win_colors.bg=white;	
  tk_display->win_colors.shadow=black; 
  tk_display->win_colors.text=black;	
  tk_display->win_colors.text_grayed=black; 
	

				/* DIALOG COLORS */
  tk_display->dlg_colors.title_bg_active=white;	
  tk_display->dlg_colors.title_bg_inactive=black;
  tk_display->dlg_colors.title_text_active=black;
  tk_display->dlg_colors.title_text_inactive=white;	
  tk_display->dlg_colors.border_active=black;
  tk_display->dlg_colors.border_inactive=black;	
  tk_display->dlg_colors.light=white;
  tk_display->dlg_colors.bg=white;	
  tk_display->dlg_colors.shadow=black; 
  tk_display->dlg_colors.text=black;	
  tk_display->dlg_colors.text_grayed=black; 


				/* ICON COLORS */
  tk_display->icn_colors.icn_bg=white;	
  tk_display->icn_colors.icn_light=white;
  tk_display->icn_colors.icn_shadow=black; 
  tk_display->icn_colors.icn_draw=black; 
  tk_display->icn_colors.icn_draw_bg=white; 
  tk_display->icn_colors.title_bg_active=white; 
  tk_display->icn_colors.title_text_active=black; 
  tk_display->icn_colors.title_bg_inactive=black; 
  tk_display->icn_colors.title_text_inactive=white; 	

  tk_RefreshColors(tk_display);


				/**** INFOS ****/


  tk_display->infos.vendor="Bruno Rivas";
  tk_display->infos.version=1;
  tk_display->infos.release=0;
  tk_display->infos.comment="IMAN Toolkit version 1.2 Copyright (c) 1993,1994 Bruno RIVAS";




		   /****** Pixmap DefaultDepth ******/


 mask=GCForeground|GCBackground;
 xgcvalues.foreground= xgcvalues.background=tk_display->bn_colors.shadow;
 gc=XCreateGC(tk_display->display,RootWindow(tk_display->display,tk_display->screen), mask, &xgcvalues);

 tk_display->pixmaps.thumbM=XCreatePixmap(tk_display->display,RootWindow(tk_display->display,tk_display->screen),cercle1_width,cercle1_height,tk_display->depth);
  XCopyPlane(tk_display->display,tk_display->pixmaps.thumb1,tk_display->pixmaps.thumbM,gc,0,0,cercle1_width,cercle1_height,0,0,1);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.radio_light);
  XCopyPlane(tk_display->display,tk_display->pixmaps.thumb2,tk_display->pixmaps.thumbM,gc,0,0,cercle1_width,cercle1_height,0,0,1);
 tk_display->pixmaps.thumbMmask=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),cercle3_bits,cercle3_width,cercle3_height);


 tk_display->pixmaps.upM=XCreatePixmapFromBitmapData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),up_bits,up_width,up_height,tk_display->bn_colors.text,tk_display->bn_colors.bg,tk_display->depth);
 tk_display->pixmaps.upMgrayed=XCreatePixmap(tk_display->display,RootWindow(tk_display->display,tk_display->screen),up4g_width,up4g_height,tk_display->depth);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.shadow);
  XCopyPlane(tk_display->display,up4g,tk_display->pixmaps.upMgrayed,gc,0,0,up4g_width,up4g_height,0,0,1);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.light);
  XCopyPlane(tk_display->display,up4l,tk_display->pixmaps.upMgrayed,gc,0,0,up4l_width,up4l_height,0,0,1);
 tk_display->pixmaps.upMmask=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),upMmask_bits,upMmask_width,up4l_height);



 tk_display->pixmaps.downM=XCreatePixmapFromBitmapData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),down_bits,down_width,down_height,tk_display->bn_colors.text,tk_display->bn_colors.bg,tk_display->depth);
 tk_display->pixmaps.downMgrayed=XCreatePixmap(tk_display->display,RootWindow(tk_display->display,tk_display->screen),down4g_width,down4g_height,tk_display->depth);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.shadow);
  XCopyPlane(tk_display->display,down4g,tk_display->pixmaps.downMgrayed,gc,0,0,down4g_width,down4g_height,0,0,1);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.light);
  XCopyPlane(tk_display->display,down4l,tk_display->pixmaps.downMgrayed,gc,0,0,down4l_width,down4l_height,0,0,1);
 tk_display->pixmaps.downMmask=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),downMmask_bits,downMmask_width,downMmask_height);



 tk_display->pixmaps.leftM=XCreatePixmapFromBitmapData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),left_bits,left_width,left_height,tk_display->bn_colors.text,tk_display->bn_colors.bg,tk_display->depth);
 tk_display->pixmaps.leftMgrayed=XCreatePixmap(tk_display->display,RootWindow(tk_display->display,tk_display->screen),left4g_width,left4g_height,tk_display->depth);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.shadow);
  XCopyPlane(tk_display->display,left4g,tk_display->pixmaps.leftMgrayed,gc,0,0,left4g_width,left4g_height,0,0,1);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.light);
  XCopyPlane(tk_display->display,left4l,tk_display->pixmaps.leftMgrayed,gc,0,0,left4l_width,left4l_height,0,0,1);
 tk_display->pixmaps.leftMmask=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),leftMmask_bits,leftMmask_width,leftMmask_height);


 tk_display->pixmaps.rightM=XCreatePixmapFromBitmapData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),right_bits,right_width,right_height,tk_display->bn_colors.text,tk_display->bn_colors.bg,tk_display->depth);
 tk_display->pixmaps.rightMgrayed=XCreatePixmap(tk_display->display,RootWindow(tk_display->display,tk_display->screen),right4g_width,right4g_height,tk_display->depth);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.shadow);
  XCopyPlane(tk_display->display,right4g,tk_display->pixmaps.rightMgrayed,gc,0,0,right4g_width,right4g_height,0,0,1);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.light);
  XCopyPlane(tk_display->display,right4l,tk_display->pixmaps.rightMgrayed,gc,0,0,right4l_width,right4l_height,0,0,1);
 tk_display->pixmaps.rightMmask=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),rightMmask_bits,rightMmask_width,rightMmask_height);



 tk_display->pixmaps.comboMgrayed=XCreatePixmap(tk_display->display,RootWindow(tk_display->display,tk_display->screen),combo1_width,combo1_height,tk_display->depth);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.shadow);
  XCopyPlane(tk_display->display,tk_display->pixmaps.combo1,tk_display->pixmaps.comboMgrayed,gc,0,0,combo1_width,combo2_height,0,0,1);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.light);
  XCopyPlane(tk_display->display,tk_display->pixmaps.combo2,tk_display->pixmaps.comboMgrayed,gc,0,0,combo2_width,combo3_height,0,0,1);
 tk_display->pixmaps.comboMmask=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),combo1_bits,combo1_width,combo1_height);

/*tk_display->pixmaps.comboM=XCreatePixmapFromBitmapData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),combo1_bits,combo1_width,combo1_height,tk_display->bn_colors.text,tk_display->bn_colors.bg,tk_display->depth);*/

 tk_display->pixmaps.comboM=XCreatePixmap(tk_display->display,RootWindow(tk_display->display,tk_display->screen),combo1_width,combo1_height,tk_display->depth);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.light);
  XCopyPlane(tk_display->display,tk_display->pixmaps.combo1,tk_display->pixmaps.comboM,gc,0,0,combo2_width,combo2_height,0,0,1);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.shadow);
  XCopyPlane(tk_display->display,tk_display->pixmaps.combo2,tk_display->pixmaps.comboM,gc,0,0,combo2_width,combo2_height,0,0,1);
  XSetForeground(tk_display->display,gc,tk_display->bn_colors.light);
  XCopyPlane(tk_display->display,tk_display->pixmaps.combo1,tk_display->pixmaps.comboM,gc,0,0,combo2_width,combo2_height,0,0,1);



		/*** Creation des structures de widgets ***/


  tk_display->numwidgets=0;
  tk_display->maxwidgets=0;
  WID_Add(tk_display,30);
  /*tk_display->widgets[0].isUsed=True;
  tk_display->widgets[0].type=0;*/


			/*** Reglage du clavier ***/


 tk_display->widget_double_click=tk_GetDoubleClickSpeed();
 XGetKeyboardControl(tk_display->display,&values);

/* fprintf(stderr,"percent: %d  mode: %d\n",values.key_click_percent,values.global_auto_repeat);*/


 XFreePixmap(tk_display->display,up4l);
 XFreePixmap(tk_display->display,down4l);
 XFreePixmap(tk_display->display,left4l);
 XFreePixmap(tk_display->display,right4l);
 XFreePixmap(tk_display->display,up4g);
 XFreePixmap(tk_display->display,down4g);
 XFreePixmap(tk_display->display,left4g);
 XFreePixmap(tk_display->display,right4g); 


 XFreeGC(tk_display->display,gc);
 memset(&tk_display->action,0,sizeof(ActionStruct));


 return (TkDisplay *)tk_display;;
}






/*
 * Fermer la session pour l'ecran tk_display
 *
 */


int tk_CloseSession(tk_display)
TkDisplay *tk_display;
{

 if(tk_display==NULL)
   return -1;
 
 XFreeCursor(tk_display->display,tk_display->cursors.normal);
 XFreeCursor(tk_display->display,tk_display->cursors.top_left);
 XFreeCursor(tk_display->display,tk_display->cursors.top_right);
 XFreeCursor(tk_display->display,tk_display->cursors.bottom_left);
 XFreeCursor(tk_display->display,tk_display->cursors.bottom_right);
 XFreeCursor(tk_display->display,tk_display->cursors.up_down);
 XFreeCursor(tk_display->display,tk_display->cursors.left_right);
 XFreeCursor(tk_display->display,tk_display->cursors.sb_up);
 XFreeCursor(tk_display->display,tk_display->cursors.sb_left);
 XFreeCursor(tk_display->display,tk_display->cursors.textedit);
/* fprintf(stderr,"Curseur passe\n");*/

				/* PIXMAPS 1-plane */
 XFreePixmap(tk_display->display,tk_display->pixmaps.iconify);
 XFreePixmap(tk_display->display,tk_display->pixmaps.zoom);
 XFreePixmap(tk_display->display,tk_display->pixmaps.close);
 XFreePixmap(tk_display->display,tk_display->pixmaps.up);
 XFreePixmap(tk_display->display,tk_display->pixmaps.down);
 XFreePixmap(tk_display->display,tk_display->pixmaps.left);
 XFreePixmap(tk_display->display,tk_display->pixmaps.right);
 XFreePixmap(tk_display->display,tk_display->pixmaps.thumb1);
 XFreePixmap(tk_display->display,tk_display->pixmaps.thumb2);
 XFreePixmap(tk_display->display,tk_display->pixmaps.radio1);
 XFreePixmap(tk_display->display,tk_display->pixmaps.radio2);
 XFreePixmap(tk_display->display,tk_display->pixmaps.radio3);
 XFreePixmap(tk_display->display,tk_display->pixmaps.radio4);
 XFreePixmap(tk_display->display,tk_display->pixmaps.graytile);
 XFreePixmap(tk_display->display,tk_display->pixmaps.thumbM);
 XFreePixmap(tk_display->display,tk_display->pixmaps.thumbMmask);
 XFreePixmap(tk_display->display,tk_display->pixmaps.upM);
 XFreePixmap(tk_display->display,tk_display->pixmaps.downM);
 XFreePixmap(tk_display->display,tk_display->pixmaps.leftM);
 XFreePixmap(tk_display->display,tk_display->pixmaps.rightM);
 XFreePixmap(tk_display->display,tk_display->pixmaps.upMmask);
 XFreePixmap(tk_display->display,tk_display->pixmaps.downMmask);
 XFreePixmap(tk_display->display,tk_display->pixmaps.leftMmask);
 XFreePixmap(tk_display->display,tk_display->pixmaps.rightMmask);
 XFreePixmap(tk_display->display,tk_display->pixmaps.upMgrayed);
 XFreePixmap(tk_display->display,tk_display->pixmaps.downMgrayed);
 XFreePixmap(tk_display->display,tk_display->pixmaps.leftMgrayed);
 XFreePixmap(tk_display->display,tk_display->pixmaps.rightMgrayed);
 XFreePixmap(tk_display->display,tk_display->pixmaps.combo1);
 XFreePixmap(tk_display->display,tk_display->pixmaps.combo2);
 XFreePixmap(tk_display->display,tk_display->pixmaps.comboM);
 XFreePixmap(tk_display->display,tk_display->pixmaps.comboMgrayed);
 XFreePixmap(tk_display->display,tk_display->pixmaps.comboMmask);
 XFreePixmap(tk_display->display,tk_display->pixmaps.popup1);
 XFreePixmap(tk_display->display,tk_display->pixmaps.popup2);
 XFreePixmap(tk_display->display,tk_display->pixmaps.popup3);
 XFreePixmap(tk_display->display,tk_display->pixmaps.popup4);

/* fprintf(stderr,"Pix passe\n");*/
 if((tk_display->fonts.flag&1)==1) XFreeFont(tk_display->display,tk_display->fonts.ega);
/* fprintf(stderr,"Pix 1\n");*/
 if((tk_display->fonts.flag&2)==2) XFreeFont(tk_display->display,tk_display->fonts.vga);
/* fprintf(stderr,"Pix 2\n");*/
 if((tk_display->fonts.flag&4)==4) XFreeFont(tk_display->display,tk_display->fonts.f8_13);
/* fprintf(stderr,"Pix 4\n");*/
/* if((tk_display->fonts.flag&8)==8) XFreeFont(tk_display->display,tk_display->fonts.f6_10);
 fprintf(stderr,"Pix 8\n");
 if((tk_display->fonts.flag&16)==16) XFreeFont(tk_display->display,tk_display->fonts.fixed);
 fprintf(stderr,"Pix 16\n");
 if((tk_display->fonts.flag&32)==32) XFreeFont(tk_display->display,tk_display->fonts.helvetica12);
 fprintf(stderr,"Pix 32\n");
 if((tk_display->fonts.flag&64)==64) XFreeFont(tk_display->display,tk_display->fonts.times12);
 fprintf(stderr,"Pix 64\n");
 if((tk_display->fonts.flag&128)==128) XFreeFont(tk_display->display,tk_display->fonts.f5_7);
*/

/* fprintf(stderr,"Attention\n");*/
 WID_Free(tk_display);
 XCloseDisplay(tk_display->display);
 free(tk_display);
 tk_display=NULL;

 return 0;
}




