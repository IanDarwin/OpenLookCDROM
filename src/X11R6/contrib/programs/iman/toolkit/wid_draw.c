/*
 *
 * 	wid_draw.c  
 * 	affichage des widgets generiques
 *
 * 	Modification :  26/12/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
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

#include <X11/iman/widgets.h>


#ifdef DESQVIEW_X_SERVER

#include "/src/toolkit/bm/close.bm"
#include "/src/toolkit/bm/iconify.bm"
#include "/src/toolkit/bm/zoom.bm"
#include "/src/toolkit/bm/up.bm"
#include "/src/toolkit/bm/down.bm"
#include "/src/toolkit/bm/left.bm"
#include "/src/toolkit/bm/right.bm"
#include "/src/toolkit/bm/cercle1.bm"
#include "/src/toolkit/bm/cercle2.bm"
#include "/src/toolkit/bm/cercle3.bm"
#include "/src/toolkit/bm/radio1.bm"
#include "/src/toolkit/bm/radio2.bm"
#include "/src/toolkit/bm/radio3.bm"
#include "/src/toolkit/bm/radio4.bm"
#include "/src/toolkit/bm/check1.bm"
#include "/src/toolkit/bm/check2.bm"
#include "/src/toolkit/bm/check3.bm"
#include "/src/toolkit/bm/check4.bm"
#include "/src/toolkit/bm/graytil.bm"
#include "/src/toolkit/bm/right4l.bm"
#include "/src/toolkit/bm/right4g.bm"
#include "/src/toolkit/bm/left4l.bm"
#include "/src/toolkit/bm/left4g.bm"
#include "/src/toolkit/bm/up4l.bm"
#include "/src/toolkit/bm/up4g.bm"
#include "/src/toolkit/bm/down4l.bm"
#include "/src/toolkit/bm/down4g.bm"
#include "/src/toolkit/bm/rightM.bm"
#include "/src/toolkit/bm/upM.bm"
#include "/src/toolkit/bm/leftM.bm"
#include "/src/toolkit/bm/downM.bm"
#include "/src/toolkit/bm/combo1.bm"
#include "/src/toolkit/bm/combo2.bm"
#include "/src/toolkit/bm/combo3.bm"
#include "/src/toolkit/bm/popup1.bm"
#include "/src/toolkit/bm/popup2.bm"
#include "/src/toolkit/bm/popup3.bm"
#include "/src/toolkit/bm/popup4.bm"

#else

#include "/usr/src/iman/toolkit/bm/close.bm"
#include "/usr/src/iman/toolkit/bm/iconify.bm"
#include "/usr/src/iman/toolkit/bm/zoom.bm"
#include "/usr/src/iman/toolkit/bm/up.bm"
#include "/usr/src/iman/toolkit/bm/down.bm"
#include "/usr/src/iman/toolkit/bm/left.bm"
#include "/usr/src/iman/toolkit/bm/right.bm"
#include "/usr/src/iman/toolkit/bm/cercle1.bm"
#include "/usr/src/iman/toolkit/bm/cercle2.bm"
#include "/usr/src/iman/toolkit/bm/cercle3.bm"
#include "/usr/src/iman/toolkit/bm/radio1.bm"
#include "/usr/src/iman/toolkit/bm/radio2.bm"
#include "/usr/src/iman/toolkit/bm/radio3.bm"
#include "/usr/src/iman/toolkit/bm/radio4.bm"
#include "/usr/src/iman/toolkit/bm/check1.bm"
#include "/usr/src/iman/toolkit/bm/check2.bm"
#include "/usr/src/iman/toolkit/bm/check3.bm"
#include "/usr/src/iman/toolkit/bm/check4.bm"
#include "/usr/src/iman/toolkit/bm/graytil.bm"
#include "/usr/src/iman/toolkit/bm/right4l.bm"
#include "/usr/src/iman/toolkit/bm/right4g.bm"
#include "/usr/src/iman/toolkit/bm/left4l.bm"
#include "/usr/src/iman/toolkit/bm/left4g.bm"
#include "/usr/src/iman/toolkit/bm/up4l.bm"
#include "/usr/src/iman/toolkit/bm/up4g.bm"
#include "/usr/src/iman/toolkit/bm/down4l.bm"
#include "/usr/src/iman/toolkit/bm/down4g.bm"
#include "/usr/src/iman/toolkit/bm/rightMmask.bm"
#include "/usr/src/iman/toolkit/bm/upMmask.bm"
#include "/usr/src/iman/toolkit/bm/leftMmask.bm"
#include "/usr/src/iman/toolkit/bm/downMmask.bm"
#include "/usr/src/iman/toolkit/bm/combo1.bm"
#include "/usr/src/iman/toolkit/bm/combo2.bm"
#include "/usr/src/iman/toolkit/bm/combo3.bm"
#include "/usr/src/iman/toolkit/bm/popup1.bm"
#include "/usr/src/iman/toolkit/bm/popup2.bm"
#include "/usr/src/iman/toolkit/bm/popup3.bm"
#include "/usr/src/iman/toolkit/bm/popup4.bm"

#endif





			/**** Fonction generique ****/


int wid_Map(tk_display,widgetid)
TkDisplay *tk_display;
WidgetID widgetid;
{

 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {
	case WI_BUTTON :
		  return BN_Map(tk_display,widgetid);
		  break;

	case WI_SCROLLBAR :
		  return SB_Map(tk_display,widgetid);
		  break;

	case WI_EDIT :
		  return ED_Map(tk_display,widgetid);
		  break;

	case WI_LIST :
		  return LS_Map(tk_display,widgetid);
		  break;

	case WI_COMBO :
		  return CB_Map(tk_display,widgetid);
		  break;

	case WI_MENU :
		  return MN_Map(tk_display,widgetid,tk_display->widgets[widgetid].menu->x,tk_display->widgets[widgetid].menu->y);
		  break;

	default : return -1;
 		  break;

   }

 }
 else return -1;
}




int wid_Unmap(tk_display,widgetid)
TkDisplay *tk_display;
WidgetID widgetid;
{

 if(widgetid>=0 && widgetid<tk_display->maxwidgets && tk_display->widgets[widgetid].isUsed==True)
 {
   switch(tk_display->widgets[widgetid].class)
   {
	case WI_BUTTON :
		  return BN_Unmap(tk_display,widgetid);
		  break;

	case WI_SCROLLBAR :
		  return SB_Unmap(tk_display,widgetid);
		  break;

	case WI_EDIT :
		  return ED_Unmap(tk_display,widgetid);
		  break;

	case WI_LIST :
		  return LS_Unmap(tk_display,widgetid);
		  break;

	case WI_COMBO :
		  return CB_Unmap(tk_display,widgetid);
		  break;

	case WI_MENU :
		  return MN_Unmap(tk_display,widgetid);
		  break;

	default : return -1;
 		  break;

   }

 }
 else return -1;
}






int WID_Refresh(tk_display,toplevel)
TkDisplay *tk_display;
Window toplevel;
{
 int i;
 Pixmap right4l, right4g, left4l, left4g, up4l, up4g, down4l, down4g;
 Pixmap upM, downM, rightM, leftM;
 int mask, direction;
 XGCValues xgcvalues;
 GC gc; 


 right4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),right4g_bits,right4g_width,right4g_height); 
 right4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),right4l_bits,right4l_width,right4l_height);
 left4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),left4l_bits,left4l_width,left4l_height);
 left4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),left4g_bits,left4g_width,left4g_height);
 up4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),up4g_bits,up4g_width,up4g_height); 
 up4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),up4l_bits,up4l_width,up4l_height);
 down4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),down4l_bits,down4l_width,down4l_height);
 down4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),down4g_bits,down4g_width,down4g_height);


 upM=tk_display->pixmaps.upM;
 downM=tk_display->pixmaps.downM;
 leftM=tk_display->pixmaps.leftM;
 rightM=tk_display->pixmaps.rightM;

 XFreePixmap(tk_display->display,tk_display->pixmaps.thumbM);
 XFreePixmap(tk_display->display,tk_display->pixmaps.thumbMmask);
 XFreePixmap(tk_display->display,tk_display->pixmaps.upM);
 XFreePixmap(tk_display->display,tk_display->pixmaps.upMgrayed);
 XFreePixmap(tk_display->display,tk_display->pixmaps.upMmask);
 XFreePixmap(tk_display->display,tk_display->pixmaps.downM);
 XFreePixmap(tk_display->display,tk_display->pixmaps.downMgrayed);
 XFreePixmap(tk_display->display,tk_display->pixmaps.downMmask); 
 XFreePixmap(tk_display->display,tk_display->pixmaps.leftM);
 XFreePixmap(tk_display->display,tk_display->pixmaps.leftMmask);
 XFreePixmap(tk_display->display,tk_display->pixmaps.leftMgrayed);
 XFreePixmap(tk_display->display,tk_display->pixmaps.rightMgrayed);
 XFreePixmap(tk_display->display,tk_display->pixmaps.rightMmask);
 XFreePixmap(tk_display->display,tk_display->pixmaps.rightM);
 XFreePixmap(tk_display->display,tk_display->pixmaps.comboMgrayed);
 XFreePixmap(tk_display->display,tk_display->pixmaps.comboMmask); 
 XFreePixmap(tk_display->display,tk_display->pixmaps.comboM); 


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


 XFreePixmap(tk_display->display,up4l);
 XFreePixmap(tk_display->display,down4l);
 XFreePixmap(tk_display->display,left4l);
 XFreePixmap(tk_display->display,right4l);
 XFreePixmap(tk_display->display,up4g);
 XFreePixmap(tk_display->display,down4g);
 XFreePixmap(tk_display->display,left4g);
 XFreePixmap(tk_display->display,right4g); 




if(tk_display->numwidgets>0)
{
 for(i=0;i<tk_display->maxwidgets;i++)
 if(tk_display->widgets[i].isUsed==True && tk_display->widgets[i].identity.usePrivateColors==False ) 
 switch(tk_display->widgets[i].class)
 {
  case WI_BUTTON: switch(tk_display->widgets[i].button->type)
		  {
		   case BN_PUSHBUTTON:
				       if(tk_display->widgets[i].button->top_level==toplevel)
					 bn_DrawPushButton(tk_display,i);
				       break;
		   case BN_POPUPBUTTON: if(tk_display->widgets[i].button->top_level==toplevel)
					  bn_DrawPopupButton(tk_display,i);
				        break;
		   case BN_REPEATBUTTON: if(tk_display->widgets[i].button->top_level==toplevel)
					   bn_DrawRepeatButton(tk_display,i);
				         break;
		   case BN_CROSSBUTTON: if(tk_display->widgets[i].button->top_level==toplevel)
					  bn_DrawCrossButton(tk_display,i);
				        break;
		   case BN_RADIOBUTTON: if(tk_display->widgets[i].button->top_level==toplevel)
					  bn_DrawRadioButton(tk_display,i);
				   	break;	
		   case BN_POPUPRADIOBUTTON: if(tk_display->widgets[i].button->top_level==toplevel)
					bn_DrawPopupRadioButton(tk_display,i);
				       break;			     
		   case BN_SCROLLBUTTON: 
					if(tk_display->widgets[i].button->pixmap==upM)
					  direction=UP;
					if(tk_display->widgets[i].button->pixmap==downM)
					  direction=DOWN;
					if(tk_display->widgets[i].button->pixmap==leftM)
					  direction=LEFT;
					if(tk_display->widgets[i].button->pixmap==rightM)
					  direction=RIGHT;
					if(direction==UP) BN_SetPixmap(tk_display,i,tk_display->pixmaps.upM,tk_display->pixmaps.up,CenterBitmap,3,3,11,11,tk_display->depth,YES);
 					if(direction==DOWN) BN_SetPixmap(tk_display,i,tk_display->pixmaps.downM,tk_display->pixmaps.down,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 					if(direction==RIGHT) BN_SetPixmap(tk_display,i,tk_display->pixmaps.rightM,tk_display->pixmaps.right,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 					if(direction==LEFT) BN_SetPixmap(tk_display,i,tk_display->pixmaps.leftM,tk_display->pixmaps.left,CenterBitmap,3,3,11,10,tk_display->depth,YES);

 					if(direction==UP) BN_SetPixmapGrayed(tk_display,i,tk_display->pixmaps.upMgrayed,tk_display->pixmaps.upMmask,CenterBitmap,3,3,11,11,tk_display->depth,YES);
 					if(direction==DOWN) BN_SetPixmapGrayed(tk_display,i,tk_display->pixmaps.downMgrayed,tk_display->pixmaps.downMmask,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 					if(direction==RIGHT) BN_SetPixmapGrayed(tk_display,i,tk_display->pixmaps.rightMgrayed,tk_display->pixmaps.rightMmask,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 					if(direction==LEFT) BN_SetPixmapGrayed(tk_display,i,tk_display->pixmaps.leftMgrayed,tk_display->pixmaps.leftMmask,CenterBitmap,3,3,11,10,tk_display->depth,YES);
					if(tk_display->widgets[i].button->top_level==toplevel)
					  bn_DrawScrollButton(tk_display,i);
				        break;
		   case BN_THUMBBUTTON: 
					BN_SetPixmap(tk_display,i,tk_display->pixmaps.thumbM,tk_display->pixmaps.thumbMmask,CenterBitmap,3,3,11,11,tk_display->depth,YES);
					if(tk_display->widgets[i].button->top_level==toplevel)
					  bn_DrawThumbButton(tk_display,i);
				  	break;
		   case BN_CHECKBUTTON: bn_DrawCheckButton(tk_display,i);
				       break;

		   case BN_COMBOBUTTON: 
						BN_SetPixmap(tk_display,i,tk_display->pixmaps.comboM,tk_display->pixmaps.comboMmask,CenterBitmap,3,3,11,11,tk_display->depth,YES);
 					BN_SetPixmapGrayed(tk_display,i,tk_display->pixmaps.comboMgrayed,tk_display->pixmaps.comboMmask,CenterBitmap,3,3,11,11,tk_display->depth,YES);
					if(tk_display->widgets[tk_display->widgets[i].button->parency_number].combo->hasFocus==True)	_LS_DrawFocus(tk_display,tk_display->widgets[tk_display->widgets[i].button->parency_number].combo->list,ON);
					else 
_LS_DrawFocus(tk_display,tk_display->widgets[tk_display->widgets[i].button->parency_number].combo->list,OFF);
					if(tk_display->widgets[i].button->top_level==toplevel)
					  bn_DrawComboButton(tk_display,i);
					break;
		  }
		  break;


  case WI_SCROLLBAR:
		  XSetWindowBackground(tk_display->display,tk_display->widgets[i].scroll->mainwindow,tk_display->sb_colors.bg);
		  XSetWindowBackground(tk_display->display,tk_display->widgets[i].scroll->thumbwindow,tk_display->sb_colors.bg);
		  XClearWindow(tk_display->display,tk_display->widgets[i].scroll->thumbwindow);
		  if(tk_display->widgets[i].scroll->parency_class==0)
		  {
		    if(tk_display->widgets[tk_display->widgets[i].scroll->parency_number].list->hasFocus==True)
		      XSetWindowBorder(tk_display->display,tk_display->widgets[i].scroll->mainwindow,tk_display->bn_colors.focus);
		    else
		      XSetWindowBorder(tk_display->display,tk_display->widgets[i].scroll->mainwindow,tk_display->bn_colors.nofocus);
		  }
		  else 
		  {
			if(tk_display->widgets[tk_display->widgets[i].scroll->parency_number].list->hasFocus==True)
			  _LS_DrawFocus(tk_display,tk_display->widgets[i].scroll->parency_number,ON);
			else 
			  _LS_DrawFocus(tk_display,tk_display->widgets[i].scroll->parency_number,OFF);
		  }
		  break;

  case WI_EDIT :  XSetWindowBackground(tk_display->display,tk_display->widgets[i].edit->window,tk_display->ed_colors.bg);
		  XClearWindow(tk_display->display,tk_display->widgets[i].edit->window);
		  ed_DrawEdit(tk_display,i);
		  if(tk_display->widgets[i].edit->hasFocus==True)
		    _ED_DrawFocus(tk_display,i,ON);
		  else _ED_DrawFocus(tk_display,i,OFF); 
 		  break;


  case WI_LIST :  
		  XSetWindowBackground(tk_display->display,tk_display->widgets[i].list->listwindow,tk_display->ls_colors.bg);
		  XClearWindow(tk_display->display,tk_display->widgets[i].list->listwindow);
		  ls_DrawList(tk_display,i);
 		  if(tk_display->widgets[i].list->hasFocus==True)
		    _LS_DrawFocus(tk_display,i,ON);
		  else _LS_DrawFocus(tk_display,i,OFF);
		  break;

  case WI_MENU :  
		  XSetWindowBackground(tk_display->display,tk_display->widgets[i].menu->window,tk_display->mn_colors.bg);
		  XClearWindow(tk_display->display,tk_display->widgets[i].menu->window);
		  if(tk_display->widgets[i].menu->type==MN_FLOATING)
		    mn_DrawFloatingMenu(tk_display,i);
		  else if(tk_display->widgets[i].menu->type==MN_MENUBAR)
		    mn_DrawMenuBar(tk_display,i);
		  if(tk_display->widgets[i].menu->hasFocus==True)
		    XSetWindowBorder(tk_display->display,tk_display->widgets[i].menu->window,tk_display->widgets[i].colors->focus);
		  else XSetWindowBorder(tk_display->display,tk_display->widgets[i].menu->window,tk_display->widgets[i].colors->nofocus);
 
		  break;


  }
 }
}






int wid_Refresh(tk_display,widgetid)
TkDisplay *tk_display;
WidgetID widgetid;
{


 if(widgetid>=0 && widgetid<tk_display->maxwidgets)
 {
   switch(tk_display->widgets[widgetid].class)
   {


	case WI_BUTTON :

	   switch(tk_display->widgets[widgetid].button->type){

		case BN_PUSHBUTTON: 	bn_DrawPushButton(tk_display,widgetid); 
				    	break;

		case BN_REPEATBUTTON: 	bn_DrawRepeatButton(tk_display,widgetid); 
				      	break;

		case BN_CROSSBUTTON: 	XClearWindow(tk_display->display,tk_display->widgets[widgetid].button->window);
					bn_DrawCrossButton(tk_display,widgetid); 
				     	break;

		case BN_POPUPBUTTON: 	bn_DrawPopupButton(tk_display,widgetid);
				     	break;

		case BN_RADIOBUTTON: 	XClearWindow(tk_display->display,tk_display->widgets[widgetid].button->window);
					bn_DrawRadioButton(tk_display,widgetid);
				     	break;

		case BN_CHECKBUTTON: 	XClearWindow(tk_display->display,tk_display->widgets[widgetid].button->window);
					bn_DrawCheckButton(tk_display,widgetid);
				     	break;

		case BN_POPUPRADIOBUTTON: XClearWindow(tk_display->display,tk_display->widgets[widgetid].button->window);
					  bn_DrawPopupRadioButton(tk_display,widgetid);
					  break;

		case BN_SCROLLBUTTON: 	bn_DrawScrollButton(tk_display,widgetid);
					break;

		case BN_THUMBBUTTON: 	bn_DrawThumbButton(tk_display,widgetid);
					break;

		case BN_COMBOBUTTON: 	bn_DrawComboButton(tk_display,widgetid);
					break;

		default: return -1;
			 break;

 	   }
	   return 0;
	   break;



	case WI_SCROLLBAR:

	   return 0;
	   break;



	case WI_EDIT :

	   return ed_DrawEdit(tk_display,widgetid);
	   break;


	case WI_LIST :
	   
	   return ls_DrawList(tk_display,widgetid);
 	   break;



	case WI_COMBO :

	   ed_DrawEdit(tk_display,tk_display->widgets[widgetid].combo->edit);
	   return ls_DrawList(tk_display,tk_display->widgets[widgetid].combo->list);
	   break;


	case WI_MENU :

	   if(tk_display->widgets[widgetid].menu==NULL&&tk_display->widgets[widgetid].menu->type==MN_MENUBAR)
	     return mn_DrawMenuBar(tk_display,widgetid);
	   if(tk_display->widgets[widgetid].menu==NULL&&tk_display->widgets[widgetid].menu->type==MN_FLOATING)
	     return mn_DrawFloatingMenu(tk_display,widgetid);
	   break;


	default: return -1;
		 break;

   }
 }
 return -1;

}



int _WID_RefreshClass(tk_display,class)
TkDisplay *tk_display;
unsigned int class;
{ 
  int i;
  Pixmap right4l, right4g, left4l, left4g, up4l, up4g, down4l, down4g;
  Pixmap upM, downM, rightM, leftM;
  int mask, direction;
  XGCValues xgcvalues;
  GC gc; 



 if(tk_display->maxwidgets>0)
 {
   switch(class)
   {

	case WI_BUTTON :

right4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),right4g_bits,right4g_width,right4g_height); 
 right4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),right4l_bits,right4l_width,right4l_height);
 	left4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),left4l_bits,left4l_width,left4l_height);
 left4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),left4g_bits,left4g_width,left4g_height);
 up4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),up4g_bits,up4g_width,up4g_height); 
 up4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),up4l_bits,up4l_width,up4l_height);
 down4l=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),down4l_bits,down4l_width,down4l_height);
 down4g=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),down4g_bits,down4g_width,down4g_height);


 		upM=tk_display->pixmaps.upM;
 		downM=tk_display->pixmaps.downM;
 		leftM=tk_display->pixmaps.leftM;
 		rightM=tk_display->pixmaps.rightM;

 		XFreePixmap(tk_display->display,tk_display->pixmaps.thumbM);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.thumbMmask);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.upM);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.upMgrayed);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.upMmask);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.downM);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.downMgrayed);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.downMmask); 
 		XFreePixmap(tk_display->display,tk_display->pixmaps.leftM);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.leftMmask);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.leftMgrayed);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.rightMgrayed);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.rightMmask);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.rightM);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.comboMgrayed);
 		XFreePixmap(tk_display->display,tk_display->pixmaps.comboMmask); 
 		XFreePixmap(tk_display->display,tk_display->pixmaps.comboM); 


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

 		XFreePixmap(tk_display->display,up4l);
 		XFreePixmap(tk_display->display,down4l);
 		XFreePixmap(tk_display->display,left4l);
 		XFreePixmap(tk_display->display,right4l);
 		XFreePixmap(tk_display->display,up4g);
 		XFreePixmap(tk_display->display,down4g);
 		XFreePixmap(tk_display->display,left4g);
 		XFreePixmap(tk_display->display,right4g); 


		if(tk_display->numwidgets>0)
		{
		  for(i=0;i<tk_display->maxwidgets;i++)
		  if(tk_display->widgets[i].isUsed==True && tk_display->widgets[i].identity.usePrivateColors==False && tk_display->widgets[i].class==WI_BUTTON) 
		  switch(tk_display->widgets[i].button->type)
		  {
		   case BN_PUSHBUTTON:
					 bn_DrawPushButton(tk_display,i);
				       break;
		   case BN_POPUPBUTTON: 
					  bn_DrawPopupButton(tk_display,i);
				        break;
		   case BN_REPEATBUTTON: 
					   bn_DrawRepeatButton(tk_display,i);
				         break;
		   case BN_CROSSBUTTON: 
					  bn_DrawCrossButton(tk_display,i);
					  wid_Refresh(tk_display,i);
				        break;
		   case BN_RADIOBUTTON:
					  bn_DrawRadioButton(tk_display,i);
					wid_Refresh(tk_display,i);
				   	break;	
		   case BN_POPUPRADIOBUTTON:
					bn_DrawPopupRadioButton(tk_display,i);
					wid_Refresh(tk_display,i);
				      	break;			     
		   case BN_SCROLLBUTTON: 
					if(tk_display->widgets[i].button->pixmap==upM)
					  direction=UP;
					if(tk_display->widgets[i].button->pixmap==downM)
					  direction=DOWN;
					if(tk_display->widgets[i].button->pixmap==leftM)
					  direction=LEFT;
					if(tk_display->widgets[i].button->pixmap==rightM)
					  direction=RIGHT;
					if(direction==UP)
BN_SetPixmap(tk_display,i,tk_display->pixmaps.upM,tk_display->pixmaps.up,CenterBitmap,3,3,11,11,tk_display->depth,YES);
 					if(direction==DOWN) BN_SetPixmap(tk_display,i,tk_display->pixmaps.downM,tk_display->pixmaps.down,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 					if(direction==RIGHT) BN_SetPixmap(tk_display,i,tk_display->pixmaps.rightM,tk_display->pixmaps.right,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 					if(direction==LEFT) BN_SetPixmap(tk_display,i,tk_display->pixmaps.leftM,tk_display->pixmaps.left,CenterBitmap,3,3,11,10,tk_display->depth,YES);

 					if(direction==UP) BN_SetPixmapGrayed(tk_display,i,tk_display->pixmaps.upMgrayed,tk_display->pixmaps.upMmask,CenterBitmap,3,3,11,11,tk_display->depth,YES);
 					if(direction==DOWN) BN_SetPixmapGrayed(tk_display,i,tk_display->pixmaps.downMgrayed,tk_display->pixmaps.downMmask,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 					if(direction==RIGHT) BN_SetPixmapGrayed(tk_display,i,tk_display->pixmaps.rightMgrayed,tk_display->pixmaps.rightMmask,CenterBitmap,3,3,11,10,tk_display->depth,YES);
 					if(direction==LEFT) BN_SetPixmapGrayed(tk_display,i,tk_display->pixmaps.leftMgrayed,tk_display->pixmaps.leftMmask,CenterBitmap,3,3,11,10,tk_display->depth,YES);
					bn_DrawScrollButton(tk_display,i);
				        break;
		   case BN_THUMBBUTTON: 
					BN_SetPixmap(tk_display,i,tk_display->pixmaps.thumbM,tk_display->pixmaps.thumbMmask,CenterBitmap,3,3,11,11,DefaultDepth(tk_display->display,tk_display->screen),YES);
					bn_DrawThumbButton(tk_display,i);
				  	break;

		   case BN_CHECKBUTTON: bn_DrawCheckButton(tk_display,i);
					wid_Refresh(tk_display,i);
				       	break;

		   case BN_COMBOBUTTON: 
						BN_SetPixmap(tk_display,i,tk_display->pixmaps.comboM,tk_display->pixmaps.comboMmask,CenterBitmap,3,3,11,11,tk_display->depth,YES);
 					BN_SetPixmapGrayed(tk_display,i,tk_display->pixmaps.comboMgrayed,tk_display->pixmaps.comboMmask,CenterBitmap,3,3,11,11,tk_display->depth,YES);
					if(tk_display->widgets[tk_display->widgets[i].button->parency_number].combo->hasFocus==True)			_LS_DrawFocus(tk_display,tk_display->widgets[tk_display->widgets[i].button->parency_number].combo->list,ON);
					else
				_LS_DrawFocus(tk_display,tk_display->widgets[tk_display->widgets[i].button->parency_number].combo->list,OFF);
					bn_DrawComboButton(tk_display,i);
					break;
		    }
		  } 
		  break;


  case WI_SCROLLBAR:
		  if(tk_display->maxwidgets>0) for(i=0;i<tk_display->maxwidgets;i++)
		  if(tk_display->widgets[i].isUsed==True && tk_display->widgets[i].identity.usePrivateColors==False && tk_display->widgets[i].class==WI_SCROLLBAR) 
		  {
			  XSetWindowBackground(tk_display->display,tk_display->widgets[i].scroll->mainwindow,tk_display->sb_colors.bg);
			  XSetWindowBackground(tk_display->display,tk_display->widgets[i].scroll->thumbwindow,tk_display->sb_colors.bg);
			  XClearWindow(tk_display->display,tk_display->widgets[i].scroll->thumbwindow);
			  if(tk_display->widgets[i].scroll->parency_class==0)
			  {
			    if(tk_display->widgets[tk_display->widgets[i].scroll->parency_number].list->hasFocus==True)
			      XSetWindowBorder(tk_display->display,tk_display->widgets[i].scroll->mainwindow,tk_display->bn_colors.focus);
			    else
			      XSetWindowBorder(tk_display->display,tk_display->widgets[i].scroll->mainwindow,tk_display->bn_colors.nofocus);
			  }
			  else 
			  {
				if(tk_display->widgets[tk_display->widgets[i].scroll->parency_number].list->hasFocus==True)
				  _LS_DrawFocus(tk_display,tk_display->widgets[i].scroll->parency_number,ON);
				else 
				  _LS_DrawFocus(tk_display,tk_display->widgets[i].scroll->parency_number,OFF);
		  	}
		  }
		  break;

   case WI_EDIT : 
 		  if(tk_display->maxwidgets>0) for(i=0;i<tk_display->maxwidgets;i++)
		  if(tk_display->widgets[i].isUsed==True && tk_display->widgets[i].identity.usePrivateColors==False && tk_display->widgets[i].class==WI_EDIT) 
		  {
  		  	XSetWindowBackground(tk_display->display,tk_display->widgets[i].edit->window,tk_display->ed_colors.bg);
		  	XClearWindow(tk_display->display,tk_display->widgets[i].edit->window);
		  	ed_DrawEdit(tk_display,i);
		  	if(tk_display->widgets[i].edit->hasFocus==True)
		  	  _ED_DrawFocus(tk_display,i,ON);
		  	else _ED_DrawFocus(tk_display,i,OFF); 
		  }
		  break;


  case WI_LIST :  
		  if(tk_display->maxwidgets>0) for(i=0;i<tk_display->maxwidgets;i++)
		  if(tk_display->widgets[i].isUsed==True && tk_display->widgets[i].identity.usePrivateColors==False && tk_display->widgets[i].class==WI_LIST) 
		  {
		  	XSetWindowBackground(tk_display->display,tk_display->widgets[i].list->listwindow,tk_display->ls_colors.bg);
		  	XClearWindow(tk_display->display,tk_display->widgets[i].list->listwindow);
		  	ls_DrawList(tk_display,i);
 		  	if(tk_display->widgets[i].list->hasFocus==True)
		  	  _LS_DrawFocus(tk_display,i,ON);
		  	else _LS_DrawFocus(tk_display,i,OFF);
		  }
		  break;

  case WI_MENU :  
		  if(tk_display->maxwidgets>0) for(i=0;i<tk_display->maxwidgets;i++)
		  if(tk_display->widgets[i].isUsed==True && tk_display->widgets[i].identity.usePrivateColors==False && tk_display->widgets[i].class==WI_MENU) 
		  {
		  	XSetWindowBackground(tk_display->display,tk_display->widgets[i].menu->window,tk_display->mn_colors.bg);
		  	XClearWindow(tk_display->display,tk_display->widgets[i].menu->window);
		  	if(tk_display->widgets[i].menu->type==MN_FLOATING)
		  	  mn_DrawFloatingMenu(tk_display,i);
		  	else if(tk_display->widgets[i].menu->type==MN_MENUBAR)
		  	  mn_DrawMenuBar(tk_display,i);
		  	if(tk_display->widgets[i].menu->hasFocus==True)
		  	  XSetWindowBorder(	tk_display->display,tk_display->widgets[i].menu->window,tk_display->widgets[i].colors->focus);
		  	else 	XSetWindowBorder(tk_display->display,tk_display->widgets[i].menu->window,tk_display->widgets[i].colors->nofocus); 

		  }
		  break;

   }
   return 0;
 }
 return -1;

}



