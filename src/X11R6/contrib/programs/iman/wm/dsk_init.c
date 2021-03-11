/*
 *
 * 	dsk_init.c
 * 	initialisation du bureau
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




#define DSK_INIT_C
#define NEED_XRM_RESOURCES

/*#define DEBUG 1*/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include "bm/cdesk.bm"
#include "bm/cwin.bm"
#include "bm/cicon.bm"
#include "bm/cbutton.bm"
#include "bm/cedit.bm"
#include "bm/cscroll.bm"
#include "bm/clist.bm"
#include "bm/cmenu.bm"

#include "bm/mnone.bm"
#include "bm/mgray5.bm"
#include "bm/mgray2.bm"
#include "bm/mgray7.bm"
#include "bm/mbric.bm"
#include "bm/mlinesv.bm"
#include "bm/mlinesh.bm"
#include "bm/mcadr.bm"
#include "bm/mpyr.bm"
#include "bm/xlogo16.bm"
#include "bm/xlogo32.bm"
#include "bm/xlogo64.bm"

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"










/*
 *
 * Boucle principale
 *
 *
 */

void DSK_Init()
{
 WidgetAttributes wid_attributes;
 WidgetTextDecoration wid_text;
 WidgetPixmapDecoration wid_pixmap;
 ItemPixmapDecoration item_pixmap;
 ItemTextDecoration item_text;

 XSetWindowAttributes attrib;
 XGCValues xgcvalues;
 XWMHints wmhints;

 unsigned long ptr;
 int mask;
 int ret;
 int i, j, k, w, h;
 unsigned long win_type;
 Pixmap pix;



	     /*** Creation de la fenetre de bureau ***/

 attrib.background_pixel=tk_display->dlg_colors.bg;
 attrib.border_pixel=tk_display->win_colors.border_inactive;
 attrib.cursor=tk_display->cursors.normal;
 attrib.event_mask=ExposureMask;
 mask=CWBackPixel|CWBorderPixel|CWCursor|CWEventMask;

 wm_desktop_window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),(DisplayWidth(tk_display->display,0)-DESKTOP_BOX_WIDTH-15),(DisplayHeight(tk_display->display,0)-DESKTOP_BOX_HEIGHT-40),DESKTOP_BOX_WIDTH,DESKTOP_BOX_HEIGHT,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 XStoreName(tk_display->display,wm_desktop_window,"Desktop panel");																						

 attrib.border_pixel=tk_display->dlg_colors.text;
 desktop_motif_window=XCreateWindow(tk_display->display,wm_desktop_window,10,7,240,80,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 desktop_defaulticon_window=XCreateWindow(tk_display->display,wm_desktop_window,10,93,240,50,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 desktop_screensaver_window=XCreateWindow(tk_display->display,wm_desktop_window,10,150,370,90,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 desktop_paper_window=XCreateWindow(tk_display->display,wm_desktop_window,10,250,280,80,0,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);

#ifdef DEBUG
 fprintf(stderr,"DSK: Fenetres creees\n");
#endif

 mask=GCForeground|GCBackground;
 xgcvalues.foreground=BlackPixel(tk_display->display,tk_display->screen);
 xgcvalues.background=tk_display->dlg_colors.bg;
 desktop_gc=XCreateGC(tk_display->display,wm_desktop_window, mask, &xgcvalues);

#ifdef DEBUG
 fprintf(stderr,"DSK: GC cree\n");
#endif

 wid_attributes.mask=SALighting;
 wid_attributes.lighting=True;
 bn_desktop_ok=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_desktop_window,wm_desktop_window,ERROR_BOX_WIDTH-100,10,90,25,&wid_attributes,Unpushed);
#ifdef DEBUG
 fprintf(stderr,"DSK: BN 1 cree\n");
#endif
 wid_text.mask=STText|STFont|STKey|STGravity;
 wid_text.font=tk_display->fonts.helvetica12;
 wid_text.text="Validate";
 wid_text.key=1;
 wid_text.gravity=CenterText;
 wid_SetTextDecoration(tk_display,bn_desktop_ok,&wid_text,True);


#ifdef DEBUG
 fprintf(stderr,"DSK: BN 1 redecore\n");
#endif

 bn_desktop_cancel=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_desktop_window,wm_desktop_window,ERROR_BOX_WIDTH-100,42,90,25,&wid_attributes,Unpushed);
 wid_text.text="Cancel";
 wid_text.key=2;
#ifdef DEBUG
 fprintf(stderr,"DSK: BN 2 cree\n");
#endif
 wid_SetTextDecoration(tk_display,bn_desktop_cancel,&wid_text,True);

#ifdef DEBUG
 fprintf(stderr,"DSK: BN 2 redecore\n");
#endif

 bn_desktop_about=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_desktop_window,wm_desktop_window,ERROR_BOX_WIDTH-100,74,90,25,&wid_attributes,Unpushed);
#ifdef DEBUG
 fprintf(stderr,"DSK: BN 3 cree\n");
#endif
 wid_text.text="About";
 wid_text.key=3;
 wid_SetTextDecoration(tk_display,bn_desktop_about,&wid_text,True);
#ifdef DEBUG
 fprintf(stderr,"DSK: BN 3 redecore\n");
#endif

 bn_desktop_help=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_desktop_window,wm_desktop_window,ERROR_BOX_WIDTH-100,106,90,25,&wid_attributes,Grayed);
#ifdef DEBUG
 fprintf(stderr,"DSK: BN 4 cree\n");
#endif
 wid_text.text="Help";
 wid_text.key=1;
 wid_SetTextDecoration(tk_display,bn_desktop_help,&wid_text,True);
#ifdef DEBUG
 fprintf(stderr,"DSK: BN 4 redecore\n");
#endif

 bn_desktop_colors=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_desktop_window,wm_desktop_window,ERROR_BOX_WIDTH-100,268,90,25,&wid_attributes,Unpushed);
 wid_text.text="Colors";
 wid_text.key=2;
 wid_SetTextDecoration(tk_display,bn_desktop_colors,&wid_text,True);

#ifdef DEBUG
 fprintf(stderr,"DSK: BN 5 redecore\n");
#endif

 bn_desktop_keyboard=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_desktop_window,wm_desktop_window,ERROR_BOX_WIDTH-100,300,90,25,&wid_attributes,Unpushed);
 wid_text.text="Keyboard";
 wid_text.key=1;
 wid_SetTextDecoration(tk_display,bn_desktop_keyboard,&wid_text,True);

 bn_desktop_clipboard=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_desktop_window,wm_desktop_window,ERROR_BOX_WIDTH-100,300,90,25,&wid_attributes,Unpushed);
 wid_text.text="Clipboard";
 wid_text.key=1;
 wid_SetTextDecoration(tk_display,bn_desktop_clipboard,&wid_text,True);


#ifdef DEBUG
 fprintf(stderr,"DSK: BN crees\n");
#endif

 wid_attributes.lighting=False;
 wid_attributes.multipleSelection=False;
 wid_attributes.itemheight=14;
 wid_attributes.mask=SALighting|SAMultipleSelection|SAItemHeight;
 cb_desktop_motif=wid_Create(tk_display,WI_COMBO,CB_RIGHTNOEDITION,desktop_motif_window,wm_desktop_window,58,20,170,110,&wid_attributes,0);
 wid_text.text="(None)";
 wid_text.key=0;
 wid_SetTextDecoration(tk_display,cb_desktop_motif,&wid_text,True);

 bn_desktop_motif=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,desktop_motif_window,wm_desktop_window,80,48,80,25,&wid_attributes,Unpushed+Grayed);
 wid_text.text="Modify";
 wid_text.key=1;
 wid_SetTextDecoration(tk_display,bn_desktop_motif,&wid_text,True);

 cb_desktop_defaulticon=wid_Create(tk_display,WI_COMBO,CB_RIGHTNOEDITION,desktop_defaulticon_window,wm_desktop_window,58,20,170,160,&wid_attributes,0);
 wid_text.text="(None)";
 wid_text.key=0;
 wid_SetTextDecoration(tk_display,cb_desktop_defaulticon,&wid_text,True);

 cb_desktop_screensaver=wid_Create(tk_display,WI_COMBO,CB_RIGHTNOEDITION,desktop_screensaver_window,wm_desktop_window,158,20,190,100,&wid_attributes,0);
 wid_text.text="(None)";
 wid_text.key=0;
 wid_SetTextDecoration(tk_display,cb_desktop_screensaver,&wid_text,True);

#ifdef DEBUG
 fprintf(stderr,"DSK: CB crees\n");
#endif

 bn_desktop_screensaver_test=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,desktop_screensaver_window,wm_desktop_window,10,20,80,25,&wid_attributes,Unpushed);
 wid_text.text="Test";
 wid_text.key=1;
 wid_SetTextDecoration(tk_display,bn_desktop_screensaver_test,&wid_text,True);

 bn_desktop_screensaver_install=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,desktop_screensaver_window,wm_desktop_window,10,54,80,25,&wid_attributes,Grayed);
 wid_text.text="Installation";
 wid_text.key=1;
 wid_SetTextDecoration(tk_display,bn_desktop_screensaver_install,&wid_text,True);

 ed_desktop_screensaver=wid_Create(tk_display,WI_EDIT,ED_FULLSELECT,desktop_screensaver_window,wm_desktop_window,210,50,40,20,&wid_attributes,0);
 wid_text.text="10";
 wid_text.key=0;
 wid_text.font=tk_display->fonts.f8_13;
 wid_SetTextDecoration(tk_display,ed_desktop_screensaver,&wid_text,True);

 cb_desktop_paper=wid_Create(tk_display,WI_COMBO,CB_RIGHTNOEDITION,desktop_paper_window,wm_desktop_window,65,20,190,75,&wid_attributes,0);
 wid_text.text="(None)";
 wid_text.key=0;
 wid_text.font=tk_display->fonts.helvetica12;
 wid_SetTextDecoration(tk_display,cb_desktop_paper,&wid_text,True);

 wid_attributes.crosstype=LittleCross;
 wid_attributes.mask=wid_attributes.mask+SACrossType;
 bn_paper_center=wid_Create(tk_display,WI_BUTTON,BN_CROSSBUTTON,desktop_paper_window,wm_desktop_window,10,50,100,25,&wid_attributes,Unpushed);
 wid_text.text="Center";
 wid_text.key=0;
 wid_SetTextDecoration(tk_display,bn_paper_center,&wid_text,True);

 bn_paper_mos=wid_Create(tk_display,WI_BUTTON,BN_CROSSBUTTON,desktop_paper_window,wm_desktop_window,145,50,100,25,&wid_attributes,Pushed);
 wid_text.text="Mosaic";
 wid_text.key=0;
 wid_SetTextDecoration(tk_display,bn_paper_mos,&wid_text,True);

#ifdef DEBUG
  fprintf(stderr,"DSK cree\n");
#endif
 wid_Map(tk_display,bn_desktop_ok);
 wid_Map(tk_display,bn_desktop_cancel);
 wid_Map(tk_display,bn_desktop_help);
 wid_Map(tk_display,bn_desktop_about);
 wid_Map(tk_display,bn_desktop_colors);
 wid_Map(tk_display,bn_desktop_clipboard);
 wid_Map(tk_display,cb_desktop_motif);
 wid_Map(tk_display,bn_desktop_motif);
 wid_Map(tk_display,cb_desktop_defaulticon);
 wid_Map(tk_display,cb_desktop_screensaver);
 wid_Map(tk_display,bn_desktop_screensaver_test);
 wid_Map(tk_display,bn_desktop_screensaver_install);
 wid_Map(tk_display,ed_desktop_screensaver);
 wid_Map(tk_display,bn_paper_center);
 wid_Map(tk_display,bn_paper_mos);
 wid_Map(tk_display,cb_desktop_paper);

 XMapWindow(tk_display->display,desktop_motif_window);
 XMapWindow(tk_display->display,desktop_defaulticon_window);
 XMapWindow(tk_display->display,desktop_screensaver_window);
 XMapWindow(tk_display->display,desktop_paper_window);

#ifdef DEBUG
  fprintf(stderr,"DSK mappe\n");
#endif

 ptr=DIALOG_BOX;
 XChangeProperty(tk_display->display,wm_desktop_window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeReplace,(char *)&ptr,1);
 ptr=TitleBar+Border+CloseBox;
 XChangeProperty(tk_display->display,wm_desktop_window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeAppend,(char *)&ptr,1);

#ifdef DEBUG
  fprintf(stderr,"DSK properties passees\n");
#endif


 ret=XQueryBestStipple(tk_display->display,RootWindow(tk_display->display,tk_display->screen),16,16,&w,&h);
 if(ret!=0){ 
	 /*fprintf(stderr,"Best stipple: width=%d  height=%d\n",w,h);*/
	 wm_info.desk_pixmap=XCreatePixmap(tk_display->display,RootWindow(tk_display->display,tk_display->screen),w,h,tk_display->depth);
	}
 else wm_info.desk_pixmap=XCreatePixmap(tk_display->display,RootWindow(tk_display->display,tk_display->screen),10,10,tk_display->depth);


#ifdef DEBUG
  fprintf(stderr,"DSK dsk_pixmap passe\n");
#endif

 pix_motifs[0]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),mnone_bits,mnone_width,mnone_height);
 item_Add(tk_display,cb_desktop_motif,0,END,TextFlag,"(None)",0,0,0,True);
 item_pixmap.mask=SPPixmap|SPWidth|SPHeight|SPDepth|SPGravity;
 item_pixmap.pixmap=pix_motifs[0];
 item_pixmap.width=16;
 item_pixmap.height=12;
 item_pixmap.depth=1;

 pix_motifs[1]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),mgray7_bits,mgray7_width,mgray7_height);
 item_Add(tk_display,cb_desktop_motif,0,END,PixmapFlag+TextFlag,"Grey 75%",0,0,0,True);	

 pix_motifs[2]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),mgray5_bits,mgray5_width,mgray5_height);
 item_Add(tk_display,cb_desktop_motif,0,END,PixmapFlag+TextFlag,"Grey 50%",0,0,0,True);	

 pix_motifs[3]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),mgray2_bits,mgray2_width,mgray2_height);
 item_Add(tk_display,cb_desktop_motif,0,END,PixmapFlag+TextFlag,"Grey 25%",0,0,0,True);

 pix_motifs[4]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),mpyr_bits,mpyr_width,mpyr_height);
 item_Add(tk_display,cb_desktop_motif,0,END,TextFlag+PixmapFlag,"Pyramid",0,0,0,True);  

 pix_motifs[5]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),mbric_bits,mbric_width,mbric_height);
 item_Add(tk_display,cb_desktop_motif,0,END,PixmapFlag+TextFlag,"Brik",0,0,0,True);

 pix_motifs[6]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),mlinesv_bits,mlinesv_width,mlinesv_height);
 item_Add(tk_display,cb_desktop_motif,0,END,PixmapFlag+TextFlag,"V lines",0,0,0,True);

 pix_motifs[7]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),mlinesh_bits,mlinesh_width,mlinesh_height);
 item_Add(tk_display,cb_desktop_motif,0,END,PixmapFlag+TextFlag,"H lines",0,0,0,True);

 pix_motifs[8]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),mcadr_bits,mcadr_width,mcadr_height);
 item_Add(tk_display,cb_desktop_motif,0,END,PixmapFlag+TextFlag,"H+V lines",0,0,0,True);

 pix_motifs[9]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),xlogo16_bits,xlogo16_width,xlogo16_height);
 item_Add(tk_display,cb_desktop_motif,0,END,PixmapFlag+TextFlag,"X logo (16)",0,0,0,True);

 pix_motifs[10]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),xlogo32_bits,xlogo32_width,xlogo32_height);
 item_Add(tk_display,cb_desktop_motif,0,END,PixmapFlag+TextFlag,"X logo (32)",0,0,0,True);

 pix_motifs[11]=XCreateBitmapFromData(tk_display->display,RootWindow(tk_display->display,tk_display->screen),xlogo64_bits,xlogo64_width,xlogo64_height);
 item_Add(tk_display,cb_desktop_motif,0,END,PixmapFlag+TextFlag,"X logo (64)",0,0,0,True);

 for(i=0;i<12;i++)
 {
   item_pixmap.pixmap=pix_motifs[i];
   item_SetPixmapDecoration(tk_display,cb_desktop_motif,i,&item_pixmap,True);
   item_SetPrecedency(tk_display,cb_desktop_motif,i,PixmapFlag,True);
 }

 numpixmotifs=12;

#ifdef DEBUG
  fprintf(stderr,"DSK pixmaps crees\n");
#endif

 wid_attributes.mask=SAPosition;
 wid_attributes.position=0;
 wid_SetAttributes(tk_display,cb_desktop_motif,&wid_attributes,True);
#ifdef DEBUG
 fprintf(stderr,"Attributes changes\n");
#endif
 item_Add(tk_display,cb_desktop_screensaver,0,END,TextFlag,"(None)",0,0,0,True);
 item_Add(tk_display,cb_desktop_screensaver,0,END,TextFlag,"X Mechanism",0,0,0,True);
 wid_SetAttributes(tk_display,cb_desktop_screensaver,&wid_attributes,True);
#ifdef DEBUG
 fprintf(stderr,"Item rajoute\n");
#endif
 item_Add(tk_display,cb_desktop_paper,0,END,TextFlag,"(None)",0,0,0,True);
 wid_SetAttributes(tk_display,cb_desktop_paper,&wid_attributes,True);
#ifdef DEBUG
 fprintf(stderr,"Item rajoute\n");
#endif
 item_Add(tk_display,cb_desktop_defaulticon,0,END,TextFlag,"(None)",0,0,0,True);
 item_Add(tk_display,cb_desktop_defaulticon,0,END,PixmapFlag+TextFlag,"X logo (64)",0,0,0,True);
 if(wm_info.dsk_defaulticon<2)
   wid_attributes.position=wm_info.dsk_defaulticon;
 else wid_attributes.position=0;
 wid_SetAttributes(tk_display,cb_desktop_defaulticon,&wid_attributes,True);
 wid_attributes.position=0;
#ifdef DEBUG
 fprintf(stderr,"Item rajoute\n");
#endif
#ifdef DEBUG
 fprintf(stderr,"DSK set position passe\n");
#endif

		/*** Creation de la fenetre de couleurs ***/
 DSK_GetPreferences();

#ifdef DEBUG
  fprintf(stderr,"DSK get preferences passes\n");
#endif

 if((wm_fonts.times_big=XLoadQueryFont(tk_display->display,"-*-times-bold-r-normal--34-240-100-100-*-*-*-*"))==NULL)
 {
   /*fprintf(stderr,"**** Avertissement: Impossible d'utiliser la police times_big\n");*/
   wm_fonts.times_big=tk_display->fonts.helvetica12;
 }
 if((wm_fonts.venice_28=XLoadQueryFont(tk_display->display,"venice-28"))==NULL)
 {
   /*fprintf(stderr,"**** Avertissement: Impossible d'utiliser la police venice-28\n");*/
   wm_fonts.venice_28=tk_display->fonts.helvetica12;
 }

 CL_Init();
 CLIP_Init();


#ifdef DEBUG
  fprintf(stderr,"CL init passe\n");
#endif

		/*** Creation de la fenetre 'about' ***/


 AB_Init();



}





