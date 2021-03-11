/*
 *
 * 	cl_init.c
 * 	initialisation des fenetres de couleur
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


#define NEED_XRM_RESOURCES

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
#include "bm/cdialog.bm"
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
#include "rgb.h"










/*
 *
 * Boucle principale
 *
 *
 */

void CL_Init()
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
 char *str_type, *sptr;




 		/*** Creation de la fenetre de couleurs */


 attrib.background_pixel=tk_display->dlg_colors.bg;
 attrib.border_pixel=tk_display->dlg_colors.border_inactive;
 attrib.cursor=tk_display->cursors.normal;
 attrib.event_mask=ExposureMask;
 mask=CWBackPixel|CWBorderPixel|CWCursor|CWEventMask;

 wm_colors_window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),140,120,525,370,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 XStoreName(tk_display->display,wm_colors_window,"System colors");

 ptr=DIALOG_BOX;
 XChangeProperty(tk_display->display,wm_colors_window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeReplace,(char *)&ptr,1);
 ptr=TitleBar+Border+CloseBox+GroupLeader+Overlapped;
 XChangeProperty(tk_display->display,wm_colors_window, tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeAppend,(char *)&ptr,1);
 XSetTransientForHint(tk_display->display,wm_colors_window,wm_desktop_window);

 wid_attributes.mask=SALighting;
 wid_attributes.lighting=False;
 bn_colors_map=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_colors_window,wm_colors_window,10,302,245,25,&wid_attributes,Grayed);
 wid_text.mask=STText|STFont|STKey|STGravity;
 wid_text.font=tk_display->fonts.helvetica12;
 wid_text.text="Colormap >>>";
 wid_text.key=1;
 wid_text.gravity=CenterText;
 wid_SetTextDecoration(tk_display,bn_colors_map,&wid_text,True);

 wid_attributes.lighting=True;
 bn_colors_ok=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_colors_window,wm_colors_window,10,335,75,25,&wid_attributes,Unpushed);
 wid_text.text="Validate";
 wid_SetTextDecoration(tk_display,bn_colors_ok,&wid_text,True);

 bn_colors_cancel=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_colors_window,wm_colors_window,95,335,75,25,&wid_attributes,Unpushed);
 wid_text.text="Cancel";
 wid_text.key=2;
 wid_SetTextDecoration(tk_display,bn_colors_cancel,&wid_text,True);

 bn_colors_help=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_colors_window,wm_colors_window,180,335,75,25,&wid_attributes,Unpushed+Grayed);
 wid_text.text="Help";
 wid_text.key=1;
 wid_SetTextDecoration(tk_display,bn_colors_help,&wid_text,True);

 wid_attributes.multipleSelection=False;
 wid_attributes.itemheight=15;
 wid_attributes.htype=SB_LEFTALIGN;
 wid_attributes.vtype=SB_BOTTOMALIGN;
 wid_attributes.mask=SAMultipleSelection|SAItemHeight|SAHVType;
 ls_colors_items=wid_Create(tk_display,WI_LIST,LS_RIGHTVSCROLL,wm_colors_window,wm_colors_window,10,40,245,65,&wid_attributes,0);
 wid_attributes.thumbsize=18;
 wid_attributes.mask=SAThumbsize;
 wid_SetAttributes(tk_display,tk_display->widgets[ls_colors_items].list->SBV,&wid_attributes,True);
 ls_colors_rgb_names=wid_Create(tk_display,WI_LIST,LS_RIGHTVSCROLL,wm_colors_window,wm_colors_window,270,40,243,250,&wid_attributes,0);
 for(i=0;i<numrgbnames;i++)
   item_Add(tk_display,ls_colors_rgb_names,0,END,TextFlag,rgb_names[i],0,0,0,True);


 wid_attributes.mask=SALighting|SANeverFocus;
 wid_attributes.lighting=True;
 wid_attributes.neverFocus=True;
 for(i=0;i<9;i++)
    bn_colors_widgets[i]=wid_Create(tk_display,WI_BUTTON,BN_POPUPBUTTON,wm_colors_window,wm_colors_window,9+31*i,5,24,24,&wid_attributes,Unpushed);
 wid_SetState(tk_display,bn_colors_widgets[0],Pushed); 


 
 wid_pixmap.mask=SPPixmap|SPGravity|SPWidth|SPHeight|SPDepth;
 wid_pixmap.pixmap=XCreateBitmapFromData(tk_display->display,wm_colors_window,cdesk_bits,16,16);
 wid_pixmap.width=16;
 wid_pixmap.height=16;
 wid_pixmap.depth=1;
 wid_pixmap.gravity=CenterBitmap;
 wid_SetPixmapDecoration(tk_display,bn_colors_widgets[0],&wid_pixmap,True);

 wid_pixmap.pixmap=XCreateBitmapFromData(tk_display->display,wm_colors_window,cwin_bits,16,16);
 wid_SetPixmapDecoration(tk_display,bn_colors_widgets[1],&wid_pixmap,True);

 wid_pixmap.pixmap=XCreateBitmapFromData(tk_display->display,wm_colors_window,cdialog_bits,16,16);
 wid_SetPixmapDecoration(tk_display,bn_colors_widgets[2],&wid_pixmap,True);

 wid_pixmap.pixmap=XCreateBitmapFromData(tk_display->display,wm_colors_window,cicon_bits,16,16);
 wid_SetPixmapDecoration(tk_display,bn_colors_widgets[3],&wid_pixmap,YES);

 wid_pixmap.pixmap=XCreateBitmapFromData(tk_display->display,wm_colors_window,cbutton_bits,16,16);
 wid_SetPixmapDecoration(tk_display,bn_colors_widgets[4],&wid_pixmap,YES);

 wid_pixmap.pixmap=XCreateBitmapFromData(tk_display->display,wm_colors_window,cscroll_bits,16,16);
 wid_SetPixmapDecoration(tk_display,bn_colors_widgets[5],&wid_pixmap,YES);

 wid_pixmap.pixmap=XCreateBitmapFromData(tk_display->display,wm_colors_window,cedit_bits,16,16);
 wid_SetPixmapDecoration(tk_display,bn_colors_widgets[6],&wid_pixmap,YES);

 wid_pixmap.pixmap=XCreateBitmapFromData(tk_display->display,wm_colors_window,clist_bits,16,16);
 wid_SetPixmapDecoration(tk_display,bn_colors_widgets[7],&wid_pixmap,YES);

 wid_pixmap.pixmap=XCreateBitmapFromData(tk_display->display,wm_colors_window,cmenu_bits,16,16);
 wid_SetPixmapDecoration(tk_display,bn_colors_widgets[8],&wid_pixmap,YES);



	  		/*** Creation des fenetres de dessin des widgets ***/

 attrib.background_pixel=wm_info.desk_bg;
 attrib.border_pixel=tk_display->win_colors.text;
 attrib.cursor=tk_display->cursors.normal;
 attrib.event_mask=ExposureMask;
 mask=CWBackPixel|CWBorderPixel|CWCursor|CWEventMask;

 color_desktop=XCreateWindow(tk_display->display,wm_colors_window,10,120,246,170,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 color_window=XCreateWindow(tk_display->display,wm_colors_window,10,120,246,170,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 color_icon=XCreateWindow(tk_display->display,wm_colors_window,10,120,246,170,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 color_menu=XCreateWindow(tk_display->display,wm_colors_window,10,120,246,170,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 color_dialog=XCreateWindow(tk_display->display,wm_colors_window,10,120,246,170,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);

 attrib.background_pixel=tk_display->win_colors.bg; 
 color_button=XCreateWindow(tk_display->display,wm_colors_window,10,120,246,170,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 color_list=XCreateWindow(tk_display->display,wm_colors_window,10,120,246,170,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 color_scroll=XCreateWindow(tk_display->display,wm_colors_window,10,120,246,170,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 color_edit=XCreateWindow(tk_display->display,wm_colors_window,10,120,246,170,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);

 attrib.background_pixel=tk_display->dlg_colors.bg;
 color_rgb=XCreateWindow(tk_display->display,wm_colors_window,270,302,243,58,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 win_Map(tk_display,color_rgb);

			/**** Affichage ****/

 wid_Map(tk_display,bn_colors_map);
 wid_Map(tk_display,bn_colors_ok);
 wid_Map(tk_display,bn_colors_cancel);
 wid_Map(tk_display,bn_colors_help);
 for(i=0;i<9;i++)
   wid_Map(tk_display,bn_colors_widgets[i]);

/* SB_SetThumbsize(tk_display,&ls_colors_items.SBV,23);*/
 wid_Map(tk_display,ls_colors_items);
 wid_Map(tk_display,ls_colors_rgb_names);

 for(i=0;i<2;i++)
   item_Add(tk_display,ls_colors_items,0,END,TextFlag,colors_desktop_text[i],0,0,0,True);



 attrib.background_pixel=tk_display->dlg_colors.bg;
 attrib.border_pixel=tk_display->dlg_colors.border_inactive;
 attrib.cursor=tk_display->cursors.normal;
 attrib.event_mask=ExposureMask;
 mask=CWBackPixel|CWBorderPixel|CWCursor|CWEventMask;


 mask=GCForeground|GCBackground;
 xgcvalues.foreground=BlackPixel(tk_display->display,tk_display->screen);
 xgcvalues.background=tk_display->dlg_colors.bg;
 colors_gc=XCreateGC(tk_display->display,wm_colors_window, mask, &xgcvalues);




}



