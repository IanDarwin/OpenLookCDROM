/*
 *
 * 	set_init.c
 * 	initialisation de la fenetre de parametrage
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





#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>

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
 * Boucle principale
 *
 *
 */

void SET_Init()
{
 WidgetAttributes wid_attributes;
 WidgetTextDecoration wid_text;

 XSetWindowAttributes attrib;
 XGCValues xgcvalues;
 XWMHints wmhints;

 unsigned long ptr;
 int mask,state;
 int ret;
 int i, j, k, w;
 unsigned long win_type;





	     /*** Creation de la fenetre de configuration ***/

 attrib.background_pixel=tk_display->dlg_colors.bg;
 attrib.border_pixel=tk_display->dlg_colors.border_inactive;
 attrib.cursor=tk_display->cursors.normal;
 attrib.event_mask=ExposureMask;
 mask=CWBackPixel|CWBorderPixel|CWCursor|CWEventMask;

 wm_setup_window=XCreateWindow(tk_display->display,RootWindow(tk_display->display,tk_display->screen),(DisplayWidth(tk_display->display,0)-270-15),(DisplayHeight(tk_display->display,0)-450-40),270,280,1,tk_display->depth,InputOutput,CopyFromParent,mask,&attrib);
 XStoreName(tk_display->display,wm_setup_window,"Options");

 mask=GCForeground|GCBackground;
 xgcvalues.foreground=BlackPixel(tk_display->display,tk_display->screen);
 xgcvalues.background=tk_display->dlg_colors.bg;
 setup_gc=XCreateGC(tk_display->display,wm_setup_window, mask, &xgcvalues);


 wid_attributes.mask=SALighting;
 wid_attributes.lighting=True;
 bn_setup_ok=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_setup_window,wm_setup_window,50,240,80,25,&wid_attributes,Unpushed);
 wid_text.mask=STText|STFont|STKey|STGravity;
 wid_text.font=tk_display->fonts.helvetica12;
 wid_text.text="Validate";
 wid_text.key=1;
 wid_text.gravity=CenterText;
 wid_SetTextDecoration(tk_display,bn_setup_ok,&wid_text,True);

 bn_setup_cancel=wid_Create(tk_display,WI_BUTTON,BN_PUSHBUTTON,wm_setup_window,wm_setup_window,140,240,80,25,&wid_attributes,Unpushed);
 wid_text.text="Cancel";
 wid_SetTextDecoration(tk_display,bn_setup_cancel,&wid_text,True);

/* if(wm_info.set_decoration==True) state=Pushed;
 else state=Unpushed;*/
 state=Pushed+Grayed;
 wid_attributes.mask=SALighting|SACrossType;
 wid_attributes.crosstype=LittleCross;
 bn_setup_decoration=wid_Create(tk_display,WI_BUTTON,BN_CROSSBUTTON,wm_setup_window,wm_setup_window,18,20,200,25,&wid_attributes,state);
 wid_text.text="Extended windowing";
 wid_SetTextDecoration(tk_display,bn_setup_decoration,&wid_text,True);

 if(wm_info.set_groups==True) state=Pushed;
 else state=Unpushed;
 bn_setup_groups=wid_Create(tk_display,WI_BUTTON,BN_CROSSBUTTON,wm_setup_window,wm_setup_window,18,45,220,25,&wid_attributes,Grayed);
 wid_text.text="Stealth reparenting capture";
 wid_SetTextDecoration(tk_display,bn_setup_groups,&wid_text,True);

 if(wm_info.set_icons==True) state=Pushed;
 else state=Unpushed;
 bn_setup_icons=wid_Create(tk_display,WI_BUTTON,BN_CROSSBUTTON,wm_setup_window,wm_setup_window,18,70,165,25,&wid_attributes,state);
 wid_text.text="Show icons";
 wid_SetTextDecoration(tk_display,bn_setup_icons,&wid_text,True);

 if(wm_info.set_icontitle==True) state=Pushed;
 else state=Unpushed;
 bn_setup_icontitle=wid_Create(tk_display,WI_BUTTON,BN_CROSSBUTTON,wm_setup_window,wm_setup_window,18,95,205,25,&wid_attributes,state);
 wid_text.text="Show icon titles";
 wid_SetTextDecoration(tk_display,bn_setup_icontitle,&wid_text,True);

/* if(wm_info.set_helpactive==True) state=Pushed;
 else state=Unpushed;*/
 state=Grayed;
 bn_setup_helpactive=wid_Create(tk_display,WI_BUTTON,BN_CROSSBUTTON,wm_setup_window,wm_setup_window,18,120,170,25,&wid_attributes,state);
 wid_text.text="Help Server running";
 wid_SetTextDecoration(tk_display,bn_setup_helpactive,&wid_text,True);

/* if(wm_info.set_debug==True) state=Pushed;
 else state=Unpushed;*/
 state=Grayed;
 bn_setup_debug=wid_Create(tk_display,WI_BUTTON,BN_CROSSBUTTON,wm_setup_window,wm_setup_window,18,145,150,25,&wid_attributes,state);
 wid_text.text="Debug mode";
 wid_SetTextDecoration(tk_display,bn_setup_debug,&wid_text,True);
												

 wid_Map(tk_display,bn_setup_ok);
 wid_Map(tk_display,bn_setup_cancel);
 wid_Map(tk_display,bn_setup_decoration);
 wid_Map(tk_display,bn_setup_groups);
 wid_Map(tk_display,bn_setup_icons);
 wid_Map(tk_display,bn_setup_icontitle);
 wid_Map(tk_display,bn_setup_helpactive);
 wid_Map(tk_display,bn_setup_debug);

 ptr=DIALOG_BOX;
 XChangeProperty(tk_display->display,wm_setup_window,tk_display->atoms._IMAN_WM_TYPE, XA_INTEGER,32, PropModeReplace,(char *)&ptr,1);
 ptr=TitleBar+Border+CloseBox;
 XChangeProperty(tk_display->display,wm_setup_window, tk_display->atoms._IMAN_WM_TYPE,XA_INTEGER,32, PropModeAppend,(char *)&ptr,1);

}




