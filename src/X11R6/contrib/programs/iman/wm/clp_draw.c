/*
 *
 * 	clip_draw.c
 * 	affichage du clipboard
 *
 * 	Modification :  18/12/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
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

int CLIP_Draw()
{
 int mask;
 int ret;
 int i, j, k, w, h;

 /*fprintf(stderr,"CLIP_Draw\n");*/

 XSetBackground(tk_display->display,clipboard_gc,tk_display->win_colors.bg);
 XSetForeground(tk_display->display,clipboard_gc,tk_display->win_colors.bg);
 if(clip_info.depth==tk_display->depth)
   XCopyArea(tk_display->display,clip_info.pixmap,wm_clipboard_window,clipboard_gc,tk_event.event.xexpose.x,tk_event.event.xexpose.y,tk_event.event.xexpose.width,tk_event.event.xexpose.height,tk_event.event.xexpose.x,tk_event.event.xexpose.y);
 else if(clip_info.depth==1)
    XCopyPlane(tk_display->display,clip_info.pixmap,wm_clipboard_window,clipboard_gc,tk_event.event.xexpose.x,tk_event.event.xexpose.y,tk_event.event.xexpose.width,tk_event.event.xexpose.height,tk_event.event.xexpose.x,tk_event.event.xexpose.y,1);

 return 0;
}




