/*
 *
 * 	tk_set.c  
 * 	modification du toolkit
 *
 * 	Modification :  31/01/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
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
 *      IMAN Development Toolkit version 1.1.a
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









/*
 * Rafraichir/Obtenir la connection avec le WM 
 *
 */

int tk_SetSystemColors(tk_display,widget_class,wid_colors,delay)
TkDisplay *tk_display;
unsigned int widget_class;
WidgetColors *wid_colors;
Bool delay;
{
  int i;
  WidgetColors *colorsptr;

  if(tk_GetConnection(tk_display)==True)
    return -1;
  if(wid_colors==(WidgetColors *)NULL)
    return -2;

  switch(widget_class)
  {	
	case WI_BUTTON :
		colorsptr=&tk_display->bn_colors;
		_TK_CopyColors(colorsptr,wid_colors);
		if(delay==False)
		  _WID_RefreshClass(tk_display,WI_BUTTON);
		return 0;
		break;


	case WI_EDIT :
		memcpy(&tk_display->ed_colors,wid_colors,sizeof(WidgetColors));
		if(delay==False)
		  _WID_RefreshClass(tk_display,WI_EDIT);
		return 0;
		break;

	case  WI_SCROLLBAR :
		memcpy(&tk_display->sb_colors,wid_colors,sizeof(WidgetColors));
		if(delay==False)
		  _WID_RefreshClass(tk_display,WI_SCROLLBAR);
		return 0;
		break;

	case  WI_LIST :
		memcpy(&tk_display->ls_colors,wid_colors,sizeof(WidgetColors));
		if(delay==False)
		  _WID_RefreshClass(tk_display,WI_LIST);
		return 0;
		break;

	case  WI_COMBO :
		memcpy(&tk_display->cb_colors,wid_colors,sizeof(WidgetColors));
		if(delay==False)
		  _WID_RefreshClass(tk_display,WI_COMBO);
		return 0;
		break;

	case  WI_MENU :
		memcpy(&tk_display->mn_colors,wid_colors,sizeof(WidgetColors));
		if(delay==False)
		  _WID_RefreshClass(tk_display,WI_MENU);
		return 0;
		break;

	default :
		return -1;
		break;


  }

}



int _TK_CopyColors(dest,src)
WidgetColors *src, *dest;
{
  dest->bg=src->bg;
  dest->fg=src->fg;
  dest->light=src->light;
  dest->shadow=src->shadow;
  dest->text=src->text;
  dest->text_grayed=src->text_grayed;
  dest->focus=src->focus;
  dest->nofocus=src->nofocus;
  dest->cross=src->cross;
  dest->check=src->check;
  dest->cursor=src->cursor;
  dest->radio_bg=src->radio_bg;
  dest->radio_light=src->radio_light;
  dest->selected=src->selected;
  dest->selected_inactive=src->selected_inactive;
  dest->text_selected=src->text_selected;
  dest->text_selected_inactive=src->text_selected_inactive;
  dest->text_grayed_selected=src->text_grayed_selected;
  dest->text_grayed_selected_inactive=src->text_grayed_selected_inactive;

  return 0;
}
