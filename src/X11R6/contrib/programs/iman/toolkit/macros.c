/*
 *
 * 	macros.c  
 * 	macros diverses
 *
 * 	Modification :  19/04/94
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

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"
#include "X11/Xatom.h"

#include <X11/iman/widgets.h>




Display *mGetDisplay(tk_display)
TkDisplay *tk_display;
{
  if(tk_display!=NULL)
    return (Display *)tk_display->display;
  else return (Display *)NULL;
}




char *mGetVendorString(tk_display)
TkDisplay *tk_display;
{
  if(tk_display!=NULL)
    return (char *)tk_display->infos.vendor;
  else return (char *)NULL;
}




int mGetTkVersion(tk_display)
TkDisplay *tk_display;
{
  if(tk_display!=NULL)
    return (int)tk_display->infos.version;
  else return -1;
}



int mGetTkRelease(tk_display)
TkDisplay *tk_display;
{
  if(tk_display!=NULL)
    return (int)tk_display->infos.release;
  else return -1;
}





char *mGetTkComment(tk_display)
TkDisplay *tk_display;
{
  if(tk_display!=NULL)
    return (char *)tk_display->infos.comment;
  else return (char *)NULL;
}




int mGetWmVersion(tk_display)
TkDisplay *tk_display;
{
  if(tk_display!=NULL)
    return (int)tk_display->wm.version;
  else return -1;
}



int mGetWmRelease(tk_display)
TkDisplay *tk_display;
{
  if(tk_display!=NULL)
    return (int)tk_display->wm.release;
  else return -1;
}



char *mGetWmComment(tk_display)
TkDisplay *tk_display;
{
  if(tk_display!=NULL)
    return (char *)tk_display->wm.comment;
  else return (char *)NULL;
}




Window mGetWmMainWindow(tk_display)
TkDisplay *tk_display;
{
  if(tk_display!=NULL)
    return (Window)tk_display->wm.main_window;
  else return (Window)NULL;
}



Bool mIsWmActive(tk_display)
TkDisplay *tk_display;
{
  if(tk_display!=NULL)
    return (int)tk_display->wm.active;
  else return -1;
}



int mGetDefaultDepth(tk_display)
TkDisplay *tk_display;
{
  if(tk_display!=NULL)
    return (int)tk_display->depth;
  else return -1;
}



int mGetHsVersion(tk_display)
TkDisplay *tk_display;
{
 help_GetConnection(tk_display);
  if(tk_display!=NULL)
    return (int)tk_display->hs.version;
  else return -1;
}



Window mGetHsMainWindow(tk_display)
TkDisplay *tk_display;
{
 help_GetConnection(tk_display);
 if(tk_display!=NULL)
    return (Window)tk_display->hs.main_window;
  else return (Window)NULL;
}


int mGetHsRelease(tk_display)
TkDisplay *tk_display;
{
  help_GetConnection(tk_display);
  if(tk_display!=NULL)
    return (int)tk_display->hs.release;
  else return -1;
}



char *mGetHsComment(tk_display)
TkDisplay *tk_display;
{
  help_GetConnection(tk_display);
  if(tk_display!=NULL)
    return (char *)tk_display->hs.comment;
  else return (char *)NULL;
}



Bool mIsHsActive(tk_display)
TkDisplay *tk_display;
{
  help_GetConnection(tk_display);
  if(tk_display!=NULL)
    return (int)tk_display->hs.active;
  else return -1;
}









