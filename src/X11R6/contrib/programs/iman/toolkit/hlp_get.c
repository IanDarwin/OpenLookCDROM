/*
 *
 * 	hlp_get.c  
 * 	informations sur le serveur d'aide
 *
 * 	Modification :  18/04/94
 *
 *	Copyright (c) 1993, 1994 Bruno RIVAS
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
#include <X11/Xatom.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>









/*
 * Rafraichir/Obtenir la connection avec le HS
 *
 */

int help_GetConnection(tk_display)
TkDisplay *tk_display;
{
  int i;
  int ret=0;

  Atom gp_actual_type;
  int gp_actual_format;
  unsigned long gp_nitems, gp_bytes_after;
  unsigned char *gp_prop;
  char *str;


  tk_display->hs.main_window=XGetSelectionOwner(tk_display->display,tk_display->atoms._IMAN_HELP_SERVER);
  if(tk_display->hs.main_window==None) return False;   
  
  tk_display->hs.active=True;
  ret=XGetWindowProperty(tk_display->display,tk_display->hs.main_window,tk_display->atoms._IMAN_HELP_SERVER,0,2,False,XA_INTEGER,&gp_actual_type,&gp_actual_format,&gp_nitems,&gp_bytes_after,&gp_prop);
  if(ret==0 && gp_nitems>=2)
  {	
    tk_display->hs.version=gp_prop[0];
    tk_display->hs.release=gp_prop[1];
    if(tk_display->hs.comment!=(char *)NULL)
	free(tk_display->hs.comment);
    XFetchName(tk_display->display,tk_display->hs.main_window,&str);
#ifdef DEBUG
    fprintf(stderr,"str=%s\n",str);
#endif
    tk_display->hs.comment=str;

  }
  else 
  {
    tk_display->hs.version=0;
    tk_display->hs.release=0;
    if(tk_display->hs.comment!=(char *)NULL)
	free(tk_display->hs.comment);
    tk_display->hs.comment=(char *)NULL;
  }
  
  if(gp_nitems>0) XFree((char *)gp_prop);  

  /*(void)fprintf(stderr,"help_GetConnection terminee  active=%d\n",tk_display->hs.active);*/
  return tk_display->hs.active;

}




