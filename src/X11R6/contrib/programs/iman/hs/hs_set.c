/*
 *
 * 	hs_set.c
 * 	Error handlers initialization
 *
 * 	Modification :  02/04/94
 *
 *	Copyright (c) 1994 Bruno RIVAS
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
 *      IMAN Help Server version 1.0
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */


#ifndef ERR_INIT_C
#define ERR_INIT_C



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


#include "hs.h"






/*
 *
 * Resizing a help session window
 *
 *
 */

int HS_ResizeMain(number,width,height)
unsigned int number;
unsigned int width,height;
{
  unsigned int oldw, oldh;

  if(number>=maxsessions)
    return -1;

  oldw=sessions[number].width;
  oldh=sessions[number].height;
  wid_Configure(tk_display,sessions[number].sb_main,width-20,60,20,height-61,CFX+CFY+CFHeight+CFWidth);
  XResizeWindow(tk_display->display,sessions[number].win_draw,width-21,height-61);
  sessions[number].width=width-21;
  sessions[number].height=height-61;
  if(oldw!=sessions[number].width || oldh!=sessions[number].height)
  {
    if(oldw!=sessions[number].width)
      XClearWindow(tk_display->display,sessions[number].win_draw);
    if(sessions[number].numtopics>0)
      HS_GetLinesDimensions(number);
    if(oldw!=sessions[number].width && sessions[number].numtopics>0)
      HS_Draw(number);
  }
  return -1;
}



int HS_ResizeTopics(number,width,height)
unsigned int number;
unsigned int width,height;
{

  if(number>=maxsessions)
    return -1;

  wid_Configure(tk_display,sessions[number].ls_topics,-1,-1,width,height,CFHeight+CFWidth);

  return -1;
}


int HS_ResizeGlossary(number,width,height)
unsigned int number;
unsigned int width,height;
{

  if(number>=maxsessions)
    return -1;

  wid_Configure(tk_display,sessions[number].ls_glossary,-1,-1,width,height,CFWidth+CFHeight);

  return -1;
}




#endif

