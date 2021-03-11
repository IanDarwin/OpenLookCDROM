/*
 *
 * 	hs_get.c
 * 	Infos on sessions
 *
 * 	Modification :  24/04/94
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


#ifndef _IMAN_HS_GET_C
#define _IMAN_HS_GET_C



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
 * Getting the number of the help session from the window ID
 *
 *
 */

int HS_GetWindowSessionNumber(window)
Window window;
{
  int i;

  if(maxsessions>0)
  for(i=0;i<maxsessions;i++)
  {
    if(sessions[i].win_main==window || sessions[i].win_draw==window || sessions[i].win_topics==window || sessions[i].win_glossary==window || sessions[i].win_about==window || sessions[i].win_error==window || sessions[i].win_warning==window || sessions[i].win_open==window || sessions[i].win_history==window || sessions[i].win_seek==window)  
	return i;
  }
  return -1;
}



int HS_GetWidgetSessionNumber(widget)
WidgetID widget;
{
  int i;

  if(maxsessions>0)
  for(i=0;i<maxsessions;i++)
  {
    if(sessions[i].mn_main==widget || sessions[i].mn_file==widget || sessions[i].mn_help==widget || sessions[i].mn_window==widget || sessions[i].bn_about_ok==widget || sessions[i].bn_error_ok==widget || sessions[i].bn_warning_ok==widget || sessions[i].bn_open_ok==widget || sessions[i].bn_open_cancel==widget || sessions[i].ls_open_file==widget || sessions[i].ls_open_dir==widget || sessions[i].ed_open==widget || sessions[i].ls_glossary==widget || sessions[i].ls_topics==widget || sessions[i].sb_main==widget || sessions[i].bn_index==widget || sessions[i].bn_next==widget || sessions[i].bn_previous==widget || sessions[i].bn_history==widget || sessions[i].bn_print==widget || sessions[i].bn_seek==widget)  
	return i;
  }
  return -1;
}




Bool IsHelpFile(name)
char *name; 
{
  int i;
  unsigned int length;

  if(name==(char *)NULL)
    return False;
  if((length=strlen(name))<5)
    return False;

  if(name[length-1]=='f' && name[length-2]=='h')
  {
    if(name[length-3]=='p' || name[length-3]=='b' )  
	return True;
    else return False;
  }
  else return False;
}





int HS_GetTopic(window)
Window window; 
{
  int i, ret;
  unsigned long *gp_prop;
  Atom gp_type;
  int gp_format;
  unsigned long gp_nitems, gp_nafter;
  ret=XGetWindowProperty(tk_display->display,window,tk_display->atoms._IMAN_HS_TOPIC,0,1,False,AnyPropertyType,&gp_type,&gp_format,&gp_nitems,&gp_nafter,(unsigned char **)&gp_prop);

  if(ret!=Success) return -1;
  if(gp_nitems>0)
    i=gp_prop[0];
  else i=-1;
  if(gp_nitems>0) free((char *)gp_prop);
  return i;

}






char *HS_GetDatabook(window)
Window window; 
{
  int i, ret;
  unsigned char *gp_prop;
  Atom gp_type;
  int gp_format;
  unsigned long gp_nitems, gp_nafter;
  ret=XGetWindowProperty(tk_display->display,window,tk_display->atoms._IMAN_HS_DATABOOK,0,500,False,AnyPropertyType,&gp_type,&gp_format,&gp_nitems,&gp_nafter,(unsigned char **)&gp_prop);

  if(ret!=Success) return (char *)NULL;
  if(gp_nitems>0)
    return (char *)gp_prop;
  else return (char *)NULL;

}





int HS_GetFilesList(number,mask)
unsigned int number;
char *mask;
{
  ItemPixmapDecoration item_pixmap;
  ItemTextDecoration item_text;
  char *title;
  int i;
  unsigned int length;

  if(number>=maxsessions)
    return -1;

  if(sessions[number].numfileinfos>0)
    file_FreeInfos(sessions[number].fileinfos,sessions[number].numfileinfos);
  sessions[number].numfileinfos=0;
 
  sessions[number].fileinfos=(FileInfo **)file_GetOrderedInfos(sessions[number].current_dir,mask,&sessions[number].numfileinfos);
  /*fprintf(stderr,"  PASSE   ");*/

  item_DeleteAll(tk_display,sessions[number].ls_open_file);
  item_DeleteAll(tk_display,sessions[number].ls_open_dir);


  /*fprintf(stderr,"%d files in %s ",sessions[number].numfileinfos,sessions[number].current_dir);*/

  title=(char *)malloc(strlen(sessions[number].current_dir)+strlen("Dir : ")+1);
  if(title==(char *)NULL)
  {
    fprintf(stderr,"Memory alloc error  ");
    file_FreeInfos(sessions[number].fileinfos,sessions[number].numfileinfos);
    HS_UnmapOpen(number);
    return -1;
  }

  if(sessions[number].numfileinfos>1) for(i=1;i<sessions[number].numfileinfos;i++)
  {
    /*fprintf(stderr,"%s \n",sessions[number].fileinfos[i]->name);*/
    if(file_IsDirectory(sessions[number].fileinfos[i]->name)==True)
    {
	item_Add(tk_display,sessions[number].ls_open_dir,0,END,TextFlag,sessions[number].fileinfos[i]->name,0,0,0,True);
	item_pixmap.mask=SPPixmap+SPDepth+SPWidth+SPHeight;
	item_pixmap.depth=1;
	item_pixmap.width=16;
	item_pixmap.height=16;
	if(strcmp(sessions[number].fileinfos[i]->name,"..")==0)
	  item_pixmap.pixmap=bm_stack;
	else item_pixmap.pixmap=bm_dir;
	item_SetPixmapDecoration(tk_display,sessions[number].ls_open_dir,END,&item_pixmap,True);
    	item_SetPrecedency(tk_display,sessions[number].ls_open_dir,END,PixmapFlag,True);

	/*item_text.mask=STText;
	item_text.text=sessions[number].fileinfos[i]->name;
	item_SetTextDecoration(tk_display,sessions[number].ls_open_dir,END,&item_text,True);*/
        /*fprintf(stderr,"%s \n",sessions[number].fileinfos[i]->name);*/
    }
    else     
    {
        item_Add(tk_display,sessions[number].ls_open_file,0,END,TextFlag,sessions[number].fileinfos[i]->name,0,0,0,True);
	item_pixmap.mask=SPPixmap+SPDepth+SPWidth+SPHeight;
	item_pixmap.depth=1;
	item_pixmap.width=16;
	item_pixmap.height=16;
	if(IsHelpFile(sessions[number].fileinfos[i]->name)==True)
	  item_pixmap.pixmap=bm_file;
	else item_pixmap.pixmap=bm_none;
	item_SetPixmapDecoration(tk_display,sessions[number].ls_open_file,END,&item_pixmap,True);
    	item_SetPrecedency(tk_display,sessions[number].ls_open_file,END,PixmapFlag,True);
    }

  }

  /*for(i=1;i<sessions[number].numfileinfos;i++)
    if(file_IsDirectory(sessions[number].fileinfos[i]->name)==True)
  	fprintf(stderr,"%s \n",sessions[number].fileinfos[i]->name);*/


  wid_SetPosition(tk_display,sessions[number].ls_open_file,0);
  wid_SetPosition(tk_display,sessions[number].ls_open_dir,0);
  sprintf(title,"Dir : %s",sessions[number].current_dir);
  win_SetTitleName(tk_display,sessions[number].win_open,title);
	
  free(title);
  wid_Refresh(tk_display,sessions[number].ls_open_file);
  wid_Refresh(tk_display,sessions[number].ls_open_dir);
  

  return 0;
}




#endif


