/*
 *
 * 	ev_widgets.c
 * 	Widgets events management
 *
 * 	Modification :  13/05/94
 *
 *	Copyright (c) 1994 Bruno RIVAS
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


#ifndef _IMAN_HS_EV_XLIB_C
#define _IMAN_HS_EV_XLIB_C



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
 * Main loop
 *
 *
 */

int EV_Widgets()
{
 WidgetAttributes wid_attributes;
 WidgetTextDecoration wid_text;
 WidgetItem widgetitem;
 ItemPixmapDecoration item_pixmap;
 ItemTextDecoration item_text;

 XSetWindowAttributes attrib;
 XWindowAttributes xwa;
 XGCValues xgcvalues;
 XEvent send_event, eventbis;

 int mask;
 int ret, ret2;
 int i, j, k, w;
 int number,range,pagerange;
 char *name, *path, *str;
 int newnumber;

  switch(tk_event.ev_type)
  {
	case BN_RELEASED :
		number=HS_GetWidgetSessionNumber(tk_event.button);
		if(number>=0 && number<maxsessions)
		{
		  if(tk_event.button==sessions[number].bn_about_ok)
		  {
		    HS_UnmapAbout(number);
		    return 0;
		  } 
		  else if(tk_event.button==sessions[number].bn_error_ok)
		    HS_UnmapError((unsigned int)number);
		  else if(tk_event.button==sessions[number].bn_warning_ok)
		    HS_UnmapWarning((unsigned int)number);
		  else if(tk_event.button==sessions[number].bn_open_ok)
		    goto VALIDATION_START;
		  else if(tk_event.button==sessions[number].bn_open_cancel)
		  {
		    HS_UnmapOpen((unsigned int)number);
		    item_DeleteAll(tk_display,sessions[number].ls_glossary);
		    item_DeleteAll(tk_display,sessions[number].ls_topics);
		  }
		  else if(tk_event.button==sessions[number].bn_index)
		  {
		    ret=HS_GetMainTopic(number,0);
		    if(ret<0) return -1;
		    ret=0;
		    goto SET_TOPIC_OK;
		  }
	    	  else if(tk_event.button==sessions[number].bn_previous || tk_event.button==sessions[number].bn_next)
		  {

		    ret=sessions[number].current_topic;
		    if(sessions[number].current_topic<sessions[number].numtopics-1 && tk_event.button==sessions[number].bn_next)
		      sessions[number].current_topic++;
		    else if(sessions[number].current_topic>0 && tk_event.button==sessions[number].bn_previous)
		      sessions[number].current_topic--;
		    if(ret!=sessions[number].current_topic)
		    {
			HS_UnloadTopic(number,ret);
			sessions[number].current_item=0;
		    	sessions[number].current_line=0;
		    	sessions[number].inc=0;
			if(sessions[number].showAllTopics==True)
			  wid_SetPosition(tk_display,sessions[number].ls_topics,sessions[number].current_topic);
			else if(sessions[number].topics[sessions[number].current_topic].class==MainTopic)
			{
				j=0;
			  	for(i=0;i<sessions[number].current_topic;i++)
				if(sessions[number].topics[i].class==MainTopic)
				  j++;
				wid_SetPosition(tk_display,sessions[number].ls_topics,j);
			}
		    	HS_LoadTopic(number,sessions[number].current_topic);
		    	XClearWindow(tk_display->display,sessions[number].win_draw);
		    	HS_Draw(number);
		    } 
		  }
	  	  return 0; 
		} 
		break;


	case SB_DOWN :
		ret=wid_GetPosition(tk_display,tk_event.scroll);
		number=HS_GetWidgetSessionNumber(tk_event.scroll);
		if(number>=0 && number<maxsessions && ret!=-1)
		{
		  sessions[number].position=ret;		  
		  i=HS_GetHeightToLine(number,sessions[number].current_line);
		  j=HS_GetLineFromHeight(number,i+sessions[number].inc+20);
		  if(j<0) j=0;
		  ret=HS_GetHeightToLine(number,j);
		  sessions[number].current_line=j;
		  sessions[number].current_item=sessions[number].lines[j].start_item;
		  sessions[number].inc=i+sessions[number].inc+20-ret;
		  	  XCopyArea(tk_display->display,sessions[number].win_draw,sessions[number].win_draw,gc,0,20,sessions[number].width,sessions[number].height-20,0,0);
		  XClearArea(tk_display->display,sessions[number].win_draw,0,sessions[number].height-20,sessions[number].width,20,False);
		  HS_Draw(number);
		} 
		break;

	case SB_THUMBMOVED:
	case SB_PAGEUP:
	case SB_PAGEDOWN :
		ret=wid_GetPosition(tk_display,tk_event.scroll);
		number=HS_GetWidgetSessionNumber(tk_event.scroll);
		if(number>=0 && number<maxsessions && ret!=-1)
		{ 
		  i=HS_GetHeightToLine(number,sessions[number].current_line);
		  j=HS_GetLineFromHeight(number,i+sessions[number].inc+(20*(range=ret-sessions[number].position)));
		  sessions[number].position=ret;
		  if(j<0) j=0;
		  ret=HS_GetHeightToLine(number,j);
		  sessions[number].current_line=j;
		  sessions[number].current_item=sessions[number].lines[j].start_item;
		  sessions[number].inc=i+sessions[number].inc+20*range-ret;
		  XClearWindow(tk_display->display,sessions[number].win_draw);
		  HS_Draw(number);
		} 
		break;

		
	case SB_UP :
		ret=wid_GetPosition(tk_display,tk_event.scroll);
		number=HS_GetWidgetSessionNumber(tk_event.scroll);
		if(number>=0 && number<maxsessions && ret!=-1)
		{
		  sessions[number].position=ret;
		  i=HS_GetHeightToLine(number,sessions[number].current_line);
		  j=HS_GetLineFromHeight(number,i+sessions[number].inc-20);
		  if(j<0) j=0;
		  ret=HS_GetHeightToLine(number,j);
		  sessions[number].current_line=j;
		  sessions[number].current_item=sessions[number].lines[j].start_item;
		  sessions[number].inc=i+sessions[number].inc-20-ret;
		  XCopyArea(tk_display->display,sessions[number].win_draw,sessions[number].win_draw,gc,0,0,sessions[number].width,sessions[number].height-20,0,20);
		  XClearArea(tk_display->display,sessions[number].win_draw,0,0,sessions[number].width,20,False);
		  HS_Draw(number);
		} 
		break;



	case LS_VALIDATION :
	case LS_DOUBLECLICKED :
		ret=wid_GetPosition(tk_display,tk_event.list);
		number=HS_GetWidgetSessionNumber(tk_event.list);
		if(number>=0 && number<maxsessions && ret>=0)
		{
		  
		  if(tk_event.list==sessions[number].ls_topics)
		  {
SET_TOPIC_OK:
			wid_SetPosition(tk_display,sessions[number].ls_topics,ret);
			ret=HS_GetMainTopic(number,ret);
			if(ret<0) return 0;
			if(ret!=sessions[number].current_topic)
			  HS_UnloadTopic(number,sessions[number].current_topic);
			sessions[number].current_topic=ret;
			sessions[number].current_item=0;
			sessions[number].current_line=0;
			sessions[number].inc=0;
			HS_LoadTopic(number,sessions[number].current_topic);
			XClearWindow(tk_display->display,sessions[number].win_draw);
			HS_Draw(number);
		  }

		  else if(tk_event.list==sessions[number].ls_open_dir)
		  {
			if(ret<0) return 0;
			wid_GetItem(tk_display,sessions[number].ls_open_dir,ret,&widgetitem);
			dir_SetCurrent(sessions[number].current_dir);
			dir_SetCurrent(widgetitem.text);
			free(sessions[number].current_dir);
			sessions[number].current_dir=(char *)dir_GetCurrent();
			HS_GetFilesList((unsigned int)number,"*");
			wid_text.mask=STText;
			wid_text.text="*";
			wid_SetTextDecoration(tk_display,sessions[number].ed_open,&wid_text,False);

		  }
		  else if(tk_event.list==sessions[number].ls_open_file)
		  {
			if(ret<0) return 0;
			ret=wid_GetItem(tk_display,sessions[number].ls_open_file,ret,&widgetitem);
			if(ret==0)
			{
OPEN_OK:		  dir_SetCurrent(sessions[number].current_dir);
			  ret=HS_OpenHelpFile(number,widgetitem.text);
			  if(ret<0)
			    return ret;
			  HS_UnmapOpen(number);
			  if(sessions[number].numtopics>0)
			  {
			    wid_SetState(tk_display,sessions[number].bn_index,Ungrayed);
  			    wid_SetState(tk_display,sessions[number].bn_previous,Ungrayed);
  			    wid_SetState(tk_display,sessions[number].bn_next,Ungrayed);
  			    wid_SetState(tk_display,sessions[number].bn_seek,Grayed);
  			    wid_SetState(tk_display,sessions[number].bn_print,Grayed);
			  }
			  XClearWindow(tk_display->display,sessions[number].win_draw);
			  wid_SetPosition(tk_display,sessions[number].ls_glossary,0);
			  wid_SetPosition(tk_display,sessions[number].ls_topics,0);
			  sessions[number].current_topic=0;
			  HS_LoadTopic(number,sessions[number].current_topic);
			  XClearWindow(tk_display->display,sessions[number].win_draw);
			  HS_Draw(number);
			  return 0;
			}
			else fprintf(stderr,"ERROR %d ",ret);
		  }
	  	  return 0; 
		}
		break;

	case LS_CLICKED :
	case LS_RELEASED :
		ret=wid_GetPosition(tk_display,tk_event.list);
		number=HS_GetWidgetSessionNumber(tk_event.list);
		if(number>=0 && number<maxsessions && ret!=-1)
		{
		  if(tk_event.list==sessions[number].ls_open_file)
		  {
			if(ret<0) return 0;
			wid_GetItem(tk_display,sessions[number].ls_open_file,ret,&widgetitem);
			wid_text.mask=STText;
			wid_text.text=widgetitem.text;
			wid_SetTextDecoration(tk_display,sessions[number].ed_open,&wid_text,False);
			
		  }
		  else if(tk_event.list==sessions[number].ls_topics && tk_event.ev_type==LS_RELEASED)
		    goto SET_TOPIC_OK;
	  	  return 0; 
		}
		break;


	case ED_VALIDATION :
		number=HS_GetWidgetSessionNumber(tk_event.edit);
		if(number>=0 && number<maxsessions && ret!=-1)
		{
		  if(tk_event.edit==sessions[number].ed_open)
		  {
VALIDATION_START:
			tk_event.edit=sessions[number].ed_open;
			wid_GetText(tk_display,sessions[number].ed_open,(unsigned char**)&str);
			if(str==(char *)NULL)
			  return 0;
			if(file_VerifyAccess(str)==0)
			{
			  if(file_IsDirectory(str)==True)
			  {
			    dir_SetCurrent(str);
			    free(sessions[number].current_dir);
			    sessions[number].current_dir=(char *)dir_GetCurrent();
			    HS_GetFilesList((unsigned int)number,"*");
			    wid_text.mask=STText;
			    wid_text.text="*";
			    wid_SetTextDecoration(tk_display,sessions[number].ed_open,&wid_text,False);
			    free(str);
			    return 0;
			  }
			  else
			  {
			    if(str!=(char *)NULL) free(str);
			    ret=wid_GetPosition(tk_display,sessions[number].ls_open_file);
			    wid_GetItem(tk_display,sessions[number].ls_open_file,ret,&widgetitem);
			    goto OPEN_OK;
			  }
			}
			name=file_ExtractName(str);
			path=file_ExtractPath(str);
			if(path!=(char *)NULL) dir_SetCurrent(path);
			wid_text.mask=STText;
			if(name==(char *)NULL)
			  name="*";
			wid_text.text=name;
			wid_SetTextDecoration(tk_display,sessions[number].ed_open,&wid_text,False);
			free(sessions[number].current_dir);
			sessions[number].current_dir=(char *)dir_GetCurrent();
			HS_GetFilesList((unsigned int)number,name);
			free(str);
			if(name!=(char *)NULL)
			  free(name);
			if(path!=(char *)NULL)
			  free(path);
			return 0;
		  }
	  	  return 0; 
		}
		break;


	case MN_SELECTED :
		ret=wid_GetPosition(tk_display,tk_event.menu);
		number=HS_GetWidgetSessionNumber(tk_event.menu);
		if(number>=0 && number<maxsessions && ret!=-1)
		{
		  if(tk_event.menu==sessions[number].mn_file)
		  switch(ret)
		  {
		  	case  0 :
				newnumber=HS_GetUnusedSession();
				if(newnumber<0)
				{
				  fprintf(stderr,"ERROR %d num=%d  max=%d...  ",newnumber,numsessions,maxsessions);
				  return -1;
				}
				HS_UseSession(newnumber);
				sessions[newnumber].owner=None;
				win_MapRaised(tk_display,sessions[newnumber].win_main);
OPEN_NO_NEW:
				HS_MapOpen((unsigned int)newnumber);
				break;

			case 1 :
				newnumber=number;
				goto OPEN_NO_NEW;
				break;

			case 3 :
				HS_UnuseSession(number);
				if(numsessions==0 && mode==MACTIVE) HS_Exit();
				break;

			case  4 :
				HS_Exit();
				break;
		  }
  		  else if(tk_event.menu==sessions[number].mn_window)
		  switch(ret)
		  {
		  	case  0 :
				win_MapRaised(tk_display,sessions[number].win_topics);
				break;
		  	case  1 :
				win_MapRaised(tk_display,sessions[number].win_glossary);
				break;

		  }
  		  else if(tk_event.menu==sessions[number].mn_help)
		  switch(ret)
		  {
		  	case  0 :
				HS_MapAbout(number);
				XSync(tk_display->display,False);
				wid_GiveFocus(tk_display,sessions[number].bn_about_ok);
				break;

		  	case  1 :
				HS_MapWarning(number);
				XSync(tk_display->display,False);
				wid_GiveFocus(tk_display,sessions[number].bn_warning_ok);
				break;
		  }
		}
		break;


	default :

		break;

  }
  return 0;

}



#endif

