/*
 *
 * 	ev_xlib.c
 * 	XLIB events management
 *
 * 	Modification :  13/05/94
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

int EV_Xlib()
{
  WidgetAttributes wid_attributes;
  WidgetTextDecoration wid_text;
  ItemPixmapDecoration item_pixmap;
  ItemTextDecoration item_text;

  XSetWindowAttributes attrib;
  XWindowAttributes xwa;
  XGCValues xgcvalues;
  XEvent send_event, eventbis;

  int mask;
  int ret, ret2;
  int i, j, k, w;
  int number;
  int range, totalheight, position, inc;
  char *databook=(char *)NULL, *helpdir=(char *)NULL, *helpfile=(char *)NULL;
  int topic;

  switch(tk_event.event.type)
  {
	  case ClientMessage :
		number=HS_GetWindowSessionNumber(tk_event.event.xclient.window);
		if(tk_event.event.xclient.message_type==tk_display->atoms.WM_PROTOCOLS && tk_event.event.xclient.data.l[0]==tk_display->atoms.WM_DELETE_WINDOW && number>=0 && number<maxsessions)
		{
		  if(tk_event.event.xclient.window==sessions[number].win_main)
		  {
		    HS_UnuseSession(number);
		    if(numsessions<=0 && mode==MACTIVE)
		      HS_Exit();
		  }
		  else if(tk_event.event.xclient.window==sessions[number].win_main && numsessions>1)
		    HS_UnuseSession(number);
		  else if(tk_event.event.xclient.window==sessions[number].win_topics)
		    win_Unmap(tk_display,sessions[number].win_topics);
		  else if(tk_event.event.xclient.window==sessions[number].win_glossary)
		    win_Unmap(tk_display,sessions[number].win_glossary);
		  else if(tk_event.event.xclient.window==sessions[number].win_error)
		    HS_UnmapError(number);
		  else if(tk_event.event.xclient.window==sessions[number].win_warning)
		    HS_UnmapWarning(number);
		  else if(tk_event.event.xclient.window==sessions[number].win_open)
		    HS_UnmapOpen(number);
		  return 0;
		}
		else if(tk_event.event.xclient.message_type==tk_display->atoms._IMAN_HS_MESSAGES)
		{
		  fprintf(stderr,"Help request %d from window %d   \n",tk_event.event.xclient.data.l[0],tk_event.event.xclient.window);
		  for(i=0;i<maxsessions;i++)
		  if(sessions[i].isUsed==True && sessions[i].owner==tk_event.event.xclient.window)
		  {
			win_MapRaised(tk_display,sessions[i].win_main);
			return 0;
		  }		    
		  databook=HS_GetDatabook(tk_event.event.xclient.window);
		  topic=HS_GetTopic(tk_event.event.xclient.window);
		  if(databook==(char *)NULL)
		  {
			number=HS_GetUnusedSession();
			if(number<0)
			{
				  fprintf(stderr,"ERROR %d num=%d  max=%d...  \n",number,numsessions,maxsessions);
				  return -1;
			}
			HS_UseSession(number);
			sessions[number].owner=tk_event.event.xclient.window;
			dir_SetCurrent("/usr/lib/iman/help");
			sessions[number].current_dir=dir_GetCurrent();
			win_MapRaised(tk_display,sessions[number].win_main);
			HS_MapOpen((unsigned int)number);
			return 0;
		  }
		  else
		  {
			fprintf(stderr,"Databook: %s   Topic: %d  \n",databook,topic);
			number=HS_GetUnusedSession();	
			if(number<0)
			{
				  fprintf(stderr,"ERROR %d num=%d  max=%d...  \n",number,numsessions,maxsessions);
				  return -1;
			}
			HS_UseSession(number);
			sessions[number].owner=tk_event.event.xclient.window;
			win_MapRaised(tk_display,sessions[number].win_main);
			wid_SetFreeze(tk_display,sessions[number].win_main,True);
  			wid_SetFreeze(tk_display,sessions[number].win_topics,True);
  			wid_SetFreeze(tk_display,sessions[number].win_glossary,True);
 
  			sessions[number].numfileinfos=0;
			helpdir=file_ExtractPath(databook);
			if(helpdir==(char *)NULL)
			{
			  dir_SetCurrent("/usr/lib/iman/help");
			  free(sessions[number].current_dir);
			  sessions[number].current_dir=dir_GetCurrent();
			}
			else
			{
			  if(strlen(helpdir)==0)
			    dir_SetCurrent("/usr/lib/iman/help");
			  else dir_SetCurrent(helpdir);
			  free(sessions[number].current_dir);
			  sessions[number].current_dir=dir_GetCurrent();
			  free(helpdir);
			}
  			HS_GetFilesList(number,"*");
  			wid_text.mask=STText;
  			wid_text.text="*";
  			wid_SetTextDecoration(tk_display,sessions[number].ed_open,&wid_text,False);
  			HS_FreeSessionData(number);
  			win_SetTitleName(tk_display,sessions[number].win_main,"Help");
  			/*win_MapRaised(tk_display,sessions[number].win_open);*/
  			XSync(tk_display->display,False);
  			XFlush(tk_display->display);

			helpfile=file_ExtractName(databook);
			ret=HS_OpenHelpFile(number,databook);
			if(helpfile!=(char *)NULL) free(helpfile);
			if(databook!=(char *)NULL) free(databook);

			if(ret<0)
			{
			    fprintf(stderr,"ERROR ret=%d  \n",ret);
			    win_MapRaised(tk_display,sessions[number].win_open);
			    return ret;
			}
			fprintf(stderr,"YEEAH ret=%d  \n",ret);
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
			if(topic>=0)
			for(i=0;i<sessions[number].maxtopics;i++)
			if(sessions[number].topics[i].isUsed==True && sessions[number].topics[i].number==topic)
			  sessions[number].current_topic=i;
			HS_LoadTopic(number,sessions[number].current_topic);
			XClearWindow(tk_display->display,sessions[number].win_draw);
			HS_Draw(number);
			return 0;
		  }

		}
		return 0;
		break;


	  case ConfigureNotify :
		number=HS_GetWindowSessionNumber(tk_event.event.xconfigure.window);
		if(number>=0 && number<maxsessions)
		{
		  if(tk_event.event.xconfigure.window==sessions[number].win_main)
		  {
		    range=sessions[number].scroll_lines;
		    totalheight=HS_GetTotalHeight(number)+20;
		    position=wid_GetPosition(tk_display,sessions[number].sb_main);
		    inc=sessions[number].inc;
		    HS_ResizeMain((unsigned int)number,tk_event.event.xconfigure.width,tk_event.event.xconfigure.height);  
		  }
		  else if(tk_event.event.xconfigure.window==sessions[number].win_topics)
		    HS_ResizeTopics((unsigned int)number,tk_event.event.xconfigure.width,tk_event.event.xconfigure.height);
		  else if(tk_event.event.xconfigure.window==sessions[number].win_glossary)
		    HS_ResizeGlossary((unsigned int)number,tk_event.event.xconfigure.width,tk_event.event.xconfigure.height);
		}
		return 0;
		break;


	  case ButtonPress :
		number=HS_GetWindowSessionNumber(tk_event.event.xbutton.window);
		if(number>=0 && number<maxsessions)
		{
		  if(tk_event.event.xbutton.window==sessions[number].win_draw && sessions[number].action==NoAction)
		  {
		    win_MapRaised(tk_display,sessions[number].win_main);
		    i=HS_GetHeightToLine(number,sessions[number].current_line);
		    j=HS_GetLineFromHeight(number,i+sessions[number].inc+tk_event.event.xbutton.y);
		    if(j<0) return 0;
		    ret=HS_GetTopicJumper(number,j,tk_event.event.xbutton.x-LMARGE);
		    /*fprintf(stderr,"Line %d: jump to topic %d  ",j,ret);*/
		    if(ret<0) return 0;
		    if(ret!=sessions[number].current_topic)
		    {
		      	HS_UnloadTopic(number,sessions[number].current_topic);
		      	sessions[number].current_topic=ret;
		    	sessions[number].current_item=0;
		    	sessions[number].current_line=0;
		    	sessions[number].inc=0;
			if(sessions[number].showAllTopics==True)
			  wid_SetPosition(tk_display,sessions[number].ls_topics,ret);
			else if(sessions[number].topics[ret].class==MainTopic)
			{
				j=0;
			  	for(i=0;i<ret;i++)
				if(sessions[number].topics[i].class==MainTopic)
				  j++;
				wid_SetPosition(tk_display,sessions[number].ls_topics,j);
			}
		    	HS_LoadTopic(number,sessions[number].current_topic);
		    	XClearWindow(tk_display->display,sessions[number].win_draw);
		    	HS_Draw(number);
		    }
		  }
		}
		return 0;
		break;


	  case Expose :
		if(tk_event.event.xexpose.count!=0)
		  return 0;
		number=HS_GetWindowSessionNumber(tk_event.event.xexpose.window);
		if(number>=0 && number<maxsessions)
		{
		  if(tk_event.event.xbutton.window==sessions[number].win_error)
		    HS_DrawError(number,0);
		  if(tk_event.event.xbutton.window==sessions[number].win_about)
		    HS_DrawAbout(number,0);
		  else if(tk_event.event.xbutton.window==sessions[number].win_warning)
		    HS_DrawWarning(number,0);
		  else if(tk_event.event.xbutton.window==sessions[number].win_draw)
		    HS_Draw(number);
		}
		return 0;
		break;


  }
	

}



#endif

