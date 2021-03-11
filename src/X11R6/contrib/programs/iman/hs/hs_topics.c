/*
 *
 * 	hs_topics.c
 * 	Help topics management
 *
 * 	Modification :  30/04/94
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


#ifndef _IMAN_HS_TOPICS_C
#define _IMAN_HS_TOPICS_C



#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>
#include <misc/file.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/xpm.h>

#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "hs.h"







int HS_GetUnusedTopic(number)
unsigned int number;
{
  int i, j;
  int ret;

  if(number< maxsessions)
  {
    if(sessions[number].numtopics>=sessions[number].maxtopics)
    {
	if(sessions[number].maxtopics<=0)
	  sessions[number].topics=(HelpTopic *)malloc(sizeof(HelpTopic)*(10));
	else
	  sessions[number].topics=(HelpTopic *)realloc(sessions[number].topics,sizeof(HelpTopic)*(sessions[number].maxtopics+10));	
	if(sessions[number].topics==(HelpTopic *)NULL)
	{
	   HS_UnuseSession(number);
	   return -10;
	}
	sessions[number].maxtopics=sessions[number].maxtopics+10;
	for(i=1;i<=10;i++)
	  HS_InitTopic(number,sessions[number].maxtopics-i);
	return sessions[number].numtopics;
    }
    for(i=0;i<sessions[number].maxtopics;i++)
	if(sessions[number].topics[i].isUsed==False)
	  return i;
    return -1;
  }
  return -1;
}




int HS_InitTopic(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(number< maxsessions)
  {
    if(index<sessions[number].maxtopics)
    {
	sessions[number].topics[index].isUsed=False;
	sessions[number].topics[index].class=0;	
	sessions[number].topics[index].number=-1;
	sessions[number].topics[index].name=(char *)NULL;
 	sessions[number].topics[index].loading=0;	
	sessions[number].topics[index].parent=-1;
	sessions[number].topics[index].statistic=0;
	sessions[number].topics[index].seek=-1;
	sessions[number].topics[index].isLoaded=False;
	sessions[number].topics[index].icon=-1;

	sessions[number].topics[index].items=(HelpItem *)NULL;
	sessions[number].topics[index].numitems=0;
	sessions[number].topics[index].maxitems=0;

   }
    return -1;
  }
  return -1;
}



int HS_UseTopic(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(number<maxsessions)
  {
    if(index<sessions[number].maxtopics && sessions[number].topics[index].isUsed==False)
    {	
	HS_InitTopic(number,index);
	sessions[number].topics[index].isUsed=True;
  	sessions[number].numtopics++;
	return 0;
    }
    return -1;
  }
  return -1;
}




int HS_UnuseTopic(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(index<sessions[number].maxtopics && sessions[number].topics[index].isUsed==True)
  {
    if(sessions[number].topics[index].name!=(char *)NULL)
      free(sessions[number].topics[index].name);
    HS_FreeItems(number,index);

    sessions[number].topics[index].seek=-1;
    sessions[number].topics[index].class=0;
    sessions[number].topics[index].loading=0;
    sessions[number].topics[index].number=-1;
    sessions[number].topics[index].isUsed=False;
    sessions[number].topics[index].isLoaded=False;
    sessions[number].topics[index].statistic=0;
    sessions[number].topics[index].icon=-1;

    sessions[number].topics[index].numitems=0;
    sessions[number].topics[index].maxitems=0;

    sessions[number].topics[index].items=(HelpItem *)NULL;
    sessions[number].topics[index].name=(char *)NULL;
    sessions[number].numtopics--;

    return 0;
  }
  return -1;
}




int HS_FreeTopics(number)
unsigned int number;
{
  int index, j;
  int ret;

  if(number>=maxsessions)
    return -1;
  if(sessions[number].maxtopics==0)
    return -2;

  for(index=0;index<sessions[number].maxtopics;index++)
  if(sessions[number].topics[index].isUsed==True)
  {
    HS_UnuseTopic(number,index);
  }
  free((char *)sessions[number].topics);
  sessions[number].numtopics=0;
  sessions[number].maxtopics=0;
  sessions[number].topics=(HelpTopic *)NULL;
  sessions[number].current_topic=-1;

  return 0;
}




int HS_UnloadTopic(number,index)
unsigned int number,index;
{
  int i,j;
  int ret;

  if(number>=maxsessions)
    return -1;
  if(sessions[number].numtopics==0)
    return -2;
  if(index>=sessions[number].maxtopics)
    return -3;
  if(sessions[number].topics[index].isUsed==False)
    return -4;
  if(sessions[number].topics[index].isLoaded==False)
    return -5;

  if(sessions[number].topics[index].numitems>0)
  for(i=0 ;i<sessions[number].topics[index].maxitems;i++)
  if(sessions[number].topics[index].items[i].isUsed==True)
  { 
    if(sessions[number].topics[index].items[i].action==DrawPixmap && sessions[number].resources[sessions[number].topics[index].items[i].res_number].loading==LoadDynamic)
	HS_UnloadPixmap(number,sessions[number].topics[index].items[i].res_number);

    else if(sessions[number].topics[index].items[i].action==DrawBitmap && sessions[number].resources[sessions[number].topics[index].items[i].res_number].loading==LoadDynamic)
	HS_UnloadBitmap(number,sessions[number].topics[index].items[i].res_number);

   else if(sessions[number].topics[index].items[i].action==SetFont && sessions[number].resources[sessions[number].topics[index].items[i].res_number].loading==LoadDynamic)
	HS_UnloadFont(number,sessions[number].topics[index].items[i].res_number);

   else if(sessions[number].topics[index].items[i].action==SetColor && sessions[number].resources[sessions[number].topics[index].items[i].res_number].loading==LoadDynamic)
	HS_UnloadColor(number,sessions[number].topics[index].items[i].res_number);

  }
  if(sessions[number].topics[index].loading==LoadDynamic)
  {
    HS_FreeItems(number,index);
    sessions[number].topics[index].isLoaded=False;
  }
  return 0;
}




int HS_LoadTopic(number,index)
unsigned int number,index;
{
  int i,j;
  int ret;

  sessions[number].current_line=0;
  if(number>=maxsessions)
    return -1;
  if(sessions[number].numtopics==0)
    return -2;
  if(index>=sessions[number].maxtopics)
    return -3;
  if(sessions[number].topics[index].isUsed==False)
    return -4;
  ret=0;
  if(sessions[number].topics[index].isLoaded==False)
  {
    ret=HS_LoadItems(number,index);
    if(ret<0)
      return -1;
    HS_VerifyTopicJumps(number,index);
  }
  /*fprintf(stderr,"Topic loaded  \n");*/

  if(sessions[number].topics[index].numitems>0)
  for(i=0 ;i<sessions[number].topics[index].maxitems;i++)
  if(sessions[number].topics[index].items[i].isUsed==True)
  { 
    if(sessions[number].topics[index].items[i].action==DrawPixmap)
	HS_LoadPixmap(number,sessions[number].topics[index].items[i].res_number);

    else if(sessions[number].topics[index].items[i].action==DrawBitmap)
	HS_LoadBitmap(number,sessions[number].topics[index].items[i].res_number);

   else if(sessions[number].topics[index].items[i].action==SetFont)
	HS_LoadFont(number,sessions[number].topics[index].items[i].res_number);

   else if(sessions[number].topics[index].items[i].action==SetColor)
	HS_LoadColor(number,sessions[number].topics[index].items[i].res_number);

  }
  sessions[number].topics[index].isLoaded=True;
  /*fprintf(stderr,"Topic  resources loaded  \n");*/
  HS_GetItemsDimensions(number,index);
  HS_GetLinesDimensions(number);
  wid_SetPosition(tk_display,sessions[number].sb_main,0);
  sessions[number].current_line=0;

  return 0;
}



int HS_VerifyTopic(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(number<maxsessions && index<sessions[number].maxtopics)
  {
    if(sessions[number].topics[index].name==(char *)NULL)
      return -1;
    if(sessions[number].topics[index].loading<=0)
    {
      sessions[number].topics[index].loading=LoadDynamic;
      sessions[number].topics[index].isLoaded=False;
    }
    if(sessions[number].topics[index].class<=0)
      return -2;
    if(sessions[number].topics[index].loading==LoadAtStart && sessions[number].topics[index].isLoaded==False)
      return -3;

    
      for(j=0;j<sessions[number].maxtopics;j++)
      if(sessions[number].topics[j].isUsed==True)
      {
	if(sessions[number].topics[j].number==sessions[number].topics[index].number &&  j!=index)
	  return -4;
      }

    return 0;
  }
  return -1;
}




int HS_GetMainTopic(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(number<maxsessions && index<sessions[number].maxtopics)
  {
      if(sessions[number].showAllTopics==True) return index;

      ret=0;     
      for(i=0;i<sessions[number].maxtopics;i++)
      if(sessions[number].topics[i].isUsed==True && sessions[number].topics[i].class==MainTopic)
      {
	if(ret==index) return i;
	ret++;
      }

    return -1;
  }
  return -1;
}




int HS_VerifyTopicJumps(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;
  Bool hasjump;


  if(number>=maxsessions)
    return -1;

  
  if(sessions[number].topics[index].isUsed==True)
  {
    /*fprintf(stderr,"*** Topic %d  numitems=%d  maxitems=%d  name=%s\n",index,sessions[number].topics[index].numitems,sessions[number].topics[index].maxitems,sessions[number].topics[index].name);*/
    for(i=0;i<sessions[number].topics[index].maxitems;i++)
    if(sessions[number].topics[index].items[i].isUsed==True)
    { 
	hasjump=False;
	for(j=0;j<sessions[number].maxtopics;j++)
	{
	  if(sessions[number].topics[j].isUsed==True && sessions[number].topics[index].items[i].topic_number==sessions[number].topics[j].number)
	  {
	    sessions[number].topics[index].items[i].topic_number=j;
	    hasjump=True;
	    /*fprintf(stderr,"Found: %d\n",j);*/
	    goto PROC_CONT;
	  }
	}
PROC_CONT:
	if(j>=sessions[number].numtopics && hasjump==False)
	  sessions[number].topics[index].items[i].topic_number=-1;
     }
  }
  return -1;
}




#endif

