/*
 *
 * 	hs_file.c
 * 	Help files I/O
 *
 * 	Modification :  05/05/94
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


#ifndef _IMAN_HS_FILE_C
#define _IMAN_HS_FILE_C



#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>
#include <errno.h>
#include <unistd.h>


#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "hs.h"


#define phf_header	"??PHF??"
#define bhf_header	"??BHF??"
#define version_text	"version="
#define release_text	"release="
#define name_text	"helpname="
#define copyright_text	"copyright="
#define vendor_text	"vendor="
#define month_text	"month="
#define year_text	"year="
#define day_text	"day="
#define exclam		"!!"
#define startresources	"startresources"
#define endresources	"endresources"
#define newresource	"newresource"
#define newnumber	"number"	
#define newloading	"loading"
#define newclass	"class"	
#define newtype		"type"
#define newname		"name"
#define startbitmap	"startbitmap"
#define endbitmap	"endbitmap"
#define startpixmap	"startpixmap"
#define endpixmap	"endpixmap"
#define startinfos	"startinfos"
#define endinfos	"endinfos"
#define starttopics	"starttopics"
#define endtopics	"endtopics"
#define newtopic	"newtopic"
#define startitems	"startitems"
#define enditems	"enditems"
#define newitem		"newitem"
#define newicon		"icon"





#define NoAction 	0
#define ReadInfos	1
#define ReadResources 	2
#define ReadTopics	3



#define CR	13
#define LF	10
#define SPACE	32


int action=0;


/*
 *
 * Getting the number of the help session from the window ID
 *
 *
 */

int HS_OpenHelpFile(number,name)
unsigned int number;
char *name;
{
  ItemPixmapDecoration item_pixmap;
  ItemTextDecoration item_text;
  int i, ret, pos;
  char *text, *ltext;
  int current_resource;
  int current_topic;
  int current_item;


  fprintf(stderr,"opening %s  ",name);
  action=0;

  if(number>=maxsessions)
    return -1;
  if(name==(char *)NULL)
    return -2;
  if(file_IsNormal(name)!=1)
    return -3;

  if(sessions[number].file!=0)
    close(sessions[number].file);
  sessions[number].file=0;

  if(sessions[number].filename!=(char *)NULL)
    free(sessions[number].filename);
  sessions[number].filename=(char *)malloc(strlen(name)+2);
  if(sessions[number].filename==(char *)NULL)
    return -4;
  memset(sessions[number].filename,0,strlen(name)+2);
  sessions[number].filename=strcpy(sessions[number].filename,name);
  
  if(sessions[number].dirname!=(char *)NULL)
    free(sessions[number].dirname);
  sessions[number].dirname=(char *)malloc(strlen(sessions[number].current_dir)+2);
  if(sessions[number].dirname==(char *)NULL)
    return -5;
  memset(sessions[number].dirname,0,strlen(sessions[number].current_dir)+2);
  sessions[number].dirname=strcpy(sessions[number].dirname,sessions[number].current_dir);
  sessions[number].index=0;

  HS_FreeSessionData(number);

  sessions[number].file=open(name,O_RDWR);
  if(sessions[number].file == -1)
    goto ERROR;
  sessions[number].stream=fopen(name,"r+");
  if(sessions[number].stream == (FILE *)NULL)
    goto ERROR;


  ret=HS_GetHelpFileFormat(number);
  if(ret!=0)
  {
	fprintf(stderr,"Open fail at File Format=%d \n",ret);
	HS_OpenFail(number);
	return -6;
  }
  /*fprintf(stderr,"File Format=%d\n",sessions[number].format);*/


  current_resource=-1;
  current_topic=-1;
  current_item=-1;

  text=(char *)HS_ReadNextLine(number);
  ltext=(char *)HS_LowerText(text);

  

  while(text!=(char *)NULL && ltext!=(char *)NULL)
  {
    /*fprintf(stderr,": %s\n",text);*/
    if(strncmp(ltext,exclam,strlen(exclam))==0)
    {
	if(action==ReadInfos)
	  action=NoAction;
	else if(action==ReadResources)
	  action=NoAction;
	else if(action==ReadTopics)
	  action=NoAction;
        free(text);
	free(ltext);
    }

						/**** INFOS ****/
 
    else if(strncmp(ltext,startinfos,strlen(startinfos))==0)
    {
	if(action==NoAction)
	  action=ReadInfos;
	else if(action!=NoAction)
	  goto ERROR;
	/*fprintf(stderr,"******* Reading infos *******\n");*/
        free(text);
	free(ltext);
    }
    else if(strcmp(ltext,endinfos)==0)
    {
	if(action==ReadInfos)
	  action=NoAction;
	else
	  goto ERROR;
	/*fprintf(stderr,">>>> infos read  ...\n");*/
        free(text);
 	free(ltext);
    }
    else if(strncmp(ltext,name_text,strlen(name_text))==0 && action==ReadInfos)
    {
	sessions[number].help_name=malloc(strlen(text)-strlen(name_text)+1);
	if(sessions[number].help_name==(char *)NULL)
	  goto ERROR;
	memset(sessions[number].help_name,0,strlen(text)-strlen(name_text)+1);
	strcpy(sessions[number].help_name,text+strlen(name_text));
	free(text);
	free(ltext);
	/*fprintf(stderr,"NAME = %s  \n",sessions[number].help_name);*/
    }
    else if(strncmp(ltext,copyright_text,strlen(copyright_text))==0)
    {
	if(action!=ReadInfos)
	  goto ERROR;   
	sessions[number].copyright=malloc(strlen(text)-strlen(copyright_text)+1);
	if(sessions[number].copyright==(char *)NULL)
	  goto ERROR;
	memset(sessions[number].copyright,0,strlen(text)-strlen(copyright_text)+1);
	strcpy(sessions[number].copyright,text+strlen(copyright_text));
	free(text);
	free(ltext);
	/*fprintf(stderr,"COPYRIGHT = %s  \n",sessions[number].copyright);*/
    }
    else if(strncmp(ltext,vendor_text,strlen(vendor_text))==0)
    {
	if(action!=ReadInfos)
	  goto ERROR;   
	sessions[number].vendor=malloc(strlen(text)-strlen(vendor_text)+1);
	if(sessions[number].vendor==(char *)NULL)
	  goto ERROR;
	memset(sessions[number].vendor,0,strlen(text)-strlen(vendor_text)+1);
	strcpy(sessions[number].vendor,text+strlen(vendor_text));
	free(text);
	free(ltext);
	/*fprintf(stderr,"VENDOR = %s  \n",sessions[number].vendor);*/
    }
    else if(strncmp(ltext,version_text,strlen(version_text))==0)
    {
	if(action!=ReadInfos)
	  goto ERROR;   
	sessions[number].version=(int)strtol(text+strlen(version_text),(char **)NULL,10);
	free(text);
	free(ltext);
	/*fprintf(stderr,"VERSION = %d  \n",sessions[number].version);*/
    }
    else if(strncmp(ltext,release_text,strlen(release_text))==0)
    {
	if(action!=ReadInfos)
	  goto ERROR;   
	sessions[number].release=(int)strtol(text+strlen(release_text),(char **)NULL,10);
	free(text);
	free(ltext);
	/*fprintf(stderr,"RELEASE = %d  \n",sessions[number].release);*/
    }
    else if(strncmp(ltext,year_text,strlen(year_text))==0)
    {
	if(action!=ReadInfos)
	  goto ERROR;   
	sessions[number].year=(int)strtol(text+strlen(year_text),(char **)NULL,10);
	free(text);
	free(ltext);
	/*fprintf(stderr,"YEAR = %d  \n",sessions[number].year);*/
    }
    else if(strncmp(ltext,month_text,strlen(month_text))==0)
    {
	if(action!=ReadInfos)
	  goto ERROR;   
	sessions[number].month=(int)strtol(text+strlen(month_text),(char **)NULL,10);
	free(text);
	free(ltext);
	/*fprintf(stderr,"MONTH = %d  \n",sessions[number].month);*/
    }
    else if(strncmp(ltext,day_text,strlen(day_text))==0)
    {
	if(action!=ReadInfos)
	  goto ERROR;   
	sessions[number].day=(int)strtol(text+strlen(day_text),(char **)NULL,10);
	free(text);
	free(ltext);
	/*fprintf(stderr,"DAY = %d  \n",sessions[number].day);*/
    }


					/**** RESOURCES ****/

    else if(strncmp(ltext,startresources,strlen(startresources))==0)
    {
	if(action==NoAction)
	  action=ReadResources;
	else if(action!=NoAction)
	  goto ERROR;
	/*fprintf(stderr,"******* Reading resources *******\n");*/
        free(text);
	free(ltext);
    }
    else if(strncmp(ltext,endresources,strlen(endresources))==0)
    {
	if(action==ReadResources)
	  action=NoAction;
	else
	  goto ERROR;
	if(current_resource>=0)
	  ret=HS_VerifyResource(number,current_resource);
	if(ret<0)
	  goto ERROR;
	/*fprintf(stderr,">>>> %d resources read and stocked ...\n",sessions[number].numresources);*/
        free(text);
	free(ltext);
    }
    else if(strncmp(ltext,newresource,strlen(newresource))==0 && action==ReadResources)
    {
	if(current_resource>=0)
	  ret=HS_VerifyResource(number,current_resource);
	if(ret<0) goto ERROR;
	current_resource=HS_GetUnusedResource(number);
	if(current_resource<0)
	  goto ERROR;
	HS_UseResource(number,current_resource);
	/*fprintf(stderr,"--- New resource : %d\n",current_resource);*/
	free(text);
	free(ltext);
    }
    else if(strncmp(ltext,newclass,strlen(newclass))==0 && action==ReadResources)
    {
	if(current_resource<0)
	  goto ERROR;
	if(sessions[number].resources[current_resource].class>0)
	  goto ERROR;
	if(strcmp(ltext+strlen(newclass)+1,"font")==0)
	  i=FONTCLASS;
	else if(strcmp(ltext+strlen(newclass)+1,"color")==0)
	  i=COLORCLASS;
	else if(strcmp(ltext+strlen(newclass)+1,"pixmap")==0)
	  i=PIXMAPCLASS;
	else if(strcmp(ltext+strlen(newclass)+1,"bitmap")==0)
	  i=BITMAPCLASS;
	else return -1;
	sessions[number].resources[current_resource].class=i;
	/*fprintf(stderr,"Class   : %d\n",i);*/
	free(text);
	free(ltext);
    } 
    else if(strncmp(ltext,newtype,strlen(newtype))==0 && action==ReadResources)
    {
	if(current_resource<0)
	  goto ERROR;
	if(sessions[number].resources[current_resource].type>0)
	  goto ERROR;
	if(strncmp(ltext+strlen(newtype)+1,"in",2)==0)
	  i=Inside;
	else if(strncmp(ltext+strlen(newtype)+1,"out",3)==0)
	  i=Outside;
	else return -1;
	sessions[number].resources[current_resource].type=i;
	/*fprintf(stderr,"Type    : %d\n",i);*/
	free(text);
	free(ltext);
    } 
    else if(strncmp(ltext,newnumber,strlen(newnumber))==0 && action==ReadResources)
    {
	if(current_resource<0)
	  goto ERROR;
	i=(int)strtol(text+strlen(newnumber)+1,(char **)NULL,10);
	sessions[number].resources[current_resource].number=i;
	/*fprintf(stderr,"Number  : %d\n",i);*/
	free(text);
	free(ltext);
    } 
    else if(strncmp(ltext,newloading,strlen(newloading))==0 && action==ReadResources)
    {
	if(current_resource<0)
	  goto ERROR;
	if(strcmp(ltext+strlen(newloading)+1,"atstart")==0)
	  i=LoadAtStart;
	else if(strcmp(ltext+strlen(newloading)+1,"whenfirstneeded")==0)
	  i=LoadWhenFirstNeeded;
	else if(strcmp(ltext+strlen(newloading)+1,"dynamic")==0)
	  i=LoadDynamic;
	sessions[number].resources[current_resource].loading=i;
	/*fprintf(stderr,"Loading : %d\n",i);*/
	free(text);
	free(ltext);
    } 
    else if(strncmp(ltext,newname,strlen(newname))==0 && action==ReadResources)
    {
	if(current_resource<0)
	  goto ERROR;
	if(sessions[number].resources[current_resource].name!=(char *)NULL)
	  goto ERROR;
	if((ret=strlen(text+strlen(newname)+1))==0)
	  goto ERROR;
	sessions[number].resources[current_resource].name=malloc(ret+1);
	if(sessions[number].resources[current_resource].name==(char *)NULL)
	  goto ERROR;
	memset(sessions[number].resources[current_resource].name,0,ret+1);
	strcpy(sessions[number].resources[current_resource].name,text+strlen(newname)+1);
	free(text);
	free(ltext);

	if(sessions[number].resources[current_resource].class==BITMAPCLASS && sessions[number].resources[current_resource].type==Inside)
	{	
		text=(char *)HS_ReadNextLine(number);
		ltext=(char *)HS_LowerText(text);
		if(text==(char *)NULL)
		  goto ERROR;
		if(strncmp(ltext,startbitmap,strlen(startbitmap))!=0)
		  goto ERROR;
		sessions[number].resources[current_resource].seek=lseek(sessions[number].file,0,1);
		/*fprintf(stderr,"Bitmap position=%d\n",sessions[number].resources[current_resource].seek);*/
		free(text);
		free(ltext);
		text=(char *)HS_ReadNextLine(number);
		ltext=(char *)HS_LowerText(text);
		while(text!=(char *)NULL &&  strncmp(ltext,endbitmap,strlen(endbitmap))!=0)
		{	
		  free(text);
		  free(ltext);
		  text=(char *)HS_ReadNextLine(number);
		  ltext=(char *)HS_LowerText(text);
		}
		if(text==(char *)NULL)
		  goto ERROR;
		free(text);
		free(ltext);
	}
	else if(sessions[number].resources[current_resource].class==PIXMAPCLASS && sessions[number].resources[current_resource].type==Inside)
	{	
		text=(char *)HS_ReadNextLine(number);
		ltext=(char *)HS_LowerText(text);
		if(text==(char *)NULL)
		  goto ERROR;
		if(strncmp(ltext,startpixmap,strlen(startpixmap))!=0)
		  goto ERROR;
		sessions[number].resources[current_resource].seek=lseek(sessions[number].file,0,1);
		/*fprintf(stderr,"Pixmap position=%d\n",sessions[number].resources[current_resource].seek);*/
		free(text);
		free(ltext);
		text=(char *)HS_ReadNextLine(number);
		ltext=(char *)HS_LowerText(text);
		while(text!=(char *)NULL &&  strncmp(ltext,endpixmap,strlen(endpixmap))!=0)
		{	
		  free(text);
		  free(ltext);
		  text=(char *)HS_ReadNextLine(number);
		  ltext=(char *)HS_LowerText(text);
		}
		if(text==(char *)NULL)
		  goto ERROR;
		free(text);
		free(ltext);
	}


	if(sessions[number].resources[current_resource].loading==LoadAtStart)
	{
	  if(sessions[number].resources[current_resource].class==0)
	    goto ERROR;
	  else if(sessions[number].resources[current_resource].class==FONTCLASS)
	  {	
		/*fprintf(stderr,"Font class ...");*/
	    	sessions[number].resources[current_resource].font=HS_LoadFont(number,current_resource);
		if(sessions[number].resources[current_resource].font==(XFontStruct *)NULL)
		  goto ERROR;
	  }
	  else if(sessions[number].resources[current_resource].class==COLORCLASS)
	  {	
	    	ret=HS_LoadColor(number,current_resource);
		if(ret<0)
		  goto ERROR;
		/*XSetWindowBackground(tk_display->display,sessions[number].win_draw,sessions[number].resources[current_resource].color);
		XClearWindow(tk_display->display,sessions[number].win_draw);*/
	  }
	  else if(sessions[number].resources[current_resource].class==BITMAPCLASS)
	  {	
	    	ret=HS_LoadBitmap(number,current_resource);
		if(ret<0)
		  goto ERROR;
		/*XSetForeground(tk_display->display,gc,tk_display->win_colors.text);		XCopyPlane(tk_display->display,sessions[number].resources[current_resource].bitmap,sessions[number].win_draw,gc,0,0,sessions[number].resources[current_resource].width,sessions[number].resources[current_resource].height,0,0,1);*/
	  }
	  else if(sessions[number].resources[current_resource].class==PIXMAPCLASS)
	  {	
		pos=lseek(sessions[number].file,0,SEEK_CUR);
	    	ret=HS_LoadPixmap(number,current_resource);
		if(ret<0)
		  goto ERROR;
		/*XSetForeground(tk_display->display,gc,tk_display->win_colors.text);		XCopyArea(tk_display->display,sessions[number].resources[current_resource].pixmap,sessions[number].win_draw,gc,0,0,sessions[number].resources[current_resource].width+10,sessions[number].resources[current_resource].height+10,0,50);*/
		lseek(sessions[number].file,pos,SEEK_SET);
		XSync(tk_display->display,False);
	  }
	  sessions[number].resources[current_resource].isLoaded=True;
	}
	/*fprintf(stderr,"Name    : %s\n",sessions[number].resources[current_resource].name);*/
	
    } 

    					/**** TOPICS ****/

    else if(strcmp(ltext,starttopics)==0)
    {
	if(action==NoAction)
	  action=ReadTopics;
	else if(action!=NoAction)
	  goto ERROR;
	/*fprintf(stderr,"******* Reading Topics *******\n");*/
        free(text);
	free(ltext);
    }
    else if(strncmp(ltext,endtopics,strlen(endtopics))==0)
    {
	if(action==ReadTopics)
	  action=NoAction;
	else
	  goto ERROR;
	if(current_topic>=0)
	  ret=HS_VerifyTopic(number,current_topic);
	if(ret<0)
	  goto ERROR;
	/*fprintf(stderr,">>>> %d topics read and stocked ...\n",sessions[number].numtopics);*/
        free(text);
	free(ltext);
	wid_Refresh(tk_display,sessions[number].ls_topics);
    }
    else if(strncmp(ltext,newtopic,strlen(newtopic))==0)
    {
	if(action!=ReadTopics)
	  goto ERROR;
	if(current_topic>=0)
	  ret=HS_VerifyTopic(number,current_topic);
	if(ret<0) fprintf(stderr,"Topic verify error %d   ",ret);
	if(ret<0) goto ERROR;
	current_topic=HS_GetUnusedTopic(number);
	if(current_topic<0)
	  goto ERROR;
	ret=HS_UseTopic(number,current_topic);
	if(ret<0) fprintf(stderr,"Topic allocation error %d   ",ret);
	if(ret<0) goto ERROR;
	/*fprintf(stderr,"--- New topic : %d\n",current_topic);*/
	free(text);
	free(ltext);
    }
    else if(strncmp(ltext,newclass,strlen(newclass))==0 && action==ReadTopics)
    {
	/*fprintf(stderr,"Class for %d\n",current_topic);*/
	if(current_topic<0)
	  goto ERROR;
	if(sessions[number].topics[current_topic].class>0)
	  goto ERROR;
	if(strncmp(text+strlen(newclass)+1,"Main",4)==0)
	  i=MainTopic;
	else if(strncmp(text+strlen(newclass)+1,"Sub",3)==0)
	  i=SubTopic;
	else return -1;
	sessions[number].topics[current_topic].class=i;
	/*fprintf(stderr,"Class   : %d\n",i);*/
	free(text);
	free(ltext);
    } 
    else if(strncmp(ltext,newnumber,strlen(newnumber))==0 && action==ReadTopics)
    {
	if(current_topic<0)
	  goto ERROR;
	i=(int)strtol(text+strlen(newnumber)+1,(char **)NULL,10);
	sessions[number].topics[current_topic].number=i;
	/*fprintf(stderr,"Number  : %d\n",i);*/
	free(text);
	free(ltext);
    } 
    else if(strncmp(ltext,newicon,strlen(newicon))==0 && action==ReadTopics)
    {
	if(current_topic<0)
	  goto ERROR;
	i=(int)strtol(text+strlen(newicon)+1,(char **)NULL,10);
	sessions[number].topics[current_topic].icon=i;
	/*fprintf(stderr,"Icon  : %d\n",i);*/
	free(text);
	free(ltext);
    } 
    else if(strncmp(ltext,newloading,strlen(newloading))==0 && action==ReadTopics)
    {
	if(current_topic<0)
	  goto ERROR;
	i=0;
	if(strcmp(text+strlen(newloading)+1,"AtStart")==0)
	  i=LoadAtStart;
	else if(strcmp(text+strlen(newloading)+1,"WhenFirstNeeded")==0)
	  i=LoadWhenFirstNeeded;
	else if(strcmp(text+strlen(newloading)+1,"Dynamic")==0)
	  i=LoadDynamic;
	sessions[number].topics[current_topic].loading=i;
	/*fprintf(stderr,"Loading : %d\n",i);*/
	free(text);
	free(ltext);
    } 
    else if(strncmp(ltext,newname,strlen(newname))==0 && action==ReadTopics)
    {
	if(current_topic<0)
	  goto ERROR;
	if(sessions[number].topics[current_topic].name!=(char *)NULL)
	  goto ERROR;
	if((ret=strlen(text+strlen(newname)+1))==0)
	  goto ERROR;
	sessions[number].topics[current_topic].name=malloc(ret+1);
	if(sessions[number].topics[current_topic].name==(char *)NULL)
	  goto ERROR;
	memset(sessions[number].topics[current_topic].name,0,ret+1);
	strcpy(sessions[number].topics[current_topic].name,text+strlen(newname)+1);
	free(text);
	free(ltext);
	if(sessions[number].topics[current_topic].class==MainTopic)
	  item_Add(tk_display,sessions[number].ls_topics,0,END,0,sessions[number].topics[current_topic].name,0,(XFontStruct *)NULL,0,True);
	else if(sessions[number].showAllTopics==True)
	  item_Add(tk_display,sessions[number].ls_topics,0,END,0,sessions[number].topics[current_topic].name,0,(XFontStruct *)NULL,0,True);
	if(sessions[number].topics[current_topic].loading==0)
	  goto ERROR;
	/*fprintf(stderr,"Name : %s\n",sessions[number].topics[current_topic].name);*/
	text=(char *)HS_ReadNextLine(number);
	ltext=(char *)HS_LowerText(text);
	if(strncmp(ltext,startitems,strlen(startitems))==0)
	  sessions[number].topics[current_topic].seek=lseek(sessions[number].file,0,SEEK_CUR);
	else goto ERROR;
	/*fprintf(stderr,"Position : %d\n",sessions[number].topics[current_topic].seek);*/
	
	if(sessions[number].topics[current_topic].loading==LoadAtStart)
	{
	  ret=HS_LoadItems(number,current_topic);	
	  if(ret<0) goto ERROR;
	  sessions[number].topics[current_topic].isLoaded=True;
	}
	if(text!=(char *)NULL)
	  free(text);
	if(ltext!=(char *)NULL)
	  free(ltext);
		
    }
    else
    {
      if(text!=(char *)NULL)
	  free(text);
      if(ltext!=(char *)NULL)
	  free(ltext);
    }
    text=(char *)HS_ReadNextLine(number);
    ltext=(char *)HS_LowerText(text);

  }

  wid_Refresh(tk_display,sessions[number].ls_topics);
  wid_Refresh(tk_display,sessions[number].ls_glossary);

  win_SetTitleName(tk_display,sessions[number].win_main,sessions[number].help_name);
  /*fprintf(stderr,"opened : %s \n",sessions[number].filename);*/
  HS_GetAllItemsDimensions(number);
  for(i=0;i<sessions[number].maxtopics;i++)
  if(sessions[number].topics[i].isUsed==True && sessions[number].topics[i].isLoaded==True)
    HS_VerifyTopicJumps(number,i);
  /*fprintf(stderr,"end of opening \n");*/
  return 0;

ERROR :
  fprintf(stderr,"Error in the PHF file\n");
  if(text!=(char *)NULL)
    free(text);
  if(ltext!=(char *)NULL)
	  free(ltext);

  item_DeleteAll(tk_display,sessions[number].ls_topics);
  item_DeleteAll(tk_display,sessions[number].ls_glossary);

  HS_OpenFail(number);
  return -1;

}




int HS_OpenFail(number)
unsigned int number;
{
  if(number>=maxsessions)
    return -1;
 
  if(sessions[number].file!=-1)
  {
    close(sessions[number].file);
    if(sessions[number].stream!=(FILE *)NULL)
      fclose(sessions[number].stream);
   }
   sessions[number].file=-1;
   sessions[number].format=0;
   sessions[number].version=-1;
   sessions[number].release=-1;
   sessions[number].index=0;

  if(sessions[number].help_name!=(char *)NULL)
  {
    free(sessions[number].help_name);
    sessions[number].help_name=(char *)NULL;
  }
  if(sessions[number].copyright!=(char *)NULL)
  {
    free(sessions[number].copyright);
    sessions[number].copyright=(char *)NULL;
  }
  if(sessions[number].vendor!=(char *)NULL)
  {
    free(sessions[number].vendor);
    sessions[number].vendor=(char *)NULL;
  }

  if(sessions[number].filename!=(char *)NULL)
  {
    free(sessions[number].filename);
    sessions[number].filename=(char *)NULL;
  }

  if(sessions[number].dirname!=(char *)NULL)
  {
    free(sessions[number].dirname);
    sessions[number].dirname=(char *)NULL;
  }
  HS_FreeSessionData(number);
  item_DeleteAll(tk_display,sessions[number].ls_topics);
  item_DeleteAll(tk_display,sessions[number].ls_glossary);

  return 0;
}




int HS_GetHelpFileFormat(number)
unsigned int number;
{
  char *ff;
  int ret,i;

  if(sessions[number].file==-1)
    return -1;
 
  ff=malloc(10);
  memset(ff,0,10);
  ret=read(sessions[number].file,ff,7);
  if(ret<0)
  {
    free(ff);
    fprintf(stderr,"  read failure %d  \n",ret);  
    return ret;
  }
  ff[7]=0;

  /*fprintf(stderr,"ff=__%s__ ",ff);*/

  if(strcmp(ff,phf_header)==0)
  {
    free(ff);
    sessions[number].format=PHF;
    sessions[number].index=7;
    return 0;
  }
  else if(strcmp(ff,bhf_header)==0)
  {
    free(ff);
    sessions[number].index=7;
    sessions[number].format=BHF;
    return 0;
  }
  free(ff);
  fprintf(stderr,"Format ERROR \n");  
  return -1;
}




char *HS_ReadNextLine(number)
unsigned int number;
{
  char *text, ff;
  int ret, i, j;
  int size;

  if(sessions[number].file==-1)
    return (char *)NULL;


FUNC_START: 
  text=(char *)malloc(10);
  if(text==(char *)NULL)
    return (char *)NULL;

  memset(text,0,10);
  ff=0;
  size=10;
  /*fprintf(stderr,"Lines allocated \n");*/ 
  

READ_START:
  ret=read(sessions[number].file,&ff,1);
  if(ret<=0)
  {
    free(text);
    fprintf(stderr,"I/0 failure return=%d  \n",ret); 
    return (char *)NULL;
  }
  if(ff==CR || ff==LF)
  {
    sessions[number].index++;
    /*lseek(sessions[number].file,sessions[number].index,0);*/
    /*fprintf(stderr,"CR ou LF detected \n");*/
    goto READ_START;  
  }


  i=0;
  while(ff!=CR && ff!=LF )
  {
	/*fprintf(stderr,"%c",ff);*/
	if(i>=size-1)
   	{
   	  text=(char *)realloc(text,size+10);
 	  if(text==(char *)NULL)
	  {
		fprintf(stderr,"text = NULL \n");
		return (char *)NULL;
	  }
	  for(j=size;j<size+10;j++)
	    text[j]=0;
	  size=size+10;
   	}
	text[i]=ff;
        ret=read(sessions[number].file,&ff,1);
        if(ret<=0)
        {
      	  if(text!=(char *)NULL) free(text);
      	  fprintf(stderr,"I/O failure return=%d\n",ret);
      	  return (char *)NULL;
        }
        i++;
  
  }
  /*fprintf(stderr,"eliminating blanks ...\n");*/
  while(i>=0 && (text[i]==CR || text[i]==LF || text[i]==SPACE || text[i]==0 ))
  {
	text[i]=0;
        i--;  
  }
  /*fprintf(stderr,"ok ...\n");*/
 
  if(text!=(char *)NULL && strlen(text)==0)
  {
    if(text!=(char *)NULL) 
      free(text);
    goto FUNC_START;
  }
  return (char *)text;
}






char *HS_ReadNextLineWithBlanks(number)
unsigned int number;
{
  char *text, ff;
  int ret, i, j;
  int size;

  if(sessions[number].file==-1)
    return (char *)NULL;

FUNC_START: 
  text=malloc(10);
  if(text==(char *)NULL)
    return (char *)NULL;
  memset(text,0,10);
  size=10;

READ_START:
  ret=read(sessions[number].file,&ff,1);
  if(ret<=0)
  {
    if(text!=(char *)NULL)
      free(text);
    fprintf(stderr,"I/0 failure return=%d  \n",ret);  
    return (char *)NULL;
  }
  if(ff==CR || ff==LF)
  {
    sessions[number].index++;
    /*lseek(sessions[number].file,sessions[number].index,0);
    fprintf(stderr,"CR ou LF detected \n");*/
    goto READ_START;  
  }


  i=0;
  while(ff!=CR && ff!=LF )
  {
	if(i>=size-1)
   	{
   	  text=(char *)realloc(text,size+10);
	  for(j=size;j<size+10;j++)
	    text[j]=0;
	  size=size+10;
   	}
	text[i]=ff;
        ret=read(sessions[number].file,&ff,1);
        if(ret<=0)
        {
          if(text!=(char *)NULL)
      	    free(text);
      	  /*fprintf(stderr,"I/O failure return=%d\n",ret); */ 
      	  return (char *)NULL;
        }
        i++;
  
  }
  while(i>=0 && (text[i]==CR || text[i]==LF || text[i]==0 ))
  {
	text[i]=0;
        i--;  
  }
  if(strlen(text)==0)
  {
    if(text!=(char *)NULL) free(text);
    goto FUNC_START;
  }
  return (char *)text;
}



#endif


