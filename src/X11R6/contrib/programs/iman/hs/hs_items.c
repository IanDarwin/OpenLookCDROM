/*
 *
 * 	hs_items.c
 * 	Help items management
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


#ifndef _IMAN_HS_ITEMS_C
#define _IMAN_HS_ITEMS_C



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



#define newaction	"ACTION"
#define newtext		"TEXT"
#define newresnumber	"RESNUMBER"
#define newtopicnumber	"TOPICNUMBER"
#define newspacing	"SPACING"
#define newmargin	"MARGIN"
#define enditems	"ENDITEMS"
#define newitem		"NEWITEM"
#define newjustify	"JUSTIFY"




int HS_GetUnusedItem(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(number<maxsessions && index<sessions[number].maxtopics  && sessions[number].topics[index].isUsed==True)
  {
    if(sessions[number].topics[index].numitems>=sessions[number].topics[index].maxitems)
    {
	if(sessions[number].topics[index].maxitems<=0)
	  sessions[number].topics[index].items=(HelpItem *)malloc(sizeof(HelpItem)*(10));	
	else
	  sessions[number].topics[index].items=(HelpItem *)realloc(sessions[number].topics[index].items,sizeof(HelpItem)*(sessions[number].topics[index].maxitems +10));	
	if(sessions[number].topics[index].items==(HelpItem *)NULL)
	{
	   HS_UnuseSession(number);
	   return -10;
	}
	sessions[number].topics[index].maxitems=sessions[number].topics[index].maxitems+10;
	for(i=1;i<=10;i++)
	  HS_InitItem(number,index,sessions[number].topics[index].maxitems-i);
	return sessions[number].topics[index].numitems;
    }
    for(i=0;i<sessions[number].topics[index].maxitems;i++)
	if(sessions[number].topics[index].items[i].isUsed==False)
	  return i;
    return -1;
  }
  return -1;
}




int HS_InitItem(number,index,it)
unsigned int number,index,it;
{
  int i, j;
  int ret;

  if(number< maxsessions)
  {
    if(index<sessions[number].maxtopics)
    {
	sessions[number].topics[index].items[it].isUsed=False;
	sessions[number].topics[index].items[it].action=0;
	sessions[number].topics[index].items[it].text=(char *)NULL;
 	sessions[number].topics[index].items[it].res_number=-1;	
	sessions[number].topics[index].items[it].topic_number=-1;
	sessions[number].topics[index].items[it].num=-1;

   }
    return -1;
  }
  return -1;
}




int HS_UseItem(number,index,it)
unsigned int number,index,it;
{
  int i, j;
  int ret;

  if(number< maxsessions)
  {
    if(index<sessions[number].maxtopics && sessions[number].topics[index].items[it].isUsed==False)
    {	
	HS_InitItem(number,index,it);
	sessions[number].topics[index].items[it].isUsed=True;
  	sessions[number].topics[index].numitems++;
    }
    return -1;
  }
  return -1;
}




int HS_UnuseItem(number,index,it)
unsigned int number,index,it;
{
  int i, j;
  int ret;

  if(index<sessions[number].maxtopics && sessions[number].topics[index].items[it].isUsed==True)
  {
    if(sessions[number].topics[index].items[it].text!=(char *)NULL)
      free(sessions[number].topics[index].items[it].text);

    sessions[number].topics[index].items[it].isUsed=False;
    sessions[number].topics[index].items[it].action=0;	
    sessions[number].topics[index].items[it].text=(char *)NULL;
    sessions[number].topics[index].items[it].res_number=-1;	
    sessions[number].topics[index].items[it].topic_number=-1;
    sessions[number].topics[index].items[it].num=-1;
    sessions[number].topics[index].numitems--;

    return 0;
  }
  return -1;
}




int HS_FreeItems(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;
  unsigned int numero;

  if(number>=maxsessions)
    return -1;
  if(index>=sessions[number].maxtopics)
    return -2;

  for(i=0;i<sessions[number].topics[index].maxitems;i++)
  if(sessions[number].topics[index].items[i].isUsed==True)
  {
    if(sessions[number].topics[index].items[i].text!=(char *)NULL)
	free(sessions[number].topics[index].items[i].text);
  }
  free((char *)sessions[number].topics[index].items);
  sessions[number].topics[index].numitems=0;
  sessions[number].topics[index].maxitems=0;
  sessions[number].topics[index].items=(HelpItem *)NULL;
  return 0;
}






int HS_VerifyItem(number,index,it)
unsigned int number,index,it;
{
  int i, j;
  int ret;

  if(number<maxsessions && index<sessions[number].maxtopics)
  {
    if(sessions[number].topics[index].items[it].action==0)
      return -1;
    if(sessions[number].topics[index].items[it].action==DrawString && sessions[number].topics[index].items[it].text==(char *)NULL)
      return -1;

    if(sessions[number].topics[index].items[it].action==DrawPixmap && sessions[number].topics[index].items[it].res_number<0)
      return -1;
   if(sessions[number].topics[index].items[it].action==DrawPixmap && sessions[number].topics[index].items[it].res_number>=sessions[number].maxresources)
      return -1;
   if(sessions[number].topics[index].items[it].action==DrawPixmap && sessions[number].resources[sessions[number].topics[index].items[it].res_number].isUsed==False)
      return -1;
  if(sessions[number].topics[index].items[it].action==DrawPixmap && sessions[number].resources[sessions[number].topics[index].items[it].res_number].class!=PIXMAPCLASS)
      return -1;

    if(sessions[number].topics[index].items[it].action==DrawBitmap && sessions[number].topics[index].items[it].res_number<0)
      return -1;
   if(sessions[number].topics[index].items[it].action==DrawBitmap && sessions[number].topics[index].items[it].res_number>=sessions[number].maxresources)
      return -1;
   if(sessions[number].topics[index].items[it].action==DrawBitmap && sessions[number].resources[sessions[number].topics[index].items[it].res_number].isUsed==False)
      return -1;
  if(sessions[number].topics[index].items[it].action==DrawBitmap && sessions[number].resources[sessions[number].topics[index].items[it].res_number].class!=BITMAPCLASS)
      return -1;

    if(sessions[number].topics[index].items[it].action==SetFont && sessions[number].topics[index].items[it].res_number<0)
      return -1;
   if(sessions[number].topics[index].items[it].action==SetFont && sessions[number].topics[index].items[it].res_number>=sessions[number].maxresources)
      return -1;
   if(sessions[number].topics[index].items[it].action==SetFont && sessions[number].resources[sessions[number].topics[index].items[it].res_number].isUsed==False)
      return -1;
   if(sessions[number].topics[index].items[it].action==SetFont && sessions[number].resources[sessions[number].topics[index].items[it].res_number].class!=FONTCLASS)
      return -1;

    if(sessions[number].topics[index].items[it].action==SetColor && sessions[number].topics[index].items[it].res_number<0)
      return -1;
   if(sessions[number].topics[index].items[it].action==SetColor && sessions[number].topics[index].items[it].res_number>=sessions[number].maxresources)
      return -1;
   if(sessions[number].topics[index].items[it].action==SetColor && sessions[number].resources[sessions[number].topics[index].items[it].res_number].isUsed==False)
      return -1;
   if(sessions[number].topics[index].items[it].action==SetColor && sessions[number].resources[sessions[number].topics[index].items[it].res_number].class!=COLORCLASS)
      return -1;

    if(sessions[number].topics[index].items[it].action==StartTopicJumper && sessions[number].topics[index].items[it].topic_number<0)
      return -1;
/*  if(sessions[number].topics[index].items[it].action==StartTopicJumper && sessions[number].topics[index].items[it].topic_number>=sessions[number].maxtopics)
      return -1;
  if(sessions[number].topics[index].items[it].action==StartTopicJumper && sessions[number].topics[sessions[number].topics[index].items[it].topic_number].isUsed==False)
      return -1;
*/
    if(sessions[number].topics[index].items[it].action==SetLeftMargin && sessions[number].topics[index].items[it].num<0)
      return -1;
    if(sessions[number].topics[index].items[it].action==SetSpacing && sessions[number].topics[index].items[it].num<0)
      return -1;

    return 0;
  }
  return -1;
}



int HS_LoadItems(number,index)
unsigned int number, index;
{
  int i, j;
  int ret;
  char *text;
  int current_item=-1;
  char *ltext=(char *)NULL;

  if(number<maxsessions && index<sessions[number].maxtopics)
  {
    /*fprintf(stderr,"--- Reading items --- \n");*/
    ret=lseek(sessions[number].file,sessions[number].topics[index].seek,SEEK_SET);
    if(ret==-1) return -1;
    current_item=-1;
    text=(char *)HS_ReadNextLineWithBlanks(number);
    if(text==(char *)NULL)
      goto ERROR;
    /*fprintf(stderr,"text=%s \n",text);*/

    while(text!=(char *)NULL && strncmp(text,enditems,strlen(enditems))!=0)
    {
      
    	ret=0;
	if(current_item>=0) ret=HS_VerifyItem(number,index,current_item);
 	if(ret<0) goto ERROR;
	/*fprintf(stderr,"-- New item : %d  ",current_item);*/

	current_item=HS_GetUnusedItem(number,index);
	if(current_item<0) goto ERROR;
	HS_UseItem(number,index,current_item);
	sessions[number].topics[index].items[current_item].isUsed=True;

	ltext=HS_LowerText(text);
	if(ltext==(char *)NULL)
	  goto ERROR;

	if(strncmp(ltext,"drawstring",10)==0)
	  i=sessions[number].topics[index].items[current_item].action=DrawString;
	else if(strncmp(ltext,"drawpixmap",10)==0)
	  i=sessions[number].topics[index].items[current_item].action=DrawPixmap;
	else if(strncmp(ltext,"drawbitmap",10)==0)
	  i=sessions[number].topics[index].items[current_item].action=DrawBitmap;
	else if(strncmp(ltext,"setfont",7)==0)
	  i=sessions[number].topics[index].items[current_item].action=SetFont;
	else if(strncmp(ltext,"setcolor",8)==0)
	  i=sessions[number].topics[index].items[current_item].action=SetColor;
	else if(strncmp(ltext,"setleftmargin",13)==0)
	  i=sessions[number].topics[index].items[current_item].action=SetLeftMargin;
	else if(strncmp(ltext,"setspacing",10)==0)
	  i=sessions[number].topics[index].items[current_item].action=SetSpacing;
	else if(strncmp(ltext,"newline",7)==0)
	  i=sessions[number].topics[index].items[current_item].action=NewLine;
	else if(strncmp(ltext,"starttopicjumper",16)==0)
	  i=sessions[number].topics[index].items[current_item].action=StartTopicJumper;
	else if(strncmp(ltext,"endtopicjumper",14)==0)
	  i=sessions[number].topics[index].items[current_item].action=EndTopicJumper;
	else if(strncmp(ltext,"setjustify",10)==0)
	  i=sessions[number].topics[index].items[current_item].action=SetJustify;
	else if(strncmp(ltext,"defaultcolor",12)==0)
	  i=sessions[number].topics[index].items[current_item].action=DefaultColor;
	else if(strncmp(ltext,"defaultfont",11)==0)
	  i=sessions[number].topics[index].items[current_item].action=DefaultFont;
	else if(strncmp(ltext,"startglossaryjumper",16)==0)
	  i=sessions[number].topics[index].items[current_item].action=StartGlossaryJumper;
	else if(strncmp(ltext,"endglossaryjumper",14)==0)
	  i=sessions[number].topics[index].items[current_item].action=EndGlossaryJumper;
	else
	{
BAD_ITEM:
	  HS_UnuseItem(number,index,current_item);
	  current_item=-1;
	  goto READ_NEW_LINE;
	}	

	/*fprintf(stderr,"Action      : %d\n",i);*/

        if(i==DrawPixmap || i==DrawBitmap)
        {
	 if(current_item<0) goto ERROR;
	 sessions[number].topics[index].items[current_item].res_number=-1;
	 j=(int)strtol(text+strlen("drawpixmap")+1,(char **)NULL,10);
	 sessions[number].topics[index].items[current_item].res_number=HS_GetResourceIndexFromNumber(number,j);
	 if(sessions[number].topics[index].items[current_item].res_number==-1)
	   goto BAD_ITEM;
	 /*fprintf(stderr,"Resnumber   : %d\n",sessions[number].topics[index].items[current_item].res_number);*/
        }
	else if(i==SetFont)
        {
	 if(current_item<0) goto ERROR;
	 sessions[number].topics[index].items[current_item].res_number=-1;
	 j=(int)strtol(text+strlen("setfont")+1,(char **)NULL,10);
	 sessions[number].topics[index].items[current_item].res_number=HS_GetResourceIndexFromNumber(number,j);
	 if(sessions[number].topics[index].items[current_item].res_number==-1)
	   sessions[number].topics[index].items[current_item].action=DefaultFont;
	 /*fprintf(stderr,"Resnumber   : %d\n",sessions[number].topics[index].items[current_item].res_number);*/
        }
	else if(i==SetColor)
        {
	 if(current_item<0) goto ERROR;
	 sessions[number].topics[index].items[current_item].res_number=-1;
	 j=(int)strtol(text+strlen("setcolor")+1,(char **)NULL,10);
	 sessions[number].topics[index].items[current_item].res_number=HS_GetResourceIndexFromNumber(number,j);
	 if(sessions[number].topics[index].items[current_item].res_number==-1)
	   sessions[number].topics[index].items[current_item].action=DefaultColor;
	 /*fprintf(stderr,"Resnumber   : %d\n",sessions[number].topics[index].items[current_item].res_number);*/
        }
        else if(i==StartTopicJumper)
        {
	 if(current_item<0) goto ERROR;
	 sessions[number].topics[index].items[current_item].topic_number=-1;
	 j=(int)strtol(text+strlen("starttopicjumper")+1,(char **)NULL,10);
	 sessions[number].topics[index].items[current_item].topic_number=j; 

	 /*fprintf(stderr,"Topicnumber : %d\n",sessions[number].topics[index].items[current_item].topic_number );*/
        }
        else if(i==StartGlossaryJumper)
        {
	 if(current_item<0) goto ERROR;
	 sessions[number].topics[index].items[current_item].def_number=-1;
	 j=(int)strtol(text+strlen("startglossaryjumper")+1,(char **)NULL,10);
	 sessions[number].topics[index].items[current_item].def_number=j; 

	 /*fprintf(stderr,"Glossarynumber : %d\n",sessions[number].topics[index].items[current_item].def_number );*/
        }
        else if(i==SetSpacing)
        {
	 if(current_item<0) goto ERROR;
	 sessions[number].topics[index].items[current_item].num=-1;
	 j=(int)strtol(text+strlen("setspacing")+1,(char **)NULL,10);
	 sessions[number].topics[index].items[current_item].num=j; 

	 /*fprintf(stderr,"Spacing     : %d\n",sessions[number].topics[index].items[current_item].num );*/
        }
        else if(i==SetLeftMargin)
        {
	 if(current_item<0) goto ERROR;
	 sessions[number].topics[index].items[current_item].num=-1;
	 j=(int)strtol(text+strlen("setleftmargin")+1,(char **)NULL,10);
	 sessions[number].topics[index].items[current_item].num=j; 

	 /*fprintf(stderr,"Margin      : %d\n",sessions[number].topics[index].items[current_item].num );*/
        }
        else if(i==SetJustify)
        {
	 if(current_item<0) goto ERROR;
	 if(strncmp(ltext+strlen("setjustify")+1,"left",4)==0)
	   j=sessions[number].topics[index].items[current_item].num=LeftJustify;
	 else if(strncmp(ltext+strlen("setjustify")+1,"right",5)==0)
	   j=sessions[number].topics[index].items[current_item].num=RightJustify;
	 else if(strncmp(ltext+strlen("setjustify")+1,"center",6)==0)
	   j=sessions[number].topics[index].items[current_item].num=CenterJustify;
	 /*fprintf(stderr,"Justify     : %d\n",j);*/
        }
        else if(i==DrawString)
        {
	 if(current_item<0) goto ERROR;
	 sessions[number].topics[index].items[current_item].text=malloc(strlen(text)-strlen("drawstring=")+1);
	 if(sessions[number].topics[index].items[current_item].text==(char *)NULL)
	   goto ERROR;
	 memset(sessions[number].topics[index].items[current_item].text,0,strlen(text)-strlen("drawstring=")+1);
	 strcpy(sessions[number].topics[index].items[current_item].text,text+strlen("drawstring")+1);
	 /*fprintf(stderr,"Text        : %s\n",sessions[number].topics[index].items[current_item].text );*/
       	}

READ_NEW_LINE:
	/*fprintf(stderr,"Before free...  ");*/
    	if(text!=(char *)NULL) 
	  free(text);
	/*fprintf(stderr,"Free 1 ... ");*/
	if(ltext!=(char *)NULL)
	  free(ltext);
	/*fprintf(stderr,"Free 2 ... ");*/
    	text=(char *)HS_ReadNextLineWithBlanks(number);
	/*fprintf(stderr,"Text read ... \n");*/
    }
    ret=0;
    /*fprintf(stderr,"Verifying\n");*/
    if(current_item>=0) ret=HS_VerifyItem(number,index,current_item);
    if(ret<0) goto ERROR;

    if(text==(char *)NULL)
      goto ERROR;
    free(text);
    HS_GetItemsDimensions(number,index);
    return 0;

ERROR:
    if(text!=(char *)NULL)
      free(text);
    return -1;
  }
  return 0;
}






int HS_GetItemsDimensions(number,index)
unsigned int number,index;
{
  int i, j, k;
  int ret;
  XFontStruct *font;

  if(number>=maxsessions)
    return -1;

  font=tk_display->fonts.helvetica12;

  i=index;
  if(sessions[number].topics[i].isUsed==True && sessions[number].topics[i].isLoaded==True)
  {
    for(j=0;j<sessions[number].topics[i].maxitems;j++)
    if(sessions[number].topics[i].items[j].isUsed==True)
    switch(sessions[number].topics[i].items[j].action)
    {
	case DrawString:
		sessions[number].topics[i].items[j].height=font->descent+font->ascent;
		sessions[number].topics[i].items[j].width=XTextWidth(font,sessions[number].topics[i].items[j].text,strlen(sessions[number].topics[i].items[j].text));
		break;

	case DrawBitmap :
	case DrawPixmap :

		k=sessions[number].topics[i].items[j].res_number;
		if(k<0)
		{
		  sessions[number].topics[i].items[j].width=0;
		  sessions[number].topics[i].items[j].height=0;
		}
		else
		{
		  sessions[number].topics[i].items[j].width=sessions[number].resources[k].width;
		  sessions[number].topics[i].items[j].height=sessions[number].resources[k].height;
		}

		break;


	case SetFont :
		k=sessions[number].topics[i].items[j].res_number;
		if(k>=0)
		  font=sessions[number].resources[k].font;
		sessions[number].topics[i].items[j].width=0;
		sessions[number].topics[i].items[j].height=0;
		break;

	default : 
		sessions[number].topics[i].items[j].width=0;
		sessions[number].topics[i].items[j].height=0;
		break;

    }
  }
  else return -1;
  return 0;
}




int HS_GetAllItemsDimensions(number)
unsigned int number;
{
  int i, j, k;
  int ret;
  XFontStruct *font;

  if(number>=maxsessions)
    return -1;

  font=tk_display->fonts.helvetica12;

  for(i=0;i<sessions[number].maxtopics;i++)
  if(sessions[number].topics[i].isUsed==True && sessions[number].topics[i].isLoaded==True )
  {
    for(j=0;j<sessions[number].topics[i].maxitems;j++)
    if(sessions[number].topics[i].items[j].isUsed==True)
    switch(sessions[number].topics[i].items[j].action)
    {
	case DrawString:
		sessions[number].topics[i].items[j].height=font->ascent+font->descent;
		sessions[number].topics[i].items[j].width=XTextWidth(font,sessions[number].topics[i].items[j].text,strlen(sessions[number].topics[i].items[j].text));
		break;

	case SetFont :
		k=sessions[number].topics[i].items[j].res_number;
		if(k>=0)
		  font=sessions[number].resources[k].font;
		sessions[number].topics[i].items[j].width=0;
		sessions[number].topics[i].items[j].height=0;
		break;

	case DrawBitmap :
	case DrawPixmap :

		k=sessions[number].topics[i].items[j].res_number;
		if(k<0)
		{
		  sessions[number].topics[i].items[j].width=0;
		  sessions[number].topics[i].items[j].height=0;
		}
		else
		{
		  sessions[number].topics[i].items[j].width=sessions[number].resources[k].width;
		  sessions[number].topics[i].items[j].height=sessions[number].resources[k].height;
		}

		break;

	default : 
		sessions[number].topics[i].items[j].width=0;
		sessions[number].topics[i].items[j].height=0;
		break;

    }
  }
  return 0;
}



char *HS_LowerText(text)
char *text;
{
  int i, j;
  int ret;
  char *ltext;

  if(text==(char *)NULL)
    return (char *)NULL;

  ret=strlen(text);
  ltext=(char *)malloc(ret+1);
  if(ltext==(char *)NULL)
    return (char *)NULL;
  memset(ltext,0,ret+1);

  for(i=0;i<ret;i++)
  {
    if(isupper(text[i]))
	ltext[i]=tolower(text[i]);
    else ltext[i]=text[i];
   
  }
  return (char *)ltext;
}



#endif

