/*
 *
 * 	hs_resources.c
 * 	Help resources management
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


#ifndef _IMAN_HS_RESOURCES_C
#define _IMAN_HS_RESOURCES_C



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







int HS_GetUnusedResource(number)
unsigned int number;
{
  int i, j;
  int ret;

  if(number< maxsessions)
  {
    if(sessions[number].numresources>=sessions[number].maxresources)
    {
	if(sessions[number].maxresources<=0)
	  sessions[number].resources=(HelpResource *)malloc(sizeof(HelpResource)*(10));	
	else
	  sessions[number].resources=(HelpResource *)realloc(sessions[number].resources,sizeof(HelpResource)*(sessions[number].maxresources +10));	
	if(sessions[number].resources==(HelpResource *)NULL)
	{
	   HS_UnuseSession(number);
	   return -10;
	}
	sessions[number].maxresources=sessions[number].maxresources+10;
	for(i=1;i<=10;i++)
	  HS_InitResource(number,sessions[number].maxresources-i);
	return sessions[number].numresources;
    }
    for(i=0;i<sessions[number].maxresources;i++)
	if(sessions[number].resources[i].isUsed==False)
	  return i;
    return -1;
  }
  return -1;
}



int HS_GetResourceIndexFromNumber(number,res_number)
unsigned int number,res_number;
{
  int i, j;
  int ret;

  if(number<maxsessions)
  {
    for(i=0;i<sessions[number].maxresources;i++)
    if(sessions[number].resources[i].isUsed==True && sessions[number].resources[i].number==res_number)
      return i;
  }
  return -1;
}



int HS_InitResource(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(number< maxsessions)
  {
    if(index<sessions[number].maxresources)
    {
	sessions[number].resources[index].isUsed=False;
	sessions[number].resources[index].isLoaded=False;
	sessions[number].resources[index].class=0;
	sessions[number].resources[index].type=0;	
	sessions[number].resources[index].number=-1;
	sessions[number].resources[index].name=(char *)NULL;
 	sessions[number].resources[index].loading=0;	
	sessions[number].resources[index].pixmap=0;
	sessions[number].resources[index].bitmap=0;
	sessions[number].resources[index].width=0;
	sessions[number].resources[index].height=0;
	sessions[number].resources[index].font=(XFontStruct *)NULL;
	sessions[number].resources[index].color=0;
	sessions[number].resources[index].seek=-1;

   }
    return -1;
  }
  return -1;
}



int HS_UseResource(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(number< maxsessions)
  {
    if(index<sessions[number].maxresources && sessions[number].resources[index].isUsed==False)
    {	
	HS_InitResource(number,index);
	sessions[number].resources[index].isUsed=True;
  	sessions[number].numresources++;
    }
    return -1;
  }
  return -1;
}




int HS_UnuseResource(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(index<sessions[number].maxresources && sessions[number].resources[index].isUsed==True)
  {

    if(sessions[number].resources[index].class==PIXMAPCLASS)
	XFreePixmap(tk_display->display,sessions[number].resources[index].pixmap);
    else if(sessions[number].resources[index].class==BITMAPCLASS)
	HS_UnloadBitmap(number,index);
    else if(sessions[number].resources[index].class==FONTCLASS)
	HS_UnloadFont(number,index);
    else if(sessions[number].resources[index].class==COLORCLASS)
	HS_UnloadColor(number,index);

    if(sessions[number].resources[index].name!=(char *)NULL)
      free(sessions[number].resources[index].name);

    sessions[number].resources[index].seek=-1;
    sessions[number].resources[index].class=0;
    sessions[number].resources[index].loading=0;
    sessions[number].resources[index].number=-1;
    sessions[number].resources[index].isUsed=False;
    sessions[number].resources[index].isLoaded=False;
    sessions[number].resources[index].pixmap=0;
    sessions[number].resources[index].bitmap=0;
    sessions[number].resources[index].width=0;
    sessions[number].resources[index].height=0;
    sessions[number].resources[index].color=0;
    sessions[number].resources[index].font=(XFontStruct *)NULL;
    sessions[number].resources[index].name=(char *)NULL;
    sessions[number].numresources--;
    return 0;
  }
  return -1;
}




int HS_FreeResources(number)
unsigned int number;
{
  int i, j;
  int ret;

  if(number<maxsessions)
  {
    if(sessions[number].maxresources>0)
    for(i=0;i<sessions[number].maxresources;i++)
      	HS_UnuseResource(number,i);
    free((char *)sessions[number].resources);

    sessions[number].numresources=0;
    sessions[number].maxresources=0;
    sessions[number].loadedresources=0;
    sessions[number].resources=(HelpResource *)NULL;
    return 0;
  }
  return -1;
}





int HS_VerifyResource(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(number<maxsessions && index<sessions[number].maxresources)
  {
    if((sessions[number].resources[index].class==BITMAPCLASS || sessions[number].resources[index].class==PIXMAPCLASS) && sessions[number].resources[index].type==Inside )
      goto STEP1;
    if(sessions[number].resources[index].name==(char *)NULL)
      return -1;
STEP1:
    if(sessions[number].resources[index].loading<=0)
    {
      sessions[number].resources[index].loading=LoadDynamic;
      sessions[number].resources[index].isLoaded=False;
    }
    if(sessions[number].resources[index].class<=0)
      return -2;
    if(sessions[number].resources[index].type<=0)
      return -3;
    if(sessions[number].resources[index].loading==LoadAtStart && sessions[number].resources[index].isLoaded==False)
      return -4;

      for(j=0;j<sessions[number].maxresources;j++)
      if(sessions[number].resources[j].isUsed==True)
      {
	if(sessions[number].resources[j].number==sessions[number].resources[index].number && j!=index)
	  return -5;
      }

    return 0;
  }
  return -1;
}







XFontStruct *HS_LoadFont(number,index)
unsigned int number, index;
{
  int i, j;
  int ret;
  XFontStruct *font;

  if(number<maxsessions && index<sessions[number].maxresources)
  {
    if(sessions[number].resources[index].isLoaded==True)
    {
      return (XFontStruct *)NULL;
    }
    for(i=0;i<maxsessions;i++)
    if(sessions[i].isUsed==True)
      for(j=0;j<sessions[i].maxresources;j++)
      if(sessions[i].resources[j].isUsed==True)
      {
	if(sessions[i].resources[j].class==FONTCLASS && sessions[i].resources[j].isLoaded==True && strcmp(sessions[i].resources[j].name,sessions[number].resources[index].name)==0 && (i!=number || j!=index))
	  return (XFontStruct *) sessions[i].resources[j].font;
      }
    font=XLoadQueryFont(tk_display->display,sessions[number].resources[index].name);
    if(font==(XFontStruct *)NULL)
      font=tk_display->fonts.helvetica12;
    sessions[number].resources[index].isLoaded=True;
    return (XFontStruct *)font;
  }
  return (XFontStruct *)NULL;
}



int HS_UnloadFont(number,index)
unsigned int number, index;
{
  int i, j;
  int ret;
  XFontStruct *font;

  if(number<maxsessions && index<sessions[number].maxresources)
  {
    for(i=0;i<maxsessions;i++)
    if(sessions[i].isUsed==True)
      for(j=0;j<sessions[i].maxresources;j++)
      if(sessions[i].resources[j].isUsed==True)
      {
	if(sessions[i].resources[j].class==FONTCLASS && sessions[i].resources[j].isLoaded==True && strcmp(sessions[i].resources[j].name,sessions[number].resources[index].name)==0 && (i!=number || j!=index))
	  goto ERROR;
      }
    if(sessions[number].resources[index].font==tk_display->fonts.helvetica12)
      goto ERROR;
    if(sessions[number].resources[index].font!=(XFontStruct *)NULL) 
      XFreeFont(tk_display->display,sessions[number].resources[index].font);
    sessions[number].resources[index].font=(XFontStruct *)NULL;
    sessions[number].resources[index].isLoaded=False;
    return 0;
  }
ERROR:
  sessions[number].resources[index].font=(XFontStruct *)NULL;
  sessions[number].resources[index].isLoaded=False;
  return -1;
}



int HS_LoadColor(number,index)
unsigned int number, index;
{
  int i, j;
  int ret;
  XColor color, rgb;

  if(number<maxsessions && index<sessions[number].maxresources)
  {
    if(sessions[number].resources[index].isLoaded==True)
      return 0;
 ret=XAllocNamedColor(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),sessions[number].resources[index].name,&color,&rgb);    
    if(ret==0)
      return -1;
    sessions[number].resources[index].color=color.pixel;
    sessions[number].resources[index].isLoaded=True;
    return 0;
  }
  return -1;
}



int HS_UnloadColor(number,index)
unsigned int number, index;
{
  int i, j;
  int ret;
  XFontStruct *font;

  if(number<maxsessions && index<sessions[number].maxresources)
  {
    for(i=0;i<maxsessions;i++)
    if(sessions[i].isUsed==True)
      for(j=0;j<sessions[i].maxresources;j++)
      if(sessions[i].resources[j].isUsed==True)
      {
	if(sessions[i].resources[j].class==COLORCLASS && sessions[i].resources[j].isLoaded==True && sessions[i].resources[j].color==sessions[number].resources[index].color && (i!=number || j!=index))
	  goto ERROR;
      }
    if(sessions[number].resources[index].color==red1)
      goto ERROR;
    if(sessions[number].resources[index].color==yellow1)
      goto ERROR;
    if(sessions[number].resources[index].color==topicjump)
      goto ERROR;
    if(sessions[number].resources[index].color==glosdef)
      goto ERROR;

    if(IsColorUsed(tk_display,sessions[number].resources[index].color)==True)
      goto ERROR;
    XFreeColors(tk_display->display,DefaultColormap(tk_display->display,tk_display->screen),&sessions[number].resources[index].color,1,0);
    sessions[number].resources[index].color=0;
    sessions[number].resources[index].isLoaded=False;
    return 0;
  }
ERROR :
  sessions[number].resources[index].color=0;
  sessions[number].resources[index].isLoaded=False;
  return -1;
}





int HS_LoadBitmap(number,index)
unsigned int number, index;
{
  int i, j;
  unsigned int width, height;
  int ret;


  if(number<maxsessions && index<sessions[number].maxresources)
  {
    if(sessions[number].resources[index].isLoaded==True)
      return 0;

    for(i=0;i<maxsessions;i++)
    if(sessions[i].isUsed==True)
      for(j=0;j<sessions[i].maxresources;j++)
      if(sessions[i].resources[j].isUsed==True)
      {
	if(sessions[i].resources[j].class==BITMAPCLASS && sessions[i].resources[j].isLoaded==True && strcmp(sessions[i].resources[j].name,sessions[number].resources[index].name)==0 && (i!=number || j!=index))
	  {
		sessions[number].resources[index].bitmap=sessions[i].resources[j].bitmap;
	  	sessions[number].resources[index].width=sessions[i].resources[j].width;
    		sessions[number].resources[index].height=sessions[i].resources[j].height;
    		sessions[number].resources[index].isLoaded=True;
    		return 0;
	  }
      }
    if(sessions[number].resources[index].type==Outside)
    {   ret=XReadBitmapFile(tk_display->display,sessions[number].win_draw,sessions[number].resources[index].name,&width,&height,&sessions[number].resources[index].bitmap,&i,&j);    
      if(ret!=BitmapSuccess)
        return -1;
      sessions[number].resources[index].width=width;
      sessions[number].resources[index].height=height;
      sessions[number].resources[index].isLoaded=True;
      return 0;
    }
    else if(sessions[number].resources[index].type==Inside)
    {     
      fseek(sessions[number].stream,sessions[number].resources[index].seek,0);
      ret=ReadAndCreateBitmap(number,index);    
      if(ret!=BitmapSuccess)
        return -1;
      sessions[number].resources[index].isLoaded=True;
      return 0;
    }

  }
  return -1;
}




int HS_UnloadBitmap(number,index)
unsigned int number, index;
{
  int i, j;
  int ret;


  if(number<maxsessions && index<sessions[number].maxresources)
  {
    for(i=0;i<maxsessions;i++)
    if(sessions[i].isUsed==True)
      for(j=0;j<sessions[i].maxresources;j++)
      if(sessions[i].resources[j].isUsed==True)
      {
	if(sessions[i].resources[j].class==BITMAPCLASS && sessions[i].resources[j].isLoaded==True && sessions[i].resources[j].bitmap==sessions[number].resources[index].bitmap && (i!=number || j!=index))
	  goto ERROR;
      }
    XFreePixmap(tk_display->display,sessions[number].resources[index].bitmap);
    sessions[number].resources[index].bitmap=0;
    sessions[number].resources[index].isLoaded=False;
    return 0;
  }
ERROR :
  sessions[number].resources[index].bitmap=0;
  sessions[number].resources[index].isLoaded=False;
  return -1;
}





int ReadAndCreateBitmap(number,index)
unsigned int number, index;
{
   char *name_type;
   unsigned int width=0, height=0;
   char *text, *bits, *type, *data;
   int size;
   int ret;
   int i=0, j=0, k=0;
   Pixmap bitmap;
   FILE *stream; 
   char equ[5] , brace[5];


   if(number>=maxsessions || sessions[number].isUsed==False || index>= sessions[number].maxresources)
     return -1;

   stream=(FILE *)sessions[number].stream;
   if (stream==(FILE *)NULL) 
	return BitmapOpenFailed;

   ret=fseek(stream,sessions[number].resources[index].seek,SEEK_SET);
   if(ret==-1)
     return -1;

   text=malloc(256);
   name_type=malloc(256);

   if(text==(char *)NULL || name_type==(char *)NULL)
   {
ERROR:
	if(name_type!=(char *)NULL)
	  free(name_type);
	if(text!=(char *)NULL)
	  free(text);
    	return -1;
   }
   memset(text,0,256);
   memset(name_type,0,256);
   memset(equ,0,5);
   memset(brace,0,5);


    while ((width==0 || height==0) && fgets(text,255,stream)!=(char *)NULL) 
    {
        /*fprintf(stderr,"%s ",text);*/
	if (strlen(text)>=255) 
    	{
	    free(text);
	    free(data);
	    return BitmapFileInvalid;
	}
	if(sscanf(text,"#define %s %d",name_type,&j)==2) 
        {
	    type=strrchr(name_type,'_');
	    if(type==(char *)NULL)
	      type=name_type;
	    else
	      type++;

	    if (strcmp("width",type)==0)
	      width=(unsigned int)j;
	    else if (strcmp("height",type)==0)
	      height=(unsigned int)j;
	    /*fprintf(stderr,"w=%d h=%d  ",width, height);*/
    	}
    }
    memset(text,0,256);
    memset(name_type,0,256);
    /*fprintf(stderr,"  SUITE ...  ");*/

    ret=fscanf(stream,"static char %s %s",text,name_type);
    while(ret==2 && strcmp(name_type,"=")!=0)    
    {
	/*fprintf(stderr,">>%s ",text);*/
        memset(text,0,256);
        memset(name_type,0,256);
	ret=fscanf(stream,"#define %s %s",text,name_type);
    }
    /*fprintf(stderr,"ret=%d ====> %s + %s",ret,text,name_type);*/
    if(ret!=2)
      goto ERROR;
    /*fprintf(stderr,"  SUITE 2 ...  ");*/
   
    type=strrchr(text,'_');
    if(type==(char *)NULL)
      type=name_type;
    else
      type++;

    if (strcmp("bits[]",type)!=0)
      goto ERROR;


    data=malloc((width*height/8)+1);
    if(data==(char *)NULL)
	goto ERROR;
    ret=fscanf(stream,"%s",text);
    if(ret==0 || ret==EOF || strcmp(text,"{")!=0)
       goto ERROR;
    memset(data,0,(width*height/8)+1);
    memset(text,0,256);
    memset(name_type,0,256);

    

    for(i=0;i<(width*height/8);i++)
    {
	    ret=fscanf(stream,"%s",text);
	    /*fprintf(stderr,"%s ",text);*/
	    if(ret==0 || strcmp(text,"}")==0 )
	    {
ERROR2 :
		free(data);
		goto ERROR;
	    }
	    j=strlen(text);
	    if(j>0)
	    {
		if(text[j-1]==',')
	      	  text[j-1]=0;
		else if(text[j-1]=='}')
		  text[j-1]=0;
		else if(text[j-1]==';')
		  text[j-2]=0;
	    }
	    data[i]=(char)strtol(text,(char **)NULL,16);
    }
   /*fprintf(stderr,"i=%d w=%d h=%d  ",i,width, height);*/
   bitmap=XCreateBitmapFromData(tk_display->display,sessions[number].win_draw,data,width,height);
   if(bitmap==0)
     goto ERROR2;
   sessions[number].resources[index].width=width;
   sessions[number].resources[index].height=height;
   sessions[number].resources[index].bitmap=bitmap;
   sessions[number].resources[index].isLoaded=True;
   free(data);
   if(name_type!=(char *)NULL)
     free(name_type);
   if(text!=(char *)NULL)
     free(text);

   return BitmapSuccess;

}




int HS_LoadPixmap(number,index)
unsigned int number, index;
{
  int i, j;
  unsigned int width, height;
  int ret;
  XpmAttributes xpm_attrib;

  xpm_attrib.valuemask=0;


  if(number<maxsessions && index<sessions[number].maxresources)
  {
    if(sessions[number].resources[index].isLoaded==True)
      return 0;
    for(i=0;i<maxsessions;i++)
    if(sessions[i].isUsed==True)
      for(j=0;j<sessions[i].maxresources;j++)
      if(sessions[i].resources[j].isUsed==True)
      {
	if(sessions[i].resources[j].class==PIXMAPCLASS && sessions[i].resources[j].isLoaded==True && strcmp(sessions[i].resources[j].name,sessions[number].resources[index].name)==0 && (i!=number || j!=index))
	  {
		sessions[number].resources[index].pixmap=sessions[i].resources[j].pixmap;
		sessions[number].resources[index].bitmap=sessions[i].resources[j].bitmap;
	  	sessions[number].resources[index].width=sessions[i].resources[j].width;
    		sessions[number].resources[index].height=sessions[i].resources[j].height;
    		sessions[number].resources[index].isLoaded=True;
    		return 0;
	  }
      }
  
    if(sessions[number].resources[index].type==Outside)
    {   ret=XpmReadFileToPixmap(tk_display->display,sessions[number].win_draw,sessions[number].resources[index].name,&sessions[number].resources[index].pixmap,&sessions[number].resources[index].bitmap,&xpm_attrib);    
      if(ret!=XpmSuccess)
        return -1;
      sessions[number].resources[index].width=xpm_attrib.width;
      sessions[number].resources[index].height=xpm_attrib.height;
      sessions[number].resources[index].isLoaded=True;
      return 0;
    }
    else if(sessions[number].resources[index].type==Inside)
    {     
      fseek(sessions[number].stream,sessions[number].resources[index].seek,0);
      ret=ReadAndCreatePixmap(number,index);    
      if(ret!=XpmSuccess)
        return -1;
      sessions[number].resources[index].isLoaded=True;
      /*XSetForeground(tk_display->display,gc,tk_display->win_colors.text);		XCopyArea(tk_display->display,sessions[number].resources[index].pixmap,sessions[number].win_draw,gc,0,0,sessions[number].resources[index].width+10,sessions[number].resources[index].height+10,0,50);*/
      return 0;
    } 

  }
  return -1;
}




int HS_UnloadPixmap(number,index)
unsigned int number, index;
{
  int i, j;
  int ret;


  if(number<maxsessions && index<sessions[number].maxresources)
  {
    for(i=0;i<maxsessions;i++)
    if(sessions[i].isUsed==True)
      for(j=0;j<sessions[i].maxresources;j++)
      if(sessions[i].resources[j].isUsed==True)
      {
	if(sessions[i].resources[j].class==PIXMAPCLASS && sessions[i].resources[j].isLoaded==True && sessions[i].resources[j].pixmap==sessions[number].resources[index].pixmap && (i!=number || j!=index))
	  goto ERROR;
      }
    XFreePixmap(tk_display->display,sessions[number].resources[index].pixmap);
    XFreePixmap(tk_display->display,sessions[number].resources[index].bitmap);
    sessions[number].resources[index].pixmap=0;
    sessions[number].resources[index].bitmap=0;
    sessions[number].resources[index].isLoaded=False;
    return 0;
  }
ERROR :
  sessions[number].resources[index].pixmap=0;
  sessions[number].resources[index].bitmap=0;
  sessions[number].resources[index].isLoaded=False;
  return -1;
}




int ReadAndCreatePixmap(number,index)
unsigned int number, index;
{
   char *text, **data;
   int num,max;
   int ret;
   int i=0, j=0, k=0;
   XpmAttributes xpm_attrib;   


   if(number>=maxsessions || sessions[number].isUsed==False || index>= sessions[number].maxresources)
     return -1;

   if (sessions[number].file<0) 
	return XpmOpenFailed;

   ret=lseek(sessions[number].file,sessions[number].resources[index].seek,SEEK_SET);
   if(ret==-1)
     return -1;
   data=(char **)malloc(10*sizeof(char *));
   if(data==(char **)NULL)
     goto ERROR;
   num=0;   
   max=10;
   xpm_attrib.valuemask=0;
  
STARTFUNC:
   text=HS_ReadNextLine(number);
   if(text==(char *)NULL)
 	goto ERROR;

   if(strncmp(text,"/*",2)==0)
   {
 	free(text);
	goto STARTFUNC;
   }
   else if(strncmp(text,"static char",11)==0)
   {
 	free(text);
	text=" ";
	while(strncmp(text,"};",2)!=0)
	{
	  text=HS_ReadNextLine(number);
	  if(text==(char *)NULL)
	    goto ERROR;
	  if(strncmp(text,"/*",2)==0)
 	    free(text);
	  else
	  {
		if(num>=max)
		{
		  data=(char **)realloc(data,(max+10)*sizeof(char *));
		  max=max+10;
		}
		i=strlen(text);
		if(text[i-1]==',')
		{
		  text[i-1]=0;
		  text[i-2]=0;
		}
		else if(text[i-1]=='"')
		  text[i-1]=0;
		else if(text[i-1]==';')
		{
		  text[i-1]=0;
		  text[i-2]=0;
		  for(j=i;j>=0;j--)
		    if(text[j]=='}')
		 	text[j]=0;
		  
		  data[num]=text+1;
		  num++;
		  goto CONTINUEFUNC;
		}


		data[num]=text+1;
		num++;

	  }	  
	}
	
   }

CONTINUEFUNC:   ret=XpmCreatePixmapFromData(tk_display->display,sessions[number].win_draw,data,&sessions[number].resources[index].pixmap,&sessions[number].resources[index].bitmap,&xpm_attrib);    
      if(ret!=XpmSuccess)
        return -1;
      sessions[number].resources[index].width=xpm_attrib.width;
      sessions[number].resources[index].height=xpm_attrib.height;
      sessions[number].resources[index].isLoaded=True;
      return XpmSuccess;

ERROR:
	if(num>0)
	for(i=0;i<num;i++)
	  if(data[num]!=(char *)NULL)
	  free(data[num]);
	if(text!=(char *)NULL)
	  free(text);
	if(data!=(char **)NULL)
	  free(data);
    	return -1;

}



#endif

