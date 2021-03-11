/*
 *
 * 	hs_draw.c
 * 	Windows drawing
 *
 * 	Modification :  04/05/94
 *
 *	Copyright (c) 1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
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


#ifndef _IMAN_HS_DRAW_C
#define _IMAN_HS_DRAW_C



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


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "hs.h"





/*
 *
 * Main drawing
 *
 *
 */

int HS_Draw(number)
unsigned int number;
{
  int mask;
  int ret;
  int i, j, k, tlength;
  int x=LMARGE,y=UMARGE;
  unsigned int line;
  int margin, justify, spacing, jumper; 
  unsigned long color;
  XFontStruct *font;
  unsigned int index;
  unsigned int height, descent;
  Pixmap bitmap, pixmap;

 
  if(number<maxsessions && sessions[number].isUsed==True && sessions[number].numtopics>0 && (index=sessions[number].current_topic)>=0 && index<sessions[number].maxtopics && sessions[number].topics[index].isUsed==True && sessions[number].topics[index].numitems>0)
  {
    line=sessions[number].current_line;
    index=sessions[number].current_topic;
    y=UMARGE-sessions[number].inc;
    x=LMARGE+margin;
    font=(XFontStruct *)sessions[number].lines[line].font;
    color=sessions[number].lines[line].color;
    spacing=sessions[number].lines[line].spacing;
    margin=sessions[number].lines[line].margin;
    justify=sessions[number].lines[line].justify;
    jumper=sessions[number].lines[line].jumper;
    XSetForeground(tk_display->display,gc,color);
    XSetFont(tk_display->display,gc,font->fid);
    /*fprintf(stderr,"Drawing  y=%d  height=%d  ...\n",y,sessions[number].height);*/

    while(y<(int)sessions[number].height && line<sessions[number].numlines)
    {    
     	x=LMARGE+margin;
	if(justify==RightJustify)
  	  x=sessions[number].width-LMARGE-sessions[number].lines[line].width;
	else if(justify==CenterJustify)
  	  x=((sessions[number].width-sessions[number].lines[line].width)/2);
        
	height=sessions[number].lines[line].height;
	descent=sessions[number].lines[line].descent;
	if(descent<=0)
	  descent=font->descent;
        /*fprintf(stderr,"Line %d :  height=%d  descent=%d  spacing=%d\n",line,height,descent,spacing);*/

 	for(i=sessions[number].lines[line].start_item;i<=sessions[number].lines[line].end_item;i++)
	{
	  if(sessions[number].topics[index].items[i].action==SetColor)
	  {
	    	ret=sessions[number].topics[index].items[i].res_number;
	  	if(ret>=0) color=sessions[number].resources[ret].color;
		if(jumper==0) XSetForeground(tk_display->display,gc,color);
	  }
	  else if(sessions[number].topics[index].items[i].action==SetFont)
	  {
	  	ret=sessions[number].topics[index].items[i].res_number;
	  	if(ret>=0) font=(XFontStruct *)sessions[number].resources[ret].font;
		XSetFont(tk_display->display,gc,font->fid);
	  }
	  else if(sessions[number].topics[index].items[i].action==DrawBitmap)
	  {
		bitmap=sessions[number].resources[sessions[number].topics[index].items[i].res_number].bitmap;
		if(jumper==StartTopicJumper)
		  XSetForeground(tk_display->display,gc,topicjump);
		else if(jumper==StartGlossaryJumper)
		  XSetForeground(tk_display->display,gc,glosdef);
		
	  	if(sessions[number].topics[index].items[i].height+descent<=height+spacing)
		  XCopyPlane(tk_display->display,bitmap,sessions[number].win_draw,gc,0,0,sessions[number].topics[index].items[i].width,sessions[number].topics[index].items[i].height,x,y+height-descent-sessions[number].topics[index].items[i].height,1);
	  	else
		  XCopyPlane(tk_display->display,bitmap,sessions[number].win_draw,gc,0,0,sessions[number].topics[index].items[i].width,sessions[number].topics[index].items[i].height,x,y,1);
		if(jumper==StartTopicJumper)
		{
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+sessions[number].topics[index].items[i].width,y+height-1);
		}
		else if(jumper==StartGlossaryJumper)
		{
			XSetLineAttributes(tk_display->display,gc,0,LineOnOffDash,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+sessions[number].topics[index].items[i].width,y+height-1);
		}
	
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);

		x=x+sessions[number].topics[index].items[i].width;
		
	  }
	  else if(sessions[number].topics[index].items[i].action==DrawPixmap)
	  {
		pixmap=sessions[number].resources[sessions[number].topics[index].items[i].res_number].pixmap;
		if(jumper==StartTopicJumper)
		  XSetForeground(tk_display->display,gc,topicjump);
		else if(jumper==StartGlossaryJumper)
		  XSetForeground(tk_display->display,gc,glosdef);
		
	  	if(sessions[number].topics[index].items[i].height+descent<=height+spacing)
		  XCopyArea(tk_display->display,pixmap,sessions[number].win_draw,gc,0,0,sessions[number].topics[index].items[i].width,sessions[number].topics[index].items[i].height,x,y+height-descent-sessions[number].topics[index].items[i].height);
	  	else
		  XCopyArea(tk_display->display,pixmap,sessions[number].win_draw,gc,0,0,sessions[number].topics[index].items[i].width,sessions[number].topics[index].items[i].height,x,y);
		if(jumper==StartTopicJumper)
		{
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+sessions[number].topics[index].items[i].width,y+height-1);
		}
		else if(jumper==StartGlossaryJumper)
		{
			XSetLineAttributes(tk_display->display,gc,0,LineOnOffDash,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+sessions[number].topics[index].items[i].width,y+height-1);
		}
		XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
		x=x+sessions[number].topics[index].items[i].width;
		
	  }

	  else if(sessions[number].topics[index].items[i].action==SetSpacing)
	  {
	   	spacing=sessions[number].topics[index].items[i].num;
		/*fprintf(stderr,"Set spacing=%d\n",spacing);*/
	  }
	  else if(sessions[number].topics[index].items[i].action==SetLeftMargin)
	  {
	  	margin=sessions[number].topics[index].items[i].num;
		if(margin>=sessions[number].width-80)
		  margin=sessions[number].width-80;
		if(margin<0)
		  margin=0;
	  }
	  else if(sessions[number].topics[index].items[i].action==SetJustify)
	  	justify=sessions[number].topics[index].items[i].num;
	  else if(sessions[number].topics[index].items[i].action==StartTopicJumper && jumper<=0)
	  { 
	  	jumper=StartTopicJumper;
		XSetForeground(tk_display->display,gc,topicjump);
	  }
	  else if(sessions[number].topics[index].items[i].action==EndTopicJumper && jumper==StartTopicJumper)
	  { 
	  	jumper=0;
		XSetForeground(tk_display->display,gc,color);
	  }	  
	  else if(sessions[number].topics[index].items[i].action==StartGlossaryJumper && jumper<=0)
	  { 
	  	jumper=StartGlossaryJumper;
		XSetForeground(tk_display->display,gc,glosdef);
	  }
	  else if(sessions[number].topics[index].items[i].action==EndGlossaryJumper && jumper==StartGlossaryJumper)
	  {
	  	jumper=0;
		XSetForeground(tk_display->display,gc,color);
	  }	  
	  else if(sessions[number].topics[index].items[i].action==DefaultColor)
	  {
	  	color=BlackPixel(tk_display->display,tk_display->screen);
		if(jumper==0) XSetForeground(tk_display->display,gc,color);
	  }
	  else if(sessions[number].topics[index].items[i].action==DefaultFont)
	  {
	  	font=tk_display->fonts.helvetica12;
		XSetFont(tk_display->display,gc,font->fid);
	  }
	  else if(sessions[number].topics[index].items[i].action==DrawString)
	  {

	    if(i==sessions[number].lines[line].start_item && sessions[number].lines[line].start_char>=0)
       	    {
	      if(sessions[number].lines[line].start_item!=sessions[number].lines[line].end_item)
		ret=strlen(sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char);
	      else
	      {
		if(sessions[number].lines[line].end_char>=0)
		  ret=(sessions[number].lines[line].end_char-sessions[number].lines[line].start_char);
		else
		  ret=strlen(sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char);
	      }
	      tlength=XTextWidth(font,sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char,ret);	    
	      if(jumper==StartTopicJumper)
	      {
			XSetForeground(tk_display->display,gc,topicjump);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+tlength,y+height-1);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
	      }
	      else if(jumper==StartGlossaryJumper)
	      {
			XSetForeground(tk_display->display,gc,glosdef);
			XSetLineAttributes(tk_display->display,gc,0,LineOnOffDash,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+tlength,y+height-1);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
	      }

		if(sessions[number].lines[line].start_item!=sessions[number].lines[line].end_item)
		{    XDrawString(tk_display->display,sessions[number].win_draw,gc,x,y+height-descent,sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char,ret=strlen(sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char));
		  /*tlength=XTextWidth(font,sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char,ret);*/
		}
		else
		{
		  if(sessions[number].lines[line].end_char>=0)
  XDrawString(tk_display->display,sessions[number].win_draw,gc,x,y+height-descent,sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char,ret=(sessions[number].lines[line].end_char-sessions[number].lines[line].start_char));
		  else XDrawString(tk_display->display,sessions[number].win_draw,gc,x,y+height-descent,sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char,ret=strlen(sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char));
		    
		  /*tlength=XTextWidth(font,sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char,ret);*/
		}
		/*tlength=XTextWidth(font,sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char,ret);*/
		x=x+tlength;
	
	    }
	    else if(i==sessions[number].lines[line].end_item && sessions[number].lines[line].end_char>=0)
       	    {
		if(sessions[number].lines[line].start_item!=sessions[number].lines[line].end_item)
		{  
		  tlength=XTextWidth(font,sessions[number].topics[index].items[i].text,sessions[number].lines[line].end_char);
	    	  if(jumper==StartTopicJumper)
	    	  {
			XSetForeground(tk_display->display,gc,topicjump);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+tlength,y+height-1);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
	   	  }
 	    	  else if(jumper==StartGlossaryJumper)
	    	  {
			XSetForeground(tk_display->display,gc,glosdef);
			XSetLineAttributes(tk_display->display,gc,0,LineOnOffDash,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+tlength,y+height-1);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
	   	  }

XDrawString(tk_display->display,sessions[number].win_draw,gc,x,y+height-descent,sessions[number].topics[index].items[i].text,sessions[number].lines[line].end_char);
		  /*tlength=XTextWidth(font,sessions[number].topics[index].items[i].text,sessions[number].lines[line].end_char);*/
		}
		else
		{	
		  tlength=XTextWidth(font,sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char,sessions[number].lines[line].end_char-sessions[number].lines[line].start_char);
	    	  if(jumper==StartTopicJumper)
	    	  {
			XSetForeground(tk_display->display,gc,topicjump);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+tlength,y+height-1);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
	   	  }
	    	  else if(jumper==StartGlossaryJumper)
	    	  {
			XSetForeground(tk_display->display,gc,glosdef);
			XSetLineAttributes(tk_display->display,gc,0,LineOnOffDash,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+tlength,y+height-1);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
	   	  }

XDrawString(tk_display->display,sessions[number].win_draw,gc,x,y+height-descent,sessions[number].topics[index].items[i].text+sessions[number].lines[line].start_char,sessions[number].lines[line].end_char-sessions[number].lines[line].start_char);
		}
		x=x+tlength;	    	
	    }
	    else 
      	    {
		  tlength=XTextWidth(font,sessions[number].topics[index].items[i].text,ret=strlen(sessions[number].topics[index].items[i].text));
	    	  if(jumper==StartTopicJumper)
	    	  {
			XSetForeground(tk_display->display,gc,topicjump);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+tlength,y+height-1);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
	   	  }
		  else if(jumper==StartGlossaryJumper)
	    	  {
			XSetForeground(tk_display->display,gc,glosdef);
			XSetLineAttributes(tk_display->display,gc,0,LineOnOffDash,CapNotLast,JoinMiter);
			XDrawLine(tk_display->display,sessions[number].win_draw,gc,x,y+height-1,x+tlength,y+height-1);
			XSetLineAttributes(tk_display->display,gc,0,LineSolid,CapNotLast,JoinMiter);
	   	  } 		  
	XDrawString(tk_display->display,sessions[number].win_draw,gc,x,y+height-descent,sessions[number].topics[index].items[i].text,ret=strlen(sessions[number].topics[index].items[i].text));
		x=x+tlength;
	    }
	  }

	}
	if(sessions[number].lines[line].height>0)
      	  y=y+sessions[number].lines[line].height+sessions[number].lines[line].spacing;
      	line++;
	/*XSync(tk_display->display,False);*/

    }
 /*   else
    {	
  	for(i=sessions[number].lines[line].start_item;i<=sessions[number].lines[line].end_item;i++)
	{
	  if(sessions[number].topics[index].items[i].action==SetColor)
	  {
	    	ret=sessions[number].topics[index].items[i].res_number;
	  	if(ret>=0) color=sessions[number].resources[ret].color;
		if(jumper==0) XSetForeground(tk_display->display,gc,color);
	  }
	  else if(sessions[number].topics[index].items[i].action==SetFont)
	  {
	  	ret=sessions[number].topics[index].items[i].res_number;
	  	if(ret>=0) font=(XFontStruct *)sessions[number].resources[ret].font;
		XSetFont(tk_display->display,gc,font->fid);
	  }
	  else if(sessions[number].topics[index].items[i].action==SetSpacing)
	   	spacing=sessions[number].topics[index].items[i].num;
	  else if(sessions[number].topics[index].items[i].action==SetLeftMargin)
	  {
	  	margin=sessions[number].topics[index].items[i].num;
		if(margin>=sessions[number].width-80)
		  margin=sessions[number].width-80;
		if(margin<0)
		  margin=0;
	  }
	  else if(sessions[number].topics[index].items[i].action==SetJustify)
	  	justify=sessions[number].topics[index].items[i].num;
	  else if(sessions[number].topics[index].items[i].action==StartTopicJumper && jumper<=0)
	  { 
	  	jumper=StartTopicJumper;
		XSetForeground(tk_display->display,gc,topicjump);
	  }
	  else if(sessions[number].topics[index].items[i].action==EndTopicJumper && jumper==StartTopicJumper)
	  {
	  	jumper=0;
		XSetForeground(tk_display->display,gc,color);
	  }	  
	  else if(sessions[number].topics[index].items[i].action==StartGlossaryJumper && jumper<=0)
	  { 
	  	jumper=StartGlossaryJumper;
		XSetForeground(tk_display->display,gc,glosdef);
	  }
	  else if(sessions[number].topics[index].items[i].action==EndGlossaryJumper && jumper==StartGlossaryJumper)
	  {
	  	jumper=0;
		XSetForeground(tk_display->display,gc,glosdef);
	  }	  
	  else if(sessions[number].topics[index].items[i].action==DefaultColor)
	  {
	  	color=BlackPixel(tk_display->display,tk_display->screen);
		if(jumper==0) XSetForeground(tk_display->display,gc,color);
	  }
	  else if(sessions[number].topics[index].items[i].action==DefaultFont)
	  {
	  	font=tk_display->fonts.helvetica12;
		XSetFont(tk_display->display,gc,font->fid);
	  }

	}
	if(sessions[number].lines[line].height>0)
     	  y=y+sessions[number].lines[line].height+sessions[number].lines[line].spacing;
      	line++;
    }*/

    
  }
  else return -1;
  /*fprintf(stderr,"y=%d  ",y);*/
  return 0;
}





/*
 *
 * Error drawing
 *
 *
 */

int HS_DrawError(number, error_type)
unsigned int number, error_type;
{
  int ret, ret2;
  int x,y;

  switch(number)
  {
	case 0 :
		x=20;
		y=80;
		XSetClipOrigin(tk_display->display,gc,x,y);
		XSetForeground(tk_display->display,gc,red1);
		XSetClipMask(tk_display->display,gc,bm_stop1);
		XCopyPlane(tk_display->display,bm_stop1,sessions[number].win_error,gc,0,0,32,32,x,y,1);
		XSetForeground(tk_display->display,gc,WhitePixel(tk_display->display,tk_display->screen));
		XSetClipMask(tk_display->display,gc,bm_stop2);
		XCopyPlane(tk_display->display,bm_stop2,sessions[number].win_error,gc,0,0,32,32,x,y,1);
		XSetClipMask(tk_display->display,gc,None);
		XSetClipOrigin(tk_display->display,gc,0,0);
		break;


	default :
		break;

  }
  return 0;
}



int HS_MapError(number)
unsigned int number;
{
  if(number>=maxsessions)
    return -1;

  wid_SetFreeze(tk_display,sessions[number].win_main,True);
  wid_SetFreeze(tk_display,sessions[number].win_topics,True);
  wid_SetFreeze(tk_display,sessions[number].win_glossary,True);
  win_Map(tk_display,sessions[number].win_error);

  return 0;
}



int HS_UnmapError(number)
unsigned int number;
{
  if(number>=maxsessions)
    return -1;

  win_Unmap(tk_display,sessions[number].win_error);
  wid_SetFreeze(tk_display,sessions[number].win_main,False);
  wid_SetFreeze(tk_display,sessions[number].win_topics,False);
  wid_SetFreeze(tk_display,sessions[number].win_glossary,False);

  return 0;
}



/*
 *
 * Warning drawing
 *
 *
 */

int HS_DrawWarning(number, error_type)
unsigned int number, error_type;
{
 XGCValues xgcvalues;
 XEvent send_event, eventbis;

 int mask;
 int ret, ret2;
 int i, j, k, w, x, y;
 int index;

  switch(number)
  {
	case 0 :
		x=20;
		y=80;
		XSetClipOrigin(tk_display->display,gc,x,y);
		XSetForeground(tk_display->display,gc,yellow1);
		XSetClipMask(tk_display->display,gc,bm_warning1);
		XCopyPlane(tk_display->display,bm_warning1,sessions[number].win_warning,gc,0,0,32,32,x,y,1);
		XSetForeground(tk_display->display,gc,WhitePixel(tk_display->display,tk_display->screen));
		XSetClipMask(tk_display->display,gc,bm_warning2);
		XCopyPlane(tk_display->display,bm_warning2,sessions[number].win_warning,gc,0,0,32,32,x,y,1);
		/*XSetForeground(tk_display->display,gc,BlackPixel(tk_display->display,tk_display->screen));
		XSetClipMask(tk_display->display,gc,bm_warning3);
		XCopyPlane(tk_display->display,bm_warning3,sessions[number].win_warning,gc,0,0,32,32,x,y,1);*/
		XSetClipMask(tk_display->display,gc,None);
		XSetClipOrigin(tk_display->display,gc,0,0);
		break;


	default :
		break;

  }
  return 0;
}



int HS_MapWarning(number)
unsigned int number;
{
  if(number>=maxsessions)
    return -1;

  wid_SetFreeze(tk_display,sessions[number].win_main,True);
  wid_SetFreeze(tk_display,sessions[number].win_topics,True);
  wid_SetFreeze(tk_display,sessions[number].win_glossary,True);
  win_Map(tk_display,sessions[number].win_warning);

  return 0;
}



int HS_UnmapWarning(number)
unsigned int number;
{
  if(number>=maxsessions)
    return -1;

  win_Unmap(tk_display,sessions[number].win_warning);
  wid_SetFreeze(tk_display,sessions[number].win_main,False);
  wid_SetFreeze(tk_display,sessions[number].win_topics,False);
  wid_SetFreeze(tk_display,sessions[number].win_glossary,False);

  return 0;
}



int HS_MapAbout(number)
unsigned int number;
{
  if(number>=maxsessions)
    return -1;

  wid_SetFreeze(tk_display,sessions[number].win_main,True);
  wid_SetFreeze(tk_display,sessions[number].win_topics,True);
  wid_SetFreeze(tk_display,sessions[number].win_glossary,True);
  win_Map(tk_display,sessions[number].win_about);
  sessions[number].action=AboutAction;

  return 0;
}



int HS_UnmapAbout(number)
unsigned int number;
{
  if(number>=maxsessions)
    return -1;

  win_Unmap(tk_display,sessions[number].win_about);
  wid_SetFreeze(tk_display,sessions[number].win_main,False);
  wid_SetFreeze(tk_display,sessions[number].win_topics,False);
  wid_SetFreeze(tk_display,sessions[number].win_glossary,False);
  sessions[number].action=NoAction;

  return 0;
}




int HS_DrawAbout(number)
unsigned int number;
{
  char *str;
  int length, clength;
  char text[6];

  if(number>=maxsessions)
    return -1;

  XSetForeground(tk_display->display,gc,BlackPixel(tk_display->display,tk_display->screen));
  XSetClipOrigin(tk_display->display,gc,10,10);
  XSetClipMask(tk_display->display,gc,bm_help1);
  XCopyPlane(tk_display->display,bm_help1,sessions[number].win_about,gc,0,0,40,40,10,10,1);
  XSetForeground(tk_display->display,gc,WhitePixel(tk_display->display,tk_display->screen));
  XSetClipOrigin(tk_display->display,gc,10,10);
  XSetClipMask(tk_display->display,gc,bm_help2);
  XCopyPlane(tk_display->display,bm_help2,sessions[number].win_about,gc,0,0,40,40,10,10,1);
  XSetClipOrigin(tk_display->display,gc,0,0);
  XSetClipMask(tk_display->display,gc,None);

  XSetFont(tk_display->display,gc,tk_display->fonts.helvetica12->fid);
  XSetForeground(tk_display->display,gc,BlackPixel(tk_display->display,tk_display->screen));
  str="IMAN Help Server";
  length=XTextWidth(tk_display->fonts.helvetica12,str,strlen(str));
  XDrawString(tk_display->display,sessions[number].win_about,gc,(400-length)/2,27,str,strlen(str));
  str="Version 1.0";
  length=XTextWidth(tk_display->fonts.helvetica12,str,strlen(str));
  XDrawString(tk_display->display,sessions[number].win_about,gc,(400-length)/2,43,str,strlen(str));
  str="Copyright (c) 1994 Bruno RIVAS";
  length=XTextWidth(tk_display->fonts.helvetica12,str,strlen(str));
  XDrawString(tk_display->display,sessions[number].win_about,gc,(400-length)/2,59,str,strlen(str));

  XDrawLine(tk_display->display,sessions[number].win_about,gc,20,75,380,75);
  XDrawLine(tk_display->display,sessions[number].win_about,gc,20,200,380,200);

  if(sessions[number].file<=0)
  {
	
    str="No help file opened";
    length=XTextWidth(tk_display->fonts.helvetica12,str,strlen(str));
    XDrawString(tk_display->display,sessions[number].win_about,gc,(400-length)/2,140,str,strlen(str));
    return 0;
  }


  /*XSetFont(tk_display->display,gc,tk_display->fonts.f8_13->fid);*/
  str="Copyright : ";
  clength=XTextWidth(tk_display->fonts.helvetica12,str,strlen(str));
  str="File : ";
  length=XTextWidth(tk_display->fonts.helvetica12,str,strlen(str));
  XDrawString(tk_display->display,sessions[number].win_about,gc,20+clength-length,105,str,strlen(str));
  if(sessions[number].file>=0 && sessions[number].filename!=(char *)NULL)
    str=sessions[number].filename;
  else str="(None)";
  XDrawString(tk_display->display,sessions[number].win_about,gc,20+clength,105,str,strlen(str));

  str="Copyright : ";
  length=XTextWidth(tk_display->fonts.helvetica12,str,strlen(str));
  XDrawString(tk_display->display,sessions[number].win_about,gc,20,122,str,strlen(str));
  if(sessions[number].file>=0 && sessions[number].copyright!=(char *)NULL)
    str=sessions[number].copyright;
  else str="(None)";
  XDrawString(tk_display->display,sessions[number].win_about,gc,20+length,122,str,strlen(str));

  str="Vendor : ";
  length=XTextWidth(tk_display->fonts.helvetica12,str,strlen(str));
  XDrawString(tk_display->display,sessions[number].win_about,gc,20+clength-length,139,str,strlen(str));
  if(sessions[number].file>=0 && sessions[number].vendor!=(char *)NULL)
    str=sessions[number].vendor;
  else str="(None)";
  XDrawString(tk_display->display,sessions[number].win_about,gc,20+clength,139,str,strlen(str));

  str="Format : ";
  length=XTextWidth(tk_display->fonts.helvetica12,str,strlen(str));
  XDrawString(tk_display->display,sessions[number].win_about,gc,20+clength-length,156,str,strlen(str));
  if(sessions[number].file>=0 && sessions[number].format==PHF)
    str="PHF";
  else if(sessions[number].file>=0 && sessions[number].format==BHF)
    str="BHF";
  else str="(None)";
  XDrawString(tk_display->display,sessions[number].win_about,gc,20+clength,156,str,strlen(str));

  str="Date : ";
  length=XTextWidth(tk_display->fonts.helvetica12,str,strlen(str));
  XDrawString(tk_display->display,sessions[number].win_about,gc,20+clength-length,173,str,strlen(str));
  if(sessions[number].file>=0 && sessions[number].year>0 && sessions[number].month>0 && sessions[number].day>0)
  {
    memset(text,0,6);
    sprintf(text,"%02d/",sessions[number].month);
    length=XTextWidth(tk_display->fonts.helvetica12,text,strlen(text));  
    XDrawString(tk_display->display,sessions[number].win_about,gc,20+clength,173,text,strlen(text));
    memset(text,0,6);
    sprintf(text,"%02d/",sessions[number].day);
    XDrawString(tk_display->display,sessions[number].win_about,gc,20+clength+length,173,text,strlen(text));
    length=length+XTextWidth(tk_display->fonts.helvetica12,text,strlen(text));
    memset(text,0,6);
    sprintf(text,"%04d",sessions[number].year);
    XDrawString(tk_display->display,sessions[number].win_about,gc,20+clength+length,173,text,strlen(text));
  }
  else
  { 
    str="Unknown or misformed";
    XDrawString(tk_display->display,sessions[number].win_about,gc,20+clength,173,str,strlen(str));
  }



  return 0;
}



int HS_MapOpen(number)
unsigned int number;
{
  ItemPixmapDecoration item_pixmap;
  ItemTextDecoration item_text;
  WidgetTextDecoration wid_text;
  char *title;
  unsigned int i;

  if(number>=maxsessions)
    return -1;

  wid_SetFreeze(tk_display,sessions[number].win_main,True);
  wid_SetFreeze(tk_display,sessions[number].win_topics,True);
  wid_SetFreeze(tk_display,sessions[number].win_glossary,True);
 
  if(sessions[number].file>=0)
  {
    close(sessions[number].file);
    if(sessions[number].stream!=(FILE *)NULL)
      fclose(sessions[number].stream);
    sessions[number].file=-1;
    sessions[number].stream=(FILE *)NULL;
  }
  sessions[number].format=0;
  sessions[number].version=-1;
  sessions[number].release=-1;
  sessions[number].index=0;
  sessions[number].year=0;
  sessions[number].month=0;
  sessions[number].day=0;

  if(sessions[number].help_name!=(char *)NULL)
  {
    free(sessions[number].help_name);
    sessions[number].help_name=(char *)NULL;
  }
  if(sessions[number].vendor!=(char *)NULL)
  {
    free(sessions[number].vendor);
    sessions[number].vendor=(char *)NULL;
  }
  if(sessions[number].copyright!=(char *)NULL)
  {
    free(sessions[number].copyright);
    sessions[number].copyright=(char *)NULL;
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

  sessions[number].numfileinfos=0;
  dir_SetCurrent(sessions[number].current_dir);
  HS_GetFilesList(number,"*");
  wid_text.mask=STText;
  wid_text.text="*";
  wid_SetTextDecoration(tk_display,sessions[number].ed_open,&wid_text,False);
  HS_FreeSessionData(number);
  win_SetTitleName(tk_display,sessions[number].win_main,"Help");
  win_MapRaised(tk_display,sessions[number].win_open);
  XSync(tk_display->display,False);
  XFlush(tk_display->display);
  wid_GiveFocus(tk_display,sessions[number].ls_open_file);
  XSync(tk_display->display,False);
  XFlush(tk_display->display);
  wid_GiveFocus(tk_display,sessions[number].ls_open_file);
  return 0;
}



int HS_UnmapOpen(number)
unsigned int number;
{
  if(number>=maxsessions)
    return -1;

  win_Unmap(tk_display,sessions[number].win_open);
  wid_SetFreeze(tk_display,sessions[number].win_main,False);
  wid_SetFreeze(tk_display,sessions[number].win_topics,False);
  wid_SetFreeze(tk_display,sessions[number].win_glossary,False);
  item_DeleteAll(tk_display,sessions[number].ls_open_file);
  item_DeleteAll(tk_display,sessions[number].ls_open_dir);

  if(sessions[number].numfileinfos>0)
  {
	file_FreeInfos(sessions[number].fileinfos,sessions[number].numfileinfos);
	sessions[number].numfileinfos=0;	
  } 
  
  return 0;
}



#endif



