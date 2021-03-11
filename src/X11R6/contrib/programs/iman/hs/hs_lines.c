/*
 *
 * 	hs_lines.c
 * 	Lines of help functions
 *
 * 	Modification :  27/04/94
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


#ifndef _IMAN_HS_LINES_C
#define _IMAN_HS_LINES_C



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



int HS_GetUnusedLine(number)
unsigned int number;
{
  int i, j;
  int ret;

  if(number<maxsessions)
  {
    if(sessions[number].numlines>=sessions[number].maxlines)
    {
	if(sessions[number].maxlines<=0)
	  sessions[number].lines=(Lines *)malloc(sizeof(Lines)*(10));
	else
	  sessions[number].lines=(Lines *)realloc(sessions[number].lines,sizeof(Lines)*(sessions[number].maxlines+10));	
	if(sessions[number].lines==(Lines *)NULL)
	{
	   HS_UnuseLine(number);
	   return -10;
	}
	sessions[number].maxlines=sessions[number].maxlines+10;
	for(i=1;i<=10;i++)
	  HS_InitLine(number,sessions[number].maxlines-i);
	return sessions[number].numlines;
    }
    for(i=0;i<sessions[number].maxlines;i++)
	if(sessions[number].lines[i].isUsed==False)
	  return i;
    return -1;
  }
  return -1;
}




int HS_InitLine(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(number< maxsessions)
  {
    if(index<sessions[number].maxlines)
    {
	sessions[number].lines[index].isUsed=False;
	sessions[number].lines[index].width=0;
	sessions[number].lines[index].height=0;
 	sessions[number].lines[index].start_item=-1;	
	sessions[number].lines[index].end_item=-1;
	sessions[number].lines[index].start_char=-1;	
	sessions[number].lines[index].end_char=-1;
	sessions[number].lines[index].descent=-1;

    }
    return -1;
  }
  return -1;
}




int HS_UseLine(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(number< maxsessions)
  {
    if(index<sessions[number].maxlines && sessions[number].lines[index].isUsed==False)
    {	
	HS_InitLine(number,index);
	sessions[number].lines[index].isUsed=True;
  	sessions[number].numlines++;
    }
    return -1;
  }
  return -1;
}




int HS_UnuseLine(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;

  if(index<sessions[number].maxlines && sessions[number].lines[index].isUsed==True)
  {
    sessions[number].lines[index].isUsed=False;
    sessions[number].lines[index].width=0;	
    sessions[number].lines[index].height=0;
    sessions[number].lines[index].start_item=-1;	
    sessions[number].lines[index].end_item=-1;
    sessions[number].lines[index].start_char=-1;	
    sessions[number].lines[index].end_char=-1;
    sessions[number].lines[index].descent=-1;

    return 0;
  }
  return -1;
}




int HS_FreeLines(number)
unsigned int number;
{
  int i, j;
  int ret;


  if(number>=maxsessions)
    return -1;

  if(sessions[number].maxlines>0 && sessions[number].lines!=(Lines *)NULL)
    free((char *)sessions[number].lines);
  sessions[number].lines=(Lines *)NULL;
  sessions[number].numlines=0;
  sessions[number].maxlines=0;
  /*sessions[number].scroll_lines=0;*/

  return 0;
}




int HS_GetNumWords(text)
char *text;
{
  int i, j;
  int ret;


  if(text==(char *)NULL)
    return -1;

  i=0;
  j=1;
  ret=strlen(text);
  if(ret==0)
    return 0;

  while(text[i]==' ')
    i++;

  for(i=i;i<ret;i++)
  {
	if(text[i]==' ' && text[i+1]!=' ' && text[i+1]!=0)
	  j++;
  }
  return j;
}



HSWord *HS_GetWords(text,numwords)
char *text;
unsigned int numwords;
{
  int i, j;
  int current;
  int ret;
  HSWord *words;
  
  if(text==(char *)NULL)
    return (HSWord *)NULL;

  i=j=0;
  ret=strlen(text);
  if(ret==0)
    return (HSWord *)NULL;
  if(numwords==0)
    return (HSWord *)NULL;
  current=0;

  words=(HSWord *)malloc(sizeof(HSWord)*numwords);
  if(words==(HSWord *)NULL) return (HSWord *)NULL;
  words[0].w=text;
  i=j=0;

  while(text[i]==' ')
  {
    i++;
    j++; 
  }

  for(i=i;i<ret;i++)
  {
	j++;
	if(text[i]==' ' && text[i+1]!=' ' && text[i+1]!=0)
	{
	  words[current].length=j;
	  current++;
	  words[current].w=text+i;
	  j=0;
	}
  }
  words[current].length=j;
  return (HSWord *)words;
}




int HS_GetLinesDimensions(number)
unsigned int number;
{
  int i, j, k,m,w;
  int ret;
  int index;
  unsigned int width=0, height=0;
  int line,length, prev=0;
  int spacing, justify, margin, jumper, topic_number;
  unsigned long color;
  XFontStruct *font;
  WidgetAttributes wid_attributes;
  int numwords;
  HSWord *words;
  int inc, current_item, current_line, position,newline,newposition,start_char;




  if(number>=maxsessions)
    return -1;
  if(sessions[number].numtopics<=0)
    return -2;
  start_char=0;
  if(sessions[number].numlines>0)
    start_char=sessions[number].lines[sessions[number].current_line].start_char;
  if(start_char<0)
    start_char=0;

  /*fprintf(stderr,"Before :  current_line=%d  inc=%d  \n",sessions[number].current_line,sessions[number].inc);*/
  if(sessions[number].numlines>0)
    HS_FreeLines(number);
  index=sessions[number].current_topic;
  line=HS_GetUnusedLine(number);
  HS_UseLine(number,line);
  sessions[number].lines[line].start_item=0;
  sessions[number].lines[line].end_item=0;
  sessions[number].lines[line].start_char=-1;
  sessions[number].lines[line].end_char=-1;
  sessions[number].lines[line].font=font=tk_display->fonts.helvetica12;
  sessions[number].lines[line].color=color=BlackPixel(tk_display->display,tk_display->screen);
  sessions[number].lines[line].spacing=spacing=0;
  sessions[number].lines[line].justify=justify=LeftJustify;
  sessions[number].lines[line].margin=margin=0;
  sessions[number].lines[line].jumper=jumper=0;
  sessions[number].lines[line].descent=-1;
  sessions[number].lines[line].width=0;
  sessions[number].lines[line].height=0;
  /*fprintf(stderr,"Before :  current_line=%d  inc=%d  \n",sessions[number].current_line,sessions[number].inc);*/
  topic_number=-1;

  for(j=0;j<sessions[number].topics[index].maxitems;j++)
    if(sessions[number].topics[index].items[j].isUsed==True)
    {
	if(sessions[number].topics[index].items[j].action==DrawString)
	  sessions[number].topics[index].items[j].width=XTextWidth(font, sessions[number].topics[index].items[j].text,strlen(sessions[number].topics[index].items[j].text));


	if(width+sessions[number].topics[index].items[j].width<=sessions[number].width-2*LMARGE && sessions[number].topics[index].items[j].action!=NewLine)
        {
		/*fprintf(stderr,"CAS 1 : %s \n",sessions[number].topics[index].items[j].text);*/
	  	sessions[number].lines[line].end_item=j;
		if(sessions[number].topics[index].items[j].action==DrawPixmap)
		{
		  /*ret=HS_GetResourceIndexFromNumber(number,sessions[number].topics[index].items[j].res_number);
		  if(ret<0) fprintf(stderr,"Resource index error ...  %d ",sessions[number].topics[index].items[j].res_number);*/
		  ret=sessions[number].topics[index].items[j].res_number;
		  sessions[number].topics[index].items[j].width=sessions[number].resources[ret].width;
		  sessions[number].topics[index].items[j].height=sessions[number].resources[ret].height;
		  
		}
		else if(sessions[number].topics[index].items[j].action==DrawBitmap)
		{
		  /*ret=HS_GetResourceIndexFromNumber(number,sessions[number].topics[index].items[j].res_number);
		  if(ret<0) fprintf(stderr,"Resource index error ...  ");*/
		  ret=sessions[number].topics[index].items[j].res_number;
		  sessions[number].topics[index].items[j].width=sessions[number].resources[ret].width;
		  sessions[number].topics[index].items[j].height=sessions[number].resources[ret].height;
		}
		else if(sessions[number].topics[index].items[j].action==DrawString)
		{
		  /*ret=XTextWidth(font, sessions[number].topics[index].items[j].text,strlen(sessions[number].topics[index].items[j].text));
		  sessions[number].topics[index].items[j].width=ret;*/
		  sessions[number].topics[index].items[j].height=font->ascent+font->descent;
		  if(font->descent>sessions[number].lines[line].descent)
			sessions[number].lines[line].descent=font->descent;
		}


		width=width+sessions[number].topics[index].items[j].width;
          	if(sessions[number].lines[line].height<sessions[number].topics[index].items[j].height)
	  	  sessions[number].lines[line].height=sessions[number].topics[index].items[j].height;
		sessions[number].lines[line].width=width;

		if(sessions[number].topics[index].items[j].action==SetColor)
		{
		  /*ret=HS_GetResourceIndexFromNumber(number,sessions[number].topics[index].items[j].res_number);*/
		  ret=sessions[number].topics[index].items[j].res_number;
		  if(ret>=0) color=sessions[number].resources[ret].color;
		}
		else if(sessions[number].topics[index].items[j].action==SetFont)
		{
		  /*ret=HS_GetResourceIndexFromNumber(number,sessions[number].topics[index].items[j].res_number);*/
		  ret=sessions[number].topics[index].items[j].res_number;
		  if(ret>=0) font=(XFontStruct *)sessions[number].resources[ret].font;
		}
		else if(sessions[number].topics[index].items[j].action==SetSpacing)
		  spacing=sessions[number].topics[index].items[j].num;
		else if(sessions[number].topics[index].items[j].action==SetLeftMargin)
		{
		  margin=sessions[number].topics[index].items[j].num;
		  if(margin>=sessions[number].width-80)
			margin=sessions[number].width-80;
		  if(margin<0)
			margin=0;
		}
		else if(sessions[number].topics[index].items[j].action==SetJustify)
		  justify=sessions[number].topics[index].items[j].num;
		else if(sessions[number].topics[index].items[j].action==StartTopicJumper && jumper==0)
		{
		  topic_number=sessions[number].topics[index].items[j].topic_number;
		  jumper=StartTopicJumper;
		}
		else if(sessions[number].topics[index].items[j].action==EndTopicJumper && jumper==StartTopicJumper)
		{
		  topic_number=-1;
		  jumper=0;
		}
		else if(sessions[number].topics[index].items[j].action==StartGlossaryJumper && jumper==0)
		  jumper=StartGlossaryJumper;
		else if(sessions[number].topics[index].items[j].action==EndGlossaryJumper && jumper==StartGlossaryJumper)
		  jumper=0;
		else if(sessions[number].topics[index].items[j].action==DefaultColor)
		  color=BlackPixel(tk_display->display,tk_display->screen);
		else if(sessions[number].topics[index].items[j].action==DefaultFont)
		  font=tk_display->fonts.helvetica12;
		sessions[number].topics[index].items[j].topic_number=topic_number;

	}	
	else if(width+sessions[number].topics[index].items[j].width>sessions[number].width-2*LMARGE && sessions[number].topics[index].items[j].action==DrawString)
	{
		/*fprintf(stderr,"CAS 2 : %s \n",sessions[number].topics[index].items[j].text);*/

		sessions[number].topics[index].items[j].height=font->ascent+font->descent;
        			
		numwords=HS_GetNumWords(sessions[number].topics[index].items[j].text);
		words=HS_GetWords(sessions[number].topics[index].items[j].text,numwords);
		length=0;

		if(numwords>0 && words!=(HSWord *)NULL)
		for(w=0;w<numwords;w++)
		{
		  length=length+words[w].length;
		  if((ret=XTextWidth(font,words[w].w,words[w].length))<=sessions[number].width-2*LMARGE) 
		  {
			if(ret+width<=sessions[number].width-2*LMARGE)
			{
			  sessions[number].lines[line].end_char=length;
  			  sessions[number].lines[line].end_item=j;
			  width=width+ret;
			  sessions[number].lines[line].width=width;
			  if(sessions[number].lines[line].descent<font->descent)
				sessions[number].lines[line].descent=font->descent;
			  if(sessions[number].lines[line].height<sessions[number].topics[index].items[j].height)
	  	  		sessions[number].lines[line].height=sessions[number].topics[index].items[j].height;


			}
		  	else 
		  	{
			  sessions[number].lines[line].end_item=j;
			  sessions[number].lines[line].end_char=length-words[w].length;
			  sessions[number].lines[line].width=width;

			  if(justify==LeftJustify)
			    width=ret+margin;
			  else width=ret;
			  line=HS_GetUnusedLine(number);
			  HS_UseLine(number,line);
			  sessions[number].lines[line].start_item=j;
  			  sessions[number].lines[line].end_item=j;
			  sessions[number].lines[line].width=width;
	  		  sessions[number].lines[line].height=sessions[number].topics[index].items[j].height;
 		 	  sessions[number].lines[line].font=font;
  			  sessions[number].lines[line].color=color;
  			  sessions[number].lines[line].spacing=spacing;
  			  sessions[number].lines[line].justify=justify;
  			  sessions[number].lines[line].margin=margin;
  			  sessions[number].lines[line].jumper=jumper;
			  sessions[number].lines[line].start_char=length-words[w].length;
			  sessions[number].lines[line].end_char=-1;
			  if(sessions[number].lines[line].descent<font->descent)
				sessions[number].lines[line].descent=font->descent;
			  if(sessions[number].lines[line].height<sessions[number].topics[index].items[j].height)
	  		  	sessions[number].lines[line].height=sessions[number].topics[index].items[j].height;
			}

		  }
		  else 
		  {
			length=length-words[w].length;
			m=0;
			prev=0;
			while(words[w].w[m]!=0 && m<words[w].length)
			{
			  if((ret=XTextWidth(font,words[w].w+prev,m+1-prev))+width>sessions[number].width-2*LMARGE)
			  {
			    sessions[number].lines[line].end_item=j;
			    sessions[number].lines[line].end_char=length+m;
			    width=width+XTextWidth(font,words[w].w+prev,m-prev);
			    sessions[number].lines[line].width=width;
			    if(justify==LeftJustify)
			      width=margin;
			    else width=0;


 			    line=HS_GetUnusedLine(number);
			    HS_UseLine(number,line);
			    prev=m;
			    sessions[number].lines[line].start_item=j;
  			    sessions[number].lines[line].end_item=j;
			    sessions[number].lines[line].width=width;
	  		    sessions[number].lines[line].height=sessions[number].topics[index].items[j].height;
 		 	    sessions[number].lines[line].font=font;
  			    sessions[number].lines[line].color=color;
  			    sessions[number].lines[line].spacing=spacing;
  			    sessions[number].lines[line].justify=justify;
  			    sessions[number].lines[line].margin=margin;
  			    sessions[number].lines[line].jumper=jumper;
			    sessions[number].lines[line].start_char=length+m;
			    sessions[number].lines[line].end_char=-1;
			    sessions[number].lines[line].descent=font->descent;
		  
			  }
			  else
			  {
				if(sessions[number].lines[line].descent<font->descent)
				  sessions[number].lines[line].descent=font->descent;
			  	if(sessions[number].lines[line].height<sessions[number].topics[index].items[j].height)
	  		  	  sessions[number].lines[line].height=sessions[number].topics[index].items[j].height;			
			  }
			  m++;
			}
			sessions[number].lines[line].end_item=j;
			sessions[number].lines[line].end_char=length+m;
			width=width+XTextWidth(font,words[w].w+prev,m-prev);
			sessions[number].lines[line].width=width;
			length=length+words[w].length;
			if(sessions[number].lines[line].descent<font->descent)
			  sessions[number].lines[line].descent=font->descent;
			if(sessions[number].lines[line].height<sessions[number].topics[index].items[j].height)
	  		  sessions[number].lines[line].height=sessions[number].topics[index].items[j].height;	
		  }

		}
		if(numwords>0 && words!=(HSWord *)NULL)
		  free((char *)words);
		sessions[number].topics[index].items[j].topic_number=topic_number;

	}
	else 
	{
		/*fprintf(stderr,"CAS 3 : %s \n",sessions[number].topics[index].items[j].text);*/
		sessions[number].lines[line].width=width;
		if(justify==LeftJustify)
		  width=margin;
		else width=0;
		line=HS_GetUnusedLine(number);
		HS_UseLine(number,line);
		sessions[number].lines[line].start_item=j;
  		sessions[number].lines[line].end_item=j;
		sessions[number].lines[line].width=width;
	  	sessions[number].lines[line].height=sessions[number].topics[index].items[j].height;
 		sessions[number].lines[line].font=font;
  		sessions[number].lines[line].color=color;
  		sessions[number].lines[line].spacing=spacing;
  		sessions[number].lines[line].justify=justify;
  		sessions[number].lines[line].margin=margin;
  		sessions[number].lines[line].jumper=jumper;

		if(sessions[number].topics[index].items[j].action==DrawPixmap)
		{
		  /*ret=HS_GetResourceIndexFromNumber(number,sessions[number].topics[index].items[j].res_number);
		  if(ret<0) fprintf(stderr,"Resource index error ...  %d",sessions[number].topics[index].items[j].res_number);*/
		  ret=sessions[number].topics[index].items[j].res_number;
		  sessions[number].topics[index].items[j].width=sessions[number].resources[ret].width;
		  sessions[number].topics[index].items[j].height=sessions[number].resources[ret].height;
		}
		else if(sessions[number].topics[index].items[j].action==DrawBitmap)
		{
		  /*ret=HS_GetResourceIndexFromNumber(number,sessions[number].topics[index].items[j].res_number);
		  if(ret<0) fprintf(stderr,"Resource index error ...  ");*/
		  ret=sessions[number].topics[index].items[j].res_number;
		  sessions[number].topics[index].items[j].width=sessions[number].resources[ret].width;
		  sessions[number].topics[index].items[j].height=sessions[number].resources[ret].height;
		}
		else if(sessions[number].topics[index].items[j].action==DrawString)
		{
		  /*ret=XTextWidth(font, sessions[number].topics[index].items[j].text,strlen(sessions[number].topics[index].items[j].text));
		  sessions[number].topics[index].items[j].width=ret;*/
		  sessions[number].topics[index].items[j].height=font->ascent+font->descent;
		  if(sessions[number].lines[line].descent<font->descent)
			sessions[number].lines[line].descent=font->descent;
		}

		width=width+sessions[number].topics[index].items[j].width;
          	if(sessions[number].lines[line].height<sessions[number].topics[index].items[j].height)
	  	  sessions[number].lines[line].height=sessions[number].topics[index].items[j].height;


		if(sessions[number].topics[index].items[j].action==SetColor)
		{
		  /*ret=HS_GetResourceIndexFromNumber(number,sessions[number].topics[index].items[j].res_number);*/
		  ret=sessions[number].topics[index].items[j].res_number;
		  if(ret>=0) color=sessions[number].resources[ret].color;
		}
		else if(sessions[number].topics[index].items[j].action==SetFont)
		{
		  /*ret=HS_GetResourceIndexFromNumber(number,sessions[number].topics[index].items[j].res_number);*/
		  ret=sessions[number].topics[index].items[j].res_number;
		  if(ret>=0) font=(XFontStruct *)sessions[number].resources[ret].font;
		}
		else if(sessions[number].topics[index].items[j].action==SetSpacing)
		  spacing=sessions[number].topics[index].items[j].num;
		else if(sessions[number].topics[index].items[j].action==SetLeftMargin)
		{
		  margin=sessions[number].topics[index].items[j].num;
		  if(margin>=sessions[number].width-80)
			margin=sessions[number].width-80;
		  if(margin<0)
		    margin=0;
		}
		else if(sessions[number].topics[index].items[j].action==SetJustify)
		  justify=sessions[number].topics[index].items[j].num;
		else if(sessions[number].topics[index].items[j].action==StartTopicJumper && jumper==0)
		{
		  topic_number=sessions[number].topics[index].items[j].topic_number;
		  jumper=StartTopicJumper;
		}
		else if(sessions[number].topics[index].items[j].action==EndTopicJumper && jumper==StartTopicJumper)
		{
		  topic_number=-1;
		  jumper=0;
		}
		else if(sessions[number].topics[index].items[j].action==StartGlossaryJumper && jumper==0)
		  jumper=StartGlossaryJumper;
		else if(sessions[number].topics[index].items[j].action==EndGlossaryJumper && jumper==StartGlossaryJumper)
		  jumper=0;
		else if(sessions[number].topics[index].items[j].action==DefaultColor)
		  color=BlackPixel(tk_display->display,tk_display->screen);
		else if(sessions[number].topics[index].items[j].action==DefaultFont)
		  font=tk_display->fonts.helvetica12;
		sessions[number].topics[index].items[j].topic_number=topic_number;

	}
/*	else if(width+sessions[number].topics[index].items[j].width<=sessions[number].width-2*LMARGE && sessions[number].topics[index].items[j].width>sessions[number].width-2*LMARGE)
	{
		width=sessions[number].topics[index].items[j].width ;
		line=HS_GetUnusedLine(number);
		HS_UseLine(number,line);
		sessions[number].lines[line].start_item=j;
  		sessions[number].lines[line].end_item=j;
		sessions[number].lines[line].width=width;
	  	sessions[number].lines[line].height=sessions[number].topics[index].items[j].height;
	}
*/

   }
  
  /*fprintf(stderr,"Before :  current_line=%d  inc=%d  \n",sessions[number].current_line,sessions[number].inc);*/

  if(sessions[number].current_line<0)
    sessions[number].current_line=0;
  if(sessions[number].current_item<0)
    sessions[number].current_item=0;

  inc=sessions[number].inc;
  current_line=sessions[number].current_line;
  current_item=sessions[number].current_item;
  position=wid_GetPosition(tk_display,sessions[number].sb_main);
  
  j=HS_GetTotalHeight(number)+20;
  i=(j-sessions[number].height+4*UMARGE);
  if(i<0)
   i=0;
  i=i/20;
  if(i<0)
    i=0;
  sessions[number].scroll_lines=i;


  wid_attributes.range=sessions[number].scroll_lines;
  i=sessions[number].height-2*UMARGE;
 
  i=i/20;
  wid_attributes.pagerange=i;
  wid_attributes.mask=SARange+SAPagerange+SAPosition;

  newline=HS_GetItemLine(number,current_item,start_char);
  height=HS_GetHeightToLine(number,newline);  
  /*fprintf(stderr,"j=%d height=%d\n",j,height);*/

  if(j-height>=sessions[number].height)
  {
    /*fprintf(stderr,"Case 1 \n");*/
    sessions[number].current_line=newline;
    sessions[number].current_item=current_item;
    i=0;
    while(i<height)
    	i=i+20;
    position=i/20;
    /*fprintf(stderr,"i=%d position=%d\n",i,position);*/
    sessions[number].inc=(position*20)-height;    
  }
  else
  {
    /*fprintf(stderr,"Case 2 \n");*/
    height=j-sessions[number].height;
    if(height<0) height=0;
    newline=sessions[number].current_line=HS_GetLineFromHeight(number,height);
    if(newline<0)
      newline=sessions[number].current_line=0;
    height=HS_GetHeightToLine(number,newline);  
    sessions[number].current_item=current_item;
    i=0;
    while(i<height)
    	i=i+20;
    position=i/20;
    sessions[number].inc=(position*20)-height;    
  }
  

  wid_attributes.position=sessions[number].position=position;
  wid_SetAttributes(tk_display,sessions[number].sb_main,&wid_attributes,False);
  if(sessions[number].scroll_lines<=0)
    wid_SetState(tk_display,sessions[number].sb_main,Grayed);
  else wid_SetState(tk_display,sessions[number].sb_main,Ungrayed);

  /*fprintf(stderr,"*** Topic %d  numitems=%d  maxitems=%d  name=%s\n",index,sessions[number].topics[index].numitems,sessions[number].topics[index].maxitems,sessions[number].topics[index].name);*/
 

  return 0;
}



int HS_GetTotalHeight(number)
unsigned int number;
{
  int i, j;
  int ret;
  unsigned int height=0;
  unsigned int spacing=0;

  height=0;

  for(i=0;i<sessions[number].numlines;i++)
  {
	if(sessions[number].lines[i].height>0)
	  height=height+sessions[number].lines[i].spacing+sessions[number].lines[i].height;	
  }
  return height;
}





int HS_GetItemLine(number,index,start_char)
unsigned int number,index;
int start_char;
{
  int i, j;


  for(i=sessions[number].numlines-1;i>=0;i--)
  {
	if(index>=sessions[number].lines[i].start_item && index<=sessions[number].lines[i].end_item)	
	{
	  if(index==sessions[number].lines[i].start_item && sessions[number].lines[i].start_char>=0)
	 	if(start_char>=sessions[number].lines[i].start_char) return i;
	  	else goto CONT;
	  else if(index==sessions[number].lines[i].start_item && sessions[number].lines[i].start_char<0)
	    return i;
	  else if(index==sessions[number].lines[i].end_item && sessions[number].lines[i].end_char>=0)
	  {
		if(start_char<=sessions[number].lines[i].end_char)
		{
		  if(index>sessions[number].lines[i].start_item) 
		    return i;
		  else if(index==sessions[number].lines[i].start_item && sessions[number].lines[i].start_char<0)
		    return i;
		  else if(index==sessions[number].lines[i].start_item && sessions[number].lines[i].start_char>=0 && start_char>=sessions[number].lines[i].start_char)
		    return i;
		}	
	  }
	  else if(index==sessions[number].lines[i].end_item && sessions[number].lines[i].end_char<0)
	    return i;
	  else if(sessions[number].lines[i].start_char<0 && sessions[number].lines[i].end_char<0)
	    return i;
	  else if(index>sessions[number].lines[i].start_item && index<sessions[number].lines[i].end_item)
	    return i;
	}
CONT:
	i=i;
  }
  return -1;
}




int HS_GetHeightToLine(number,index)
unsigned int number,index;
{
  int i, j;
  int ret;
  unsigned int height=0;
  int spacing=0;

  if(index>=sessions[number].numlines)
    return -1;
  height=0;
  if(index>0)
  for(i=0;i<index;i++)
  {
        if(sessions[number].lines[i].height>0) spacing=sessions[number].lines[i].spacing;
	else spacing=0;
	if(spacing<0) spacing=0;
	height=height+spacing+sessions[number].lines[i].height;
  }
  return height;
}




int HS_GetLineFromHeight(number,height)
unsigned int number;
int height;
{
  int i, j;
  int ret;
  int spacing;

  j=0;  
  spacing=0;
  for(i=0;i<sessions[number].numlines;i++)
  {
        if(sessions[number].lines[i].height>0) spacing=sessions[number].lines[i].spacing;
	else spacing=0;
	if(spacing<0) spacing=0;
	if(height>=j && height<j+sessions[number].lines[i].height+spacing)
	  return i;
	j=j+spacing+sessions[number].lines[i].height;
  }
  return -1;
}




int HS_GetTopicJumper(number,line,x)
unsigned int number,line;
int x;
{
  int i, j;
  int ret;
  int width;
  unsigned int jumper;
  unsigned int topic;
  int jump_number;

  width=0;
  jump_number=-1;
  jumper=sessions[number].lines[line].jumper;
  i=sessions[number].lines[line].start_item;
  topic=sessions[number].current_topic;
  /*fprintf(stderr,"start:%d  end:%d\n",i,sessions[number].lines[line].end_item);*/
  width=sessions[number].lines[line].margin;

  while(width<x && i<=sessions[number].lines[line].end_item)
  {
     j=0;
     /*fprintf(stderr,"item %d  action=%d  width=%d  ",i,sessions[number].topics[topic].items[i].action,width);*/
     if(sessions[number].topics[topic].items[i].action==DrawString)
     {
	if(sessions[number].lines[line].start_char>=0 && i==sessions[number].lines[line].start_item)
	{
	  if(sessions[number].lines[line].end_char>=0 && i==sessions[number].lines[line].end_item)
	    j=XTextWidth(sessions[number].lines[line].font,sessions[number].topics[topic].items[i].text+sessions[number].lines[line].start_char,sessions[number].lines[line].end_char-sessions[number].lines[line].start_char);
	  else j=XTextWidth(sessions[number].lines[line].font,sessions[number].topics[topic].items[i].text+sessions[number].lines[line].start_char,strlen(sessions[number].topics[topic].items[i].text)-sessions[number].lines[line].start_char);

	}
	else if(sessions[number].lines[line].end_char>=0 && i==sessions[number].lines[line].end_item)
	  j=XTextWidth(sessions[number].lines[line].font,sessions[number].topics[topic].items[i].text,sessions[number].lines[line].end_char);
	else j=sessions[number].topics[topic].items[i].width;

      }
      else if(sessions[number].topics[topic].items[i].action==StartTopicJumper)
      {
	jump_number=sessions[number].topics[topic].items[i].topic_number;
	jumper=StartTopicJumper;
      }
      else if(sessions[number].topics[topic].items[i].action==EndTopicJumper)
      {
	jump_number=-1;
	jumper=0;
      }
      else j=sessions[number].topics[topic].items[i].width;
      /*fprintf(stderr,"j:%d\n",j);*/

      if(j<0) j=0;
      if(x>=width && x<width+j)
      {
	if(jumper==StartTopicJumper)
	  return sessions[number].topics[topic].items[i].topic_number;
      }
      width=width+j;
      i++;
  }
  return -1;
}





#endif

