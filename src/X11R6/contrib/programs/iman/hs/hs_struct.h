/*
 *
 * 	hs_struct.h
 * 	Help structures
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




#ifndef _IMAN_HS_STRUCT_H
#define _IMAN_HS_STRUCT_H


#define PIXMAPCLASS	1
#define BITMAPCLASS	2
#define FONTCLASS	3
#define COLORCLASS	4
#define ACTIONCLASS	5

#define LoadAtStart		1
#define LoadWhenFirstNeeded	2
#define LoadDynamic		3

#define Inside		1
#define Outside		2

#define MainTopic	1
#define SubTopic	2

#define LeftJustify	10
#define CenterJustify	20
#define RightJustify	30

#define PHF	1
#define BHF	2


#define UMARGE	5
#define LMARGE	5
#define INTERMARGE	3


typedef struct	{
		  unsigned short class, type;
		  int number;
		  unsigned short loading;
		  long seek;
		  Bool isUsed;
		  Bool isLoaded;

		  Pixmap pixmap;
		  Pixmap bitmap;
		  unsigned int width, height;
		  XFontStruct *font;
		  unsigned long color;
		  char *name;
		  
	      	}HelpResource;

#define Nothing 		0
#define DrawString		1
#define DrawPixmap		2
#define DrawBitmap		3
#define SetFont			4
#define SetColor		5
#define SetLeftMargin		6
#define SetSpacing		7
#define NewLine			8
#define StartTopicJumper	9
#define EndTopicJumper		10
#define SetJustify		11
#define DefaultColor		12
#define DefaultFont		13
#define StartGlossaryJumper	14
#define EndGlossaryJumper	15



typedef struct  {
		  unsigned int action;
		  Bool isUsed;

		  char *text;
		  int res_number;
 		  int topic_number;
		  int def_number;
		  int num;

		  unsigned int width, height;

		}HelpItem;


typedef struct  {
		  Bool isUsed;

		  int start_item;
 		  int end_item;
		  int start_char;
		  int end_char;
		  XFontStruct *font;
		  unsigned long color;
		  unsigned int spacing, margin, jumper, justify;

		  unsigned int width, height;
		  int descent;

		}Lines;


typedef struct 	{
	   	  char *w;
		  int length;
		}HSWord;



typedef struct 	{
		  unsigned short class;
		  int number;
		  unsigned int parent;
		  unsigned short loading;
		  int icon;

		  unsigned int statistic;
		  long seek;

		  Bool isUsed;
		  Bool isLoaded;
		  char *name;

		  unsigned int numitems, maxitems;
		  HelpItem *items;

		}HelpTopic;



typedef struct	{
		  Bool isUsed;
		  int action;
		  Window owner;

		  Window win_main, win_draw, win_topics, win_error, win_about;
		  Window win_open, win_warning, win_glossary, win_history, win_seek;		 

		  ButtonID bn_index, bn_previous, bn_next, bn_seek, bn_print, bn_history;
		  ButtonID bn_error_ok, bn_error_cancel;
		  ButtonID bn_warning_ok, bn_warning_cancel;
		  ButtonID bn_about_ok, bn_open_ok, bn_open_cancel;
		  ListID ls_topics, ls_open_file, ls_open_dir, ls_glossary,ls_history;
		  EditID ed_open;
		  MenuID mn_main, mn_file, mn_options, mn_help, mn_window;
		  ScrollbarID sb_main;
	
		  unsigned int width, height;
		  unsigned int numlines, maxlines;
		  Lines *lines;
		  int current_line, scroll_lines, inc;
		  int position;

		  int file;
		  FILE *stream;
		  int format;
		  char *filename;
		  char *dirname;
		  char *current_dir;
		  int numfileinfos;
		  FileInfo **fileinfos;

		  int version, release, update, index;
		  char *help_name;
		  char *copyright;
		  char *vendor;
		  unsigned int year;
		  unsigned short month, day;

		  unsigned int numresources;
		  unsigned int maxresources;
		  unsigned int loadedresources;
		  HelpResource *resources;

		  unsigned int numtopics;
		  unsigned int maxtopics;
		  unsigned int loadedtopics;
		  int current_topic;
		  int current_item;
		  HelpTopic *topics;
		  Bool showAllTopics;

		}HelpSession;






#endif

