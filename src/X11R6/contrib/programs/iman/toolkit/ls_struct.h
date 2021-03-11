/*
 *
 * 	ls_struct.h
 * 	structure des listes
 *
 * 	Modification :  11/11/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
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
 *      IMAN Development Toolkit version 1.0.d
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */



#ifndef LS_STRUCT_H
#define LS_STRUCT_H

typedef struct {

	int flags;
	int precedency;
	int state;

	int x, y, width, height;

	char *text;
	XFontStruct *font;
	
	Pixmap pixmap, pix_grayed, pix_mask, pix_maskgrayed;
	int pix_width, pix_height, pix_depth;

	Bool selected;
	
	} ListItem;



typedef struct {
	Window mainwindow, listwindow;
	Window parent, top_level;

	ScrollbarID SBH, SBV;

	int flags;
	int type;
	int state;
	int attribut;
	int wid_number;

	int x, y, width, height;

	int numitems, maxitems;
	int maxwidth, maxheight, itemheight;
	int selecteditem;
	Bool selected;


	int topitem, downitem;
	int hpos;
	XFontStruct *font;
	ListItem *items;
	
	int repeat, continuity;
	Bool hasFocus;
	Bool multipleSelection;

	int parency_class;
 	int parency_number;

	short debug;

	} ListStruct;


typedef int ListID;



#define LS_SIMPLE   		40 
#define LS_HSCROLL  		41
#define LS_LEFTVSCROLL  	42
#define LS_RIGHTVSCROLL		43
#define LS_HLEFTVSCROLL 	44
#define LS_HRIGHTVSCROLL 	45

#define LS_MAINWINDOW		46
#define LS_LISTWINDOW		47


#define LS_LMARGE	3
#define LS_UMARGE	1
#define LS_INTERMARGE 	7



#endif

