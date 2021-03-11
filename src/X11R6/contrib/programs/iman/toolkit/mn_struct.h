/*
 *
 * 	mn_struct.h
 * 	structure des menus
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



#ifndef MN_STRUCT_H
#define MN_STRUCT_H


typedef int MenuID;

typedef struct {

	int flags;
	int precedency;
	int type;
	int state;

	int number;
	MenuID submenu;

	int x, y, width, height;

	char *text;
	int key;
	XFontStruct *font;
	
	Pixmap pixmap, pix_grayed, pix_mask, pix_maskgrayed;
	int pix_width, pix_height, pix_depth;

	Bool selected;
	
	} MenuItem;



typedef struct {

	Window window;
	Window parent, top_level;
	

	int flags;
	int type;
	int state;
	int attribut;
	int wid_number;

	int x, y, width, height;

	int numitems, maxitems;
	int maxwidth, maxheight;

	int selecteditem, oldselecteditem;
	Bool selected;

	char *title;
	XFontStruct *font;
	MenuItem *items;
	
	int repeat, continuity;
	Bool hasFocus;

	int parency_class;
	int parency_number;

	MenuID prevmenu;

	short debug;

	} MenuStruct;







#define MN_MENUBAR  	60 
#define MN_FLOATING  	61


#define MN_LMARGE	20
#define MN_UMARGE	3
#define MN_INTERMARGE 	7


#define MN_ITEM		0	/* Default */
#define MN_SUBMENU	1
#define MN_HBAR		2
#define MN_VBAR		3


#define CloseWhenNoFocus 	0
#define RemainMapped		1




#endif

