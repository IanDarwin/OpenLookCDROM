/*
 *
 * 	bn_struct.h
 * 	structure des boutons
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



#ifndef BN_STRUCT_H
#define BN_STRUCT_H


		/************** BUTTON ***************/

typedef struct {

	Window window;
	Window parent;
	Window top_level;

	int flags;
	int type, state, crosstype;
	int wid_number;

	int x, y;
	int width, height;

	char *text;
	int txt_key;
	XFontStruct *txt_font;
	int txt_x, txt_y;
	short txt_gravity;

	Pixmap pixmap, pix_mask;
	int pix_x, pix_y;
	int pix_width, pix_height;
	int pix_depth;
	int pix_gravity;
	Pixmap  pixmap_grayed, pixgrayed_mask;
	int pixgrayed_x, pixgrayed_y;
	int pixgrayed_width, pixgrayed_height;
	int pixgrayed_depth;
	int pixgrayed_gravity;

	int  lighting;
	Bool hasFocus;
	Bool neverFocus;
	Bool continuity;

	int parency_class;
	int parency_number;
	
	short repeat;
	short debug;

	}ButtonStruct;


typedef int ButtonID;

		/******* Types de BOUTON *******/			


#define BN_PUSHBUTTON   	1
#define BN_CROSSBUTTON  	2
#define BN_CHECKBUTTON  	3
#define BN_RADIOBUTTON 	 	4
#define BN_REPEATBUTTON 	5
#define BN_PMBUTTON         	7
#define BN_SCROLLBUTTON     	8
#define BN_THUMBBUTTON      	9
#define BN_POPUPBUTTON		10
#define BN_POPUPRADIOBUTTON	11
#define BN_COMBOBUTTON		12


		/********* Bitmap Gravity *********/


#define NoBitmap          0

#define NorthWestBitmap	  1
#define NorthBitmap	  2
#define NorthEastBitmap	  3
#define WestBitmap	  4
#define CenterBitmap      5
#define EastBitmap        6
#define SouthWestBitmap   7
#define SouthBitmap       8
#define SouthEastBitmap   9
#define UserDefinedBitmap 10


		/*********** Text Gravity ************/

#define NoText		0
#define CenterText	5
#define UserDefinedText 10



		/*********** Cross Types *************/

#define NoCross 	0
#define BigCross	1
#define LittleCross	2
#define CheckMark	3




#define CHECKSIZE 	20
#define CROSSSIZE 18
#define RADIOSIZE 19


#endif 		/* BN_STRUCT_H */


