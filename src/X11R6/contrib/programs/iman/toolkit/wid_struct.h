/*
 *
 * 	wid_struct.h
 * 	structure des widgets
 *
 * 	Modification :  25/11/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
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
 *      IMAN Development Toolkit version 1.2
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */




#ifndef WID_STRUCT_H
#define WID_STRUCT_H



typedef struct {
		unsigned long bg;
		unsigned long fg;
		unsigned long light;
		unsigned long shadow;

		unsigned long text;
		unsigned long text_grayed;		

		unsigned long selected;
		unsigned long selected_inactive;
		unsigned long text_selected;
		unsigned long text_grayed_selected;
		unsigned long text_selected_inactive;
		unsigned long text_grayed_selected_inactive;

		unsigned long cursor;
		unsigned long focus;
		unsigned long nofocus;

		unsigned long cross, check, radio_bg, radio_light;
		

		}WidgetColors;


typedef struct {
		Bool usePrivateColors;
		Bool hasBorder;
	
		int gravity;
		Cursor cursor;
		Colormap colormap;
		Visual *visual;

		}WidgetIdentity;



typedef struct {
		unsigned int number;
		Bool isUsed;
		int class;
		int type;

		WidgetIdentity 	identity;
		WidgetColors 	*colors;

		ButtonStruct *button;
		ScrollbarStruct *scroll;
		EditStruct  *edit;
		ListStruct  *list;
		ComboStruct *combo;
		MenuStruct  *menu;
		short debug;

		}WidgetStruct;




typedef struct {
		unsigned long mask;

		int lighting;
		int direction;

		unsigned int range;
		unsigned int pagerange;
		unsigned int thumbsize;
		
		Bool neverFocus;
		Bool multipleSelection;
		Bool border;

		int htype, vtype;
		unsigned int itemheight;
		unsigned int position;
		unsigned int crosstype;

		Colormap colormap;
		Visual *visual;
		Cursor cursor;

	       }WidgetAttributes;





#define SALighting		1
#define SADirection		2
#define SARange			4
#define SAPagerange		8
#define SAThumbsize		16
#define SANeverFocus		32
#define SAMultipleSelection	64
#define SABorder		128
#define SAHVType		256
#define SAItemHeight		512
#define SAPosition		1024
#define SACrossType		2048
#define SAColormap		4096
#define SAVisual		8192
#define SACursor		16384



typedef struct {
		unsigned long mask;

		char *text;
		XFontStruct *font;
		unsigned int key;
		unsigned int gravity;
		int x, y;

		} WidgetTextDecoration;


typedef struct {
		unsigned long mask;

		char *text;
		XFontStruct *font;
		unsigned int key;

		} ItemTextDecoration;


#define STText		1
#define STFont		2
#define STKey		4
#define STGravity	8
#define STX		16
#define STY		32



typedef struct {
		unsigned long mask;

		Pixmap pixmap, pixmap_mask;
		unsigned int depth;
		unsigned int gravity;

		int x, y;
		unsigned int width, height;

		} WidgetPixmapDecoration;


typedef struct {
		unsigned long mask;

		Pixmap pixmap, pixmap_mask;
		unsigned int depth;
		unsigned int width, height;

		} ItemPixmapDecoration;


#define SPPixmap		1
#define SPPixmapMask		2
#define SPDepth			4
#define SPGravity		8
#define SPX			16
#define SPY			32
#define SPWidth			64
#define SPHeight		128



typedef struct {
		unsigned long mask;

		int x, y;
		unsigned int width, height;

	       }WidgetConfigure;



#define CFX		1
#define CFY		2
#define CFWidth		4
#define CFHeight	8



typedef struct{

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

	}WidgetItem;



		

#endif 		/* WID_STRUCT_H */



