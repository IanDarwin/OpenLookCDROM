/*
 *
 * 	ed_struct.h  
 * 	structure des zones d'edition
 *
 * 	Modification :  11/11/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
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




#ifndef ED_STRUCT_H
#define ED_STRUCT_H


typedef struct {

	Window window, parent, top_level;

	int flags;
	int type;
	int state;
	int wid_number;

	unsigned char text[152];
	XFontStruct *font;

	int x, y;
	int width, height;

	int txt_x, txt_y;
	int txt_width, txt_height;

	short charwidth;
	short charheight;

	unsigned int length, maxlength;
	unsigned int position;

	int mode;
	int sel1, sel2;


	Bool hasFocus;
	Bool neverFocus;
	Bool continuity;

	int parency_class;
        int parency_number;

	short repeat;
	short debug;

	} EditStruct;



typedef int EditID;


#define ED_NORMALEDIT   	30
#define ED_SECRETEDIT		31
#define ED_FULLSELECT		32


#endif

