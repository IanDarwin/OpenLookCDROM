/*
 *
 * 	sb_struct.h
 * 	structure des ascenseurs
 *
 * 	Modification :  04/12/93
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
 *      IMAN Development Toolkit version 1.1.a
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */




#ifndef SB_STRUCT_H
#define SB_STRUCT_H


		/************** SCROLLBAR ***************/

typedef struct {

	Window mainwindow, thumbwindow;
	Window parent;
	Window top_level;

	ButtonID bn_thumb,B1,B2;

	int flags;
	int type, state;
	int wid_number;

	int x, y;
	int width, height;

	int position, oldposition;
	int range, pagerange;
	int thumbsize;

	int repeat, gap;

	Bool hasFocus;
	Bool neverFocus;
	Bool continuity;

	int parency_class;	
	int parency_number;

	short debug;

	}ScrollbarStruct;


typedef int ScrollbarID;



		/******* Types de SCROLLBAR *******/			


#define SB_LEFTALIGN		20
#define SB_RIGHTALIGN		21
#define SB_TOPALIGN		22
#define SB_BOTTOMALIGN		23
#define SB_HTHUMB		24
#define SB_VTHUMB		25

#define SB_MAINWINDOW		26
#define SB_THUMBWINDOW		27



#endif 		/* SB_STRUCT_H */


