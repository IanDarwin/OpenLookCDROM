/*
 *
 * 	tk_misc.h
 * 	infos diverses
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




#ifndef TK_MISC_H
#define TK_MISC_H



		/********* Etats communs a plusieurs widgets *********/


#define Ungrayed	0
#define Unchecked	0
#define Unpushed        0
#define Unselected	0
#define Unblocked	0
#define Pushed          1
#define Grayed          2
#define Blocked		4
#define Checked		8
#define Selected	16
#define Stopped		32
#define Frozen		64


		/**************  Flags communs a plusieurs widgets *************/

#define TextFlag	1
#define PixmapFlag	2
#define PixmapMaskFlag	4
#define FontFlag	8
#define ParencyFlag	16
#define PixmapGrayedFlag 32
#define PixmapMaskGrayedFlag 64
#define NoBorderFlag	128



		/******** Donnees booleennes diverses  *********/


#define HORIZONTAL  	0
#define VERTICAL    	1

#define YES		1
#define NO		0

#define ON          	1
#define OFF         	0

#define INSERT  	1
#define DESTROY 	0

#define START		0
#define END		-1

#define UP          0
#define DOWN        1
#define INCRUST     2
#define LEFT	    3
#define RIGHT	    4
#define UPSIMPLE    5
#define DOWNSIMPLE  6



#endif 			/* TK_MISC_H */


