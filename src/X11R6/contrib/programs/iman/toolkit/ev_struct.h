/*
 *
 * 	ev_struct.h
 * 	structure des evenements
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




#ifndef EV_STRUCT_H
#define EV_STRUCT_H


			/******** Format des evenements ********/


typedef struct {
	int ev_type;
	unsigned long ev_widget;

	ButtonID button;
	ScrollbarID scroll;
	EditID   edit;
	ListID   list;
	ComboID  combo;
	MenuID   menu;

/*	ButtonStruct *button;
	ScrollbarStruct *scroll;
	EditStruct   *edit;
	ListStruct   *list;
	ComboStruct  *combo;
	MenuStruct   *menu;
*/	
	XEvent event;
	short debug;
	} TkEvent;

			/**** Classes de widgets reconnus ****/


#define	 WI_BUTTON	1
#define  WI_SCROLLBAR	2
#define  WI_EDIT	3
#define  WI_LIST	4
#define  WI_COMBO	5
#define  WI_MENU	6


			/******* Types d'evenements *******/

#define NO_EVENT    	 0
#define JUSTFOCUS	 1


#define BN_PUSHED  	 2
#define BN_RELEASED 	 3
#define BN_MOVED	 4
#define BN_PRESSED	 5
#define BN_UNPRESSED	 6
#define BN_KEYUNKNOWN	 31

#define SB_DOWN		 7
#define SB_UP		 8
#define SB_LEFT		 9
#define SB_RIGHT	 10
#define SB_PAGEDOWN	 11
#define SB_PAGEUP	 12
#define SB_PAGELEFT	 13
#define SB_PAGERIGHT	 14
#define SB_THUMBMOVED	 15
#define SB_STATUS	 16

#define ED_VALIDATION	 17
#define ED_KEYUNKNOWN	 18


#define LS_CLICKED	 20
#define LS_RELEASED	 21
#define LS_DOUBLECLICKED 22
#define LS_VALIDATION	 23
#define LS_KEYUNKNOWN	 24

#define CB_VALIDATION	 25
#define CB_PROPOSITION   26
#define CB_KEYUNKNOWN	 27

#define MN_SELECTED	 28
#define MN_KEYUNKNOWN    29
#define MN_ABORTED	 30

#define XLIBEVENT	 50



#endif 		/***  EV_STRUCT_H  ***/


