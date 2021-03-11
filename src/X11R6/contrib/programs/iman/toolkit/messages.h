/*
 *
 * 	messages.h  
 * 	messages entre le client et le WM
 *
 * 	Modification :  23/04/94
 *
 *	Copyright (c) 1993, 1994 Bruno RIVAS
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



#ifndef MESSAGES_H
#define MESSAGES_H


#define WmNoMessage		0
#define WmJustLoaded		1000
#define WmResetColors		1001
#define WmEndOfServices 	1002
#define WmSaveYourself  	1003
#define WmFreezeWidgets 	1004
#define WmUnfreezeWidgets 	1005


#define ClSetWindowName 	2000
#define ClSetIconName		2001
#define ClResetColors   	2002
#define ClResetDecoration 	2003
#define ClResetDesktop	  	2004

#define RequestingHelp	  	3000



#endif

