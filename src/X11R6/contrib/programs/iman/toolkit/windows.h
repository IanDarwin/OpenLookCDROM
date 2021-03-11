/*
 *
 * 	windows.h
 * 	informations sur les fenetres
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



#ifndef WINDOWS_H
#define WINDOWS_H


# define TOP_LEVEL_WINDOW 	0
# define DIALOG_BOX		100
# define ICON_WINDOW		101
# define TITLE_BAR		102
# define BORDER_BOX		103
# define REPARENTING_WINDOW	105
# define REPARENTING_DIALOG	106
# define SMALL_TOP_WINDOW	107
# define SMALL_SIDE_WINDOW	108



# define TitleBar		1
# define CloseBox		2
# define IconifyBox		4
# define ZoomBox		8
# define Border			16
# define MDWParent 		32
# define MDWChild		64
# define GroupLeader		128
# define GroupMember		256
# define Overlapped		512
# define AlwaysOnTop		1024
# define Unmoveable		2048
# define Unresizable		4096
# define MoveableWhenZoomed	8192
# define Iconic 		16384
# define LeftSmallBar		32768		
# define RightSmallBar		65536



#ifndef IgnoreState
#define IgnoreState 	5
#endif 



#endif


