/*
 *
 * 	tk_colors.h
 * 	couleurs des widgets
 *
 * 	Modification :  26/12/93
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



#ifndef TK_COLORS_H
#define TK_COLORS_H



typedef struct {
		unsigned long bg, light, shadow, text, text_grayed;
		unsigned long border_active, border_inactive;
		unsigned long title_bg_active, title_bg_inactive;
		unsigned long title_text_active, title_text_inactive;
		} WindowColors;


typedef struct {
		unsigned long icn_bg;
		unsigned long icn_light, icn_shadow;
		unsigned long icn_draw, icn_draw_bg;
		unsigned long title_bg_active;
		unsigned long title_text_active;
		unsigned long title_bg_inactive;
		unsigned long title_text_inactive;
		} IconColors;		


#endif		/* TK_COLORS_H */
		

