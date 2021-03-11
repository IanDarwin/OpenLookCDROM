/*
 *
 * 	widgets.h
 * 	fichier general
 *
 * 	Modification :  24/04/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
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




#ifndef WIDGETS_H
#define WIDGETS_H


typedef int WidgetID; 

#include <X11/iman/os.h>
#include <X11/iman/tk_colors.h>
#include <X11/iman/tk_misc.h>
#include <X11/iman/bn_struct.h>
#include <X11/iman/ed_struct.h>
#include <X11/iman/sb_struct.h>
#include <X11/iman/ls_struct.h>
#include <X11/iman/cb_struct.h>
#include <X11/iman/mn_struct.h>
#include <X11/iman/ev_struct.h>
#include <X11/iman/wid_struct.h>
#ifdef DESQVIEW_X_SERVER
#include <X11/iman/tk_syst.h>
#include <X11/iman/tk_error.h>
#else
#include <X11/iman/tk_system.h>
#include <X11/iman/tk_errors.h>
#endif
#include <X11/iman/funcs.h>


#endif /* WIDGETS_H */


