/*
 *
 * 	os.h
 * 	os-dependant infos
 *
 * 	Modification :  11/04/94
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



#ifndef TK_OS_H
#define TK_OS_H


#ifndef UNIX386_X_SERVER
#define UNIX386_X_SERVER
#endif

/*
#define GENERIC_X_SERVER  	0
#define UNIX386_X_SERVER  	1
#define DESQVIEW_X_SERVER 	2
#define LINUX386_X_SERVER 	3
#define NCD_X_SERVER	  	20
#define TEKTRONIX_X_SERVER 	21
*/



#ifdef UNIX386_X_SERVER
#define HasTCPIP  1
#define HasRPC	  1
#endif


#ifdef  LINUX386_X_SERVER
#define HasTCPIP  1
#define HasRPC	  1
#endif



#ifdef DESQVIEW_X_SERVER
#define HasTCPIP  1
#endif



#ifndef WithdrawnState
#define WithdrawnState 	0
#endif


#endif 




