/*
 *
 * 	hs_data.h
 * 	Data used by the HS
 *
 * 	Modification :  04/04/94
 *
 *	Copyright (c) 1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
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
 *      IMAN Help Server version 1.0
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */




#ifndef _IMAN_HS_DATA_H
#define _IMAN_HS_DATA_H





#define ErFileCorrupted		-100
#define ErBadFile		-101
#define ErBadResource		-102
#define ErBadTopic		-103
#define ErBadItem		-104
#define ErNoHelpFile		-105
#define ErNoPrinter		-106


#define NoAction		0
#define AboutAction		1


#define	MACTIVE			0
#define MSILENT			1



#ifdef  DESQVIEW_X_SERVER
#define FILELENGTH		13
#define HELP_DIRECTORY		"/dvx/iman/help"
#else
#define FILELENGTH		35
#define HELP_DIRECTORY		"/usr/lib/iman/help"
#endif



#ifdef _IMAN_HS_MAIN_C

  unsigned int mode;

  TkDisplay *tk_display;
  TkEvent tk_event;  

  Window hs_main_window;
  Window hs_error_window;
  GC gc;

  unsigned int numsessions;
  unsigned int maxsessions;

  HelpSession *sessions;

  Pixmap bm_bug, bm_warning1, bm_warning2, bm_warning3, bm_question1, bm_question2, bm_question3, bm_stop1, bm_stop2;
  Pixmap bm_stack, bm_dir, bm_file, bm_none;
  Pixmap bm_help1, bm_help2;
  Pixmap xpm_next, xpm_next_mask;
  Pixmap xpm_previous, xpm_previous_mask;
  Pixmap xpm_index, xpm_index_mask;
  Pixmap xpm_printer, xpm_printer_mask;
  Pixmap xpm_find, xpm_find_mask;
  Pixmap xpm_glossary, xpm_glossary_mask;
  unsigned long red1, yellow1;
  unsigned long topicjump, glosdef;


  char *wm_file, *hs_file;

#else

  extern unsigned int mode;

  extern TkDisplay *tk_display;
  extern TkEvent tk_event;  

  extern Window hs_main_window;
  extern Window hs_error_window;
  extern GC gc;

  extern unsigned int numsessions;
  extern unsigned int maxsessions;

  extern HelpSession *sessions;

  extern Pixmap bm_bug, bm_warning1, bm_warning2, bm_warning3, bm_question1, bm_question2, bm_question3, bm_stop1, bm_stop2;
  extern Pixmap bm_stack, bm_dir, bm_file, bm_none;
  extern Pixmap bm_help1, bm_help2;
  extern Pixmap xpm_next, xpm_next_mask;
  extern Pixmap xpm_index, xpm_index_mask;
  extern Pixmap xpm_previous, xpm_previous_mask;
  extern Pixmap xpm_printer, xpm_printer_mask;
  extern Pixmap xpm_find, xpm_find_mask;
  extern Pixmap xpm_glossary, xpm_glossary_mask;
  extern unsigned long red1, yellow1;
  extern unsigned long topicjump, glosdef;


  char *wm_file, *hs_file;

#endif




#endif


