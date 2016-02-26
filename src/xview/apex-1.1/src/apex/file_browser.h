/*
 * $Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/file_browser.h,v
 * 1.13 93/01/06 00:54:43 gounares Exp Locker: gounares $
 */

/*
 * file_browser.h
 * 
 * include after xview includes
 * 
 * written by Alexander Gounares 10/1/92
 * 
 */

/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#ifndef __file_browser_h
#define __file_browser_h

#include "editor.h"

typedef struct _File_browser {
	Frame           frame;
	Panel           panel;
	Panel_item      dir_item;
	Panel_item      file_item;
	Panel_item      filter_item;
	Panel_item      filter_choice_item;
	Panel_item      file_list;
	Panel_item      dir_list;
	Panel_item      btn1;
	Panel_item      btn2;
	Panel_item      btn3;
	Panel_item      file_msg;
	Panel_item      dir_msg;
	Editor         *pEd;
}               FB;

FB             *create_generic_browser( /* Editor *pEd */ );
void            create_load_browser_proc();
void            create_save_browser_proc();
void            create_link_browser_proc();
void            save_existing_file_proc();
char           *re_comp();
char           *pmatch();


#endif
