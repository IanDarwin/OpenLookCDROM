/*
 * $Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/object.h,v 1.1 93/01/06 03:27:45 gounares Exp Locker: gounares $
 */
/*
 * object.h
 * 
 * standard blah blah here.  Object.[ch] set of modules deal with the interface
 * for object linking, etc.  The actual mucky business of saving/loading
 * files is handled by file_io.[ch] module.
 * 
 * written by Alex Gounares
 * 
 * 9/23/92
 * 
 * include after X includes and editor.h
 */
/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#ifndef __object_h
#define __object_h

#include "editor.h"

typedef struct _OC {
	Frame frame;
	Panel panel;
	Panel_item section_choice;
	Panel_item file_label;
	Panel_item text_button;
	Textsw text;
	Panel_item exec_choice;
	Menu file_menu;
	Menu textsw_menu;
	Panel_item file_button;
	Panel_item panel2;
	Panel_item apply_button;
	Panel_item cancel_button;
	Editor *ped;
	Object_list *pOL1;
	Editor *ped1;
	Object_list *pOL2;
	Editor *ped2;
	char *szHead;
	char *szHeadfile;
	char *szDesc;
	char *szTail;
	char *szTailfile;
	int fState;
} OC;

Object_list *get_block( /*Editor *pEditor*/);
void obj_prev_proc();
void obj_next_proc();
void obj_run_proc();
void obj_show_proc();
void obj_create_proc();
void obj_delete_proc();
void obj_desc_proc();
void show_ol();
OC *create_OC();
void show_oc();
void update_object_status();


#endif
