/*
 * $Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/editor.h,v 1.13
 * 93/01/06 00:54:38 gounares Exp Locker: gounares $
 */

/*
 * editor.h
 * 
 * include after X includes
 * 
 * written by Alexander Gounares 9/18/92
 */

/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#ifndef __editor_h
#define __editor_h

#include <sys/param.h>
#include <dirent.h>
#include "file_io.h"
#include "s_pad.h"

typedef struct _Editor {
	int             id;
	int             fEdited;
	int             fCaps_lock_on;
	int             fRead_only;
	Textsw          textsw;
	Frame           frame;
	Panel           panel;
	Panel_item      file_button;
	Panel_item      view_button;
	Panel_item      edit_button;
	Panel_item      find_button;
	Panel_item      extras_button;
	Panel_item      tools_button;
	Panel_item      dismiss_button;
	Panel_item      help_button;
	Panel_item      tb_run_button;
	Panel_item      tb_desc_button;
	Panel_item      tb_show_button;
	Panel_item      tb_prev_button;
	Panel_item      tb_next_button;
	Panel_item      tb_create_button;
	Panel_item      tb_delete_button;
	Panel_item      tb_hide_button;
	Panel_item      tb_csh_button;
	Panel_item      tb_nav_button;
	Panel_item      tb_apex_button;
	Panel_item      tb_sp_button;
	Menu            file_menu;
	Menu_item       load_menu_item;
	Menu_item       save_menu_item;
	Menu_item       save_as_menu_item;
	Menu            view_menu;
	Menu            tools_menu;
	Menu            help_menu;
	char            szDirname[MAXPATHLEN];
	char            szFilename[MAXNAMLEN];
	Icon            icon;
	Object_list    *pOL_current;
	Server_image    create_image;
	Server_image    finish_image;
	struct _Editor *new_ped;
	File           *pFile;
	SP             *psp;
	void           *pOLP;	       /* pointer to the Object_list popup */
}               Editor;

#define EDITOR_MAIN	45
#define EDITOR_SMALL	20

Editor         *create_editor();
Editor         *new_editor();
void            set_editor();
Menu            editor_menu_gen_proc();

#endif
