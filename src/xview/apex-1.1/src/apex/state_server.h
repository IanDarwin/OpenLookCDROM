/*
 * $Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/state_server.h,v 1.1 93/01/06 03:27:52 gounares Exp Locker: gounares $
 */
/*
 * state_server.h
 * 
 * The usual stuff here.
 * 
 * written by Alexander Gounares
 * 
 * 9/27/92
 */
/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#ifndef __state_server_h
#define __state_server_h

#ifndef __navigator_h
#include "navigator.h"
#endif

void            add_editor();
void            remove_editor();
int             fLast_Editor();
void            remove_editor_by_client();
int             get_next_editor_id();
Editor        **get_editor_array();
NG             *find_navigator();
void            add_navigator();
void            remove_navigator();
Editor         *get_editor_by_client();

#endif
