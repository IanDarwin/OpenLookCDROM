/*
 * $Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/object_list.h,v 1.1 93/01/06 03:27:48 gounares Exp Locker: gounares $
 */
/*
 * object_list.h
 * 
 * declarations and prototypes for the object list popup window
 * 
 * 
 * written by Alexander Gounares  11/8/92
 */
/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#ifndef __object_list_h
#define __object_list_h

typedef struct _OLP {
	Frame           frame;
	Panel           panel;
	Panel_item		first_button;
	Panel_item      list;
	Editor         *ped;
}               OLP;

void            create_olp_proc();
void            olp_list_proc();
void			rescan_olp();
#endif
