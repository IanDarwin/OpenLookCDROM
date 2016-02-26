/*
 * $Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/s_pad.h,v 1.13
 * 93/01/06 00:55:05 gounares Exp Locker: gounares $
 */

/*
 * s_pad.h
 * 
 * declarations for the scratch pad
 */

/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#ifndef __s_pad_h
#define __s_pad_h

typedef struct _SP {
	Frame           frame;
	Textsw          textsw;
	void           *ped;
}               SP;

SP             *create_sp();
void            show_sp_proc();
void            clear_sp();
void            set_sp();
void            set_sp_ped();

#endif
