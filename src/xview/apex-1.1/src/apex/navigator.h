/*
 * $Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/navigator.h,v 1.13
 * 93/01/06 00:54:53 gounares Exp Locker: gounares $
 */

/*
 * navigator.h
 * 
 * written by Alexander Gounares 11/8/92
 */

/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#ifndef __navigator_h
#define __navigator_h

#include <X11/X.h>
#include <X11/Xlib.h>
#include <xview/scrollbar.h>

#define MAX_WIDTH 2000
#define MAX_HEIGHT 1000
#define NODE_NORMAL 0
#define NODE_SELECTED 1
void            create_navigator_proc();

typedef int     NG;

#endif
