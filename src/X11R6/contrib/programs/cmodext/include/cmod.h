/*

Copyright (c) 1990 - 1994  FUJITSU LIMITED
Copyright (c) 1990 - 1991  PFU Limited

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE FUJITSU LIMITED AND PFU LIMITED BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the FUJITSU LIMITED and
PFU Limited shall not be used in advertising or otherwise to promote
the sale, use or other dealings in this Software without prior written
authorization from the FUJITSU LIMITED and PFU Limited.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
                               fujiwara@a80.tech.yk.fujitsu.co.jp

*/

#ifndef _CMOD_H_
#define _CMOD_H_

#ident "@(#)cmod.h	1.6 4/3/91"

/*
 * name of this extension
 */
#define CONTROLMODIFIERS_PROTOCOL_NAME	 "Control-Modifiers"

/*
 * current version numbers
 */
#define CONTROLMODIFIERS_MAJOR_VERSION	1
#define CONTROLMODIFIERS_MINOR_VERSION	0

/*
 * mask
 */
/* LockMask etc. are in X.h */
#define CModKanaLockMask		(1 << 16)

/*
 * request stuff
 */
#define X_ControlModifiersGetVersion	0
#define X_ControlModifiersSetMask	1
#define X_ControlModifiersGetMask	2
#define X_ControlModifiersGetState	3
#define X_ControlModifiersChangeState	4
#define X_ControlModifiersGrab		5
#define X_ControlModifiersUngrab	6

/*
 * event stuff
 */
#define ControlModifiersNotify		0
#define ControlModifiersNumberEvents	(ControlModifiersNotify + 1)

/*
 * error stuff
 */
#define ControlModifiersError		0
#define ControlModifiersNumberErrors	(ControlModifiersError + 1)

#ifndef _CONTROLMODIFIERS_SERVER_
/*
 * Extra definitions that will only be needed in the client
 */

typedef struct {
    int type;			/* type of event */
    unsigned long serial;	/* # of last request processed by server */
    int send_event;		/* true if this came from SendEvent request */
    Display *display;		/* Display tye event was read from */
    unsigned long change;	/* change mask */
    unsigned long state;	/* state */
} ControlModifiersEvent;

#endif /* not _CONTROLMODIFIERS_SERVER */

#endif /* _CMOD_H_ */
