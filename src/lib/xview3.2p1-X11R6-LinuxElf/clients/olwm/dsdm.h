#ident "@(#)dsdm.h	1.3	93/06/28"

/*
 *      (c) Copyright 1992 Sun Microsystems, Inc.
 */

/*
 *	Sun design patents pending in the U.S. and foreign countries. See
 *	LEGAL_NOTICE file for terms of the license.
 */

#ifndef _OLWM_DSDM_H
#define _OLWM_DSDM_H

/* constants */

#define DND_VERSION		0
#define DND_INTEREST_RECT	0
#define DND_INTEREST_WINDOW	1
#define DND_SITE_DEFAULT	(1<<2)
#define DND_SITE_FORWARD	(1<<3)

/* public functions */

extern void DragDropStartDSDM();
extern void DragDropStopDSDM();
extern void DragDropInit();

#endif /* _OLWM_DSDM_H */
