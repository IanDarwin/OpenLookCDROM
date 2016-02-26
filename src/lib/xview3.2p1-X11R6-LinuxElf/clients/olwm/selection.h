#ident	"@(#)selection.h	1.3	93/06/28 SMI"

/*
 *      (c) Copyright 1992 Sun Microsystems, Inc.
 */

/*
 *	Sun design patents pending in the U.S. and foreign countries. See
 *	LEGAL_NOTICE file for terms of the license.
 */

#ifndef _OLWM_SELECTION_H
#define _OLWM_SELECTION_H

extern	Time	SelectionTime;

extern	Bool	IsSelected(/*  client  */);
extern	int	AddSelection(/*  client, time  */);
extern	Bool	RemoveSelection(/*  client  */);
extern	Bool	ToggleSelection(/*  client, time  */);
extern	void	ClearSelections(/*  dpy  */);
extern	Client* EnumSelections(/*  void*  */);

extern	void	SelectionInit();
extern	void	SelectionRegister(/*  seln, handler  */);
extern	void	SelectionResponse(/*  event  */);

#endif /* _OLWM_SELECTION_H */
