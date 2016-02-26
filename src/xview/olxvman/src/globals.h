/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: globals.h,v 1.6 89/10/03 15:57:51 kit Exp $
 * $Athena: globals.h,v 4.5 88/12/19 13:46:58 kit Exp $
 *
 * Copyright 1987, 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:    Chris D. Peterson, MIT Project Athena
 * Created:   October 22, 1987
 */

#include "man.h"

/* bookkeeping global variables. */

extern int man_pages_shown;		/* The current number of manual
					   pages being shown, if 0 we exit. */

extern Manual * manual;		        /* The manual structure. */
extern int sections;			/* The number of manual sections. */

extern OLXVManObjs	*OLXVMan;
extern SearchPopupObjs	*SearchPopup;
extern ManualPageObjs	*ManualPages[NUMMANPAGES];
extern int		CurrentManualPage;
extern int		CurrentSection;
extern int		CurrentEntry;
extern char		SaveFile[100];
extern Bool		WasCompressed;

extern char * option_names[];

