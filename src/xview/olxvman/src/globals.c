/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: globals.c,v 1.6 89/10/03 15:57:50 kit Exp $
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

#if ( !defined(lint) && !defined(SABER))
  static char rcs_version[] = "$Athena: globals.c,v 4.5 88/12/19 13:46:53 kit Exp $";
#endif

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <gdd.h>
#include "olxvman_ui.h"
#include "man.h"

Manual * manual;		/* The manual structure. */
int sections;			/* The number of manual sections. */

int man_pages_shown;		/* The current number of manual
				   pages being shown, if 0 we exit. */

OLXVManObjs	*OLXVMan;
SearchPopupObjs	*SearchPopup;
ManualPageObjs	*ManualPages[NUMMANPAGES];
int		CurrentManualPage = 0;
int		CurrentSection = -1;
int		CurrentEntry = -1;
char		SaveFile[100];
Bool		WasCompressed;
