/*
 * $Id: version.c,v 2.3 1994/08/19 19:53:08 billr Exp $
 */
/*
 * version.c - current version of calentool program
 *
 * calentool - day/week/month/year-at-a-glance calendar for XView/Open Look
 * 
 * Copyright 1988, 1989, 1991, 1994 by Tektronix, Inc. - All Rights Reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Tektronix, Inc. not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 * 
 * TEKTRONIX INCORPORATED MAKES NO REPRESENTATIONS ABOUT THE
 * SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS"
 * WITHOUT EXPRESS OR IMPLIED WARRANTY.  TEKTRONIX INCORPORATED
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO
 * EVENT SHALL TEKTRONIX INCORPORATED BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author: Bill Randle, Tektronix, Inc. <billr@saab.cna.tek.com>
 */ 

#include "ct.h"
#include "patchlevel.h"

/*#define TEST */

static char vers[64];
static char vers_name[] = "Calendar Tool V2.3X";
#ifdef TEST
static char vers_date[] = " [08/17/94]";
static char vers_test[] = "delta";
#endif

char *
version()
{
	char vers_pl[4];

	strcpy(vers, vers_name);
	if (PATCHLEVEL > 0) {
		sprintf(vers_pl, "p%d", PATCHLEVEL);
		strcat(vers, vers_pl);
	}
#ifdef TEST
	strcat(vers, vers_test);
	strcat(vers, vers_date);
#endif

	return vers;
}
