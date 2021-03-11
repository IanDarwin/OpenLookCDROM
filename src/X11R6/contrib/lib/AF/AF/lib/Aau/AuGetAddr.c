/*
 * Xau - X Authorization Database Library
 *
 * $XConsortium: AuGetAddr.c,v 1.9 89/12/12 17:15:23 rws Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
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
 * Author:  Keith Packard, MIT X Consortium
 */
/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

# include "AFauth.h"
# include "Aos.h"

static
binaryEqual (register char *a, register char *b, register int len)
{
    while (len--)
	if (*a++ != *b++)
	    return 0;
    return 1;
}

AFauth *
AFauGetAuthByAddr (
#if NeedWidePrototypes
unsigned int	family,
unsigned int	address_length,
#else
unsigned short	family,
unsigned short	address_length,
#endif
const char*	address,
#if NeedWidePrototypes
unsigned int	number_length,
#else
unsigned short	number_length,
#endif
const char*	number,
#if NeedWidePrototypes
unsigned int	name_length,
#else
unsigned short	name_length,
#endif
const char*	name)
{
    FILE    *auth_file;
    char    *auth_name;
    AFauth   *entry;

    auth_name = AFauFileName ();
    if (!auth_name)
	return 0;
    if (access (auth_name, R_OK) != 0)		/* checks REAL id */
	return 0;
    auth_file = fopen (auth_name, "r");
    if (!auth_file)
	return 0;
    for (;;) {
	entry = AFauReadAuth (auth_file);
	if (!entry)
	    break;
	/*
	 * Match when:
	 *   either family or entry->family are AFFamilyWild or
	 *    family and entry->family are the same
	 *  and
	 *   either address or entry->address are empty or
	 *    address and entry->address are the same
	 *  and
	 *   either number or entry->number are empty or
	 *    number and entry->number are the same
	 *  and
	 *   either name or entry->name are empty or
	 *    name and entry->name are the same
	 */

	if ((family == AFFamilyWild || entry->family == AFFamilyWild ||
	     (entry->family == family &&
	      address_length == entry->address_length &&
	      binaryEqual ((char *)entry->address, (char *)address, (int)address_length))) &&
	    (number_length == 0 || entry->number_length == 0 ||
	     (number_length == entry->number_length &&
	      binaryEqual ((char *)entry->number, (char *)number, (int)number_length))) &&
	    (name_length == 0 || entry->name_length == 0 ||
	     (entry->name_length == name_length &&
 	      binaryEqual ((char *)entry->name, (char *)name, (int)name_length))))
	    break;
	AFauDisposeAuth (entry);
    }
    (void) fclose (auth_file);
    return entry;
}
