/*
 * Aau - X Authorization Database Library
 *
 * $XConsortium: AuWrite.c,v 1.2 88/12/08 16:40:29 keith Exp $
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

#include    "AFauth.h"

write_short (unsigned short s, FILE	*file)
{
    unsigned char   file_short[2];

    file_short[0] = (s & 0xff00) >> 8;
    file_short[1] = s & 0xff;
    if (fwrite ((char *) file_short, (int) sizeof (file_short), 1, file) != 1)
	return 0;
    return 1;
}

static
write_counted_string (unsigned short count, char *string, FILE *file)
{
    if (write_short (count, file) == 0)
	return 0;
    if (fwrite (string, (int) sizeof (char), (int) count, file) != count)
	return 0;
    return 1;
}

int
AFauWriteAuth (FILE *auth_file, AFauth *auth)
{

    if (write_short (auth->family, auth_file) == 0)
	return 0;
    if (write_counted_string ((unsigned short) auth->address_length, 
	(char *) auth->address, auth_file) == 0)
	return 0;
    if (write_counted_string ((unsigned short) auth->number_length, 
	(char *) auth->number, auth_file) == 0)
	return 0;
    if (write_counted_string ((unsigned short) auth->name_length, 
	(char *) auth->name, auth_file) == 0)
	return 0;
    if (write_counted_string ((unsigned short) auth->data_length, 
	(char *) auth->data, auth_file) == 0)
	return 0;
    return 1;
}
