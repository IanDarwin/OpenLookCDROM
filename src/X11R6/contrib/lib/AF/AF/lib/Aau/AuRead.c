/*
 * Aau - X Authorization Database Library
 *
 * $XConsortium: AuRead.c,v 1.3 89/11/19 13:10:23 rws Exp $
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

static
read_short (unsigned short *shortp, FILE *file)
{
    unsigned char   file_short[2];

    if (fread ((char *) file_short, (int) sizeof (file_short), 1, file) != 1)
	return 0;
    *shortp = file_short[0] * 256 + file_short[1];
    return 1;
}

static
read_counted_string (unsigned short *countp, char **stringp, FILE *file)
{
    unsigned short  len;
    char	    *data;

    if (read_short (&len, file) == 0)
	return 0;
    if (len == 0) {
	data = 0;
    } else {
    	data = (char *)malloc ((unsigned) len);
    	if (!data)
	    return 0;
    	if (fread (data, (int) sizeof (char), (int) len, file) != len) {
	    free ((void *)data);
	    return 0;
    	}
    }
    *stringp = data;
    *countp = len;
    return 1;
}

AFauth *
AFauReadAuth (FILE	*auth_file)
{
    AFauth   local;
    AFauth   *ret;

    if (read_short (&local.family, auth_file) == 0)
	return 0;
    if (read_counted_string ((unsigned short *) &local.address_length, 
		(char **) &local.address, auth_file) == 0)
	return 0;
    if (read_counted_string ((unsigned short *) &local.number_length, 
		(char **) &local.number, auth_file) == 0) {
	if (local.address) free ((void *)local.address);
	return 0;
    }
    if (read_counted_string ((unsigned short *) &local.name_length, 
		(char **) &local.name, auth_file) == 0) {
	if (local.address) free ((void *)local.address);
	if (local.number) free ((void *)local.number);
	return 0;
    }
    if (read_counted_string ((unsigned short *) &local.data_length, 
		(char **) &local.data, auth_file) == 0) {
	if (local.address) free ((void *)local.address);
	if (local.number) free ((void *)local.number);
	if (local.name) free ((void *)local.name);
	return 0;
    }
    ret = (AFauth *) malloc (sizeof (AFauth));
    if (!ret) {
	if (local.address) free ((void *)local.address);
	if (local.number) free ((void *)local.number);
	if (local.name) free ((void *)local.name);
	if (local.data) free ((void *)local.data);
	return 0;
    }
    *ret = local;
    return ret;
}
