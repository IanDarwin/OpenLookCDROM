#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/auto.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
#endif /* RCS_ID */
#endif /* LINT */
/***********************************************************
$Copyright$,1994 by Tektronix, Inc., Wilsonville, Oregon.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Tektronix or Tek not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

TEKTRONIX DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
TEKTRONIX BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/*
 * May 19, 1992
 * Copyright 1992 Guido van Rossum And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Guido van Rossum And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 */

/*
 * A meta-handler that recognizes most file types by looking in the
 * first part of the file.  The file must be seekable!
 * (IRCAM sound files are not recognized -- these don't seem to be
 * used any more -- but this is just laziness on my part.) 
 */

#include "st.h"

IMPORT void gettype();

int
autostartread(ft)
ft_t ft;
{
	char *type;
	char header[132];
	if (!ft->seekable)
		return fail(ft, "Type AUTO input must be a file, not a pipe");
	if (fread(header, 1, sizeof header, ft->fp) != sizeof header)
		return fail(ft, "Type AUTO detects short file");
	fseek(ft->fp, 0L - sizeof header, 1); /* Seek back */
	type = 0;
	if (strncmp(header, ".snd", 4) == 0 ||
	    strncmp(header, "dns.", 4) == 0 ||
	    header[0] == '\0' && strncmp(header+1, "ds.", 3) == 0) {
		type = "au";
	}
	else if (strncmp(header, "FORM", 4) == 0) {
		if (strncmp(header + 8, "AIFF", 4) == 0)
			type = "aiff";
		else if (strncmp(header + 8, "8SVX", 4) == 0)
			type = "8svx";
	}
	else if (strncmp(header, "RIFF", 4) == 0 &&
		 strncmp(header + 8, "WAVE", 4) == 0) {
		type = "wav";
	}
	else if (strncmp(header, "Creative Voice File", 19) == 0) {
		type = "voc";
	}
	else if (strncmp(header+65, "FSSD", 4) == 0 &&
		 strncmp(header+128, "HCOM", 4) == 0) {
		type = "hcom";
	}
	else if (strncmp(header, "SOUND", 5) == 0) {
		type = "sndt";
	}
	else if (header[0] == 0 && header[1] == 0) {
		int rate = (header[2] & 0xff) + ((header[3] & 0xff) << 8);
		if (rate >= 4000 && rate <= 25000)
			type = "sndr";
	}
  	if (type == 0) {
  		printf("Type AUTO doesn't recognize this header\n");
                printf("Trying: -t raw -r 11000 -b -u\n\n");
                type = "raw";
                ft->info.rate = 11000;
                ft->info.size = BYTE;
                ft->info.style = UNSIGNED;
                }
	report("Type AUTO changed to %s", type);
	ft->filetype = type;
	AFGetType(ft); /* Change ft->h to the new format */
	return (* ft->h.startread)(ft);
}

autostartwrite(ft) 
ft_t ft;
{
	fail(ft, "Type AUTO can only be used for input!");
}
