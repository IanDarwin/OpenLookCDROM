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
/* 
 * Returns the name of the connection AfOpenConnection would use.  This is
 * better than just printing the "display" variable in a program because that
 * could be NULL and/or there could be an environment variable set.
 * This makes it easier for programmers to provide meaningful error
 * messages. 
 * 
 */
/* Original for X written at Waterloo - JMSellens */

#include <stdio.h>

char *getenv();

/* Note that AudioFile looks at the DISPLAY variable, too. */

char *
AFAudioConnName(char *conn)
{
    char *a;
    if ( conn != (char *) NULL && *conn != '\0' )
	return(conn);
    if ( (a = getenv("AUDIOFILE")) != (char *)NULL )
	return(a);
    if ( (a = getenv("DISPLAY")) != (char *)NULL )
	return(a);
    return("");
}
