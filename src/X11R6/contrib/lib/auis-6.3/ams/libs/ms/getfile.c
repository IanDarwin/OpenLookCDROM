/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/getfile.c,v 2.11 1993/08/25 20:35:52 susan Exp $";
#endif

#include <andrewos.h> /* sys/file.h */
#include <ms.h>
#include <sys/stat.h>

MS_GetPartialFile(FileName, Buf, BufLim, offset, remaining, ct)
char *FileName, *Buf;
int BufLim, offset, *remaining, *ct;
{
    int fd;
    struct stat statbuf;

    debug(1, ("MS_GetPartialFile %s\n", FileName));
    *remaining = 0;
    *ct = 0;
    if ((fd = open(FileName, O_RDONLY, 0)) < 0
	&& (RetryBodyFileName(FileName) < 0
	    || (fd = open(FileName, O_RDONLY, 0)) < 0)) {
	AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_GETPARTIALFILE);
    }
    if (fstat(fd, &statbuf) != 0) {
	close(fd);
	AMS_RETURN_ERRCODE(errno, EIN_FSTAT, EVIA_GETPARTIALFILE);
    }
    if (offset > 0 && lseek(fd, offset, L_SET) < 0) {
	close(fd);
	AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_GETPARTIALFILE);
    }
    if ((*ct = read(fd, Buf, BufLim)) < 0) {
	close(fd);
	AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_GETPARTIALFILE);
    }
    *remaining = statbuf.st_size -offset - *ct;
    if (*ct < BufLim) Buf[*ct] = '\0';  /* Be nice to clients */
    close(fd);
    return(0);
}
