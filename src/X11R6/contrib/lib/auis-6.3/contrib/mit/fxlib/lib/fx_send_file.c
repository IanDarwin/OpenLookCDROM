/**********************************************************************
 * File Exchange client library
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_send_file.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_send_file.c,v 1.3 1992/12/15 21:53:21 rr2b R6tape $
 *
 * Copyright 1989, 1990 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 **********************************************************************/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/fx_send_file.c,v 1.3 1992/12/15 21:53:21 rr2b R6tape $";
#endif

#include <mit-copyright.h>

 

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <strings.h>
#include "fxcl.h"

/*
 * fx_send_file -- send a file to the exchange
 */

long
fx_send_file(fxp, p, filename)
     FX *fxp;
     Paper *p;
     char *filename;
{
  FILE *fp;
  struct stat buf;
  Paper to_send;
  long code;

  paper_copy(p, &to_send);

  /* check file status */
  if (stat(filename, &buf)) return((long) errno);
  if (buf.st_mode & S_IFDIR) return((long) EISDIR);
  if (buf.st_mode & S_IEXEC) to_send.flags |= PAPER_EXECUTABLE;

  /* send file with correct name */
  if (!to_send.filename) {
    to_send.filename = rindex(filename, '/') + 1;
    if (to_send.filename == (char *) 1) to_send.filename = filename;
  }
  if ((fp = fopen(filename, "r")) == NULL) return((long) errno);

  code = fx_send(fxp, &to_send, fp);
  (void) fclose(fp);
  return(code);
}
