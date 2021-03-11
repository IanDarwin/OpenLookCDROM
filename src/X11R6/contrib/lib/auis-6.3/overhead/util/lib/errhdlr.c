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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/errhdlr.c,v 1.6 1992/12/15 21:09:01 rr2b R6tape $";
#endif

#include <util.h>
#include <errno.h>
#include <andrewos.h>		/* <strings.h> */

char EH_Error_Msg[EH_ERR_MSG_BUF_SIZE];
EH_environment *_error_handler_env;

char *emalloc(size)
long size;
{
  /* Returns a pointer to a hunk of memory of size. 
     Errors are signalled. */
  char *tmp_ptr;

  EH_cond_error_on(!(tmp_ptr = (char *)malloc(size)),
		   EH_ret_code(EH_module_system,ENOMEM),
		   "Malloc failed");
  return(tmp_ptr);
}

char *erealloc(old, newsize)
char *old;
long newsize;
{
  /* Returns a pointer to a hunk of memory of newsize, with
     the contents of old (trunc) copied into it. 
     Errors are signalled.  */
  char *tmp_ptr;

  EH_cond_error_on(!(tmp_ptr = (char *)realloc(old, newsize)),
		   EH_ret_code(EH_module_system,ENOMEM),
		   "Realloc failed");
  return(tmp_ptr);
}


char *CopyString(old)
char *old;
{
  /* Returns a newly emalloc'd copy of the string.
     Errors from emalloc are passed up. */

  return(strcpy(emalloc(strlen(old)+1), old));
}

