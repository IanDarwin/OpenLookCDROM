/*
 * This file is provided for unrestricted use
 * provided that this legend is included on all tape media
 * and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 *
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#include <X11/Xos.h>
#include <stdio.h>
#include <sys/param.h>
#include <xview/xview.h>
#include "xvnews.h"

/* The regular expression routines are the routines that are
   causing the greatest portability problems for xvnews.
   Essentially, there are three types of routines: BSD, featuring
   re_comp and re_exec, AT&T, featuring compile and step, and
   the new POSIX.2 routines regcomp and regexec.

   To keep xvnews portable I'm wrapping these functions into two
   functions: xv_comp and xv_step.
   */

#if defined(SVR4_REGEX)
#define INIT		register char *sp = instring;
#define GETC()		(*sp++)
#define PEEKC()		(*sp)
#define UNGETC(c)	(--sp)
#define RETURN(c)	return c;
#define ERROR(c)	fprintf(stderr, "xvnews: Error %d compiling regexp\n", c)

static char regex_buf[MAXPATHLEN];

#endif

#if defined(POSIX_REGEX)
#include <regex.h>
#else
#include <regexp.h>
#endif

#ifdef POSIX_REGEX
static regex_t xv_regex_local;
#endif

extern char *xvnews_comp(regex_str)
  char *regex_str;
{
#ifdef POSIX_REGEX
  regcomp(&xv_regex_local, regex_str, REG_NOSUB|REG_ICASE);
  return (char *)NULL;
#endif

#ifdef SVR4_REGEX
  compile(regex_str, regex_buf, &regex_buf[MAXPATHLEN], '\0');
  return (char *)NULL;
#endif

#ifdef BSD_REGEX
  return re_comp(regex_str);
#endif  
}

extern int xvnews_exec(s)
  char *s;
{
  int result = -1;
  
#ifdef POSIX_REGEX
  result = !regexec(&xv_regex_local, s, (size_t)0, NULL, 0);
#endif

#ifdef SVR4_REGEX
  result = step(s, regex_buf);
#endif

#ifdef BSD_REGEX
  result = re_exec(s);
#endif

  return result;
}

extern void xvnews_free()
{
#ifdef POSIX_REGEX
  regfree(&xv_regex_local);
#endif
}

