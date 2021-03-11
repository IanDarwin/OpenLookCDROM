/*
 * The FX (File Exchange) Server
 *
 * $Author: susan $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/util.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/util.c,v 1.4 1993/05/17 16:52:35 susan Exp $
 *
 * Copyright 1989, 1990 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

#include <mit-copyright.h>

/*
 * This file contains miscellaneous utility routines.
 */

#ifndef lint
static char rcsid_util_c[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/util.c,v 1.4 1993/05/17 16:52:35 susan Exp $";
#endif /* lint */

#include <fxserver.h>
#include <ctype.h>
#include <sys/file.h>
#include <syslog.h>

/*
 * Check a string to see if it is a valid course name.  This means
 * that it does not begin with "." and only contains alphanumeric
 * characters and ".".  Return 1 on valid, 0 on invalid.
 */

valid_course_name(s)
    char *s;
{
    register char *ptr;
    register short cc=0;		/* character count */

    if (!*s || *s == '.')
	return 0;
    
    for (ptr=s; *ptr; ptr++, cc++)
      {
	if (!isalnum(*ptr) && *ptr != '.' && *ptr != '_')
	  return 0;
	if (cc > 128) return 0;	/* limit length */
      }
    return 1;
}

/*
 * Check a string to see if it is a valid file name.  This means that
 * it doesn't contain any slashes.  Return 1 on valid, 0 on invalid.
 */

valid_filename(s)
    char *s;
{
    register char *ptr;
    register short cc=0;

    for (ptr=s; *ptr; ptr++, cc++)
	if (*ptr == '/' || cc > 128)
	    return 0;
    return 1;
}

/*
 * Check a user's access to perform a certain operation.  The user's
 * presense in the specified acl file is sufficient to grant access.
 */

check_access(file)
    char *file;
{
    char aclname[MAXPATHLEN];

    sprintf(aclname, "%sACL-%s", curconn->coursepath, file);
    Debug((stderr, "Checking access in %s\n", aclname));

    return acl_check(aclname, curconn->authname);
}

/*
 * Check a user's access to see if it's a GOD
 */

is_god()
{
    char aclname[MAXPATHLEN];

    sprintf(aclname, "%s/ACL-GOD", root_dir);
    Debug((stderr, "Checking access in %s\n", aclname));

    return acl_check(aclname, curconn->authname);
}

/*
 * Check a Paper to see if it contains a wildcard.
 */

contains_wildcard(paper)
    Paper *paper;
{
  register char *s;

  /* Don't allow control characters, esp \001 which could be used
   * to fake database entries.  The error message being about
   * wildcards is confusing, but nobody will get it except users
   * who are trying to fool the server.
   */
  for(s=paper->author; *s != '\0'; s++)
    if (*s < ' ') return(1);
  for(s=paper->filename; *s != '\0'; s++)
    if (*s < ' ') return(1);
  for(s=paper->location.host; *s != '\0'; s++)
    if (*s < ' ') return(1);
  for(s=paper->owner; *s != '\0'; s++)
    if (*s < ' ') return(1);
  for(s=paper->desc; *s != '\0'; s++)
    if (*s < ' ') return(1);

  /* Don't allow wildcards. */
    return (paper->type == TYPE_WILDCARD ||
	    paper->assignment == ASSIGNMENT_WILDCARD ||
	    !strcmp(paper->author, AUTHOR_WILDCARD) ||
	    !strcmp(paper->owner, OWNER_WILDCARD) ||
	    !strcmp(paper->filename, FILENAME_WILDCARD));
}

/*
 * Copy a Paper into another Paper, and malloc new strings for it.
 */

copy_paper(src, dest)
    Paper *src;
    Paper *dest;
{
    *dest = *src;
    dest->author = xsave_string(dest->author);
    dest->owner = xsave_string(dest->owner);
    dest->desc = xsave_string(dest->desc);
    dest->filename = xsave_string(dest->filename);
    dest->location.host = xsave_string(dest->location.host);
}

/*
 * Free the malloc'ed portions of a Paper.
 */

free_paper(p)
    Paper *p;
{
    xfree(p->author);
    xfree(p->owner);
    xfree(p->desc);
    xfree(p->filename);
    xfree(p->location.host);
}

/*
 * Print a paper description into a local string.
 */

#ifdef DEBUG
char *print_paper(paper)
    Paper *paper;
{
    static char bfr[2048];

    sprintf(bfr, PRINTPAPER(paper));
    
    return bfr;
}
#endif /* DEBUG */

/*
 * Copy a file.
 */

copy_file(src, dest)
    char *src;
    char *dest;
{
    int srcfd, destfd, nbytes;
    char bfr[1024];

    if ((srcfd = open(src, O_RDONLY)) == -1)
	return 1;
    if ((destfd = open(dest, O_WRONLY|O_CREAT, 0600)) == -1) {
	close(srcfd);
	return 1;
    }
    while ((nbytes = read(srcfd, bfr, sizeof bfr)) > 0) {
	if (write(destfd, bfr, nbytes) != nbytes) {
	    close(srcfd);
	    close(destfd);
	    unlink(dest);
	    return 1;
	}
    }
    close(srcfd);
    close(destfd);
    if (nbytes == -1) {
	unlink(dest);
	return 1;
    }
    return 0;
}

/*
 * Count the number of chars, words, and lines in a buffer.
 */

do_wc(bfr, size, token, words, lines)
    char *bfr;
    int size;
    int *token;
    int *words, *lines;
{
    char *ptr;
    
    for (ptr=bfr; size; size--) {
	if (*ptr > ' ' && *ptr < 0177) {
	    if (!*token) {
		(*words)++;
		(*token)++;
	    }
	    continue;
	}
	if (*ptr == '\n')
	    (*lines)++;
	else if (*ptr != ' ' && *ptr != '\t')
	    continue;
	*token = 0;
    }
}
    
/*
 * Display a fatal error message and exit.
 */

fatal(s, a, b, c, d, e)
    char *s;
    int a, b, c, d, e;
{
    extern char *sys_errlist[];
    extern int errno;
    
    printf("fxserver: fatal error - ");
    printf(s, a, b, c, d, e);
    printf("\n  Last system error = %s\n", sys_errlist[errno]);
    exit(1);
}

/*
 * Log a warning message.
 */

do_log(type, s, a, b, c, d, e)
    int type;
    char *s;
    int a, b, c, d, e;
{
    static int opened = 0;

    if (!opened) {
#ifdef LOG_LOCAL4
	openlog("fxserver", LOG_PID, LOG_LOCAL4);
#else
	openlog("fxserver", LOG_PID);
#endif
	opened = 1;
    }
    
    syslog(type, s, a, b, c, d, e);
}

log_warning(s, a, b, c, d, e)
    char *s;
    int a, b, c, d, e;
{
    do_log(LOG_WARNING, s, a, b, c, d, e);
}

log_info(s, a, b, c, d, e)
    char *s;
    int a, b, c, d, e;
{
    do_log(LOG_INFO, s, a, b, c, d, e);
}

/*
 * Malloc and die on no more memory.
 */

#ifndef MALLOC_LEAK
char *xmalloc(n)
    int n;
{
    char *ret;

    ret = (char *)malloc((unsigned)n);
    if (!ret)
	fatal("Out of memory!!");
    return ret;
}

/*
 * Realloc and die on no more memory.
 */

char *xrealloc(s, n)
    char *s;
    int n;
{
    char *ret;

    ret = (char *)realloc(s, (unsigned)n);
    if (!ret)
	fatal("Out of memory!!");
    return ret;
}

/*
 * Free memory allocated by xmalloc or xrealloc.
 */

xfree(s)
    char *s;
{
    free(s);
}
#endif /* MALLOC_LEAK */

/*
 * Save a string into a malloc'ed area.
 */

char *xsave_string(s)
    char *s;
{
    char *ret;

    ret = xmalloc(strlen(s)+1);
    (void) strcpy(ret, s);
    return ret;
}
