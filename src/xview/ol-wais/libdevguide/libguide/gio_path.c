/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
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

#ifndef lint
static char     sccsid[] = "@(#)gio_path.c	2.10 91/10/15 Copyright 1989 Sun Microsystems";
#endif

#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include "gio.h"

#define OK		0
#define	ERROR		-1

/*
 * Functions to manipulate file paths.
 */

#define	GIL_SUFFIX	".G"
#define PROJ_SUFFIX	".P"

/*
 * Expand a path in place.  Returns OK if successful, otherwise sets errno
 * and returns ERROR.
 */
int
#ifdef __STDC__
gio_expand_path(char *path)
#else
gio_expand_path(path)
	char	       *path;
#endif
{
	void		expand_path();
	char		buf[MAXPATHLEN];

	expand_path(path, buf);
	strcpy(path, buf);
	return OK;
}

/*
 * Expand a path to a GIL file in place.  Returns OK if successful, 
 * otherwise sets errno and returns error.
 */
int
#ifdef __STDC__
gio_expand_gil_path(char *path)
#else
gio_expand_gil_path(path)
	char	       *path;
#endif
{
	if (gio_expand_path(path) != OK)
		return ERROR;

	if (gio_is_gil_path(path))
		return OK;

	if (strlen(path) + strlen(GIL_SUFFIX) >= MAXPATHLEN) {
		errno = ENAMETOOLONG;
		return ERROR;
	}

	strcat(path, GIL_SUFFIX);
	return OK;
}


/*
 * Expand a path to a project file in place.
 */
int
#ifdef __STDC__
gio_expand_proj_path(char *path)
#else
gio_expand_proj_path(path)
	char	*path;
#endif
{
	if (gio_expand_path(path) != 0)
		return ERROR;

	if (gio_is_proj_path(path))
		return OK;

	if (strlen(path) + strlen(PROJ_SUFFIX) >= MAXPATHLEN) {
		errno = ENAMETOOLONG;
		return ERROR;
	}

	strcat(path, PROJ_SUFFIX);
	return 0;
}


/*
 * Return True if the given path ends with the gil suffix.
 */
int
#ifdef __STDC__
gio_is_gil_path(char *path)
#else
gio_is_gil_path(path)
	char	*path;
#endif
{
	return (strcmp(path + strlen(path) - strlen(GIL_SUFFIX),
		       GIL_SUFFIX) == 0);
}


/*
 * Return True if the given path ends with the project suffix.
 */
int
#ifdef __STDC__
gio_is_proj_path(char *path)
#else
gio_is_proj_path(path)
	char	*path;
#endif
{
	return (strcmp(path + strlen(path) - strlen(PROJ_SUFFIX),
		       PROJ_SUFFIX) == 0);
}


/*
 * expand_path from OpenWindows V2 FCS XView libraries
 *
 * Handles:
 *	~/ => home dir
 *	~user/ => user's home dir
 *   If the environment variable a = "foo" and b = "bar" then:
 *	$a	=>	foo
 *	$a$b	=>	foobar
 *	$a.c	=>	foo.c
 *	xxx$a	=>	xxxfoo
 *	${a}!	=>	foo!
 *	\$a	=>	\$a
 *
 * Arguments:
 *	nm		input string
 *	pathname	buffer to output expanded path
 */
void
#ifdef __STDC__
expand_path(char *nm, char *buf)
#else
expand_path(nm, buf)
	char  *nm, *buf;
#endif
{
	register char  *s, *d;
	char            lnm[MAXPATHLEN];
	int             q;
	register char  *trimchars = "\n \t";
	char           *getenv();

	/* Strip off leading & trailing whitespace and cr */
	while (strchr(trimchars, *nm) != NULL)
		nm++;
	s = nm + (q = strlen(nm)) - 1;
	while (q-- && strchr(trimchars, *s) != NULL)
		*s = '\0';

	s = nm;
	d = lnm;
	q = nm[0] == '\\' && nm[1] == '~';

	/* Expand inline environment variables */
	while (*d++ = *s)
	{
		if (*s == '\\') {
			if (*(d - 1) = *++s)
			{
				s++;
				continue;
			} else
				break;
		}
		else if (*s++ == '$') {
			register char  *start = d;
			register        braces = *s == '{';
			register char  *value;
			while (*d++ = *s)
				if (braces ? *s == '}' : !isalnum(*s))
					break;
				else
					s++;
			*--d = 0;
			value = getenv(braces ? start + 1 : start);
			if (value) {
				for (d = start - 1; *d++ = *value++;);
				d--;
				if (braces && *s)
					s++;
			}
		}
	}

	/* Expand ~ and ~user */
	nm = lnm;
	s = "";
	if (nm[0] == '~' && !q) { /* prefix ~ */
		if (nm[1] == '/' || nm[1] == 0)
		{ /* ~/filename */
			if (s = getenv("HOME"))
			{
				if (*++nm)
					nm++;
			}
		}
		else
		{	/* ~user/filename */
			register char  *nnm;
			register struct passwd *pw;
			for (s = nm; *s && *s != '/'; s++);
			nnm = *s ? s + 1 : s;
			*s = 0;
			pw = (struct passwd *) getpwnam(nm + 1);
			if (pw == 0) {
				*s = '/';
				s = "";
			} else {
				nm = nnm;
				s = pw->pw_dir;
			}
		}
	}
	d = buf;
	if (*s) {
		while (*d++ = *s++);
		*(d - 1) = '/';
	}
	s = nm;
	while (*d++ = *s++);
}
