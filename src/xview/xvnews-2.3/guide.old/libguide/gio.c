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
static char     sccsid[] = "@(#)gio.c	2.28 91/08/26 Copyright 1989 Sun Microsystems";
#endif

/*
 * GUIDE Intermediate Language (GIL) file input / output interface.
 *
 * This file implements the functions required to read and write GIL
 * files.  The GIL syntax is based on Lisp so that they may be read
 * directly into a Lisp or Scheme interpreter.
 *
 * Many of these functions return a string to indicate the begin or
 * end of a special sequence in the GIL syntax, for example, a list.
 * Internally it is common to only use the first character returned.
 * I did it this way so the functions are consistent and to allow 
 * for flexibility in the future.
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <malloc.h>
#include "guide.h"
#include "gio.h"

/*
 * Delimiter for a full name.
 */
#define DELIMITER	':'

/*
 * File pointers.
 */
static FILE    *Inp = stdin;
static FILE    *Outp = stdout;

/*
 * Internal state variables.
 */
static char     Indent[12];			/* indent string */
static int	Indent_level;
static int      Newline = TRUE;			/* TRUE if on new output line */
static char	Buf[MAXPATHLEN];		/* work buffer */

/*
 * System error message definitions.
 */
extern int      sys_nerr;
extern char    *sys_errlist[];

/*
 * Internal function declarations.
 */
static int     	gil_version();
static void     skip_comments();
static char    *get_token();

/*
 * Return the string representation of a boolean.
 */
char    *
#ifdef __STDC__
gio_boolean_string(int b)
#else
gio_boolean_string(b)
	int             b;
#endif
{
	return b ? gio_true_string() : gio_false_string();
}

/*
 * Close the input file.
 */
void
gio_close_input()
{
	if (Inp != stdin)
	{
		fclose(Inp);
		Inp = stdin;
	}
}

/*
 * Close the output file.
 */
void
gio_close_output()
{
	if (Outp != stdout)
	{
		fflush(Outp);
		ftruncate(fileno(Outp), ftell(Outp));
		fclose(Outp);
		Outp = stdout;
	}
}

/*
 * Return the comment string.
 */
char	       *
gio_comment_string()
{
	return ";";
}

/*
 * Return the boolean false string.
 */
char	       *
gio_false_string()
{
	return "nil";
}

/*
 * Return the file begin string.
 */
char	       *
gio_file_begin_string()
{
	return "(";
}

/*
 * Return the file end string.
 */
char	       *
gio_file_end_string()
{
	return ")";
}

/*
 * Get a boolean from the input file.  Returns TRUE if successful.
 */
int
#ifdef __STDC__
gio_get_boolean(int *b)
#else
gio_get_boolean(b)
	int	       *b;
#endif
{
	char	       *p = get_token();

	if (strcmp(p, gio_true_string()) == 0)
	{
		*b = TRUE;
		return TRUE;
	}
	if (strcmp(p, gio_false_string()) == 0)
	{
		*b = FALSE;
		return TRUE;
	}
	return FALSE;
}

/*
 * Return whether we are at end-of-file.
 */
int
gio_get_eof()
{
	int		ch;

	while (isspace(ch = fgetc(Inp)))
		;
	if (ch == EOF)
		return TRUE;
	ungetc(ch, Inp);
	return FALSE;
}

/*
 * Get the start of the file.  Returns TRUE if successful.
 */
int
gio_get_file_begin()
{
	int		ch;

	while (isspace(ch = fgetc(Inp)))
		;
	if (ch == *gio_file_begin_string())
		return TRUE;
	ungetc(ch, Inp);
	return FALSE;
}

/*
 * Get the end of the file.  Returns TRUE if successful.
 */
int
gio_get_file_end()
{
	int		ch;

	while (isspace(ch = fgetc(Inp)))
		;
	if (ch == *gio_file_end_string())
		return TRUE;
	ungetc(ch, Inp);
	return FALSE;
}

/*
 * Get a handler from the input file.  Returns TRUE if successful.
 * Sets the string pointer to a buffer allocated with malloc if the string
 * is not empty, otherwise NULL.  Handler could be a quoted string
 * for special language cases (PostScript, Lisp, etc...) or just
 * a simple name for C.
 */
int
#ifdef __STDC__
gio_get_handler(char **s)
#else
gio_get_handler(s)
	char	      **s;
#endif
{
#define	INC		32
	int		ch;
	int		lth;			/* string length */
	int		c = 0;			/* string count */

	while (isspace(ch = fgetc(Inp)))
		;
	ungetc(ch, Inp);

	if (ch != *gio_string_begin_string())
		return gio_get_name(s);
	else
	{
		*s = malloc(lth = INC);
		*s[c++] = fgetc(Inp);
		while (((ch = fgetc(Inp)) != EOF) &&
		       (ch != *gio_string_end_string()))
		{
			if (c + 2 == lth)
				*s = realloc(*s, lth += INC);
			if (ch == '\\')
				ch = fgetc(Inp);
			(*s)[c++] = (char) ch;
		}
		(*s)[c] = *gio_string_end_string();
		(*s)[c+1] = '\0';
		return TRUE;
	}
#undef INC
}

/*
 * Get an integer from the input file.  Returns TRUE if successful.
 */
int
#ifdef __STDC__
gio_get_integer(int *i)
#else
gio_get_integer(i)
	int	       *i;
#endif
{
	char	       *p = get_token();
	char	       *q;
	long		l = strtol(p, &q, 10);

	if (p != q)
	{
		*i = (int) l;
		return TRUE;
	}
	return FALSE;
}

/*
 * Get a keyword from the input file.  Returns TRUE if successful.
 * The keyword is returned in a static buffer.
 */
int
#ifdef __STDC__
gio_get_keyword(char **s)
#else
gio_get_keyword(s)
	char	       **s;
#endif
{
	*s = get_token();
	return TRUE;
}

/*
 * Get a list as a string from the input file.  Returns TRUE if successful.
 * Sets the string pointer to a buffer allocated with malloc if the string
 * is not empty, otherwise NULL.
 */
int
#ifdef __STDC__
gio_get_list(char **s)
#else
gio_get_list(s)
	char	      **s;
#endif
{
#define	INC		32
	int		ch;
	int		lth;			/* string length */
	int		c;			/* string count */

	while (isspace(ch = fgetc(Inp)))
		;
	if (ch != *gio_list_begin_string())
	{
		ungetc(ch, Inp);
		return FALSE;
	}

	if (gio_get_list_end())
	{
		*s = NULL;
		return TRUE;
	}

	*s = malloc(lth = INC);
	for (c = 0; (ch = fgetc(Inp)) != *gio_list_end_string(); c++)
	{
		if (c + 1 == lth)
			*s = realloc(*s, lth += INC);
		if (ch == '\\')
			ch = fgetc(Inp);
		(*s)[c] = (char) ch;
	}
	(*s)[c] = '\0';
	return TRUE;
#undef INC
}

/*
 * Get the start of a list from the input file.  Returns TRUE if successful.
 */
int
gio_get_list_begin()
{
	int		ch;

	while (isspace(ch = fgetc(Inp)))
		;
	if (ch == *gio_list_begin_string())
		return TRUE;
	ungetc(ch, Inp);
	return FALSE;
}

/*
 * Get the end of a list from the input file.  Returns TRUE if successful.
 */
int
gio_get_list_end()
{
	int		ch;

	while (isspace(ch = fgetc(Inp)))
		;
	if (ch == *gio_list_end_string())
		return TRUE;
	ungetc(ch, Inp);
	return FALSE;
}

/*
 * Get a symbol from the input file.  Returns TRUE if successful.
 * Sets the string pointer to a buffer allocated with malloc if the string
 * is not empty, otherwise NULL.
 */
int
#ifdef __STDC__
gio_get_name(char **s)
#else
gio_get_name(s)
	char	      **s;
#endif
{
	char	       *p = get_token();

	if (strcmp(p, gio_false_string()) == 0)
		*s = NULL;
	else {
		*s = malloc(strlen(p) + 1);
		strcpy(*s, p);
	}
	return TRUE;
}


/*
 * Get the full name of an object in a gil file.
 * The syntax of a full name is:
 * 	(name)		when the object is top-level;
 * 	(parent name)	when the object is not top-level;
 *	(parent name "item")	when the object is a setting or
 *				menu item.
 */
int
#ifdef __STDC__
gio_get_full_name(char **parent, char **name, char **item)
#else
gio_get_full_name(parent, name, item)
	char	**parent;
	char	**name;
	char	**item;
#endif
{
	char	*tmp1, *tmp2;

	*parent = NULL;
	*name = NULL;
	*item = NULL;

	if (!gio_get_list_begin())
		return FALSE;

	if (!gio_get_name(&tmp1))
		return FALSE;

	if (gio_get_list_end())
	{
		*name = (char *)malloc(strlen(tmp1)+1);
		strcpy(*name, tmp1);
		return TRUE;
	}

	if (gio_get_string(&tmp2))
	{
		*name = (char *)malloc(strlen(tmp1)+1);
		*item = (char *)malloc(strlen(tmp2)+1);
		strcpy(*name, tmp1);
		strcpy(*item, tmp2);
		if (!gio_get_list_end())
			return FALSE;
		return TRUE;
	}
	
	*parent = (char *)malloc(strlen(tmp1)+1);
	strcpy(*parent, tmp1);
	free(tmp1);
	if (gio_get_name(&tmp1))
	{
		*name = (char *)malloc(strlen(tmp1)+1);
		if (!gio_get_list_end())
			if (gio_get_string(&tmp2))
			{
				*item = (char *)malloc(strlen(tmp2)+1);
				strcpy(*name, tmp1);
				strcpy(*item, tmp2);
				if (gio_get_list_end())
					return TRUE;
				return FALSE;
			}
		strcpy(*name, tmp1);
		return TRUE;
	}
	return FALSE;
}


/*
 * Get the full name of an object in a project file.
 * The syntax of a full name is:
 * 	interface:name		when the object is top-level;
 * 	interface:parent:name	when the object is not top-level;
 *	interface:parent:name:"item"	when the object is a setting or
 *				menu item.
 */
int
#ifdef __STDC__
gio_get_proj_full_name(char **interface, char **parent,
		       char **name, char **item)
#else
gio_get_proj_full_name(interface, parent, name, item)
	char	**interface;
	char	**parent;
	char	**name;
	char	**item;
#endif
{
	char	*tmp1, *tmp2;

	*interface = NULL;
	*parent = NULL;
	*name = NULL;
	*item = NULL;

	if (!gio_get_list_begin())
		return FALSE;

	if (!gio_get_name(&tmp1))
		return FALSE;
	*interface = (char *)malloc(strlen(tmp1)+1);
	strcpy(*interface, tmp1);
	free(tmp1);

	if (!gio_get_name(&tmp1))
		return FALSE;

	if (gio_get_list_end())
	{
		*name = (char *)malloc(strlen(tmp1)+1);
		strcpy(*name, tmp1);
		return TRUE;
	}

	if (gio_get_string(&tmp2))
	{
		*name = (char *)malloc(strlen(tmp1)+1);
		*item = (char *)malloc(strlen(tmp2)+1);
		strcpy(*name, tmp1);
		strcpy(*item, tmp2);
		if (!gio_get_list_end())
			return FALSE;
		return TRUE;
	}
	
	*parent = (char *)malloc(strlen(tmp1)+1);
	strcpy(*parent, tmp1);
	free(tmp1);
	if (gio_get_name(&tmp1))
	{
		*name = (char *)malloc(strlen(tmp1)+1);
		if (!gio_get_list_end())
			if (gio_get_string(&tmp2))
			{
				*item = (char *)malloc(strlen(tmp2)+1);
				strcpy(*name, tmp1);
				strcpy(*item, tmp2);
				if (gio_get_list_end())
					return TRUE;
				return FALSE;
			}
		strcpy(*name, tmp1);
		return TRUE;
	}
	return FALSE;

}


/*
 * Get the start of an object from the input file.  Returns TRUE if successful.
 */
int
gio_get_object_begin()
{
	int		ch;

	while (isspace(ch = fgetc(Inp)))
		;
	if (ch == *gio_object_begin_string())
		return TRUE;
	ungetc(ch, Inp);
	return FALSE;
}

/*
 * Get the end of an object from the input file.  Returns TRUE if successful.
 */
int
gio_get_object_end()
{
	int		ch;

	while (isspace(ch = fgetc(Inp)))
		;
	if (ch == *gio_object_end_string())
		return TRUE;
	ungetc(ch, Inp);
	return FALSE;
}

/*
 * Get a string from the input file.  Returns TRUE if successful.
 * Sets the string pointer to a buffer allocated with malloc if the string
 * is not empty, otherwise NULL.
 */
int
#ifdef __STDC__
gio_get_string(char **s)
#else
gio_get_string(s)
	char	      **s;
#endif
{
#define	INC		32
	int		ch;
	int		lth;	/* string length */
	int		c = 0;	/* string count */

	while (isspace(ch = fgetc(Inp)))
		;
	if (ch != *gio_string_begin_string())
	{
		ungetc(ch, Inp);
		return FALSE;
	}

	if (gio_get_string_end())
	{
		*s = NULL;
		return TRUE;
	}

	*s = malloc(lth = INC);
	while (((ch = fgetc(Inp)) != EOF) && (ch != *gio_string_end_string()))
	{
		if (c + 1 == lth)
			*s = realloc(*s, lth += INC);
		if (ch == '\\')
			ch = fgetc(Inp);
		(*s)[c++] = (char) ch;
	}
	(*s)[c] = '\0';
	return TRUE;
#undef INC
}

/*
 * Get the start of a string from the input file.  Returns TRUE if successful.
 */
int
gio_get_string_begin()
{
	int		ch;

	while (isspace(ch = fgetc(Inp)))
		;
	if (ch == *gio_string_begin_string())
		return TRUE;
	ungetc(ch, Inp);
	return FALSE;
}

/*
 * Get the end of a string from the input file.  Returns TRUE if successful.
 */
int
gio_get_string_end()
{
	int		ch;

	if ((ch = fgetc(Inp)) == *gio_string_end_string())
		return TRUE;
	ungetc(ch, Inp);
	return FALSE;
}

/*
 * Write an integer to the output file.
 */
char	       *
#ifdef __STDC__
gio_integer_string(int i)
#else
gio_integer_string(i)
	int             i;
#endif
{
	sprintf(Buf, "%d", i);
	return Buf;
}

/*
 * Return the string representation of a keyword.
 */
char    *
#ifdef __STDC__
gio_keyword_string(char *s)
#else
gio_keyword_string(s)
	char	       *s;
#endif
{
	return s;
}

/*
 * Return the list begin string.
 */
char	       *
gio_list_begin_string()
{
	return "(";
}

/*
 * Return the list end string.
 */
char	       *
gio_list_end_string()
{
	return ")";
}

/*
 * Return the string representation of a name.
 */
char	       *
#ifdef __STDC__
gio_name_string(char *s)
#else
gio_name_string(s)
	char	       *s;
#endif
{
	return s ? s : gio_false_string();
}

/*
 * Return the object begin string.
 */
char	       *
gio_object_begin_string()
{
	return "(";
}

/*
 * Return the object end string.
 */
char	       *
gio_object_end_string()
{
	return ")";
}

/*
 * Open an input file.  Returns NULL if successful, otherwise an error
 * message.
 */
char           *
#ifdef __STDC__
gio_open_gil_input(char *name)
#else
gio_open_gil_input(name)
	char           *name;
#endif
{
	int	v;
	char	*errmsg;

	/*
	 * If the input file exists and is the correct version, open it
	 * and skip leading comments.
	 */
	if (Inp = fopen(name, "r"))
	{
		v = gil_version(Inp);
		if ((v != 0) && (v <= G_VERSION))
		{
			skip_comments(Inp);
			return NULL;
		}
		else
			errmsg = "unrecognized file format"; /* No dgettext() wrapper on purpose */
	}
	else
		errmsg = sys_errlist[errno]; 

	/*
	 * Otherwise, return an error message.
	 */
	sprintf(Buf, "%s: %s", name, errmsg);

	return Buf;
}

/*
 * Open an output file.  Returns NULL if successful, otherwise an error
 * message.
 */
char           *
#ifdef __STDC__
gio_open_gil_output(char *outfile)
#else
gio_open_gil_output(outfile)
	char           *outfile;
#endif
{
	struct stat	statbuf;
	int		version_written = FALSE;

	/* 
	 * Look for the specified output file.
	 */
	if (stat(outfile, &statbuf) != OK)
	{
		/* Output file does not exits.  Open a new one.
		 */
		if (Outp = fopen(outfile, "w"))
		{
			/* First line is the version number.
			 */
			fprintf(Outp, "%s%d\n", G_VERSION_PREFIX, G_VERSION);
			return NULL;
		}
		else
			goto ERROR_EXIT;
	}
	/*
	 * The output file exists.  Make sure we can successfully open it 
	 * before backing it up.
	 */
	if (Outp = fopen(outfile, "a"))
	{
		char		backup[MAXPATHLEN];
		FILE		*bakp = NULL;
		char		*comment = gio_comment_string();
		int		len = strlen(comment);

		/*
		 * Rename existing file to a backup and start a new output file.
		 */
		fclose(Outp);
		sprintf(backup, "%s.BAK", outfile);
		if ((rename(outfile, backup) != OK) ||
		    !(bakp = fopen(backup, "r")) ||
		    !(Outp = fopen(outfile, "w")))
			goto ERROR_EXIT; /* file access error */

		/* 
		 * Ignore any unrelated leading text until first comment (eg.
		 * garbage from mailtool).
		 */
		while (fgets(Buf, sizeof (Buf), bakp) &&
		       strncmp(Buf, comment, len) != 0)
			;
		/*
		 * Preserve any comments.
		 */
		do
		{
			if (!version_written)
			{
				/* First comment is always version number.
				 */
				fprintf(Outp, "%s%d\n",
					G_VERSION_PREFIX,
					G_VERSION);
				version_written = TRUE;
			}
			else
				fputs(Buf, Outp);
		}
		while (fgets(Buf, sizeof (Buf), bakp) &&
		       strncmp(Buf, comment, len) == 0);

		fclose(bakp);
		return NULL;
	}

 ERROR_EXIT:
	/*
	 * Return a message if unsuccessful.
	 */
	sprintf(Buf, "%s: %s", outfile, sys_errlist[errno]);
	return Buf;
}


/*
 * Open an input proj file.  Returns NULL if successful, otherwise an error
 * message.
 */ 
char	*
#ifdef __STDC__
gio_open_proj_input(char *name)
#else
gio_open_proj_input(name)
	char	*name;
#endif
{
	return gio_open_gil_input(name);
}


/*
 * Open an output proj file.  Returns NULL if successful, otherwise an error
 * message.
 */
char	*
#ifdef __STDC__
gio_open_proj_output(char *name)
#else
gio_open_proj_output(name)
	char	*name;
#endif
{
	return gio_open_gil_output(name);
}


/*
 * Open an input resource file.	Returns NULL is successful, otherwise
 * an error message.
 */
char	*
#ifdef __STDC__
gio_open_resfile_input(char *name)
#else
gio_open_resfile_input(name)
	char	*name;
#endif
{
	return gio_open_gil_input(name);
}

/*
 * Open an output file.  Returns NULL if successful, otherwise an error
 * message.
 */
char           *
#ifdef __STDC__
gio_open_output(char *name)
#else
gio_open_output(name)
	char           *name;
#endif
{
	if (Outp = fopen(name, "w"))
		return NULL;

	sprintf(Buf, "%s: %s", name, sys_errlist[errno]);
	return Buf;
}

/*
 * fprintf to the current output file.
 */
#ifdef __STDC__
void
gio_printf(char *fmt, ...)
#else
void
gio_printf(fmt, va_alist)
	char	*fmt;
	va_dcl
#endif
{
	va_list		args;

	if (Newline)
		fputs(Indent, Outp);

#ifdef __STDC__
	va_start(args, fmt);
#else
	va_start(args);
#endif
	Newline = fmt[strlen(fmt) - 1] == '\n';
	vfprintf(Outp, fmt, args);
	va_end(args);
}

/*
 * Write a character to the output file, preceeded by the indent string if we
 * are on a new line.
 */
void
#ifdef __STDC__
gio_putc(char c)
#else
gio_putc(c)
	char            c;
#endif
{
	if (Newline)
		fputs(Indent, Outp);
	fputc(c, Outp);
	Newline = c == '\n';
}

/*
 * Write characters to the output file, preceeded by the indent string if we
 * are on a new line.
 */
void
#ifdef __STDC__
gio_puts(char *s)
#else
gio_puts(s)
	char           *s;
#endif
{
	if (Newline)
		fputs(Indent, Outp);
	fputs(s, Outp);
	Newline = s[strlen(s) - 1] == '\n';
}

/*
 * Write a boolean to the output file.
 */
void
#ifdef __STDC__
gio_put_boolean(int b)
#else
gio_put_boolean(b)
	int             b;
#endif
{
	gio_puts(b ? gio_true_string() : gio_false_string());
}

/*
 * Write an integer to the output file.
 */
void
#ifdef __STDC__
gio_put_float(double d)
#else
gio_put_float(d)
	double	d;
#endif
{
	sprintf(Buf, "%f", d);
	gio_puts(Buf);
}

/*
 * Write an integer to the output file.
 */
void
#ifdef __STDC__
gio_put_integer(int i)
#else
gio_put_integer(i)
	int             i;
#endif
{
	sprintf(Buf, "%d", i);
	gio_puts(Buf);
}

/*
 * Write a keyword to the output file.
 */
void
#ifdef __STDC__
gio_put_keyword(char *s)
#else
gio_put_keyword(s)
	char		*s;
#endif
{
	gio_puts(s);
}

/*
 * Write a handler to the output file.
 */
void
#ifdef __STDC__
gio_put_handler(char *s)
#else
gio_put_handler(s)
	char	       *s;
#endif
{
	char	*newstr;
	char	*p;

	if (s && (s[0] == *gio_string_begin_string()) &&
	    (s[strlen(s)-1] == *gio_string_end_string())) {
		newstr = malloc(strlen(s) + 1);
		strcpy(newstr, s);
		newstr[strlen(s)-1] = '\0';

		fputc(*gio_string_begin_string(), Outp);
		for (p = newstr+1; *p; p++) {
			if ((*p == *gio_string_end_string()) || (*p == '\\'))
				fputc('\\', Outp);
			fputc(*p, Outp);
		}
		fputc(*gio_string_end_string(), Outp);
		free(newstr);
	} else
		gio_put_name(s);
}

/*
 * Write a name list to the output file.
 */
void
#ifdef __STDC__
gio_put_name(char *s)
#else
gio_put_name(s)
	char	       *s;
#endif
{
	gio_puts(s ? s : gio_false_string());
}

/*
 * Write a full name to the output file.
 */
void
#ifdef __STDC__
gio_put_full_name(char *parent, char *name, char *item)
#else
gio_put_full_name(parent, name, item)
	char		*parent;
	char		*name;
	char		*item;
#endif
{
	gio_puts(gio_list_begin_string());
	if (parent)
	{
		gio_puts(parent);
		gio_putc(' ');
	}
	if (name)
	{
		gio_puts(name);
	}
	if (item)
	{
		gio_putc(' ');
		gio_put_string(item);
	}
	gio_puts(gio_list_end_string());
	gio_putc('\n');
}

/*
 * Write a full name (the project form) to the output file.
 */
void
#ifdef __STDC__
gio_put_proj_full_name(char *interface, char *parent, char *name, char *item)
#else
gio_put_proj_full_name(interface, parent, name, item)
	char		*interface;
	char		*parent;
	char		*name;
	char		*item;
#endif
{
	gio_puts(gio_list_begin_string());

	if (interface)
	 {
		gio_puts(interface);
		gio_putc(' ');
	}
	if (parent)
	{
		gio_puts(parent);
		gio_putc(' ');
	}
	if (name)
	{
		gio_puts(name);
	}
	if (item)
	{
		gio_putc(' ');
		gio_put_string(item);
	}

	gio_puts(gio_list_end_string());
	gio_putc('\n');
}


/*
 * Write a string to the output file.
 */
void
#ifdef __STDC__
gio_put_string(char *s)
#else
gio_put_string(s)
	char           *s;
#endif
{
	gio_puts(gio_string_begin_string());
	gio_put_string_to_file(s);
	gio_puts(gio_string_end_string());
}

/*
 * Write a string to the output file.
 */
void
#ifdef __STDC__
gio_put_string_to_file(char *s)
#else
gio_put_string_to_file(s)
	char           *s;
#endif
{
	register char	*p;

	if (s)
		for (p = s; *p; p++) {
			if (*p == *gio_string_end_string() ||
			    *p == '\\')
				fputc('\\', Outp);
			fputc(*p, Outp);
		}
}

/*
 * Set the current indent level of the output file.
 */
void
#ifdef __STDC__
gio_set_indent(int level)
#else
gio_set_indent(level)
	int             level;
#endif
{
	register int    i;

	for (i = 0; i != level; i++)
		Indent[i] = '\t';

	Indent[i] = '\0';
	Indent_level = level;
}

/*
 * Get the current indent level of the output file.
 */
int
gio_get_indent()
{
	return Indent_level;
}

/*
 * Return the string begin string.
 */
char	       *
gio_string_begin_string()
{
	return "\"";
}

/*
 * Return the string end string.
 */
char	       *
gio_string_end_string()
{
	return "\"";
}

/*
 * Return the string representation of a string.  Truncates the string if
 * it is too long.
 */
char    *
#ifdef __STDC__
gio_string_string(char *s)
#else
gio_string_string(s)
	char           *s;
#endif
{
	char	       *p;
	int		i = 0;

	Buf[i++] = *gio_string_begin_string();
	if (s)
		for (p = s; *p && i < MAXPATHLEN - 4; p++)
		{
			if (*p == *gio_string_end_string())
				Buf[i++] = '\\';
			Buf[i++] = *p;
		}
	Buf[i++] = *gio_string_end_string();
	Buf[i] = '\0';
	return Buf;
}

/*
 * Return the boolean true string.
 */
char	       *
gio_true_string()
{
	return "t";
}

/*
 * Internal utility functions.
 */

/*
 * Get the next token from the input stream.  It is assumed that we
 * are not at end-of-file.
 */
static char *
get_token()
{
	int		ch;
	int		c = 0;			/* character count */

	while (isspace(ch = fgetc(Inp)))
		;
	do {
		if (c < sizeof (Buf) - 1)
			Buf[c++] = (char) ch;
	} while (!isspace(ch = fgetc(Inp)) && ch != ')');

	ungetc(ch, Inp);
	Buf[c] = '\0';
	return Buf;
}

/*
 * Return whether a file contains a GIL file.  Returns non-0 version
 * number if true, otherwise FALSE.  Leaves file positioned at the
 * beginning of first comment (should be version number).
 */
static int
#ifdef __STDC__
gil_version(FILE *fp)
#else
gil_version(fp)
	FILE	       *fp;
#endif
{
	char	*ascii_version_num;
	char	*tmp;
	long	int_version_num;
	long	first_char;
	int	version = 0;
	int	len = strlen(G_VERSION_PREFIX);

	rewind(fp);
	while(fgets(Buf, sizeof (Buf), fp))
	{
		/* Ignore lines until a GIL prefix is found.
		 */
		if (strncmp(Buf, G_VERSION_PREFIX, len) == 0)
		{
			/* Prefix matched.  Point to the version number and
			 * convert it to an integer.
			 */
			first_char = ftell(fp);
			Buf[strlen(Buf)-1] = '\0';
			ascii_version_num = Buf + len;
			int_version_num = strtol(ascii_version_num, &tmp, 10);

			if(ascii_version_num != tmp)
				version = (int) int_version_num;
			fseek(fp, first_char, 0);
			break;
		}
	}
	return version;
}

/*
 * Skip leading comments in a GIL file.
 */
static void
#ifdef __STDC__
skip_comments(FILE *fp)
#else
skip_comments(fp)
	FILE	       *fp;
#endif
{
	long		pos;

	for (;;) {
		pos = ftell(fp);
		if (!fgets(Buf, sizeof (Buf), fp) ||
		    Buf[0] != *gio_comment_string())
			break;
	}
	fseek(fp, pos, 0);
}
