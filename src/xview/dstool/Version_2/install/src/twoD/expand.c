/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#include <stdio.h>
#include <pwd.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif


#include <constants.h>

#define TRUE 1
#define FALSE 0
#define NULL 0

/*
 * expand_filename()
 * Convert NAME to absolute path, and canonicalize it.
 * Second arg DEFAULT is directory to start with if NAME is relative
 * (does not start with slash); if DEFAULT is nil or missing,
 * the present working directory is used.
 * Filenames containing . or .. as components are simplified;
 * initial ~ is expanded.  See also the function sub_in_filename which expands
 * environmental variables.
 * Adapted from GNU EMACS (fileio.c) by fjw 8/20/92
 */
int
expand_filename(name, defalt)
char *name, *defalt;
{
  char *nm,*getcwd();
  
  register char *newdir, *p, *o;
  int 		tlen, relative;
  char 		*target;
  struct passwd *pw;
  char          *filename_as_directory();
  nm = name;
  
  /* If nm is absolute ( ie doesn't contain /./ or /../ ) we can return right away. */
  /* Check for and eliminate //  */
  if ( nm[0] == '/' )
    {
      p = nm;
      relative = FALSE;
      while (*p)		/* check for relative paths */
	{
          if (p[0] == '/' && p[1] == '/' ) /* forget everything before here */
            nm = p + 1;
          if (p[0] == '/' && p[1] == '~')  /* ditto, but relative path */
            nm = p + 1, relative = TRUE;
	  if (p[0] == '/' && p[1] == '.'   /* relative paths */
	      && (p[2] == '/' || p[2] == 0
		  || (p[2] == '.' && (p[3] == '/' || p[3] == 0))))
	    relative = TRUE;
	  p++;
	}
      if (!relative)
	{
	  strcpy( name, nm );
	  return ( 0 );		/* name is absolute the way it is; nothing to expand */
	}
    }

  /* Now determine directory to start with and put it in NEWDIR.  */

  newdir = NULL;

  if (nm[0] == '~')
    {
      if (nm[1] == '/' || nm[1] == 0)
	{
	  newdir = (char *) getenv ("HOME"); /* Handle ~ on its own.  */
	}
      else
	{
	  char *user = nm + 1;				  /* Handle ~ followed by user name.  */

#ifdef HAS_STRCHR
	  char *ptr = (char *) strchr (user, '/');   /* Find end of name.  */
#endif

#ifdef HAS_INDEX
	  char *ptr = (char *) index (user, '/');   /* Find end of name.  */
#endif
	  /* char *ptr = (char *) index (user, '/');	*/
	  int len = ptr ? ptr - user : strlen (user);
	  o = (char *) calloc(len + 1, sizeof(char));	  /* Copy the user name into temp storage.  */
	  strncpy(o, user, len);
	  o[len] = 0;

	  pw = (struct passwd *) getpwnam (o);	  /* Look up the user name.  */
	  if (!pw)
	    {
	      fprintf(stderr,"User \"%s\" is not known", o);
	      cfree(o);
	      return ( -1 );
	    }
	  cfree(o);
	  newdir = (char *) pw->pw_dir;
	  nm += len;		/* Discard the user name from NM.  */
	}      
      nm++;			/* Discard the ~ from NM.  */
      if (newdir == NULL)
	newdir = (char *) "";
    }

  if (nm[0] != '/' && !newdir)
    {
      if (!defalt)
	/*	getwd(defalt);*/
	getcwd(defalt, SIZE_OF_DIR_PLUS_FNAME);
      newdir = defalt;
    }

  /* Now concatenate the directory and name to new space */

  tlen = (newdir ? strlen (newdir) + 1 : 0) + strlen (nm) + 1;
  target = (char *) calloc (tlen, sizeof(char));
  *target = 0;

  if (newdir)
    {
      filename_as_directory (target, newdir);
    }

  if( *nm == '/' && target[strlen(target)-1] == '/' ) /* make sure that appending is correct */
    directory_filename(target);
  if( *nm != '/' && target[strlen(target)-1] != '/' )
    filename_as_directory (target, newdir);
  strcat (target, nm);

  /* Now canonicalize by removing /. and /foo/.. if they appear */

  p = target;
  o = target;

  while (*p)
    {
      if (*p != '/')			   /* copy most things */
 	{
	  *o++ = *p++;
	}
      else if (!strncmp (p, "//", 2) )	   /* reset if double slash */
	{
	  o = target;
	  p++;
	}
      else if (p[0] == '/' && p[1] == '.' && /* skip over /. and don't add to target */
	       (p[2] == '/' || p[2] == 0))
	p += 2;
      else if (!strncmp (p, "/..", 3)	   /* go up one level   */
	       && o != target
	       && (p[3] == '/' || p[3] == 0))
	{
	  while (o != target && *--o != '/') /* back up to previous / */
	    ;
	  if (o == target && *o == '/')
	    ++o;
	  p += 3;
	}
      else				   /* else just ordinary foo/foobar */
 	{
	  *o++ = *p++;
	}
    }

  strcpy(name,target);
  name[strlen(target)-strlen(o)] = 0;
  cfree(target);

  return( 0 );
}

/* 
 * filename_as_directory()
 * Return a string representing file FILENAME interpreted as a directory.
 * For a Unix-syntax file name, just appends a slash if needed.
 * Adapted from GNU EMACS by fjw 8/20/92
 */
char *
filename_as_directory (out, in)
     char *out, *in;
{
  int size = strlen (in) - 1;

  strcpy (out, in);
  /* For Unix syntax, Append a slash if necessary */
  if (out[size] != '/')
    strcat (out, "/");
  return out;
}


/*
 * directory_filename()
 * Returns the file name of the directory named DIR.
 * This is the name of the file that holds the data for the directory DIR.
 * In Unix-syntax, this just removes the final slash.
 * Value is nonzero if the string output is different from the input.
 * Adapted from GNU EMACS by fjw 8/20/92
 */

directory_filename (dir)
     char *dir;
{
  long slen;

  slen = strlen (dir) - 1;
  /* Process as Unix format: just remove any final slash. */
  if (dir[slen] == '/' && slen > 1)
    {
      dir[slen] = 0;
      return 1;
    }
  return 0;
}


/* file-name-absolute
       "Return TRUE if file FILENAME specifies an absolute path name.")
*/
filename_absolute(filename)
char *filename;
{
  if (*filename == '/' || *filename == '~' )
    return 1;
  else
    return 0;
}


