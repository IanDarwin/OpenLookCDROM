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

#define TRUE 1
#define FALSE 0
#define NULL 0

/*
 * sub_in_filename()
 * Substitute environment variables referred to in STRING.
 * A $ begins a request to substitute; the env variable name is the alphanumeric
 * characters and underscores after the $, or is surrounded by braces or paren.
 * If a ~ appears following a /, everything through that / is discarded.
 * Algorithm adapted from GNU EMACS (fileio.c) by fjw 8/21/92
 */
sub_in_filename(string)
char *string;
{
  char *nm;

  register char *s, *p, *o, *x, *endp;
  char *target;
  int total = 0;
  int need_sub = FALSE;
  char *xnm;

  nm = string;
  endp = nm + strlen(string);

  /* If /~ or // appears, discard everything through first slash. */

  for (p = nm; p != endp; p++)
    {
      if ((p[0] == '~' || p[0] == '/') && p != nm && p[-1] == '/')
	{
	  nm = p;
	  need_sub = TRUE;
	}
    }

  /* See if any variables are substituted into the string
     and find the total length of their values in `total' */

  for (p = nm; p != endp;   )
    if (*p != '$')		/* increment until "$" */
      p++;
    else			/* if more than one "$" exists, this'll take last one */
      {
	p++;
	if (p == endp)
	  goto badsubst;
	else if (*p == '{')	/* find matching brace; o marks beginning of variable... */
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose;
	    s = p;		/* ...and s is the end. */
	  }
	else if (*p == '(')	/* do same for parentheses fjw */
	  {
	    o = ++p;
	    while (p != endp && *p != ')') p++;
	    if (*p != ')') goto missingclose;
	    s = p;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) 
	      p++;
	    s = p;
	  }

	target = (char *) calloc(s - o + 1, sizeof(char));	/* Copy out variable name... */
	strncpy (target, o, s - o);
	target[s - o] = 0;

	o = (char *) getenv (target);	/* ...and grab the variable value... */
	if (!o) goto badvar;
	total += strlen (o);	/* ...just so we can figure out how much space we need! */
	cfree(target);
	cfree(o);
	need_sub = TRUE;
      }

  if (!need_sub)		/* original string unchanged if no substitutions needed */
    return 0;

  /* If substitution required, recopy the string and do it */
  /* Make space for the new copy */
  xnm = (char *) calloc(strlen(string) + total + 1, sizeof(char));
  x = xnm;

  /* Copy the rest of the name through, replacing $ constructs with values */
  for (p = nm; *p;   )
    if (*p != '$')
      *x++ = *p++;
    else
      {
	p++;
	if (p == endp)
	  goto badsubst;	/* don't really need this b/c weeded it out before! */
	else if (*p == '{')	
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose; /* Same for this one... */
	    s = p++;
	  }
	else if (*p == '(')
	  {
	    o = ++p;
	    while (p != endp && *p != ')') p++;
	    if (*p != ')') goto missingclose; /* ... ditto */
	    s = p++;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) p++;
	    s = p;
	  }

	/* Do all this again b/c we're expanding ALL env variables. Copy out the variable name */
	target = (char *) calloc(s - o + 1, sizeof(char));
	strncpy (target, o, s - o);
	target[s - o] = 0;

	o = (char *) getenv (target);	/* and get variable value */
	cfree(target);
	if (!o)			/* yeah, I know, but I'm paranoid about these things. */
	  goto badvar;		/* What if someone setenvs between first and next check? */
	cfree(target);
	cfree(o);
	strcpy (x, o);
	x += strlen (o);
      }

  *x = 0;

  /* If /~ or // appears due to expansion, discard everything through first slash. */

  for (p = xnm; p != x; p++)
    if ((p[0] == '~' ||	 p[0] == '/') && p != nm && p[-1] == '/')
      xnm = p;

  strncpy(string, xnm, x-xnm);
  string[x-xnm]=0;
  cfree(xnm);
  
  return 0;

 badsubst:
  fprintf(stderr,"Bad format environment-variable substitution\n");
 missingclose:
  fprintf(stderr,"Missing \"}\" in environment-variable substitution\n");
 badvar:
  fprintf(stderr,"Substituting nonexistent environment variable \"%s\"\n", target);
  return 1;
}
