/* -*-C-*-
********************************************************************************
*
* File:         utils.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/utils.c,v 2.9 1994/06/06 15:41:03 npm Exp $
* Description:  random, non-X utility functions needed by WINTERP
* Author:       Niels Mayer
* Created:      Thu Oct 25 22:37:08 1990
* Modified:     Sun Jun  5 14:30:57 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/utils.c,v 2.9 1994/06/06 15:41:03 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

/*
 * <limits.h> defines machine dependent limits on sizes of numbers, if your
 * machine doesn't have this, then your compiler doesn't conform to standards
 * XPG2, XPG3, POSIX.1, FIPS 151-1 and you should complain to the manufacturer.
 * 
 * If for some reason your system isn't standards-conforming, you may work
 * around this problem by using the following definitions (assuming 32 bit machine):
 * 
 * #define USHRT_MAX 65535
 * #define SHRT_MIN (-32768)
 * #define SHRT_MAX 32767
 */
#include <limits.h>

#include <stdio.h>
#include <stdlib.h>		/* for getenv(), etc */
#include <fcntl.h>		/* needed for fcntl(2) calls */
#include <Xm/Xm.h>
#include "winterp.h"

LVAL k_NOVERBOSE;

/******************************************************************************/
Dimension Get_Dimension_Argument()
{
  LVAL lval_num;
  long num;

  lval_num = xlgafixnum();
  num = (long) getfixnum(lval_num);
  if (num < 0L)
    xlerror("FIXNUM (Dimension) value must be >= 0.", lval_num);
  if (num > (long) USHRT_MAX)
    xlerror("FIXNUM (Dimension) value too large.", lval_num);
  return ((Dimension) num);
}


/******************************************************************************/
Position Get_Position_Argument()
{
  LVAL lval_num;
  long num;

  lval_num = xlgafixnum();
  num = (long) getfixnum(lval_num);
  if (num < (long) SHRT_MIN)
    xlerror("FIXNUM (Position) value too small.", lval_num);
  if (num > (long) SHRT_MAX)
    xlerror("FIXNUM (Position) value too large.", lval_num);
  return ((Position) num);
}


/******************************************************************************/
int Get_Int_Argument(min, max)
     long min;
     long max;
{
  LVAL lval_num;
  long num;

  lval_num = xlgafixnum();
  num = (long) getfixnum(lval_num);
  if (num < min)
    xlerror("FIXNUM value too small.", lval_num);
  if (num > max)
    xlerror("FIXNUM value too large.", lval_num);
  return ((int) num);
}


/******************************************************************************
 * This proc opens file <name> using a search path.... it is used to override
 * the ospopen() in xlisp/unixstuf.c such that the search path can be retrieved
 * from a WINTERP resource. ospopen() is used in XLISP by LOAD and RESTORE.
 *
 * This code was once based on Tom Almy's ospopen() from unixstuf.c.
 ******************************************************************************/
FILE *ospopen(name, ascii)
char *name;
int ascii;  /* value not used in UNIX */
{
  /* don't do a thing if user specifies explicit path */
  if ((name[0] == '/') || (name[0] == '.')) {
    return (OSAOPEN(name, "r")); /* from xlisp/unixstuf.c -- open file, setting close-on-exec */
  }
  else {
    FILE *fp = NULL;		/* NPM -- added this initialization... */
    char *path;
    char *newnamep;
    char ch;
    int  namelen = strlen(name); /* NPM--added this to fix potential string overflow bug */
    int  pathlen;		/* NPM--added this to fix potential string overflow bug */
    char newname[FNAMEMAX+1];	/* NPM -- subst FNAMEMAX for Almy's '256' */

    if (!(path = getenv(PATHNAMES))) /* "XLPATH" environment variable supersedes resource lispLoadPath */
      path = user_prefs.lisp_load_path;	/* if no XLPATH, then use resource lispLoadPath... */

    while ((fp == NULL) && (*path != '\0')) { /* while file not found and end of path not reached... */
      pathlen = 0;
      newnamep = newname;
      while (((ch = *path++) != '\0')
	     && (ch != ':')
	     && (ch != ' ')
	     && (++pathlen < FNAMEMAX))	/* NPM: note addition to prevent str overflow */
	*newnamep++ = ch;
      if (pathlen > 0) {	/* NPM: don't do anything on empty path, or if extra ' ' exists... */
	if ((namelen + pathlen + 1) < FNAMEMAX) {
	  if (*(newnamep-1) != '/')
	    *newnamep++ = '/';	/* final path separator needed */
	  *newnamep = '\0';	/* null terminate it */
	  strncpy(newnamep, name, namelen);
	  *(newnamep + namelen) = '\0';	/* null terminate it */
	  fp = fopen(newname, "r");
	}
	else
	  xlfail("File path or file name too long (check resource lispLoadPath or env-var XLPATH).");
      }
    }
    
    /* all paths failed, try lispLibDir, else current directory */
    if (fp == NULL) {		
      int  libdirlen;
      if (libdirlen = strlen(user_prefs.lisp_lib_dir)) { /* if there's a lispLibDir */
	if ((libdirlen + namelen + 1) < FNAMEMAX) { /* and it fits */
	  strncpy(newname, user_prefs.lisp_lib_dir, libdirlen); /* prepend resource 'lispLibDir' */
	  newnamep = &(newname[libdirlen]);
	  if (*(newnamep-1) != '/')
	    *newnamep++ = '/';	/* final path separator needed */
	  *newnamep = '\0';	/* null terminate it */
	  strncpy(newnamep, name, namelen); /* append name */
	  *(newnamep + namelen) = '\0'; /* null terminate it */
	  fp = fopen(newname, "r");
	}
	else 
	  xlfail("File path or file name too long (check resource lispLibDir).");
      }
    }

    if (fp) {			/* lispLoadPath or lispLibDir succeeded */
      fcntl(fileno(fp), F_SETFD, 1); /* set close-on-exec */
      return (fp);
    }
    else			/* lispLoadPath and lispLibDir failed, check current directory just in case */       
      return (OSAOPEN(name, "r")); /* from xlisp/unixstuf.c -- open file, setting close-on-exec */
  }
}


/******************************************************************************
 * xlrdsave - save the last expression returned by the reader.
 * this comes from xlisp/xlisp.c, which isn't linked in with WINTERP
 ******************************************************************************/
void xlrdsave(expr)
  LVAL expr;
{
  extern LVAL s_1plus,s_2plus,s_3plus,s_minus;
  setvalue(s_3plus,getvalue(s_2plus));
  setvalue(s_2plus,getvalue(s_1plus));
  setvalue(s_1plus,getvalue(s_minus));
  setvalue(s_minus,expr);
}


/******************************************************************************
 * xlevsave - save the last expression returned by the evaluator.
 * this comes from xlisp/xlisp.c, which isn't linked in with WINTERP
 ******************************************************************************/
void xlevsave(expr)
  LVAL expr;
{
  extern LVAL s_1star,s_2star,s_3star;
    setvalue(s_3star,getvalue(s_2star));
    setvalue(s_2star,getvalue(s_1star));
    setvalue(s_1star,expr);
}


/******************************************************************************
 *
 ******************************************************************************/
void Winterp_Print_Prompt(newline_p)
     Boolean newline_p;
{
  extern LVAL s_stdout;

  if (user_prefs.enable_init_msgs) {
    if (newline_p)
      xlfreshline(getvalue(s_stdout));
    stdputstr("X> ");		/* use this to separate results of different evaluations */
    fflush(stdout); fflush(stderr); /* otherwise output won't happen while blocked in XtAppNextEvent() */
  }
}


/******************************************************************************
 *
 ******************************************************************************/
void Winterp_Print_Newline()
{
  extern LVAL s_stdout;

  if (user_prefs.enable_init_msgs) {
    xlfreshline(getvalue(s_stdout));
    fflush(stdout); fflush(stderr); /* otherwise output won't happen while blocked in XtAppNextEvent() */
  }
}


/******************************************************************************
 * (REDIRECT_STDERR)
 * 	--> returns an input-stream suitable for reading.
 *
 * The returned input-stream represents the text sent to the unix standard
 * error (stderr), both for the WINTERP application and for any
 * subprocesses invoked by WINTERP. This is typically used to trap error
 * output from subprocesses and pop up a dialog indicating a problem.
 *
 * The stream can be used for reading by existing XLISP/WINTERP reading
 * primitives, e.g. READ-LINE, READ-CHAR, FSCANF-FIXNUM, FSCANF-STRING,
 * FSCANF-FLONUM, etc.
 *
 * The returned input-stream is non-blocking, which means you may read
 * more than one character at a time without worrying about a deadlock.
 * However, note that just because you've been able to read N characters
 * in one read operation, doesn't mean you've read all characters output
 * on STDERR. To retrieve the full contents, you need to keep on reading
 * until you've hit EOF. 
 *
 * Instead of polling the stream (till hitting EOF) to see if more chars
 * have been read, it is better to add an input callback via XT_ADD_INPUT
 * and have the callback code read from the input-stream whenever there
 * are characters to be read.
 ******************************************************************************/
LVAL Prim_REDIRECT_STDERR()
{
  FILE *fp;
  int stderr_pipe[2];

  xllastarg();

  /*
   * Redirect stderr to pipe.  Make it unbuffered, and set NDELAY
   * so later reads don't block.
   */
  (void) pipe(stderr_pipe);
  (void) dup2(stderr_pipe[1], fileno(stderr));
  (void) fcntl(stderr_pipe[0], F_SETFL, O_NDELAY);
  setbuf(stderr, NULL);
  
  if (!(fp = fdopen(stderr_pipe[0], "r")))
    return (NIL);
  setbuf(fp, NULL);
 
  return (cvfile(fp, S_FORREADING));
}


/******************************************************************************
 * (REDIRECT_STDOUT)
 * 	--> returns an input-stream suitable for reading.
 *
 * This is just like REDIRECT_STDERR except that the returned input-stream
 * represents the text sent to the unix standard output (stdout).
 ******************************************************************************/
LVAL Prim_REDIRECT_STDOUT()
{
  FILE *fp;
  int stdout_pipe[2];

  xllastarg();

  /*
   * Redirect stdout to pipe.  Make it unbuffered, and set NDELAY
   * so later reads don't block.
   */
  (void) pipe(stdout_pipe);
  (void) dup2(stdout_pipe[1], fileno(stdout));
  (void) fcntl(stdout_pipe[0], F_SETFL, O_NDELAY);
  setbuf(stdout, NULL);
  
  if (!(fp = fdopen(stdout_pipe[0], "r")))
    return (NIL);
  setbuf(fp, NULL);
 
  return (cvfile(fp, S_FORREADING));
}


/******************************************************************************
 * (READ_EVAL_PRINT <ustream>)
 * 	--> returns T if success, NIL if hit EOF, or FIXNUM if error.
 *
 * This gives programmatic access to the top-level read/eval/print routine.
 * By top-level, I mean that it will act like the top-level R.E.P w/r/t
 * using the global environment and setting up error and breakloop returns.
 ******************************************************************************/
LVAL Prim_READ_EVAL_PRINT()
{
  extern LVAL true;
  LVAL fptr;
  int result;
  
  fptr = xlgetfile(FALSE);
  xllastarg();

  result = Read_From_Stream_Eval_And_Print(fptr);

  if (result == TRUE)
    return (true);		/* return T for success */
  else if (result == FALSE)
    return (NIL);		/* return NIL cuz hit EOF */
  else 
    return (cvfixnum((FIXTYPE) result)); /* error occured, return the CF_* mask value as fixnum... */
}


/******************************************************************************
 * (REQUIRE <module-name>)
 * 	--> returns T if the requested module got loaded from a file.
 *	    returns NIL if the requested module had previously been loaded
 *	    (that is, (PROVIDE <module-name>) had been called).
 *	    If the module couldn't be loaded, will signal an error.
 *
 *	<module-name> is a STRING, specifying the name of the file to load.
 *	<module-name> can be the full path to the file (e.g. "/users/mayer/foo.lsp"),
 *	or a filename plus extension (e.g. "lib-utils/unixstuf.lsp", or "foo.lsp"),
 *	or a filename without extension (e.g. "lib-utils/unixstuf" or "foo").
 *
 * REQUIRE is typically used to load "libraries" in WINTERP. Library files
 * which are to be loaded by REQUIRE should have a (PROVIDE <module-name>)
 * at the end which tells REQUIRE the desired module has been loaded succesfully.
 * REQUIRE will not load a file which has previously been PROVIDED, thus achieving
 * the effect of only loading a given module once, even if a particular module
 * is REQUIRE'd multiple times throughout the files that comprise a given
 * application.
 * 
 * When REQUIRE determines that <module-name> hasn't previously been loaded, it
 * uses the same mechanism and load paths as used by the XLISP LOAD command.
 * First, the Unix environment variable "XLPATH" is consulted; if that isn't
 * set, the X resource Winterp.lispLoadPath is consulted to retrieve a
 * sequence of paths comprised of colon-separated paths (e.g.
 * /users/mayer/:/users/mayer/tmp:/users/mayer/winterp/). If the required
 * module isn't found on that path, the resource Winterp.lispLibDir is
 * consulted to find another directory for the module. Finally, if the required
 * module isn't found anywhere on the aforementioned paths, the current directory
 * is searched for the requested file. If no file is found, an XLISP error is
 * signalled.
 *
 * Note that (REQUIRE "lib-utils/unixstuf.lsp") will load file
 * "unixstuf.lsp" even if (PROVIDE "lib-utils/unixstuf") had
 * been called by a previous successful LOAD or REQUIRE of that file.
 * (REQUIRE "lib-utils/unixstuf.lsp") will load the same file 
 * twice, because "lib-utils/unixstuf.lsp" is not equal to
 * "lib-utils/unixstuf". To avoid confusion on this you should
 * always specify <module-name> for PROVIDE and REQUIRE without
 * the ".lsp" extension  -- "<module-name>.lsp" is the filename that
 * will get loaded by (REQUIRE <module-name>)...
 *
 * The global variable *MODULES* is set to a list of <module-name> which have
 * been previously PROVIDE'd...
 ******************************************************************************/
static LVAL s_MODULES;		/* used by PROVIDE and REQUIRE */
LVAL Prim_REQUIRE()
{
  extern LVAL xlenv, xlfenv, true; /* from XLISP... */
  register LVAL elt;
  LVAL  oldenv, oldfenv;
  LVAL  l_modules 	 = getvalue(s_MODULES);
  LVAL  lval_module_name = xlgastring();
  char* module_name	 = getstring(lval_module_name);
  int   module_name_len  = strlen(module_name);
  xllastarg();
   
  while ((l_modules != NIL)
	 && stringp((elt = car(l_modules)))
	 && !(   (module_name_len == getslength(elt))
	      && (strncmp(module_name, getstring(elt), module_name_len) == 0))
	 )
    l_modules = cdr(l_modules);

  if (l_modules == NIL) {	/* REQUIRE'd module not found on "*modules*" list -- load it! */
    /* protect some pointers */
    xlstkcheck(2);
    xlprotect(oldenv);
    xlprotect(oldfenv);

    /* establish global environment */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlfenv = NIL;

    if (!xlload(module_name, user_prefs.enable_init_msgs, FALSE))
      xlerror("REQUIRE couldn't load file (did you forget to set resource 'lispLibDir' or 'lispLoadPath'?)",
	      lval_module_name);

    /* restore environment */
    xlenv = oldenv;
    xlfenv = oldfenv;

    /* restore the stack */
    xlpopn(2);

    return (true);
  }
  else				/* REQUIRE'd module has already been PROVIDED -- don't load, return NIL  */
    return (NIL);
}


/******************************************************************************
 * (PROVIDE <module-name>)
 * 	--> returns NIL if <module-name> has already been PROVIDE'd;
 *		    T   if <module-name> wasn't previously PROVIDE'd.
 *
 *	<module-name> is a STRING, specifying the name of a module/file that
 *	has been loaded succesfully. <module-name> is typically
 *	a filename plus extension (e.g. "lib-utils/unixstuf.lsp", or "foo.lsp"),
 *	or a filename without extension (e.g. "lib-utils/unixstuf" or "foo").
 *
 * PROVIDE and REQUIRE are typically used to load "libraries" in WINTERP.
 * Library files which are to be loaded by REQUIRE should have a
 * (PROVIDE <module-name>) at the end which tells REQUIRE the desired module
 * has been loaded succesfully. REQUIRE will not load a file which has
 * previously been PROVIDED, thus achieving the effect of only loading a
 * given module once, even if a particular module is REQUIRE'd multiple
 * times throughout the files that comprise a given application.
 *
 * Note that (REQUIRE "lib-utils/unixstuf.lsp") will load file
 * "unixstuf.lsp" even if (PROVIDE "lib-utils/unixstuf") had
 * been called by a previous successful LOAD or REQUIRE of that file
 * (REQUIRE "lib-utils/unixstuf.lsp") will load the same file 
 * twice, because "lib-utils/unixstuf.lsp" is not equal to
 * "lib-utils/unixstuf". To avoid confusion on this you should
 * always specify <module-name> for PROVIDE and REQUIRE without
 * the ".lsp" extension -- "<module-name>.lsp" is the filename that
 * will get loaded by (REQUIRE <module-name>)...
 *
 * The global variable *MODULES* is set to a list of <module-name> which have
 * been previously PROVIDE'd...
 ******************************************************************************/
LVAL Prim_PROVIDE()
{
  extern LVAL true;		/* from XLISP... */
  register LVAL elt;
  LVAL  l_modules 	 = getvalue(s_MODULES);
  LVAL  lval_module_name = xlgastring();
  char* module_name	 = getstring(lval_module_name);
  int   module_name_len  = strlen(module_name);
  xllastarg();
   
  while ((l_modules != NIL)
	 && stringp((elt = car(l_modules)))
	 && !(   (module_name_len == getslength(elt))
	      && (strncmp(module_name, getstring(elt), module_name_len) == 0))
	 )
    l_modules = cdr(l_modules);

  if (l_modules == NIL) {	/* Module not found on "*modules*" list -- add it! */
    xlsave1(l_modules);		/* protect from gc */
    l_modules = getvalue(s_MODULES);
    setvalue(s_MODULES, cons(lval_module_name, l_modules));
    xlpop();			/* l_modules */
    return (true);
  }
  else {
    return (NIL);
  }
}


/******************************************************************************
 * remember to XtFree() the results of this proc when done.
 ******************************************************************************/
char* Wut_Sexp_To_String(sexp, length)
     LVAL sexp;
     int* length;
{
  extern int plevel, plength;	/* xlisp/xlprin.c */
  LVAL stream, next;
  char *str;
  char *result;
  int ch;
  unsigned int len;

  xlsave1(stream);
  stream = newustream();	/* create an unnamed stream for printing */

  /* print the value into unnamed stream */
  plength = plevel = 32767; 
  xlprintl(stream, sexp, TRUE);

  /* the following is stolen/derived from xlfio.c:getstroutput() */

  /* compute the length of the stream */
  for (next = gethead(stream), len = 0;
       (next != NIL);
       next = cdr(next)) {
    len++;
  }

  result = str = (char*) XtMalloc((unsigned) ((len + 1) * sizeof(char)));

  /* copy the characters into the new string */
  while ((ch = xlgetc(stream)) != EOF) {
    *str++ = ch;
  }
  *str = '\0';

  xlpop();			/* stream */
  *length = (int) len;

  return (result);
}


/******************************************************************************
 *
 ******************************************************************************/
Wutils_Init()
{
  s_MODULES = xlenter("*MODULES*");
  setsvalue(s_MODULES, NIL);	/* declare this a special variable so we can set it via PROGV */

  k_NOVERBOSE  = xlenter(":NOVERBOSE");
}
