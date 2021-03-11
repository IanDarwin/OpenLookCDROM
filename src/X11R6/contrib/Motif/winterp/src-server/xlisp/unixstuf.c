/* -*-C-*-
********************************************************************************
*
* File:         unixstuff.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/unixstuf.c,v 2.4 1994/06/06 15:59:10 npm Exp $
* Description:  UNIX-Specific interfaces for XLISP
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:47 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, Niels Mayer, Luke Tierney and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Enterprise Integration
* Technologies, Hewlett-Packard Company, Niels Mayer, Luke Tierney and David
* Betz make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
* LUKE TIERNEY AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD
* COMPANY, NIELS MAYER, LUKE TIERNEY NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/unixstuf.c,v 2.4 1994/06/06 15:59:10 npm Exp $";

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

/* Modified again by Tom Almy */
/*I doubt that standard input and output can be redirected with this version*/

#include <stdlib.h>
#include <errno.h>
#ifdef WINTERP
#include <fcntl.h>		/* needed for fcntl(2) calls */
#endif /* WINTERP */
#include "xlisp.h"

extern char *sys_errlist[];	/* part of <errno.h>, but not defined by it. */
extern int sys_nerr;		/* part of <errno.h>, but not defined by it. */
extern int errno;		/* some but not all systems require this */


/******************************************************************************
 * Prim_POPEN - start a process and open a pipe for read/write 
 * (code stolen from xlfio.c:xopen())
 *
 * syntax: (popen <command line> :direction <direction>)
 *                <command line> is a string to be sent to the subshell (sh).
 *                <direction> is either :input (to read from the pipe) or
 *                                      :output (to write to the pipe).
 *                                      (:input is the default)
 *
 * Popen returns a stream, or NIL if files or processes couldn't be created.
 * The  success  of  the  command  execution  can be checked by examining the 
 * return value of pclose. 
 *
 * Added to XLISP by Niels Mayer
 ******************************************************************************/
LVAL Prim_POPEN()
{
  extern LVAL k_direction, k_input, k_output;
  char *name,*mode;
  FILE *fp;
  LVAL dir;
#ifdef BETTERIO
  int iomode;		/* file mode, as stored in node */
#endif /* BETTERIO */

  /* get the process name and direction */
  name = (char *) getstring(xlgastring());
  if (!xlgetkeyarg(k_direction, &dir))
    dir = k_input;
  
  /* get the mode */
  if (dir == k_input) {
    mode = "r";
#ifdef BETTERIO
    iomode = S_FORREADING;
#endif /* BETTERIO */
  }
  else if (dir == k_output) {
    mode = "w";
#ifdef BETTERIO
    iomode = S_FORWRITING;
#endif /* BETTERIO */
  }
  else
    xlerror("bad direction",dir);

  xllastarg();
  
  /* try to open the file */
#ifdef BETTERIO
  return ((fp = popen(name, mode)) ? cv_pipe(fp, iomode) : NIL);
#else /* !defined(BETTERIO) */
  return ((fp = popen(name, mode)) ? cv_pipe(fp) : NIL);
#endif /* BETTERIO */
}


/******************************************************************************
 * Prim_PCLOSE - close a pipe opened by Prim_POPEN().
 * (code stolen from xlfio.c:xclose())
 *
 * syntax: (pclose <stream>)
 *                  <stream> is a stream created by popen.
 * returns T if the command executed successfully, otherwise, 
 * returns the exit status of the opened command.
 *
 * Added to XLISP by Niels Mayer
 ******************************************************************************/
LVAL Prim_PCLOSE()
{
  extern LVAL true;
  LVAL fptr;
  int  result;

  /* get file pointer */
  fptr = xlga_pipe();
  xllastarg();

  /* make sure the file exists */
  if (getfile(fptr) == CLOSED)
    return (NIL);

  /* close the pipe */
  result = pclose(getfile(fptr));
  setfile(fptr, CLOSED);
  setsavech(fptr, '\0');

  if (result == -1)
    xlfail("stream passed to pclose() has not been opened with popen()");

  /* return T if success (exit status 0), else return exit status */
  return (result ? cvfixnum(0xff & (result>>8)) : true);
}


/******************************************************************************
 * Prim_SYSTEM - run a process, sending output (if any) to stdout/stderr
 *
 * syntax: (system <command line>)
 *		   <command line> is a string to be sent to the subshell (sh).
 *
 * Returns T if the command executed succesfully, otherwise returns the
 * integer shell exit status for the command.
 *
 * Added to XLISP by Niels Mayer
 ******************************************************************************/
LVAL Prim_SYSTEM()
{
  extern LVAL true;
  LVAL command;
  int  result;
  char temptext[1024];

  /* get shell command */
  command = xlgastring();
  xllastarg();
  
  /* run the process */
  result = system((char *) getstring(command));

  if (result == -1) {		/* if a system error has occured */
    if (errno < sys_nerr)
      (void) sprintf(temptext, "Error in system(3S): %s", sys_errlist[errno]);
    else
      (void) strcpy(temptext, "Error in system(3S): unknown error.");
    xlfail(temptext);
  }

  /* return T if success (exit status 0), else return exit status */
  return (result ? cvfixnum(0xff & (result>>8)) : true);
}


/******************************************************************************
 * (FFLUSH <output-stream>)
 * 	--> returns T on success, else error
 *
 * Calls fflush(3S) on <output-stream>
 ******************************************************************************/
LVAL Prim_FFLUSH()
{
  extern LVAL true;
  LVAL lval_stream;
  int  result;
  char temptext[1024];

  lval_stream = xlgastream();
  if (getfile(lval_stream) == CLOSED)
    xlerror("File not opened.", lval_stream);
  if ((lval_stream->n_sflags & S_FORWRITING) == 0)
    xlerror("File not writeable.", lval_stream);
  xllastarg();

  result = fflush(getfile(lval_stream));

  if (result != 0) {		/* if a system error has occured */
    if (errno < sys_nerr)
      (void) sprintf(temptext, "Error in fflush(3S): %s", sys_errlist[errno]);
    else
      (void) strcpy(temptext, "Error in fflush(3S): unknown error.");
    xlfail(temptext);
  }

  /* return T if success (exit status 0), else return fflush() result */
  return (true);
}


/******************************************************************************
 * (FSCANF-FIXNUM <stream> <scanf-format>)
 * This routine calls fscanf(3s) on a <stream> that was previously openend
 * via open or popen. It will not work on an USTREAM.
 * <scanf-format> is a format string containing a single conversion
 * directive that will result in an integer valued conversion.
 * %d, %u, %o, %x, %ld, %lu, %lo and %lx style conversions 
 * are acceptable for this routine.
 * WARNING: specifying a <scanf-format> that will result in the conversion
 * of a result larger than sizeof(long) will result in corrupted memory and
 * core dumps. 
 * 
 * This routine will return an FIXNUM if fscanf() returns 1 (i.e. if
 * the one expected conversion has succeeded. It will return NIL if the
 * conversion wasn't successful, or if EOF was reached.
 ******************************************************************************/
LVAL Prim_FSCANF_FIXNUM()
{
  LVAL  lval_stream;
  char* fmt;
  long  result;
  
  lval_stream = xlgastream();
  if (getfile(lval_stream) == CLOSED)
    xlerror("File not opened.", lval_stream);
  fmt = (char *) getstring(xlgastring());
  xllastarg();
  
  result = 0L;			/* clear it out hibits incase short is written */
  /* if scanf returns result <1 then an error or eof occured. */
  if (fscanf(getfile(lval_stream), fmt, &result) < 1)
    return (NIL);
  else
    return (cvfixnum((FIXTYPE) result));
}


/******************************************************************************
 * (FSCANF-STRING <stream> <scanf-format>)
 * This routine calls fscanf(3s) on a <stream> that was previously openend
 * via open or popen. It will not work on an USTREAM.
 * <scanf-format> is a format string containing a single conversion
 * directive that will result in a string valued conversion.
 * %s, %c, and %[...] style conversions are acceptable for
 * this routine.
 * WARNING: specifying a <scanf-format> that will result in the conversion
 * of a result larger than 1024 characters will result in corrupted
 * memory and core dumps.
 * 
 * This routine will return a string if fscanf() returns 1 (i.e. if
 * the one expected conversion has succeeded. It will return NIL if the
 * conversion wasn't successful, or if EOF was reached.
 ******************************************************************************/
LVAL Prim_FSCANF_STRING()
{
  LVAL lval_stream;
  char* fmt;
  char result[BUFSIZ];

  
  lval_stream = xlgastream();
  if (getfile(lval_stream) == CLOSED)
    xlerror("File not opened.", lval_stream);
  fmt = (char *) getstring(xlgastring());
  xllastarg();
  
  result[0] = result[1] = '\0';	/* if the conversion is %c, then fscanf
				   doesn't null terminate the string,
				   so do it just incase */

  /* if scanf returns result <1 then an error or eof occured. */
  if (fscanf(getfile(lval_stream), fmt, result) < 1)
    return (NIL);
  else
    return (cvstring(result));
}


/******************************************************************************
 * (FSCANF-FLONUM <stream> <scanf-format>)
 * This routine calls fscanf(3s) on a <stream> that was previously openend
 * via open or popen. It will not work on an USTREAM.
 * <scanf-format> is a format string containing a single conversion
 * directive that will result in an FLONUM valued conversion.
 * %e %f or %g are valid conversion specifiers for this routine.
 *
 * WARNING: specifying a <scanf-format> that will result in the conversion
 * of a result larger than sizeof(float) will result in corrupted memory and
 * core dumps. 
 * 
 * This routine will return a FLONUM if fscanf() returns 1 (i.e. if
 * the one expected conversion has succeeded. It will return NIL if the
 * conversion wasn't successful, or if EOF was reached.
 ******************************************************************************/
LVAL Prim_FSCANF_FLONUM()
{
  LVAL lval_stream;
  char* fmt;
  float result;
  
  lval_stream = xlgastream();
  if (getfile(lval_stream) == CLOSED)
    xlerror("File not opened.", lval_stream);
  fmt = (char *) getstring(xlgastring());
  xllastarg();
  
  /* if scanf returns result <1 then an error or eof occured. */
  if (fscanf(getfile(lval_stream), fmt, &result) < 1)
    return (NIL);
  else
    return (cvflonum((FLOTYPE) result));
}


#ifdef TIMES
/***********************************************************************/
/**								      **/
/**		     Time and Environment Functions		      **/
/**								      **/
/***********************************************************************/

#include <unistd.h>		/* POSIX... */
#include <sys/times.h>

unsigned long ticks_per_second()
{
  return((unsigned long) sysconf(_SC_CLK_TCK));	/* sysconf() is POSIX... */
}


unsigned long run_tick_count()
{
  struct tms tm;

  times(&tm);
  return((unsigned long) tm.tms_utime + tm.tms_stime );
}


unsigned long real_tick_count()
{				   /* Real time */
  return((unsigned long) (ticks_per_second() *
			  time((unsigned long *) NULL)));
}


LVAL xtime()
{
  LVAL expr, result;
  unsigned long tm, rtm;
  double dtm, rdtm;

/* get the expression to evaluate */
  expr = xlgetarg();
  xllastarg();

  tm = run_tick_count();
  rtm = real_tick_count();
  result = xleval(expr);
  tm = run_tick_count() - tm;
  rtm = real_tick_count() - rtm;
  dtm = (tm > 0) ? tm : -tm;
  rdtm = (rtm > 0) ? rtm : -rtm;
  sprintf(buf, "CPU %.2f sec., Real %.2f sec.\n", dtm / ticks_per_second(),
					    rdtm / ticks_per_second());
  trcputstr(buf);
  return(result);
}


LVAL xruntime() {
    xllastarg();
    return(cvfixnum((FIXTYPE) run_tick_count()));
}


LVAL xrealtime() {
    xllastarg();
    return(cvfixnum((FIXTYPE) real_tick_count()));
}

#endif /* TIMES */


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/* -- stuff.c  -- operating system specific routines */
/* -- Written by dbetz for XLISP 2.0 */
/* -- Copied by EFJohnson from a BIX message */
/* -- Modified by Tom Almy, Niels Mayer, Ken Whedbee, and others no doubt */
/* -- Unix System V */

#define LBSIZE	200

/* -- external variables */
extern	FILE	*tfp;

/* -- local variables */
static	char	lbuf[LBSIZE];
int	lposition;  /* export this */
static	int	lindex;
static	int	lcount;


/* -- osinit - initialize */
void osinit(banner)
char	*banner;
{
#ifndef WINTERP
	fprintf(stderr,"%s\nUNIX version\n", banner );
#endif /* notdef(WINTERP) */
	lindex	= 0;
	lcount	= 0;
	lposition = 0;
}


/* -- osfinish - clean up before returning to the operating system */
void osfinish()
{
}


/* -- xoserror - print an error message */
void xoserror(msg)
char	*msg;

{
	printf( "error: %s\n", msg );
}


/* osrand - return next random number in sequence */
long osrand(rseed)
  long rseed;
{
    long k1;

    /* make sure we don't get stuck at zero */
    if (rseed == 0L) rseed = 1L;

    /* algorithm taken from Dr. Dobbs Journal, November 1985, page 91 */
    k1 = rseed / 127773L;
    if ((rseed = 16807L * (rseed - k1 * 127773L) - k1 * 2836L) < 0L)
	rseed += 2147483647L;

    /* return a random number between 0 and MAXFIX */
    return rseed;
}

#ifdef WINTERP
/******************************************************************************
 * For WINTERP, we need to wrap calls to fopen() so that we can call fcntl()
 * to set close-on-exec. When we run subprocesses out of WINTERP, we don't 
 * want the files opened in WINTERP to be opened in the subproc. This 
 * so that we don't run out of file descriptors in the subproc, and so that 
 * WINTERP can be shut down correctly after it has spawned subprocesses.
 ******************************************************************************/
/* -- osaopen -- open an ascii file */
FILE	*osaopen( name, mode )
char	*name, *mode;
{
  FILE* fp = fopen(name, mode);
  if (fp)
    fcntl(fileno(fp), F_SETFD, 1); /* set close-on-exec */
  return (fp);
}

/* -- osbopen -- open a binary file */
FILE	*osbopen( name, mode )
char	*name, *mode;
{
  FILE* fp = fopen(name, mode);
  if (fp)
    fcntl(fileno(fp), F_SETFD, 1); /* set close-on-exec */
  return (fp);
}
#endif /* WINTERP */


#ifdef PATHNAMES
#ifdef WINTERP
/* see ../utils.c:ospopen() -- allows retrieval of search path from winterp resource, else getenv() */
#else /* !defined(WINTERP) */
/* ospopen - open using a search path */
FILE *ospopen(name, ascii)
char *name;
int ascii;  /* value not used in UNIX */
{
  FILE *fp = NULL;		/* NPM -- added this initialization... */
  char *path = getenv(PATHNAMES);
  char *newnamep;
  char ch;
  int  namelen = strlen(name);	/* NPM--added this to fix potential string overflow bug */
  int  pathlen;			/* NPM--added this to fix potential string overflow bug */
  char newname[FNAMEMAX+1];	/* NPM -- subst FNAMEMAX for Almy's '256' */

  /* don't do a thing if user specifies explicit path */
  if (strchr(name,'/') != NULL || path == NULL)
    return fopen(name, "r");

  do {
    if (*path == '\0')		/* no more paths to check */
      /* check current directory just in case */
      return fopen(name, "r");

    pathlen = 0;
    newnamep = newname;
    while ((ch = *path++) != '\0' && ch != ':' && ch != ' ' && (++pathlen < FNAMEMAX)) /* NPM: note addition to prevent str overflow */
      *newnamep++ = ch;
    if (pathlen > 0) {		/* NPM: don't do anything on empty path, or if extra ' ' exists... */
      if ((*(newnamep-1) != '/') && (++pathlen < FNAMEMAX))
	*newnamep++ = '/';	/* final path separator needed */
      *newnamep = '\0';		/* null terminate it */
      if ((namelen + pathlen) < FNAMEMAX) {
	strncpy(newnamep, name, namelen);
	*(newnamep + namelen) = '\0'; /* null terminate it */
	fp = fopen(newname, "r");
      }
      else
	xlfail("File path or file name too long.");
    }
  } while (fp == NULL);		/* not yet found */

  return (fp);
}
#endif /* WINTERP */
#endif /* PATHNAMES */


#ifdef BETTERIO
/* rename argument file as backup, return success name */
/* For new systems -- if cannot do it, just return TRUE! */
int renamebackup(filename)
  char *filename;
{
    char *bufp, ch=0;

    strcpy(buf, filename);  /* make copy with .bak extension */

    bufp = &buf[strlen(buf)];	/* point to terminator */
    while (bufp > buf && (ch = *--bufp) != '.' && ch != '/') ;


    if (ch == '.') strcpy(bufp, ".bak");
    else strcat(buf, ".bak");

    unlink(buf);

    return !rename(filename, buf);
}
#endif /* BETTERIO */


/* -- ostgetc - get a character from the terminal */
int	ostgetc()
{
	while(--lcount < 0 )
		{
		if ( fgets(lbuf,LBSIZE,stdin) == NULL )
			return( EOF );
		if ( tfp )
			fputs( lbuf, tfp );
		lcount = strlen( lbuf );
		lindex = 0;
		}

	return( lbuf[lindex++] );
}


/* -- ostputc - put a character to the terminal */
void ostputc( ch )
int	ch;
{
	if (ch == '\n') lposition = 0;
	else lposition++;

	/* -- output the character */
	putchar( ch );

	/* -- output the char to the transcript file */
	if ( tfp )
		putc( ch, tfp );
}


/* -- osflush - flush the terminal input buffer */
void osflush()
{
	lindex = lcount = lposition = 0;
}


#ifdef WINTERP
/* For WINTERP, this is now defined in winterp.c:
   void oscheck()
   {
   }
*/
#else /* !defined(WINTERP) */
void oscheck()
{
}
#endif /* WINTERP */


/* -- ossymbols - enter os-specific symbols */
void ossymbols()
{
}
