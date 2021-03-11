/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/time/RCS/writestmp.c,v 1.4 1992/12/15 21:57:11 rr2b R6tape $";
#endif

#include <stdio.h>
#include <writestmp.eh>
#include <andrewos.h>
#include <observe.ih>
#include <im.ih>
#include <util.h>
#include <environ.ih>

/* Defined constants and macros */
#define MAX_LINE_LENGTH 70  /* can't be less than 6 */
#define DS_VERSION 1 /* datastream version */

/* External declarations */

/* Forward Declarations */
static void WriteLine();
static char *GlomStrings(), *ReadLine(), *EncodeFont();

/* Global variables */


boolean
writestamp__InitializeClass(c)
struct classheader *c;
{
/* 
  Initialize all the class data.
*/

  return(TRUE);
}


boolean
writestamp__InitializeObject(c, self)
struct classheader *c;
struct writestamp *self;
{
/*
  Inititialize the object instance data.
*/
  return(writestamp_InitializeDefaults(self));
}


void
writestamp__FinalizeObject(c, self)
struct classheader *c;
struct writestamp *self;
{
/*
  Finalize the object instance data.
*/
  return;
}


static void
writestamp__UpdateTime(self)
struct writestamp *self;
{
  /* No Op, for writestamp. */

  writestamp_FormatTime(self);

  return;
}


void
writestamp__WriteDataPart(self, fp)
struct writestamp *self;
FILE *fp;
{
/*
  Write the object data out onto the datastream.
*/
  char thetime[50];

  super_WriteDataPart(self, fp);
  writestamp_SetTime(self, time(0));
  writestamp_FormatTime(self);
  sprintf(thetime, "%ld", writestamp_GetTime(self));
  WriteLine(fp, thetime);
}


long
writestamp__ReadDataPart(self, fp)
struct writestamp *self;
FILE *fp;
{
/*
  Read in the object from the file.
*/
  char *buf;
  long ec;

  if ((ec = super_ReadDataPart(self, fp)) != dataobject_NOREADERROR) {
    return(ec);
  }
  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  writestamp_SetTime(self, atol(buf));
  writestamp_FormatTime(self);
  free(buf);

  return(dataobject_NOREADERROR);
}


static void
WriteLine(f, l)
FILE *f;
char *l;
{
/* 
  Output a single line onto the data stream, quoting
  back slashes and staying within line length limits.
  Warning:  this routine wasn't meant to handle embedded
  newlines.
*/

  char buf[MAX_LINE_LENGTH];
  int i = 0;

  for (;*l != '\0'; ++l) {
    if (i > (MAX_LINE_LENGTH - 5)) {
      buf[i++] = '\\';  /* signal for line continuation */
      buf[i++] = '\n';
      buf[i++] = '\0';
      fputs(buf,f);
      i = 0;
    } /* if (i > ...) */
    switch (*l) {
    case '\\': 
      /* if a backslash, quote it. */
      buf[i++] = '\\';
      buf[i++] = *l;
      break;
    default:
      buf[i++] = *l;
    } /* switch (*l) */
  } /* for (; *l != ... ) */

  /* Need to empty buffer */
  if ((i > 0) && (buf[i-1]==' ')) {
    /* don't allow trailing whitespace */
    buf[i++] = '\\';
    buf[i++] = '\n';
    buf[i++] = '\0';
    fputs(buf,f);
    fputs("\n",f);
  } else {
    buf[i++] = '\n';
    buf[i++] = '\0';
    fputs(buf,f);
  }
}


static char *
GlomStrings(s, t)
char *s, *t;
{
/* 
  Safely (allocs more memory) concatenates the two strings, 
  freeing the first.  Meant to build a new string of unknown length.
*/

  char *r;

  if (r = (char *)malloc(strlen(s)+strlen(t)+1)) {
    *r = '\0';
    strcpy(r,s);
    free(s);
    strcat(r,t);
    return(r);
  } else {
    free(s);
    return(NULL);
  }
}


static char *
ReadLine(f)
FILE *f;
{
/* 
  Reads from the datastream, attempting to return a single string.
  Undoes quoting and broken lines.
  Warning:  this routine wasn't meant to handle embedded
  newlines.
  Warning:  possible source of memory leaks;  remember to 
  free the returned string when finished with it!
*/

  char buf[MAX_LINE_LENGTH], /* (BUG) What if the datastream is broken? */
       buf2[MAX_LINE_LENGTH],
      *result;
  int i,j;

  
  if (result = (char *)malloc(1)) {
    *result = '\0';

    while (fgets(buf,sizeof(buf),f)) {
      for (i = 0, j = 0; buf[i] != '\0'; ++i) {
	switch (buf[i]) {
	case '\\':
	  /* Unquote backslash or splice line */
	  switch (buf[++i]) {
	  case '\\':
	    /* Unquote the backslash */
	    buf2[j++] = buf[i];
	    break;
	  case '\n':
	    /* broke long line */
	    break;
	  default:
	    /* things like \enddata come through here */
	    buf2[j++] = '\\';
	    buf2[j++] = buf[i];
	    break;
	  } /* switch (buf[++i]) */
	  break;
	case '\n':
	  /* An unquoted newline means end of string */
	  buf2[j++] = '\0';
	  result = GlomStrings(result, buf2);
	  return(result);
	default:
	  buf2[j++] = buf[i];
	  break;
	} /* switch (buf[i]) */
      } /* for (i = 0, ...) */
      buf2[j++] = '\0';
      result = GlomStrings(result, buf2);
    } /* while (fgets...) */
    /* Should not get here... it means we went off the end
       of the data stream.  Ooops. */
  } /* if (result = ... ) */
  return(NULL);
}


static char *
EncodeFont(self)
struct writestamp *self;
{
/*
  Returns a string representing the name of the font for this object.
  (BUG) I shouldn't have to do this function, it should be a method
  of the fontdesc object.  In any case, I handle only Bold, Italic,
  and fixed styles.
*/

  char *buf, type[15];
  long myfonttype, myfontsize;
  char *myfontname;

  *type = '\0';
  myfontname = fontdesc_GetFontFamily(writestamp_GetFont(self));
  myfontsize = fontdesc_GetFontSize(writestamp_GetFont(self));
  myfonttype = fontdesc_GetFontStyle(writestamp_GetFont(self));
  if (myfonttype & fontdesc_Bold) strcpy(type,"b");
  if (myfonttype & fontdesc_Italic) strcpy(type,"i");
  if (myfonttype & fontdesc_Fixed) strcpy(type,"f");
  if (buf = (char *)malloc(strlen(myfontname)+25)) {
    sprintf(buf,"%s%d%s", myfontname, myfontsize, type);
    return (buf);
  } else {
    return(NULL);
  }
}
