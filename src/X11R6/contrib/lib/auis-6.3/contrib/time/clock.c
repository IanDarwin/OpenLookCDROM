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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/time/RCS/clock.c,v 1.7 1994/02/01 20:52:10 rr2b Exp $";
#endif

#include <andrewos.h>
#include <stdio.h>
#include <observe.ih>
#include <im.ih>
#include <util.h>
#include <environ.ih>
#include <fontdesc.ih>
#include <clock.eh>

/* Defined constants and macros */
#define MAX_LINE_LENGTH 70  /* can't be less than 6 */
#define DS_VERSION 1 /* datastream version */
#define FONTFAMILY "andysans"
#define FONTTYPE fontdesc_Plain
#define BORDERWIDTH 2		/* border line width */
#define HOURSWIDTH 4		/* hour hand line width */
#define MINUTESWIDTH 2		/* minute hand line width */
#define SECONDSWIDTH 1		/* second hand line width */
#define HOURSLENGTH 60		/* hour hand length */
#define MINUTESLENGTH 80	/* minute hand length */
#define SECONDSLENGTH -20	/* second hand length */

/* External declarations */

/* Forward Declarations */
static void WriteLine();
static char *GlomStrings(), *ReadLine();

/* Global variables */


boolean
clock__InitializeClass(c)
struct classheader *c;
{
/* 
  Initialize all the class data.
*/
  return(TRUE);
}


static void
UpdateTime(self)
struct clock *self;
{
  struct tm *the_time;
  
  self->now = time(0);
  self->ev = im_EnqueueEvent(UpdateTime, self, event_SECtoTU(self->epoch - (self->now % self->epoch)));
  the_time = localtime(&(self->now));
  self->clockface.seconds = the_time->tm_sec;
  self->clockface.minutes = the_time->tm_min + (the_time->tm_sec/12)/5.0;
  self->clockface.hours = (the_time->tm_hour%12) + (the_time->tm_min/12)/5.0;
  clock_NotifyObservers(self, NULL);
}


boolean
clock__InitializeObject(c, self)
struct classheader *c;
struct clock *self;
{
/*
  Inititialize the object instance data.
*/
  static char *labels[] = {"12", "3", "6", "9", NULL};

  self->ev = NULL;
  self->epoch = 1;		/* update every second */
  self->options.timestamp = time(0);
  self->options.hours_width = HOURSWIDTH;
  self->options.minutes_width = MINUTESWIDTH;
  self->options.seconds_width = SECONDSWIDTH;
  self->options.border_width = BORDERWIDTH;
  self->options.hours_length = HOURSLENGTH;
  self->options.minutes_length = MINUTESLENGTH;
  self->options.seconds_length = SECONDSLENGTH;
  self->options.border_shape = square;
  self->options.major_ticks = 12;
  self->options.minor_ticks = 60;
  self->options.tick_length = 5;
  self->options.labels = labels;
  for (self->options.num_labels = 0; self->options.labels[self->options.num_labels]; ++(self->options.num_labels));
  self->options.fontfamily = "Andy";
  self->options.fontface = fontdesc_Plain;
  
  UpdateTime(self);
  return(TRUE);
}


void
clock__FinalizeObject(c, self)
struct classheader *c;
struct clock *self;
{
/*
  Finalize the object instance data.
*/
    if (self->ev) {
	event_Cancel(self->ev);
	self->ev=NULL;
    }
/*  if (self->tod) free(self->tod); */

  return;
}


static void
clock__WriteDataPart(self, fp)
struct clock *self;
FILE *fp;
{
/*
  Write the object data out onto the datastream.
*/
  char outbuf[255];
  int i;

  sprintf(outbuf, "hand lengths, %d, %d, %d",
	  self->options.hours_length,
	  self->options.minutes_length,
	  self->options.seconds_length);
  WriteLine(fp, outbuf);

  sprintf(outbuf, "hand widths, %d, %d, %d",
	  self->options.hours_width,
	  self->options.minutes_width,
	  self->options.seconds_width);
  WriteLine(fp, outbuf);

  sprintf(outbuf, "ticks, %d, %d, %d",
	  self->options.tick_length,
	  self->options.major_ticks,
	  self->options.minor_ticks);
  WriteLine(fp, outbuf);

  sprintf(outbuf, "border, %d, %d",
	  self->options.border_shape,
	  self->options.border_width);
  WriteLine(fp, outbuf);

  sprintf(outbuf, "labels, %d, %d, %s",
	  self->options.num_labels,
	  self->options.fontface,
	  self->options.fontfamily);
  WriteLine(fp, outbuf);

  if (self->options.num_labels > 0) {
    for (i = 0; i < self->options.num_labels; ++i) {
      WriteLine(fp, self->options.labels[i]);
    }
  }
}


long
clock__Write(self, fp, id, level)
struct clock *self;
FILE *fp;
long id;
int level;
{
/*
  Write the object data out onto the datastream.

  Sample output from datastream version 1:
    \begindata{clock, 1234567}
    Datastream version: 1
    hand lengths, 60, 80, -20
    hand widths, 4, 2, 1
    ticks, 5, 12, 60
    border, 1, 4
    labels, 4, Andy, 0
    12
    3
    6
    9
    \enddata{clock, 1234567}

*/

  long uniqueid = clock_UniqueID(self);

  if (id != clock_GetWriteID(self)) {
    /* New Write Operation */
    clock_SetWriteID(self, id);
    fprintf(fp, "\\begindata{%s,%d}\nDatastream version: %d\n",
	    class_GetTypeName(self), uniqueid, DS_VERSION);

    clock__WriteDataPart(self, fp);

    fprintf(fp, "\\enddata{%s,%d}\n", class_GetTypeName(self), uniqueid);
  }
  return(uniqueid);
}


static long
clock__ReadDataPart(self, fp)
struct clock *self;
FILE *fp;
{
/*
  Read in the object from the file.
*/
  char *buf;
  
  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  if (sscanf(buf, "hand lengths, %d, %d, %d",
	     &(self->options.hours_length),
	     &(self->options.minutes_length),
	     &(self->options.seconds_length)) < 3)
    return(dataobject_PREMATUREEOF);
  free(buf);

  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  if (sscanf(buf, "hand widths, %d, %d, %d",
	     &(self->options.hours_width),
	     &(self->options.minutes_width),
	     &(self->options.seconds_width)) < 3)
    return(dataobject_PREMATUREEOF);
  free(buf);

  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  if (sscanf(buf, "ticks, %d, %d, %d",
	     &(self->options.tick_length),
	     &(self->options.major_ticks),
	     &(self->options.minor_ticks)) < 3)
    return(dataobject_PREMATUREEOF);
  free(buf);

  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  if (sscanf(buf, "border, %d, %d",
	     &(self->options.border_shape),
	     &(self->options.border_width)) < 2)
    return(dataobject_PREMATUREEOF);
  free(buf);

  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  if ((self->options.fontfamily = (char *)malloc(201*sizeof(char))) == NULL)
    return(dataobject_OBJECTCREATIONFAILED);
  if (sscanf(buf, "labels, %d, %d, %200s",	
	     &(self->options.num_labels),
	     &(self->options.fontface),
	     self->options.fontfamily) < 3)
    return(dataobject_PREMATUREEOF);
  free(buf);

  if ((self->options.num_labels) > 0) {
      int i;

      if ((self->options.labels = (char **)malloc(self->options.num_labels*sizeof(char *))) == NULL)
	return(dataobject_OBJECTCREATIONFAILED);
      
      for(i = 0; i < self->options.num_labels; ++i) {
	  if ((buf = ReadLine(fp)) == NULL)
	    return(dataobject_PREMATUREEOF);
	  self->options.labels[i] = buf;
      } /* for i */
  } /* if num_labels > 0 */

  return(dataobject_NOREADERROR);
}



static long
SanelyReturnReadError(self, fp, id, code)
     struct clock *self;
     FILE *fp;
     long id;
     long code;
{
    /*
      Suck up the file until our enddata, then return the error code.
      */
    char *buf, buf2[255];

    buf = NULL;
    sprintf(buf2, "\\enddata{%s,%ld}", class_GetTypeName(self), id);
    do {
	if (buf != NULL) free(buf);
	if ((buf = ReadLine(fp)) == NULL)
	  return(dataobject_PREMATUREEOF);
    } while (strncmp(buf, "\\enddata{", 9) != 0); /* find an enddata */

    if (strcmp(buf, buf2) != 0) {
	free(buf);
	return(dataobject_MISSINGENDDATAMARKER); /* not ours! */
    }
    free(buf);

    return(code);
}


long
clock__Read(self, fp, id)
struct clock *self;
FILE *fp;
long id;
{
/*
  Read in the object from the file.
*/
  char *buf, buf2[255];
  long result;
  
  clock_SetID(self, clock_UniqueID(self));

  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  if (strncmp(buf,"Datastream version:",19))
    return(SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT));
  if (atoi(buf+19) != DS_VERSION)	/* datastream version */
    return(SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT));
  free(buf);

  return(SanelyReturnReadError(self, fp, id, clock__ReadDataPart(self, fp)));
}



void
clock__SetOptions(self, options)
struct clock *self;
struct clock_options *options;
{
  /* BUG Pass back the same struct clock_options * that was returned via
     GetOptions! */

  options->timestamp = time(0);
  clock_SetModified(self);
  clock_NotifyObservers(self, NULL);
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
