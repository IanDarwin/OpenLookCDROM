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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/time/RCS/timeoday.c,v 1.9 1994/02/01 20:53:44 rr2b Exp $";
#endif

#include <andrewos.h>
#include <stdio.h>
#include <observe.ih>
#include <im.ih>
#include <util.h>
#include <environ.ih>
#include <timeoday.eh>

/* Defined constants and macros */
#define MAX_LINE_LENGTH 70  /* can't be less than 6 */
#define DS_VERSION 1 /* datastream version */
#define FONTFAMILY "andysans"
#define FONTTYPE 0
#define FONTSIZE 12
#define SECMIN 60
#define MINHOUR 60
#define HOURDAY 12

/* External declarations */

/* Forward Declarations */
static void WriteLine();
static char *GlomStrings(), *ReadLine(), *EncodeFont();

/* Global variables */
static char *months[] = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", NULL};
static char *weekdays[] = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", NULL};
static int maxdigraphlen;


boolean
timeoday__InitializeClass(c)
struct classheader *c;
{
/* 
  Initialize all the class data.
*/
  int i, t;

  maxdigraphlen = 2;
  for (i=0; months[i]; ++i) {
    t = strlen(months[i]);
    if (t>maxdigraphlen) maxdigraphlen = t;
  }
  for (i=0; weekdays[i]; ++i) {
    t = strlen(weekdays[i]);
    if (t>maxdigraphlen) maxdigraphlen = t;
  }
  return(TRUE);
}


void
timeoday__FormatTime(self)
struct timeoday *self;
{
/*     Field Descriptors:
          n    insert a new-line character
          t    insert a tab character
          m    month of year - 01 to 12
          O    month of year - 1 to 12
          d    day of month - 01 to 31
          A    day of month - 1 to 31
          Z    ordinal day of month - 1st to 31st
          y    last 2 digits of year - 00 to 99
          Y    year - 1900 on ...
          D    date as mm/dd/yy
          H    hour - 00 to 23
	  u    hour - 1 to 12
          M    minute - 00 to 59
          S    second - 00 to 59
          T    time as HH:MM:SS
          j    day of year - 001 to 366
          w    day of week - Sunday = 0
          W    weekday - Sunday to Saturday
          a    abbreviated weekday - Sun to Sat
          h    abbreviated month - Jan to Dec
          o    month - January to December
          r    time in AM/PM notation
	  P    AM or PM
*/
  int i, j;
  struct tm *the_time;
  
  if (self->tod == NULL) return;

  the_time = localtime(&(self->now));
  for(j = i = 0; self->format[i]; ++i) {
    if (self->format[i]=='%') {
      switch(self->format[++i]) {
      case '%':
	self->tod[j++] = '%';
	break;
      case 'n':
	self->tod[j++] = '\n';
	break;
      case 't':
	self->tod[j++] = '\t';
	break;
      case 'm':
	sprintf(self->tod+j, "%02d", the_time->tm_mon+1);
	j = strlen(self->tod);
	break;
      case 'O':
	sprintf(self->tod+j, "%d", the_time->tm_mon+1);
	j = strlen(self->tod);
	break;
      case 'd':
	sprintf(self->tod+j, "%02d", the_time->tm_mday);
	j = strlen(self->tod);
	break;
      case 'A':
	sprintf(self->tod+j, "%d", the_time->tm_mday);
	j = strlen(self->tod);
	break;
      case 'Z':
	sprintf(self->tod+j, "%d%s", the_time->tm_mday,
		ORDINALIZE(the_time->tm_mday));
	j = strlen(self->tod);
	break;
      case 'y':
	sprintf(self->tod+j, "%02d", (the_time->tm_year)%100);
	j = strlen(self->tod);
	break;
      case 'Y':
	sprintf(self->tod+j, "%4d", the_time->tm_year+1900);
	j = strlen(self->tod);
	break;
      case 'D':
	sprintf(self->tod+j, "%02d/%02d/%02d", the_time->tm_mon+1,
		the_time->tm_mday, (the_time->tm_year)%100);
	j = strlen(self->tod);
	break;
      case 'H':
	sprintf(self->tod+j, "%02d", the_time->tm_hour);
	j = strlen(self->tod);
	break;
      case 'M':
	sprintf(self->tod+j, "%02d", the_time->tm_min);
	j = strlen(self->tod);
	break;
      case 'S':
	sprintf(self->tod+j, "%02d", the_time->tm_sec);
	j = strlen(self->tod);
	break;
      case 'T':
	sprintf(self->tod+j, "%02d:%02d:%02d", the_time->tm_hour,
		the_time->tm_min, the_time->tm_sec);
	j = strlen(self->tod);
	break;
      case 'j':
	sprintf(self->tod+j, "%03d", the_time->tm_yday+1);
	j = strlen(self->tod);
	break;
      case 'w':
	sprintf(self->tod+j, "%01d", the_time->tm_wday);
	j = strlen(self->tod);
	break;
      case 'a':
	sprintf(self->tod+j, "%3s", weekdays[the_time->tm_wday]);
	j += 3;
	break;
      case 'W':
	sprintf(self->tod+j, "%s", weekdays[the_time->tm_wday]);
	j = strlen(self->tod);
	break;
      case 'h':
	sprintf(self->tod+j, "%3s", months[the_time->tm_mon]);
	j += 3;
	break;
      case 'o':
	sprintf(self->tod+j, "%s", months[the_time->tm_mon]);
	j = strlen(self->tod);
	break;
      case 'r':
	sprintf(self->tod+j, "%02d:%02d:%02d %s", 
		((the_time->tm_hour)%12)==0?12:
		((the_time->tm_hour)%12),
		the_time->tm_min, the_time->tm_sec,
		the_time->tm_hour > 11 ? "PM" : "AM");
	j = strlen(self->tod);
	break;
      case 'u':
	sprintf(self->tod+j, "%d", ((the_time->tm_hour)%12)==0?12:((the_time->tm_hour)%12));
	j = strlen(self->tod);
	break;
      case 'P':
	sprintf(self->tod+j, "%s", the_time->tm_hour > 11 ? "PM" : "AM");
	j = strlen(self->tod);
	break;
      }
    } else {
      self->tod[j++] = self->format[i];
    }
  }
  self->tod[j] = '\0';
  timeoday_NotifyObservers(self,observable_OBJECTCHANGED);
}


static void
UpdateTime(self)
struct timeoday *self;
{
  timeoday_UpdateTime(self);
}


void
timeoday__UpdateTime(self)
struct timeoday *self;
{
  self->now = time(0);
  self->ev = im_EnqueueEvent(UpdateTime, self, event_SECtoTU(self->epoch - (self->now % self->epoch)));
  timeoday_FormatTime(self);

  return;
}



void
timeoday__SetFormat(self, format)
struct timeoday *self;
char *format;
{
  int i;
  char prof_namebuf[100];

  if (self->ev) {
    event_Cancel(self->ev);
    self->ev = NULL;
  }

  if (self->format) free(self->format);
  sprintf(prof_namebuf, "%sdefaultformat", class_GetTypeName(self));
  if (format != NULL) {
      self->format = NewString(format);
  } else {
      self->format = environ_GetProfile(prof_namebuf);
      if (self->format == NULL) {
	  self->format = NewString("");
      } else {
	  self->format = NewString(self->format);
      }
  }
  if (strcmp(self->format, "")==0) {
    if (self->format) free(self->format);
    self->format = NewString("%o %A, %Y");
  }
  self->epoch = SECMIN*MINHOUR*HOURDAY;
  for(i=0; self->format[i]; ++i) {
    if (self->format[i] == '%') {
      switch(self->format[++i]) {
      case 'S': case 'T': case 'r':
	self->epoch = MIN(self->epoch, 1);
	break;
      case 'M':
	self->epoch = MIN(self->epoch, SECMIN);
	break;
      case 'H': case 'u':
	self->epoch = MIN(self->epoch, SECMIN*MINHOUR);
	break;
      }
    }
  }
  if (self->tod) free(self->tod);
  self->tod = malloc(strlen(self->format)/2*maxdigraphlen+1);
  UpdateTime(self);
}


boolean
timeoday__InitializeDefaults(self)
struct timeoday *self;
{
  char *fontfamily;
  int fonttype, fontsize;
  char prof_namebuf[100];

  timeoday_SetFormat(self, NULL);

  sprintf(prof_namebuf, "%sdefaultfontfamily", class_GetTypeName(self));
  fontfamily = environ_GetProfile(prof_namebuf);
  if ((fontfamily == NULL) || (strcmp(fontfamily, "") == 0)) fontfamily = FONTFAMILY;

  sprintf(prof_namebuf, "%sdefaultfonttype", class_GetTypeName(self));
  fonttype = environ_GetProfileInt(prof_namebuf, FONTTYPE);

  sprintf(prof_namebuf, "%sdefaultfontsize", class_GetTypeName(self));
  fontsize = environ_GetProfileInt(prof_namebuf, FONTSIZE);
  self->myfontdesc = fontdesc_Create(fontfamily, fonttype, fontsize);

  timeoday_FormatTime(self);
  return(TRUE);
}


boolean
timeoday__InitializeObject(c, self)
struct classheader *c;
struct timeoday *self;
{
/*
  Inititialize the object instance data.
*/

  self->ev = NULL;
  self->tod = NULL;
  self->format = NULL;

  return(timeoday_InitializeDefaults(self));
}


void
timeoday__FinalizeObject(c, self)
struct classheader *c;
struct timeoday *self;
{
/*
  Finalize the object instance data.
*/
    if (self->ev) {
	event_Cancel(self->ev);
	self->ev=NULL;
    }
  if (self->tod) free(self->tod);
  if (self->format) free(self->format);
  return;
}


void
timeoday__WriteDataPart(self, fp)
struct timeoday *self;
FILE *fp;
{
/*
  Write the object data out onto the datastream.
*/
  char *encfont;

  WriteLine(fp, self->format);
  encfont = self->myfontdesc ? EncodeFont(self) : NULL;
  WriteLine(fp, encfont ? encfont : "");
  if (encfont) {
    free(encfont);
    encfont = NULL;
  }
}


long
timeoday__Write(self, fp, id, level)
struct timeoday *self;
FILE *fp;
long id;
int level;
{
/*
  Write the object data out onto the datastream.

  Sample output from datastream version 1:
    \begindata{timeoday, 1234567}
    Datastream version: 1
    format string
    font
    \enddata{timeoday, 1234567}

*/

  long uniqueid = timeoday_UniqueID(self);

  if (id != timeoday_GetWriteID(self)) {
    /* New Write Operation */
    timeoday_SetWriteID(self, id);
    fprintf(fp, "\\begindata{%s,%d}\nDatastream version: %d\n",
	    class_GetTypeName(self), uniqueid, DS_VERSION);

    timeoday_WriteDataPart(self, fp);

    fprintf(fp, "\\enddata{%s,%d}\n", class_GetTypeName(self), uniqueid);
  }
  return(uniqueid);
}


long
timeoday__ReadDataPart(self, fp)
struct timeoday *self;
FILE *fp;
{
/*
  Read in the object from the file.
*/
  char *buf;
  
  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  timeoday_SetFormat(self, buf);
  free(buf);

  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  if (strcmp(buf,"")!= 0) {
    char name[MAXPATHLEN];
    long style, size;
    if (fontdesc_ExplodeFontName(buf,name,sizeof(name), &style, &size)) {
      timeoday_SetFont(self,fontdesc_Create(name,style,size));
    }
  }
  free(buf);

  return(dataobject_NOREADERROR);
}



long
timeoday__Read(self, fp, id)
struct timeoday *self;
FILE *fp;
long id;
{
/*
  Read in the object from the file.
*/
  char *buf, buf2[255];
  long result;
  
  timeoday_SetID(self, timeoday_UniqueID(self));

  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  if (strncmp(buf,"Datastream version:",19))
    return(dataobject_BADFORMAT);
  if (atoi(buf+19) != DS_VERSION)	/* datastream version */
    return(dataobject_BADFORMAT);
  free(buf);

  if ((result = timeoday_ReadDataPart(self, fp)) != dataobject_NOREADERROR)
    return(result);

  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  sprintf(buf2, "\\enddata{%s,%ld}", class_GetTypeName(self), id);
  if (strcmp(buf, buf2)) {
    free(buf);
    return(dataobject_MISSINGENDDATAMARKER);
  }
  free(buf);

  return(dataobject_NOREADERROR);
}



void
timeoday__SetFont(self, f)
struct timeoday *self;
struct fontdesc *f;
{
/*
  Set the font descriptor for this object.
*/

  self->myfontdesc = f;
  timeoday_NotifyObservers(self,observable_OBJECTCHANGED);
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
struct timeoday *self;
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
  myfontname = fontdesc_GetFontFamily(self->myfontdesc);
  myfontsize = fontdesc_GetFontSize(self->myfontdesc);
  myfonttype = fontdesc_GetFontStyle(self->myfontdesc);
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
