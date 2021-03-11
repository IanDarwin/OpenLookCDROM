/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/hyplink/RCS/link.c,v 1.23 1994/02/07 21:43:42 rr2b Exp $";
#endif


#include <stdio.h>
#include <ctype.h>
#include <andrewos.h>
#include <sys/param.h>
#include <pshbttn.ih>
#include <environ.ih>
#include <fontdesc.ih>
#include <path.ih>
#include <link.eh>

extern char *getenv();

/* Defined constants and macros */
#define MAX_LINE_LENGTH 70  /* can't be less than 6 */
#define DS_VERSION 3 /* datastream version */

/* Forward Declarations */
static void WriteLine();
static char *GlomStrings(), *ReadLine();
#ifdef PL8
static char *EncodeFont();
#endif

/* Global variables */


boolean
link__InitializeClass(c)
struct classheader *c;
{
/* 
  Initialize all the class data.
*/
  return(TRUE);
}


boolean
link__InitializeObject(c, self)
struct classheader *c;
struct link *self;
{
/*
  Inititialize the object instance data.
*/

    self->link = NULL;
#ifdef PL8
    self->new_ds = 0;
#endif

    self->pos = 0;
    self->len = 0;
    link_SetStyle(self,environ_GetProfileInt("linkstyle", 2));
    return(TRUE);
}


void
link__FinalizeObject(c, self)
struct classheader *c;
struct link *self;
{
/*
  Finalize the object instance data.
*/
  if (self->link) free(link_GetRawLink(self));
  self->link = NULL;
  return;
}


#if 0
char *
link__GetResolvedLink(self)
struct link *self;
{
/*  Returns the filename stored in the link, except that
    substrings of the form $FOO or $(FOO) or ${FOO} are
    replaced (where possible) with the value of the
    environment variable FOO.
    Result is returned in a static buffer overwritten with each call.
*/

    static char filename[1 + MAXPATHLEN], buf[1 + MAXPATHLEN];
    char *dollar, *varstart, *varend, *envval, *linkval;
    int c, bracket = 0;

    if (!(linkval = link_GetRawLink(self)))
	return (NULL);
    strcpy(filename, linkval);
    while (dollar = strchr(filename, '$')) {
	varstart = dollar + 1;
	switch (*varstart) {
	  case '\0':
	    return (filename);
	  case '{':
	    bracket = 1;
	    ++varstart;
	    if (!(varend = strchr(varstart, '}')))
		return (filename);
	    if (varend == varstart)
		return (filename);
	    break;
	  case '(':
	    bracket = 1;
	    ++varstart;
	    if (!(varend = strchr(varstart, ')')))
		return (filename);
	    if (varend == varstart)
		return (filename);
	    break;
	  default:
	    varend = varstart + 1;
	    while ((*varend != '\0')
		   && isascii(*varend)
		   && (isalnum(*varend) || (*varend == '_')))
		++varend;
	    break;
	}
	c = *varend;
	*varend = '\0';
	envval = getenv(varstart);
	*varend = c;
	if (!envval)
	    return (filename);
	*dollar = '\0';
	strcpy(buf, filename);
	strcat(buf, envval);
	strcat(buf, bracket ? varend + 1 : varend);
	strcpy(filename, buf);
    }
    return (filename);
}

#else
char *link__GetResolvedLink(self)
struct link *self;
{
    static char localbuf[MAXPATHLEN+1];
    if(link_GetRawLink(self)) {
	return path_UnfoldFileName(link_GetRawLink(self), localbuf,NULL);
    } else return NULL;
}
#endif

long
link__Write(self, fp, id, level)
struct link *self;
FILE *fp;
long id;
int level;
{
/*
  Write the object data out onto the datastream.

  Sample output from datastream version 1:
    \begindata{link, 1234567}
    Datastream version: 1
    This is my button label
    /afs/andrew.cmu.edu/usr5/mcinerny/target_file
    
    \enddata{link, 1234567}

  Sample output from datastream version 2:
    \begindata{link, 1234567}
    Datastream version: 2
    /afs/andrew.cmu.edu/usr5/mcinerny/target_file -- the target of the link
    \begindata{link, 1234567}   -- really pushbutton (superclass)
    Datastream version: 2
    This is my button label     -- label
    2                           -- style
    andy12b                     -- font
    black                       -- foreground color
    0xFFFFFF                    -- background color, RGB representation
    \enddata{link, 1234567}     -- again, really for the pushbutton part
    \enddata{link, 1234567}

  Sample output from datastream version 3:
    \begindata{link, 1234567}
    Datastream version: 2
    /afs/andrew.cmu.edu/usr5/mcinerny/target_file -- the target of the link
    1234				-- pos
    5678				-- len
    \begindata{link, 1234567}   -- really pushbutton (superclass)
    Datastream version: 2
    This is my button label     -- label
    2                           -- style
    andy12b                     -- font
    black                       -- foreground color
    0xFFFFFF                    -- background color, RGB representation
    \enddata{link, 1234567}     -- again, really for the pushbutton part
    \enddata{link, 1234567}

*/

  long uniqueid = link_UniqueID(self);

  if (id != link_GetWriteID(self)) {
    /* New Write Operation */
#ifdef PL8
    if (self->new_ds == 0) {
    fprintf(fp, "\\begindata{%s,%d}\nDatastream version: %d\n",
	    class_GetTypeName(self), uniqueid, 1); /* lie! */
    WriteLine(fp, link_GetText(self) ? link_GetText(self) : "");
    WriteLine(fp, link_GetRawLink(self) ? link_GetRawLink(self) : "");
    WriteLine(fp, link_GetButtonFont(self) ? EncodeFont(self) : "");
    } else {
#endif /* PL8 */
    fprintf(fp, "\\begindata{%s,%d}\nDatastream version: %d\n",
	    class_GetTypeName(self), uniqueid, DS_VERSION);
    WriteLine(fp, link_GetRawLink(self) ? link_GetRawLink(self) : "");
    fprintf(fp, "%d\n", link_GetPos(self));
    fprintf(fp, "%d\n", link_GetLen(self));
    super_Write(self, fp, id, level);
#ifdef PL8
    } /* if (self->new_ds == 0) */
#endif /* PL8 */

    fprintf(fp, "\\enddata{%s,%d}\n",
	    class_GetTypeName(self), uniqueid);
    link_SetWriteID(self, id);
  }
  return(uniqueid);
}



static long
link_SanelyReturnReadError(self, fp, id, code)
     struct link *self;
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


static long
ReadOldFormat(self, fp, id)
     struct link *self;
     FILE *fp;
     long id;
{
    char *buf;

    if ((buf = ReadLine(fp)) == NULL) {
	return(link_SanelyReturnReadError(self, fp, id, dataobject_PREMATUREEOF));
    }
    if (strcmp(buf,"")!= 0 ) {
	link_SetText(self, buf);
    }
    free(buf);
    
    if ((buf = ReadLine(fp)) == NULL) {
	return(link_SanelyReturnReadError(self, fp, id, dataobject_PREMATUREEOF));
    }
    if (strcmp(buf,"")!= 0 ) {
	link_SetLink(self, buf);
    }
    free(buf);
    
    if ((buf = ReadLine(fp)) == NULL) {
	return(link_SanelyReturnReadError(self, fp, id, dataobject_PREMATUREEOF));
    }
    if (strcmp(buf,"")!= 0) {
	char name[255];
	long style, size;
	
	if (!fontdesc_ExplodeFontName(buf,name,sizeof(name),
				      &style, &size)) {
	    strcpy(name,"andy");
	    style = fontdesc_Bold;
	    size = 12;
				      }
	link_SetButtonFont(self,fontdesc_Create(name,style,size));
    }
    free(buf);
    return(dataobject_NOREADERROR);
}


long
link__Read(self, fp, id)
struct link *self;
FILE *fp;
long id;
{
/*
  Read in the object from the file.
*/

  char *buf;
  int ds_version; 
  long error;
  
  link_SetID(self, link_UniqueID(self));


  if ((buf = ReadLine(fp)) == NULL) {
      return(link_SanelyReturnReadError(self, fp, id, dataobject_PREMATUREEOF));
  }
  if (strncmp(buf,"Datastream version:",19)) {
    return(link_SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT));
  }
  ds_version = atoi(buf+19);
  if ((ds_version < 1) || (ds_version > DS_VERSION)) {
    return(link_SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT));
  }
  free(buf);

  switch (ds_version) {
    case 1:
      if ((error = ReadOldFormat(self, fp, id)) != dataobject_NOREADERROR) {
	  return(error);	/* already read past \enddata */
      }
      break;
    case 2:
    case 3:
#ifdef PL8
      self->new_ds = 1;		/* preserve new data on write */
#endif /* PL8 */
      if ((buf = ReadLine(fp)) == NULL) {
	  return(link_SanelyReturnReadError(self, fp, id, dataobject_PREMATUREEOF));
      }
      if (strcmp(buf,"")!= 0 ) {
	  link_SetLink(self, buf);
      }
      free(buf);
      
      if (ds_version == 3) {
	  if ((buf = ReadLine(fp)) == NULL) {
	      return(dataobject_PREMATUREEOF);
	  }
	  link_SetPos(self, atol(buf));

	  free(buf);

	  if ((buf = ReadLine(fp)) == NULL) {
	      return(dataobject_PREMATUREEOF);
	  }
	  link_SetLen(self, atol(buf));
	  free(buf);
      }

      if ((buf = ReadLine(fp)) == NULL) {
	  return(link_SanelyReturnReadError(self, fp, id, dataobject_PREMATUREEOF));
      }
      if (strncmp(buf,"\\begindata{", 11)!= 0 ) {
	  return(link_SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT));
      }
      free(buf);
      super_Read(self, fp, id);

      break;

    default:
      fprintf(stderr, "ERROR: Link asked to read a format it doesn't know (ver. %d).  Shouldn't happen.\n", ds_version);
      return(link_SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT));
  }

  return(link_SanelyReturnReadError(self, fp, id, dataobject_NOREADERROR));
}



void
link__SetLink(self, link)
struct link *self;
char *link;
{
/*
  Set the link target (a filename) for this object.
*/

    if (link_GetRawLink(self)) free(link_GetRawLink(self));
    if (link) {
      self->link =  (char *)malloc(1+strlen(link));
      strcpy(link_GetRawLink(self), link);
    } else {
      self->link = NULL;
    }
    link_SetModified(self);
    link_NotifyObservers(self, 0);
}

void link__SetPos(self, pos)
struct link *self;
long pos; { /*  Set the link position for this object. */

    self->pos = pos;
    link_NotifyObservers(self, 0);
}

void link__SetLen(self, len)
struct link *self;
long len; { /*  Set the link length for this object. */

    self->len = len;
    link_NotifyObservers(self, 0);
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

  r = (char *)malloc(strlen(s)+strlen(t)+1);
  *r = '\0';
  strcpy(r,s);
  free(s);
  strcat(r,t);
  return r;
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

  
  result = (char *)malloc(1);
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
  return(NULL);
}

#ifdef PL8

static char *
EncodeFont(self)
struct link *self;
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
  myfontname = fontdesc_GetFontFamily(link_GetButtonFont(self));
  myfontsize = fontdesc_GetFontSize(link_GetButtonFont(self));
  myfonttype = fontdesc_GetFontStyle(link_GetButtonFont(self));
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

#endif /* PL8 */
