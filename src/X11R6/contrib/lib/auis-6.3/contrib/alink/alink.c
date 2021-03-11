/* ********************************************************************** *\
 * Copyright (c) AT&T Bell Laboratories	1990  -	All Rights Reserved	  *
 ****************************************************************************
*        Copyright AT&T Bell Laboratories - All Rights Reserved        *
*                                                                          *
* Permission to use, copy, modify, and distribute this software and its    *
* documentation for any purpose and without fee is hereby granted,         *
* provided that the above copyright notice appear in all copies and        *
* that both that copyright notice and this permission notice appear in     *
* supporting documentation, and that the name of IBM not be used in        *
* advertising or publicity pertaining to distribution of the software      *
* without specific, written prior permission.                              *
*                                                                          *
* IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL IBM *
* BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY      *
* DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER  *
* IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING   *
* OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.    *
****************************************************************************

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/alink/RCS/alink.c,v 1.6 1994/04/26 22:31:56 rr2b Exp $";
#endif


#include <stdio.h>
#include <andrewos.h>
#include <environ.ih>
#include <alink.eh>

/* Defined constants and macros */
#define MAX_LINE_LENGTH 70
#define DS_VERSION 1 /* datastream version */

/* External declarations */
extern char *malloc();

/* Forward Declarations */
static void WriteLine();
static char *GlomStrings(), *ReadLine(), *EncodeFont();
static char *ReadAudio();
static void WriteAudio();

/* Global variables */

boolean
alink__InitializeClass(c)
struct classheader *c; {
/* 
  Initialize all the class data.
*/
  return(TRUE);
}

boolean
alink__InitializeObject(c, self)
struct classheader *c;
struct alink *self; {
/*
  Inititialize the object instance data.
*/

    alink_SetText(self,"Alink (v10)");
    self->label_set = 0;
    self->audio = NULL;
    self->audioLength = 0;
    alink_SetStyle(self,environ_GetProfileInt("alinkstyle", 2));
    return(TRUE);
}

void
alink__FinalizeObject(c, self)
struct classheader *c;
struct alink *self; {
/*
  Finalize the object instance data.
*/
  if (self->audio) free(alink_GetAudio(self));
  self->audio = NULL;
  return;
}

long
alink__Write(self, fp, id, level)
struct alink *self;
FILE *fp;
long id;
int level; {
/*
  Write the object data out onto the datastream.

  Sample output from datastream version 1:
    \begindata{alink, 1234567}
    Datastream version: 1
    This is my button label
    1234
    <1234 bytes of audio in a64>
    \enddata{alink, 1234567}

*/

  long uniqueid = alink_UniqueID(self);
  if (id != alink_GetWriteID(self)) {
    /* New Write Operation */
    alink_SetWriteID(self, id);
    fprintf(fp, "\\begindata{%s,%d}\nDatastream version: %d\n",
	    class_GetTypeName(self), uniqueid, DS_VERSION);
    WriteLine(fp, alink_GetText(self) ? alink_GetText(self) : "");
    fprintf(fp, "%ld\n", alink_GetAudioLength(self));
    WriteAudio(fp, alink_GetAudioLength(self), alink_GetAudio(self));
    fprintf(fp, "\\enddata{%s,%d}\n",
	    class_GetTypeName(self), uniqueid);
  }
  return(uniqueid);
}


long
alink__Read(self, fp, id)
struct alink *self;
FILE *fp;
long id;
{
/*
  Read in the object from the file.
*/

  char *buf, buf2[255];
  int ds_version;
  long len;
  
  alink_SetID(self, alink_UniqueID(self));


  if ((buf = ReadLine(fp)) == NULL) {
    return(dataobject_PREMATUREEOF);
  }
  if (strncmp(buf,"Datastream version:",19)) {
    return(dataobject_BADFORMAT);
  }
  ds_version = atoi(buf+19);
  if (ds_version != 1) {
    return(dataobject_BADFORMAT);
  }
  free(buf);

  if ((buf = ReadLine(fp)) == NULL) {
    return(dataobject_PREMATUREEOF);
  }

  if (strcmp(buf,"")!= 0 ) {
    alink_SetText(self, buf);
  }
  free(buf);

  do {
      if ((buf = ReadLine(fp)) == NULL) {
	  return(dataobject_PREMATUREEOF);
      }
  } while (buf[0] == '\n' || !buf[0]);
  if (strcmp(buf,"")!= 0 ) {
      len = atol(buf);
  }
  free(buf);

  if ((buf = ReadAudio(fp, len)) == NULL) {
    return(dataobject_PREMATUREEOF);
  }
  if (strcmp(buf,"")!= 0 ) {
    alink_SetAudio(self, len, buf);
  }
  free(buf);

  if ((buf = ReadLine(fp)) == NULL) {
    return(dataobject_PREMATUREEOF);
  }

  sprintf(buf2, "\\enddata{%s,%ld}",
	  class_GetTypeName(self), id);
  if (strcmp(buf, buf2)) {
    free(buf);
    return(dataobject_MISSINGENDDATAMARKER);
  }
  free(buf);
  return(dataobject_NOREADERROR);
}


void
alink__SetText(self, txt)
struct alink *self;
char *txt;
{
/*
  Set the text label for this object.
*/

    if (txt) {
      self->label_set = 1;
    } else {
      self->label_set = 0;
    }
    super_SetText(self, txt);
}

void
alink__SetAudio(self, len, audio)
struct alink *self;
long len;
char *audio;
{
/*
  Set the audio data for this object.
*/

    if (alink_GetAudio(self)) free(alink_GetAudio(self));
    if (audio) {
      self->audio =  (char *)malloc(len);
      self->audioLength = len;
      bcopy(audio, alink_GetAudio(self), alink_GetAudioLength(self));
    } else {
      self->audioLength = 0;
      self->audio = NULL;
    }
}

long A64L(s)
char *s; {
    int i;
    long out = 0;
    for (i = 5; i >= 0; i--) {
	char c = s[i];
	int n;
	if (c == '|') c = '\\';
	n = (int) c - ' ';
	out = out << 6;
	out += n;
    }
    return out;
}

char *L64A(l)
long l; {
    int i;
    static char out[6];
    for (i = 0; i < 6; i++) {
	int n = l & 0x3f;
	char c = (char) n + ' ';
	l = l >> 6;
	if (c == '\\') c = '|';
	out[i] = c;
    }
    return out;
}

static void
WriteAudio(fp, len, audio)
FILE *fp;
long len; 
char *audio; {
/*
  Output the audio onto the datastream.
*/
    char *buf;
    long llen = len / 4;
    long buflen = llen * 6  + llen / 10;
    long *lbuf;
    long bptr = 0;
    int i;
    int lcount = 1;

    buf = malloc(buflen);
    if (buf == NULL) return;
    lbuf = (long *) audio;
    for (i = 0; i < llen; i++) {
	char *s = L64A(lbuf[i]);
	bcopy(s, buf + bptr, 6);
	bptr += 6;
	if (lcount++ == 10) {
	    bcopy("\n", buf+bptr, 1);
	    bptr++;
	    lcount = 1;
	}
    }
    fwrite(buf, 1, buflen, fp);
    fprintf(fp, "\n");
    free(buf);
}

static void
WriteLine(f, l)
FILE *f;
char *l; {
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
ReadAudio(fp, len)
FILE *fp;
long len; {
    char *buf;
    long *lbuf;
    long llen = len / 4;
    long buflen = llen * 6  + llen / 10;
    int i;
    int bptr = 0;
    int lcount = 1;

    buf = malloc(buflen);
    if (buf == NULL) return NULL;
    if (fread(buf, 1, buflen, fp) != buflen) {
	free(buf);
	return NULL;
    }
    lbuf = (long *) malloc(len);
    if (lbuf == NULL) {
	free(buf);
	return NULL;
    }
    for (i = 0; i < llen; i++) {
	lbuf[i] = A64L(buf + bptr);
	bptr += 6;
	if (lcount++ == 10) {
	    bptr++;
	    lcount = 1;
	}
    }
    /* transfer to lbuf */
    fread(buf, 1, 1, fp);	/* read the newline */
    if (buf[0] != '\n') {
	free(buf);
	return NULL;
    }
    return (char *) lbuf;
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

long alink__WriteOtherFormat(self, file, writeID, level, usagetype, boundary)
struct alink *self;
FILE *file;
long writeID;
int level;
int usagetype;
char *boundary;
{
    char *s, *end;
    unsigned char c1, c2, c3;
    int ct=0;

    if (self->header.dataobject.writeID == writeID)  return(self->header.dataobject.id);
    self->header.dataobject.writeID = writeID;
    
    fprintf(file, "\n--%s\nContent-type: audio/basic\nContent-Transfer-Encoding: base64\n", boundary);
    if (self->label_set) {
	fprintf(file, "Content-Description: %s\n", alink_GetText(self));
    }
    fputs("\n", file);
    s=self->audio;
    end = self->audio + self->audioLength; 
    while (s < end) {
	c1 = *s++;
	c2 = *s++;
	if (s > end) {
            output64chunk(c1, 0, 0, 2, file);
	} else {
	    c3 = *s++;
            if (s > end) {
                output64chunk(c1, c2, 0, 1, file);
            } else {
                output64chunk(c1, c2, c3, 0, file);
            }
        }
        ct += 4;
        if (ct > 71) {
            putc('\n', file);
            ct = 0;
        }
    }
    if (ct) putc('\n', file);
    return(self->header.dataobject.id);
}

boolean alink__ReadOtherFormat(self, file, fmt, encoding, desc)
struct alink *self;
FILE *file;
char *fmt;
char *encoding;
char *desc;
{
    char TmpFile[250];
    FILE *tmpfp = NULL;
    int size;
    static int tmpfilectr = 1;

    if (strcmp(fmt, "audio/basic")) return(FALSE);
    /* Need to decode base64 or q-p here */
    if (!strncmp(encoding, "base64", 6)
	 || !strncmp(encoding, "quoted-printable", 16)) {
	sprintf(TmpFile, "/tmp/rastxwd.%d.%d", getpid(), tmpfilectr++);
	tmpfp = fopen(TmpFile, "w");
	if (!tmpfp) return(FALSE);
	if (!strncmp(encoding, "base64", 6)) {
	    size = from64(file, tmpfp);
	} else {
	    size = fromqp(file, tmpfp);
	}
	fclose(tmpfp);
	tmpfp = fopen(TmpFile, "r");
	if (!tmpfp) return(FALSE);
	file = tmpfp;
    } else {
	/* ACK!  NEED TO SET SIZE.  PROBABLY WON'T EVER HAPPEN, BUT... */
	int pos, c;
	size = 0;
	pos = ftell(file);
	while ((c=getc(file)) != EOF) ++size;
	fseek(file, pos, 0);
    }
    self->audio = malloc(size + 10);
    if (self->audio) {
	self->audioLength = fread(self->audio, sizeof(char), size, file);
    }
    if (tmpfp) {
	fclose(tmpfp);
	unlink(TmpFile); 
    }
    if (desc && *desc) {
	alink_SetText(self, desc);
    }
    if (self->audio) {
	return(TRUE);
    } else {
	return (FALSE);
    }
}

