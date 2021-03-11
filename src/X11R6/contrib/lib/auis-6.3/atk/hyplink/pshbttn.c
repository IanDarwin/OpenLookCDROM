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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/hyplink/RCS/pshbttn.c,v 1.17 1993/01/08 16:31:54 rr2b R6tape $";
#endif

#include <stdio.h>
#include <sys/param.h>
#include <util.h>
#include <andrewos.h>
#include <atom.ih>
#include <cursor.ih>
#include <environ.ih>
#include <fontdesc.ih>
#include <graphic.ih>
#include <observe.ih>
#include <pshbttn.eh>

/* Defined constants and macros */
#define DS_VERSION 2 /* datastream version */

#define MAX_LINE_LENGTH 70  /* can't be less than 6 */


/* External declarations */

/* Forward Declarations */
static void WriteLine();
static char *GlomStrings(), *ReadLine(), *EncodeFont();

/* Global variables */
static struct atom *pushbutton_trigger;


boolean
pushbutton__InitializeClass(c)
struct classheader *c;
{
    /* 
      Initialize all the class data.
      */
    if ((pushbutton_trigger = atom_Intern("buttonpushed")) == NULL) return(FALSE);
    return(TRUE);
}


boolean
pushbutton__InitializeObject(c, self)
struct classheader *c;
struct pushbutton *self;
{
/*
  Inititialize the object instance data.
*/

    self->text = NULL;
    observable_DefineTrigger(self, pushbutton_trigger);

    self->style = environ_GetProfileInt("pushbuttonstyle", 2);

    self->foreground_name = environ_GetProfile("foreground");
    if(self->foreground_name) self->foreground_name=NewString(self->foreground_name);
    if ((self->foreground_name == NULL) || (strcmp(self->foreground_name, "") == 0)) {
	if (self->foreground_name != NULL) free(self->foreground_name);
	self->foreground_name = NewString("black");
    } else {
	self->foreground_name = NewString(self->foreground_name);
    }
    self->foreground_color[0] = 0;
    self->foreground_color[1] = 0;
    self->foreground_color[2] = 0;

    self->background_name = environ_GetProfile("background");
    if(self->background_name) self->background_name=NewString(self->background_name);
    if ((self->background_name == NULL) || (strcmp(self->background_name, "") == 0)) {
	if (self->background_name != NULL) free(self->background_name);
	self->background_name = NewString("white");
    } else {
	self->background_name = NewString(self->background_name);
    }
    self->background_color[0] = 0;
    self->background_color[1] = 0;
    self->background_color[2] = 0;

    self->myfontdesc = NULL;
    return(TRUE);
}


void
pushbutton__FinalizeObject(c, self)
struct classheader *c;
struct pushbutton *self;
{
/*
  Finalize the object instance data.
*/
  if (self->text != NULL) {
    free(self->text);
    self->text = NULL;
  }
  if (self->foreground_name != NULL) {
      free(self->foreground_name);
      self->foreground_name = NULL;
  }
  if (self->background_name != NULL) {
      free(self->background_name);
      self->background_name = NULL;
  }
  return;
}


static void
pushbutton__WriteDataPart(self, fp)
struct pushbutton *self;
FILE *fp;
{
/*
  Write the object data out onto the datastream.
*/
  char buf[100], *encfont;

  WriteLine(fp, self->text ? self->text : "");
  sprintf(buf,"%d", self->style); /* *BUG* how do we tell a defaulted 
				     style from a set style? */
  WriteLine(fp, buf);

  encfont = self->myfontdesc ? EncodeFont(self) : NULL;
  WriteLine(fp, encfont ? encfont : "");
  if (encfont) {
    free(encfont);
    encfont = NULL;
  }

#ifdef PL8
  if (!(self->new_DS)) return;
#endif /* PL8 */

  if (self->foreground_name != NULL) {
      sprintf(buf, "%s", self->foreground_name);
  } else {
      sprintf(buf, "0x%02x%02x%02x",
	      self->foreground_color[0],
	      self->foreground_color[1],
	      self->foreground_color[2]);
  }
  WriteLine(fp, buf);

  if (self->background_name != NULL) {
      sprintf(buf, "%s", self->background_name);
  } else {
      sprintf(buf, "0x%02x%02x%02x",
	      self->background_color[0],
	      self->background_color[1],
	      self->background_color[2]);
  }
  WriteLine(fp, buf);

}



long
pushbutton__Write(self, fp, id, level)
struct pushbutton *self;
FILE *fp;
long id;
int level;
{
/*
  Write the object data out onto the datastream.

  Sample output from datastream version 1:
    \begindata{pushbutton, 1234567}
    Datastream version: 1
    This is my button label     -- label
    2                           -- style
    andy12b                     -- font
    \enddata{pushbutton, 1234567}

  Sample output from datastream version 2:
    \begindata{pushbutton, 1234567}
    Datastream version: 2
    This is my button label     -- label
    2                           -- style
    andy12b                     -- font
    black                       -- foreground color
    0xFFFFFF                    -- background color, RGB representation
    \enddata{pushbutton, 1234567}

*/

  long uniqueid = pushbutton_UniqueID(self);

  if (id != pushbutton_GetWriteID(self)) {
    /* New Write Operation */
    pushbutton_SetWriteID(self, id);
    fprintf(fp, "\\begindata{%s,%d}\nDatastream version: %d\n",
	    class_GetTypeName(self), 
	    uniqueid, 
#ifndef PL8
	    DS_VERSION);
#else /* PL8 */
            self->new_DS?DS_VERSION:1);	/* lie, if we must*/
#endif /* PL8 */

    pushbutton__WriteDataPart(self, fp);

    fprintf(fp, "\\enddata{%s,%d}\n", class_GetTypeName(self), uniqueid);
  }
  return(uniqueid);
}


static long
pushbutton__ReadDataPart(self, fp, dsversion)
struct pushbutton *self;
FILE *fp;
{
/*
  Read in the object from the file.
*/
  char *buf;
  unsigned char rgb[3];
  
  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  if (strcmp(buf,"")!= 0 )
    pushbutton_SetText(self, buf);
  free(buf);

  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  if (strcmp(buf,"")!= 0 )
    pushbutton_SetStyle(self, atoi(buf));
  free(buf);

  if ((buf = ReadLine(fp)) == NULL)
    return(dataobject_PREMATUREEOF);
  if (strcmp(buf,"")!= 0) {
    char name[MAXPATHLEN];
    long style, size;
    if (fontdesc_ExplodeFontName(buf,name,sizeof(name), &style, &size)) {
      pushbutton_SetButtonFont(self,fontdesc_Create(name,style,size));
    }
  }
  free(buf);

  if (dsversion >= 2) {
      if ((buf = ReadLine(fp)) == NULL)
	return(dataobject_PREMATUREEOF);
      if (strcmp(buf,"")!= 0 ) {
	  if ((strncmp(buf, "0x", 2) != 0)
	      &&(strncmp(buf, "0X", 2) != 0)) {
	      pushbutton_SetFGColor(self, buf, 0, 0, 0);
	    } else {
		pushbutton_ParseRGB(self, buf, rgb);
		pushbutton_SetFGColor(self, NULL, rgb[0], rgb[1], rgb[2]);
	    }
      }
      free(buf);
	  
      if ((buf = ReadLine(fp)) == NULL)
	return(dataobject_PREMATUREEOF);
      if (strcmp(buf,"")!= 0 ) {
	  if (strncmp(buf, "0x", 2) != 0)  {
	      pushbutton_SetBGColor(self, buf, 0, 0, 0);
	    } else {
		pushbutton_ParseRGB(self, buf, rgb);
		pushbutton_SetBGColor(self, NULL, rgb[0], rgb[1], rgb[2]);
	    }
      }
      free(buf);
  }

  return(dataobject_NOREADERROR);
}



static long
pushbutton_SanelyReturnReadError(self, fp, id, code)
     struct pushbutton *self;
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
pushbutton__Read(self, fp, id)
struct pushbutton *self;
FILE *fp;
long id;
{
/*
  Read in the object from the file.
  (BUG) Doesn't set the font.
*/
  char *buf;
  long result;
  int dsversion;
  
  pushbutton_SetID(self, pushbutton_UniqueID(self));

  if ((buf = ReadLine(fp)) == NULL)
    return(pushbutton_SanelyReturnReadError(self, fp, id, dataobject_PREMATUREEOF));
  if (strncmp(buf,"Datastream version:",19))
    return(pushbutton_SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT));
  if ((dsversion = atoi(buf+19)) > DS_VERSION)	/* datastream version */
    return(pushbutton_SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT));
  if (dsversion < 1)
    return(pushbutton_SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT));
  free(buf);
#ifdef PL8
  if (dsversion > 1) self->new_DS = TRUE; /* preserve information */
#endif /* PL8 */

  if ((result = pushbutton__ReadDataPart(self, fp, dsversion)) != dataobject_NOREADERROR)
    return(pushbutton_SanelyReturnReadError(self, fp, id, result));

  return(pushbutton_SanelyReturnReadError(self, fp, id, dataobject_NOREADERROR));
}


void
pushbutton__SetText(self, txt)
struct pushbutton *self;
char *txt;
{
/*
  Set the text label for this object.
*/

    if (self->text) {
      free(self->text);
      self->text = NULL;
    }
    if (txt)
      if (self->text = (char *)malloc(1+strlen(txt)))
	strcpy(self->text, txt);
    pushbutton_SetModified(self);
    pushbutton_NotifyObservers(self, observable_OBJECTCHANGED);
}


void
pushbutton__SetStyle(self, stylecode)
struct pushbutton *self;
int stylecode;
{
/*
  Set the style code for this object.
*/

    self->style = stylecode;
    pushbutton_SetModified(self);
    pushbutton_NotifyObservers(self,observable_OBJECTCHANGED);
}


void
pushbutton__SetButtonFont(self, f)
struct pushbutton *self;
struct fontdesc *f;
{
/*
  Set the font descriptor for this object.
*/

    /* DON'T EVER FREE FONTS! */
    /* if (self->myfontdesc) free(self->myfontdesc); */

    self->myfontdesc = f;
    pushbutton_SetModified(self);
    pushbutton_NotifyObservers(self,observable_OBJECTCHANGED);
}



void
pushbutton__SetFGColor(self, name, red, green, blue)
     struct pushbutton *self;
     char *name;
     int red, green, blue;
{
    /*
      Set the foreground color for this object.
    */

    if (self->foreground_name != NULL) free(self->foreground_name);
    self->foreground_name = NULL;

    if (name != NULL) {
	self->foreground_name = NewString(name);
	self->foreground_color[0] = 0;
	self->foreground_color[1] = 0;
	self->foreground_color[2] = 0;
    } else {
	self->foreground_color[0] = red;
	self->foreground_color[1] = green;
	self->foreground_color[2] = blue;
    }
    
    pushbutton_SetModified(self);
    pushbutton_NotifyObservers(self,observable_OBJECTCHANGED);
#ifdef PL8
    self->new_DS = TRUE; /* preserve information */
#endif /* PL8 */
}


void
pushbutton__SetBGColor(self, name, red, green, blue)
     struct pushbutton *self;
     char *name;
     int red, green, blue;
{
    /*
      Set the background color for this object.
    */

    if (self->background_name != NULL) free(self->background_name);
    self->background_name = NULL;

    if (name != NULL) {
	self->background_name = NewString(name);
	self->background_color[0] = 0;
	self->background_color[1] = 0;
	self->background_color[2] = 0;
    } else {
	self->background_color[0] = red;
	self->background_color[1] = green;
	self->background_color[2] = blue;
    }
    
    pushbutton_SetModified(self);
    pushbutton_NotifyObservers(self,observable_OBJECTCHANGED);
#ifdef PL8
    self->new_DS = TRUE; /* preserve information */
#endif /* PL8 */
}


char *
pushbutton__GetFGColor(self, rgb_vect)
     struct pushbutton *self;
     unsigned char rgb_vect[];
{
    /*
      Return the foreground color for this object.
    */
    
    rgb_vect[0] = self->foreground_color[0];
    rgb_vect[1] = self->foreground_color[1];
    rgb_vect[2] = self->foreground_color[2];

    return(self->foreground_name);
}


char *
pushbutton__GetBGColor(self, rgb_vect)
     struct pushbutton *self;
     unsigned char rgb_vect[];
{
    /*
      Return the background color for this object.
    */
    
    rgb_vect[0] = self->background_color[0];
    rgb_vect[1] = self->background_color[1];
    rgb_vect[2] = self->background_color[2];

    return(self->background_name);
}


static int
htoin(s, n)
     char *s;
     int n;
{
    int i, t;

    for(i = 0, t = 0; i < n; ++i) {
	t *= 16;
	t += ((s[i]>='a')
	      ?(s[i]-'a'+10)
	      :((s[i]>='A')
		?(s[i]-'A'+10)
		:(s[i]-'0')));
    }

    return(t);
}


void
pushbutton__ParseRGB(self, rgb_string, rgb_vect)
     struct pushbutton *self;
     char *rgb_string;
     unsigned char rgb_vect[];
{
    /*
      Return the background color for this object.
    */
    
    if ((rgb_string != NULL)
	&& ((strncmp(rgb_string, "0x", 2) == 0)
	    || (strncmp(rgb_string, "0X", 2) == 0))
	&& (strlen(rgb_string) == 8)) {
	rgb_vect[0] = htoin(rgb_string+2,2);
	rgb_vect[1] = htoin(rgb_string+4,2);
	rgb_vect[2] = htoin(rgb_string+6,2);
	}
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
struct pushbutton *self;
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

