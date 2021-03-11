/* Copyright 1994 Carnegie Mellon University All rights reserved.
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

#include <andrewos.h>
#include <stdio.h>
#include <text.ih>
#include <unknown.eh>
#include <ctype.h>

static char keywordbuf[32];

char *unknown__GetRealClass(self)
struct unknown *self;
{
    return self->realclass?self->realclass:"unknown";
}

void unknown__SetRealClass(self, rclass)
struct unknown *self;
char *rclass;
{
    char buf[256];
    if(rclass && class_IsTypeByName(rclass, "unknown")) {
	/* assume that the first subobject in the 'unknown' dataobject set the real
	 class */
	self->wrapped=TRUE;
	return;
    }
    if(self->realclass) free(self->realclass);
    /* delete the old message */
    unknown_AlwaysDeleteCharacters(self, 0, unknown_GetLength(self));
    if(rclass) {
	self->realclass=NewString(rclass);
	/* tell the user that the inset is not supported */
	sprintf(buf, "? (see 'help unknown')\nThe %s inset is not supported in this version of AUIS.\n", self->realclass);
    } else {
	self->realclass=NULL;
	/* tell the user there was some inset here that wasn't supported but we don't know what
	 it was supposed to be. */
	sprintf(buf, "? (see 'help unknown')\nAn unidentified inset was here but could not be displayed.\n");
    }
    fprintf(stderr, "%s", buf);
    unknown_AlwaysInsertCharacters(self, 0, buf, strlen(buf));
}
    
boolean unknown__InitializeObject(c, self)
struct classheader *c;
struct unknown *self;
{
    unknown_SetReadOnly(self, TRUE);
    self->odata=NULL;
    self->wrapped=FALSE;
    self->realclass=NULL;
    return TRUE;
}

void unknown__FinalizeObject(c, self)
struct classheader *c;
struct unknown *self;
{
    if(self->odata) {
	text_Destroy(self->odata);
	self->odata==NULL;
    }
    if(self->realclass) {
	free(self->realclass);
	self->realclass=NULL;
    }
}

/* get a character from the file and put it in the text */
static int tgetc(self, file)
struct text *self;
FILE *file;
{
    int ch=getc(file);
    char charch=ch;
    if(ch!=EOF && self) text_AlwaysInsertCharacters(self, text_GetLength(self), &charch, 1);
    return ch;
}

/* put a character back in the file and remove it from the text. */
static int tungetc(self, ch, file)
struct text *self;
FILE *file;
{
    ungetc(ch, file);
    if(self) text_AlwaysDeleteCharacters(self, text_GetLength(self)-1, 1);
}

/* Read one unknown dataobject, putting the raw datastream into self, and checking
 that the enddata id matches id.  If lev is 0 or 1 the "real" class name for this object
 will be set on the unknown inset uself. */
static long RealRead(uself, self, file, id, lev)
struct unknown *uself;
struct text *self;
FILE *file;
long id;
int lev;
{
    int ch;
    int sawslash=0;
    int level=0;
    long res=0;
    while((ch=tgetc(self, file))!=EOF) {
	if(sawslash) {
	    char *p=keywordbuf;
	    sawslash=0;
	    if(ch=='{' || ch=='}' || ch=='\\') continue;
	    /* else look for keyword{data} */
	    *p++=ch;
	    while((ch=tgetc(self, file))!=EOF && ch!='{'  && ch!='\\' && ch!='}') {
		if(p-keywordbuf<sizeof(keywordbuf)-1) {
		    *p++=ch;
		}
	    }
	    if(ch==EOF) return dataobject_PREMATUREEOF;
	    if(ch=='\\' || ch=='}') {
		/* well this may have LOOKED like a keyword but somebody wrote
		 some strange datastream.  ungetting the \ or } and starting
		 the loop again will ensure that we don't miss an enddata. */
		tungetc(self, ch, file);
		continue;
	    }
	    *p='\0';
	    if(ch=='{') {
		if(strcmp(keywordbuf, "begindata")==0) {
		    /* read the start of a subobject, and then call ourselves recursively. */
		    long bid=0;
		    /* everything after the '{' and before the ',' is the classname */
		    while((ch=tgetc(self, file))!=EOF && ch!=',');
		    if(ch==EOF) return dataobject_PREMATUREEOF;
		    /* skip any space after the comma */
		    while((ch=tgetc(self, file))!=EOF && isspace(ch));
		    if(ch==EOF) return dataobject_PREMATUREEOF;
		    /* put back the first non-space character after the comma */
		    tungetc(self, ch, file);
		    /* read the dataobject id */
		    while((ch=tgetc(self, file))!=EOF && isdigit(ch)) bid=bid*10+(ch-'0');
		    if(ch==EOF) return dataobject_PREMATUREEOF;
		    /* discard any chunk after the id and before the '}' */
		    if(ch!='}') while((ch=tgetc(self, file))!=EOF && ch!='}');
		    if(ch==EOF) return dataobject_PREMATUREEOF;
		    /* if there is a newline after the '}' slurp it up, otherwise leave the character
		     for the subobject. */
		    if((ch=tgetc(self, file))!=EOF && ch!='\n') tungetc(self, ch, file);
		    else if(ch==EOF) return dataobject_PREMATUREEOF;
		    /* recurse to read the subobject */
		    res=RealRead(uself, self, file, bid, lev+1);
		    if(res!=0) return res;
		    continue;
		} else if(strcmp(keywordbuf, "enddata")==0) {
		    /* exit this subobject, checking that the dataobject id matches the
		     begindata.  If a mismatch is detected just warn the user. */
		    int eid=0;
		    char cbuf[100];
		    long cpos=text_GetLength(self), cepos;
		    /* scan the classname */
		    while((ch=tgetc(self, file))!=EOF && ch!=',');
		    if(lev==0 || lev==1) {
			/* if this is the top level object or the one directly below it
			 remember the class name so we  can tell the user about it.
			 If the name is "unknown" then we will assume that the the
			 second level subobject set the name appropriately */
			cepos=text_GetLength(self)-1;
			if(ch==',' && cepos>cpos) cepos--;
			if(cepos-cpos+1>sizeof(cbuf)) {
			    cepos=cpos+sizeof(cbuf)-1;
			}
			text_CopySubString(self, cpos, cepos-cpos+1, cbuf, FALSE);
			unknown_SetRealClass(uself, cbuf);
		    }
		    if(ch==EOF) return dataobject_PREMATUREEOF;
		    /* skip the space after the comma in \enddata{classname, 328} */
		    while((ch=tgetc(self, file))!=EOF && isspace(ch));
		    if(ch==EOF) return dataobject_PREMATUREEOF;
		    /* put back the first non-space character, it should be the first digit
		     of the dataobject id. */
		    tungetc(self, ch, file);
		    /* read the enddata id */
		    while((ch=tgetc(self, file))!=EOF && isdigit(ch)) eid=eid*10+(ch-'0');
		    if(ch==EOF) return dataobject_PREMATUREEOF;
		    /* slurp up garbage after the id and before the '}' */
		    if(ch!='}') while((ch=tgetc(self, file))!=EOF && ch!='}');
		    if(ch==EOF) return dataobject_PREMATUREEOF;
		    /* slurp up the newline if it is there. */
		    if((ch=tgetc(self, file))!=EOF && ch!='\n') tungetc(self, ch, file);
		    else if(ch==EOF) return dataobject_PREMATUREEOF;
		    if(eid!=id) {
			fprintf(stderr, "warning: %s__Read: enddata id %d doesn't match begindata id %d.\n", class_GetTypeName(self), eid, id);
		    }
		    return dataobject_NOREADERROR;
		}
	    }


	} else {
	    if(ch=='\\') sawslash=1;
	}
    }
    return dataobject_PREMATUREEOF;
}

/* this function will read and ignore any dataobject. */
long unknown__Read(self, file, id)
struct unknown *self;
FILE *file;
long id;
{
    long ret;
    if(self->odata==NULL) {
	/* make the text to keep the raw datastream, make it readonly so it is less likely to
	 be modified by the user if they ever get their hands on it. */
	self->odata=text_New();
	if(self->odata) text_SetReadOnly(self->odata, TRUE);
    } else {
	/* remove any old data left over from a previous read. */
	if(text_GetLength(self->odata)) text_AlwaysDeleteCharacters(self->odata, 0, text_GetLength(self->odata));
    }
    /* actually read all the data up to the appropriate enddata */
    ret=RealRead(self, self->odata, file, id, 0);
    if(self->odata) {
	char buf[256];
 /* if the unknown object was at the top level when we read it in, we
	     now wrap it in the unknown inset since upon writing the data contained
	     in it may become invalid.  This would occur for example if it contains
	     references to dataobject_UniqueID's of objects outside itself.  There is
	     also the potential for conflict between id's in the object and objects
	     outside. Manual intervention will be required to */
	if(!self->wrapped) {
	    /* put in the begindata which we missed the first time because the caller of __Read
	     had to slurp it up. */
	    sprintf(buf, "\\begindata{%s,%ld}\n", unknown_GetRealClass(self), id);
	    text_AlwaysInsertCharacters(self->odata, 0, buf, strlen(buf));
	} else {
	    /* it was already wrapped, so we search for the \enddata{unknown,9282}\n that was stuffed into the
	     text and delete it, the write routine will put in the right begindata/enddata pair around the unknown
	     inset's begindata/enddata. */
	    long pos=text_GetLength(self->odata)-1;
	    while(pos>=0) {
		if(text_GetChar(self->odata, pos)=='\\') break;
		pos--;
	    }
	    if(pos>=0) text_AlwaysDeleteCharacters(self->odata, pos, text_GetLength(self->odata)-pos);
	    unknown_AlwaysDeleteCharacters(self, 0, unknown_GetLength(self));
	    /* display a message to the user indicating that the data contained here may have been
	     corrupted by a write as an unknown inset and point them at the documentation. */
	    sprintf(buf, "? (see 'help unknown') The %s inset included here may have been corrupted.\nSee the help file for 'unknown' for recovery options.\n", unknown_GetRealClass(self));
	    unknown_AlwaysInsertCharacters(self, 0, buf, strlen(buf));
	}
    }
}  

/* wrap the unknown inset in an 'unknown' inset to protect future readers from a possibly corrupted
 version of the data. */
long unknown__Write(self, file, writeid, level)
struct unknown *self;
FILE *file;
long writeid;
int level;
{
    if(unknown_GetWriteID(self)!=writeid) {
	long pos=0, len;
	/* output the raw inset data wrapped in an 'unknown' dataobject. */
	fprintf(file, "\\begindata{%s,%ld}\n", class_GetTypeName(self), unknown_UniqueID(self));
	if(self->odata) {
	    /* write out the data verbatim! */
	    while(pos<text_GetLength(self->odata)) {
		char *buf=text_GetBuf(self->odata, pos, text_GetLength(self->odata)-pos, &len);
		if(len==0 || fwrite(buf, 1, len, file)!=len) return -1;
		pos+=len;
	    }
	} else fprintf(file, "\n");
	fprintf(file, "\\enddata{%s,%ld}\n", class_GetTypeName(self), unknown_UniqueID(self));
    }
    return unknown_UniqueID(self);
}
