/* File compress.c created by R S Kemmetmueller
  
   compress: a dataobject to store a hidden region of text. */
/* Copyright 1992, 1994 Carnegie Mellon University and IBM.  All rights reserved.
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/compress.c,v 1.5 1994/02/22 20:14:18 rr2b Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <envrment.ih>
#include <viewref.ih>
#include <text.ih>
#include <toolcnt.h>

#include "compress.eh"

#define TEXT_VIEWREFCHAR '\377'

boolean compress__InitializeClass(ClassID)
struct classheader *ClassID;
{
    return TRUE;
}

boolean compress__InitializeObject(ClassID,self)
struct classheader *ClassID;
struct compress *self;
{
    self->lines= 0;
    self->loc= -1;
    compress_SetExportEnvironments(self, TRUE);
    return TRUE;
}

void compress__FinalizeObject(ClassID,self)
struct classheader *ClassID;
struct compress *self;
{
    return;
}

/* GetModified always returns 0, so that the parent document won't realize one of its subobjects has changed when a region is compressed. */
long compress__GetModified(self)
struct compress *self;
{
    return 0;
}

/* override */
/* GetPosForLine "tricks" everyone who calls it by ALSO counting newlines inside NESTED compress objects */
long compress__GetPosForLine(self, line)
struct compress *self;
long line;
{
    long i=1, pos=0, len=compress_GetLength(self);
    int ch;

    while (i < line) {
	if (pos >= len)
            break;
	if ((ch=compress_GetChar(self,pos)) == '\012')
	    i++;
	else if (ch==TEXT_VIEWREFCHAR) { /* this check is quicker than letting _IsThere figure it out */
	    struct compress *isthere=compress_IsThere((struct text *)self,pos);
	    if (isthere)
		i+= compress_GetLines(isthere)-1; /* -1 to compensate for \n after box being counted */
	}
        pos++;
    }
    return pos;
}

/* override */
/* GetLineForPos "tricks" everyone who calls it by ALSO counting newlines inside NESTED compress objects */
long compress__GetLineForPos(self, pos)
struct compress *self;
long pos;
{
    long line=1;
    int ch;

    while (--pos >= 0)
	if ((ch=compress_GetChar(self,pos)) == '\012')
	    line++;
	else if (ch==TEXT_VIEWREFCHAR) { /* this check is quicker than letting _IsThere figure it out */
	    struct compress *isthere=compress_IsThere((struct text *)self,pos);
	    if (isthere)
		line+= compress_GetLines(isthere)-1; /* -1 to compensate for \n after box being counted */
	}
    return line;
}

/* quickAddView is a ripoff of text_AlwaysAddView, but with all but the bare necessities ripped out of it. It also changes env_WRAPView to env_INSERTView, which is a zillion times faster, but doesn't allow nesting. */
/* text_AlwaysInsertFile calls dictionary_Insert. I have no idea what that does. I don't call it when I insert a compress object/view, and it seems to work OK, so I just left it out. -RSK*/
struct environment *quickAddView(self, pos, viewtype, compressobj)
struct text *self;
long pos;
char *viewtype;
struct compress *compressobj;
{
    struct viewref *newviewref=viewref_Create(viewtype, (struct dataobject *)compressobj);
    char c=TEXT_VIEWREFCHAR;
    register struct environment *newenv, *root=self->rootEnvironment;

    viewref_AddObserver(newviewref,self);
    text_AlwaysInsertCharacters(self, pos, &c, 1);
    if (root && environment_GetEnclosing(root,pos)!=root) /* can't nest environments! (in source code, anyway) */
	environment_Remove(self->rootEnvironment, pos,1, environment_Style, TRUE);
    newenv= environment_InsertView(self->rootEnvironment, pos,newviewref,TRUE);
    if (newenv) {
	environment_SetLength(newenv,1);
	environment_SetStyle(newenv, FALSE,FALSE);
    }
    return newenv;
}

void compress__Compress(ClassID, txt, pos,len)
struct classheader *ClassID;
struct text *txt;
long pos, len;
{
    struct compress *self;
    if (len<1) return;
    self= compress_New();
    if (compress_GetLength(self) > 0)
	compress_Clear(self); /* just to make sure */
    if (text_GetChar(txt,pos+len-1) == '\n')
	--len; /* don't grab trailing newline, it'd look ugly */
    compress_CompressInFront(self, txt,pos,len);
    quickAddView(txt, pos, "compressv",self);
    text_NotifyObservers(txt, 0);
}

/* IsThere returns a pointer to the compress inset at that position, if there IS one there, or returns NULL if there isn't one there */
struct compress *compress__IsThere(ClassID, txt, pos)
struct classheader *ClassID;
struct text *txt;
long pos;
{
    if (text_GetChar(txt,pos)==TEXT_VIEWREFCHAR) {
	struct environment *env=environment_GetInnerMost(txt->rootEnvironment, pos);
	if (env->type == environment_View) {
	    struct dataobject *inset= env->data.viewref->dataObject;
	    if (class_IsTypeByName(class_GetTypeName(inset), "compress"))
		return (struct compress *)inset;
	}
    }
    return NULL;
}

/* CompressInFront compresses the specified region by removing it from the parent document and stashing it into itself (in the front). */
void compress__CompressInFront(self,txt,pos,len)
struct compress *self;
struct text *txt;
long pos,len;
{
    compress_AlwaysCopyText(self,0, txt,pos,len);
    text_AlwaysDeleteCharacters(txt, pos,len);
}

/* CompressInBack compresses the specified region by removing it from the parent document and stashing it into itself (at the end). */
void compress__CompressInBack(self,txt,pos,len)
struct compress *self;
struct text *txt;
long pos,len;
{
    compress_AlwaysCopyText(self,compress_GetLength(self), txt,pos,len);
    text_AlwaysDeleteCharacters(txt, pos,len);
}

/* global pointers that store a list of compresses to run through, immediately after doupdate is done */
static struct compress **compresslist=NULL, **endcompresslist=NULL;

static boolean doupdate(self,txt,pos,env)
struct compress *self;
struct text *txt;
long pos;
struct environment *env;
{
    boolean retval=FALSE;
    if (env->type == environment_View){
	struct viewref *vr=env->data.viewref;
	char *name=class_GetTypeName(vr->dataObject);
	if (self == NULL) {
	    if (!class_IsTypeByName(name,"compress"))
		return FALSE;
	    self= (struct compress *)vr->dataObject;
	    if (compresslist!=NULL && compresslist<endcompresslist)
		*compresslist++= self;
	}
	else {
	    if (self != (struct compress *)vr->dataObject)
		return FALSE;
	    retval= TRUE;
	}
	/* we have a compress */
	self->loc= pos;
    }
    return retval;
}

/* DoAll updates all the data held by the compresses in this text object, and will enumerate through them as well if callBack is non-NULL */
static void DoAll(txt,callBack)
struct text *txt;
boolean (*callBack)();
{
#define MAXCOMPRESSES 1024 /* MAXCOMPRESSES just determines the maximum number that can have the callBack() executed on them. ALL of the compresses in the text will have their positions updated, no matter HOW many there are, so don't worry about it. */
    struct compress *compresses[MAXCOMPRESSES];
    compresslist= compresses;
    endcompresslist= compresses + MAXCOMPRESSES;
    if (txt) {
	text_EnumerateEnvironments(txt, 0,text_GetLength(txt), doupdate,NULL);
	if (callBack != NULL)
	    /* do in reverse order. this makes sure the positions stay valid if we're decompressing as we go */
	    while (--compresslist >= compresses)
		(*(callBack))(*compresslist,txt);
	text_NotifyObservers(txt,0);
    }
}

static boolean decompress(self,txt)
struct compress *self;
struct text *txt;
{
    text_AlwaysCopyText(txt,self->loc+1, self,0,compress_GetLength(self));
    text_AlwaysDeleteCharacters(txt,self->loc,1); /* this deletes the character, which reduces the view env's length to zero.  When the env removes itself, it destroys its viewref, which in turn destroys the dataobject it was looking at */
    return TRUE; /* (not used anyway) */
}

/* DecompressBox puts the compressed text back into the parent document */
void compress__DecompressBox(self,txt)
struct compress *self;
struct text *txt;
{
    DoAll(txt,NULL); /* make sure everything's updated */
    decompress(self,txt);
}

/* PartialDecompress decompresses the part that needs decompressing, and re-compresses the rest */
void compress__PartialDecompress(self,txt, pos,len)
struct compress *self;
struct text *txt;
long pos,len;
{
    long cprslen=compress_GetLength(self);
    if (pos==0 && len>=cprslen) {
	compress_DecompressBox(self,txt);
	return;
    }
    if (len<1)
	return;
    DoAll(txt,NULL); /* update compress boxes' positions */
    if (pos==0 || pos+len>=cprslen) {
	/* just decompress out of the front or back */
	text_AlwaysCopyText(txt,self->loc+(pos>0), self,pos,len);
	compress_AlwaysDeleteCharacters(self,pos,len);
	compress_SetLines(self, compress_GetLineForPos(self, compress_GetLength(self)));
	compress_NotifyObservers(self,0);
    } else {
	/* decompress out of the middle */
	long refpos=self->loc+1, reflen=cprslen-pos;
	/* first, decompress the ENTIRE last part of the box */
	text_AlwaysCopyText(txt,refpos, self,pos,reflen);
	compress_AlwaysDeleteCharacters(self,pos,reflen);
	compress_SetLines(self, compress_GetLineForPos(self, compress_GetLength(self)));
	compress_NotifyObservers(self,0);
	/* then, REcompress the part at the end we didn't WANT decompressed */
	compress_Compress(txt,refpos+len,reflen-len);
    }
}

/* DecompressAll decompresses everything in the parent document */
void compress__DecompressAll(classID, txt)
struct classheader *classID;
struct text *txt;
{
    DoAll(txt,decompress);
}

/* static variables used for limited-range operations */
static long startrange, endrange;

static boolean decompressIfInRange(self,txt)
struct compress *self;
struct text *txt;
{
    if (self->loc >= startrange && self->loc <= endrange)
	return decompress(self,txt);
    return TRUE; /* (not used anyway) */
}

/* DecompressRange decompresses everything in the parent document that falls within len after pos */
void compress__DecompressRange(classID, txt,pos,len)
struct classheader *classID;
struct text *txt;
long pos,len;
{
    startrange= pos;
    endrange= pos+len;
    DoAll(txt,decompressIfInRange);
}
