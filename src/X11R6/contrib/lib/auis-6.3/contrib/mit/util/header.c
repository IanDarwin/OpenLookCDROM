/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/util/RCS/header.c,v 1.10 1992/12/15 21:50:50 rr2b R6tape $";
#endif

#include <class.h>
#include <andrewos.h>
#include <text.ih>
#include <dict.ih>
#include <dataobj.ih>
#include <envrment.ih>
#include <fontdesc.ih>
#include <style.ih>
#include <stylesht.ih>

#include "header.eh"

static char *header_prompts[] = {
    "  Left\t\t:  ",
    "    Center\t:  ",
    "          Right\t:  "
};

static struct style *header_promptStyle;

boolean header__InitializeClass(classID)
struct classheader classID;
{
    if (!(header_promptStyle = style_New()))
	return FALSE;
    
    style_AddNewFontFace(header_promptStyle, fontdesc_Bold);
    return TRUE;
}



void header__ObservedChanged(self,t,value)
struct header *self;
struct text *t;
long value;
{
    if(value==0) header_SetModified(self);
}

char *header__ViewName(self)
struct header *self;
{
    return "headrtv";
}

void header_SetPrompt(textobj, string)
struct text *textobj;
char *string;
{
    int x, l;
    struct environment *newenv;

    l = strlen(string);

    /* Now to change the view */
    x = text_GetFence(textobj);
    text_ClearFence(textobj);
    text_ReplaceCharacters(textobj, 0, x, string, l);
    text_SetFence(textobj, l);

    newenv = environment_InsertStyle(textobj->rootEnvironment, 0, header_promptStyle, TRUE);
    environment_SetLength(newenv, l - 1);
}	  

/* header_Read: the basic philosophy of this routine is to be completely forward compatible.
  Restrictions:
  In future versions  the first line in the data must always be type:[header|footer] or this routine will leave the object empty.
  Also the texts must always occur in order from left to right.
  */
long header__Read(self, file, id)
struct header *self;
FILE *file;
long id;
{

    /* this is take mostly from the template in dataobj.c */

    long endcount = 1;
    boolean begindata;
    char *s;
    long c;
    long status;
    char objectname[200];
    char buf[256];
    long objectid;
    int texts=header_ltext,i;
    struct dataobject *newobject;

    header_SetID(self,header_UniqueID(self));/* change id to unique number */
    if(!fgets(buf,sizeof(buf),file)) return dataobject_PREMATUREEOF;
    s=rindex(buf,':');
    if(s) *s++='\0';
    else return dataobject_PREMATUREEOF;
    if(strcmp(buf,"where")) return dataobject_NOREADERROR;
    else {
	if(!strcmp(s,"header\n")) self->where=header_HEADER;
	else if(!strcmp(s,"footer\n")) self->where=header_FOOTER;
    }
    if(!fgets(buf,sizeof(buf),file)) return dataobject_PREMATUREEOF;
    s=rindex(buf,':');
    if(s) *s++='\0';
    else return dataobject_PREMATUREEOF;
    if(!strcmp(buf,"active")) {
	for(i=header_ltext;*s && i<header_TEXTS;i++,s++) {
	    /* Currently ALWAYS_ACTIVE_MODE is defined in header.ch */
#ifdef ALWAYS_ACTIVE_MODE
	    self->active[i] = TRUE;	    
#else /* ALWAYS_ACTIVE_MODE */
	    self->active[i]=(*s)-'0';
#endif /* ALWAYS_ACTIVE_MODE */
	}
    }
    while (endcount != 0)  {
        while ((c = getc(file)) != EOF && c != '\\')  {
	    if(endcount == 1){
		return dataobject_NOREADERROR;
	    }
        }
        if (c == EOF) return dataobject_NOREADERROR;
        if ((c = getc(file)) == EOF)
            return dataobject_PREMATUREEOF;
        if (c == 'b')  {
            begindata = TRUE;
            s = "egindata";
        }
        else if (c == 'e')  {
            begindata = FALSE;
            s = "nddata";
        }
        else  {
	    if(endcount == 1){
		/* Place handling of \x characters here */
	    }
            continue;
        }
        while ((c = getc(file)) != EOF && c == *s) s++;
        if (c == '{' && *s == '\0')  {
            if (begindata) {
                s = objectname;
                while ((c = getc(file)) != EOF && c != ',')
                    *s++ = c;
                if (c == EOF) return dataobject_PREMATUREEOF;
                *s = '\0';
		objectid = 0;
		while ((c = getc(file)) != EOF && c != '}')
		    if(c >= '0' && c <= '9')objectid = objectid * 10 + c - '0';
		if (c == EOF) return dataobject_PREMATUREEOF;
		if((c = getc(file))!= '\n') ungetc(c,file);
		/* Call the New routine for the object */
		if(!strcmp(objectname,"text")) {
		    if(texts>=header_TEXTS) continue;
		    status = text_Read(self->texts[texts], file, objectid);
		    if (status != dataobject_NOREADERROR) return status;
		    dictionary_Insert(NULL,(char *)objectid, (char *)self->texts[texts]);
		    header_SetPrompt(self->texts[texts], header_prompts[texts]);
		    text_SetObjectInsertionFlag(self->texts[texts], FALSE);
		    texts++;
		} else {
		    if ((newobject = (struct dataobject *) class_NewObject(objectname)))  {
			/* Call the read routine for the object */
			status = dataobject_Read(newobject, file, objectid);
			if (status != dataobject_NOREADERROR) return status;
			/* We don't know this object so ignore it */
			dataobject_Destroy(newobject);

		    }
		    else {
			endcount += 1;
			/* return dataobject_OBJECTCREATIONFAILED; */
		    }
		}

	    }
	    else  {
		endcount -= 1;
		while ((c = getc(file)) != EOF && c != '}');
		if((c = getc(file))!= '\n') ungetc(c,file);
            }
        }
        else if(endcount == 1){
	    
        /* 	    Place Handling of characters following \  
           */	}
    }
    return dataobject_NOREADERROR;
}

static long header_FencedWrite(textobj, file, writeID, level)
struct text *textobj;
FILE *file;
long writeID;
int level;
{
    int len, pos;

    if (textobj->header.dataobject.writeID != writeID)  {
	textobj->header.dataobject.writeID = writeID;
	fprintf(file, "\\begindata{%s,%ld}\n", 		
		(textobj->WriteAsText)?"text": class_GetTypeName(textobj),
		dataobject_UniqueID(&textobj->header.dataobject));
	fprintf(file, "\\textdsversion{%d}\n", 12);
	if (textobj->styleSheet->templateName)
	    fprintf(file, "\\template{%s}\n", textobj->styleSheet->templateName);
	stylesheet_Write(textobj->styleSheet, file);
	len = text_GetLength(textobj);
	pos = text_GetFence(textobj);
	len = len - pos;
	text_WriteSubString(textobj, pos, len, file, 1);
	fprintf(file, "\\enddata{%s,%d}\n",
		(textobj->WriteAsText)?"text": class_GetTypeName(textobj),
		textobj->header.dataobject.id);
	fflush(file);
    }
    return textobj->header.dataobject.id;
}


#define printbool(x) ((x)?"1":"0")
long header__Write(self, file, writeID, level)
struct header *self;
FILE *file;
long writeID;
int level;
{
    if (header_GetWriteID(self) != writeID)  {
	header_SetWriteID(self,writeID);
	fprintf(file, "\\begindata{%s,%ld}\n", class_GetTypeName(self),header_GetID(self));
	fprintf(file,"where:");
	switch(self->where) {
	    case header_FOOTER:
		fprintf(file,"footer\n");
		break;
	    default:
	    case header_HEADER:
		fprintf(file,"header\n");
	}
	fprintf(file,"active:%s%s%s\n", printbool(self->active[header_ltext]), printbool(self->active[header_ctext]), printbool(self->active[header_rtext]));
	header_FencedWrite(self->texts[header_ltext], file,header_GetWriteID(self),1);
	header_FencedWrite(self->texts[header_ctext], file,header_GetWriteID(self),1);
	header_FencedWrite(self->texts[header_rtext], file,header_GetWriteID(self),1);
	fprintf(file, "\\enddata{%s,%ld}\n", class_GetTypeName(self),header_GetID(self));
    }

    return header_GetID(self);
}

#define HEADERTXTHELPSTRING "Click on 'Left', 'Center', or 'Right' Above"

boolean header__InitializeObject(classID,self)
struct classheader *classID;
struct header *self;
{
    int i;
    for(i=header_ltext;i<header_TEXTS;i++) {
	self->texts[i]=text_New();
	if(!self->texts[i]) {
	    for(i--;i>=header_ltext;i--) text_Destroy(self->texts[i]);
	    return FALSE;
	}
	header_SetPrompt(self->texts[i], header_prompts[i]);
	text_SetObjectInsertionFlag(self->texts[i], FALSE);
	text_AddObserver(self->texts[i],self);
	self->active[i]=TRUE;
    }
    self->where=header_HEADER;
    return TRUE;
}

void header__SetHeader(h, which, str)
struct header *h;
int which;
char *str;
{
    long pos;
    if(which<0 || which>=header_TEXTS) return;
    pos=text_GetFence(h->texts[which]);
    if(!str) str="";
    text_ReplaceCharacters(h->texts[which], pos, text_GetLength(h->texts[which])-pos, str, strlen(str));
}

struct header *header__Create(classID, type, left, center, right)
struct classheader *classID;
int type;
char *left, *center, *right;
{
    if(type==header_FOOTER || type==header_HEADER) {
	struct header *h=header_New();
	if(h) {
	    h->where=type;
	    header_SetHeader(h, header_ltext, left);
	    header_SetHeader(h, header_ctext, center);
	    header_SetHeader(h, header_rtext, right);
	    return h;
	}
    }
    return NULL;
}

void header__FinalizeObject(classID,self)
struct classheader *classID;
struct header *self;
{
    long i;
    for(i=header_ltext;i<header_TEXTS;i++)
	if(self->texts[i]) text_Destroy(self->texts[i]);
}
