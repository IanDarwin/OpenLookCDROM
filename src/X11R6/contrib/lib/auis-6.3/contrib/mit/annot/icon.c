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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/annot/RCS/icon.c,v 1.6 1992/12/15 21:50:50 rr2b R6tape $";
#endif


 

#include "dataobj.ih"
#include "dict.ih"
#include "icon.eh"

boolean
icon__InitializeClass(classID)
    struct classinfo * classID;
{
    return TRUE;
}


boolean
icon__InitializeObject(classID,self)
    struct classinfo * classID;
    struct icon * self;
{
    self->child = (struct dataobject *)0;
    self->width = 200;
    self->height = 100;
    self->title = (char *)malloc(1);
    strcpy(self->title,"");
    return TRUE;
}




void
icon__SetSize(self,x,y)
    struct icon * self;
    long x;
    long y;
{
    if ((self->width != x) || (self->height != y)) {
	self->width = x;
	self->height = y;
	icon_NotifyObservers(self, icon_SizeChanged);
    }
}


void
icon__GetSize(self,x,y)
    struct icon * self;
    long * x;
    long * y;
{
    *x = self->width;
    *y = self->height;
}


void
icon__SetChild(self, dobj)
    struct icon * self;
    struct dataobject * dobj;
{
    if (self->child != (struct dataobject *)0)
	dataobject_Destroy(self->child);
    self->child = dobj;
    icon_NotifyObservers(self, icon_ChildChanged);
}

struct dataobject *
icon__GetChild(self)
    struct icon * self;
{
    return self->child;
}

void
icon__SetTitle(self, title)
struct icon * self;
char * title;
{
    if ((self->title = (char *)malloc(strlen(title) + 1)) != 0)
	strcpy(self->title, title);
    icon_NotifyObservers(self, icon_TitleChanged);
}   

char *
icon__GetTitle(self)
struct icon * self;
{
    return self->title;
}


long
icon__Write(self, file, writeID, level)
struct icon *self;
FILE *file;
long writeID;
int level;
{   
    int  haschild = 0;
    char * title = self->title;
    if (icon_GetWriteID(self) != writeID)  {
	icon_SetWriteID(self,writeID);
	if (self->child != (struct dataobject *)0)
	    haschild = 1;
	fprintf(file, "\\begindata{%s,%ld}\n", class_GetTypeName(self),icon_GetID(self));
	fprintf(file, "%ld %ld %d \n",self->width,self->height, haschild);

	fprintf(file,"\\title{");
	while (*title != '\0') {
	    if (*title == '\\')
		fputs("\\\\", file);
	    else if (*title == '}')
		fputs("\\}", file);
	    else
		fputc(*title, file);
	    title++;
	}
	fputs("}\n",file);

	if (haschild) 
	    dataobject_Write(self->child,file,
			     self->header.dataobject.writeID,
			     2);
	fprintf(file, "\\enddata{%s,%ld}\n", class_GetTypeName(self),icon_GetID(self));
    }

    return icon_GetID(self);
}

static check_for_title(self, file)
struct icon * self;
FILE * file;
{
    char * match = "title{";
    char title[1024];
    char c;
    int x;

    while (*match != '\0') {
	c = fgetc(file);
	if (c == EOF) 
	    return dataobject_PREMATUREEOF;
	if (c != *match){
	    ungetc(c,file); 
	    icon_SetTitle(self,"");
	    return dataobject_NOREADERROR;
	}
	++match;
    }

    if((c = fgetc(file))!= '\n')
	ungetc(c,file);
    x = 0;
    while ((c = fgetc(file)) != '}') {
	if (c == EOF)
	    return dataobject_PREMATUREEOF;
	if (c == '\\')
	    if ((c = fgetc(file)) != EOF)
		title[x] = c;
	    else return dataobject_PREMATUREEOF;
	else
	    title[x] = c;
	if (++x == 1024)
	    return dataobject_BADFORMAT;
    }
    title[x] = '\0';
    icon_SetTitle(self,title);

    while ((c = fgetc(file)) != '\\')
	if (c == EOF)
	    return dataobject_PREMATUREEOF;
    return dataobject_NOREADERROR;
}

long
icon__Read(self,file,id)
    struct icon * self;
    FILE * file;
    long id;
{
    long x,y,haschild;
    long objectid;
    int c;
    char * match;
    char datatype[1024];
    struct dataobject * newobject;
    long status;

    if (self->child != (struct dataobject *)0)
	dataobject_Destroy(self->child);
    self->child = (struct dataobject *)0;
    icon_SetID(self,icon_UniqueID(self));

    fscanf(file,"%ld %ld %d", &x, &y, &haschild);
    self->width = x;
    self->height = y;

    while ((c = fgetc(file)) != '\\')
	if (c == EOF)
	    return dataobject_PREMATUREEOF;

    if (check_for_title(self, file) != dataobject_NOREADERROR)
	return dataobject_BADFORMAT;

    newobject = (struct dataobject *)0;
    if (haschild) {
	match = "begindata{";
	while (*match != '\0') {
	    c = fgetc(file);
	    if (c == EOF) 
		return dataobject_PREMATUREEOF;
	    if (c != *match)
		return dataobject_BADFORMAT;
	    ++match;
	}
	if((c = fgetc(file))!= '\n')
	    ungetc(c,file);
	x = 0;
	while ((c = fgetc(file)) != ',') {
	    if (c == EOF)
		return dataobject_PREMATUREEOF;
	    datatype[x] = c;
	    if (++x == 1024)
		return dataobject_BADFORMAT;
	}
	datatype[x] = '\0';
	objectid = 0;
	while ((c = fgetc(file)) != '}') {
	    if (c == EOF)
		return dataobject_PREMATUREEOF;
	    if(c >= '0'	&& c <=	'9')
		objectid = objectid * 10 + c - '0';
	}

	if((c = getc(file))!= '\n')
	    ungetc(c,file);

	newobject = (struct dataobject *)class_NewObject(datatype);
	if (newobject == (struct dataobject *)0)
	    return dataobject_OBJECTCREATIONFAILED;
	dictionary_Insert(NULL,(char *)objectid, (char *)newobject);
	status = dataobject_Read(newobject, file, objectid);
	if (status != dataobject_NOREADERROR) 
	    return status;
    }
    /* adapt a cavalier attitude towards enddata */
    while ((c = fgetc(file)) != '\n')
	if (c == EOF)
	    break;

    icon_SetChild(self,newobject);  /* might be null */

    return dataobject_NOREADERROR;
}

