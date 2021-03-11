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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/annot/RCS/ps.c,v 1.4 1992/12/15 21:50:50 rr2b R6tape $";
#endif


#include "class.h"
#include "dataobj.ih"
#include "dict.ih"
#include "ps.eh"
#include "text.ih"
#define WIDTH 438
#define HEIGHT 244

/* image dimensions in 72 dot/inch pixels */
#define HALF_PAGE_WIDTH 432
#define HALF_PAGE_HEIGHT 324


/****************************************************************/
/*		private functions				*/
/****************************************************************/

static check_for_title(self, file)
struct ps * self;
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
	    ps_SetTitle(self,"");
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
    ps_SetTitle(self,title);

    while ((c = fgetc(file)) != '\\')
	if (c == EOF)
	    return dataobject_PREMATUREEOF;
    return dataobject_NOREADERROR;
}



/****************************************************************/
/*		class procedures				*/
/****************************************************************/
boolean
ps__InitializeClass(classID)
    struct classheader * classID;
{
    return TRUE;
}

boolean
ps__InitializeObject(classID,self)
struct classheader * classID;
struct ps * self;
{
    struct text * to;

    to = text_New();
    text_SetReadOnly(to,0);
    ps_SetChild(self,to);
    ps_SetTitle(self,"PostScript");
    ps_SetSize(self, WIDTH, HEIGHT);
    ps_SetPixelWidth(self, HALF_PAGE_WIDTH);
    ps_SetPixelHeight(self, HALF_PAGE_HEIGHT);
    return TRUE;
}

/****************************************************************/
/*		instance methods				*/
/****************************************************************/

void
ps__SetChild(self,child)
    struct ps * self;
    struct dataobject * child;
{
	super_SetChild(self,child);
	if (child != (struct dataobject *) 0)
	    text_SetReadOnly((struct text *) child, 0);
}

long
ps__Write(self, file, writeID, level)
struct ps *self;
FILE *file;
long writeID;
int level;
{   
    int  haschild = 0;
    long w, h, pw, ph;
    char * title = ps_GetTitle(self);
    struct dataobject *childob = ps_GetChild(self);

    if (ps_GetWriteID(self) != writeID)  {
	ps_SetWriteID(self,writeID);
	if (childob != (struct dataobject *)0)
	    haschild = 1;
	ps_GetSize(self, &w, &h);
	pw = ps_GetPixelWidth(self);
	ph = ps_GetPixelHeight(self);
	fprintf(file, "\\begindata{%s,%ld}\n", class_GetTypeName(self),ps_GetID(self));
	fprintf(file, "%ld %ld %d %ld %ld \n", w, h, haschild, pw, ph);

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
	    dataobject_Write(childob,file,
			     self->header.dataobject.writeID,
			     2);
	fprintf(file, "\\enddata{%s,%ld}\n", class_GetTypeName(self),ps_GetID(self));
    }

    return ps_GetID(self);
}

long
ps__Read(self,file,id)
    struct ps * self;
    FILE * file;
    long id;
{
    long x, y, haschild, width, height;
    long objectid;
    int c;
    char *match;
    char datatype[1024];
    struct dataobject * newobject;
    long status;

    newobject = ps_GetChild(self);

    if (newobject != (struct dataobject *)0)
	/* has the effect of destroying the child */
	ps_SetChild(self, (struct dataobject *)0);

    ps_SetID(self,ps_UniqueID(self));

    fscanf(file,"%ld %ld %d %ld %ld", &x, &y, &haschild, &width, &height);
    ps_SetSize(self, x, y);
    ps_SetPixelWidth(self, width);
    ps_SetPixelHeight(self, height);

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

    ps_SetChild(self,newobject);  /* might be null */

    return dataobject_NOREADERROR;
}
