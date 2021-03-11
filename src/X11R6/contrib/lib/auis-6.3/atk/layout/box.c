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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/layout/RCS/box.c,v 1.11 1994/04/17 19:33:20 rr2b Exp $";
#endif

/* $ACIS$ */

 

#include <stdio.h>
#include <assert.h>

#include <class.h>
#include <andrewos.h>
#include <ctype.h>
#include <dataobj.ih>
#include <rect.h>
#include <view.ih>
#include <graphic.ih>

#include <box.eh>

static boolean debug=FALSE;
#ifndef _IBMR2
extern char * malloc ();
#endif /* _IBMR2 */

#define classname(do) ((do) == NULL ? "<NO OBJECT>" : class_GetTypeName(do))
#define safename(c) ((c) == NULL ? "<NULL DATA>" : classname(c->data))

/* initialize entire class */

boolean
box__InitializeClass(classID)
struct classheader *classID;	    /* unused */
{
    if (debug)
	printf("box_InitializeClass()\n");

    return TRUE;
}

/* get corresponding view name */

char *					/* returns "boxview */
box__ViewName(self)
struct box *self;
{
    return "boxview";
}

/* Initialize new data box */

boolean					/* returns TRUE for success */
box__InitializeObject(classID, self)
struct classheader *classID;		/* unused */
struct box *self;
{

    if (debug)
	printf("box_InitializeObject\n");

    self->inGetModified = FALSE;
    self->contents = NULL;
    return TRUE;
}

/* tear down a box */

void
box__FinalizeObject(classID, self)
struct classheader *classID;	/* unused */
struct box *self;
{
    if (debug)
	printf("box_FinalizeObject\n");
}

/* toggle debugging flag */

void
box__ToggleDebug(self)
struct box *self;
{
    if (debug) {
	printf("box debugging off\n");
	debug = 0;
    } else {
	debug = 1;
	printf("box debugging on - ESC-ESC turns it off\n");
    }
}

/* write box to file */

long					/* returns id of object written */
box__Write(self, f, writeID, level)
struct box *self;
FILE * f;				/* file to be written */
long writeID;				/* unique ID of object in output file */
int level;				/* nesting level */
{

    if (debug)
	printf("box_Write(%ld, %d)\n", writeID, level);

    if (getDataObject(self).writeID != writeID) {
	getDataObject(self).writeID = writeID;
	fprintf (f, "\\begindata{%s,%ld}\n", class_GetTypeName(self), box_GetID(self));
	if (self->contents != NULL) {
	    if (debug)
		printf("Writing out %s\n", class_GetTypeName(self->contents));
	    dataobject_Write (self->contents, f, super_GetWriteID(self), level + 1);
	}
	fprintf (f, "\\enddata{%s,%ld}\n", class_GetTypeName(self), box_GetID(self));
    }
    return box_GetID(self);
}

/* object to and print out bad input */

static void
objectto(f, message)
FILE *f;				/* input file containing offending material */
char *message;				/* error message */
{
    int ch;

    printf("%s:  \"", message);
    for (;;) {
	ch = getc(f);
	if (ch == '\n' || ch == EOF || ch == '\0')
	    break;
	putchar(ch);
    }
    printf("\"\n");
}

/* scan input for a specific string */

static boolean			    /* returns TRUE for success */
fgetstring(f, string)
FILE *f;			    /* input file */
char *string;			    /* desired input string */
{
    int ch;

    ch = getc(f);
    while (ch == *string && *string != '\0') {
	ch = getc(f);
	string++;
    }
    ungetc(ch, f);
    if (*string != '\0')
	return TRUE;
    else
	return FALSE;
}

/* read ASCII representation of a box */

/*
   box data stream:

The box data stream follows ATK standards, starting with a begindata and
ending with an enddata directive.  In between comes the contained object.

*/

static long			    /* returns read error status */
readASCII(self, f, id)
struct box *self;
FILE *f;			    /* input file */
long id;			    /* unique identifier in data stream */
{
    int ch;
    char dataname[256];
    char *np;
    long uniqueID;
    while ((ch = getc(f)) != EOF) {
	if (ch == 0) {
	    printf("stopped on zero character");
	    return dataobject_BADFORMAT;
	}
	if (ch != '\\') {
	    objectto(f, "box:  missing begindata or enddata\n");
	    return dataobject_BADFORMAT;
	}

	ch = getc(f);
	ungetc(ch, f);
	if (ch == 'b') {	    /* begindata coming */

	    if (fgetstring(f, "begindata{") != 0) {
		objectto(f, "box:  missing begindata");
		return dataobject_BADFORMAT;
	    }
	    for (np = dataname; np < dataname + sizeof dataname - 1 && (ch = getc(f)) != EOF && ch != ','; np++)
		*np = ch;
	    *np = '\0';
	    if (fscanf(f, "%ld ", &uniqueID) != 1) {
		objectto(f, "box:  missing , after component name");
		return dataobject_BADFORMAT;
	    }
	    if (fgetstring(f, "}") != 0) {
		objectto(f, "box:  missing closing brace after begindata");
		return dataobject_BADFORMAT;
	    }
	    if (fgetstring(f, "\n") != 0)
		objectto(f, "box:  extra stuff after begindata");
	    np=dataname;
	    if(!class_Load(np)) {
		np="unknown";
	    }
	    self->contents = (struct dataobject *)class_NewObject(np);
	    if (self->contents == NULL) {
		printf("Could not create %s object.\n", dataname);
		return dataobject_OBJECTCREATIONFAILED;
	    }
	    self->contents->id = uniqueID;
	    dataobject_Read(self->contents, f, uniqueID);
	    box_SetModified(self);
	    box_NotifyObservers(self, observable_OBJECTCHANGED);

	    if (debug)
		printf("done adding component\n");
	}

	else if (ch == 'e')	{   /* enddata coming */
	    if (fscanf(f, "enddata{%255[^,}\n],%ld}\n", dataname, &uniqueID) != 2) {
		objectto(f, "box:  expected enddata or another component");
		return dataobject_BADFORMAT;
	    }
	    else if (strcmp(dataname, class_GetTypeName(self)) != 0) {
		objectto(f, "box: wrong data name in enddata");
		return dataobject_BADFORMAT;
	    }
	    else if (uniqueID != id) {
		objectto(f, "box:  wrong unique ID in enddata");
		return dataobject_BADFORMAT;
	    }
	    return dataobject_NOREADERROR;
	}

	else {
	    objectto(f, "box:  bad input line");
	    return dataobject_BADFORMAT;
	}
    } /* end of reading loop */
    printf("box:  premature EOF");
    return dataobject_PREMATUREEOF;
}

/* read box from file */

long				    /* returns read error status */
box__Read(self, f, id)
struct box *self;
FILE * f;			    /* input file */
long id;			    /* unique identifier in data stream */
{
    long rc;

    if (debug)
	printf("box_Read(%d)\n", id);

    box_SetID(self, box_UniqueID(self));
    box_SetModified(self);

    rc = readASCII(self, f, id);
    box_NotifyObservers(self, observable_OBJECTCHANGED);
    if (debug)
	printf("box_Read rc = %d\n", rc);

    return rc;
}

/* check to see if modified */

long
box__GetModified(self)
struct box *self;
{
    long rc, cc;

    rc = super_GetModified(self);
    if (!self->inGetModified && self->contents != NULL) {
	self->inGetModified = TRUE;
	cc = dataobject_GetModified(self->contents);
	if (cc > rc) rc = cc;
	self->inGetModified = FALSE;
    }

    if (debug)
	printf("box_GetModified = %d\n", rc);

    return rc;
}

/* fill in contents */

void
box__FillInContents(self, name)
struct box *self;
char *name;
{
    struct dataobject *newobject;

    if (debug)
	printf("box_FillInContents(%s)\n", name);

    newobject = (struct dataobject *) class_NewObject(name);
    if (newobject != NULL) {
	if (self->contents != NULL)
	    dataobject_Destroy(self->contents);
	newobject->id = (long) newobject;
	self->contents = newobject;
    }
    box_SetModified(self);
    box_NotifyObservers(self, observable_OBJECTCHANGED);
}
