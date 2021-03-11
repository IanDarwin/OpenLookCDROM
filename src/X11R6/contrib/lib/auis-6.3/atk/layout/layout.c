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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/layout/RCS/layout.c,v 1.13 1994/04/17 19:33:20 rr2b Exp $";
#endif

/* $ACIS$ */

#include <andrewos.h>
#include <stdio.h>
#include <assert.h>

#include <class.h>
#include <ctype.h>
#include <dataobj.ih>
#include <view.ih>
#include <graphic.ih>
#include <layout.eh>

#define classname(do) ((do) == NULL ? "<NO OBJECT>" : class_GetTypeName(do))
#define safename(c) ((c) == NULL ? "<NULL DATA>" : classname(cData(c)))

#define	MINSIZE	5	    /* minimum width/height resulting from constraints */

/* initialize entire class */

boolean layout__InitializeClass(classID)
struct classheader *classID;	    /* unused */
{
    if (debug)
	printf("layout_InitializeClass()\n");

    return TRUE;
}

/* get corresponding view name */

char *					/* returns "layoutview */
layout__ViewName(self)
struct layout *self;
{
    return "layoutview";
}

/* Initialize new data layout */

boolean					/* returns TRUE for success */
layout__InitializeObject(classID, self)
struct classheader *classID;		/* unused */
struct layout *self;
{

    if (debug)
	printf("layout_InitializeObject\n");

    self->firstcomponent = NULL;
    self->inGetModified = FALSE;
    return TRUE;
}

/* tear down a layout */

void layout__FinalizeObject(classID, self)
struct classheader *classID;	/* unused */
struct layout *self;
{
    if (debug)
	printf("layout_FinalizeObject\n");
}

/* toggle debugging flag */

void layout__ToggleDebug(self)
struct layout *self;
{
    if (debug) {
	printf("layout debugging off\n");
	debug = 0;
    } else {
	debug = 1;
	printf("layout debugging on - ESC-ESC turns it off\n");
    }
}

/* write layout to file */

long					/* returns id of object written */
layout__Write(self, f, writeID, level)
struct layout *self;
FILE * f;				/* file to be written */
long writeID;				/* unique ID of object in output file */
int level;				/* nesting level */
{
    struct component *c;

    if (debug)
	printf("layout_Write(%ld, %d)\n", writeID, level);

    if (getDataObject(self).writeID != writeID) {
	getDataObject(self).writeID = writeID;
	fprintf (f, "\\begindata{%s,%ld}\n", class_GetTypeName(self), layout_GetID(self));
	forallcomponents(self, c) {
	    fprintf(f, "<%ld,%ld,%ld,%ld", cLeft(c), cTop(c), cWidth(c), cHeight(c));
	    if (cData(c) != NULL) {
		fprintf(f, ",");
		if (cVaries(c))
		    fprintf(f, "V");
		else
		    fprintf(f, "F");
		fprintf(f, ">");
		fprintf(f, "\n");
		if (debug)
		    printf("Writing out %s\n", class_GetTypeName(cData(c)));
		dataobject_Write (cData(c), f, super_GetWriteID(self), level + 1);
	    }
	    else
		fprintf(f, ">\n");  /* no child present */
	}
	fprintf (f, "\\enddata{%s,%ld}\n", class_GetTypeName(self), layout_GetID(self));
    }
    return layout_GetID(self);
}

/* object to and print out bad input */

static void objectto(f, message)
FILE *f;			    /* input file containing offending material */
char *message;			    /* error message */
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

/* read ASCII representation of a layout */

/*
   Layout data stream:

The layout data stream follows ATK standards, starting with a begindata and
ending with an enddata directive.  In between comes a sequence of components,
ordered front to back.  Each component starts with a line of the form:

<left,top,width,height,properties>

where the first four are decimal numbers giving the geometry of the component's
rectangle, and the fifth is a set of letters giving specific properites:

  V or F for variable or fixed components (* is an acceptable substitute for V)

This line is followed by the ATK representation for the component object.
A null component is represented by omitting the fifth field and data entirely.

*/


static long			    /* returns read error status */
readASCII(self, f, id)
struct layout *self;
FILE *f;			    /* input file */
long id;			    /* unique identifier in data stream */
{
    int ch;
    struct component *c;
    char dataname[256];
    char *np;
    long uniqueID;
    boolean havechild;
    long left, top, width, height;

    while ((ch = getc(f)) != EOF) {
	switch(ch) {

	    case '\0':
		printf("stopped on zero character");
		return dataobject_BADFORMAT;

	    case '<': /* another component coming */
		c = layout_CreateComponent(self);
		if (fscanf(f, "%d,%d,%d,%d", &left, &top, &width, &height) != 4) {
		    objectto(f, "layout:  expected four numbers separated by commas");
		    layout_RemoveComponent(self, c);
		    return dataobject_BADFORMAT;
		}
		layout_SetComponentSize(self, c, left, top, width, height);
		c->varies = TRUE;
		ch = getc(f);
		havechild = FALSE;
		if (ch != ',')
		    ungetc(ch, f);
		else {
		    havechild = TRUE;
		    for (;;) {
			ch = getc(f);
			if (ch == '>')
			    break;
			else if (ch == '*' || ch == 'V')
			    c->varies = TRUE;
			else if (ch == 'F')
			    c->varies = FALSE;
			else {
			    ungetc(ch, f);
			    objectto(f, "layout:  unknown property or > missing");
			    layout_RemoveComponent(self, c);
			    return dataobject_BADFORMAT;
			}
		    };
		}
		if (debug)
		    printf("Got <%ld,%ld,%ld,%ld,%c%c> havechild=%d\n", cLeft(c), cTop(c), cWidth(c), cHeight(c), (cVaries(c) ? 'V' : 'F'), havechild);
		if (fgetstring(f, "\n") != 0) {
		    objectto(f, "layout:  trash after coordinates");
		    layout_RemoveComponent(self, c);
		    return dataobject_BADFORMAT;
		}

		if (havechild) {
		    if (fgetstring(f, "\\begindata{") != 0) {
			objectto(f, "layout:  missing component begindata");
			layout_RemoveComponent(self, c);
			return dataobject_BADFORMAT;
		    }
		    for (np = dataname; np < dataname + sizeof dataname - 1 && (ch = getc(f)) != EOF && ch != ','; np++)
			*np = ch;
		    *np = '\0';
		    if (fscanf(f, "%ld ", &uniqueID) != 1) {
			objectto(f, "layout:  missing , after component name");
			layout_RemoveComponent(self, c);
			return dataobject_BADFORMAT;
		    }
		    if (fgetstring(f, "}") != 0) {
			objectto(f, "layout:  missing closing brace after begindata");
			layout_RemoveComponent(self, c);
			return dataobject_BADFORMAT;
		    }
		    if (fgetstring(f, "\n") != 0)
			objectto(f, "layout:  extra stuff after begindata");
		    np=dataname;
		    if(!class_Load(dataname)) {
			np="unknown";
		    }
		    c->data = (struct dataobject *)class_NewObject(np);
		    if (cData(c) == NULL) {
			printf("Could not create %s object.\n", dataname);
			return dataobject_OBJECTCREATIONFAILED;
		    }
		    cData(c)->id = uniqueID;
		    dataobject_Read(cData(c), f, uniqueID);

		}
		layout_Demote(self, c);
		if (debug)
		    printf("done adding component\n");
		break;

	    case '\\': /* enddata coming */
		if (fscanf(f, "enddata{%255[^,}\n],%ld}\n", dataname, &uniqueID) != 2) {
		    objectto(f, "layout:  expected enddata or another component");
		    return dataobject_BADFORMAT;
		}
		else if (strcmp(dataname, class_GetTypeName(self)) != 0) {
		    objectto(f, "layout: wrong data name in enddata");
		    return dataobject_BADFORMAT;
		}
		else if (uniqueID != id) {
		    objectto(f, "layout:  wrong unique ID in enddata");
		    return dataobject_BADFORMAT;
		}
		return dataobject_NOREADERROR;

	    default:
		ungetc(ch, f);
		objectto(f, "layout:  bad input line");
		return dataobject_BADFORMAT;

	} /* end switch */
    } /* end reading loop */
    printf("layout:  premature EOF");
    return dataobject_PREMATUREEOF;
}

/* read layout from file */

long				    /* returns read error status */
layout__Read(self, f, id)
struct layout *self;
FILE * f;			    /* input file */
long id;			    /* unique identifier in data stream */
{
    long rc;

    if (debug)
	printf("layout_Read(%d)\n", id);

    layout_SetID(self, layout_UniqueID(self));
    layout_SetModified(self);

    rc = readASCII(self, f, id);
    layout_NotifyObservers(self, observable_OBJECTCHANGED);
    if (debug)
	printf("layout_Read rc = %d\n", rc);

    return rc;
}

/* remove a component */

void layout__RemoveComponent(self, c)
struct layout *self;
struct component *c;		    /* component to be removed */
{
    struct component *prev;

    if (debug)
	printf("layout_RemoveComponent(%s)\n", safename(c));

    if (cData(c) != NULL) {
	/* dataobject_Destroy(cData(c)); */
	c->data = NULL;
    }

    if (c == self->firstcomponent)
	self->firstcomponent = c->nextcomponent;
    else {
	forallcomponents(self, prev) {
	    if (prev->nextcomponent == c) {
		prev->nextcomponent = c->nextcomponent;
		break;
	    }
	}
	if (prev == NULL)
	    printf("layout:  attempt to remove non-component\n");
    }
    free(c);
    layout_SetModified(self);
    layout_NotifyObservers(self, observable_OBJECTCHANGED);

    if (debug)
	printf("imbedded remove complete\n");
}

/* fill in component */

void layout__FillInComponent(self, name, c)
struct layout *self;
char *name;				/* name of dataobject subclass */
struct component *c;			/* component to be filled in */
{
    struct dataobject *newobject;

    if (debug)
	printf("layout_FillInComponent(%s)\n", name);

    newobject = (struct dataobject *) class_NewObject(name);
    if (newobject != NULL) {
	if (cData(c) != NULL)
	    dataobject_Destroy(cData(c));
	newobject->id = (long) newobject;
	c->data = newobject;
    }
    layout_SetModified(self);
    layout_NotifyObservers(self, observable_OBJECTCHANGED);
}

/* create component */

struct component *layout__CreateComponent(self)
struct layout *self;
{
    struct component *c;

    if (debug)
	printf("layout_CreateComponent\n");

    c = (struct component *)malloc(sizeof(struct component));
    if (c == NULL) {
	printf("Insufficient memory for component\n");
	exit(4);
    }
    c->data = NULL;
    c->left = 0;
    c->top = 0;
    c->width = 0;
    c->height = 0;
    c->varies = TRUE;
    c->nextcomponent = self->firstcomponent;
    self->firstcomponent = c;
    /* we don't notify anybody of changes yet because this component is still vacant */
    return c;
}

/* change component size */

void layout__SetComponentSize(self, c, x, y, w, h)
struct layout *self;
struct component *c;			/* component to change */
long x,	y, w, h;			/* new position and size */
{
    if (debug)
	printf("layout_SetComponentSize(%s, %d, %d, %d, %d)\n", classname(cData(c)), x, y, w, y);

    if (cLeft(c) != x || cTop(c) != y || cWidth(c) != w || cHeight(c) != h) {
	c->left = x;
	c->top = y;
	c->width = w;
	c->height = h;
	layout_SetModified(self);
	layout_NotifyObservers(self, observable_OBJECTCHANGED);
    }
}

/* check to see if modified */

long layout__GetModified(self)
struct layout *self;
{
    struct component *c;
    long rc, cc;

    rc = super_GetModified(self);
    if (!self->inGetModified) {
	self->inGetModified = TRUE;
	forallcomponents(self, c) {
	    if (cData(c) != NULL) {
		cc = dataobject_GetModified(cData(c));
		if (cc > rc) rc = cc;
	    }
	}
	self->inGetModified = FALSE;
    }

    if (debug)
	printf("layout_GetModified = %d\n", rc);

    return rc;
}

/* see if b would completely obscure c */

#define obscures(self, b, c) ((cLeft(b) <= 0 || cLeft(c) > 0 && cLeft(b) <= cLeft(c)) && (cWidth(b) <= 0 || cWidth(c) > 0 && cRight(b) >= cRight(c)) && (cTop(b) <= 0 || cTop(c) > 0 && cTop(b) <= cTop(c)) && (cHeight(b) <= 0 || cHeight(c) > 0 && cBottom(b) >= cBottom(c)))

/* see if b and c overlap */

#define overlaps(self, b, c) ((cLeft(b) <= 0 || cWidth(c) <= 0 || cLeft(b) <= cRight(c)) && (cWidth(b) <= 0 || cLeft(c) <= 0 ||cRight(b) >= cLeft(c)) && (cTop(b) <= 0 || cHeight(c) <= 0 || cTop(b) <= cBottom(c)) && (cHeight(b) <= 0 || cTop(c) <= 0 || cBottom(b) >= cTop(c)))

/* promote component to front of stack*/

void layout__Promote(self, c)
struct layout *self;
struct component *c;			/* component to be promoted to front */
{
    struct component *prev;
    boolean changed;

    if (debug)
	printf("layout_Promote(%s)\n", classname(cData(c)));

    changed = FALSE;
    if (c != self->firstcomponent) {
	forallcomponents(self, prev) {
	    if (overlaps(self, prev, c))
		changed = TRUE;
	    if (prev->nextcomponent == c) {
		prev->nextcomponent = c->nextcomponent;
		c->nextcomponent = self->firstcomponent;
		self->firstcomponent = c;
		break;
	    }
	}
	if (prev == NULL)
	    printf("Layout: Attempt to promote non-component\n");
    }
    if (changed) {
	layout_SetModified(self);
	layout_NotifyObservers(self, observable_OBJECTCHANGED);
    }
}

/* demote component to back of the bus */

void layout__Demote(self, c)
struct layout *self;
struct component *c;			/* component to be demoted to back */
{
    struct component *prev;

    if (debug)
	printf("layout_Demote(%s)\n", safename(c));

    if (c == self->firstcomponent)
	self->firstcomponent = c->nextcomponent;
    else {
	forallcomponents(self, prev) {
	    if (prev->nextcomponent == c) {
		prev->nextcomponent = c->nextcomponent;
		break;
	    }
	}
	if (prev == NULL)
	    printf("layout:  attempt to demote non-component\n");
    }
    
    if (self->firstcomponent == NULL) {
	c->nextcomponent = self->firstcomponent;
	self->firstcomponent = c;
    }
    else {
	forallcomponents(self, prev)
	  if (prev->nextcomponent == NULL || obscures(self, prev->nextcomponent, c)) {
	      c->nextcomponent = prev->nextcomponent;
	      prev->nextcomponent = c;
	      break;
	  }
    }

    layout_SetModified(self);
    layout_NotifyObservers(self, observable_OBJECTCHANGED);
}

/* make object variable */

void layout__MakeVariable(self, c)
struct layout *self;
struct component *c;			/* component which may vary */
{
    if (debug)
	printf("layout_MakeVariable(%s)\n", safename(c));

    c->varies = TRUE;
}

/* make object fixed */

void layout__MakeFixed(self, c)
struct layout *self;
struct component *c;			/* component which may vary */
{
    if (debug)
	printf("layout_MakeFixed(%s)\n", safename(c));

    c->varies = FALSE;
}
