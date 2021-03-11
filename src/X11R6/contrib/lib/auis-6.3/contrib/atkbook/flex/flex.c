static char *flex_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/flex/RCS/flex.c,v 1.1 1992/10/06 22:11:55 susan R6tape $";

/* **************************************************** *\
Copyright 1989 Nathaniel S. Borenstein
Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear in
supporting documentation, and that the name of 
Nathaniel S. Borenstein not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission. 

Nathaniel S. Borenstein DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
Nathaniel S. Borenstein BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
\* ***************************************************** */
#include <flex.eh>
#include <dataobj.ih>
#include <message.ih>
#include <lpair.ih>

extern char *index();

boolean flex__InitializeObject(classID, self)
struct classheader *classID;
struct flex *self;
{
    self->left = NULL;
    self->right = NULL;
    self->lvname = NULL;
    self->rvname = NULL; 
    self->porf = lpair_PERCENTAGE;
    self->vorh = lpair_VERTICAL;
    self->pct = 50;
    self->movable = 1;
    return TRUE;
}

void flex__FinalizeObject(c, self)
struct classheader *c;
struct flex *self;
{
    if (self->lvname) {
	free(self->lvname);
	self->lvname = NULL;
    }
    if (self->rvname) {
	free(self->rvname);
	self->rvname = NULL;
    }
    if (self->left) {
	dataobject_Destroy(self->left);
	self->left = NULL;
    }
    if (self->right) {
	dataobject_Destroy(self->right);
	self->right = NULL;
    }
}

void flex__DeleteObjects(self)
struct flex *self;
{
    flex_FinalizeObject(self);
    /* reset everything to initial state */
    self->porf = lpair_PERCENTAGE;
    self->vorh = lpair_VERTICAL;
    self->pct = 50;
    self->movable = 1;
}

void flex__SetDisplayParams(self, porf, vorh, movable, pct)
struct flex *self;
int porf, vorh, movable, pct;
{
    if ((porf != self->porf) || (vorh != self->vorh)
	 || (movable != self->movable)
	 || (pct != self->pct)) {
	self->porf = porf;
	self->vorh = vorh;
	self->movable = movable;
	self->pct = pct;
    }
}

void flex__ToggleParts(self)
struct flex *self;
{
    char *name;
    struct dataobject *d;

    d = self->left;
    self->left = self->right;
    self->right = d;
    name = self->lvname;
    self->lvname = self->rvname;
    self->rvname = name;
}

boolean flex__InsertObject (self, d, viewname)
struct flex *self;
struct dataobject *d;
char *viewname;
{
    struct dataobject *d2;
    char *n1, *n2;

    d2 = (struct dataobject *) class_NewObject("flex");
    if (d == NULL || d2 == NULL) return(FALSE);
    n1 = malloc(1+strlen(viewname));
    n2 = malloc(9);
    if (n1 == NULL || n2 == NULL) return(FALSE);
    strcpy(n1, viewname);
    strcpy(n2, "flexview");
    self->left = d;
    self->right = d2;
    self->lvname = n1;
    self->rvname = n2;
    return(TRUE);
}

long flex__Write(self,fp ,writeid,level)
struct flex *self;
FILE *fp;
long writeid;
int level;
{
    if (flex_GetWriteID(self) != writeid) {
	flex_SetWriteID(self, writeid);

	fprintf(fp,"\\begindata{%s,%ld}\n",
		class_GetTypeName(self),
		flex_GetID(self));
	fprintf(fp, "$ %d %d %d %d\n", self->porf,
		self->vorh, self->movable, self->pct);
	if(self->left == NULL){
	    fprintf(fp, "\\ObjectEmpty\n");
	} else {
	    dataobject_Write(self->left,fp,writeid,level+1);
	    fprintf(fp, "\\view{%s}\n", self->lvname);
	}
	if(self->right == NULL){
	    fprintf(fp, "\\ObjectEmpty\n");
	} else {
	    dataobject_Write(self->right,
			     fp, writeid,
			     level+1);
	    fprintf(fp, "\\view{%s}\n", self->rvname);
	}
	fprintf(fp,"\\enddata{%s,%ld}\n",
		class_GetTypeName(self),
		flex_GetID(self));
    }
    return flex_GetID(self);
}

long flex__Read(self, fp, id)
struct flex *self;
FILE *fp;
long id;
{
    int status;
    char LineBuf[250];

    if (fgets(LineBuf, sizeof(LineBuf)-1, fp) == NULL) {
	return(dataobject_PREMATUREEOF);
    }
    sscanf(LineBuf, "$ %d %d %d %d\n", &(self->porf),
	    &(self->vorh), &(self->movable), &(self->pct));
    status = ReadOneObject(self, fp, TRUE);
    if (status != dataobject_NOREADERROR) return status;
    status = ReadOneObject(self, fp, FALSE);
    if (status != dataobject_NOREADERROR) return status;
    while (fgets(LineBuf, sizeof(LineBuf)-1, fp) != NULL) {
	if (!strncmp(LineBuf, "\\enddata{flex", 13)) {
	    return dataobject_NOREADERROR;
	}
    }
    return(dataobject_PREMATUREEOF);
}

static ReadOneObject(self, fp, IsLeft)
struct flex *self;
FILE *fp;
boolean IsLeft;
{
    char LineBuf[250], *s, *obidstr, *thisname;
    int status, obid;
    struct dataobject *newob = NULL;

    if (fgets(LineBuf, sizeof(LineBuf)-1, fp) == NULL) {
	return(dataobject_PREMATUREEOF);
    }
    if (strncmp(LineBuf, "\\begindata{", 11) != NULL) {
	if (strncmp(LineBuf, "\\ObjectEmpty", 12) != NULL) {
	    return(dataobject_BADFORMAT);
	}
	if (IsLeft) {
	    self->left = NULL;
	    self->lvname = NULL;
	} else {
	    self->right = NULL;
	    self->rvname = NULL;
	}
	return(dataobject_NOREADERROR);
    }
    thisname = &LineBuf[11];
    obidstr = index(thisname, ',');
    if (!obidstr) return(dataobject_BADFORMAT);
    *obidstr++ = '\0';
    s = index(obidstr, '}');
    if (!s) return(dataobject_BADFORMAT);
    *s = '\0';
    obid = atoi(obidstr);
    if ((newob = (struct dataobject *)
	  class_NewObject(thisname)))  {
	status = dataobject_Read(newob, fp, obid);
	if (status != dataobject_NOREADERROR) return status;
    } else {
	return(dataobject_OBJECTCREATIONFAILED);
    }
    if (fgets(LineBuf, sizeof(LineBuf)-1, fp) == NULL) {
	return(dataobject_PREMATUREEOF);
    }
    if (strncmp(LineBuf, "\\view{", 6)) {
	return(dataobject_BADFORMAT);
    }
    thisname = &LineBuf[6];
    s = index(thisname, '}');
    if (s) *s = '\0';
    s = malloc(1+strlen(thisname));
    if (!s) return(dataobject_OBJECTCREATIONFAILED);
    strcpy(s, thisname);
    if (IsLeft) {
	self->lvname = s;
	self->left = newob;
    } else {
	self->rvname = s;
	self->right = newob;
    }
    return(dataobject_NOREADERROR);
}

