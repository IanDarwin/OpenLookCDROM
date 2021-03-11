static char *switcher_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/switcher/RCS/switcher.c,v 1.1 1992/10/06 22:21:29 susan R6tape $";

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
#include <switcher.eh>
#include <dataobj.ih>

extern char *index();

boolean switcher__InitializeObject(c, self)
struct classheader *c;
struct switcher *self;
{
    self->FirstSwitchee = NULL;
    self->NowPlaying = NULL;
    return(TRUE);
}

void switcher__FinalizeObject(c, self)
struct classheader *c;
struct switcher *self;
{
    struct switchee *sw;
    for (sw = self->FirstSwitchee; sw; sw = sw->next) {
	dataobject_Destroy(sw->d);
	free(sw->label);
	free(sw->viewname);
    }
}

long switcher__Write(self,fp ,writeid,level)
struct switcher *self;
FILE *fp;
long writeid;
int level;
{
    struct switchee *sw;
    if (switcher_GetWriteID(self) != writeid) {
	switcher_SetWriteID(self, writeid);

	fprintf(fp, "\\begindata{switcher,%ld}\n",
		switcher_GetID(self));
	for (sw = self->FirstSwitchee; sw; sw=sw->next) {
	    if (sw == self->NowPlaying) {
		fprintf(fp, "*%s\n", sw->label);
	    } else {
		fprintf(fp, "%s\n", sw->label);
	    }
	    dataobject_Write(sw->d, fp, writeid, level+1);
	    fprintf(fp, "\\view{%s}\n", sw->viewname);
	}
	fprintf(fp,"\\enddata{switcher,%ld}\n",
		switcher_GetID(self));
    }
    return switcher_GetID(self);
}

long switcher__Read(self, fp, id)
struct switcher *self;
FILE *fp;
long id;
{
    char LineBuf[250], Label[250], *s, *obidstr, *thisname;
    int status, obid;
    struct dataobject *newob = NULL;

    while (TRUE) {
	if (fgets(Label, sizeof(Label)-1, fp) == NULL) {
	    return(dataobject_PREMATUREEOF);
	}
	if (!strncmp(Label, "\\enddata{switcher", 16)) {
	    return dataobject_NOREADERROR;
	}
	if (fgets(LineBuf, sizeof(LineBuf)-1, fp) == NULL) {
	    return(dataobject_PREMATUREEOF);
	}
	if (strncmp(LineBuf, "\\begindata{", 11)) {
	    return(dataobject_BADFORMAT);
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
	    if (status != dataobject_NOREADERROR) {
		return status;
	    }
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
	s = index(Label, '\n');
	if (s) *s = '\0';
	if (Label[0] == '*') {
	    if (!switcher_AddObject(self, newob,
				    Label+1, thisname)) {
		return(dataobject_OBJECTCREATIONFAILED);
	    }
	    switcher_SetNowPlaying(self, newob);
	} else if (!switcher_AddObject(self, newob,
				       Label, thisname)) {
		return(dataobject_OBJECTCREATIONFAILED);
	}
    }
}


boolean switcher__AddObject(self, d, label, viewname)
struct switcher *self;
struct dataobject *d;
char *label, *viewname;
{
    struct switchee *sw, *swtmp;

    sw = (struct switchee *) malloc(sizeof(struct switchee));
    if (sw == NULL) return(FALSE);
    if (label == NULL) label = "Next object";
    if (viewname == NULL) viewname = dataobject_ViewName(d);
    if (viewname == NULL) viewname = "view";
    sw->d = d;
    sw->label = malloc(1+strlen(label));
    if (sw->label == NULL) return(FALSE);
    strcpy(sw->label, label);
    sw->viewname = malloc(1+strlen(viewname));
    if (sw->viewname == NULL) return(FALSE);
    strcpy(sw->viewname, viewname);
    sw->next = NULL;

    /* find right place to put it */
    for (swtmp = self->FirstSwitchee;
	  (swtmp != NULL) && swtmp->next;
	  swtmp = swtmp->next) {
	;
    }
    if (swtmp != NULL) {
	swtmp->next = sw;
    } else { /* first one ever */
	self->FirstSwitchee = sw;
    }
    if (self->NowPlaying == NULL) {
	self->NowPlaying = sw;
	switcher_NotifyObservers(self,
		observable_OBJECTCHANGED);
    }
    return(TRUE);
}

boolean switcher__DeleteObject(self, d)
struct switcher *self;
struct dataobject *d;
{
    struct switchee *sw, *prevsw;

    for(prevsw = NULL, sw = self->FirstSwitchee;
	 sw != NULL;
	 prevsw = sw, sw = sw->next) {
	if (sw->d == d) {
	    if (prevsw != NULL) {
		prevsw->next = sw->next;
	    } else {
		self->FirstSwitchee = sw->next;
	    }
	    dataobject_Destroy(sw->d);
	    free(sw->label);
	    free(sw->viewname);
	    return(TRUE);
	}
    }
    return(FALSE);
}

boolean switcher__SetNowPlaying(self, d)
struct switcher *self;
struct dataobject *d;
{
    struct switchee *sw;

    for (sw = self->FirstSwitchee; sw; sw = sw->next) {
	if (sw->d == d) {
	    if (self->NowPlaying == sw) {
		return(TRUE); /* no change */
	    }
	    self->NowPlaying = sw;
	    switcher_NotifyObservers(self,
			observable_OBJECTCHANGED);
	    return(TRUE);
	}
    }
    return(FALSE);
}

char *switcher__ViewName(self)
struct switcher *self;
{
    return("switview"); 
    /* Two cheers for short file names */
}
