static char *hggh_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/hgghds/RCS/hggh.c,v 1.1 1992/10/06 22:16:25 susan R6tape $";

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
#include <stdio.h>
#include <butt.ih>
#include <hggh.eh>

extern char *malloc();

boolean hggh__InitializeObject(c, self)
struct classheader *c;
struct hggh *self;
{
    self->b = butt_New();
    if (self->b == NULL) return(FALSE);
    butt_SetText(self->b, "Toggle");
    return(TRUE);
}

void hggh__SetButtonText(self, t)
struct hggh *self;
char *t;
{
    butt_SetText(self->b, t);
    butt_NotifyObservers(self->b, observable_OBJECTCHANGED);
}

long hggh__Write(self, fp, id, level)
struct hggh *self;
FILE *fp;
long id;
int level;
{
    long uniqueid = hggh_UniqueID(self);

    if (id != hggh_GetWriteID(self)) {
	/* New write operation */
	hggh_SetWriteID(self, id);
	fprintf(fp,
	  "\\begindata{%s,%ld}\n%s\n\\enddata{%s,%ld}\n",
	  class_GetTypeName(self), uniqueid,
	  self->b->text ? self->b->text : "",
	  class_GetTypeName(self), uniqueid);
    }
    return(uniqueid);
}

long hggh__Read(self, fp, id)
struct hggh *self;
FILE *fp;
long id;
{
    char LineBuf[250];

    if (fgets(LineBuf,sizeof(LineBuf), fp) == NULL) {
	return(dataobject_PREMATUREEOF);
    }
    /* should check for quoted backslashes first */
    hggh_SetButtonText(self, LineBuf); 
    /* Now read in the \enddata line */
    if (fgets(LineBuf,sizeof(LineBuf), fp) == NULL) {
	return(dataobject_PREMATUREEOF);
    }
    if (strncmp(LineBuf, "\\enddata", 8)) {
	return(dataobject_MISSINGENDDATAMARKER);
    }
    return(dataobject_NOREADERROR);
}
