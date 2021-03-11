static char *conob_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/con/RCS/conob.c,v 1.1 1992/10/06 22:08:31 susan R6tape $";

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
#include <conob.eh>
#include <dataobj.ih>
#include <fontdesc.ih>

extern char *malloc(), *index();

boolean conob__InitializeObject(c, self)
struct classinfo *c;
struct conob *self;
{
    self->numval = 42;
    self->strval = NULL;
    self->displaymin = 0;
    self->displaymax = 100;
    self->Boxed = TRUE;
    self->DisplayTemplate = NULL;
    return(TRUE);
}

void conob__FinalizeObject(c, self)
struct classheader *c;
struct conob *self;
{
    if (self->DisplayTemplate) {
	free(self->DisplayTemplate);
	self->DisplayTemplate = NULL;
    }
}

void conob__GetStringToDisplay(self, buf, len, IsClick)
struct conob *self;
char *buf;
int len;
boolean IsClick;
{
    char *templateptr, *t, *end, NumBuf[25], *strval;

    if (self->strval == NULL) {
	strval = "<no value>";
    } else {
	strval = self->strval;
    }
    end = buf+len-1; /* leave room for null */
    if (self->DisplayTemplate == NULL) {
	templateptr = "$ (*)";
    } else {
	templateptr = self->DisplayTemplate;
    }
    while(*templateptr && (buf < end)) {
	switch (*templateptr) {
	    case '\\':
		/* Force copy of next char */
		*buf++ = *++templateptr;
		break;
	    case '$':
		/* copy over numeric value */
		sprintf(NumBuf, "%d", self->numval);
		for (t=NumBuf; *t && (buf<end); ++t) {
		    *buf++ = *t;
		}
		break;
	    case '*':
		/* copy over string value */
		for (t=strval; *t && (buf<end); ++t) {
		    *buf++ = *t;
		}
		break;
	    default:
		*buf++ = *templateptr;
	}
	++templateptr;
    }
    *buf = '\0';
}

long conob__Write(self, fp, writeID, level)
struct conob *self;
FILE *fp;
long writeID;
int level;
{
    if (conob_GetWriteID(self) != writeID)  {
        conob_SetWriteID(self,writeID);
	fprintf(fp, "\\begindata{%s,%ld}\n",
		class_GetTypeName(self),
		conob_GetID(self));
	conob_WriteState(self, fp);
	fprintf(fp,
		"\\enddata{%s,%ld}\n",
		class_GetTypeName(self),
		conob_GetID(self));
    }

    return conob_GetID(self);
}

void conob__WriteState(self, fp)
struct conob *self;
FILE *fp;
{
    fprintf(fp, "$a %d\n$b %d\n$c %d\n", self->displaymin,
	     self->displaymax, self->Boxed);
    if (self->DisplayTemplate) {
	fprintf(fp, "$d %s\n", self->DisplayTemplate);
    }
}

long conob__Read(self, fp, id)
struct conob *self;
FILE *fp;
long id;
{
    char LineBuf[250];

    while (fgets(LineBuf, sizeof(LineBuf)-1, fp) != NULL) {
	if (strncmp(LineBuf, "\\enddata{", 9) == 0) {
	    return(dataobject_NOREADERROR);
	}
	conob_HandleDataLine(self, LineBuf);
    }
    return dataobject_PREMATUREEOF;
}

void conob__HandleDataLine(self, line)
struct conob *self;
char *line;
{
    if (*line == '$' && (*(line+2) == ' ')) {
	switch (*(line+1)) {
	    case 'a':
		self->displaymin = atoi(line+3);
		return;
	    case 'b':
		self->displaymax = atoi(line+3);
		return;
	    case 'c':
		self->Boxed = atoi(line+3);
		return;
	    case 'd':
		conob_SetDisplayTemplate(self, line+3);
		return;
	}
    }
    fprintf(stderr, "Ignoring unrecognized data: %s\n", line);
}

char *conob__ViewName(self)
struct conob *self;
{
    return("conview"); /* overrides default for subclasses */
}

void conob__SetNumval(self, num)
struct conob *self;
long num;
{
    self->numval = num;
}

void conob__SetStrval(self, str)
struct conob *self;
char *str;
{
    self->strval = str;
}

void conob__SetDisplayTemplate(self, dt)
struct conob *self;
char *dt;
{
    char *s;

    if (dt == NULL ) {
	if (self->DisplayTemplate != NULL) {
	    free(self->DisplayTemplate);
	}
	self->DisplayTemplate = NULL;
	return;
    }
    s = index(dt, '\n');
    if (s) *s = NULL; /* End templates at newlines */
    if (self->DisplayTemplate == NULL) {
	self->DisplayTemplate = malloc(1+strlen(dt));
    } else {
	self->DisplayTemplate = realloc(self->DisplayTemplate,
					1+strlen(dt));
    }
    if (self->DisplayTemplate != NULL) {
	strcpy(self->DisplayTemplate, dt);
    }
}

void conob__SetDisplayMin(self, min)
struct conob *self;
int min;
{
    self->displaymin = min;
}

void conob__SetDisplayMax(self, max)
struct conob *self;
int max;
{
    self->displaymax = max;
}

void conob__SetBoxed(self, boxed)
struct conob *self;
boolean boxed;
{
    self->Boxed = boxed;
}

