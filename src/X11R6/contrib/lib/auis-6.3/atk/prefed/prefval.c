/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University All rights Reserved. */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/prefval.c,v 1.6 1992/12/15 21:38:20 rr2b R6tape $";
#endif



 


#include <andrewos.h>
#include <math.h>
#include <class.h>
#include <ctype.h>

#include <observe.ih>

#include <util.h>

#include "prefval.eh"

#define zfree(x) do { if(x) { free(x); (x)=NULL;}} while (0)

#define MAX_LINE_LENGTH 70

static char *GlomStrings(), *ReadLine();

static char *strsave(str)
char *str;
{
    char *result;

    if(str==NULL) return NULL;

    result=(char *)malloc(strlen(str)+1);
    
    if(result==NULL) return NULL;

    strcpy(result, str);

    return result;
}

void prefval__FreeValue(self, v)
struct prefval *self;
struct prefval_value *v;
{
    switch(prefval_GetType(self)) {
	case prefval_Filename:
	    zfree(v->v.fval);
	    break;
	case prefval_Directory:
	    zfree(v->v.dval);
	    break;
	case prefval_String:
	    zfree(v->v.sval);
	    break;
	case prefval_Font:
	    zfree(v->v.fontval);
	    break;
	case prefval_Color:
	    zfree(v->v.cval);
	    break;
	default: ;
    }
}

/* copy v2 to v1. */
void prefval__CopyValue(self, v1, v2)
struct prefval *self;
struct prefval_value *v1, *v2;
{
    prefval_FreeValue(self, v1);
    switch(prefval_GetType(self)) {
	case prefval_Filename:
	    v1->v.fval=strsave(v2->v.fval);
	    break;
	case prefval_Directory:
	    v1->v.dval=strsave(v2->v.dval);
	    break;
	case prefval_String:
	    v1->v.sval=strsave(v2->v.sval);
	    break;
	case prefval_Font:
	    v1->v.fontval=strsave(v2->v.fontval);
	    break;
	case prefval_Color:
	    v1->v.cval=strsave(v2->v.cval);
	    break;
	default: 
	    *v1 = (*v2);
	    break;
    }
    v1->set=v2->set;
}

void prefval__InitValue(self, v)
struct prefval *self;
struct prefval_value *v;
{
    switch(prefval_GetType(self)) {
	case prefval_Filename:
	    v->v.fval=NULL;
	    break;
	case prefval_Directory:
	    v->v.dval=NULL;
	    break;
	case prefval_String:
	    v->v.sval=NULL;
	    break;
	case prefval_Font:
	    v->v.fontval=NULL;
	    break;
	case prefval_Color:
	    v->v.cval=NULL;
	    break;
	case prefval_Boolean:
	case prefval_Integer:
	case prefval_Real:
	    break;
	default:
	    fprintf(stderr, "WARNING: prefval_InitValue called on prefval of unkown type %d.\n", prefval_GetType(self));
	    break;
    }
    v->set=TRUE;
}

static boolean EnsureListSize(self, n)
struct prefval *self;
int n;
{
    struct prefval_value *result=self->values;
    
    if(n<self->vlistsize) return TRUE;
    
    if(result) {
	result=(struct prefval_value *)realloc(result, sizeof(struct prefval_value)*(n+1));
    } else result=(struct prefval_value *)malloc(sizeof(struct prefval_value)*(n+1));
    if(result) {
	int i;
	for(i=self->vlistsize;i<=n;i++) {
	    prefval_InitValue(self, result+i);
	}
	self->vlistsize=n+1;
	self->values=result;
	return TRUE;
    } else return FALSE;
}

static boolean EnsureChoiceListSize(self, n)
struct prefval *self;
int n;
{
    struct prefval_value *cvalues=self->cvalues;
    char **choices=self->choices;
    int i;

    if(n<self->clistsize) return TRUE;

    if(cvalues) {
	cvalues=(struct prefval_value *)realloc(cvalues, sizeof(struct prefval_value)*(n+1));
    } else cvalues=(struct prefval_value *)malloc(sizeof(struct prefval_value)*(n+1));
    
    if(cvalues==NULL) return FALSE;

    if(choices) {
	choices=(char **)realloc(choices, sizeof(char *)*(n+1));
    } else choices=(char **)malloc(sizeof(char *)*(n+1));

    if(choices==NULL) {
	free(cvalues);
	return FALSE;
    }

    for(i=self->clistsize;i<=n;i++) {
	choices[i]=NULL;
	prefval_InitValue(self, cvalues+i);
    }
    self->clistsize=n+1;
    self->choices=choices;
    self->cvalues=cvalues;
}

boolean prefval__InitializeObject(classID, self)
struct classheader *classID;
struct prefval *self;
{
    self->listmax=1;
    self->valueset=FALSE;
    self->prefname=NULL;
    self->appname=NULL;
    self->condition=NULL;
    self->type=prefval_Integer;
    self->vlistsize=0;
    self->values=NULL;
    self->clistsize=0;
    self->choices=NULL;
    self->cvalues=NULL;
    self->separator=NULL;
    self->curitem=0;
    return TRUE;
}

void prefval__FinalizeObject(classID, self)
struct classheader *classID;
struct prefval *self;
{
    zfree(self->separator);
    zfree(self->prefname);
    zfree(self->appname);
    if(self->values) {
	int i;
	for(i=0;i<self->vlistsize;i++) {
	    prefval_FreeValue(self, self->values+i);
	}
	zfree(self->values);
    }
    if(self->choices) {
	int i;
	for(i=0;i<self->clistsize;i++) {
	    if(self->cvalues) prefval_FreeValue(self, self->cvalues+i);
	    zfree(self->choices[i]);
	}
	zfree(self->choices);
    }
}

static boolean appproc(self, fp, buf)
struct prefval *self;
FILE *fp;
char *buf;
{
    prefval_SetAppName(self, buf);
    return FALSE;
}

static boolean prefproc(self, fp, buf)
struct prefval *self;
FILE *fp;
char *buf;
{
    prefval_SetPrefName(self, buf);
    return FALSE;
}

static boolean listproc(self, fp, buf)
struct prefval *self;
FILE *fp;
char *buf;
{
    int i=atoi(buf);
    
    if(prefval_GetType(self)==prefval_None) return TRUE;

    while(--i>=0) {
	char *tbuf=ReadLine(fp);
	struct prefval_value *pv;

	if(tbuf==NULL) return FALSE;
	
	pv = prefval_StringToValue(self, tbuf);
	
	prefval_SetIndexValue(self, i, pv);
	free(tbuf);
    }
    return FALSE;
}

static boolean clistproc(self, fp, buf)
struct prefval *self;
FILE *fp;
char *buf;
{
    int i=atoi(buf);
    
    if(prefval_GetType(self)==prefval_None) return TRUE;
    
    while(--i>=0) {
	char *tbuf=ReadLine(fp), *tbuf2;
	struct prefval_value *pv;

	if(tbuf==NULL) return TRUE;
	
	pv = prefval_StringToValue(self, tbuf);
	
	tbuf2=ReadLine(fp);
	if(tbuf2==NULL) {
	    free(tbuf);
	    return TRUE;
	}
	prefval_SetChoice(self, i, tbuf2, pv);
	free(tbuf);
	free(tbuf2);
    }
    return FALSE;
}


static boolean typeproc(self, fp, buf)
struct prefval *self;
FILE *fp;
char *buf;
{
    prefval_SetType(self, prefval_StringToType(buf));
    return FALSE;
}

static boolean condproc(self, fp, buf)
struct prefval *self;
FILE *fp;
char *buf;
{
    prefval_SetCondition(self, buf);
    return FALSE;
}

static boolean sepproc(self, fp, buf)
struct prefval *self;
FILE *fp;
char *buf;
{
    prefval_SetSeparator(self, buf);
    return FALSE;
}

static boolean listmaxproc(self, fp, buf)
struct prefval *self;
FILE *fp;
char *buf;
{
    prefval_SetListMax(self, atoi(buf));
    return FALSE;
}

static boolean doneproc(self, fp, buf)
struct prefval *self;
FILE *fp;
char *buf;
{
    return TRUE;
}


static struct dataprocs {
    char *name;
    procedure func;
} sprocs[]={
    {"appname", (procedure)appproc},
    {"prefname", (procedure)prefproc},
    {"list", (procedure)listproc},
    {"clist", (procedure)clistproc},
    {"type", (procedure)typeproc},
    {"separator", (procedure)sepproc},
    {"listmax", (procedure)listmaxproc},
    {"cond", (procedure)condproc},
    {"done", (procedure)doneproc},
    {NULL, NULL}
};

/* set in ReadLine if a line has a control directive */
static boolean linehascontrol;

static long dostuff(self, fp, rock, procs)
struct prefval *self;
FILE *fp;
long rock;
struct dataprocs *procs;
{
    char *buf, *buf2;
    boolean done=FALSE;
    while(!done) {
	struct dataprocs *dps;
	buf=ReadLine(fp);
	if(buf==NULL) return dataobject_PREMATUREEOF;
	if(!linehascontrol) {
	    free(buf);
	    continue;
	}
	buf2=ReadLine(fp);
	if(buf2==NULL) {
	    free(buf);
	    return dataobject_PREMATUREEOF;
	}

	for(dps=procs;dps->name;dps++) {
	    if(!strcmp(dps->name, buf+1)) {
		if(dps->func) done=((boolean (*)())dps->func)(self, rock, buf2);
		break;
	    }
	}
	free(buf);
	free(buf2);
    }
    
    return dataobject_NOREADERROR;
}


 
static long prefval__ReadDataPart(self, fp, dsversion)
struct prefval *self;
FILE *fp;
int dsversion;
{
    /*
      Read in the object from the file.
      */
    long err;
    err = dostuff(self, fp, fp, sprocs);
    return err;
}

static struct prefval_value *prefval__StringToValue(self, str)
struct prefval *self;
char *str;
{
    static struct prefval_value sv;
    if(prefval_GetType(self)==prefval_None) return NULL;
    sv.set=TRUE;
    switch(prefval_GetType(self)) {
	case prefval_Integer:
	    sv.v.ival=atoi(str);
	    break;
	case prefval_Real:
	    sv.v.rval=(float)atof(str);
	    break;
	case prefval_Filename:
	    sv.v.fval=str;
	    break;
	case prefval_Directory:
	    sv.v.dval=str;
	    break;
	case prefval_Font:
	    sv.v.fontval=str;
	    break;
	case prefval_String:
	    sv.v.sval=str;
	    break;
	case prefval_Boolean:
	    sv.v.bval=(*str=='T' || *str=='t' || *str=='y' || *str=='Y' || (*str=='o' && str[1]=='n'));
	    break;
	case prefval_Color:
	    sv.v.sval=str;
	    break;
	default:
	    fprintf(stderr, "prefval: WARNING: unkown type for preference %s\n", prefval_GetPrefName(self)?prefval_GetPrefName(self):"(NULL)");
	    return NULL;
    }
    return &sv;
}

static struct prefvaltypes {
    char *name;
    enum prefval_Type type;
} types[] = {
    {"Integer", prefval_Integer},
    {"Real", prefval_Real},
    {"Boolean", prefval_Boolean},
    {"String", prefval_String},
    {"Filename", prefval_Filename},
    {"Directory", prefval_Directory},
    {"Font", prefval_Font},
    {"Color", prefval_Color},
    {NULL, prefval_None}
};


enum prefval_Type prefval__StringToType(classID, str)
struct classheader *classID;
char *str;
{
    struct prefvaltypes *t=types;

    while(t->name) {
	if(FOLDEDEQ(t->name, str)) {
	    return t->type;
	}
	t++;
    }
    fprintf(stderr, "prefval WARNING: couldn't map '%s' to a type specifier!\n", str);
    return prefval_None;
}

char *prefval__TypeString(self)
struct prefval *self;
{
    
    struct prefvaltypes *t=types;

    while(t->name) {
	if(t->type==prefval_GetType(self)) {
	    return t->name;
	    break;
	}
	t++;
    }
    return "Unkown";
}

char *prefval__IndexValueString(self, which)
struct prefval *self;
int which;
{
    static char buf[1024];
    if(which>=self->vlistsize) return "NO VALUE 3!";
    if(!self->values[which].set) {
	return NULL;
    }
    switch(prefval_GetType(self)) {
	case prefval_None:
	    return "NO VALUE 2!";
	case prefval_Filename:
	case prefval_Directory:
	case prefval_Font:
	case prefval_Color:
	case prefval_String:
	    return self->values[which].v.sval;
	case prefval_Boolean:
	    return self->values[which].v.bval?"Yes":"No";
	case prefval_Integer:
	    sprintf(buf, "%d", self->values[which].v.ival);
	    return buf;
	case prefval_Real:
	    sprintf(buf, "%f", self->values[which].v.rval);
	    return buf;
	default:
	    ;
    }
    return "NO VALUE 1!";
}

static char prefvalbuf[1024];
static char *PreferenceString(self)
struct prefval *self;
{
    char *buf=prefvalbuf;
    int i;
    if(prefval_GetAppName(self)) {
	strcat(buf, prefval_GetAppName(self));
	strcat(buf, ".");
    }
    if(prefval_GetPrefName(self)) {
	strcat(buf, prefval_GetPrefName(self));
	strcat(buf,": ");
    }
    for(i=prefval_GetListSize(self)-1;i>=0;i--) {
	char *vs=prefval_IndexValueString(self, i);
	strcat(buf, vs?vs:"");
	if(i && prefval_GetSeparator(self)) strcat(buf, prefval_GetSeparator(self));
    }
    return buf;
}


char *prefval__PreferenceString(self)
struct prefval *self;
{
    prefvalbuf[0]='\0';
    return PreferenceString(self);
}

char *prefval__FullPreferenceString(self)
struct prefval *self;
{
    if(prefval_GetCondition(self)) {
	sprintf(prefvalbuf, "?%s:", prefval_GetCondition(self));
    } else prefvalbuf[0]='\0';
    return PreferenceString(self);
}


void prefval__SetFromPreferenceString(self, str)
struct prefval *self;
char *str;
{
    prefval_ClearValues(self);
    if(self->separator) {
	char *b=str;
	char *p;
	int i=0;
	
	do {
	    p=index(b, self->separator[0]);
	    if(p) *p='\0';
	    i++;
	    if(p) *p=self->separator[0];
	    b=p+1;
	    if(p && self->separator[0] && self->separator[1]==' ') {
		while(isspace(*b)) b++;
	    }
	} while (p);
	
	b=str;
	
	do {
	    p=index(b, self->separator[0]);
	    if(p) *p='\0';
	    i--;
	    if(*b) prefval_SetIndexValue(self, i, prefval_StringToValue(self, b));
	    if(p) *p=self->separator[0];
	    b=p+1;
	    if(p && self->separator[0] && self->separator[1]==' ') {
		while(isspace(*b)) b++;
	    }
	} while (p);
    } else {
	prefval_SetIndexValue(self, 0, prefval_StringToValue(self, str));
    }
}


static long prefval_SanelyReturnReadError(self, fp, id, code)
struct prefval *self;
FILE *fp;
long id;
long code;
{
    /*
      Suck up the file until our enddata, then return the error code.
      */
    char *buf, buf2[255];

    buf = NULL;
    sprintf(buf2, "\\enddata{%s,%ld}", class_GetTypeName(self), id);
    do {
	if (buf != NULL) free(buf);
	if ((buf = ReadLine(fp)) == NULL)
	    return(dataobject_PREMATUREEOF);
    } while (strncmp(buf, "\\enddata{", 9) != 0); /* find an enddata */

    if (strcmp(buf, buf2) != 0) {
	free(buf);
	return(dataobject_MISSINGENDDATAMARKER); /* not ours! */
    }
    free(buf);

    return(code);
}


long prefval__Read(self, fp, id)
struct prefval *self;
FILE *fp;
long id;
{

  char *buf;
  int dsversion;
  long err=dataobject_NOREADERROR;
  
  prefval_SetID(self, prefval_UniqueID(self));

  if ((buf = ReadLine(fp)) == NULL) err=dataobject_PREMATUREEOF;
  else if (strncmp(buf,"Datastream version:",19)) {
      err=dataobject_BADFORMAT;
  } else if ((dsversion = atoi(buf+19)) > prefval_DS_VERSION)	{
      err=dataobject_BADFORMAT;
  }
  if(buf) free(buf);

  prefval_SetModified(self);
  prefval_NotifyObservers(self, prefval_Generic);
  if(err==dataobject_NOREADERROR) {
      return prefval_SanelyReturnReadError(self, fp, id, prefval_ReadDataPart(self, fp, dsversion));
  } else return(prefval_SanelyReturnReadError(self, fp, id, err));
}



long prefval__Write(self, file, writeID, level)
struct prefval *self;
FILE *file;
long writeID;
int level;
{
    if (prefval_GetWriteID(self) != writeID)  {
	int i=prefval_GetListSize(self);
	prefval_SetWriteID(self,writeID);
	fprintf(file, "\\begindata{%s,%ld}\n", class_GetTypeName(self),prefval_GetID(self));
	fprintf(file, "Datastream version:%d\n",prefval_DS_VERSION);

	/* write the type info */
	fprintf(file, "\\type\n%s\n", prefval_TypeString(self));
	if(prefval_GetSeparator(self)) fprintf(file, "\\separator\n%s\n", prefval_GetSeparator(self));
	if(prefval_GetCondition(self)) fprintf(file, "\\cond\n%s\n", prefval_GetCondition(self));
	/* and the list */
	if(prefval_GetListMax(self)>1) fprintf(file, "\\listmax\n%d\n", prefval_GetListMax(self));
	fprintf(file, "\\list\n%d\n", i);
	while(--i>=0) {
	    char *vs=prefval_IndexValueString(self, i);
	    fprintf(file, "%s\n", vs?vs:"");
	}
	fprintf(file, "\\done\n\n");
	fprintf(file, "\\enddata{%s,%ld}\n", class_GetTypeName(self),prefval_GetID(self));
    }

    return prefval_GetID(self);
}


void prefval__SetAppName(self, name)
struct prefval *self;
char *name;
{
    zfree(self->appname);
    self->appname=strsave(name);
    prefval_SetModified(self);
}

void prefval__SetPrefName(self, name)
struct prefval *self;
char *name;
{
    zfree(self->prefname);
    self->prefname=strsave(name);
    prefval_SetModified(self);
}

void prefval__SetSeparator(self, name)
struct prefval *self;
char *name;
{
    zfree(self->separator);
    self->separator=strsave(name);
    prefval_SetModified(self);
}

void prefval__ClearChoices(self)
struct prefval *self;
{
    int i;
    for(i=0;i<self->clistsize;i++) {
	if(self->choices) zfree(self->choices[i]);
	if(self->cvalues) prefval_FreeValue(self, self->cvalues+i);
    }
    zfree(self->choices);
    zfree(self->cvalues);
    self->clistsize=0;
}

void prefval__ClearValues(self)
struct prefval *self;
{
    int i;
    for(i=0;i<self->vlistsize;i++) {
	if(self->values) prefval_FreeValue(self, self->values+i);
    }
    zfree(self->values);
    self->vlistsize=0;
}


void prefval__SetChoices(self, nchoices, choices, tvalues)
struct prefval *self;
int nchoices;
char **choices;
struct prefval_value *tvalues;
{
    int i;
    
    prefval_ClearChoices(self);

    if(!EnsureChoiceListSize(self, nchoices-1)) return;
    
    self->clistsize=nchoices;

    for(i=0;i<nchoices;i++) {
	self->choices[i]=(char *)malloc(strlen(choices[i])+1);
	if(self->choices[i]) strcpy(self->choices[i], choices[i]);
	prefval_CopyValue(self, self->cvalues+i, tvalues+i);
    }
    prefval_SetModified(self);
}

void prefval__SetChoice(self, which, choice, tvalue)
struct prefval *self;
int which;
char *choice;
struct prefval_value *tvalue;
{
    if(!EnsureChoiceListSize(self, which)) return;
    
    if(choice) {
	self->choices[which]=(char *)malloc(strlen(choice)+1);
	if(self->choices[which]) strcpy(self->choices[which], choice);
    } else self->choices[which]=NULL;
    
    prefval_CopyValue(self, self->cvalues+which, tvalue);
    
    prefval_SetModified(self);
}

void prefval__SetValues(self, nvalues, tvalues)
struct prefval *self;
int nvalues;
struct prefval_value *tvalues;
{
    int i;

    if(!EnsureListSize(self, nvalues-1)) return;

    for(i=0;i<nvalues;i++) prefval_CopyValue(self, self->values+i, tvalues+i);

    self->valueset=TRUE;
    self->vlistsize=nvalues;
    prefval_SetModified(self);
}

void prefval__SetIndexValue(self, which, tvalue)
struct prefval *self;
int which;
struct prefval_value *tvalue;
{
    if(!EnsureListSize(self, which)) return;
    if(tvalue) prefval_CopyValue(self, self->values+which, tvalue);
    if(tvalue) self->valueset=TRUE;
    prefval_SetModified(self);
}

void prefval__SetValue(self, tvalue)
struct prefval *self;
struct prefval_value *tvalue;
{
    prefval_SetIndexValue(self, 0, tvalue);
}

static char **choices=NULL;
static struct prefval_value *cvalues=NULL;

static int compare_choices(i1, i2)
int *i1, *i2;
{
    if(choices==NULL) return 0;
    
    return strcmp(choices[*i2], choices[*i1]);
}
	     
void prefval__SortChoices(self)
struct prefval *self;
{
    int *indices;
    int i;
    int listsize=self->clistsize;
    if(listsize==0) return;
    
    choices=(char **)malloc(sizeof(char *)*listsize);
    
    if(choices==NULL) return;
    
    cvalues=(struct prefval_value *)malloc(sizeof(struct prefval_value)*listsize);

    if(cvalues==NULL) {
	zfree(choices);
	return;
    }

    indices=(int *)malloc(sizeof(int)*listsize);

    if(indices==NULL) {
	zfree(choices);
	zfree(cvalues);
	return;
    }

    for(i=0;i<listsize;i++) {
	choices[i]=strsave(self->choices[i]);
	prefval_InitValue(self, cvalues+i);
	prefval_CopyValue(self, cvalues+i, self->cvalues+i);
	indices[i]=i;
    }

    qsort(indices, listsize, sizeof(int), compare_choices);


    prefval_ClearChoices(self);

    for(i=listsize-1;i>=0;i--) {
	int ind=indices[i];
	prefval_SetChoice(self, i, choices[ind], cvalues+ind);
	zfree(choices[ind]);
	prefval_FreeValue(self, cvalues+ind);
    }

    zfree(choices);
    zfree(cvalues);
    zfree(indices);
}


void prefval__SetCondition(self, cond)
struct prefval *self;
char *cond;
{
    zfree(self->condition);
    self->condition=strsave(cond);
    prefval_SetModified(self);
}

void prefval__SetModified(self)
struct prefval *self;
{
    self->isdefault=FALSE;
    super_SetModified(self);
}

void prefval__SetDefault(self)
struct prefval *self;
{
    self->isdefault=TRUE;
}

static char *GlomStrings(s, t)
char *s, *t;
{
    /* 
      Safely (allocs more memory) concatenates the two strings, 
      freeing the first.  Meant to build a new string of unknown length.
      */

    char *r;

    if (r = (char *)malloc(strlen(s)+strlen(t)+1)) {
	*r = '\0';
	strcpy(r,s);
	free(s);
	strcat(r,t);
	return(r);
    } else {
	free(s);
	return(NULL);
    }
}

static char *ReadLine(f)
FILE *f;
{
    /* 
      Reads from the datastream, attempting to return a single string.
      Undoes quoting and broken lines.
      Warning:  this routine wasn't meant to handle embedded
	 newlines.
	 Warning:  possible source of memory leaks;  remember to 
	   free the returned string when finished with it!
	   */

    char buf[MAX_LINE_LENGTH], /* (BUG) What if the datastream is broken? */
    buf2[MAX_LINE_LENGTH],
    *result;
    int i,j;

    linehascontrol=FALSE;
    if (result = (char *)malloc(1)) {
	*result = '\0';

	while (fgets(buf,sizeof(buf),f)) {
	    for (i = 0, j = 0; buf[i] != '\0'; ++i) {
		switch (buf[i]) {
		    case '\\':
			/* Unquote backslash or splice line */
			switch (buf[++i]) {
			    case '\\':
				/* Unquote the backslash */
				buf2[j++] = buf[i];
				break;
			    case '\n':
				/* broke long line */
				break;
			    default:
				/* things like \enddata come through here */
				linehascontrol=TRUE;
				buf2[j++] = '\\';
				buf2[j++] = buf[i];
				break;
			} /* switch (buf[++i]) */
			break;
		    case '\n':
			/* An unquoted newline means end of string */
			buf2[j++] = '\0';
			result = GlomStrings(result, buf2);
			return(result);
		    default:
			buf2[j++] = buf[i];
			break;
		} /* switch (buf[i]) */
	    } /* for (i = 0, ...) */
	    buf2[j++] = '\0';
	    result = GlomStrings(result, buf2);
	} /* while (fgets...) */
	/* Should not get here... it means we went off the end
	 of the data stream.  Ooops. */
    } /* if (result = ... ) */
    return(NULL);
}
