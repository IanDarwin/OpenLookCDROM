/* Copyright 1992 by Carnegie Mellon University. All rights Reserved. $Disclaimer: 
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

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/prefs.c,v 1.22 1994/04/24 15:39:48 rr2b Exp $ */

#ifndef NORCSID
static char *rcsid_prefs_c = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/prefs.c,v 1.22 1994/04/24 15:39:48 rr2b Exp $";
#endif /* NORCSID */

#include <andrewos.h>
#include <math.h>
#include <class.h>
#include <util.h>
#include <ctype.h>

#include <text.ih>

#include "prefs.eh"
#include "prefval.ih"
#include <dict.ih>
#include <environ.ih>
#include <stylesht.ih>
#include <sbutton.ih>
#include <im.ih>
#include <strcache.ih>
#include <envrment.ih>

/* tests two cached strings for equality. */
static int FindCat(a, b)
struct prefgroup *a;
char *b;
{
    return a->name==b?0:1;
}

#define STRDUP(x) (x?NewString(x):NULL)
#define SAVESTR(x) (x?strcache_SaveStr(x):NULL)
#define zfree(xxx) do { if(xxx) { free(xxx); (xxx)=NULL;}} while (0)

static char *ReadLine();	

static void FreeHelpStyles(self)
struct prefs *self;
{
    zfree(self->llist);
    self->llistsize=self->llistcnt=0;
}


static struct environment *AddStyle(self, pos, len, style)
struct text *self;
long pos, len;
struct style *style;
{
    struct environment *newenv;
    newenv = environment_InsertStyle(self->rootEnvironment, pos, style, TRUE);
    environment_SetLength(newenv, len);
    environment_SetStyle(newenv, FALSE, FALSE);
    return newenv;
}


static void ApplyHelpStyles(self)
struct prefs *self;
{
    long i;
    struct stylesheet *ss=text_GetStyleSheet(self->help);
    struct style *b=NULL, *bb, *s;
    if(ss) {
	b=stylesheet_Find(ss, "subsection");
	bb=stylesheet_Find(ss, "groupname");
    }
    for(i=0;i<self->llistcnt;i++) {
	switch(self->llist[i].type) {
	    case PrefsSubsection:
		s=b;
		break;
	    case PrefsGroupname:
		s=bb;
		break;
	    default:
		continue;
	}
	AddStyle(self->help, self->llist[i].pos, self->llist[i].len, s);
    }
    FreeHelpStyles(self);
}

boolean prefs__InitializeObject(classID, self)
struct classheader *classID;
struct prefs *self;
{
    char *file;
    FILE *fp2;
    long err, errs=0;
    char *files, filenamebuf[1025];

    self->selfmod=FALSE;
    self->llistsize=self->llistcnt=0;
    self->llist=NULL;
    
    self->version=(-1);
    
    self->sane=FALSE;

    self->pstyle=self->hstyle=NULL;
    
    self->prefs=list_New();
    if(self->prefs==NULL) {

	return FALSE;
    }
    self->categories=list_New();
    if(self->categories==NULL) {
	list_Destroy(self->prefs);
	return FALSE;
    }

    
    self->help=text_New();
    if(self->help==NULL) {
	list_Destroy(self->prefs);
	list_Destroy(self->categories);
	return FALSE;
    } else {
	text_ReadTemplate(self->help, "prefhelp", FALSE);
	text_ReadTemplate(self->help, "default", FALSE);
    }

    self->readingdefaults=TRUE;

    files=environ_GetProfile("PrefEdPath");

    if(files==NULL) {
	files=environ_Get("PREFEDPATH");
	if(files==NULL) files=environ_AndrewDir("/lib/prefs.def");
    }

    prefs_ReadTemplate(self, "default", FALSE);
    while (files && *files) {
	int len;
	file = index(files, ':');
	if(file==NULL) len=strlen(files);
	else len = file-files;
	strncpy(filenamebuf, files, len);
	filenamebuf[len]='\0';
	err=prefs_AlwaysInsertFile(self, NULL, filenamebuf, prefs_GetLength(self));
	if(err<=0) {
	    fprintf(stderr, "prefs WARNING: couldn't read preferences definitions file %s!\n", filenamebuf);
	    errs++;
	}
	if(file) files=file+1;
	else files=NULL;
    }
    files=environ_AndrewDir("/lib/global.prf");
    if(files && access(files, R_OK)==0) {
	err=prefs_AlwaysInsertFile(self, NULL, files, prefs_GetLength(self));
	if(err<=0) {
	    fprintf(stderr, "prefs WARNING: couldn't read preferences definitions file %s!\n", files);
	    errs++;
	}	
    }
    if(errs) err=1;
    else err=prefs_ReadDataPart(self);
    if(err!=dataobject_NOREADERROR) {
	FreeHelpStyles(self);
	list_Destroy(self->prefs);
	list_Destroy(self->categories);
	fprintf(stderr, "prefs WARNING: couldn't read preferences definitions file(s)!\n");
	return FALSE;
    }
    ApplyHelpStyles(self);
    prefs_SetCopyAsText(self, TRUE);
    self->lastheader=NULL;
    self->lastgroup=NULL;
    
    return TRUE;
}


static boolean freestring(name, rock)
char *name;
long rock;
{
    zfree(name);
    return TRUE;
}

static void freestringlist(vl)
struct list *vl;
{
    list_Enumerate(vl, freestring, NULL);
    list_Destroy(vl);
}

static boolean freeall(pd, rock)
struct prefdesc *pd;
struct prefs *rock;
{
    zfree(pd->cond);
    zfree(pd->val);
    if(pd->mark) {
	mark_Destroy(pd->mark);
    }
    if(pd->pm) {
	prefs_RemoveMark(rock, pd->pm);
	mark_Destroy(pd->pm);
    }
    if(pd->defviews) {
	list_Destroy(pd->defviews);
    }
    if(pd->prevlines) {
	freestringlist(pd->prevlines);
    }
    if(pd->freeobject && pd->obj) prefval_Destroy(pd->obj);
    free(pd);
    return TRUE;
}


static boolean freegroups(pg, self)
struct prefgroup *pg;
struct prefs *self;
{
    zfree(pg);
    return TRUE;
}

void prefs__FinalizeObject(classID, self)
struct classheader *classID;
struct prefs *self;
{
    if(self->categories) {
	list_Enumerate(self->categories, freegroups, self);
	list_Destroy(self->categories);
    }
    if(self->prefs) {
	list_Enumerate(self->prefs, freeall, self);
	list_Destroy(self->prefs);
    }
    if(self->lastline.vl) {
	list_Destroy(self->lastline.vl);
    }
    if(self->lastline.prevlines) {
	freestringlist(self->lastline.prevlines);
    }
    text_Destroy(self->help);
}

static boolean endflag=TRUE;

static char *trans[]={
    "file", "pvaltvf",
    "string", "pvaltv",
    "integer", "pvaltv",
    "boolean", "pvalbv",
    "slider", "pvalslv",
    "color", "pvalcv",
    "font", "pvalfv",
    "filename", "pvaltvf",
    "filechoices", "pvaltvfc",
    "directory", "pvaltvf",
    "filelist", "pvaltvf",
    "directorylist", "pvaltvf",
    "stringlist", "pvaltvl",
    "slider", "pvalslv",
    NULL, NULL
};

char *prefs__TranslateViewName(classID, name)
struct classheader *classID;
char *name;
{
    char **p=trans;
    if(name == NULL)
	return(NULL);
    while(p[0] && !FOLDEDEQ(p[0],name)) {
	p+=2;
    }
    if(p[0]) return p[1];
    else {
	struct classinfo *ci=class_Load(name);
	static struct classinfo *vci=NULL;
	if(vci==NULL) vci=class_Load("view");
	if(ci && class_IsType(ci, vci)) return name;
	else return "pvaltv";
    }
}

struct thedescrock {
    struct prefdesc *pd;
    char *name;
    char *app;
    char *cond;
};

struct therock {
    struct prefs *self;
    long pos;
    struct prefval *obj;
};

static boolean AddViews(vname, tr)
char *vname;
struct therock *tr;
{
    struct prefs *self=tr->self;
    struct prefval *obj=tr->obj;
    long pos=tr->pos;
    prefs_AlwaysAddView(self, pos, vname, obj);
    prefs_AlwaysInsertCharacters(self, pos+1, "\n", 1);
    tr->pos+=2;
    return TRUE;
}

static struct prefdesc *NewPrefDesc(self, p)
struct prefs *self;
char *p;
{
    struct prefdesc *result=(struct prefdesc *)malloc(sizeof(struct prefdesc));
    char *q;
    enum prefval_Type type;
    char separators[3];
    separators[0]=separators[1]=separators[2]='\0';
    result->app=NULL;
    result->name=NULL;
    result->group=NULL;
    result->seps=NULL;
    result->cond=NULL;
    result->val=NULL;
    result->defviews=NULL;
    result->low=(0);
    result->high=(-1);
    result->listsize=1;
    result->writetype=FALSE;
    result->writeviews=FALSE;
    result->views=NULL;
    result->mark=NULL;
    result->indefs=FALSE;
    result->shadow=FALSE;
    result->order=(-1);
    result->helppos=(-1);
    result->freehelp=FALSE;
    result->expert=FALSE;
    result->prevlines=NULL;
    result->pm=NULL;
    q=index(p, ' ');
    if(q) *q='\0';
    result->type=prefval_StringToType(p);
    if(q==NULL) return result;
    q++;
    p=index(q, ' ');
    if(p) *p='\0';
    if(FOLDEDEQ(q, "List")) {
	result->listsize=65536;
	if(p) {
	    p++;
	    strncpy(separators, p, 2);
	} else strcpy(separators, ":");

	result->seps=SAVESTR(separators);
    }
    return result;
}

static boolean CopyViewName(name, nl)
char *name;
struct list *nl;
{
    list_InsertEnd(nl, name);
    return TRUE;
}

static struct list *CopyViewList(vl)
struct list *vl;
{
    struct list *nl;

    if(vl==NULL) return NULL;

    nl=list_New();
    if(nl==NULL) return NULL;

    list_Enumerate(vl, CopyViewName, nl);
    return nl;
}


static boolean CopyStr(name, nl)
char *name;
struct list *nl;
{
    list_InsertEnd(nl, STRDUP(name));
    return TRUE;
}

static struct list *CopyList(vl)
struct list *vl;
{
    struct list *nl;

    if(vl==NULL) return NULL;

    nl=list_New();
    if(nl==NULL) return NULL;

    list_Enumerate(vl, CopyStr, nl);
    return nl;
}

static struct prefdesc *DuplicateDesc(pd)
struct prefdesc *pd;
{
    struct prefdesc *result=(struct prefdesc *)malloc(sizeof(struct prefdesc));
    if(result==NULL) return NULL;
    result->app=pd->app;
    result->name=pd->name;
    result->group=pd->group;
    result->seps=pd->seps;
    result->cond=STRDUP(pd->cond);
    result->val=STRDUP(pd->val);
    result->views=pd->views;
    result->prevlines=CopyList(pd->prevlines);
    result->defviews=CopyViewList(pd->defviews);
    result->low=pd->low;
    result->high=pd->high;
    result->type=pd->type;
    result->listsize=pd->listsize;
    result->writetype=FALSE;
    result->writeviews=FALSE;
    result->mark=NULL;
    result->indefs=FALSE;
    result->shadow=FALSE;
    result->order=(-1);
    result->obj=NULL;
    result->freeobject=FALSE;
    result->expert=pd->expert;
    result->helppos=pd->helppos;
    result->freehelp=FALSE;
    result->pm=NULL;
    return result;
}

static boolean FixOrderValues(pd, val)
struct prefdesc *pd;
long val;
{
    if(pd->order>val) pd->order++;
    return TRUE;
}

struct prefdesc *prefs__DuplicatePref(self, pd, newapp, newcond)
struct prefs *self;
struct prefdesc *pd;
char *newapp, *newcond;
{
    struct prefdesc *result=DuplicateDesc(pd);
    char *v;
    static char *star;

    if(star==NULL) star=SAVESTR("*");
    
    if(result==NULL) return NULL;
    zfree(result->cond);
    result->cond=STRDUP(newcond);
    result->app=SAVESTR(newapp);
    result->obj=prefval_New();
    if(result->obj==NULL) {
	freeall(result, self);
	return NULL;
    }
    result->freeobject=TRUE;
    prefval_SetAppName(result->obj, newapp);
    prefval_SetPrefName(result->obj, pd->name); 
    prefval_SetType(result->obj, pd->type);
    prefval_SetListMax(result->obj, pd->listsize);
    prefval_SetSeparator(result->obj, pd->seps);
    prefval_SetCondition(result->obj, result->cond);
    prefval_SetRangeLow(result->obj, result->low);
    prefval_SetRangeHigh(result->obj, result->high);
    v=prefval_PreferenceString(pd->obj);
    if(v) {
	v=index(v, ':');
	if(v) {
	    v++;
	    while(isspace(*v)) v++;
	}
    }
    if(v) prefval_SetFromPreferenceString(result->obj, v);
    list_InsertEnd(self->prefs, result);
    prefval_AddObserver(result->obj, self);
    if(pd->order>=0) {
	long ipos=(-1);
	list_Enumerate(self->prefs, FixOrderValues, pd->order);
	self->maxorder++;
	if(pd->app==star || pd->app==NULL) {
	    result->order=pd->order;
	    pd->order--;
	    if(pd->pm) {
		ipos=mark_GetPos(pd->pm);
	    }
	} else {
	    result->order=pd->order+1;
	    ipos=mark_GetEndPos(pd->pm)+1;
	}
	if(ipos>=0) {
	    prefs_InsertCharacters(self, ipos, " \n", 2);
	    result->pm=prefs_CreateMark(self, ipos, 1);
	    mark_SetStyle(result->pm, FALSE, FALSE);
	}
    } else {
	long pos=prefs_GetLength(self);
	result->order= ++self->maxorder;
	prefs_InsertCharacters(self, pos, " \n", 2);
	result->pm=prefs_CreateMark(self, pos, 1);
	mark_SetStyle(result->pm, FALSE, FALSE);
    }
    if(result->pm) 
    prefs_UpdateOneInText(self, result);
    return result;
}

void prefs__DeletePref(self, pd)
struct prefs *self;
struct prefdesc *pd;
{
    if(pd->pm) {
	long pos=mark_GetPos(pd->pm)-2, lpos=(-1);
	while(pos>=0) {
	    if(prefs_GetChar(self, pos)=='\n') {
		if(prefs_GetChar(self, pos+1)=='#') lpos=pos+1;
		else break;
	    }
	    pos--;
	}
	prefs_DeleteCharacters(self, mark_GetPos(pd->pm), mark_GetLength(pd->pm)+1);
	if(lpos>=0) prefs_DeleteCharacters(self, lpos, mark_GetPos(pd->pm)-lpos);
	prefs_RemoveMark(self, pd->pm);
	mark_Destroy(pd->pm);
	pd->pm=NULL;
    }
    if(!pd->indefs) {
	list_Delete(self->prefs, pd);
	freeall(pd, self);
    } else {
	if(pd->val) {
	    prefval_SetFromPreferenceString(pd->obj, pd->val);
	} else {
	    prefval_SetFromPreferenceString(pd->obj, "");
	}
	prefval_SetDefault(pd->obj);
	prefval_NotifyObservers(pd->obj, observable_OBJECTCHANGED);
    }
}

static struct list *HandleViewList(self, buf)
struct prefs *self;
char *buf;
{
    char *p=buf;
    struct list *vl;
    vl=list_New();
    if(vl==NULL) return NULL;
    while(p) {
	char *q=index(p, ' ');
	char *vname;
	long pos=prefs_GetLength(self);
	if(q) *q='\0';
	if(*p!='\0') {
	    vname=SAVESTR(prefs_TranslateViewName(p));
	    if(vname==NULL) continue;
	    list_InsertEnd(vl, vname);
	}
	if(q) p=q+1;
	else break;
    }
    return vl;
}


static boolean FindDesc(pd, rock)
struct prefdesc *pd;
struct thedescrock *rock;
{
    char *app=rock->app;
    char *name=rock->name;
    char *cond=rock->cond;

    if(pd->shadow) return TRUE;
    
    if(!(name==pd->name)) return TRUE;

    if((pd->app==NULL || (pd->app[0]=='*' && pd->app[1]=='\0')) && pd->cond==NULL && rock->pd==NULL) {
	rock->pd=pd;
	return TRUE;
    }

    if(pd->app && !(app==pd->app)) return TRUE;
    
    if(pd->cond==NULL) {
	rock->pd=pd;
	return TRUE;
    }
    if((cond==pd->cond || (cond && pd->cond && strcmp(cond, pd->cond)==0))) rock->pd=pd;
    
    return TRUE;
}


static boolean ListsEqual(l1, l2)
struct list *l1, *l2;
{
    if(l1==l2) return TRUE;
    
    if(l1==NULL || l2==NULL) return TRUE;
    
    if(list_Size(l1)!=list_Size(l2)) return FALSE;
    list_Start(l1);
    list_Start(l2);
    
    do {
	if(list_Data(l1)!=list_Data(l2)) return FALSE;
    } while(list_Advance(l1) && list_Advance(l2));
    return TRUE;
}


static boolean InsertPref(pd, self)
struct prefdesc *pd;
struct prefs *self;
{
    if(pd->shadow && self->sortby!=prefs_Group) return TRUE;
 
    if(self->lastgroup==NULL || (self->lastgroup!=pd->group)) {
	struct prefgroup *pg;
	self->lastgroup=pd->group;
	if(list_Enumerate(self->categories, FindCat, pd->group)) return TRUE;
	pg=(struct prefgroup *)malloc(sizeof(struct prefgroup));
	if(pg==NULL) return FALSE;
	pg->name=pd->group;
	pg->grouphelp=NULL;
	list_InsertEnd(self->categories, pg);
    }
    return TRUE;
/* see code at end of this file... it doesn't get used right now, but might be useful someday... */
}

static void HandlePref(self, line, order)
struct prefs *self;
struct prefline *line;
int *order;
{
    struct prefval *obj=NULL;
    int len;
    char *p;
    long lpos;
    struct prefdesc *desc;
    struct thedescrock otherrock;
    boolean newobj=FALSE;
    char *app=SAVESTR(line->app), *name=SAVESTR(line->name);
    char *cond=line->cond;
    char *val=line->val;
    char *group=SAVESTR(line->group);

    otherrock.app=app;
    otherrock.name=name;
    otherrock.cond=cond;
    otherrock.pd=NULL;
    /* if a type was specified don't bother with the lookup */
   if(!line->type || self->readingdefaults) list_Enumerate(self->prefs, FindDesc, &otherrock);
    if(otherrock.pd==NULL) {
	if(line->shadow) fprintf(stderr, "prefs WARNING: no match found for shadow preference %s.%s\n", app, name);
	desc=NewPrefDesc(self, line->type?line->type:"String");
	desc->helppos=line->helppos;
	if(line->expert>=0) desc->expert=line->expert;
	if(line->vl && list_Size(line->vl)>0) desc->defviews=CopyViewList(line->vl);
	else {
	    desc->defviews=list_New();
	    list_InsertEnd(desc->defviews, SAVESTR("pvaltv"));
	}
	if(line->views) desc->views=SAVESTR(line->views);
	else desc->views=SAVESTR("pvaltv");
	if(!self->readingdefaults) {
	    if(line->type) desc->writetype=TRUE;
	    if(line->vl) desc->writeviews=TRUE;
	}
	newobj=TRUE;
    } else {
	desc=otherrock.pd;
	if(!line->shadow && (app==desc->app) && (cond==desc->cond || (cond && desc->cond && strcmp(cond, desc->cond)==0)) && ListsEqual(line->vl, desc->defviews)) {
	    obj=desc->obj;
	    if(!self->readingdefaults) {
		if(desc->prevlines) freestringlist(desc->prevlines);
		if(line->prevlines) {
		    desc->prevlines=CopyList(line->prevlines);
		} else desc->prevlines=NULL;
	    }
	} else {
	    group=desc->group;
	    desc=DuplicateDesc(desc);
	    if(line->expert>=0) desc->expert=line->expert;
	    if(line->helppos>=0) {
		desc->helppos=line->helppos;
	    }
	    if(line->shadow) {
		desc->shadow=TRUE;
		desc->group=SAVESTR(line->group);
		desc->obj=otherrock.pd->obj;
		desc->freeobject=FALSE;
		list_InsertEnd(self->prefs, desc);

		if(!self->readingdefaults) {
		    if(desc->prevlines) freestringlist(desc->prevlines);
		    if(line->prevlines) {
			desc->prevlines=CopyList(line->prevlines);
		    } else desc->prevlines=NULL;
		}
	    } else newobj=TRUE;
	    if(line->vl) {
		desc->views=SAVESTR(line->views);
		
		if(desc->defviews) {
		    list_Destroy(desc->defviews);
		}
		desc->defviews=CopyViewList(line->vl);
		if(!self->readingdefaults) desc->writeviews=TRUE;
	    }
	}
    }
    
    if(newobj) {
	long spos;
	obj=prefval_New();
	if(obj==NULL) return;
	if(desc->app!=app) {
	    desc->app=app;
	}
	if(desc->name!=name) {
	    desc->name=name;
	}
	if(desc->group!=group) {
	    desc->group=group;
	}
	zfree(desc->cond);
	desc->cond=STRDUP(cond);
	if(!self->readingdefaults) {
	    if(desc->prevlines) freestringlist(desc->prevlines);
	    if(line->prevlines) {
		desc->prevlines=CopyList(line->prevlines);
	    } else desc->prevlines=NULL;
	}
	desc->obj=obj;
	desc->freeobject=TRUE;
	if(line->low<=line->high) {
	    desc->low=line->low;
	    desc->high=line->high;
	}
	list_InsertEnd(self->prefs, desc);
	prefval_SetAppName(obj, app);
	prefval_SetPrefName(obj, name); 

	prefval_SetType(obj, desc->type);
	prefval_SetListMax(obj, desc->listsize);
	prefval_SetSeparator(obj, desc->seps);
	prefval_SetCondition(obj, cond);
	prefval_SetRangeLow(obj, desc->low);
	prefval_SetRangeHigh(obj, desc->high);
	prefval_AddObserver(obj, self);
    }
    if(self->readingdefaults) {
	zfree(desc->val);
	desc->val=STRDUP(val);
    }
    p=val;
    if(!desc->indefs) desc->indefs=self->readingdefaults;
    if(p!=NULL && !desc->shadow) {
	if(!self->readingdefaults && desc->order>=0) {
	    fprintf(stderr, "Preferences Warning: multiple settings for %s.%s, first setting taken.\nFirst setting was: %s\nLater settings for %s.%s will be removed when you save your preferences.\n", desc->app, desc->name, prefval_PreferenceString(desc->obj), desc->app, desc->name);
	    return;
	}
	while(isspace(*p)) p++;
	prefval_SetFromPreferenceString(desc->obj, p);
	if(self->readingdefaults) prefval_SetDefault(desc->obj);
	else {
	    if(self->maxorder<*order) self->maxorder=(*order);
	    desc->order=(*order)++;
	}
    }
}


static char *myindex(p, ch)
char *p;
char ch;
{
    boolean haveslash=FALSE;
    while(*p) {
	if(!haveslash && *p==ch) return p;
	if(*p=='\\')  haveslash=!haveslash;
	else haveslash=FALSE;
	p++;
    }
    return NULL;
}

static struct wbuf {
    char *buf;
    int size;
} typebuf={
    NULL, 0
};

static struct wbuf viewsbuf={
    NULL, 0
};

static struct wbuf groupbuf={
    NULL,
    0
};

static struct wbuf appbuf={
    NULL,
    0
};
static struct wbuf namebuf={
    NULL,
    0
};
static struct wbuf valbuf={
    NULL,
    0
};

static struct wbuf condbuf={
    NULL,
    0
};

static char * SetBuf(wb, str)
struct wbuf *wb;
char *str;
{
    if(str==NULL) str="";
    if(wb->buf==NULL) {
	wb->size=strlen(str)+1;
	wb->buf=(char *)malloc(wb->size);
    } else {
	int len=strlen(str)+1;
	if(len>wb->size) {
	    wb->buf=(char *)realloc(wb->buf, len);
	    if(wb->buf==NULL) {
		wb->size=0;
		return NULL;
	    }
	    wb->size=len;
	}
    }
    strcpy(wb->buf, str);
    return wb->buf;
}

#ifndef MAX
#define MAX(x,y) ((x)<(y)?y:x)
#endif

static void AddHelpStyle(self, pos, len, type)
struct prefs *self;
long pos;
long len;
enum style_type type;
{
    if(self->llistcnt>=self->llistsize) {
	if(self->llist==NULL) {
	    self->llistsize=MAX(300, self->llistcnt+1);
	    self->llist=(struct hstyles *)malloc(self->llistsize*sizeof(struct hstyles));
	    if(self->llist==NULL) return;
	} else {
	    self->llistsize+=100;
	    self->llist=(struct hstyles *)realloc(self->llist, self->llistsize*sizeof(struct hstyles));
	    if(self->llist==NULL) return;
	}
    }
    self->llist[self->llistcnt].pos=pos;
    self->llist[self->llistcnt].len=len;
    self->llist[self->llistcnt].type=type;
    self->llistcnt++;
}

#define TypeBufSet(str) SetBuf(&typebuf, str)
#define ViewsBufSet(str) SetBuf(&viewsbuf, str)
#define GroupBufSet(str) SetBuf(&groupbuf, str)
#define AppBufSet(str) SetBuf(&appbuf, str)
#define NameBufSet(str) SetBuf(&namebuf, str)
#define ValBufSet(str) SetBuf(&valbuf, str)
#define CondBufSet(str) SetBuf(&condbuf, str)

static void AddCategory(self, group, pos)
struct prefs *self;
char *group;
long pos;
{
    struct prefgroup *pg=(struct prefgroup *)list_Enumerate(self->categories, FindCat, group);
    if(pg==NULL) {
	pg=(struct prefgroup *)malloc(sizeof(struct prefgroup));
	if(pg==NULL) return;
	pg->grouphelp=(-1);
	list_InsertEnd(self->categories, pg);
	pg->name=SAVESTR(group);
    }
    if(pos>=0) pg->grouphelp=pos;
}

static boolean ReadPrefLine(self,line)
struct prefs *self;
struct prefline *line;
{
    char *buf=NULL, *pp, *app, *name, *val;
    boolean errorflag=FALSE;
    boolean seentype=FALSE;
    boolean seenviews=FALSE;
    boolean seengroup=FALSE;
    boolean seenhelp=FALSE;
    boolean addedcategory=FALSE;
    while(1) {
	pp=buf=ReadLine(self);
	if(buf==NULL) {
	    return FALSE;
	}
	if(buf[0]=='#' && buf[1]!='~') {
	    if(!self->readingdefaults) {
		if(line->prevlines==NULL) {
		    line->prevlines=list_New();
		    if(line->prevlines==NULL) return FALSE;
		}
		list_InsertEnd(line->prevlines, STRDUP(buf));
	    }
	    continue;
	} else if(buf[0]=='#') {
	    char *p=index(buf+2, ' ');
	    if(p!=NULL) p++;
	    if(strncmp(buf+2, "type ", 5)==0) {
		if(p==NULL) continue;
		if(seentype) {
		    fprintf(stderr, "prefs WARNING: already saw type!\n");
		    errorflag=TRUE;
		}
		seentype=TRUE;
		line->type=TypeBufSet(p);
		continue;
	    } else if(strncmp(buf+2, "views ", 6)==0 || strncmp(buf+2, "view ", 5)==0) {
		if(p==NULL) continue;
		if(seenviews) {
		    fprintf(stderr, "prefs WARNING: already saw views!\n");
		    errorflag=TRUE;
		}
		seenviews=TRUE;
		line->views=ViewsBufSet(p);
		if(line->vl) list_Destroy(line->vl);
		line->vl=HandleViewList(self, p);
		continue;
	    } else if(strncmp(buf+2, "group ", 6)==0) {
		if(p==NULL) continue;
		if(seengroup) {
		    fprintf(stderr, "prefs WARNING: already saw group!\n");
		    errorflag=TRUE;
		}
		seengroup=TRUE;
		line->group=GroupBufSet(p);
		continue;
	    } else if(strcmp(buf+2, "shadow")==0) {
		line->shadow=TRUE;
		continue;
	    } else if(strncmp(buf+2, "range ", 6)==0) {
		if(p==NULL) continue;
		line->low=atol(p);
		p=index(p, ' ');
		if(p==NULL) continue;
		line->high=atol(p+1);
		continue;
	    } else if(strncmp(buf+2, "help", 4)==0) {
		long pos=prefs_GetFence(self);
		long epos=pos;
		long hpos=text_GetLength(self->help);
		if(seenhelp) {
		    fprintf(stderr, "prefs WARNING: already saw help!\n");
		    errorflag=TRUE;
		}
		seenhelp=TRUE;
		while(buf) {
		    epos=prefs_GetFence(self);
		    buf=ReadLine(self);
		    if(strcmp(buf, "#~endhelp")==0) break;
		}
		text_CopyText(self->help, hpos, self, pos, epos-pos);
		text_InsertCharacters(self->help, hpos+epos-pos, "\n", 1);
		line->helppos=hpos;
		continue;
	    } else if(strncmp(buf+2, "grouphelp", 9)==0) {
		long pos=prefs_GetFence(self);
		long epos=pos;
		long hpos=text_GetLength(self->help);
		while(buf) {
		    epos=prefs_GetFence(self);
		    buf=ReadLine(self);
		    if(strcmp(buf, "#~endhelp")==0) break;
		}
		text_CopyText(self->help, hpos, self, pos, epos-pos);
		text_InsertCharacters(self->help, hpos+epos-pos, "\n", 1);
		if(line->group) {
		    addedcategory=TRUE;
		    AddCategory(self, line->group, hpos);
		}
		continue;
	    } else if(strncmp(buf+2, "expert", 5)==0) {
		line->expert=1;
		continue;
	    } else if(strncmp(buf+2, "novice", 5)==0) {
		line->expert=0;
		continue;
	    } else if(strncmp(buf+2, "PrefEdVersion ", 14)==0) {
		long version=atol(p);
		if(version>prefs_DS_VERSION) {
		    fprintf(stderr, "prefs WARNING: file was last written by a latter version of the preferences editor.\n");
		}
		self->version=version;
		continue;
	    }
	} else if(buf[0]=='?') {
	    char *p=myindex(buf, ':'); 
	    if(p) {
		struct prefval *obj;
		*p='\0';
		line->cond=CondBufSet(buf+1);
		pp=p+1;
	    } else fprintf(stderr, "prefs: Warning malformed conditional preference:\n%s\n",buf);
	}
	while(isspace(*pp)) pp++;
	if(*pp==0) {
	    if(!self->readingdefaults) {
		if(line->prevlines==NULL) {
		    line->prevlines=list_New();
		    if(line->prevlines==NULL) return FALSE;
		}
		list_InsertEnd(line->prevlines, STRDUP(buf));
	    }
	    continue;
	}
	
	val=index(pp, ':');
	if(val) {
	    *val='\0';
	    val++;
	    while(isspace(*val)) val++;
	}
	name=index(pp, '.');
	if(name) {
	    *name='\0';
	    name++;
	    app=pp;
	} else {
	    app="*";
	    name=pp;
	};
	if(line->group) {
	    self->lastgroup=SAVESTR(line->group);
	    if(!addedcategory) AddCategory(self, line->group, -1);
	} else line->group=self->lastgroup;
	line->app=AppBufSet(app);
	line->name=NameBufSet(name);
	if(errorflag) {
	    fprintf(stderr, "prefs WARNING: error in description for preference: %s.%s\n", line->app?line->app:"(NULL)", line->name?line->name:"(NULL)");
	}
	if(name && line->helppos>=0) {
	    long len=strlen(name), len2;
	    long pos=line->helppos;
	    AddHelpStyle(self, pos, len, PrefsSubsection);
	    text_AlwaysInsertCharacters(self->help, pos, name, len);
	    pos+=len;
	    if(self->lastgroup) {
		text_AlwaysInsertCharacters(self->help, pos, " (", 2);
		pos+=2;
		len=strlen(self->lastgroup);
		AddHelpStyle(self, pos, len, PrefsGroupname);
		text_AlwaysInsertCharacters(self->help, pos, self->lastgroup, len);
		pos+=len;
		text_AlwaysInsertCharacters(self->help, pos, ")", 1);
		pos++;
	    }	    
	    text_AlwaysInsertCharacters(self->help, pos, "\n", 1);
	}
	line->val=ValBufSet(val);
	return TRUE;
    }
}

#define SAFESTR(x) (x?x:"")

static int sortbyname(pd1, pd2)
struct prefdesc *pd1, *pd2;
{
    int result=lc_strcmp((pd1)->name, (pd2)->name);
    if(result!=0) return result;
    else {
	result=lc_strcmp((pd1)->app, (pd2)->app);
	if(result!=0) return result;
	else return lc_strcmp(SAFESTR((pd1)->group), SAFESTR((pd2)->group));
    }
}

static int sortbyapp(pd1, pd2)
struct prefdesc *pd1, *pd2;
{
    int result=lc_strcmp((pd1)->app, (pd2)->app);
    if(result!=0) return result;
    else {
	result=lc_strcmp(SAFESTR((pd1)->group), SAFESTR((pd2)->group));
	if(result!=0) return result;
	else return lc_strcmp((pd1)->name, (pd2)->name);
    }
}


static int sortbygroup(pd1, pd2)
struct prefdesc *pd1, *pd2;
{
    int result=lc_strcmp(SAFESTR((pd1)->group), SAFESTR((pd2)->group));
    if(result!=0) return result;
    else {
	result=lc_strcmp((pd1)->app, (pd2)->app);
	if(result!=0) return result;
	else return lc_strcmp((pd1)->name, (pd2)->name);
    }
}

static boolean sane=FALSE;
#define ISWILD(p) (p==NULL || (p[0]=='*' && p[1]=='\0'))

static int sortbyorder(pd1, pd2)
struct prefdesc *pd1, *pd2;
{
    if(sane) {
	if((pd1->name==pd2->name) && ISWILD(pd1->app) && !ISWILD(pd2->app)) return 1;
	if((pd1->name==pd2->name) && ISWILD(pd2->app) && !ISWILD(pd1->app)) return -1;
    }
    if((pd1)->order==-1 && (pd2)->order!=-1) return 1;
    if((pd2)->order==-1 && (pd1)->order!=-1) return -1;
    return (pd1)->order-(pd2)->order;
}

static int (*sortprocs[])()={
    sortbyname,
    sortbyapp,
    sortbygroup,
    sortbyorder
};

static int catsort(pg1, pg2)
struct prefgroup *pg1, *pg2;
{
    return ULstrcmp(pg1->name, pg2->name);
}

void prefs__Sort(self, sortby, perm)
struct prefs *self;
enum prefs_SortType sortby;
boolean perm;
{
    if((int)sortby>=(int)prefs_MaxSortType) return;

    if(perm) self->sortby=sortby;

    sane=self->sane;
    
    list_Sort(self->prefs, sortprocs[(int)sortby]);
    list_Sort(self->categories, catsort);
}

static boolean ResetOrder(pd, self)
struct prefdesc *pd;
struct prefs *self;
{
    pd->order=(-1);
    return TRUE;
}

long prefs__ReadDataPart(self)
struct prefs *self;
{
    /*
      Read in the object from the file.
      */
    long err=dataobject_NOREADERROR;
    struct prefline line;
    boolean cont=TRUE;
    int order=0;
    if(!self->readingdefaults) {
	list_Enumerate(self->prefs, ResetOrder, self);
	self->maxorder=(-1);
    }
    self->lastgroup=SAVESTR("---Unkown---");
    do {
	bzero(&line, sizeof(line));
	line.low=line.high+1;
	line.expert=(-1);
	line.helppos=(-1);
	cont=ReadPrefLine(self, &line);
	if(!cont) {
	    self->lastline=line;
	    continue;
	}	    
	HandlePref(self, &line, &order);
	if(line.vl) {
	    list_Destroy(line.vl);
	    line.vl=NULL;
	}
	if(line.prevlines) {
	    freestringlist(line.prevlines);
	    line.prevlines=NULL;
	}
    } while (cont);
    if(!self->readingdefaults) {
	prefs_Sort(self, prefs_Order, TRUE);
    }
    if(self->readingdefaults || self->version!=-1 || !environ_GetProfileSwitch("KeepOrigPrefs", TRUE)) {
	prefs_UpdateText(self);
    }
    return err;
}

static boolean ResetValues(pd, self)
struct prefdesc *pd;
struct prefs *self;
{

    if(pd->val) {
	prefval_SetFromPreferenceString(pd->obj, pd->val);
    } else {
	prefval_SetFromPreferenceString(pd->obj, "");
    }
    prefval_SetDefault(pd->obj);
    return TRUE;
}

long prefs__ReScan(self)
struct prefs *self;
{
    long err;
    list_Enumerate(self->prefs, ResetValues, self);
    prefs_SetReadingDefaults(self, FALSE);
    err=prefs_ReadDataPart(self);
    ApplyHelpStyles(self);
    return err;
}

long prefs__Read(self, fp, id)
struct prefs *self;
FILE *fp;
long id;
{
    long err=dataobject_NOREADERROR;
    endflag=TRUE;
    err=super_Read(self, fp, id);
    if(err<0) return err;
    prefs_ReScan(self);
    return err;
}

static boolean PrefvalP(pd, obj)
struct prefdesc *pd;
struct prefval *obj;
{
    if(pd->obj==obj) return FALSE;
    else return TRUE;
}

void prefs__ObservedChanged(self, changed, val)
struct prefs *self;
struct observable *changed;
long val;
{
    static struct classinfo *prefval_ci=NULL;
    struct prefdesc *pd=NULL;
    if(prefval_ci==NULL) prefval_ci=class_Load("prefval");
    if(val!=observable_OBJECTDESTROYED && class_GetType(changed)==prefval_ci && (pd=(struct prefdesc *)list_Enumerate(self->prefs, PrefvalP, changed))) {
	prefs_UpdateOneInText(self, pd);
	self->selfmod=TRUE;
	prefs_NotifyObservers(self, observable_OBJECTCHANGED);
	self->selfmod=FALSE;
    }
    super_ObservedChanged(self, changed, val);
}

struct writerock {
    struct prefs *self;
    FILE *fp;
    void (*writefunc)();
    char *buf;
    long pos, len;
};

static boolean PrintPrevLines(line, rock)
char *line;
struct writerock *rock;
{
    sprintf(rock->buf, "%s\n", line);
    rock->writefunc(rock, NULL);
    return TRUE;
}

static boolean writeprefs(pd, rock)
struct prefdesc *pd;
struct writerock *rock;
{
    struct prefs *self=rock->self;
    FILE *fp=rock->fp;
    char wbuf[10240];
    rock->buf=wbuf;
    if(pd->shadow) return TRUE;
    if(pd->obj && !prefval_GetIsDefault(pd->obj)) {
	if(pd->prevlines) {
	    list_Enumerate(pd->prevlines, PrintPrevLines, rock);
	}
	if(pd->writetype) {
	    sprintf(wbuf, "##type %s%s%s%s%s\n", prefval_TypeString(pd->obj), (prefval_GetListMax(pd->obj) > 1) ? " " : "",
		(prefval_GetListMax(pd->obj) > 1) ? "List" : "",
		    (prefval_GetSeparator(pd->obj)) ? " " : "",
		    prefval_GetSeparator(pd->obj)?prefval_GetSeparator(pd->obj):"");
	    rock->writefunc(rock, NULL);
	}
	
	if(pd->writeviews && pd->views) {
	    sprintf(wbuf, "##view %s\n", pd->views);
	    rock->writefunc(rock, NULL);
	}
	if(pd->cond) {
	    sprintf(wbuf, "?%s:", pd->cond);
	} else wbuf[0]='\0';
	sprintf(wbuf+strlen(wbuf), "%s\n", prefval_PreferenceString(pd->obj));
	rock->writefunc(rock, pd);
    } else {
	if(pd->pm) {
	    prefs_RemoveMark(rock->self, pd->pm);
	    mark_Destroy(pd->pm);
	    pd->pm=NULL;
	}
    }
    return TRUE;
}

long prefs__WritePlain(self, file, writeID, level)
struct prefs *self;
FILE *file;
long writeID;
int level;
{
    return super_Write(self, file, writeID, level);
}


static void twrite(rock, pd)
struct writerock *rock;
struct prefdesc *pd;
{
    int len=strlen(rock->buf);
    prefs_AlwaysReplaceCharacters(rock->self, rock->pos, rock->len, rock->buf, len);
    if(pd) {
	if(pd->pm) {
	    prefs_RemoveMark(rock->self, pd->pm);
	    mark_Destroy(pd->pm);
	}
	pd->pm=prefs_CreateMark(rock->self, rock->pos, len-1);
	mark_SetStyle(pd->pm, FALSE, FALSE);
    }
    rock->pos+=len;
}

void prefs__UpdateOneInText(self, pd)
struct prefs *self;
struct prefdesc *pd;
{
    char *nval=prefval_FullPreferenceString(pd->obj);
    int len;
    nval=nval?nval:"";
    len=strlen(nval);
    if(pd->pm==NULL) {
	long pos=prefs_GetLength(self);
	if(prefval_GetIsDefault(pd->obj)) return;
	prefs_InsertCharacters(self, pos, nval, len);
	prefs_InsertCharacters(self, prefs_GetLength(self), "\n", 1);
	pd->pm=prefs_CreateMark(self, pos, len);
	mark_SetStyle(pd->pm, FALSE, FALSE);
	pd->order= ++self->maxorder;
	return;
    }
    if(mark_GetLength(pd->pm)==0) {
	fprintf(stderr, "Preference %s.%s has been deleted from text.\n", pd->app?pd->app:"(NULL)", pd->name?pd->name:"(NULL)");
	return;
    }
    prefs_AlwaysReplaceCharacters(self, mark_GetPos(pd->pm), mark_GetLength(pd->pm), nval, len);
    mark_SetLength(pd->pm, len);    
}

void prefs__UpdateText(self)
struct prefs *self;
{
    struct writerock thisrock;
    char buf[1024];
    prefs_AlwaysDeleteCharacters(self, 0, prefs_GetLength(self));
    prefs_SetWriteID(self,im_GetWriteID());
    thisrock.self=self;
    thisrock.fp=NULL;
    thisrock.writefunc=(void (*)())twrite;
    thisrock.len=0;
    prefs_Sort(self, prefs_Order, FALSE);
    sprintf(buf, "#~PrefEdVersion %d\n", prefs_DS_VERSION);
    prefs_AlwaysInsertCharacters(self, 0, buf, strlen(buf));
    thisrock.pos=prefs_GetLength(self);
    list_Enumerate(self->prefs, writeprefs, &thisrock);
    prefs_Sort(self, self->sortby, FALSE);
    if(self->lastline.prevlines) list_Enumerate(self->lastline.prevlines, PrintPrevLines, &thisrock);
}

static void filewrite(rock, pd)
struct writerock *rock;
struct prefdesc *pd;
{
    fprintf(rock->fp, "%s", rock->buf);
}

long prefs__Write(self, file, writeID, level)
struct prefs *self;
FILE *file;
long writeID;
int level;
{
    struct writerock thisrock;
    if (prefs_GetWriteID(self) != writeID)  {
	if(level) super_Write(self, file, writeID, level);
	else {
	    prefs_SetWriteID(self,writeID);
	    thisrock.self=self;
	    thisrock.fp=file;
	    thisrock.writefunc=(void (*)())filewrite;
	    prefs_Sort(self, prefs_Order, FALSE);
	    fprintf(file, "#~PrefEdVersion %d\n", prefs_DS_VERSION);
	    list_Enumerate(self->prefs, writeprefs, &thisrock);
	    prefs_Sort(self, self->sortby, FALSE);
	    if(self->lastline.prevlines) list_Enumerate(self->lastline.prevlines, PrintPrevLines, &thisrock);
	}
    }
    return prefs_GetID(self);
}


static char *ProcessLine(buf)
char *buf;
{
    char *p, *q=buf;

    while(p=index(q, '\\')) {
	if(p[1]=='\\') {
	    p[0]=p[1];
	    strcpy(p+1, p+2);
	    q=p+1;
	} else {
	    *p=='\n';
	}
    }
    return buf;
}

#if 0
    /* Make sure that gap is at end of buffer so that GetBuf
       gives us the whole buffer, not just the part before the gap.  */
    text_GetGap(txtobj, text_GetLength(txtobj), 0L);
    buf = text_GetBuf(txtobj, 0L, text_GetLength(txtobj), &length);
#endif
#define BUFSTEP 400
static char *ReadLine(self)
struct prefs *self;
{
    static int bufsize=0;
    int bp=0;
    static char *buf=NULL;
    
    long pos=prefs_GetFence(self); /* use the fence to track our position. */
    long epos=0;

   
    if(self==NULL) {
	zfree(buf);
	return NULL;
    }

    if(buf==NULL) {
	bufsize=BUFSTEP;
	buf=malloc(bufsize);
    }

    if(buf==NULL) return NULL;

    if(pos>=prefs_GetLength(self)) return NULL;
    
    epos=prefs_Index(self, pos,  '\n', prefs_GetLength(self)-pos);

    if(epos<0 || epos>prefs_GetLength(self)) epos=prefs_GetLength(self)-1;

    if(epos-pos>bufsize) {
	buf=realloc(buf, epos-pos+1);
	if(buf==NULL) return NULL;
    }
    
    prefs_CopySubString(self, pos, epos-pos, buf, FALSE);

    prefs_SetFence(self, epos+1);

    return buf;
}


char *prefs__ViewName(self)
struct prefs *self;
{
    return "pintv";
}


#if 0
    switch(self->sortby) {
	case prefs_App:
	    header=prefval_GetAppName(obj);
	    break;
	case prefs_Name:
	    header=prefval_GetPrefName(obj);
	    break;
	case prefs_Group:
	    header=pd->group;
	    break;
	case prefs_Order:
	    header=NULL;
	default:
	    header="ERROR!";
    }
    spos=lpos=prefs_GetLength(self);
    if(header) {
	if(self->lastheader==NULL || !FOLDEDEQ(self->lastheader, header)) {
	    self->lastheader=SAVESTR(header);
	    len=strlen(header);
	    prefs_AlwaysInsertCharacters(self, lpos, header, len);
	    lpos+=len;
	    prefs_AlwaysInsertCharacters(self, lpos, "\n\n", 2);
	    if(self->hstyle) prefs_AlwaysAddStyle(self, spos, lpos-spos, self->hstyle);
	    lpos+=2;
	}
    }
    spos=lpos;
    if(prefval_GetAppName(obj)) {
	len=strlen(prefval_GetAppName(obj));
	prefs_AlwaysInsertCharacters(self, lpos, prefval_GetAppName(obj), len);
	lpos+=len;
	prefs_AlwaysInsertCharacters(self, lpos, ".", 1);
	lpos++;
    }
    if(prefval_GetPrefName(obj)) {
	len=strlen(prefval_GetPrefName(obj));
	prefs_AlwaysInsertCharacters(self, lpos, prefval_GetPrefName(obj), len);
	lpos+=len;
    }
    prefs_AlwaysInsertCharacters(self, lpos, ":", 1);

    if(self->pstyle) prefs_AlwaysAddStyle(self, spos, lpos - spos, self->pstyle);

    lpos++;

    if(pd->defviews) {
	thisrock.self=self;
	thisrock.pos=lpos;
	thisrock.obj=obj;
	list_Enumerate(pd->defviews, AddViews, &thisrock);
	lpos=thisrock.pos;
    } else {
	prefs_AlwaysAddView(self, lpos, "pvaltv", obj);
	prefs_AlwaysInsertCharacters(self, lpos+1, "\n", 1);
	lpos+=2;
    }
    prefs_AlwaysInsertCharacters(self, lpos, "\n", 1);
    if(pd->mark) {
	prefs_RemoveMark(self, pd->mark);
	mark_Destroy(pd->mark);
    }
    pd->mark=prefs_CreateMark(self, spos, lpos+2-spos);
    return TRUE;
#endif
