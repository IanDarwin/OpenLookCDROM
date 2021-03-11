/* Copyright Carnegie Mellon University 1992 All rights reserved.
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
static char *pintv_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pintv.c,v 1.30 1994/02/01 20:11:40 rr2b Exp $";
#endif /* NORCSID */

#include <andrewos.h>
#include <class.h>

#define dontDefineRoutinesFor_observable
#define dontDefineRoutinesFor_view
#include "pintv.eh"

#include <event.ih>
#include <observe.ih>
#include <text.ih>
#include <textv.ih>
#include <envrment.ih>
#include <stylesht.ih>
#include <style.ih>
#include <sbuttonv.ih>
#include <sbutton.ih>
#include <lpair.ih>
#include <label.ih>
#include <labelv.ih>
#include <view.ih>
#include <rect.h>
#include <util.h>
#include <message.ih>
#include <mark.ih>
#include <environ.ih>
#include <envrment.ih>
#include <viewref.ih>
#include <complete.ih>
#include <dataobj.ih>

#include <prefs.ih>
#include <prefval.ih>
#include <strcache.ih>

#include <phelpv.ih>

#define TEXT_VIEWREFCHAR '\377'

#define SAVESTR(str) (str?strcache_SaveStr(str):NULL)
#define TEXT(tv) ((struct text *)textview_GetDataObject(tv))
#define PREFS(pv) ((struct prefs *)pintv_GetDataObject(pv))
#define RFOLDEDEQ(x,y) ((x)==(y))

static char texteditatomstr[]="TextEdit";

static struct sbutton_list blist[]={
    {"Edit As Text", 0, texteditatomstr, FALSE},
    {NULL, 0, NULL, FALSE}
};

static struct labelview *MakeLabel(str)
char *str;
{
    struct label *l=label_New();
    struct labelview *lv=labelview_New();

    if(l==NULL) {
	if(lv) labelview_Destroy(lv);
	return NULL;
    } else if(lv==NULL) {
	if(l) label_Destroy(l);
	return NULL;
    }

    labelview_SetDataObject(lv, l);

    label_SetFlags(l, label_CENTERED | label_BOXED);

    label_SetText(l, str);

    return lv;
}

static void DestroyLabel(lv)
struct labelview *lv;
{
    struct label *l=(struct label *)labelview_GetDataObject(lv);
    label_Destroy(l);
    labelview_Destroy(lv);
}

static struct textview *MakeText()
{
    struct text *t;
    struct textview *tv;
    
    t=text_New();
    
    if(t==NULL) return NULL;

    text_ReadTemplate(t, "prefs", FALSE);

    text_SetReadOnly(t, TRUE);
    
    tv=textview_New();
    if(tv==NULL) {
	text_Destroy(t);
	return NULL;
    } 
    textview_SetDataObject(tv, t);
    return tv;
}

static void DestroyText(tv, tva)
struct textview *tv;
struct view *tva;
{
    struct text *t=TEXT(tv);
    textview_DeleteApplicationLayer(tv, tva);
    text_Destroy(t);
    textview_Destroy(tv);
}

static void DestroyButtons(sbv)
struct sbuttonv *sbv;
{
    struct sbutton *sb=(struct sbutton *)sbuttonv_GetDataObject(sbv);
    sbutton_Destroy(sb);
    sbuttonv_Destroy(sbv);
}

static char currlab[]="---Current---";
static char *currlabp=NULL;

extern void pintv_EditAsText();

boolean pintv__InitializeObject(classID, self)
struct classheader *classID;
struct pintv *self;
{
    struct sbutton_prefs *prefs=sbutton_GetNewPrefs("PrefEdButtons");
    struct sbutton *sb;

    if(prefs==NULL) return FALSE;

    self->autolist=environ_GetProfileSwitch("PrefEdAutoList", FALSE);
    sbutton_InitPrefs(prefs, "PrefEd");
    sbutton_InitPrefs(prefs, "PrefEdButtons");
    
    self->lockdown=FALSE;
    
    self->prefs = prefs;
    
    self->errors=NULL;
    self->reportevent=NULL;
    self->oevent=NULL;
    
    self->prefslist=NULL;
    
    self->cat_sel=self->pref_sel=NULL;

    currlabp=SAVESTR(currlab);
    
    self->category=currlabp;
    if(self->category==NULL) {
	sbutton_FreePrefs(prefs);
	return FALSE;
    }

    self->pref=NULL;

    self->helpv=NULL;
    self->helpva=NULL;
      
    self->top=lpair_New();
    self->labelpair=lpair_New();
    self->leftpair=lpair_New();
    self->rightpair=lpair_New();
    self->blr=lpair_New();
    
    self->catlabel=MakeLabel("Categories");
    self->plabel=MakeLabel("Preferences");
    self->categories=MakeText();
    self->preferences=MakeText();
    self->cpref=MakeText();
    
    self->categories_al = textview_GetApplicationLayer(self->categories);
    self->preferences_al = textview_GetApplicationLayer(self->preferences);

    self->cpref_al = textview_GetApplicationLayer(self->cpref);
    
    lpair_HSplit(self->labelpair, self->leftpair, self->rightpair, 60, TRUE);

    lpair_VTFixed(self->leftpair, self->catlabel, self->categories_al,  40, TRUE);
    lpair_VTFixed(self->rightpair, self->plabel, self->preferences_al, 40, TRUE);
    
    self->buttons = sbuttonv_CreateFilledSButtonv("sbuttonv", prefs, blist);
   
    sb=(struct sbutton *)sbuttonv_GetDataObject(self->buttons);
    sbutton_SetLayout(sb, 1, 1, sbutton_GrowColumns);

    sbutton_FreePrefs(prefs);

    sbutton_AddRecipient(sb, atom_Intern(texteditatomstr), self, pintv_EditAsText, NULL);
    lpair_VSplit(self->top, self->labelpair, self->blr, 80, TRUE);
    lpair_VSplit(self->blr, self->cpref_al, NULL, 30, TRUE);
    pintv_VFixed(self, self->top, self->buttons, 40, TRUE);
    
    self->redosizes=TRUE;
    
    return TRUE;
}

static void DestroyLpair(lp)
struct lpair *lp;
{
    lpair_Destroy(lp);
}

static void ClearLpair(lp)
struct lpair *lp;
{
    lpair_SetNth(lp, 0, NULL);
    lpair_SetNth(lp, 1, NULL);
}

static void UpdatePrefs();
static void ClearCPref();
static void UpdateCPref();
static struct environment *SelectLine();

void pintv__ObservedChanged(self, changed, value)
struct pintv *self;
struct observable *changed;
long value;
{
    if((struct observable *)self->errors==changed && value==-1) self->errors=NULL;
    if((struct observable *)PREFS(self)==changed && value!=(-1) && !PREFS(self)->selfmod) {
	self->lockdown=TRUE;
	self->category=NULL;
	if(self->cat_sel) self->cat_sel=SelectLine(self->categories, self->cat_sel, FALSE);
	if(self->pref_sel) self->pref_sel=SelectLine(self->preferences, self->pref_sel, FALSE);
	UpdatePrefs(self);
	self->pref=NULL;
	if(self->autolist) UpdateCPref(self);
	else ClearCPref(self);
    }
    super_ObservedChanged(self, changed, value);
}

void pintv__FinalizeObject(classID, self)
struct classheader *classID;
struct pintv *self;
{
    if(self->errors) text_RemoveObserver(self->errors, self);
    ClearLpair((struct lpair *)self);
    ClearLpair(self->top);
    ClearLpair(self->labelpair);
    ClearLpair(self->leftpair);
    ClearLpair(self->rightpair);
    ClearLpair(self->blr);
    if(self->reportevent) {
	event_Cancel(self->reportevent);
    	self->reportevent=NULL;
    }
    if(self->oevent) {
	event_Cancel(self->oevent);
	self->oevent=NULL;
    }
    if(self->errors) text_Destroy(self->errors);
    DestroyText(self->cpref, self->cpref_al);
    DestroyText(self->preferences, self->preferences_al);
    DestroyText(self->categories, self->categories_al);
    DestroyLabel(self->plabel);
    DestroyLabel(self->catlabel);
    DestroyButtons(self->buttons);
    DestroyLpair(self->top);
    DestroyLpair(self->labelpair);
    DestroyLpair(self->leftpair);
    DestroyLpair(self->rightpair);
    DestroyLpair(self->blr);
    if(self->helpva) phelpv_DeleteApplicationLayer(self->helpv, self->helpva);
    if(self->helpv) phelpv_Destroy(self->helpv);
}

boolean pintv__InitializeClass(classID)
struct classheader *classID;
{
    return TRUE;
}

void pintv__FullUpdate(self, type, left, top, width, height)
struct pintv *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle bounds;
    pintv_GetVisualBounds(self, &bounds);
    if(self->redosizes) {
	long dw=bounds.width, dh=40;
	enum view_DSattributes vdsa=sbuttonv_DesiredSize(self->buttons, bounds.width, bounds.height, view_HeightFlexible, &dw, &dh);
	self->redosizes=FALSE;
	if(dh<bounds.height) {
	    pintv_GetObjSize(self, 1)=dh;	
	    ((struct lpair *)self)->needsfull=TRUE;
	}
	vdsa=labelview_DesiredSize(self->catlabel, bounds.width, bounds.height, view_HeightFlexible, &dw, &dh);
	if(dh<bounds.height-pintv_GetObjSize(self, 1)) {
	    lpair_GetObjSize(self->leftpair, 0)=dh;
	    lpair_GetObjSize(self->rightpair, 0)=dh;
	    ((struct lpair *)self)->needsfull=TRUE;
	}	    
    }
    super_FullUpdate(self, type, left, top, width, height);
}

static struct environment *SelectLine(tv, oldsel, donew)
struct textview *tv;
struct environment *oldsel;
boolean donew;
{
    struct text *t=TEXT(tv);
    long line=text_GetLineForPos(t, textview_GetDotPosition(tv));
    long pos=text_GetPosForLine(t, line);
    long lend=text_Index(t, pos, '\n', text_GetLength(t)-pos);
    struct stylesheet *ss=text_GetStyleSheet(t);
    struct style *bold;
    if(oldsel) {
	text_RegionModified(t, environment_Eval(oldsel), environment_GetLength(oldsel));
	environment_Remove(oldsel, environment_Eval(oldsel), environment_GetLength(oldsel), environment_Style, FALSE);
    }
    if(!donew) {
	text_NotifyObservers(t, observable_OBJECTCHANGED);
	return NULL;
    }
    if(lend<0 || ss==NULL ) return NULL;
    bold=stylesheet_Find(ss, "bold");
    if(bold==NULL) return NULL;
    oldsel=text_AlwaysAddStyle(t, pos, lend-pos, bold);
    text_NotifyObservers(t, observable_OBJECTCHANGED);
    return oldsel;
}


struct addrock {
    struct pintv *self;
    struct style *bold;
    struct style *prefstyle;
    struct sbutton_prefs *prefs;
    long lastpos, npos;
    struct prefdesc *cpd;
};

static struct environment *AddView(self, pos, viewtype, dataobject)
struct text *self;
long pos;
char *viewtype;
struct dataobject *dataobject;
{
    struct environment *newenv=text_AlwaysWrapViewChar(self, pos, viewtype, dataobject);
  /* yes, this looks weird, but the viewref takes ownership of the object.... so all this does is decrement the reference count the code which adds prefvals to the queue grabs an extra reference so that this destroy won't remove the prefs ownership of the prefval */
    dataobject_Destroy(dataobject);
    return newenv;
}

static struct viewref *InsertObject(self, pos, name, viewname)
struct text *self;
long pos;
char *name;
char *viewname;
{
    struct dataobject *newobject;
    struct environment *env;

    if((newobject = (struct dataobject *) class_NewObject(name)))  {
        newobject->id = dataobject_UniqueID(newobject); 
        /* Register the object with the dictionary */
        /* is this needed? dictionary_Insert(NULL, (char *) newobject->id, (char *) newobject); */
        if (viewname == NULL || *viewname == '\0')
	    viewname = dataobject_ViewName(newobject);
        if (env = AddView(self, pos, viewname, newobject))
            return env->data.viewref;
    }
    return NULL;
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


static struct dstyle {
    long pos, len;
    struct dataobject *data;
    struct style *style;
    char *view;
    struct prefdesc *pd;
    struct text *pt;
    struct dstyle *next;
} *firststyle=NULL, *freedstyle=NULL;

static struct dstyle *GetDStyle(pt)
struct text *pt;
{
    struct dstyle *result;
    if(freedstyle) {
	result=freedstyle;
	freedstyle=freedstyle->next;
    } else {
	result=(struct dstyle *)malloc(sizeof(struct dstyle));
    }
    result->pt=pt;
    result->next=firststyle;
    firststyle=result;
    return result;
}

static void PushStyle(pt, pos, len, style)
struct text *pt;
struct style *style;
long pos, len;
{
    struct dstyle *n=GetDStyle(pt);
    if(n==NULL) return;
    n->view=NULL;
    n->data=NULL;
    n->pd=NULL;
    n->pos=pos;
    n->len=len;
    n->style=style;
}

static char viewrefchar='\377';
static void PushView(pt, pos, view, data)
struct text *pt;
struct dataobject *data;
char *view;
long pos;
{
    struct dstyle *n=GetDStyle(pt);
    if(n==NULL) return;
    text_AlwaysInsertCharacters(pt, pos, &viewrefchar, 1);
    n->pos=pos;
    n->view=view;
    n->data=data;
    n->style=NULL;
    n->pd=NULL;
}

static void PushObject(pt, pos, objname, dummy)
struct text *pt;
char *objname;
long pos;
char *dummy;
{
    struct dstyle *n=GetDStyle(pt);
    if(n==NULL) return;
    text_AlwaysInsertCharacters(pt, pos, &viewrefchar, 1);
    n->pos=pos;
    n->view=objname;
    n->data=NULL;
    n->style=NULL;
    n->pd=NULL;
}

static void PushMark(pt, pd, pos, len)
struct text *pt;
struct prefdesc *pd;
long pos, len;
{
    struct dstyle *n=GetDStyle(pt);
    if(n==NULL) return;
    n->data=NULL;
    n->pd=pd;
    n->pos=pos;
    n->len=len;
    n->style=NULL;
    n->data=NULL;
    n->view=NULL;
}

static void DoStyles(self)
struct pintv *self;
{
    struct dstyle *d=firststyle, *next;
    while(d) {
	next=d->next;
	if(d->pd) {
	    struct prefdesc *pd=d->pd;
	    if(pd->mark==NULL) {
		pd->mark=text_CreateMark(d->pt, d->pos, d->len);
		if(pd->mark) mark_SetStyle(pd->mark, FALSE, FALSE);
	    } else {
		mark_SetPos(pd->mark, d->pos);
		mark_SetLength(pd->mark, d->len);
		mark_SetStyle(pd->mark, FALSE, FALSE);
	    }
	} else if(d->style) {
	    AddStyle(d->pt, d->pos, d->len, d->style);
	} else if(d->data) {
	    AddView(d->pt, d->pos, d->view, d->data);
	} else if(d->view) {
	    InsertObject(d->pt, d->pos, d->view, NULL);
	}
	d->next=freedstyle;
	freedstyle=d;
	d=next;
    }
    firststyle=NULL;
}

static long AddVal(ct, pos, label, val, bolditalic)
struct text *ct;
char *label;
char *val;
boolean bolditalic;
{
    struct stylesheet *ss=text_GetStyleSheet(ct);
    struct style *bold=stylesheet_Find(ss, bolditalic?"bold":"italic");
    long len=strlen(label);
    text_AlwaysInsertCharacters(ct, pos, label, len);
    text_AlwaysInsertCharacters(ct, pos+len, ": ", 2);
#ifdef HIGHLIGHT_LABELS    
    if(bold) PushStyle(ct, pos, len, bold);
#endif /* HIGHLIGHT_LABELS */
    pos+=len+2;
    len=strlen(val);
    text_AlwaysInsertCharacters(ct, pos, val, len);
    text_AlwaysInsertCharacters(ct, pos+len, "\n", 1);
#ifndef HIGHLIGHT_LABELS /* HIGHLIGHT_VALUES */
    if(bold) PushStyle(ct, pos, len, bold);
#endif /* ndef HIGHLIGHT_LABELS */
    return pos+len+1;
}

static boolean AddViews(name, rock)
char *name;
struct addrock *rock;
{
    struct prefdesc *pd=rock->cpd;
    struct text *ct=TEXT(rock->self->cpref);
    text_AlwaysInsertCharacters(ct, rock->lastpos, "\n", 1);
    /* for convenience dealing with the other objects
     AddView assumes the owner doesn't want the object any
     more, for the prefvals we do want them, so we grab extra
     ownership of the object here. */
    dataobject_Reference(pd->obj);
    PushView(ct, rock->lastpos+1, name, (struct dataobject *)pd->obj);
    rock->lastpos+=2;
    return TRUE;
}

static boolean AddCommentLines(line, rock)
char *line;
struct addrock *rock;
{
    int len=strlen(line);
    struct text *pt=TEXT(rock->self->cpref);
    text_AlwaysInsertCharacters(pt, rock->lastpos, line, strlen(line));
    text_AlwaysInsertCharacters(pt, rock->lastpos+len, "\n", 1);
    rock->lastpos+=len+1;
    return TRUE;
}

static char deleteatomstr[]="Delete";
static char changeatomstr[]="ChangeCondition";
static char duplicateatomstr[]="Duplicate";
static char changeappatomstr[]="ChangeApp";
static char resetatomstr[]="Reset";


static struct sbutton_list blistt[]={
    {
	"Duplicate",
	0,
	duplicateatomstr,
	FALSE
    },
    {
	NULL, /* this will be filled in or not based on whether
	       a deletion of the given preference should be allowed */
	0,
	deleteatomstr,
	FALSE
    },
    {
	NULL,
	0,
	NULL,
	FALSE
    }

};

static struct sbutton_list blistc[]={
     {
	"Change",
	0,
	changeappatomstr,
	FALSE
    },
    {
	NULL,
	0,
	NULL,
	FALSE
    }
};


static struct sbutton_list blistc2[]={
    {
	"Change",
	0,
	changeatomstr,
	FALSE
    },
    {
	NULL,
	0,
	NULL,
	FALSE
    }
};

static struct sbutton_list blistd[]=
{
    {
	"Reset",
	0,
	resetatomstr,
	FALSE
    },
    {
	NULL,
	0,
	NULL,
	FALSE
    }
};


enum {
    None,
    CPUType,
    MachName,
    EnvVar,
    Cancel
} Choices;

static char *choices[]={
    "Always",
    "CPU Type",
    "Machine Name",
    "Environment Variable",
    "Cancel",
    NULL
};


static boolean getcondition(self, current)
struct pintv *self;
char **current;
{
    long result;
    int num=0;
    char *prompt, *newcond, *def=NULL;
    char buf[1024];
    static char defbuf[1024];

    if(*current) {
	switch(*current[0]) {
	    case 'C':
		num=CPUType;
		break;
	    case 'M':
		num=MachName;
		break;
	    case 'E':
		num=EnvVar;
		break;
	}
    }
    if(message_MultipleChoiceQuestion(self, 100, "Type of Conditional?", num, &result, choices, NULL)<0 || result==Cancel) {
	message_DisplayString(self, 0, "Cancelled!");
	return TRUE;
    }

    if(*current) def= *current+2;
    switch(result) {
	case CPUType: 
	    prompt="CPU Type?";
	    break;
	case MachName:
	    prompt="Machine Name?";
	    break;
	case EnvVar:
	    prompt="Expression like ENV=b, or ENV!=b.";
	    def=NULL;
	    if(*current) {
		if(*((*current)+1)=='!') {
		    char *c= *current+2;
		    if(strlen(c)>sizeof(defbuf)-1) break;
		    strcpy(defbuf, c);
		    c=index(defbuf, '=');
		    if(c==NULL) break;
		    *c='\0';
		    c=index(*current+2, '=');
		    strcat(defbuf, "!");
		    strcat(defbuf, c);
		    def=defbuf;
		} else def= *current+2;
	    } 
	    break;
	case None:
	    *current=NULL;
	    return FALSE;
    }

    if(message_AskForString(self, 100, prompt, (num==result)?def:NULL, buf, sizeof(buf))<0) {
	message_DisplayString(self, 0, "Cancelled!");
	return TRUE;
    }

    switch(result) {
	case CPUType:
	case MachName:
	    defbuf[0]=(result==CPUType)?'C':'M';
	    defbuf[1]='=';
	    strcpy(defbuf+2, buf);
	    break;
	case EnvVar: {
	    char *ne=index(buf, '!');
	    defbuf[0]='E';
	    defbuf[2]='\0';
	    if(ne) {
		defbuf[1]='!';
		if(ne[1]=='=') {
		    ne[0]='\0';
		    strcat(defbuf, buf);
		    strcat(defbuf, "=");
		    strcat(defbuf, ne+2);
		} else {
		    message_DisplayString(self, 0, "Invalid expression for environment variable test.");
		    return TRUE;
		}
	    } else {
		ne=index(buf, '=');
		defbuf[1]='=';
		if(ne==NULL) {
		    message_DisplayString(self, 0, "Invalid expression for environment variable test.");
		    return TRUE;
		}
		strcat(defbuf, buf);
	    }
	    }	    
    }
    *current=defbuf;
    return FALSE;
}

struct uniqrock {
    char *prefname;
    char *appname;
    char *cond;
};


static boolean checkuniq(pd, ur)
struct prefdesc *pd;
struct uniqrock *ur;
{
    if(strcmp(pd->name, ur->prefname)==0 && strcmp(pd->app, ur->appname)==0 && ((pd->cond==ur->cond) || (pd->cond && ur->cond && strcmp(pd->cond, ur->cond)==0))) return FALSE;
    return TRUE;
}


static boolean AddInstances();

static void FixPref(self, pd)
struct pintv *self;
struct prefdesc *pd;
{
    struct stylesheet *ss;
    struct addrock rock;
    
    if(pd->mark==NULL) {
	message_DisplayString(self, 100, "Couldn't find area in prefs text to delete!");
	return;
    }

    text_AlwaysDeleteCharacters(TEXT(self->cpref), mark_GetPos(pd->mark), mark_GetLength(pd->mark));


    ss=text_GetStyleSheet(TEXT(self->cpref));
    if(ss==NULL) return;

    rock.self=self;
    rock.bold = stylesheet_Find(ss, "bold");
    rock.prefstyle = stylesheet_Find(ss, "prefname");
    rock.prefs=self->prefs;
    rock.lastpos = 0;
    rock.npos = mark_GetPos(pd->mark);
    AddInstances(pd, &rock);
    DoStyles(self);
    text_NotifyObservers(TEXT(self->cpref), observable_OBJECTCHANGED);
}

static void AddPref(self, pd, pd2)
struct pintv *self;
struct prefdesc *pd;
struct prefdesc *pd2;
{
    struct stylesheet *ss;
    struct addrock rock;
    
    if(pd->mark==NULL) {
	message_DisplayString(self, 100, "Couldn't find area in prefs text to insert duplicate!");
	return;
    }

    ss=text_GetStyleSheet(TEXT(self->cpref));
    if(ss==NULL) return;

    rock.self=self;
    rock.bold = stylesheet_Find(ss, "bold");
    rock.prefstyle = stylesheet_Find(ss, "prefname");
    rock.prefs=self->prefs;
    rock.lastpos = 0;
    rock.npos = mark_GetPos(pd->mark)+mark_GetLength(pd->mark);
    AddInstances(pd2, &rock);
    DoStyles(self);
    if(self->cpref && pd2->mark) textview_SetTopPosition(self->cpref, mark_GetPos(pd2->mark));
    text_NotifyObservers(TEXT(self->cpref), observable_OBJECTCHANGED);
}

static void changecondition(self, sb, pd)
struct pintv *self;
struct sbutton *sb;
struct prefdesc *pd;
{
    struct uniqrock ur;
    char *newcond=pd->cond;
    struct prefdesc *pd2;
    
    if(getcondition(self, &newcond)) return;

    ur.cond=newcond;
    ur.appname=pd->app;
    ur.prefname=pd->name;
    
    pd2=(struct prefdesc *)list_Enumerate(PREFS(self)->prefs, checkuniq, &ur);
    if(pd2) {
	message_DisplayString(self, 30, "Such a preference for that application already exists!");
	return;
    }

    pd->cond=newcond?NewString(newcond):NULL;
    prefval_SetCondition(pd->obj, pd->cond);
    prefval_NotifyObservers(pd->obj, observable_OBJECTCHANGED);
    FixPref(self, pd);
    message_DisplayString(self, 0, "Condition changed.");
}

static void duplicate(self, sb, pd)
struct pintv *self;
struct sbutton *sb;
struct prefdesc *pd;
{
    struct uniqrock ur;
    struct prefs *prefs=PREFS(self);
    char buf[1024];
    struct prefdesc *pd2;
    char *cond;
    
    if(message_AskForString(self, 100,"New preference for which application?", NULL, buf, sizeof(buf))<0) {
	message_DisplayString(self, 0, "Cancelled!");
	return;
    }
    
    ur.cond=pd->cond;
    if(getcondition(self, &ur.cond)) return;
    ur.prefname=pd->name;
    ur.appname=buf;

    pd2=(struct prefdesc *)list_Enumerate(PREFS(self)->prefs, checkuniq, &ur);
    if(pd2) {
	message_DisplayString(self, 30, "Such a preference for that application already exists!");
	return;
    }
    pd2=prefs_DuplicatePref(prefs, pd, buf, ur.cond);
    PREFS(self)->selfmod=TRUE;
    prefs_NotifyObservers(PREFS(self), observable_OBJECTCHANGED);
    PREFS(self)->selfmod=FALSE;
    AddPref(self, pd, pd2);
    message_DisplayString(self, 0, "Preference duplicated.");
}

static void changeapp(self, sb, pd)
struct pintv *self;
struct sbutton *sb;
struct prefdesc *pd;
{
    struct uniqrock ur;
    char buf[1024];
    struct prefdesc *pd2;
    if(message_AskForString(self, 100,"Apply this preference to which application?", NULL, buf, sizeof(buf))<0) {
	message_DisplayString(self, 0, "Cancelled!");
	return;
    }
    
    ur.prefname=pd->name;
    ur.appname=buf;
    ur.cond=pd->cond;
    pd2=(struct prefdesc *)list_Enumerate(PREFS(self)->prefs, checkuniq, &ur);
    if(pd2) {
	message_DisplayString(self, 30, "Such a preference for that application already exists!");
	return;
    }
    pd->app=SAVESTR(buf);
    prefval_SetAppName(pd->obj, pd->app);

    FixPref(self, pd);
    message_DisplayString(self, 0, "Application changed.");
}


static void delete(self, sb, pd)
struct pintv *self;
struct sbutton *sb;
struct prefdesc *pd;
{
    long dotpos=textview_GetDotPosition(self->preferences);
    long toppos=textview_GetTopPosition(self->preferences);
    if(pd->mark==NULL) {
	message_DisplayString(self, 100, "Couldn't find area in prefs text to delete!");
	return;
    }
    
    text_AlwaysDeleteCharacters(TEXT(self->cpref), mark_GetPos(pd->mark), mark_GetLength(pd->mark));
    
    text_RemoveMark(TEXT(self->cpref), pd->mark);
    text_NotifyObservers(TEXT(self->cpref), observable_OBJECTCHANGED);
    
    prefs_DeletePref(PREFS(self), pd);

    UpdatePrefs(self);
    
    if(dotpos>text_GetLength(TEXT(self->preferences))) {
	dotpos=text_GetLength(TEXT(self->preferences));
    }
	
    if(toppos>text_GetLength(TEXT(self->preferences))) {
	toppos=text_GetLength(TEXT(self->preferences));
    }
    textview_SetTopPosition(self->preferences, toppos);
    textview_SetDotPosition(self->preferences, dotpos);
    textview_WantUpdate(self->preferences, self->preferences);
    
    PREFS(self)->selfmod=TRUE;
    prefs_NotifyObservers(PREFS(self), observable_OBJECTCHANGED);
    PREFS(self)->selfmod=FALSE;
    message_DisplayString(self, 0, "Preference deleted.");
}

static void reset(self, sb, pd)
struct pintv *self;
struct sbutton *sb;
struct prefdesc *pd;
{
    if(pd->indefs) {
	prefs_DeletePref(PREFS(self), pd);
	return;
    }
    
    if(pd->val) {
	prefval_SetFromPreferenceString(pd->obj, pd->val);
    } else {
	prefval_SetFromPreferenceString(pd->obj, "");
    }
    prefval_NotifyObservers(pd->obj, observable_OBJECTCHANGED);
    message_DisplayString(self, 0, "Preference reset to default.");
}


static void SetHelp(self, pd)
struct pintv *self;
struct prefdesc *pd;
{
    if(pd->helppos>=0 && self->helpv) {
	phelpv_SetTopPosition(self->helpv, pd->helppos);
	phelpv_SetDotPosition(self->helpv, pd->helppos);
	phelpv_SetDotLength(self->helpv, 0);
	phelpv_FrameDot(self->helpv, pd->helppos);
    }
}


#define INCLUDE(self, pd, autolist) ((autolist || pd->name==self->pref) && (pd->group==self->category || (self->category==currlabp && pd->order>=0)))

static boolean sethelp=FALSE;

static boolean AddInstances(pd, rock)
struct addrock *rock;
struct prefdesc *pd;
{
    struct pintv *self=rock->self;

    if(INCLUDE(self, pd, self->autolist)) {
	static char editors[]="Current Setting (";
	static char comments[]="Comments";
	static char def[]=" (";
	struct text *pt=TEXT(self->cpref);
	struct style *bold=rock->bold;
	struct sbutton_prefs *prefs=rock->prefs;
	struct sbutton *sb;
	long pos,len;
	struct style *prefstyle=rock->prefstyle;
	char *val;

	if(sethelp) {
	    sethelp=FALSE;
	    SetHelp(self, pd);
	}

	/* if this entry is from the defaults then
	    it cannot be deleted. */
	
	if(pd->indefs) {
	    blistt[1].label=NULL;
	} else blistt[1].label="Delete";
	
	sb=sbutton_CreateFilledSButton(prefs, blistt);
	if(sb==NULL) return TRUE;
	sbutton_SetLayout(sb, 1, 1, sbutton_GrowColumns);
	sbutton_AddRecipient(sb, atom_Intern(duplicateatomstr), self, duplicate, pd);
	sbutton_AddRecipient(sb, atom_Intern(deleteatomstr), self, delete, pd);
	pos=rock->npos;
	PushView(pt, pos, "sbuttonv", (struct dataobject *)sb);
	pos++;
	text_AlwaysInsertCharacters(pt, pos, "\n", 1);
	pos++;
	len=strlen(pd->name);
	text_AlwaysInsertCharacters(pt, pos, pd->name, len);
	text_AlwaysInsertCharacters(pt, pos+len, "\n\n", 2);
	if(prefstyle!=NULL) {
	   PushStyle(pt, pos, len,  prefstyle);
	}
	pos+=len+2;
	
	sb=sbutton_CreateFilledSButton(prefs, blistc);
	if(sb==NULL) return TRUE;

	sbutton_SetLayout(sb, 1, 1, sbutton_GrowRows);
	sbutton_AddRecipient(sb, atom_Intern(changeappatomstr), self, changeapp, pd);
	PushView(pt, pos, "sbuttonv", (struct dataobject *)sb);
	pos++;
	pos=AddVal(pt, pos, "Application", pd->app, TRUE);
	
	
	sb=sbutton_CreateFilledSButton(prefs, blistc2);
	if(sb==NULL) return TRUE;

	sbutton_SetLayout(sb, 1, 1, sbutton_GrowRows);
	sbutton_AddRecipient(sb, atom_Intern(changeatomstr), self, changecondition, pd);
	PushView(pt, pos, "sbuttonv", (struct dataobject *)sb);
	pos++;
	pos=AddVal(pt, pos, "Conditional", pd->cond?pd->cond:"Always", TRUE);


	if(pd->val) {
	    text_AlwaysInsertCharacters(pt, pos, editors, sizeof(editors)-1);

	    sb=sbutton_CreateFilledSButton(prefs, blistd);
	    if(sb==NULL) return TRUE;

	    sbutton_SetLayout(sb, 1, 1, sbutton_GrowRows);
	    sbutton_AddRecipient(sb, atom_Intern(resetatomstr), self, reset, pd);
	    PushView(pt, pos+sizeof(editors)-1, "sbuttonv", (struct dataobject *)sb);
	    PushStyle(pt, pos, sizeof(editors)-2, bold);
	    pos+=sizeof(editors) /* - 1  REMOVE -1 WHEN doing views */;
	    text_AlwaysInsertCharacters(pt, pos, " to ", 4);
	    val=pd->val?pd->val:"<None>";
	    len=strlen(val);
	    text_AlwaysInsertCharacters(pt, pos+4, val, len);

	    text_AlwaysInsertCharacters(pt, pos+4+len, ")", 1);
	    pos+=5+len;
	}
	if(pd->defviews) {
	    rock->lastpos=pos;
	    rock->cpd=pd;
	    list_Enumerate(pd->defviews, AddViews, rock);
	    pos=rock->lastpos;
	}
	text_AlwaysInsertCharacters(pt, pos, "\n", 1);
	pos++;
	
	if(pd->prevlines && list_Size(pd->prevlines)>0) {
	    text_AlwaysInsertCharacters(pt, pos, comments, sizeof(comments)-1);
	    text_AlwaysInsertCharacters(pt, pos+sizeof(comments)-1, "\n", 1);
	    PushStyle(pt, pos, sizeof(comments)-1, bold);
	    rock->lastpos=pos+sizeof(comments);
	    list_Enumerate(pd->prevlines, AddCommentLines, rock);
	    pos=rock->lastpos;
	}
	PushObject(pt, pos, "bp", NULL);
	pos++;
	text_AlwaysInsertCharacters(pt, pos, "\n", 1);
	pos++;
	PushMark(pt, pd, rock->npos, pos-rock->npos);
	rock->npos=pos;
    }
    return TRUE;
}

static boolean AddPreferences(pd, self)
struct prefdesc *pd;
struct pintv *self;
{
    char *name=pd->name;
    struct text *pt=TEXT(self->preferences);
    text_AlwaysInsertCharacters(pt, text_GetLength(pt), name, strlen(name));
    text_AlwaysInsertCharacters(pt, text_GetLength(pt), "\n", 1);
    return TRUE;
}

static boolean CheckPrefName(name1, name2)
struct prefdesc *name1, *name2;
{
    return !RFOLDEDEQ(name1->name, name2->name);
}

static boolean AddPreferencesToList(pd, self)
struct pintv *self;
struct prefdesc *pd;
{
    if(INCLUDE(self, pd, TRUE) && !pd->shadow) {
	if(list_Enumerate(self->prefslist, CheckPrefName, pd)) return TRUE;
	list_InsertEnd(self->prefslist, pd);
    }
    return TRUE;
}

static boolean RemoveMarks(pd, rock)
struct prefdesc *pd;
struct addrock *rock;
{
    struct pintv *self=rock->self;
    if(pd->mark) {
	text_RemoveMark(TEXT(self->cpref), pd->mark);
	mark_Destroy(pd->mark);
	pd->mark=NULL;
    }
    return TRUE;
}

static int mycmp(n1, n2)
struct prefdesc *n1, *n2;
{
    int result=ULstrcmp(n1->name, n2->name);
    return result;
}

static void UpdatePrefs(self)
struct pintv *self;
{

    self->pref_sel=NULL;
    text_AlwaysDeleteCharacters(TEXT(self->preferences), 0, text_GetLength(TEXT(self->preferences)));

    if(self->prefslist) list_Destroy(self->prefslist);

    self->prefslist=list_New();
    if(self->prefslist==NULL) return;
    list_Enumerate(PREFS(self)->prefs, AddPreferencesToList, self);
    list_Sort(self->prefslist, mycmp);
    list_Enumerate(self->prefslist, AddPreferences, self);
    text_NotifyObservers(TEXT(self->preferences), 0);
}

static void ClearCPref(self)
struct pintv *self;
{
    struct text *ct=TEXT(self->cpref);
  /*  struct environment *rt;
    rt = ct->rootEnvironment;
    ct->rootEnvironment = NULL;
    environment_FreeTree(rt);
    ct->rootEnvironment = environment_GetRootEnvironment();
*/
    text_AlwaysDeleteCharacters(ct, 0, text_GetLength(ct));

    /* horrible hack... please forgive me. -rr2b
     this convinces textview to remove most of it's marks
     from the text, thus speeding up the text changes
     which are about to be done.
     */
    textview_SetDataObject(self->cpref, ct);
}

static void UpdateCPref(self)
struct pintv *self;
{
    struct text *ct=TEXT(self->cpref);
    struct stylesheet *ss;
    struct style *prefstyle;
    struct addrock rock;
    struct environment *rt;
#ifdef TIMEY
    struct timeval a,b;

    gettimeofday(&b, NULL);
    printf("time before:%lf\n", (((double)b.tv_sec)*1000000.0+(double)b.tv_usec)/1000000);
#endif /* TIMEY */
    ClearCPref(self);
    
    ss=text_GetStyleSheet(ct);
    if(ss==NULL) return;
    
    rock.self=self;
    rock.bold = stylesheet_Find(ss, "bold");
    rock.prefstyle = stylesheet_Find(ss, "prefname");
    rock.prefs=self->prefs;
    rock.lastpos = rock.npos = 0;
    
    sethelp=TRUE;
    list_Enumerate(PREFS(self)->prefs, RemoveMarks, &rock);
    list_Enumerate(PREFS(self)->prefs, AddInstances, &rock);
    DoStyles(self);
    text_NotifyObservers(ct, observable_OBJECTCHANGED);
#ifdef TIMEY
    gettimeofday(&a, NULL);
    printf("time after:%lf\n", (((double)a.tv_sec)*1000000.0+(double)a.tv_usec)/1000000);
    printf("time:%lf\n", ((double)(((double)a.tv_sec)*1000000+(double)a.tv_usec)-(((double)b.tv_sec)*1000000+(double)b.tv_usec))/1000000);
#endif /* TIMEY */
}


static boolean LocatePref(pd, self)
struct prefdesc *pd;
struct pintv *self;
{
    if(INCLUDE(self, pd, FALSE)) return FALSE;
    else return TRUE;
}

static void MoveCPref(self)
struct pintv *self;
{
    struct prefdesc *pd=(struct prefdesc *)list_Enumerate(PREFS(self)->prefs, LocatePref, self);
    if(pd && pd->mark && mark_GetPos(pd->mark)>=0) {
	textview_SetTopPosition(self->cpref, mark_GetPos(pd->mark));
	textview_SetDotPosition(self->cpref, mark_GetPos(pd->mark));
	textview_SetDotLength(self->cpref, 0);
	SetHelp(self, pd);
    } else {
	char buf[1024];
	sprintf(buf, "Couldn't locate instance of preference %s.\n", self->pref);
	message_DisplayString(self, 0, buf);
    }
}

static boolean FindNewHelp(pd, self)
struct pintv *self;
struct prefdesc *pd;
{
    long pos=textview_GetDotPosition(self->cpref);
    if(pd->mark && pos >= mark_GetPos(pd->mark) && pos<=mark_GetEndPos(pd->mark)) {
	SetHelp(self, pd);
	return FALSE;
    }
    return TRUE;
}

static void ReProcess(self)
struct pintv *self;
{
    message_DisplayString(self, 0, "Processing changes from text.");
    prefs_ReScan(PREFS(self));
    PREFS(self)->selfmod=TRUE;
    prefs_NotifyObservers(PREFS(self), observable_OBJECTCHANGED);
    PREFS(self)->selfmod=FALSE;
    self->lockdown=FALSE;
}

struct view *pintv__Hit(self, action, x, y, numberOfClicks)
struct pintv *self;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    char buf[1024];
    struct view *result;
    if(self->lockdown) {
	ReProcess(self);
    }
    result=super_Hit(self, action, x, y, numberOfClicks);
    if(result==(struct view *)self->categories) {
	self->cat_sel=SelectLine(self->categories, self->cat_sel, TRUE);
	if(self->cat_sel==NULL) return (struct view *)self;
	text_CopySubString(TEXT(self->categories), environment_Eval(self->cat_sel), environment_GetLength(self->cat_sel), buf, FALSE);
	self->category=SAVESTR(buf);	
	UpdatePrefs(self);
	self->pref=NULL;
	if(self->autolist) UpdateCPref(self);
	else ClearCPref(self);
	return (struct view *)self;
    } else if(result==(struct view *)self->preferences) {
	self->pref_sel=SelectLine(self->preferences, self->pref_sel, TRUE);
	if(self->pref_sel==NULL) return (struct view *)self;
	text_CopySubString(TEXT(self->preferences), environment_Eval(self->pref_sel), environment_GetLength(self->pref_sel), buf, FALSE);
	self->pref=SAVESTR(buf);
	if(self->autolist) {
	    MoveCPref(self);
	} else UpdateCPref(self);
	return (struct view *)self;
    }
    return result;
}

static boolean VerifyPrefSanity(pd2, self)
struct prefdesc *pd2;
struct pintv *self;
{
    struct prefdesc *pd=self->cpd;
    if(RFOLDEDEQ(pd2->name, pd->name) && pd2->order>pd->order) {
	pd->order=pd2->order;
	self->newerrors=TRUE;
	if(self->errors==NULL) {
	    self->errors=text_New();
	}
	if(self->errors==NULL) {
	    fprintf(stderr, "prefs: Failed to create error buffer for sanity check output!\n");
	    fprintf(stderr, "prefs: sanity check failed on preference %s\n", pd->name);
	} else {
	    text_AddObserver(self->errors, self);
	    text_AlwaysInsertCharacters(self->errors, text_GetLength(self->errors), "\t", 1);
	    text_AlwaysInsertCharacters(self->errors, text_GetLength(self->errors), pd2->app, strlen(pd2->app));
	    text_AlwaysInsertCharacters(self->errors, text_GetLength(self->errors), "\n", 1);
	}
    }
   return TRUE;
}

static boolean CheckSanity(pd, self)
struct prefdesc *pd;
struct pintv *self;
{
    if(pd->order>=0 && (pd->app==NULL || pd->app[0]=='\0' || (pd->app[0]=='*' && pd->app[1]=='\0'))) {
	long pos=self->errors ? text_GetLength(self->errors) : 0;
	self->cpd=pd;
	self->newerrors=FALSE;
	list_Enumerate(PREFS(self)->prefs, VerifyPrefSanity, self);
	if(self->newerrors && self->errors) {
	    text_AlwaysInsertCharacters(self->errors, 0, "\n", 1);
	    text_AlwaysInsertCharacters(self->errors, 0, pd->name, strlen(pd->name));
	}
    }
    return TRUE;
}

static boolean AddCategories(pg, self)
struct pintv *self;
struct prefgroup *pg;
{
    struct text *cat=TEXT(self->categories);
    text_AlwaysInsertCharacters(cat, text_GetLength(cat), pg->name, strlen(pg->name));
    text_AlwaysInsertCharacters(cat, text_GetLength(cat), "\n", 1);
    return TRUE;
}

extern struct event *pintv_GetKeepEvent(), *pintv_GetReportEvent();

void pintv__SetDataObject(self, d)
struct pintv *self;
struct dataobject *d;
{
    struct prefs *prefs=(struct prefs *)d;
    struct prefdesc *pd;
    struct text *t=TEXT(self->categories);
    struct stylesheet *ss=text_GetStyleSheet(t);
    struct style *bold;
    struct prefgroup pg;

    pg.name=currlabp;
    pg.grouphelp=NULL;
    
    super_SetDataObject(self, d);
    
    if((!prefs_GetReadOnly(prefs)) && prefs->version==-1 && environ_GetProfileSwitch("KeepOrigPrefs", TRUE)) self->oevent=pintv_GetKeepEvent(self);
    if(ss==NULL) return;
    
    bold=stylesheet_Find(ss, "bold");
    if(bold==NULL) return;

    
    AddCategories(&pg, self);
    list_Enumerate(prefs->categories, AddCategories, self);
    pd=(struct prefdesc *)list_Enumerate(prefs->prefs, CheckSanity, self);
    if(self->errors) {
	self->reportevent=pintv_GetReportEvent(self);
    }
    if(pd) {
	fprintf(stderr, "Preferences Warning: sanity check violation!\n");
	fprintf(stderr, "Sanity check failed on %s.\n", pd->name);
    }
    PREFS(self)->sane=TRUE;
    if(prefs->help) {
	self->helpv=phelpv_New();
	if(self->helpv!=NULL) {
	    self->helpva = phelpv_GetApplicationLayer(self->helpv);
	    phelpv_SetDataObject(self->helpv, prefs->help);
	    lpair_SetNth(self->blr, 1, self->helpva);
	    phelpv_SetPrefs(self->helpv, PREFS(self));
	}
    }
    self->cat_sel=AddStyle(t, 0, strlen(currlabp), bold);
    UpdatePrefs(self);
    if(self->autolist) UpdateCPref(self);
    else ClearCPref(self);
}

void pintv__WantInputFocus(self, requestor)
struct pintv *self;
struct view *requestor;
{
    super_WantInputFocus(self, requestor);
}

void pintv__WantUpdate(self, requestor)
struct pintv *self;
struct view *requestor;
{
    if((requestor==(struct view *)self->cpref || requestor==(struct view *)self->cpref_al)) {
	list_Enumerate(PREFS(self)->prefs, FindNewHelp, self);
    }
    super_WantUpdate(self, requestor);
}
