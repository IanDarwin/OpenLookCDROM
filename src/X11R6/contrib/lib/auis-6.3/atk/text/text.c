/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *         Copyright Carnegie Mellon Univ. 1992,1993 - All Rights Reserved *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/text.c,v 2.86 1994/02/07 19:20:03 rr2b Exp $";
#endif


#include <andrewos.h>
#include <class.h>
#include <ctype.h>

#define CHECK_BE1

#include <attribs.h>
#ifdef CHECK_BE1
#include <be1be2.ih>
#endif /* CHECK_BE1 */
#include <dataobj.ih>
#include <dict.ih>
#include <environ.ih>	/* for datastream test only */
#include <envrment.ih>
#include <filetype.ih>
#include <style.ih>
#include <stylesht.ih>
#include <tabs.ih>
#include <txtstvec.h>
#include <viewref.ih>
#include <text.eh>

#define MAXENVSTACK 100
#define TEXT_VIEWREFCHAR '\377'

#define DEFAULTDATASTREAMNUMBER 12
#define text_UNSET -100
#define text_USEDEFAULT -200

#define MAX_QP_CHARS 76 /* The Quoted-Printable encoding REQUIRES that encoded lines be no more than 76 characters long */
#define LAST_QP_CHAR (MAX_QP_CHARS - 1)

static stylesIncludeBeginning = text_UNSET;
static stylesIncludeEnd = text_UNSET;

/* Place holder character for viewrefs */
/* All viewrefs contain this char, but the presence of this */
/* char does not necessarily denote a viewref */

struct environmentelement {
    struct environment *environment;
    long pos;
};

/* When breaking up a line, trailing spaces (except the first one) */
/* are now written on the following line. When the new reader is */
/* generally distributed, the following define can be commented out */
/* and this redundant blank can be avoided */

#define WRITETRAILINGBLANK 1

/* various routines have environment stacks allocated dynamically
	when they are called.  Some functions (WrapStyles, 
	text_HandleKeyword, ...) assume these stacks exist and
	access them via envptr and envBegin.
	Any function using these variables must save and restore them.
*/
static struct environmentelement *envBegin = NULL;
static struct environmentelement *envptr = NULL;

static long HighBitStart = -1;
static void ClearStyles();

static int DataStreamVersion = 0;

static void AddObj(self, obj)
struct text *self;
struct dataobject *obj;
{
    long i;

    if(self->nobjs>=self->objssize) {
	long oldsize=self->objssize;
	self->objssize+=10;
	if(self->objs) self->objs=(struct dataobject **)realloc(self->objs, sizeof(struct dataobject *)*self->objssize);
	else self->objs=(struct dataobject **)malloc(sizeof(struct dataobject *)*self->objssize);
	if(self->objs==NULL) {
	    self->nobjs=0;
	    self->objssize=0;
	    return;
	}
	while(oldsize<self->objssize) {
	    self->objs[oldsize]=NULL;
	    oldsize++;
	}
    }
    dataobject_AddObserver(obj, self);
    self->objs[self->nobjs]=obj;
    self->nobjs++;
}

static void DelObj(self, obj)
struct text *self;
struct dataobject *obj;
{
    long i;
    for(i=0;i<self->nobjs;i++) {
	if(self->objs[i]==obj) break;
    }
    if(i>=self->nobjs) return;
    self->nobjs--;
    while(i<self->nobjs) {
	self->objs[i]=self->objs[i+1];
	i++;
    }
}

static struct environment *text__AlwaysWrapViewChar(self, pos, viewtype, dataobject)
struct text *self;
long pos;
char *viewtype;
struct dataobject *dataobject;
{
    struct viewref *newviewref;
    struct environment *newenv;
    
    AddObj(self, dataobject);
    newviewref = viewref_Create(viewtype, dataobject);
    viewref_AddObserver(newviewref,self);
    newenv = environment_InsertView(self->rootEnvironment, pos, newviewref, TRUE);
    environment_SetLength(newenv, 1);
    environment_SetStyle(newenv, FALSE, FALSE);
    return newenv;
}

boolean text__InitializeObject(classID, self)
struct classheader *classID;
struct text *self;
{
    self->objs=NULL;
    self->nobjs=0;
    self->objssize=0;
    self->executingGetModified = FALSE;
    self->rootEnvironment = environment_GetRootEnvironment();
    self->styleSheet = stylesheet_New();
    self->currentViewreference = NULL;
    self->exportEnvs = TRUE;
    self->WriteAsText = FALSE;
    self->CopyAsText = FALSE;
    self->writeStyle = text_DefaultWrite;
    self->templateName = NULL;
#if 0
    if (DataStreamVersion == 0) {
        char *buf;
        if ((buf = environ_GetConfiguration("BE2TextFormat")) != NULL && *buf != '\0')
            DataStreamVersion = atoi(buf);
        if (DataStreamVersion < 10)
#endif /* 0 */
            DataStreamVersion = DEFAULTDATASTREAMNUMBER;
#if 0
    }
#endif /* 0 */
    if(stylesIncludeEnd == text_UNSET){
	stylesIncludeEnd = environ_GetProfileSwitch("stylesincludeend", TRUE);
	stylesIncludeBeginning = environ_GetProfileSwitch("stylesincludebeginning", FALSE);
	if(stylesIncludeEnd == TRUE && stylesIncludeBeginning == FALSE){
	    stylesIncludeEnd = stylesIncludeBeginning = text_USEDEFAULT;
	}
    }
    return(TRUE);
}

void text__FinalizeObject(classID, self)
struct classheader *classID;
struct text *self;
{
    ClearStyles(self);
    environment_Destroy(self->rootEnvironment);
    if (self->styleSheet)
        stylesheet_Destroy(self->styleSheet);
    if (self->templateName != NULL)
        free(self->templateName);
}


void text__SetBaseTemplateName(self, name)
struct text *self;
char *name;
{
    if(self->templateName != NULL) free(self->templateName);
    if(name==NULL) self->templateName=NULL;
    else {
	self->templateName=(char *)malloc(strlen(name)+1);
	if(self->templateName==NULL) return;
	strcpy(self->templateName, name);
    }
}

/* 
 * Perhaps there should be some other condition for reading this template.
 * Otherwise maybe it should just store the template name away somewhere and
 * read it "when the time is right."
 */

void text__SetAttributes(self, attributes)
struct text *self;
struct attributes *attributes;
{
    super_SetAttributes(self, attributes);

    while (attributes) {
        if (strcmp(attributes->key, "template") == 0) {
            if (self->templateName != NULL)
                free(self->templateName);
            self->templateName = malloc(strlen(attributes->value.string) + 1);
            strcpy(self->templateName, attributes->value.string);
            text_ReadTemplate(self, self->templateName, (text_GetLength(self) == 0)); 
	}
	else if (strcmp(attributes->key, "datastream") == 0) {
	    if (strcmp(attributes->value.string, "no") == 0) {
		text_SetWriteStyle(self, text_NoDataStream);
	    }
	    else if (strcmp(attributes->value.string, "yes") == 0) {
		text_SetWriteStyle(self, text_DataStream);
	    }
	}
    
	attributes = attributes->next;
    }
}

struct viewref *text__InsertObject(self, pos, name, viewname)
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
	dictionary_Insert(NULL, (char *) newobject->id, (char *) newobject);
        if (viewname == NULL || *viewname == '\0')
	    viewname = dataobject_ViewName(newobject);
	/* the viewref will prevent the dataobject from being destroyed until the viewref is destroyed. */
	dataobject_UnReference(newobject);
        if (env = text_AddView(self, pos, viewname, newobject))
            return env->data.viewref;
    }
    return NULL;
}


/* 
 * ListObjects is slightly sneaky.  If SIZE is 0, then the type of 
 * list should be (struct dataobject ***).  It is an output parameter 
 * that will be filled in with a pointer to a malloced array of 
 * pointers to children objects.  The number of children will be 
 * returned as usual.  In the unlikely event of a malloc or realloc 
 * failure, no array is returned, and the number of children reported 
 * is -1. 
 */
#if 0
static long text_ListObjects(self, list, size)
struct text *self;
struct dataobject **list;
long size;
{
    struct dataobject *ob,**ptr,**cptr;
    struct environment *rootenv;
    struct environment *startenv;
    struct environment *endenv;
    struct environment *curenv;
    struct environment *newenv;
    struct environment *parentenv;
    long end;
    long i;
    long elen;
    int levels;
    long count = 0;
    long len =  text_GetLength(self);
    long pos = 0;
    boolean growYourOwn = (size == 0); /* grow an array ourselves? */
    struct dataobject ***stash = (struct dataobject ***)list;

    startenv = environment_GetInnerMost(self->rootEnvironment, pos);
    endenv = environment_GetInnerMost(self->rootEnvironment, pos + len - 1);
    rootenv = environment_GetCommonParent(startenv, endenv);
    curenv = rootenv;

    if (growYourOwn) {
	size = 64;
	list = (struct dataobject **)malloc(size *
					    sizeof(struct dataobject **));
	if (list == NULL)
	    return -1;
	else
	    *stash = list;
    }

    while (curenv != startenv)  {
        curenv = environment_GetChild(curenv, pos);
    }
    i = 0;
    end = len;
    cptr = list; 
    while (i < end)  {
        newenv = environment_GetInnerMost(self->rootEnvironment, i);
        elen = environment_GetNextChange(self->rootEnvironment, i);
        if (elen + i > end)
            elen = end - i;
        if (curenv != newenv)  {
            parentenv = environment_GetCommonParent(curenv, newenv);
            levels = environment_Distance(curenv, parentenv);
            while (levels > 0)  {
                levels--;
            }
            curenv = parentenv;
            if (curenv != newenv)  {
                struct environment *stack[100];
                struct environment *te;
                int level = 0;
                for (te = newenv; te != parentenv; te = (struct environment *) te->header.nestedmark.parent)
                    stack[level++] = te;
                while (level > 0)  {
                    curenv = stack[--level];
                    if (curenv->type == environment_View)  
                        break;
                }
            }
        }
        if (curenv->type == environment_View)  {
            if((ob = curenv->data.viewref->dataObject) != NULL){
                for(ptr = list ; ptr < cptr; ptr++)
                    if (*ptr == ob) break;
                if(ptr == cptr) {
                    if(count == size)
			if (!growYourOwn)
			    return count;
			else {
			    size *= 2;
			    list = (struct dataobject **)
			      realloc(list,size * sizeof(struct dataobject **));
			    if (list == NULL)
				return -1;
			    else{
				*stash = list;
				cptr = list + count;
			    }
			}
		    *cptr++	= ob;
		    count++;
		}
            }
            i += curenv->header.nestedmark.length;
            elen = 0;
        }
        elen += i;
        i = elen;
    }
    if (count == 0 && growYourOwn)
	free(list);
    return count;
}
#else /* !0 */
static long text_ListObjects(self, list, size)
struct text *self;
struct dataobject **list;
long size;
{
    struct dataobject ***stash=(struct dataobject ***)list;
    boolean growYourOwn = (size == 0); /* grow an array ourselves? */
    
    if (growYourOwn) {
	if(self->nobjs==0) return 0;
	size = self->nobjs;
	list = (struct dataobject **)malloc(size *
					    sizeof(struct dataobject **));
	if (list == NULL)
	    return -1;
	else
	    *stash = list;
    } else if(size>self->nobjs) size=self->nobjs;

    while(--size>=0) {
	list[size]=self->objs[size];
    }
    return self->nobjs;
}
#endif /* !0 */

static void ClearStyles(self)
struct text *self;
{
    struct environment *rt;
    
    struct dataobject **dbuf, **d;
    long num;
    
    num = text_ListObjects(self, (struct dataobject **)&dbuf, 0);
    
    if (num > 0) {
	for(d = dbuf; num--; d++) {
	    dataobject_RemoveObserver(*d, self);
	}
	free(dbuf);
    }

    rt = self->rootEnvironment;
    self->rootEnvironment = NULL;
    environment_FreeTree(rt);
    stylesheet_FreeStyles(self->styleSheet);
    self->rootEnvironment = environment_GetRootEnvironment();
    self->nobjs=0;
}

void text__Clear(self)
struct text *self;
{
    ClearStyles(self);
    super_Clear(self);
}

long text__GetModified(self)
struct text *self;
{
    struct dataobject **dbuf, **d;
    long maxSoFar;
    long num;

    if (self->executingGetModified)
	return 0;

    maxSoFar = super_GetModified(self);
    num = text_ListObjects(self, (struct dataobject **)&dbuf, 0);

    if (num > 0) {
	self->executingGetModified = TRUE;
	for(d = dbuf; num--; d++) {
	    int x;
	    x = dataobject_GetModified(*d);
	    if (x > maxSoFar)
		maxSoFar = x;
	}
	free(dbuf);
	self->executingGetModified = FALSE;
    }

    return maxSoFar;
}

void text__ClearCompletely(self)
struct text *self;
{
    ClearStyles(self);
    super_Clear(self);
}

void text__LengthChanged(self, pos, len)
struct text *self;
long pos;
long len;
{
    super_LengthChanged(self, pos, len);
    environment_Update(self->rootEnvironment, pos, len);
}

boolean DoReplaceCharacters(self, pos, len, repStr, repLen, alwaysp)
struct text *self;
long pos, len;
char *repStr;
long repLen;
boolean alwaysp;
{
    struct environment *environment;

    if (alwaysp)
        super_AlwaysReplaceCharacters(self, pos, len, repStr, repLen);
    else
        if (! super_ReplaceCharacters(self, pos, len, repStr, repLen))
            return FALSE;

    environment = environment_GetInnerMost(self->rootEnvironment, pos);

    if ((environment_Eval(environment) == pos) && (environment_GetNextChange(environment, pos) == len))
        environment_SetLength(environment, environment->header.nestedmark.length + repLen - len);

    return TRUE;
}

boolean text__ReplaceCharacters(self, pos, len, repStr, repLen)
struct text *self;
long pos, len;
char *repStr;
long repLen;
{
    return DoReplaceCharacters(self, pos, len, repStr, repLen, FALSE);
}

void text__AlwaysReplaceCharacters(self, pos, len, repStr, repLen)
struct text *self;
long pos, len;
char *repStr;
long repLen;
{
    DoReplaceCharacters(self, pos, len, repStr, repLen, TRUE);
}

void text__AlwaysDeleteCharacters(self, pos, len)
struct text *self;
long pos;
long len;
{
    struct environment *te;
    struct environment *le;
    struct environment *re;
    struct environment *ne;
    long stylePos;
    long lPos;

    super_AlwaysDeleteCharacters(self, pos, len);

    te = self->rootEnvironment;
    stylePos = pos;
    while (stylePos > 0) {
	re = environment_GetChild(te, pos);
	if (re != NULL
	    && re->type == environment_Style) {
	    if ((lPos = environment_Eval(re)) == pos) {
		le = environment_GetChild(te, pos - 1);
		if (le != NULL
		    && le->type == environment_Style
		    && re->data.style == le->data.style
		    && ((lPos = environment_Eval(le)) + environment_GetLength(le)) == pos) {
		    /* merge together these two styles */

		    ne = environment_Add(te, lPos, pos - lPos + environment_GetLength(re));
		    if (ne != NULL) {
			ne->type = environment_Style;
			ne->data.style = le->data.style;
			environment_Delete(le);
			environment_Delete(re);
			te = ne;
			stylePos = pos - lPos;
			continue;
		    }
		}
	    }
	    else {
		stylePos = pos - lPos;
		te = re;
		continue;
	    }
	}
	return;
    }
}

static int ParseInteger(file,id)
FILE *file;
long *id;
{
    int c;
    while ((c = getc(file)) != EOF && c != ',' && c != '}')
        if (c >= '0' && c <= '9')
            *id = *id * 10 + c - '0';
    return c;
}

long text__HandleKeyWord(self, pos, keyword, file)
struct text *self;
long pos;
char *keyword;
FILE *file;
{
    struct environment *newenv;
    struct style *style;

    static char *EOFerror =
      "EOF encountered while reading in a view marker or template name - ignoring\n";

    if (strcmp(keyword, "textdsversion") == 0)  {
        long versionnumber = 0;
        int c;

        while ((c = getc(file)) != EOF && c != '}')
            if (c >= '0' && c <= '9')
                versionnumber = versionnumber * 10 + (c - '0');

        while (c != EOF && (c = getc(file)) != '\n')
            ;

        ((struct simpletext *) self)->Version = versionnumber;

        /* Handle outdated data stream versions here */

        return 0;
    }

    if (strcmp(keyword, "view") == 0) {
        /* Parse the view name, the dataobject name, and inset id. */

        struct dataobject *mydataobject;
        char viewname[200];
        long viewid;
        long objectid,desw,desh;
        int i;
        struct viewref *newviewref;
        int c;

        i = 0;
        while ((c = getc(file)) != EOF && c != ',')
            if (i < sizeof (viewname) - 1)
                viewname[i++] = c;
        viewname[i] = '\0';
        if (c == EOF) {
            fprintf(stderr, EOFerror);
            return -1;
        }

        objectid = 0;
        c = ParseInteger(file, &objectid);
        if (c == EOF || c == '}') {
            fprintf(stderr, EOFerror);
            return -1;
        }

        viewid = 0;
        c = ParseInteger(file, &viewid);
        if (c == EOF) {
            fprintf(stderr, EOFerror);
            return -1;
        }

        desw = desh = 0;

        if (c == ',') {
            /* New format with desired view size saved */

            if ((c = ParseInteger(file, &desw)) == EOF) {
                fprintf(stderr, EOFerror);
                return -1;
            }	

            if (c == ',' && ((c = ParseInteger(file, &desh)) == EOF)) {
                fprintf(stderr, EOFerror);
                return -1;
            }

            if (c == ',') {
                long junk;  /* Eat up any future args that may be added */

                while ((c = ParseInteger(file, &junk)) != EOF) {
                    if (c == '}')
                        break;
                    junk = 0;
                }

                if (c == EOF) {
                    fprintf(stderr, EOFerror);
                    return -1;
                }	
            }
        }
	if(text_GetObjectInsertionFlag(self) == FALSE){
	    char bb[512];
	    long ll;
	    sprintf(bb,"[A %s VIEW WAS HERE]",viewname);
	    ll = strlen(bb);
	    text_InsertCharacters(self, pos, bb, ll);
	    return ll;
	}

        mydataobject = (struct dataobject *) dictionary_LookUp(NULL, (char *) objectid);
        /* No dataobject for this view; it may have never existed or */
        /* maybe the dataobject could not be found. */
        if (mydataobject == NULL)
	    return 0;
	AddObj(self, mydataobject);
        newviewref = viewref_Create(viewname, mydataobject);
        viewref_AddObserver(newviewref, self);
        newviewref->desw = desw;
        newviewref->desh = desh;
        newenv = environment_InsertView(envptr->environment, pos - envptr->pos, newviewref, TRUE);
        text_AddInCharacter(self, pos, TEXT_VIEWREFCHAR);
        environment_SetLength(newenv, 1);
        environment_SetStyle(newenv, FALSE, FALSE);

        return 1;   /* Added one char for viewref... */
    }

    if (strcmp(keyword, "define") == 0) {
	stylesheet_Read(self->styleSheet, file, 0);
        return 0;
    }

    if (strcmp(keyword, "template") == 0) {
        char templatename[200];
        int i;
        int c;

        i = 0;
        while ((c = getc(file)) != EOF && c != '}')
            templatename[i++] = c;
        templatename[i] = '\0';
        if (c == EOF)  {
            fprintf(stderr, EOFerror);
            return -1;
        }

        if ((c = getc(file)) != EOF && c == '\n')
            ;

        text_ReadTemplate(self, templatename, FALSE);

        return 0;
    }
    if( *keyword == '^' && keyword[1] == '\0'){
	/* Note that the high order bit needs set for the following characters
	    This works because these are always the inner-most brackets */
	HighBitStart = pos;
	return(0);
    }
    else HighBitStart = -1; /* just to be safe */

    /* Assume Style keyword: */
    /* Insert an environment with a yet undetermined length, */
    /* and push it on the stack pending an end brace. */

    style = stylesheet_Find(self->styleSheet, keyword);
    if (style == NULL)  {
        style = style_New();
        style_SetName(style, keyword);
        stylesheet_Add(self->styleSheet, style); 
    }

    newenv = environment_InsertStyle(envptr->environment, pos - envptr->pos, style, TRUE);
    envptr++;
    envptr->environment = newenv;
    envptr->pos = pos;

    return 0;
}

long text__HandleCloseBrace(self, pos, file)
struct text *self;
long pos;
FILE *file;
{
    if(HighBitStart != -1){
	unsigned char *foo;
	long rl;
	foo =(unsigned char *) text_GetBuf(self,HighBitStart,pos - HighBitStart,&rl);
	while(rl-- > 0)
	    *foo++ |= '\200';
	HighBitStart = -1;
	return(0);
    }
	
    if (envptr != envBegin) {
        long len = pos - envptr->pos;
	if (len > 0){
	    environment_SetLength(envptr->environment, len);
	    if((stylesIncludeEnd != text_USEDEFAULT) && 
	       (envptr->environment->type == environment_Style))
		environment_SetStyle(envptr->environment, stylesIncludeBeginning,stylesIncludeEnd);

	}
        else
            environment_Delete(envptr->environment);
        envptr--;
        return 0;
    } else {
        /* Extra close-braces */
        text_AddInCharacter(self, pos, '}');
        return 1;
    }
}

struct environment *text__AlwaysAddStyle(self, pos, len, style)
struct text *self;
long pos, len;
struct style *style;
{
    register struct environment *newenv;

    if ((newenv = environment_WrapStyle(self->rootEnvironment, pos, len, style)) != NULL) {
	if(stylesIncludeEnd != text_USEDEFAULT)
	    environment_SetStyle(newenv, stylesIncludeBeginning,stylesIncludeEnd);
        text_RegionModified(self, pos, len);
        text_SetModified(self);
    }

    return newenv;
}

struct environment *text__AddStyle(self, pos, len, style)
struct text *self;
long pos;
long len;
struct style *style;
{
    if (text_GetReadOnly(self) || pos < text_GetFence(self))
        return NULL;
    else
        return text_AlwaysAddStyle(self, pos, len, style);
}

struct environment *text__AlwaysAddView(self, pos, viewtype, dataobject)
struct text *self;
long pos;
char *viewtype;
struct dataobject *dataobject;
{
    struct viewref *newviewref;
    char c = TEXT_VIEWREFCHAR;
    register struct environment *newenv;

    AddObj(self, dataobject);
    newviewref = viewref_Create(viewtype, dataobject);
    viewref_AddObserver(newviewref,self);
    text_AlwaysInsertCharacters(self, pos, &c, 1);
    newenv = environment_WrapView(self->rootEnvironment, pos, 1, newviewref);
    environment_SetStyle(newenv, FALSE, FALSE);
    text_RegionModified(self, pos, 1);
    text_SetModified(self);
    return newenv;
}

struct environment *text__AddView(self, pos, viewtype, dataobject)
struct text *self;
long pos;
char *viewtype;
struct dataobject *dataobject;
{
    if (text_GetReadOnly(self) || pos < text_GetFence(self))
        return NULL;
    else
        return text_AlwaysAddView(self, pos, viewtype, dataobject);
}

/*
 * Assuming a \begindata has been read, discards up to and including
 * a matching \enddata (discards internal levels of \begindata ... \enddata).
 * Returns FALSE if EOF is reached before the \enddata.
 * Something better needs to be done about this.
 */

static boolean DiscardToEnddata(file)
FILE *file;
{
    int c, i;
    char buf[20];
trymore:
    do {
        if ((c = getc(file)) == EOF)
            return FALSE;
    } while (c != '\\');
haveback:
    i = 0;
    while (1) {     /* Read possible keyword */
        if ((c = getc(file)) == EOF)
            return FALSE;
        if (i == 0 && (c == '\\' || c == '}' || c == '}'))
            goto trymore;   /* Just a quoted char */
        if (c == '{')       /* End of keyword */
            break;
        if (i < sizeof (buf) - 1)
            buf[i++] = c;
    }
    buf[i] = '\0';
    do {
        if ((c = getc(file)) == EOF)
            return FALSE;
    } while (c != '}');
    /* If it's a begindata, recurse to discard subobject */
    if (strcmp(buf, "begindata") == 0) {
        if (DiscardToEnddata(file) == FALSE)
            return FALSE;
        goto trymore;
    }
    if (strcmp(buf, "enddata") != 0)
        goto trymore;
    return TRUE;
}


long text__AlwaysInsertFile(self, file, filename, position)
struct text *self;
FILE *file;
char *filename;
long position;
{
    char *objectName;
    long objectID;
    int myfile = 0;
    int length = 0;
    if (file == NULL) {
        if (filename != NULL && ((file = fopen(filename,"r")) != NULL))
            myfile++;
        else
            return 0;
    }
    objectName = filetype_Lookup(file, filename, &objectID, NULL);
    if (objectName != NULL && !class_IsTypeByName(objectName, "text"))  {
        struct dataobject *dat;
	if(text_GetObjectInsertionFlag(self) == FALSE){
	    /* ignore non-text object */
	    char bb[512];
	    long ll;
	    fprintf(stderr,
		    "Insertion of objects not allowed, ignoring %s!\n",objectName);
	    DiscardToEnddata(file);
	    sprintf(bb,"[A %s OBJECT WAS INSERTED HERE]",objectName);
	    ll = strlen(bb);
	    text_AlwaysInsertCharacters(self, position, bb, ll);
	    length = ll;
	}
	else {
	    dat = (struct dataobject *) class_NewObject(objectName);
	    if (dat == NULL) {
		fprintf(stderr,
			"Text: Can't find routines for object '%s'; ignoring!\n", objectName);
		DiscardToEnddata(file);
		length = 0;
	    } else {
		dataobject_Read(dat, file, objectID);
		dictionary_Insert(NULL, (char *) objectID, (char *) dataobject_UniqueID(dat));
		/* the viewref will prevent the dataobject from being destroyed until the viewref is destroyed. */
		dataobject_UnReference(dat);
		text_AlwaysAddView(self, position, dataobject_ViewName(dat), dat);
		length = 1;
	    }
	}
    } else {
        boolean wasReadOnly;
	long oldfence = text_GetFence(self);
	wasReadOnly = text_GetReadOnly(self);
        /* ReadSubString checks read-only, making this ugliness necessary. */
	if(wasReadOnly){
	    text_SetReadOnly(self, FALSE);
	    length = text_ReadSubString(self, position, file, objectID > 0);
	    text_SetReadOnly(self, wasReadOnly);
	}
	else if( position < oldfence){
	    /* reset the fence properly */
	    text_SetFence(self,0);
	    length = text_ReadSubString(self, position, file, objectID > 0);
	    text_SetFence(self,oldfence + length);
	}
	else 
	    length = text_ReadSubString(self, position, file, objectID > 0);
    }

    if (myfile)
        fclose(file);
    return length;
}


long text__InsertFile(self, file, filename, position)
struct text *self;
FILE *file;
char *filename;
long position;
{
    if (text_GetReadOnly(self) || position < text_GetFence(self))
        return 0;
    else
        return text_AlwaysInsertFile(self, file, filename, position);
}

#ifdef CHECK_BE1

static boolean HasBinaryChars(self)
struct text *self;  /* (Other than viewrefs) */
{
    long pos = 0;
    while (pos < text_GetLength(self)) {
        long gotlen;
        unsigned char *p =
          (unsigned char *) text_GetBuf(self, pos, 1024, &gotlen);
        while (gotlen--) {
            if (*p == (unsigned char) TEXT_VIEWREFCHAR) {
                struct environment *env =
                  environment_GetInnerMost(self->rootEnvironment, pos);
                if (env == NULL || env->type != environment_View)
                    return TRUE;
            } else if (*p & 0x80)
                return TRUE;
            pos++;
            p++;
        }
    }
    return FALSE;
}

static void TryConversion(self)
struct text *self;
{
/*    fprintf(stderr, "File contains nonascii characters\n"); */
    if (class_Load("be1be2") == NULL) {
        fprintf(stderr, "Be1be2 not found; skipping BE1 check\n");
        return;
    }
    if (be1be2_CheckBE1(self) == FALSE)
        return;
    fprintf(stderr, "Converting BE1 file\n");
    be1be2_Convert(self);
}

#endif /* CHECK_BE1 */

long text__Read(self, file, id)
struct text *self;
FILE *file;
long id;
{
    int retval;
    ClearStyles(self);
    if (self->templateName != NULL)
        text_ReadTemplate(self, self->templateName, FALSE); 
    retval = super_Read(self, file, id);

#ifdef CHECK_BE1
    if (retval == dataobject_NOREADERROR)
        if (HasBinaryChars(self))
            TryConversion(self);
#endif /* CHECK_BE1 */

    if(self->styleSheet) {
	struct style *global=stylesheet_Find(self->styleSheet, "global");
	if(global) text_SetGlobalStyle(self, global);
    }
    return retval;
}

static int StringMatch(self, pos, c)
register struct text *self;
register long pos;
register char *c;
{
    /* Tests if the text begins with the given string */
    while (*c != '\0') {
        if (text_GetChar(self, pos) != *c)
            return FALSE;
        pos++; c++;
    }
    return TRUE;
}

static boolean TestForNoTemplate(style)
struct style *style;
{
    return ! style->template;
}

long text__Write(self, file, writeID, level)
struct text *self;
FILE *file;
long writeID;
int level;
{
    boolean quoteCharacters = FALSE;

    /* Determine when to use datastream format (quoteCharacters TRUE) */

    if (level != 0) /* Text object is a child */
        quoteCharacters = TRUE;

    if (self->exportEnvs) {
        if (environment_NumberOfChildren(self->rootEnvironment) > 0)
	    quoteCharacters = TRUE; /* There's at least one style */
	else if (stylesheet_EnumerateStyles(self->styleSheet, (procedure) TestForNoTemplate, 0) != NULL) {
	    quoteCharacters = TRUE;
	}
    }

    if (StringMatch(self, 0, "\\begindata"))
        quoteCharacters = TRUE;

    switch (text_GetWriteStyle(self)) {
	case text_NoDataStream:
	    quoteCharacters = FALSE;
	    break;
	case text_DataStream:
	    quoteCharacters = TRUE;
	    break;
    }

    if (self->header.dataobject.writeID != writeID)  {
        if (quoteCharacters) {
            self->header.dataobject.writeID = writeID;
	    fprintf(file, "\\begindata{%s,%ld}\n", 		
		    (self->WriteAsText)?"text": class_GetTypeName(self),
		    dataobject_UniqueID(&self->header.dataobject));
            fprintf(file, "\\textdsversion{%d}\n", DataStreamVersion);
            if (self->styleSheet->templateName)
                fprintf(file, "\\template{%s}\n", self->styleSheet->templateName);
            stylesheet_Write(self->styleSheet, file);
            text_WriteSubString(self, 0, text_GetLength(self), file, quoteCharacters);
	    fprintf(file, "\\enddata{%s,%d}\n",
		    (self->WriteAsText)?"text": class_GetTypeName(self),
		    self->header.dataobject.id);
            fflush(file);
        }
        else
            super_Write(self, file, writeID, level);
    }
    return self->header.dataobject.id;
}

long text__ReadSubString(self, pos, file, quoteCharacters)
struct text *self;
long pos;
FILE *file;
boolean quoteCharacters;
{
    struct environmentelement environmentStack[MAXENVSTACK];
    struct environmentelement *lastEnvBegin = envBegin;
    struct environmentelement *lastEnvptr = envptr;
    struct environment *rootenv;
    long len;

    envptr = environmentStack;
    envBegin = environmentStack;

    rootenv = environment_GetEnclosing(self->rootEnvironment, pos);
    envptr->environment = rootenv;
    envptr->pos = environment_Eval(rootenv);
    HighBitStart = -1 ; 

    len = super_ReadSubString(self, pos, file, quoteCharacters);
    if (envptr != environmentStack)  {
        fprintf(stderr, "All environments not closed. - Closing them by default\n");

        while (envptr != environmentStack)  {
            environment_SetLength(envptr->environment, pos + len - envptr->pos);
            envptr--;
        }
    }

    envBegin = lastEnvBegin;
    envptr = lastEnvptr;

    return len;
}

static void PutsRange(p, fp, ep)
char *p;
FILE *fp;
char *ep;
{
    while (p < ep)
        putc(*p++, fp);
}

static char *WriteOutBuf(file,outbuf,outp,lastblank)
FILE *file;
char *outbuf,*outp,*lastblank;
{
    char blankchar,*temp;
    if(lastblank == NULL || lastblank == outbuf) {
        lastblank = outp;
        blankchar = '\\';
    }
    else blankchar = *lastblank;
    PutsRange(outbuf, file, lastblank);
    lastblank++;
#ifdef WRITETRAILINGBLANK
    putc(blankchar,file);
#else /* WRITETRAILINGBLANK */
    if(blankchar != ' ')  putc(blankchar,file);
#endif /* WRITETRAILINGBLANK */
    putc('\n',file);
    for(temp = outbuf; lastblank < outp; lastblank++)
        *temp++ = *lastblank;
    return temp;
}

/*
 * This is now always writing version 12.  Version 11 code has been
 * removed to save space.  It could be retrieved if necessary.
 */

static void text__WriteSubString(self, pos, len, file, quoteCharacters)
struct text *self;
long pos;
long len;
FILE *file;
{
    struct environment *rootenv;
    struct environment *startenv;
    struct environment *endenv;
    struct environment *curenv;
    struct environment *newenv;
    struct environment *parentenv;
    register int lastblankset = FALSE;
    long end;
    long i;
    long elen;
    int levels;
    char c;
    long envpos;
    int  realcount;
    char outbuf[120],*outp,*endp,*temp,*lastblank;
    char *buf = NULL;
    long bufLen;
    boolean writinghighbit;
    int prevblankset=FALSE;
    
    endp = outbuf + LAST_QP_CHAR;
    outp = outbuf;lastblank = NULL; 

    if (len <= 0 ) return;

    if(!quoteCharacters){
        super_WriteSubString(self,pos,len,file,FALSE);
        return;
    }

    startenv = environment_GetInnerMost(self->rootEnvironment, pos);
    endenv = environment_GetInnerMost(self->rootEnvironment, pos + len - 1);
    rootenv = environment_GetCommonParent(startenv, endenv);

    for (envpos = environment_Eval(rootenv); pos == envpos && pos + len == envpos + rootenv->header.nestedmark.length; rootenv = (struct environment *) rootenv->header.nestedmark.parent, envpos = environment_Eval(rootenv));

    curenv = rootenv;
    realcount = 1;
    while (curenv != startenv)  {
	curenv = environment_GetChild(curenv, pos);
	if ((curenv->type == environment_Style && 
	     (outp + 2 + strlen(curenv->data.style->name) > endp))
	    || (curenv->type == environment_View && outp != outbuf)){
	    outp = WriteOutBuf(file,outbuf,outp,lastblank);
	}
	if (curenv->type == environment_Style){
	    *outp++ = '\\';
	    for(temp = curenv->data.style->name; *temp; temp++){
		*outp++ = *temp;
	    }
	    *outp++ = '{';
	}
    }

    i = pos;

    end = pos + len;
    while (i < end)  {
        newenv = environment_GetInnerMost(self->rootEnvironment, i);
        elen = environment_GetNextChange(self->rootEnvironment, i);
        if (elen + i > end)
            elen = end - i;
        if (curenv != newenv)  {
            parentenv = environment_GetCommonParent(curenv, newenv);
            levels = environment_Distance(curenv, parentenv);
            while (levels > 0)  {
                *outp++ = '}';
                levels--;
            }  
            curenv = parentenv;
            if (curenv != newenv)  {
                struct environment *stack[100];
                struct environment *te;
                int level = 0;

                for (te = newenv; te != parentenv; te = (struct environment *) te->header.nestedmark.parent)
                    stack[level++] = te;
                while (level > 0)  {
                    curenv = stack[--level];
                    if ((curenv->type == environment_Style && 
                          (outp + 2 + strlen(curenv->data.style->name) > endp))
                         || (curenv->type == environment_View && outp != outbuf)){
                        outp = WriteOutBuf(file,outbuf,outp,lastblank);
                        lastblank = NULL;
                        lastblankset = FALSE;
                    }
                    if (curenv->type == environment_Style){
                        *outp++ = '\\';
                        for(temp = curenv->data.style->name; *temp; temp++){
			    *outp++ = *temp;
			    if(outp>=endp) {
				PutsRange(outbuf, file, outp);
				outp=outbuf;
			    }
                        }
                        *outp++ = '{';
                    }
                    else if (curenv->type == environment_View)  
                        break;
                }
            }
        }
        if (curenv->type == environment_View)  {
            if(outp != outbuf){
                /* flush out remaining cached characters */
                outp = WriteOutBuf(file,outbuf,outp,lastblank);
            }
            /*  code to write out view information */
            dataobject_Write(curenv->data.viewref->dataObject ,file,self->header.dataobject.writeID,2);
            sprintf(outbuf,"\\view{%s,%ld,%ld,%ld,%ld", curenv->data.viewref->viewType, dataobject_UniqueID( curenv->data.viewref->dataObject), curenv->data.viewref->viewID,curenv->data.viewref->desw, curenv->data.viewref->desh);
            while(*outp) outp++;
            i += curenv->header.nestedmark.length;
            elen = 0;
            /*	    realcount = outp - outbuf; */
        }
        elen += i;

        bufLen = 0;
	writinghighbit = FALSE;
        while (i < elen)  {
            /* 	    Code for writing out actual text
                */
            if (bufLen == 0)
                buf = text_GetBuf(self, i, 1024, &bufLen);
            bufLen--, c = *buf++;

	    if(((int)c & 128 )!= 0){
		if(!writinghighbit){
		    *outp++ = '\\';
		    *outp++ = '^';
		    *outp++ = '{';
		    writinghighbit = TRUE;
		}
		c = (char) (((int)c) & 127);
	    }
	    else{
		if(writinghighbit){
		    *outp++ = '}';
		    writinghighbit = FALSE;
		}
	    }

            if (c == '\\' || c == '{' || c == '}')
                *outp++ = '\\';

	    *outp = c;
	    prevblankset=lastblankset;
            if(c == ' ' || c == '\t'){
                if(lastblankset == FALSE){
                    lastblank = outp;
                    lastblankset = TRUE;
                }
            }
            else 
                lastblankset = FALSE;
            outp++;
            if(c == '\n'){
		PutsRange(outbuf,file,outp-1);
		if(prevblankset) {
		    putc('\\', file);
		    putc('\n', file);
		}
		putc('\n', file);
                if(realcount ) putc('\n',file);
                outp = outbuf;
                lastblank = NULL;
                realcount = 0;
            }
            else if(outp > endp){
                char *cp;
                outp = WriteOutBuf(file,outbuf,outp,lastblank);
                lastblank = NULL;
                lastblankset = FALSE;
                for(cp = outbuf; cp < outp; cp++){
                    if(*cp == ' '){
                        if (cp == outbuf || *(cp - 1) != ' '){
                            lastblank = cp;
                            lastblankset = TRUE;
                        }
                    }
                    else lastblankset = FALSE;
                }
		if (outbuf == outp) realcount = 0;
            }
            else realcount++;
            i++;
        }
	if(writinghighbit){
	    *outp++ = '}';
	    writinghighbit = FALSE;
	}

    }

    /* flush out cached characters */
    if(outp != outbuf){
        *outp++ = '\\';
        *outp++ = '\n';
        PutsRange(outbuf,file,outp);
    }
    levels = environment_Distance(curenv, rootenv);
    while (levels-- > 0)
        putc('}', file);
}

static WrapStyle(self,curenv,pos)
struct text *self;
struct environment *curenv;
long pos;
{
    struct environment *newenv;
    if (curenv->type == environment_Style){
        /* New Style */
        struct style *style;
        char *keyword = curenv->data.style->name;
        style = stylesheet_Find(self->styleSheet, keyword);
        if (style == NULL)  {
            style = style_New();
            /* style_SetName(style, keyword); 		    -wjh */
            style_Copy(curenv->data.style, style);		/* -wjh */
            style->template = FALSE;			/* -wjh */
            stylesheet_Add(self->styleSheet, style); 
        }
	newenv = environment_InsertStyle(envptr->environment, pos - envptr->pos, style, TRUE);
	envptr++;
	envptr->environment = newenv;
	envptr->pos = pos;
    }
    else {
        struct viewref *newviewref;
        struct environment *newenv;
        newviewref = viewref_Create(curenv->data.viewref->viewType, curenv->data.viewref->dataObject);
        newviewref->desw = curenv->data.viewref->desw;
        newviewref->desh = curenv->data.viewref->desh;
        viewref_AddObserver(newviewref,self);
	newenv = environment_InsertView(envptr->environment, pos - envptr->pos, newviewref, TRUE);
	envptr++;
	envptr->environment = newenv;
	envptr->pos = pos;
    }
}

/* recurse up the tree and WrapStyles while unwinding.  This way, the
	outermost style is deepest in the stack
*/
	static void 
CopySurroundingStyles(self, pos, curenv)
	struct text *self;
	long pos;
	struct environment *curenv;
{
	struct environment *parent 
		= (struct environment *)curenv->header.nestedmark.parent;
	if (parent == NULL) return;
	CopySurroundingStyles(self, pos, parent);
	WrapStyle(self, curenv, pos);
}

	
boolean text__CopyTextExactly(self,pos,srctext,srcpos,len)
    struct text *self;
    long pos;
    struct text *srctext;
    long srcpos;
    long len;
{
    if (pos >= text_GetFence(self)) {
	text_AlwaysCopyTextExactly(self,pos,srctext,srcpos,len);
	return TRUE;
    }
    else
        return FALSE;
}

void text__AlwaysCopyTextExactly(self,pos,srctext,srcpos,len)
struct text *self;
long pos;
struct text *srctext;
long srcpos;
long len;
{
    struct environment *startenv;
    struct environment *curenv;
    struct environment *newenv;
    struct environment *dstrootenv;
    struct environment *parentenv;
    long end,ll,dend;
    long i,j;
    long elen;
    static struct classinfo *SimpleText = NULL;		/* -wjh */
    struct environmentelement environmentStack[MAXENVSTACK];
    struct environmentelement *lastEnvBegin = envBegin;
    struct environmentelement *lastEnvptr = envptr;

    if (SimpleText == NULL) 			/* -wjh */
	SimpleText = class_Load("simpletext");	/* -wjh */

    if (len <= 0 || srcpos < 0)
        return;
    if(text_GetLength(srctext) < srcpos + len)
        len = text_GetLength(srctext) - srcpos;

    if(self == srctext && srcpos + len > pos){
        struct text *newtext = text_New();
        /* there is probably a better way to do this, but for now
            this avoids a lot of problems */
        text_AlwaysCopyTextExactly(newtext,0,srctext,srcpos,len);
        text_AlwaysCopyTextExactly(self,pos,newtext,0,len);
        text_Destroy(newtext);
        return;
    }
    super_AlwaysCopyText(self,pos,srctext,srcpos,len);

    if (class_GetType(srctext) == SimpleText)		/* -wjh */
	return;					/* -wjh */


    /* loop through the environment tree of the source around the selected 
	text and copy styles and viewrefs to destination */

    /* the environmentStack is a stack of open environments in 
	the destination
    */
    envptr = environmentStack;
    envBegin = environmentStack;

    /*  The first entry in the stack is for the environment surrounding
	the destination point.
    */
    dstrootenv = environment_GetEnclosing(self->rootEnvironment, pos);
    envptr->environment = dstrootenv;
    envptr->pos = environment_Eval(dstrootenv);

    startenv = environment_GetInnerMost(srctext->rootEnvironment, srcpos);
    
    /* put onto the environmentStack (via WrapStyles) a style for
	each style surrounding the beginning of the selected source text 
	(Older versions of this code did not go all the way out to the
	root, but went out only as far as the smallest style that surrounded
	the source text and stuck out at one end or the other.  This 
	meant that styles were not copied exactly and that if the text
	did not cross a style boundary, styles might not be copied at all.)
    */
    CopySurroundingStyles(self, pos, startenv);

    /* The guts of copying styles is here.  At each style change point:
	to close a style: an element is popped off environmentStack
	to open a style: an element is pushed on the environmentStack
    */
    curenv = startenv;
    i = srcpos;j = pos;
    end = srcpos + len;
    dend = pos + len;
    while (i < end)  {
        newenv = environment_GetInnerMost(srctext->rootEnvironment, i);
        elen = environment_GetNextChange(srctext->rootEnvironment, i);
        if (elen + i > end)
            elen = end - i;
        if (curenv != newenv)  {
            parentenv = environment_GetCommonParent(curenv, newenv);
	    while (curenv != parentenv)  {
		long envlen = j - envptr->pos;
		if (len > 0)  {
		    environment_SetLength(envptr->environment, envlen);
		    if((stylesIncludeEnd != text_USEDEFAULT) && 
		       (envptr->environment->type == environment_Style))
			environment_SetStyle(envptr->environment, stylesIncludeBeginning,stylesIncludeEnd);
		}
		else  {
		    environment_Delete(envptr->environment);
		}
		envptr--;
		curenv = (struct environment *) environment_GetParent(curenv);
	    }

            if (curenv != newenv)  {
                struct environment *stack[100];
                struct environment *te;
                int level = 0;

                for (te = newenv; te != parentenv; te = (struct environment *) te->header.nestedmark.parent)
                    stack[level++] = te;
                while (level > 0)  {
                    curenv = stack[--level];
                    ll = environment_GetLength(curenv);
                    if( j + ll > dend ) ll = dend - j;
                    WrapStyle(self,curenv,j);
                }
            }
        }
        i += elen;
        j += elen;
    }

    /* Set length on the remaining environments in the stack */

    if (envptr != environmentStack)  {
        while (envptr != environmentStack)  {
            environment_SetLength(envptr->environment, pos + len - envptr->pos);
            envptr--;
        }
    }
    envBegin = lastEnvBegin;
    envptr = lastEnvptr;
}

void text__AlwaysCopyText(self,pos,srctext,srcpos,len)
struct text *self;
long pos;
struct text *srctext;
long srcpos;
long len;
{
    struct environment *rootenv;
    struct environment *startenv;
    struct environment *endenv;
    struct environment *curenv;
    struct environment *newenv;
    struct environment *dstrootenv;
    struct environment *parentenv;
    long end,ll,dend;
    long i,j;
    long elen;
    long envpos;
    static struct classinfo *SimpleText = NULL;		/* -wjh */
    struct environmentelement environmentStack[MAXENVSTACK];
    struct environmentelement *lastEnvBegin = envBegin;
    struct environmentelement *lastEnvptr = envptr;

    if (SimpleText == NULL) 			/* -wjh */
	SimpleText = class_Load("simpletext");	/* -wjh */

    if (len <= 0 || srcpos < 0 ||
         srcpos + len > text_GetLength(srctext))
        return;
    if(self == srctext && srcpos + len > pos){
        struct text *newtext = text_New();
        /* there is probably a better way to do this, but for now
            this avoids a lot of problems */
        text_AlwaysCopyText(newtext,0,srctext,srcpos,len);
        text_AlwaysCopyText(self,pos,newtext,0,len);
        text_Destroy(newtext);
        return;
    }
    super_AlwaysCopyText(self,pos,srctext,srcpos,len);

    if (class_GetType(srctext) == SimpleText)		/* -wjh */
	return;					/* -wjh */

    envptr = environmentStack;
    envBegin = environmentStack;

    dstrootenv = environment_GetEnclosing(self->rootEnvironment, pos);
    envptr->environment = dstrootenv;
    envptr->pos = environment_Eval(dstrootenv);

    /* loop through the environment tree and copy styles and viewrefs */
    if(text_GetLength(srctext) < srcpos + len)
        len = text_GetLength(srctext) - srcpos;
    startenv = environment_GetInnerMost(srctext->rootEnvironment, srcpos);
    endenv = environment_GetInnerMost(srctext->rootEnvironment, srcpos + len - 1);
    rootenv = environment_GetCommonParent(startenv, endenv);

    for (envpos = environment_Eval(rootenv); srcpos == envpos && srcpos + len == envpos + rootenv->header.nestedmark.length; rootenv = (struct environment *) rootenv->header.nestedmark.parent, envpos = environment_Eval(rootenv));

    curenv = rootenv;
    while (curenv != startenv)  {
        curenv = environment_GetChild(curenv, srcpos);
        WrapStyle(self,curenv,pos);
    }
    i = srcpos;j = pos;
    end = srcpos + len;
    dend = pos + len;
    while (i < end)  {
        newenv = environment_GetInnerMost(srctext->rootEnvironment, i);
        elen = environment_GetNextChange(srctext->rootEnvironment, i);
        if (elen + i > end)
            elen = end - i;
        if (curenv != newenv)  {
            parentenv = environment_GetCommonParent(curenv, newenv);
	    while (curenv != parentenv)  {
		long envlen = j - envptr->pos;
		if (len > 0)  {
		    environment_SetLength(envptr->environment, envlen);
		    if((stylesIncludeEnd != text_USEDEFAULT) && 
		       (envptr->environment->type == environment_Style))
			environment_SetStyle(envptr->environment, stylesIncludeBeginning,stylesIncludeEnd);
		}
		else  {
		    environment_Delete(envptr->environment);
		}
		envptr--;
		curenv = (struct environment *) environment_GetParent(curenv);
	    }

            if (curenv != newenv)  {
                struct environment *stack[100];
                struct environment *te;
                int level = 0;

                for (te = newenv; te != parentenv; te = (struct environment *) te->header.nestedmark.parent)
                    stack[level++] = te;
                while (level > 0)  {
                    curenv = stack[--level];
                    ll = environment_GetLength(curenv);
                    if( j + ll > dend ) ll = dend - j;
                    WrapStyle(self,curenv,j);
                }
            }
        }
        i += elen;
        j += elen;
    }

    /* Set length on the remaining environments in the stack */

    if (envptr != environmentStack)  {
        while (envptr != environmentStack)  {
            environment_SetLength(envptr->environment, pos + len - envptr->pos);
            envptr--;
        }
    }
    envBegin = lastEnvBegin;
    envptr = lastEnvptr;
}

void text__SetEnvironmentStyle(self, envptr, styleptr)
struct text *self;
struct environment *envptr;
struct style *styleptr;
{
    if (envptr->type != environment_View) {
        envptr->data.style = styleptr;
        envptr->type = environment_Style;
        text_RegionModified(self, environment_Eval(envptr), envptr->header.nestedmark.length);
    }
    else fprintf(stderr, "Can't set environment style; wrong environment type.\n");
}

void text__SetGlobalStyle(self, styleptr)
struct text *self;
struct style *styleptr;
{
    if (self->rootEnvironment->type != environment_View) {
        self->rootEnvironment->data.style = styleptr;
        self->rootEnvironment->type = environment_Style;
        text_RegionModified(self, environment_Eval(self->rootEnvironment), self->rootEnvironment->header.nestedmark.length);
    }
    else fprintf(stderr, "Can't set global style; wrong environment type.\n");
}

struct style *text__GetGlobalStyle(self)
struct text *self;
{
    return self->rootEnvironment->data.style;
}

/* New definition of environment state vector -- controls the formatting of text */

void text__InitStateVector(classID, sv)
struct classheader *classID;
struct text_statevector *sv;
{
    sv->CurLeftMargin = sv->CurRightMargin = sv->CurRightEdge =
      sv->CurLeftEdge = sv->CurTopMargin = sv->CurBottomMargin =
      sv->CurFontAttributes = sv->CurScriptMovement = 
      sv->CurIndentation = sv->CurSpacing = sv->CurSpread =
      sv->SpecialFlags = 0;
    sv->CurFontSize = 12;
    sv->CurJustification = style_LeftAndRightJustified;
    sv->CurFontFamily = "andysans";
    sv->CurCachedFont = NULL;
    sv->CurView = NULL;
    sv->CurColor = NULL;

    sv->tabs = tabs_Create();
}

void text__FinalizeStateVector(classID,sv)
struct classheader *classID;
struct text_statevector *sv;
{
    if(sv->tabs) {
	tabs_Death(sv->tabs);
	sv->tabs=NULL;
    } else {
	fprintf(stderr, "text: WARNING FinalizeStatevector called when sv->tabs==NULL.\n");
#ifdef DEBUG
	abort();
#endif
    }
}

static PlayTabs(sv, oldsv, styleptr)
struct text_statevector *sv, *oldsv;
struct style * styleptr;
{
    /* Tab updating is defined as copying over all of the old tabs and then */
    /* applying the modifiers in the style to the new tabs. */

    long numTabChanges;
    struct tabentry **TabChangeArray;
    long i;

    if (oldsv->tabs)
	sv->tabs = oldsv->tabs;
    else
	sv->tabs = tabs_Create();

    style_GetTabChangeList(styleptr, &numTabChanges, &TabChangeArray);
     for (i = 0; i < numTabChanges; i++)
	sv->tabs = tabs_ApplyChange(sv->tabs, TabChangeArray[i]);
    if (TabChangeArray)
	free(TabChangeArray);
}

/* This routine takes a pointer to a state vector, a pointer and the */
/* style to use, and plays that style over the state vector. */

static void PlayStyle(sv, styleptr)
struct text_statevector *sv;
struct style *styleptr;
{
    register long delta;
    struct text_statevector oldvalues;
    char *color;


    /* Using new style system.  Store old values for use in calculations. */

    oldvalues = *sv;

    /* Font Faces */

    sv->CurFontAttributes |= styleptr->AddFontFaces;
    sv->CurFontAttributes &= styleptr->OutFontFaces;

    /* Font Sizes */

    switch (styleptr->FontSize.SizeBasis) {
        case style_PreviousFontSize:
            sv->CurFontSize += styleptr->FontSize.Operand;
            break;
        case style_ConstantFontSize:
            sv->CurFontSize = styleptr->FontSize.Operand;
            break;
        default:
            /* Illegal basis */
            break;
    }

    /* Font Family */

    if (styleptr->FontFamily)
        sv->CurFontFamily = styleptr->FontFamily;

    /* Left Margin */

    delta = styleptr->NewLeftMargin.DotCvtOperand;

    switch(styleptr->NewLeftMargin.MarginBasis) {
        case style_LeftMargin:
        case style_LeftEdge:
            sv->CurLeftMargin = oldvalues.CurLeftMargin + delta;
            break;
        case style_ConstantMargin:
            sv->CurLeftMargin= delta;
            break;
        case style_RightEdge:
        case style_RightMargin:
            sv->CurLeftMargin = oldvalues.CurRightMargin + delta;
            break;
        case style_TopMargin:
        case style_TopEdge:
        case style_BottomMargin:
        case style_BottomEdge:
            /* Top and bottom margins not yet implemented */
            break;
        case style_PreviousIndentation:
            sv->CurLeftMargin = oldvalues.CurIndentation + delta;
            break;
        default:
            /* Unknown margin op */
            break;
    }

    /* Right Margin */

    delta = styleptr->NewRightMargin.DotCvtOperand;

    switch(styleptr->NewRightMargin.MarginBasis) {
        case style_LeftMargin:
        case style_LeftEdge:
            sv->CurRightMargin = oldvalues.CurLeftMargin + delta;
            break;
        case style_ConstantMargin:
            sv->CurRightMargin = delta;
            break;
        case style_RightEdge:
        case style_RightMargin:
            sv->CurRightMargin = oldvalues.CurRightMargin + delta;
            break;
        case style_TopMargin:
        case style_TopEdge:
        case style_BottomMargin:
        case style_BottomEdge:
            /* Top and bottom margins not yet implemented */
            break;
        case style_PreviousIndentation:
            sv->CurRightMargin = oldvalues.CurIndentation + delta;
            break;
        default:
            /* Unknown margin op */
            break;
    }

    /* Indent */

    delta = styleptr->NewIndentation.DotCvtOperand;

    switch(styleptr->NewIndentation.MarginBasis) {
        case style_LeftMargin:
        case style_LeftEdge:
            sv->CurIndentation = delta;
            break;
        case style_ConstantMargin:
            sv->CurIndentation = delta - sv->CurLeftMargin;
            break;
        case style_RightEdge:
        case style_RightMargin:
            sv->CurIndentation = sv->CurRightMargin - sv->CurLeftMargin + delta;
            break;
        case style_TopMargin:
        case style_TopEdge:
        case style_BottomMargin:
        case style_BottomEdge:
            /* Top and bottom margins not yet implemented */
            break;
        case style_PreviousIndentation:
            sv->CurIndentation = oldvalues.CurIndentation + delta;
            break; 
        default:
            /* Unknown margin op */
            break;
    }

    /* Script movements */

    delta = styleptr->FontScript.DotCvtOperand;

    switch(styleptr->FontScript.ScriptBasis) {
        case style_PreviousScriptMovement:
            sv->CurScriptMovement = oldvalues.CurScriptMovement + delta;
            break;
        case style_ConstantScriptMovement:
            sv->CurScriptMovement = delta;
            break;
        default:
            /* Unknown script movement */
            break;
    }

    /* Justifications */

    switch(styleptr->NewJustification) {
        case style_PreviousJustification: break;
        case style_LeftJustified:
        case style_RightJustified:
        case style_Centered:
        case style_LeftAndRightJustified:
        case style_LeftThenRightJustified: 
            sv->CurJustification = styleptr->NewJustification;
            break;
        default:
            /* Unknown justification */
            break;
    }

    /* Interline spacing */

    delta = styleptr->NewInterlineSpacing.DotCvtOperand;

    switch(styleptr->NewInterlineSpacing.SpacingBasis) {
        case style_InterlineSpacing:
            sv->CurSpacing = oldvalues.CurSpacing + delta;
            break;
        case style_InterparagraphSpacing:
            sv->CurSpacing = oldvalues.CurSpread + delta;
            break;
        case style_ConstantSpacing:
            sv->CurSpacing = delta;
            break;
        case style_AboveSpacing:
        case style_BelowSpacing:
            /* Above and below spacing not yet implemented */
            break;
        default:
            /* Unknown interline spacing */
            break;
    }

    /* Interparagraph spacing (spread) */

    delta = styleptr->NewInterparagraphSpacing.DotCvtOperand;

    switch(styleptr->NewInterparagraphSpacing.SpacingBasis) {
        case style_InterlineSpacing:
            sv->CurSpread = oldvalues.CurSpacing + delta;
            break;
        case style_InterparagraphSpacing:
            sv->CurSpread = oldvalues.CurSpread + delta;
            break;
        case style_ConstantSpacing:
            sv->CurSpread = delta;
            break;
        case style_AboveSpacing:
        case style_BelowSpacing:
            /* Above and below spacing not yet supported */
            break;
        default:
            /* Unknown interparagraph spacing (spread) */
            break;
    }

    /* Miscellaneous flags */

    sv->SpecialFlags |= styleptr->AddMiscFlags;
    sv->SpecialFlags &= styleptr->OutMiscFlags;

    /* Color */

    if( ( color = style_GetAttribute(styleptr,"color")) != NULL)
	sv->CurColor = color;

    /* Tabs */
    PlayTabs(sv, &oldvalues, styleptr);
}

/* Takes a pointer to a state vector, and a pointer to an */
/* environment, and applies the environment changes to the */
/* state vector, in the right order
  The state vector must be initialized.*/

void text__ApplyEnvironment(classID, sv, defaultStyle, env)
struct classheader *classID;
struct text_statevector *sv;
struct style *defaultStyle;
struct environment *env;
{
    if (env == NULL) {
        if (defaultStyle != NULL)
            PlayStyle(sv, defaultStyle);
        return;
    }

    text_ApplyEnvironment(sv, defaultStyle,
      (struct environment *) env->header.nestedmark.parent);

    if ((env->type == environment_Style) && (env->data.style != NULL))
        PlayStyle(sv, env->data.style);
}

struct viewref *text__FindViewreference(self, pos, len)
register struct text *self;
register long pos, len;
{
    while (len > 0) {
        long gotlen;
        char *s = text_GetBuf(self, pos, len, &gotlen);
	if(s == NULL) break;
        for (len -= gotlen; gotlen--; pos++)
            if (*s++ == TEXT_VIEWREFCHAR) {
                struct environment *curenv = environment_GetInnerMost(self->rootEnvironment, pos);
                if (curenv && curenv->type == environment_View)  
                    return curenv->data.viewref;
            }
    }
    return NULL;
}

void text__ObservedChanged(self, changed, value)
struct text *self;
struct observable *changed;
long value;
{
    long pos, len;
    static struct classinfo *vci=NULL;
    
    if (self->rootEnvironment == NULL)
        return;

    if (changed == (struct observable *) self->currentViewreference && value != observable_OBJECTDESTROYED)
        self->currentViewreference = NULL;

    pos = 0;
    len = text_GetLength(self);

    if(value==observable_OBJECTDESTROYED && (struct observable *)self!=changed) {
	if(vci==NULL) vci=class_Load("viewref");
	if(class_GetType(changed)!=vci) {
	    DelObj(self, (struct dataobject *)changed);
	}
    }
    while (len > 0) {
        long gotlen;
        char *s = text_GetBuf(self, pos, len, &gotlen);
        for (len -= gotlen; gotlen--; pos++)
            if (*s++ == TEXT_VIEWREFCHAR) {
                struct environment *curenv = environment_GetInnerMost(self->rootEnvironment, pos);
		if (curenv && curenv->type == environment_View) {
		    if(changed == (struct observable *) curenv->data.viewref) {
			if(value == observable_OBJECTDESTROYED) {
			    curenv->data.viewref = NULL;
			    text_AlwaysDeleteCharacters(self, pos, 1);
			} else {
			    text_RegionModified(self, pos, 1);
			}
			text_NotifyObservers(self, 0);
			return;
		    }
		    if(changed == (struct observable *)curenv->data.viewref->dataObject && value!=observable_OBJECTDESTROYED) {

			/* text_RegionModified(self, pos, 1); */
			text_NotifyObservers(self, 0);
			return;
		    }
		}
	    }
    }
}
static struct environment *text__EnumerateEnvironments(self,pos,len,callBack,rock)
struct text *self;
long pos,len;
boolean (*callBack)();
long rock;
{   /* calls callback(rock,self,current_pos,env) on each environment found
      starting at pos and going len characters
      if callback returns TRUE .text__EnumerateEnvironment
        returns the current environment  */
    struct environment *rootenv;
    struct environment *startenv;
    struct environment *endenv;
    struct environment *curenv;
    struct environment *newenv;
    struct environment *parentenv;
    long end;
    long i;
    long elen;
    long envpos;  

    startenv = environment_GetInnerMost(self->rootEnvironment, pos);
    endenv = environment_GetInnerMost(self->rootEnvironment, pos + len - 1);
    rootenv = environment_GetCommonParent(startenv, endenv);

    for (envpos = environment_Eval(rootenv); pos == envpos && pos + len == envpos + rootenv->header.nestedmark.length; rootenv = (struct environment *) rootenv->header.nestedmark.parent, envpos = environment_Eval(rootenv));

    curenv = rootenv;
    while (curenv != startenv)  {
	curenv = environment_GetChild(curenv, pos);
	    if((*(callBack))(rock,self,pos,curenv))
		return curenv;
    }
    i = pos;
    end = pos + len;
    while (i < end)  {
        newenv = environment_GetInnerMost(self->rootEnvironment, i);
        elen = environment_GetNextChange(self->rootEnvironment, i);
        if (elen + i > end)
            elen = end - i;
        if (curenv != newenv)  {
            parentenv = environment_GetCommonParent(curenv, newenv);
/*            levels = environment_Distance(curenv, parentenv); */
            curenv = parentenv;
            if (curenv != newenv)  {
                struct environment *stack[100];
                struct environment *te;
                int level = 0;

                for (te = newenv; te != parentenv; te = (struct environment *) te->header.nestedmark.parent)
                    stack[level++] = te;
                while (level > 0)  {
                    curenv = stack[--level];
		    if((*(callBack))(rock,self,i,curenv))
			return curenv;
                }
            }
	}
	i += elen;
    }
    return NULL;
}

struct stk {
    char *item;
    int IsReal;
    struct stk *next, *prev;
    int pos, len;
} *Top = NULL;


char *TranslateStyleFrom[] = {
    "typewriter",
    "quotation",
    "excerptedcaption",
    "signature",
    "footnote",
    NULL
};

char *TranslateStyleTo[] = {
    "fixed",
    "excerpt",
    "bold,excerpt",
    "smaller",
    "",
    NULL
};

char *
WriteStyle(env, outp, IsOpen, outbuf)
struct environment *env;
char *outp, *outbuf;
int IsOpen;
{
    char *name = env->data.style->name;
    char *temp, *s, *comma, *dum, negation[50];
    int i, IsReal=1, len;

    for (i=0; TranslateStyleFrom[i] != NULL; ++i) {
	if (!strcmp(name, TranslateStyleFrom[i])) {
	    name = TranslateStyleTo[i];
	    if(*name == (char)0) 
		return(outp);
	    break;
	}
    }
    s = name;
    while (s) {
	comma = (char *) index(s, ',');
	if (comma) *comma = (char)0;
	sprintf(negation, "</%s>", s);
	len = strlen(negation);
	dum = outp-len;
	if ((outp > outbuf) && !strncmp(dum, negation, len)) {
	    /* This is just a case of </bold><bold>, which we optimize away in the output stream when it is really easy to do so, in order to make the richtext more readable */
	    outp = dum;
	} else {
	    *outp++ = '<';
	    for(temp = s; *temp; temp++){
		*outp++ = *temp;
	    }
	    *outp++ = '>';
	}
	PushLevel(s, environment_Eval(env), env->header.nestedmark.length, IsReal);
	IsReal = 0;
	if (comma) {
	    *comma = ',';
	    s = ++comma;
	} else s = NULL;
    }
    return(outp);
}

PushLevel(s, pos, len, IsReal)
char *s;
int pos, len, IsReal;
{
    struct stk *tmp = (struct stk *) malloc(sizeof(struct stk));
    char *cp = malloc(1+strlen(s));
    strcpy(cp, s);
    tmp->item = cp;
    tmp->IsReal = IsReal;
    tmp->pos = pos;
    tmp->len = len;
    tmp->next = Top;
    tmp->prev = NULL;
    if (Top) Top->prev = tmp;
    Top = tmp;
}

char *PopLevel(IsReal)
int *IsReal;
{
    char *s;
    struct stk *tmp = Top;

    if (!Top) return(NULL);
    s = Top->item;
    *IsReal = Top->IsReal;
    Top = Top->next;
    if (Top) Top->prev = NULL;
    free(tmp);
    return(s);
}

/* This routine removes a styleNode from the list of embedded styles */
void DeleteStyleNode(styleNode)
struct stk *styleNode;
{
    if(styleNode->prev)
	styleNode->prev->next = styleNode->next;
    else Top = NULL;
    if(styleNode->next)
	styleNode->next->prev = styleNode->prev;
    if(styleNode->item)
	free(styleNode->item);
    free(styleNode);
}

static char *WriteOutBufOther(file, outbuf, outp)
FILE *file;
char *outbuf,*outp;
{
    char *savedp, *endp, *new_endp = NULL;
    boolean UglyChop = FALSE;

    /* set endp to point to the character which should begin the next line.  Need to be sure UglyChop is not in the middle of an =xx encoding. */

    savedp = endp = outp;
    if (outp - outbuf >= MAX_QP_CHARS) { /* if there are MAX_QP_CHARS or more, do special tests for =xx codes */

        /* Look for =xx codes at end of outbuf */
	if (*(outbuf + LAST_QP_CHAR) == '=')
	    endp = outbuf + LAST_QP_CHAR;
	else if (*(outbuf + (LAST_QP_CHAR - 1)) == '=')
	    endp = outbuf + (LAST_QP_CHAR - 1);
	else
	    endp = outbuf + MAX_QP_CHARS;
	savedp = endp;

	{ /* Look left for beginning of style code <xxxx.  If we find one, set endp to the LeftBracket; if not, restore endp to savedp. */
	     boolean FoundLeftBracket = FALSE;
	     boolean FoundRightBracket = FALSE;

	     endp--; /* back up one; you don't want to use the char at endp but the one before. */
	     while (!FoundLeftBracket && !FoundRightBracket && endp > outbuf) {
		 switch (*endp) {
		     case '<':
			 FoundLeftBracket = TRUE;
			 savedp = endp;
			 break;
		     case '>':
			 FoundRightBracket = TRUE;
			 endp = savedp;
			 break;
		     default:
			 endp--;
			 break;
		 }
		 if (FoundRightBracket || FoundLeftBracket)
		     break;
	     }

	     if (endp == outbuf) /* If we didn't find a style code, restore endp from savedp; hopefully we'll find a space. */
		 endp = savedp;
	 }
    }

    if (endp == savedp) /* back up one; you don't want to use the char at endp but the one before. */
	endp--;

    while (endp > outbuf) { /* Look for space so we can substitute a newline for prettier output. */
	if (isspace(*endp)) {
	    if (endp + 1 < outp)/* endp points to space; set new_endp to char after space. */
		new_endp = endp + 1; 
	    else
		new_endp = outp;
	    break;
	}
	endp--;
    }

    if (endp == outbuf) { /* Found no space; must use UglyChop; restore endp from savedp; set new_endp to NULL, signifying that there is no space. */
       endp = savedp;
       new_endp = NULL;
       UglyChop = TRUE;
    }

    PutsRange(outbuf, file, endp); /* Spit out outbuf up to, but not including, endp. */

    if (UglyChop) /* Handle UglyChop, if necessary. */
	putc('=', file);
    putc('\n', file);

    if (new_endp) /* If new_endp is non-NULL, this means we found a space and will substitute a newline for the space. */
	endp = new_endp;

    bcopy(endp, outbuf, outp - endp);
    return((char*) (outbuf + (outp - endp)));
}

/* Codes to figure out how to write out the next chunk of stuff */
#define COMING_NOTHING 0
#define COMING_PLAIN 1
#define COMING_STYLE 2
#define COMING_INSET 3

long text__WriteOtherFormat(self, file, writeID, level, usagetype, boundary)
struct text *self;
FILE *file;
long writeID;
int level;
int usagetype;
char *boundary;
{
    long pos, len;
    struct environment *rootenv;
    struct environment *startenv;
    struct environment *endenv;
    struct environment *curenv;
    struct environment *newenv;
    struct environment *parentenv;
    long end;
    long i;
    long elen;
    int levels;
    unsigned char c;
    long envpos;
    int  retcode;
    char outbuf[120],*outp,*endp, *temp;
    char *buf = NULL;
    long bufLen;
    int nextcode;
    char *charset;
    boolean terminateNewline = FALSE;

    if (strcmp(class_GetTypeName(self), "text")) {
        /* subclasses should act the same as superclasses */
        return(super_WriteOtherFormat(self, file, writeID, level, usagetype, boundary));
    }
    pos = 0;
    len = text_GetLength(self);
    Top = NULL;
    if (self->header.dataobject.writeID == writeID)  return(self->header.dataobject.id);
    self->header.dataobject.writeID = writeID;

    if (text_CheckHighBit(self)) {
	charset = (char *) environ_Get("MM_CHARSET");
	if (!charset) charset = "ISO-8859-1";
    } else charset = "US-ASCII";

    nextcode = ComingNext(self, pos);
    /* if null boundary, this is NOT multipart at all */
    if (boundary && (nextcode == COMING_STYLE || nextcode == COMING_PLAIN)) {
	fprintf(file, "\n--%s\nContent-type: text/%s; charset=%s\nContent-Transfer-Encoding: quoted-printable\n\n", boundary, (nextcode == COMING_STYLE) ? "richtext" : "plain", charset);
    }
    endp = outbuf + LAST_QP_CHAR;
    outp = outbuf;

    if (len <= 0 ) return;

    startenv = environment_GetInnerMost(self->rootEnvironment, pos);
    endenv = environment_GetInnerMost(self->rootEnvironment, pos + len - 1);
    rootenv = environment_GetCommonParent(startenv, endenv);

    for (envpos = environment_Eval(rootenv); pos == envpos && pos + len == envpos + rootenv->header.nestedmark.length; rootenv = (struct environment *) rootenv->header.nestedmark.parent, envpos = environment_Eval(rootenv));
    curenv = rootenv;
    while (curenv != startenv) {
	curenv = environment_GetChild(curenv, pos);
	if (curenv->type == environment_Style && 
	     (outp + 2 + strlen(curenv->data.style->name) > endp)) {
	    outp = WriteOutBufOther(file,outbuf,outp);
	}
	if (curenv->type == environment_Style) {
	    if (nextcode == COMING_STYLE) {
		outp = WriteStyle(curenv, outp, 1, outbuf);
	    }
	}
    }

    i = pos;

    end = pos + len;
    while (i < end)  {
	newenv = environment_GetInnerMost(self->rootEnvironment, i);
	elen = environment_GetNextChange(self->rootEnvironment, i);
	if (elen + i > end)
	    elen = end - i;
	if(Top && curenv->type == environment_View) /* we've escaped all styles before the inset and opened them back up after writing the inset; we can now assume all style info in stack pointed to by Top is valid for next elen */
	    curenv = newenv;
	else if (curenv != newenv)  {
	    char *s, *envnm;
	    int IsReal;

	    parentenv = environment_GetCommonParent(curenv, newenv);
	    levels = environment_Distance(curenv, parentenv);
	    if (nextcode == COMING_STYLE) {
		while (levels > 0)  {
		    envnm = (char *) PopLevel(&IsReal);
		    if (envnm) {
			*outp++ = '<';
			*outp++ = '/';
			for (s = envnm; s && *s; ++s) {
			    *outp++ = *s;
			}
			*outp++ = '>';
			free(envnm);
		    }
		    else break;
		    if (IsReal) levels--;
		}
	    }
	    curenv = parentenv;
	    if (curenv != newenv) {
		struct environment *stack[100];
		struct environment *te;
		int level = 0;

		for (te = newenv; te != parentenv; te = (struct environment *) te->header.nestedmark.parent)
		    stack[level++] = te;
		while (level > 0) {
		    curenv = stack[--level];
		    if (curenv->type == environment_Style && 
			 (outp + 2 + strlen(curenv->data.style->name) > endp)) {
			outp = WriteOutBufOther(file,outbuf,outp);
		    }
		    if (curenv->type == environment_Style){
			if (nextcode == COMING_STYLE) {
			    outp = WriteStyle(curenv, outp, 1, outbuf);
			}
		    }
		    else if (curenv->type == environment_View)  
			break;
		}
	    }
	}
	if (curenv->type == environment_View)  {
	    struct stk *tmp = Top;
	    if(outp != outbuf){
		/* We get here when there is less than 76 characters in the cache and it doesn't end in a hard newline */
		/* We need to add a soft-newline because this will be the last line in a multi-part part and there must be atleast one newline aside from the newline associated with the next boundary */
		outp = WriteOutBufOther(file,outbuf,outp);
		fprintf(file, "=\n"); /* soft newline */
	    }
	    if (tmp && (nextcode == COMING_STYLE)) { /* Close open environments */
		fprintf(file, "</%s>", tmp->item);
		while (tmp->next) {
		    fprintf(file, "</%s>", tmp->next->item);    
		    tmp = tmp->next;
		}
		fprintf(file, "=\n"); /* soft newline */
	    }	    
	    retcode = dataobject_WriteOtherFormat(curenv->data.viewref->dataObject , file, self->header.dataobject.writeID, 2, usagetype, boundary);
	    if (retcode) {
		if (!tmp || nextcode != COMING_STYLE) {
		    nextcode = ComingNext(self, i+curenv->header.nestedmark.length);
		}
		if (nextcode == COMING_STYLE || nextcode == COMING_PLAIN) {
		    fprintf(file, "\n--%s\nContent-type: text/%s; charset=%s\nContent-Transfer-Encoding: quoted-printable\n\n", boundary, (nextcode == COMING_STYLE) ? "richtext" : "plain", charset);
		}
	    }
	    if (tmp && (nextcode == COMING_STYLE)) { /* Reopen enviornments */
		struct stk *styleNode;
		for(styleNode = tmp; styleNode;) {
		    if(styleNode->pos + styleNode->len - 1 > i) {
			fprintf(file, "<%s>", styleNode->item);
			styleNode = styleNode->prev;
		    }
		    else { /* Don't reopen, delete, any style that ends on the inset */
			struct stk *gone = styleNode;
			styleNode = styleNode->prev;
			DeleteStyleNode(gone);
		    }
		}
		fprintf(file, "=\n");
	    }	    
	    i += curenv->header.nestedmark.length;
	    elen = 0;
	}
	elen += i;

	bufLen = 0;
	while (i < elen)  {
	    /* 	    Code for writing out actual text
		*/
	    if (bufLen == 0)
		buf = text_GetBuf(self, i, 1024, &bufLen);
	    bufLen--, c = *buf++;

	    if ((c < 32 && (c != '\n' && c != '\t'))
		|| (c == '=')
		|| ((c >= 127))) {
		static char basis_hex[] = "0123456789ABCDEF";
		*outp++ = '=';
		*outp++ = basis_hex[c>>4];
		*outp++ = basis_hex[c&0xF];
	    } else if (c == '<' &&
		       (ULstrncmp(buf, "comment>", 8) && ULstrncmp(buf, "/comment>", 9)) &&
		       (nextcode == COMING_STYLE)) {
		*outp++ = '<';
		*outp++ = 'l';
		*outp++ = 't';
		*outp++ = '>';
	    } else if(c == '\n' && (nextcode == COMING_STYLE)) {
		*outp++ = '<';
		*outp++ = 'n';
		*outp++ = 'l';
		*outp++ = '>';
		/* Flush the line now, for richtext aesthetics */
		*outp++ = '\n';
		*outp = (char)0;
		outp = WriteOutBufOther(file,outbuf,outp);
	    } else  {
		*outp++ = c;
	    }
	    if(outp > endp){
		*outp = (char)0;
		outp = WriteOutBufOther(file, outbuf, outp);
	    }
	    i++;
	}
    }

    /* flush out cached characters */
    if(outp != outbuf){ /* We get here if there are some chars in outbuf (there must be less than MAX_QP_CHARS); the remaining contents may not be newline terminated. */
	terminateNewline = TRUE;
#if 0
	*outp = (char)0;
	outp = WriteOutBufOther(file, outbuf, outp);
#else
        PutsRange(outbuf,file,outp);
#endif
    }
    if((levels = environment_Distance(curenv, rootenv)) > 0) {
	char *s;
	int IsReal;
	while (levels-- > 0) {
	    if (s = PopLevel(&IsReal)) {
		terminateNewline = TRUE;
		fputs("</", file);
		fputs(s, file);
		fputs(">", file);
		free(s);
	    }
	    else {
		break;
	    }
	    if (!IsReal) ++levels;
	}
    }
    if(terminateNewline) {
	putc('\n', file);
    }
    return(self->header.dataobject.id);
}

int ComingNext(self, pos)
struct text *self;
int pos;
{
    struct environment *e2, *e3;
    int elen;

    if(!self->rootEnvironment ||
       !(e2 = environment_GetInnerMost(self->rootEnvironment, pos)) ||
        (pos >= text_GetLength(self)))
	return(COMING_PLAIN);
    else {
	elen = environment_GetNextChange(self->rootEnvironment, pos);
	e3 = environment_GetInnerMost(self->rootEnvironment, pos + elen + 1);
    }
    if(e2->type == environment_View)
	return(COMING_INSET);
    else if(e2->type == environment_Style)
	return(COMING_STYLE);
    else if(e3) {
	if(e3->type == environment_Style)
		return(COMING_STYLE);
    	else if(e3->type == environment_View)
		return(COMING_PLAIN);
    }
    else return(COMING_PLAIN);
}

boolean text__CheckHighBit(self)
struct text *self;
{
    long i, len=text_GetLength(self);
    struct simpletext *st=(struct simpletext *)self;
    
    if(st->highbitflag>=0) return st->highbitflag;
    
    st->highbitflag=0;
    for(i=0;i<len;i++) {
	if(text_GetChar(self, i)&0x80) {
	    if(text_GetChar(self, i)==TEXT_VIEWREFCHAR) {
		struct environment *e= environment_GetInnerMost(self->rootEnvironment, i);
		if(e->type==environment_View) continue;
	    }
	    st->highbitflag=1;
	    return st->highbitflag;
	}
    }
    return st->highbitflag;
}
