/* $Id: html.c,v 1.7 1994/05/16 15:50:48 rr2b Exp $ */
/* This is a HTML viewing/editing object */
/* Unfortunately, there's lots of duplicate information between the stylesheet
 * info and the tables below.  It would be nice if we could snarf the info 
 * from the stylesheet, but it's just too much hassle
 */

/*
 * Copyright 1993, City University
 * Copyright 1993, 1994, Nick Williams. 
 * 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * City University, Nick Williams, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 *
 * City University, Nick Williams, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL City University, Nick Williams, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */
/* $Disclaimer: 
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
 *  $ */

#include <andrewos.h> /* strings.h */
#include <class.h>
#include <ctype.h>
#include <stdio.h>

#include <attribs.h>
#include <buffer.ih>
#include <dataobj.ih>
#include <environ.ih>
#include <envrment.ih>
#include <gif.ih>
#include <icon.ih>
#include <iconview.ih>
#include <im.ih>
#include <image.ih>
#include <imagev.ih>
#include <keystate.ih>
#include <proctbl.ih>
#include <stylesht.ih>
#include <style.ih>
#include <nstdmark.ih>
#include <tree23.ih>
#include <text.ih>
#include <view.ih>
#include <viewref.ih>

/* So that we can feedback. Ick. */
#include <message.ih>
#include <htmlview.ih>
#include <html.eh>

#define NEED_STRING_PROTO

extern char versionString[];

static void ChangeTitle();
static void ChangeIndexable();
static struct entityMapping* getEntityMapping();
static struct entityElement* pushEntity();
static void popEntity();
static struct entityElement* withinEntity();
static void closeEntity();
static char* outputNewlines();
static void hrule();
static char* findLocalFile();


/* The following entities are supported as basic types: */
enum entityCode {
    entityHead =1,
    entityTitle,
    entityBody,
    entityHTML,

    entityAnchor,
    entityPre,
    entityHeader1,
    entityHeader2,
    entityBold,
    entityBlockQuote,
    entityEmphasised, /* 10 */
    entityAddress,
    entityParagraph,
    entityImage,
    entityIsIndex,
    entityNextId,
    entityListItem,
    entityDescTerm,
    entityDescDefun,
    entityEmpty,

    entityDescription, /* 20 */
    entityEnumerate,
    entityMenu,
    entityItemize,
    entityDirectory,

    entityItalic,
    entityUnderline,
    entityTypewriter,
    entityStrong,
    entityVariable,
    entityCitation,
    entityCode,
    entitySample,
    entityKeyboard,

    entityRule,
    entityBreak,
    entityForm,
    entityInput,
    entitySelection,
    entityOption,

    entityList = (1<<8),
};

#define entFlag(x) ((1)<<((x)+16))
#define entFlagsEnvironment 0
#define entFlagsNewline entFlag(0)
#define entFlagsSingle  entFlag(1)
#define entFlagsCompact entFlag(2)
#define entFlagsReplace entFlag(3)
#define entFlagsParagraph entFlag(4)
#define entFlagsParExplicit entFlag(10)

#define entFlagsNoStyle entFlag(6)
#define entFlagsUnknown entFlag(7)
#define entFlagsAdornment entFlag(8)

/* And this is how they look... */
struct entityMapping {
    char* string;
    enum entityCode code;
    int flags;
    void (*fn)(); /* Function to call when environment ends */
} basicEntities[] = {
    /* Top level things */
    { "head",    entityHead,       0 },
    { "body",    entityBody,       entFlagsNoStyle },
    { "html", entityHTML, entFlagsSingle|entFlagsNoStyle },

    /* Things that go into header */
    { "title",   entityTitle,      0, ChangeTitle },
    { "nextid",  entityNextId,     entFlagsSingle },
    { "isindex", entityIsIndex,    entFlagsSingle, ChangeIndexable },

    /* Things that go into the body */
    { "a",	 entityAnchor,	   0 },
    { "pre",     entityPre,        entFlagsNewline|entFlagsParagraph },
    { "h1",      entityHeader1,    entFlagsNewline|entFlagsParagraph },
    { "h2",      entityHeader2,    entFlagsNewline|entFlagsParagraph },
    { "h3",      entityHeader2,    entFlagsNewline|entFlagsParagraph },
    { "h4",      entityHeader2,    entFlagsNewline },
    { "h5",      entityHeader2,    entFlagsNewline },
    { "h6",      entityHeader2,    entFlagsNewline },
    { "b",       entityBold, 0 },
    { "i",       entityItalic, 0 },
    { "u",       entityUnderline, 0 },
    { "tt",      entityTypewriter, 0 },
    { "strong",  entityStrong, 0 },
    { "em",      entityEmphasised, 0 },
    { "var",     entityVariable, 0 },
    { "cite",    entityCitation, 0 },
    { "code",    entityCode, 0 },
    { "samp",    entitySample, 0 },
    { "kbd",     entityKeyboard, 0 },
    { "address", entityAddress,    entFlagsNewline|entFlagsCompact },
    { "p",       entityParagraph,  entFlagsNewline|entFlagsParagraph|entFlagsParExplicit|entFlagsSingle|entFlagsNoStyle },
    { "blockquote", entityBlockQuote, entFlagsNewline },
    { "img",     entityImage, entFlagsSingle },
    { "hr", entityRule, entFlagsSingle|entFlagsUnknown|entFlagsNewline },
    { "br", entityBreak, entFlagsNewline|entFlagsParExplicit|entFlagsSingle|entFlagsNoStyle },

    /* List environments */
    { "dl",	 entityList|entityDescription, entFlagsNewline },
    { "ul",	 entityList|entityItemize,    entFlagsNewline },
    { "ol",	 entityList|entityEnumerate,  entFlagsNewline },
    { "menu",	 entityList|entityMenu,	      entFlagsNewline },
    { "dir",	 entityList|entityDirectory,  entFlagsNewline },

    /* The elements of lists */
    { "li",	entityListItem,	    entFlagsReplace|entFlagsNewline }, 
    { "dt",	entityDescTerm,	    entFlagsReplace|entFlagsNewline },
    { "dd",	entityDescDefun,    entFlagsReplace|entFlagsNewline },

    /* Forms support */
    { "form", entityForm, 0 },
    { "input", entityInput, entFlagsSingle|entFlagsUnknown },
    { "select", entitySelection, entFlagsUnknown },
    { "option", entityOption, entFlagsReplace |entFlagsUnknown|entFlagsNewline },
    { 0 }
};
struct entityMapping noEntity = { 0, entityEmpty, entFlagsUnknown };

struct styleMapping {
    char* styleName;	
    char* entityName;
};
struct styleMapping stylemap[] = {
    { "address", "address" },
    { "anchor",  "a" },
    { "header1", "h1" },
    { "header2", "h2" },
    { "header3", "h3" },
    { "header4", "h4" },
    { "header5", "h5" },
    { "header6", "h6" },
    { "header7", "h7" },
    { "header8", "h8" },
    { "bold",    "b" },
    { "italic",  "i" },
    { "typewriter", "tt" },
    { "underline", "u" },
    { "emphasised", "em" },
    { "preformatted", "pre" },
    { "list-item", "li" },
    { "bulleted-list", "ul" },
    { "enumerated-list", "ol" },
    { "directory", "dir" },
    { "data-tag", "dt" },
    { "data-description", "dd" },
    { 0, 0 }
};

/* The entities are as follows */
struct entityElement {
    char string[256];
    long start;
    long length;
    long data;
    struct style* style;
    struct entityMapping* em;
    struct entityElement* prev;
};

/* This is for the keys of variables stored as attributes on tags */
struct keylist {
    char name[80];
    struct keylist* next;
};
static struct keylist* keyList = 0;

/* Some constants... */
/* These are the return results of the FindEntity routine */
#define htmlFoundEntity   0
#define htmlPartialEntity 1
#define htmlNoEntity      2

static char* whiteSpace = " \t\n";
static char* parBreakString = "\n\n<p>";
static char* breakString = "<br>\n";

static char errbuf[200]; /* A string buffer to temporarily play with error messages */
static char errString[] = "\nErrors encountered while reading document:\n\n";

/* This is the name of the attribute attached to the styles */
static char* styleHTMLCodes = "htmlcodes";

/* ------------------------------------------------------------------------ */
/*  Management and utility routines for the magic mappings                   */
/* ------------------------------------------------------------------------ */

/*
 * The next few declarations are for managing the translation file which maps 
 * special characters to html entities 
 */
struct HTMLMagicMapping {
    char* magicstring;
    int magiclen;
    char* string;
};
static struct HTMLMagicMapping HTMLDefaultCharMap[] = {
    { "<", 1, "lt" },
    { ">", 1, "gt" },
    { "&", 1, "amp" },
    { "\"", 1, "quot" },
    { 0, 0 }
};
static struct HTMLMagicMapping* HTMLCharMap = HTMLDefaultCharMap;
static char* HTMLMagicCharacters = 0;

/* 
 * The InitializeClass() is here to parse the mapping file which translates 
 * between special characters (e.g. >, <, and &) and the HTML entity which 
 * represents them (e.g. "&gt;", "&lt;" and "&amp;").
 */
boolean 
html__InitializeClass(classID)
struct classheader *classID;
{
    struct HTMLMagicMapping* HTMLCharMap = 0;
    struct HTMLMagicMapping* hmap;
    char* HTMLMagicFile;
    int mapcount = 64; 
    char* hmstr;
    FILE* fp;

    if (!(HTMLMagicFile = environ_Get("HTMLMAGICFILE"))) {
        HTMLMagicFile = environ_AndrewDir("/lib/htmlmagic");
    }

    fp = fopen(HTMLMagicFile, "r");
    if (fp) {
	int mc = 0;
	char buf[256];
	char* mstr;
	char* estr;	

	fgets(buf, sizeof(buf), fp);
	if (isdigit(*buf)) {
	    mapcount = atoi(buf);
	    HTMLCharMap = (struct HTMLMagicMapping*)malloc(sizeof(struct HTMLMagicMapping)*(mapcount+1));
	
	    if (HTMLCharMap) {
	        hmap = HTMLCharMap;
	        while (fgets(buf, sizeof(buf), fp)) {
		    if (buf[0] == '#') {
		        continue;
		    }
		    mstr = strtok(buf, " \t");
		    if (!mstr) {
		        continue;
		    }
		    estr = strtok(0, " \t\n");
		    if (estr) {
		        int ml = strlen(mstr);
		        hmap->magicstring = (char*)malloc(ml+1);
		        hmap->string = (char*)malloc(strlen(estr)+1);
		        if (!(hmap->magicstring && hmap->string)) {
			    fclose(fp);
			    return FALSE;
		        }
		        strcpy(hmap->magicstring, mstr);
		        strcpy(hmap->string, estr);
		        hmap->magiclen=ml;
		        hmap++,mc++;
		        if (mc==mapcount) {
			    /* Stop here. Broken magic file*/
			    break;
		        }
		    }
	        }
	        hmap->magicstring = 0;
	    }	    
	}
	fclose(fp);
    }

    if (HTMLCharMap == 0) {
	HTMLCharMap = HTMLDefaultCharMap;
    }

    /* Build up the string containing all our special characters */
    HTMLMagicCharacters = (char*) malloc(sizeof(char)*mapcount);
    hmstr = HTMLMagicCharacters;
    for (hmap = HTMLCharMap; hmap && hmap->magicstring; hmap++) {
	*hmstr++ = *(hmap->magicstring);
    }
    *hmstr='\0';
    return TRUE;
}

/*
 * Take a "magic" character and return a string containing the html entity
 * which represents that character.  len is set to the length of the return
 * string.
 */  
char*
html_MagicToString(x,len)
char* x;
int* len;
{
    struct HTMLMagicMapping* hm;
    static char buf[32];

    for (hm = HTMLCharMap; hm && hm->magicstring; hm++) {
	if (strncmp(hm->magicstring, x, hm->magiclen)==0) {
	    sprintf(buf, "&%s;", hm->string);
	    *len = hm->magiclen;
	    return buf;
	}
    }
    return 0;
}

/* 
 * Take a string which may be a html entity referring to a character.
 * If it is, then the "magic" string which the html translates to is returned.
 * else 0 is returned
 */
char*
html_StringToMagic(str)
char* str;
{
    struct HTMLMagicMapping* hm;
    for (hm = HTMLCharMap; hm && hm->magicstring; hm++) {
	if (strcmp(hm->string, str)==0) {
	    return hm->magicstring;
	}
    }
    return 0;
}

/*
*/
void
html__Inform(self, msg)
struct html* self;
char* msg;
{
    struct text* errtext;
    static char buf[256];

    if (!self->errorBuffer) {
	self->errorBuffer = buffer_Create("HTML-Errors", NULL, "text", NULL);
	buffer_SetScratch(self->errorBuffer, TRUE);
	errtext =(struct text *)buffer_GetData(self->errorBuffer);

	text_InsertCharacters(errtext, 0, versionString, strlen(versionString));
	text_InsertCharacters(errtext, text_GetLength(errtext), errString, strlen(errString));
    }

    sprintf(buf, "line %ld: %s\n", self->lineNumber, msg);

    if (self->errorBuffer) {
	errtext =(struct text *)buffer_GetData(self->errorBuffer);
	text_InsertCharacters(errtext, text_GetLength(errtext), buf, strlen(buf));
    } else {
	fprintf(stderr, buf);
    }
}

boolean
html__HasErrors(self)
struct html* self;
{
    if (self->errorBuffer) {
	return TRUE;
    } else {
	return FALSE;
    }
}

/* ------------------------------------------------------------------------ */
/* Management of instances of entities: Each instance of an entity may have */
/* variables attached to it                                                 */
/* ------------------------------------------------------------------------ */

/* Attach a variable assignment to a style */
/* If the value is NULL, then empty string ("") is placed as value */
static void 
storeVar(style, key, value)
struct style* style;
char* key;
char* value;
{
    struct keylist* k = keyList;
    char* s;
    for (s = key; s && *s; s++) {
	if (isupper(*s)) {
	    *s = tolower(*s);
	}
    }

    for (k = keyList; k && (strcmp(k->name, key)!=0) ; k = k->next)
	;

    if (!k) {
	/* Record the key for later use... */
	struct keylist* nk = (struct keylist*) malloc(sizeof(struct keylist));
	if (nk) {
	    strcpy(nk->name, key);
	    nk->next = keyList;
	    keyList = nk;
	}
    }
    style_AddAttribute(style, key, value ? value : "");
}

/* Parse a variable list from a tag calling storeVar on all the variables */
static void
addVars(style, vars)
struct style* style;
char* vars;
{
    char* s = vars;
    char* value = 0;
    char* key = s;
    int inquote = 0;

    for ( ; s && *s; s++) {
	if (*s == '\"') {
	    inquote = !inquote;
	} else if (*s == '=' && !inquote) {
	    *s = '\0';
	    value = s+1;
	} else if (strchr(whiteSpace, *s)) {
	    if (!inquote) {
		/* We have the end of a variable. Store it */
		*s = '\0';
		storeVar(style, key, value);
		value = 0;
		key = s+1;
	    } /* else ignore it */
	}
    }
    if (*key != '\0') {
	storeVar(style, key, value);
    }
}

char*
html_StyleToVariables(style)
struct style* style;
{
    /* XXX: We don't watch to see if buf is overrun! */
    struct keylist* k;
    char* s;
    static char buf[256];
    char* bufptr = buf;

    buf[0] = '\0';
    for (k = keyList; k; k = k->next) {
	s = style_GetAttribute(style, k->name);
	if (s) {
	    *bufptr++ = ' ';
	    /* We have a valid variable attached to this style. Dump it */
	    if (s && *s != '\0') {
		sprintf(bufptr, "%s=%s", k->name, s);
	    } else {
		strcpy(bufptr, k->name);
	    }
	    bufptr += strlen(bufptr);
	}
    }
    return buf;
}

/* ------------------------------------------------------------------------ */
/* Some routines for outside use.  I.e. the view object                     */
/* ------------------------------------------------------------------------ */

void
html__ChangeTitle(self, name)
struct html* self;
char* name;
{
    struct buffer* buf = buffer_FindBufferByData(self);
    if (self->title)  {
	free(self->title);
    }
    self->title = (char*) malloc(strlen(name)+1);
    if (self->title) {
	strcpy(self->title, name);
	if (buf) {
	    buffer_SetName(buf, self->title);
	}
	html_SetModified(self);
	html_NotifyObservers(self, observable_OBJECTCHANGED);
    } else if (buf) {
	buffer_SetName(buf, "Untitled");
    }
}

void
html__ChangeIndexable(self, flag)
struct html* self;
int flag;
{
    self->isindex = flag;
    html_SetModified(self);
    html_NotifyObservers(self, observable_OBJECTCHANGED);
}

void
html__AddLink(self, inpos, len, uri)
struct html* self;
long inpos;
long len;
char* uri;
{
    int pos = inpos;
    char vars[256];
    struct entityMapping* em;
    struct entityElement* ep;

    em = getEntityMapping("a"); /* Get the stuff about this */
    sprintf(vars, "href=%s", uri);
    ep = pushEntity(self, &pos, em, "a", vars, 1);
    pos += len;
    closeEntity(self, ep, &pos, 1);
    popEntity(self);
    html_RegionModified(self, inpos, len);
    html_NotifyObservers(self, observable_OBJECTCHANGED);
}

char*
html__GetAnchorDest(self, pos)
struct html* self;
long pos;
{
    struct environment* env;

    env = environment_GetInnerMost(self->header.text.rootEnvironment, pos);
    while (env && !(env->type == environment_Style && strcmp(env->data.style->name, "anchor") == 0)) {
	env = (struct environment*)environment_GetParent(env);
    }
    if (!env) {
	return 0;
    }

    return html_StyleToVariables(env->data.style);
}

char*
html__GetAttribute(self, env, attr)
struct html* self;
struct environment* env;
char* attr;
{
    if (env->type != environment_Style) {
	return 0;
    } else {
	return style_GetAttribute(env->data.style, attr);
    }
}

void
html__ChangeAttribute(self, tv, env, attr, value)
struct html* self;
struct view* tv;
struct environment* env;
char* attr;
char* value;
{
    char cb[256];
    char* ptr;
    cb[0] = '0';

    /* XXX:  BUG! NYI
     * We need to take a copy of the style and free the old one, otherwise
     * we'll be adding this attribute onto all styles... 
     */
    if (env->type == environment_Style) {
	if (value && (*value=='\0' || strcmp(value, "\"\"")) == 0) {
	    style_RemoveAttribute(env->data.style, attr);
	} else {
	    storeVar(env->data.style, attr, value);
	}

	/* Check to see if this modification should trigger any callback */
	strcpy(cb, "callback:");
	strcat(cb, attr);
	if (ptr = style_GetAttribute(env->data.style, cb)) {
	    /* Now we use the proctable to call the callback */
	    /* This code is taken from the metax class */
	    struct proctable_Entry *proc;
	    struct im *im = view_GetIM(tv);

	    printf("doing a callback: %s\n", ptr);
	    proc = proctable_Lookup(ptr);
	    if(proc) {
		switch(keystate_DoProc(im->keystate, proc, 0, tv)) {
		    case keystate_NoProc:
			message_DisplayString(im, 0, "Could not load procedure");
			break;
		    case keystate_TypeMismatch:
			message_DisplayString(im, 0, "Bad command");
			break;
		}
	    } else {
		message_DisplayString(im, 0, "Do nothing");
	    }
	}
	/* We're done */
	html_SetModified(self);
	html_NotifyObservers(self, observable_OBJECTCHANGED);
    }
}

void
html__GetAttributeList(self, env, list, count)
struct html* self;
struct environment* env;
char* list[];
int* count;
{
    struct keylist* k;
    char* s;

    *count = 0;
    if (env->type != environment_Style) {
	return;
    }
    
    for (k = keyList; k; k = k->next) {
	s = style_GetAttribute(env->data.style, k->name);
	if (s) {
	    /* We have a valid variable attached to this style. Dump it */
		list[*count] = (char*)malloc(strlen(k->name)+1);
		strcpy(list[*count], k->name);
		(*count)++;
	}
    }
}

/* ------------------------------------------------------------------------ */
/* Internal callbacks do not need to mess with modified flags or anything
 * like that: they are purely used during parsing. However, at the end of 
 * a parse run, you should make sure you call the SetModified and
 * NotifyObservers methods.                                                 */
/* ------------------------------------------------------------------------ */
static void
ChangeTitle(self, ep, buf, len) /* Internal Callback */
struct html* self;
struct entityElement* ep;
char* buf;
long len;
{
    if (self->title)  {
	free(self->title);
    }
    self->title = (char*) malloc(len+1);
    strncpy(self->title, buf, len);
    self->title[len]='\0';
}

static void
ChangeIndexable(self, ep, buf, len) /* Internal Callback */
struct html* self;
struct entityElement* ep;
char* buf;
long len;
{
    self->isindex = 1;
}


/* ------------------------------------------------------------------------ */
/* Play around with entity environments                                     */
/* ------------------------------------------------------------------------ */

/* getEntityCode: take a string, do a lookup and return the code */
static struct entityMapping*
getEntityMapping(string)
char* string;
{
    struct entityMapping* b;
    for (b = basicEntities; b->string; b++) {
	if (strcmp(b->string, string) == 0) {
	    return b;
	}
    }
    return &noEntity;
}

/* Take an entity off the stack */
static void 
popEntity(self)
struct html* self;
{
    struct entityElement* e = self->entities;
    if (e) {
	self->entities = e->prev;
	/* We don't destroy the sub objects, as these are now hooked into the text object... */
	free(e);
    }
}

/* Push an entity onto the stack. This should be called when parsing
 * It records the current position and prepares a style to plop down
 * when the end of the entity is known
 */
static struct entityElement*
pushEntity(self, pos, em, name, vars, force)
struct html* self;
long* pos;
struct entityMapping* em;
char* name;
char* vars;
int force;
{   
    char* string;
    char buf[256];
    struct entityElement* e;
    struct environment *env;

    e = (struct entityElement*) malloc(sizeof(struct entityElement));
    if (e) {
	/* adjust the list with the new entity, default values */
	e->prev  = self->entities;
	e->em    = em;
	if (pos) {
	    e->start = *pos;
	} else {
	    e->start = 0;
	}
	e->style = 0;
	e->data  = 0;
	strcpy(e->string, name);

	if (force >= 0) {
	    if (!withinEntity(self, entityHead) || force) {
		struct style* thisStyle;
		/* We find the stylename no matter what, as it's useful */
		struct styleMapping* sm = stylemap;
		for (; sm && sm->styleName && strcmp(sm->entityName, name)!=0; sm++) {
		    /* empty loop */;
				}
		if (sm->styleName) {
		    string = sm->styleName;
		} else {
		    string = name;
		}

		if (!(em->flags & entFlagsNoStyle)) {
		    thisStyle = stylesheet_Find(html_GetStyleSheet(self), string);

		    /* Get the thisStyle for this entity, make one up if it's bogus */
		    if (!thisStyle) {
			sprintf(errbuf, "could not load the style \"%s\". An error in the html template?", name);
			html_Inform(self, errbuf);
			/* If things go wrong, who you gunna call? */
			thisStyle = style_New();
			style_SetName(thisStyle, name);
			style_AddAttribute(thisStyle, "color", "blue");
		    }

		    /* Create a new thisStyle, which is copy of the norm as handle for these specific attributes */
		    e->style = style_New();
		    style_Copy(thisStyle, e->style);
		    /* We store all sorts of useful stuff attached to the style */
		    sprintf(buf, "%d", (em->flags | (int)em->code));
		    style_AddAttribute(e->style, styleHTMLCodes, buf);
		    if (vars && *vars) {
			addVars(e->style, vars);
		    }
		}

		if (em->code == entityRule) {
		    hrule(self, pos);
		} else if (em->code == entityImage) {
		    html_AddImage(self, pos, style_GetAttribute(e->style, "src"));
		    self->withinPar = 1;
		    (*pos)++;
		} else if (em->flags & entFlagsUnknown) {
		    /* Unknown entities: dump out the name of the entity, with spaces around it */
		    /* The spaces ensure that we get the style wrapping in the right order, by */
		    /* Frigging the start pos of this entity */
		    int len;
		    /* XXX: Should read the ALT variable and use that text if available */
		    sprintf(buf, " <%s> ", name);
		    len = strlen(buf);
		    html_AlwaysInsertCharacters(self, *pos, buf, len);
		    if (self->tagStyle) {
			env = html_AlwaysAddStyle(self, *pos+1, len-2, self->tagStyle);
			if (env) environment_SetStyle(env, FALSE, FALSE);
		    }
		    self->withinPar = 1;
		    *pos += len;
		}
	    }
	}
    } else {
	html_Inform(self, "there's no memory left to parse this document.");
    }
    self->entities = e;
    return e;
}

static struct entityElement*
entityPeek(self)
struct html* self;
{
    return self->entities;
}

/* Run down the stack, returns TRUE if we can find the entityCode anywhere */
static struct entityElement*
withinEntity(self, code)
struct html* self;
enum entityCode code;
{
    struct entityElement* e;
    for (e = self->entities; e; e=e->prev) {
	if (e->em->code == code) {
	    return e;
	}
    }
    return 0;
}

/* Run down the stack, returns entity if we can find the entityCode anywhere */
static struct entityElement*
withinEntityClass(self, code)
struct html* self;
enum entityCode code;
{
    struct entityElement* e;
    for (e = self->entities; e; e=e->prev) {
	if ((int)e->em->code & (int)code) {
	    return e;
	}
    }
    return 0;
}

/* 
  Find first entity in buffer.  Return strlower(entity name) (vars) (pos) and
  delete the entity from the input buffer.
  Return value:

  If the return value signifies a token, then varPos is 
  set to the character position at which the token should take effect.
  Note: The following routine is destructive to buf.
 */
int
html_FindEntity(buf, pos, entity, vars)
char* buf;
long* pos;
char* entity;
char* vars;
{
    char* s;
    /* .....<idXXXXXX varsXXXXXXX>.....  */
    /*      \         \          \       */
    /*       posStart  posVar     posEnd */

    char* posStart;
    char* posEnd; 
    char* posVar;
    *entity='\0';
    *pos = 0;

    if ((posStart = strchr(buf+*pos, '<'))) {
	/* We have at least a start token... */
	*pos = posStart - buf;
	if ((posEnd = strchr(posStart, '>'))) {
	    /* We have an end of a token */
	    /* token is buf[posStart..posEnd]) */
	    posVar= strpbrk(posStart, whiteSpace);
	    if (posVar && posVar < posEnd) {
		/* we have to break up entity name and variables */
		strncpy(entity, posStart+1, (posVar)-(posStart+1));
		/* Strncpy does not neccessarily null terminate! */
		entity[(posVar)-(posStart+1)] = '\0';
		strncpy(vars,   posVar+1, (posEnd)-(posVar+1));
		vars[(posEnd)-(posVar+1)] = '\0';
	    } else {
		strncpy(entity, posStart+1, (posEnd)-(posStart+1));
		entity[(posEnd)-(posStart+1)] = '\0';
		vars[0] = '\0';
	    }

	    for (s=entity;*s;s++) {	
		if (isupper(*s)) {
		    *s = tolower(*s);
		}
	    }
	    strcpy(posStart, posEnd+1); /* delete the token from the string */
	    return htmlFoundEntity;
	} else {
	    return htmlPartialEntity;
	}
    }
    return htmlNoEntity;
}

/* Call this when you find a '>' (endofentity) during read.
 * it takes the entity and plops a style into the text object
 * to represent the entity
 */
static void closeEntity(self, ep, pos, force)
struct html* self;
struct entityElement* ep;
long* pos;
int force;
{
    struct entityElement* e;
    struct environment *env;
    if (ep == 0) {
	return;
    }
    ep->length = *pos - ep->start; 

    if (ep->style == 0) {
	return;
    }
    if (ep->em->code == entityListItem) {
	if ((e = withinEntityClass(self, entityList)) == 0) {
	    /* They've given us crap! */
	    /* XXX: Tell stupid user about it */
	    html_Inform(self, "a list tag was found with no enclosing list");
	    return;
	} else {
	    char buf[5];
	    switch((int)(e->em->code) & 0xff) {
		case entityEnumerate:
		    sprintf(buf, "%ld.\t", ++e->data);
		    break;
		case entityItemize:
		case entityMenu:
		case entityDirectory:
		default:
		    strcpy(buf, "*\t");
		    break;
	    }
	    if (!html_TagItem(self, ep->start, (*pos - ep->start)-1, buf, 0, 0)) {
		html_Inform(self, "An internal error occurred: the item could not be tagged");
	    }
	    *pos += strlen(buf);
	}
    } else {
	if (!withinEntity(self, entityHead) || force) {
	    env = html_AlwaysAddStyle(self, ep->start, (*pos - ep->start) ,ep->style);
	     /* So the next inserted characters aren't part of this style */
	    if(env) {
		environment_SetStyle(env, FALSE, FALSE);
	    }
	}
    }
}

void
html__AddEntity(self, pos, len, name, vars)
struct html* self;
long pos;
long len;
char* name;
char* vars;
{   
    /* Map name to style */
    struct entityMapping* em = getEntityMapping(name);
    if (em == 0) {
	return;
    }
    pushEntity(self, &pos, em, name, vars, 1);
    pos += len;
    closeEntity(self, entityPeek(self), &pos, 1);
    popEntity(self);
    html_RegionModified(self, pos, len);
    html_NotifyObservers(self, observable_OBJECTCHANGED);
}

/* ------------------------------------------------------------------------ */
/* The class maintenance methods                                            */
/* ------------------------------------------------------------------------ */

boolean 
html__InitializeObject(classID, self)
struct classheader* classID;
struct html* self;
{
    char tbuf[32];
    struct style* is;

    html_SetCopyAsText(self, TRUE);
    html_SetExportEnvironments(self, FALSE);
    self->errorBuffer = 0;
    self->withinPar = 0;
    self->entities = 0;
    self->base = 0;
    self->title = 0;
    self->isindex = 0;
    if (html_ReadTemplate(self, "html", FALSE)) {
	html_Inform(self, "could not read the html template");
    }

    self->tagStyle = stylesheet_Find(html_GetStyleSheet(self),"tag");
    if (!self->tagStyle) {
	self->tagStyle = style_New();
	    style_SetName(self->tagStyle, "tag");
    }
    style_AddAttribute(self->tagStyle, "htmladornment", "true");
    sprintf(tbuf, "%d", entFlagsAdornment);
    style_AddAttribute(self->tagStyle, styleHTMLCodes, tbuf); 

    is = stylesheet_Find(html_GetStyleSheet(self),"list-item");
    if (!is) {
	html_Inform(self, "The template was lacking a list-item, probably a problem with the template?");
	is = style_New();
	style_SetName(is, "list-item");
    }
    self->itemStyle = style_New();
    style_Copy(is, self->itemStyle);
    sprintf(tbuf, "%d", entFlagsReplace|entFlagsNewline|entityListItem);
    style_AddAttribute(self->itemStyle, styleHTMLCodes, tbuf); 

    return TRUE;
}

void
html__FinalizeObject(classID, self)
struct classheader* classID;
struct html* self;
{
    if (self->title) {
	free(self->title);
    }
    if (self->base) {
	free(self->base);
    }
    while (entityPeek(self)) {
	popEntity(self);
    }
}

void
html__Clear(self)
struct html* self;
{
    super_Clear(self);
}

/* ------------------------------------------------------------------------ */
/* and actually displaying, reading and writing of HTML is done like so...  */
/* ------------------------------------------------------------------------ */

/*
 * Put text into the display buffer to be viewed 
 * The text will only be displayed if we are not within HEAD right now
 * Whitespace is optimised such that multiple whitespace is compressed to a 
 * single whitespace character.  All whitespace is mapped into ' ' (spaces).
 * Magic HTML thangs (such as "&lt;") are mapped into real characters.
 */
static void
maybeDisplay(self, pos, buf, inlen)
struct html* self;
long* pos;
char* buf;
long* inlen;
{
    long realLen = *inlen;
    long len = *inlen;
    int justspaced = 0;

    char* s;

    char* rdPos;
    char* wrPos = buf+len;

    if (buf==0 || (withinEntity(self, entityHead)) || buf[0] == '\0' || len == 0) {
	return;
    }

    if (!self->withinPar) {
	justspaced = 1;
    }

    if (justspaced == 0 && *pos > 0) {
      justspaced = (strchr(whiteSpace, html_GetChar(self, (*pos)-1)) ? 1:0);
    }

    if (!withinEntity(self, entityPre)) {
	/* Run thru the string removing extraneous whitespace, and making 
	 * all whitespace appear as real spaces */
	for (rdPos = wrPos = buf; len && *rdPos; len--, rdPos++) {
	    if (strchr(whiteSpace, *rdPos)) {
		/* We're looking at whitespace */
		if (justspaced == 0) {
		    *wrPos++ = ' '; justspaced++;
		} /* else, ignore multiple whitespace */
	    } else {
		*wrPos++ = *rdPos;
		justspaced=0;
	    }
	}
    }
    /* We've possibly adjusted the length of the string */
    len = (int) (wrPos - buf);	

    /* Look for	magic '&' name ';' */    
    s = buf;
    while (s && len && (s = strchr(s, '&'))) {
	char* endtok = strchr(s, ';');
	char* newstring;
	char c;
	if (s > (buf + len)) {
	    /* Past the limit */
	    break;
	}
	if (endtok) {
	    c = *endtok;
	    *endtok++='\0';
	    newstring = html_StringToMagic(s+1);
	    if (newstring != '\0') {
		int x;
		/* We need to adjust length as we're about to replace chars */
		x = (strlen(s+1) + 2)-(strlen(newstring)) ;
		realLen -= x;
		len -= x -1; /* -1 coz we're adjusting it a few lines later */
		/* Put the new string in there and the rest of the buffer */
		sprintf(s, "%s%s", newstring, endtok);
	    } else {
		*(endtok-1) = c;
	    }
	}
	s++, len--;
    }
    *inlen = realLen;
    html_AlwaysInsertCharacters(self, *pos, buf, len);
    *pos += len;
    if (len) {
	self->withinPar = 1;
    }
}

/* Output a paragraph break or a small line break */
/* Calling this will only output newlines if the entityMapping indicates that it is a good time to do this */
static int
newpar(self, eMapping, pos) 
struct html* self;
struct entityMapping* eMapping;
long pos;
{
    char c;
    long xpos;
    int count = 0;

    if ((eMapping->flags & entFlagsNewline) && self->withinPar) {
	for (xpos=pos-1; xpos > 0 && (c=html_GetChar(self, xpos)) == '\n'; xpos--)
	{ count++;}

	if (eMapping->flags & entFlagsParagraph) {
	    /* Want to output a paragraph break */
	    if (count < 2) {
		html_AlwaysInsertCharacters(self, pos, "\n\n", 2-count);
		pos += 2-count;
	    }
	    self->withinPar = 0;
	} else {
	    /* Want to output a line break */
	    if (count == 0) {
		html_AlwaysInsertCharacters(self, pos, "\n", 1);
		pos++;
	    }
	}
    }
    return pos;
}

static void
hrule(self, pos)
struct html* self;
long* pos;
{
    if (html_GetChar(self, (*pos)-1) != '\n') {
	html_AlwaysInsertCharacters(self, *pos, "\n", 1);
	(*pos)++;
    }
    html_InsertObject(self, *pos, "bp", "bpv");
    (*pos)++;
    html_AlwaysInsertCharacters(self, *pos, "\n", 1);
    (*pos)++;
    html_NotifyObservers(self, 0);
}

void
html__AddImage(self, pos, file)
struct html* self;
long* pos;
char* file;
{
    long objectID = 0;
    struct image* dat;
    char* filename;
    struct buffer* buf;
    static char defaultImage[256];

    buf = buffer_FindBufferByData(self);
    if (buf) {
	filename = findLocalFile(file, buffer_GetFilename(buf));
    } else {
	filename = file;
    }

    /* XXX: Hardcoded to use GIF. Fix up sometime (How? Requires fix of image class) */
    if (dat = (struct image*) class_NewObject("gif")) {
	if (filename) {
	    image_Load(dat, filename, NULL);
	} else {
	    /* This is all a bit of a frig to get something reasonable instead of a 200x200 blank */
	    if (!defaultImage[0]) {
		char*s = environ_GetProfile("defaultImage");
		if (s) {
		    strcpy(defaultImage, s);
		}
	    }
	    if (defaultImage[0]) {
		image_Load(dat, defaultImage, NULL);
	    } else {
		/* XXX: What to do with the image? 
		image_Destroy(dat);
		dat = 0; */
	    }
	}
    }
    if (dat) {
	html_AlwaysAddView(self, *pos, "imagev", dat);
	html_NotifyObservers(self, 0);
    }
}

static int 
fixStyles(rock, self, pos, curenv)
long rock;
struct text* self;
long pos;
struct environment* curenv;
{
    if(curenv->type == environment_Style)
    {
        environment_SetStyle(curenv, TRUE, TRUE);
    }

    /* FALSE means don't stop */
    return FALSE;
}


long
html__Read(self, file, id)
struct html* self;
FILE* file;
long id;
{
    html_SetID(self, html_UniqueID(self));
    html_ReadSubString(self, 0, file, 1);

    /* Set all the environments back with SetStyle to TRUE,
     * TRUE so that we behave like an editor
     */
    html_EnumerateEnvironments(self, 0, html_GetLength(self), (procedure) fixStyles, 1);
    return dataobject_NOREADERROR;
}

long
html__ReadSubString(self, startPos, file, quoteCharacters)
struct html* self;
long startPos;
FILE* file;
int quoteCharacters;
{
    char buf[1024];
    char entity[80]; /* The entity names are usually small. Badness */
    char vars[1024];
    long pos = 0;
    long myPos = startPos;
    char* s;
    struct entityMapping* eMapping;
    struct entityElement* ep;

    buf[0]='\0';
    *vars = '\0';
    s = fgets(buf, 1024, file);
    self->lineNumber = 1;
    while (s) {
	switch (html_FindEntity(buf, &pos, entity, vars)) {
	    case htmlFoundEntity:
		/* Print out what we have so far */
		if (pos > 0) {
		    maybeDisplay(self, &myPos, buf, &pos);
		}
		if (*entity == '/') {
		    /* End of environment */
		    /* Close all replaceable entities within our body */
		    eMapping = getEntityMapping((entity+1));
		    ep = entityPeek(self);
		    while (ep && (ep->em->code != eMapping->code) &&
			   (ep->em->flags & entFlagsReplace)) {
			closeEntity(self, ep, &myPos, 0);
			popEntity(self);
			ep = entityPeek(self);
		    }
		    if (ep == 0) {
			sprintf(errbuf, "a %s close entity was found with no matching start entity", entity);
			html_Inform(self, errbuf);
			strcpy(buf, buf+pos);
			*vars = '\0';
			break;
		    }
		    if (ep->em->code != eMapping->code) {
			sprintf(errbuf, "unmatched environments: a <%s> follows <%s>\n", entity, ep->string);
			html_Inform(self, errbuf);
			/* Get rid of the text that we've printed */
			strcpy(buf, buf+pos);
			*vars = '\0';
			break;
		    } else {
			/* Valid start/end pair. Wrap an environment up */
			closeEntity(self, ep, &myPos, 0);
			if (ep->em->fn) {
			    ep->em->fn(self, ep, buf, pos);
			}
		        popEntity(self);
		        myPos = newpar(self, eMapping, myPos);
		    }
		} else {
		    eMapping = getEntityMapping(entity);

		    /* Start of environment */
		    myPos = newpar(self, eMapping, myPos);
		    if (eMapping->flags & entFlagsReplace) {
			ep = entityPeek(self);
			if (ep && ep->em->flags & entFlagsReplace) {
			    closeEntity(self, ep, &myPos, 0);
			    popEntity(self);
			    myPos = newpar(self, eMapping, myPos);
			}
		    }   
		    pushEntity(self, &myPos, eMapping, entity, vars, 0);
		}

		if (eMapping->flags & entFlagsSingle) {
		    /* The closeEntity get's optimised to nothing, but hey */
		    closeEntity(self, entityPeek(self), &myPos, 0);
		    if (eMapping->fn) {
			eMapping->fn(self, ep, buf, pos);
		    }
		    popEntity(self);
		    myPos = newpar(self, eMapping, myPos);
		}
		/* Get rid of the text that we've printed */
		strcpy(buf, buf+pos);
		*vars = '\0';
		break;
	    case htmlNoEntity:
		pos = strlen(buf);
		maybeDisplay(self, &myPos, buf, &pos);
		buf[0] = '\0';
		pos = 0;
		/* fall thru to get more */
	    case htmlPartialEntity:
		/* Append to end of current live data */
		self->lineNumber++; /* Well, this is only approximate, because the prev. fgets may be partial */
		s = fgets(buf+strlen(buf), 1024 - strlen(buf), file);
		break;
	}

    }
    return myPos-startPos;
}

static void
writeHeader(self, file)
struct html* self;
FILE* file;
{
    fprintf(file, "<head>\n");
    if (self->title) {
	fprintf(file, "<title>%s</title>\n", self->title);
    }
    if (self->base) {
	fprintf(file, "<base href=%s>\n", self->base);
    }
    if (self->isindex) {
	fprintf(file, "<isindex>\n");
    }
    /* The nextid */
    /* The links.. ? */
    
    fprintf(file, "</head>\n\n");
}

/* Used by the write method to write some stuff */
static void 
PutsRange(self, p, fp, ep)
struct html* self;
char *p;
FILE *fp;
char *ep;
{
    if (!self->adornment) {
	while (p < ep)
	    putc(*p++, fp);
    }
}

long
html__Write(self, file, id, level)
struct html* self;
FILE* file;
long id;
int level;
{
    writeHeader(self, file);

    fprintf(file, "<body>\n");
    html_WriteSubString(self, 0, html_GetLength(self), file, 1);
    fprintf(file, "\n</body>\n");
    return self->header.dataobject.id;
}

/* Take a style and produce the html entity name which corresponds to it */
static char*
getHTML(style)
struct style* style;
{
    char* s = style_GetAttribute(style, "html");
    if (s && *s) {
	return s;
    } else {
	/* Check our mapping table */
	struct styleMapping* sm = stylemap;
	for (; sm && sm->styleName; sm++) {
	    if (strcmp(style->name, sm->styleName) == 0) {
		return sm->entityName;
	    }
	}
	return style->name;
    }
}

char*
html__EnvStart(self, outp, style, parImply, brImply, newlines)
struct html* self;
char* outp;
struct style* style;
int* parImply;
int* brImply;
int* newlines;
{
    char* temp;
    char* name;
    char* vars;
    char* s;
    struct entityMapping* em;

    s = style_GetAttribute(style, "htmladornment");
    if (s) {
	/* XXX: This is a horrible hack and shouldn't be done like this. 
	 * It should be handled at a higher level, but it's too hard right
	 * now to grok the ATK text__write() code... 
	 */
	self->adornment++;
	return outp;
    }
    if (self->adornment) {
	return outp;
    }
		 
    /* A valid entity, let's put it on the stack, so we can keep tabs... */
    name = getHTML(style);
    vars = html_StyleToVariables(style);
    em = getEntityMapping(name);
    pushEntity(self, 0, em, name, vars, -1);

    if (em->flags & entFlagsParagraph) {
	*parImply = 1;
    }
    if (em->flags & entFlagsNewline) {
	*brImply = 1;
    }
    outp = outputNewlines(*newlines, *parImply, *brImply, outp);
    *newlines = 0;
    *parImply = 0;
    *brImply = 0;

    if (em->flags & entFlagsNewline) {
	*outp++ = '\n';
    }
    if (em->flags & entFlagsParagraph) {
	*outp++ = '\n';
    }
    /* And now dump it out */
    *outp++ = '<';
    while (*name) {
	*outp++ = *name;
	name++;
    }
    while (*vars) {
	*outp++ = *vars;
	vars++;
    }
    *outp++ = '>';
    return outp;
}

char*
html__EnvEnd(self, outp, style, parImply, brImply)
struct html* self;
char* outp;
struct style* style;
int* parImply;
int* brImply;
{
    char* temp;
    char* s;
    int code;

    if ((s = style_GetAttribute(style, "htmladornment"))) {
	/* XXX: It's that hack again */
	self->adornment--;
	return outp;
    }
    if (self->adornment) {
	return outp;
    }
    popEntity(self); /* XXX: Should verify this matched the peek */
    code = html_StyleToCodes(style);
    if (code & entFlagsParagraph) {
	*parImply = 1;
    }
    if (code & entFlagsNewline) {
	*brImply = 1;
    }

    if (code & (entFlagsSingle|entFlagsReplace)) {
	return outp;
    }
    s = getHTML(style);
    *outp++ = '<';
    *outp++ = '/';
    for(temp = s; *temp; temp++){
	*outp++ = *temp;
    }
    *outp++ = '>';
    if (code & (entFlagsNewline|entFlagsParagraph)) {
	*outp++ = '\n';
    }
    return outp;
}

static char*
outputNewlines(newlines, parImplied, brImplied, outp)
int newlines;
int parImplied;
int brImplied;
char* outp;
{
    char* temp;
    if (newlines >= 2) {
	if (!parImplied) {
	    for(temp = parBreakString; *temp; temp++){
		*outp++ = *temp;
	    }
	}
	newlines = 0;
    }			    
    parImplied = 0;
    if (newlines == 1) {
	if (!parImplied && !brImplied) {
	    for (temp = breakString; *temp; temp++) {
		*outp++ = *temp;
	    }
	}
	newlines = 0;
    }   
    return outp;
}

void
html__WriteSubString(self, pos, len, file, quoteCharacters)
struct html* self;
long pos;
long len;
FILE* file;
int quoteCharacters;
{
    struct environment* rootenv;
    struct environment *startenv;
    struct environment *endenv;
    struct environment *curenv;
    struct environment *newenv;
    struct environment *parentenv;

    int lastblankset = TRUE; /* IF last character was whitespace */
    /* The following two are set if an explicit <p> or <br> is detected, OR
      * if an implied one (e.g. from a title, or somesuch) is detected.
      */
    int parImplied = 0; /* IF parbreak implied from previous style */
    int brImplied  = 0; /* IF  a line break has been implied by the last style */
    int newlines = 0;

    long end;
    long i;
    long elen;
    int levels;
    char c;
    long envpos;

    char outbuf[180],*outp,*endp,*temp;
    char *buf = NULL;
    long bufLen;

    /* Quicky optimisation */
    if (len <= 0 ) return;
    self->lineNumber = 0; /* In case any errors are produced during a write. Can it happen? */

    /* fprintf(stderr, "WriteSubString(%ld, %ld)\n", pos, len); */
    /* Make sure we start in the right state... */
    /* This should be set by looking at curenv(pos) */
    self->adornment = 0;
      
    endp = outbuf + 78; /* Hackety hack */
    outp = outbuf;

    startenv = environment_GetInnerMost(self->header.text.rootEnvironment, pos);
    endenv = environment_GetInnerMost(self->header.text.rootEnvironment, pos + len - 1);
    rootenv = environment_GetCommonParent(startenv, endenv);

    /* Determine the root of the environment, by going back up the tree */
    for (envpos = environment_Eval(rootenv);
	 pos == envpos && pos + len == envpos + rootenv->header.nestedmark.length;
	 rootenv = (struct environment *) rootenv->header.nestedmark.parent,
	 envpos = environment_Eval(rootenv));

    /* Now open all starting environments up */
    curenv = rootenv;
    while (curenv != startenv)  {
	curenv = environment_GetChild(curenv, pos);
	/* Make sure we write out buffer if it gets too full */
	if ((curenv->type == environment_Style && 
	     (outp + 2 + strlen(curenv->data.style->name) > endp))
	    || (curenv->type == environment_View && outp != outbuf)){
	    PutsRange(self, outbuf, file, outp);
	    outp = outbuf;
	}
	/* The actual open environment */
	if (curenv->type == environment_Style){
	    outp = html_EnvStart(self, outp, curenv->data.style, &parImplied, &brImplied, &newlines);
	}
    }

    /* Now, we go through the document looking at each environment */
    i = pos;
    end = pos + len;
    while (i < end)  {
        newenv = environment_GetInnerMost(self->header.text.rootEnvironment, i);
        elen = environment_GetNextChange(self->header.text.rootEnvironment, i);
        if (elen + i > end)
            elen = end - i;
        if (curenv != newenv)  {
	    /* A new environment, so make sure previous ones are closed */
            parentenv = environment_GetCommonParent(curenv, newenv);
            levels = environment_Distance(curenv, parentenv);
            while (levels > 0)  {
		if (curenv->type == environment_Style) {
		    outp = html_EnvEnd(self, outp, curenv->data.style, &parImplied, &brImplied);
		}
		curenv = (struct environment*)environment_GetParent(curenv);
                levels--;
            }  
            curenv = parentenv;
	    /* Now we're in a new environment, curenv is the parent? */
            if (curenv != newenv)  {
                struct environment *stack[100];
                struct environment *te;
                int level = 0;

		/* Open all of the environments that start at this spot */
                for (te = newenv; te != parentenv; te = (struct environment *) te->header.nestedmark.parent)
                    stack[level++] = te;
                while (level > 0)  {
                    curenv = stack[--level];
                    if ((curenv->type == environment_Style && 
                          (outp + 2 + strlen(curenv->data.style->name) > endp))
                         || (curenv->type == environment_View && outp != outbuf)){
			PutsRange(self, outbuf, file, outp);
			outp = outbuf;
                    }
                    if (curenv->type == environment_Style){
			outp = html_EnvStart(self, outp, curenv->data.style, &parImplied, &brImplied, &newlines);
                    }
                }
            }
        }
        if (curenv->type == environment_View)  {
	    /* Views (inner sub-objects) are ignored */
            if(outp != outbuf){
                /* flush out remaining cached characters */
		PutsRange(self, outbuf, file, outp);
		outp = outbuf;
            }
	    i += curenv->header.nestedmark.length;
	    elen = 0;
	}
        elen += i;

        bufLen = 0;
	if (self->adornment) {
	    i = elen; /* Jump to end of text */
	}
        while (i < elen)  {
            /* Code for writing out actual text */
            if (bufLen == 0) {
                buf = html_GetBuf(self, i, 1024, &bufLen);
	    }
            bufLen--, c = *buf++;

	    if (strchr(HTMLMagicCharacters, c)) {
		char* s;
		int len;
		s = html_MagicToString(buf-1,&len);
		while (s && *s) {
		    *outp++ = *s++;
		}
		buf+=len-1;
		lastblankset = FALSE;
	    } else {
		switch(c) {
		case '\n':
		    /* Make sure pre-formatted doesn't get munched. */
		    if (withinEntity(self, entityPre)) {
			*outp++ = c;
		    } else {
		        newlines++;
		    }
		    break;
		default:
		    /* This is a normal character */
		    /* However, multiple spaces get shrunk... unless PRE */
		    if (withinEntity(self, entityPre)) {
			*outp++ = c;
		    } else {
			outp = outputNewlines(newlines, parImplied, brImplied, outp);
			newlines = 0;
			parImplied = 0;
			brImplied = 0;

			if(c == ' ' || c == '\t' || c == '\n'){
			    if(lastblankset == FALSE){
				*outp++ = ' ';
				lastblankset = TRUE;
			    }
			} else {
			    *outp++ = c;
			    lastblankset = FALSE;
			}
		    }
		}
	    }

	    /* Buffer management */
	    if (outp > endp) {
		PutsRange(self, outbuf, file, outp);
		outp = outbuf;
            }
            i++;
        }
    }

    /* flush out cached characters */
    if(outp != outbuf){
        PutsRange(self, outbuf,file,outp);
    }
    /* And close all remaining environments */
    levels = environment_Distance(curenv, rootenv);
    while (levels-- > 0) {
	if (curenv->type == environment_Style) {
	    outp = html_EnvEnd(self, outbuf, curenv->data.style, &parImplied, &brImplied);
	}
	curenv = (struct environment*)environment_GetParent(curenv);
	PutsRange(self, outbuf, file, outp);
    }
}


/* Make the (pos,len) region a list item, with a tagged bit inserted at the start */
boolean
html__TagItem(self, pos, len, text, itemS, extraStyle)
struct html* self;
long pos, len;
char* text;
char* itemS;
struct style* extraStyle;
{
    struct environment *env;
    int tlen = strlen(text);
    struct style* iStyle = self->itemStyle;

    if (itemS) {
	iStyle = stylesheet_Find(html_GetStyleSheet(self),itemS);
    }

    if (!(self->tagStyle && self->itemStyle)) {
	return FALSE;
    }
    html_AlwaysInsertCharacters(self, pos, text, tlen);
    if (extraStyle) {
	env = html_AlwaysAddStyle(self, pos, tlen, extraStyle);
	if (env) {
	    environment_SetStyle(env, FALSE, FALSE);
	}
    }
    env = html_AlwaysAddStyle(self, pos, tlen, self->tagStyle);
    if (env) {
	environment_SetStyle(env, FALSE, FALSE);
    }
    env = html_AlwaysAddStyle(self, pos, len+tlen, iStyle);
    if (env) {
	environment_SetStyle(env, FALSE, FALSE);
    }
    html_RegionModified(self, pos, len+tlen);
    html_NotifyObservers(self, observable_OBJECTCHANGED);
    return TRUE;
}

int
html_StyleToCodes(style)
struct style* style;
{
    char* s = style_GetAttribute(style, styleHTMLCodes);
    if (!s) {
	return 0;
    }

    return atoi(s);
}

/* Look at pos and untag the paragraph, getting rid of tag and styles */
int
html__UntagItem(self, pos)
struct html* self;
long pos;
{
    struct environment* env;
    struct environment* item;
    struct environment* list;
    struct environment* enext;
    int itempos, itemlen;
    int code;
    long len;

    /* Find the tag */
    enext = environment_GetInnerMost(self->header.text.rootEnvironment, pos);

    do {
	env = enext;
	while (env && env->type != environment_Style){
	    env = (struct environment*)environment_GetParent(env);
	}
	if (!env) {
	    return 0;
	}
	code = html_StyleToCodes(env->data.style);
	if (code & (int)entityList) {
	    /* We couldn't find the item before we left the list */
	    return 0; 
	}
	enext = (struct environment*)environment_GetParent(env);
    } while (!(code & entFlagsAdornment));

    /* Find the item */
    do {
	item = enext;
	while (item && item->type != environment_Style){
	    item = (struct environment*)environment_GetParent(item);
	}
	if (!item) {
	    return 0;
	}
	code = html_StyleToCodes(item->data.style);
	if (code & (int)entityList) {
	    /* We couldn't find the item before we left the list */
	    return 0; 
	}
	enext = (struct environment*)environment_GetParent(item);
    } while ((code & 0xff) != (int)entityListItem);

    /* Find the list */
    do {
	list = enext;
	while (list && list->type != environment_Style){
	    list = (struct environment*)environment_GetParent(list);
	}
	if (!list) {
	    return 0;
	}
	code = html_StyleToCodes(list->data.style);
	enext = (struct environment*)environment_GetParent(list);
    } while (!(code & (int)entityList));

    /* Delete the list-item style */
    itempos = environment_Eval(item);
    itemlen = environment_GetLength(item);
    environment_Delete(item);
    /* delete the tag. This will automatically remove the adorning style */
    len = environment_GetLength(env);
    html_AlwaysDeleteCharacters(self, environment_Eval(env), len);
    /* Delete the list style */
    environment_Remove(self->header.text.rootEnvironment, itempos, itemlen-len+1, environment_Style, FALSE);

    /* Tell people about it */
    html_RegionModified(self, itempos, itemlen);
    html_NotifyObservers(self, observable_OBJECTCHANGED);
    return len;
}


struct environment*
html__GetEntityEnvironment(self, pos, env)
struct html* self;
long pos;
struct environment* env;
{
    struct environment* e, *parent;
    char* s;

    if (env) {
	e = env;
    } else {
	e = environment_GetInnerMost(self->header.text.rootEnvironment, pos);
    }

    while(e) {
	/* Check if this is the one. */
	if (e->type == environment_Style && (s = html_GetAttribute(self, e, "htmladornment")) == 0) {
	    break;
	}

	parent = (struct environment*)environment_GetParent(e);
	e = parent;
    };

    return e;
}

/*
 * Find a file by looking either at the relative file passed in as
 * second parameter, or by looking along a list of pseudo-roots
 */
static char*
findLocalFile(path, relativeRoot)
char* path;
char* relativeRoot;
{
    static char buf[256];
    char* s;
    char* ptr = buf;

    if (*path == '/') {
	ptr = environ_GetProfile("webPath");
	while (ptr) {
	    s = strchr(ptr, ',');
	    if (s) {
		strncpy(buf, ptr, s-ptr);
		buf[s-ptr] = '\0';
		ptr = s+1;
	    } else {
		strcpy(buf, ptr);
		ptr = 0;
	    }
	    strcat(buf, path);
	    /* Check buf */
	    if (access(buf, R_OK) == 0) {
		return buf;
	    }
	}
	return 0;
    } else {
	/* Relative pathname, use current buffer */
	strcpy(buf, relativeRoot);
	if (s = strrchr(buf, '/')) {
	    s[1] = '\0';
	} else {
	    /* Bizarreness happening! */
	    return 0;
	}
	strcat(buf, path);
	if (access(buf, R_OK) == 0) {
	    return buf;
	}
	return 0;
    }
}

