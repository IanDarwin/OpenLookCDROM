/* File srctext.c created by R L Quinn

   srctext, a Source Text mode for ATK. */

/* Copyright 1988,1994 Carnegie Mellon University and IBM. All rights reserved.
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/srctext.c,v 1.19 1994/02/28 21:02:02 rr2b Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <ctype.h>


#include <envrment.ih>
#include <style.ih>
#include <stylesht.ih>
#include <fontdesc.ih>
#include <attribs.h>
#include <environ.ih>
#include <im.ih> /* for CheckLineLengths' dialog box */
#include <message.ih> /* for CheckLineLengths' dialog box */
#include <buffer.ih> /* for CheckLineLengths' checkpoint detector */

#include <dataobj.ih> /* for SrcInsets */
#include <viewref.ih> /* for SrcInsets */
#include <sys/errno.h> /* for SrcInsets */
#include <filetype.ih> /* for SrcInsets */
#include <dict.ih> /* for SrcInsets */

#include "dogtags.h"
#include "compress.ih"
#include "srctext.eh"

#define TEXT_VIEWREFCHAR '\377'

/* these characters look like << and >> (in most fonts), and are used to delimit SrcInsets in the saved document */
#define SRCINSET_STARTCHAR '\253'
#define SRCINSET_ENDCHAR '\273'

/* GetEnvironment returns the environment enclosing pos, or the root environment if no style is there */
struct environment *srctext__GetEnvironment(self,pos)
struct srctext *self;
long pos;
{
    struct environment *me=NULL, *root=self->header.text.rootEnvironment;
    if (root)
	me= environment_GetEnclosing(root,pos);
    return me;
}

/* nextInset returns the position of the first inset in the text after pos.  It searches a maximum of len chars after pos, and returns -1 if none were found */
static long nextInset(self,pos,len)
struct srctext *self;
long pos, len;
{
    long VRCpos;
    TryNextVRC: ; /* jump back here if the ViewRefChar wasn't REALLY an inset */
    VRCpos= srctext_Index(self, pos, TEXT_VIEWREFCHAR, len);
    if (VRCpos>=0)
	/* make sure it's not just a plain ol' \377 char */
	if (environment_GetInnerMost(self->header.text.rootEnvironment, VRCpos)->type == environment_View)
	    return VRCpos;
	else {
	    pos= VRCpos+1;
	    goto TryNextVRC;
	}
    return -1;
}

/* GetStyle returns the style enclosing pos, or NULL if no style is there */
struct style *srctext__GetStyle(self,pos)
struct srctext *self;
long pos;
{
    struct environment *me=srctext_GetEnvironment(self,pos);
    return (me!=self->header.text.rootEnvironment) ? me->data.style : NULL;
}

/* srctext_WrapStyle calls environment_InsertStyle. It makes sure it doesn't wrap styles around insets, since we can't nest environments. This method should be called by RedoStyles, but interactive things like BackwardCheckWord should call WrapStyleNow instead. */
void srctext__WrapStyle(self, pos,len, style, begflag,endflag)
struct srctext *self;
long pos,len;
struct style *style;
boolean begflag,endflag;
{
    struct environment *newenv;
    if (style!=NULL) {
	long nextinset=nextInset(self, pos,len);
	long distToNextEnv=environment_GetNextChange(self->header.text.rootEnvironment, pos);
	/* only call environment_Remove if we suspect it's absolutely necessary, because it is S L O W */
	if (distToNextEnv<len || srctext_GetStyle(self,pos+1))
	    environment_Remove(self->header.text.rootEnvironment, pos,len, environment_Style, TRUE);
	newenv=environment_InsertStyle(self->header.text.rootEnvironment, pos, style, TRUE);
	if (nextinset < 0) { /* no insets in the way */
	    environment_SetLength(newenv, len);
	    environment_SetStyle(newenv,begflag,endflag);
	}
	else { /* dang, we can't nest environments so we have to jump past that inset */
	    environment_SetLength(newenv, nextinset-pos);
	    environment_SetStyle(newenv,begflag,TRUE);
	    len-= nextinset-pos+1;
	    pos= nextinset+1;
	    srctext_WrapStyle(self, pos,len, style, TRUE,endflag); /* recurse to style the rest */
	}
    }
}

/* srctext_WrapStyleNow() wraps a style, and ALSO flags the region as modified. This should be called for all *interactive* styling that results from user interaction in the view, such as BackwardCheckWord */
void srctext__WrapStyleNow(self, posn,len, style, begflag,endflag)
struct srctext *self;
long posn, len;
struct style *style;
boolean begflag,endflag;
{
    if (style!=NULL) {
	srctext_WrapStyle(self, posn,len, style, begflag,endflag);
	srctext_RegionModified(self, posn,len);
    }
}

/* The actual work is done in a static function, so InitializeObject can call it.  The *method* is a dummy that calls this. */
static void SetupStyles(self)
struct srctext *self;
{
    struct stylesheet *ss=srctext_GetStyleSheet(self);
    self->kindStyle[UPRCSE]= NULL;
    if ((self->comment_style= stylesheet_Find(ss, "comment")) == NULL) {
	self->comment_style= style_New();
	style_SetName(self->comment_style, "comment");
	style_AddNewFontFace(self->comment_style, fontdesc_Italic);
	stylesheet_Add(ss, self->comment_style);
    }
    if ((self->linecomment_style= stylesheet_Find(ss, "linecomment")) == NULL) {
	self->linecomment_style= style_New();
	style_Copy(self->comment_style, self->linecomment_style);
	style_SetName(self->linecomment_style, "linecomment");
	stylesheet_Add(ss, self->linecomment_style);
    }
    if ((self->function_style= stylesheet_Find(ss, "function")) == NULL) {
	self->function_style= style_New();
	style_SetName(self->function_style, "function");
	style_AddNewFontFace(self->function_style, fontdesc_Bold);
	style_SetFontSize(self->function_style, style_PreviousFontSize, 2);
	stylesheet_Add(ss, self->function_style);
    }
    if ((self->label_style= stylesheet_Find(ss, "label")) == NULL) {
	self->label_style= style_New();
	style_SetName(self->label_style, "label");
	style_AddNewFontFace(self->label_style, fontdesc_Bold | fontdesc_Italic);
	stylesheet_Add(ss, self->label_style);
    }
    if ((self->string_style= stylesheet_Find(ss, "string")) == NULL) {
	self->string_style= style_New();
	style_SetName(self->string_style, "string");
	/*style_AddFlag(self->string_style, style_Underline); removed by popular demand -RSK*/
	stylesheet_Add(ss, self->string_style);
    }
    if ((self->kindStyle[USRDEF]= stylesheet_Find(ss, "userdef")) == NULL) {
	self->kindStyle[USRDEF]= style_New();
	style_SetName(self->kindStyle[USRDEF], "userdef");
	style_AddNewFontFace(self->kindStyle[USRDEF], fontdesc_Bold);
	stylesheet_Add(ss, self->kindStyle[USRDEF]);
    }
    if (stylesheet_Find(ss, "global") == NULL) {
	struct style *global=style_New();
	style_SetName(global, "global");
	style_SetFontFamily(global, "andytype");
	style_AddNewFontFace(global, fontdesc_Fixed);
	style_AddFlag(global, style_ContinueIndent);
	style_AddFlag(global, style_TabsCharacters);
	style_SetNewLeftMargin(global, style_LeftEdge, 16, style_RawDots);
	style_SetNewIndentation(global, style_LeftMargin, -16, style_RawDots);
	style_SetJustification(global, style_LeftJustified);
	stylesheet_Add(ss, global);
    }
    srctext_SetGlobalStyle(self, stylesheet_Find(ss,"global"));
}

/* override this if the language needs additional styles */
void srctext__SetupStyles(self)
struct srctext *self;
{
    SetupStyles(self);
}

/* redundant code, copied from eza.c, to determine checkpointing status */
long CkpInterval; /* How often to run Checkpoint routine. */
#define DEFAULTCKPINTERVAL 30 /* Default for CkpInterval. */
long CkpLatency; /* The minimum amount of time to wait to checkpoint a buffer. */
#define DEFAULTCKPLATENCY 4 /* Default for CkpLatency. */
/**/

boolean srctext__InitializeObject(classID, self)
struct classheader *classID;
struct srctext *self;
{
    srctext_SetCopyAsText(self, TRUE); /* otherwise regions get pasted as INSETS */
    srctext_SetExportEnvironments(self, TRUE); /* keep styles intact on Copy/Cut */
    /*srctext_SetWriteStyle(self, text_NoDataStream); /* save as plain ol' text * /
    srctext_SetObjectInsertionFlag(self, FALSE); /* don't let insets get inserted here * /
    These became obsolete with the addition of SrcInset support. RSK92remove*/
    srctext_SetForceUpper(self,FALSE);
    self->useTabs=TRUE;
    self->reindentComments= environ_GetProfileSwitch("ReindentComments", TRUE);
    self->indentingEnabled= FALSE;
    self->words= NULL;
    self->OverstrikeMode= FALSE; /*RSK91overstrike*/

    /* Initialize Variables used to control the indenting style. */
    self->contIndent=2;
    self->commentIndent= 1;
    self->levelIndent=4;
    self->tabSize= 8;
    self->numTabStops= 0;
    self->maxlinelength=0;
    if ((CkpInterval = environ_GetProfileInt("CheckpointInterval", DEFAULTCKPINTERVAL)) != 0)
	CkpLatency = environ_GetProfileInt("CheckpointMinimum", DEFAULTCKPLATENCY * CkpInterval) / CkpInterval;

    /* Initialize variables for Esc-1 and Esc-2 comments */
    self->commentString= NULL;
    self->commentCol= 0;
    self->commentFixed= FALSE;
    self->linecommentString= NULL;
    self->linecommentCol= 0;
    self->linecommentFixed= FALSE;
    self->remarkPadding= 1;

    /* Initialize callback functions to NULL. */
    self->preWriteFctn= self->postWriteFctn= NULL;

    SetupStyles(self);
    return TRUE;
}

void srctext__FinalizeObject(classID, self)
struct classheader *classID;
struct srctext *self;
{
}

void srctext__HashInsert(classID,hashTable,word)
struct classheader *classID;
Dict *hashTable[];
Dict *word;
{
    Dict *bucket;
    int hashval;
    if (word!=NULL) {
	hashval= HASH(word->stng);
	bucket= (Dict *)malloc(sizeof(Dict));
	bucket->stng= (char *)malloc(strlen(word->stng)+1);
	strcpy(bucket->stng,word->stng);
 	bucket->val= word->val;
	bucket->kind= word->kind;
	bucket->next= hashTable[hashval];
	hashTable[hashval]= bucket;
    }
}

static void PutPrefStringIntoHashTable(hashTable, st,kind)
Dict *hashTable[];
char *st;
int kind;
{
    static char prefword[255];
    static Dict prefdict[]= {{prefword,0,0}};
    char *stcp=prefword;
    if (!st) return;
    prefdict->kind= kind;
    do {
	if (is_whitespace(*st) || *st==',' || *st=='\0') {
	    if (stcp>prefword) { /* we've got a word to insert */
		*stcp= '\0';
		stcp= prefword;
		if (kind==UPRCSE) makeupper(prefword);
		srctext_HashInsert(hashTable, prefdict);
	    }
	}
	else
	    *stcp++= *st;
    } while (*st++);
}

void srctext__BuildTable(classID,classname,hashTable,wordlist)
struct classheader *classID;
char *classname;
Dict *hashTable[];
Dict wordlist[];
{
    int i;
    char profilename[256], preflist[1024];
    Dict *wordPtr;
    char *p;
    
    /* clear out the hash table */
    for (i=0; i<TABLESIZE; ++i)
	hashTable[i]= NULL;

    /* put the language's keyword set (if it HAS one) into hash table */
    wordPtr= wordlist;
    while (wordPtr!=NULL && wordPtr->stng!=NULL) {
	srctext_HashInsert(hashTable, wordPtr);
	++wordPtr;
    }
    /* add user-defined keywords (from "preferences" file) to hash table */
    sprintf(profilename,"%s_userdef\0", classname);
    p=environ_GetProfile(profilename);
    if(p) {
	strcpy(preflist,p);
	PutPrefStringIntoHashTable(hashTable, preflist, USRDEF);
    }
    /* add uppercase-words (from "preferences" file) to hash table */
    sprintf(profilename,"%s_uppercase\0", classname);
    p=environ_GetProfile(profilename);
    if(p) {
	strcpy(preflist,p);
	PutPrefStringIntoHashTable(hashTable, preflist, UPRCSE);
    }
}

/* srctext_Lookup does a case-sensitive hash table lookup */
Dict *srctext__Lookup(classID,hashTable,word) 
struct classheader *classID;
Dict *hashTable[];
char *word;
{
    Dict *bucket;
    static Dict miss={NULL,0,0};
    if (hashTable!=NULL && word!=NULL) {
	bucket= hashTable[HASH(word)];
	while (bucket!=NULL && (strcmp(bucket->stng,word))!=0)
	    bucket= bucket->next;
	if (bucket!=NULL) 
	    return bucket;
    }
    return &miss;
}

/* override */
void srctext__Clear(self)
struct srctext *self;
{
    super_Clear(self); /* This destroyes all styles in the stylesht. */
    srctext_SetupStyles(self);
}

/* addSortedTabStop adds a number to the appropriate location in the array of tab stops */
static void addSortedTabStop(self,tabstop)
struct srctext *self;
int tabstop;
{
    int t=0;
    if (self->numTabStops >= MAX_TABSTOPS) return; /* ignore if too many */
    /* otherwise find the correct position in the array */
    while (t < self->numTabStops && tabstop > self->tabStop[t]) ++t;
    /* move larger tabs out of the way if need be */
    if (t < self->numTabStops) {
	int tm=self->numTabStops;
	while (tm>t) {
	    self->tabStop[tm]= self->tabStop[tm-1];
	    --tm;
	}
    }
    /* add the tab stop to the array and increment the tab stop count */
    self->tabStop[t]= tabstop;
    ++self->numTabStops;
}

/* setTabStops parses string of tab stops given in ezinit */
static void setTabStops(self,st)
struct srctext *self;
char *st;
{
    int tabstop=0;
    self->numTabStops= 0;
    if (!st) return; /* done if no tab stops specified */
    /* otherwise parse string and add tabstops to array */
    while (*st!='\0') {
	sscanf(st,"%d",&tabstop);
	addSortedTabStop(self,tabstop);
	while (isdigit(*st)) ++st; /* skip over already-read tab stop number */
	while (*st!='\0' && !isdigit(*st)) ++st; /* skip blanks, commas, etc, to next tab stop number */
    }
}

/* override */
void srctext__SetAttributes(self,atts)
struct srctext *self;
struct attributes *atts;
{
    super_SetAttributes(self,atts);
    while (atts!=NULL) {
	char *key=atts->key;
	if (strcmp(key,"force-upper")==0)
	    srctext_SetForceUpper(self,atoi(atts->value.string));
	else if (strcmp(key,"use-tabs")==0)
	    self->useTabs=atoi(atts->value.string);
	else if (strcmp(key,"reindent-comments")==0)
	    self->reindentComments=atoi(atts->value.string);
	else if (strcmp(key,"comment-indent")==0)
	    self->commentIndent=atoi(atts->value.string);
	else if (strcmp(key,"cont-indent")==0)
	    self->contIndent=atoi(atts->value.string);
	else if (strcmp(key,"level-indent")==0)
	    self->levelIndent=atoi(atts->value.string);
	else if (strcmp(key,"comment-col")==0) {
	    self->commentCol=atoi(atts->value.string);
	    if (self->commentCol<0) {
		self->commentCol= -self->commentCol -1;
		self->commentFixed= TRUE;
	    }
	    else --self->commentCol; /* convert to left-margin=0 */
	}
	else if (strcmp(key,"linecomment-col")==0) {
	    self->linecommentCol=atoi(atts->value.string);
	    if (self->linecommentCol<0) {
		self->linecommentCol= -self->linecommentCol -1;
		self->linecommentFixed= TRUE;
	    }
	    else --self->linecommentCol; /* convert to left-margin=0 */
	}
	else if (strcmp(key,"remark-padding")==0)
	    self->remarkPadding=atoi(atts->value.string);
	else if (strcmp(key,"enable-indentation")==0)
	    self->indentingEnabled=atoi(atts->value.string);
	else if (strcmp(key,"tab-size")==0)
	    self->tabSize=atoi(atts->value.string);
	else if (strcmp(key,"tab-stops")==0)
	    setTabStops(self,atts->value.string);
	else if (strcmp(key,"max-length")==0)
	    srctext_SetMaxLineLength(self,atoi(atts->value.string));
	else if (strcmp(key,"overstrike")==0) /*RSK91overstrike*/
	    srctext_ChangeOverstrikeMode(self,atoi(atts->value.string));
	atts=atts->next;
    }
}

/* base64[] contains the character forms of all base64 digits, in order. (pray that none of these characters are ever used for comment delimiters in any language!) */
static char base64[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_-";

/* encode64 munches up str so that it's base-64 encoded using the above table. Make sure the string passed here has at least 4/3 its actual length allocated! */
static void encode64(str)
char *str;
{
    int len, globs, temp0=0, temp1=0, temp2=0;
    char *src, *dst;
    len= strlen(str);
    globs= len/3;
    src= str+globs*3;
    dst= str+globs*4;
    /* first, clean up the ragged end and terminate the string that's going to be filled in */
    if (*src) {
	temp0= (*src & 0xFC) >> 2;
	temp1= (*src & 0x03) << 4;
	if (*(src+1)) {
	    temp1+= (*(src+1) & 0xF0) >> 4;
	    temp2= (*(src+1) & 0x0F) << 2;
	}
	*dst= base64[temp0];
	*(dst+1)= base64[temp1];
	*(dst+2)= base64[temp2];
	*(dst+3)= '\0';
    }
    else
	*dst= '\0';
    /* now, traverse the string backward and expand each 3-char glob to 4 base64 chars */
    if (globs>0)
	do  {
	    src-= 3;
	    dst-= 4;
	    *(dst+3)= base64[src[2] & 0x3F];
	    *(dst+2)= base64[((src[2] & 0xC0) >> 6) + ((src[1] & 0x0F) << 2)];
	    *(dst+1)= base64[((src[1] & 0xF0) >> 4) + ((src[0] & 0x03) << 4)];
	    *dst= base64[(src[0] & 0xFC) >> 2];
	} while (src>str);
}

/* base64value returns the actual value of a base64 digit that's in character form */
static int base64value(ch)
char ch;
{
    char *digit=index(base64,ch);
    if (digit)
	return (digit-base64);
    else
	return (-1);
}

/* decode64 turns the encoded str into readable format. This will shrink str's length to 3/4 its original size */
static boolean decode64(str)
char *str;
{
    char *result=(char *)malloc((strlen(str)*4)/3 +1);
    char *src=str, *dst=result;
    int tmp;
    while (*src) {
	tmp= base64value(*src++);
	if (tmp<0) { free(result); return FALSE; }
	*dst= tmp << 2;
	if (!*src) goto EndString;
	tmp= base64value(*src++);
	if (tmp<0) { free(result); return FALSE; }
	*dst+= (tmp & 0x30) >> 4;
	*++dst= (tmp & 0x0F) << 4;
	if (!*src) goto EndString;
	tmp= base64value(*src++);
	if (tmp<0) { free(result); return FALSE; }
	*dst+= (tmp & 0x3C) >> 2;
	*++dst= (tmp & 0x03) << 6;
	if (!*src) goto EndString;
	tmp= base64value(*src++);
	if (tmp<0) { free(result); return FALSE; }
	*dst+= tmp;
	EndString: ; /* jump here if source string ends abruptly */
	*++dst= '\0';
    }
    strcpy(str, result);
    free(result);
    return TRUE;
}

/* SrcInset Rule:  SrcInsets should NOT have any newlines in them.  If there ARE any newlines, it means the SrcInset was tampered with by the user, so we poke those newline characters into the file right after the inset.  The *exception* to this rule is when the "srctext.BreakInsetsWithNewlinesLineNumbersBeDamned" preference is set to TRUE.  In this case, we added those newlines OURSELF, and we don't want to poke them back into the file because they'll be written back out again as part of the SrcInset when we _Write the file.  Obviously, the buffer with the translated inset is going to have LESS newline characters than the original file with the broken SrcInset, which makes the compiler's line numbers NOT match the buffer's. Yech. */

/* DiscardToEnddata is swiped directly from atk/text/text.c. It's used by InsertSrcInsetFile */
/* Assuming a \begindata has been read, discards up to and including a matching \enddata (discards internal levels of \begindata ... \enddata).  Returns FALSE if EOF is reached before the \enddata.  Something better needs to be done about this. */
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

/* InsertAFile is the guts of text_AlwaysInsertFile, but it will *unconditionally* create an INSET if MUSTinset is TRUE. */
static long InsertAFile(self, file, position, objectName, objectID, MUSTinset)
struct srctext *self;
FILE *file;
long position;
char *objectName;
long objectID;
boolean MUSTinset;
{
    int length = 0;
    if (MUSTinset || (objectName!=NULL && objectID!=0 && !class_IsTypeByName(objectName, "text")))  {
	/* Either this is a NON-text datastream (we have to make an inset), or we WANT it to be an inset regardless or its type */
	struct dataobject *dat;
	if(srctext_GetObjectInsertionFlag(self) == FALSE){
	    /* ignore non-text object */
	    char bb[512];
	    long ll;
	    fprintf(stderr, "Insertion of objects not allowed, ignoring %s!\n",objectName);
	    DiscardToEnddata(file);
	    sprintf(bb,"[A %s OBJECT WAS INSERTED HERE]",objectName);
	    ll = strlen(bb);
	    srctext_AlwaysInsertCharacters(self, position, bb, ll);
	    length = ll;
	} else {
	    dat = (struct dataobject *) class_NewObject(objectName);
	    if (dat == NULL) {
		fprintf(stderr, "Srctext: Can't find routines for object '%s'; ignoring!\n", objectName);
		DiscardToEnddata(file);
		length = 0;
	    } else {
		dataobject_Read(dat, file, dataobject_UniqueID(dat));
		dictionary_Insert(NULL, (char *) objectID, (char *) dataobject_UniqueID(dat));
		srctext_AlwaysAddView(self, position, dataobject_ViewName(dat),    dat);
		length = 1;
	    }
	}
    } else {
	boolean wasReadOnly;
	long oldfence = srctext_GetFence(self);
	wasReadOnly = srctext_GetReadOnly(self);
	/* ReadSubString checks read-only, making this ugliness necessary. */
	if(wasReadOnly){
	    srctext_SetReadOnly(self, FALSE);
	    length = srctext_ReadSubString(self, position, file, objectID > 0);
	    srctext_SetReadOnly(self, wasReadOnly);
	} else if( position < oldfence){
	    /* reset the fence properly */
	    srctext_SetFence(self,0);
	    length = srctext_ReadSubString(self, position, file, objectID > 0);
	    srctext_SetFence(self,oldfence + length);
	} else 
	    length = srctext_ReadSubString(self, position, file, objectID > 0);
    }
    return length;
}

/* InsertSrcInsetFile is a wrapper for InsertAFile, that looks up the object's info and tells InsertAFile we MUST have an inset. */
static long InsertSrcInsetFile(self, file, filename, position)
struct srctext *self;
FILE *file;
char *filename; /* not used */
long position;
{
    char *objectName;
    long objectID;
    objectName= filetype_Lookup(file, filename, &objectID, NULL);
    return InsertAFile(self, file, position, objectName, objectID, TRUE /* we absolutely positively MUST have an inset */);
}

/* override */
/* srctext_AlwaysInsertFile should be less cautious than text's, and only make the file be an inset if it MUST.  This gets called by Paste, too. */
long srctext__AlwaysInsertFile(self, file, filename, position)
struct srctext *self;
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
            ++myfile;
        else
            return 0;
    }
    objectName = filetype_Lookup(file, filename, &objectID, NULL);
    length= InsertAFile(self, file, position, objectName, objectID, FALSE /* we probably DON'T want an inset */);
    dogtags_substituteregion(self, &position,&length);

    if (myfile)
        fclose(file);
    return length;
}

/* srctext_TranslateSrcInset turns a SrcInset into a REAL inset, and returns the length of what got inserted minus the length of the SrcInset that got replaced */
long srctext__TranslateSrcInset(self,pos)
struct srctext *self;
long pos;
{
    long startinset=pos++, len=srctext_GetLength(self);
    int c, remembernewlines=0;
    char linebuffer[81];
    char *filename=NewString("/tmp/SrcInsetR.XXXXXX");
    FILE *decodedfile;
    long difflength=0;

    if(filename==NULL) return 0;
    /* open decoded file */
    mktemp(filename); /* randomize the filename */
    decodedfile= fopen(filename, "w+");
    do  {
	long chunklen;
	/* skip over chunk separator (spaces), and count doomed newlines */
	while (pos<len && (is_whitespace(c=srctext_GetChar(self,pos)) || c=='\n')) {
	    ++pos;
	    if (c=='\n') ++remembernewlines;
	}
	for (chunklen=0; chunklen<80 && !is_whitespace(c=srctext_GetChar(self,pos)) && c!='\n' && c!=EOF; ++pos)
	    linebuffer[chunklen++]= c;
	if (chunklen<1) break;
	linebuffer[chunklen]= '\0';
	if (linebuffer[0]!=SRCINSET_ENDCHAR) {
	    decode64(linebuffer);
	    fputs(linebuffer, decodedfile);
	}
    } while (pos<len && linebuffer[0]!=SRCINSET_ENDCHAR);
    if (linebuffer[0]!=SRCINSET_ENDCHAR) {
	/* bogus inset, not terminated */
	unlink(filename);
	fclose(decodedfile);
	free(filename);
	return 0;
    }
    ++pos; /* skip over extra space in source code */

    /* insert newlines to compensate for the ones we're about to destroy */
    if (!environ_GetProfileSwitch("srctext.BreakInsetsWithNewlinesLineNumbersBeDamned", FALSE)) {
	difflength= remembernewlines;
	while (remembernewlines-- > 0)
	    srctext_AlwaysInsertCharacters(self, pos, "\n", 1);
    }

    /* insert decoded file into source */
    rewind(decodedfile);
    difflength+= InsertSrcInsetFile(self, decodedfile, filename, pos);
    unlink(filename); /* delete temporary decoded file */
    free(filename);
    fclose(decodedfile);
    /* remove SrcInset data */
    difflength-= pos-startinset;
    srctext_AlwaysDeleteCharacters(self, startinset, pos-startinset);

    return difflength;
}

/* srctext_FindSrcInsets is called after a _Read is complete, but BEFORE the _RedoStyles happens. It hunts down SrcInset delimiters and calls _TranslateSrcInset to turn them into REAL insets. */
void srctext__FindSrcInsets(self)
struct srctext *self;
{
    long pos=0, len=srctext_GetLength(self);
    while (pos<len) {
	pos= srctext_Index(self, pos, SRCINSET_STARTCHAR, len-pos);
	if (pos==EOF)
	    pos= len;
	else {
	    long difflen;
	    difflen= srctext_TranslateSrcInset(self,pos);
	    if (!difflen) ++pos; /* just in case the inset was bogus */
	    len+= difflen;
	}
    }
}

/* override */
long srctext__Read(self, file, id)
struct srctext *self;
FILE *file;
long id;
{
    long tmpRetValue;
    tmpRetValue= super_Read(self, file, id);
    if (tmpRetValue == dataobject_NOREADERROR) {
	if (id==0) /* it better NOT be a datastream! */
	    srctext_FindSrcInsets(self);
	srctext_SetupStyles(self); /* text_Read blows the stylesheet away, so we have to either find (or CREATE) all the styles AGAIN */
	dogtags_substitute(self);
	if (environ_GetProfileSwitch("InitialRedoStyles", TRUE))
	    srctext_RedoStyles(self);
    }
    return tmpRetValue;
}

/* override */
long srctext__ReadTemplate(self, templateName, inserttemplatetext)
struct srctext *self;
char *templateName;
boolean inserttemplatetext;
{
    long retval;
    retval= super_ReadTemplate(self,templateName,inserttemplatetext);
    if (retval >= 0)
	srctext_SetupStyles(self);
    return retval;
}

/* LinesTooLong goes through the entire file to see if there are any lines exceeding maxlen. Returns TRUE if there are. */
boolean srctext__LinesTooLong(self,maxlen)
struct srctext *self;
int maxlen;
{
    long pos=0, len=srctext_GetLength(self);
    int c, linelen=0;
    while (pos<len) {
	c= srctext_GetChar(self,pos++);
	if (c=='\n') {
	    if (linelen>maxlen)
		return TRUE;
	    linelen= 0;
	}
	else if (c=='\t')
	    linelen=(linelen+8)&~7;
	else
	    ++linelen;
    }
    return FALSE;
}

/* CheckLineLengths displays a message in view if any lines are found that exceed maxlen. (if view is NULL, a guess is made about where to display the message) */
void srctext__CheckLineLengths(self,maxlen,view)
struct srctext *self;
int maxlen;
struct view *view;
{
    if (maxlen<1)
	/* nothing to check-- forget it */
	return;
    if (srctext_LinesTooLong(self,maxlen)) {
	char msg[512], *filename, *pathname;
	if (!view)
	    /* guess which window requested the length check */
	    view= (struct view *)(im_GetLastUsed());
	pathname= buffer_GetFilename(buffer_FindBufferByData(self));
	filename= rindex(pathname,'/');
	if (!filename)
	    filename= pathname; /* just in case pathname has no slashes */
	else ++filename; /* skip over that slash */
	sprintf(msg,"Warning: saving `%s' with lines exceeding %d characters.", filename, srctext_GetMaxLineLength(self));
	message_DisplayString(view, 65 /*probably a dialog box*/, msg);
    }
}

/* simplewrite() is stolen from smpltext.c */
static boolean simplewrite(file,p,len)
FILE *file;
char *p;
long len;
{
    /* Write out the literal contents of the character buffer.  write() is used because it is more efficient than fwrite.  if write() fails, attempt with fwrite (), if it succeeds then continue, if it fails, then when buffer calls ferror, it will notice that something has gone wrong */
    long wroteLen;
    while (len > 0)  {
	wroteLen = write(fileno(file), p, len);
	if (wroteLen == -1) {
	    if (errno == EINTR)  continue;
	    if((wroteLen = fwrite(p,1,len,file)) <= 0)	{
		fprintf(stderr, "Error while writing text object.\n");
		return FALSE;
	    }
	    fflush(file);
	}
	len -= wroteLen;
	p = &p[wroteLen];
    }
    return TRUE;
}

/* writesrc does the work for _Write, and can also be called for the stuff inside compress boxes */
static boolean writesrc(self, file, writeID)
struct srctext *self;
FILE *file;
long writeID;
{
    boolean success=TRUE;
    long pos=0, len=srctext_GetLength(self);
    /* check line lengths if a max-length was set (unless checkpointing) */
    if (srctext_GetMaxLineLength(self)) {
	struct buffer *buff= buffer_FindBufferByData(self);
	if (buffer_GetCkpClock(buff)<=CkpLatency)
	    srctext_CheckLineLengths(self,srctext_GetMaxLineLength(self),NULL);
    }
    /* find ATK insets and turn them into SrcInsets, which have a format that won't make compilers explode (hopefully). */
    do  {
	long nextinset, actuallen;
	nextinset= nextInset(self, pos,len-pos);
	nextinset= (nextinset>=0) ? nextinset : len;
	/* flush out all the plain text up through 'nextinset' */
	do  {
	    char *plainbuffer;
	    plainbuffer= srctext_GetBuf(self, pos, nextinset-pos, &actuallen);
	    simplewrite(file, plainbuffer, actuallen);
	    pos+= actuallen;
	} while (pos<nextinset);
	pos= nextinset;
	if (nextinset<len) {
	    /* write out the inset, encode64 it, and shove it into the source file */
	    struct environment *env=environment_GetInnerMost(self->header.text.rootEnvironment, nextinset);
	    if (env->type == environment_View) { /* make extra sure it's a view */
		struct dataobject *inset= env->data.viewref->dataObject;
		success= success && srctext_OutputSrcInset(self,file,writeID, inset);
	    }
	    ++pos; /* skip over the \377 */
	}
    } while (pos<len && success);
    return success;
}

/* srctext_OutputSrcInset converts the datastream of the passed inset into base64-encoded chunks, and outputs those chunks to file. Retruns TRUE on success. */
boolean srctext__OutputSrcInset(self, file, writeID, inset)
struct srctext *self;
FILE *file;
long writeID;
struct dataobject *inset;
{
    boolean success=TRUE;
    FILE *tempfile;
    char paddedbuffer[84], *linebuffer;
    char *filename=NewString("/tmp/SrcInsetW.XXXXXX");
    if(filename==NULL) return FALSE;
    /* check for special cases (compress insets) */
    if (class_IsTypeByName(class_GetTypeName(inset), "compress")) {
	struct srctext *compresscontents=srctext_New();
	srctext_AlwaysCopyText(compresscontents,0, inset,0,compress_GetLength((struct compress *)inset));
	success= writesrc(compresscontents,file, writeID);
	srctext_Destroy(compresscontents);
	return success;
    }
    /* set up padding to separate the encoded chunks */
    paddedbuffer[0]= ' ';
    if (srctext_GetMaxLineLength(self) && environ_GetProfileSwitch("srctext.BreakInsetsWithNewlinesLineNumbersBeDamned", FALSE))
	paddedbuffer[0]= '\n';
    linebuffer= paddedbuffer+1; /* don't mess with the separator char */

    /* open temp file and write inset out in normal format */
    mktemp(filename);
    tempfile= fopen(filename, "w+");
    if (!tempfile) {
	free(filename);
	return FALSE;
    }
    unlink(filename); /* this file is REALLY temporary */
    free(filename);
    dataobject_Write(inset, tempfile, writeID, 2 /* level 2, an inset */);
    /* read the temp file back in, encode64 it, and send result to source file */
    rewind(tempfile);
    sprintf(linebuffer, "%c", SRCINSET_STARTCHAR);
    success= success && simplewrite(file, linebuffer, 1);
    while (!feof(tempfile)) {
	int count=0;
	while (!feof(tempfile) && count<60)
	    linebuffer[count++]= fgetc(tempfile);
	linebuffer[count]= '\0';
	encode64(linebuffer);
	success= success && simplewrite(file, paddedbuffer, strlen(paddedbuffer));
    }
    sprintf(linebuffer, " %c ", SRCINSET_ENDCHAR);
    success= success && simplewrite(file, linebuffer, 3);
    fclose(tempfile);
    return success;
}

/* override */
long srctext__Write(self, file, writeID, level)
struct srctext *self;
FILE *file;
long writeID;
int level;
{
    long len=srctext_GetLength(self), retval;

    /* terminate file with newline if not already (and it's not an inset) */
    if (level==0 && len>0 && srctext_GetChar(self, len-1) != '\n')
        srctext_InsertCharacters(self, len++, "\n", 1);

    if (self->preWriteFctn)
	(*(self->preWriteFctn))(self,file,writeID,level);

    /* see if we should format it or let the text object handle things */
    if (level==0 && srctext_GetWriteStyle(self)!=text_DataStream) {
	if (!writesrc(self, file, writeID))
	    fprintf(stderr, "srctext error: File could not be written properly. (Saving insets requires free space in /tmp.  If this file has insets, clean out /tmp and try again).\n");
	retval= self->header.dataobject.id;
    }
    else /* this thing is an inset (or else user set "datastream=yes"), just belch it out as a datastream */
	retval= super_Write(self, file, writeID, level);

    if (self->postWriteFctn)
	(*(self->postWriteFctn))(self,file,writeID,level);

    return retval;
}

/* SetWriteCallbacks can be used to set up a "remove nasties" and "restore nasties" fuction, useful for things like EZ-integrated debuggers that insert special breakpoint characters and such into the actual dataobject, but DON'T want them written to the file. */
void srctext__SetWriteCallbacks(self, pre, post)
struct srctext *self;
procedure *pre, *post;
{
    self->preWriteFctn= pre;
    self->postWriteFctn= post;
}

/* override */
/* srctext_GetModified checks for modified insets in a sneaky way. The method inherited from text would be ridiculously slow because it checks ALL *environments*, and there are a LOT of them in a huge source file. Instead, this hunts down TEXT_VIEWREFCHAR's only, thus ignoring *style* environments. */
/* the base ATK version was fixed somewhere around version 5.1.something.  When that's been installed, this override can Go Away */
long srctext__GetModified(self) /*RSK92add*/
struct srctext *self;
{
    long pos=0, len=srctext_GetLength(self);
    long mod, maxmod=self->header.dataobject.modified;
    while ((pos=nextInset(self, pos,len-pos)) >= 0 && pos<len) {
	struct environment *env=environment_GetInnerMost(self->header.text.rootEnvironment, pos);
	if (env->type == environment_View) {
	    struct dataobject *inset= env->data.viewref->dataObject;
	    mod= dataobject_GetModified(inset);
	    if (mod > maxmod)
		maxmod= mod;
	}
	++pos;
    }
    return maxmod;
}

/* static data cached by GetLineForPos and GetPosForLine */
static long prev_line=1, prev_mod=-1;
static struct srctext *prev_self=NULL;
static struct mark *prev_pos=NULL;

/* COUNT_LINES is a pseudofunction that takes a srctext object, position, and the name of a variable to modify.  It counts the number of lines represented at position p (1 if there's a newline there, ??? if it's a compress inset, otherwise lin is unchanged). MAKE SURE PARMS HAVE NO SIDE EFFECTS! */
#define COUNT_LINES(st,p,lin) \
    { \
        int ch=srctext_GetChar((st),(p)); \
	if (ch=='\012') \
	    ++lin; \
	else if (ch==TEXT_VIEWREFCHAR) { \
	    struct environment *env=environment_GetInnerMost((st)->header.text.rootEnvironment, (p)); \
	    if (env->type == environment_View) { \
		struct dataobject *inset= env->data.viewref->dataObject; \
		if (class_IsTypeByName(class_GetTypeName(inset), "compress")) \
		    lin+= compress_GetLines((struct compress *)inset)-1; /* -1 to compensate for \n after box being counted */ \
	    } \
	} \
    }

/* override */
/* GetPosForLine "tricks" everyone who calls it by ALSO counting newlines inside compress objects */
long srctext__GetPosForLine(self, line)
struct srctext *self;
long line;
{
    register long base_line=1, pos=0;
    long len=srctext_GetLength(self), mod=self->header.dataobject.modified;

    if (self==prev_self && mod==prev_mod && prev_pos && !mark_ObjectFree(prev_pos)) {
	if (line>=prev_line) {
	    /* save a lot of counting by starting at the cache */
	    base_line= prev_line;
	    pos= mark_GetPos(prev_pos);
	    if (line==prev_line)
		pos= srctext_GetBeginningOfLine(self,pos);
	} else if (line>(prev_line/2)) {
	    /* save a lot of counting by going backward from the cache */
	    base_line= line - prev_line;
	    pos= mark_GetPos(prev_pos);
	    while (base_line <= 0) {
		if (pos<0) {
		    pos= -2;
		    break;
		}
		--pos;
		COUNT_LINES(self,pos,base_line);
	    }
	    ++pos;
	    base_line= line;
	    goto GotPosForLine; /* skip normal forward count */
	}
    }
    while (base_line < line) {
	if (pos >= len)
	    break;
	COUNT_LINES(self,pos,base_line);
	++pos;
    }
    GotPosForLine: ; /* jump here if we did a backward count, to skip the normal forward count */
    line= base_line; /* just in case the requested line was purely fictitious-- make sure we cache something real */
    if (self==prev_self) {
	/* we're interested in somewhere else now, so "cache" the new PosForLine */
	prev_line= line;
	prev_mod= mod;
	if (prev_pos==NULL)
	    prev_pos= srctext_CreateMark(self, pos,0);
	else
	    mark_SetPos(prev_pos, pos);
    } else {
	/* we're interested in a whole different dataobject, so make a new "cache" */
	if (prev_pos) {
	    if (!mark_ObjectFree(prev_pos))
		srctext_RemoveMark(prev_self,prev_pos);
	    mark_Destroy(prev_pos);
	}
	prev_pos= srctext_CreateMark(self, pos,0);
	prev_line= line;
	prev_mod= mod;
	prev_self= self;
    }
    return pos;
}

/* override */
/* GetLineForPos "tricks" everyone who calls it by ALSO counting newlines inside compress objects */
long srctext__GetLineForPos(self, pos)
struct srctext *self;
long pos;
{
    register long base_pos=0, line=1;
    long mod=self->header.dataobject.modified;
    if (self==prev_self && mod==prev_mod && prev_pos && !mark_ObjectFree(prev_pos)) {
	long prvpos=mark_GetPos(prev_pos);
	if (pos>=prvpos) {
	    /* save a lot of counting by starting at the cache */
	    base_pos= prvpos;
	    line= prev_line;
	} else if (pos>(prvpos/2)) {
	    /* save a lot of counting by going backward from the cache */
	    register long neglines=0;
	    base_pos= prvpos;
	    while (--base_pos >= pos)
		COUNT_LINES(self,base_pos,neglines);
	    line= prev_line - neglines;
	    goto GotLineForPos; /* skip normal forward count */
	}
    }
    while (base_pos < pos) {
	COUNT_LINES(self,base_pos,line);
	++base_pos;
    }
    GotLineForPos: ; /* jump here if we did a backward count, to skip the normal forward count */
    if (self==prev_self) {
	/* we're interested in somewhere else now, so "cache" the new LineForPos */
	prev_line= line;
	prev_mod= mod;
	if (prev_pos==NULL)
	    prev_pos= srctext_CreateMark(self, pos,0);
	else
	    mark_SetPos(prev_pos, pos);
    } else {
	/* we're interested in a whole different dataobject, so make a new "cache" */
	if (prev_pos) {
	    if (!mark_ObjectFree(prev_pos))
		srctext_RemoveMark(prev_self,prev_pos);
	    mark_Destroy(prev_pos);
	}
	prev_pos= srctext_CreateMark(self, pos,0);
	prev_line= line;
	prev_mod= mod;
	prev_self= self;
    }
    return line;
}

/* override this if the language needs to check for procedure names and such */
void srctext__BackwardCheckWord(self,from,to)
struct srctext *self;
long from,to;
{
    Dict *word;
    char buff[1024];
    long j=from;
    if (srctext_IsTokenChar(self, srctext_GetChar(self, from)) && !srctext_GetStyle(self, from) && !srctext_InString(self, from)) {
	j= srctext_BackwardCopyWord(self,from,to,buff);
	srctext_Keywordify(self,buff,TRUE);
	if ((word=srctext_Lookup(self->words,buff))->stng!=NULL) {
	    int l= strlen(buff);
	    if (srctext_GetForceUpper(self))
		srctext_ReplaceCharacters(self, j+1,l, word->stng,l);
	    srctext_WrapStyleNow(self,j+1,l, self->kindStyle[word->kind], FALSE,FALSE);
	}
	else if (srctext_GetForceUpper(self)) {
	    makeupper(buff); /* just in case Keywordify didn't do this (ctext) */
	    if ((word=srctext_Lookup(self->words,buff))->stng!=NULL) {
		int l= strlen(buff);
		if (word->kind==UPRCSE)
		    srctext_ReplaceCharacters(self, j+1,l, word->stng,l);
	    }
	}
    }
    return /*j*/;
}

/* CheckComment will wrap the comment style around slash-star comments. (it's not used HERE, but is inherited by C, PL/x, and some other languages, and overridden by others.)  'start' is the position of slash (not asterisk) that starts the comment. Returns position of ending slash. */
long srctext__CheckComment(self, start)
struct srctext *self;
long start;
{
    int prev, c=0;
    long end=start+1, len=srctext_GetLength(self);

    while (++end<len) {
	prev= c;
	c= srctext_GetChar(self, end);
	if (c=='/' && prev=='*')
	    break;
    }
    srctext_WrapStyle(self, start,end-start+1, self->comment_style, FALSE,FALSE);
    return end;
}

/* CheckLinecomment will wrap the linecomment style to the end of the line. (it's not used HERE, but is universally useful for any languages with bang comments.) */
long srctext__CheckLinecomment(self, start)
struct srctext *self;
long start;
{
    long end=srctext_GetEndOfLine(self,start);
    srctext_WrapStyle(self, start,end-start, self->linecomment_style, FALSE,TRUE);
    return end-1;
}

/* CheckString will wrap the string style around strings and character constants. No need to override this, since only RedoStyles will call it, and it already knows when it's appropriate or not. */
long srctext__CheckString(self, start)
struct srctext *self;
long start;
{
    long end=start, len=srctext_GetLength(self);
    int delim=srctext_GetChar(self,start);
    while (end<len)
	if (srctext_GetChar(self,++end)==delim && !srctext_Quoted(self,end))
	    break;
    srctext_WrapStyle(self, start,end-start+1, self->string_style, FALSE,FALSE);
    return end;
}

/* override this if the language needs to check for procedure names and such */
long srctext__CheckWord(self,i,end)
struct srctext *self;
long i,end;
{
    long j;
    Dict *word;
    char buff[1024];
    j=srctext_CopyWord(self,i,end,buff);
    srctext_Keywordify(self,buff,FALSE);
    if ((word=srctext_Lookup(self->words,buff))->stng!=NULL)
	srctext_WrapStyle(self,i,strlen(word->stng), self->kindStyle[word->kind], FALSE,FALSE);
    return j;
}

/* BackwardCheckLabel styles a label if it's the first thing on a line. 'pos' is the position of the character after the label (usually a colon). This is called by both RedoStyles (data object), and StyleLabel (view object) */
void srctext__BackwardCheckLabel(self, pos)
struct srctext *self;
long pos;
{
    char buff[256];
    long startlabel;
    while (is_whitespace(srctext_GetChar(self, --pos))) ;
    startlabel= srctext_BackwardCopyWord(self,pos,0,buff);
    srctext_Keywordify(self,buff,FALSE);
    if (startlabel<pos && srctext_Lookup(self->words, buff)->stng == NULL) {
	/* the word before the ':' is not a keyword */
	long startline=startlabel;
	while (is_whitespace(srctext_GetChar(self, startline))) --startline;
	if (startline<0 || srctext_GetChar(self, startline)=='\n')
	    /* and it's the first thing on line */
	    srctext_WrapStyleNow(self, startlabel+1,pos-startlabel, self->label_style, TRUE,TRUE);
    }
}

/* override this if there's a different way to identify comments in that language */
/* InCommentStart returns the starting position of the comment, if pos is inside one, otherwise 0 is returned */
long srctext__InCommentStart(self, pos)
struct srctext *self;
long pos;
{
    struct style *sty= srctext_GetStyle(self,pos);
    if (sty==self->linecomment_style || sty==self->comment_style) {
	long start= environment_Eval(srctext_GetEnvironment(self, pos));
	if (start<1)
	    start= 1; /* make sure it's "TRUE" even if start position is zero. */
	else if (srctext_GetChar(self,start-1) == TEXT_VIEWREFCHAR) {
	    /* oops, an inset. jump it and recurse to keep looking */
	    long continued=srctext_InCommentStart(self, start-1);
	    if (continued) return continued;
	}
	return start;
    }
    else
	return 0;
}

/* override this for any source views that have indentation schemes */
/* Indentation returns the proper indentation of the line that starts at pos */
int srctext__Indentation(self,pos)
struct srctext *self;
long pos;
{
    /* leave it where it is; there's no "generic" indentation scheme */
    return srctext_CurrentIndent(self,pos);
}

/* override this if non-alphanumeric characters are tokens in that language */
boolean srctext__IsTokenChar(self,ch)
struct srctext *self;
char ch;
{
    return isalnum(ch);
}

/* override this if the language is more or less picky about what's recognized */
/* This function "keywordifies" a string. "Keywordify" means "make the word findable in the hash table". */
char *srctext__Keywordify(self, buff, checkforceupper)
struct srctext *self;
char *buff;
boolean checkforceupper;
{
    return buff;
}

/* srctext_RemoveStyles gets called by all RedoStyles methods to first remove the old styles */
void srctext__RemoveStyles(self)
struct srctext *self;
{
    struct environment *root=self->header.text.rootEnvironment;
    /* JUST remove the STYLES, don't go blowing away embedded views! */
    environment_Remove(root,0,srctext_GetLength(self),environment_Style,TRUE);
    return;
}

/* override this if the language has comments or keywords */
void srctext__RedoStyles(self)
struct srctext *self;
{
    /* first, remove the old styles */
    srctext_RemoveStyles(self);
    /* then, put the new styles in (if there WERE any-- srctext is non-language-specific) */
    return;
}

long srctext__SkipWhitespace(self,pos,end)
struct srctext *self;
long pos,end;
{
    while (pos<end && is_whitespace(srctext_GetChar(self,pos))) ++pos;
    return pos;
}

/* assumes that pos points to 1 BEFORE ending delimiter */
long srctext__BackwardSkipString(self,pos,delim)
struct srctext *self;
long pos;
char delim;
{
    while (pos>=0 && (srctext_GetChar(self,pos)!=delim || srctext_Quoted(self,pos)))
	--pos;
    return pos-1;
}

boolean srctext__InString(self,pos)
struct srctext *self;
long pos;
{
    int c, lastquote=0;
    long commentskip;
    boolean quotes=FALSE;
    --pos;
    while (pos>=0 && ((c=srctext_GetChar(self,pos))!='\n' || srctext_Quoted(self,pos))) {
	if ((c=='\'' || c=='\"') && !srctext_Quoted(self,pos)) {
	    if (c==lastquote) {
		quotes=FALSE;
		lastquote=0;
	    } else if (!quotes) {
		quotes=TRUE;
		lastquote=c;
	    }
	} else if ((commentskip=srctext_InCommentStart(self,pos)) > 0) {
	    /* skip over comments; quotes inside them are irrelevant */
	    while (pos>commentskip && srctext_GetChar(self,pos-1)!='\n')
		--pos;
	}
	--pos;
    }
    return quotes;
}

/* srctext_CopyWord copies a word from 'pos' to 'end' into 'buffer'. It returns the position of the last character in the word. */
long srctext__CopyWord(self,pos,end,buffer)
struct srctext *self;
long pos,end;
char buffer[];
{
    int ch, count=0;
    while (count<256 && pos<end && srctext_IsTokenChar(self, ch=srctext_GetChar(self,pos))) {
	buffer[count++]=ch;
	++pos;
    }
    buffer[count]=0;
    return pos-1;
}

long srctext__BackwardCopyWord(self,from,to,buffer)
struct srctext *self;
long from,to;
char buffer[];
{
    long count=0,i=0,j;
    buffer[0]=0;
    while (from-i>=to && srctext_IsTokenChar(self, srctext_GetChar(self,from-i))) ++i;
    j=from-i;
    if (from-i>=to-1)
	for (; i>0 && count<255; --i,++count)
	    buffer[count]=srctext_GetChar(self,from-i+1);
    buffer[count]=0;
    return j;
}

/* srctext_CurrentIndent returns the indentation of the current line */
int srctext__CurrentIndent(self,pos)
struct srctext *self;
long pos;
{
    register int ind=0;

    pos= srctext_GetBeginningOfLine(self,pos);
    while (1)
	switch (srctext_GetChar(self,pos++)) {
	    case '\t':
		ind=(ind+8)&~7;
		break;
	    case ' ':
		++ind;
		break;
	    default:
		return ind;
	}
}

/* Return the current "column" (chars from left margin), compensating for tab characters */
int srctext__CurrentColumn(self,pos)
struct srctext *self;
long pos;
{
    int ind=0;
    long oldPos=pos; /* save the current position */

    /* backup to the beginning of the line */
    pos= srctext_GetBeginningOfLine(self,pos);

    /* count the number of columns to the current position */
    while(pos<oldPos)
	if(srctext_GetChar(self,pos++)=='\t')
	    ind=(ind+8)&~7;
	else
	    ++ind;

    return ind;
}

/* srctext_NextTabStop returns the column that the next Tab Stop (or multiple of tabSize, whichever comes first) from curcol. A zero is returned if we have to move to the next line */
int srctext__NextTabStop(self,curcol)
struct srctext *self;
int curcol;
{
    int nextMultiple=((int)(curcol/self->tabSize)+1) * self->tabSize;
    int tabnum=0;
    if (self->numTabStops<1)
	/* there ARE no tab stops */
	if (self->tabSize>0)
	    return nextMultiple;
	else
	    /* tabSize was *disabled*, so just move 1 space */
	    return curcol+1;
    /* find next tab stop */
    while (tabnum<self->numTabStops && self->tabStop[tabnum]<=curcol)
	++tabnum;
    if (tabnum>=self->numTabStops)
	/* reached the end of the tab stops */
	if (self->tabSize>0)
	    return nextMultiple;
	else
	    /* tabSize was *disabled*, so jump to next line */
	    return 0;
    /* return next multiple of tabSize, or next tab stop, whichever is closest */
    if (self->tabStop[tabnum]<=nextMultiple || self->tabSize<1)
	return self->tabStop[tabnum];
    else
	return nextMultiple;
}

/* ExtendToOutdent modifies the pos and len parameters to appropriately point to the full indentation level that pos is inside of.  It returns TRUE if it thinks the region has a "significant" size (3 lines or more). */
boolean srctext__ExtendToOutdent(self,indent, pos,len)
struct srctext *self;
int indent;
long *pos, *len;
{
    register long start=*pos, end=*pos, temp=srctext_GetBeginningOfLine(self, start);
    long lines=0, txtlen=srctext_GetLength(self);
    /* find beginning */
    do  {
	start= temp;
	temp= srctext_GetBeginningOfLine(self, temp-1);
	++lines;
    } while (temp>0 && (srctext_CurrentIndent(self,temp)>=indent || srctext_GetChar(self, srctext_SkipWhitespace(self, temp,txtlen))=='\n'));
    *pos= srctext_SkipWhitespace(self, start, txtlen);
    /* find end */
    end= srctext_GetEndOfLine(self, end);
    while (end<txtlen && (srctext_CurrentIndent(self,end+1)>=indent || srctext_GetChar(self, srctext_SkipWhitespace(self, end+1,txtlen))=='\n')) {
	end= srctext_GetEndOfLine(self, end+1);
	++lines;
    }
    *len= end - *pos +1; /* compress will strip off the last newline, but grab it anyway in case the last line was totally blank */
    return (lines>2);
}

/* returns true iff the character at pos is quoted (ie. "\"). Override this is you want to take into account the slash being quoted. (ie "\\", for ctext and idltext). */
boolean srctext__Quoted(self, pos)
struct srctext *self;
long pos;
{
    return FALSE;
}

/* Paren Balancer data structures */
struct paren_node {
    long type;
    struct paren_node *next;
};
#define new(type) (type *) malloc(sizeof(type))

static    char *opens="({[";
static    char *closes=")}]";
long srctext__ReverseBalance(self, pos)
struct srctext *self;
long pos;
{
    boolean found=FALSE, atleastone=FALSE;
    int thischar;
    char *parentype;
    struct paren_node *parenstack=NULL;

    while ((parenstack!=NULL || !atleastone) && (pos>0)) {
	thischar= srctext_GetChar(self, --pos);

	if ((parentype=index(opens,thischar)) != NULL) {
	    if (srctext_InCommentStart(self,pos) || srctext_InString(self,pos))
		/* we're in a comment or string. Ignore character! */
		continue;
	    if (parenstack==NULL || parenstack->type != (parentype-opens))
		break;
	    else {
		struct paren_node *temp=parenstack;

		parenstack= parenstack->next;
		free(temp);
		if (pos==0 && parenstack!=NULL)
		    break;
		else if (parenstack==NULL) {
		    found= TRUE;
		    break;
		}
	    }
	}
	else if ((parentype=index(closes, thischar)) != NULL) {
	    struct paren_node *temp=new(struct paren_node);

	    if (srctext_InCommentStart(self,pos) || srctext_InString(self,pos))
		/* we're in a comment or string. Ignore character! */
		continue;
	    temp->type= parentype - closes;
	    temp->next= parenstack;
	    parenstack= temp;
	}
    }
    if (found)
	return pos;
    else
	return EOF;
}

boolean srctext__DoMatch(self,pos,str,len)
struct srctext *self;
long pos;
char *str;
int len;
{
    while(len>0 && srctext_GetChar(self,pos++)==*str++)
	--len;
    return len==0;
}

/*
 * optimized whitespace insertion
 * 7 June 1987 - Miles Bader
 */

static char *spaces="        ";

/*RSK91overstrike: changed srctext_InsertCharacters calls to srctext_ JustInsertCharacters calls*/
#define TABOVER(self,oldpos,pos,oldcol,col) \
    if(col>oldcol){\
        srctext_DeleteCharacters(self,oldpos,pos-oldpos);\
        pos=oldpos;\
        if(self->useTabs){\
            int nc;\
            while(col>=(nc=(oldcol+8)&~7)){\
                oldcol=nc;\
                srctext_JustInsertCharacters(self,pos++,"\t",1);\
            }\
        }else{\
            int count=(col-oldcol)/8;\
            while(count--){\
                srctext_JustInsertCharacters(self,pos,spaces,8);\
                oldcol+=8;\
                pos+=8;\
            }\
        }\
        srctext_JustInsertCharacters(self,pos,spaces,col-oldcol);\
        pos+=(col-oldcol);\
   }

/*RSK91overstrike: this is a duplicate of the original simpletext_InsertCharacters; it's used by the TABOVER macro to ignore overstrike mode*/
boolean srctext__JustInsertCharacters(self,pos,str,len)
struct srctext *self;
long pos;
char *str;
long len;
{
    if (pos >= srctext_GetFence(self)) {
        srctext_AlwaysInsertCharacters(self, pos, str, len);
	return TRUE;
    }
    else
        return FALSE;
}

long srctext__TabAndOptimizeWS(self,pos,inc)
struct srctext    *self;
long	pos;
int	inc;
{
    long     home=0, oldPos, col=0, oldCol=0, target;

    while(pos>0 && srctext_GetChar(self,pos-1)!='\n') {
	++home;
	--pos;
    }

    oldPos= pos;
    while(home--){
	switch(srctext_GetChar(self,pos)){
	    case '\t':
		col=(col+8)&~7;
		break;
	    case ' ':
		++col;
		break;
	    default:
		if(col>oldCol+1)
		    TABOVER(self,oldPos,pos,oldCol,col);

		oldPos=pos+1;
		oldCol= ++col;
	}
	++pos;
    }

    /* the column that we are aiming for */
    if(inc==0)
	target=col;
    else
	target=(col+inc)-(col%inc);

    /* skip over existing white space */
    while(col<target){
	int	nc;
	int	c=srctext_GetChar(self,pos);

	if(c==' '){
	    ++col;
	    ++pos;
	}else if(c=='\t' && (nc=(col+8)&~7)<=target){
	    col=nc;
	    ++pos;
	}else
	    break;
    }

    /* add new whitespace */
    TABOVER(self,oldPos,pos,oldCol,target);

    return pos;
}

/* COMMENTSTYLED is a macro that determines whether the character at pos is in a comment */
#define COMMENTSTYLED(self,pos) ((srctext_GetStyle((self),(pos))==(self)->comment_style) || (srctext_GetStyle((self),(pos))==(self)->linecomment_style))

/* Indent will leave existing comments where they are unless reindent-comments is on. ReindentLine is used by the ^J key, but [Tab] still calls this routine. */
long srctext__Indent(self,mark)
struct srctext *self;
struct mark *mark;
{
    boolean blankline;
    int ind, c=0;
    long end, pos=mark_GetPos(mark);

    pos= srctext_GetBeginningOfLine(self,pos);
    do {
	end= srctext_SkipWhitespace(self,pos,srctext_GetLength(self));
	blankline= (srctext_GetChar(self,end)=='\n' || end>=srctext_GetLength(self));
	if (blankline || !COMMENTSTYLED(self,end+1)) {
	    /* either it's a blank line (so guess), */
	    /* or it's code (use correct indentation) */
	    ind= srctext_Indentation(self,pos);
	    if (blankline && mark_GetLength(mark))
		/* a blank line in a selected region shouldn't waste chars */
		ind= 0;
	}
	else
	    /* it's a non-blank comment line (leave it where it is unless reindent-comments is on) */
	    if (self->reindentComments)
		ind= srctext_Indentation(self,pos);
	    else
		ind= srctext_CurrentIndent(self,pos);
	/* optimize whitespace and scrap extraneous afterwards */
	pos= srctext_TabAndOptimizeWS(self,pos,ind);
        for(end=pos; is_whitespace(srctext_GetChar(self,end)); ++end) ;
        if(end>pos)
            srctext_DeleteCharacters(self,pos,end-pos);
        end=mark_GetEndPos(mark);
	while(pos<end && (c=srctext_GetChar(self,pos++))!=EOF && c!='\n') ;
    } while(pos<end && c!=EOF);
    return pos;
}

/* ReindentLine, which is called when ^J is hit, reindents properly, and in a comment will guess how the next line should be indented. */
void srctext__ReindentLine(self,pos)
struct srctext *self;
long pos;
{
    long end, skipws;
    int c;
    boolean blankline;

    pos= srctext_GetBeginningOfLine(self,pos);
    skipws= srctext_SkipWhitespace(self,pos,srctext_GetLength(self));
    c= srctext_GetChar(self,skipws);
    blankline= ((c=='\n') || (c==EOF));
    if (blankline || !srctext_InCommentStart(self,skipws+1))
	/* code, or empty comment line; use proper indentation */
	pos= srctext_TabAndOptimizeWS(self,pos,srctext_Indentation(self,pos));
    else
	/* don't mess up continued comments unless reindent-comments is on */
	if (self->reindentComments)
	    pos= srctext_TabAndOptimizeWS(self,pos,srctext_Indentation(self,pos));
	else
	    pos= srctext_TabAndOptimizeWS(self,pos,srctext_CurrentIndent(self,pos));
    for(end=pos; (c=srctext_GetChar(self,end))==' ' || c=='\t'; ++end);
    if(end>pos)
        srctext_DeleteCharacters(self,pos,end-pos);
}

/* srctext_ReflowComment will check to see if pos is inside a comment. If the comment is continued from a previous line, the carriage return and extra whitespace will be removed to "flow" the comments together. */
boolean srctext__ReflowComment(self,pos)
struct srctext *self;
long pos;
{
    long startcomment=srctext_InCommentStart(self,pos);
    if (startcomment) {
	long startline=srctext_GetBeginningOfLine(self,pos);
	if (startline>startcomment) {
	    /* comment is broken by a carriage return inside it-- flow back together if second line isn't blank */
	    long skipws= srctext_SkipWhitespace(self,startline, srctext_GetLength(self));
	    if (srctext_GetChar(self, skipws) != '\n') {
		srctext_ReplaceCharacters(self,startline-1, skipws-startline+1, " ",1);
		return TRUE;
	    }
	}
    }
    return FALSE;
}

/* Breakable returns TRUE if pos is after whitespace or a comma, or the start of a change flag, which means the line can be broken at pos. */
boolean srctext__Breakable(self, pos)
struct srctext *self;
long pos;
{
    char pch, ch=srctext_GetChar(self, pos);
    boolean instring=srctext_InString(self,pos);
    if (is_whitespace(ch) && !instring)
	return TRUE;
    pch= srctext_GetChar(self, pos-1);
    if ((is_whitespace(pch) || pch==',') && !instring)
	return TRUE;
    if (ch=='@' && srctext_InCommentStart(self,pos))
	return TRUE;
    return FALSE;
}

/* BreakLine breaks up a line so it fits within the specified max-length. endofline is a mark (so it stays put during reindenting) pointing to the newline at the end of the line to be broken. */
void srctext__BreakLine(self,endofline)
struct srctext *self;
struct mark *endofline;
{
    long end;
    int c;
    int ccol= srctext_CurrentColumn(self,mark_GetPos(endofline)-1);
    int prevlen= ccol+1; /* add 1 to ensure at least 1 iteration of while loop */
    /* ccol & prevlen keep track of how long the remainder of the line IS and WAS */
    /* this prevents infinite loops when a 'word' is too long to break up right */
    while (ccol>=srctext_GetMaxLineLength(self) && ccol<prevlen) {
	/* line too long; fragment it */
	long stopline= mark_GetPos(endofline);
	int curcol= ccol+1;
	do  {
	    --stopline;
	    c= srctext_GetChar(self,stopline);
	    if (c=='\t') /* recalculate */
		curcol= srctext_CurrentColumn(self, stopline);
	    else /* decrement (it's a LOT quicker!) */
		--curcol;
	} while (c!='\n' && stopline>0 && (!srctext_Breakable(self,stopline) || curcol>srctext_GetMaxLineLength(self)));
	if (stopline<1 || c=='\n')
	    return; /* never mind */
	if (srctext_GetStyle(self,stopline)==self->linecomment_style)
	    /* we can't break bang comments! */
	    stopline= environment_Eval(srctext_GetEnvironment(self, stopline));
	end= stopline;
	while (stopline>0 && is_whitespace(srctext_GetChar(self,stopline-1)))
	    --stopline;
	if (stopline<end)
	    srctext_DeleteCharacters(self,stopline,end-stopline);
	/* pretend there's a blank line so it uses 'desired' _Indentation */
	srctext_InsertCharacters(self,stopline,"\n\n",2);
	srctext_ReindentLine(self,stopline+1);
	stopline= srctext_SkipWhitespace(self,stopline+1, srctext_GetLength(self));
	srctext_DeleteCharacters(self, stopline, 1); /* remove extra newline */
	/* reindent *again* in case we have to right-justify an end-of-comment */
	srctext_ReindentLine(self,stopline);
	prevlen= ccol;
	ccol= srctext_CurrentColumn(self,mark_GetPos(endofline)-1);
    }
}

/*RSK91overstrike: mostly snagged from Patch10's ConfirmViewDeletion in atk/text/txtvcmod.c*/
static boolean MakeSureNotOverstrikingView(d, pos, len)
struct srctext *d;
long pos, len;
{
    boolean hasViews;
    struct environment *env;

    for (hasViews = FALSE; len--; ++pos)
	if (srctext_GetChar(d, pos) == TEXT_VIEWREFCHAR) {
	    env= environment_GetInnerMost(d->header.text.rootEnvironment, pos);
	    if (env->type == environment_View) {
		hasViews = TRUE;
		break;
	    }
	}
    if (! hasViews)
	return TRUE;
    return FALSE; /*can't type over insets*/
}

/*RSK91overstrike: this routine was based on Patch10's textview_ViDeleteCmd (atk/text/txtvcmod.c).*/
void srctext__OverstrikeAChar(d,pos)
struct srctext *d;
long pos;
{
    int	dsize;
    char c;

    if ( srctext_GetChar(d, pos - 1) == '\n' && srctext_GetChar(d, pos) == '\n' )
	return;
    dsize = srctext_GetLength(d);
    if (MakeSureNotOverstrikingView(d, pos, 1))
	if ( (c=srctext_GetChar(d, pos))!='\n' && c!='\t' && pos<dsize)
	    srctext_DeleteCharacters(d, pos, 1);
}

/*RSK91overstrike: override simpletext's normal character insertion*/
boolean srctext__InsertCharacters(self, pos, str, len)
struct srctext *self;
long pos;
char *str;
long len;
{
    if (srctext_IsInOverstrikeMode(self)) {
	int i= 0;
	do  {
	    if (*(str+i)!='\n' && *(str+i)!='\t')
		srctext_OverstrikeAChar(self,pos+i);
	    ++i;
	} while (i<len);
    }
    return super_InsertCharacters(self,pos,str,len);
}

/* Address of pos and address of len are passed in. Sets length to zero if no valid tokens are found. */
/* (this is used by other packages, but not the source views themselves) */
void srctext__GetToken(self,pos,len)
struct srctext *self;
long *pos,
     *len;
{
    long revpos = (*pos);
    long forpos = (*pos)+(*len);
    long endtxt = srctext_GetLength(self);

    if (srctext_IsTokenChar(self, srctext_GetChar(self, revpos)))
	while (revpos>=0 && srctext_IsTokenChar(self, srctext_GetChar(self, revpos-1))) --revpos;
    (*pos) = revpos;
    while (forpos<endtxt && srctext_IsTokenChar(self,srctext_GetChar(self,forpos))) ++forpos;
    (*len) = forpos-(*pos);         
    return;
}
