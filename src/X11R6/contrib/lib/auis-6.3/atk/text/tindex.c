/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/tindex.c,v 1.11 1993/10/26 22:43:58 gk5g Exp $";
#endif
 
#include <andrewos.h>
#include <environ.ih>
#include <print.ih>
#include <complete.ih>
#include <textv.ih>
#include <text.ih>
#include <style.ih>
#include <stylesht.ih>
#include <fontdesc.ih>
#include <proctbl.ih>
#include <envrment.ih>
#include <search.ih>
#include <ctype.h>
#include <content.ih>
#include <buffer.ih>
#include <cursor.ih>
#include <tindex.eh>

#define tindex_HIDDEN -16
#define tindex_VISIBLE -17

#define Data(self) ((struct content *)(((struct view *) self)->dataobject))

#define Text(self) ((struct text *) ((self)->header.view.dataobject))
static char *indexnames[] = {
    "index",
    "indexi",
    ""
};
static struct view *FindView(v1,v2,v3,dat)
struct view *v1,*v2,*v3;
long dat;
{
    if(class_IsTypeByName(class_GetTypeName(v2),"textview")){
	return v2;
    }
    return NULL;
}

static struct view *getcurrentview(self)
struct view *self;
{
    struct buffer *buf;
    struct content *ct;
    ct = Data(self);
    buf = buffer_FindBufferByData((struct dataobject *)ct->srctext);
    if(buf)
	return buffer_EnumerateViews(buf,(procedure) FindView,NULL);
    return NULL;
}

#define indexnamecount 2
static struct textview *getrealview(v)
struct view *v;
{
    char *name;
    name = class_GetTypeName(v);
    if(strcmp(name,"contentv") == 0)
	return (struct textview *)getcurrentview(v);
    else if(class_IsTypeByName(name,"textview"))
	return (struct textview *) v;
    else return NULL;
}
static struct cursor *WaitCursor;
static boolean skipnewlines(d,pos,len)
struct text *d;
long *pos,*len;
{
    long i,end;
    int c;
    end = *pos + *len;
    for(i = *pos ; i < end; i++){
	c = text_GetChar(d,i);
	if(! isspace(c)) break;
    }
    *pos = i;
    for( ; i < end; i++){
	if(text_GetChar(d,i) == '\n') {
	    end = i;
	    break;
	}
    }
    if(*pos > end) return FALSE;
    *len = end - *pos;
    return TRUE;
	
}
static void printindex(self)
struct view *self;
{
    tindex_PrintIndex((struct view *) getrealview(self));
}
static void previewindex(self)
struct view *self;
{
    tindex_PreviewIndex((struct view *) getrealview(self));
}

static void tindex__PrintIndex(ClassID,self)
struct classheader *ClassID;
struct view *self;
{
    if(self == NULL) return;
    message_DisplayString(NULL,0,"Preparing Index - please wait");
    environ_Put("IndexOnly",NULL);
    print_ProcessView(self, 1, 1, "Index", "");
    environ_Delete("IndexOnly");
    message_DisplayString(NULL,0,"Index sent to printer; watch console for result");

}
static void tindex__PreviewIndex(ClassID,self)
struct classheader *ClassID;
register struct view *self;
{
    if(self == NULL) return;
    message_DisplayString(NULL,0,"Preparing Index - please wait");
    environ_Put("IndexOnly",NULL);
    print_ProcessView(self, 0, 1, "Index", "");
    environ_Delete("IndexOnly");
    message_DisplayString(NULL,0,"Index Preview window should appear soon");
}
static void tindex_IndexTermCmd(v)
register struct view *v;
{
    /* Prompt for an index term and call index_IndexTerm */
    char thisString[100],*error;
    struct text *d;
    long gf,i;
    struct textview *self;
    if((self = getrealview(v)) == NULL) return ;
    d = Text(self);
    gf = message_AskForString(v, 0, "Term to index ", NULL, thisString, sizeof(thisString));
    if (gf < 0) return;
    i = tindex_IndexTerm(d,thisString,&error);
    if(i < 0){
	message_DisplayString(v,0,error);
    }
    else if(i == 0){
	message_DisplayString(v,0,"Term not found");
    }
    else {
	if(i == 1) sprintf(thisString,"one instance indexed");
	else sprintf(thisString,"%d instances found",i);
	message_DisplayString(v,0,thisString);
	if(v != (struct view *)self) content_reinit(Data(v));
	text_RegionModified(d,0,text_GetLength(d));
	text_NotifyObservers(d,0);
    }
}
void tindex_ReadIndexFile(v)
register struct view *v;
{
    /* Prompt for an index term and call index_IndexTerm */
    FILE *f,*fopen();
    char thisString[1024],*error;
    struct text *d;
    struct textview *self;
    if((self = getrealview(v)) == NULL) return ;
    d = Text(self);
    if(completion_GetFilename(v,"Index file to read: ",NULL,thisString, sizeof(thisString),FALSE,TRUE) == -1)
	return;
    if((f = fopen(thisString,"r")) == NULL){
	message_DisplayString(v,0,"Can't open file");
	return;
    }
    error = tindex_ReadIndexList(d,f);
    fclose(f);
    if(error != NULL){
	message_DisplayString(v,0,error);
    }
    else {
	message_DisplayString(v,0,"Done");
	if(v != (struct view *)self) content_reinit(Data(v));
	text_RegionModified(d,0,text_GetLength(d));
	text_NotifyObservers(d,0);
    }
}
void tindex_WriteIndexFile(v)
register struct view *v;
{
    /* Prompt for an index term and call index_IndexTerm */
    FILE *f,*fopen();
    char thisString[1024],runit[2500];
    struct text *d;
    struct textview *self;
    if((self = getrealview(v)) == NULL) return ;
    d = Text(self);
    if(completion_GetFilename(v,"Index file to Write: ",NULL,thisString, sizeof(thisString),FALSE,FALSE) == -1)
	return;
    if((f = fopen(thisString,"w")) == NULL){
	message_DisplayString(v,0,"Can't open file");
	return;
    }
    tindex_WriteIndexList(d,f);
    fclose(f);
    sprintf(runit,"sort -u -o %s %s &",thisString,thisString);
    system(runit);

    message_DisplayString(v,0,"Done");
}
static void tindex_FudgeFonts(txt,name , ftype)
struct text *txt;
char *name;
int ftype;
{
    struct style *Style;
    if(txt && (Style = stylesheet_Find(txt->styleSheet,name )) != NULL){
	switch(ftype){
	    case fontdesc_Plain:
		style_ClearOldFontFaces(Style);
		style_ClearNewFontFaces(Style);
		break;
	    case tindex_HIDDEN:
		style_AddHidden(Style);
		break;
	    case tindex_VISIBLE:
		style_RemoveHidden(Style);
		break;
	    default:
		style_AddNewFontFace(Style,ftype);
	}
    }
    text_RegionModified(txt,0,text_GetLength(txt));
    text_NotifyObservers(txt,0);
}
void tindex_MakeIndexPlain(v)
register struct view *v;
{
    struct textview *self;
    if((self = getrealview(v)) == NULL) return ;
    tindex_FudgeFonts(Text(self),"index",fontdesc_Plain);
}
void tindex_MakeIndexItalic(v)
register struct view *v;
{
    struct textview *self;
    if((self = getrealview(v)) == NULL) return ;
    tindex_FudgeFonts(Text(self),"index",fontdesc_Italic);
}
void tindex_HideInvIndex(v)
register struct view *v;
{
    struct textview *self;
    if((self = getrealview(v)) == NULL) return ;
    tindex_FudgeFonts(Text(self),"indexi", tindex_HIDDEN);
}
void tindex_ExposeInvIndex(v)
register struct view *v;
{
    struct textview *self;
    if((self = getrealview(v)) == NULL) return ;
    tindex_FudgeFonts(Text(self),"indexi", tindex_VISIBLE);
}
static indexstyle(name)
register char *name;
{
    register char **sp;
    register int which = 0;
    if(name == NULL) return 0;
    for(sp = indexnames;which < indexnamecount && sp &&  *sp && **sp; sp++){
	which++;
	if(**sp == *name && strcmp(*sp,name) == 0){
	    return which;
	}
    }
    return 0;
}

static boolean isindexenv(self,text,pos,env)
struct content *self;
struct text *text;
long pos;
struct environment *env;
{
    char *sn;
    if (env->type == environment_Style){
	struct style *style = env->data.style;
	if(style == NULL || ((sn = style_GetName(style)) == NULL)) return FALSE;
	if(indexstyle(sn)!= 0) return TRUE;
    }
    else return TRUE;
    return FALSE;
}
int tindex__IndexTerm(classID,d,term,error)
struct classheader *classID;
struct text *d;
char *term,**error;
{
    char *lastPattern = NULL;
    char  *tp;
    int pos = 0, len,dlen,c;
    register int j;
    struct style *Style = NULL;
    struct environment *te;
 
    tp =search_CompilePattern(term,&lastPattern);
    if(tp != '\0'){
	if(error) *error = tp;
	return -1;
    }
    j = 0;len = 0;
    dlen = text_GetLength(d);
    for(pos = 0;(pos = search_MatchPattern(d,pos,lastPattern)) >= 0;pos += len){
	len =  search_GetMatchLength();
	if(len == 0) {
	    return 0;
	}
	/* check if word or phrase is just part of another word */
	if(pos > 0){
	    c = text_GetChar(d,pos - 1);
	    if(isalpha(c)) continue;
	}
	if(pos + len < dlen){
	    c = text_GetChar(d,pos + len);
	    if(isalpha(c)) continue;
	}
	 /* check if already has an index style */
	if(text_EnumerateEnvironments(d,pos,len,isindexenv,NULL) == NULL){
	   /* make it an index */
	    if(Style == NULL && (Style = stylesheet_Find(d->styleSheet,"index" )) == NULL){
		Style = style_New();
		style_SetName(Style, "index");
		stylesheet_Add(d->styleSheet, Style);
		/* should give some style attributes here */
	    }
	    te = environment_WrapStyle(d->rootEnvironment,pos,len,Style);
	    environment_SetStyle(te, FALSE, FALSE);
/*	    environment_Update(d->rootEnvironment, pos, len); */
	    j++;
	}
    }
    return(j);
}
static skipchapnumber(d,pos,len)
struct text *d;
long *pos,*len;
{
    long i;
    int c;
    c = text_GetChar(d,*pos);
    if(c >= '0' && c <= '9'){
	i = text_Index(d,*pos,'\t',*len);
	i++;
	if(i > *pos && i < *pos + *len){
	    *len = *len - (i - *pos);
	    *pos = i;
	}
    }
fflush(stdout);
}
static boolean writeindex(f,text,pos,env)
FILE *f;
struct text *text;
long pos;
struct environment *env;
{
    long len;
    char *sn,c;
    if (env->type == environment_Style){
	struct style *style = env->data.style;
	if(style == NULL || ((sn = style_GetName(style)) == NULL)) return FALSE;
	if(indexstyle(sn)== 1){
	    len = environment_GetLength(env);
	    skipnewlines(text,&pos,&len);
	    skipchapnumber(text,&pos,&len);
	    for(len += pos ;pos < len ;pos++){
		c = text_GetChar(text,pos);
		if(isupper(c)) fputc(tolower(c),f);
		else fputc(c,f);
	    }
	    putc('\n',f);
	}
    }
    return FALSE;
}
void tindex__WriteIndexList(classID,d,f)
struct classheader *classID;
struct text *d;
FILE *f;
{
    text_EnumerateEnvironments(d,0,text_GetLength(d),writeindex,(long)f);
}
char *tindex__ReadIndexList(classID,d,f)
struct classheader *classID;
struct text *d;
FILE *f;
{
    char buf[512],*error,*nl;
    while(fgets(buf,511,f) != NULL){
	if((nl  = index(buf,'\n')) != NULL) *nl = '\0';
	if(tindex_IndexTerm(d,buf,&error) < 0){
	    return error;
	}
    }
    return NULL;
}
boolean tindex__InitializeClass(ClassID)
struct classheader *ClassID; 
{
    struct classinfo *viewtype = class_Load("view");

    proctable_DefineProc("tindex-print",(procedure) printindex,viewtype,NULL,"print index");
    proctable_DefineProc("tindex-preview",(procedure) previewindex,viewtype,NULL,"preview index");
    proctable_DefineProc("tindex-index-term",(procedure) tindex_IndexTermCmd,viewtype,NULL,"index all instances of a term");
    proctable_DefineProc("tindex-read-index-file",(procedure) tindex_ReadIndexFile,viewtype,NULL,"read a list of index terms ");
    proctable_DefineProc("tindex-write-index-file",(procedure) tindex_WriteIndexFile,viewtype,NULL,"write a list of index terms ");
    proctable_DefineProc("tindex-index-italic",(procedure) tindex_MakeIndexItalic,viewtype,NULL,"make the index entries italic");
    proctable_DefineProc("tindex-index-plain",(procedure) tindex_MakeIndexPlain,viewtype,NULL,"make the index entries plain");
    proctable_DefineProc("tindex-expose-inv-index",(procedure) tindex_ExposeInvIndex,viewtype,NULL,"Expose the invisible index entries");
    proctable_DefineProc("tindex-hide-inv-index",(procedure) tindex_HideInvIndex,viewtype,NULL,"Hide the invisible index entries");
    WaitCursor = cursor_Create(NULL);
    if(WaitCursor) cursor_SetStandard(WaitCursor,Cursor_Wait);
    return TRUE;
}


