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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/content.c,v 1.19 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <ctype.h>
#include <envrment.ih>
#include <text.ih>
#include <style.ih>
#include <buffer.ih>
#include <environ.ih>
#include <stylesht.ih>
#include <content.eh>

#define INDENTSPACE 6
#ifndef TEXT_VIEWREFCHAR
#define TEXT_VIEWREFCHAR '\377'
#endif

#define ISPRINT(x) ((((unsigned char)x)>160)?TRUE:isprint(x))

static char defaultlist[] = 
/*    "majorheading,heading,subheading,chapter,section,subsection,paragraph,function" */
    "chapter,section,subsection,paragraph,function"
;
static char *indexnames[] = {
    "index",
    "indexi",
    ""
};
#define indexnamecount 2

static int doindent();
static int unindent();
static void NoteStyle();
static int ensure();

static findinlist(lst,cnt,str)
char **lst; 
int cnt;
char *str;
{
    int i;
    for(i = 0; i < cnt; i++,lst++){
	if(*lst == NULL || str == NULL) return -1;
	if(**lst == *str && (*lst == str || strcmp(*lst,str) == 0)){
	    return i;
	}
    }
    return -1;
}
static appendlist(lst,cnt,ostr,TEST)
char **lst;
int cnt;
char *ostr;
int TEST;
{   /* BUG -- OVERFLOWS NOT DETECTED */
#ifndef _IBMR2
    extern char *malloc();
#endif /* _IBMR2 */
    char *str;
    long len;
    int next = 1;
    ;
    if(ostr == NULL || (len = strlen(ostr)) == 0 || ((str = malloc(len + 1)) == NULL)) return;
    strcpy(str,ostr);
    if(TEST){
	if(findinlist(lst,cnt,str) != -1) return cnt;
    }
    while(*str){
	if(*str == ',' || *str == '\n') {
	    *str = '\0';
	    next++;
	}
	else if(*str == '\0') break;
/*	else if(*str == ' ') ; */
	else if(next){
	    lst[cnt++] = str;
	    next = 0;
	}
	str++;
    }
    lst[cnt] = NULL;
    return cnt;
}
#define setchap(cp,remloc,remlen)   mark_SetPos(cp->rem,remloc);mark_SetLength(cp->rem,remlen);mark_SetModified(cp->rem,1)
static char *chapnote(ip)
int *ip;
{
	static char ret[128];
	char *c;
	c = ret;
	sprintf(ret,"%d",*ip++);
	while(*ip > 0 || ip[1] > 0 || ip[2] > 0){
		while(*c != '\0') c++;
		sprintf(c,".%d",*ip++);
	}
	while(*c != '\0') c++;
	*c++ = '\t';
	*c='\0';
	return(ret);
}
#define NUMLEV 24	
static struct content_chapentry *findcp(self,pos,len)
struct content *self;
long pos,len;
{
    struct content_chapentry *cp;
    if(pos < 0) return(self->entry);
    pos += len;
    for(cp = self->entry; cp != NULL;cp = cp->next){
	if(mark_GetPos(cp->loc) <= pos && mark_GetPos(cp->loc) + mark_GetLength(cp->loc) >= pos)
	    break;
	if(cp->next == NULL) return NULL;
    }
    return(cp);
}
static struct content_chapentry *findremcp(self,pos)
struct content *self;
long pos;
{
    struct content_chapentry *cp;
    if(pos < 0) return(self->entry);
    for(cp = self->entry; cp != NULL;cp = cp->next){
	if(mark_GetPos(cp->rem) <= pos && mark_GetPos(cp->rem) + mark_GetLength(cp->rem) >= pos)
	    break;
	if(cp->next == NULL) return NULL;
    }
    return(cp);
}
static struct content_chapentry *findindexcp(self,pos,len)
struct content *self;
long pos,len;
{
    struct content_chapentry *cp;
    if(pos < 0) return(self->entry);
    pos += len;
    for(cp = self->indexentry; cp != NULL;cp = cp->next){
	if(mark_GetPos(cp->loc) <= pos && mark_GetPos(cp->loc) + mark_GetLength(cp->loc) >= pos)
	    break;
	if(cp->next == NULL) return NULL;
    }
    return(cp);
}
static long StoI(self,ppos,lev,src)
struct content *self;
long *ppos;
int *lev;
struct text *src;
{
    char *cp,c,buf[128];
    long pos = *ppos;
    cp = buf;
    if(self->srctext == NULL) return(0);
    for(; ((c = text_GetChar(src,pos)) == '\t' )|| c == ' ' || c == '\n'; pos++) ;
    *ppos = pos;
    for(; ((c = text_GetChar(src,pos)) == '\t') || c == '.' || isdigit(c) ; pos++){
	if(c == '.' || c == '\t'){
	    *cp = '\0';
	    if(lev){
		*lev= atoi(buf);
		lev++;
	    }
	    if(c == '\t') break;
	    cp = buf;
	}
	else *cp++ = c;
	
    }
    if(c == '\t' && cp != buf){
	if(lev) *lev = 0;
	return(pos + 1 - *ppos);
    }
    return(0);

}
static long content__StringToInts(self,pos,lev)
struct content *self;
long pos;
int *lev;
{
return StoI(self,&pos,lev,(struct text *)self);
}
    
static denumber(self,cp)
struct content *self;
struct content_chapentry *cp;
{
    long pos,len,opos,olen;
    opos = pos = mark_GetPos(cp->rem);
    olen = mark_GetLength(cp->rem);
    if((len = StoI(self,&pos,NULL,self->srctext)) > 0){
	text_AlwaysDeleteCharacters(self->srctext,pos,len);
}
    return(opos + olen <= pos + len);
}
static struct mark *content__locate(self,pos)
struct content *self;
long pos;
{
    struct content_chapentry *cp;
    if(self->srctext == NULL) return NULL;
    cp = findcp(self,pos,0);
    if(cp == NULL && (cp = findindexcp(self,pos,0)) == NULL){
/*	for(cp = self->entry; cp != NULL;cp = cp->next)
	    if(cp->next == NULL) return(cp->rem); */
	return(NULL);
    }
    return cp->rem;
}
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
    if(*pos >= end) return FALSE;
    *len = end - *pos;
    return TRUE;
	
}
static boolean skipchapnumber(d,pos,len,update)
struct text *d;
long *pos,*len;
boolean update;
{
    long i,end;
    int c;
    end = *pos + *len;
    for(i = *pos ; i < end; i++){
	c = text_GetChar(d,i);
	if(c == '.' || isdigit(c)) continue;
	if(isspace(c) && c != '\t') continue;
	break;
    }
    if(i >= end ) return FALSE;
    if(c == '\t') i++;
    if(i >= end ) return FALSE;
    if(update){
	*pos = i;
	*len = end - *pos;
    }
    return TRUE;	
}
struct content_chapentry *content__CopyEntry(self,pos,len,buf,buflen)
struct content *self;
long pos,len;
char *buf;
long buflen;
{   /* copy out the index entry for the given pos in the source text */
    struct content_chapentry *cp;
    long llen;
    char *cc;
    if(self->srctext == NULL) return NULL;
    if(skipnewlines(self->srctext,&pos,&len) == NULL) return NULL;
    cp = findremcp(self,pos);
    if(cp == NULL){
/* fprintf(stderr,"remcp failed, pos = %d\n",pos);fflush(stderr); */
	return(NULL);
    }
    if((llen = mark_GetLength(cp->loc)) > buflen) llen = buflen;
    content_CopySubString(self,mark_GetPos(cp->loc),llen,buf,FALSE);
/* fprintf(stderr,"CEbuf = |%s| \n",buf);fflush(stderr); */

    for(cc = buf; *cc != '\0'; cc++)
	if(!ISPRINT(*cc) && *cc != '\t') *cc = ' ';
    cc--;
    while(cc >= buf && isspace(*cc)) *cc-- = '\0';
    return cp;
}
   
static void content__Denumerate(self,pos,len)
struct content *self;
long pos,len;
{
    struct content_chapentry *cp,*endcp;
    if(self->srctext == NULL) return;
    if(pos < 0){
	endcp = NULL;
	self->enumerate = FALSE;
    }
    else endcp = findcp(self,pos,len);
    for(cp = findcp(self,pos,0);cp != NULL;cp = cp->next){
	if(cp == endcp) break;
	denumber(self,cp);
    }
    self->doindent = TRUE;
    if(!self->isindented) doindent(self);   
    text_NotifyObservers(self->srctext,0);
}
static number(self,string,cp)
struct content *self;
char *string;
struct content_chapentry *cp;
{
    char c;
    long pos,npos,end;
    struct text *src = self->srctext;
    if(string != NULL && cp != NULL) {
	int len = strlen(string);
/*	denumber(self,cp); */
	pos = mark_GetPos(cp->rem);
	end =  mark_GetPos(cp->rem) + mark_GetLength(cp->rem) ;
	for(npos = pos; npos <end && ((c = text_GetChar(src,npos)) == '\t') || c == ' ' || c == '\n'; npos++) ;
	if(npos == end && pos + 1 < npos){
	    npos = pos + 1;
	    c = text_GetChar(src,npos);
	}
	if(npos == end){
	    text_AlwaysInsertCharacters(self->srctext,npos,string,len);
	}
	else {
	    if(c == TEXT_VIEWREFCHAR)
		text_AlwaysCopyText(self->srctext,npos + 1,self->srctext,npos,1);
	    else{
		text_AlwaysInsertCharacters(self->srctext,npos + 1,&c,1);
	    }
	    text_AlwaysInsertCharacters(self->srctext,npos + 1,string,len);
	    text_AlwaysDeleteCharacters(self->srctext,npos,1);
	}
	return TRUE;
    }
    return FALSE;

}
static long content__Enumerate(self,opos,len,start)
struct content *self;
long opos,len;
char *start;
{   
    int tab[NUMLEV],lev[NUMLEV],i,pos,lastlev,curlev,initializing;
    long slen,maxlen;
    char *nn;
    struct content_chapentry *cp,*endcp;
    char buf[256];
    int res;

    if(self->srctext == NULL) return;
    ensure(self,&(self->entry),opos,len);
    ensure(self,&(self->indexentry),opos,len);
    
    if(self->isindented) unindent(self);
    if(start){
	slen = strlen(start);
	if(*start < '0' || *start > '9') start = NULL;
	else if(start[slen - 1] != '\t'){
	    strcpy(buf,start);
	    buf[slen - 1] = '\t';
	    buf[slen] = '\0';
	    start = buf;
	    slen++;
	}
    }
    initializing = FALSE;
    for(i = 0; i < NUMLEV; i++) lev[i] = tab[i] = 0;
    for(cp = self->entry; cp != NULL;cp = cp->next)
	tab[cp->which]++;
    lastlev = 0;curlev = 0;
    pos = 0;maxlen = 0;
    if(opos < 0) {
	endcp = NULL;
	self->enumerate = TRUE;
    }
    else endcp = findcp(self,opos,len);
    cp = findcp(self,opos,0);
    if(endcp == cp) return;
    if(start !=	NULL &&	cp != NULL) {
	res = denumber(self,cp);
	while (cp->loc == NULL || mark_GetLength(cp->loc) == 0 || res){
	    if((cp = cp->next) == endcp || cp == NULL ) break;
	    res = denumber(self,cp);
	}
	if(cp && cp != endcp){
	    number(self,start,cp);
	    pos = mark_GetPos(cp->loc);
	    if(content_StringToInts(self,pos,lev) == 0){
		/* should never happen */
		for(i = 0; i < NUMLEV; i++) lev[i] = 0;
	    }
	    else initializing = TRUE;
	}
    }
    
    for(;cp != NULL && cp != endcp;cp = cp->next){
	if(!initializing) res = denumber(self,cp);
	if(res || cp->loc == NULL /* || mark_GetLength(cp->loc) == 0 */) continue;
	pos = mark_GetPos(cp->loc);
	if(cp->which != lastlev){
	    for(curlev = 0, i = 0; i < cp->which; i++) 
		if(tab[i]) curlev++;
	    for(i = curlev+1; i < NUMLEV; i++)
		lev[i] = 0;
	    lastlev = cp->which;
	    if(initializing){
		initializing = FALSE;
		continue;
	    }
	}
	lev[curlev]++;
	nn = chapnote(lev);
	slen = strlen(nn);
	if(number(self,nn,cp) == FALSE)
	    lev[curlev]--;
	else {
	    if(slen > maxlen) maxlen = slen;
	}
    }
    content_NotifyObservers(self,0);
    text_NotifyObservers(self->srctext,0);
    return maxlen;
}

static int doindent(self)
struct content *self;
{   
    int tab[NUMLEV],i,lastlev,curlev;
    struct content_chapentry *cp;
    static char buf[] = "                                                          ";

    if(self->srctext == NULL) return;
    for(i = 0; i < NUMLEV; i++) tab[i] = 0;
    for(cp = self->entry; cp != NULL;cp = cp->next)
	tab[cp->which]++;
    lastlev = 0;curlev = 0;
    for(cp = self->entry;cp != NULL;cp = cp->next){
	if(cp->which != lastlev){
	    for(curlev = 0, i = 0; i < cp->which; i++) 
		if(tab[i]) curlev++;
	    lastlev = cp->which;
	}
	cp->space = curlev;
	content_AlwaysInsertCharacters(self,mark_GetPos(cp->loc),buf,cp->space *INDENTSPACE);
    }
    self->isindented = TRUE;
}
static int unindent(self)
struct content *self;
{   
    struct content_chapentry *cp;
    long len,start;
    start = 0;
    for(cp = self->entry;cp != NULL;cp = cp->next){
	if((len =  mark_GetPos(cp->loc) - start) > 0){
	    content_AlwaysDeleteCharacters(self,start,len);
	}
	start = mark_GetPos(cp->loc) + mark_GetLength(cp->loc) + 1;
	cp->space = -1;
   }
    self->isindented = FALSE;
}
static struct content_chapentry *newentry(self,next,addmark)
struct content *self;
struct content_chapentry *next;
boolean addmark;
{
    struct content_chapentry *cp;
    cp = (struct content_chapentry *)malloc(sizeof(struct content_chapentry ));
    cp->rem = text_CreateMark(self->srctext,0,0);
    mark_SetStyle(cp->rem,FALSE,TRUE);
    if(addmark){
	cp->loc = content_CreateMark(self,0,0);
	mark_SetStyle(cp->loc,FALSE,FALSE);
    }
    else cp->loc = NULL;
    cp->next = next;
    cp->space = -1;
    return cp;
}
static freeentry(self,cp, lastcp)
struct content *self;
struct content_chapentry *cp,*lastcp;
{
    if(cp->loc) content_RemoveMark(self,cp->loc);
    if(self->srctext && cp->rem ) text_RemoveMark(self->srctext,cp->rem);
    if(lastcp != NULL)
	lastcp->next = cp->next;
    free(cp);
}   
static int ensure(self,base,pos,len)
struct content *self;
struct content_chapentry **base;
long pos,len;
{
    struct content_chapentry *cp,*lastcp;
    long llen;
    int count;
    if(self->srctext == NULL || self->InUpdate) return;
    self->InUpdate = TRUE;
    count = 0;
    /* see if any marks have changed */
    lastcp = NULL;
    for(cp = *base; cp != NULL; lastcp = cp,cp = cp->next){
	if(mark_GetModified(cp->loc)){
	    if(mark_GetPos(cp->loc) < pos) continue;
	    llen = mark_GetLength(cp->loc);
	    if(pos > 0 && mark_GetPos(cp->loc) + llen > pos + len) break;
	    if(mark_GetLength(cp->loc) == 0) {
		if(lastcp == NULL) *base = cp->next;
		freeentry(self,cp,lastcp);
		count++;
		if(lastcp == NULL){
		    cp = *base;
		    if(cp == NULL) break;
		}
		else cp = lastcp;
		continue;
	    }
	}
    }
    self->InUpdate = FALSE;
    return count;
}
static copymark(desttext,destmark,srctext,srcmark,cap)
struct text *desttext;
struct mark *destmark;
struct text *srctext;
struct mark *srcmark;
boolean cap;
{
    char *c,buf[256],*cp;
    long len = mark_GetLength(srcmark);

    if(len > 250) c = malloc(len + 10);
    else c = buf;
    text_CopySubString(srctext,mark_GetPos(srcmark),len,c,FALSE);
 /*   if(cap && islower(*c)) *c = toupper(*c); */
    for(cp = c; *cp != '\0'; cp++) if(!ISPRINT(*cp) && !isspace(*cp)) *cp = ' ';
    text_AlwaysReplaceCharacters(desttext,mark_GetPos(destmark),mark_GetLength(destmark),
				  c,len);

    if(len > 250) free(c);
    mark_SetLength(destmark,len);
    mark_SetModified(destmark,0);
    text_NotifyObservers(desttext,0);
}
static freeentrys(self)
struct content *self;
{
    struct content_chapentry *cp,*lastcp;
    lastcp = NULL;
    for(cp = self->entry; cp != NULL;cp = cp->next){
	if(lastcp != NULL){
	    freeentry(self,lastcp,NULL);
	}
	lastcp = cp;
    }
    if(lastcp != NULL){
	freeentry(self,lastcp,NULL);
    }
    lastcp = NULL;
    for(cp = self->indexentry; cp != NULL;cp = cp->next){
	if(lastcp != NULL){
	    freeentry(self,lastcp,NULL);
	}
	lastcp = cp;
    }
    if(lastcp != NULL){
	freeentry(self,lastcp,NULL);
    }
    self->entry = self->indexentry = NULL;
}
static clear(self)
struct content *self;
{
    content_SetReadOnly(self,FALSE);
    freeentrys(self);
    content_Clear(self);
    content_SetReadOnly(self,TRUE);
}
static boolean ns(self,text,pos,env)
struct content *self;
struct text *text;
long pos;
struct environment *env;
{
    if (env->type == environment_Style){
	NoteStyle(self,pos,environment_GetLength(env),env->data.style);
    }
    return FALSE;
}
#define INDEXHEADER "------INDEX----\n"

void content__reinit(self)
struct content *self;
{
    struct content_chapentry *cp;
    if(self->srctext == NULL) return;
    clear(self);
    text_EnumerateEnvironments(self->srctext,0, text_GetLength(self->srctext),ns,(long)self);
    if(self->indexentry){
	content_AlwaysInsertCharacters(self,content_GetLength(self) ,INDEXHEADER,strlen(INDEXHEADER));
    }
    for(cp = self->indexentry; cp != NULL ; cp = cp->next){
	mark_SetPos(cp->loc,content_GetLength(self));
	copymark((struct text *)(self),cp->loc,self->srctext,cp->rem,TRUE);
	content_AlwaysInsertCharacters(self,mark_GetPos(cp->loc) + mark_GetLength(cp->loc) + 1,"\n",1);
    }
    self->isindented = FALSE;
    if(self->chapcount != -1){
	/* determine if new text is already enumerated */
	int i = 0;
	for(cp = self->entry; cp != NULL; cp = cp->next)
	    i++;
	if(self->chapcount > 0 && self->chapcount > i / 4)
	    self->doindent = FALSE;
	self->chapcount = -1;
    }
    if(self->enumerate) content_Enumerate(self,-1,0,NULL);
    else if(self->doindent) doindent(self);

}

static interestingstyle(self,name)
register struct content *self;
register char *name;
{
    register char **sp;
    register int which = 0;
    if(name == NULL) return 0;
    for(sp = self->names;sp &&  *sp && **sp; sp++){
	if(which++ == self->namecount) break;
	if(**sp == *name && strcmp(*sp,name) == 0){
	    return which;
	}
    }
    return 0;
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

static struct content_chapentry *addindexentry(self,pos,len,base)
struct content *self;
long pos,len;
struct content_chapentry **base;
{
    char buf[512],Buf[512];
    struct content_chapentry *cp,*lastcp;
    int rc;
    lastcp = NULL;
    /* should probably keep and end pointer around to save time here */
    if(len > 512) len = 512;
    text_CopySubString(self->srctext,pos,len,buf,FALSE);
    strcpy(Buf,buf);
    if(islower(*buf)) *Buf = toupper(*buf);
    else *buf = tolower(*Buf);
    for(cp = *base; cp != NULL;cp = cp->next){
	rc = text_GetChar(self->srctext,mark_GetPos(cp->rem));
	if(islower(rc)){
	    if(rc > *buf || (rc == *buf && text_Strncmp(self->srctext,mark_GetPos(cp->rem),buf,len) > 0))
		break;
	    
	}
	else {
	    if(rc > *Buf || (rc == *Buf && text_Strncmp(self->srctext,mark_GetPos(cp->rem),Buf,len) > 0))
		break;
	}
	lastcp = cp;
    }
    if(lastcp){
	cp = newentry(self,cp,TRUE);
	lastcp->next = cp;
    }
    else{
	cp = newentry(self,*base,TRUE);
	*base = cp;
    }
    return cp;
}
static struct content_chapentry *insertentry(self,pos,len,base)
struct content *self;
long pos,len;
struct content_chapentry **base;
{
    struct content_chapentry *cp,*lastcp;
    lastcp = NULL;
    /* should probably keep and end pointer around to save time here */
    for(cp = *base; cp != NULL; lastcp = cp,cp = cp->next){
	if(mark_GetPos(cp->rem) > pos) break;
    }
    if(lastcp){
	cp = newentry(self,lastcp->next,TRUE);
	lastcp->next = cp;
	mark_SetPos(cp->loc,mark_GetPos(lastcp->loc) + mark_GetLength(lastcp->loc) + 1);
    }
    else{
	cp = newentry(self,*base,TRUE);
	*base = cp;
    }
    return cp;

}

static void NoteStyle(self,pos,len,style)
struct content *self;
long pos,len;
struct style *style;
{
    int which;
    char *sn;
    struct content_chapentry *cp;
    if(style == NULL || ((sn = style_GetName(style)) == NULL)) return;
    if((which = interestingstyle(self,sn)) > 0){
	  struct content_chapentry *cp;
	  if(skipnewlines(self->srctext,&pos,&len)){
	      if(self->chapcount >= 0){
		  /* counting number of entries w/ chap numbers */
		  long npos,nlen;
		  npos = pos;
		  nlen = len;
		  skipchapnumber(self->srctext,&npos,&nlen,TRUE);
		  if(npos != pos) self->chapcount++;
	      }
	     cp = insertentry(self,pos,len,&(self->entry));
	     setchap(cp,pos,len);
	     copymark((struct text *)(self),cp->loc,self->srctext,cp->rem,FALSE);
	     content_AlwaysInsertCharacters(self,mark_GetPos(cp->loc) + mark_GetLength(cp->loc) + 1,"\n",1);
	     cp->which = which;
	     mark_SetModified(cp->rem,0);
	     }
      }
    else if((which = indexstyle(sn))!= 0) {
	if(skipnewlines(self->srctext,&pos,&len) &&
	   skipchapnumber(self->srctext,&pos,&len,TRUE) ){
	   cp = addindexentry(self,pos,len,&(self->indexentry));
	   setchap(cp,pos,len);
	   }
	/* later the index will added to the end of the document 
	 */
    }
}
static doshuffle(self)
struct content *self;
{
    /* punt for now */
 /*   content_reinit(self);  */
}
static checknewline(self,cp)
struct content *self;
struct content_chapentry *cp;
{
    /* punt for now */
}
static boolean updatemark(d,m,nonum)
struct text *d;
struct mark *m;
boolean nonum;
{
    long pos ,len;
    pos = mark_GetPos(m);
    len = mark_GetLength(m);
    if(skipnewlines(d,&pos,&len) &&
	(!nonum || skipchapnumber(d,&pos,&len,TRUE)) ){
	if(pos != mark_GetPos(m)) mark_SetPos(m,pos);
	if(len != mark_GetLength(m)) mark_SetLength(m,len);
	return TRUE;
    }
    else return FALSE;
}
static mod(self,base,nonum)
struct content *self;
struct content_chapentry **base;
boolean nonum;
{
    struct content_chapentry *cp,*lastcp;
/*    int shuffle = 0; */
    if(self->InUpdate) return;
    self->InUpdate = TRUE;
    /* see if any marks have changed */
    lastcp = NULL;
    for(cp = *base; cp != NULL; lastcp = cp,cp = cp->next){
	if(mark_GetModified(cp->rem)){
	    /* update local mark */
	    if(!updatemark(self->srctext,cp->rem,nonum )){
		if(lastcp == NULL) *base = cp->next;
		freeentry(self,cp,lastcp);
		if(lastcp == NULL){
		    cp = *base;
		    if(cp == NULL) break;
		}
		else cp = lastcp;
		continue;
	    }
	    copymark((struct text *)(self),cp->loc,self->srctext,cp->rem,nonum);
/*
	    if(lastcp && mark_GetPos(cp->rem) < mark_GetPos(lastcp->rem))
		shuffle++;
*/
	    checknewline(self,cp);
	    mark_SetModified(cp->loc, 0);
	    mark_SetModified(cp->rem, 0);
	}
    }
/*    if(shuffle)  marks are out of order 
	doshuffle(self); */
    self->InUpdate = FALSE;
}
static void update(self)
struct content *self;
{

    mod(self,&(self->entry),FALSE); 
    mod(self,&(self->indexentry),TRUE);
}
void content__UpdateSource(self,pos,len)
struct content *self;
long pos,len;
{
    /* unnecessary function */
}

boolean content__InitializeObject(classID,self)
struct classheader *classID;
struct content *self;
{
    char *p;
    self->entry = self->indexentry = NULL;
    self->srctext = NULL;
    self->names[0] = NULL;
    if((p = environ_Get("ContentsList")) == NULL){
	if((p = environ_GetProfile("ContentsList"))== NULL)
	    p = defaultlist;
    }
    self->namecount = appendlist(self->names,0,p,FALSE);
    self->InUpdate = FALSE;
    self->enumerate = FALSE;
    self->isindented = FALSE;
    return TRUE;
}
void content__SetSourceText(self,txt)
struct content *self;
struct text *txt;
{
    if(self->srctext != txt){
	if(self->srctext){
	    clear(self);
	    text_RemoveObserver(self->srctext,self);
	}
	self->srctext = txt;
	self->chapcount = 0;
	self->doindent = TRUE;
	content_reinit(self);
	text_AddObserver(txt,self);
	content_SetReadOnly(self,TRUE);
    }
}

void content__FinalizeObject(classID,self)
struct classheader *classID;
struct content *self;
{
    if(self->srctext){
	freeentrys(self);
	text_RemoveObserver(self->srctext,self);
    }
}

void content__ObservedChanged(self,changed,value)
struct content *self;
struct observable *changed;
long value;
{
    if(changed == (struct observable *) self->srctext){
	if(value == observable_OBJECTDESTROYED){
	    self->srctext = NULL;
	    clear(self);
	}
	else update(self);
    }
    else super_ObservedChanged(self,changed,value);
}
char *content__ViewName(self)
struct content *self;
{
return "contentv";
}
