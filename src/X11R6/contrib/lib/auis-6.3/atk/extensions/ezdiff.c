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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/ezdiff.c,v 2.26 1994/05/16 02:07:36 rr2b Exp $";
#endif


 
#include <class.h>

#include <andrewos.h> /* sys/file.h */
#ifndef hpux 
#include <sys/wait.h>
#endif /* hpux */
#if POSIX_ENV || defined(hpux)
#include <sys/signal.h>
#endif /* POSIX_ENV || hpux*/
#include <buffer.ih>
#include <mark.ih>
#include <textv.ih>
#include <text.ih>
#include <message.ih>
#include <proctbl.ih>
#include <environ.ih>
#include <im.ih>
#include <cursor.ih>
#include <ezdiff.eh>

#define ObjectOf(V) (((struct view *)(V))->dataobject)
#define USECURRENTMARK -32000l
static struct ezdiff *firstlink , *lastlink;
static char buffername[1024];
static struct cursor *WaitCursor;

struct diffinfo {
    struct mark *m;
    char buf[128];
};

#define buffertext(BUF) (struct text *)buffer_GetData(BUF)
static struct ezdiff *FindBufferDiff(b,which)
struct buffer *b;
int *which;
{
    register struct ezdiff *ep;
    if(b == NULL) return NULL;
    for(ep = firstlink; ep != NULL ; ep = ep->next){
	if(ep->buf[0] == b){
	    if(which) *which = 0;
	    return ep;
	}
	else if( ep->buf[1] == b) {
	    if(which) *which = 1;
	    return ep;
	}
    }
    return NULL;
}
static struct ezdiff *FindViewDiff(v,which)
struct textview *v;
int *which;
{
    return FindBufferDiff(buffer_FindBufferByData(ObjectOf(v)),which);
}
static char *getpair(cp,ip)
char *cp;
int *ip;
{
    if(*cp == '\0') return(NULL);
    while  (*cp == ' ' || *cp ==  ',' || *cp == 'a' || *cp == 'd' || *cp == 'c' ){
	cp++;
	if(*cp == '\0') return(NULL);
    }
    *ip = atoi(cp);
    if(*ip < 1 && *cp != '0') return(NULL);
    while(*cp >= '0' &&  *cp <= '9') cp++;
    while  (*cp == ' ' || *cp ==  ',' ){
	cp++;
    }
    ip++;
    *ip = atoi(cp);
    if(*ip == 0) *ip = *(ip - 1);
    else 	while(*cp >= '0' &&  *cp <= '9') cp++;
    return(cp);
}
#define BUFSIZE 4096
static struct mark *setmark (ip,d,pi,ppos)
int *ip;
struct text *d;
int *pi , *ppos;
{
    int spos, len;
    register int i, pos;
    len = text_GetLength(d);
    pos = *ppos;
    i= *pi;
    while (i<*ip)
    {
	if (pos >= len) break;
	if (text_GetChar(d,pos) == '\012') i=i+1;
	pos = pos+1;
    }
    spos = pos;ip++;
    (*ip)++;
    while (i<*ip )
    {
	if (pos >= len) break;
	if (text_GetChar(d,pos) == '\012') i=i+1;
	pos = pos+1;
    }
    *pi = i;
    *ppos = pos;
    return(text_CreateMark(d,spos,pos - spos));
}

/* Return -1 if something here fails.  Return 0 for success. */
static int ezdiff_setupmarkers(self,s)
struct ezdiff *self;
char *s;
{
    struct text *d1,*d2;
    FILE *file,*fopen();
    char buf[BUFSIZE],*cp,dc;
    int ip[2],lastmark;
    int i1,i2,pos1,pos2,count = 0;
    d1 = buffertext(self->buf[0]);
    d2 = buffertext(self->buf[1]);
    i1 = i2 = 1;
    pos1 = pos2 = 0;
    lastmark = 0;
    self->cmark = 0;

    if((file = fopen(s,"r")) == NULL) return -1;

    while(fgets(buf,BUFSIZE,file) != NULL){
	/* count the number of needed marks */
	switch(*buf){
	    case '<':
	    case '>':
	    case '-':
		break;
	    default :
		count++;
	}
    }
    count++;count++;
    self->m1 = (struct mark **) malloc(sizeof(struct mark *) * count);
    self->m2 = (struct mark **) malloc(sizeof(struct mark *) * count);
    rewind(file);
    while(fgets(buf,BUFSIZE,file) != NULL){
	switch(*buf){
	    case '<':
	    case '>':
	    case '-':
		break;
	    default :
		if((cp = getpair(buf,ip)) == NULL) break;
		if((dc = *cp) == 'a'){ *(ip + 1) = 0;(*ip)++; }
		self->m1[lastmark] = setmark(ip,d1,&i1,&pos1);
		getpair(cp,ip);
		if(dc == 'd')  { *(ip + 1) = 0;(*ip)++; }
		self->m2[lastmark] = setmark(ip,d2,&i2,&pos2);
		if(++lastmark >= count) return -1;
	}
    }
    self->nummarks = lastmark;
    fclose(file);
    return 0;
}

struct ezdiff *ezdiff__Create(classID,buf1,buf2,ignoreblanks)
struct classheader *classID;
struct buffer *buf1,*buf2;
boolean ignoreblanks;
{
    struct ezdiff *self;
    char name1[256],name2[256],fnm[512];
    char *argv[6],**av;
    int pid, exitstatus;
#if POSIX_ENV || defined(hpux)
    int status;
#else /* hpux */
    union wait status;
#endif /* hpux */
    struct text *dobj1=(struct text *)buffer_GetData(buf1), *dobj2=(struct text *)buffer_GetData(buf2);
    enum textwritestyle tws1=text_GetWriteStyle(dobj1), tws2=text_GetWriteStyle(dobj2); /*RSK*/

    sprintf(name1,"/tmp/%s-1",buffer_GetName(buf1));
    sprintf(name2,"/tmp/%s-2",buffer_GetName(buf2));
    sprintf(fnm,"/tmp/wmdiff.%s.%s", buffer_GetName(buf1),buffer_GetName(buf2));
    text_SetWriteStyle(dobj1,text_NoDataStream);
    text_SetWriteStyle(dobj2,text_NoDataStream);
    buffer_WriteToFile(buf1,name1,0);
    buffer_WriteToFile(buf2,name2,0);
    text_SetWriteStyle(dobj1,tws1);
    text_SetWriteStyle(dobj2,tws2);
    /* should check that filetype == NULL */
    av = argv;
    *av++ = "diff";
    if(ignoreblanks) *av++ = "-b";
    *av++ = name1;
    *av++ = name2;
    *av = NULL;
    self = NULL;
    if(WaitCursor) im_SetProcessCursor(WaitCursor);
#if POSIX_ENV || defined(hpux)
    signal(SIGCHLD, SIG_DFL);
#endif /* POSIX || hpux */
    if((pid = osi_vfork()) == 0) {
	close(1);
	if((open(fnm,O_RDWR | O_CREAT | O_TRUNC,0600)) == -1){
	    _exit(3);
	}
	execvp("diff",argv);
	_exit(4);
    }
#if POSIX_ENV || defined(hpux)
    waitpid(pid, &status, NULL);
#else /* POSIX_ENV || defined(hpux) */
    wait(&status);
#endif /* POSIX_ENV || defined(hpux) */

#ifdef hpux
    exitstatus = (((int)status >>8)&0377);
#else /* hpux */
#if POSIX_ENV 
    /* XXX This may also work for hpux. */
    exitstatus = (WEXITSTATUS(status));
#else
    exitstatus = (status.w_T.w_Retcode);
#endif /* POSIX_ENV */
#endif /* hpux */
    switch (exitstatus) {
	case 0: /* No Differences */
	    message_DisplayString(NULL,0,"Files are identical");
	    break;
	case 2:
	    message_DisplayString(NULL,0,"Diff Failed");
	    break;
	case 3:
	    message_DisplayString(NULL,0,"Can't open output file in /tmp");
	    break;
	case 4:
	    message_DisplayString(NULL,0,"Fork of diff failed");
	    break;
	case 1:	/* Success */
	    if((self = FindBufferDiff(buf1,NULL))!= NULL)
		ezdiff_Destroy(self);
	    if((self = FindBufferDiff(buf2,NULL))!= NULL)
		ezdiff_Destroy(self);
	    self = ezdiff_New();
	    self->buf[0] = buf1;
	    self->buf[1] = buf2;
	    self->bname[0] = malloc(strlen(buffer_GetName(buf1)) + 1);
	    strcpy(self->bname[0],buffer_GetName(buf1));
	    self->bname[1] = malloc(strlen(buffer_GetName(buf2)) + 1);
	    strcpy(self->bname[1],buffer_GetName(buf2));
	    self->numbufs = 2;
	    if (ezdiff_setupmarkers(self,fnm) == -1)
	    {
		message_DisplayString(NULL,0,"Can't make sense of diff.");
		self->buf[0] = NULL;
		self->buf[1] = NULL;
		ezdiff_Destroy(self);
		self = NULL;
		break;
	    }
	    buffer_AddObserver(self->buf[0],self);
	    buffer_AddObserver(self->buf[1],self);
	    break;
	default:
	    message_DisplayString(NULL,0,"Unknown return status from diff");
	    break;
    }
    im_SetProcessCursor(NULL);
    unlink(name1); unlink(name2); unlink(fnm);
    return self;
}
boolean ezdiff__InitializeObject(classID,self)
struct classheader *classID;
struct ezdiff *self;
{
    if(lastlink != NULL) lastlink->next = self;
    self->next = NULL;
    lastlink = self;
    if(firstlink == NULL) firstlink = self;
    self->m1 = self->m2 = NULL;
    self->numbufs = 0;
    self->cmark = 0;
    self->buf[0] = self->buf[1] = NULL;
    self->bname[0] = self->bname[1] = NULL;
    return TRUE;
}
void ezdiff__ObservedChanged(self, changed, value)
struct ezdiff *self;
struct observable *changed;
long value;
{
    /* If the buffer changes, Diff is probably wrong. So.. */
    if(value == observable_OBJECTDESTROYED){
	if(changed == (struct observable *) self->buf[0] )
	    self->buf[0] = NULL;
	else if(changed == (struct observable *) self->buf[1] )
	    self->buf[1] = NULL;
    }
    else {
	if(self->bname[0] && self->bname[1] && 
	   self->buf[0] && self->buf[1] &&
	   strcmp(buffer_GetName(self->buf[0]), self->bname[0]) == 0 &&
	   strcmp(buffer_GetName(self->buf[1]), self->bname[1]) == 0)
	    /* probably an unimportant change to the buffer, ignore */
	    return;
    }
    message_DisplayString(NULL,0,"Diff buffer being modified. Destroying diff");
    ezdiff_Destroy(self);
}
void ezdiff__FinalizeObject(ClassID,self)
struct classheader *ClassID;
struct ezdiff *self;
{
    if(self->buf[0] != NULL) buffer_RemoveObserver(self->buf[0],self);
    if(self->buf[1] != NULL) buffer_RemoveObserver(self->buf[1],self);

    if(self == firstlink) {
	firstlink = self->next;
	if(self == lastlink) lastlink = NULL;
    }
    else {
	struct ezdiff *ep;
	for(ep = firstlink; ep->next != NULL; ep = ep->next)
	    if(ep->next == self) break;
	ep->next = self->next;
	if(self == lastlink) lastlink = ep;
    }
    if(self->m1 != NULL) free((char *)self->m1);
    if(self->m2 != NULL) free((char *)self->m2);
    if(self->bname[0]) free(self->bname[0]);
    if(self->bname[1]) free(self->bname[1]);

}
static int LocateInView(v1,v2,v3,dat)
struct view *v1,*v2,*v3;
long dat;
{
    struct diffinfo *d = (struct diffinfo *) dat;
    struct mark *m = d->m;
    if(class_IsTypeByName(class_GetTypeName(v2),"textview")){
	struct textview *tv = (struct textview *)v2;
	textview_SetDotPosition(tv,mark_GetPos(m));
	textview_SetDotLength(tv,mark_GetLength(m));
	textview_FrameDot(tv,mark_GetPos(m));
	message_DisplayString(tv,0,d->buf);
    }
    return 0; /* go through all views */
}
static ezdiff_PointOut(self,v,delta)
struct ezdiff *self;
struct textview *v;
long delta;
{
    static struct diffinfo d1;
    if(self == NULL){
	if((self = FindViewDiff(v,NULL)) == NULL){
	    message_DisplayString(v,0,"No diff currently associated with this buffer");
	    return ;
	}
    }
    if(delta < 0 && delta != USECURRENTMARK){
	message_DisplayString(v,0,"No previous file differences");
	return ;
    }
    if(delta >= self->nummarks){
	message_DisplayString(v,0,"No subsequent file differences");
	return ;
    }
    if(delta != USECURRENTMARK) self->cmark = delta;
    d1.m = self->m1[self->cmark];
    sprintf(d1.buf,"diff %d of %d",self->cmark + 1,self->nummarks);
    buffer_EnumerateViews(self->buf[0], LocateInView, (long) &d1);
    d1.m = self->m2[self->cmark];
    buffer_EnumerateViews(self->buf[1], LocateInView, (long) &d1);
}
static ezdiff_Current(v,delta)
struct textview *v;
long delta;
{
    ezdiff_PointOut(NULL,v,USECURRENTMARK);
}
static ezdiff_Next(v,delta)
struct textview *v;
long delta;
{
    int which,cmark,pos;
    struct ezdiff *self;
    struct mark **m;
    if((self = FindViewDiff(v,&which)) == NULL){
	message_DisplayString(v,0,"No diff associated with this file");
	return ;
    }
    pos = textview_GetDotPosition(v);
    if(which == 0) m = self->m1;
    else m = self->m2;
    for(cmark = 0; cmark < self->nummarks; cmark++){
	if(mark_GetPos(m[cmark]) > pos) break;
    }
    ezdiff_PointOut(self,v,cmark);
}
static ezdiff_Change(v,delta)
struct textview *v;
long delta;
{
    int which,cmark;
    long szz;
    struct ezdiff *self;
    struct mark *srcm,*dstm;
    struct text *dsttext,*srctext;
    char buf[32000];
    if((self = FindViewDiff(v,&which)) == NULL){
	message_DisplayString(v,0,"No diff associated with this file");
	return ;
    }
    cmark = self->cmark;
    if(which == 1) {
	srcm = self->m1[cmark];
	dstm = self->m2[cmark];
    }
    else{
	srcm = self->m2[cmark];
	dstm = self->m1[cmark];
    }
    srctext = (struct text *)  mark_GetObject(srcm);
    dsttext = (struct text *)  mark_GetObject(dstm);
    if((szz = mark_GetLength(srcm)) >= 32000) {
	message_DisplayString(v,100,"Mark too big to copy, Use Cut and Paste");
	return;
    }
    if(szz > 0){
	text_CopySubString(srctext,mark_GetPos(srcm),szz,buf,FALSE);
    }
    text_AlwaysReplaceCharacters(dsttext,mark_GetPos(dstm) ,mark_GetLength(dstm),buf,szz);
    text_NotifyObservers(dsttext,1);
    ezdiff_PointOut(NULL,v,USECURRENTMARK);
}
static ezdiff_Last(v,delta)
struct textview *v;
long delta;
{
    int which,cmark,pos;
    struct ezdiff *self;
    struct mark **m;
    if((self = FindViewDiff(v,&which)) == NULL){
	message_DisplayString(v,0,"No diff associated with this file");
	return ;
    }
    pos = textview_GetDotPosition((struct textview *)v);
    if(which == 0) m = self->m1;
    else m = self->m2;
    for(cmark = 0; cmark < self->nummarks; cmark++){
	if(mark_GetPos(m[cmark]) >= pos ) break;
    }
    cmark--;
    ezdiff_PointOut(self,v,cmark);
}

static ezdiff_Start(v,dat)
struct view *v;
long dat;
{
    struct ezdiff *self;
    char ans[100];
    boolean ib = TRUE;
    struct buffer *NotedBuf;
    struct buffer *bb;
    if((bb =  buffer_FindBufferByData(ObjectOf(v))) == NULL) return;
    if(!class_IsTypeByName(class_GetTypeName(v),"textview")){
	message_DisplayString(v, 0, "Diff works on text files only");
	return;
    }
    if((self = FindBufferDiff(bb,NULL)) != NULL){
	if(message_AskForString(v,0, "Diff Exists. Reinitialize?[n]",0,ans,sizeof(ans)) < 0 || (*ans != 'y' && *ans != 'Y') ) {
	    message_DisplayString(v, 0, "Punt!");
	    return;
	}
	ezdiff_Destroy(self);
    }
    if(*buffername) NotedBuf = buffer_FindBufferByName(buffername);
    else NotedBuf = NULL;
    if(NotedBuf == NULL){
	/* we save the buffer name instead of a pointer to the buffer itself.
	 This protects us in the case where a selected buffer is destroyed */
	strncpy(buffername,buffer_GetName(bb),255);
	message_DisplayString(v, 0, "Buffer Noted. Start in second buffer to Initialize");
    }
    else{
	if(bb == NotedBuf){
	    message_DisplayString(v, 0, "Can't Diff a buffer with itself.");
	    *buffername = '\0';
	    return;
	}
	if(environ_GetProfileSwitch("AlwaysIgnoreBlanks",FALSE) == FALSE){
	if(message_AskForString(v,0, "Ignore Blanks?[n]",0,ans,sizeof(ans)) < 0 || (*ans != 'y' && *ans != 'Y') ) 
		ib = FALSE;
	}
	message_DisplayString(v, 0, "Running Diff. Please Wait...");
	while(im_Interact(0));
	if(ezdiff_Create(NotedBuf,bb,ib)!= NULL){
	    message_DisplayString(v, 0, "Diff Completed");
	    ezdiff_PointOut(NULL,(struct textview *)v,USECURRENTMARK);
	}
	*buffername = '\0';
    }
}
boolean ezdiff__InitializeClass(ClassID)
struct classheader *ClassID;
{
    struct classinfo *textviewtype = class_Load("textview");

    firstlink = lastlink = NULL;
    *buffername = '\0';
    proctable_DefineProc("ezdiff-start",ezdiff_Start,textviewtype,NULL,"Start diff process");
    proctable_DefineProc("ezdiff-current",ezdiff_Current,textviewtype,NULL,"Get Current Diff");
    proctable_DefineProc("ezdiff-next",ezdiff_Next,textviewtype,NULL,"Get Next Diff");
    proctable_DefineProc("ezdiff-last",ezdiff_Last,textviewtype,NULL,"Get Last Diff");
    proctable_DefineProc("ezdiff-change",ezdiff_Change,textviewtype,NULL,"Change to other mark");
    WaitCursor = cursor_Create(NULL);
    if(WaitCursor) cursor_SetStandard(WaitCursor,Cursor_Wait);
    return TRUE;
}


