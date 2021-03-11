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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/tm/RCS/tm.c,v 2.42 1994/05/19 17:13:33 rr2b Exp $";
#endif


 

#include <ctype.h>
#include <andrewos.h> /* sys/types.h sys/file.h */
#include <sys/ioctl.h>
#include <sys/signal.h>
#include <sys/errno.h>
#ifndef hp9000s300
#include <sys/wait.h>
#endif /* hp9000s300 */
#ifndef O_NDELAY
#include <fcntl.h>
#endif /* O_NDELAY */
#ifdef SOLARIS
#include <sys/filio.h>
#endif

#include <class.h>

#include <tm.eh>

#include <mark.ih>
#include <im.ih>
#include <environ.ih>
#include <filetype.ih>
#include <stylesht.ih>
#include <envrment.ih>

#include <attribs.h>

#ifdef hpux
#include <sgtty.h>
#include <sys/ptyio.h>
#define CBREAK 0
#endif /* hpux */

#ifdef linux
#include <bsd/sgtty.h>
#endif

#if defined(bsdi)
#include <sys/ioctl_compat.h>
#endif

#ifdef M_UNIX
#include <sys/termio.h>
#include <sys/stream.h>
#include <sys/ptem.h>
#include <sys/ttold.h>
#define CRMOD O_CRMOD
#define RAW O_RAW
#define CBREAK O_CBREAK
#endif


extern int errno;
extern char *sys_errlst[];

/* ---------------------------------------------------------------- */

#define scrPos(self,x,y) \
 ((self)->screen+(y)*((self)->width+1)+(x))

static char spaces[]="                                                                                ";
#define SPACES (sizeof(spaces)-1)

static int fillChars(self,pos,num)
struct termulator *self;
long pos,num;
{
    while(num>0){
	int inserted=(num>SPACES ? SPACES : num);
	super_AlwaysInsertCharacters(self,pos,spaces,inserted);
	num-=inserted;
	pos+=inserted;
    }

    return pos+num;
}

static long fillLines(self,pos,width,height)
struct termulator *self;
long pos;
int width,height;
{
    long fence=termulator_GetFence(self),oldpos=pos;

    while(height--){
	pos=fillChars(self,pos,width);
	super_AlwaysInsertCharacters(self,pos++,"\n",1);
    }

    if(fence==oldpos)
	termulator_SetFence(self,pos);

    return pos;
}

static long stripEnds(self,num)
struct termulator *self;
long num;
{
    long pos=scrPos(self,0,0)+self->width;

    while(num--){
	long end=pos;
	while(pos>0 && termulator_GetChar(self,pos-1)==' ')
	    pos--;
	if(pos<end)
	    termulator_AlwaysDeleteCharacters(self,pos,end-pos);
	pos+=1+self->width;
    }

    return pos-self->width;
}

void termulator__SetScreenSize(self,width,height)
struct termulator *self;
int width,height;
{
    if(self->screen>=0){
	int dw=width-self->width;
	int dh=height-self->height;

	if(dh>0)
	    /* chop off bottom of too-high screen */
	    termulator_AlwaysDeleteCharacters(self,
					      scrPos(self,0,self->height),
					      dh*(self->width+1));
	else if(dh<0){
	    /* add lines to a too-short screen */
	    fillLines(self,
		      scrPos(self,0,self->height),
		      width,
		      height-self->height);
	}

	if(dw>0){
	    /* chop off all too-long lines */
	    long pos=scrPos(self,self->width,0);
	    long count=self->height-(dh>0?dh:0);

	    while(count--){
		termulator_AlwaysDeleteCharacters(self,pos,dw);
		pos+=1+width;
	    }
	}else if(dw<0){
	    /* extend all too-short lines */
	    long pos=scrPos(self,self->width,0);
	    long count=self->height-(dh>0?dh:0);

	    while(count--)
		pos=fillChars(self,pos,-dw)+1+width;
	}
    }

    self->width=width;
    self->height=height;
}

static boolean termulate(self)
struct termulator *self;
{
    if(self->screen<0){
	long bpos=self->lastSubmit,
	    pos=termulator_GetFence(self);
	int ht=self->height,wid,
	    c;

	while(ht-->0 && pos>bpos){
	    while(pos>bpos &&
		  termulator_GetChar(self,pos--)!='\n')
		;
	}

	self->screen=pos;

	self->screenEnv=
	  environment_InsertStyle(self->header.text.rootEnvironment,
			     pos,
			     self->screenStyle,
			     TRUE);
	environment_SetLength(self->screenEnv,termulator_GetFence(self)-pos);
	environment_SetStyle(self->screenEnv,TRUE,TRUE);

	ht=self->height;

	/* make a screen */
	while(ht-->0){
	    wid=self->width;

	    while((c=termulator_GetChar(self,pos))!='\n' &&
		  c!=EOF &&
		  wid-->0)
		pos++;

	    if(wid>0)
		pos=fillChars(self,pos,wid);
	    else if(wid<0){
		long end=pos;
		while((c=termulator_GetChar(self,end))!='\n' && c!=EOF)
		    end++;
		termulator_AlwaysDeleteCharacters(self,pos,end-pos);
	    }

	    if(c==EOF)
		super_AlwaysInsertCharacters(self,pos,"\n",1);

	    pos++;
	}

	termulator_SetFence(self,scrPos(self,0,self->height));

	self->x=0;
	self->y=0;
    }

    return TRUE;
}  

void termulator__Untermulate(self)
struct termulator *self;
{
    if(self->screen>=0){
	long pos,fence;

	termulator_SetFence(self,scrPos(self,0,self->height));

	/* strip off blanks on every line above us.... */
	pos=stripEnds(self,self->y)+self->x;

	/* now get rid of cruft */
	fence=termulator_GetFence(self);
	if(pos<fence)
	    termulator_AlwaysDeleteCharacters(self,pos,fence-pos);

	environment_SetStyle(self->screenEnv,FALSE,FALSE);

	self->cursor=pos;
	self->screen= -1;
    }
}

void termulator__GotoXY(self,x,y)
struct termulator *self;
int x,y;
{
    if(x<0 || y<0 || x>=self->width || y>=self->height)
	return;

    if(self->screen<0 &&
       ((y==0 && x==0 && self->cursor==self->lastSubmit) ||
	!termulate(self)))
	return;

    self->cursor=scrPos(self,x,y);
    self->x=x;
    self->y=y;
}

void termulator__Backspace(self,num)
struct termulator *self;
long num;
{
    if(num>self->x)
	num=self->x;

    if(num>0){
	self->x-=num;
	self->cursor-=num;
    }
}

void termulator__CR(self)
struct termulator *self;
{
    self->cursor-=self->x;
    self->x=0;
}

void termulator__Tab(self)
struct termulator *self;
{
    int newx=(self->x+8)&~7;
    long pos=self->cursor,
        fence=termulator_GetFence(self);

    if(self->screen<0){
	int c=EOF;

	while(pos<fence && self->x<newx && (c=termulator_GetChar(self,pos))!='\n' && c!=EOF){
	    pos++;
	    self->x++;
	}

	if(self->x<newx)
	    super_AlwaysInsertCharacters(self,pos,"        ",newx-self->x);
    }else if(newx>=self->width)
	newx=self->width-1;

    pos+=newx-self->x;
    if(pos>fence)
	termulator_SetFence(self,pos);

    self->cursor=pos;
    self->x=newx;
}

void termulator__Newline(self,num)
struct termulator *self;
long num;
{
    if(self->screen>=0){
	if((self->mode&(CRMOD|RAW))==CRMOD) /* \n -> \r\n */
	    self->x=0;
	self->y+=num;
	if(self->y>=self->height)
	    termulator_Scroll(self,self->height-self->y+1);
	else
	    self->cursor=scrPos(self,self->x,self->y);
    }else{
	long pos=self->cursor,
	    fence=termulator_GetFence(self);
	int c=EOF;

	do
	    while(pos<fence &&
		  (c=termulator_GetChar(self,pos))!='\n' && c!=EOF)
		pos++;
	while(c=='\n' && --num>0);

	while(num-->0)
	    super_AlwaysInsertCharacters(self,pos,"\n",1);

	self->cursor= ++pos;

	/* I suppose it should really space over to the right
	 * column in raw mode, but oh well...
	 */
	if(pos>fence)
	    termulator_SetFence(self,pos);

	self->x=0;
    }
}

/* this works either in normal or full-screen mode
  * chars shouldn't include any "special" characters, like newline or tab
 */
void termulator__WriteChars(self,chars,len)
register struct termulator *self;
char *chars;
long len;
{
    long pos=self->cursor,
        fence=termulator_GetFence(self),
        deleted;
    struct environment *env = NULL;

    if(self->screen<0){
	if(!self->insert){
	    /* not guaranteed that len characters exist */
	    register long end;
	    register int c;

	    for(end=pos; end-pos<len && end<fence; end++){
		c=termulator_GetChar(self,end);
		if(c==EOF || c=='\n')
		    break;
	    }

	    deleted=end-pos;
	    if(deleted>0)
		termulator_AlwaysDeleteCharacters(self,pos,deleted);
	}else
	    deleted=0;
    }else{
	if(len+self->x>=self->width)
	    len=self->width-self->x;

	if(self->insert)
	    termulator_AlwaysDeleteCharacters(self,
					   pos+
					  (self->width-self->x)-
					      len,
					      len);
	else
	    termulator_AlwaysDeleteCharacters(self,pos,len);

	deleted=len;
    }
 
    /* Standout mode is NOT HANDLED CORRECTLY here.  It will not un-highlight
     * something written over the middle of a highlighted region.
     * Fortunately this doesn't happen too often.  To do it correctly
     * wouldn't be too hard.
     */
    if(self->standout){
	env=environment_GetInnerMost(self->header.text.rootEnvironment,pos);
	if(env==self->screenEnv){
	    env=
	      environment_InsertStyle(env,
				 pos-self->screen,
				 self->soStyle,
				 TRUE);
	    environment_SetLength(env,0);
	}else if(env==self->header.text.rootEnvironment){
	    env=environment_InsertStyle(env,pos,self->soStyle,TRUE);
	    environment_SetLength(env,0);
	}
	environment_SetStyle(env,TRUE,TRUE);
    }

    super_AlwaysInsertCharacters(self,pos,chars,len);

    if(self->standout)
	environment_SetStyle(env,FALSE,FALSE);

    self->x+=len;
    self->cursor=pos+len;

    fence-=deleted;
    if(pos==fence)
	termulator_SetFence(self,fence+len);
}

void termulator__InsertChars(self,num)
struct termulator *self;
long num;
{
    long pos=self->cursor;

    if(self->screen>=0){
	if(self->x+num>self->width)
	    num=self->width-self->x;

	termulator_AlwaysDeleteCharacters(self,pos+num,(self->width-self->x)-num);
    }

    fillChars(self,pos,num);
}

void termulator__DeleteChars(self,num)
struct termulator *self;
long num;
{
    long pos=self->cursor,
        fence=termulator_GetFence(self);

    if(self->screen<0){
	/* not guaranteed that len characters exist */
	long end;
	int c;

	for(end=pos; end-pos<num && end<fence; end++){
	    c=termulator_GetChar(self,end);
	    if(c==EOF || c=='\n')
		break;
	}

	termulator_AlwaysDeleteCharacters(self,pos,end-pos);
    }else{
	if(self->x+num>self->width)
	    num=self->width-self->x;

	termulator_AlwaysDeleteCharacters(self,pos,num);
	fillChars(self,pos+(self->width-self->x)-num,num);
    }
}

void termulator__ClearChars(self,x,num)
struct termulator *self;
int x;
long num;
{
    long pos=self->cursor,
        fence=termulator_GetFence(self);

    if(self->screen<0){
	/* not guaranteed that len characters exist */
	long end;
	int c;

	for(end=pos; end-pos<num && end<fence; end++){
	    c=termulator_GetChar(self,end);
	    if(c==EOF || c=='\n')
		break;
	}

	if(end>fence)
	    end=fence;

	termulator_AlwaysDeleteCharacters(self,pos,end-pos);
	if(end-pos==num)
	    fillChars(self,pos,num);
    }else{
	if(self->x+num>self->width)
	    num=self->width-self->x;

	termulator_AlwaysDeleteCharacters(self,pos,num);
	fillChars(self,pos,num);
    }
}

void termulator__InsertLines(self,num)
struct termulator *self;
long num;
{
    long newpos;

    if(self->screen<0 && !termulate(self))
	return;

    newpos=scrPos(self,0,self->y);
    fillLines(self,newpos,self->width,num);
    termulator_AlwaysDeleteCharacters(self,
				       scrPos(self,0,self->height),
				       num*(self->width+1));
    self->x=0;
    self->cursor=newpos;
}

void termulator__DeleteLines(self,num)
struct termulator *self;
long num;
{
    long newpos;

    if(self->screen<0 && !termulate(self))
	return;

    newpos=scrPos(self,0,self->y);
    fillLines(self,scrPos(self,0,self->height),self->width,num);
    termulator_AlwaysDeleteCharacters(self,
				      newpos,
				      num*(self->width+1));
    self->x=0;
    self->cursor=newpos;
}

void termulator__ClearLines(self,y,num)
struct termulator *self;
int y;
long num;
{
    long newpos,numchars;

    if(self->screen<0 && !termulate(self))
	return;

    newpos=scrPos(self,0,y);
    numchars=num*(self->width+1);

    /* do the fillLines first, otherwise the screenEnv gets nuked */
    fillLines(self,newpos,self->width,num);
    termulator_AlwaysDeleteCharacters(self,newpos+numchars,numchars);

    self->cursor=scrPos(self,self->x,self->y);
}

void termulator__Scroll(self,num)
struct termulator *self;
long num;
{
    if(num>=self->height){
	/* this in effect clears the screen */
	termulator_ClearLines(self,0,self->height);
	return;
    }	  

    fillLines(self,scrPos(self,0,self->height),self->width,num);

    /* let scrolled-off-top lines remain... */
    self->screen=stripEnds(self,num);
    
    self->x=0;
    self->y=self->height-1;
    self->cursor=scrPos(self,0,self->height-1);
}

/* ---------------------------------------------------------------- */

#define SYSERROR(str) \
   (self->errstr=str,self->errno=errno,termulator_NotifyObservers(self,0))
#define ERROR(str) \
   (self->errstr=str,self->errno=0,termulator_NotifyObservers(self,0))

static void readFromProc(fp,self)
FILE *fp;
struct termulator *self;
{
    int pty=fileno(fp);
    int len;
    struct sgttyb tty;
    char buf[4000];

    len=read(pty,buf,sizeof(buf));

    if(len==0){
	im_RemoveFileHandler(fp);
	fclose(fp);
	self->ptyFile=NULL;
	ERROR("EOF from child.");
	return;
    }else if(len==-1){
	if(errno!=EINTR){
	    im_RemoveFileHandler(fp);
	    fclose(fp);
	    self->ptyFile=NULL;
	    ERROR("Lost connection with child.");
	}	    
	return;
    }

    ioctl(pty,TIOCGETP,&tty);
    self->mode=tty.sg_flags;

    termulator_ProcessOutput(self,buf,len);

    termulator_NotifyObservers(self,0);
}

static char *ignoreChar(self,buf,len)
struct termulator *self;
char *buf;
long len;
{
    return buf+1;
}

static char *cntlChar(self,buf,len)
struct termulator *self;
char *buf;
long len;
{
    static char rep[2]="^";

    rep[1]=(*buf+'@');
    termulator_WriteChars(self,rep,2);
    return buf+1;
}

/* used for some common occurances */
static char *specialChar(self,buf,len)
struct termulator *self;
char *buf;
register long len;
{
    int c= *buf,num=1;

    while(*++buf==c && --len>0)
	num++;

    switch(c){
	case '\r':
	    termulator_CR(self);
	    break;
	case '\b':
	    termulator_Backspace(self,num);
	    break;
	case '\n':
	    termulator_Newline(self,num);
	    break;
	case '\t':
	    while(num--)
		termulator_Tab(self);
	    break;
    }

    return buf;
}

/* read in directory string */
static char *readDir(self,buf,len)
struct termulator *self;
char *buf;
register long len;
{
    char *dir=self->parseBuf+self->parseLen;
    int left=sizeof(self->parseBuf)-self->parseLen;
    char *origbuf = buf;

    if(*buf=='\001')
	buf++;

    while(len-->0 && *buf!='\002' && left>0){
     	if(!isprint((unsigned char)*buf)) {
 	    /*
 	     * Unlikely character, assume he's not really calling
 	     * readDir.
 	     */
 	    termulator_WriteChars(self, origbuf, 1);
 	    return origbuf+1;
 	}
	*dir++= *buf++;
	self->parseLen++;
	left--;
    }

    if(len>0){
	char *home=environ_Get("HOME");

	if(self->cwd!=NULL)
	    free(self->cwd);

	*dir++='\0';
	filetype_CanonicalizeFilename(self->parseBuf,
				      self->parseBuf,
				      sizeof(self->parseBuf));
	if(home!=NULL){
	    long hlen=strlen(home);
	    if(strncmp(self->parseBuf,home,hlen)==0){
		char *p=self->parseBuf+hlen,*q=self->parseBuf+1;
		*self->parseBuf='~';
		do
		    *q++= *p++;
		while(*(p-1)!='\0');
	    }
	}

	self->cwd=malloc(strlen(self->parseBuf)+1);
	strcpy(self->cwd,self->parseBuf);

	self->parseLen=0;

	self->state=NULL;

	return buf+1;
    }else{
	self->state=readDir;
	return buf;
    }
}

void termulator__SetDispSize(self,width,height)
struct termulator *self;
int width,height;
{
#ifdef TIOCSWINSZ
    int mw,mh;

    if(self->dispHeight==height && self->dispWidth==width)
	return;

    if(self->width<width)
	mw=width;
    else
	mw=self->width;
    if(self->height<height)
	mh=height;
    else
	mh=self->height;

    if(self->width<mw || self->height<mh){
	termulator_SetScreenSize(self,mw,mh);
	self->width=mw;
	self->height=mh;
    }

#ifdef M_UNIX
    {
	char buf[80];
	sprintf(buf,"LINES=%d",mh);
	putenv(buf);
	sprintf(buf,"COLS=%d",mw);
	putenv(buf);
    }
#else
    if(self->ptyFile!=NULL){
	struct winsize ws;
	ws.ws_row=height;
	ws.ws_col=width;
	if(ioctl(fileno(self->ptyFile),TIOCSWINSZ,&ws)<0)
	    SYSERROR("SetDispSize(ioctl)");
    }
#endif

    self->dispWidth=width;
    self->dispHeight=height;
#endif /* TIOCSWINSZ */
}

/* the state machine for interpretting output */
void termulator__ProcessOutput(self,buf,len)
struct termulator *self;
char *buf;
register long len;
{
    register char *(**escapes)()=self->escapes;
    register char *end=buf;

    while(len>0){
	register char *(*action)()=self->state;

	if(action==NULL){
	    do{
		action=escapes[(unsigned char)*end];
		if(action!=NULL)
		    break;
		end++;
	    }while(--len>0);
	    
	    if(end>buf){
		termulator_WriteChars(self,buf,end-buf);
		buf=end;
	    }
	}

	if(len>0){
	    end=(*action)(self,buf,len);
	    len-=(end-buf);
	    buf=end;
	}
    }
}

static void setupStyles(self)
struct termulator *self;
{
    termulator_SetGlobalStyle(self,
			      stylesheet_Find(self->header.text.styleSheet, "global"));
    self->cmdStyle=
      stylesheet_Find(self->header.text.styleSheet,"command");
    self->noechoStyle=
      stylesheet_Find(self->header.text.styleSheet,"noechocmd");
    self->soStyle=
      stylesheet_Find(self->header.text.styleSheet,"standout");
    self->screenStyle=
      stylesheet_Find(self->header.text.styleSheet,"screen");
}

void termulator__Clear(self)
struct termulator *self;
{
    long pos=termulator_GetLength(self);

    if(self->screen>=0)
	pos=self->screen;
    else{
	while(pos>0 && termulator_GetChar(self,pos)!='\n')
	    pos--;
	pos++;
    }

    termulator_AlwaysDeleteCharacters(self,0,pos);

    self->cursor-=pos;
    self->lastSubmit-=pos;
    if(self->screen>0)
	self->screen-=pos;

    termulator_NotifyObservers(self,0);
}

boolean termulator__InitializeObject(classID,self)
struct classheader *classID;
struct termulator *self;
{
    struct attributes templateAttribute;
    int i;

#ifndef hp9000s300
    (void)signal(SIGTTOU,SIG_IGN);
#endif /* hp9000s300 */

    self->screen= -1;
    self->cursor=0;
    self->lastSubmit=0;
    self->mode=0;
    self->insert=FALSE;
    self->standout=FALSE;
    self->width=80;
    self->height=24;
#ifdef TIOCSWINSZ
    self->dispWidth=self->width;
    self->dispHeight=self->height;
#endif /* TIOCSWINSZ */
    self->inpView=NULL;
    self->errstr=NULL;
    self->x=self->y=0;
    self->parseLen=0;
#ifdef HACKEDNOECHO
    self->inpLen=0;
#endif /* HACKEDNOECHO */
    self->cwd=NULL;
    self->state=NULL;
    self->args=NULL;
    self->screenEnv=NULL;
    self->cmdEnv=NULL;

    self->ptyFile=NULL;
    self->pid=0;

    self->maxsize = environ_GetProfileInt("maxsize", 10000);
    self->extraroom = self->maxsize / 5;

    for(i=0;i<' ';i++)
	self->escapes[i]=cntlChar;
    for(i=' ';i<256;i++)
	self->escapes[i]=NULL;
    self->escapes['\t']=specialChar;
    self->escapes['\n']=specialChar;
    self->escapes['\b']=specialChar;
    self->escapes['\r']=specialChar;
    self->escapes['\001']=readDir;
    self->escapes['\000']=ignoreChar;
    self->escapes['\007']=ignoreChar;
    self->escapes['\177']=ignoreChar;

    self->lastCmd=NULL;
    termulator_EnterCmd(self,NULL); /* initialize command stack */

    termulator_SetExportEnvironments(self,FALSE);

    templateAttribute.key = "template";
    templateAttribute.value.string = "tm";
    templateAttribute.next = NULL;

    self->currentlyReadingTemplateKluge=TRUE;
    termulator_SetAttributes(self,&templateAttribute);
    self->currentlyReadingTemplateKluge=FALSE;

    setupStyles(self);

    return TRUE;
}

void termulator__FinalizeObject(classID,self)
struct classheader *classID;
struct termulator *self;
{
    if(self->pid!=0){
	im_RemoveZombieHandler(self->pid);
	kill(self->pid,SIGHUP);
    }
    if(self->ptyFile!=NULL)
	im_RemoveFileHandler(self->ptyFile);
}

static void childDied(pid,self,status)
int pid;
struct termulator *self;
#ifdef hp9000s300
int *status;
#else /* hp9000s300 */
union wait *status;
#endif /* hp9000s300 */
{
    static char buf[40];

    strcpy(buf,"Child ");
    statustostr(status,buf+6,34);

    if(self->ptyFile!=NULL){
	im_RemoveFileHandler(self->ptyFile);
	fclose(self->ptyFile);
	self->ptyFile=NULL;
    }

    self->pid=0;
    self->mode=0;

    ERROR(buf);
}

static int ON=1;
/* Does ioctl take a pointer arg? */

#if defined(sys_sun4_41) || defined(hpux) || defined(_IBMR2) || defined(NeXT) || defined(sys_pmax_ul4) || defined(sys_dec_alpha)

#define ENTER_TIOCREMOTE(self) \
(self->remote || \
  (ioctl(fileno(self->ptyFile),TIOCREMOTE,&ON) != -1 && \
  (/*fprintf(stderr,"Into TIOCREMOTE mode.\n"),*/self->remote++)))

#ifndef INCORRECTSIGNALS
static int OFF=0;
#define LEAVE_TIOCREMOTE(self) \
(!self->remote || \
 (ioctl(fileno(self->ptyFile),TIOCREMOTE, &OFF) != -1 && \
  (/*fprintf(stderr,"Out of TIOCREMOTE mode.\n"),*/self->remote--,TRUE)))
#endif /* INCORRECTSIGNALS */

#else /* !defined(hpux) && !defined(_IBMR2) && !defined(NeXT) && !defined(sys_pmax_ul4) */
/* integer arg3 to ioctl */

#if !defined(M_UNIX) && !defined(linux)
#define ENTER_TIOCREMOTE(self) \
(self->remote || \
 (ioctl(fileno(self->ptyFile),TIOCREMOTE,1) != -1 && \
  (/*fprintf(stderr,"Into TIOCREMOTE mode.\n"),*/self->remote++)))

#ifndef INCORRECTSIGNALS
static int OFF=0;
#define LEAVE_TIOCREMOTE(self) \
(!self->remote || \
 (ioctl(fileno(self->ptyFile),TIOCREMOTE, 0) != -1 && \
  (/*fprintf(stderr,"Out of TIOCREMOTE mode.\n"),*/self->remote--,TRUE)))
#endif /* INCORRECTSIGNALS */

#else /* M_UNIX */
#define ENTER_TIOCREMOTE(self)

#ifndef INCORRECTSIGNALS
static int OFF=0;
#define LEAVE_TIOCREMOTE(self)
#endif /* INCORRECTSIGNALS */
#endif /*M_UNIX */
#endif /* defined(hpux) || defined(_IBMR2) || defined(NeXT) || defined(sys_pmax_ul4) */

int termulator__StartProcess(self,args)
struct termulator *self;
char **args;
{
    int     pid/*,ppid=getpid()*/;
    int     pty, slave;
#if POSIX_ENV
    int pgrp = getpgrp();
#else
    int pgrp = getpgrp(0);
#endif
    struct sgttyb tty;
#ifdef TIOCSWINSZ
    struct winsize ws;
#endif /* TIOCSWINSZ */
    char ptyname[64];
 
    if(args==NULL)
	return -1;
/*
    {
	fprintf(stderr,"pid: %d, pgrp: %d\n",getpid(),getpgrp(0));
    }
*/
#if !(defined(POSIX_ENV) || defined(hpux))
    {
	int fd=open("/dev/tty", 2);
	if(fd>=0){
	    ioctl(fd,TIOCNOTTY,0);
	    close(fd);
	}else
	    setpgrp(0,getpid());
    }
#endif /* hpux */

    if(!GetPtyandName(&pty, &slave, ptyname, sizeof(ptyname))) {
	ERROR("Could not open any pty's");
	return -1;
    }

    self->ptyFile=fdopen(pty,"rw");

    if(self->ptyFile==NULL){
	ERROR("fdopen failed!?!");
	return -1;
    }

    self->remote=0;

    ENTER_TIOCREMOTE(self);

    ioctl(pty,FIOCLEX,0);

#ifdef TIOCSWINSZ
    ws.ws_row=self->dispHeight;
    ws.ws_col=self->dispWidth;
    ioctl(pty,TIOCSWINSZ,&ws);
#endif /* TIOCSWINSZ */

    if((pid = osi_vfork()) < 0){
	SYSERROR("startSubProc(vfork)");
	return errno;
    }

    if(pid == 0){
	int pid;
#ifdef POSIX_ENV
	int ptyChannel;

        /* Become a session leader. */
        if(setsid() < 0)
            perror("setsid");
        /* Re-open the pty so that it becomes the controlling terminal.
         * NOTE:  GetPtyandName must open the pty with the O_NOCTTY flag,
         * otherwise the parent (typescript) process will have the controlling
         * terminal and we (the shell) won't be able to get it.
         */
        if((ptyChannel = open(ptyname, O_RDWR)) < 0) {
             fprintf(stderr, "Could not open %s.\n", ptyname);
             exit(1);
        }
        close(ptyChannel);
#else
#ifdef hpux
	setpgrp();
#endif /* hpux */
#endif /* !POSIX_ENV */
	pid=getpid();
	close(pty);
	close(0);
	close(1);
	close(2); 
	dup2(slave, 0);
	close(slave);
#ifdef POSIX_ENV
	/* Set current terminal process group. */
	if(tcsetpgrp(0, pid) < 0)
	    perror("tcsetpgrp failed");
#else
#ifndef hpux
	ioctl(0,TIOCSPGRP,&pid);
	setpgrp(0,pid);
#endif /* hpux */
#endif /* POSIX_ENV */
	dup(0);
	dup(0);
	environ_Put("TERM",termulator_GetTerm(self));
	environ_Put("TERMCAP",termulator_GetTermcap(self));

	/* kernel doesn't reset pty's */
	ioctl(0,TIOCGETP,&tty);
	tty.sg_flags|=CRMOD+ECHO;
	tty.sg_flags&=~(CBREAK|RAW);

	ioctl(0,TIOCSETP,&tty);

	execvp(args[0],args);

	/* totally lost it; can't do much else... */
	_exit(-1);
    }

    self->pid=pid;

    im_AddZombieHandler(pid,childDied,(long)self);
    im_AddFileHandler(self->ptyFile,readFromProc,self,0);
#if defined(POSIX_ENV) && !defined(bsdi)
    setpgrp();
#else
    setpgrp(0,pgrp);
#endif
    self->args=args;

    ioctl(pty,TIOCGETP,&tty);
    self->mode=tty.sg_flags;

    return 0;
}

char *termulator__ViewName(self)
struct termulator *self;
{
    return "tmview";
}

static char ctrlbuf[2]="^";
#define echoCTRL(self,c) \
  (ctrlbuf[1]=(((c)+'@')&0x7f),termulator_WriteChars(self,ctrlbuf,2))

void termulator__ProcessInput(self,buf,len)
struct termulator *self;
char *buf;
long len;
{
    register char *end=buf;
#ifdef INCORRECTSIGNALS
    if(self->mode&RAW)
#else /* INCORRECTSIGNALS */
    if(self->mode&(RAW|CBREAK))
#endif /* INCORRECTSIGNALS */
	termulator_SendInput(self,buf,len);
    else{
	while(len-->0)
	    switch(*end++){
		case '\r':
#ifdef INCORRECTSIGNALS
		    if(self->mode&CBREAK)
			end[-1]='\n';
		    else
#endif /* INCORRECTSIGNALS */
			termulator_Submit(self);
		    break;
#ifdef INCORRECTSIGNALS
		case INTRCHAR:
		case QUITCHAR:
#ifndef hp9000s300
		case STOPCHAR:
#endif /* hp9000s300 */
		    end--;
		    if(end>buf)
			termulator_SendInput(self,buf,end-buf);

#ifdef HACKEDNOECHO
		    self->inpLen=0; /* flush buffer */
#endif /* HACKEDNOECHO */

		    switch(*end){
			case INTRCHAR:
			    termulator_Signal(self,SIGINT); break;
			case QUITCHAR:
			    termulator_Signal(self,SIGQUIT); break;
#ifndef hp9000s300
			case STOPCHAR:
			    termulator_Signal(self,SIGTSTP); break;
#endif /* hp9000s300 */
		    }

		    if(self->mode&ECHO)
		        echoCTRL(self,*end);

		    buf=end+1;

		    break;
#endif /* INCORRECTSIGNALS */
	    }

	if(end>buf)
	    termulator_SendInput(self,buf,end-buf);
    }
}

void termulator__SendInput(self,buf,len)
struct termulator *self;
char *buf;
long len;
{
    long newlen;

    if(len==0)
	return;

#ifndef INCORRECTSIGNALS
    LEAVE_TIOCREMOTE(self);
#endif /* INCORRECTSIGNALS */
    newlen=write(fileno(self->ptyFile),buf,len);
    if(newlen<len)
	SYSERROR("SendInput(write)");

#ifdef INCORRECTSIGNALS
    if(self->mode&ECHO){
	if(self->mode&RAW)
	    termulator_ProcessOutput(self,buf,newlen);
	else{
	    char *end=buf;
	    while(newlen-->0){
		if(!isprint((unsigned char)*end)){
		    if(end>buf)
			termulator_WriteChars(self,
					      buf,
					      end-buf);
		    switch(*end){
			case '\n':
			    termulator_Newline(self,1);
			    break;
			case '\t':
			    termulator_Tab(self);
			    break;
			default:
			    echoCTRL(self,*end);
		    }
		    buf=end+1;
		}
		end++;
	    }
	    if(end>buf)
		termulator_WriteChars(self,buf,end-buf);
	}
	termulator_NotifyObservers(self,0);
    }
#endif /* INCORRECTSIGNALS */
}

/* this should only get called if self->screen is NULL */
void termulator__Submit(self)
struct termulator *self;
{
    int wrote;

    if(self->ptyFile==NULL){
	ERROR("No child process.");
	return;
    }

#ifndef INCORRECTSIGNALS
    ENTER_TIOCREMOTE(self);
#endif /* INCORRECTSIGNALS */

#ifdef HACKEDNOECHO
    if(self->mode&ECHO)
#endif /* HACKEDNOECHO */
    {
	char buf[5000],*p,*save;
	long pos=termulator_GetFence(self), fence=pos;
	int c;

	do{
	    long opos=pos;

	    p=buf;

	    while((c=termulator_GetChar(self,pos))!=EOF){
		pos++;
		if(c=='\n')
		    break;
		*p++=c;
	    }

	    *p++='\n';

	    wrote=write(fileno(self->ptyFile),buf,p-buf);

	    if(wrote==-1 && errno==EWOULDBLOCK){
		ERROR("Process not ready for input.");
		pos=opos;
		break;
	    }

	    if(wrote<p-buf){
		SYSERROR("Submit(write)");
		break;
	    }
	}while(c!=EOF);

	save=malloc(pos-fence+1);
	p=save;

	if(fence<pos){ /* some highlighted text exists */
#ifndef HACKEDNOECHO
	    if(!(self->mode&ECHO))
		termulator_AlwaysDeleteCharacters(self,fence,pos-fence);
	    else
#endif /* HACKEDNOECHO */
	    {
		while(fence<pos)
		    *p++=termulator_GetChar(self,fence++);
		*p='\0';
		if(!termulator_EnterCmd(self,save))
		    free(save); /* don't save after all */

		termulator_SetFence(self,pos);

		self->cursor=pos;

		environment_SetStyle(self->cmdEnv,FALSE,FALSE); /* leave out \n */
	    }
	}

	termulator_Newline(self,1);

	self->lastSubmit=termulator_GetFence(self);
    }
#ifdef HACKEDNOECHO
    else{
	self->inpBuf[self->inpLen++]='\n';
	wrote=write(fileno(self->ptyFile),self->inpBuf,self->inpLen);
	if(wrote<self->inpLen){
	    char *p=self->inpBuf;

	    SYSERROR("Submit(write)");
	    while(p+wrote<self->inpBuf+self->inpLen){
		*p= *(p+wrote);
		p++;
	    }
	}
	self->inpLen-=wrote;
    }
#endif /* HACKEDNOECHO */

    if(termulator_GetLength(self)>self->maxsize){
	long del = termulator_GetLength(self) - self->maxsize + self->extraroom;

	if(self->screen>=0 && self->screen<del)
	    del=self->screen;

	termulator_AlwaysDeleteCharacters(self,0,del);

	self->cursor-=del;
	self->lastSubmit-=del;
	if(self->screen>0)
	    self->screen-=del;
    }
}

static void preInsert(self,posP,fenceP)
struct termulator *self;
long *posP, *fenceP;
{
    *fenceP=termulator_GetFence(self);

    if(*posP>=*fenceP){
	/* Since cooked mode and termulator don't mix very well,
         * we keep switching back.  This could get sort of silly.
         */
	if(self->screen>=0){
	    long oldfence= *fenceP;
	    termulator_Untermulate(self);
	    *fenceP=termulator_GetFence(self);
	    *posP-=oldfence-*fenceP; /* most of screen probably went bye-bye */
	}

	if(*fenceP==termulator_GetLength(self)){
	    self->cmdEnv=
	      environment_InsertStyle(self->header.text.rootEnvironment,
				 *fenceP,
				 (self->mode&ECHO
				  ?self->cmdStyle
				  :self->noechoStyle),
				 TRUE);
	    environment_SetLength(self->cmdEnv,0);
	    environment_SetStyle(self->cmdEnv,FALSE,TRUE);
	}

	if(*fenceP==*posP)
	    environment_SetStyle(self->cmdEnv,TRUE,TRUE);
    }
}

static void postInsert(self,pos,fence)
struct termulator *self;
long pos,fence;
{
    if(termulator_GetLength(self)==fence)
	self->cmdEnv=NULL;
    else if(pos==fence)
	environment_SetStyle(self->cmdEnv,FALSE,TRUE);
}

long termulator__Read(self,fp,id)
struct termulator *self;
FILE *fp;
long id;
{
    char buf[1000];
    static char **argbuf[500];
    char **args,**strtoargv(),dummyS[100];
    int dummyI;

    if(self->currentlyReadingTemplateKluge)
	return super_Read(self,fp,id);

    if(fgets(buf,sizeof(buf),fp)==NULL)
	return dataobject_PREMATUREEOF;

    if(*buf=='\0')
	args=NULL;
    else
	args=strtoargv(buf,argbuf,sizeof(argbuf));

    if(fscanf(fp,"\\enddata{%[^,],%d}\n",dummyS,&dummyI)!=2)
	return dataobject_MISSINGENDDATAMARKER;

/* A statement here to start up termulator with the read args 
has been removed , as it represented a major security hole.  tpn 2/28/90 */

    return dataobject_NOREADERROR;
}

long termulator__Write(self,fp,writeID,level)
struct termulator *self;
FILE *fp;
long writeID;
int level;
{
    if(writeID!=termulator_GetWriteID(self)){ /* only write a given version once */
	char buf[1000];

	termulator_SetWriteID(self,writeID);

	fprintf(fp,"\\begindata{%s,%d}\n",
		class_GetTypeName(self), termulator_UniqueID(self));

	if(self->args==NULL)
	    *buf='\0';
	else
	    argvtostr(self->args,buf,sizeof(buf));

	fprintf(fp,"%s\n\\enddata{%s,%d}\n",
		buf,class_GetTypeName(self), termulator_UniqueID(self));
    }

    return termulator_UniqueID(self);
}

/* this routine in effect puts the bold cooked-mode characters at the end of the document */
void termulator__AlwaysInsertCharacters(self,pos,buf,len)
struct termulator *self;
long pos;
char *buf;
long len;
{
    long fence;

    preInsert(self,&pos,&fence);
    super_AlwaysInsertCharacters(self,pos,buf,len);
    postInsert(self,pos,fence);
}

/* need this for yanks */
long termulator__ReadSubString(self,pos,file,quoteCharacters)
struct termulator *self;
long pos;
FILE *file;
boolean quoteCharacters;
{
    long fence, rval;

    preInsert(self,&pos,&fence);
    rval=super_ReadSubString(self,pos,file,quoteCharacters);
    postInsert(self,pos,fence);

    return rval;
}

void termulator__EOT(self)
struct termulator *self;
{
#ifndef INCORRECTSIGNALS
    ENTER_TIOCREMOTE(self);
#endif /* INCORRECTSIGNALS */
    if(write(fileno(self->ptyFile),&self,0)<0)
	SYSERROR("EOT(write)");    
}

void termulator__Signal(self,signo)
struct termulator *self;
int signo;
{
    int pgrp;

#ifdef POSIX_ENV
    /*
     * NOTE: we assume here that if you have a POSIX system, you also
     * have an "ioctl" that lets you send a signal to the process
     * group of the slave side of the pty.  That is true for SunOS 4.1[.x]
     * (TIOCSIGNAL), System V Release 4 (TIOCSIGNAL, although it takes
     * the value of the signal, rather than a pointer to that value,
     * as an argument), 4.3-reno and presumably 4.4BSD (TIOCSIG), and,
     * I think, later versions of HP-UX.
     */
#ifdef TIOCSIGNAL
#if defined(sys_sun3_41) || defined(sys_sun4_41)
    /* SunOS 4.1[.x] */
    if(ioctl(fileno(self->ptyFile),TIOCSIGNAL,&signo)<0)
	SYSERROR("Signal(ioctl)");
#else /* defined(sys_sun3_41) || defined(sys_sun4_41) */
    /* SVR4 */
    if(ioctl(fileno(self->ptyFile),TIOCSIGNAL,signo)<0)
	SYSERROR("Signal(ioctl)");
#endif /* defined(sys_sun3_41) || defined(sys_sun4_41) */
#else /* TIOCSIGNAL */
#ifdef TIOCSIG
    if(ioctl(fileno(self->ptyFile),TIOCSIG,&signo)<0)
	SYSERROR("Signal(ioctl)");
#endif /* TIOCSIG */
#endif /* TIOCSIGNAL */
#else /* POSIX_ENV */
#ifdef hpux
    if (0) {}
#else /* hpux */
    if(ioctl(fileno(self->ptyFile),TIOCGPGRP,&pgrp)<0)
	SYSERROR("Signal(ioctl)");
#endif /* hpux */
    else if(killpg(pgrp,signo)==-1)
	SYSERROR("Signal(kill)");
#endif /* POSIX_ENV */
}

char *termulator__GetTerm(self)
struct termulator *self;
{
    return "wm";
}

char *termulator__GetTermcap(self)
struct termulator *self;
{
    return "wm|termulator dumb terminal:bs";
}

char *termulator__GrabPrevCmd(self,str)
struct termulator *self;
char *str;
{
    long slen=(str==NULL ? 0 : strlen(str));
    struct cmd *start=self->curCmd;

    while(self->curCmd!=NULL && self->curCmd->prev!=NULL){
	self->curCmd=self->curCmd->prev;
	if(str==NULL || strncmp(self->curCmd->string,str,slen)==0)
	    return self->curCmd->string;
    }

    self->curCmd=start;

    return NULL;
}

char *termulator__GrabNextCmd(self,str)
struct termulator *self;
char *str;
{
    long slen=(str==NULL ? 0 : strlen(str));
    struct cmd *start=self->curCmd;

    while(self->curCmd!=NULL && self->curCmd->next!=NULL){
	self->curCmd=self->curCmd->next;
	if(str==NULL || strncmp(self->curCmd->string,str,slen)==0)
	    return self->curCmd->string;
    }

    self->curCmd=start;

    return NULL;
}

void termulator__RewindCmds(self)
struct termulator *self;
{
    self->curCmd=self->lastCmd;
}

boolean termulator__EnterCmd(self,str)
struct termulator *self;
char *str;
{
    struct cmd *lc=self->lastCmd;    

    if(str!=NULL &&
       (strlen(str)==1 ||
	(lc!=NULL && lc->prev!=NULL && strcmp(str,lc->prev->string)==0))){
	self->curCmd=lc;
	return FALSE;
    }

    self->lastCmd=(struct cmd *)malloc(sizeof(struct cmd));
    self->lastCmd->string="";
    self->lastCmd->next=NULL;
    self->lastCmd->prev=lc;

    if(lc!=NULL){
	lc->next=self->lastCmd;
	lc->string=str;
    }

    self->curCmd=self->lastCmd;

    return TRUE;
}
