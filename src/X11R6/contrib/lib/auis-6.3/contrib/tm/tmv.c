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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/tm/RCS/tmv.c,v 2.14 1994/04/14 02:52:42 rr2b Exp $";
#endif


 

#include <andrewos.h>
#include <sys/signal.h>
#include <sys/errno.h>
#ifdef linux
#include <bsd/sgtty.h>
#else
#include <sgtty.h>
#endif

#include <class.h>
#include <ctype.h>

#include <tmv.eh>

#include <tm.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <proctbl.ih>
#include <message.ih>
#include <environ.ih>
#include <frame.ih>
#include <im.ih>
#include <mark.ih>
#include <envrment.ih>
#include <fontdesc.ih>
#include <text.ih>
#include <txtstvec.h>

#if defined(hpux) || defined(M_UNIX) || defined(SOLARIS)
#define CBREAK 0
#endif /* hpux */

extern int errno;
extern char *sys_errlist[];
char *argvtostr(),**strtoargv();

struct keymap *tmviewKeymap=NULL;
struct menulist *tmviewMenus=NULL;
struct proctable_Entry *rawKeyPE=NULL;

#define BORDERWIDTH 5

static void (*textview_BeginningOfLine)()=NULL;

void rawKey(self,key)
struct tmview *self;
long key;
{
    char buf=key;

    termulator_ProcessInput((struct termulator *)self->header.view.dataobject,
			      &buf,1);
}

void tmview__SetDataObject(self,tm)
struct tmview *self;
struct termulator *tm;
{
    struct termulator *oldtm=(struct termulator *)self->header.view.dataobject;

    super_SetDataObject(self,tm);

    if(oldtm!=NULL){
	termulator_RemoveMark(oldtm,self->curpos);
	mark_Destroy(self->curpos);
    }

    if(tm!=NULL){
	termulator_SetScreenSize(tm,80,24);
	self->curpos=termulator_CreateMark(tm,0,0);
	mark_SetStyle(self->curpos,FALSE,FALSE); /* so it acts like the dot */
	if(tm->inpView==NULL){
	    tm->inpView=self;
	    self->height=self->width= -1; /* recalc on redraw */
	}
    }
}

static enum keymap_Types rawKeyLookup(self,key,peP,rockP)
struct tmview *self;
long key;
struct proctable_Entry **peP;
long *rockP;
{
    *peP=rawKeyPE;
    *rockP=key;

    return keymap_Proc;
}

static void trackTermulator(self)
struct tmview *self;
{
    struct termulator *tm=(struct termulator *)self->header.view.dataobject;
    int pos=tm->cursor;

    if(tm->mode&(CBREAK|RAW))
	keystate_SetOverride(self->keystate,rawKeyLookup,self);
    else
	keystate_SetOverride(self->keystate,NULL,0);

    if(tm->errstr!=NULL && self->header.textview.hasInputFocus){
	char buf[100];
	if(tm->errno!=0){
	    sprintf(buf,"%s: %s",tm->errstr,sys_errlist[tm->errno]);
	    message_DisplayString(self,0,buf);
	}else
	    message_DisplayString(self,0,tm->errstr);
	tm->errstr=NULL;
    }

    if(self->screen!=tm->screen){
	if(tm->screen>=0 /* &&
	   !tmview_Visible(self,tm->screen) ||
	   !tmview_Visible(self,tm->screen+(tm->width+1)*tm->height) */)
	    tmview_SetTopPosition(self,tm->screen);
	self->screen=tm->screen;
    }

    if(self->cwd!=tm->cwd && self->title==NULL){
	im_SetTitle(tmview_GetIM(self),tm->cwd);
	self->cwd=tm->cwd;
    }

    if(tmview_GetDotLength(self)==0){
	int dot=tmview_GetDotPosition(self);
	if(dot!=pos &&
	   (tm->mode&(CBREAK|RAW) ||
	    dot==mark_GetPos(self->curpos))){
	   tmview_SetDotPosition(self,pos);
	   if(tmview_Visible(self,dot))
	       tmview_FrameDot(self,pos);
	}
	mark_SetPos(self->curpos,pos);
    }
}

void tmview__Update(self)
struct tmview *self;
{
    trackTermulator(self);
    super_Update(self);
}

void tmview__FullUpdate(self,type,left,top,width,height)
struct tmview *self;
enum view_UpdateType type;
long left,top,width,height;
{
    struct termulator *tm=(struct termulator *)self->header.view.dataobject;

    if(tm->inpView==self && (width!=self->width || height!=self->height)){
	struct text_statevector sv;
	struct FontSummary *fs;

	text_InitStateVector(&sv);
	if(tm->screen < 0)
	    text_ApplyEnvironment(&sv,
				  tm->screenStyle,
				  tm->header.text.rootEnvironment);
	else
	    text_ApplyEnvironment(&sv,tm->screenStyle,tm->screenEnv);
	/* ApplyEnvironment doesn't do this */
	sv.CurCachedFont=fontdesc_Create(sv.CurFontFamily,
					 sv.CurFontAttributes,
					 sv.CurFontSize);
	fs=fontdesc_FontSummary(sv.CurCachedFont,tmview_GetDrawable(self));
	termulator_SetDispSize(tm,
			       (width-2*BORDERWIDTH)/fs->maxSpacing,
			       (height-2*BORDERWIDTH)/fs->maxHeight);
	self->width=width;
	self->height=height;
    }

    if(self->title!=NULL)
	im_SetTitle(tmview_GetIM(self),self->title);

    trackTermulator(self);
    super_FullUpdate(self,type,left,top,width,height);
}

void tmview__ReceiveInputFocus(self)
struct tmview *self;
{
    super_ReceiveInputFocus(self);
}    

boolean tmview__InitializeObject(classID,self)
struct classheader *classID;
struct tmview *self;
{
    self->keystate=keystate_Create(self,tmviewKeymap);
    self->curpos=NULL;
    self->screen= -1;
    self->cwd=NULL;
    self->menus=menulist_DuplicateML(tmviewMenus,self);
    self->shmenus=NULL;
    tmview_SetBorder(self,BORDERWIDTH,BORDERWIDTH);
    self->height=self->width=0;
    self->title=NULL;
    return TRUE;
}

void tmview__FinalizeObject(classID,self)
struct classheader *classID;
struct tmview *self;
{
    struct termulator *tm=(struct termulator *)self->header.view.dataobject;

    if(self->curpos!=NULL && tm!=NULL){
	termulator_RemoveMark(tm,self->curpos);
	mark_Destroy(self->curpos);
    }
}	

void tmview__GetClickPosition(self,pos,noc,action,startLeft,startRight,leftPosP,rightPosP)
struct tmview *self;
int pos,noc;
enum view_MouseAction action;
int startLeft,startRight,*leftPosP,*rightPosP;
{
    if(noc%3!=0){
	super_GetClickPosition(self,pos,noc,action,startLeft, startRight,leftPosP,rightPosP);
    }else{
	struct termulator *tm=
	  (struct termulator *)self->header.view.dataobject;
	struct environment *rootEnv=tm->header.text.rootEnvironment,*env;

	termulator_Untermulate(tm);

	env=environment_GetEnclosing(rootEnv,pos);
	if(env==rootEnv)
	    super_GetClickPosition(self,pos,noc,action,startLeft, startRight,leftPosP,rightPosP);
	else{
	    *leftPosP=environment_Eval(env);
	    *rightPosP=environment_GetLength(env)+*leftPosP;
	}
    }
}

void tmview__PostKeyState(self,ks)
struct tmview *self;
struct keystate *ks;
{
    struct termulator *tm=(struct termulator *)self->header.view.dataobject;

    if(tm->mode&(CBREAK|RAW))
	keystate_SetOverride(self->keystate,rawKeyLookup,self);
    else
	keystate_SetOverride(self->keystate,NULL,0);

    self->keystate->next=NULL;
    super_PostKeyState(self,keystate_AddBefore(self->keystate,ks));
}

void tmview__PostMenus(self,ml)
struct tmview *self;
struct menulist *ml;
{
    if(self->shmenus!=NULL)
	menulist_ChainAfterML(self->menus,self->shmenus,1);
    if(ml!=self->menus)
	menulist_ChainAfterML(self->menus,ml,0);
    super_PostMenus(self,self->menus);
}

static void submit(self)
struct tmview *self;
{
    struct termulator *tm=(struct termulator *)self->header.view.dataobject;
    int pos;

    tm->inpView=self;

    termulator_Untermulate(tm);

    pos=termulator_GetLength(tm);

    mark_SetPos(self->curpos,pos);
    tmview_SetDotPosition(self,pos);
    tmview_SetDotLength(self,0);

    termulator_Submit(tm);

    tmview_FrameDot(self,tm->cursor);
}

static void submitStay(self)
struct tmview *self;
{
    struct termulator *tm=(struct termulator *)self->header.view.dataobject;
    int pos=termulator_GetLength(tm);

    tm->inpView=self;

    tmview_SetDotPosition(self,pos);
    tmview_SetDotLength(self,0);
    while(pos>0 && termulator_GetChar(tm,pos-1)!='\n')
	pos--;
    tmview_SetTopPosition(self,pos);

    termulator_Submit(tm);

    /* assumedly, the length has changed, so curpos!=dot after this */
    mark_SetPos(self->curpos,termulator_GetLength(tm));
}

static void selfInsert(self,key)
struct tmview *self;
long key;
{
    char c=key;
    struct termulator *tm=(struct termulator *)self->header.view.dataobject;
    int pos;

    tm->inpView=self;

    termulator_Untermulate(tm);

    tmview_CollapseDot(self);

    pos=tmview_GetDotPosition(self);
    if(pos<termulator_GetFence(tm))
	pos=termulator_GetLength(tm);

#ifdef HACKEDNOECHO
    if(tm->mode&ECHO)
#endif /* HACKEDNOECHO */
	termulator_InsertCharacters(tm,pos++,&c,1);
#ifdef HACKEDNOECHO
    else
	termulator_ProcessInput(tm,&c,1);
#endif /* HACKEDNOECHO */

    tmview_FrameDot(self,pos);
    tmview_SetDotPosition(self,pos);

    termulator_NotifyObservers(tm,0);
}

static void sendSignal(self,key)
struct tmview *self;
long key;
{
    char c=key;
    struct termulator *tm=
      (struct termulator *)self->header.view.dataobject;

    tm->inpView=self;

    termulator_ProcessInput(tm,&c,1);
    termulator_NotifyObservers(tm,0);
}

static void eraseLine(self)
struct tmview *self;
{
    struct termulator *tm=
      (struct termulator *)self->header.view.dataobject;
    int fence;

    tm->inpView=self;

    termulator_Untermulate(tm);

    fence=termulator_GetFence(tm);

#ifdef HACKEDNOECHO
    if(tm->mode&ECHO)
#endif /* HACKEDNOECHO */
	termulator_DeleteCharacters(tm,fence,termulator_GetLength(tm)-fence);

    tmview_FrameDot(self,tmview_GetDotPosition(self));

    termulator_NotifyObservers(tm,0);
}

#define REP_PREV 1
#define REP_NEXT 2
#define REP_MATCH 4

static void replaceCmd(self,how)
struct tmview *self;
long how;
{
    struct termulator *tm=
      (struct termulator *)self->header.view.dataobject;
    int pos,len,fence,c;
    char *cmd,buf[200],*p=buf;

    tm->inpView=self;

    termulator_Untermulate(tm);

    pos=tmview_GetDotPosition(self);
    len=tmview_GetDotLength(self);
    fence=termulator_GetFence(tm);

    if(termulator_GetLength(tm)==fence)
	termulator_RewindCmds(tm);

    if(how&REP_MATCH)
	while(fence<pos && (c=termulator_GetChar(tm,fence++))!=EOF)
	    *p++=c;
    *p='\0';

    cmd=(how&REP_PREV ? termulator_GrabPrevCmd(tm,buf) : termulator_GrabNextCmd(tm,buf));

    if(cmd==NULL){
	message_DisplayString(self,1,"No more commands!");
	return;
    }

    cmd+=p-buf;

    if(fence>pos){
	len-=(fence-pos);
	pos=fence;
	if(len<0)
	    len=0;
    }

    termulator_DeleteCharacters(tm,pos,len);

    len=strlen(cmd);
    termulator_InsertCharacters(tm,pos,cmd,len);

    tmview_FrameDot(self,pos);
    tmview_SetDotPosition(self,pos);
    tmview_SetDotLength(self,len);

    termulator_NotifyObservers(tm,0);
}

static void getLine(self,pos,beginP,endP)
struct tmview *self;
int pos;
int *beginP,*endP;
{
    struct termulator *tm=
      (struct termulator *)self->header.view.dataobject;
    struct environment *env=tm->header.text.rootEnvironment;
    int c,nc;
    int end=pos;

    while(pos>0 && termulator_GetChar(tm,pos-1)!='\n')
	pos--;
    *beginP=pos;
    while((c=termulator_GetChar(tm,end))!=EOF && c!='\n')
	end++;
    *endP=end;

    nc=environment_GetNextChange(env,pos)+pos;
    if(nc<end){
	env=environment_GetInnerMost(env,nc);
	*beginP=nc;
	*endP=nc+environment_GetLength(env);
    }
}

static void move(self)
struct tmview *self;
{
    int pos,len,begin,end;
    struct termulator *tm=
      (struct termulator *)self->header.view.dataobject;

    tm->inpView=self;

    termulator_Untermulate(tm);

    pos=tmview_GetDotPosition(self);
    len=tmview_GetDotLength(self);
    end=termulator_GetLength(tm);
    begin=end;

    if(len==0){
	int endpos;
	if(pos>=end){
	    replaceCmd(self,REP_PREV);
	    return;
	}
	getLine(self,pos,&pos,&endpos);
	len=endpos-pos;
    }

    while(len-->0){
	char c=termulator_GetChar(tm,pos++);
	termulator_InsertCharacters(tm,end++,&c,1);	
    }

    tmview_SetDotLength(self,end-begin);
    tmview_SetDotPosition(self,begin);
    tmview_FrameDot(self,begin);
}

static void exec(self)
struct tmview *self;
{
    move(self);
    submit(self);
}

static void deleteOrEOT(self)
struct tmview *self;
{
    struct termulator *tm=(struct termulator *)self->header.view.dataobject;
    int pos;
    int c;

    tm->inpView=self;

    termulator_Untermulate(tm);

    pos=tmview_GetDotPosition(self);

    if((c=termulator_GetChar(tm,pos))=='\n' || c==EOF)
	termulator_EOT(tm);
    else
	termulator_DeleteCharacters(tm,pos,1);

    tmview_FrameDot(self,pos);

    termulator_NotifyObservers(tm,0);
}

static void startProc(self)
struct tmview *self;
{
    struct termulator *tm=(struct termulator *)self->header.view.dataobject;
    char buf[5000],*com;
    char *shell=environ_Get("SHELL");
    char *argbuf[500],**argv;

    tm->inpView=self;

    if(shell==NULL)
	shell="/bin/csh";

#define processExists(pid) (kill(pid,0)!=-1 || errno!=ESRCH)

    if(tm->pid!=0 && !processExists(tm->pid))
	tm->pid=0;
    
    if(tm->pid!=0 &&
       tm->ptyFile!=NULL &&
       (message_AskForString(self,
			     0,
			     "Process already exists, start a new one? [n] ",
			     NULL,
			     buf,sizeof(buf))==-1 ||
	*buf!='y'))
	return;

    if(tm->pid!=0 &&
       message_AskForString(self,
			     0,
			     "Kill old process? [y] ",
			     NULL,
			     buf,sizeof(buf))==0 &&
       *buf=='y'){
	termulator_Signal(tm,SIGTERM);
	if(processExists(tm->pid))
	    termulator_Signal(tm,SIGKILL);
    }

    com=argvtostr(tm->args,buf,sizeof(buf));
    if(com==NULL)
	strcpy(buf,shell);

    if(message_AskForString(self,0,"Command: ",buf,buf,sizeof(buf))==-1)
	return;

    argv=strtoargv(buf,argbuf,sizeof(argbuf));
    if(argv==NULL){
	message_DisplayString(self,1,"Too many arguments.");
	return;
    }

    termulator_StartProcess(tm,argv);
}

static void bol(self)
struct tmview *self;
{
    struct termulator *tm=(struct termulator *)self->header.view.dataobject;
    int pos,fence;

    tm->inpView=self;

    termulator_Untermulate(tm);

    pos=tmview_GetDotPosition(self);
    fence=termulator_GetFence(tm);

    if(textview_BeginningOfLine!=NULL)
	(*textview_BeginningOfLine)(self);
    if(pos>fence && tmview_GetDotPosition(self)<fence)
	tmview_SetDotPosition(self,fence);
}

static void clear(self)
struct tmview *self;
{
    termulator_Clear((struct termulator *)self->header.view.dataobject);
}

static void execStr(self,str)
struct tmview *self;
char *str;
{
    struct termulator *tm=(struct termulator *)self->header.view.dataobject;
    eraseLine(self);
    termulator_InsertCharacters(tm,termulator_GetLength(tm),str,strlen(str));
    submit(self);
}

boolean tmview__ReadShMenus(self,filename)
struct tmview *self;
char *filename;
{
    FILE *fp;
    char buf[1000];
    struct proctable_Entry *pe=
      proctable_DefineProc("tmview-exec-str",
			    execStr,
			    &tmview_classinfo,
			    NULL,
			    "Execute a string.");

    if(self->shmenus==NULL){
	self->shmenus=menulist_Create(self);
	if(self->shmenus==NULL)
	    return FALSE;
    }

    fp=fopen(filename,"r");
    if(fp==NULL)
	return FALSE;

    while(fgets(buf,500,fp)!=NULL){
	char *str=strchr(buf,':');
	if(str!=NULL){
	    char *save;
	    int len;
	    *str++='\0';
	    len=strlen(str);
	    str[len-1]='\0';
	    save=malloc(len);
	    strcpy(save,str);
	    menulist_AddToML(self->shmenus,buf,pe,save,0);
	}else
	    menulist_AddToML(self->shmenus,buf,NULL,0,0);
    }

    fclose(fp);

    return TRUE;
}

struct bind_Description tmviewBindings[]={
    {"tmview-submit", "\r",0, NULL,0,0, submit, "Submits a line of input to the subprocess."},
    {"tmview-submit-and-stay", "\n",0, NULL,0,0, submitStay, "Submits a line of input, but also keeps cursor at this line, and puts it at the top of the screen."},
    {"tmview-signal", "\003",'\003', NULL,0,0, sendSignal, "Send a signal to the subproces."},
    {"tmview-signal", "\032",'\032'},
    {"tmview-signal", "\034",'\034'},
    {"tmview-erase-line", "\025",0, NULL,0,0, eraseLine, "Erases current line."},
    {"tmview-prev-cmd", "\033=",REP_PREV, NULL,0,0, replaceCmd, "Retrieves previous command."},
    {"tmview-next-cmd", "\033-",REP_NEXT, NULL,0,0, replaceCmd, "Retrieves next command."},
    {"tmview-next-cmd", "\033`",0},
    {"tmview-prev-match-cmd", "\033[",REP_PREV|REP_MATCH, NULL,0,0, replaceCmd, "Retrieves previous matching command."},
    {"tmview-next-match-cmd", "\033]",REP_NEXT|REP_MATCH, NULL,0,0, replaceCmd, "Retrieves next matching command."},
    {"tmview-move", "\033+",0, "Move~30",0,0, move, "Move selected text to end."},
    {"tmview-exec", "\033\r",0, "Execute~30",0,0, exec, "Move, then execute command."},
    {"tmview-delete-or-eot", "\004",0, NULL,0,0, deleteOrEOT, "Sends EOT at end of line, otherwise deletes next character."},
    {"tmview-start-new-process", "\030!",0, "Termulator,Start new process",0,0, startProc, "Starts a new child process."},
    {"tmview-beginning-of-line", "\001",0, NULL,0,0, bol, "Beginning of line or input, whichever is closest."},
    {"tmview-clear", NULL,0, "Termulator,Clear",0,0, clear, "Clears the text of the tm."},

    {NULL, NULL,0, "Save"},
    {NULL, NULL,0, "Switch File"},
    {NULL, NULL,0, "Plainer"},
    {NULL, NULL,0, "Plainest"},
    {NULL, NULL,0, "File,Save As",0,1},
    {NULL, NULL,0, "File,Save All"},
    {NULL, NULL,0, "File,Insert File"},
    {NULL, NULL,0, "File,Set Printer",0,1},
    {NULL, NULL,0, "File,Preview",0,1},
    {NULL, NULL,0, "File,Print",0,1},
    {NULL, NULL,0, "File,Add Template"},
    {NULL, NULL,0, "Search/Spell,Query Replace"},
    {NULL, NULL,0, "Search/Spell,Check Spelling"},
    NULL
};

void tmview__SetFileMenus(self,on)
struct tmview *self;
boolean on;
{
    if(menulist_SetMask(self->menus,(on?0:1)))
	tmview_PostMenus(self,self->menus);
}

boolean tmview__InitializeClass(classID)
struct classheader *classID;
{
    struct proctable_Entry *pe=
      proctable_DefineProc("tmview-self-insert",
			    selfInsert,
			    &tmview_classinfo,
			    NULL,
			    "If echoing, insert a key, otherwise store it.");
    char buf[2],c;

    if(textview_BeginningOfLine==NULL){
	struct proctable_Entry *pe;

	class_Load("textview");
	pe=proctable_Lookup("textview-beginning-of-line");
	if(pe!=NULL)
	    textview_BeginningOfLine=(void (*)())proctable_GetFunction(pe);
    }

    tmviewKeymap=keymap_New();
    tmviewMenus=menulist_New();
    
    if(tmviewKeymap==NULL || tmviewMenus==NULL)
	return FALSE;
 bind_BindList(tmviewBindings,tmviewKeymap,tmviewMenus,&tmview_classinfo);
 
    rawKeyPE=
      proctable_DefineProc("tmview-raw-key",
			    rawKey,&tmview_classinfo,NULL,
			    "Send a key directly to the subprocess in this view.");

    buf[1]='\0';
    for(c=' '; c<127; c++){
	buf[0]=c;
	keymap_BindToKey(tmviewKeymap,buf,pe,c);
    }

    return TRUE;
}
