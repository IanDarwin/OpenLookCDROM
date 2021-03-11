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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/typescript/RCS/tscript.c,v 2.84 1994/04/26 20:13:42 rr2b Exp $";
#endif


 

#include <andrewos.h> /* sys/types.h sys/time.h */
#include <class.h>
#include <text.ih>
#include <mark.ih>
#include <keystate.ih>
#include <frame.ih>

#include <menulist.ih>
#include <proctbl.ih>
#include <keymap.ih>
#include <im.ih>
#include <message.ih>
#include <msghndlr.ih>
#include <typetext.ih>
#include <smpltext.ih>
#include <dataobj.ih>
#include <fontdesc.ih>
#include <envrment.ih>
#include <style.ih>
#include <stylesht.ih>
#include <environ.ih>
#include <print.ih>
#include <filetype.ih>
#include <tscript.eh>

#include <ctype.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#ifdef hpux
#include <sys/bsdtty.h>
#include <sys/ptyio.h>
#endif /* hpux */
#ifdef sys_sun4_51
#define SY_AIX221 1
#endif

#ifdef M_UNIX
#include <sys/termio.h>
#include <sys/stream.h>
#include <sys/ptem.h>
#endif

#if defined(sys_telmat)
#include <sys/termio.h>
#include <sys/stream.h>
#include <sys/ptem.h>
#include <sys/stropts.h>
#endif /* sys_telmat */

#include <signal.h>

#if defined(POSIX_ENV) && !defined(sun)
#include <termios.h>
#else
#if SY_AIX221
#ifndef sys_sun4_51
#include <sys/devinfo.h>
#include <sys/pty.h>
#endif
#include <sys/termio.h>
#include <sys/tty.h>
#else /* #if SY_AIX221 */
#include <sgtty.h>
#endif /* #if SY_AIX221 */
#endif /* defined(POSIX_ENV) && !defined(sun) */

#if SY_AIX12
static char io_buffer[4096];
#endif /* SY_AIX12 */

#if SY_AIX31 || SY_AIX31
#define UTMP_MUNGE 1
#endif

#ifdef UTMP_MUNGE
#include <utmp.h>
#endif

#ifdef CONTRIB_ENV
#define PRINTER_SETUP_DIALOG_ENV 1
#endif

/* #define DONTCUTSTYLES */
#define MyEnvinfo(text,pos) environment_GetInnerMost(text->rootEnvironment,pos)
#define TEXT(A) ((struct text*)(A->header.view.dataobject))
#define TEXTOBJ(A) A->header.view.dataobject
#define ISPIPESCRIPT(self) (self->pipescript)

static char *DefaultArgList[] = { 0, 0 };
/* #define SENDRAW 1 */

static char **myarglist = NULL;
static boolean Pipescript = FALSE;
static struct style *staticBoldStyle = NULL;
static struct keymap *ssmap;
static struct menulist *typescriptMenus;
static FILE *df = NULL;
static FILE *odf = NULL;

static void Typescript_SelfInsertCmd();
static void MyCanOutHandler();	
static void SendSig();
static int WritePty();
static int typescriptAddtypescriptMenus();

/* The following are initialized from the proctable by our InitializeClass
 * routine. There may well be a better way of doing this. The routines
 * should certainly be void instead of int, but the proctable package
 * is screwed. */
static int 	(*textview_EndOfTextCmd)();
static int 	(*textview_SelfInsertCmd)();
static int    	(*textview_DigitCmd)();
static int 	(*textview_BeginningOfLineCmd)();
static int 	(*textview_EndOfLineCmd)();
static int 	(*textview_YankCmd)();
static int 	(*textview_BackwardsRotatePasteCmd)();
static int 	(*textview_RotatePasteCmd)();
static int 	(*textview_DeleteCmd)();
static int 	(*textview_RuboutCmd)();
static int	(*textview_LineToTop)();
static int 	(*typescript_CopyRegionCmd)(); 
static int 	(*typescript_ZapRegionCmd)(); 

#ifndef _IBMR2
extern char *realloc();
#endif /* _IBMR2 */

static long maxSize;
static long extraRoom;
static char *cmd;
static int CmdSize;
static boolean FileMenu = FALSE;
#define	SetCmdSize(A) if(A>CmdSize) cmd = (char*)realloc(cmd,(CmdSize = 64 + A))

static int
typescriptAddMenu(nbuf, proc)
char *nbuf;
struct proctable_Entry *proc;
{
    char *c, *bf, *cp, *retstr;

    retstr = NULL;
    bf = (char*) malloc(strlen(nbuf) + 1);
    for(c = bf,cp = nbuf; *cp != '\0'; cp++ ,c++) {
	if(*cp == ':') {
	    *c = '\0';
	    retstr = c + 1;
	}
	else 
	    *c = *cp;
    }
    *c = '\0';
    if(retstr && *retstr)
	menulist_AddToML(typescriptMenus, bf, proc, (long)retstr, 0);
    else free(bf);
}
    
static int
typescript_PreviewCmd(self)
struct typescript *self;
{
    doprint(self,0);
}

static int
typescript_PrintCmd(self)
struct typescript *self;
{
    doprint(self,1);
}

static int
doprint(self, porp)
struct typescript *self;
int porp;
{
    message_DisplayString(self, 0, "Processing request.");
    im_ForceUpdate();
    if(class_Load("print") == NULL) {
	message_DisplayString(self, 0, "Print aborted: could not load class \"print\".");
        return;
    }
    print_ProcessView((struct view *)self, porp, 1, "pipescript", "");
    if (porp)
	message_DisplayString(self, 0, "Print request submitted; watch console for results.");
    else
	message_DisplayString(self, 0, "Preview window should appear soon.");
}

static int
typescript_SaveAs(self)
struct typescript *self;
{
    char frs[256], mes[256];
    FILE *f;
    int failed = 0;

    if(message_AskForString(self, 0, "File Name: ", "", frs, 256) == -1) 
	return;
    filetype_CanonicalizeFilename(mes, frs, 256);
    if((f = fopen(mes, "w")) == NULL) 
	failed++;
    else {
	text_Write(TEXT(self), f, im_GetWriteID(), 0); 
	if(vclose(fileno(f)) < 0) 
	    failed++;
    }
    if(failed) {
	sprintf(frs, "Can't write %s", mes);
	message_DisplayString(self, 0, frs);
	return;
    }
    sprintf(frs, "wrote %s", mes);
    message_DisplayString(self, 0, frs);
}

static int
typescript_SetPrinterCmd(self)
struct typescript *self;
{
    struct msghandler *messageLine = (struct msghandler *) typescript_WantHandler(self, "message");
    char *currentPrinter, *defaultPrinter, answer[256], prompt[sizeof("Current printer is . Set printer to []: ") + 128];

    if(messageLine == NULL)
        return;
    currentPrinter = environ_Get("LPDEST");
    if (currentPrinter == NULL)
  	currentPrinter = environ_Get("PRINTER");
    defaultPrinter = environ_GetProfile("print.printer");
    if (!defaultPrinter) defaultPrinter = environ_GetProfile("print.spooldir");
    if(currentPrinter && defaultPrinter)
        sprintf(prompt, "Current printer is %.64s. Set printer to [%.64s]: ", currentPrinter, defaultPrinter);
    else if(defaultPrinter)
        sprintf(prompt, "Set printer to [%.64s]: ", defaultPrinter);
    else
        strcpy(prompt, "Set printer to: ");
    if(msghandler_AskForString(messageLine, 0, prompt, NULL, answer, sizeof(answer)) == -1)
        return;
    if(*answer != '\0') {
	environ_Put("LPDEST",answer);
	environ_Put("PRINTER",answer);
	defaultPrinter = answer;
    }
    else {
	environ_Delete("LPDEST");
	environ_Delete("PRINTER");
    }
    if(defaultPrinter) {
        sprintf(prompt, "Printer set to %.64s.", defaultPrinter);
        msghandler_DisplayString(messageLine, 0, prompt);
    }
    else
        msghandler_DisplayString(messageLine, 0, "Printer not set.");
}

static int
typescriptAddSearchMenu()
{
    struct proctable_Entry *tempProc;

    if(tempProc = proctable_Lookup("textview-search"))
        menulist_AddToML(typescriptMenus, "Search~20,Forward~10", tempProc, NULL, 0);

    if(tempProc = proctable_Lookup("textview-reverse-search"))
        menulist_AddToML(typescriptMenus, "Search~20,Backward~11", tempProc, NULL, 0);
}

static int
typescriptAddFileMenu()
{
    struct proctable_Entry *tempProc;
    struct classinfo *classInfo = &typescript_classinfo;

    tempProc=proctable_DefineProc("typescript-Save-As",	typescript_SaveAs,  classInfo, NULL, "Prompt for file name to save");
    menulist_AddToML(typescriptMenus, "File~30,Save As~10", tempProc, NULL, 0);

    tempProc=proctable_DefineProc("typescript-Print", typescript_PrintCmd, classInfo, NULL, "Print typescript");
    menulist_AddToML(typescriptMenus, "File~30,Print~21", tempProc, NULL, 0);

    tempProc=proctable_DefineProc("typescript-Preview", typescript_PreviewCmd, classInfo, NULL, "Preview typescript");
    menulist_AddToML(typescriptMenus, "File~30,Preview~22", tempProc, NULL, 0);

    tempProc = proctable_DefineProc("typescript-SetPrinter", typescript_SetPrinterCmd, classInfo, NULL, "Set Printer");
#ifdef PRINTER_SETUP_DIALOG_ENV
    class_Load("printopts"); /* Make sure the textview is loaded first. */
    if ((tempProc = proctable_Lookup("printopts-post-window")) != NULL)
	menulist_AddToML(typescriptMenus, "File~30,Printer Setup~20", tempProc, NULL, 0);
#else
    menulist_AddToML(typescriptMenus, "File~30,Set Printer~20", tempProc, NULL, 0);
#endif /* PRINTER_SETUP_DIALOG_ENV */
}

static int
AnounceDeath(self)
struct typescript *self;
{
    char buf[512];
    char *shell = environ_Get("SHELL");

    if(shell == NULL) 
	shell = "/bin/csh";
    if(ISPIPESCRIPT(self)) {
	sprintf(buf, "EOF on pipe");
	message_DisplayString(self, 15, buf);
    }
    else if(strcmp(self->progname,shell) == 0) {
	sprintf(buf,"This process died. Start new typescript to continue work.");
	message_DisplayString(self, 100, buf);
    }
    else {
	sprintf(buf,"The %s process died.",self->progname);
	message_DisplayString(self, 100, buf);
    }    
}

static struct environment *
GetCommandEnv(self, pos, start, end)
register struct typescript *self;
register long pos;
long *start, *end;
{
    register struct environment *te;

    te = MyEnvinfo(TEXT(self), pos);
    if((te->data.style != staticBoldStyle) && (pos > 0)) {
	/* cursor may be at end of environment */
	te = MyEnvinfo(TEXT(self), pos - 1);
    }
    if(te->data.style != staticBoldStyle)
	return NULL;
    /* here we are in the command's bold region */
    *start = environment_Eval(te);
    *end =  *start + te->header.nestedmark.length;
    return te;
}

static void 
typescript_RuboutCmd(self)
register struct typescript *self;
{
    if(self->readOnlyLen != -1) {
#ifdef SENDRAW
	write(self->SubChannel, "\010" , 1);
#else
	if(self->readOnlyLen > 0) 
	    self->readOnlyLen--;
#endif
    }
    else if(typescript_GetDotPosition(self) + typescript_GetDotLength(self) > text_GetFence(TEXT(self)))
	textview_RuboutCmd((struct textview *)self);
}

struct typescript *
typescript__Create(classID, arglist, diskf, filemenu)
struct classheader *classID;
char **arglist;
FILE *diskf;
boolean filemenu;
{
    struct typescript *self;
    struct typetext *tt;
    if(arglist == NULL) Pipescript = TRUE;
    else if(*arglist) myarglist = arglist;
    df = diskf;
    FileMenu = filemenu;
    tt = typetext_New();
    self = typescript_New();
    typescript_SetDataObject(self, (struct dataobject *)tt);
    return(self);
}

struct typescript *
typescript__CreatePipescript(classID, indiskf, outdiskf, filemenu)
struct classheader *classID;
FILE *indiskf;
FILE *outdiskf;
boolean filemenu;
{
    struct typescript *self;

    self = typescript_Create(NULL, indiskf, filemenu);
    odf = outdiskf;
    return self;
}

static void
MaintainLastEnv(td)
struct typescript *td; 
{
    long len, spos;
    struct text *mydoc;
    struct environment *te;

    mydoc =  TEXT(td);
    spos = mark_GetPos(td->cmdStart);
    len = text_GetLength(mydoc);
    if(spos < len) {
	te = MyEnvinfo(mydoc, spos);
	if(te->data.style != staticBoldStyle)
	    te = environment_InsertStyle(mydoc->rootEnvironment, spos, staticBoldStyle, TRUE);
        environment_SetStyle(te, FALSE, FALSE);
	environment_SetLength(te, len - spos);
   }

}

void
SaveCommand(td)
struct typescript *td;
{
    long len, spos;
    struct text *mydoc;
    char *textBuf;
    long retLen;

    mydoc =  TEXT(td);
    spos = mark_GetPos(td->cmdStart);
    len = text_GetLength(mydoc);
    if(spos < len) {
	while(spos < len) {
	    textBuf = text_GetBuf(mydoc, spos, len - spos, &retLen);
	    if(textBuf) {
		text_InsertCharacters(td->cmdText, text_GetLength(td->cmdText), textBuf, retLen);
		spos += retLen;
	    }
	    else break;
	}
	text_InsertCharacters(td->cmdText, text_GetLength(td->cmdText), "\n", 1);
    }
}

static void
TypescriptLeftCommand(tsv)
register struct typescript *tsv; 
{
    register long pos;
    long start,end;

    pos = typescript_GetDotPosition(tsv);
    if(typescript_GetDotLength(tsv)== 0) {
	if(mark_GetPos(tsv->cmdStart) == pos) 
	    return;
	if(GetCommandEnv(tsv,pos,&start,&end) != NULL) {
	    if(start == pos) 
		return;
	    typescript_SetDotPosition(tsv, start);
	    typescript_WantUpdate(tsv, tsv);
	    return;
	}
    }
    textview_BeginningOfLineCmd((struct textview *)tsv);
}

static void 
TypescriptEndOfLineCommand(tsv)
register struct typescript *tsv; 
{
    register long pos;
    long start,end;

    pos = typescript_GetDotPosition(tsv);
    if(typescript_GetDotLength(tsv) == 0 && GetCommandEnv(tsv, pos, &start, &end) != NULL) {
	if(end == pos) 
	    return;
	typescript_SetDotPosition(tsv, end);
	typescript_WantUpdate(tsv, tsv);
    }
    else 
	textview_EndOfLineCmd((struct textview *)tsv);
}

static void
TypescriptEOTCommand(tv)
struct typescript   *tv; 
{
    static struct timeval t = { 0, 0 };
    int wfds = 1 << tv->SubChannel;

    if(tv->SubChannel < 0) 
	return;
    select(32, 0, &wfds, 0, &t);
    if(!wfds) {
	message_DisplayString(tv, 0, "Process not ready for input");
	return;
    }	
    if(typescript_GetDotPosition(tv) >= text_GetLength(TEXT(tv))) {
#if defined(POSIX_ENV) && !defined(sun)
	/* non-SunOS POSIX pty is in cooked mode.  Give it an EOF char.
	 * XXX - this distinction needs to be more precise; POSIX systems
	 * differ fairly strongly in their pty behaviors, and System V
	 * Release 4 and 4.4BSD probably behave more like SunOS.
	 */
    {
	struct termios tios;
	tcgetattr(tv->SubChannel, &tios);
	WritePty(tv, &tios.c_cc[VEOF], 1);
    }
#else /* defined(POSIX_ENV) && !defined(sun) */
	WritePty(tv, "", 0);
#endif /* defined(POSIX_ENV) && !defined(sun) */
    }
    else
	textview_DeleteCmd(tv);
}

static void
TypescriptINTCommand(tv)
register struct typescript *tv; 
{
    SendSig(tv, SIGINT);
}

static void
TypescriptSTOPCommand (tv)
register struct typescript *tv; 
{
#if SY_AIX221
/* %%%%%%  must changed if AIX supports the STOP signal */
    SendSig(tv, 0);
#else /* if SY_AIX221 */
    SendSig(tv, SIGTSTP);
#endif /* if SY_AIX221 */
}

static void
TypescriptQUITCommand (tv)
register struct typescript *tv; 
{
    SendSig(tv, SIGQUIT);
}

static void
SendSig (tv, sig) 
register struct typescript *tv; 
{
#if defined(POSIX_ENV) && !defined(sun)
/* The non-SunOS POSIX pty is in cooked mode, so query the
 * pty for signal chars, then write that signal
 * char to the pty.  The pty will deliver the
 * signal.
 * XXX - this distinction needs to be more precise; POSIX systems
 * differ fairly strongly in their pty behaviors, and System V
 * Release 4 and 4.4BSD probably behave more like SunOS.
 */
    struct termios tios;
    char intchar;

    /* Get interrupt chars each time in case the user mucks with them. */
    tcgetattr(tv->SubChannel, &tios);
    switch(sig) {
        case SIGINT:
            intchar = tios.c_cc[VINTR];
            break;
        case SIGQUIT:
            intchar = tios.c_cc[VQUIT];
            break;
        case SIGTSTP:
            intchar = tios.c_cc[VSUSP];
            break;
        default:
            return;
	}
    WritePty(tv, &intchar, 1);
    return;
#else /* defined(POSIX_ENV) && !defined(sun) */
    int pgrp = 0;
    if(tv->SubChannel < 0) 
	return;
#if SY_AIX221
    pgrp = tv->pgrpid;	/* get saved process group */
#else
    ioctl(tv->SubChannel, TIOCGPGRP, &pgrp);
#endif
    if(pgrp == 0)
	message_DisplayString(tv, 0, "Can't send signal to subprocess");
    else 
	killpg(pgrp, sig);
    return;
#endif /* defined(POSIX_ENV) && !defined(sun) */
}

static void 
TypescriptUnboundCommand(tv)
register struct typescript *tv; {
}

static void 
smashReadOnlyBuf(tv)
register struct typescript *tv; 
{   /* Clear out the buf when no longer needed since it probably contains a password */
    register char *c;
    register int i = READONLYMAX;

    for(c = tv->readOnlyBuf; i > 0; i-- )
	*c++ = 'X';
}


#ifdef CONTRIB_ENV
#define PRINTER_SETUP_DIALOG_ENV 1
#endif
static void
TypescriptDoReturnCommand (tv,endpos)
register struct typescript *tv;
register long endpos;
{
    register struct text *d;
    int maxpos, vfp, wfds;
    register stpos, len;
    static struct timeval t = { 0, 0};

    if(tv->SubChannel < 0) {
	AnounceDeath(tv);
	return;
    }
    MaintainLastEnv(tv);
    SaveCommand(tv);
    tv->lastCmdPos = text_GetLength(tv->cmdText);

    wfds = 1 << tv->SubChannel;
    d = TEXT(tv);
    maxpos = text_GetLength(d);
    select(32, 0, &wfds, 0, &t);
    if(!wfds || tv->OutputWait) {
	if(!tv->OutputWait) {
	    /* add an output handler */
	    im_AddCanOutHandler(tv->SCFile, (procedure) MyCanOutHandler, (char*) tv, 6);
	    tv->OutputWait = 1;
	}
	/* force newline addition to end of input during typeahead,
	 as it is forced in the normal case */
	text_InsertCharacters(d, maxpos, "\n", 1);
	mark_SetPos(tv->cmdStart, maxpos + 1);
	typescript_SetDotPosition(tv, maxpos + 1);
	return;
    }	

    /* compute end of stuff to send */
    if(maxpos > (vfp = mark_GetPos(tv->cmdStart))) {
	stpos = vfp;
	len = maxpos - stpos;
	if(len < 0)
	    len = 0;
    }
    else {
	stpos = maxpos;
	len = 0;
    }
    SetCmdSize(len + 1);

#ifdef SENDRAW
    if(tv->readOnlyLen == 0)
	write(tv->SubChannel,"\n",1);
#else
    if(tv->readOnlyLen > 0) {
        if(len > 0) {
	    text_CopySubString(d, stpos, len , cmd, FALSE);
            WritePty(tv, cmd, len);
	}
        tv->readOnlyBuf[tv->readOnlyLen] = '\n';
        WritePty(tv, tv->readOnlyBuf, tv->readOnlyLen + 1);
        text_InsertCharacters (d, stpos + len++, "\n", 1);
	smashReadOnlyBuf(tv);
    }
#endif
    else {
        text_InsertCharacters(d, stpos + len++, "\n", 1);
	text_CopySubString(d, stpos, len, cmd, FALSE);
	if(len < 100)
            WritePty(tv, cmd , len);
	else {
	    int tl;
	    char *cc;
	    for(cc = cmd,tl = len; tl >= 100; cc += 100, tl -= 100)
		WritePty(tv, cc , 100);
	    if(tl > 0)
		WritePty(tv, cc , tl);
	}
    }
    tv->readOnlyLen = -1;
    stpos += len;
    if(endpos < 0)
	typescript_SetDotPosition(tv, stpos);
    else 
	typescript_SetDotPosition(tv, endpos);
    typescript_SetDotLength(tv, 0);
    tv->lastCmdPos = text_GetLength(tv->cmdText);
    mark_SetPos(tv->cmdStart, stpos);
    mark_SetLength(tv->cmdStart, 0);
    text_SetFence(d, stpos);
    typescript_FrameDot(tv, stpos);
    text_NotifyObservers(d , 0);
    if(text_GetLength(d) > maxSize)
	typetext_AlwaysDeleteCharacters((struct typetext*) d, 0, text_GetLength(d) - maxSize + extraRoom);
}

static void
TypescriptReturnCommand (tv)
register struct typescript *tv; 
{
    TypescriptDoReturnCommand(tv, -1);
}

static void 
TypescriptReturnAndPositionCommand(self, data)
struct typescript *self;
long data;
{
    TypescriptDoReturnCommand(self, text_GetFence(TEXT(self)));
    textview_LineToTop((struct textview*)self, data); 
} 

static int
typescript_HandleMenus(self, data)
struct typescript *self;
long data;
{
    char *s = (char*) data;

    while(*s) {
	if((*s == '\n') || (*s == '\r')) 
	    TypescriptReturnCommand(self);
	else
	    Typescript_SelfInsertCmd(self, *s);
	s++;
    }
}

static void
TypescriptZapCommand(tv)
register struct typescript *tv;
{
    register struct text *d;
    int maxpos;
    register stpos;
#ifdef SENDRAW
    if(tv->readOnlyLen == 0) {
	write(tv->SubChannel,"\025",1);
	return;
    }
#else
    if(tv->readOnlyLen != -1) {
        tv->readOnlyLen = 0;
        return;
    }
#endif
    d = TEXT(tv);
    maxpos = text_GetLength(d);
    stpos = mark_GetPos(tv->cmdStart);
    if(maxpos > stpos) {
	text_DeleteCharacters(d, stpos, maxpos - stpos);
	typescript_SetDotPosition(tv, stpos);
	typescript_SetDotLength(tv, 0);
	tv->lastCmdPos = text_GetLength(tv->cmdText);
    }
    text_NotifyObservers(d , 0); 
}

static void
GrabCommandHere(tv, where)
long where;
register struct typescript *tv; 
{
    register int i;
    long start, size,len;
    struct environment *te;

    i = where;
    if(i <0 ) i = 0;
    te = MyEnvinfo(TEXT(tv), i);
    if(te->data.style != staticBoldStyle) 
	return;
    /* here we are in the command's bold region */
    start = environment_Eval(te);
    size = te->header.nestedmark.length;
    /* now compute the next place to start */
    SetCmdSize(size);
    /* now copy the bits out */
    text_CopySubString(TEXT(tv), start, size , cmd, FALSE);
    if((len = typescript_GetDotLength(tv)) > 0)
/* deletecharacters will check the fence for us */
	text_DeleteCharacters(TEXT(tv), typescript_GetDotPosition(tv), len);
    start = text_GetLength(TEXT(tv));
    text_InsertCharacters(TEXT(tv), start, cmd, size);
    typescript_SetDotPosition(tv, start);
    typescript_SetDotLength(tv, size);
    typescript_FrameDot(tv, start);
    text_NotifyObservers(TEXT(tv), 0);
}

static int
GrabCommand(tv, fromText, start, end)
register struct typescript   *tv;
struct text *fromText;
long start;
long end;
{
    long size, len, pos;

    size = end - start;
    /* now copy the bits out */
    SetCmdSize(size);
    text_CopySubString(fromText, start, size, cmd, FALSE);
    if((len = typescript_GetDotLength(tv)) > 0) {
	/* deletecharacters will check the fence for us */
	text_DeleteCharacters(TEXT(tv),typescript_GetDotPosition(tv),len);
    }
    pos = text_GetLength(TEXT(tv));
    text_InsertCharacters(TEXT(tv), pos, cmd, size);
    typescript_SetDotPosition(tv, pos);
    typescript_SetDotLength(tv, size);
    typescript_FrameDot(tv, pos);
    text_NotifyObservers(TEXT(tv), 0);
}

static void
GrabLastCommand (tv)
register struct typescript *tv;
{
    long cmdEnd;

    if(tv->lastCmdPos == 0)
	return;
    cmdEnd = tv->lastCmdPos - 1;
    tv->lastCmdPos = text_GetBeginningOfLine(tv->cmdText, cmdEnd);
    GrabCommand(tv, tv->cmdText, tv->lastCmdPos, cmdEnd);
}

static void
GrabNextCommand (tv)
register struct typescript   *tv;
{
    long cmdEnd;

    if(tv->lastCmdPos >= text_GetLength(tv->cmdText))
	return;
    tv->lastCmdPos = text_GetEndOfLine(tv->cmdText, tv->lastCmdPos) + 1;
    cmdEnd = text_GetEndOfLine(tv->cmdText, tv->lastCmdPos);
    if(tv->lastCmdPos != cmdEnd)
	GrabCommand(tv, tv->cmdText, tv->lastCmdPos, cmdEnd);
}

static void
GrabCurrentCommand(tv)
struct typescript *tv; 
{
    register int i;
    register struct text *d;
    struct environment *te;
    int dlen;
    register long lineBegin, lineEnd;

    d = TEXT(tv);
    dlen = text_GetLength(d);
    if((i = typescript_GetDotLength(tv)) == 0) {
	lineEnd = text_GetEndOfLine(d, typescript_GetDotPosition(tv));
	lineBegin = text_GetBeginningOfLine(d, lineEnd);

	for(i = lineBegin; i < lineEnd; i++) {
	    te = MyEnvinfo(d, i);
	    if(te->data.style == staticBoldStyle) {
		long start, size;

		/* here we are in the command's bold region */
		start = environment_Eval(te);
		size = te->header.nestedmark.length;
		GrabCommand(tv, d, start, start + size);
		return;
	    }
	}
    }
    else {
	/* i still has the length */
	lineBegin = typescript_GetDotPosition(tv);
	lineEnd = lineBegin + i;
    }
    /* now send from lineBegin to lineEnd inclusive */
    SetCmdSize(lineEnd - lineBegin);
    text_CopySubString(d, lineBegin, lineEnd - lineBegin , cmd, FALSE);
    text_InsertCharacters(d, dlen, cmd, i = lineEnd - lineBegin);
    typescript_SetDotPosition(tv, dlen);
    typescript_SetDotLength(tv, i);
    typescript_FrameDot(tv, dlen);
    text_NotifyObservers(d, 0);
    typescript_PostMenus(tv , tv->menulist);
    /* MaintainLastEnv(tv); */
}

static void
ExecuteCurrentCommand(tv)
struct typescript *tv; 
{
    if(typescript_GetDotPosition(tv) < text_GetFence(TEXT(tv)))
	GrabCurrentCommand(tv);
    TypescriptReturnCommand (tv);
}

static void 
SetTitle(self, titleLine)
struct typescript *self;
char *titleLine;
{

#define WMTITLELEN 70 /* Can you say "Magic hack?" */

    char titleBuffer[1000];
    int len, maxLen = WMTITLELEN - 1;
    char *tb;

    *titleBuffer = '\0';

    tb = titleBuffer;
    if(self->title) {
	len = strlen(self->title);
	if(len < WMTITLELEN - 3) {
	    strcpy(titleBuffer, self->title);
	    strcat(titleBuffer, ": ");
	    maxLen -= (len + 2);
	    tb = &titleBuffer[len + 2];
	}
	else {
	    strcpy(titleBuffer, self->title);
	    titleLine = NULL;
	}
    }
		
    if(titleLine) {
	char *home = environ_Get("HOME");

	if(home) {
	    int hlen = strlen(home);
	    if(strncmp(titleLine,home,hlen) == 0) {
		strcpy(tb,"~");
		--maxLen;
		titleLine += hlen;
	    }
	}		    
	len = strlen(titleLine);
	if(len > maxLen) {
	    char *partialName;

	    maxLen -= sizeof("---") - 1;
	    partialName = strchr(titleLine + (len - maxLen), '/');
	    if(partialName == NULL)
		partialName = titleLine + (len - maxLen);
	    else
		++partialName; /* Skip slash... */
	    strcpy(tb, "---");
	    strcat(tb, partialName);
	}
	else
	    strcat(tb, titleLine);
    }
    if (self->frame)
	frame_SetTitle(self->frame, titleBuffer);
    else
	im_SetTitle(typescript_GetIM(self), titleBuffer); 
}

#define StartMagicChar 1 /* Ctrl A */
#define EndMagicChar 2  /* Ctrl B */

static char * 
ReadDirName(self, f, buf, bufsiz)
struct typescript *self;
FILE *f;
char *buf;
int *bufsiz;
{
    register char *cp;
    register c;
    register int i = *bufsiz;
    for(cp = buf; --i && FILE_HAS_IO(f); cp++) {
	if((c = getc(f)) == EOF) {
	    im_RemoveFileHandler(f);
	    self->SubChannel = -1;
	    AnounceDeath(self);
	    break;
	}
	if((*cp = c) == '\n') {
	    cp++;
	    break;
	}
	if(*cp == EndMagicChar) {
	    *cp = '\0';
	    if(im_ChangeDirectory(buf) == 0)
		SetTitle(self,buf);
	    return(NULL);
	}
    }
    *bufsiz = i;
    return(cp);
}

static void
ReadFromProcess(f, td)
FILE *f;
register struct typescript *td;
{
    char buf[4000];
    register char *bp = buf;
    register long dotpos, vfp;
    register c = getc(f), i = 3999;
    int reframe = 0;
    char *input;
    int cpos;
    struct text *d = TEXT(td);

    mark_IncludeBeginning(td->cmdStart) = FALSE;
    dotpos = typescript_GetDotPosition(td);
    if(td->lastPosition == -1)
	td->lastPosition = dotpos;
     if(c != EOF) {
	 if(odf) 
	     putc(c, odf);
	if(((c == StartMagicChar) || (c == EndMagicChar)) && !td->pipescript) {
	    cpos = i;
	    if(input = ReadDirName(td, f, bp, &cpos)) {
		bp = input;
		i = cpos;
	    }
	}
	else if(c != '\015') 
	    *bp++ = c;
        while(--i && FILE_HAS_IO(f)) {
	    if((c = getc(f)) == EOF) 
		break;
	    if(odf) 
		putc(c, odf);
            *bp = c;
	    if(((*bp == StartMagicChar) || (*bp == EndMagicChar)) && !td->pipescript) {
		cpos = i;
		if(input = ReadDirName(td, f, bp, &cpos)) {
		    bp = input;
		    i = cpos;
		    if(i == 0) 
			break;
			 /* we could lose some output this way, but only if someone
			is cat-ing a binary or something else bogus.
			Even then , I doubt the reads will be this big. */
		}
	    }
	    else if(*bp != '\015') 
		bp++;
        }
	if(bp != buf) {
	    vfp = text_GetFence(d);
	    if(vfp <= dotpos) {
		dotpos += bp - buf;
		reframe++;
	    }
	    text_InsertCharacters (d, vfp, buf, bp - buf);
	    text_SetFence(d, vfp = vfp + (bp - buf));
	    if(mark_GetPos(td->cmdStart) < vfp) {
		mark_SetPos(td->cmdStart, vfp);
		mark_SetLength(td->cmdStart, 0);
	    }
	    if(reframe) {
		typescript_SetDotPosition(td,dotpos); 
/*		typescript_FrameDot(td,dotpos); */	
	    }
	    text_NotifyObservers(d, 0);
	}
#if defined(POSIX_ENV) && !defined(sun)
	{
	    struct termios tios;

	    tcgetattr(td->SlaveChannel, &tios);
	    if(tios.c_lflag & ECHO)
		td->readOnlyLen = -1;	/* echo should be on */
	    else if(td->readOnlyLen == -1)
		td->readOnlyLen = 0;	/* no echo just started */
	}
#else /* defined(POSIX_ENV) && !defined(sun) */
#if (!vax | vax2) && (!defined(M_UNIX)) /* everything but vax_11 */ /* this prevents vax from inserting chars in doc ! */
        {
#if SY_AIX221
            struct termio ThisTerm;

            ioctl(fileno(f), TCGETA, &ThisTerm);
            if(ThisTerm.c_lflag & ECHO)
		td->readOnlyLen = -1;	/* echo should be on */
	    else if(td->readOnlyLen == -1)
                td->readOnlyLen = 0;	/* noecho mode just started... */
#else /* #if SY_AIX221 */
            struct sgttyb sgttyb;

            ioctl(fileno(f), TIOCGETP, &sgttyb);
            if(sgttyb.sg_flags & ECHO)
		td->readOnlyLen = -1;	/* echo should be on */
	    else if(td->readOnlyLen == -1)
                td->readOnlyLen = 0;	/* noecho mode just started... */
#endif /* #if SY_AIX221 */
	}
#endif /* (!vax | vax2)   */
#endif /* defined(POSIX_ENV) && !defined(sun) */
     }
    if(c == EOF) {
	if(odf) fclose(odf);
        im_RemoveFileHandler(f);
	td->SubChannel = -1;
	AnounceDeath(td);
    }
    mark_IncludeBeginning(td->cmdStart) = TRUE;
}

static void
ClearTypescriptText(tv)
struct typescript *tv;
{
    struct text *d = TEXT(tv);
    int p;

    p = text_GetBeginningOfLine(d, mark_GetPos(tv->cmdStart));
    if(p != 0) {
	typetext_AlwaysDeleteCharacters((struct typetext*)d, 0, p);
	text_NotifyObservers(d, 0);
    }
}

static void
ClearTypescript(tv)
struct typescript *tv;
{
    ClearTypescriptText(tv);
    if (tv->cmdText != NULL) {
        text_Clear(tv->cmdText);
    }
    tv->lastCmdPos = 0;
}

static void
NoEchoCommand(tv)
struct typescript *tv;
{
    if(tv->readOnlyLen == -1)
        tv->readOnlyLen = 0;
}

/*
 * Reset the tty to a known state (like 'stty sane').
 *
 * This is really vendor-dependent stuff.
 */
static void
ResetTTY(fd)
int fd;	    /* file descriptor for the tty */
{
#if defined(POSIX_ENV) && !defined(sun)
	/* Reset pty with Posix termios ioctl's */
	/* The pty runs in cooked mode.  Set everything to a known state
	 * se we can send signals, etc.
	 */
	{
		struct termios tios;

		tcgetattr(fd, &tios);
		tios.c_iflag = ICRNL;
#ifdef ONLCR
		tios.c_oflag = ONLCR;
#else
		tios.c_oflag = 0;
#endif
		tios.c_cflag = B9600|CS8|HUPCL|CREAD;
		tios.c_lflag = ISIG|ICANON|ECHO;
		tios.c_cc[VINTR] = '\003';	/* ^C */
		tios.c_cc[VQUIT] = '\034';	/* ^\ */
		tios.c_cc[VERASE] = '\010';	/* ^h */
		tios.c_cc[VKILL] = '\025';	/* ^u */
		tios.c_cc[VEOF] = '\004';	/* ^d */
		tios.c_cc[VSTART] = '\021';	/* ^q */
		tios.c_cc[VSTOP] = '\023';	/* ^s */
		tios.c_cc[VSUSP] = '\032';	/* ^z */

		if(tcsetattr(fd, TCSANOW, &tios) < 0)
			perror("typescript: failed to set terminal characteristics");
	}
#else
#if SY_AIX221
	{
	    struct termio ThisTerm;
	    ioctl(fd, TCGETA, &ThisTerm);
	    ThisTerm.c_iflag &= ~ICRNL;
	    ThisTerm.c_oflag &=	~OPOST;	    /* turn all this stuff off */
	    ThisTerm.c_lflag |= ECHO;
	    ThisTerm.c_cflag |=	ISIG;	    /* enable signals */
	    ThisTerm.c_cflag &=	~ICANON;    /* turn off the editing */
	    ioctl(fd, TCSETAW, &ThisTerm);
	}
#else /* #if SY_AIX221 */
	{	/* kernel doesn't reset pty's */
	    struct sgttyb sgttyb;
	    ioctl(fd, TIOCGETP, &sgttyb);
	    sgttyb.sg_flags &= ~CRMOD;
#if (!vax | vax2) /* everything but vax_11 */
	    /*  causes echoing on the vax; needed on RT and sun 
		so the test in ReadFromProcess will work */
	    sgttyb.sg_flags |= ECHO;
#endif /* (!vax | vax2)  */
#ifdef sun
	    /* Running in raw mode introduces other problems.
	       It makes a zero-byte write not introduce an EOF.
	       Don't do it.  "tm" manages to live without it.... */
	    sgttyb.sg_flags &= ~(CBREAK|RAW);
#else
	    /* Run in raw mode to fix select problems. */
	    sgttyb.sg_flags |= RAW;
#endif
	    ioctl (fd, TIOCSETP, &sgttyb);
	}
#endif /* #if SY_AIX221 */
#endif /* defined(POSIX_ENV) && !defined(sun) */
}

boolean
typescript__InitializeObject(classID, tp)
struct classheader *classID;
struct typescript *tp;
{
    int pid;
    char **arglist = NULL;
#if POSIX_ENV || defined(SGI_4D_ENV) || defined(sys_telmat)
    int pgrp = getpgrp();
#else
    int pgrp = getpgrp(0);
#endif
    int ptyChannel;
    int masterChannel;
    char ptyname[64];

#if !(defined(POSIX_ENV) || defined(hpux))
/* Disassociate this process from its controling tty. Must be done after opening the childs pty because that will become the controlling tty if we don't have one. */
    {
	int fd;

#ifdef SIGTTOU
	(void)signal(SIGTTOU, SIG_IGN);    /* For handling signals when changing the window size */
#endif

	fd = open("/dev/tty", O_RDWR);
	if(fd >= 0) {
	    ioctl(fd, TIOCNOTTY, 0);
	    close(fd);
	} 
	else
            setpgrp(0, 0);
      }
#endif

    tp->frame = NULL;
    tp->pipescript = FALSE;
    
    if(Pipescript) {
	tp->pipescript = TRUE;
	Pipescript = FALSE;
    }
    else if(myarglist) {
	arglist = myarglist;
	myarglist = NULL;
    }
    else {
        char *shell = environ_Get("SHELL");

        DefaultArgList[0] = (shell != NULL) ? shell : "/bin/csh";
        arglist = DefaultArgList;
    }
    tp->ChopLines = TRUE;
    tp->OutputWait = 0;
    typescript_SetBorder(tp, 5, 5);
    tp->readOnlyLen = -1;
    tp->lastPosition = -1;
    tp->pgrpid = 0;
    tp->ptyname = NULL;

    if(tp->pipescript) {
	tp->SCFile = df;
	tp->SubChannel = -1;
	typescriptAddSearchMenu ();
	typescriptAddFileMenu();
	tp->keystate = keystate_Create(tp, ssmap);
        tp->menulist = menulist_DuplicateML(typescriptMenus, tp);
        tp->cmdText = NULL;
	maxSize = 100000;
	return TRUE;
    }
    
    tp->progname =  malloc(strlen(*arglist) + 1);
    strcpy(tp->progname, *arglist);
    if(!GetPtyandName(&masterChannel, &ptyChannel, ptyname, sizeof(ptyname))) {
	printf("Can't connect subchannel\n");
	return FALSE;
    }

    tp->SubChannel = masterChannel;
    tp->SlaveChannel = ptyChannel;
    tp->ptyname = malloc(strlen(ptyname) + 1);
    if(tp->ptyname)
	strcpy(tp->ptyname, ptyname);

#if !defined(POSIX_ENV) || defined(sun)
    /* On non-SunOS POSIX systems we do not put the pty into REMOTE mode.
     * Instead we leave the pty in cooked mode.
     * XXX - this distinction needs to be more precise; POSIX systems
     * differ fairly strongly in their pty behaviors, and System V
     * Release 4 and 4.4BSD probably behave more like SunOS.
     */
#if SY_AIX221
    {
#ifndef SOLARIS
	int mode;

	mode = REMOTE;
	ioctl(tp->SubChannel, PTYSETM, &mode);

	/* In order to establish a controlling terminal on AIX (and SysV??)
	 * we need to close the server (slave) and let the child reopen it.
	 * since the child is the first to open it, it will become the
	 * controlling terminal.  The master cannot have it open for this
	 * to happen.  If someone else comes along and opens it between the
	  * close and the open below, we are S.O.L.
	 */
	signal(SIGHUP, SIG_IGN);    /* ignore the hangup when we close the pty */
	close(ptyChannel);	    /* we reopen it in the child */
#endif /* SOLARIS */
    }
#else /* #if SY_AIX221 */
    {
	int ON = 1;
	/* Convert TIOCREMOTE to 4.3 version, then try 4.2 version if
	   that doesn't work.  This is a HACK, and relies upon knowledge
	   of the encoding of ioctl's.  BUT it compiles correctly
	   with the ioctl.h from either 4.2 or 4.3.  What can you do?
	   Note that the ON flag is passed by reference in 4.3, by value in 4.2 */
#if defined(__STDC__) && !defined(__HIGHC__) || defined(_IBMR2) || defined(hpux) || defined(NeXT) || defined(sys_pmax_ul4)  || defined(__GNUC__)
        if(ioctl(tp->SubChannel, _IOW('t', TIOCREMOTE&0xff, int), &ON) == -1)
            ioctl(tp->SubChannel, _IO('t', TIOCREMOTE&0xff), ON);
#else /* defined(__STDC__) && !defined(__HIGHC__) */
	if(ioctl (tp->SubChannel, _IOW(t, TIOCREMOTE&0xff, int), &ON) == -1)
	    ioctl (tp->SubChannel, _IO(t, TIOCREMOTE&0xff), ON);
#endif /* defined(__STDC__) && !defined(__HIGHC__) */
	ioctl(tp->SubChannel, FIOCLEX, 0);
    }
#endif /* #if SY_AIX221 */
#endif /* defined(POSIX_ENV) && !defined(sun) */


    if((pid = osi_vfork ()) < 0) {
	printf("Fork failed\n");
	return FALSE;
    }
    if(pid == 0) {
	int pid = getpid ();
#ifdef POSIX_ENV
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
#else
#ifdef hpux
	setpgrp2(0, pid);
#else /* hpux */
#if SY_AIX221
	setpgrp();
	/* reopen the terminal so it becomes the controlling terminal */
	if((ptyChannel = open(ptyname, 2)) < 0) {
	    fprintf(stderr, "Could not open %s.\n", ptyname);
	    exit(1);
	}
#else /* if !SY_AIX221 */
	setpgrp(0, pid);
#endif /* if SY_AIX221 */
#endif /* hpux */
#endif /* !POSIX_ENV */
        dup2(ptyChannel, 0);
        dup2(ptyChannel, 1);
        dup2(ptyChannel, 2);

	/* Don't leave any unnecessary open file descriptors for child. */
	{
#if !defined(hpux) && !defined(M_UNIX) && !defined(SGI_4D_ENV) && !defined(sys_sun4_51) && !defined(sys_telmat)
	    extern int getdtablesize();
#endif /* hpux */
	    int numfds = getdtablesize();
	    int fd;

#if defined(sys_telmat)
	    if (ioctl(ptyChannel, I_PUSH, "ptem") == -1)
		perror("ioctl(I_PUSH) on ptem failed");
	    if (ioctl(ptyChannel, I_PUSH, "ldterm") == -1)
		perror("ioctl(I_PUSH) on ldterm failed");
#endif /* sys_telmat */

	    for(fd = 3; fd < numfds; fd++)
		close(fd);
	}

#ifdef POSIX_ENV
    /* Set current terminal process group. */
    if(tcsetpgrp(0, pid) < 0)
	perror("tcsetpgrp failed");
#else
#if !SY_AIX221
	ioctl(0, TIOCSPGRP, &pid);
#endif /* if SY_AIX221 */
#endif /* POSIX_ENV */
        environ_Put("TERM", "wm");
        environ_Delete("TERMCAP");
	ResetTTY(0);
	execvp(tp->progname, arglist);
	{   
	    char buf[200];

	    sprintf(buf, "Couldn't exec %s\n", tp->progname);
	    write(1, buf, strlen(buf));
	    _exit(1);
	}
    }
#ifndef POSIX_ENV
#if SY_AIX221
    tp->pgrpid = pid;	/* save pid for sending quit and intr signals */
#else /* SY_AIX221 */
#ifdef hpux
   setpgrp2(0, pgrp);
#else /* hpux */
    setpgrp(0, pgrp);
#endif /* hpux */
#endif /* SY_AIX221 */
#endif /* !POSIX_ENV */

    tp->SCFile = fdopen(tp->SubChannel, "r");
#if SY_AIX12
    setvbuf(tp->SCFile, io_buffer, _IOLBF, sizeof(io_buffer));
#endif

    if(df) {
	struct proctable_Entry *UserMenuProc;
	char nbuf[512];

	UserMenuProc = proctable_DefineProc("Read-User-Menus", typescript_HandleMenus, &typescript_classinfo, NULL, "Handle user supplied menus"); 
	while(fgets(nbuf, sizeof nbuf, df)) {
	    register pos = strlen(nbuf) - 1;
	    if(pos > 0) {
		if(nbuf[pos-1] == '\\')
		    nbuf[pos-1] = '\0';
		else
		    nbuf[pos] = '\r';
		typescriptAddMenu(nbuf, UserMenuProc);
	    }
	}
	fclose(df);
	df = NULL;
    }   
    typescriptAddtypescriptMenus();
    typescriptAddSearchMenu ();
    if(FileMenu) {
	typescriptAddFileMenu();
	FileMenu = FALSE;
    }
    tp->menulist = menulist_DuplicateML(typescriptMenus, tp);
    tp->keystate = keystate_Create(tp, ssmap);
    tp->title = NULL;
    tp->cmdText = text_New();
    tp->lastCmdPos = 0;
#ifdef UTMP_MUNGE
    utmp_add(ptyname, pid);
#endif
    return TRUE;
}

#define COBSIZE 100
/* Called when can send to pty.  Removes handler when no more data remains to be sent. */


static void 
MyCanOutHandler(afile, ad)
FILE *afile;
struct typescript *ad; 
{
    long start;
    register struct text *myd;
    register long i, end;
    register char *tp, tc;
    char buffer[COBSIZE];

    if(ad->SubChannel < 0) 
	return;
    myd = TEXT(ad);
    start = text_GetFence(myd);
    end = text_GetLength(myd);
    if(start >= end) {
        if(ad->readOnlyLen > 0) {
            WritePty(ad, ad->readOnlyBuf, ad->readOnlyLen);
	    smashReadOnlyBuf(ad);
            ad->readOnlyLen = -1;
        }
	ad->OutputWait = 0;
	im_RemoveCanOutHandler(ad->SCFile);
	return;
    }
    tp = buffer;
    for(i = start; i < end;) {
	tc = *tp++ = text_GetChar(myd, i);
	i++;		/* text_GetChar may become macro, so don't do this inside */
	if(tc == '\n') 
	    break;
	if(i >= start + COBSIZE - 2) 
	    break;
    }
    text_SetFence(myd, i);
    WritePty(ad, buffer, tp-buffer);
}

void
typescript__FinalizeObject(classID, ap)
struct classheader *classID;
struct typescript *ap; 
{
  /* dataobject_Destroy(TEXTOBJ(ap)); */ /* the doc will destroy it's own marks */
    if(ap->title)
	free(ap->title);
    if(ap->cmdText)
	text_Destroy(ap->cmdText);
    if(ap->ptyname) {
#ifdef UTMP_MUNGE
        utmp_delete(ap->ptyname);
#endif
	free(ap->ptyname);
    }
}

void 
typescript__ObservedChanged(ap, ov, value)
register struct typescript *ap;
struct observable *ov;
long value; 
{
    register long fencepos;

    /* after output has arrived, make sure view's dot is >= fence */
    if(ov == (struct observable*) TEXT(ap)) {
	if(value == observable_OBJECTDESTROYED)
	    /* the typescript can't do much without it's text */
	    typescript_Destroy(ap);
	else {
	    MaintainLastEnv(ap);
	    fencepos = text_GetFence(TEXT(ap));
	    if((fencepos <= typescript_GetDotPosition(ap)) && typescript_Visible(ap, ap->lastPosition)) {
		typescript_FrameDot(ap, fencepos);
	    }
	    typescript_WantUpdate(ap, ap);
	}
    }
}

void 
typescript__PostMenus(self, menulist)
struct typescript *self;
struct menulist *menulist;
{
    /* Ignore the textviews menus,
      but take advantage of the fact that it knows when to
      post (and retract) the selection menus */
    if(self->header.textview.hasInputFocus) {
	long mask;
	boolean readonly = typescript_GetDotPosition(self) < text_GetFence(TEXT(self));

	mask = ((typescript_GetDotLength(self) > 0) ? typescript_SelectionMenus : typescript_NoSelectionMenus) |
	  (readonly ? 0 : typescript_AfterFenceMenus);

	if(menulist_SetMask(self->menulist, mask))
	    super_PostMenus(self, self->menulist);
    }
}

void 
typescript__ReceiveInputFocus(me)
register struct typescript *me; 
{
    super_ReceiveInputFocus(me);
    me->keystate->next = NULL;
    me->header.textview.keystate->next = NULL; 
    keystate_AddBefore(me->keystate, me->header.textview.keystate); 
    typescript_PostKeyState(me , me->keystate);
    menulist_SetMask(me->menulist, textview_NoMenus);
}

static void 
typescript_handlereadonly(self ,c)
register struct typescript *self;
char c;
{   /* This will put characters in the read-only buffer without displaying them.
      Deals with the no-echo mode for entering passwords and the like. */
#ifdef SENDRAW
    write(self->SubChannel,&c,1);
#else
    if(isprint(c)) {
	if(self->readOnlyLen < (READONLYMAX - 1)) { /* Make sure to leave space for ending carriage return. */
	    self->readOnlyBuf[self->readOnlyLen++] = c;
	}
    }
#endif
}

static int
PositionDot(self)
register struct typescript *self;
{    
    register long dotpos,markpos;
    struct text *d = TEXT(self);

    if((dotpos = typescript_GetDotPosition(self)) < (markpos = text_GetFence(d))){
	while(dotpos < markpos) {
	    if(text_GetChar(d,dotpos) == '\n') {
		textview_EndOfTextCmd(self);
		return;
	    }
	    dotpos++;
	}
	typescript_SetDotPosition(self,dotpos); 
    }
    if(dotpos == markpos) {
/*	modify environment style so environment doesn't get fragmented */
        struct environment *te =  MyEnvinfo(TEXT(self), dotpos);
        if(te->data.style == staticBoldStyle)
            environment_SetStyle(te, TRUE , FALSE);
    }
}

static void 
typescript_YankCmd(self)
register struct typescript *self;
{    
    PositionDot(self);
    textview_YankCmd(self);
}

static void 
Typescript_SelfInsertCmd(self, a)
register struct typescript *self;
register char a;
{
    PositionDot(self);
    if(self->readOnlyLen != -1)
	typescript_handlereadonly(self, a);
    else
   	textview_SelfInsertCmd(self, a);
}

static void 
Typescript_DigitCmd(self, a)
register struct typescript *self;
char a;
{
    PositionDot(self);
    if(self->readOnlyLen != -1)
	typescript_handlereadonly(self, a);
    else
	textview_DigitCmd(self, a);
}

static int
typescript_BackwardsRotatePasteCmd(self)
    struct typescript *self;
{
    if(typescript_GetDotPosition(self) < text_GetFence(TEXT(self)))
	return;
    textview_BackwardsRotatePasteCmd(self);
}

static int
typescript_RotatePasteCmd(self)
struct typescript *self;
{
    if(typescript_GetDotPosition(self) < text_GetFence(TEXT(self)))
	return;
    textview_RotatePasteCmd(self);
}

/* What to do when the textview hasn't defined something... */
int 
typescript_NoTextviewKey(self, key)
struct typescript *self;
long key;
{
    message_DisplayString(self, 0, "Could not execute command. Failure in looking up textview command.");
    return 0;
}

/*
 * Drag&drop support.
 * Drag current working directory (or at least what we think is
 * the current working directory) out onto another window.
 */
static void typescript_DragCwdCmd(self)
struct typescript *self;
{
    struct im *im = typescript_GetIM(self);
    char wd[4096];

    if (im != NULL) {
	getwd(wd);
	im_DropFile(im, wd, NULL);
    }
}

static int typescript_ResetTTY(self)
struct typescript *self;
{
    ResetTTY(self->SlaveChannel);
}


static int
typescriptAddtypescriptMenus()
{
    struct proctable_Entry *tempProc;
    struct classinfo *classInfo = &typescript_classinfo;

    tempProc = proctable_Lookup("typescript-yank");
    keymap_BindToKey(ssmap, "\031", tempProc, 0);

    menulist_AddToML(typescriptMenus,"Paste~11",tempProc,NULL, typescript_AfterFenceMenus | typescript_NoSelectionMenus);

    tempProc = proctable_Lookup("typescript-rotate-backward-paste");
    keymap_BindToKey(ssmap, "\033\031", tempProc, 0);

    tempProc = proctable_Lookup("typescript-rotate-paste");
    keymap_BindToKey(ssmap, "\033y", tempProc, 0);

    tempProc = proctable_Lookup("typescript-Grab-Last-Cmd");
    keymap_BindToKey(ssmap, "\033=", tempProc, 0);

    tempProc = proctable_Lookup("typescript-Grab-Current-Cmd");
    keymap_BindToKey(ssmap, "\033+", tempProc, 0);
    menulist_AddToML(typescriptMenus,"Move~30",tempProc,NULL,0);

    tempProc = proctable_Lookup("typescript-Execute-Current-Cmd");
    keymap_BindToKey(ssmap, "\033\015", tempProc, 0);
    menulist_AddToML(typescriptMenus,"Execute~31",tempProc,NULL,0);

    tempProc = proctable_Lookup("typescript-Grab-Next-Cmd");
    keymap_BindToKey(ssmap, "\033`", tempProc, 0);
}

boolean 
typescript__InitializeClass(classID)
struct classheader *classID;
{
    struct proctable_Entry *tempProc, *si, *dig;
    struct classinfo *classInfo = &typescript_classinfo;
    register long i;
    char str[2];
    maxSize = environ_GetProfileInt("maxsize", 10000);
    extraRoom = maxSize / 10;

    CmdSize = 256;
    cmd = (char*) malloc(CmdSize);
    ssmap = keymap_New();
    typescriptMenus = menulist_New();

/* Initialize our pointers to textview command routines. */
    class_Load("textview"); /* Make sure the textview is loaded first. */
    if(tempProc = proctable_Lookup("textview-line-to-top")) {
        proctable_ForceLoaded(tempProc);
        textview_LineToTop = proctable_GetFunction(tempProc);
    }
    else
        textview_LineToTop = typescript_NoTextviewKey;

    if(tempProc = proctable_Lookup("textview-end-of-text")) {
        textview_EndOfTextCmd = proctable_GetFunction(tempProc);
    }
    else
        textview_EndOfTextCmd = typescript_NoTextviewKey;

    if(tempProc = proctable_Lookup("textview-self-insert"))
        textview_SelfInsertCmd = proctable_GetFunction(tempProc);
    else
        textview_SelfInsertCmd = typescript_NoTextviewKey;

    if(tempProc = proctable_Lookup("textview-digit"))
        textview_DigitCmd = proctable_GetFunction(tempProc);
    else
        textview_DigitCmd = typescript_NoTextviewKey;

    if(tempProc = proctable_Lookup("textview-beginning-of-line"))
        textview_BeginningOfLineCmd = proctable_GetFunction(tempProc);
    else
        textview_BeginningOfLineCmd = typescript_NoTextviewKey;

    if(tempProc = proctable_Lookup("textview-end-of-line"))
        textview_EndOfLineCmd = proctable_GetFunction(tempProc);
    else
        textview_EndOfLineCmd = typescript_NoTextviewKey;

    if(tempProc = proctable_Lookup("textview-yank"))
        textview_YankCmd = proctable_GetFunction(tempProc);
    else
        textview_YankCmd = typescript_NoTextviewKey;

    if(tempProc = proctable_Lookup("textview-rotate-backward-paste"))
        textview_BackwardsRotatePasteCmd = proctable_GetFunction(tempProc);
    else
        textview_BackwardsRotatePasteCmd = typescript_NoTextviewKey;

    if(tempProc = proctable_Lookup("textview-rotate-paste"))
        textview_RotatePasteCmd = proctable_GetFunction(tempProc);
    else
        textview_RotatePasteCmd = typescript_NoTextviewKey;

    if(tempProc = proctable_Lookup("textview-delete-next-character"))
        textview_DeleteCmd = proctable_GetFunction(tempProc);
    else
        textview_DeleteCmd = typescript_NoTextviewKey;

    if(tempProc = proctable_Lookup("textview-copy-region"))
        typescript_CopyRegionCmd = proctable_GetFunction(tempProc);
    else
        typescript_CopyRegionCmd = typescript_NoTextviewKey;

    if(tempProc = proctable_Lookup("textview-zap-region"))
        typescript_ZapRegionCmd = proctable_GetFunction(tempProc);
    else
        typescript_ZapRegionCmd = typescript_NoTextviewKey;

    if(tempProc	= proctable_Lookup("textview-delete-previous-character")    )
        textview_RuboutCmd = proctable_GetFunction(tempProc);
    else
        textview_RuboutCmd = typescript_NoTextviewKey;

    tempProc = proctable_DefineProc("typescript-interupt-command", (procedure) TypescriptINTCommand, classInfo, NULL, "Handle ^C");
    keymap_BindToKey(ssmap, "\003", tempProc, 0);
#if SY_AIX221
    keymap_BindToKey(ssmap, "\177", tempProc, 0);   /* bind DEL too! */
#endif /* #if SY_AIX221 */

    tempProc = proctable_DefineProc("typescript-beginning-of-line", (procedure) TypescriptLeftCommand, classInfo, NULL, "Move to line beginning");
    keymap_BindToKey(ssmap, "\001", tempProc, 0);

    tempProc = proctable_DefineProc("typescript-end-of-line", (procedure) TypescriptEndOfLineCommand, classInfo, NULL, "Move to line end");
    keymap_BindToKey(ssmap, "\005", tempProc, 0);

    tempProc = proctable_DefineProc("typescript-EOT-command", (procedure) TypescriptEOTCommand, classInfo, NULL, "Handle ^D");
    keymap_BindToKey(ssmap, "\004", tempProc, 0);

    tempProc = proctable_DefineProc("typescript-return-cmd", (procedure) TypescriptReturnCommand, classInfo, NULL, "Handle enter key");
    keymap_BindToKey(ssmap, "\015", tempProc, 0);

    tempProc = proctable_DefineProc("typescript-return-and-position", (procedure) TypescriptReturnAndPositionCommand, classInfo, NULL, "Handle enter key and place command at top of the screen");
    keymap_BindToKey(ssmap, "\012", tempProc, 0);

    tempProc = proctable_DefineProc("typescript-ZAP-command", (procedure) TypescriptZapCommand, classInfo, NULL, "handles ^U line clearing");
    keymap_BindToKey(ssmap, "\025", tempProc, 0);

    tempProc = proctable_DefineProc("typescript-Stop-cmd", (procedure) TypescriptSTOPCommand, classInfo, NULL, "Handle ^Z Stop cmd");
    keymap_BindToKey(ssmap, "\032", tempProc, 0);

    tempProc = proctable_DefineProc("typescript-unbound", (procedure) TypescriptUnboundCommand, classInfo, NULL, "Does nothing.");
    keymap_BindToKey(ssmap, "\033~", tempProc, 0);

    tempProc = proctable_DefineProc("typescript-zap-region", (procedure) typescript_ZapRegionCmd, classInfo, NULL, "Cut region to kill-buffer."); 
    keymap_BindToKey(ssmap, "\027", tempProc, 0);
    menulist_AddToML(typescriptMenus, "Cut~11", tempProc, NULL, typescript_AfterFenceMenus | typescript_SelectionMenus);

    tempProc = proctable_DefineProc("typescript-copy-region", (procedure) typescript_CopyRegionCmd, classInfo, NULL, "Copy region to kill-buffer.");
    keymap_BindToKey(ssmap, "\033w", tempProc, 0);
    menulist_AddToML(typescriptMenus, "Copy~12", tempProc, NULL, typescript_SelectionMenus);

    tempProc = proctable_DefineProc("typescript-QUIT-command", (procedure) TypescriptQUITCommand, classInfo, NULL, "handles quit cmd");
    keymap_BindToKey(ssmap, "\034", tempProc, 0);

    tempProc = proctable_DefineProc("typescript-No-Echo-Cmd", (procedure) NoEchoCommand, classInfo, NULL, "Turn Off Echoing");
    keymap_BindToKey(ssmap, "\030\014", tempProc, 0);

    tempProc = proctable_DefineProc("typescript-delete-previous-character", (procedure) typescript_RuboutCmd, classInfo, NULL, "Delete the previous character.");
    keymap_BindToKey(ssmap, "\010", tempProc, 0); 
    keymap_BindToKey(ssmap, "\177", tempProc, 0);
 
    si = proctable_DefineProc("Typescript-self-insert", (procedure) Typescript_SelfInsertCmd, classInfo, NULL, "Insert a character.");
    dig = proctable_DefineProc("Typescript-digit", (procedure) Typescript_DigitCmd, classInfo, NULL, "Insert a character.");
    str[0] = ' ';
    str[1] = '\0';
    for(i = 32; i < 127; i++)  {
	if(i < 48 || i > 57)
	    keymap_BindToKey(ssmap, str, si, i);
	else
	    keymap_BindToKey(ssmap, str, dig, i);
	str[0]++;
    }

    tempProc = proctable_DefineProc("typescript-Clear", (procedure) ClearTypescript, classInfo, NULL, "Clear Typescript");
    menulist_AddToML(typescriptMenus,"Clear~51",tempProc,NULL,0);

    tempProc = proctable_DefineProc("typescript-Clear-Text", (procedure) ClearTypescriptText, classInfo, NULL, "Clear the text of the typescript");

    tempProc = proctable_DefineProc("typescript-drag-cwd", (procedure) typescript_DragCwdCmd, classInfo, NULL, "Drag out current working directory.");
    tempProc = proctable_DefineProc("typescript-reset-tty", (procedure) typescript_ResetTTY, classInfo, NULL, "Reset the tty (stty sane)");
    keymap_BindToKey(ssmap, "\033r", tempProc, 0);

/* We need to define these here so that fcomp can look them up before a tscript object is created. */
    tempProc = proctable_DefineProc("typescript-yank", (procedure) typescript_YankCmd, classInfo, NULL, "Yank text back from kill-buffer.");

    tempProc = proctable_DefineProc("typescript-rotate-backward-paste", (procedure) typescript_BackwardsRotatePasteCmd, classInfo, NULL, "Rotate kill-buffer backwards.");

    tempProc = proctable_DefineProc("typescript-rotate-paste", (procedure) typescript_RotatePasteCmd, classInfo, NULL, "Rotate kill-buffer.");

    tempProc = proctable_DefineProc("typescript-Grab-Last-Cmd", (procedure) GrabLastCommand, classInfo, NULL, "Grab Last Command");

    tempProc = proctable_DefineProc("typescript-Grab-Current-Cmd", (procedure) GrabCurrentCommand, classInfo, NULL, "Grab Current Command");

    tempProc = proctable_DefineProc("typescript-Execute-Current-Cmd", (procedure) ExecuteCurrentCommand, classInfo, NULL, "Execute Current Command");

    tempProc = proctable_DefineProc("typescript-Grab-Next-Cmd", (procedure) GrabNextCommand, classInfo, NULL, "Grab Next Command");

    return TRUE;
}

void 
typescript__Update(self)
struct typescript *self;
{
    self->lastPosition = -1;
    super_Update(self);
}

struct view *
typescript__Hit(self, action, x, y, numberOfClicks)
struct typescript *self;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    struct view *v;
    struct im *im;
    int i;
    char *p;

    v = super_Hit(self, action, x, y, numberOfClicks);
    if (action == view_LeftFileDrop) {
	char **files;
	struct im *im = typescript_GetIM(self);

	if (im == NULL) return v;
	files = im_GetDroppedFiles(im);
	if (files) {
	    if (files[0] != NULL)
		free(files[0]);	/* ignore host for now */
	    for (i = 1; files[i] != NULL; i++) {
		if (i != 0)
		    textview_SelfInsertCmd(self, ' ');
		for (p = files[i]; *p != '\0'; p++)
		    textview_SelfInsertCmd(self, *p);
		free(files[i]);
	    }
	    free(files);
	}
    }
    return v;
}

#ifdef TIOCGWINSZ
static int
NullWinSizeProc()
{
}
#endif /* TIOCGWINSZ */

void 
typescript__FullUpdate(self, type, left, top, width, height)
struct typescript *self;
enum view_UpdateType type;
long left;
long top;
long width;
long height;
{
    self->lastPosition = -1;
    super_FullUpdate(self, type, left, top, width, height);

#ifdef TIOCGWINSZ
    {
	struct style *tsStyle;
	enum style_FontSize dummy;
	char ffamily[1000];
	long tssize;
	struct fontdesc *tsfont;
	struct FontSummary *fontSummary;
	long widthinchar;
	struct winsize winSize;

	im_SignalHandler(SIGWINCH, NullWinSizeProc, NULL);

	tsStyle = typescript_GetDefaultStyle(self);
	style_GetFontSize(tsStyle, &dummy, &tssize);
	style_GetFontFamily(tsStyle, ffamily, 1000);
	style_AddTabsCharacters(tsStyle);
	tsfont = fontdesc_Create(ffamily, style_GetAddedFontFaces(tsStyle), tssize);
	fontSummary = fontdesc_FontSummary(tsfont, typescript_GetDrawable(self));
	if(fontSummary) widthinchar = (width / fontSummary->maxSpacing) - 1;
	else widthinchar = 12;

	ioctl (self->SubChannel, TIOCGWINSZ, &winSize);
	if(winSize.ws_col != widthinchar) {
	    winSize.ws_col = widthinchar;
	    ioctl(self->SubChannel, TIOCSWINSZ, &winSize);
	}
    }
#endif /* TIOCGWINSZ */
}

void 
typescript__GetClickPosition(self, position, numberOfClicks, action, startLeft, startRight, leftPos, rightPos)
struct typescript *self;
long position;
long numberOfClicks;
enum view_MouseAction action;
long startLeft;
long startRight;
long *leftPos;
long *rightPos;
{
    if(numberOfClicks  %3) {
	super_GetClickPosition(self, position, numberOfClicks, action, startLeft, startRight, leftPos, rightPos);
	return;
    }
    if(GetCommandEnv(self, position, leftPos, rightPos) == NULL)
	super_GetClickPosition(self, position, numberOfClicks, action, startLeft, startRight, leftPos, rightPos);
}


static int 
WritePty(tv, buf, len)
struct typescript *tv;
char *buf;
int len;
{
#if defined(POSIX_ENV) && !defined(sun)
/* Write to a non-SunOS POSIX pty.  Turn off echo before the write,
 * then reset echo to its previous state after the write.
 * XXX - this distinction needs to be more precise; POSIX systems
 * differ fairly strongly in their pty behaviors, and System V
 * Release 4 and 4.4BSD probably behave more like SunOS. */
    int ret;
    struct termios tios;

    /* Save flags. */
    tcgetattr(tv->SlaveChannel, &tios);
    tios.c_lflag &= ~ECHO;	/* turn off echo for a moment */
    tcsetattr(tv->SlaveChannel, TCSANOW, &tios);
    /* Write the data */
    ret = write(tv->SubChannel, buf, len);
    /* Put echo back */
    tios.c_lflag |= ECHO;	/* assume it was on!! */
    tcsetattr(tv->SlaveChannel, TCSANOW, &tios);
    return ret;
#else /* defined(POSIX_ENV) && !defined(sun) */
/* Non-POSIX or SunOS...just write to the pty. */
    return write(tv->SubChannel, buf, len);
#endif /* defined(POSIX_ENV) && !defined(sun) */
}

void 
typescript__SetDataObject(tp, obj)
struct typescript *tp;
struct dataobject *obj;
{
    struct style *defaultStyle;
    char bodyFont[100];
    long fontSize = 10;
    long fontStyle = fontdesc_Fixed;
    char *font;
    long stpos;
    struct typetext *shtext = (struct typetext*) obj;
    struct text *SsText = (struct text*) shtext;

    super_SetDataObject(tp, obj);
    if(!staticBoldStyle) {
	if((staticBoldStyle = stylesheet_Find(SsText->styleSheet, "bold")) == NULL) {
	    staticBoldStyle = style_New();
	    style_SetName(staticBoldStyle, "bold");
	    stylesheet_Add(SsText->styleSheet, staticBoldStyle);
	}
	style_AddNewFontFace(staticBoldStyle, fontdesc_Bold);
    }
    if((defaultStyle = typescript_GetDefaultStyle(tp)) == NULL) {
	defaultStyle = style_New();
	style_SetName(defaultStyle, "default");
    }
    if((font = environ_GetProfile("typescript.bodyfont")) == NULL || ! fontdesc_ExplodeFontName(font, bodyFont, sizeof(bodyFont), &fontStyle, &fontSize)) {
	strcpy(bodyFont, "AndyType");
	fontSize = 10;
	fontStyle = fontdesc_Fixed;
    }
    style_SetFontFamily(defaultStyle, bodyFont);
    style_SetFontSize(defaultStyle, style_ConstantFontSize, fontSize);
    style_ClearNewFontFaces(defaultStyle);
    style_AddNewFontFace(defaultStyle, fontStyle);
    style_SetJustification(defaultStyle, style_LeftJustified);
    style_SetNewLeftMargin(defaultStyle,style_LeftMargin,16384, style_Inches);
    style_SetNewIndentation(defaultStyle,style_LeftEdge,-16384, style_Inches);
    typescript_SetDefaultStyle(tp, defaultStyle);
    stpos = text_GetLength(SsText);
    tp->cmdStart = text_CreateMark(SsText,stpos,0);
    text_SetFence(SsText, stpos);
    mark_IncludeBeginning(tp->cmdStart) = TRUE;
    if(!shtext->hashandler) {
	im_AddFileHandler(tp->SCFile, (procedure) ReadFromProcess, (char*) tp, 6);
	shtext->hashandler = TRUE;
    }
    typescript_SetDotPosition(tp,stpos);
    im_ForceUpdate();
}

void 
typescript__SetTitle(self, title)
struct typescript *self;
char *title;
{
    if(self->title != NULL)
	free(self->title);
    if(title || *title != '\0') {
	self->title = (char*) malloc(strlen(title) + 1);
	if(self->title)
	    strcpy(self->title, title);
    }
    else
	self->title = NULL;
}

char *
typescript__GetTitle(self)
struct typescript *self;
{
    return self->title;
}

/*
 * Set/Get frame.
 * Typescriptapp sets the initial frame.  When the title
 * changes (via the ^A(pwd)^B hack), typescript then has
 * a frame which can be given the new title.
 */
void 
typescript__SetFrame(self, frame)
struct typescript *self;
struct frame *frame;
{
    self->frame = frame;
}

struct frame *
typescript__GetFrame(self)
struct typescript *self;
{
    return self->frame;
}

#ifdef UTMP_MUNGE
/*
 * The following establishes a utmp entry if /etc/utmp is writeable.
 * Since typescript should never be setuid root (implying ez and everything else is),
 * this will only work on relaxed systems where /etc/utmp is open.
 */
int 
utmp_add(ptyname, pid)
char *ptyname;
int pid;
{
    struct utmp utmp;
    char *username;

    bzero(&utmp, sizeof(utmp));
    username = cuserid(NULL);
    if(username) {
        strncpy(utmp.ut_user, username, sizeof(utmp.ut_user));

	/* skip /dev/ */
        strncpy(utmp.ut_line, ptyname + 5, sizeof(utmp.ut_line));

	/* skip /dev/ */
        strncpy(utmp.ut_id, ptyname + 5, sizeof(utmp.ut_id));

        utmp.ut_type = USER_PROCESS;
        utmp.ut_pid = pid; /* pid of shell or command */
        utmp.ut_time = time(NULL);
        strncpy(utmp.ut_host, "typescript", sizeof(utmp.ut_host));
        setutent();
        pututline(&utmp); /* This may fail on secure systems. */
    }
}

int 
utmp_delete(ptyname)
char *ptyname;
{
    struct utmp utmp;

    bzero(&utmp, sizeof(utmp));
    setutent();

    /* skip /dev/ */
    strncpy(utmp.ut_line, ptyname+5, sizeof(utmp.ut_line));

    /* skip /dev/ */
    strncpy(utmp.ut_id, ptyname+5, sizeof(utmp.ut_id));

    utmp.ut_type = DEAD_PROCESS;
    pututline(&utmp);	/* This may fail on secure systems. */
}
#endif
