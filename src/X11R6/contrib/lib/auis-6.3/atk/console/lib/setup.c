/* ********************************************************************** *\
 *	   Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/setup.c,v 2.72 1994/01/30 16:42:23 rr2b Exp $";
#endif

/* 
 ***************************************************************
 * Routines for reading in .console files for Instrument Console
 ****************************************************************
 */

#include <andrewos.h>
#include <class.h>
#include <conclass.ih>
#include <im.ih>
#include <view.ih>
#include <menulist.ih>
#include <graphic.ih>
#include <fontdesc.ih>
#include <envrment.ih>
#include <environ.ih>
#include <cursor.ih>
#include <filetype.ih> /* ~ expansion */
#include <logv.ih>
#include <scroll.ih>
#include <text.ih>
#include <console.h>
#include <convers.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <signal.h>
#include <ctype.h>
#include <sys/errno.h>
#include <sys/wait.h>
#include <sitevars.h>


extern ClearWindow();
extern PromptToWindow();
extern GetStringFromWindow();
extern InitPstrings();
extern int Pposx, Pposy;
extern char Pstring1[256], Pstring2[256], Pstring3[256], Pstring4[MAXPATHLEN];

static struct display VirginDisplay;
struct fontdesc *icon12font,
    *console10font;
int BogusInt; /* for storing values for AlarmRect which are now ignored */
extern int DynamicXmin,
    DynamicXmax,
    DynamicYmin,
    DynamicYmax,
    OutgoingAge;
extern char EXTENSION[];
extern int DrawDebug();
extern struct RegionLog *WhichErrorLog();
extern char OtherVenusStr[],
    FetchVenusStr[],
    FinishedVenusStr[],
    PrimaryErrorBuffer[];

/* MAXCHILDPIDS is from console.h */
int	children[MAXCHILDPIDS] = { -1 };
int	theGetstatsPid = -1;

boolean	HideIt = FALSE,
    ParsingError,
    LogErrorsExternally;
FILE	*ExternalLogFP = NULL;
char	ParsingErrorText[128];
extern int	ForceErrorMonitoring;

extern struct classinfo *consoleClass_GetInfoHack();
extern struct menulist *PrepareUserMenus();

void ToggleDebugging (self, rock)
    struct consoleClass *self;
    char *rock;
{
    mydbg(("Entering: ToggleDebugging\n"));
    MYDEBUGGING = ! MYDEBUGGING;
}

void Reaper ()
{
    int pid;
    int *child;
    /* the following counter is added as an array bounds check */
    int i;
#if POSIX_ENV
    int status;
#else
    union wait status;
#endif

    mydbg(("Entering: Reaper\n"));
    i = 0;
#if POSIX_ENV
    while ((pid = waitpid(-1, &status, WNOHANG)) > 0) {
#else
    while ((pid = wait3(&status, WNOHANG, NULL)) > 0) {
#endif
	for (child = children; *child != -1 && i < MAXCHILDPIDS; child++) {
	    i++;
	    if (*child == pid) {
		if (child[1] == -1) {
		    *child = -1;
		} else {
		    *child = 0;
		}
		break;
	    }
	}
    }
}

#ifdef POSIX_ENV
    void DieGracefully(
#ifdef __STDC__		       
		       int sig
#endif
		       )
#else
DieGracefully()
#endif
{
    mydbg(("Entering: DieGracefully\n"));
    KillInitExecProcesses(TRUE);
    exit(1);
}

struct FuncStruct {
    char *keyword, *defaultleftclick;
    int datumindex;
};

struct FuncStruct AuxFuncParse[] = {
    "disk", "Disk * is $% full.",  -1,
    "directory", "There are $ files in directory *.", -1,
    "external", "External value $ text *.", -1,
    0, 0, 0
};

struct FuncStruct FuncParse[] = {
        "loadcpu", "The CPU load is $%.",  LOADCPU,
        "loadio", "The I/O load is $%.",  LOADIO,
        "loaduser", "The user load is $%.",  LOADUSER,
        "loadsys", "The system load is $%.",  LOADSYS,
        "loadidle", "The system is idle $% of the time.",  LOADIDLE,
        "vm", "Virtual memory utilization is $%.",  VM,
#if defined(sys_telmat)
        "pagein", "$ pages in.",  PAGE_IN,
        "pageerr", "$ pages out.",  PAGE_OUT,
#else
          "pagein", "$ pages in.",  PAGEIN,
          "pageerr", "$ pages out.",  PAGEOUT,
#endif
	"pagereplacable","$ replacable pages.",  PAGEREPLACABLE,
        "pagereplaceable","$ replaceable pages.", PAGEREPLACABLE,
        "pagedeficit", "$ page deficit.",  PAGEDEFICIT,
        "memactive", "$ active pages in memory.",  MEMACTIVE,
        "memfree", "$ pages in free list.",  MEMFREE,
        "queuerun", "There are $ processes waiting to run.",  QUEUERUN,
        "queueblock", "$ blocked processes.",  QUEUEBLOCK,
        "queuemem", "$ processes awaiting memory.",  QUEUEMEM,
        "intsio", "$ I/O interrupts.",  INTSIO,
        "intssys", "$ sys call interrupts.",  INTSSYS,
        "intsswap", "$ context swaps.",  INTSSWAP,
        "ndstatin", "$ network disk in.",  NDSTATIN,
        "ndstatout", "$ network disk out.",  NDSTATOUT,
        "ndstaterr", "$ network disk errors.",  NDSTATERR,
        "marinerfetch", "!@#MARINERFETCH: ",  MARINERFETCH,
        "marinerother", "*",  MARINEROTHER,
        "clockhours", "!@#FULLTIME: ",  CLOCKHOURS,
        "clockhourfifths", "!@#FULLTIME: ",  CLOCKHOURFIFTHS,
        "clockminutes", "!@#FULLTIME: ",  CLOCKMINUTES,
        "clockseconds", "!@#FULLTIME: ",  CLOCKSECONDS,
        "clockall", "!@#FULLTIME: ",  CLOCKALL,
        "date", "Date: *",  DATE,
        "mail", "!@#MAILSTATUS: ",  MAIL,
        "errors", "$ errors have been logged on the console.",  ERRORS,
        "alarm", "*",  ALARM,
        "always", "This is a display that always appears.",  ALWAYS,
        "procsuser", "$% of the maximum user processes are in use.",  PROCSUSER,
        "procstotal", "$% of the maximum total processes are in use.",  PROCSTOTAL,
        "trouble", "!@#TROUBLE: yes",  TROUBLE,
        "errorlog", "",  ERRORLOG,
        "reportlog", "",  REPORTLOG,
        "userlog", "",  USERLOG,
        "sillylog", "",  SILLYLOG,
        "windowuse", "$% of the maximum available windows are in use.",  WINDOWUSE,
        "printqueue", "!@#PRINTSTATUS: yes",  PRINTQUEUE,
        "printsent", "$ file(s) have been shipped but not yet printed.",  PRINTSENT,
        "printerrors", "$ file(s) have not been printed due to errors.",  PRINTERRORS,
        "marinerfinished", "*",  MARINERFINISHED,
        "vicepersonal", "Your AFS quota is $% full",   VICEPERSONAL,
        "vicepartition", "Your AFS partition is $% full",   VICEPARTITION,
        "netresponses", "Netresponses not implemented yet...",  NETRESPONSES,
        "udpidle", "There have been no UDP messages for $ seconds.",  UDPIDLE,
        "unauthenticated", "Your AFS connection has expired; use 'klog' to reconnect.",  UNAUTHENTICATED,
        "procsother", "$ processes are in use by other users.",  PROCSOTHER,
        "outgoingmail", "$ pieces of mail you sent are not yet delivered.", OUTGOINGMAIL,
        0, 0, 0,
};

char *DisplayTypeTable[]= {
	"debug",
	"debug",
#define DISPFLAG_DEBUG 1
	"ekggraph",
#define DISPFLAG_EKGGRAPH 2
	"gauge",
#define DISPFLAG_GAUGE 3
	"dial",
#define DISPFLAG_DIAL 4
	"indicator",
#define DISPFLAG_INDICATOR 5
	"bargraph",
#define DISPFLAG_BARGRAPH 6
	"ringalarm",
#define DISPFLAG_RINGALARM 7
	"signaltrouble",
#define DISPFLAG_SIGNALTROUBLE 8
	"logerror",
#define DISPFLAG_LOGERROR 9
	"logreport",
#define DISPFLAG_LOGREPORT 10
	"loguser",
#define DISPFLAG_LOGUSER 11
	"logsilly",
#define DISPFLAG_LOGSILLY 12
	"drawlog",
#define DISPFLAG_DRAWLOG 13
	"drawtitle",
#define DISPFLAG_DRAWTITLE 14
	"drawnothing",
#define DISPFLAG_DRAWNOTHING 15
	0
};

char *StyleTable[] = {
	"logindicator",
#define SFLAG_LOGINDICATOR 0
	"repeatindicator",
#define SFLAG_REPEATINDICATOR 1
	"dialhidden",
#define SFLAG_DIALHIDDEN 2
	"leftindicator",
#define SFLAG_LEFTINDICATOR 3
	"reversescrolling",
#define SFLAG_REVERSESCROLLING 4
	"squaredial",
#define SFLAG_SQUAREDIAL 5
	"horizontal",
#define SFLAG_HORIZONTAL 6
	0
};

char *OptionTable[] =  {
	"xmin",
#define FLAG_XMIN 0
	"xmax",
#define FLAG_XMAX 1
	"ymin",
#define FLAG_YMIN 2
	"ymax",
#define FLAG_YMAX 3
	"function",
#define FLAG_FUNCTION 4
	"initstring",
#define FLAG_INITSTRING 5
	"displaytype",  
#define FLAG_DISPLAYTYPE 6
	"boxed",
#define FLAG_BOXED 7
	"valuemax",
#define FLAG_VALUEMAX 8
	"label",
#define FLAG_LABEL 9
	"promptfont",
#define FLAG_PROMPTFONT 10
	"loglength",
#define FLAG_LOGLENGTH 11
	"handlength",
#define FLAG_HANDLENGTH 12
	"handwidth",
#define FLAG_HANDWIDTH 13
	"displaystyle",
#define FLAG_DISPLAYSTYLE 14
	"threshhold",
#define FLAG_THRESHHOLD 15
	"adjustlabelfont",
#define FLAG_ADJUSTLABELFONT 16
	"adjusttextfont",
#define FLAG_ADJUSTTEXTFONT 17
	"labelfont",
#define FLAG_LABELFONT  18
	"textfont",
#define FLAG_TEXTFONT 19
	"instrument",
#define FLAG_NEWINSTRUMENT 20
	"text",
#define FLAG_TEXT 21
	"desiredwidth",
#define FLAG_DESIREDWIDTH 22
	"desiredheight",
#define FLAG_DESIREDHEIGHT 23
	"period",
#define FLAG_PERIOD 24
	"set",
#define FLAG_SET 25
	"global",
#define FLAG_GLOBAL 26
	"vmpollfrequency",
#define FLAG_VMPOLLFREQ 27
	"diskpollfrequency",
#define FLAG_DISKPOLLFREQ 28
	"mailpollfrequency",
#define FLAG_MAILPOLLFREQ 29
	"clockpollfrequency",
#define FLAG_CLOCKPOLLFREQ 30
	"leftlabel",
#define FLAG_LEFTLABEL 31
	"rightlabel",
#define FLAG_RIGHTLABEL 32
	"toplabel",
#define FLAG_TOPLABEL 33
	"bottomlabel",
#define FLAG_BOTTOMLABEL 34
	"scalefactor",
#define FLAG_SCALEFACTOR 35
	"noclip",
#define FLAG_NOCLIP 36
	"vmpollperiod",
#define FLAG_VMPOLLPERIOD 37
	"diskpollperiod",
#define FLAG_DISKPOLLPERIOD 38
	"mailpollperiod",
#define FLAG_MAILPOLLPERIOD 39
	"clockpollperiod",
#define FLAG_CLOCKPOLLPERIOD 40
	"icon",
#define FLAG_ICON 41
	"iconfont",
#define FLAG_ICONFONT 42
	"fontfamily",
#define FLAG_FONTFAMILY 43
	"maxlabelfontsize",
#define FLAG_MAXLABELFONTSIZE 44
	"maxtextfontsize",
#define FLAG_MAXTEXTFONTSIZE 45
	"ceiling",
#define FLAG_CEILING 46
	"noblank",
#define FLAG_NOBLANK 47
	"leftclickto",
#define FLAG_LEFTCLICKTO 48
	"leftclickstring",
#define FLAG_LEFTCLICKSTRING 49
	"externalname",
#define FLAG_EXTERNALNAME 50
	"windowpollperiod",
#define FLAG_WINDOWPOLLPERIOD 51
	"flashmin",
#define FLAG_FLASHMIN 52
	"flashmax",
#define FLAG_FLASHMAX 53
	"minheight",
#define FLAG_MINHEIGHT 54
	"maxheight",
#define FLAG_MAXHEIGHT 55
	"minwidth",
#define FLAG_MINWIDTH 56
	"maxwidth",
#define FLAG_MAXWIDTH 57
	"noinvisibleclick",
#define FLAG_NOINVISIBLECLICK 58
	"if",
#define FLAG_IF 59
	"unless",
#define FLAG_UNLESS 60
	"internalmenus",
#define FLAG_INTERNALMENUS 61
	"remotefunction",
#define FLAG_REMOTEFUNCTION 62
	"dirpollperiod",
#define FLAG_DIRPOLLPERIOD 63
	"printpollperiod",
#define FLAG_PRINTPOLLPERIOD 64
	"highlightboxmin",
#define FLAG_HIGHLIGHTBOXMIN 65
	"highlightboxmax",
#define FLAG_HIGHLIGHTBOXMAX 66
	"highlightnotchmin",
#define FLAG_HIGHLIGHTNOTCHMIN 67
	"highlightnotchmax",
#define FLAG_HIGHLIGHTNOTCHMAX 68
	"highlightinvertmin",
#define FLAG_HIGHLIGHTINVERTMIN 69
	"highlightinvertmax",
#define FLAG_HIGHLIGHTINVERTMAX 70
	"alarmrectangle",
#define FLAG_ALARMRECTANGLE 71
	"columns",
#define FLAG_COLUMNS 72
	"machines",
#define FLAG_MACHINES 73
	"fillchar",
#define FLAG_FILLCHAR 74
	"netpollfreq",
#define FLAG_NETPOLLFREQ 75
	"netpollperiod",
#define FLAG_NETPOLLPERIOD 76
	"externalerrorfile",
#define FLAG_EXTERNALERRORFILE 77
	"dynamicrectangle",
#define FLAG_DYNAMICRECTANGLE 78
	"venuspollperiod",
#define FLAG_VENUSPOLLPERIOD 79
	"updatealways",
#define FLAG_ALWAYSUPDATE 80
	"initexec",
#define FLAG_INITEXEC 81
	"outgoingage",
#define FLAG_OUTGOINGAGE 82
	"statusserver",
#define FLAG_STATUSSERVER 83
	0};
char *ClickOptions[] = {
	"errorlog",
#define CLICK_ERRORLOG 0
	"reportlog",
#define CLICK_REPORTLOG 1
	"userlog",
#define CLICK_USERLOG 2
	"sillylog",
#define CLICK_SILLYLOG 3
	0
};


int LaccNameLookup(string, table)
char *string;
char **table;
{
    int i;

    if (!string) return(-1);
    for (i = 0; table[i] != 0; i++){
	if (!lc_strcmp(string, table[i])){
	    return(i);
	}
    }
    return(-1);
}


struct FuncStruct *AuxFuncLookup(string, index, len, num)
char *string;
int index;
int len, *num;
{
    struct FuncStruct *myfs;

    myfs = &AuxFuncParse[index];
    if (!lc_strncmp(string, myfs->keyword, len)){
	char *c;
	c = string;
	c += len;
	*num = atoi(c);
	return(myfs);
    }
    return(NULL);
}

struct FuncStruct *FuncLookup(string)
    char *string;
{
    struct FuncStruct *myfs;
    int num;
    if (string == NULL || *string == NULL) return(NULL);
    for (myfs = &FuncParse[0]; myfs->datumindex; ++myfs) {
        if (!lc_strcmp(string, myfs->keyword)) return(myfs);
    }
    if ((myfs = AuxFuncLookup(string, 0, 4, &num)) != NULL){
	myfs->datumindex = DISK1 + num - 1;
	return(myfs);
    }
    if ((myfs = AuxFuncLookup(string, 1, 9, &num)) != NULL){
	myfs->datumindex = DIRECTORY1 + num - 1;
	return(myfs);
    }
    if ((myfs = AuxFuncLookup(string, 2, 8, &num)) != NULL){
	myfs->datumindex = EXTERNAL1 - num + 1;
	return(myfs);
    }
    return(NULL);
}


struct fontdesc *SetupFont(fontname)
char *fontname;
{
    struct fontdesc *font;
    char FontBuffer[50];
    long FontSize, FontStyle;

    if (fontname == NULL || *fontname == '\0') {
	font = NULL;
    } else {
	bzero(FontBuffer, 50);
	fontdesc_ExplodeFontName(fontname, FontBuffer, sizeof(FontBuffer), &FontStyle, &FontSize);
	font = fontdesc_Create(FontBuffer, FontStyle, FontSize);
    }
    if(font == NULL){
	mydbg(("SetupFont(%s) failed!\n", fontname ? fontname : "NULL NAME"));
    }
    return(font);
}



#define NOMORETOKENS '\0'
#define MAXTOKENSIZE 256
char *GetNextToken(self, MapToLower, fp, lineno, PanicAtEOF)
    struct consoleClass *self;
    boolean MapToLower;
    FILE *fp;
    int *lineno;
    boolean PanicAtEOF;
{
    boolean StillLooking = TRUE,
	    InQuotes = FALSE;
    static char Token[MAXTOKENSIZE + 1];
    int     TokenPointer = 0;
    int     c = 0,
            diff = 0;
    /* Following are used to identify substitution fields by environment variables. */
    int     Trigger = 0;
    char   *GetEnvName = NULL;

    diff = 'a' - 'A';
    while (StillLooking && (TokenPointer < MAXTOKENSIZE)) {
	c = getc(fp);
	switch (c) {
	    case EOF: 
		StillLooking = FALSE;
		break;
	    case '\n': 
		(*lineno) = (*lineno) + 1;
		/* TokenPointer = 0; */
	    case ' ': case ',': case '(': case ')': case '[': 
	    case '=': case ']': case ';': case '@': case '\t': 
	    case '?': case '{': case '}':
		if (InQuotes) Token[(TokenPointer++)] = c;
		else
		    if (TokenPointer) StillLooking = FALSE;
		break;
	    case '#':
		if (InQuotes) Token[TokenPointer++] = c;
		else {
		    /* spin until the EOL or the EOF... whichever happens first */
		    while (((c = getc(fp)) != '\n') && (c != EOF));
		    /* put the last character back */
		    ungetc(c, fp);
		}
		break;
	    case '\"': 
		InQuotes = !InQuotes;
		break;
	    default: 
		if (MapToLower && c <= 'Z' && c >= 'A') c = c + diff;
		/* The user can specify substitution fields like %USER%. If something like this is found, we check for an environment variable USER and if this is found, we substitute the $USER value where %USER% was. */
		if (c=='%') {
		    if (Trigger==0) {
			Token[TokenPointer++] = c;
			Trigger = TokenPointer;
		    }
		    else {
			/* Found second trigger character, check getenv value */
			Token[TokenPointer] = '\0';
			if ((GetEnvName=(char *)getenv(&Token[Trigger]))!=NULL) {
			    strcpy(&Token[Trigger-1],GetEnvName);
			    TokenPointer = Trigger-1 + strlen(GetEnvName);
			}
			else Token[TokenPointer++] = c;
			Trigger = 0;
		    }
		}
		else Token[TokenPointer++] = c;
	}
    }
    Token[TokenPointer] = '\0';
    if (StillLooking) arrgh(("Console: Token too long - truncated -> %s\n", Token));
    if (TokenPointer > 0 || PanicAtEOF == FALSE) return(Token);
    sprintf(ParsingErrorText, "Line %d: Unexpected EOF in parsing console file.\n", *lineno);
    ParsingError = TRUE;
    return("Error");
}



InitializeDisplays(self, IsStartup)
    struct consoleClass *self;
    boolean IsStartup;
{
    int     i, j;

    mydbg(("Entering: InitializeDisplays\n"));
    DynamicXmin = DynamicXmax = DynamicYmin = DynamicYmax = -1;
    ScaleFactor = 100;
    ExternalsInUse = 0;
    if (IsStartup) {
	j = 36500000;
	VeryFirstDisplay = (struct display *) malloc (sizeof (struct display));
    }
    else {
	CleanOldState(self);
	j = VeryFirstDisplay->Threshhold;/* Preserve Alarm Setting */
    }
    for (i = 1; i <= DisplayTypeCount; ++i) {
	Numbers[i].IsDisplaying = FALSE;
	Numbers[i].FirstDisplay = NULL;
    }
    bcopy(&VirginDisplay, VeryFirstDisplay, sizeof(struct display));
    VeryFirstDisplay->Threshhold = j;
}

CleanOldState(self)
struct consoleClass *self;
{
    struct display *dp, *dp2;
    int     i;

    mydbg(("Entering: CleanOldState\n"));
    VirginDisplay.Textfont = VirginDisplay.Labelfont = PromptFont;
    for (i = 0; i <= NUMINTERNALVARIABLES; ++i) {
        if (IntrnlVars[i].InUse) {
            if (IntrnlVars[i].turnon) {
                if (IntrnlVars[i].Value) {
		    menulist_DeleteFromML(self->userMenulist, IntrnlVars[i].turnoff);
		    free(IntrnlVars[i].turnoff);
                }
                else {
		    menulist_DeleteFromML(self->userMenulist, IntrnlVars[i].turnon);
		    free(IntrnlVars[i].turnon);
                }
            }
            IntrnlVars[i].InUse = FALSE;
            IntrnlVars[i].Value = FALSE;
            IntrnlVars[i].turnon = NULL;
            IntrnlVars[i].turnoff = NULL;
        }
    }
    /* Never deallocate the first display */
    dp = VeryFirstDisplay->NextOfAllDisplays;
    while (dp) {
	dp2 = dp;
	if (dp->AssociatedLogView != NULL){
	    scroll_UnlinkTree(dp->ScrollLogView);
	    scroll_Destroy(dp->ScrollLogView);
	    logview_Destroy(dp->AssociatedLogView);
	}
	dp = dp->NextOfAllDisplays;
        free(dp2);
    }
    KillInitExecProcesses(FALSE);

    for (i = 0; i < 4; i++){
	RegionLogs[i].ScrollReverse = FALSE;
    }

    ReInitializeRemoteInstruments();

    menulist_ChainBeforeML(self->stdMenulist, self->userMenulist, self->userMenulist);
    consoleClass_PostMenus(self, self->stdMenulist);
}

KillInitExecProcesses(killPIDs)
boolean killPIDs;
{
    SIGSET_TYPE    the_mask, oldmask;
    int     j;
    int    *child;

    mydbg(("Entering: KillInitExecProcesses\n"));
#ifdef POSIX_ENV
    (void) sigemptyset(&the_mask);
#ifdef SIGURG
    (void) sigaddset(&the_mask, SIGURG);
#endif
    (void) sigaddset(&the_mask, SIGTERM);
    (void) sigaddset(&the_mask, SIGCHLD);
    if (sigprocmask(SIG_BLOCK, &the_mask, &oldmask) < 0)
	perror("sigprocmask");
#else
#ifdef SIGURG
    oldmask = sigblock((1 << (SIGCHLD - 1)) |
		       (1 << (SIGTERM - 1)) |
		       (1 << (SIGURG - 1)));
#else /* NOT SIGURG */
    oldmask = sigblock((1 << (SIGCHLD - 1)) |
		       (1 << (SIGTERM - 1)));
#endif /* SIGURG */
#endif /* POSIX_ENV */
    j = 0;
    for ( child = children; *child != -1 && j < MAXCHILDPIDS ; child++) {
	j++;
	if (*child) 
	    if((theGetstatsPid != *child) || (killPIDs)) 
			killpg(*child, SIGTERM);
	*child = -1;
    }
#ifdef POSIX_ENV
    if (sigprocmask(SIG_SETMASK, &oldmask, NULL) < 0)
	perror("sigprocmask");
#else
    sigsetmask(oldmask);
#endif
}

extern char *GetUserPaths();
SetConsoleLib()
{
    char *tmpbuf = NULL;
    char *tmp = NULL;
    struct stat statBuf;
    mydbg(("Entering SetConsoleLib\n"));

    libnum = 0;
    BasicLib = LocalLib = UserLib = -1;
    tmpbuf = environ_AndrewDir("/lib/consoles");
    libpaths[libnum] = (char *) malloc(strlen(tmpbuf) + 1);
    strcpy(libpaths[libnum], tmpbuf);
    errno = ENOTDIR;
    if (stat(libpaths[libnum], &statBuf) == 0 && (statBuf.st_mode & S_IFMT) == S_IFDIR) {
	BasicLib = libnum;
	++libnum;
    }
    else {
	arrgh(("Console: Cannot find library %s: error %d\n", libpaths[libnum], errno));
	free(libpaths[libnum]);
    }
#ifdef CMU_ENV
    tmpbuf = environ_LocalDir("/lib/consoles/local");
#else
    tmpbuf = environ_LocalDir("/lib/consoles");
#endif
    libpaths[libnum] = (char *) malloc(strlen(tmpbuf) + 1);
    strcpy(libpaths[libnum], tmpbuf);
    errno = ENOTDIR;
    if (stat(libpaths[libnum], &statBuf) == 0 && (statBuf.st_mode & S_IFMT) == S_IFDIR) {
	LocalLib = libnum;
	++libnum;
    }
    else {
	free(libpaths[libnum]);
    }
    UserLib = libnum;
    for (tmp = GetUserPaths(); tmp != NULL && *tmp != NULL; tmp = GetUserPaths()) {
	if (libnum >= MAXLIBS) {
	    arrgh(("Console: Cannot have more than %d CONSOLELIB paths.  Ignoring ones starting with %s.\n", MAXLIBS - 2, tmp));
	}
	libpaths[libnum] = (char *) malloc(strlen(tmp) + 1);
	strcpy(libpaths[libnum], tmp);
	errno = ENOTDIR;
	if (stat(libpaths[libnum], &statBuf) == 0 && (statBuf.st_mode & S_IFMT) == S_IFDIR) {
	    ++libnum;
	}
	else {
	    arrgh(("Console: Cannot find library %s: error %d\n", libpaths[libnum], errno));
	    free(libpaths[libnum]);
	}
    }
}

#ifndef DOWNCASE
#define DOWNCASE(x) (isascii(x) && isalpha(x) && isupper(x) ? (tolower(x)) : (x) )
#endif /* DOWNCASE */

char *lcfilename(s)
  char *s;				/* source string */
{
    char *p;
    char *c;
    mydbg(("Entering lcfilename\n"));
    if (s == NULL || *s == NULL) return(NULL);

    p = rindex(s, '/');
    if (p == NULL || *p == NULL) p = s;

    for (c = p; *c != '\0'; *c++) *c = DOWNCASE(*c);

    return(s);
}



extern char *TitleFromFile();
extern stablk();
extern struct datum *BuildDatum();
extern char *RealProgramName;

/* | mask = turn on, & ~mask = turn off */
#define	STD_MASK	0   /* standard menulist */
#define	ALR_OFF_MASK	1   /* Alarm -is- off, Set Alarm menu -is- showing */
#define	ALR_ON_MASK	2   /* Alarm -is- set, Turn Off Alarm -is- showing */
#define	SHR_MASK	4   /* Menus -are- shrunk, Expand Menus -is- showing */
#define	EXP_MASK	8   /* Menus -are- expanded, Shrink Menus -is- showing */
#define	DEB_OFF_MASK   16   /* Debugging -is- off, Turn On Debugging -is- showing */
#define	DEB_ON_MASK    32   /* Debugging -is- on, Turn Off Debugging -is- showing */
#define	GETSTAT_MASK   64   /* Getstats -is- dead, Restart Getstats -is- showing */

/* FindConsoleInPaths
 * by: mmackay
 * date: Thu May 25 14:10:03 EDT 1989
 */
#define STDEXTLTH 8
#define MAXMATCHES 7
#define MODIFYTYPEIN 7
char *FindConsoleInPaths(theTypeInPtr)
char *theTypeInPtr;
{
    FILE *pfd = NULL;
    char *theName = NULL;
    /* Extensions we recognize automatically */
    char *conExtension = "con";
    char *vopExtension  = "vop";
    char *consoleExtension = "console";
    char *vopconExtension = "vopcon";
    char *ConsoleExtension  = "Console";
    char *VopconExtension  = "Vopcon";
    /* end of extensions that we recognize automatically */
    char *c;
    int loopCtr = 0;
    int i;
    boolean hasExtension = FALSE;
    boolean hasPath = FALSE;
    boolean openStatus = FALSE;
    boolean triedStrippingExtension = FALSE;
    boolean caseChange = FALSE;

    mydbg(("Entering FindConsoleInPaths\n"));
    if (theTypeInPtr == NULL) return(NULL);

    /* if we can open the file, then don't go into messy detail, just return */
    if ((pfd  = fopen(theTypeInPtr, "r")) != NULL) {
	fclose(pfd);
	return(theTypeInPtr);
    }
    /* do our best to cope with anything that appears to be an Andrew path name */
    c = rindex(theTypeInPtr, '.');
    if (c != NULL && index(c, '/') == NULL) hasExtension = TRUE;
    if (index(theTypeInPtr, '/') != NULL) hasPath = TRUE;


    retry:
      loopCtr = 0;
    caseChange = FALSE;
    do {
	/* bump the loop counter */
	++loopCtr;
	/* if the typein should be modified, then try all lower-case here, do it only once */
	if (loopCtr == MODIFYTYPEIN && caseChange == FALSE) {
	    theTypeInPtr = lcfilename(theTypeInPtr);
	    caseChange = TRUE;
	    loopCtr = 1; /* reset - and try again */
	}
	if (hasExtension == FALSE) {
	    if (theName != NULL) {
		free(theName);
		theName = NULL;
	    }
	    /* add two to the malloc to accomodate a dot plus the trailing null */
	    theName = malloc(strlen(theTypeInPtr) + STDEXTLTH + 2);
	    if (theName == NULL) {
		fprintf(stderr, "\nFATAL ERROR: in %s, couldn't allocate memory!\n", RealProgramName);
		fflush(stderr);
		return(NULL);
	    }
	    strcpy(theName, theTypeInPtr);
	    strcat(theName, ".");
	    switch(loopCtr) {
		case 1:
		    strcat(theName, conExtension);
		    break;
		case 2:
		    strcat(theName, consoleExtension);
		    break;
		case 3:
		    strcat(theName, ConsoleExtension);
		    break;
		case 4:
		    strcat(theName, vopExtension);
		    break;
		case 5:
		    strcat(theName, vopconExtension);
		    break;
		case 6:
		    strcat(theName, VopconExtension);
		    break;
		default:
		    strcat(theName, conExtension);
		    break;
	    }
	}
	else {/* extension already included */
	    if( theName != NULL) {
		free(theName);
		theName = NULL;
	    }
	    theName = malloc(strlen(theTypeInPtr) + 1);
	    if (theName == NULL) {
		fprintf(stderr, "\nFATAL ERROR: couldn't allocate memory in %s!\n", RealProgramName);
		fflush(stderr);
		return(NULL);
	    }
	    strcpy(theName, theTypeInPtr);
	}
	openStatus = FALSE;
	if(hasPath == FALSE) {
	    for (i = 0; (i < libnum) && (libpaths[i] != NULL) ; i++){
		char tmpbuf[MAXPATHLEN];

		bzero(tmpbuf, MAXPATHLEN);
		sprintf(tmpbuf, "%s/%s", libpaths[i], theName);
		if ((pfd = fopen(tmpbuf, "r")) != NULL){
		    fclose(pfd);
		    if (theName !=NULL) {
			free(theName);
			theName = NULL;
		    }
		    theName = malloc(MAXPATHLEN * sizeof(char));
		    if (theName == NULL) {
			fprintf(stderr, "\nFATAL ERROR: in %s, couldn't allocate memory!\n", RealProgramName);
			fflush(stderr);
			return(NULL);
		    }
		    strcpy(theName, tmpbuf);
		    return(theName);
		}
	    }
	}
	else {
	    if ((pfd = fopen(theName, "r")) != NULL) {
		fclose(pfd);
		return(theName);
	    }
	}
	/* otherwise we already tried it when we started */
    } while ((loopCtr < MAXMATCHES) && (openStatus == FALSE));

    /* if we got here, back up over the extension, and try again applying the variety we support */
    if ((triedStrippingExtension == FALSE) && (hasExtension == TRUE)) {
	c = index(theTypeInPtr, '.'); /* better be true if hasExtension is */
	*c = '\0';
	triedStrippingExtension = TRUE;
	hasExtension = FALSE;
	loopCtr = 0;
	goto retry;
    }
    /* default condition */
    return(NULL);
#undef STDEXTLTH
#undef MAXMATCHES
#undef MODIFYTYPEIN
}

/* GetConsoleFileFromTypeIn
 * by:     mmackay
 * date: Tue May 30 15:50:19 EDT 1989
 * NOTE: Leaves ConFile open by design
 */
boolean GetConsoleFileFromTypeIn(self, IsStartup)
struct consoleClass *self;
boolean IsStartup;
{
#define MAXTRIES 5
    FILE         *pfd =NULL ;
    char            *theTypeInPtr = NULL, *tmp = NULL;
    char            theTypedInput[MAXPATHLEN];
    int	       maxtries = 1;
    boolean      theFirstPass = TRUE;


    mydbg(("Entering: GetConsoleFileFromTypeIn(self, %d)\n", IsStartup));
    InitializeDisplays(self, IsStartup);
    strcpy(FontFamily, "andysans");
    mydbg(("Opening console file %s\n", ConFile));
    do {
	++maxtries;
	if (IsStartup) {
	    arrgh(("\nFATAL ERROR: there is no type-in on startup!\n"));
	    exit(1);
	}
	if (PauseEnqueuedEvents) {
	    PauseEnqueuedEvents = FALSE;
	}
	SetStandardCursor(self, Cursor_Arrow);
	PauseEnqueuedEvents = TRUE;
	if (PromptFont == NULL) {
	    char *s;
	    s = environ_GetProfile("bodyfont");
	    if (!s || !*s) s = PromptFontName;
	    PromptFont = SetupFont(s);
	}
	if (EventFont == NULL) {
	    EventFont = SetupFont("andy16b");
	}
	if (theTypeInPtr != NULL) {
	    free(theTypeInPtr);
	    theTypeInPtr = NULL;
	}
	theTypeInPtr = (char *) malloc(MAXPATHLEN);
	if (theTypeInPtr != NULL) {
	    bzero(theTypeInPtr, MAXPATHLEN); 
	    strcpy(theTypeInPtr, ConFile);
	} else {
	    fprintf(stderr, "\nFATAL ERROR: couldn't allocate memory in %s!\n", RealProgramName);
	    fflush(stderr);
	    /* well, you COULD exit! */
	    return(FALSE);
	}
	if (IsStartup == FALSE) {
	    InitPstrings();		
	    if (theFirstPass == TRUE) {
		sprintf(Pstring1, "The current %s is [%s].", RealProgramName, theTypeInPtr);
		theFirstPass = FALSE;
	    } else {
		sprintf(Pstring1, "Cannot open %s file [%s].", RealProgramName, theTypedInput);
	    }
	    sprintf(Pstring2, "Please type another %s file name:", RealProgramName);
	    sprintf(Pstring3, "==>> ");
	    PromptToWindow(self);
	    GetStringFromWindow(self, MAXPATHLEN);
	    if (Pstring4[0] != '\0') {
		if (theTypeInPtr != NULL) {
		    bzero(theTypeInPtr, MAXPATHLEN); 
		    strcpy(theTypeInPtr, Pstring4);
		    theTypedInput[0] = '\0';
		    strcpy(theTypedInput, Pstring4);
		}
	    } else {
		theTypedInput[0] = ' ';
		theTypedInput[1] = '\0';
	    }
	    SetStandardCursor(self, Cursor_Wait);
	    if ((*theTypeInPtr) == '~') {
		char buffer[MAXPATHLEN];
		if (theTypeInPtr != NULL) {
		    sprintf(buffer, "%s",theTypeInPtr);
		    bzero(theTypeInPtr, MAXPATHLEN);
		    filetype_CanonicalizeFilename(theTypeInPtr, buffer, MAXPATHLEN);
		}
	    }
	}
	if ((tmp = FindConsoleInPaths(theTypeInPtr)) != NULL)
	    strcpy(theTypeInPtr,tmp);
	else
	    theTypeInPtr = NULL;

	if (theTypeInPtr == NULL) {
	    pfd = NULL; /* obviously */
	}else {
	    if (theTypeInPtr != NULL) {
		strcpy(ConFile, theTypeInPtr);
		free(theTypeInPtr);
		theTypeInPtr = NULL;
	    }
	    pfd = fopen(ConFile, "r");
	}
	PauseEnqueuedEvents = FALSE;
    } while ((pfd == NULL) && (maxtries <= MAXTRIES));
    if (maxtries > MAXTRIES) {
	fprintf(stderr, "WARNING: Sorry; you only get %d tries for typed input in %s.\n%s restarting %s.\n", MAXTRIES, RealProgramName, RealProgramName, ConFile);
	fflush(stderr);
	return(TRUE);
    }
    return(TRUE);
}

SetupFromConsoleFile(self, IsStartup)
struct consoleClass *self;
boolean IsStartup;
{
    struct display *thisdisp;
    int            *newchild;
    char           *s, *TokenBuf, *string, *tmp = NULL;
    char            *theTypeInPtr = NULL;
    char            DEFAULTFILE[50]; 
    FILE          *pfd = NULL;
    int             RawXmin, RawXmax, RawYmin, RawYmax, RowHeight, ColWidth, findex, inc, i, mask, Rows, Columns, Machines, j;
    int lineno = 0;
    boolean InstrumentFound = FALSE;
    boolean triedOnce = FALSE;
    boolean retryFailure = TRUE;

    mydbg(("Entering: SetupFromConsoleFile(self, %d)\n", IsStartup));
    sprintf(DEFAULTFILE, "%s", "fudgenut.con");
    HideIt = FALSE;
    DiskPollFreq = VenusPollFreq = VenusPollCt = DiskPollCt = MailPollFreq = MailPollCt = DirPollFreq = PrintPollFreq = PrintPollCt = DirPollCt = WindowPollFreq = WindowPollCt = 60;
    OutgoingAge = 3600;
    ClockPollFreq = ClockPollCt = VMPollFreq = VMPollCt = 2;
    NetPollFreq = NetPollCt = 60;
    if (ExternalLogFP != NULL) {
	fclose(ExternalLogFP);
    }
    ExternalLogFP = NULL;
    LogErrorsExternally = FALSE;
    if (!IsStartup) {
	SetStandardCursor(self, Cursor_Wait);
    }
    else{
	self->menuMask = (STD_MASK | ALR_OFF_MASK | SHR_MASK);
	if (MYDEBUGGING) 
	    self->menuMask |= DEB_ON_MASK;
	menulist_SetMask(self->stdMenulist, self->menuMask);
    }

    Rows = Columns = Machines = 0;
    InitializeDisplays(self, IsStartup);
    strcpy(FontFamily, "andysans");
    ParsingError = FALSE;
    mydbg(("Opening console file %s\n", ConFile));
    if (IsStartup) {
	pfd = fopen(ConFile, "r");
	if (pfd == NULL) {
	    if ((*ConFile) == '~') {
		char buffer[MAXPATHLEN];
		sprintf(buffer, "%s", ConFile);
		filetype_CanonicalizeFilename(ConFile, buffer, MAXPATHLEN);
	    }
	    theTypeInPtr = (char *) malloc(MAXPATHLEN);
	    if (theTypeInPtr != NULL) {
		bzero(theTypeInPtr, MAXPATHLEN); 
		strcpy(theTypeInPtr, ConFile);
	    }
	    if ((tmp = FindConsoleInPaths(theTypeInPtr)) != NULL)
		strcpy(theTypeInPtr,tmp);
	    else
		theTypeInPtr = NULL;
	    
	    if(theTypeInPtr != NULL) {
		strcpy(ConFile, theTypeInPtr);
		free(theTypeInPtr);
		theTypeInPtr = NULL;
	    }
	    pfd = fopen(ConFile, "r");
	    if (pfd == NULL) {
		if (retryFailure == TRUE) {
		    retryFailure = FALSE;
		    fprintf(stderr, "ERROR: %s couldn't open %s; proceeding with %s...\n", RealProgramName, ConFile, DEFAULTFILE);
		    fflush(stderr);
		    strcpy(ConFile, DEFAULTFILE);
		    theTypeInPtr = (char *) malloc(MAXPATHLEN);
		    if (theTypeInPtr != NULL) {
			bzero(theTypeInPtr, MAXPATHLEN); 
			strcpy(theTypeInPtr, ConFile);
		    }
		    if ((tmp = FindConsoleInPaths(theTypeInPtr)) != NULL)
			strcpy(theTypeInPtr,tmp);
		    else
			theTypeInPtr = NULL;

		    if(theTypeInPtr != NULL) {
			strcpy(ConFile, theTypeInPtr);
			free(theTypeInPtr);
			theTypeInPtr = NULL;
		    }
		    pfd = fopen(ConFile, "r");
		} else {
		    if ( pfd == NULL) {
			exit(1);
		    }
		}
	    }
	}
    } else {
	pfd = fopen(ConFile, "r");
    }
    if ((pfd == NULL) && (IsStartup== FALSE)) {
	if(GetConsoleFileFromTypeIn(self, IsStartup) == FALSE) {
	    fprintf(stderr, "FATAL ERROR: couldn't open %s for %s!\n", ConFile, RealProgramName);
	    fflush(stderr);
	    exit(1);
	}
    }
    if (! IsStartup){
	InitPstrings();
	sprintf(Pstring2, "Initializing %s...", TitleFromFile(ConFile, FALSE));
	PromptToWindow(self);
    }
    VeryFirstDisplay->WhatToDisplay = &Numbers[CLOCKALL];	
    Numbers[CLOCKALL].FirstDisplay = VeryFirstDisplay;
    Numbers[CLOCKALL].IsDisplaying = TRUE;
    VeryFirstDisplay->DrawFunction = functions[DISPFLAG_RINGALARM];
    VeryFirstDisplay->NextOfAllDisplays = NULL;
    thisdisp = VeryFirstDisplay;
    triedOnce = FALSE;
	lineno = 1;
    if (pfd != NULL){
    while (*(string = GetNextToken(self, TRUE, pfd, &lineno, FALSE)) && !ParsingError) {
	if ((string != NULL) && (string != "\n") && (string != "\0")) {
	    findex = stablk(string, OptionTable, 1);
	} else {
	    if(triedOnce) {
		if(pfd != NULL) {
		    fclose(pfd);
		}
		return;
	    } else {
		triedOnce = TRUE;
		break;
	    }
	}
	switch (findex) {
	    case FLAG_STATUSSERVER:
		/* for Vopcon to choose which server machine to use */
		TokenBuf = GetNextToken(self, FALSE, pfd, &lineno, TRUE);
		StatusServer = (char *) malloc(1 + strlen(TokenBuf));
		strcpy(StatusServer, TokenBuf);
		break;
	    case FLAG_NEWINSTRUMENT:
		if (!InstrumentFound){
		    InstrumentFound = TRUE;
		    ConfigureMachines(self, &Rows, &Columns, &Machines, TRUE);
		    if (Rows == 0) Rows = 1;
		    if (Columns == 0) Columns = 1;
		}
		thisdisp->NextOfAllDisplays = (struct display *) malloc(sizeof(struct display));
		thisdisp = thisdisp->NextOfAllDisplays;
		bcopy(&VirginDisplay, thisdisp, sizeof(struct display));
		break;
	    case FLAG_XMIN:
		thisdisp->RelXmin = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_XMAX:
		thisdisp->RelXmax = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_YMIN:
		thisdisp->RelYmin = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_YMAX:
		thisdisp->RelYmax = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_FUNCTION:
		{
		struct FuncStruct *fs;
		struct display *mydisp;

		string = GetNextToken(self, TRUE, pfd, &lineno, TRUE);
		fs = FuncLookup(string);
		if (!fs) {
		    ParsingError = TRUE;
		    sprintf(ParsingErrorText, "Line %d: I don't know about computing %s\n", lineno, string);
		    break;
		}
		switch (fs->datumindex) {
		    case ERRORLOG:
			thisdisp->AssociatedLogView = logview_New();
			thisdisp->ScrollLogView = scroll_Create(thisdisp->AssociatedLogView, scroll_LEFT);
			scroll_SetParameters(thisdisp->ScrollLogView, 75, 75, 10, 10);
			logview_SetDataObject(thisdisp->AssociatedLogView, RegionLogs[ERRORREGIONLOG].TextLog);
			break;
		    case REPORTLOG:
			thisdisp->AssociatedLogView = logview_New();
			thisdisp->ScrollLogView = scroll_Create(thisdisp->AssociatedLogView, scroll_LEFT);
			scroll_SetParameters(thisdisp->ScrollLogView, 75, 75, 10, 10);
			logview_SetDataObject(thisdisp->AssociatedLogView, RegionLogs[REPORTREGIONLOG].TextLog);
			break;
		    case USERLOG:
			thisdisp->AssociatedLogView = logview_New();
			thisdisp->ScrollLogView = scroll_Create(thisdisp->AssociatedLogView, scroll_LEFT);
			scroll_SetParameters(thisdisp->ScrollLogView, 75, 75, 10, 10);
			logview_SetDataObject(thisdisp->AssociatedLogView, RegionLogs[USERREGIONLOG].TextLog);
			break;
		    case SILLYLOG:
			thisdisp->AssociatedLogView = logview_New();
			thisdisp->ScrollLogView = scroll_Create(thisdisp->AssociatedLogView, scroll_LEFT);
			scroll_SetParameters(thisdisp->ScrollLogView, 75, 75, 10, 10);
			logview_SetDataObject(thisdisp->AssociatedLogView, RegionLogs[SILLYREGIONLOG].TextLog);
			break;
		}
		thisdisp->WhatToDisplay = &Numbers[fs->datumindex];
		Numbers[fs->datumindex].IsDisplaying = TRUE;
		if (thisdisp->UpdateAlways) {
		    thisdisp->WhatToDisplay->AnAlwaysUpdate = TRUE;
		}
		if (Numbers[fs->datumindex].FirstDisplay == NULL) {
		    Numbers[fs->datumindex].FirstDisplay = thisdisp;
		} else {
		    for (mydisp = Numbers[fs->datumindex].FirstDisplay;
			 mydisp->NextDisplay; mydisp = mydisp->NextDisplay) {;}
		    mydisp->NextDisplay = thisdisp;
		}
		thisdisp->NextDisplay = NULL;
		thisdisp->ClickStringLeft = fs->defaultleftclick;
		break;
		}
	    case FLAG_REMOTEFUNCTION:
		RawXmin = (1000 * thisdisp->RelXmin) / Columns;
		RawXmax = (1000 * thisdisp->RelXmax) / Columns;
		RawYmin = (1000 * thisdisp->RelYmin) / Rows;
		RawYmax = (1000 * thisdisp->RelYmax) / Rows;
		RowHeight = (1000 * ScaleFactor) / Rows;
		ColWidth = (1000 * ScaleFactor) / Columns;
		string = GetNextToken(self, TRUE, pfd, &lineno, TRUE);
		for (i = 0; i < Machines; ++i) {
		    if (i > 0) {
			thisdisp->NextOfAllDisplays = (struct display *) malloc(sizeof(struct display));
			bcopy(thisdisp, thisdisp->NextOfAllDisplays,
			      sizeof(struct display));
			thisdisp = thisdisp->NextOfAllDisplays;
			thisdisp->NextOfAllDisplays = NULL;
		    }
		    thisdisp->WhatToDisplay = BuildDatum(string, i);
		    thisdisp->NextDisplay = thisdisp->WhatToDisplay->FirstDisplay;
		    thisdisp->WhatToDisplay->FirstDisplay = thisdisp;
		    thisdisp->AssociatedLog = WhichErrorLog(i);
		    if ((thisdisp->WhatToDisplay == (struct datum *) - 1)
			|| (thisdisp->AssociatedLogView == (struct logview *) - 1)) {
			ParsingError = TRUE;
			sprintf(ParsingErrorText, "Line %d: I don't understand remote function %s\n", lineno, string);
			break;
		    }
		    thisdisp->RelYmin = (((i / Columns) * RowHeight) + RawYmin) / 1000;
		    thisdisp->RelYmax = (((i / Columns) * RowHeight) + RawYmax) / 1000;
		    thisdisp->RelXmin = (((i % Columns) * ColWidth) + RawXmin) / 1000;
		    thisdisp->RelXmax = (((i % Columns) * ColWidth) + RawXmax) / 1000;

		}
		break;
	    case FLAG_DISPLAYTYPE:
		findex = stablk(string = GetNextToken(self, TRUE, pfd, &lineno, TRUE), DisplayTypeTable, 1);
		if (findex < 1) {
		    ParsingError = TRUE;
		    sprintf(ParsingErrorText, "Line %d: I don't know about drawing %s\n", lineno, string);
		    break;
		}
		thisdisp->PreDrawFunction = prefunctions[findex];
		thisdisp->DrawFunction = functions[findex];
		break;
	    case FLAG_DISPLAYSTYLE:
		findex = stablk(string = GetNextToken(self, TRUE, pfd, &lineno, TRUE), StyleTable, 1);
		switch (findex) {
		    case SFLAG_LOGINDICATOR:
			thisdisp->DisplayStyle = LOGINDICATOR;
			break;
		    case SFLAG_REPEATINDICATOR:
			thisdisp->DisplayStyle = REPEATINDICATOR;
			break;
		    case SFLAG_DIALHIDDEN:
			thisdisp->DisplayStyle = DIALHIDDEN;
			break;
		    case SFLAG_LEFTINDICATOR:
			thisdisp->DisplayStyle = LEFTINDICATOR;
			break;
		    case SFLAG_REVERSESCROLLING:
			thisdisp->DisplayStyle = REVERSESCROLLING;
			thisdisp->AssociatedLog->ScrollReverse = TRUE;
			break;
		    case SFLAG_SQUAREDIAL:
			thisdisp->DisplayStyle = SQUAREDIAL;
			break;
		    case SFLAG_HORIZONTAL:
			thisdisp->DisplayStyle = HORIZONTAL;
			break;
		    default:
			ParsingError = TRUE;
			sprintf(ParsingErrorText, "Line %d: I don't know about display style %s\n", lineno, string);
			break;
		}
		break;
	    case FLAG_ALWAYSUPDATE:
		thisdisp->UpdateAlways = TRUE;
		if (thisdisp->WhatToDisplay != &Numbers[0]) {
		    thisdisp->WhatToDisplay->AnAlwaysUpdate = TRUE;
		}
		break;
	    case FLAG_OUTGOINGAGE:
		OutgoingAge = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
            case FLAG_INITEXEC:
                {
                boolean allowexec;

                allowexec = ! environ_GetProfileSwitch("SecurityConscious", FALSE);
		if (allowexec) {
		    SIGSET_TYPE omask, nmask;
		    TokenBuf = GetNextToken(self, FALSE, pfd, &lineno, TRUE);
#ifdef POSIX_ENV
		    sigemptyset(&nmask);
		    sigaddset(&nmask, SIGCHLD);
		    if(sigprocmask(SIG_BLOCK, &nmask, &omask)<0) {
			perror("sigprocmask");
		    }
#else
		    omask = sigblock(1 << (SIGCHLD - 1));
#endif
                    /* added an array bounds check here */
                    j = 0;
                    for (newchild = children; *newchild > 0 && j < MAXCHILDPIDS; newchild++)
                        j++;
                    if (*newchild == -1)
                        newchild[1] = -1;
                    if ((*newchild = osi_vfork()) == 0) {
			NEWPGRP();
                        execl(_SITE_BIN_SH, "sh", "-c", TokenBuf, 0);
                        _exit(0);
                    }
                    if (*newchild == -1)
			*newchild = 0;
#ifdef POSIX_ENV
		    if(sigprocmask(SIG_BLOCK, &omask, NULL)<0) {
			perror("sigprocmask");
		    }
#else
		    sigsetmask(mask);
#endif
                }
                }
		break;
	    case FLAG_BOXED:
		thisdisp->Boxed = TRUE;
		break;
	    case FLAG_VALUEMAX:
		thisdisp->ValueMax = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_LABEL:
	    case FLAG_RIGHTLABEL:
	    case FLAG_TOPLABEL:
	    case FLAG_LEFTLABEL:
	    case FLAG_BOTTOMLABEL:
		TokenBuf = GetNextToken(self, FALSE, pfd, &lineno, TRUE);
		thisdisp->label = (char *) malloc(1 + strlen(TokenBuf));
		strcpy(thisdisp->label, TokenBuf);
		switch (findex) {
		    case FLAG_RIGHTLABEL:
			thisdisp->IsLabelling = RIGHT_LABEL;
			break;
		    case FLAG_TOPLABEL:
			thisdisp->IsLabelling = TOP_LABEL;
			break;
		    case FLAG_LEFTLABEL:
			thisdisp->IsLabelling = LEFT_LABEL;
			break;
		    case FLAG_BOTTOMLABEL:
		    default:	/* default label is bottom */
			thisdisp->IsLabelling = BOTTOM_LABEL;
			break;
		}
		break;
	    case FLAG_HANDLENGTH:
		thisdisp->HandLength = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_HANDWIDTH:
		thisdisp->HandWidth = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_THRESHHOLD:
		thisdisp->Threshhold = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_CEILING:
		thisdisp->Ceiling = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_ADJUSTLABELFONT:
		thisdisp->AdjustLabelFont = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_ADJUSTTEXTFONT:
		thisdisp->AdjustTextFont = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_LABELFONT:
		TokenBuf = GetNextToken(self, FALSE, pfd, &lineno, TRUE);
		thisdisp->NameOfLabelFont = (char *) malloc(1 + strlen(TokenBuf));
		strcpy(thisdisp->NameOfLabelFont, TokenBuf);
		if (!IsStartup) {
		    thisdisp->Labelfont = SetupFont(thisdisp->NameOfLabelFont);
		}
		break;
	    case FLAG_ICONFONT:
	    case FLAG_TEXTFONT:
		TokenBuf = GetNextToken(self, TRUE, pfd, &lineno, TRUE);
		thisdisp->NameOfTextFont = (char *) malloc(1 + strlen(TokenBuf));
		strcpy(thisdisp->NameOfTextFont, TokenBuf);
		if (!IsStartup) {
		    thisdisp->Textfont = SetupFont(thisdisp->NameOfTextFont);
		}
		break;
	    case FLAG_ICON:
		thisdisp->Iconic = TRUE;
		/* Falls through */
	    case FLAG_TEXT:
		TokenBuf = GetNextToken(self, FALSE, pfd, &lineno, TRUE);
		thisdisp->disptext = (char *) malloc(1 + strlen(TokenBuf));
		strcpy(thisdisp->disptext, TokenBuf);
		thisdisp->IsTexting = TRUE;
		thisdisp->ParseDisplayText = NeedsParsed(thisdisp->disptext);
		break;
	    case FLAG_DESIREDWIDTH:
		MinWidth = MaxWidth = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_DESIREDHEIGHT:
		MinHeight = MaxHeight = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_MINHEIGHT:
		MinHeight = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_MAXHEIGHT:
		MaxHeight = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_MINWIDTH:
		MinWidth = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_MAXWIDTH:
		MaxWidth = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_PERIOD:
		Period = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_SET:
	    case FLAG_GLOBAL:
		break;	/* No-ops */
	    case FLAG_PROMPTFONT:
		strcpy(PromptFontName, GetNextToken(self, TRUE, pfd, &lineno, TRUE)); /* throwing this away now */
		if (!IsStartup) {
		    char *s;
		    s = environ_GetProfile("bodyfont");
		    if (!s || !*s) s = PromptFontName;
		    PromptFont = SetupFont(s);
		    EventFont = SetupFont("andy16b");
		}
		break;
	    case FLAG_VMPOLLPERIOD:
	    case FLAG_VMPOLLFREQ:
		VMPollFreq = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		VMPollCt = VMPollFreq + 1;
		break;
	    case FLAG_VENUSPOLLPERIOD:
		VenusPollFreq = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		VenusPollCt = VenusPollFreq + 1;
		break;
	    case FLAG_WINDOWPOLLPERIOD:
		WindowPollFreq = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		WindowPollCt = WindowPollFreq + 1;
		break;
	    case FLAG_DISKPOLLPERIOD:
	    case FLAG_DISKPOLLFREQ:
		DiskPollFreq = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		DiskPollCt = DiskPollFreq + 1;
		break;
	    case FLAG_MAILPOLLPERIOD:
	    case FLAG_MAILPOLLFREQ:
		MailPollFreq = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		MailPollCt = MailPollFreq + 1;
		break;
	    case FLAG_CLOCKPOLLPERIOD:
	    case FLAG_CLOCKPOLLFREQ:
		ClockPollFreq = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		ClockPollCt = ClockPollFreq + 1;
		break;
	    case FLAG_DIRPOLLPERIOD:
		DirPollFreq = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		DirPollCt = DirPollFreq + 1;
		break;
	    case FLAG_PRINTPOLLPERIOD:
		PrintPollFreq = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		PrintPollCt = PrintPollFreq + 1;
		break;
	    case FLAG_NETPOLLPERIOD:
	    case FLAG_NETPOLLFREQ:
		NetPollFreq = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		NetPollCt = NetPollFreq + 1;
		break;
	    case FLAG_INITSTRING:
		TokenBuf = GetNextToken(self, FALSE, pfd, &lineno, TRUE);
		thisdisp->WhatToDisplay->RawText = (char *) malloc(1 + strlen(TokenBuf));
		strcpy(thisdisp->WhatToDisplay->RawText, TokenBuf);
		break;
	    case FLAG_SCALEFACTOR:
		ScaleFactor = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_NOCLIP:
		thisdisp->Clipped = FALSE;
		break;
	    case FLAG_NOBLANK:
		thisdisp->WhiteOut = FALSE;
		break;
	    case FLAG_FONTFAMILY:
		if (strlen(string) > 4) {
		    strcpy(FontFamily, GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		} else {
		    ParsingError = TRUE;
		    sprintf(ParsingErrorText,
			    "Line %d: FontFamily must be spelled out --font is not a keyword", lineno);
		}
		break;
	    case FLAG_MAXLABELFONTSIZE:
		thisdisp->MaxLabelFontSize = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_MAXTEXTFONTSIZE:
		thisdisp->MaxTextFontSize = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_LEFTCLICKTO:
		findex = stablk(string = GetNextToken(self, TRUE, pfd, &lineno, TRUE), ClickOptions);
		switch (findex) {
		    case CLICK_ERRORLOG:
			thisdisp->AssociatedLog = &RegionLogs[ERRORREGIONLOG];
			break;
		    case CLICK_REPORTLOG:
			thisdisp->AssociatedLog = &RegionLogs[REPORTREGIONLOG];
			break;
		    case CLICK_USERLOG:
			thisdisp->AssociatedLog = &RegionLogs[USERREGIONLOG];
			break;
		    case CLICK_SILLYLOG:
			thisdisp->AssociatedLog = &RegionLogs[SILLYREGIONLOG];
			break;
		    default:
			ParsingError = TRUE;
			sprintf(ParsingErrorText, "Line %d: I don't understand how to click to %s\n", lineno, string);
			break;
		}
		break;
	    case FLAG_LEFTCLICKSTRING:
		TokenBuf = GetNextToken(self, FALSE, pfd, &lineno, TRUE);
		thisdisp->ClickStringLeft = (char *) malloc(1 + strlen(TokenBuf));
		strcpy(thisdisp->ClickStringLeft, TokenBuf);
		break;
	    case FLAG_EXTERNALNAME:
		if (++ExternalsInUse >= NUMEXTERNALS) {
		    ParsingError = TRUE;
		    sprintf(ParsingErrorText, "Line %d: Too many externals\n", lineno);
		    break;
		}
		if (thisdisp->WhatToDisplay == &Numbers[0]) {
		    ParsingError = TRUE;
		    sprintf(ParsingErrorText, "Line %d: Must specify function before external name\n", lineno);
		    break;
		}
		TokenBuf = GetNextToken(self, FALSE, pfd, &lineno, TRUE);
		thisdisp->WhatToDisplay->ExtName = (char *) malloc(1 + strlen(TokenBuf));
		thisdisp->WhatToDisplay->RawText = (char *) malloc(256);
		bzero(thisdisp->WhatToDisplay->RawText, 256);
		strcpy(thisdisp->WhatToDisplay->ExtName, TokenBuf);
		break;
	    case FLAG_HIGHLIGHTBOXMIN:
		thisdisp->FlashStyle = 1;
		thisdisp->FlashMin = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		thisdisp->MayFlash = TRUE;
		break;
	    case FLAG_HIGHLIGHTNOTCHMIN:
		thisdisp->FlashStyle = 2;
		thisdisp->FlashMin = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		thisdisp->MayFlash = TRUE;
		break;
	    case FLAG_HIGHLIGHTINVERTMIN:
		thisdisp->FlashStyle = 3;
		thisdisp->FlashMin = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		thisdisp->MayFlash = TRUE;
		break;
	    case FLAG_FLASHMIN:
		thisdisp->FlashMin = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		thisdisp->MayFlash = TRUE;
		break;
	    case FLAG_HIGHLIGHTBOXMAX:
		thisdisp->FlashStyle = 1;
		thisdisp->FlashMax = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		thisdisp->MayFlash = TRUE;
		break;
	    case FLAG_HIGHLIGHTNOTCHMAX:
		thisdisp->FlashStyle = 2;
		thisdisp->FlashMax = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		thisdisp->MayFlash = TRUE;
		break;
	    case FLAG_HIGHLIGHTINVERTMAX:
		thisdisp->FlashStyle = 3;
		thisdisp->FlashMax = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		thisdisp->MayFlash = TRUE;
		break;
	    case FLAG_FLASHMAX:
		thisdisp->FlashMax = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		thisdisp->MayFlash = TRUE;
		break;
	    case FLAG_NOINVISIBLECLICK:
		thisdisp->ClickWhenInvisible = FALSE;
		break;
	    case FLAG_UNLESS:
		thisdisp->AppearIfTrue = FALSE;	/* DROP THROUGH */
	    case FLAG_IF:
		thisdisp->DependentUponVariables = TRUE;
		thisdisp->WhichVariable = i = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		if (i > NUMINTERNALVARIABLES || i < 0) {
		    sprintf(ParsingErrorText, "Line %d: No such internal variable as %d\n", lineno, i);
		    ParsingError = TRUE;
		    break;
		}
		IntrnlVars[i].InUse = TRUE;
		break;
	    case FLAG_INTERNALMENUS:
		i = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		if (i > NUMINTERNALVARIABLES || i < 0) {
		    sprintf(ParsingErrorText, "Line %d: No such internal variable as %d\n", lineno, i);
		    ParsingError = TRUE;
		    break;
		}
		IntrnlVars[i].InUse = TRUE;
		TokenBuf = GetNextToken(self, FALSE, pfd, &lineno, TRUE);
		IntrnlVars[i].turnoff = (char *) malloc(1 + strlen(TokenBuf));
		strcpy(IntrnlVars[i].turnoff, TokenBuf);
		TokenBuf = GetNextToken(self, FALSE, pfd, &lineno, TRUE);
		IntrnlVars[i].turnon = (char *) malloc(1 + strlen(TokenBuf));
		strcpy(IntrnlVars[i].turnon, TokenBuf);
		break;
	    case FLAG_ALARMRECTANGLE:
		BogusInt = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		BogusInt = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		BogusInt = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		BogusInt = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    case FLAG_COLUMNS:
		ChooseColumns(atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE)));
		ConfigureMachines(self, &Rows, &Columns, &Machines, FALSE);
		if (Rows == 0)
		    Rows = 1;
		if (Columns == 0)
		    Columns = 1;
		break;
	    case FLAG_MACHINES:
		ChooseMachines(self,GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		ConfigureMachines(self, &Rows, &Columns, &Machines, FALSE);
		if (Rows == 0)
		    Rows = 1;
		if (Columns == 0)
		    Columns = 1;
		break;
	    case FLAG_FILLCHAR:
		thisdisp->DisplayStyle = (int) (GetNextToken(self, TRUE, pfd, &lineno, TRUE)[0]);
		break;
	    case FLAG_EXTERNALERRORFILE:
		s = GetNextToken(self, FALSE, pfd, &lineno, TRUE);
		if ((ExternalLogFP = fopen(s, "a")) == NULL) {
		    ParsingError = TRUE;
		    sprintf(ParsingErrorText, "Line %d: Cannot open external log file %s\n", lineno, s);
		    break;
		}
		LogErrorsExternally = TRUE;
		break;
	    case FLAG_DYNAMICRECTANGLE:
		DynamicXmin = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		DynamicXmax = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		DynamicYmin = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		DynamicYmax = atoi(GetNextToken(self, TRUE, pfd, &lineno, TRUE));
		break;
	    default:
		ParsingError = TRUE;
		sprintf(ParsingErrorText, "Line %d: I don't understand the console option keyword %s\n", lineno, string);
		break;
	}
    }
    /* the pfd is definitely NOT null if we got here, so close the file */
    fclose(pfd);
    } else {
	return;
    }
    if (ParsingError) {
	if (IsStartup) {
	    arrgh(("FATAL ERROR: parsing problem in %s:\n>>> %s\n", RealProgramName, ParsingErrorText));
	    exit(-1);
	}
	InitPstrings();
	sprintf(Pstring2, "%s", ParsingErrorText);
	PromptToWindow(self);
	if(GetConsoleFileFromTypeIn(self, IsStartup) == FALSE) {
	    fprintf(stderr,"FATAL ERROR: getting %s file type-in!\n", RealProgramName);
	    fflush(stderr);
	    exit(-1);
	}
    }
    if (!IsStartup) {
	im_SetPreferedDimensions(0, 0, MinWidth, MinHeight);
	ClearWindow(self);
	if (HideIt) {
	    arrgh(("HideMe no longer exists - see posts on andy.console\n"));
	}
	consoleClass_FlushGraphics(self);
    }
    consoleClass_WantUpdate(self, self);
    IntrnlVars[0].Value = (Numbers[ERRORS].Value > 0) ? TRUE : FALSE;
    DoLOAD = Numbers[LOADCPU].IsDisplaying
      || Numbers[LOADIO].IsDisplaying;
    DoCPU = Numbers[LOADUSER].IsDisplaying
      || Numbers[LOADSYS].IsDisplaying
      || Numbers[LOADIDLE].IsDisplaying;
    DoVM = Numbers[VM].IsDisplaying;
#if defined(sys_telmat)
    DoPAGING = Numbers[PAGE_IN].IsDisplaying
      || Numbers[PAGE_OUT].IsDisplaying
#else /* sys_telmat */
    DoPAGING = Numbers[PAGEIN].IsDisplaying
      || Numbers[PAGEOUT].IsDisplaying
#endif /* sys_telmat */
      || Numbers[PAGEREPLACABLE].IsDisplaying
      || Numbers[PAGEDEFICIT].IsDisplaying;
    DoMEM = Numbers[MEMACTIVE].IsDisplaying
      || Numbers[MEMFREE].IsDisplaying;
    DoQUEUE = Numbers[QUEUERUN].IsDisplaying
      || Numbers[QUEUEBLOCK].IsDisplaying
      || Numbers[QUEUEMEM].IsDisplaying;
    DoINTS = Numbers[INTSIO].IsDisplaying
      || Numbers[INTSSYS].IsDisplaying
      || Numbers[INTSSWAP].IsDisplaying;
    DoNDSTAT = Numbers[NDSTATIN].IsDisplaying
      || Numbers[NDSTATOUT].IsDisplaying
      || Numbers[NDSTATERR].IsDisplaying;
    DoPROCESSES = Numbers[PROCSUSER].IsDisplaying
      || Numbers[PROCSOTHER].IsDisplaying
      || Numbers[PROCSTOTAL].IsDisplaying;
    DoWindows = Numbers[WINDOWUSE].IsDisplaying;
    DoVMStat = FALSE;
    for (i = LOADCPU; i <= NDSTATERR; ++i) {
	if (Numbers[i].IsDisplaying) {
	    DoVMStat = TRUE;
	    break;
	}
    }
    DoVMStat = DoVMStat || DoPROCESSES;
    DoDiskFreeStat = FALSE;
    for (i = DISK1; i <= DISK6; ++i) {
	if (Numbers[i].IsDisplaying) {
	    DoDiskFreeStat = TRUE;
	    break;
	}
    }
    DoPrintChecking = Numbers[PRINTQUEUE].IsDisplaying
      || Numbers[PRINTSENT].IsDisplaying
      || Numbers[PRINTERRORS].IsDisplaying;
    DoDirChecking = FALSE;
    for (i = DIRECTORY1; i <= LASTDIRECTORY - ExternalsInUse; ++i) {
	if (Numbers[i].IsDisplaying) {
	    DoDirChecking = TRUE;
	    break;
	}
    }
    DoMailChecking = Numbers[MAIL].IsDisplaying;
    DoTroubleChecking = Numbers[TROUBLE].IsDisplaying;
    DoVenusChecking = Numbers[VICEPERSONAL].IsDisplaying
      || Numbers[VICEPARTITION].IsDisplaying
      || Numbers[UNAUTHENTICATED].IsDisplaying;
    DoNetChecking = Numbers[NETRESPONSES].IsDisplaying;
    DoCheckClock = Numbers[CLOCKHOURS].IsDisplaying
      || Numbers[CLOCKHOURFIFTHS].IsDisplaying
      || Numbers[CLOCKMINUTES].IsDisplaying
      || Numbers[CLOCKSECONDS].IsDisplaying
      || Numbers[CLOCKALL].IsDisplaying
      || Numbers[DATE].IsDisplaying
      || Numbers[UDPIDLE].IsDisplaying;
    DoVenusMonitoring = DoVenusMonitoring || Numbers[MARINERFETCH].IsDisplaying
      || Numbers[MARINEROTHER].IsDisplaying || Numbers[MARINERFINISHED].IsDisplaying
      || Numbers[VICEPERSONAL].IsDisplaying || Numbers[VICEPARTITION].IsDisplaying;

    FontCount = 0;
    inc = 2;
    for (i = 6; i < 37; i += inc) {
	if (i == 12) {
	    inc = 6;
	}
	AvailFontNames[FontCount] = (char *) malloc(5 + strlen(FontFamily));
	if (AvailFontNames[FontCount] != NULL) {
	    sprintf(AvailFontNames[FontCount], "%s%d", FontFamily, i);
	    AvailFontPts[FontCount] = i;
	    FontsAvail[FontCount] = 0;
	    ++FontCount;
	}
    }
    for (thisdisp = VeryFirstDisplay; thisdisp; thisdisp = thisdisp->NextOfAllDisplays) {
	if (thisdisp->DrawFunction == functions[DISPFLAG_EKGGRAPH] && thisdisp->WhatToDisplay->Valuelist == NULL) {
	    thisdisp->WhatToDisplay->Valuelist = (int *) malloc(DATAMAX * sizeof(int));
	    bzero(thisdisp->WhatToDisplay->Valuelist, (DATAMAX * sizeof(int)));
	}
    }
    if (!IsStartup) {
	SetStandardCursor(self, Cursor_Arrow);
    }
    Numbers[MARINEROTHER].RawText = OtherVenusStr;
    Numbers[MARINERFETCH].RawText = FetchVenusStr;
    Numbers[MARINERFINISHED].RawText = FinishedVenusStr;
    self->userMenulist = PrepareUserMenus(self, consoleClass_GetInfoHack());
    consoleClass_WantUpdate(self, self);

    /* NOTE change, always leave parsing with events enqueued events on... */
    if(PauseEnqueuedEvents) {
	PauseEnqueuedEvents = FALSE;
    }
}


InitDisplay(self)
struct consoleClass *self;
{
    struct display *dp;
    int     i;

    mydbg(("entering: InitDisplay\n"));
    dup2(1, 2);
    im_SetPreferedDimensions(0, 0, MaxWidth, MaxHeight);
    ClearWindow(self);
    if (HideIt) {
	arrgh(("HideMe no longer exists - see posts on andy.console\n"));
    }
    consoleClass_WantUpdate(self, self);
    EventFont = SetupFont("andy16b");
    InitPstrings();
    sprintf(Pstring2, "Initializing %s...", TitleFromFile(ConFile, FALSE));
    PromptToWindow(self);

    if(PromptFont == NULL){
	char *s;
	s = environ_GetProfile("bodyfont");
	if (!s || !*s) s = PromptFontName;
	PromptFont = SetupFont(s);
    }
    icon12font = SetupFont("icon12");
    console10font = SetupFont("con10");
    for (dp = VeryFirstDisplay, i = 0; dp; dp = dp->NextOfAllDisplays, i++) {
	dp->Labelfont = SetupFont(dp->NameOfLabelFont);
	dp->Textfont = SetupFont(dp->NameOfTextFont);
    }
    consoleClass_SetFont(self, PromptFont);
    consoleClass_FlushGraphics(self);
}



PostParseArgs(name) 
char *name;
{
    mydbg(("entering: PostParseArgs\n"));
    if (fpacheck()){
	/* this is open to revision, both in content and location */
	arrgh(("********CONSOLE WARNING********\n"));
	arrgh(("The FPA board on this machine is bad - console and some other programs may not run properly - please notify:\n"));
	arrgh(("[Public Workstation]:  Consultant on duty\n"));
	arrgh(("[Private Workstation]: Departmental Maintainer\n"));
	arrgh(("[Unsure]:              Advisor <advisor+>\n"));
	arrgh(("\nThank You\n"));
    }
    if (name != NULL){
	strcpy(ConFile, name);
    }
    else{
	char *s;
	s = environ_GetProfile("default");
	if (!s || !*s) s = "Default";
	strcpy(ConFile, s);
    }
    /* arrgh(("Initializing %s (%s%s); please wait...\n", RealProgramName, ConFile, CONSOLE_VERSION)); */
}

char MyHomeDir[MAXPATHLEN] = "";

OneTimeInit(self)
struct consoleClass *self;
{
    int i, fd;
#ifdef POSIX_ENV
    struct sigaction vec;
#else
    struct sigvec vec;
#endif

    mydbg(("entering: OneTimeInit\n"));
    NEWPGRP();
#ifdef SIGXFSZ
    signal(SIGXFSZ, ToggleDebugging);
#endif /* SIGXFSZ */
    signal(SIGCHLD, Reaper);

#ifdef POSIX_ENV
    vec.sa_handler = DieGracefully;
    sigemptyset(&vec.sa_mask);
#ifdef WM_ENV
    (void) sigaddset(&vec.sa_mask, SIGURG);
#endif
    sigaction(SIGTERM, &vec, NULL);
#else /* ! POSIX */
    vec.sv_handler = DieGracefully;
#ifdef WM_ENV
    vec.sv_mask = 1<<(SIGURG-1);
#else /* WM_ENV */
    vec.sv_mask = 0;
#endif /* WM_ENV */
    vec.sv_onstack = FALSE;
    sigvec(SIGTERM, &vec, NULL);
#endif /* POSIX */

    MyHomeDir[0] = '\0';
    strcpy(MyHomeDir, environ_GetHome(NULL));
    /* HOW DO YOU HANDLE THIS IF THERE IS NO HOME ENVIRONMENT VARIABLE?? */
    if (MyHomeDir[0] == '\0'){/* should not happen */
	arrgh(("console: Could not determine home Directory\n"));
    }

    OneTimeRemoteInit(self); 

    for (i=0; i <= NUMINTERNALVARIABLES; ++i) {
	IntrnlVars[i].InUse = FALSE;
	IntrnlVars[i].Value = FALSE;
	IntrnlVars[i].turnon = NULL;
	IntrnlVars[i].turnoff = NULL;
    }
    bzero(&Numbers[0], (DisplayTypeCount + 1) * sizeof (struct datum));
    for (i=1; i<= DisplayTypeCount; ++i) {
	Numbers[i].ValueCtr = DATAMIN;
	Numbers[i].RawText = Nullity;
	Numbers[i].ExtName = Nullity;
    }

    Numbers[ERRORS].RawText = PrimaryErrorBuffer;

    Numbers[ALARM].RawText = (char *)malloc(250); /* Allow long reminder lines */
    InitClock(); /* Does mallocs for clock texts */
    CheckClock(self);
    strcpy(Numbers[ALARM].RawText, "The alarm clock is not set.");
    Numbers[ERRORLOG].Value=ERRORREGIONLOG;
    Numbers[REPORTLOG].Value=REPORTREGIONLOG;
    Numbers[USERLOG].Value=USERREGIONLOG;
    Numbers[SILLYLOG].Value=SILLYREGIONLOG;
    for (i=0; i<=NUMREGIONLOGS; ++i) {
	RegionLogs[i].WhichDatum = &Numbers[ERRORLOG+i];
    }

    /* Set up VirginDisplay, to be copied with bcopy later on */
    bzero(&VirginDisplay, sizeof (struct display));
    VirginDisplay.DrawFunction = DrawDebug;
    VirginDisplay.DisplayStyle = -1;
    VirginDisplay.WhatToDisplay = &Numbers[0];
    VirginDisplay.ValueMax = 100;
    VirginDisplay.LastClickValue = -999;
    VirginDisplay.HandLength=9;
    VirginDisplay.HandWidth = 1;
    VirginDisplay.Clipped = TRUE;
    VirginDisplay.ClickWhenInvisible = TRUE;
    VirginDisplay.AppearIfTrue = TRUE;
    VirginDisplay.WhichVariable = -1;
    VirginDisplay.FlashMax = 9999;
    VirginDisplay.AssociatedLog = &RegionLogs[ERRORREGIONLOG];
    VirginDisplay.Ceiling = 999999999;
    VirginDisplay.WhiteOut = TRUE;
    VirginDisplay.MaxTextFontSize = 36;
    VirginDisplay.MaxLabelFontSize = 36;
    VirginDisplay.NameOfLabelFont = PromptFontName;
    VirginDisplay.NameOfTextFont = PromptFontName;
    VirginDisplay.label = VirginDisplay.disptext = VirginDisplay.ClickStringLeft = Nullity;
}

NeedsParsed(s)
char *s;
{
    boolean FoundIt = FALSE;

    while (*s != NULL) {
	if (*s == '$' || *s == '*') {
	    FoundIt = TRUE;
	}
	++s;
    }
    return(FoundIt);
}


extern char *sys_errlist[];
extern int sys_nerr;

ClearAllLogs(self, rock)
struct consoleClass *self;
char *rock;
{
    int i, len;

    mydbg(("entering: ClearAllLogs\n"));
    for (i=0; i<=NUMREGIONLOGS; ++i) {
	len = text_GetLength(RegionLogs[i].TextLog);
	text_AlwaysDeleteCharacters(RegionLogs[i].TextLog, 0, len);
	text_NotifyObservers(RegionLogs[i].TextLog, 0);
    }
}


WriteMonsterLog(self, rock)
struct consoleClass *self;
char *rock;
{
    char *LogFileName, Question[80], buffer[MAXPATHLEN];
    int i;
    boolean KeepAsking = TRUE;
    FILE *fp = NULL;

    mydbg(("entering: WriteMonsterLog\n"));
    LogFileName = (char *)malloc (MAXPATHLEN);
    while (KeepAsking) {
	PauseEnqueuedEvents = TRUE;
	InitPstrings();
	sprintf(Pstring1, "File to write:");
	sprintf(Pstring2, "[%s]", _SITE_LOGFILE);
	sprintf(Pstring3, "==>> ");
	PromptToWindow(self);
	GetStringFromWindow(self, MAXPATHLEN);
	PauseEnqueuedEvents = FALSE;
	if (!strlen(Pstring4)) {
	    strcpy(LogFileName, _SITE_LOGFILE);
	}
	else{
	    strcpy(LogFileName, Pstring4);
	}
	if (*LogFileName == '~'){
	    sprintf(buffer, "%s", LogFileName);
	    filetype_CanonicalizeFilename(LogFileName, buffer, MAXPATHLEN);
	}

	if ((fp = fopen(LogFileName, "w")) == NULL) {
	    sprintf(Question, "Root can't write %s [%s]; try again: ", LogFileName,
		    errno>0 && errno<=sys_nerr ? sys_errlist[errno] : "unknown error" );
	    /*    continue;*/
	    exit(-1);
	}
	KeepAsking = FALSE;
    }
    SetStandardCursor(self, Cursor_Wait);
    consoleClass_SetFont(self, PromptFont);
    for (i=0; i<=NUMREGIONLOGS; ++i) {
	fprintf(fp, "\n\nContents of log %d:\n\n", i);
	text_Write(RegionLogs[i].TextLog, fp, im_GetWriteID(), 0);
    }
    fclose(fp);
    sprintf(buffer, "Wrote LogFile: %s",LogFileName);
    ReportInternalError(self, buffer);
    free(LogFileName);
    RedrawDisplays(self);
    SetStandardCursor(self, Cursor_Arrow);
    consoleClass_WantUpdate(self, self);
}

