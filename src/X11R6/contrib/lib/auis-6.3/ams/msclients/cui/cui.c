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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/cui/RCS/cui.c,v 2.60 1994/03/29 03:53:46 rr2b Exp $";
#endif


#define CUIP_MAJ_VER 2
#define CUIP_MIN_VER 0

 

#include <andrewos.h>
#include <cui.h>
#include <hdrparse.h>
#include <errprntf.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#define CUI_SOURCE_CUI_C
#include <cuimach.h>

#ifndef DISABLELONGJUMPS
#include <setjmp.h>
#include <signal.h>
#endif /* DISABLELONGJUMPS */
#ifndef HANDLED_NTOHL
#include <netinet/in.h> /* strictly for ntohl routine */
#endif /* HANDLED_NTOHL */

#define HEADBUFSIZE (CUIMACH_GULPSIZE * AMS_SNAPSHOTSIZE)
 /* Size of header buffer for each SNAP transaction */
 /* Ought to remain a multiple of AMS_SNAPSHOTSIZE */

#define CMDBUF LINEMAX

#define MAXDEALIASES 25 	/* Number of times we can dealias the same
				   line or recursively read in source
				   files (ugh!) */

#define DEFAULTUPDATES 4	/* Number of days worth of notices to show
				   user on first update */

#define ANSBUFMAX 81

extern char EditorBuf[];
char *LogFileName = NULL;
#ifndef DISABLELONGJUMPS
static jmp_buf CUI_jenv;
#endif /* DISABLELONGJUMPS */

int	CUI_CheckNewMessages (),
	CreateNewMessageDirectory (),
	HelpUser (),
	KillServer (),
	FlagSomething (),
	UnflagSomething (),
	ForgetFlag (), 
	AlterSubscription (),
	AlterSubscriptionLine (),
	GracefulExit (),
	EditFileCmd (),
	KeepHeader(),
	OmitHeader(),
	GetBody (),
	ForkYourself (),
	DeleteMessages (),
	UndeleteMessages (),
	FlagUrgent (),
	FlagNotUrgent (),
	CUI_PurgeDeletions (),
	DisplayFile (),
	UpdateMsgs (),
	ReadCommandsFromFile (),
	PrintBody (),
	EchoArgs (),
	PrintVersionNumbers (),
	DemonLoop (),
	MatchFolder(),
	MakeAlias (),
	ExecuteAlias (),
	RebuildSubscriptionMaps (),
	Epoch (),
	WhenIs (),
	WhatsNew (),
	WriteFile (),
	ViewMessageCmd (),
	StoreMessageCmd (),
	MailCmd (),
	ReplyMailCmd (),
	WideReplyMailCmd (),
	WiderReplyMailCmd (),
	WhoIs (),
	ReconstructDirectory (),
	BrowseMsgs(),
	ClassifyMessage(),
	CopyMessage(),
	AppendMessage(),
	AppendDelMessage(),
	GetHeaders (),
	ForwardMailCmd(),
	SetOption(),
	MergeDirs(),
	GetDirInfo(),
	RmMessageDir(),
	UnlinkViceFile(),
	ReplaceMessage(),
	ConvertOldStuff(),
	RenameDir(),
	CUI_FreeCaches(),
	SubListCmd(),
	ShowOptSettings(),
	ResendCmd(),
	Reindex(),
	Redraft(),
	TakeHints(),
	Scavenge(),
	ListCmd();

extern int  SNAP_debugmask,
	    CUI_SnapIsRunning,
	    CUI_OnSameHost;

extern struct tm   *localtime ();
extern	FILE * qopen ();

extern char *convlongto64 (), *StripWhiteEnds (), *getenv (), *copy(), *GetLine();
extern char *sys_errlist[],
	   *ms_errlist[],
	   *ms_errcauselist[],
	   *ms_errvialist[],
	   *rpc_errlist[];
extern int  sys_nerr,
	    ms_nerr,
	    ms_nerrcause,
	    ms_nerrvia,
	    rpc_nerr;
#ifndef POSIX_ENV
extern long gtime (), time (), atol();
#endif

extern char CUI_VersionString[];
static int ShowChainInfo = 0, PrintFixed = 0, PrintRot13 = 0;
#define MAXCOMMANDS 100
/* during startup be sure this many free command slots exist
   to be used by aliases later
*/
#define MAXEXTRACOMMANDS 25

#define EDIT_MODE  1
#define BROWSE_MODE -1

#define DESCRIBEEXPERTISE (ExpertLevels[ExpertiseLevel])

static char *ExpertLevels[] = {
#define LEVEL_NOVICE 0
    "novice",
#define LEVEL_EXPERT 1
    "expert",
#define LEVEL_WIZARD 2
    "wizard",
#define LEVEL_MAGIC 3
    "magic",
#define LEVEL_PARSECMDLINEARGS 42
    /* used to temporarily overide users default level while processing the command line arguments */
    0
};

/*
  This table used to be initilized at compile time.  On a macintosh the
  double quote literals get removed by a preprocessor and repaced by a macro
  that finds where the strings are loaded at runtime.  So that this would work
  Commands is now initilized at startup.
*/

struct InputParse {
    char   *Command;
    int     (*Function)();
    char   *HelpText;
    int     Level;
}		    Commands[MAXCOMMANDS];

/*
   add a new command to the command table
*/
static struct InputParse *add_command_scan=Commands;
static int num_cmds=0;

void newcmd(xCommand,xFunction,xHelpText,xLevel)
char   *xCommand;
int     (*xFunction)();
char   *xHelpText;
int     xLevel;
{
    add_command_scan->Command=xCommand;
    add_command_scan->Function=xFunction;
    add_command_scan->HelpText=xHelpText;
    add_command_scan->Level=xLevel;
    add_command_scan++;
    /* leave room for user alias commands */
    if (num_cmds++ >= (MAXCOMMANDS-MAXEXTRACOMMANDS)) {
	fputs("CUI: internal error; too many built-in commands\n", stderr);
	exit(1);
    }
}
	
void cmd_init()
{
    newcmd("?", HelpUser, "'? [cmd]' -- prints the help message for the command", LEVEL_NOVICE);
    newcmd("alias", MakeAlias, "'alias cmd <big cmd>' -- define a new command as a macro (shorthand) for a set of commands", LEVEL_EXPERT);
    newcmd("allreply", WiderReplyMailCmd, "'allreply <msg>' -- send a reply to the sender & readers of a message", LEVEL_NOVICE);
    newcmd("append", AppendMessage, "'append <msg> <folder>' -- Append a message to a different folder", LEVEL_EXPERT);
    newcmd("browse", BrowseMsgs, "'browse <folder> [since <date>]' -- go through all messages in a folder (since a date)", LEVEL_NOVICE);
    newcmd("cat", DisplayFile, "'cat <filename>' -- displays the contents of any file in AFS", LEVEL_EXPERT);
    newcmd("check", CUI_CheckNewMessages, "'check [directory] [spec file]' -- gathers new mail from given directory", LEVEL_WIZARD);
    newcmd("classify", ClassifyMessage, "'classify <msg> <folder>' -- Put a message in a different folder and delete it from the current one", LEVEL_NOVICE);
    newcmd("convert", ConvertOldStuff, "'convert' -- Convert old Maillib-format mail to AMS directory", LEVEL_EXPERT);
    newcmd("copy", CopyMessage, "'copy <msg> <folder>' -- Copy a message into a different folder", LEVEL_NOVICE);
    newcmd("create", CreateNewMessageDirectory, "'create <folder>  [parent folder [filename or message number]]' -- creates a new message folder", LEVEL_EXPERT);
    newcmd("dappend", AppendDelMessage, "'dappend <msg> <folder>' -- Append a message to a different folder and delete it from the current one", LEVEL_EXPERT);
    newcmd("delete", DeleteMessages, "'delete <msg list>' -- delete all named messages", LEVEL_NOVICE);
    newcmd("dirinfo", GetDirInfo, "'dirinfo <folder>' -- print information about a message folder", LEVEL_EXPERT);
    newcmd("echo", EchoArgs, "'echo <anything>' -- print out exactly what you ask for", LEVEL_EXPERT);
    newcmd("epoch", Epoch, "'epoch <folder> <date>' -- purge all messages in a tree before a given date", LEVEL_WIZARD);
    newcmd("editfile", EditFileCmd, "'edit <filename>' -- invokes the local machine's editor on any AFS file", LEVEL_EXPERT);
    newcmd("exit", GracefulExit, "'exit' -- gracefully ends a session", LEVEL_NOVICE);
    newcmd("flag", FlagSomething, "'flag <msg> <flagname>' -- Flags a message with the user-defined flag name.", LEVEL_EXPERT);
    newcmd("forgetflag", ForgetFlag, "'forgetflag <flagname> <folder>' -- Eliminates a user-defined flag from a folder.", LEVEL_EXPERT);
    newcmd("fork", ForkYourself, "'fork' -- For server demons only, goes into background", LEVEL_WIZARD);
    newcmd("forward",ForwardMailCmd, "'forward <msg> <name>' -- forward a message to someone", LEVEL_NOVICE);
    newcmd("free", CUI_FreeCaches, "'free' -- frees up storage by forgetting about all messages & folders thus seen", LEVEL_EXPERT);
    newcmd("headers", GetHeaders, "'headers <message dir> [since <date>]' -- list headers in a folder, optionally since a certain date", LEVEL_NOVICE);
    newcmd("help", HelpUser, "'help' -- prints this help message", LEVEL_NOVICE);
    newcmd("keep", KeepHeader, "'keep <header list>' -- includes these headers when showing messages", LEVEL_EXPERT);
    newcmd("kill", KillServer, "'kill' -- kills your message server", LEVEL_WIZARD);
    newcmd("list", ListCmd, "'list <folder> -- list available message folders", LEVEL_NOVICE);
    newcmd("loop", DemonLoop, "'loop n p <cui command>' -- Loop n times (-1 forever), executing the command every p seconds", LEVEL_WIZARD);
    newcmd("match", MatchFolder, "'match <pattern>' -- find names of folders matching a pattern", LEVEL_EXPERT);
    newcmd("mail", MailCmd, "'mail <names>' -- send mail to the named people", LEVEL_NOVICE);
    newcmd("merge", MergeDirs, "'merge <dir1> <dir2>' -- merge the first folder into the second", LEVEL_EXPERT);
    newcmd("noturgent", FlagNotUrgent, "'noturgent <msg>' -- Marks a message as not 'urgent'", LEVEL_EXPERT);
    newcmd("rename", RenameDir, "'rename <olddir> <newdir>' -- Change the name of a message folder", LEVEL_NOVICE);
    newcmd("omit", OmitHeader, "'omit <header list>' -- deletes these headers when showing messages", LEVEL_EXPERT);
    newcmd("print", PrintBody, "'print <msg>' -- Print the body of the message specified on an AFS-connected printer", LEVEL_NOVICE);
    newcmd("purge", CUI_PurgeDeletions, "'purge <folder>' -- Purge all messages deleted from a message folder", LEVEL_NOVICE);
    newcmd("quit", GracefulExit, "'quit' -- gracefully ends a session", LEVEL_NOVICE);
    newcmd("rebuild", RebuildSubscriptionMaps, "'rebuild <root>' -- Rebuilds the subscription map files", LEVEL_WIZARD);
    newcmd("reconstruct", ReconstructDirectory, "'reconstruct <folder> [1] [2]' -- Reconstruct the magic file for a corrupted message folder", LEVEL_WIZARD);
    newcmd("redraft", Redraft, "'redraft <msg>' -- Restore the named message as the draft of a message you are sending", LEVEL_NOVICE);
    newcmd("reindex", Reindex, "'reindex <root>' -- Rechecks the master update files.", LEVEL_WIZARD);
    newcmd("replace", ReplaceMessage, "'replace <msg> <filename>' -- Replace an entire message body with a file", LEVEL_WIZARD);
    newcmd("reply", ReplyMailCmd, "'reply <msg>' -- send a reply to the sender of a message", LEVEL_NOVICE);
    newcmd("resend", ResendCmd, "'resend <msg> <name list>' -- Resend the message to whoever you like", LEVEL_EXPERT);
    newcmd("rmdir", RmMessageDir, "'rmdir <folder>' -- delete an empty message folder", LEVEL_NOVICE);
    newcmd("scavenge", Scavenge, "'scavenge <folder> [recurse] [purge] [norecurse] [nopurge]' -- Scavenge the named directory to update the index", LEVEL_WIZARD);
    newcmd("set", SetOption, "'set <option> <value>' -- set an option variable", LEVEL_NOVICE);
    newcmd("show", ShowOptSettings, "'show <option>' -- show one or all of your option variables", LEVEL_EXPERT);
    newcmd("source", ReadCommandsFromFile, "'source <filename>' -- execute commands found in source file", LEVEL_EXPERT);
    newcmd("store", StoreMessageCmd, "'store <msg> <filename>' -- save a message in a file", LEVEL_EXPERT);
    newcmd("sublist", SubListCmd, "'sublist [folder]' -- list your subscriptions", LEVEL_EXPERT);
    newcmd("subscribe", AlterSubscription, "'subscribe <folder>' -- alter your subscription to a \n\tgiven message folder (edit them all by default)", LEVEL_NOVICE);
    newcmd("takehints", TakeHints, "'takehints [all]' -- tell the bb server to check for master update file hints", LEVEL_WIZARD);
    newcmd("type", GetBody, "'type <msg>' -- Display the body of the message specified", LEVEL_NOVICE);
    newcmd("undelete", UndeleteMessages, "'undelete <msg list>' -- undelete all named messages", LEVEL_NOVICE);
    newcmd("unflag", UnflagSomething, "'unflag <msg> <flagname>' -- Unflags a message with the user-defined flag name.", LEVEL_EXPERT);
    newcmd("unlink", UnlinkViceFile, "'unlink <filename>' -- Remove (permanently) a file in AFS", LEVEL_EXPERT);
    newcmd("update", UpdateMsgs, "'update <folder list>' -- show new messages in named folders (default: your subscription)", LEVEL_NOVICE);
    newcmd("urgent", FlagUrgent, "'urgent <msg>' -- Marks a message as 'urgent'", LEVEL_EXPERT);
    newcmd("version", PrintVersionNumbers, "'version' -- show the current version numbers of Message System components", LEVEL_EXPERT);
    newcmd("view", ViewMessageCmd, "'view <msg>' -- use the editor on a message (useful mostly on the PC)", LEVEL_NOVICE);
    newcmd("whatsnew", WhatsNew, "'whatsnew' -- lists all folders with new messagse", LEVEL_WIZARD);
    newcmd("whenis", WhenIs, "'whenis <time>' -- show how the date parser will parse the time string you type", LEVEL_EXPERT);
    newcmd("whois", WhoIs, "'whois <name>' -- show how the message server will interpret a destination address", LEVEL_NOVICE);
    newcmd("widereply", WideReplyMailCmd, "'widereply <msg>' -- send a reply to the readers of a message", LEVEL_NOVICE);
    newcmd("write", WriteFile, "'write <full path filename>' -- lets you enter the contents of a file to be written by the message server", LEVEL_EXPERT);
    newcmd(0,0,0,0); /* Last entry must be null-initialized */
}

static char *SendOpts[] = {
#define SEND_SAVEDRAFT 0
    "draft",
#define SEND_EDIT 1
    "edit",
#define SEND_SET 2
    "set",
#define SEND_SUBMIT 3
    "submit",
#define SEND_TYPE 4
    "type",
    0
};

static char *Actions[] = {
#define MORE_ALL 0
    "allreply",
#define MORE_DEL 1
    "delete",
#define MORE_FORWARD 2
    "forward",
#define MORE_MAIL 3
    "mail",
#define MORE_NEXT 4
    "next",
#define MORE_PRINT 5
    "print",
#define MORE_PROCEED 6
    "proceed",
#define MORE_REPLY 7
    "reply",
#define MORE_CLASS 8
    "classify",
#define MORE_COPY 9
    "copy",
#define MORE_APPEND 10
    "append",
#define MORE_SKIP 11
    "skip",
#define MORE_WRITE 12
    "store",
#define MORE_TYPE 13
    "type",
#define MORE_WIDE 14
    "widereply",
#define MORE_SET 15
    "set",
#define MORE_VIEW 16
    "view",
#define MORE_RESEND 17
    "resend",
    0
};

static char *Options[] = {
#define OPT_DEBUG 0
    "debug",
#define OPT_LEVEL 1
    "level",
#define OPT_PROMPT 2
    "prompt",
#define OPT_HEADERS 3
    "headers",
#define OPT_SCRIPT 4
    "scriptmode",
#define OPT_TERMINAL 5
    "terminal",
#define OPT_BLIND 6
    "blind",
#define OPT_SEENLAST 7
    "seenlast",
#define OPT_WHATMEWORRY 8
    "whatmeworry",
#define OPT_EDITOR 9
    "editor",
#define OPT_LOGFILE 10
    "logfile", 
#define OPT_SHOWCHAINS 11
    "chains",
#define OPT_PRINTFIXED 12
    "fixprint",
#define OPT_PRINTROT13 13
    "rot13print",
#define OPT_PRINTER 14
    "printer",
#define OPT_BBDAEMON 15
    "bbdaemon",
    0
};

static char *YesNoOptions[] = {
#define ANS_NO 0
    "off",
#define ANS_YES 1
    "on",
    0
};

extern int  CUIDebugging, CUI_RPCInProgress,
	    CUI_IsMagic;
int	ExpertiseLevel = LEVEL_NOVICE;
int	Interactive = TRUE;
int	HeadersOn = FALSE;   /* Header filtering initially off */
int	Validation = TRUE;
int	BlindStatus = AMS_SEND_BLINDNO;
int	WhatMeWorry = FALSE;
int	BBDaemon = FALSE;
char	Prompt[80] = "CUI> ";
Boolean IgnoreMissingSourceFile = TRUE;

HandleCUISignal(signum, ActNormal) 
int signum, *ActNormal;
{
#ifndef DISABLELONGJUMPS
    if (signum == SIGINT) {
	if (CUI_RPCInProgress) {
	    moreprintf("\n\nSNAP call in progress -- cannot interrupt.\n\n");
	    *ActNormal = 0;
	    return;
	} else {
	    moreprintf("\n\nInterrupt -- type 'quit' to exit.\n\n");
	    longjmp(CUI_jenv, 1);
	}
    }
#endif /* DISABLELONGJUMPS */
    *ActNormal = 1;
}

    
cui_prog_main(argc, argv)
int	argc;
char  **argv;
{
    char   *s;
    int tmplevel;
    extern char ProgramName[];

    cmd_init();
    strcpy(ProgramName, "cui"); /* ProgramName is declared in libutil.a */

#ifndef DISABLELONGJUMPS
    if(setjmp(CUI_jenv)!=0) {
      fprintf(stderr,"?^C during initialization or command line processing\n");
      exit(1);
    }
#endif /* DISABLELONGJUMPS */

    amsconfig(argc, argv, CUI_SnapIsRunning ? "cuis" : "cuin");
    if (CUI_Initialize(NULL, NULL)) {
	ReportError("Error initializing; program terminated.", ERR_CRITICAL, FALSE);
	exit(1);
    }
    CUI_SetClientSignalHandler(HandleCUISignal);
    {char buf[100];
     sprintf(buf,"cui.%d.%d",CUIP_MAJ_VER,CUIP_MIN_VER);
     CUI_SetClientVersion(buf);
    }   
    if(CUI_SnapIsRunning && (CUI_CheckVersion()!=0))
     ;
    tmplevel = ExpertiseLevel;
    ExpertiseLevel = LEVEL_PARSECMDLINEARGS;
    if (argc > 1) {
	s = malloc(CMDBUF);
	if (!s) {
	    ReportError("Out of memory in initialization phase", ERR_FATAL, FALSE);
	    exit(-1);
	}
	*s = '\0';
	while (--argc > 0) {
	    ++argv;
	    if (strlen(s) + strlen(argv[0]) + 2 > CMDBUF) {
		ReportError("Arguments too long", ERR_FATAL, FALSE);
		exit(-1);
	    }
	    strcat(s, argv[0]);
	    strcat(s, " ");
	}
	if (ProcessMultipleCommands(s, 0) != 0) {
	    ReportError("Error in executing command line arguments; aborting.",
		    ERR_CRITICAL, FALSE);
	    GracefulExit("");
	}
	free(s);
    }
    else {
	Interactive = FALSE;
	ReadCommandsFromFile("~/.cuirc", 0);
	Interactive = TRUE;
    }
    if (ExpertiseLevel == (LEVEL_PARSECMDLINEARGS)) ExpertiseLevel = tmplevel;
    IgnoreMissingSourceFile = FALSE;
    while (TRUE) {		/* Exit only via exit command */
#ifndef DISABLELONGJUMPS
	setjmp(CUI_jenv);
#endif /* DISABLELONGJUMPS */
	debug(2,("Ready for next command\n"));
	if (Interactive) {
	    moreprintf("%s", Prompt);
	    fflush(stdout);
	}
	s = GetLine();
	if ((long) s < 0) {
	    if (Interactive)
		moreprintf("EOF\n");
	    GracefulExit("");
	}
	ProcessMultipleCommands(s, 0);
    }
}

ProcessMultipleCommands(s, AliasCt)
char   *s;
int	AliasCt;
{
    char    CurrentCommand[CMDBUF];
    int     bytesleft, result;

    debug(1,("ProcessMultipleCommands %s (%d)\n", s, AliasCt));
    bytesleft = GetNextCommand(&s, CurrentCommand, CMDBUF);
    while (bytesleft > 0) {
	if (strlen(CurrentCommand) > 0) {
	    if ((result = ProcessCommand(CurrentCommand, AliasCt)) != 0) {
		bytesleft = 0;
		if (CurrentCommand[0]) {
		    CurrentCommand[0] = '\0';
		    if (result == MORE_NO_MORE) {
			ReportError("Command aborted -- Ignoring the rest of your command line", ERR_WARNING, FALSE);
			return(-1);
		    } else {
			if (!WhatMeWorry) {
			ReportError("Command failed -- Ignoring the rest of your command line", ERR_WARNING, FALSE);
			    return(-1);
			}
		    }
		} else {
		break;
		}
	    }
	}
	CurrentCommand[0] = '\0';
	bytesleft = GetNextCommand(&s, CurrentCommand, CMDBUF);
    }
    if (bytesleft < 0) {
	ReportError("Command line too long -- aborted", ERR_CRITICAL, FALSE);
	return(-1);
    }
    if (CurrentCommand[0]) {
	return(ProcessCommand(CurrentCommand, AliasCt));
    }
    return(0);
}

ProcessCommand(CurrentCommand, AliasCount)
char   *CurrentCommand;
int	AliasCount;
{
    int     Matches,
	    LastMatch = 0,
	    MatchLen = 0,
	    i,
	    TooHigh = 0,
	    LastTooHigh = 0;
    char   *FirstWord = CurrentCommand;

    debug(1,("ProcessCommand %s (%d)\n", CurrentCommand, AliasCount));
    if (*CurrentCommand == '\0')
	return(0);
    while (*CurrentCommand && *CurrentCommand != ' ' && *CurrentCommand != '\t') {
	if (isupper(*CurrentCommand)) {
	    *CurrentCommand = tolower(*CurrentCommand);
	}
	++CurrentCommand;
    }
    if (*CurrentCommand) {
	*CurrentCommand++ = 0;
    }
    Matches = 0;
    for (i = 0; Commands[i].Command; ++i) {
	if (Commands[i].Level > ExpertiseLevel)
	    continue;
	if (!strcmp(Commands[i].Command, FirstWord)) {
	    Matches = 1;
	    LastMatch = i;
	    break;
	}
    }
    if (!Matches) {		/* No exact matches, try partials */
	MatchLen = strlen(FirstWord);
	for (i = 0; Commands[i].Command; ++i) {
	    if (!strncmp(Commands[i].Command, FirstWord, MatchLen)) {
		if (Commands[i].Level > ExpertiseLevel) {
		    ++TooHigh;
		    LastTooHigh = i;
		    continue;
		}
		++Matches;
		LastMatch = i;
	    }
	}
    }
    if (Matches > 1) {
	char *MyQVec[100];
	int MyIndices[100], myindex;

	MyQVec[0] = "Ambiguous command; please choose:";
	myindex = 1;

	for (i = 0; Commands[i].Command; ++i) {
	    if (Commands[i].Level > ExpertiseLevel)
		continue;
	    if (!strncmp(Commands[i].Command, FirstWord, MatchLen)) {
		MyQVec[myindex] = Commands[i].HelpText;
		MyIndices[myindex++] = i;
	    }
	}
	MyQVec[myindex] = "None of the above";
	MyIndices[myindex] = -1;
	MyQVec[myindex+1] = NULL;
	LastMatch = MyIndices[ChooseFromList(MyQVec, myindex)];
	if (LastMatch >= 0) {
	    debug(2,("Command re-parsed to %s; executing\n", Commands[LastMatch].Command));
	    return((*(Commands[LastMatch].Function))(CurrentCommand, AliasCount, Commands[LastMatch].HelpText));
	}
    }
    else
	if (Matches == 0) {
	    if (TooHigh == 1) {
		char ErrorText[256];

		moreprintf("'%s' is a level '%s' command.\n",
			Commands[LastTooHigh].Command, ExpertLevels[Commands[LastTooHigh].Level]);
		sprintf(ErrorText, "Do you want to execute the '%s %s' command anyway", Commands[LastTooHigh].Command, CurrentCommand);
		if (GetBooleanFromUser(ErrorText, TRUE)) {
		    debug(2,("Command parsed to %s; executing\n", Commands[LastTooHigh].Command));
		    return((*(Commands[LastTooHigh].Function))(CurrentCommand, AliasCount, Commands[LastTooHigh].HelpText));
		}
	    }
	    else
		if (TooHigh > 1) {
		    moreprintf("'%s' is not a recognized command at expertise level %s.\n", FirstWord, DESCRIBEEXPERTISE);
		    moreprintf("However, it does match commands at higher expertise levels:\n");
		    for (i = 0; Commands[i].Command; ++i) {
			if (!strncmp(Commands[i].Command, FirstWord, MatchLen)) {
			    moreprintf("\t%s (level %s)\n", Commands[i].Command,
				    Commands[i].Level > LEVEL_NOVICE ? Commands[i].Level > LEVEL_EXPERT ? Commands[i].Level > LEVEL_WIZARD ? "magic" : "wizard" : "expert" : "novice");
			}
		    }
		    moreprintf("You can change your expertise level with the 'level' command.\n");
		}
	    else {
		moreprintf("Unrecognized command: %s\n", FirstWord);
		AMS_RETURN_ERRCODE(EINVAL, EIN_COMMANDPARSE, EVIA_PROCESSCOMMAND);
	    }
	}
    else {
	debug(2,("Command parsed to %s; executing\n", Commands[LastMatch].Command));
	return((*(Commands[LastMatch].Function))(CurrentCommand, AliasCount, Commands[LastMatch].HelpText));
    }
    return(-1);
}

ReadCommandsFromFile(arg, AliasCt)
char   *arg;
int	AliasCt;
{
    MapcarFunctionToFileLines(ProcessCommand, arg, AliasCt);
}

MapcarFunctionToFileLines(ProcessFunc, arg, AliasCt)
int	(*ProcessFunc)();
char   *arg;
int	AliasCt;
{
    char   *filenam,
	    Buf[MAXBODY + 1],
	    ErrorText[256],
	    Line[LINEMAX + 1],
	   *s;
    char    RealFileName[MAXPATHLEN + 1];
    int     bodylen;
    long    offset,
	    bytesleft,
	    bytesunfetched;

    debug(1,("ReadCommandsFromFile %s (%d)\n", arg, AliasCt));
    ++AliasCt;
    if (AliasCt > MAXDEALIASES) {
	ReportError("alias/source loop -- aborted", ERR_WARNING, FALSE);
	return(-1);
    }
    filenam = StripWhiteEnds(arg);
    if (*filenam == '\0') {
	ReportError("You must supply a file name", ERR_WARNING, FALSE);
	return(-1);
    }
    offset = 0;
    bytesleft = 0;
    bytesunfetched = 0;
    if ((mserrcode = MS_DisambiguateFile(filenam, RealFileName, FALSE)) != 0) {
	if (IgnoreMissingSourceFile && AMS_ERRNO == ENOENT) {
	    return(0);
	}
	sprintf(ErrorText, "The file %s can not be opened or read", filenam);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    do {
	debug(2,("Going into get partial file %s\n", RealFileName));
	if ((mserrcode = MS_GetPartialFile(RealFileName, Buf + bytesleft, MAXBODY - bytesleft, offset, &bytesunfetched, &bodylen)) != 0)
	    break;
	if (bodylen <= 0)
	    break;
	Buf[bodylen + bytesleft] = '\0';/* Make sure null-terminated */
	s = Buf;
	bytesleft = (long) GetNextCommand(&s, Line, LINEMAX);
	while (bytesleft > 0) {
	    if ((*ProcessFunc)(Line, AliasCt) != 0) {
		return(-1);
	    }
	    bytesleft = (long) GetNextCommand(&s, Line, LINEMAX);
	}
	if (bytesleft < 0) {
	    sprintf(ErrorText,
		    "The file %s has an enormous line in it and can't be processed",
		    RealFileName);
	    ReportError(ErrorText, ERR_WARNING, FALSE);
	    return(-1);
	}
	strcpy(Buf, Line);
	bytesleft = strlen(Line);
	offset += bodylen;
    } while (bytesunfetched > 0);
    if (bodylen < 0) {
	if (offset) {
	    sprintf(ErrorText, "The file %s could not be read completely", RealFileName);
	}
	else {
	    sprintf(ErrorText, "The file %s could not be read", RealFileName);
	}
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
 /* May still be one leftover command */
    if ((*ProcessFunc)(Line, AliasCt) != 0) {
	return(-1);
    }
    return(0);
}

GracefulExit(arg)
char   *arg;
{
    debug(1,("GracefulExit\n"));
    CUI_PurgeMarkedDirectories(TRUE, FALSE);
    CUI_EndConversation();
    exit(0);
}

SetOption(arg)
char   *arg;
{
char  AnsBuf[ANSBUFMAX], AnsBuf2[ANSBUFMAX], ErrorText[256];
FILE *LogFP;
int	Ans;
    debug(1,("SetOption %s\n", arg));
    
    Ans = MoreSelect(-1, AnsBuf, ANSBUFMAX, Options, arg, "Option");

    if ((ExpertiseLevel == LEVEL_NOVICE) && (Ans != OPT_LEVEL) && (Ans != OPT_TERMINAL)) {
	moreprintf("'set %s' is a level 'expert' command.\n",Options[Ans]);
	if (!GetBooleanFromUser("Do you want to execute the command anyway", TRUE))return(0);
    }

    if (Ans>=0) switch (Ans) {
	case OPT_PRINTFIXED:
	    Ans = MoreSelect(PrintFixed, AnsBuf2, ANSBUFMAX, YesNoOptions, AnsBuf, "Print in fixed width");
	    if (Ans<0) break;
	    PrintFixed = Ans;
	    break;
	case OPT_PRINTROT13:
	    Ans = MoreSelect(PrintRot13, AnsBuf2, ANSBUFMAX, YesNoOptions, AnsBuf, "Print Rot13");
	    if (Ans<0) break;
	    PrintRot13 = Ans;
	    break;
	case OPT_SHOWCHAINS:
	    Ans = MoreSelect(ShowChainInfo, AnsBuf2, ANSBUFMAX, YesNoOptions, AnsBuf, "Show chains");
	    if (Ans<0) break;
	    ShowChainInfo = Ans;
	    break;
	case OPT_PRINTER:
	    if (!AnsBuf || !*AnsBuf) {
		char *myans;
		moreprintf("Please enter new printer: ");
		myans = GetLine();
		if ((long) myans < 0) {
		    moreprintf("EOF\n");
		    break;
		}
		CUI_SetPrinter(myans);
	    } else {
		CUI_SetPrinter(AnsBuf);
	    }
	    break;
	case OPT_LOGFILE:
	    StripWhiteEnds(AnsBuf);
	    if (!AnsBuf || !*AnsBuf) {
		if (LogFileName) {
		    ReportError("CUI Logging turned off.", ERR_WARNING, FALSE);
		    free(LogFileName);
		    LogFileName = NULL;
		} else {
		    ReportSuccess("Logging is currently turned off; turn it on with 'set log <filename>'.");
		}
		return(0);
	    }
	    LogFP = fopen(AnsBuf, "a");
	    if (!LogFP) {
		char ErrorText[256];

		sprintf(ErrorText, "Cannot open log file %s; sorry (%d).", AnsBuf, errno);
		ReportError(ErrorText, ERR_WARNING, FALSE);
	    } else {
		fclose(LogFP);
		if (LogFileName) {
		    LogFileName = realloc(LogFileName, strlen(AnsBuf)+1);
		} else {
		    LogFileName = malloc(strlen(AnsBuf) + 1);
		}
		if (LogFileName) {
		    strcpy(LogFileName, AnsBuf);
		    ReportError("CUI Logging turned on.", ERR_WARNING, FALSE);
		} else {
		    ReportError("Out of memory", ERR_WARNING, FALSE);
		}
	    }
	    break;
	case OPT_PROMPT:
	    if (!AnsBuf || !*AnsBuf) {
		char *myans;
		moreprintf("Please enter new prompt: ");
		myans = GetLine();
		if ((long) myans <= 0) {
		    moreprintf("EOF\n");
		    break;
		}
		strcpy(Prompt, myans);
	    } else {
		strcpy(Prompt,AnsBuf);
	    }
	    break;
	case OPT_DEBUG:
	    moreprintf("Setting debug mode to %s\n",AnsBuf);
	    SetDebugMode(AnsBuf);
	    break;
	case OPT_LEVEL:
	    if (!AnsBuf || !*AnsBuf) {
	       moreprintf("Expertise level currently %s\n", DESCRIBEEXPERTISE);
	    } else {
	       Ans = MoreSelect(ExpertiseLevel, AnsBuf2, ANSBUFMAX, ExpertLevels, AnsBuf, "Level");
	       if (Ans<0) break;
	       if (Ans == LEVEL_MAGIC && !CUI_IsMagic) {
		   ReportError("Sorry; you are not authorized to be a magic user", ERR_WARNING, FALSE);
	       } else {
		   ExpertiseLevel=Ans;
		   if (Interactive)
		      moreprintf("Expertise level set to %s\n", DESCRIBEEXPERTISE);
	    }  }
	    break;
	case OPT_SCRIPT:
	    Ans = MoreSelect(Interactive, AnsBuf2, ANSBUFMAX, YesNoOptions, AnsBuf, "Scriptmode");
	    if (Ans<0) break;
	    Interactive = ! Ans;
	    break;
	case OPT_HEADERS:
	   Ans = MoreSelect(Interactive, AnsBuf2, ANSBUFMAX, YesNoOptions, AnsBuf, "Header filtering");
	    if (Ans<0) break;
	    HeadersOn = Ans;
	    if (Interactive) {
		DescribeHeads();
	    }
	    break;
	case OPT_TERMINAL:
	    ResetTerminalParams(AnsBuf);
	    break;
	case OPT_EDITOR:
	    if (!AnsBuf || !*AnsBuf) {
		char *myans;
		
		moreprintf("Please enter name of new editor: ");
		myans=GetLine();
		if ((long) myans <= 0) break;
		arg = StripWhiteEnds(myans);
	    } else {
		arg=StripWhiteEnds(AnsBuf);
	    }
	    if (Interactive) {
		ReportSuccess("Caution: this setting will only be in effect for this session.");
	    }
	    SetEditorToUse(arg);
	    break;
	case OPT_BLIND:
	    Ans = MoreSelect(BlindStatus, AnsBuf2, ANSBUFMAX, YesNoOptions, AnsBuf, "Blind copy");
	    if (Ans<0) break;
	    BlindStatus = Ans ? AMS_SEND_BLINDYES : AMS_SEND_BLINDNO;
	    break;
	case OPT_SEENLAST:
	    MarkSeenLast(AnsBuf);
	    break;
	case OPT_WHATMEWORRY:
	    Ans = MoreSelect(WhatMeWorry, AnsBuf2, ANSBUFMAX, YesNoOptions, AnsBuf, "What, me worry?");
	    if (Ans<0) break;
	    WhatMeWorry = Ans;
	    break;
	case OPT_BBDAEMON:
	    if ((Ans = MoreSelect(BBDaemon, AnsBuf2, ANSBUFMAX, YesNoOptions, AnsBuf, "Run like a daemon")) < 0) break;
	    BBDaemon = Ans;
	    ExpertiseLevel = LEVEL_WIZARD;
	    ResetTerminalParams("0 500");
	    Interactive = FALSE;
	    WhatMeWorry = TRUE;
	    strcpy(Prompt, "BBDAEMON> ");
	    break;
	default:
	    sprintf(ErrorText, "Unexpected parse of answer: %d\n", Ans);
	    ReportError(ErrorText, ERR_CRITICAL, FALSE);
	    break;
    }	    /* End switch */

    return(0);
}

static int MyMallocLevel=0, MSDebug=0, MSSnapDebug=0, MSMalloc=0;

SetDebugMode(arg)
char   *arg;
{
    char   *s;
    int newpipescript = 0;

    debug(1,("SetDebugMode %s\n", arg));
    s = arg = StripWhiteEnds(arg);
    if (!strcmp(s, "all")) {
	CUIDebugging = -1;
	SNAP_debugmask = 0xffffffff;
#ifdef ANDREW_MALLOC_ENV
	MyMallocLevel = 5;
	SetMallocCheckLevel(5);
#endif /* #ifdef ANDREW_MALLOC_ENV */
	if (mserrcode = MS_DebugMode(-1, 255, 5)) {
	    ReportError("Cannot set debugging levels", ERR_WARNING, TRUE);
	    return(-1);
	}
	MSDebug = -1;
	MSSnapDebug = 255;
	MSMalloc = 5;
	moreprintf("Debugging maximized for CUI, SNAP, and MS\n");
	return(0);
    }
    while (*s && *s != ' ' && *s != '\t') {
	++s;
    }
    if (*s)
	*s++ = '\0';
    CUIDebugging = atoi(arg);
    arg = s;
    while (*s && *s != ' ' && *s != '\t') {
	++s;
    }
    if (*s)
	*s++ = '\0';
    SNAP_debugmask = atoi(arg);
    arg = s;
    while (*s && *s != ' ' && *s != '\t') {
	++s;
    }
    if (*s)
	*s++ = '\0';
#ifdef ANDREW_MALLOC_ENV
    SetMallocCheckLevel(MyMallocLevel = atoi(arg));
#endif /* ANDREW_MALLOC_ENV */
    arg = s;
    while (*s && *s != ' ' && *s != '\t') {
	++s;
    }
    if (*s)
	*s++ = '\0';
    MSDebug = atoi(arg);
    arg = s;
    while (*s && *s != ' ' && *s != '\t') {
	++s;
    }
    if (*s)
	*s++ = '\0';
    MSSnapDebug = atoi(arg);
    arg = s;
    while (*s && *s != ' ' && *s != '\t') {
	++s;
    }
    if (*s)
	*s++ = '\0';
    MSMalloc = atoi(arg);
    arg = s;
    while (*s && *s != ' ' && *s != '\t') {
	++s;
    }
    if (*s)
	*s++ = '\0';
    newpipescript = atoi(arg);
    if (mserrcode = MS_DebugMode(MSDebug, MSSnapDebug, MSMalloc) != 0) {
	ReportError("Could not set debugging modes for message server", ERR_WARNING, TRUE);
    }
    if (CUI_SnapIsRunning && MS_OpenDebuggingPipescript(newpipescript) != 0) {
	ReportError("Could not open debugging pipescript for message server", ERR_WARNING, TRUE);
    }
    moreprintf("Debug levels:  CUI %d SNAP %d malloc %d MS %d MSSNAP %d MSMalloc %d\n", CUIDebugging, SNAP_debugmask, MyMallocLevel, MSDebug, MSSnapDebug, MSMalloc);
    return(0);
}

HelpUser(arg)
char   *arg;
{
    int     i, NoCmd = TRUE;

    arg = StripWhiteEnds(arg);

    debug(1,("HelpUser %s\n", arg));
    if (!arg || !*arg) {
	NoCmd=FALSE;
    moreprintf("This is the cui -- the Common User Interface to the Andrew Message System\n\n");
    moreprintf("Any command may be unambiguously abbreviated.\n\nThe valid commands at expertise level '%s' are:\n\n", DESCRIBEEXPERTISE);
    }
    for (i = 0; Commands[i].Command; ++i) {
	if (Commands[i].Level > ExpertiseLevel) {
	    if (arg && *arg && 0==strncmp(arg,Commands[i].Command,strlen(arg))) {
		NoCmd=FALSE;
		if (moreprintf("%s\n", Commands[i].HelpText)==MORE_NO_MORE)
		    return(MORE_NO_MORE);
		if (moreprintf("'%s' is at expertise level '%s'.  Use the 'set level' command to change.\n", Commands[i].Command, ExpertLevels[Commands[i].Level] )==MORE_NO_MORE)
		    return(MORE_NO_MORE);
	       }

	    continue;
	}
	if (Commands[i].Function == ExecuteAlias) {
	    if (!arg || !*arg || 0==strncmp(arg,Commands[i].Command,strlen(arg))) {
		NoCmd=FALSE;
	   if (moreprintf("'%s' -- same as typing '%s'\n", Commands[i].Command, Commands[i].HelpText)==MORE_NO_MORE)
		    return(MORE_NO_MORE);
	       }
	}
	else {
	    if (!arg || !*arg || 0==strncmp(arg,Commands[i].Command,strlen(arg))) {
		NoCmd=FALSE;
		if (moreprintf("%s\n", Commands[i].HelpText)==MORE_NO_MORE)
		    return(MORE_NO_MORE);
		}
	}
    }
    if (arg && *arg && NoCmd) {
	  moreprintf("There is no such command as '%s' -- sorry.\n",arg);
    }
    return(0);
}

PrintBody(arg)
char   *arg;
{
    int     cuid, rc;
    char    ErrorText[256], *arg2;

    debug(1,("PrintBody %s\n", arg));
    while (1) {
	 if ((cuid=ParseMessageNumber(arg))<0) return(-1);
	 sprintf(ErrorText, "Trying to print message %d; please wait...", cuid);
	 ReportSuccess(ErrorText);
	 rc=CUI_PrintBodyFromCUIDWithFlags(cuid, (PrintFixed ? AMS_PRINT_FIXED : 0) | (PrintRot13 ? AMS_PRINT_ROT13 : 0), NULL);
	 arg2=index(arg,' ');
	 arg=index(arg,',');
	 if (arg2 && (!arg || arg2 < arg)) {
		arg = arg2;
	}
	 if (arg && *arg) arg++;
	    else break;
    }
    return(rc);
}

static Boolean HeadKeep;
struct head_list {
	char *header;
	struct head_list *next;
	};
struct head_list *HeadList=NULL;

KeepHeader(arg)
char *arg;
{
    debug(1,("KeepHeader of %s\n",arg));
    CheckPrompted("Please enter a list of headers")
    HeadKeep=TRUE;
    HeadersOn=TRUE;
    AddHeads(arg);
    DescribeHeads();
    return(0);
}

OmitHeader(arg)
char *arg;
{
    debug(1,("OmitHeader of %s\n",arg));
    CheckPrompted("Please enter a list of headers")
    HeadKeep=FALSE;
    HeadersOn=TRUE;
    AddHeads(arg);
    DescribeHeads();
    return(0);
}

DescribeHeads() {
    struct head_list *thl;

    if (!Interactive) return;
    if (!HeadersOn) {
	moreprintf("All headers will be printed.\n");
	if (HeadList) {
	    moreprintf("\nHowever, if you type 'set headers' then\n    ");
	} else {
	    return;
	}
    }
    if (!HeadList) {
	moreprintf("All headers will be printed, but this will change if you use the 'keep' or 'omit' commands.\n");
	return;
    }
    if (HeadKeep) {
	moreprintf("Only these headers will be printed:\n");
    } else {
	moreprintf("All but these headers will be printed:\n");
    }
    for (thl = HeadList; thl; thl = thl->next) {
	moreprintf("\t%s\n", thl->header);
    }
}

AddHeads(arg)
char *arg;
{
char *s, *s2;
struct head_list *t, *t2, *last;
    HeadList=NULL;
    while (arg && *arg!='\0') {
	s=index(arg,':');
	s2=index(arg,' ');
	if (s2 && (!s || s2 < s)) {
		s = s2;
	}
	if (s) *s++='\0';
	t2=(struct head_list *)malloc(sizeof(struct head_list));
	if (t2==NULL) {
	   ReportError("No more memory to store headers.", ERR_WARNING, FALSE);
	   return(-1);
	}
	if (HeadList==NULL) {
	    HeadList=t2;
	    t2->next=NULL;
	} else {
	    last=HeadList;
	    for (t=HeadList;
		 (t!=NULL) && 0<ULstrcmp(t->header, arg);
		 t=t->next)
		{ last=t; }
	    if (t==HeadList) {
		t2->next=t;
		HeadList=t2;
	    } else {
		last->next=t2;
		t2->next=t;
	    }
	}
	t2->header=copy(arg);
	arg=s;
    }
}

CheckHead(arg)
char *arg;
{
    struct head_list *t;
    for (t=HeadList; (t!=NULL); t=t->next) {
	if (ULstrncmp(t->header, arg, strlen(t->header)) == 0)
	    break;
    }
    return(HeadKeep && t);
}

GetBody(arg)
char   *arg;
{
    int     cuid, rc;
    char *arg2;

    debug(1,("GetBody %s\n", arg));
    while (1) {
	 if ((cuid=ParseMessageNumber(arg))<0) return(-1);
	 rc = GetBodyFromCUID(cuid);
	 DescribeFlags(cuid);
	 arg2=index(arg,' ');
	 arg=index(arg,',');
	 if (arg2 && (!arg || arg2 < arg)) {
		arg = arg2;
	}
	 if (arg && *arg) arg++;
	    else break;
    }
    return(rc);
}

DescribeFlags(cuid) 
int cuid;
{
    char Snapshot[AMS_SNAPSHOTSIZE], *id, *dir, AttrName[AMS_ATTRNAMEMAX], ErrorText[256];
    int i, started = 0;

    if (cuid <=0 || GetAMSID(cuid, &id, &dir) != 0) {
	sprintf(ErrorText, "There is no such message number as %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
     if (CUI_GetSnapshotFromCUID(cuid, Snapshot) != 0) {
	return(-1); 	/* Error message already reported */
    }
    for (i=0; i<AMS_NUM_UATTRS; ++i) {
	if (AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_UATTR(i))) {
	    if (!CUI_GetAttrName(dir, i, AttrName)) {
		if (started) {
		    moreprintf(", %s", AttrName);
		} else {
		    moreprintf("\nThis message is flagged as: %s", AttrName);
		    ++started;
		}
	    }
	}
    }
    if (started) {
	moreprintf(".\n");
    }
}
		

GetBodyFromCUID(cuid)
int	cuid;
{
    char    SnapshotBuf[AMS_SNAPSHOTSIZE], FileName[1+MAXPATHLEN], *dir, *id, ErrorText[256];

    debug(1,("GetBodyFromCUID %d\n", cuid));
#ifdef METAMAIL_ENV
    {
#ifdef POSIX_ENV
#include <termios.h>
      struct termios ttystatein, ttystateout;
#else      
#include <sgtty.h>
        struct sgttyb ttystatein, ttystateout;
#endif
        char ctype[100], TmpFileName[1+MAXPATHLEN], Cmd[1+MAXPATHLEN];
        int ShouldDelete, code;
        extern int LinesOnTerminal;

        if (CUI_GetHeaderContents(cuid,(char *) NULL, HP_CONTENTTYPE, ctype, sizeof(ctype) - 1) != 0) {
            /* error already reported */
            return(-1);
        }
        if (!getenv("NOMETAMAIL") && ctype[0] && ULstrncmp(ctype, "x-be2", 5) && nontext(ctype)) {
            if (CUI_GetBodyToLocalFile(cuid, TmpFileName, &ShouldDelete)) {
                return(-1); /* error reported */
            }
            sprintf(Cmd, "metamail -m cui %s %s %s", (LinesOnTerminal > 0) ? "-p" : "", ShouldDelete ? "-z" : "", TmpFileName);
#ifdef POSIX_ENV
	    tcgetattr(fileno(stdin), &ttystatein);
	    tcgetattr(fileno(stdout), &ttystateout);
            code = system(Cmd);
	    tcsetattr(fileno(stdin), 0, &ttystatein);
	    tcsetattr(fileno(stdout), 0, &ttystateout);
#else
            gtty(fileno(stdin), &ttystatein);
            gtty(fileno(stdout), &ttystateout);
            code = system(Cmd);
            stty(fileno(stdin), &ttystatein);
            stty(fileno(stdout), &ttystateout);
#endif
            /* if (code) */ return(0);
        }
    }
#endif
    if (GetAMSID(cuid, &id, &dir)) {
	sprintf(ErrorText, "Illegal message number: %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    mserrcode = MS_WriteUnscribedBodyFile(dir, id, FileName);
    if (mserrcode) {
	ReportError("Could not get unscribed body from message server", ERR_WARNING, TRUE);
	return(-1);
    }
    if (DisplayMessage(FileName)) {
	MS_UnlinkFile(FileName);
	return(-1);
    }
    MS_UnlinkFile(FileName); /* ignore errors */
    if (CUI_GetSnapshotFromCUID(cuid, SnapshotBuf) != 0) { /* Was Snapshot */
	return(-1); 	/* Error message already reported */
    }
    if (AMS_GET_ATTRIBUTE(SnapshotBuf, AMS_ATT_UNSEEN) &&
    AMS_GET_ATTRIBUTE(SnapshotBuf, AMS_ATT_MAYMODIFY)) {
	/* Mark it as "read" in database */
	CUI_MarkAsRead(cuid);
    }
    CUI_ProcessMessageAttributes(cuid, SnapshotBuf);
    return(0);
}

ViewMessageCmd(arg)
char *arg;
{
int cuid;
    if ((cuid = ParseMessageNumber(arg))<0) return(-1);
    return(ViewMessage(cuid));
}

ViewMessage(cuid)
int cuid;
{
    Boolean FinishedElsewhere;
    int     bodylen, fd;
    long    offset,
	    bytesunfetched;	/* *** Added for PC 8/20/86 *** */
    char    BodyBuf[MAXBODY], LocalName[MAXPATHLEN + 1],
	    ErrorText[256];

    debug(1,("ViewMessage %d\n", cuid));
    offset = 0;
    CUI_GenLocalTmpFileName(LocalName);
    if (LocalName==NULL)  return(-1);	/* It reported the error */
    fd = open(LocalName, O_WRONLY | O_CREAT, 0600);
    if (fd < 0) {
	ReportError("Cannot open local file", ERR_WARNING, FALSE);
	return(-1);
    }
    do {
	if (CUI_GetPartialBody(BodyBuf, MAXBODY, cuid, offset, &bytesunfetched, &bodylen) != 0)
	    break;
	if (bodylen <= 0)
	    break;
	writeall(fd, BodyBuf, bodylen);
	offset += bodylen;
    } while (bytesunfetched > 0);
    close(fd);
    if (bodylen < 0) {
	if (offset) sprintf(ErrorText, "The body of message %d could not be completely read", cuid);
	  else	    sprintf(ErrorText, "The body of message %d could not be read", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	unlink(LocalName);
	return(-1);
    }
    if ((bodylen == 0) && (offset == 0)) {
	moreprintf("<Empty Message>\n");
	unlink(LocalName);
	return(0);
    }
    if (EditLocalFile(LocalName, &FinishedElsewhere)) {
	unlink(LocalName);
	return(-1);
    }
    if (!FinishedElsewhere) unlink(LocalName);
    return(0);
}

StoreMessageCmd(arg)
char *arg;
{
char *FileName;
int cuid;
    debug(1,("StoreMessage %s\n", arg));
    if ((cuid = ParseMessageNumber(arg))<0) return(-1);
    FileName=index(arg,' ');
    if (!FileName || !*FileName) {
	ReportError("You must enter a file name.",ERR_WARNING, FALSE);
	return(-1);
    } else FileName++;
    return(StoreMessage(cuid,FileName, 0L));
}

StoreMessage(cuid, fname, offset)
int cuid;
char *fname;
long offset;
{
    int     bodylen;
    long    offset_in, bytesunfetched;	/* *** Added for PC 8/20/86 *** */
    char    BodyBuf[MAXBODY], FullName[1+MAXPATHLEN],
	    ErrorText[256];

    offset_in=0;
    if (mserrcode = MS_DisambiguateFile(fname, FullName, AMS_DISAMB_FILEMAYBECREATED)) {
	ReportError("The file name makes no sense", ERR_WARNING, TRUE);
	return(-1);
    }

    do {
	if (CUI_GetPartialBody(BodyBuf, MAXBODY-1, cuid, offset_in, &bytesunfetched, &bodylen) != 0)
	    return(-1);
	if (bodylen <= 0)
	    break;
	mserrcode = MS_StorePartialFile(FullName, offset, bodylen, 0600, TRUE, BodyBuf);
	if (mserrcode) {
	    ReportError("Message server cannot store file", ERR_WARNING, TRUE);
	    return(-1);
	}
	offset += bodylen;
	offset_in += bodylen;
    } while (bytesunfetched > 0);
    if (bodylen < 0) {
	if (offset) sprintf(ErrorText, "The body of message %d could not be completely read", cuid);
	  else	    sprintf(ErrorText, "The body of message %d could not be read", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if ((bodylen == 0) && (offset == 0)) moreprintf("<Empty Message>\n");
      else  ReportSuccess("Message stored.");

    return(0);
}

DisplayFile(arg)
char   *arg;
{
    char Buf[MAXBODY],
	    ErrorText[256],
	    FileName[MAXPATHLEN + 1];
    int     bodylen;
    long    offset,
	    bytesunfetched;	/* *** Added for PC  8/20/86  *** */

    debug(1,("DisplayFile %s\n", arg));
    if (ParseFileName(arg,FileName,FALSE)) return(-1);
    offset = 0;
    do {
	if ((mserrcode = MS_GetPartialFile(FileName, Buf, sizeof(Buf)-1, offset, &bytesunfetched, &bodylen)) != 0)
	    break;
	if (bodylen <= 0)
	    break;
	Buf[bodylen] = '\0';
	if (moreprintf("%s", Buf) == MORE_NO_MORE) return(MORE_NO_MORE);
	offset += bodylen;
    } while (bytesunfetched > 0);
    if (mserrcode || bodylen < 0) {
	if (offset) {
	    sprintf(ErrorText, "The file %s could not be read", FileName);
	}
	else {
	    sprintf(ErrorText, "The file %s could not be completely read", FileName);
	}
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

DisplayMessage(arg)
char   *arg;
{
    char   *cl, *nl,
	    Buf[MAXBODY],
	    ErrorText[256],
	    FileName[MAXPATHLEN + 1];
    int     bodylen, inheaders=HeadersOn, len, keptlast = 0;
    long    offset,
	    bytesunfetched;	/* *** Added for PC  8/20/86  *** */

    debug(1,("DisplayMessage %s\n", arg));
    if (ParseFileName(arg,FileName,FALSE)) return(-1);
    offset = 0;
    do {
	if ((mserrcode = MS_GetPartialFile(FileName, Buf, sizeof(Buf)-1, offset, &bytesunfetched, &bodylen)) != 0)
	    break;
	if (bodylen <= 0)
	    break;
	Buf[bodylen] = '\0';
	cl = Buf;
	while (inheaders && (nl = index(cl, '\n'))) {
	    inheaders = nl - cl;
	    len = inheaders + 1;
	    if (!inheaders) break;
	    *nl++ = '\0';
	    if (*cl == ' ' || *cl == '\t') {
		if (keptlast) if (moreprintf("%s\n", cl) == MORE_NO_MORE) return(MORE_NO_MORE);
	    } else {
		if (CheckHead(cl)) {
		    keptlast = 1;
		    if (moreprintf("%s\n", cl) == MORE_NO_MORE) return(MORE_NO_MORE);
		} else keptlast = 0;
	    }
	    offset += len;
	    bodylen -= len;
	    cl = nl;
	}
	if (inheaders) continue;
	if (moreprintf("%s", cl) == MORE_NO_MORE) return(MORE_NO_MORE);
	offset += bodylen;
    } while (bytesunfetched > 0);
    if (mserrcode || bodylen < 0) {
	if (offset) {
	    sprintf(ErrorText, "The file %s could not be read", FileName);
	}
	else {
	    sprintf(ErrorText, "The file %s could not be completely read", FileName);
	}
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

BrowseMsgs(arg)
char   *arg;
{
    debug(1,("BrowseMsgs %s\n", arg));
    CheckPrompted("Please enter message group name (and since date)")
    return(GetHeadersFn(arg,'s'));
}

GetHeaders(arg)
char   *arg;
{
    debug(1,("GetHeaders %s\n", arg));
    CheckPrompted("Please enter message group name (and since date)")
    return(GetHeadersFn(arg,'d'));
}

#define DATELEN  7
#define TIMELEN 10

GetHeadersFn(arg, fn)
char   *arg, fn;
{
    char   *date,
	   date64[DATELEN],
	    ErrorText[256],
	    newdate[TIMELEN],
	   *DirName;
    Boolean IsDone;

    debug(1,("GetHeadersFn %s\n", arg));
    if (ParseDirName(arg, &DirName)) return(-1);
    date = index(arg, ' ');
    if (date) {
	*date++ = '\0';
	date = StripWhiteEnds(date);
	if (!strncmp(date, "since ", 6) || !strncmp(date, "Since ", 6)) {
	    date += 6;
	    date = StripWhiteEnds(date);
	}
    }
    if (date!=NIL) {	     /* These are in two so we can skip since  */
    /* Convert date to base 64 -- there must be a better way to do this */
	if (0==strcmp(date,"lastread")) {
	    date64[0] = '\0';
	    if ((mserrcode = MS_GetAssociatedTime(DirName, date64, TIMELEN)) != 0) {
		 sprintf(ErrorText, "Couldn't get associated time for %s", DirName);
		 ReportError(ErrorText, ERR_WARNING, TRUE);
		 return(-1);
	    }
	    strcpy(ErrorText,"No new messages.\n");
	} else {
	    int year, month, day, hour, min, sec, wday;
	    long gtm;

	    mserrcode = MS_ParseDate(date, &year, &month, &day, &hour, &min, &sec, &wday, &gtm);
	    if (mserrcode) {
		sprintf(ErrorText, "I don't understand the date %s", date);
		ReportError(ErrorText, ERR_WARNING, TRUE);
		return(-1);
	    }
	    strcpy(date64, convlongto64(gtm, 0));
	    debug(2,("Converted date %s to %s\n", date, date64));
	    sprintf(ErrorText, "No messages have appeared since %d/%d/%d %d:%02d:%02d on %s\n",
		month + 1, day, year,
		hour, min, sec, DirName);
	}
    }
    else {
	*date64 = '\0';
	sprintf(ErrorText, "The message folder %s is empty\n", DirName);
    }
    if (!*date64) date=NULL;
	else date=date64;

    return( (fn=='s')? StepThroughMsgs(DirName, date, ErrorText,
					 newdate, &IsDone)  :
		       GetHeadersSinceDate(DirName, date, ErrorText));
}

GetHeadersSinceDate(DirName, date64, NothingMessage)
char *DirName, *date64, *NothingMessage;
{
    char headbuf[HEADBUFSIZE], ErrorText[256], *s;
    int     cuid, IsDup;
    long    numbytes,
	    totalbytes,
	    status;		/* ***	 Added 8/19 for PC *** */

    debug(1, ("GetHeadersSinceDate %s %s\n\t(nothing-message %s)", DirName, date64, NothingMessage));
    totalbytes = 0;
    do {
	if (CUI_GetHeaders(DirName, date64, headbuf, HEADBUFSIZE, totalbytes, &numbytes, &status, FALSE) != 0) {
	    ReportError("Could not get headers properly", ERR_WARNING, TRUE);
	    return(-1);
	}
	if (numbytes <= 0)
	    break;
	if (totalbytes==0) moreprintf("\n");
	totalbytes += numbytes;
	for (s = headbuf; s - headbuf < numbytes; s += AMS_SNAPSHOTSIZE) {
	    cuid = GetCuid(AMS_ID(s), DirName, &IsDup);
	    debug(2,("Date: %s ID: %s Caption:", AMS_DATE(s), AMS_ID(s)));
	    if (PrintCaption(cuid, s, IsDup) == MORE_NO_MORE) return(MORE_NO_MORE);
	}
    } while (status > 0);
    if (status < 0) {
	sprintf(ErrorText, "Couldn't read all of the headers successfully");
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    if (NothingMessage && totalbytes == 0) {
	if (MORE_NO_MORE == moreprintf("%s", NothingMessage)) return(MORE_NO_MORE);
    }
    return(0);
}

GetNextCommand(sptr, buf, lim)
char  **sptr;
char   *buf;
int	lim;
{
    int     i = 0;
    char   *s;

    debug(1,("GetNextCommand\n"));
    if ((long) *sptr == 0)
	return(-1);
    debug(2,("Looking through %s for next command\n", *sptr));

    while (**sptr && (**sptr == ' ' || **sptr == '\t' || **sptr == '\n')) {  /* Strip leading whitespace */
	++*sptr;
    }

    while (**sptr == '#') {             /* Check for comments */
	while (**sptr && **sptr != '\n')       /* Skip the comment */
	    ++*sptr;
	while (**sptr && (**sptr == ' ' || **sptr == '\t' || **sptr == '\n'))  /* Strip leading whitespace */
	    ++*sptr;
    }

    while (**sptr && **sptr != '\n' && **sptr != ';' && i < lim) {
	if (**sptr == '\\') {
	    ++*sptr;
	}
	if ( **sptr=='#' &&  (*(*sptr-1)==' ' || *(*sptr-1)=='\t')) {  /* Trying to allow trailing comments  */
		--*sptr;				      /* but not skip through bb#andrew.test */
		break;
	}
	buf[i++] = **sptr;
	if (**sptr) {
	    ++*sptr;
	}
    }
    if (i >= lim) {
	return(-1);
    }
    buf[i] = '\0';
    if (**sptr)
	++* sptr;
    while (**sptr && (**sptr == ' ' || **sptr == '\t' || **sptr == '\n')) {
	++*sptr;
    }
    s = *sptr;
    for (i = 0; *s != '\0'; ++i, ++s) {
	;
    }
    debug(2,("returning %s\n", buf));
    return(i);
}

PrintVersionNumbers() {
    char    msv[80];

    debug(1,("Print version numbers\n"));
    if ((mserrcode = MS_GetVersion(msv, sizeof msv)) != 0) {
	ReportError("Couldn't get message server version number", ERR_WARNING, TRUE);
	moreprintf("%s\n", CUI_VersionString);
	return(-1);
    }
    moreprintf("%s %s\n", CUI_VersionString, msv);
    return(0);
}

DemonLoop(arg, AliasCt)
char   *arg;
int	AliasCt;
{
    int     Period,
	    Passes;
    char   *cmd = arg,
	    *CmdBuf;

    debug(1,("DemonLoop %s\n", arg));
    while (*cmd && *cmd != ' ' && *cmd != '\n') {
	++cmd;
    }
    if (*cmd) {
	*cmd++ = '\0';
    }
    Passes = atoi(arg);
    if (!Passes) {
	ReportError("You must tell me how many times to loop (-1 is forever)", ERR_WARNING, FALSE);
	return(0);
    }
    arg = cmd;
    while (*cmd && *cmd != ' ' && *cmd != '\n') {
	++cmd;
    }
    if (*cmd) {
	*cmd++ = '\0';
    }
    Period = atoi(arg);
    if (Period <= 0) {
	ReportError("Must sleep for a positive number of seconds", ERR_WARNING, FALSE);
	return(0);
    }
    cmd = StripWhiteEnds(cmd);
    CmdBuf = (char *) malloc(strlen(cmd)+1);
    if (CmdBuf == NULL) {
	ReportError("Out of storage for command buffer", ERR_WARNING, FALSE);
	return(0);
    }
    while (TRUE) {
	strcpy(CmdBuf, cmd);	/* ProcessCommand eats its arguments */
	ProcessMultipleCommands(CmdBuf, AliasCt);
				/* always pass it top-level ct */
	if (Passes > 0 && --Passes == 0) {
	    free(CmdBuf);
	    return(0);
	}
	sleep(Period);
    }
}

MakeAlias(arg)
char   *arg;
{
    char   *word,
	   *s,
	    ErrorText[2000];
    int     i,j;

    debug(1,("MakeAlias %s\n", arg));
    CheckPrompted("Please enter an alias followed by a command")
    word = arg = StripWhiteEnds(arg);
    while (*arg && *arg != ' ' && *arg != '\n') {
	++arg;
    }
    if (*arg) {
	*arg++ = '\0';
    }
    word = StripWhiteEnds(word);
    arg = StripWhiteEnds(arg);
    if (!*word) {
	ReportError("Incomplete Alias definition -- aborted", ERR_WARNING, FALSE);
	return(-1);
    }
    if (!*arg) {
	    for (i = 0; Commands[i].Command; ++i) {
	if (!strcmp(Commands[i].Command, word)) {
	    if (Commands[i].Function != ExecuteAlias) {
		sprintf(ErrorText, "%s is a built-in command and cannot be removed", word);
		ReportError(ErrorText, ERR_WARNING, FALSE);
		return(-1);
	    }
		/* Found the alias, need to remove it */
		for (j = i; Commands[j].Command; ++j) {
		    ;
		}
		--j;
		free(Commands[i].Command);
		free(Commands[i].HelpText);
		Commands[i].Command = Commands[j].Command;
		Commands[i].Function = Commands[j].Function;
		Commands[i].HelpText = Commands[j].HelpText;
		Commands[j].Command = NULL;
		Commands[j].Function = NULL;
		Commands[j].HelpText = NULL;
		sprintf(ErrorText, "Removed alias for %s", word);
		ReportSuccess(ErrorText);
		return(0);
	    }
	}
    }
    for (s = word; *s; ++s) {
	if (isupper(*s))
	    *s = tolower(*s);
    }
    for (i = 0; Commands[i].Command; ++i) {
	if (!strcmp(Commands[i].Command, word)) {
	    if (Commands[i].Function != ExecuteAlias) {
		sprintf(ErrorText, "%s is a built-in command and cannot be aliased", word);
		ReportError(ErrorText, ERR_WARNING, FALSE);
		return(-1);
	    }
	    break;
	}
    }
    if (i + 1 >= MAXCOMMANDS) {
	ReportError("Sorry; alias limit exceeded.", ERR_WARNING, FALSE);
	return(-1);
    }
    if (!Commands[i].Command) {
	Commands[i + 1].Command = NULL;
	Commands[i + 1].Function = NULL;
	Commands[i + 1].HelpText = NULL;
    }
    Commands[i].Command = malloc(1 + strlen(word));
    if (!Commands[i].Command) {
	ReportError("Out of memory--cannot define new alias", ERR_WARNING, FALSE);
	return(-1);
    }
    strcpy(Commands[i].Command, word);
    Commands[i].HelpText = malloc(1 + strlen(arg));
    if (!Commands[i].HelpText) {
	free(Commands[i].Command);
	ReportError("Out of memory -- cannot define new alias body", ERR_WARNING, FALSE);
	return(-1);
    }
    strcpy(Commands[i].HelpText, arg);
    Commands[i].Function = ExecuteAlias;
    if (Interactive) {
	sprintf(ErrorText, "Defined '%s' as an alias for '%s'", word, arg);
	ReportSuccess(ErrorText);
    }
    return(0);
}

ExecuteAlias(Arguments, AliasCt, Definition)
char   *Arguments,
       *Definition;
int	AliasCt;
{
    char   *FullCommand;
    int     status;

    debug(1,("Execute Alias %s %s with count %d\n", Definition, Arguments, AliasCt));
    ++AliasCt;
    if (AliasCt > MAXDEALIASES) {
	ReportError("alias/source loop -- aborted", ERR_WARNING, FALSE);
	return(-1);
    }
    FullCommand = malloc(strlen(Arguments) + strlen(Definition) + 3);
    if (!FullCommand) {
	ReportError("Out of memory for alias expansion", ERR_WARNING, FALSE);
	return(-1);
    }
    sprintf(FullCommand, "%s %s", Definition, Arguments);
    debug(2,("Real command to execute is %s\n", FullCommand));
    status = ProcessMultipleCommands(FullCommand, AliasCt);
    free(FullCommand);
    return(status);
}

UpdateMess(FullName, NickName, substatus)
char   *FullName, *NickName;
int	substatus;
{
    Boolean IsDone = FALSE;
    int     code;
    char    date64[TIMELEN],
	    newdate[TIMELEN],
	    ErrorText[256],
	   *DirName;

    debug(1,("UpdateMess: %s\n", FullName));

    if (substatus == AMS_ASKSUBSCRIBED) {
	sprintf(ErrorText, "Check %s notices", NickName);
	if (!GetBooleanFromUser(ErrorText, FALSE))
	    return(0);
    }
    if ((mserrcode = CUI_DisambiguateDir(FullName, &DirName)) != 0) {
	if (AMS_ERRNO == ENOENT) {
	    return(CUI_HandleMissingFolder(FullName));
	}
	CUI_ReportAmbig(FullName, "folder");
	return(-1);
    }
    if (substatus == AMS_PRINTSUBSCRIBED) {
	return(CUI_PrintUpdatesWithFlags(FullName, NickName, (PrintFixed ? AMS_PRINT_FIXED : 0) | (PrintRot13 ? AMS_PRINT_ROT13 : 0), NULL));
    }
    if (substatus == AMS_SHOWALLSUBSCRIBED) {
	moreprintf("Showing all of %s ...  ", NickName);
	strcpy(date64, "000000");
    } else {
	if (MORE_NO_MORE == moreprintf("Checking %s ...  ", NickName)) {
	    return(MORE_NO_MORE);
	}
	fflush(stdout);
	date64[0] = '\0';
	if ((mserrcode = MS_GetAssociatedTime(DirName, date64, TIMELEN)) != 0) {
	    moreprintf("\n");
	    fflush(stdout);
	    sprintf(ErrorText, "Couldn't get associated time for %s", DirName);
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	    return(-1);
	}
	debug(4,("Updating messages on %s since %s\n", DirName, date64));
	newdate[0] = '\0';
	if (date64[0] == '\0') {
	    strcpy(date64, convlongto64(time(0) - (60L * 60L * 24L * DEFAULTUPDATES), 0));
	    strcpy(newdate, date64);
	    sprintf(ErrorText, "\nFirst time updating %s\nStarting with messages from %d days ago...", DirName, DEFAULTUPDATES);
	    ReportSuccess(ErrorText);
	}
    }
    code = GetHeadersSinceDate(DirName, date64, NULL);
    if ((code != MORE_NO_MORE) && (code!=0)) return(code);
    code=StepThroughMsgs(DirName, date64, ErrorText, newdate, &IsDone);
    if (newdate[0]) {
	debug(4,("Setting associated time for %s to %s\n", DirName, newdate));
	mserrcode = MS_SetAssociatedTime(DirName, newdate);
	if (!mserrcode)
	    mserrcode = MS_FastUpdateState();
	if (mserrcode) {
	    ReportError("Could not update your profile", ERR_WARNING, TRUE);
	}
    }
    return(code);
}

int StepThroughMsgs(DirName, date64, ErrorText, newdate, IsDone)
char *DirName, *date64, *ErrorText, *newdate;
Boolean *IsDone;
{
    Boolean StayHere;
    char    AnsBuf[ANSBUFMAX],
	    headbuf[HEADBUFSIZE],
	    *s, *arg2, *arg;
    int     cuid, IsDup;
    long    totalbytes, skipvalue, /* totalbytes is an offset into the file */
	    numbytes,		/* Numbytes is how many were returned to us */
	    status;		/* Status is how many are left to be fetched */
    totalbytes = 0;
    skipvalue = 0;
    *IsDone=FALSE;
    *newdate = '\0';
    do {
	int	Ans,
		MoreDefault;
	if (CUI_GetHeaders(DirName, date64, headbuf, HEADBUFSIZE, totalbytes, &numbytes, &status, FALSE) != 0) {
	    ReportError("Could not get the headers", ERR_WARNING, TRUE);
	    return(-1);
	}
	if (numbytes <= 0)
	    break;
	if (totalbytes==0) moreprintf("\n\n");
	totalbytes += numbytes;
	skipvalue = 0;
	for (s = headbuf; s - headbuf < numbytes; s += AMS_SNAPSHOTSIZE) {
	    cuid = GetCuid(AMS_ID(s), DirName, &IsDup);
	    debug(4,("Date: %s ID: %s Caption:", AMS_DATE(s), AMS_ID(s)));
	    if (PrintCaption(cuid, s, IsDup) == MORE_NO_MORE) return(MORE_NO_MORE);
	    MoreDefault = MORE_TYPE;
	    StayHere = TRUE;
	    while (StayHere) {
		Ans = MoreSelect(MoreDefault, AnsBuf, ANSBUFMAX, Actions, NIL, "CUI READ>");
		if (Interactive && Ans == -2) {
		    moreprintf("\nEOF ignored; use 'quit' to exit READ mode or 'help' for help.\n");
		    continue;
		}
		if (Ans<0) return(MORE_NO_MORE);
		switch (Ans) {
		    case MORE_PROCEED:
			return(0);    /* Don't update profile with this message */
		    case MORE_TYPE:
			arg=AnsBuf;
			while (1) {
			    if ((*arg) != '\0') cuid=atoi(arg);
			    GetBodyFromCUID(cuid);
			    arg2=index(arg,' ');
			    arg=index(arg,',');
			    if (arg2 && (!arg || arg2 < arg)) {
				arg = arg2;
			    }
			    if (arg && *arg) arg++;
			      else break;
			}
			MoreDefault = MORE_NEXT;
			break;
		    case MORE_VIEW:
			if (AnsBuf[0] != '\0') {
			/* * skipvalue=atoi(AnsBuf)-cuid;
			    CalcSkip(skipvalue,&s,headbuf,numbytes,&StayHere,&totalbytes,&status); *** */
			    cuid=atoi(AnsBuf);
			}
			ViewMessage(cuid);
			MoreDefault = MORE_NEXT;
			break;
		    case MORE_CLASS:
			if (!*AnsBuf) {
			    ReportError("You must enter a new folder name.",ERR_WARNING, FALSE);
			}
			else  CUI_CloneMessage(cuid, AnsBuf, MS_CLONE_COPYDEL);
			MoreDefault = MORE_NEXT;
			break;
		    case MORE_COPY:
			if (!*AnsBuf) {
			    ReportError("You must enter a new folder name.",ERR_WARNING, FALSE);
			}
			else CUI_CloneMessage(cuid, AnsBuf, MS_CLONE_COPY);
			break;
		    case MORE_APPEND:
			if (!*AnsBuf) {
			    ReportError("You must enter a new folder name.",ERR_WARNING, FALSE);
			}
			else CUI_CloneMessage(cuid, AnsBuf, MS_CLONE_APPEND);
			break;
		    case MORE_WRITE:
			if (!*AnsBuf) {
			    ReportError("You must enter a file name.",ERR_WARNING, FALSE);
			}
			else {
			    StoreMessage(cuid, AnsBuf, 0L);
			}
			break;
		    case MORE_DEL:
			if (AnsBuf[0] != '\0') {
			/* * skipvalue=atoi(AnsBuf)-cuid;
			    CalcSkip(skipvalue,&s,headbuf,numbytes,&StayHere,&totalbytes,&status); ** */
			    cuid=atoi(AnsBuf);
			}
			if (!CUI_DeleteMessage(cuid)) {
			    sprintf(ErrorText, "Deleted message %d", cuid);
			    ReportSuccess(ErrorText);
			}
			MoreDefault = MORE_NEXT;
			break;
		    case MORE_PRINT:
			sprintf(ErrorText, "Trying to print message %d; please wait...", cuid);
			ReportSuccess(ErrorText);
			if (AnsBuf[0] != '\0') {
			/* * skipvalue=atoi(AnsBuf)-cuid;
			    CalcSkip(skipvalue,&s,headbuf,numbytes,&StayHere,&totalbytes,&status);  ** */
			    cuid=atoi(AnsBuf);
			}
			CUI_PrintBodyFromCUIDWithFlags(cuid, (PrintFixed ? AMS_PRINT_FIXED : 0) | (PrintRot13 ? AMS_PRINT_ROT13 : 0), NULL);
			MoreDefault = MORE_NEXT;
			break;
		    case MORE_SKIP:
			if (strlen(AnsBuf)==0) StayHere = FALSE;
			  else {
			       skipvalue=atol(AnsBuf) * AMS_SNAPSHOTSIZE;
			       CalcSkip(skipvalue,&s,headbuf,numbytes,&StayHere,&totalbytes,&status);
			  }
			break;
		    case MORE_NEXT:
			StayHere = FALSE;
			break;
		    case MORE_MAIL:
			SendSomeMail(0, AnsBuf, AMS_REPLY_FRESH);
			break;
		    case MORE_FORWARD: {
			char *realname=NULL;

			if (RealWhoIs(AnsBuf, &realname) && !GetBooleanFromUser("Proceed with mail composition anyway", FALSE))
			    break;
			SendSomeMail(cuid, realname, AMS_REPLY_FORWARD);
			if (realname) free(realname);
			break;
			}
		    case MORE_RESEND:
			{
			char Buf[500];
			sprintf(Buf, "%d %s", cuid, AnsBuf);
			ResendCmd(Buf);
			break;
			}
		    case MORE_REPLY:
			SendSomeMail(cuid, NIL, AMS_REPLY_SENDER);
			break;
		    case MORE_WIDE:
			SendSomeMail(cuid, NIL, AMS_REPLY_WIDE);
			break;
		    case MORE_ALL:
			SendSomeMail(cuid, NIL, AMS_REPLY_WIDER);
			break;
		    case MORE_SET:
			SetOption(AnsBuf);
			break;
		    default:
			sprintf(ErrorText, "Unexpected parse of answer: %d\n", Ans);
			ReportError(ErrorText, ERR_CRITICAL, FALSE);
			break;
		}	/* End switch */
	    }	/* End while  */
	    if (s - headbuf < numbytes) strcpy(newdate, AMS_DATE(s));
	    if (*IsDone)
		break;
	}   /* 	End  for  */
	if (*IsDone) {
	    break;
	}
    } while (status > 0);
    if (status < 0) {
	sprintf(ErrorText, "Couldn't read all of the headers successfully");
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    if (totalbytes == 0) {
	moreprintf("No new messages.\n");
    }
    return(0);
}

CalcSkip(skipvalue,s,headbuf,numbytes,StayHere,totalbytes,status)
long *totalbytes, skipvalue, numbytes, *status;
char **s, *headbuf;
Boolean *StayHere;
{
/* * debug(1,("<CalcSkip> skipvalue=%ld, %ld bytes buffered\n", skipvalue, numbytes));
 *  debug(1,("<CalcSkip> %ld bytes remaining, current buffer ended at %ld\n",
 *			*status, *totalbytes));
 *  debug(1,("<CalcSkip> We are %ld bytes into this one\n", (long)((*s)-headbuf)));  *** */
    *StayHere = FALSE;
    /* 	First case is where we are inside this buffer load */
    if ((skipvalue + *s - headbuf < numbytes) &&
	(skipvalue + *s - headbuf >= 0)) {
       *s += (skipvalue-AMS_SNAPSHOTSIZE);
  /* ** debug(1,("<CalcSkip> Returning buffer offset of %ld bytes\n", (long)((*s)-headbuf)));  ** */
       return;
    }
    /* Here we know we are outside this gulp.	 */
/* * debug(1,("<CalcSkip> Going to new buffer.\n"));  ** */

    /* Calculate new file offset, not buffer offset */
    skipvalue = (*s-headbuf)-numbytes+(*totalbytes)+skipvalue;

    if ( skipvalue < (*totalbytes)+(*status)) { /* Are we skipping in range? */
       *totalbytes=skipvalue;
       if (*totalbytes<0) *totalbytes=0;
       *s += numbytes;	 /* to pop us out of for loop */
       (*status) = 1L;	    /* keep us in do loop	 */
    } else { /* Go to last message */
       if ((*status)==0) /* but if there is only ONE buffer load, stay in it   */
	    *s = headbuf + numbytes - (2*AMS_SNAPSHOTSIZE) ;
	 else {
	    (*totalbytes)+=(*status)-AMS_SNAPSHOTSIZE;
	    *s += numbytes;
	    (*status) = 1L;	 /* keep us in do loop	      */
	 }
    }
/* * debug(1,("<CalcSkip> returning new file offset of %ld bytes\n", *totalbytes));
 *  debug(1,("<CalcSkip> buffer offset of %ld bytes\n", (long)((*s)-headbuf)));
 *  debug(1,("<CalcSkip> and %ld bytes remaining\n", *status));   ******* */
}

UpdateMsgs(arg, AliasCt)
char   *arg;
int	AliasCt;
{
    char   *dname;
    char mapFile[1+MAXPATHLEN], lmapFile[1+MAXPATHLEN];
    char FullName[1+MAXPATHLEN], NickName[1+MAXPATHLEN];
    char *start, *end;
    int numChanged, junk;
    FILE *mapfl;
    int substatus, code;

    CheckQuestion("Press enter to update all groups you are subscribed to,\nor enter a list of groups")
    dname = StripWhiteEnds(arg);
    if (*dname == '\0') {
	CUI_CheckMailboxes(NULL);
	if (mserrcode = MS_NameChangedMapFile(mapFile, FALSE, FALSE, &numChanged, &junk, &junk, &junk, &junk)) {
	    ReportError("Cannot get list of changed folders\n", ERR_CRITICAL, TRUE);
	    return(-1);
	}
	moreprintf("%d folder%s with new messages\n", numChanged,
		   numChanged == 1 ? "" : "s");

	CUI_GenLocalTmpFileName(lmapFile);
	if(CUI_GetFileFromVice(lmapFile, mapFile)!=0 || (mapfl=fopen(lmapFile, "r"))==NULL) {
	    ReportError("Cannot open changed-map-file\n", ERR_CRITICAL, TRUE);
	    MS_UnlinkFile(mapFile);
	    return(-1);
	}
	
	while(fgets(NickName, sizeof(NickName), mapfl)) {
	    start = index(NickName, ':');
	    if (!start) continue;
	    *start++ = '\0';
	    end = index(start, ' ');
	    if (!end) continue;
	    *end++ = '\0';

	    strcpy(FullName, start);
	    substatus = atoi(end);

	    if (code = UpdateMess(FullName, NickName, substatus)) {
		if (code == MORE_NO_MORE) return(0);
		fclose(mapfl);
		unlink(lmapFile);
		MS_UnlinkFile(mapFile);
		return(-1);
	    }
	}

	fclose(mapfl);
	unlink(lmapFile);
	MS_UnlinkFile(mapFile);
	return(0);
    }
    else {
	char *DirName, *nextdname;

	while (*dname != '\0') {
	    for (nextdname = dname; *nextdname && *nextdname != ' '; ++nextdname) {
		;
	    }
	    if (*nextdname) {
		*nextdname = '\0';
		++nextdname;
	    }
	    if (ParseDirName(dname, &DirName)) {
		dname = nextdname; /* added by nsb 5/22/87 */
		continue;
	    }
	    CUI_CheckMailboxes(DirName);
	    if ((mserrcode = MS_GetSubscriptionEntry(DirName, NickName, &substatus)) != 0) {
		ReportError("Cannot get your subscription information", ERR_CRITICAL, TRUE);
		return(-1);
	    }

	    if (substatus==AMS_ASKSUBSCRIBED)	/* Since they specifically asked for it, give it.  */
		substatus=AMS_ALWAYSSUBSCRIBED;

	    if (code = UpdateMess(DirName, NickName, substatus)) {
		if (code == MORE_NO_MORE) return(0);
		return(-1);
	    }
	    dname = nextdname;
	}
    }
    return(0);
}

AlterSubscription(arg, AliasCt)
char   *arg;
int	AliasCt;
{
    long    spcode;
    int     i = 0;
    char    MapFile[MAXPATHLEN + 1],
	    PathElt[MAXPATHLEN + 1],
	    ErrorText[256];

    CheckQuestion("Press enter to check subscription to all message groups,\nor enter a list of groups to subscribe to")
    arg = StripWhiteEnds(arg);
    if (*arg) {
	return(AlterSubscriptionLine(arg));
    }
    else
	do {
	    spcode = MS_GetSearchPathEntry(i, PathElt, MAXPATHLEN);
	    ++i;
	    if (spcode) {
		break;
	    }
	    if ((mserrcode = MS_NameSubscriptionMapFile(PathElt, MapFile)) != 0) {
		if (AMS_ERRNO == ENOENT)
		    continue;	/* user may not have his own message dir,
				   for example */
		sprintf(ErrorText, "MS can not generate subscription map file for %s", PathElt);
		ReportError(ErrorText, ERR_CRITICAL, TRUE);
		continue;
	    }
	    if (MapcarFunctionToFileLines(AlterSubscriptionLine, MapFile, AliasCt) != 0) {
		ReportSuccess("Subscription editing aborted");
		unlink(MapFile);
		return(-1);
	    }
	    unlink(MapFile);
	} while (!spcode);
    return(0);
}

AlterSubscriptionLine(text)
char   *text;
{
    int code;
    code = AlterSubLine(text);
    if (code) {
	if ((AMS_ERRNO == EACCES) || GetBooleanFromUser("Do you want to continue editing your subscriptions", FALSE)) {
	    code = 0;
	}
    }
    return(code);
}

AlterSubLine(text)
char   *text;
{
    char   *Full,
	    Nick[MAXPATHLEN + 1],
	    ErrorText[256],
	   *s,
	   *t,
	   *mynick = NULL,
	    text2[MAXPATHLEN + 1];
    int     status,
	    newstatus,
	    alen;

    text = StripWhiteEnds(text);
    for (s = text; *s && *s != ':'; ++s) {
	;
    }
    if (*s == ':') {
	mynick = text;
	*s++ = '\0';
	text = s;
    }
    if (*text == '*')
	++text;
    for (s = text; *s; ++s) {
	if (*s == ' ' || *s == '\t')
	    *s = '\0';
    }
    strcpy(text2, text);
    for (s = text2 + 1; *s; ++s) {
	if (*s == '.' && *(s - 1) != '/')
	    *s = '/';
    }
    if ((mserrcode = CUI_DisambiguateDir(text2, &Full)) != 0) {
	CUI_ReportAmbig(text, "folder");
	return(-1);
    }
    if ((mserrcode = MS_GetSubscriptionEntry(Full, Nick, &status)) != 0) {
	sprintf(ErrorText, "Cannot get subscripton entry for %s", Full);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    if (mynick) {
	strcpy(Nick, mynick);
    }
    if (Nick[0] == '\0') {
	mynick = rindex(text, '/');
	strcpy(Nick, mynick ? mynick + 1 : text);
    }
tryagain:
    switch (status) {
	case AMS_ALWAYSSUBSCRIBED:
	    moreprintf("Subscribe to %s (Yes, No, Ask, ShowAll, Print, Quit) [Yes] ? ", Nick);
	    break;
	case AMS_ASKSUBSCRIBED:
	    moreprintf("Subscribe to %s (Yes, No, Ask, ShowAll, Print, Quit) [Ask] ? ", Nick);
	    break;
	case AMS_PRINTSUBSCRIBED:
	    moreprintf("Subscribe to %s (Yes, No, Ask, ShowAll, Print, Quit) [Print] ? ", Nick);
	    break;
	case AMS_SHOWALLSUBSCRIBED:
	    moreprintf("Subscribe to %s (Yes, No, Ask, ShowAll, Print, Quit) [ShowAll] ? ", Nick);
	    break;
	case AMS_UNSUBSCRIBED:
	default:
	    moreprintf("Subscribe to %s (Yes, No, Ask, ShowAll, Print, Quit) [No] ? ", Nick);
	    break;
    }
    fflush(stdout);
    s = GetLine();
    if (s == (char *) - 1)
	return(-1);
    for (alen = 0, t = s; *t; ++t, ++alen) {
	if (isupper(*t)) {
	    *t = tolower(*t);
	}
    }
    newstatus = status;
    if (*s) {
	if (strncmp(s, "yes", alen) == 0) {
	    newstatus = AMS_ALWAYSSUBSCRIBED;
	}
	else
	    if (strncmp(s, "no", alen) == 0) {
		newstatus = AMS_UNSUBSCRIBED;
	    }
	else
	    if (strncmp(s, "ask", alen) == 0) {
		newstatus = AMS_ASKSUBSCRIBED;
	    }
	else
	    if (strncmp(s, "showall", alen) == 0) {
		newstatus = AMS_SHOWALLSUBSCRIBED;
	    }
	else
	    if (strncmp(s, "print", alen) == 0) {
		newstatus = AMS_PRINTSUBSCRIBED;
	    }
	else
	    if (strncmp(s, "quit", alen) == 0) {
		return(-1);
	    }
	else {
	    moreprintf("Choose one of the following:\n\tYes -- subscribe to this message group\n\tNo -- Do not subscribe\n\tAsk -- subscribe, but don't see the notices automatically\n\tQuit -- Go back to CUI prompt\n");
	    goto tryagain;
	}
    }
    if (status != newstatus && (mserrcode = MS_SetSubscriptionEntry(Full, Nick, newstatus)) != 0) {
	ReportError("Cannot set subscription entry values", ERR_CRITICAL, TRUE);
	return(-1);
    }
    return(0);
}

KillServer() {
    if ((mserrcode = MS_Die()) != 0) {
	ReportError("Could not kill message server", ERR_WARNING, TRUE);
    }
    return(0);
}

EchoArgs(arg)
char   *arg;
{
    moreprintf("%s\n", arg);
    return(0);
}

ForkYourself() {
    if (fork()) {
	exit(0);
    }
    RedirectOutput();
    return(0);
}

DeleteMessages(arg)
char   *arg;
{
    int     cuid,
	rc;
    char    ErrorText[256], *arg2;

    debug(1,("DeleteMessages %s\n", arg));
    while (1) {
	 if ((cuid=ParseMessageNumber(arg))<0) return(-1);
	 rc = CUI_DeleteMessage(cuid);
	 if (rc) return(-1);		/* already reported error */
	 sprintf(ErrorText, "Deleted message %d", cuid);
	 ReportSuccess(ErrorText);
	 arg2=index(arg,' ');
	 arg=index(arg,',');
	 if (arg2 && (!arg || arg2 < arg)) {
		arg = arg2;
	}
	 if (arg && *arg) arg++;
	    else break;
    }
    return(0);
}

UndeleteMessages(arg)
char   *arg;
{
    int     cuid,
	rc;
    char    ErrorText[256], *arg2;

    debug(1,("UndeleteMessages %s\n", arg));
    while (1) {
	 if ((cuid=ParseMessageNumber(arg))<0) return(-1);
	 rc = CUI_UndeleteMessage(cuid);
	 if (rc) return(-1);		/* already reported error */
	 sprintf(ErrorText, "Undeleted message %d", cuid);
	 ReportSuccess(ErrorText);
	 arg2=index(arg,' ');
	 arg=index(arg,',');
	 if (arg2 && (!arg || arg2 < arg)) {
		arg = arg2;
	}
	 if (arg && *arg) arg++;
	    else break;
    }
    return(0);
}

Epoch(arg)
char   *arg;
{
    char *dname,
        *date,
        *FullName,
         NameBuf[1+MAXPATHLEN],
         date64[DATELEN],
         ErrorText[1000];
    int year, month, day, hour, min, sec, wday;
    long gtm;

    debug(1,("Epoch %s\n", arg));
    dname = StripWhiteEnds(arg);
    date = index(dname, ' ');
    if (date) {
	*date++ = '\0';
	date = StripWhiteEnds(date);
    }
    if (!dname || !*dname) {
	ReportError("You must supply the name of a folder tree", ERR_WARNING, FALSE);
	return(-1);
    }
    if ((mserrcode = CUI_DisambiguateDir(dname, &FullName)) != 0) {
 	if ((mserrcode = MS_DisambiguateFile(dname, NameBuf, AMS_DISAMB_FILEEXISTS))) {
	    CUI_ReportAmbig(arg, "folder");
	    return(-1);
	}
	FullName = NameBuf;
    }
    dname = FullName;
    if (!date || !*date) {
	ReportError("You must supply a date on which to begin the new epoch", ERR_WARNING, FALSE);
	return(-1);
    }

/* Convert date to base 64 -- there must be a better way to do this */
    mserrcode = MS_ParseDate(date, &year, &month, &day, &hour, &min, &sec, &wday, &gtm);
    if (mserrcode) {
	sprintf(ErrorText, "I don't understand the date %s", date);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    hour = 0;
    min = 0;
    sec = 0;
    strcpy(date64, convlongto64(gtm, 0));
    debug(2,("Converted date %s to %s\n", date, date64));

    if (Interactive) {
	sprintf(ErrorText, "Warning.  You are about to delete all messages in the folder \n\t%s\nand all its subfolders dated before\n\t%d/%d/%d %d:%02d.\n\nAre you sure you want to do this",
	    dname, month + 1, day, year,
	    hour, min, sec);
	if (!GetBooleanFromUser(ErrorText, FALSE)) {
	    ReportSuccess("No epoch performed.");
	    return(0);
	}
    }
    if ((mserrcode = MS_Epoch(dname, date64)) != 0) {
	ReportError("Epoch failed or partially failed", ERR_CRITICAL, TRUE);
	return(-1);
    }
    return(0);
}

WhenIs(arg)
char   *arg;
{
    int year, month, day, hour, min, sec, wday;
    long gtm;
    char    ErrorText[256];

    CheckPrompted("Please enter a date")

    mserrcode = MS_ParseDate(arg, &year, &month, &day, &hour, &min, &sec, &wday, &gtm);
    if (mserrcode) {
	sprintf(ErrorText, "I don't understand the date %s", arg);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    debug(1, ("Day of week is %d; unix time is %d\n", wday, gtm));
    moreprintf("%d/%d/%d %d:%02d:%02d\n",
	    month + 1, day, year,
	    hour, min, sec);
    return(0);
}

WriteFile(arg, AliasCt)
char   *arg;
int	AliasCt;
{
    long offset = 0L;
    char FullName[1+MAXPATHLEN];

    CheckPrompted("Please enter a file name")
    arg = StripWhiteEnds(arg);
    if (mserrcode = MS_DisambiguateFile(arg, FullName, AMS_DISAMB_FILEMAYBECREATED)) {
	ReportError("The file name makes no sense", ERR_WARNING, TRUE);
    }
    if (Interactive) {
	moreprintf("Enter the text you want to write to file %s:\n", FullName);
	moreprintf("To end, %s.\n", EOF_STRING);
    }
    if (StorePartialFile(FullName, &offset, 0600))
	return(-1);
    if (Interactive)
	ReportSuccess("File stored by message server");
    return(0);
}

MailCmd(arg)
char   *arg;
{
    debug(1,("MailCmd %s\n", arg));
    CheckQuestion("Press enter, or enter a name or network address")
    SendSomeMail(0, arg, AMS_REPLY_FRESH);
}

ReplyMailCmd(arg)
char   *arg;
{
    int     cuid;

    debug(1,("ReplyMailCmd %s\n", arg));
    if ((cuid=ParseMessageNumber(arg))<0) return(-1);
    return(SendSomeMail(cuid, NULL, AMS_REPLY_SENDER));
}

WideReplyMailCmd(arg)
char   *arg;
{
    int     cuid;

    debug(1,("ReplyMailCmd %s\n", arg));
    if ((cuid=ParseMessageNumber(arg))<0) return(-1);
    return(SendSomeMail(cuid, NULL, AMS_REPLY_WIDE));
}

WiderReplyMailCmd(arg)
char   *arg;
{
    int     cuid;

    debug(1,("ReplyMailCmd %s\n", arg));
    if ((cuid=ParseMessageNumber(arg))<0) return(-1);
    return(SendSomeMail(cuid, NULL, AMS_REPLY_WIDER));
}

ForwardMailCmd(arg)
char   *arg;
{
    char *addr;
    int     cuid, code;
    char    *realname=NULL;	/*init so we can see if we want to free later*/
    debug(1,("ForwardMailCmd %s\n", arg));
    if ((cuid=ParseMessageNumber(arg))<0) return(-1);
    addr=index(arg,' ');
    if (addr) {  /* Address was included. */
	if (RealWhoIs(addr, &realname) && !GetBooleanFromUser("Proceed with mail composition anyway", FALSE))
	    return(0);
	addr=realname;
    }
    code = SendSomeMail(cuid, addr, AMS_REPLY_FORWARD);
    if (realname!=NULL) free(realname);
    return(code);
}

Redraft(arg)
char   *arg;
{
    int     cuid;

    debug(1,("Redraft %s\n", arg));
    if ((cuid=ParseMessageNumber(arg))<0) return(-1);
    return(SendSomeMail(cuid, NULL, AMS_REPLY_REDRAFT));
}

SendSomeMail(cuid, to, code)
int	cuid,
	code;
char   *to;
{
    char   *id, *dir, 
	    TmpName[MAXPATHLEN + 1], TmpName2[MAXPATHLEN + 1],
	    AnsBuf[MAXPATHLEN + 1],
	    ShortFolderName[MAXPATHLEN + 1],
            *FolderName,
	    ErrorText[256];
    int     returncode, Ans;
    long    offset, bodyoffset = 0;
    Boolean MaySubmit, SubmitHosed;

    if (code!=AMS_REPLY_FRESH) {
	if (cuid <= 0) {
	    sprintf(ErrorText, "Illegal message number: %d", cuid);
	    ReportError(ErrorText, ERR_WARNING, FALSE);
	    return(-1);
	}
	if (GetAMSID(cuid, &id, &dir) != 0) {
	    sprintf(ErrorText, "There is no such message number as %d", cuid);
	    ReportError(ErrorText, ERR_WARNING, FALSE);
	    return(-1);
	}
	CUI_GenTmpFileName(TmpName);
	/* We change AMS_REPLY_FORWARD to AMS_REPLY_FORWARD_FMT to avoid a  stupid prefix about including your text here */ 
	if (CUI_NameReplyFile(cuid, code == AMS_REPLY_FORWARD ?  AMS_REPLY_FORWARD_FMT : code, TmpName) != 0) 
	    return(-1);
	if (code == AMS_REPLY_REDRAFT) {
	    sprintf(ErrorText, "Restored message draft from message %d.", cuid);
	    ReportSuccess(ErrorText);
	} else {
	    /*   Could put auto-edit parameter here... EditFile(TmpName);** */
	    if (code == AMS_REPLY_FORWARD) {
		char *realname, HeadBuf[2000];
		if (!to || !*to) {
		    moreprintf("Forward To: ");
		    fflush(stdout);
		    to = GetLine();
		    if (to == (char *) - 1) return(-1);
		}
		if (RealWhoIs(to, &realname) && !GetBooleanFromUser("Proceed with mail composition anyway", FALSE)) return(-1);
		sprintf(HeadBuf, "To: %s\n", realname);
		free(realname);
		CUI_GenTmpFileName(TmpName2);
		if (PutStringToViceFile(TmpName2, HeadBuf)) return(-1);
		moreprintf("Enter your prefix to the message and then %s\n", EOF_STRING);
		/* The hard-wired 4 in the next line is a terrible hack.
		   It reflects the fact that the first line of the file is "To: " */
		CUI_CopyViceFileTails(TmpName, 5L, TmpName2, (long) strlen(HeadBuf));
		offset = FindBodyStartInForwardedViceFile(TmpName2);
		if (offset<=0) return(-1);
		bodyoffset = offset - strlen(HeadBuf) + 4; /* compensate for ``terrible hack'' */
		strcpy(HeadBuf, TmpName2);
		strcpy(TmpName2, TmpName);
		strcpy(TmpName, HeadBuf);
	    } else {
		moreprintf("Enter the body of your message and then %s\n", EOF_STRING);
		/* Following hack to get the length of the reply file. ** */
		if ((mserrcode = MS_GetPartialFile(TmpName, NIL, 0, 0L, &offset, &returncode)) != 0) {
		    return(-1);
		}
	    }
	    if (StorePartialFile(TmpName, &offset, 0600)) return(-1); /* Error reported */
	    if (code == AMS_REPLY_FORWARD) {
		if (CUI_CopyViceFileTails(TmpName2, bodyoffset, TmpName, offset)) return(-1);
		MS_UnlinkFile(TmpName2); /* ignore errors */
	    }
	}
    }
    else {
	CUI_GenTmpFileName(TmpName);
	if (0 != SendSomeLines(TmpName, to, &offset, code, cuid)) {
		return(-1);
	}
    }
    SubmitHosed = TRUE;
    while (SubmitHosed) {

       MaySubmit = FALSE;
       while (!MaySubmit) {
	  Ans=MoreSelect(SEND_TYPE, AnsBuf, MAXPATHLEN, SendOpts, NIL, "CUI SEND>");
	    if (Interactive && Ans == -2) {
		moreprintf("\nEOF ignored; use 'quit' to exit SEND mode or 'help' for help.\n");
		continue;
	    }
	   if (Ans<0) {
	       MS_UnlinkFile(TmpName); /* ignore errors */
	       return(0);
	   }
	   switch (Ans) {
	       case SEND_EDIT:
		   if (EditFile(TmpName, EDIT_MODE) == 1) {
		       return(0);  /* Running sendmessage */
		   }
		   break;
	       case SEND_TYPE:
		   DisplayFile(TmpName);
		   moreprintf("\n\n(Other headers will be added by the message server and delivery system.)\n");
		   break;
	       case SEND_SUBMIT:
		   MaySubmit = TRUE;
		   break;
	       case SEND_SET:
		   SetOption(AnsBuf);
		   break;
	       case SEND_SAVEDRAFT:
		   GetStringFromUser("In what folder do you want to save the draft message", ShortFolderName, sizeof(ShortFolderName), FALSE);
		   if (CUI_DisambiguateDir(ShortFolderName, &FolderName)) {
		       CUI_ReportAmbig(ShortFolderName, "folder");
		       break;
		   }
		   mserrcode = MS_AppendFileToFolder(TmpName, FolderName);
		   if (mserrcode) {
		       sprintf(ErrorText, "Could not save draft in folder %s.", ShortFolderName);
		       ReportError(ErrorText, ERR_WARNING, TRUE);
		       break;
		   }
		   sprintf(ErrorText, "Draft saved in folder %s.", ShortFolderName);
		   ReportSuccess(ErrorText);
		   return(0);
	   }
       }
       moreprintf("Submitting mail; please wait...\n");
       *AnsBuf = '\0';
       if (CUI_ValidateFile(TmpName, TmpName2)) {
	   moreprintf("Please fix the bad recipients using the 'edit' command.\n");
       } else {
	   MS_UnlinkFile(TmpName); /* ignore errors */
	   strcpy(TmpName, TmpName2); /* In case we loop again */
	   SubmitHosed=CUI_SubmitMessage(TmpName, BlindStatus);
       }
    }
    if (code != AMS_REPLY_FRESH) CUI_MarkRepliedTo(cuid);
    return(0);
}

SendSomeLines(TmpName, to, offset_p, code, cuid)
char *TmpName, *to;
long *offset_p;
int code, cuid;
{
    char *s, HeadBuf[3000], *realname, BigBuf[1000];
    if (!to || !*to) {
	moreprintf("To: ");
	fflush(stdout);
	to = GetLine();
	if (to == (char *) - 1)
	   return(-1);
    }
    if (RealWhoIs(to, &realname) && !GetBooleanFromUser("Proceed with mail composition anyway", FALSE))
	return(-1);
    sprintf(HeadBuf, "To: %s\n", realname);
    if (realname) free(realname);
    if (code==AMS_REPLY_FRESH) {
	for (;;) {
	    moreprintf("Subject: ");
	    fflush(stdout);
	    s = GetLine();
	    if (s == (char *) - 1) return(-1);
	    s = StripWhiteEnds(s);
	    if (*s) break;
	    ReportError("You must provide a subject", ERR_WARNING, FALSE);
	}
	strcpy(BigBuf, "Subject: ");
	strcat(BigBuf, s);
	moreprintf("CC: ");
	fflush(stdout);
	s = GetLine();
	if (s == (char *) - 1)
	    return(-1);
	strcat(BigBuf, "\nCC: ");
        if (RealWhoIs(s, &realname) && !GetBooleanFromUser("Proceed with mail composition anyway", FALSE)) {
	    return(-1);
	}
	strcat(BigBuf, realname);
    } else {
	GetSubject(cuid, BigBuf, 1000);
    }
    strcat(HeadBuf, BigBuf);
    strcat(HeadBuf, "\n\n");
    *offset_p = (long)strlen(HeadBuf);
    mserrcode = MS_StorePartialFile(TmpName, 0L, strlen(HeadBuf), 0600, TRUE, HeadBuf);
    if (mserrcode) {
	ReportError("Message server cannot store file", ERR_WARNING, TRUE);
	return(-1);
    }
    moreprintf("Enter the body of your message and then %s\n", EOF_STRING);
    if (StorePartialFile(TmpName, offset_p, 0600)) {
	return(-1);		/* Error was reported */
    }
    return(0);
    }

GetSubject(cuid, strbuf, strbuflen)
int cuid, strbuflen;
char *strbuf;
{
    int     bodylen;
    long    offset,
	    bytesunfetched;	/* *** Added for PC 8/20/86 *** */
    char    BodyBuf[MAXBODY], *UsefulStart, *searched, *scr;
    Boolean InHeaders=TRUE;

    offset = 0;
    do {
	UsefulStart=BodyBuf;
	if (CUI_GetPartialBody(BodyBuf, MAXBODY-1, cuid, offset, &bytesunfetched, &bodylen) != 0)
	    break;
	if (bodylen <= 0)
	    break;
	BodyBuf[bodylen] = '\0';
	UsefulStart=BodyBuf;
	while (InHeaders && UsefulStart) { /* Some bodies may not end in \n */
	    searched=index(UsefulStart,'\n');
	    if (searched) *searched++='\0';
	    if (*UsefulStart != ' ' && *UsefulStart != '\t' && !ULstrncmp(UsefulStart,"Subject",7)) {
		    strncpy(strbuf, UsefulStart, strbuflen);
	      return;
	   }
	    UsefulStart=searched;
	    InHeaders=(*searched != '\n');       /* last line of headers */
	} /* End While */
	if (!UsefulStart) break;
	offset += bodylen;
    } while (bytesunfetched > 0);
    moreprintf("Subject: ");
    scr = GetLine();
    if ((long) scr <= 0) {
	moreprintf("EOF\n");
	*strbuf = '\0';
    }
    strncpy(strbuf, scr, strbuflen);
}

CreateNewMessageDirectory(arg)
char   *arg;
{
    int cuid, HasParent = TRUE;
    char   *s, *id, *dir,
	   *ParentDir,
	    ParentBuf[1+MAXPATHLEN],
	   *pdir,
	    ErrorText[256],
	    FullDirName[MAXPATHLEN + 1],
	   *bodydir,
	   *shortname,
	   *InitFile = NULL,
	    InitFileFull[1 + MAXPATHLEN];

    debug(1,("Creating new message directory: %s\n", arg));
    shortname = StripWhiteEnds(arg);
    s = index(shortname, ' ');
    if (s) {
	*s++ = '\0';
	pdir = StripWhiteEnds(s);
	s = index(pdir, ' ');
	if (s) {
	    *s++ = '\0';
	    InitFile = StripWhiteEnds(s);
	}
    }
    else {
	moreprintf("parent folder [top level mail folder] : ");
	pdir = GetLine();
	if ((long) pdir <= 0) {
	    moreprintf("EOF\n");
	    return(-1);
	}
    }
    if (*pdir == '\0') {
	pdir = "~/.MESSAGES";
    }
    if (*shortname == '+' || index(shortname, '.') || index(shortname, '/')) {
	ReportError("Message folder names may not start with a '+' and may not contain '/' or '.'", ERR_WARNING, FALSE);
	return(-1);
    }
    if (CUI_DisambiguateDir(pdir, &ParentDir)) {
 	if (MS_DisambiguateFile(pdir, ParentBuf, FALSE)) {
	    CUI_ReportAmbig(pdir, "folder");
	    return(-1);
	}
	ParentDir = ParentBuf;
	HasParent = FALSE;
	sprintf(ErrorText, "The folder '%s' is top-level, so no folder creation notice will appear.", ParentDir);
	ReportSuccess(ErrorText);
    }
    sprintf(FullDirName, "%s/%s", ParentDir, shortname);
    bodydir = FullDirName;	/* Later we want to make these separable
				 */
    if (CUI_CreateNewMessageDirectory(FullDirName, bodydir))
	return(-1);
    if (HasParent && !InitFile && Interactive && GetBooleanFromUser("Do you want to add a folder creation notice to the parent folder", TRUE)) {
	moreprintf("Enter the name of the file that contains the initial notice body or a message number\n");
	InitFile = GetLine();
	if ((long)InitFile <= 0) {
	    moreprintf("EOF\n");
	    return(-1);
	}
	InitFile = StripWhiteEnds(InitFile);
    }
    if (InitFile) {
	cuid = atoi(InitFile);
	if (cuid <= 0 && (mserrcode = MS_DisambiguateFile(InitFile, InitFileFull, FALSE))) {
	    CUI_ReportAmbig(InitFile, "file");
	    strcpy(ErrorText, "Folder creation worked; no notices posted.");
	    ReportError(ErrorText, ERR_WARNING, FALSE);
	    return(-1);
	}
	if (cuid > 0) {
	    if (GetAMSID(cuid, &id, &dir) != 0) {
		sprintf(ErrorText, "There is no such message number as %d", cuid);
		ReportError(ErrorText, ERR_WARNING, FALSE);
		return(-1);
	    }
	    mserrcode = MS_InstallWelcomeMessage(ParentDir, dir, id, shortname);
	    sprintf(InitFileFull,"message %d",cuid);
	} else {
	    mserrcode = MS_InstallWelcomeMessage(ParentDir, NULL, InitFileFull, shortname);
	}
	if (mserrcode) {
	    if (AMS_ERRNO == EMSNOPARENT) {
		ReportSuccess("Initial notice worked; parental notice failed");
		return(0);
	    }
	    ReportError("Cannot add notification message to parent", ERR_WARNING, TRUE);
	    return(-1);
	}
	sprintf(ErrorText, "Installed initial notice from %s", InitFileFull);
	ReportSuccess(ErrorText);
    }
    return(0);
}

#define PADTOCOLUMNA 10
#define PADTOCOLUMNB 29
#define LOTSASPACE "                                                                "

PrintCaption(cuid, Snapshot, IsDup)
int	cuid, IsDup;
char   *Snapshot;
{
    int     len, padlen;
    char   *s,
	   *t;
    char   *RawCap,
	    CaptionBuffer[250];

    RawCap = AMS_CAPTION(Snapshot);
    sprintf(CaptionBuffer, "%-3d ", cuid);
    if (AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_DELETED)) {
	strcat(CaptionBuffer, "D");
    }
    if (AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_UNSEEN)) {
	strcat(CaptionBuffer, "N");
    }
    if (AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_REPLIEDTO)) {
	strcat(CaptionBuffer, "R");
    }
    if (AMS_GET_ATTRIBUTE(Snapshot, AMS_ATT_URGENT)) {
	strcat(CaptionBuffer, "U");
    }
    if (IsDup) strcat(CaptionBuffer, "2");
    if (ShowChainInfo) {
	char Cbuf[15];
	unsigned long refint;

	bcopy(AMS_CHAIN(Snapshot), &refint, sizeof(unsigned long));
	if (refint) { /* Don't bother showing the zero chains */
	    sprintf(Cbuf, "-%d", ntohl(refint));
	    strcat(CaptionBuffer, Cbuf);
	}
	padlen = 12 - strlen(CaptionBuffer);
    } else {
	padlen = 8 - strlen(CaptionBuffer);
    }
    
    for ( ; padlen>0; --padlen) {
	strcat(CaptionBuffer, " ");
    }
    s = index(RawCap, '\t');
    if (!s || ((s - RawCap) > PADTOCOLUMNA)) {
	strcat(CaptionBuffer, RawCap);
    }
    else {
	*s = '\0';
	strcat(CaptionBuffer, RawCap);
	for (len = strlen(RawCap); len < PADTOCOLUMNA; ++len) {
	    strcat(CaptionBuffer, " ");
	}
	*s++ = '\t';            /*  Restore the buffer for next time    */
	t = index(s, '\t');
	if (!t || ((t - s) > PADTOCOLUMNB)) {
	    strcat(CaptionBuffer, s);
	}
	else {
	    *t = '\0';
	    strcat(CaptionBuffer, s);
	    for (len = strlen(s); len < PADTOCOLUMNB; ++len) {
		strcat(CaptionBuffer, " ");
	    }
	    *t++ = '\t';
	    strcat(CaptionBuffer, t);
	}
    }
    return(moreprintf("%s\n", CaptionBuffer));
}

WhoIs(arg)
char   *arg;
{
    char    *realname;
    int badct;

    CheckPrompted("Please enter one or more names to check")
    moreprintf("Verifying name list '%s'...\n", arg);
    badct = RealWhoIs(arg, &realname);
    moreprintf("\n%s\n", realname);
    if (realname) free(realname);
    if (badct>1) moreprintf("Total of %d errors\n", badct);
    return(0);
}

RealWhoIs(text, newtext)
char   *text,
       **newtext;
{
    text = StripWhiteEnds(text);
    return(CUI_RewriteHeaderLine(text, newtext));
    /* The previous routine reports its own errors */
}


EditFileCmd(arg)
char *arg;
{
     return(EditFile(arg, EDIT_MODE ));
}

EditFile(arg, edittype)
char   *arg;
int edittype;
{
    Boolean FinishedElsewhere;
    char    FileName[MAXPATHLEN + 1], LocalName[MAXPATHLEN + 1];

    debug(1,("EditFile %s, %d\n", arg, edittype));
    if (ParseFileName(arg,FileName,FALSE)) return(-1);
    CUI_GenLocalTmpFileName(LocalName);
    if (LocalName==NULL) return(-1);  /* It reported the error. */
    if (CUI_GetFileFromVice(LocalName, FileName)) return(-1);
    if (EditLocalFile(LocalName, &FinishedElsewhere)) return(-1);
    if (edittype != EDIT_MODE) {
	  unlink(LocalName);
	  return(0);
    }
    CUI_StoreFileToVice(LocalName, FileName);
    if (!FinishedElsewhere) unlink(LocalName);
    return(FinishedElsewhere ? 1 : 0);
}


ClassifyMessage(arg)
char *arg;
{
    return(CloneMessage(arg, MS_CLONE_COPYDEL));
}

AppendMessage(arg)
char *arg;
{
    return(CloneMessage(arg, MS_CLONE_APPEND));
}

CopyMessage(arg)
char *arg;
{
    return(CloneMessage(arg, MS_CLONE_COPY));
}

AppendDelMessage(arg)
char *arg;
{
    return(CloneMessage(arg, MS_CLONE_APPENDDEL));
}

CloneMessage(arg, Code)
char *arg;
int Code;
{
    int cuid;
    char SnapshotBuf[AMS_SNAPSHOTSIZE];

    if ((cuid=ParseMessageNumber(arg))<0) return(-1);
    arg=index(arg,' ');
    if (!arg || !*arg) {
	moreprintf("Please enter a new folder name: ");
	fflush(stdout);
	arg=GetLine();
	if ((long) arg <= 0) {
	    moreprintf("EOF\n");
	    return(-1);
	}
    } else {
	arg++;
    }
    if ((Code == MS_CLONE_COPYDEL) || (Code == MS_CLONE_APPENDDEL)) {
	if (CUI_GetSnapshotFromCUID(cuid, SnapshotBuf)) {
	    return(-1); /* error reported */
	}
	if (! AMS_GET_ATTRIBUTE(SnapshotBuf, AMS_ATT_MAYMODIFY)) {
	    if (Code == MS_CLONE_COPYDEL) {
		Code = MS_CLONE_COPY;
	    } else {
		Code = MS_CLONE_APPEND;
	    }
	}
    }
    return(CUI_CloneMessage(cuid, arg, Code));
 }

DirectoryChangeHook(adddir, deldir, rock)
char *adddir, *deldir, *rock;
{
    debug(1, ("Directory change hook adding %s deleting %s\n", adddir ? adddir : "<NULL>", deldir ? deldir : "<NULL>"));
}

extern int LinesOnTerminal, TerminalLineWidth;

ShowOptSettings(arg)
char *arg;
{
    int i, SaidSomething = 0, len;

    arg = StripWhiteEnds(arg);
    len = strlen(arg);
    for (i=0; Options[i]; ++i) {
	if (!*arg || !strncmp(arg, Options[i], len)) {
	    ++SaidSomething;
	    switch(i) {
		case OPT_DEBUG:
		    moreprintf("Debugging levels: cui %d snap %d malloc %d ms %d snap %d malloc %d\n", CUIDebugging, SNAP_debugmask, MyMallocLevel, MSDebug, MSSnapDebug, MSMalloc);
		    break;
		case OPT_LEVEL:
		    moreprintf("Expertise level currently %s\n", DESCRIBEEXPERTISE);
		    break;
		case OPT_PROMPT:
		    moreprintf("Current prompt is %s\n", Prompt);
		    break;
		case OPT_HEADERS:
		    DescribeHeads();
		    break;
		case OPT_SCRIPT:
		    moreprintf("You are running in %s mode.\n", Interactive ? "interactive" : "script");
		    break;
		case OPT_TERMINAL:
		    moreprintf("Terminal height is %d; width is %d\n", LinesOnTerminal, TerminalLineWidth);
		    break;
		case OPT_BLIND:
		    moreprintf("Blind copies of all outgoing mail will%s be kept.\n", BlindStatus == AMS_SEND_BLINDYES ? "" : " NOT");
		    break;
		case OPT_SEENLAST:
		    --SaidSomething; /* Can not explain this */
		    break;
		case OPT_WHATMEWORRY:
		    moreprintf("Errors on a command line will%s cause other commands on the same line to be ignored\n", WhatMeWorry ? " NOT" : "");
		    break;
		case OPT_EDITOR:
		    moreprintf("The editor that cui will use to edit things is %s\n", EditorBuf[0] == '\0' ? getenv("EDITOR") : EditorBuf);
		    break;
		case OPT_LOGFILE:
		    if (LogFileName) {
			moreprintf("All output is being logged to file %s\n", LogFileName);
		    } else {
			moreprintf("Output from cui is NOT being logged.\n");
		    }
		    break;
		case OPT_SHOWCHAINS:
		    if (ShowChainInfo) {
			moreprintf("Captions should include message chain numbers\n");
		    } else {
			moreprintf("Captions should NOT include message chain numbers\n");
		    }
		    break;
		case OPT_BBDAEMON:
		    if (BBDaemon){
			moreprintf("You are running like a daemon\n");
		    }
		    else {
			moreprintf("You are not running like a daemon\n");
		    }
		    break;
		default:
		    moreprintf("No one has written the 'show' documentation for the '%s' option yet.\n", Options[i]);
		    break;
	    }
	}
    }
    if (!SaidSomething) {
	moreprintf("Sorry; there is no %s option to explain to you.\n", arg);
    }
}

TakeHints(arg)
char *arg;
{
    int DoAll, ProtFailures;

    DoAll = (*arg == 'a' || *arg == 'A') ? 1 : 0;
    mserrcode = MS_TakeHints(DoAll, &ProtFailures);
    if (mserrcode) {
	ReportError("Could not take hints properly", ERR_CRITICAL, TRUE);
    } else if (ProtFailures) {
	char ErrText[150];
	sprintf(ErrText, "There were %d protection failures trying to take hints.", ProtFailures);
	ReportError(ErrText, ERR_CRITICAL, TRUE);
    }
}

FindBodyStartInForwardedViceFile(ViceFile)
char *ViceFile;
{
    char    Buf[MAXBODY], *s;
    int     bodylen;
    long    offset = 0,
	    bytesunfetched;	/* *** Added for PC  8/20/86  *** */
    int InBody = 0, SawBoundary = 0;

    do {
	if ((mserrcode = MS_GetPartialFile(ViceFile, Buf, sizeof(Buf)-1, offset, &bytesunfetched, &bodylen)) != 0) {
	    ReportError("Could not get message from server", ERR_WARNING, TRUE);
	    return(-1);
	}
	if (bodylen <= 0) break;
	Buf[bodylen] = '\0';
	for(s=index(Buf, '\n'); s; s=index(s, '\n')) {
	    if (SawBoundary) {
		if (*(s+1) == '\n') return(offset + (s - Buf) + 2);
	    }
	    if (!InBody && *++s == '\n') {
		InBody = 1;
	    } else if (InBody && !SawBoundary && (*(s+1) == '-') && (*(s+2) == '-')) {
		SawBoundary = 1; /* Just need next newline now */
	    }
	    ++s;
	}
	offset += bodylen -1; /* The -1 is a cheap way to handle if the two newlines wrap calls */
    } while (bytesunfetched > 0);
    ReportError("There seems to be no message there!", ERR_WARNING, FALSE);
    return(0);
}

MatchFolder(arg, aliasct)
char *arg;
int aliasct;
{
    char Fname[1+MAXPATHLEN];

    mserrcode = MS_MatchFolderName(arg, Fname);
    if (mserrcode) {
	ReportError("Could not get folder name matches", ERR_WARNING, TRUE);
    } else {
	moreprintf("The message server found the following matches to '%s':\n\n", arg);
	DisplayFile(Fname);
	MS_UnlinkFile(Fname); /* ignore errors */
    }
}

ReconstructDirectory(arg)
char *arg;
{
    static char   *HowToVec[] = {
	"How do you want to sort the reconstructed folder",
	"By time stamp on the raw files",
	"By parsing the 'Date' header (slower; sets time stamps)",
	0
    };
    int TrustTimeStamp = 0;
    char *s;

    s = index(arg, ' ');
    if (s) {
	*s++ = '\0';
	TrustTimeStamp = (atoi(s) == 1)?1:0;
    } else if (Interactive) {
	TrustTimeStamp = (ChooseFromList(HowToVec, 1) == 1) ? 1 : 0;
    }
    return(CUI_ReconstructDirectory(arg, TrustTimeStamp));
}

Scavenge(arg)
char *arg;
{
    int Recurse = -1, numgood, numbad, Purge = -1;
    char *s, *t, *DirName;

    s = index(arg, ' ');
    while (s) {
	*s++ = '\0';
	t = index(s, ' ');
	if (t) *t = '\0';
	LowerStringInPlace(s, strlen(s));
	if (!strncmp(s, "recurse", 7)) {
	    Recurse = 1;
	} else if (!strncmp(s, "norecurse", 9)) {
	    Recurse = 0;
	} else if (!strncmp(s, "purge", 5)) {
	    Purge = 1;
	} else if (!strncmp(s, "nopurge", 7)) {
	    Purge = 0;
	}
	s = t;
    }
    if (Recurse == -1) Recurse = (GetBooleanFromUser("Recursively scavenge all subdirectories", FALSE)) ? 1 : 0;
    if (Purge == -1) Purge = (GetBooleanFromUser("Purge deletions after scavenging", FALSE)) ? 1 : 0;
    if ((mserrcode = CUI_DisambiguateDir(arg, &DirName)) != 0) {
	CUI_ReportAmbig(arg, "folder");
	return(-1);
    }
    mserrcode = MS_ScavengeDirectory(DirName, Recurse, &numgood, &numbad, Interactive ? 0: 1, Purge);
    if (mserrcode) {
	ReportError("Scavenging operation failed", ERR_WARNING, TRUE);
    } else {
	if (numbad > 0) {
	    moreprintf("Scavenge complete.  However, %d of %d folders scavenged failed.\n", numbad, numbad + numgood);
	} else if (Interactive) {
	    if (numgood > 1) {
		moreprintf("Scavenged %d folders.\n", numgood);
	    } else {
		moreprintf("Scavenged %s.\n", DirName);
	    }
	}
    }
    return(mserrcode);
}

SubscriptionChangeHook() {} /* satisfy the linker */

#ifdef METAMAIL_ENV
nontext(s)
char *s;
{
    char *t;
    if (!s) return(1);
    while (*s && isspace(*s)) ++s;
    for(t=s; *t; ++t) if (isupper(*t)) *t = tolower(*t);
    while (t > s && isspace(*--t)) {;}
    if (((t-s) == 3) && !strncmp(s, "text", 4)) return(0);
    if (strncmp(s, "text/plain", 10)) return(1);
    t = (char *) index(s, ';');
    while (t) {
        ++t;
        while (*t && isspace(*t)) ++t;
        if (!strncmp(t, "charset", 7)) {
            s = (char *) index(t, '=');
            if (s) {
                ++s;
                while (*s && isspace(*s)) ++s;
                if (!strncmp(s, "us-ascii", 8)) return(0);
            }
            return(1);
        }
        t = (char *) index(t, ';');
    }
    return(0); /* no charset, was text/plain */
}
#endif
