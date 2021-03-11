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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/cui/RCS/cuifns.c,v 2.18 1994/03/29 03:56:09 rr2b Exp $";
#endif


 

#include <andrewos.h> /* sys/file.h sys/time.h */
#include <cui.h>
#include <hdrparse.h>
#include <errprntf.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#define CUI_SOURCE_CUIFNS_C
#include <cuimach.h>

extern char *getpass();
extern int     Interactive, CUI_SnapIsRunning, LinesOnTerminal, TerminalLineWidth;

extern struct tm *localtime();

extern char *LogFileName;

char *GetLine();

extern char *StripWhiteEnds();
extern char **unix_sys_errlist,
	   *ms_errlist[],
	   *ms_errcauselist[],
	   *ms_errvialist[],
	   *rpc_errlist[];
extern int  unix_sys_nerr,
	    ms_nerr,
	    ms_nerrcause,
	    ms_nerrvia,
	    rpc_nerr;

ResetTerminalParams (arg)
char *arg;
{
    int i, j;
    char *s;

    arg = StripWhiteEnds(arg);
    s = strchr(arg, ' ');
    if (s) {
	*s++ = '\0';
	j = atoi(s);
    } else {
	j = 80;
    }
    i = atoi(arg);
    if (i<=0) {
	if (strcmp(arg, "0")) {
	    moreprintf("Usage: set terminal <#ht> <#wd>\n");
	    moreprintf("Terminal height is %d; width %d\n", LinesOnTerminal, TerminalLineWidth);
	    return(-1);
	}
	SetTerminalParams(0,j);
	if (Interactive) {
	    moreprintf("The 'more' prompt will not appear.\n");
	}
	return(0);
    }
    SetTerminalParams(i,j);
    if (Interactive) {
	if (i) {
	    moreprintf("Terminal height reset to %d; width %d\n", i, j);
	}
    }
    return(0);
}

GetBooleanFromUser(prompt, DefaultAns)
char   *prompt;
Boolean DefaultAns;
{
    char   *ans;

    debug(1,("GetBooleanFromUser %s (%d)\n", prompt, DefaultAns));
    while (TRUE) {
	moreprintf("%s [%s] ? ", prompt, DefaultAns ? "Yes" : "No");
	fflush(stdout);
	ans = GetLine();
	if ((long) ans <= 0)
	    return(-1);
	ans = StripWhiteEnds(ans);
	if (!*ans)
	    return(DefaultAns);
	if (*ans == 'y' || *ans == 'Y')
	    return(TRUE);
	if (*ans == 'n' || *ans == 'N')
	    return(FALSE);
	moreprintf("Please answer yes or no.\n");
    }
}

GetStringFromUser(prompt, buf, len, IsPassword)
char   *prompt, *buf;
int len, IsPassword;
{
    char   *ans;
    debug(1,("GetStringFromUser %s\n", prompt));
    if (IsPassword) {
	moreprintf("%s", prompt);
	fflush(stdout);
	ans = getpass(" ? ");
    } else {
	moreprintf("%s ? ", prompt);
	fflush(stdout);
	ans = GetLine();
    }
    if (ans==NULL) {
	*buf = '\0';
    } else {
	strncpy(buf, ans, len);
    }
}

ReportError(text, level, Decode)
char   *text;
int	level;
Boolean Decode;
{
    char    ErrorText[500],
	    NumDum[10];
    int     errnum,
	    errcause,
	    errvia,
	    errrpc;
    Boolean MustDie = FALSE, IsViceErr = FALSE;

    debug(1,("ReportError %s (%d, %d)\n", text, level, Decode));

    if (level <= ERR_FATAL) {
	level = ERR_CRITICAL;
	MustDie = TRUE;
    }
    if (mserrcode == 0) Decode = 0;
    if (!Decode) {
	errprintf2("cui", level, NIL, NIL, "%s", text);
	if (MustDie) {errprintf2("cui", level, NIL, NIL, "Program terminated."); exit(-1);}
	return(0);
    }
    errnum = AMS_ERRNO;
    errcause = AMS_ERRCAUSE;
    errvia = AMS_ERRVIA;
    errrpc = AMS_RPCERRNO;
    if (errrpc) {
	sprintf(ErrorText, "%s (AMS RPC error: %s)", text, rpc_errlist[errrpc]);
	errprintf2("cui", level, NIL, NIL, ErrorText);
	if (MustDie) {errprintf2("cui", level, NIL, NIL, "Program terminated."); exit(-1);}
	return(0);
    }

    if (errnum < 0 || errnum >= (EMSBASE + ms_nerr)
	    || (errnum < EMSBASE && errnum > unix_sys_nerr && !vdown(errnum))
       ) {
	errprintf2("cui", ERR_WARNING, NIL, NIL, "errnum %d out of range", errnum);
	errnum = EMSUNKNOWN;
    }
    if (errcause < 0 || errcause >= ms_nerrcause) {
	errprintf2("cui", ERR_WARNING, NIL, NIL, "errcause %d out of range", errcause);
	errcause = EIN_UNKNOWN;
    }
    if (errvia < 0 || errvia >= ms_nerrvia) {
	errprintf2("cui", ERR_WARNING, NIL, NIL, "errvia %d out of range", errvia);
	errvia = EVIA_UNKNOWN;
    }
    if (errnum < EMSBASE) {
	if (vdown(errnum))
	      sprintf(ErrorText, "%s - Connection timed out (in ", text);
	  else {
	    if (unix_sys_errlist[errnum])
	      sprintf(ErrorText, "%s - %s (in ", text, unix_sys_errlist[errnum]);
	     else
	      sprintf(ErrorText, "%s - Unknown error %d (in ", text, errnum);
	}
    }
    else {
	if (ms_errlist[errnum - EMSBASE]) {
	    sprintf(ErrorText, "%s - %s (in ", text, ms_errlist[errnum - EMSBASE]);
	}
	else {
	    sprintf(ErrorText, "%s - Unknown error %d (in ", text, errnum);
	}
    }
    if (ms_errcauselist[errcause]) {
	strcat(ErrorText, ms_errcauselist[errcause]);
    }
    else {
	strcat(ErrorText, "(in unknown call ");
	sprintf(NumDum, "%d", errcause);
	strcat(ErrorText, NumDum);
    }
    strcat(ErrorText, " in ");
    if (ms_errvialist[errvia]) {
	strcat(ErrorText, ms_errvialist[errvia]);
    }
    else {
	strcat(ErrorText, "in unknown caller ");
	sprintf(NumDum, "%d", errvia);
	strcat(ErrorText, NumDum);
    }
    errprintf2("cui", level, NIL, NIL, "%s)", ErrorText);
    if (errnum == EACCES) {
	int Authenticated;

	mserrcode = MS_CheckAuthentication(&Authenticated);
	if (mserrcode) {
	    if (vdown(AMS_ERRNO)) {
		IsViceErr = TRUE;
	    }
	} else if (!Authenticated) {
	    errprintf2("cui", ERR_CRITICAL, NIL, NIL, "Your AFS authentication has apparently expired.");
	}
    }
    if (IsViceErr || vdown(errnum)) {
	errprintf2("cui", ERR_CRITICAL, NIL, NIL, "A file server or the network is down.");
    }
#ifdef PLUMBFDLEAKS
    if (errnum == EMFILE) {
	fdplumb_SpillGuts();
    }
#endif /* PLUMBFDLEAKS */
    if (MustDie) {errprintf2("cui", level, NIL, NIL, "Program terminated."); exit(-1);}
    return(0);
}

errprintf2(s1, i, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)
int i;
char *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9, *s10, *s11, *s12;
{
    errprintf(s1, i, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12);
    if (LogFileName) {
	FILE *fp;

	fp = fopen(LogFileName, "a");
	if (fp) {
	    char *typestr;

	    if (i <= ERR_CRITICAL) {
		typestr = "critical";
	    } else if (i <= ERR_WARNING) {
		typestr = "warning";
	    } else if (i <= ERR_MONITOR) {
		typestr = "monitor";
	    } else {
		typestr = "debug";
	    }
	    fprintf(fp, "<%s:%s> ", s1, typestr);
	    fprintf(fp, s4, s5, s6, s7, s8, s9, s10, s11, s12);
	    PrintTimeStampAndNewline(fp);
	    fclose(fp);
	}
    }
}
    

ReportSuccess(text)
char   *text;
{
    debug(1,("ReportSuccess %s\n", text));
    moreprintf("%s\n", text);

    if (LogFileName) {
	FILE *fp;

	fp = fopen(LogFileName, "a");
	if (fp) {
	    fprintf(fp, "%s", text);
	    PrintTimeStampAndNewline(fp);
	    fclose(fp);
	}
    }

}

PrintTimeStampAndNewline(fp)
FILE *fp;
{
    struct tm *TmBuf;
    long now;
    int hour, ispm = 0;

    now = time(0);
    TmBuf = localtime(&now);
    hour = TmBuf->tm_hour;
    if (hour > 11) {
        ++ispm;
	if (hour > 12) {
	    hour -= 12;
	}
    } else if (hour == 0) {
	hour = 12;
    }
    fprintf(fp, "  (%d:%02d:%02d %cM)\n", hour, TmBuf->tm_min, TmBuf->tm_sec, ispm ? 'P' : 'A');
}    


MoreSelect(Default, AnsBuf, AnsMax, MoreOptions, s, Prompt_string)
int	Default, AnsMax;
char   *AnsBuf, *MoreOptions[], *s, *Prompt_string;

{
    char   *t;
    int     Matches = 0,
	    MatchLen = 0,
	    LastMatch = 0,
	    i = 0;

    debug(1,("MoreSelect %d\n", Default));
    while (TRUE) {
	*AnsBuf = '\0';
	if (!s || !*s) {
	     if (!Interactive) return(-1);
	     moreprintf("\n%s (Type '?' for help) [%s]: ", Prompt_string,
			 (Default>=0)?MoreOptions[Default]:"quit");
	     fflush(stdout);
	     s = GetLine();
	     if (s == (char *) - 1) {
		 clearerr(stdin);
		 return(-2);
	     }
	}
	debug(4,("Processing selection %s\n", s));
	s = StripWhiteEnds(s);
	if (*s == '\0')
	    return(Default);

	for (t = s; *t && *t != ' ' && *t != '\t'; ++t) {
	    if (isupper(*t)) {
		*t = tolower(*t);
	    }
	}
	if (*t) {
	    *t++ = '\0';
	    strncpy(AnsBuf, t, AnsMax);
	}
	if (*s == '?' || !ULstrcmp(s,"help")) {
	    moreprintf("Your options are: help, quit, ");
	    for (i = 0; MoreOptions[i+1]; ++i)
		moreprintf("%s, ", MoreOptions[i]);
	    moreprintf("%s\n", MoreOptions[i]);
	    *s='\0';
	    continue;
	}
	if (*s == 'q' || !ULstrcmp(s,"quit")) {
	    return(-1);
	}
	Matches = 0;
	for (i = 0; MoreOptions[i]; ++i) {
	    if (!strcmp(MoreOptions[i], s)) {
		Matches = 1;
		LastMatch = i;
		break;
	    }
	}
	if (!Matches) { 	/* No exact matches, try partials */
	    MatchLen = strlen(s);
	    for (i = 0; MoreOptions[i]; ++i) {
		if (!strncmp(MoreOptions[i], s, MatchLen)) {
		    ++Matches;
		    LastMatch = i;
		}
	    }
	}
	if (Matches == 1) return(LastMatch);
	if (Matches > 1) {
	    moreprintf("Ambiguous selection: '%s'\nChoose one of: ", s);
	    for (i = 0; MoreOptions[i]; ++i) {
		if (!strncmp(MoreOptions[i], s, MatchLen))
		    moreprintf("%s ", MoreOptions[i]);
	    }
	    moreprintf("\n");
	}
	else
	    if (Matches == 0)
		moreprintf("Unrecognized selection: %s (type 'help' for help)\n", s);
	*s='\0';
    }  /* End While */
}

HandleTimeout(name, retries, restarts)
char   *name;
int	retries,
	restarts;
{
 /* 	if (retries < 2) { *//* In snap 2, retries are not needed */
    if (retries < 0) {
	ReportError("Connection to message server timed out; retrying...", ERR_WARNING, FALSE);
	return(CUI_RPC_RETRY);
    }
    if (restarts < 2) {
	ReportError("Reconnecting to message server; please wait...", ERR_WARNING, FALSE);
	return(CUI_RPC_RESTART);
    }
    return(CUI_RPC_BUGOUT);	/* No message needed here -- will
				   propogate error */
}

DidRestart() {
    ReportSuccess("Reconnected to Message Server!");
}

#define INSERT_FILE_CHAR 2

StorePartialFile(fname, offset_p, mode)
char   *fname;
long	*offset_p;
int	mode;
{
    int     c,
	    pos = 0;
    char    BigBuf[WRITEFILECHUNK];
    Boolean FileOpen = FALSE;
    FILE *fp = NULL;

    while (1) {
	if (FileOpen) c=getc(fp);
	else c = getchar();
	if (c==EOF)
	    if (FileOpen) {
		fclose(fp);
		FileOpen=FALSE;
		moreprintf("File inserted.  Continue entering your message...\n");
		continue;
	    } else break;

	if (c==INSERT_FILE_CHAR) {
	    char *InsertFile, FileName[MAXPATHLEN+1];

	    c=getchar(); /* to flush the CR */
	    moreprintf("Please enter name of file to insert: ");
	    fflush(stdout);
	    InsertFile=GetLine();
	    if (((long) InsertFile < 0)
		|| (ParseFileName(InsertFile, FileName, FALSE))) {
		ReportError("Cannot resolve filename", ERR_WARNING, FALSE);
		moreprintf("Returning to typed input:\n");
		continue;
	    }
	    fp = fopen(FileName, "r");
	    if (fp == NULL) {
		ReportError("Cannot open file", ERR_WARNING, FALSE);
		moreprintf("Returning to typed input:\n");
	    } else {
		FileOpen=TRUE;
	    }
	    continue;
	}

	BigBuf[pos++] = c;
	if (pos >= WRITEFILECHUNK) {
	    mserrcode = MS_StorePartialFile(fname, *offset_p, pos, 0600, TRUE, BigBuf);
	    if (mserrcode) {
		ReportError("Message server cannot store file", ERR_WARNING, TRUE);
		return(-1);
	    }
	    *offset_p += pos;
	    pos = 0;
	}
    }
    if (pos > 0 || *offset_p == 0) {
	mserrcode = MS_StorePartialFile(fname, *offset_p, pos, 0600
, TRUE, BigBuf);
	if (mserrcode) {
	    ReportError("Message server cannot store file", ERR_WARNING, TRUE);
	    return(-1);
	}
	*offset_p += pos;
    }
    clearerr(stdin);		/* To get rid of the EOF condition */
    return(0);
}

ChooseFromList(QVec, def)
char **QVec;
int def;
{
    char *ans;
    int i, myans = 0, numanswers;

    moreprintf("%s\n", QVec[0]);
    for (numanswers=1; QVec[numanswers]; ++numanswers) {
	moreprintf("    %d - %s\n", numanswers, QVec[numanswers]);
    }
    while (myans <= 0 || myans >= numanswers) {
	moreprintf("Choose one [%d - %s]: ", def, QVec[def]);
	ans = GetLine();
	myans = ((long) *ans > 0) ? atoi(ans) : def;
	if (!myans) {
	    int hitcount = 0, lasthit = 0, len;

	    len = strlen(ans);
	    for (i=1; QVec[i]; ++i) {
		if (!ULstrncmp(ans, QVec[i][0] == '\'' ? QVec[i]+1 : QVec[i], len)) {
		    lasthit = i;
		    ++hitcount;
		}
	    }
	    if (hitcount == 1) return(lasthit);
	}
    }
    return(myans);
}

StyleStrip(buffer)
char *buffer;
{}
int ParseMessageNumber(arg)
char *arg;
{
int cuid;
    CheckPrompted("Please enter message number")
    arg = StripWhiteEnds(arg);
    if (*arg == '\0') {
	ReportError("You must supply a message number.", ERR_WARNING, FALSE);
	return(-1);
    }
    cuid = atoi(arg);
    if (cuid == 0 && *arg != '0') {
	char ErrorText[256];
	sprintf(ErrorText, "Illegal message number: %s", arg);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    return cuid;
}

int ParseFileName(arg,FileName,code)
char *arg, *FileName;
int code;
{
    char *next;
    CheckPrompted("Please enter a file name")
    arg = StripWhiteEnds(arg);
    if (*arg == '\0') {
	ReportError("You must supply a file name", ERR_WARNING, FALSE);
	return(-1);
    }
    next=strchr(arg,' ');   /* Allow further arguments */
    if (next) *next='\0';
    if ((mserrcode = MS_DisambiguateFile(arg, FileName, code)) != 0) {
	debug(4,("MS_Disamb returned %s\n",FileName));
	CUI_ReportAmbig(arg, "file");
	return(-1);
    }
    if (next) *next=' ';
    return(0);
}

int ParseDirName(arg,Dirname)
char *arg, **Dirname;
{
    char *next;
    CheckPrompted("Please enter a folder name")
    arg = StripWhiteEnds(arg);
    if (*arg == '\0') {
	ReportError("You must supply a folder name", ERR_WARNING, FALSE);
	return(-1);
    }
    next=strchr(arg,' ');   /* Allow further arguments */
    if (next) *next='\0';
    if (CUI_DisambiguateDir(arg, Dirname)) {
	CUI_ReportAmbig(arg, "folder");
	return(-1);
    }
    if (next) *next=' ';
    return(0);
}

#define ALL 1
#define SOME 0

ListCmd(arg)
char *arg;
{
	 return(ExposeSubscriptions(arg, ALL));
}

SubListCmd(arg)
char *arg;
{
	 return(ExposeSubscriptions(arg, SOME));
}

ExposeSubscriptions(dir_template, code)
char *dir_template;
int code;
{
   char PathElt[MAXPATHLEN+1], MapFile[MAXPATHLEN+1], Buf[MAXBODY], ErrorText[256], *s, *subscr = NULL, *shortname, *longname, *nextline;
   int	i=0, substatus, bodylen;
   long offset, bytesleft, bytesunfetched, spcode;
   debug(1,("Exposing subscription availability list.\n"));
   moreprintf("Getting folder list; please stand by...\n");

   do {
      spcode=MS_GetSearchPathEntry(i, PathElt, MAXPATHLEN);
      ++i;
      if (spcode) break;
      if ((code==ALL) && MORE_NO_MORE == moreprintf("\n<%s>\n",PathElt)) {
	  return(0);
      }
      if ((mserrcode=MS_NameSubscriptionMapFile(PathElt, MapFile))!=0) {
	  if (AMS_ERRNO == ENOENT) continue;  /* User may not have his own
						 message dir		 */
	  sprintf(ErrorText,"MS can not generate subscription map file for %s", PathElt);
	  ReportError(ErrorText, ERR_CRITICAL, TRUE);
	  continue;
      }
      offset=bytesleft=bytesunfetched=0;
      do {
       if ((mserrcode = MS_GetPartialFile(MapFile, Buf + bytesleft, MAXBODY - 1 - (int)bytesleft, offset, &bytesunfetched, &bodylen)) != 0)
	     {
	     sprintf(ErrorText,"Can't open map file %s (for %s)", MapFile, PathElt);
	     ReportError(ErrorText, ERR_CRITICAL, TRUE);
	     break;
	     }
	 if (bodylen <= 0)
	     break;
	 Buf[bodylen + bytesleft] = '\0';/* Make sure null-terminated */
	 for (s=Buf; *s; s=nextline) {
	     nextline=strchr(s,'\n');
	     if (!nextline) {
		    break;
		    }
	     *nextline++ = '\0';
	     debug(4,("Parsing %s\n",s));
	     longname=strchr(s,':');
	     if (longname) *longname++ = '\0';
	       else	    longname=s;
	     shortname=s;
	     s=strchr(longname,' ');
	     if (s) {
		 *s++ = '\0';
		 substatus=atoi(s);
	     } else substatus=AMS_UNSUBSCRIBED;

	     if (strncmp(dir_template,shortname,strlen(dir_template)))
		 continue;

	     switch(substatus) {
	       case AMS_ASKSUBSCRIBED:	  subscr="A"; break;
	       case AMS_ALWAYSSUBSCRIBED: subscr="S"; break;
	       case AMS_UNSUBSCRIBED:	  subscr="N"; break;
	       case AMS_PRINTSUBSCRIBED:  subscr="P"; break;
	       case AMS_SHOWALLSUBSCRIBED:subscr="F"; break;
	     }
	     if ((code==ALL) || (substatus!=AMS_UNSUBSCRIBED)) {
		   if (MORE_NO_MORE == moreprintf("(%s) %s\n",subscr,shortname)) {
		       return(0);
		   }
	     }
	 }  /* End for */
	 strcpy(Buf, s);
	 bytesleft = strlen(s);
	 offset += bodylen;
      } while (bytesunfetched > 0);
      if (bodylen < 0) { /* was <= 0, seemed like a bug on empty paths */
	  if (offset) {
	      sprintf(ErrorText, "The file %s could not be read completely", MapFile);
	  } else {
	      sprintf(ErrorText, "The file %s could not be read", MapFile);
	  }
	  ReportError(ErrorText, ERR_WARNING, TRUE);
      }
      /*      Possible to have leftovers????????       */
   } while (!spcode);
   return(0);
}

MergeDirs(arg)
char *arg;
{
    char *s, *FromDir, *ToDir, ErrorText[256];

    CheckPrompted("Please enter from and to folder names")
    if (ParseDirName(arg,&FromDir)) return(-1);
    s = strchr(arg, ' ');
    if (s) s++;
    if (ParseDirName(s, &ToDir)) return(-1);
    mserrcode = MS_MergeDirectories(FromDir, ToDir);
    if (mserrcode) {
	ReportError("Could not merge folders.", ERR_WARNING, TRUE);
	return(-1);
    } else {
	if (Interactive) {
	    sprintf(ErrorText, "Merged %s into %s.", FromDir, ToDir);
	    ReportSuccess(ErrorText);
	}
	return(0);
    }
}


GetDirInfo(arg)
char *arg;
{
    int ProtCode=0, MsgCount=0, AttrCt;
    char *DirName, IntroFile[1+MAXPATHLEN], ErrorText[256], *arg2, Attrs[1+(AMS_NUM_UATTRS*(1+AMS_ATTRNAMEMAX))];
    while (1) {
	if (ParseDirName(arg,&DirName)) return(-1);
	mserrcode = MS_GetDirInfo(DirName, &ProtCode, &MsgCount);
	if (mserrcode) {
	sprintf(ErrorText, "Cannot get information about folder %s.", DirName);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
	}
	switch(ProtCode) {
	case AMS_DIRPROT_READ:
	    moreprintf("You have read-only rights on private bboard folder %s.\n", DirName);
	    break;
	case AMS_DIRPROT_LOCALBB:
	    moreprintf("You have read-only rights on local bboard folder %s.\n", DirName);
	    break;
	case AMS_DIRPROT_EXTBB:
	    moreprintf("You have read-only rights on external bboard folder %s.\n", DirName);
	    break;
	case AMS_DIRPROT_OFFBB:
	    moreprintf("You have read-only rights on official bboard folder %s.\n", DirName);
	    break;
	case AMS_DIRPROT_MODIFY:
	    moreprintf("You have read and modify rights on folder %s.\n", DirName);
	    break;
	case AMS_DIRPROT_MBOX:
	    moreprintf("You have read, write, and Mailbox-processing rights on folder %s.\n", DirName);
	    break;
	case AMS_DIRPROT_FULLMAIL:
	    moreprintf("Folder %s is one of your private mail folders.\nYou have read, write, and Mailbox-processing rights on it.\n", DirName);
	    break;
	default:
	    moreprintf("You have no rights at all on folder %s.\n", DirName);
	    break;
	 }
	 moreprintf("It has %d messages.\nLooking for explanatory information on folder %s...\n\n", MsgCount, DirName);
	 sprintf(IntroFile, "%s/%s", DirName, AMS_EXPLANATIONFILE);
	 DisplayFile(IntroFile); /* reports its own errors */

	 arg2=strchr(arg,' ');
	 arg=strchr(arg,',');
	 if (arg2 && (!arg || arg2 < arg)) {
		arg = arg2;
	}
	 if (arg && *arg) arg++;
	    else break;
    }
    mserrcode = MS_GetDirAttributes(DirName, &AttrCt, Attrs, '\001', FALSE);
    if (AttrCt > 0) {
	char *s, *t;
	int i;

	moreprintf("There %s %s user-defined flags in this folder: ", (AttrCt > 1) ? "are" : "is", cvEng(AttrCt, 0, 1000));
	for (s=Attrs, i=0; s && i<AttrCt; ++i) {
	    t = strchr(s, '\001');
	    if (t) *t++ = '\0';
	    if ((i+1) == AttrCt) {
		moreprintf("%s'%s'.\n", (AttrCt > 1) ? "and ": "", s);
	    } else {
		moreprintf("'%s', ", s);
	    }
	    s = t;
	}
    } else {
	moreprintf("There are no user-defined attributes in this folder.\n");
    }
    return(0);
}

RmMessageDir(arg)
char *arg;
{
    char *DirName;

    if (ParseDirName(arg,&DirName)) return(-1);
    return(CUI_RemoveDirectory(DirName));
}

UnlinkViceFile(arg)
char *arg;
{
    char FileName[1+MAXPATHLEN], ErrorText[256];

    if (ParseFileName(arg, FileName, AMS_DISAMB_EXISTS)) return(-1);
    mserrcode = MS_UnlinkFile(FileName);
    if (mserrcode) {
	sprintf(ErrorText, "Could not unlink file %s.\n", FileName);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    sprintf(ErrorText, "Unlinked file %s\n", FileName);
    return(0);
}

ReplaceMessage(arg)
char *arg;
{
    int     cuid, Reparse;
    char    ErrorText[256], *id, *dir, FileName[1+MAXPATHLEN], *fname;

    arg = StripWhiteEnds(arg);
    fname = strchr(arg, ' ');
    if (!fname) {
	ReportError("Usage: replace <msg number> <body file name>", ERR_WARNING, FALSE);
	return(-1);
    }
    *fname++ = '\0';
    cuid = atoi(arg);
    if (cuid < 0 || (GetAMSID(cuid, &id, &dir) != 0)) {
	sprintf(ErrorText, "Illegal message number: %s", arg);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    mserrcode = MS_DisambiguateFile(fname, FileName, AMS_DISAMB_EXISTS);
    if (mserrcode) {
	CUI_ReportAmbig(fname, "file");
	return(-1);
    }
    if (GetBooleanFromUser("Do you want the message server to reparse the message body", FALSE)) {
	Reparse = 1;
    } else {
	Reparse = 0;
    }
    mserrcode = MS_EditMessage(dir, id, FileName, Reparse);
    if (mserrcode) {
	sprintf(ErrorText, "Could not replace body of message %d from file %s.", cuid, FileName);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    sprintf(ErrorText, "Replaced body of message %d from file %s.", cuid, FileName);
    return(0);
}


MarkSeenLast(ans)
char *ans;
{
    char *id, *dir, ErrorText[256], SnapshotBuf[AMS_SNAPSHOTSIZE];
    int cuid;

    if ((cuid = ParseMessageNumber(ans))<0) return(-1);
    if (GetAMSID(cuid, &id, &dir) != 0) {
	sprintf(ErrorText, "Illegal message number: %d", cuid);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    if (CUI_GetSnapshotFromCUID(cuid, SnapshotBuf)) {
	return(-1);
    }
    mserrcode = MS_SetAssociatedTime(dir, AMS_DATE(SnapshotBuf));
    if (!mserrcode) {
	mserrcode = MS_FastUpdateState();
    }
    if (mserrcode) {
	ReportError("Could not update your profile", ERR_WARNING, TRUE);

    } else {
	sprintf(ErrorText, "Set message %d as the most recent message read on %s", cuid, dir);
	ReportSuccess(ErrorText);
    }
}

ConvertOldStuff() {
    int good, bad;

    ReportSuccess("Warning:  Converting from ReadMail to Messages/CUI will LOSE all of your old classification information.");
    if (GetBooleanFromUser("Confirm: Do you want to convert all your old Maillib mail to the new format", FALSE)) {
	ReportSuccess("Converting old mail; please wait...");
	mserrcode = MS_ConvertOldMail(&good, &bad);
	if (mserrcode) {
	    ReportError("Could not convert old mail", ERR_WARNING, TRUE);
	    return(-1);

	} else {
	    char ErrorText[256];

	    ReportSuccess("Conversion complete.");
	    if (good) {
		sprintf(ErrorText, "%d old pieces of mail were converted.", good);
		ReportSuccess(ErrorText);
	    }
	    if (bad) {
		sprintf(ErrorText, "%d old pieces of mail could not be converted.", bad);
		ReportSuccess(ErrorText);
	    }
	}
    }
    return(0);
}

RenameDir(arg)
char *arg;
{
    char *new, *old;

    new = strchr(arg, ' ');
    if (!new) {
	ReportError("Command format is: 'rename <oldname> <newname>'", ERR_WARNING, FALSE);
	return(-1);
    }
    *new++ = '\0';
    if (CUI_DisambiguateDir(arg, &old)) {
	CUI_ReportAmbig(arg, "folder");
	return(-1);
    }
    return(CUI_RenameDir(old, new));
}

ResendCmd(arg)
char *arg;
{
    char *name;

    arg = StripWhiteEnds(arg);
    name = strchr(arg, ' ');
    if (name) {
	*name++ = '\0';
	name = StripWhiteEnds(name);
    }
    if (!name || !*name) {
	char *line;
	moreprintf("Resend message to: ");
	fflush(stdout);
	line = GetLine();
	if ((long) line <= 0) {
	    ReportError("EOF", ERR_WARNING, FALSE);
	    return(-1);
	}
	name = StripWhiteEnds(line);
    }
    if (!name || !*name) {
	ReportError("Command format is: 'resend <msg> <name list>'", ERR_WARNING, FALSE);
	return(-1);
    }
    ReportSuccess("Resending message; please wait...");
    return(CUI_ResendMessage(atoi(arg), name));
}

RebuildSubscriptionMaps(arg) 
char *arg;
{
    char FName[1+MAXPATHLEN];

    arg = StripWhiteEnds(arg);
    if (*arg) {
	mserrcode = MS_DisambiguateFile(arg, FName, AMS_DISAMB_ISADIR);
	if (mserrcode) {
	    CUI_ReportAmbig(arg, "tree root");
	    return(-1);
	}
	arg = FName;
	mserrcode = MS_RebuildOneSubscriptionMap(arg);
    } else {
	mserrcode = MS_RebuildSubscriptionMaps();
    }
    if (mserrcode) {
	ReportError("Could not rebuild subscription maps", ERR_WARNING, TRUE);
    }
    else {
	ReportSuccess("Rebuilt Subscription maps");
    }
    return(0);
}

Reindex(arg) 
char *arg;
{
    int slowgood, fastgood, bad, absent, probablygood;
    char FName[1+MAXPATHLEN];

    arg = StripWhiteEnds(arg);
    if (*arg) {
	mserrcode = MS_DisambiguateFile(arg, FName, AMS_DISAMB_ISADIR);
	if (mserrcode) {
	    CUI_ReportAmbig(arg, "tree root");
	    return(-1);
	}
	arg = FName;
	mserrcode = MS_RebuildOneMasterUpdateFile(arg, &fastgood, &slowgood, &bad, &absent, &probablygood);
    } else {
	mserrcode = MS_RebuildMasterUpdateFiles(&fastgood, &slowgood, &bad, &absent, &probablygood);
    }
    if (mserrcode) {
	ReportError("Could not rebuild the master update files", ERR_WARNING, TRUE);
    } else {
	char Message[512];
	int total;

	total = fastgood + slowgood + bad + absent + probablygood;
	if (total > 0) {
	    sprintf(Message, "Rebuilt the master update files for %s: \n\t%d entries were perfect, \n\t%d were correct but slowly validated, \n\t%d needed correction, \n\t%d needed correction but were probably just race conditions, and\n\t%d needed to be added or deleted.\n\n\t%d%% correct overall.", *arg ? arg : "your whole mspath", fastgood, slowgood, bad, probablygood, absent, (100 * (slowgood+probablygood+fastgood)) / total);
	} else {
	    strcpy(Message, "There was nothing to reindex!\n");
	}
	ReportSuccess(Message);
    }
}

WhatsNew(arg)
char *arg;
{
    int changed, unavail, missing, slowpokes, fastguys, newmail;
    char MapFile[1+MAXPATHLEN], Message[256];

    mserrcode = MS_DoIHaveMail(&newmail);
    if (mserrcode) {
	ReportError("Cannot check for new mail", ERR_WARNING, TRUE);
    } else {
	moreprintf("You have %d pieces of mail in your mailbox\n\n", newmail);
    }
    mserrcode = MS_NameChangedMapFile(MapFile, FALSE, GetBooleanFromUser("List Unchanged", FALSE), &changed, &unavail, &missing, &slowpokes, &fastguys);
    if (mserrcode) {
	ReportError("Cannot get changed file", ERR_WARNING, TRUE);
    } else {
	sprintf(Message, "%d changed, %d unavailable, %d missing, %d slowpokes, %d fast checks", changed, unavail, missing, slowpokes, fastguys);
	ReportSuccess(Message);
	if (GetBooleanFromUser("Do you want to see a list of the folders with new messages", TRUE)) {
	    DisplayFile(MapFile);
	}
	if (mserrcode = MS_UnlinkFile(MapFile)) {
	    ReportError("Left the map file on /tmp", ERR_WARNING, TRUE);
	}
    }
}

FlagSomething(arg)
char *arg;
{
    return(MaybeFlagSomething(arg, TRUE));
}

UnflagSomething(arg)
char *arg;
{
    return(MaybeFlagSomething(arg, FALSE));
}

MaybeFlagSomething(arg, DoSet)
char *arg;
Boolean DoSet;
{
    int cuid, code;

    if ((cuid=ParseMessageNumber(arg))<0) return(-1);
    arg=strchr(arg,' ');
    if (!arg || !*arg) {
	moreprintf("Please enter a flag name: ");
	fflush(stdout);
	arg=GetLine();
    } else {
	arg++;
    }
    if ((long) arg <=0) {
	moreprintf("EOF\n");
	return;
    }
    arg = StripWhiteEnds(arg);
    code = CUI_FixAttribute(cuid, arg, DoSet);
    if (!code) {
	char ErrorText[256];
	sprintf(ErrorText, "Message %d is now %sflagged as %s", cuid, DoSet ? "" : "not ", arg);
	ReportSuccess(ErrorText);
    }
}
 
ForgetFlag(arg)
char *arg;
{
    char *dirname, *s, ErrorText[1000];

    arg = StripWhiteEnds(arg);
    s = strchr(arg, ' ');
    if (!s) {
	ReportError("Syntax:  forgetflag <flag> <folder>", ERR_WARNING, FALSE);
	return(-1);
    }
    *s++ = '\0';
    if (CUI_DisambiguateDir(s, &dirname)) {
	CUI_ReportAmbig(s, "folder");
	return(-1);
    }
    mserrcode = MS_DeleteAttr(dirname, arg);
    if (mserrcode) {
	ReportError("Could not delete flag", ERR_WARNING, TRUE);
	return(-1);
    }
    sprintf(ErrorText, "Deleted flag %s from folder %s", arg, dirname);
    ReportSuccess(ErrorText);
    return(0);
}
FlagNotUrgent(arg)
char *arg;
{
    return(FlagUrgency(arg, FALSE));
}

FlagUrgent(arg)
char *arg;
{
    return(FlagUrgency(arg, TRUE));
}

FlagUrgency(arg, urgency) 
char *arg;
int urgency;
{
    int     cuid,
	rc;
    char    ErrorText[256], *arg2;

    debug(1,("FlagUrgency %s\n", arg));
    while (1) {
	 if ((cuid=ParseMessageNumber(arg))<0) return(-1);
	 rc = CUI_FlagUrgency(cuid, urgency);
	 if (rc) return(-1);		/* already reported error */
	 sprintf(ErrorText, "%sarked message %d as 'urgent'", urgency ? "M" : "Unm", cuid);
	 ReportSuccess(ErrorText);
	 arg2=strchr(arg,' ');
	 arg=strchr(arg,',');
	 if (arg2 && (!arg || arg2 < arg)) {
		arg = arg2;
	}
	 if (arg && *arg) arg++;
	    else break;
    }
    return(0);
}

#ifdef M_UNIX

/* get a longer passwd - 
 * doesn't handle signals well
 */
#include <termio.h>

char *
getpass(prompt)
char *prompt;
{
	struct termio tty;
	int flags;
	int got = 1;
	FILE *f = stdin;
	int fd, c;
	static char buf[256];
	int len = sizeof(buf);
	char *ptr = buf;

	fd = fileno(f);
	ioctl(fd, TCGETA, &tty);
	flags = tty.c_lflag;
	tty.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHONL);
	ioctl(fd, TCSETAF, &tty);
	fputs(prompt, stderr);
	while ((c = getc(f)) != '\n' && c != '\r' && c != EOF) {
		if (got++ < len)
			*ptr++ = c;
	}
	*ptr = '\0';
	tty.c_lflag = flags;
	ioctl(fd, TCSETAF, &tty);
	putc('\n',stderr);
	return(buf);
}
#endif
