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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/vuibase.c,v 1.37 1993/07/26 22:59:09 Zarf Exp $";
#endif


 

#include <andrewos.h>
#include <util.h>
#include <ams.h>
#define VUI_SOURCE_VUIBASE_C
#include <vui.h>
#include <panel.h>
#include <lmenus.h>
#include <vuimenus.h>
#include <keycodes.h>
#include <fcntl.h>
#include <vuidebug.h>


#define CUI_RPC_BUGOUT 0
#define CUI_RPC_RESTART 1
#define CUI_RPC_RETRY 2
#define PCM_HELPDIR "doc/inprogram/vui/help"

#define	MINIFILE_ROW	10
#define	BIGFILE_ROW	0
#define	WHOIS_ROW	8

extern char *AndrewDir(), *GetSUser(), *getenv(), *copy();

extern char VUI_editor[], *StripWhiteEnds(), CUI_VersionString[]; 
extern char ENT_filename[], VUI_printer[];
extern char *VUI_HelpFile, VUI_bccto[], sep_line[];

extern long CUI_DisambiguateDir(), CUI_SetSubscriptionEntry();
extern long CUI_GetVersConfigString(), MS_UpdateState();
extern long MS_NameChangedMapFile(), MS_GetSearchPathEntry(); 
extern long MS_NameSubscriptionMapFile(), MS_GetPartialFile(); 
extern long MS_StorePartialFile(), MS_DoIHaveMail(), MS_DisambiguateFile(); 
extern long MS_GetDirInfo(), MS_GetSubscriptionEntry(), MS_ParseDate(); 
extern long MS_UnlinkFile(), MS_GetVersion(), MS_CheckAuthentication(); 

extern int VUI_Mailcount, HeadersOn, BlindStatus, (*VUI_RepaintFn)();
extern int current_option, BODY_field_len;
extern MENU_TREE *current_menu_tree;
extern PANEL sess_bp[], parm_data[];
extern unsigned char bar_row;
extern Boolean HeadKeep;
extern struct head_list *HeadList;
extern char ENT_ReplyFilename[], ENT_headerfile[], RunningOnHost[];
extern long ENT_ReplyOffset, ENT_headlength;
extern int CUI_SnapIsRunning;
extern Boolean VUI_MSConnected;

extern FIELD entry_fields[];

extern MARKED_MSGS *MarkedMsgList;

static char last_folder_name[MAXPATHLEN+1] = { '\0' };

int Interactive = FALSE, HostMayHaveChanged = TRUE;
int CUIDebugging;
FILE *debugfile;

/* The following routine figures out the name of the map file to use for the folder list.  The dir_page_list structure has one element for each screenful of folder names. Because of a myriad of problems the strategy is to have the message server general the list of folders once, then re-use the file.  The objective of this routine is to decide when a new file must be generated, especially in the case of show-all folders when the file can change in the middle of one screen.  In all other cases only one file is required. */

struct dir_file_list {
    char *filename;
    int xentry;
    struct dir_file_list *next;
} *DIR_file_list = (struct dir_file_list *)NIL;

ClearMapFileNames()
{
    struct dir_file_list *dfl, *nxt;
    dfl=DIR_file_list;
    while (dfl != (struct dir_file_list *)NIL) {
	if (!CUIDebugging) RealUnlinkViceFile(dfl->filename);
	free(dfl->filename);
	nxt = dfl->next;
	free(dfl);
	dfl = nxt;
    }
    DIR_file_list = (struct dir_file_list *)NIL;
}

CacheMapFileName(aentry, name)
int aentry;
char *name;
{
    struct dir_file_list *dfl;
    debug((1,"Caching %s for entry %d\n", name, aentry));
    dfl=(struct dir_file_list *)malloc(sizeof(struct dir_file_list));
    if (dfl == (struct dir_file_list *)NIL) return(-1);
    dfl->filename = (char *)malloc(1+strlen(name));
    if (dfl->filename == NIL) return(-1);
    strcpy(dfl->filename, name);
    dfl->next = DIR_file_list;
    dfl->xentry = aentry;
    DIR_file_list = dfl;
    return(0);
}

GetMapFileName(aentry, name)
int aentry;
char *name;
{
    struct dir_file_list *dfl;
    debug((1,"Looking for name of entry %d...", aentry));
    dfl=DIR_file_list;
    *name = '\0';
    while (dfl != (struct dir_file_list *)NIL) {
	if (aentry == dfl->xentry) {
	    strcpy(name, dfl->filename);
	    debug((1,"its %s\n", name));
	    return;
	}
	dfl = dfl->next;
    }
    debug((1,"not found.\n"));
}

GenMapFileName(suborall, mailorbb, MapFile, pathentry_p)
int suborall, mailorbb, *pathentry_p;
char *MapFile;
{
    char PathElt[MAXPATHLEN+1], ErrorText[256];
    long spcode;

    GetMapFileName(*pathentry_p, MapFile);
    if (*MapFile) return(0);   /* If we got one, return it */
    
    if (suborall == CHANGED) {
	int n_ch, n_un, n_miss, n_slow, n_fast;
	debug((1,"In this context FALSE = %d\n",FALSE));
	ReportSuccess("Checking subscriptions for new messages. Please stand by...");
	if ((mserrcode=MS_NameChangedMapFile(MapFile, (int)FALSE, (int)FALSE, &n_ch, &n_un, &n_miss, &n_slow, &n_fast))!=0) {
	    ReportError("MS cannot generate list of changed folders", ERR_WARNING, TRUE);
	    return(-1);
	}
	if (n_ch == 0) {
	    ReportSuccess("None of your subscribed bboards have new messages.");
	    *MapFile = '\0';
	    return(0);
	}
	CacheMapFileName(*pathentry_p, MapFile);
	sprintf(ErrorText, "You have %d folders with new messages.", n_ch);
	ReportSuccess(ErrorText);
	return(0);
    }

    while (1) {
	if (suborall==ALL) {
	    spcode=MS_GetSearchPathEntry(*pathentry_p, PathElt, MAXPATHLEN);
	    debug((2,"Entry %d is in %s\n", *pathentry_p, PathElt));
	    if (spcode || !*PathElt) { /* no more path entries, so we're done */
		*MapFile = '\0';
		return(0);
	    }
	    if (mailorbb==BBOARD) {  /* skip mailpath if doing bboards */
		spcode=MS_GetSearchPathEntry(AMS_MAILPATH, MapFile, MAXPATHLEN);
		if (spcode) return(-1);
		if (!strcmp(PathElt, MapFile)) {
		    (*pathentry_p)++;
		    continue;
		}
	    }
	} else *PathElt = '\0';

      /* ** What does this return if you are not subscribed to anything?? ** */
	if ((mserrcode=MS_NameSubscriptionMapFile(PathElt, MapFile))!=0) {
	    if (AMS_ERRNO != ENOENT) {/* User may not have his own message dir */
		sprintf(ErrorText, "MS can not generate subscription map file for %s", PathElt);
		ReportError(ErrorText, ERR_WARNING, TRUE);
	    }
	    if (suborall == ALL) {
		(*pathentry_p)++;
		continue;
	    }
	}
	break;
    }
    CacheMapFileName(*pathentry_p, MapFile);
    return(0);
}

GetLongNameFromSubsMap(searchname, foundname, currentpage)
char *searchname, *foundname;
struct dir_page_list *currentpage;
{
    GetOrChangeSubsMapEntry(searchname,foundname,0,currentpage);
}


ChangeSubscriptionMapEntry (searchname, newstatus, currentpage)
char *searchname;
int newstatus;
struct dir_page_list *currentpage;
{
    GetOrChangeSubsMapEntry(searchname,NIL,newstatus,currentpage);
}

GetOrChangeSubsMapEntry (searchname,foundname, newstatus, currentpage)
char *searchname,*foundname;
int newstatus;
struct dir_page_list *currentpage;
{
    char ErrorText[256], FileBuf[MAXBODY], OurFileName[MAXPATHLEN+1];
    char *s, *shortname, *longname, *nextline;
    int  bodylen, increment, entryx;
    long offset, bytesunfetched;

    entryx = currentpage->path_entry;
    offset = currentpage->map_offset;
    bytesunfetched = 0;
    while (1) {
	GetMapFileName(entryx, OurFileName);
	if (!*OurFileName) break;
	do {
	    if ((mserrcode = MS_GetPartialFile(OurFileName, FileBuf, MAXBODY - 1, offset, &bytesunfetched, &bodylen)) != 0)  {
		ReportError(VUIMSG_MAPFILE, ERR_WARNING, TRUE);
		return(-1);
	    }
	    debug((2,"We opened the file and read %d bytes starting at %ld.\n", bodylen, offset));
	    if (bodylen <= 0)       /* We hit the end of this file so try next */
		break;
	    FileBuf[bodylen] = '\0';  /* Make sure null-terminated */
	    for (s=FileBuf; *s; s=nextline) {
		nextline=strchr(s,'\n');
		if (nextline==NIL) break;
		*nextline++ = '\0';
		increment = nextline - s;       /* Move pointer of where we've been */
		debug((9,"Parsing %s\n",s));
		longname=strchr(s,':');
		if (longname!=NIL) *longname++ = '\0';
		else              longname=s;
		shortname=s;
		if (!strcmp(shortname, searchname)) {
		    s=strchr(longname,' ');
		    if (foundname != NIL) { /* This is a getlongname request */
			*s = '\0';
			strcpy(foundname,longname);
			debug((2,"The longname for %s is %s\n",shortname,longname));
		    } else { /* this is a change sub entry */
			if (s!=NIL) s++;
			sprintf(FileBuf, "%1d", newstatus);
			offset += increment - (nextline - s);
			mserrcode=MS_StorePartialFile(OurFileName, offset, strlen(FileBuf),
						      0644, FALSE, FileBuf);
		    }
		    return(0);
		}
		offset += increment;
	    }  /* End for */
	} while (bytesunfetched>0);
	if (bodylen <= 0) {
	    sprintf(ErrorText, "%s %s",VUIMSG_READFILE, OurFileName);
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	}
	offset=0;
	entryx++;
    }
    if (foundname != NIL) /* Couldn't find longname */
	*foundname = '\0';
}

PopQuiz(msg)
char *msg;
{
    char str_out[2];
    if (VUI_RepaintFn == (int(*)()) NULL) {
	ConfirmUser(msg);
	return;
    }
    DrawBorderTitle("Press enter to continue",MINIFILE_ROW);
    CursorOff();
    ShowString(msg, 13, 2, strlen(msg), NORMAL);
    GetFromUser(13, strlen(msg), 0, NIL, FTYPE_SEL, str_out);
    (*VUI_RepaintFn)();
    ShowBar();
    CursorOn();
}

HelpKey (curfield, maxopts)
int curfield, maxopts;
{
    char filename[MAXPATHLEN + 1];

    if ((int(*)()) NULL == VUI_RepaintFn || current_option < 0 ||
	    (MENU_TREE *) NIL == current_menu_tree) {
	ReportError(VUIMSG_NOHELP, ERR_WARNING, FALSE);
    } else {
	sprintf(filename, "%s%s/pcm%d.hlp", AndrewDir("/"), PCM_HELPDIR,
		current_menu_tree[current_option].this);
	DisplayHelpFile(filename,TRUE);
    }
    return(REDRAW_KEY_HIT);
}

int DisplayHelpFile(filename,redraw)
char *filename;
int redraw;
{
    char insertbuf[80];
#ifdef IBMPC
    sprintf(insertbuf,
	     "VUI Help (Press %s to leave, %s to print, %s or %s to page up/down)",
	     ESC_STRING, F2_STRING, PGUP_STRING, PGDN_STRING);
#else /* IBMPC */
    sprintf(insertbuf,
	     "VUI Help (Press %s to leave, %s or %s to page up/down)",
	     ESC_STRING, PGUP_STRING, PGDN_STRING);
#endif /*IBMPC */
    return ShowMiniFile(filename, insertbuf,0L,redraw,MINIFILE_ROW);
}

FIELD *HelpKey2 (curfield)
FIELD *curfield;
{
    int row,col;
    char filename[MAXPATHLEN+1];
    FIELD *savefield;

    savefield=GetCurrentField();
    if (VUI_HelpFile != NIL){
        sprintf(filename,"%s%s/%s", AndrewDir("/"), PCM_HELPDIR, VUI_HelpFile);
    }
     else if (current_option<0 || current_menu_tree==(MENU_TREE *)NIL) {
        ReportError(VUIMSG_NOHELP, ERR_WARNING, FALSE);
        return(curfield);
    } else
        sprintf(filename,"%s/vui%d.hlp", PCM_HELPDIR,
                               current_menu_tree[current_option].this);
    GetCursorPosition(&row,&col);
    DisplayHelpFile(filename,(savefield==&entry_fields[3])?FALSE:TRUE);
    debug((1,"About to set current field to %ld\n", savefield));
    SetCurrentField(savefield, PANEL_NOPROMPT);
    if (curfield != (FIELD *)NULL)
	PositionCursor(row,col);
    return(curfield);
}

FIELD *ShowReplyMsg(curfield)
FIELD *curfield;
{
    int row,col;
    char  insertbuf[80];
    FIELD *savefield;

    if (*ENT_ReplyFilename) {
	savefield=GetCurrentField();
	GetCursorPosition(&row,&col);
	sprintf(insertbuf,
		"Original Message (Press %s to leave, %s or %s to page up/down)",
		ESC_STRING,PGUP_STRING,PGDN_STRING);
	ShowMiniFile(ENT_ReplyFilename, insertbuf,ENT_ReplyOffset,
		     (savefield==&entry_fields[3])?FALSE:TRUE,MINIFILE_ROW);
	SaveFileOffset(&ENT_ReplyOffset);
	debug((1,"About to set current field to %ld\n", savefield));
	SetCurrentField(savefield, PANEL_NOPROMPT);
	PositionCursor(row,col);
    }
    else ErrorBeep();
    return(curfield);
}

FIELD *PrintKey2 (curfield)
FIELD *curfield;
{
    char filename[MAXPATHLEN+1];
    int rc;
    if (VUI_HelpFile != NIL)
        sprintf(filename,"%s%s/%s", AndrewDir("/"), PCM_HELPDIR, VUI_HelpFile);
     else
	sprintf(filename, "%s%s/pcm%d.hlp", AndrewDir("/"), PCM_HELPDIR, current_menu_tree[current_option].this);
    rc=PrintFileDataLocal(filename);
    ClearPrompt();
    sprintf(filename,"File%s printed. Press %s to leave", (rc)?" not":"", ESC_STRING);
    ReportSuccess(filename);
    return(curfield);
}

int DrawBorderTitle(msg,row)
char *msg;
int   row;
{
    char strbuf[81];
    int i, len;

    /* +===============| title |================+ */

    strcpy(strbuf,sep_line);
    strbuf[0] = '\311'; strbuf[79] = '\273';
    if ((len = strlen(msg)) > 72) len = 72;
    i = (80 - len) / 2;
    strncpy(strbuf+i-2,"\265  ",2);
    strncpy(strbuf+i+len," \306 ",2);
    ShowString(strbuf, row, 0, 80, HILITE);
    ShowString(msg, row, i, len, RVIDEO);

    for (i=row+1;i<bar_row;i++) {
	ClearLine(i);
	ShowString("\272", i, 0, 1, HILITE);
	ShowString("\272", i, 79, 1, HILITE);
    }
    ShowString("\310", bar_row, 0, 1, HILITE);
    ShowString("\274", bar_row, 79, 1, HILITE);
    ShowString(sep_line, bar_row, 1,78, HILITE);
    return 0;
}

ShowMiniFile(filename, msg,offset,redraw,row)
char *filename, *msg;
long offset;
int redraw,row;
{
    DrawBorderTitle(msg,row);
    CursorOff();
    RestoreCurrentField(NIL);
    DisplayMiniFile(filename,offset,row+1);
    if ((redraw) && (VUI_RepaintFn!=(int (*)())NULL)) {
	(*VUI_RepaintFn)();
    } 
    ShowBar();
    CursorOn();
}

int
VUI_Help()
{
    VUI_RepaintFn = (int (*)())NULL;
    VUI_HelpFile = "mainpanel.hlp";
    HelpKey2((FIELD *)NULL);
    return(MENU_DO_OVER);
}

int
VUIshowMOTD()
{
    FIELD *savefield;
    char   MOTD_filename[MAXPATHLEN],insertbuf[80];

    savefield = GetCurrentField();
    if (((mserrcode=CUI_GetVersConfigString("motd",MOTD_filename)) != 0) ||
	 (CheckViceFileEmpty(MOTD_filename)))
	return (-1);

    sprintf(insertbuf,
	     "Note of the Day (Press %s to leave, %s or %s to page up/down)",
	     ESC_STRING,PGUP_STRING,PGDN_STRING);
    ShowMiniFile(MOTD_filename, insertbuf,0L,TRUE,BIGFILE_ROW);

    SetCurrentField(savefield,PANEL_NOPROMPT);
    return(0);
}

#ifdef DEBUG
DebugKey (curfield, maxopts)
int curfield, maxopts;
{
    if (CUIDebugging==0) {
       debugfile= (FILE *) fopen("vui.bug","a");
       CUIDebugging= -1;
       debug((1,"This file should be open now.\n"));
       ErrorBeep();
       ErrorBeep();
    } else {
       fclose(debugfile);
       CUIDebugging=0;
       ErrorBeep();
    }
    return(curfield);
}
#endif

#ifdef DEBUG
FIELD *DebugKey2 (curfield)
FIELD *curfield;
{
    if (CUIDebugging==0) {
       debugfile= (FILE *) fopen("vui.bug","a");
       CUIDebugging= -1;
       debug((1,"This file should be open now.\n"));
       ErrorBeep();
       ErrorBeep();
    } else {
       fclose(debugfile);
       CUIDebugging=0;
       ErrorBeep();
    }
    return(curfield);
}
#endif

static char **CFL_QVec;
static int CFL_def;

ShowtheList ()
{
    char textbuf[81];
    int i, row;

    DrawBorderTitle("Multiple Choice Selection",WHOIS_ROW);
    ShowString(CFL_QVec[0],  9, 1, 78, NORMAL);
    for (row=10, i=1; row<bar_row-1; row++) {
        if (CFL_QVec[i]) {
            sprintf(textbuf," %d - ", i);
	    strncat(textbuf,CFL_QVec[i],(78-strlen(textbuf)-1));
	    textbuf[78] = '\0';
            i++;
            ShowString(textbuf, row, 1, 78, NORMAL);
        } else
            ShowString("", row, 1, 78, NORMAL);
    }
    sprintf(textbuf, "Enter a number from 1 to %d [%d]: ", i-1, CFL_def);
    ShowString(textbuf, bar_row-1, 1, 78, NORMAL);
    ShowCursor();
    return(strlen(textbuf));
}

ChooseFromList (QVec, def)
char **QVec;
int def;
{
    char *savehelpfile, **listp;
    int myans = 0, col, (*saverepaintfn)(), listlen = 0;

    saverepaintfn = VUI_RepaintFn;
    savehelpfile = VUI_HelpFile;
    VUI_RepaintFn = ShowtheList;
    VUI_HelpFile = "multchoice.hlp";
    CFL_QVec = QVec;
    CFL_def = def;
    col=ShowtheList();
    for (listp = QVec+1; *listp != NULL; listp++) listlen++;
    if (0>GetNUser(bar_row-1, col+1, 2, 1, listlen, &myans)) myans=def;
    VUI_RepaintFn = saverepaintfn;
    VUI_HelpFile = savehelpfile;
    (*VUI_RepaintFn)();
    ShowBar();
    return(myans);
}

DisplayCriticalError(msg)
char *msg;
{
    int row, rc, msglen, whatsleft;
    char *msgptr;

    msglen = strlen(msg);

    if (msglen > 72) {
	DrawBorderTitle("Critical Error", MINIFILE_ROW);
	
	msgptr = msg;
	for (row = MINIFILE_ROW+2; row < bar_row-1; row++) {
	    whatsleft = strlen(msgptr);
	    ShowString(msgptr, row, 1, (whatsleft > 78) ? 78 : whatsleft, NORMAL);
	    msgptr += 78;
	    if (msgptr-msg >= msglen) break;
	}
    }
    rc = GetBooleanFromUser("There has been a CRITICAL error; should I quit",TRUE);
	
    if (msglen > 72) (*VUI_RepaintFn)();
	
    return(rc);
}

CheckForMail()
{
int mc;
char msgbuf[80];

    if (!VUI_MSConnected) /* If we have tempararily lost our connection */
	return(0);

    if (mserrcode = MS_DoIHaveMail(&mc)) {
	ReportError(VUIMSG_MBCHECK, ERR_WARNING, TRUE);
	return(0);
    }
    if (mc == VUI_Mailcount) return(0);
    VUI_Mailcount = mc;
    sprintf(msgbuf, "You have %d item%s in your mailbox.", mc, (mc==1)?"":"s");
    PopQuiz(msgbuf);
    return(1);
}

CheckNewMessages (verbose)
int verbose;
{
    char DirName[2];
    int save_interactive, rc, mc;

    *DirName='\0';
    save_interactive = Interactive;
    Interactive = verbose;
    rc = CUI_CheckNewMessages(DirName);
    if (mserrcode=MS_DoIHaveMail(&mc)) {
	ReportError(VUIMSG_MBCHECK, ERR_WARNING, TRUE);
    } else VUI_Mailcount = mc;
    Interactive = save_interactive;
    return(rc);

}

CreateNewMessageDirectory (arg)
char *arg;
{
    char argbuf[81];
    char   *s, *s2, *FullName, *ShortName,
            NewName[1+MAXPATHLEN], NewFullName[1+MAXPATHLEN], ErrorText[256];

    if (arg == NULL) {
	arg=GetSUser("Enter new message folder:", NIL, NIL, argbuf);
	if (arg==NIL) return(-1);
    }
    debug((1,"Creating new message folder: %s\n", arg));
    ShortName = StripWhiteEnds(arg);

    s = strrchr(ShortName, '.');
    s2 = strrchr(ShortName, '/');
    if (s2 > s) s = s2;
    if (s) *s++ = '\0';

    if (s == NIL) {
        mserrcode = MS_GetSearchPathEntry(AMS_MAILPATH, NewName, MAXPATHLEN);
        if (mserrcode) {
            ReportError("Cannot look up the name of your mail path", ERR_WARNING, TRUE);
            return(-1);
        }
        FullName = NewName;
        s = ShortName;
    } else {
         if (mserrcode=CUI_DisambiguateDir(ShortName, &FullName)) {
            sprintf(ErrorText, "The folder '%s' can not be found.", ShortName);
            ReportError(ErrorText, ERR_WARNING, TRUE);
            return(-1);
         }
    }
    sprintf(NewFullName, "%s/%s", FullName, s);
    return(CUI_CreateNewMessageDirectory(NewFullName, NewFullName));
}

PrintNewMessages (shortname)
char *shortname;
{
    char *fullname;
    debug((1,"PrintNewMessages(%s)\n",shortname));
    if ((mserrcode = CUI_DisambiguateDir(shortname, &fullname)) != 0) {
        ReportError(VUIMSG_FCHECK, ERR_WARNING, TRUE);
        return(-1);
    }
    return(CUI_PrintUpdates(fullname, shortname));
}

RmMessageDirectory (current_dir)
char *current_dir;
{
    char *DirName, ErrorText[256];

    if (!strcmp(current_dir, AMS_DEFAULTMAILDIR)){
	ReportError(VUIMSG_MFOLDER, ERR_WARNING, FALSE);
	return(-1);
    }
    sprintf(ErrorText,"Are you sure you want to delete folder %.50s?",
                      current_dir);
    if (GetBooleanFromUser(ErrorText, FALSE)) {
        if ((mserrcode = CUI_DisambiguateDir(current_dir, &DirName)) != 0) {
            ReportError(VUIMSG_FCHECK, ERR_WARNING, TRUE);
            return(-1);
        }
        return(CUI_RemoveDirectory(DirName));
     } else {
        ReportSuccess("Folder not deleted.");
        return(-1);
     }
}

RenameDirectory (current_dir)
char *current_dir;
{
    char *newname, namebuf[81];
    if (!strcmp(current_dir, AMS_DEFAULTMAILDIR)){
	ReportError(VUIMSG_MFOLDER, ERR_WARNING, FALSE);
	return(-1);
    }
    newname=GetSUser("Enter new folder name:", NIL, NIL, namebuf);
    if (newname!=NIL) {
	return(CUI_RenameDir(current_dir, newname));
    }
    ReportSuccess("Folder not renamed.");
    return(-1);
}

MergeDirectories (current_dir)
char *current_dir;
{
    char *newname, ErrorText[256], *FromDir, *ToDir, ToD[MAXPATHLEN+1],holddirname[256];
    if (!strcmp(current_dir, AMS_DEFAULTMAILDIR)){
	ReportError(VUIMSG_MFOLDER, ERR_WARNING, FALSE);
	return(-1);
    }
    strcpy(holddirname, current_dir);
    if ((mserrcode = CUI_DisambiguateDir(current_dir, &FromDir)) != 0) {
        ReportError(VUIMSG_FCHECK, ERR_WARNING, TRUE);
        return(-1);
    }
    newname=GetSUser("Enter name of receiving folder:", NIL,
                     last_folder_name, last_folder_name);
    if (newname==NIL) return(0);
    ToDir=ToD;
    if (mserrcode=CUI_DisambiguateDir(last_folder_name, &ToDir)) {
        sprintf(ErrorText,
                "The folder '%s' does not exist. Please create it first.",
                last_folder_name);
        ReportError(ErrorText, ERR_WARNING, TRUE);
        return(-1);
    }
    mserrcode = CUI_MergeDirectories(FromDir, ToDir);
    if (mserrcode) {
        ReportError("Could not merge folders.", ERR_WARNING, TRUE);
        return(-1);
    } else {
        sprintf(ErrorText, "Merged %s into %s.", holddirname, last_folder_name);
        ReportSuccess(ErrorText);
        return(0);
    }
}

GetDirInfo (current_dir, longinfo)
char *current_dir;
int longinfo;
{
    int ProtCode=0, MsgCount=0;
    char *DirName, ErrorText[256], *msg, explainfile[MAXPATHLEN+1];
    debug((1,"GettingDirInfo for %s\n",current_dir));
    if ((mserrcode = CUI_DisambiguateDir(current_dir, &DirName)) != 0) {
        ReportError(VUIMSG_FCHECK, ERR_WARNING, TRUE);
        return(-1);
    }
    if (longinfo) {
	FIELD *savefield;
	char  insertbuf[80];
        sprintf(ErrorText,"%s/%s", DirName, AMS_EXPLANATIONFILE);
        mserrcode=MS_DisambiguateFile(ErrorText, explainfile, AMS_DISAMB_EXISTS);
        if (mserrcode) {
            ReportError("There is no information file available", ERR_WARNING, TRUE);
            return(-1);
        }
	savefield = GetCurrentField();
	sprintf(insertbuf, "Folder Information (Press %s to leave)",ESC_STRING);
	ShowMiniFile(ErrorText, insertbuf,0L,TRUE,MINIFILE_ROW);
	SetCurrentField(savefield,PANEL_NOPROMPT);
        return(0);
    }
    mserrcode = MS_GetDirInfo(DirName, &ProtCode, &MsgCount);
    if (mserrcode) {
       sprintf(ErrorText, "Cannot get information about folder %s.", DirName);
       ReportError(ErrorText, ERR_WARNING, TRUE);
       return(-1);
    }
    switch(ProtCode) {
    case AMS_DIRPROT_READ:
        msg="A private bboard";
        break;
    case AMS_DIRPROT_LOCALBB:
        msg="A local bboard";
        break;
    case AMS_DIRPROT_EXTBB:
        msg="An external bboard";
        break;
    case AMS_DIRPROT_OFFBB:
        msg="An official bboard";
        break;
    case AMS_DIRPROT_MODIFY:
        msg="A read and modify folder";
        break;
    case AMS_DIRPROT_MBOX:
        msg="A read, write, and Mailbox-processing folder";
        break;
    case AMS_DIRPROT_FULLMAIL:
        msg="One of your private mail folders";
        break;
    default:
        msg="An inaccessible folder";
        break;
    }
    sprintf(ErrorText,"%s with %d messages", msg, MsgCount);
    debug((2,"%s with %d messages", msg, MsgCount));
    ReportSuccess(ErrorText);
    return(-1);
}

GracefulExit (end_conv)
int end_conv;
{
    debug((1,"GracefulExit\n"));
    if (end_conv)
	CUI_EndConversation();
    else
	MS_UpdateState();
}

AlterSubscription (current_dir, newstatus)
char *current_dir;
int newstatus;
{
    char tempname[81], NickName[MAXPATHLEN+1], *nameptr;
    int status;
    debug((1,"Altering subscription for %s to %d\n",current_dir, newstatus));
    if ((mserrcode = MS_GetSubscriptionEntry(current_dir, tempname, &status)) != 0) {
        ReportError(VUIMSG_SUBENTRY, ERR_WARNING, TRUE);
        return(-1);
    }
    debug((2,"Ready to set Full=%s, temp=%s\n",current_dir, tempname));
    if (*tempname) strcpy(NickName, tempname);
      else CUI_BuildNickName(current_dir, NickName);

    nameptr = NickName;
    if (status != newstatus &&
        (mserrcode = CUI_SetSubscriptionEntry(current_dir, nameptr, newstatus)) != 0){
        ReportError(VUIMSG_SUBENTRY, ERR_WARNING, TRUE);
        return(-1);
    }
    return(0);
}

StoreMessage (cuid, opt, headerson)
int cuid, opt, headerson;
{
  int     bodylen, bytesleft;
  FILE   *fd;
  long    offset, offset_in, bytesunfetched;
  char    BodyBuf[MAXBODY+1], *fname, *UsefulStart, *searched = NULL,
  FullName[MAXPATHLEN+1], fnamebuf[81];
  Boolean InHeaders = TRUE, LineGood = FALSE;

    debug((1,"StoreMessage(%d, %d, %d)\n",cuid, opt, headerson));
    fd = 0;
    if (opt==(int)OPT_D_LOCAL || opt==(int)OPT_D_REMOTE) {
        fname=GetSUser("Enter name of file to store message:", NIL, NIL, fnamebuf);
        if (fname==NIL) return(-1);
        if (opt!=(int)OPT_D_LOCAL) {
            if (MS_DisambiguateFile(fname, FullName, AMS_DISAMB_FILEMAYBECREATED)){
                ReportError("Cannot create file by that name.", ERR_WARNING, FALSE);
		return(-1);
            } else fname = FullName;
        }
        if (opt==(int)OPT_D_LOCAL) {
            debug((2,"Storing message %d in file %s\n", cuid, fname));
            fd = fopen(fname, "wt");   /*  "wt"-> write/translated */
            if (fd == (FILE *)NIL) {
                ReportError("Cannot create local file", ERR_WARNING, FALSE);
		return(-1);
            }
            ReportSuccess("Storing...");
        }
    } else {
#ifdef IBMPC
        if (!GetBooleanFromUser("Ready to print on your PC printer; OK?",
                                TRUE)) return(-1);
	if (!PrinterAvailable()) {
	    ReportError("Unable to locate your local printer.",ERR_WARNING,FALSE);
	    return(-1);
	}
        setmode(fileno(stdprn), O_TEXT);  /* change to text on the printer */
        ReportSuccess("Printing...");
        fd=stdprn;
#else
	return(-1);
#endif
    }
    bytesleft=0;
    offset_in=offset=0L;
    UsefulStart = NIL;
    do {
        debug((2,"Getting body for %d at offset %ld\n",cuid, offset_in));
        if (CUI_GetPartialBody(BodyBuf+bytesleft, MAXBODY-bytesleft, cuid,
                               offset_in, &bytesunfetched, &bodylen) != 0) {
	    return(-1);
        }
        debug((2,"Read %d bytes at %ld with %ld to go.\n",
                bodylen, offset_in, bytesunfetched));
        if (bodylen <= 0)
            break;
        offset_in += bodylen;
        UsefulStart=BodyBuf;
        BodyBuf[bodylen+bytesleft] = '\0';
        bytesleft=0;
        while (InHeaders && headerson && UsefulStart!=NIL) {
            searched=strchr(UsefulStart,'\n');
            if (searched!=NIL) *searched++='\0';
              else if (bytesunfetched>0) {
                 strcpy(BodyBuf, UsefulStart);
                 bytesleft=strlen(BodyBuf);
                 break;
            }
            /* Header continues */
            if (*UsefulStart == ' ' || *UsefulStart == '\t') {
                if (LineGood) {
                    OutputLine(fname, &offset, UsefulStart, opt, fd, TRUE);
                }
            } else {
                if (CheckHead(UsefulStart)) {
                    OutputLine(fname, &offset, UsefulStart, opt, fd, TRUE);
                    LineGood=TRUE;
                } else LineGood=FALSE;
            }
            UsefulStart=searched;
            if (searched!=NIL) InHeaders=(*searched != '\n');
        }
        if (bytesleft>0) continue;
        if (!UsefulStart) break;
        OutputLine(fname, &offset, UsefulStart, opt, fd, FALSE);
    } while (bytesunfetched > 0);
    if (bytesleft>0 && UsefulStart)
        OutputLine(fname, &offset, UsefulStart, opt, fd, FALSE);
    if (opt==(int)OPT_O_LOCAL) {
        fputs("\f\n", fd);  /* Emit form feed.  */
        fflush(fd);
    }
    if (opt==(int)OPT_D_LOCAL) fclose(fd);
    if (bodylen < 0) {
        ReportError(VUIMSG_MSGREAD, ERR_WARNING, FALSE);
	return(-1);
    }
    if ((bodylen == 0) && (offset == 0)) ReportSuccess("<Empty Message>");
      else  ClearError();
}

OutputLine (fname, offset, buffer, opt, fd, sendcr)
char *fname, *buffer;
int opt;
Boolean sendcr;
FILE *fd;
long *offset;
{
    int bodylen, rc;
    bodylen=strlen(buffer);
    rc = 0;
    if (opt==(int)OPT_D_REMOTE) {
        if (sendcr) {
            buffer[bodylen++]='\n';
            rc=buffer[bodylen];
            buffer[bodylen]='\0';
        }
        mserrcode=MS_StorePartialFile(fname, *offset, bodylen, 0644, TRUE, buffer);
        if (sendcr) {
            buffer[bodylen]=rc;
            buffer[bodylen-1]='\0';
        }
        debug((2,"Stored %d bytes at %ld.\n",bodylen, *offset));
        if (mserrcode) {
            ReportError(VUIMSG_MSSTORE, ERR_WARNING, TRUE);
            return(-1);
        }
    } else {
        debug((2,"Wrote %d bytes.\n",bodylen));
        buffer[bodylen]='\0';
        rc=fputs(buffer, fd);
        if (sendcr) rc=fputc('\n', fd);
        fflush(fd);
        if (rc==EOF) {
            ReportError("Error writing locally...", ERR_WARNING, FALSE);
            return(-1);
        }
    }
    *offset += bodylen;
    return(0);
}

WhenIs ()
{
    char ErrorText[256], *ck_date, datebuf[81];
    ck_date=GetSUser("Enter date to check:", NIL, NIL, datebuf);
    if (ck_date==NIL) return(0);
    RealWhenIs(ck_date, ErrorText);
    ReportSuccess(ErrorText);
    return(0);
}

RealWhenIs (date_in, date_out)
char *date_in, *date_out;
{
    char ErrorText[256];
    int year, month, day, hour, min, sec, wday;
    long gtm;

    mserrcode = MS_ParseDate(date_in, &year, &month, &day, &hour, &min, &sec, &wday, &gtm);
    if (mserrcode) {
	sprintf(ErrorText, "I don't understand the date %s", date_in);
	ReportError(ErrorText, ERR_WARNING, TRUE);
	return(-1);
    }
    sprintf(date_out,"%d/%d/%d %d:%02d:%02d", month+1, day, year, hour, min, sec);
    return(0);
}

WhoIs ()
{
    char *addr, *result, addrbuf[81];
    int badct;
    addr=GetSUser("Enter address to check:", NIL, NIL, addrbuf);
    if (addr==NIL) return(0);
    ReportSuccess("Verifying name list...");
    badct=CUI_RewriteHeaderLine(addr, &result);
    if (badct==0) ReportSuccess(result);
    free(result);
    return(0);
}

DeleteMessage (msgno, updatescreen)
long msgno;
Boolean updatescreen;
{
    int cuid, save_row, new_row;
    char SnapshotBuf[AMS_SNAPSHOTSIZE], textbuf[256], abuf[5];

    cuid = CuidFromMsgno(msgno);
    if (CUI_GetSnapshotFromCUID(cuid, SnapshotBuf)) return(-1);

    sprintf(textbuf, "Message %ld ", msgno);

    new_row = MsgIsVisible(msgno);
    if (new_row >= 0) {
	GetAndSetMsgRow(&save_row, new_row);
	GetCurrentMsgAttributes(abuf, sizeof(abuf));
	abuf[sizeof(abuf)-1] = '\0';
    }

    if (AMS_GET_ATTRIBUTE(SnapshotBuf, AMS_ATT_DELETED)) {
        if (CUI_UndeleteMessage(cuid)) {            /* mas - Delete -> delete */
            ReportError("Error recovering message.", ERR_WARNING, FALSE);
        } else {
	    strcat(textbuf, "recoverd.");
            ReportSuccess(textbuf);
	    if (new_row >= 0) {
		abuf[0] = abuf[1] = ' ';
		if (AMS_GET_ATTRIBUTE(SnapshotBuf, AMS_ATT_UNSEEN))
		    abuf[0] = 'N';
	    }
	}
    } else {
        if (CUI_DeleteMessage(cuid)) {
            ReportError("Error deleting message.", ERR_WARNING, FALSE);
        } else {
	    strcat(textbuf, "deleted.");
            ReportSuccess(textbuf);
	    if (new_row >= 0) {
		abuf[0] = 'D'; 
		abuf[1] = ' ';
		if (AMS_GET_ATTRIBUTE(SnapshotBuf, AMS_ATT_UNSEEN))
		    abuf[1] = 'N';
	    }
	}
    }
    if (new_row >= 0) {
	ReshowCurrentMsgLine(abuf, updatescreen, (save_row == new_row));
	GetAndSetMsgRow(&new_row, save_row);
    }
    return(0);
}

ToggleMessageMark(msgno, updatescreen, quiet)
long msgno;
Boolean updatescreen, quiet;
{
    int save_row, new_row;
    Boolean marked;
    char abuf[5];
    MARKED_MSGS *linkp;

    if ((new_row = MsgIsVisible(msgno)) >= 0) {
	GetAndSetMsgRow(&save_row, new_row);
	GetCurrentMsgAttributes(abuf, sizeof(abuf));
	abuf[sizeof(abuf)-1] = '\0';

	if (abuf[2] == 'M') {
	    abuf[2] = ' ';
	    marked = FALSE;
	}
	else {
	    abuf[2] = 'M';
	    marked = TRUE;
	}
	ReshowCurrentMsgLine(abuf, updatescreen, (save_row == new_row));
	GetAndSetMsgRow(&new_row, save_row);
    } else {
	linkp = MarkedMsgList;
	while (linkp && linkp->msgno != msgno) linkp = linkp->next;
	if (linkp && linkp->msgno == msgno) marked = FALSE;
	else marked = TRUE;
    }

    if (!quiet) {
	if (marked) ReportSuccess("Message marked.");
	else ReportSuccess("Message Unmarked.");
    }

    return(marked);
}

CloneMessage (msgno, code, updatescreen, ShouldAsk)
long msgno;
int  code;
Boolean updatescreen, ShouldAsk;
{
    int cuid;
    char *newdir, FolderName[MAXPATHLEN+1], dirbuf[81];

    cuid = CuidFromMsgno(msgno);

    if (ShouldAsk) {
	for (;;) {
	    newdir = GetSUser("Enter name of folder to receive message:", 
			  NIL, last_folder_name, dirbuf);
	    if (newdir==NIL) return(-1);
	    if (*newdir != '?') break;
	    ListFolders();
	}
	strcpy(last_folder_name, newdir);
   }
   strcpy(FolderName, last_folder_name);
   if (CUI_CloneMessage(cuid, FolderName, code))
        return(-1);
   if (code==MS_CLONE_COPYDEL) {
       int save_row, new_row;
       char abuf[5];
       if ((new_row = MsgIsVisible(msgno)) >= 0) {
	   GetAndSetMsgRow(&save_row, new_row);
	   GetCurrentMsgAttributes(abuf, sizeof(abuf));
	   abuf[sizeof(abuf)-1] = '\0';
	   if (abuf[0] != 'D') abuf[1] = abuf[0];
	   abuf[0] = 'D';
	   ReshowCurrentMsgLine(abuf, updatescreen, (save_row == new_row));
	   GetAndSetMsgRow(&new_row, save_row);
       }
   }
   return(0);
}

ListFolders()
{
    char ErrorText[256], FileBuf[MAXBODY], OutFileName[MAXPATHLEN+1], 
    *s, *shortname, *longname, *nextline;
    int  bodylen, increment;
    long offset, offsetout, bytesunfetched;
    char insertbuf[80], PathElt[MAXPATHLEN+1], MapFile[MAXPATHLEN+1];

    ReportSuccess("Generating folder list.  Please stand by...");
    if (mserrcode=MS_GetSearchPathEntry(AMS_MAILPATH, PathElt, MAXPATHLEN)) {
	    sprintf(ErrorText,"Can't get search path entry");
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	    return(-1);
    }
    if (mserrcode) return(-1);
    if ((mserrcode=MS_NameSubscriptionMapFile(PathElt, MapFile))!=0) {
	if (AMS_ERRNO != ENOENT) {/* User may not have his own message dir */
	    sprintf(ErrorText, "MS can not generate subscription map file for %s", PathElt);
	    ReportError(ErrorText, ERR_WARNING, TRUE);
	}
	return(-1);
    }
    CUI_GenTmpFileName(OutFileName);
    offset = 0L;   offsetout = 0L;
    do {
	if ((mserrcode = MS_GetPartialFile(MapFile, FileBuf, MAXBODY - 1, offset, &bytesunfetched, &bodylen)) != 0)  {
	    ReportError(VUIMSG_MAPFILE, ERR_WARNING, TRUE);
	    return(-1);
	}
        debug((2,"We opened the file and read %d bytes starting at %ld.\n", bodylen, offset));
        if (bodylen <= 0)       /* We hit the end of this file so try next */
             break;
        FileBuf[bodylen] = '\0';  /* Make sure null-terminated */
        for (s=FileBuf; *s; s=nextline) {
	    nextline=strchr(s,'\n');
            if (nextline==NIL) break;
            *nextline++ = '\0';
            increment = nextline - s;       /* Move pointer of where we've been */
            debug((9,"Parsing %s\n",s));
            longname=strchr(s,':');
            if (longname!=NIL) *longname++ = '\0';
              else              longname=s;
            shortname=s;
	    s=strchr(longname,' ');
	    if (s!=NIL) s++;
	    sprintf(PathElt, "%s\n", shortname);
	    if (mserrcode=MS_StorePartialFile(OutFileName, offsetout, strlen(PathElt), 0644, FALSE, PathElt)) {
		sprintf(ErrorText,"Can't write folder list");
		ReportError(ErrorText, ERR_WARNING, TRUE);
		return(-1);
	    }
	    offsetout += strlen(PathElt);
	    offset += increment;
         }  /* End for */
    } while (bytesunfetched>0);
    if (bodylen <= 0) {
          ReportError(VUIMSG_READFILE, ERR_WARNING, TRUE);
    }
    sprintf(insertbuf, "Press %s to leave, %s or %s to page up/down",
	     ESC_STRING,PGUP_STRING,PGDN_STRING);
    ShowMiniFile(OutFileName, insertbuf,0L,TRUE,MINIFILE_ROW);
}

PrintBody (cuid)
int cuid;
{
     return(CUI_PrintBodyFromCUID(cuid));
}

EditFile (arg, resave)
char *arg;
int resave;
{
    /* Boolean */ short FinishedElsewhere;  /* mas : caused core dump */
    char    FileName[MAXPATHLEN + 1], LocalName[MAXPATHLEN + 1];
    char ErrorText[256];

    debug((1,"EditFile %s\n", arg));
    CUI_GenLocalTmpFileName(LocalName);
    if (LocalName==NIL) return(-1);  /* It reported the error. */
    if ((mserrcode = MS_DisambiguateFile(arg, FileName, AMS_DISAMB_FILEEXISTS)) == 0) {
        debug((4,"MS_Disamb returned %s\n",FileName));
        if (CUI_GetFileFromVice(LocalName, FileName)) return(-1);
    } else {
        if (!GetBooleanFromUser("File not found. Do you wish to create it?",
                                TRUE)) {
            return(-1);
        }
        CursorOn();  /* get boolean left it off */
        if ((mserrcode = MS_DisambiguateFile(arg, FileName, AMS_DISAMB_FILEMAYBECREATED)) != 0) {
            sprintf(ErrorText, "The file '%s' cannot be created.", arg);
            ReportError(ErrorText, ERR_WARNING, TRUE);
            return(-1);
        }
        debug((4,"MS_Disamb returned %s\n",FileName));
        /* ** May have to create empty file here if local editor can't *** */
    }
    if (EditLocalFile(LocalName, &FinishedElsewhere) || !resave) {
          unlink(LocalName);
          return(resave);
    }
    CUI_AppendFileToVice(LocalName, FileName, 0L);
    if (!FinishedElsewhere) unlink(LocalName);
    return(FinishedElsewhere ? 1 : 0);
}

SetEditor ()
{
    char *edit_ptr, editbuf[81];
    edit_ptr=GetSUser("Enter name of editor to use:", NIL, VUI_editor, editbuf);
    if (edit_ptr==NIL) return(-1);
    strcpy(VUI_editor, edit_ptr);
    SetEditorToUse(VUI_editor);
}

SetPrinter ()
{
    char *edit_ptr, editbuf[81];
    edit_ptr = GetSUser("Enter name of printer to use:", NIL,
			 VUI_printer, editbuf);
    if (edit_ptr==NIL) return (-1);
    if (CUI_SetPrinter(editbuf) != 0) return(-1);
    strcpy(VUI_printer,edit_ptr);
    return(0);
}

RealUnlinkViceFile (arg)
char *arg;
{
    char FileName[1+MAXPATHLEN];

    if (MS_DisambiguateFile(arg, FileName, AMS_DISAMB_FILEEXISTS)!=0)
        return(-1);
    debug((4,"MS_Disamb returned %s\n",FileName));
    if (MS_UnlinkFile(FileName)) return(-1);
    return(0);
}

CheckViceFileEmpty (filename)
char *filename;
{
    int  bodylen;
    long unfetched;
    char fullname[1+MAXPATHLEN], body[LINE_LENGTH+1];

    if ((MS_DisambiguateFile(filename, fullname, AMS_DISAMB_FILEEXISTS) != 0) ||
	 (MS_GetPartialFile(fullname, body, LINE_LENGTH, 0L, &unfetched, &bodylen) != 0))
	return(-1);

    if (bodylen <= 0) return(-1);

    return(0);
}

GetVersion (ver_str, ver_str_len)
char *ver_str;
int ver_str_len;
{
    char msv[80];

    debug((1,"Print version numbers\n"));
    *msv = ' ';
    if ((mserrcode = MS_GetVersion(msv+1, (sizeof msv)-1)) != 0) {
        ReportError("Couldn't get message server version number",
                    ERR_WARNING, TRUE);
        strncpy(ver_str, CUI_VersionString, ver_str_len);
        return(-1);
    }
    strncpy(ver_str, CUI_VersionString, ver_str_len);
    strncat(ver_str, msv, ver_str_len - strlen(CUI_VersionString));
    return(0);
}

GetMyHostname()
{
    long unfetched;
    int  bytesfetched;
    char *bufptr, buffer[81];

    if (!VUI_MSConnected) return(-1);
    if (!HostMayHaveChanged) return(0);

    if (mserrcode = MS_GetPartialFile("/.hostname", buffer, LINE_LENGTH, 0L, 
				       &unfetched, &bytesfetched))
	return(-1);

    buffer[bytesfetched] = '\0';
    bufptr = StripWhiteEnds(buffer);
    if (bufptr && *bufptr) strcpy(RunningOnHost, bufptr);
    HostMayHaveChanged = FALSE;
    return(0);
}

HandleTimeout (name, retries, restarts)
char   *name;
int     retries,
        restarts;
{
    debug((1,"HandleTimeout(%s, %d, %d)\n",name, retries, restarts));

    VUI_MSConnected = TRUE; /* assume we will be connected */

    if (retries < 0) {
        ReportError("Connection to message server timed out; retrying...",
                    ERR_WARNING, FALSE);
        debug((1,"Handle timeout returning RPC_RETRY\n"));
        return(CUI_RPC_RETRY);
    }
    if (restarts < 2) {
        ReportError("Reconnecting to message server; please wait...",
                    ERR_WARNING, FALSE);
        debug((1,"Handle timeout returning RPC_RESTART\n"));
        return(CUI_RPC_RESTART);
    }
    debug((1,"Handle timeout returning RPC_BUGOUT\n"));
    return(CUI_RPC_BUGOUT);     /* No message needed here -- will
                                   propogate error */
}

DidRestart () 
{
    while (KeyHit()) KeyIn();    /* Clear the keyboard buffer in case they were
                                   pounding on it. */
    ReportSuccess("Reconnected to Message Server!");
    HostMayHaveChanged = TRUE;
}

char *PackSpecialHeader(header, data, freedata)
char *header, *data;
int freedata;
{
    /* this routine builds a header line by concatanating the header
      and the data, keeping in mind that it should be header format.
      The first arg is static pointer, the second one is malloced */

    int   headlen, datalen;
    char *result, *eol;

    if (header == NIL) return(NIL);

    headlen = strlen(header);
    datalen = (data==NIL) ? 0 : strlen(data);
    result = (char *)malloc(headlen + datalen + 10);
    if (!result) {
	ReportError(VUIMSG_NOMEM, ERR_CRITICAL, FALSE);
	return(NIL);
    }

    strcpy(result, header);

    if (headlen > LINE_LENGTH - datalen) {
	if ((datalen > 0) && ((eol = strchr(data, '\n')) != NIL)) {
	    if ((eol - data) > LINE_LENGTH-headlen)
		strcat(result, "\n ");
	} else
	    strcat(result, "\n ");
    }
    if (data) strcat(result, data);

    if (freedata && data) free(data);
    return(result);
}

int AddSpecialHeaders(opt)
int opt;
{
    /* add special headers such as, vote, ack, folder subs */

    int   i, rc;
    char *outvec[10], buffer[256], *bufptr, *validated = NIL;
    char fulldirname[MAXPATHLEN+1], *newdir, *prompt, bigbuf[1024];
    
    if (opt == (int)OPT_E_ACK) prompt = "Acknowledge to:";
    else if (opt == (int)OPT_E_VOTE) prompt = "Send votes to:";
    else if (opt == (int)OPT_E_REDIST) prompt = "Redsitribution to:";
    else prompt = NIL;

    if (prompt) {
	if ((bufptr = GetSUser(prompt, NIL, NIL, buffer)) == NIL)
	    return(FALSE);
	bufptr = StripWhiteEnds(bufptr);
	if (!*bufptr) return(FALSE);
	ReportSuccess("Validating the address...");
	if ((CUI_RewriteHeaderLine(bufptr, &validated)) || (!validated)) {
	    if (validated) free(validated);
	    return(FALSE);
	}
    }

    switch (opt) {
	case (int)OPT_E_REDIST:
	case (int)OPT_E_ACK:
	    if ((outvec[0] = PackSpecialHeader((opt==(int)OPT_E_ACK ? ACK_STRING :
						REDIST_STRING), validated, TRUE)) == NIL)
		return(FALSE);
	    outvec[1] = NIL;
	    break;

	case (int)OPT_E_VOTE:
	    strcpy(bigbuf, VOTE_ID_STRING);
	    strcat(bigbuf, RunningOnHost);
	    strcat(bigbuf, ", ");
	    bufptr = GetSUser("Vote question:",NIL,NIL,buffer);
	    bufptr = StripWhiteEnds(bufptr);
	    if ((outvec[0] = PackSpecialHeader(bigbuf,buffer,FALSE)) == NIL)
		return(FALSE);
	    bigbuf[0] = '\0';
	    for (i=0; i<20; i++) {
		char tmpbuf[LINE_LENGTH];
		sprintf(tmpbuf,"Vote choice %d:",i+1);
		if ((bufptr = GetSUser(tmpbuf, NIL,NIL, buffer)) == NIL) break;
		bufptr = StripWhiteEnds(bufptr);
		if (!*bufptr) break;
		if (i>0) strcat(bigbuf, ", ");
		strcat(bigbuf, bufptr);
	    }
	    if (!(outvec[1] = PackSpecialHeader(VOTE_CHOICE_STRING, bigbuf, FALSE)) ||
		!(outvec[2] = PackSpecialHeader(VOTE_TO_STRING, validated, TRUE)))
		return(FALSE);
	    outvec[3] = NIL;
	    break;

	case (int)OPT_E_INVITE:
	    if ((bufptr = GetSUser("Bboard name:",NIL,NIL,buffer)) == NIL)
		return(FALSE);
	    bufptr = StripWhiteEnds(bufptr);
	    if (!*bufptr) return(FALSE);
	    newdir = fulldirname;
	    if (mserrcode = CUI_DisambiguateDir(bufptr, &newdir)) {
		ReportError("Can not resolve bboard path",ERR_WARNING, FALSE);
		return(FALSE);
	    }
	    if ((outvec[0] = PackSpecialHeader(INVITE_STRING, newdir, FALSE)) == NIL)
		return(FALSE);
	    outvec[1] = NIL;
	    break;

	default:
	    return(FALSE);
    }

    rc = AddHeaderToFile(outvec);

    for (i=0; outvec[i] != NIL; i++)
	free(outvec[i]);

    ClearError();

    return(rc);
}

SubmitMessage () 
{
    int     rc;
    long    filelength;
    char    out_file[MAXPATHLEN+1];

    ReportSuccess("Submitting message for delivery.  Please stand by...");

    if (ValidateAddrList())
	return(-1);

    out_file[0] = '\0';
    filelength = 0L;
    if (CompressFile(ENT_headerfile, out_file, &filelength, FALSE, TRUE))
	return(-1);

    if (CompressFile(ENT_filename, out_file, &filelength, FALSE, FALSE))
        return(-1);

    debug((2,"Message body compressed; %ld bytes written to output file %s.\n",
	    filelength, out_file));

    rc=CUI_SubmitMessage(out_file, BlindStatus);

    if (rc)
	RealUnlinkViceFile(out_file); /* There was an error, get rid of this */

    return(rc);
}

ValidateAddrList ()
{
    int  rc;
    char in_file[MAXPATHLEN+1],out_file[MAXPATHLEN+1];
    long filelength;

    filelength = 0L;
    *in_file = *out_file = '\0';
    if (CompressFile(ENT_headerfile, in_file, &filelength, FALSE, TRUE)) return(-1);

    if (rc = CUI_ValidateFile(in_file, out_file)) {
	if (*out_file == '\0') /* real errors? */
	    return(-1);
    } /* just validation errors, reload the validated file */

    filelength = 0L;
    RealUnlinkViceFile(in_file);
    if (DecompressFile(out_file, in_file, &filelength, TRUE, TRUE)) return(-1);
    RealUnlinkViceFile(ENT_headerfile);
    strcpy(ENT_headerfile, in_file);
    ENT_headlength = filelength;
    return(rc ? -1 : 0);
}

int NeedLineFeed(buf)
char *buf;
{
    int   colon_len;
    char *colon;

    if (*buf == ' ') return(TRUE);

    colon = strchr(buf,':');
    if (colon == NIL) return(FALSE);
    colon_len = colon - buf + 1;

    if (ULstrncmp(buf, "to:", colon_len) == 0) return(TRUE);
    if (ULstrncmp(buf, "cc:", colon_len) == 0) return(TRUE);
    if (ULstrncmp(buf, "subject:", colon_len) == 0) return(TRUE);

    return(FALSE);
}

CompressFile (in_file, Outputfile, filelength, erase_in_file, header_format)
char *in_file, *Outputfile;
long *filelength;
Boolean erase_in_file, header_format;
{
    int     bodylen, i, tmplen, linelen, add_linefeed;
    long    offset_in, bytesunfetched;
    char    *BodyBuf, *this_char, *line_start, save_lastchar;

    if (!*Outputfile) CUI_GenTmpFileName(Outputfile);
    debug((1,"CompressFile(%s, %s, %ld, %c)\n", 
	   in_file, Outputfile, *filelength, erase_in_file));
    debug((3,"Compressing message into %s\n",Outputfile));

    if (!(BodyBuf = (char *)malloc(BODY_field_len+1))) {
	ReportError(VUIMSG_NOMEM, ERR_WARNING, FALSE);
	return(-1);
    }

    offset_in=0;

    do {
       if ((mserrcode = MS_GetPartialFile(in_file, BodyBuf, BODY_field_len, 
					  offset_in, &bytesunfetched, &bodylen)) != 0) {
           debug((2,"Can't read the message body file for some reason.\n"));
           return(-1);
       }
       debug((2,"Read %d bytes at %ld with %ld to go.\n", 
	      bodylen, offset_in, bytesunfetched));
       if (bodylen <= 0) break;

       /* strip out those unused lines at the end of file */

       this_char=BodyBuf+bodylen-1;
       tmplen = bodylen;
       while ((tmplen) && (*this_char==' ')) {
	   this_char--;
	   tmplen--;
       }
       if (((bodylen - tmplen) > (3 * LINE_LENGTH)) || /* more than 3 blank lines */
	    (bytesunfetched <= 0)) /* or the last buffer of the file */
	   *(this_char+1)='\0';	   /* then strip the blanks */

       line_start=BodyBuf;
       for (i = bar_row - N_TCS_LINES; i > 0; i--) {
            this_char = line_start + LINE_LENGTH - 1;
            linelen = LINE_LENGTH;
	    add_linefeed = FALSE;
            if ((*this_char == ' ') || (!*this_char)) {/* if line goes all the way don't add \n */
                while (linelen>0 && *this_char==' ') {
                    linelen--;
                    this_char--;
                }
                if (linelen>0 && !*this_char) {  /* The end of this block    */
                    *this_char='\n';
                    i = 0;
                } else {                /* A normal line                */
		    if ((header_format) && (linelen <= 0) && 
			((*this_char == ' ') || (!*this_char))) {
			linelen = 0; /* remove extra blanks from header */
		    } else {
			*(this_char+1)='\n';
			linelen++;      /* Count the \n also */
		    }
                }
            }
	    else if ((*this_char) && (header_format) && (NeedLineFeed(this_char+1))) {
		add_linefeed = TRUE;
		save_lastchar = *(line_start+linelen);
		*(line_start+linelen) = '\n';
		linelen++;
	    }

	    if (linelen > 0) {
		if (mserrcode = MS_StorePartialFile(Outputfile, *filelength, 
						    linelen, 0644, TRUE, line_start)) {
		    ReportError(VUIMSG_MSSTORE,ERR_WARNING,TRUE);
		    return(-1);
		}
	    }
            debug((2,"Line %d is '%.20s'\n", i, line_start));
            debug((2,"Wrote %d bytes at %ld.\n",linelen, *filelength));
	    if (add_linefeed)
		*(line_start+linelen-1) = save_lastchar;
            *filelength += linelen;
            line_start += LINE_LENGTH;
       }
       offset_in += bodylen;
    } while (bytesunfetched > 0);

    debug((2,"Message body added; %ld bytes written to output file.\n", *filelength));

    if (erase_in_file)
        RealUnlinkViceFile(in_file);

    if (mserrcode = MS_StorePartialFile(Outputfile, *filelength, 2, 0644, TRUE, "\n\n")) {
	ReportError(VUIMSG_MSSTORE,ERR_WARNING,TRUE);
	return(-1);
    } else *filelength += 1;

    return(0);
}

DecompressFile (in_file, out_file, filelength, erase_in_file, convert_tabs)
char *in_file, *out_file;
long *filelength;
Boolean erase_in_file, convert_tabs;
{
    int     bodylen, i, bytesleft;
    long    offset, bytesunfetched, outfilelength;
    char    BodyBuf[MAXBODY+2], *in_buf_p, *out_buf_p, OutBuf[LINE_LENGTH], *eol;

    CUI_GenTmpFileName(out_file);

    debug((2,"Decompress file (%s, %s, %ld, %c)\n",in_file, out_file,
             *filelength, erase_in_file));

    offset=(filelength==(long *)NIL)?0L : *filelength;
    outfilelength=0L;   bytesleft=0;
    out_buf_p=OutBuf;    in_buf_p=BodyBuf;
    if (filelength!=(long *)NIL) *filelength=0L;

    do {
        debug((2,"Reading data at offset %ld\n", offset));
        if ((mserrcode = MS_GetPartialFile(in_file, in_buf_p, MAXBODY-bytesleft, offset+(long)bytesleft, &bytesunfetched, &bodylen)) != 0)
            return(-1);
        debug((2,"Read %d bytes and %ld are left\n",bodylen, bytesunfetched));
        if (bodylen <= 0) break;
        BodyBuf[bodylen+bytesleft] = '\0';
	if ((bytesunfetched <= 0) && (strchr(in_buf_p,'\n') == NIL)) {
	    BodyBuf[bodylen+bytesleft] = '\n';
	    BodyBuf[bodylen+bytesleft+1] = '\0';
	} /* this code doesn't work if there is no CR in the buffer, so there is this hack! */
        in_buf_p=BodyBuf;

        while (1) {
           eol=strchr(in_buf_p, '\n');
           if (eol==NIL && bytesunfetched>0) {
               out_buf_p=BodyBuf;
               bytesleft = 0;
               while (*in_buf_p) {/* Copy what's left to the beginning of the input */
                   *out_buf_p++ = *in_buf_p++;
                   bytesleft++;
               }
               in_buf_p=out_buf_p;
               debug((2,"No more nls found; %d bytes left in current buffer.\n", bytesleft));
               break;
           }
	   out_buf_p = OutBuf;
           for (i=0; i<LINE_LENGTH; i++) {  /* mas V1.3 : fixed tabs */
                if (!*in_buf_p || *in_buf_p=='\n') {
                    *out_buf_p++ = ' ';
		} else {
		    if (*in_buf_p == '\t') {
			if (convert_tabs) 
			    *out_buf_p++ = ' ';
			else
			    do {
				*out_buf_p++ = ' ';
			    } while (++i%TABLEN);
			in_buf_p++;
		    } else {
			*out_buf_p++ = *in_buf_p++;
		    }
		    offset++;
		}
	   }
           mserrcode = MS_StorePartialFile(out_file, outfilelength, LINE_LENGTH, 0644, TRUE, OutBuf);
           if (mserrcode) {
               ReportError(VUIMSG_MSSTORE,ERR_WARNING,TRUE);
               return(-1);
           }
           debug((2,"Wrote %d bytes at offset %ld\n", LINE_LENGTH, outfilelength));
           outfilelength += (long)LINE_LENGTH;
           if (eol==NIL) break;
           if (*in_buf_p == '\n') {
               in_buf_p++;
               offset++;
           }
        }  /* End while handling this buffer load */
    } while (bytesunfetched > 0);

    if (filelength!=(long *)NIL) *filelength=outfilelength;
    if (erase_in_file) { debug((2,"erasing %s\n", in_file));
        RealUnlinkViceFile(in_file);
    }
    return(0);
}


GetHeaderFields(in_file, out_file, offset)
char *in_file, *out_file;
long *offset;
{
    int     bodylen,len;
    long    bytesunfetched, offset_out;
    char    BodyBuf[WRITEFILECHUNK+1], *UsefulStart, *comma, *EOL;
    Boolean InHeaders = TRUE;

    CUI_GenTmpFileName(out_file);

    debug((3,"Reading message headers from %s\n", in_file));

    *offset = offset_out = 0L;

    do {
        if ((mserrcode = MS_GetPartialFile(in_file, BodyBuf, WRITEFILECHUNK, 
					   *offset, &bytesunfetched, &bodylen)) != 0) {
	    ReportError(VUIMSG_MSGREAD, ERR_WARNING, TRUE);
	    RealUnlinkViceFile(out_file);
            return(-1);
        }
        debug((2,"Read %d bytes at %ld with %ld to go.\n",
		bodylen, *offset,  bytesunfetched));
        if (bodylen <= 0) break;

        BodyBuf[bodylen] = '\0';
	UsefulStart = BodyBuf;
	len = bodylen;
	EOL = NIL;
	while (InHeaders) {
	    EOL = strchr(UsefulStart, '\n');
	    if (EOL == NIL) break;
	    *offset += (EOL - UsefulStart + 1);
	    while (UsefulStart < EOL) {
		if (EOL - UsefulStart > LINE_LENGTH) {
		    comma = strchr(UsefulStart, ',');
		    if (comma == NIL || comma > EOL || comma == EOL - 1) comma = EOL;
		} else 
		    comma = EOL;
		len = comma - UsefulStart + 1;
		mserrcode = MS_StorePartialFile(out_file, offset_out, len, 0600, TRUE,
						UsefulStart);
		offset_out += len;
		if (!mserrcode && *comma == ',') 
		    mserrcode = MS_StorePartialFile(out_file,offset_out++,1,0600,TRUE,"\n");
		if (mserrcode) {
		    ReportError(VUIMSG_MSSTORE, ERR_WARNING, TRUE);
		    RealUnlinkViceFile(out_file);
		    return(-1);
		}
		if (comma < EOL && *(comma+1) != ' ' && *(comma+1) != '\t')
		    *comma = ' ';
		else comma++;
		UsefulStart = comma;
	    }
	    if (*UsefulStart == '\n')
		InHeaders = FALSE;
	}
	if (InHeaders && EOL == NIL) {
	    if (!*UsefulStart && *(UsefulStart-1) == '\n') *offset--;
	    bytesunfetched = 1;
	}
    } while (InHeaders && bytesunfetched > 0);

    return(0);
}

extern int HeadersOn;
Boolean HeadKeep;
struct head_list *HeadList = (struct head_list *)NIL;

static int AddHeads (arg)
char *arg;
{
    char *s, *s2;
    struct head_list *t, *t2, *last;
    while (arg && *arg!='\0') {
        s=strchr(arg,':');
        s2=strchr(arg,' ');
        if (s2 && (!s || s2 < s)) {
                s = s2;
        }
        if (s) *s++='\0';
        t2=(struct head_list *)malloc(sizeof(struct head_list));
        if (t2==(struct head_list *)NIL) {
           ReportError(VUIMSG_NOMEM, ERR_WARNING, FALSE);
           return(-1);
        }
        if (HeadList==(struct head_list *)NIL) {
            HeadList=t2;
            t2->next=(struct head_list *)NIL;
        } else {
            last=HeadList;
            for (t=HeadList; t && 0<ULstrcmp(t->header, arg); t=t->next) last=t;
	    if (t==(struct head_list *)NIL)
		t = last;
            if (t==HeadList) {
                if (0>ULstrcmp(t->header,arg)) {
                    t2->next=t;
                    HeadList=t2;
                } else {
                    t->next=t2;
                    t2->next=(struct head_list *)NIL;
                }
            } else
               if (t->next==(struct head_list *)NIL) {
                   t2->next=(struct head_list *)NIL;
                   t->next=t2;
               } else {
                   t2->next=t;
                   last->next=t2;
               }
        }
        t2->header=copy(arg);
        arg=s;
    }
    return(0);
}

int KeepHeader (arg)
char *arg;
{
    /* Put + at beginning to add headers */
    if (*arg=='+') {
        if (HeadersOn && !HeadKeep) {
            ReportError(VUIMSG_AOHEAD,ERR_WARNING, FALSE);
            return(-1);
        }
        arg++;
    } else HeadList=(struct head_list *)NIL;
    HeadKeep=TRUE;
    HeadersOn=TRUE;
    return(AddHeads(arg));
}

int OmitHeader (arg)
char *arg;
{
    if (*arg=='+') {
        if (HeadersOn && HeadKeep) {
            ReportError(VUIMSG_AOHEAD, ERR_WARNING, FALSE);
            return(-1);
        }
        arg++;
    } else HeadList=(struct head_list *)NIL;
    HeadKeep=FALSE;
    HeadersOn=TRUE;
    return(AddHeads(arg));
}



CheckHead (arg)
char *arg;
{
    struct head_list *t;
    int i = -1;

    debug((1,"Checking %s for useful header.\n", arg));
    for (t=HeadList; t && (i<0); t=t->next)
        if (!(i=ULsubstr(arg, t->header))) return(TRUE==HeadKeep);
    return(FALSE==HeadKeep);
}

extern char *ms_errlist[],*rpc_errlist[];
extern char *sys_errlist[];
extern int ms_nerr, rpc_nerr, sys_nerr;
#ifdef DEBUG
extern char *ms_errcauselist[],*ms_errvialist[];
extern int  ms_nerrcause, ms_nerrvia;
#endif /* DEBUG */

ReportError (text, error_level, decode)
char *text;
int error_level, decode;
{
    char    ErrorText[500], NumDum[10];
    int     i, errnum;
    Boolean MustDie = FALSE,ShouldWait = FALSE;
    PRMPT temp;
#ifdef DEBUG
    int	    errcause, errvia, errrpc;
#endif /* DEBUG */

    debug((1,"ERROR: %s, %d, %d\n",text, error_level, decode));
    if (error_level <= ERR_CRITICAL) {
	if (error_level < ERR_CRITICAL)
	    MustDie = TRUE;
	else
	    ShouldWait = TRUE;
    }
    if (Interactive) {
	if (decode) {
	    debug((1," Mserrcode = %ld\n", mserrcode));
            errnum = AMS_ERRNO;
            if (errnum == ETIMEDOUT) {
                temp.pdata="A file server or the network is down";
                temp.plen=strlen(temp.pdata);
                ShowError(&temp);
		if (MustDie) {
		    TearDown(VUI_NORMAL_EXIT);
		    exit(-1);
		}
                return;
            } else if (errnum == EACCES) {
		int Authenticated;
		temp.pdata = NULL;
		mserrcode = MS_CheckAuthentication(&Authenticated);
		if (mserrcode) {
		    if (vdown(AMS_ERRNO)) {
			temp.pdata = "A file server or the network is down.";
		    } else {
			temp.pdata = NULL;
		    }
		} else if (!Authenticated) {
		    temp.pdata = "Your Vice authentication has apparently expired.";
		} else temp.pdata = "You do not have access to this.";
		if (temp.pdata) {
		    temp.plen=strlen(temp.pdata);
		    ShowError(&temp);
		    if (MustDie) {
			TearDown(VUI_NORMAL_EXIT);
			exit(-1);
		    }
		    return;
		}
            }
        }
        strncpy(ErrorText,text,71);
	ErrorText[71] = '\0';
        temp.pdata=ErrorText;
	if (strlen(temp.pdata)>70) {
	    strcpy((temp.pdata+67),"...");
	    temp.plen = 70;
	}
	else
	    temp.plen = strlen(temp.pdata);
        ShowError(&temp);
        /* should wait a while here??? */
	for (i=0; i<8000; i++);
   } else {
        printf("%s\n",text);
   }
#ifdef DEBUG
   if (decode) {
        errnum = AMS_ERRNO;
        errcause = AMS_ERRCAUSE;
        errvia = AMS_ERRVIA;
        errrpc = AMS_RPCERRNO;
        debug((2,"errnum=%d, errcause=%d, errvia=%d, errrpc=%d\n",
                 errnum, errcause, errvia, errrpc));
        debug((2," and by the way sys_nerr=%d\n", sys_nerr));
        if (errrpc) {
            debug((1,"%s (AMS RPC error: %s)\n", text, rpc_errlist[errrpc]));
            return(0);
        }

    /* ETIMEDOUT hacks due to sys_nerr=37 on the PC so no errlist message for it. Comes out 'out of range' otherwise. */
        if (errnum < 0 || errnum >= (EMSBASE + ms_nerr)
            || (errnum < EMSBASE && errnum > sys_nerr && errnum != ETIMEDOUT)
           ) {
        debug((1,"errnum %d out of range\n", errnum));
        errnum = EMSUNKNOWN;
        }
        if (errcause < 0 || errcause >= ms_nerrcause) {
        debug((1, "errcause %d out of range\n", errcause));
        errcause = EIN_UNKNOWN;
        }
        if (errvia < 0 || errvia >= ms_nerrvia) {
        debug((1, "errvia %d out of range\n", errvia));
        errvia = EVIA_UNKNOWN;
        }
        if (errnum < EMSBASE) {
            if (errnum==ETIMEDOUT)
                sprintf(ErrorText, "%s - Connection timed out \n(in ", text);
              else {
                if (sys_errlist[errnum])
                 sprintf(ErrorText, "%s - %s \n(in ", text, sys_errlist[errnum]);
                else
                 sprintf(ErrorText, "%s - Unknown error %d \n(in ", text, errnum);
            }
        }
        else {
            if (ms_errlist[errnum - EMSBASE]) {
                sprintf(ErrorText, "%s - %s \n(in ", text, ms_errlist[errnum - EMSBASE]);
            }
             else {
                sprintf(ErrorText, "%s - Unknown error %d \n(in ",text,errnum);
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
        debug((1, "%s\n", ErrorText));
        if (errnum == ETIMEDOUT) {
            debug((1, "A file server or the network is down.\n"));
        } else if (errnum == EMSUNAUTH) {
            debug((1, "Your Vice authentication has apparently expired.\n"));
        }
   }
#endif /* DEBUG */

   if (MustDie || ShouldWait) {
       if (Interactive) {
	   if (DisplayCriticalError(text)) {
	       TearDown(VUI_EXIT_NOSAVE);
	       exit(-1);
	   }
	   else if (CUI_SnapIsRunning)
	       VUI_MSConnected = FALSE;
       }
       else {
	   exit(-1);
       }
   }
}

ReportSuccess (msg)
char *msg;
{
    int i;
    PRMPT temp;

    if (Interactive) {
        temp.pdata=msg;
	if (strlen(temp.pdata)>70) {
	    strcpy((temp.pdata+67),"...");
	    temp.plen = 70;
	} else
	    temp.plen=strlen(temp.pdata);
        ShowMsg(&temp);
        /* should wait a while here??? */
        for (i=0; i<7000; i++);
    } 
    else
        printf("%s\n",msg);
}

NonfatalBizarreError(text)
char *text;
{
    ReportError(text, ERR_WARNING, FALSE);
}

#ifdef DEBUG
/* VARARGS */
debugrtn(n, format, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20)
int n;
char *format, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8,
    *s9, *s10, *s11, *s12, *s13, *s14, *s15, *s16, *s17, *s18, *s19, *s20;
{
   if (CUIDebugging & (n)) {
        safefprintf(debugfile, format, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20);/* 
 **   fprintf(stdprn, format, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
                s12, s13, s14, s15, s16, s17, s18, s19, s20);  ***** */
    }
}
#endif

/* Flags for debugging tell which procedures to debug.  A current list:
        (They should be ORed together as appropriate)
        1       Entry to each procedure
        2       Procedures in cui.c, the main routines
        4       Most cui library routines
        8       Very low-level debugging, not usually wanted
        16      cuisnap routines
        32      AMSID hashing performance

        64      AMS_RETURN_ERRCODE macro
 */

MKEYTAB f1_keys[] = {
#ifdef DEBUG
    KEYCODE_ALT_F1,     DebugKey,           /* Alt-F1      */
#endif
    KEYCODE_F1,         HelpKey,            /* F1          */
    0, 0};

