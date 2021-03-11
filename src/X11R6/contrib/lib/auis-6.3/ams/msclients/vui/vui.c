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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/vui.c,v 2.40 1993/10/04 17:39:49 gk5g Exp $";
#endif

/* 
 *      This module contains the mainline VUI code for access
 *      to Message servers from an IBM PC/XT/AT
 */

#include <andrewos.h>
#include <ams.h>
#include <vui.h>
#include <vuivers.h>
#include <vuimenus.h>
#include <panel.h>
#include <lmenus.h>
#include <cmdargs.h>
#include <hdrparse.h>
#include <vuidebug.h>

static char default_headers[] = "cc:date:from:subj:to";
#define DEFAULT_HEADERS strcpy(malloc(strlen(default_headers)+1),default_headers)

extern char *getenv(), CUI_VersionString[], *GetSUser();

extern PANEL intro[], sess_bp[], mail_bp[], bboard_bp[], body_bp[], entry_bp[], *PanelArray[], parm_data[], mess_bp[], file_entry_bp[];

extern FIELD entry_fields[], *FieldArray[];

extern int GetNUser(), Interactive, ShowCursor(), FileClear(), ShowParmData(), CUI_SnapIsRunning, ShowDirData(), ShowMsgData(), ShowBodyData();

extern char ProgramName[];	/* comes from libutil.a */

extern MKEYTAB dir_keys[], msg_keys[], body_keys[], f1_keys[];
extern Boolean HeadKeep;

extern long SetTopOfPage(), SetTopOfPageAndSearch();

extern long MS_DisambiguateFile(), MS_StorePartialFile(), MS_GetDirInfo(), MS_ReInitialize(), MS_GetNewMessageCount(), MS_AppendFileToFolder(), CUI_DisambiguateDir();

extern unsigned char bar_row;

extern MENU_OPTS vui_menus[];
extern MENU_TREE main_menu[];
extern MENU_TREE folder_menu[];
extern MENU_TREE bb_menu[];
extern MENU_TREE logout_menu[];
extern MENU_TREE parms_menu[];
extern MENU_TREE mmsg_menu[];
extern MENU_TREE mbody_menu[];
extern MENU_TREE bbody_menu[];
extern MENU_TREE fmsg_menu[];
extern MENU_TREE bmsg_menu[];

extern MARKED_MSGS *MarkedMsgList;

int HelpCmd(), (*VUI_RepaintFn)();
char *VUI_HelpFile;
char VUI_editor[80], VUI_bccto[80], VUI_printer[80];
int VUI_UsePrintSpooldir = 0;
int VUI_Mailcount = 0;
Boolean VUI_AlwaysPurge = FALSE;
Boolean MSG_mailorbb;
Boolean VUI_MSConnected = TRUE;
Boolean exit_logout = FALSE;

char ProgramVersion[80];
char RunningOnHost[81];
extern char *ProgramPrefix;

char IBMid[] = "(c) Copyright IBM Corporation, 1987, 1988 ";
char *mailyn;     /* Switch indicator       */
char *bbrdyn;     /* Switch indicator       */
char *sendyn;     /* Switch indicator       */
char *debugn;     /* Switch indicator       */
char *subscr;     /* Switch indicator       */
char *fileyn;     /* Switch indicator       */
char *foldyn;     /* Switch indicator       */
char *nochange;
char *mailarg, *datearg;  /* Initial directory and search date */

VIDEOPARMS vp;
int     HeadersOn = FALSE;   /* Header filtering initially off */
int     BlindStatus = AMS_SEND_BLINDYES;                        /* V1.3 MAC */
long    mserrcode;

PRIVATE STABLE switchtable[] = {
        'm', FALSE, &mailyn, NULL,
        'p', FALSE, &foldyn, NULL,
        'b', FALSE, &bbrdyn, NULL,
        'n', FALSE, &subscr, NULL,
        's', FALSE, &sendyn, NULL,
        'f', FALSE, &fileyn, NULL,
        'd', FALSE, &debugn, NULL,
	'c', FALSE, &nochange, NULL,
        0, 0, NULL, NULL
        };

PRIVATE PTABLE positiontable[] = { &mailarg, &datearg, (char **) NIL };

ReDrawMain ()
{
    ShowString(" ", bar_row, 0, 1, HILITE);
    ShowString(" ", bar_row, 79, 1, HILITE);
    DrawPanel(intro, NIL, PANEL_CLEAR);
    ShowCursor();
}

int vuimain (argc, argv)
int argc;
char **argv;
{
int opt, last_opt = 0;
Boolean reshow = TRUE, quit = FALSE, GotoFlag, show_motd = TRUE;

    strcpy(ProgramName, "vui");
    amsconfig(argc, argv, CUI_SnapIsRunning? "vuis": "vuin");
    Interactive=FALSE;
    SetUp(argc, argv, &opt);
    show_motd = CUI_GetProfileSwitch(ProgramName,"motd",FALSE);  /* pdb */
    umask(077);  /* pdb - make sure vui temp files are only readable by owner */
    while (!quit) {
        GotoFlag=TRUE;

        if (reshow) {
            GotoFlag=FALSE;
            DrawPanel(intro, NIL, PANEL_CLEAR);
            ShowBar();
	    reshow=FALSE;
	}
	if (show_motd) {
	    if (VUIshowMOTD() != -1) {
		ShowBar();
	    }
	    show_motd = FALSE;
	}
	CheckForMail();
	if (opt == MENU_DO_OVER || opt == MENU_EMPTY) {
	    opt=MenuInput(main_menu, f1_keys, &last_opt);
	    if (opt<0) {
		ReportError("To leave the program, please select the 'Exit' command.", ERR_WARNING, FALSE);
		opt = MENU_EMPTY;
		continue;
	    }
	}
        switch (opt) {
        case OPT_G_MAIL:
	    opt=Dir_Panel(AMS_DEFAULTMAILDIR, "unread", MAIL);
	    if (opt==MENU_EMPTY && VUI_MSConnected && GetBooleanFromUser("No unread mail; do you wish to see mail for the last week?", TRUE)){
		opt=Dir_Panel(AMS_DEFAULTMAILDIR, "7 days ago", MAIL);
		if (opt==MENU_EMPTY && VUI_MSConnected && GetBooleanFromUser("Not even any mail this week; how about all your mail?", TRUE)){
		    opt=Dir_Panel(AMS_DEFAULTMAILDIR, "", MAIL);
		} else ClearPrompt();
	    } else ClearPrompt();
	    if (opt==(int)OPT_EXIT) break;
                break;
        case OPT_G_MAILF:
	    opt=Dir_Panel(mailarg, datearg, FOLDER);
	    break;
        case OPT_G_BBOARD:
	    opt=Dir_Panel(mailarg, datearg, BBOARD);
	    break;
        case OPT_G_SEND:
	    opt=SendSomeMail(0, AMS_REPLY_FRESH);
	    break;
        case OPT_G_PARMS:
	    opt=Parms_Panel();
	    break;
	case OPT_HELP:
	    opt=VUI_Help();
	    VUI_RepaintFn = ReDrawMain;
	    break;
	case OPT_MOTD:
	    if (VUIshowMOTD() == -1)
		ReportError("There is no current message of the day",ERR_WARNING, FALSE);
	    opt = MENU_EMPTY;
	    GotoFlag = FALSE;
	    break;
	case OPT_LOGOUT:
	    exit_logout = Logout_Panel();
	    if (!exit_logout) {
		opt = MENU_DO_OVER;
		break;
	    }
	case OPT_EXIT:
	    quit = TRUE;
	}
	if (opt==MENU_DO_OVER) reshow=TRUE;
        if (GotoFlag && opt==MENU_EMPTY) {
           ConfirmUser("Returning to main menu. Press enter to continue.");
           opt = MENU_DO_OVER;
        }
        mailarg=NIL; datearg=NIL;
	if (opt==(int)OPT_EXIT && CheckForMail()) {
	    if (!GetBooleanFromUser("Do you really want to quit now?", FALSE)) {
		opt = MENU_DO_OVER;
		quit = FALSE;
	    }
	}
    }

    TearDown(VUI_NORMAL_EXIT);
    return exit_logout;
}

TearDown (exittype)
Boolean exittype;
{
    if (exittype == VUI_NORMAL_EXIT) {
	GracefulExit(TRUE);
    }

    RestoreVideoEnv(&vp, PANEL_CLEAR, 0);
}

SetUp (argc, argv, opt_p)
int argc, *opt_p;
char **argv;
{
char *editor, heads[1024];
static char vui_version[80], vui_date[256];

    sprintf(vui_version,"Version %d.%d",VUI_MAJORVERSION, VUI_MINORVERSION);
    intro[1].pdata = vui_version;
    sprintf(ProgramVersion,"((prog %s%sVUI %d %d))",ProgramPrefix,
	    *ProgramPrefix? "-":"",VUI_MAJORVERSION, VUI_MINORVERSION);
    printf("%s%sVUI Version %d.%d\n", ProgramPrefix, *ProgramPrefix? "-":"",
	    VUI_MAJORVERSION, VUI_MINORVERSION);
    strcpy(RunningOnHost, "Host.Unknown");
    if (CUI_SnapIsRunning) ReportSuccess("Connecting to message server...");
    /* ser version before CUI_Initialize so check version can see it*/
    {char buf[100];
     sprintf(buf,"%s%sVUI.%d.%d", ProgramPrefix,
	    *ProgramPrefix? "-":"",  VUI_MAJORVERSION,VUI_MINORVERSION);
     CUI_SetClientVersion(buf);
    }   
    if (CUI_Initialize((int (*)())0, NIL)) {
	ConfirmUser("Press enter to continue...");
    }
    if(CUI_SnapIsRunning && (CUI_CheckVersion()!=0))
	ConfirmUser("Press enter to continue...");
    VUI_RepaintFn = ReDrawMain;
    VUI_HelpFile = NIL;
    datearg=NIL;
    /* Check command args, but as default, check mail */
    ParseArgs(argc, argv, switchtable, positiontable, HelpCmd);
    unlink("vui.bug");
    if (debugn && *debugn=='1') { 
       debugfile=fopen("vui.bug","w");
       CUIDebugging= -1;
    } else {
       CUIDebugging=0;
    }
    debug((2,"We are now back from ParseArgs\n"));
    if (mailyn && *mailyn=='1')        *opt_p = (int)OPT_G_MAIL;
      else if (bbrdyn && *bbrdyn=='1') *opt_p = (int)OPT_G_BBOARD;
      else if (sendyn && *sendyn=='1') *opt_p = (int)OPT_G_SEND;
      else if (foldyn && *foldyn=='1') *opt_p = (int)OPT_G_MAILF;
      else *opt_p = MENU_DO_OVER;
    strcpy(vui_date, intro[0].pdata);
    RealWhenIs("today", vui_date);
    intro[0].pdata = vui_date;
    editor=strchr(intro[0].pdata,' ');
    if (editor!=NIL) *editor='\0'; /* Only take the date part of the date/time */
    sprintf(CUI_VersionString,"VUI.%s.%d.%d",VUI_HOST, VUI_MAJORVERSION, VUI_MINORVERSION);
    CUI_SetClientVersion(CUI_VersionString);

    CUI_GetProfileString(ProgramName, "editor", VUI_editor, sizeof VUI_editor);
    if (*VUI_editor == '\0') {
	editor=getenv("EDITOR"); /* for backward compatibility */
	if (editor!=NIL && *editor) {
	    CUI_SetProfileString(ProgramName, "editor", editor);
	    strcpy(VUI_editor, editor);
	} else *VUI_editor = '\0';
    }
    SetEditorToUse(VUI_editor);

    BlindStatus = (CUI_GetProfileSwitch(ProgramName, "bcc", FALSE))?AMS_SEND_BLINDYES: AMS_SEND_BLINDNO;
    CUI_GetProfileString(ProgramName, "bccto", VUI_bccto, sizeof VUI_bccto);
    CUI_GetProfileString(ProgramName, "KeyHeaders", heads, sizeof heads);
    if (*heads != '\0') {
	if (CUI_GetProfileSwitch(ProgramName, "OmitKeys", FALSE))
	    OmitHeader(heads);
	else KeepHeader(heads);
    } else KeepHeader(DEFAULT_HEADERS);
    VUI_printer[0] = '\0';
    CUI_GetProfileString("print","printer",VUI_printer, sizeof VUI_printer);
    if (VUI_printer[0] == '\0') {
	CUI_GetProfileString("print","spooldir",VUI_printer, sizeof VUI_printer);
	if (VUI_printer[0]) VUI_UsePrintSpooldir = 1;
    }
    VUI_AlwaysPurge = (CUI_GetProfileSwitch(ProgramName, "AlwaysPurge", FALSE))?TRUE:FALSE;
    SaveVideoEnv (&vp, (int)(!nochange || *nochange != '1'));
    InitPanels (PanelArray, FieldArray);
    FixUpMenus();
    InitMenus (vui_menus);
    SetBaseMenu(vui_menus);
    GetMyHostname();
    Interactive=TRUE;
}

InitDirPanel(current_dir, substatus_p, mailorbb, datashown, manual, matchstr)
char *current_dir, *substatus_p, *matchstr;
int mailorbb, datashown, manual;
{
    if (InitDirData(current_dir, substatus_p, mailorbb, datashown, manual, matchstr)) {
        return(MENU_ERROR);
    }
    if (matchstr != NIL && *matchstr) {
	bboard_bp[0].pdata = "Bulletin Boards matching your search criteria";
	}
    else {
	switch(datashown) {
	    case (ALL):
		bboard_bp[0].pdata = "All Bulletin Boards you have access to";
		break;
	    case (SUBSCRIBED):
		bboard_bp[0].pdata = "All Bulletin Boards you subscribe to";
		break;
	    case (CHANGED):
		bboard_bp[0].pdata = "All Bulletin Boards with new messages";
	}
    }
    bboard_bp[0].pcol = 40 - (strlen(bboard_bp[0].pdata)>>1);
    bboard_bp[0].plen = strlen(bboard_bp[0].pdata);
    debug((3,"Heading started at %d for %d chars\n", bboard_bp[0].pcol, bboard_bp[0].plen));
    if ('\0' != *current_dir) return(0);
    switch (mailorbb) {
	case MAIL:
	    ReportError("You have no 'mail' folder.", ERR_WARNING, FALSE);
	    break;
        case FOLDER:
	    ReportError("You have no personal folders.", ERR_WARNING, FALSE);
	    break;
        case BBOARD:
	    if (NIL!=matchstr) {
		char ErrorText[256];
		sprintf(ErrorText,"No folders match '%s'.", matchstr);
		ReportError(ErrorText, ERR_WARNING, FALSE);
	    } else
	    switch (datashown) {
	        case SUBSCRIBED:
		    ReportError("You are not subscribed to any bboards.", ERR_WARNING, FALSE);
		    break;
		case ALL:
		    ReportError("No bboards are accessible. Check *.mspath in your Andrew preferences file.", ERR_WARNING, FALSE);
		    break;
		case CHANGED:
		    ReportError("No bboards have new messages.", ERR_WARNING, FALSE);
		    break;
	    }
    }
    return(MENU_EMPTY);
}

Dir_Panel (auto_dir, auto_date, mailorbb)
char *auto_dir, *auto_date;
int mailorbb;
{
int opt, datashown, rc, last_opt = 0, (*saverepaintfn)();
Boolean reshow = TRUE, quit = FALSE;
char *date = NIL, *matchstr = NIL, substatus, *savehelpfile, *reshowstr, matchbuf[81], dirnamebuf[MAXPATHLEN+1], *current_dir = dirnamebuf;

    debug((1,"Dir Panel(%s, %s, %d)\n",auto_dir, auto_date, mailorbb));
    saverepaintfn = VUI_RepaintFn;
    savehelpfile  = VUI_HelpFile;
    reshowstr = NIL;
    VUI_RepaintFn = ShowDirData;
    VUI_HelpFile = NIL;
    if (NIL != auto_dir) {
	datashown=ALL;
	matchstr = auto_dir;
    } else {
	datashown=(mailorbb!=BBOARD)?ALL:SUBSCRIBED;
	matchstr=NIL;
    }

    while (1) {
	rc=InitDirPanel(current_dir, &substatus, mailorbb, datashown, MANUAL, matchstr);
	if (rc == 0) break;
	if (rc == MENU_EMPTY && datashown == SUBSCRIBED) {
	    datashown = ALL;
	    continue;
	}
	quit = TRUE;
	break;
    }

    if (NIL != auto_dir && !rc) {
        if (!GetCurrentDir(current_dir, &substatus, FALSE) || !*current_dir) {
	    ReportError("The folder you requested does not exist.", ERR_WARNING, FALSE);
	    return(MENU_EMPTY);
	}
	rc=Msg_Panel(auto_date, current_dir, mailorbb, datashown==CHANGED);
	if (rc == -1) rc = MENU_EMPTY;
	quit = TRUE;
    }
    while (rc != MENU_DO_OVER && !quit) {
        if (reshow) {
           DrawPanel ((mailorbb!=BBOARD)?mail_bp:bboard_bp, NIL, PANEL_CLEAR);
	   ShowBar();
           ShowDirData();
           reshow=FALSE;
        }

	CheckForMail();
        opt=MenuInput((mailorbb!=BBOARD)?folder_menu:bb_menu, dir_keys, &last_opt);

        debug((1,"Dir Panel processing option %d\n",opt));
        if (opt<0) {
            rc=MENU_DO_OVER;
            quit=TRUE;
            break;
        }
        if (!GetCurrentDir(current_dir, &substatus, FALSE) || !*current_dir) {
	    if (opt != (int)OPT_U_NONE) {
		ShowDirData();  /* This means the folder went away or something */
		continue;
	    }
	    GetCurrentDirFromMap(current_dir); /* try getting it from MAP file */
	}
	    
        switch (opt) {
        /* ** Show options ** */
        case OPT_D_READ:
	    opt=Msg_Panel(date, current_dir, mailorbb, datashown==CHANGED);
	    if (opt==(int)OPT_C_NEXT)
		while (opt==(int)OPT_C_NEXT || opt==MENU_EMPTY) {
		    IncrDir(current_dir, &substatus);
		    if ((*current_dir=='\0') && (substatus == 'A'))
			substatus = '\0';
		    if (substatus=='A') {
			char msg_text[200], NickName[200];
			CUI_BuildNickName(current_dir, NickName);
			sprintf(msg_text,"Do you want to see new messages in %s?",NickName);
			if (!GetBooleanFromUser(msg_text, FALSE))
			    continue;
		    }
		    if ('\0'==*current_dir) {
			opt=MENU_EMPTY;
			reshow=TRUE;
			break;
		    }
		    opt=Msg_Panel(date, current_dir, mailorbb, datashown==CHANGED);
		}
	    if (opt==MENU_EMPTY) break;
	    if (opt==-1) opt = MENU_DO_OVER;
            if (opt!=MENU_DO_OVER) {
                quit=TRUE;
                rc=opt;
            }
            reshow=TRUE;
            break;

	/**************     Folder Commands     *****************/
        case OPT_D_ADD:
            rc=CreateNewMessageDirectory((char *)NULL);
        case OPT_D_MERGE:
            if (opt==(int)OPT_D_MERGE)  rc=MergeDirectories(current_dir);
        case OPT_D_RENAME:
            if (opt==(int)OPT_D_RENAME) rc=RenameDirectory(current_dir);
        case OPT_D_DELETE:
            if (opt==(int)OPT_D_DELETE) rc=RmMessageDirectory(current_dir);
            if (rc==0) {
                GetCurrentDir(current_dir, &substatus, FALSE);
                if (current_dir==NIL) {
                    rc=MENU_EMPTY;
                    quit = TRUE;
                } else
		    ShowDirData();
            }
            break;
        case OPT_D_INFO:
        case OPT_D_BRIEF:
        case OPT_D_LONG:
            GetDirInfo(current_dir, (opt==(int)OPT_D_LONG));
            break;
        case OPT_S_POST:
            opt=SendSomeMail(0, OPT_S_POST);
            if (opt==MENU_ERROR) {
                rc=opt;
                quit=TRUE;
            } else reshow=TRUE;
            break;

        /* ** Subscription Options ** */
        case OPT_U_NONE:
            rc=AMS_UNSUBSCRIBED;     reshowstr="None";
        case OPT_U_FULL:
            if (opt==(int)OPT_U_FULL) { rc=AMS_ALWAYSSUBSCRIBED; reshowstr="Full"; }
        case OPT_U_ASK:
            if (opt==(int)OPT_U_ASK ) { rc=AMS_ASKSUBSCRIBED;    reshowstr="Ask";  }
        case OPT_U_PRINT:
            if (opt==(int)OPT_U_PRINT){ rc=AMS_PRINTSUBSCRIBED;  reshowstr="Print";}
        case OPT_U_ALL:
            if (opt==(int)OPT_U_ALL) { rc=AMS_SHOWALLSUBSCRIBED; reshowstr="Showall";}
            if (!AlterSubscription(current_dir, rc))
                ReshowCurrentDirLine(NIL, reshowstr, TRUE);
            break;
	/************     Expose Commands     ************/
        case OPT_L_MATCH:
            matchstr=GetSUser("Enter string to start bboards:", NIL, NIL, matchbuf);
            if (matchstr==NIL) break;
	    datashown=ALL;
	    mailorbb=BBOARD;
	case OPT_L_CHANGED:
	    if (opt==(int)OPT_L_CHANGED) {
		if (datashown == CHANGED) {
		    ReportError("Already showing changed Bboards.", ERR_WARNING, FALSE);
		    break;
		} else {
		    datashown = CHANGED;
		    matchstr=NIL;
		    mailorbb = BBOARD;
		}
	    }
	case OPT_L_PERSONAL:
	    if (opt==(int)OPT_L_PERSONAL) {
		datashown = ALL;
		matchstr = NIL;
		mailorbb = FOLDER;
	    }
        case OPT_L_ALL:
	    if (opt==(int)OPT_L_ALL) {
		if (datashown == ALL) {
		    ReportError("Already showing all Bboards.", ERR_WARNING, FALSE);
		    break;
		} else {
		    datashown = ALL;
		    matchstr = NIL;
		    mailorbb = BBOARD;
		}
	    }

        case OPT_L_SUBSCR:
	    if (opt==(int)OPT_L_SUBSCR) {
		if (datashown == SUBSCRIBED) {
		    ReportError("Already showing subscribed Bboards.", ERR_WARNING, FALSE);
		    break;
		} else {
		    datashown = SUBSCRIBED;
		    matchstr = NIL;
		    mailorbb = BBOARD;
		}
	    }

	    rc=InitDirPanel(current_dir, &substatus, mailorbb, datashown, MANUAL, matchstr);
	    last_opt=0;
	    /* Unfortunately, if there weren't any matches, you lose big */
	    if (rc == MENU_EMPTY) {
		ConfirmUser("Returning to main menu. Press enter to continue.");
		rc = MENU_DO_OVER;
		quit = TRUE;
	    }
	    if (rc==MENU_ERROR) quit = TRUE;
              else reshow = TRUE;
            break;
	}
    }

    FreeDirData();
    CUI_FreeCaches();   /* Start the numbering over from 1 */
    VUI_RepaintFn = saverepaintfn;
    VUI_HelpFile = savehelpfile;
    return(rc);
}

#include <setjmp.h>
jmp_buf filepanel, filepanel2;
extern KEYTAB file_keys[];

DisplayMiniFile (filename,offset,start_row)
char *filename;
long offset;
int  start_row;
{
    int rc;
    debug((1,"Showing file %s",filename));

    if (InitFileData(filename, start_row, 1, 78,offset)) return(-1);
    ShowFileData();
    while (1) {
        rc=setjmp(filepanel);
        if (rc==-1) break;
        GetKey(file_keys, FALSE);
    }
    FreeFileData();
    return(0);
}

Logout_Panel ()
{
    int opt,last_opt = 0;

    opt = MenuInput(logout_menu, f1_keys, &last_opt);

    if (opt < 0)
	return (FALSE);

    switch (opt) {
	case OPT_L_LOGOUT:
	    return (LOGOUT_ALL);
	case OPT_L_MSSERVE:
	    return (LOGOUT_MSSERVER);
	case OPT_L_PCSERVE:
	    return (LOGOUT_PCSERVER);
    }

    return(FALSE);
}

Parms_Panel ()
{
int opt, last_opt = 0, rc, (*saverepaintfn)(), change = 0;
Boolean reshow = TRUE, quit = FALSE;
char *heads, *savehelpfile, headbuf[1024], *bccto;
    debug((1,"Parms Panel\n"));
    saverepaintfn = VUI_RepaintFn;
    savehelpfile  = VUI_HelpFile;
    VUI_RepaintFn = ShowParmData;
    VUI_HelpFile = NIL;
    rc = 0;
    while (!quit) {
        if (reshow) {
            InitParmData(parm_data, headbuf, 40);
            DrawPanel (sess_bp, NIL, PANEL_CLEAR);
	    ShowBar();
            DrawPanel (parm_data, NIL, PANEL_NOCLEAR);
            reshow=FALSE;
        }
	CheckForMail();
        opt=MenuInput(parms_menu, f1_keys, &last_opt);
        if (opt<0) {
            rc = MENU_DO_OVER;
            break;
        }
        switch (opt) {
        /* ** Header options ** */
        case OPT_H_KEEP:
            heads=GetSUser("Enter list of headers to keep:",
                     "To add headers to the list, start with '+'.", NIL, headbuf);
            if (heads==NIL) break;
            reshow=0==KeepHeader(heads);  /* Reshow if it went OK */
		    change |= 0x01;
            break;
        case OPT_H_OMIT:
            heads=GetSUser("Enter list of headers to omit:",
                     "To add headers to the list, start with '+'.", NIL, headbuf);
            if (heads==NIL) break;
            reshow=0==OmitHeader(heads);  /* Reshow if it went OK */
		    change |= 0x01;
            break;
        case OPT_H_ALL:
            HeadersOn=!HeadersOn;
		    change |= 0x01;
            reshow=TRUE;
            break;
		case OPT_H_DEFAULT:
		    reshow = 0==KeepHeader(DEFAULT_HEADERS);
		    change |= 0x01;
		    break;
        /* ** Blind copy options ** */
        case OPT_B_YES:
            reshow=TRUE;
            BlindStatus=AMS_SEND_BLINDYES;                      /* V1.3 MAC */
		    change |= 0x02;
            break;
        case OPT_B_NO:
            BlindStatus=AMS_SEND_BLINDNO;                       /* V1.3 MAC */
		    change |= 0x02;
            reshow=TRUE;
            break;
        case OPT_B_BCCTO:
            bccto=GetSUser("Enter folder name:", "This folder will receive copies of outgoing mail", NIL, VUI_bccto);
            if (bccto==NIL) break;
	    rc = 0;
	    if ((mserrcode = CUI_DisambiguateDir(bccto, &heads)) != 0) {
		if (AMS_ERRNO != ENOENT) {
		    ReportError(VUIMSG_FCHECK, ERR_WARNING, TRUE);
		    rc = -1;
		} else
		    if (GetBooleanFromUser("Folder does not exist. Do you wish to create it?", TRUE))
			rc=CreateNewMessageDirectory(bccto);
	    } else {
		int ProtCode, MsgCount;
		if (mserrcode = MS_GetDirInfo(heads, &ProtCode, &MsgCount)) {
		   ReportError("Cannot get information about folder.", ERR_WARNING, TRUE);
		   rc = -1;
		} else {
		   if (ProtCode != AMS_DIRPROT_MBOX && ProtCode != AMS_DIRPROT_FULLMAIL) {
		       ReportError("You do not have proper permission for that folder.", ERR_WARNING, FALSE);
		       rc = -1;
		   }
		}
	    }
	    if (rc == 0) {
		BlindStatus=AMS_SEND_BLINDYES;
		strcpy(VUI_bccto, bccto);
		reshow=TRUE;
		change |= 0x06;
	    }
            break;
        case OPT_P_EDIT:
            SetEditor();
            reshow=TRUE;
	    change |= 0x10;
            break;
	case OPT_P_PRINTER:
	    if (SetPrinter() == 0) {
		reshow=TRUE;
		change |= 0x20;
	    }
	    break;
	case OPT_P_PURGE:
	    VUI_AlwaysPurge = (VUI_AlwaysPurge)?FALSE:TRUE;
	    reshow = TRUE;
	    change |= 0x40;
	    break;
        }
    }

    /* For those preferences that span interfaces, * is used, where vui SPECIFIC preferences get 'vui'. The alternative is to ask if the user wants them for vui only, but why confuse them. */

    if (change && GetBooleanFromUser("Do you wish to make these changes permanent?", FALSE)) {
	if (change & 0x01) {
	    if (HeadersOn) {
		FormKeyHeaders(headbuf, sizeof headbuf);
		CUI_SetProfileString("*", "KeyHeaders", headbuf);
		CUI_SetProfileString("*", "OmitKeys", HeadKeep?"no":"yes");
	    } else {
		CUI_SetProfileString("*", "KeyHeaders", "<none>");
		CUI_SetProfileString("*", "OmitKeys", "yes");
	    }
	}
	if (change & 0x02)
	    CUI_SetProfileString("*", "bcc", (BlindStatus==AMS_SEND_BLINDYES)?"yes":"no");
	if (change & 0x04)
	    CUI_SetProfileString("*", "bccto", VUI_bccto);
	if (change & 0x10)
	    CUI_SetProfileString(ProgramName, "editor", VUI_editor);
	if (change & 0x20) {
	    if (VUI_UsePrintSpooldir)
	        CUI_SetProfileString("print", "spooldir", VUI_printer);
	    else
	        CUI_SetProfileString("print", "printer", VUI_printer);
	}
	if (change & 0x40)
	    CUI_SetProfileString(ProgramName,"AlwaysPurge",(VUI_AlwaysPurge)?"yes":"no");
	MS_ReInitialize();
    } else {
	if (change & 0x04)
	    ConfirmUser("Change of folder will only take effect if made permanent");
    }
    VUI_RepaintFn = saverepaintfn;
    VUI_HelpFile = savehelpfile;
    return(rc);
}

Msg_Panel (startdate, current_dir, mailorbb, autonext)
char *startdate, *current_dir;
int mailorbb;
Boolean autonext;
{
    int opt, dirtype, attr, mark_count;
    long msgno, numnew, numtotal, holdmsgno;
    int rc, last_opt = 0, (*saverepaintfn)(), access, count;
    Boolean reshow, quit, puntme;
    char *savehelpfile, LastOldDate[AMS_DATESIZE],*matchstr,matchbuf[81];

    debug((1,"Msg Panel(%s, %s, %d)\n",startdate, current_dir, dirtype));
    reshow = TRUE; quit = FALSE; puntme = FALSE; matchstr = NIL; attr = 0;

    if (MS_GetDirInfo(current_dir, &access, &count)) {
	ReportError("Unable to check your access rights on this folder.",ERR_WARNING,FALSE);
	return(-1);
    }
    if (access == AMS_DIRPROT_MBOX || access == AMS_DIRPROT_FULLMAIL) dirtype = MAIL;
    else if (access == AMS_DIRPROT_MODIFY) dirtype = FOLDER;
    else dirtype = BBOARD;

    MSG_mailorbb = dirtype;

    if (dirtype==MAIL) {
	/* don't do this everytime you get to a writeable bboard */
	if ((!autonext) || (VUI_Mailcount > 0))
	    CheckNewMessages(TRUE);
    }

    if (MS_GetNewMessageCount(current_dir, &numnew, &numtotal, LastOldDate, (long)TRUE/*Fetch, boy*/)) return(MENU_ERROR);
    if (numtotal == 0L) {
	ReportError("This folder has no messages.", ERR_WARNING, FALSE);
	return(MENU_EMPTY);
    }

    if (rc=InitMsgData(LastOldDate, numnew, numtotal, current_dir))
        return(rc);

    saverepaintfn = VUI_RepaintFn;  savehelpfile  = VUI_HelpFile;
    VUI_RepaintFn = ShowMsgData;    VUI_HelpFile = NIL;
    while (!quit) {
        if (reshow) {
	    DrawPanel (mess_bp, NIL, PANEL_CLEAR);
	    ShowBar();
            ShowMsgData();
            reshow=FALSE;
        }
	CheckForMail();
	if (mailorbb==MAIL && dirtype==MAIL)
	    opt = MenuInput(mmsg_menu,msg_keys,&last_opt);
	else
	    opt=MenuInput((dirtype!=BBOARD)?fmsg_menu:bmsg_menu, msg_keys,&last_opt);
        debug((1,"Msg Panel processing option %d\n",opt));
        if (opt<0) {
            rc=MENU_DO_OVER;
            quit=TRUE;
            break;
        }
        GetCurrentMsg(&msgno);
        switch (opt) {
        case OPT_C_READ:
	    if (MarkedMsgList) /* If the user has marked messages */
		opt = Body_Panel(MarkedMsgList->msgno, dirtype, TRUE, autonext);
	    else 
		opt=Body_Panel(msgno, dirtype, TRUE, autonext);
            if (opt == MENU_EMPTY) break;
            if (opt!=MENU_DO_OVER) {
		rc=opt;
		if (opt == (int)OPT_C_PUNT) {
		    puntme  = TRUE;
		    rc=(int)OPT_C_NEXT;
		}
                quit=TRUE;
            }
            reshow=TRUE;
            break;
        /* ** Reply options  ** */
	case OPT_C_RESEND: {
	    char newaddr_buf[81], *newaddr;
	    newaddr=GetSUser("Enter address to resend to:", NIL, NIL, newaddr_buf);
	    if (NIL==newaddr) break;
	    CUI_ResendMessage(CuidFromMsgno(msgno), newaddr);
	    break;
	    }
	case OPT_R_SENDER:
            rc=AMS_REPLY_SENDER;
        case OPT_C_RESTORE:
	    if (opt==(int)OPT_C_RESTORE) rc=AMS_REPLY_REDRAFT;
        case OPT_R_READER:
            if (opt==(int)OPT_R_READER)  rc=AMS_REPLY_WIDE;
        case OPT_R_ALL:
            if (opt==(int)OPT_R_ALL)     rc=AMS_REPLY_WIDER;
        case OPT_C_FORWARD:
            if (opt==(int)OPT_C_FORWARD) rc=AMS_REPLY_FORWARD;
        case OPT_C_SEND:
            if (opt==(int)OPT_C_SEND)    rc=AMS_REPLY_FRESH;
        case OPT_S_POST:
            if (opt==(int)OPT_S_POST)    rc=(int)OPT_S_POST;
            opt=SendSomeMail(CuidFromMsgno(msgno), rc);
            if (opt!=MENU_DO_OVER && opt!=MENU_EMPTY) {
                rc=opt;
                quit=TRUE;
            } else reshow=TRUE;
            break;
        case OPT_O_REMOTE:
	case OPT_C_PRINT:
	    if (MarkedMsgList) {
		msgno = -1;
		while((msgno = (long)GetNextMarkedMsg(msgno)) >= 0) {
		    rc = PrintBody(CuidFromMsgno(msgno));
		    if (rc) break;
		}
		AskAndRemoveMarks(TRUE);
	    } else
		PrintBody(CuidFromMsgno(msgno));
            break;
        case OPT_C_DELETE:
	    if (MarkedMsgList) {
		msgno = -1;
		while((msgno = (long)GetNextMarkedMsg(msgno)) >= 0) {
		    rc = DeleteMessage(msgno, TRUE);
		    if (rc) break;
		}
		AskAndRemoveMarks(TRUE);
	    } else
		DeleteMessage(msgno, TRUE);
            break;
        /* ** Classify options  ** */
        case OPT_Y_MOVE:
            attr = MS_CLONE_COPYDEL;
        case OPT_Y_COPY:
            if (opt==(int)OPT_Y_COPY) attr = MS_CLONE_COPY;
        case OPT_Y_APPEND:
            if (opt==(int)OPT_Y_APPEND) attr = MS_CLONE_APPEND;
	    if (MarkedMsgList) {
		msgno = -1; mark_count = 0;
		while((msgno = (long)GetNextMarkedMsg(msgno)) >= 0) {
		    rc = CloneMessage(msgno, attr, TRUE, !mark_count); 
		    if (rc) break;
		    mark_count++;
		}
		AskAndRemoveMarks(TRUE);
	    } else
		CloneMessage(msgno, attr, TRUE, TRUE);
            break;
        case OPT_D_REMOTE:
        case OPT_D_LOCAL:
        case OPT_O_LOCAL:
            StoreMessage(CuidFromMsgno(msgno), opt, HeadersOn);
            break;
	case OPT_C_SETNEW:
	    SetUpdatemsgno(msgno, dirtype==BBOARD, TRUE, TRUE);
	    break;
	case OPT_S_SINCE:
	case OPT_S_TOPSET:
	    if (0<=(holdmsgno=SetTopOfPage(opt==(int)OPT_S_TOPSET))) {
		msgno=holdmsgno;
		reshow=TRUE;
	    }
	    break;
	case OPT_D_SEARCH:
	    matchstr = GetSUser("Enter string to match:",NIL,NIL,matchbuf);
	    if (0<=(holdmsgno = SetTopOfPageAndSearch(matchstr))) {
		    msgno = holdmsgno;
		    reshow = TRUE;
	    }
	    break;
	case OPT_C_NEWMAIL:
	    UpdateProf(puntme);
	    if (CheckNewMessages(TRUE)==0) {
		if (MS_GetNewMessageCount(current_dir, &numnew,	&numtotal, LastOldDate, (long)TRUE/*Fetch, boy*/))
		    ReportError("Unable to get new message count.",ERR_WARNING,FALSE);
		else if (numtotal == 0L || numnew == 0L)
		    ReportError("No new messages for this folder.", ERR_WARNING, FALSE);
		else if (numnew > 0L) {
		    InitMsgData(LastOldDate, numnew, numtotal, current_dir);
		    reshow = TRUE;
		}
	    }
	    break;
	case OPT_C_MARK:
	    if (ToggleMessageMark(msgno, TRUE, FALSE) == TRUE)
		LinktoMarkList(msgno);
	    else DelinkFromMarkList(msgno);
	    break;
	case OPT_C_PUNT:
	case OPT_C_NEXT:
	    puntme = opt==(int)OPT_C_PUNT;
	    rc=(int)OPT_C_NEXT;
	    quit=TRUE;
	    break;
	}
    }
    UpdateProf(puntme);
    PurgeCurrentDir();
    FreeMsgData();
    FreeMarkList();
    VUI_RepaintFn = saverepaintfn;
    VUI_HelpFile = savehelpfile;
    return(rc);
}


Body_Panel (msgno, dirtype, show_splat, autonext)
long msgno;
int dirtype;
Boolean show_splat, autonext;
{
    Boolean reshow = TRUE, quit = FALSE, reading_marked;
    int save_headers_state, rc, opt, last_opt = 0, (*saverepaintfn)();
    char *savehelpfile;
    long newmsgno, updated_msgno = -1, next_marked_msg = -1;

    debug((1,"Body Panel(%ld, %d)\n",msgno, dirtype));
    save_headers_state=HeadersOn;
    if (rc=InitBodyData(msgno, HeadersOn)) return(rc);

    if (MarkedMsgList) reading_marked = TRUE;
    else reading_marked = FALSE;

    saverepaintfn = VUI_RepaintFn;
    savehelpfile  = VUI_HelpFile;
    VUI_RepaintFn = ShowBodyData;
    VUI_HelpFile = NIL;
    while (!quit) {
        if (reshow) {
            ShowBodyData();
            reshow=FALSE;
        }
	CheckForMail();
        opt=MenuInput(((dirtype!=BBOARD)?mbody_menu:bbody_menu),body_keys, &last_opt);
        if (opt<0) {
            rc=MENU_DO_OVER;
            break;
        }
        switch (opt) {
	    case OPT_C_HEADS:
		FreeBodyData();
		if (rc=InitBodyData(msgno, !HeadersOn)) {
		    if (rc != MENU_EMPTY) {
			rc=MENU_ERROR;
			goto cleanup;
		    }
		}
		HeadersOn= !HeadersOn;
		reshow=TRUE;
		break;

	    case OPT_C_RESEND: {
		char newaddr_buf[81], *newaddr;
		newaddr=GetSUser("Enter address to resend to:", NIL, NIL, newaddr_buf);
		if (NIL==newaddr) break;
		CUI_ResendMessage(CuidFromMsgno(msgno), newaddr);
		break;
		}
	    case OPT_R_SENDER:
		rc=AMS_REPLY_SENDER;
	    case OPT_C_RESTORE:
		if (opt==(int)OPT_C_RESTORE) rc=AMS_REPLY_REDRAFT;
	    case OPT_R_READER:
		if (opt==(int)OPT_R_READER)  rc=AMS_REPLY_WIDE;
	    case OPT_R_ALL:
		if (opt==(int)OPT_R_ALL)     rc=AMS_REPLY_WIDER;
	    case OPT_C_FORWARD:
		if (opt==(int)OPT_C_FORWARD) rc=AMS_REPLY_FORWARD;
	    case OPT_C_SEND:
		if (opt==(int)OPT_C_SEND)    rc=AMS_REPLY_FRESH;
		opt=SendSomeMail(CuidFromMsgno(msgno), rc);
		if (opt!=MENU_DO_OVER && opt!=MENU_EMPTY) {
		    rc=opt;
		    quit=TRUE;
		} else reshow=TRUE;
		break;

	    case OPT_O_REMOTE:
	    case OPT_C_PRINT:
		PrintBody(CuidFromMsgno(msgno));
		break;
	    case OPT_C_DELETE:
		DeleteMessage(msgno, FALSE);
		break;

	    case OPT_Y_MOVE:
		rc=MS_CLONE_COPYDEL;
	    case OPT_Y_COPY:
		if (opt==(int)OPT_Y_COPY) rc=MS_CLONE_COPY;
	    case OPT_Y_APPEND:
		if (opt==(int)OPT_Y_APPEND) rc=MS_CLONE_APPEND;
		CloneMessage(msgno, rc, FALSE, TRUE);
		break;
	    case OPT_S_POST:
		opt=SendSomeMail(0, OPT_S_POST);
		if (opt!=MENU_DO_OVER && opt!=MENU_EMPTY) {
		    rc=opt;
		    quit=TRUE;
		} else reshow=TRUE;
		break;
	    case OPT_D_REMOTE:
	    case OPT_D_LOCAL:
	    case OPT_O_LOCAL:
		StoreMessage(CuidFromMsgno(msgno), opt, HeadersOn);
		break;

	    case OPT_C_MARK:
		if (ToggleMessageMark(msgno, FALSE, FALSE))
		    LinktoMarkList(msgno);
		else { /* get the next to read since we are delinking the current message */
		    next_marked_msg = (long)GetNextMarkedMsg(msgno);
		    DelinkFromMarkList(msgno);
		}
		break;

	    case OPT_G_NEXTM:
		if (updated_msgno != msgno) {
		    UpdateMsgData(msgno, show_splat);
		    updated_msgno = msgno;
		}
		if (reading_marked) {
		    if (next_marked_msg >= 0) /* did he unmark the one he was reading? */
			newmsgno = next_marked_msg;
		    else
			newmsgno = (long)GetNextMarkedMsg(msgno);
		    next_marked_msg = -1;
		}
		else
		    IncrMsg(&newmsgno);

		if (newmsgno>=0) {
		    msgno=newmsgno;
		    FreeBodyData();
		    if (InitBodyData(msgno, HeadersOn))  {
			rc=MENU_DO_OVER; 
			goto cleanup;
		    }
		    reshow=TRUE;
		} else {
		    if (autonext) {
			rc=opt;
			quit=TRUE;
		    } else {
			if (reading_marked) {
			    ReportError("No more marked messages.", ERR_WARNING, FALSE);
			    AskAndRemoveMarks(FALSE);
			}
			else
			    ReportError("No more messages.", ERR_WARNING, FALSE);
			rc = MENU_DO_OVER;
			quit = TRUE;
		    }
		}
		break;
	    case OPT_C_PUNT:
		rc=opt;
		quit=TRUE;
   		break;
	}
    }
    HeadersOn=save_headers_state;
    /* A hack to avoid Update twice since we did it if nextm */
    if (rc==(int)OPT_G_NEXTM)
	rc=(int)OPT_C_NEXT;
    else /* well the hook didn't work right, so here is another one */
	if (updated_msgno != msgno) 
	    UpdateMsgData(msgno, show_splat);

cleanup:
    FreeBodyData();
    VUI_RepaintFn = saverepaintfn;
    VUI_HelpFile = savehelpfile;
    return(rc);
}

int AskAndRemoveMarks(visible)
Boolean visible;
{
    MARKED_MSGS *linkp;

    if (!MarkedMsgList) return(0);

    if (GetBooleanFromUser("Mark operation done. Remove marks?", TRUE)) {
	linkp = MarkedMsgList;
	while(linkp) {
	    ToggleMessageMark(linkp->msgno, visible, TRUE);
	    linkp = linkp->next;
	}
	FreeMarkList();
	return(0);
    }
    return(-1);
}

HelpCmd ()
{
   printf("  ** Visual User Interface - Full screen access to the Andrew Message System **\n\
  vui [ -m | -p <name> '<date>' | -b <name> '<date>' | -n | -s | -f | -c]\n\
       -m       Process mail.\n\
       -c       Use larger screen size for EGA/VGA (43 or 50 by 80)\n");
   printf(
"       -p       Read personal folders.\n\
       -b       Read bulletin boards.\n\
       -n       Check for new messages on all subscriptions.\n\
       -s       Send a message.\n");
   printf(
"     <name>     The name of a folder or bboard you want to read.\n\
     <date>     A date to see messages since.  If you enter more\n\
                than one word use quotes (i.e. '7 days ago').\n");
}

SetTerminalParams (h, w)
int h, w;
{
}
