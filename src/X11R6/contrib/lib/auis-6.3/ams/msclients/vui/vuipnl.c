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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/vuipnl.c,v 1.51 1994/03/29 04:13:54 rr2b Exp $";
#endif

/* 
 *      This module defines, and responds to input from, the
 *      main VUI panel.
 */

#include <andrewos.h>
#include <ctype.h>
#include <util.h>
#include <ams.h>
#define VUI_SOURCE_VUIPNL_C
#include <vui.h>
#include <hdrparse.h>
#include <panel.h>
#include <lmenus.h>
#include <fcntl.h>
#include <keycodes.h>
#include <vuidebug.h>

extern char *GetSUser();

extern long CUI_DisambiguateDir(), CUI_CacheDirName();
extern long MS_GetPartialFile(), MS_GetNthSnapshot(), MS_WriteUnscribedBodyFile(), MS_SetAssociatedTime(), MS_UpdateState(), MS_UnlinkFile(), MS_DisambiguateFile(), MS_ParseDate();

extern int Interactive, CUIDebugging, CUI_SnapIsRunning, (*VUI_RepaintFn)();
extern FILE *debugfile;
extern int current_option;
extern MENU_TREE *current_menu_tree;
extern PANEL sess_bp[], parm_data[];
extern unsigned char bar_row;
extern int HeadersOn, BlindStatus;
extern Boolean HeadKeep;
extern Boolean MSG_mailorbb;
extern struct head_list *HeadList;
extern char VUI_editor[], VUI_bccto[], VUI_printer[], RunningOnHost[];
extern Boolean VUI_AlwaysPurge;
char sep_line[] = {
 0315,0315,0315,0315,0315,0315,0315,0315,0315,0315,
 0315,0315,0315,0315,0315,0315,0315,0315,0315,0315,
 0315,0315,0315,0315,0315,0315,0315,0315,0315,0315,
 0315,0315,0315,0315,0315,0315,0315,0315,0315,0315,
 0315,0315,0315,0315,0315,0315,0315,0315,0315,0315,
 0315,0315,0315,0315,0315,0315,0315,0315,0315,0315,
 0315,0315,0315,0315,0315,0315,0315,0315,0315,0315,
 0315,0315,0315,0315,0315,0315,0315,0315,0315,0315,
 0 
 };

ShowParmData ()
{
int i;

    for (i=9; i<bar_row; i++) ClearLine(i);
    DrawPanel (sess_bp, NIL, PANEL_NOCLEAR);
    ShowBar();
    DrawPanel (parm_data, NIL, PANEL_NOCLEAR);
}

ShowBar()
{
    ShowString(sep_line, bar_row, 0, LINE_LENGTH, HILITE);

    if (CUI_SnapIsRunning) {
	GetMyHostname();
	ShowString(RunningOnHost, bar_row, LINE_LENGTH-strlen(RunningOnHost),
		   strlen(RunningOnHost), RVIDEO);
    }

}


/* *****************************************************************
 *
 *    Directory data handling routines
 *
 ***************************************************************** */
#define N_DIR_FIELDS 2          /* number of fields per line  */
PRIVATE int DIR_CurrentRow, DIR_MaxVisible, DIR_suborall,
	    DIR_mailorbb, DIR_max_row;
PRIVATE Boolean DIR_LastPageFlag, DIR_AutoorMan;
PRIVATE char *DIR_PageBuf = NIL, DIR_template[1+LINE_LENGTH] = { '\0' } ;
PRIVATE struct dir_page_list *DIR_FirstPage = (struct dir_page_list *)NIL, *DIR_CurrentPage = (struct dir_page_list *)NIL;
PANEL *dir_data;

int DirPageUpKey (curfield, maxopts)
int curfield, maxopts;
{
/*    char ErrorText[LINE_LENGTH]; */
    debug((2,"DirPageUpKey\n"));
    if ((struct dir_page_list *)NIL==DIR_CurrentPage->prevpage) {
        ReportError(VUIMSG_NMFOLDER, ERR_WARNING, FALSE);
    } else {
        EraseMenuCursor();
        if (!LoadOneDirPage(DIR_CurrentPage->prevpage, DIR_mailorbb,
                          DIR_suborall, DIR_template, &DIR_MaxVisible, NIL)) {
            DIR_CurrentPage=DIR_CurrentPage->prevpage;
            DIR_CurrentRow=0;
            ShowDirData();
        }
        ShowMenuCursor();
    }
    return(curfield);
}

int DirPageDnKey (curfield, maxopts)
int curfield, maxopts;
{       /* NOTE: when calling from program, maxopts==-1 will avoid display */
    int save_maxvisible, rc;
    struct dir_page_list *newpage;
    char ErrorText[LINE_LENGTH];
    debug((2,"DirPageDnKey\n"));
    if (DIR_MaxVisible<DIR_max_row) {
        DIR_LastPageFlag=TRUE;
        ReportError(VUIMSG_NMFOLDER, ERR_WARNING, FALSE);
        return(curfield);
    }
    if (maxopts>=0)  {
        EraseMenuCursor();
        sprintf(ErrorText,"Loading new page of %s...",
                          (DIR_mailorbb!=BBOARD)?"folders":"bboards");
        ReportSuccess(ErrorText);
    }
    rc=LoadOneDirPage(DIR_CurrentPage->nextpage, DIR_mailorbb, DIR_suborall,
                      DIR_template, &save_maxvisible, &newpage);
    if (!rc && save_maxvisible>=0) {
        if ((struct dir_page_list *)NIL==DIR_CurrentPage->nextpage) {
            DIR_CurrentPage->nextpage=newpage;
            newpage->prevpage=DIR_CurrentPage;
            DIR_CurrentPage=newpage;
        } else DIR_CurrentPage=DIR_CurrentPage->nextpage;
        DIR_MaxVisible=save_maxvisible;
	DIR_CurrentRow= 0;
        if (maxopts>=0) {
            ShowDirData();
            ClearError();
        }
    } else {
        DIR_LastPageFlag=TRUE;
        if (!rc) {
            ReportError(VUIMSG_NMFOLDER, ERR_WARNING, FALSE);
        }
    }
    if (maxopts>=0) ShowMenuCursor();
    return(curfield);
    }

int DirMovement (curfield, maxopts,how)
int curfield, maxopts;
Boolean how;
    {
    register int row;
    debug((2,"DirMovement %d\n",(int)how));
    row=N_DIR_FIELDS*DIR_CurrentRow;
    ShowString(dir_data[row].pdata, dir_data[row].prow,
               dir_data[row].pcol,  dir_data[row].plen,
               NORMAL);
    row++;
    ShowString(dir_data[row].pdata, dir_data[row].prow,
               dir_data[row].pcol,  dir_data[row].plen,
               NORMAL);
    if (how == DIR_UP_ARROW) {
	if (DIR_CurrentRow==0)
	    DIR_CurrentRow=DIR_MaxVisible;
	else DIR_CurrentRow--;
    }
    else if (how == DIR_DOWN_ARROW) {
	if (DIR_CurrentRow==DIR_MaxVisible)
	    DIR_CurrentRow=0;
	else DIR_CurrentRow++;
    }
    else if (how == DIR_HOME_KEY) {
	DIR_CurrentRow=0;
    }
    else { /* DIR_END_KEY */
	DIR_CurrentRow=DIR_MaxVisible;
    }
    row=N_DIR_FIELDS*DIR_CurrentRow;
    ShowString(dir_data[row].pdata, dir_data[row].prow,
               dir_data[row].pcol,  dir_data[row].plen,
               RVIDEO);
    row++;
    ShowString(dir_data[row].pdata, dir_data[row].prow,
               dir_data[row].pcol,  dir_data[row].plen,
               RVIDEO);
    return(curfield);
    }

int DirUpArrow (curfield, maxopts)
int curfield,maxopts;
{
    return(DirMovement(curfield,maxopts,DIR_UP_ARROW));
}

int DirDnArrow (curfield, maxopts)
int curfield, maxopts;
    {
    return(DirMovement(curfield,maxopts,DIR_DOWN_ARROW));
    }

int DirHomeKey (curfield, maxopts)
int curfield, maxopts;
    {
    return(DirMovement(curfield,maxopts,DIR_HOME_KEY));
    }

int DirEndKey (curfield, maxopts)
int curfield, maxopts;
    {
    return(DirMovement(curfield,maxopts,DIR_END_KEY));
}

InitDirData (current_dir, substatus, mailorbb, suborall, autoorman, matchstr)
char *current_dir, *matchstr, *substatus;
int mailorbb, suborall;
Boolean autoorman;
{
    struct dir_page_list *newpage;
    int n_seen, i;
    PANEL *pp;

    debug((1, "InitDirData(%ld, %d, %d, %s)\n", current_dir, mailorbb, suborall, matchstr));
    DIR_max_row = bar_row - 3;
    debug((2, "DIR_max_row is set to %d\n", DIR_max_row));
    if (DIR_PageBuf == NIL) {
	DIR_PageBuf = (char *)malloc(bar_row * (LINE_LENGTH + 2));
	debug((2, "DIR_PageBuf allocated at %ld\n", DIR_PageBuf));
    }
    if (DIR_PageBuf == NIL) {
	ReportError(VUIMSG_NOMEM, ERR_FATAL, FALSE);
	*current_dir = '\0';;
	return(MENU_ERROR);
    }
    if (dir_data == (PANEL *) NIL) {
	dir_data = (PANEL *) malloc(((DIR_max_row+1)*N_DIR_FIELDS+1)*sizeof(PANEL));
	debug((2, "dir_data allocated at %ld\n", dir_data));
    }
    if (dir_data == (PANEL *) NIL) {
	ReportError(VUIMSG_NOMEM, ERR_FATAL, FALSE);
	*current_dir = '\0';
	return(MENU_ERROR);
    }
    for (i=2, pp=dir_data; i<bar_row; i++) {
	pp->prow = i;	pp->pcol = 0;	pp->pattr = NORMAL;
	pp->plen = 65;	pp->pdata = NIL;    pp++;
	pp->prow = i;	pp->pcol = 65;	pp->pattr = NORMAL;
	pp->plen = 15;	pp->pdata = NIL;    pp++;
    }
    pp->plen = 0; /* NULL PANEL */
    ClearMapFileNames();
    *current_dir = '\0'; /* set the empty flag */
    if (LoadOneDirPage(NIL, mailorbb, suborall, matchstr, &n_seen, &newpage)) {
	/*  FreeDirData();  this gets done up higher */
	*current_dir = '\0';
	return(MENU_EMPTY);
    }
    if (n_seen < 0) {
	DIR_LastPageFlag = TRUE;
	*current_dir = '\0';
    }
    else {
	DIR_MaxVisible = n_seen;
	DIR_CurrentRow = 0;
	DIR_suborall = suborall;
	if (matchstr != NIL && *matchstr) {
	    debug((2, "Only showing folders that start with '%s'\n", matchstr));
	    strncpy(DIR_template, matchstr, 80);
	} else *DIR_template = '\0';
	DIR_LastPageFlag = FALSE;
	DIR_mailorbb = mailorbb;
	DIR_CurrentPage = newpage;
	DIR_FirstPage = newpage;
	DIR_AutoorMan = autoorman;
	strcpy(current_dir, dir_data[N_DIR_FIELDS * DIR_CurrentRow].pdata);
	(*substatus)=dir_data[1+N_DIR_FIELDS*DIR_CurrentRow].pdata[0]; 
    }
    return(0);
}

PRIVATE LoadOneDirPage (this_page, mailorbb, suborall, matchstr,
                        n_seen_p, new_page_p)
struct dir_page_list *this_page, **new_page_p;
int mailorbb, suborall, *n_seen_p;
char *matchstr;
{
    char MapFile[MAXPATHLEN+1],/* ErrorText[256], */
    FileBuf[MAXBODY],
    *s, *subscr, *shortname, *longname, *nextline, *PageBufPtr;
    int  i=0, substatus, bodylen, line_count, row, maxlen;
    long offset, bytesunfetched;
    struct dir_page_list *new_page;

    *n_seen_p = -1; subscr = NIL;
    PageBufPtr = DIR_PageBuf;
    debug((1,"LoadOneDirPage(%ld, %d, %d, %ld)\n",this_page, mailorbb, suborall,
	    new_page_p));

    if (this_page==(struct dir_page_list *)NIL) {
	debug((2,"Creating new page list entry.\n"));
	this_page=(struct dir_page_list *)malloc(sizeof(struct dir_page_list));
	if (this_page==(struct dir_page_list *)NIL) {
	    ReportError(VUIMSG_ALLOCPAGE, ERR_CRITICAL, FALSE);
	    return(-1);
	}
	this_page->path_entry=(mailorbb!=BBOARD)?AMS_MAILPATH:0;
	this_page->map_offset=0;
	this_page->nextpage=(struct dir_page_list *) NIL;
	this_page->prevpage=(struct dir_page_list *) NIL;
    }
    if (new_page_p!=(struct dir_page_list **)NIL) (*new_page_p)=this_page;
    i=this_page->path_entry;
    line_count= -1;
    offset=this_page->map_offset;
    bytesunfetched=0;
    debug((2,"Starting page at entry %d offset %ld.\n", i, offset));

    do {
	if (GenMapFileName(suborall, mailorbb, MapFile, &i))
	    return(-1);
	if (!*MapFile) break;
	debug((2,"Subscription map put in %s\n", MapFile));
	do {
	    if ((mserrcode = MS_GetPartialFile(MapFile,	FileBuf, MAXBODY - 1, offset,	&bytesunfetched, &bodylen)) != 0)  {
		ReportError(VUIMSG_MAPFILE, ERR_CRITICAL, TRUE);
		break;
	    }
	    debug((2,"We opened	the file and read %d bytes starting at %ld.\n",	bodylen,    offset));
	    if (bodylen <= 0)       /* We hit the end of this file so try next */
		break;
	    FileBuf[bodylen] = '\0';/* Make sure null-terminated */
	    for (s=FileBuf; *s; s=nextline) {
		nextline=strchr(s,'\n');
		if (nextline==NIL) break;
		*nextline++ = '\0';
		offset += nextline - s;       /* Move pointer of where we've been */
		debug((9,"Parsing %s\n",s));
		/* ***** Temp patch to workaround MS bug on deleting folders *** */
		if (*s == ':') continue;
		longname=strchr(s,':');
		if (longname!=NIL) *longname++ = '\0';
		else              longname=s;
		shortname=s;
		s=strchr(longname,' ');
		if (s!=NIL) {
		    *s++ = '\0';
		    substatus=atoi(s);
		} else substatus=AMS_UNSUBSCRIBED;


		/* Keep 'mail' out of the list of bboard folders  */
		if (strcmp(shortname, AMS_DEFAULTMAILDIR) == 0) {
		    if (mailorbb == BBOARD) continue;
		} else if (mailorbb == MAIL) /* we want mail and this is not it */
		    continue;
/*
		if ((mailorbb == BBOARD) &&  !strcmp(shortname, AMS_DEFAULTMAILDIR))
		    continue;
*/		
		if ((matchstr!=NIL) && (*matchstr)) {
		    Boolean nomatch;
		    char *matchptr;
		    nomatch = TRUE;
		    matchptr = shortname;
		    while ((strlen(matchstr) <= strlen(matchptr)) && (nomatch)) {
			if (strncmp(matchstr,matchptr,strlen(matchstr)) == 0) nomatch = FALSE;
			matchptr++;
		    }
		    if (nomatch) {
			debug((2,"No match on string.\n"));
			continue;
		    }
		}

		switch(substatus) {
		    case AMS_ASKSUBSCRIBED:
			subscr="Ask"; break;
		    case AMS_ALWAYSSUBSCRIBED: 
			subscr="Full"; break;
		    case AMS_UNSUBSCRIBED:
			subscr="None"; break;
		    case AMS_PRINTSUBSCRIBED:
			subscr="Print"; break;
		    case AMS_SHOWALLSUBSCRIBED:
			subscr="Showall"; break;
		}
		if ((suborall==ALL) || substatus!=AMS_UNSUBSCRIBED) {
		    line_count++;
		    debug((2,"Adding %s (%s) to row %d\n", shortname, subscr, line_count));
		    row=N_DIR_FIELDS*line_count;
		    dir_data[row].pdata=PageBufPtr;
		    maxlen=dir_data[row].plen;
		    while (maxlen--) {
			*PageBufPtr++ = *shortname;
			if (*shortname) shortname++;
		    }
		    *PageBufPtr++='\0';
		    dir_data[1+row].pdata=PageBufPtr;
		    maxlen=dir_data[1+row].plen;
		    while (maxlen--) {
			if (*subscr) *PageBufPtr++ = *subscr++;
			else *PageBufPtr++=' ';
		    }
		    *PageBufPtr++='\0';

		    if (DIR_max_row==line_count) break;
		}
	    }  /* End for */
	} while (bytesunfetched>0 && line_count<DIR_max_row);
	
	/* reset offset to 0 when we go to next entry */
	if (line_count<DIR_max_row) {
	    offset=0;
	    if (mailorbb!=BBOARD) break;
	    ++i;
	}
    } while (line_count<DIR_max_row && suborall == ALL);

    *n_seen_p = line_count++;
    if (*n_seen_p<0) return(0);
    *(++PageBufPtr)='\0';
    while (line_count<=DIR_max_row) {
	dir_data[N_DIR_FIELDS*line_count].pdata=PageBufPtr;
	dir_data[1+N_DIR_FIELDS*line_count++].pdata=PageBufPtr;
    }
    debug((2,"Now we have %d lines visible\n",*n_seen_p));
    if ((struct dir_page_list *)NIL==this_page->nextpage) {
	new_page=(struct dir_page_list *)malloc(sizeof(struct dir_page_list));
	if (new_page==(struct dir_page_list *)NIL) {
	    ReportError(VUIMSG_ALLOCPAGE, ERR_CRITICAL, FALSE);
	    return(-1);
	}
	this_page->nextpage=new_page;
	new_page->prevpage=this_page;
	new_page->path_entry=i;
	new_page->map_offset=offset;
	new_page->nextpage=(struct dir_page_list *)NIL;
	debug((2,"Next page (addr %ld) will be at entry	%d, offset %ld\n", new_page, i,	    offset));
    }
    /* *ClearError(); ****** */
    return(0);
}

IncrDir (current_dir, substatus)
char *current_dir, *substatus;
{
   /* Goto next directory  */
   if (DIR_CurrentRow<DIR_MaxVisible) {
       DIR_CurrentRow++;
       GetCurrentDir(current_dir, substatus, FALSE);
   } else {
       DirPageDnKey(0,-1);
       if (DIR_LastPageFlag) *current_dir = '\0';
         else GetCurrentDir(current_dir, substatus, FALSE);
   }
   debug((2,"Incremented directory to %s at row %d",
           (*current_dir)?current_dir:"NONE",DIR_CurrentRow));
}

GetCurrentDir (current_dir, status, wantshort)
char *current_dir, *status;
Boolean wantshort;
{
    char long_name[MAXPATHLEN+1], *long_name_p = long_name, *shortname;
    shortname = dir_data[N_DIR_FIELDS * DIR_CurrentRow].pdata;
    *current_dir = '\0';
    if (wantshort) {
	strcpy(current_dir, shortname);
	debug((2,"Returning current directory of %s\n", shortname));
	return(0);
    }
    if ((mserrcode = CUI_DisambiguateDir(shortname, &long_name_p)) != 0 ) {
	if (AMS_ERRNO == ENOENT) {
	    long_name_p = long_name;
	    GetLongNameFromSubsMap(shortname, long_name_p, DIR_CurrentPage);
	    if (CUI_HandleMissingFolder(long_name_p)) {
		ReportError("Sorry; that folder is no longer readable.", ERR_WARNING, FALSE);
	    }
	} else ReportError(VUIMSG_FCHECK, ERR_WARNING, TRUE);
	return(MENU_EMPTY);
    }
    CUI_CacheDirName(shortname, long_name_p);
    strcpy(current_dir, long_name_p);
    debug((2,"Returning current directory of %s\n", current_dir));
    if (status!=NIL) (*status)=dir_data[1+N_DIR_FIELDS*DIR_CurrentRow].pdata[0];
    return 1;
}

GetCurrentDirFromMap (current_dir)
char *current_dir;
{
    char long_name[MAXPATHLEN+1],*long_name_p, status;

    GetCurrentDir(current_dir,&status,TRUE); /* get the short name */ 
    long_name_p = long_name;
    long_name[0] = '\0';
    GetLongNameFromSubsMap(current_dir,long_name_p, DIR_CurrentPage);
    strcpy(current_dir, long_name);
}
    
ShowDirData ()
{
    register int row;
    debug((1,"ShowDirData\n"));
    debug((1,"dir_data = %d, DIR_CurrentRow = %d\n", dir_data, DIR_CurrentRow));
    row=N_DIR_FIELDS*DIR_CurrentRow;
    dir_data[row].pattr=RVIDEO;
    dir_data[1+row].pattr=RVIDEO;
    DrawPanel(dir_data, NIL, PANEL_NOCLEAR);
    dir_data[row].pattr=NORMAL;
    dir_data[1+row].pattr=NORMAL;
}

ReshowCurrentDirLine (dir_name, status, display)
char *dir_name, *status;
Boolean display;
{
    int maxlen, row;
    char *p;
    debug((1,"ReshowCurrentDirLine %d to %s %s\n", DIR_CurrentRow, dir_name, status));
    row=N_DIR_FIELDS*DIR_CurrentRow;
    debug((2,"dir name was %s(%d)\n", dir_data[row].pdata, dir_data[row].plen));
    if (dir_name!=NIL) {
        p=dir_data[row].pdata;
        maxlen=dir_data[row].plen;
        while (maxlen--) {
           *p++ = *dir_name;
           if (*dir_name) dir_name++;
        }
        if (display) ShowString(dir_data[row].pdata, dir_data[row].prow,
                                dir_data[row].pcol,  dir_data[row].plen,
                                RVIDEO);
    }
    row++;
    debug((2,"dir stat was %s(%d)\n", dir_data[row].pdata, dir_data[row].plen));
    if (status!=NIL) {
        p=dir_data[row].pdata;
        maxlen=dir_data[row].plen;
        while (maxlen--) {
           *p++ = *status;
           if (*status) status++;
        }
   /* **  *p='\0';  *** */
        if (display) ShowString(dir_data[row].pdata, dir_data[row].prow,
                                dir_data[row].pcol,  dir_data[row].plen,
                                RVIDEO);
    }
    debug((2,"dir name is %s(%d)\n", dir_data[row-1].pdata,dir_data[row-1].plen));
    debug((2,"dir stat is %s\n", dir_data[row].pdata, dir_data[row].plen));
    UpdateScreen();
}

FreeDirData ()
{
    struct dir_page_list *p, *q;

    debug((1, "FreeDirData"));
    if (DIR_FirstPage == (struct dir_page_list *)NIL) return;
    for (p=DIR_FirstPage; p!=(struct dir_page_list *)NIL; ) {
         q=p;
         p=p->nextpage;
         free(q);
    }
    DIR_FirstPage=(struct dir_page_list *)NIL;
    free(DIR_PageBuf);
    DIR_PageBuf=NIL;
    free(dir_data);
    dir_data = (PANEL *) NIL;
    ClearMapFileNames();
}

DirectoryChangeHook(adddir, deldir, rock)
char *adddir, *deldir, *rock;                   /* ditto */
{
    char dir_name_buf[MAXPATHLEN+1], *current_dir = dir_name_buf, substatus;
    debug((1, "Directory change hook adding %s deleting %s\n",
              adddir ? adddir : "<NULL>", deldir ? deldir : "<NULL>"));
    /* If we're in bboards, assume we didn't create new bboard */
    if (DIR_mailorbb == BBOARD) return;
    FreeDirData();
    if (InitDirData(current_dir, &substatus, DIR_mailorbb, DIR_suborall, DIR_AutoorMan, DIR_template))
        return;
    if (!GetCurrentDir(current_dir, &substatus, FALSE) || !*current_dir) {
        ReportError("No folders left.", ERR_WARNING, FALSE);
        DIR_suborall=ALL;
       *DIR_template='\0';
        FreeDirData();
        InitDirData(current_dir, &substatus, DIR_mailorbb, DIR_suborall, DIR_AutoorMan, DIR_template);
        GetCurrentDir(current_dir, &substatus, FALSE);
    }
}

SubscriptionChangeHook(longname, shortname, subscrtype)
char *longname, *shortname;
int subscrtype;
{
    char *subscr;
    debug((1, "Subscription change hook %s\n", shortname));
    
    ChangeSubscriptionMapEntry (shortname, subscrtype, DIR_CurrentPage);

    switch(subscrtype) {
	case AMS_ASKSUBSCRIBED:    subscr="Ask"; break;
	case AMS_ALWAYSSUBSCRIBED: subscr="Full"; break;
	case AMS_PRINTSUBSCRIBED:  subscr="Print"; break;
	case AMS_SHOWALLSUBSCRIBED:subscr="Showall"; break;
	case AMS_UNSUBSCRIBED: default:	subscr="None"; break;
    }
    ReshowCurrentDirLine((char *)NULL, subscr, FALSE);

/*    if (subscrtype != AMS_UNSUBSCRIBED && DIR_suborall == SUBSCRIBED) {
	ReportSuccess("This will be added to the list next time in.\n");
    }  *****/
    return;
}

/* *****************************************************************
 *
 *    Session/Parameter data handling routines
 *
 ***************************************************************** */

FormKeyHeaders(outstr, outstrlen)
char *outstr;
int outstrlen;
{
    struct head_list *thl;
    *outstr = '\0';
    if (HeadList ==(struct head_list *)NIL) return;

    for (thl = HeadList; thl; thl = thl->next) {
	if (outstrlen<(3+strlen(thl->header))) {
	    strcpy(outstr,"...");
	    return;
	}
	outstrlen -= strlen(thl->header)+1;
	strcpy(outstr,thl->header);
	outstr += strlen(thl->header);
	*outstr++ = ':';
    }
    *(outstr-1)='\0';  /* Get rid of the last ':'  */
}

InitParmData (parm_data, parmspace, parmspacelen)
PANEL parm_data[];
char *parmspace;
int parmspacelen;
{
    int i;
   GetVersion(parm_data[0].pdata, parm_data[0].plen);
   if (HeadersOn) {
      if (HeadKeep) parm_data[1].pdata="Keeping";
         else parm_data[1].pdata="Omitting";
      FormKeyHeaders(parmspace, parmspacelen);
      parm_data[2].pdata = parmspace;
   } else {
      parm_data[1].pdata="All will be shown";
      parm_data[2].pdata="(ALL)";
   }
   switch (BlindStatus) {
   case AMS_SEND_BLINDYES:                              /* V1.3 MAC */
      parm_data[3].pdata="Yes";
      break;
   case AMS_SEND_BLINDNO:                               /* V1.3 MAC */
      parm_data[3].pdata="No";
      break;
   }
   parm_data[4].pdata=(*VUI_bccto)?VUI_bccto:"<None specified>";
   parm_data[5].pdata=(*VUI_editor)?VUI_editor:"<None specified>";
   parm_data[6].pdata=(*VUI_printer)?VUI_printer:"<None specified>";
   parm_data[7].pdata=(VUI_AlwaysPurge)?"Yes":"No";
   for (i=0; i<8; i++) parm_data[i].plen=strlen(parm_data[i].pdata);
}

/* *****************************************************************
 *
 *    Caption screen data handling routines
 *
 ***************************************************************** */

#define N_MSG_FIELDS 4
PRIVATE int MSG_CurrentRow, MSG_MaxVisible, MSG_max_row;
        long MSG_Updatemsgno, MSG_StartNum, MSG_Total;
PRIVATE Boolean MSG_LastPageFlag;
PRIVATE char *MSG_PageBuf = NIL, MSG_date[TIMELEN+1] = {'\0'}, MSG_DirName[MAXPATHLEN+1], MSG_ShortName[MAXPATHLEN+1];
PRIVATE char MSG_template[81];
PANEL *msg_data;
PRIVATE long  *msg_msgno;
extern PANEL mess_bp[];

typedef struct perpagemsgno {
    struct perpagemsgno *prev;
    long msgno;
    struct perpagemsgno *next;
} PerPageMsgno;

PerPageMsgno *MsgnoList = NULL,*CurrentMsgnoList = NULL;

MARKED_MSGS *MarkedMsgList = (MARKED_MSGS *)NULL;

extern char *convlongto64();
extern long time (), atol();

int MsgPageUpKey (curfield, maxopts)
int curfield, maxopts;
{
/*    long stop_msgno,LastPageStartNum; */
    debug((2,"MsgPageUpKey\n"));
    if (MSG_StartNum == 0L) {
        ReportError(VUIMSG_NOMOREMSG, ERR_WARNING, FALSE);
    } else {
	if (*MSG_template == '\0') { /* no searching */
	    MSG_StartNum -= (long)MSG_max_row;
	    if (MSG_StartNum < 0L) MSG_StartNum = 0L;
	} else { /* we need to search */
	    if (CurrentMsgnoList && CurrentMsgnoList->prev) {
		MSG_StartNum = CurrentMsgnoList->prev->msgno;
		CurrentMsgnoList = CurrentMsgnoList->prev;
	    } else {
		ReportError(VUIMSG_NOMOREMSG,ERR_WARNING,FALSE);
		return(curfield);
	    }
	}
	EraseMenuCursor();
	if (!LoadOneMsgPage(MSG_DirName, MSG_StartNum,&MSG_MaxVisible)) {
	    MSG_CurrentRow=0;
	    ShowMsgData();
	    ClearError();
	}
        ShowMenuCursor();
    }
    return(curfield);
}

int MsgPageDnKey (curfield, maxopts)
int curfield, maxopts;
{
    int save_maxvisible, rc;
    debug((2,"MsgPageDnKey\n"));
/*    if ((MSG_StartNum+(long)MSG_max_row) > MSG_Total) { */
    if (MSG_MaxVisible < MSG_max_row) {
        MSG_LastPageFlag=TRUE;
        if (maxopts>=0) ReportError(VUIMSG_NOMOREMSG, ERR_WARNING, FALSE);
        return(curfield);
    }
/*    MSG_StartNum += (long)MSG_max_row; */
    if (*MSG_template && CurrentMsgnoList && CurrentMsgnoList->next)
	MSG_StartNum = CurrentMsgnoList->next->msgno;
    else
	MSG_StartNum = msg_msgno[MSG_max_row];
    if (maxopts>=0) {
        EraseMenuCursor();
        ReportSuccess("Loading new page of message captions...");
    }
    rc=LoadOneMsgPage(MSG_DirName, MSG_StartNum, &save_maxvisible);
    if (!rc && save_maxvisible >= 0) {                          /* V1.3 MAC */
        MSG_MaxVisible=save_maxvisible;
        MSG_CurrentRow=(MSG_MaxVisible>0)?1:0;
	if (*MSG_template != '\0') {
	    if (CurrentMsgnoList->next == NULL) {
		if ((CurrentMsgnoList->next = (PerPageMsgno *)
		     malloc(sizeof(PerPageMsgno))) == NULL) {
		    ReportError(VUIMSG_NOMEM,ERR_CRITICAL,FALSE);
		    return(curfield);
		}
		CurrentMsgnoList->next->prev = CurrentMsgnoList;
		CurrentMsgnoList = CurrentMsgnoList->next;
		CurrentMsgnoList->next = NULL;
		CurrentMsgnoList->msgno = MSG_StartNum;
	    } else
		CurrentMsgnoList = CurrentMsgnoList->next;
	}
        if (maxopts>=0) {
            ShowMsgData();
            ClearError();
        }
    } else {
        MSG_LastPageFlag=TRUE;
        if (!rc && maxopts>=0) ReportError(VUIMSG_NOMOREMSG, ERR_WARNING, FALSE);
    }
    if (maxopts>=0) ShowMenuCursor();
    return(curfield);
}

int MsgMovement (curfield, maxopts, how)
int curfield, maxopts;
Boolean how;
{
    register int row, i;
    debug((2,"MsgUpArrowKey\n"));
    row=N_MSG_FIELDS*MSG_CurrentRow;
    for (i=N_MSG_FIELDS; i; i--, row++) {
         ShowString(msg_data[row].pdata, msg_data[row].prow,
                    msg_data[row].pcol,  msg_data[row].plen,
                    NORMAL);
    }
    if (how == MSG_UP_ARROW) {
	if (MSG_CurrentRow==0)
	    MSG_CurrentRow=MSG_MaxVisible;
	else MSG_CurrentRow--;
    }
    else if (how == MSG_DOWN_ARROW) {
	if (MSG_CurrentRow==MSG_MaxVisible)
	    MSG_CurrentRow=0;
	else MSG_CurrentRow++;
    }
    else if (how == MSG_HOME_KEY) {
	MSG_CurrentRow=0;
    }
    else { /* MSG_END_KEY */
	MSG_CurrentRow=MSG_MaxVisible;
    }
    row=N_MSG_FIELDS*MSG_CurrentRow;
    for (i=N_MSG_FIELDS; i; i--, row++) {
         ShowString(msg_data[row].pdata, msg_data[row].prow,
                    msg_data[row].pcol,  msg_data[row].plen,
                    RVIDEO);
    }
    return(curfield);
}

int MsgUpArrow (curfield, maxopts)
int curfield, maxopts;
{
    return(MsgMovement(curfield,maxopts,MSG_UP_ARROW));
}

int MsgDnArrow (curfield, maxopts)
int curfield, maxopts;
    {
    return(MsgMovement(curfield,maxopts,MSG_DOWN_ARROW));
}

int MsgHomeKey (curfield, maxopts)
int curfield, maxopts;
    {
    return(MsgMovement(curfield,maxopts,MSG_HOME_KEY));
}

int MsgEndKey (curfield, maxopts)
int curfield, maxopts;
    {
    return(MsgMovement(curfield,maxopts,MSG_END_KEY));
    }

InitMsgData(startdate, NumNew, NumTotal, LongName)
char *startdate, *LongName;
long NumNew, NumTotal;
{
    char ErrorText[256], holddate[TIMELEN+1], status[2];
    long holdupdatemsgno;
    int i, n_shown;
    PANEL *pp;

    debug((1,"InitMsgData(%s, %s)\n", startdate, LongName));
    status[1]='\0';
    GetCurrentDir(LongName, &status[0], FALSE);
    holdupdatemsgno = MSG_Updatemsgno;
    if (MSG_PageBuf==NIL) {
        MSG_PageBuf=(char *)malloc(bar_row * 85);
        debug((2,"MSG_PageBuf allocated at %ld\n",MSG_PageBuf));
        if (MSG_PageBuf==NIL) {
            ReportError(VUIMSG_NOMEM, ERR_FATAL, FALSE);
            return(MENU_EMPTY);
        }
    }
    MSG_max_row = bar_row - 3;
    debug((2, "MSG_max_row is set to %d\n", MSG_max_row));
    if (msg_data == (PANEL *) NIL) {
	msg_data = (PANEL *) malloc(((MSG_max_row+1)*N_MSG_FIELDS+1)*sizeof(PANEL));
	debug((2, "msg_data allocated at %ld\n", msg_data));
	msg_msgno = (long *)malloc((MSG_max_row+1)*sizeof(long));
	if (msg_data == (PANEL *) NIL || msg_msgno == (long *)NULL) {
	    ReportError(VUIMSG_NOMEM, ERR_FATAL, FALSE);
	    return(-1);
	}
	for (i=2, pp=msg_data; i<bar_row; i++) {
	    pp->prow = i;	pp->pcol = 10;	pp->pattr = NORMAL;
	    pp->plen = 4;	pp->pdata = NIL;    pp++;
	    pp->prow = i;	pp->pcol = 0;	pp->pattr = NORMAL;
	    pp->plen = 10;	pp->pdata = NIL;    pp++;
	    pp->prow = i;	pp->pcol = 14;	pp->pattr = NORMAL;
	    pp->plen = 31;	pp->pdata = NIL;    pp++;
	    pp->prow = i;	pp->pcol = 45;	pp->pattr = NORMAL;
	    pp->plen = 35;	pp->pdata = NIL;    pp++;
	}
	pp->plen = 0; /* NULL PANEL */
    }
    MSG_StartNum = NumTotal - NumNew - 1L; /* start with last old one */
    if (MSG_StartNum<0L) MSG_StartNum = 0L; /* In case there is only 1 total */
    MSG_Total = NumTotal;
    MSG_Updatemsgno = MSG_StartNum;
    debug((2,"%s has %ld new of %ld, so starting with %ld.\n",LongName, NumNew, NumTotal, MSG_StartNum));
    strcpy(MSG_DirName, LongName);
    *MSG_template = '\0';
    if (LoadOneMsgPage(MSG_DirName, MSG_StartNum, &n_shown)) {
        FreeMsgData();
        return(MENU_EMPTY);
    }

    if (n_shown<0) {
	/* Actually I don't think we can get here any more. */
        MSG_Updatemsgno = holdupdatemsgno;
	if (startdate==NIL) ReportSuccess("The folder is empty");
	else if (!strcmp("lastread", startdate)) ReportSuccess("No new messages");
	else if (!strcmp("unread", startdate)) ReportSuccess("No unread messages");
	else {
	    sprintf(ErrorText,"No messages since %s",startdate);
	    ReportSuccess(ErrorText);
	}
        return(MENU_EMPTY);
    }
    GetCurrentDir(MSG_ShortName, NIL, TRUE);
    mess_bp[3].pdata = MSG_ShortName;
    MSG_MaxVisible = n_shown;
    strcpy(MSG_date,holddate);
    if (NumNew == NumTotal) MSG_CurrentRow = MSG_StartNum;
    else MSG_CurrentRow=(MSG_StartNum==(long)(NumTotal-1))?0:1;
    MSG_LastPageFlag=FALSE;
    return(0);
}

PRIVATE LoadOneMsgPage (DirName, startnum, n_shown_p)
char *DirName;
int *n_shown_p;
long startnum;
{
    char snapshot[AMS_SNAPSHOTSIZE], *PageBufPtr,*SavePageBufPtr, *RawCap;
    int     maxlen, line_count, row;
    Boolean NewFlag = FALSE;
    long msgno;
    static char line_str[80];

    line_count= -1;
    PageBufPtr = MSG_PageBuf;

    debug((2,"Starting page at message %ld.\n", startnum));
    for (msgno=startnum; msgno<MSG_Total; msgno++) {
	if (mserrcode = MS_GetNthSnapshot(DirName, msgno, snapshot)) {
	    ReportError(VUIMSG_SNAPSHOT, ERR_WARNING, TRUE);
	    break;
	}
        debug((2,"Date:%s ID: %s\n", AMS_DATE(snapshot), AMS_ID(snapshot)));
        if (NewFlag && !AMS_GET_ATTRIBUTE(snapshot, AMS_ATT_UNSEEN))
        continue;      /* we are looking for only (N) messages */
        line_count++;
        debug((2,"Adding %ld to row %d\n", msgno, line_count));
        row=N_MSG_FIELDS*line_count;
	SavePageBufPtr = PageBufPtr;
	msg_msgno[line_count] = msgno;
        msg_data[row].pdata=PageBufPtr;    /* Load flags  */
        maxlen=0;
        if (MSG_mailorbb!=BBOARD) {
	    if (AMS_GET_ATTRIBUTE(snapshot, AMS_ATT_DELETED)) {
		*PageBufPtr++='D';  maxlen++;
		CUI_MarkDirectoryForPurging(DirName);
	    }
            if (AMS_GET_ATTRIBUTE(snapshot, AMS_ATT_UNSEEN)) {
                *PageBufPtr++='N';  maxlen++;
            }
	} else {
            if (msgno==MSG_Updatemsgno) {
                *PageBufPtr++='*';  maxlen++;
            }
        }
        while (maxlen++<msg_data[row].plen) *PageBufPtr++=' ';	
        *PageBufPtr='\0';
	if (MarkedMsgList) { /* Show the mark when user pages up and down */
	    MARKED_MSGS *mml;
	    mml = MarkedMsgList;
	    while (mml) {
		if (mml->msgno == msgno) {
		    *(PageBufPtr-2) = 'M';
		    break;
		}
		mml = mml->next;
	    }
	}
        RawCap = AMS_CAPTION(snapshot);
        msg_data[++row].pdata=PageBufPtr;  /* Get Date */
        maxlen=msg_data[row].plen;
        while (maxlen--) {
            if (*RawCap!='\t') *PageBufPtr++ = *RawCap++;
              else *PageBufPtr++=' ';
        }
        *PageBufPtr='\0'; RawCap++;

        msg_data[++row].pdata=PageBufPtr;  /* Get Subj */
        maxlen=msg_data[row].plen;
        while (maxlen--) {
            if (*RawCap!='\t') *PageBufPtr++ = *RawCap++;
              else *PageBufPtr++=' ';
        }
        *PageBufPtr='\0'; RawCap++;
        msg_data[++row].pdata=PageBufPtr;  /* Get From,len */
        maxlen=msg_data[row].plen;
        while (maxlen--) {
            if (*RawCap!='\0') *PageBufPtr++ = *RawCap++;
              else *PageBufPtr++=' ';
        }
        *PageBufPtr='\0'; RawCap++;
	if (*MSG_template != '\0') { /* search headers */
	    int l1,l2;
	    Boolean nomatch;
	    char *matchptr;
	    matchptr = msg_data[row-1].pdata;
	    nomatch = TRUE; l1 = strlen(MSG_template); l2 = strlen(matchptr);
	    while ((l1 <= l2) && (nomatch)) {
		if (ULstrncmp(MSG_template,matchptr,l1) == 0)
		    nomatch=FALSE;
		matchptr++;l2--;
	    }
	    if (nomatch) {
		line_count--;
		PageBufPtr = SavePageBufPtr;
		continue;
	    }
	}
        if (MSG_max_row==line_count) break;
    }  /* End For  */
    debug((2,"We picked up %d lines\n",line_count));
    *n_shown_p = line_count++;
    if (*n_shown_p<0) return(0);
    *(++PageBufPtr)='\0';
    while (line_count<=MSG_max_row) {
           row=N_MSG_FIELDS*line_count++;
           maxlen=N_MSG_FIELDS;
           while (maxlen--) {
               msg_data[row++].pdata=PageBufPtr;
	   }
    }
    debug((2,"So %d lines are visible and the rest null\n",*n_shown_p));
    sprintf(line_str, "%ld of %ld.", msg_msgno[0]+1L, MSG_Total);
    mess_bp[1].pdata = line_str;

    return(0);
}

ShowMsgData ()
{
    register int row, i;
    debug((1,"ShowMsgData\n"));
    ShowString(mess_bp[1].pdata, mess_bp[1].prow,
               mess_bp[1].pcol,  mess_bp[1].plen,
               NORMAL);
    row=N_MSG_FIELDS*MSG_CurrentRow;
    for (i=N_MSG_FIELDS; i; i--) msg_data[row++].pattr=RVIDEO;
    DrawPanel(msg_data, NIL, PANEL_NOCLEAR);
    row=N_MSG_FIELDS*MSG_CurrentRow;
    for (i=N_MSG_FIELDS; i; i--) msg_data[row++].pattr=NORMAL;
}

GetCurrentMsg (msgno)
long *msgno;
{
   debug((2,"Returning current message %ld\n",MSG_StartNum+(long)MSG_CurrentRow));
/*   (*msgno)=MSG_StartNum+(long)MSG_CurrentRow; */
   (*msgno) = msg_msgno[MSG_CurrentRow];
}

CuidFromMsgno(msgno)
long msgno;
{
    char snapshot[AMS_SNAPSHOTSIZE];
    int j;
    if (mserrcode = MS_GetNthSnapshot(MSG_DirName, msgno, snapshot)){
	ReportError(VUIMSG_SNAPSHOT, ERR_WARNING, TRUE);
	return(-1);
    }
    return(CUI_GetCuid(AMS_ID(snapshot), MSG_DirName, &j));
}

PurgeCurrentDir()
{
    debug((2,"Checking for purges on %s\n", MSG_DirName));
    if	(CUI_DoesDirNeedPurging(MSG_DirName)) {
	if ((VUI_AlwaysPurge) || 
	    (GetBooleanFromUser("Do you want to purge the deleted messages?", TRUE)))
	    CUI_PurgeDeletions(MSG_DirName);
    }
}

ReshowCurrentMsgLine (status, display, highlight)
char *status;
Boolean display, highlight;
{
    register int row;
    int maxlen;
    char *p;

    debug((1,"ReshowCurrentMsgLine %d to %s\n", MSG_CurrentRow, status));
    row=N_MSG_FIELDS*MSG_CurrentRow;
    p=msg_data[row].pdata;
    maxlen=msg_data[row].plen;
    while (maxlen--) {
       if (*status) *p++ = *status++;
       else *p++=' ';
    }
    if (!display) return;
    ShowString(msg_data[row].pdata, msg_data[row].prow,
               msg_data[row].pcol,  msg_data[row].plen,
	       (highlight ? RVIDEO : NORMAL));
}

GetCurrentMsgAttributes(buf, buflen)
char *buf;
int buflen;
{
    register int row;
    int maxlen;
    char *p;

    row = N_MSG_FIELDS * MSG_CurrentRow;
    p = msg_data[row].pdata;
    maxlen = msg_data[row].plen;
    while (maxlen-- && buflen--) *buf++ = *p++;

    return(0);
}

MsgIsVisible(msgno)
long msgno;
{
    int i;

    for (i=0; i<=MSG_MaxVisible; i++)
	if (msg_msgno[i] == msgno) break;

    if (i > MSG_MaxVisible) return(-1);

    return(i);
}

GetAndSetMsgRow(old_row, new_row)
int *old_row, new_row;
{
    *old_row = MSG_CurrentRow;

    if ((new_row > MSG_MaxVisible) || (new_row < 0)) return(-1);

    MSG_CurrentRow = new_row;
    return(0);
}

IncrMsg (msgno_p)
long *msgno_p;
{
   /* Goto next message */
   if (MSG_CurrentRow<MSG_MaxVisible) {
       MSG_CurrentRow++;
       GetCurrentMsg(msgno_p);
   } else {
       MsgPageDnKey(0,-1);
       if (MSG_LastPageFlag) *msgno_p = -1L;
         else GetCurrentMsg(msgno_p);
   }
   debug((2,"Incremented message to %ld at row %d", *msgno_p, MSG_CurrentRow));
}

long 
SetTopOfPage (bynumber)
int bynumber;
{
    char *Prompt;
    long msgno;
    int holdmsg;

    debug((2,"SetTopOfPage(%d)\n", bynumber));
    if (bynumber) {
	Prompt = "Enter message number to start the page:";
	ClearLine(bar_row+1);
	ShowString(Prompt, bar_row+1, 0, strlen(Prompt), NORMAL);
	if (0>GetNUser(bar_row+1, strlen(Prompt)+1, 5, 1, (int)MSG_Total, &holdmsg))
	    return(-1);
	msgno = (long)holdmsg - 1L;
	debug((2,"You entered %d so we are starting at %ld\n", holdmsg, msgno));
    } else {
	char newdate_buf[81], *newdate, date64[AMS_DATESIZE], headbuf[AMS_SNAPSHOTSIZE], ErrorText[256];
	int year, month, day, hour, min, sec, wday;
	long gtm, numbytes, bytesleft;

	newdate=GetSUser("Enter starting date:", NIL, NIL, newdate_buf);
	if (NIL==newdate) return(-1);
	mserrcode = MS_ParseDate(newdate, &year, &month, &day, &hour, &min, &sec, &wday, &gtm);
	if (mserrcode) {
	    ReportError("Can't understand the date", ERR_WARNING, TRUE);
	    return(-1);
	}
	strcpy(date64,convlongto64(gtm, 0));
	debug((2,"Converted date %s to %s\n", newdate, date64));
	CUI_GetHeaders(MSG_DirName, date64, headbuf, AMS_SNAPSHOTSIZE, 0L, &numbytes, &bytesleft, TRUE);
	if (numbytes<=0) {
	    sprintf(ErrorText, "No messages have appeared since %d/%d/%d %d:%02d:%02d\n", month + 1, day, year, hour, min, sec);
	    ReportError(ErrorText, ERR_WARNING, FALSE);
	    return(-1);
	}
	msgno = MSG_Total - (numbytes+bytesleft)/(long)AMS_SNAPSHOTSIZE;
    }
    *MSG_template = '\0';
    FreeMsgnoList();
    if (!LoadOneMsgPage(MSG_DirName, msgno, &MSG_MaxVisible)) {
	MSG_CurrentRow=0;
    }
    MSG_StartNum = msgno;
    return(msgno);
}

long SetTopOfPageAndSearch(matchstr)
char *matchstr;
{
    long save_msgno;

    if (matchstr == NIL || *matchstr == '\0') return MENU_EMPTY;
    FreeMsgnoList();
    debug((2, "Only showing messages that start with '%s'\n", matchstr));
    strncpy(MSG_template, matchstr, 80);
    save_msgno = msg_msgno[MSG_CurrentRow];
    if (!LoadOneMsgPage(MSG_DirName,0L,&MSG_MaxVisible) && (MSG_MaxVisible >= 0)) {
	MSG_CurrentRow = 0;
	MSG_StartNum = msg_msgno[MSG_CurrentRow];
	if ((MsgnoList = (PerPageMsgno *)malloc(sizeof(PerPageMsgno))) == NULL) {
	    ReportError(VUIMSG_NOMEM,ERR_CRITICAL,FALSE);
	    return(MENU_EMPTY);
	}
	CurrentMsgnoList = MsgnoList;
	MsgnoList->next = MsgnoList->prev = (PerPageMsgno *)NULL;
	MsgnoList->msgno = MSG_StartNum;
    }
    else {
	*MSG_template = '\0';
	LoadOneMsgPage(MSG_DirName,save_msgno,&MSG_MaxVisible);
	ShowMsgData();
	ReportError("No match!",ERR_WARNING,FALSE);
	return(MENU_EMPTY);
    }
    return(msg_msgno[MSG_CurrentRow]);
}

FreeMsgData ()
{
    free(MSG_PageBuf);
    MSG_PageBuf=NIL;
    free(msg_data);
    msg_data = (PANEL *) NIL;
    FreeMsgnoList();
}

FreeMsgnoList()
{
    PerPageMsgno *p;

    while (MsgnoList != NULL) {
	p = MsgnoList;
	MsgnoList = p->next;
	free(p);
    }
    CurrentMsgnoList = MsgnoList = NULL;
}

UpdateMsgData (msgno, update_msgno)
long msgno;
Boolean update_msgno;
{
    char snapshot[AMS_SNAPSHOTSIZE];
    int j, cuid, save_row, new_row;

    debug((1,"Updating message data(%ld, %d)\n",msgno, update_msgno));
    if (mserrcode = MS_GetNthSnapshot(MSG_DirName, msgno, snapshot)) {
	ReportError(VUIMSG_SNAPSHOT, ERR_WARNING, TRUE);
        return(-1);
    }

    cuid = CUI_GetCuid(AMS_ID(snapshot), MSG_DirName, &j);
    if (AMS_GET_ATTRIBUTE(snapshot, AMS_ATT_UNSEEN) &&
        AMS_GET_ATTRIBUTE(snapshot, AMS_ATT_MAYMODIFY)) {
        CUI_MarkAsRead(cuid);        /*    Mark it as "read" in database  */
	if ((new_row = MsgIsVisible(msgno)) >= 0) {
	    char abuf[5];
	    GetAndSetMsgRow(&save_row, new_row);
	    GetCurrentMsgAttributes(abuf, sizeof(abuf));
	    abuf[sizeof(abuf)-1] = '\0'; abuf[0] = abuf[1] = ' ';
	    if (AMS_GET_ATTRIBUTE(snapshot, AMS_ATT_DELETED))
		abuf[0] = 'D';
	    ReshowCurrentMsgLine(abuf, FALSE, (save_row == new_row)); /* remove 'N'         */
	    GetAndSetMsgRow(&new_row, save_row);
	}
    }
    if (update_msgno) SetUpdatemsgno(msgno, FALSE, FALSE);
    return(CUI_ProcessMessageAttributes(cuid, snapshot));
}

SetUpdatemsgno (msgno, display_it, force_it)
long msgno;
Boolean display_it, force_it;
{
    int new_row, save_row;
    char abuf[5];

    if (!force_it && MSG_Updatemsgno>msgno) return(-1);

    if (DIR_mailorbb==BBOARD) {
	if ((new_row = MsgIsVisible(MSG_Updatemsgno)) >= 0) { /* clear the previous */
	    GetAndSetMsgRow(&save_row, new_row);
	    GetCurrentMsgAttributes(abuf, sizeof(abuf));
	    abuf[sizeof(abuf)-1] = '\0';
	    abuf[0] = abuf[1] = ' ';
	    ReshowCurrentMsgLine(abuf, display_it, (save_row == new_row));
	    GetAndSetMsgRow(&new_row, save_row);
	}
	
	if ((new_row = MsgIsVisible(msgno)) >= 0) { /* set the new one */
	    GetAndSetMsgRow(&save_row, new_row);
	    GetCurrentMsgAttributes(abuf, sizeof(abuf));
	    abuf[sizeof(abuf)-1] = '\0';
	    abuf[0] = '*'; abuf[1] = ' ';
	    ReshowCurrentMsgLine(abuf, display_it, (save_row == new_row));
	    GetAndSetMsgRow(&new_row, save_row);
	}
    }
    MSG_Updatemsgno = msgno;

    return(0);
}

int GetNextMarkedMsg(msgno) 
long msgno;
{
    MARKED_MSGS *linkp;

    if (!MarkedMsgList) return(-1); /* no marked list, error */

    if (msgno < 0) return((int)(MarkedMsgList->msgno)); /* return the first one */

    linkp = MarkedMsgList; /* finde the next after this msgno */

    while (linkp && linkp->msgno != msgno) linkp = linkp->next;

    if (linkp && linkp->next) return((int)(linkp->next->msgno)); /* return the next one */

    return(-1); /* no more, error */
}

int LinktoMarkList(msgno)
long msgno;
{
    MARKED_MSGS *tmp, *linkp;

    if ((tmp = (MARKED_MSGS *)malloc(sizeof(MARKED_MSGS))) == 0) {
	ReportError(VUIMSG_NOMEM, ERR_CRITICAL, FALSE);
	return(-1);
    }
    tmp->msgno = msgno;
    tmp->next = tmp->prev = (MARKED_MSGS *)NULL;

    if (MarkedMsgList == (MARKED_MSGS *)NULL) {
	MarkedMsgList = tmp;
    } else {
	linkp = MarkedMsgList;
	while(linkp->next) linkp = linkp->next;
	tmp->prev = linkp;
	linkp->next = tmp;
    }

    return(0);
}

int DelinkFromMarkList(msgno)
long msgno;
{
    MARKED_MSGS *linkp;

    linkp = MarkedMsgList;

    while (linkp && (linkp->msgno != msgno)) linkp = linkp->next;

    if (linkp) { /* delink this one */
	if (linkp->prev) linkp->prev->next = linkp->next;
	if (linkp->next) linkp->next->prev = linkp->prev;
	if (linkp == MarkedMsgList) MarkedMsgList = linkp->next;
	free(linkp);
    } 

    return(0);
}

FreeMarkList()
{
    MARKED_MSGS *linkp;

    while (MarkedMsgList) {
	linkp = MarkedMsgList->next;	
	free(MarkedMsgList);
	MarkedMsgList = linkp;
    }
    MarkedMsgList = (MARKED_MSGS *)NULL;
    return(0);
}
    
UpdateProf (all)
Boolean all;
{
    char    snapshotbuf[AMS_SNAPSHOTSIZE], newdate[DATELEN];

    if (all) {
	if (MS_GetNthSnapshot(MSG_DirName, (long)MSG_Total-(long)1, snapshotbuf)) return(-1);
	ClearError();
    } else {
        if (MSG_Updatemsgno==-1L) return(0);
	if (MS_GetNthSnapshot(MSG_DirName, (long)MSG_Updatemsgno, snapshotbuf)) return(-1);
 
    }
    strcpy(newdate, AMS_DATE(snapshotbuf));
    debug((4,"Setting associated time for %s to %s\n", MSG_DirName, newdate));
    mserrcode = MS_SetAssociatedTime(MSG_DirName, newdate);
    if (!mserrcode) mserrcode = MS_UpdateState();
    if (mserrcode) {
        ReportError("Could not update your profile", ERR_WARNING, TRUE);
    }
}

/* *****************************************************************
 *
 *    Message entry data handling routines
 *
 ***************************************************************** */
long ENT_offset, ENT_filelength,ENT_ReplyOffset;
long ENT_headoffset, ENT_headlength;
char ENT_filename[MAXPATHLEN+1],ENT_ReplyFilename[MAXPATHLEN+1];
char *ENT_body_field = NIL;
char ENT_headerfile[MAXPATHLEN+1], *ENT_head_field = NIL;

Boolean ENT_Anyloss, ENT_Savethepage;

int BODY_field_len, HEAD_field_len;

extern FIELD entry_fields[];
extern jmp_buf entrypanel;

InitEntryScreen(body_field, field_len, start_row, rows, fieldname)
char **body_field;
int  *field_len, start_row, rows;
FIELD *fieldname;
{
    int i;
    XTENT *opt, *op;
    if (*body_field == (char *)NULL) {
	*field_len = rows * LINE_LENGTH;
	*body_field = (char *)malloc(*field_len+1);
	opt = (XTENT *) malloc((1+rows)*sizeof(XTENT));
	if (!(*body_field) || (!opt)) {
	    ReportError(VUIMSG_NOMEM, ERR_WARNING, FALSE);
	    return(-1);
	}
	for (i=start_row, op=opt; i<start_row+rows; i++, op++) {
	    op->xrow = i; op->xcol = 0; op->xlen = LINE_LENGTH, op->xprt = 0;
	}
	op->xrow = op->xcol = op->xlen = op->xprt = 0;
	fieldname->flocation = opt;
	fieldname->fdata = *body_field;
    }
    **body_field='\0';
    return(0);
}

InitEntryData (cuid, code, templatefile)
int cuid, code;
char *templatefile;
{
    long msgno,offset;
    char filename[MAXPATHLEN+1],tmp_file[MAXPATHLEN+1];

    debug((1,"InitEntryData(%d, %d, %s)\n", cuid, code, templatefile));

    ENT_ReplyFilename[0] = '\0';
    ENT_ReplyOffset = 0L;
    ENT_headerfile[0] = '\0';

    if (InitEntryScreen(&ENT_body_field, &BODY_field_len, 
		     N_TCS_LINES, bar_row-N_TCS_LINES,&entry_fields[1]) != 0)
	return(-1);

    if (InitEntryScreen(&ENT_head_field, &HEAD_field_len, 
		     0, N_TCS_LINES-1,&entry_fields[0]) != 0)
	return(-1);

    if (templatefile == NIL) {
	CUI_GenTmpFileName(filename);
	if (CUI_NameReplyFile(cuid, code, filename)) return (-1);
	if (code != AMS_REPLY_FRESH) {
	    GetCurrentMsg(&msgno);
	    WriteUnscribedBody(msgno,ENT_ReplyFilename);
	}
	templatefile = filename;
    }

    if (GetHeaderFields(templatefile, tmp_file, &offset)) return(-1);
    if (DecompressFile(tmp_file, ENT_headerfile, &ENT_headlength, TRUE, TRUE)) return(-1);
    if (DecompressFile(templatefile, ENT_filename, &offset, TRUE, FALSE)) return(-1);
    ENT_filelength = offset;

    ENT_headoffset = 0L;
    ENT_offset = 0L;
    if (LoadOneEntPage(ENT_headerfile, ENT_headlength,
			0L, ENT_head_field, HEAD_field_len, (long *)NIL)) return(-1);
    SetProtectedFields(&entry_fields[0]);
    return(LoadOneEntPage(ENT_filename, ENT_filelength,
			   0L, ENT_body_field, BODY_field_len, (long *)NIL));
}

FIELD *EntryEscKey (curfield)
FIELD *curfield;
    {
    longjmp(entrypanel, -1);
    return((FIELD *)NIL); /* Actually never reached. Put in to avoid hc warnings */
    }
/* *** This hides the 'move to beginning of the line' function. **** */
/* *** So I took it out of the table of entry keys.             **** */
FIELD *EntryHomeKey (curfield)
FIELD *curfield;
{
    debug((2,"EntryHomeKey\n"));
    if (((curfield==&entry_fields[1]) && (0L==ENT_offset))  ||
	((curfield==&entry_fields[0]) && (0L==ENT_headoffset))) {
        ReportError(VUIMSG_BENTLIST, ERR_WARNING, FALSE);
    } else {
        SaveCurrentEntPage();
	if (curfield == &entry_fields[0]) {
	    ENT_headoffset = 0L;
	    LoadOneEntPage(ENT_headerfile, ENT_headlength, 0L, ENT_head_field,
			   HEAD_field_len, (long *)NULL);
	}
	else {
	    ENT_offset = 0L;
	    LoadOneEntPage(ENT_filename, ENT_filelength, 0L, ENT_body_field,
			   BODY_field_len, (long *)NIL);
	}
        ShowEntData();
        ENT_Savethepage=FALSE;
    }
    return(curfield);
}

FIELD *EntryPageDnKey (curfield)
FIELD *curfield;
{
    debug((2,"EntryPageDnKey\n"));
    SaveCurrentEntPage();

    if (curfield == &entry_fields[0]) { /* headers */
	EntryPageDown(ENT_headerfile, &ENT_headlength, &ENT_headoffset, HEAD_field_len,
		      ENT_head_field, 0);
    } else { /* body  */
	EntryPageDown(ENT_filename, &ENT_filelength, &ENT_offset, BODY_field_len,
		      ENT_body_field, N_TCS_LINES);
    }
    return(curfield);
}

int EntryPageDown(filename, filelength, offset, field_len, body_field, row)
char *filename, *body_field;
long *filelength, *offset;
int   field_len, row;
{
    Boolean MustPosition = FALSE;
    int bytestoload;
    long result;

    bytestoload = field_len - ((field_len / LINE_LENGTH) / 2) * LINE_LENGTH;
    if (!LoadOneEntPage(filename, *filelength, *offset+(long)(bytestoload),
			body_field, field_len, &result)) {
	ShowEntData(); 
	if (result == 0L) ErrorBeep();
	else {
	    *offset += (long)bytestoload;
	    if (*offset == (*filelength-(long)(field_len-bytestoload)))
		MustPosition = TRUE;
	}
    }

    if (MustPosition)
	PositionCursor(((field_len-bytestoload)/LINE_LENGTH)+row,0);
    return(0);
}

FIELD *EntryPageUpKey (curfield)
FIELD *curfield;
{
    debug((2,"EntryPageUpKey\n"));
    SaveCurrentEntPage();

    if (curfield == &entry_fields[0]) { /* headers */
	EntryPageUp(ENT_headerfile, &ENT_headlength, &ENT_headoffset, HEAD_field_len,
		    ENT_head_field, 0);
    } else { /* body */
	EntryPageUp(ENT_filename, &ENT_filelength, &ENT_offset, BODY_field_len,
		      ENT_body_field, N_TCS_LINES);
    }

    return(curfield);
}

int EntryPageUp (filename, filelength, offset, field_len, body_field, row)
char *filename, *body_field;
long *filelength, *offset;
int   field_len, row;
{
    long result;
    int  bytestoload;

    if (*offset == 0L) {
	    ReportError(VUIMSG_BENTLIST, ERR_WARNING, FALSE);
    } else {
	bytestoload = field_len - ((field_len / LINE_LENGTH) / 2) * LINE_LENGTH;
	if (!LoadOneEntPage(filename, *filelength, *offset-(long)(bytestoload),
			    body_field, field_len, &result)) {
	    if (result != 0L)
		*offset -= bytestoload;
/*	    ShowEntData(); */
	}
	ShowEntData();
    }
/*    ShowCursor(); */
    return(0);
}

RemoveShowReplyFile()
{
    if (*ENT_ReplyFilename)
	RealUnlinkViceFile(ENT_ReplyFilename);

    ENT_ReplyFilename[0] = '\0';
}

LoadOneEntPage (filename, filelength, offset, body_field, field_len, bytesread_p)
char *filename;
long  filelength, offset, *bytesread_p;
char *body_field;
int   field_len;
{
    int     bodylen;
    long    bytesunfetched;

    if (bytesread_p!=(long *)NIL) *bytesread_p = 0L; /* just in case of error */

    if (offset >= filelength) {
	debug((2,"Going down past end of file; returning current position\n"));
	return(0); 
    }

    debug((2,"Loading page from %s at offset %ld\n",filename, offset));
    if ((mserrcode = MS_GetPartialFile(filename, body_field, field_len,
					offset, &bytesunfetched, &bodylen)) != 0) {
        ReportError(VUIMSG_READFILE, ERR_WARNING, TRUE);
        return(-1);
    }
    debug((2,"Returned %d bytes and %ld are left\n",bodylen, bytesunfetched));
    if (bodylen <= 0) return(0);
    body_field[bodylen] = '\0';
    if (bytesread_p!=(long *)NIL) *bytesread_p = /* field_len */ bodylen;
    return(0);
}

SetProtectedFields(curfield)
FIELD *curfield;
{
    int i;
    char  *buf, *endbuf, *colon;
    XTENT *this;

    if (curfield == (FIELD *)NULL) return (-1);

    if ((this = curfield->flocation) == (XTENT *)NULL) return(0);
    if ((buf = curfield->fdata) == (char *)NULL) return(0);

    endbuf = buf;
    while ((*endbuf) && (endbuf-buf <= ((N_TCS_LINES - 1) * LINE_LENGTH)))
	endbuf++;

    for (i=0; i<N_TCS_LINES-1; i++) {
	if ((!*buf) || (buf >= endbuf))
	    this->xprt = 0;
	else {
	    if (*buf == ' ')
		this->xprt = 1;
	    else {
		colon = strchr(buf, ':');
		if ((colon == NIL) || (colon - buf > LINE_LENGTH))
		    this->xprt = 0;
		else
		    this->xprt = (colon - buf) + 2;
	    }
	}
	buf += LINE_LENGTH;
	this++;
    }
    return(0);
}

SaveCurrentEntPage ()
{
    if (!ENT_Savethepage) return(0);
    debug((1,"Saving current page at offset %ld\n", ENT_offset));
    /* also save the headers */
    ENT_Savethepage=FALSE;
    if (SaveBodyData(HEAD_field_len, ENT_head_field, ENT_headerfile,
		     HEAD_field_len, &ENT_headoffset, &ENT_headlength)) return(-1);
    return(SaveBodyData(BODY_field_len, ENT_body_field, ENT_filename,
			BODY_field_len, &ENT_offset, &ENT_filelength));
}

int SaveBodyData(bytes, body_field, filename, field_len, offset, filelength)
int   bytes;
char *body_field, *filename;
int   field_len;
long *offset, *filelength;
{
    int     i,bytesleft;
    Boolean padding;
    char   *in_char;

    in_char = body_field;
    i=bytes;
    padding=FALSE;

    while (i--) {               /* Pad the field out with blanks */
         if (padding) *in_char++=' ';
	 else {
             if (!*in_char) {   /* We hit the end of the input  */
                 *in_char++=' ';
                 padding=TRUE;
             } else in_char++;
         }
    }
    if ((bytes == field_len) || (*filelength < *offset+(long)bytes)) {
	bytesleft = ((field_len / LINE_LENGTH) / 2) * LINE_LENGTH;
	if ((*offset >= *filelength) ||
	    (*filelength - *offset == (long)(bytesleft))) {
	    *filelength = *offset + (long)field_len;
	    debug((2,"Extending file length to %ld.\n", *filelength));
	}
	else if (*filelength < *offset+(long)bytes) { /* uneven extends due to inserts */
	    debug((2,"Uneven extends, filelength=%ld, offset=%ld, bytes=%d\n",
		   *filelength, *offset, bytes));
	    *filelength = *offset + (long)bytes; 
	}
    }

    debug((2,"Storing page to %s at offset %ld\n",filename, *offset));
    if (mserrcode = MS_StorePartialFile(filename, *offset,bytes,0600,FALSE,body_field)) {
        ReportError(VUIMSG_MSSTORE, ERR_WARNING, TRUE);
        return(-1);
    }
    return(0);
}

ShowEntData ()
{
    SetProtectedFields(&entry_fields[0]);

    CursorOff();
    ShowCursor();
    CursorOn();
}

SetSavethepage (curfield)
FIELD *curfield;
{
     ENT_Savethepage=TRUE;
     ENT_Anyloss=TRUE;
}

int ExpandEntryArea(curfield)
FIELD *curfield;
{
    debug((1,"ExpandEntryArea()\n"));
    if (SaveCurrentEntPage() != 0) 
	return (-1);

    if (curfield == &entry_fields[0]) /* are we in headers? */
	return(ExpandEntryPage(ENT_headerfile, &ENT_headoffset,
			       &ENT_headlength, HEAD_field_len, ENT_head_field));

    return(ExpandEntryPage(ENT_filename, &ENT_offset,
			    &ENT_filelength, BODY_field_len, ENT_body_field));
}

int ExpandEntryPage(filename, start_offset, filelength, field_len, body_field)
char *filename;
long *start_offset, *filelength;
int   field_len;
char *body_field;
{
    int  bodylen,rc = 0;
    long bytesunfetched;
    long offset, stop_offset, write_offset;

    debug((1,"ExpandEntryPage()\n"));
    debug((2,"start_offset=%ld, filelength=%ld\n",*start_offset,*filelength));

    offset = *start_offset; /* save current offset in file */

    stop_offset = offset + (field_len / LINE_LENGTH - 1) * LINE_LENGTH;

    *start_offset = *filelength - (*filelength - stop_offset) % field_len;

    while (stop_offset <= *start_offset) {
	debug((2,"stop_offset=%ld, start_offset=%ld\n",stop_offset,*start_offset));
	mserrcode = MS_GetPartialFile(filename, body_field, field_len,
			*start_offset, &bytesunfetched, &bodylen);
	if (mserrcode) {
	    ReportError(VUIMSG_READFILE, ERR_WARNING, TRUE);
	    rc = -1;
	    break;
	}
	body_field[bodylen] = '\0';
	debug((2,"read %d bytes, body_field is {%s}\n",bodylen,body_field));
	write_offset = *start_offset + LINE_LENGTH;

	if (bodylen < field_len) {
	    for (rc=0; rc<bodylen; rc++)
		if (body_field[rc] != ' ') break;
	    if (rc != bodylen) 
		rc = SaveBodyData(field_len, body_field, filename, 
				  field_len, &write_offset, filelength);
	    else {
		rc = 0;
		debug((2,"Skipped saving since it was all blanks\n"));
	    }
	}
	else rc = SaveBodyData(field_len, body_field, filename, 
			       field_len, &write_offset, filelength);
	if (rc) break;
	*start_offset -= (long)field_len;
    }

    *start_offset = offset; /* restore the original offset */

    rc += LoadOneEntPage(filename, *filelength, *start_offset,
			  body_field, field_len, (long *)NIL);
    return (rc ? -1 : 0);
}

int ShrinkEntryArea(curfield)
FIELD *curfield;
{
    debug((1,"ShrinkEntryArea()\n"));
    if ((!ENT_Savethepage) || (SaveCurrentEntPage() != 0)) return(-1);

    if (curfield == &entry_fields[0])
	return(ShrinkEntryPage(ENT_headerfile, &ENT_headoffset,
			       &ENT_headlength, HEAD_field_len, ENT_head_field));

    return(ShrinkEntryPage(ENT_filename, &ENT_offset,
			    &ENT_filelength, BODY_field_len, ENT_body_field));
}

int ShrinkEntryPage(filename, start_offset, filelength, field_len, body_field)
char *filename;
long *start_offset, *filelength;
int   field_len;
char *body_field;
{
    int  bodylen,rc = 0;
    long bytesunfetched;
    long offset;

    debug((2,"start_offset = %ld, filelength=%ld\n",*start_offset,*filelength));

    offset = *start_offset; /* save the current offset into the file */

    *start_offset += (long)(field_len - LINE_LENGTH);

    while (*start_offset < *filelength) {
	debug((2,"start_offset = %ld, filelength=%ld\n",*start_offset,*filelength));
	mserrcode = MS_GetPartialFile(filename, body_field, field_len,
			*start_offset+(long)LINE_LENGTH, &bytesunfetched, &bodylen);
	if (mserrcode) {
	    ReportError(VUIMSG_READFILE, ERR_WARNING, TRUE);
	    rc = -1;
	    break;
	}
	body_field[bodylen] = '\0';
	debug((2,"read %d bytes, offset %ld\n",bodylen,(*start_offset-(long)LINE_LENGTH)));
	if (bodylen <= 0) break;
	if ((rc = SaveBodyData(bodylen, body_field, filename, 
			       field_len, start_offset, filelength)) != 0) break;
	*start_offset += (long)field_len;
    }
    if (!rc) {
	debug((2,"Clearning the last line since everything was pushed up\n"));
	*start_offset = *filelength - (long)LINE_LENGTH; /* blank out last line */
	body_field[0] = '\0';
	rc = SaveBodyData(LINE_LENGTH, body_field, filename, 
			  field_len, start_offset, filelength);
    }
	
    *start_offset = offset; /* restore the original offset in file */
    ENT_Savethepage = TRUE; /* file has changed, current page needs resave */

    rc += LoadOneEntPage(filename, *filelength, *start_offset,
			  body_field, field_len, (long *)NIL);
    return(rc ? -1 : 0);
}

int ReloadHeaderFile()
{
    ENT_headoffset = 0L;

    if (LoadOneEntPage(ENT_headerfile, ENT_headlength, ENT_headoffset,
			ENT_head_field,	HEAD_field_len, (long *)NULL)) return (-1);

    SetProtectedFields(&entry_fields[0]);
    return(0);
}

int AddHeaderToFile(what)
char **what;
{
    int  i,len;
    long newlength, finallength;
    char newfile[MAXPATHLEN+1],outfile[MAXPATHLEN+1], BigBuf[WRITEFILECHUNK+1], *bufptr;

    bufptr = BigBuf;
    for (i=0; what[i] != NIL; i++) {
	strcpy(bufptr, what[i]);
	bufptr += strlen(what[i]);
	*bufptr++ = '\n';
	*bufptr   = '\0';
    }

    len = strlen(BigBuf);
    CUI_GenTmpFileName(newfile);
    if (mserrcode = MS_StorePartialFile(newfile, 0L, len, 0600, TRUE,BigBuf)) {
	ReportError(VUIMSG_MSSTORE, ERR_WARNING, FALSE);
	RealUnlinkViceFile(newfile);
	return(FALSE);
    }
    newlength = len;
    finallength = 0L;

    if ((CompressFile(ENT_headerfile, newfile, &newlength, FALSE, TRUE)) ||
	(DecompressFile(newfile, outfile, &finallength, TRUE, TRUE))) {
	RealUnlinkViceFile(newfile);
	return(FALSE);
    }
	
    ENT_headoffset = 0L;
    ENT_headlength = finallength;
    RealUnlinkViceFile(ENT_headerfile);
    strcpy(ENT_headerfile, outfile);
    
    return(TRUE);
}

FreeEntData()
{
    FreeOneEntry(ENT_filename, &ENT_filelength, &ENT_offset, 
		 &ENT_body_field, &entry_fields[1]);

    FreeOneEntry(ENT_headerfile, &ENT_headlength, &ENT_headoffset,
		 &ENT_head_field, &entry_fields[0]);

    RemoveShowReplyFile();
}

FreeOneEntry(filename, filelength, offset, body_field, fieldname)
char *filename, **body_field;
long *filelength, *offset;
FIELD *fieldname;
{
    if (*body_field != NIL) {
	free(*body_field);
	*body_field = NIL;
	if (fieldname->flocation != (XTENT *)NULL) {
	    free(fieldname->flocation);
	    fieldname->flocation = (XTENT *)NULL;
	}
    }

    if (*filename) {
	RealUnlinkViceFile(filename);
	*filename = '\0';
    }
    *filelength = *offset = 0L;
}


/* *****************************************************************
 *
 *    Message body or file data handling routines
 *
 ***************************************************************** */

MSG_OR_FILE FileStruct;
MSG_OR_FILE MsgStruct;

MSG_OR_FILE *CurrentFile;

Boolean HeaderLineGood;
PRIVATE int BODY_Filter;


int InitMsgOrFile(filename, start_row, start_col, len,file_offset)
char *filename;
int start_row, start_col, len;
long file_offset;
{
struct msg_page_list *newpage;
PANEL *pp;
int row,rc;

    CurrentFile->HeadOffset = 0L;
    CurrentFile->MaxRow = bar_row - start_row -1;
    CurrentFile->BodyLines = (CurrentFile->MaxRow + 2);
    strcpy(CurrentFile->FileName, filename);

    if ((CurrentFile->PageBuf = (char *)malloc(CurrentFile->BodyLines * LINE_LENGTH)) == NIL) {
	ReportError(VUIMSG_NOMEM,ERR_FATAL,FALSE);
	return(-1);
    }

    if (CurrentFile->BodyData == (PANEL *)NULL) {
	CurrentFile->BodyData = (PANEL *)malloc(CurrentFile->BodyLines * sizeof(PANEL));
	if (CurrentFile->BodyData == (PANEL *)NULL) {
	    ReportError(VUIMSG_NOMEM,ERR_FATAL,FALSE);
	    free(CurrentFile->PageBuf);
	    return(-1);
	}
    }

    for (pp = CurrentFile->BodyData, row = start_row; row < bar_row; row++, pp++) {
	pp->prow = row;		pp->pcol = start_col;	pp->pdata = NIL;
	pp->pattr = NORMAL;	pp->plen = len;
    }
    pp->prow = pp->pcol = pp->pattr = pp->plen = 0; pp->pdata = NIL;

    if (LoadOneBodyPage(NIL, &newpage)) {
        FreeFileStruct();
        return(-1);
    }
    CurrentFile->CurrentPage=newpage;
    CurrentFile->FirstPage=newpage;

    while (CurrentFile->CurrentPage->offset < file_offset) {
	rc=LoadOneBodyPage(CurrentFile->CurrentPage->nextpage, &newpage);
	if (!rc && CurrentFile->MaxVisible>=0) {
	    if ((struct msg_page_list *)NIL==CurrentFile->CurrentPage->nextpage) {
		CurrentFile->CurrentPage->nextpage=newpage;
		newpage->prevpage=CurrentFile->CurrentPage;
		CurrentFile->CurrentPage=newpage;
	    } else CurrentFile->CurrentPage=CurrentFile->CurrentPage->nextpage;
	}
	else break;
    }

    return(0);
}

int BodyHomeKey (curfield, maxopts)
int curfield, maxopts;
{
    debug((2,"BodyHomeKey\n"));
    if ((struct msg_page_list *)NIL==CurrentFile->CurrentPage->prevpage) {
        ReportError(VUIMSG_BMSGFILE, ERR_WARNING, FALSE);
    } else {
	if (CurrentFile->ThisIsAMessage)
	    EraseMenuCursor();
        if (!LoadOneBodyPage(CurrentFile->FirstPage, NIL)) {
            CurrentFile->CurrentPage=CurrentFile->FirstPage;
	    if (CurrentFile->ThisIsAMessage)
		ShowBodyData();
	    else
		ShowFileData();
        }
	if (CurrentFile->ThisIsAMessage)
	    ShowMenuCursor();
    }
    return(curfield);
}

int BodyPageUpKey (curfield, maxopts)
int curfield, maxopts;
{
    debug((2,"BodyPageUpKey\n"));
    if ((struct msg_page_list *)NIL==CurrentFile->CurrentPage->prevpage) {
        ReportError(VUIMSG_BMSGFILE, ERR_WARNING, FALSE);
    } else {
	if (CurrentFile->ThisIsAMessage)
	    EraseMenuCursor();
        if (!LoadOneBodyPage(CurrentFile->CurrentPage->prevpage, NIL)) {
            CurrentFile->CurrentPage=CurrentFile->CurrentPage->prevpage;
	    if (CurrentFile->ThisIsAMessage)
		ShowBodyData();
	    else
		ShowFileData();
        }
	if (CurrentFile->ThisIsAMessage)
	    ShowMenuCursor();
    }
    return(curfield);
}

int BodyPageDnKey (curfield, maxopts)
int curfield, maxopts;
{
    int rc;
    struct msg_page_list *newpage;

    debug((2,"BodyPageDnKey\n"));
    if (CurrentFile->ThisIsAMessage)
	EraseMenuCursor();
    rc=LoadOneBodyPage(CurrentFile->CurrentPage->nextpage, &newpage);
    if (!rc && CurrentFile->MaxVisible>=0) {
        if ((struct msg_page_list *)NIL==CurrentFile->CurrentPage->nextpage) {
            CurrentFile->CurrentPage->nextpage=newpage;
            newpage->prevpage=CurrentFile->CurrentPage;
            CurrentFile->CurrentPage=newpage;
        } else CurrentFile->CurrentPage=CurrentFile->CurrentPage->nextpage;
	if (CurrentFile->ThisIsAMessage)
	    ShowBodyData();
	else ShowFileData();
    } else {
        if (!rc) 
	    ReportError("You are at the end of this message/file.",ERR_WARNING, FALSE);
    }
    if (CurrentFile->ThisIsAMessage)
	ShowMenuCursor();
    return(curfield);
}

struct msg_page_list *AllocateMsgPage(this_page, offset, prevpage, nextpage)
long offset;
struct msg_page_list *this_page,*prevpage, *nextpage;
{
    struct msg_page_list *newpage;

    if (this_page != (struct msg_page_list *)NULL)
	return(this_page);

    newpage = (struct msg_page_list *)malloc(sizeof(struct msg_page_list));
    if (newpage == (struct msg_page_list *)NULL) {
	ReportError(VUIMSG_ALLOCPAGE, ERR_CRITICAL, FALSE);
	return((struct msg_page_list *)NULL);
    }

    newpage->offset = offset;
    newpage->prevpage = prevpage;
    newpage->nextpage = nextpage;

    return(newpage);
}

char *FilterHeaders(UsefulStart, offset, bodylen, InHeaders, ReturnBufPtr, linenum)
char *UsefulStart, **ReturnBufPtr;
long *offset;
int  *bodylen, *linenum, *InHeaders;
{
    int   maxlen, startbodylen, line_count;
    char *searched, *PageBufPtr;
    Boolean LineGood;

    LineGood = HeaderLineGood;
    startbodylen = *bodylen;
    PageBufPtr = *ReturnBufPtr;
    line_count = *linenum;

    while ((*InHeaders) && (UsefulStart != NIL)) {
	searched=strchr(UsefulStart,'\n');
	if (searched != NIL) {
	    *searched++ = '\0';
	    *offset += searched - UsefulStart;
	    *bodylen -= searched - UsefulStart;
	} 
	else { /* return and get another full buffer */
	    break;  
	}

	if (((*UsefulStart == ' ' || *UsefulStart == '\t') && (LineGood)) ||
	    ((*UsefulStart != ' ' && *UsefulStart != '\t') && 
	     (LineGood = CheckHead(UsefulStart)))) {
		maxlen = 0;
		while (*UsefulStart) {
		    if (maxlen == 0) {
			line_count++;
			debug((2,"Adding '%.30s' to row %d\n",UsefulStart, line_count));
			*PageBufPtr++ = '\0';
			CurrentFile->BodyData[line_count].pdata = PageBufPtr;
			maxlen = CurrentFile->BodyData[line_count].plen;
		    }
		    if (*UsefulStart == '\t') {
			do {*PageBufPtr++=' ';} while (--maxlen%TABLEN);
			UsefulStart++;
		    } else {
			*PageBufPtr++ = *UsefulStart++;
			maxlen--;
		    }
		}
		*PageBufPtr++ = '\0';
	}
	else LineGood = FALSE;

	UsefulStart = searched;
	if (searched == NIL)
	    *InHeaders = TRUE;
	else 
	    *InHeaders = (*searched != '\n');
	if (!(*InHeaders)) {   /* when we leave headers skip a line */
	    HeaderLineGood = FALSE;
	    CurrentFile->HeadOffset = *offset - 1;
	    if (line_count < CurrentFile->MaxRow) { /* don't do this if end of page */
		line_count++;
		*PageBufPtr++ = '\0';
		CurrentFile->BodyData[line_count].pdata = PageBufPtr;
	    }
	}
	if (line_count == CurrentFile->MaxRow) break;
    }

    if (*InHeaders && *bodylen > 0) {
	if (*bodylen >= startbodylen) { /* must be all garbage */
	    *offset += *bodylen;
	    *bodylen = 0;
	} 
	else if (UsefulStart && *bodylen > strlen(UsefulStart)) { /* nulls in headers */
	    *bodylen -= strlen(UsefulStart);
	    *offset  += strlen(UsefulStart);
	    while (*UsefulStart && *bodylen > 0) {
		*bodylen -= 1;
		*offset  += 1;
	    }
	}
    }
    HeaderLineGood = LineGood;
    *ReturnBufPtr = PageBufPtr;
    *linenum = line_count;
    return (UsefulStart);
}
    
int LoadOneBodyPage (this_page, new_page_p)
struct msg_page_list *this_page, **new_page_p;
{
    char    headbuf[MAXBODY], *PageBufPtr, *UsefulStart;
    int     line_count, bodylen, maxlen, InHeaders;
    long    offset, bytesunfetched;

    debug((1,"LoadOneBodyPage(%ld)\n",this_page));
    PageBufPtr = CurrentFile->PageBuf;

    this_page = AllocateMsgPage(this_page, 0L, (struct msg_page_list *)NULL,
				 (struct msg_page_list *)NULL);

    if (this_page == (struct msg_page_list *)NULL)
	return(-1);

    if (new_page_p != (struct msg_page_list **)NIL) (*new_page_p) = this_page;

    line_count= -1;
    offset = this_page->offset;

    if ((CurrentFile->ThisIsAMessage) && (BODY_Filter))
	InHeaders = offset <= CurrentFile->HeadOffset;
    else InHeaders = FALSE;
    maxlen = 0;

    do {
        debug((2,"Reading %s at offset %ld\n", CurrentFile->FileName, offset));
        if (MS_GetPartialFile(CurrentFile->FileName, headbuf,MIN(CurrentFile->BodyLines*LINE_LENGTH-1,MAXBODY-1), offset, &bytesunfetched, &bodylen) != 0) return(-1);
        debug((2,"Returned %d bytes and %ld are left\n",bodylen, bytesunfetched));

	if (bodylen <= 0) break; /* empty buffer? */

        headbuf[bodylen] = '\0';
	
        UsefulStart = FilterHeaders(headbuf, &offset, &bodylen, &InHeaders,
				    &PageBufPtr, &line_count);

        if ((UsefulStart == NIL) ||
	     ((line_count == CurrentFile->MaxRow) && (InHeaders)))
	    break;

	if ((InHeaders) && (bodylen > 0) && (bytesunfetched > 0)) continue;

        while (bodylen>0) {
	    if (!*UsefulStart) {
		UsefulStart++;
		bodylen--;
		offset++;
		continue;
	    }
            if (maxlen==0 || *UsefulStart=='\n') {
                if (line_count==CurrentFile->MaxRow) break;
                if (*UsefulStart=='\n') {
                    UsefulStart++;
                    offset++;
		    bodylen--;
                }
                line_count++;
                *PageBufPtr++ = '\0';
                CurrentFile->BodyData[line_count].pdata=PageBufPtr;
                maxlen=CurrentFile->BodyData[line_count].plen;
		continue;
            }
            offset++;
	    bodylen--;
            if (*UsefulStart == '\t') {
                do {*PageBufPtr++=' ';} while (--maxlen%TABLEN);
                UsefulStart++;
            } else {
                *PageBufPtr++ = *UsefulStart++;
                maxlen--;
            }
        }
    } while (bytesunfetched > 0 && line_count<CurrentFile->MaxRow);

    *PageBufPtr='\0';

    if (bodylen < 0) {
	ReportError(VUIMSG_MSGREAD, ERR_WARNING, FALSE);
    }

    if ((bodylen == 0) && (offset == 0)) {
        ReportError("The message is empty", ERR_WARNING, FALSE);
    }
    debug((2,"We picked up %d lines\n",line_count));
    if ((CurrentFile->MaxVisible = line_count++) < 0) return(0);
    *(++PageBufPtr)='\0';
    while (line_count<=CurrentFile->MaxRow) {
         CurrentFile->BodyData[line_count++].pdata = PageBufPtr;
    }
    debug((2,"So %d lines are visible and the rest null\n",CurrentFile->MaxVisible));

    this_page->nextpage = AllocateMsgPage(this_page->nextpage, offset, this_page,
					   (struct msg_page_list *)NULL);
    if (this_page->nextpage == (struct msg_page_list *)NULL)
	return (-1);
    
    debug((2,"Next page (addr %ld) will be at offset %ld\n", 
	    this_page->nextpage->offset, offset));

    return(0);
}

FreeFileStruct()
{
    struct msg_page_list *p, *q;

    if (CurrentFile->CurrentPage != (struct msg_page_list *)NIL)
	CurrentFile->HeadOffset = CurrentFile->CurrentPage->offset;
    if (CurrentFile->FirstPage !=(struct msg_page_list *)NIL) {
	for (p=CurrentFile->FirstPage; p!=(struct msg_page_list *)NIL; ) {
	    q=p;
	    p=p->nextpage;
	    free(q);
	}
	CurrentFile->FirstPage=(struct msg_page_list *)NIL;
    }
    if (CurrentFile->PageBuf != NIL) {
	free(CurrentFile->PageBuf);
	CurrentFile->PageBuf=NIL;
    }

    if (CurrentFile->ThisIsAMessage)
	MS_UnlinkFile(CurrentFile->FileName);
    if (CurrentFile->BodyData != (PANEL *)NULL) {
	free(CurrentFile->BodyData);
	CurrentFile->BodyData = (PANEL *)NULL;
    }
}   


/* *****************************************************************
 *
 *    Message body data handling routines
 *
 ***************************************************************** */

InitBodyData (msgno, head_filtering)
long msgno; 
int head_filtering;
{
    char filename[MAXPATHLEN+1];

#ifdef METAMAIL_ENV
#if defined(POSIX_ENV) || defined(USE_CURSES_AND_TERMIOS)
#include <curses.h>
#include <termios.h>
#define gtty(f, tty_p) tcgetattr(f, tty_p)
#define stty(f, tty_p) tcsetattr(f, TCSANOW, tty_p)
#else
#include <sgtty.h>
#include <curses.h>
#endif
    SGTTY ttystatein, ttystateout;

        char ctype[100], TmpFileName[1+MAXPATHLEN], Cmd[1+MAXPATHLEN];
        int ShouldDelete, cuid = CuidFromMsgno(msgno), code;

        if (CUI_GetHeaderContents(cuid,(char *) NULL, HP_CONTENTTYPE, ctype, sizeof(ctype) - 1) != 0) {
            /* error already reported */
            return(-1);
        }
        if (!getenv("NOMETAMAIL") && ctype[0] && ULstrncmp(ctype, "x-be2", 5) && nontext(ctype)) {
            if (CUI_GetBodyToLocalFile(cuid, TmpFileName, &ShouldDelete)) {
                return(-1); /* error reported */
            }
            sprintf(Cmd, "reset; metamail -m vui -p %s %s", ShouldDelete ? "-z" : "", TmpFileName);
            gtty(fileno(stdin), &ttystatein);
            gtty(fileno(stdout), &ttystateout);
            code = system(Cmd);
            stty(fileno(stdin), &ttystatein);
            stty(fileno(stdout), &ttystateout);
            RedrawScreen(0); 
	    UpdateMsgData(msgno, 1); 
	    return(MENU_EMPTY);
        }
#endif
    debug((1,"InitBodyData(%ld, %d)\n", msgno, head_filtering));
    BODY_Filter=head_filtering;
    HeaderLineGood = FALSE;
    CurrentFile = &MsgStruct;
    CurrentFile->ThisIsAMessage = TRUE;

    if (WriteUnscribedBody(msgno,filename) == -1) return (MENU_EMPTY);

    if ((InitMsgOrFile(filename,0,0,80,0L) != 0) || (CurrentFile->MaxVisible < 0)) {
        FreeFileStruct();
        return(MENU_EMPTY);
    }

    return(0);
}

WriteUnscribedBody(msgno,filename)
long msgno;
char *filename;
{
    char snapshot[AMS_SNAPSHOTSIZE];

    if (mserrcode = MS_GetNthSnapshot(MSG_DirName, msgno, snapshot)){
	ReportError(VUIMSG_SNAPSHOT, ERR_WARNING, TRUE);
	return(-1);
    }
   
    if (mserrcode = MS_WriteUnscribedBodyFile(MSG_DirName,  AMS_ID(snapshot), filename)) {
	ReportError("Could not create viewable message.\n", ERR_WARNING, TRUE);
	return(-1);
    }
    return(0);
}

ScrambleBody (curfield, maxopts)
int curfield, maxopts;
{
int i, j, ch;
char   *c;
   debug((1,"ScrambleBody()\nLine ..."));
   for (i=0; i<=CurrentFile->MaxVisible; i++) {
        c=CurrentFile->BodyData[i].pdata;
        for (j=CurrentFile->BodyData[i].plen; j && *c; j--, c++) {
            if (*c >= 'a' && *c <= 'z') {
                ch = *c + 13;
                if ( ch > 'z' ) ch -= 26;
                (*c) = ch;
            } else {
                if (*c >= 'A' && *c <= 'Z') {
                    ch = *c + 13;
                    if ( ch > 'Z') ch -= 26;
                    (*c) = ch;
                }
            }
        }
   }
   ShowBodyData();
   return(curfield);
}

ShowBodyData ()
{
    debug((1,"ShowBodyData\n"));
    DrawPanel(CurrentFile->BodyData, NIL, PANEL_NOCLEAR);
    return(0);
}

FreeBodyData ()
{
    CurrentFile = &MsgStruct;
    FreeFileStruct();
}

/* *****************************************************************
 *
 *    File data handling routines
 *
 ***************************************************************** */

extern jmp_buf filepanel;

FIELD *FileEscKey (curfield)
FIELD *curfield;
{
    longjmp(filepanel, -1);
    return((FIELD *)NIL); /* Actually never reached. Put in to avoid hc warnings */
}

FIELD *FilePageDnKey(curfield)
FIELD *curfield;
{
    BodyPageDnKey(0,0);
    return(curfield);
}

FIELD *FilePageUpKey(curfield)
FIELD *curfield;
{
    BodyPageUpKey(0,0);
    return(curfield);
}

FIELD *FileHomeKey(curfield)
FIELD *curfield;
{
    BodyHomeKey(0,0);
    return(curfield);
}


InitFileData (filename, start_row, start_col, len,file_offset)
char *filename;
int start_row, start_col, len;
long file_offset;
{
    debug((1,"InitFileData(%s)\n", filename));
    CurrentFile = &FileStruct;
    CurrentFile->ThisIsAMessage = FALSE;
    if ((InitMsgOrFile(filename,start_row,start_col, len,file_offset) != 0) || 
	 (CurrentFile->MaxVisible < 0)) {
        FreeFileStruct();
        return(-1);
    }
    return(0);
}


ShowFileData ()
{
int i;
PANEL *pp;
    debug((1,"ShowFileData\n"));
    pp=CurrentFile->BodyData;
    for (i=0; i<=CurrentFile->MaxRow; i++, pp++)
        ShowString (pp->pdata, pp->prow, pp->pcol, pp->plen, pp->pattr);
    return(0);
}

FreeFileData ()
{
    CurrentFile = &FileStruct;
    FreeFileStruct();
    CurrentFile = &MsgStruct;
}

int SaveFileOffset(where)
long *where;
{
    *where = FileStruct.HeadOffset;
    return 0;
}

/* The following is a hack to avoid re-entrancy problem in getnuser  */
extern jmp_buf inppanel;
jmp_buf save_inppanel;

PrintFileDataLocal (FileName)
char *FileName;
{
#ifdef IBMPC
char    headbuf[MAXBODY], FullName[MAXPATHLEN+1];
int     bodylen, rc, i;
long    offset, bytesunfetched;
    if (MS_DisambiguateFile(FileName, FullName, AMS_DISAMB_FILEEXISTS)) {
        ReportError("Can't find file by that name.", ERR_WARNING, FALSE);
        return(-1);
    }

    for (i=0; i<_JBLEN; i++) save_inppanel[i] = inppanel[i];
    rc=GetBooleanFromUser("Ready to print on your local printer; OK?", TRUE);
    for (i=0; i<_JBLEN; i++) inppanel[i] = save_inppanel[i];
    if (!rc) return(-1);
    if (!PrinterAvailable()) {
	ReportError("Unable to locate your local printer.",ERR_WARNING,FALSE);
	return(-1);
    } 
    setmode(fileno(stdprn), O_TEXT);  /* change to text on the printer */
    ReportSuccess("Printing...");
    offset = 0L;
    do {
        debug((2,"Getting page for %s at offset %ld\n",FileName, offset));
        if ((mserrcode = MS_GetPartialFile(FullName, headbuf,
                 MAXBODY-1, offset, &bytesunfetched, &bodylen)) != 0) {
            ReportError(VUIMSG_READFILE,ERR_WARNING,TRUE);
            return(-1);
        }
        debug((2,"Returned %d bytes and %ld are left\n",bodylen, bytesunfetched));
        if (bodylen <= 0) break;
        headbuf[bodylen] = '\0';
        rc=fputs(headbuf, stdprn);
        fflush(stdprn);
        if (rc==EOF) {
            ReportError("Error writing locally...", ERR_WARNING, FALSE);
            return(-1);
        }
        debug((2,"Wrote %d bytes.\n",bodylen));
        offset += bodylen;
    } while (bytesunfetched > 0);
    fputs("\f\n", stdprn);  /* Emit form feed.  */
    fflush(stdprn);
    ClearError();
#endif
    return(0);
}

extern int DebugKey(), HelpKey();
extern FIELD *DebugKey2(), *HelpKey2(), *PrintKey2(), *ShowReplyMsg();

MKEYTAB dir_keys[] = {
    KEYCODE_PAGE_DOWN,  DirPageDnKey,       /* PgDn        */
    KEYCODE_PAGE_UP,    DirPageUpKey,       /* PgUp        */
    KEYCODE_UP,         DirUpArrow,         /* UpArrow     */
    KEYCODE_DOWN,       DirDnArrow,         /* DownArrow   */
    KEYCODE_HOME,       DirHomeKey,         /* Home        */
    KEYCODE_END,        DirEndKey,          /* End         */
#ifdef DEBUG
    KEYCODE_ALT_F1,     DebugKey,           /* Alt-F1      */
#endif
    KEYCODE_F1,         HelpKey,            /* F1          */
    0, 0};

MKEYTAB msg_keys[] = {
    KEYCODE_PAGE_DOWN,  MsgPageDnKey,       /* PgDn        */
    KEYCODE_PAGE_UP,    MsgPageUpKey,       /* PgUp        */
    KEYCODE_UP,         MsgUpArrow,         /* UpArrow     */
    KEYCODE_DOWN,       MsgDnArrow,         /* DownArrow   */
    KEYCODE_HOME,       MsgHomeKey,         /* Home        */
    KEYCODE_END,        MsgEndKey,          /* End         */
#ifdef DEBUG
    KEYCODE_ALT_F1,     DebugKey,           /* Alt-F1      */
#endif
    KEYCODE_F1,         HelpKey,            /* F1          */
    0, 0};

MKEYTAB body_keys[] = {
    KEYCODE_PAGE_DOWN,  BodyPageDnKey,      /* PgDn        */
    KEYCODE_PAGE_UP,    BodyPageUpKey,      /* PgUp        */
    KEYCODE_HOME,       BodyHomeKey,        /* Home        */
#ifdef DEBUG
    KEYCODE_ALT_F1,     DebugKey,           /* Alt-F1      */
#endif
    KEYCODE_F1,         HelpKey,            /* F1          */
    KEYCODE_F2,         ScrambleBody,       /* F2          */
    0, 0};

KEYTAB data_keys[] = {
    KEYCODE_ESCAPE,     EntryEscKey,        /* Esc         */
    KEYCODE_PAGE_DOWN,  EntryPageDnKey,     /* PgDn        */
    KEYCODE_PAGE_UP,    EntryPageUpKey,     /* PgUp        */
#ifdef DEBUG
    KEYCODE_ALT_F1,     DebugKey2,          /* Alt-F1      */
#endif
    KEYCODE_F1,         HelpKey2,           /* F1          */
    KEYCODE_F2,		ShowReplyMsg,	    /* F2   */
    0, 0};

KEYTAB file_keys[] = {
    KEYCODE_PAGE_DOWN,  FilePageDnKey,      /* PgDn        */
    KEYCODE_PAGE_UP,    FilePageUpKey,      /* PgUp        */
    KEYCODE_HOME,       FileHomeKey,        /* Home        */
    KEYCODE_ESCAPE,     FileEscKey,         /* Esc         */
#ifdef DEBUG
    KEYCODE_ALT_F1,     DebugKey2,          /* Alt-F1      */
#endif
    KEYCODE_F2,         PrintKey2,          /* F2          */
    0, 0};

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
    t = (char *) strchr(s, ';');
    while (t) {
        ++t;
        while (*t && isspace(*t)) ++t;
        if (!strncmp(t, "charset", 7)) {
            s = (char *) strchr(t, '=');
            if (s) {
                ++s;
                while (*s && isspace(*s)) ++s;
                if (!strncmp(s, "us-ascii", 8)) return(0);
            }
            return(1);
        }
        t = (char *) strchr(t, ';');
    }
    return(0); /* no charset, was text/plain */
}
#endif
