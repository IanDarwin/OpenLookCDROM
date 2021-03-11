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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/help/src/RCS/help.c,v 2.112 1993/11/09 21:26:11 gk5g Exp $";
#endif

/* $ACIS$ */

 


/*---------------------------------------------------------------------------*/
/*									     */
/*		          	ATK Help Program			     */
/*									     */
/*	History:							     */
/*		original be2 version: Mike Kazar, c. 1985		     */
/*									     */
/*		complete ATK rewrite: Marc Pawliger, 2/89		     */
/*									     */
/*	See README for programmer details				     */
/*									     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/*	MODULE: help.c							     */
/*		Subclass of view, includes all major lookup and display      */
/*		routines.						     */
/*---------------------------------------------------------------------------*/

#include <class.h>

#define label gezornenplatz
/* sys/types.h in AIX PS2 defines "struct label", causing a type name clash.
   Avoid this by temporarily redefining "label" to be something else in the preprocessor. */
#include <andrewos.h> /* sys/types.h sys/file.h */
#undef label

#include <help.eh>
#include <bind.ih>
#include <proctbl.ih>
#include <dataobj.ih>
#include <environ.ih>
#include <filetype.ih>
#include <im.ih>
#include <label.ih>
#include <labelv.ih>
#include <lpair.ih>
#include <menulist.ih>
#include <keymap.ih>
#include <message.ih>
#include <panel.ih>
#include <rofftext.ih>
#include <scroll.ih>
#include <text.ih>
#include <textv.ih>
#include <attribs.h>
#include <view.ih>

#include <errno.h>
#include <ctype.h>
#include <sys/errno.h>
#include <sys/param.h>

#include <index.h>
#include <regexp.h> /* use the reg expression routines in overhead */

#include <config.h>
#include <helpsys.h>
#include <help.h>
#include <helpdb.ih>

/*---------------------------------------------------------------------------*/
/*				GLOBALS					     */
/*---------------------------------------------------------------------------*/

/* the new menu and key states */
struct keymap *Help_Map;
struct menulist *Help_Menus;

char *help_tutorialDirs[MAX_TUTORIAL_DIRS];
char help_changesDir[MAXPATHLEN];

/* a list of instances of help */
struct self_help *help_ego = (struct self_help *)NULL;

struct cursor *help_waitCursor; /* the watch cursor */

char **help_panelList = NULL; /* used for enumerating across the help index */
int help_panelIndex = 0, help_panelListSize = help_MAXPANEL;

/* hooks to textview and frame procs */
void (*help_textSearch)() = (void (*)())NULL;
void (*help_textRevSearch)() = (void (*)())NULL;
void (*help_textSearchAgain)() = (void (*)())NULL;
void (*help_textCopyRegion)() = (void (*)())NULL;
void (*help_textPageDown)() = (void (*)())NULL;
void (*help_textPageUp)() = (void (*)())NULL;
void (*help_frameSetPrinter)() = (void (*)())NULL;

#ifdef CONTRIB_ENV
void (*help_poptPostWindow)() = (void (*)())NULL;
#endif

extern void help_aux_AddSearchDir();
extern void help_aux_AddBookmark();
extern void help_aux_ExitProc();
extern void help_aux_Print();
extern void help_aux_NewHelp();

extern void init_hlptextview();

void SetupMenus();
static void TogglePanels();
static void ToggleOverviews();
static void TogglePrograms();
static void ToggleHistory();
static void SortAndMakePanel();
static char *AddToPanelList();
static void RestorePanel();

static int packedString[] = {037, 036, 0, 0};
static int compressedString[] = {037, 0235, 0220};
static int gzippedString[] = {037, 0213, 010, 010};
struct filterinfo {
    char *cmd;
    int len;
    int *magic;
    boolean possible;
};
struct filterinfo filters[] = {
    {"zcat", 3, compressedString, 0},
    {"pcat", 4, packedString, 0},
    {"gunzip", 4, gzippedString, 0},
    {0, 0, 0, 0}
};
char *colcmd = "col -b | tr '\t' ' '";

/*---------------------------------------------------------------------------*/
/*			CONDITIONAL DEBUGGING				     */
/*---------------------------------------------------------------------------*/

#ifdef DEBUGGING
/*
 * debugging statements can be included by compiling add modules with
 * -DDEBUGGING.  Debugging output is toggled by existance/nonexistance
 * of the environment variable HELPDEBUG.
 */
int HELPDEBUG = 0;
#undef DEBUG
#define DEBUG(arg) if (HELPDEBUG != 0) { printf arg; fflush(stdout); }
#else
#undef DEBUG
#define DEBUG(arg)
#endif /* DEBUGGING */


/*---------------------------------------------------------------------------*/
/*				UTILITY FUNCTIONS			     */
/*---------------------------------------------------------------------------*/

/*
 * copy protoname into aresult, prepending /usr/andy or whatever,
 * as appropriate
 */
static char *
AndyCopy(aproto, aresult)
register char *aproto, *aresult;
{
    register char *tp;

    tp = environ_AndrewDir(aproto);
    strcpy(aresult, tp);
    return aresult;
}

/*
 * allocate new string
 */
static char *
CopyString(as)
register char *as;
{
    register char *tp;
    tp = (char*) malloc(strlen(as)+1);
    if (!tp)
	return NULL;
    strcpy(tp, as);
    return tp;
}

/*
 * parse comma terminated field followed by new-line terminated field
 */
static int 
ScanLine(afile, ae1, ae2)
register FILE *afile;
char *ae1, *ae2;
{
    register int state;		/* 0->reading ae1, 1->reading ae2 */
    register int tc;		/* char we're reading */
    register char *tp;		/* points to string */

    state = 0;
    tp = ae1;
    while(1) {
        tc = getc(afile);
        if (tc < 0 || tc == '\n') {
            /* done */
            if (state == 1) {
                *tp++ = 0;
                return 2;
            } else return 0;
        }
        if (state == 0 && tc == ',') {
            state = 1;
            *tp++ = 0;
            tp = ae2;
            continue;
        }
        *tp++ = tc;
    }
}

/* just like system, but fail if the command to be executed has a '`' in it. */
static int safesystem(acmd)
char *acmd;
{
    if(index(acmd, '`')) {
	fprintf(stderr, "help: command execution failed due to illegal character '`' in command.\n");
	return -1;
    }
    return system(acmd);
}

/*
 * just like system(3) only closes fds 3..., and doesn't wait
 */
static int 
mysystem(acmd)
register char *acmd;
{
    register long pid;
    if(index(acmd, '`')) {
	fprintf(stderr, "help: command execution failed due to illegal character '`' in command.\n");
	return -1;
    }
    pid = osi_vfork();
    if (pid < 0) return -1;
    else if (pid == 0) {
        /* child, next close window mgr's fd, so that parent window can be killed */
        for(pid = 3; pid < getdtablesize(); pid++) close(pid);
        execl("/bin/sh", "sh", "-c", acmd, NULL);
        _exit(127);
	/*NOTREACHED*/
    }
    else return 0;      /* parent, success */
}


/*
 * lowercases's a string.
 */
char *
LowerCase(astring)
register char *astring;
{
    register char *tp = astring;

    while (tp && *tp != NULL)
	if (isupper(*tp)) {
	    *tp = tolower(*tp);
	    tp++;
	} else
	    tp++;
    return astring;
}


/*
 * maps string(n) to string.n in place
 */
static char *
MapParens(s)
char *s;
{
    char *lpp, *rpp;

    if (((lpp = rindex(s, '(')) != NULL) &&  /* has a ( */
	((rpp = rindex(s, ')')) != NULL) &&  /* and a ) */
	(*(rpp+1) == '\0')) {			     /* ) is the last char */
	*lpp = '.';
	*rpp = '\0';
	return s;
    }
}


/*
 * stolen from libcs.  Returns the index of string small in big, 0 otherwise
 */
static char *
sindex(big, small) 
char *big, *small;
{
    register char *bp, *bp1, *sp;
    register char c = *small++;

    if (c==0) return(0);
    for (bp=big;  *bp;  bp++)
	if (*bp == c) {
	    for (sp=small,bp1=bp+1;   *sp && *sp == *bp1++;  sp++)
		;
	    if (*sp==0) return(bp);
	}
    return 0;
}

/*
 * add an item to the history buffer
 */
void 
AddHistoryItem (self, marcp, flash)
register struct help *self;
int marcp;			/* is this a bookmark? */
int flash;			/* should we expose the history panel? */
{
    struct history_entry *ent;
    register struct cache *c = self->info;

    DEBUG(("addhist\n"));
    if (c->histent == NULL || c->histent[0] == '\0')
	return;

    ent = (struct history_entry *) malloc (sizeof(struct history_entry));
    if (ent == NULL)
	return;

    ent->pos = textview_GetDotPosition((struct textview *)c->view),
    ent->top = textview_GetTopPosition((struct textview *)c->view),
    ent->sellen = textview_GetDotLength((struct textview *)c->view),
    ent->fname = (char *) malloc (strlen(c->histent) + 1);
    strcpy(ent->fname, c->histent);

    if (marcp == help_HE_HISTORY) { /* normal history add */
	
	/* now add the item */
	c->lastHist = panel_Add(c->hist, c->histent, ent, TRUE);
    } else {			/* a bookmark */
	char tfname[HNSIZE + 11];

	sprintf(tfname, "%s @ %d", c->histent, ent->top);
	/* now add the item */
	c->lastHist = panel_Add(c->hist, tfname, ent, TRUE);
    }

    if (flash == help_SHOW_HIST)
	ToggleHistory(self, help_SHOW_HIST);

    DEBUG(("OUT addhist\n"));
}


/*---------------------------------------------------------------------------*/
/*				MAIN FUNCTIONS				     */
/*---------------------------------------------------------------------------*/

/*
 * Help's file displayer - given a filename, attempts to find out if
 * it's a roff file (1st char '.' or '#' or '\'') and read it in using rofftext.
 * Otherwise, assumes it's text or ATK format and lets text_read take care
 * of the dirty work of dealing with that.
 *
 * Can display any file that uses a textview or a subclass of textview.
 * Creates a new view and dataobject for the file and reads the file into the
 * new object.
 *
 * Also takes care of adding elements to the history panel for the last-shown
 * help file.
 */
static int 
ShowFile(self, afilename, amoreFlag, hist)
register struct help *self;
register char *afilename;	/* the file */
int amoreFlag;			/* put "(more)" in the titlebar?
				   TRUE - yes, FALSE - no,
				   help_USE_OLD_TITLE - use last setting */
int hist;			/* add this previous file to the history?
				   help_HIST_NOADD - none at all,
				   help_HIST_NAME - aname,
				   help_HIST_TAIL - tail of the found filename
				   */
{
    register FILE *fd;
    long objectID;
    static char *manfiles[] = MACFILES;
    char **manptr = manfiles;
    char tbuffer[MAXPATHLEN];
    char isTroffFormat = 0;
    int tc;
    static int lastmore;
    char outfile[MAXPATHLEN];
    boolean inCat;
    int pos;
    int useFilter;
    boolean checkFilters;
    boolean hlp_view = FALSE;
    int ind;
    boolean reOpen;
    char *p, *nextp;
    char *realfilename = afilename;


    struct attributes attrs, *attr;

    char *viewName, *objName, *tmp;

    struct cache *c = self->info; /* view and dataobject to replace with new file object */

    struct view *newview;
    struct dataobject *newdata;
    struct view *oldview = c->view;
    struct dataobject *olddata = c->data;

    if (message_Asking(help_ego->this->info->view)) {
	DEBUG(("retracting\n"));
	message_CancelQuestion(help_ego->this->info->view);
    }

    /* Check if file exists */
    if (!(fd = fopen(realfilename, "r"))) {
	char buf[HELP_MAX_ERR_LENGTH *2];
	
	if (access(realfilename, 4) < 0) {
	    if (errno == ETIMEDOUT)
		sprintf(buf, "%s %s", err_server, err_read);
	    else
		sprintf(buf, err_missing, afilename);
	    ERRORBOX(c->view, buf);
	} else ERRORBOX(c->view, err_read);
	return 0;
    }
    /* is it troff ? */
    tc = getc(fd);

    useFilter = -1;
    checkFilters = FALSE;
    reOpen = FALSE;
    for (ind = 0; filters[ind].cmd != 0; ind++) {
	if ((filters[ind].possible = (tc == *filters[ind].magic))) {
	    checkFilters = TRUE;
	    if (filters[ind].len == 1) {
		useFilter = ind;
	    }
	}
    }

    if (checkFilters) {
	/* possible filter to be applied */
	pos = 1;
	reOpen = TRUE;
	while (checkFilters && useFilter == -1) {
	    tc = getc(fd);
	    checkFilters = FALSE;
	    for (ind = 0; filters[ind].cmd != 0; ind++) {
		if (filters[ind].possible) {
		    if ((filters[ind].possible = (tc ==	filters[ind].magic[pos])))  {
			checkFilters = TRUE;
			if (filters[ind].len == pos + 1) {
			    useFilter = ind;
			}
		    }
		}
	    }
	    pos++;
	}
    }

    inCat = FALSE;
    for (p = index(afilename, '/'); p != NULL; p = nextp) {
	p++;
	nextp = index(p,'/');
	if (strncmp(p, "man", 3) == 0 && nextp) {
	    inCat = FALSE;
	}
	else if (strncmp(p, "cat", 3) == 0 && nextp) {
	    inCat = TRUE;
	}
    }

    if (useFilter >= 0 || inCat) {
	static int count;
	char cmd[MAXPATHLEN + 50];

	fclose(fd);
	reOpen = TRUE;
	sprintf(outfile, "/tmp/hfil.%d.%d", getpid(), count++);

	if (useFilter >= 0 && inCat) {
	    sprintf(cmd, "%s < %s | %s > %s", filters[useFilter].cmd, afilename, colcmd, outfile);
	}
	else if (useFilter >= 0) {
	    sprintf(cmd, "%s < %s > %s", filters[useFilter].cmd, afilename, outfile);
	}
	else {
	    sprintf(cmd, "cat %s | %s > %s", afilename, colcmd, outfile);
	}
	safesystem(cmd);
	realfilename = outfile;
    }
    else if (reOpen) {
	fclose(fd);
    }


    if (reOpen) {
	if (! (fd = fopen(realfilename, "r"))) {
	    char buf[HELP_MAX_ERR_LENGTH *2];

	    if (access(realfilename, 4) < 0) {
		if (errno == ETIMEDOUT)
		    sprintf(buf, "%s %s", err_server, err_read);
		else
		    sprintf(buf, err_missing, realfilename);
		ERRORBOX(c->view, buf);
	    } else ERRORBOX(c->view, err_read);
	    return 0;
	}
	if (realfilename == outfile) {
	    unlink(realfilename);
	}
	tc = getc(fd);
    }

    ungetc(tc, fd);
    isTroffFormat = (tc == '.' || tc == '#' || tc == '\'');

    /* what kind of object does this file want ? (don't touch attributes) */
    objName = filetype_Lookup(NULL, realfilename, NULL, NULL);
    if ((newdata = (struct dataobject *) class_NewObject(objName)) ==
	(struct dataobject *)NULL) {
	sprintf(tbuffer, err_dobj, objName);
	ERRORBOX(c->view, tbuffer);
	return 0;
    }

    /* what kind of view do we need ? */
    viewName = dataobject_ViewName(newdata);
    if (strcmp(viewName, "textview") == 0) {
	hlp_view = TRUE;
	viewName = "hlptextview";   /* Provides hypertext functions */
    }
    if ((newview = (struct view *)class_NewObject(viewName)) ==
	(struct view *)NULL) {
	sprintf(tbuffer, err_view, viewName);
	ERRORBOX(c->view, tbuffer);
	dataobject_Destroy(newdata);
	return 0;
    }
    if (hlp_view)
	init_hlptextview(newview);  /* Put it in helpaux because this file already almost blows cpp. */

    /* is it a subclass of textview? */
    if (!class_IsTypeByName(class_GetTypeName(newview), "textview")) {
	/* not text, punt */
	sprintf(tbuffer, err_nontext, viewName);
	ERRORBOX(c->view, tbuffer);
	view_Destroy(newview);
	dataobject_Destroy(newdata);
	return 0;
    }

    /* hook 'em up */
    view_SetDataObject(newview, newdata);

    /* since now committed, take care of the history for previous item, if we
       haven't already done so, and we should */

    if (c->histat != help_HIST_NOADD) {

	DEBUG(("hist... getting hist on %d\n", (int)c->view));
	
	/* save position of last view's top, dot and dotlen */
	
	AddHistoryItem(self, help_HE_HISTORY, help_IGNORE_TOGGLE);
	
	/* only add this entry once */
	c->histat = help_HIST_NOADD;
    }

    attrs.next = (struct attributes *)NULL;
    attrs.key = "readonly";
    attrs.value.integer = 0;
    text_SetAttributes((struct text *)newdata, &attrs);
    fseek(fd, 0 ,0);	/* somebody moved the pointer */
    attr = NULL;
    filetype_Lookup(fd, realfilename, &objectID, attr);
    if (attr != NULL)
	text_SetAttributes((struct text *)newdata, attr);
    if (!isTroffFormat) {
	if (text_Read((struct text *)newdata, fd, objectID) != dataobject_NOREADERROR) {
	    ERRORBOX(c->view, err_read);
	    view_Destroy(newview);
	    dataobject_Destroy(newdata);
	    return 0;
	}
    } else {			/* it's troff */
	char *manroot = "/usr/man", *slash = NULL;

	/* check if the manfiles are absolute or relative paths.
	   If 1st char is '/', path is absolute, OW, we andycopy it
	   into a temp buffer, since it will be relative to ANDREWDIR */

	while (*manptr) {
	    if (**manptr != '/') {
		tmp = (char *)malloc(strlen(*manptr)+2);
		strcpy(tmp, "/");
		strcat(tmp, *manptr);
		*manptr = AndyCopy(tmp, (char *)malloc(MAXPATHLEN));
		free(tmp);
	    }
	    manptr++;
	}
	message_DisplayString(c->view, 0, msg_converting);
	im_ForceUpdate();

	/* ======== START of change to root of man pages ========== */
	slash = (char*)strchr(afilename, '/');
	while(slash && (slash+1)) {
	    if(!strncmp(slash+1,"man",3) && *(slash+4) == '/') {
		/* found substring "man" somewhere in afilename; chdir there */
		slash += 4;
		*slash = '\0';
		manroot = afilename;
		break;
	    }
	    else 
		slash = (char*)strchr(slash + 1, '/');
	}
	if(!slash) {	/* troff file not located in standard man path */
	    /* Chdir to directory containing afilename */
	    manroot = afilename;
	    slash = rindex(afilename,'/');
	    if(slash) *slash = '\0';
	}
	chdir(manroot);
	if(slash) *slash = '/';
	/* ======== END of change to root of man pages ========== */

	if (rofftext_ReadRoffIntoText((struct text *)newdata, fd, 0, manfiles) != dataobject_NOREADERROR) {
	    ERRORBOX(c->view, err_read);
	    view_Destroy(newview);
	    dataobject_Destroy(newdata);
	    return 0;
	}
	message_DisplayString(c->view, 0, "");
    }
    textview_SetTopPosition((struct textview *)newview, 0);
    textview_SetDotPosition((struct textview *)newview, 0);
    textview_SetDotLength((struct textview *)newview, 0);
    attrs.value.integer = 1;
    text_SetAttributes((struct text *)newdata, &attrs);

    /* now swap the old with the new */
    DEBUG(("swapping..."));
    view_UnlinkTree(c->view);
    c->view = newview;
    c->data = newdata;

    /* now set up the history state */

    c->histat = hist;

    switch (hist) {
      case help_HIST_NAME:
	strcpy(c->histent, c->name);
	break;
      case help_HIST_NOADD:	/* do this so that bookmarks will work correctly */
      case help_HIST_TAIL:
	tmp = rindex(afilename,'/');
	strcpy(c->histent, (tmp != NULL) ? tmp+1 : afilename);
    }
    DEBUG(("setting histent to: %s\n", c->histent));

    /* now hook the new view up to the scrollbar */
    scroll_SetView(c->scroll, c->view); /* does a linktree */
    textview_SetEmbeddedBorder((struct textview *) c->view, 20, 5);
    dataobject_NotifyObservers(c->data, (long)0);

    /* get rid of the old view and dataobject*/
    DEBUG(("destroying... %d ",(int)oldview));
    view_Destroy(oldview);
    dataobject_Destroy(olddata);

    strcpy(tbuffer, afilename);

    DEBUG(("titling\n"));

    if ((amoreFlag == TRUE) || ((amoreFlag == help_USE_OLD_TITLE) && lastmore == TRUE))
	strcat(tbuffer, " (more)");
    if (lastmore != help_USE_OLD_TITLE)
	lastmore = amoreFlag;
    im_SetTitle(view_GetIM((struct view *)c->view), tbuffer);
    
    super_WantInputFocus(self, c->view);
    
    fclose(fd);

    DEBUG(("showfile returning\n"));
    return 1;
    /* phew! */
}

/*
FindEntryInDirs( dirs, entry, extension ) 
char *dirs[];	A list of directories to be searched for a given entry.
char *entry;	The entry to be searched for in dirs sans extension.
char *extension;    The extension that will be concatenated to entry for searching purposes.

   Purpose: This routine is used to find a specified file within an ordered list of possible directories that are specified via that AndrewSetup file (see config.h for possible AndrewSetup entries).  The dirs[] are a NULL-terminated list of pointers to character strings.  Entry is a pointer to a character string that is a file-name w/o the extension.  Extension is the extension that will be placed at the end of entry.  The first match that is found is returned in a static memory area. DON'T FREE THE VALUE THAT IS RETURNED... COPY IT TO SOME WORKING AREA OR USE IT BEFORE MAKING ANOTHER CALL TO THIS ROUTINE.

   Used in: HistoryHelp(), ShowTutorial(), GetHelpOn(). NOTE: this routine is currently only used to find tutorial files because it is useful to be able to place tutorials in various places ( /usr/local/help, /usr/andrew/help, ...).  In the future other files (like .help files) may have various homes and this routine should be used to access them.
*/

static char *
FindEntryInDirs(dirs, entry , extension)
char	*dirs[];
char	*entry, *extension;
{
  static char	fullPath[MAXPATHLEN];
  char		*returnPath = NULL;
  int		i = 0;
  
  *fullPath = '\0';
  if(entry && dirs && *dirs) {
      for(i = 0; dirs[i] && *dirs[i] ; i++) {
	  sprintf(fullPath,"%s/%s%s",dirs[i],entry,extension);
	  if(access(fullPath, 0) == 0) break;
	  else *fullPath = '\0';
      }
  }
  if(*fullPath != '\0')
      returnPath = fullPath;
  return(returnPath);
}

/*
 * Main function for getting help on a string
 *
 * Returns:
 *	-1: if a server was down while trying to get help on this topic
 *	 0: if no help found for this topic
 *	 1: if successful

    Pseudo-code:
{
    if (index not open)
	return error;
    lowercase(topic);
    map topic(n) -> topic.n;
    if (first time for this topic)
	code = setuphelp for this topic;
    if (code != error)
	setupmenus if this topic has tutorial or changes files;
    if (empty file list)
	out of files, return error;
    else if (code != error) {
	show current file;
	check if we should erase the "show more files" menu item;
    }
    if (code == error)
	show error message;
    else
	advance current file pointer to next file;
    return code;
}
*/
int 
help_GetHelpOn(self, aname, isnew, ahistory, errmsg)
register struct help *self;
char *aname;	/* what topic */
long isnew;	/* is this a new topic? */
int ahistory;	/* show in history log under what name?
		   help_HIST_NOADD - none at all,
		   help_HIST_NAME - aname,
		   help_HIST_TAIL - tail of the found filename
		   */
char *errmsg;	/* error to print if failure. "Error" if this is NULL */
{
    register struct cache *c = self->info;
    char tbuffer[MAXPATHLEN];
    char helpBuffer[HNSIZE + 200];
    int code = 1;

    DEBUG(("%s get help: setup(%s)\n",(isnew) ? "new" : "old", aname));

    if (!helpdb_CheckIndex(c->view))
	return -1;

    LowerCase(aname);
    MapParens(aname);

     if (isnew) {		/* if the first time through, setup c->all */
	 code = helpdb_SetupHelp(c, aname, TRUE);

#ifdef DEBUGGING
	if (code != 0) {
	    DEBUG(("set found something\n"));
	} else {
	    DEBUG(("code to 0, tmplist == NULL\n"));
	}
#endif /*DEBUGGING*/

	if (code != 0) {
	    /* initially turn off changes and tutorial menus */
	    c->flags &= ~(MENU_SwitchChangesMenu | MENU_SwitchTutorialMenu | MENU_SwitchMoreMenu);
	
	    /* should we display "show tutorial" or "show changes" menus? */
	    if(FindEntryInDirs(help_tutorialDirs, c->name, TUTORIAL_EXT)) {
		if (!(c->flags & MENU_SwitchTutorialMenu))
		    c->flags |= MENU_SwitchTutorialMenu;
	    }
	    sprintf(tbuffer, "%s/%s%s", help_changesDir, c->name, CHANGE_EXT);
	    if (access(tbuffer, 0) == 0 && !(c->flags & MENU_SwitchChangesMenu))
		c->flags |= MENU_SwitchChangesMenu;
	}
    }

    im_SetProcessCursor(help_waitCursor);

    if (c->cur && (code == 1)) {
	DEBUG(("showfiling\n"));
	code = ShowFile(self, c->cur->fileName,
			(c->cur->next != (struct helpFile *)NULL),
			ahistory);
	DEBUG(("helpName: %s\n",c->name));
    }

    /* see if we should display "show more" menu item */
    if (c->cur && c->cur->next)
	c->flags |= MENU_SwitchMoreMenu; /* turn it on */
    else
	c->flags &= ~MENU_SwitchMoreMenu; /* turn it off */

    /* now set up the menus */
    SetupMenus(c);

    im_SetProcessCursor((struct cursor *)NULL);
    
    if (code == 0) {		/* error */
	DEBUG(("ERROR\n"));
        sprintf(helpBuffer, (errmsg == NULL) ? err_generic : errmsg);
	ERRORBOX(c->view, helpBuffer);
    } else if(code == 2) /* ran command-alias */
	code = 0;
    else /* no error, advance to next file on list */
	c->cur = c->cur->next;
    DEBUG(("Help on returning %d\n",code));
    return code;
}

/*---------------------------------------------------------------------------*/
/*			HELP OBJECT ULTILITY FUNCTIONS			     */
/*---------------------------------------------------------------------------*/

/*
 * setup the menumask based on internal flags
 */
void 
SetupMenus(c)
register struct cache *c;
{
    DEBUG(("IN setupmenus.."));
    menulist_SetMask(c->menus, c->flags);
    view_PostMenus(c->view, c->menus);
    DEBUG(("OUT setupmenus\n"));
}


/*
 * Do a search in the overview panel
 */
static void 
SearchOverviews(self)
register struct help* self;
{
    if (!self->showPanels)
	TogglePanels(self, help_ALWAYS_TOGGLE);
    if (!self->showOverview)
	ToggleOverviews(self, help_ALWAYS_TOGGLE);
    (*help_textSearch) ((struct textview *)self->overviewPanel);
}


/*
 * Do a search in the programs panel
 */
static void 
SearchPrograms(self)
register struct help* self;
{
    if (!self->showPanels)
	TogglePanels(self, help_ALWAYS_TOGGLE);
    DEBUG(("panels\n"));
    if (!self->showList)
	TogglePrograms(self, help_ALWAYS_TOGGLE);
    DEBUG(("search\n"));
    (*help_textSearch) ((struct textview *)self->listPanel);
}
    

/*
 * because textview's and frame's procs are bound at runtime, we must call them
 * like this, rather than just binding in a call to proctable_GetProc(...)
 */

static void 
TextviewProc(self, rock)
register struct help* self;
long rock;
{
    switch(rock) {
      case help_SEARCH:
	(*help_textSearch) ((struct textview *)self->info->view);
	break;
      case help_RSEARCH:
	(*help_textRevSearch) ((struct textview *)self->info->view);
	break;
      case help_SEARCH_AGAIN:
	(*help_textSearchAgain) ((struct textview *)self->info->view);
	break;
      case help_COPY_REGION:
	(*help_textCopyRegion) ((struct textview *)self->info->view);
	break;
      case help_PAGE_DOWN:
	(*help_textPageDown) ((struct textview *)self->info->view);
	break;
      case help_PAGE_UP:
	(*help_textPageUp) ((struct textview *)self->info->view);
	break;
      case help_SET_PRINTER:
#ifdef CONTRIB_ENV
	if (help_poptPostWindow)
	    (*help_poptPostWindow) (self);
	else
	    ERRORBOX(self->info->view, err_noproc);
#else
	if (help_frameSetPrinter)
	    (*help_frameSetPrinter) (self);
	else
	    ERRORBOX(self->info->view, err_noproc);
#endif
    }
}

/*
 * quit help
 */
static void 
Quit(self)
register struct help *self;
{
    im_SetProcessCursor(help_waitCursor);
    im_KeyboardExit();
}

/*
 * send gripes/kudos to the help maintainers
 */
static void 
SendComments(self)
register struct help *self;
{
    char cmd[MAXPATHLEN], *prof;
    
    message_DisplayString(self, 0, msg_comment);
    im_ForceUpdate();
    im_SetProcessCursor(help_waitCursor);
    prof = environ_GetConfiguration(SETUP_GRIPE_ADDRESS);
    if (prof == NULL)
	prof = DEFAULT_GRIPE_ADDRESS;
    sprintf(cmd, "%s %s", COMMENTCMD, prof);
    mysystem(cmd);
    im_SetProcessCursor((struct cursor *) NULL);
}

/*
 * show a tutorial, if it exists
 */
static void 
ShowTutorial(self)
register struct help *self;
{
    static char tbuffer[MAXPATHLEN];
    char *tmp = NULL;

    if(tmp = FindEntryInDirs(help_tutorialDirs,self->info->name,TUTORIAL_EXT)) {
	strcpy(tbuffer,tmp);
	self->info->flags &= ~MENU_SwitchTutorialMenu; /* turn off menu item */
	SetupMenus(self->info);
    }
    ShowFile(self, tbuffer, help_USE_OLD_TITLE, help_HIST_TAIL);
}

/*
 * show changes doc
 */
static void 
ShowChanges(self)
register struct help *self;
{
    static char tbuffer[MAXPATHLEN];
    
    sprintf(tbuffer, "%s/%s%s", help_changesDir, self->info->name, CHANGE_EXT);
    self->info->flags &= ~MENU_SwitchChangesMenu; /* turn off menu item */
    SetupMenus(self->info);
    ShowFile(self, tbuffer, help_USE_OLD_TITLE, help_HIST_TAIL);
}

/*
 * show next file in list
 */
void NextHelp(self)
register struct help *self;
{
    if (!(self->info->flags & MENU_SwitchMoreMenu)) {
	return;
    }
    if(help_GetHelpOn(self, self->info->name, help_OLD, help_HIST_TAIL, err_no_more)) {
	panel_ClearSelection(self->historyPanel);
    }
}

/*
 * get help on a clicked-on history item
 */
void 
HistoryHelp(self, ent, apanel)
struct help *self;		/* callback rock */
struct history_entry *ent;	/* panelEntry rock */
struct panel *apanel;		/* appropriate panel */
{
    char buf[HNSIZE + HELP_MAX_ERR_LENGTH];
    char fnbuf[MAXPATHLEN];
    char *tmp = NULL, *dir = NULL;
    int code;
    int file = FALSE;

    DEBUG(("histhelp: %s\n", ent->fname));
    im_SetProcessCursor(help_waitCursor);
    sprintf(buf, err_unexpected, ent->fname);

    /* if it's a change or tutorial file, showfile it, since most of the time,
       the CHANGEDIR and TUTORIALDIR aren't indexed */
    tmp = rindex(ent->fname, '.');
    if (tmp) {
	if (!strcmp(tmp, CHANGE_EXT)) {
	    file = TRUE;
	    dir = help_changesDir;
	}
	if (!strcmp(tmp, TUTORIAL_EXT)) {
	    file = TRUE;
	    *tmp = '\0';
	    dir = FindEntryInDirs( help_tutorialDirs, ent->fname , TUTORIAL_EXT );
	}
    }

    if (file) {
	strcpy(fnbuf, dir );
	strcat(fnbuf, "/");
	strcat(fnbuf, ent->fname);
	DEBUG(("history file: %s\n",fnbuf));
	code = ShowFile(self, fnbuf, FALSE, help_HIST_NOADD);
    } else
	code = help_GetHelpOn(self, ent->fname, help_NEW, help_HIST_NOADD, buf);

    if (code == 0) {
	DEBUG(("history help = 0\n"));
	/* error, so restore the old selection */
	if (self->info->lastHist != NULL) {
	    panel_MakeSelection(self->historyPanel, self->info->lastHist);
	}
    } else {
	panel_ClearSelection(self->overviewPanel);
	panel_ClearSelection(self->listPanel);
	textview_SetDotPosition((struct textview *)self->info->view, ent->pos);
	textview_SetDotLength((struct textview *)self->info->view, ent->sellen);
	textview_SetTopPosition((struct textview *)self->info->view, ent->top);
    }
    im_SetProcessCursor((struct cursor *)NULL);
    self->info->lastHist = panel_CurrentHighlight(self->historyPanel);
}

/*
 * show overview or a help file from the program list panel
 */
void 
OverviewHelp(self, name, apanel)
struct panel *apanel;
register char *name;		/* which topic to request - panelEntry rock */
register struct help *self;
{
    char buf[HNSIZE + HELP_MAX_ERR_LENGTH];
    
    sprintf(buf, err_sorry, name);
    if (help_GetHelpOn(self, name, help_NEW, help_HIST_NAME, buf) != 0) {
	panel_ClearSelection(self->historyPanel);
	/* clear the selection in the other panel */
	if (apanel == self->overviewPanel) {
	    panel_ClearSelection(self->listPanel);
	} else {		/* it was self->listPanel */
	    panel_ClearSelection(self->overviewPanel);
	}
    }
}


/*---------------------------------------------------------------------------*/
/*			     PANEL FUNCTIONS				     */
/*---------------------------------------------------------------------------*/

static boolean
EnsurePanelListSize()
{
    if(help_panelList || (help_panelList = (char**)calloc(help_panelListSize,sizeof(char*))))
	if(help_panelIndex >= (help_panelListSize-1)) {
	    help_panelListSize *= 2;
	    if(help_panelList = (char**) realloc(help_panelList,sizeof(char*) * help_panelListSize)) {
		int i;

		for(i = help_panelIndex; i < help_panelListSize; i++)
		    help_panelList[i] = NULL;
	    }
	}
    return(help_panelList ? TRUE : FALSE);
}

void
FreePanelListData()
{
    if(help_panelList && (help_panelIndex > 0)) {
	int i;

	for(i = 0; i <= help_panelIndex; i++)
/* Don't free the panelList strings because these strings are passed in as the client data (tag) for the panel object items and panel_FreeAllTags() will release these.*/
	    if(help_panelList[i]) help_panelList[i] = NULL;
	free(help_panelList);
	help_panelList = NULL;
	help_panelListSize = help_MAXPANEL;
	help_panelIndex = 0;
    }
}

/*
 * setup panels and default entries.
	This function had an ifdef to map it between cmu and non-cmu.
	But the non-cm,u functionality is needed for overviews, so
	I converted it to a runtime choice.   -- wjh

	readpairs:
		fname is the name of a file containing pair lines:
			panel-entry,help-key
		def is NULL, but could be an array of keys
	non-readpairs:
		fname is the name of a  directory of help files
		def is a list of extensions of files to be used in the panel
 
	returns the number of entries added to the panel
 */
long
SetupPanel(readpairs, fname, panel, def)
	boolean readpairs;
	char *fname;
	struct panel *panel;		/* the panel to add entries to */
	char **def;
{
	char **defptr;
	register FILE *tfile;
	char elt1[HNSIZE], elt2[HNSIZE];
	register long code;
	char *tmp;
	DIR *helpdir;
	DIRENT_TYPE *ent;
	char tf[MAXPATHLEN];
	char *tfp;
	long entriesadded = 0;

	if (readpairs) {
		tfile = fopen(fname, "r");		/* try opening file */
		if (tfile) {
			while (1) {
				code = ScanLine(tfile, elt1, elt2);
				if (code != 2) break;
				if (elt1[0] != '#') {
					panel_Add(panel, elt1, CopyString(elt2), FALSE);
					entriesadded++;
				}
			}
			fclose(tfile);
		}
		else {			/* if that don't woik, use da backup */
			defptr = def;
			if (defptr) 
			    while (*defptr != (char *) NULL) {
				panel_Add(panel, *defptr, *defptr, FALSE);
				defptr++;
				entriesadded++;
			    }
		}
	}
	else if ((helpdir = opendir(fname)) == (DIR *)NULL)
		ERRORBOX(panel, err_openhelpdir);
	else {
	    FreePanelListData();
	    if(!EnsurePanelListSize()) {
		fprintf(stderr,"Couldn't allocate enough memory!\n");
		return(0);
	    }

	    strcpy(tf, fname); /* make a base for filenames dir/dir/ */
	    tfp = tf + strlen(tf);
	    *tfp++ = '/';	/* tfp points just after the last '/' */

	    while((ent = readdir(helpdir)) != (DIRENT_TYPE *)NULL) {
		strcpy(tfp, ent->d_name); /* finish the filename */
		defptr = def;
		tmp = rindex(ent->d_name, '.');
		if (tmp != NULL) {
		    if (defptr)
			while (*defptr != (char *) NULL) {
			    if (strcmp(tmp, *defptr++) == 0) {
				*tmp = '\0'; /* chop off extension */
				/* found a good candidate filename */
				AddToPanelList(ent->d_name);
				*tmp = '.';   /* replace period */
				entriesadded++;
			    }
			}
		}
	    }
	    closedir(helpdir);

	    /* now make the list */
	    SortAndMakePanel(panel);
	}
	return entriesadded;
}


/*
 * Setup the lpairs for the side panel(s)
 */
struct view *SetupLpairs(self)
register struct help *self;
{
    long which = 0;

    lpair_UnlinkTree(self->panelLpair1);
    lpair_UnlinkTree(self->panelLpair2);

    if (self->showOverview) which |= help_SHOW_OVER;
    if (self->showList) which |= help_SHOW_LIST;
    if (self->showHistory) which |= help_SHOW_HIST;

    DEBUG(("lpair which: %d\n",which));

    switch (which) {
        case 0:
	    return (struct view *)NULL;

	case help_SHOW_OVER:
	    return (struct view *)self->overviewLpair;

	case help_SHOW_LIST:
	    return (struct view *)self->listLpair;

	case help_SHOW_HIST:
	    return (struct view *)self->historyLpair;

	case help_SHOW_OVER | help_SHOW_LIST:
	    lpair_SetNth(self->panelLpair1, 0, self->overviewLpair);
	    lpair_SetNth(self->panelLpair1, 1, self->listLpair);
	    self->panelLpair1->needsfull = 3;
	    return (struct view *)self->panelLpair1;

	case help_SHOW_OVER | help_SHOW_HIST:
	    lpair_SetNth(self->panelLpair1, 0, self->overviewLpair);
	    lpair_SetNth(self->panelLpair1, 1, self->historyLpair);
	    self->panelLpair1->needsfull = 3;
	    return (struct view *)self->panelLpair1;

	case help_SHOW_LIST | help_SHOW_HIST:
	    lpair_SetNth(self->panelLpair1, 0, self->listLpair);
	    lpair_SetNth(self->panelLpair1, 1, self->historyLpair);
	    self->panelLpair1->needsfull = 3;
	    return (struct view *)self->panelLpair1;

	case help_SHOW_OVER | help_SHOW_LIST | help_SHOW_HIST:
	    lpair_SetNth(self->panelLpair1, 0, self->overviewLpair);
	    lpair_SetNth(self->panelLpair1, 1, self->listLpair);
	    self->panelLpair1->needsfull = 3;

	    lpair_SetNth(self->panelLpair2, 0, self->panelLpair1);
	    lpair_SetNth(self->panelLpair2, 1, self->historyLpair);
	    self->panelLpair2->needsfull = 2;
	    return (struct view *)self->panelLpair2;
	}
}


/*
 * turn side panels on and off
 */
static void 
TogglePanels(self, rock)
register struct help *self;
long rock;
{
    if ((self->showPanels && (rock == help_SHOW_PANEL)) ||
	(!self->showPanels && (rock == help_HIDE_PANEL)))
	return;
    /* could fall through here if rock is ALWAYS_TOGGLE */
    self->showPanels = 1 - self->showPanels;    	/* toggle it */
    self->info->flags ^= (MENU_TogglePanelShow | MENU_TogglePanelHide); /* toggle menus */

    if (self->showPanels) {
	scroll_UnlinkTree(self->info->scroll);
	if((self->showOverview + self->showList + self->showHistory) == 0) {
	    self->showOverview = self->showList = self->showHistory = 1;
	    self->info->flags ^= (MENU_ToggleOverHide | MENU_ToggleOverShow |
				  MENU_ToggleListHide | MENU_ToggleListShow |
				  MENU_ToggleHistHide | MENU_ToggleHistShow);
	    lpair_SetNth(self->mainLpair, 1, SetupLpairs(self));
	}	    
	lpair_LinkTree(self->mainLpair, self);
    } else {
	if(self->showOverview) {
	    self->showOverview = 0;
	    self->info->flags ^= (MENU_ToggleOverHide | MENU_ToggleOverShow);
	}
	if(self->showList) {
	    self->showList = 0;
	    self->info->flags ^= (MENU_ToggleListHide | MENU_ToggleListShow);
	}
	if(self->showHistory) {
	    self->showHistory = 0;
	    self->info->flags ^= (MENU_ToggleHistHide | MENU_ToggleHistShow);
	}
	lpair_UnlinkTree(self->mainLpair);
	scroll_LinkTree(self->info->scroll, self);
    }
    SetupMenus(self->info);
    /* here we want to update everything */
    if(self->info && self->info->view) view_WantInputFocus(self->info->view, self->info->view);
    help_Update(self);
}


/*
 * toggle overview panel on and off
 */
static void 
ToggleOverviews(self, rock)
register struct help *self;
long rock;
{
    struct view *v;
    boolean doUpdate = FALSE;

    if ((self->showOverview && (rock == help_SHOW_OVER)) ||
	(!self->showOverview && (rock == help_HIDE_OVER)))
	return;
     /* could fall through here if rock is ALWAYS_TOGGLE */

    self->showOverview = 1 - self->showOverview; /* toggle */
    v = SetupLpairs(self);
    self->info->flags ^= (MENU_ToggleOverHide | MENU_ToggleOverShow);

    if (v) {
	if (!self->showPanels) {
	    self->showPanels = 1;
	    self->info->flags ^= (MENU_TogglePanelShow | MENU_TogglePanelHide);
	    scroll_UnlinkTree(self->info->scroll);
	    lpair_LinkTree(self->mainLpair, self);
	    doUpdate = TRUE;
	}
	self->mainLpair->needsfull = 2;  /* -- hack to get redraw to work */
	lpair_SetNth(self->mainLpair, 1, v);
	SetupMenus(self->info);
	if(doUpdate) help_Update(self);
	super_WantInputFocus(self, self->info->view);
    } else if (self->showPanels)
	TogglePanels(self, help_ALWAYS_TOGGLE); /* turn them off */
}


/*
 * toggle program list panel on and off
 */
static void 
TogglePrograms(self, rock)
register struct help *self;
long rock;
{
    struct view *v;
    boolean doUpdate = FALSE;

    if ((self->showList && (rock == help_SHOW_LIST)) ||
	(!self->showList && (rock == help_HIDE_LIST)))
	return;
     /* could fall through here if rock is ALWAYS_TOGGLE */

    self->showList = 1 - self->showList; /* toggle */
    v = SetupLpairs(self);
    self->info->flags ^= (MENU_ToggleListHide | MENU_ToggleListShow);

    if (v) {
	if (!self->showPanels) {
	    self->showPanels = 1;
	    self->info->flags ^= (MENU_TogglePanelShow | MENU_TogglePanelHide);
	    scroll_UnlinkTree(self->info->scroll);
	    lpair_LinkTree(self->mainLpair, self);
	    doUpdate = TRUE;
	}
	self->mainLpair->needsfull = 2;  /* -- hack to get redraw to work */
	lpair_SetNth(self->mainLpair, 1, v);
	SetupMenus(self->info);
	if(doUpdate) help_Update(self);
	super_WantInputFocus(self, self->info->view);
    } else if (self->showPanels)
	TogglePanels(self, help_ALWAYS_TOGGLE); /* turn them off */
}



/*
 * toggle history panel on and off
 */
static void 
ToggleHistory(self, rock)
register struct help *self;
long rock;
{
    struct view *v;
    boolean doUpdate = FALSE;

    if ((self->showHistory && (rock == help_SHOW_HIST)) ||
	(!self->showHistory && (rock == help_HIDE_HIST)))
	return;
     /* could fall through here if rock is ALWAYS_TOGGLE */

    self->showHistory = 1 - self->showHistory; /* toggle */
    v = SetupLpairs(self);
    self->info->flags ^= (MENU_ToggleHistHide | MENU_ToggleHistShow);

    if (v) {
	if (!self->showPanels) {
	    self->showPanels = 1;
	    self->info->flags ^= (MENU_TogglePanelShow | MENU_TogglePanelHide);
	    scroll_UnlinkTree(self->info->scroll);
	    lpair_LinkTree(self->mainLpair, self);
	    doUpdate = TRUE;
	}
	self->mainLpair->needsfull = 2;  /* -- hack to get redraw to work */
	lpair_SetNth(self->mainLpair, 1, v);
	SetupMenus(self->info);
	if(doUpdate) help_Update(self);
	super_WantInputFocus(self, self->info->view);
    } 
    else if (self->showPanels)
	TogglePanels(self, help_ALWAYS_TOGGLE); /* turn them off */
}


/*
 * Used to add all files in a directory to the expanded program list
 */
static void 
ExpanderAux(dname)
char *dname;
{
    DIR *tmpdir;
    DIRENT_TYPE *tde;
    char tf[MAXPATHLEN], *tfp;
    
    tmpdir = opendir(dname);
    
    if (tmpdir == (DIR *)NULL)
	return; /* don't use unopened directory */
    strcpy(tf, dname); /* make a base for filenames dir/dir/ */
    tfp = tf + strlen(tf);
    *tfp++ = '/';	/* tfp points just after the last '/' */
    
    while((tde=readdir(tmpdir)) != NULL) {
	if (*tde->d_name != '.') { /* no dot files */
	    strcpy(tfp, tde->d_name); /* finish the filename */
	    AddToPanelList(tde->d_name);
	}
    }
    closedir(tmpdir);
}


/*
 * comparison function for qsort
 */
static int 
panelCompare(s1, s2)
char **s1, **s2;
{
    return (strcmp(*s1, *s2));
}


/*
 * terminates and then sorts the panelList
 */
static void 
SortAndMakePanel(p)
struct panel *p;
{
    register int i;

    DEBUG(("sort and make..."));
    DEBUG(("sort..."));
    if(help_panelIndex > 1) 
	qsort(help_panelList, help_panelIndex, sizeof(char *), panelCompare);
    DEBUG(("removing..."));
    if(help_panelIndex > 0){	
	static char lastItem[100] = "";

	panel_FreeAllTags(p);
	panel_RemoveAll(p);
	DEBUG(("adding..."));
	for (i = 0; i < help_panelIndex; i++) {
	    if(help_panelList[i] && strcmp(lastItem,help_panelList[i])) {
		panel_Add(p, help_panelList[i], help_panelList[i], FALSE);
		strcpy(lastItem, help_panelList[i]);
	    }
	}
	*lastItem = '\0';
    }
    DEBUG(("done\n"));
}


/*
 * just adds a string to the global panelList
 */
static char *
AddToPanelList(s)
char *s;
{
    if(EnsurePanelListSize()) {
	if(!(help_panelList[help_panelIndex] = (char*)malloc(strlen(s) + 1))) {
	    fprintf(stderr,"Couldn't allocate enough memory!\n");
	    return(NULL);
	}
	strcpy(help_panelList[help_panelIndex], s);
	help_panelIndex++;
	return help_panelList[(help_panelIndex - 1)];
    }
    else {
	fprintf(stderr,"Couldn't allocate enough memory!\n");
	return(NULL);
    }
}    


/*
 * Index library callback helper for 'expand the program list'
 */
static void 
Expander(aindex, ac, self)
struct Index *aindex;
struct indexComponent *ac;
struct help *self;
{
    if(ac && ac->name && (*(ac->name) != '\0')) {
	AddToPanelList(ac->name);
    }
}


/*
 * toggle programs list size.  Like da name dun say.
 */
void 
ToggleProgramListSize(self, rock)
register struct help* self;
long rock;
{
    struct helpDir *thd;

    if ((self->expandedList && (rock == help_EXPAND)) ||
	(!self->expandedList && (rock == help_SHRINK)))
	return;
    /* could fall through here if rock is ALWAYS_TOGGLE */
    if (!self->showList) TogglePrograms(self, help_ALWAYS_TOGGLE);

    self->expandedList = 1 - self->expandedList; /* toggle */
    self->info->flags ^= (MENU_ToggleSizeExpand | MENU_ToggleSizeShrink); /* toggle menus */

    if (self->oldpanel)	/* using tmp, restore original */
	RestorePanel(self);

    if (!self->expandedList) { /* shrink it down */
	char *tmp, pathName[MAXPATHLEN];

	DEBUG(("shrinking "));
	
	self->expandedList = 0;

	panel_FreeAllTags(self->listPanel);
	panel_RemoveAll(self->listPanel); /* this frees all the strings in panelLits */
	
	/* add only the small list of entries to listPanel */
	tmp = environ_GetConfiguration(SETUP_PANELSDIR);
	if (tmp == NULL)
	    tmp = environ_AndrewDir(DEFAULT_PANELSDIR);
	sprintf(pathName, "%s%s", tmp, PROGRAMFILE);

	if (0 == SetupPanel(TRUE, pathName, self->listPanel, NULL)) {
		/* we got nothing from the lib/help.programs file.  Try using extensions. */
		tmp = environ_GetConfiguration(SETUP_HELPDIR);
		if (tmp == NULL)
			tmp = environ_AndrewDir(DEFAULT_HELPDIR);
		strcpy(pathName, tmp);
	
		SetupPanel(FALSE, pathName, self->listPanel, program_ext_array);
	}

    } else {			/* pump it up */

	DEBUG(("expanding "));

	FreePanelListData();
	if(EnsurePanelListSize() == FALSE) {
	    fprintf(stderr,"Couldn't allocate enough memory!\n");
	    return;
	}

	if (!helpdb_CheckIndex(self))
	    return;
	message_DisplayString(self, 0, msg_expanding);
	im_ForceUpdate();
	im_SetProcessCursor(help_waitCursor);
	self->expandedList = 1;

	/* get everything in the index */
	helpdb_Enumerate(Expander, self);

	/* now add in the auxiliary help dirs */
	for(thd = (struct helpDir *)helpdb_GetHelpDirs(); thd; thd = thd->next) {
	    char *tmp;
	    char *subdir = MANSUBS;
	    char dir[MAXPATHLEN];
	    
	    tmp = rindex(thd->dirName, '/');
	    if (tmp)
		tmp = tmp+1;
	    else
		tmp = thd->dirName;
	    if (!strcmp(tmp, "man")) {
		strcpy(dir, thd->dirName);
		strcat(dir, "/man");
		tmp = dir + strlen(dir);
		*(tmp + 1) = '\0';
		while (*subdir) {
		    *tmp = *subdir++;
		    ExpanderAux(dir);
		}
	    } else
		ExpanderAux(dir);
	}

	/* now make the list */
	SortAndMakePanel(self->listPanel);

	im_SetProcessCursor((struct cursor *) NULL);
    }
    label_SetText(self->listLab, (self->expandedList) ?
		  "Full Program List" : "Programs");
    SetupMenus(self->info);
    message_DisplayString(self, 0, "Done.");
}


/*
 * restores the original program panel (unfiltered)
 */
static void 
RestorePanel(self)
register struct help *self;
{
    if (!self->oldpanel) {
	DEBUG(("already have old panel\n"));
	return;
    }

    self->info->flags ^= (MENU_ToggleRestoreShow | (MENU_ToggleFilterShow | MENU_ToggleReFilterShow));

    if (!self->showPanels)
	TogglePanels(self, help_ALWAYS_TOGGLE);

    panel_FreeAllTags(self->tmpanel);
    panel_RemoveAll(self->tmpanel);
    scroll_SetView(self->listScroll, self->oldpanel);
    self->oldpanel = (struct panel *)NULL;

    label_SetText(self->listLab, (self->expandedList) ?
		  "Full Program List" : "Programs");
    SetupMenus(self->info);
}


/*
 * replaces the program panel with a new panel that contains only those
 * entries from the old panel specified by the user
 */
static void 
FilterPanel(self, rock)
register struct help *self;
long rock;
{
    int code;
    char buf[255];
    char lbuf[255];
    struct panel_Entry *pe, *list;
    regexp *pattern;    /* used to store compiled version of expression */

    /* if rock == sort && self->oldpanel, punt */
    /* if rock == resort && self->oldpanel, OK */
    if (self->oldpanel && (rock == help_FILTER_FILTER)) { /* it's already being used */
	DEBUG(("already using old panel\n"));
	return;
    }

    if (!self->showPanels)
	TogglePanels(self, help_ALWAYS_TOGGLE);

    code = message_AskForString(self, 0, msg_filter_prompt,
				0, buf, sizeof(buf)-1);
    if ((code < 0) || (buf[0] == '\0'))
	return;

    FreePanelListData();

    if (rock == help_FILTER_FILTER)
	list = self->listPanel->panelList;
    else
	list = self->tmpanel->panelList;
    
    pattern = reg_comp(buf); /* compile the pattern and save the results */
    if (pattern == NULL) {
	ERRORBOX(self->info->view, "Sorry; filter pattern was not understood");
	return;
    } else {
	for (pe = list; pe != NULL; pe = pe->next) {
	    if (reg_exec(pattern, pe->tag) != NULL)
		AddToPanelList(pe->tag);
	}
	DEBUG(("done\n"));
	free(pattern);	/* return the space */
    }
    if(help_panelIndex == 0){
	char bbuf[512];
	sprintf(bbuf,"Panel does not contain \"%s\"",buf);
	ERRORBOX(self->info->view,bbuf);
	return;
    }

    if (rock == help_FILTER_FILTER) { /* only the first time */
	self->oldpanel = self->listPanel;

	self->info->flags ^= (MENU_ToggleRestoreShow | (MENU_ToggleFilterShow | MENU_ToggleReFilterShow));
	SetupMenus(self->info);
    }

    im_SetProcessCursor(help_waitCursor);
    im_ForceUpdate();

    sprintf(lbuf, "%s '%s'", "filtered by ", buf);
    label_SetText(self->listLab, lbuf);

    /* now make the list */
    SortAndMakePanel(self->tmpanel);
    
    scroll_SetView(self->listScroll, self->tmpanel);
    DEBUG(("set data\n"));

    im_SetProcessCursor((struct cursor *)NULL);
}

/*
 * random key hit proc to chastise the user
 */
static void 
nono(self)
register struct help *self;
{
    message_DisplayString(self, 0, err_readonly);
}


static struct bind_Description helpBindings[] = {

/*
    {"proc-name", keybinding, keyrock,
    "menu string", menurock, menuflag, function-to-call,
    "documentation string", module-name}
*/

    /* Toggling menu items */

    /* if self->showPanels */
    {"help-toggle-panels", NULL, help_HIDE_PANEL,
    "Panels~10,Hide Panels~2", help_HIDE_PANEL, MENU_TogglePanelHide, TogglePanels,
    NULL, NULL},

    {"help-toggle-panels", NULL, help_SHOW_PANEL,
    "Panels~10,Show Panels~2", help_SHOW_PANEL, MENU_TogglePanelShow, TogglePanels,
    NULL, NULL},

    {"help-toggle-panels", HELP_KEY_TOGGLE_PANELS, help_ALWAYS_TOGGLE,
    NULL, help_ALWAYS_TOGGLE, MENU_Never, TogglePanels,
    "toggle help panels", NULL},


    /* if self->showOverview */
    {"help-toggle-overviews", NULL, help_HIDE_OVER,
    "Panels~10,Hide Overviews~20", help_HIDE_OVER, MENU_ToggleOverHide, ToggleOverviews,
    NULL, NULL},

    {"help-toggle-overviews", NULL, help_SHOW_OVER,
    "Panels~10,Show Overviews~20", help_SHOW_OVER, MENU_ToggleOverShow, ToggleOverviews,
    NULL, NULL},

    {"help-toggle-overviews", HELP_KEY_TOGGLE_OVER, help_ALWAYS_TOGGLE,
    NULL, help_ALWAYS_TOGGLE, MENU_Never, ToggleOverviews,
    "toggle overviews panel", NULL},


    /* if self->showList */
    {"help-toggle-programs", NULL, help_HIDE_LIST,
    "Panels~10,Hide Programs~22", help_HIDE_LIST, MENU_ToggleListHide, TogglePrograms,
    NULL, NULL},

    {"help-toggle-programs", NULL, help_SHOW_LIST,
    "Panels~10,Show Programs~22", help_SHOW_LIST, MENU_ToggleListShow, TogglePrograms,
    NULL, NULL},

    {"help-toggle-programs", HELP_KEY_TOGGLE_PROGRAMS, help_ALWAYS_TOGGLE,
    NULL, help_ALWAYS_TOGGLE, MENU_Never, TogglePrograms,
    "toggle programs panel", NULL},


    /* if self->showHistory */
    {"help-toggle-history", NULL, help_HIDE_HIST,
    "Panels~10,Hide History~24", help_HIDE_HIST, MENU_ToggleHistHide, ToggleHistory,
    NULL, NULL},

    {"help-toggle-history", NULL, help_SHOW_HIST,
    "Panels~10,Show History~24", help_SHOW_HIST, MENU_ToggleHistShow, ToggleHistory,
    NULL, NULL},

    {"help-toggle-history", HELP_KEY_TOGGLE_HIST, help_ALWAYS_TOGGLE,
    NULL, help_ALWAYS_TOGGLE, MENU_Never, ToggleHistory,
    "toggle history panel", NULL},


    /* if self->expandedList */
    {"help-toggle-size", NULL, help_SHRINK,
    "Panels~10,Shrink Programs List~10", help_SHRINK, MENU_ToggleSizeShrink,
    ToggleProgramListSize, NULL, NULL},

    {"help-toggle-size", NULL, help_EXPAND,
    "Panels~10,Expand Programs List~10", help_EXPAND, MENU_ToggleSizeExpand,
    ToggleProgramListSize, NULL, NULL},

    {"help-toggle-size", HELP_KEY_TOGGLE_PROGRAMS, help_ALWAYS_TOGGLE,
    NULL, help_ALWAYS_TOGGLE, MENU_Never, ToggleProgramListSize,
    "toggle program list size", NULL},


    {"help-filter-programs", NULL, help_FILTER_FILTER,
    "Other~10,Filter Programs Panel~10", help_FILTER_FILTER, MENU_ToggleFilterShow, FilterPanel,
    "filter-programs", NULL},

    {"help-refilter-programs", NULL, help_FILTER_REFILTER,
    "Other~10,ReFilter Programs Panel~12", help_FILTER_REFILTER, MENU_ToggleReFilterShow, FilterPanel,
    "filter-programs", NULL},

    {"help-restore-programs", NULL, help_FILTER_RESTORE,
    "Other~10,Restore Programs Panel~10", help_FILTER_RESTORE, MENU_ToggleRestoreShow, RestorePanel,
    "restore programs panel", NULL},


    {"help-show-more", HELP_KEY_SHOW_MORE, 0,
    "Show More Documentation~30", 0, MENU_SwitchMoreMenu, NextHelp,
    "show more documentation", NULL},

    {"help-show-tutorial", HELP_KEY_SHOW_TUTORIAL, 0,
    "Show Tutorial~32", 0, MENU_SwitchTutorialMenu, ShowTutorial,
    "show tutorial", NULL},

    {"help-changes", HELP_KEY_SHOW_CHANGES, 0,
    "Show Changes~34", 0, MENU_SwitchChangesMenu, ShowChanges,
    "show changes on topic", NULL},
    
    {"help-copy-region", HELP_KEY_COPY_REGION, help_COPY_REGION,
    "Copy~2", help_COPY_REGION, MENU_SwitchCopy, TextviewProc,
    "Copy region to kill-buffer", NULL},

    
    /* non-toggling menu items */


    {"help-delete-window", HELP_KEY_DELETE_WINDOW, 0,
    "Delete Window~80",0 ,MENU_SwitchDeleteMenu, help_aux_ExitProc,
    "delete this window", NULL},

    {"help-add-search-dir", HELP_KEY_ADD_SEARCH_DIR, 0,
    "Other~10,Add Search Directory~20", 0, MENU_Always, help_aux_AddSearchDir,
    "add a directory to the search path", NULL},

    {"help-add-bookmark", HELP_KEY_ADD_BOOKMARK, 0,
    "Other~10,Make Bookmark~30", 0, MENU_Always, help_aux_AddBookmark,
    "add a history entry for this file", NULL},

    {"help-show-help-on", HELP_KEY_HELP_ON, help_ON,
    "Show Help on ...~14", help_ON, MENU_Always, help_aux_NewHelp,
    "show help on a prompted keyword", NULL},

    {"help-show-help-on", HELP_KEY_HELP_ON_2, help_ON,
    NULL, help_ON, MENU_Always, help_aux_NewHelp,
    NULL, NULL},

    {"help-new-show-help-on", HELP_KEY_WINDOW_SPLIT, help_ON | help_NEW_WIN,
    "Window~12,Show Help on...~14", help_ON | help_NEW_WIN, MENU_Always, help_aux_NewHelp,
    "show help on a prompted keyword in a new window", NULL},

    {"help-show-help-on-selected", HELP_KEY_HELP_ON_SELECTED, help_SEL,
    "Show Help on Selected Word~16", help_SEL, MENU_Always, help_aux_NewHelp,
    "show help on selected region", NULL},

    {"help-new-show-help-on-selected", NULL, help_SEL | help_NEW_WIN,
    "Window~12,Show Help on Selected Word~26", help_SEL | help_NEW_WIN, MENU_Always, help_aux_NewHelp,
    "show help on selected region in a new window", NULL},

    {"help-comments", HELP_KEY_SEND_COMMENT, 0,
    "Send Comment On Help~31", 0, MENU_Always, SendComments,
    "send comments on help", NULL},

#ifdef CONTRIB_ENV
/* If we're building Contrib, use PrinterSetup instead of SetPrinter */
   {"help-printer-setup", HELP_KEY_SET_PRINTER, help_SET_PRINTER,
    "Print~6,Printer Setup~12", help_SET_PRINTER, MENU_Always, TextviewProc,
    "Pop up dialogue box to set printer name and other parameters.", "printopts"},
#else /* CONTRIB_ENV */
   {"help-set-printer", HELP_KEY_SET_PRINTER, help_SET_PRINTER,
     "Print~6,Set Printer~12", help_SET_PRINTER, MENU_Always, TextviewProc,
    "set the printer", NULL},
#endif /*CONTRIB_ENV */

    {"help-print", HELP_KEY_PRINT, 0,
    "Print~6,Print This File~22", 0, MENU_Always, help_aux_Print,
    "print current help file", NULL},

    {"help-quit", HELP_KEY_QUIT, 0,
    "Quit~99", 0, MENU_Always, Quit,
    "quit help", NULL},

    /* imported from textview... */

    {"help-search", HELP_KEY_SEARCH, help_SEARCH,
    "Search~4,Forward~12", help_SEARCH, MENU_Always, TextviewProc,
    "forward search", NULL},

    {"help-reverse-search", HELP_KEY_RSEARCH, help_RSEARCH,
    "Search~4,Backward~14", help_RSEARCH, MENU_Always, TextviewProc,
    "reverse search", NULL},
    
    {"help-search-again", HELP_KEY_SEARCH_AGAIN, help_SEARCH_AGAIN,
    "Search~4,Search Again~16", help_SEARCH_AGAIN, MENU_Always, TextviewProc,
    "search again", NULL},

    {"help-search-overviews", HELP_KEY_SEARCH_OVERVIEWS, 0,
    "Search~4,Search Overviews~20", 0, MENU_Always, SearchOverviews,
    "search overviews", NULL},

    {"help-search-programs", HELP_KEY_SEARCH_PROGRAMS, 0,
    "Search~4,Search Programs~22", 0, MENU_Always, SearchPrograms,
    "search programs", NULL},
    
    {"help-next-screen", HELP_KEY_NEXT_SCREEN, help_PAGE_DOWN,
    NULL, help_PAGE_DOWN, 0, TextviewProc,
    "advance to next screen", NULL},

    {"help-next-screen", HELP_KEY_NEXT_SCREEN2, help_PAGE_DOWN,
    NULL, help_PAGE_DOWN, 0, TextviewProc,
    "advance to next screen", NULL},

    {"help-prev-screen", HELP_KEY_PREV_SCREEN, help_PAGE_UP,
    NULL, help_PAGE_UP, 0, TextviewProc,
    "go back to previous screen", NULL},

    NULL};

/*
 * makes new instance of a help object.  Creates keylist and menus, binds procedures
 * to menus and keys, and adds a default filetype so that all files will have
 * at least the default template when they are displayed.
 */
boolean 
help__InitializeClass(classID)
struct classheader *classID;
{
    unsigned char c[2];
    struct proctable_Entry *pe;

#ifdef DEBUGGING
    if ((char *)getenv("HELPDEBUG") != (char *) NULL)
	HELPDEBUG = 1;
#endif /* DEBUGGING */
    
    DEBUG(("IN init class\n"));
    Help_Menus = menulist_New();
    Help_Map = keymap_New();
    DEBUG(("created Help_Menus, Help_Map\n"));

    if (!Help_Menus || !Help_Map)
	return FALSE;

    /* make all printing keys put a readonly message in the message line */
    c[1] = '\0';
    pe = proctable_DefineProc("help-readonly-key", nono, &help_classinfo, 0, "fake readonliness");
    for (c[0] = (unsigned char)0; c[0] <= (unsigned char)127; c[0]++)
	if (isprint(c[0]) || c[0] == '\n' || c[0] == ' ')
	    keymap_BindToKey(Help_Map, c, pe, NULL);
    DEBUG(("about to bind helpBindings\n"));
    bind_BindList(helpBindings, Help_Map, Help_Menus, &help_classinfo);

    DEBUG(("about to create catchall filetype\n"));
    /* what this is really doing is adding a catchall filetype and template */
    filetype_AddEntry("*", "text", "template=default");

    DEBUG(("about to load helpdb\n"));
    class_Load("helpdb");
    DEBUG(("about to load framecmds\n"));
    class_Load("framecmds");
#ifdef CONTRIB_ENV
    DEBUG(("about to load printopts\n"));
    class_Load("printopts");
#endif /* CONTRIB_ENV */

    DEBUG(("OUT init class\n"));
    return TRUE;
}

