/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/papers.c,v 1.5 1993/07/16 14:24:15 rr2b Exp $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/papers.c,v $ */
/* $Author: rr2b $ */

#ifndef lint
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/papers.c,v 1.5 1993/07/16 14:24:15 rr2b Exp $";
#endif /* lint */
/*
 * papers.c
 *
 * This does most of the work for the EOS applications.
*/

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *  For full copyright information see:'mit-copyright.h'     *
 *************************************************************/

#include <mit-copyright.h>
#include <class.h>
#include <bind.ih>
#include <buffer.ih>
#include <complete.ih>
#include <cursor.ih>
#include <dataobj.ih>
#include <environ.ih>
#include <envrment.ih>
#include <eos.h>
#include <fontdesc.ih>
#include <frame.ih>
#include <hesiod.h>
#include <im.ih>
#include <menulist.ih>
#include <message.ih>
#include <newbttnv.ih>
#include <point.h>
#include <pshbttn.ih>
#include <rect.h>
#include <scroll.ih>
#include <stdio.h>
#include <style.ih>
#include <text.ih>
#include <textv.ih>
#include <view.ih>

/* sys/types.h in AIX PS2 defines "struct label",  causing a type name clash.
  Avoid this by temporarily redefining "label" to be something else. */
#define label gezornenplatz
#include <eosfx.ih>
/* eosfx.ih includes sys/types.h */
#undef label

#include <eos.ih>   /* eos.ih uses struct label */
#include <papers.eh>   /* papers.eh uses struct label */

extern boolean papers__InitializeObject();
extern void papers__SetTitle();
extern void papers__SetDisplay();
extern void papers__FinalizeObject();
extern void papers__LinkTree();
extern void papers__FullUpdate();
extern struct view *papers__Hit();

#include <sort.h>

/* #define DEBUG_SORT */

#ifdef DEBUG_SORT
#define DEBUG
#endif

#ifdef DEBUG
#include <stdio.h>
#define debug(x) printf x ; fflush(stdin);
#endif /* DEBUG */

#define	SortByAuthor	1
#define	SortByNumber	2
#define	SortByFilename	3
#define	SortByDesc	4
#define	SortByCreate	5
#define	SortNoMore	0
#define	SortPreference	"SortOrder"
#define	SortLimit	5
#define	SortKeysHandouts    0
#define	SortKeysGrading	    1
#define	SortKeysExchange    2
static char *papertypes[3][2] = {
    { "Handouts", "number,author" },
    { "Grading", "number,author,filename" },
    { "Exchange", "author,filename" }
};
static int comparison[3][SortLimit];


/* One bit of data that is global to papers.c and papersaux.c */
struct menulist *papers_global_menus;
static struct cursor *clockcursor;
static struct style *deleted;
static struct style *marked;

void ToggleMark();
void RemoveMarks();
char *mktemp();
#ifndef _IBMR2
void free();
#endif _IBMR2
char *strtok();
void SetSortOrder();

static char *UniqueBufferName(name)
char *name;
/*
 Returns a string name of buffer which is guaranteed to not 
 already exist. If, in the unlikely case, in which a name
 cannot be found, NULL will be returned.
 */
{
    int suffix, namelen;
    static char *uname;

    namelen = strlen(name);

    if (buffer_FindBufferByName(name) == NULL)
	/* The name already is unique! */
	return name;

    /* Allocate room for unique name, plus a bit extra for suffix */
    uname = (char *) malloc(strlen(name) + 5);
    strcpy(uname, name);

    for (suffix = 1; suffix < 1000 ; suffix++) {
	sprintf(uname + namelen, "-%d", suffix);
	if (buffer_FindBufferByName(uname) == NULL)
	    return uname;
    }
    /* There are over 1,000 buffers with a similar name! (Erk!)
      So - give up!
      */
    return NULL;
}


static char *RealName(name)
char *name;
/*
  Takes a username and tries to find a password entry for it via Hesiod;
  If the resolve fails or the password entry cannot be parsed, then the
  original name is returned, else the name given in passwd is returned, with
	the parameter name following in parentheses;
  e.g. RealName("jsmith") == "jsmith" || "John Smith (jsmith)";
 */
{
    char **namelist, *realname, *tmp;
    static char finalname[128];
    char *index();
    int i;

    if ((namelist = hes_resolve(name, "passwd")) == NULL) {
	strcpy(finalname, name);
	strcat(finalname, "(no hesiod info)");
    } else {
	/* Extract name from password entry */
	realname = *namelist;
	for (i=0; i<4; i++)
	    if ((realname = index(++realname, ':')) == NULL) {
		/* Password entry is screwy - so give up and return original */
		strcpy(finalname, name);
		return finalname;
	    }
	/* Remove rest of password entry */
	if ((tmp = index(++realname,':')) != NULL)
	    *tmp = '\0';
	/* Make sure this is just the name, no unneccassry junk */
	if ((tmp = index(realname, ',')) != NULL)
	    *tmp = '\0';
	/* Just to be nice, add on the original name */
	strcpy(finalname, realname);
	strcat(finalname, " (");
	strcat(finalname, name);
	strcat(finalname, ")");
    }
    return finalname;
}


void CancelOperation(self, fxp)
struct papers *self;
FX **fxp;
/* Tidy up after an fx operation */
{
    eosfx_Close(fxp);
    text_SetReadOnly(self->textobj, TRUE);
    papers_WantInputFocus(self, self->textv);
    papers_WantUpdate(self, self->textv);
    papers_WantInputFocus(self, self);
    StopWaitCursor();
    im_ForceUpdate();
}

boolean OpenCourse(self, fxp, continueIfPoss)
struct papers *self;
FX **fxp;
boolean continueIfPoss; /* If we should continue after a warning from server */
{
    char *errormsg;

    errormsg = eosfx_OpenCourse(self->course, fxp);
    /* (fxp == NULL && errormsg) or (fxp is valid for reads&writes) */
    /* or (fxp is valid for reads, invalid for writes && errormsg) */
    if (fxp == NULL || errormsg) {
	/* fxp is not valid for BOTH reads & writes */
	message_DisplayString(self, DIALOG, errormsg);
	if (fxp && continueIfPoss) {
	    /* fxp is valid for reads */
	    message_DisplayString(self, MESSAGE, "Continuing...");
	    return TRUE;
	} else {
	    /* fxp is invalid */
	    message_DisplayString(self, MESSAGE, "Cancelled.");
	    CancelOperation(self, fxp);
	    return FALSE;
	}
    } else
	/* fxp is valid for both reads & writes */
	return TRUE;
}


void MarkAsTaken(self, fxp, index)
struct papers *self;
FX *fxp;
struct paperPositions *index;
/*
  When grading, if a paper is selected for EDIT or KEEP, we want to change
  the status of the paper to TAKEN.
   This routine is careful not to close the fxp.
 */
{
    Paper tmppaper;
    char *errormsg;

    eosfx_PaperCopy(&index->paper->p, &tmppaper);
    tmppaper.type = TAKEN;
    if (errormsg = eosfx_Move (fxp, &index->paper->p, &tmppaper)) {
	message_DisplayString(self, DIALOG, errormsg);
	return;
    }
    index->flags |= eos_PaperTaken;
    message_DisplayString(self, MESSAGE, "Paper has been marked as taken.");
    CancelOperation(self, NULL);
    im_ForceUpdate();
}

#ifdef OLD_PAPERS_DISPLAY
/* Macro to make lines smaller */
#define dispWindow (self->daddy->displaywindow)

void papers_Display(self, triggerer, rock)
struct papers *self;
struct observable *triggerer;
long rock;
/* create frame and window;
  For each paper marked {
      find out which paper they're talking about;
      make a unique buffer name similar to paper.filename;
      create buffer;
      pull through the paper into temporary file;
      read file into buffer;
      set buffer name;
      place buffer into frame;
          goose the fontsize to the one we want for display;
  }
  finish up;
*/
{
    FX *fxp;
    char filename[128];
    char bufname[128];
    char *errormsg;
    int i;
    boolean GotOne = FALSE, many;
    struct paperPositions *index;
    struct textview *tv;
    long dsize, usize;
    enum style_FontSize basis;

    if (self->markcount == 0) {
	message_DisplayString(self, MESSAGE, "There are no papers selected!");
	return;
    }

    if (!self->daddy) {
	message_DisplayString(self, DIALOG, "I've been orphaned! Cannot tell daddy what you want to display!");
	CancelOperation(self, NULL);
	return;
    }

    index = self->Positions;
    if (!OpenCourse(self, &fxp, TRUE)) {
	CancelOperation(self, &fxp);
	return;
    }

    many = environ_GetProfileSwitch ("DisplayMany", FALSE);

    /* Create the new window - a frame/buffer view */
    if (many || dispWindow == NULL || dispWindow->im == NULL || dispWindow->data == NULL || (!buffer_Visible(dispWindow->data))) {
	if (dispWindow == NULL) dispWindow = (struct readWindow *)malloc(sizeof (struct readWindow));
	dispWindow->im = im_Create(NULL);
	dispWindow->fr = frame_New();
	if (!dispWindow->im || !dispWindow->fr) {
	    message_DisplayString(self, DIALOG, "Could not create a new window!");
	    CancelOperation(self, &fxp);
	    return;
	}
	/* Make sure there is a message handler for the new frame. -njw 9/6/90 */
	frame_PostDefaultHandler(dispWindow->fr, "message", frame_WantHandler(dispWindow->fr, "message"));
	im_SetView(dispWindow->im, dispWindow->fr);
	/* We also want to give the frame something to chew on, in case
	    we cannot find it a file to look at -njw */
	dispWindow->data = buffer_FindBufferByName("scratch");
	if (!dispWindow->data)
	    dispWindow->data = buffer_Create("scratch","scratch", NULL, NULL);
	buffer_SetName(dispWindow->data, "Display");
	buffer_SetScratch(dispWindow->data, TRUE);
	buffer_SetCkpClock(dispWindow->data, 0);
	frame_SetBuffer(dispWindow->fr, dispWindow->data, TRUE);
    } else im_ExposeWindow(dispWindow->im);
    message_DisplayString(dispWindow->fr, MESSAGE, "Please wait: retrieving the file(s)...");
    im_ForceUpdate();

    /* For each marked paper... */
    for (i = self->markcount; i>0; i--) {
	while (!((index->flags) & eos_PaperMarked)) index = index->next;

	if (index->flags & eos_PaperDeleted) {
	    message_DisplayString(self, MESSAGE, "That paper has been deleted.");
	} else {
	    strcpy(bufname, UniqueBufferName(index->paper->p.filename));
	    if (bufname == NULL) {
		message_DisplayString(self, DIALOG, "What a lot of buffers you've got! Cannot create a new one...");
		CancelOperation(self, &fxp);
		return;
	    }
	    /* Receive the paper into a temporary file */
	    strcpy(filename, "/tmp/");
	    strcat(filename, index->paper->p.filename);
	    strcpy(filename, eosfx_LocalUnique(filename));
	    if (errormsg = eosfx_RetrieveFile(fxp, &index->paper->p, filename)) {
		message_DisplayString(dispWindow->fr, MESSAGE, "Error encountered");
		message_DisplayString(dispWindow->fr, DIALOG, errormsg);
		if (!GotOne && many) im_Destroy(dispWindow->im);
		CancelOperation(self, &fxp);
		return;
	    } else GotOne = TRUE;
	    dispWindow->data = buffer_GetBufferOnFile(filename, buffer_ForceNew);
	    unlink(filename);
	    buffer_SetFilename(dispWindow->data, NULL);
	    buffer_SetName(dispWindow->data, bufname);
	    frame_SetBuffer(dispWindow->fr, dispWindow->data, TRUE);

	    /* tickle the font size if it's different from the default */
	    tv = (struct textview *) frame_GetView(dispWindow->fr);
	    if (strcmp (class_GetTypeName(tv), "textview") == 0) {
		struct style *ds = textview_GetDefaultStyle(tv);
		usize = environ_GetProfileInt("DisplayFontsize", 20);
		style_GetFontSize(ds, &basis, &dsize);
		if (basis == style_ConstantFontSize && dsize != usize)
		    style_SetFontSize(ds, style_ConstantFontSize, usize);
	    }

	    frame_SetCommandEnable(dispWindow->fr, TRUE);
	}
	ToggleMark(self, index);
	index = index->next;
    }
    message_DisplayString(dispWindow->fr, MESSAGE, "");
    CancelOperation(self, &fxp);
    return;
}

#else /* OLD_PAPERS_DISPLAY */

void papers_Display(self, triggerer, rock)
struct papers *self;
struct observable *triggerer;
long rock;
/* create frame and window;
  For each paper marked {
      find out which paper they're talking about;
      make a unique buffer name similar to paper.filename;
      create buffer;
      pull through the paper into temporary file;
      read file into buffer;
      set buffer name;
      place buffer into frame;
          goose the fontsize to the one we want for display;
  }
  finish up;
*/
{
    FX *fxp;
    char filename[128];
    char bufname[128];
    char *errormsg;
    int i;
    struct paperPositions *index;
    struct textview *tv;
    long dsize, usize;
    enum style_FontSize basis;

    if (self->markcount == 0) {
	message_DisplayString(self, MESSAGE, "There are no papers selected!");
	return;
    }

    if (!self->daddy) {
	message_DisplayString(self, DIALOG, "I've been orphaned! Cannot tell daddy what you want to display!");
	CancelOperation(self, NULL);
	return;
    }

    if (!OpenCourse(self, &fxp, TRUE)) {
	CancelOperation(self, &fxp);
	return;
    }

    message_DisplayString(self, MESSAGE, "Please wait: retrieving the file(s)...");

    im_ForceUpdate();

    index = self->Positions;

   /* For each marked paper... */
    for (i = self->markcount; i>0; i--) {
	while (!((index->flags) & eos_PaperMarked)) index = index->next;

	if (index->flags & eos_PaperDeleted) {
	    message_DisplayString(self, MESSAGE, "That paper has been deleted.");
	} else {
	    strcpy(bufname, UniqueBufferName(index->paper->p.filename));
	    if (bufname == NULL) {
		message_DisplayString(self, DIALOG, "What a lot of buffers you've got! Cannot create a new one...");
		CancelOperation(self, &fxp);
		return;
	    }
	    /* Receive the paper into a temporary file */
	    strcpy(filename, "/tmp/");
	    strcat(filename, index->paper->p.filename);
	    strcpy(filename, eosfx_LocalUnique(filename));
	    if (errormsg = eosfx_RetrieveFile(fxp, &index->paper->p, filename)) {
		message_DisplayString(self, MESSAGE, "Error encountered");
		message_DisplayString(self, DIALOG, errormsg);
		CancelOperation(self, &fxp);
		return;
	    }
	    eos_SetBuffer(self->daddy, filename, buffer_ForceNew);
	    unlink(filename);
	    eos_SetFontDisplay(self->daddy);
#if 0
	    /* tickle the font size if it's different from the default */
	    tv = (struct textview *) frame_GetView(self->daddy->frame);
	    if (strcmp (class_GetTypeName(tv), "textview") == 0) {
		struct style *ds = textview_GetDefaultStyle(tv);
		usize = environ_GetProfileInt("DisplayFontsize", 20);
		style_GetFontSize(ds, &basis, &dsize);
		if (basis == style_ConstantFontSize && dsize != usize)
		    style_SetFontSize(ds, style_ConstantFontSize, usize);
	    }
#endif
	}
	ToggleMark(self, index);
	index = index->next;
    }
    message_DisplayString(self, MESSAGE, "Done.");
    CancelOperation(self, &fxp);
    return;
}
#endif /*OLD_PAPERS_DISPLAY */

static void Print(self, rock)
struct papers *self;
long rock;
{
    FX *fxp;
    FILE *printer;
    char string[128];
    char printcommand[256], command[256];
    char *errormsg;
    int i;
    struct paperPositions *index;
    char *AndrewDir();

    if (self->markcount == 0) {
	message_DisplayString(self, MESSAGE, "There are no papers selected!");
	return;
    }

    index = self->Positions;

    /* Create the print command to activate the pipe...
      Use ezprint, specifying file to come from the stdin
      */
    strcpy(printcommand, AndrewDir("/bin/ezprint"));
    strcat(printcommand, " -s -o ");

    StartWaitCursor();
    if (!OpenCourse(self, &fxp, TRUE)) return;

    /* For each marked paper... */
    for (i = self->markcount; i>0; i--) {
	while (!((index->flags) & eos_PaperMarked)) index = index->next;

	if (index->flags & eos_PaperDeleted) {
	    message_DisplayString(self, MESSAGE, "That paper has been deleted.");
	} else {
	    /* Start a pipe to the print command, ready for a file to be
		given to it.
		*/
	    strcpy(command, printcommand);
	    strcat(command, index->paper->p.filename);
	    if ((printer = popen(command, "w")) == NULL) {
		message_DisplayString(self, DIALOG, "Could not run ezprint!");
		CancelOperation(self, &fxp);
		return;
	    }

	    /* Retrieve the file, placing it into the pipe which leads into
	     the printer.
	     */
	    if (errormsg = eosfx_Retrieve(fxp, &index->paper->p, printer)) {
		CancelOperation(self, &fxp);
		message_DisplayString(self, DIALOG, errormsg);
		pclose(printer);
		return;
	    }

	    pclose(printer);
	    strcpy(string, index->paper->p.filename);	
	    strcat(string, " has been sent to printer");
	    message_DisplayString(self, MESSAGE, string);
	}
	ToggleMark(self, index);
	im_ForceUpdate();
	index = index->next;
    }

    CancelOperation(self, &fxp);
}

void papers_Keep(self, triggerer, rock)
struct papers *self;
struct observable *triggerer;
long rock;
{
    FX *fxp;
    char string[128], filename[128];
    char *errormsg;
    int i;
    struct paperPositions *index;

    if (self->markcount == 0) {
	message_DisplayString(self, MESSAGE, "There are no papers selected!");
	return;
    }

    index = self->Positions;

    if (self->thiswindow == papersGrading) {
	/* we need to inform the server the paper has been taken, i.e.
	 * we need write access */
	if (!OpenCourse(self, &fxp, FALSE))
	    return;
    } else
	if (!OpenCourse(self, &fxp, TRUE))
	    return;

    /* For each marked paper... */
    for (i = self->markcount; i>0; i--) {
	while (!((index->flags) & eos_PaperMarked)) index = index->next;

	if (index->flags & eos_PaperDeleted) {
	    message_DisplayString(self, MESSAGE, "That paper has been deleted.");
	} else {
	    /* Retrieve the paper into a locally unique named file */ 
	    strcpy(filename, eosfx_LocalUnique(index->paper->p.filename));
	    if (errormsg = eosfx_RetrieveFile(fxp, &index->paper->p, filename)) {
		CancelOperation(self, &fxp);
		message_DisplayString(self, DIALOG, errormsg);
		return;
	    }

	    if (self->thiswindow == papersGrading) 
		MarkAsTaken(self, fxp, index);

	    strcpy(string, "File has been received as ");
	    strcat(string, filename);
	    message_DisplayString(self, MESSAGE, string);
	}
	im_ForceUpdate();
	ToggleMark(self, index);
	index = index->next;
    }

    CancelOperation(self, &fxp);
    return;
}

void papers_Edit(self, triggerer, rock)
struct papers *self;
struct observable *triggerer;
long rock;
/*
  Retrieve paper into local file and tell the parent eos to make a buffer for
  the new file 
 */
{
    FX *fxp;
    char string[128];
    char filename[128];
    char *errormsg;
    int i;
    struct paperPositions *index;
    int	default_edit_keep = TRUE;   /* Students will want to keep edits by default */

    if (self->markcount == 0) {
	message_DisplayString(self, MESSAGE, "There are no papers selected!");
	return;
    }

    if (!self->daddy) {
	message_DisplayString(self, DIALOG, "I've been orphaned! Cannot tell daddy what you want to edit!");
	CancelOperation(self, NULL);
	return;
    }

    index = self->Positions;

    if (self->thiswindow == papersGrading) {
	/* we need to inform the server the paper has been taken, i.e.
	 * we need write access */
	if (!OpenCourse(self, &fxp, FALSE))
	    return;
    } else
	if (!OpenCourse(self, &fxp, TRUE))
	    return;

    /* For each marked paper... */
    for (i = self->markcount; i>0; i--) {
	while (!((index->flags) & eos_PaperMarked)) index = index->next;

	if (index->flags & eos_PaperDeleted) {
	    message_DisplayString(self, MESSAGE, "That paper has been deleted.");
	} else {
	    /* Retrieve paper into locally unique named file */
	    /* Graders add uname to file name */
	    if (self->thiswindow == papersGrading) {
		strcpy(filename, index->paper->p.author);
		strcat(filename, "_");
		strcat(filename, index->paper->p.filename);
		strcpy(filename, eosfx_LocalUnique(filename));
	    } else {
		strcpy(filename, eosfx_LocalUnique(index->paper->p.filename));
	    }
	    if (errormsg = eosfx_RetrieveFile(fxp, &index->paper->p, filename)) {
		CancelOperation(self, &fxp);
		message_DisplayString(self, DIALOG, errormsg);
		return;
	    }

	    if (self->thiswindow == papersGrading) {
		/* graders will not want to keep by default */
		default_edit_keep = FALSE;
		MarkAsTaken(self, fxp, index);
	    }

	    /* If EditImpliesKeep is true, we keep the file. */
	    if (environ_GetProfileSwitch("EditImpliesKeep", default_edit_keep)) {
		strcpy(string, "Received file as ");
		strcat(string, filename);
		message_DisplayString(self, MESSAGE, string);
		/* Tell parent eos about the new file */
		eos_SetBuffer(self->daddy, filename, buffer_ForceNew);
	    } else {
		/* Tell parent eos about the new file */
		eos_SetBuffer(self->daddy, filename, buffer_ForceNew);
		unlink (filename); /* Must unlink AFTER read */
	    }
	    /* If we're grading, hang the struct paper * off the buffer. */
	    if (self->thiswindow == papersGrading) {
		Paper *ppaper = (Paper *)malloc(sizeof(Paper));
		eosfx_PaperCopyContents (&(index->paper->p), ppaper);
		dataobject_Put(buffer_GetData(frame_GetBuffer(self->daddy->frame)), self->daddy->paperatom, self->daddy->paperatom, (long)ppaper);
	    }
	}
	im_ForceUpdate();
	ToggleMark(self, index);
	index = index->next;
    }

    CancelOperation(self, &fxp);
    return;

}

void papers_Hide( self, triggerer, rock)
struct papers *self;
struct observable *triggerer;
long rock;
{
    if (!self) return;
    self->IDoNotExist = TRUE;
    /* "If I close my eyes, then you can't see me!" */
    im_VanishWindow(papers_GetIM(self));
}

void papers_GradingToggleAndList(self, triggerer, rock)
struct papers *self;
struct observable *triggerer;
long rock;
/* This is normally a button-triggered procedure, but it can
  * also be used to perform the initial listing of the papers
  * i.e. p = papers_New();
  *      set p->toggle to be papers_OLD or papers_NEW
    *      papers_GradingToggleAndList(p, NULL, 0);
  */
{
    struct pushbutton *button =  (struct pushbutton *) newbuttonview_GetDataObject(self->toggle);

    if (self->toggled == papers_NEW) {
	papers_SetTitle(self, "Old Papers");
	pushbutton_SetText(button, "NEW DOCS");
	self->toggled = papers_OLD;
	papers_ListGrade(self);
    } else if (self->toggled == papers_OLD) {
	papers_SetTitle(self, "Papers to Grade");
	pushbutton_SetText(button, "OLD DOCS");
	self->toggled = papers_NEW;
	papers_ListGrade(self);
    }
    return;
}

/* --------- Menu functions -------- */

static char *confirms[] = { "Confirm", "Cancel", NULL };

void papers_ReadList(self, rock)
struct papers *self;
long rock;
/* Called from menu option "Update List" */
{
    switch (self->thiswindow) {
	case papersGrading:
	    papers_ListGrade(self);
	    break;
	case papersHandouts:
	    papers_ListHandouts(self);
	    break;
	case papersExchange:
	    papers_ListExchange(self);
	    break;
	default:
	    break;
    }
}

void DeletePaper(self, paper)
struct papers *self;
struct Paper *paper;
/* Used once in the entire code. Hmmph */
{
    FX *fxp;
    char *errormsg;

    if (!OpenCourse(self, &fxp, FALSE))
	return;
    if (errormsg = eosfx_Delete(fxp, paper)) {
 	CancelOperation(self, &fxp);
	message_DisplayString(self, DIALOG, errormsg);
	return;
    }

    CancelOperation(self, &fxp);
}

void papers_RemovePaper(self, rock)
struct papers *self;
long rock;
/* Invoked from the menu option. We want to delete every selected paper */
{
    FX *fxp;
    char *errormsg;
    int answer, i;
    struct paperPositions *index;

    if (self->markcount == 0) {
	message_DisplayString(self, MESSAGE, "There are no papers selected!");
	return;
    }

    message_MultipleChoiceQuestion(self, DIALOG, "Please confirm you wish to do this", 1, &answer, confirms, NULL);
    if (answer) {
	message_DisplayString(self, MESSAGE, "Cancelled");
	CancelOperation(self, NULL);
	return;
    }

    index = self->Positions;
    if (!OpenCourse(self, &fxp, FALSE)) return;

    for (i = self->markcount; i>0; i--) {
	while(!((index->flags) & eos_PaperMarked)) index = index->next;

	if (index->flags & eos_PaperDeleted)
	    message_DisplayString(self, MESSAGE, "That paper is already deleted");
	else {
	    if (errormsg = eosfx_Delete(fxp, &index->paper->p)) {
		CancelOperation(self, &fxp);
		message_DisplayString(self, DIALOG, errormsg);
		return;
	    }

	    ToggleMark(self, index);
	    index->flags |= eos_PaperDeleted;
	    index->environment = environment_New();
	    index->environment = environment_InsertStyle(self->textobj->rootEnvironment, index->textpos, deleted, TRUE);
	    environment_SetLength(index->environment, index->textlength);
	    text_RegionModified(self->textobj, index->textpos, index->textlength);
	}
	index = index->next;
    }

    if (self->markcount > 1)
	message_DisplayString(self, MESSAGE, "Selected papers have been deleted");
    else
	message_DisplayString(self, MESSAGE, "The paper has been deleted");

    CancelOperation(self, &fxp);
    return;
}

static char *handoutchoices[] =
{
    "The contents of the current edit window",
    "A file",
    "Cancel",
    NULL
};


void papers__GradingListType(self, type)
struct papers *self;
enum papers_Toggles type;
{
    /* This routine is for external use - it sets the object to
	be a listing of the required type.
	*/
    if (type == papers_NEW)
	self->toggled = papers_OLD;
    else
	self->toggled = papers_NEW;
    papers_GradingToggleAndList(self, NULL, 0L);
}

static void MakeHandout(self)
struct papers *self;
{
    Paper paper;
    char answer[128], name[33], strnum[6], *errormsg;
    long result;

    StopWaitCursor();
    if (self->daddy == NULL) {
	message_DisplayString(self, DIALOG, "Program bug - I am an orphan!");
	CancelOperation(self, NULL);
	return;
    }

    message_MultipleChoiceQuestion(self, DIALOG, "What do you want to use as the handout?", 0, &result, handoutchoices, NULL);
    if (result == 2) {
	message_DisplayString(self, MESSAGE, "Cancelled.");
	CancelOperation(self, NULL);
	return;
    }

    eosfx_PaperClear(&paper);
    paper.type = HANDOUT;

    if (result == -1) {
	message_DisplayString(self, DIALOG, "Failed to get a straight answer!");
	CancelOperation(self, NULL);
	return;
    }

    if (result == 0) {
	strcpy(answer, mktemp("/tmp/.eosXXXXXX"));
	buffer_WriteToFile(frame_GetBuffer(self->daddy->frame), answer, buffer_ReliableWrite);
	paper.filename = buffer_GetName(frame_GetBuffer(self->daddy->frame));
    } else {
	/* Specify filename */
	if (completion_GetFilename(self, "What is the name of the file? ", NULL, answer, sizeof(answer), FALSE, TRUE) == -1) {
	    CancelOperation(self, NULL);
	    return;
	}
	paper.filename = eosfx_PathTail(answer);
	message_DisplayString(self, MESSAGE, "");
	im_ForceUpdate();
    }

    if (message_AskForString(self, DIALOG, "Please give a description of the handout", "", name, 32) == -1) {
	message_DisplayString(self, MESSAGE, "Cancelled.");
	CancelOperation(self, NULL);
	return;
    } else
	paper.desc = name;

    if (message_AskForString(self, DIALOG, "For which class meeting number is the handout?", "", strnum, 5) == -1) {
	message_DisplayString(self, MESSAGE, "Cancelled.");
	CancelOperation(self, NULL);
	return;
    } else
	paper.assignment = atoi(strnum);

    StartWaitCursor();
    paper.type = HANDOUT;
    message_DisplayString(self, MESSAGE, "Please wait: Sending handout...");
    im_ForceUpdate();
    errormsg = eosfx_SendFile(self->course, answer, &paper, FALSE);
    if (result == 0) unlink (answer);	/* del temp */
    if (errormsg != NULL) {
	message_DisplayString(self, MESSAGE, "Cancelled");
	message_DisplayString(self, DIALOG, errormsg);
    } else message_DisplayString(self, MESSAGE, "Done.");
    CancelOperation(self, NULL);
    return;
}

static void MakeExchange(self)
struct papers *self;
{
    Paper paper;
    char answer[128], name[128], *errormsg;
    long result;

    StopWaitCursor();
    message_MultipleChoiceQuestion(self, DIALOG, "What do you want to use as the submission?", 0, &result, handoutchoices, NULL);

    eosfx_PaperClear(&paper);

    switch (result) {
	case -1:
	    message_DisplayString(self, DIALOG, "failed to get a straight answer...");
	    break;
	case 0:
	    /* Take current buffer */
	    if (self->daddy == NULL) {
		message_DisplayString(self, DIALOG, "Program bug - Do not know what the buffer is!");
		CancelOperation(self, NULL);
		return;
	    }

	    if (message_AskForString(self, DIALOG, "Please give a name for the paper", "", name, 32) == -1) {
		message_DisplayString(self, MESSAGE, "Cancelled.");
		CancelOperation(self, NULL);
		return;
	    }

	    message_DisplayString(self, MESSAGE, "Please wait: Sending paper...");
	    StartWaitCursor();
	    im_ForceUpdate();

	    strcpy(answer, mktemp("/tmp/.eosXXXXXX"));
	    buffer_WriteToFile(frame_GetBuffer(self->daddy->frame), answer, buffer_ReliableWrite);
	    paper.type = EXCHANGE;
	    paper.filename = eosfx_PathTail(name);
	    errormsg = eosfx_SendFile(self->course, answer, &paper, FALSE);
	    unlink (answer);
	    if (errormsg != NULL) {
		message_DisplayString(self, MESSAGE, "Cancelled");
		message_DisplayString(self, DIALOG, errormsg);
	    } else message_DisplayString(self, MESSAGE, "Done.");
	    break;
	case 1:
	    /* Specify filename */
	    if (completion_GetFilename(self, "What is the name of the file? ", NULL, answer, sizeof(answer), FALSE, TRUE) == -1) return;
	    /* Since completion MUST match, we have the name of the file - can go straight ahead and throw it in! */
	    message_DisplayString(self, MESSAGE, "Please wait: Sending exchange paper...");
	    StartWaitCursor();
	    im_ForceUpdate();
	    paper.type = EXCHANGE;
	    errormsg = eosfx_SendFile(self->course, answer, &paper, FALSE);
	    if (errormsg != NULL) {
		message_DisplayString(self, MESSAGE, "Cancelled");
		message_DisplayString(self, DIALOG, errormsg);
	    } else message_DisplayString(self, MESSAGE, "Done.");
	    break;
	case 2:
	    /* Cancel */
	    break;
    }

    CancelOperation(self, NULL);
    return;
}


void MakePaper(self, triggerer, rock)
struct papers *self;
struct observable *triggerer;
long rock;
/* Button "SUBMIT" - could be either a handout or exchange paper */
{
    switch (self->thiswindow) {
	case papersHandouts:
	    MakeHandout(self);
	    break;
	case papersExchange:
	    MakeExchange(self);
	    break;
	default:
	    break;
    }

    return;
}


/* ----- Misc functions & exported methods ------ */

int
compare(p1, p2, data)
Paperlist p1, p2;
int data;
{
    int i,j;
#ifdef DEBUG_SORT
debug(("Begin comparison:\n"));
#endif

    for (i = 0; comparison[data][i] != SortNoMore; i++) {
	switch (comparison[data][i]) {
	    case SortByNumber:
#ifdef DEBUG_SORT
debug(("compare using assignment number\n"));
#endif
		if (p2->p.assignment < p1->p.assignment)
		    return -1;
		else if (p2->p.assignment > p1->p.assignment)
		    return 1;
		break;
	    case SortByAuthor:
#ifdef DEBUG_SORT
debug(("compare using author\n"));
#endif
		j = strcmp(p2->p.author, p1->p.author);
		if (j != 0)
		    return j;
		break;
	    case SortByFilename:
#ifdef DEBUG_SORT
debug(("compare using filename\n"));
#endif
		j = strcmp(p2->p.filename, p1->p.filename);
		if (j != 0)
		    return j;
		break;
	    case SortByDesc: /* Why would anyone want to sort on this ? */
#ifdef DEBUG_SORT
debug(("compare using description\n"));
#endif
		j = strcmp(p2->p.desc, p1->p.desc);
		if (j != 0)
		    return j;
		break;
	    case SortByCreate: /* Creation time */
#ifdef DEBUG_SORT
debug(("compare using creation time\n"));
#endif
		if (p2->p.created.tv_sec < p1->p.created.tv_sec)
		    return -1;
		else if (p2->p.created.tv_sec > p1->p.created.tv_sec)
		    return 1;
		break;
		      
	}
    }
    return 0; /* If we haven't discriminated it yet, assume it is identical */
}


int papers__ListHandouts(self)
struct papers *self;
/* Opens the course. Gets the list of papers from FX
  * Steps thru list, placing text into self->textobj describing the
  * paper. Also sets global array paperstextpos[0..numofpapers]
  * where paperstextpos[i] = the text position of the start of the 
  * i'th paper description.
		    * numofpapers is set to be the total counted. This value is
		    * also returned.
		    * The textobj is set to be writable during list and then after 
		    * the procedure has finished, it sets text to be read-only
		    */
{
    int i;
    Paper criterion;
    Paperlist node;
    FX *fxp;
    char *errormsg;
    char string[256];

    message_DisplayString(self, MESSAGE, "Please wait: Reading the list...");
    text_SetReadOnly(self->textobj, FALSE);
    StartWaitCursor();
    im_ForceUpdate();

    text_ClearCompletely(self->textobj);

    /* Make sure the list is 'clean' before we get a new list */
    eosfx_DestroyPositions(&self->Positions);
    eosfx_ListDestroy(&self->list);
    self->markcount = 0;

    if (!OpenCourse(self, &fxp, TRUE)) {
	message_DisplayString(self, MESSAGE, "Operation aborted due to errors");
	return 0;
    }

    /* Set the criterion - totally clean apart from type.
      * i.e. We want anything at all - so long as it is a paper.
      */
    eosfx_PaperClear(&criterion);
    criterion.type = HANDOUT;
    criterion.assignment = self->assignment;
#if 0
    criterion.author = self->student;
#endif

    if (errormsg = eosfx_List(fxp, &criterion, &self->list)) {
	textput(errormsg);
	message_DisplayString(self, MESSAGE, "Operation aborted due to errors");
	self->list = NULL;
	CancelOperation(self, &fxp);
	return 0;
    }

    eosfx_Close(&fxp);

    /* Step through the list of papers, displaying each one. */
    i = 0;
    sort(&(self->list->Paperlist_res_u.list), SortKeysHandouts);
    for (node = self->list->Paperlist_res_u.list; node !=NULL; node = node->next) {
	i++;
	sprintf(string, "Class Meeting %d: '%s'\nDescription: %s\n\tAuthor: %s\n\n", node->p.assignment, node->p.filename,node->p.desc, RealName(node->p.author));
	eosfx_AddPaperText(&self->Positions, node, text_GetLength(self->textobj), strlen(string));
	textput(string);
    }

    if (!i) {
	char tmpstring[50];
	if (self->assignment) sprintf (tmpstring, "class meeting number %d", self->assignment);
	else sprintf (tmpstring, "any class meeting number");
	sprintf (string, "No handouts of %s available at present.\n", tmpstring);
	textput(string);
	self->list = NULL;
	message_DisplayString(self, MESSAGE, "");
    } else {
	if (i != 1) sprintf(string, "There are %d handouts", i);
	else sprintf(string, "There is one handout available.");
	message_DisplayString(self, MESSAGE, string);
    }

    message_DisplayString(self, MESSAGE, "");
    CancelOperation(self, NULL);
    return i;
}

int papers__ListExchange(self)
struct papers *self;
/* Opens the course. Gets the list of papers from FX
  * Steps thru list, placing text into self->textobj describing the
  * paper. Also sets self->list to show the list of papers and attributes
  * The textobj is set to be writable during list and then after 
  * the procedure has finished, it sets text to be read-only
  * The return value is the number of papers counted
  */
{
    int i;
    Paper criterion;
    Paperlist node;
    FX *fxp;
    char *errormsg;
    char string[256];

    text_SetReadOnly(self->textobj, FALSE);
    message_DisplayString(self, MESSAGE, "Please wait: reading list");
    StartWaitCursor();
    im_ForceUpdate();

    text_ClearCompletely(self->textobj);

    /* Make sure the list is 'clean' before we get a new list */
    eosfx_DestroyPositions(&self->Positions);
    eosfx_ListDestroy(&self->list);
    self->markcount = 0;

    if (!OpenCourse(self, &fxp, TRUE)) {
	message_DisplayString(self, MESSAGE, "Operation aborted due to errors");
	return 0;
    }


    /* Set the criterion - totally clean apart from type.
      * i.e. We want anything at all - so long as it is a paper.
      */
    eosfx_PaperClear(&criterion);
    criterion.type = EXCHANGE;
    criterion.author = self->student;

    if(errormsg = eosfx_List(fxp, &criterion, &self->list)) {
	textput(errormsg);
	message_DisplayString(self, MESSAGE, "Operation aborted due to errors");
	self->list = NULL;
	CancelOperation(self, &fxp);
	return 0;
    }

    eosfx_Close(&fxp);

    /* Step through the list of papers, displaying each one. */
    i = 0;
    sort(&(self->list->Paperlist_res_u.list), SortKeysExchange);
    for (node = self->list->Paperlist_res_u.list; node !=NULL; node = node->next) {
	i++;
	sprintf(string, "'%s' by %s\n", node->p.filename, RealName(node->p.author));
	eosfx_AddPaperText(&self->Positions, node, text_GetLength(self->textobj), strlen(string));
	textput(string);
    }

    if (!i) {
	sprintf (string, "No exchanges by %s available at present.\n", (self->student)? self->student : "anyone");
	textput(string);
	self->list = NULL;
	message_DisplayString(self, MESSAGE, "");
    } else {
	if (i != 1) sprintf(string, "There are %d papers", i);
	else sprintf(string, "There is one paper available.");
	message_DisplayString(self, MESSAGE, string);
    }

    CancelOperation(self, NULL);
    return i;
}

int papers__ListGrade(self)
struct papers *self;
/* Opens the course. Gets the list of papers from FX
  * Steps thru list, placing text into self->textobj describing the
  * paper. Also sets global array paperstextpos[0..numofpapers]
  * where paperstextpos[i] = the text position of the start of the 
  * i'th paper description.
  * numofpapers is set to be the total counted. This value is
  * also returned.
  * The textobj is set to be writable during list and then after 
  * the procedure has finished, it sets text to be read-only
  */
{
    int i;
    Paper criterion;
    Paperlist node;
    FX *fxp;
    char *errormsg;
    char string[256], typestring[128];

    text_SetReadOnly(self->textobj, FALSE);
    message_DisplayString(self, MESSAGE, "Please wait: reading list");
    StartWaitCursor();
    text_ClearCompletely(self->textobj);
    im_ForceUpdate();

    /* Make sure the list is 'clean' before we get a new list */
    eosfx_DestroyPositions(&self->Positions);
    eosfx_ListDestroy(&self->list);
    self->markcount = 0;

    if (!OpenCourse(self, &fxp, TRUE)) {
	message_DisplayString(self, MESSAGE, "Operation aborted due to errors");
	return 0;
    }
    /* Set the criterion - 
     *  The defaults have been set to 0 in papers_InitializeClass
         * or to default values by papers_SetDefault.
         * default values of 0 are wild cards.
     */
    eosfx_PaperClear(&criterion);

    criterion.type = TYPE_WILDCARD;
    criterion.assignment = self->assignment;
    criterion.author = self->student;

    if (errormsg = eosfx_List(fxp, &criterion, &self->list)) {
	textput(string);
	message_DisplayString(self, MESSAGE, "Operation aborted due to errors");
	self->list = NULL;
	CancelOperation(self, &fxp);
	return 0;
    }

    eosfx_Close(&fxp);

    /* Step through the list of papers, displaying each one. */
    i = 0;
    sort(&(self->list->Paperlist_res_u.list), SortKeysGrading);
    for (node = self->list->Paperlist_res_u.list; node !=NULL; node = node->next) {
	/* Are we looking at old papers or new papers? */
	if (self->toggled == papers_NEW) {
	    if (node->p.type == TURNEDIN) {
		i++;
		sprintf(string, "%s by %s (Assignment %d)\n\n", node->p.filename, RealName(node->p.author), node->p.assignment);
		eosfx_AddPaperText(&self->Positions, node, text_GetLength(self->textobj), strlen(string));
		textput(string);
	    }
	} else {
	    /* Old papers are of several different types. So tell user
	     which paper is which type...
	     */
	    switch (node->p.type) {
		case TAKEN:
		    sprintf(typestring, "Taken by %s", RealName(node->p.owner));
		    break;
		case GRADED:
		    sprintf(typestring, "Graded by %s", RealName(node->p.owner));
		    break;
		case PICKEDUP:
		    strcpy(typestring, "Picked Up");
		    break;
		default:
		    typestring[0] = '\0';
	    }
	    if (typestring[0] != '\0') {
		i++;
		sprintf(string, "%s:\n%s by %s (Assignment %d)\n\n", typestring, node->p.filename, RealName(node->p.author), node->p.assignment);
		eosfx_AddPaperText(&self->Positions, node, text_GetLength(self->textobj), strlen(string));
		textput(string);
	    }
	}
    }

    if (!i) {
	if (self->assignment) sprintf (typestring, "assignment %d", self->assignment);
	else sprintf (typestring, "any assignment");
	sprintf (string, "No papers of %s by %s available at present.\n", typestring, (self->student)? self->student : "anyone");
	textput(string);
	message_DisplayString(self, MESSAGE, "");
	self->list = NULL;
    } else {
	if (i != 1) sprintf(string, "There are %d papers to grade", i);
	else sprintf(string, "There is one paper to grade.");
	message_DisplayString(self, MESSAGE, string);
    }

    CancelOperation(self, NULL);
    return i;
}

void papers__SetDefault(self)
struct papers *self;
{
    char uname[33], strnum[6];

    switch (self->thiswindow) {
	case papersGrading:
	    if (self->assignment) sprintf(uname, "%d", self->assignment);
	    else sprintf (uname, "*");
	    if (message_AskForString(self, MESSAGE, "List papers matching assignment number (* for all): ", uname, strnum, 5) == -1) return;
	    if (strnum[0] == '*') self->assignment = 0;
	    else self->assignment = atoi(strnum);
	    if (message_AskForString(self, MESSAGE, "List papers matching student user name (* for all): ", (self->student)? self->student : "*", uname, 32) == -1) return;
	    if (self->student != NULL) free (self->student);
	    if (uname[0] == '*') self->student = NULL;
	    else {
		self->student = malloc (strlen (uname) + 1);
		strcpy (self->student, uname);
	    }
	    break;
	case papersHandouts:
	    if (self->assignment) sprintf(uname, "%d", self->assignment);
	    else sprintf (uname, "*");
	    if (message_AskForString(self, MESSAGE, "List handouts matching class meeting number (* for all): ", uname, strnum, 5) == -1) return;
	    if (strnum[0] == '*') self->assignment = 0;
	    else self->assignment = atoi(strnum);
	    /* We don't currently care about author of handouts */
#if 0
	    if (message_AskForString(self, MESSAGE, "List papers matching author user name (* for all): ", (self->student)? self->student : "*", uname, 32) == -1) return;
	    if (self->student != NULL) free (self->student);
	    if (uname[0] == '*') self->student = NULL;
	    else {
		self->student = malloc (strlen (uname) + 1);
		strcpy (self->student, uname);
	    }
#endif
	    break;
	case papersExchange:
	    if (message_AskForString(self, MESSAGE, "List papers matching author user name (* for all): ", (self->student)? self->student : "*", uname, 32) == -1) return;
	    if (self->student != NULL) free (self->student);
	    if (uname[0] == '*') self->student = NULL;
	    else {
		self->student = malloc (strlen (uname) + 1);
		strcpy (self->student, uname);
	    }
	    break;
	default:
	    break;
    }
}

void papers_SetAndList(self, rock)
struct papers *self;
long rock;
{
    papers_SetDefault(self);
    papers_ReadList(self, NULL);
}

static struct bind_Description paperBindings[]={
    {"papers-remove-paper",   NULL, 0, "Remove selected paper(s)~30",	   0,MENUS_instructor, papers_RemovePaper,  NULL},
    {"papers-set-default", NULL, 0, "List By ...~09", 0, MENUS_general, papers_SetAndList, "Set criteria for and list papers."},
    {"papers-list", NULL, 0, "Update list~10",
    0, MENUS_general, papers_ReadList, NULL},
    {"papers-print", NULL, 0, "Print selected paper(s)~15", 0, MENUS_general, Print, NULL},
    NULL
};


boolean papers__InitializeClass(classID)
struct classheader classID;
{
    papers_global_menus = menulist_New();
    bind_BindList(paperBindings, NULL, papers_global_menus, &papers_classinfo);

    deleted = style_New();
    marked  = style_New();
    style_SetFontSize(deleted, style_ConstantFontSize, 10);
    style_AddNewFontFace(deleted, fontdesc_Italic);
    style_AddNewFontFace(marked, fontdesc_Bold);
    clockcursor = cursor_Create(NULL);
    cursor_SetStandard(clockcursor, Cursor_Wait);

    return TRUE;
}

void
SetSortOrder()
{
    char tmpstring[256];
    char *string;
    char *tok;
    int i,j;

    for (j = 0; j < 3; j++) {
	sprintf(tmpstring, "%s%s", papertypes[j][0], SortPreference);
#ifdef DEBUG_SORT
printf("Reading preferences for %s\n", tmpstring); fflush(stdout);
#endif
	string = environ_GetProfile(tmpstring);
	if (!string || *string == '\0') {
	    strcpy(tmpstring, papertypes[j][1]);
	    string = tmpstring;
	}

	i = 0;
	tok = strtok(string, ", ");
	do {
#ifdef DEBUG_SORT
	    printf("Setting sort preference to %s\n", tok); fflush(stdout);
#endif
	    if (strcmp(tok, "author") == 0)
		comparison[j][i] = SortByAuthor;
	    else if (strcmp(tok, "filename") == 0)
		comparison[j][i] = SortByFilename;
	    else if (strcmp(tok, "number") == 0)
		comparison[j][i] = SortByNumber;
	    else if (strcmp(tok, "description") == 0)
		comparison[j][i] = SortByDesc;
	    else if (strcmp(tok, "creation") == 0)
		comparison[j][i] = SortByCreate;
	    i++;
	} while ((tok = strtok(NULL,", ")) != NULL && i < SortLimit);
	comparison[j][i] = SortNoMore;
    }

}

void papers__Update(self)
register struct papers *self;
{
    papers_EraseVisualRect(self);
    papers_FullUpdate(self, view_FullRedraw, papers_GetLogicalTop(self), papers_GetLogicalLeft(self), papers_GetLogicalWidth(self), papers_GetLogicalHeight(self));
}

/* The routines ToggleMark, SimulateClick and papers__Hit were modified from
  messages/lib/captions.c */
void ToggleMark(self, node)
struct papers *self;
struct paperPositions *node;
/* Toggles whether the paper described by 'node' is marked or not.
  the text display is modified accordingly
  */
{
    if (node->flags & eos_PaperMarked) {
	node->flags &= ~eos_PaperMarked;
	environment_Delete(node->environment);
	text_RegionModified(self->textobj, node->textpos, node->textlength);
	(self->markcount)--;
    } else {
	node->flags |= eos_PaperMarked;
	node->environment = environment_New();
	node->environment = environment_InsertStyle(self->textobj->rootEnvironment, node->textpos, marked, TRUE);
	environment_SetLength(node->environment, node->textlength);
	text_RegionModified(self->textobj, node->textpos, node->textlength);
	(self->markcount)++;
    }
    papers_WantUpdate(self, self->textv);
}

void RemoveMarks(self, rock)
struct papers *self;
long rock;
/* Removes all marks. The text display is modified also */
{
    struct paperPositions *node;

    StartWaitCursor();
    for (node = self->Positions; node != NULL;node = node->next)
	if (node->flags & eos_PaperMarked) ToggleMark(self, node);
    StopWaitCursor();
}

void SimulateClick(self, IsLeftClick)
struct papers *self;
boolean IsLeftClick;
/* this code is based on code in the captions object, used
  in the Andrew messages application.
  */
{
    long start;
    struct paperPositions *node;

    papers_WantInputFocus(self, self->textv);
    start = textview_GetDotPosition(self->textv);
    node = eosfx_LocatePaper(self->Positions, start, NULL);
    textview_SetDotPosition(self->textv, node->textpos);
    textview_SetDotLength(self->textv, 0);
    if (IsLeftClick) {
	RemoveMarks(self, 0L);
	if (!(node->flags & eos_PaperDeleted)) ToggleMark(self, node);
    } else
	if (!(node->flags & eos_PaperDeleted)) ToggleMark(self, node);
}

void papers__PostMenus(self, menulist)
struct papers *self;
struct menulist *menulist;
/* We want our menus, but no others! */
{
    menulist_SetMask(self->menus, self->menuflags);
    super_PostMenus(self,self->menus); 
}

void papers__ReceiveInputFocus(self)
struct papers *self;
{
    /* We always want the textview to be the star attraction, so that
      it has its cursor visible (almost all of the time)
      */
    papers_WantInputFocus(self, self->textv);
}
