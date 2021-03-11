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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/textobjects/RCS/diredv.c,v 1.10 1992/12/15 21:46:29 rr2b R6tape $";
#endif

#include <class.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <im.ih>
#include <view.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <proctbl.ih>
#include <envrment.ih>
#include <bind.ih>
#include <cursor.ih>
#include <message.ih>
#include <observe.ih>
#include <filetype.ih>
#include <search.ih>
#include <frame.ih>
#include <buffer.ih>

#include <dired.ih>
#include <diredv.eh>

extern int errno;
extern char *sys_errlist[];

#define Dired(self) \
    ((struct dired *) (self)->header.view.dataobject)

#define WAITON()    im_SetProcessCursor(waitCursor)
#define WAITOFF()   im_SetProcessCursor(NULL)

static struct keymap *diredKeymap;
static struct menulist *diredMenus;

static struct cursor *waitCursor;

#define mmask_None              0
#define mmask_ItemMarked        (1 << 0)
#define mmask_DotFilesOn        (1 << 1)
#define mmask_DotFilesOff       (1 << 2)
#define mmask_LongModeOn        (1 << 3)
#define mmask_LongModeOff       (1 << 4)

static void SetMenuMask(self)
struct diredview *self;
{
    struct dired *dired = Dired(self);
    int mmask = 0;

    if (dired_AnythingMarked(dired))
        mmask |= mmask_ItemMarked;

    if (dired_GetLongMode(dired))
        mmask |= mmask_LongModeOn;
    else
        mmask |= mmask_LongModeOff;

    if (dired_GetDotFiles(dired))
        mmask |= mmask_DotFilesOn;
    else
        mmask |= mmask_DotFilesOff;

    if (menulist_SetMask(self->menulist, mmask))
        super_PostMenus(self, self->menulist);
}

/*
 * GetFullName simplifies a filename relative to the
 * currently viewed directory.
 */

static char *GetFullName(self, filename)
struct diredview *self;
char *filename;
{
    static char fullName[256];
    char buf[256];
    sprintf(buf, "%s/%s", dired_GetDir(Dired(self)), filename);
    filetype_CanonicalizeFilename(fullName, buf, sizeof (fullName));
    return fullName;
}

/*
 * The `ptproc_' routines are the keyboard / menu handlers.
 */

static void ptproc_Refresh(self, rock)
struct diredview *self;
long rock;
{
    char *dir = dired_GetDir(Dired(self));
    WAITON();
    if (dired_SetDir(Dired(self), dir) < 0) {
        char buf[256];
        if (dir == NULL)
            sprintf(buf, "No directory specified.\n");
        else
            sprintf(buf, "Could not read: %s (%s)\n",
              dir, sys_errlist[errno]);
        message_DisplayString(self, 0, buf);
    }
    WAITOFF();
}

static int ZoomProc(filename, foundp)
char *filename, **foundp;
{
    if (*foundp == NULL) {
        *foundp = filename;
        return TRUE;    /* Keep searching in case others marked */
    } else
        return FALSE;   /* Oops, more than one marked */
}

/* Perform the excrutiating activities necessary to visit file */

static int VisitFile(self, fname)
struct diredview *self;
char *fname;
{
    register struct buffer *buffer;
    char realName[1000], buf[1000];
    boolean fileIsDir, fileExists;
    struct stat statBuf;
    struct frame *newFrame;
    struct im *window;

    filetype_CanonicalizeFilename(realName, fname, sizeof (realName) - 1);
    fname = realName;

    fileExists = fileIsDir = FALSE;
    if (stat(fname, &statBuf) >= 0) {
	fileExists = TRUE;
	if ((statBuf.st_mode & S_IFMT) == S_IFDIR)
	    fileIsDir = TRUE;
    }

    if (!fileExists)
      {
	sprintf (buf, "No such file %s.", fname);
	message_DisplayString (self, 0, buf);
	return;
      }
    buffer = buffer_GetBufferOnFile(fname, 0);
    if (buffer == NULL)
      {
	sprintf (buf, "Could not access file %s.\n", fname);
	message_DisplayString (self, 0, buf);
	return;
      }

    if((newFrame = frame_New()) == NULL) {
	fprintf(stderr,"dired: Could not allocate enough memory to visit file.\n");
	return(-1);
    }

    /* Note that frame menulist procs are bound in this procedure */
    frame_SetCommandEnable(newFrame, TRUE);

    if((window = im_Create(NULL)) == NULL) {
	fprintf(stderr,"dired: Could not create new window to view file.\n");
	if(newFrame) frame_Destroy(newFrame);
	return(-1);
    }
    im_SetView(window, newFrame);

    frame_PostDefaultHandler(newFrame, "message",
			     frame_WantHandler(newFrame, "message"));
    frame_SetBuffer(newFrame, buffer, TRUE);
}

static void ptproc_Zoom(self, rock)
struct diredview *self;
long rock;
{
    struct dired *dired = Dired(self);
    struct stat stbuf;
    char *fname, buf[256], tmpfname[256];

    if (! dired_AnythingMarked(dired)) {
        fname = dired_Locate(dired, diredview_GetDotPosition(self));
        if (fname != NULL)
            dired_Mark(dired, fname);
    }

    fname = NULL;
    if (dired_EnumerateMarked(dired, ZoomProc, &fname) != NULL) {
        message_DisplayString(self, 0,
          "Cannot zoom into more than one file.\n");
        return;
    }

    if (fname == NULL)
        return;

    /*
     * When changing into "..", try to do it by just
     * stripping the last component.  That way, dired
     * exhibits much more reasonable behavior with
     * symbolic links.
     */

    if (strcmp(fname, "..") == 0) {
        char *s;
        strcpy(tmpfname, dired_GetDir(dired));
        for (s = tmpfname; *s; s++) /* Go to end of path */
            ;
        fname = tmpfname;
        if (--s != tmpfname) {      /* Nothing if plain slash */
            if (*s == '/')          /* Zap trailing slash */
                *s-- = '\0';
            while (*s != '/') {     /* Remove trailing component */
                if (s > tmpfname)
                    *s-- = '\0';
                else {  /* Not referenced from root! */
                    fname = "..";
                    goto giveup;
                }
            }
            if (s > tmpfname)      /* Remove slash if not root */
                *s = '\0';
        }
    } else {
      giveup:
        fname = GetFullName(self, fname);
    }

    WAITON();

    if (stat(fname, &stbuf) < 0) {
        sprintf(buf, "Could not stat: %s (%s)\n", fname, sys_errlist[errno]);
        WAITOFF();
        message_DisplayString(self, 0, buf);
        return;
    }

    switch (stbuf.st_mode & S_IFMT) {
        case S_IFDIR:
/*
            sprintf(buf, "Reading directory: %s\n", fname);
            message_DisplayString(self, 0, buf);
            im_ForceUpdate();
            if (dired_SetDir(dired, fname) < 0)
                sprintf(buf, "Could not read: %s (%s)\n", fname, sys_errlist[errno]);
            else
                strcpy(buf, "Done.\n");
            message_DisplayString(self, 0, buf);
*/
            VisitFile (self, fname);
            break;
        case S_IFREG:
/*
            sprintf(buf, "Not implemented: Editing File: %s\n", fname);
            message_DisplayString(self, 0, buf);
            im_ForceUpdate();
*/
            VisitFile(self, fname);
            break;
        default:    /* S_IFLNK never occurs; other cases are devs. */
            sprintf(buf, "Cannot Read Special File: %s\n", fname);
            message_DisplayString(self, 0, buf);
            break;
    }

    WAITOFF();
}

static int DeleteProc(filename, self)
char *filename;
struct diredview *self;
{
    char buf[256], ques[256];

    sprintf(ques, "Delete %s [n]? ", filename);
    if (message_AskForString(self, 0, ques, NULL, buf, sizeof (buf)) < 0) {
        message_DisplayString(self, 0, "Cancelled.\n");
        return FALSE;
    }

    if (buf[0] == 'y' || buf[0] == 'Y') {
        sprintf(buf, "Deleting: %s\n", filename);
        message_DisplayString(self, 0, buf);
        im_ForceUpdate();
        WAITON();
        if (unlink(GetFullName(self, filename)) < 0) {
            sprintf(buf, "Cannot delete: %s (%s)\n", filename, sys_errlist[errno]);
            WAITOFF();
            message_DisplayString(self, 0, buf);
            return FALSE;
        }
        WAITOFF();
    }

    return TRUE;
}

static void ptproc_Delete(self, rock)
struct diredview *self;
long rock;
{
    struct dired *dired = Dired(self);

    if (! dired_AnythingMarked(dired)) {
        char *fname =
          dired_Locate(dired, diredview_GetDotPosition(self));
        if (fname != NULL)
            dired_Mark(dired, fname);
    }

    if (dired_EnumerateMarked(dired, DeleteProc, self) == NULL)
        message_DisplayString(self, 0, "Done.\n");

    ptproc_Refresh(self, 0);
}

static int RenameProc(filename, self)
char *filename;
struct diredview *self;
{
    char buf[256], ques[256], ans[256];

    sprintf(ques, "Rename %s to [don't rename] : ", filename);

    if (message_AskForString(self, 0, ques, NULL, ans, sizeof (ans)) < 0) {
        message_DisplayString(self, 0, "Cancelled.\n");
        return FALSE;
    }

    if (ans[0] != '\0') {
        sprintf(buf, "Renaming: %s\n", filename);
        message_DisplayString(self, 0, buf);
        im_ForceUpdate();
        WAITON();
        /* Have to copy the full name since it's a static buffer */
        strcpy(buf, GetFullName(self, filename));
        if (rename(buf, GetFullName(self, ans)) < 0) {
            sprintf(buf, "Cannot rename: %s (%s)\n", filename, sys_errlist[errno]);
            WAITOFF();
            message_DisplayString(self, 0, buf);
            return FALSE;
        }
        WAITOFF();
    }

    return TRUE;
}

static void ptproc_Rename(self, rock)
struct diredview *self;
long rock;
{
    struct dired *dired = Dired(self);

    if (! dired_AnythingMarked(dired)) {
        char *fname =
          dired_Locate(dired, diredview_GetDotPosition(self));
        if (fname != NULL)
            dired_Mark(dired, fname);
    }

    if (dired_EnumerateMarked(dired, RenameProc, self) == NULL)
        message_DisplayString(self, 0, "Done.\n");

    ptproc_Refresh(self, 0);
}

static void ptproc_DownLine(self, rock)
struct diredview *self;
long rock;
{
    /* (textview-next-line proc handles the arg count) */
    struct proctable_Entry *NextPE =
      proctable_Lookup("textview-next-line");
    if (NextPE != NULL && NextPE->proc != NULL)
        (*NextPE->proc)(self);
}

static void ptproc_UpLine(self, rock)
struct diredview *self;
long rock;
{
    /* (textview-previous-line proc handles the arg count) */
    struct proctable_Entry *NextPE =
      proctable_Lookup("textview-previous-line");
    if (NextPE != NULL && NextPE->proc != NULL)
        (*NextPE->proc)(self);
}

static void ptproc_ToggleSelect(self, rock)
struct diredview *self;
long rock;
{
    struct dired *dired = Dired(self);
    long count = im_Argument(self->header.view.imPtr);
    struct proctable_Entry *NextPE =
      proctable_Lookup("textview-next-line");

    im_ClearArg(self->header.view.imPtr);

    if (count > 10000)
        count = 10000;

    while (count--) {
        long pos = diredview_GetDotPosition(self);
        char *fname = dired_Locate(dired, pos);

        if (fname != NULL) {
            if (dired_IsMarked(dired, fname))
                dired_Unmark(dired, fname);
            else
                dired_Mark(dired, fname);
        }

        if (NextPE != NULL && NextPE->proc != NULL)
            (*NextPE->proc)(self);
    }
}

static void ptproc_RegexpSelect(self, rock)
struct diredview *self;
long rock;
{
    struct dired *dired = Dired(self);
    char buf[256], *res;
    static struct SearchPattern *pat = NULL;
    long pos;

    if (message_AskForString(self, 0,
      "Regular expression: ", ".*", buf, sizeof (buf)) < 0
       || buf[0] == '\0') {
        message_DisplayString(self, 0, "Cancelled.\n");
        return;
    }

    res = search_CompilePattern(buf, &pat);
    if (res != NULL) {
        message_DisplayString(self, 0, res);
        return;
    }

    WAITON();

    pos = 0;
    while (1) {
        struct stat stbuf;
        pos = search_MatchPattern(dired, pos, pat);
        if (pos < 0 || (res = dired_Locate(dired, pos)) == NULL)
            break;
        if (stat(GetFullName(self, res), &stbuf) >= 0 &&
          (stbuf.st_mode & S_IFMT) != S_IFDIR)
            dired_Mark(dired, res);
        while (++pos < dired_GetLength(dired))
            if (dired_GetChar(dired, pos) == '\n')
                break;
    }

    WAITOFF();
    message_DisplayString(self, 0, "Done.\n");
}

static void ptproc_ModeChange(self, change)
struct diredview *self;
long change;
{
    struct dired *dired = Dired(self);
    switch (change) {
        case 0: dired_SetLongMode(dired, TRUE); break;
        case 1: dired_SetLongMode(dired, FALSE); break;
        case 2: dired_SetDotFiles(dired, TRUE); break;
        case 3: dired_SetDotFiles(dired, FALSE); break;
    }
    ptproc_Refresh(self, 0);
}

/*
 * Class procedures
 */

static struct bind_Description diredBindings[] = {
    {   "dired-zoom-in", "z", 0,
        "Directory Edit,Zoom In~10",
         0, mmask_ItemMarked, ptproc_Zoom,
        "Edit selected file or switch to selected directory." },
    {   "dired-delete", "d", 0,
        "Directory Edit,Delete~20",
         0, mmask_ItemMarked, ptproc_Delete,
         "Delete selected file(s)." },
    {   "dired-rename", "r", 0,
        "Directory Edit,Rename~30",
         0, mmask_ItemMarked, ptproc_Rename,
         "Rename selected file(s)." },
    {   "dired-refresh", "\r", 0,
         "Directory Edit,Refresh~40",
         0, mmask_None, ptproc_Refresh,
         "Reread current directory." },
    {    "dired-toggle-select", " ", 0,
         NULL, 0, 0, ptproc_ToggleSelect,
         "Toggle file at cursor." },
    {    "dired-down-line", "n", 0,
         NULL, 0, 0, ptproc_DownLine,
         "Move cursor down (equiv ^N)." },
    {    "dired-up-line", "p", 0,
         NULL, 0, 0, ptproc_UpLine,
         "Move cursor up (equiv ^P)." },
    {    "dired-regexp-select", "=", 0,
         "Directory Edit,Regexp Select~60",
         0, mmask_None, ptproc_RegexpSelect,
         "Select files by regular expression." },
    {    "dired-long-mode-on", "l", 0,
         "Directory Edit,Long Format~50",
         0, mmask_LongModeOff, ptproc_ModeChange,
         "List files in long format." },
    {    "dired-long-mode-off", "s", 1,
         "Directory Edit,Short Format~50",
         1, mmask_LongModeOn, ptproc_ModeChange,
         "List files in normal format (names only)." },
    {    "dired-dot-files-on", "i", 2,
         "Directory Edit,Show Invisible~55",
         2, mmask_DotFilesOff, ptproc_ModeChange,
         "Display invisible files." },
    {    "dired-dot-files-off", "h", 3,
         "Directory Edit,Hide Invisible~55",
         3, mmask_DotFilesOn, ptproc_ModeChange,
         "Do not display invisible files." },
    NULL
};

boolean diredview__InitializeClass(classID)
struct classheader *classID;
{
    diredKeymap = keymap_New();
    diredMenus = menulist_Create(NULL);

    bind_BindList(diredBindings, diredKeymap, diredMenus, &diredview_classinfo);

    waitCursor = cursor_Create(NULL);
    cursor_SetStandard(waitCursor, Cursor_Wait);

    return TRUE;
}

boolean diredview__InitializeObject(classID, self)
struct classheader *classID;
struct diredview *self;
{
    self->keystate = keystate_Create(self, diredKeymap);
    self->menulist = menulist_DuplicateML(diredMenus, self);
    menulist_SetMask(self->menulist, mmask_None);
    return TRUE;
}

void diredview__FinalizeObject(classID, self)
struct classheader *classID;
struct diredview *self;
{
    if(self->menulist) menulist_Destroy(self->menulist);
}

/*
 * Overrides
 */

void diredview__SetDataObject(self, object)
struct diredview *self;
struct basicobject *object;
{
    struct dired *dired = (struct dired *) object;
    if (dired_GetDir(dired) == NULL)
        dired_SetDir(dired, ".");
    super_SetDataObject(self, object);
    dired_NotifyObservers(dired, 0);
}

struct view *diredview__Hit(self, action, x, y, numberOfClicks)
struct diredview *self;
enum view_MouseAction action;
long x, y, numberOfClicks;
{
    int button;

    switch (action) {
        default:
            button = 0;
            break;
        case view_LeftDown:
        case view_LeftMovement:
            button = 1;     /* Left button; select */
            break;
        case view_RightDown:
        case view_RightMovement:
            button = 2;     /* Right button; deselect */
            break;
    }

    if (button) {
        struct dired *dired = Dired(self);
        long pos = diredview_Locate(self, x, y, NULL);
        char *fname = dired_Locate(dired, pos);

        if (fname) {
            if (button == 1)
                dired_Mark(dired, fname);
            else
                dired_Unmark(dired, fname);

            while (pos > 0 &&
              dired_GetChar(dired, pos - 1) != '\n')
                pos--;
            diredview_SetDotPosition(self, pos);
            diredview_SetDotLength(self, 0);
        }
    }

    return (struct view *) self;
}

void diredview__PostKeyState(self, keystate)
struct diredview *self;
struct keystate *keystate;
{
    keystate_AddBefore(self->keystate, keystate);
    super_PostKeyState(self, self->keystate);
}

void diredview__PostMenus(self, menulist)
struct diredview *self;
struct menulist *menulist;
{
    menulist_ChainAfterML(self->menulist, menulist, 0);
    super_PostMenus(self, self->menulist);
}

void diredview__ObservedChanged(self, changed, value)
struct diredview *self;
struct observable *changed;
long value;
{
    SetMenuMask(self);
    super_ObservedChanged(self, changed, value);
}
