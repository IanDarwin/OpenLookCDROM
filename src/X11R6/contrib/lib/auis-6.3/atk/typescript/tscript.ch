/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 


#define READONLYMAX 65

/* Typescript menu masks. */
#define typescript_SelectionMenus 1   /* Menus to be posted when there is a selection region. */
#define typescript_NoSelectionMenus 2 /* Menus to be posted only when there is no
                                     * selection. Handles the bogus case of the
                                     * paste item which should show up all the
                                     * time.
                                     */
#define typescript_AfterFenceMenus 4 /* Menus to be posted for writable textviews. */

class typescript[tscript] : textview[textv] {
overrides:
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    Update();
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    ReceiveInputFocus();
    ObservedChanged (struct thisobject *changed, long value);
    PostMenus(struct menulist *menulist);
    GetClickPosition(long position, long numberOfClicks, enum view_MouseAction action, long startLeft, long startRight, long *leftPos, long *rightPos);
    SetDataObject(struct dataobject *dataObject);

methods:
    SetTitle(char *title);
    GetTitle() returns char *;
    SetFrame(struct frame *f);
    GetFrame() returns struct frame *;
classprocedures:
    InitializeObject(struct typescript *self) returns boolean;
    Create(char **arglist,FILE *diskf,boolean filemenu) returns struct typescript *;
    CreatePipescript(FILE *indiskf, FILE *outdiskf, boolean filemenu) returns struct typescript *;
    InitializeClass() returns boolean;
    FinalizeObject(struct typescript *ap);
data:
    struct frame *frame;	/* First frame on the window (for setting the title) */
    struct mark *cmdStart;	/* last place a new command was started */
    long lastPosition;		/* used for deciding to frame the dot when reading from the process */
    FILE *SCFile;               /* Sub Channel file * for BE 2file handler. */
    short SubChannel;           /* File descriptor of sub process i/o channel. */
    int SlaveChannel;          /* File descriptor of slave side of the pty. */
    short readOnlyLen;		/* Number of charaters in read only buffer. -1 if not in read only mode. */
    char readOnlyBuf[READONLYMAX]; /* Place to gather characters when in read only (no echo) mode. */
    char OutputWait;
    char ChopLines;
    struct keystate *keystate;
    struct menulist *menulist;
    char *progname;
    boolean pipescript;
    int	pgrpid;                /* process group id for signaling in no job-control systems */
    char *ptyname;             /* Name of the slave side pty device. */
    char *title;
    struct text *cmdText;
    long lastCmdPos;
};
