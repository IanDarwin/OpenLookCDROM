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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/text822v.c,v 1.21 1992/12/15 21:48:03 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <sys/param.h>
#include <cui.h>
#include <fdphack.h>
#include <class.h>
#include <amsutil.ih>
#include <text.ih>
#include <environ.ih>
#include <envrment.ih>
#include <captions.ih>
#include <im.ih>
#include <folders.ih>
#include <proctbl.ih>
#include <menulist.ih>
#include <keystate.ih>
#include <keymap.ih>
#include <message.ih>
#include <bind.ih>
#include <style.ih>
#include <scroll.ih>
#include <text822v.eh>
#define dontDefineRoutinesFor_sendmessage
#include <sendmsg.ih>
#undef dontDefineRoutinesFor_sendmessage
#include <ams.ih>

#define Text(self) ((struct text *) t822view_GetDataObject(self))

static struct keymap *t822view_standardkeymap;
static struct menulist *t822view_standardmenulist;

void BodiesCompound(self, cmds)
struct t822view *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), self, "t822view", cmds);
}

void BodiesTextviewCommand(self, cmds)
struct t822view *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), self, "textview", cmds);
}

void BodiesCaptionsCommand(self, cmds)
struct t822view *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), t822view_GetCaptions(self), "captions", cmds);
}

static void DownFocus(self)
struct t822view *self;
{
    struct folders *f = captions_GetFolders(t822view_GetCaptions(self));

    if (f->sm) {
	ams_Focus(f->sm->HeadTextview);
    } else {
	ams_Focus(f);
    }
}

static void UpFocus(self)
struct t822view *self;
{
    ams_Focus(t822view_GetCaptions(self));
}

static struct bind_Description t822view_standardbindings [] = {
    /* procname, keysequence, key rock, menu string, menu rock, proc, docstring, dynamic autoload */
    {"bodies-down-focus", "\030n", NULL, NULL, NULL, 0, DownFocus, "Move input focus to folders or sendmessage"},
    {"bodies-up-focus", "\030p", NULL, NULL, NULL, 0, UpFocus, "Move input focus to captions"},
    {"bodies-down-focus", "\030\016", NULL, NULL, NULL, 0, DownFocus, "Move input focus to folders or sendmessage"},
    {"bodies-up-focus", "\030\020", NULL, NULL, NULL, 0, UpFocus, "Move input focus to captions"},
    {"bodies-compound-operation", NULL, NULL, NULL, NULL, 0, BodiesCompound, "Execute a compound bodies operation"},
    {"bodies-textview-compound", NULL, NULL, NULL, NULL, 0, BodiesTextviewCommand, "Execute a compound 'textview' operation on the bodies"},
    {"bodies-captions-compound", NULL, NULL, NULL, NULL, 0, BodiesCaptionsCommand, "Execute a compound 'folders' operation."},
    {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL}
};

static struct style *InstructionsStyle = NULL;

boolean t822view__InitializeClass(c)
struct classheader *c;
{
    t822view_standardkeymap = keymap_New();
    t822view_standardmenulist = menulist_New();
    bind_BindList(t822view_standardbindings, t822view_standardkeymap, t822view_standardmenulist, &t822view_classinfo);
    InstructionsStyle = style_New();
    style_SetFontSize(InstructionsStyle, style_ConstantFontSize, 16);
    style_SetJustification(InstructionsStyle, style_LeftJustified);
    style_SetFontFamily(InstructionsStyle, amsutil_GetDefaultFontName());
    return(TRUE);
}

boolean t822view__InitializeObject(c, self)
struct classheader *c;
struct t822view *self;
{
    t822view_SetWhatIAm(self, WHATIAM_BODIES);
    ams_AddCheckpointBodies(self);
    self->myks = keystate_Create(self, t822view_standardkeymap);
    self->myml = menulist_DuplicateML(t822view_standardmenulist, self);
    self->mycaps = NULL;
    self->PriorReadOnliness = FALSE;
    return(TRUE);
}

void t822view__SetCaptions(self, cap)
struct t822view *self;
struct captions *cap;
{
    self->mycaps = cap;
}

void t822view__ShowHelp(self, text)
struct t822view *self;
char *text;
{
    int len;
    static char *InitialHelpString = "To see the captions for messages in a folder, click with the left mouse button on the folder name.\n\nTo see a message, click on its caption in the middle region of the window.\n\nTo perform actions on a folder, or get information about it, click on the help icon next to its name.";
    char *SurrogateHelpFile, *s;
    char ErrMsg[500+MAXPATHLEN];
    struct environment *et;
    extern int errno;
    struct text *t = Text(self);
    struct im *im;

    t822view_SetDotPosition(self, 0);
    t822view_SetTopPosition(self, 0);
    s = (char *) environ_GetProfile("messages.surrogatehelpfile");
    if (s) {
	if(SurrogateHelpFile = malloc(1+strlen(s)))
	    strcpy(SurrogateHelpFile, s);
	else s = SurrogateHelpFile;
    } else SurrogateHelpFile = s;
    text_ClearCompletely(t);
    im = t822view_GetIM(self);
    if (im && im_GetInputFocus(im) == NULL) t822view_WantInputFocus(self, self);
    if (!text) {
	if (SurrogateHelpFile && *SurrogateHelpFile) {
	    FILE *fp;
	    char FName[1+MAXPATHLEN];

	    ams_TildeResolve(ams_GetAMS(), SurrogateHelpFile, FName);
	    fp = fopen(FName, "r");
	    if (!fp) {
		sprintf(ErrMsg, "Cannot read surrogate help file %s (%d).", FName, errno);
		text = ErrMsg;
	    } else {
		text_AlwaysInsertFile(t, fp, FName, 0);
		fclose(fp);
	    }
	} else {
	    text = InitialHelpString;
	}
    }
    if (text) {
	len = strlen(text);
	text_AlwaysInsertCharacters(t, 0, text, len);
	et = environment_InsertStyle(t->rootEnvironment, 0, InstructionsStyle, 1);
	environment_SetLength(et, len);
    }
    t822view_WantUpdate(self, self);
}


/* we override ObservedChanged so we can be sure the keystate is posted
	when there is a change in the read-onliness */
void t822view__ObservedChanged(self, changed, value)
    struct t822view *self;
    struct observable *changed;
    long value;
{
    struct text *text = Text(self);
    boolean RO;
    super_ObservedChanged(self, changed, value);
    if (value == observable_OBJECTDESTROYED)  
	return;
    RO = text_GetReadOnly(text);
    if (self->PriorReadOnliness != RO
         && ((struct textview *) self)->hasInputFocus) {
	t822view_WantInputFocus(self, self);	/* post key state */
	self->PriorReadOnliness = RO;
    }
}



void t822view__PostMenus(self, ml)
struct t822view *self;
struct menulist *ml;
{
    menulist_ClearChain(self->myml);
    if (ml) menulist_ChainAfterML(self->myml, ml, ml);
    super_PostMenus(self, self->myml);
}

void t822view__PostKeyState(self, ks)
struct t822view *self;
struct keystate *ks;
{
    self->myks->next = NULL;
    if (amsutil_GetOptBit(EXP_KEYSTROKES) 
		&& text_GetReadOnly(Text(self))) {
	if (ks) keystate_AddAfter(ks, self->myks);
	super_PostKeyState(self, self->myks);
    } else {
	super_PostKeyState(self, ks);
    }
}

struct view *t822view__GetApplicationLayer(self)
struct t822view *self;
{
     return (struct view *) scroll_Create(self, scroll_LEFT);
}

void t822view__DeleteApplicationLayer(self, scrollbar)
struct t822view *self;
struct scroll *scrollbar;
{
    ams_RemoveCheckpointBodies(self);
    scroll_Destroy(scrollbar);
}

void t822view__SetDataObject(self, dat)
struct t822view *self;
struct dataobject *dat;
{
    super_SetDataObject(self, dat);
    t822view_ShowHelp(self, NULL);
    self->PriorReadOnliness = text_GetReadOnly(Text(self));
}

struct captions *
t822view__NewCaptionsInNewWindow(self)
struct t822view *self;
{
    struct captions *cap = captions_New();

    t822view_SetCaptions(self, cap);
    captions_SetBodies(cap, self);
    cap->myframe = ams_InstallInNewWindow(captions_GetApplicationLayer(cap), "messages-captions", "Message Captions", environ_GetProfileInt("captions.width", 600), environ_GetProfileInt("captions.height", 250), cap);
    return(cap);
}

void t822view__FinalizeObject(c, self)
struct classheader *c;
struct t822view *self;
{
    ams_RemoveCheckpointBodies(self);
    if (self->mycaps) {
	captions_SetBodies(self->mycaps, NULL);
    }
    if (self->myks) keystate_Destroy(self->myks);
    if (self->myml) menulist_Destroy(self->myml);
}

struct captions *t822view__GetCaptions(self)
struct t822view *self;
{
    if (!self->mycaps) {
	t822view_NewCaptionsInNewWindow(self);
    }
    return(self->mycaps);
}
