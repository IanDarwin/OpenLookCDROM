/* Copyright 1992 Carnegie Mellon University All rights reserved.
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

#include <andrewos.h>
#include <class.h>

#include <pintv.ih>
#include <im.ih>
#include <event.ih>
#include <buffer.ih>
#include <text.ih>
#include <frame.ih>
#include <textv.ih>
#include <cursor.ih>
#include <prefs.ih>
#include <complete.ih>

#define TEXT(tv) ((struct text *)textview_GetDataObject(tv))
#define PREFS(pv) ((struct prefs *)pintv_GetDataObject(pv))
#define RFOLDEDEQ(x,y) ((x)==(y))


static char explanation[]="In the following cases a general preference of the form \"*.prefname:...\" or \"prefname:...\" was found to precede a preference of the form \"appname.prefname:...\"\nIn cases like this the general preference overrides the specific preference, which probably isn't what was intended.  If you save your preferences they will be re-ordered so that this conflict does not occur.  This may cause a CHANGE in behavior.\n\n";


static void ReportErrors(self, curtime)
struct pintv *self;
long curtime;
{
    struct buffer *b;
    struct frame *f;
    char bufname[256];

    self->reportevent=NULL;
    
    if(self->errors==NULL) {
	fprintf(stderr, "Preferences Warning: errors object disappeared before errors could be reported.\n");
	return;
    }
    buffer_GetUniqueBufferName("Preferences Errors", bufname, sizeof(bufname));
    text_AlwaysInsertCharacters(self->errors, 0, explanation, sizeof(explanation)-1);
    b=buffer_Create(bufname, NULL, NULL, self->errors);
    if(b==NULL) {
	fprintf(stderr, "Preferences Warning: error buffer could not be created.\n");
	return;
    }
    buffer_SetDestroyData(b, TRUE);
    text_RemoveObserver(self->errors, self);
    self->errors=NULL;
    f=frame_GetFrameInWindowForBuffer(b);
    if(f==NULL) {
	fprintf(stderr, "Preferences Warning: couldn't get a window for the error buffer %s\n", bufname);
	fprintf(stderr, "Use switch buffer to examine the error list.\n");
    }
    textview_WantInputFocus((struct textview *)frame_GetView(f), (struct textview *)frame_GetView(f));
}

struct event *pintv_GetReportEvent(rock)
long rock;
{
    return im_EnqueueEvent(ReportErrors, rock, 0);
}


static void KeepOldCopy(self, curtime)
struct pintv *self;
long curtime;
{
    struct prefs *prefs=(struct prefs *)pintv_GetDataObject(self);
    char buf[1024];
    char error[1500];
    struct cursor *busy, *old=NULL;
    int close_err=0;
    FILE *fp=NULL;
    self->oevent=NULL;
    old=im_GetProcessCursor();
    message_DisplayString(self, 100, "Make sure you have a backup copy of your preferences.\nYou may not like how the preferences\n editor re-formats them.\n The preferences editor is still experimental,\nit may corrupt your preferences under some situations.\nIf this happens please let us know.");
    strcpy(buf, "~/keep.prf");
    while(1) {
	if (completion_GetFilename(self, "File to save old preferences to:", buf, buf, sizeof(buf), FALSE, FALSE) == -1) {
	    errno = 0;
	    message_DisplayString(self, 100, "Cancelled.  If you save you will overwrite your old preferences.");
	    break;
	}
	busy=cursor_Create(self);
	if(busy) {
	    cursor_SetStandard(busy, Cursor_Wait);
	    im_SetProcessCursor(busy);
	}
	fp=fopen(buf, "w");
	if(fp==NULL) {
	    sprintf(error, "Couldn't open file %s for writing.", buf);
	    message_DisplayString(self, 100, error);
	    im_SetProcessCursor(old);
	    continue;
	}	    
	errno=0;
	prefs_WritePlain(prefs, fp, im_GetWriteID(), 0);
#ifdef AFS_ENV
	if(ferror(fp)) close_err=(-1);
	else close_err=vclose(fileno(fp));
	fclose(fp);
	if(close_err!=0)
#else
	if((fclose(fp))!=0)
#endif
	{
	    sprintf(error, "Failed to save old preferences in %s.", buf);
	    message_DisplayString(self, 100, error);
	    im_SetProcessCursor(old);
	    continue;
	} else {
	    sprintf(error, "Saved old preferences in %s.", buf);
	    message_DisplayString(self, 0, error);
	    break;
	}
    }
    prefs_UpdateText(prefs);
    im_SetProcessCursor(old);
}

struct event *pintv_GetKeepEvent(rock)
long rock;
{
    return im_EnqueueEvent(KeepOldCopy, rock, 0);
}

static boolean FindTextBuffer(b, data)
struct buffer *b;
struct prefs *data;
{
    if(buffer_GetData(b)==(struct dataobject *)data && buffer_GetDefaultViewname(b) && strcmp(buffer_GetDefaultViewname(b), "textview")==0) return TRUE;
    else return FALSE;
}

void pintv_EditAsText(self, sb, rock)
struct pintv *self;
struct sbutton *sb;
long rock;
{
    char bufname[1024];
    struct buffer *b;
    struct frame *f;
    struct prefs *prefs=PREFS(self);
    prefs_UpdateText(prefs);
    b=buffer_Enumerate(FindTextBuffer, prefs);
    if(b==NULL) {
	buffer_GetUniqueBufferName("PrefsText", bufname, sizeof(bufname));
	b=buffer_Create(bufname, NULL, NULL, prefs);
	if(b==NULL) {
	    fprintf(stderr, "Preferences Warning: text buffer could not be created.\n");
	    return;
	}
	buffer_SetDefaultViewname(b, "textview");
    }
    f=frame_GetFrameInWindowForBuffer(b);
    if(f==NULL) {
	fprintf(stderr, "Preferences Warning: couldn't get a window for the error buffer %s\n", bufname);
	fprintf(stderr, "Use switch buffer to examine the error list.\n");
    }
    textview_WantInputFocus((struct textview *)frame_GetView(f), (struct textview *)frame_GetView(f));
}
