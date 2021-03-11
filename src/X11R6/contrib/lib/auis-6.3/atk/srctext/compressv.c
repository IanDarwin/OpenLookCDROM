/* File compressv.c created by R S Kemmetmueller

   compressv: a view to display a box where the hidden text lies. */
/* Copyright 1992, 1994 Carnegie Mellon University and IBM. All rights reserved.
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/compressv.c,v 1.5 1994/02/22 20:14:18 rr2b Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <view.ih>
#include <rect.h>
#include <fontdesc.ih>
#include <cursor.ih>
#include <environ.ih>
#include <textv.ih>
#include <text.ih>

#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <proctbl.ih>
#include <bind.ih>
#include <message.ih>

#include "compress.ih"
#include "compressv.eh"

static struct fontdesc *boxfont;
static char *boxfgcolor=NULL, *boxbgcolor=NULL;
static int boxwidth=0;
static struct keymap *c_Map;
static struct menulist *c_Menus;

void compressLines(), decompressLines();
void compressRegion(), decompressAll();

static struct bind_Description compressBindings[]={
    {"compressv-compress-lines",NULL,0, NULL,0,0, compressLines, "Compresses lines in the specified range (or selected region) into a box; will prompt if none specified."},
    {"compressv-decompress-lines",NULL,0, NULL,0,0, decompressLines, "Takes any lines within the specified range (or selected region) out of the box they were in."},
    {"compressv-compress-region",NULL,0, NULL,0,0, compressRegion, "Blindly compresses the selected region, doesn't know how to combine compresses or grab entire lines, but useful if you WANT to nest compresses."},
    {"compressv-decompress-all",NULL,0, NULL,0,0, decompressAll, "Expand all compressed regions in document to normal size."},
    NULL
};

void compressv__GetOrigin(self, width, height, originX, originY)
struct compressv *self;
long width;
long height;
long *originX;
long *originY;
{
    *originX = 0;
    *originY = height;
    return;
}

/* compressv_BoxText returns the string that should appear in the box that gets drawn. It points to static storage, so either use the return value immediately, or make a copy of it. */
char *compressv__BoxText(self)
struct compressv *self;
{
    static char boxstr[256];
    struct compress *cmprs=(struct compress *)compressv_GetDataObject(self);
    int numlines=compress_GetLines(cmprs);
    sprintf(boxstr, "%1d compressed line%s", numlines,(numlines!=1)?"s":"");
    return boxstr;
}

void compressv__Print(self, f, process, final, toplevel)
struct compressv *self;
FILE *f;
char *process;
char *final;
int toplevel;
{
    /* note: the placement of the troff font size changes, \s-2 and \s+2, is not as haphazard as they may seem. If the -2 size includes BOTH spaces, the left edge of the box gets disconnected. If the -2 size includes NEITHER space, the top and bottom edges extend too far left. */
    fprintf(f, "\\(br \\s-2%s \\s+2\\(br\\l'|0\\(rn'\\l'|0\\(ul'", compressv_BoxText(self));
}

struct view *compressv__Hit(self,action, mousex,mousey, numberOfClicks)
struct compressv *self;
enum view_MouseAction action;
long mousex, mousey, numberOfClicks;
{
    struct text *txt=(struct text *)self->parenttext;
    struct compress *fn=(struct compress *)compressv_GetDataObject(self);
    if (action==view_LeftUp || action==view_RightUp) {
	long mod=text_GetModified(txt);
	compress_DecompressBox(fn,txt);
	text_RestoreModified(txt,mod);
	text_NotifyObservers(txt,0);
	if (action==view_LeftUp) {
	    /* put cursor around decompressed text for easy re-compression */
	    textview_SetDotPosition(self->parentview, fn->loc);
	    textview_SetDotLength(self->parentview, compress_GetLength(fn));
	}
    }
    return (struct view *)self;
}

enum view_DSattributes compressv__DesiredSize(self, width, height, pass, desiredwidth, desiredheight)
struct compressv *self;
long width, height;
enum view_DSpass pass;
long *desiredwidth, *desiredheight;
{
    fontdesc_StringSize(boxfont, compressv_GetDrawable(self), compressv_BoxText(self), desiredwidth, desiredheight);
    *desiredwidth= (boxwidth>0) ? boxwidth : *desiredwidth+8; 
    *desiredheight= fontdesc_FontSummary(boxfont, compressv_GetDrawable(self)) -> maxHeight;
    return(view_HeightFlexible | view_WidthFlexible);
}

static DoUpdate(self,full)
struct compressv *self;
boolean full;
{
    struct rectangle enclosingRect;
    enclosingRect.top = 0; enclosingRect.left = 0;
    enclosingRect.width  = compressv_GetLogicalWidth(self);
    enclosingRect.height = compressv_GetLogicalHeight(self);
    compressv_PostCursor(self,&(enclosingRect),self->cursor);
    compressv_EraseRect(self,&(enclosingRect));
    enclosingRect.width--; enclosingRect.height--;
    if (!boxbgcolor)
	compressv_DrawRect(self,&(enclosingRect));

    /* draw string in center of box */
    compressv_MoveTo(self, (enclosingRect.width+1)/2, (enclosingRect.height-1)/2);
    compressv_DrawString(self,compressv_BoxText(self),(view_BETWEENTOPANDBOTTOM | view_BETWEENLEFTANDRIGHT));
}

void compressv__LinkTree(self, parent)
register struct compressv *self;
struct view *parent;
{
    struct compress *fn=(struct compress *)compressv_GetDataObject(self);
    super_LinkTree(self,parent);
    while (!class_IsTypeByName(class_GetTypeName(parent), "textview"))
	if ((parent=parent->parent) == NULL) return;
    self->parenttext= (struct text *)view_GetDataObject(parent);
    self->parentview= (struct textview *)parent;
    if (compressv_GetDrawable(self) && compressv_GetIM(self)) { /* make sure the graphic & im exist */
	compressv_SetFont(self, boxfont);
	if (boxbgcolor) compressv_SetBackgroundColor(self, boxbgcolor, 0,0,0);
	if (boxfgcolor) compressv_SetForegroundColor(self, boxfgcolor, 0,0,0);
    }
    if (fn) /* make sure we HAVE a dataobject */
	compress_SetLines(fn, compress_GetLineForPos(fn, compress_GetLength(fn))); /* I wanted to do this in compress_Compress, but the value didn't get copied when the compress box got copied. */
}

void compressv__FullUpdate(self,type,left,top,width,height)
struct compressv *self;
enum view_UpdateType type;
long left,top,width,height;
{
    DoUpdate(self,TRUE);
}

void compressv__Update(self)
struct compressv *self;
{
    DoUpdate(self,FALSE);
}

void compressv__ReceiveInputFocus(self)
struct compressv *self;
{
    super_ReceiveInputFocus(self);
    if(self->parentview){	
	/* this should only happen when the view is created */
	textview_SetDotPosition(self->parentview,textview_GetDotPosition(self->parentview) + 1);
	textview_WantInputFocus(self->parentview,self->parentview);
    }
}

/* blow ourselves away if our compress got decompressed */
void compressv__ObservedChanged(self, changed, value)
struct compressv *self;
struct observable *changed;
long value;
{
    boolean destroy=FALSE;
    /* check this BEFORE we call super_, because super_ will set our dataobject to NULL if it IS destroyed. */
    if (changed==(struct observable *)compressv_GetDataObject(self) && value==observable_OBJECTDESTROYED)
	destroy= TRUE;
    super_ObservedChanged(self,changed,value);
    if (destroy)
	compressv_Destroy(self);
}

boolean compressv__InitializeObject(classID,self)
struct classheader *classID;
struct compressv *self;
{
    self->cursor= cursor_Create(self);
    cursor_SetStandard(self->cursor,Cursor_CrossHairs);
    self->parenttext= NULL;
    self->parentview= NULL;
    return TRUE;
}

void compressv__FinalizeObject(classID,self)
struct classheader *classID;
struct compressv *self;
{
    if (self->cursor!=NULL) cursor_Destroy(self->cursor);
}

boolean compressv__InitializeClass(classID)
struct classheader *classID;
{
    char family[256];
    long style=0, size=8;    
    char *fontname=environ_GetProfile("CompressBoxFont");
    boxwidth= environ_GetProfileInt("CompressBoxWidth", 0);
    boxfgcolor= environ_GetProfile("CompressBoxForegroundColor");
    boxbgcolor= environ_GetProfile("CompressBoxBackgroundColor");
    if (!fontname || !*fontname || !fontdesc_ExplodeFontName(fontname, family,sizeof(family), &style,&size)) {
	/* either fontname is NULL or empty, or the name is invalid */
	if (fontname && *fontname)
	    fprintf(stderr, "Invalid font specified in CompressBoxFont preference.\n");
	strcpy(family, "andysans");
	style= 0;
	size= 8;
    }
    boxfont= fontdesc_Create(family, style, size);
    c_Menus = menulist_New();
    c_Map = keymap_New();
    bind_BindList(compressBindings,c_Map,c_Menus,class_Load("textview"));
    return TRUE;
}

/* engulfBoxes snags any compress insets that are butted up against the ends of the specified region, and includes them in the region */
void engulfBoxes(txt, ppos,plen)
struct text *txt;
long *ppos,*plen;
{
    while (compress_IsThere(txt,*ppos-1) || (text_GetChar(txt,*ppos-1)=='\n' && compress_IsThere(txt,*ppos-2))) {
	--*ppos;
	++*plen;
    }
    while (compress_IsThere(txt,*ppos+*plen) || (text_GetChar(txt,*ppos+*plen)=='\n' && compress_IsThere(txt,*ppos+*plen+1)))
	++*plen;
}

boolean getLinesFromUser(self, rString,prompt, pfirst,plast)
struct textview *self;
char *rString,*prompt;
long *pfirst,*plast;
{
    struct text *txt=(struct text *)textview_GetDataObject(self);
    char range[32];
    int numbers;
    *range= '\0';
    if (!rString || !*rString || (unsigned long)rString<256) { /* none specified */
	long pos=textview_GetDotPosition(self), len=textview_GetDotLength(self);
	if (len>0)
	    sprintf(range, "%ld-%ld", text_GetLineForPos(txt,pos), text_GetLineForPos(txt,pos+len));
	else if (message_AskForString(self, 0, prompt, NULL, range, sizeof(range)) < 0)
	    return FALSE;
    } else
	strcpy(range, rString);
    if (!*range) return FALSE;
    numbers= sscanf(range, " %ld%*[^0123456789]%ld ", pfirst,plast);
    if (numbers<1) return FALSE;
    if (numbers==1) *plast= *pfirst;
    return TRUE;
}

void compressLines(self, rString)
struct textview *self;
char *rString;
{
    struct text *txt=(struct text *)textview_GetDataObject(self);
    long mod=text_GetModified(txt);
    long pos,len, first,last;
    if (!getLinesFromUser(self, rString,"First,last line to compress: ", &first,&last))
	return;
    pos= text_GetPosForLine(txt,first);
    len= text_GetPosForLine(txt,last+1) - pos;

    /* make sure we aren't just trying to re-compress a box */
    if (len>1 || !compress_IsThere(txt,pos)) {
	engulfBoxes(txt,&pos,&len);
	last= text_GetLineForPos(txt,pos+len-1); /* find last engulfed line */
	compress_DecompressRange(txt,pos,len);
	len= text_GetPosForLine(txt,last+1) - pos; /* recalculate length */
	compress_Compress(txt,pos,len);
    }
    text_RestoreModified(txt,mod);
    text_NotifyObservers(txt,0);
}

void decompressLines(self, rString)
struct textview *self;
char *rString;
{
    struct text *txt=(struct text *)textview_GetDataObject(self);
    long mod=text_GetModified(txt);
    long pos,len, first,last;
    struct compress *cprs;
    if (!getLinesFromUser(self, rString,"First,last line to decompress: ", &first,&last))
	return;
    pos= text_GetPosForLine(txt,first);
    len= text_GetPosForLine(txt,last+1) - pos;

    /* check for partial decompression at end of region */
    if (len>0 && (cprs=compress_IsThere(txt,pos+len-1)) != NULL) {
	long firstline=text_GetLineForPos(txt,pos+len-1);
	long extractlen=compress_GetPosForLine(cprs, last-firstline+2);
	compress_PartialDecompress(cprs,txt, 0,extractlen);
    }
    /* check for partial decompression at beginning of region (this may make our pos and len values invalid) */
    if (len>0 && (cprs=compress_IsThere(txt,pos-1)) != NULL) {
	long firstline=text_GetLineForPos(txt,pos-1);
	long extractpos=compress_GetPosForLine(cprs, first-firstline+1) -1 /* grab newline too */;
	compress_PartialDecompress(cprs,txt, extractpos, compress_GetLength(cprs)-extractpos);
    }
    if (len>0) {
	/* recalculate pos and len */
	pos= text_GetPosForLine(txt,first);
	len= text_GetPosForLine(txt,last+1) - pos -1 /* omit newline */;
	compress_DecompressRange(txt,pos,len);
    } else if ((cprs=compress_IsThere(txt,pos-1)) != NULL) {
	/* length is zero, so entire range must be INSIDE a single box */
	long firstline=text_GetLineForPos(txt,pos-1);
	long extractpos=compress_GetPosForLine(cprs, first-firstline+1) -1 /* grab newline too */;
	long extractlen=compress_GetPosForLine(cprs, last-firstline+2) -extractpos;
	compress_PartialDecompress(cprs,txt, extractpos,extractlen);
    }
    text_RestoreModified(txt,mod);
    text_NotifyObservers(txt,0);
}

void compressRegion(self, rock)
struct textview *self;
long rock;
{
    struct text *txt=(struct text *)textview_GetDataObject(self);
    long mod=text_GetModified(txt);
    long pos=textview_GetDotPosition(self), len=textview_GetDotLength(self);
    if (len>0) {
	compress_Compress(txt,pos,len);
	text_RestoreModified(txt,mod);
	text_NotifyObservers(txt,0);
    }
}

void decompressAll(self, rock)
struct textview *self;
long rock;
{
    struct text *txt=(struct text *)textview_GetDataObject(self);
    long mod=text_GetModified(txt);
    compress_DecompressAll(txt);
    text_RestoreModified(txt,mod);
    text_NotifyObservers(txt,0);
}
