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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/txtvcmsc.c,v 1.9 1994/02/01 23:51:21 Zarf Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <txtvcmds.h>
#include <text.ih>
#include <im.ih>
#include <message.ih>
#include <viewref.ih>
#include <envrment.ih>
#include <style.ih>
#include <fnote.ih>
#include <buffer.ih>

#define AUXMODULE 1
#include <textv.eh>

#ifdef CONVERSIONERROR
void textview_PrintFile(self)
struct textview *self;
{
    register struct text *d = Text(self);
    register struct buffer *b = (struct buffer *) buffer_FindData(d);
    char *filename = b->fname;
    
    if (! filename || ! filename[0]) {
	filename = b->bname;
	if (! filename || ! filename[0])
	    filename = "unknown";
    }
    
    doctroff_PrintDoc(d, 1, 1, filename, "");
}

void textview_PreviewCmd(self)
register struct textview *self;
{
    register struct text *d = Text(self);
    register struct buffer *b = buffer_finddata(d);
    char *filename = b->fname;
    
    if (! filename || ! filename[0])  {
	filename = b->bname;
	if (! filename || ! filename[0])
	    filename = "unknown";
    }
    
    doctroff_PrintDoc(d, 0, 1, filename, "");
}

void textview_SetPrinterCmd(self)
struct textview *self;
{
    char p[200];
    char *currentprinter;
    char str[300];
    
    currentprinter = (char *) environ_Get("LPDEST");
    if (currentprinter == NULL)
  	currentprinter = (char *) environ_Get("PRINTER");
    if (currentprinter == NULL)
        currentprinter = environ_GetProfile("print.printer");
    if (currentprinter == NULL)
        currentprinter = environ_GetProfile("print.spooldir");
    if (message_AskForString(self, 0,
            "Set Printer to: ", currentprinter, p, 200) < 0) {
	if (p[0] == '\0') {
	    environ_Delete("LPDEST");
	    environ_Delete("PRINTER");
	    if (environ_GetProfile("print.printer"))
		strcpy(p, environ_GetProfile("print.printer"));
	    else
		strcpy(p, environ_GetProfile("print.spooldir"));
	}
	else {
	    environ_Put("LPDEST", p);
	    environ_Put("PRINTER", p);
	}
    }
    else
	strcpy(p, currentprinter);
    sprintf(str, "Printer now set to %s", p); 
    message_DisplayString(self, 0, str);
}
#endif /* CONVERSIONERROR */

void textview_ToggleViModeCmd(self)
struct textview *self;
{
    long lcstate = im_GetLastCmd(textview_GetIM(self));

    textview_ToggleVIMode(self);
    im_SetLastCmd(textview_GetIM(self), lcstate);	/* be transparent */
}

void textview_ViCommandCmd(self, key)
struct textview	*self;
long		key;
{
    char tc;

    tc = im_GetCharacter(self->header.view.imPtr);
    switch (tc )
    {
	case 'r':
	    textview_InsertFile(self, key);
	    break;
	case 'q':
	    message_DisplayString(self, 0, "Please use Quit menu item.");
	    break;
	case 'w':
	    message_DisplayString(self, 0, "Please use Save or File/Save-As menu items.");
	    break;
	case 'e':
	    message_DisplayString(self, 0, "Please use Switch File menu item.");
	    break;
    }
}

void textview_ToggleEditorCmd(self)
struct textview *self;
{
    textview_ToggleEditor(self);
}

void textview_GrabReference(self,key)
struct textview *self;
long key;
{
    long pos,len;
    struct viewref *vr;
    struct text *d = Text(self);
    pos = textview_GetDotPosition(self);
    len = textview_GetDotLength(self);
    if (len == 0) len = text_GetLength(d) - pos;
    if ((vr = text_FindViewreference(d, pos, len)) == NULL)
        message_DisplayString(self, 0, "No References Found");
    else
        d->currentViewreference = vr;
}

void textview_PlaceReference(self,key)
struct textview *self;
long key;
{
    long pos;
    char p[250];
    struct text *d = Text(self);
    boolean promptforname = im_ArgProvided(textview_GetIM(self));

    im_ClearArg(textview_GetIM(self)); 

    if (ConfirmReadOnly(self))
        return;
    if(text_GetObjectInsertionFlag(Text(self)) == FALSE){
	message_DisplayString(self, 0, "Object Insertion Not Allowed!");
	return;
    }

    *p = '\0';
    if(d->currentViewreference == NULL) {
        message_DisplayString(self, 0, "No References Found");
        return;
    }
    pos = textview_GetDotPosition(self) +  textview_GetDotLength(self);
    if(promptforname && message_AskForString (self, 0, "View to place here ", d->currentViewreference->viewType, p, 200) < 0) return;
    if (p[0] == '\0')  strcpy(p,d->currentViewreference->viewType);
    if(textview_objecttest(self,p,"view") == FALSE) return;
    text_AddView(d,pos,p, d->currentViewreference->dataObject);
    text_NotifyObservers(d,observable_OBJECTCHANGED);
}

void textview_CheckSpelling(self)
struct textview *self;
{
    message_DisplayString(self, 0,
       "Sorry; \"Check Spelling\" is not implemented.");
}

void textview_ToggleReadOnly(self)
struct textview *self;
{
    boolean argp = im_ArgProvided(textview_GetIM(self)), arg;
    struct text *myText = Text(self);
    struct buffer *buf;

    if (argp)
	arg = im_Argument(textview_GetIM(self));

    buf = buffer_FindBufferByData(myText);

    if ((argp && arg) || (!argp && text_GetReadOnly(myText))) {
	/* In readonly mode. */
	if (buf)
	    buffer_SetReadOnly(buf, FALSE);
	else
	    text_SetReadOnly(myText, FALSE);
    }
    else {
	if (buf)
	    buffer_SetReadOnly(buf, TRUE);
	else
	    text_SetReadOnly(myText, TRUE);
    }

    if (text_GetReadOnly(myText))
	message_DisplayString(self, 0, "Text is now read only.");
    else
        message_DisplayString(self, 0, "Text is now writable.");
    text_NotifyObservers(myText, observable_OBJECTCHANGED); /* Handles updating of menus on read only transition. */
}

void textview_InsertPageBreak (self)
    register struct textview *self;
{
    long pos;
    struct text *d;

    d = Text(self);
    if(text_GetObjectInsertionFlag(d) == FALSE){
	message_DisplayString(self, 0, "Object Insertion Not Allowed!");
	return;
    }
    pos = textview_GetDotPosition(self);
    textview_PrepareInsertion(self, TRUE);
    if(text_GetChar(d,pos) != '\n'){
	text_InsertCharacters(d,pos,"\n",1);
    }
    if(text_GetChar(d,pos - 1) != '\n'){
	text_InsertCharacters(d,pos,"\n",1);
	pos++;
    }

    /* self->currentViewreference = */
    text_InsertObject(d, pos,"bp","bpv"); 
    textview_FinishInsertion(self);
    text_NotifyObservers(d,observable_OBJECTCHANGED);
    if (im_GetLastCmd(textview_GetIM(self)) == lcInsertEnvironment) {
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
    }
}

void textview_NextPage (self)
    register struct textview *self;
{
    long pos,len;
    struct text *d;
    struct viewref *vr;
    d = Text(self);
    len = text_GetLength(d);
    pos = textview_GetDotPosition(self);
    while ( pos <= len && (pos = text_Index(d,pos,TEXT_VIEWREFCHAR,len - pos)) != EOF){
	if((vr = text_FindViewreference(d,pos,1)) != NULL && 
	   *(vr->viewType) == 'b' &&
	   strcmp(vr->viewType,"bpv") == 0){
	    textview_SetDotPosition(self,pos + 2);
	    textview_SetTopPosition(self,pos + 2);
	    break;
	}
	pos++;
    }
}
long text_rindex(txt,pos,c)
register struct text *txt;
register long pos;
register char c;
{
    for(;pos > 0;pos--){
	if(text_GetChar(txt,pos) == c) return pos;
    }
    return EOF;
}
void textview_LastPage (self)
    register struct textview *self;
{
    long pos,cnt;
    struct text *d;
    struct viewref *vr;
    d = Text(self);
    pos = textview_GetDotPosition(self);
    for ( cnt = 0;pos > 0 && (pos = text_rindex(d,pos,TEXT_VIEWREFCHAR)) != EOF;pos--){
	if((vr = text_FindViewreference(d,pos,1)) != NULL && 
	   *(vr->viewType) == 'b' &&
	   strcmp(vr->viewType,"bpv") == 0){
	    if(cnt++ == 0) continue;
	    textview_SetDotPosition(self,pos + 2);
	    textview_SetTopPosition(self,pos + 2);
	    return;
	}
    }
    textview_SetDotPosition(self,0);
    textview_SetTopPosition(self,0);
}
#if 0
boolean lookforfootnote(self,text,pos,env)
struct textview *self;
struct text *text;
long pos;
struct environment *env;
{
    struct style *st;
    char *sn;
    if(env->type == environment_Style) {
	st = env->data.style;
	if(st != NULL && ((sn = style_GetName(st)) != NULL) &&
	   *sn == 'f' && strcmp(sn,"footnote") == 0) return TRUE;
    }
    return FALSE;
}
#endif

void textview_InsertFootnote(self)
    register struct textview *self;
{
    long pos;
    struct fnote *fn;

    if(text_GetObjectInsertionFlag(Text(self)) == FALSE){
	message_DisplayString(self, 0, "Object Insertion Not Allowed!");
	return;
    }
    pos = textview_GetDotPosition(self);
#if 0
    if(text_EnumerateEnvironments(Text(self),pos,0,lookforfootnote,NULL) != NULL){
	message_DisplayString(self,0,"footnotes in footnotes not supported");
	return;
    }
#endif
    fn = fnote_New();
/*    self->currentViewreference = text_InsertObject(Text(self), pos,"fnote","fnotev"); */
    text_AddView(Text(self), pos,"fnotev",fn);
    fnote_addenv(fn,Text(self),pos);
    
    text_NotifyObservers(Text(self),observable_OBJECTCHANGED);
    textview_SetDotPosition(self,pos + 1);
}
void textview_OpenFootnotes(self)
    register struct textview *self;
{
    fnote_OpenAll(Text(self));
}
void textview_CloseFootnotes(self)
    register struct textview *self;
{
    fnote_CloseAll(Text(self));
}
void textview_WriteFootnotes(self)
    register struct textview *self;
{
    FILE *f,*fopen();
    struct text *tmpt;
    f = fopen("/tmp/notes","w");
    tmpt = text_New();
    fnote_CopyAll(Text(self),tmpt,1,TRUE);
    text_Write(tmpt,f,0,0);
    fclose(f);
    text_Destroy(tmpt);
}

