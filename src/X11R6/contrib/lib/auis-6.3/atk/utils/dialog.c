/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/utils/RCS/dialog.c,v 1.6 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 

#include <stdio.h>
#include <andrewos.h>
#include <environ.ih>
#include <fontdesc.ih>
#include <observe.ih>
#include <sbutton.ih>
#include <text.ih>
#include <util.h>
#include <graphic.ih>

#include <dialog.eh>

boolean dialog__InitializeClass(classID, self)
struct classheader *classID;
struct dialog *self;
{
    return TRUE;
}


boolean dialog__InitializeObject(classID, self)
struct classheader *classID;
struct dialog *self;
{
    struct sbutton_prefs *prefs;
    
    self->prefs=sbutton_GetNewPrefs("dialog");

    prefs=sbutton_GetNewPrefs(NULL);
    
    if(self->prefs == NULL || prefs == NULL) {
	if(self->prefs!=NULL) sbutton_FreePrefs(self->prefs);
	if(prefs!=NULL) sbutton_FreePrefs(prefs);
	return FALSE;
    }
    
    sbutton_InitPrefs(self->prefs, "dialog");

    *prefs=(*self->prefs);

    prefs->refcount=0;

    sbutton_InitPrefs(prefs, "dialogbutton");    
    
    self->text=text_New();
    if(self->text==NULL) return FALSE;

    self->buttons=sbutton_CreateSButton(prefs);
    if(self->buttons==NULL) {
	sbutton_FreePrefs(self->prefs);
	text_Destroy(self->text);
	return FALSE;
    }        
    return TRUE;
}

void dialog__FinalizeObject(classID, self)
struct classheader *classID;
struct dialog *self;
{
    if(self->prefs) {
	sbutton_FreePrefs(self->prefs);
	self->prefs=NULL;
    }
    
    if(self->text) {
	text_Destroy(self->text);
	self->text=NULL;
    }
    if(self->buttons) {
	sbutton_Destroy(self->buttons);
	self->buttons=NULL;
    }
}


char *dialog__ViewName(self)
struct dialog *self;
{
    return "dialogv";
}

long dialog__Write(self, fp, id, level)
struct dialog *self;
FILE *fp;
long id;
int level;
{
    long uniqueid = dialog_UniqueID(self);

    if (id != dialog_GetWriteID(self)) {
	/* New Write Operation */
	sbutton_SetWriteID(self, id);
	
	fprintf(fp, "\\begindata{%s,%d}\nDatastream version: %d\n",
		class_GetTypeName(self), 
		uniqueid, dialog_DS_VERSION);

	if(!self->text) {
	    struct text *t=text_New();
	    text_Write(t, fp, id, level+1);
	    text_Destroy(t);
	} else {
	    text_Write(self->text, fp, id, level+1);
	}

	if(!self->buttons) {
	    struct sbutton *s=sbutton_New();
	    sbutton_Write(s, fp, id, level+1);
	    sbutton_Destroy(s);
	} else {
	    sbutton_Write(self->buttons, fp, id, level+1);
	}
	fprintf(fp, "\\enddata{%s,%d}\n", class_GetTypeName(self), uniqueid);
    }
    return(uniqueid);
}

static long SanelyReturnReadError(self, fp, id, code)
struct dialog *self;
FILE *fp;
long id;
long code;
{
    /*
      Suck up the file until our enddata, then return the error code.
      */
    char buf[1024], buf2[255];

    sprintf(buf2, "\\enddata{%s,%ld}\n", class_GetTypeName(self), id);
    do {
	if ((fgets(buf, sizeof(buf)-1, fp)) == NULL)
	    return(dataobject_PREMATUREEOF);
    } while (strncmp(buf, "\\enddata{", 9) != 0); /* find an enddata */

    if (strcmp(buf, buf2) != 0) {
	return(dataobject_MISSINGENDDATAMARKER); /* not ours! */
    }

    return(code);
}

static char *DatastreamHeader="Datastream version:";

long dialog__Read(self, fp, id)
struct dialog *self;
FILE *fp;
long id;
{
    char buf[1024], *p;
    long err;
    long textid;
    
    dialog_SetID(self, dialog_UniqueID(self));

    p=fgets(buf, sizeof(buf)-1, fp);
    if(!p) return dataobject_PREMATUREEOF;
    if (strncmp(buf, DatastreamHeader, sizeof(DatastreamHeader) - 1))
	return(SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT));
    
    if ((atoi(buf+sizeof(DatastreamHeader)-1)) > dialog_DS_VERSION) return(SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT));

    p=fgets(buf, sizeof(buf)-1, fp);
    if(!p) return SanelyReturnReadError(self, fp, id, dataobject_PREMATUREEOF);
    
    if(sscanf(buf, "\\begindata{text,%d}", &textid )!=1)  SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT);
    
    if(self->text) text_Clear(self->text);
    else self->text=text_New();

    if(!self->text) return  SanelyReturnReadError(self, fp, id, dataobject_PREMATUREEOF);
    err=text_Read(self->text, fp, textid);

    if(err!=dataobject_NOREADERROR) return SanelyReturnReadError(self, fp, id, err);
    
    if(!self->buttons) return SanelyReturnReadError(self, fp, id, dataobject_PREMATUREEOF);
    
    p=fgets(buf, sizeof(buf)-1, fp);
    if(!p) return SanelyReturnReadError(self, fp, id, dataobject_PREMATUREEOF);
    
    if(sscanf(buf, "\\begindata{sbutton,%d}", &textid )!=1)  return SanelyReturnReadError(self, fp, id, dataobject_BADFORMAT);

    err=sbutton_Read(self->buttons, fp, textid);
    
    if(err!=dataobject_NOREADERROR) return SanelyReturnReadError(self, fp, id, err);

    return SanelyReturnReadError(self, fp, id, dataobject_NOREADERROR); 
}

void dialog__SetText(self, text)
struct dialog *self;
struct text *text;
{
    if(self->text) text_Destroy(self->text);
    self->text=text;
}

void dialog__SetButtons(self, buttons)
struct dialog *self;
struct sbutton *buttons;
{
    if(self->buttons) sbutton_Destroy(self->buttons);
    self->buttons=buttons;
}

char *dialog__GetForeground(self)
struct dialog *self;
{
    char *fg, *bg;
    graphic_GetDefaultColors(&fg, &bg);
    if(self->prefs->colors[sbutton_FOREGROUND]) return self->prefs->colors[sbutton_FOREGROUND];
    else return fg?fg:"black";
}

char *dialog__GetBackground(self)
struct dialog *self;
{
    char *fg, *bg;
    graphic_GetDefaultColors(&fg, &bg);
    if(self->prefs->colors[sbutton_BACKGROUND]) return self->prefs->colors[sbutton_BACKGROUND];
    else return bg?bg:"white";
}
