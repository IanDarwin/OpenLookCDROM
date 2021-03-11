/* Copyright 1994 Carnegie Mellon University All rights reserved.
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
#include <menulist.ih>
#include <bind.ih>
#include <complete.ih>
#include <observe.ih>
#include <text.ih>
#include <unknown.ih>
#include <message.ih>
#include <unknownv.eh>

static struct menulist *umenus=NULL;


static void SaveRaw(aself, rock)
struct basicobject *aself;
long rock;
{
    struct unknownv *self=(struct unknownv *)aself;
    struct unknown *dself=(struct unknown *)unknownv_GetDataObject(self);
    if(dself->odata==NULL) {
	message_DisplayString(self, 0, "The unavailable inset had no data apparently.");
    } else {
	long pos=0, len;
	char buf[1024];
	FILE *fp=NULL;
	int res;
	buf[0]='\0';
	res=completion_GetFilename(self, "File for raw inset data:", "/tmp/", buf, sizeof(buf), FALSE, FALSE);
	if(res!=0) {
	    message_DisplayString(self, 0, "Cancelled.");
	    return;
	}
	fp=fopen(buf, "w");
	if(fp==NULL) {
	    message_DisplayString(self, 100, "Couldn't open the file for writing.");
	    return;
	}

	    /* write out the data verbatim! */
	while(pos<text_GetLength(dself->odata)) {
	    char *buf=text_GetBuf(dself->odata, pos, text_GetLength(dself->odata)-pos, &len);
	    if(len==0 || fwrite(buf, 1, len, fp)!=len) {
		message_DisplayString(self, 100, "There was an error writing out the data.");
		return;
	    }
	    pos+=len;
	}
	if(fclose(fp)!=0) {
	    message_DisplayString(self, 100, "There was an error writing out the data.");
	    return;
	}
	message_DisplayString(self, 0, "The raw inset data has been written.");
    }
    return;
}

static char sepline[]="### Raw Inset Data, do not edit ###\n";
static void DisplayRaw(aself, rock)
struct basicobject *aself;
long rock;
{
    struct unknownv *self=(struct unknownv *)aself;
    struct unknown *dself=(struct unknown *)unknownv_GetDataObject(self);
    if(dself->odata) {
	unknown_AlwaysInsertCharacters(dself, unknown_GetLength(dself), sepline, strlen(sepline));
	unknown_AlwaysCopyText(dself, unknown_GetLength(dself), dself->odata, 0, text_GetLength(dself->odata));
	unknown_NotifyObservers(dself, observable_OBJECTCHANGED);
	message_DisplayString(self, 0, "Inserted raw data for unavailable inset.");
    } else {
	message_DisplayString(self, 0, "The unavailable inset had no data apparently.");
    }
}

static struct bind_Description  ubind[]={
    {"unknownv-save-raw", NULL, 0, "Unknown~30,Save Raw Data", 0, 0, SaveRaw, "Saves the raw data for an unavailable inset.", NULL},
    {"unknownv-display-raw", NULL, 0, "Unknown~30,Display Raw Data", 0, 0, DisplayRaw, "Displays the raw data for an unavailable inset.", NULL},
    {NULL, NULL, 0, NULL, 0, 0, NULL, NULL, NULL}
};

boolean unknownv__InitializeClass(c)
struct classheader *c;
{
    umenus=menulist_New();
    if(umenus==NULL) return FALSE;
    bind_BindList(ubind, NULL, umenus, &unknownv_classinfo);
    return TRUE;
}

boolean unknownv__InitializeObject(c, self)
struct classheader *c;
struct unknownv *self;
{
    self->menus=menulist_DuplicateML(umenus, self);
    if(self->menus) return TRUE;
    else return FALSE;
}

void unknownv__FinalizeObject(c, self)
struct classheader *c;
struct unknownv *self;
{
    if(self->menus) menulist_Destroy(self->menus);
}

void unknownv__PostMenus(self, menus)
struct unknownv *self;
struct menulist *menus;
{
    menulist_UnchainML(self->menus, self);
    if(menus) menulist_ChainBeforeML(self->menus, menus, self);
    super_PostMenus(self, self->menus);
}
