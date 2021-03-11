/* File rawtext.c created by R S Kemmetmueller

   rawtext, a style-less mode for ATK. */

/* Copyright 1988, 1994 Carnegie Mellon University and IBM. All rights reserved.
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/rawtext.c,v 1.6 1994/02/22 20:14:18 rr2b Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <envrment.ih>
#include <environ.ih>
#include <attribs.h>
#include "rawtext.eh"

boolean rawtext__InitializeObject(classID, self)
struct classheader *classID;
struct rawtext *self;
{
    self->NewlineEOF= environ_GetProfileSwitch("rawtext.newlineateof",FALSE); /*RSK92add*/
    self->OverstrikeMode= FALSE; /*RSK92overstrike*/
    rawtext_SetCopyAsText(self, TRUE); /* otherwise regions get pasted as INSETS */ /*RSK92add*/
    rawtext_SetObjectInsertionFlag(self, FALSE); /* don't let insets get inserted here */ /*RSK92add*/
    rawtext_SetExportEnvironments(self, FALSE); /*RSK91add*/
    return TRUE;
}

void rawtext__FinalizeObject(classID, self)
struct classheader *classID;
struct rawtext *self;
{
    return;
}

/* override */
long rawtext__Write(self, file, writeID, level)
struct rawtext *self;
FILE *file;
long writeID;
int level;
{
    long temp, len=rawtext_GetLength(self);
    if (len>0 && rawtext_PutNewlineAtEOF(self) && rawtext_GetChar(self, len-1)!='\n')
	/* add a newline char to the end */
        rawtext_InsertCharacters(self, len, "\n", 1);
    temp=super_Write(self, file, writeID, level);
    return temp;
}

/* override */
void rawtext__SetAttributes(self,atts)
struct rawtext *self;
struct attributes *atts;
{
    super_SetAttributes(self,atts);
    while (atts!=NULL) {
	if (strcmp(atts->key,"newline-at-eof")==0)
	    self->NewlineEOF= atoi(atts->value.string);
	else if (strcmp(atts->key,"overstrike")==0) /*RSK92overstrike*/
	    rawtext_ChangeOverstrikeMode(self,atoi(atts->value.string));
	atts=atts->next;
    }
}

/*RSK92overstrike: this is a duplicate of the original simpletext_InsertCharacters; it's used by the TABOVER macro to ignore overstrike mode*/
boolean rawtext__JustInsertCharacters(self,pos,str,len)
struct rawtext *self;
long pos;
char *str;
long len;
{
    if (pos >= rawtext_GetFence(self)) {
        rawtext_AlwaysInsertCharacters(self, pos, str, len);
	return TRUE;
    }
    else
        return FALSE;
}

/*RSK91overstrike: mostly snagged from Patch10's ConfirmViewDeletion in atk/text/txtvcmod.c*/
#define TEXT_VIEWREFCHAR '\377'
static boolean MakeSureNotOverstrikingView(d, pos, len)
struct rawtext *d;
long pos, len;
{
    boolean hasViews;
    struct environment *env;

    for (hasViews = FALSE; len--; pos++)
	if (rawtext_GetChar(d, pos) == TEXT_VIEWREFCHAR) {
	    env= environment_GetInnerMost(d->header.text.rootEnvironment, pos);
	    if (env->type == environment_View) {
		hasViews = TRUE;
		break;
	    }
	}
    if (! hasViews)
	return TRUE;
    return FALSE; /*can't type over insets*/
}

/*RSK91overstrike: this routine was based on Patch10's textview_ViDeleteCmd (atk/text/txtvcmod.c).*/
void rawtext__OverstrikeAChar(d,pos)
struct rawtext *d;
long pos;
{
    int	dsize;
    char c;

    if ( rawtext_GetChar(d, pos - 1) == '\n' && rawtext_GetChar(d, pos) == '\n' )
	return;
    dsize = rawtext_GetLength(d);
    if (MakeSureNotOverstrikingView(d, pos, 1)) {
	if ( (c=rawtext_GetChar(d, pos)) != '\n' && pos < dsize) {
	    if (c!='\t')
		rawtext_DeleteCharacters(d, pos, 1);
	}		
	rawtext_NotifyObservers(d, observable_OBJECTCHANGED);
    }
}

/*RSK91overstrike: override simpletext's normal character insertion*/
boolean rawtext__InsertCharacters(self, pos, str, len)
struct rawtext *self;
long pos;
char *str;
long len;
{
    if (rawtext_IsInOverstrikeMode(self)) {
	int i= 0;
	do  {
	    if (*(str+i)!='\n' && *(str+i)!='\t')
		rawtext_OverstrikeAChar(self,pos+i);
	    i++;
	} while (i<len);
    }
    return super_InsertCharacters(self,pos,str,len);
}

/* Change Log

$Log: rawtext.c,v $
 * Revision 1.6  1994/02/22  20:14:18  rr2b
 * Updated to the latest version.
 *
 * Revision 1.6  1992/06/08  16:02:30  rskm
 * added overstrike mode
 *
 * Revision 1.5  1992/05/21  12:30:59  turner
 * Convert some <> includes to quotes.
 *
 * Revision 1.4  1992/04/14  14:50:13  rskm
 * added a *preference*, rawtext.NewlineAtEOF (can be overridden by ezinit option)
 *
 * Revision 1.3  1992/03/24  19:50:16  rskm
 * added newline-at-eof ezinit option
 *
 * Revision 1.2  1992/03/24  16:10:08  rskm
 * made _Write method add newline to end if needed
 *
 * Revision 1.1  1991/10/22  10:39:12  rskm
 * Initial revision
 *

<date> <time><programmer>
<reason><version><Brief description and why change was made.> */
