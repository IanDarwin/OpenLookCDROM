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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/txtvcmod.c,v 3.3 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <ctype.h>
#include <txtvcmds.h>
#include <txtvinfo.h>

#include <text.ih>
#include <im.ih>
#include <message.ih>
#include <envrment.ih>
#include <viewref.ih>
#include <complete.ih>

#include <sys/param.h>
#include <sys/stat.h>

#define AUXMODULE 1
#include <textv.eh>

extern void textview_ForwardWordCmd();
extern void textview_EndOfWordCmd();
extern void textview_BackwardWordCmd();
extern void textview_ForwardWSWordCmd();
extern void textview_BackwardWSWordCmd();
extern void textview_EndOfWSWordCmd();

static void yankDeleteWord ();

int textview_GetNextNonSpacePos(self, pos)
struct textview *self;
int pos;
{
    struct text *d = Text(self);
    long len = text_GetLength(d);

    while (pos < len)  {
	long tc = 0xff & text_GetChar(d,pos);
	if (tc != ' ' && tc != '\t') {
	    return pos;
	}
	pos++;
    }
    return pos;
}
    
void textview_AddSpaces(self, pos, startPos, len)
struct textview *self;
long pos;
long startPos;
long len;
{
    struct text *d = Text(self);

    if (len > 0) {
	char *buf = (char *) malloc(len + 1);

	text_CopySubString(d, startPos, len, buf, FALSE);
	text_InsertCharacters(d, pos, buf, len);
	free(buf);
    }
}
	
boolean ConfirmViewDeletion(self, pos, len)
struct textview *self;
long pos, len;
{
    struct text *d;
    boolean hasViews;
    static char *yesOrNo[] = {"Yes", "No", NULL};
    long answer;
    struct environment *env;

    d = Text(self);

    for (hasViews = FALSE; len--; pos++)
	if (text_GetChar(d, pos) == TEXT_VIEWREFCHAR) {
	    env = textview_GetStyleInformation(self, NULL, pos, NULL);
	    if (env->type == environment_View) {
		hasViews = TRUE;
		textview_ReleaseStyleInformation(self, env);
		break;
	    }
	    textview_ReleaseStyleInformation(self, env);
	}

    if (! hasViews)
        return TRUE;

    if (message_MultipleChoiceQuestion(self, 80,
         "Really delete inset(s)?", 1, &answer,
          yesOrNo, NULL) < 0 || answer != 0) {
        message_DisplayString(self, 0, "Cancelled.\n");
        return FALSE;
    }

    return TRUE;
}

/* Added friendly read-only behavior 04/27/89 --cm26 */

boolean ConfirmReadOnly(self)
struct textview *self;
{
    if (text_GetReadOnly(Text(self))) {
        message_DisplayString(self, 0,
          "Document is read only.");
        return TRUE;
    } else
        return FALSE;
}

void textview_SelfInsertCmd(self, a)
register struct textview *self;
char a;
{
    register int ct, i, pos;
    register struct text *d;

    if (ConfirmReadOnly(self))
        return;

    d = Text(self);
    ct=im_Argument(textview_GetIM(self));
    pos = textview_CollapseDot(self);
    textview_PrepareInsertion(self, FALSE);
    for (i = 0; i < ct; i++) {
    	text_InsertCharacters(d,pos++,&a,1);
    }
    textview_FinishInsertion(self);
    textview_SetDotPosition(self,pos);
    textview_FrameDot(self, pos);
    text_NotifyObservers(d, observable_OBJECTCHANGED);
    if (im_GetLastCmd(textview_GetIM(self)) == lcInsertEnvironment) {
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
    }
}

void textview_DeleteWordCmd (self)
register struct textview *self;
{
    register int count, pos, len;


    if ( self->editor == VI )
	yankDeleteWord(self, DELETE, textview_ForwardWordCmd);
    else {
	if (ConfirmReadOnly(self))
	    return;

	pos = textview_CollapseDot(self);

	count = im_Argument(textview_GetIM(self));
	im_ClearArg(textview_GetIM(self));   /* Don't want Fwd Word to use it! */

	while (count--)
	    textview_ForwardWordCmd(self);

	len = textview_GetDotPosition(self) - pos;

	if (ConfirmViewDeletion(self, pos, len)) {
	    textview_DeleteCharacters(self, pos, len);
	    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
	    textview_FrameDot(self, pos);
	}
    }
}

void textview_ForeKillWordCmd (self)
register struct textview *self;
{
    register int count, pos, len;
    register struct text *d;

    if ( self->editor == VI )
	yankDeleteWord(self, DELETE, textview_ForwardWordCmd);
    else {
	if (ConfirmReadOnly(self))
	    return;

	d = Text(self);

	pos = textview_CollapseDot(self);

	count = im_Argument(textview_GetIM(self));
	im_ClearArg(textview_GetIM(self));   /* Don't want Fwd Word to use it! */

	while (count--)
	    textview_ForwardWordCmd(self);

	len = textview_GetDotPosition(self) - pos;

	if (ConfirmViewDeletion(self, pos, len)) {
	    if (im_GetLastCmd(textview_GetIM(self)) == lcKill) {
		FILE *pasteFile;
		pasteFile = im_OnlyFromCutBuffer(textview_GetIM(self));
		len += text_InsertFile(d, pasteFile, NULL, pos);
		im_CloseFromCutBuffer(textview_GetIM(self), pasteFile);
		/* hack! Back up the ring so we overwrite old with new. */
		im_RotateCutBuffers(textview_GetIM(self), 1);
	    }

	    textview_DoCopyRegion(self, pos, len, FALSE, d->CopyAsText);
	    textview_DeleteCharacters(self, pos, len);
	    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
	    textview_FrameDot(self, pos);
	    im_SetLastCmd(textview_GetIM(self), lcKill);
	}
    }
}

void textview_OpenLineCmd(self)
register struct textview *self;
{
    register int i, pos, ct;
    char tc;
    struct text *d;

    if (ConfirmReadOnly(self))
        return;
    pos = textview_CollapseDot(self);
    d=Text(self);
    tc = '\012';
    i = 0;
    ct = im_Argument(textview_GetIM(self));
    textview_PrepareInsertion(self, TRUE);
    for (i = 0; i < ct; i++) {
	text_InsertCharacters(d,pos,&tc,1);
    }
    textview_FinishInsertion(self);

    if ( self->editor == VI )
	textview_NextLineCmd(self);

    text_NotifyObservers(d, observable_OBJECTCHANGED);
    if (im_GetLastCmd(textview_GetIM(self)) == lcInsertEnvironment) {
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
    }
}

void textview_JoinCmd(self)
    register struct textview *self;
{
    struct text	*text;
    register int	i, ct;
    long pos;
    char	blank = ' ';

    if (ConfirmReadOnly(self))
        return;
    ct = im_Argument(self->header.view.imPtr);
    im_ClearArg(self->header.view.imPtr);
    text = Text(self);
    for (i = 0; i < ct; i++)
    {
	textview_EndOfLineCmd(self);
	pos = textview_GetDotPosition(self);
	if (pos < text_GetLength(text) )
	{
	    if ( text_GetChar(text, pos) == '\n' )
	    {
		text_ReplaceCharacters(text, pos, 1,  &blank, 1);
		textview_SetDotPosition(self, ++pos);
	    }
	    if ( charType(text_GetChar(text, pos)) == WHITESPACE )
		textview_DeleteWordCmd(self);
	}
    }
}

/* In order to append to ATK datastream in the cutbuffer,
  we must yank it into text, and then cut the larger datastream
  back out in place of the old. Otherwise we might get two nested
  text obejcts instead of one long text object. */

static void yankKillLine (self, action)
    register struct textview *self;
    int		action;
{
    register int count, pos, endpos, lastpos, numNLs, applen = 0;
    register struct text *d;

    endpos = pos = textview_CollapseDot(self);
    d = Text(self);
    if ( text_GetChar(d,pos) == '\012')
	return;
    count = im_Argument(self->header.view.imPtr);
    lastpos = text_GetLength(d);

    /* find end of line range to be deleted/yanked */
    for (numNLs = 0; endpos < lastpos && numNLs < count; endpos++)
	if (text_GetChar(d, endpos) == '\012')
	    numNLs++;

    if (im_GetLastCmd(textview_GetIM(self)) == lcKill) {
	FILE *pasteFile;
	pasteFile = im_OnlyFromCutBuffer(textview_GetIM(self));
	applen = text_InsertFile(d, pasteFile, NULL, pos);
	im_CloseFromCutBuffer(textview_GetIM(self), pasteFile);
	/* hack! back up the ring so we overwrite old with new. */
	im_RotateCutBuffers(textview_GetIM(self), 1);
    }

    textview_DoCopyRegion(self, pos, endpos + applen - pos, FALSE, d->CopyAsText);

    /* If this is not a deletion operation, we still have to
      remove the chars we inserted for our append operation */
    if ( action == DELETE )
	textview_DeleteCharacters(self, pos, endpos + applen - pos);
    else
	textview_DeleteCharacters(self, pos, applen);
	
    im_SetLastCmd(self->header.view.imPtr, lcKill);	/* mark us as a text killing command */
    text_NotifyObservers(d, observable_OBJECTCHANGED);
}

void textview_YankLineCmd(self)
    register struct textview *self;
{
    yankKillLine(self, YANK);
}

boolean textview_objecttest(self,name,desiredname)
register struct textview *self;
char *name,*desiredname;
{
    if(class_Load(name) == NULL){
        char foo[640];
        sprintf(foo,"Can't load %s",name);
         message_DisplayString(self, 0, foo);
        return(FALSE);
    }
    if(! class_IsTypeByName(name,desiredname)){
        char foo[640];
        sprintf(foo,"%s is not a %s",name,desiredname);
         message_DisplayString(self, 0, foo);
        return(FALSE);
    }
    return(TRUE);
}

/*
  Prompt user for dataobject to insert, unless dataobject name is passed
  as (char *) in rock. If ArgProvided (^U) then also ask for view name.
*/

void textview_InsertInsetCmd (self, rock)
register struct textview *self;
long rock;
{
    char iname[100];
    char viewname[200];
    long pf;
    boolean promptforname = im_ArgProvided(textview_GetIM(self));

    im_ClearArg(textview_GetIM(self));    
    viewname[0] = '\0';
    if (ConfirmReadOnly(self))
        return;
    if(text_GetObjectInsertionFlag(Text(self)) == FALSE){
	message_DisplayString(self, 0, "Object Insertion Not Allowed!");
	return;
    }

    if (rock == NULL || (rock >= 0 && rock < 256)) {
        pf = message_AskForString(self, 0, "Data object to insert here: ", 0, iname, sizeof(iname));
        if (pf < 0){
            message_DisplayString(self, 0, "Punt!");
            return;
        }
        if(strlen(iname)==0){
            message_DisplayString(self, 0, "No name specified");
            return;
        }
    }
    else {
        strncpy(iname, (char *) rock, sizeof(iname));
    }

    if(textview_objecttest(self,iname,"dataobject") == FALSE) return;
    if(promptforname){
        if( message_AskForString (self, 0, "View to place here ", 0, viewname, 200) < 0) return;
        if(textview_objecttest(self,viewname,"view") == FALSE) return;
    }
    textview_PrepareInsertion(self, FALSE);
    self->currentViewreference = text_InsertObject(Text(self), textview_GetDotPosition(self), iname,viewname);
    textview_FinishInsertion(self);
    text_NotifyObservers(Text(self), observable_OBJECTCHANGED);
    if (im_GetLastCmd(textview_GetIM(self)) == lcInsertEnvironment) {
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
    }
    im_ForceUpdate();
}

void textview_InsertFile(self)
struct textview *self;
{
    char filename[MAXPATHLEN];
    FILE *inputFile;
    long initialPos;
    long pos;
    struct stat buf;
    
    if (ConfirmReadOnly(self))
        return;
    if (im_GetDirectory(filename) != NULL) /* Use CWD for now */
        strcat(filename, "/");
    if (completion_GetFilename(self, "Insert file: ", filename, filename, sizeof(filename), FALSE, TRUE) == -1 )
        return;
    stat(filename,&buf);
    if(buf.st_mode & S_IFDIR) {
	message_DisplayString(self, 0, "Can't insert a directory"); 
	return;
    }
    if ((inputFile = fopen(filename, "r")) == NULL) {
        message_DisplayString(self, 0, "Could not insert file."); /* Really should give a more informative error message. */
        return;
    }

    textview_CollapseDot(self);

    if ( self->editor == VI )
    {
	textview_EndOfLineCmd(self);
	textview_OpenLineCmd(self);
    }

    initialPos = pos = textview_GetDotPosition(self);

    textview_PrepareInsertion(self, FALSE);
    pos+= text_InsertFile(Text(self), inputFile,filename, pos); /*added "pos+=" so insertion would be highlighted. RSK*/

    fclose(inputFile);

    textview_SetDotPosition(self,initialPos);
    textview_SetDotLength(self, pos - initialPos);
/*  im_SetLastCmd(textview_GetIM(self), lcYank); */
    text_NotifyObservers(Text(self), observable_OBJECTCHANGED);
}

static void YankCmd(self, onlycut)
register struct textview *self;
boolean onlycut;
{
    long ct;
    long initialPos;
    long pos;
    struct text *d;
    FILE *pasteFile;

    if (ConfirmReadOnly(self))
        return;
    ct = im_Argument(textview_GetIM(self));
    if (ct > 100) {
        message_DisplayString(self, 0, "Yank argument limit: 100");
        return;
    }

    d = Text(self);

    initialPos = pos = textview_CollapseDot(self);

    textview_PrepareInsertion(self, FALSE);
    while (ct--) {
	pasteFile = onlycut ? im_OnlyFromCutBuffer(textview_GetIM(self)): im_FromCutBuffer(textview_GetIM(self));
        pos += text_InsertFile(d, pasteFile, NULL, pos);
	im_CloseFromCutBuffer(textview_GetIM(self), pasteFile);
    }
    textview_FinishInsertion(self);

    textview_SetDotPosition(self,initialPos);
    textview_SetDotLength(self, pos - initialPos);
    im_SetLastCmd(textview_GetIM(self), lcYank);
    text_NotifyObservers(d, observable_OBJECTCHANGED);
}

void textview_YankCmd(self)
struct textview *self;
{
    YankCmd(self, FALSE);
}

static textview_DoRotatePaste(self, count)
register struct textview *self;
int count;
{
    register struct text *d = Text(self);

    im_ClearArg(textview_GetIM(self));        /* Make it safe to call yank. */

    if (ConfirmReadOnly(self))
        return;
    text_DeleteCharacters(d,		/* Get rid of what is there now. */
	textview_GetDotPosition(self), textview_GetDotLength(self));
    if (im_GetLastCmd(textview_GetIM(self)) != lcYank)    /* If not following yank. */
        count--;    /* Make it get top thing off of ring, instead of one down on ring. */
    im_RotateCutBuffers(textview_GetIM(self), count);    /* Put the ring in the right place. */
    YankCmd(self, TRUE);    /* Snag it in off the ring. */
}



void textview_PutAfterCmd(self)
    register struct textview *self;
{
    FILE *pasteFile;
    int cutChar;
    long	oldPos;

    oldPos = textview_CollapseDot(self);

    pasteFile = im_FromCutBuffer(textview_GetIM(self));
    while ( (cutChar = getc(pasteFile)) != EOF && cutChar != '\n' );
    im_CloseFromCutBuffer(textview_GetIM(self), pasteFile);
    if ( cutChar == '\n' )
    {
	/* lines are being pasted - insert text after CURRENT LINE */
	textview_NextLineCmd(self);
	textview_BeginningOfLineCmd(self);
	if ( textview_GetDotPosition(self) <= oldPos )
	{
	    /* at end of file and no NL - add one */
	    textview_EndOfLineCmd(self);
	    textview_OpenLineCmd(self);
	}
    }
    /* otherwise insert at current cursor position */
    textview_YankCmd(self);
}

void textview_PutBeforeCmd(self)
    register struct textview *self;
{
    FILE *pasteFile;
    int cutChar;

    textview_CollapseDot(self);

    pasteFile = im_FromCutBuffer(textview_GetIM(self));
    while ( (cutChar = getc(pasteFile)) != EOF && cutChar != '\n' );
    im_CloseFromCutBuffer(textview_GetIM(self), pasteFile);
    if ( cutChar == '\n' )
	/* lines are being pasted - insert text before CURRENT LINE */
	textview_BeginningOfLineCmd(self);
    /* otherwise insert at current cursor position */
    textview_YankCmd(self);
}

void textview_BackwardsRotatePasteCmd(self)
struct textview *self;
{
    textview_DoRotatePaste(self, - im_Argument(textview_GetIM(self)));
}

void textview_RotatePasteCmd(self)
struct textview *self;
{
    textview_DoRotatePaste(self, im_Argument(textview_GetIM(self)));
}

void textview_InsertNLCmd(self)
register struct textview *self;
{
    textview_SelfInsertCmd(self,'\012');
    if (im_GetLastCmd(textview_GetIM(self)) != lcInsertEnvironment) {
	im_SetLastCmd(textview_GetIM(self), lcNewLine);
    }
}

static int stringmatch(d,pos,c)
register struct text *d;
register long pos;
register char *c;
{
    /* Tests if the text begins with the given string */
    while(*c != '\0') {
        if(text_GetChar(d,pos) != *c) return FALSE;
        pos++; c++;
    }
    return TRUE;
}

/* NOTE! If you call this routine with the appendFlag set,
  if what is already in the cutFile is ATK datastream,
  and if what is in the region is formatted as ATK datastream,
  the cutFile will contain TWO nested datastreams.

  You must yank the old into the region and cut the larger region
  if you want one datastream.
*/

void textview__DoCopyRegion(self, pos, len, appendFlag, copyAsText)
struct textview *self;
long pos, len;
boolean appendFlag;
{
    struct text *d;
    register long nextChange;
    FILE *cutFile;
    int UseDataStream;

    d = Text(self);
    environment_GetInnerMost(d->rootEnvironment, pos);
    nextChange = environment_GetNextChange(d->rootEnvironment, pos);

    cutFile = im_ToCutBuffer(textview_GetIM(self));
    if (UseDataStream = ((nextChange <= len|| stringmatch(d,pos,"\\begindata")) && text_GetExportEnvironments(d)))
	fprintf(cutFile, "\\begindata{%s, %d}\n",
		copyAsText ? "text": class_GetTypeName(d),
		/* d->header.dataobject.id */ 999999);
    d->header.dataobject.writeID = im_GetWriteID();
    text_WriteSubString(d, pos, len, cutFile, UseDataStream);
    
    if (UseDataStream)
	fprintf(cutFile, "\\enddata{%s,%d}\n", 
		copyAsText ? "text": class_GetTypeName(d), /* d->header.dataobject.id */ 999999);

    if (appendFlag)
	im_AppendToCutBuffer(textview_GetIM(self), cutFile);
    else
	im_CloseToCutBuffer(textview_GetIM(self), cutFile);
}

void textview_ZapRegionCmd(self)
register struct textview *self;
{
    long pos, len;
    struct text *d = Text(self);

    if (ConfirmReadOnly(self))
        return;
    pos = textview_GetDotPosition(self);
    len = textview_GetDotLength(self);

    textview_DoCopyRegion(self, pos, len, FALSE, d->CopyAsText);
    textview_DeleteCharacters(self, pos, len);
    textview_SetDotLength(self, 0);
    im_SetLastCmd(textview_GetIM(self), lcKill);
    text_NotifyObservers(Text(self), observable_OBJECTCHANGED);
}

/* In order to safely allow killing multiple lines of text, each possibly */
/* including ATK objects, the cutbuffer contents are pasted back and then */
/* a larger area is re-cut.  This isn't visible and wouldn't be so bad were */
/* it not for the extreme inefficiency of cutbuffer transfers. */

void textview_KillLineCmd(self)
register struct textview *self;
{
    register int count, pos, endpos, lastpos;
    register struct text *d;


    if ( self->editor == VI )
    	yankKillLine(self, DELETE);
    else {
	if (ConfirmReadOnly(self))
	    return;
	d = Text(self);
	count = im_Argument(textview_GetIM(self));
	endpos = pos = textview_CollapseDot(self);
	lastpos = text_GetLength(d);

	while (count-- > 0 && endpos < lastpos) {
	    if (text_GetChar(d, endpos) == '\012')
		endpos++;
	    else
		while (text_GetChar(d, endpos) != '\012' && endpos < lastpos)
		    endpos++;
	}

	if (im_GetLastCmd(textview_GetIM(self)) == lcKill) {
	    FILE *pasteFile;
	    pasteFile = im_OnlyFromCutBuffer(textview_GetIM(self));
	    endpos += text_InsertFile(d, pasteFile, NULL, pos);
	    im_CloseFromCutBuffer(textview_GetIM(self), pasteFile);
	}

	if (endpos > pos) {
	    textview_DoCopyRegion(self, pos, endpos - pos, FALSE, d->CopyAsText);
	    textview_DeleteCharacters(self, pos, endpos - pos);
	    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
	    textview_SetDotLength(self, 0);
	    im_SetLastCmd(textview_GetIM(self), lcKill);
	}
    }
}

/* At MIT They think of subsequent kills as part of one
  user command.
  Multiple kills will overwrite one cut buffer element.
  Multiple kills interrupted by a cut in another
  window will result in the latter cut being appended to
  the other window's cut. */

void textview_MITKillLineCmd(self)
register struct textview *self;
{
    register int count, pos, endpos, lastpos;
    register struct text *d;


    if ( self->editor == VI )
    	yankKillLine(self, DELETE);
    else {
	if (ConfirmReadOnly(self))
	    return;
	d = Text(self);
	count = im_Argument(textview_GetIM(self));
	endpos = pos = textview_CollapseDot(self);
	lastpos = text_GetLength(d);

	while (count-- > 0 && endpos < lastpos) {
	    if (text_GetChar(d, endpos) == '\012')
		endpos++;
	    else
		while (text_GetChar(d, endpos) != '\012' && endpos < lastpos)
		    endpos++;
	}

	if (im_GetLastCmd(textview_GetIM(self)) == lcKill) {
	    FILE *pasteFile;
	    pasteFile = im_OnlyFromCutBuffer(textview_GetIM(self));
	    endpos += text_InsertFile(d, pasteFile, NULL, pos);
	    im_CloseFromCutBuffer(textview_GetIM(self), pasteFile);
	    /* hack! back up the ring so we overwrite old with new. */
	    im_RotateCutBuffers(textview_GetIM(self), 1);
	}

	if (endpos > pos) {
	    textview_DoCopyRegion(self, pos, endpos - pos, FALSE, d->CopyAsText);
	    textview_DeleteCharacters(self, pos, endpos - pos);
	    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
	    textview_SetDotLength(self, 0);
	    im_SetLastCmd(textview_GetIM(self), lcKill);
	}
    }
}

/* This routine will, if immediately followed by a text cutting commnd, cause
 * the cut to append to the current buffer instead of placing it in a new
 * buffer. */

void textview_AppendNextCut(self)
struct textview *self;
{
    im_SetLastCmd(textview_GetIM(self), lcKill);	/* mark us as a text killing command */
}

void textview_CopyRegionCmd (self)
register struct textview *self;
{
    struct text *d = Text(self);

    textview_DoCopyRegion(self,
		  textview_GetDotPosition(self),
		  textview_GetDotLength(self), FALSE, d->CopyAsText);
}

void textview_GetToCol (self, col)
register struct textview *self;
register int col;
{
    register struct text *d;
    register int pos;

    d = Text(self);
    pos = textview_GetDotPosition (self);
    textview_PrepareInsertion(self, FALSE);
    while (col > 0)  {
	if (col >= 8) {
	    col -= 8;
	    text_InsertCharacters (d,pos,"\011",1);
	}
	else  {
	    col--;
	    text_InsertCharacters(d,pos," ",1);
	}
	pos++;
    }
    textview_FinishInsertion(self);
    textview_SetDotPosition (self,pos);
    text_NotifyObservers(d, observable_OBJECTCHANGED);
    if (im_GetLastCmd(textview_GetIM(self)) == lcInsertEnvironment) {
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
    }
}

void textview_InsertSoftNewLineCmd(self)
register struct textview *self;
{
    textview_SelfInsertCmd(self,'\r');
}

void textview_MyLfCmd(self)
register struct textview *self;
{
    register struct text *d = Text(self);
    register int pos, startPos, endPos;

    if (ConfirmReadOnly(self))
	return;
    pos = textview_CollapseDot(self);
    startPos = text_GetBeginningOfLine(d, pos);
    endPos = textview_GetNextNonSpacePos(self, startPos);
    pos = text_GetEndOfLine(d, pos);
    textview_SetDotPosition(self, pos);
    textview_InsertNLCmd(self);
    textview_AddSpaces(self, textview_GetDotPosition(self), startPos, endPos - startPos);
    textview_SetDotPosition(self, textview_GetDotPosition(self) + endPos - startPos);
}

void textview_MySoftLfCmd(self)
struct textview *self;
{
    register struct text *d;
    register int pos, len, endPos, startPos;
    long c;

   if (ConfirmReadOnly(self))
        return;

    d = Text(self);
    pos = textview_CollapseDot(self);
    for (startPos = pos; startPos > 0 && (c = text_GetChar(d, startPos - 1)) != '\r' && c != '\n'; startPos--) {
    }

    endPos = textview_GetNextNonSpacePos(self, startPos);
    len = text_GetLength(d);
    while (pos < len && (c = text_GetChar(d, pos)) != '\r' && c != '\n') {
	pos++;
    }
    
    textview_SetDotPosition(self, pos);
    textview_InsertSoftNewLineCmd(self);
    textview_AddSpaces(self, textview_GetDotPosition(self), startPos, endPos - startPos);
    textview_SetDotPosition(self, textview_GetDotPosition(self) + endPos - startPos);
}

void textview_DeleteCmd (self)
register struct textview *self;
{
    register int pos, len;

    if (ConfirmReadOnly(self))
        return;
    pos = textview_CollapseDot(self);

    len = im_Argument(textview_GetIM(self));

    if (ConfirmViewDeletion(self, pos, len)) {
	textview_DeleteCharacters(self, pos, len);
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
	textview_SetDotPosition(self, pos); /* this is USUALLY unnecessary, but... -RSK*/
        textview_FrameDot(self, pos);
    }
}

void textview_ViDeleteCmd (self)
register struct textview *self;
{
    register struct text *d;
    register int pos, len, j;
    int		dsize;

    if (ConfirmReadOnly(self))
        return;
    d = Text(self);
    pos = textview_CollapseDot(self);

    if ( text_GetChar(d, pos - 1) == '\n' && text_GetChar(d, pos) == '\n' )
	return;
	
    dsize = text_GetLength(d);
    len = im_Argument(self->header.view.imPtr);

    if (ConfirmViewDeletion(self, pos, len)) {
	for (j = 0; j < len; j++)
	{
	    if ( text_GetChar(d, pos) != '\n' && pos < dsize)
	    {
		textview_DeleteCharacters(self, pos, 1);
	    }
	    else
		if ( pos-- >= 0 )
		{
		    textview_DeleteCharacters(self, pos, 1);
		}
		else
		{
		    pos	= 0;
		    break;
		}
	    dsize--;
	}
	textview_FrameDot(self, pos);
	text_NotifyObservers(d, observable_OBJECTCHANGED);
    }
}

void textview_KillWhiteSpaceCmd(self)
register struct textview *self;
{
    register struct text *d;
    register int p, tc, ep;

    /* First move back until no longer looking at whitespace */
    /* Then delete forward white space. */

    if (ConfirmReadOnly(self))
        return;
    d = Text(self);
    p = textview_CollapseDot(self);
    while (p > 0)  {
	tc = 0xff & text_GetChar (d,p-1);
	if (tc == 9 || tc == 32) 
	    p--;
	else
	    break;
    }
    textview_SetDotPosition(self,p);
    for (ep = p;
       ((tc = text_GetChar (d,ep)) != EOF) && (tc =='\t' || tc == ' '); ep++)
	;
    textview_DeleteCharacters(self, p, ep - p);
    im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
}

int textview_GetSpace(self, pos)
struct textview *self;
register int pos;
{
    register int rval;
    register int tc;
    register struct text *d;
    register long len;
    
    d = Text(self);
    rval = 0;
    len = text_GetLength(d);
    while (pos < len)  {
	tc = 0xff & text_GetChar(d,pos);
	if (tc == 32) rval++;
	else if (tc == 9)
	    rval += 8;
	else
	    return rval;
	pos++;
    }
    return rval;
}

static void AdjustIndentation(self, amount)
struct textview *self;
{
    int indentation;

    if (ConfirmReadOnly(self))
        return;

    textview_StartOfParaCmd(self);
    indentation = textview_GetSpace(self,
             textview_GetDotPosition(self));
    textview_KillWhiteSpaceCmd(self);
    indentation += amount;
    textview_GetToCol(self, indentation);
    textview_EndOfParaCmd(self);
}

void textview_UnindentCmd(self)
struct textview *self;
{
    AdjustIndentation(self, -4);
}

void textview_IndentCmd(self)
struct textview *self;
{
    AdjustIndentation(self, 4);
}

void textview_ExchCmd(self)
register struct textview *self;
{
    register long p;
    register long len;

    p = textview_GetDotPosition(self);
    len = textview_GetDotLength(self);
    textview_SetDotPosition(self, mark_GetPos(self->atMarker));
    textview_SetDotLength(self, mark_GetLength(self->atMarker));
    mark_SetPos(self->atMarker,p);
    mark_SetLength(self->atMarker, len);
    textview_FrameDot(self, textview_GetDotPosition(self));
    textview_WantUpdate(self, self);
}

void textview_RuboutCmd (self)
register struct textview *self;
{
    register long endpos, len;
    struct text *d = Text(self);

    if (ConfirmReadOnly(self))
        return;

    endpos = textview_CollapseDot(self);

    len = im_Argument(textview_GetIM(self));

    if (endpos == 0)
        return;

    if (endpos - len < 0)
	len = endpos;

    if (ConfirmViewDeletion(self, endpos - len, len)) {
	if ( self->editor == VI )
	    for ( ; endpos > 0 && len > 0 && text_GetChar(d, endpos - 1) != '\n' ; endpos--)
	    {
		text_DeleteCharacters(d,endpos - 1, 1);
		len--;
	    }
	else {
	    textview_DeleteCharacters(self, endpos -= len, len);
	}
        textview_SetDotPosition(self, endpos);
        textview_FrameDot(self, endpos);
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
    }
}

/* Switches the two characters before the dot. */

void textview_TwiddleCmd (self)
register struct textview *self;
{
    long pos;
    register struct text *text;
    char char1, char2;
    struct environment *env1, *env2;
    struct viewref *vr1, *vr2;

    if (ConfirmReadOnly(self))
        return;

    pos = textview_CollapseDot(self);
    text = Text(self);

    if (pos < 2 || text_GetReadOnly(text) == TRUE)
        return;

    char1 = text_GetChar(text, pos - 2);
    char2 = text_GetChar(text, pos - 1);

    text_ReplaceCharacters(text, pos - 2, 1, &char2, 1);
    text_ReplaceCharacters(text, pos - 1, 1, &char1, 1);

    /* Code to deal with VIEWREFCHARS  -cm26 */
    /* If a view must be moved, it's deleted then reinserted */
    /* (environment_Update does not seem to work) */
    /* The order of the following is kind of important */

    env1 = environment_GetInnerMost(text->rootEnvironment, pos - 2);
    if (env1 != NULL)
        if (env1->type != environment_View)
            env1 = NULL;
        else {
            vr1 = env1->data.viewref;
            env1->data.viewref = NULL;  /* Protect viewref from Delete */
            environment_Delete(env1);
        }

    env2 = environment_GetInnerMost(text->rootEnvironment, pos - 1);
    if (env2 != NULL)
        if (env2->type != environment_View)
            env2 = NULL;
        else {
            vr2 = env2->data.viewref;
            env2->data.viewref = NULL;  /* Protect viewref from Delete */
            environment_Delete(env2);
        }

    if (env1 != NULL)
        environment_WrapView(text->rootEnvironment,
            pos - 1, 1, vr1);

    if (env2 != NULL)
        environment_WrapView(text->rootEnvironment,
            pos - 2, 1, vr2);

    textview_FrameDot(self, pos);
    text_NotifyObservers(text, observable_OBJECTCHANGED);
}

void textview_RuboutWordCmd (self)
register struct textview *self;
{
    register int count, endpos, len;

    if (ConfirmReadOnly(self))
        return;

    endpos = textview_CollapseDot(self);

    count = im_Argument(textview_GetIM(self));
    im_ClearArg(textview_GetIM(self));   /* Don't want Bkwd Word to use it! */

    while (count--)
	textview_BackwardWordCmd(self);

    len = endpos - textview_GetDotPosition(self);

    if (ConfirmViewDeletion(self, endpos - len, len)) {
	textview_DeleteCharacters(self, endpos, -len);
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
        textview_FrameDot(self, endpos - len);
    }
}

/* The safest way to prepend 'back killed' text is to yank it and kill the larger region. */

void textview_BackKillWordCmd (self)
register struct textview *self;
{
    register int count, endpos, pos, startpos;
    register struct text *d;

    if (ConfirmReadOnly(self))
        return;

    d = Text(self);

    endpos = pos = textview_CollapseDot(self);

    count = im_Argument(textview_GetIM(self));
    im_ClearArg(textview_GetIM(self));   /* Don't want Bkwd Word to use it! */

    while (count--)
	textview_BackwardWordCmd(self);

    startpos = textview_GetDotPosition(self);

    if (ConfirmViewDeletion(self, startpos, endpos - startpos)) {
	if ((im_GetLastCmd(textview_GetIM(self)) == lcKill)) {
	    FILE *pasteFile;
	    pasteFile = im_OnlyFromCutBuffer(textview_GetIM(self));
	    endpos += text_InsertFile(d, pasteFile, NULL, pos);
	    im_CloseFromCutBuffer(textview_GetIM(self), pasteFile);
	    /* hack! Back up the ring so we overwrite old with new. */
	    im_RotateCutBuffers(textview_GetIM(self), 1);
	}
	textview_DoCopyRegion(self, startpos, endpos - startpos, FALSE, d->CopyAsText);
	textview_DeleteCharacters(self, startpos, endpos - startpos);
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
        textview_FrameDot(self, startpos);
	im_SetLastCmd(textview_GetIM(self), lcKill);
    }
}

static void yankDeleteWord (self, action, moveFunction)
    register struct textview *self;
    int		action;
    void	(*moveFunction) ();
{ 
    register int i, ct, pos, npos, cutpos;
    boolean	backward = FALSE;
    FILE	*cutFile;
    register struct text *text;

    if (ConfirmReadOnly(self))
        return;

    text = Text(self);
    textview_CollapseDot(self);
    i = 0;
    ct = im_Argument(textview_GetIM(self));
    im_ClearArg(textview_GetIM(self));
    cutFile = im_ToCutBuffer(textview_GetIM(self));
    while (i<ct)  {
	pos = textview_GetDotPosition(self);
	(*moveFunction)(self);
	npos=textview_GetDotPosition(self);
	if ( npos < pos )
	{
	    /* backward move */
	    backward	= TRUE;
	    cutpos = pos;
	    pos = npos;
	    npos = cutpos;
	}
	for (cutpos = pos; cutpos < npos; cutpos++)
	    putc(text_GetChar(text, cutpos), cutFile);
	if ( action == DELETE && ConfirmViewDeletion(self, pos, npos - pos) )
	    textview_DeleteCharacters(self, pos, npos - pos);
	if ( backward && action == YANK )
	    pos	= npos;
	textview_SetDotPosition(self, pos);
	i++;
    }
    im_CloseToCutBuffer(textview_GetIM(self), cutFile);
    textview_FrameDot(self, pos);
    text_NotifyObservers(Text(self), observable_OBJECTCHANGED);
}

void textview_YankWordCmd(self)
    register struct textview *self;
{
    yankDeleteWord(self, YANK, textview_ForwardWordCmd);
}

void textview_DeleteEndOfWordCmd(self)
    register struct textview *self;
{
    yankDeleteWord(self, DELETE, textview_EndOfWordCmd);
}

void textview_YankEndOfWordCmd(self)
    register struct textview *self;
{
    yankDeleteWord(self, YANK, textview_EndOfWordCmd);
}

void textview_DeleteBackwardWordCmd(self)
    register struct textview *self;
{
    yankDeleteWord(self, DELETE, textview_BackwardWordCmd);
}

void textview_YankBackwardWordCmd(self)
    register struct textview *self;
{
    yankDeleteWord(self, YANK, textview_BackwardWordCmd);
}

void textview_DeleteWSWordCmd(self)
    register struct textview *self;
{
    yankDeleteWord(self, DELETE, textview_ForwardWSWordCmd);
}

void textview_YankWSWordCmd(self)
    register struct textview *self;
{
    yankDeleteWord(self, YANK, textview_ForwardWSWordCmd);
}

void textview_DeleteBackwardWSWordCmd(self)
    register struct textview *self;
{
    yankDeleteWord(self, DELETE, textview_BackwardWSWordCmd);
}

void textview_YankBackwardWSWordCmd(self)
    register struct textview *self;
{
    yankDeleteWord(self, YANK, textview_BackwardWSWordCmd);
}

void textview_DeleteEndOfWSWordCmd(self)
    register struct textview *self;
{
    yankDeleteWord(self, DELETE, textview_EndOfWSWordCmd);
}

void textview_YankEndOfWSWordCmd(self)
    register struct textview *self;
{
    yankDeleteWord(self, YANK, textview_EndOfWSWordCmd);
}

static viYankDeleteLine(self, action)
    struct textview *self;
    int		action;
{
    register int  count, pos, endpos, lastpos, numNLs, applen = 0;
    register struct text *d;
 
    pos = textview_CollapseDot(self);
    d = Text(self);
    count = im_Argument(textview_GetIM(self));
    lastpos = text_GetLength(d);

    /* find beginning of line */
    while (pos > 0 && text_GetChar(d, pos - 1) != '\012')
	pos--;

    endpos = pos;

    /* find end of line range to be deleted/yanked */
    for (numNLs = 0; endpos < lastpos && numNLs < count; endpos++)
	if (text_GetChar(d, endpos) == '\012')
	    numNLs++;

    if (im_GetLastCmd(textview_GetIM(self)) == lcKill) {
	FILE *pasteFile;
	pasteFile = im_OnlyFromCutBuffer(textview_GetIM(self));
	applen = text_InsertFile(d, pasteFile, NULL, pos);
	im_CloseFromCutBuffer(textview_GetIM(self), pasteFile);
	/* hack! back up the ring so we overwrite old with new. */
	im_RotateCutBuffers(textview_GetIM(self), 1);
    }

    textview_DoCopyRegion(self, pos, endpos + applen - pos, FALSE, d->CopyAsText);

    /* If this is not a deletion operation, we still have to
      remove the chars we inserted for our append operation */
    if ( action == DELETE ) {
	textview_DeleteCharacters(self, pos, endpos + applen - pos);
	if (pos == text_GetLength(d) && pos > 0 )
	{
	    textview_SetDotPosition(self, pos - 1);
	    im_ClearArg(self->header.view.imPtr);
	    textview_BeginningOfFirstWordCmd(self);
	    pos = textview_GetDotPosition(self);
	}
	text_NotifyObservers(d, observable_OBJECTCHANGED);
	textview_SetDotPosition(self, pos);
	textview_FrameDot(self, pos);
	textview_WantUpdate(self, self);
    }
    else
	textview_DeleteCharacters(self, pos, applen);
	
    im_SetLastCmd(self->header.view.imPtr, lcKill);	/* mark us as a text killing command */
}

void textview_ViDeleteLineCmd(self)
    struct textview *self;
{
    viYankDeleteLine(self, DELETE);
}

void textview_ViYankLineCmd(self)
    struct textview *self;
{
    viYankDeleteLine(self, YANK);
}

void textview_OpenLineBeforeCmd(self)
    struct textview *self;
{
    long	oldPos;
    char	nlChar	=	'\n';

    oldPos	= textview_GetDotPosition(self);
    textview_PreviousLineCmd(self);
    im_ClearArg(textview_GetIM(self)); 
    if ( oldPos == textview_GetDotPosition(self) )
    {
	textview_PrepareInsertion(self, TRUE);
	text_InsertCharacters(Text(self), 0, &nlChar, 1);
	textview_FinishInsertion(self);
	textview_SetDotPosition(self, 0);
    }
    else
    {
	textview_EndOfLineCmd(self);
	textview_OpenLineCmd(self);
    }
    textview_ToggleVIMode(self);
}

void textview_OpenLineAfterCmd(self)
    struct textview *self;
{
    im_ClearArg(self->header.view.imPtr); 
    textview_EndOfLineCmd(self);
    textview_OpenLineCmd(self);
    textview_ToggleVIMode(self);
}

void textview_InsertAtBeginningCmd(self)
    struct textview *self;
{
    textview_BeginningOfLineCmd(self);
    textview_ToggleVIMode(self);
}

void textview_InsertAtEndCmd(self)
    struct textview *self;
{
    textview_EndOfLineCmd(self);
    textview_ToggleVIMode(self);
}

void textview_ChangeRestOfLineCmd(self)
    struct textview *self;
{
    im_ClearArg(self->header.view.imPtr); 
    textview_KillLineCmd(self);
    textview_ToggleVIMode(self);
}

void textview_ChangeLineCmd(self)
    struct textview *self;
{
    int		dsize;
    struct text	*text;
    
    text	= Text(self);
    dsize	= text_GetLength(text);
    
    textview_ViDeleteLineCmd(self);
    if ( text_GetEndOfLine(text, textview_GetDotPosition(self)) == dsize )
    	textview_OpenLineAfterCmd(self);
    else
	textview_OpenLineBeforeCmd(self);
}

void textview_ChangeWordCmd(self)
    struct textview *self;
{
    textview_DeleteEndOfWordCmd(self);
    textview_ToggleVIMode(self);
}

void textview_ChangeSelectionCmd(self)
    struct textview *self;
{
    textview_ZapRegionCmd(self);
    textview_ToggleVIMode(self);
}

void textview_ReplaceCharCmd(self)
    struct textview *self;
{
    char 	tc;
    long	pos;
    

    im_ClearArg(self->header.view.imPtr); 
    pos	= textview_GetDotPosition(self);
    tc = im_GetCharacter(textview_GetIM(self));
    text_ReplaceCharacters(Text(self), pos, 1, &tc, 1);
    text_NotifyObservers(Text(self), observable_OBJECTCHANGED);
}

void textview_SubstituteCharCmd(self)
    struct textview *self;
{

    textview_ViDeleteCmd(self);
    textview_ToggleVIMode(self);
}

void textview_DigitCmd(self, c)
struct textview *self;
register char c;
{
    struct im *im = textview_GetIM(self);

    if ( self->editor == VI && self->viMode == COMMAND )
    {
	if (im_ArgProvided(im) || c != '0') {
	    im_BumpArg(im, c-'0');
	    im_DisplayArg(im);
	}
	else {
		/* kludge to handle "0" vi command */
		textview_BeginningOfLineCmd(self);
	}
	return;
    }
    textview_SelfInsertCmd(self, c);
}

static void AdjustCase(self, upper, firstOnly)
struct textview *self;
boolean upper, firstOnly;
{
    long pos, len, count, i;
    boolean capitalize;

    if (ConfirmReadOnly(self))
        return;

    pos = textview_GetDotPosition(self);
    len = textview_GetDotLength(self);
    count = len;

    if (len == 0)
        while (pos > 0 && isalnum(text_GetChar(Text(self), pos - 1)))
            pos--;

    capitalize = TRUE;

    while (pos < text_GetLength(Text(self))) {
        i = text_GetChar(Text(self), pos);
        if (len == 0) {
            if (! isalnum(i))
                break;
        } else {
            if (--count < 0)
                break;
        }
        if (upper) {
            if ((capitalize || ! firstOnly) && islower(i)) {
                char new = toupper(i);
                text_ReplaceCharacters(Text(self), pos, 1, &new, 1);
            }
	    else if(!capitalize && firstOnly && isupper(i)){
		char new = tolower(i);
		text_ReplaceCharacters(Text(self), pos, 1, &new, 1);
	    }
        } else if (isupper(i)) {
            char new = tolower(i);
            text_ReplaceCharacters(Text(self), pos, 1, &new, 1);
        }
        capitalize = isspace(i);
        pos++;
    }

    text_NotifyObservers(Text(self), observable_OBJECTCHANGED);
}

void textview_UppercaseWord(self, key)
struct textview *self;
long key;
{
    AdjustCase(self, TRUE, FALSE);
}

void textview_LowercaseWord(self, key)
struct textview *self;
long key;
{
    AdjustCase(self, FALSE, FALSE);
}

void textview_CapitalizeWord(self, key)
struct textview *self;
long key;
{
    AdjustCase(self, TRUE, TRUE);
}

void textview_ToggleCase(self, key)
struct textview *self;
long key;
{
    long pos = textview_CollapseDot(self);
    long len = text_GetLength(Text(self));
    int i;
    char new;

    if (ConfirmReadOnly(self) || pos==len)
        return;

    i = text_GetChar(Text(self), pos);

    if (isupper(i))
        new = tolower(i);
    else if (islower(i))
        new = toupper(i);
    else
        new = i;

    text_ReplaceCharacters(Text(self), pos, 1, &new, 1);
    text_NotifyObservers(Text(self), observable_OBJECTCHANGED);

    textview_SetDotPosition(self, pos + 1);
}

void textview_QuoteCmd(self)
register struct textview *self;
{
    register long i;
    long count;
    register struct text *d;
    char tc;
    long where;

    if (ConfirmReadOnly(self))
        return;
    count = im_Argument(textview_GetIM(self));
    d = Text(self);
    tc = im_GetCharacter(textview_GetIM(self));
    if (tc >= '0' && tc <= '7') {
        char c1 = im_GetCharacter(textview_GetIM(self)) - '0',
             c2 = im_GetCharacter(textview_GetIM(self)) - '0';
        tc = ((tc - '0' << 3) + c1 << 3) + c2;
    }
    where = textview_GetDotPosition(self);
    textview_PrepareInsertion(self, tc == '\n');
    for (i = 0; i < count; i++) {
	text_InsertCharacters(d, where+i, &tc, 1);
    }
    textview_FinishInsertion(self);
    textview_SetDotPosition(self, where+count);
    text_NotifyObservers(d, observable_OBJECTCHANGED);
    if (im_GetLastCmd(textview_GetIM(self)) == lcInsertEnvironment) {
	im_SetLastCmd(textview_GetIM(self), lcInsertEnvironment);
    }
}

