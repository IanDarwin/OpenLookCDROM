/* File asmtext.c created by R S Kemmetmueller
  Copyright 1994 Carnegie Mellon University and IBM.  All rights reserved.
 
   asmtext, an Assembly language mode for ATK.
 
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/asmtext.c,v 1.10 1994/02/28 21:02:02 rr2b Exp $";
#endif

#include <class.h>


#include <attribs.h>
#include <toolcnt.h>    /*RSK90add*/

#include "srctext.ih"
#include "asmtext.eh"

static Dict *words[TABLESIZE];

/* addBangComment postpends a character to the string used to remember bang-comment chars.  This is of little use, except to make the info available to the VIEW, which needs it to create the proper keybindings */ /*RSK91add*/
static void addBangComment(self,ch)
struct asmtext *self;
char *ch;
{
    int indx=0;
    while (indx<MAX_BANGCHARS && self->bangComments[indx]!='\0')
	indx++;
    if (indx>=MAX_BANGCHARS) {
	printf("\nWARNING - you have exceeded the maximum of %d bang-comments.  `%c' ignored.", MAX_BANGCHARS, *ch);
	fflush(stdout);
	return;
    }
    /* if null string passed, assume semicolon */
    self->bangComments[indx]= (ch!=NULL)?(*ch):(';');
    self->bangComments[indx+1]= '\0';
}

/* setReindentFilterName remembers the name of the filter that was specified in ezinit */ /*RSK91add*/
static void setReindentFilterName(self,name)
struct asmtext *self;
char *name;
{
    if (self->reindentFilterName!=NULL)
	free(self->reindentFilterName);
    self->reindentFilterName= (char *)malloc(strlen(name)+1);
    strcpy(self->reindentFilterName, name);
}

void asmtext__SetAttributes(self,atts)
struct asmtext *self;
struct attributes *atts;
{
    super_SetAttributes(self,atts);
    while (atts!=NULL) {
	if (strcmp(atts->key,"bang-comment")==0)
	    addBangComment(self,atts->value.string);
	else if (strcmp(atts->key,"c-comments")==0)
	    asmtext_SetCComments(self,atoi(atts->value.string));
	else if (strcmp(atts->key,"reindent-filter-name")==0) {
	    setReindentFilterName(self,atts->value.string);
	    self->header.srctext.indentingEnabled= TRUE;
	}
	atts=atts->next;
    }
}

boolean asmtext__InitializeClass(classID)
struct classheader *classID;
{
    static Dict asmkeywords[]={
	{NULL,0,0} };
    srctext_BuildTable("asmtext",words,asmkeywords);
    return TRUE;
}

boolean asmtext__InitializeObject(classID, self)
struct classheader *classID;
struct asmtext *self;
{
    self->header.srctext.words= (Dict **)words; /*RSK91mod*/
    self->header.srctext.useTabs= FALSE;
    self->bangComments[0]= '\0';
    self->CComments= FALSE;
    self->reindentFilterName= NULL;
    ToolCount("EditViews-asmtext",NULL);    /*RSK90add*/
    return TRUE;
}

void asmtext__RedoStyles(self)
struct asmtext *self;
{
    struct nestedmark *root = (struct nestedmark *)self->header.text.rootEnvironment;
    long posn, len = asmtext_GetLength(self);
    int prev=0, c = '\n';
    /* c is initialized to a newline so the start of the file looks like the start of line. */
    asmtext_RemoveStyles(self); /* Remove the old styles, but leave the root environment in place. */
    for (posn=0; posn<len-1; posn++) {
	prev = c;
	posn=asmtext_SkipWhitespace(self,posn,len);
	c = asmtext_GetChar(self, posn);
	switch (c) {
	    case '\n':
	    case ' ': case '\t':
		/* common characters that can't be token chars or bang comments-- skip for speed */
		break;
	    case '*':
		if (prev=='/' && asmtext_UseCComments(self)) {
		    posn= asmtext_CheckComment(self,posn-1);
		    c= '\n';
		    break;
		}
		/* fall thru in case no C comments, but * IS a bang comment */
	    default:
		/* see if this is a bang-comment character */
		if (index(self->bangComments,c)!=NULL) {
		    posn= asmtext_CheckLinecomment(self, posn);
		    c= '\n';
		}
		if (asmtext_IsTokenChar(self,c))
		    /* might be a user-defined keyword */
		    posn= asmtext_CheckWord(self,posn,len);
	}
    }
}

/* override */
/* in srctext, Indent is called when Tab is hit with a region selected. This is being overridden in asmtext because asmtextview's Reindent should override srctextview's, and should never ever call this */
long asmtext__Indent(self,mark)
struct asmtext *self;
struct mark *mark;
{
    printf("\nERROR - asmtext__Indent empty override entered.\n");
    fflush(stdout);
    return 0;
}

/* asmtext_Keywordify makes buff all uppercase.  checkforceupper and Force Upper status are ignored. */
/* This function "keywordifies" a string. "Keywordify" means "make the word all upper case so it will be found in the hash table". */
char *asmtext__Keywordify(self, buff, checkforceupper)
struct asmtext *self;
char *buff;
boolean checkforceupper;
{
    if (buff!=NULL && strlen(buff)>0)
	makeupper(buff);
    return buff;
}
