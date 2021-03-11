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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/dsearch.c,v 2.12 1992/12/15 21:35:02 rr2b R6tape $";
#endif


 

#include <class.h>
#include <text.ih>
#include <textv.ih>
#include <im.ih>
#include <search.ih>
#include <message.ih>
#include <proctbl.ih>

#include <dsearch.eh>

#define MAXSTRING	256
/* Describe how the current seach string has failed. */
#define FORWARD		1
#define REVERSE		2

static char lastString[MAXSTRING] = "";
static char *lastPattern;
static int lcSearch = 0;
static int failures = 0;
static int lastDirection = FORWARD;

static int GetPattern();

static void dynsearch_SearchForward(view)
    register struct textview *view;
{

    int pos = 0, argument;
    register int count;
    register struct text *text;

    lastDirection = FORWARD;
    text = (struct text *) view->header.view.dataobject;
    argument = im_Argument(textview_GetIM(view));
    if (GetPattern(view, text, FORWARD) < 0)
	return;
    textview_SetDotPosition(view, textview_GetDotPosition(view) + textview_GetDotLength(view));
    for (count = 0; count < argument; count++) {
	pos = search_MatchPattern (text, textview_GetDotPosition(view), lastPattern);
	if (pos < 0) {
	    textview_SetDotLength(view,0);
	    message_DisplayString(view, 0, "Search failed.");
	    failures |= FORWARD;
	    textview_WantUpdate(view, view);
	    return;
	}
	failures = 0;
	textview_SetDotPosition(view, pos);
	textview_SetDotLength(view, search_GetMatchLength());
    }
    textview_FrameDot(view, pos);
    textview_WantUpdate(view, view);
    message_DisplayString(view, 0, "Done.");
    return;
}

static void dynsearch_SearchReverse(view)
    register struct textview *view;
{

    int argument, originalPos, pos = 0;
    register int count;
    register struct text *text;

    lastDirection = REVERSE;
    text = (struct text *) view->header.view.dataobject;
    originalPos = textview_GetDotPosition(view);
    argument = im_Argument(textview_GetIM(view));
    if (GetPattern(view, text, REVERSE) < 0)
	return;
    textview_SetDotLength(view, 0);
    if (originalPos > 0)
        textview_SetDotPosition(view, originalPos - 1);
    for (count = 0; count < argument; count++) {
	pos = search_MatchPatternReverse(text, textview_GetDotPosition(view), lastPattern);
	if (pos < 0) {
	    textview_SetDotPosition(view, originalPos);
            message_DisplayString(view, 0, "Reverse search failed.");
	    failures |= REVERSE;
	    textview_WantUpdate(view, view);
	    return;
	}
	failures = 0;
	textview_SetDotPosition(view, pos);
	textview_SetDotLength(view, search_GetMatchLength());
    }
    textview_FrameDot(view, pos);
    textview_WantUpdate(view, view);
    message_DisplayString(view, 0, "Done.");
    return;
}


static int GetPattern(view, text, direction)
    struct textview *view;		/* textview we're using */
    struct text *text;		/* and doc */
    int direction;			/* direction of the search */
{

    char string[MAXSTRING];
    int pos, len;
    int useLast = FALSE;
    int lastCmdWasSearch;
    int lastSearchFailed = FALSE;

    pos = textview_GetDotPosition(view);
    len = textview_GetDotLength(view);
    lastCmdWasSearch = im_GetLastCmd(textview_GetIM(view)) == lcSearch;
    if (lastCmdWasSearch)
	lastSearchFailed = (failures & direction);
    /* Now figure out what pattern to use. */
    if (lastCmdWasSearch && !lastSearchFailed)
	useLast = TRUE;
    else if (lastCmdWasSearch || len == 0) {

        char prompt[MAXSTRING + sizeof("Reverse search for : ")];

        if (*lastString != '\0')
            sprintf(prompt, "%s for [%s] : ", (direction == FORWARD) ? "Search" : "Reverse search", lastString);
        else
            sprintf(prompt, "%s for: ", (direction == FORWARD) ? "Search" : "Reverse search");
	if (message_AskForString(view, 0, prompt, NULL, string, 100) < 0)
	    return -1;
	if (string[0] == 0)
	    useLast = TRUE;
    }
    else {
	/* Use the current selection. */

	char unquotedString[MAXSTRING];

	if (len >= sizeof(unquotedString) - 1) {
	    message_DisplayString(view, 0, "Search string too long - continuing with truncating string.");
	    len = sizeof(unquotedString) -1;
	}
	    
	text_CopySubString(text, pos, len, unquotedString, FALSE);
	if (search_GetQuotedSearchString(unquotedString, string, sizeof(string)) == NULL) {
	    message_DisplayString(view, 0, "Search string too long - continuing with truncating string.");
	}
	    
	if (strcmp(string, lastString) == 0)
	    useLast = TRUE;
    }
    if (!useLast) {

        char *errorMessage;

	failures = 0;
	errorMessage = search_CompilePattern(string, &lastPattern);
	if (errorMessage != NULL) {
	    message_DisplayString(view, 0, errorMessage);
	    return -1;
	}
	strcpy(lastString, string);
    }
    im_SetLastCmd(textview_GetIM(view), lcSearch);
    return 0;
}

void dynsearch_SearchAgain(self)
struct textview *self;
{
    struct text *d = (struct text *)textview_GetDataObject(self);
    long	savePos, pos;

    if (lastPattern != NULL) {
        savePos = pos = textview_GetDotPosition(self);

	if (lastDirection == FORWARD) {
            pos = textview_CollapseDot(self);
            pos = search_MatchPattern(d, pos, lastPattern);
        }
        else {
            textview_SetDotLength(self, 0);
            if (pos > 0)
		textview_SetDotPosition(self, --pos);
            pos = search_MatchPatternReverse(d, pos, lastPattern);
        }
        if (pos < 0)
	{
            message_DisplayString(self, 0, "Search failed.");
	    if ( !lastDirection )
        	textview_SetDotPosition(self, savePos);
	}
        else {
            textview_SetDotPosition(self,pos);
            textview_SetDotLength(self, search_GetMatchLength());
            textview_FrameDot(self,pos);
            textview_WantUpdate(self, self);
        }
    }
    else
        message_DisplayString(self, 0, "Must have searched at least once to search again.");
}

void dynsearch_SearchAgainOpposite(self)
struct textview *self;
{
    if (lastDirection == FORWARD) {
	lastDirection = REVERSE;
    }
    else {
	lastDirection = FORWARD;
    }
    dynsearch_SearchAgain(self);
}

boolean dynsearch__InitializeClass(classID)
    struct classheader *classID;
{
    struct classinfo *textviewClassinfo;

    lcSearch = im_AllocLastCmd();
    lastPattern = NULL;
    lastString[0] = 0;

    textviewClassinfo = class_Load("textview");
    proctable_DefineProc("dynsearch-search-forward", (procedure) dynsearch_SearchForward, textviewClassinfo, NULL,
                         "Search forward for a pattern; uses selection.");
    proctable_DefineProc("dynsearch-search-reverse", (procedure) dynsearch_SearchReverse, textviewClassinfo, NULL,
                         "Search backwards for a pattern; uses selection.");
    proctable_DefineProc("dynsearch-search-again", (procedure) dynsearch_SearchAgain, textviewClassinfo, NULL,
                         "Search again in the same direction, using the last search pattern.");
    proctable_DefineProc("dynsearch-search-again-opposite", (procedure) dynsearch_SearchAgainOpposite, textviewClassinfo, NULL,
                         "Search again in the opposite direction, using the last search pattern.");
    return TRUE;
}
