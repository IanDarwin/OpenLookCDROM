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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/txtvcsrch.c,v 1.9 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <ctype.h>
#include <txtvcmds.h>
#include <text.ih>
#include <im.ih>
#include <message.ih>
#include <search.ih>
#include <environ.ih>
#include <mark.ih>

#define AUXMODULE 1
#include <textv.eh>

/* Search command statics. */

#define SRCHSTRLEN 100
char *lastPattern = NULL;
static char searchString[SRCHSTRLEN] = "";
boolean forwardSearch = TRUE; /* TRUE if last search was forward. */

boolean Quoted(doc, pos)
struct text *doc;
long pos;
{
    /* returns true iff the character at pos is quoted (ie. "\"). Takes into account the slash being quoted. (ie "\\"). */

    boolean retval = FALSE;

    pos--;
    while (text_GetChar(doc, pos) == '\\' && pos > 0) {
        retval = !retval;
	pos--;
    }

    return retval;
}

int textview_SearchCmd(self)
register struct textview *self;
{
    char defSrchString[SRCHSTRLEN], *tp, messageBuf[120], *prompt;
    int pos = 0, gf, ct;
    register int j;
    register struct text *d;
    boolean defaultExists = FALSE;

    d = Text(self);
    ct = im_Argument(textview_GetIM(self));

    if (*searchString != '\0'){
	defaultExists = TRUE;
	strcpy(defSrchString, searchString);
    }
    if (defaultExists) {
	sprintf(messageBuf, "Search for [%s] : ", defSrchString);
	prompt = messageBuf;
    }
    else
	prompt = "Search for: ";
    gf = message_AskForString(self, 0, prompt, NULL, searchString, sizeof(searchString));
    if (gf < 0) return(-1);
    if (defaultExists && *searchString == '\0')
	strcpy(searchString, defSrchString);
    textview_SetDotPosition(self,
       textview_GetDotPosition(self)+textview_GetDotLength(self));
    if (!defaultExists || *searchString != '\0') {
	tp = search_CompilePattern(searchString,&lastPattern);
	if (tp != 0) {
	    message_DisplayString(self, 0, tp);
	    return(-2);
	}
    }
    j = 0;
    while (j<ct) {
        pos = search_MatchPattern(d,
            textview_GetDotPosition(self),lastPattern);
	if (pos < 0) {
            textview_SetDotLength(self,0);
            message_DisplayString(self, 0, "Search failed");
            return(-3);
        }
        textview_SetDotPosition(self,pos);
        textview_SetDotLength(self, search_GetMatchLength());
        j++;
    }
    textview_FrameDot(self,pos);
    textview_WantUpdate(self, self);
    forwardSearch = TRUE;
    return(0);
}

int textview_RSearchCmd(self)
register struct textview *self;
{
    int ct, gf, orgpos, pos = 0;
    register int j;
    register struct text *d;
    char defSrchString[SRCHSTRLEN], *tp, messageBuf[130], *prompt;
    boolean defaultExists = FALSE;

    d = Text(self);
    orgpos = textview_GetDotPosition(self);
    ct = im_Argument(textview_GetIM(self));
    if (*searchString != '\0'){
	defaultExists = TRUE;
	strcpy(defSrchString, searchString);
    }
    if (defaultExists) {
	sprintf(messageBuf, "Reverse search for [%s] : ", defSrchString);
	prompt = messageBuf;
    }
    else
	prompt = "Reverse search for: ";
    gf=message_AskForString(self, 0, prompt, NULL, searchString, sizeof(searchString));
    if (gf < 0)
        return -1;
    if (defaultExists && *searchString == '\0')
	strcpy(searchString, defSrchString);
    textview_SetDotLength(self,0);
    if (orgpos > 0) textview_SetDotPosition(self,orgpos-1);
    if (!defaultExists || *searchString != '\0') {
	tp = search_CompilePattern(searchString,&lastPattern);
	if (tp != 0) {
	    message_DisplayString(self, 0, tp);
	    return -2;
	}
    }
    j=0;
    while (j<ct) {
        pos = search_MatchPatternReverse(d,
           textview_GetDotPosition(self),lastPattern);
	if (pos < 0) {
            textview_SetDotPosition(self,orgpos);
            message_DisplayString(self, 0, "Search failed");
            return -3;
        }
        textview_SetDotPosition(self,pos);
        textview_SetDotLength(self, search_GetMatchLength());
        j++;
    }
    textview_FrameDot(self,pos);
    textview_WantUpdate(self, self);
    forwardSearch = FALSE;
    return 0;
}

void textview_SearchAgain(self)
struct textview *self;
{
    struct text *d = Text(self);
    long	savePos, pos;

    if (lastPattern != NULL) {
        savePos = pos = textview_GetDotPosition(self);

	if (forwardSearch) {
	    pos = textview_GetDotPosition(self) + textview_GetDotLength(self);
            textview_SetDotPosition(self, pos);
            textview_SetDotLength(self, 0);
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
	    if ( !forwardSearch )
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

void textview_SearchAgainOppositeCmd(self)
    register struct textview *self;
{
    forwardSearch	^= TRUE;
    textview_SearchAgain(self);
}

void textview_QueryReplaceCmd(self)
struct textview *self;
{
    boolean defaultExists = FALSE;
    boolean keepAsking = TRUE;
    boolean keepRunning = TRUE;
    boolean returnPosition = TRUE;
    char *lastPattern = NULL;
    char *prompt = NULL;
    char *searchError = NULL;
    char casedString[SRCHSTRLEN];
    char promptBuf[SRCHSTRLEN +20];
    int numFound = 0;
    int numReplaced = 0;
    int replacementLen = 0;
    int reply = 0;
    long fencePos = 0;
    long originalLength = 0;
    long originalPos = 0;
    long pos = 0, lastpos = -1;
    long searchPos = 0;
    static char replacement[SRCHSTRLEN] = "";
    static char defSrchString[SRCHSTRLEN] = "";
    static int expertReplace = -999999;
    struct mark *area = NULL;
    struct text *d = NULL;

    if (ConfirmReadOnly(self))
	return;
    casedString[0] = '\0';
    promptBuf[0] = '\0';
    if (expertReplace == -999999)
	expertReplace = environ_GetProfileSwitch("ExpertMode", 0);

    d = Text(self);

    if (*searchString != '\0'){
	defaultExists = TRUE;
	sprintf(defSrchString, "%s", searchString);
    }
    if (defaultExists) {
	sprintf(promptBuf, "Replace [%s] : ", defSrchString);
	if (*searchString == '\0')
	    sprintf(searchString, "%s", defSrchString);
	prompt = promptBuf;
    }
    else
	prompt = "Replace: ";

    if (message_AskForString(self, 0, prompt, NULL, searchString, sizeof(searchString)) < 0)
	return;
    if (defaultExists && *searchString == '\0')
	sprintf(searchString, "%s", defSrchString);

    prompt = "New string: ";
    if (message_AskForString(self, 0, prompt, NULL, replacement, sizeof(replacement)) < 0)
	return;
    replacementLen = strlen(replacement);

    if (!defaultExists || *searchString != '\0')
	if ((searchError = search_CompilePattern(searchString, &lastPattern)) != NULL) {
	    message_DisplayString(self, 0, searchError);
	    return;
	}

    originalPos = searchPos = textview_GetDotPosition(self);
    originalLength = textview_GetDotLength(self);

    area = (originalLength != 0) ? text_CreateMark(d, originalPos, originalLength) : NULL;

    if (searchPos < (fencePos = text_GetFence(d))) searchPos = fencePos;

    while ((pos = search_MatchPattern(d, searchPos, lastPattern)) >= 0
	    && keepRunning
	    && (area == NULL || pos < mark_GetPos(area) + mark_GetLength(area)))  {

	long matchLen = search_GetMatchLength();
	/* length can change between matches on RE searches */

	if (pos != lastpos) ++numFound;
	lastpos = pos;

	textview_SetDotPosition(self, pos);
	textview_SetDotLength(self, matchLen);
	textview_FrameDot(self, pos);

	if (keepAsking) {
	    textview_Update(self);
	    reply = im_GetCharacter(textview_GetIM(self));
	}
	else
	    reply = ' ';

	switch (reply) {
	    case 'q':
	    case '\007':
	    case EOF:		/* Interrupted im_GetChar */
		keepRunning = FALSE;
		break;
	    case '-': /* WHAT IS THIS?? -- GHOTI */
		{
		int i;

		if (isupper(text_GetChar(d, pos))) {
		    for (i = 1; i < matchLen; i++)
			if (islower(text_GetChar(d, pos + i))) break;
		    if (i >= matchLen) {
			for (i = 0; i < replacementLen; i++)
			    casedString[i] = (islower(replacement[i])) ? toupper(replacement[i]) : replacement[i];
			casedString[i] = '\0';
		    }
		    else { /* Upcase first letter. */
			for (i = 0; i < replacementLen; i++)
			    casedString[i] = replacement[i];
			casedString[i] = '\0';
			casedString[0] = (islower(replacement[0])) ? toupper(replacement[0]) : replacement[0];
		    }
		}
		else
		    strncpy(casedString, replacement, replacementLen);
		
		text_ReplaceCharacters(d, pos, matchLen, casedString, replacementLen);
		searchPos = pos + replacementLen;
		++numReplaced;
		}
		break;
	    case '!':
	    case '.':
	    case ',':
	    case '=':
		if (reply == '!')
		    keepAsking = FALSE;
		else {
		    keepRunning = FALSE;
		    if (reply == ',')
			returnPosition = FALSE;
		    else if (reply == '='){
			returnPosition = FALSE;
			searchPos = pos + matchLen;
			break;
		    }
		}
		/* Fall through. */
	    case ' ':
		text_ReplaceCharacters(d, pos, matchLen, replacement, replacementLen);
		searchPos = pos + replacementLen;
		++numReplaced;
		break;
	    case 'n':
		searchPos = pos + 1;
		break;
	    case 'r':
		if (expertReplace)  {
		    message_DisplayString(self, 0, "Recursive editing; ^C exits.");
		    im_KeyboardProcessor();
		    break;
		}
		/* Otherwise, fall through to the default */
	    default:
		message_DisplayString(self, 0, expertReplace ?
				      "One of ' ', '.', 'n', 'r', '!', 'q' '-' ',' '='" :
				      "One of ' ', '.', 'n', '!', 'q' '-' ',' '='");
		im_ForceUpdate();
		break;
	}

	if (keepAsking)
	    textview_Update(self);
    }
    if (numFound > 0)  {
	char messageString[100];
	if (returnPosition){
	    if (area != NULL) {
		textview_SetDotPosition(self, mark_GetPos(area));
		textview_SetDotLength(self, mark_GetLength(area));
	    } else {
		textview_SetDotPosition(self, originalPos);
		textview_SetDotLength(self, originalLength);
	    }
	    textview_FrameDot(self, originalPos);
	    text_NotifyObservers(d, observable_OBJECTCHANGED);
	}
	sprintf(messageString, "Found %d occurrences; replaced %d.",
		numFound, numReplaced);
	message_DisplayString(self, 0, messageString);
    }
    else
	message_DisplayString(self, 0, "No occurrences found.");

    if (area != NULL) {
	text_RemoveMark(d, area);
	mark_Destroy(area);
    }

    return;
}

struct paren_node {
    long type;
    struct paren_node *next;
};

long	skipToNextBalanceSymbol(doc, pos, direction)
struct text *doc;
long	    pos;
int	    direction;
{
    /*
     * skip to next paren, bracket, or brace, ignoring
     * quoted strings and comments.  Search direction specified in "direction"
     */
    boolean incomment = FALSE, instring = FALSE, doublestring;
    int thischar, nextChar;
    static char *opens = "({[", *closes = ")}]";
    long docLength = text_GetLength(doc);
    long limit;
    int	 increment;

    if ( pos < 0  || pos >= docLength )
	return EOF;
    if ( direction == FORWARD )
    {
	increment = 1;
	limit = docLength;
    }
    else
    {
	increment = -1;
	limit = 0;
    }
    while ( pos != limit )
    {
	pos += increment;
        thischar = text_GetChar(doc, pos);
	nextChar = (pos == limit) ? 0 : text_GetChar(doc, pos + increment);

	if (incomment) {
	    if(thischar == '*' && nextChar == '/') {
                incomment = FALSE;
	    }
        }
	else if (!Quoted(doc, pos)) {
            if (instring) {
		if ((thischar == '"' && doublestring) || (thischar == '\'' && !doublestring)) {
                    instring = FALSE;
		}
            }
            else if (thischar == '"') {
                instring = TRUE;
		doublestring = TRUE;
	    }
	    else if (thischar == '\'') {
		instring = TRUE;
		doublestring = FALSE;
	    }
	    else if (thischar == '/' && nextChar == '*') {
		incomment = TRUE;
	    }
	    else if ( (index(opens, thischar) != NULL ||
		       index(closes, thischar) != NULL) &&
		      !incomment && !instring )
		return pos;
	}
    }
    return EOF;
}

long balance(doc, pos)
struct text *doc;
long pos;
{
    /*
     * Returns the pos of the balancing symbol to the one
     * pointed to by the passed value of pos
     */
    char *parentype;
    struct paren_node *parenstack = NULL;
    static char *opens = "({[", *closes = ")}]";
    char    *closeTable, *openTable;

    if ( pos <= 0  || pos >= text_GetLength(doc) )
	return EOF;
    if ( index(opens, text_GetChar(doc, pos)) != NULL )
    {
	/* searching forward */
	openTable = opens;
	closeTable = closes;
    }
    else
	if ( index(closes, text_GetChar(doc, pos)) != NULL )
	{ 
	    /* searching backward */
	openTable = closes;
	closeTable = opens;
	}
    do
    {
	if ( (parentype = (char *) index(closeTable, text_GetChar(doc, pos))) != NULL )
	{
	    if ( parenstack == NULL || parenstack->type != (parentype - closeTable) )
		break;
	    else
	    {
		struct paren_node *temp = parenstack;

		parenstack = parenstack->next;
		free(temp);
		if (parenstack == NULL)
		    return pos; /* found matching symbol */
	    }
	}
	else if ((parentype = (char *) index(openTable, text_GetChar(doc, pos))) != NULL )
	{
	    struct paren_node *temp = NEW(struct paren_node);

	    temp->type = parentype - openTable;
	    temp->next = parenstack;
	    parenstack = temp;
	}
    } while ( parenstack != NULL && 
	    (pos = skipToNextBalanceSymbol(doc, pos, openTable == opens ? FORWARD : BACKWARD)) != EOF );

    return EOF;
}

void textview_BalanceCmd(self)
struct textview *self;
{
    register	struct text	*doc;
    long	pos, docLength;
    long	leftBalancedPos = EOF, rightBalancedPos = EOF;
    static 	char	balanceSymbols[] = "[{()}]";

    doc	= Text(self);
    docLength	= text_GetLength(doc);
    pos = textview_GetDotPosition(self);
    while ( pos < docLength)
    {
	if ( (pos =  skipToNextBalanceSymbol(doc, pos - 1, FORWARD)) == EOF )
	    break;
	if ( ((char *) index(balanceSymbols, text_GetChar(doc, pos))) - balanceSymbols > 2 )
	{
	    leftBalancedPos	= balance(doc, pos);
	    rightBalancedPos	= pos;
	    break;
	}
	else
	    if ( (pos = balance(doc, pos)) != EOF )
		pos++;
	    else
		break;
    }
    if ( leftBalancedPos == EOF || rightBalancedPos == EOF )
    {
	message_DisplayString(self, 0, "Sorry, could not balance.");
	return;
    }
    textview_SetDotPosition(self, leftBalancedPos);
    textview_SetDotLength(self, rightBalancedPos - leftBalancedPos + 1);
    textview_FrameDot(self, leftBalancedPos);
    textview_WantUpdate(self, self);
}


