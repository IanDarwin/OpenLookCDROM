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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/readscr.c,v 2.16 1993/10/26 22:43:58 gk5g Exp $";
#endif


 

/* ** readscr.c  -  converts an ASCII file containing Scribe commands to
		 a base editor II document ** */


#include <andrewos.h> /* sys/file.h */
#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <class.h>
#include <im.ih>
#include <textv.ih>
#include <text.ih>
#include <print.ih>
#include <style.ih>
#include <stylesht.ih>
#include <envrment.ih>

#include <readscr.eh>

#define STACKSIZE 1000
#define STRINGSIZE 50
#define NORM 0
#define BEGEND 1
#define PT 2
#define SCRBEGIN "begin"
#define SCREND "end"

static struct environment *Par;
/* Next one is not used anywhere...
static struct environment *NewPar;
 */
static int Top, CurPos, Offset, PPos, Mode, End, LineFeeds, OldFormat;
static char Left[] = "{[(<\"'\0",  Right[] = "}])>\"'\0";
static struct StackItem {
    struct environment *Parent;
    struct style *StyleName;
    int Delimiter, Position, CheckMode;
} *Stack[STACKSIZE + 1];

static textfix();
static linefix();
static scribefix();
static longscribe();
static shortscribe();
static startenv();
static finishenv();

static goshdarn(errmsg)
char *errmsg;
{
    fprintf(stderr, "<warning:readscr>%s\n", errmsg);
}

boolean readscr__InitializeObject(classID) /* stupid convention */
    struct classheader *classID;
{
    return TRUE;
}

struct text *readscr__Begin(classID, d, pos, len, purge, version, GetTemplate)
struct classheader *classID;
struct text *d;
int pos, len, purge, GetTemplate;
char *version;
{
    if (GetTemplate && text_ReadTemplate(d, "scribe", 0)) {
	goshdarn("Couldn't read template.");
	return(0);
    }
    Par = d->rootEnvironment;
    Top = -1;
    CurPos = End = LineFeeds = 0;
    Offset = pos;
    PPos = CurPos;
    Mode = NORM;
/* Important note:  "yes" in lower case should be considered as equivalent to "2" */
    if (version && (!strcmp(version, "Yes"))) {
	OldFormat = 1;
    } else {
	OldFormat = 0;
    }
    End = pos + len;
    if (textfix(d, len)) {
	goshdarn("Error parsing text.");
	return(0);
    }
    if (purge) {
	text_AlwaysDeleteCharacters(d, pos, len);
    }
    return(d);
}

readscr__PrintFile(classID, filename, tv, d, Version, TrashWhenDone)
struct classheader *classID;
char *filename, *Version;
int TrashWhenDone;
struct textview *tv;
struct text *d;
{
    int fd;
    struct stat statbuf;
    char *s;

    text_Clear(d);
    if (stat(filename, &statbuf)
    || (s = malloc(1+statbuf.st_size)) == NULL
    || (fd = open(filename, (TrashWhenDone ? osi_O_READLOCK : O_RDONLY), 0444)) < 0) {
	return(1);
    }
    if (TrashWhenDone && osi_ExclusiveLockNoBlock(fd)
     || (read(fd, s, statbuf.st_size) != statbuf.st_size)
     || (text_AlwaysInsertCharacters(d, 0, s, statbuf.st_size), 
        (readscr_Begin(d, 0, statbuf.st_size, 1, Version, 1)) != d)
     || print_ProcessView(tv, 1, 0, "Scribe-format-file", "")
     || (TrashWhenDone && unlink(filename))) {
	close(fd); /* Held open to now for locking purposes */
	return(1);
    } else {
	close(fd); /* Held open to now for locking purposes */
	return(0);
    }
}


/* ** textfix - reads chars in and handles them appropriately ** */
static textfix(d, len)
struct text *d;
int len;
{
    register int i, tmp;

    tmp = text_GetChar(d, (CurPos + Offset));
    while ((CurPos < len) /* && (tmp != -1) */) {
	if (tmp == -1) {
	    goshdarn("Text includes a -1 character; treating as normal text...");
	}
	switch (tmp) {
	    case '\n':
		++LineFeeds;
		++CurPos;
		break;

	    case '@':
		if (scribefix(d)) {
		    goshdarn("Error parsing scribe command.");
		    return(1);
		}
		break;

	    case '}':
	    case ']':
	    case ')':
	    case '>':
	    case '"':
	    case '\'':
		i = 0;
		while (Right[i] != '\0') {
		    if (tmp == Right[i]) {
			if (Top >= 0 && Stack[Top]->Delimiter == Left[i]) {
			    switch (Stack[Top]->CheckMode) {
				case PT:
				case NORM:
				    if (text_GetChar(d,CurPos+Offset+1) == '\n') {
					++LineFeeds;
				    }
				    if (LineFeeds > 0) linefix(d);
				    if (finishenv()) {
					return(1);
				    }
				    break;

				case BEGEND:
				    if (LineFeeds > 0) linefix(d);
				    text_AddInCharacter(d, text_GetLength(d), tmp);
				    End++;
				    break;

				default:
				    break;
			    }
			    ++CurPos;
			    break;
			}
			else {
			    if (LineFeeds > 0) linefix(d);
			    text_AddInCharacter(d, text_GetLength(d), tmp);
			    ++End;
			    ++CurPos;
			    break;
			}
		    }
		    else {
			++i;
		    }
		}
		break;

	    default:
		if (LineFeeds > 0) linefix(d);
		text_AddInCharacter(d, text_GetLength(d), tmp);
		++End;
		++CurPos;
		break;
	}
	tmp = text_GetChar(d, (CurPos + Offset));
    }

    if (LineFeeds == 1) ++LineFeeds;
    if (LineFeeds > 0) linefix(d);
    while (Top > -1) {
	if (finishenv()) {
	    return(1);
	}
    }
    return(0);
}

/* ** linefix - if single \n, output space, if multiple \n's, output n-1 \n's ** */
static linefix(d)
struct text *d;
{
    if (OldFormat) {
	if (LineFeeds == 1) {
	    text_AddInCharacter(d, text_GetLength(d), ' ');
	    End++;
	    LineFeeds = 0;
	}
	else {
	    while (LineFeeds > 0) {
		text_AddInCharacter(d, text_GetLength(d), '\n');
		End++;
		--LineFeeds;
	    }
	    LineFeeds = 0;
	}
    }
    else {
	if (LineFeeds == 1) {
	    text_AddInCharacter(d, text_GetLength(d),' ');
	    End++;
	    LineFeeds = 0;
	}
	else {
	    while (LineFeeds > 1) {
		text_AddInCharacter(d, text_GetLength(d),'\n');
		End++;
		--LineFeeds;
	    }
	    LineFeeds = 0;
	}
    }
    return;
}

/* ** scribefix - deal with @commands ** */
static scribefix(d)
struct text *d;
{
    register int i, next, lowernext;
    char shortcommand[STRINGSIZE], realstring[STRINGSIZE];

    i = 0;
    ++CurPos;
    next = text_GetChar(d, (CurPos + Offset));
    switch (next) {
	case '@':
	    if (LineFeeds > 0) linefix(d);
	    text_AddInCharacter(d, text_GetLength(d), '@');
	    End++;
	    ++CurPos;
	    break;

	 case '*':
	    ++LineFeeds;
	    ++CurPos;
	    break;

	 default:
	    while ((next!='{')&&(next!='[')&&(next!='(')&&(next!='<')&&
		   (next!='"')&&(next!='\'')&&(next!=' ')&&(next!='}')&&
		   (next!=']')&&(next!=')')&&(next!='>')&&(next!='\n') &&
		   i < STRINGSIZE){
		lowernext = (isupper(next)) ? tolower(next) : next;
		realstring[i] = next;
		shortcommand[i++] = lowernext;
		++CurPos;
		next = text_GetChar(d, (CurPos + Offset));
	    }
	    shortcommand[i] = '\0';
	    realstring[i] = '\0';

	    if ((next == ' ') || (next == '}') || (next == ']') || (next == ')') ||
		(next == '>') || (next == '\n') || (i == STRINGSIZE)){
		if (LineFeeds > 0) linefix(d);
		text_AddInCharacter(d, text_GetLength(d),'@');
		text_AlwaysInsertCharacters(d, text_GetLength(d),realstring, strlen(realstring));
		End += strlen(realstring) +1;
	    }
	    else if ((strcmp(shortcommand,SCRBEGIN)==0)||(strcmp(shortcommand,SCREND)==0)){
		if (longscribe(d, shortcommand)) return(1);
	    }
	    else {
		if (shortscribe(d, shortcommand)) return(1);
	    }
	    break;
    }
    return(0);
}

/* ** longscribe - deal with @begin and @end scribe environments ** */
static longscribe(d, shortcommand)
struct text *d;
char *shortcommand;
{
    register int i;
    struct style *tempstyle;
    register int next, delim, delim2;
    char longcommand[STRINGSIZE], dstr[2], dstr2[2], sname[STRINGSIZE];

    i = 0;
    delim = text_GetChar(d, (CurPos + Offset));

    dstr[0] = delim;
    dstr[1] = '\0';

    ++CurPos;
    next = text_GetChar(d, (CurPos + Offset));

    while((next!='}')&&(next!=']')&&(next!= ')')&&(next!='>')&&(next!='"')&&(next!='\'')) {
	if (isupper(next)) {
	    next = tolower(next);
	}
	longcommand[i++] = next;
	++CurPos;
	next = text_GetChar(d, (CurPos + Offset));
    }
    longcommand[i]='\0';

    delim2 = text_GetChar(d, (Offset + CurPos));
    CurPos++;
    dstr2[0] = delim2;
    dstr2[1] = '\0';
    Mode = BEGEND;

    if (strlen(longcommand) == 0) {
	goshdarn("Environment field empty.");
	return(1);
    }

    if (strcmp(longcommand, "b") == 0) strcpy(sname, "bold");
    else if (strcmp(longcommand, "i") == 0) strcpy(sname, "italic");
    else if (strcmp(longcommand, "u") == 0) strcpy(sname, "underline");
    else strcpy(sname, longcommand);

    tempstyle = stylesheet_Find(d->styleSheet, sname);
    if (strcmp(shortcommand, SCRBEGIN) == 0) {
	if (tempstyle) {
	    if (startenv(delim, tempstyle)) {
		return(1);
	    }
	}
	else {
	    Mode = PT;
	    if (LineFeeds > 0) linefix(d);
	    text_AddInCharacter(d, text_GetLength(d),'@');
	    text_AlwaysInsertCharacters(d, text_GetLength(d),SCRBEGIN, 1);
	    text_AlwaysInsertCharacters(d, text_GetLength(d),dstr, strlen(dstr));
	    text_AlwaysInsertCharacters(d, text_GetLength(d),longcommand, strlen(longcommand));
	    text_AlwaysInsertCharacters(d, text_GetLength(d),dstr2, strlen(dstr2));
	    End += strlen(dstr2) + strlen(longcommand) + strlen(dstr) + 2;
	}
    }
    else if (strcmp(shortcommand, SCREND) == 0) {
	if (Top >= 0 && Stack[Top]->CheckMode == BEGEND) {
	    if ((tempstyle) || (strcmp(style_GetName(Stack[Top]->StyleName),style_GetName(tempstyle)) == 0)) {
		if (LineFeeds > 0) linefix(d);
		if (finishenv()) {
		    return(1);
		}
	    }
	    else {
		if (LineFeeds > 0) linefix(d);
		text_AddInCharacter(d, text_GetLength(d), '@');
		text_AlwaysInsertCharacters(d, text_GetLength(d),SCREND, 1);
		text_AlwaysInsertCharacters(d, text_GetLength(d),dstr, strlen(dstr));
		text_AlwaysInsertCharacters(d, text_GetLength(d),longcommand, strlen(longcommand));
		text_AlwaysInsertCharacters(d, text_GetLength(d),dstr2, strlen(dstr2));
		End += strlen(dstr2) + strlen(longcommand) + strlen(dstr) + 2;
		if (finishenv()) {
		    return(1);
		}
	    }
	}
	else if (Top >= 0 && Stack[Top]->CheckMode == PT) {
	    if (LineFeeds > 0) linefix(d);
	    text_AddInCharacter(d, text_GetLength(d), '@');
	    text_AlwaysInsertCharacters(d, text_GetLength(d),SCREND, 1);
	    text_AlwaysInsertCharacters(d, text_GetLength(d),dstr, strlen(dstr));
	    text_AlwaysInsertCharacters(d, text_GetLength(d),longcommand, strlen(longcommand));
	    text_AlwaysInsertCharacters(d, text_GetLength(d),dstr2, strlen(dstr2));
	    End += strlen(dstr2) + strlen(dstr) + strlen(longcommand) + 2;
	    if (finishenv()) {
		return(1);
	    }
	}
	else {
	    goshdarn("Unmatched delimiters.");
	    return(1);
	}
    }
    else {
	goshdarn("Using longscribe for a shortscribe command.");
	return(1);
    }
    return(0);
}


/* ** shortscribe - deal with all other environments ** */
static shortscribe(d, shortcommand)
struct text *d;
char *shortcommand;
{
    struct style *tempstyle;
    int delim;
    char dstr[2], sname[STRINGSIZE];

    delim = text_GetChar(d, (Offset + CurPos));
    CurPos++;
    dstr[0] = delim;
    dstr[1] = '\0';

    if (strcmp(shortcommand, "b") == 0) strcpy(sname, "bold");
    else if (strcmp(shortcommand, "i") == 0) strcpy(sname, "italic");
    else if (strcmp(shortcommand, "u") == 0) strcpy(sname, "underline");
    else strcpy(sname, shortcommand);

    if (tempstyle = stylesheet_Find(d->styleSheet, sname)) {
	Mode = NORM;
	if (startenv(delim, tempstyle)) {
	    return(1);
	}
    }
    else {
	Mode = PT;
	if (LineFeeds > 0) linefix(d);
	text_AddInCharacter(d, text_GetLength(d),'@');
	text_AlwaysInsertCharacters(d, text_GetLength(d),shortcommand, strlen(shortcommand));
	text_AlwaysInsertCharacters(d, text_GetLength(d),dstr, strlen(dstr));
	End += strlen(dstr) + strlen(shortcommand) + 1;
    }
    return(0);
}

/* ** startenv - add an environment to the stack ** */
static startenv(delim, tempstyle)
int delim;
struct style *tempstyle;
{
    int rpos;

    if (Top++ >= STACKSIZE) {
	goshdarn("Environment stack overflow.");
	return(1);
    }
    Stack[Top] = (struct StackItem *)malloc(sizeof(struct StackItem));
    Stack[Top]->Parent = Par;
    Stack[Top]->StyleName = tempstyle;
    Stack[Top]->Delimiter = delim;
    Stack[Top]->Position = End;
    Stack[Top]->CheckMode = Mode;

    rpos = End - PPos;
    Par = environment_InsertStyle(Par, rpos, tempstyle, 1);
    environment_SetStyle(Par, FALSE, FALSE);
    PPos = End;
    return(0);
}

/* ** finishenv - remove an environment from the stack ** */
static finishenv()
{
    int length;

    if (Top <= -1) {
	goshdarn("Environment stack overflow.");
	return(1);
    }
    length = End - PPos;
    environment_SetLength(Par, length);
    Par = Stack[Top]->Parent;
    if (--Top >= 0) PPos = Stack[Top]->Position;
    else PPos = 0;
    free(Stack[Top+1]);
    return(0);
}

