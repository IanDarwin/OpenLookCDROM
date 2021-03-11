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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/scan822.c,v 2.12 1993/05/04 00:51:42 susan Exp $";
#endif

/* scan822.c: routines to parse pieces of header lines. */

#include <andrewos.h> /* strings.h */
#include <stdio.h>
#include <ctype.h>

#ifndef _IBMR2
extern char *malloc(), *realloc();
#endif /* _IBMR2 */

#include <util.h>
#include "mail.h"

#define TRUE 1
#define FALSE 0

int BracketField(Hdr, FieldName, pBegin, pEnd, pLineBegin)
char *Hdr, *FieldName;
char **pBegin, **pEnd, **pLineBegin;
{/* Takes an RFC822 header in Hdr and the name of a field in FieldName.  If the given field name is in the given header, this procedure sets pBegin and pEnd to point to the text of the header (setting pBegin after the FieldName (with colon) and whitespace, and setting pEnd to point to the trailing newline of that field).  If pLineBegin is non-null, it is set to point to the beginning of the line containing the field.
Returns 1 if the header is found, 0 otherwise.
  */
    char *Finger, *Begin, *Dum;

    Finger = Hdr;
    while (*Finger != '\0' && *Finger != '\n') {
	if (ULstlmatch(Finger, FieldName)) {
	    Dum = &Finger[strlen(FieldName)];
	    for (;;) {
		while (*Dum == ' ' || *Dum == '\t') ++Dum;
		if (*Dum == '\n' &&
		    (*(Dum+1) == ' ' || *(Dum+1) == '\t') ) ++Dum;
		else break;
	    }
	    Begin = Dum;
	    do {		/* find end of a possibly multi-line header */
		Dum = index(Dum, '\n');
		if (Dum == NULL) break;
		++Dum;
	    } while (*Dum == ' ' || *Dum == '\t');
	    if (Dum == NULL) {	/* recover from malformed Hdr */
		Dum = &Finger[strlen(Finger)];
	    } else {
		--Dum;
	    }
	    if (Dum > Begin) {		/* don't return null fields */
		*pBegin = Begin;
		*pEnd = Dum;
		if (pLineBegin != NULL) *pLineBegin = Finger;
		return 1;
	    }
	}
	do {		/* skip to the next real header */
	    Finger = index(Finger, '\n');
	    if (Finger == NULL) return 0;
	    ++Finger;
	} while (*Finger == ' ' || *Finger == '\t');
    }
    return 0;
}

static char *tokPtr; static int tokCount, tokMax;

static int AddTok(ch)
char ch;
{/* Check for overflow to NextWord's RsltBuf. */
    if (tokCount >= tokMax) return 0;
    *tokPtr++ = ch; ++tokCount;
    return 1;
}

int IsOK822Atom(ch)
char ch;
{/* Return a Boolean saying whether this character is OK as an RFC822 ``atom'' constituent. */
    if (!isascii(ch)) return FALSE;		/* Must be seven-bit ASCII */
    if (ch != ' ' && iscntrl(ch)) return FALSE;		/* Must not be a CTL or a space */
    switch (ch) {	/* Check for RFC822 ``special'' characters */
	case '(': case ')': case '<': case '>': case '@':
	case ',': case ';': case ':': case '\\': case '"':
	case '.': case '[': case ']':
	    return FALSE;
	default:
	    return TRUE;
    }
}

int Next822Word(Line, LineEnd, RsltBuf, sizeRsltBuf)
char **Line, *LineEnd, *RsltBuf; int sizeRsltBuf;
{/* Scan the string pointed to by Line for the next non-comment token, which will either be a word or some kind of punctuation.  (Rejects imbedded NUL characters.)
    Copy it to RsltBuf and return the updated pointer via Line.  Return is822Atom, is822QuotedString, or is822Special in the appropriate cases.
    If the buffer size is exceeded, or there's a lexical error, return 0.
 */
    int CommentCount, Quoted;
    register char *Scan = *Line;

    tokPtr = RsltBuf; *tokPtr = '\0';
    tokCount = 0;
    tokMax = sizeRsltBuf - 1;	/* leave room for the NUL */
    for (; Scan < LineEnd; ++Scan) {
	if (*Scan == ' ' || *Scan == '\t' || *Scan == '\r' || *Scan == '\n') /* nothing */ ;
	else if (*Scan == '\0') return 0;
	else if (*Scan == '(') {
	    CommentCount = 1; Quoted = 0;
	    for (++Scan; Scan < LineEnd; ++Scan) {
		if (Quoted) {
		    Quoted = 0;
		} else {
		    if (*Scan == '(') ++CommentCount;
		    else if (*Scan == ')') {if (--CommentCount == 0) break;}
		    else if (*Scan == '\n' || *Scan == '\r') return 0;
		    else if (*Scan == '\\') Quoted = 1;
		}
	    }
	    if (CommentCount > 0 || Quoted) return 0;	/* unbalanced comment */
	} else break;
    }
    if (Scan >= LineEnd) {*Line = LineEnd; return is822End;}
    if (IsOK822Atom(*Scan)) {
	for (; Scan < LineEnd; ++Scan) {
	    if (IsOK822Atom(*Scan)) {
		if (! AddTok(*Scan)) return 0;
	    } else {
		break;
	    }
	}
	*tokPtr = '\0';
	*Line = Scan;	/* Save the character for next time */
	return is822Atom;
    } else if (*Scan == '"') {
	Quoted = 0;
	for (++Scan; Scan < LineEnd; ++Scan) {
	    if (Quoted) {
		if (! AddTok(*Scan)) return 0;
		Quoted = 0;
	    } else {
		if (*Scan == '"') {++Scan; break;}	/* Skip the terminal char */
		else if (*Scan == '\\') Quoted = 1;
		else if (*Scan == '\r' || *Scan == '\n') return 0;
		else if (! AddTok(*Scan)) return 0;
	    }
	}
	if (Quoted) return 0;	/* unbalanced quoted string */
	*tokPtr = '\0';
	*Line = Scan;
	return is822QuotedString;
    } else {	/* Return the special. */
	AddTok(*Scan);
	*tokPtr = '\0';
	++Scan;
	*Line = Scan;
	return is822Special;
    }
}

char *Next822LPart(Line, LineEnd, RsltBuf, sizeRsltBuf)
char *Line, *LineEnd, *RsltBuf; int sizeRsltBuf;
{/* Scan the string pointed to by Line for the next non-comment token, which will either be a local-part or some kind of punctuation.
    Copy it to RsltBuf and return the updated Line pointer.
    If the buffer size is exceeded, or on lexical errors, return NULL.
 */
    int SpaceLeft, ExpectWord, WordsSeen;
    char *This, *Next, *Space, *Out;

    Out = RsltBuf; ExpectWord = 1;
    Next = Line; Space = NULL;

    for (WordsSeen = 0; ; ++WordsSeen) {
	This = Next;
	SpaceLeft = sizeRsltBuf - (Out - RsltBuf);
	switch (Next822Word(&Next, LineEnd, Out, SpaceLeft)) {
	    case is822End:
		return Next;
	    case is822Atom: case is822QuotedString:
		if (ExpectWord == 0) return This;
		if (Space != NULL) *Space = ' ';
		Space = Out + strlen(Out);
		Out = Space + 1;
		ExpectWord = 0;
		break;
	    case is822Special:
		if (WordsSeen != 0) {
		    if (*Out == '.' && ExpectWord == 0) {
			if (Space != NULL) Out = Space;
			Space = NULL;
			*Out++ = '.';
			ExpectWord = 1;
		    } else {
			return This;
		    }
		} else {	/* first thing seen; return it */
		    return Next;
		}
		break;
	    case 0: default:
		return NULL;
	}
    }
}

char *Next822Phrase(Line, LineEnd, RsltBuf, sizeRsltBuf)
char *Line, *LineEnd, *RsltBuf; int sizeRsltBuf;
{/* Scan the string pointed to by Line for the next non-comment token, which will either be a phrase or some kind of punctuation.
    Copy it to RsltBuf and return the updated Line pointer.
    If the buffer size is exceeded, or on lexical errors, return NULL.
 */
    int SpaceLeft;
    char *Next, *Space, *Out;

    Out = RsltBuf;
    Next = Line; Space = NULL;

    for (;;) {
	SpaceLeft = sizeRsltBuf - (Out - RsltBuf);
	switch (Next822Word(&Next, LineEnd, Out, SpaceLeft)) {
	    case is822End:
		return Next;
	    case is822Atom: case is822QuotedString:
		if (*Space != '\0') *Space = ' ';
		Space = Out + strlen(Out);
		Out = Space + 1;
		break;
	    case is822Special:
		return Next;
	    case 0: default:
		return NULL;
	}
    }
}

char *Quote822LPart(Cleartext)
char *Cleartext;
{/* Return a copy of Cleartext, malloced, that is a quoted representation of Cleartext as an RFC822 Local-part.  Return NULL on malloc failure. */
    int NewLen;
    char *Src, *Dst, *Copy;

    NewLen = 0;
    for (Src = Cleartext; *Src != '\0'; ++Src) {
	if (*Src == '.') {
	    if (Src[1] == '.') ++NewLen;	/* multiple dots */
	} else {
	    if (!IsOK822Atom(*Src)) ++NewLen;
	}
    }
    if (NewLen == 0) {	/* nothing to quote */
	return NewString(Cleartext);
    } else {		/* must quote something */
	Copy = malloc(3+NewLen+strlen(Cleartext));
	if (Copy != NULL) {
	    Dst = Copy;
	    *Dst++ = '"';
	    for (Src = Cleartext; *Src != '\0'; ++Src) {
		if (*Src == '"' || *Src == '\\' || *Src == '\r') *Dst++ = '\\';
		*Dst++ = *Src;
	    }
	    *Dst++ = '"';
	    *Dst++ = '\0';
	}
	return Copy;
    }
}

char *Quote822Phrase(Cleartext)
char *Cleartext;
{/* Return a copy of Cleartext, malloced, that is a quoted representation of Cleartext as an RFC822 phrase.  Return NULL on malloc failure. */
    int NewLen;
    char *Src, *Dst, *Copy;

    NewLen = 0;
    for (Src = Cleartext; *Src != '\0'; ++Src) {
	if (*Src != ' ' && !IsOK822Atom(*Src)) ++NewLen;
    }
    if (NewLen == 0) {	/* nothing to quote */
	return NewString(Cleartext);
    } else {		/* must quote something */
	Copy = malloc(3+NewLen+strlen(Cleartext));
	if (Copy != NULL) {
	    Dst = Copy;
	    *Dst++ = '"';
	    for (Src = Cleartext; *Src != '\0'; ++Src) {
		if (*Src == '"' || *Src == '\\' || *Src == '\r') *Dst++ = '\\';
		*Dst++ = *Src;
	    }
	    *Dst++ = '"';
	    *Dst++ = '\0';
	}
	return Copy;
    }
}
