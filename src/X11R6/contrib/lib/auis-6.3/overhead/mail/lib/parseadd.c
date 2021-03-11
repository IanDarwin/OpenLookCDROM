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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/parseadd.c,v 2.22 1994/04/23 17:33:53 rr2b Exp $";
#endif

/*
		Subroutines for parsing mail addresses.
*/


#include <stdio.h>
#include <ctype.h>
#include <andrewos.h>
#include <parseadd.h>
#include "parsey.h"

typedef unsigned char bool;
#define FALSE	0
#define TRUE	1

#define NIL	0

extern char *StrCopy();


static int locallexer(lexerrock, lval)
struct parser *lexerrock;
YYSTYPE *lval;
{
    extern char mail_parseyytext[];
    extern YYSTYPE mail_parseyylval;
    int res;
    bcopy(lval, &mail_parseyylval, sizeof(mail_parseyylval));
    res=mail_parseyylex();
    bcopy(&mail_parseyylval, lval, sizeof(mail_parseyylval));
    return parser_TranslateTokenNumber(lexerrock, res);
}
/*
   Parse a string address list.  The addresses are represented
   by the chain of PARSED_ADDRESS structures starting at  AddrOut.
   If the return value is non-zero, it is a value indicating
   the reason for the failure (see parseadd.h).
*/

int ParseAddressList(AddrIn, AddrOut)
    char *AddrIn;
    PARSED_ADDRESS **AddrOut;
{
    extern int ParseErrorReason;
    extern PARSED_ADDRESS *yyparsedaddress;
    struct parser *p;
    /* Check for null pointer */
    if (AddrIn == NIL) return PA_NULL_POINTER;
    p=parsey_New();
    if(p==NULL) return -2;
    SetNextLine(AddrIn);
    pareset_lexer();
    parser_Parse(p, locallexer, p);
    parser_Destroy(p);
    if (yyparsedaddress == NIL)
	return ParseErrorReason;
    else {
	*AddrOut = yyparsedaddress;
	return PA_OK;
    }
}

/*
   Unparse a single address to a buffer in external
   ASCII format. The addresses are always written according to
   RFC822.  In addition, although comments are retained & printed
   out (on demand), they may not appear in their original positions.
   Note that this routine is *NOT* the inverse of ParseAddressList.
*/

int UnparseOneAddress(Addr, Mode, Buffer, Length, Prefix, LineLength)
    PARSED_ADDRESS *Addr;
    int Mode;
    char *Buffer;
    int Length;
    char *Prefix;
    int LineLength;
{
    extern PARSED_ADDRESS *MakeAddress(), *MakeAddrList();
    register PARSED_ADDRESS *Copy, *List;
    int code, dummy;

    /* Make a copy of the address & place it in a list by itself */
    Copy = MakeAddress(Addr->Kind, Addr->LocalPart);
    if (Copy == NIL) return PA_NO_MEM;
    *Copy = *Addr;	/* This overwrites the previous 2 fields */
    Copy -> Next = Copy;
    Copy -> Prev = Copy;
    List = MakeAddrList(Copy);
    if (List == NIL) return PA_NO_MEM;
    code = UnparseAddressList(List,
			      Mode,
			      Buffer,
			      Length,
			      NIL,
			      Prefix,
			      LineLength,
			      &dummy);
    free(List);
    free(Copy);
    return code;
}

static char DefaultPrefix[] = " ";
#define DefaultLineLength	80

/* Globals for unparsing */
static char *UP_NextPos, *UP_EndOfLastAddress, *UP_Prefix, *UP_EndOfHeader;
static int UP_PrefixLength, UP_BytesLeft, UP_MaxLineLength, UP_LineLength, UP_HowMany;
static int UP_HeaderLength;

/*
   Unparse an address list to a buffer in external
   ASCII format. The addresses are always written according to
   RFC822.  In addition, although comments are retained & printed
   out (on demand), they may not appear in their original positions.
   Note that this routine is *NOT* the inverse of ParseAddressList.
*/

int UnparseAddressList(AddrList,	/* The address list to unparse */
		       Mode,		/* Unparse format */
		       Buffer,		/* Buffer to unparse into */
		       Length,		/* Length of buffer */
		       Header,		/* Text for header */
		       Prefix,		/* Prefix for folded lines */
		       LineLength,	/* Max line length */
		       HowMany)		/* How many were unparsed */
    PARSED_ADDRESS *AddrList;
    int Mode;
    char *Buffer;
    int Length;
    char *Header;
    char *Prefix;
    int LineLength, *HowMany;
{
    int result;

    /* Check parameters & set defaults */
    if (AddrList == NIL || Buffer == NIL) return PA_NULL_POINTER;
    UP_HeaderLength = (Header != NIL ? strlen(Header) : 0);
    if (Length <= 0 || Length <= UP_HeaderLength) return PA_BAD_LEN;
    UP_BytesLeft = Length;

    /* If no prefix specified, choose one */
    UP_Prefix = Prefix;
    if (UP_Prefix == NIL)
	if (UP_HeaderLength == 0)
	    UP_Prefix = DefaultPrefix;
	else {
	    register char *c;
	    UP_Prefix = StrCopy(Header);	/* Bad hack to get storage */
	    if (UP_Prefix == NIL) return PA_NO_MEM;
	    for (c=UP_Prefix; *c!='\0'; c++) *c = ' ';
	}
    UP_PrefixLength = strlen(UP_Prefix);

    /* If no max line length specified, choose one */
    UP_MaxLineLength = LineLength;
    if (UP_MaxLineLength == 0) UP_MaxLineLength = DefaultLineLength;
    if (UP_MaxLineLength > 0 && UP_MaxLineLength <= UP_HeaderLength) return PA_BAD_LEN;

    /* Set up global variables & start unparse */
    bcopy(Header, Buffer, UP_HeaderLength);
    UP_LineLength = UP_HeaderLength;
    UP_NextPos = Buffer + UP_HeaderLength;
    UP_EndOfHeader = UP_NextPos;
    UP_HowMany = 0;
    result = StartUnparse(AddrList, Mode);
    *HowMany = UP_HowMany;
    if (UP_HeaderLength > 0) StrFree(UP_Prefix);
    return result;
}

static int StartUnparse(AddrList, Mode)
    PARSED_ADDRESS *AddrList;
    int Mode;
{
    bool First;

    UP_BytesLeft--;		/* Allow room for terminating '\0' */
    UP_EndOfLastAddress = UP_NextPos;
    First = TRUE;
    FOR_ALL_ADDRESSES(Addr, AddrList, {
	int result;

	result = UnparseAddress(Addr,
				Mode,
				(bool) (Addr->Next->Kind == DUMMY_ADDRESS),
				!First);

	/* See if it went okay */
	if (result != PA_OK) {
	    if (result == PA_TOO_LONG) {
		*UP_EndOfLastAddress = '\0';
		return PA_PARTIAL;
	    }
	    return result;
	}

	First = FALSE;
	UP_HowMany++;
    })

    /* Got through whole list */
    *UP_NextPos = '\0';
    return PA_OK;
}

static int UnparseAddress(Addr, Mode, Last, NewLine)
    PARSED_ADDRESS *Addr;
    int Mode;
    bool Last, NewLine;
{
    switch (Addr->Kind) {
	case SIMPLE_ADDRESS:	return UnparseSimpleAddress(Addr, Mode, Last, NewLine);
	case GROUP_ADDRESS:	return UnparseGroupAddress(Addr, Mode, Last);
	default:		return PA_UNK_ADDRESS;
    }
}

/*
   Task: get this address into the buffer as nicely as possible
*/

static int UnparseSimpleAddress(Addr, Mode, Last, NewLine)
    PARSED_ADDRESS *Addr;
    int Mode;
    bool Last, NewLine;
{
    int result;

    if (!Last) UP_BytesLeft--;	/* Leave room for separating , */
    result = PrintSimpleAddress(Addr, Mode);
    if (result != PA_OK) return result;

    /* Add trailing , */
    if (!Last) {
	*UP_NextPos++ = ',';
	UP_LineLength++;
    }

    /* See if ok on the current line */
    if (UP_LineLength > UP_MaxLineLength) Fold(NewLine);
    if (!Last) UP_EndOfLastAddress = UP_NextPos-1;		/* Point at , */

    /* Put in trailing ' ', if there's room */
    if (UP_LineLength < UP_MaxLineLength && UP_BytesLeft > 0 && !Last) {
	*UP_NextPos++ = ' ';
	UP_BytesLeft--;
	UP_LineLength++;
    }
    return PA_OK;
}

static int UnparseGroupAddress(Addr, Mode, Last)
    PARSED_ADDRESS *Addr;
    int Mode;
    bool Last;
{
    int result;
    char *BeforeGroup;

    UP_BytesLeft -= 2;		/* For ':', & ';' */
    if (!Last) UP_BytesLeft--;	/* Leave room for separating , */

    /* Dump group name */
    BeforeGroup = UP_EndOfLastAddress;	/* Save in case group doesn't make it */
    result = PrintRoutePhrase(Addr->LocalPart, Mode);
    if (result != PA_OK) return result;
    *UP_NextPos++ = ':';

    /* See if ok here */
    if (UP_LineLength > UP_MaxLineLength) Fold((bool) (UP_HowMany>0));
    UP_EndOfLastAddress = UP_NextPos-1;

    /* Put in trailing ' ' if there's room */
    if (UP_LineLength < UP_MaxLineLength && UP_BytesLeft > 0) {
	*UP_NextPos++ = ' ';
	UP_BytesLeft--;
	UP_LineLength++;
    }

    /* Now, dump each member */
    FOR_ALL_GROUP_MEMBERS(a, Addr, {
	result = UnparseSimpleAddress(a,
				      Mode,
				      (bool) (a->Next->Kind == DUMMY_ADDRESS),
				      TRUE);
	if (result != PA_OK) {
	    UP_EndOfLastAddress = BeforeGroup;
	    return result;
	}
    })

    /* Got all members, put in ';' */
    *UP_NextPos++ = ';';
    UP_BytesLeft--;
    UP_LineLength++;
    UP_EndOfLastAddress = UP_NextPos;

    /* Add trailing ',', if necessary */
    if (!Last) {
	*UP_NextPos++ = ',';
	UP_LineLength++;
    }
    /* Put in trailing ' ', if there's room */
    if (UP_LineLength < UP_MaxLineLength && UP_BytesLeft > 0 && !Last) {
	*UP_NextPos++ = ' ';
	UP_BytesLeft--;
	UP_LineLength++;
    }
    return PA_OK;
}

static bool SafeCopy(s)
    char *s;
{
    register int len;

    len = strlen(s);
    if (UP_BytesLeft < len) return FALSE;
    bcopy(s, UP_NextPos, len);
    UP_NextPos += len;
    UP_BytesLeft -= len;
    UP_LineLength += len;
    return TRUE;
}

#define IFPUTS(s)\
	if (SafeCopy(s))\
	    ;\
	else\
	    return PA_TOO_LONG

#define IFPUTC(c)\
	if (UP_BytesLeft >= 1) {\
	    *UP_NextPos++ = c;\
	    UP_BytesLeft--;\
	    UP_LineLength++;\
	} else\
	    return PA_TOO_LONG

PrintSimpleAddress(Addr, Mode)
    register PARSED_ADDRESS *Addr;
    int Mode;
{
    bool Comments;
    register int nhosts;
    int result;

    /* Dump route phrase if there is one */
    Comments = (Mode & UP_NO_COMMENTS) == 0;
    if (Addr->RoutePhrase != NIL && Comments) {
	PrintRoutePhrase(Addr->RoutePhrase, Mode);
	IFPUTC(' ');
    }

    /* Count the number of hosts */
    nhosts = 0;
    FOR_ALL_REVERSE_HOSTS(h, Addr, { nhosts++; })

    switch (nhosts) {
	case 0:		result = PrintWith0Hosts(Addr, Mode);
			break;
	case 1:		result = PrintWith1Host(Addr, Mode);
			break;
	default:	result = PrintWithManyHosts(Addr, Mode, nhosts);
    }
    if (result != PA_OK) return result;

    /* Now dump comments if desired */
    if (Comments) {
	register ADDRESS_COMMENT *c;
	for (c=Addr->Comments; c!=NIL; c=c->Next) {
	    IFPUTC(' ');
	    IFPUTS(c->Text);
	}
    }
    return PA_OK;
}

static PrintWith0Hosts(Addr, Mode)
    register PARSED_ADDRESS *Addr;
    int Mode;
{
    bool RoutePhrase;
    int result;

    /* Just print local part -- surround with <> if route phrase printed */
    RoutePhrase = (Addr->RoutePhrase != NIL && (Mode & UP_NO_COMMENTS) == 0);
    if (RoutePhrase) IFPUTC('<');
    result = PrintLocalPart(Addr->LocalPart, Mode);
    if (result != PA_OK) return result;
    if (RoutePhrase) IFPUTC('>');
    return PA_OK;
}

static PrintWith1Host(Addr, Mode)
    register PARSED_ADDRESS *Addr;
    int Mode;
{
    bool RoutePhrase;
    int result;

    RoutePhrase = (Addr->RoutePhrase != NIL && (Mode & UP_NO_COMMENTS) == 0);
    if (RoutePhrase) IFPUTC('<');
    result = PrintLocalPart(Addr->LocalPart, Mode);
    if (result != PA_OK) return result;
    IFPUTC('@');
    IFPUTS(Addr->Hosts->Next->Name);
    if (RoutePhrase) IFPUTC('>');
    return PA_OK;
}

static PrintWithManyHosts(Addr, Mode, Nhosts)
    register PARSED_ADDRESS *Addr;
    int Mode, Nhosts;
{
    IFPUTC('<');
    FOR_ALL_REVERSE_HOSTS(host, Addr, {
	if (Nhosts == 1) {
	    int result;
	    IFPUTC(':');
	    result = PrintLocalPart(Addr->LocalPart, Mode);
	    if (result != PA_OK) return result;
	    IFPUTC('@');
	    IFPUTS(host->Name);
	    IFPUTC('>');
	    return PA_OK;
	}
	IFPUTC('@');
	IFPUTS(host->Name);
	if (--Nhosts > 1) IFPUTC(',');
    })
}

static int Shift(Start, Dist)
    char *Start;
    int Dist;
{
    register char *From, *To;

    if (Dist > UP_BytesLeft) return PA_TOO_LONG;
    for (From=UP_NextPos-1, To=UP_NextPos+Dist-1; From>=Start; ) *To-- = *From--;
    UP_BytesLeft -= Dist;
    UP_NextPos += Dist;
    return PA_OK;
}

static DoFold(Break)
    char *Break;
{
    int Dist, result;
    char *AddrStart;

    Dist = UP_PrefixLength+1;	/* Distance to shift, including '\n' */
    AddrStart = Break;
    if (*Break == ' ') {
	AddrStart++;	/* Start after space */
	Dist--;		/* Save a char, we'll clobber space */
    }

    /* Shift adress to right to make room for fold characters (if there's room) */
    result = Shift(AddrStart, Dist);
    if (result != PA_OK) return result;

    /* Copy in folding characters */
    *Break++ = '\n';
    bcopy(UP_Prefix, Break, UP_PrefixLength);
    UP_LineLength = UP_NextPos - Break;
    return PA_OK;
}

static char *NextSpace(c)
    register char *c;
{
    for (; c<UP_NextPos; c++)
	switch (*c) {
	    case '\\':	c++;
			break;
	    case ' ':	return c;
	}
    return NIL;
}

static Fold(NewLine)
    bool NewLine;
{
    bool JustFolded;

    /* Start by folding it to next line, if desired */
    if (NewLine) {
	DoFold(UP_EndOfLastAddress+1);	/* +1 skips comma */
	JustFolded = TRUE;
    } else
	JustFolded = FALSE;

    /* If it fits, just bailout here */
    if (UP_LineLength <= UP_MaxLineLength) return;

    /* Still, too long, do it the hard way */
    while (UP_LineLength > UP_MaxLineLength) {
	register char *Last, *LastSpace, *PrevSpace;
	register int Count, LastCount = 0;

	/* Point to beginning of line */
	Last = UP_NextPos - UP_LineLength;
	if (JustFolded) {
	    Last += UP_PrefixLength;
	    Count = UP_PrefixLength;
	} else
	    /* Make sure that you don't back up to the header */
	    if (Last < UP_EndOfHeader) {
		Count = UP_HeaderLength;
		Last = UP_EndOfHeader;
	    } else
		Count = 0;

	PrevSpace = NIL;
	LastSpace = NIL;
	while (Count<=UP_MaxLineLength+1) {
	    register char *Next;

	    Next = NextSpace(Last);
	    if (Next == NIL) break;		/* No more spaces, just leave */
	    Count += Next - Last + 1;
	    Last = Next + 1;			/* Skip over this space */
	    PrevSpace = LastSpace;
	    LastSpace = Next;
	    LastCount = Count;
	}

	/* If folding at LastSpace leaves line short enough, go for it */
	JustFolded = TRUE;
	if (LastSpace != NIL && (LastCount <= UP_MaxLineLength+1 || PrevSpace == NIL))
	    DoFold(LastSpace);
	else
	    if (PrevSpace != NIL)
		DoFold(PrevSpace);
	    else
		break;
    }
}

static int QuoteAndPrint(String)
    register char *String;
{
    IFPUTC('"');
    for (; *String!='\0'; String++)
	switch (*String) {
	    case '"':
	    case '\\':
	    case '\n':	IFPUTC('\\');
	    default:	IFPUTC(*String);
	}
    IFPUTC('"');
    return PA_OK;
}

int Unquote(String)
    register char *String;
{
    register char *to;
    register bool InQuotes;

    for (to=String, InQuotes=FALSE; ;String++)
	switch (*String) {
	    case '\0':	if (InQuotes) return PA_SYNTAX_ERROR;
			*to = '\0';
			return PA_OK;
	    case '"':	InQuotes = !InQuotes;
			break;
	    case '\\':	if (InQuotes) String++;
	    default:	*to++ = *String;
	}
}

/*
   Spit out a route phrase in RFC822 format.  This may require
   quoting because we accept dots in route phrases.
*/

static int PrintRoutePhrase(Phrase, Mode)
    char *Phrase;
    int Mode;
{
    register char *c;
    register bool legal;

    /* See if legal phrase */
    for (legal=TRUE, c=Phrase; *c!='\0'&&legal; c++) {
	if (!isprint(*c)) legal = FALSE;
	else {
	    switch (*c) {
		case '"':
		    do if (*c++ == '\\') c++; while (*c!='"');
		    break;
		case '(': case ')': case ',': case '.': case ':': case ';':
		case '<': case '>': case '@': case '[': case '\\': case ']':
		    legal = FALSE;
		    break;
		default:
		    break;
	    }
	}
    }

    if (legal) {
	IFPUTS(Phrase);
	return PA_OK;
    }

    /* Illegal; we have to quote it */
    return QuoteAndPrint(Phrase);
}

/*
   Spit out a local part in RFC822 format.  This may require
   quoting because we accept spaces in route phrases.  The caller
   has 2 choices for what to do with any spaces that appear:

	Quote the whole thing if any spaces appear (the default)
	Turn a string of spaces into a '.' (set UP_SPACES_TO_DOTS)
*/

static int PrintLocalPart(Part, Mode)
    char *Part;
    int Mode;
{
    register char *c, last;
    register enum { LEGAL, ILLEGAL, MUSTQUOTE } status;

    /* See if any spaces */
    for (status=LEGAL, c=Part, last='\0'; status!=MUSTQUOTE && *c!='\0'; c++) {
	switch (*c) {
	    case '"':
		do if (*c++ == '\\') c++; while (*c!='"');
		break;
	    case '.':
		if (last == '\0' || last == ' ' || last == '.') {
		    status = MUSTQUOTE;
		    break;
		} else
		    status = ILLEGAL;
		break;
	    case ' ':
		if (last == '.')
		    status = MUSTQUOTE;
		else
		    status = ILLEGAL;
		break;
	    case '(': case ')': case ',': case ':': case ';':
	    case '<': case '>': case '@': case '[': case '\\': case ']':
		status = MUSTQUOTE;
	    default:
		break;
	}
	last = *c;
    }
    if (last == '.') status = MUSTQUOTE;

    if (status == LEGAL) {
	IFPUTS(Part);
	return PA_OK;
    }

    /* See what mode desired */
    if ((Mode & UP_SPACES_TO_DOTS) == 0 || status == MUSTQUOTE)
	return QuoteAndPrint(Part);

    /* Oh well, must turn spaces to dots */
    for (c=Part; *c!='\0'; c++)
	switch(*c) {
	    case '"':	do {
			    if (*c == '\\') IFPUTC(*c++);
			    IFPUTC(*c++);
			} while(*c != '"');
			IFPUTC('"');
			break;
	    case ' ':	/* Compress spaces & turn to dot */
			while (*c == ' ' || *c == '\t') c++;
			c--;	/* Back to last non-space */
	                IFPUTC('.');
			break;
	    default:	IFPUTC(*c);
	}

    return PA_OK;
}

int FreeHost(Host)
    ADDRESS_HOST *Host;
{
    RemHost(Host);
    if (Host->Name != NIL) StrFree(Host->Name);
    free(Host);
    return PA_OK;
}

static int FreeHosts(Addr)
    PARSED_ADDRESS *Addr;
{
    FOR_ALL_REVERSE_HOSTS(h, Addr, {
	int code;
	code = FreeHost(h);
	if (code != PA_OK) return code;
    })

    /* Free dummy header */
    free(Addr->Hosts);
    return PA_OK;
}

static int FreeComments(Addr)
    PARSED_ADDRESS *Addr;
{
    register ADDRESS_COMMENT *c, *Next;

    for (c=Addr->Comments; c!=NIL; c=Next) {
	Next = c -> Next;
	StrFree(c->Text);
	free(c);
    }
    return PA_OK;
}

static int FreeGroupMembers(Addr)
    PARSED_ADDRESS *Addr;
{
    return FreeAddressList(Addr->Members);
}

/*
   Free the space used by an address.  It is first removed
   from any chain it might be on.
*/

int FreeAddress(AddrIn)
    register PARSED_ADDRESS *AddrIn;
{
    register int code;

    if (AddrIn == NIL) return PA_NULL_POINTER;
    code = RemAddress(AddrIn);
    if (code != PA_OK) return code;

    switch (AddrIn->Kind) {
	case SIMPLE_ADDRESS:	StrFree(AddrIn->LocalPart);
				code = FreeHosts(AddrIn);
				if (code != PA_OK) return code;
				if (AddrIn->RoutePhrase != NIL)
				    StrFree(AddrIn->RoutePhrase);
				code = FreeComments(AddrIn);
				if (code != PA_OK) return code;
				if (AddrIn->MD != NIL) la_FreeMD(AddrIn->MD);
				break;
	case GROUP_ADDRESS:	StrFree(AddrIn->LocalPart);
				code = FreeGroupMembers(AddrIn);
	case DUMMY_ADDRESS:	break;
	default:		return PA_UNK_ADDRESS;
    }

    free(AddrIn);
    return PA_OK;
}

/*
   Free the space used by an address list.
*/

int FreeAddressList(Addrs)
    PARSED_ADDRESS *Addrs;
{
    FOR_ALL_ADDRESSES(addr, Addrs, {
	int code;
	code = FreeAddress(addr);
	if (code != PA_OK) return code;
    })

    FreeAddress(Addrs);	/* Free the head */
    return PA_OK;
}

/*
   Remove an address from a list or group of addresses.  Address
   must be on some list.
*/

int RemAddress(Addr)
    register PARSED_ADDRESS *Addr;
{
    register PARSED_ADDRESS *Save;

    Save = Addr -> Next;
    Save -> Prev = Addr -> Prev;
    Addr -> Prev -> Next = Save;
    Addr -> Next = NIL;
    Addr -> Prev = NIL;
    return PA_OK;
}

/*
   Remove a host from a host list -- the host must be on a
   list for this routine to work.
*/

int RemHost(Host)
    register ADDRESS_HOST *Host;
{
    register ADDRESS_HOST *Save;

    Save = Host -> Next;
    Save -> Prev = Host -> Prev;
    Host -> Prev -> Next = Save;
    Host -> Next = Host;
    Host -> Prev = Host;
    return PA_OK;
}

ADDRESS_HOST *MakeHost(name)
    char *name;
{
    register ADDRESS_HOST *host;

    host = (ADDRESS_HOST *) malloc(sizeof(ADDRESS_HOST));
    if (host == NIL) {
	NoStorage("MakeHost");
	return NIL;
    }
    host -> Name = name;
    host -> Next = host;
    host -> Prev = host;
    return host;
}

int AddHost(Addr, Host)
    PARSED_ADDRESS *Addr;
    ADDRESS_HOST *Host;
{
    Host -> Next = Host;
    Host -> Prev = Host;
    AppendHosts(Addr->Hosts, Host);
    return PA_OK;
}

/*
 * Replace the contents of the address Addr with the address
 * ReplaceWith.  ReplaceWith is freed.
 */

int ReplaceAddress(Addr, ReplaceWith)
    PARSED_ADDRESS *Addr, *ReplaceWith;
{
    int code;
    PARSED_ADDRESS tmpAddr;

    code = RemAddress(ReplaceWith);
    if (code != PA_OK) return code;

    tmpAddr = *ReplaceWith;

    *ReplaceWith = *Addr;
    ReplaceWith->Next = ReplaceWith->Prev = ReplaceWith;
    FreeAddress(ReplaceWith);

    Addr->Kind = tmpAddr.Kind;
    Addr->LocalPart = tmpAddr.LocalPart;
    Addr->Hosts = tmpAddr.Hosts;
    Addr->Members = tmpAddr.Members;
    Addr->RoutePhrase = tmpAddr.RoutePhrase;
    Addr->Comments = tmpAddr.Comments;
    Addr->MD = tmpAddr.MD;
    Addr->Extra = tmpAddr.Extra;

    return PA_OK;
}

PARSED_ADDRESS *SingleAddress(AddrList, pCount)
PARSED_ADDRESS *AddrList;
int *pCount;
{
/* Count the number of addressees are in the initial list */
/* Return some single simple address */
	PARSED_ADDRESS *Single = NULL, *First = NULL;

	FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
		switch (ThisAddr->Kind) {
	case SIMPLE_ADDRESS:
			(*pCount)++;
			Single = ThisAddr;
			break;
	case GROUP_ADDRESS:
			Single = SingleAddress(ThisAddr->Members, pCount);
			break;
	default:
			break;
		}
		if (!First) First = Single;
	})
	return(First);
}

