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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/shr/RCS/utils.c,v 2.10 1993/07/08 18:36:08 Zarf Exp $";
#endif

#include <andrewos.h>
#include <stdio.h>
#include <ctype.h>
#include <cui.h> /* Just for debugging, could as easily be ms.h */

/* Random utilities used by BOTH cui and ms */

/* This is done with a char * declaration instead of a macro because it
	gets used so many times -- this makes the binaries smaller */

char *ErrDebugString = "Returning error codes %d %d %d\n";

/* This routine takes a string and returns a pointer to a version of it in which neither
	the beginning nor ending have white space.  It is destructive in the sense that
	the old string will also appear with the trailing white space deleted. */

char *
StripWhiteEnds(string)
char *string;
{
    char *stripped;
    int len;

    if (!string) return(string);
    for (stripped = string; *stripped && (*stripped == ' ' || *stripped == '\t' || *stripped == '\n'); ++stripped) {
	;
    }
    len = strlen(stripped) - 1;
    while (len>=0 && (stripped[len] == ' ' || stripped[len] == '\t' || stripped[len] == '\n')) {
	stripped[len] = '\0';
	--len;
    }
    return(stripped);
}

bone(buf, len)
register char *buf;
int len;
{
    register char *s;

    s = buf + len -1;
    while (s >= buf) *s-- = '\377';
}

ReduceWhiteSpace(string)
char *string;
{
    char *old = string, *new = string;
    int InWhite = 1;

    while (*old) {
	switch (*old) {
	    case ' ':
	    case '\t':
	    case '\n':
		if (InWhite) {
		    break;
		}
		*old = ' '; /* always should be a space */
		InWhite = 1;
		*new++ = *old;
		break;
	    default:
		InWhite = 0;
		*new++ = *old;
		break;
	}
	++old;
    }
    *new = '\0';
}


#define BIGDATESTR 250

/* like rindex, but finds any of chars in second arg */

char *
multrindex(s, t)
char *s, *t;
{
    char *u, *v;

    for (u = s; *u; ++u) {
	;
    }
    --u; /* Now points to last char in s */
    while (u >= s) {
	for (v = t; *v; ++v) {
	    if (*v == *u) {
		return(u);
	    }
	}
	--u;
    }
    return((char *) NULL);
}

#define MAGICNAME ".MESSAGES"  /* This matches .MESSAGES*  */

BuildNickName(FullName, NickName)
char   *FullName,
       *NickName;
{
    char   *s, *t;

    for (s = FullName; *s; ++s) {
	if (*s == '.' && !strncmp(s, MAGICNAME, sizeof(MAGICNAME) - 1))
	    break;
    }
    if (*s) {
	t = strchr(s, '/');
	if (t) {
	    strcpy(NickName, t+1);
	} else {
	    strcpy(NickName, FullName);
	}
    }
    else {
	strcpy(NickName, FullName);
    }
    for (s = NickName; *s; ++s) {
	if (*s == '/')
	    *s = '.';
    }
    return(0);
}

/* The following routine compares the first string, which is assumed
	to be all lower case, to a second string which is not, and
	returns 0 if they are a case insensitive match */

lc2strncmp(s1, s2, len)
char *s1, *s2;
int len;
{
    while (len && *s1 && *s2 && (*s1 == *s2 || *s1 == tolower(*s2))) {
	++s1; ++s2; --len;
    }
    return(len && (*s1 || *s2));
}

LowerStringInPlace(string, len)
char *string;
int len;
{
    while (len--) {
	if (isupper(*string)) {
	    *string = *string - 'A' + 'a';
	}
	++string;
    }
}

char *
NextAddress(add)
char *add;
{
    int parenlevel = 0, inquotes = 0;

    while (*add) {
	switch(*add) {
	    case '"':
		inquotes = !inquotes;
		break;
	    case '(':
		++parenlevel;
		break;
	    case ')':
		--parenlevel;
		break;
	    case ',':
		if (!inquotes && (parenlevel <= 0)) return(add);
		break;
	    default:
		break;
	}
	++add;
    }
    return(NULL);
}


/* Returns pointer to static storage describing the integer foo.  For small
integers, this is "zero", "one", and so forth; for integers outside the
range, it's a pointer to overwritable static storage.  If Capitalized is
non-zero, it's a pointer to overwritable "Zero", "One", or so forth. */

static char *Nms[] = {"zero", "one", "two", "three", "four", "five", "six",
"seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
"sixteen", "seventeen", "eighteen", "nineteen"};
static char *Tens[] = {"twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"};
static char NmBuff[22];

char *cvEng(foo, Capitalized, MaxToSpellOut)
int foo, Capitalized, MaxToSpellOut;
{
	if (foo < 0 || foo >= 100 || foo >= MaxToSpellOut) {
		sprintf(NmBuff, "%d", foo);
		return NmBuff;
	}
	if (foo < 20) {
	    strcpy(NmBuff, Nms[foo]);
	} else {
	    strcpy(NmBuff, Tens[(foo/10) - 2]);
	    if (foo%10) {
		strcat(NmBuff, "-");
		strcat(NmBuff, Nms[foo%10]);
	    }
	}
	if (Capitalized && islower(NmBuff[0])) NmBuff[0] = toupper(NmBuff[0]);
	return NmBuff;
}
