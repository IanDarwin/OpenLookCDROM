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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/parseutl.c,v 2.8 1992/12/15 21:04:17 rr2b R6tape $";
#endif

/*
		Utility subroutines for mail address parsing.
*/


#include <stdio.h>
#include "parseadd.h"

#define NIL	0

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

NoStorage(proc)
    char *proc;
{
    extern int ParseErrorReason;

    fprintf(stderr, "[PARSEADDR] Out of storage in routine %s\n", proc);
    ParseErrorReason = PA_NO_MEM;
}

char *StrCopy(old)
    char *old;
{
    char *new;

    new = (char *) malloc(strlen(old)+1);
    if (new == NIL) {
	NoStorage("StrCopy");
	return NIL;
    }
    strcpy(new, old);
    return new;
}

char *StrCat3(s1, s2, s3)
    char *s1, *s2, *s3;
{
    register char *new;

    new = (char *) malloc(strlen(s1)+strlen(s2)+strlen(s3)+1);
    if (new == NIL) {
	NoStorage("StrCat3");
	return NIL;
    }
    strcpy(new, s1);
    strcat(new, s2);
    strcat(new, s3);
    return new;
}

StrFree(s)
    char *s;
{
    free(s);
}

PARSED_ADDRESS *MakeAddress(kind, local)
    ADDRESS_KIND kind;
    char *local;
{
    register PARSED_ADDRESS *addr;

    addr = (PARSED_ADDRESS *) malloc(sizeof(PARSED_ADDRESS));
    if (addr == NIL) {
	NoStorage("MakeAddress");
	return NIL;
    }
    addr -> Kind = kind;
    addr -> LocalPart = local;
    addr -> Members = NIL;
    addr -> RoutePhrase = NIL;
    addr -> Comments = NIL;
    addr -> Next = addr;
    addr -> Prev = addr;
    addr -> MD = NIL;
    addr -> Extra = NIL;
    return addr;
}

ADDRESS_COMMENT *MakeComment(text)
    char *text;
{
    register ADDRESS_COMMENT *comment;

    comment = (ADDRESS_COMMENT *) malloc(sizeof(ADDRESS_COMMENT));
    if (comment == NIL) {
	NoStorage("MakeComment");
	return NIL;
    }
    comment -> Text = text;
    comment -> Next = NIL;
    return comment;
}

/*
   Append hosts on h2 to end of hosts on h1.  Both
   h1 & h2 are doubly-linked circular chains.
 */

ADDRESS_HOST *AppendHosts(h1, h2)
    register ADDRESS_HOST *h1, *h2;
{
    ADDRESS_HOST *temp;

    h1 -> Prev -> Next = h2;
    h2 -> Prev -> Next = h1;
    temp = h1 -> Prev;
    h1 -> Prev = h2 -> Prev;
    h2 -> Prev = temp;
    return h1;
}

/*
  Turn doubly-linked circular list into a list with
  a dummy header.
*/

ADDRESS_HOST *MakeHostList(h)
    ADDRESS_HOST *h;
{
    ADDRESS_HOST *Head;
    extern ADDRESS_HOST *MakeHost();

    Head = MakeHost(NIL);
    if (Head == NIL) return NIL;
    return (h != NIL ? AppendHosts(Head, h) : Head);
}

PARSED_ADDRESS *AppendAddresses(a1, a2)
    register PARSED_ADDRESS *a1, *a2;
{
    PARSED_ADDRESS *temp;

    if (a2 == NIL) return a1;
    if (a1 == NIL) return a2;
    a1 -> Prev -> Next = a2;
    a2 -> Prev -> Next = a1;
    temp = a1 -> Prev;
    a1 -> Prev = a2 -> Prev;
    a2 -> Prev = temp;
    return a1;
}

/*
  Turn doubly-linked circular list into a list with
  a dummy header.
*/

PARSED_ADDRESS *MakeAddrList(a)
    PARSED_ADDRESS *a;
{
    PARSED_ADDRESS *Head;

    Head = MakeAddress(DUMMY_ADDRESS, NIL);
    if (Head == NIL) return NIL;
    return (a != NIL ? AppendAddresses(Head, a) : Head);
}
