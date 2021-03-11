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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/strpself.c,v 2.20 1993/05/05 19:49:43 susan Exp $";
#endif

#include <andrewos.h>	/* <strings.h> */
#include <ms.h>
#include <parseadd.h>
#include <mail.h>

extern char Me[], MeInFull[], MyMailDomain[], *MyShortAddress;

static int SameName(n1, n2, nlen, namesep)
char *n1, *n2; int nlen, namesep;
{/* Works like ULstrncmp, but treats spaces and namesep as the same. */
    int c1, c2;

    for (; nlen > 0; --nlen) {
	c1 = *n1++;
	c2 = *n2++;
	/* convert UC to LC */
	if (c1 < 'Z' && c1 > 'A') c1 -= ('a' - 'A');
	if (c2 < 'Z' && c2 > 'A') c2 -= ('a' - 'A');
	/* convert namesep chars to spaces */
	if (namesep > 0) {
	    if (c1 == namesep) c1 = ' ';
	    if (c2 == namesep) c2 = ' ';
	}
	if (c1 != c2) return 0;
    }
    /* they're equivalent. */
    return 1;
}

static void StripList(AddrList)
PARSED_ADDRESS *AddrList;
{/* Recursively strip Me@MyMailDomain from the list. */
    int outType, la_errcode, nameSep;
    char *IDpart, *PostID, *s;

    nameSep = CheckAMSNameSep(MyMailDomain);
    FOR_ALL_ADDRESSES(tmpaddr, AddrList, {
		       switch (tmpaddr->Kind) {
			   case SIMPLE_ADDRESS:
			       IDpart = PostID = NULL;
			       la_errcode = la_KindDomain(tmpaddr, &outType, &IDpart, &PostID, MyMailDomain);
			       if (la_errcode != laerr_NoError) break;
			       if (outType == latype_LocalID || outType == latype_LocalName) {
				   if (IDpart == NULL) break;
				   if (ULstrcmp(IDpart, Me) == 0) {
				       /* We could do a zillion heuristics in the LocalName case. instead, we just do the most obvious, specifically to catch canonical addresses on non-AMS delivery configurations */
				       /* You might think these next two lines are necessary, but it turns out that FreeAddress does it for us automatically, according to CFE.  I've left them in so that nobody else will make the same mistake I did.  -- NSB */
				       /*	tmpaddr->Prev->Next = tmpaddr->Next; */
				       /*	tmpaddr->Next->Prev = tmpaddr->Prev; */
				       FreeAddress(tmpaddr);
				   } else if (outType == latype_LocalName) {
				       s = strrchr(MyShortAddress, '@');
				       if (s != NULL && *s == '@') {
					   if ((strlen(IDpart) == (s-MyShortAddress)) && SameName(MyShortAddress, IDpart, (s-MyShortAddress), nameSep) && ULstrcmp(s+1, MyMailDomain) == 0) {
					       FreeAddress(tmpaddr);
					   }
				       }
				   }
				   free(IDpart);
			       } else if (outType == latype_Remote && tmpaddr->Hosts->Next != tmpaddr->Hosts && tmpaddr->Hosts->Next->Next == tmpaddr->Hosts) {
				   s = strrchr(MyShortAddress, '@');
				   if (s != NULL && *s == '@' && ULstrcmp(s+1, tmpaddr->Hosts->Next->Name) == 0) {
				       /* TODO: bonus points for checking useridplusworks */
				       if ((strlen(tmpaddr->LocalPart) == (s-MyShortAddress)) && SameName(MyShortAddress, tmpaddr->LocalPart, (s-MyShortAddress), nameSep)) {
					   FreeAddress(tmpaddr);
				       }
				   }
			       }
			       break;
			   case GROUP_ADDRESS:
			       StripList(tmpaddr->Members);
			       break;
			   default:
			       break;
		       }
		       })
}

StripMyselfFromAddressList(Old, New)
char *Old, **New;
{
    PARSED_ADDRESS *AddrList;
    int Newsize, Dummy;

    if (!Old || ParseAddressList(Old, &AddrList) != PA_OK) {
	return(-1);
    }
    StripList(AddrList);
    Newsize = 2*strlen(Old);
    *New = malloc(Newsize);
    if (!*New) {
	FreeAddressList(AddrList);
	return(-1);
    }
    if (UnparseAddressList(AddrList, UP_SPACES_TO_DOTS, *New, Newsize, "", "	", 69, &Dummy) != PA_OK) {
	free(*New);
	FreeAddressList(AddrList);
	return -1;
    }
    FreeAddressList(AddrList);
    return(0);
}
