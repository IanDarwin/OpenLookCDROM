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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/rsndhdr.c,v 2.11 1992/12/15 21:20:51 rr2b R6tape $";
#endif

#include <andyenv.h>
#include <stdio.h>
#include <pwd.h>
#include <svcconf.h>
#include <util.h>
#include <ms.h>
#include <hdrparse.h>
#include <parseadd.h>
#include <mail.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif /* #ifdef WHITEPAGES_ENV */

extern PARSED_ADDRESS *SingleAddress();
extern char MyMailDomain[];

AuthenticReSentHeader(msg)
struct MS_Message *msg;
{
    char *s;
    int len, outType, numitems;
    PARSED_ADDRESS *AddrHead, *SingAddr;
    char *IDpart, *PostID;
    struct passwd *p;

    if (msg->AuthUid == 0 || msg->AuthCell == NULL) return (0);
    s = msg->ParsedStuff->HeadBody[HP_RESENTFROM];
    if (!s) {
	return(0);
    }
    len = msg->ParsedStuff->HeadBodyLen[HP_RESENTFROM];
    s[len] = '\0';
    if (ParseAddressList(s, &AddrHead) != PA_OK) {
	s[len] = '\n';
	return(0);
    }
    s[len] = '\n';
    numitems = 0;
    SingAddr = SingleAddress(AddrHead, &numitems);
    if (numitems != 1) {
	FreeAddressList(AddrHead);
	return(0);
    }
    if (la_KindDomain(SingAddr, &outType, &IDpart, &PostID, msg->AuthCell) != laerr_NoError) {
	FreeAddressList(AddrHead);
	return(0);
    }
    FreeAddressList(AddrHead);
    if (outType != latype_LocalID) {
	if (IDpart) free(IDpart);
	return(0);
    }
    p = getcpwuid(msg->AuthUid, msg->AuthCell);
    if (p == NULL || strcmp(p->pw_name, IDpart)) {
	if (IDpart) free(IDpart);
	return(0);
    }
    if (IDpart) free(IDpart);
    return(1);
}
