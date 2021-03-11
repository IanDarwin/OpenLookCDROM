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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/locnamex.c,v 2.7 1992/12/15 21:04:17 rr2b R6tape $";
#endif

/*
	locnamex.c -- Deallocator for low-level resolver of local addresses
*/

#include <stdio.h>
#include "mail.h"

void la_FreeMD(MD)
struct MailDom *MD;
{/* Somebody's about to erase the pointer MD. */

    if (MD != NULL && (--(MD->Refs)) == 0) {	/* Keep the reference count */
	if (MD->Next != NULL && MD->Prev != NULL) {
	    MD->Next->Prev = MD->Prev;
	    MD->Prev->Next = MD->Next;
	}
	if (MD->Orig != NULL) free(MD->Orig);
	if (MD->Final != NULL) free(MD->Final);
	if (MD->FwdPrefs != NULL) free(MD->FwdPrefs);
	if (MD->Fwds != NULL) {
	    if (MD->Fwds[0] != NULL) free(MD->Fwds[0]);
	    free(MD->Fwds);
	}
	free(MD);
    }
}
