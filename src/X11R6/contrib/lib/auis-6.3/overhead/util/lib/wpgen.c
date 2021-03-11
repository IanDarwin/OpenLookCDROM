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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/wpgen.c,v 2.10 1992/12/15 21:11:36 rr2b R6tape $";
#endif

/* ************************************************************ *\
	wpgen.c
	The wp_Generate function.
	Include file ``wp.h'' declares the procedures for clients.
\* ************************************************************ */

#include <stdio.h>
#include <andrewos.h>		/* sys/file.h sys/types.h sys/time.h strings.h */
#include <ctype.h>
#include <errno.h>
#include <truth.h> /* itc.h -> truth.h DMT */
#include <sys/stat.h>
#include <pwd.h>
#include <util.h>
#ifdef WHITEPAGES_ENV  /* avoid makedepend "errors" */
#include <bt.h>
#include <wp.h>
#include <btwp.h>
#endif /* WHITEPAGES_ENV   */
extern int errno;
#if !POSIX_ENV
extern char *malloc();
extern char *realloc();
#endif /* _IBMR2 */

/*
To enumerate all the prime keys in the database, use wp_Generate(PKPtr).  Give this procedure a pointer to a null Prime Key and it will allocate the first PrimeKey and return it.  Give it back that PrimeKey and it will deallocate it, find the next one, and return the next one.  If there's no next one, it will return wperr_NoError and yet set your pointer to NULL.
*/
wp_ErrorCode cwp_Generate(cd, PKPtr)
struct wp_cd *cd; wp_PrimeKey *PKPtr;
{
    struct wp_CD *CD = (struct wp_CD *) cd;
    bt_ErrorCode BTErr;
    int LoopNum;
    char *EachKey, KeyBuf[PKLEN+3];
    static char PrimeTail[3] = {KeyTagSep, KeyTagR, '\0'};

    if (CD == NULL || CD->Tag != wpcdTag) return wperr_NotACD;
    for (LoopNum = 0; LoopNum < 5; ++LoopNum) {
	if (*PKPtr == NULL)
	    BTErr = bt_CursorToStart(CD->cursor);
	else if (strcmp(CD->PrevPK, *PKPtr) == 0) {
	    BTErr = bterr_NoError;
	} else {
	    strncpy(KeyBuf, *PKPtr, PKLEN);
	    strcpy(&KeyBuf[PKLEN], PrimeTail);
	    BTErr = bt_Search(CD->cursor, KeyBuf);
	}
	if (BTErr != bterr_NoError && wp_RetryThis(BTErr+wperr_BTreeBaseValue)) {
	    if (cwp_ReInitialize((struct wp_cd *) CD) != wperr_NoError) sleep(3);
	    continue;
	}
	if (BTErr != bterr_NoError) {CD->PrevPK[0] = '\0'; return w_BTreeErr(BTErr);}

	for (;;) {
	    BTErr = bt_NextCursor(CD->cursor);
	    if (BTErr == bterr_NoNextKey) {
		if (*PKPtr != NULL) {free(*PKPtr); *PKPtr = NULL;}
		CD->PrevPK[0] = '\0';
		return wperr_NoError;	/* end of the ride */
	    }
	    if (BTErr == bterr_NoError)
		BTErr = bt_GetCursorKey(CD->cursor, &EachKey);
	    if (BTErr != bterr_NoError) {
		CD->PrevPK[0] = '\0';
		if (wp_RetryThis(BTErr+wperr_BTreeBaseValue)) {
		    if (cwp_ReInitialize((struct wp_cd *) CD) != wperr_NoError) sleep(3);
		    goto MoreTries;
		}
		return w_BTreeErr(BTErr);
	    }
	    if (strcmp(&EachKey[strlen(EachKey) - 2], PrimeTail) == 0) break;
	    free(EachKey);
	}
	/* OK, we want to return this PK. */
	if (*PKPtr != NULL) {
	    if (strlen(*PKPtr) >= PKLEN) {	/* use the existing storage */
		strncpy(*PKPtr, EachKey, PKLEN);
		(*PKPtr)[PKLEN] = '\0';
		free(EachKey);
		strcpy(CD->PrevPK, *PKPtr);
		return wperr_NoError;
	    } else {free(*PKPtr); *PKPtr = NULL;}
	}
	EachKey[PKLEN] = '\0';	/* re-use stg from bt package */
	*PKPtr = EachKey;
	strcpy(CD->PrevPK, *PKPtr);
	return wperr_NoError;
      MoreTries:;
    }
    return w_BTreeErr(BTErr);
}
