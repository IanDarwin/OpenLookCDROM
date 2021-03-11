

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/addrambig.c,v 1.3 1992/12/15 21:04:17 rr2b R6tape $";
#endif
#include <andrewos.h>

#ifdef WHITEPAGES_ENV
#include <wp.h>

/*
 * Returns wperr_NoError if "addr" is a sufficiently clear match for
 * the entry with primary key "PK" in the cell for "wpCD" to allow
 * delivery.  Returns wperr_TooManyKeysFound if it is not an
 * unambiguous match.  Returns some other error code if there was a
 * temporary failure.
 */

wp_ErrorCode AddressMatchesUnambiguously(wpCD, addr, PK)
struct wp_cd *wpCD;
char *addr;
char *PK;
{
    wp_SearchToken STok;
    wp_PrimeKey otherPK;
    wp_ErrorCode wpErr;
    int MinMatch, OutMatch, ReturnResult;

    wpErr = wp_SetUp(addr, LookupUIDWithLastPart, &STok);
    if (wpErr != wperr_NoError) {
	return wpErr;
    }

    wpErr = cwp_Lookup(wpCD, STok, &MinMatch, MatchAll, &OutMatch, &otherPK);
    wp_DeAllocate(STok);
    switch (wpErr)
      {
      case wperr_NoError:
	  break;

      case wperr_NoKeysFound:
      case wperr_TooManyKeysFound:
	  return wperr_TooManyKeysFound;

      default:
	  return wpErr;
      }

    if (strcmp(PK, otherPK)) {
	free(otherPK);
	return wperr_TooManyKeysFound;
    }

    if (OutMatch > MatchExPA || OutMatch == MatchAbPh || OutMatch == MatchAbOv || (OutMatch > MatchFirstNameAbbrev && OutMatch <= MatchNoHeuristics)) {
	free(otherPK);
	return wperr_TooManyKeysFound;
    }

    free(otherPK);
    return wperr_NoError;
}


#endif /* WHITEPAGES_ENV */
