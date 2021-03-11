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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/canon.c,v 2.7 1992/12/15 21:03:27 rr2b R6tape $";
#endif

/*
	canon.c -- canonicalize a person's name
*/

#include <ctype.h>

#define Ctxt 0
#define Cnul 1
#define Cwsp 2
#define Cdot 3
static char CharClass[0200] = {
	Cnul, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0000 - 0007 */
	Ctxt, Cwsp, Cwsp, Cwsp, Cwsp, Cwsp, Ctxt, Ctxt,	/* 0010 - 0017 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0020 - 0027 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0030 - 0037 */
	Cwsp, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0040 - 0047 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Cdot, Ctxt,	/* 0050 - 0057 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0060 - 0067 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0070 - 0077 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0100 - 0107 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0110 - 0117 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0120 - 0127 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Cwsp,	/* 0130 - 0137 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0140 - 0147 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0150 - 0157 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt,	/* 0160 - 0167 */
	Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt, Ctxt };	/* 0170 - 0177 */
void CanonicalizePersonName(LocalPart)
char *LocalPart;
{/* Overwrites the storage in LocalPart with a canonicalized version--normalizing dots and spaces. */
/* Turns dots to spaces unless they look like an abbreviation. */
    char *SPtr, *DPtr, Chr;
    enum {Ignore, Copying, SeeDot, DblDot, EndScan, STOP} State;
    int Class;

    SPtr = DPtr = LocalPart;
    State = Ignore;
    while (State != STOP) {
	Chr = *SPtr++;
	Class = CharClass[toascii(Chr)];
	switch (State) {
	    case Ignore:
		if (Class == Cnul) State = STOP;
		else if (Class == Ctxt || Class == Cdot) {*DPtr++ = Chr; State = Copying;}
		break;
	    case Copying:
		if (Class == Ctxt) *DPtr++ = Chr;
		else if (Class == Cnul) State = STOP;
		else if (Class == Cwsp) State = EndScan;
		else State = SeeDot;
		break;
	    case SeeDot:
		if (Class == Ctxt) {*DPtr++ = ' '; *DPtr++ = Chr; State = Copying;}
		else if (Class == Cnul) {*DPtr++ = '.'; State = STOP;}
		else if (Class == Cwsp) {*DPtr++ = '.'; State = EndScan;}
		else State = DblDot;
		break;
	    case DblDot:
		if (Class == Ctxt) {*DPtr++ = '.';*DPtr++ = ' ';*DPtr++ = Chr; State = Copying;}
		else if (Class == Cnul) {*DPtr++ = '.'; State = STOP;}
		else if (Class == Cwsp) {*DPtr++ = '.'; State = EndScan;}
		break;
	    case EndScan:
		if (Class == Ctxt || Class == Cdot) {*DPtr++=' '; *DPtr++=Chr; State=Copying;}
		else if (Class == Cnul) State = STOP;
		break;
	}
    }
    *DPtr++ = '\0';	/* terminate it */
}
