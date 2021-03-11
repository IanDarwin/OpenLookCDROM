/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


/* $Header $ */
/* $Source $ */





/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Suite-object

MODULE	suitecv.ch

VERSION: 0.0

AUTHOR	TC Peters & GW Keim
 	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Suite-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  11/01/88	Created (GW Keim)

END-SPECIFICATION  ************************************************************/

class suitecv: textview [textv] {
    classprocedures:
	InitializeClass() returns boolean;
	InitializeObject() returns boolean;
	FinalizeObject();
    overrides:
        FullUpdate(enum view_UpdateType type, long left, long top, long width, long height ) returns void;
	PostKeyState(struct keystate *kstate);
	ReceiveInputFocus();
	LoseInputFocus();
    data:
	struct keystate *kstate;
	struct suite_item *parent_item;
	struct suiteev *parent_EV;
	int debug;
};



/*
    $Log: suitecv.ch,v $
*Revision 1.7  1993/06/03  20:28:17  gk5g
*overrode FullUpdate so that we can update the Read/Write text and only change text if the caption has changed.
*
*Revision 1.6  1993/05/04  01:06:17  susan
*RCS Tree Split
*
*Revision 1.5.1.1  1993/02/02  00:45:20  rr2b
*new R6tape branch
*
*Revision 1.5  1992/12/14  23:20:33  rr2b
*add $Logs back after disclaimerization took them out
*
Revision 1.3  1991/09/12  19:20:30  bobg
Update copyright notice

Revision 1.2  1989/05/08  16:43:18  gk5g
changed references from suiteEV to suiteev

Revision 1.1  89/05/04  12:36:31  gk5g
Initial revision

Revision 1.1  89/04/28  20:26:19  tom
Initial revision

*/
