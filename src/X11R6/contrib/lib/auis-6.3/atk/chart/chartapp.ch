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

#ifndef lint
static char *rcsidchartapp_ch = "$Header $";
#endif

/*
    $Log: chartapp.ch,v $
*Revision 1.4  1993/05/04  01:11:11  susan
*RCS Tree Split
*
*Revision 1.3.1.1  1993/02/02  01:16:22  rr2b
*new R6tape branch
*
*Revision 1.3  1992/12/14  20:37:37  rr2b
*disclaimerization
*
Revision 1.2  1991/09/12  19:26:42  bobg
Update copyright notice

Revision 1.1  1989/04/28  22:44:33  tom
Initial revision

*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Chart Application-object

MODULE	chartapp.ch

VERSION	0.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that suport the Chart Application-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  02/23/88	Created (TCP)

END-SPECIFICATION  ************************************************************/

class chartapp: application [app]
  {
  classprocedures:
    InitializeObject( struct chartapp *)	    returns boolean;
    FinalizeObject();

  overrides:
   Start()				    returns boolean;
   ParseArgs( long argc, char **argv )	    returns boolean;

  data:
    char				    source[512];
    struct chart			   *chart_data_object;
    struct chartv			   *chart_view_object;
    struct im				   *im;
    struct frame			   *frame;
  };
