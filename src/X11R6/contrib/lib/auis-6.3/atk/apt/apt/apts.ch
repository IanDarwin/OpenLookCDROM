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






/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Apt Tool Set

MODULE	apts.ch

VERSION	0.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Procedures that support the Apt Tool Set.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  05/18/89	Created (TCP)

END-SPECIFICATION  ************************************************************/

#define  apts_VERSION    1

package apts
  {

classprocedures:

  CompareStrings( char *s1, char *s2 )			returns long;
  SubstringIndex( char *pattern, char *string )		returns long;
  StripString( char *string)				returns char *;
  CaptureString( char *source, char *target )		returns long;

  HourMinuteSecond( long *hour, long *minute, long *second );
  HourOfDay( char *hour );
  MinuteOfHour( char *minute );
  SecondOfMinute( char *second );

  YearMonthDay( long *year, long *month, long *day )	returns long;

  DaysInMonth( long year, long month )			returns long;
  WeekDayOffset( long year, long month, long day )	returns long;

  };


/*
    $Log: apts.ch,v $
*Revision 1.6  1993/05/04  01:06:01  susan
*RCS Tree Split
*
*Revision 1.5.1.1  1993/02/02  00:43:02  rr2b
*new R6tape branch
*
*Revision 1.5  1992/12/14  23:20:33  rr2b
*add $Logs back after disclaimerization took them out
*
Revision 1.3  1991/09/12  19:20:03  bobg
Update copyright notice

Revision 1.2  1989/05/18  22:45:00  tom
Add more misc tool items.

Revision 1.1  89/05/18  20:31:13  tom
Initial revision

*/
