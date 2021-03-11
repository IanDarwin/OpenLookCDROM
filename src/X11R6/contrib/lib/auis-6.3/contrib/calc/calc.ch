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

/*
    $Log: calc.ch,v $
*Revision 1.3  1993/05/04  01:40:10  susan
*RCS Tree Split
*
*Revision 1.2.1.1  1993/02/02  05:05:50  rr2b
*new R6tape branch
*
*Revision 1.2  1992/12/14  21:43:11  rr2b
*fixed disclaimerization
*.
*
# Revision 1.1  1992/10/05  20:07:19  susan
# Initial revision
#
Revision 1.3  1991/09/12  19:25:30  bobg
Update copyright notice

Revision 1.2  1989/08/04  17:09:31  tom
Accept keyboard inputs;
Suppress Shrink & Help Icons.

Revision 1.1  89/05/10  20:57:54  tom
Initial revision

*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Calc Data-object

MODULE	calc.ch

VERSION	1.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that suport the Calc Data-object.

HISTORY
  02/23/88	Created (TCP)

END-SPECIFICATION  ************************************************************/


#define  calc_VERSION    1

#define  calc_value_changed				1


class calc : apt
  {

overrides:

  Read( FILE *file, long id )				returns long;
  Write( FILE *file, long id, long level )		returns long;
  ViewName()						returns char *;

methods:

  SetValue( double value );

macromethods:

  Value()			      ((self)->value)

classprocedures:

  InitializeObject( struct calc *self )			returns boolean;
  FinalizeObject( struct calc *self );

data:

  long				      memory;
  double			      value;
  long				      id;
  };

