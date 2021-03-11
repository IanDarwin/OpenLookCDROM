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
* $Header: /src/a2/contrib/calc/RCS/calcv.ch,v 1.1 1992/10/05 20:07:19 susan Exp rr2b $";
#endif

/*
    $Log: calcv.ch,v $
*Revision 1.3  1993/05/04  01:40:10  susan
*RCS Tree Split
*
*Revision 1.2.1.1  1993/02/02  05:05:59  rr2b
*new R6tape branch
*
*Revision 1.2  1992/12/14  21:44:00  rr2b
*fixed disclaimerization
*.
*
# Revision 1.1  1992/10/05  20:07:19  susan
# Initial revision
#
Revision 1.4  1991/09/12  19:25:33  bobg
Update copyright notice

Revision 1.3  1989/10/13  18:12:05  gk5g
Fix for Bug#64.  Calc captions not appearing on Sun3.  The problem was that the placement value for each caption was being stored in a char.  The possible range of values for the placement (from graphic.ch) is 0 - 0200.  The stored placement value was being clipped and then compared against a literal constant that exceeded that constant.  All captions were being drawn at x = 0 and were not visible because the ClipRect was being set.

Revision 1.2  89/08/04  17:09:35  tom
Accept keyboard inputs;
Suppress Shrink & Help Icons.

Revision 1.1  89/05/10  20:58:01  tom
Initial revision


*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Calcv Data-object

MODULE	calcv.ch

VERSION	1.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that suport the Calcv Data-object.

HISTORY
  02/23/88	Created (TCP)

END-SPECIFICATION  ************************************************************/

#define  calcv_VERSION		    1

struct calcv_states
  {
  unsigned int		   inputfocus		    : 1;
  unsigned int		   pending_display	    : 1;
  unsigned int		   pending_outline	    : 1;
  unsigned int		   point_present	    : 1;
  };

struct calcv_area_states
  {
  unsigned int		   highlighted		    : 1;
  };


struct calcv_area
  {
  char				     *string;
  struct fontdesc		     *font;
  struct rectangle		      bounds;
  char				      shape;
  int				      mode;
  void				    (*hit_handler)();
  struct calcv_setup		     *spec;
  struct calcv_area_states	      states;
  };


class calcv : aptv
  {
overrides:

  SetDataObject( struct calc *calc );
  FullUpdate( enum view_UpdateType type, long left, long top, long width, long height );
  Update();
  ObservedChanged( struct view *changed, long value );
  Hit( enum view_MouseAction action, long x, long y, long n )	returns struct view *;
  Print( FILE *file, char *processor, char *finalFormat, boolean topLevel );
  ReceiveInputFocus();
  LoseInputFocus();

methods:

classprocedures:

  InitializeClass()			    returns boolean;
  InitializeObject( struct calcv *self )    returns boolean;
  FinalizeObject( struct calcv *self );

data:

  struct calcv_states	  states;
  struct keystate	 *keystate;
  long			  area_count;
  struct calcv_area	  areas[20];
  struct rectangle	  bounds;
  char			  expression[50];
  char			  prior_expression[50];
  char			  operand_1[25], operand_2[25];
  char			  pending_op;
  };
