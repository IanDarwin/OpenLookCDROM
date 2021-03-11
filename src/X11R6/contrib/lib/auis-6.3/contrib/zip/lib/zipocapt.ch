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
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/*
zipocapt.H

  03/31/88	Create for ATK (TCP)
*/

#define  zipocapt_VERSION    1


#define  Data				(self->header.zipobject.data_object)
#define  Env				(Data->env)
#define  View				(self->header.zipobject.view_object)
#define  Edit				(self->header.zipobject.edit_object)
#define  Print				(self->header.zipobject.print_object)


class zipocapt : zipobject[zipobj]
  {

overrides:

  Object_Icon()						returns char;
  Object_Icon_Cursor()					returns char;
  Object_Datastream_Code()				returns char;

  Build_Object( pane, action, x, y, clicks, X, Y )	returns long;
  Show_Object_Properties( pane, figure )		returns long;
  Draw_Object( object, pane )			        returns long;
  Clear_Object( object, pane )			        returns long;
  Print_Object( object, pane )			        returns long;
  Proximate_Object_Points( object, pane, x, y )	        returns long;
  Enclosed_Object( object, pane, x, y, w, h )		returns boolean;
  Object_Enclosure( object, pane, *x, *y, *w, *h )	returns long;
  Highlight_Object_Points( object, pane )	        returns long;
  Normalize_Object_Points( object, pane )	        returns long;
  Expose_Object_Points( object, pane )		        returns long;
  Hide_Object_Points( object, pane )		        returns long;
  Set_Object_Point( object, point, x, y )	        returns long;
  Adjust_Object_Point_Suite( object, x_delta, y_delta ) returns long;
  Set_Object_Font( object, font )			returns long;

  };

