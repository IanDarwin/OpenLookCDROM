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
zipstatus.H

  04/15/88	Created for ATK (TCP)
*/

#define  zipstatus_VERSION		    1

struct zipstatus_options
  {
  unsigned int				    xxxx		: 1;
  };

struct zipstatus_states
  {
  unsigned int				    xxxx		: 1;
  };


class zipstatus[zipstat] : zipview[zipv]
  {
overrides:

methods:

  Issue_Message( char *msg )					    returns long;
  Acknowledge_Message( char *msg )				    returns long;
  Clear_Message()						    returns long;
  Issue_Status_Message( char *msg )				    returns long;
  Acknowledge_Status_Message( char *msg )			    returns long;
  Issue_Figure_Status_Message( figure )				    returns long;
  Acknowledge_Figure_Status_Message( figure )			    returns long;
  Issue_Image_Status_Message( image )				    returns long;
  Acknowledge_Image_Status_Message( image )			    returns long;
  Issue_Stream_Status_Message( stream )				    returns long;
  Acknowledge_Stream_Status_Message( stream )			    returns long;
  Issue_Pane_Status_Message( pane )				    returns long;
  Acknowledge_Pane_Status_Message( pane )			    returns long;
macromethods:

classprocedures:

  InitializeObject( struct zipstatus *self )			    returns boolean;
  FinalizeObject( struct zipstatus *self );


data:

  struct zipstatus_options	 options;
  struct zipstatus_states	 states;
  };
