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

TITLE	The Sched Data-object Specification

MODULE	sched.ch

NOTICE	IBM Internal Use Only

DESCRIPTION
	This defines the suite of Methods that support the LightTable Data-object.

HISTORY
  01/20/88	Created (TCP)

END-SPECIFICATION  ************************************************************/

#define  sched_VERSION    1

class sched : dataobject [dataobj]
  {
overrides:

methods:

  Set_Debug( debug );

macromethods:


classprocedures:

  Create( char *stream_name )				returns struct sched *;
  InitializeObject( struct sched *self )		returns boolean;

data:

  struct zip			     *zip;
  struct zip_stream		     *stream;
  char				      stream_name[512];
  long				      id;
  };

