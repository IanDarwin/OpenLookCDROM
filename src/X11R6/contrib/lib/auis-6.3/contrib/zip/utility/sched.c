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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/utility/RCS/sched.c,v 1.4 1993/01/08 16:36:51 rr2b R6tape $";
#endif


/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Sched Data-object Program

MODULE	sched.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the LightTable Data-object.

HISTORY
  10/10/88	Created (TCP)

END-SPECIFICATION  ************************************************************/

#include "sched.eh"
#include "zip.ih"
#include <errno.h>
static boolean debug=FALSE;
void
sched__Set_Debug( self, mode )
  register struct sched		     *self;
  {
  debug = mode;
  }

struct sched *
sched__Create( ClassID, stream_name )
  register struct  classheader	     *ClassID;
  register char			     *stream_name;
  {
  register struct sched		     *self;
  register long			      status;

  IN(sched_Create);
  if ( self = sched_New() )
    {
    if ( stream_name  &&  *stream_name )
      {
      strcpy( self->stream_name, stream_name );
      DEBUGst(Stream-name,self->stream_name);
      if ( status = zip_Open_Stream(self->zip, &self->stream,
				self->stream_name, NULL ) )
        { DEBUG(Open Failure);
        printf( "Schedule: Unable to Open %s\n", self->stream );
        }
        else  status = zip_Read_Stream( self->zip, self->stream );
      }
    }
  OUT(sched_Create);
  return  self;
  }


boolean
sched__InitializeObject( classID, self )
  register struct classheader	     *classID;
  register struct sched		     *self;
  {
  register long			      status = false;

  IN(sched_InitializeObject);
  self->stream = NULL;
  *self->stream_name = 0;
  if ( self->zip = zip_New() )
    status = true;
  OUT(sched_InitializeObject);
  return  status;
  }
