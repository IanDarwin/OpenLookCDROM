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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/utility/RCS/lt.c,v 1.4 1993/01/08 16:36:06 rr2b R6tape $";
#endif


/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The LightTable Data-object Program

MODULE	lt.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the LightTable Data-object.

HISTORY
  10/10/88	Created (TCP)

END-SPECIFICATION  ************************************************************/


#include "class.h"
#include "lt.eh"
#include "zip.ih"
#include "raster.ih"
#include <errno.h>

static boolean debug=FALSE;
#define  BackgroundWidth	    (self->background_width)
#define  BackgroundHeight	    (self->background_height)

void
lt__Set_Debug( self, mode )
  register struct lt		     *self;
  {
  debug = mode;
  zip_Set_Debug( self->zip, debug );
  }

boolean
lt__InitializeObject( classID, self )
  register struct classheader	     *classID;
  register struct lt		     *self;
  {
  IN(lt_InitializeObject);
  self->zip = zip_New();
  zip_Set_Debug( self->zip, debug );
  self->foreground_stream = NULL;
  *self->foreground_stream_name = 0;
  self->raster = raster_New();
  OUT(lt_InitializeObject);
  return TRUE;
  }

long
lt__Read_Visuals( self, foreground, background )
  register struct lt		     *self;
  register char			     *foreground, *background;
  {
  register long			      status = 0;
  register FILE			     *file;
  static char			     *source =
    "*G;-1000,1000\nLW0\n>1000,-1000";

  IN(lt_Read_Visuals);
  strcpy( self->foreground_stream_name, foreground );
  strcpy( self->background_raster_name, background );
  DEBUGst(Stream-name,self->foreground_stream_name);
  if ( status = zip_Open_Stream(self->zip, &self->foreground_stream,
				self->foreground_stream_name, NULL ) )
    { DEBUG(Open Failure);
    if ( status ==  zip_system_status_value_boundary + ENOENT )
      { DEBUG(Non-existent);
      if ( (status = zip_Create_Stream( self->zip,
			&self->foreground_stream, self->foreground_stream_name, 0 )) == zip_ok )
        status = zip_Set_Stream_Source( self->zip, self->foreground_stream, source );
      }
    }
    else
    status = zip_Read_Stream( self->zip, self->foreground_stream );
  if ( status == zip_ok )
    {
    if ( file = fopen( self->background_raster_name, "r" ) )
      {
      if ( raster_Read( self->raster, file, 12345 ) )
        { DEBUG(ERROR Reading Raster);
	printf( "LT: Error Reading Raster-file '%s'\n", self->background_raster_name );
	status = zip_failure;
        }
        else
	{ DEBUG(Success Reading Raster);
        self->background_width = raster_GetWidth( self->raster );
        DEBUGdt(BackgroundWidth,self->background_width);
        self->background_height = raster_GetHeight( self->raster );
        DEBUGdt(BackgroundHeight,self->background_height);
	}
      fclose( file );
      }
      else
      {
      status = zip_failure;
      printf( "LT: Error Opening Raster-file '%s'\n", self->background_raster_name );
      }
    }
    else printf( "LT: Error Reading Zip-file '%s'\n", self->foreground_stream_name );
  OUT(lt_Read_Visuals);
  return  status;
  }
