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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/utility/RCS/ltapp.c,v 1.4 1993/01/08 16:36:06 rr2b R6tape $";
#endif


#include <andrewos.h>
#include <stdio.h>
#include <class.h>
#include <im.ih>
#include <frame.ih>
#include <message.ih>
#include <view.ih>
#include <environ.ih>
#include <lpair.ih>
#include <text.ih>
#include <textv.ih>
#include <lt.ih>
#include <ltv.ih>
#include <ltapp.eh>
#include <sys/types.h>
#include <zip.ih>

static boolean debug=FALSE;

boolean 
ltapp__InitializeObject( classID, self )
  register struct classheader		 *classID;
  register struct ltapp			 *self;
  {
  IN(ltapp_InitializeObject);
  *(self->trace_stream_name) = 0;
  *(self->trace_raster_name) = 0;
  ltapp_SetMajorVersion( self, 1 );
  ltapp_SetMinorVersion( self, 0 );
  OUT(ltapp_InitializeObject);
  return TRUE;
  }

boolean
ltapp__ParseArgs( self, argc, argv )
  register struct ltapp			 *self;
  register int				  argc;
  register char				**argv;
  {
  IN(ltapp_ParseArgs);
  while ( *++argv )
    {
    DEBUGst(ARGV,*argv);
    if ( **argv == '-' )
      {
      if ( strcmp( *argv, "-d" ) == 0 )
        debug = 1;
      else  printf( "LightTable: Unrecognized switch '%s'\n", *argv );
      }
      else
      {
      if ( *(self->trace_stream_name) == 0 )
        strcpy( self->trace_stream_name, *argv );
	else
        if ( *(self->trace_raster_name) == 0 )
          strcpy( self->trace_raster_name, *argv );
	  else  printf( "LightTable: Unrecognized argument '%s'\n", *argv );
      }
    }
  OUT(ltapp_ParseArgs);
  return TRUE;
  }

boolean 
ltapp__Start( self )
  register struct ltapp			 *self;
  {
  IN(ltapp_Start);
  if( !super_Start(self) )
    return FALSE;
  self->im = im_Create( NULL );
  if ( !self->im )
    {
    fprintf(stderr,"lighttable: Failed to create new window; exiting.\n");
    return(FALSE);
    }
  if((self->lt = lt_New()) == NULL) {
      fprintf(stderr,"lighttable: Could not allocate enough memory; exiting.\n");
      return(FALSE);
  }
  lt_Set_Debug( self->lt, debug );
  if ( self->trace_stream_name[0]  && self->trace_raster_name[0]  )
    if ( lt_Read_Visuals( self->lt, self->trace_stream_name, self->trace_raster_name ) )
      return(FALSE);
  if((self->ltview = ltv_New()) == NULL) {
      fprintf(stderr,"lighttable: Could not allocate enough memory; exiting.\n");
      return(FALSE);
  }
  ltv_Set_Debug( self->ltview, debug );
  ltv_SetDataObject( self->ltview, self->lt );
  if((self->frame = frame_New()) == NULL) {
      fprintf(stderr,"lighttable: Could not allocate enough memory; exiting.\n");
      return(FALSE);
  }
  frame_SetView( self->frame, self->ltview );
  im_SetView( self->im, self->frame );
  OUT(ltapp_Start);
  return TRUE;
  }

int
ltapp__Run(self)
  register struct ltapp			  *self;
  {
  IN(ltapp_Run);
  ltapp_Fork( self );
  ltv_WantInputFocus( self->ltview, self->ltview );
  DEBUG(...Interacting...);
  while ( im_Interact( 0 ) ) ;
  im_KeyboardProcessor();
  OUT(ltapp_Run);
  return(0);
  }
