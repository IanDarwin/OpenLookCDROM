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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/utility/RCS/schedapp.c,v 1.4 1993/01/08 16:36:51 rr2b R6tape $";
#endif


#include <andrewos.h>
#include <im.ih>
#include <frame.ih>
#include <message.ih>
#include <view.ih>
#include <environ.ih>
#include <lpair.ih>
#include <text.ih>
#include <textv.ih>
#include <sched.ih>
#include <schedv.ih>
#include <schedapp.eh>
#include <zip.ih>

static boolean debug=FALSE;


boolean 
schedapp__InitializeObject( classID, self )
  register struct classheader		 *classID;
  register struct schedapp		 *self;
  {
  IN(schedapp_InitializeObject);
  *(self->stream_name) = 0;
  OUT(schedapp_InitializeObject);
  return TRUE;
  }

boolean
schedapp__ParseArgs( self, argc, argv )
  register struct schedapp		 *self;
  register int				  argc;
  register char				**argv;
  {
  IN(schedapp_ParseArgs);
  while ( *++argv )
    {
    DEBUGst(ARGV,*argv);
    if ( **argv == '-' )
      {
      if ( strcmp( *argv, "-d" ) == 0 )
        debug = 1;
      else  printf( "Sched: Unrecognized switch '%s'\n", *argv );
      }
      else
      {
      if ( *(self->stream_name) == 0 )
        strcpy( self->stream_name, *argv );
      }
    }
  OUT(schedapp_ParseArgs);
  return TRUE;
  }

boolean 
schedapp__Start( self )
  register struct schedapp			 *self;
  {
  IN(schedapp_Start);
  if( !super_Start(self) )
    return FALSE;
  if((self->im  = im_Create(NULL)) == NULL) {
    fprintf(stderr,"sched: Failed to create new window; exiting.\n");
    return(FALSE);
  }
  if (*self->stream_name == 0 ) strcpy( self->stream_name,"itcCR.scd");
  if((self->sched = sched_Create( self->stream_name )) == NULL) {
      fprintf(stderr,"sched: Could not allocate enough memory; exiting.\n");
      return(FALSE);
  }
  sched_Set_Debug( self->sched, debug );
  if((self->schedview = schedv_New()) == NULL) {
      fprintf(stderr,"sched: Could not allocate enough memory; exiting.\n");
      return(FALSE);
  }
  schedv_Set_Debug( self->schedview, debug );
  schedv_SetDataObject( self->schedview, self->sched );
  if((self->frame = frame_New()) == NULL) {
      fprintf(stderr,"sched: Could not allocate enough memory; exiting.\n");
      return(FALSE);
  }
  frame_SetView( self->frame, self->schedview );
  im_SetView( self->im, self->frame );
  OUT(schedapp_Start);
  return TRUE;
  }

int
schedapp__Run(self)
  register struct schedapp			  *self;
  {
  IN(schedapp_Run);
  schedapp_Fork( self );
  schedv_WantInputFocus( self->schedview, self->schedview );
  DEBUG(...Interacting...);
  while ( im_Interact( 0 ) ) ;
  im_KeyboardProcessor();
  OUT(schedapp_Run);
  return(0);
  }
