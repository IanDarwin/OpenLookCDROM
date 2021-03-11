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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/chart/RCS/chartmap.c,v 1.8 1993/05/04 01:11:11 susan Exp $";
#endif

int chartmap_debug = 0;

#define debug chartmap_debug

/* $Header $ */
/* $Source $ */





/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Map Chart View-object

MODULE	chartmap.c

VERSION	0.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Chart View-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  03/28/89	Created (TCP)

END-SPECIFICATION  ************************************************************/

#include <stdio.h>
#include <sys/param.h>
#ifndef _IBMR2
#include <math.h>
#endif /* _IBMR2 */
#include <class.h>
#include <graphic.ih>
#include <observe.ih>
#include <environ.ih>
#include <view.ih>
#include <im.ih>
#include <rect.h>
#include <apt.h>
#include <aptv.ih>
#include <chartobj.ih>
#include <chart.ih>
#include <chartmap.eh>
#include <ctype.h>
#include <zip.ih>
#include <zipv.ih>

#define  Screen			1
#define  Paper			2

#define  Bounds			(chartmap_ChartBounds(self))
#define  Left			(chartmap_ChartLeft(self))
#define  Top			(chartmap_ChartTop(self))
#define  Width			(chartmap_ChartWidth(self))
#define  Height			(chartmap_ChartHeight(self))
#define  Right			(chartmap_ChartRight(self))
#define  Bottom			(chartmap_ChartBottom(self))

#define  PixelsPerUnit		(chartmap_PixelsPerUnit( self ))
#define  PixelsPerInterval	(chartmap_PixelsPerInterval( self ))

#define  Items			(chartmap_Items(self))
#define  ItemBounds(shadow)	(chartmap_ItemBounds(self,(shadow)))
#define  ItemLeft(shadow)	(chartmap_ItemLeft(self,(shadow)))
#define  ItemTop(shadow)	(chartmap_ItemTop(self,(shadow)))
#define  ItemWidth(shadow)	(chartmap_ItemWidth(self,(shadow)))
#define  ItemHeight(shadow)	(chartmap_ItemHeight(self,(shadow)))
#define  ItemCenter(shadow)	(chartmap_ItemCenter(self,(shadow)))
#define  ItemMiddle(shadow)	(chartmap_ItemMiddle(self,(shadow)))
#define  NextItem(shadow)	(chartmap_NextItem(self,(shadow)))

#define  Zip			(self->zip_data_object)
#define  ZipView		(self->zip_view_object)

boolean
chartmap__InitializeObject( classID, self)
  register struct classheader	 *classID;
  register struct chartmap	 *self;
  {
  register boolean		  status = TRUE;
  register FILE			 *file;
  char				  file_name[MAXPATHLEN];

  IN(chartmap_InitializeObject);
  chartmap_SetShrinkIcon( self, 'e', "icon12", "MapChart", "andysans10b" );
  chartmap_SetHelpFileName( self, "/usr/andy/help/ez.help"/*=== ===*/ );
  chartmap_SetChartOptions( self, chartobj_SuppressScales | chartobj_SuppressLabels );
  if ( Zip = zip_New() )
    {
    if ( ZipView = zipview_New() )
      {
      zipview_SetDataObject( ZipView, Zip );
      strcpy(file_name, environ_AndrewDir("/lib/zip/samples/states.zip"));
      printf("file_name: '%s'\n", file_name);
      if ( file = fopen( file_name, "r" ) )
	{
        if ( zip_Read( Zip, file, 1234 ) )
	  {
          status = FALSE;
/*===*/printf("ChartMap: Unable to Read Map-stream\n" );
	  }
        fclose( file );
	}
	else
	{
        status = FALSE;
/*===*/printf("ChartMap: Unable to Open Map-stream\n" );
	}
      }
      else
      {
      status = FALSE;
/*===*/printf( "ChartMap: Unable to Create ZipView Object\n" );
      }
    }
    else
    {
    status = FALSE;
/*===*/printf( "ChartMap: Unable to Create Zip Object\n" );
    }
  OUT(chartmap_InitializeObject);
  return  status;
  }

void 
chartmap__FinalizeObject( classID, self )
  register struct classheader   *classID;
  register struct chartmap	*self;
  {
  IN(chartmap_FinalizeObject);
  if ( Zip )	    zip_Destroy( Zip );
  if ( ZipView )    zipview_Destroy( ZipView );
  OUT(chartmap_FinalizeObject);
  }

void
chartmap__SetDebug( self, state )
  register struct chartmap	 *self;
  register char			  state;
  {
  IN(chartmap_SetDebug);
  super_SetDebug( self, debug = state );
  OUT(chartmap_SetDebug);
  }

struct view *
chartmap__HitChart( self, action, x, y, clicks )
  register struct chartmap	     *self;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  register zip_type_figure	      figure;
  char				      msg[512];

  IN(chartmap_HitChart);
/*===*/
  if ( action == view_LeftDown )
    {
    chartmap_Announce( self, "" );
    figure = zipview_Within_Which_Figure( ZipView,
		chartmap_EnclosedXToLocalX( ZipView, x ),
		chartmap_EnclosedYToLocalY( ZipView, y ) );
    sprintf( msg, "Figure '%s'", zip_Figure_Name( Zip, figure ) );
    chartmap_Announce( self, msg );
    }
  OUT(chartmap_HitChart);
  return  (struct view *) self;
  }

void
chartmap__DrawChart( self )
  register struct chartmap	     *self;
  {
  IN(chartmap_DrawChart);

  zipview_InsertViewSize( ZipView, self,
	    chartmap_BodyLeft(self)+4,  chartmap_BodyTop(self)+4,
	    chartmap_BodyWidth(self)-8, chartmap_BodyHeight(self) - 8 );
  zipview_FullUpdate( ZipView, view_FullRedraw, 0,0,
	    chartmap_BodyWidth(self)-8, chartmap_BodyHeight(self) - 8 );

  OUT(chartmap_DrawChart);
  }

void
chartmap__PrintChart( self )
  register struct chartmap	     *self;
  {
  IN(chartmap_PrintChart);
/*===*/
  OUT(chartmap_PrintChart);
  }


/*
    $Log: chartmap.c,v $
 * Revision 1.8  1993/05/04  01:11:11  susan
 * RCS Tree Split
 *
 * Revision 1.7.1.1  1993/02/02  01:17:33  rr2b
 * new R6tape branch
 *
 * Revision 1.7  1992/12/15  21:29:20  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.6  1992/12/14  23:20:33  rr2b
 * add $Logs back after disclaimerization took them out
 *
 * Revision 1.4  1992/08/25  19:40:53  rr2b
 * hacked to avoid global name conflicts
 * .
 *
 * Revision 1.3  1991/09/12  16:04:48  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.2  1990/06/13  18:54:40  gk5g
 * Fixed up the palette feature.  The palette used to use a matix view but it now used a suite.
 *
 * Revision 1.1  89/04/28  22:45:04  tom
 * Initial revision
 * 
*/
