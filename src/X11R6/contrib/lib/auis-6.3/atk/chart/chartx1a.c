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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/chart/RCS/chartx1a.c,v 1.7 1992/12/15 21:29:20 rr2b R6tape $";
#endif

/*****************************************\

  Chart Programming Guide --  Example #1

\*****************************************/

#include  <andrewos.h>
#include  <im.ih>
#include  <frame.ih>
#include  <chart.ih>
#include  <chartv.ih>
#include  <chartx1a.eh>


#define  Chart		    (self->chart_data_object)
#define  ChartView	    (self->chart_view_object)
#define  Frame		    (self->frame)
#define  Im		    (self->im)

boolean 
chartx1app__InitializeObject( classID, self )
  struct classheader	 *classID;
  struct chartx1app	 *self;
  {
  chartx1app_SetMajorVersion( self, 0 );
  chartx1app_SetMinorVersion( self, 0 );
  Chart = chart_New();
  ChartView = chartv_New();
  return TRUE;
  }

boolean
chartx1app__Start( self )
  struct chartx1app	  *self;
  {
  long			   status = TRUE, mortgage, food, insurance,
			   entertainment, savings, education, vacation;

  super_Start( self );
  if ( Chart  &&  ChartView )
    {
    mortgage =	    Query( "Mortgage" );
    food =	    Query( "Food" );
    insurance =	    Query( "Insurance" );
    entertainment = Query( "Entertainment" );
    savings =	    Query( "Savings" );
    education =	    Query( "Education" );
    vacation =	    Query( "Vacation" );

    chart_SetChartAttribute( Chart, chart_Type( "Pie" ) );
    chart_SetChartAttribute( Chart, chart_TitleCaption( "Home Budget" ) );
    chart_SetItemAttribute( Chart, chart_CreateItem( Chart, "Mortgage", NULL ),
	chart_ItemValue( mortgage ) );
    chart_SetItemAttribute( Chart, chart_CreateItem( Chart, "Food", NULL ),
	chart_ItemValue( food ) );
    chart_SetItemAttribute( Chart, chart_CreateItem( Chart, "Insurance", NULL ),
	chart_ItemValue( insurance ) );
    chart_SetItemAttribute( Chart, chart_CreateItem( Chart, "Entertainment", NULL ),
	chart_ItemValue( entertainment ) );
    chart_SetItemAttribute( Chart, chart_CreateItem( Chart, "Savings", NULL ),
	chart_ItemValue( savings ) );
    chart_SetItemAttribute( Chart, chart_CreateItem( Chart, "Education", NULL ),
	chart_ItemValue( education ) );
    chart_SetItemAttribute( Chart, chart_CreateItem( Chart, "Vacation", NULL ),
	chart_ItemValue( vacation ) );
    if((Frame = frame_New()) == NULL) {
	fprintf(stderr,"Could not allocate enough memory.\nexiting.\n");
	exit(-1);
    }
    frame_SetView( Frame, ChartView );
    if((Im = im_Create(NULL)) == NULL) {
	fprintf(stderr,"Could not create new window.\nexiting.\n");
	exit(-1);
    }
    im_SetView( Im, Frame );
    chartv_SetDataObject( ChartView, Chart );
    chartv_WantInputFocus( ChartView, ChartView );
    return  status;
    }
    else 
    {
    printf( "ChartX1: Failed to Create objects\n" );
    return FALSE;
    }
  }


Query( topic )
  char			     *topic;
  {
  char			      response[255];

  printf( "Enter %s: ", topic );
  gets( response );
  return  atoi( response );
  }
