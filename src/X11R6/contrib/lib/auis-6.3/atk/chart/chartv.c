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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/chart/RCS/chartv.c,v 1.33 1993/11/18 02:36:04 gk5g Exp $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Chart View-object

MODULE	chartv.c

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
  03/23/89	Created (TCP)
  05/02/89	Accomodate SUN compiler casting (TCP)
  05/04/89	Add Line chart (TCP)
  05/07/89	Change lpair_VSplit to lpair_SetNth in ReChart (TCP)
  05/10/89	Eliminate FullUpdate after lpair_SetNth (unneeded) (TCP)
  05/21/89	Use sub-window ("Form" class) for Buttons (TCP)
  06/02/89	Announce that Delete feature not yet ready (TCP)
		Add Sort-by-Position to Palette
		Fix new chart creation displaying
  06/08/89	Set Desired dimensions to 300,300 (was 200,200) (TCP)
  07/20/89	Accomodate matrix-class interface change (TCP)
  08/24/89	Upgrade to Suite V1.0 interfaces (TCP)
  08/28/89	Create chartp.c to cope with "too many defines" (TCP)
  09/01/89	Upgrade to V1.0 (TCP)
  09/06/89	Suppress Palette til real (TCP)
  09/07/89	Suppress menu of non-supported Chart types til real (TCP)

END-SPECIFICATION  ************************************************************/


#include <andrewos.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <graphic.ih>
#include <view.ih>
#include <im.ih>
#include <frame.ih>
#include <rect.h>
#include <menulist.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <bind.ih>
#include <lpair.ih>
#include <text.ih>
#include <textv.ih>
#include <proctbl.ih>
#include <message.ih>
#include <apt.h>
#include <apts.ih>
#include <aptv.ih>
#include <suite.ih>
#include <chart.h>
#include <chartobj.ih>
#include <chart.ih>
#include <chartv.eh>


static   struct menulist	 *class_menulist;
static   struct keymap		 *class_keymap;

extern int			  sys_nerr;
extern char			 *sys_errlist[];

int chartv_debug = 0;


struct chartv *
chartv__Create( ClassID, specification, anchor )
  register struct  classheader	 *ClassID;
  chartv_Specification		 *specification;
  register struct view		 *anchor;
  {
  register struct chartv	 *self;

  IN(chartv_Create);
  if ( (self = chartv_New()) == NULL )
    {
    fprintf( stderr, "ChartV:  ERROR -- Unable to Create self\n" );
    }
    else
    {
    ClientAnchor = anchor;
    while ( specification  &&  specification->attribute )
      {
      SetChartAttribute( self, specification->attribute, specification->value );
      specification++;
      }
    }
  OUT(chartv_Create);
  return  self;
  }
  
void chartv_Save_Command(), chartv_Add_Command(),  chartv_ReChart_Command(), chartv_Delete_Command(),  chartv_Print_Command();
static void   Quit_Command(),Sort_Command(), Palette_Command(), DEBUG_Command();

static struct bind_Description	    view_menu[] =
  {
  { "chartv-Save", "",	    0,	"Save~50",	0, menu_default,
    chartv_Save_Command,	"Save Database" },
  { "chartv-Add", "",	    0,	"Add~60",	0, menu_default,
    chartv_Add_Command,	"Add Item" },
  { "chartv-Delete", "",   0,	"Delete~61",	0, menu_default,
    chartv_Delete_Command,	"Delete Item" },
  { "chartv-Print", "",    0,	"Print~63",	0, menu_default,
    chartv_Print_Command,	"Print Database" },
  { "chartv-Palette",  "", 0,	"Expose Palette~70",0, menu_palette_hidden,
    Palette_Command,	"Palette-toggle" },
  { "chartv-Palette",  "", 0,	"Hide Palette~70", 0, menu_palette_exposed,
    Palette_Command,	"Palette-toggle" },
  { "chartv-Quit",  "",    0,	"Quit~99",	0, menu_default,
    Quit_Command,	"Quit" },
/*===MUST BE DYNAMIC===*/
  { "chartv-ReChart", "",   0,	"ReChart~10,Histogram~1",
    (long)"Histogram",	menu_default,	chartv_ReChart_Command,"ReChart To Histogram" },
  { "chartv-ReChart", "",   0,	"ReChart~10,Pie~2",
    (long)"Pie",	menu_default, chartv_ReChart_Command,	"ReChart To Pie" },
  { "chartv-ReChart", "",   0,	"ReChart~10,Dot~3",
    (long)"Dot",	menu_default, chartv_ReChart_Command,	"ReChart To Dot" },
  { "chartv-ReChart", "",   0,	"ReChart~10,Line~4",
    (long)"Line",	menu_default, chartv_ReChart_Command,	"ReChart To Line" },
/*===
  { "chartv-ReChart", "",   0,	"ReChart~10,Cartesian~5",
    (long)"Cartesian",	menu_default,	ReChart_Command,"ReChart To Cartesian" },
  { "chartv-ReChart", "",   0,	"ReChart~10,Map~10",
    (long)"Map",	menu_default, ReChart_Command,	"ReChart To Map" },
  { "chartv-ReChart", "",   0,	"ReChart~10,Stack~11",
    (long)"Stack",	menu_default,	ReChart_Command,"ReChart To Stack" },
===*/
  { "chartv-Sort", "",    0,	"Sort~20,Label Ascend~10",
	chart_ByLabel | chart_Ascend, menu_default,    Sort_Command,	"Sort " },
  { "chartv-Sort", "",    0,	"Sort~20,Label Descend~11",
	chart_ByLabel | chart_Descend, menu_default,   Sort_Command,	"Sort " },
  { "chartv-Sort", "",    0,	"Sort~20,Value Ascend~20",
	chart_ByValue | chart_Ascend, menu_default,    Sort_Command,	"Sort " },
  { "chartv-Sort", "",    0,	"Sort~20,Value Descend~21",
	chart_ByValue | chart_Descend, menu_default,   Sort_Command,	"Sort " },
  { "chartv-Sort", "",    0,	"Sort~20,Position Ascend~30",
	chart_ByPosition | chart_Ascend, menu_default,    Sort_Command,	"Sort " },
  { "chartv-Sort", "",    0,	"Sort~20,Position Descend~31",
	chart_ByPosition | chart_Descend, menu_default,   Sort_Command,	"Sort " },
  NULL
  };

boolean
chartv__InitializeClass( classID )
  register struct classheader *classID;
  {
  IN(chartv_InitializeClass );
  class_menulist = menulist_New();
  class_keymap = keymap_New();
  bind_BindList( view_menu, class_keymap, class_menulist, &chartv_classinfo );
  proctable_DefineProc( "chartv-DEBUG", DEBUG_Command, &chartv_classinfo, 
			NULL, "Toggle debug flag.");
  OUT(chartv_InitializeClass );
  return TRUE;
  }

boolean
chartv__InitializeObject( classID, self)
  register struct classheader *classID;
  register struct chartv      *self;
  {
  register long		       status = true;

  IN(chartv_InitializeObject);
  DEBUGst(RCSID,rcsid);
  self->instance = (struct chartv_instance *) calloc( 1, sizeof(struct chartv_instance) );
  ReadOnly = true;
/*===*/  ReadOnly = false;
/*===*/  DescriptionExposed = true;
  Menu = menulist_DuplicateML( class_menulist, self );
  menulist_SetView( Menu, self );
  Keystate = keystate_Create( self, class_keymap );
  chartv_SetShrinkIcon( self, 'e', "icon12", "Chart", ItemCaptionFontNamePhrase );
  chartv_SetHelpFileName( self, "/usr/andy/help/ez.help"/*=== ===*/ );
  chartv_SetDimensions( self, 300, 300 );
  chartv_SetOptions( self, aptv_SuppressControl |
			   aptv_SuppressBorder |
			   aptv_SuppressEnclosures );
  if ( status == true )
    {
    Description = text_New();
    DescriptionView = textview_New();
    textview_SetDataObject( DescriptionView, Description );
    DescriptionViewScroll = textview_GetApplicationLayer( DescriptionView );
    text_AddObserver( Description, self );
    }
  if ( status == true )
    { DEBUG(Created Description);
    PairView = lpair_New();
    }
  OUT(chartv_InitializeObject);
  return  status;
  }

void 
chartv__FinalizeObject( classID, self )
  register struct classheader   *classID;
  register struct chartv	*self;
  {
  IN(chartv_FinalizeObject);
  if ( self->instance )
    {
    if ( ChartViewer )	chartobj_Destroy( ChartViewer );
    if ( Menu )		menulist_Destroy( Menu );
    if ( Keystate )	keystate_Destroy( Keystate );
    Destroy_Palette( self );
    free( self->instance );
    }
  OUT(chartv_FinalizeObject);
  }

struct view *
chartv__GetApplicationLayer( self )
  register struct chartv     *self;
  {
  IN(chartv_GetApplicationLayer);
  ApplicationLayer = true;
  if ( Menu )
    {
    menulist_SetMask( Menu, menu_default | menu_applicationlayer );
    chartv_PostMenus( self, Menu );
    }
  OUT(chartv_GetApplicationLayer);
  return  (struct view *) self;
  }

void
chartv__DeleteApplicationLayer( self, view )
  register struct chartv     *self;
  register struct view	     *view;
  {
  IN(chartv_DeleteApplicationLayer);
  ApplicationLayer = false;
  if ( Menu )
    {
    menulist_SetMask( Menu, menu_default );
    chartv_PostMenus( self, Menu );
    }
  OUT(chartv_DeleteApplicationLayer);
  }

void
chartv__SetDataObject( self, data_object )
  register struct chartv      *self;
  register struct chart	      *data_object;
  {
  IN(chartv_SetDataObject);
  Chart = data_object;
  super_SetDataObject( self, Chart );
  if ( ChartViewer )
    chartobj_SetDataObject( ChartViewer, Chart );
  DEBUGst(ChartType,chart_Type( Chart ));
  OUT(chartv_SetDataObject);
  }

void
chartv__ReceiveInputFocus( self )
  register struct chartv     *self;
  {
  IN(chartv_ReceiveInputFocus);
  
  InputFocus = true;
  if ( Keystate )
    {
    Keystate->next = NULL;
    chartv_PostKeyState( self, Keystate );
    }
  if ( Menu )
    {
    menulist_SetMask( Menu, menu_default | 
	((PaletteExposed) ? menu_palette_exposed : menu_palette_hidden) |
	((Application) ? menu_application : 0) );
    if(PaletteExposed) 
	view_PostMenus( (struct view *)Palette, Menu );
    chartv_PostMenus( self, Menu );
    }
  chartv_SetTransferMode( self, graphic_BLACK );
  chartv_DrawRectSize( self, Left, Top, Width-1, Height-1 );
  OUT(chartv_ReceiveInputFocus);
  }

void
chartv__LoseInputFocus( self )
  register struct chartv     *self;
  {
  IN(chartv_LoseInputFocus);
  InputFocus = false; 
  if ( ! IgnoreLoseInputFocus )
    { DEBUG(Do Not Ignore);
    chartv_SetTransferMode( self, graphic_WHITE );
    chartv_DrawRectSize( self, Left, Top, Width-1, Height-1 );
    }
  OUT(chartv_LoseInputFocus);
  }

long
chartv__SetChartAttribute( self, attribute, value )
  {  return  SetChartAttribute( self, attribute, value );  }

static
SetChartAttribute( self, attribute, value )
  register struct chartv     *self;
  register long		      attribute, value;
  {
  register long		      status = ok;

  IN(SetChartAttribute);
  switch ( attribute )
    {
    case  chartv_arrangement:
      Arrangement = value;			break;
    case  chartv_backgroundshade:
      BackgroundShade = value;			break;
    case  chartv_borderstyle:
      BorderStyle = value;			break;
    case  chartv_bordersize:
      BorderSize = value;			break;
    case  chartv_cursor:
      ChartCursorByte = value;			break;
    case  chartv_cursorfontname:
      apts_CaptureString( value, &ChartCursorFontName ); break;
    case  chartv_datum:
      ClientDatum = value;			break;
    case  chartv_hithandler:
      HitHandler = (struct view *(*)()) value;	break;
    case  chartv_itemborderstyle:
      ItemBorderStyle = value;			break;
    case  chartv_itembordersize:
      ItemBorderSize = value;			break;
    case  chartv_itemhighlightstyle:
      ItemHighlightStyle = value;		break;
    case  chartv_labelfontname:
      apts_CaptureString( value, *LabelFontName ); break;
    case  chartv_scalefontname:
      apts_CaptureString( value, *ScaleFontName ); break;
    case  chartv_titleborderstyle:
      TitleBorderStyle = value;			break;
    case  chartv_titlebordersize:
      TitleBorderSize = value;			break;
    case  chartv_titlecaptionfontname:
      apts_CaptureString( value, *TitleFontName ); break;
    case  chartv_titledataobjecthandler:
      TitleDataObjectHandler = (struct view (*)()) value;break;
    case  chartv_titlehighlightstyle:
      TitleHighlightStyle = value;		break;
    case  chartv_titlehithandler:
      TitleHitHandler = (struct view (*)()) value;break;
    case  chartv_titleplacement:
      TitlePlacement = value;			break;
    case  chartv_titleviewobjecthandler:
      TitleViewObjectHandler = (struct view (*)()) value;break;

    default:
      fprintf( stderr, "ChartV: Unrecognized ChartAttribute (%d) -- Ignored\n", attribute );
    }

  OUT(SetChartAttribute);
  return  status;
  }

long
chartv__ChangeChartAttribute( self, attribute, value )
  {  return  ChangeChartAttribute( self, attribute, value );  }

static
ChangeChartAttribute( self, attribute, value )
  register struct chartv     *self;
  register long		      attribute, value;
  {
  register long		      status = ok;

  IN(ChangeChartAttribute);
  if ( (status = SetChartAttribute( self, attribute, value )) == ok )
    {
/*===*/
    }
  OUT(ChangeChartAttribute);
  return  status;
  }

long
chartv__ChartAttribute( self, attribute )
  register struct chartv     *self;
  register long		      attribute;
  {
  register long		      value = NULL;

/*===*/
  return  value;
  }

struct chart_item *
chartv__CurrentItem( self )
  register struct chartv     *self;
  {
  register struct chart_item *item = NULL;

  if ( ChartViewer )
    item = chartobj_CurrentItem( ChartViewer );
  return  item;
  }

void
chartv__SetDebug( self, state )
  register struct chartv      *self;
  register char		       state;
  {
  IN(chartv_SetDebug);
  chartv_debug = state;
  if ( Chart )		chart_SetDebug( Chart, chartv_debug );
  if ( ChartViewer )	chartobj_SetDebug( ChartViewer, chartv_debug );
  OUT(chartv_SetDebug);
  }

void 
chartv__FullUpdate( self, type, left, top, width, height )
  register struct chartv	 *self;
  register enum view_UpdateType	  type;
  register long			  left, top, width, height;
  {
  IN(chartv_FullUpdate);
  if ( (!IgnoreFullUpdate)  &&  Chart  &&
       (type == view_FullRedraw || type == view_LastPartialRedraw) )
    {
    super_FullUpdate( self, type, left, top, width, height );
    chartv_ClearClippingRect( self );
    chartv_GetLogicalBounds( self, Bounds );
    if ( InputFocus )
      chartv_DrawRectSize( self, Left, Top, Width-1, Height-1 );
    if ( ! chartv_BypassUpdate(self) )
      { DEBUG(Not Bypassed);
      if ( ! Initialized )  Initialize( self );
      chartv_ClearBody( self );
      if ( ChartViewer )
	{ DEBUG(ChartViewer Exists);
	lpair_InsertViewSize( PairView, self,
	    chartv_BodyLeft(self)+4,  chartv_BodyTop(self)+4,
	    chartv_BodyWidth(self)-8, (chartv_BodyHeight(self) - 8) );
        lpair_FullUpdate( PairView, type, 0,0,
	    chartv_BodyWidth(self)-8, (chartv_BodyHeight(self) - 8) );
	lpair_GetEnclosedBounds( PairView, PairBounds );
	}
      }
    }
  IgnoreFullUpdate = false;
  OUT(chartv_FullUpdate);
  }

static
Initialize( self )
  register struct chartv     *self;
  {
  register char		     *moniker = NULL;

  IN(Initialize);
  if ( Chart )
    {
    moniker = (char *) chart_ChartAttribute( Chart, chart_Type(0) );
    DEBUGst(Moniker,moniker);
    DEBUGst(Chart-module-name,chart_ModuleName( Chart, moniker ));
    }
  if ( moniker )
    {
    if ( ChartViewer = (struct chartobj *)
	class_NewObject( chart_ModuleName( Chart, moniker ) ) )
      { DEBUG(Created);
      Initialized = true;
      chartobj_SetDebug( ChartViewer, chartv_debug );
      chartobj_SetDataObject( ChartViewer, Chart );
      lpair_VSplit( PairView, ChartViewer, DescriptionViewScroll, 0, 100 );
      }
    }
  OUT(Initialize);
  }

struct view *
chartv__Hit( self, action, x, y, clicks )
  register struct chartv	  *self;
  register enum view_MouseAction   action;
  register long			   x, y, clicks;
  {
  register struct view		  *hit;

  IN(chartv_Hit );
  chartv_Announce( self, "" );
  if ( (hit = super_Hit( self, action, x, y, clicks )) == NULL )
    { DEBUG(Accept Hit);
    if ( ChartViewer  &&  chartv_Within( self, x, y, PairBounds ) )
      { DEBUG(Pair Hit);
      if ( !InputFocus  &&  action == view_LeftDown )
        chartv_WantInputFocus( self, self );
      hit = (struct view *) lpair_Hit( PairView, action,
	    chartv_EnclosedXToLocalX( PairView, x ),
	    chartv_EnclosedYToLocalY( PairView, y ), clicks );
      }
    }
  OUT(chartv_Hit );
  return  hit;
  }

void
chartv__Print( self, file, processor, format, level )
  register struct chartv     *self;
  register FILE		     *file;
  register char		     *processor;
  register char		     *format;
  register boolean	      level;
  {
  IN(chartv_Print);
  if ( ChartViewer )
    chartobj_Print( ChartViewer, file, processor, format, level );
  OUT(chartv_Print);
  }

void
chartv_Add_Command( self )
  register struct chartv     *self;
  {
  char			     *reply;
  register struct chart_item *item;

  IN(Add_Command);
/*===*/
  while ( true )
    {
    chartv_Query( self, "Enter Name: ", "", &reply );
    if ( reply == NULL  ||  *reply == 0 )
      break;
    if ( item = chart_CreateItem( Chart, reply, NULL ) )
      {
      chartv_Query( self, "Enter Value: ", "", &reply );
      chartv_Announce( self, "" );
      if ( reply == NULL  ||  *reply == 0 )
        break;
      chart_SetItemAttribute( Chart, item, chart_ItemValue(atoi( reply )) );
      chart_NotifyObservers( Chart, chart_ItemsSorted/*===*/ );
      }
      else
      {
/*===*/
      }
    }
/*===*/
  chart_SetModified( Chart );
  if( PaletteInitialized )
      suite_Reset( ControlSuite, suite_Normalize );
  OUT(Add_Command);
  }

void
chartv_Delete_Command( self )
  register struct chartv     *self;
  {
  IN(Delete_Command);
  if ( chartobj_CurrentItem( ChartViewer ) )
    { DEBUG(CurrentItem Exists);
    chart_DestroyItem( Chart, chartobj_CurrentItem( ChartViewer ) );
    chartobj_CurrentItem( ChartViewer ) = NULL;
    chart_NotifyObservers( Chart, chart_ItemDestroyed );
    chart_SetModified( Chart );
    }
  if( PaletteInitialized )
      suite_Reset( ControlSuite, suite_Normalize );
  OUT(Delete_Command);
  }


chartv_ReChart( self, moniker )
  register struct chartv     *self;
  register char		     *moniker;
  {
  struct rectangle	      bounds;
  register struct chartobj   *prior_viewer = ChartViewer;

  IN(ReChart);
  DEBUGst(Moniker,moniker);
  if ( moniker  &&  *moniker )
    {
    chart_SetChartAttribute( Chart, chart_Type(moniker) );
    bounds.left = bounds.top = 0;
    bounds.width = Width; bounds.height = Height;
    if ( prior_viewer )
      chartobj_GetEnclosedBounds( ChartViewer, &bounds );
    if ( ChartViewer = (struct chartobj *)
	class_NewObject( chart_ModuleName( Chart, moniker ) ) )
      { DEBUGst(Created,moniker);
      chartobj_SetDebug( ChartViewer, chartv_debug );
      chartobj_SetDataObject( ChartViewer, Chart );
      if ( prior_viewer )
	{ DEBUG(Prior Viewer);
	chartv_SetTransferMode( self, graphic_WHITE );
	chartv_FillRect( self, &bounds, graphic_WHITE );
	chartv_SetTransferMode( self, graphic_BLACK );
	}
	else
	{ DEBUG(No Prior Viewer);
        Initialized = true;
        lpair_VSplit( PairView, ChartViewer, DescriptionViewScroll, 0, 100 );
	lpair_InsertViewSize( PairView, self,
	    chartv_BodyLeft(self)+4,  chartv_BodyTop(self)+4,
	    chartv_BodyWidth(self)-8, (chartv_BodyHeight(self) - 8) );
        lpair_FullUpdate( PairView, view_FullRedraw, 0,0,
	    chartv_BodyWidth(self)-8, (chartv_BodyHeight(self) - 8) );
	lpair_GetEnclosedBounds( PairView, PairBounds );
	Activate_Viewer( self );
	}
      lpair_SetNth( PairView, 0, ChartViewer );
      if ( prior_viewer )
	{ DEBUG(Destroy Prior Viewer);
	chart_RemoveObserver( Chart, prior_viewer );
	chartobj_Destroy( prior_viewer );
	}
      }
    }
  OUT(ReChart);
  }
void
chartv_ReChart_Command( self, moniker )
  register struct chartv     *self;
  register char		     *moniker;
  {
  IN(ReChart_Command);
  DEBUGst(moniker,moniker);
  chartv_ReChart( self, moniker );
  OUT(ReChart_Command);
  }

void
chartv_Print_Command( self )
  register struct chartv	*self;
  {
  register FILE			*file;
  char				 msg[512], *chart_file_name;
  char				 file_name[L_tmpnam];

  IN(Print_Command);
  chartv_UseWaitCursor( self );
  chart_file_name = (char *) chart_ChartAttribute( Chart, chart_FileName(0) );
  sprintf( msg, "Printing '%s' ...", chart_file_name );
  chartv_Announce( self, "Printing ..." );
  if ( file = fopen( tmpnam(file_name), "w" ) )
    {
    chartobj_Print( ChartViewer, file, "PostScript", "PostScript", 1 );
    fclose( file );
    sprintf( msg, "print -Tnative %s", file_name );
    system( msg );
    sprintf( msg, "Printed '%s'", chart_file_name );
    chartv_Announce( self, msg );
    unlink(file_name);
    }
    else chartv_Announce( self, "Error Printing" );
  chartv_UseNormalCursor( self );
  if( PaletteInitialized )
      suite_Reset( ControlSuite, suite_Normalize );
  OUT(Print_Command);
  }

static void
Sort_Command( self, datum )
  register struct chartv     *self;
  register long		      datum;
  {
  IN(Sort_Command);
  chart_Sort( Chart, datum, NULL );
  chart_NotifyObservers( Chart, chart_ItemsSorted );
  OUT(Sort_Command);
  }

void
chartv_Save_Command( self )
  register struct chartv     *self;
  {
  char			      msg[512],
			      original_name[512], backup_name[512];
  char			     *file_name;
  register FILE		     *file;
  register long		      serial = 1, status = ok;
  struct stat		      st;

  IN(Save_Command);
  if ( chart_ChartAttribute( Chart, chart_FileName(0)) == NULL )
    { DEBUG(Need FileName);
    chartv_QueryFileName( self, "Enter FileName: ", &file_name );
    chartv_Announce( self, "" );
    chart_SetChartAttribute( Chart, chart_FileName( file_name ) );
    }
  if ( Description_Modified( self ) )
    Preserve_Description( self );
  file_name = (char *) chart_ChartAttribute( Chart, chart_FileName(0) );
  if ( file_name )
    {
    chartv_UseWaitCursor( self );
    sprintf( original_name, "%s", file_name );
    DEBUGst(Original-name,original_name);
    chartv_Announce( self, "Saving ..." );
    sprintf( backup_name, "%s.BACKUP", file_name );
    if ( stat( original_name, &st ) == 0 )
      { DEBUG(Existent File);
      while ( ! stat( backup_name, &st ) )
        sprintf( backup_name, "%s.BACKUP.%d", file_name, serial++ );
      DEBUGst(Backup-name,backup_name);
      if ( rename( original_name, backup_name ) )
        { DEBUG(ReName Failure);
        sprintf( msg, "Unable to Create Backup for '%s'", file_name );
        chartv_Announce( self, msg );
	status = failure;
        }
      }
    if ( status == ok )
      {
      if ( file = fopen( file_name, "w" ) )
        { DEBUG(File Opened);
        chart_Write( Chart, file, im_GetWriteID()+1/*===*/, 0 );
        fclose( file );
        sprintf( msg, "Wrote '%s'", file_name );
        chartv_Announce( self, msg );
        im_SetTitle( chartv_GetIM( self ), file_name );
        LastModified = chart_GetModified( Chart );
        }
        else
        { DEBUG(File Open Failed);
        sprintf( msg, "Unable to Open '%s' (%s)", file_name, sys_errlist[errno] );
        chartv_Announce( self, msg );
        chart_SetChartAttribute( Chart, chart_FileName( NULL ) );
        }
      }
    chartv_UseNormalCursor( self );
    }
  if( PaletteInitialized )
      suite_Reset( ControlSuite, suite_Normalize );
  OUT(Save_Command);
  }

static void
DEBUG_Command( self )
  register struct chartv     *self;
  {
  IN(DEBUG_Command);
  chartv_SetDebug( self, !chartv_debug );
  chart_SetDebug( Chart, chartv_debug );
  OUT(DEBUG_Command);
  }

static void
Palette_Command( self )
  register struct chartv     *self;
  {
  IN(Palette_Command);
  if ( PaletteExposed )
    Hide_Palette( self );
    else {
	Expose_Palette( self );
	view_PostMenus((struct view *) Palette, Menu);
    }
  OUT(Palette_Command);
  }

static void
Quit_Command( self )
  register struct chartv     *self;
  {
  static char		     *choices[] =
		{"Cancel", "Save", "Save & Quit", "Quit Anyway", 0};
  long			      response = 0;

  IN(Quit_Command);
  Description_Modified( self );
  if ( (chart_GetModified( Chart ) > LastModified) )
    {
    message_MultipleChoiceQuestion(
	    self, 0, "Outstanding Modifications:", 0, &response, choices, NULL );
    DEBUGdt(Response,response);
    switch ( response )
      {
      case 0:	  break;
      case 1:	  chartv_Save_Command( self );  break;
      case 2:	  chartv_Save_Command( self );
      case 3:	  exit(0);
      default:    break;
      }
    }
    else  exit(0);
  OUT(Quit_Command);
  }

static
Description_Modified( self )
  register struct chartv	 *self;
  {
  register boolean		  status = false;

  IN(Description_Modified);
  if ( text_GetModified( Description ) != DescriptionLastModified )
    {
    DescriptionLastModified = text_GetModified( Description );
    status = true;
    chart_SetModified( Chart );
    }
  OUT(Description_Modified);
  return  status;
  }

static
Preserve_Description( self )
  register struct chartv	 *self;
  {
  register FILE			 *file;
  struct stat			  st;
  register char			 *buffer = NULL;
  char				  temp_name[L_tmpnam];

  IN(Preserve_Description);
  
  file = fopen( tmpnam(temp_name), "w" );
  text_Write( Description, file, im_GetWriteID(), 1 );
  fclose( file );
  stat( temp_name, &st );
  buffer = (char *) malloc( st.st_size + 2 );
  file = fopen( temp_name, "r" );
  fread( buffer, st.st_size + 1, st.st_size, file );
  fclose( file );
/*===*/
  unlink(temp_name);
  OUT(Preserve_Description);
  }

void
chartv__LinkTree( self, parent )
    struct chartv *self;
    struct view *parent;
{
    super_LinkTree(self, parent);
    if(chartv_GetIM(self)) {
	lpair_LinkTree(PairView, self);
    }
}
