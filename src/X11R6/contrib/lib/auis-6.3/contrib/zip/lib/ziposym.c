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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/ziposym.c,v 1.9 1993/10/18 18:00:10 gk5g Exp $";
#endif

/* ziposym.c	Zip Object -- Symbols					      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Symbols

MODULE	ziposym.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  07/10/88	Created (TC Peters)
  11/01/88	Optimize (TCP)
  11/16/88	Fix Symbol-set listing (TCP)
  05/01/89	Use symbolic font-names (TCP)
  05/26/89	Accomodate symbol-set name backward-compatibility (TCP)
  07/13/89	Fix non-view (print) usage (TCP)
   09/15/89	Fix symbol set path corruption (copy AndrewDir()  result), remove excess TransferMode activity in Draw() and DrawSymbol, modify Build accordingly (SCG)
  11/20/89	Fixed bogus dereferencing of argument to strlen() in Filter() (GW Keim)

END-SPECIFICATION  ************************************************************/

#include <andrewos.h>
#include <class.h>
#include <view.ih>
#include <environ.ih>
#include <text.ih>
#include <textv.ih>
#include <fontdesc.ih>
#include <zipobj.ih>
#include <ziposym.eh>
#include <sys/param.h>
#include <sys/stat.h>
#include <ctype.h>
#include <errno.h>

#ifndef M_UNIX
extern int				  sys_nerr;
#endif
extern char				 *sys_errlist[];
static char				 *symbol_library_path = NULL;

static struct symbol_set	         *symbol_sets = NULL;
static long				  symbol_sets_count;

#define  SVL				 (self->symbol_view_left)
#define  SVT				 (self->symbol_view_top)
#define  SVW				 (self->symbol_view_width)
#define  SVH				 (self->symbol_view_height)
#define  SVR				 (self->symbol_view_right)
#define  SVB				 (self->symbol_view_bottom)
#define  SymbolSelected			 (self->symbol_selected)
#define  SelectedSymbolSetName		 (self->selected_symbol_set_name)
#define  SelectedSymbolIndexName	 (self->selected_symbol_index_name)
#define  SelectedSymbolSet		 (self->selected_symbol_set)
#define  SelectedSymbolSetIndex		 (self->selected_symbol_set_index)
#define  SelectedSymbol			 (self->selected_symbol)
#define  SymbolSetDisplayed		 (self->symbol_set_displayed)
#define  MaxSymbolSetsCount		 100
#define  SymbolSets			 (symbol_sets)
#define  SymbolLibraryPath		 (symbol_library_path)
#define  SymbolLibraries		 (self->library_paths)
#define  CurrentSymbolSet		 (self->current_symbol_set)
#define  CurrentSymbolSetIndex		 (self->current_symbol_set_index)
#define  GrayShade			 (self->gray_shade)
#define  GrayGraphic			 (self->gray_graphic)
#define  LineWidth			 (self->line_width)
#define  LineStyle			 (self->line_style)
#define  LastNumber			 (self->last_number)
#define  OutstandingSurround		 (self->outstanding_surround)
#define  OutstandingLeft		 (self->outstanding_left)
#define  OutstandingTop			 (self->outstanding_top)
#define  OutstandingWidth		 (self->outstanding_width)
#define  OutstandingHeight		 (self->outstanding_height)

static struct symbol				 *Symbol_Set_Vector();
static struct symbol_set			 *Symbol_Set();
static char					 *Symbol_Algorithm(), *Pixel(), *Number(), *String(), *Skip_Colon();
static enum view_MouseAction			  Accept_Property_Hit();
static long					  Show_Symbol_Dialog();

static Draw();
static Draw_Symbol();
static int Identify_Pathed_Symbol_Sets();
static Open_Symbol_Set_File();
static Open_File();
static int Identify_Paths();
static Draw_Set_Name();
static long Show_Symbol_Dialog();
static Show_Set_Symbols();
static Highlight_Symbol();
static Invert_Symbol();
static Decline_Property_Hits();


boolean
ziposymbol__InitializeObject( classID, self )
  register struct classheader	         *classID;
  register struct ziposymbol	         *self;
  {
  register char				 *p;

  IN(ziposymbol_InitializeObject);
  SymbolSelected = OutstandingSurround = false;
  *SelectedSymbolSetName = NULL;
  *SelectedSymbolIndexName = NULL;
  SelectedSymbolSet = CurrentSymbolSet = NULL;
  SelectedSymbolSetIndex = CurrentSymbolSetIndex = NULL;
  SelectedSymbol = NULL;
  SymbolSetDisplayed = NULL;
  SymbolLibraries = NULL;
  GrayShade = NULL;
  LineStyle = NULL;
  LineWidth = 1;
  LastNumber = NULL;
  if ( symbol_library_path == NULL )
  {
    p = environ_AndrewDir( "/lib/zip/symbols" );
    if ( p && ( symbol_library_path = ( char * ) malloc( strlen( p ) + 1 )))
	strcpy( symbol_library_path, p );
  }
  DEBUGst(LibPath,symbol_library_path);
  OUT(ziposymbol_InitializeObject);
  return  true;
  }

void
ziposymbol__FinalizeObject( classID, self )
  register struct classheader	         *classID;
  register struct ziposymbol	         *self;
  {
  register long				  i;
  register struct symbol_set		 *sets = SymbolSets;

  IN(ziposymbol_FinalizeObject);
  if ( SymbolSets )
    {
    while ( sets->set_name )
      {
      if ( sets->set_file )
	fclose( sets->set_file );
      if ( sets->set_symbols )
        for ( i = 0; i < sets->set_symbols_count; i++ )
	  {
	  if ( sets->set_symbols[i].symbol_icon )
            free( sets->set_symbols[i].symbol_icon );
  	  if ( sets->set_symbols[i].symbol_name )
            free( sets->set_symbols[i].symbol_name );
	  if ( sets->set_symbols[i].symbol_algorithm )
            free( sets->set_symbols[i].symbol_algorithm );
	  }
      free( sets->set_symbols );
      sets++;
      }
    free( SymbolSets );
    }
  OUT(ziposymbol_FinalizeObject);
  }

char
ziposymbol__Object_Icon( self )
  register struct ziposymbol		 *self;
  {
  IN(ziposymbol_Object_Icon);
  OUT(ziposymbol_Object_Icon);
  return  'Q';
  }

char
ziposymbol__Object_Icon_Cursor( self )
  register struct ziposymbol		 *self;
  {
  IN(ziposymbol_Object_Icon_Cursor);
  OUT(ziposymbol_Object_Icon_Cursor);
  return  'B';
  }

char
ziposymbol__Object_Datastream_Code( self )
  register struct ziposymbol		 *self;
  {
  IN(ziposymbol_Object_Datastream_Code);
  OUT(ziposymbol_Object_Datastream_Code);
  return  'P';
  }

long
ziposymbol__Show_Object_Properties( self, pane, figure )
  register struct ziposymbol		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  register long				  status;

  IN(ziposymbol_Show_Object_Properties);
  status = Show_Symbol_Dialog( self, pane );
  OUT(ziposymbol_Show_Object_Properties);
  return  status;
  }

long
ziposymbol__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct ziposymbol		 *self;
  register zip_type_pane		  pane;
  register enum view_MouseAction	  action;
  register long				  x, y, clicks;
  register zip_type_point		  X, Y;
  {
  register long				  status = zip_ok;
  int					  position = 0; /*===*/
  register zip_type_figure		  figure;
  char					  symbol_string[257];

  IN(ziposymbol_Build_Object);
  switch ( action )
    {
    case view_LeftDown:
      if ( SelectedSymbol )
	{
        sprintf( symbol_string, "%s;%s", SelectedSymbolSetName, SelectedSymbolIndexName );
        DEBUGst(Symbol-string,symbol_string);
        if ( (status =
          zip_Create_Figure( Data, &CurrentFigure, NULL, zip_symbol_figure,
			 CurrentImage, position )) == zip_success )
	  {
	  zip_Set_Figure_Text( Data, CurrentFigure, symbol_string );
          ziposymbol_Set_Object_Point( self, CurrentFigure, zip_figure_origin_point, X, Y );
          ziposymbol_Set_Object_Point( self, CurrentFigure, zip_figure_auxiliary_point, X, Y );
	  CurrentFigure->zip_figure_zoom_level = pane->zip_pane_zoom_level;
          pane->zip_pane_edit->zip_pane_edit_last_point_id = zip_figure_auxiliary_point;
	  zip_Set_Figure_Shade( Data, CurrentFigure,
			        pane->zip_pane_edit->zip_pane_edit_current_shade );
	  zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
	  }
	}
	else  zipview_Announce( View, "No Symbol Selected." );
      break;
    case view_LeftUp:
      zipview_Set_Pane_Cursor( View, pane, 'B', CursorFontName ); /*=== ?? ===*/
      if ( SelectedSymbol  &&  (figure = CurrentFigure) )
	{
	zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
	if ( figure_x_point == figure_x_points(0)  &&
	     figure_y_point == figure_y_points(0) )
	  {
	  zipedit_Delete_Figure( Edit, figure, pane ); 
	  }
	  else zipview_Draw_Figure( View, CurrentFigure, pane );
	}
        break;
    case view_LeftMovement:
      if ( SelectedSymbol  &&  CurrentFigure )
	{
	zipview_Draw_Figure( View, CurrentFigure, pane );
        ziposymbol_Set_Object_Point( self, CurrentFigure, zip_figure_auxiliary_point, X, Y );
	zipview_Draw_Figure( View, CurrentFigure, pane );
	}
      break;
    }
  OUT(ziposymbol_Build_Object);
  return  status;
  }

long
ziposymbol__Draw_Object( self, figure, pane )
  register struct ziposymbol		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(ziposymbol_Draw_Object);
  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane );
  OUT(ziposymbol_Draw_Object);
  return  status;
  }

long
ziposymbol__Clear_Object( self, figure, pane )
  register struct ziposymbol		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(ziposymbol__Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane );
  OUT(ziposymbol__Clear_Object);
  return  status;
  }

static
Draw( self, figure, pane )
  register struct ziposymbol		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register short			  left = window_x_point, top = window_y_point,
					  width = window_x_points(0) - left,
					  height = window_y_points(0) - top,
					  x_factor = 1, y_factor = 1,
					  transfer_mode;
  register unsigned char		  shade;
  register char				 *algorithm;

  IN(Draw);
  if ( width < 0 )
    { DEBUG(X-Flipped);
    left = window_x_points(0);
    width = -width;
    x_factor = -1;
    }
  if ( height < 0 )
    { DEBUG(Y-Flipped);
    top = window_y_points(0);
    height = -height;
    y_factor = -1;
    }
  zipview_Set_Pane_Clip_Area( View, pane );
  transfer_mode = zipview_GetTransferMode( View );
  if ( figure->zip_figure_mode.zip_figure_mode_shaded  &&
       View->mouse_action != view_LeftMovement  &&
      (shade = figure->zip_figure_fill.zip_figure_shade) >= 1  &&
          shade <= 100 )
    {
    /* Shade of '0' means Transparent --- Shade of '1' means White */
    DEBUGdt(Shade,figure->zip_figure_fill.zip_figure_shade);
    if ( (shade = ('0' + ((shade + 10) / 10)) - 1) > '9' )  shade = '9';
    DEBUGdt(Shade-index,shade);
/*    if ( transfer_mode != graphic_COPY )
      zipview_SetTransferMode( View, graphic_COPY ); */
    zipview_FillTrapezoid( View, left, top, width, left, top + height, width,
	    zipview_Define_Graphic( View,
		zip_Define_Font( Data, ShadeFontName, NULL ), shade ) );
    }
  if ( OutstandingSurround )
    {
    OutstandingSurround = false;
    if ( transfer_mode != graphic_INVERT )
      zipview_SetTransferMode( View, graphic_INVERT );
    zipview_DrawRectSize( View, OutstandingLeft, OutstandingTop, OutstandingWidth, OutstandingHeight );
    }
  if ( View->mouse_action == view_LeftMovement )
    {
    OutstandingSurround = true;
    if ( transfer_mode != graphic_WHITE )
      zipview_SetTransferMode( View, graphic_WHITE );
    zipview_EraseRectSize( View, left-1, top-1, width+2, height+2 );
    zipview_SetTransferMode( View, graphic_INVERT );
    zipview_DrawRectSize( View, OutstandingLeft = left, OutstandingTop = top,
				OutstandingWidth = width, OutstandingHeight = height );
    }
    else
    {
    if ( algorithm = Symbol_Algorithm( self, figure ) )
      {
      if ( zipview_GetTransferMode( View ) != transfer_mode )
        zipview_SetTransferMode( View, transfer_mode );
      Draw_Symbol( self, figure, pane, algorithm,
	         left, top, width, height, x_factor, y_factor, false );
      }
      else  status = zip_failure;
    }
  zipview_Set_Pane_Clip_Area( View, pane );
  if ( ExposePoints )	    ziposymbol_Expose_Object_Points( self, figure, pane );
  if ( HighlightPoints )    ziposymbol_Highlight_Object_Points( self, figure, pane );
  OUT(Draw);
  return  status;
  }

long
ziposymbol__Print_Object( self, figure, pane )
  register struct ziposymbol		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register long				  left, top, width, height,
					  x_factor = 1, y_factor = 1;
  char					 *algorithm;

  IN(ziposymbol__Print_Object);
  left   = print_x_point;		top    = print_y_point;
  width  = print_x_points(0) - left;	height = print_y_points(0) - top;
  if ( figure->zip_figure_mode.zip_figure_mode_shaded )
    {
    DEBUGdt(Shade,figure->zip_figure_fill.zip_figure_shade);
    zipprint_Set_Shade( Print, figure->zip_figure_fill.zip_figure_shade );
    }
    else zipprint_Set_Shade( Print, 0 /* Transparent */ );
  if ( width < 0 )
    { DEBUG(X-Flipped);
    left = print_x_points(0);
    width = -width;
    x_factor = -1;
    }
  if ( height < 0 )
    { DEBUG(Y-Flipped);
    top = print_y_points(0);
    height = -height;
    y_factor = -1;
    }
  if ( algorithm = Symbol_Algorithm( self, figure ) )
    Draw_Symbol( self, figure, pane, algorithm, left, top, width, height,
	       x_factor, y_factor, true );
    else  status = zip_failure;
  OUT(ziposymbol__Print_Object);
  return  status;
  }

static
Draw_Symbol( self, figure, pane, algorithm, left, top, width, height,
	     x_factor, y_factor, print )
  register struct ziposymbol		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register char				 *algorithm;
  register zip_type_pixel		  left, top, width, height;
  register long				  x_factor, y_factor;
  register boolean			  print;
  {
  register long				  status = zip_ok, level, mode = NULL;
  long					  x, y, n, m, q, r;
  register long				  current_x = 0, current_y = 0,
					  start_x, start_y, end_x, end_y,
					  x_diff, y_diff,
					  start_angle = 0, offset_angle;
  register char				  operator, secondary_operator = 0, shade;
  register double			  M, D, XO, YO;
  struct rectangle			  rectangle;
  register char				 *buffer_ptr;
  char					  buffer[32000];
  register struct fontdesc		 *font/*=== = (struct fontdesc *)
					    zip_Define_Font( Data, "andysans10", NULL )===*/;

  IN(Draw_Symbol);
  DEBUGdt(Left,left);  DEBUGdt(Top,top);
  DEBUGdt(Width,width);DEBUGdt(Height,height);
  if ( left   > (x = zipview_Pane_Left( View, pane )) )		x = left;
  if ( top    > (y = zipview_Pane_Top( View, pane )) )		y = top;
  if ( width  < (m = zipview_Pane_Right( View, pane )  - x) )	m = width;
  if ( height < (n = zipview_Pane_Bottom( View, pane ) - y) )	n = height;
  if ( !print )
    zipview_Set_Clip_Area( View, pane, x-1, y-1, m+2, n+2 );
  M = width / 2000.0;  D = height / 2000.0;
  DEBUGgt(M,M);    DEBUGgt(D,D);
  XO = left + width / 2.0; YO = top + height / 2.0;
  DEBUGgt(XO,XO);    DEBUGgt(YO,YO);
  *buffer = 0;
  GrayGraphic = NULL;
  GrayShade = LineStyle = NULL;
  LineWidth = 1;
  if ( print )
    zipprint_Set_Line_Width( Print, 1 );
   else   if ( zipview_GetLineWidth( View ) != LineWidth )
    zipview_SetLineWidth( View, LineWidth );
  while ( operator = *algorithm )
    {
    DEBUGct(Operator,operator);
    if ( operator != '(' )
      {
      secondary_operator = *(algorithm+1);
      DEBUGct(Secondary-Operator,secondary_operator);
      algorithm = Skip_Colon( algorithm );
      if  ( operator != 'U' )
        algorithm = Pixel( self, algorithm, &x, &y, M, D, XO, YO, x_factor, y_factor );
      }
    switch( operator )
      {
      case 'A':  DEBUG(ArcTo);
	algorithm = Number( self, algorithm, &n );
	DEBUGdt(Mode,n);
	switch ( n )
	  {
	  case 1: n = (y_factor < 0) ?
		      ((x_factor < 0) ? 3 : 2) :
		      ((x_factor < 0) ? 4 : 1); break;
	  case 2: n = (y_factor < 0) ?
		      ((x_factor < 0) ? 4 : 1) :
		      ((x_factor < 0) ? 3 : 2); break;
	  case 3: n = (y_factor < 0) ?
		      ((x_factor < 0) ? 1 : 4) :
		      ((x_factor < 0) ? 2 : 3); break;
	  case 4: n = (y_factor < 0) ?
		      ((x_factor < 0) ? 2 : 3) :
		      ((x_factor < 0) ? 1 : 4); break;
	  }
	rectangle.width  = 2 * abs(x - current_x);
	rectangle.height = 2 * abs(y - current_y);
	offset_angle = 90;
	x_diff = abs(current_x - x);  y_diff = abs(current_y - y);
	start_x = current_x; start_y = current_y; end_x = x; end_y = y;
	switch ( n )
	  {
	  case 1:
	    if ( y < current_y )
	      {
	      rectangle.left   = x - x_diff;
	      rectangle.top    = y;
	      start_x = x; start_y = y; end_x = current_x; end_y = current_y;
	      }
	      else
	      {
	      rectangle.left   = current_x - x_diff;
	      rectangle.top    = current_y;
	      }
	    start_angle = 0;
	    break;
	  case 2:
	    if ( y < current_y )
	      {
	      rectangle.left   = current_x - x_diff;
	      rectangle.top    = y - y_diff;
	      start_x = x; start_y = y; end_x = current_x; end_y = current_y;
	      }
	      else
	      {
	      rectangle.left   = x - x_diff;
	      rectangle.top    = current_y - y_diff;
	      }
	    start_angle = 90;
	    break;
	  case 3:
	    if ( y < current_y )
	      {
	      rectangle.left   = x;
	      rectangle.top    = y - y_diff;
	      }
	      else
	      {
	      rectangle.left   = current_x;
	      rectangle.top    = current_y - y_diff;
	      start_x = x; start_y = y; end_x = current_x; end_y = current_y;
	      }
	    start_angle = 180;
	    break;
	  case 4:
	    if ( y < current_y )
	      {
	      rectangle.left   = current_x;
	      rectangle.top    = y;
	      }
	      else
	      {
	      rectangle.left   = x;
	      rectangle.top    = current_y;
	      start_x = x; start_y = y; end_x = current_x; end_y = current_y;
	      }
	    start_angle = 270;
	    break;
	  default:
	    DEBUGct(Default -- ERROR,*algorithm);
	  }
	if ( print )
	  {
	  zipprint_Draw_Arc( Print,
		   rectangle.left + rectangle.width/2,
		   rectangle.top  + rectangle.height/2,
		   rectangle.width/2, rectangle.height/2,
		   start_x, start_y, end_x, end_y );
	  zipprint_Move_To( Print, x, y );
	  }
	  else
	  {
	  zipview_DrawArc( View, &rectangle, start_angle, offset_angle );
	  zipview_MoveTo( View, x, y );
	  }
	break;
      case 'B':  DEBUG(DrawBox);
	algorithm = Pixel( self, algorithm, &m, &n, M, D, XO, YO, x_factor, y_factor );
	if ( print )
	  {
	  zipprint_Set_Shade( Print, GrayShade );
	  zipprint_Draw_Rectangle( Print, x, y, m, n );
	  }
	  else
	  {
	  if ( GrayGraphic )
	    {
/*	    if ( zipview_GetTransferMode( View ) != graphic_COPY )
	      zipview_SetTransferMode( View, graphic_COPY ); */
	    zipview_FillTrapezoid( View, x+1, y+1, abs(m - x)-1,
					 x+1, n-1, abs(m - x)-1, GrayGraphic );
	    }
/*	  if ( zipview_GetTransferMode( View ) != graphic_BLACK )
	    zipview_SetTransferMode( View, graphic_BLACK ); */
	  zipview_DrawRectSize( View, x, y, abs(m - x), abs(n - y) );
	  }
	break;
      case 'C':  DEBUG(DrawCircle);
        algorithm = Number( self, algorithm, &n );
	n = n * M;
	DEBUGdt(Radius,n);
	if ( n > 0 )
	  if ( print )
	    zipprint_Draw_Circle( Print, x, y, n );
	    else
	    zipview_DrawOvalSize( View, x - n, y - n, n<<1, n<<1 );
	break;
      case 'D':  DEBUG(DrawTo);
	if ( print )
	  zipprint_Draw_Line( Print, current_x, current_y, x, y );
	  else
	  zipview_DrawLineTo( View, x, y );
	break;
      case 'E':  DEBUG(DrawEllipse);
        algorithm = Number( self, algorithm, &n );
	n = n * M;
	DEBUGdt(X-Radius,n);
        algorithm = Number( self, algorithm, &m );
	m = m * D;
	DEBUGdt(Y-Radius,m);
	if ( m > 0  &&  n > 0 )
	  if ( print )
	    zipprint_Draw_Ellipse( Print, x, y, n, m );
	    else
	    {
	    if ( GrayGraphic )
	      zipview_FillOvalSize( View, x-n, y-m, n<<1, m<<1, GrayGraphic );
	    zipview_DrawOvalSize( View, x-n, y-m, n<<1, m<<1 );
	    }
	break;
      case 'F':  DEBUG(SetFont);
	algorithm++;
	buffer_ptr = buffer;
	while ( *algorithm )
	  if ( *algorithm == '"' )   break;
	    else   *buffer_ptr++ = *algorithm++;
	*buffer_ptr = 0;
	if ( *algorithm == '"' )  algorithm++;
	if ( *algorithm == ';' )  algorithm++;
	DEBUGst(Font-name,buffer);
	font = (struct fontdesc *) zip_Define_Font( Data, buffer, NULL );
	if ( print )
	  zipprint_Change_Font( Print, font );
	  else
          zipview_SetFont( View, font );
	break;
      case 'G':  DEBUG(GrayGraphic);
	GrayShade = LastNumber;
	DEBUGdt(GrayShade,GrayShade);
	if ( !print )
	 if ( GrayShade > 0 &&  GrayShade <= 100 )
	  {
	  if ( (shade = ('0' + ((GrayShade + 10) / 10)) - 1) > '9' )  shade = '9';
	  DEBUGct(Shade,shade);
	  GrayGraphic = zipview_Define_Graphic( View,
			zip_Define_Font( Data, ShadeFontName, NULL ), shade );
	  }
	  else
	  {
	  GrayGraphic = NULL;
	  GrayShade = NULL;
	  }
	break;
      case 'I':  DEBUG(Incorporate);
	{
	register struct symbol		 *vector;
	register struct symbol_set	 *css = CurrentSymbolSet;
	register long			  cssi = CurrentSymbolSetIndex;
	register char			 *set_name = CurrentSymbolSet->set_name;

	algorithm = Pixel( self, algorithm, &m, &n, M, D, XO, YO, x_factor, y_factor );
	algorithm = String( self, algorithm, buffer );
	algorithm = Number( self, algorithm, &q );
	vector = Symbol_Set_Vector( self, set_name );
	Draw_Symbol( self, figure, pane, vector[q].symbol_algorithm,
		     x, y, m-x, n-y, x_factor, y_factor, print );
	CurrentSymbolSet = css; CurrentSymbolSetIndex = cssi;
	break;
	}
      case 'L':  DEBUG(Set-Line...);
	if ( secondary_operator == 'W' )
	  { DEBUG(...Width);
	  LineWidth = LastNumber;
	  DEBUGdt(LineWidth,LineWidth);
	  if ( print )
	    {
	    zipprint_Set_Line_Width( Print, LineWidth ); 
	    }
	    else
	    {
	    if ( zipview_GetLineWidth( View ) != LineWidth )
		zipview_SetLineWidth( View, LineWidth );
	    }
	  }
	else
	if ( secondary_operator == 'S' )
	  { DEBUG(...Style);
	  if ( print )
	    {
/*===*/
	    }
	    else
	    {
/*===*/
	    }
	  }
	break;
      case 'M':  DEBUG(MoveTo);
	if ( print )
	  zipprint_Move_To( Print, x, y );
	  else
	  zipview_MoveTo( View, x, y );
	break;
      case 'R':  DEBUG(DrawRoundBox);
	algorithm = Pixel( self, algorithm, &m, &n, M, D, XO, YO, x_factor, y_factor );
	algorithm = Number( self, algorithm, &q );
	algorithm = Number( self, algorithm, &r );
	/*===q = r = 5;===*/
	if ( print )
	  zipprint_Draw_Round_Rectangle( Print, x, y, m, n, q*100, r*100 );
	  else
	  zipview_DrawRRectSize( View, x, y, m - x, n - y, q, r );
	break;
      case 'S':  DEBUG(DrawString);
	algorithm++;
	buffer_ptr = buffer;
	while ( *algorithm )
	  {
	  if ( *algorithm == '"' )
	    break;
	    else   *buffer_ptr++ = *algorithm++;
          }
	*buffer_ptr = 0;
	if ( *algorithm == '"' )  algorithm++;
	if ( *algorithm == ';' )  algorithm++;
	DEBUGst(Stack,buffer);
	if ( print )
	  {
/*===	  zipprint_Change_Font( Print, font );===*/
	  zipprint_Draw_String( Print, x, y, buffer, mode );
/*===	  zipprint_Restore_Font( Print ); ===*/
	  }
	  else
	  {
	  zipview_MoveTo( View, x, y );
/*===     zipview_SetFont( View, font );===*/
	  zipview_DrawString( View, buffer,
	    graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBOTTOM );
	  }
	break;
      case '(':  DEBUG(Parenthsize);
	level = 0;
	algorithm++;
	buffer_ptr = buffer;
	while ( *algorithm )
	  {
	  if ( *algorithm == '(' )  level++;
	    else if ( *algorithm == ')' )  level--;
	  if ( level >= 0 )
	    *buffer_ptr++ = *algorithm++;
	    else  break;
          }
	*buffer_ptr = 0;
	if ( *algorithm == ')' )  algorithm++;
	DEBUGst(Stack,buffer);
	break;
      case 'U':  DEBUG(Utilize);
	if ( *buffer )
	  {
          algorithm = Number( self, algorithm, &m );
          algorithm = Number( self,  algorithm, &n );
	  Draw_Symbol( self, figure, pane, buffer, left, top, width, height,
		       m*x_factor, n*y_factor, print );
	  }
	break;
      default:  DEBUGct(Default -- ERROR,*algorithm);
	algorithm++;
	break;
      }
    current_x = x;  current_y = y;
    }
  if ( !print )
    {
    zipview_Reset_Pane_Clip_Area( View, pane );
    if ( zipview_GetLineWidth( View ) != 1 )
      zipview_SetLineWidth( View, 1 );
    }
  OUT(Draw_Symbol);
  return  status;
  }
  /****************************************************************/
static
Filter( entry )
  register DIRENT_TYPE		 *entry;
  {
  register char			 *end = entry->d_name + strlen(entry->d_name);

  return ( !(*entry->d_name == '.'  &&
	     (*(entry->d_name+1) == '.'  ||  *(entry->d_name+1) == '\0')) &&
	   !((end - entry->d_name) > 3  &&
	     ((*(end-3) == 'B'  &&  *(end-2) == 'A'  &&  *(end-1) == 'K')  ||
	      (*(end-3) == 'C'  &&  *(end-2) == 'K'  &&  *(end-1) == 'P') ) ) );
  }

static
Identify_Symbol_Sets( self )
  register struct ziposymbol		 *self;
  {
  register long				  status = zip_ok, i;

  IN(Identify_Symbol_Sets);
  if ( SymbolSets == NULL )
    {
    if ( SymbolLibraries == NULL )
      {
      if ( (status = Identify_Paths( self, &SymbolLibraries )) == zip_ok)
        SymbolSets = (struct symbol_set *)
		    calloc( 1, (MaxSymbolSetsCount+1) * sizeof(struct symbol_set) );
      }
    if ( status == zip_ok )
      for ( i = 0; i < SymbolLibraries->zip_paths_count; i++ )
	{
        status = Identify_Pathed_Symbol_Sets( self, SymbolLibraries->zip_paths_vector[i] );
	}
    }
  DEBUGdt(Status,status);
  OUT(Identify_Symbol_Sets);
  return  status;
  }

static int
Identify_Pathed_Symbol_Sets( self, path )
  register struct ziposymbol		 *self;
  register char				 *path;
  {
  register long				  status = zip_ok, count, i, j;
  char					  msg[1025];
  DIRENT_TYPE				**anchor;
  struct stat				  stats;
  int					  alphasort();

  IN(Identify_Pathed_Symbol_Sets);
  DEBUGst(Path,path);
  sprintf( msg, "Accessing Directory '%s'", path );
  zipview_Announce( View, msg );
  if ( stat( path, &stats ) )
    { DEBUG(Accessing ERROR);
    status = zip_failure;
    sprintf( msg, "ERROR Accessing Directory '%s'", path );
    zipview_Announce( View, msg );
    }
    else
    {
    if ( (count = scandir( path, &anchor, Filter, alphasort )) < 0 )
      { DEBUG(Scanning ERROR);
      status = zip_failure;
      sprintf( msg, "ERROR Scanning Directory '%s'", path );
      zipview_Announce( View, msg );
      }
      else
      {
      zipview_Announce( View, "Done." );
      DEBUGdt(Count,count);
      if ( count )
	{
	if ( SymbolSets )
	  {
	  for ( i = 0, j = symbol_sets_count;
		i < count  &&  ((i + j) < MaxSymbolSetsCount);
		i++ )
	    {
	    SymbolSets[i+j].set_name = anchor[i]->d_name;
	    DEBUGst(Set-name,SymbolSets[i+j].set_name);
	    SymbolSets[i+j].set_path = path;
	    }
	  symbol_sets_count += count;
	  }
	  else
	  { DEBUG(ERROR: SymbolSets Unallocated);
/*===*/
	  }
	}
      }
    }
  OUT(Identify_Pathed_Symbol_Sets);
  return  status;
  }

static struct symbol_set *
Symbol_Set( self, set_name )
  register struct ziposymbol		 *self;
  register char				 *set_name;
  {
  register struct symbol_set		 *set = NULL, *sets = SymbolSets;

  IN(Symbol_Set);
  if ( sets == NULL )
    { DEBUG(Not Initialized);
/*===*/
    }
  while ( sets->set_name )
    { DEBUGst(Set-name,sets->set_name);
    if ( strcmp( sets->set_name, set_name ) == 0 )
      { DEBUG(Found Set);
      set = sets;
      break;
      }
    sets++;
    }
  OUT(Symbol_Set);
  return  set;
  }

static struct symbol *
Symbol_Set_Vector( self, set_name )
  register struct ziposymbol		 *self;
  register char				 *set_name;
  {
  register struct symbol		 *symbol_vector = NULL;
  register struct symbol_set		 *sets = SymbolSets;

  IN(Symbol_Set_Vector);
  if ( sets == NULL )
    { DEBUG(Not Initialized);
/*===*/
    }
  while ( sets->set_name )
    { DEBUGst(Set-name,sets->set_name);
    if ( strcmp( sets->set_name, set_name ) == 0 )
      { DEBUG(Found Set);
      if ( sets->set_file == NULL )
	{ DEBUG(Set-file Not Open);
	if ( Open_Symbol_Set_File( self, sets ) )
	  { DEBUG(Open ERROR);
/*===*/
	  break;
	  }
	}
      symbol_vector = sets->set_symbols;
      break;
      }
    sets++;
    }
  OUT(Symbol_Set_Vector);
  return  symbol_vector;
  }

static
Open_Symbol_Set_File( self, set )
  register struct ziposymbol		 *self;
  register struct symbol_set		 *set;
  {
  register long				  status = zip_ok,
					  length, count = 0;
  char					  buffer[8193];
  register boolean			  continuation = false;

  IN(Open_Symbol_Set_File);
  if ( Open_File( self, set ) == zip_ok )
    { DEBUG(Open OK);
    set->set_symbols = (struct symbol *)
	calloc( 256, sizeof(struct symbol) );
/*===use grits===*/
    while ( fgets( buffer, sizeof buffer - 1, set->set_file ) )
      {
      length = strlen( buffer );
      if ( buffer[length-1] == '\n' )
	buffer[length-1] = 0;
      DEBUGst(Buffer,buffer);
      if ( *buffer != '#'  ||  *buffer == '\n' )
	{
	if ( continuation )
	  {
	  set->set_symbols[count].symbol_algorithm = (char *)
	    realloc( set->set_symbols[count].symbol_algorithm,
		     strlen( set->set_symbols[count].symbol_algorithm ) +
		      length+1 );
	  strcat( set->set_symbols[count].symbol_algorithm, buffer );
	  }
	  else
	  {
          set->set_symbols[++count].symbol_algorithm = (char *)
	    malloc( length+1 );
          strcpy( set->set_symbols[count].symbol_algorithm, buffer );
	  }
	if ( buffer[length-2] == '\\' )
	  {
	  set->set_symbols[count].
	    symbol_algorithm[strlen(set->set_symbols[count].symbol_algorithm)-1] = 0;
	  continuation = true;
	  }
	  else
	  continuation = false;
	}
      DEBUGst(Algorithm,set->set_symbols[count].symbol_algorithm);
      }
    set->set_symbols_count = count;
    zipview_Announce( View, "Done." );
/*===use grits===*/
    }
    else
    { DEBUG(Open ERROR);
    status = zip_failure;
    sprintf( buffer, "ERROR Accessing '%s/%s': %s",
	     set->set_path, set->set_name, sys_errlist[errno] ); 
    zipview_Announce( View, buffer );
    }
  OUT(Open_Symbol_Set_File);
  return  status;
  }

static
Open_File( self, set )
  register struct ziposymbol		 *self;
  register struct symbol_set		 *set;
  {
  register long				  i, status = zip_failure;
  char					  buffer[512];

  IN(Open_File);
  DEBUGst(Set-name,set->set_name);
  sprintf( buffer, "Accessing '%s/%s'", set->set_path, set->set_name ); 
  DEBUGst(File-name,buffer);
  zipview_Announce( View, buffer );
  sprintf( buffer, "%s/%s", set->set_path, set->set_name );
  errno = NULL;
  if ( set->set_file = fopen( buffer, "r" ) )
    status = zip_ok;
    else
    {
    if ( errno == ENOENT )
      {
      status = -1;
      for ( i = 0; status == -1  &&  i < SymbolLibraries->zip_paths_count; i++ )
	{
	errno = NULL;
	sprintf( buffer, "%s/%s", SymbolLibraries->zip_paths_vector[i], set->set_name );
	DEBUGst(Trying,buffer);
	if ( set->set_file = fopen( buffer, "r" ) )
	  { DEBUG(Success);
	  status = zip_ok;
	  break;
	  }
	if ( status == -1 )
	  status = zip_system_status_value_boundary + errno;
	}
      }
      else  status = zip_system_status_value_boundary + errno;
    }
  DEBUGdt(Status,status);
  OUT(Open_File);
  return  status;
  }

static int
Identify_Paths( self, paths_ptr )
  register struct ziposymbol	     *self;
  register zip_type_paths	     *paths_ptr;
  {
  register long			      new_path = 1, status = zip_ok;
  char				     *zippath_profile, *zippath_string;

  IN(Identify_Paths);
  if ( (*paths_ptr = (zip_type_paths)
        calloc( 1, sizeof(struct zip_paths) + (10 * sizeof(zip_type_path)) )) != NULL )
    {
    (*paths_ptr)->zip_paths_count = 0;
    if ( (zippath_profile = (char *) environ_GetProfile( "ZipSymbolPaths" ))  ||
         (zippath_profile = (char *) environ_GetProfile( "ZipSymbolPath" )) )
      {
      if ( zippath_string = (char *) malloc( strlen( zippath_profile ) + 1 ) )
        {
        strcpy( zippath_string, zippath_profile );
	DEBUGst(Path-string,zippath_string);
        while ( *zippath_string != '\0'  &&  (*paths_ptr)->zip_paths_count < 10 )
          {
          if ( new_path  &&  *zippath_string != ':' )
            {
            (*paths_ptr)->zip_paths_vector[(*paths_ptr)->zip_paths_count++] =
              zippath_string;
	    new_path = false;
	    }
          if ( *zippath_string  == ':' )
            {
	    *zippath_string = 0;
	    DEBUGst(Path,zippath_string);
            new_path = true;
            }
          zippath_string++;
	  }
	}
	else status = zip_insufficient_stream_space;
      }
    if ( (*paths_ptr)->zip_paths_vector[(*paths_ptr)->zip_paths_count] =
         (char *) malloc( strlen( symbol_library_path ) + 1 ))
    {
      strcpy( (*paths_ptr)->zip_paths_vector[(*paths_ptr)->zip_paths_count],
		symbol_library_path );
      (*paths_ptr)->zip_paths_count++;
    }
      else status = zip_insufficient_stream_space;
    }
    else status = zip_insufficient_stream_space;
  DEBUGdt(Status,status);
  OUT(Identify_Paths);
  return status;
  }

static char *
Symbol_Algorithm( self, figure )
  register struct ziposymbol		 *self;
  register zip_type_figure		  figure;
  {
  register long				  symbol_index;
  register char				 *s, *t, *algorithm = NULL, *number;
  static char				  string[257], symbol_string[257];
  register struct symbol		 *symbol_vector;
  static char				 *old_names[] =
    { "Arrows", "arrows",	"Borders", "borders",
      "CMU", "cmu",		"Computers", "computer",
      "DayBooks", "daybooks",
      "FlowChart", "flowchrt",  "Games","games",
      "General", "general",	"Home", "home",
      "ITC", "itc",		"Months", "months",
      "Mathematical", "math",	"Organization", "organize",
      "Shields", "shields",	"Stars", "stars",
      NULL };
  register char				**old_name = old_names;

  IN(Symbol_Algorithm);
  if ( Identify_Symbol_Sets( self ) == zip_ok )
    {
    s = figure->zip_figure_datum.zip_figure_text;
    DEBUGst(Text-string,s);
    t = string;
    while ( *s  &&  *s != ';'  &&  *s != '\n' )    *t++ = *s++;
    *t = 0;
    DEBUGst(Original Symbol-set-name,string);
    /* Backward compatibility for symbol-set name change */
    while ( *old_name )
      { DEBUGst(Check,*old_name);
      if ( strcmp( *old_name, string ) == 0 )
	{
	fprintf( stderr, "Zip: SymbolSet '%s' Replaced By '%s'\n",
		    *old_name, *(old_name+1) );
	strcpy( string, *(old_name+1) );
	if ( *s == ';' )  number = ++s;  else number = "1";
	sprintf( symbol_string, "%s;%s", string, number );
	DEBUGst(symbol-string,symbol_string);
	zip_Set_Figure_Text( Data, figure, symbol_string );
	break;
	}
      old_name += 2;
      }
    DEBUGst(Used Symbol-set-name,string);
    if ( symbol_vector = Symbol_Set_Vector( self, string ) )
      {
      if ( CurrentSymbolSet = Symbol_Set( self, string ) )
	{
        if ( *s == ';' )    s++;
        t = string;
        while ( *s  &&  *s != ';'  &&  *s != '\n' )    *t++ = *s++;
        *t = 0;
        DEBUGst(Symbol-set-index,string);
        CurrentSymbolSetIndex = symbol_index = atol( string );
        DEBUGdt(Symbol-index,symbol_index);
        algorithm = symbol_vector[symbol_index].symbol_algorithm;
	}
	else
	{ DEBUG(Error);
	fprintf( stderr, "Zip: SymbolSet '%s' Not Found\n", string );
	}
      }
      else
      { DEBUG(ERROR in Symbol-Set-Vector);
/*===*/
      }
    }
    else
    { DEBUG(ERROR Identifying Symbol-Sets);
/*===*/
    }
  DEBUGst(Algorithm,algorithm);
  OUT(Symbol_Algorithm);
  return  algorithm;
  }

static
Highlight_Set_Name( self, pane, set )
  register struct ziposymbol		 *self;
  register zip_type_pane		  pane;
  register struct symbol_set		 *set;
  {
  register struct symbol_set		 *candidate = SymbolSets;
  register struct fontdesc		 *normal_font, *highlight_font;

  IN(Highlight_Set_Name);
  normal_font    = (struct fontdesc *) zip_Define_Font( Data, "andysans10", NULL );
  highlight_font = (struct fontdesc *) zip_Define_Font( Data, "andysans10b", NULL );
  while ( candidate->set_name )
    {
    if ( candidate->set_highlighted  &&  candidate != set )
      { DEBUG(Normalize);
      candidate->set_highlighted = false;
      Draw_Set_Name( self, pane, candidate, normal_font );
      }
    else
    if ( (! candidate->set_highlighted)  &&  candidate == set )
      { DEBUG(Highlight);
      candidate->set_highlighted = true;
      Draw_Set_Name( self, pane, candidate, highlight_font );
      }
    candidate++;
    }
  OUT(Highlight_Set_Name);
  }

static
Draw_Set_Name( self, pane, set, font )
  register struct ziposymbol		 *self;
  register zip_type_pane		  pane;
  register struct symbol_set		 *set;
  register struct fontdesc		 *font;
  {
  IN(Draw_Set_Name);
  zipview_Set_Clip_Area( View, pane, SVL, SVT + 40, SVW/3, SVH - 50 );
  zipview_SetTransferMode( View, graphic_WHITE );
  zipview_EraseRectSize( View, set->set_left, set->set_top,
			       set->set_right  - set->set_left,
		               set->set_bottom - set->set_top );
  zipview_SetTransferMode( View, graphic_BLACK );
  zipview_SetFont( View, font );
  zipview_MoveTo( View, set->set_left, set->set_top );
  zipview_DrawString( View, set->set_name, graphic_ATLEFT | graphic_ATTOP );
  OUT(Draw_Set_Name);
  }

static long
Show_Symbol_Dialog( self, pane )
  register struct ziposymbol		 *self;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok, x, y;
  register struct symbol_set		 *set;
  register struct fontdesc		 *normal_font, *highlight_font;

  IN(Show_Symbol_Dialog);
  zipview_Use_Working_Pane_Cursors( View );
  if ( (status = Identify_Symbol_Sets( self )) == zip_ok )
    {
    zipedit_Set_Keyboard_Processor( Edit, self, Accept_Property_Hit );
    zipedit_Set_Pending_Processor( Edit, self, Show_Symbol_Dialog );
    zipview_Set_Pane_Clip_Area( View, pane );
    SymbolSelected = false;
    SVL = zipview_Pane_Left( View, pane ) + 20;  SVT = zipview_Pane_Top( View, pane ) + 20;
    SVW = zipview_Pane_Width( View, pane ) - 40; SVH = zipview_Pane_Height( View, pane ) - 40;
    SVR = SVL + SVW;  SVB = SVT + SVH;
    zipview_SetTransferMode( View, graphic_WHITE );
    zipview_EraseRectSize( View, SVL-1, SVT-1, SVW+2, SVH+2 );
    zipview_SetTransferMode( View, graphic_BLACK );
    zipview_DrawRRectSize( View, SVL, SVT, SVW-1, SVH-1, 10,10 );
    zipview_DrawRRectSize( View, SVL+1, SVT+1, SVW-3, SVH-3, 10,10 );
    zipview_MoveTo( View, SVL + SVW/3, SVT );
    zipview_DrawLineTo( View, SVL + SVW/3, SVT + SVH-1 );
    zipview_MoveTo( View, SVL, SVT + 40 );
    zipview_DrawLineTo( View, SVL + SVW/3, SVT + 40 );
    zipview_MoveTo( View, SVL + SVW/6, SVT + 25 );
    zipview_SetFont( View, zip_Define_Font( Data, "andysans12b", NULL ) );
    zipview_Set_Clip_Area( View, pane, SVL, SVT + 5, SVW/3, 40 );
    zipview_DrawString( View, "Symbols", graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBOTTOM );
    set = SymbolSets;
    normal_font = (struct fontdesc *) zip_Define_Font( Data, "andysans10", NULL );
    highlight_font = (struct fontdesc *) zip_Define_Font( Data, "andysans10b", NULL );
    x = SVL + 5;  y = SVT + 54;
    while ( set->set_name )
      { DEBUGst(Display Set-name,set->set_name);
      set->set_left = x;
      set->set_right = SVL + SVW/3; 
      set->set_top = y - 6;
      set->set_bottom = y + 6;
      if ( set->set_highlighted )
        {
        Draw_Set_Name( self, pane, set, highlight_font );
        Show_Set_Symbols( self, pane, set );
        }
        else
        Draw_Set_Name( self, pane, set, normal_font );
      y += 12;
      set++;
      }
    zipview_Use_Normal_Pane_Cursors( View );
    zipview_Set_Pane_Cursor( View, pane, 'a', "icon12" );
    zipview_Set_Pane_Clip_Area( View, pane );
    zipview_Announce( View, "Select a Symbol-Set ..." );
    }
  OUT(Show_Symbol_Dialog);
  return  status;
  }

static
Show_Set_Symbols( self, pane, set )
  register struct ziposymbol		 *self;
  register zip_type_pane		  pane;
  register struct symbol_set		 *set;
  {
  register struct symbol		 *symbols;
  register short			  row = 0, col = 0, x, y,
					  x_increment, y_increment;

  IN(Show_Set_Symbols);
  DEBUGst(Set-name,set->set_name);
  zipview_Set_Pane_Clip_Area( View, pane );
  zipview_Set_Pane_Cursor( View, pane, 'H', "icon12" );
  SymbolSetDisplayed = set;
  if ( set->set_file == NULL )
    Open_Symbol_Set_File( self, set );
  x = SVL + SVW/3 + 4;  y = SVT + 4;
  x_increment = (SVR - x) / 4 - 4;  y_increment = (SVB - y) / 4 - 4;
  zipview_SetTransferMode( View, graphic_WHITE );
  zipview_EraseRectSize( View, x, y, SVR - x - 4, SVB - y - 4 );
  zipview_SetTransferMode( View, graphic_BLACK );
  symbols = set->set_symbols;
  symbols++;
  while ( symbols->symbol_algorithm )
    {
    symbols->symbol_left = x; symbols->symbol_right = x + x_increment;
    symbols->symbol_top  = y; symbols->symbol_bottom = y + y_increment;
    zipview_DrawRectSize( View, x, y, x_increment, y_increment );
    Draw_Symbol( self, NULL, pane, symbols->symbol_algorithm,
		 x+4, y+4, x_increment-6, y_increment-6, 1, 1, false );
    if ( symbols->symbol_highlighted )
      Invert_Symbol( self, symbols );
    if ( ++col == 4 )
      { row++; y += y_increment + 4; col = 0; x = SVL + SVW/3 + 4; }
      else x += x_increment + 4;
    symbols++;
    }
  zipview_Set_Pane_Clip_Area( View, pane );
  zipview_Set_Pane_Cursor( View, pane, 'a', "icon12" );
  OUT(Show_Set_Symbols);
  }

static
Highlight_Symbol( self, set, symbol )
  register struct ziposymbol		 *self;
  register struct symbol_set		 *set;
  register struct symbol		 *symbol;
  {
  register struct symbol		 *candidate = set->set_symbols;

  IN(Highlight_Symbol);
  candidate++;
  while ( candidate->symbol_algorithm )
    {
    if ( candidate->symbol_highlighted  &&  candidate != symbol )
      { DEBUG(Normalize);
      candidate->symbol_highlighted = false;
      Invert_Symbol( self, symbol );
      }
    else
    if ( (! candidate->symbol_highlighted)  &&  candidate == symbol )
      { DEBUG(Highlight);
      candidate->symbol_highlighted = true;
      Invert_Symbol( self, symbol );
      }
    candidate++;
    }
  OUT(Highlight_Symbol);
  }

static
Invert_Symbol( self, symbol )
  register struct ziposymbol		 *self;
  register struct symbol		 *symbol;
  {
  IN(Invert_Symbol);
  zipview_SetTransferMode( View, graphic_INVERT );
  zipview_FillRectSize( View, symbol->symbol_left+2, symbol->symbol_top+2,
			symbol->symbol_right  - symbol->symbol_left - 3,
			symbol->symbol_bottom - symbol->symbol_top - 3,
			zipview_WhitePattern( View ) );
  OUT(Invert_Symbol);
  }

static enum view_MouseAction
Accept_Property_Hit( self, pane, c, action, x, y, clicks )
  register struct ziposymbol		 *self;
  register zip_type_pane		  pane;
  register char				  c;
  register enum view_MouseAction	  action;
  register long				  x, y, clicks;
  {
  register struct symbol_set		 *set = SymbolSets;
  register struct symbol		 *symbols;
  register short			  serial = 1;
  static boolean			  exit_on_up;

  IN(Accept_Property_Hit)
  if ( action == view_LeftDown )
    {
    if ( x >= SVL  &&  x <= SVR &&  y >= SVT   &&  y <= SVB )
      { DEBUG(Inside Symbol-view);
      if ( x <= SVL + SVW/3 )
        { DEBUG(Inside Set-names);
        while ( set->set_name )
          {
          if ( x >= set->set_left  &&  x <= set->set_right  &&
	       y >= set->set_top   &&  y <= set->set_bottom )
	    { DEBUGst(Set-name Hit,set->set_name);
	    strcpy( SelectedSymbolSetName, set->set_name );
	    DEBUGst(Selected Set,SelectedSymbolSetName);
	    SelectedSymbolSet = CurrentSymbolSet = set;
	    SelectedSymbolSetIndex = CurrentSymbolSetIndex = NULL;
	    Highlight_Set_Name( self, pane, set );
	    Show_Set_Symbols( self, pane, set );
	    zipview_Announce( View, "Select a Symbol, then Drag an Area ..." );
	    break;
	    }
          set++;
          }
	}
      else
        { DEBUG(Inside Symbol-icons);
        if ( SymbolSetDisplayed )
	  {
          symbols = SymbolSetDisplayed->set_symbols;
          symbols++;
          while ( symbols  &&  serial <= SymbolSetDisplayed->set_symbols_count )
	    {
	    if ( x >= symbols->symbol_left  &&  x <= symbols->symbol_right  &&
	         y >= symbols->symbol_top   &&  y <= symbols->symbol_bottom )
	      { DEBUG(Inside Symbol Icon);
	      sprintf( SelectedSymbolIndexName, "%d", serial );
	      DEBUGst(Selected Index,SelectedSymbolIndexName);
	      SelectedSymbolSetIndex = serial;
	      SelectedSymbol = &SelectedSymbolSet->set_symbols[serial];
	      Highlight_Symbol( self, SelectedSymbolSet, SelectedSymbol );
	      SymbolSelected = true;
	      break;
	      }
	    serial++;  symbols++;
	    }
	  }
	}
      }
      else
      { DEBUG(Outside Symbol-view);
      exit_on_up = true;
      *SelectedSymbolIndexName = 0;
      SelectedSymbol = NULL;
      SelectedSymbolSetIndex = NULL;
      zipview_Announce( View, "No Symbol Selected." );
      }
    }
    else
    {
    if ( action == view_LeftUp )
      {
      if ( SymbolSelected  ||  exit_on_up )
        Decline_Property_Hits( self, pane );
      }
      else if ( action == view_NoMouseEvent  ||  action == view_RightDown )
        Decline_Property_Hits( self, pane );
    exit_on_up = false;
    }
  if ( action != view_RightDown )
    action = view_NoMouseEvent;
  OUT(Accept_Property_Hit);
  return  action;
  }

static
Decline_Property_Hits( self, pane )
  register struct ziposymbol		 *self;
  register zip_type_pane		  pane;
  {
  IN(Decline_Property_Hits);
  zipview_SetTransferMode( View, graphic_WHITE );
  zipview_EraseRectSize( View, SVL, SVT, SVW, SVH );
  zipedit_Set_Keyboard_Processor( Edit, NULL, NULL );
  zipedit_Set_Pending_Processor( Edit, NULL, NULL );
  zipview_Set_Pane_Cursor( View, pane, 'B', CursorFontName );
  zipview_Draw_Pane( View, pane );
  OUT(Decline_Property_Hits);
  }

static char *
Skip_Colon( string )
  register char				 *string;
  {
  while ( *string  &&  *string != ':' )  string++;
  if ( *string )     string++;
  return  string;
  }

static char *
String( self, string, s )
  register struct ziposymbol		 *self;
  register char				 *string;
  register char				**s;
  {
  static char				  extracted_string[257];
  register char				 *p = extracted_string;

  IN(String);
  *extracted_string = 0;
  *s = extracted_string;
  while ( *string  &&  *string != ','  &&  *string != '\n' )
    {
    *p++ = *string++;
    }
  *p = 0;
  if ( *string  &&  (*string == ','  ||  *string == '\n') )  string++;
  DEBUGst(Extracted,extracted_string );
  OUT(String);
  return  string;
  }

static char *
Number( self, string, n )
  register struct ziposymbol		 *self;
  register char				 *string;
  register long				 *n;
  {
  char					  number_string[257];
  register char				 *p = number_string;
  register boolean			  number_found = false;

  IN(Number);
  while ( *string  &&
        (((*string - '0') >= 0  && (*string - '0') <= 9 )  ||
	   *string == '-'  ||  *string == '+' ) )
    {
    *p++ = *string++;
    number_found = true;
    }
  *p = 0;
  DEBUGst(Number-string,number_string);
  if ( number_found )
    {
    if ( *string )  string++;
    *n = LastNumber = atol( number_string );
    }
    else  *n = 0;
  DEBUGdt(N,*n );
  OUT(Number);
  return  string;
  }

static char *
Pixel( self, string, x, y, M, D, XO, YO, x_factor, y_factor )
  register struct ziposymbol		 *self;
  register char				 *string;
  register long				 *x, *y;
  register double			  M, D, XO, YO;
  register long				  x_factor, y_factor;
  {
  long					  X, Y;

  IN(Pixel);
  string = Number( self, string, &X );
  string = Number( self, string, &Y );
  *x = XO + (X * x_factor) * M;
  *y = YO - (Y * y_factor) * D;
  DEBUGdt(X-Pixel,*x );
  DEBUGdt(Y-Pixel,*y );
  OUT(Pixel);
  return  string;
  }
