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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipoimbd.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif

/* zipoimbd.c	Zip Object -- Imbed					      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: zipoimbd.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  06:58:59  rr2b
 * new R6tape branch
 *
 * Revision 1.3  1992/12/15  21:57:55  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.15  1991/09/12  16:43:01  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.14  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.13  1991/08/26  22:28:36  gk5g
 * Patch submitted by Guy Harris:
 * ATK seems not to have made up its mind whether PostScript format in printing is called "postscript" or "PostScript", nor even whether "troff" format is called "troff" or "Troff". Attached are some patches that teach some insets confused about this subject that they're called "PostScript" and "troff", respectively.
 * The patches to "zip" succeeded in improving the quality of the PostScript it generated, by causing it to be properly preceded with "\!" or whatever so that "*roff" would know what to do with it.
 *
 * Revision 2.12  1989/09/08  17:42:00  ghoti
 * removal of unused variables
 *
 * Revision 2.11  89/08/23  16:34:01  tom
 * Override GetModified to check for changes to Imbedded objects
 * 
 * Revision 2.10  89/08/03  12:19:10  ghoti
 * added inlcude of andrewos.h for bcopy and index (HPUX)
 * changed #include "" to #include <>
 * 
 * Revision 2.9  89/06/30  17:13:35  tom
 * Prevent dumps when Data/View object not accessible.
 * 
 * Revision 2.8  89/05/26  21:35:45  tom
 * <nop>
 * 
 * Revision 2.7  89/05/01  22:13:41  tom
 * Use special symbolic font-names.
 * 
 * Revision 2.6  89/02/08  16:50:40  ghoti
 * change copyright notice
 * 
 * Revision 2.5  89/02/07  19:46:50  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.4  88/11/23  18:43:24  tom
 * Improve format of object-type query.
 * 
 * Revision 2.3  88/11/18  21:10:55  tom
 * Handle variable line-widths.
 * 
 * Revision 2.2  88/10/11  20:34:15  tom
 * Change Printing interface to remove pane and figure args.
 * 
 * Revision 2.1  88/09/27  18:15:59  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:39:06  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:45:24  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Imbed

MODULE	zipoimbd.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  04/13/88	Created (TC Peters)
  11/23/88	Change object-type query format (TCP)
  05/01/89	Use symbolic font-names (TCP)
  06/30/89	Use Announce only when View-object exists (TCP)
		Various ensurance checks for existence of Object.
  08/07/89	Add Object_Modified override (TCP)
		Correct Print language/processor references.

END-SPECIFICATION  ************************************************************/

#include <andrewos.h>
#include <view.ih>
#include <dataobj.ih>
#include <zipobj.ih>
#include <zipoimbd.eh>

struct imbed
  {
  struct dataobject			 *data_object;
  struct view				 *view_object;

  };

static Load_Object();
static Draw();

boolean
zipoimbed__InitializeObject( classID, self )
  register struct classheader	         *classID;
  register struct zipoimbed	         *self;
  {
  IN(zipoimbed_InitializeObject);
  self->no_outline = false;
  OUT(zipoimbed_InitializeObject);
  return  true;
  }

void
zipoimbed__Destroy_Object( self, figure )
  register struct zipoimbed	         *self;
  register zip_type_figure		  figure;
  {
  register struct imbed			 *imbed =
		     (struct imbed *)figure->zip_figure_datum.zip_figure_anchor;

  IN(zipoimbed_Destroy_Object);
  view_Destroy( imbed->view_object );
  dataobject_Destroy( imbed->data_object );
  OUT(zipoimbed_Destroy_Object);
  }

char
zipoimbed__Object_Icon( self )
  register struct zipoimbed		 *self;
  {
  IN(zipoimbed__Object_Icon);
  OUT(zipoimbed__Object_Icon);
  return  'E';
  }

char
zipoimbed__Object_Icon_Cursor( self )
  register struct zipoimbed		 *self;
  {
  IN(zipoimbed__Object_Icon_Cursor);
  OUT(zipoimbed__Object_Icon_Cursor);
  return  'B';
  }

char
zipoimbed__Object_Datastream_Code( self )
  register struct zipoimbed		 *self;
  {
  IN(zipoimbed__Object_Datastream_Code);
  OUT(zipoimbed__Object_Datastream_Code);
  return  'I';
  }

long
zipoimbed__Show_Object_Properties( self, pane, figure )
  register struct zipoimbed		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  char					 *response;

  zipview_Set_Pane_Cursor( View, pane, '@', IconFontName );
  CurrentFigure = NULL;
  zipview_Query( View, "Outline Imbedded Object? [Yes]: ",
					NULL, &response );
  if ( *response == 'n'  ||  *response == 'N' )
      self->no_outline = true;
  zipview_Announce( View, "Draw Imbedded Object from Upper-left to Lower-right." );
  zipview_Set_Pane_Cursor( View, pane, 'B', CursorFontName );
  return  zip_ok;
  }

long
zipoimbed__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct zipoimbed		 *self;
  register zip_type_pane		  pane;
  register long				  action, x, y, clicks;
  register zip_type_point		  X, Y;
  {
  register long				  status = zip_ok;
  register zip_type_figure		  figure;
  char					 *response;

  IN(zipoimbed__Build_Object);
  switch ( action )
    {
    case view_LeftDown:
      if ( (status =
        zip_Create_Figure( Data, &CurrentFigure, NULL, zip_imbed_figure,
			   CurrentImage, 0 )) == zip_success )
	{
        zipoimbed_Set_Object_Point( self, CurrentFigure, zip_figure_origin_point,X, Y );
        zipoimbed_Set_Object_Point( self, CurrentFigure, zip_figure_auxiliary_point, X, Y );
	CurrentFigure->zip_figure_zoom_level = pane->zip_pane_zoom_level;
        pane->zip_pane_edit->zip_pane_edit_last_point_id = zip_figure_auxiliary_point;
	if ( self->no_outline )
	    CurrentFigure->zip_figure_style = 1;
	}
      break;
    case view_LeftUp:
      zipview_Set_Pane_Cursor( View, pane, 'B', CursorFontName );
      if ( figure = CurrentFigure )
	{
	if ( figure_x_point == figure_x_points(0)  &&
	     figure_y_point == figure_y_points(0) )
	  {
	  zipedit_Delete_Figure( Edit, figure, pane );
          break;
	  }
	  else
	  {
	  zipview_Query( View, "Enter Object Type [text]: ",
					NULL, &response );
	  if ( response == NULL  ||  *response == 0 )
	    response = "text";
	  zipview_Use_Working_Pane_Cursors( View );
	  if ( (status = Load_Object( self, figure, response, true )) != zip_ok )
    	    zipedit_Delete_Figure( Edit, figure, pane );
	  zipview_Use_Normal_Pane_Cursors( View );
	  }
	}
      /* Fall-thru */
    case view_LeftMovement:
      if ( CurrentFigure  &&  status == zip_ok )
	{
	zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
	zipview_Draw_Figure( View, CurrentFigure, pane );
        zipoimbed_Set_Object_Point( self, CurrentFigure, zip_figure_auxiliary_point, X, Y );
	zipview_Draw_Figure( View, CurrentFigure, pane );
	zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
	}
      break;
    }
  OUT(zipoimbed__Build_Object);
  return  status;
  }

struct view *
zipoimbed__Object_Hit( self, figure, action, x, y, clicks )
  register struct zipoimbed		 *self;
  register zip_type_figure		  figure;
  register enum view_MouseAction	  action;
  register long			          x, y, clicks;
  {
  register struct imbed			 *imbed = (struct imbed *)
						   figure->zip_figure_datum.zip_figure_anchor;
  register struct view			 *view = NULL;
  register zip_type_pane		  pane;
  register zip_type_figure		  next = figure->zip_figure_next;

  IN(zipoimbed__Object_Hit );
  if ( next )
    { DEBUG(Popping Figure to Front);
    zip_Unhook_Figure( Data, figure );
    while ( next  &&  next->zip_figure_next ) next = next->zip_figure_next;
    zip_Hook_Figure( Data, figure, next );
    pane = zipview_Which_Pane( View, x, y );
    if ( zipview_Condition( View, pane, figure, zip_draw ) )
      Draw( self, figure, pane, zip_draw );
    }
  if ( imbed )
    view = view_Hit( imbed->view_object, action,
		   view_EnclosedXToLocalX( imbed->view_object, x ),
		   view_EnclosedYToLocalY( imbed->view_object, y ), clicks );
  OUT(zipoimbed__Object_Hit );
  return  view;
  }

long
zipoimbed__Read_Object( self, figure )
  register struct zipoimbed		 *self;
  register zip_type_figure		  figure;
  {
  register long				  status = zip_ok;

  IN(zipoimbed__Read_Object);
  status = zip_Read_Figure( Data, figure );
  OUT(zipoimbed__Read_Object);
  return  status;
  }

long
zipoimbed__Read_Object_Stream( self, figure, file, id )
  register struct zipoimbed		 *self;
  register zip_type_figure		  figure;
  register FILE				 *file;
  register long				  id;
  {
  register long				  status = zip_ok, imbed_id;
  register struct imbed			 *imbed = (struct imbed *)
						   figure->zip_figure_datum.zip_figure_anchor;
  char					  line[512],
					  name[100], id_string[20];
  register char				 *s, *t;

  IN(zipoimbed__Read_Object_Stream);
  fgets( line, sizeof(line) - 1, file );
  DEBUGst(Line,line);
  if ( strncmp( line, "begindata", 9 ) == 0 )
    { DEBUG(Begindata OK);
    if ( s = (char *) index( line, '{' ) )
      { DEBUG(LP OK);
      t = name;  s++;
      while ( *s != ',' )  *t++ = *s++;  *t = 0;
      DEBUGst(Load-name,name);
      t = id_string;  s++;
      while ( *s != '}' )  *t++ = *s++;  *t = 0;
      DEBUGst(ID-string,id_string);
      imbed_id = atoi( id_string );
      DEBUGdt(ID,imbed_id);
      if ( (status = Load_Object( self, figure, name, false )) == zip_ok )
        { DEBUG(Load OK);
        imbed = (struct imbed *) figure->zip_figure_datum.zip_figure_anchor;
	status = dataobject_Read( imbed->data_object, file, imbed_id );
        }
      }
      else
      { DEBUG(LP ERROR);
      status = zip_failure;/*==s/b ???===*/
      }
    }
    else
    { DEBUG(Begindata EROR);
    status = zip_failure;/*==s/b ???===*/
    }
  DEBUGdt(Status,status);
  OUT(zipoimbed__Read_Object_Stream);
  return  status;
  }

static
Load_Object( self, figure, name, announce )
  register zip_type_figure		  figure;
  register struct zipoimbed		 *self;
  register char				 *name;
  register boolean			  announce;
  {
  register long				  status = zip_ok;
  register struct imbed			 *imbed;
  register struct dataobject		 *data_object;
  register struct view			 *view_object;
  char					 *view_name;
  char					  msg[512];

  IN(Load_Object);
  DEBUGst(Load-name,name);
  if ( announce  &&  View )
    {
    sprintf( msg, "Loading '%s' Object", name );
    zipview_Announce( View, msg );
    }
  figure->zip_figure_datum.zip_figure_anchor = NULL;
  if ( data_object = (struct dataobject *) class_NewObject( name ) )
    { DEBUG(DataObject Loaded);
    view_name = dataobject_ViewName( data_object );
    DEBUGst(ViewName,view_name);
    if ( view_object = (struct view *) class_NewObject( view_name ) )
      { DEBUG(ViewObject Loaded);
      figure->zip_figure_datum.zip_figure_anchor = (char *) calloc( 1, sizeof(struct imbed) );
      imbed = (struct imbed *) figure->zip_figure_datum.zip_figure_anchor;
      imbed->data_object = data_object;
      imbed->view_object = view_object;
      view_SetDataObject( view_object, data_object );
      if ( announce  &&  View )  zipview_Announce( View, "Done" );
      }
      else
      { DEBUG(ViewObject Load ERROR);
      sprintf( msg, "Unable to Load '%s'ViewObject", name );
      if ( View )
        zipview_Announce( View, msg );
        else  fprintf( stderr, "Zip: %s\n", msg );
      status = zip_failure; /*===s/b/object not found===*/
      }
    }
    else
    { DEBUG(DataObject Load ERROR);
    sprintf( msg, "Unable to Load '%s' DataObject", name );
    if ( View )
      zipview_Announce( View, msg );
      else  fprintf( stderr, "Zip: %s\n", msg );
    status = zip_failure; /*===s/b/object not found===*/
    }
  DEBUGdt(Status,status);
  OUT(Load_Object);
  return  status;
  }

long
zipoimbed__Write_Object( self, figure )
  register struct zipoimbed		 *self;
  register zip_type_figure		  figure;
  {
  register long				  status = zip_ok;
  register struct imbed			 *imbed =
		     (struct imbed *)figure->zip_figure_datum.zip_figure_anchor;

  IN(zipoimbed__Write_Object);
  if ( ! figure->zip_figure_state.zip_figure_state_deleted  &&
       imbed  &&  imbed->data_object )
    {
    figure->zip_figure_datum.zip_figure_anchor = NULL;
    if ( (status = zip_Write_Figure( Data, figure )) == zip_ok )
      dataobject_Write( imbed->data_object,
		    figure->zip_figure_image->zip_image_stream->zip_stream_file,
		    WriteStreamId,
		    1 /* Indicate Not Top-level */ );
    figure->zip_figure_datum.zip_figure_anchor = (char *) imbed;
    DEBUGdt(Status,status);
    }
  OUT(zipoimbed__Write_Object);
  return  status;
  }

long
zipoimbed__Object_Modified( self, figure  )
  register struct zipoimbed		 *self;
  register zip_type_figure		  figure;
  {
  register struct imbed			 *imbed =
		     (struct imbed *)figure->zip_figure_datum.zip_figure_anchor;
  register long				  modified = 0;

  IN(zipoimbed_Object_Modified);
  if ( imbed  &&  imbed->data_object )
    modified = dataobject_GetModified( imbed->data_object );
  OUT(zipoimbed_Object_Modified);
  return  modified;
  }

long
zipoimbed__Draw_Object( self, figure, pane )
  register struct zipoimbed		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoimbed__Draw_Object);
  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane, zip_draw );
  OUT(zipoimbed__Draw_Object);
  return  status;
  }

long
zipoimbed__Clear_Object( self, figure, pane )
  register struct zipoimbed		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoimbed__Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane, zip_clear );
  OUT(zipoimbed__Clear_Object);
  return  status;
  }

static
Draw( self, figure, pane, action )
  register struct zipoimbed		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register long				  action;
  {
  register long				  status = zip_ok;
  register struct imbed			 *imbed;
  register long				  left, top, width, height,
					  L, T, W, H;
  register short			  transfer_mode, shade;

  IN(Draw);
  left  = L = window_x_point;		 top    = T  = window_y_point;
  width = W = window_x_points(0) - left; height = H = window_y_points(0) - top;
  if ( width < 0 )
    { left  = L = window_x_points(0);  width = W = -width; }
  if ( height < 0 )
    { top  = T = window_y_points(0);   height = H = -height; }
  DEBUGdt(L,L);DEBUGdt(T,T);DEBUGdt(W,W);DEBUGdt(H,H);
  transfer_mode = zipview_GetTransferMode( View );
  if ( View->mouse_action != view_LeftMovement  &&
       (imbed = (struct imbed *)figure->zip_figure_datum.zip_figure_anchor)  &&
       action == zip_draw )
    {
    if ( (shade = zip_Contextual_Figure_Shade( Data, figure )) >= 1  &&
	  shade <= 100 )
      { DEBUGdt(Shade,shade);
      if ( (shade = ('0' + ((shade + 10) / 10)) - 1) > '9' )  shade = '9';
      DEBUGdt(Shade-index,shade);
      zipview_SetTransferMode( View, graphic_COPY );
      zipview_FillRectSize( View, left,top, width,height,
        zipview_Define_Graphic( View, zip_Define_Font( Data, ShadeFontName, NULL ), shade ));
      }
    zipview_SetTransferMode( View, graphic_BLACK );
    zipview_Set_Pane_Clip_Area( View, pane );
    DEBUG(LinkTree);
    view_LinkTree( imbed->view_object, View );
    DEBUG(InsertViewSize);
    view_InsertViewSize( imbed->view_object, View,
			 L + 1, T + 1, W - 2, H - 2 );
    DEBUG(FullUpdate); /*=== FORCE CLIPING TO INSIDE PANE SOMEHOW ===*/
    view_FullUpdate( imbed->view_object, view_FullRedraw,
		     L + 1, T + 1, W - 2, H - 2 );
    zipview_Set_Pane_Clip_Area( View, pane );
    }
    else
    if ( action == zip_clear )
      {
      zipview_SetTransferMode( View, graphic_WHITE );
      zipview_EraseRectSize( View, left+1, top+1, width-1, height-1 );
      }
  zipview_SetTransferMode( View, transfer_mode );
  if ( (zip_Contextual_Figure_Line_Width( Data, figure ) > 0  &&  figure->zip_figure_style == 0)  ||
       View->mouse_action == view_LeftMovement )
    zipview_DrawRectSize( View, left, top, width, height );
  if ( ExposePoints )
    zipoimbed_Expose_Object_Points( self, figure, pane );
  if ( HighlightPoints )
    zipoimbed_Highlight_Object_Points( self, figure, pane );

  OUT(Draw);
  return  status;
  }

long
zipoimbed__Print_Object( self, figure, pane )
  register struct zipoimbed		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register char				 *language, *processor;
  register struct imbed			 *imbed = (struct imbed *)
						   figure->zip_figure_datum.zip_figure_anchor;

  IN(zipoimbed__Print_Object);
  fprintf( zipprint_Printing_File( Print ), "%s %s\n%s %d %d %s\n",
	   zipprint_Printing_Prefix( Print ),
	   "gsave   % Save Zip Environment Surrounding Imbedded Object",
	   zipprint_Printing_Prefix( Print ),
	   print_x_point/100, print_y_points(0)/100,
	   "translate 1 -1 scale  % Set Zip Imbedded Object Environment"
	   );
  if ( imbed  &&  imbed->view_object )
    {
    processor = "troff";
    if ( Printing->zip_printing_processor == zip_postscript )
      processor = "PostScript";
    language = "troff";
    if ( Printing->zip_printing_language == zip_postscript )
      language = "PostScript";
    view_Print( imbed->view_object,
	      Printing->zip_printing_file, processor, language, 0 );
    }
  fprintf( zipprint_Printing_File( Print ), "%s %s\n",
	   zipprint_Printing_Prefix( Print ),
	   "grestore  % Restore Zip Environment Surrounding Imbedded Object");
  OUT(zipoimbed__Print_Object);
  return  status;
  }
