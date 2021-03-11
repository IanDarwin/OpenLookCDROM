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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipdf00.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/* zipedf.c	Zip Data-object	-- Figures			      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/*
    $Log: zipdf00.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  06:53:56  rr2b
 * new R6tape branch
 *
 * Revision 1.3  1992/12/15  21:57:11  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.7  1991/09/12  16:41:34  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.6  1990/08/21  14:14:25  sg08
 * Add Set_Figure_<Attribute> methods for color and line styles (SCG)
 *
 * Revision 2.5  89/02/17  18:06:29  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 * 
 * Revision 2.4  89/02/08  16:48:43  ghoti
 * change copyright notice
 * 
 * Revision 2.3  89/02/04  18:47:39  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.2  88/11/18  21:03:28  tom
 * Add Set_Figure_Line_Width.
 * 
 * Revision 2.1  88/09/27  18:10:32  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:25:31  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:43:09  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip Data-object -- Figures

MODULE	zipedf.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Figure facilities
	of the Zip Data-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  03/31/88	Created (TCP)
  11/17/88	Add Set_Figure_Line_Width (TCP/SG)
   08/14/90	Add Set_Figure_<Attribute> methods for color and line styles (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "dataobj.ih"
#include "graphic.ih"
#include "zipobj.ih"
#include "zip.ih"

#define	 Data			      self
#define	 Objects(i)		      ((*self->objects)[i])
#define	 SetStreamModified	      {zip_SetModified(self);\
					figure->zip_figure_image->zip_image_stream->\
					zip_stream_states.zip_stream_state_modified = 1;}

long
zip__Create_Figure( self, figure, name, type, image, peer )
  register struct zip		     *self;
  register zip_type_figure	     *figure;
  register char			     *name;
  register zip_type_image	      image;
  register zip_type_figure	      peer;
  {
  register int			      status = zip_ok;
  register zip_type_figure	      peer_ptr = peer;

  IN(zip_Create_Figure);
  if ( image )
    {
    if ( *figure = (zip_type_figure) calloc( 1, sizeof(struct zip_figure) ) )
      {
      (*figure)->zip_figure_image = image;
      (*figure)->zip_figure_image->zip_image_stream->
		zip_stream_states.zip_stream_state_modified = 1;
      if ( peer_ptr )
        {
        (*figure)->zip_figure_next = peer_ptr->zip_figure_next;
        peer_ptr->zip_figure_next = *figure;
        }
        else
        {
	peer_ptr = image->zip_image_figure_anchor;
	while ( peer_ptr  &&  peer_ptr->zip_figure_next )
	  peer_ptr = peer_ptr->zip_figure_next;
	if ( peer_ptr )
	  peer_ptr->zip_figure_next = *figure;
	  else
	  image->zip_image_figure_anchor = *figure;
        }
      (*figure)->zip_figure_type = type;
      (*figure)->zip_figure_line_width = 255;
      (*figure)->zip_figure_line_cap = -1;
      (*figure)->zip_figure_line_join = -1;
      if ( name  &&  *name )
        status = zip_Set_Figure_Name( self, *figure, name );
      }
      else  status = zip_insufficient_figure_space;
    }
    else  status = zip_image_non_existent;
  OUT(zip_Create_Figure);
  return status;
  }

long
zip__Destroy_Figure( self, figure )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  {
  register int			      status = zip_ok;

  IN(zip_Destroy_Figure);
  if ( figure )
    {
    if ( figure->zip_figure_name )
      {
      SetStreamModified;
      symtab_delete( ((zip_type_stream)zip_Containing_Figure_Stream( self, figure ))->
			zip_stream_symbol_table,
		   figure->zip_figure_name );
      free( figure->zip_figure_name );
      }
    if ( figure->zip_figure_datum.zip_figure_text )
      free( figure->zip_figure_datum.zip_figure_text );
    if ( figure->zip_figure_points )
      free( figure->zip_figure_points );
    if ( ! figure->zip_figure_state.zip_figure_state_unhooked )
      zip_Unhook_Figure( self, figure );
    free( figure );
    }
    else  status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zip_Destroy_Figure);
  return status;
  }

long
zip__Hook_Figure( self, figure, peer_figure )
  register struct zip		     *self;
  register zip_type_figure	      figure,  peer_figure;
  {
  register int				  status = zip_ok;

  IN(zip_Hook_Figure);
  if ( figure  &&  peer_figure )
    {
    if ( figure->zip_figure_state.zip_figure_state_unhooked )
      {
      figure->zip_figure_next = peer_figure->zip_figure_next; 
      peer_figure->zip_figure_next = figure;
      figure->zip_figure_image = peer_figure->zip_figure_image;
      zip_Set_Figure_Name( self, figure, zip_Figure_Name( self, figure ) );
      figure->zip_figure_state.zip_figure_state_unhooked = false;
      SetStreamModified;
      }
      else
      status = zip_failure; /*=== s/b "Already Hooked" ==*/
    }
    else
    status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zip_Hook_Figure);
  return status;
  }

long
zip__Unhook_Figure( self, figure )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  {
  register int			      status = zip_ok;
  register zip_type_figure	      figure_ptr;

  IN(zip_Unhook_Figure);
  if ( figure )
    {
    if ( ! figure->zip_figure_state.zip_figure_state_unhooked )
      {
      SetStreamModified;
      figure_ptr = (zip_type_figure) figure->zip_figure_image->zip_image_figure_anchor;
      if ( figure_ptr == figure )
	figure->zip_figure_image->zip_image_figure_anchor = figure->zip_figure_next;
	else
	while( figure_ptr  &&  figure_ptr->zip_figure_next != figure )
	  figure_ptr = figure_ptr->zip_figure_next;
      figure_ptr->zip_figure_next = figure->zip_figure_next;
      figure->zip_figure_next = NULL;
      figure->zip_figure_image = NULL;
      figure->zip_figure_state.zip_figure_state_unhooked = true;
      }
      else  status = zip_failure; /*=== s/b/ "Already Un-hooked" ===*/
    }
    else  status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zip_Unhook_Figure);
  return status;
  }

long
zip__Set_Figure_Name( self, figure, name )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  register char			     *name;
  {
  register int			      status = zip_ok;
  zip_type_figure		      duplicate;

  IN(zip_Set_Figure_Name);
  if ( figure )
    {
    DEBUGst(Given Name ,name);
    DEBUGst(Figure Name,figure->zip_figure_name);
    SetStreamModified;
    if ( figure->zip_figure_name )
      *figure->zip_figure_name = '\0';
    if ( name  &&  *name != '\0' )
      {
      if ( symtab_find( figure->zip_figure_image->zip_image_stream->zip_stream_symbol_table,
		      name, &duplicate ) == 0
/*===something wrong in symtab===*/ && strcmp(name,duplicate->zip_figure_name)==0 )
        {
        status = zip_duplicate_figure_name;
/*===
printf( "DUP FIG NAME '%s' (Stream '%s'  Figure '%s')\n",
name, figure->zip_figure_image->zip_image_stream->zip_stream_name,duplicate->zip_figure_name);
===*/
        }
        else
        {
        if ( (figure->zip_figure_name = (char *) malloc( strlen( name ) + 1 )) == NULL )
          status = zip_insufficient_figure_space;
          else
          {
          strcpy( figure->zip_figure_name, name );
          symtab_add( figure->zip_figure_image->zip_image_stream->zip_stream_symbol_table,
		    figure->zip_figure_name, figure );
          }
	}
      }
      else
      {
      if ( figure->zip_figure_name )
        free( figure->zip_figure_name );
      figure->zip_figure_name = NULL;
      }
    }
    else  status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zip_Set_Figure_Name);
  return status;
  }

long
zip__Set_Figure_Text( self, figure, text )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  {
  register int			      status = zip_ok;

  IN(zip_Set_Figure_Text);
  if ( figure )
    {
    DEBUGst(Given Text,text);
    DEBUGst(Figure Name,figure->zip_figure_name);
    SetStreamModified;
    if ( text )
      if ( (figure->zip_figure_datum.zip_figure_text = (char *) malloc( strlen( text ) + 1 )) != NULL )
        strcpy( figure->zip_figure_datum.zip_figure_text, text );
        else
        status = zip_insufficient_figure_space;
      else figure->zip_figure_datum.zip_figure_text = NULL;
    }
    else
    status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zip_Set_Figure_Text);
  return status;
  }

long
zip__Set_Figure_Pattern( self, figure, pattern )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  register char			      pattern;
  {
  register int			      status = zip_ok;

  IN(zip_Set_Figure_Pattern);
  if ( figure )
    {
    DEBUGct(Given Pattern,pattern);
    DEBUGst(Figure Name,figure->zip_figure_name);
    SetStreamModified;
    figure->zip_figure_fill.zip_figure_pattern = pattern;
    if ( pattern )
      figure->zip_figure_mode.zip_figure_mode_patterned = on;
      else
      figure->zip_figure_mode.zip_figure_mode_patterned = off;
    }
    else
    status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zip_Set_Figure_Pattern);
  return status;
  }

long
zip__Set_Figure_Shade( self, figure, shade )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  register long			      shade;
  {
  register int			      status = zip_ok;

  IN(zip_Set_Figure_Shade);
  if ( figure )
    {
    DEBUGdt(Given Shade,shade);
    DEBUGst(Figure Name,figure->zip_figure_name);
    SetStreamModified;
    status = zipobject_Set_Object_Shade(
		Objects(figure->zip_figure_type), figure, shade );
    }
    else
    status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zip_Set_Figure_Shade);
  return status;
  }

long
zip__Set_Figure_Font( self, figure, name )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  register char			     *name;
  {
  register int			     status = zip_ok;
  short				     font = NULL;

  IN(zip_Set_Figure_Font);
  if ( figure  &&  name  &&  zip_Define_Font( self, name, &font ) )
    {
    DEBUGst(Given Font,font);
    DEBUGst(Figure Name,figure->zip_figure_name);
    SetStreamModified;
    status = zipobject_Set_Object_Font(
		Objects(figure->zip_figure_type), figure, font );
    }
    else
    {
    if ( figure == NULL )
      status = zip_figure_non_existent;
    else
    if ( name == NULL )
      figure->zip_figure_font = NULL;
    }
  ZIP_STATUS();
  OUT(zip_Set_Figure_Font);
  return status;
  }

long
zip__Set_Figure_Mode( self, figure, mode )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  {
  register int			      status = zip_ok;

  IN(zip_Set_Figure_Mode);
  if ( figure )
    {
    DEBUGst(Figure Name,figure->zip_figure_name);
    SetStreamModified;
    figure->zip_figure_mode.zip_figure_mode_top    = off;
    figure->zip_figure_mode.zip_figure_mode_middle = off;
    figure->zip_figure_mode.zip_figure_mode_bottom = off;
    figure->zip_figure_mode.zip_figure_mode_left   = off;
    figure->zip_figure_mode.zip_figure_mode_center = off;
    figure->zip_figure_mode.zip_figure_mode_right  = off;
    figure->zip_figure_mode.zip_figure_mode_halo   = off;

    if ( mode & zip_top )	figure->zip_figure_mode.zip_figure_mode_top    = on;
    else
    if ( mode & zip_middle )	figure->zip_figure_mode.zip_figure_mode_middle = on;
    else
    if ( mode & zip_bottom )	figure->zip_figure_mode.zip_figure_mode_bottom = on;

    if ( mode & zip_left )	figure->zip_figure_mode.zip_figure_mode_left   = on;
    else
    if ( mode & zip_center )	figure->zip_figure_mode.zip_figure_mode_center = on;
    else
    if ( mode & zip_right )	figure->zip_figure_mode.zip_figure_mode_right  = on;

    if ( mode & zip_halo )	figure->zip_figure_mode.zip_figure_mode_halo   = on;
    }
    else
    status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zip_Set_Figure_Mode);
  return status;
  }

long
zip__Set_Figure_Line_Width( self, figure, width )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  register long			      width;
  {
  register int			      status = zip_ok;

  IN(zip_Set_Figure_Line_Width);
  if ( figure )
    {
    DEBUGdt(Given Width,width);
    DEBUGst(Figure Name,figure->zip_figure_name);
    SetStreamModified;
    figure->zip_figure_line_width = width;
    }
    else
    status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zip_Set_Figure_Line_Width);
  return status;
  }

long
zip__Set_Figure_Line_Dash( self, figure, pattern, offset, type )
  register struct zip			*self;
  register zip_type_figure		figure;
  register char				*pattern;
  register int				offset;
  register short			type;
  {
  register int			      status = zip_ok;

      IN(zip_Set_Figure_Line_Dash);
      if ( figure )
      {
	  if ( pattern )
            {
	      if ( figure->zip_figure_line_dash_pattern = malloc( strlen( pattern ) + 1 ))
		  strcpy( figure->zip_figure_line_dash_pattern, pattern );
	      figure->zip_figure_line_dash_offset = offset;
	      figure->zip_figure_line_dash_type = type;
	    }
	  else figure->zip_figure_line_dash_type = graphic_LineSolid;
	  SetStreamModified;
      }
      else
	  status = zip_figure_non_existent;
      ZIP_STATUS();
      OUT(zip_Set_Figure_Line_Dash);
      return status;
  }

long
zip__Set_Figure_Line_Cap( self, figure, cap )
  register struct zip			*self;
  register zip_type_figure		figure;
  register short			cap;
  {
  register int			      status = zip_ok;

      IN(zip_Set_Figure_Line_Cap);
      if ( figure )
      {
	  SetStreamModified;
	  figure->zip_figure_line_cap = cap;
      }
      else
	  status = zip_figure_non_existent;
      ZIP_STATUS();
      OUT(zip_Set_Figure_Line_Cap);
      return status;
  }

long
zip__Set_Figure_Line_Join( self, figure, join )
  register struct zip			*self;
  register zip_type_figure		figure;
  register short			join;
  {
  register int			      status = zip_ok;

      IN(zip_Set_Figure_Line_Join);
      if ( figure )
      {
	  SetStreamModified;
	  figure->zip_figure_line_join = join;
      }
      else
	  status = zip_figure_non_existent;
      ZIP_STATUS();
      OUT(zip_Set_Figure_Line_Join);
      return status;
  }

long
zip__Set_Figure_Line_Color( self, figure, red, green, blue )
  register struct zip			*self;
  register zip_type_figure		figure;
  register double			red, green, blue;
  {
  register int			      status = zip_ok;

     IN(zip_Set_Figure_Line_Color);
     if ( figure )
      {
	  if ( figure->zip_figure_color_values == NULL )
	    figure->zip_figure_color_values = zip_Allocate_Color_Values( self );
          if ( figure->zip_figure_color_values &&
	     ( figure->zip_figure_color_values->line == NULL ))
	    figure->zip_figure_color_values->line = zip_Allocate_Color( self );
 	  if (  figure->zip_figure_color_values && figure->zip_figure_color_values->line )
            {
  	      SetStreamModified;
  	      figure->zip_figure_color_values->line->red = red;
  	      figure->zip_figure_color_values->line->green = green;
  	      figure->zip_figure_color_values->line->blue = blue;
	    }
	  else status = zip_failure;
      }
      else
	  status = zip_figure_non_existent;
      ZIP_STATUS();
      OUT(zip_Set_Figure_Line_Color);
      return status;
  }

long
zip__Set_Figure_FillFG_Color( self, figure, red, green, blue )
  register struct zip			*self;
  register zip_type_figure		figure;
  register double			red, green, blue;
  {
  register int			      status = zip_ok;

     IN(zip_Set_Figure_FillFG_Color);
     if ( figure )
      {
	  if ( figure->zip_figure_color_values == NULL )
	    figure->zip_figure_color_values = zip_Allocate_Color_Values( self );
          if ( figure->zip_figure_color_values &&
	     ( figure->zip_figure_color_values->fillfg == NULL ))
	    figure->zip_figure_color_values->fillfg = zip_Allocate_Color( self );
 	  if (  figure->zip_figure_color_values && figure->zip_figure_color_values->fillfg )
            {
  	      SetStreamModified;
  	      figure->zip_figure_color_values->fillfg->red = red;
  	      figure->zip_figure_color_values->fillfg->green = green;
  	      figure->zip_figure_color_values->fillfg->blue = blue;
	    }
	  else status = zip_failure;
      }
      else
	  status = zip_figure_non_existent;
      ZIP_STATUS();
      OUT(zip_Set_Figure_FillFG_Color);
      return status;
  }

long
zip__Set_Figure_FillBG_Color( self, figure, red, green, blue )
  register struct zip			*self;
  register zip_type_figure		figure;
  register double			red, green, blue;
  {
  register int			      status = zip_ok;

     IN(zip_Set_Figure_FillBG_Color);
     if ( figure )
      {
	  if ( figure->zip_figure_color_values == NULL )
	    figure->zip_figure_color_values = zip_Allocate_Color_Values( self );
          if ( figure->zip_figure_color_values &&
	     ( figure->zip_figure_color_values->fillbg == NULL ))
	    figure->zip_figure_color_values->fillbg = zip_Allocate_Color( self );
 	  if (  figure->zip_figure_color_values && figure->zip_figure_color_values->fillbg )
            {
  	      SetStreamModified;
  	      figure->zip_figure_color_values->fillbg->red = red;
  	      figure->zip_figure_color_values->fillbg->green = green;
  	      figure->zip_figure_color_values->fillbg->blue = blue;
	    }
	  else status = zip_failure;
      }
      else
	  status = zip_figure_non_existent;
      ZIP_STATUS();
      OUT(zip_Set_Figure_FillBG_Color);
      return status;
  }

long
zip__Set_Figure_Point( self, figure, point, x, y )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  {
  register int			      status = zip_ok;

  IN(zip_Set_Figure_Point);
  if ( figure )
    {
    SetStreamModified;
    status = zipobject_Set_Object_Point(
		Objects(figure->zip_figure_type), figure, point, x, y );
    }
    else  status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zip_Set_Figure_Point);
  return status;
  }

long
zip__Adjust_Figure_Point_Suite( self, figure, x_delta, y_delta )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  {
  register int			      status = zip_ok;

  IN(zip_Adjust_Figure_Point_Suite);
  if ( figure )
    {
    SetStreamModified;
    status = zipobject_Adjust_Object_Point_Suite(
		Objects(figure->zip_figure_type), figure, x_delta, y_delta );
    }
    else  status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zip_Adjust_Figure_Point_Suite);
  return status;
  }

long
zip__Change_Figure_Point( self, figure, old_x, old_y, new_x, new_y )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  register long			      old_x, old_y, new_x, new_y;
  {
    SetStreamModified;
return  zip_failure;/*===*/
  }

long
zip__Remove_Figure_Point( self, figure, old_x, old_y )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  register long			      old_x, old_y;
  {
    SetStreamModified;
return  zip_failure;/*===*/
  }

long
zip__Add_Figure_Point( self, figure, new_x, new_y )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  register long			      new_x, new_y;
  {
    SetStreamModified;
return  zip_failure;/*===*/
  }


struct zip_figure *
zip__Figure( self, name )
  register struct zip		     *self;
  register char			     *name;
  {
  zip_type_figure		      figure = NULL;
  register zip_type_stream_chain      stream_link = StreamAnchor;

  IN(zip_Figure);
  while ( stream_link )
    {
    if ( stream_link->zip_stream_chain_ptr->zip_stream_symbol_table )
       if ( symtab_find( stream_link->zip_stream_chain_ptr->zip_stream_symbol_table,
	     name, &figure ) == 0 )
	  goto exit_point;
    stream_link = stream_link->zip_stream_chain_next;
    }
  exit_point:
  OUT(zip_Figure);
  return figure;
  }

struct zip_figure *
zip__Image_Figure( self, image, name )
  register struct zip		     *self;
  register struct zip_image	     *image;
  register char			     *name;
  {
  zip_type_figure		      figure = NULL;

  IN(zip_Image_Figure);
  if ( image  &&  name )
    if ( image->zip_image_stream->zip_stream_symbol_table )
      symtab_find( image->zip_image_stream->zip_stream_symbol_table, name, &figure );
  OUT(zip_Image_Figure);
  return figure;
  }

struct zip_figure *
zip__Stream_Figure( self, stream, name )
  register struct zip		     *self;
  register struct zip_stream	     *stream;
  register char			     *name;
  {
  zip_type_figure		      figure = NULL;

  IN(zip_Stream_Figure);
  if ( stream  &&  name )
    if ( stream->zip_stream_symbol_table )
      symtab_find( stream->zip_stream_symbol_table, name, &figure );
  OUT(zip_Stream_Figure);
  return figure;
  }

long
zip__Allocate_Figure_Points_Vector( self, anchor )
  register struct zip		     *self;
  register zip_type_point_pairs	     *anchor;
  {
  register int			      status = zip_ok;

  IN(zip__Allocate_Figure_Points_Vector);
  if ( (*anchor = (zip_type_point_pairs)
	 malloc( sizeof(struct zip_point_pairs) +
	 (zip_points_allocation * sizeof(struct zip_point_pair)) )) == NULL )
    status = zip_insufficient_figure_space;
    else
    (*anchor)->zip_points_count = 0;
  OUT(zip__Allocate_Figure_Points_Vector);
  return status;
  }


int
zip__Enlarge_Figure_Points_Vector( self, anchor )
  register struct zip		     *self;
  register zip_type_point_pairs	     *anchor;
  {
  register int			      status = zip_ok;

  IN(zip__Enlarge_Figure_Points_Vector);
  if ( (*anchor = (zip_type_point_pairs)
	 realloc( *anchor, sizeof(struct zip_point_pairs) +
		 ((*anchor)->zip_points_count * sizeof(struct zip_point_pair)) +
		 (zip_points_allocation * sizeof(struct zip_point_pair)) )) == NULL )
    status = zip_insufficient_figure_space;
  OUT(zip__Enlarge_Figure_Points_Vector);
  return status;
  }
