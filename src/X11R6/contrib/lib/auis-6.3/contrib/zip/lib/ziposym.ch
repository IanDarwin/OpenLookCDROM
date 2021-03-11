/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/*
ziposymbol.H

  07/10/88	Create for ATK (TCP)
*/

#define  ziposymbol_VERSION    1

#define  Data				(self->header.zipobject.data_object)
#define  Env				(Data->env)
#define  View				(self->header.zipobject.view_object)
#define  Edit				(self->header.zipobject.edit_object)
#define  Print				(self->header.zipobject.print_object)


struct symbol
  {
  short					  symbol_serial;
  char					 *symbol_icon;
  char					 *symbol_name;
  char					 *symbol_algorithm;
  short					  symbol_left, symbol_top, symbol_right, symbol_bottom;
  boolean				  symbol_highlighted;
  };

struct symbol_set
  {
  char					 *set_name;
  char					 *set_path;
  FILE					 *set_file;
  struct symbol				 *set_symbols;
  short					  set_symbols_count;
  short					  set_left, set_top, set_right, set_bottom;
  boolean				  set_highlighted;
  };


class ziposymbol[ziposym] : ziporect
  {
classprocedures:
  InitializeObject( struct ziposymbol *self )		returns boolean;
  FinalizeObject( struct ziposymbol *self );

overrides:
  Object_Icon()						returns char;
  Object_Icon_Cursor()					returns char;
  Object_Datastream_Code()				returns char;

  Build_Object( pane, action, x, y, clicks, X, Y )	returns long;
  Show_Object_Properties( pane, figure )		returns long;
  Draw_Object( object, pane )				returns long;
  Clear_Object( object, pane )				returns long;
  Print_Object( object, pane )				returns long;

data:
  long					  symbol_view_left,  symbol_view_top,
					  symbol_view_width, symbol_view_height,
					  symbol_view_right, symbol_view_bottom;
  boolean				  symbol_selected;
  char					  selected_symbol_set_name[257],
					  selected_symbol_index_name[25];
  struct symbol_set			 *selected_symbol_set;
  short					  selected_symbol_set_index;
  struct symbol				 *selected_symbol;
  struct symbol_set			 *symbol_set_displayed,
					 *current_symbol_set;
  short					  current_symbol_set_index;
  struct graphic			 *gray_graphic;
  char					  gray_shade, line_width, line_style;
  long					  last_number;
  boolean				  outstanding_surround;
  long					  outstanding_left, outstanding_top,
					  outstanding_width, outstanding_height;
  struct zip_paths			 *library_paths;
  };

