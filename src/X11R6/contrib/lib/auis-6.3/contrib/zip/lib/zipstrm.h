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

/* zipefs00.h	Zip Subroutine Library Stream Objects Header		      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: zipstrm.h,v $
 * Revision 1.3  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.2.1.1  1993/02/02  07:02:55  rr2b
 * new R6tape branch
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.6  1991/09/12  20:09:22  bobg
 * Update copyright notice
 *
 * Revision 2.5  1990/08/21  14:42:11  sg08
 * Add color and line style fields
 *
 * Revision 2.4  89/02/08  16:52:12  ghoti
 * change copyright notice
 * 
 * Revision 2.3  89/02/07  20:30:51  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.2  88/11/18  21:16:00  tom
 * Add line_width field.
 * 
 * Revision 2.1  88/09/27  18:19:22  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:48:42  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:47:22  tom
 * Initial revision
 * 
 * Revision 1.1  87/10/28  21:39:00  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	Zip Subroutine Library Stream Objects Header

MODULE	zipefs00.h

NOTICE	IBM Internal Use Only

DESCRIPTION
	This file is to be included in the compilation of both client-programs
	and the Zip Subroutine Library modules. It defines the Stream structure
	and symbolics related to that object.

HISTORY
  05/01/85	Created (TCP)
  12/10/85	Eliminated Image Chain-units (TCP)
		Added Object Storage Pool
  07/20/86	Introduced zip_Close_Stream as macro: to nullify anchor (TCP)
  01/27/87	Change types to pixel/point appropriately (TCP)
  08/25/87      Added symbolics EoB and zip_default_buffer_size (SCG)
  08/28/87	Migration: change zip_stream_file_descriptor from file to FILE (TCP)
  03/31/88	Revised for ATK (TCP)
  11/17/88	Add line_width field (TCP/SG)
   08/16/90	Add color and line style fileds (SCG)

END-SPECIFICATION  ************************************************************/

/* Stream Architecture *********************************************************

1)	COMMENTARY: Line beginning with '#' or 'white-space' (blank, tab, new-line) ---

		#	<comment>\n
			<comment>\n
			
2)	IMAGE SPECIFICATION: Line beginning with '{' or '}' ---

		{	[begin-image-name]\n
		}	[end-image-name]\n

3)	FIGURE SPECIFICATION: Line beginning with '*' ---

		*<figure-type>;<x-position>,<y-position>[;<zoom-level>]\n
	
			figure-type: n
		
				A	Text
				B	Dot
				C	Line
				D	Poly-Line
				E	Polygon
				F	Trapezoid
				G	Rectangle
				H	Poly-Dot
				I	Spray
				J	Circle
				K	Picture

	
			x-position,y-position:
		
				[-]n,[-]n
	
			zoom-level:
	
				[-]n	(defaults to 0)
	

4)	ATTRIBUTE SPECIFICATION: Line beginning with one of following characters ---

		N	<name>\n
				characters upto new-line (trimmed)
		T	<text>\n
				characters upto new-line (trimmed)
		F	<font-name>\n
				characters upto new-line (trimmed)
		Z	<zoom-level>\n
				[-]n
		C	<color>[;...]\n
				1.n	Red
				2.n	Green
				3.n	Blue
				( "n" is percent of each color component )
		P	<pattern>\n
				(character within specified Font or
				 default "Pattern Font" (shape10{?aptpatterns?}))
		R	<file-reference>
				name of file containing zip material (eg, Picture)

5)	POSITIONING SPECIFICATION: Line beginning with '>' ---

		>	<next-x-position,next-y-position>[;...]\n
				[-]n,[-]n


EXAMPLE ---

{Pennsylvania
{Allegehny
{Pittsburgh
*4;113,204;0
>135,209;155,301;-401,300;-401,400;289,-153
.
.
.
}Pittsburgh
.
.
.
}Allegehny
.
.
.
}Pennsylvania



****************************************************************************************/

/*===
#define  Xip_Close_Stream( x )\
	{ziP_Close_Stream(x);\
	 (x) = NULL;}
===*/
#define   EoB					  036
#define   zip_default_buffer_size		  1025

typedef  struct zip_stream_attributes		  zip_type_stream_attributes;

struct zip_stream_attributes
  {
  unsigned int					  zip_stream_attribute_reference	:1;
  unsigned int					  zip_stream_attribute_Y		:1;
  unsigned int					  zip_stream_attribute_Z		:1;
  };


typedef  struct zip_stream_states		  zip_type_stream_states;

struct zip_stream_states
  {
  unsigned int					  zip_stream_state_exposed		:1;
  unsigned int					  zip_stream_state_hidden		:1;
  unsigned int					  zip_stream_state_modified		:1;
  };


typedef  struct zip_stream_file_states		  zip_type_stream_file_states;

struct zip_stream_file_states
  {
  unsigned int					  zip_stream_file_state_open_read	:1;
  unsigned int					  zip_stream_file_state_open_write	:1;
  unsigned int					  zip_stream_file_state_open_update	:1;
  };


typedef  struct zip_stream			 *zip_type_stream;

struct zip_stream
  {
  char						 *zip_stream_name;
  zip_type_stream_attributes			  zip_stream_attributes;
  zip_type_stream_states			  zip_stream_states;
  zip_type_stream_file_states			  zip_stream_file_states;
  char						 *zip_stream_file_name;
  char						 *zip_stream_file_path;
  char						 *zip_stream_file_full_name;
  FILE						 *zip_stream_file;
  zip_type_point				  zip_stream_greatest_x;
  zip_type_point				  zip_stream_least_x;
  zip_type_point				  zip_stream_greatest_y;
  zip_type_point				  zip_stream_least_y;
  char						 *zip_stream_text;
  short						  zip_stream_font;
  unsigned char					  zip_stream_type;
  unsigned char					  zip_stream_zoom_level;
  unsigned char					  zip_stream_line_width;
  short						  zip_stream_line_cap;
  short						  zip_stream_line_join;
  char						  *zip_stream_line_dash_pattern;
  int						  zip_stream_line_dash_offset;
  short						  zip_stream_line_dash_type;
  struct zip_color_values			  *zip_stream_color_values;
  union
    {
    unsigned char				  zip_stream_pattern;
    unsigned char				  zip_stream_shade;
    }			zip_stream_fill;   
  zip_type_image				  zip_stream_image_anchor;
  char						 *zip_stream_client_data;
  char						 *zip_stream_data_stream;
  struct symtab_type				 *zip_stream_symbol_table;
  char						 *zip_stream_object_pool;
  zip_type_point				  zip_stream_pseudo_x_offset;
  zip_type_point				  zip_stream_pseudo_y_offset;
  };

typedef  struct zip_stream_chain		 *zip_type_stream_chain;

struct zip_stream_chain
  {
  zip_type_stream_chain				  zip_stream_chain_next;
  zip_type_stream				  zip_stream_chain_ptr;
  };
