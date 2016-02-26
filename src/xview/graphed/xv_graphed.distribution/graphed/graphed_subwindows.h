/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	GRAPHED_SUBWINDOWS
#define	GRAPHED_SUBWINDOWS

#include <xview/xview.h>

#include "graphed_svi.h"	/* Pixrects zu Server Images ! */


#define	MY_WIN_ROW_GAP       5
#define	MY_WIN_COLUMN_GAP    5
#define	MY_WIN_LEFT_MARGIN   5
#define	MY_WIN_RIGHT_MARGIN  5
#define	MY_WIN_TOP_MARGIN    10
#define	MY_WIN_BOTTOM_MARGIN 10

#define	MY_WIN_DEFAULTS \
	WIN_ROW_GAP,       MY_WIN_ROW_GAP,      \
	WIN_COLUMN_GAP,    MY_WIN_COLUMN_GAP,   \
	XV_LEFT_MARGIN,   MY_WIN_LEFT_MARGIN,  \
	XV_RIGHT_MARGIN,  MY_WIN_RIGHT_MARGIN, \
	XV_TOP_MARGIN,    MY_WIN_TOP_MARGIN,   \
	XV_BOTTOM_MARGIN, MY_WIN_BOTTOM_MARGIN
	
/************************************************************************/
/*									*/
/*				Subframes allgemein			*/
/*									*/
/************************************************************************/


extern	void	create_node_subframe ();
extern	void	show_node_subframe   ();

extern	void	create_node_defaults_subframe ();
extern	void	show_node_defaults_subframe   ();

extern	void	create_edge_subframe ();
extern	void	show_edge_subframe   ();

extern	void	create_edge_defaults_subframe ();
extern	void	show_edge_defaults_subframe   ();

extern	void	create_group_subframe ();
extern	void	show_group_subframe   ();

extern	void	show_file_selection_subframe ();
extern	void	show_font_edit_subframe      ();
extern	void	show_type_edit_subframe      ();

extern	Node	get_currently_edited_node  ();
extern	Edge	get_currently_edited_edge  ();
extern	Group	get_currently_edited_group ();
extern	Graph	get_currently_edited_graph ();


/************************************************************************/
/*									*/
/*	    Node- and Edge_defaults_subframe : info structure		*/
/*									*/
/************************************************************************/

typedef	struct {
	int		showing;
	Node_attributes	attr;
	Node_attributes	orig_attr;
}
	Node_defaults_subframe_info;

typedef	struct {
	int		showing;
	Edge_attributes	attr;
	Edge_attributes	orig_attr;
}
	Edge_defaults_subframe_info;


/************************************************************************/
/*									*/
/*				Base-frame				*/
/*									*/
/************************************************************************/


extern	Frame	base_frame;

extern	int	screenwidth;
extern	int	screenheight;



typedef	enum {
	FRAME_LABEL_FILENAME,
	FRAME_LABEL_EDITED,
	FRAME_LABEL_CONSTRAINED,
	FRAME_LABEL_GROUP_LABELLING_OPERATION,
	FRAME_LABEL_MODE_STRING
}
	Frame_label_attribute;
	
extern	void	set_base_frame_label ();



/************************************************************************/
/*									*/
/*				Notice					*/
/*									*/
/************************************************************************/

#include <xview/notice.h>

typedef enum {	/*fisprompt - vielleicht besser in user_header.h definieren*/
	PROMPT_ACCEPT,
	PROMPT_REFUSE,
	PROMPT_CANCEL
}	Prompt;


/************************************************************************/
/*									*/
/*				Panels					*/
/*									*/
/************************************************************************/

#include <xview/panel.h>

Panel		node_edge_panel;	/* Knoten- und Kantenformen	*/
Panel		menubar_panel;

extern	void		create_node_edge_panel ();
extern	void		create_menubar_panel ();

extern	Panel_item	create_graphed_color_selection_item ();


/************************************************************************/
/*									*/
/*				Canvas					*/
/*									*/
/************************************************************************/


#include <xview/canvas.h>
#include <xview/scrollbar.h>
#include <xview/cursor.h>

/* Die folgenden Event's werden in working_area_canvas an die Event-	*/
/* prozeduren weitergeleitet.						*/

#define	MY_CANVAS_PICK_EVENTS	WIN_IN_TRANSIT_EVENTS, \
				LOC_DRAG, \
				WIN_MOUSE_BUTTONS,     \
				WIN_LEFT_KEYS,         \
				WIN_TOP_KEYS,          \
				WIN_RIGHT_KEYS

#define	MY_CANVAS_KBD_EVENTS	WIN_ASCII_EVENTS


/* Standardgroesse fuer working_area_canvas	*/

#define	DEFAULT_WORKING_AREA_CANVAS_WIDTH  4000
#define	DEFAULT_WORKING_AREA_CANVAS_HEIGHT 4000
#define	DEFAULT_WORKING_AREA_WINDOW_WIDTH  500
#define	DEFAULT_WORKING_AREA_WINDOW_HEIGHT 500

typedef	struct	graphed_canvas {
	Canvas		canvas;
	Frame		frame;
	Scrollbar	vertical_scrollbar,
			horizontal_scrollbar;
	Pixwin		*pixwin;
	int		gridwidth;
}
	Graphed_canvas;

extern	Canvas		working_area_canvas;
extern	Graphed_canvas	canvases [];
	
	
#define	working_area_canvas_pixwin (canvases[wac_buffer].pixwin)

extern	int	create_canvas                 ();
extern	void	destroy_frame_and_canvas      ();

extern	void	init_graphed_colormap         ();
extern	void	set_colormap                  ();

extern	void	set_wac_mouse_position        ();
extern	int	set_working_area_size         ();
extern	void	scroll_working_area           ();
extern	void	scroll_working_area_relative  ();
extern	void	scroll_working_area_to_middle ();
extern	void	hide_working_area_cursor      ();
extern	void	show_working_area_cursor      ();

extern	void	get_scroll_offset                 ();
extern	void	translate_wac_to_base_frame_space ();

extern	void	set_canvas_frame_label ();


/************************************************************************/
/*									*/
/*			   Messge-textsw				*/
/*									*/
/************************************************************************/


#include <xview/textsw.h>

extern	Textsw	message_textsw;

extern	void	create_message_textsw ();


/************************************************************************/
/*									*/
/*				COLOR					*/
/*									*/
/************************************************************************/


#define GRAPHED_COLORMAPSIZE 16

extern	u_char	red   [GRAPHED_COLORMAPSIZE],
		green [GRAPHED_COLORMAPSIZE],
		blue  [GRAPHED_COLORMAPSIZE];
	
extern	void	init_graphed_colormap         ();





extern	char	*nei_strings           [];
extern	Server_image	nei_images            [];
extern	char	*nei_strings_for_cycle [];
extern	char	*nei_images_for_cycle  [];



extern	char	*nlp_strings           [];
extern	Server_image	nlp_images            [];
extern	char	*nlp_strings_for_cycle [];
extern	char	*nlp_images_for_cycle  [];


#endif
