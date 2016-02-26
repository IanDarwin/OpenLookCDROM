/* Copyright Universitaet Passau 1990 */
/* GraphEd Source, 1986-1990 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				edge_sf.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt die Verwaltung des edge_defaults_subframe	*/
/*		zum Einstellen der Default-Knotenparameter.		*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "graphed_subwindows.h"

#include "graphed_svi.h"


/************************************************************************/
/*									*/
/*		GLOBALE FUNKTIONEN / PROZEDUREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_edge_defaults_subframe ()			*/
/*	void	show_edge_defaults_subframe   (info, complete_proc)	*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN / PROZEDUREN				*/
/*									*/
/************************************************************************/


static	void		hide_edge_defaults_subframe           ();
static			notify_edge_defaults_subframe_buttons ();
static			notify_edge_defaults_subframe_cycles  ();
static	Panel_setting	notify_arrowlength_selection ();
static	Panel_setting	notify_arrowangle_selection  ();



/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/

static	Frame		edge_defaults_subframe;
static	Panel		edge_defaults_subframe_panel;

static	Panel_item	edge_defaults_subframe_quit_button;
static	Panel_item	edge_defaults_subframe_arrowlength_selection;
static	Panel_item	edge_defaults_subframe_arrowangle_selection;
static	Panel_item	edge_defaults_subframe_label_visibility_cycle;
static	Panel_item      edge_defaults_subframe_type_cycle;
static	Panel_item      edge_defaults_subframe_font_cycle;
static	Panel_item      edge_defaults_subframe_edgecolor_selection;

static	Edge_defaults_subframe_info	*info;
static	void				(*completion_proc)();
static	int				showing_edge_defaults_subframe = FALSE;



/************************************************************************/
/*									*/
/*		EDGE_SUBFRAME AUFBAUEN UND VERWALTEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_edge_defaults_subframe ()			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	show_edge_defaults_subframe (info, completion_proc)	*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	void	hide_edge_defaults_subframe ()			*/
/*									*/
/*	Laesst den Kantensubframe bis zum naechsten Gebrauch wieder	*/
/*	verschwinden (edge_defaults_subframe wird aus Zeitgruenden	*/
/*	nicht physikalisch geloescht).					*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	install_current_elv_in_edge_defaults_subframe        ()	*/
/*	void	install_current_arrowlength_in_edge_defaults_subframe()	*/
/*	void	install_current_arrowangle_in_edge_defaults_subframe ()	*/
/*									*/
/************************************************************************/



void	create_edge_defaults_subframe ()
{
	Menu		edge_defaults_subframe_menu;
	int		row_count = 0;
	static	char	arrowangle_selection_notify_string [] = {
				CTRL_A, CTRL_B, CTRL_C, CR, '\0'};
	static	char	arrowlength_selection_notify_string [] = {
				CTRL_A, CTRL_B, CTRL_C, CTRL_D, CR, '\0'};

	edge_defaults_subframe = (Frame)xv_create(base_frame, FRAME_CMD,
		FRAME_LABEL,		"Edge Defaults",
		FRAME_CMD_PUSHPIN_IN,	TRUE,
		FRAME_DONE_PROC,	hide_edge_defaults_subframe,
		WIN_CLIENT_DATA,	empty_edge,
		NULL);

	edge_defaults_subframe_panel = (Panel)xv_get(edge_defaults_subframe, FRAME_CMD_PANEL);

	edge_defaults_subframe_label_visibility_cycle = xv_create(edge_defaults_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(edge_defaults_subframe_panel, 0),
		XV_Y,			xv_row(edge_defaults_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"label",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_CHOICE_STRINGS,	boolean_ordered_list("visible",TRUE, "invisible",FALSE), 0,
		PANEL_NOTIFY_PROC,	notify_edge_defaults_subframe_cycles,
		NULL);

	row_count ++;
	edge_defaults_subframe_arrowlength_selection = xv_create(edge_defaults_subframe_panel, PANEL_TEXT,
		XV_X,				xv_col(edge_defaults_subframe_panel, 0),
		XV_Y,				xv_row(edge_defaults_subframe_panel, row_count),
		PANEL_LABEL_STRING,		"arrow length",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			"",
		PANEL_VALUE_STORED_LENGTH,	5,
		PANEL_VALUE_DISPLAY_LENGTH,	5,
		PANEL_NOTIFY_STRING,		arrowlength_selection_notify_string,
		PANEL_NOTIFY_LEVEL,		PANEL_SPECIFIED,
		PANEL_NOTIFY_PROC,		notify_arrowlength_selection,
		NULL);
	
	edge_defaults_subframe_arrowangle_selection = xv_create(edge_defaults_subframe_panel, PANEL_TEXT,
		XV_X,				xv_col(edge_defaults_subframe_panel, 22),
		XV_Y,				xv_row(edge_defaults_subframe_panel, row_count),
		PANEL_LABEL_STRING,		"angle",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			"",
		PANEL_VALUE_STORED_LENGTH,	5,
		PANEL_VALUE_DISPLAY_LENGTH,	5,
		PANEL_NOTIFY_STRING,		arrowangle_selection_notify_string,
		PANEL_NOTIFY_LEVEL,		PANEL_SPECIFIED,
		PANEL_NOTIFY_PROC,		notify_arrowangle_selection,
		NULL);

	row_count ++;
	edge_defaults_subframe_type_cycle = xv_create(edge_defaults_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(edge_defaults_subframe_panel, 0),
		XV_Y,			xv_row(edge_defaults_subframe_panel, row_count),
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"   edgetype",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICE_IMAGES,	white_icon_svi, 0, /* Dummy */
		PANEL_NOTIFY_PROC,	notify_edge_defaults_subframe_cycles,
		NULL);
	
	edge_defaults_subframe_font_cycle = xv_create(edge_defaults_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(edge_defaults_subframe_panel, 22),
		XV_Y,			xv_row(edge_defaults_subframe_panel, row_count),
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"label font",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_LABEL_Y,		(int)xv_get(edge_defaults_subframe_type_cycle, PANEL_LABEL_Y),
		PANEL_VALUE_Y,		xv_row(edge_defaults_subframe_panel, row_count) + DEFAULT_ICON_HEIGHT,
		PANEL_NOTIFY_PROC,	notify_edge_defaults_subframe_cycles,
		NULL);
	
/* Commented out MH conversion
	row_count ++;
	edge_defaults_subframe_edgecolor_selection = create_graphed_color_selection_item(
		edge_defaults_subframe_panel,
		xv_col(edge_defaults_subframe_panel, 11),
		xv_row(edge_defaults_subframe_panel, row_count) + DEFAULT_ICON_HEIGHT,
		notify_edge_defaults_subframe_cycles);
*/

	window_fit(edge_defaults_subframe_panel);	
	window_fit(edge_defaults_subframe);

	showing_edge_defaults_subframe = FALSE;
}


void	show_edge_defaults_subframe (caller_info, caller_completion_proc)
Edge_defaults_subframe_info	*caller_info;
void				(*caller_completion_proc)();
{
	int		x, y;
	int		edge_defaults_subframe_height, edge_defaults_subframe_width;
	Graphed_font	font;


	if (showing_edge_defaults_subframe) hide_edge_defaults_subframe (edge_defaults_subframe);
	
	info            = caller_info;
	completion_proc = caller_completion_proc;

	translate_wac_to_base_frame_space (&x,&y);
	edge_defaults_subframe_height = (int)xv_get(edge_defaults_subframe, XV_HEIGHT);
	edge_defaults_subframe_width  = (int)xv_get(edge_defaults_subframe, XV_WIDTH);
	
	xv_set(edge_defaults_subframe_label_visibility_cycle,	PANEL_VALUE, bool_to_int(info->attr.label_visibility), NULL);
	xv_set(edge_defaults_subframe_arrowlength_selection,	PANEL_VALUE, int_to_ascii(info->attr.arrow_length), NULL);
	xv_set(edge_defaults_subframe_arrowangle_selection,	PANEL_VALUE, float_to_ascii((float)rad_to_deg(info->attr.arrow_angle)), NULL);
	xv_set(edge_defaults_subframe_type_cycle,		PANEL_VALUE, info->attr.type_index, NULL);
	xv_set(edge_defaults_subframe_font_cycle,		PANEL_VALUE, info->attr.font_index, NULL);
/*	xv_set(edge_defaults_subframe_edgecolor_selection,	PANEL_VALUE, info->attr.color, NULL); */

	x = screenwidth/2  - edge_defaults_subframe_width/2;
	y = screenheight/2 - edge_defaults_subframe_height/2;

	xv_set(edge_defaults_subframe,
		WIN_X,		maximum (minimum (x, screenwidth  - edge_defaults_subframe_width),  0),
		WIN_Y,		maximum (minimum (y, screenheight - edge_defaults_subframe_height), 0),
		XV_SHOW,	TRUE,
		NULL);
	
	info->showing                  = TRUE;
	showing_edge_defaults_subframe = TRUE;
}


static	void	hide_edge_defaults_subframe (frame)
Frame		frame;
{
	showing_edge_defaults_subframe = FALSE;
	info->showing                  = FALSE;
			
	info->attr.label_visibility = int_to_bool ((int)xv_get(edge_defaults_subframe_label_visibility_cycle, PANEL_VALUE));
	info->attr.arrow_length     = atoi ((char *)xv_get(edge_defaults_subframe_arrowlength_selection), PANEL_VALUE);
	info->attr.arrow_angle      = deg_to_rad (atoi ((char *)xv_get(edge_defaults_subframe_arrowangle_selection)), PANEL_VALUE);
	info->attr.type_index       = (int)xv_get(edge_defaults_subframe_type_cycle, PANEL_VALUE);
	info->attr.font_index       = (int)xv_get(edge_defaults_subframe_font_cycle, PANEL_VALUE);
/*	info->attr.color            = (int)xv_get(edge_defaults_subframe_edgecolor_selection, PANEL_VALUE); */

	xv_set(edge_defaults_subframe, XV_SHOW, FALSE, NULL);
	
	(*completion_proc)(info);
}



void	install_fontlist_in_edge_defaults_subframe (list)
char	*list;
{
	xv_set(edge_defaults_subframe_font_cycle, ATTR_LIST, list, NULL);
	xv_set(edge_defaults_subframe_font_cycle, PANEL_VALUE, current_edgefont_index, NULL);
}


void	install_edgetypelist_in_edge_defaults_subframe (list)
char	*list;
{
	xv_set(edge_defaults_subframe_type_cycle, ATTR_LIST, list, NULL);
	xv_set(edge_defaults_subframe_type_cycle, PANEL_VALUE, current_edgetype_index, NULL);
}
/************************************************************************/
/*									*/
/*			NOTIFYPROZEDUREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	static notify_edge_defaults_subframe_buttons(item, event)	*/
/*	static notify_edge_defaults_subframe_cycles (item, value,event)	*/
/*									*/
/*	static Panel_setting notify_arrowlength_selection (item, event)	*/
/*	static Panel_setting notify_arrowangle_selection  (item, event)	*/
/*									*/
/************************************************************************/



static		notify_edge_defaults_subframe_buttons (item, event)
Panel_item	item;
Event		*event;
{
	if (item == edge_defaults_subframe) {
		hide_edge_defaults_subframe ();
	}
}


static		notify_edge_defaults_subframe_cycles (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
}


static	Panel_setting	notify_arrowlength_selection (item, event)
Panel_item		item;
Event			*event;
{
	int	length;
	
	switch (event_id (event)) {
		case CTRL_A :	xv_set(edge_defaults_subframe_arrowlength_selection, PANEL_VALUE, "0", NULL);	break;
		case CTRL_B :	xv_set(edge_defaults_subframe_arrowlength_selection, PANEL_VALUE, "8", NULL);	break;
		case CTRL_C :	xv_set(edge_defaults_subframe_arrowlength_selection, PANEL_VALUE, "12", NULL);	break;
		case CTRL_D :	xv_set(edge_defaults_subframe_arrowlength_selection, PANEL_VALUE, "16", NULL);	break;
		case CR :	length = atoi ((char *)xv_get(edge_defaults_subframe_arrowlength_selection, PANEL_VALUE));
				break;
	}
	
	return PANEL_NONE;
}


static	Panel_setting	notify_arrowangle_selection (item, event)
Panel_item		item;
Event			*event;
{
	float	angle;
	
	switch (event_id (event)) {
		case CTRL_A :	xv_set(edge_defaults_subframe_arrowangle_selection, PANEL_VALUE, "30", NULL);	break;
		case CTRL_B :	xv_set(edge_defaults_subframe_arrowangle_selection, PANEL_VALUE, "45", NULL);	break;
		case CTRL_C :	xv_set(edge_defaults_subframe_arrowangle_selection, PANEL_VALUE, "60", NULL);	break;
		case CR :	break;
	}
	
	return PANEL_NONE;
}
