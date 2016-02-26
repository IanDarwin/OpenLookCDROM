/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				about.c					*/
/*									*/
/************************************************************************/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"

#include "graphed_subwindows.h"
#include "graphed_mpr.h"



/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_about_subframe ()					*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/


Frame		about_subframe;


/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/


static	void	create_about_subframe    ();
static		notify_about_buttons     ();
static		notify_about_cycle       ();
static		about_subframe_done      ();


/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	Panel		about_panel;
static	Textsw		about_textsw;

static	Panel_item	graphed_icon_button;
static	Panel_item	unipassau_icon_button;
static	Panel_item	graphed_about_cycle;
static	Panel_item	graphed_about_quit_button;

#define N_ABOUT_TOPICS 16

static	char		*about_topics[N_ABOUT_TOPICS] = {
			"GraphEd",
			"General",
			"Create",
			"Edit",
			"Graph Grammars",
			"GraGra",
			"InOut",
			"Misc",
			"Tools",
			"Layout",
			"Goody",
			"User",
			"Termgraph",
			"References",
			"Source",
			"People" };

static	char		*about_topics_for_cycle[N_ABOUT_TOPICS+3];

static	char		*about_topic_filenames[N_ABOUT_TOPICS] = {
			"help/help.GraphEd",
			"help/help.General",
			"help/help.Create",
			"help/help.Edit",
			"help/help.GraphGrammars",
			"help/help.GraGra",
			"help/help.InOut",
			"help/help.Misc",
			"help/help.Tools",
			"help/help.Layout",
			"help/help.Goody",
			"help/help.User",
			"help/help.Termgraph",
			"help/help.References",
			"help/help.Source",
			"help/help.People" };
			

static	Pixrect	*uni_passau_logo = (Pixrect*)NULL;
static	Pixrect	*uni_passau_big_logo = (Pixrect*)NULL;


static void	create_about_subframe()
{
	Menu	about_subframe_menu;
	int	row_count = 0;
	char	*filename;
	FILE	*f;

	about_subframe = (Frame)xv_create(base_frame, FRAME,
		FRAME_NO_CONFIRM,	TRUE,
		FRAME_ICON,		icon_create(ICON_IMAGE, about_icon_svi, 0),

/* Removed MH Conversion
		FRAME_SHOW_SHADOW, TRUE,
*/
		FRAME_DONE_PROC,	about_subframe_done,
		NULL);

	about_panel = (Panel)xv_create(about_subframe, PANEL,
		XV_HEIGHT,		WIN_EXTEND_TO_EDGE,
		NULL);

	if (uni_passau_logo     == (Pixrect*)NULL) {
		filename = file_exists_somewhere ("uni-passau.logo",
		                                  getenv ("GRAPHED_INPUTS"));
		if (filename != NULL) {
			f = fopen (filename, "r");
			uni_passau_logo = pr_load (f, NULL);
			fclose (f);
		}
	}

	if (uni_passau_big_logo == (Pixrect*)NULL) {
		filename = file_exists_somewhere ("uni-passau.big.logo",
		                                  getenv ("GRAPHED_INPUTS"));
		if (filename != NULL) {
			f = fopen (filename, "r");
			uni_passau_big_logo = pr_load (f, NULL);
			fclose (f);
		}
	}

	if (uni_passau_logo != (Pixrect*)NULL && uni_passau_big_logo != (Pixrect*)NULL) {
		unipassau_icon_button = xv_create(about_panel, PANEL_BUTTON,
			XV_X,			xv_col (about_panel, 0),
			XV_Y,			xv_row (about_panel, 0),
			PANEL_LABEL_IMAGE,	pr_to_svi(uni_passau_logo),
			/* Removed, defunct, MH Conversion
			PANEL_MENU_CHOICE_IMAGES, uni_passau_big_logo, 0,
			*/
			NULL);
	}

	graphed_icon_button = xv_create(about_panel, PANEL_BUTTON,
		XV_X,	iif(uni_passau_logo != (Pixrect*)NULL,
				xv_col(about_panel, 0) + (uni_passau_logo->pr_size.x - graphed_icon_pixrect.pr_size.x) / 2,
				xv_col (about_panel, 6)),
		XV_Y,	xv_row (about_panel, 9),
		PANEL_LABEL_IMAGE,	graphed_icon_svi,
		NULL);

	fill_panel_choice_attr_list_of_strings (
		about_topics, N_ABOUT_TOPICS, about_topics_for_cycle);

	graphed_about_cycle = xv_create(about_panel, PANEL_CHOICE_STACK,
		ATTR_LIST,		about_topics_for_cycle,
		XV_X,			xv_col (about_panel, 0),
		XV_Y,			xv_row (about_panel, 14),
		PANEL_LABEL_STRING,	"Topic :",
		PANEL_NOTIFY_PROC,	notify_about_cycle,
		NULL);

	graphed_about_quit_button = xv_create(about_panel, PANEL_BUTTON,
		XV_X,			xv_col (about_panel, 1),
		XV_Y,			xv_row (about_panel, 17),
		PANEL_LABEL_STRING,	"quit",
		PANEL_NOTIFY_PROC,	notify_about_buttons,
		NULL);

	window_fit(about_panel);

	
	about_textsw = (Textsw)xv_create(about_subframe, TEXTSW,
		WIN_Y,			0,
		WIN_RIGHT_OF,		about_panel,
		WIN_COLUMNS,		53,
		XV_HEIGHT,		WIN_EXTEND_TO_EDGE,
		TEXTSW_DISABLE_LOAD,	TRUE,
		TEXTSW_DISABLE_CD,	TRUE,
		TEXTSW_READ_ONLY,	TRUE,
		TEXTSW_BROWSING,	TRUE,
		NULL);
		
/*fis: steht bereits in xv_create		
	xv_set(about_textsw, 
		TEXTSW_DISABLE_LOAD,	TRUE,
		TEXTSW_DISABLE_CD,	TRUE,
		TEXTSW_READ_ONLY,	TRUE,
		TEXTSW_BROWSING,	TRUE,
		NULL);
*/

	window_fit(about_subframe);
}


static	showing_about_subframe = FALSE;


void		show_about_subframe(node_or_edge)
Node_or_edge	node_or_edge;
{
	char	*filename;
	
	if (showing_about_subframe) {
		return;
	}
	
	create_about_subframe();
	
	xv_set(about_subframe,
		WIN_X,		screenwidth  / 2 - (int)xv_get(about_subframe, XV_WIDTH) / 2,
		WIN_Y,		screenheight / 2 - (int)xv_get(about_subframe, XV_HEIGHT) / 2,
		XV_SHOW,	TRUE,
		NULL);
	
	filename = file_exists_somewhere (about_topic_filenames[0],
		getenv ("GRAPHED_INPUTS"));
	
	if (filename != NULL) {
		xv_set(about_textsw, TEXTSW_FILE, filename, NULL);
	} else {
		warning ("No Information about %s available\n", about_topics[0]);
	}
	
	showing_about_subframe = TRUE;
}




static		notify_about_buttons (item, event)
Panel_item	item;
Event		*event;
{
	if (item == graphed_about_quit_button) {
		xv_destroy_safe (about_subframe);
		showing_about_subframe = FALSE;
	}
}


static		about_subframe_done (frame)
Frame		frame;
{
	xv_destroy_safe (about_subframe);
	showing_about_subframe = FALSE;
}


static		notify_about_cycle (item, event)
Panel_item	item;
Event		*event;
{
	int	topic = (int)xv_get(item, PANEL_VALUE);
	char	*filename;
	
	filename = file_exists_somewhere (about_topic_filenames[topic],
		getenv ("GRAPHED_INPUTS"));
	
	if (filename != NULL) {
		xv_set(about_textsw, TEXTSW_FILE, filename, NULL);
	} else {
		warning ("No Information on %s available\n", about_topics[topic]);
	}
}
