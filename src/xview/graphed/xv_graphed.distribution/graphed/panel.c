/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				panel.c					*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul verwaltet das node_edge_panel, das zum Auswaehlen	*/
/*	von Knoten- und Kantenzeichensaetzen sowie -typen		*/
/*	verwendet wird.							*/
/*									*/
/************************************************************************/

#include "user_header.h"

#include "font.h"
#include "install.h"
#include "menu.h"

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_node_edge_panel ()				*/
/*	void	create_menubar_panel   ()				*/
/*									*/
/*	void	install_nodetypelist_in_nodetype_selection (list)	*/
/*	void	install_edgetypelist_in_edgetype_selection (list)	*/
/*	void	install_current_nodetype_in_nodetype_selection ()	*/
/*	void	install_current_edgetype_in_edgetype_selection ()	*/
/*									*/
/*	void	install_fontlist_in_nodefont_selection (list)		*/
/*	void	install_fontlist_in_edgefont_selection (list)		*/
/*	void	install_current_nodefont_in_nodefont_selection ()	*/
/*	void	install_current_edgefont_in_edgefont_selection ()	*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/

Panel		node_edge_panel;	/* Knoten- und Kantentypen	*/
Panel		menubar_panel;


/************************************************************************/
/*									*/
/*			LOKALE FUNKTIONEN				*/
/*									*/
/************************************************************************/


static	int	notify_nodetype_selection(),
		notify_edgetype_selection(),
		notify_nodefont_selection(),
		notify_edgefont_selection(),
		notify_nodecolor_selection(),
		notify_edgecolor_selection();

static	void	panel_show_about_graphed ();


/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	Panel_item	nodetype_selection,
			edgetype_selection,
			nodefont_selection,
			edgefont_selection,
			nodecolor_selection,
			edgecolor_selection,
			graphed_ident,
			graphed_icon_ident;


/************************************************************************/
/*									*/
/*				NODE_EDGE_PANEL				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_node_edge_panel ()				*/
/*	void	create_menubar_panel ()					*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	notify_nodetype_selection (item, value, event)		*/
/*	static	notify_edgetype_selection (item, value, event)		*/
/*	static	notify_nodefont_selection (item, value, event)		*/
/*	static	notify_edgefont_selection (item, value, event)		*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	install_fontlist_in_nodefont_selection (list)		*/
/*	void	install_fontlist_in_edgefont_selection (list)		*/
/*	void	install_current_nodefont_in_nodefont_selection ()	*/
/*	void	install_current_edgefont_in_edgefont_selection ()	*/
/*									*/
/*	void	install_nodetypelist_in_nodetype_selection (list)	*/
/*	void	install_edgetypelist_in_edgetype_selection (list)	*/
/*	void	install_current_nodetype_in_nodetype_selection ()	*/
/*	void	install_current_edgetype_in_edgetype_selection ()	*/
/*									*/
/************************************************************************/



/* Unused due to Conversion MH

static	void	panel_show_menu (item, event)
Panel_item	item;
Event		*event;
{

	if (event_action (event) == MS_RIGHT && event_is_down (event) &&
            !test_user_interface_locked() &&
	    !draging_node  && !draging_edge &&
	    !draging_group && !draging_group_box &&
	    !making_node && !making_edge) {

		menu_called_from = MENU_CALLED_FROM_MENUBAR;
		inactivate_menu_item (GET_SELECTION);
		inactivate_menu_item (GET_AS_GRAPH);
		inactivate_menu_item (LOAD_AGAIN);
		inactivate_menu_item (STORE_BY_SUBFRAME);
		inactivate_menu_item (STORE_TO_SAME_FILE);

		menu_show (
			xv_get(item, PANEL_CLIENT_DATA),
			xv_get(item, XV_OWNER),
			event,
			0);

		activate_menu_item (GET_SELECTION);
		activate_menu_item (GET_AS_GRAPH);
		activate_menu_item (LOAD_AGAIN);
		activate_menu_item (STORE_BY_SUBFRAME);
		activate_menu_item (STORE_TO_SAME_FILE);
		
		menu_called_from = MENU_CALLED_FROM_NOWHERE;

	} else {
		panel_default_handle_event (item, event);
	}

	force_repainting ();
}


static	void	panel_do_default_menu_item (item, event)
Panel_item	item;
Event		*event;
{
	Menu	menu = xv_get(item, PANEL_CLIENT_DATA);
	
	if (!test_user_interface_locked()) {
		dispatch_user_action (
			(User_action) menu_get (
				(Menu_item) menu_get (menu, MENU_DEFAULT_ITEM),
				MENU_CLIENT_DATA));
	}
}

*/


void	create_node_edge_panel ()
{	
	char	*graphed_ident_buffer1 [100];
	char	*graphed_ident_buffer2 [100];
	
	xv_set(node_edge_panel,
		WIN_ROW_HEIGHT,		64,
		WIN_COLUMN_WIDTH,	80,
		PANEL_LAYOUT,		PANEL_HORIZONTAL,
		NULL);

	nodetype_selection = xv_create(node_edge_panel, PANEL_CHOICE_STACK,
		XV_X,			xv_col (node_edge_panel, 0),
		XV_Y,			xv_row (node_edge_panel, 0),
		PANEL_CHOICE_IMAGES,	white_icon_svi, NULL,
		PANEL_FEEDBACK,		PANEL_NONE,
		PANEL_NOTIFY_PROC,	notify_nodetype_selection,
		NULL);
		
	edgetype_selection = xv_create(node_edge_panel, PANEL_CHOICE_STACK,
		XV_X,			xv_col (node_edge_panel, 0),
		XV_Y,			xv_row (node_edge_panel, 1),
		PANEL_CHOICE_IMAGES,	white_icon_svi, NULL,
/*		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,*/
		PANEL_FEEDBACK,		PANEL_NONE,
		PANEL_NOTIFY_PROC,	notify_edgetype_selection,
		NULL);

/* Commented MH Conversion	
	
	nodecolor_selection = create_graphed_color_selection_item (node_edge_panel,
		xv_col (node_edge_panel, 1) + 32,
 		xv_row (node_edge_panel, 0),
		notify_nodecolor_selection);
			
	edgecolor_selection = create_graphed_color_selection_item (node_edge_panel,
		xv_col (node_edge_panel,1) + 32,
		xv_row (node_edge_panel, 1),
		notify_edgecolor_selection);
	
*/
	
	nodefont_selection = xv_create(node_edge_panel, PANEL_CHOICE_STACK,
		XV_X,			xv_col (node_edge_panel, 3),
		XV_Y,			xv_row (node_edge_panel, 0),
		PANEL_CHOICE_STRINGS,	"0123456789", NULL,
		PANEL_NOTIFY_PROC,	notify_nodefont_selection,
		NULL);
	
	edgefont_selection = xv_create(node_edge_panel, PANEL_CHOICE_STACK,
		XV_X,			xv_col (node_edge_panel, 3),
		XV_Y,			xv_row (node_edge_panel, 1),
		PANEL_CHOICE_STRINGS,	"0123456789", NULL,
		PANEL_NOTIFY_PROC,	notify_edgefont_selection,
		NULL);

	sprintf (graphed_ident_buffer1, "GraphEd %s  (C) University of Passau 1986 - 1991", GRAPHED_VERSION);
	sprintf (graphed_ident_buffer2, "              Lehrstuhl für Theoretische Informatik");
	
	graphed_icon_ident = xv_create(node_edge_panel, PANEL_MESSAGE,
		XV_X,			xv_col (node_edge_panel, 0),
		XV_Y,			xv_row (node_edge_panel, 2),
		PANEL_LABEL_BOLD	,TRUE,
		PANEL_LABEL_STRING,	graphed_ident_buffer1,
		NULL);
	graphed_icon_ident = xv_create(node_edge_panel, PANEL_MESSAGE,
		XV_X,			xv_col (node_edge_panel, 0), 
		XV_Y,			xv_row (node_edge_panel, 2) + 20,
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_LABEL_STRING,	graphed_ident_buffer2,
		NULL);
}


/************************************************************************/
/*									*/
/*			    Notify - Procedures				*/
/*									*/
/************************************************************************/



static		notify_nodetype_selection (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	dispatch_user_action (SET_NODETYPE, value);
}


static		notify_edgetype_selection (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	dispatch_user_action (SET_EDGETYPE, value);
}



static		notify_nodefont_selection (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	dispatch_user_action (SET_NODEFONT, value);
}


static		notify_edgefont_selection (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	dispatch_user_action (SET_EDGEFONT, value);
}


static		notify_nodecolor_selection (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	dispatch_user_action (SET_NODECOLOR, value);
}


static		notify_edgecolor_selection (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	dispatch_user_action (SET_EDGECOLOR, value);
}


/************************************************************************/
/*									*/
/*			    Install-procedures				*/
/*									*/
/************************************************************************/


void	install_nodetypelist_in_nodetype_selection (list)
char	*list;
{
	xv_set(nodetype_selection, ATTR_LIST, list, NULL);
}


void	install_edgetypelist_in_edgetype_selection (list)
char	*list;
{
	xv_set(edgetype_selection, ATTR_LIST, list, NULL);
}



void	install_current_nodetype_in_nodetype_selection ()
{
	xv_set(nodetype_selection, PANEL_VALUE, current_nodetype_index, NULL);
}


void	install_current_edgetype_in_edgetype_selection ()
{
	xv_set(edgetype_selection, PANEL_VALUE, current_edgetype_index, NULL);
}


void	install_fontlist_in_nodefont_selection (list)
char	*list;
{
	xv_set(nodefont_selection, ATTR_LIST, list, NULL);
}


void	install_fontlist_in_edgefont_selection (list)
char	*list;
{
	xv_set(edgefont_selection, ATTR_LIST, list, NULL);
}



void	install_current_nodefont_in_nodefont_selection ()
{
	xv_set(nodefont_selection, PANEL_VALUE, current_nodefont_index, NULL);
}


void	install_current_edgefont_in_edgefont_selection ()
{
	xv_set(edgefont_selection, PANEL_VALUE, current_edgefont_index, NULL);
}

void	install_current_nodecolor_in_nodecolor_selection ()
{
/* Commented MH Conversion
	xv_set(nodecolor_selection, PANEL_VALUE, current_nodecolor, NULL);
*/
}


void	install_current_edgecolor_in_edgecolor_selection ()
{
/* Commented MH Conversion
	xv_set(edgecolor_selection, PANEL_VALUE, current_edgecolor, NULL);
*/
}

/************************************************************************/
/*									*/
/*		Hilfsprozedur zum Erzeugen einer Farbauswahl		*/
/*									*/
/************************************************************************/
/*									*/
/*  static Pixrect *create_colored_pr (width, heigth, depth, color)	*/
/*									*/
/*======================================================================*/
/*									*/
/*  Panel_item	   create_graphed_color_selection_item (panel,		*/
/*                 x,y, notify_proc)					*/
/*									*/
/************************************************************************/



static	Pixrect	*create_colored_pr (width, height, depth, color)
int		width, height, depth;
int		color;
{
	Pixrect	*pr;
	
	pr = mem_create (width, height, depth);
	
	pr_rop (pr, 0,0, width,height, PIX_SRC | PIX_COLOR(color), (Pixrect *)NULL, 0, 0);
	
	return pr;
}


/* include a little image	*/
static	short	triangle_up_array[] = {
#include "images/triangle_up.pr"
};
mpr_static (triangle_up, 16,16,1, triangle_up_array);


Panel_item	create_graphed_color_selection_item (panel, x,y, notify_proc)
Panel		panel;
int		x,y;
int		(*notify_proc)();
{	
	Panel_item	color_selection;
	Pixwin		*pw;
	
	
	pw= (Pixwin *)xv_get(panel, WIN_PIXWIN);
	pw_setcmsname  (pw, GRAPHED_COLORMAP_NAME);
	pw_putcolormap (pw, 0, GRAPHED_COLORMAPSIZE, red, green, blue);


#define	SQUARE_WIDTH  16
#define	SQUARE_HEIGHT 16
/* Changed MH conversion
#define	SQUARE_DEPTH  (pw->pw_pixrect->pr_depth)
*/
/* UMARBEITUNG NOTWENDIG */

#define SQUARE_DEPTH 1

	color_selection = (Panel)xv_create(panel, PANEL_CHOICE,

/* Changed MH Conversion
#define IMAGE(i) create_colored_pr (SQUARE_WIDTH, SQUARE_HEIGHT, SQUARE_DEPTH, (i))
*/
#define IMAGE(i) xv_create(XV_NULL, SERVER_IMAGE, XV_WIDTH, SQUARE_WIDTH, XV_HEIGHT, SQUARE_HEIGHT, SERVER_IMAGE_BITS, create_colored_pr (SQUARE_WIDTH, SQUARE_HEIGHT, SQUARE_DEPTH, (i)), 0)

		PANEL_CHOICE_IMAGES,
			IMAGE(0),  IMAGE(1),  IMAGE(2),  IMAGE(3),
			IMAGE(4),  IMAGE(5),  IMAGE(6),  IMAGE(7),
			IMAGE(8),  IMAGE(9),  IMAGE(10), IMAGE(11),
			IMAGE(12), IMAGE(13), IMAGE(14), IMAGE(15),
			0,

#define	X(i) (x + SQUARE_WIDTH*(i))
#define	Y(i) (y + SQUARE_HEIGHT*(i))
		PANEL_CHOICE_XS,	X(0), X(1), X(2), X(3), X(4), X(5), X(6), X(7),
					X(0), X(1), X(2), X(3), X(4), X(5), X(6), X(7), 0,
		PANEL_CHOICE_YS,	Y(0), Y(0), Y(0), Y(0), Y(0), Y(0), Y(0), Y(0),
					Y(2), Y(2), Y(2), Y(2), Y(2), Y(2), Y(2), Y(2), 0,
		
		PANEL_MARK_IMAGES,	&triangle_up, 0,
		PANEL_NOMARK_IMAGES,	0,
		PANEL_MARK_XS,		X(0), X(1), X(2), X(3), X(4), X(5), X(6), X(7),
					X(0), X(1), X(2), X(3), X(4), X(5), X(6), X(7), 0,
		PANEL_MARK_YS,		Y(1), Y(1), Y(1), Y(1), Y(1), Y(1), Y(1), Y(1),
					Y(3), Y(3), Y(3), Y(3), Y(3), Y(3), Y(3), Y(3), 0,
					
		PANEL_NOTIFY_PROC,	notify_proc,
		NULL);
	
	return color_selection;
}
