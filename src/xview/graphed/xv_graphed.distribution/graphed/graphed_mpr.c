/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*			graphed_mpr.c					*/
/*									*/
/************************************************************************/
/*									*/
/*	Enthaelt alle Include-Dateien fuer Icon's und Cursor's.		*/
/*									*/
/************************************************************************/

#include <pixrect/pixrect_hs.h>
#include "graphed_mpr.h"
#include "config.h"


/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Pixrect	graphed_icon_pixrect					*/
/*									*/
/*	Pixrect	working_area_cursor_pixrect;				*/
/*	Pixrect	type_setup_cursor_pixrect;				*/
/*	Pixrect	font_setup_cursor_pixrect;				*/
/*									*/
/*	Pixrect	nei_none_icon_pixrect;					*/
/*	Pixrect	nei_middle_icon_pixrect;				*/
/*	Pixrect	nei_corner_icon_pixrect;				*/
/*	Pixrect	nei_clipped_icon_pixrect;				*/
/*	Pixrect	nei_special_icon_pixrect;				*/
/*									*/
/*	Pixrect	nlp_upperleft_icon_pixrect;				*/
/*	Pixrect	nlp_upperright_icon_pixrect;				*/
/*	Pixrect	nlp_lowerleft_icon_pixrect;				*/
/*	Pixrect	nlp_lowerright_icon_pixrect;				*/
/*	Pixrect	nlp_middle_icon_pixrect;				*/
/*									*/
/*	Pixrect	white_icon_pixrect;					*/
/*	Pixrect	insert_icon_pixrect;					*/
/*	Pixrect	delete_icon_pixrect;					*/
/*	Pixrect	add_icon_pixrect;					*/
/*									*/
/*	Diese Variablen werden implizit mit dem Makro mpr_static	*/
/*	deklariert. Diesem Makro ist es auch zu verdanken, dass die	*/
/*	Bilder fuer node_edge_interface und nodelabel_placement nicht	*/
/*	als Array eingelesen werden koennen.				*/
/*									*/
/************************************************************************/
/************************************************************************/
/*									*/
/*			GraphEd's icon					*/
/*									*/
/************************************************************************/


static short graphed_icon_data[] =
{
#include "images/graphed.icon"
};
mpr_static (graphed_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            graphed_icon_data);


/************************************************************************/
/*									*/
/*				Cursor					*/
/*									*/
/************************************************************************/


static short working_area_cursor_data[] =
{
#include "images/fadenkreuz.cursor"
};
mpr_static (working_area_cursor_pixrect,
            DEFAULT_CURSOR_WIDTH, DEFAULT_CURSOR_HEIGHT, DEFAULT_CURSOR_DEPTH,
            working_area_cursor_data);


static short type_setup_cursor_data[] =
{
#include "images/zeigefinger.cursor"
};
mpr_static (type_setup_cursor_pixrect,
            DEFAULT_CURSOR_WIDTH, DEFAULT_CURSOR_HEIGHT, DEFAULT_CURSOR_DEPTH,
            type_setup_cursor_data);


mpr_static (font_setup_cursor_pixrect,
            DEFAULT_CURSOR_WIDTH, DEFAULT_CURSOR_HEIGHT, DEFAULT_CURSOR_DEPTH,
            type_setup_cursor_data);


/************************************************************************/
/*									*/
/*			Node_edge_interface				*/
/*									*/
/************************************************************************/


static short nei_none_icon_data[] =
{
#include "images/nei_none.icon"
};
mpr_static (nei_none_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            nei_none_icon_data);

static short nei_middle_icon_data[] =
{
#include "images/nei_middle.icon"
};
mpr_static (nei_middle_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            nei_middle_icon_data);

static short nei_corner_icon_data[] =
{
#include "images/nei_corner.icon"
};
mpr_static (nei_corner_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            nei_corner_icon_data);

static short nei_clipped_icon_data[] =
{
#include "images/nei_clipped.icon"
};
mpr_static (nei_clipped_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            nei_clipped_icon_data);

static short nei_special_icon_data[] =
{
#include "images/nei_special.icon"
};
mpr_static (nei_special_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            nei_special_icon_data);


/************************************************************************/
/*									*/
/*			Nodelabel_placement				*/
/*									*/
/************************************************************************/


static short nlp_upperleft_icon_data[] =
{
#include "images/nlp_upperleft.icon"
};
mpr_static (nlp_upperleft_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            nlp_upperleft_icon_data);

static short nlp_upperright_icon_data[] =
{
#include "images/nlp_upperright.icon"
};
mpr_static (nlp_upperright_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            nlp_upperright_icon_data);

static short nlp_lowerleft_icon_data[] =
{
#include "images/nlp_lowerleft.icon"
};
mpr_static (nlp_lowerleft_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            nlp_lowerleft_icon_data);

static short nlp_lowerright_icon_data[] =
{
#include "images/nlp_lowerright.icon"
};
mpr_static (nlp_lowerright_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            nlp_lowerright_icon_data);

static short nlp_middle_icon_data[] =
{
#include "images/nlp_middle.icon"
};
mpr_static (nlp_middle_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            nlp_middle_icon_data);


/************************************************************************/
/*									*/
/*			Sonstige					*/
/*									*/
/************************************************************************/


static short white_icon_data[] =
{
#include "images/white.icon"
};
mpr_static (white_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            white_icon_data);


static short insert_icon_data[] =
{
#include "images/insert.icon"
};
mpr_static (insert_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            insert_icon_data);

static short delete_icon_data[] =
{
#include "images/delete.icon"
};
mpr_static (delete_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            delete_icon_data);

static short add_icon_data[] =
{
#include "images/add.icon"
};
mpr_static (add_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            add_icon_data);


/************************************************************************/
/*									*/
/*			   Menubar icons				*/
/*									*/
/************************************************************************/

static short about_icon_data[] =
{
#include "images/about.icon"
};
mpr_static (about_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            about_icon_data);


static short create_mode_icon_data[] =
{
#include "images/create_mode.icon"
};
mpr_static (create_mode_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            create_mode_icon_data);


static short edit_icon_data[] =
{
#include "images/edit.icon"
};
mpr_static (edit_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            edit_icon_data);


static short gragra_icon_data[] =
{
#include "images/gragra.icon"
};
mpr_static (gragra_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            gragra_icon_data);


static short file_icon_data[] =
{
#include "images/file.icon"
};
mpr_static (file_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            file_icon_data);


static short misc_icon_data[] =
{
#include "images/misc.icon"
};
mpr_static (misc_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            misc_icon_data);


static short tools_icon_data[] =
{
#include "images/tools.icon"
};
mpr_static (tools_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            tools_icon_data);


static short goodies_icon_data[] =
{
#include "images/goodies.icon"
};
mpr_static (goodies_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            goodies_icon_data);


static short layout_icon_data[] =
{
#include "images/layout.icon"
};
mpr_static (layout_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            layout_icon_data);


static short user_icon_data[] =
{
#include "images/user.icon"
};
mpr_static (user_icon_pixrect,
            DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH,
            user_icon_data);

