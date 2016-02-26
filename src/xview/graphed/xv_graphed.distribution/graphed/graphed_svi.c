/* Copyright Universitaet Passau 1990 */
/* GraphEd Source, 1986-1990 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*			graphed_svi.c					*/
/*									*/
/************************************************************************/
/*									*/
/*	Enthaelt alle Include-Dateien fuer Icon's und Cursor's.		*/
/*									*/
/************************************************************************/

#include <xview/svrimage.h>
#include "graphed_svi.h"
#include "config.h"


/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Server_image graphed_icon_svi					*/
/*									*/
/*	Server_image working_area_cursor_svi;				*/
/*	Server_image type_setup_cursor_svi;				*/
/*	Server_image font_setup_cursor_svi;				*/
/*									*/
/*	Server_image nei_none_icon_svi;					*/
/*	Server_image nei_middle_icon_svi;				*/
/*	Server_image nei_corner_icon_svi;				*/
/*	Server_image nei_clipped_icon_svi;				*/
/*	Server_image nei_special_icon_svi;				*/
/*									*/
/*	Server_image nlp_upperleft_icon_svi;				*/
/*	Server_image nlp_upperright_icon_svi;				*/
/*	Server_image nlp_lowerleft_icon_svi;				*/
/*	Server_image nlp_lowerright_icon_svi;				*/
/*	Server_image nlp_middle_icon_svi;				*/
/*									*/
/*	Server_image white_icon_svi;					*/
/*	Server_image insert_icon_svi;					*/
/*	Server_image delete_icon_svi;					*/
/*	Server_image add_icon_svi;					*/
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
Server_image	graphed_icon_svi;


/************************************************************************/
/*									*/
/*				Cursor					*/
/*									*/
/************************************************************************/


static short working_area_cursor_data[] =
{
#include "images/fadenkreuz.cursor"
};
Server_image	working_area_cursor_svi;


static short type_setup_cursor_data[] =
{
#include "images/zeigefinger.cursor"
};
Server_image	type_setup_cursor_svi;


static short font_setup_cursor_data[] =
{
#include "images/zeigefinger.cursor"
};
Server_image	font_setup_cursor_svi;


/************************************************************************/
/*									*/
/*			Node_edge_interface				*/
/*									*/
/************************************************************************/


static short nei_none_icon_data[] =
{
#include "images/nei_none.icon"
};
Server_image	nei_none_icon_svi;


static short nei_middle_icon_data[] =
{
#include "images/nei_middle.icon"
};
Server_image	nei_middle_icon_svi;


static short nei_corner_icon_data[] =
{
#include "images/nei_corner.icon"
};
Server_image	nei_corner_icon_svi;


static short nei_clipped_icon_data[] =
{
#include "images/nei_clipped.icon"
};
Server_image	nei_clipped_icon_svi;


static short nei_special_icon_data[] =
{
#include "images/nei_special.icon"
};
Server_image	nei_special_icon_svi;


/************************************************************************/
/*									*/
/*			Nodelabel_placement				*/
/*									*/
/************************************************************************/


static short nlp_upperleft_icon_data[] =
{
#include "images/nlp_upperleft.icon"
};
Server_image	nlp_upperleft_icon_svi;


static short nlp_upperright_icon_data[] =
{
#include "images/nlp_upperright.icon"
};
Server_image	nlp_upperright_icon_svi;


static short nlp_lowerleft_icon_data[] =
{
#include "images/nlp_lowerleft.icon"
};
Server_image	nlp_lowerleft_icon_svi;


static short nlp_lowerright_icon_data[] =
{
#include "images/nlp_lowerright.icon"
};
Server_image	nlp_lowerright_icon_svi;


static short nlp_middle_icon_data[] =
{
#include "images/nlp_middle.icon"
};
Server_image	nlp_middle_icon_svi;


/************************************************************************/
/*									*/
/*			Sonstige					*/
/*									*/
/************************************************************************/


static short white_icon_data[] =
{
#include "images/white.icon"
};
Server_image	white_icon_svi;


static short insert_icon_data[] =
{
#include "images/insert.icon"
};
Server_image	insert_icon_svi;


static short delete_icon_data[] =
{
#include "images/delete.icon"
};
Server_image	delete_icon_svi;


static short add_icon_data[] =
{
#include "images/add.icon"
};
Server_image	add_icon_svi;


/************************************************************************/
/*									*/
/*			   Menubar icons				*/
/*									*/
/************************************************************************/

static short about_icon_data[] =
{
#include "images/about.icon"
};
Server_image	about_icon_svi;


static short create_mode_icon_data[] =
{
#include "images/create_mode.icon"
};
Server_image	create_mode_icon_svi;


static short edit_icon_data[] =
{
#include "images/edit.icon"
};
Server_image	edit_icon_svi;


static short gragra_icon_data[] =
{
#include "images/gragra.icon"
};
Server_image	gragra_icon_svi;


static short file_icon_data[] =
{
#include "images/file.icon"
};
Server_image	file_icon_svi;


static short misc_icon_data[] =
{
#include "images/misc.icon"
};
Server_image	misc_icon_svi;


static short tools_icon_data[] =
{
#include "images/tools.icon"
};
Server_image	tools_icon_svi;


static short goodies_icon_data[] =
{
#include "images/goodies.icon"
};
Server_image	goodies_icon_svi;


static short layout_icon_data[] =
{
#include "images/layout.icon"
};
Server_image	layout_icon_svi;


static short user_icon_data[] =
{
#include "images/user.icon"
};
Server_image	user_icon_svi;



void svi_init ()
{
	graphed_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	graphed_icon_data,
		NULL);
	working_area_cursor_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	working_area_cursor_data,
		NULL);
	type_setup_cursor_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	type_setup_cursor_data,
		NULL);
	font_setup_cursor_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	font_setup_cursor_data,
		NULL);
	nei_none_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	nei_none_icon_data,
		NULL);
	nei_middle_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	nei_middle_icon_data,
		NULL);
	nei_corner_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	nei_corner_icon_data,
		NULL);
	nei_clipped_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	nei_clipped_icon_data,
		NULL);
	nei_special_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	nei_special_icon_data,
		NULL);
	nlp_upperleft_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	nlp_upperleft_icon_data,
		NULL);
	nlp_upperright_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	nlp_upperright_icon_data,
		NULL);
	nlp_lowerleft_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	nlp_lowerleft_icon_data,
		NULL);
	nlp_lowerright_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	nlp_lowerright_icon_data,
		NULL);
	nlp_middle_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	nlp_middle_icon_data,
		NULL);
	white_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,		/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	white_icon_data,
		NULL);
	insert_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	insert_icon_data,
		NULL);
	delete_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	delete_icon_data,
		NULL);
	add_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,		/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	add_icon_data,
		NULL);
	about_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,		/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	about_icon_data,
		NULL);
	create_mode_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	create_mode_icon_data,
		NULL);
	edit_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,		/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	edit_icon_data,
		NULL);
	gragra_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	gragra_icon_data,
		NULL);
	file_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,		/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	file_icon_data,
		NULL);
	misc_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,		/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	misc_icon_data,
		NULL);
	tools_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,		/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	tools_icon_data,
		NULL);
	goodies_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	goodies_icon_data,
		NULL);
	layout_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,	/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	layout_icon_data,
		NULL);
	user_icon_svi = (Server_image)xv_create(XV_NULL, SERVER_IMAGE,		/*pixr*/
		XV_HEIGHT,		DEFAULT_ICON_HEIGHT,
		XV_WIDTH,		DEFAULT_ICON_WIDTH,
		SERVER_IMAGE_BITS,	user_icon_data,
		NULL);
}


#include	<pixrect/pixrect_hs.h>

Server_image	pr_to_svi (pr)
Pixrect		*pr;
{
	/* Convert a Pixrect * to Server_image		*/
	/* Used for Conversion SunView -> XView		*/

	return (Server_image)xv_create(XV_NULL, SERVER_IMAGE,
		XV_HEIGHT,		pr->pr_height,
		XV_WIDTH,		pr->pr_width,
		XV_DEPTH,		pr->pr_depth,
		SERVER_IMAGE_BITS,	((struct mpr_data *)(pr->pr_data))->md_image,
		NULL);

}
