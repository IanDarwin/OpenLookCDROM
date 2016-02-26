/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifndef lint
static char	sccsid[] = "@(#)gcc.c	1.22 90/05/08 Copyright 1989 Sun Microsystems";
#endif

#include	<stdio.h>
#include	<sys/param.h>
#include	<sys/types.h>
#include	<xview/xview.h>
#include	<xview/panel.h>
#include	<xview/textsw.h>
#include	<xview/svrimage.h>
#include	"gcm.h"
#include	"gcc_ui.h"

void			(*Callback)();
static caddr_t		Client_data;
static char		Current_color[MAXPATHLEN];
static gcc_win_objects	*Gcc_win;
static void		set_color_field();
static void		activate_values();
static void		fill_color_bits();

static char	*Colors[] = {
	"BG1", "BG2", "BG3", "Highlight", GUIDE_COLOR_LIST, NULL
};

/*
 * Initialize the gcc popup window with a title.
 */
void
gcc_initialize(owner, title)
	Xv_opaque	owner;
	char		*title;
{
int		row = 0;
int		cms_installed;
char		**color;
Xv_opaque	color_glyph;
unsigned short	color_glyph_bits[128];

	if(!Gcc_win)
		Gcc_win = gcc_win_objects_initialize(NULL, owner);

	xv_set(Gcc_win->win, XV_LABEL, title, 0);

	gcm_initialize_colors(Gcc_win->controls, NULL, NULL);
	cms_installed = !strcmp((char *)xv_get(Gcc_win->controls, WIN_CMS_NAME),
				gcm_colormap_name());

	for(color = Colors; *color; row++, color++) {

		fill_color_bits(color_glyph_bits, *color);

		if(cms_installed) {
			color_glyph = xv_create(0, SERVER_IMAGE,
				XV_WIDTH,		16,
				XV_HEIGHT,		16,
				SERVER_IMAGE_COLORMAP,	gcm_colormap_name(),
				SERVER_IMAGE_DEPTH,	8,
				SERVER_IMAGE_BITS,	color_glyph_bits,
				0);

			xv_set(Gcc_win->color_list,
				PANEL_LIST_INSERT, row,
				PANEL_LIST_GLYPH, row, color_glyph,
				PANEL_LIST_STRING, row, *color,
				0);
		} else {
			xv_set(Gcc_win->color_list,
				PANEL_LIST_INSERT, row,
				PANEL_LIST_STRING, row, *color,
				0);
		}
	}
	set_color_field(Colors[0]);
}

/*
 * Activate the color popup.  Set footer messages, current color, and
 * register a callback function to be called when the apply button is
 * pressed.  Store some client data for the caller.
 */
void
gcc_activate(left, right, func, client_data, color)
	char	*left;
	char	*right;
	void	(*func)();
	caddr_t	client_data;
	char	*color;
{
	xv_set(Gcc_win->win, FRAME_LEFT_FOOTER, left, 0);
	xv_set(Gcc_win->win, FRAME_RIGHT_FOOTER, right, 0);
	xv_set(Gcc_win->win, WIN_SHOW, TRUE, WIN_FRONT, 0);

	activate_values(TRUE);

	strcpy(Current_color, color);
	set_color_field(Current_color);

	Callback = func;
	Client_data = client_data;
}

/*
 * Deactivate the color popup.
 */
gcc_deactivate()
{
	Callback = NULL;
	Client_data = NULL;
	xv_set(Gcc_win->win, FRAME_LEFT_FOOTER, "", 0);
	xv_set(Gcc_win->win, FRAME_RIGHT_FOOTER, "", 0);

	set_color_field("");
	activate_values(FALSE);
}

/*
 * Suspend or re-enable color chooser if active or was active.
 */
void
gcc_suspend(suspend)
	int	suspend;
{
static int	was_active = FALSE;

	if(!Gcc_win)
		return;

	if((suspend && (int)xv_get(Gcc_win->win, WIN_SHOW)) ||
	   (!suspend && was_active)) {
		xv_set(Gcc_win->win,
			FRAME_CMD_PUSHPIN_IN,	!suspend,
			WIN_SHOW,		!suspend,
			0);
		was_active = suspend;
	}
}

/*
 * Notify callback function for `color_list'.
 */
/* ARGSUSED */
int
gcc_list_proc(item, string, client_data, op, event)
	Panel_item	item;
	char		*string;
	Xv_opaque	client_data;
	Panel_list_op	op;
	Event		*event;
{
	switch(op) {
	case PANEL_LIST_OP_DESELECT:
		break;

	case PANEL_LIST_OP_SELECT:
		set_color_field(string);
		break;

	case PANEL_LIST_OP_VALIDATE:
		break;

	case PANEL_LIST_OP_DELETE:
		break;
	}
	return XV_OK;
}

/*
 * Notify callback function for `apply'.
 */
/*ARGSUSED*/
void
gcc_apply(item, event)
	Panel_item	item;
	Event		*event;
{
	strcpy(Current_color,
		(char *)xv_get(Gcc_win->color_name, PANEL_VALUE));
	set_color_field(Current_color);

	if(Callback)
		(Callback)(Current_color, Client_data);

	xv_set(Gcc_win->apply, PANEL_NOTIFY_STATUS, XV_ERROR, 0);
}

/*
 * Notify callback function for `reset'.
 */
/*ARGSUSED*/
void
gcc_reset(item, event)
	Panel_item	item;
	Event		*event;
{
	set_color_field(Current_color);
	xv_set(Gcc_win->reset, PANEL_NOTIFY_STATUS, XV_ERROR, 0);
}

/*
 * Set a color name into the text field and change the color on the blot.
 */
static void
set_color_field(color)
	char	*color;
{
int	index;

	if((index = gcm_color_index(color)) == -1)
		index = gcm_color_index("black");

	xv_set(Gcc_win->color_name, PANEL_VALUE, color, 0);
	xv_set(Gcc_win->color_blot, PANEL_ITEM_COLOR, index, 0);
}

/*
 * Make panel items active or inactive.
 */
static void
activate_values(state)
	int	state;
{
	xv_set(Gcc_win->apply, PANEL_INACTIVE, !state, 0);
	xv_set(Gcc_win->reset, PANEL_INACTIVE, !state, 0);
}

/*
 * Fill an array of shorts with a particular color index for use in
 * an 8 bit deep SERVER_IMAGE.
 */
static void
fill_color_bits(bits, color_name)
	unsigned short	bits[];
	char		*color_name;
{
int		i;
int		color_index;
unsigned short	c;

	if((color_index = gcm_color_index(color_name)) == -1)
		color_index = 0;

	c = (color_index << 8) | color_index;

	for(i = 0; i < 128; i++)
		bits[i] = c;
}
