/*
 * This file is provided for unrestricted use
 * provided that this legend is included on all tape media
 * and as a part of the software program in whole or part.  Users
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

#include <stdio.h>
#include <xview/xview.h>
#include <xview/cms.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/window.h>
#include <xview/font.h>
#include <xview/defaults.h>
#include <xview/notice.h>
#include <gfm_ui.h>

#include "xvnews_ui.h"
#include "xvnews.h"
#include "gcc_ui.h"
#include "gcc.h"
#include "gcm.h"

struct xvnews_props {
	char			background[30];
	char			highlight[30];
	char			textcolor[30];
	char			textback[30];
	char			indent[8];
	char			print[256];
	char			editor[256];
	char			filter[256];
	char			log[256];
	int			rescan;
	int			header;
	int			sort;
	int			listfont;
	int			listfontsize;
	int			textfont;
	int			textfontsize;
	int			displaysize;
	int			searchdefault;
	int			nextdefault;
	int			prevdefault;
        int                     undelete_popup;
};

static struct xvnews_props Props;
static struct xvnews_props OldProps;

static char	*Fontnames[] = 
	{"default", "Lucida", "Times", "Courier", ""};
static int	Fontsizes[] = 
	{8, 10, 12, 14, 16, 18, 24, 0};

static int	ColorChooser;
static int	Chooser = 0;

STATIC_FUNCTION( void apply_color, (Panel, char *, char *));
STATIC_FUNCTION( void apply_font, (Textsw, int, int));

void change_color_prop();

void
display_props(item, event)
	Panel_item      item;
	Event           *event;
{
	xvnews_xvnews_window_objects *ip = (xvnews_xvnews_window_objects *)xv_get(item, XV_KEY_DATA, INSTANCE, NULL);
	xvnews_props_objects *pp = (xvnews_props_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);
	Display *dpy = (Display *)xv_get(ip->xvnews_window, XV_DISPLAY);

	if (Chooser)
		gcc_suspend(FALSE);
	if (DefaultDepth(dpy, DefaultScreen(dpy)) < 2) {
		xv_set(pp->props_color_class, PANEL_INACTIVE, TRUE, NULL);
		xv_set(pp->color_menu_button, PANEL_INACTIVE, TRUE, NULL);
		xv_set(pp->props_color, PANEL_INACTIVE, TRUE, NULL);	
	}
	xv_set(pp->props, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
	xv_set(pp->props, XV_SHOW, TRUE, NULL);
}

	
static void apply_color(panel, bgcolor, txtcolor)
Panel		panel;
char		*bgcolor;
char		*txtcolor;
{
	Panel_item	item;
	int		i;
	int		nitems = 0;

	if (strlen(bgcolor)) {
		gcm_initialize_colors(panel, bgcolor, NULL);
	}
	if (strlen(txtcolor)) {
		PANEL_EACH_ITEM(panel, item)
			if ((Xv_pkg *)xv_get(item, XV_TYPE) == PANEL_CHOICE) {
				xv_set(item, PANEL_ITEM_COLOR, 
					gcm_color_index(txtcolor), NULL);
				nitems = (int)xv_get(item, 
						PANEL_CHOICE_NCOLS, NULL);
				for (i = 0; i < nitems; i++)
					xv_set(item, PANEL_CHOICE_COLOR, i,
						gcm_color_index(txtcolor), NULL);
			} else {
				xv_set(item, PANEL_ITEM_COLOR, 
					gcm_color_index(txtcolor), NULL);
			}
		PANEL_END_EACH;
	}

	panel_paint(panel, PANEL_CLEAR);
}

extern void apply_post_defaults(pp, gfm)
xvnews_post_popup_objects       *pp;
gfm_popup_objects               *gfm;
{
	if (pp != NULL)  {
		apply_color(pp->post_controls,Props.highlight, Props.textcolor);
		apply_font(pp->post_window, Props.listfont, 
			Fontsizes[Props.listfontsize]);
		apply_color(gfm->controls, Props.highlight, Props.textcolor);
		if (strlen(Props.textcolor) || strlen(Props.textback))
			gcm_initialize_colors(pp->post_window, Props.textback, Props.textcolor);
	}
}

extern void apply_props(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_props_objects	*ip = (xvnews_props_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_xvnews_window_objects *bw = (xvnews_xvnews_window_objects *) xv_get(xv_get(ip->props, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);
	xvnews_search_popup_objects       *srp = (xvnews_search_popup_objects *) xv_get(bw->xvnews_window, XV_KEY_DATA, SEARCH_POPUP, NULL);
	gfm_popup_objects       *gfm = (gfm_popup_objects *) xv_get(bw->xvnews_window, XV_KEY_DATA, SAVE_POPUP, NULL);
	kill_popup_objects       *kp = (kill_popup_objects *) xv_get(bw->xvnews_window, XV_KEY_DATA, KILL_POPUP, NULL);
	xvnews_undelete_popup_objects       *up = (xvnews_undelete_popup_objects *) xv_get(bw->xvnews_window, XV_KEY_DATA, UNDELETE_POPUP, NULL);
	
	/* apply the props in the props window... */


	apply_color(ip->props_controls, Props.highlight, Props.textcolor);
	apply_color(bw->controls2, Props.background, Props.textcolor);
	apply_color(bw->controls1, Props.highlight, Props.textcolor);
	apply_color(gfm->controls, Props.highlight, Props.textcolor);
	apply_color(kp->kill_controls, Props.highlight, Props.textcolor);
	apply_color(up->controls3, Props.background, Props.textcolor);

	if (strlen(Props.textcolor) || strlen(Props.textback))
		gcm_initialize_colors(bw->article_window, Props.textback, Props.textcolor);

	apply_color(srp->controls4, Props.highlight, Props.textcolor);

	defaults_set_string("XVnews.background", Props.background);
    	defaults_set_string("XVnews.highlight", Props.highlight);
    	defaults_set_string("XVnews.textcolor", Props.textcolor);
    	defaults_set_string("XVnews.textback", Props.textback);
    	defaults_set_integer("XVnews.listfont", Props.listfont);
    	defaults_set_integer("XVnews.listfontsize", Props.listfontsize);
    	defaults_set_integer("XVnews.textfont", Props.textfont);
    	defaults_set_integer("XVnews.textfontsize", Props.textfontsize);

	/* apply_font(bw->xvnews_window, Props.listfont, 
		Fontsizes[Props.listfontsize]);
	*/

	apply_font(bw->article_window, Props.listfont, 
		Fontsizes[Props.listfontsize]);


	if (event != NULL) {

		/* only do this if you actually selected "Apply Props!" */
		char	defaults[256];

		if (!notice_prompt(ip->props_controls, NULL,
			NOTICE_MESSAGE_STRINGS, "Confirm modifying ~/.Xdefaults file.",
			"This action may delete comments in file and will store",
			"all command line arguments given to xvnews.", NULL,
			NOTICE_BUTTON_YES, "save properties",
			NOTICE_BUTTON_NO, "cancel",
			NULL))
				return;
		Props.displaysize = xv_get(ip->display_text, PANEL_VALUE, NULL);
		strcpy(Props.print, (char *)xv_get(ip->print_text, PANEL_VALUE, NULL));
		Props.editor[0] = '\0';
		if (strlen((char *)xv_get(ip->editor_text, PANEL_VALUE)))
			strcpy(Props.editor, (char *)xv_get(ip->editor_text, PANEL_VALUE));
		Props.filter[0] = '\0';
		if (strlen((char *)xv_get(ip->filter_text, PANEL_VALUE)))
		strcpy(Props.filter, (char *)xv_get(ip->filter_text, PANEL_VALUE));
		strcpy(Props.log, (char *)xv_get(ip->log_text, PANEL_VALUE, NULL));
		Props.rescan = xv_get(ip->rescan_text, PANEL_VALUE, NULL);
		strncpy(Props.indent, (char *)xv_get(ip->indent_text, PANEL_VALUE, NULL), 7);
		Props.indent[7] = '\0';
		resize_panel(bw, Props.displaysize);
		Props.header = xv_get(ip->header_select, PANEL_TOGGLE_VALUE, 0, NULL);
		Props.sort = xv_get(ip->sort, PANEL_TOGGLE_VALUE, 0);
		defaults_set_integer("XVnews.Header", Props.header);
		defaults_set_integer("XVnews.sortSubjects", Props.sort);
		defaults_set_integer("XVnews.displaysize", Props.displaysize);
		defaults_set_string("XVnews.postEditor", Props.editor);
		if (strlen(Props.print))
		  defaults_set_string("XVnews.Print", Props.print);
		if (strlen(Props.filter))
		  defaults_set_string("XVnews.saveFilter", Props.filter);
		if (strlen(Props.log))
		  defaults_set_string("XVnews.logFile", Props.log);
		defaults_set_string("XVnews.Indent", Props.indent);
		defaults_set_integer("XVnews.Rescan", Props.rescan);
		Props.searchdefault = xv_get(xv_get(bw->search_butt, PANEL_ITEM_MENU,NULL),MENU_DEFAULT,NULL);
		Props.nextdefault = xv_get(xv_get(bw->next_butt, PANEL_ITEM_MENU,NULL),MENU_DEFAULT,NULL);
		Props.prevdefault = xv_get(xv_get(bw->prev_butt, PANEL_ITEM_MENU,NULL),MENU_DEFAULT,NULL);
		defaults_set_integer("XVnews.SearchDefault", Props.searchdefault);
		defaults_set_integer("XVnews.NextDefault", Props.nextdefault);
		defaults_set_integer("XVnews.PrevDefault", Props.prevdefault);
		init_rescan_timer(bw);
		assert( Global->home );
		sprintf(defaults, "%s/.Xdefaults", Global->home);
		defaults_store_db(defaults);
	}

	/* copy the props into OldProps */

	strcpy(OldProps.background, Props.background);
	strcpy(OldProps.highlight, Props.highlight);
	strcpy(OldProps.textcolor, Props.textcolor);
	OldProps.listfont = Props.listfont;
	OldProps.listfontsize = Props.listfontsize;
	OldProps.textfont = Props.textfont;
	OldProps.textfontsize = Props.textfontsize;
}


/*
 * Done callback function for `props'.
 */
void
props_done(frame)
	Frame		frame;
{
	(void)gcc_suspend(TRUE);
	xv_set(frame, XV_SHOW, FALSE, 0);

}

/*
 * Notify callback function for `props_done'.
 */
void
props_done_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_props_objects	*ip = (xvnews_props_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	(void)gcc_suspend(TRUE);
	xv_set(ip->props, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(ip->props, XV_SHOW, FALSE, NULL);
}


/*
 * Notify callback function for `props_color_class'.
 */
void
change_color_class(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	xvnews_props_objects	*ip = (xvnews_props_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch (value) {
	case 0:		xv_set(ip->props_color, PANEL_VALUE, 
				Props.background, NULL);
			break;
	case 1:		xv_set(ip->props_color, PANEL_VALUE, 
				Props.highlight, NULL);
			break;
	case 2:		xv_set(ip->props_color, PANEL_VALUE, 
				Props.textcolor, NULL);
			break;
	case 3:		xv_set(ip->props_color, PANEL_VALUE,
				Props.textback, NULL);
	}
}

/*
 * Notify callback function for `color_menu_button'.
 */
void
call_color_menu(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_props_objects	*ip = (xvnews_props_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_xvnews_window_objects *bp = (xvnews_xvnews_window_objects *) xv_get(xv_get(ip->props, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);

	if (!Chooser) {
		(void) gcc_initialize(bp->xvnews_window, 
        		"xvnews Color Chooser");
		Chooser = 1;
	} 
	gcc_deactivate();
	switch((int)xv_get(ip->props_color_class, PANEL_VALUE, NULL)) {
		case 0:		(void)gcc_activate("Background Color", "",
					change_color_prop, ip, Props.background);
				ColorChooser = 0;
				break;
		case 1:		(void)gcc_activate("Highlight color", "",
					change_color_prop, ip, Props.highlight);
				ColorChooser = 1;
				break;
		case 2:		(void)gcc_activate("Text color", "", 
					change_color_prop, ip, Props.textcolor);
				ColorChooser = 2;
				break;
		case 3:		(void)gcc_activate("Text Background", "",
					change_color_prop, ip, Props.textback);
				ColorChooser = 3;
		}
}

void
change_color_prop(color, ip )
char	*color;
xvnews_props_objects	*ip;
{
	switch (ColorChooser) {
	case 0:		strcpy(Props.background, color);
			break;
	case 1:		strcpy(Props.highlight, color);
			break;
	case 2:		strcpy(Props.textcolor, color);
			break;
	case 3:		strcpy(Props.textback, color);
	}

	if (ColorChooser == (int)xv_get(ip->props_color_class, PANEL_VALUE, NULL))
		xv_set(ip->props_color, PANEL_VALUE, color, NULL);

	return;
}

/*
 * Notify callback function for `props_font_class'.
 */
void
change_font_class(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	xvnews_props_objects	*ip = (xvnews_props_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	switch(value) {
	case 0:
		if (Props.listfont != -1)
			xv_set(ip->props_font, PANEL_VALUE, Props.listfont, NULL);
		xv_set(ip->props_fontsize, PANEL_VALUE, 
			Props.listfontsize, NULL);
		break;
	case 1:
		xv_set(ip->props_font,PANEL_VALUE,Props.textfont,NULL);
		xv_set(ip->props_fontsize, PANEL_VALUE, 
			Props.textfontsize, NULL);
	}
}

/*
 * Notify callback function for `props_font'.
 */
void
change_font(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	xvnews_props_objects	*ip = (xvnews_props_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	switch((int)xv_get(ip->props_font_class, PANEL_VALUE, NULL)) {
	case 0:
		Props.listfont = value;
		break;
	case 1:
		Props.textfont = value;
	}
}

/*
 * Notify callback function for `props_fontsize'.
 */
void
change_fontsize(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	xvnews_props_objects	*ip = (xvnews_props_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch((int)xv_get(ip->props_font_class, PANEL_VALUE, NULL)) {
	case 0:
		Props.listfontsize = value;
		break;
	case 1:
		Props.textfontsize = value;
	}
}

/*
 * Notify callback function for `props_reset'.
 */
void
reset_props(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_props_objects	*ip = (xvnews_props_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	strcpy(Props.background, OldProps.background);
	strcpy(Props.highlight, OldProps.highlight);
	strcpy(Props.textcolor, OldProps.textcolor);
	Props.listfont = OldProps.listfont;
	Props.listfontsize = OldProps.listfontsize;
	Props.textfont = OldProps.textfont;
	Props.textfontsize = OldProps.textfontsize;

	xv_set(ip->props_color_class, PANEL_VALUE, 0, NULL);
	xv_set(ip->props_color, PANEL_VALUE, Props.background, NULL);
	xv_set(ip->props_font_class, PANEL_VALUE, 0, NULL);
	xv_set(ip->props_font, PANEL_VALUE, Props.listfont, NULL);
	xv_set(ip->props_fontsize, PANEL_VALUE, Props.listfontsize, NULL);
}

static void apply_font(textsw, fn, fs)
Textsw		textsw;
int		fn;
int		fs;
{
	/* create and apply the given font to the given textsw */

	Xv_Font		font;

	switch (fn) {
	case -1:
		return;
	case 1:
		font = (Xv_Font)xv_find(textsw, FONT,
			FONT_FAMILY, FONT_FAMILY_LUCIDA,
			FONT_STYLE, FONT_STYLE_NORMAL,
			FONT_SIZE, fs,
			NULL);
		break;
	case 2:
		font = (Xv_Font)xv_find(textsw, FONT,
			FONT_FAMILY, FONT_FAMILY_ROMAN,
			FONT_STYLE, FONT_STYLE_NORMAL,
			FONT_SIZE, fs,
			NULL);
		break;
	case 3:
		font = (Xv_Font)xv_find(textsw, FONT,
			FONT_FAMILY, FONT_FAMILY_COUR,
			FONT_STYLE, FONT_STYLE_NORMAL,
			FONT_SIZE, fs,
			NULL);
		break;
	case 0:
		font = (Xv_Font)xv_find(textsw, FONT,
			FONT_FAMILY, FONT_FAMILY_DEFAULT_FIXEDWIDTH,
			FONT_STYLE, FONT_STYLE_NORMAL,
			FONT_SIZE, fs,
			NULL);
	}
	if (font)
		xv_set(textsw, TEXTSW_FONT, font, NULL);
}
		
extern void apply_defaults(ip)
xvnews_xvnews_window_objects *ip;
{
	/* apply the defaults that are in the database to the given IP */

	xvnews_props_objects *pp = (xvnews_props_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);
	char	*size;
static	int	resize = 0;
extern  struct globals	*Global;

	memset(Props.background, '\0', 30);
	strcpy(Props.background, 
		(char *)defaults_get_string("XVnews.background", NULL, ""));

	if (strlen(Props.background))
		xv_set(pp->props_color, PANEL_VALUE, Props.background, NULL);

	memset(Props.highlight, '\0', 30);
	strncpy(Props.highlight,
		(char *)defaults_get_string("XVnews.highlight", NULL, ""), 29);
	Props.highlight[29] = '\0';

	memset(Props.textcolor, '\0', 30);
	strncpy(Props.textcolor,
		(char *)defaults_get_string("XVnews.textcolor", NULL, ""), 29);
	Props.textcolor[29] = '\0';

	memset(Props.textback, '\0', 30);
	strncpy(Props.textback,
		(char *)defaults_get_string("XVnews.textback", NULL, ""), 29);
	Props.textback[29] = '\0';

	memset(Props.editor, '\0', 256);
	strcpy(Props.editor, 
		(char *)defaults_get_string("XVnews.postEditor", NULL, ""));
	memset(Props.print, '\0', 256);
	strcpy(Props.print, 
		(char *)defaults_get_string("XVnews.Print", NULL, ""));
	memset(Props.filter, '\0', 256);
	strcpy(Props.filter, 
		(char *)defaults_get_string("XVnews.saveFilter", NULL, ""));
	memset(Props.log, '\0', 256);
	strcpy(Props.log, 
		(char *)defaults_get_string("XVnews.logFile", NULL, ""));
	strncpy(Props.indent,
		(char *)defaults_get_string("XVnews.Indent", NULL, "> "),7);
	Props.indent[7] = '\0';
	Props.header = defaults_get_integer("XVnews.Header", NULL, 0);
	Props.sort = defaults_get_integer("XVnews.sortSubjects", NULL, 0);
	Props.listfont = defaults_get_integer("XVnews.listfont", NULL, -1);
	Props.rescan = defaults_get_integer("XVnews.Rescan", NULL, 15); 
	Props.searchdefault = defaults_get_integer("XVnews.SearchDefault", NULL, 2);
	Props.nextdefault = defaults_get_integer("XVnews.NextDefault", NULL, 3);
	Props.prevdefault = defaults_get_integer("XVnews.PrevDefault", NULL, 2);
	size = defaults_get_string("window.scale", "Window.Scale", "");
	if (!strcmp("large", size)) {
		int  width = defaults_get_integer("Window.Width","window.width",640);

		Props.displaysize = defaults_get_integer("XVnews.displaysize", NULL,8);
		if (width == 640)
			xv_set(ip->xvnews_window, XV_WIDTH, 815, NULL);
		Props.listfontsize = defaults_get_integer("XVnews.listfontsize",
                    NULL, 3);
	} else {
		Props.listfontsize = defaults_get_integer("XVnews.listfontsize",
                    NULL, 2);
		Props.displaysize = defaults_get_integer("XVnews.displaysize", NULL,9);
	}
	Global->multiclick = defaults_get_integer(
		"openwindows.multiclicktimeout", 
			"OpenWindows.MultiClickTimeout", 4);
	xv_set((Menu)xv_get(ip->search_butt, PANEL_ITEM_MENU, NULL), MENU_DEFAULT, Props.searchdefault, NULL);
	xv_set((Menu)xv_get(ip->next_butt, PANEL_ITEM_MENU, NULL), MENU_DEFAULT, Props.nextdefault, NULL);
	xv_set((Menu)xv_get(ip->prev_butt, PANEL_ITEM_MENU, NULL), MENU_DEFAULT, Props.prevdefault, NULL);
	if (Props.listfont!= -1)
		xv_set(pp->props_font, PANEL_VALUE, Props.listfont, NULL);
	if (Props.listfontsize != -1)
		xv_set(pp->props_fontsize, PANEL_VALUE, Props.listfontsize, NULL);
	xv_set(pp->display_text, PANEL_VALUE, Props.displaysize, NULL);
	xv_set(pp->rescan_text, PANEL_VALUE, Props.rescan, NULL);
	xv_set(pp->indent_text, PANEL_VALUE, Props.indent, NULL);
	xv_set(pp->header_select, PANEL_TOGGLE_VALUE, 0, Props.header, NULL);
	xv_set(pp->sort, PANEL_TOGGLE_VALUE, 0, Props.sort, NULL);
	if (strlen(Props.editor)) 
		xv_set(pp->editor_text, PANEL_VALUE, Props.editor, NULL);
	if (strlen(Props.print)) 
		xv_set(pp->print_text, PANEL_VALUE, Props.print, NULL);
	if (strlen(Props.filter)) 
		xv_set(pp->filter_text, PANEL_VALUE, Props.filter, NULL);
	if (strlen(Props.log)) 
		xv_set(pp->log_text, PANEL_VALUE, Props.log, NULL);
	else {
		char	file[256];

		sprintf(file, "%s/news.record", Global->newsdir);
		xv_set(pp->log_text, PANEL_VALUE, file, NULL);
	}
			
	Props.textfontsize = defaults_get_integer("XVnews.textfontsize",NULL, 3);
	Props.textfont = defaults_get_integer("XVnews.textfont", NULL, 0);

	if (resize == 0) {
		resize_panel(ip, Props.displaysize);
		resize = 1;
	}
	apply_props(pp->props, NULL);
}
