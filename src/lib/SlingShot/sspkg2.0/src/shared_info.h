/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef X_MISC_INCLUDED
#define X_MISC_INCLUDED

/* @(#)shared_info.h 1.8 92/07/07 */

#include <X11/Xlib.h>
#include <sspkg/canshell.h>

typedef struct {
	Canvas_shell		canvas_shell;
	Display 		*dpy;
	int			screen_number;
	Xv_opaque		cms;		/* XView "Cms" */
	short			num_colors;
	short			control_cms;	/* bool */
	short			win_fg;
	short			win_bg;
	short			bg2;
	short			bg3;
	short			white;
	unsigned long		*pixels;

	Xv_opaque /*Xv_Font*/	font;		/* Should never be NULL. */
	XFontStruct		*font_info;

	Xv_opaque /*Panel*/	edit_panel;
	Xv_opaque /*Panel_text*/edit_item;
	Xv_opaque /*Drawtext*/	focus_drawtext;
} Shared_info;


/* 
 * control cms index:		OpenLook name	rectobj attr/macro
 * ------------------		-------------	---------------
 * 0  (WIN_BACKGROUND_COLOR) 	BG1		RECTOBJ_BG_COLOR/pixel_bg
 * 1		 		BG2 "indented"	RECTOBJ_BG2/pixel_bg2
 * 2 				BG3 "shadow"	RECTOBJ_BG3/pixel_bg3
 * 3 				White		RECTOBJ_WHITE/pixel_white
 *
 * last (WIN_FOREGROUND_COLOR)	none		RECTOBJ_FG_COLOR/pixel_fg
 */

#define pixel_fg(sh_info, index)					\
		*((sh_info)->pixels + 					\
			(((index) == -1) ? ((sh_info)->win_fg) : (index)))

#define pixel_bg(sh_info, index)					\
		*((sh_info)->pixels + 					\
			(((index) == -1) ? ((sh_info)->win_bg) : (index)))

#define pixel_bg2(sh_info, index)					\
		*((sh_info)->pixels + 					\
			(((index) == -1) ? ((sh_info)->bg2) : (index)))

#define pixel_bg3(sh_info, index)					\
		*((sh_info)->pixels + 					\
			(((index) == -1) ? ((sh_info)->bg3) : (index)))

#define pixel_white(sh_info, index)					\
		*((sh_info)->pixels + 					\
			(((index) == -1) ? ((sh_info)->white) : (index)))

#endif

