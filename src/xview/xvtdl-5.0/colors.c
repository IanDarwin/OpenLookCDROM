/*
 *  $Id$
 * **********************************************************************
 *
 *   Colors.c => routines that govern the use and manipulation of
 *               colors.
 *
 * ----------------------------------------------------------------------
 * Copyright (c) 1993 by Mike Jipping and Hope College
 *
 * Permission is granted to copy and distribute this file in modified or
 * unmodified form, for noncommercial use, provided (a) this copyright notice
 * is preserved, (b) no attempt is made to restrict redistribution of this
 * file, and (c) this file is not distributed as part of any collection whose
 * redistribution is restricted by a compilation copyright.
 * ----------------------------------------------------------------------
 *
 * Revision History:
 *
 *  $Log$
 */

#include "globaldefs.h"

extern props_props_frame_objects	*props_props_frame;

void close_foreground_color_chooser();
void close_background_color_chooser();

int gcc_initialized=FALSE;

/*
 * **********************************************************************
 * This closes the color chooser and sets the property frame value.
 */
void close_foreground_color_chooser(cn, cd)
char *cn;
caddr_t cd;
{
	xv_set(props_props_frame->fore_color, PANEL_VALUE, cn, 0);
}

/*
 * **********************************************************************
 * This closes the color chooser and sets the property frame value.
 */
void close_background_color_chooser(cn, cd)
char *cn;
caddr_t cd;
{
	xv_set(props_props_frame->back_color, PANEL_VALUE, cn, 0);
}

/*
 * **********************************************************************
 * This routine simply opens the color chooser for the fourground
 * color.
 */
void open_foreground_color_chooser (item, event)
Panel_item item; 
Event      *event;
{
	xv_set(item, PANEL_NOTIFY_STATUS, XV_ERROR, 0);
	if (!gcc_initialized) {
		gcc_initialize(tdlist, "Color Chooser");
		gcc_initialized = TRUE;
	}
	gcc_activate(NULL, NULL, (void *)close_foreground_color_chooser, NULL, "ivory");
}

/*
 * **********************************************************************
 * This routine simply opens the color chooser for the background
 * color.
 */
void open_background_color_chooser (item, event)
Panel_item item; 
Event      *event;
{
	xv_set(item, PANEL_NOTIFY_STATUS, XV_ERROR, 0);
	if (!gcc_initialized) {
		gcc_initialize(tdlist, "Color Chooser");
		gcc_initialized = TRUE;
	}
	gcc_activate(NULL, NULL, (void *)close_background_color_chooser, NULL, "blue");
}

