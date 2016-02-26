/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef CANVAS_SHELL_IMPL_H
#define CANVAS_SHELL_IMPL_H

/* @(#)canshell_impl.h 1.10 92/10/21 */

#include <sspkg/canshell.h>
#include <sspkg/rectobj.h>
#include <sspkg/list.h>
#include "rectobj_impl.h"


typedef struct canvas_shell_info {
	Rectobj_info	rectobj_info;	/* must be first for dual inheritance */
	Shared_info	shared_info;
	Listnode	listnode;	/* attach to list of all canvas shells*/
	Pixmap		batch_pixmap;
	Rect		repaint_rect;

	/* flags */
	char		repaint_clear;
	char		batch_repaint;
	char		auto_drop_site;
	char		win_mapped;

	short		delay_repaint; /* not boolean - maintains count */

	Rectobj		grab_rectobj;
	Proc_ptr	grab_event_proc;
	void		*grab_arg;
	int		(*misc_event_proc)();

	Proc_ptr	resize_proc;

} Canvas_shell_info;

#define CANVAS_SHELL_PRIVATE(canvas_shell)	\
		XV_PRIVATE(Canvas_shell_info, Canvas_shell_struct, canvas_shell)
#define CANVAS_SHELL_PUBLIC(canvas_shell_info)	\
		(Canvas_shell)(canvas_shell_info->rectobj_info.listnode.handle)

#endif

