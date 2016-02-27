/* FILE xplan.c ****************************************
 *
 * xplan - project planning tool
 * Copyright (C) 1992 Brian Gaubert, Mark M. Lacey, Richard Malingkas,
 * and Mike Marlow.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License (distributed with this program in the file
 * COPYING) for more details.
 * 
 * If you did not received a copy of the GNU General Public License
 * along with this program, write to the Free Software Foundation,
 * Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * Since this was a project for a one semester software engineering
 * course, the authors will not be offering support for the product
 * after its release.
 *
 * DESCRIPTION OF CONTENTS
 *
 * xplan.c - Contains main() for project xplan
 * This file was generated by `gxv'.
 *
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/dragdrop.h>
#include <xview/cursor.h>
#include <gdd.h>

#include <time.h>

#include "db.h"
#include "main_ui.h"
#include "file_ui.h"
#include "PERT_Xui.h"
#include "dependencies_ui.h"
#include "export_ui.h"
#include "gantt_Xui.h"
#include "resources_ui.h"
#include "taskinfo_ui.h"
#include "string_table.h"


/*
 * External variable declarations.
 */
main_mainWindow_objects	*Main_mainWindow;
file_filePopup_objects	*File_filePopup;
PERT_pertPopup_objects	*PERT_pertPopup;
dependencies_dependenciesPopup_objects	*Dependencies_dependenciesPopup;
export_exportPopup_objects	*Export_exportPopup;
gantt_ganttPopup_objects	*Gantt_ganttPopup;
resources_resourcesPopup_objects	*Resources_resourcesPopup;
taskinfo_taskinfoPopup_objects	*Taskinfo_taskinfoPopup;

extern char project_filename[];
extern char export_filename[];

#ifdef MAIN

/*
 * Instance XV_KEY_DATA key.  An instance is a set of related
 * user interface objects.  A pointer to an object's instance
 * is stored under this key in every object.  This must be a
 * global variable.
 */
Attr_attribute	INSTANCE;

/*
 * main for project xplan
 */
void
main(int argc, char **argv)
{
   /* print copyright information */
   puts("xplan version 1.0, Copyright (C) 1992 Brian Gaubert, "
	"Mark M. Lacey,\nRichard Malingkas, and Mike Marlow.\n"
	"xplan comes with ABSOLUTELY NO WARRANTY; for details see the "
	"file COPYING,\nwhich you should have received with this "
	"software.  This is free software,\nand you are welcome to "
	"redistribute it under certain conditions (again see\nthe "
	"file COPYING for more details).");

   /* set up initial filenames */
   strcpy(project_filename, "untitled.prj");
   strcpy(export_filename, "untitled.tex");
   
	/*
	 * Initialize XView.
	 */
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
	INSTANCE = xv_unique_key();

	/*
	 * Initialize user interface components.
	 * Do NOT edit the object initializations by hand.
	 */
	Main_mainWindow = main_mainWindow_objects_initialize(NULL, NULL);
	Dependencies_dependenciesPopup = dependencies_dependenciesPopup_objects_initialize(NULL, Main_mainWindow->mainWindow);
	File_filePopup = file_filePopup_objects_initialize(NULL, Main_mainWindow->mainWindow);
	PERT_pertPopup = PERT_pertPopup_objects_initialize(NULL, Main_mainWindow->mainWindow);
	Export_exportPopup = export_exportPopup_objects_initialize(NULL, Main_mainWindow->mainWindow);
	Gantt_ganttPopup = gantt_ganttPopup_objects_initialize(NULL, Main_mainWindow->mainWindow);
	Resources_resourcesPopup = resources_resourcesPopup_objects_initialize(NULL, Main_mainWindow->mainWindow);
	Taskinfo_taskinfoPopup = taskinfo_taskinfoPopup_objects_initialize(NULL, Main_mainWindow->mainWindow);
	
	/*
	 * Initialize the Drag Drop package.
	 */
	gdd_init_dragdrop(Main_mainWindow->mainWindow);

   /* Create our main task list, which will be used throughout the */
   /* application */
   create_main_task_list();
   create_resource_table();
	
	/*
	 * Turn control over to XView.
	 */
	xv_main_loop(Main_mainWindow->mainWindow);
	exit(0);
}

#endif
