/*
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
 * Functions that are used by the GUI frequently
 *
 */
#include <stdio.h>
#include <dirent.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/dragdrop.h>
#include <xview/cursor.h>
#include <xview/notice.h>
#include <gdd.h>
#include "string_table.h"
#include "db.h"
#include "julian.h"
#include "xplan.h"

extern int selected_dep;
extern struct task_node *current_dep;

extern int selected_res;
extern struct resource_node *current_res;

/* FUNCTION load_data_to_taskinfoPopup ***********************************

   PURPOSE

   Loads the data from a task node into the task info popup.

   SAMPLE CALL

   load_data_to_taskinfoPopup(node);

   INPUTS

   node --- a task node

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Dec 1992
   Tester.... Mark M. Lacey, 7 Dec 1992

   MODIFICATIONS (most recent to least)

*/
void load_data_to_taskinfoPopup(struct task_node *node)
{
   char *temp;
   int value;

   if (!node) return;

   if (node->data->name != NULL) {
      /* set the name in the popup window */
      xv_set(Taskinfo_taskinfoPopup->name, PANEL_VALUE,
	     node->data->name, NULL);
   } else {
      xv_set(Taskinfo_taskinfoPopup->name, PANEL_VALUE,
	     "", NULL);
   }
   
   if (node->data->desc != NULL) {
      /* set the description in the popup window */
      xv_set(Taskinfo_taskinfoPopup->description, PANEL_VALUE,
	     node->data->desc, NULL);
   } else {
      xv_set(Taskinfo_taskinfoPopup->description, PANEL_VALUE,
	     "", NULL);
   }

   /* set the duration in the popup window */
   xv_set(Taskinfo_taskinfoPopup->duration, PANEL_VALUE,
	  node->data->duration, NULL);

   xv_set(Taskinfo_taskinfoPopup->floatTime, PANEL_VALUE,
	  node->data->float_time, NULL);

   /* we use zero for empty dates throughout */
   if (node->data->planned_start_date==0) {
      xv_set(Taskinfo_taskinfoPopup->plannedStartDate, PANEL_VALUE,
	     "", NULL);
   } else {
      temp = julian_to_str_date(node->data->planned_start_date);
      xv_set(Taskinfo_taskinfoPopup->plannedStartDate, PANEL_VALUE,
	     temp, NULL);
   }

   if (node->data->planned_end_date==0) {
      xv_set(Taskinfo_taskinfoPopup->plannedEndDate, PANEL_VALUE,
	     "", NULL);
   } else {
      temp = julian_to_str_date(node->data->planned_end_date);
      xv_set(Taskinfo_taskinfoPopup->plannedEndDate, PANEL_VALUE,
	     temp, NULL);
   }

   if (node->data->actual_start_date==0) {
      xv_set(Taskinfo_taskinfoPopup->actualStartDate, PANEL_VALUE,
	     "", NULL);
   } else {
      temp = julian_to_str_date(node->data->actual_start_date);
      xv_set(Taskinfo_taskinfoPopup->actualStartDate, PANEL_VALUE,
	     temp, NULL);
   }

   if (node->data->actual_end_date==0) {
      xv_set(Taskinfo_taskinfoPopup->actualEndDate, PANEL_VALUE,
	     "", NULL);
   } else {
      temp = julian_to_str_date(node->data->actual_end_date);
      xv_set(Taskinfo_taskinfoPopup->actualEndDate, PANEL_VALUE,
	     temp, NULL);
   }
      
   if (node->data->forecast_start_date==0) {
      xv_set(Taskinfo_taskinfoPopup->forecastStartDate, PANEL_VALUE,
	     "", NULL);
   } else {
      temp = julian_to_str_date(node->data->forecast_start_date);
      xv_set(Taskinfo_taskinfoPopup->forecastStartDate, PANEL_VALUE,
	     temp, NULL);
   }

   if (node->data->forecast_end_date==0) {
      xv_set(Taskinfo_taskinfoPopup->forecastEndDate, PANEL_VALUE,
	     "", NULL);
   } else {
      temp = julian_to_str_date(node->data->forecast_end_date);
      xv_set(Taskinfo_taskinfoPopup->forecastEndDate, PANEL_VALUE,
	     temp, NULL);
   }

   if (node->data->earliest_start_date==0) {
      xv_set(Taskinfo_taskinfoPopup->earliestStartDate, PANEL_VALUE,
	     "", NULL);
   } else {
      temp = julian_to_str_date(node->data->earliest_start_date);
      xv_set(Taskinfo_taskinfoPopup->earliestStartDate, PANEL_VALUE,
	     temp, NULL);
   }

   if (node->data->earliest_end_date==0) {
      xv_set(Taskinfo_taskinfoPopup->earliestEndDate, PANEL_VALUE,
	     "", NULL);
   } else {
      temp = julian_to_str_date(node->data->earliest_end_date);
      xv_set(Taskinfo_taskinfoPopup->earliestEndDate, PANEL_VALUE,
	     temp, NULL);
   }

   if (node->data->latest_start_date==0) {
      xv_set(Taskinfo_taskinfoPopup->latestStartDate, PANEL_VALUE,
	     "", NULL);
   } else {
      temp = julian_to_str_date(node->data->latest_start_date);
      xv_set(Taskinfo_taskinfoPopup->latestStartDate, PANEL_VALUE,
	     temp, NULL);
   }

   if (node->data->latest_end_date==0) {
      xv_set(Taskinfo_taskinfoPopup->latestEndDate, PANEL_VALUE,
	     "", NULL);
   } else {
      temp = julian_to_str_date(node->data->latest_end_date);
      xv_set(Taskinfo_taskinfoPopup->latestEndDate, PANEL_VALUE,
	     temp, NULL);
   }

   value = node->data->milestone;
   value |= ((int)node->data->deliverable) << 1;
   xv_set(Taskinfo_taskinfoPopup->options, PANEL_VALUE, value, NULL);
}

/* FUNCTION fill_dependency_popup ***********************************

   PURPOSE

   Loads the data from a task node into the dependency popup

   SAMPLE CALL

   fill_dependency_popup(node);

   INPUTS

   node --- a task node

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Dec 1992
   Tester.... Mark M. Lacey, 7 Dec 1992

   MODIFICATIONS (most recent to least)

*/
void fill_dependency_popup(struct task_node *node)
{
   struct task_node *traverse;
   int nrows, count=0;

   if (!node) return;

   traverse = node->data->dependencies->head;

   /* find the number of rows in the popup */
   nrows = (int)
     xv_get(Dependencies_dependenciesPopup->dependencyList,
	    PANEL_LIST_NROWS);

   /* if it is non-zero, delete them */
   if (nrows != 0)
     xv_set(Dependencies_dependenciesPopup->dependencyList,
	    PANEL_LIST_DELETE_ROWS, 0, nrows, NULL);

   /* traverse the dependencies and list them */
   while (traverse) {
      xv_set(Dependencies_dependenciesPopup->dependencyList,
	     PANEL_LIST_INSERT, count, PANEL_LIST_STRING, count,
	     traverse->data->name, NULL);
      ++count;
      traverse = traverse->next;
   }

   if (count != 0) {
      xv_set(Dependencies_dependenciesPopup->dependencyList,
	     PANEL_LIST_SELECT, 0, TRUE, NULL);
      selected_dep = 0;
   } else {
      selected_dep = -1;
   }
   current_dep = node->data->dependencies->head;
}

/* FUNCTION fill_resource_popup ****************************************

   Loads the data from a task node into the resource popup

   SAMPLE CALL

   fill_resource_popup(node);

   INPUTS

   node --- a task node

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Dec 1992
   Tester.... Mark M. Lacey, 7 Dec 1992

   MODIFICATIONS (most recent to least)

*/
void fill_resource_popup(struct task_node *node)
{
   struct resource_node *res;
   int nrows, count = 0;

   if (!node) return;

   res = node->data->resources->head;

   nrows = (int)
     xv_get(Resources_resourcesPopup->resourceList,
	    PANEL_LIST_NROWS);

   if (nrows != 0)
     xv_set(Resources_resourcesPopup->resourceList,
	    PANEL_LIST_DELETE_ROWS, 0, nrows, NULL);

   /* for each resource, add it to the list */
   while (res) {
      xv_set(Resources_resourcesPopup->resourceList,
	     PANEL_LIST_INSERT, count, PANEL_LIST_STRING,
	     count, res->data->resource->name, NULL);
      ++count;
      res = res->next;
   }

   if (count != 0) {
      xv_set(Resources_resourcesPopup->resourceList,
	     PANEL_LIST_SELECT, 0, TRUE, NULL);
      selected_res = 0;
   } else {
      selected_res = -1;
   }
   current_res = node->data->resources->head;
}
     
/* FUNCTION store_data_from_taskinfoPopup *********************************

   Retrieves information from task info popup

   SAMPLE CALL

   store_data_from_taskinfoPopup(node);

   INPUTS

   node --- a task node

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Dec 1992
   Tester.... Mark M. Lacey, 7 Dec 1992

   MODIFICATIONS (most recent to least)

*/
void store_data_from_taskinfoPopup(struct task_node *node)
{
   char
     *name,
     *description,
     *planned_start_date_str,
     *planned_end_date_str,
     *actual_start_date_str,
     *actual_end_date_str,
     *forecast_start_date_str,
     *forecast_end_date_str,
     *str;
   int value, t;
   Xv_notice notice;

   if (!node) return;

   /* Get the task name from the popup window */
   name = (char *) xv_get(Taskinfo_taskinfoPopup->name,
			  PANEL_VALUE);
   if (node->data->name != NULL) free(node->data->name);
   node->data->name = (char *) malloc(strlen(name)+1);
   strcpy(node->data->name, name);

   /* Get the task description from the popup window */
   description = (char *)
     xv_get(Taskinfo_taskinfoPopup->description, PANEL_VALUE);
   
   if (node->data->desc != NULL) free(node->data->desc);
   node->data->desc = (char *) malloc(strlen(description)+1);
   strcpy(node->data->desc, description);

   /* Get the task duration from the popup window */
   node->data->duration = (unsigned)
     xv_get(Taskinfo_taskinfoPopup->duration, PANEL_VALUE);
   
   /* Get the planned start date from the popup window and convert it */
   /* to our internal format */
   str = planned_start_date_str = (char *)
     xv_get(Taskinfo_taskinfoPopup->plannedStartDate, PANEL_VALUE);

   t = str_date_to_julian(planned_start_date_str);
   node->data->planned_start_date = (unsigned) t;
   /* empty string */

   if (!t && (strtok(str, " ")!=NULL)) {
      notice = xv_create(Taskinfo_taskinfoPopup->taskinfoPopup, NOTICE,
			 NOTICE_MESSAGE_STRINGS, 
			 "Planned start date is invalid",
			 NULL,
			 NOTICE_BUTTON_YES, "Continue",
			 XV_SHOW, TRUE,
			 NULL);
      xv_destroy_safe(notice);
      return;
   }


   /* Get the planned end date from the popup window and convert it */
   /* to our internal format */
   str = planned_end_date_str = (char *)
     xv_get(Taskinfo_taskinfoPopup->plannedEndDate, PANEL_VALUE);
   t = str_date_to_julian(planned_end_date_str);
   node->data->planned_end_date = (unsigned) t;
   
   if (!t && (strtok(str, " ")!=NULL)) {
      notice = xv_create(Taskinfo_taskinfoPopup->taskinfoPopup, NOTICE,
			 NOTICE_MESSAGE_STRINGS, 
			 "Planned end date is invalid",
			 NULL,
			 NOTICE_BUTTON_YES, "Continue",
			 XV_SHOW, TRUE,
			 NULL);
      xv_destroy_safe(notice);
      return;
   }
   /* Get the actual start date from the popup window and convert it */
   /* to our internal format */
   str = actual_start_date_str = (char *)
     xv_get(Taskinfo_taskinfoPopup->actualStartDate, PANEL_VALUE);
   t = str_date_to_julian(actual_start_date_str);
   node->data->actual_start_date = (unsigned) t;
   
   if (!t && (strtok(str, " ")!=NULL)) {
      notice = xv_create(Taskinfo_taskinfoPopup->taskinfoPopup, NOTICE,
			 NOTICE_MESSAGE_STRINGS, 
			 "Actual start date is invalid",
			 NULL,
			 NOTICE_BUTTON_YES, "Continue",
			 XV_SHOW, TRUE,
			 NULL);
      xv_destroy_safe(notice);
      return;
   }


   /* Get the actual end date from the popup window and convert it */
   /* to our internal format */
   str = actual_end_date_str = (char *)
     xv_get(Taskinfo_taskinfoPopup->actualEndDate, PANEL_VALUE);
   t = str_date_to_julian(actual_end_date_str);
   node->data->actual_end_date = (unsigned) t;
   
   if (!t && (strtok(str, " ")!=NULL)) {
      notice = xv_create(Taskinfo_taskinfoPopup->taskinfoPopup, NOTICE,
			 NOTICE_MESSAGE_STRINGS, 
			 "Actual end date is invalid",
			 NULL,
			 NOTICE_BUTTON_YES, "Continue",
			 XV_SHOW, TRUE,
			 NULL);
      xv_destroy_safe(notice);
      return;
   }

   /* Get the forecast start date from the popup window and convert it */
   /* to our internal format */
   str = forecast_start_date_str = (char *)
     xv_get(Taskinfo_taskinfoPopup->forecastStartDate, PANEL_VALUE);
   t = str_date_to_julian(forecast_start_date_str);
   node->data->forecast_start_date = (unsigned) t;
   
   if (!t && (strtok(str, " ")!=NULL)) {
      notice = xv_create(Taskinfo_taskinfoPopup->taskinfoPopup, NOTICE,
			 NOTICE_MESSAGE_STRINGS, 
			 "Forecast start date is invalid",
			 NULL,
			 NOTICE_BUTTON_YES, "Continue",
			 XV_SHOW, TRUE,
			 NULL);
      xv_destroy_safe(notice);
      return;
   }
   /* Get the forecast end date from the popup window and convert it */
   /* to our internal format */
   str = forecast_end_date_str = (char *)
     xv_get(Taskinfo_taskinfoPopup->forecastEndDate, PANEL_VALUE);
   t = str_date_to_julian(forecast_end_date_str);
   node->data->forecast_end_date = (unsigned) t;
   
   value = (int) xv_get(Taskinfo_taskinfoPopup->options, PANEL_VALUE);
   
   if (!t && (strtok(str, " ")!=NULL)) {
      notice = xv_create(Taskinfo_taskinfoPopup->taskinfoPopup, NOTICE,
			 NOTICE_MESSAGE_STRINGS, 
			 "Forecast end date is invalid",
			 NULL,
			 NOTICE_BUTTON_YES, "Continue",
			 XV_SHOW, TRUE,
			 NULL);
      xv_destroy_safe(notice);
      return;
   }


   node->data->milestone = value & 1;
   node->data->deliverable = (value & 2) >> 1;
}

/* FUNCTION store_data_from_taskinfoPopup *********************************

   Retrieves information from task info popup

   SAMPLE CALL

   store_data_from_taskinfoPopup(node);

   INPUTS

   node --- a task node

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Dec 1992
   Tester.... Mark M. Lacey, 7 Dec 1992

   MODIFICATIONS (most recent to least)

*/

/* FUNCTION fill_filelist ****************************************

   PURPOSE

   Fill the filelist with the contents a directory

   SAMPLE CALL

   fill_filelist(path);

   INPUTS

   path --- the directory to read

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Dec 1992
   Tester.... Mark M. Lacey, 7 Dec 1992

   MODIFICATIONS (most recent to least)

*/
void fill_filelist(char *path)
{
   DIR            *dirp;	/* points to directory structure */
   struct dirent  *dp;	/* points to directory entry structure */
   struct stat     s_buf;	/* points to file stat structure */
   char            buf[MAXNAMLEN + 1];	/* holds directory entry name */
   int             nrows;	/* how many rows in scrolling list */
   char ext[5];
   int len;
   
   if (stat(path, &s_buf) == -1) {
      perror(path);
   }

   /* handle unreadable directory */
   if ((s_buf.st_mode & S_IREAD) == 0) {
      /* do something here */
   } else {
      /* is the path a directory? */
      if (S_ISDIR(s_buf.st_mode)) {
	 /* is the directory openable? */
	 if ((dirp = opendir(path)) == NULL) {
	    perror(path);
	 }
	 nrows = xv_get(File_filePopup->fileList, PANEL_LIST_NROWS);
	 xv_set(File_filePopup->fileList, PANEL_LIST_DELETE_ROWS,
		0, nrows, NULL);

	 /* create a list of selectable items */
	 /* while there are entries in the directory */
	 
	 for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
	    /* skip if the entry is current dir */
	    if (strcmp(dp->d_name, ".")) {
	       len = strlen(dp->d_name);
	       if (!stat(dp->d_name, &s_buf)) {
		  if (S_ISDIR(s_buf.st_mode)) {
		     /* if it's a directory */
		     strcpy(buf, dp->d_name);
		     strcat(buf, "/");
		     /* add selection to list */
		     xv_set(File_filePopup->fileList,
			    PANEL_LIST_INSERT, 0,
			    PANEL_LIST_STRING, 0, buf,
			    NULL);
		  } else {
		     if (len < 4) continue;
		     /* if it is a project file, add it */
		     if (strcmp(&dp->d_name[len-4], ".prj")==0) {
			/* add selection to list */
			strcpy(buf, dp->d_name);
			xv_set(File_filePopup->fileList,
			       PANEL_LIST_INSERT, 0,
			       PANEL_LIST_STRING, 0, buf,
			       NULL);
		     }
		  }
	       }
	    }
	 }
	 if (closedir(dirp))	/* close the directory */
	   perror();
	 xv_set(File_filePopup->fileList,	/* sort the list,
						 * ascending */
		PANEL_LIST_SORT, PANEL_FORWARD, NULL);
      }
   }
}

