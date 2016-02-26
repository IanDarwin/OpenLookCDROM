/*
 * $Id$
 * **********************************************************************
 *
 *   File.c ==> routines that control file handling.
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
 * $Log$
 */

#include "globaldefs.h"
#include "gfm.h"
#include "category_ui.h"

extern FILE *yyin;
void freeup();
void refresh_db();
void refresh_category();

int merging, checking=FALSE;

#ifndef TDL
extern category_category_base_objects	*Category_category_base;
gfm_popup_objects *file_window;

/*
 * **********************************************************************
 * This routine merges an out-of-date todo database with the current 
 * in-memory database.  Most of the work is done by the parsing code
 * (todo.y), but in reaction to the "mergin flag" set here.  We also must
 * reset menus ans such.
 */
void merge_in_changes()
{
	struct stat *st;
	int sterror2, response;

   /*
    * Do the merging thang...
    */
	merging = TRUE;

	yyin = fopen(fname, "r");
	yyparse();
	propagate();
	fclose(yyin);

	merging = FALSE;

   /*
    *  Reset menus and category markers
    */
	xv_set(categories,
			 PANEL_ITEM_MENU, cat_menu(category_head),
          PANEL_CLIENT_DATA, category_head,
			 0);
	xv_set(category_name, PANEL_LABEL_STRING, category_head->name, 0);
	xv_set(entry_category, PANEL_ITEM_MENU, cat_menu(category_head), 0);
	xv_set(entry_category_name, PANEL_LABEL_STRING, category_head->name, 0);
	xv_set(Category_category_base->cat_insert_location,
			 PANEL_ITEM_MENU, cat_menu(category_head),
			 0);
	xv_set(Category_category_base->cat_insert_location_name,
			 PANEL_LABEL_STRING, category_head->name,
			 0);

	entry_head = category_head->entry_head;
	entry_tail = category_head->entry_tail;
	rl_head = category_head->rl_head;
	rl_tail = category_head->rl_tail;
	display_list(curr_month, curr_day, curr_year);

   /*
    *  Reset our idea of modification times.
    */
	st = NEW(struct stat);
	sterror2 = stat(fname, st);
	modify_time = st->st_mtime;
}

/*
 * **********************************************************************
 * Initialize the GFM file manager.
 */
void initialize_file_stuff()
{
	merging = 0;
	file_window = gfm_initialize(NULL, tdlist, "File Dialog");
}

/*
 * **********************************************************************
 * This routine checks and handles the difference in the modification
 * times of the database file and the in-memory version.
 */
int file_status_ok()
{
	struct stat *st;
	int sterror2, response;

	checking = TRUE;
	st = NEW(struct stat);
	sterror2 = stat(fname, st);
	if (st->st_mtime > modify_time) {
		response = notice_prompt(tdlist, NULL,
										 NOTICE_MESSAGE_STRINGS,
										    "The ToDo database file has been updated",
										    "by someone else.  What do you want to do?",
										 0,
										 NOTICE_BUTTON, "Merge changes", 0,
										 NOTICE_BUTTON, "Save/overwrite", 1,
										 NOTICE_BUTTON, "Ignore changes", 2,
										 0);
		
		switch (response) {
   		case 0:
			   merge_in_changes();
				checking = FALSE;
			   return 1;
		   case 1:
				changed = TRUE;
				refresh_db(FALSE);
				checking = FALSE;
			   return 1;
			case 2:
				checking = FALSE;
            return 0;
		}
	} else {
		checking = FALSE;
		return 1;
	}
}

/*
 * **********************************************************************
 * Complete the loading of a file from the file manager window.  This
 * means pointer resetting and file parsing.
 */
int finish_file_load(gip, dir, file)
gfm_popup_objects *gip;
char              *dir;
char              *file;
{
	if (changed) refresh_db(TRUE);
 
	freeup();

   sprintf(fname, "%s/%s", dir, file);

	yyin = fopen(fname, "r");
	if (yyin == NULL) {
		notice_prompt(tdlist, NULL,
						  NOTICE_MESSAGE_STRINGS,
						     fname,
						     "does not exist!",
						  0,
						  NOTICE_BUTTON, "Ok", 0,
						  0);
		return GFM_ERROR;
	}
	yyparse();
	propagate();
	fclose(yyin);

	if (category_head == NULL)
		category_head = (struct category_rec *)new_category("Every Day", NULL, FALSE);
	
	xv_set(categories,
			 PANEL_ITEM_MENU, cat_menu(category_head),
          PANEL_CLIENT_DATA, category_head,
			 0);
	xv_set(category_name, PANEL_LABEL_STRING, category_head->name, 0);
	xv_set(entry_category, PANEL_ITEM_MENU, cat_menu(category_head), 0);
	xv_set(entry_category_name, PANEL_LABEL_STRING, category_head->name, 0);
	xv_set(Category_category_base->cat_insert_location,
			 PANEL_ITEM_MENU, cat_menu(category_head),
			 0);
	xv_set(Category_category_base->cat_insert_location_name,
			 PANEL_LABEL_STRING, category_head->name,
			 0);

	entry_head = category_head->entry_head;
	entry_tail = category_head->entry_tail;
	rl_head = category_head->rl_head;
	rl_tail = category_head->rl_tail;
	display_list(curr_month, curr_day, curr_year);

	return GFM_OK;
}

/*
 * **********************************************************************
 * Activate the file manager window for a file load.
 */
Menu_item load_file(item, op)
Menu_item	item;
Menu_generate	op;
{
	if (op == MENU_NOTIFY) {
		gfm_activate(file_window, NULL, NULL, NULL, finish_file_load, NULL, GFM_LOAD);
	}
	return item;
}

/*
 * **********************************************************************
 * This routine completes a merge of a file into the current database.
 * This is different than an "out-of-date merge" (naming confusion is
 * unfortunate); this simply reads in the file given into the database.
 */
int finish_file_merge(gip, dir, file)
gfm_popup_objects *gip;
char              *dir;
char              *file;
{
	char oldname[256];

	strcpy(oldname, fname);
	
   sprintf(fname, "%s/%s", dir, file);
 
	yyin = fopen(fname, "r");
	if (yyin == NULL) {
		notice_prompt(tdlist, NULL,
						  NOTICE_MESSAGE_STRINGS,
						     fname,
						     "does not exist!",
						  0,
						  NOTICE_BUTTON, "Ok", 0,
						  0);
		return GFM_ERROR;
	}
	yyparse();
	propagate();
	fclose(yyin);

	xv_set(categories, PANEL_ITEM_MENU, cat_menu(category_head), 0);
	xv_set(Category_category_base->cat_insert_location,
			 PANEL_ITEM_MENU, cat_menu(category_head),
			 0);
	xv_set(entry_category, PANEL_ITEM_MENU, cat_menu(category_head), 0);

	strcpy(fname, oldname);

	return GFM_OK;
}

/*
 * **********************************************************************
 * Activate the file manager window for a file merge.
 */
Menu_item merge_file(item, op)
Menu_item	item;
Menu_generate	op;
{
	if (op == MENU_NOTIFY) {
		gfm_activate(file_window, NULL, NULL, NULL, finish_file_merge, NULL, GFM_LOAD);
	}
   return item;
}

/*
 * **********************************************************************
 * Complete the saving of the database with a file from the file
 * manager window.  
 */
int finish_file_save(gip, dir, file)
gfm_popup_objects *gip;
char              *dir;
char              *file;
{
	char oldname[256];

	strcpy(oldname, fname);
	
   sprintf(fname, "%s/%s", dir, file);
	refresh_db(FALSE);
	fclose(yyin);

	strcpy(fname, oldname);

	return GFM_OK;
}

/*
 * **********************************************************************
 * Activate the file manager window for a file save.
 */
Menu_item save_file(item, op)
Menu_item	item;
Menu_generate	op;
{
	if (op == MENU_NOTIFY) {
		gfm_activate(file_window, NULL, NULL, NULL, finish_file_save, NULL, GFM_SAVE);
	}
   return item;
}

/*
 * **********************************************************************
 * Complete the saving of a category to a file from the file manager
 * window.
 */
int finish_cat_save(gip, dir, file)
gfm_popup_objects *gip;
char              *dir;
char              *file;
{
	struct category_rec *cr, *parent;
	char filname[256];
	FILE *outfd;

	cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
	parent = cr->parent;
	cr->parent = NULL;
   sprintf(filname, "%s/%s", dir, file);
	outfd = fopen(filname, "w");
	refresh_category(cr, outfd, FALSE);
	fclose(outfd);

	cr->parent = parent;
	changed = FALSE;
}

/*
 * **********************************************************************
 * Activate the file manager window for a category save.
 */
Menu_item save_cat_file(item, op)
Menu_item	item;
Menu_generate	op;
{
	if (op == MENU_NOTIFY) {
		gfm_activate(file_window, NULL, NULL, NULL, finish_cat_save, NULL, GFM_SAVE);
	}
	return item;
}

#endif  /*TDL*/

/*
 * **********************************************************************
 * This routine writes the structures -- non-recurring and recurring, for
 * a category -- to the database file.  Note that we are given the file
 * descriptor for saving...no file stuff is necessary here.  
 */
void refresh_category(cr, outfd, fromquit)
struct category_rec *cr;
FILE *outfd;
int fromquit;
{
	int datecode, month, day, year, units, sterror;
	struct entry_list *el, *todayel;
	struct day_entry *de, *todayde, *tmpde;
	struct recurrence_list *rl;
	struct stat *st;
	char backup[LINESIZ], rlstr[20], tmpstr[20];
	char date[12], unit;

	if (cr->parent == NULL) {
		fprintf(outfd, "category: ''%s''\n", cr->name);
	} else {
		fprintf(outfd, "category: ''%s''\nparent: ''%s''\n", cr->name, cr->parent->name);
	}
		
	/*** Non-recurring entries ***/
	el = cr->entry_head;
	for (el = cr->entry_head; el != NULL; el = el->next) {
		for (de = el->first; de != NULL; de = de->next) {
			if ((! de->checked ) || (EQUAL(on_propagation, "retain"))) {
				year = (int)(de->starting_day_code / 10000);
				month = (int)((de->starting_day_code - year*10000) / 100);
				day = de->starting_day_code % 100;
				
				if (de->checked) {
					year = (int)(el->day_code / 10000);
					month = (int)((el->day_code - year*10000) / 100);
					day = el->day_code % 100;
				
					fprintf(outfd, "%2d/%2d/%2d:%d:''%s''\n",
							  month, day, year+90, 0, de->text);
				} else {
					year = (int)(de->starting_day_code / 10000);
					month = (int)((de->starting_day_code - year*10000) / 100);
					day = de->starting_day_code % 100;
				
					fprintf(outfd, "%2d/%2d/%2d:%d:''%s''\n", month, day, year+90,
							  de->priority, de->text);
				}
				
				/*** Write the deadline line ***/
				if (de->deadline != NULL) {
					if (de->deadline->relative) {
						units = de->deadline->datecode - (de->deadline->datecode / 10) * 10;
						switch (units) {
						case 0: unit = 'd'; break;
						case 1: unit = 'w'; break;
						case 2: unit = 'm'; break;
						case 3: unit = 'y'; break;
						}
						sprintf(date, "%d%c", de->deadline->datecode/10, unit);
					} else {
						sprintf(date, "%d/%d/%d",
								  dc_month(de->deadline->datecode), dc_day(de->deadline->datecode), dc_year(de->deadline->datecode)-1900);
					}
					fprintf(outfd, "deadline:%s:%d:%d %d:%d:%d:\"%s\":\"%s\":%d %d:\n",
							  date,
							  de->deadline->actions,
							  de->deadline->delete_time, de->deadline->delete_units,
							  de->deadline->priority_up_units,
							  de->deadline->priority_down_units,
							  de->deadline->mail_on, de->deadline->mail_after,
							  de->deadline->move_time, de->deadline->move_units);
				}
			} else {
#ifndef TDL
				if (fromquit) log_entry(de,cr);
#endif
			}
		}
	}
	
	/*** Recurring Entries ***/
	for (rl = cr->rl_head; rl != NULL; rl = rl->next) {
		year = (int)(rl->starting_day_code / 10000);
		month = (int)((rl->starting_day_code - year*10000) / 100);
		day = rl->starting_day_code % 100;
		
		rlstr[0] = '\0';
		if (rl->daily) strcat(rlstr, "d");
		if (rl->weekly) strcat(rlstr, "w");
		if (rl->biweekly) strcat(rlstr, "b");
		if (rl->monthly) strcat(rlstr, "m");
		if (rl->yearly) strcat(rlstr, "y");
		if (rl->dow != -1) {
			sprintf(tmpstr, "D%1d", rl->dow);
			strcat(rlstr, tmpstr);
		}
		if (rl->week_number != 0) {
			sprintf(tmpstr, "N%1d", rl->week_number);
			strcat(rlstr, tmpstr);
		}
		if (rl-> number_of_weeks != 0) {
			sprintf(tmpstr, "W%1d", rl->number_of_weeks);
			strcat(rlstr, tmpstr);
		}
		if (rl->number_of_months != 0) {
			sprintf(tmpstr, "O%1d", rl->number_of_months);
			strcat(rlstr, tmpstr);
		}
		if (rl->dom != 0) {
			sprintf(tmpstr, "S%1d", rl->dom);
			strcat(rlstr, tmpstr);
		}
		
		fprintf(outfd, "%2d/%2d/%2d|%s:%d:''%s''\n", month, day, year+90,
				  rlstr, rl->priority, rl->text);
		
		/*** Don't forget that deadline line ***/
		if (rl->deadline != NULL) {
			if (rl->deadline->relative) {
				units = rl->deadline->datecode - (rl->deadline->datecode / 10) * 10;
				switch (units) {
				case 0: unit = 'd'; break;
				case 1: unit = 'w'; break;
				case 2: unit = 'm'; break;
				case 3: unit = 'y'; break;
				}
				sprintf(date, "%d%c", rl->deadline->datecode/10, unit);
			} else {
				sprintf(date, "%d/%d/%d",
						  dc_month(rl->deadline->datecode), dc_day(rl->deadline->datecode), dc_year(rl->deadline->datecode)-1900);
			}
			fprintf(outfd, "deadline:%s:%d:%d %d:%d:%d:\"%s\":\"%s\":%d %d:\n",
					  date,
					  rl->deadline->actions,
					  rl->deadline->delete_time, rl->deadline->delete_units,
					  rl->deadline->priority_up_units,
					  rl->deadline->priority_down_units,
					  rl->deadline->mail_on, rl->deadline->mail_after,
					  rl->deadline->move_time, rl->deadline->move_units);
		}
	}

	if (cr->subcats != NULL) {
		for (cr=cr->subcats; cr != NULL; cr=cr->next)
			refresh_category(cr, outfd, fromquit);
	}
}

/*
 * **********************************************************************
 * This routine writes the structures -- non-recurring and recurring, for
 * all categories -- to the database file.  Note that we have to write a
 * backup file here, and save the correct file permissions.
 */
void refresh_db (fromquit)
int fromquit;
{
	int datecode, month, day, year, units, sterror;
	struct entry_list *el, *todayel;
	struct day_entry *de, *todayde, *tmpde;
	struct recurrence_list *rl;
	struct category_rec *cr;
	FILE *outfd;
	struct stat *st;
	char backup[LINESIZ], rlstr[20], tmpstr[20];
	char date[12], unit;

	/* First, do we need to do this...*/
	if (!changed) return;

#ifndef TDL
	/* Next, what if the thing was written already? */
	if (!checking)
		if (!file_status_ok()) return;
#endif

	/* Backup the file...*/
	sprintf(backup, "%s.BAK", fname);
	rename(fname, backup);
	st = NEW(struct stat);
	sterror = stat(backup, st);
	outfd = fopen(fname, "w");

	cr = category_head;
	while (cr != NULL) {
		refresh_category(cr, outfd, fromquit);
		cr = cr->next;
	}
	fclose(outfd);
	if (! sterror) chmod(fname, st->st_mode);

	sterror = stat(fname, st);
	access_time = st->st_atime;
	modify_time = st->st_mtime;

	changed = FALSE;
}

/*
 * **********************************************************************
 * This routine frees up the memory held by pointers and data structures,
 * for use by file reloading.
 */
void freeup()
{
	struct entry_list *el, *tmpel;
	struct day_entry *de, *tmpde;
	struct recurrence_list *rl, *tmprl;
	struct category_rec *cr, *tmpcr;

	cr = category_head;
	while (cr != NULL) {
		el = cr->entry_head;
		for (el = cr->entry_head; el != NULL;) {
			for (de = el->first; de != NULL;) {
				tmpde = de;
				de = de->next;
				free(tmpde);
			}
			tmpel = el;
			el = el->next;
			free(tmpel);
		}
		for (rl = cr->rl_head; rl != NULL;) {
			tmprl = rl;
			rl = rl->next;
			free(tmprl);
		}
		tmpcr = cr;
		cr = cr->next;
		free(tmpcr);
	}

	category_head = NULL;
	entry_head = entry_tail = NULL;
	rl_head = rl_tail = NULL;
}
