/*
 * $Id: print.c,v 4.1 1992/10/06 11:33:13 jipping Exp $
 * *********************************************************************
 * Print.c --> Routines for printing todo lists.
 *
 * The "print" window was constructed using Sun's GUIDE.  These routines
 * reference window portions through pointer interfaces established by
 * GUIDE code.  The actual code to construct the interface can be found 
 * in "print_ui.c" and "print_ui.h".
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
 * $Log: print.c,v $
 * Revision 4.1  1992/10/06  11:33:13  jipping
 * Fixes after beta test: The "&" in the passwd gecos field is now replaced
 * with users' login name.
 *
 * Revision 4.0  1992/09/15  11:37:31  jipping
 * Release 4.0 beta:
 *    * added the ability for multiple pages in both regular and PostScript
 *      modes
 *    * added a priority indication for both modes
 *    * added #ifdefs for "tdl"
 *
 * Revision 3.1  1992/07/30  14:11:16  jipping
 * Added the use of the PRINTER environment variable.
 *
 * Revision 3.0  1992/07/27  18:42:35  jipping
 * Release 3.0 includes:
 * * added a "print_all" routine to print all items in "List All" list
 * * fixed hard coded temp file name with call to "tempnam"
 * * fixed some pinning errors
 * * changed from userid to gecos on output print
 * `.
 *
 * Revision 2.2  1992/07/16  13:38:49  jipping
 * COmplete rewrote the printing methods to accomodate printing
 * all categories; see print_category and print_list.
 *
 * Revision 2.1  1992/07/15  17:25:47  jipping
 * Implemented the feature of printing all categories.
 * (1) added the print_category routine
 * (2) made print_list call print_category as requested.
 *
 * Revision 2.0  1992/07/06  12:42:08  jipping
 * Initial release.
 *
 */

#include "globaldefs.h"

extern void add_to_display_list(), sort_display_list();

int pageno;

#ifndef TDL

int incl_checked;
Attr_attribute	INSTANCE;
print_print_base_objects	*print_print_base;

/*
 * **********************************************************************
 * Initialize the print window...by calling the routine set up by GUIDE.
 */

void initialize_print ()
{
	print_print_base = print_print_base_objects_initialize(NULL, tdlist);

	xv_set(print_print_base->print_or_file, 
			 PANEL_VALUE, EQUAL(default_print_dest, "printer")?0:1,
			 0);
	if (EQUAL(default_print_dest, "printer")) {
		xv_set(print_print_base->filename, XV_SHOW, FALSE, 0);
		xv_set(print_print_base->printer, XV_SHOW, TRUE, 0);
	} else {
		xv_set(print_print_base->printer, XV_SHOW, FALSE, 0);
		xv_set(print_print_base->filename, XV_SHOW, TRUE, 0);
	}
	xv_set(print_print_base->scale,
			 PANEL_VALUE, 100,
			 PANEL_INACTIVE, TRUE,
			 0);
	xv_set(print_print_base->incl_checked_items, PANEL_VALUE, 1, 0);
	if ((int)strlen(default_printer) > 0) {
		xv_set(print_print_base->printer, PANEL_VALUE, default_printer, 0);
	} else {
		xv_set(print_print_base->printer, PANEL_VALUE, getenv("PRINTER"), 0);
	}
	if ((int)strlen(print_file) > 0)
		xv_set(print_print_base->filename, PANEL_VALUE, print_file, 0);
	if (postscriptmode) {
		xv_set(print_print_base->postscript, PANEL_VALUE, 1, 0);
		xv_set(print_print_base->scale, PANEL_INACTIVE, FALSE, 0);
	}
	xv_set(print_print_base->filename, 
			 XV_SHOW, EQUAL(default_print_dest, "printer")?0:1,
			 0);
	
	incl_checked = 1;
}

/*
 * **********************************************************************
 * Notify callback function for the "cancel" button.
 */

void print_cancel(item, event)
Panel_item	item;
Event		*event;
{
	xv_set(print_print_base->categories, PANEL_INACTIVE, FALSE, 0);
	xv_set(print_print_base->print_base,
			 XV_SHOW, FALSE,
			 0);
}

#endif

/*
 * **********************************************************************
 * This routine fills the "print list" with the contents of a category.
 * A "print list" is a linked list (much like a display list) of items to
 * be printed.  Here we go through non-recurring and recurring items and
 * add them to the print list.
 */
void fill_print_list_with_category(cr, do_subcats)
struct category_rec *cr;
int do_subcats;
{
	struct entry_list *el;
	struct day_entry *de;
	struct recurrence_list *rl;
	struct day_entry *tmprl, *rl2;
	int today_datecode, datecode;

	/*
	 * Setup pointers and such
	 */
	datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
	today_datecode =
		(today.tm_year-90)*10000 + (today.tm_mon+1)*100 + today.tm_mday;
	entry_head = cr->entry_head;
	entry_tail = cr->entry_tail;
	rl_head = cr->rl_head;
	rl_tail = cr->rl_tail;
	
	/*
	 * If we have one, get its entries....
	 */
	el = (struct entry_list *)entry_search(datecode, FALSE, NULL);
	if (el != NULL) {
		for (de = el->first; de != NULL; de = de->next) {
			add_to_display_list(de);
		}
	}
	
	/*
	 *  Search the recurrence list for possible candidates to print
	 */
	el = (struct entry_list *)entry_search(datecode, TRUE, NULL);
	for (rl = rl_head; rl != NULL; rl = rl->next) {
		if (datecode_matches(datecode, rl)) {
         if (! entry_rl_search(el, rl)) {
				rl2 = el->rl_last;
				tmprl = NEW(struct day_entry);
				tmprl->next = NULL;
				strcpy(tmprl->text, rl->text);
				tmprl->priority = rl->priority;
				tmprl->starting_day_code = rl->starting_day_code;

            tmprl->checked = FALSE;
            if (rl2 == NULL) {
               tmprl->prev = NULL;
               el->rl_first = el->rl_last = tmprl;
            } else {
               tmprl->prev = rl2;
               rl2->next = el->rl_last = tmprl;
            }
         } else {
            tmprl = (struct day_entry *)entry_rl_search(el, rl);
         } 
         tmprl->recurring_entry = TRUE;
			add_to_display_list(tmprl);
		}
	}
	
	if (do_subcats) {
		if (cr->subcats != NULL) {
			for (cr=cr->subcats; cr != NULL; cr=cr->next)
				fill_print_list_with_category(cr,do_subcats);
		}
	}
}

/*
 * **********************************************************************
 * Once we have a print list, this routine will print it out.  It walks
 * through the print list, managing pages and such.
 */
void print_print_list(tmp,ps,incl_checked,scale_factor,username,catname,title)
FILE *tmp;
int ps,incl_checked;
float scale_factor;
char *username, *catname, *title;
{
	int c, entry;
	struct entry_list *el;
	struct day_entry *de;
	struct recurrence_list *rl;
	struct day_entry *tmprl, *rl2;
	struct day_entry_list *tdel, *del;

	if (ps) {
		fprintf(tmp, "%%%%Page: %d\n%d 612 792 0 FMBEGINPAGE\n", pageno, pageno++);
      fprintf(tmp, "72 746 540 756 R\n7 X\n0 K\nV\n72 32.67 540 42.67 R\n");
      fprintf(tmp, "V\n99 72 540 720 R\nV\n0 F\n0 X\n(T) 99 708 T\n(o Do List) 110.33 708 T\n1 F\n");
		fprintf(tmp, "(for %s) 99 687 T\n", username);
		fprintf(tmp, "(Category: %s) 99 668 T\n", catname);
		fprintf(tmp, "(Date: %s) 99 649 T\n", title);
		copyfile2(PRINT_PROLOG2, tmp);
	} else {
		fprintf(tmp, "*** TO DO LIST ***\n");
		fprintf(tmp, "      for %s\n", username);
		fprintf(tmp, "      Category: %s\n", catname);
		fprintf(tmp, "      Date: %s\n\n\n", title);
	}

	entry = 0;
	tdel = NULL;

	for (del=delhead; del != NULL; tdel=del,del=del->next) {
		if (tdel != NULL) free(tdel);
		if (! del->de->checked) {
			if (ps) {
				fprintf(tmp, "(");
				for (c=0; c < (int)strlen(del->de->text); c++)
					if ( (del->de->text[c] != '(') & (del->de->text[c] != ')')) {
						putc(del->de->text[c], tmp);
					} else {
						putc('\\',tmp);
						putc(del->de->text[c], tmp);
					}
				fprintf(tmp, ") %d %d TODOITEM\n", del->de->priority, entry++);
			} else {
				fprintf(tmp, "  [ ] (%d) %s\n\n", del->de->priority, del->de->text);
				entry++;
			}
		} else {
			if (incl_checked == 1) {
				if (ps) {
					fprintf(tmp, "(");
					for (c=0; c < (int)strlen(del->de->text); c++)
						if ( (del->de->text[c] != '(') & (del->de->text[c] != ')')) {
							putc(del->de->text[c], tmp);
						} else {
							putc('\\',tmp);
							putc(del->de->text[c], tmp);
						}
					fprintf(tmp, ") %d TODOCHECKEDITEM\n", entry++);
				} else {
					fprintf(tmp, "  [X] %s\n\n", del->de->text);
					entry++;
				}
			}
		}
		if ((entry == 28) && (del->de->next != NULL)) {
			if (ps) {
				fprintf(tmp, "FMENDPAGE\n%%%%EndPage:\n");		
				fprintf(tmp, "%%%%Page:%d \n%d 612 792 0 FMBEGINPAGE\n", pageno, pageno++);
				fprintf(tmp, "72 746 540 756 R\n7 X\n0 K\nV\n72 32.67 540 42.67 R\n");
				fprintf(tmp, "V\n99 72 540 720 R\nV\n0 F\n0 X\n(T) 99 708 T\n(o Do List) 110.33 708 T\n1 F\n");
				fprintf(tmp, "(for %s) 99 687 T\n", username);
				fprintf(tmp, "(Category: %s) 99 668 T\n", catname);
				fprintf(tmp, "(Date: %s) 99 649 T\n", title);
				copyfile2(PRINT_PROLOG2, tmp);
			} else {
				fprintf(tmp, "\f");
			}
			entry = 0;
		}
	}

	if (ps) {
		fprintf(tmp, "FMENDPAGE\n%%%%EndPage:\n");		
	} else {
		fprintf(tmp, "\f");
	}
}

/*
 * **********************************************************************
 * This routine fills the print list for a category. If necessary, that
 * is, if specified by print_tree=TRUE, we recurse to children
 * categories. 
 *
 */
print_category (cr,tmp,ps,incl_checked,scale_factor,name,title,print_tree)
struct category_rec *cr;
FILE *tmp;
int ps,incl_checked;
float scale_factor;
char *name, *title;
int print_tree;
{
	fill_print_list_with_category(cr, FALSE);
	sort_display_list(delhead, delcurr, 1);
	print_print_list(tmp,ps,incl_checked,scale_factor,name,cr->name,title);

	if (print_tree) {
		if (cr->subcats != NULL) {
			for (cr=cr->subcats; cr != NULL; cr=cr->next) {
				pageno = 0;
				delhead = delcurr = delprev = NULL;
				print_category (cr,tmp,ps,incl_checked,scale_factor,name,title,print_tree);
			}
		}
	}
}

/*
 * **********************************************************************
 * This routine fills the print list for all categories. If necessary,
 * is, if specified by print_tree=TRUE, we print a different header.
 */
void print_all (tmp,ps,incl_checked,scale_factor,name,title,print_tree)
FILE *tmp;
int ps,incl_checked;
float scale_factor;
char *name, *title;
int print_tree;
{
	int today_datecode, datecode, count, nrows, entry;
	int c;
	struct entry_list *el;
	struct day_entry *de;
	struct recurrence_list *rl;
	struct day_entry *tmprl, *rl2;
	struct category_rec *cr;
	Server_image check;
	char txt[80];
	
#ifndef TDL
	if (print_tree) {
		cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
		fill_print_list_with_category(cr, TRUE);
	} else {
		cr = category_head;
		while (cr != NULL) {
			fill_print_list_with_category(cr,TRUE);
			cr = cr->next;
		}
	}

	cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
	entry_head = cr->entry_head;
	entry_tail = cr->entry_tail;
	rl_head = cr->rl_head;
	rl_tail = cr->rl_tail;
#endif
		
	/* 
	 *  And, finally, contruct the list.
	 */
	sort_display_list(delhead, delcurr, 1);
	if (print_tree) {
		sprintf(txt, "Tree for \"%s\"", cr->name);
	} else {
		strcpy(txt, "ALL CATEGORIES\n");
	}
	print_print_list(tmp,ps,incl_checked,scale_factor,name,txt,title);
	
}

#ifndef TDL

/*
 * **********************************************************************
 * Notify callback function for "Done" button --> actually DO the print,
 * depending on the setting of the items.
 *
 * This is a driver program.  It determines the correct type of printing
 * to do, and does it by calling the correct routines.
 *
 * Note that the current version depends on two prolog files for printing
 * in PostScript.  These must be set up in the "globaldefs.h" file, and 
 * are denoted by the PROLOG1 and PROLOG2 macros.
 */

void print_list(item, event)
Panel_item item; 
Event      *event;
{
	print_print_base_objects
		*ip = (print_print_base_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *temp_file, new_file[80];
	FILE *tmp;
	int ps;
	float scale_factor;
	struct category_rec *cr;
	char day_title[50], personname[80];
	char printercmd[120];
	char *amp, *gecos, *comma;
	struct tm *now;
   struct timeval tv;
	struct passwd *pwd;

	/* 
    *  Open the temp file.
    */
	if (xv_get(print_print_base->print_or_file, PANEL_VALUE) == 0) {
		if ( (temp_file = (char *)tempnam(NULL, "xvtdl")) == NULL) {
			fprintf(stderr, "Unable to create temporary file name\n");
			return;
		}
	} else {
		temp_file = (char *)malloc(sizeof(char)*256);
		strcpy(temp_file, (char *)xv_get(print_print_base->filename, PANEL_VALUE));
	}
	if ( (tmp = fopen(temp_file, "w")) == NULL) {
		fprintf(stderr, "Unable to open file %s\n", temp_file);
		free(temp_file);
		return;
	}

   /*
    * Get set up.  Set up the time variables, and set up the file 
    * header.
    */
   now = localtime(&tv.tv_sec);
	now->tm_mon = curr_month - 1;
	now->tm_mday = curr_day;
	now->tm_year = curr_year-1900;
	now->tm_wday = zeller(curr_month, curr_day, curr_year);
	strftime(day_title, 50, "%A, %B %e, %Y", now);

	ps = (xv_get(ip->postscript, PANEL_VALUE) == 1);
	scale_factor = (float)xv_get(ip->scale, PANEL_VALUE);
	pwd = getpwuid(getuid());
	if (pwd->pw_name[0] > 96) pwd->pw_name[0] -= 32;

	personname[0] = '\0';
	gecos = malloc(80*sizeof(char));
	strcpy(gecos, pwd->pw_gecos);
	comma = strchr(gecos, ',');
	if (comma != NULL) *comma = '\0';
	amp = strchr(gecos, '&');
	if (amp == NULL) {
		strcpy(personname, gecos);
	} else {
		while (amp != NULL) {
			*amp = '\0';
			strcat(personname, gecos);
			strcat(personname, pwd->pw_name);
			gecos = ++amp;
			amp = strchr(gecos, '&');
		}
	}

	if (ps) {
		copyfile2(PRINT_PROLOG1, tmp);
		fprintf(tmp, "%f %f 612 792 0 1 3 FMDOCUMENT\n", scale_factor/100.0, scale_factor/100.0);
		fprintf(tmp, "/fillprocs 32 array def\n");
		fprintf(tmp, "fillprocs 0 { 0.000000 grayness } put\n");
		fprintf(tmp, "fillprocs 1 { 0.100000 grayness } put\n");
		fprintf(tmp, "fillprocs 2 { 0.300000 grayness } put\n");
		fprintf(tmp, "fillprocs 3 { 0.500000 grayness } put\n");
		fprintf(tmp, "fillprocs 4 { 0.700000 grayness } put\n");
		fprintf(tmp, "fillprocs 5 { 0.900000 grayness } put\n");
		fprintf(tmp, "fillprocs 6 { 0.970000 grayness } put\n");
		fprintf(tmp, "fillprocs 7 { 1.000000 grayness } put\n");
		fprintf(tmp, "fillprocs 8 {<0f1e3c78f0e1c387> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 9 {<0f87c3e1f0783c1e> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 10 {<cccccccccccccccc> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 11 {<ffff0000ffff0000> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 12 {<8142241818244281> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 13 {<03060c183060c081> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 14 {<8040201008040201> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 15 {} put\n");
		fprintf(tmp, "fillprocs 16 { 1.000000 grayness } put\n");
		fprintf(tmp, "fillprocs 17 { 0.900000 grayness } put\n");
		fprintf(tmp, "fillprocs 18 { 0.700000 grayness } put\n");
		fprintf(tmp, "fillprocs 19 { 0.500000 grayness } put\n");
		fprintf(tmp, "fillprocs 20 { 0.300000 grayness } put\n");
		fprintf(tmp, "fillprocs 21 { 0.100000 grayness } put\n");
		fprintf(tmp, "fillprocs 22 { 0.030000 grayness } put\n");
		fprintf(tmp, "fillprocs 23 { 0.000000 grayness } put\n");
		fprintf(tmp, "fillprocs 24 {<f0e1c3870f1e3c78> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 25 {<f0783c1e0f87c3e1> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 26 {<3333333333333333> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 27 {<0000ffff0000ffff> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 28 {<7ebddbe7e7dbbd7e> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 29 {<fcf9f3e7cf9f3f7e> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 30 {<7fbfdfeff7fbfdfe> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 31 {} put\n");
		fprintf(tmp, "%%%%EndSetup\n");
		fprintf(tmp, "0 18 /NewCenturySchlbk-Bold FMDEFINEFONT\n");
		fprintf(tmp, "1 14 /NewCenturySchlbk-Italic FMDEFINEFONT\n");
		fprintf(tmp, "2 9 /NewCenturySchlbk-Roman FMDEFINEFONT\n");
		fprintf(tmp, "%%%%BeginPaperSize: Letter\n");
		fprintf(tmp, "%%%%EndPaperSize\n");
	}

   /*
    *  Setup...clear the print list.
    */
	pageno = 0;
	delhead = delcurr = delprev = NULL;
	
   /*
	 * Now determine the TYPE of printing to do.  Then call the right
	 * routine. 
	 */
	if (xv_get(ip->categories, PANEL_INACTIVE) == TRUE) {
		if (xv_get(ip->categories, PANEL_CLIENT_DATA) == PRINT_ALL) {
			print_all(tmp,ps,incl_checked,scale_factor,personname,day_title,FALSE);
		} else {
			print_all(tmp,ps,incl_checked,scale_factor,personname,day_title,TRUE);
		}
	} else if (xv_get(ip->categories, PANEL_VALUE) == 0) {
		cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
		print_category(cr,tmp,ps,incl_checked,scale_factor,personname,day_title,FALSE);
	} else if (xv_get(ip->categories, PANEL_VALUE) == 1) {
		cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
		print_category(cr,tmp,ps,incl_checked,scale_factor,personname,day_title,TRUE);
		entry_head = cr->entry_head;
		entry_tail = cr->entry_tail;
		rl_head = cr->rl_head;
		rl_tail = cr->rl_tail;
	} else {
		cr = category_head;
		while (cr != NULL) {
			print_category(cr,tmp,ps,incl_checked,scale_factor,personname,day_title,TRUE);
			cr = cr->next;
		}
		cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
		entry_head = cr->entry_head;
		entry_tail = cr->entry_tail;
		rl_head = cr->rl_head;
		rl_tail = cr->rl_tail;
	} 

	/*
    *  And we're done.
    */
	fclose(tmp);

	if (xv_get(print_print_base->print_or_file, PANEL_VALUE) == 0) {
#ifdef SVR4		
		sprintf(printercmd, "lp -d %s %s",
				  (char *)xv_get(ip->printer, PANEL_VALUE), temp_file);
#else
		sprintf(printercmd, "lpr -P%s %s",
				  (char *)xv_get(ip->printer, PANEL_VALUE), temp_file);
#endif
		system(printercmd);
		
		unlink(temp_file);
	} 
	free(temp_file);

	xv_set(print_print_base->categories, PANEL_INACTIVE, FALSE, 0);
	xv_set(print_print_base->print_base,
			 XV_SHOW, FALSE,
			 0);
}

/*
 * **********************************************************************
 * Callback function for the "Print..." button on the ToDo List window.
 */

void start_print(item, event)
	Panel_item	item;
	Event		*event;
{
	if (strlen((char *)xv_get(print_print_base->printer, PANEL_VALUE)) == 0)
		xv_set(print_print_base->printer, PANEL_VALUE, "lp", 0);
	xv_set(print_print_base->print_base,
			 XV_SHOW, TRUE,
			 0);
}

/*
 * **********************************************************************
 * Notify callback function for the printer type item.
 */
void
change_printer_type(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	print_print_base_objects
		*ip = (print_print_base_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	if (value == 0) {
		xv_set(ip->scale, PANEL_INACTIVE, TRUE, 0);
	} else {
		xv_set(ip->scale, PANEL_INACTIVE, FALSE, 0);
	}
		
}

/*
 * **********************************************************************
 * Notify callback function for the "Include items" widget.  Makes a 
 * checkbox widget into an "exclusive" checkbox widget ('cause it looks
 * nice).
 */
void change_checked(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	print_print_base_objects
		*ip = (print_print_base_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	if (value != incl_checked) {
		incl_checked = incl_checked==1?2:1;
		xv_set(ip->incl_checked_items, PANEL_VALUE, incl_checked, 0);
	}
}

/*
 * **********************************************************************
 * Notify callback function for the printer type item.
 */
void
change_destination(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	print_print_base_objects
		*ip = (print_print_base_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	if (value == 0) {
		xv_set(ip->filename, XV_SHOW, FALSE, 0);
		xv_set(ip->printer, XV_SHOW, TRUE, 0);
	} else {
		xv_set(ip->printer, XV_SHOW, FALSE, 0);
		xv_set(ip->filename, XV_SHOW, TRUE, 0);
	}
}
#endif
