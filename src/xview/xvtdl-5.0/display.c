/*
 * $Id$
 * **********************************************************************
 *
 *  Display.c ==> routines that govern functionality replated to 
 *                displaying the todo list entries.
 *
 *  Here note that the "display list" is really a linked list of items
 *  *to be displayed*.  A separate routine is called to display the 
 *  display list.
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

extern struct entry_list *entry_search();
extern struct day_entry *entry_rl_search();

struct day_entry_list *delhead, *delcurr, *delprev;
int position[10];

/*
 * **********************************************************************
 * This routine clears the todo list display of all entries.  It also 
 * initializes display variables.
 */
clear_display()
{
#ifndef TDL
   int nrows, count;
   
   nrows = xv_get(todo, PANEL_LIST_NROWS);
   xv_set(todo, XV_SHOW, FALSE, NULL);
   for (count = nrows-1; count >= 0; count --)
      xv_set(todo,
             PANEL_LIST_DELETE, count,
             0);
   xv_set(todo, XV_SHOW, TRUE, NULL);
#endif

	delhead = delcurr = delprev = NULL;
}

/*
 * **********************************************************************
 * Add an item to the display list.  Ye 'ol linked list append.
 */
add_to_display_list(de)
struct day_entry *de;
{
	delprev = delcurr;
	delcurr = NEW(struct day_entry_list);
	delcurr->de = de;
	delcurr->next = NULL;
	delcurr->prev = delprev;
	if (delprev != NULL) delprev->next = delcurr;
	if (delhead == NULL) delhead = delcurr;
}

/*
 * **********************************************************************
 * This routine sorts the display list according to the specifications
 * given by the user in the property sheet.  This is a recursive 
 * routine -- each "sortlevel" sorts according to the criteria 
 * specified at that priority level.
 */
sort_display_list(begin_del, end_del, sortlevel)
struct day_entry_list *begin_del, *end_del;
int sortlevel;
{
	struct day_entry_list *del, *tmpdel;
	int changed=TRUE, swap, change;

	/* 
    * We might not have to sort the list at all...
     */
	if (sort_order[sortlevel-1] == '3') return;
	if (begin_del == end_del) return;

	/* 
    * Phase one: sort the list according to the criteria at this 
    * sort level.
    *   NOTE: this is a bubble sort.  easy, but not too efficient.
    */
	while (changed) {
		changed = FALSE;
		for (del=begin_del; del!=end_del; del=del->next){
			switch(sort_order[sortlevel-1]) {
			   case '0':
				   if (EQUAL(priority_listing, "ascending")) {
						swap = (del->de->priority > del->next->de->priority);
					} else {
						swap = (del->de->priority < del->next->de->priority);
					}
					break;

				case '1':
				   if (EQUAL(chron_listing, "ascending")) {
						swap = (del->de->starting_day_code > del->next->de->starting_day_code);
					} else {
						swap = (del->de->starting_day_code < del->next->de->starting_day_code);
					}
					break;

				case '2':
			      swap = (strcmp(del->de->text, del->next->de->text) > 0);
					break;
			}
			if (swap) {
				if (del->next == end_del) end_del = del;
				if (begin_del == del) begin_del = del->next;
				if (del->prev == NULL) {
					delhead = del->next;
				} else {
					del->prev->next = del->next;
				}
				tmpdel = del;
				del = del->next;
				if (del->next != NULL) del->next->prev = tmpdel;
				tmpdel->next = del->next;
				del->next = tmpdel;
				del->prev = tmpdel->prev;
				tmpdel->prev = del;

				changed = TRUE;
			}
		}
	}

   /*
	 * Phase two: 
	 *    if sortlevel < 3, walk through list and sort where items 
	 *    "cross" the criteria boundary (e.g., change priorities).
	 */
	if (sortlevel > 2) return;
	if (sort_order[sortlevel-1] == '3') return;

	del = begin_del;
	for (tmpdel=begin_del->next; tmpdel!=end_del; tmpdel=tmpdel->next) {
		switch(sort_order[sortlevel-1]) {
		   case '0':
			   change = (del->de->priority != tmpdel->de->priority);
				break;
				
			case '1':
				change = (del->de->starting_day_code != tmpdel->de->starting_day_code);
				break;
				
			case '2':
				/* This should neve be encountered */
				break;
		}
		if (change) {
			sort_display_list(del, tmpdel->prev, sortlevel+1);
			del = tmpdel;
		}
	}
}

/*
 * **********************************************************************
 * This routine displays the linked display list.  It is in two versions:
 * the TDL and XVTDL versions.  The first is a textual display; the
 * second involves XView list manipulation.
 */

#ifdef TDL

display_display_list ()
{
	struct day_entry_list *del;
	int pos;
	
	for (del=delhead, pos=0; del != NULL; del = del->next, pos++) {
		if (del->de->checked) {
			printf("  [X] %s\n", del->de->text);
		} else {
			printf("  [%d] %s\n", del->de->priority, del->de->text);
		}
	}
}

#else

display_display_list ()
{
	struct day_entry_list *del;
	int pos;
	
	for (del=delhead, pos=0; del != NULL; del = del->next, pos++) {
		xv_set(todo,
				 PANEL_LIST_STRING, pos, del->de->text,
				 PANEL_LIST_GLYPH,
				    pos,
				    del->de->checked?checked_on:checks[del->de->priority],
				 PANEL_LIST_CLIENT_DATA, pos, del->de,
				 0);
	}
}

#endif

/*
 * **********************************************************************
 * This routine fills the linked display list with the contents of
 * the category given -- both non-recurring and recurring entries.
 */
fill_display_with_category(cat, datecode)
struct category_rec *cat;
int datecode;
{
   int today_datecode, count, nrows, pos, prop;
	struct category_rec *cr;
   struct entry_list *el;
   struct day_entry *de;
   struct recurrence_list *rl;
   struct day_entry *tmprl, *rl2;
   struct entry_list *tmpeh, *tmpet;
   struct recurrence_list *tmprh, *tmprt;
   char txt[80];
#ifndef TDL
   Server_image check;
#endif
   
   /*
	 * First, a little setup...
	 */
   tmpeh = entry_head; entry_head = cat->entry_head;
   tmpet = entry_tail; entry_tail = cat->entry_tail;
   tmprh = rl_head;    rl_head = cat->rl_head;
   tmprt = rl_tail;    rl_tail = cat->rl_tail;

	/*
	 * Now, fill in with non-recurring entries.
	 */
   el = entry_search(datecode, FALSE, NULL);

   if (el != NULL) {
      for (de = el->first; de != NULL; de = de->next) {
			add_to_display_list(de);
		}
   }

   /*
    *  Search the recurrence list for possible candidates to display.
    *  Note a few things:
    *     (1)  A dummy list is created if none exists to speed searching.
    *     (2)  When an entry is found that matches, a day entry is
    *          created for that item.
    */
   el = entry_search(datecode, TRUE, NULL);
   entry_head = cat->entry_head;
   entry_tail = cat->entry_tail;
   rl_head = cat->rl_head;
   rl_tail = cat->rl_tail;
   for (rl = rl_head; rl != NULL; rl = rl->next) {
      if (datecode_matches(datecode, rl)) {
         if (! entry_rl_search(el, rl)) {
            rl2 = el->rl_last;
            tmprl = NEW(struct day_entry);
            tmprl->next = NULL;
            strcpy(tmprl->text, rl->text);
            tmprl->priority = rl->priority;
            tmprl->starting_day_code = rl->starting_day_code;

            if (rl->deadline != NULL) {
               tmprl->deadline = NEW(struct deadline_rec);
               tmprl->deadline->datecode = rl->deadline->datecode;
               tmprl->deadline->relative = rl->deadline->relative;
               tmprl->deadline->actions = rl->deadline->actions;
               tmprl->deadline->delete_time = rl->deadline->delete_time;
               tmprl->deadline->delete_units = rl->deadline->delete_units;
               tmprl->deadline->priority_up_units =
                  rl->deadline->priority_up_units;
               tmprl->deadline->priority_down_units =
                  rl->deadline->priority_down_units;
               strcpy(tmprl->deadline->mail_on, rl->deadline->mail_on);
               strcpy(tmprl->deadline->mail_after, rl->deadline->mail_after);
               tmprl->deadline->move_time = rl->deadline->move_time;
               tmprl->deadline->move_units = rl->deadline->move_units;
            } else {
               tmprl->deadline = NULL;
            }

            tmprl->checked = FALSE;
            if (rl2 == NULL) {
               tmprl->prev = NULL;
               el->rl_first = el->rl_last = tmprl;
            } else {
               tmprl->prev = rl2;
               rl2->next = el->rl_last = tmprl;
            }
#ifndef TDL
            check = checks[tmprl->priority];
#endif
         } else {
            tmprl = entry_rl_search(el, rl);
#ifndef TDL
            check = tmprl->checked?checked_on:checks[tmprl->priority];
#endif
         } 
         tmprl->recurring_entry = TRUE;

			add_to_display_list(tmprl);
      }
   }

#ifndef TDL
	cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
   if (cr == cat) {
		entry_head = cr->entry_head;
		entry_tail = cr->entry_tail;
		rl_head = cr->rl_head;
		rl_tail = cr->rl_tail;
	} else {		
		entry_head = tmpeh;
		entry_tail = tmpet;
		rl_head = tmprh;
		rl_tail = tmprt;
	}
#else
   entry_head = tmpeh;
   entry_tail = tmpet;
   rl_head = tmprh;
   rl_tail = tmprt;
#endif
}

#ifndef TDL

/*
 * **********************************************************************
 * Given a month, day, and year spec, this routine drives the todo list
 * display for a specific category.  This includes recurring and
 * non-recurring entries.  The list is assembled, then sorted, then
 * displayed.
 */
display_list (month, day, year)
int month, day, year;
{
   int datecode, count;
   struct category_rec *cr;

   /*
    *  Clear the position tally for sorting the display.  Then clear
    *  the display.
    */
   for (count=0; count<10; count++) position[count] = -1;
   clear_display();

   /*
    *  Compute the correct datecode.
    */
   datecode = (year-1990)*10000 + month*100 + day;

   /*
    *  Find the correct category.
    */
   cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);

   /*
    * If we have one, display its entries....
    */
   xv_set(todo, XV_SHOW, FALSE, NULL);
   fill_display_with_category(cr, datecode);
	sort_display_list(delhead, delcurr, 1);
	display_display_list();
   xv_set(todo, XV_SHOW, TRUE, NULL);

   xv_set(list_menu, MENU_DEFAULT, 2, 0);
   xv_set(list_all, PANEL_LABEL_STRING, "List All", 0);
}

/*
 * ********************************************************************
 * This routine drives the todo list display for a category tree -- the
 * current category becomes the root of the tree.  This includes
 * recurring and non-recurring entries.  The list is assembled, then
 * sorted, then displayed.
 */
Menu_item display_tree(item, op)
Menu_item   item;
Menu_generate  op;
{
   int datecode, count;
   struct category_rec *cr;

   if (op == MENU_NOTIFY) {

      /*
       *  Clear the position tally for sorting the display, and the 
       *  display itself.
       */
      for (count=0; count<10; count++) position[count] = -1;
      clear_display();
      
      /*
       *  compute the correct datecode and find the entry list for that
       *  date.
       */
      datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
      
      /*
       *  Find the correct category.
       */
      cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);

		/*
       * Generate and display...
       */
      xv_set(todo, XV_SHOW, FALSE, NULL);
		fill_display_with_category(cr, datecode);
		if (cr->subcats != NULL) {
			for (cr=cr->subcats; cr != NULL; cr=cr->next)
				fill_display_with_category(cr, datecode);
		}
		sort_display_list(delhead, delcurr, 1);
		display_display_list();
      xv_set(todo, XV_SHOW, TRUE, NULL);

		/*
       * Set the list button correctly
       */
      xv_set(list_menu, MENU_DEFAULT, 5, 0);
      xv_set(list_all, PANEL_LABEL_STRING, "List Cat", 0);
      xv_set(print_print_base->categories,
             PANEL_CLIENT_DATA, PRINT_TREE,
             PANEL_INACTIVE, TRUE,
             0);
      }

   return(item);
}

/*
 * ********************************************************************
 * This routine drives the todo list display for a category tree -- the
 * PARENT of the current category becomes the root of the tree.  This
 * includes recurring and non-recurring entries.  The list is assembled,
 * then sorted, then displayed.
 */
Menu_item display_parent(item, op)
Menu_item   item;
Menu_generate  op;
{
   int datecode, count;
   struct category_rec *cr;

   if (op == MENU_NOTIFY) {

      /*
       *  Find the correct category.
       */
      cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
      if (cr->parent == NULL) {
         notice_prompt(tdlist, NULL,
                       NOTICE_MESSAGE_STRINGS,
                       "This category has no parent category.",
                       0,
                       NOTICE_BUTTON, "Ok", 1,
                       0);
      } else {
         /*
          *  Clear the position tally for sorting the display, and the 
          *  display itself.
          */
         for (count=0; count<10; count++) position[count] = -1;
         clear_display();
         
         /*
          *  compute the correct datecode and find the entry list for that
          *  date.
          */
         datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
         
			/*
			 * Generate and display...
			 */
         xv_set(todo, XV_SHOW, FALSE, NULL);
         fill_display_with_category(cr->parent, datecode);
			sort_display_list(delhead, delcurr, 1);
			display_display_list();
         xv_set(todo, XV_SHOW, TRUE, NULL);

			/*
			 * Set the list button correctly
			 */
         xv_set(list_menu, MENU_DEFAULT, 2, 0);
         xv_set(list_all, PANEL_LABEL_STRING, "List All", 0);
      }
   }

   return(item);
}

/*
 * **********************************************************************
 * This routine displays the todo list for all items, including recurring
 * and non-recurring entries. 
 */
Menu_item display_all(item, op)
Menu_item   item;
Menu_generate  op;
{
   int datecode, count;
   struct category_rec *cr, *tcr;

   if (op == MENU_NOTIFY) {

      /*
       *  Clear the position tally for sorting the display, and the 
       *  display itself.
       */
      for (count=0; count<10; count++) position[count] = -1;
      clear_display();
      
      /*
       *  compute the correct datecode and find the entry list for that
       *  date.
       */
      datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
      
		/*
		 * Generate and display...
		 */
      xv_set(todo, XV_SHOW, FALSE, NULL);
      cr = category_head;
      while (cr != NULL) {
			fill_display_with_category(cr, datecode);
			if (cr->subcats != NULL) {
				for (tcr=cr->subcats; tcr != NULL; tcr=tcr->next)
					fill_display_with_category(tcr, datecode);
			}
         cr = cr->next;
      }
		sort_display_list(delhead, delcurr, 1);
		display_display_list();
      xv_set(todo, XV_SHOW, TRUE, NULL);

		/*
		 * Set the list button correctly
		 */
      xv_set(list_menu, MENU_DEFAULT, 5, 0);
      xv_set(list_all, PANEL_LABEL_STRING, "List Cat", 0);
      xv_set(print_print_base->categories,
             PANEL_CLIENT_DATA, PRINT_ALL,
             PANEL_INACTIVE, TRUE,
             0);
   }
   return(item);

}

/*
 * **********************************************************************
 * This routine displays the todo list for a specific category,
 * including recurring and non-recurring entries. This calls
 * "display_list", but also changes the label on the list button.
 */
Menu_item display_category(item, op)
Menu_item   item;
Menu_generate  op;
{
   if (op == MENU_NOTIFY) {
      display_list(curr_month, curr_day, curr_year);
      xv_set(list_menu, MENU_DEFAULT, 2, 0);
      xv_set(list_all, PANEL_LABEL_STRING, "List All", 0);
      xv_set(print_print_base->categories,
             PANEL_INACTIVE, FALSE,
             0);
   }
   return(item);
}

#endif
