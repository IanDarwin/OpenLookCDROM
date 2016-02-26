/*
 * $Id$
 * 
 *  Category.c ==> routines to implement category manipulation.  
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
#include "category_ui.h"

extern void choose_new_category();
extern void finish_category_edit();
extern void switch_insert_modes();

#ifndef TDL
category_category_base_objects   *Category_category_base;
#endif

/*
 * **********************************************************************
 * Given the name of a category, this routine creates a new category -- 
 * menu item, data structure, and all.
 */
struct category_rec *new_category(name, aftercr, subcat)
char *name;
struct category_rec *aftercr;
int subcat;
{
   struct category_rec *cr, *tmpcr;
   int num_cats;

   tmpcr = NEW(struct category_rec);
   if (aftercr == NULL) {
      tmpcr->next = category_head;
      category_head = tmpcr;
      tmpcr->parent = NULL;
   } else {
      if (subcat) {
         tmpcr->next = aftercr->subcats;
         aftercr->subcats = tmpcr;
         tmpcr->parent = aftercr;
      } else {
         tmpcr->next = aftercr->next;
         aftercr->next = tmpcr;
         tmpcr->parent = aftercr->parent;
      }
   }
   strcpy(tmpcr->name, name);
   tmpcr->entry_head = tmpcr->entry_tail = (struct entry_list *) NULL;
   tmpcr->rl_head = tmpcr->rl_tail = (struct recurrence_list *) NULL;
   tmpcr->subcats = NULL;

   return(tmpcr);
}

/*
 * **********************************************************************
 * Starting at "startcr", this routine searches for a category named
 * "name", returning a pointer to the structure.  This routine is 
 * recursive, since it must search the category tree structure.
 */
struct category_rec *cat_search(startcr, name)
struct category_rec *startcr;
char *name;
{
   struct category_rec *cr, *tmpcr=NULL;

   cr = startcr;
   while (cr != NULL) {
      if (EQUAL(cr->name, name)) break;
      if (cr->subcats != NULL) {
         tmpcr = cat_search(cr->subcats, name);
         if (tmpcr != NULL) {cr = tmpcr; break;}
      }
      cr = cr->next;
   }
   return cr;
}

#ifndef TDL

/*
 * **********************************************************************
 * This routine contructs a menu of categories.  It builds pullrights
 * for subcategories.
 */
Menu cat_menu(cat)
struct category_rec *cat;
{
   struct category_rec *cr;
   Menu m, m2;
   Menu_item mi;

   m = xv_create(XV_NULL, MENU, 0);

   for (cr=cat; cr != NULL; cr = cr->next) {
      mi = xv_create(XV_NULL, MENUITEM,
                     MENU_STRING, cr->name,
                     MENU_NOTIFY_PROC, choose_new_category,
                     MENU_CLIENT_DATA, cr,
                     0);
      if (cr->subcats != NULL) {
         m2 = cat_menu(cr->subcats);
         xv_set(mi, MENU_PULLRIGHT, m2, 0);
      }
      xv_set(m, MENU_APPEND_ITEM, mi, 0);
   }

   return m;
}

/*
 * **********************************************************************
 * This initializes the category edit window.  Called from main.
 */
void initialize_category_editor()
{
   Category_category_base =
      category_category_base_objects_initialize(NULL, tdlist);
   xv_set(Category_category_base->cat_insert_pos,
          PANEL_NOTIFY_PROC, switch_insert_modes, 0);
   
   xv_set(Category_category_base->cat_insert_location,
          PANEL_ITEM_MENU, cat_menu(category_head),
          PANEL_CLIENT_DATA, category_head,
          0);
   xv_set(Category_category_base->cat_insert_location_name,
          PANEL_LABEL_STRING, category_head->name,
          0);
}

/*
 * **********************************************************************
 * This opens the category editor in response to the menu selection.
 */
Menu_item open_category_editor(item, op)
Menu_item   item;
Menu_generate  op;
{
   int choice, choices;
   struct category_rec *cr;

   if (op == MENU_NOTIFY) {
      xv_set(Category_category_base->cat_insert_pos, PANEL_VALUE, 0, 0);
      xv_set(Category_category_base->cat_name, PANEL_VALUE, "", 0);
      xv_set(Category_category_base->category_base, XV_SHOW, TRUE, 0);
   }
	return(item);
}

/*
 * **********************************************************************
 * Called in response to the "Done..." button on the category editor.
 * Note that this depends heavily on PANEL_CLIENT_DATA attached to various
 * panel elements.
 */
void insert_category(item, event)
Panel_item item; 
Event      *event;
{
   char text[LINESIZ];
   struct category_rec *cr, *insertcr=NULL, *checkcr=NULL;
   int pos, index, subcat, after;

   /* 
    * Collect data from the panel elements
    */
   strcpy(text, (char *)xv_get(Category_category_base->cat_name, PANEL_VALUE));
   insertcr = (struct category_rec *)xv_get(Category_category_base->cat_insert_location, PANEL_CLIENT_DATA);
   after = xv_get(Category_category_base->cat_insert_pos, PANEL_VALUE) == 0;
   subcat = xv_get(Category_category_base->cat_insert_pos, PANEL_VALUE) == 1;
   
   /*
    * If the category is not a duplicate, then insert it into the 
    * category tree appropriately.
    */
   checkcr = cat_search(category_head, text);
   if (checkcr == NULL) {
      if (subcat) {
         new_category(text, insertcr, TRUE);
      } else {
         new_category(text, insertcr, FALSE);
      }
      xv_set(categories, PANEL_ITEM_MENU, cat_menu(category_head), 0);
      xv_set(entry_category, PANEL_ITEM_MENU, cat_menu(category_head), 0);
      xv_set(Category_category_base->cat_insert_location,
             PANEL_ITEM_MENU, cat_menu(category_head),
             0);
		changed = TRUE;
      refresh_db(FALSE);
   } else {
      notice_prompt(entry_frame, NULL,
                    NOTICE_MESSAGE_STRINGS,
                    "There is already a category",
                    "with this name...Please respecify.",
                    0,
                    NOTICE_BUTTON, "Ok", 1,
                    0);
      xv_set(Category_category_base->cat_name, PANEL_VALUE, "", 0);
      return;
   }
}

/*
 * **********************************************************************
 * Switch categories here!  We need to look up which category was
 * chosen, then find it.  Then we simply display the new category.
 * Note that there are three fields (current list, entry editor, and
 * category editor) to update.  
 */
void choose_new_category (menu, menu_item)
Menu menu;
Menu_item menu_item;
{
   int i, pos;
   Menu cm;
   struct category_rec *cr, *pcr, *currcr;
   struct entry_list *currel, *prevel;
   struct recurrence_list *currrl, *prevrl;

   while (xv_get(menu, MENU_PARENT) != NULL) menu = xv_get(menu, MENU_PARENT);
   cr = (struct category_rec *)xv_get(menu_item, MENU_CLIENT_DATA);

   if (xv_get(categories, PANEL_ITEM_MENU) == menu) {
      entry_head = cr->entry_head;
      entry_tail = cr->entry_tail;
      rl_head = cr->rl_head;
      rl_tail = cr->rl_tail;
      xv_set(categories, PANEL_CLIENT_DATA, cr, 0);
      xv_set(category_name, PANEL_LABEL_STRING, cr->name, 0);
      xv_set(print_print_base->categories, PANEL_INACTIVE, FALSE, 0);
      display_list(curr_month, curr_day, curr_year);
   } else if (xv_get(Category_category_base->cat_insert_location, PANEL_ITEM_MENU) == menu) {
      xv_set(Category_category_base->cat_insert_location, PANEL_CLIENT_DATA, cr, 0);
      xv_set(Category_category_base->cat_insert_location_name, PANEL_LABEL_STRING, cr->name, 0);
   } else {
      xv_set(entry_category, PANEL_CLIENT_DATA, cr, 0);
      xv_set(entry_category_name, PANEL_LABEL_STRING, cr->name, 0);
   }
}

/*
 * **********************************************************************
 * Menu callback for the "Modify Category" menu selection.  This will
 * edit the *current* category.
 */
Menu_item edit_category(item, op)
Menu_item   item;
Menu_generate  op;
{
   struct category_rec *cr, *pcr, *currcr;

   switch (op) {
   case MENU_DISPLAY:
      break;

   case MENU_DISPLAY_DONE:
      break;

   case MENU_NOTIFY:
      cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
      xv_set(Category_category_base->cat_name, PANEL_VALUE, cr->name, 0);
      xv_set(Category_category_base->cat_insert_pos, PANEL_VALUE, 0, 0);
   
      pcr = NULL;
      if (cr->parent == NULL) {
         currcr = category_head;
         while (currcr != cr) {
            pcr = currcr; 
            currcr = currcr->next;
         }
      } else if (cr->parent->subcats == cr) {
         pcr = cr->parent;
			xv_set(Category_category_base->cat_insert_pos, PANEL_VALUE, 1, 0);
      } else {
         currcr = cr->parent->subcats;
         while (currcr != cr) {
            pcr = currcr; 
            currcr = currcr->next;
         }
         pcr->next = cr->next;
      }

      xv_set(Category_category_base->cat_insert_location,
             PANEL_CLIENT_DATA, pcr,
             0);
      if (pcr == NULL) {
         xv_set(Category_category_base->cat_insert_location_name,
                PANEL_LABEL_STRING, category_head->name,
                PANEL_CLIENT_DATA, pcr,
                0);
      } else {
         xv_set(Category_category_base->cat_insert_location_name,
                PANEL_LABEL_STRING, pcr->name,
                PANEL_CLIENT_DATA, pcr,
                0);
      }
      xv_set(Category_category_base->cat_done,
             PANEL_NOTIFY_PROC, finish_category_edit,
             PANEL_CLIENT_DATA, cr,
             0);
      
      xv_set(Category_category_base->category_base, XV_SHOW, TRUE, 0);

      break;

   case MENU_NOTIFY_DONE:
      break;
   }
   return item;
}

/*
 * **********************************************************************
 * Menu callback for the "Delete Category" menu selection.  This will
 * delete the *current* category.
 */
Menu_item delete_category(item, op)
Menu_item   item;
Menu_generate  op;
{
   int i, pos;
   int response, choice;
   Menu cm;
   char catname[80];
   struct category_rec *cr, *pcr, *currcr;
   struct entry_list *currel, *prevel;
   struct recurrence_list *currrl, *prevrl;

   switch (op) {
   case MENU_DISPLAY:
      break;

   case MENU_DISPLAY_DONE:
      break;

   case MENU_NOTIFY:
      cr = (struct category_rec *)xv_get(categories, PANEL_CLIENT_DATA);
      
      response = notice_prompt(tdlist, NULL,
                    NOTICE_MESSAGE_STRINGS,
                    "Are you sure you want to delete",
                    cr->name,
                    0,
                    NOTICE_BUTTON_YES, "Yes, Delete",
                    NOTICE_BUTTON_NO, "No, Cancel",
                    0);
      if (response == NOTICE_NO) return item; 

      pcr = NULL;
      if (cr->parent == NULL) {
         currcr = category_head;
         while (currcr != cr) {
            pcr = currcr; 
            currcr = currcr->next;
         }
      
         if (pcr == NULL) {
            category_head = cr->next;
         } else {
            pcr->next = cr->next;
         }
      } else if (cr->parent->subcats == cr) {
         cr->parent->subcats = cr->next;
      } else {
         currcr = cr->parent->subcats;
         while (currcr != cr) {
            pcr = currcr; 
            currcr = currcr->next;
         }
         pcr->next = cr->next;
      }
      
      /*  If there is another category, display another one! */
      if (cr->next == NULL) {
         if (cr->parent == NULL) {
            if (category_head == NULL) {
               category_head = new_category("Every Day", NULL, FALSE);
            }
            pcr = category_head;
         } else {
            pcr = cr->parent;
         }
      } else {
         pcr = cr->next;
      }
      entry_head = pcr->entry_head;
      entry_tail = pcr->entry_tail;
      rl_head = pcr->rl_head;
      rl_tail = pcr->rl_tail;
      xv_set(categories,
             PANEL_CLIENT_DATA, pcr,
             0);
      strcpy(catname, pcr->name);
      display_list(curr_month, curr_day, curr_year);
      
      /* rebuild the menu */
      xv_set(categories, PANEL_ITEM_MENU, cat_menu(category_head), 0);
      xv_set(entry_category, PANEL_ITEM_MENU, cat_menu(category_head), 0);
      xv_set(Category_category_base->cat_insert_location,
             PANEL_ITEM_MENU, cat_menu(category_head),
             0);
      xv_set(categories, PANEL_CLIENT_DATA, pcr, 0);
      xv_set(category_name, PANEL_LABEL_STRING, pcr->name, 0);
      xv_set(Category_category_base->cat_insert_location, PANEL_CLIENT_DATA, pcr, 0);
      xv_set(Category_category_base->cat_insert_location_name, PANEL_LABEL_STRING, pcr->name, 0);
      xv_set(entry_category, PANEL_CLIENT_DATA, pcr, 0);
      xv_set(entry_category_name, PANEL_LABEL_STRING, pcr->name, 0);

      /* free the data structure. */
      prevel = NULL;
      for (currel=cr->entry_head;
           currel != NULL;
           prevel=currel,currel=currel->next,free(prevel));
      prevrl = NULL;
      for (currrl=cr->rl_head;
           currrl != NULL;
           prevrl=currrl,currrl=currrl->next,free(prevrl));
      free(cr);

      break;

   case MENU_NOTIFY_DONE:
      break;
   }
   return item;
}


/*
 * **********************************************************************
 * Call back routine to list all categories.  Does a little house keeping
 *  -- like make the categories item in print window inactive -- but 
 * really nothing terribly special.
 */
void list_all_categories(item, event)
Panel_item item; 
Event      *event;
{
   xv_set(print_print_base->categories,
          PANEL_CLIENT_DATA, PRINT_ALL,
          PANEL_INACTIVE, TRUE,
          0);
   display_all(curr_month, curr_day, curr_year);
}

/*
 * **********************************************************************
 * Call back routine to finish up category edits.
 */
void finish_category_edit(item, event)
Panel_item item; 
Event      *event;
{
   char text[LINESIZ];
   struct category_rec *cr, *insertcr=NULL, *pcr=NULL;
   int pos, index, subcat, after;

   strcpy(text, (char *)xv_get(Category_category_base->cat_name, PANEL_VALUE));
   cr = (struct category_rec *)xv_get(Category_category_base->cat_done, PANEL_CLIENT_DATA);
   insertcr = (struct category_rec *)xv_get(Category_category_base->cat_insert_location, PANEL_CLIENT_DATA);
   pcr = (struct category_rec *)xv_get(Category_category_base->cat_insert_location_name, PANEL_CLIENT_DATA);
   after = xv_get(Category_category_base->cat_insert_pos, PANEL_VALUE) == 0;
   subcat = xv_get(Category_category_base->cat_insert_pos, PANEL_VALUE) == 1;

   if (insertcr == cr) {
      notice_prompt(entry_frame, NULL,
                    NOTICE_MESSAGE_STRINGS,
                    "You cannot specify a category",
                    "in relation to itself!",
                    0,
                    NOTICE_BUTTON, "Ok", 1,
                    0);
      return;
   }

   strcpy(cr->name, text);
   xv_set(category_name, PANEL_LABEL_STRING, cr->name, 0);
   xv_set(Category_category_base->cat_insert_location_name, PANEL_LABEL_STRING, cr->name, 0);
   xv_set(Category_category_base->cat_insert_location, PANEL_CLIENT_DATA, cr, 0);
   xv_set(entry_category_name, PANEL_LABEL_STRING, cr->name, 0);
   xv_set(Category_category_base->cat_done,
          PANEL_NOTIFY_PROC, insert_category,
          0);

   changed = TRUE;

	if (insertcr == NULL) return;

   if ( ! (after & (insertcr->next == cr)) && ! (subcat & (insertcr->subcats == cr))) {
		if (cr->parent == NULL) {
			if (pcr == NULL) {
				category_head = cr;
			} else {
				pcr->next = cr->next;
			}
		} else if (cr->parent->subcats == cr) {
			cr->parent->subcats = cr->next;
		} else {
			pcr->next = cr->next;
		}
		if (after) {
			if (category_head == cr) category_head = cr->next;
			cr->next = insertcr->next;
			insertcr->next = cr;
		} else {
			cr->next = insertcr->subcats;
			insertcr->subcats = cr;
		}
	}

   /* rebuild the menu */
   xv_set(categories, PANEL_ITEM_MENU, cat_menu(category_head), 0);
   xv_set(entry_category, PANEL_ITEM_MENU, cat_menu(category_head), 0);
   xv_set(Category_category_base->cat_insert_location,
          PANEL_ITEM_MENU, cat_menu(category_head),
          0);
   xv_set(categories, PANEL_CLIENT_DATA, cr, 0);
   xv_set(Category_category_base->cat_insert_location, PANEL_CLIENT_DATA, cr, 0);
   xv_set(entry_category, PANEL_CLIENT_DATA, cr, 0);

}

/*
 * Event callback function for `cat_insert_pos'.
 */
void switch_insert_modes(item, event)
Panel_item  item;
Event    *event;
{
   struct category_rec *cr;
   category_category_base_objects *ip =
      (category_category_base_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
   
   cr = (struct category_rec *)xv_get(Category_category_base->cat_insert_location, PANEL_CLIENT_DATA);
   if (xv_get(Category_category_base->cat_insert_pos, PANEL_VALUE) == 1) {
      if (cr->subcats != NULL) {
         notice_prompt(entry_frame, NULL,
                       NOTICE_MESSAGE_STRINGS,
                       "There is already a subcategory",
                       "for this category.",
                       0,
                       NOTICE_BUTTON, "Ok", 1,
                       0);
         xv_set(Category_category_base->cat_insert_pos, PANEL_VALUE, 0, 0);
      }
   }
}

void load_cat_from_file(item, event)
Panel_item item; 
Event      *event;
{
}
 
#endif
