/*
        Open look version - Openwindow 2.0.
	Original Rolo by Ron Hitchens
c*/
/*
 * The changes made in this file for porting is a product of
 * Sun Microsystems, Inc. and are provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this code without charge.

 * THIS CODE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This code is provided with no support and without any obligation on the
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
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/seln.h>
#include <sys/param.h>
#include <ctype.h>
 
#include "defs.h"
#include "help.h"

#include "openp.h"
/*
	local	vars
*/
int	panel_width, panel_height;
int	load_file_action = 0;
static void	goto_card();
extern	struct	card	*current, *first ,*last ;
extern	struct	card	*make_card() ,*insert_card();
extern	struct	card	*undelete_card();
extern	Textsw	rolocard;
extern	int	need_save;
extern	char	*rolofile;

void	set_slider_max(), show_card();
static char * get_selection() , * re_comp();
extern char 	*first_char (), *index (), *re_comp (), *strcpy(),
	*strncpy (), *strcat (), *sprintf (), *getenv ();
extern	void	dbl_clck_list_proc();

Panel
init_panel(frame)
Frame	frame;
{
Panel	panel;
int	i;
	panel = (Panel)xv_create(frame, PANEL,
				XV_HELP_DATA, "rolo:panel",
				0);
		/* pop windows associated with some menu items */
	pop_undelete_frame = make_pop_undelete_frame( frame);
	pop_list_frame = make_pop_list_frame( frame);
	pop_help_frame = make_pop_help_frame( frame );
	pop_file_frame = make_pop_file_frame( frame);
			/* 3 main menu buttons on main window */
	(void)xv_create(panel, PANEL_BUTTON,
				XV_X,		ROLO_PANEL_X_GAP,
				PANEL_LABEL_STRING, "File",
				PANEL_ITEM_MENU, make_file_menu() ,
				XV_HELP_DATA, "rolo:file",
				0);
	(void)xv_create(panel, PANEL_BUTTON,
				PANEL_LABEL_STRING, "View",
				PANEL_ITEM_MENU, make_view_menu(),
				XV_HELP_DATA, "rolo:view",
				0);
	(void)xv_create(panel, PANEL_BUTTON,
				PANEL_LABEL_STRING, "Edit",
				PANEL_ITEM_MENU, make_edit_menu(),
				XV_HELP_DATA, "rolo:edit",
				0);
			/* Find button and input caret */
	(void)xv_create(panel, PANEL_BUTTON,
				XV_X,		ROLO_PANEL_X_GAP,
				XV_Y,		40,
				PANEL_LABEL_STRING, "Find",
				XV_HELP_DATA, "rolo:find",
				PANEL_ITEM_MENU, xv_create(NULL, MENU,
					MENU_STRINGS,"forwards",
						"backwards",0,
					MENU_ACTION_PROC,
						find_menu_proc,
						0),
				0);
	regex_item = xv_create(panel, PANEL_TEXT,
			PANEL_BLINK_CARET,              TRUE,
			PANEL_LABEL_STRING,             "",
			XV_HELP_DATA, "rolo:find",
			/* don't know yet */
			PANEL_VALUE_DISPLAY_LENGTH,     30,
			PANEL_VALUE_STORED_LENGTH,      80,
			PANEL_NOTIFY_PROC,      find_text_button_proc,
			0);

	window_fit_height( panel);
	i = (int) xv_get ( panel, XV_HEIGHT, 0);
		/* slider with end buttons */
	slider_item = xv_create(panel, PANEL_SLIDER,
				XV_HELP_DATA, "rolo:slider",
				XV_X,		ROLO_PANEL_X_GAP,
				XV_Y,		i,
				PANEL_MIN_VALUE,	0,
				PANEL_MAX_VALUE,        4,
				PANEL_VALUE,            1,
				PANEL_SLIDER_WIDTH,     300,
				PANEL_SLIDER_END_BOXES,	TRUE,
				PANEL_SHOW_RANGE,       FALSE,
				PANEL_SHOW_VALUE,       TRUE,
				PANEL_NOTIFY_LEVEL,     PANEL_DONE,
				PANEL_NOTIFY_PROC,      slider_random_proc,
				0);
	window_fit_height( panel);
	i = (int) xv_get ( panel, XV_HEIGHT, 0);
			/* up down button for cards */
	(void)xv_create(panel, PANEL_BUTTON,
				XV_HELP_DATA,"rolo:arrows",
				XV_X, 375,
				XV_Y, ( i - 60),	
				PANEL_LABEL_IMAGE,make_glyphs(uparrow,16,16),
				PANEL_NOTIFY_PROC,	up_one_card_proc,
				0);
	(void)xv_create(panel, PANEL_BUTTON,
				XV_X, 375,
				XV_Y, (i - 30),	
				XV_HELP_DATA,"rolo:arrows",
				PANEL_LABEL_IMAGE,make_glyphs(downarrow,16,16),
				PANEL_NOTIFY_PROC,	down_one_card_proc,
				0);
	window_fit_width( panel);
	panel_width = (int)xv_get( panel, XV_WIDTH, 0);
	panel_height = (int)xv_get( panel, XV_HEIGHT, 0);
	return(panel);
}

/*
	Pop up list of deleted cards for undeleting
*/
Frame
make_pop_undelete_frame(base_frame)
Frame	base_frame;
{
Frame	frame;
Panel	undel_list_panel;
	frame = xv_create( base_frame, FRAME_CMD,
				XV_X, 0,
				XV_Y, 0,
				XV_LABEL, "UNDELETE CARD LIST",
				XV_WIDTH,	450,
				XV_HEIGHT,	300,
				WIN_IS_CLIENT_PANE,
				FRAME_SHOW_LABEL, TRUE,
                                FRAME_CMD_PUSHPIN_IN,       TRUE,
                                0);
	undel_list_panel = (Panel)xv_get( frame , FRAME_CMD_PANEL, 0 );
	if (!undel_list_panel)
	{
		(void)fprintf(stderr,"can't create pop up undelete window\n");
		xv_destroy_safe(base_frame);
		exit(1);
	}
	xv_set( undel_list_panel,
			XV_HELP_DATA, "rolo:undel_list_popup", 0);
	xv_create( undel_list_panel, PANEL_BUTTON,
			XV_HELP_DATA,		"rolo:undel_list_popup",
			XV_X,	ROLO_PANEL_X_GAP,
			PANEL_LABEL_STRING, "Undelete(before)",
                        PANEL_NOTIFY_PROC, undel_before_proc,
                        0);
	xv_create( undel_list_panel, PANEL_BUTTON, 
			XV_HELP_DATA,		"rolo:undel_list_popup",
                        PANEL_LABEL_STRING, "Undelete(after)", 
                        PANEL_NOTIFY_PROC, undel_after_proc, 
                        0);
	undel_list_item = xv_create( undel_list_panel, PANEL_LIST,
			XV_HELP_DATA,		"rolo:undel_list_popup",
			XV_X,			ROLO_PANEL_X_GAP,
			XV_Y,			30,
                        PANEL_LIST_WIDTH,       400,/* num pixels*/
                        PANEL_LIST_DISPLAY_ROWS,       10,/* num rows */
			PANEL_CHOOSE_ONE,	FALSE,
                        0);
	window_fit(undel_list_panel);
	window_fit(frame);
	return(frame);
}


/*
	Pop up window list of cards
*/
Frame
make_pop_list_frame(base_frame)
Frame	base_frame;
{
Frame	frame;
Panel	panel;
void	delete_from_list();
		
	frame = xv_create( base_frame, FRAME_CMD,
				XV_LABEL, "CARD LIST",
				XV_WIDTH,	450,
				XV_Y,		500,
				WIN_IS_CLIENT_PANE,
				FRAME_SHOW_LABEL, TRUE,
                                FRAME_CMD_PUSHPIN_IN,       TRUE,
                                0);
	panel = (Panel)xv_get( frame , FRAME_CMD_PANEL, 0 );
	if (!panel)
	{
		(void)fprintf(stderr,"can't create pop up list window\n");
		xv_destroy_safe(base_frame);
		exit(1);
	}
	xv_set( panel,
			XV_HELP_DATA, "rolo:list_popup", 0);
#ifdef DOUBLECLICK
	xv_create( panel, PANEL_BUTTON,
			XV_HELP_DATA, "rolo:list_popup",
			XV_X,			ROLO_PANEL_X_GAP,
			PANEL_LABEL_STRING,	"show card",
			PANEL_NOTIFY_PROC,	jump_to_card,
			0);
#endif DOUBLECLICK
	xv_create( panel, PANEL_BUTTON,
			XV_HELP_DATA, "rolo:list_popup",
			XV_X,			ROLO_PANEL_X_GAP,
			PANEL_LABEL_STRING,	"delete selected cards",
			PANEL_NOTIFY_PROC,	delete_from_list,
			0);
	list_item = xv_create( panel, PANEL_LIST,
			XV_HELP_DATA, "rolo:list_popup",
			XV_X,			ROLO_PANEL_X_GAP,
			XV_Y,			30,
                        PANEL_LIST_WIDTH,       400,/* num pixels*/
                        PANEL_LIST_DISPLAY_ROWS,        15,/* num rows */
			PANEL_CHOOSE_ONE,	FALSE,
			PANEL_NOTIFY_PROC,	dbl_clck_list_proc,
                        0);
	window_fit(panel);
	window_fit(frame);
	xv_set(frame, XV_SHOW, FALSE ,0);
	return(frame);
}

/*
	Pop up help window
*/
Frame
make_pop_help_frame(base_frame)
Frame	base_frame;
{
Frame	frame;
Panel	panel;
Panel_item	help_list_item;
int	i,j, k = 1;
char	string[100];
char	*ptr , *lptr;
		
	frame = xv_create( base_frame, FRAME_CMD,
				XV_LABEL, "HELP",
				WIN_IS_CLIENT_PANE,
				FRAME_SHOW_LABEL, TRUE,
                                FRAME_CMD_PUSHPIN_IN,       TRUE,
				PANEL_LAYOUT,	PANEL_VERTICAL,
                                0);
	panel = (Panel)xv_get( frame , FRAME_CMD_PANEL, 0 );
	if (!panel)
	{
		(void)fprintf(stderr,"can't create pop up help window\n");
		xv_destroy_safe(base_frame);
		exit(1);
	}
	help_list_item = xv_create( panel, PANEL_LIST ,
				XV_HELP_DATA, "rolo:help_popup",
				XV_X,	ROLO_PANEL_X_GAP,
				PANEL_LIST_WIDTH, xv_col(panel,80),
				PANEL_LIST_DISPLAY_ROWS, 25,
				PANEL_LIST_NROWS, 220,
				PANEL_READ_ONLY,	TRUE,
				PANEL_CHOOSE_NONE, TRUE,
				0);
	for(i = 0 ; i < sizeof (help_msg) / sizeof (char *); i++)
	{
		lptr = ptr = help_msg[i];/* each line contains
						many sentences */
		while ( *lptr != '\0')
		{
			j = 0;
			while (( *ptr != '\n' )&(*ptr != '\0')&(j<100))
				string[j++] = *ptr++;
			string[j] = '\0';
			xv_set( help_list_item,
				PANEL_LIST_STRING, k++ ,string,
				0);
			lptr = ++ptr;
		}
	}
	window_fit(panel);
	window_fit(frame);
	xv_set(frame, XV_SHOW, FALSE ,0);
	return(frame);
}

/*
	Pop up window for file operations
*/
Frame
make_pop_file_frame( base_frame)
Frame	base_frame;
{
Frame	frame;
Panel	panel;
int	i;
		
	frame = xv_create( base_frame, FRAME_CMD,
				XV_LABEL, "FILE NAME",
				XV_X,		100,
				XV_Y,		100,
				XV_WIDTH,	200,
				XV_HEIGHT,	100,
				WIN_IS_CLIENT_PANE,
				PANEL_LAYOUT,           PANEL_VERTICAL,
				FRAME_SHOW_LABEL, TRUE,
                                FRAME_CMD_PUSHPIN_IN,       TRUE,
                                0);
	panel = (Panel)xv_get( frame , FRAME_CMD_PANEL, 0 );
	if (!panel)
	{
		(void)fprintf(stderr,"can't create pop up file window\n");
		xv_destroy_safe(base_frame);
		exit(1);
	}
	xv_set( panel,
			XV_HELP_DATA, "rolo:file_popup", 0);
	pop_file_message = xv_create( panel, PANEL_MESSAGE,
			PANEL_LABEL_STRING,"",
			0);
	pop_file_path_item = xv_create( panel, PANEL_TEXT ,
			PANEL_LABEL_STRING,"Enter path name:",
			PANEL_VALUE, getenv("HOME"),	/* default */
			0);
	pop_file_name_item = xv_create( panel, PANEL_TEXT ,
			PANEL_LABEL_STRING," Enter file name:",
			PANEL_VALUE, rolofile,		/* default */
			0);
	window_fit(panel);
	i = (int) xv_get ( panel, XV_HEIGHT, 0);
	pop_file_button_label = 
			xv_create( panel, PANEL_BUTTON,
			XV_X,	ROLO_PANEL_X_GAP,
			XV_Y,	i ,	
			PANEL_LABEL_STRING,"ok",
			PANEL_NOTIFY_PROC, ok_file_proc,
			0);
	window_fit(panel);
	window_fit(frame);
	xv_set(frame, XV_SHOW, FALSE, 0 );
	return(frame);
}

/*	file
		save file to disk
                load file from disk
                save file to file       ->      filename
                load file from file     ->      filename
*/
Menu
make_file_menu()
{
Menu	menu;
	menu = (Menu)xv_create( XV_NULL,MENU,
			MENU_PULLRIGHT_ITEM , "Save",
				(Menu)xv_create( NULL, MENU,
					MENU_STRINGS, "to current file",
						"to named file ...",0,
					MENU_ACTION_PROC, file_save_proc,
					0),
                        MENU_PULLRIGHT_ITEM, "Load",
                                (Menu)xv_create( NULL, MENU,
					MENU_STRINGS,
						"from current file",
						"from named file ...",0,
					MENU_ACTION_PROC, file_load_proc,
					0),
			MENU_RELEASE,
			0);
	return(menu);
}

/* view-
                list
                help
                sort cards ->   forwards
                                backwards
*/
Menu
make_view_menu()
{
        Menu menu;
        menu = xv_create(XV_NULL, MENU,
                        MENU_ITEM,
                                MENU_STRING,"list...",
                                MENU_NOTIFY_PROC,       list_menu_proc,
                                0,
                        MENU_ITEM,
                                MENU_STRING,"help...",
                                MENU_NOTIFY_PROC,       help_menu_proc,
                                0,
                        MENU_ITEM,
                                MENU_STRING,    "sort cards",
                                MENU_PULLRIGHT,(Menu)
                                        xv_create( NULL, MENU,
                                        MENU_STRINGS,    "forwards",
                                                        "backwards",
                                                        0,
					MENU_ACTION_PROC,
						sort_menu_proc,
                                                0),
                                0,
                        0);
        return(menu);
}

/*
	Make the edit menu
*/
Menu
make_edit_menu()
{
	Menu menu;
        menu = xv_create( XV_NULL, MENU,
                        MENU_ITEM,
                                MENU_STRING, "New Card",
				MENU_PULLRIGHT, xv_create(NULL, MENU,
					MENU_STRINGS,	"after",
							"before", 0,
					MENU_ACTION_PROC,
						new_card_menu_proc,
					0),
                                0,
                        MENU_ITEM,
                                MENU_STRING,            "delete",
				MENU_PULLRIGHT, xv_create(NULL, MENU,
					MENU_STRINGS,"current",
						     "from list...",0,
					MENU_ACTION_PROC,       delete_menu_proc,
					0),
                                0,
                        MENU_ITEM,
                                MENU_STRING,    "undelete card(last)",
				MENU_PULLRIGHT, xv_create(NULL, MENU,
					MENU_STRINGS,   "after",
							"before", 0,
					MENU_ACTION_PROC,
						undelete_menu_proc,
					0),
                                0,
                        MENU_ITEM,
                                MENU_STRING,    "undelete card...",
                                MENU_NOTIFY_PROC,       undelete_pop_menu_proc,
                                0,
                        0);
        return(menu);
}

/*
	Return a glyph (picture) for a given array of shorts
*/
Server_image *
make_glyphs(array,w,h)
u_short   *array;
int	w,h;
{
        Server_image    *server_image;

        server_image = (Server_image *)xv_create(NULL,SERVER_IMAGE,
                                        XV_WIDTH, w,
                                        XV_HEIGHT,h,
                                        SERVER_IMAGE_DEPTH, 1,
                                        SERVER_IMAGE_BITS,array,
                                        0);
        return(server_image);
}

/*
	Notify procs for all the buttons and menu items
*/
/*
	Notify procs for the File button.
*/
caddr_t
file_save_proc(menu, menu_item)
Menu            menu;
Menu_item       menu_item;
{
	int	choice = (int) xv_get(menu_item, MENU_VALUE, 0 );
	if (choice == 2)        /* get a file name to save to */
	{			/* new name becomes current file */
		(void)xv_set(pop_file_message,
			PANEL_LABEL_STRING,"save to:",0);
		(void)xv_set(pop_file_button_label,
			PANEL_LABEL_STRING,"save",0);
		load_file_action = 0;
			/* Pop a window to inquire for file name */
		(void)xv_set( pop_file_frame, XV_LABEL, "SAVE TO FILE", 0 );
		(void)xv_set( pop_file_frame, XV_SHOW, TRUE,
					WIN_FRONT, TRUE,
					0 );
		return;
	}
			/* save to current file name */
	xv_set(rolo_frame, FRAME_BUSY, TRUE, 0);
	dump_rolo (first, rolofile);
	xv_set(rolo_frame, FRAME_BUSY, FALSE, 0);
}

caddr_t
file_load_proc(menu, menu_item)
Menu            menu;
Menu_item       menu_item;
{
	int	choice = (int) xv_get(menu_item, MENU_VALUE, 0 );
	save_card(current);
	if (choice == 2)	/* get a file name to load from */
	{			/* new name becomes current file */
		(void)xv_set(pop_file_message,
			PANEL_LABEL_STRING,"load from:",0);
		(void)xv_set(pop_file_button_label,
			PANEL_LABEL_STRING,"load",0);
		load_file_action = 1;
		(void)xv_set( pop_file_frame, XV_LABEL, "LOAD FROM FILE", 0 );
		(void)xv_set( pop_file_frame, XV_SHOW, TRUE ,
					WIN_FRONT, TRUE,
					0);
		return;
	}
				/* load from current file (revert) */
	if ((need_save == TRUE) && (verify_no_save () == FALSE)) {
		return;
	}
	xv_set(rolo_frame, FRAME_BUSY, TRUE, 0);
	nuke_active_cards ();
	init_rolo (rolofile);
	xv_set(rolo_frame, FRAME_BUSY, FALSE, 0);
}

/*
	Notify proc for sort button,Sort all the cards accordingly
*/
caddr_t
sort_menu_proc(menu, menu_item)
Menu            menu;
Menu_item       menu_item;
{
	int	i = (int)xv_get(menu_item, MENU_VALUE, 0 );
	xv_set(rolo_frame, FRAME_BUSY, TRUE, 0);
	if (i == 2)
		sort_cards (TRUE);	/* sort descending */
	else
		sort_cards (FALSE);	/* sort ascending */
	need_save = TRUE;
	xv_set(rolo_frame, FRAME_BUSY, FALSE, 0);
}

/*
	Pops a list window.
*/
void
list_menu_proc(menu, menu_item)
Menu            menu;
Menu_item       menu_item;
{
	int	x0,x1,y0,y1;
	static	int	first=1;
	if (!first)
	{
		xv_set( pop_list_frame ,
			XV_SHOW , TRUE ,
			WIN_FRONT , TRUE ,
			0);
		return;
	}
	else
		first = 0;
	find_win_pos(&x0, &x1, &y0, &y1 );
	xv_set( pop_list_frame ,
			XV_X,	- WIN_BORDERS_WIDTH,
			XV_Y,	y1,
			XV_SHOW , TRUE ,
			WIN_FRONT , TRUE ,
			0);
	/*debug_list(list_item,"list_item\0");*/
}

/*
	Pops a help window at the lower right corner
*/
void
help_menu_proc(menu, menu_item)
Menu            menu;
Menu_item       menu_item;
{
	int	x0,x1,y0,y1;
	static	int	first=1;
	if (!first)
	{
		xv_set( pop_help_frame ,
			XV_SHOW , TRUE ,
			WIN_FRONT , TRUE ,
			0);
		return;
	}
	else
		first=0;
	find_win_pos(&x0, &x1, &y0, &y1 );
	xv_set( pop_help_frame ,
			XV_X,	x1,
			XV_Y,	y1,
			XV_SHOW , TRUE ,
			WIN_FRONT , TRUE ,
			0);
}

/*
	Save the old card,
	Makes a new card, and puts it in the databases
	The new card is displayed and will be edited on the main window
*/
caddr_t
new_card_menu_proc(menu, menu_item)
Menu            menu;
Menu_item       menu_item;
{
int	n;
struct	card *c, *p , *t;

	save_card (current);
	c = make_card (NULL_CARD);
	if (c == NULL_CARD) {
		(void)msg ("Can't allocate space for a new card");
		return;
	}

	n = (int) xv_get (menu_item, MENU_VALUE, 0);
	if (n == 2)	/* insert before */
		t = current->c_prev;	/* c_prev may be NULL */
	else		/* by default insert after */
		t = current;
	p = insert_card (c, t);		/* insert c after t */
	need_save = TRUE;

        set_slider_max (renumber (first));      /* update slider range */
	insert_card_list(p);		/* into the active list */
	show_card (p);
	xv_set( rolocard, WIN_SET_FOCUS , 0 ); /* set window focus to text window */
}

/*
	Delete cards from the rolodex
	Currently you can only delete the current card one by one
*/
struct  card    *delete_from_active_list();
caddr_t
delete_menu_proc(menu, menu_item)
Menu            menu;
Menu_item       menu_item;
{
int	i;
struct	card	*p;

	save_card(current);

	i = (int)xv_get(menu_item, MENU_VALUE,0);
	if ( i == 2)	/* delete from list */
	{
		if ((int)xv_get(pop_list_frame, XV_SHOW,0) == FALSE )
		{
			int	x0,x1,y0,y1;
			find_win_pos(&x0, &x1, &y0, &y1 );
			xv_set( pop_list_frame ,
					XV_X,	- WIN_BORDERS_WIDTH,
					XV_Y,	y1,
					XV_SHOW , TRUE ,
					0);
		}
		else
			xv_set( pop_list_frame ,
					WIN_FRONT, TRUE,
					0);
		return ((caddr_t) 0);
	}
	else
	{
		if (first == last) {
			/* if we're deleting the last card, add a dummy */
			(void) insert_card (make_card (NULL_CARD), NULL_CARD);
			set_slider_max (renumber (first)); 
			insert_card_list(first);		/* into the active list */
		}
		/* and the new current card will be... */
		p = (current == last) ? current->c_prev : current->c_next;
		delete_card (current);	/* also insert into undel list */
	}
	need_save = TRUE;
        set_slider_max (renumber (first));
        show_card (p);	/* show new current card */
	return ((caddr_t) 0);
}
 
void
delete_from_list(panel, event)
Panel   panel;
Event	*event;
{
struct  card	*p;
 /* new current card will be... */
	p = delete_from_active_list();
				/* nothing deleted */
	if (p == (struct card *)NO_ACTION)
		return ;
	if (p==NULL)	/* deleted everything,add a dummy*/
	{
		first = NULL; last = NULL;
		p=insert_card (make_card (NULL_CARD), NULL_CARD);
		set_slider_max (renumber (first)); 
		insert_card_list(p);		/* into the active list */
	}
	need_save = TRUE;
        set_slider_max (renumber (first));
        show_card (p);	/* show new current card */
}

/*
	Recover last deleted card. Either before or after current card.
*/
caddr_t 
undelete_menu_proc(menu, menu_item) 
Menu            menu; 
Menu_item       menu_item; 
{ 
int	choice;
struct	card	*p;
static	int	bozo = 0;
	save_card( current);
	choice = (int) xv_get( menu_item, MENU_VALUE, 0 );
	switch(choice)
	{
	case 1:
		p = undelete_card ( current );
		break;
	case 2:
		p = undelete_card ( current->c_prev );
		break;
	default:
		return((caddr_t) 0);
	}
	need_save = TRUE;
	set_slider_max (renumber (first));
	if (p == NULL_CARD) {
		if (bozo) {
                        (void)msg ("There are no deleted cards to undelete");
                } else {   
			window_bell (rolo_frame);
                        bozo++;
                }
		return((caddr_t) 0);
        }
	undel_insert_list(p,0);
	show_card (p);
	return ((caddr_t) 0);
} 
 
/*
	Pop a window containing a list of previously deleted cards
	(in the same session), user recover a collection of cards in
	the list by highlighting them with the adjust(middle)mouse
	button.
*/
void 
undelete_pop_menu_proc(menu, menu_item) 
Menu            menu; 
Menu_item       menu_item; 
{ 
	int	x1,x0,y0,y1;
	static	int	first=1;
	if (!first)
	{
		xv_set( pop_undelete_frame ,
			XV_SHOW , TRUE ,
			WIN_FRONT , TRUE ,
			0);
		return;
	}
	else
		first=0;
	find_win_pos( &x0, &x1, &y0, &y1);
	xv_set( pop_undelete_frame ,
		XV_X,	x1 ,
		XV_Y, 0,
		XV_SHOW , TRUE ,
		WIN_FRONT , TRUE ,
		0);
	/*debug_list(undel_list_item, "undel_list_item\0"); */
} 

/* Panel procs */
/*	slider first card button is pressed , goto first card */
void			/* set to first card */
slider_begin_proc(panel_item, event)
Panel_item              panel_item;
Event           *event;
{
	save_card(current);
	xv_set( slider_item, PANEL_VALUE, 1 , 0 );
	show_card(first);
}

void			/* set to last card */
slider_end_proc(panel_item, event)
Panel_item              panel_item;
Event           *event;
{
int	end = last->c_num - 1;
	save_card(current);
	xv_set( slider_item, PANEL_VALUE, end , 0 );
	show_card(last);
}

/*	slider at a new (random) postion  */
void			/* show that card */
slider_random_proc(panel_item, value,  event)
Panel_item              panel_item;
int			value;
Event           *event;
{
	save_card(current);
	goto_card(value);
}

/*
	The user has selected a card in the list of active cards, 
	show the first picked card on the list.
*/
void	/* jump to first chosen card */
jump_to_card(panel_item,  event)
Panel_item              panel_item;
Event           *event;
{
struct	card	*p;
int	chosen;
	if (last == NULL_CARD)	return;
	for (chosen = 0; chosen < last->c_num; chosen++)
		if ((int)xv_get(list_item,PANEL_LIST_SELECTED,chosen,0))
		{
			save_card(current);
			p = (struct card *)xv_get( list_item,
				PANEL_LIST_CLIENT_DATA, chosen, 0);
			if (p != NULL_CARD)	 show_card(p);
			break;
		}
}

/* recover a deleted card before or after current card */
void
undel_before_proc(panel_item, event) 
Panel_item              panel_item; 
Event           *event; 
{ 
	undel_card(TRUE);
} 
 
void
undel_after_proc(panel_item, event) 
Panel_item              panel_item;  
Event           *event;  
{
	undel_card(FALSE);
}


/*
	Step up the list one card at a time
*/
void
up_one_card_proc(panel_item, event)
Panel_item              panel_item;
Event           *event;
{
	struct	card *p;
	static int bozo = 0;

	save_card(current);
	p = current->c_prev ;
	if ( p == NULL_CARD)
	{
		if (bozo)
			(void)msg ("This is the first card");
		else
		{
			window_bell (rolo_frame);
			bozo++;
		}
	return;
	}
	show_card(p);
	bozo = 0;
}

/*
	Step down the list one card at a time
*/
void
down_one_card_proc(panel_item, event)
Panel_item              panel_item;
Event           *event;
{
	struct	card *p;
	static int bozo = 0;

	save_card(current);
	p = current->c_next ;
	if ( p == NULL_CARD)
	{
		if (bozo)
			(void)msg ("This is the last card");
		else
		{
			window_bell (rolo_frame);
			bozo++;
		}
	return;
	}
	show_card(p);
	bozo = 0;
}


/*
	When find button or menu is clicked, chaeck if there's 
	a text string entered, if so, search for the pattern in
	the rolodex. otherwise an active selection( if mouse has
	hilighted some text some where on the screen)is used as
	the pattern. Rolo will then display the next card with
	that pattern, multiple clicking shows all the cards with
	the matching pattern successively.
*/
caddr_t
find_menu_proc(menu , menu_item )
Menu	menu;
Menu	menu_item;
{
int	dir;
char            *e, *pe,  regbuf [MAX_SELN_LEN];

	pe = (char *) xv_get(regex_item,PANEL_VALUE);
		/* if no panel text and selection active, use it */
	if (((pe==NULL)||(strlen(pe)==0))
				&& (e = get_selection ()) != NULL)
	{
                (void) strcpy (regbuf, e);
                /* panel item is empty, copy selection into it */
		xv_set (regex_item, PANEL_VALUE, regbuf, 0);
        }
	else
		(void) strcpy (regbuf, pe );
	dir = (int) xv_get(menu_item, MENU_VALUE , 0 );
	find_card(regbuf,dir); /* 2 = backward search */
			/* 1 = forward search */
}

/*
	User has entered a string pattern, go look for it, multiple 
	returns gives the next card, it does not care about mouse
	selections.
*/
void
find_text_button_proc(panel_item, event)
Panel_item	panel_item;
Event		event;
{
char	*string;

	string = (char *) xv_get( regex_item, PANEL_VALUE, 0 );
	if (strlen(string)==0)
			return;
	else
		find_card( string, 1);
}

/*
	actually looking for the card in the data base and display it
*/
find_card( regbuf,dir)
char            *regbuf;
int dir;
{
	char            *e;
        struct card     *p;
        static          int bozo = 0;

        save_card (current);
        bozo = 0;
	if (strlen (regbuf) == 0) {
                if (bozo) {
                        (void)msg ("Enter an expression to search for");
                } else {
			window_bell (rolo_frame);
                        bozo++;
                }
                return;
        }
        e = re_comp (regbuf);
        if (e != NULL) {
                (void)msg ("Regular Expression error: %s", e);
                return;
        }
	if (dir == 2)
		p = (current == first) ? last : current->c_prev;
	else
		p = (current == last) ? first : current->c_next;

        while (p != current) {
                if (re_exec (p->c_text) == 1) {
                        show_card (p);
                        return;
                }
                if (dir == 2) {
                        p = (p == first) ? last : p->c_prev;
                } else {
                        p = (p == last) ? first : p->c_next;
                }
        }
	if (re_exec (p->c_text) != 1)           /* wrapped back to current */
                        window_bell (rolo_frame);
}


/*
	Gets a filename from the user and save or load the file
*/
void
ok_file_proc( panel_item, event )
Panel   panel_item;
Event   event;
{
static char	file[400];
char	*path, *filename;

	save_card(current);
	path = (char *)xv_get( pop_file_path_item, PANEL_VALUE);
	filename = (char *)xv_get( pop_file_name_item, PANEL_VALUE);

	if (filename_ok (filename) == FALSE) {
		(void)msg ("Please enter a file name to save to");
		return;
	}
	if  ( filename_ok (path))
		sprintf(file,"%s/%s",path,filename);
	else
		strcpy( filename, file );
	xv_set(rolo_frame, FRAME_BUSY, TRUE, 0);
	if ( !load_file_action)	/* not load == save */
		write_rolo (file);
	else
	{		/* load_file */
		if (!((need_save == TRUE) && (verify_no_save () == FALSE)))
			read_rolo( file);
	}
	xv_set(rolo_frame, FRAME_BUSY, FALSE, 0);
}

/* gives coordinates of all 4 corners of the base window */
find_win_pos(x0,x1,y0,y1)
int	*x0, *x1 , *y0, *y1;
{
	*x0 = (int) xv_get( rolo_frame, XV_X);
	*y0 = (int) xv_get( rolo_frame, XV_Y);
	*x1 = ((int) xv_get( rolo_frame, XV_WIDTH))
					+ WIN_BORDERS_WIDTH;
	*y1 = ((int) xv_get( rolo_frame, XV_HEIGHT));
}

/* ----------------------------------------------------------------------- */

/*		Interposer functions watching for frame events		*/


/*
 *	Interposer proc for catching window resize events.  We're a little
 *	bit fascist here and insist that the frame remain at least big
 *	enough to display the whole panel and at least three lines of the
 *	text window.
 */

#define MIN_CARD_ROWS	3

Notify_value
catch_resize (frame, event, arg, type)
	Frame			frame;
	Event			*event;
	Notify_arg		arg;
	Notify_event_type	type;
{
	Panel			panel;
	int			width;
	int			height;
	int			card_height;
	int			frame_height;
	Notify_value		value;

	value = notify_next_event_func (frame, event, arg, type);
	value = 1;

	if (event_id (event) != WIN_RESIZE) {
		return (value);
	}

	if ((int) xv_get (frame, FRAME_CLOSED) == TRUE) {
		return (value);
	}

	panel = (Panel) xv_get (frame, FRAME_NTH_SUBWINDOW, 0);/* sunview*/
	if ( panel == NULL ) /* xview */
		panel = (Panel) xv_get (frame, FRAME_NTH_SUBWINDOW, 1);
	if ( panel == NULL )
	{ /* if it fails don't bother to keep going */
		(void)fprintf(stderr,"No such sub window.\n");
		return(0);
	}

	width = (int) xv_get (panel, XV_WIDTH);
	if (width < panel_width) {
		xv_set (panel, XV_WIDTH, panel_width, 0);
		window_fit_width (frame);
	}

	height = (int) xv_get (panel, XV_HEIGHT);
	if (height < panel_height) {
		xv_set (panel, XV_HEIGHT, panel_height, 0);
		window_fit_height (frame);
	}

	card_height = (int) xv_get (rolocard, WIN_ROWS);
	if (card_height < MIN_CARD_ROWS) {
		xv_set (rolocard, WIN_ROWS, MIN_CARD_ROWS, 0);
		window_fit_height (frame);
	}

	/*
	 * This catches cases where the subwindows are completely clipped
	 * and don't shrink as far as the frame is concerned.
	 */
	card_height = (int) xv_get (rolocard, XV_HEIGHT);
	frame_height = (int) xv_get (frame, XV_HEIGHT);
	if (frame_height < (panel_height + card_height)) {
		window_fit_height (frame);
	}

	return (value);
}


/*
 *	Interposer function to catch destroy events.  We want to know when
 *	the tool about to be destroyed so that we can save any changes to the
 *	cards back out to disk.  This gets a little tricky because the
 *	text edit window will veto a tool destroy (selecting Quit from
 *	the tool menu) if there is any text in the window.  This is because
 *	from its point of view the text buffer has been modified because we
 *	inserted the initial contents of the card.  The way we get around
 *	this is by saving the contents of the window, resetting it, calling
 *	the rest of the notification chain, then restoring the contents of
 *	the window.  The destroy proc for the text window is called in that
 *	notification chain and it will see an empty window and not object to
 *	the tool being destroyed.  If this function is called again with
 *	a notification flag other than DESTROY_CHECKING, that means the tool
 *	is really going away (either the user OKed a quit or suntools is
 *	shutting down).  In that case we write the cards back out if they
 *	have been changed, without any fancy footwork.
 */

Notify_value
rolo_destroy (frame, status)
	Frame	frame;
	Destroy_status status;
{
	if (status == DESTROY_CHECKING) {
		Notify_value	s;

		save_card (current);
		textsw_reset (rolocard, 0, 0);		/* fake out textedit */
		s = notify_next_destroy_func (frame, status);
		show_card (current);
		return (s);
	}

	save_card (current); 
	if (need_save && (save_or_not_save (rolo_frame,
		"You have made changes that will be destroyed.")
		== FALSE))
			dump_rolo (first, rolofile);

	return (notify_next_destroy_func (frame, status));
}

/* ----------------------------------------------------------------------- */

/*		Utility procs called by the event handlers above	*/


/*
 *	Set the range on the slider.  Lower bound is always 1, upper bound
 *	is set to the argument i.  The slider size is adjusted according to
 *	how many digits it takes to represent the value.  This looks a bit
 *	goofy because of the way the ATTR_COLS() macro works.  These macros
 *	do NOT return an integer which represents a number of pixels, they
 *	encode a value in the high bits which is interpreted later inside
 *	the library code.  This means you cannot say:
 *		x = panel_width - ATTR_COLS(n);
 *	See the macro definitions in <view2/attr.h>
 */

void
set_slider_max (i)
	int	i;
{
	int	delta = 6;		/* space for 3 digits */

	if (i < 100) {
		delta = 5;		/* space for 2 digits */
	}

	if (i < 10) {
		delta = 4;		/* space for 1 digit */
	}
	delta += 13;	/* space for the buttons and stuff */
	xv_set (slider_item,
		PANEL_MIN_VALUE,	1,
		PANEL_MAX_VALUE,	i,
		PANEL_SLIDER_WIDTH,	panel_width - 15 - xv_cols (rolo_frame,delta),
		0);
}

/*
 *      Get the primary selection, copy it into a static buffer, up to a
 *      maximum, and return a pointer to it.  Return NULL if there is no
 *      current primary selection to get.
 */

static
char *
get_selection()
{
        Seln_holder     holder;
        Seln_request    *buffer;
        static char     sel_text [MAX_SELN_LEN + 1];
 
        holder = seln_inquire (SELN_PRIMARY);
	if ((holder.state == SELN_FAILED)
		||(holder.state == SELN_NONE))	/* can't get it */
			return (NULL);
        buffer = seln_ask (&holder, SELN_REQ_CONTENTS_ASCII, 0, 0);
 
        (void) strncpy (sel_text, buffer->data + sizeof (Seln_attribute),
                MAX_SELN_LEN);
        sel_text [MAX_SELN_LEN] = '\0';
 
        if (strlen (sel_text) == 0) {           /* empty string is no sel. */
                return (NULL);
        }
 
        return (sel_text);
}


/*
 *	Update the displayed information which indicates which card is
 *	currently being displayed.  Provisions are made for the special
 *	cases where the help message or index list is being displayed.
 *	The tool name stripe and the slider value are set to indicate
 *	the number of the current card.
 */

update_num_display (i)
	int	i;
{
	char	buf [MAXPATHLEN + 20];		/* worst case */

	switch (i) {
	case LISTALLCARDS:
		(void) sprintf (buf, "%s - %s  (First lines of all %d cards)",
				NAME, rolofile, last->c_num);
		break;

	case HELPDISPLAYED:
		(void) sprintf (buf, "%s - %s  (Help Message)",
				NAME, rolofile);
		break;

	default:
		xv_set (slider_item, PANEL_VALUE, i, 0);
		(void) sprintf (buf, "%s - %s  (Card %d of %d)",
				NAME, rolofile, i, last->c_num);
		break;
	}

	set_stripe (buf);
}

/*
 *	Save off the contents of the card being displayed in the text window.
 *	If the data in the window has been modified by the user, we need to
 *	replace the data in the card struct with the contents of the window.
 *	Since there isn't a way to load some data into the window as an
 *	initial value and mark it as "clean", we need to use a brute force
 *	method to determine if any changes have been made.  We copy out
 *	the contents of the window and compare it to the data we inserted
 *	in the first place.  If they are the same, no change was made and
 *	there is nothing to do.  If they are different, we throw away the
 *	old contents and stash the pointer to the new in the card struct.
 */

save_card (p)
	struct	card 	*p;
{
	int	red, len;
	char	*c;

	if (p == NULL_CARD) {
		/*
		 * If nil, the text window is being used for an index list
		 * or help message.  If so, we know there is no card data
		 * to be saved, and that some button has been clicked.  So
		 * at this point we'll redisplay the last card that was
		 * displayed.  The slider item should still remember which
		 * one it was.
		 */
		goto_card ((int) xv_get (slider_item,PANEL_VALUE));
		return;
	}

	len = (int) xv_get (rolocard, TEXTSW_LENGTH);
	c = malloc (len + 1);
	red = (int) xv_get (rolocard, TEXTSW_CONTENTS, 0, c, len);
	if (red != len) {
		(void)fprintf (stderr, "rolo: fetch error: red=%d, len=%d\n",
				red, len);
		return;
	}

	c [len] = '\0';
	if (strcmp (c, p->c_text) == 0) {
		free (c);			/* didn't change */
	} else {
		free (p->c_text);		/* changed and must save */
		p->c_text = c;
		update_card_list(p);		/* put in the list */
		need_save = TRUE; 
	}
}

/*
 *	Display the card pointed to by p.  Reset the text window, which
 *	clears it.  Insert the text of the card into the window, then
 *	roll it back to the beginning.  After displaying the text of
 *	the card, update the display items on the panel and set the global
 *	current pointer to point at this card.
 */

void
show_card (p)
	struct	card	*p;
{
	textsw_reset (rolocard, 0, 0);
	textsw_insert (rolocard, p->c_text, strlen (p->c_text));
	xv_set (rolocard, TEXTSW_INSERTION_POINT, 0, 0);
	textsw_normalize_view (rolocard, 0);
	update_num_display (p->c_num);
	show_on_list(p->c_num);
	current = p;
}

/*
	Highlight the card being shown
*/
show_on_list(num)
{
	int chosen, cardnum;
	if (last == NULL_CARD)	return;
	for (chosen = 0; chosen < last->c_num; chosen++)
		if ((int)xv_get(list_item,PANEL_LIST_SELECTED,chosen,0))
		{
			xv_set (list_item, PANEL_LIST_SELECT, chosen,
				FALSE, 0);
		}
	xv_set (list_item, PANEL_LIST_SELECT, num-1, TRUE,0);
}

/*
 *	Change to the card with the given index number.  The number is
 *	range checked, then searched for.  When found, the pointer to the
 *	card struct is passed to the function which actually changes
 *	to the new card.
 */

static
void
goto_card (i)
	int		i;
{
	struct	card	*p, *q;

	if ((i < 1) || (i > last->c_num)) {
		(void)msg ("Sorry, don't have a card #%d", i);
		show_card (first);	/* show something we're sure of */
		return;			/* unlikely to happen */
	}

	/* reduce the search space a bit */
	if ((current != NULL_CARD) && (i >= current->c_num)) {
		q = current;
	} else {
		q = first;
	}

	for (p = q; p != NULL_CARD; p = p->c_next) {
		if (p->c_num == i) {
			show_card (p);
			return;
		}
	}

	(void)msg ("Unexpected inconsistency, couldn't find card #%d", i);
}

/*
 *	Ask the user if they're really sure they don't want to save the
 *	changes they've made.  This function only really exists because
 *	the same question is asked in two places (I really hate duplicating
 *	code).
 */

verify_no_save ()
{
	return (confirm (
 		"You have made changes which will be lost.  Are you sure?"));
}



/*
 *	Make sure a filename is reasonable.  This is important because the
 *	filename is provided via the selection mechanism, which could contain
 *	all manner of gibberish.
 */

static
int
filename_ok (p)
	char	*p;
{
	char	*q, *bad_chars = "!^&*()|~`{}[]:;\\\"'<>?";

	for (q = p; *q != '\0'; q++) {
		if (( ! isgraph (*q)) || (index (bad_chars, *q) != NULL)) {
			(void)msg ("Sorry, that looks like a bad filename to me");
			return (FALSE);
		}
	}

	return (TRUE);
}

