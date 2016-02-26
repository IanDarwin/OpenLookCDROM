/*
	This module is added to make the code look nicer
	in the .c files
*/
#include <xview/svrimage.h>

static  u_short uparrow[] = 
{
#include "bup.curs"
};

static  u_short downarrow[] = 
{
#include "bdown.curs"
};

static	u_short	dummy[] = { 0,0,0,0 };

	/*	rolo panel buttons all 10 pixels from the left */
#define		ROLO_PANEL_X_GAP	10
	/* HEIGHT of the banner on each window */
#define		WIN_BANNER_HEIGHT	40
	/* WIDTH of window borders on both sides */
#define		WIN_BORDERS_WIDTH	5
#define		NO_ACTION		-99

	/* Global  items */
extern Frame   rolo_frame ;
Frame pop_undelete_frame, pop_list_frame, pop_help_frame, pop_file_frame;
Panel_item	slider_item, list_item, undel_list_item, rolocard_item;
Panel_item	regex_item, pop_file_path_item, pop_file_name_item,
		pop_file_message, pop_file_button_label;
extern Textsw		rolocard;

	/* utility routines */
Server_image	*make_glyphs();	/* (array of shorts, w, h) */
Menu		make_view_menu(), make_edit_menu(), make_file_menu();
Frame		make_pop_undelete_frame(), make_pop_list_frame(),
		make_pop_help_frame(), make_pop_file_frame();

	/*	MENU_ACTION_PROC	Format
		caddr_t		menu_action_proc(menu, menu_item);
				Menu            menu;
				Menu_item       menu_item;
	*/
caddr_t		file_load_proc(), file_save_proc();
caddr_t		sort_menu_proc();
caddr_t		find_menu_proc();
caddr_t		undelete_menu_proc(), new_card_menu_proc();
caddr_t		delete_menu_proc();


	/*	MENU_NOTIFY_PROC	Format
		void	MENU_NOTIFY_PROC(menu, menu_item)
				Menu            menu;
				Menu_item       menu_item;
	*/
void		list_menu_proc(), help_menu_proc();
void		undelete_pop_menu_proc();

	/*	PANEL_NOTIFY_PROC	Format
		void	PANEL_NOTIFY_PROC((panel_item, event)
				Panel_item              panel_item;
				Event           *event;
	*/
			/* Slider */
void		slider_begin_proc(), slider_end_proc();
void		slider_random_proc(); /* (item, value , event ) */
			/* List	 */
void		add_to_list_proc(),	sub_from_list_proc();
			/* Undelete list */
/*
void		add_to_undel_list_proc(), sub_from_undel_list_proc();
*/
void		undel_before_proc(), undel_after_proc();

void		up_one_card_proc(), down_one_card_proc();
void		find_text_button_proc();
void		ok_file_proc();
void		jump_to_card();
