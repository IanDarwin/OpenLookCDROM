#ifndef	props_HEADER
#define	props_HEADER

/*
 * props_ui.h - User interface object and function declarations.
 * This file was generated by `gxv' from `props.G'.
 * DO NOT EDIT BY HAND.
 */

extern Attr_attribute	INSTANCE;


typedef struct {
	Xv_opaque	props_frame;
	Xv_opaque	props_control_panel;
	Xv_opaque	props_category;
	Xv_opaque	reset_button;
	Xv_opaque	done_button;
	Xv_opaque	other_panel;
	Xv_opaque	other_msg;
	Xv_opaque	def_priority;
	Xv_opaque	propagation_behavior;
	Xv_opaque	fore_chooser;
	Xv_opaque	fore_color;
	Xv_opaque	back_chooser;
	Xv_opaque	back_color;
	Xv_opaque	printing_panel;
	Xv_opaque	printing_msg;
	Xv_opaque	def_print_dest;
	Xv_opaque	def_printer;
	Xv_opaque	def_filename;
	Xv_opaque	def_print_mode;
	Xv_opaque	logging_panel;
	Xv_opaque	logging_msg;
	Xv_opaque	log_preference;
	Xv_opaque	log_info;
	Xv_opaque	log_filename;
	Xv_opaque	deadline_panel;
	Xv_opaque	deadline_actions;
	Xv_opaque	deadline_delete_units;
	Xv_opaque	deadline_delete_time;
	Xv_opaque	deadline_up_increment;
	Xv_opaque	deadline_down_increment;
	Xv_opaque	deadline_on_mail_to;
	Xv_opaque	deadline_after_mail_to;
	Xv_opaque	deadline_move_units;
	Xv_opaque	deadline_move_after;
	Xv_opaque	sorting_panel;
	Xv_opaque	sorting_msg;
	Xv_opaque	sort_level1;
	Xv_opaque	sort_level2;
	Xv_opaque	sort_level3;
	Xv_opaque	sorting_msg2;
	Xv_opaque	priority_direction;
	Xv_opaque	chron_direction;
} props_props_frame_objects;

extern props_props_frame_objects	*props_props_frame_objects_initialize();

extern Xv_opaque	props_props_frame_props_frame_create();
extern Xv_opaque	props_props_frame_props_control_panel_create();
extern Xv_opaque	props_props_frame_props_category_create();
extern Xv_opaque	props_props_frame_reset_button_create();
extern Xv_opaque	props_props_frame_done_button_create();
extern Xv_opaque	props_props_frame_other_panel_create();
extern Xv_opaque	props_props_frame_other_msg_create();
extern Xv_opaque	props_props_frame_def_priority_create();
extern Xv_opaque	props_props_frame_propagation_behavior_create();
extern Xv_opaque	props_props_frame_fore_chooser_create();
extern Xv_opaque	props_props_frame_fore_color_create();
extern Xv_opaque	props_props_frame_back_chooser_create();
extern Xv_opaque	props_props_frame_back_color_create();
extern Xv_opaque	props_props_frame_printing_panel_create();
extern Xv_opaque	props_props_frame_printing_msg_create();
extern Xv_opaque	props_props_frame_def_print_dest_create();
extern Xv_opaque	props_props_frame_def_printer_create();
extern Xv_opaque	props_props_frame_def_filename_create();
extern Xv_opaque	props_props_frame_def_print_mode_create();
extern Xv_opaque	props_props_frame_logging_panel_create();
extern Xv_opaque	props_props_frame_logging_msg_create();
extern Xv_opaque	props_props_frame_log_preference_create();
extern Xv_opaque	props_props_frame_log_info_create();
extern Xv_opaque	props_props_frame_log_filename_create();
extern Xv_opaque	props_props_frame_deadline_panel_create();
extern Xv_opaque	props_props_frame_deadline_actions_create();
extern Xv_opaque	props_props_frame_deadline_delete_units_create();
extern Xv_opaque	props_props_frame_deadline_delete_time_create();
extern Xv_opaque	props_props_frame_deadline_up_increment_create();
extern Xv_opaque	props_props_frame_deadline_down_increment_create();
extern Xv_opaque	props_props_frame_deadline_on_mail_to_create();
extern Xv_opaque	props_props_frame_deadline_after_mail_to_create();
extern Xv_opaque	props_props_frame_deadline_move_units_create();
extern Xv_opaque	props_props_frame_deadline_move_after_create();
extern Xv_opaque	props_props_frame_sorting_panel_create();
extern Xv_opaque	props_props_frame_sorting_msg_create();
extern Xv_opaque	props_props_frame_sort_level1_create();
extern Xv_opaque	props_props_frame_sort_level2_create();
extern Xv_opaque	props_props_frame_sort_level3_create();
extern Xv_opaque	props_props_frame_sorting_msg2_create();
extern Xv_opaque	props_props_frame_priority_direction_create();
extern Xv_opaque	props_props_frame_chron_direction_create();
#endif