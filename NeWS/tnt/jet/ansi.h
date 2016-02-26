#if defined(__STDC__) || defined(__cplusplus)
# define _P(s) s
#else
# define _P(s) ()
#endif


/* gen_parse.c */
int init_parse _P((void));
int ansi_mode _P((void));
int vt52_mode _P((void));

/* main.c */
int main _P((int argc, char *argv[]));
void ready_handler _P((int tag, caddr_t data));
void persistent_handler _P((int tag, caddr_t data));
void ps_death _P((int tag, caddr_t data));
void keyboard_handler _P((int tag, caddr_t caddr));
void string_handler _P((int tag, caddr_t data));
void reset_handler _P((int tag, caddr_t data));
void apply_handler _P((int tag, caddr_t data));
void save_defaults_handler _P((int tag, caddr_t data));
void send_args _P((void));
int build_wire _P((void));

/* parse.c */
void set_saved_lines _P((int val));
int init_term _P((int rows, int cols));
void input_handler _P((void));
void config_size _P((void));
void repaint_handler _P((void));
void reshape_handler _P((void));
void set_mode_handler _P((void));

/* pty.c */
int get_ptys _P((void));
void load_pty _P((void));
int spawn_shell _P((char *cmd, int is_console, int is_login));
void fill_out_utmp _P((void));
void clear_utmp _P((void));
int pty_set_size _P((int fd, int x, int y));

/* select.c */
void invert_on _P((void));
void invert_off _P((void));
void selection_start_handler _P((void));
void find_handler _P((int tag, caddr_t data));
void init_word_syntax _P((void));
void selection_motion_handler _P((void));
void selection_stop_handler _P((void));
void update_handler _P((void));
void quick_finish _P((void));
int finish_scrolling _P((void));
void erase_cursor _P((void));
void draw_cursor _P((void));
void repair_cursor _P((void));

/* tnt.c */
void define_package_c _P((void));
void enter_package_c _P((int *ret));
void ps_set_tag_c _P((char *a, int b));
void ps_init_ps_c _P((char *font_name, int font_size, int retained, int scrollbar_side, int autoscale, int cols, int c, int r));
void ps_show_at_c _P((int x, int y, char_type *str, int len, attribute_type *ats));
void ps_reset_canvas_c _P((void));
void ps_clear_region_c _P((int x1, int y1, int x2, int y2));
int ps_scroll_region_c _P((int y_1, int y_2, int n));
int ps_clear_screen_c _P((void));
int ps_invert_box_c _P((int x1, int y1, int x2, int y2));
void ps_insert_char_c _P((int x, int y, int n, int len));
void ps_delete_char_c _P((int x, int y, int n, int len));
int ps_bell_c _P((void));
int ps_set_selection_c _P((char *str));
int to_char_x _P((double x));
int to_char_y _P((double y));
void text_size_handler _P((void));
int ps_set_size_c _P((int x, int y));
int ps_flush_PostScript_c _P((void));
int ps_scroll_bottom_c _P((void));
int ps_set_view_c _P((int x, int y));
int ps_set_defaults_c _P((char *font_name, int font_size, int retained, int scrollmode, int scrollbar_side, char *term_id, int autoscale, int cols, int saved_lines));
int ps_apply_c _P((void));
int ps_cancel_selection_c _P((void));
int ps_set_pins_c _P((int pin_x, int pin_y, int last_x, int last_y));
int ps_set_label_c _P((char *str));
int ps_set_icon_label_c _P((char *str));
int ps_draw_cursor_c _P((int x, int y));
int ps_erase_cursor_c _P((void));
int ps_put_char_c _P((int c));
int ps_new_clip_c _P((void));
int ps_set_clip_c _P((void));
int ps_namedictbegin_c _P((char *str));
int ps_dictenddef_c _P((void));
int ps_namebooleandef_c _P((char *name, int value));
int ps_nameintdef_c _P((char *name, int value));
int ps_namerealdef_c _P((char *name, double value));
int ps_namestringdef_c _P((char *name, char *value));
int ps_namenamedef_c _P((char *name, char *value));
int ps_ready_c _P((void));

#undef _P
