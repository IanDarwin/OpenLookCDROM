
#pragma ident   "@(#)schedule.h 1.3     93/05/25"

#ifdef USE_PROTOTYPES

struct schedule *new_schedule(char *menu_name, int direction,
	time_t date, time_t repeat_minutes, char *filename,
	struct hostlist *hl);
struct schedule *add_schedule(struct schedule *head, char *menu_name,
	int direction, time_t date, time_t repeat_minutes,
	char *filename, struct hostlist *hl);
void free_schedule(struct schedule *head);
void delete_schedule(struct schedule *head, char *menu_name, int direction);
void reorder_list(struct schedule *head);
void load_schedule(void);
void set_current_schedule_proc(Menu menu, Menu_item menu_item);
int schedule_exists(struct schedule *head, char *menu_name,
	int direction);
void enter_schedule_info(int warnchange);
void schedule_add_proc(Menu menu, Menu_item menu_item);
void schedule_change_proc(Menu menu, Menu_item menu_item);
void schedule_delete_proc(Menu menu, Menu_item menu_item);
void schedule_item_proc(Menu menu, Menu_item menu_item);
void reload_schedule_menu(struct schedule *head);
void action_choice_proc(Panel_item item, unsigned int value,
	Event *event);
Notify_value schedule_timer_proc(void);
void activate_schedule_timer(void);
void batch_process_proc(Panel_item item, Event *event);
void doschedule(void);

#else

struct schedule *new_schedule();
struct schedule *add_schedule();
void free_schedule();
void delete_schedule();
void reorder_list();
void load_schedule();
void set_current_schedule_proc();
int schedule_exists();
void enter_schedule_info();
void schedule_add_proc();
void schedule_change_proc();
void schedule_delete_proc();
void schedule_item_proc();
void reload_schedule_menu();
void action_choice_proc();
Notify_value schedule_timer_proc();
void activate_schedule_timer();
void batch_process_proc();
void doschedule();

#endif
