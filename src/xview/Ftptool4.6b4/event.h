
#pragma ident   "@(#)event.h 1.2     93/01/13"

#ifdef USE_PROTOTYPES

void local_cd_select(void);
void local_cd_text(void);
void local_cd_dotdot(void);
void remote_cd_select(void);
void remote_cd_text(void);
void remote_cd_dotdot(void);
Notify_value destroy_func(Notify_client client, Destroy_status status);
Notify_value sig_func(void);
void cycle_busy_icon(void);
void start_busy_cycle(void);
void end_busy_cycle(void);
void props_event_proc(Panel panel, Event *event);
void base_event_proc(Xv_Window window, Event *event);
void resize_window(Panel panel, Panel_item list, Panel_item dismiss);
void local_event_proc(Xv_Window window, Event *event);
void schedule_event_proc(Xv_Window window, Event *event);
void host_event_proc(Xv_Window window, Event *event);
void send_noop_command(void);
void idle_timer_on(void);
void idle_timer_off(void);

#else

void local_cd_select();
void local_cd_text();
void local_cd_dotdot();
void remote_cd_select();
void remote_cd_text();
void remote_cd_dotdot();
Notify_value destroy_func();
Notify_value sig_func();
void cycle_busy_icon();
void start_busy_cycle();
void end_busy_cycle();
void props_event_proc();
void base_event_proc();
void resize_window();
void local_event_proc();
void schedule_event_proc();
void host_event_proc();
void send_noop_command();
void idle_timer_on();
void idle_timer_off();

#endif
