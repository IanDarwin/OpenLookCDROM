
#pragma ident   "@(#)dnd.h 1.3     93/05/25"

#ifdef USE_PROTOTYPES
void remote_drop(Xv_Window server, Event *event, Selection_requestor sel);
void remote_list_event(Panel_item item, Event *event);
void local_drop(Xv_Window server, Event *event, Selection_requestor sel);
void local_list_event(Panel_item item, Event *event);
void get_remote_list_event_proc(void);
void get_local_list_event_proc(void);
#else
void remote_drop();
void remote_list_event();
void local_drop();
void local_list_event();
void get_remote_list_event_proc();
void get_local_list_event_proc();
#endif
