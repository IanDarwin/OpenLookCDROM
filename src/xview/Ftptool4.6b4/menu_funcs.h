
#pragma ident   "@(#)menu_funcs.h 1.2     93/01/13"

#ifdef USE_PROTOTYPES

Menu file_menu_gen(Menu m, Menu_generate op);
Menu file_copy_menu_gen(Menu m, Menu_generate op);
Menu file_delete_menu_gen(Menu m, Menu_generate op);
Menu view_menu_gen(Menu m, Menu_generate op);
void change_local_list_menu(void);
void change_remote_list_menu(void);
Menu props_menu_gen(Menu m, Menu_generate op);
Menu send_list_menu_gen(Menu m, Menu_generate op);
Menu receive_list_menu_gen(Menu m, Menu_generate op);
Menu host_menu_gen(Menu m, Menu_generate op);

#else

Menu file_menu_gen();
Menu file_copy_menu_gen();
Menu file_delete_menu_gen();
Menu view_menu_gen();
void change_local_list_menu();
void change_remote_list_menu();
Menu props_menu_gen();
Menu send_list_menu_gen();
Menu receive_list_menu_gen();
Menu host_menu_gen();

#endif
