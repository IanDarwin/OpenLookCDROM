
#pragma ident   "@(#)create_main.h 1.2     93/01/13"

#ifdef USE_PROTOTYPES

void create_base_window(void);
void create_property_window(void);
void create_local_window(void);

#else

void create_base_window();
void create_property_window();
void create_local_window();

#endif
