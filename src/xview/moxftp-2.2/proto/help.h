static void Help_Once_action P_((Widget, XEvent*, String*, Cardinal*));
static void Help_Once_cb P_((Widget, char*, caddr_t));
static void Help_by_title_cb P_((Widget, char*, caddr_t));
static void Help_register_cb P_((Widget, char*, caddr_t));
static void Set_Help_Title_cb P_((Widget, char*, caddr_t));
static void Set_help_action P_((Widget, XEvent*, String*, Cardinal*));
static void Set_help_cb P_((Widget, char*, caddr_t));
static void clear_help P_((Widget, char*, char*));
static int compare_menu P_((void*, void*));
static void help_once P_((Widget));
static void set_help_text P_((Widget));
static void set_the_help_text P_((Widget, char*));
static int title_type P_((char*));