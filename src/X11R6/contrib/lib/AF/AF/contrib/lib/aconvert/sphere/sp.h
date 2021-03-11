/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* File: functions.h */

/* User library functions */

struct header_t *sp_open_header();
struct header_t *sp_create_header();
int sp_close_header();

FILE *sp_get_fp();
int sp_set_fp();

int sp_get_hbytes();
int sp_get_nfields();
int sp_get_fieldnames();

int sp_print_lines();
int sp_format_lines();
int sp_fpcopy();

int sp_get_field();
int sp_get_data();
int sp_get_type();
int sp_get_size();

int sp_clear_fields();
int sp_add_field();
int sp_delete_field();
int sp_change_field();

int sp_is_std();

extern char *std_fields[];

int sp_set_dealloc();
int sp_get_dealloc();
