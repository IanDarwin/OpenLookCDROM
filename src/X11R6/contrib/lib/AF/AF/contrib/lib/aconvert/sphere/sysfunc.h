/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* File: sysfunctions.h */


/* C Library functions */

/*extern char	*index(), *rindex();*/
extern double	atof();
extern long	atol(), ftell();
extern int	atoi(), getopt();


/* Support library functions */

int spx_read_header();
struct field_t **spx_get_field_vector();
int spx_copy_field_vector();
struct header_t *spx_allocate_header();
int spx_deallocate_header();
struct field_t *spx_allocate_field();
struct field_t *spx_allocate_field_str();
int spx_deallocate_field();
int spx_tp();

extern char *spx_malloc();
extern int spf_free();
