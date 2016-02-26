/********************************************************************************/
/* lib/unix.h --								*/
/*										*/
/* Author : A.J.Doherty								*/
/********************************************************************************/

#ifndef _FSPtoollib_UNIX_H_
#define _FSPtoollib_UNIX_H_ 1

#include "common.h"

/********************************************************************************/
/* The UnixLib part of FSPtoollib requires var-args so we need to include the	*/
/* stdarg.h variable argument list routines. Use of ANSI-C means this should be	*/
/* fairly standard. Any problems will have to be dealt with on a system by	*/
/* system basis.								*/

#include <stdarg.h>

/********************************************************************************/
/* Some systems don't define putenv function prototypes, if we know that sys	*/
/* has putenv then we can declare it ourselves in the form in which we use it	*/

#ifdef HAS_PUTENV
extern int putenv(char*);
#endif

/********************************************************************************/

extern void perrclose(FILE*);
extern void free_space(int*,int*);
extern void terminate_process(int);
extern void setenv_var(const char*);

extern long csectime(void);

extern FILE *perropen(char*[]);

extern char *c_strdup(const char*);
extern char *getenv_var(const char*);

extern char **string_to_args(const char*,const char*);

extern void *c_calloc(size_t,int);
extern void *c_malloc(size_t);

extern pid_t unix_spawn(char*[]);
extern pid_t unix_command(char*[]);

extern void  register_error_handler(void (*erhandlr)(int,...));
extern void  default_error_handler(int,...);
extern void  initialise_fsptool_lib(void);

/********************************************************************************/
#endif
