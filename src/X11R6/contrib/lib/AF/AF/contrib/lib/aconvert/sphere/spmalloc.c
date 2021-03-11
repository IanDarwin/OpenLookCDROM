/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* LINTLIBRARY */

/* File: spmalloc.c */


#include <sys/types.h>
#include "sysparam.h"


static int dealloc = 1;

char *spx_malloc(bytes)
u_int bytes;
{
extern char *malloc();

if (bytes == 0)
	bytes = 1;
return malloc(bytes);
}

spx_free(p)
char *p;
{
if (dealloc)
	(void) free(p);
}

FUNCTION sp_set_dealloc(n)
int n;
{
dealloc = n;
}

FUNCTION int sp_get_dealloc()
{
return dealloc;
}
