#ifdef IDENT_STRING
static char rcsid_pcthreads_h[]
    = "$Header: /pdd/wssw/archives/MTX/rcs/mit/lib/pcthreads/pcthreads.h,v 1.7 1992/06/29 20:03:09 chiba Exp $";
#endif

#ifdef  USE_MONITER_MTX
#include <mach.h>
#endif

#include <cthreads.h>

#define PTHREAD_PROCSYNC 1
#define PTHREAD_PROCASYNC 2

#define pthread_attr_default 3
#define pthread_mutexattr_default 4
#define pthread_condattr_default 3


#define pthread_t cthread_t
#define pthread_attr_t int
#define pthread_mutexattr_t int
#define pthread_condattr_t int
#define pthread_mutex_t struct mutex
#define pthread_cond_t struct condition
#define pthread_key_t int

#define DATAKEYS_MAX 5
