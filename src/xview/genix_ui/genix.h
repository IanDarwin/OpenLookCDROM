/* Global information for genix program */

#define	MAXPEOPLE 1000
#define MAXCOUPLES 750
#define PRTNAMELEN 20		/* used in scrolling lists */
#define SNOFFSET 8		/* i.e., ~1/3 of PRTNAMELEN for anglo names */

#include <sys/types.h>
#include "objects.h"

extern int Changed;
extern char *CurrentFileName;
extern Person root;
extern Person *allp[];
extern int np;
extern Couple *allc[];
extern int nc;

extern char *emalloc(off_t);
extern void error(...), warning(...);
