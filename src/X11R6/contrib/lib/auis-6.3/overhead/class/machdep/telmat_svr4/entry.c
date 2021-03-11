#include "class.h"
#include <setjmp.h>
extern long class_ErrorReturnValue;
extern int doload_trace;

#define ClassEntry(num) \
  void* ClassEntry ## num (void *header_addr, void* p1, void* p2, void* p3, void* p4, void* p5, void* p6, void* p7, void* p8) \
{ \
    void *lp1 = p1; \
    void *lp2 = p2; \
    void *lp3 = p3; \
    void *lp4 = p4; \
    void *lp5 = p5; \
    void *lp6 = p6; \
    void *lp7 = p7; \
    void *lp8 = p8; \
    void *res; \
    struct classinfo *classinfo_addr; /* adresse de la structure classinfo \
				     contenant la fonction a chercher */ \
    struct classheader *cl = (struct classheader*)(header_addr); \
    if ((cl == NULL) || (classinfo_addr=class_Load(cl->name)) == NULL) \
	return (void*)class_ErrorReturnValue; \
    cl->classprocedures = classinfo_addr->procs; \
    if (doload_trace > 1) { \
      fprintf(stderr,"   num(%d) head(0x%x) name(%s) clproc(0x%x)\n",num,cl,cl->name,cl->classprocedures); \
      fprintf(stderr,"           info(0x%x) name(%s) procs(0x%x)\n",classinfo_addr, classinfo_addr->name,classinfo_addr->procs); \
   fprintf(stderr,"           lp1(0x%x) lp2(0x%x) lp3(0x%x)\n",lp1,lp2,lp3); \
   } \
    res = (void*) ((long(*)())((classinfo_addr->procs->routines[(num)]))) \
                             (cl, lp1, lp2, lp3, lp4, lp5, lp6, lp7, lp8); \
    return res; \
}

#include <../common/entrydefs.h>

/*    return (void*) ((long(*)())((classinfo_addr->procs->routines[(num)]))) \
      (cl,(void*)((long(*)())((classinfo_addr->procs->routines[0]))) (cl,cl->versionnumber), lp1, lp2, lp3, lp4, lp5, lp6, lp7, lp8) ; \*/
/*    return (void*) ((long(*)())((classinfo_addr->procs->routines[(num)]))) (header_addr, lp1, lp2, lp3, lp4, lp5, lp6, lp7, lp8, lp9, lp10) ; */

