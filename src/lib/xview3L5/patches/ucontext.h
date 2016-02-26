#ifndef _SYS_UCONTEXT_H
#define	_SYS_UCONTEXT_H

/* Dummy sys/ucontext.h for linux, to satisfy xview sources
 * NOTE! THIS IS A DUMMY, DO NOT USE */

#include <sys/types.h>
#include <sys/signal.h>

typedef struct ucontext {
	sigset_t	uc_sigmask;
} ucontext_t;

void savecontext(ucontext_t *, sigset_t);
int restorecontext(ucontext_t *);

#endif /* _SYS_UCONTEXT_H */
