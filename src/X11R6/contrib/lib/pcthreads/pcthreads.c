/* $XConsortium$ */

#ifdef IDENT_STRING
static char rcsid_pcthreads_c[]
    = "$Header: /pdd/wssw/archives/MTX/rcs/mit/lib/pcthreads/pcthreads.c,v 1.13 1992/10/23 22:02:40 smithj Exp $";
#endif

/*
 * pcthreads.c
 *
 * POSIX pthreads to MACH cthreads ininterface routines
 */

/*

Copyright (c) 1989  X Consortium
Copyright 1992 by Data General Corporation, Westborough, Massachusetts,
and OMRON Corporation.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/

#include <stdio.h>

#include "pcthreads.h"

#ifndef Xalloc
#ifndef xalloc
extern long *Xalloc (), *Xrealloc ();
extern void Xfree();
#endif
#endif


/******* KEY MECHANISM *********
 *
 * pthread_setspecific must be called before calling pthread_getspecific.
 * pthread_exit must be called before a thread exits.
 *
 */
static int next_key = 0;
static pthread_mutex_t Key_mutex;
static pthread_mutex_t PerThreadData_mutex;
/*
 * PerThreadData_mutex is used in order to guarantee a child thread doesn't
 * execute cthread_data until its parent thread execute cthread_set_data.
 *
 */

static pthread_mutex_t KeyThreadFreeList_mutex;
static void (*DestructorArray[DATAKEYS_MAX])();

typedef struct key_thread{
    void *KeyValue[DATAKEYS_MAX];
    struct key_thread *next;
} KeyThreadRec, *KeyThreadPtr;

#define MAX_KEY_FREE_LIST 30

static KeyThreadPtr pKeyThreadFreeList = NULL;

static KeyThreadPtr GetKeyThreadFromFreeList();
static void ReturnKeyThreadToFreeList();
/**********************************/


#ifdef  USE_MONITOR_MTX
#define	MAX_THREAD	512
#define MAX_SYMBOL	512*(MAX_MUTEX+10)
#define	MAX_MUTEX	5

typedef any_t	(* any_t_func)();

/*
** the monitor threads id
*/
static cthread_t   monitor_thread;

/*
** threads infomation table
*/
static struct _pthread_info_ {
	cthread_t	parent;	  /* parent thread's id */
	cthread_t	self;	  /* thread's id */
	any_t		(* function_ptr)(); /* entry point address */
	any_t		arg;	  /* argment */
	int		status;	  /* indicates the thread's situation */
#define	S_NONE			0 /* normal situation */
#define	S_TRY_MUTEX_LOCK	1 /* tried to lock a mutex but coudn't. */
#define S_CONDITION_WAIT	2 /* waiting a signal on a condition variable. */
#define S_EXEC_C_SIGNAL		3 /* sending a signal. */
#define S_EXEC_C_BROADCAST	4 /* sending a broadcast signal. */
	mutex_t		mutex;	              /* waiting a mutex variable */
	condition_t	condition;	      /* waiting a condition variable */
	mutex_t		lock_list[MAX_MUTEX]; /* list of the locking mutex */
} pthreads_info[MAX_THREAD+1];

static int			pthreads_count;
struct mutex			pthreads_info_lock;

/*
** thread monitor symbol table
*/
static struct _name_info_ {
	any_t addr;	/* symbol addres */
	char *name;	/* symbol name   */
} name_info[MAX_SYMBOL];

static int name_count = 0;

#define pthread_info_clear( i )		\
{ int j; \
	pthreads_info[i].parent       = 0; \
	pthreads_info[i].self         = 0; \
	pthreads_info[i].function_ptr = 0; \
	pthreads_info[i].arg	      = 0; \
	pthreads_info[i].status	      = S_NONE; \
	pthreads_info[i].mutex        = 0; \
	pthreads_info[i].condition    = 0; \
	for(j = 0; j < MAX_MUTEX; j++)  \
		pthreads_info[i].lock_list[j] = 0; \
}

#ifdef	USE_CTHREAD_SET_DATA
#define pthread_set_id( i ) \
	pthreads_info[i].self = cthread_self(); \
	cthread_set_data(pthreads_info[i].self,(any_t)i); 

#define pthread_get_id() \
	(int)cthread_data(cthread_self())

#else
#define pthread_set_id( i ) \
	pthreads_info[i].self = cthread_self();

int
pthread_get_id()
{
	int	i;
	cthread_t self = cthread_self();

	for(i = 0; i < MAX_THREAD; i++) 
		if(pthreads_info[i].self == self) 
			break;

        if(i == MAX_THREAD)
		fprintf(stderr,"pthread_get_id():bad thread id\n");

	return i;
}

int
get_id_from_thread(id)
cthread_t id;
{
	int	i;

	for(i = 0; i < MAX_THREAD; i++) 
		if(pthreads_info[i].self == id) 
			break;

        if(i == MAX_THREAD)
		fprintf(stderr,"get_id_from_thread():bad thread id\n");

	return i;
}
#endif

void pthread_set_symname(addr, name)
any_t addr;
char *name;
{
	int	i;

	for(i = 0; i < name_count; i++) {
		if(name_info[i].addr == addr) return;
	}

	if(name_count < MAX_SYMBOL) {
		name_info[name_count].name = name;
		name_info[name_count].addr = addr;
		name_count++;
	} else {
		fprintf(stderr,"pthread_set_symname():table overflow\n");
	}
}

static void pthread_symname(addr, buf, len)
any_t addr;
char *buf;
int len;
{
	int	i;

	if(addr == 0) {
		strncpy(buf, "                    ", len);
	} else {
		for(i = 0; i < name_count; i++) {
			if(name_info[i].addr == addr) {
				break;
			}
		}

		if( i >= name_count ) {
			strncpy(buf, "????????????????????", len);
		} else {
			strncpy(buf, name_info[i].name, len);
		}
	}
	buf[len] = 0;
}



static void pthread_monitor_list_thread()
{
	cthread_t cparent;
	int	parent;
	any_t	p;
	int	i, j;
	char	s_parent[20];
	char	s_self[20];
	char	s_mutex[32];
	char	s_con[32];

	j = 0;

	for(i = 0; i < MAX_THREAD; i++) {
		if(pthreads_info[i].self) {
			j++;
			if(cparent = pthreads_info[i].parent) {
#ifdef  USE_CTHREAD_SET_DATA
				parent = (int)cthread_data(cparent);
#else
				parent = get_id_from_thread(cparent);
#endif
				p = (any_t)pthreads_info[parent].function_ptr;
			} else {
				p = 0;
			}

			pthread_symname(p, s_parent, 6);

			p = (any_t)pthreads_info[i].function_ptr;
			pthread_symname(p, s_self, 6);

			fprintf(stderr, "%3d %-6s(%08x) %-6s(%08x)", j,
				s_parent, pthreads_info[i].parent,
				s_self, pthreads_info[i].self);

			switch(pthreads_info[i].status) {
			case S_NONE:
				fprintf(stderr, " RUN\n");
				break;

			case S_TRY_MUTEX_LOCK:
				p = (any_t)pthreads_info[i].mutex;
				pthread_symname(p, s_mutex, 8);
				fprintf(stderr, 
				  " TRY MUTEX LOCK : mutex = %-8s(%08x)\n",
				  s_mutex, pthreads_info[i].mutex);
				break;

			case S_CONDITION_WAIT:
				p = (any_t)pthreads_info[i].mutex;
				pthread_symname(p, s_mutex, 8);
				fprintf(stderr, 
				  " CONDITION WAIT : mutex = %-8s(%08x),",
				  s_mutex, pthreads_info[i].mutex);

				p = (any_t)pthreads_info[i].condition;
				pthread_symname(p, s_con, 8);
				fprintf(stderr, " condi = %-8s(%08x) \n",
					s_con, pthreads_info[i].condition);
				break;

			case S_EXEC_C_SIGNAL:
				p = (any_t)pthreads_info[i].condition;
				pthread_symname(p, s_con, 8);
				fprintf(stderr,
				  "SEND SIGNAL     : condi =  %-8s(%08x) \n",
				  s_con, pthreads_info[i].condition);
				break;

			case S_EXEC_C_BROADCAST:
				p = (any_t)pthreads_info[i].condition;
				pthread_symname(p, s_con, 8);
				fprintf(stderr,
				  "SEND BROADCAST  : condi =  %-8s(%08x) \n",
					s_con, pthreads_info[i].condition);
				break;
			}
		}
	}
}

static char	*help_text[] = {
" The Thread Monitor has the following commands. ",
" t - Displays every client's situation.         ",
" m - Displays all mutex variables locked at the moment.",
" h - Displays a list of commands.",
" q - Sends KILL signal to itself, and terminates the program.",
" ",
0 };

static void pthread_monitor_list_help()
{
	char **p = help_text;

	while(*p) {
		fprintf(stderr,"%s\n",*p);
		p++;
	}
}


static void pthread_monitor_list_mutex()
{
	cthread_t cparent;
	int	parent;
	any_t	p;
	int	i, j, k;
	char	s_self[20];
	char	s_mutex[32];
	char	s_con[32];

	j = 0;

	for(i = 0; i < MAX_THREAD; i++) {
		if(pthreads_info[i].self) {
			j++;
			p = (any_t)pthreads_info[i].function_ptr;
			pthread_symname(p, s_self, 6);
			fprintf(stderr, "%3d THREAD %-6s(%08x)\n\tMUTEX LOCK : ", j,
				s_self, pthreads_info[i].self);
			for(k = 0; k < MAX_MUTEX; k++) {
				if(pthreads_info[i].lock_list[k]) {
					p = (any_t)pthreads_info[i].lock_list[k];
					pthread_symname(p, s_mutex, 8);
					fprintf(stderr, " %-8s(%08x) ",
					 	s_mutex, p);
				}
			}
			fprintf(stderr,"\n");
		}
	}
}

static void pthread_monitor()
{
	FILE *input;
	char input_buf[128];
	char *p;
	int  c;

	if((input = fopen("/dev/tty", "r")) == NULL) {
		fprintf(stderr, "can't open /dev/tty\n");
	    	fflush(stderr);
		cthread_exit(1);
	}
		
	while(1) {
		fprintf(stderr,"#pthread_monitor> ");
		fflush(stderr);
		p = input_buf;
		while((c = fgetc(input)) != '\n') {
			if(c == EOF) break;
			*p++ = c;
		}
		*p = NULL;

		p = input_buf;
		while(1) {
			c = *p++;
			if(c != ' ' && c != '\t') break;
		}

		switch( c ) {
		case 't': /* list thread */
			mutex_lock( &pthreads_info_lock );
			fprintf(stderr, " noumber of pthread = %d\n",
				pthreads_count );
			pthread_monitor_list_thread();
			mutex_unlock( &pthreads_info_lock );
			break;
		case 'm': /* list mutex */
			mutex_lock( &pthreads_info_lock );
			fprintf(stderr, " noumber of pthread = %d\n",
				pthreads_count );
			pthread_monitor_list_mutex();
			mutex_unlock( &pthreads_info_lock );
			break;
		case 'h':
                case '?':
			pthread_monitor_list_help();
			break;

		case 'q': /* quit */
			mutex_clear(&pthreads_info_lock);
			fclose(input);
                        kill(getpid(),9);
			cthread_exit(1);
		}
	}
}
#else
void pthread_set_symname(addr, name)
{
}
#endif


void pthread_init(mode)
int mode;
{
    KeyThreadPtr pKeyThread;

#ifdef  USE_MONITOR_MTX
	int	i;
	extern	int main();
#endif

    cthread_init();

    KeyInit();

#ifdef  USE_MONITOR_MTX
	for(i = 0; i < MAX_THREAD; i++) {
		pthread_info_clear( i );
	}

	pthreads_count = 1;

	pthreads_info[0].parent	      = 0;
	pthreads_info[0].function_ptr = (any_t_func)main;
	pthreads_info[0].arg 	      = 0;
	pthread_set_id(0);
#endif

    pKeyThread = GetKeyThreadFromFreeList();
    if (!pKeyThread)
    {
        ErrorF("alloc failed in pthread_create\n");
        return;
    }
    cthread_set_data(cthread_self(), (char *)pKeyThread);

#ifdef  USE_MONITOR_MTX
	mutex_init(&pthreads_info_lock);

        monitor_thread = cthread_fork((any_t_func)pthread_monitor, 0);

        if (monitor_thread == 0)
        {
           	fprintf(stderr,"pthread_monitor create failed\n");
		fflush(stderr);
        }

        cthread_detach(&monitor_thread);
#endif
}


#ifdef  USE_MONITOR_MTX
static any_t pthread_exit_self( i )
int	i;
{
	any_t t;
        any_t p;
        char  s_self[8];

	pthread_set_id(i);

#ifndef UNUSE_CTHREAD_SET_NAME
	p = (any_t)pthreads_info[i].function_ptr;
	pthread_symname(p, s_self, 6);

	cthread_set_name(pthreads_info[i].self, s_self);
#endif

	t = (*(pthreads_info[ i ].function_ptr))( pthreads_info[ i ].arg );

	mutex_lock( &pthreads_info_lock );

	pthreads_count--;
	pthread_info_clear( i );

	mutex_unlock( &pthreads_info_lock );

	return t;
}
#endif

int
pthread_create(threadptr,attr,funcptr,arg)
    cthread_t *threadptr;
    pthread_attr_t attr;
    any_t (*funcptr)();
    any_t arg;
{
#ifdef  USE_MONITOR_MTX
	int	i;
#endif
    KeyThreadPtr pKeyThread;

    pKeyThread = GetKeyThreadFromFreeList();
    if (!pKeyThread){
        ErrorF("alloc failed in pthread_create\n");
        return -1;
    }

    pthread_mutex_lock(&PerThreadData_mutex);


#ifdef  USE_MONITOR_MTX
	mutex_lock( &pthreads_info_lock );

	for(i = 0; i < MAX_THREAD; i++) {
		if(pthreads_info[i].self == 0) {
			pthreads_info[i].self  = (cthread_t)1;
			pthreads_count++;
			break;
		}
	}

	mutex_unlock( &pthreads_info_lock );

	if(i < MAX_THREAD) {
		pthreads_info[i].parent       = cthread_self();
		pthreads_info[i].function_ptr = funcptr;
		pthreads_info[i].arg          = arg;

		*threadptr = cthread_fork((any_t *)pthread_exit_self, i);
	} else {
		*threadptr = cthread_fork((any_t *)funcptr, arg);
	}
#else
	*threadptr = cthread_fork((any_t *)funcptr,arg);
#endif
    cthread_set_data(*threadptr, (char *)pKeyThread);
    pthread_mutex_unlock(&PerThreadData_mutex);

	return 0;
}


int pthread_detach(thread)
pthread_t *thread;
{
	cthread_detach(*thread);
	return 0;
}


int pthread_mutex_lock(m)
mutex_t m;
{
#ifdef  USE_MONITOR_MTX
	int i = pthread_get_id();
	int j;
#endif

#ifdef  USE_MONITOR_MTX
	pthreads_info[i].mutex   = m;
	pthreads_info[i].status  = S_TRY_MUTEX_LOCK;
#endif

	mutex_lock(m);

#ifdef  USE_MONITOR_MTX
	pthreads_info[i].status  = S_NONE;
	for(j = 0; j < MAX_MUTEX; j++) {
		if(pthreads_info[i].lock_list[j] == 0) {
			pthreads_info[i].lock_list[j] = m;
			break;
		}
	}
#endif
	return 0;
}


int pthread_mutex_unlock(m)
mutex_t m;
{
#ifdef  USE_MONITOR_MTX
	int i = pthread_get_id();
	int j;
#endif

	mutex_unlock(m);

#ifdef  USE_MONITOR_MTX
	for(j = 0; j < MAX_MUTEX; j++) {
		if(pthreads_info[i].lock_list[j] == m) {
			pthreads_info[i].lock_list[j] = 0;
			break;
		}
	}
#endif
	return 0;
}


int pthread_mutex_trylock(m)
mutex_t m;
{
#ifdef  USE_MONITOR_MTX
	int i = pthread_get_id();
	int j, t;

	if(t = mutex_try_lock(m)) {;
		for(j = 0; j < MAX_MUTEX; j++) {
			if(pthreads_info[i].lock_list[j] == 0) {
				pthreads_info[i].lock_list[j] = m;
				break;
			}
		}
	}
	return t;
#else
	return mutex_try_lock(m);
#endif
}


int pthread_mutex_init(m,attr)
mutex_t m;
int attr;
{
	mutex_init(m);
	return 0;
}


int pthread_cond_init(c,attr)
condition_t c;
int attr;
{
	condition_init(c);
	return 0;
}


int pthread_cond_signal(c)
condition_t c;
{
#ifdef  USE_MONITOR_MTX
	int i = pthread_get_id();

	pthreads_info[i].condition = c;
	pthreads_info[i].status    = S_EXEC_C_SIGNAL;
#endif
	condition_signal(c);

#ifdef  USE_MONITOR_MTX
	pthreads_info[i].status    = S_NONE;
#endif
	return 0;
}


int pthread_cond_broadcast(c)
condition_t c;
{
#ifdef  USE_MONITOR_MTX
	int i = pthread_get_id();

	pthreads_info[i].condition = c;
	pthreads_info[i].status    = S_EXEC_C_BROADCAST;
#endif

	condition_broadcast(c);

#ifdef  USE_MONITOR_MTX
	pthreads_info[i].status    = S_NONE;
#endif
	return 0;
}


int pthread_cond_wait(c,m)
condition_t c;
mutex_t m;
{
#ifdef  USE_MONITOR_MTX
	int i = pthread_get_id();

	pthreads_info[i].mutex     = m;
	pthreads_info[i].condition = c;
	pthreads_info[i].status    = S_CONDITION_WAIT;
#endif
	condition_wait(c,m);

#ifdef  USE_MONITOR_MTX
	pthreads_info[i].mutex = 0;
	pthreads_info[i].condition = 0;
	pthreads_info[i].status    = S_NONE;
#endif
	return 0;
}


int pthread_attr_create(attr)
pthread_attr_t *attr;
{
	*attr = 1;
	return(0);
}


int pthread_mutexattr_create(attr)
pthread_mutexattr_t *attr;
{
	*attr = 1;
	return(0);
}


int pthread_condattr_create(attr)
pthread_condattr_t *attr;
{
	*attr = 1;
	return(0);
}


void pthread_yield()
{
	cthread_yield();
}


static int
CreateKeyThreadFreeList()
{
    KeyThreadPtr pKeyThread, pCurrentKeyThread;
    register int i;

    pKeyThread = (KeyThreadPtr)Xalloc( sizeof(KeyThreadRec) *
                                      MAX_KEY_FREE_LIST);
    if (!pKeyThread){
        ErrorF("alloc error in CreateKeyThreadFreeList\n");
        return -1;
    }

    pCurrentKeyThread = pKeyThread;

    for (i = 0; i < MAX_KEY_FREE_LIST - 1; i++){
        pCurrentKeyThread->next = pCurrentKeyThread + 1;
        pCurrentKeyThread++;
    }
    pCurrentKeyThread->next = pKeyThreadFreeList;

    pKeyThreadFreeList = pKeyThread;

    return 0;
}


static int
KeyInit()
{
    int i;

    for (i = 0; i < DATAKEYS_MAX; i++)
        DestructorArray[i] = NULL;

    pthread_mutex_init(&Key_mutex, pthread_mutexattr_default);
    pthread_mutex_init(&PerThreadData_mutex, pthread_mutexattr_default);
    pthread_mutex_init(&KeyThreadFreeList_mutex, pthread_mutexattr_default);

    CreateKeyThreadFreeList();

    return 0;
}


static KeyThreadPtr
GetKeyThreadFromFreeList()
{
    KeyThreadPtr pKeyThread;

    if (!pKeyThreadFreeList){
        CreateKeyThreadFreeList();
    }

    pthread_mutex_lock(&KeyThreadFreeList_mutex);
    pKeyThread = pKeyThreadFreeList;
    pKeyThreadFreeList = pKeyThreadFreeList->next;
    pthread_mutex_unlock(&KeyThreadFreeList_mutex);

    return pKeyThread;
}


static void
ReturnKeyThreadToFreeList(pKeyThread)
    KeyThreadPtr pKeyThread;
{
    pthread_mutex_lock(&KeyThreadFreeList_mutex);
    pKeyThread->next = pKeyThreadFreeList;
    pKeyThreadFreeList = pKeyThread;
    pthread_mutex_unlock(&KeyThreadFreeList_mutex);
}


void pthread_exit(stat)
any_t *stat;
{
#ifdef  USE_MONITOR_MTX
	int	i = pthread_get_id();
#endif

    register int key;
    void *value;

    for (key = next_key - 1; key >= 0; key--){
        if ( DestructorArray[key] ){
            pthread_mutex_lock(&PerThreadData_mutex);
            pthread_getspecific(key, &value);
            pthread_mutex_unlock(&PerThreadData_mutex);
            (*DestructorArray[key])(value);
        }
    }
    pthread_mutex_lock(&PerThreadData_mutex);
    ReturnKeyThreadToFreeList( (KeyThreadPtr)cthread_data(cthread_self()) );
    pthread_mutex_unlock(&PerThreadData_mutex);

#ifdef  USE_MONITOR_MTX
	mutex_lock( &pthreads_info_lock );
	pthreads_count--;
	pthread_info_clear( i );
	mutex_unlock( &pthreads_info_lock );
#endif
	cthread_exit(stat);
}

int
pthread_key_create(pKey, destructor)
    pthread_key_t  *pKey;
    void (*destructor)();
{
    pthread_mutex_lock(&Key_mutex);
    *pKey = next_key++;
    pthread_mutex_unlock(&Key_mutex);

    if (*pKey >= DATAKEYS_MAX){
        return -1;
    }

    DestructorArray[*pKey] = destructor;

    return 0;
}


int
pthread_setspecific(key, value)
    pthread_key_t  key;
    void *value;
{
    KeyThreadPtr pKeyThread;

    pthread_mutex_lock(&PerThreadData_mutex);
    pKeyThread = (KeyThreadPtr)cthread_data(cthread_self());
    pthread_mutex_unlock(&PerThreadData_mutex);
    if (!pKeyThread)
        ErrorF("SET SPECIFIC error\n");

    (pKeyThread->KeyValue)[key] = value;

    return 0;
}


int
pthread_getspecific(key, value)
    pthread_key_t  key;
    void  **value;
{
    KeyThreadPtr pKeyThread;

    pKeyThread = (KeyThreadPtr)cthread_data(cthread_self());
    if (!pKeyThread){
        ErrorF("GET SPECIFIC error\n");
        return -1;
    }
    *value = (pKeyThread->KeyValue)[key];

    return 0;
}


pthread_t
pthread_self()
{
    return cthread_self();
}
