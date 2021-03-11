/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)utils.c,v 1.5 1994/05/27 03:10:50 greg Exp $
 */
/***********************************************************
Some portions derived from:

Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include <audio/audio.h>
#include <audio/Aos.h>
#include <stdio.h>
#include "misc.h"
#include "opaque.h"
#include <signal.h>
#if !defined(SYSV) && !defined(AMOEBA) && !defined(_MINIX)
#include <sys/resource.h>
#endif
#include <time.h>

#ifdef AMOEBA
#include "osdep.h"
#include <amoeba.h>
#include <module/mutex.h>
#endif

#ifdef SIGNALRETURNSINT
#define SIGVAL int
#else
#define SIGVAL void
#endif

#if defined(SYSV) || defined(SVR4)
#define signal sigset
#endif

extern char *display;

#ifndef AMOEBA
extern Bool PartialNetwork;
#endif

Bool CoreDump;
Bool noTestExtensions;

Bool allow_any_host = FALSE;

int auditTrailLevel = 1;

#ifdef AMOEBA
static mutex print_lock
#endif

void ddxUseMsg();

#if !defined(SVR4) && !defined(hpux) && !defined(linux) && !defined(AMOEBA) && !defined(_MINIX)
extern char *sbrk();
#endif

#ifdef AIXV3
#define AIXFILE "/tmp/aixfile"
FILE *aixfd;
int SyncOn  = 0;
extern int SelectWaitTime;
#endif

#ifdef DEBUG
#ifndef SPECIAL_MALLOC
#define MEMBUG
#endif
#endif

#ifdef MEMBUG
#define MEM_FAIL_SCALE 100000
long Memory_fail = 0;

#endif

#ifdef sgi
int userdefinedfontpath = 0;
#endif /* sgi */

Bool Must_have_memory = FALSE;

char *dev_tty_from_init = NULL;		/* since we need to parse it anyway */

/* Force connections to close on SIGHUP from init */

/*ARGSUSED*/
SIGVAL
AutoResetServer (sig)
    int sig;
{
    dispatchException |= DE_RESET;
    isItTimeToYield = TRUE;
#ifdef GPROF
    chdir ("/tmp");
    exit (0);
#endif
#if defined(USG) || defined(SYSV) || defined(SVR4) || defined(linux) || defined(_MINIX)
    signal (SIGHUP, AutoResetServer);
#endif
#ifdef AMOEBA
    WakeUpMainThread();
#endif
}

/* Force connections to close and then exit on SIGTERM, SIGINT */

/*ARGSUSED*/
SIGVAL
GiveUp(sig)
    int sig;
{

#if defined(SYSV) || defined(SVR4) || defined(linux) || defined(_MINIX)
    /*
     * Don't let any additional occurances of thses signals cause
     * premature termination
    */
    signal(SIGTERM, SIG_IGN);
    signal(SIGINT, SIG_IGN);
#endif

    dispatchException |= DE_TERMINATE;
    isItTimeToYield = TRUE;
#ifdef AMOEBA
    WakeUpMainThread();
#endif
}


static void
AbortServer()
{
    fflush(stderr);
    if (CoreDump)
    {
#ifdef AMOEBA
	IOPCleanUp();
#endif
	abort();
    }
    exit (1);
}

void
Error(str)
    char *str;
{
#ifdef AMOEBA
    mu_lock(&print_lock);
#endif
    perror(str);
#ifdef AMOEBA
    mu_unlock(&print_lock);
#endif
}

#ifndef DDXTIME
#ifndef AMOEBA
long
GetTimeInMillis()
{
    struct timeval  tp;

    gettimeofday(&tp, 0);
    return(tp.tv_sec * 1000) + (tp.tv_usec / 1000);
}
#else
long
GetTimeInMillis()
{
    return sys_milli();
}
#endif /* AMOEBA */
#endif

AdjustWaitForDelay (waitTime, newdelay)
    pointer	    waitTime;
    unsigned long   newdelay;
{
    static struct timeval   delay_val;
    struct timeval	    **wt = (struct timeval **) waitTime;
    unsigned long	    olddelay;

    if (*wt == NULL)
    {
	delay_val.tv_sec = newdelay / 1000;
	delay_val.tv_usec = 1000 * (newdelay % 1000);
	*wt = &delay_val;
    }
    else
    {
	olddelay = (*wt)->tv_sec * 1000 + (*wt)->tv_usec / 1000;
	if (newdelay < olddelay)
	{
	    (*wt)->tv_sec = newdelay / 1000;
	    (*wt)->tv_usec = 1000 * (newdelay % 1000);
	}
    }
}

void UseMsg()
{
    ErrorF("use: Au [:<listen port offset>] [option]\n");
    ErrorF(" -aa		allow any host to connect\n");
#ifndef AMOEBA
#ifdef PART_NET
    ErrorF(" -pn		partial networking enabled [default]\n");
    ErrorF(" -nopn		partial networking disabled\n");
#else
    ErrorF(" -pn		partial networking enabled\n");
    ErrorF(" -nopn		partial networking disabled [default]\n");
#endif
#endif
}

/*
 * This function parses the command line. Handles device-independent fields
 * and allows ddx to handle additional fields.  It is not allowed to modify
 * argc or any of the strings pointed to by argv.
 */
void
ProcessCommandLine ( argc, argv )
int	argc;
char	*argv[];
{
     int	i;
#ifndef AMOEBA
#ifdef PART_NET
     PartialNetwork = TRUE;
#endif
#endif

     for (i = 1; i < argc; i++) {
	if (argv[i][0] == ':') {
	    display = argv[i];
	    display++;
	} else if (strcmp(argv[i], "-aa") == 0)
	    allow_any_host = TRUE;
#ifndef AMOEBA
	else if (strcmp(argv[i], "-pn") == 0)
	    PartialNetwork = TRUE;
	else if (strcmp(argv[i], "-nopn") == 0)
	    PartialNetwork = FALSE;
#endif
	else {
	    UseMsg();
	    exit(1);
	}
     }
}

/* XALLOC -- X's internal memory allocator.  Why does it return unsigned
 * int * instead of the more common char *?  Well, if you read K&R you'll
 * see they say that alloc must return a pointer "suitable for conversion"
 * to whatever type you really want.  In a full-blown generic allocator
 * there's no way to solve the alignment problems without potentially
 * wasting lots of space.  But we have a more limited problem. We know
 * we're only ever returning pointers to structures which will have to
 * be long word aligned.  So we are making a stronger guarantee.  It might
 * have made sense to make Xalloc return char * to conform with people's
 * expectations of malloc, but this makes lint happier.
 */

unsigned long * 
Xalloc (amount)
    unsigned long amount;
{
    char		*malloc();
    register pointer  ptr;
	
    if ((long)amount <= 0)
	return (unsigned long *)NULL;
    /* aligned extra on long word boundary */
    amount = (amount + 3) & ~3;
#ifdef MEMBUG
    if (!Must_have_memory && Memory_fail &&
	((random() % MEM_FAIL_SCALE) < Memory_fail))
	return (unsigned long *)NULL;
#endif
    if (ptr = (pointer)malloc(amount))
	return (unsigned long *)ptr;
    if (Must_have_memory)
	FatalError("Out of memory");
    return (unsigned long *)NULL;
}

/*****************
 * Xcalloc
 *****************/

unsigned long *
Xcalloc (amount)
    unsigned long   amount;
{
    unsigned long   *ret;

    ret = Xalloc (amount);
    if (ret)
	bzero ((char *) ret, (int) amount);
    return ret;
}

/*****************
 * Xrealloc
 *****************/

unsigned long *
Xrealloc (ptr, amount)
    register pointer ptr;
    unsigned long amount;
{
    char *malloc();
    char *realloc();

#ifdef MEMBUG
    if (!Must_have_memory && Memory_fail &&
	((random() % MEM_FAIL_SCALE) < Memory_fail))
	return (unsigned long *)NULL;
#endif
    if ((long)amount <= 0)
    {
	if (ptr && !amount)
	    free(ptr);
	return (unsigned long *)NULL;
    }
    amount = (amount + 3) & ~3;
    if (ptr)
        ptr = (pointer)realloc((char *)ptr, amount);
    else
	ptr = (pointer)malloc(amount);
    if (ptr)
        return (unsigned long *)ptr;
    if (Must_have_memory)
	FatalError("Out of memory");
    return (unsigned long *)NULL;
}
                    
/*****************
 *  Xfree
 *    calls free 
 *****************/    

void
Xfree(ptr)
    register pointer ptr;
{
    if (ptr)
	free((char *)ptr); 
}

#ifdef CAHILL_MALLOC
#include <malloc.h>

unsigned long * 
debug_Xalloc (file, line, amount)
    char *file;
    int line;
    unsigned long amount;
{
    register pointer  ptr;
	
    if ((long)amount <= 0)
	return (unsigned long *)NULL;
    /* aligned extra on long word boundary */
    amount = (amount + 3) & ~3;
    if (ptr = (pointer)debug_malloc(file, line, amount))
	return (unsigned long *)ptr;
    if (Must_have_memory)
	FatalError("Out of memory");
    return (unsigned long *)NULL;
}

/*****************
 * Xcalloc
 *****************/

unsigned long *
debug_Xcalloc (file, line, amount)
    char *file;
    int line;
    unsigned long   amount;
{
    unsigned long   *ret;

    ret = debug_Xalloc (file, line, amount);
    if (ret)
	bzero ((char *) ret, (int) amount);
    return ret;
}

/*****************
 * Xrealloc
 *****************/

unsigned long *
debug_Xrealloc (file, line, ptr, amount)
    char *file;
    int line;
    register pointer ptr;
    unsigned long amount;
{
    if ((long)amount <= 0)
    {
	if (ptr && !amount)
	    debug_free(file, line, ptr);
	return (unsigned long *)NULL;
    }
    amount = (amount + 3) & ~3;
    if (ptr)
        ptr = (pointer)debug_realloc(file, line, (char *)ptr, amount);
    else
	ptr = (pointer)debug_malloc(file, line, amount);
    if (ptr)
        return (unsigned long *)ptr;
    if (Must_have_memory)
	FatalError("Out of memory");
    return (unsigned long *)NULL;
}
                    
/*****************
 *  Xfree
 *    calls free 
 *****************/    

void
debug_Xfree(file, line, ptr)
    char *file;
    int line;
    register pointer ptr;
{
    if (ptr)
	debug_free(file, line, (char *)ptr); 
}
#endif

OsInitAllocator ()
{
#ifdef MEMBUG
    static int	been_here;

    /* Check the memory system after each generation */
    if (been_here)
	CheckMemory ();
    else
	been_here = 1;
#endif
}

/*VARARGS1*/
void
AuditF(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9) /* limit of ten args */
    char *f;
    char *s0, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9;
{
#ifdef X_NOT_STDC_ENV
    long tm;
#else
    time_t tm;
#endif
    char *autime, *s;

    if (*f != ' ')
    {
	time(&tm);
	autime = ctime(&tm);
	if (s = index(autime, '\n'))
	    *s = '\0';
	if (s = rindex(argvGlobal[0], '/'))
	    s++;
	else
	    s = argvGlobal[0];
	ErrorF("AUDIT: %s: %d %s: ", autime, getpid(), s);
    }
    ErrorF(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
}

/*VARARGS1*/
void
FatalError(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9) /* limit of ten args */
    char *f;
    char *s0, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9;
{
    ErrorF("\nFatal server error:\n");
    ErrorF(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
    ErrorF("\n");
    AbortServer();
    /*NOTREACHED*/
}

/*VARARGS1*/
void
ErrorF( f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9) /* limit of ten args */
    char *f;
    char *s0, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9;
{
#ifdef AIXV3
    fprintf(aixfd, f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
    fflush (aixfd);

    if (SyncOn)
        sync();
#else
#ifdef AMOEBA
    mu_lock(&print_lock);
#endif
    fprintf( stderr, f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
#ifdef AMOEBA
    mu_unlock(&print_lock);
#endif
#endif
}

#ifdef AIXV3
OpenDebug()
{
        if((aixfd = fopen(AIXFILE,"w")) == NULL )
        {
                fprintf(stderr,"open aixfile failed\n");
                exit(-1);
        }
        chmod(AIXFILE,00777);
}
#endif
