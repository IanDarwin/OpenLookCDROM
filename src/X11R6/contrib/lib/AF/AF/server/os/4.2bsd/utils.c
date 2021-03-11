/***********************************************************
Copyright 1987, 1990 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#include <stdio.h>
#include "Aos.h"
#include "misc.h"
#include "audio.h"
#include "input.h"
#include "opaque.h"
#include <sys/signal.h>

extern char *deviceName;

#ifndef SYSV
extern int limitDataSpace, limitStackSpace;
#endif
extern ABool permitOldBugs;
extern int monitorResolution;

extern char *sbrk();

#ifdef DEBUG
#ifndef SPECIAL_MALLOC
#define MEMBUG
#endif
#endif

#ifdef MEMBUG
#define MEM_FAIL_SCALE 100000
long Memory_fail = 0;
static pointer minfree = NULL;
static void CheckNode();
#endif

ABool Must_have_memory = FALSE;

char *dev_tty_from_init = NULL;		/* since we need to parse it anyway */

/* Force connections to close on SIGHUP from init */

#ifdef SYSV
void
#else
int
#endif
AutoResetServer ()
{
    dispatchException |= DE_RESET;
    isItTimeToYield = TRUE;
#ifdef GPROF
    chdir ("/tmp");
    exit (0);
#endif
#ifdef SYSV
    signal (SIGHUP, AutoResetServer);
#endif
}

/* Force connections to close and then exit on SIGTERM, SIGINT */

GiveUp()
{
    dispatchException |= DE_TERMINATE;
    isItTimeToYield = TRUE;
}


static void
AbortServer()
{
    AbortDDA();
    fflush(stderr);
    abort();
}

void
Error(str)
    char *str;
{
    perror(str);
}

#if defined (UTEK) || defined (UTEKV)
/*
 * Tektronix has a shared-memory time value which doesn't
 * match gettimeofday at all, but it is only accessible
 * inside the driver.
 */
#else
long
GetTimeInMillis()
{
    struct timeval  tp;

    gettimeofday(&tp, 0);
    return(tp.tv_sec * 1000) + (tp.tv_usec / 1000);
}
#endif

void UseMsg()
{
    ErrorF("use: AudioFile [:<deviceNumber>] [option]\n");
#ifdef MEMBUG
    ErrorF("-alloc int             chance alloc should fail\n");
#endif
    ErrorF("-auth string           select authorization file\n");	
    ErrorF("-help                  prints message with these options\n");
    ErrorF("-I                     ignore all remaining arguments\n");
#ifndef SYSV
    ErrorF("-ld int                limit data space to N Kb\n");
    ErrorF("-ls int                limit stack space to N Kb\n");
#endif
    ErrorF("-to #                  connection time out\n");

    ddaUseMsg();
}

/*
 * This function parses the command line. Handles device-independent fields
 * and allows dda to handle additional fields.  It is not allowed to modify
 * argc or any of the strings pointed to by argv.
 */
void
ProcessCommandLine ( argc, argv )
int	argc;
char	*argv[];

{
    int i, skip;

#ifdef MEMBUG
    if (!minfree)
	minfree = (pointer)sbrk(0);
#endif

    for ( i = 1; i < argc; i++ )
    {
	/* call dda first, so it can peek/override if it wants */
        if( (skip = ddaProcessArgument(argc, argv, i)) != 0)
	{
	    i += (skip - 1);
	}
	else if (argv[i][0] == ':')
        {
            /* initialize deviceName */
            deviceName = argv[i];
            deviceName++;
        }		
#ifdef MEMBUG
	else if ( strcmp( argv[i], "-alloc") == 0)
	{
	    if(++i < argc)
	        Memory_fail = atoi(argv[i]);
	    else
		UseMsg();
	}
#endif
	else if ( strcmp( argv[i], "-auth") == 0)
	{
	    if(++i < argc)
	        InitAuthorization (argv[i]);
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-help") == 0)
	{
	    UseMsg();
	    exit(0);
	}
#ifndef SYSV
	else if ( strcmp( argv[i], "-ld") == 0)
	{
	    if(++i < argc)
	    {
	        limitDataSpace = atoi(argv[i]);
		if (limitDataSpace > 0)
		    limitDataSpace *= 1024;
	    }
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-ls") == 0)
	{
	    if(++i < argc)
	    {
	        limitStackSpace = atoi(argv[i]);
		if (limitStackSpace > 0)
		    limitStackSpace *= 1024;
	    }
	    else
		UseMsg();
	}
#endif
	else if ( strcmp( argv[i], "-to") == 0)
	{
	    if(++i < argc)
		TimeOutValue = ((long)atoi(argv[i])) * MILLI_PER_SECOND;
	    else
		UseMsg();
	}
	else if ( strcmp( argv[i], "-x") == 0)
	{
	    if(++i >= argc)
		UseMsg();
	    /* For U**x, which doesn't support dynamic loading, there's nothing
	     * to do when we see a -x.  Either the extension is linked in or
	     * it isn't */
	}
	else if ( strcmp( argv[i], "-I") == 0)
	{
	    /* ignore all remaining arguments */
	    break;
	}
 	else
 	{
	    UseMsg();
	    exit (1);
        }
    }
}

#ifndef SPECIAL_MALLOC

#ifdef MEMBUG
#define FIRSTMAGIC 0x11aaaa11
#define SECONDMAGIC 0x22aaaa22
#define FREEDMAGIC  0x33aaaa33

typedef struct _MallocHeader	*MallocHeaderPtr;

typedef struct _MallocHeader {
	unsigned long	amount;
	unsigned long	time;
	MallocHeaderPtr	prev;
	MallocHeaderPtr	next;
	unsigned long	magic;
} MallocHeaderRec;

typedef struct _MallocTrailer {
	unsigned long	magic;
} MallocTrailerRec, *MallocTrailerPtr;

unsigned long	MemoryAllocTime;
unsigned long	MemoryAllocBreakpoint = ~0;
unsigned long	MemoryActive = 0;
unsigned long	MemoryValidate;

MallocHeaderPtr	MemoryInUse;

#define request(amount)	((amount) + sizeof (MallocHeaderRec) + sizeof (MallocTrailerRec))
#define Header(ptr)	((MallocHeaderPtr) (((char *) ptr) - sizeof (MallocHeaderRec)))
#define Trailer(ptr)	((MallocTrailerPtr) (((char *) ptr) + Header(ptr)->amount))

static unsigned long *
SetupBlock(ptr, amount)
    unsigned long   *ptr;
{
    MallocHeaderPtr	head = (MallocHeaderPtr) ptr;
    MallocTrailerPtr	tail = (MallocTrailerPtr) (((char *) ptr) + amount + sizeof (MallocHeaderRec));

    MemoryActive += amount;
    head->magic = FIRSTMAGIC;
    head->amount = amount;
    if (MemoryAllocTime == MemoryAllocBreakpoint)
	head->amount = amount;
    head->time = MemoryAllocTime++;
    head->next = MemoryInUse;
    head->prev = 0;
    if (MemoryInUse)
	MemoryInUse->prev = head;
    MemoryInUse = head;

    tail->magic = SECONDMAGIC;
    
    return (unsigned long *)(((char *) ptr) + sizeof (MallocHeaderRec));
}

ValidateAllActiveMemory ()
{
    MallocHeaderPtr	head;
    MallocTrailerPtr	tail;

    for (head = MemoryInUse; head; head = head->next)
    {
	tail = (MallocTrailerPtr) (((char *) (head + 1)) + head->amount);
    	if (head->magic == FREEDMAGIC)
	    FatalError("Free data on active list");
    	if(head->magic != FIRSTMAGIC || tail->magic != SECONDMAGIC)
	    FatalError("Garbage object on active list");
    }
}

#endif

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
	
    if(!amount)
	return (unsigned long *)NULL;
    /* aligned extra on long word boundary */
    amount = (amount + 3) & ~3;
#ifdef MEMBUG
    if (MemoryValidate)
	ValidateAllActiveMemory ();
    if (!Must_have_memory && Memory_fail &&
	((random() % MEM_FAIL_SCALE) < Memory_fail))
	return (unsigned long *)NULL;
    if (ptr = (pointer)malloc(request(amount)))
	return SetupBlock (ptr, amount);
#else
    if ( (ptr = (pointer)malloc(amount)) != NULL)
	return (unsigned long *)ptr;
#endif
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
    if (MemoryValidate)
	ValidateAllActiveMemory ();
    if (!amount)
    {
	Xfree(ptr);
	return (unsigned long *)NULL;
    }
    if (!Must_have_memory && Memory_fail &&
	((random() % MEM_FAIL_SCALE) < Memory_fail))
	return (unsigned long *)NULL;
    amount = (amount + 3) & ~3;
    if (ptr)
    {
	CheckNode(ptr);
	ptr = (pointer)realloc((char *) Header (ptr), request(amount));
    }
    else
	ptr = (pointer)malloc(request(amount));
    if (ptr)
	return SetupBlock (ptr, amount);
#else
    if (!amount)
    {
	if (ptr)
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
#endif
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
#ifdef MEMBUG
    if (MemoryValidate)
	ValidateAllActiveMemory ();
    if (ptr)
    {
	MallocHeaderPtr	head;

	CheckNode(ptr);
	head = Header(ptr);
	head->magic = FREEDMAGIC;
	free ((char *) head);
    }
#else
    if (ptr)
	free((char *)ptr); 
#endif
}

#ifdef MEMBUG
static void
CheckNode(ptr)
    pointer ptr;
{
    MallocHeaderPtr	head;
    MallocHeaderPtr	f, prev;

    if (ptr < minfree)
	FatalError("Trying to free static storage");
    head = Header(ptr);
    if (((pointer) head) < minfree)
	FatalError("Trying to free static storage");
    if (head->magic == FREEDMAGIC)
	FatalError("Freeing something already freed");
    if(head->magic != FIRSTMAGIC || Trailer(ptr)->magic != SECONDMAGIC)
	FatalError("Freeing a garbage object");
    if(head->prev)
	head->prev->next = head->next;
    else
	MemoryInUse = head->next;
    if (head->next)
	head->next->prev = head->prev;
    MemoryActive -= head->amount;
}

DumpMemoryInUse (time)
    unsigned long   time;
{
    MallocHeaderPtr	head;

    for (head = MemoryInUse; head; head = head->next)
	if (head->time >= time)
	    printf ("0x%08x %5d %6d\n", head,
					head->amount,
					head->time);
}

static unsigned long	MarkedTime;

MarkMemoryTime ()
{
    MarkedTime = MemoryAllocTime;
}

DumpMemorySince ()
{
    DumpMemoryInUse (MarkedTime);
}
#endif
#endif

/*VARARGS1*/
void
FatalError(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9) /* limit of ten args */
    char *f;
    char *s0, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9;
{
    ErrorF("\nFatal server bug!\n");
    ErrorF(f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
    ErrorF("\n");
    AbortServer();
    /*NOTREACHED*/
}

/*VARARGS*/
void ErrorF( f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9) /* limit of ten args */
    char *f;
    char *s0, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9;
{
    fprintf( stderr, f, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
}
