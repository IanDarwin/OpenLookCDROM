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
/* $Header: /crl/audio/AF/server/include/RCS/task.h,v 1.14 1994/02/22 17:19:51 stewart Exp $ */
#ifndef TASK_H
#define TASK_H

/* Definitions for manipulating audio tasks within the server. */

#include <sys/time.h>
#include <sys/types.h>

#include <audio.h>
#include <audiodev.h>
#include <ac.h>
#include <misc.h>
#include <dia.h>

/* Useful constants. */
#define	RUN_TASK_BAND	  10		/* How long in the future to run
					 * tasks, in milliseconds. */

/* Type declarations. */

typedef struct tTask {
	struct tTask *next;		/* Next task on free list. */
	ClientPtr   client;		/* Pointer to client struct. */
	fd_set      fdmask;		/* Save fd mask for processing loop.*/
	pointer     request;		/* Client request information.   */
	ATime        time;		/* ATime at which to process task. */
	VoidProc    proc;		/* Procedure to call. */
	pointer     p;			/* Pointer to the task data. */
	int         len;		/* Amount of data left. */
	int	    ssize;		/* sample size of remaining data */
	int	    mask;		/* request mask: endian-ness */
	AudioDevicePtr      aDev;	/* Pseudo device handle. */
	ACPtr       ac;			/* Audio context handle. */
	struct timeval systime;		/* System time (for scheduling). */
} Task;

typedef Task *TaskPtr;

/* Function declarations. */

void InitTaskQueue(void);
void AddTask(VoidProc proc, TaskPtr task, int ms);/* Add a task to the queue.*/
TaskPtr NewTask(void);			/* Allocate a new task. */
struct timeval *TimeOfNextTask(void);	/* Get time for next task to execute.*/
void RunPendingTasks(void);

#endif	/* TASK_H */
