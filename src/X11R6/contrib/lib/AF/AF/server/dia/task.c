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

#if !defined(lint) && !defined(SABER)
static char rcsid_task_c[] = "$Header: /crl/audio/AF/server/RCS/task.c,v 1.18 1993/11/17 20:08:33 tml Exp $";
#endif

#include "task.h"

#define INITIAL_TASK_COUNT	128	/* Number of tasks to start with. */

static int numTasks = 0;
static int maxTasks = INITIAL_TASK_COUNT;
static TaskPtr *taskQueue = NULL;
static TaskPtr freeList = NULL;

#define PARENT(index)	        ((index)/2)   /* Index of parent node. */

/* Function:	TimeLessThan() compares two timeval structures, and returns
 *		TRUE if the first argument is less than the second one.
 */

static ABool
TimeLessThan(struct timeval t1, struct timeval t2)
{
	if ( (t1.tv_sec < t2.tv_sec)
	    || (t1.tv_sec == t2.tv_sec && t1.tv_usec < t2.tv_usec)) 
	  return TRUE;
	else
	  return FALSE;
}

/* Function:	AddTime() adds a millisecond offset to a timeval.
 *	Modifies its first argument, the timeval.
 */

static void
AddTime(struct timeval *tp, int ms)
{
	tp->tv_usec += (ms * 1000);
	tp->tv_sec += tp->tv_usec / 1000000;
	tp->tv_usec = tp->tv_usec % 1000000;
}

/* Function:	InitTaskQueue() initializes the task queue.
 *		The task queue is a heap based priority queue.
 *		The sorting key is the system time, higher priority
 *		to earliest time value.
 * Arguments:	None
 * Returns:	Nothing
 * Notes:
 */

void
InitTaskQueue(void)
{
	if (taskQueue != NULL) {
		ErrorF("InitTaskQueue: already initialized!\n");
	}

	taskQueue = (TaskPtr *) calloc(INITIAL_TASK_COUNT, sizeof(TaskPtr));
	if (taskQueue == NULL) {
		ErrorF("InitTaskQueue: cannot allocate task Queue!\n");
	}
}


/* Function:	upHeap()
 * Arguments:	index.
 * Returns:	none.
 * Notes:
 */
void
upHeap(int k)
{
	TaskPtr task;

	while((k>1) &&
	  TimeLessThan(taskQueue[k]->systime,taskQueue[PARENT(k)]->systime)) 
	{
		task = taskQueue[PARENT(k)];
		taskQueue[PARENT(k)] = taskQueue[k];
		taskQueue[k] = task;
		k = PARENT(k);
	}
}


/* Function:	downHeap()
 * Arguments:	index.
 * Returns:	none.
 * Notes:
 */
downHeap(int k)
{
	TaskPtr vTask;
	int j;

	vTask = taskQueue[k];
	while ( k <= numTasks/2) {
	  j = k+k;
	  if ((j < numTasks) &&
	    TimeLessThan(taskQueue[j+1]->systime,
			     taskQueue[j]->systime))
		j++;
	  if (TimeLessThan(vTask->systime, taskQueue[j]->systime))
	    break;
	  taskQueue[k] = taskQueue[j];
	  k = j;
	}
	taskQueue[k] = vTask;
}

/* Function:	NewTask() returns a pointer to an empty task structure.
 *		The structure is guaranteed to be zero'ed.
 * Arguments:	None.
 * Returns:	A TaskPtr.
 * Notes:
 * 	This function allocates the memory for tasks one at a time,
 *	reusing any returned to the free pool.
 */

TaskPtr
NewTask(void)
{
	TaskPtr new;

	/* Check the free list, and allocate if necessary. */

	if (freeList != NULL) {
		new = freeList;
		freeList = freeList->next;
		new->next = NULL;
	}
	else {
		new = (TaskPtr) malloc(sizeof(Task));
		bzero(new, sizeof(Task));
	}
	return(new);
}

/* Function:	AddTask() adds a task to the task queue.
 * Arguments:	task:	Task to add
 * Returns:	Nothing.
 * Notes:
 *	This function maintains a simple heap-based priority queue based
 *	on the time that a task is scheduled for.
 */

void
AddTask(VoidProc proc, TaskPtr task, int ms)
{
	if (taskQueue == NULL) {
		ErrorF("AddTask: task Queue not initialized!\n");
	}
	if (task == NULL) {
		ErrorF("AddTask: no task to add!\n");
	}

	/* Finish setting up the task structure. */

	task->proc = proc;
	if (gettimeofday(&task->systime, (struct timezone *)NULL) != 0) {
		ErrorF("gettimeofday failed!\n");
	}
	AddTime(&task->systime, ms);

	/* Reallocate the table if needed, being sure to zero out the
	 * newly allocated space. */

	++numTasks;
	if (numTasks == maxTasks) {
		maxTasks *= 2;
		taskQueue = (TaskPtr *) realloc((char *) taskQueue,
						maxTasks * sizeof(TaskPtr));
		bzero((char *) &taskQueue[numTasks],
		      maxTasks/2 * sizeof(TaskPtr));
	}
	taskQueue[numTasks] = task;
	upHeap(numTasks);
}


/* Function:	TimeOfNextTask() returns the time OFFSET at which the
 *		first task in the queue should execute.
 * Arguments:	None.
 * Returns:	pointer to a static struct timeval, giving the offset,
 *		or NULL if there is no task to run.
 * Notes:
 *	The variable tv is a static struct so we can return a pointer to it.
 */

struct timeval *
TimeOfNextTask(void)
{
	static struct timeval tv;
	struct timeval *cur;

	if (numTasks == 0) {
		return NULL;		/* No tasks to run. */
	}
	if (gettimeofday(&tv, (struct timezone *) NULL) != 0) {
		ErrorF("TimeOfNextTask: gettimeofday failed!\n");
	}
	cur = &taskQueue[1]->systime;

	/* Create a time offset. */

	tv.tv_sec = cur->tv_sec - tv.tv_sec;
	tv.tv_usec = cur->tv_usec - tv.tv_usec;

	/* Check microseconds. */

	if (tv.tv_usec < 0) {
		tv.tv_sec--;
		tv.tv_usec += 1000000;
	}

	/* Check seconds.  If negative, then return zeros, because this
	 * should already have occured.
	 */

	if (tv.tv_sec < 0) {
		tv.tv_sec = 0;
		tv.tv_usec = 0;
	}

	/* Return a pointer to the timeout value. */

	return(&tv);
}

/* Function:	RunFirstTask() executes the first task on the queue
 * Arguments:
 * Returns:
 * Notes:
 */

void
RunFirstTask()
{
	TaskPtr task;

	if (numTasks == 0) {		/* Nothing to do. */
		return;
	}

	/* Save the first task to run, and then re-heapify.
	 * We do it in this order so that tasks can add new tasks
 	 * when they're executed.
	 */

	task = taskQueue[1];

	taskQueue[1] = taskQueue[numTasks];
	taskQueue[numTasks--] = NULL;

	downHeap(1);

	/* Execute the task...*/

	(*(task->proc))(task);

	/* And put it on the free list. */

	bzero(task, sizeof(Task));
	if (freeList != NULL) {
		task->next = freeList;
		freeList = task;
	}
	else
	  freeList = task;
}

/* Function:	RunPendingTasks() runs any pending tasks on the queue.
 * Arguments:	None.
 * Returns:	Nothing.
 * Notes:
 *	RunPendingTask() runs all tasks that are scheduled to run in the
 *	past or within RUN_TASK_BAND milliseconds.
 */

void
RunPendingTasks(void)
{
	TaskPtr task;
	struct timeval tv;

	while (numTasks > 0) {
		gettimeofday(&tv, (struct timezone *)NULL);
		AddTime(&tv, RUN_TASK_BAND);
		task = taskQueue[1];
		if ( TimeLessThan(task->systime, tv))
		  RunFirstTask();
		else
		  break;
	}
}
