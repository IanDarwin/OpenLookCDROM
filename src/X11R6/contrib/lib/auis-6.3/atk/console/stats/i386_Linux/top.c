/*
 * top.c		- show top CPU processes
 *
 * Copyright (c) 1992 Branko Lankester
 * Copyright (c) 1992 Roger Binns
 *
 * Snarfed and HEAVILY modified for the YAPPS (yet another /proc ps)
 * by Michael K. Johnson, johnsonm@stolaf.edu.  What is used is what
 * is required to have a common interface.
 *
 * Modified Michael K Johnson's ps to make it a top program.
 * Also borrowed elements of Roger Binns kmem based top program.
 * Changes made by Robert J. Nation (nation@rocket.sanders.lockheed.com)
 * 1/93
 *
 * Modified by Michael K. Johnson to be more efficient in cpu use
 * 2/21/93
 *
 * Changed top line to use uptime for the load average.  Also
 * added SIGTSTP handling.  J. Cowley, 19 Mar 1993.
 *
 * Bashed heavily by Michael O'Reilly to provide stats for andrew
 * console. 
 */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <sys/ioctl.h>
#include <pwd.h>
#include <linux/sched.h>
#include <linux/tty.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <ctype.h>
#include <setjmp.h>
#include "ps.h"
#include "getstats.h"

#define sendval(text) {printf text ;fflush(stdout);}
/********************************************************************
 * this structure stores some critical information from one frame to
 * the next
 ********************************************************************/
struct save_hist {
  int ticks;
  int pid;
  int pcpu;
  int utime;
  int stime;
};
struct save_hist new_save_hist[NR_TASKS];


/********************************************************************
 * Misc function declarations
 ********************************************************************/
void do_setup();
void end();
void stop();
void window_size();
void clear_screen();
void show_procs(struct ps_proc_head *ph, unsigned int maxcmd);
float get_elapsed_time();
unsigned int show_meminfo();
void do_stats(struct ps_proc_head *ph, float elapsed_time,int pass);
void show_task(struct ps_proc *this, unsigned int main_mem, int pcpu);
void do_key(char c);



/********************************************************************
 * Data used to control screen formatting
 ********************************************************************/
char *cm, *cl, *clrtobot,*clrtoeol;
int lines,cols,maxlines;
int display_procs;
unsigned int maxcmd;
jmp_buf redraw_jmp;

/********************************************************************
 * Controls how long we sleep between screen updates
 ********************************************************************/
float sleeptime = 5;



/********************************************************************
 * Main procedure. Contains loop which displays tasks, and then
 * sleeps/waits for input
 ********************************************************************/
int top_main()
{
  static int initted = 0;
  static struct ps_proc_head *ph;
  float elapsed_time;
  if (!initted) {
    initted++;
    /* first time through, just collect process stats */
    ph = take_snapshot(1, 1, 1, 1, 0, 0, 0);
    elapsed_time = get_elapsed_time();
    do_stats(ph,elapsed_time,0);
  }
  /* display the tasks */
  show_procs(ph, maxcmd);
      
}


/********************************************************************
 * This is the real program!
 * It reads process info and displays it.
 ********************************************************************/
void show_procs(struct ps_proc_head *ph, unsigned int maxcmd)
{
  struct ps_proc *this,*best;
  int count,top;
  int index,best_index;
  float elapsed_time;
  unsigned int main_mem;


  /* get the process info */
  ph = refresh_snapshot(ph, 1, 1, 1, 1, 0, 0, 0);
  /* immediately find out the elapsed time for the frame */
  elapsed_time = get_elapsed_time();
  
  /* display the system stats, also calculate percent CPU time */
  do_stats(ph,elapsed_time,1);

  /* display the memory and swap space usage */
  main_mem = show_meminfo();

#if 0  
  /* finally! loop through to find the top task, and display it */
  count = 0;
  top = 100;
  while((count < maxlines)&&(top >= 0))
    {
      /* find the top of the remaining processes */
      top=-1;
      this = ph->head;
      best = this;
      best_index = 0;
      index=0;
      while(this !=NULL)
	{
	  if(new_save_hist[index].pcpu>top)
	    {
	      top = new_save_hist[index].pcpu;
	      best = this;
	      best_index = index;
	    }
	  index++;
	  this = this->next;
	}
      count++;
      if(top>=0)
	{
	  /* display the process */
	  show_task(best,main_mem,new_save_hist[best_index].pcpu);
	}
      new_save_hist[best_index].pcpu=-1;
    }
  printf("%s%s",clrtobot,tgoto(cm,0,5));

  /* make sure that the screen is updated */
  fflush(stdout);
#endif
}

/********************************************************************
 * Finds the current time (in microseconds) and finds the time
 * elapsed since the last update. This is essential for computing
 * percent CPU usage.
 ********************************************************************/
float get_elapsed_time()
{
  struct timeval time;
  static struct timeval oldtime;
  struct timezone timez;
  float elapsed_time;

  gettimeofday(&time,&timez);
  elapsed_time = (time.tv_sec - oldtime.tv_sec) +
    (float)(time.tv_usec - oldtime.tv_usec)/1000000.0;
  oldtime.tv_sec = time.tv_sec;
  oldtime.tv_usec= time.tv_usec;
  return elapsed_time;
}



/********************************************************************
 * Reads the memory info and displays it 
 * returns the total memory available for use in percent memory 
 * usage calculations.
 ********************************************************************/
unsigned int show_meminfo()
{
  char memory[1024];
  static int fd;
  unsigned int main_mem, used_mem, free_mem, shared_mem, buf_mem;
  unsigned int swap_mem, used_swap, free_swap;

  fd = open("/proc/meminfo", O_RDONLY, 0);
  if (fd == -1) 
    {
      end();
    }
  read(fd,memory,sizeof(memory)-1);
  close(fd);
  sscanf(memory, "%*s %*s %*s %*s %*s %*s %u %u %u %u %u %*s %u %u %u",
	 &main_mem, &used_mem, &free_mem, &shared_mem, &buf_mem,
	 &swap_mem, &used_swap, &free_swap);

  sendval(("%d:%d\n", VM, (int) 100 - (((float) free_mem / main_mem) * 100)));
  sendval(("%d:%d\n", PAGEREPLACABLE, buf_mem / 1024));
  sendval(("%d:%d\n", MEMACTIVE, used_mem / 1024));
  sendval(("%d:%d\n", MEMFREE, free_mem / 1024));
#if 0
  printf("Mem:  %5dK av, %5dK used, %5dK free, %5dK shrd, %5d buff%s\n",
	 main_mem/1024, used_mem/1024, free_mem/1024, 
	 shared_mem/1024, buf_mem/1024,clrtoeol);
  printf("Swap: %5dK av, %5dK used, %5dK free%s\n%s\n",
	 swap_mem/1024, used_swap/1024, free_swap/1024,clrtoeol,
	 clrtoeol);
#endif
  return main_mem;
}


/********************************************************************
 * Calculates the number of tasks in each state (running, sleeping, etc.)
 * Calculates the CPU time in each state (system, user, nice, etc)
 * calculates percent cpu usage for each task 
 ********************************************************************/
void do_stats(struct ps_proc_head *ph,float elapsed_time,int pass)
{
  struct ps_proc *this;
  int index,total_time,i;
  int sleeping = 0,stopped = 0,zombie = 0,running = 0;
  int system_ticks = 0,user_ticks = 0,nice_ticks = 0,idle_ticks = 1000;
  static int prev_count=0;
  static struct save_hist save_hist[NR_TASKS];
  int stime, utime;
  /* make sure that there aren't too many tasks */
  if(ph->count >NR_TASKS)
    {
      end();
    }

  /* make a pass through the data to get stats */
  index=0;
  this = ph->head;
  while(this != NULL)
    {
				/* this is changed to match the things */
				/* andrew asks for. */
      if((this->state == 'S')||(this->state == 'T'))
	sleeping++;
      else if(this->state == 'D')
	stopped++;
      else if(this->state == 'Z')
	zombie++;
      else if(this->state == 'R')
	running++;

      /* calculate time in this process */
      /* time is sum of user time (utime) plus system time (stime) */
      total_time = this->utime + this->stime;
      new_save_hist[index].ticks = total_time;
      new_save_hist[index].pid = this->pid;
      stime = this->stime;
      utime = this->utime;
      new_save_hist[index].stime = stime;
      new_save_hist[index].utime = utime;
      /* find matching entry from previous pass*/
      i=0;
      while(i<prev_count)
	{
	  if(save_hist[i].pid == this->pid)
	    {
	      total_time -= save_hist[i].ticks;
	      stime -= save_hist[i].stime;
	      utime -= save_hist[i].utime;

	      i = NR_TASKS;
	    }
	  i++;
	}
      /* calculate percent cpu time for this task */
      new_save_hist[index].pcpu = (total_time * 10) /elapsed_time;
      if (new_save_hist[index].pcpu > 999)
	new_save_hist[index].pcpu = 999;

      /* calculate time in idle, system, user and niced tasks */
      idle_ticks -= new_save_hist[index].pcpu;
      system_ticks += stime;
      user_ticks += utime;
      if(this->priority < PZERO)
	nice_ticks += new_save_hist[index].pcpu;

      index++;
      this = this->next;
    }

  if(idle_ticks < 0)
    idle_ticks = 0;
  system_ticks = (system_ticks * 10) /elapsed_time;      
  user_ticks = (user_ticks * 10) /elapsed_time;

  /* display stats */
  if(pass>0)
    {
      sendval(("%d:%d\n", QUEUERUN, running));
      sendval(("%d:%d\n", QUEUEBLOCK, sleeping));
      sendval(("%d:%d\n", QUEUEMEM, stopped));
      sendval(("%d:%d\n", PROCSTOTAL, 
	       (int) ((running + sleeping + zombie + stopped) *
		      100)/NR_TASKS)); 
      
#if 0      
      printf("%d processes: %d sleeping, %d running, %d zombie, %d stopped%s\n",
	     ph->count,sleeping,running,zombie,stopped,clrtoeol);
#endif
      sendval(("%d:%d\n", LOADUSER, (user_ticks + nice_ticks)/10));
      sendval(("%d:%d\n", LOADSYS, system_ticks/10));
      sendval(("%d:%d\n", LOADIDLE, idle_ticks/10));
#if 0
      printf("CPU states: %2d.%d%% user, %2d.%d%% nice,",
	     user_ticks/10, user_ticks%10,
	     nice_ticks/10, nice_ticks%10);
      printf(" %2d.%d%% system, %2d.%d%% idle%s\n",
	     system_ticks/10, system_ticks%10,
	     idle_ticks/10, idle_ticks%10,clrtoeol);
#endif
    }

  /* save this frame's information */
  for(i=0;i<ph->count;i++)
    {
      /* copy the relevant info for the next pass */
      save_hist[i].pid = new_save_hist[i].pid;
      save_hist[i].ticks = new_save_hist[i].ticks;
      save_hist[i].stime = new_save_hist[i].stime;
      save_hist[i].utime = new_save_hist[i].utime;
    }
  prev_count = ph->count;
}
