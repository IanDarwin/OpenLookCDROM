/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/getshell.c,v 1.3 91/03/01 11:05:51 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/getshell.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/getshell.c,v $$Revision: 1.3 $";

/* start a shell */

#include <sys/file.h>
#include <sys/signal.h>
#include <sgtty.h>
#include <stdio.h>
#include "bitmap.h"
#include "defs.h"

#define SHELL		"/bin/sh"

static char line[] = {"/dev/ptypX"};
static int  pty_index=5;		/* better hit rate than 0 */
extern char **environ;

/*	get a pty line */

int
getapty()
   {
   register int i;
   int fd;

   line[5] = 'p';
   for(line[8]='p';line[8]<='t';line[8]+= 1)
      for (i=1;i<=16;i++) {
         line[9]="0123456789abcdef"[(pty_index+i)%16];
         if ((fd = open(line,2)) >= 0) {
            /* pty_index = (pty_index+i)%16;   temp */
            line[5] = 't';
            return(fd);
            }
         }
   return(-1);
   }
      
int getatty()
   {
   int fd;
   line[5]='t';
   fd=open(line,2);
   if (fd<0) {
      sleep(3);
      return (open(line,2));
      }
   return(fd);
   }

char *
last_tty()
   {
   return(line);
   }

/******************************************************************************/
/* start a command */

get_command(argv,file)
char **argv;
int *file;
   {
   register int i;				/* counter */
   int fd;					/* file desc */
   int tty;					/* fd of /dev/tty */
   int pid;					/* pid of shell */
   int group;					/* process group id */
   int tty_slots;				/* # of tty slots */
   char *name, *get_path();
   char *getenv();
   char *shell = getenv("SHELL");
   char *arg[2];
   char who[MAXNAME];

   if (argv == (char **) 0 ) {
      argv = arg;
      *argv = shell?shell:SHELL;
      *(argv+1) = (char *) 0;
      }
   name = get_path(argv[0]);

   if (name == (char *) 0 || *name == '\0')
      return(-2);

#ifdef DEBUG
   dprintf(s)(stderr,"EXECING: ");
   for(i=0;argv[i]!='\0';i++)
      dprintf(s)(stderr,"%s ",argv[i]);
   dprintf(s)(stderr,"\n");
#endif

   if ((*file=getapty()) < 0)
      return(-1);

   if ((pid=fork()) > 0) {
      /* parent side of fork */
      char buff[2];
      read(*file,buff,sizeof(buff));	/* wait for slave side to open */
#ifdef DEBUG
      dprintf(s)(stderr,"EXEC done, slave side open\r\n ");
#endif
      return(pid);
      }
   else if (pid<0)
      /* error side of fork */
      return(pid);

   /* child side of fork */
   for(i=0;i<NSIG;i++)
      signal( i, SIG_DFL );

   /* void association with controlling terminal */

#ifdef TIOCNOTTY
   tty = open("/dev/tty",0);
   ioctl(tty,TIOCNOTTY,0);
   close(tty);
#endif

   /* open slave side of ptty */

   if ((fd=getatty())<0) {
      _quit("");
      perror("Slave side of p-tty won't open");
      exit(1);
      }

   group=getpid();

#ifndef SYSV
   tty_slots = getdtablesize();
#else
   tty_slots = 20;
#endif

   for(i=0;i<tty_slots;i++) if (i != fd) close(i);

   /* set the uid-0 stuff up */

   if (geteuid() < 1) {
      int uid = getuid();
      fchmod(fd,0622);
      fchown(fd,uid,-1);
      setreuid(uid,uid);

      uid = getgid();
      fchown(fd,-1,uid);
      setregid(uid,uid);
      }

   i = dup(fd);
   close(fd);
   dup(i);
   dup(i);

   setpgrp(group,group);
   ioctl(0,TIOCSPGRP,&group);

	adjust_mode(NTTYDISC,ECHO|CRMOD|EVENP|ODDP);
	restore_modes(0);

   /* add a utmp entry */

#ifdef WHO
   add_utmp(0,sprintf(who,"%s%c",HOST,line[9]));
#endif

   /* start the command */

#ifdef DEBUG
   dprintf(s)(stderr,"execing %s (%s ...)\r\n",name,*argv);
#endif

   do_env("TERM=",TERMNAME);
   do_env("TERMCAP=","");

   write(2,"\n",1);	/* tell master that slave side is open */
   execve(name,argv,environ);
   _exit(1);
   }

/* half open a ptty then return */

char *
half_open(file)
int *file;
   {
   register int i;				/* counter */
   int pid;					/* file desc */

   if ((*file=getapty()) < 0)
      return((char *) 0);
   ioctl(*file,TIOCREMOTE,0);	/* I dunno */
   return(line);
   }

/* get a complete path name from command */

static char path[512];
static char start[512];

char *
get_path(name)
char *name;
   {
   char *getenv(), *index();
   register char c, *next, *list;

   if (index("/.",*name))
      if (access(name,X_OK)==0)
         return(name);
      else
         return((char *)0);

   strcpy(start,getenv("PATH"));
   for(list=start;next=index(list,':');list=next+1) {
      *next = '\0';
      sprintf(path,"%s/%s",list,name);
      if (access(path,X_OK) == 0)
         return(path);
      }

   sprintf(path,"%s/%s",list,name);
   if (list && access(path,X_OK) == 0) {
      return(path);
      }
   else {
      return((char *) 0);
      }
   }

/* change an environment variable */

do_env(name,value)
char *name, *value;
   {
   register int i;
   int n = strlen(name);
   
   for(i=0;environ[i] != (char *) 0;i++)
      if (strncmp(environ[i],name,n) == 0) {
         strcpy(environ[i]+n,value);
         break;
         }
   }
