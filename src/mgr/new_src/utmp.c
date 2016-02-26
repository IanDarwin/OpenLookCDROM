/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/utmp.c,v 1.3 91/03/01 11:06:01 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/utmp.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/utmp.c,v $$Revision: 1.3 $";

/* manage utmp file */

#include <pwd.h>
#include <utmp.h>
#include <sys/file.h>
#include <sys/time.h>
#include <stdio.h>

#define UTMP	"/etc/utmp"
#define TTYS	"/etc/ttys"

static struct utmp	entry, save_entry;
static char		zap[sizeof(entry)];
static int		save_slot;

/* remove an entry from utmp file */

int
rm_utmp(line)	
char *line;
   {
   return(_rm_utmp(line,0));
   }

/* remove and save an entry in the utmp file */

int
save_utmp(line)
char *line;
   {
   return(_rm_utmp(line,1));
   }

/* add an entry to the utmp file */

int
add_utmp(fd,host)
int fd;
char *host;
   {
   return(_add_utmp(fd,host,0));
   }

/* restore a previously saved utmp file entry */

int
restore_utmp(fd,host)
int fd;
char *host;
   {
   return(_add_utmp(fd,host,1));
   }

/* defined here so we needn't include defs.h */

#ifdef SYSV
#define index		strchr
#define rindex		strrchr
#endif

/* utmp add-entry service routine */

int
_add_utmp(fd,host,flag)
int fd;
char *host;
int flag;
   {
   char *ttyname(), *rindex();
   long time();
   struct passwd *getpwuid();
   struct timeval tp;
   char *line = ttyname(0);
   int tty;
   int tell;

   if ((fd = open(UTMP,O_RDWR)) >= 0) {
      if (flag) {
         lseek(fd,(long) (save_slot*sizeof(entry)),0);
         write(fd,&save_entry,sizeof(entry));
         }
      else if (line && (tty=ttyslot())) {

         lseek(fd,(long) (tty*sizeof(entry)),0);
         if (rindex(line,'/'))
            line = rindex(line,'/')+1;

         strncpy(entry.ut_line, line, sizeof entry.ut_line);
         strncpy(entry.ut_name, getpwuid(getuid())->pw_name,
		sizeof entry.ut_name);
         if (host == (char *) 0)
            strcpy(entry.ut_host, "");
         else
            strncpy(entry.ut_host, host, sizeof entry.ut_host);
         gettimeofday(&tp,0);
         entry.ut_time = tp.tv_sec;
      
         write(fd,&entry,sizeof(entry));
         }
      close(fd);
      return(tty);
      }
   return(-1);
   }

/* remove utmp entry service routine */

int
_rm_utmp(line,flag)
char *line;
int flag;
   {
   int tty;
   int fd;
   FILE *file;
   char *fgets(), *rindex();
   char buff[32];
   int neof = 0;

   /* find ttyslot */

   if (line == (char *) 0) 
      return(-1);

   if (rindex(line,'/'))
      line = rindex(line,'/')+1;

   if (file = fopen(TTYS,"r")) {
      for(tty=1; neof = fgets(buff,sizeof(buff),file) != NULL;tty++)
         if (strncmp(line,buff+2,strlen(line)) == 0) break;
      fclose(file);
      }
   else {
      return(-2);
      }
      
   /* zap utmp entry */

   if ( neof &&(fd = open(UTMP,O_RDWR))>=0) {
      lseek(fd,(long) (tty*sizeof(entry)),0);
      if (flag) {
	 save_slot = tty;
         read(fd,&save_entry,sizeof(entry));
         lseek(fd,(long) (tty*sizeof(entry)),0);
         }
      write(fd,zap,sizeof(entry));
      close(fd);
      return(tty);
      }
   return(-1);
   }
