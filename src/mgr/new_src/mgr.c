/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/mgr.c,v 1.3 91/03/01 11:05:53 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/mgr.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/mgr.c,v $$Revision: 1.3 $";

/* main routine for MGR */

#include "bitmap.h"
#include <stdio.h>
#include <sys/signal.h>
#ifdef sun
#include <sys/time.h> 
#endif
#include <errno.h>
#include "defs.h"
#include "menu.h"
#include "font.h"
#include "event.h"

#ifndef Min
#define Min(x,y)		((x)>(y)?(y):(x))
#endif
#define POLL(poll)		(poll&mask ? &set_poll : (struct timeval *) 0)

#ifdef sun
struct timeval set_poll = {
   (long) 0, (long) POLL_INT
   };				/* set select to poll */
#endif

char *mouse_dev = MOUSE_DEV;		/* name of mouse device */
static int bitmaptype = 1;			/* old, non portable bitmap format */

#ifdef MOVIE
	int sig_share();				/* to trap SIGUSR[12] for logging */
	char *log_command=NULL;		/* process to pipe logging info to */
	FILE *log_file=NULL;			/* file pointer for logging */
	int log_now=0;					/* should be local */
#endif

main(argc,argv)
int argc;
char **argv;
   {
   register WINDOW *win;		/* current window to update */
   register int i;			/* counter */
   register int count;			/* # chars read from shell */
   int maxbuf = MAXBUF;			/* # chars processed per window */
   int shellbuf = MAXSHELL;		/* # chars processed per shell */
   int type=1;				/* state var. for parsing argv */
   int reads;				/* masks, result of select */
   int tty;					/* fd for controlling tty */

   unsigned char c;			/* reads from kbd go here */
   char start_file[MAX_PATH];		/* name of startup file */
   char *screen_dev = SCREEN_DEV;	/* name of frame buffer */
   char *default_font = (char * )0;	/* default font */
   char *rindex(), *getenv(), *ttyname();
   char *term = getenv("TERM");		/* place to put terminal name */
#ifdef SHRINK
   BITMAP *prime;
#endif

#ifdef DEBUG
   setlinebuf(stderr);
#endif

#ifdef MALLOC
	malloc_debug(2);
#endif

   SETMOUSEICON(&mouse_arrow);
	timestamp();							/* initialize the timestamp */

   /* process arguments */

   sprintf(start_file,"%s/%s",getenv("HOME"),STARTFILE);
   while(--argc > 0) {
      argv++;
#ifdef DEBUG
      dprintf(S)(stderr,"argument %s type %c\r\n",*argv,type==1?'*':type);
#endif
      switch(type) {
         case 1:	/* looking for flag */
              if (**argv == '-')
                 switch(type = *(*argv+1)) {
#ifdef DEBUG
                 case 'd': debug = 1;
                           strcpy(debug_level,*argv+2);
                           fprintf(stderr,"Debug level: [%s]\n",debug_level);
                           type = 1;
                           break;
#endif
                 case 'v': 		/* print version number */
                           printf("Mgr version %s created on %s at: %s\n",
                                  version[0],version[1],version[2]);
                           exit(1);
                 case 'V': 		/* print version and options */
                           printf("Mgr version %s created on %s at: %s by %s\n",
                                  version[0],version[1],version[2],version[5]);
                           printf("Compile flags: %s\n",version[3]);
                           printf("Home directory: %s\n",version[4]);
									printf("built with: %s\n",version[6]);
									printf(" Graphics directory: %s\n",version[7]);
									printf(" Compile flags: %s\n",version[8]);
                           exit(1);
                 case 'x': strcpy(start_file,"/dev/null");
                           type = 1;
                           break;
                 case 'n': 	/* use new style bitmap headers */
                           bitmaptype = 0;
                           break;
                 }
              else fprintf(stderr,"Invalid argument %s, ignored\n",*argv);
              break;
         case 'm':	/* set mouse device */
              mouse_dev = *argv;
              type = 1;
              break;
         case 's':	/* set start file */
              strcpy(start_file,*argv);
              type = 1;
              break;
         case 'F':	/* set default font file */
              default_font = *argv;
              type = 1;
         case 'P':	/* set polling timeout */
              set_poll.tv_usec = (long) atoi(*argv);
              break;
         case 'b':	/* set shell buffering */
              shellbuf = atoi(*argv);
              shellbuf = BETWEEN(5,shellbuf,1024);
              break;
         case 'B':	/* set window buffering */
              maxbuf = atoi(*argv);
              maxbuf = BETWEEN(1,maxbuf,shellbuf);
              break;
         case 'f':	/* set font directory */
              font_dir = *argv;
              type = 1;
              break;
         case 'i':	/* set icon directory */
              icon_dir = *argv;
              type = 1;
              break;
#ifdef MOVIE
         case 'Z':   /* set save_file argument  - start logging NOW */
              log_command = *argv;
              type = 1;
              log_now++;
#ifdef DEBUG
              dprintf(L)(stderr,"Starting logging NOW at [%s]\n",
								log_command);
#endif
              break;
         case 'z':   /* set save_file argument */
              log_command = *argv;
              type = 1;
#ifdef DEBUG
              dprintf(L)(stderr,"Starting logging LATER at [%s]\n",
								log_command);
#endif
              break;
#endif
         case 'S':	/* set alternate frame buffer */
              screen_dev = *argv;
              type = 1;
              break;
         default:	/* invalid flag */
             fprintf(stderr,"Invalid flag %c, ignored\r\n",type);
              type = 1;
              break;
         }
      }

   /* keep mgr from being run within itself */

   if (term && strcmp(TERMNAME,term)==0) {
      fprintf(stderr,"Can't invoke mgr from within itself\n");
      exit(1);
      }
   /* save tty modes for ptty's */

   save_modes(0);

   /* free all unused fd's */

   count = getdtablesize();
   for(i=3;i<count;i++)
      close(i);

#ifdef WHO
   save_utmp(ttyname(0));
#endif

   /* initialize the keyboard; sometimes a special device */
   initkbd();

   /* initialize the bell; sometimes a special device requiring funnys */
   initbell();

   /* get the default font file */

   if (default_font || (default_font = getenv(DEFAULT_FONT)))
      font = open_font(default_font);

   if (font == (struct font *) 0)
      font = open_font("");
   font->ident = 0;

   /* set up the default font names */

   /* open the mouse */

   if ((mouse=open(mouse_dev,0)) <0) {
      perror("can't find the mouse, or it is already in use\n");
      mouse = 0;
      }
   else if (set_mouseio(mouse) < 0)
      fprintf(stderr,"can't set mouse to exclusive use\n");

   mousex=mousey=32;

   /* find the screen */

#ifdef SHRINK
   if ((prime = bit_open(screen_dev)) == (BITMAP *) 0) {
      perror("can't find the screen");
      exit(2);
      }
	if (getenv("MGRSIZE")) {
		int x, y, w, h;
		sscanf(getenv("MGRSIZE"),"%d %d %d %d",&x,&y,&w,&h);
   	screen = bit_create(prime,x,y,w,h);
		}
	else
		screen = prime;
#else
   if ((screen = bit_open(screen_dev)) == (BITMAP *) 0) {
      perror("can't find the screen");
      exit(2);
      }
#endif

   set_tty(0);
   copyright(screen);

	/* other misc initialization */

	mouse_save = bit_alloc(32,32,0,BIT_DEPTH(screen));

   SETMOUSEICON(&mouse_cup);

   /* catch the right interrupts */

   for(i=0;i<NSIG;i++) switch(i) {
#ifdef MOVIE
      case SIGUSR1:     /* experimental logging stuff */
      case SIGUSR2:     signal(i,sig_share);
                        break;
#endif
      case SIGCHLD:     signal(SIGCHLD,sig_child);
                        break;
      case SIGILL:	/* <= 3.0 abort gererates this one */
      case SIGCONT:
      case SIGIOT:	/* 3.2 abort generates this (gee thanks, SUN!) */
      case SIGQUIT:
                        break;
      case SIGTTIN:
      case SIGTTOU:     signal(i,SIG_IGN);
                        break;
      default:          signal(i,catch);
                        break;
      }

   /* set the terminal type */

   if (term && strlen(term) >= strlen(TERMNAME))
      strcpy(term,TERMNAME);

	/* get default font definitions */

      {
      char buff[MAX_PATH];
      sprintf(buff,"%s/%s",font_dir,STARTFILE);
      startup(buff);
      }


#ifdef MOVIE
	/* start logging */

	if (log_now) {
		extern int no_initial;
		no_initial = 1;				/* no need to save initial display image */
		do_buckey('S' | 0x80);		/* simulate the key-press */
		}
#endif

   /* process startup file */

   startup(start_file);
   if (active != (WINDOW *) 0)
      ACTIVE_ON();
   else {
      MOUSE_OFF(screen,mousex,mousey);
      erase_win(screen,0,0);
      MOUSE_ON(screen,mousex,mousey);
   }

   /* turn on mouse cursor */

   MOUSE_OFF(screen,mousex,mousey);
   SETMOUSEICON(&mouse_arrow);
   MOUSE_ON(screen,mousex,mousey);

   /* void tty association 		(does bad things when mgr crashes) */

   /*  void_tty();       Flakey ... Commented Out! */

   /* always look for keyboard and mouse input */

   mask |= (1<<mouse) | (1<<0); 

   /* main polling loop */

   while(1) {

      /* see if any window died */

      for(win=active;win != (WINDOW *) 0;) 
         if (W(flags)&W_DIED) {
#ifdef DEBUG
            dprintf(d)(stderr,"Destroying %s-%d\r\n",W(tty),W(num));
#endif
            destroy(win);
            win = active;
            }
         else
            win = W(next);

      /* wait for input */

      reads = mask & ~poll;

#ifdef DEBUG
      dprintf(l)(stderr,"select: mask=0x%x poll=0x%x 0x%x got\r\n",
                 mask,poll,reads);
#endif
#ifdef MOVIE
		DOFLUSH();
#endif
      if (select(32,&reads,0,0,POLL(poll)) <0) {
#ifdef DEBUG
         dprintf(l)(stderr,"select failed %d->%d\r\n",
                 reads, mask & ~poll);
         if (debug) 
            perror("Select:");
#endif
         reads = mask & ~poll;
         continue;
         }
#ifdef DEBUG
      dprintf(l)(stderr,"0x%x\r\n",reads);
#endif

      /* process mouse */

      if (reads & (1<<mouse))
         do {
            proc_mouse(mouse);
            }
         while(mouse_count());
      
      /* process keyboard input */
      
      if (reads&1 && active && !(ACTIVE(flags)&W_NOINPUT)) {
         read(0,&c,1);
#ifdef BUCKEY
         if ( (ACTIVE(flags)&W_NOBUCKEY)  ||  !do_buckey(c) )
            write(ACTIVE(to_fd),&c,1);
#else
         write(ACTIVE(to_fd),&c,1);
#endif
			if (ACTIVE(flags)&W_DUPKEY && c==ACTIVE(dup))
         	write(ACTIVE(to_fd),&c,1);
         continue;
         }
       else if (reads&1 && !active) {		/* toss the input */
         read(0,&c,1);
#ifdef BUCKEY
         do_buckey(c);
#endif
	 }

      /* process shell output */

      for(win=active;win != (WINDOW *) 0;win=W(next)) {
         register int fd_bit = W(from_fd) ? 1<<W(from_fd) : 0;

         /* read data into buffer */

         if (fd_bit&reads&(~poll)) {
            W(current) = 0;
            if ((W(max) = read(W(from_fd),W(buff),shellbuf)) > 0) {
               poll |= fd_bit;
#ifdef DEBUG
               dprintf(p)(stderr,"%s: reading %d [%.*s]\r\n",W(tty),
                     W(max),W(max),W(buff));
#endif
               }
            else {
               poll &= ~fd_bit;
#ifdef KILL
               if (W(flags)&W_NOKILL)
                  W(flags) |= W_DIED;
#endif
#ifdef DEBUG
               if(debug) {
                  fprintf(stderr,"%s: Select boo-boo fd(%d) code %d\r\n",
                          W(tty),W(from_fd),W(max));
                  perror(W(tty)); 
                  }
#endif
               }
            }
 
         /* check for window to auto-expose */

         if (fd_bit&poll && W(flags)&W_EXPOSE && !(W(flags)&W_ACTIVE)) {
#ifdef DEBUG
            dprintf(m)(stderr,"%s: activating self\r\n",W(tty));
#endif
            MOUSE_OFF(screen,mousex,mousey);
            cursor_off();
            ACTIVE_OFF();
            expose(win);
            ACTIVE_ON();
            cursor_on();
            MOUSE_ON(screen,mousex,mousey);
            }

         /* write data into the window */

         if (fd_bit&poll && W(flags)&(W_ACTIVE|W_BACKGROUND)) {
            
#ifdef PRIORITY			/* use priority scheduling */
            if (win==active)
               count = Min(maxbuf,W(max)-W(current));
            else if (W(flags)&W_ACTIVE)
               count = Min(maxbuf>>1,W(max)-W(current));
            else
               count = Min(maxbuf>>2,W(max)-W(current));
#else				/* use round robin scheduling */
            count = Min(maxbuf,W(max)-W(current));
#endif

            i = put_window(win,W(buff)+W(current),count);
#ifdef DEBUG
            dprintf(w)(stderr,"%s: writing %d/%d %.*s [%.*s]\r\n",
                       W(tty),i,count,i,W(buff)+W(current),count-i,
                       W(buff)+W(current)+i);
#endif

            W(current) += i;
            if (W(current) >= W(max))
               poll &= ~fd_bit;
            }
         }
      }
   }

/*************************************************************************
 *	catch dead children 
 */

sig_child(sig)
int sig;
   {
   register WINDOW *win;
   extern int errno;

   /* see if a shell has died, mark deleted */

#ifdef DEBUG
   dprintf(d)(stderr,"Looking for dead windows\r\n");
#endif

   for(win=active;win != (WINDOW *) 0;win=W(next)) {
      if (kill(W(pid),0) != 0 && !(W(flags)&W_NOKILL)) {
         W(flags) |= W_DIED;
#ifdef DEBUG
         dprintf(d)(stderr, "%s-%d DIED\r\n",W(tty),W(num));
#endif
         }
      }
   }

/* process mouse */

int
proc_mouse(mouse)
int mouse;
   {
   int dx, dy;
   static int still_down = 0;
   register int button, done = 0;

   do {
      button = mouse_get(mouse,&dx,&dy);
      MOUSE_OFF(screen,mousex,mousey);
      mousex += 2*dx;
      mousey -= 2*dy;
      mousex = BETWEEN(0,mousex,BIT_WIDE(screen)-1);
      mousey = BETWEEN(0,mousey,BIT_HIGH(screen)-1);
      if (button != button_state) {
         do_button( button );
         done++;
         }
      MOUSE_ON(screen,mousex,mousey);
      } while (mouse_count() && !done);
   return(done);
   }

/* reopen the mouse after suspend */
      
int
mouse_reopen()
   {
   int m = open(mouse_dev,2);
   set_mouseio(m);
   return(m);
   }

/* return bitmap type */

int
get_bm_type()
   {
   return (bitmaptype);
   }

/* return time stamp (100th's of seconds since startup) */

int
timestamp()
	{
	static long offset = 0;
	struct timeval timeval;
	
	gettimeofday(&timeval,0l);
	
	if (offset == 0) {
		offset =  timeval.tv_sec;
		return(0);
		}
	else {
		return((timeval.tv_sec - offset) * 100 + timeval.tv_usec / 10000);
		}
	}
