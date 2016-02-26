/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.
 * Users may copy, modify or distribute this file at will.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 * Modifications to the original Sun Microsystems, Inc. source code
 * made by the Grasshopper Group are in the Public Domain.
 *
 * Extensions to this file by Eric Messick of the Grasshopper Group.
 *
 * Grasshopper Group
 * 212 Clayton St
 * San Francisco, CA 94117
 *
 */

#ifndef lint
static	char sccsid[] = "@(#)main.c 9.7 88/01/19 Copyright 1985 Sun Micro";
static	char RCSid[] = "@(#)$Header: /it/grass/gterm/RCS/main.c,v 2.17 1991/05/13 21:25:47 hugh Exp $";
#endif

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */


#include <stdio.h>
#include <psio.h>
#include <signal.h>
#include <pwd.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#ifdef SYSVREF
#ifdef INTERLANTCP
#include <interlan/il_types.h>
#include <interlan/socket.h>
#include <interlan/in.h>
#include <interlan/netdb.h>
#else
#include <sys/types.h>
#endif
#else
#ifdef REF
#include <sys/types.h>
#endif
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#endif

#ifdef REF
#include <ref/config.h>
#endif

#ifdef SYSVREF
#define	SIGCHLD	SIGCLD
#else
#include <sys/wait.h>
#endif

/*#define DEBUG*/
#ifdef DEBUG
#define err0(A) fprintf(stderr,A)
#define err1(A,B) fprintf(stderr,A,B)
#define err2(A,B,C) fprintf(stderr,A,B,C)
#define err3(A,B,C,D) fprintf(stderr,A,B,C,D)
#else
#define err0(A)
#define err1(A,B)
#define err2(A,B,C)
#define err3(A,B,C,D)
#endif

extern void exit();
extern void perror();
extern char *strcpy();
#ifndef bcopy
extern void bcopy();
#endif
extern void endpwent();
extern void free();
extern char *strcat();

extern	PSFILE *Connect_To_Server();

int (*tc_parse)();

int	console;
static	int KillChild();
static	int ReapChild();
static	char *term = NULL;
static	int seed = 0;
static	int Persist;
static	int loginshell = 0;
static	int tflag = 0;
static	char *framelabel = NULL;
static	char *iconlabel = NULL;
static	char *userinit = "" ;
static	char *font = "" ;
static	int xorg = -1;
static	int yorg = -1;
static	int iconx = -1;
static	int icony = -1;
static	int reload = 0;
static	int starticonic = 0 ;
static	int save_lines = 0 ;
static  char *display = NULL;
int	fontsize = -1;
int	BackGround = 1;
int	PageMode = 0;			/* "pause on end of page" mode */
int	userCharsPerLine = -1;
int	userLinesPerScreen = -1;

main(argc,argv)
    int	argc;
    char **argv;
{
    int retval;
    char *s_name, **s_args, *argv0 = argv[0];
    FILE *Client;
    PSFILE *Keyboard;
    extern char *getenv();
    extern PSFILE *tc_init_screen();
    extern FILE *spawn_slave();
    extern int tc_display(), tc_display_dumb(), tc_display_psterm();

    for (argc--, argv++; argc > 0 && argv[0][0] == '-'; argc--, argv++) {
	if (strcmp(argv[0], "-bg") == 0 || strcmp(argv[0], "-ga") == 0) {
	    BackGround++;		/* put ourself in the background */
	    continue;
	}
	if (strcmp(argv[0], "-C") == 0) {
	    console++;
	    continue;
	}
	if (strcmp(argv[0], "-co") == 0) {
	    if (--argc > 0)
		userCharsPerLine = atoi(*++argv);
	    continue;
	}
	if (strcmp(argv[0], "-display") == 0) {
		if (--argc > 0) {
		  display = *++argv;
	  }
		continue;
	}
	if (strcmp(argv[0], "-F") == 0) {
	    if (--argc > 0) {
		font = *++argv;
	    }
	    continue;
	}
	if (strcmp(argv[0], "-f") == 0) {
	    continue;
	}
	if (strcmp(argv[0], "-fg") == 0) {
	    BackGround = 0 ;		/* don't put ourself in the background */
	    continue;
	}
	if (strcmp(argv[0], "-fl") == 0) {
	    if (--argc > 0)
		framelabel = *++argv;
	    continue;
	}
	if (strcmp(argv[0], "-fs") == 0 || strcmp(argv[0], "-fontsize") == 0) {
	    if (--argc > 0)
		fontsize = atoi(*++argv);
	    continue;
	}
	if (strcmp(argv[0], "-ic") == 0) {
	    starticonic++;
	    continue;
	}
	if (strcmp(argv[0], "-il") == 0) {
	    if (--argc > 0)
		iconlabel = *++argv;
	    continue;
	}
	if (strcmp(argv[0], "-ixy") == 0) {
	    if (--argc > 0)
		iconx = atoi(*++argv);
	    if (--argc > 0)
		icony = atoi(*++argv);
	    if (iconx >= 0 && icony >= 0)
		continue;
	}
	if (strcmp(argv[0], "-li") == 0) {
	    if (--argc > 0)
		userLinesPerScreen = atoi(*++argv);
	    continue;
	}
	if (strcmp(argv[0], "-ls") == 0) {
	    loginshell++;
	    continue;
	}
	if (strcmp(argv[0], "-pm") == 0 || strcmp(argv[0], "-ps") == 0) {
	    PageMode++;			/* enable page mode */
	    continue;
	}
	if (strcmp(argv[0], "-r") == 0) {
	    reload++;
	    continue;
	}
	if (strcmp(argv[0], "-s") == 0) {
	    if (--argc > 0)
		seed = atoi(*++argv);
	    continue;
	}
	if (strcmp(argv[0], "-sl") == 0) {
	    if (--argc > 0)
		save_lines = atoi(*++argv);
	    continue;
	}
	if (strcmp(argv[0], "-t") == 0) {
	    if (--argc > 0) {
		term = *++argv;
		tflag++;
	    }
	    continue;
	}
	if (strcmp(argv[0], "-ui") == 0) {
	    if (--argc > 0) {
		userinit = *++argv;
	    }
	    continue;
	}
	if (strcmp(argv[0], "-w") == 0) {
	    Persist++;
	    continue;
	}
	/* We ignore this to keep command line commpattabilty with psterm */
	if (strcmp(argv[0], "-wh") == 0) {
	    if (--argc > 0)
		++argv;
	    if (--argc > 0)
		++argv;
	    continue;
	}
	if (strcmp(argv[0], "-xy") == 0) {
	    if (--argc > 0)
		xorg = atoi(*++argv);
	    if (--argc > 0)
		yorg = atoi(*++argv);
	    if (xorg >= 0 && yorg >= 0)
		continue;
	}
	fprintf(stderr, "Usage: %s %s \\\n\t%s \\\n\t%s \\\n\t%s\n", argv0,
		"[-bg] [-C] [-co columns] [-display hostname] [-F font] [-f]",
		"[-fg] [-fl framelabel] [-fs fontsize] [-ic] [-il iconlabel]",
		"[-ixy x y] [-li lines] [-ls] [-pm] [-r] [-sl savelines]",
		"[-t termtype] [-ui userinit] [-w] [-xy x y] [command]");
	(void) Connect_To_Server();
	Fatal("gterm:  error in arguments", (char *)NULL);
    }
    if (display) {
	    set_newsserver(display);
    }
    if (loginshell) {
	char *s, *p;
#ifndef SYSVREF
	char *rindex(), *index();
#else
#define index(s, c)		(char *)strchr(s, c)
#endif
	struct passwd *pw, *getpwuid();
	static char *loginargv[4];

	s_name = "/bin/sh";		/* default shell name */
	s_args = loginargv;

	s_args[0] = "-sh";
	s_args[1] = 0;
	if (pw = getpwuid(getuid())) {
	    if (pw->pw_dir)
		set_environment_var("HOME", pw->pw_dir);
	    if (pw->pw_name) {
		set_environment_var("LOGNAME", pw->pw_name);
	        set_environment_var("USER", pw->pw_name);
	    }
	    if (pw->pw_shell) {
		if (s = rindex(pw->pw_shell,'/')) {
		    p = (char *)malloc ((unsigned)strlen(s)+4);
		    if (p) {
			sprintf(p, "-%s", s+1);
			s_args[0] = p;
		    }
		    p = (char *)malloc((unsigned)strlen(pw->pw_shell)+4);
		    if (p) {
			s_name = p;
			strcpy(s_name, pw->pw_shell);
		    }
		    set_environment_var("SHELL",pw->pw_shell);
		}
	    }
	    endpwent();
	}
	if (argc > 0) {
	    int i, count = 0;
	    /*
	     * This goofy business in case someone wants to run complex
	     * shell commands after login-shell initialization...
	     */
	    for (i=0; i<argc; i++) {
		count += strlen(argv[i]);
	    }
	    if (s_args[2] = (char *)malloc((unsigned)count+argc+4)) {
		s_args[2][0] = 0;
		for (i=0; i<argc; i++) {
		    if (i)
			strcat(s_args[2], " ");
		    strcat(s_args[2], argv[i]);
		}
		s_args[0]++;
		s_args[1] = "-c";
		s_args[3] = 0;
	    }
	}
    } else if (argc > 0) {
	s_name = *argv;
	s_args = argv;
    } else {
#ifndef SYSVREF
	static char *def_argv[] = { "csh", NULL};
#else
	static char *def_argv[] = { "-sh", NULL};
#endif

	s_args = def_argv;
	if ((s_name = getenv("SHELL")) == NULL) {
#ifndef SYSVREF
	    s_name = *def_argv;
	    s_args[0] = s_name;
#else
	    s_name = "sh";
#endif
	}
	else {
	    s_args[0] = s_name;
	}
    }
    /*
     * if $NEWSSERVER isn't defined, and stdin is a socket, chances
     * are we're being invoked with rsh, so figure out where the
     * rsh command came from and use that for NEWSSERVER.
     */
    if (getenv("NEWSSERVER")==0) {
	    if (getenv("DISPLAY")) {
		    set_newsserver(getenv("DISPLAY"));
	    } else {
		    struct hostent *hp;
		    struct sockaddr_in remote;
		    int n = sizeof remote;
		    if (getpeername(0, (struct sockaddr *)&remote, &n) == 0) {
			    if (remote.sin_family == AF_INET) {
				    char srv[128];
				    sprintf(srv, "%lu.%d;",
					  ntohl(remote.sin_addr.s_addr), 2000);
				          hp = gethostbyaddr(
						    (char*) &remote.sin_addr,
						    sizeof (remote.sin_addr),
						    remote.sin_family);
				    if (hp) strcat(srv,hp->h_name);
				    set_environment_var("NEWSSERVER", srv);
			    }
		    }
	    }
    }
    if (getenv("DISPLAY") == 0) set_display(getenv("NEWSERVER"));
    if ( !tflag || term == NULL) {
#ifdef RandomBehaviour
	/* Choose one at random - should scan termcap? */
	static char * def_term[] = {
	    "ansi", "h19", "wyse",
	};

	if (seed == 0)
	    seed = getpid();
	srand(seed);
	term = def_term[(rand()>>4)%((sizeof def_term)/(sizeof def_term[0]))];
#else
	term = "psterm";
#endif
    }
    tc_parse = tc_display; /* the default case */
    if ( ! strcmp(term, "dumb") ) tc_parse = tc_display_dumb;
    if ( ! strcmp(term, "psterm") ) tc_parse = tc_display_psterm;
    if ( framelabel == NULL  &&  tc_parse != tc_display ) {
	    char *p;
	    static char labelbuf[128];
	    /* FIXUP: If host has a different domain then the server,
	    ** use the whole name! */
	    gethostname(labelbuf, sizeof labelbuf);
	    if ((p = index(labelbuf, '.')) != NULL)
	      *p = 0;
	    if (argc > 0) {
		    strcat(labelbuf, " ");
		    strcat (labelbuf, argv[0]);
	    }	    
	    framelabel = labelbuf;
    }
    if (iconlabel == NULL)
      iconlabel = framelabel;
    if (BackGround) {
	int i;
	/*
	 * Close any extraneous files.
	 */
	for (i = getdtablesize(); i > 2; i--)
	    close(i);
	retval = open(CONSOLE_FILENAME, O_WRONLY);
	if (retval) {
		/* Humm, maybe open /dev/null as stderr insted? */
		dup2(retval, 2);
		close(retval);
	}
	close(1); close(0);
    }
    if (xorg < 0)
	xorg = 0;
    if (yorg < 0)
	yorg = 0;
    err0("tc_init_screen()...\n");
    Keyboard =
	tc_init_screen(term, xorg, yorg, fontsize, framelabel, iconlabel,
			reload, userinit, font, starticonic, iconx, icony, save_lines);
    err0("tc_init_screen() done;\n");
    if (Keyboard == NULL) {
	char *foo;
	foo = getenv("NEWSSERVER");
	if (foo)
	    Fatal("Can't connect to NeWS server at: %s", foo);
	else
	    Fatal("Can't connect to NeWS server; did you set NEWSSERVER?",
		  (char *)NULL);
    }

    tc_initmodemenu();
    set_environment_var("TERM", term);
    err1("spawning: %s :\n", s_name);
    if ((Client = spawn_slave(s_name, s_args)) == NULL)
	Fatal("Cannot spawn %s", s_name);
    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
	signal(SIGINT, KillChild);
    if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
	signal(SIGQUIT, KillChild);
    signal(SIGHUP, KillChild);
    signal(SIGTERM, KillChild);
    signal(SIGCHLD, ReapChild);
    signal(SIGWINCH, SIG_IGN);
    err0("terminal()...\n");
    terminal(fileno(Client), psio_fileno(Keyboard));
    Exit(0);
    /*NOTREACHED*/
}

Fatal(fmt, a1)
char *fmt, *a1;
{
	extern int ConnectionEstablished;
	char buf[1024];

/* Dont use PostScriptErrorMesage as in tnt1.0 popmsg is buggy! -HD */
	if (ConnectionEstablished) {
		/*FIXUP: Do something here to kill off PostScript Side */
		KillPSSide();
	}
	fprintf(stderr, "gterm: ");
	fprintf(stderr, fmt, a1);
	fprintf(stderr, ".\n");
	Exit(1);
}

Exit(status)
{

    CleanupPty();
    exit(status);
}

static
KillChild(sig)
{
    extern int pgrp;

    if (pgrp != 0)
	killpg(pgrp, sig);
    CleanupPty();
    signal(sig, SIG_DFL);
    kill(0, sig);
}

static
ReapChild()
{
    int error_code, code2, code3, code4;
#ifdef HAVE_WAIT3
    union wait status;
    int pid = wait3(&status, WNOHANG, 0);
    error_code = status.w_retcode;
/*    code2 = WEXITSTATUS(status);
    code3 = status.w_termsig;
    code4 = status.w_status;*/
#else	/* !HAVE_WAIT3 */
    int pid = wait(&error_code);
    signal(SIGCHLD, ReapChild);
#endif

    if (pid < 0) {
	perror("fruitless wait3");
    } else if (Persist == 0) {
	if (error_code) {
/*		fprintf(stderr, 
			"child %d exit status=%d=0x%x Termsig=%d all=0x%x\n",
			pid, code2, code2, code3, code4);
*/		Fatal("child exit status %d ", error_code);
	} else {
		err0("Clean ReapChild.\n");
		Exit(0);
	} 
    } else { /* Persist true */
	    /*sleep(2);*/
		Fatal("waited child exit status %d ", error_code);
    }
}

set_environment_var(name, value)
char       *name, *value; {
    register    len;
    register char **ap;
    register char **new;
    register char *buf;
    static      alloced = 0;
    extern char **environ;

    len = strlen(name);
    buf = (char *) malloc((unsigned)(len + strlen(value) + 2));
    if (buf==NULL) return;
    sprintf(buf, "%s=%s", name, value);
    for (ap = environ; *ap; ap++)
	if (strncmp(*ap, buf, len+1) == 0) {
	    *ap = buf;
	    return;
	}
    len = ap - environ;
    new = (char **) malloc((unsigned)((len + 2) * sizeof(char *)));
    if (new==NULL) return;
    bcopy((char *)environ, (char *)new, len * sizeof(char *));
    new[len] = buf;
    new[len + 1] = 0;
    if (alloced)
	free((char *)environ);
    alloced = 1 ;
    environ = new;
}

unsetenv(name)
char *name;
{
	register    len;
	register char **ap;
	register char **new;
	extern char **environ;

	len = strlen(name);
	for (new = ap = environ;  (*new = *ap) != NULL;  ap++) {
		if (strncmp(*ap, name, len) == 0  &&  (*ap)[len] == '=') {
			/* Memory leak bug: we cannot free(*ap) here, because we don't know
			 * whether *ap was created with putenv(). */
			}
		else	new++;
		}
}

 /*  Sets the environment variable DISPLAY from the passed string, having
 **  striped off leading characters before a ';'.  In other
 **  words you can pass it a NEWSSERVER Var.
 */
set_display(str)
  char *str;
{
  int n;
  char *a, *p;

        /* Does not check to see if the host is known */
        if (str == NULL || (n = strlen(str)) == 0) return;
        p = (char *)malloc(n+3);
	/* Untested malloc return... */
	a = index(str, ';');
	if (a) {
		++a;
	} else {
		a = str;
	}
        if (index(str, ':')) {
		strcpy(p, a);
	} else {
		sprintf(p, "%s:0.0", a);
	}
	set_environment_var("DISPLAY", p);
        free(p);
}

  /*  check to see if the host name in the passed string is know, if
  **  so it sets the NEWSERVER and DISPLAY envrionment vars.
  */
set_newsserver(str)
  char *str;
{
	/* IMPROVE: Check to see if this is allready NEWSSERVER var */
	int n;
	struct hostent *hp;
	char *loop, *display;

        if (str == NULL || (n = strlen(str)) == 0) return;
	if (n == 0) return;
	display = (char *)malloc(n);
	strcpy(display, str);
	/* Untested malloc return... */
	for(loop=display; *loop != NULL; ++loop)
	  if (*loop == ':') *loop = NULL;
	if((hp = gethostbyname(display)) != NULL) {
		char srv[512];
		sprintf(srv, "%lu.%d;%s%s",
			ntohl(*(u_long *)hp->h_addr), 2000, display,
			(index(str, ':')) ? index(str, ':') : "");
		set_environment_var("NEWSSERVER", srv);
		set_display(getenv("NEWSSERVER"));
	} else {
		fprintf(stderr,"gterm: unknown -display host '%s'\n",
			display);
		Exit(1);
	}
	free(display);
}
