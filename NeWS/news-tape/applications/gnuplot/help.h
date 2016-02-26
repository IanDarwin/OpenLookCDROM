	/*
         *	help.h: header file for the VMS-help emulator
	 */

#include <stdio.h>
#include <ctype.h>
#define BSD
/* #define USG */
#ifdef USG
#include <sys/types.h>
#endif
#include <sys/file.h>
#include <sys/param.h>
#include <sys/dir.h>
#ifdef BSD
#include <sys/ioctl.h>
#include <sys/signal.h>
#endif


#define iswhite(_c)       (_c ==' ' || _c =='	' || _c =='\n' || _c =='\r')
#define PROMPT " Topic? "
#define SUBPROMPT "subtopic? "
#define HELPEX	".HLP"				/* help extension */
#define	MANEX	".MANUAL"			/* manual filename*/
#define MAN_SUBEX "/.MANUAL"                    /* other man filename */
#define XREFEX  ".XREF"                         /* cross reference */
#define INFOHEAD "file "
#define INFOTAIL " | sed 's/^/     /'"

#define LF '\n'
#define RET '\r'
#define ESC '\033'

#define	MAXLINELEN	128
#define	MAXNAMELEN	 24		/* max length of NAME of topix */
#define	MAXNAMES	256		/* maximum number of subtopics */
#define COLUMNSPACE	  3
#define TERMWID		 7
/* #define HELPDIR "/usr/local/help" */
#define HELPDIR "help.dir"
#define VIEWPROGRAM  "/usr/ucb/more"    /* program to look at text */
#define VIEWPROGOPTS1 "-d"
#define VIEWPROGOPTS2 "-18"
#define SHELLPROG    "/bin/sh"		/* program to execute text */
#define SHELLOPTS    "-c"

char 	progname[MAXNAMELEN];
char	olddir[MAXNAMELEN + 68];
char	newdir[MAXNAMELEN + 68];
char	*helpdir;
char	**environ;	/* environment, for forks */
char    gbl_ppt[MAXLINELEN];

int	dumb_flag;
int	list_flag;
int	col_flag;
int	frst_flag;

char	*helpcmds[] =
	{ "Return - Exit this level of help",
	  "*	  - Print this message",
	  "?	  - Reprint this help and its subtopics",
	  "#	  - Reprint just subtopics of this help",
	  ".	  - Look at current manual page, if any",
	  "$<topic>	- Get information on topic files",
	  ".<topic>	- Look at topic manual page, if any",
	  "<topic> 	- Look at help for subtopic `topic'" ,
	  NULL
	};

#ifdef BSD
struct tchars  *init_tchars = NULL;
struct tchars  *here_tchars = NULL;

#define Set_Tc_Init(FD) \
        ((init_tchars==NULL)?(0):(ioctl(FD,TIOCSETC,init_tchars)))
#define Set_Tc_Here(FD) \
        ((here_tchars==NULL)?(0):(ioctl(FD,TIOCSETC,here_tchars)))

int is_tty = 0;
char *read_resp();

#define ESC_COMP       1
#define CTRLD_COMP    -1
#define SPEC_CHARS  ".$?*#"

char gbl_match[MAXNAMELEN];

#else
#define Set_Tc_Init  1
#define Set_Tc_Here  0
#endif
