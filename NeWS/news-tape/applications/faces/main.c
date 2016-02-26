#ifndef lint
static char sccsid[] = "@(#)main.c 1.2 88/12/11" ;
#endif

/*  Icon face server for monitoring mail and print jobs.
 *  This program is based on the AT&T v8 mail face server,
 *  vismon, but is not derived from vismon source.
 * 
 *  Copyright (c) Rich Burridge - Sun Microsystems Australia.
 *                                All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged. 
 * 
 *  No responsibility is taken for any errors on inaccuracies inherent
 *  either to the comments or the code of this program, but if reported
 *  to me, then an attempt will be made to fix them.
 */

#include "faces.h"

char *get_field(), *getname() ;
int do_check() ;
struct recinfo *rec_exists() ;

struct comminfo *communities = NULL ;  /* Community alias/username chain. */
struct comminfo *clast = NULL ;     /* End of chain of community records. */
struct machinfo *machines = NULL ;  /* Known machine/communities. */
struct machinfo *mlast = NULL ;     /* End of chain of machine records. */
struct psinfo *psrecs = NULL ;      /* List of news.ps animation files. */
struct psinfo *plast = NULL ;       /* End of chain of NeWS animation files. */
struct recinfo *recs = NULL ;       /* Mail messages being monitored. */
struct recinfo *last = NULL ;       /* End of the chain of mail records. */
struct stat buf ;                   /* Buffer for file existence stat call. */
long lastsize ;                     /* Last known size of the mail folder. */

enum gr_type gtype ;     /* Indicates what graphics system is being used. */
enum mon_type mtype ;    /* Type of monitoring operation to do. */

char bgicon[MAXLINE] ;           /* Alternate background pattern. */
char community[MAXLINE] ;        /* Community name ("real" host name). */
char facedir[MAXLINE] ;          /* Directory containing face images. */
char fname[MAXTYPES][MAXLINE] ;  /* Array of various face name types. */
char iconname[MAXLINE] ;    /* Name of the icon file for this person. */
char machfile[MAXLINE] ;    /* Name of the machine/community file. */
char nextline[MAXLINE] ;    /* Next line from users mail spool file. */
char peopfile[MAXLINE] ;    /* Name of the people/username file. */
char printer[MAXLINE] ;     /* Printer name to monitor. */
char progname[MAXLINE] ;    /* Name of this program. */
char spoolfile[MAXLINE] ;   /* Full pathname of users current mail. */
char *username ;            /* This users name. */

int beeps ;         /* Number of beeps for arrival of new mail. */
int column ;        /* Column number for next icon. */
int dontshowno ;    /* Indicates if number of messages should be displayed. */
int dontshowtime ;  /* Indicates if timestamp for new mail should be shown. */
int dontshowuser ;  /* Indicates if username for new mail should be shown. */
int facetype ;      /* Type of face file found. */
int firsttime ;     /* Zeroised after first mail/printer check. */
int flashes ;       /* Number of flashes for arrival of new mail. */
int height ;        /* Height in pixels of faces display. */
int iconic ;        /* Start as an icon if set. */
int invert ;        /* Set if to use reverse video. */
int ix ;            /* Initial X position of the icon. */
int iy ;            /* Initial Y position of the icon. */
int newmail ;       /* Set if there is new mail this time around. */
int noicons ;       /* Number of faces this time around. */
int period ;        /* Period in seconds for new mail check. */
int row ;           /* Row number for next icon. */
int width ;         /* Width in pixels of faces display. */
int wx ;            /* Initial X position of the window. */
int wy ;            /* Initial Y position of the window. */


main(argc,argv)
int argc ;
char *argv[] ;
{
  STRCPY(progname,argv[0]) ;    /* Save this programs name. */
  initialise() ;                /* Set default values for various options. */
  get_options(argc,argv) ;      /* Read and process command line options. */
  read_mailrc() ;               /* Get number of flashes and beeps. */
  read_machines() ;             /* Setup up the machine/community chain. */
  read_aliases() ;              /* Setup the hostname alias subchains. */
  if (init_ws_type())           /* Determine window system type. */
    {
      FPRINTF(stderr,"Error initialising window system.\n") ;
      exit(1) ;
    }
  make_icon() ;                 /* Create default faces icon. */
  make_frame(argc,argv) ;       /* Create faces window/icon. */
  do_check() ;                  /* Generate initial faces display. */
  start_tool() ;                /* Start up and display the faces icon. */
}


a_to_u(community, user, realname)    /* Turn possible alias into username. */
char *community, *user, *realname ;
{
  struct comminfo *ctemp ;
  struct peopinfo *ptemp ;

  STRCPY(realname, user) ;      /* In case alias not found. */
  ctemp = communities ;         /* Point to chain of communities. */
  while (ctemp != NULL)
    if (EQUAL(ctemp->community, community))
      {
        ptemp = ctemp->people ;
        while (ptemp != NULL)
          if (EQUAL(ptemp->alias, user))
            {
              STRCPY(realname, ptemp->username) ;
              return ;
            }
          else ptemp = ptemp->next ;
        return ;
      }
    else ctemp = ctemp->next ;
}


initialise()
{
  lastsize = 0 ;              /* Initial size of spoolfile. */
  firsttime = 1 ;             /* No checks made yet. */
  iconic = 0 ;                /* Initially an open window. */
  STRCPY(fname[BLITTYPE], "48x48x1") ;
  STRCPY(fname[SUNTYPE], "sun.icon") ;
  STRCPY(fname[NEWSTYPE], "face.ps") ;
  wx = wy = ix = iy = 0 ;

#ifdef FBMONTYPE
  mtype = FBMONTYPE ;         /* Type of monitoring to do. */
#else
  mtype = MONNEW ;            /* Just monitor new mail by default. */
#endif FBMONTYPE

#ifdef BACKGROUND
  STRCPY(bgicon, BACKGROUND) ;          /* Alternate background pattern. */
#else
  STRCPY(bgicon, "") ;                  /* Default is to use root gray. */
#endif BACKGROUND

#ifdef FACEDIR
  STRCPY(facedir,FACEDIR) ;  /* Different directory for face icons. */
#else
  STRCPY(facedir,"/usr/local/faces") ;  /* Directory for face icons. */
#endif FACEDIR

  SPRINTF(machfile, "%s/machine.tab", facedir) ;
  SPRINTF(peopfile, "%s/people.tab", facedir) ;

#ifdef INVERT
  invert = INVERT ;          /* Set indicates reverse video. */
#else
  invert = 0 ;               /* Default is normal video. */
#endif INVERT

#ifdef PERIOD
  period = PERIOD ;          /* Period for new mail/print check. */
#else
  period = 60 ;              /* Default time in seconds for new check. */
#endif PERIOD

#ifdef DONTSHOWNO
  dontshowno = DONTSHOWNO ;  /* If nonzero, do not show number of messages. */
#else
  dontshowno = 0 ;           /* Show number of messages for this face. */
#endif DONTSHOWNO

#ifdef SPOOLFILE
  STRCPY(spoolfile,SPOOLFILE) ;   /* Alternative spoolfile to monitor. */
#else
  username = getname() ;     /* Get users name from passwd entry. */
  SPRINTF(spoolfile,"/usr/spool/mail/%s",username) ;
#endif SPOOLFILE

#ifdef DONTSHOWTIME
  dontshowtime = DONTSHOWTIME ;    /* If nonzero, do not show timestamp. */
#else
  dontshowtime = 0 ;         /* Show time stamp for new mail for this face. */
#endif DONTSHOWTIME

#ifdef DONTSHOWUSER
  dontshowuser = DONTSHOWUSER ;    /* If nonzero, do not show username. */
#else
  dontshowuser = 0 ;         /* Show username on the face icons. */
#endif DONTSHOWUSER
}


h_to_c(host, community)        /* Turn hostname into community name. */
char *host, *community ;
{
  struct machinfo *temp ;      /* Pointer to next machine record. */
 
  temp = machines ;            /* Point to list of machine/communitys. */
  if (host[0] == '\0') GETHOSTNAME(community, MAXLINE) ;
  else STRCPY(community, host) ;   /* Copied in case machine name not found. */
  while (temp != NULL)
    {
      if (EQUAL(temp->machine, community))
        {
          STRCPY(community, temp->community) ;
          return ;
        }
      temp = temp->next ;      /* Point to next record. */
    }
}


make_iconname(community, user)    /* Construct the icon name. */
char *community, *user ;
{

/*  Sets up community and user based on the first successful
 *  open from the following list of files:
 *
 *  $(FACEDIR)/community/user/[face.ps, sun.icon, 48x48x1]
 *  $(FACEDIR)/misc./user/[face.ps, sun.icon, 48x48x1]
 *  $(FACEDIR)/community/unknown/[face.ps, sun.icon, 48x48x1]
 *  $(FACEDIR)/misc./unknown/[face.ps, sun.icon, 48x48x1]
 *
 *  If none of these are found, the "blank face" is returned.
 */

  int i ;

  for (i = 0; i < MAXTYPES; i++)
    {
      SPRINTF(iconname, "%s/%s/%s/%s", facedir, community, user, fname[i]) ;
      if (stat(iconname, &buf) != -1) return ;
    }
 
  for (i = 0; i < MAXTYPES; i++)
    {
      SPRINTF(iconname, "%s/misc./%s/%s", facedir, user, fname[i]) ;
      if (stat(iconname, &buf) != -1)
        {
          STRCPY(community, "misc.") ;
          return ;
        }
    }
 
  for (i = 0; i < MAXTYPES; i++)
    {
      SPRINTF(iconname, "%s/%s/unknown/%s", facedir, community, fname[i]) ;
      if (stat(iconname, &buf) != -1)
        {
          STRCPY(user, "unknown") ;
          return ;
        }
    }

  for (i = 0; i < MAXTYPES; i++)
    {
      SPRINTF(iconname, "%s/misc./unknown/%s", facedir, fname[i]) ;
      if (stat(iconname, &buf) != -1)
        {
          STRCPY(community, "misc.") ;
          STRCPY(user, "unknown") ;
          return ;
        }
    }

/* The "blank face" should be used, so community and user are nulled. */

  community[0] = user[0] = '\0' ;
}


read_mailrc()                /* Get number of flashes and beeps. */
{
  char mrcname[MAXLINE] ;    /* Full pathname of the .mailrc file. */
  char *ptr ;
  FILE *fd ;

  beeps = 0 ;     /* Defaults if .mailrc file not found. */
  flashes = 0 ;
  if (getenv("HOME"))
    {
      SPRINTF(mrcname, "%s/.mailrc", getenv("HOME")) ;
      if ((fd = fopen(mrcname, "r")) == NULL) return ;
      while (fgets(nextline, MAXLINE, fd) != NULL)
        if (EQUAL(nextline, "set"))
          {
            ptr = index(nextline, ' ') ;
            if (EQUAL(ptr+1, "flash"))
              {
                ptr = index(nextline, '=') ;
                SSCANF(ptr+1, "%d", &flashes) ;
              }
            else if (EQUAL(ptr+1, "bell"))
              {
                ptr = index(nextline, '=') ;
                SSCANF(ptr+1, "%d", &beeps) ;
              }
          }
    }
  FCLOSE(fd) ;
}
