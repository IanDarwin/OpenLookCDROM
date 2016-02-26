/*LINTLIBRARY*/
#ifndef lint
static char sccsid[] = "@(#)get.c 1.2 88/12/09" ;
#endif
	
/*  Extraction routines used by faces.
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
#include "extern.h"
#include "patchlevel.h"


get_blit_ikon(name, buf)      /* Load blit ikon file. */
char *name ;
unsigned short buf[256] ;
{
  FILE *fin ;
  char *ptr ;
  int i, j, temp ;

  if ((fin = fopen(name, "r")) == NULL) return(-1) ;
  for (i = 0; i < BLITHEIGHT; i++)
    {
      FGETS(nextline, MAXLINE, fin) ;
      ptr = nextline ;
      for (j = 0; j < 3; j++)
        {
          while (*ptr == ' ' || *ptr == '\t') ptr++ ;
          SSCANF(ptr,"0x%X",&temp) ;
          buf[i*4+j] = (short) temp ;
          ptr = index(ptr,',') ;
          ptr++ ;         
        }                 
      buf[i*4+3] = 0 ;    
    }
  for (i = BLITHEIGHT; i < ICONHEIGHT; i++)
    for (j = 0; j < 4; j++) buf[i*4+j] = 0 ;
  FCLOSE(fin) ;
  return(0) ;
}


char *
get_field(ftype,line)      /* Get field option from given line. */
enum field_type ftype ;
char *line ;
{
  char *ptr1, *ptr2 ;
  char str1[5],str2[MAXLINE],str3[4],str4[4],str5[3],str6[9],str7[5] ;

  SSCANF(line,"%s %s %s %s %s %s %s",
               str1,str2,str3,str4,str5,str6,str7) ;
  switch ((int) ftype)
    {
      case HOSTNAME  : if ((ptr2 = rindex(str2,'!')) != NULL)
                         {
                           ptr1 = str2 ;
                           *ptr2 = '\0' ;
                           while (ptr2 != ptr1)
                             {
                               *ptr2-- ;
                               if (*ptr2 == '!') return(ptr2+1) ;
                             }
                           return(ptr2) ;
                         }
                       else if ((ptr2 = index(str2,'@')) != NULL)
                         return(ptr2+1) ;
                       else return(NULL) ;
 
      case TIMESTAMP : str6[5] = '\0' ;
                       return(str6) ;
 
      case USERNAME  : if ((ptr2 = rindex(str2,'!')) != NULL)
                         return(ptr2+1) ;
                       else if ((ptr2 = index(str2,'@')) != NULL)
                         {
                           ptr1 = str2 ;
                           *ptr2 = '\0' ;
                           while (ptr2 != ptr1)
                             {
                               *ptr2-- ;
                               if (*ptr2 == '@') return(ptr2+1) ;
                             }
                           return(ptr2) ;
                         }
                       else return(str2) ;
    }
/*NOTREACHED*/
}


get_icon(dirname, buf)            /* Read in ikon or .icon file. */
char *dirname ;
unsigned short buf[256] ;
{

/*  Attempts to open the correct face file.
 *  If the face file is face.ps, then another record is added to the list
 *  of NeWS .ps files to animate at a later time.
 *  If this is 48x48x1 or sun.icon, and the open is successful, then the
 *  face image is read into buf.
 *  -1 is returned on failure.
 */

  char *ptr ;

  ptr = rindex(dirname,'/') ;     /* Find last slash in iconname. */
  if (EQUAL(ptr+1,"face.ps"))
    if (get_news_icon(dirname) == 0) return NEWSTYPE ;
  if (EQUAL(ptr+1,"sun.icon"))
    if (get_sun_icon(dirname, buf) == 0) return SUNTYPE ;
  if (EQUAL(ptr+1,"48x48x1"))
    if (get_blit_ikon(dirname, buf) == 0) return BLITTYPE ;
  return -1 ;
}


get_news_icon(name)    /* Create record for news.ps file. */
char *name ;
{
  FILE *fin ;

  if (gtype != NEWS) return -1 ;
  if ((fin = fopen(name, "r")) == NULL) return -1 ;
  FCLOSE(fin) ;
  add_ps_rec(row, column, name) ;
  return 0 ;
}


get_options(argc,argv)    /* Read and process command line options. */
int argc ;
char *argv[] ;
{
  char next[MAXLINE] ;    /* The next command line parameter. */

  INC ;
  while (argc > 0)
    {
      if (argv[0][0] == '-')
        switch (argv[0][1])
          {
            case 'P' : mtype = MONPRINTER ;  /* Monitor printer queue. */
                       INC ;
                       getparam(printer, argv, "-P needs printer name") ;
                       break ;
            case 'a' : mtype = MONALL ;      /* Monitor all of the spoolfile. */
                       break ;
            case 'b' : INC ;                 /* Alternate background pattern. */
                       getparam(bgicon, argv, "-b needs background icon") ;
                       break ;
            case 'f' : INC ;                 /* New directory for face icons. */
                       getparam(facedir, argv, "-f needs face directory") ;
                       SPRINTF(machfile, "%s/machine.tab", facedir) ;
                       SPRINTF(peopfile, "%s/people.tab", facedir) ;
                       break ;
            case 'i' : invert = 1 ;          /* Reverse video. */
                       break ;
            case 'n' : dontshowno = 1 ;      /* Don't show number of messages. */
                       break ;
            case 'p' : INC ;                 /* No. of seconds between checks. */
                       getparam(next, argv, "-p needs period time") ;
                       period = atoi(next) ; /* No. of seconds between checks. */
                       break ;
            case 's' : INC ;                 /* Alternative spoolfile. */
                       getparam(spoolfile, argv, "-s needs spool directory") ;
                       break ;
            case 't' : dontshowtime = 1 ;    /* Do not show timestamps. */
                       break ;
            case 'u' : dontshowuser = 1 ;    /* Do not show usernames. */
                       break ;
            case 'v' : FPRINTF(stderr, "%s version 1.3.%1d\n", progname, PATCHLEVEL) ;
                       exit(1) ;

/*  SunView windowing arguments. -Wp, -WP and -Wi are used in the NeWS
 *  implementation to initially position the window and icon.
 */

            case 'W' : switch (argv[0][2])
                         {
                           case 'H' : break ;   /* -WH, no sub-args follow */
                           case 'i' : iconic = 1 ;
                                      break ;   /* -Wi, start as an icon. */
                           case 'g' :           /* -Wg, set default color. */
                           case 'n' : break ;   /* -Wn, no label at all */
                           case 'h' :           /* -Wh, height */
                           case 'I' :           /* -WI "icon filename" */
                           case 'l' :           /* -Wl "some window label" */
                           case 'L' :           /* -Wl "some icon label" */
                           case 't' :           /* Font filename */
                           case 'T' :           /* Icon font filename */
                           case 'w' : INC ;     /* Width, in columns. */
                                      break ;
                           case 'p' : INC ;     /* -Wp xnum ynum */
                                      getparam(next, argv,
                                               "-Wp needs x coordinate") ;
                                      wx = atoi(next) ;
                                      INC ;
                                      getparam(next, argv,
                                               "-Wp needs y coordinate") ;
                                      wy = atoi(next) ;
                                      break ;
                           case 'P' : INC ;      /* -WP xnum ynum */
                                      getparam(next, argv,
                                               "-WP needs x coordinate") ;
                                      ix = atoi(next) ;
                                      INC ;
                                      getparam(next, argv,
                                               "-WP needs y coordinate") ;
                                      iy = atoi(next) ;
                                      break ;
                           case 's' : INC ; INC ;  /* -Ws xnum ynum */
                                      break ;
                           case 'b' :              /* -Wb r g b (bg color spec) */
                           case 'f' : INC ; INC ; INC ;  /* Same, fg color */
                                      break ;
                           default :  FPRINTF(stderr,"%s: -W%c unknown argument\n",
                                                      progname, argv[0][2]) ;
                                      break ;
                         }
                       break ;
            default  : FPRINTF(stderr, "Usage: %s [-P printer] ", progname) ;
                       FPRINTF(stderr, "[-Wi] [-Wp x y] [-WP x y] ") ;
                       FPRINTF(stderr, "[-b background] [-f facedir] [-i] [-n] ") ;
                       FPRINTF(stderr, "[-p period] [-s spoolfile] [-u] [-t] [-v]\n") ;
                       exit(1) ;
          }
      INC ;
    }
}


char *
getname()       /* Get users name from passwd entry. */
{
  char *getlogin(), *username ;
  struct passwd *getpwuid(), *pwent ;

  if (!(username = getlogin()))
    {
      pwent = getpwuid(getuid()) ;
      username = pwent->pw_name ;
      endpwent() ;                   /* Close the passwd file */
    }
  return username ;
}


getparam(s, argv, errmes)
char *s, *argv[], *errmes ;
{
  if (*argv != NULL && argv[0][0] != '-') STRCPY(s, *argv) ;
  else
    {
      FPRINTF(stderr,"%s: %s as next argument.\n", progname, errmes) ;
      exit(1) ;
    }
}


get_sun_icon(name, buf)     /* Load Sun icon file. */
char *name ;
unsigned short buf[256] ;
{
  FILE *fin ;
  char *ptr ;
  int i, j, temp ;

  if ((fin = fopen(name, "r")) == NULL) return -1 ;
  FGETS(nextline, MAXLINE, fin) ;        /* Load Sun icon file. */
  FGETS(nextline, MAXLINE, fin) ;
  for (i = 0; i < 32; i++)
    {
      FGETS(nextline, MAXLINE, fin) ;
      ptr = nextline ;
      for (j = 0; j < 8; j++)
        {
          while (*ptr == ' ' || *ptr == '\t') ptr++ ;
          SSCANF(ptr,"0x%X",&temp) ;
          buf[i*8+j] = (short) temp ;
          ptr = index(ptr, ',') ;
          ptr++ ;
        }
    }    
  FCLOSE(fin) ;
  return(0) ;
}
