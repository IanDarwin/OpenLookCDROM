/*LINTLIBRARY*/
#ifndef lint
static char sccsid[] = "@(#)mon.c 1.4 88/12/11" ;
#endif

/*  Monitoring routines used by the faces program.
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

adjust()          /* Adjust the row and column position. */
{
  struct psinfo *this ;

  if (mtype != MONNEW)
    {
      if (++column == NO_PER_ROW)
        {
          column = 0 ;
          row++ ;
        }
      return ;
    }
  if (psrecs != NULL)           /* Adjust animation positions for MONNEW */
    {
      this = psrecs ;           /* Point to beginning of chain. */
      while (this != NULL)
        {
          this->column++ ;      /* Adjust column position. */
          if (facetype == NEWSTYPE && this->next == NULL)
            this->column-- ;    /* Reset for brand new animate record. */
          this = this->next ;
        }
    }
}


do_check()        /* Perform another check of the appropriate type. */
{
  switch ((int) mtype)
    {
      case MONALL     : do_mail(MONALL) ;  /* Monitor all of the mail file. */
                        break ;
      case MONNEW     : do_mail(MONNEW) ;  /* Monitor new mail only. */
                        break ;
      case MONPRINTER : do_printer() ;     /* Monitor the print queue. */
                        break ;
    }
  firsttime = 0 ;
}


do_mail(mtype)              /* Monitor a mail file for new or all mail. */
enum mon_type mtype ;
{        
  char host[MAXLINE] ;      /* Pointer to host name from the "From" line. */
  char *ptr ;               /* Pointer to extract field. */
  char realname[MAXLINE] ;  /* Real username for this user. */
  char ts[MAXLINE] ;        /* Pointer to time stamp from the "From" line. */
  char user[MAXLINE] ;      /* Pointer to user name from the "From" line. */
  FILE *fp ;                /* File descriptor for users mail spool file. */
  struct recinfo *crec ;    /* Pointer to current mail record for updating. */

  column = row = 0 ;        /* Start in top left corner of pixrect. */
  newmail = 0 ;             /* Assume no new mail. */
  noicons = 0 ;

  if (mtype == MONNEW) make_pixrect(NO_PER_ROW) ;
  if (stat(spoolfile,&buf) == -1)
    {
      lastsize = 0 ;
      if (mtype == MONNEW) show_display() ;   /* Show new mail. */
      else make_display() ;     /* Output icons and tidyup chain of records. */
      return ;
    }
  if (buf.st_size > lastsize) newmail = 1 ;   /* New mail found. */
  if (mtype == MONNEW)
    if (buf.st_size <= lastsize)   /* Is the size of mail folder bigger? */
      {
        lastsize = buf.st_size ;   /* No: save new size and exit. */
        show_display() ;
        return ;
      }

  if ((fp = fopen(spoolfile,"r")) == NULL)     /* Open spoolfile. */
    {
      if (mtype == MONNEW) show_display() ;   /* Show new mail. */
      else make_display() ;     /* Output icons and tidyup chain of records. */
      return ;
    }
  if (mtype == MONNEW) FSEEK(fp, lastsize, 0) ;
  lastsize = buf.st_size ;
  while (fgets(nextline,MAXLINE,fp) != NULL)
    if (EQUAL(nextline,"From "))
      {
        host[0] = user[0] = ts[0] = '\0' ;

        if ((ptr = get_field(HOSTNAME,nextline)) != NULL)
          STRCPY(host, ptr) ;
        if ((ptr = get_field(USERNAME,nextline)) != NULL)
          STRCPY(user, ptr) ;
        if ((ptr = get_field(TIMESTAMP,nextline)) != NULL)
          STRCPY(ts, ptr) ;

        h_to_c(host, community) ;    /* Turn hostname into community name. */
        a_to_u(community, user, realname) ;
        make_iconname(community, realname) ;
        if (mtype == MONNEW)
          {
            add_face(BOTH, ORDINARY, iconname) ;
            if (!dontshowtime) text(ICON, LEFT, ts) ;
            if (!dontshowuser) text(WINDOW, LEFT, realname) ;
            adjust() ;
          }
        else
          {
            if ((crec = rec_exists(community, realname)) != NULL)
              {
                STRCPY(crec->ts, ts) ;
                if (!crec->total) noicons++ ;
                crec->total++ ;
              }
            else add_record(community, realname, ts, 0) ;
          }
      }
  FCLOSE(fp) ;
  if (mtype == MONNEW) show_display() ;   /* Show new mail. */
  else make_display() ;     /* Output icons and tidyup chain of records. */
}


do_printer()                 /* Monitor printer queue. */
{
  struct recinfo *this, *next ;
  FILE *fp ;                 /* File descriptor for users mail spool file. */
  char command[MAXLINE] ;    /* Lpq system call for this printer. */
  char owner[MAXLINE] ;      /* Owner of job in the print queue. */
  char tempname[MAXLINE] ;   /* Temporary unique filename. */
  int size ;                 /* Size of this print job in bytes. */

  noicons = 0 ;
  SPRINTF(tempname,"/tmp/faces%04d",getpid()) ;
  SPRINTF(command,"lpq -P %s > %s",printer,tempname) ;
  if (system(command))
    {
      FPRINTF(stderr,"%s: system call for printer %s stats failed.\n",
              progname,printer) ;
      return ;
    }
  if ((fp = fopen(tempname,"r")) == NULL)     /* Open spoolfile. */
    {
      FPRINTF(stderr,"%s: couldn't open printer stats in %s.\n",
              progname,tempname) ;
      return ;
    }
  column = row = 0 ;          /* Start in top left corner of pixrect. */
  FGETS(nextline,MAXLINE,fp) ;
  if (EQUAL(nextline,"no entries"))
    {
      make_pixrect(1) ;               /* Just the "no print" icon. */
      add_face(BOTH, NOPRINT, "") ;   /* Set to "no print" icon. */
    }
  else if (EQUAL(nextline,"Printer Error: may need attention!"))
    {
      make_pixrect(1) ;               /* Just the "no paper" icon. */
      add_face(BOTH, NOPAPER, "") ;   /* Set to "no paper" icon. */
      text(BOTH, LEFT, printer) ;     /* Output printer name. */
    }
  else
    {
      FGETS(nextline,MAXLINE,fp) ;    /* Skip the next line. */
      while (fgets(nextline,MAXLINE,fp) != NULL)
        {
          SSCANF(&nextline[7], "%s", owner) ;
          SSCANF(&nextline[60], "%d", &size) ;
          h_to_c("", community) ;
          make_iconname(community, owner) ;
          add_record("",owner,"",size) ;
        }
      make_pixrect(noicons) ;
      this = recs ;
      while (this != NULL)
        {
          next = this->next ;
          add_face(WINDOW, ORDINARY, this->iconname) ;
          SPRINTF(nextline, "%1d", this->size) ;
          if (!dontshowuser)
            text(WINDOW, LEFT, this->username) ;  /* Owner. */
          text(WINDOW, RIGHT, nextline) ;         /* Size. */
          if (this == recs)
            {
              add_face(ICON, ORDINARY, this->iconname) ;
              SPRINTF(nextline, "%1d %s", noicons, (noicons == 1 ? "job" : "jobs")) ;
              text(ICON, RIGHT, nextline) ;   /* Number of jobs. */
            }
          adjust() ;    /* Adjust column and row. */
          remove_record(this) ;
          this = next ;
        }
      recs = last = NULL ;
    }
  FCLOSE(fp) ;
  UNLINK(tempname) ;
  show_display() ;
}


make_pixrect(count)   /* Make window pixrect the correct size. */
int count ;
{
  int c, r ;          /* Size in columns and rows of window display. */

  r = ((count-1) / NO_PER_ROW) + 1 ;   /* Number of rows of faces. */
  c = NO_PER_ROW ;                     /* Full width display. */
  if (count <= 10)
    {
      r = 1 ;         /* One row. */
      c = count ;     /* Of 'count' columns. */
    }

  height = r * ICONHEIGHT ;    /* Height of the icon display. */
  width = c * ICONWIDTH ;      /* Width of the icon display. */
  create_pixrects(width, height) ;
}


make_display()              /* Output icons and tidyup chain of records. */
{
  int count ;               /* Name of faces in icon display. */
  struct recinfo *this, *next ;

  count = noicons ;         /* Number of faces to display. */
  if (!count) count = 1 ;   /* Always one "no mail" icon. */
  make_pixrect(count) ;

  if (!noicons) add_face(BOTH, NOMAIL, "") ;
  else
    {
      this = recs ;
      while (this != NULL)
        {
          next = this->next ;
          add_face(BOTH, ORDINARY, this->iconname) ;
          if (!dontshowno)
            {
              SPRINTF(nextline, "%1d", this->total) ;
              text(ICON, RIGHT, nextline) ;
            }
          if (!dontshowtime) text(ICON, LEFT, this->ts) ;
          if (!dontshowuser) text(WINDOW, LEFT, this->username) ;
          adjust() ;
          remove_record(this) ;
          this = next ;
        }
      recs = last = NULL ;
    }
  show_display() ;         /* Display the latest set of faces. */
}
