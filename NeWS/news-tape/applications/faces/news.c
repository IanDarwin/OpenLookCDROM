/*LINTLIBRARY*/
#ifndef lint
static char sccsid[] = "@(#)news.c 1.3 88/12/11" ;
#endif

/*  NeWS dependent graphics routines used by faces,
 *  the visual mail and print job monitor.
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
#include <sundev/kbd.h>
#include <sundev/kbio.h>

#ifdef SUNOS3.x
int fullmask ;               /* Full mask of file descriptors to check on. */
int readmask ;               /* Readmask used in select call. */
#else
fd_set fullmask ;            /* Full mask of file descriptors to check on. */
fd_set readmask ;            /* Readmask used in select call. */
#endif SUNOS3.x

int kbdfd ;                  /* File descriptor for the keyboard. */
int psfd ;                   /* NeWS connection file descriptor. */
unsigned short ibuf[256] ;   /* Ikon/icon image buffer. */

extern FILE *PostScript ;
extern FILE *PostScriptInput ;


add_face(display, itype, name)     /* Add this icon to the display. */
enum disp_type display ;
enum icon_type itype ;
char *name ;
{
  char face[MAXLINE] ;        /* Canvas containing face (Source). */

  switch ((int) itype)
    {
      case NOMAIL   : STRCPY(face, "NomailIcon") ;
                      break ;
      case NOPAPER  : STRCPY(face, "NopaperIcon") ;
                      break ;
      case NOPRINT  : STRCPY(face, "NoprintIcon") ;
                      break ;
      case ORDINARY : if ((facetype = get_icon(name, ibuf)) != -1) 
                        switch (facetype)
                          {
                            case BLITTYPE :
                            case SUNTYPE  : load_icon("Curface") ;
                                            STRCPY(face, "CurfaceIcon") ;
                            case NEWSTYPE : break ;
                          }
                      else STRCPY(face, "NofaceIcon") ;
                      break ;
    }

  if (display == ICON)
    if (mtype == MONPRINTER) adjust_image("MPR", face, 0, 0) ;
    else adjust_image("MPR", face, row, column) ;
  else if (display == WINDOW) adjust_image("PR", face, row, column) ;
  else
    {
      adjust_image("MPR", face, row, column) ;
      adjust_image("PR", face, row, column) ;
    }
  FFLUSH(PostScript) ;
}


adjust_image(dest, face, row, column)
char *dest, *face ;
int row, column ;
{
  switch ((int) mtype)
    {
      case MONNEW     : FPRINTF(PostScript, "%d 0 %d %d %s ShiftImage\n",
                                ICONWIDTH, (NO_PER_ROW-1)*ICONWIDTH, ICONHEIGHT,
                                dest) ;
                        if (facetype == NEWSTYPE)
                          {
                            FPRINTF(PostScript, "%d %d %d %d %s ClearBox\n",
                                    column*ICONWIDTH, row*ICONHEIGHT,
                                    ICONWIDTH, ICONHEIGHT, dest) ;
                            return ;
                          }
      case MONALL     :
      case MONPRINTER : FPRINTF(PostScript,"%s %d %d %d %d %s MoveImage\n",
                                face, ICONWIDTH, ICONHEIGHT,
                                column*ICONWIDTH, row*ICONHEIGHT, dest) ;
    }
}


beep_flash(beeps, flashes)    /* Perform visual feedback. */
int beeps, flashes ;
{
  int i, x ;

  if (beeps)
    for (i = 0; i < beeps; i++)
      {
        x = KBD_CMD_BELL ;
        IOCTL(kbdfd, KIOCCMD, &x) ;
        usleep(250000) ;
        x = KBD_CMD_NOBELL ;
        IOCTL(kbdfd, KIOCCMD, &x) ;
        usleep(250000) ;
      }
}


create_pixrects(width, height)        /* Create pixrects for the face images. */
{
  int h, i, j, w ;

  if (mtype == MONNEW && !firsttime) return ;
  FPRINTF(PostScript,"%d %d PR DoPixrects\n", width, height) ;
  h = height ;
  w = width ;
  if (mtype == MONPRINTER)
    {
      h = ICONHEIGHT ;
      w = ICONWIDTH ;
    }
  FPRINTF(PostScript,"%d %d MPR DoPixrects\n", w, h) ;
  if (strlen(bgicon))
    {
      for (i = 0; i < (height / 64); i++)
        for (j = 0; j < (width / 64); j++)
          {
            FPRINTF(PostScript,"BackgroundIcon %d %d %d %d PR MoveImage\n",
                                  ICONWIDTH, ICONHEIGHT, j, i) ;
            FPRINTF(PostScript,"BackgroundIcon %d %d %d %d MPR MoveImage\n",
                                  ICONWIDTH, ICONHEIGHT, j, i) ;
          }
    }
  else FPRINTF(PostScript,"SetBackground\n") ;
}


do_news_ps(psrecs)     /* Display chain of NeWS animations. */
struct psinfo *psrecs ;
{
  struct psinfo *next, *this ;

  this = psrecs ;      /* Point to beginning of NeWS records. */
  while (this != NULL)
    {
      next = this->next ;
      if ((mtype != MONNEW) || (this->column < NO_PER_ROW))
        {
          FPRINTF(PostScript, "(%s) %d %d %d %d ExecFile\n",
                  this->name, ICONWIDTH, ICONHEIGHT,
                  this->column*ICONWIDTH, this->row*ICONHEIGHT) ;
          FFLUSH(PostScript) ;
        }
      this = next ;
    }
}


init_ws_type()
{
  if (ps_open_PostScript() < 0) return -1 ;
  if (send_ps_file(NEWSFILE) == -1)
    {
      FPRINTF(stderr,"%s: cannot open %s\n", progname, NEWSFILE) ;
      FCLOSE(PostScript) ;
      return(-1) ;
    }
  FFLUSH(PostScript) ;
  if (ferror(PostScript))
    {
      FCLOSE(PostScript) ;
      return(-1) ;
    }
  if (invert) FPRINTF(PostScript, "/Invert false def\n") ;
  else FPRINTF(PostScript, "/Invert true def\n") ;
  gtype = NEWS ;
  return(0) ;
}


load_icon(name)
char *name ;
{
  int i, j ;

  FPRINTF(PostScript,"/%sIcon 64 64 1 { } { <\n", name) ;
  for (i = 0; i < 32; i++)
    {
      for (j = 0; j < 8; j++) FPRINTF(PostScript,"%.4X ", ibuf[i*8+j]) ;
      FPRINTF(PostScript,"\n") ;
    }
  FPRINTF(PostScript,"> } buildimage def\n") ;
}


/*ARGSUSED*/
make_frame(argc,argv)
int argc ;
char *argv[] ;
{
  if ((kbdfd = open("/dev/kbd", 0)) == -1)
    {
      FPRINTF(stderr,"%s: can't open keyboard.\n", progname) ;
      exit(1) ;
    }

  psfd = fileno(PostScriptInput) ;

#ifdef SUNOS3.x
  fullmask = 1 << psfd ;
#else
  FD_ZERO(&fullmask) ;
  FD_SET(psfd, &fullmask) ;
#endif SUNOS3.x

  if (strlen(bgicon))
    if (get_sun_icon(bgicon, ibuf) == 0) load_icon("Background") ;

  FPRINTF(PostScript,"%d %d %d %d %d %d %d MakeFrame\n",
          wx, wy, NO_PER_ROW*ICONWIDTH+10, ICONHEIGHT*10+10,
          ix, iy, iconic) ;
  FPRINTF(PostScript,"InitFont\n") ;
  width = NO_PER_ROW * ICONWIDTH ;
  height = ICONHEIGHT ;
}


make_icon()
{
  if (get_sun_icon("noface.icon", ibuf) == 0) load_icon("Noface") ;
  if (get_sun_icon("nomail.icon", ibuf) == 0) load_icon("Nomail") ;
  if (get_sun_icon("nopaper.icon", ibuf) == 0) load_icon("Nopaper") ;
  if (get_sun_icon("noprint.icon", ibuf) == 0) load_icon("Noprint") ;
}


send_ps_file(fname)
char *fname ;
{
  FILE *stream ;
  int c ;

  if ((stream = fopen(fname,"r")) == NULL) return -1 ;
  while ((c = getc(stream)) != EOF) PUTC(c,PostScript) ;
  FCLOSE(stream) ;
  return 0 ;
}


show_display()        /* Show the latest set of mail icon faces. */
{
  if (mtype != MONPRINTER)
    FPRINTF(PostScript,"%d %d %d %d ShowDisplay\n",
                        width, height, width, height) ;
  else FPRINTF(PostScript,"%d %d %d %d ShowDisplay\n",
                        width, height, ICONWIDTH, ICONHEIGHT) ;

  if (newmail) beep_flash(beeps, flashes) ;
  if (psrecs != NULL) do_news_ps(psrecs) ;
  FFLUSH(PostScript) ;
}


start_tool()
{
  int type ;                     /* Value from NeWS server. */
  struct psinfo *next, *this ;   /* For removing current chain of records. */
  struct timeval tval ;

  tval.tv_usec = 0 ;
  tval.tv_sec = period ;

  for (;;)
    {
      readmask = fullmask ;
#ifdef SUNOS3.x
      SELECT(32, &readmask, 0, 0, &tval) ;
      if (readmask && (1 << psfd))
#else
      SELECT(FD_SETSIZE, &readmask, (fd_set *) 0, (fd_set *) 0, &tval) ;
      if (FD_ISSET(psfd, &readmask))
#endif SUNOS3.x
        {
          if (pscanf(PostScriptInput, "%d", &type) == EOF) exit(1) ;
          switch (type)
            {
              case DIED     : exit(0) ;
              case PAINTED  : if (psrecs != NULL) do_news_ps(psrecs) ;
            }
        }
      else
        {
          this = psrecs ;     /* Point to beginning of NeWS records. */
          if (mtype != MONNEW)
            {
              while (this != NULL)
                {
                  next = this->next ;
                  if (this->name != NULL) free(this->name) ;
                  free((char *) this) ;   /* Remove this record. */
                  this = next ;
                }
              psrecs = plast = NULL ;
            }
          do_check() ;        /* Check the mail/printer again. */
        }
    }
}


text(display,just,str)
enum disp_type display ;
enum just_type just ;
char *str ;
{
  int i, len ;
  int c, r ;             /* Column and row position for this face. */
  int x, y ;             /* Position of start of this text string. */
  char line[MAXLINE] ;   /* PostScript text string to be builtup. */
  char tpr ;             /* Indicator for destination offscreen canvas. */

  c = column ;
  r = row ;
  switch ((int) display)
    {
      case BOTH   : text(ICON, just, str) ;
                    text(WINDOW, just, str) ;
                    return ;
      case ICON   : tpr = 'M' ;       /* MPR canvas. */
                    if (mtype != MONALL) c = r = 0 ;
                    break ;
      case WINDOW : tpr = ' ' ;       /*  PR canvas. */
    }

  len = strlen(str) ;         /* Character length of text. */
  if (len > 10)
    {
      len = 10 ;
      str[10] = '\0' ;   /* Maximum of 10 characters. */
    }

  line[0] = '\0' ;
  for (i = 0; i < len; i++)
    switch (str[i])
      {
        case '\\' : STRCAT(line,"\\\\") ;
                    break ;
        case '('  : STRCAT(line,"\\(") ;
                    break ;
        case ')'  : STRCAT(line,"\\)") ;
                    break ;
        default   : STRNCAT(line,&str[i],1) ;
      }

  switch ((int) just)
    {
      case LEFT  : x = c*ICONWIDTH+2 ;
                   y = r*ICONHEIGHT+2 ;
                   break ;
      case RIGHT : x = (c+1)*ICONWIDTH-(len*6)-2 ;
                   y = r*ICONHEIGHT+2 ;
    }
  FPRINTF(PostScript,"%d %d %d %d %cPR ClearBox\n", x, y, len*6+2, 10, tpr) ;
  FPRINTF(PostScript,"(%s) %d %d %cPR MakeText\n", line, x, y, tpr) ;
}
