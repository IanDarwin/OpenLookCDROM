/*LINTLIBRARY*/
#ifndef lint
static char sccsid[] = "@(#)sunview.c 1.1 88/12/04" ;
#endif

/*  SunView dependent graphics routines used by faces,
 *  the visual mail and print job monitor.
 * 
 *  Copyright (c) Rich Burridge - Sun Microsystems Australia.
 *                                All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged. 
 * 
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if reported
 *  to me, then an attempt will be made to fix them.
 */

#include "faces.h"
#include "extern.h"
#include <stdio.h>
#include <suntool/sunview.h>
#include <suntool/canvas.h>

#define  ICON_SET                (void) icon_set
#define  NOTIFY_SET_ITIMER_FUNC  (void) notify_set_itimer_func
#define  PR_DESTROY              (void) pr_destroy
#define  PR_REPLROP              (void) pr_replrop
#define  PR_ROP                  (void) pr_rop

#ifdef SUNOS3.x
#define  PR_TTEXT                (void) pf_ttext
#else
#define  PR_TTEXT                (void) pr_ttext
#endif SUNOS3.x

#define  PW_ROP                  (void) pw_rop
#define  WIN_BELL                (void) win_bell
#define  WINDOW_SET              (void) window_set

#define  SMALLFONT    "/usr/lib/fonts/fixedwidthfonts/screen.r.7"
#define  ITIMER_NULL  ((struct itimerval *) 0)

int repaint_proc() ;

Canvas canvas ;
Frame frame ;
Icon faces_icon ;
Pixfont *sfont ;              /* Small font for timestamp and count. */
Pixrect *mpr, *pr ;           /* Pointers to current pixrects. */
Pixrect *old_mpr, *old_pr ;   /* Pointers to previous pixrects. */
Pixrect *background ;         /* Pointer to background pattern or gray. */
Pixwin *fpw, *pw ;
Rect *temprect ;
int ffd ;                     /* File descriptor of faces frame. */

short gray_image[] = { 0x8000, 0x8000, 0x2000, 0x2000 } ;
mpr_static(gray_pr, 4, 4, 1, gray_image) ;

short noface_image[] = {
#include "noface.icon"
} ;
mpr_static(noface_pr, 64, 64, 1, noface_image) ;

short nomail_image[] = {
#include "nomail.icon"
} ;
mpr_static(nomail_pr, 64, 64, 1, nomail_image) ;

short nopaper_image[] = {
#include "nopaper.icon"
} ;
mpr_static(nopaper_pr, 64, 64, 1, nopaper_image) ;

short noprint_image[] = {
#include "noprint.icon"
} ;
mpr_static(noprint_pr, 64, 64, 1, noprint_image) ;


add_face(display, itype, name)
enum disp_type display ;
enum icon_type itype ;
char *name ;
{
  Pixrect *face_pr ;
  unsigned short buf[256] ;  /* Ikon/icon image. */
  unsigned short *ptr ;
  int freeit ;               /* Set if we should destroy this pixrect. */
  int i ;

  freeit = 0 ;
  switch ((int) itype)
    {
      case NOMAIL   : face_pr = &nomail_pr ;
                      break ;
      case NOPAPER  : face_pr = &nopaper_pr ;
                      break ;
      case NOPRINT  : face_pr = &noprint_pr ;
                      break ;
      case ORDINARY : if (get_icon(name, buf) == -1) face_pr = &noface_pr ;
                      else
                        {
                          face_pr = mem_create(64, 64, 1) ;
                          ptr = (unsigned short *)
                                ((struct mpr_data *) face_pr->pr_data)->md_image ;
                          for (i = 0; i < 256; i++) ptr[i] = buf[i] ;
                          freeit = 1 ;
                        }
                      break ;
    }
  if (display == ICON)
    if (mtype == MONPRINTER) adjust_image(mpr, face_pr, 0, 0) ;
    else adjust_image(mpr, face_pr, row, column) ;
  else if (display == WINDOW) adjust_image(pr, face_pr, row, column) ;
  else
    {
      adjust_image(mpr, face_pr, row, column) ;
      adjust_image(pr, face_pr, row, column) ;
    }
  if (freeit) PR_DESTROY(face_pr) ;
}


adjust_image(fpr, face_pr, row, column)    /* Place new face in memory pixrect. */
Pixrect *fpr, *face_pr ;
int row, column ;
{
  switch ((int) mtype)
    {
      case MONNEW     : PR_ROP(fpr, 64, 0, 640-64, 64, PIX_SRC, fpr, 0, 0) ;
      case MONALL     :
      case MONPRINTER : PR_ROP(fpr, column*ICONWIDTH, row*ICONHEIGHT,
                               ICONWIDTH, ICONHEIGHT, PIX_SRC, face_pr, 0, 0) ;
    }
}


Notify_value
check_mail()
{
  do_check() ;
}


beep_flash(beeps, flashes)    /* Perform visual feedback. */
int beeps, flashes ;
{
  static struct timeval btime = { 0, 250000 } ;   /* Beep timer. */
  int bfd ;        /* File descriptor for faces window, or -1. */
  Pixwin *bpw ;    /* Pixwin pointer for faces window, or 0. */

  if (beeps || flashes)
    do
      {
        bfd = beeps > 0 ? ffd : -1 ;
        bpw = flashes > 0 ? fpw : (Pixwin *) 0 ;
        WIN_BELL(bfd, btime, bpw) ;
        SELECT(0, (fd_set *) 0, (fd_set *) 0, (fd_set *) 0, &btime) ;
        beeps-- ;
        flashes-- ;
      } 
    while (beeps > 0) ;
}


create_pixrects(width, height)   /* Create pixrects for the face images. */
int width, height ;
{
  old_pr = pr ;
  pr = mem_create(width, height, 1) ;
  PR_REPLROP(pr, 0, 0, width, height, PIX_SRC, background, 0, 0) ;
  faces_icon = (Icon) window_get(frame, FRAME_ICON) ;
  old_mpr = (Pixrect *) icon_get(faces_icon, ICON_IMAGE) ;
  if (mtype == MONPRINTER) mpr = mem_create(ICONWIDTH, ICONHEIGHT, 1) ;
  else
    {
      mpr = mem_create(width, height, 1) ;
      PR_REPLROP(mpr, 0, 0, width, height, PIX_SRC, background, 0, 0) ;
    }
  if (mtype == MONNEW && old_pr != NULL)
    PR_ROP(pr, 0, 0, width, height, PIX_SRC, old_pr, 0, 0) ;
  if (mtype == MONNEW && old_mpr != NULL)
    PR_ROP(mpr, 0, 0, width, height, PIX_SRC, old_mpr, 0, 0) ;
}


init_ws_type()
{
  gtype = SUNVIEW ;
  return 0 ;
}


make_frame(argc,argv)
int argc ;
char *argv[] ;
{
  unsigned short buf[256] ;  /* For background pattern. */
  unsigned short *ptr ;
  int i ;

  if (strlen(bgicon))
    {
      if (get_sun_icon(bgicon, buf) == 0) background = &gray_pr ;
      background = mem_create(64, 64, 1) ;
      ptr = (unsigned short *)
            ((struct mpr_data *) background->pr_data)->md_image ;
      for (i = 0; i < 256; i++) ptr[i] = buf[i] ;
    }
  else background = &gray_pr ;

  frame = window_create((Window) 0, FRAME,
                        FRAME_ICON, faces_icon,
                        FRAME_SHOW_LABEL, FALSE,
                        FRAME_SUBWINDOWS_ADJUSTABLE, FALSE,
                        FRAME_NO_CONFIRM, TRUE,
                        WIN_WIDTH, NO_PER_ROW * ICONWIDTH + 10,
                        WIN_HEIGHT, ICONHEIGHT * 10 + 10,
                        FRAME_ARGS, argc,argv,
                        0) ;
  fpw = (Pixwin *) window_get(frame, WIN_PIXWIN) ;
  ffd = (int) window_get(frame, WIN_FD) ;

  canvas = window_create(frame, CANVAS,
                         CANVAS_REPAINT_PROC, repaint_proc,
                         CANVAS_RETAINED, TRUE,
                         0) ;
  pw = canvas_pixwin(canvas) ;
  sfont = pf_open(SMALLFONT) ;
  width = NO_PER_ROW * ICONWIDTH ;
  height = ICONHEIGHT ;
}


make_icon()
{
  if (mtype == MONPRINTER)
    faces_icon = icon_create(ICON_IMAGE, &noprint_pr, 0) ;
  else faces_icon = icon_create(ICON_IMAGE, &nomail_pr, 0) ;
}


/*ARGSUSED*/
repaint_proc(canvas, pw, repaint_area)
Canvas canvas ;
Pixwin *pw ;
Rectlist *repaint_area ;
{
  PW_ROP(pw, 0, 0, width, height, PIX_SRC, pr, 0, 0) ;
}


show_display()    /* Show the latest set of mail icon faces. */
{
  if (invert)     /* Invert the memory pixrects before displaying. */
    {
      PR_ROP(pr, 0, 0, width, height, PIX_NOT(PIX_DST), pr, 0, 0) ;
      PR_ROP(mpr, 0, 0, width, height, PIX_NOT(PIX_DST), pr, 0, 0) ;
    }

  if (mtype != MONPRINTER)
    {
      temprect = (Rect *) icon_get(faces_icon, ICON_IMAGE_RECT) ;
      temprect->r_height = height ;
      temprect->r_width = width ;
      ICON_SET(faces_icon, ICON_HEIGHT, height,
                           ICON_WIDTH, width,
                           ICON_IMAGE_RECT, temprect,
                           ICON_IMAGE, mpr,
                           0) ;
    }
  else ICON_SET(faces_icon, ICON_IMAGE, mpr, 0) ;
  WINDOW_SET(frame, FRAME_ICON, faces_icon, 0) ;

  temprect = (Rect *) window_get(frame, FRAME_OPEN_RECT) ;
  temprect->r_height = height+10 ;
  temprect->r_width = width+10 ;
  WINDOW_SET(frame, FRAME_OPEN_RECT, temprect, 0) ;
  PW_ROP(pw, 0, 0, width, height, PIX_SRC, pr, 0, 0) ;

  if (newmail) beep_flash(beeps, flashes) ;
  if (old_pr) PR_DESTROY(old_pr) ;
  if (old_mpr) PR_DESTROY(old_mpr) ;
}


start_tool()
{
  struct itimerval tval ;

  tval.it_interval.tv_usec = 0 ;
  tval.it_interval.tv_sec = period ;
  tval.it_value.tv_usec = 0 ;
  tval.it_value.tv_sec = period ;
  NOTIFY_SET_ITIMER_FUNC(frame, check_mail,
                         ITIMER_REAL, &tval, ITIMER_NULL) ;
  window_main_loop(frame) ;
}


text(display, just, str)
enum disp_type display ;
enum just_type just ;
char *str ;
{
  int len ;
  int c, r ;         /* Column and row position for this face. */
  int x, y ;         /* Position of start of this text string. */
  Pixrect *tpr ;

  c = column ;
  r = row ;
  switch ((int) display)
    {
      case BOTH   : text(ICON, just, str) ;
                    text(WINDOW, just, str) ;
                    return ;
      case ICON   : tpr = mpr ;
                    if (mtype != MONALL) c = r = 0 ;
                    break ;
      case WINDOW : tpr = pr ;
    }

  len = strlen(str) ;              /* Character length of text. */
  if (len > 10)
    {
      len = 10 ;
      str[10] = '\0' ;   /* Maximum of 10 characters. */
    }
  switch ((int) just)
    {
      case LEFT  : x = c*ICONWIDTH+2 ;
                   y = (r+1)*ICONHEIGHT-2 ;
                   break ;
      case RIGHT : x = (c+1)*ICONWIDTH-(len*6)-2 ;
                   y = (r+1)*ICONHEIGHT-2 ;
    }
  PR_ROP(tpr, x, y-9, len*6+2, 10, PIX_CLR, (Pixrect *) NULL, 0, 0) ;
  PR_TTEXT(tpr, x, y, PIX_SRC ^ PIX_NOT(PIX_DST), sfont, str) ;
}
