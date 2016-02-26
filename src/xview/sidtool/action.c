
/*  @(#)action.c 1.7 91/09/05
 *
 *  Various routines that manipulate objects in sidtool.
 *
 *  Screen design and original implementation
 *               Copyright (C) 1981, 1982, 1983 - Brad A. Myers
 *
 *  Current implementation
 *               Copyright (C) 1991 Rich Burridge
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors on inaccuracies inherent
 *  either to the comments or the code of this program, but if reported
 *  to me, then an attempt will be made to fix them.
 */

#include <stdio.h>
#include "sidtool.h"
#include "extern.h"

/*  Wait a while and check keyboard for commands while blinking
 *  current players score.
 */

void
blinkpause()
{
  int i ;

  on = 1 ;
  if (autoplay) longpause(5) ;
  else
    for (i = 1; i <= 16; i++)
      {
        showplayerscore(player) ;
        on = !on ;
        longpause(5) ;
      }
}


/*  To get over displaying the last cursor incorrectly after a cleared screen,
 *  the old cursor position is forced off the screen.
 */

void
clear_screen()
{
  blt_scrn(orgx, orgy, width, height, RCLR) ;
  oldcx = 1000 ;
  oldcy = 1000 ;
}


void
docredits()
{
  int g, i, x, y ;

  credits = 1 ;
  clear_screen() ;
  dohelp() ;
  longpause(6) ;

  newbugs(0) ;
  untranspt(130, 290, &dotx, &doty) ;
  untranspt(860, 290, &x, &y) ;
  transpt(x, y, &i, &ciry) ;
  blt_scrn(0, ciry - 37, 762, 100, RINV) ;

  for (g = (int) POKEY; g <= (int) SHADOW; g++)
    {
      bugs[g].mx = x + g * 2 ;
      bugs[g].my = doty ;
      bugs[g].dir = (enum dir_type) g ;
      transpt(bugs[g].mx, bugs[g].my, &(bugs[g].scrx), &(bugs[g].scry)) ;
      drawbug(&bugs[g]) ;                        /* Should be invisible. */
    }
  drawdot(dotx, doty, BIGDOT) ;
  cirx = 720 ;
  inc = 0 ;
  movei = 1 ;
  progstate = MOVELEFT ;
}


void
dohelp()
{
  int g, i, x, y ;
  char line[MAXLINE] ;

  draw_text(105, 30, BOLDFONT, titlestring) ;
  draw_text(105, 50, NORMALFONT,
    "Screen design and original implementation - Copyright (C) 1981 - Brad A. Myers.") ;
  draw_text(105, 70, NORMALFONT,
    "Current implementation - Copyright (C) 1991 Rich Burridge.") ;

  for (g = (int) POKEY; g <= (int) SHADOW; g++)
    {
      if (g > 1) y = YBASE + 145 ;
      else       y = YBASE + 75 ;
      x = (g % 2) ? 384 : 100 ;

      blt_mem_to_scrn(x, y, 45, 45, RRPL,
                      (enum icon_type) ((int) BUGPICS + (g*2)), 0, 0) ;
      blt_mem_to_scrn(x, y, 45, 21, RXOR,
                      (enum icon_type) ((int) EYES + g), 0, 0) ;
      longpause(1) ;
      SPRINTF(line, "- %s", names[g]) ;
      draw_text(x + 60, y + 25, NORMALFONT, line) ;
      longpause(1) ;
    }

  i = 0 ;
  while (helpstrs[i] != NULL)
    {
      if (helpstrs[i] != '\0')
        draw_text(105, i*15 + 365, NORMALFONT, helpstrs[i]) ;
      i++ ;
    }
}


int
doinc(dir, posx, posy, mx, my, x, y, nx, ny)
enum dir_type dir ;
int posx, posy, mx, my, *x, *y, *nx, *ny ;
{
  register int status, tx, ty ;

  *x = posx ;
  *y = posy ;
  tx = mx ;
  ty = my ;

  switch (dir)
    {
      case UP    : *y = posy - 2 ;
                   ty = my - 1 ;
                   break ;
      case DOWN  : *y = posy + 2 ;
                   ty = my + 1 ;
                   break ;
      case LEFT  : *x = posx - 2 ;
                   tx = mx - 1 ;
                   break ;
      case RIGHT : *x = posx + 2 ;
                   tx = mx + 1 ;
                   break ;
    }
  untranspt(*x, *y, nx, ny) ;
  if (tx == -2) tx = XSIZE + 2 ;
  else if (tx == XSIZE + 3) tx = -1 ;
  status = 1 ;

  if ((*nx == -2) && (dir == LEFT))
    {
      *nx = XSIZE + 2 ;
      transpt(*nx, *ny, x, y) ;
    }
  else if ((*nx == XSIZE + 3) && (dir == RIGHT))
    {
      *nx = -1 ;
      transpt(*nx, *ny, x, y) ;
    }
  else if (!(walls[*nx+2][*ny] ||
            (gcentered(posx, posy) && walls[tx+2][ty]))) /* do nothing */ ;
  else status = 0 ;
  return(status) ;
}


void
doplay()
{
  if (sremove)       /* Jump here if have been eaten or starting new game. */
    {
      removecircle() ;
      numcir[player]-- ;
    }
  curdir    = LEFT ;             /* Jump here if got all dots. */
  fruiton   = 0 ;
  sc        = ' ' ;
  inc       = 0 ;
  count     = 1 ;
  posx      = (SWIDTH / 2) - 11 ;
  posy      = YBASE + SQUARE * 21 ;
  fruittime = randomrange(1000, 2500) ;
  untranspt(posx, posy, &cirmx, &cirmy) ;
  drawcir((enum icon_type) ((int) CIRCLES + ((int) curdir*4) + inc),
           posx, posy) ;
  newbugs(1) ;
  if (demomode || !autoplay)
    {
      SCHRFUNC(RXOR) ;
      draw_text(357, YBASE + SQUARE*16, NORMALFONT, "READY!") ;
      blinkpause() ;
      SCHRFUNC(RXOR) ;
      draw_text(357, YBASE + SQUARE*16, NORMALFONT, "READY!") ;
      SCHRFUNC(RRPL) ;
    }
}


enum dir_type
dorandomdir(dir, scrx, scry, mx, my, x, y, nx, ny, ranrange)
enum dir_type dir ;
int scrx, scry, mx, my, *x, *y, *nx, *ny, ranrange ;
{
  enum dir_type newdir, rev, status ;
  int i, test ;

  test = randomrange(1, ranrange) ;
  rev = reversedir(dir) ;
  if ((test == 1) || (!checkinc(dir, mx, my)))
    {
      newdir = (enum dir_type) randomrange(0, 3) ;
      for (i = 0; i <= 3; i++)
        {
          if (newdir != rev)
            if (checkinc(newdir, mx, my))
              {
                status = newdir ;
                DOINC(newdir, scrx, scry, mx, my, x, y, nx, ny) ;
                return(status) ;
              }
          newdir = (enum dir_type) (((int) newdir + 1) % 4) ;
        }
    }    
  else
    { 
      DOINC(dir, scrx, scry, mx, my, x, y, nx, ny) ;
      status = dir ;
    }
  return(status) ;
}


void
doupdate()
{
  count++ ;
  if (count % circatchup == 0) return ;       /* Go slower than bugs. */
  drawcir((enum icon_type) ((int) CIRCLES + ((int) newdir*4) + inc), x, y) ;
  if (count % 4 == 0) inc = (inc + 1) % 4 ;
  if (fruiton)
    if ((nx == FRUITMX) && (ny == FRUITMY)) destroyfruit() ;
  if (dots[player][nx+1][ny] != NODOT)
    {
      if (dots[player][nx+1][ny] == SMALLDOT) updatescore(1) ;
      else
        { 
          changebugs() ;
          updatescore(5) ;
        }
      numdots[player]-- ;
      drawdot(nx, ny, dots[player][nx+1][ny]) ;
      dots[player][nx+1][ny] = NODOT ;
      if (!numdots[player])
        {
          resetmaze() ;
          gamestate = FALSE ;
          progstate = RESETGAME ;
          longjmp(exception, val) ;
        }
    }    
  curdir = newdir ;
  posx = x ;
  posy = y ;
  cirmx = nx ;
  cirmy = ny ;
}


void
draw_joystick(dir)
enum dir_type dir ;
{
  if (jdir != dir)
    {
      blt_scrn(XBASE + 330, YBASE + 10, 44, 44, RCLR) ;
      blt_mem_to_scrn(XBASE + 330, YBASE + 10, 44, 44, RRPL,
                  (enum icon_type) ((int) RJSTICK + (int) dir), 0, 0) ;
      jdir = dir ;
    }
}


void
explodecircle(posx, posy)
int posx, posy ;
{
  int i ;
 
  for (i = 0; i <= 8; i++)
    {
      blt_mem_to_scrn(oldcx - GOFFSET + 5, oldcy - GOFFSET + 5,
                      45, 45, RXOR, CURCIRCLE, 0, 0) ;
      blt_mem(CURCIRCLE, 0, 0, 45, 45, RRPL,
              (enum icon_type) ((int) EXPLODE + i), 0, 0) ;
      blt_mem_to_scrn(posx - GOFFSET + 5, posy - GOFFSET + 5,
                      45, 45, RXOR, CURCIRCLE, 0, 0) ;
      oldcx = posx ;
      oldcy = posy ;
      ppause(pausetime * 70) ;
    } 
  longpause(3) ;
}


/*  Look for tunnels on the borders.  For each, show the area as black
 *  on the screen and set the walls and tunnel global variables to
 *  reflect the presence of the tunnel.
 */

void
fixexits()
{
  int x, y, t ;

  ppause(pausetime * 10) ;
  for (y = 1; y <= YSIZE; y++)
    if (maze[0][y] == ' ')
      {
        walls[1][y] = 0 ;
        walls[0][y] = 0 ;
        x = -1 ;
        do
          {
            x++ ;
            tunnel[x][y] = 1 ;
          }
        while (maze[x][y] == ' ') ;
        transpt(0, y, &x, &t) ;
        blt_scrn(3, t-2-SQUARE/2, XBASE-(SQUARE/2)-5, SQUARE*2+6, RINV) ;
      }

  ppause(pausetime * 10) ;
  for (y = 1; y <= YSIZE; y++)
    if (maze[XSIZE+1][y] == ' ')
      {
        walls[XSIZE+4][y] = 0 ;
        walls[XSIZE+5][y] = 0 ;
        x = XSIZE+1 ;
        do
          {
            x-- ;
            tunnel[x][y] = 1 ;
          }
        while (maze[x][y] == ' ') ;
        transpt(0, y, &x, &t) ;
        blt_scrn(XBASE-(SQUARE/2)+SQUARE*(XSIZE+1)+4,
                 t-2-SQUARE/2, XBASE-(SQUARE/2)-5, SQUARE*2+6, RINV) ;
      }
}


/* Only called when gcentered. */

void
headto(destx, desty, scrx, scry, mx, my, dir, x, y, nx, ny)
enum dir_type *dir ;
int destx, desty, scrx, scry, mx, my, *x, *y, *nx, *ny ;
{
  enum dir_type dirar[5], rev ;
  int i, s, xinc, yinc ;

  rev = reversedir(*dir) ;
  xinc = mx - destx ;
  yinc = my - desty ;
  if (abs(xinc) > abs(yinc)) s = 2 ;
  else s = 1 ;
  if (xinc < 0)
    {
      dirar[3-s] = RIGHT ;
      dirar[s+2] = LEFT ;
    }
  else
    {
      dirar[3-s] = LEFT ;
      dirar[s+2] = RIGHT ;
    }
  if (yinc < 0)
    {
      dirar[s] = DOWN ;
      dirar[5-s] = UP ;
    }
  else
    {
      dirar[s] = UP ;
      dirar[5-s] = DOWN ;
    }

  for (i = 1; i <= 4; i++)     /* Adjust so reverse is last choice. */
    if (dirar[i] == rev)
      {
        for (s = i; s <= 3; s++) dirar[s] = dirar[s+1] ;
        dirar[4] = rev ;
        break ;
      }

  for (s = 1; s <= 4; s++)
    {
      if (checkinc(dirar[s], mx, my))
        {
          *dir = dirar[s] ;
          DOINC(*dir, scrx, scry, mx, my, x, y, nx, ny) ;
          return ;
        }
    }
}


void
initgame()
{
  int i, j ;

  if (autoplay) autoscore = 0 ;
  pausetime = -skilllevel * 20 + (speed * 100) ;
  circatchup = -skilllevel * 4 + 46 ;
  highplayer = -1 ;
  for (j = 1; j < MAXNUMPLAYERS; j++)
    {
      numdots[j] = 0 ;
      numcir[j] = 3 ;
      fruitchances[j] = 0 ;
      setdots(j) ;
      curbluetime[j] = 1 + (-skilllevel * 60 + 900) ;
      if (!autoplay)
        {
          score[j] = 0 ;
          if (demomode) fruitmaze[j] = 8 ;
          else fruitmaze[j] = 1 ;
          for (i = 1; i < 8; i++)
            if (demomode) fruitsgotten[j][i] = 1 ;
            else fruitsgotten[j][i] = 0 ;
        }
    }    
}


void
move_left()      /* Animate screen and bugs left. */
{
  int g ;

  ppause(pausetime) ;
  if (movei % 8) cirx-- ;
  drawcir((enum icon_type) ((int) CIRCLES + (int) LEFT*4 + inc), cirx, ciry) ;
  if (movei % 4 == 0) inc = (inc + 1) % 4 ;
  for (g = (int) POKEY; g <= (int) SHADOW; g++)
    {
      drawbug(&bugs[g]) ;               /* Erase old. */
      bugs[g].scrx-- ;
      if (movei % 13 == 0) bugs[g].pic = (bugs[g].pic + 1) % 2 ;
      if (movei % 18 == 0)
        bugs[g].dir = (enum dir_type) (((int) bugs[g].dir + 1) % 4) ;
      drawbug(&bugs[g]) ;               /* Draw new. */
    }
  if (++movei > 662)
    {
      credits = 2 ;
      drawdot(dotx, doty, BIGDOT) ;
      for (g = (int) POKEY; g <= (int) SHADOW; g++)
        {
          drawbug(&bugs[g]) ;           /* Erase old. */
          bugs[g].bluetime = 32000 ;
          drawbug(&bugs[g]) ;           /* Draw new as blue. */
        }
      SCHRFUNC(RXOR) ;
      movej = 200 ;
      movex = 1 ;
      movei = 1 ;
      progstate = MOVERIGHT ;
    }
}


void
move_right()    /* Animate eating screen and bugs right. */
{
  int g ;

   ppause(pausetime * movex / 4) ;
   if (movei % 26) cirx++ ;
   drawcir((enum icon_type) ((int) CIRCLES + (int) RIGHT*4 + inc), cirx, ciry) ;
   if (movei % 4 == 0) inc = (inc + 1) % 4 ;
   for (g = (int) POKEY; g <= (int) SHADOW; g++)
     if (!bugs[g].eyesonly)
       {
         drawbug(&bugs[g]) ;                   /* Erase old. */
         if (movei % 2) bugs[g].scrx++ ;
         if (movei % 13 == 0) bugs[g].pic = (bugs[g].pic + 1) % 2 ;
         if (cirx >= bugs[g].scrx-20)
           {
             bugs[g].eyesonly = 1 ;
             SPRINTF(buffer, "%1d", movej) ;
             draw_text(bugs[g].scrx - 20, 330, NORMALFONT, buffer) ;
             longpause(1) ;
             SPRINTF(buffer, "%1d", movej) ;
             draw_text(bugs[g].scrx - 20, 330, NORMALFONT, buffer) ;
             movej *= 2 ;
             movex++ ;
           }
         else drawbug(&bugs[g]) ;       /* Draw new. */
       }
  if (++movei > 665)
    {
      SCHRFUNC(RRPL) ;
      longpause(1) ;
      credits = 0 ;
      progstate = INITGAME ;
    }
}


void
process_button(str)        /* Do appropriate panel button item action. */
char *str ;
{
  if (EQUAL(str, but_names[(int) BUT_HELP]))                 /* Help. */
    {
      clear_screen() ;
      dohelp() ;
      progstate = INITGAME ;
    }
  else if (EQUAL(str, but_names[(int) BUT_PROPS]))           /* Props. */
    {
      position_popup(W_PROPS) ;
      show_window(W_PROPS, TRUE) ;
    }
  else if (EQUAL(str, but_names[(int) BUT_SCORES]))          /* Scores. */
    {
      position_popup(W_SCORES) ;
      show_window(W_SCORES, TRUE) ;
    }
  else if (EQUAL(str, but_names[(int) BUT_QUIT]))            /* Quit. */
    exit(0) ;
  else if (EQUAL(str, but_names[(int) BUT_NEW]))             /* New Game. */
    {
      stopped = 0 ;
      activate_button(BUT_HELP,   FALSE) ;
      activate_button(BUT_PROPS,  FALSE) ;
      activate_button(BUT_SCORES, FALSE) ;
      activate_button(BUT_NEW,    FALSE) ;
      activate_choice(C_PLAY, FALSE) ;
      autoplay = !get_choice(C_PLAY) ;
      if (autoplay == TRUE) numplayers = 1 ;
      else                  lastnumplayers = numplayers ;
      started   = TRUE ;
      progstate = INITGAME ;
      warp_mouse() ;
      set_cursor(FALSE) ;
      set_label(BUT_STOP) ;
      set_timer(TRUE) ;
    }
  else if (EQUAL(str, but_names[(int) BUT_STOP]))            /* Stop. */
    {
      stopped = 1 ;
      activate_button(BUT_HELP,   TRUE) ;
      activate_button(BUT_PROPS,  TRUE) ;
      activate_button(BUT_SCORES, TRUE) ;
      activate_button(BUT_NEW,    TRUE) ;
      activate_choice(C_PLAY, TRUE) ;
      set_cursor(TRUE) ;
      set_label(BUT_CONT) ;
      set_timer(FALSE) ;
    }
  else if (EQUAL(str, but_names[(int) BUT_CONT]))            /* Continue. */
    {
      stopped = 0 ;
      activate_button(BUT_HELP,   FALSE) ;
      activate_button(BUT_PROPS,  FALSE) ;
      activate_button(BUT_SCORES, FALSE) ;
      activate_button(BUT_NEW,    FALSE) ;
      activate_choice(C_PLAY, FALSE) ;
      set_choice(C_PLAY, !autoplay) ;
      set_cursor(FALSE) ;
      set_label(BUT_STOP) ;
      set_timer(TRUE) ;
    }
  else if (EQUAL(str, but_names[(int) BUT_APPLY]))           /* Apply. */
    {
      numplayers = get_value(P_PLAYERS) + 1 ;
      activate_message(M_PLAYER1, (numplayers >= 1)) ;
      activate_message(M_PLAYER2, (numplayers >= 2)) ;
      activate_message(M_PLAYER3, (numplayers >= 3)) ;
      activate_message(M_PLAYER4, (numplayers >= 4)) ;
      skilllevel = get_value(P_SKILL) ;
      highscore  = allhighscores[skilllevel].score ;
      set_score(HIGH_SCORE, highscore) ;
      write_cmdline() ;
    }
  else if (EQUAL(str, but_names[(int) BUT_RESET]))           /* Reset. */
    {
      set_value(P_PLAYERS, numplayers) ;
      set_value(P_SKILL,   skilllevel) ;
    }
  do_flush() ;
}


void
removecircle()
{
  set_image((enum image_type) (numcir[player]-1),
            (enum icon_type) ((int) CIRCLES + ((int) RIGHT*4)), RXOR) ;
}


void
resetmaze()
{
  int i ;

  erasebugs() ;
  longpause(1) ;
  for (i = 1; i <= 20; i++)
    {
      blt_mem_to_scrn(oldcx - GOFFSET + 5, oldcy - GOFFSET + 5,
                      45, 45, RXOR, CURCIRCLE, 0, 0) ;
      longpause(3) ;
    }
  longpause(1) ;
  if (fruitmaze[player] < 8) fruitmaze[player]++ ;
  fruitchances[player] = 0 ;
  setdots(player) ;
  drawmaze() ;
  if (curbluetime[player] > 1) curbluetime[player] -= 60 ;
}


/* Returns the reverse direction of the parameter (left goes to right, etc.) */

enum dir_type
reversedir(dir)
enum dir_type dir ;
{
  return((enum dir_type) (((int) dir + 2) % 4)) ;
}


void
setdots(player)
int player ;
{
  int x,y ;

  for (y = 1; y <= YSIZE; y++)
    {
      dots[player][0][y] = NODOT ;
      dots[player][1][y] = NODOT ;
      dots[player][XSIZE+2][y] = NODOT ;
      dots[player][XSIZE+3][y] = NODOT ;
      for (x = 1; x <= XSIZE; x++)
        if (maze[x][y] == '.')
          {
            dots[player][x+1][y] = SMALLDOT ;
            numdots[player]++ ;
          }
        else if (maze[x][y] == '*')
          {
            dots[player][x+1][y] = BIGDOT ;
            numdots[player]++ ;
          }
        else dots[player][x+1][y] = NODOT ;
    }      
}


/*  Move each bug one bit in appropriate direction; change direction
 *  if appropriate.
 */

void
updatebugs()
{
  register struct bugrec *g ;
  int bemean ;

  for (g = &bugs[(int) POKEY]; g <= &bugs[(int) SHADOW]; g++)
    {
      g->count++ ;
      if (g->inbox || g->intunnel)
        if (g->count % 2 == 0) return ;               /* Slow in box. */

      if (g->bluetime > 0)
        {
          if (g->count % CATCHUP == 0) return ;       /* Go slower if blue. */
          drawbug(g) ;                  /* Erase old before change blueTime. */
          g->bluetime-- ;
        }
      else drawbug(g) ;                    /* Erase old. */

      if (g->count % 7 == 0) g->pic = (g->pic + 1) % 2 ;

      if (gcentered(g->scrx,g->scry))
        {
          g->intunnel = tunnel[g->mx+1][g->my] ;

          if (!g->bluetime)
            if (skilllevel < 5)
              bemean = randomrange(0, 10-skilllevel-GIND(g)) == 0 ;
            else bemean = randomrange(0, skilllevel-5+GIND(g)) != 0 ;
          else bemean = FALSE ;

          if (g->inbox)
            if ((g->mx == boxx) && (g->my == boxy)) g->inbox = FALSE ;

          if (g->eyesonly)
            {
              if ((!g->enteringbox) && (g->mx == boxx) && (g->my == boxy))
                {
                  g->dir = DOWN ;
                  g->enteringbox = TRUE ;
                  DOINC(g->dir, g->scrx, g->scry, g->mx, g->my,
                        &x, &y, &nx, &ny) ;
                }
              else if (g->enteringbox)
                if ((g->my > boxy + 2) &&
                    (!doinc(g->dir, g->scrx, g->scry, g->mx, g->my,
                            &x, &y, &nx, &ny)))
                  {
                    g->dir = UP ;
                    g->enteringbox = FALSE ;
                    g->inbox = TRUE ;
                    g->eyesonly = FALSE ;
                    DOINC(g->dir, g->scrx, g->scry, g->mx, g->my,
                          &x, &y, &nx, &ny) ;
                  }
                else DOINC(g->dir, g->scrx, g->scry, g->mx, g->my,
                           &x, &y, &nx, &ny) ;
              else headto(boxx, boxy, g->scrx, g->scry, g->mx, g->my,
                          &g->dir, &x, &y, &nx, &ny) ;
            }
          else if (g->boxtime)          /* Inbox should be true also. */
            {
              g->boxtime-- ;
              if (g->boxtime < 0)       /* Heading to exit. */
                {
                  if (g->mx == boxx)    /* Found exit. */
                    {
                      g->boxtime = 0 ;
                      g->dir = UP ;
                      DOINC(g->dir, g->scrx, g->scry, g->mx, g->my,
                            &x, &y, &nx, &ny) ;
                    }
                  else headto(boxx, boxy, g->scrx, g->scry, g->mx, g->my,
                              &g->dir, &x, &y, &nx, &ny) ;
                }
              else if (!g->boxtime)      /* Start heading to exit. */
                {
                  g->boxtime = -1 ;
                  headto(boxx, boxy, g->scrx, g->scry, g->mx, g->my,
                         &g->dir, &x, &y, &nx, &ny) ;
                }
              else if (!doinc(g->dir, g->scrx, g->scry, g->mx, g->my,
                              &x, &y, &nx, &ny))
                {
                  g->dir = reversedir(g->dir) ; /* Bounce up an down a bit. */
                  DOINC(g->dir, g->scrx, g->scry, g->mx, g->my,
                        &x, &y, &nx, &ny) ;
                }
            }    
           else if (g->inbox)   /* Must be leaving the box; just keep going. */
             DOINC(g->dir, g->scrx, g->scry, g->mx, g->my,
                   &x, &y, &nx, &ny) ;
           else if (bemean)              /* Chase the circle. */
             headto(cirmx, cirmy, g->scrx, g->scry, g->mx, g->my,
                    &g->dir, &x, &y, &nx, &ny) ;
           else g->dir = dorandomdir(g->dir, g->scrx, g->scry, g->mx, g->my,
                                     &x, &y, &nx, &ny, 3) ;
         }
       else DOINC(g->dir, g->scrx, g->scry, g->mx, g->my, &x, &y, &nx, &ny) ;

       g->scrx = x ;
       g->scry = y ;
       g->mx = nx ;
       g->my = ny ;
       drawbug(g) ;                /* Draw new. */
    }
}


void
updatefruit()
{
  blt_mem_to_scrn(fruitx - GOFFSET, fruity - GOFFSET, 45, 45, RXOR,
                  (enum icon_type) ((int) FRUIT + fruitmaze[player]), 0, 0) ;
  if (fruiton)                                    /* Turning fruit off. */
    {
      fruitchances[player]++ ;
      if (fruitchances[player] > 2)
        fruittime = -1 ;                          /* Already had 2 chances. */
      else fruittime = randomrange(1000, 2500) ;
    }            
  else fruittime = randomrange(500, 1000) ;       /* Turning fruit on. */
  fruiton = !fruiton ;
}                
                 
                 
void
updatescore(amt) 
int amt ;        
{                
  int i, temp ;
                 
  if (autoplay)  
    if (!demomode)
      {          
        autoscore += amt ;
        set_score(AUTO_SCORE, autoscore) ;
        return ; 
      }          
  temp = score[player] + amt ;
  if (temp >= 1000)
    if (score[player] < 1000)
      {
        make_sound(S_NEWSCREEN) ;
        for (i = 1; i < 7; i++)
          {
            blt_mem_to_scrn(oldcx - GOFFSET + 5, oldcy - GOFFSET + 5,
                            45, 45, RXOR, CURCIRCLE, 0, 0) ;
            ppause(pausetime * 100) ;
          }
        numcir[player]++ ;
        blt_mem_to_scrn(30 + (numcir[player]-1) * 60, 20, 50, 50, RXOR,
                    (enum icon_type) ((int) CIRCLES + ((int) RIGHT*4)), 0, 0) ;
      }
  score[player] = temp ;
  set_score(player, score[player]) ;

  if (score[player] > highscore)
    {
      highplayer = player ;
      highscore  = score[player] ;
      set_score(HIGH_SCORE, highscore) ;
    }
}
