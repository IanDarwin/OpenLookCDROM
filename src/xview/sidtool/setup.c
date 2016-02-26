
/*  @(#)setup.c 1.8 91/09/05
 *
 *  Initialisation, setup and cyclic routines used by sidtool.
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
#include <sys/types.h>
#include <time.h>
#include <sys/timeb.h>
#include "sidtool.h"
#include "extern.h"
#include "patchlevel.h"

#include "images/down.xbm"
#include "images/left.xbm"
#include "images/right.xbm"
#include "images/up.xbm"

#include "images/BASHFUL0.xbm"
#include "images/BASHFUL1.xbm"
#include "images/POKEY0.xbm"
#include "images/POKEY1.xbm"
#include "images/SHADOW0.xbm"
#include "images/SHADOW1.xbm"
#include "images/SPEEDY0.xbm"
#include "images/SPEEDY1.xbm"

#include "images/blueghost0.xbm"
#include "images/blueghost1.xbm"
#include "images/bluepics0.xbm"
#include "images/bluepics1.xbm"

#include "images/cirDOWN0.xbm"
#include "images/cirDOWN1.xbm"
#include "images/cirDOWN2.xbm"
#include "images/cirDOWN3.xbm"
#include "images/cirLEFT0.xbm"
#include "images/cirLEFT1.xbm"
#include "images/cirLEFT2.xbm"
#include "images/cirLEFT3.xbm"
#include "images/cirRIGHT0.xbm"
#include "images/cirRIGHT1.xbm"
#include "images/cirRIGHT2.xbm"
#include "images/cirRIGHT3.xbm"
#include "images/cirUP0.xbm"
#include "images/cirUP1.xbm"
#include "images/cirUP2.xbm"
#include "images/cirUP3.xbm"
 
#include "images/circleexplode0.xbm"
#include "images/circleexplode1.xbm"
#include "images/circleexplode2.xbm"
#include "images/circleexplode3.xbm"
#include "images/circleexplode4.xbm"
#include "images/circleexplode5.xbm"
#include "images/circleexplode6.xbm"
#include "images/circleexplode7.xbm"
#include "images/circleexplode8.xbm"

#include "images/cornerDL.xbm"
#include "images/cornerLU.xbm"
#include "images/cornerRD.xbm"
#include "images/cornerUR.xbm"

#include "images/eyesDOWN.xbm"
#include "images/eyesLEFT.xbm"
#include "images/eyesRIGHT.xbm"
#include "images/eyesUP.xbm"

#include "images/fruit1.xbm"
#include "images/fruit2.xbm"
#include "images/fruit3.xbm"
#include "images/fruit4.xbm"
#include "images/fruit5.xbm"
#include "images/fruit6.xbm"
#include "images/fruit7.xbm"
#include "images/fruit8.xbm"

#include "images/picbigdot.xbm"
#include "images/picsmalldot.xbm"


void
changeplayers(startgame)
int startgame ;
{
  int cnt ;

  if (numplayers == 1)
    {
      if (fruiton) updatefruit() ;
      gamestate = TRUE ;
      progstate = RESETGAME ;
      longjmp(exception, val) ;
    }
  cnt = 0 ;
  do
    {
      cnt++ ;
      player = 1 + (player % numplayers) ;
      if (cnt > 5)             /* Game all over. */
        {
          progstate = DOLEAVE ;
          longjmp(exception, val) ;
        }
    }
  while (!numcir[player]) ;
  clear_screen() ;
  longpause(1) ;
  drawmaze() ;
  blinkpause() ;
  if (!startgame)
    {
      gamestate = TRUE ;
      progstate = RESETGAME ;
      longjmp(exception, val) ;
    }
}


int
checkcollision(nx, ny, g)
register int nx, ny ;
int *g ;
{
  register struct bugrec *tg ;

  for (tg = &bugs[(int) POKEY]; tg <= &bugs[(int) SHADOW]; tg++)
    if (tg->mx == nx)
      if (tg->my == ny)
        if (!tg->eyesonly)
          {
            *g = GIND(tg) ;
            return(1) ;
          }
  return(0) ;
}


int
checkinc(dir, mx, my)
enum dir_type dir ;
int mx, my ;
{
  switch (dir)
    {
      case UP    : return(!walls[mx+2][my-1]) ;
      case RIGHT : return(!walls[mx+3][my]) ;
      case DOWN  : return(!walls[mx+2][my+1]) ;
      case LEFT  : return(!walls[mx+1][my]) ;
    }
  return(0) ;
}


void
destroyfruit()
{
  int inc ;

  fruitsgotten[player][fruitmaze[player]]++ ;
  updatefruit() ;                                    /* Turn fruit off. */
  inc = fruit_scores[fruitmaze[player]] ;
  updatescore(inc) ;
  SCHRFUNC(RXOR) ;
  make_sound(S_EATFRUIT) ;
  SPRINTF(buffer, "%1d0", inc) ;
  draw_text(fruitx + 10, fruity + 10, NORMALFONT, buffer) ;
  longpause(5) ;
  SPRINTF(buffer, "%1d0", inc) ;
  draw_text(fruitx + 10, fruity + 10, NORMALFONT, buffer) ;
  SCHRFUNC(RRPL) ;
}


/* Returns true if the screen position is in the center of a square. */

int
gcentered(scrx, scry)
int scrx, scry ;
{
  return(((scrx - XBASE) % SQUARE == 0) && ((scry - YBASE)% SQUARE == 0)) ;
}


void
get_icons()
{
  load_image(CORNER,    cornerUR_bits) ;
  load_image(CORNER1,   cornerRD_bits) ;
  load_image(CORNER2,   cornerDL_bits) ;
  load_image(CORNER3,   cornerLU_bits) ;

  load_image(BDOT,      picbigdot_bits) ;
  load_image(SDOT,      picsmalldot_bits) ;

  load_image(CIRCLES,   cirRIGHT0_bits) ;
  load_image(CIRCLES1,  cirRIGHT1_bits) ;
  load_image(CIRCLES2,  cirRIGHT2_bits) ;
  load_image(CIRCLES3,  cirRIGHT3_bits) ;
  load_image(CIRCLES4,  cirUP0_bits) ;
  load_image(CIRCLES5,  cirUP1_bits) ;
  load_image(CIRCLES6,  cirUP2_bits) ;
  load_image(CIRCLES7,  cirUP3_bits) ;
  load_image(CIRCLES8,  cirLEFT0_bits) ;
  load_image(CIRCLES9,  cirLEFT1_bits) ;
  load_image(CIRCLES10, cirLEFT2_bits) ;
  load_image(CIRCLES11, cirLEFT3_bits) ;
  load_image(CIRCLES12, cirDOWN0_bits) ;
  load_image(CIRCLES13, cirDOWN1_bits) ;
  load_image(CIRCLES14, cirDOWN2_bits) ;
  load_image(CIRCLES15, cirDOWN3_bits) ;

  load_image(BUGPICS,   POKEY0_bits) ;
  load_image(BUGPICS1,  POKEY1_bits) ;
  load_image(BUGPICS2,  BASHFUL0_bits) ;
  load_image(BUGPICS3,  BASHFUL1_bits) ;
  load_image(BUGPICS4,  SPEEDY0_bits) ;
  load_image(BUGPICS5,  SPEEDY1_bits) ;
  load_image(BUGPICS6,  SHADOW0_bits) ;
  load_image(BUGPICS7,  SHADOW1_bits) ;

  load_image(BLUEBUG,   blueghost0_bits) ;
  load_image(BLUEBUG1,  blueghost1_bits) ;
  load_image(BLUEPICS,  bluepics0_bits) ;
  load_image(BLUEPICS1, bluepics1_bits) ;

  load_image(EYES,      eyesRIGHT_bits) ;
  load_image(EYES1,     eyesUP_bits) ;
  load_image(EYES2,     eyesLEFT_bits) ;
  load_image(EYES3,     eyesDOWN_bits) ;

  load_image(EXPLODE,   circleexplode0_bits) ;
  load_image(EXPLODE1,  circleexplode1_bits) ;
  load_image(EXPLODE2,  circleexplode2_bits) ;
  load_image(EXPLODE3,  circleexplode3_bits) ;
  load_image(EXPLODE4,  circleexplode4_bits) ;
  load_image(EXPLODE5,  circleexplode5_bits) ;
  load_image(EXPLODE6,  circleexplode6_bits) ;
  load_image(EXPLODE7,  circleexplode7_bits) ;
  load_image(EXPLODE8,  circleexplode8_bits) ;

  load_image(FRUIT1,    fruit1_bits) ;
  load_image(FRUIT2,    fruit2_bits) ;
  load_image(FRUIT3,    fruit3_bits) ;
  load_image(FRUIT4,    fruit4_bits) ;
  load_image(FRUIT5,    fruit5_bits) ;
  load_image(FRUIT6,    fruit6_bits) ;
  load_image(FRUIT7,    fruit7_bits) ;
  load_image(FRUIT8,    fruit8_bits) ;

  load_image(FRUIT,     fruit1_bits) ;          /* Force creation. */
  load_image(CURCIRCLE, cirRIGHT0_bits) ;

  load_image(RJSTICK,   right_bits) ;           /* Joystick directions. */
  load_image(UJSTICK,   up_bits) ;
  load_image(LJSTICK,   left_bits) ;
  load_image(DJSTICK,   down_bits) ;
}


int
get_int_resource(rtype, intval)   /* Get integer resource from the server. */
enum res_type rtype ;
int *intval ;
{
  char *val ;

  if ((val = get_resource(rtype)) == NULL) return(0) ;
  *intval = atoi(val) ;
  return(1) ;
}


void
get_options(argc, argv)    /* Read and process command line options. */
int argc ;
char *argv[] ;
{
  char next[MAXLINE] ;     /* The next command line parameter. */
  int intval ;             /* Used to convert integer values. */

  INC ;
  while (argc > 0)
    {
      if (argv[0][0] == '-')
        switch (argv[0][1])
          {
            case 'd' : demomode = TRUE ;     /* Run in self demo. mode. */
                       break ;

            case 'm' : if (EQUAL(argv[0], "-maze"))
                         {
                           INC ;
                           getparam(m_name, argv,
                                    "-maze needs maze filename") ;
                         }
                       break ;

            case 'p' :      if (EQUAL(argv[0], "-pause"))
                         {
                           INC ;
                           getparam(next, argv,
                                    "-pause needs pause factor") ;
                           intval = atoi(next) ;
                           if (intval >= 0) lpauseval = intval ;
                         }
                       else if (EQUAL(argv[0], "-players"))
                         {
                           INC ;
                           getparam(next, argv,
                                    "-players needs number of players") ;
                           intval = atoi(next) ;
                           if (intval >= 1 && numplayers <= 4)
                             numplayers = intval ;
                         }

            case 'r' : if (EQUAL(argv[0], "-retained"))
                         retained = TRUE ;
                       break ;

            case 's' :      if (EQUAL(argv[0], "-scorefile"))
                         {
                           INC ;
                           getparam(s_name, argv,
                                    "-scorefile needs score filename") ;
                         }
                       else if (EQUAL(argv[0], "-skill"))
                         {
                           INC ;
                           getparam(next, argv,
                                    "-skill needs skill level") ;
                           intval = atoi(next) ;
                           if (intval >= 1 && intval <= 10)
                             skilllevel = intval ;
                         }
                       else if (EQUAL(argv[0], "-slug"))
                         {
                           INC ;
                           getparam(next, argv,
                                    "-slug needs slug factor") ;
                           intval = atoi(next) ;
                           if (intval >= 0) slugval = intval ;
                         }
                       break ;

            case 'v' :
            default  : FPRINTF(stderr, "%s V3.0.%1d\n", progname, PATCHLEVEL) ;
                       FPRINTF(stderr, "\n\nUsage: %s ", progname) ;
                       FPRINTF(stderr, "[-d] [-s scorefile] [-v]\n") ;
                       exit(1) ;
          }
      INC ;
    }
}


void
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


void
init_options()         /* Initialise default option values. */
{
  char *env ;

  orgx    = 0 ;                /* X origin of the sidtool window. */
  orgy    = 0 ;                /* Y origin of the sidtool window. */
  width   = SWIDTH ;           /* Width of the sidtool window. */
  height  = SHEIGHT ;          /* Height of the sidtool window. */
  speed   = SPEED ;            /* Default speed of machine. */

  numplayers = DEF_PLAYERS ;   /* Default number of players. */
  skilllevel = DEF_SKILL ;     /* Default skill level. */
  lpauseval  = LPAUSE ;        /* Default long pause value (microseconds). */
  slugval    = SLUG_MSECS ;    /* Default main loop time (millisecs). */

  SPRINTF(titlestring, "%s - Software Interactive Debugger V3.0.%1d.",
          progname, PATCHLEVEL) ;
  m_name[0] = '\0' ;           /* Use hard-wired maze by default. */
  STRCPY(s_name, S_NAME) ;     /* Default sidtool highscore filename. */
  canpaint = FALSE ;
  demomode = FALSE ;
  retained = FALSE ;           /* Drawing canvas not retained by default. */
  jdir     = LEFT ;            /* Initial joystick direction. */

  if ((env = getenv("SID_SCORE"))   != NULL) STRCPY(s_name, env) ;
}


void
initialize()
{
  int g, x, y ;
  FILE *fn ;

  SRAND((int) time((time_t *) NULL)) ;             /* Randomise start. */

  get_icons() ;

  if (m_name[0] == '\0')                           /* Use default maze? */
    {
      for (y = 0; y <= YSIZE+1; y++)
      for (x = 0; x <= XSIZE+1; x++) maze[x][y] = defmaze[y][x] ;
    }
  else
    {
      if ((fn = fopen(m_name, "r")) == NULL)
        {
          FPRINTF(stderr, "%s: unable to open maze file %s.\n",
                  progname, m_name) ;
          exit(1) ;
        }

      FPRINTF(stderr, "Reading alternative maze file.\n") ;
      for (y = 0; y <= YSIZE+1; y++)
        {
          FGETS(buffer, MAXLINE, fn) ;
          for (x = 0; x <= XSIZE+1; x++) maze[x][y] = buffer[x] ;
        }
      FCLOSE(fn) ;
    }

  transpt(FRUITMX, FRUITMY, &fruitx, &fruity) ;
  readallhighscores() ;
  highscore = allhighscores[skilllevel].score ;

  g = (int) POKEY ;
  for (y = 1; y <= YSIZE; y++)
    for (x = 1; x <= XSIZE; x++)
      if ((maze[x][y] >= '0') && (maze[x][y] <= '9'))
        {
          startpos[g].x = x ;
          startpos[g].y = y ;
          startpos[g].time = maze[x][y] - '0' ;
          if (g < (int) SHADOW) g++ ;
          if (maze[x][y] == '0')
            {
              boxx = x ;
              boxy = y ;
            }
        }    
  for (x = 0; x < XSIZE+3; x++)
    for (y = 0; y < YSIZE; y++) tunnel[x][y] = 0 ;
}


void
make_play()         /* Perform next movement of each sid tool object. */
{
  int diffm, diffs ;
  struct timeb before, after ;

  ftime(&before) ;
  updatebugs() ;
  if (checkcollision(cirmx, cirmy, &g)) handlecollision(&bugs[g]) ;
  if (fruittime != -1)
    {
      fruittime-- ;
      if (!fruittime) updatefruit() ;
    }
  newdir = curdir ;
  if (gcentered(posx, posy))
    {
      if (autoplay)
        newdir = dorandomdir(curdir, posx, posy, cirmx, cirmy,
                             &x, &y, &nx, &ny, 1) ;
      else
        switch (curdir)
          {
            case UP    :      if (sc == 'r' && !walls[cirmx+3][cirmy])
                           newdir = RIGHT ;
                         else if (sc == 'l' && !walls[cirmx+1][cirmy])
                           newdir = LEFT ;
                         else if (sc == 'd' && !walls[cirmx+2][cirmy+1])
                           newdir = DOWN ;
                         break ;
 
            case DOWN  :      if (sc == 'r' && !walls[cirmx+3][cirmy])
                           newdir = RIGHT ;
                         else if (sc == 'l' && !walls[cirmx+1][cirmy])
                           newdir = LEFT ;
                         else if (sc == 'u' && !walls[cirmx+2][cirmy-1])
                           newdir = UP ;
                         break ;

            case RIGHT :      if (sc == 'l' && !walls[cirmx+1][cirmy])
                           newdir = LEFT ;
                         else if (sc == 'u' && !walls[cirmx+2][cirmy-1])
                           newdir = UP ;
                         else if (sc == 'd' && !walls[cirmx+2][cirmy+1])
                           newdir = DOWN ;
                         break ;

            case LEFT  :      if (sc == 'r' && !walls[cirmx+3][cirmy])
                           newdir = RIGHT ;
                         else if (sc == 'u' && !walls[cirmx+2][cirmy-1])
                           newdir = UP ;
                         else if (sc == 'd' && !walls[cirmx+2][cirmy+1])
                           newdir = DOWN ;
                         break ;
          }
    }
  if (doinc(newdir, posx, posy, cirmx, cirmy, &x, &y, &nx, &ny)) doupdate() ;
  else
    { 
      if (!gcentered(posx, posy)) doupdate() ;          /* Until centered. */
      else
        { 
          if (oldcurdir != curdir)
            {
              blt_mem_to_scrn(oldcx - GOFFSET + 5, oldcy - GOFFSET + 5,
                              45, 45, RXOR, CURCIRCLE, 0, 0) ;
              blt_mem(CURCIRCLE, 0, 0, 45, 45, RRPL,
                      (enum icon_type) ((int) CIRCLES + ((int) curdir*4)),
                      0, 0) ;
              blt_mem_to_scrn(oldcx - GOFFSET + 5, oldcy - GOFFSET + 5,
                              45, 45, RXOR, CURCIRCLE, 0, 0) ;
              oldcurdir = curdir ;
            }
        }    
    }    
  draw_joystick(curdir) ;
  if (checkcollision(cirmx, cirmy, &g)) handlecollision(&bugs[g]) ;

/*  This is an attempt to fudge the correct speed, irrespective of the type
 *  of machine that sidtool is running on.
 */

  ftime(&after) ;
  diffs = after.time    - before.time ;
  diffm = after.millitm - before.millitm ;

  if (diffm < 0)
    {
      diffm = -diffm ;
      diffs = diffs - 1 ;
    }
  diffm += (diffs * 1000) ;
  if (diffm < slugval) usleep((unsigned int) ((slugval - diffm) * 1000)) ;
}


void
make_sound(stype)
enum sound_type stype ;
{
}


void
next_state()       /* Next iteration through the automation. */
{
  switch (progstate)
    {
      case STARTUP   :
                       SETJMP(exception) ;
                       if (progstate) break ;
                       else progstate = INITGAME ;
                       break ;
      case INITGAME  : initgame() ;
                       progstate = PLAY ;
                       break ;
      case PLAY      : play() ;
                       progstate = DOPLAY ;
                       break ;
      case DOPLAY    : doplay() ;
                       progstate = MAKEPLAY ;
                       break ;
      case MAKEPLAY  : make_play() ;
                       break ;
      case DOREST    : numplayers = 1 ;
                       progstate = HIGHSCORE ;
                       break ;
      case HIGHSCORE : writehighscore() ;
                       break ;
      case DOCREDIT  : docredits() ;
                       break ;
      case MOVELEFT  : move_left() ;
                       break ;
      case MOVERIGHT : move_right() ;
                       break ;
      case DOLEAVE   : SCHRFUNC(RRPL) ;
                       autoplay  = TRUE ;
                       set_choice(C_PLAY, !autoplay) ;
                       progstate = DOREST ;
                       break ;
      case RESETGAME : sremove   = gamestate ;
                       progstate = DOPLAY ;
    } 
}


void
play()                    /* Initialise for next player to play. */
{
  fruiton = 0 ;
  blueblink = 200 ;
  blueincblink = 25 ;
  if (numplayers == 1)
    {
      player = 1 ;
      drawmaze() ;
      blinkpause() ;
    }  
  else 
    {
      player = numplayers ;
      changeplayers(1) ;
    }
  sremove = TRUE ;
}      
 
       
int
randomrange(low, high)     /* Return a random number between low and high. */
int low,high ;
{      
  return((((int) random() & 077777) % (high-low+1)) + low) ;
}


void
read_resources()          /* Read all possible resources from database. */
{
  int intval ;

  if (get_int_resource(R_PLAYERS, &intval)) numplayers = intval ;
  if (get_int_resource(R_SKILL,   &intval)) skilllevel = intval ;
  if (get_int_resource(R_PAUSE,   &intval)) lpauseval  = intval ;
  if (get_int_resource(R_SLUG,    &intval)) slugval    = intval ;
}


void
restore_screen()          /* Called when window needs to be drawn. */
{
  int g ;

  if (!canpaint || !redraw++) return ;
  clear_screen() ;
  if (!started)
    {
      set_cursor(TRUE) ;
      dohelp() ;
    }
  else if (credits)
    {
      dohelp() ;
      blt_scrn(3, ciry-37, 762, 100, RSET) ;
      if (credits == 1)
        {
          drawdot(dotx, doty, BIGDOT) ;
          drawcir((enum icon_type) ((int) CIRCLES + ((int) LEFT*4 + inc)),
                  cirx, ciry) ;
        }
      else
        drawcir((enum icon_type) ((int) CIRCLES + ((int) RIGHT*4 + inc)),
                cirx, ciry) ;
      for (g = (int) POKEY; g <= (int) SHADOW; g++)
        if (!bugs[g].eyesonly) drawbug(&bugs[g]) ;
    }
  else
    {
      set_cursor(FALSE) ;
      drawmaze() ;
      for (g = (int) POKEY; g <= (int) SHADOW; g++) drawbug(&bugs[g]) ;
      drawcir((enum icon_type) ((int) CIRCLES + ((int) curdir*4) + inc),
              posx, posy) ;
    }
}


/* Convert from maze coordinates to screen coordinates. */

void
transpt(mx, my, scrx, scry)
int mx, my, *scrx, *scry ;
{
  *scrx = (mx - 1) * SQUARE + XBASE ;
  *scry = (my - 1) * SQUARE + YBASE ;
}


/* Convert from screen coordinates to maze coordinates. */

void
untranspt(scrx, scry, mx, my)
int scrx, scry, *mx, *my ;
{
  *mx = ((scrx - XBASE) / SQUARE) + 1 ;
  *my = ((scry - YBASE) / SQUARE) + 1 ;
}
