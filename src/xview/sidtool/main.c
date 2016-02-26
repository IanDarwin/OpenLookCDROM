
/*  @(#)main.c 1.9 91/09/05
 *
 *  Main routine for the sidtool program.
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
#include <sys/fcntl.h>
#include <sys/param.h>
#include "sidtool.h"

jmp_buf exception ;
int val ;

struct scorerec allhighscores[11] ;
struct startrec startpos[4] ;
struct bugrec bugs[4] ;           /* The bad guys. */

char *but_names[] = {             /* Button labels. */
  "Help",
  "Props...",
  "Scores...",
  "Quit",
  "New Game",
  "    Stop   ",
  "Continue",
  "Apply",
  "Reset"
} ;

/*  REQUIREMENTS FOR MAZE:
 *
 *  The maze size is fixed at 26 by 28.
 *  The maze is not allowed to have any dead ends.
 *  All boxes in the maze are defined by paths.
 *  The upper left corner of each box is signified by a small s.
 *  Use the letters r,d,l,u to signify the direction to travel around boxes.
 *  Corners must be signified by changing the letter.
 *  The tunnel start must be signified by a capital S.
 *  The exit box must have the opening at the top.
 *  The ghost number 0 must be started directly above this exit.
 *  The exit should be signified by capital R's.
 *  All ghosts except for one must start in the box.
 *  The amount of time spent in the box is specified by the number which
 *    shows where the ghost goes.     
 *  Small dots in the maze are signified by periods '.';
 *  Large dots in the maze are signified by asterisks '*';
 *  Tunnels may be on left and right. 
 *  All tunnels must have exits at both sides.
 *  There must be a row around the entire maze defining the border.
 *  All non-tunnel parts of the border must be 'x's
 *  The area in the tunnel in which the ghosts go slowly is defined as the
 *     area between the exit and the first non-space character.  Thus a '.'
 *     causes the ghosts to speed up.
 */

char *defmaze[] = {
  "rrrrrrrrrrrrrdxrrrrrrrrrrrrd",
  "u............d u....0......d",
  "u.sd.srrrrrd.d u.srRRrd.sd.d",
  "u*ud.udlllll.rru.u   8d.ud*d",
  "u.ul.ud..........u98  d.ud.d",
  "u....ud.sd.sd.sd.ulllll.ul.d",
  "ulll.ud.ul.ud.ud...........d",
  "x  u.ud....ud.ud.srrrrrd.dll",
  "Srru.ul.rrrud.ud.u dllll.rrx",
  "    ....ullll.ul.ull....    ",
  "Srrrrrd..............sd.rrrx",
  "dllllll.srrrrrrrd.rd.ud.ulll",
  "d.......udlllllll*ud.ud....u",
  "d.sd.sd.ud........ud.ud.sd.u",
  "d.ud.ul.ud.srrrrd.ud.ul.ud.u",
  "d.ud....ud.ulllll.ud....ul.u",
  "d.urrrd.ud        ud.sd....u",
  "d.ullld.urrrrrrrrrud.ud.sd.u",
  "d....ud.ulllllllllll.ud.ud.u",
  "d.sd.ud..............ud.ud*u",
  "d*ud.urd.srrrrrrrrrd.ud.ud.u",
  "d.ul.uld.ullllllllll.ul.ul.u",
  "d.....ud.....  ............u",
  "rrrrd.urrrd.srrrrd....srd.ru",
  "xllll.ullld.ulll d....ull.uT",
  "   ......ud....uld.sd.....  ",
  "Srd.srrd.ud.rd..ud.ul.srd.rx",
  "x d.ulll.ul.urd.ul....ull.ux",
  "x d.........u d....rd.....ux",
  "xxrrrrrrrrrruxrrrrrurrrrrrux"
} ;

char *helpstrs[] = {
  "Move the terminal screen around the maze.",
  "Eating a small dot = 10 points.",
  "Eating an 'idea' spot = 50 points.",
  "Eating fruit when visible = 100 up to 5000 points.",
  "Eating all the dots changes fruit to one worth more points.",
  "Eating an 'idea' spot turns bad guys white.",
  "While white or blinking, bad guys = 200, 400, 800 and 1600 points.",
  "Avoid being eaten by bad guys when not white or blinking.",
  "",
  "Three screens per game.  Extra screen awarded at 10000 points.",
  "",
  "Movement of the screen is achieved with the following keys:",
  "  LEFT - 'h'    DOWN - 'j'    UP - 'k'    RIGHT - 'l'",
  "",
  "or by using the mouse as a joystick.",
  "",
  "Skill level 1 = easy; 10 = hard. Standard = 5.",
  "",
  "These are various option buttons at the top of the window.",
  "",
  "  Help      - gives you this help message.",
  "  Props     - allows you to set the number of players and the skill level",
  "  Scores    - shows you the high score for each level.",
  "  Quit      - terminates SID tool.",
  "  New Game  - starts a new game. Set play mode to demo or game first.",
  "  Stop      - stops the game (if running).",
  "",
  (char *) NULL
} ;

char *names[] = {
  "Time Dependencies",
  "Uninitialized Variables",
  "Fence Posts",
  "Multiple Process Interaction"
} ;

char *resources[] = {              /* Sidtool resources read. */
  "players",                       /* Integer: number of players. */
  "skillLevel",                    /* Integer: skill level. */
  "pauseFactor",                   /* Integer: pause factor. */
  "slugFactor"                     /* Integer: main loop slug factor. */
} ;

char maze[XSIZE+2][YSIZE+2] ;
char sc, buffer[MAXLINE] ;
char m_name[MAXLINE] ;             /* Alternate maze filename. */
char s_name[MAXLINE] ;             /* Score file name. */
char *progname ;                   /* This programs name. */
char titlestring[MAXLINE] ;

int fruit_scores[] = { 0, 10, 30, 50, 70, 100, 200, 300, 500 } ;

int blueblink, blueincblink, boxx, boxy ;
int fruittime, fruitx, fruity, numplayers ;
int skilllevel, circatchup, pausetime ;
int highplayer, autoscore, lastnumplayers ;
int curbluetime[MAXNUMPLAYERS+1], score[MAXNUMPLAYERS+1] ;
int numcir[MAXNUMPLAYERS+1], fruitmaze[MAXNUMPLAYERS+1] ;
int numdots[MAXNUMPLAYERS+1], fruitchances[MAXNUMPLAYERS+1] ;
int fruitsgotten[MAXNUMPLAYERS+1][9] ;
int highscore, player, cirmx, cirmy, bugssincedot ;
int walls[XSIZE+6][YSIZE+1] ;
int tunnel[XSIZE+4][YSIZE+2] ;

int c ;                 /* Contains latest mouse or keyboard interaction. */
int g, posx, posy, x, y, nx, ny, count, inc ;
int orgx, orgy, width, height ;   /* Position and dimension of window. */
int oldcx,oldcy ;                 /* Old position of the screen. */
int on = 0 ;                      /* Current blinking state of score. */
int canpaint ;                    /* Set, when drawing to the canvas is ok. */
int credits ;                     /* Direction of credits if on. */
int cirx ;                        /* X position of screen during credits. */
int ciry ;                        /* Y position of screen during credits. */
int dotx ;                        /* X position of BIGDOT during credits. */
int doty ;                        /* Y position of BIGDOT during credits. */
int lpauseval ;                   /* Long pause value (in microseconds). */
int movei, movej, movex ;         /* Used to animate screen during credits. */
int redraw ;                      /* If set, then screen should be redrawn. */
int slugval ;                     /* Loop slug factor (in milliseconds). */
int speed ;                       /* Class (speed) of this machine. */
int started ;                     /* Indicates if we have started a game. */
int stopped ;                     /* Set if user has pressed Stop button. */
int retained ;                    /* Set, if the drawing canvas is retained. */

BOOLEAN autoplay ;
BOOLEAN demomode ;
BOOLEAN fruiton ;
BOOLEAN gamestate ;         /* State of the game, 1 = remove circle. */
BOOLEAN sremove ;           /* Whether screen should be removed. */
 
enum dir_type curdir ;      /* Current direction of the screen. */
enum dir_type jdir ;        /* Current joystick icon direction. */
enum dir_type newdir ;
enum dir_type oldcurdir ;   /* Old direction of the screen. */
enum dot_type dots[MAXNUMPLAYERS+1][XSIZE+4][YSIZE+2] ;
enum rop_type sfunc ;       /* Used by SCHRFUNC for cursor function. */
enum sid_type progstate ;   /* State machine for main loop. */


void
changebugs()
{
  register struct bugrec *p ;

  make_sound(S_EATBIGDOT) ;
  bugssincedot = 0 ;
  for (p = &bugs[(int) POKEY]; p <= &bugs[(int) SHADOW]; p++)
    if (!p->eyesonly)
      {
        drawbug(p) ;
        p->bluetime = curbluetime[player] ;
        if ((!p->boxtime) && (!p->inbox))
          p->dir = reversedir(p->dir) ;
        drawbug(p) ;                    /* Will be blue now. */
      }
}


/* Add boolean value to saved command line if TRUE. */

void
cmdbool(value, line, arg)
int value ;
char *line, *arg ;
{
  if (value == TRUE) STRCAT(line, arg) ;
}

 
/* Add integer argument to saved command line if not the default value. */
 
void
cmdint(value, defval, line, arg)
int value, defval ;
char *line, *arg ;
{
  char buf[MAXPATHLEN] ;
 
  if (value != defval)
    {
      SPRINTF(buf, arg, value) ;
      STRCAT(line, buf) ;
    }
}
 
 
/* Add string argument to saved command line if not NULL. */
 
void
cmdstr(value, line, arg)
char *value, *line, *arg ;
{
  char buf[MAXPATHLEN] ;

  if (value != NULL)
    {
      SPRINTF(buf, arg, value) ;
      STRCAT(line, buf) ;
    }
}


void
destroyblue(g)
register struct bugrec *g ;
{
  int i, inc, x ;
 
  drawbug(g) ;                  /* Turn off. */
  g->eyesonly = 1 ;
  g->bluetime = 0 ;
  inc = 20 ;
  for (i = 1; i <= bugssincedot; i++) inc *= 2 ;
  bugssincedot++ ;
  x = g->scrx + 10 ;
       if (x > 740) x = 740 ;
  else if (x < 5)   x = 5 ;
  SCHRFUNC(RXOR) ;
  SPRINTF(buffer, "%1d0", inc) ;
  draw_text(x, g->scry + 10, NORMALFONT, buffer) ;
  make_sound(S_EATBLUE) ;
  longpause(5) ;
  SPRINTF(buffer, "%1d0", inc) ;
  draw_text(x, g->scry + 10, NORMALFONT, buffer) ;
  SCHRFUNC(RRPL) ;
  drawbug(g) ;                  /* Turn on as eyesonly. */
  updatescore(inc) ;
}


void
do_startup()
{
  activate_message(M_PLAYER1, (numplayers >= 1)) ;
  activate_message(M_PLAYER2, (numplayers >= 2)) ;
  activate_message(M_PLAYER3, (numplayers >= 3)) ;
  activate_message(M_PLAYER4, (numplayers >= 4)) ;
  set_value(P_PLAYERS, numplayers) ;
  set_value(P_SKILL,   skilllevel) ;

  sfunc = RRPL ;
  set_cursor(FALSE) ;
  lastnumplayers = 1 ;
  autoplay  = FALSE ;
  initgame() ;
  initialize() ;
  redraw    = 0 ;             /* Don't redraw the screen, the first time. */
  started   = 1 ;
  autoplay  = 1 ;
  progstate = STARTUP ;
}


/*  Draws a box starting at maze position mx, my. Mx, my should be an
 *  s or S position in maze. Travels around path until reach x or s.
 */

void
drawbox(mx, my)
int mx, my ;
{
  int last ;

  last = 'r' ;
  ppause(pausetime * 10) ;
  walls[mx+2][my] = 1 ;
  if (maze[mx][my] == 's') drawcorner(mx, my, UR) ;
  else if (maze[mx][my] == 'S') drawline(mx, my, 'r') ;
  else if (maze[mx][my] == 'T')
    {
      drawline(mx, my, 'l') ;
      mx -= 2 ;
      last = 'l' ;
    }
   mx++ ;
   for (;;)
     {
       ppause(pausetime) ;
       walls[mx+2][my] = 1 ;
       switch (maze[mx][my])
         {
           case 's' :
           case 'S' :
           case 'T' : return ;
           case 'd' :      if (last == 'r') drawcorner(mx, my, RD) ;
                      else if (last == 'l') drawcorner(mx, my, UR) ;
                      else                  drawline(mx, my, 'd') ;
                      last = 'd' ;
                      my++ ;
                      break ;
           case 'l' :      if (last == 'd') drawcorner(mx, my, DL) ;
                      else if (last == 'u') drawcorner(mx, my, RD) ;
                      else                  drawline(mx, my, 'l') ;
                      last = 'l' ;
                      mx-- ;
                      break ;
           case 'r' :
           case 'R' :      if (last == 'u') drawcorner(mx, my, UR) ;
                      else if (last == 'd') drawcorner(mx, my, LU) ;
                      else                  drawline(mx, my, maze[mx][my]) ;
                      last = 'r' ;
                      mx++ ;
                      break ;
           case 'u' :      if (last == 'l') drawcorner(mx, my, LU) ;
                      else if (last == 'r') drawcorner(mx, my, DL) ;
                      else                  drawline(mx, my, 'u') ;
                      last = 'u' ;
                      my-- ;
                      break ;
           case 'x' : drawline(mx, my, last) ;
                      return ;
         }
      }   
}


void
drawbug(g)
register struct bugrec *g ;
{
  int inc, winc ;

  inc = g->scrx - GOFFSET ;
  winc = 0 ;

  if (inc < 0)
    if (inc <= 45) return ;
    else  winc = inc ;
  else if (inc > SWIDTH - 45)
    if (inc > SWIDTH - 1) return ;
    else
      { 
        winc = SWIDTH - 45 - inc ;
        inc = 0 ;
      }
  else inc = 0 ;

  if (g->eyesonly)
    {
      blt_mem_to_scrn(g->scrx - GOFFSET - inc, g->scry - GOFFSET,
                      45 + winc, 21, RXOR,
                      (enum icon_type) ((int) EYES + (int) g->dir),
                      -inc, 0) ;
/* Fake blt to get speed same. */
      blt_mem((enum icon_type) ((int) BUGPICS + (GIND(g)*2) + g->pic),
              0, 0, 45 + winc, 21, RRPL,
              (enum icon_type) ((int) BUGPICS + (GIND(g)*2) + g->pic), 0, 0) ;
    }
  else if (g->bluetime > 0)
    {
      if ((g->bluetime < blueblink) &&
          (g->bluetime % blueincblink > blueincblink / 2))
        blt_mem_to_scrn(g->scrx - GOFFSET - inc, g->scry - GOFFSET,
                        45 + winc, 45, RXOR,
                        (enum icon_type) ((int) BLUEPICS + g->pic), -inc, 0) ;
      else
        blt_mem_to_scrn(g->scrx - GOFFSET - inc, g->scry - GOFFSET,
                        45 + winc, 45, RXOR,
                        (enum icon_type) ((int) BLUEBUG + g->pic), -inc, 0) ;
/* Fake blt to get speed same. */
      blt_mem((enum icon_type) ((int) EYES + (int) g->dir),
               0, 0, 45 + winc, 21, RRPL,
              (enum icon_type) ((int) EYES + (int) g->dir), 0, 0) ;
    }
  else
    { 
      blt_mem_to_scrn(g->scrx - GOFFSET - inc, g->scry - GOFFSET,
                      45 + winc, 45, RXOR,
                      (enum icon_type) ((int) BUGPICS + (GIND(g)*2) + g->pic),
                      -inc, 0) ;
      blt_mem_to_scrn(g->scrx - GOFFSET - inc, g->scry - GOFFSET,
                      45 + winc, 21, RXOR,
                      (enum icon_type) ((int) EYES + (int) g->dir), -inc, 0) ;
    }
}


void
drawcir(p, x, y)      /* Draw the specified screen on the screen. */
enum icon_type p ;
int x, y ;
{
  if (x < 0) return ;                   /* Fully off left side. */
  else if (x > SWIDTH+1) return ;       /* Fully off right side. */

  if (!(oldcx == x && oldcy == y))
    {
      blt_mem_to_scrn(oldcx - GOFFSET + 5, oldcy - GOFFSET + 5,
                      45, 45, RXOR, CURCIRCLE, 0, 0) ;
      blt_mem(CURCIRCLE, 0, 0, 45, 45, RRPL, p, 0, 0) ;
      blt_mem_to_scrn(x - GOFFSET + 5, y - GOFFSET + 5,
                      45, 45, RXOR, CURCIRCLE, 0, 0) ;
      oldcx = x ;
      oldcy = y ;
    }
}


/*  Draw a corner at MAZE position mx,my turning the direction dir. */

void
drawcorner(mx, my, dir)
int mx, my ;
enum corner_type dir ;
{
  int x, y ;

  transpt(mx, my, &x, &y) ;
  blt_mem_to_scrn(x, y, SQUARE, SQUARE, RRPL,
                  (enum icon_type) ((int) CORNER + (int) dir), 0, 0) ;
}


void
drawdot(mx, my, size)    /* Draws a dot at the maze position mx,my. */
int mx, my ;
enum dot_type size ;
{
   transpt(mx, my, &mx, &my) ;
   if (size == BIGDOT)
     blt_mem_to_scrn(mx, my, 24, 24, RXOR, BDOT, 0, 0) ;
   else if (size == SMALLDOT)
     blt_mem_to_scrn(mx + 9, my + 9, 7, 7, RXOR, SDOT, 9, 9) ;
}


/*  Draw a maze line from mx,my in the direction dir.
 *  Parameters: dir should be d,u,l,r, (or R for a thin line).
 */

void
drawline(mx, my, dir)
int mx, my, dir ;
{
  int x, y ;

  transpt(mx, my, &x, &y) ;
  switch (dir)
    {
      case 'd' :
      case 'u' : blt_scrn(x + SQUARE/2, y, 2, SQUARE, RSET) ;
                 break ;
      case 'l' :
      case 'r' : blt_scrn(x, y + SQUARE/2, SQUARE, 2, RSET) ;
                 break ;
      case 'R' : blt_scrn(x, y + SQUARE/2, SQUARE, 1, RSET) ;
    }
}


void
drawmaze()     /* Draw the maze, the dots, the scores, etc. on the screen. */
{
  int x, y ;

  clear_screen() ;
  for (y = 0; y <= YSIZE; y++)
    {
      walls[1][y] = 1 ;
      walls[0][y] = 1 ;
      walls[XSIZE+4][y] = 1 ;
      walls[XSIZE+5][y] = 1 ;
      for (x = 0; x <= XSIZE+1; x++) walls[x+2][y] = 0 ;
    }

  for (y = 0; y <= YSIZE+1; y++)
    for (x = 0; x <= XSIZE+1; x++)
      if ((maze[x][y] == 's') || (maze[x][y] == 'S') || (maze[x][y] == 'T'))
        drawbox(x, y) ;
      else if (maze[x][y] == 'x') walls[x+2][y] = 1 ;   /* Borders. */

  ppause(pausetime * 10) ;
  blt_scrn(XBASE - (SQUARE / 2) - 2, YBASE - (SQUARE / 2) - 2,
           SQUARE * (XSIZE + 1) + 6, SQUARE * (YSIZE + 1) + 6, RINV) ;
  fixexits() ;

  for (y = 1; y <= YSIZE; y++)
    for (x = 1; x <= XSIZE; x++)
      if (dots[player][x+1][y] == SMALLDOT)
        {
          ppause(pausetime) ;
          drawdot(x, y, SMALLDOT) ;
        }
      else if (dots[player][x+1][y] == BIGDOT)
        {
          ppause(pausetime) ;
          drawdot(x, y, BIGDOT) ;
        }

  for (x = 1; x <= 4; x++) showplayerscore(x) ;
  set_score(HIGH_SCORE, highscore) ;

  for (x = 1; x <= numcir[player]; x++)
    {
      ppause(pausetime * 10) ;
      set_image((enum image_type) (x-1),
                (enum icon_type) ((int) CIRCLES + ((int) RIGHT*4)), RRPL) ;
   }

  set_image(CURFRUIT,
            (enum icon_type) ((int) FRUIT + fruitmaze[player]), RRPL) ;
  set_score(FRUIT_SCORE, fruit_scores[fruitmaze[player]]) ;

  if (autoplay && (!demomode))
    {
      SCHRFUNC(RXOR) ;
      draw_text(339, YBASE+SQUARE*16, NORMALFONT, "GAME OVER!") ;
      SCHRFUNC(RRPL) ;
      set_score(AUTO_SCORE, 0) ;
    }
}


void
erasebugs()         /* Erase all bugs from the screen. */
{
  int g ;

  for (g = (int) POKEY; g <= (int) SHADOW; g++)
    drawbug(&bugs[g]) ;
}


void
geths(fd, record)                /* Get one high score record in. */
int fd ;
struct scorerec *record ;
{
  char buffer[32], valuestr[7] ;
  int i ;

  i = read(fd, buffer, 23) ;
  for (i = 0; i < 16; i++) record->who[i] = (char) (buffer[i] ^ ENKEY) ;
  record->who[i] = '\0' ;
  for (i = 0; i < 7; i++) valuestr[i] = (char) (buffer[i+16] ^ ENKEY) ;
  record->score = atoi(valuestr) ;
}


void
handlecollision(g)
struct bugrec *g ;
{
  struct bugrec *tg ;

  if (g->bluetime > 0) destroyblue(g) ;
  else
    { 
      make_sound(S_DIE) ;
      drawbug(g) ;                         /* Erase one that ate screen. */
      explodecircle(posx, posy) ;
      for (tg = &bugs[(int) POKEY]; tg <= &bugs[(int) SHADOW]; tg++)
        if (tg != g) drawbug(tg) ;
      if (autoplay)
        {
          progstate = DOCREDIT ;
          longjmp(exception, val) ;
        }
      else if (!numcir[player])
        {
          SCHRFUNC(RXOR) ;
          draw_text(339, YBASE+SQUARE*16, NORMALFONT, "GAME OVER!") ;
          longpause(5) ;
          draw_text(339, YBASE+SQUARE*16, NORMALFONT, "GAME OVER!") ;
          SCHRFUNC(RRPL) ;
          if (numplayers == 1)
            {
              progstate = DOLEAVE ;
              longjmp(exception, val) ;
            }
        }
      else longpause(1) ;
      changeplayers(0) ;
    }
}


void
longpause(n)              /* Wait for a longer while. */
int n ;
{
  int i ;

  do_flush() ;
  for (i = 0; i < n; i++) usleep((unsigned int) lpauseval) ;
}


void
newbugs(drawthem)
int drawthem ;
{
  register struct bugrec *p ;

  for (p = &bugs[(int) POKEY]; p <= &bugs[(int) SHADOW]; p++)
    {
      p->dir = UP ;
      p->mx = startpos[GIND(p)].x ;
      p->my = startpos[GIND(p)].y ;
      transpt(p->mx, p->my, &(p->scrx), &(p->scry)) ;
      p->bluetime = 0 ;
      p->eyesonly = 0 ;
      p->boxtime = ((-2*skilllevel+25) / 5) * startpos[GIND(p)].time ;
      if (!p->boxtime) p->inbox = 0 ;
      else p->inbox = 1 ;
      p->enteringbox = 0 ;
      p->count = 0 ;
      p->delay = 5 ;
      p->pic = GIND(p) % 2 ;
      p->intunnel = 0 ;
      if (drawthem) drawbug(p) ;
    }
}


void
ppause(len)              /* Wait for len tics. */
int len ;
{
  int delay ;

  do_flush() ;
  delay = len / 200 ;
  if (delay <= 1) return ;

  usleep((unsigned int) delay) ;
}


void
puths(fd, record)                /* Put one high score record out. */
int fd ;
struct scorerec record ;
{
  char buffer[32], valuestr[7] ;
  int i, value ;

  for (i = 0; i < 16; i++) buffer[i] = record.who[i] ^ ENKEY ;
  value = record.score ;
  SPRINTF(valuestr, "%d", value) ;
  for (i = 0; i < 7; i++) buffer[i+16] = valuestr[i] ^ ENKEY ;
  WRITE(fd, buffer, 23) ;
}


/*  Reads all high scores and names into the global table allhighscores.
 *  If file not found, then sets all high scores to zero.
 */

void
readallhighscores()
{
  int hsfile, level ;

  if ((hsfile = open(s_name, O_RDWR)) == -1)
    {
      if ((hsfile = creat(s_name, 0777)) == -1)
        {
          FPRINTF(stderr,"%s: unable to create highscores file.\n", progname) ;
          return ;
        }

      for (level = 0; level <= 10; level++)
        {
          allhighscores[level].score = 0 ;
          STRCPY(allhighscores[level].who, " ") ;
          set_list(level) ;
          puths(hsfile, allhighscores[level]) ;
        }
    }    
  else
    for (level = 1; level <= 10; level++)
      {
/*###738 [cc] warning: passing arg 2 of `geths' makes integer from pointer without a cast%%%*/
/*###738 [cc] warning: passing arg 1 of `geths' makes pointer from integer without a cast%%%*/
        geths(hsfile, &allhighscores[level]) ;
        set_list(level) ;
      }
  CLOSE(hsfile) ;
}


void
savescorefile()     /* Write away new highscore values. */
{
  int fd, level ;

  allhighscores[skilllevel].score = highscore ;
  if ((fd = open(s_name, O_WRONLY)) == -1)
    FPRINTF(stderr,"%s: unable to open highscores file.\n", progname) ;
  else
    { 
      for (level = 1; level <= 10; level++)
        puths(fd, allhighscores[level]) ;
      CLOSE(fd) ;
    }
}


void
showplayerscore(player)
int player ;
{
  SCHRFUNC(RXOR) ;
  set_score(player, score[player]) ;
  SCHRFUNC(RRPL) ;
}


void
write_cmdline()
{
  char line[MAXPATHLEN] ;

  line[0] = NULL ;
  cmdbool(demomode,  line, " -d ") ;                        /* Demo. mode? */
  cmdint(lpauseval,  LPAUSE,      line, " -pause %d ") ;    /* Pause factor. */
  cmdint(numplayers, DEF_PLAYERS, line, " -players %d ") ;  /* # of players. */
  cmdint(skilllevel, DEF_SKILL,   line, " -skill %d ") ;    /* Skill level. */
  cmdint(slugval,    SLUG_MSECS,  line, " -slug %d ") ;     /* Slug factor. */
  cmdstr(s_name, line, " -scorefile %s ") ;             /* High score file. */
  save_cmdline(line) ;
}


/*  If highscore is better than old high score for this skill level then
 *  asks for player's name and enters name and score into table and writes
 *  file.
 */

void
writehighscore()
{
  if ((highscore >= allhighscores[skilllevel].score) &&
        (highplayer != -1) && (!demomode))
    {
      make_sound(S_ERROR) ;
      set_timer(FALSE) ;
      position_popup(W_SCORES) ;
      show_window(W_SCORES, TRUE) ;
      set_highscore(skilllevel) ;
    }
  else progstate = DOCREDIT ;
}
