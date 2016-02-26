
/*  @(#)extern.h 1.6 91/08/19
 *
 *  Contains the external variable definitions used by sidtool.
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

extern BOOLEAN autoplay ;
extern BOOLEAN changed ;
extern BOOLEAN demomode ;
extern BOOLEAN gamestate ;      /* State of the game, 1 = remove circle. */
extern BOOLEAN sremove ;        /* Whether Sun screen should be removed. */

extern char buffer[] ;
extern char *but_names[] ;
extern char *helpstrs[] ;              /* Help message strings. */
extern char *resources[] ;             /* Sidtool resource read. */

extern char *defmaze[] ;               /* Default SID tool maze. */
extern char maze[XSIZE+2][YSIZE+2] ;
extern char m_name[] ;                 /* Alternate maze filename. */
extern char *names[] ;
extern char *progname ;                /* This programs name. */
extern char s_name[] ;
extern char sc ;
extern char titlestring[] ;

extern int autoscore ;
extern int blueblink ;
extern int blueincblink ;
extern int boxx ;
extern int boxy ;
extern int bugssincedot ;
extern int c ;             /* Contains latest mouse or keyboard interaction. */
extern int canpaint ;      /* Set, when drawing to the canvas is ok. */
extern int circatchup ;
extern int cirmx ;
extern int cirmy ;
extern int cirx ;          /* X position of screen during credits. */
extern int ciry ;          /* Y position of screen during credits. */
extern int count ;
extern int credits ;       /* Direction of credits if on. */
extern int curbluetime[MAXNUMPLAYERS+1] ;
extern int dotx ;          /* X position of BIGDOT during credits. */
extern int doty ;          /* Y position of BIGDOT during credits. */
extern int fruit_scores[] ;
extern int fruitchances[MAXNUMPLAYERS+1] ;
extern int fruitsgotten[MAXNUMPLAYERS+1][9] ;
extern int fruiton ;
extern int fruittime ;
extern int fruitx ;
extern int fruity ;
extern int fruitmaze[MAXNUMPLAYERS+1] ;
extern int g ;
extern int gamestate ;
extern int height ;
extern int highplayer ;
extern int highscore ;
extern int inc ;
extern int lastnumplayers ;
extern int lpauseval ;        /* Long pause value (in microseconds. */
extern int movei ;            /* Used to animate screen during credits. */
extern int movej ;
extern int movex ;
extern int numcir[MAXNUMPLAYERS+1] ;
extern int numdots[MAXNUMPLAYERS+1] ;
extern int numplayers ;
extern int nx ;
extern int ny ;
extern int oldcx ;
extern int oldcy ;
extern int on ;            /* Current blinking state of score. */
extern int orgx ;
extern int orgy ;
extern int pausetime ;
extern int player ;
extern int posx ;
extern int posy ;
extern int redraw ;        /* If set, then screen should be redrawn. */
extern int retained ;      /* Set, if the drawing canvas is retained. */
extern int score[MAXNUMPLAYERS+1] ;
extern int skilllevel ;
extern int slugval ;       /* Loop slug factor (in milliseconds). */
extern int speed ;         /* Speed of this machine. */
extern int started ;
extern int stopped ;       /* Set if user has pressed Stop button. */
extern int tunnel[XSIZE+4][YSIZE+2] ;
extern int val ;
extern int walls[XSIZE+6][YSIZE+1] ;
extern int width ;
extern int x ;
extern int y ;

extern jmp_buf exception ;
extern long random() ;
extern enum dir_type dorandomdir() ;
extern enum dir_type reversedir() ;

extern enum dir_type curdir ;     /* Current direction of the screen. */
extern enum dir_type jdir ;       /* Current joystick icon direction. */
extern enum dir_type newdir ;
extern enum dir_type oldcurdir ;  /* Old direction of the screen. */
extern enum dot_type dots[MAXNUMPLAYERS+1][XSIZE+4][YSIZE+2] ;
extern enum rop_type sfunc ;      /* Used by SCHRFUNC for cursor function. */
extern enum sid_type progstate ;  /* State machine for main loop. */

extern struct bugrec bugs[4] ;         /* The bad guys. */
extern struct scorerec allhighscores[11] ;
extern struct startrec startpos[4] ;
