/*
     Copyright (c) 1994    Frank Adelstein

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */

/************/
/* INCLUDES */
/************/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Viewport.h>

#ifdef HASNETAUDIO
#include <audio/audiolib.h>
#include <audio/soundlib.h>
#include <audio/Xtutil.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>

/*************/
/* DEFINES   */
/*************/

#define TITLE     "crock"
#define CLASS     "Xrock"
#define FONTNAME  "crockfont"

/* environment variable name */
#define CROCKROOTENV "CROCKROOT"

#define POPUPWIDTH  550
#define POPUPHEIGHT 250
#define MAXINDEX 200
#define MAXMOVES 50
#define MAXSTRENGTH 100

/* sizes of various bitmaps */
#define NUMBERW 32
#define NUMBERH 32

/* game mode types */
#define GOOD 0
#define EVIL 1

/* why then the darkening of the light */
#define MAXDIM   128
#define MINDIM   4

#define GRAVITY  10
#define DRAG     5
#define FREEZESLEEP 35
#define ZAPSLEEP 10

#define COMPUTER 0
#define HUMAN    1

#define BEFORETIME  20
#define FIGHTTIME   10
#define DURINGTIME  500
#define ENDTIME     50

/* things player->nextmove can be assigned */
#define NONE   0
#define LEFT   1
#define RIGHT  2
#define WIN    3
#define LOSE   4

/* things gamestate can be */
#define BEFORE 0
#define FIGHT  1
#define DURING 2
#define END    3
#define POSE   4
#define AFTER  5

/* z position of objects */
#define FRONT    0
#define BACK     1
#define ANYWHERE 2

/* object types */
#define BLOOD 0
#define PLAYER 1
#define HEART 2
#define WEIGHT 3
#define SHOT 4
#define HEAD 5
#define BODY 6

/* volume stuff */
#define VOLUME_FORMAT        "Volume: %3d%%"
#define MIN_VOLUME         1
#define MAX_VOLUME         200
#define DEFAULT_VOLUME     100

#define MIN(x,y) ((x<y)?x:y)
#define MAX(x,y) ((x>y)?x:y)

#ifdef hpux
#define random rand
#define srandom srand
#endif

/*************/
/* STRUCTS   */
/*************/

typedef struct {
  int   x1, y1, x2, y2;
} coord;

/* RImage -- contains information on a bitmap (cached image) */
typedef struct {
  char   tag[50];	/* tag or name of bitmap */
  Pixmap pm;		/* actual bitmap */
  Pixmap mask;		/* mask for bitmap */
  int    width, height;	/* height and width of bitmap */
  int    offsetx, offsety;	/* x and y offset for bitmap */
  coord  attack;	/* boundary area of bitmap that is attacking */
  coord  high;		/* boundary area of bitmap for high part of body */
  coord  middle;	/* boundary area of bitmap for middle part of body */
  coord  low;		/* boundary area of bitmap for low part of body */
} RImage;

#ifdef HASNETAUDIO
/* Bucket -- containts information on a bucket (cached sound) */
typedef struct {
  char       tag[50];	/* tag or name of sound */
  AuBucketID sound;	/* actual sound */
} Bucket;
#endif

/* Move -- contains information on a move made of a sequnce of images */
typedef struct {
  char         movename[25];		/* name of the move */
  char         soundname[50];		/* name of tag of sound for move */
  int          frameindex[25];		/* index of frames for move */
  int          framedelay[25];		/* delay for each frame of move */
  int          maxindex;		/* number of elements in array used */
  int          damage;			/* damage a move does */
  int          xoff, yoff;		/* x and y force it does */
  int          flags;			/* any special attributes */
  int          x, y;			/* position (only used for 
					   projectiles) */
} Move;

/* Item -- contains information about an object */
typedef struct {
  int           x, y;			/* x and y position */
  int           z;			/* FRONT, BACK or ANYWHERE */
  int           xoff, yoff;		/* x and y velocity */
  int           sleep;			/* how long for current frame */
  int           ttl;			/* time to live in system ticks */
  int           sequence;
  int           seqnum;
  int           objecttype;		/* blood, ice, gore, weapon ... */
  int           gravity;		/* 0 = no gravity, 1 = falls down */
  int           effects;		/* who does this object effect?
					   0 = no one, 1 = player1, 
					   2 = player2, 3 = both */
  Move          *moves;			/* note: this points to someone
					   else's array of moves */
  RImage        *Images1, *Images2;	/* note: this points to someone
					   else's array of images */
  GC            gc1, gc2;		/* gc used to draw image */
} Item;

/* Backnode -- linked list structure of backgrounds */
typedef struct Backnode_t {
  struct Backnode_t     *prev;
  struct Backnode_t     *next;
  char                   name[30];		/* name of image */
  char                   path[100];		/* path to file */
  int                    isloaded;		/* do we need to load it? */
  Pixmap                 backg1, backg2;	/* bitmaps */
  int                    xoffset, yoffset;	/* bitmap offsets */
  int                    width, height;	/* height and width of bitmap */
} Backnode;

/* Listnode -- linked list structure of items */
typedef struct Listnode_t {
  struct Listnode_t     *prev;
  struct Listnode_t     *next;
  Item        object;
} Listnode;

/* Keynode -- linked list structure of key bindings (strings) */
typedef struct Keynode_t {
  struct Keynode_t     *prev;
  struct Keynode_t     *next;
  char                  *binding;
} Keynode;

/* Voicenode -- linked list structure of sounds */
typedef struct Voicenode_t {
  struct Voicenode_t     *prev;
  struct Voicenode_t     *next;
#ifdef HASNETAUDIO
  AuBucketID              sound;
#else
  int                     sound;
#endif
} Voicenode;

/* Attacktype -- struct of different ways they attacked */
typedef struct Attacktype_t {
  int                     highhit;	/* landed a high hit on opponent */
  int                     mediumhit;	/* landed a mid  hit on opponent */
  int                     lowhit;	/* landed a low  hit on opponent */
  int                     bighit;	/* fired projectile hit opponent */
  int                     teleport;	/* used teleport during the round */
} Attacktype;

/* Player -- contains information on a player */
typedef struct {
  int           width;			/* player width */
  int           x, y;			/* x and y values of player */
  int           xoff, yoff;		/* x and y velocity */
  int           sequence;		/* move sequence of player */
  int           seqnum;			/* index into move sequence */
  int           nextmove;		/* moving left, right or none */
  int           status;			/* winning, losing, or none */
  int           locked;			/* only allow 1 attack at a time */
  int           teleport;		/* 0-> no teleport; 1-6-> disappearing
					   7-> zipping; 8-13->reappearing */
  int           frozen;			/* non-zero -> attacks in a 
					     blocked move are harmless,  
					   2 -> player got hit by freeze */
  int           facing;			/* which way is player facing */
  int           stepsize;		/* size of step when char. moves */
  int           attrib;			/* current player attributes */
  int           maxattrib;		/* max achievable attributes of char */
  int           sleep;			/* number of ticks before next move */
  int           strength;		/* health level */
  int           wins;			/* number of wins */
  int           playertype;		/* player is computer or human */
  Attacktype    attacks;		/* what type of attacks were used */
  Move          moves[MAXMOVES]; 	/* array of moves player has */
  int           maxmoves;		/* max index of moves array */
  char          *movetable[MAXMOVES];	/* array of names of moves */
  int           maxmovetable;		/* max index of array of move names */
  RImage        Images1[MAXINDEX], Images2[MAXINDEX];	/* array of bitmaps */
  int           Maxindex;		/* max index of images */
#ifdef HASNETAUDIO
  Bucket       bucket1[MAXINDEX], bucket2[MAXINDEX];	/* array of sounds */
  int           maxbuckets;		/* max index of sounds */
#endif
  Keynode       *keybindings;		/* array of l.l. of keybindings */
  int           lasthit[3];		/* record the last 3 moves that hit */
  int           lmindex;		/* index into last move queue */
  Listnode      *projectiles;		/* linked list of projectile fired */
} Player;


/* Glob -- the global data struct on general state info of game, etc. */
typedef struct {
  unsigned int  wdepth1, wdepth2;	/* # of bitplanes for each display */
  unsigned int  playwidth, playheight;	/* width and height of playfield */
  int           volume1, volume2;	/* sound volume */
  int           continue1, continue2;	/* set when user clicks "play again" */
  int           screen1, screen2;	/* display 1/2 is left/right player */
  int           infatality;		/* currently executing a fatality? */
  int           dimbackground;		/* grey out the background? */
  int           shake;			/* shake the screen? */
  int           numscreens;		/* one or two screens to run on */
  int           round;			/* what round is this */
  int           gamemode;		/* blood and gore or rated G */
  int           gamestate;		/* before, during, after a match */
  int           gametimer;		/* time left in match, till match 
					   starts or after match till other
					   player drops */
  int           debugflag;		/* bit value for debug flags
					   1 = verbose bitmap load info
					 */
  Listnode      *objectlist;		/* linked list of objects */
  XtAppContext  Appcon1;		/* X application context */
  GC            blackgc1, blackgc2;	/* X graphics contexts */
  GC            whitegc1, whitegc2;	
  GC            redgc1, redgc2;	
  GC            bluegc1, bluegc2;
  GC            smallgc1, smallgc2;
  Backnode      *backgrounds;		/* linked list of background bitmaps */
  Backnode      *currentbg;		/* pointer to current background */
  char          *startsoundfile;	/* "program complete" sound fname */
#ifdef HASNETAUDIO
  AuBucketID    fight1, fight2;		/* sounds for "fight",  */
  AuBucketID    finish1, finish2;	/*   "finish him",      */
  AuBucketID    p1wins1, p1wins2;	/* "player one wins",   */
  AuBucketID    p2wins1, p2wins2;	/* "player two wins",   */
  AuBucketID    flawless1, flawless2;	/* "flawless victory ", */
  AuBucketID    fatality1, fatality2;	/* and "fatality"       */
  AuBucketID    startfatal1, startfatal2;	/* ominous snd  */
#else
  int          fight1, fight2, finish1, finish2;	/* dummy variables */
  int          p1wins1, p1wins2, p2wins1, p2wins2;	/* otherwise       */
  int          flawless1, flawless2, fatality1, fatality2;
  int          startfatal1, startfatal2;
#endif
  Pixmap        hand1, hand2;		/* hand holding score */
  Pixmap        handm1, handm2;		/* mask for hand holding score */
  Pixmap        grey4a, grey4b;		/* grey bitmaps used for stipple */
  Pixmap        grey8a, grey8b;
  Pixmap        grey16a, grey16b;
  Pixmap        grey32a, grey32b;
  Pixmap        grey64a, grey64b;
  Pixmap        grey128a, grey128b;
  Pixmap        zero1, zero2;		/* number bitmaps (they're small) */
  Pixmap        one1, one2;
  Pixmap        two1, two2;
  Pixmap        three1, three2;
  Pixmap        four1, four2;
  Pixmap        five1, five2;
  Pixmap        six1, six2;
  Pixmap        seven1, seven2;
  Pixmap        eight1, eight2;
  Pixmap        nine1, nine2;
  Pixmap        temp1, temp2;		/* temp bitmaps used for 2xbuffer */
  Widget        Toplevel1, Toplevel2;	/* toplevel widget for each display */
  Widget        shape1, shape2;		/* shaped window -- shhhhhh! */
#ifdef HASNETAUDIO
  AuServer      *aud1, *aud2;		/* connection to sound */
#endif
  Player        player1, player2;	/* player data structs */
} Glob;


/*************/
/* GLOBALS   */
/*************/

/**************/
/* PROTOTYPES */
/**************/

/* control.c */
Widget makecontrolpanel();

/* crock.c */
void main();
void ParseArgs();
void mainloop();

/* draw.c */
int ShakeIt();
void DrawIt();
void DrawBack();
void DrawPowerBar();
void DrawTime();
void DrawText();

/* fatality.c */
void DoTeleport();
void DimIt();

/* intersection.c */
void PlayerIntersection();

/* list.c */
void AddNode();
void DeleteNode();
void MakeBlood();
void MakeBlootAt();

/* load.c */
void ReadInBitmaps();
void updatemessage();
void XSetUp();
void XSetUp2();
void LoadGlobSound();
void LoadBackground();
void MakeGreyMaps();
void MakeNumberMaps();
void MakeHandMaps();

/* misc.c */
int  Findtag();
#ifdef HASNETAUDIO
int  Findsoundtag();
void Playsound();
void Playsoundfile();
void Playglobsound();
#endif
int  Findmove();
int  leftedge();
int  rightedge();
void Update();
void GetRumor();
void Initstuff();
void Quit();

/* move.c */
void HandleMove();
void ComputerPlay();

/* special.c */
void StartSpecialMove();
void MoveSpecialMove();

/* update.c */
void UpdatePlayerState();
int  ProjectileIntersection();
void ProjectileHit();
void UpdateObjects();
void UpdateGameState();
void HandleLevel();

/* zap.c */
void DrawZap();
void SpringZap();
