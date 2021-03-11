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

#include "types.h"
#include <varargs.h>


/* 
 * find the index that matches a tag, -1 if not found
 */

int Findtag (tag, player) 
char *tag;
Player *player;
{
  int index;
  for (index = 0; index < player->Maxindex; index++) 
    if (strcmp(player->Images1[index].tag, tag) == 0)
      return index;
  return -1;

}

#ifdef HASNETAUDIO
/* 
 * find the index that matches a tag, -1 if not found
 */

int Findsoundtag (tag, player) 
char *tag;
Player *player;
{
  int index;
  for (index = 0; index < player->maxbuckets; index++) 
    if (strcmp(player->bucket1[index].tag, tag) == 0)
      return index;
  return -1;

}

typedef struct {
  Voicenode *top;
  Glob      *glob;
} privateplayCBdata;

/* lint lint lint */
/*ARGSUSED*/
void 
PlaysoundCB(aud, which, event, calldata)
AuServer            *aud;
AuEventHandlerRec   *which;
AuEvent             *event;
privateplayCBdata       *calldata;
{
  AuBucketID  temp;
  AuStatus    ret_status;
  char errormsg[80];
  int  volume;

  if (calldata->top == NULL) {
    /* we're done */
    return;
  }

  /* there's something else to play, so play it */
  temp = calldata->top->sound;
  volume = (aud == calldata->glob->aud1) ? 
    calldata->glob->volume1: calldata->glob->volume2;

  DeleteNode(&calldata->top, calldata->top);

  AuSoundPlayFromBucket(aud, temp, AuNone, 
			AuFixedPointFromFraction(volume, 100),
			PlaysoundCB, calldata, 1, NULL, NULL, NULL, 
			&ret_status);

  if (ret_status != AuSuccess) {
    AuGetErrorText (aud, ret_status, errormsg, 80);
    fprintf(stderr, "AuSoundPlayFromBucket Error(%d): %s\n", 
	    ret_status, errormsg);
  }
}
/*
 * play a sound from a file
 */
void Playsoundfile (glob, filename)
Glob *glob;
char *filename;
{
  AuFlowID flow;
  AuStatus ret_status;
  char errormsg[80];
  
  if (glob->aud1) {
    AuSoundPlayFromFile(glob->aud1, filename, AuNone, 
			AuFixedPointFromFraction(glob->volume1, 100),
			NULL, NULL, &flow, NULL, NULL, &ret_status);

    if (ret_status != AuSuccess) {
      AuGetErrorText (glob->aud1, ret_status, errormsg, 80);
      fprintf(stderr, "AuSoundPlayFromBucket Error(%d): %s\n", 
	      ret_status, errormsg);
    }
    (void) AuFlush(glob->aud1);
  }
  if (glob->aud2) {
    AuSoundPlayFromFile(glob->aud2, filename, AuNone, 
			AuFixedPointFromFraction(glob->volume2, 100),
			NULL, NULL, &flow, NULL, NULL, &ret_status);
    (void) AuFlush(glob->aud1);
  }
  Update(glob);
}

/*
 * play a sound for a move
 */
void Playsound (glob, move, player)
Glob *glob;
char *move;
Player *player;
{
  int moveindex, soundindex;
  AuFlowID flow;
  AuStatus ret_status;
  char errormsg[80];
  
  moveindex = Findmove (move, player);
  soundindex = Findsoundtag (player->moves[moveindex].soundname, player);
  if (soundindex == -1) {
    /* no sound matches for that player */
    return;
  }
  if (glob->aud1) {
    AuSoundPlayFromBucket(glob->aud1, player->bucket1[soundindex].sound,
			  AuNone, AuFixedPointFromFraction(glob->volume1, 100),
			  NULL, NULL, 1, NULL, NULL, NULL,
			  &ret_status);

    if (ret_status != AuSuccess) {
      AuGetErrorText (glob->aud1, ret_status, errormsg, 80);
      fprintf(stderr, "AuSoundPlayFromBucket Error(%d): %s\n", 
	      ret_status, errormsg);
    }
  }
  if (glob->aud2) {
    AuSoundPlayFromBucket(glob->aud2, player->bucket2[soundindex].sound,
			  AuNone, AuFixedPointFromFraction(glob->volume2, 100),
			  NULL, NULL, 1, NULL, NULL, NULL, 
			  &ret_status);
  }
}

/*
 * play a list of global sounds (not a per player sound)
 */
void Playglobsound (va_alist)
va_dcl
{
  Glob *glob;
  int moveindex, soundindex;
  AuBucketID sound1, sound2;

  /* just in case */
  static privateplayCBdata cb1,  cb2;
  Voicenode *temp;

  /* variable argument stuff */
  va_list  ap;

  va_start(ap);
  glob = va_arg(ap, Glob *);

  cb1.glob = glob;
  cb2.glob = glob;

  /* build linked list of sounds to play */
  sound1 = va_arg(ap, AuBucketID);
  while (sound1 != NULL) {
    sound2 = va_arg(ap, AuBucketID);
    if ((temp = (Voicenode *) malloc (sizeof (Voicenode))) == NULL) {
      (void) fprintf(stderr, "Eeeek, can't malloc voicenode.\n");
      exit (-1);
    }
    temp->next = NULL;
    temp->prev = NULL;
    temp->sound = sound1;
    AddNode (&cb1.top, temp, sizeof (Voicenode));
    
    /* repeat for second other display */
    if (glob->aud2 != NULL) {
      if ((temp = (Voicenode *) malloc (sizeof (Voicenode))) == NULL) {
	(void) fprintf(stderr, "Eeeek, can't malloc voicenode.\n");
      exit (-1);
      }
      temp->next = NULL;
      temp->prev = NULL;
      temp->sound = sound2;
      AddNode (&cb2.top, temp, sizeof (Voicenode));
    }
    sound1 = va_arg(ap, AuBucketID);
    
  }

  /* let the callbacks deal with stuff */

  PlaysoundCB(glob->aud1, NULL, NULL, &cb1);

  if (glob->aud2) {
    PlaysoundCB(glob->aud2, NULL, NULL, &cb2);
  }
  va_end (ap);
}

#else
void Playsound (glob, move, player)
Glob *glob;
char *move;
Player *player;
{
  return;
}
void Playsoundfile (glob, filename)
Glob *glob;
char *filename;
{
  return;
}
void Playglobsound (glob, sound1, sound2)
Glob *glob;
void *sound1;
void *sound2;
{
  return;
}

#endif

/* 
 * find the index that matches a move, -1 if not found
 */

int Findmove (move, player) 
char *move;
Player *player;
{
  int index;
  for (index = 0; index < player->maxmoves; index++) 
    if (strcmp(player->moves[index].movename, move) == 0)
      return index;
  return -1;

}

/* 
 * function to suck all the events out of the queue
 * so updates and pending events (if any) will take effect
 */

void Update(glob)
Glob *glob;
{
  while (XtAppPending(glob->Appcon1)) {
    XtAppProcessEvent (glob->Appcon1, XtIMAll);
  }
  if (glob->numscreens == 2) {
    while (XtAppPending(glob->Appcon1)) {
      XtAppProcessEvent (glob->Appcon1, XtIMAll);
    }
  }
  return;
}

/* check if player is not beyond left edge of screen */
/*ARGSUSED*/
leftedge(glob, player, offset)
Glob *glob;
Player *player;
int offset;
{
  return (player->x + offset > 0);
}

/* check if player is not beyond right edge of screen */
rightedge(glob, player, offset)
Glob *glob;
Player *player;
int offset;
{
  return (player->x + offset + player->width/2 < (int) glob->playwidth);
}

#ifdef hpux
#include <poll.h>
usleep(x)
int x;
{ 
  struct pollfd fds;
  poll (NULL, 0, x/1000); 
}
#endif

/*
 * add the keybindings 
 */
AddBinding(glob, player, screen, which)
Glob      *glob;
Player    *player;
int        screen;
int        which;
{

  /* translation variables */
  char translation[100];
  XtTranslations trans_table;

  char    movename[50], binding[80];
  Keynode *temp;

  for (temp = player->keybindings; temp != NULL; temp = temp->next) {
    if (sscanf(temp->binding, "%*s %s %s", movename, binding) != 2) {
      /* shouldn't happen since this was checked earlier */
      (void) fprintf (stderr, "Can't parse line in input file: \n%s", 
		      temp->binding);
      continue;
    }	/* end if sscanf */
    /* add a translation for a human player */
    /* Note: this might cause problems for 64-bit architectures */
    (void) sprintf(translation, "%s: handlemove(%s, %u, %s)\n", 
		   binding, movename, (void *) glob, 
		   (which == LEFT)?"1":"2");
    
    /* now add the translations */
    trans_table = XtParseTranslationTable (translation);
    
    /* assert the key bindings */
    /* figure out which display to use */
    if (glob->numscreens == 2 && screen == 2) {
      XtOverrideTranslations (XtNameToWidget(glob->Toplevel2, "*Frame"), 
			      trans_table);
    } else {
      XtOverrideTranslations (XtNameToWidget(glob->Toplevel1, "*Frame"), 
			      trans_table);
    }	/* end if numscreens == 2 */
  }	/* end for */
}

/*
 * remove the keybindings 
 */
RemoveBinding(glob, screen)
Glob      *glob;
int        screen;
{
  
  if (glob->numscreens == 2 && screen == 2) {
    XtUninstallTranslations (XtNameToWidget(glob->Toplevel2, "*Frame"));
  } else {
    XtUninstallTranslations (XtNameToWidget(glob->Toplevel1, "*Frame"));
  }	/* end if numscreens == 2 */
}

void
GetRumor(glob, string)
Glob     *glob;
char     *string;
{

  char *filename;
  FILE *fp;
  int   numlines, rumorline, i;

  /* If it's computer vs. computer, there's no hints */
  if (glob->player1.playertype == COMPUTER &&  
      glob->player2.playertype == COMPUTER ) {
    *string = '\0';
    return;
  }

  /* For human vs. computer, if human wins use a true rumor, 
     if computer wins, use a false one */
  if (glob->player1.playertype == COMPUTER ||
      glob->player2.playertype == COMPUTER) {
    if ((glob->player1.status == WIN && glob->player1.playertype == HUMAN) ||
	(glob->player2.status == WIN && glob->player2.playertype == HUMAN)) {
      filename = TRUERUMOR;
    } else {
      filename = FALSERUMOR;
    }
  } else {
    /* it's human vs. human, use whatever */
    if (random() > 0.5) {
      filename = TRUERUMOR;
    } else {
      filename = FALSERUMOR;
    }
  }

  /* open the rumor file */
  if ((fp = fopen(filename, "r")) == NULL) {
    (void) fprintf(stderr, "Hey!  Can't open rumorfile %s!  Oh well.\n",
		   filename);
    *string='\0';
    return;
  }

  /* first line says how many lines of rumors there are */
  fscanf(fp, "%d", &numlines);
  
  rumorline = random() % numlines;
  for (i = 0; i <= rumorline; i++)
    fgets(string, 200, fp);

  /* kill the newline */
  string[strlen(string)-1] = '\0';
  
  (void) fclose(fp);
  return;
}

/* 
 * initialize stuff for a player 
 */
static void
initplayer (glob, player)
Glob     *glob;
Player   *player;
{

  if (player->projectiles)
    DeleteNode(&player->projectiles, player->projectiles);

  player->projectiles  = NULL;

  /* reset the players strength levels */
  player->sequence          = 0;
  player->seqnum            = 0;
  player->sleep             = 0;
  player->teleport          = 0;
  player->y                 = 0;
  player->nextmove          = NONE;
  player->status            = NONE;
  player->strength          = 0;
  player->lasthit[0]        = 0;
  player->lasthit[1]        = 0;
  player->lasthit[2]        = 0;
  player->lmindex           = 0;
  player->attacks.highhit   = 0;
  player->attacks.mediumhit = 0;
  player->attacks.lowhit    = 0;
  player->attacks.bighit    = 0;
  player->attacks.teleport  = 0;


  if (glob->round == 1) {
    /* reset values if it's the first round */
    player->wins       = 0;
  }
}

/*
 * initialize some variables 
 */

void Initstuff(glob)
Glob     *glob;
{
  /* free up object list */
  if (glob->objectlist)
    DeleteNode(&glob->objectlist, glob->objectlist);

  initplayer(glob, &glob->player1);
  initplayer(glob, &glob->player2);

  glob->objectlist           = NULL;
  glob->dimbackground        = 0;
  glob->infatality           = 0;
  glob->shake                = 0;

  /* players 1 and 2 start at different places on the screen */
  glob->player1.x            = -20;
  glob->player2.x            = 250;

  /* Note: velocity (offset) is not reset, since I think it looks */
  /* cool when someone punches someone, time runs out, the next   */
  /* round starts and they slide back a little.  Velocities are   */
  /* damped to zero by friction and gravity quickly anyway.       */

  if (glob->round == 1) {
    /* reset background image initially */
    if (glob->currentbg == NULL) {
      glob->currentbg = glob->backgrounds;
    } else {
      if (glob->currentbg->next != NULL) {
	/* cycle to next background */
	glob->currentbg = glob->currentbg->next;
      } else {
	/* otherwise go to back to first background */
	glob->currentbg = glob->backgrounds;
      }
    }
    if (glob->currentbg == NULL) {
      (void) fprintf(stderr, "Yikes!  We've got no background bitmap!\n");
      exit(1);
    }
    
    if (!glob->currentbg->isloaded) {
      LoadBackground(glob);
    }
  }	/* end if (round ==  1) */
}

/*
 * terminate program
 */
/*ARGSUSED*/
void Quit(glob) 
Glob     *glob;
{
  exit(0);
}
