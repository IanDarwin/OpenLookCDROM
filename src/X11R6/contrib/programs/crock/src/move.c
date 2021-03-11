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

/* 
 * player wants to move.  Deal with it.
 */
/* next line is for lint */
/*ARGSUSED*/
void HandleMove(w, event, params, num_params)
Widget          w;
XExposeEvent   *event;
char          **params;
int            *num_params;
{
  Glob   *glob;
  Player *player;
  int     temp;
  extern Glob *Gglob;
  /*
   * Calling conventions: param[0] is the movename, 
   *                      param[1] is *glob
   *                      param[2] is *player
   */
  glob = Gglob;
  player = (*params[2]=='1')? &glob->player1:&glob->player2;

  if (strcmp(*params, "quit") == 0) Quit(glob);

  if (player->locked && 
      strcmp(player->moves[player->sequence].movename, "highhit") &&
      strcmp(player->moves[player->sequence].movename, "block") &&
      strcmp(player->moves[player->sequence].movename, "crouch") ||
      glob->gamestate == POSE || player->status == LOSE ) return;

  if (strcmp(player->moves[player->sequence].movename, "block") &&
      strcmp(player->moves[player->sequence].movename, "crouch")) {
    /* not blocking or crouching */
    if (strcmp(*params, "unblock") == 0 || 
	strcmp(*params, "uncrouch") == 0) {
      /* unblock or uncrouch was selected but we weren't blocking 
         or crouching.  most likely the block timed out.  Basically
	 we just want to ignore this case.
       */
      return; 
    } else if ((temp = Findmove(*params, player)) != -1) {
      /* check for special moves */
      if (player->moves[temp].flags) {
	if (player->moves[temp].flags & player->attrib
	    || 1) {
	  /* player->attrib disabled for now, I'm sick of it */
	  /* move has some special properties and 
	     player is allowed to use it -- deal with it */
	  StartSpecialMove(glob, player, temp);
	} else {
	  /* can't do selected move */
	  return;
	}	/* end else if flags & attrib */
      }	else { 
	/* initiate the (non-special) selected move */
	Playsound(glob, *params, player);
	player->sequence = temp;
	player->seqnum   = 0;
	player->sleep    = player->moves[temp].framedelay[0];
	player->locked   = 1;
      }	/* end else flags  */
    }	/* end if findmove */
  }	/* end if !block */
  else if (!strcmp(player->moves[player->sequence].movename, "block") && 
	   !strcmp(*params, "unblock")) {
    /* unblocking */
    player->sequence  = 0;
    player->seqnum    = 0;
    player->locked    = 0;
    player->sleep      = player->moves[0].framedelay[0];
  }	/* end else if block && unblock */
  else if (!strcmp(player->moves[player->sequence].movename, "crouch") && 
	   !strcmp(*params, "uncrouch")) {
    /* uncrouching */
    player->sequence  = 0;
    player->seqnum    = 0;
    player->locked    = 0;
    player->sleep      = player->moves[0].framedelay[0];
  }	/* end else if crouch && uncrouch */
  else if (!strcmp(player->moves[player->sequence].movename, "crouch") &&
	   !strcmp(*params, "fatality")) {
    temp = Findmove("bigfatality", player);
    /* have to be crouching for this to work... */
    if (player->moves[temp].flags) {
      StartSpecialMove(glob, player, temp);
    }
  }	/* end else if crouch && fatal */
}

void cComputerPlay(glob, player)
Glob   *glob;
Player *player;
{

  char *args[5];
  char globstr[20], playerstr[20];
  static long seed;
  static int firsttime = 1;
  int choice;
  int  i;

  if (firsttime) {
    srandom(time(NULL) * getpid());
    firsttime = 0;
  }

  if (player->playertype == HUMAN) {
    /* not our problem */
    return;
  }
  
  /* 
   * decide on what to do.  First approximation
   * is a naive approach that'll do the following
   * with the following probabilities:
   *
   * nothing: 40% (0<x<40)
   * move left: 10% (40<x<50)
   * move right: 10% (50<x<60)
   * block: 10% (60<x<70)
   * attack: 30% (70<x<100)
   * 
   * note that attacks are chosen at random from the list.
   * (blocking will be added later)
   */

  /* I think this next line doesn't work under 64-bit architectures */
  (void) sprintf(globstr, "%u", (void *) glob);
  strcpy(playerstr,(player == &glob->player1)?"1":"2");

  choice = random() % 100;

  if (choice < 40) {
    return;
  } else if (choice < 50) {
    /* go right */
    args[0] = "moveright";
  } else if (choice < 60) {
    /* go left */
    args[0] = "moveleft";
  } if (choice < 70) {
    /* (un)block */
    if (!strcmp(player->moves[player->sequence].movename, "block")) {
      args[0] = "unblock";
    } else {
      args[0] = "block";
    }
  } else {
    /* choose an attack */
    i = random() % player->maxmovetable;
    args[0] = player->movetable[i];
    if (!strcmp("quit", args[0]) ||
	!strcmp("block", args[0]) ||
	!strcmp("unblock", args[0])) {
      /* don't let the computer quit or directly (un)block */
      return;
    }
  }

  args[1] = globstr;
  args[2] = playerstr;

  /* invoke the callback directly (so we're cheating a little */
  HandleMove(NULL, NULL, args, 3);
}

/*
 * take II at making a computer player 
 */

/* prefixed with 'c' to avoid name collisions */
typedef enum responses {cNOTHING, cATTACK, 
			cBLOCK, cJUMP, cCROUCH,
			cBACK, cFORWARD, cFIRE, 
			cTELEPORT, cFATALITY} responses;

/* go through the array of probabilities, until we find the right one */
/* return the corresponding response */
static responses
findresponse (situation, response, num)
int        *situation;
responses  *response;
int         num;
{
  int i = 0;
  while (i < 100) {
    i += *situation;
    if (num < i) {
      return *response;
    }
    situation++;
    response++;
  }
}

void 
ComputerPlay(glob, player)
Glob   *glob;
Player *player;
{

  Player *otherplayer;

  char *args[5];
  char globstr[20], playerstr[20];
  static long seed;
  static int firsttime = 1;
  int choice, randnum;
  int  i;


  /*
   * the 7 situations to check for:
   *       projectile fired
   *       opponent attacking
   *       opponent blocked
   *       oppenent frozen
   *       we won
   *       close to oppenent
   *       far from oppenent
   *
   * There are 10 possible responses:
   *       attack
   *       fire projectile
   *       move closer
   *       move away
   *       jump
   *       block
   *       crouch
   *       teleport
   *       fatality
   *       nothing
   *
   * Note that the 'attack' move encompasses many possible moves,
   * as does the fatality.
   *
   * Each situation has several possible responses.  The
   * situation array describes the probabilities to invoke
   * each of these responses (and must add up to 100, we're dealing
   * with ints here, no need to bother with floats).  
   * The response array indicates
   * which response corresponds to each probability in the situation 
   * array.
   */

  static int s1[] = {25, 10, 25, 40};
  static int s2[] = {25, 10, 25, 40}; 
  static int s3[] = {65, 5, 5, 25};
  static int s4[] = {40, 15, 10, 5, 30};
  static int s5[] = {30, 15, 30, 25}; 
  static int s6[] = {65, 2, 15, 28}; 
  static int s7[] = {5, 30, 10, 55};

  static int *situation[] = {s1, s2, s3, s4, s5, s6, s7};

  static responses r1[] = {cBLOCK, cJUMP, cCROUCH, cNOTHING}; /* projectile */
  static responses r2[] = {cBLOCK, cJUMP, cCROUCH, cNOTHING}; /* attacking  */
  static responses r3[] = {cATTACK, cBACK, cFIRE, cNOTHING};  /* blocked    */
  static responses r4[] = {cATTACK, cFORWARD, cTELEPORT, cFIRE, cNOTHING};  
							      /* frozen     */
  static responses r5[] = {cATTACK, cFIRE, cFATALITY, cNOTHING};
  							      /* win        */
  static responses r6[] = {cATTACK, cFIRE, cBACK, cNOTHING};  /* near       */
  static responses r7[] = {cFIRE, cFORWARD, cTELEPORT, cNOTHING};
							      /* far        */

  static responses *response[] = {r1, r2, r3, r4, r5, r6, r7};

  /* otherplayer will be whatever "player" is not (i.e. player1 or player2) */
  otherplayer = (player == &glob->player1)?&glob->player2:&glob->player1;


  if (firsttime) {
    srandom(time(NULL) * getpid());
    firsttime = 0;
  }

  if (player->playertype == HUMAN) {
    /* not our problem */
    return;
  }

  randnum = random() % 100;

  /* set to default to pretend we're close  */
    i = 6;

  /* check for the 7 situations */

  if (otherplayer->projectiles != NULL) {
    if ((player->facing == RIGHT && 
	otherplayer->projectiles->object.x > player->x) ||
	  (player->facing == LEFT &&
	   otherplayer->projectiles->object.x < player->x)) {
	    /* there's a projectile */
      i = 0;
    }
  } else if (otherplayer->frozen == 1) {
    /* other player is blocked */
    i = 2;
  } else if (otherplayer->frozen == 2) {
    /* otherplayer is frozen */
    i = 3;
  } else if (otherplayer->moves[otherplayer->sequence].damage) {
    /* other player is attacking */
    i = 1;
  } else if (player->status == WIN) {
    /* we won */
    i = 4;
  } else if (abs (player->x - otherplayer->x) < 200) {
    /* near */
    i = 5;
  } else if (abs (player->x - otherplayer->x) > 300) {
    /* far */
    i = 6;
  } 

  /* find which response to use */
  choice = findresponse(situation[i], response[i], randnum);

  switch (choice) {
  case cNOTHING:
    return;
    break;
  case cATTACK:
    if (player->sequence == Findmove("block", player)) {
      /* we have to unblock before we can attack */
      args[0] = "unblock";
    } else if (player->sequence == Findmove("crouch", player)) {
      /* we have to uncrouch before we can attack */
      args[0] = "uncrouch";
    } else {
      /* choose an attack */
      randnum = random() % player->maxmovetable;
      args[0] = player->movetable[randnum];
      if (!strcmp("quit", args[0]) ||
	  !strcmp("block", args[0]) ||
	  !strcmp("unblock", args[0]) ||
	  !strcmp("throwcleaver", args[0]) ||
	  !strcmp("jump", args[0]) ||
	  !strcmp("teleport", args[0])) {
	/* don't let the computer quit or directly (un)block */
	return;
      }
    }
    break;
  case cBLOCK:
    args[0] = "block";
    break;
  case cJUMP:
    args[0] = "jump";
    break;
  case cCROUCH:
    args[0] = "crouch";
    break;
  case cBACK:
    if (player->facing == LEFT) {
      args[0] = "moveright";
    } else {
      args[0] = "moveleft";
    }
    break;
  case cFORWARD:
    if (player->facing == LEFT) {
      args[0] = "moveleft";
    } else {
      args[0] = "moveright";
    }
    break;
  case cFIRE:
    args[0] = "throwcleaver";
    break;
  case cTELEPORT:
    args[0] = "teleport";
    break;
  case cFATALITY:
    do {
      if (player->sequence == Findmove("block", player)) {
	/* we have to unblock before we can attack */
	args[0] = "unblock";
	break;
      } else if (player->sequence == Findmove("crouch", player)) {
	/* we have to uncrouch before we can attack */
	args[0] = "uncrouch";
	break;
      } else {
	/* choose an attack */
	randnum = random() % player->maxmovetable;
	args[0] = player->movetable[randnum];
	if (!strcmp("quit", args[0]) ||
	    !strcmp("block", args[0]) ||
	    !strcmp("unblock", args[0])) {
	  /* don't let the computer quit or directly (un)block */
	  return;
	}
      }
    } while (!player->moves[randnum].flags);
    break;
  }

  /* I think this next line doesn't work under 64-bit architectures */
  (void) sprintf(globstr, "%u", (void *) glob);
  strcpy(playerstr,(player == &glob->player1)?"1":"2");

  args[1] = globstr;
  args[2] = playerstr;

  /* invoke the callback directly (so we're cheating a little */
  /* format of the arguments is : movename globptr playerN */
  HandleMove(NULL, NULL, args, 3);
}
