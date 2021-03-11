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
#include "flags.h"

/* state of the bigfatality */
static int bigfatalstate = 0;


/*
 * quick little routine to return 1 if player
 * is currently on frame i from the end, and 0 otherwise.
 */
whatframe(player, i) 
Player *player;
int     i;
{
  return (strcmp(player->Images1[player->moves[player->sequence].
				 frameindex[player->seqnum]].tag,
		 player->Images1[player->moves[player->sequence].
				 frameindex[player->moves[player->sequence].
					    maxindex - i]].tag) == 0);
  }

/* 
 * do whatever setup is required for a particular special move 
 */
void StartSpecialMove(glob, player, sequence)
Glob    *glob;
Player  *player;
int      sequence;
{
  Player *otherplayer;

  /* otherplayer will be whatever "player" is not (i.e. player1 or player2) */
  otherplayer = (player == &glob->player1)?&glob->player2:&glob->player1;

  switch (player->moves[sequence].flags) {
  case PROJECTILE:
    /* if it's a projectile sequence we're cool, don't have to do anything */
    break;
  case TELEPORT:
    /* if we're not teleporting already, start it up */
    if (player->teleport == 0) {
      player->teleport++;
      player->attacks.teleport++;
      Playsound(glob, "teleport", player);
    }
    /* this move does NOT lock the player, so just return immediately */
    return;
  case FLYINGKICK:
    /* if it's a flying kick, we're cool, don't have to do anything */
    break;
  case BLOCKOFF:
    /* for the head fatality, you have to be close enough */
    /* and the victor must have used only high hits */
    if (glob->gamestate == END && 
	abs(glob->player1.x - glob->player2.x) < 200 &&
	!(player->attacks.mediumhit | 
	  player->attacks.lowhit | 
	  player->attacks.bighit)) {
      glob->infatality++;
      Playglobsound(glob, glob->startfatal1, glob->startfatal2, NULL);
      glob->dimbackground = 4;
    } else {
      return;
    }
    break;
  case SIXTEENTONS:
    /* AND NOW...WHEN CONFRONTED WITH A FIEND COMING AT YOU WITH
       A BANANA, JUST RELEASE THE 16 TON WEIGHT ON HIM!!!! */

    /* has to be at the end of a round */
    if (glob->gamestate == END) {
      glob->infatality++;
      Playglobsound(glob, glob->startfatal1, glob->startfatal2, NULL);
      glob->dimbackground = 4;
    } else {
      return;
    }
    break;
  case HEARTFATALITY:
    /* heart fatality -- must be in right range */
    if (glob->gamestate == END &&
	abs(glob->player1.x - glob->player2.x) > 100 &&
	abs(glob->player1.x - glob->player2.x) < 300) {
      glob->infatality++;
      Playglobsound(glob, glob->startfatal1, glob->startfatal2, NULL);
      glob->dimbackground = 4;
    } else {
      return;
    }
    break;
  case NEARFATALITY:
    /* if it's a fatality sequence it has to be the end of the game
       and we have to be close enough to the other player */
    if (glob->gamestate == END && 
	abs(glob->player1.x - glob->player2.x) < 150) {
      glob->infatality++;
      Playglobsound(glob, glob->startfatal1, glob->startfatal2, NULL);
      glob->dimbackground = 4;
    } else {
      return;
    }
    break;
  case FARFATALITY:
    /* if it's a fatality sequence it has to be the end of the game
       and we have to be close enough to the other player */
    if (glob->gamestate == END && 
	abs(glob->player1.x - glob->player2.x) > 200) {
      glob->infatality++;
      Playglobsound(glob, glob->startfatal1, glob->startfatal2, NULL);
      glob->dimbackground = 4;
    } else {
      return;
    }
    break;
  case BIGFATALITY:
    /* if it's the end of the game, we're crouching
       with a flawless victory */
    if (glob->gamestate != END || player->strength < MAXSTRENGTH) {
      /* bomb out otherwise */
      return;
    }

    /* put player in the 'bigfatality' move */
    player->sequence = sequence;
    player->seqnum = 0;
    player->locked = 1;
    player->sleep  = player->moves[sequence].framedelay[0];
    
    /* knock both players back to their sides of the screen */
    glob->player1.xoff = 50;
    glob->player2.xoff = 50;
    
    /* make sure both players don't move for a while */
    glob->player2.sleep = 30;

    bigfatalstate = 0;
    glob->infatality++;
    Playglobsound(glob, glob->startfatal1, glob->startfatal2, NULL);
    break;
  }	/* end switch */

  /* initiate the selected move */
  player->sequence = sequence;
  player->seqnum   = 0;
  player->sleep    = player->moves[sequence].framedelay[0];
  player->locked   = 1;
  
}

/*
 * called during a special move to handle "special" things
 * that should happen during said move.
 * note that special moves can do things even when the
 * player is inbetween moves (player->sleep != 0)
 * so generally, we'll NOT do anything unless sleep is zero
 * (lightnening is the exception)
 */
void MoveSpecialMove(glob, player)
Glob    *glob;
Player  *player;
{
  Player *otherplayer;

  /* otherplayer will be whatever "player" is not (i.e. player1 or player2) */
  otherplayer = (player == &glob->player1)?&glob->player2:&glob->player1;
  
  switch (player->moves[player->sequence].flags) {
  case  PROJECTILE:
    if (player->sleep) return;

    /* if it's a projectile sequence ... */
    /* set the players proj field to point to the projectile move */
    /* which should be the next one after the currently selected one */
    if (player->projectiles) 
      /* if one has already been launched, we're done */
      return;
    if (player->seqnum == player->moves[player->sequence].maxindex - 2) {
      /* last frame of this sequence is when the projectile gets created */
      Listnode shot;

      /* let's make a shot */
      shot.next = NULL;
      shot.prev = NULL;
      shot.object.x = player->x;
      shot.object.y = -3;
      shot.object.z = FRONT;
      shot.object.xoff = player->moves[player->sequence + 1].xoff;
      shot.object.xoff *= player->facing == LEFT ? -1 : 1;
      shot.object.yoff = player->moves[player->sequence + 1].yoff;
      shot.object.ttl = 50;
      shot.object.objecttype = SHOT;
      shot.object.gravity = 0;
      shot.object.effects = (player == &glob->player1)? 2 : 1;
      shot.object.moves = player->moves;
      shot.object.Images1 = player->Images1;
      shot.object.Images2 = player->Images2;
      if (player->moves[player->sequence + 1].flags == FREEZE) {
	shot.object.gc1 = glob->bluegc1;
	shot.object.gc2 = glob->bluegc2;
      } else {
	shot.object.gc1 = glob->whitegc1;
	shot.object.gc2 = glob->whitegc2;
      }
      shot.object.sequence = player->sequence + 1;
      shot.object.sleep = 1;
      shot.object.seqnum = 0;

      /* sanity check */
      if (shot.object.moves[shot.object.sequence].flags != PROJECTIMAGE &&
	  shot.object.moves[shot.object.sequence].flags != FREEZE) {
	(void) fprintf (stderr, "EEEEKKK!  Error in config file!  Projectile image not in proper place!\n");
	break;
      }
      AddNode(&player->projectiles, &shot, sizeof(Listnode));
    }
    break;
  case FLYINGKICK:
    if (player->sleep) return;

    if (whatframe(player, 3)) {
      /* this frame's tag is the same as the third last frame of the
       * flying kick, so it's the 'in the air' frame. 
       * We need to make the character move to fly across the screen.
       * It's an ugly if, but then, that's why this is considered 
       * a special move.
       */
      int deltax;

      if (player->facing == LEFT) {
	/* move left */
	deltax = -player->stepsize;
	if (leftedge(glob, player, deltax)) {
	  player->x += deltax;
	}
      } else {
	/* move right */
	deltax = player->stepsize;
	if (rightedge(glob, player, deltax)) {
	  player->x += deltax;
	} 
      }
    }
    if (whatframe(player,1)) {
      /* if this is the last frame of the flying kick */
      if (player->facing == LEFT && player->x < otherplayer->x ||
          player->facing == RIGHT && player->x > otherplayer->x) {
        /* they've crossed, start teleport and it'll fix up things */
        player->teleport = 1;
      }
    }
    break;
  case SIXTEENTONS:
    /* AND NOW...WHEN CONFRONTED WITH A FIEND COMING AT YOU WITH
       A BANANA, JUST RELEASE THE 16 TON WEIGHT ON HIM!!!! */

    /* has to be at the end of a round */
    if (whatframe(player, 1) && player->sleep == 0) {
      Listnode weight;
      /* let's make a weight */
      weight.next = NULL;
      weight.prev = NULL;
      weight.object.x = otherplayer->x;
      weight.object.y = -400;
      weight.object.z = FRONT;
      weight.object.xoff = 0;
      weight.object.yoff = 0;	/* let gravity take its course */
      weight.object.ttl = 50;
      weight.object.objecttype = WEIGHT;
      weight.object.gravity = 1;	/* definitely */
      weight.object.effects = (otherplayer == &glob->player1) ? 1 : 2;
      weight.object.moves = player->moves;
      weight.object.Images1 = player->Images1;
      weight.object.Images2 = player->Images2;
      weight.object.gc1 = glob->blackgc1;
      weight.object.gc2 = glob->blackgc2;
      weight.object.sequence = Findmove("16tons", player);
      weight.object.sleep = 10;
      weight.object.seqnum = 0;
      AddNode(&glob->objectlist, &weight, sizeof(Listnode));
    }
    break;
  case HEARTFATALITY:
    if (whatframe(player, 6) && player->sleep == 0) {
      /* 4th from the end...in chest */
      otherplayer->sequence = Findmove("zap", otherplayer);
      if (otherplayer->sequence == -1) {
        otherplayer->sequence = 0;
      }
      otherplayer->seqnum = 0;
      otherplayer->sleep  = ZAPSLEEP;
      otherplayer->locked = 1;
      MakeBlood(glob, otherplayer);
      MakeBlood(glob, otherplayer);
    }
    if (whatframe(player, 5) && player->sleep == 0) {
      Listnode heart;
      /* 3rd from the end, otherplayer should collapse */
      otherplayer->sequence = Findmove("lose", otherplayer);
      MakeBlood(glob, otherplayer);
      MakeBlood(glob, otherplayer);
      /* let's make a heart */
      heart.next = NULL;
      heart.prev = NULL;
      heart.object.x = player->x;
      heart.object.y = -60;
      heart.object.z = FRONT;
      heart.object.xoff = 15 * player->facing == LEFT?1:-1;
      heart.object.yoff = 5;
      heart.object.ttl = 50;
      heart.object.objecttype = HEART;
      heart.object.gravity = 0;
      heart.object.effects = 0;
      heart.object.moves = player->moves;
      heart.object.Images1 = player->Images1;
      heart.object.Images2 = player->Images2;
      heart.object.gc1 = glob->redgc1;
      heart.object.gc2 = glob->redgc2;
      heart.object.sequence = Findmove("heart", player);
      heart.object.sleep = 10;
      heart.object.seqnum = 0;
      AddNode(&glob->objectlist, &heart, sizeof(Listnode));
    }
    break;
  case BLOCKOFF:
    if (whatframe(player, 3) && player->sleep == 0) {
      /* have to knock the other player's block off */
      /* make the other player headless and create a head object */

      Listnode head;

      otherplayer->sequence = Findmove("noheadstand", otherplayer);
      if (otherplayer->sequence == -1) {
	otherplayer->sequence = 0;
      }
      otherplayer->seqnum = 0;
      otherplayer->sleep  = otherplayer->moves[otherplayer->sequence].
	framedelay[0];

      /* let's make a head */
      head.next = NULL;
      head.prev = NULL;
      head.object.x = otherplayer->x;
      head.object.y = 0;
      head.object.z = (otherplayer == &glob->player1) ? BACK : FRONT;
      head.object.xoff = random() % 4 + 2;
      head.object.yoff = 25;
      head.object.ttl = 60;
      head.object.objecttype = HEAD;
      head.object.gravity = 0;
      head.object.effects = 0;
      head.object.moves = otherplayer->moves;
      head.object.Images1 = otherplayer->Images1;
      head.object.Images2 = otherplayer->Images2;
      head.object.gc1 = glob->blackgc1;
      head.object.gc2 = glob->blackgc2;
      head.object.sequence = Findmove("head", otherplayer);
      head.object.sleep = 10;
      head.object.seqnum = 0;
      AddNode(&glob->objectlist, &head, sizeof(Listnode));
    }
    break;
  case NEARFATALITY:
    /* 
     * if it's a massive uppercut punch, AND they're in about
     * the middle of the screen, AND we're on the
     * Fire Sword Shrine background, then move the punchee
     * out of the way and create an object to get impaled 
     * Level names shouldn't be coded here...sigh.
     */
    if (whatframe(player, 2) && player->sleep == 0 &&
	otherplayer->x > glob->playwidth/2 - 200 &&
	otherplayer->x < glob->playwidth/2 + 200 &&
	strncmp(glob->currentbg->name, "Fire Sword Shrine", 17) == 0) {
      
      /* let's make a body to be impaled */
      Listnode body;

      body.next = NULL;
      body.prev = NULL;
      body.object.x = 30;
      body.object.y = -300;
      body.object.z = BACK;
      body.object.xoff = 0;
      body.object.yoff = 50;
      body.object.ttl = 60;
      body.object.objecttype = BODY;
      body.object.gravity = 1;
      body.object.effects = 0;
      body.object.moves = otherplayer->moves;
      body.object.Images1 = otherplayer->Images1;
      body.object.Images2 = otherplayer->Images2;
      body.object.gc1 = glob->blackgc1;
      body.object.gc2 = glob->blackgc2;
      body.object.sequence = Findmove("impaled", otherplayer);
      body.object.sleep = 10;
      body.object.seqnum = 0;
      AddNode(&glob->objectlist, &body, sizeof(Listnode));

      glob->shake = 100;
      glob->dimbackground = 0;
      otherplayer->x = -500;
    }
    break;
  case FARFATALITY:
    if (whatframe(player, 4)) {
      /* it's 5th from the end frame */
      /* make sure the other player is getting zapped */
      DrawZap(glob, player, otherplayer);
      /* let's make him flash */
      if (otherplayer->frozen == 0) {
	otherplayer->frozen = 2;
      } else {
	otherplayer->frozen = 0;
      }
    }
    if (whatframe(player,5) && player->sleep == 0) {
      otherplayer->sequence = Findmove("zap", otherplayer);
      if (otherplayer->sequence == -1) {
        otherplayer->sequence = 0;
      }
      otherplayer->seqnum = 0;
      otherplayer->sleep  = ZAPSLEEP;
      otherplayer->locked = 1;
      glob->gamestate = POSE;
    }
    if (whatframe(player, 4) && player->sleep == 0) {
      /* no gore, if we're rated G */
      if (glob->gamemode == GOOD) 
	break;
      MakeBlood(glob, otherplayer);
      MakeBlood(glob, otherplayer);
      MakeBlood(glob, otherplayer);
      MakeBlood(glob, otherplayer);
      MakeBlood(glob, otherplayer);
      if (player->strength == MAXSTRENGTH) {
	/* flawless victories blow them to a skeleton */
	otherplayer->sequence = Findmove("skeleton", otherplayer);
        if (otherplayer->sequence == -1) {
          otherplayer->sequence = Findmove("lose", otherplayer);
        }
      } else {
	otherplayer->sequence = Findmove("noheadlose", otherplayer);
        if (otherplayer->sequence == -1) {
          otherplayer->sequence = Findmove("lose", otherplayer);
        }
      }
      otherplayer->seqnum = 0;
      otherplayer->frozen = 0;
      /* reset the sleep for the new image */
      otherplayer->sleep = otherplayer->moves[otherplayer->sequence].
	framedelay[otherplayer->seqnum];
    }
    break;
  case BIGFATALITY:
    switch (bigfatalstate) {
      Listnode bigguy;
    case 0:
    case 1:
    case 2:
    case 3:
    case 4:
    case 5: bigfatalstate++;
      break;

    case 6:
      /* big guy that jumps down is actually just another object
	 and his punch is part of the sequence with an appropriate delay */
      
      /* let's make a bigguy */
      bigguy.next = NULL;
      bigguy.prev = NULL;
      bigguy.object.x = glob->playwidth/2;
      bigguy.object.y = -100;
      bigguy.object.z = FRONT;
      bigguy.object.xoff = 0;
      bigguy.object.yoff = 0;
      bigguy.object.ttl = 50;
      bigguy.object.objecttype = PLAYER;
      bigguy.object.gravity = 1;
      bigguy.object.effects = 0;
      bigguy.object.moves = player->moves;
      bigguy.object.Images1 = player->Images1;
      bigguy.object.Images2 = player->Images2;
      bigguy.object.gc1 = glob->blackgc1;
      bigguy.object.gc2 = glob->blackgc2;
      bigguy.object.sequence = Findmove("bigguy", player);
      bigguy.object.sleep = 10;
      bigguy.object.seqnum = 0;
      AddNode(&glob->objectlist, &bigguy, sizeof(Listnode));
      bigfatalstate++;
      break;
    case 7:
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    case 16:
    case 17:
    case 18:
    case 19:
      bigfatalstate++;
      break;
    case 20:
      StartShape(glob, XtDisplay(glob->Toplevel1), otherplayer, &glob->shape1);
      if (glob->numscreens == 2) 
	StartShape(glob, XtDisplay(glob->Toplevel2), otherplayer, 
		   &glob->shape2);
      /* so the punchee won't be seen */
      otherplayer->x = -500;
      bigfatalstate++;
      break;
    case 21:
      MoveShape(glob, XtDisplay(glob->Toplevel1), otherplayer, &glob->shape1);
      if (glob->numscreens == 2)
	MoveShape(glob, XtDisplay(glob->Toplevel2), otherplayer, &glob->shape2);
      break;
    }
    break;
  }	/* end switch */
}
