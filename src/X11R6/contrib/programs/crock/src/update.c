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

/*
 * routines to update various things like the game state, 
 * objects, players, and such.
 */

#include "types.h"
#include "flags.h"

void
UpdatePlayerState(glob, player, otherplayer)
Glob *glob;
Player *player, *otherplayer;

{
  int deltax;

  /* we compute velocity regardless of whether they're asleep or not */
  /* compute offset from current veleocity */
  /* remember, positive velocity (xoff) means they're going backwards */
  if (player->xoff != 0) {
    if (player->facing == LEFT) {
      if (leftedge(glob, player, player->xoff) && 
	  rightedge(glob, player, player->xoff)) {
	player->x += player->xoff;
      } 
    } else {
      if (leftedge(glob, player, -player->xoff) &&
	  rightedge(glob, player, -player->xoff)) {
	player->x -= player->xoff;
      } 
    }
  }

  /* we should rework this...it's not terribly efficient, but...*/
  if (player->y == 0) {
    /* you're on the ground, so x motion will slow to a stop */
    player->xoff -= DRAG;
    if (player->xoff < 0) {
      player->xoff = 0;
    }
  } 

  player->y -= player->yoff;
  player->yoff -= GRAVITY;
  if (player->y > 0) {
    player->y = 0;
    player->yoff = 0;
  }

  /* handle special moves */
  if (player->moves[player->sequence].flags) {
    MoveSpecialMove(glob, player);
  }

  if (player->sleep == 0) {
    /* it's our turn */
    
    /* 
     * note that we should only be moving when we're in "moveleft" 
     * or "moveright".
     */
    if (player->sequence == Findmove ("moveleft", player)) {
      /* we're supposed to move left */
      deltax = -player->stepsize;
      if (leftedge(glob, player, deltax) && 
	  ((player->facing == LEFT && player->x + deltax >= otherplayer->x) ||
	   (player->facing == RIGHT && player->x + deltax<= otherplayer->x))) {
	player->x += deltax;
      }
    } 	/* end if  sequence == moveleft */
    else if (player->sequence == Findmove ("moveright", player)) {
      /* we're supposed to move right */
      deltax = player->stepsize;
      if (rightedge(glob, player, deltax) &&
	  ((player->facing == LEFT && player->x + deltax >= otherplayer->x) ||
	   (player->facing == RIGHT && player->x + deltax <= otherplayer->x))){
	player->x += deltax;
      } 
    }	/* end else if sequence == moveright */

    
    player->seqnum++;
    if (player->seqnum >= player->moves[player->sequence].maxindex) {
      /* we're at the end of that sequence , check what's next */
      if (player->sequence == Findmove ("win", player)) {
	/* end of win sequence, game over dude */
	glob->gamestate = AFTER;
	player->seqnum--;
      }	/* end if player->sequence == WIN */
      else if (player->status == WIN && glob->gamestate == POSE) {
	/* pose for the victory shot */
	player->seqnum   = 0;
	player->sequence = Findmove ("win", player);
	Playsound(glob, "win", player);
	player->locked   = 1;
      } 	/* end if WIN  and POSE */
      else if (player->status == LOSE) {
	if (glob->gamestate == END) {
	  /* player just got killed, make stunned */
	  player->seqnum   = 0;
	  player->sequence = Findmove ("stun", player);
	  player->locked   = 1;
	  player->strength = 0;
	} else if (glob->gamestate == POSE) {
	  /* player just got killed, make him act like it */
	  /* as long as he's not already knocked down */
	  if (player->moves[player->sequence].flags != ONGROUND) {
	    player->seqnum   = 0;
	    player->sequence = Findmove ("lose", player);
	    Playsound(glob, "lose", player);
	    player->locked   = 1;
	    player->strength = 0;
	  } else {
	    /* just keep him on the ground in the last pose */
	    player->seqnum--;
	  }
	}	/* end if POSE */
      }	/* end else if LOSE */
      else if (player->sequence == Findmove("trip", player) ||
	       player->sequence == Findmove("bighit", player)) {
	/* they just got knocked down, make them get back up */
	player->seqnum   = 0;
	player->sequence = Findmove("getup", player);
      } else {
	/* set sequence back to default */
	player->seqnum   = 0;
	player->sequence = 0;
	player->frozen   = 0;		/* no longer helpless */
	if (glob->gamestate == DURING || glob->gamestate == END) {
	  /* only unlock them during a fight or at the end */
	  player->locked   = 0;
	}	/* end if glob->gamestate */
      }	/* end else (player->nextmove) */
    }	/* end if seqnum > maxindex */
    
    /* reset the sleep for the new image */
    player->sleep = player->moves[player->sequence].
      framedelay[player->seqnum];
    /* reset the frozen attribute (if they are iced) */
    if (player->frozen == 2) {
      player->frozen--;
    }	/* end if player->frozen */
  }	/* end if player->sleep */

  /* gore, man, gore */
  if (glob->gamemode != GOOD && 
      player->sequence == Findmove("skeleton", player)) {
    MakeBigBlood(glob, player);
    MakeBigBlood(glob, player);
    MakeBigBlood(glob, player);
  }	/* end if GOOD and skeleton */
}

void ProjectileHit(glob, shot, player)
Glob   *glob;
Listnode *shot;
Player *player;
{
  Player *otherplayer;
  int localx, localy;

  /* otherplayer will be whatever "player" is not (i.e. player1 or player2) */
  otherplayer = (player == &glob->player1)?&glob->player2:&glob->player1;

  /* it's a hit */
  if (otherplayer->sequence == Findmove("block", otherplayer)) {
    /* he's blocking (no penalty to attacker).  Min. damage */
    otherplayer->strength  -= 
      shot->object.moves[shot->object.sequence].damage / 4;
    if (otherplayer->strength < 0) otherplayer->strength = 0;
    if (otherplayer->strength <= 0 && glob->gamestate == DURING) {
      /* that was the final blow */
      player->status = WIN;
      otherplayer->status = LOSE;
    }
  } else {
    /* check if projectile is a freeze */
    if (shot->object.moves[shot->object.sequence].flags == FREEZE) {
      if (otherplayer->frozen == 2) {
	/* double ice backfire! */
	otherplayer->frozen = 0;
	otherplayer->locked = 0;
	otherplayer->sleep = 1;
	player->frozen = 2;
	player->locked = 1;
	player->sleep = FREEZESLEEP;
      } else {
	/* stop right there... */
	otherplayer->sleep = FREEZESLEEP;
	otherplayer->frozen = 2;
	otherplayer->locked = 1;
      }
    } else {
      /* normal hit, normal damage, unfreeze attacker and get hit */
      Playsound(glob, "bighit", otherplayer);
      MakeBlood(glob, otherplayer);
      MakeBlood(glob, otherplayer);
      MakeBlood(glob, otherplayer);
      MakeBlood(glob, otherplayer);
      MakeBlood(glob, otherplayer);
      otherplayer->sequence   = Findmove("bighit", otherplayer);
      player->attacks.bighit++;
      otherplayer->seqnum     = 0;
      otherplayer->locked     = 1;
      otherplayer->strength  -= 
	shot->object.moves[shot->object.sequence].damage;
      if (otherplayer->strength < 0) otherplayer->strength = 0;
      otherplayer->sleep      = otherplayer->moves[otherplayer->sequence].
	framedelay[otherplayer->seqnum];
      /* set  x and y offsets */
      otherplayer->xoff = shot->object.moves[shot->object.sequence].xoff;
      otherplayer->yoff = shot->object.moves[shot->object.sequence].yoff;
      player->locked         = 0;
    }		/* end else if FREEZE */
  }		/* end if (other sequence == block) */
}		/* end function ProjectileHit() */


/*
 * intersect a projectile with a player.  This probably
 * should be folded into PlayerIntersection() but there's
 * enough happening here that's (initially) conceptually
 * different, that I'm opting to keep them separate.
 */
int ProjectileIntersection(glob, shot, otherplayer) 
Glob   *glob;
Listnode *shot;
Player *otherplayer;
{

  RImage  *shotframe, *otherplayerframe;
  coord    defense, attack;
  coord    defenses[3];
  int      i, shotx, shoty, otherlocalx, otherlocaly;
  double   m1, m2;

  shotframe = &shot->object.Images1[shot->object.moves[shot->object.sequence].
				frameindex[shot->object.seqnum]];

  otherplayerframe = &otherplayer->Images1[otherplayer->
	   moves[otherplayer->sequence].frameindex[otherplayer->seqnum]];

  defenses[0] = otherplayerframe->high;
  defenses[1] = otherplayerframe->middle;
  defenses[2] = otherplayerframe->low;
  attack      = shotframe->attack;

  shotx       = shot->object.x +
                shot->object.Images1[shot->object.moves[shot->object.sequence].
				frameindex[shot->object.seqnum]].offsetx;

  shoty       = shot->object.y +
                shot->object.Images1[shot->object.moves[shot->object.sequence].
				frameindex[shot->object.seqnum]].offsety;

  /* calculate otherplayer position (position + offset of image) */
  otherlocalx = otherplayer->x + otherplayer->
                Images1[otherplayer->moves[otherplayer->sequence].
			frameindex[otherplayer->seqnum]].offsetx;
  otherlocaly = otherplayer->y + otherplayer->
                Images1[otherplayer->moves[otherplayer->sequence].
			frameindex[otherplayer->seqnum]].offsety;

  /* check these against high, middle and low zones */
  for (i = 0; i < 3; i++) {
    defense = defenses[i];

    /* skip if this zone has no vulnerable region */
    if (defense.x1 == -1) {
      continue;
    }
    /* filter out the definitely not intersecting cases */
    if (MAX(attack.y1 + shoty, attack.y2 + shoty) <= 
	MIN(defense.y1 + otherlocaly, defense.y2 + otherlocaly) ||
	MIN(attack.y1 + shoty, attack.y2 + shoty) >= 
	MAX(defense.y1 + otherlocaly, defense.y2 + otherlocaly))  {
      /* either too high, low or on wrong side of boundary */
      continue;
    }
    
    if (otherplayer->facing == LEFT) {
      if(MAX(attack.x1 + shotx, attack.x2 + shotx) <= 
	 MIN(defense.x1 + otherlocalx, defense.x2 + otherlocalx)) {
	continue;
      }	/* end if MIN */
      if (attack.x1 + shotx > 
	  defense.x1 + otherlocalx + otherplayer->width/2) {
	/* skip if shot passes by player */
	continue;
      }
    } else {
      if (MIN(attack.x1 + shotx, attack.x2 + shotx) >= 
	  MAX(defense.x1 + otherlocalx, defense.x2 + otherlocalx)) {
	continue;
      }	/* end if MAX */
      if (attack.x1 + shotx < 
	  defense.x1 + otherlocalx - otherplayer->width/2) {
	/* skip if shot passes by player */
	continue;
      }
    }	/* end if-else RIGHT */

    /* it's a hit */
    return 1;
  }	/* end for */
  return 0;
}

/* 
 * deal with objects
 */
void
UpdateObjects (glob, top, position)
Glob *glob;
Listnode **top;
int   position;
{
  Listnode *temp;
  Player   *p;

  for (temp = *top; temp != NULL; temp = temp->next) {
    if (position != ANYWHERE && position != temp->object.z) {
      /* if we care about the position, skip entries that don't match */
      continue;
    }
    /* update it's x and y values */
    temp->object.x += temp->object.xoff;
    temp->object.y -= temp->object.yoff;
    if (temp->object.gravity) {
      temp->object.yoff -= GRAVITY;
    }
    if (temp->object.y >= 0) {
      temp->object.y = 0;
      temp->object.yoff = 0;
    }
    if (temp->object.y == 0) {
      /* you're on the ground, so x motion will slow to a stop */
      if (abs(temp->object.xoff) < (2*DRAG)) {
	temp->object.xoff = 0;
      } else {
	temp->object.xoff -= (2*DRAG) * temp->object.xoff > 0?1:-1;
      }
    } 

    switch (temp->object.objecttype) {
    case HEART:
      /* if it's a heart, it's a special case and should follow the 
	 winner's hand */
      p = (glob->player1.status == WIN)? &glob->player1: &glob->player2;
      temp->object.x = p->Images1[p->moves[p->sequence].
				  frameindex[p->seqnum]].attack.x1 +  p->x;
      temp->object.y = p->Images1[p->moves[p->sequence].
				  frameindex[p->seqnum]].attack.y1 + p->y;
      break;
    case WEIGHT:
      /* if it's a 16 ton weight, it's a special case and should get
	 rid of the player it effects, eventually */
      if (temp->object.effects) {
	/* hasn't squished anyone yet */
	p = (temp->object.effects == 1) ? &glob->player1 : &glob->player2;
	if (temp->object.y == 0) {
	  /* it's on the ground */
	  MakeBlood(glob,p);
	  MakeBlood(glob,p);
	  MakeBlood(glob,p);
	  p->x = -500;
	  temp->object.effects = 0;
          /* make the other player bounce a bit */
	  p = (temp->object.effects == 1) ? &glob->player2 : &glob->player1;
          p->yoff += 20;
	  glob->shake++;
	}
      }
    break;
    case HEAD:
      /* draw a line connecting the head */
      if (glob->player1.status == LOSE) {
	p = &glob->player1;
      } else {
	p = &glob->player2;
      }
      temp->object.x = p->x + random() % 8 - 4;
      if (temp->object.y < -20) {
	temp->object.gravity = 1;
      } else if (temp->object.y == 0) {
	temp->object.yoff = 25;
	temp->object.gravity = 0;
      }

      /* draw the spring connecting the head  to the body */
      SpringZap(glob, p, temp);
      break;
    case BODY:
      if (strncmp(glob->currentbg->name, "Fire Sword Shrine", 17) == 0) {
	/* these things only work on the Fire Sword Level */
	if (temp->object.y > -280 && temp->object.gravity) {
	  p = (glob->player1.status == LOSE)? &glob->player1: &glob->player2;
	  /* come to an abrupt halt on the sword */
	  temp->object.gravity = 0;
	  temp->object.yoff = 0;
	  MakeBloodAt(glob, p, temp->object.x, temp->object.y, 30, 60);
	  MakeBloodAt(glob, p, temp->object.x, temp->object.y, 30, 60);
	  MakeBloodAt(glob, p, temp->object.x, temp->object.y, 30, 60);
	  MakeBloodAt(glob, p, temp->object.x, temp->object.y, 30, 60);
	  MakeBloodAt(glob, p, temp->object.x, temp->object.y, 30, 60);
	  MakeBloodAt(glob, p, temp->object.x, temp->object.y, 30, 60);
	}
	if (temp->object.y > -280 && temp->object.y < -200) {
	  temp->object.y++;
	}
      }
      break;
    }	/* end switch */
    
    
    if (temp->object.effects) {
      /* test for intersection with other player */
      if (temp->object.effects & 1) {
	/* it effects player 1 */
	if (ProjectileIntersection(glob, temp, &glob->player1)) {
	  ProjectileHit(glob, temp, &glob->player2); 
	  temp->object.ttl = 0;
	}
      }
      if (temp->object.effects & 2) {
	/* it effects player 2 */
	if (ProjectileIntersection(glob, temp, &glob->player2)) {
	  ProjectileHit(glob, temp, &glob->player1);
	  temp->object.ttl = 0;
	}	/* if ProjInter() */
      }		/* if object.effects & 2*/
    }		/* if object.effects */

    /* foreach object, decrement ttl and delete it if it's < 1 
       or if it's offscreen */
    if (--temp->object.ttl < 1 || 
	  temp->object.x   < -100 || 
	  temp->object.x > (int) glob->playwidth + 100) {
      DeleteNode(top, temp);
      return;
    } 

    /* update sequence */
    if (--temp->object.sleep == 0) {
      if (++temp->object.seqnum >= 
	  temp->object.moves[temp->object.sequence].maxindex) {
	/* if we're at the end of it's sequence, reset it to the first frame */
	temp->object.seqnum = 0;
      }
      temp->object.sleep = temp->object.moves[temp->object.sequence].
	                         framedelay[temp->object.seqnum];
    }

    /* draw it */
    DrawIt(glob, glob->Toplevel1, temp->object.gc1, glob->temp1,
	   temp->object.Images1[temp->object.moves[temp->object.sequence].
				frameindex[temp->object.seqnum]].pm,
	   temp->object.Images1[temp->object.moves[temp->object.sequence].
				frameindex[temp->object.seqnum]].mask,
	   temp->object.x + temp->object.Images1[temp->object.
				moves[temp->object.sequence].
				frameindex[temp->object.seqnum]].offsetx,
	   temp->object.y + temp->object.Images1[temp->object.
				moves[temp->object.sequence].
				frameindex[temp->object.seqnum]].offsety,
	   temp->object.Images1[temp->object.moves[temp->object.sequence].
				frameindex[temp->object.seqnum]].width,
	   temp->object.Images1[temp->object.moves[temp->object.sequence].
				frameindex[temp->object.seqnum]].height,
	   0, 0);
    if (glob->numscreens == 2) {
      DrawIt(glob, glob->Toplevel2, temp->object.gc2, glob->temp2,
	     temp->object.Images2[temp->object.moves[temp->object.sequence].
				  frameindex[temp->object.seqnum]].pm,
	     temp->object.Images2[temp->object.moves[temp->object.sequence].
				  frameindex[temp->object.seqnum]].mask,
	     temp->object.x + temp->object.Images1[temp->object.
				   moves[temp->object.sequence].
				   frameindex[temp->object.seqnum]].offsetx,
	     temp->object.y + temp->object.Images1[temp->object.
				   moves[temp->object.sequence].
				   frameindex[temp->object.seqnum]].offsety,
	     temp->object.Images2[temp->object.moves[temp->object.sequence].
				  frameindex[temp->object.seqnum]].width,
	     temp->object.Images2[temp->object.moves[temp->object.sequence].
				  frameindex[temp->object.seqnum]].height, 
	     0, 0);
    }
  }
}

static firstpose, firstfight, firstend, firstduring;

/*
 * deal with switching from different states (before, during and after fight).
 */
void
UpdateGameState (glob)
Glob *glob;
{

  static char roundtext[2] = " ";
  static char rumortext[200];

  /* handle details of switching the game state */
  switch (glob->gamestate) {
  case FIGHT:
    /* draw the word (bitmap) "Fight" at the top */
    DrawText(glob, 50, 50, "FIGHT!", 0);
    if (glob->gametimer == 0) {
      /* let them loose */
      glob->player1.locked = 0;
      glob->player2.locked = 0;
      glob->gamestate = DURING;
      glob->gametimer = DURINGTIME;
    }
    break;
  case BEFORE:
    /* set up later variables */
    firstpose = 1; firstfight = 1; firstend = 1; firstduring = 0;

    /* "charge" up the health levels */
    if (glob->player1.strength != MAXSTRENGTH) {
      glob->player1.strength += MAXSTRENGTH/BEFORETIME;
    }
    if (glob->player2.strength != MAXSTRENGTH) {
      glob->player2.strength += MAXSTRENGTH/BEFORETIME;
    }
    
    DrawText(glob, 200, 50, "round", 0);
    roundtext[0] = (char) glob->round + '0';
    DrawText(glob, 220, 75, roundtext, 0);

    if (glob->gametimer == 0) {
      glob->gamestate = FIGHT;
      glob->gametimer = FIGHTTIME;
      Playglobsound(glob, glob->fight1, glob->fight2, NULL);
    }
    break;
  case DURING:
    if (glob->player1.strength <= 0 && glob->player2.strength > 0) {
      /* player 2 wins */
      glob->player2.status = WIN;
      glob->player2.frozen = 0;
      glob->player2.locked = 0;
      glob->player1.status = LOSE;
      if (++glob->player2.wins == 2) {
	/* fatalities only after 2 wins */
	glob->gamestate = END;
	glob->gametimer = ENDTIME;
	glob->round     = 0;
      } else {
	glob->gamestate = POSE;
      }
    } else if (glob->player2.strength <= 0 && glob->player1.strength > 0) {
      /* player 1 wins */
      glob->player1.status = WIN;
      glob->player1.frozen = 0;
      glob->player1.locked = 0;
      glob->player2.status = LOSE;
      if (++glob->player1.wins == 2) {
	/* fatalities only after 2 wins */
	glob->gamestate = END;
	glob->gametimer = ENDTIME;
	glob->round     = 0;
      } else {
	glob->gamestate = POSE;
      }
    } else if (glob->player1.strength <= 0 && glob->player2.strength <= 0) {
      /* draw */
      /* we don't handle this case yet */
    } else {
      /* no winner yet, both still have strength.  check for timeout */
      if (glob->gametimer == 0) {
	/* time's up */
        (void) fprintf(stderr,"%c%c%c", 7, 7, 7);
	DrawText(glob, 20, 20, "TIMES UP", 0);
	DrawText(glob, 50, 250, "(wimps)", 0);
	
	if (glob->player1.strength > glob->player2.strength) {
	  /* player 1 wins by having more strength left */
	  DrawText(glob, 50, 300, "player1 wins (by decision)", 0);
	  glob->player1.status = WIN;
	  glob->player2.status = LOSE;
	} else if (glob->player2.strength > glob->player1.strength) {
	  /* player 2 wins by having more strength left */
	  DrawText(glob, 250, 300, "player2 wins (by decision)", 0);
	  glob->player2.status = WIN;
	  glob->player1.status = LOSE;
	} else {
	  /* draw -- we don't handle that yet */
	  DrawText(glob, 200, 300, "draw", 0);
	}
	glob->gamestate = AFTER;
      }	/* end if (gametimer ) */
    }	/* end if glob player strength ... */
    break;
  case END:
    if (firstend) {
      if (glob->player1.status == WIN && 
	  !(glob->player1.attacks.highhit |
	    glob->player1.attacks.mediumhit |
	    glob->player1.attacks.lowhit)) {
	/* player 1 won only by projectiles, not nice */
	glob->player1.status = LOSE;
      } else if (glob->player2.status == WIN && 
	  !(glob->player2.attacks.highhit |
	    glob->player2.attacks.mediumhit |
	    glob->player2.attacks.lowhit)) {
	/* player 2 won only by projectiles, not nice */
	glob->player2.status = LOSE;
      } else {
	Playglobsound(glob, glob->finish1, glob->finish2, NULL);
      }
      firstend = 0;
    }
    if (glob->dimbackground) {
      /* make it get darker */
      if (glob->dimbackground != MAXDIM) {
	glob->dimbackground *= 2;
      }
    } else {
      if (glob->player1.status == LOSE && glob->player2.status == LOSE) {
	/* draw the word (bitmap) "Finish Him" at the top */
	DrawText(glob, 100, 50, "HONORLESS VICTORY!!!", 0);
	DrawText(glob, 100, 250, "BEERHUNTER IS ANGERED!!!", 0);
	if (glob->gametimer == 0) {
	  glob->gamestate = AFTER;
	  glob->gametimer++;
	}
      } else {
	/* draw the word (bitmap) "Finish Him" at the top */
	DrawText(glob, 100, 50, "FINISH", 0);
	DrawText(glob, 100, 250, "HIM!", 0);
      }
    }
    if (glob->gametimer == 0) {
      glob->gamestate = POSE;
    }
    break;
  case POSE:
    if (glob->player1.status == WIN) {
      DrawText(glob, 50, 150, "player1 wins", 0);
      if (firstpose) {
	if (glob->player1.strength == MAXSTRENGTH) {
	  if (glob->infatality) {
	    Playglobsound(glob, glob->p1wins1, glob->p1wins2,
			  glob->flawless1, glob->flawless2,
			  glob->fatality1, glob->fatality2, 
			  NULL);
	  } else {
	    Playglobsound(glob, glob->p1wins1, glob->p1wins2, 
			  glob->flawless1, glob->flawless2,
			  NULL);
	  } 
	} else {
	  if (glob->infatality) {
	    Playglobsound(glob, glob->p1wins1, glob->p1wins2,
			  glob->fatality1, glob->fatality2, NULL);
	  } else {
	    Playglobsound(glob, glob->p1wins1, glob->p1wins2, NULL);
	  }
	}
	GetRumor(glob, rumortext);
	firstpose = 0;
      }
    } else if (glob->player2.status == WIN) {
      DrawText(glob, 350, 150, "player2 wins", 0);
      if (firstpose) {
	if (glob->player2.strength == MAXSTRENGTH) {
	  if (glob->infatality) {
	    Playglobsound(glob, glob->p2wins1, glob->p2wins2,
			  glob->flawless1, glob->flawless2,
			  glob->fatality1, glob->fatality2, NULL);
	  } else {
	    Playglobsound(glob, glob->p2wins1, glob->p2wins2, 
			  glob->flawless1, glob->flawless2,
			  NULL);
	  } 
	} else {
	  if (glob->infatality) {
	    Playglobsound(glob, glob->p2wins1, glob->p2wins2,
			  glob->fatality1, glob->fatality2, NULL);
	  } else {
	    Playglobsound(glob, glob->p2wins1, glob->p2wins2, NULL);
	  }
	}
	GetRumor(glob,rumortext);
	firstpose = 0;
      }
    }
    if (glob->player1.wins == 2 || glob->player2.wins == 2) {
      /* only print a rumor if it's the end of a match */
      DrawText(glob, 50, 340, rumortext, 2);
    }
    switch(glob->infatality) {
    case 0:
      DrawText(glob, 200, 200, "by k.o.", 0);
      break;
    case 1:
      DrawText(glob, 200, 200, "by fatality", 1);
      break;
    case 2:
      DrawText(glob, 200, 200, "by double fatality", 1);
      break;
    case 3:
      DrawText(glob, 200, 200, "by triple fatality", 1);
      DrawText(glob, 200, 250, "excellent!!!", 1);
      break;
    }	/* end switch glob->infatality */
  }	/* end switch glob->gamestate */

  /* handle any level specific details (if needed) */
  HandleLevel(glob);

  if (glob->shake) glob->shake++;
  glob->gametimer--;
}

void
HandleLevel(glob)
Glob *glob;
{
  static int show;

  if (strcmp(glob->currentbg->name, "The Graveyard") == 0) {
    if (glob->gamestate == POSE) {
      if ((glob->player1.status == WIN && 
	   glob->player1.strength == MAXSTRENGTH) || 
	  (glob->player2.status == WIN && 
	   glob->player2.strength == MAXSTRENGTH)) {
	if (firstpose) {
	  show = 150;
	}

	/* create the ghostly hand */
	DrawIt(glob, glob->Toplevel1, glob->blackgc1, glob->temp1,
	       glob->hand1, glob->handm1, 
	       500, 220 + show, /* x and y position */
	       68, 150, /* width and height */
	       show, 0);	/* start and end offset */
      }
      if (show > 0) show -= 30;
    }
  } else if (strcmp(glob->currentbg->name, "Fire Sword Shrine") == 0) {
    if (glob->gamestate == POSE && glob->infatality && firstpose) {
      /* scroll it if it's a fatality */
/*      glob->shake = 100; */
/*      glob->dimbackground = 0; */
    }
  }
}
