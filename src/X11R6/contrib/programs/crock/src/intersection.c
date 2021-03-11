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

void PlayerIntersection(glob, player)
Glob    *glob;
Player  *player;
{

  Player  *otherplayer;
  RImage  *playerframe, *otherplayerframe;
  coord    defense, attack;
  coord    defenses[3];
  int      i, localx, otherlocalx, localy, otherlocaly;

  /* otherplayer will be whatever "player" is not (i.e. player1 or player2) */
  otherplayer = (player == &glob->player1)?&glob->player2:&glob->player1;

  playerframe = &player->Images1[player->moves[player->sequence].
				frameindex[player->seqnum]];

  otherplayerframe = &otherplayer->Images1[otherplayer->
	   moves[otherplayer->sequence].frameindex[otherplayer->seqnum]];

  /* check if player's current frame is an attacking move, return if not */
  if (player->frozen || playerframe->attack.x1 == -1) {
    return;
  }

  defenses[0] = otherplayerframe->high;
  defenses[1] = otherplayerframe->middle;
  defenses[2] = otherplayerframe->low;
  attack      = playerframe->attack;

  /* calculate player position (position + offset of image) */
  localx = player->x + player->Images1[player->moves[player->sequence].
				       frameindex[player->seqnum]].offsetx;
  localy = player->y + player->Images1[player->moves[player->sequence].
				       frameindex[player->seqnum]].offsety;

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
    if (MAX(attack.y1 + localy, attack.y2 + localy) <= 
	MIN(defense.y1 + otherlocaly, defense.y2 + otherlocaly) ||
	MIN(attack.y1 + localy, attack.y2 + localy) >= 
	MAX(defense.y1 + otherlocaly, defense.y2 + otherlocaly))  {
      /* either too high, low or on wrong side of boundary */
      continue;
    }
    
    if (player->facing == RIGHT) {
      if(MAX(attack.x1 + localx, attack.x2 + localx) <= 
	 MIN(defense.x1 + otherlocalx, defense.x2 + otherlocalx)) {
	continue;
      }	/* end if MIN */
    } else {
      if (MIN(attack.x1 + localx, attack.x2 + localx) >= 
	  MAX(defense.x1 + otherlocalx, defense.x2 + otherlocalx)) {
	continue;
      }	/* end if MAX */
    }	/* end if-else RIGHT */

    /* it's a hit */

    if (otherplayer->sequence == Findmove("block", otherplayer)) {
      /* he's blocking.  Min. damage and pause attacker */
      otherplayer->strength  -= player->moves[player->sequence].damage / 4;
      if (otherplayer->strength < 0) otherplayer->strength = 0;
      if (otherplayer->strength <= 0 && glob->gamestate == DURING) {
	/* that was the final blow */
	player->status = WIN;
	otherplayer->status = LOSE;
      } else {
	/* otherwise pause the attacker (because move was blocked) */
	player->sleep += 5;
	player->frozen = 1;

	/* this should be in a special file, but for the moment ... */
	if (player->sequence == Findmove("flykick", player)) {
	  /* for a flying kick, when it hits, pop it to the almost 
	     end of seq. */
	  player->seqnum = player->moves[player->sequence].maxindex - 3;
	}
      }
      /* get out of for loop, no need to check other cases */
      break;
    } else {

      /* this should be in a special file, but for the moment ... */
      if (player->sequence == Findmove("flykick", player)) {
	/* for a flying kick, when it hits, pop it to the almost end of seq. */
	player->seqnum = player->moves[player->sequence].maxindex - 3;
      }

      /* update the last hit queue */
      otherplayer->lasthit[otherplayer->lmindex++ % 3] = player->sequence;

      /* if we're hitting with the same move the last 3 times, 
	 then make it do no damage */
      if (otherplayer->lasthit[0] == otherplayer->lasthit[1] &&
	  otherplayer->lasthit[1] == otherplayer->lasthit[2]) {
	player->frozen = 1; 
	return;
      }	/* end if player->lastmoves */

      /* normal hit, normal damage, unfreeze attacker and get hit */
      switch (i) {
      case 0:
	/* high hit */
	otherplayer->sequence   = Findmove("highhit", otherplayer);
	player->attacks.highhit++;
	Playsound(glob, "highhit", otherplayer);
	break;
      case 1:
	/* mid hit */
	otherplayer->sequence   = Findmove("midhit", otherplayer);
	player->attacks.mediumhit++;
	Playsound(glob, "midhit", otherplayer);
	break;
      case 2:
	/* low hit */
	otherplayer->sequence   = Findmove("trip", otherplayer);
	player->attacks.lowhit++;
	Playsound(glob, "trip", otherplayer);
	break;
      }
      if (otherplayer->sequence == -1 ) {
	otherplayer->sequence   = Findmove("bighit", otherplayer);
	Playsound(glob, "bighit", otherplayer);
      }
      otherplayer->seqnum     = 0;
      otherplayer->locked     = 1;
      otherplayer->strength  -= player->moves[player->sequence].damage;
      if (otherplayer->strength < 0) otherplayer->strength = 0;
      otherplayer->sleep      = otherplayer->moves[otherplayer->sequence].
	framedelay[otherplayer->seqnum];
      /* set  x and y offsets */
      otherplayer->xoff = player->moves[player->sequence].xoff;
      otherplayer->yoff = player->moves[player->sequence].yoff;
      player->locked         = 0;
      break;
    }		/* end if (other sequence == block) */
  }		/* end for */
}
