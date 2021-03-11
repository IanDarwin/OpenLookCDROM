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

/* linked list manipulation routines */

#include "types.h"

/*
 * add a node to the end of a linked list, when given the the root and new node
 */
void AddNode (top, newnode, nodesize)
Listnode **top;
Listnode  *newnode;
int        nodesize;
{
  Listnode *temp, *localtop;
  
  localtop = *top;

  /* make and initialize new node */
  if (((void *) temp = (void *) malloc (nodesize)) == NULL) {
    fprintf(stderr, "couldn't malloc new node.  Quiting!\n");
    exit(5);
  }
  temp->next = NULL;
  temp->prev = NULL;

  (void) memcpy (temp, newnode, nodesize);

  /* check for empty list */
  if (localtop == NULL) {
    /* add root */
    *top = temp;
  } else {
    /* find end of list and add node there */
    while (localtop->next != NULL) 
      localtop = localtop->next;

    localtop->next = temp;
    temp->prev     = localtop;
  }
}

void DeleteNode (top, delnode)
Listnode **top;
Listnode  *delnode;
{

  if (delnode->prev != NULL) 
    delnode->prev->next = delnode->next;
  if (delnode->next != NULL)
    delnode->next->prev = delnode->prev;

  /* handle special case if it's the first node */
  if (delnode == *top)
    *top = delnode->next;

  free (delnode);
}

void MakeBloodObject (glob, player, xspurt, yspurt)
Glob   *glob;
Player *player;
int     xspurt, yspurt;
{
  Listnode blood;
  int direction;


  /* wuss out for the kids */
  if (glob->gamemode == GOOD) return;

  /* let's make some blood */
  direction = player->facing == LEFT?-1:1;
  blood.next = NULL;
  blood.prev = NULL;
  blood.object.x = player->x + 100;
  blood.object.y = player->y - 160;
  blood.object.z = FRONT;
  blood.object.xoff = random() % xspurt * direction;
  blood.object.yoff = random() % yspurt + 20;
  blood.object.ttl = random() % 20 + 40;
  blood.object.objecttype = BLOOD;
  blood.object.gravity = 1;
  blood.object.effects = 0;
  blood.object.moves = player->moves;
  blood.object.Images1 = player->Images1;
  blood.object.Images2 = player->Images2;
  blood.object.gc1 = glob->redgc1;
  blood.object.gc2 = glob->redgc2;
  blood.object.sequence = Findmove("blood", player);
  blood.object.sleep = 5;
  blood.object.seqnum = 0;
  AddNode(&glob->objectlist, &blood, sizeof(Listnode));

}

void MakeBigBlood(glob, player) 
Glob   *glob;
Player *player;
{
  MakeBloodObject(glob, player, 20, 60);
}

void MakeBlood(glob, player) 
Glob   *glob;
Player *player;
{
  MakeBloodObject(glob, player, 20, 40);
}

/* I've coded things stupidly, I shouldn't need yet another routine */
void MakeBloodAt (glob, player, x, y, xspurt, yspurt)
Glob   *glob;
Player *player;
int     xspurt, yspurt, x, y;
{
  Listnode blood;
  int direction;

  /* wuss out for the kids */
  if (glob->gamemode == GOOD) return;

  /* let's make some blood */
  direction = player->facing == LEFT?-1:1;
  blood.next = NULL;
  blood.prev = NULL;
  blood.object.x = x + 100;
  blood.object.y = y - 160;
  blood.object.z = FRONT;
  blood.object.xoff = random() % xspurt * direction;
  blood.object.yoff = random() % yspurt + 20;
  blood.object.ttl = random() % 20 + 40;
  blood.object.objecttype = BLOOD;
  blood.object.gravity = 1;
  blood.object.effects = 0;
  blood.object.moves = player->moves;
  blood.object.Images1 = player->Images1;
  blood.object.Images2 = player->Images2;
  blood.object.gc1 = glob->redgc1;
  blood.object.gc2 = glob->redgc2;
  blood.object.sequence = Findmove("blood", player);
  blood.object.sleep = 5;
  blood.object.seqnum = 0;
  AddNode(&glob->objectlist, &blood, sizeof(Listnode));

}
