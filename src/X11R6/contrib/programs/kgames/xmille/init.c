# include	"mille.h"

/*
 * @(#)init.c	1.1 (Berkeley) 4/1/82
 */

init() {

	reg PLAY	*pp;
	reg int		i, j;
	reg CARD	card;

	for (j = 0; j < C_RIGHT_WAY; j++)
		Numseen[j] = 0;
	Numgos = 0;

	for (i = 0; i < 2; i++) {
		pp = &Player[i];
		pp->hand[0] = C_INIT;
		for (j = 0; j < NUM_SAFE; j++) {
			pp->safety[j] = S_UNKNOWN;
			pp->coups[j] = FALSE;
		}
		for (j = 1; j < HAND_SZ; j++) {
			pp->hand[j] = *--Topcard;
			if (i == COMP) {
				account(card = *Topcard);
				if (issafety(card))
					pp->safety[card - S_CONV] = S_IN_HAND;
			}
		}
		pp->mileage = 0;
		pp->hand_tot = 0;
		pp->safescore = 0;
		pp->coupscore = 0;
		pp->can_go = FALSE;
		pp->speed = C_INIT;
		pp->battle = C_INIT;
		pp->new_speed = FALSE;
		pp->new_battle = FALSE;
		for (j = 0; j < NUM_MILES; j++)
			pp->nummiles[j] = 0;
	}
	if (Order)
		sort(Player[PLAYER].hand);
	Discard = C_INIT;
	Finished = FALSE;
	End = 700;
}

shuffle() {

	reg int		i, r;
	reg CARD	temp;

	for (i = 0; i < DECK_SZ; i++) {
		r = roll(1, DECK_SZ) - 1;
		if (r < 0 || r > DECK_SZ - 1) {
			fprintf(stderr, "shuffle: card no. error: %d\n", r);
			die();
		}
		temp = Deck[r];
		Deck[r] = Deck[i];
		Deck[i] = temp;
	}
	Topcard = &Deck[DECK_SZ];
}
