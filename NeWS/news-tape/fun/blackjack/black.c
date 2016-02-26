#include <black.h>

#define	PLAYER	0
#define	SPLIT	1
#define	DEALER	2

#define	UP	1
#define	DOWN	0

#define	SUIT(c)		(((c) & 0xF0) >> 4)
#define	VALUE(c)	((((c) & 0x0F) > 10) ? 10 : ((c) & 0x0F))

int	deck[52];
int	hand[3][52];
int	count[3];
int	points[3];

int	play_chips = 500;
int	deck_card_count = 0;
int	deal_down = -1;
int	split = 0;
int	wager;

main(argc, argv)
int	argc;
char	**argv;
{
	char	cwager[99];
	char	buff[99];
	long	t;
	int	playing = 1;
	int	tag;
	int	dummy;

	ps_open_PostScript();
	time(&t);
	srandom(getpid() + (int)((t >>16) +t));
	ps_init();
	ps_flush_PostScript();
	init_deck();
	shuffle();	/* shuffle the cards */
	wager = 1;
	ps_set_wager(wager);

	while(playing)
	{
		count[PLAYER] = 0;
		count[SPLIT] = 0;
		count[DEALER] = 0;
		deal_down = -1;
		split = 0;
		ps_message("");
		ps_hit_ok();
		ps_stand_ok();
		ps_wager_ok();
		ps_split_no();
		ps_newgame_no();

		card(PLAYER, UP);
		card(DEALER, DOWN);
		deal_down = hand[DEALER][0];
		card(PLAYER, UP);
		card(DEALER, UP);
		while(1)
		{
			if ((hand[PLAYER][0] & 0x0F) == (hand[PLAYER][1] & 0x0F))
				ps_split_ok();
			ps_flush_PostScript();
			if (ps_hit())
			{
				ps_wager_no();
				ps_split_no();
				switch (split)
				{
					case 0 :	/* no split */
						card(PLAYER, UP);
						if (points[PLAYER] > 21)
						{
							calc_winner();
						}
						break;
					case 1 :	/* first of split hands */
						card(PLAYER, UP);
						if (points[PLAYER] > 21)
						{
							ps_message("Play second hand");
							ps_split_second();
							split++;
						}
						break;
					case 2 :	/* second of split hands */
						card(SPLIT, UP);
						if (points[SPLIT] > 21 && points[PLAYER] > 21)
						{
							calc_winner();
						}
						else if (points[SPLIT] > 21)
						{
							ps_hit_no();
							ps_wager_no();
							ps_flip_card(deal_down & 0x0F, SUIT(deal_down));
							deal_down = -1;
							while (points[DEALER] <= 16)
								card(DEALER, UP);
							calc_winner();
						}
						break;
				}
			}
			if (ps_split())
			{
				split = 1;
				count[PLAYER] = 1;
				count[SPLIT] = 1;
				hand[SPLIT][0] = hand[PLAYER][1];
				ps_clear_cards();
				redraw_cards();
				ps_split_first();
				ps_split_no();
			}
			if (ps_stand())
			{
				ps_split_no();
				if (split == 1)
				{
					ps_split_second();
					split++;
				}
				else
				{
					ps_hit_no();
					ps_wager_no();
					ps_flip_card(deal_down & 0x0F, SUIT(deal_down));
					deal_down = -1;
					while (points[DEALER] <= 16)
						card(DEALER, UP);
					calc_winner();
				}
			}
			if (ps_wager(cwager))
			{
				wager = atoi(cwager);
			}
			if (ps_paint())
			{
				redraw_cards();
			}
			if (ps_newgame())
			{
				ps_hit_ok();
				ps_stand_ok();
				ps_newgame_no();
				ps_split_no();
				ps_clear_cards();
				break;
			}
			else if (ps_quit())
			{
				sprintf(buff, "You leave with %d dollars", play_chips);
				ps_message(buff);
				ps_flush_PostScript();
				playing = 0;
				break;
			}
		}
	}
	ps_close_PostScript();
	exit(0);
}

card(who, up)
int	who, up;
{
	int	c, i;
	int	ace = 0;

	if (deck_card_count > 51)
		shuffle();
	c = deck[deck_card_count++];
	hand[who][count[who]] = c;

	points[who] = 0;
	for (i = 0; i <= count[who]; i++)
	{
		if (VALUE(hand[who][i]) == 1)
			ace++;
		points[who] += VALUE(hand[who][i]);
	}
	for (i = 0; i < ace; i++)
		if (points[who] < 12)
			points[who] += 10;

	ps_show_card(who, up, count[who], SUIT(c), (c & 0x0F), split);
	count[who]++;
}

redraw_cards()
{
	int	i;

	if (split == 0)
	{
		for(i = 0; i < count[PLAYER]; i++)
			ps_show_card(PLAYER, UP, i, SUIT(hand[PLAYER][i]), (hand[PLAYER][i]) & 0x0F, 0);
	}
	else
	{
		for(i = 0; i < count[PLAYER]; i++)
			ps_show_card(PLAYER, UP, i, SUIT(hand[PLAYER][i]), (hand[PLAYER][i]) & 0x0F, 1);
		for(i = 0; i < count[SPLIT]; i++)
			ps_show_card(SPLIT, UP, i, SUIT(hand[SPLIT][i]), (hand[SPLIT][i]) & 0x0F, 2);
	}
	if (deal_down != -1)
		ps_show_card(DEALER, DOWN, 0, SUIT(hand[DEALER][0]), (hand[DEALER][0]) & 0x0F, 0);
	else
		ps_show_card(DEALER, UP, 0, SUIT(hand[DEALER][0]), (hand[DEALER][0]) & 0x0F, 0);
	for(i = 1; i < count[DEALER]; i++)
		ps_show_card(DEALER, UP, i, SUIT(hand[DEALER][i]), (hand[DEALER][i]) & 0x0F, 0);
}

init_deck()
{
	int	i, j;
	int	n = 0;

	for (i=1; i < 5; i++)
		for (j = 1; j < 14; j++)
		{
			deck[n++] = ((i & 0x0F) << 4) | (j & 0x0F);
		}
}

shuffle()
{
	int	i, j, c;
	long	random();

	for(i=52-1;i>=0;i--)
	{
		j = random() % 52;
		if (i != j)
		{
			c = deck[i];
			deck[i] = deck[j];
			deck[j] = c;
		}
	}
	deck_card_count = 0;
}

calc_winner()
{
	int	res = 0, res2 = 0;

	if (points[PLAYER] > 21)
		res = -1;
	else if (points[DEALER] > 21)
		res = 1;
	else if (points[PLAYER] > points[DEALER])
		res = 1;
	else if (points[PLAYER] == points[DEALER])
		res = 0;
	else
		res = -1;
	if(split != 0)
	{
		if (points[SPLIT] > 21)
			res2 = -1;
		else if (points[DEALER] > 21)
			res2 = 1;
		else if (points[SPLIT] > points[DEALER])
			res2 = 1;
		else if (points[SPLIT] == points[DEALER])
			res2 = 0;
		else
			res2 = -1;
		if (res == 0)
		{
			if (res2 == 0)
				ps_message("Both hands push");
			else if (res2 == -1)
				ps_message("One push, one loss");
			else if (res2 == 1)
				ps_message("One push, one win");
		}
		else if (res == 1)
		{
			if (res2 == 0)
				ps_message("One win, one push");
			else if (res2 == -1)
				ps_message("One win, one loss");
			else
				ps_message("Two wins");
		}
		else
		{
			if (res2 == 0)
				ps_message("One loss, one push");
			else if (res2 == -1)
				ps_message("Two losses");
			else
				ps_message("One loss, one win");
		}
	}
	else
	{
		if (res == 0)
			ps_message("Push");
		else if (res == 1)
			ps_message("You win");
		else
			ps_message("Dealer wins");
	}
	play_chips = play_chips + wager * (res + res2);
	ps_set_chips(play_chips);
	ps_newgame_ok();
	ps_hit_no();
	ps_stand_no();
}
