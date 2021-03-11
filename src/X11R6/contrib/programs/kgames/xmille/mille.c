# include	"mille.h"
# include	<signal.h>
# ifdef attron
#	include	<term.h>
# endif	attron

/*
 * @(#)mille.c	1.3 (Berkeley) 5/10/83
 */

int	rub();

char	_sobuf[BUFSIZ];
bool	restore;

main(ac, av)
int		ac;
reg char	*av[]; {

	restore = FALSE;
	setbuf(stdout, _sobuf);
	Play = PLAYER;
	init_ui (&ac, av);
	switch (ac) {
	  case 2:
		rest_f(av[1]);
		restore = TRUE;
	  case 1:
		break;
	  default:
		printf("usage: milles [ restore_file ]\n");
		exit(-1);
		/* NOTREACHED */
	}
# ifndef PROF
	srandom(getpid());
# else
	srand(0);
# endif
	signal(SIGINT, rub);
	for (;;) {
		if (!restore || (Player[PLAYER].total >= 5000
		    || Player[COMP].total >= 5000)) {
			if (Player[COMP].total < Player[PLAYER].total)
				Player[PLAYER].games++;
			else if (Player[COMP].total > Player[PLAYER].total)
				Player[COMP].games++;
			Player[COMP].total = 0;
			Player[PLAYER].total = 0;
		}
		do {
			if (!restore)
				Handstart = Play = other(Handstart);
			if (!restore || On_exit) {
				shuffle();
				init();
			}
			newboard();
			if (restore)
				Error (Initstr);
			prboard();
			do {
				domove();
				if (Finished)
					newscore();
				prboard();
			} while (!Finished);
			check_more();
			restore = On_exit = FALSE;
		} while (Player[COMP].total < 5000
		    && Player[PLAYER].total < 5000);
	}
}

/*
 *	Routine to trap rubouts, and make sure they really want to
 * quit.
 */
rub() {

	signal(SIGINT, 1);
	if (getyn("Really? "))
		die();
	signal(SIGINT, rub);
}

/*
 *	Time to go beddy-by
 */
die() {

	signal(SIGINT, 1);
	if (outf)
		fflush(outf);
	finish_ui ();
	exit(1);
}
