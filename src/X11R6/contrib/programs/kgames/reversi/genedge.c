/*
 *	generate preliminary edge score array
 */

char	board[9];

main ()
{
	register int	i;
	for (board[1] = -1; board[1] <= 2; board[1]++)
	for (board[2] = -1; board[2] <= 2; board[2]++)
	for (board[3] = -1; board[3] <= 2; board[3]++)
	for (board[4] = -1; board[4] <= 2; board[4]++)
	for (board[5] = -1; board[5] <= 2; board[5]++)
	for (board[6] = -1; board[6] <= 2; board[6]++)
	for (board[7] = -1; board[7] <= 2; board[7]++)
	for (board[8] = -1; board[8] <= 2; board[8]++) {
		for (i = 1; i <= 8; i++) {
			if (board[i] == 2)
				break;
		}
		if (i == 9) {
			for (i = 1; i <= 8; i++) {
				switch (board[i]) {
				case 0:
					printf (" -");
					break;
				case -1:
					printf (" O");
					break;
				case 1:
					printf (" *");
					break;
				}
			}
			printf ("\n");
		}
		else
		{
			printf ("?\n");
		}
	}
	exit (0);
}
