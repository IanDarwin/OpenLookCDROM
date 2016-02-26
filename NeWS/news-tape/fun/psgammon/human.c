/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.
 * Users may copy, modify, or distribute this file at will.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifndef lint
static  char sccsid[] = "@(#)human.c 9.2 88/08/16 SMI";
#endif

/*
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

#include "defs.h"
#include "interface.h"

#define INTERMED	8

int humanmoves[4][4];
int intpnts[3][2], intcntr;	/* intermediate points in a move */
int rollstack[4], rollcntr;	/* rolls used in a move */
int humannomove;		/* human couldn't move on last turn */

/*
 * HumanMove
 *
 * make sure the move is valid; then send a message to the server
 * to paint it and one to bkg so it can make the computer's move;
 * piece_number is not used except in the message back to the server
 * (which needs it)
 */

HumanMove(piece_number, from_point, to_point)
int piece_number, from_point, to_point;
{
	int from, to;

	from = CvtPnt_ServerToClient(from_point);
	to = CvtPnt_ServerToClient(to_point);

	if (legalstart(from) && legalend(to) && legalmove(from, to)) {
		domove(from, to, piece_number, to_point);
	} else {
		ps_InvalidMove(piece_number);
		ps_flush_PostScript();
        }
}

legalstart(point)
int point;
{
	if ((computerboard[point] != 0) && (point != BAR)) {
		message(ERR, "You can't move my pieces!!!");
		return(0);
	}
	if (humanboard[point] == 0) {
		fprintf(stderr, "Server messed up! Human starting from empty point!");
		return(0); /* the server actually catches this */
	}
	if (point != BAR && humanboard[BAR] != 0) {
		message(ERR, "You must remove your barmen first!");
		return(0);
	}
	return(1);
}

legalend(point)
int point;
{
	if (computerboard[point] > 1 && point != HOME) {
		message(ERR, "You cannot land on a blocked point!");
		return(0);
	}
	return(1);
}

legalmove(from, to)
int from, to;
{
	int rollneeded, dice[2], numdice, index, furthest, midpnt;

	if (to == HOME) {
		for (index = 24; index > 6; index--) {
			if (index == from && humanboard[index] == 1)
				continue;
			if (humanboard[index] > 0) {
				message(ERR, "You cannot bear off until all your pieces are in your inner table!");
				return(0);
			}
		}
	}
	clearintermediatepnts();
	if (from == to)
		return(0);
	if (from == BAR && to == HOME)	/* never possible, and it messes */
		return(0);		/* up the algorithm */
	if (from == BAR)
		rollneeded = 25 - to;
	else if (to == HOME)
		rollneeded = from;
	else
		rollneeded = from - to;
	if (rollneeded < 0) {
		message(ERR, "Backwards move!");
		return(0);
	}
        if (humanboard[BAR] > 1 && rollneeded == humandieroll[0] + humandieroll[1]) {
                message(ERR, "You must remove all of your barmen!");
                return(0);
	}
	if (to == HOME) {
		for (furthest = 6; furthest > 1; furthest--) {
			if (humanboard[furthest] > 0)
				break;
		}
	}
	if ((humandieroll[0] & NUMMASK) == (humandieroll[1] & NUMMASK)) {
		if ((humandieroll[0] & ROLLUSED) == 0)
			numdice = 4;
		else if ((humandieroll[0] & SECONDROLLUSED) == 0)
			numdice = 3;
		else if ((humandieroll[1] & ROLLUSED) == 0)
			numdice = 2;
		else
			numdice = 1;
		if (rollneeded % (humandieroll[0] & NUMMASK) == 0) {
			if (rollneeded / (humandieroll[0] & NUMMASK) > numdice) {
				message(ERR, "You didn't roll that move!");
				return(0);
			}
			midpnt = from;
			for (index = 0; index < numdice; index++) {
				if ((rollneeded -= (humandieroll[0] & NUMMASK)) <= 0) {
					useroll(humandieroll[0] & NUMMASK);
					break;
				}
				midpnt -= humandieroll[0] & NUMMASK;
				if (computerboard[midpnt] == 1) {
					addintermediatepnt(midpnt, 1);
				} else if (computerboard[midpnt] <= 0) {
					addintermediatepnt(midpnt, 0);
				} else {
					message(ERR, "You cannot touch down on a blocked point!");
					return(0);
				}
				useroll(humandieroll[0] & NUMMASK);
			}
			return(1);
		} else if (to == HOME) {
			if (numdice * (humandieroll[0] & NUMMASK) < rollneeded) {
				message(ERR, "You didn't roll that move!");
				return(0);
			}
			if (from != furthest) {
				message(ERR, "You didn't roll that move!");
				return(0);
			}
			midpnt = from;
			for (index = 0; index < numdice; index++) {
				if ((rollneeded -= (humandieroll[0] & NUMMASK)) <= 0) {
					useroll(humandieroll[0] & NUMMASK);
					break;
				}
				midpnt -= humandieroll[0] & NUMMASK;
				for (furthest = 6; furthest > 1; furthest--) {
					if (humanboard[furthest] > 0 && !(furthest == from && humanboard[from] == 1))
						break;
				}
				if (midpnt != furthest) {
					message(ERR, "You didn't roll that move!");
					return(0);
				}
				if (computerboard[midpnt] == 1) {
					addintermediatepnt(midpnt, 1);
				} else if (computerboard[midpnt] <= 0) {
					addintermediatepnt(midpnt, 0);
				} else {
					message(ERR, "You cannot touch down on a blocked point!");
					return(0);
				}
				useroll(humandieroll[0] & NUMMASK);
			}
			return(1);
		} else {
			message(ERR, "You didn't roll that move!");
			return(0);
		}
	} else {	/* not doubles */
		if (humandieroll[0] & ROLLUSED) {
			dice[0] = humandieroll[1] & NUMMASK;
			numdice = 1;
		} else if (humandieroll[1] & ROLLUSED) {
			dice[0] = humandieroll[0] & NUMMASK;
			numdice = 1;
		} else {
			dice[0] = humandieroll[0] & NUMMASK;
			dice[1] = humandieroll[1] & NUMMASK;
			numdice = 2;
		}
		if (rollneeded == dice[0]) {
			useroll(dice[0]);
			return(1);
		}
		if (numdice == 2 && rollneeded == dice[1]) {
			useroll(dice[1]);
			return(1);
		}
		if (numdice == 2 && rollneeded == dice[0] + dice[1]) {
			midpnt = from - (humandieroll[0] & NUMMASK);
			if (computerboard[midpnt] == 1) {
				useroll(dice[0]);
				useroll(dice[1]);
				addintermediatepnt(midpnt, 1);
				return(1);
			}
			if (computerboard[midpnt] <= 0) {
				useroll(dice[0]);
				useroll(dice[1]);
				addintermediatepnt(midpnt, 0);
				return(1);
			}
			midpnt = from - (humandieroll[1] & NUMMASK);
			if (computerboard[midpnt] == 1) {
				useroll(dice[0]);
				useroll(dice[1]);
				addintermediatepnt(midpnt, 1);
				return(1);
			}
			if (computerboard[midpnt] <= 0) {
				useroll(dice[0]);
				useroll(dice[1]);
				addintermediatepnt(midpnt, 0);
				return(1);
			}
			message(ERR, "You cannot touch down on a blocked point!");
			return(0);
		}
		if (to != HOME) {
			message(ERR, "You didn't roll that move!");
			return(0);
		}
		if (dice[0] > dice[1]) {
			if (dice[0] > rollneeded) {
				if (from != furthest) {
					message(ERR, "You didn't roll that move!");
					return(0);
				}
				useroll(dice[0]);
				return(1);
			}
		} else {
			if (dice[1] > rollneeded) {
				if (from != furthest) {
					message(ERR, "You didn't roll that move!");
					return(0);
				}
				useroll(dice[1]);
				return(1);
			}
		}
		if (numdice < 2) {
			message(ERR, "You didn't roll that move!");
			return(0);
		}
		if (dice[0] + dice[1] > rollneeded) {
			if (from != furthest) {
				message(ERR, "You didn't roll that move!");
				return(0);
			}
			midpnt = from - (humandieroll[0] & NUMMASK);
			for (furthest = 6; furthest > 1; furthest--) {
				if (humanboard[furthest] > 0 && !(furthest == from && humanboard[from] == 1))
					break;
			}
			if (midpnt != furthest) {
				message(ERR, "You didn't roll that move!");
				return(0);
			}
			if (computerboard[midpnt] == 1) {
				useroll(dice[0]);
				useroll(dice[1]);
				addintermediatepnt(midpnt, 1);
				return(1);
			}
			if (computerboard[midpnt] <= 0) {
				useroll(dice[0]);
				useroll(dice[1]);
				addintermediatepnt(midpnt, 0);
				return(1);
			}
			midpnt = from - (humandieroll[1] & NUMMASK);
			if (midpnt != furthest) {
				message(ERR, "You didn't roll that move!");
				return(0);
			}
			if (computerboard[midpnt] == 1) {
				useroll(dice[0]);
				useroll(dice[1]);
				addintermediatepnt(midpnt, 1);
				return(1);
			}
			if (computerboard[midpnt] <= 0) {
				useroll(dice[0]);
				useroll(dice[1]);
				addintermediatepnt(midpnt, 0);
				return(1);
			}
			message(ERR, "You cannot touch down on a blocked point!");
			return(0);
		}
		message(ERR, "You didn't roll that  move!");
		return(0);
	}
}

useroll(num)
int num;
{
	if (num == (humandieroll[0] & NUMMASK)) {
		if ((humandieroll[0] & ROLLUSED) == 0) {
			humandieroll[0] |= ROLLUSED;
			rollstack[rollcntr++] = humandieroll[0] & NUMMASK;
		} else if ((humandieroll[0] & SECONDROLLUSED) == 0) { /* must be dbles */
			humandieroll[0] |= SECONDROLLUSED;
			rollstack[rollcntr++] = humandieroll[0] & NUMMASK;
		} else if ((humandieroll[1] & ROLLUSED) == 0) {
			humandieroll[1] |= ROLLUSED;
			rollstack[rollcntr++] = humandieroll[1] & NUMMASK;
		} else {
			humandieroll[1] |= SECONDROLLUSED;
			rollstack[rollcntr++] = humandieroll[1] & NUMMASK;
		}
	} else {
		humandieroll[1] |= ROLLUSED;
		rollstack[rollcntr++] = humandieroll[1] & NUMMASK;
	}
}

domove(from, to, piece_number, to_point)
int from, to, piece_number, to_point;
{
	int ind = 0;

	humanmoves[movesmade][FROM] = from;
	if (intcntr) {
		for (ind = 0; ind < intcntr; ind++) {
			humanmoves[movesmade][TO] = intpnts[ind][0];
			if (intpnts[ind][1]) {
				movepiece(intpnts[ind][0], BAR, COMPUTER);
				ps_MovePiece(
				    CvtPnt_ClientToServer(intpnts[ind][0], HUMAN),
				    CvtPnt_ClientToServer(BAR, COMPUTER)
				);
				humanmoves[movesmade][HITBLOT] = 1;
			} else {
				humanmoves[movesmade][HITBLOT] = 0;
			}
			humanmoves[movesmade][DIEUSED] = rollstack[ind] | INTERMED;
			humanmoves[++movesmade][FROM] = intpnts[ind][0];
		}
	}
	if (computerboard[to] == 1 && to != HOME) {
		ps_MovePiece(
		    CvtPnt_ClientToServer(to, HUMAN),
		    CvtPnt_ClientToServer(BAR, COMPUTER)
		);
		movepiece(to, BAR, COMPUTER);
		humanmoves[movesmade][HITBLOT] = 1;
	} else {
		humanmoves[movesmade][HITBLOT] = 0;
	}
	humanmoves[movesmade][TO] = to;
	humanmoves[movesmade][DIEUSED] = rollstack[ind];
	movepiece(from, to, HUMAN);
	ps_HumanMove(piece_number, to_point);
	ps_flush_PostScript();
	if (++movesmade == maxmoves || humanboard[HOME] == 15)
		endhumanturn();
}

int rollsleft, dicecopy[4], boardcopy[26], numdice;

starthumanturn()
{
	int index;

	/* find the maximum number of moves the human can make */

	boardcopy[BAR] = humanboard[BAR];
	for (index = 1; index < 25; index++) {
		boardcopy[25 - index] = humanboard[index];
		if (computerboard[index] > 1)
			boardcopy[25 - index] = BLOCKED;
	}
	dicecopy[0] = humandieroll[0];
	dicecopy[1] = humandieroll[1];
	if (humandieroll[0] == humandieroll[1]) {
		dicecopy[2] = dicecopy[3] = dicecopy[0];
		numdice = 4;
		rollsleft = 4;
	} else {
		numdice = 2;
		rollsleft = 2;
	}
	maxmoves = 0;
	findmaxrolls(0);
	if (!diddouble)
		sendtogammon(NODOUBLE);
	diddouble = 0;
	if (maxmoves > 0) {
		state = MOVE;
		ps_SetCursor(MOVE_CUR);
		ps_HumanCanMove(1);
		ps_flush_PostScript();
		movesmade = 0;
		humannomove = 0;
		sendtogammon(CANMOVE);
		sendtogammon(DIEROLL);
		sendtogammon((humandieroll[0] & NUMMASK) + '0');
		sendtogammon((humandieroll[1] & NUMMASK) + '0');
		lastmoved = HUMAN;
	} else {
		message(MSG, "You can't move...");
		humannomove = 1;
		ps_SetCursor(THINKING_CUR);
		ps_flush_PostScript();
		sleep(2);
		message(MSG, "Thinking...");
		sendtogammon(CANTMOVE);
		state = THINKING;
		if (logfp != NULL)
			fprintf(logfp, "Human: (%d, %d) Can't move.\n", humandieroll[0], humandieroll[1]);
	}
}

findmaxrolls(movesdone)
int movesdone;
{
	int point, roll;

	if (boardcopy[BAR] > 0) {
		for (roll = 0; roll < numdice; roll++) {
			if (dicecopy[roll] & ROLLUSED)
				continue;
			if (boardcopy[dicecopy[roll]] != BLOCKED) {
				boardcopy[dicecopy[roll]]++;
				boardcopy[BAR]--;
				dicecopy[roll] |= ROLLUSED;
				if (++movesdone > maxmoves)
					maxmoves = movesdone;
				if (--rollsleft == 0)
					return(1);
				if (findmaxrolls(movesdone))
					return(1);
				dicecopy[roll] &= ~ROLLUSED;
				boardcopy[dicecopy[roll]]--;
				boardcopy[BAR]++;
				rollsleft++;
				movesdone--;
			}
		}
		if (boardcopy[BAR] > 0)
			return(0);
	}
	for (point = 1; point <= 24; point++) {
		if (boardcopy[point] <= 0)
			continue;
		for (roll = 0; roll < numdice; roll++) {
			if (dicecopy[roll] & ROLLUSED)
				continue;
			if (dicecopy[roll] + point >= 25) {
				int c;

				for (c = 1; c < 24; c++)
					if (boardcopy[c] > 0)
						break;
				if (c < 19)
					continue;
				if (dicecopy[roll] + point > 25 && point != c)
					continue;
				boardcopy[HOME]++;
			} else if (boardcopy[dicecopy[roll] + point] != BLOCKED) {
				boardcopy[dicecopy[roll] + point]++;
			} else {
				continue;
			}
			boardcopy[point]--;
			dicecopy[roll] |= ROLLUSED;
			if (++movesdone > maxmoves)
				maxmoves = movesdone;
			if (--rollsleft == 0)
				return(1);
			if (findmaxrolls(movesdone))
				return(1);
			dicecopy[roll] &= ~ROLLUSED;
			if (dicecopy[roll] + point >= 25)
				boardcopy[HOME]--;
			else
				boardcopy[dicecopy[roll] + point]--;
			boardcopy[point]++;
			rollsleft++;
			movesdone--;
		}
	}
	return(0);
}

endhumanturn()
{
	int c;

	if (humanboard[HOME] == 15) {
		if (computerboard[BAR] > 0) {
			win(HUMAN, BACKGAMMON);
		} else {
			for (c = 1; c < 25; c++)
				if (computerboard[c] > 0)
					break;
			if (c <= 6)
				win(HUMAN, BACKGAMMON);
			else if (computerboard[HOME] == 0)
				win(HUMAN, GAMMON);
			else
				win(HUMAN, STRAIGHT);
		}
	} else {
		sendhumanmove();
		message(MSG, "Thinking...");
		state = THINKING;
		ps_SetCursor(THINKING_CUR);
		ps_HumanCanMove(0);
		ps_flush_PostScript();
	}
}

undoonemove()
{
	if (movesmade == 0)
		return;
	movesmade--;
	movepiece(humanmoves[movesmade][TO], humanmoves[movesmade][FROM], HUMAN);
	ps_MovePiece(
	    CvtPnt_ClientToServer(humanmoves[movesmade][TO], HUMAN),
	    CvtPnt_ClientToServer(humanmoves[movesmade][FROM], HUMAN)
	);
	if (humanmoves[movesmade][HITBLOT]) {
		movepiece(BAR, humanmoves[movesmade][TO], COMPUTER);
		ps_MovePiece(
		    CvtPnt_ClientToServer(BAR, COMPUTER),
		    CvtPnt_ClientToServer(humanmoves[movesmade][TO], COMPUTER)
		);
	}
	ps_flush_PostScript();

	/* painstakingly find which roll was used and reclaim it */

	if ((humandieroll[0] & NUMMASK) == (humandieroll[1] & NUMMASK)) {
		if (humandieroll[1] & SECONDROLLUSED) {
			humandieroll[1] &= ~SECONDROLLUSED;
		} else if (humandieroll[1] & ROLLUSED) {
			humandieroll[1] &= ~ROLLUSED;
		} else if (humandieroll[0] & SECONDROLLUSED) {
			humandieroll[0] &= ~SECONDROLLUSED;
		} else {
			humandieroll[0] &= ~ROLLUSED;
		}
	} else {
		if ((humandieroll[0] & NUMMASK) == (humanmoves[movesmade][DIEUSED] & ~INTERMED)) {
			humandieroll[0] &= ~ROLLUSED;
		} else {
			humandieroll[1] &= ~ROLLUSED;
		}
	}
	if (movesmade && (humanmoves[movesmade-1][DIEUSED] & INTERMED))
		undoonemove();
}

undowholemove()
{
	while (movesmade)
		undoonemove();
}

sendhumanmove()
{
	int c;

	for (c = 0; c < movesmade; c++) {
		if (c > 0)
			sendtogammon(',');
		if (humanmoves[c][FROM] == BAR) {
			sendtogammon('B');
		} else {
			if (humanmoves[c][FROM] > 9)
				sendtogammon(humanmoves[c][FROM] / 10 + '0');
			sendtogammon(humanmoves[c][FROM] % 10 + '0');
		}
		sendtogammon('-');
		if (humanmoves[c][TO] == HOME) {
			sendtogammon('H');
		} else {
			if (humanmoves[c][TO] > 9)
				sendtogammon(humanmoves[c][TO] / 10 + '0');
			sendtogammon(humanmoves[c][TO] % 10 + '0');
		}
	}
	sendtogammon('\n');
	if (logfp != NULL) {
		fprintf(logfp, "Human: (%d, %d) ", humandieroll[0] & NUMMASK, humandieroll[1] & NUMMASK);
		for (c = 0; c < movesmade; c++) {
			if (c > 0)
				fprintf(logfp, ", ");
			fprintf(logfp, "%d%c%d", humanmoves[c][FROM],
				humanmoves[c][HITBLOT] ? 'x' : '-',
				humanmoves[c][TO]);
		}
		putc('\n', logfp);
	}
}

showlasthumanmove()
{
	int index;

	if (movesmade == 0) {
		message(ERR, "Nobody has moved yet!");
		return;
	}
	if (dicedisplayed == COMPUTER)
		showlastdice();
	if (!humannomove) {
		for (index = movesmade-1; index >= 0; index--) {
			movepiece(humanmoves[index][TO], humanmoves[index][FROM], HUMAN);
			ps_MovePiece(
			    CvtPnt_ClientToServer(humanmoves[index][TO], HUMAN),
			    CvtPnt_ClientToServer(humanmoves[index][FROM], HUMAN)
			);
			if (humanmoves[index][HITBLOT]) {
			    movepiece(BAR, humanmoves[index][TO], COMPUTER);
			    ps_MovePiece(
				CvtPnt_ClientToServer(BAR, COMPUTER),
				CvtPnt_ClientToServer(humanmoves[index][TO], COMPUTER)
			    );
			}
		}
	}
	ps_flush_PostScript();
	sleep(2);
	if (!humannomove) {
		for (index = 0; index < movesmade; index++) {
			if (humanmoves[index][HITBLOT]) {
			    movepiece(humanmoves[index][TO], BAR, COMPUTER);
			    ps_MovePiece(
				CvtPnt_ClientToServer(humanmoves[index][TO], COMPUTER),
				CvtPnt_ClientToServer(BAR, COMPUTER)
			    );
			}
			movepiece(humanmoves[index][FROM], humanmoves[index][TO], HUMAN);
			ps_MovePiece(
			    CvtPnt_ClientToServer(humanmoves[index][FROM],HUMAN),
			    CvtPnt_ClientToServer(humanmoves[index][TO], HUMAN)
			);
		}
	}
	if (dicedisplayed == COMPUTER)
		showlastdice();
	ps_flush_PostScript();
}

clearintermediatepnts()
{
	int i;

	for (i = 0; i < 3; i++)
		intpnts[i][0] = intpnts[i][1] = 0;
	intcntr = 0;
	rollcntr = 0;
}

addintermediatepnt(point, hitblot)
int point, hitblot;
{
	intpnts[intcntr][0] = point;
	intpnts[intcntr][1] = hitblot;
	intcntr++;
}
