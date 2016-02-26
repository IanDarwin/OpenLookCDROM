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
static  char sccsid[] = "@(#)buttons.c 9.1 87/11/05 SMI";
#endif

/*
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

#include "defs.h"
#include "interface.h"

double_proc()
{
	message(ERR, "");
	switch (state) {
	case STARTGAME:
		message(ERR, "The game hasn't started yet!");
		break;
	case COMPUTERDOUBLING:
		message(ERR, "You must accept or refuse the computer's double first!");
		break;
	case HUMANDOUBLING:
		message(ERR, "Hold on, the computer is thinking about whether to accept your double!");
		break;
	case ROLL:
		if (lastdoubled == HUMAN) {
			message(ERR, "You don't have the cube!");
		} else if (gamevalue >= 64) {
			message(ERR, "You can't double any higher!");
		} else {
			sendtogammon(DOUBLEREQ);
			state = HUMANDOUBLING;
			diddouble = 1;
			if (logfp != NULL)
				fprintf(logfp, "Human doubled.\n");
		}
		break;
	case MOVE:
		message(ERR, "You can only double before you roll the dice!");
		break;
	case THINKING:
		message(ERR, "It's not your turn!");
		break;
	case GAMEOVER:
		message(ERR, "The game is over!");
		break;
	}
}

accept_proc()
{
	message(ERR, "");
	switch (state) {
	case STARTGAME:
		message(ERR, "The game hasn't started yet!");
		break;
	case COMPUTERDOUBLING:
		gamevalue *= 2;
		lastdoubled = COMPUTER;
		ps_DrawCube(gamevalue, 0);
		ps_flush_PostScript();
		sendtogammon(ACCEPT_DBLE);
		message(MSG, "Thinking...");
		startcomputerturn();
		break;
	case HUMANDOUBLING:
		message(ERR, "Hold on, the computer is thinking about whether to accept your double!");
		break;
	case ROLL:
		message(ERR, "The computer hasn't doubled you!");
		break;
	case MOVE:
		message(ERR, "The computer hasn't doubled you!");
		break;
	case THINKING:
		message(ERR, "It's not your turn!");
		break;
	case GAMEOVER:
		message(ERR, "The game is over!");
		break;
	}
}

refuse_proc()
{
	message(ERR, "");
	switch (state) {
	case STARTGAME:
		message(ERR, "The game hasn't started yet!");
		break;
	case COMPUTERDOUBLING:
		win(COMPUTER, REFUSEDDBLE);
		break;
	case HUMANDOUBLING:
		message(ERR, "Hold on, the computer is thinking about whether to accept your double!");
		break;
	case ROLL:
		message(ERR, "The computer hasn't doubled you!");
		break;
	case MOVE:
		message(ERR, "The computer hasn't doubled you!");
		break;
	case THINKING:
		message(ERR, "It's not your turn!");
		break;
	case GAMEOVER:
		message(ERR, "The game is over!");
		break;
	}
}

showmove_proc()
{
	message(ERR, "");
	if (state == STARTGAME) {
		message(ERR, "The game hasn't started yet!");
	} else {
		if (lastmoved == HUMAN && movesmade > 0)
			showlasthumanmove();
		else
			showlastcomputermove();
	}
}

redo_proc()
{
	message(ERR, "");
	switch (state) {
	case STARTGAME:
		message(ERR, "The game hasn't started yet!");
		break;
	case COMPUTERDOUBLING:
		message(ERR, "You must accept or refuse the computer's double!");
		break;
	case HUMANDOUBLING:
		message(ERR, "Hold on, the computer is thinking about whether to accept your double!");
		break;
	case ROLL:
		message(ERR, "You haven't moved yet!");
		break;
	case MOVE:
		if (movesmade <= 0)
			message(ERR, "You haven't moved yet!");
		else
			undoonemove();
		break;
	case THINKING:
		message(ERR, "It's not your turn!");
		break;
	case GAMEOVER:
		message(ERR, "The game is over!");
		break;
	}
}

redoall_proc()
{
	message(ERR, "");
	switch (state) {
	case STARTGAME:
		message(ERR, "The game hasn't started yet!");
		break;
	case COMPUTERDOUBLING:
		message(ERR, "You must accept or refuse the computer's double!");
		break;
	case HUMANDOUBLING:
		message(ERR, "Hold on, the computer is thinking about whether to accept your double!");
		break;
	case ROLL:
		message(ERR, "You haven't moved yet!");
		break;
	case MOVE:
		if (movesmade <= 0)
			message(ERR, "You haven't moved yet!");
		else
			undowholemove();
		break;
	case THINKING:
		message(ERR, "It's not your turn!");
		break;
	case GAMEOVER:
		message(ERR, "The game is over!");
		break;
	}
}

forfeit_proc()
{
	message(ERR, "");
	if (state == STARTGAME) {
		message(ERR, "The game hasn't started yet!");
	} else if (state == GAMEOVER) {
		message(ERR, "The game is over!");
	} else if (state == COMPUTERDOUBLING) {
		win(COMPUTER, REFUSEDDBLE);
	} else {
		message(ERR, "");
		win(COMPUTER, RESIGNED);
	}
}

quit_proc()
{
	message(ERR, "");
	if (state != GAMEOVER && state != STARTGAME) {
		message(ERR, "If you want to quit the game you must forfeit first.");
		return;
	}
	cleanup();
}

newgame_proc()
{
	message(ERR, "");
	if (state == STARTGAME) {	
		message(MSG, "To begin the game, roll the dice by clicking the left or middle buttons.");
		message(ERR, "(The mouse cursor may be anywhere over the board when rolling the dice.)");
	} else if (state != GAMEOVER) {
		message(ERR, "If you want to restart the game, you must forfeit this one first.");
	} else {
		ps_InitGame();
		ps_flush_PostScript();
		startgammon();
		replacepieces();
		initpieces();
		numhdice = numcdice = 0;
		ps_DrawDice(0, 0, 0, 0);
		gamevalue = 1;
		lastdoubled = NOBODY;
		ps_DrawCube(gamevalue, 1);
		state = STARTGAME;
		message(MSG, "To begin the game, roll the dice by clicking the left or middle buttons.");
		message(ERR, "(The mouse cursor may be anywhere over the board when rolling the dice.)");
		/* the messages do the ps_flush_PostScript() */
	}
}
