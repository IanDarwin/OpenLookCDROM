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
static  char sccsid[] = "@(#)main.c 9.2 88/08/16 SMI";
#endif

/*
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

/*
 * this C code is a modified version of gammontool (that's my
 * excuse for how lousy the code is...)
 */

#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include "psio.h"
#include "defs.h"

#include "board.h"
#include "interface.h"
#include "message.h"
#include "panel.h"
#include "init.h"

main(argc, argv)
int argc;
char **argv;
{
	int cleanup(), fixedsize = 0, debug = 0;

	while (--argc) {
		++argv;
		if (!strcmp(*argv, "-f"))
			fixedsize = 1;
		else if (!strcmp(*argv, "-d"))
			debug = 1;
		else
			gammonbin = *argv;
	}

	signal(SIGINT, cleanup);
	signal(SIGQUIT, cleanup);
	signal(SIGTERM, cleanup);	
	if (ps_open_PostScript() == 0) {
		fprintf(stderr, "No NeWS server.\n");
		exit(1);
	}
	ps_InitConstants();
        if (fixedsize)
		ps_FixedSize();
	ps_SetupPanel();
	ps_SetupMessages();
	ps_SetupBoard();
        GetOldScore();
	ps_CreateWindow();
	ps_SetupInterface();
	if (debug)
	    ps_DebuggingOn();
        ps_flush_PostScript();
	message(MSG, "To begin the game, roll the dice by clicking the left or middle buttons.");
	message(ERR, "(The mouse cursor may be anywhere over the board when rolling the dice.)");
	initmatch();
	startgammon();

        ProcessRequests();

	cleanup();
}

cleanup()
{
    	ps_close_PostScript();
	if (logfp != NULL)
		fprintf(logfp, "Match over.\n");
        killpg(getpgrp(), SIGINT);
	exit(0);
}

ProcessRequests()
{
	int readfds;

	while (1) {
	    readfds = (1 << gammonfd) | (1 << psio_fileno(PostScriptInput));
	    if (select(32, &readfds, NULL, NULL, NULL) <= 0) {
		    fprintf(stderr, "Select failed!\n");
		    continue;
	    }
	    if (readfds & (1 << gammonfd))
		    readfromgammon();
	    if (readfds & (1 << psio_fileno(PostScriptInput)))
		    ReadPostScript();
	}
}

ReadPostScript()
{
	char buf[64];
	int arg1, arg2, arg3;
	int i;

	/*
	 * the server-side routines send ASCII messages generated
	 * by sprintf; choke if you want, but since the number and
	 * size of the messages is so small the performance
	 * improvement achieved by using encoded data is minimal,
	 * and encoded data is much harder to look at for
	 * debugging purposes
	 */

	for (i = 0; i < 64; i++)
	{
		buf[i] = psio_getc(PostScriptInput);

		if (psio_error(PostScriptInput)) {
			fprintf(stderr, "PostScript process died!\n");
			cleanup();
			/* cleanup exits */
		}

		if (buf[i] == '\n')
			break;

		if (psio_eof(PostScriptInput))
			if (i == 0) {
				fprintf(stderr, "PostScript process died!\n");
				cleanup();
				/* cleanup exits */
			}
			else
				break;
	}
	buf[++i] = '\0';

	switch (buf[0]) {
	case ps_MoveCode:
		sscanf(buf, "%*c %d %d %d", &arg1, &arg2, &arg3);
		HumanMove(arg1, arg2, arg3);
		break;
	case ps_DoubleRequestCode:
		double_proc();
		break;
	case ps_AcceptDoubleCode:
		accept_proc();
		break;
	case ps_RefuseDoubleCode:
		refuse_proc();
		break;
	case ps_ShowLastMoveCode:
		showmove_proc();
		break;
	case ps_RedoLastMoveCode:
		redo_proc();
		break;
	case ps_RestartMoveCode:
		redoall_proc();
		break;
	case ps_ForfeitCode:
		forfeit_proc();
		break;
	case ps_QuitCode:
		quit_proc();
		break;
	case ps_NewGameCode:
		newgame_proc();
		break;
        case ps_GotMouseClickCode:
		HandleMouseClick();
	        break;
	default:
		printf("Bad command: %s\n", buf);
		break;
	}
}

HandleMouseClick()
{
        message(ERR, "");
	switch (state) {
	case STARTGAME:
		initgame();
		break;
	case ROLL:
		rolldice(HUMAN);
		message(MSG, "Your turn to move.");
		starthumanturn();
		break;
	case MOVE:
		break;
	case THINKING:
		message(ERR, "Hold on, I'm still thinking!");
		break;
	case GAMEOVER:
		message(ERR, "The game is over!");
		break;
	case COMPUTERDOUBLING:
		message(ERR, "Do you accept the computer's double?");
		break;
	case HUMANDOUBLING:
		message(ERR, "I'm thinking about it!");
		break;
	}
}

/*
 * unfortunately the point numbering convention used by gammontool
 * is very inconvenient for the server-side code, and I didn't feel
 * like trying to change the old gammontool code, so the following
 * two conversion routines must be called every time a point number
 * is transmitted
 */

CvtPnt_ClientToServer(point, who)
int point, who;
{
	if (point == BAR) {
		if (who == HUMAN)
			return(0);
		else
			return(25);
	} else if (point == HOME) {
		if (who == HUMAN)
			return(26);
		else
			return(27);
	}
	return(point);
}

CvtPnt_ServerToClient(point)
int point;
{
	if (point == 0 || point == 25)
		return(BAR);
	else if (point == 26 || point == 27)
		return(HOME);
	else
		return(point);
}
#define NUMPOINTS	4

int humanpieces[NUMPOINTS][2] = { {6, 5}, {8, 3}, {13, 5}, {24, 2} };
int computerpieces[NUMPOINTS][2] = { {1, 2}, {12, 5}, {17, 3}, {19, 5} };

GetOldScore()
{
	getscore(humanname, &humanscore, &computerscore);
        ps_SetHumanName(humanname);
        ps_DrawScore(humanscore, computerscore);
}

static
initmatch()
{
	struct stat buf;
	char logfile[512];

	srandom(time(0));
	state = STARTGAME;
	humancolor = WHITE;	/* for drawing purposes these must be set */
	initpieces();		/* they will be updated when game starts */
	numhdice = numcdice = 0;
	lastdoubled = NOBODY;
	lastmoved = NOBODY;
	alreadyrolled = 0;

	/* open the log file only if it is already there */

	sprintf(logfile, "%s/gammonlog", getenv("HOME"));
	if (stat(logfile, &buf) == 0) {
		if ((logfp = fopen(logfile, "a")) != NULL) {
			setbuf(logfp, NULL);
			if (buf.st_size > 0)
				putc('\n', logfp);
			fprintf(logfp, "New match.\n");
		}
	}
}


message(error, str)
int error;
char *str;
{
	if (error)
		ps_ErrorMsg(str);
	else
		ps_Message(str);
	ps_flush_PostScript();
}

initgame()
{
	if (logfp != NULL)
		fprintf(logfp, "New game.\n");
	initpieces();
	gamevalue = 1;
	lastdoubled = NOBODY;
	diddouble = 0;
	rolldice(BOTH);		/* initial roll; doubles not allowed */
	while (humandieroll[0] == computerdieroll[0])
		rolldice(BOTH);
	if (humandieroll[0] > computerdieroll[0]) {
		sendtogammon(HUMANFIRST);
		message(MSG, "You go first.");
		dicedisplayed = HUMAN;
		starthumanturn();
	} else {
		sendtogammon(COMPUTERFIRST);
		alreadyrolled = 1;
		message(MSG, "The computer goes first.  Thinking...");
		state = THINKING;
		dicedisplayed = COMPUTER;
		ps_SetCursor(THINKING_CUR);
		ps_flush_PostScript();
	}
}

initpieces()
{
	int index;

	for (index = 0; index < 26; index++)
		humanboard[index] = computerboard[index] = 0;
	for (index = 0; index < NUMPOINTS; index++) {
		humanboard[humanpieces[index][0]] = humanpieces[index][1];
		computerboard[computerpieces[index][0]] = computerpieces[index][1];
	}
}

rolldice(who)
int who;
{
	int d0, d1, c;

	/* save dice currently displayed for show-last-move */

        bcopy(currentdice, lastdice, sizeof(lastdice));

	/* generate numbers for new dice */

	d0 = random() % 6 + 1;
	d1 = random() % 6 + 1;

	/* assign numbers depending on whose turn it is */

	switch (who) {
	case HUMAN:
		humandieroll[0] = d0;
		humandieroll[1] = d1;
		numhdice = 2;
		numcdice = 0;
		dicedisplayed = HUMAN;
		ps_DrawDice(d0, d1, 0, 0);
		currentdice[0] = d0;
		currentdice[1] = d1;
		currentdice[2] = 0;
		currentdice[3] = 0;
		ps_flush_PostScript();
		break;
	case COMPUTER:
		computerdieroll[0] = d0;
		computerdieroll[1] = d1;
		numcdice = 2;
		numhdice = 0;
		dicedisplayed = COMPUTER;
		ps_DrawDice(0, 0, d0, d1);
		currentdice[0] = 0;
		currentdice[1] = 0;
		currentdice[2] = d0;
		currentdice[3] = d1;
		ps_flush_PostScript();
		break;
	case BOTH:			/* special case for first roll */
		humandieroll[0] = d0;
		humandieroll[1] = d1;
		computerdieroll[0] = d1;
		computerdieroll[1] = d0;
		numhdice = numcdice = 1;
		ps_DrawDice(0, d0, d1, 0);
		currentdice[0] = 0;
		currentdice[1] = d0;
		currentdice[2] = d1;
		currentdice[3] = 0;
		ps_flush_PostScript();
		break;
	}
}

win(who, how)
int who, how;
{
	int points = gamevalue;

	if (who == HUMAN) {
		switch (how) {
		case STRAIGHT:
			message(MSG, "You beat the computer!");
			if (logfp != NULL)
				fprintf(logfp, "Human won.\n");
			break;
		case GAMMON:
			message(MSG, "You gammoned the computer!");
			points *= 2;
			if (logfp != NULL)
				fprintf(logfp, "Human gammoned computer.\n");
			break;
		case BACKGAMMON:
			message(MSG, "You backgammoned the computer!!!");
			points *= 3;
			if (logfp != NULL)
				fprintf(logfp, "Human backgammoned computer.\n");
			break;
		case REFUSEDDBLE:
			message(MSG, "The computer refused your double!");
			if (logfp != NULL)
				fprintf(logfp, "Computer refused double.\n");
			break;
		case RESIGNED:
			message(MSG, "The computer resigned!");
			points *= 3;
			if (logfp != NULL)
				fprintf(logfp, "Computer resigned\n");
			break;
		}
		humanscore += points;
		ps_DrawScore(humanscore, computerscore);
		ps_flush_PostScript();
	} else {
		switch (how) {
		case STRAIGHT:
			message(MSG, "The computer beat you!");
			if (logfp != NULL)
				fprintf(logfp, "Computer won.\n");
			break;
		case GAMMON:
			message(MSG, "The computer gammoned you!");
			points *= 2;
			if (logfp != NULL)
				fprintf(logfp, "Computer gammoned human.\n");
			break;
		case BACKGAMMON:
			message(MSG, "The computer backgammoned you!!!");
			points *= 3;
			if (logfp != NULL)
				fprintf(logfp, "Computer backgammoned human.\n");
			break;
		case REFUSEDDBLE:
			message(MSG, "The computer wins!");
			if (logfp != NULL)
				fprintf(logfp, "Human refused double.\n");
			break;
		case RESIGNED:
			message(MSG, "The computer takes triple the stakes!");
			points *= 3;
			if (logfp != NULL)
				fprintf(logfp, "Human resigned\n");
			break;
		}
		computerscore += points;
		ps_DrawScore(humanscore, computerscore);
		ps_flush_PostScript();
	}
	state = GAMEOVER;
	ps_SetCursor(ORIGINAL_CUR);
	ps_flush_PostScript();
	savescore(humanscore, computerscore);
	if (logfp != NULL)
		fprintf(logfp, "Score: %d (human), %d (computer)\n", humanscore, computerscore);
}

movepiece(from, to, who)
int from, to, who;
{
	int color, fromnum, tonum;

	if (who == HUMAN) {
		fromnum = humanboard[from]--;
		tonum = humanboard[to]++;
		color = humancolor;
	} else {
		fromnum = computerboard[from]--;
		tonum = computerboard[to]++;
		color = computercolor;
	}
}

replacepieces()
{
	int c, p, pmax, pc, moves[30][5], movcntr = 0, pnt;

	for (c = 0; c < 26; c++) {
		pc = pointcount(c, humancolor);
		if (humanboard[c] > 0 && humanboard[c] > pc) {
			pmax = humanboard[c] - pc;
			for (p = 0; p < pmax; p++) {
				pnt = findapoint(humancolor);
				moves[movcntr][0] = c;
				moves[movcntr][1] = pnt;
				moves[movcntr][2] = humanboard[c]--;
				moves[movcntr][3] = humanboard[pnt]++;
				moves[movcntr][4] = humancolor;
				movcntr++;
			}
		}
		pc = pointcount(c, computercolor);
		if (computerboard[c] > 0 && computerboard[c] > pc) {
			pmax = computerboard[c] - pc;
			for (p = 0; p < pmax; p++) {
				pnt = findapoint(computercolor);
				moves[movcntr][0] = c;
				moves[movcntr][1] = pnt;
				moves[movcntr][2] = computerboard[c]--;
				moves[movcntr][3] = computerboard[pnt]++;
				moves[movcntr][4] = computercolor;
				movcntr++;
			}
		}
	}
}

pointcount(point, color)
int point, color;
{
	int index;

	for (index = 0; index < NUMPOINTS; index++) {
		if (color == humancolor) {
			if (humanpieces[index][0] == point)
				return(humanpieces[index][1]);
		} else {
			if (computerpieces[index][0] == point)
				return(computerpieces[index][1]);
		}
	}
	return(0);
}

findapoint(color)
int color;
{
	int index;

	for (index = 0; index < NUMPOINTS; index++) {
		if (humancolor == color) {
			if (humanboard[humanpieces[index][0]] < humanpieces[index][1])
				return(humanpieces[index][0]);
		} else {
			if (computerboard[computerpieces[index][0]] < computerpieces[index][1])
				return(computerpieces[index][0]);
		}

	}
}

/* display the die rolls which were last on the screen */
/* a second call to showlastdice redisplays the original dice */

showlastdice()
{
	int swapspace[4];

        bcopy(currentdice, swapspace, sizeof(swapspace));
        bcopy(lastdice, currentdice, sizeof(currentdice));
        bcopy(swapspace, lastdice, sizeof(lastdice));

        ps_DrawDice(currentdice[0], currentdice[1],
		    currentdice[2], currentdice[3]);
	ps_flush_PostScript();
}
