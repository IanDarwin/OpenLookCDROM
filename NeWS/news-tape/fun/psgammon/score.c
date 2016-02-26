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
static  char sccsid[] = "@(#)score.c 9.1 87/11/05 SMI";
#endif

/*
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <fcntl.h>
#include <pwd.h>
#include "score.h"
#include "defs.h"

getscore(name, humanp, computerp)
	char *name;
	int *humanp;
	int *computerp;
{
	struct passwd *pwd;
	struct scorerec srec;
	int uid;
	FILE *score;

	uid = getuid();
	pwd = getpwuid(uid);
	if (pwd == NULL) {
		strcpy(name, "human");
	} else {
		strncpy(name, pwd->pw_name, MAXNAME);
	}
	*humanp = 0;
	*computerp = 0;

	score = fopen(SCOREFILE, "r");
	if (score == NULL) {
		return;
	}

	while (fread(&srec, sizeof (srec), 1, score) != NULL) {
		if (srec.uid != uid) {
			continue;
		}
		*humanp = srec.human;
		*computerp = srec.computer;
		break;
	}

	fclose(score);
}

savescore(human, computer)
	int human;
	int computer;
{
	int uid;
	struct scorerec srecs[1000];
	struct scorerec *sp;
	int nbytes;
	int nrecs;
	int score;

	uid = getuid();
	score = open(SCOREFILE, O_CREAT|O_RDWR, 0644);
	if (score < 0) {
		message(ERR, "can't open score file");
		return;
	}

	while ((nbytes = read(score, srecs, sizeof (srecs))) > 0) {
		if (nbytes % sizeof (struct scorerec)) {
			message(ERR, "Corrupted score file");
			close(score);
			return;
		}
		nrecs = nbytes / sizeof (struct scorerec);
		for (sp = srecs; sp < &srecs[nrecs] && sp->uid != uid; sp++)
			;
		if (sp->uid == uid) {
			break;
		}
	}

	if (nbytes < 0) {
		message(ERR, "can't read score file");
		close(score);
		return;
	}

	if (nbytes > 0 && sp < &srecs[nrecs]) {
		if (lseek(score, (int)sp - (int)&srecs[nrecs], 1) < 0) {
			message(ERR, "can't seek in score file");
			close(score);
			return;
		}
	} else {
		sp = srecs;
		if (lseek(score, 0, 2) < 0) {
			message(ERR, "can't seek in score file");
			close(score);
			return;
		}
	}

	sp->human = human;
	sp->computer = computer;
	sp->uid = uid;

	if (write(score, sp, sizeof(struct scorerec))
	    != sizeof(struct scorerec)) {
		message(ERR, "can't write score file");
	}

	close(score);
}
