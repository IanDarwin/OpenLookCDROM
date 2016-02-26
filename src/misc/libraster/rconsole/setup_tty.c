/*
 * rconsole
 * Copyright 1990 Tom Lawrence, Brown University
 * Last Modification 6/14/90
 */

#include <sys/types.h>
#include <stdio.h>

#include "global.h"

#include <fcntl.h>
#include <signal.h>
#include <utmp.h>
#include <pwd.h>
#include <string.h>
#include <strings.h>

char utmp[] = "/etc/utmp";
char tty[] = "/dev/tty";

extern char *getenv(), *ttyname();

/****************************************************************************
set up the utmp entry. If clear is true, clear it.
****************************************************************************/
void
setup_utmp(ttyfd, clear)
	int ttyfd, clear;
{
	FILE *fp;
	int offset;
	struct utmp utmp_entry;
	int old_fd0;
	struct passwd *user;
	char *ttynm, *ch;

	if ((user = getpwuid(getuid())) == 0) {
		fputs("rconsole: who are you??\n", stderr);
		return;
	}
	if ((fp = fopen(utmp, "r+")) == NULL) {
		fputs("rconsole: fopen: ", stderr);
		perror(utmp);
		exit(1);
	}

	/* YUCK! ttyslot only checks fd 0. What a hack. */
	old_fd0 = dup(0);
	dup2(ttyfd, 0);
	offset = ttyslot() * sizeof(struct utmp);

	/* zip to the proper file offset in /etc/utmp */
	fseek(fp, offset, 0);
	fread((char *)&utmp_entry, sizeof(struct utmp), 1, fp);

	if (clear)
		memset((char *) &utmp_entry, 0, sizeof(struct utmp));

	else {
		/* get name of this tty device (0 is still a dup of ttyfd) */
		ttynm = ttyname(0);

		/* if full tty pathname, we only want last element */
		if (ch = rindex(ttynm, '/'))
			ch++;
		else
			ch = ttynm;

		strncpy(utmp_entry.ut_name, user->pw_name, 8);
		strncpy(utmp_entry.ut_line, ch, 8);
#ifdef notdef
		strncpy(utmp_entry.ut_host, "rconsole", 8);
#endif
		utmp_entry.ut_time = time(0);
	}

	/* zip back to correct offset and write the new entry */
	fseek(fp, offset, 0);
	fwrite((char *) (&utmp_entry), sizeof(struct utmp), 1, fp);
	fclose(fp);

	/* blecch revisited. */
	dup2(old_fd0, 0);
	close(old_fd0);
}

/****************************************************************************
get a pty/tty pair and initialize them.
****************************************************************************/
tty_pair
init_tty(maxrow, maxcol)
	int maxrow, maxcol;
{
#ifdef SUNOS4
	struct termios termios;
#else
	struct sgttyb sgttyb;
	struct tchars tchars;
	struct ltchars ltchars;
	int lmode;
#endif
	struct winsize winsize;
	tty_pair pair;
	int pid, x, fd;
	char ch, name[10], *comm;
	struct passwd *user;

	/* grab the first free pty. */

	for (ch = 'p'; ch <= 'r'; ch++) {
		for (x = 0; x < 16; x++) {
			sprintf(name, "/dev/pty%c%x", ch, x);
			if ((pair.pty = open(name, O_RDWR)) != -1)
				goto gotone;
		}
	}
	fputs("rconsole: no available ptys\n", stderr);
	exit(1);

gotone:
	name[5] = 't';		/* change from pty to tty */

	/* Grab tty modes now before fd's get twisted up. */
#ifdef SUNOS4
	if (tcgetattr(0, &termios) < 0) {
		perror("rconsole: tcgetattr");
		exit(1);
	}
#else
	if (ioctl(0, TIOCGETP, &sgttyb) < 0) {
		perror("rconsole: TIOCGETP");
		exit(1);
	}
	if (ioctl(0, TIOCGETC, &tchars) < 0) {
		perror("rconsole: TIOCGETC");
		exit(1);
	}
	if (ioctl(0, TIOCGLTC, &ltchars) < 0) {
		perror("rconsole: TIOCGLTC");
		exit(1);
	}
	if (ioctl(0, TIOCLGET, &lmode) < 0) {
		perror("rconsole: TIOCLGET");
		exit(1);
	}
#endif

	/* create a child to run the shell (or exec command) */
	if ((pid = fork()) < 0) {
		perror("rconsole: fork");
		exit(1);
	}
	if (pid == 0) {
		/* Add a few things to the environment */
		(void) putenv("TERM=sun");
		(void) putenv("TCONSOLE=");

		/* disassociate from controlling tty */
		if ((fd = open(tty, O_RDWR)) < 0) {
			fputs("rconsole: open: ", stderr);
			perror(tty);
			exit(1);
		}
		if (ioctl(fd, TIOCNOTTY) < 0) {
			perror("rconsole: TIOCNOTTY");
			exit(1);
		}
		close(fd);

		/* open our tty, making it our controlling tty */
		if ((pair.tty = open(name, O_RDWR)) < 0) {
			fputs("rconsole: open: child: ", stderr);
			perror(name);
			exit(1);
		}

		/* catch console output */
		if (ioctl(pair.tty, TIOCCONS) < 0) {
			perror("rconsole: TIOCCONS");
			exit(1);
		}
		/* set up initial stdin, stdout and stderr */
		dup2(pair.tty, 0);

		dup2(pair.tty, 1);
		dup2(pair.tty, 2);

		close(pair.pty);

		/* run the shell or other command */

		if (EXEC) {
			execvp(*EXEC, EXEC);
			fputs("rconsole: execvp: ", stderr);
			perror(*EXEC);
			exit(1);
		} else {
			if ((user = getpwuid(getuid())) == 0) {
				fputs("rconsole: who are you??\n", stderr);
				exit(1);
			}
			if (comm = rindex(user->pw_shell, '/'))
				comm++;
			else
				comm = user->pw_shell;

			execl(user->pw_shell, comm, 0);
			fputs("rconsole: execl: ", stderr);
			perror(user->pw_shell);
			exit(1);
		}
	}

	/*
	 * Grab the tty. This was done separately in the child since it was to
	 * become the controlling tty there, but not here.
	 */
	if ((pair.tty = open(name, O_RDWR)) < 0) {
		fputs("rconsole: open: parent: ", stderr);
		perror(name);
		exit(1);
	}

	/* copy terminal modes from the real console to the new tty */
#ifdef SUNOS4
	if (tcsetattr(pair.tty, TCSANOW, &termios) < 0) {
		perror("rconsole: tcsetattr");
		exit(1);
	}
#else
	if (ioctl(pair.tty, TIOCSETP, &sgttyb) < 0) {
		perror("rconsole: TIOCSETP");
		exit(1);
	}
	if (ioctl(pair.tty, TIOCSETC, &tchars) < 0) {
		perror("rconsole: TIOCSETC");
		exit(1);
	}
	if (ioctl(pair.tty, TIOCSLTC, &ltchars) < 0) {
		perror("rconsole: TIOCSLTC");
		exit(1);
	}
	if (ioctl(pair.tty, TIOCLSET, &lmode) < 0) {
		perror("rconsole: TIOCLSET");
		exit(1);
	}
#endif

	/* set up the terminal's window size */
	winsize.ws_col = maxcol;
	winsize.ws_row = maxrow;

	if (ioctl(pair.tty, TIOCSWINSZ, &winsize) < 0) {
		perror("rconsole: TIOCSWINSZ");
		exit(1);
	}
	return (pair);
}
