/* -*-Mode: C; -*- */
#ifndef _TTY_CLIX_INCLUDE
#define _TTY_CLIX_INCLUDE

/* tty_clix.i,v 1.2 1994/05/27 06:22:07 me Exp */

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/*
 * Tty manipulation routines for Intergraph CLIX machines.
 *
 * Author: Steven S. Crooks
 * Date: December 25th, 1991.
 * Description: Various routines for tweaking ttys on Intergraph CLIX systems
 *		that the Emu system expects us to declare.
 *
 */

/* Mandatory */
#ifndef _TTY_INCLUDED
#define _TTY_INCLUDED
#endif

#include <stdlib.h>
#include <sys/termio.h>
#include <utmp.h>
#include <pwd.h>
#include <errno.h>

Import struct utmp *getutid();

/*
 * File descriptors to try.
 */
Local int Fd_try[] = {
	0, 1, 2, -1, 0, -1 };

char clix_ptyname[20];


/*
 * Search for an available pty, putting descriptor in passed "descr" and
 * returning name, if found. Since we're only called once (better be), it's
 * OK to stick this name in static storage.
 *
 * The process code is expected to take care of the hairy mode/process
 * group/controlling tty/etc. setup stuff later when the child is forked
 * off, so we don't do it now.
 *
 */
Export String
tty_find_pty(w)
TermWidget w;
{
	char ttyname[80];
	struct stat st;
	register char *c1, *c2;
	Import void tty_set_nonblocking();
 
#ifdef DEBUG
debug("-- Entering tty_find_pty() --");
#endif	
	for (c1 = PTYCHAR1; *c1; ++c1) {
		for (c2 = PTYCHAR2; *c2; ++c2) {
			/*
			 * Go for the master side.
			 */
			if (sprintf(ttyname, PTYDEV, *c1, *c2) < 0)
				fatal("sprintf failed in %s line %d", __FILE__,
				      __LINE__);
#ifdef DEBUG
			debug("tty_find_pty: Trying to open %s", ttyname);
#endif
			if (stat(ttyname, &st) == -1)
				continue;
			if ((w->term.master = open(ttyname, O_RDWR | O_NDELAY,
						   0)) >= 0) {
#ifdef DEBUG
				debug("tty_find_pty: Opened master %s on fd %d",
				      ttyname, w->term.master);
#endif
				(void)strcpy(clix_ptyname, ttyname);

				/*
				 * Now make sure that the slave side is
				 * available.  This allows for a bug in some
				 * versions of rlogind and gives us a handle
				 * for ioctl() ops.
				 */
				(void)sprintf(ttyname, TTYDEV, *c1, *c2);
#ifdef DEBUG
				debug("unix_tty_find_pty: Checking slave side %s", ttyname);
#endif
				if ((w->term.slave = open(ttyname, O_RDWR, 0)) >= 0) {
					/*
					 * It works, so close it back up and
					 * move on.  We'll open it for real
					 * when we fork so that it is the
					 * control terminal.
					 */
#ifdef DEBUG
					debug("unix_tty_find_pty: Found pty %s", ttyname);
#endif
					if (close(w->term.slave) == -1)
						warn("Couldn't close the slave pty: %s",
						     strerror(errno));
					return XtNewString(ttyname);
				}
				else {
					close(w->term.master);
				}
			}
		}
	}
	pmessage("No free ptys.");
	return NULL;
}


/*
 * Set a new tty size.
 */
/*ARGSUSED*/
Export void
tty_set_size(w, rows, cols, width, height)
TermWidget w;
int rows, cols, width, height;
{
#ifdef DEBUG
	debug("-- Entering tty_set_size() --");
#endif
#ifdef TIOCSWINSZ
	struct winsize wz;
	int pgrp;

	wz.ws_row = rows;
	wz.ws_col = cols;
	wz.ws_xpixel = width;
	wz.ws_ypixel = height;
	if (ioctl(w->term.slave, TIOCSWINSZ, &wz) == -1)
		warn("Couldn't set terminal size: %s", strerror(errno));
#ifdef SIGWINCH
	if (ioctl(w->term.slave, TIOCGPGRP, &pgrp) != -1)
		kill(-pgrp, SIGWINCH);
	else
		warn("Couldn't send the SIGWINCH signal: %s", sterror(errno));
#endif
#endif
}


/*
 * Add a utmp/wtmp entry for a tty.
 */
Export void tty_add_utmp(w)
TermWidget w;
{
	struct passwd *pw;
	struct utmp utmp;
	char *name;

#ifdef DEBUG
	debug("-- Entering tty_add_utmp() --");
#endif
	/*
	 * Reset before searching.
	 */
	setutent();

	/*
	 * Copy the last two chars over (e.g. "ttyp0" -> "p0") for
	 * /etc/inittab lookup.
	 */
	(void)strncpy(utmp.ut_id, w->term.tname + strlen(w->term.tname) - 4,
		      sizeof(utmp.ut_id));

	/*
	 * Look for slot of former process.
	 */
	utmp.ut_type = DEAD_PROCESS;

	/*
	 * Position us at the entry.
	 */
	(void)getutid(&utmp); /* return value doesn't seem to work */

	/*
	 * Build a new entry there.
	 */
	
	/*
	 * Who's there?
	 */
	pw = getpwuid(w->term.uid);

	/*
	 * We're somebody...
	 */
	utmp.ut_type = USER_PROCESS;
	utmp.ut_exit.e_exit = 2;
	utmp.ut_exit.e_termination = 0;

	/*
	 * Or are we?
	 */
	name = (pw && pw->pw_name) ? pw->pw_name : "(unknown)";
	
	/*
	 * Copy over name information.
	 */
	(void)strncpy(utmp.ut_user, name, sizeof(utmp.ut_user));
	(void)strncpy(utmp.ut_id, w->term.tname + strlen(w->term.tname) - 4,
		      sizeof(utmp.ut_id));
	(void)strncpy(utmp.ut_line, w->term.tname + strlen("/dev/"),
		      sizeof(utmp.ut_line));
	(void)strncpy(utmp.ut_name, name, sizeof(utmp.ut_name));

	/*
	 * pid and current time.
	 */
	if ((utmp.ut_pid = getpid()) == -1)
		fatal("Can't get pid in %s line %d: %s", __FILE__, __LINE__,
		      strerror(errno));
#ifdef DEBUG
	debug("tty_add_utmp:  Adding entry for pid %d, line %s", utmp.ut_pid,
	      utmp.ut_line);
#endif
	if ((utmp.ut_time = time(NULL)) == -1)
		warn("time() failed in %s line %d: %s", __FILE__, __LINE__,
		     strerror(errno));

	/*
	 * Write it out.
	 */
	pututline(&utmp);

	/*
	 * Close up shop.
	 */
	endutent();
}


/*
 * Remove a utmp/wtmp entry for a tty.
 */
/*ARGSUSED*/
Export void tty_remove_utmp(w)
TermWidget w;
{
	struct utmp utmp, *utptr;
	
#ifdef DEBUG
	debug("-- Entering tty_remove_utmp() --");
#endif
	/*
	 * Time to clean up the old utmp entry we made.
	 */
	utmp.ut_type = USER_PROCESS;
	(void)strncpy(utmp.ut_id, w->term.tname + strlen(w->term.tname) - 4,
		      sizeof(utmp.ut_id));
	setutent();

	utptr = getutid(&utmp);

#ifdef DEBUG
	debug("tty_remove_utmp: this pid = %d, utmp pid = %d, term.pid = %d",
	      getpid(), utptr->ut_pid, w->term.pid);
#endif
	/*
	 * Write it out only if it exists and the pid's match.
	 */
	if (utptr && (utptr->ut_pid == w->term.pid)) {
		/*
		 * Write obituary for the deceased.
		 */
		utptr->ut_type = DEAD_PROCESS;
		utptr->ut_time = time((long *)0);

		/*
		 * Inform the grieving relatives.
		 */
		pututline(utptr);
	} else
		warn("Couldn't close the utmp entry");
	
	/*
	 * Close the casket and go home (jeeze this is morbid!).
	 */
	endutent();
}


/*
 * Sets a terminfo structure to "sane" values.
 */
void
clix_set_sane(tio)
struct termio *tio;
{
	/* Set the values to the same as "stty sane".  It's only fair. */

	/* set up terminal input control */
	tio->c_iflag &= ~(IGNBRK|PARMRK|INPCK|INLCR|IGNCR|IUCLC|IXOFF);
	tio->c_iflag |= BRKINT|IGNPAR|ISTRIP|ICRNL|IXON;

	/* set up output treatment */
	tio->c_oflag &= ~(OLCUC|OCRNL|ONOCR|ONLRET|OFILL|OFDEL|NLDLY|CRDLY|
			 TABDLY|BSDLY|VTDLY|FFDLY);
	tio->c_oflag |= OPOST|ONLCR;

	/* set up hardware control */
	tio->c_cflag &= ~(CSIZE|PARODD|CLOCAL);
	tio->c_cflag |= CS8|CREAD;
	
	/* set up line discipline */
	tio->c_lflag &= ~(XCASE|ECHOE|ECHONL|NOFLSH);
	tio->c_lflag |= ISIG|ICANON|ECHO|ECHOK;

	/* set up special characters */
	tio->c_cc[VERASE] = CERASE;
	tio->c_cc[VKILL] = CKILL;
	tio->c_cc[VQUIT] = CQUIT;
	tio->c_cc[VINTR] = CINTR;
	tio->c_cc[VEOF] = CEOF;
	tio->c_cc[VEOL] = CNUL;
}


/*
 * These next three routines complement each other by getting and setting
 * some number of tty values using a private (and probably machine dependent)
 * format. It is up to the implementor of these routines to devise some
 * suitable data structure for getting, setting and properly storing said
 * values for their particular operating system. The term widget will
 * encode these values as "generic" and leave it to the implementor to keep
 * them straight.
 *
 */


/*
 * Get the current settings for tty.
 */
Export Generic
tty_get_values(w)
TermWidget w;
{
	register int i, *try;
	static struct termio origtty, ret;
	struct termio *thistty;

#ifdef DEBUG
	debug("-- Entering tty_get_values() --");
#endif
	if (w == NULL) {
		try = Fd_try;
		thistty = &origtty;
	} else {
		try = Fd_try + 4;
		try[0] = w->term.master;
		thistty = &ret;
	}
	for (i = 0; try[i] >= 0; i++)
		if (ioctl(try[i], TCGETA, thistty) != -1)
			break;

	/*
	 * Massage things a bit, if these are previous settings.
	 */
	if (w == NULL) {
		thistty->c_iflag &= ~(INLCR|IGNCR);
		thistty->c_iflag |= ICRNL;
		thistty->c_oflag &= ~(OCRNL|ONLRET|NLDLY|CRDLY|TABDLY|
				      BSDLY|VTDLY|FFDLY);
		thistty->c_oflag |= ONLCR;
		thistty->c_lflag |= ISIG|ICANON|ECHO;
		thistty->c_cc[VEOF] = CEOF;
		thistty->c_cc[VEOL] = CNUL;
	}

	/*
	 * If there was no terminal to base tty settings off of, they should
	 * be set to some sane values.
	 */
	if (try[i] == -1) {
		clix_set_sane(thistty);

		/*
		 * Make sure the baud rate is set to a reasonable 9600.
		 */
		thistty->c_cflag &= ~(CBAUD);
		thistty->c_cflag |= B9600;
	}

	return (Generic)thistty;
}


/*
 * Must return some "sane" set of hardwired values.
 */
Export Generic
tty_get_sane(w)
TermWidget w;
{
	static struct termio tio;
     
#ifdef DEBUG
	debug("-- Entering tty_get_sane() --");
#endif
	/* Grab current values */
	bcopy(tty_get_values(w), &tio, sizeof(tio));

	clix_set_sane(&tio);

	return (Generic)&tio;
}

/*
 * Whap some settings onto the tty.
 */
Export void
tty_set_values(w, val)
TermWidget w;
Generic val;
{
	register int i, *try;
	struct termio *tio = (struct termio *)val;

#ifdef DEBUG
	debug("-- Entering tty_set_values() --");
#endif
	if (w == NULL)
		try = Fd_try;
	else {
		try = Fd_try + 4;
		try[0] = w->term.master;
	}
	for (i = 0; try[i] >= 0; i++)
		if (ioctl(try[i], TCSETA, tio) != -1)
				break;

	if (try[i] == -1)
		warn("Couldn't change terminal's stty settings: %s",
		     strerror(errno));
}


/*ARGSUSED*/
Export void
tty_set_nonblocking(w, fd)
TermWidget w;
int fd;
{
	int mode = 1;

#ifdef DEBUG
	debug("-- Entering tty_set_nonblocking() --");
#endif
#if 1
	if ((mode = fcntl(fd, F_GETFL, 0)) == -1)
		fatal("Couldn't get initial pty mode: %s", strerror(errno));
	mode |= O_NDELAY;
	if (fcntl(fd, F_SETFL, mode) == -1)
		warn("Coudn't set pty mode: %s", strerror(errno));
#else
	if (ioctl(fd, FIONBIO, (char *)&mode) == -1)
		fatal("Couldn't set pty nonblocking: %s", strerror(errno));
#endif
}

#endif	/* _TTY_CLIX_INCLUDE */
