#include	"mille.h"
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<sys/time.h>
# ifdef	attron
#	include	<term.h>
#	define	_tty	cur_term->Nttyb
# endif	attron

/*
 * @(#)save.c	1.4 (Berkeley) 7/3/83
 */

typedef	struct stat	Stat;
typedef	struct tm	Time;

char	*ctime();

int	read(), write();

/*
 *	This routine saves the current game for use at a later date
 */
extern int	errno;
extern char	*sys_errlist[];

save() {

	reg char	*sp, *ans;
	char		*GetpromptedInput ();
	reg int		outf;
	reg Time	*tp;
	char		buf[80];
	Time		tme;
	Stat		junk;

	tp = &tme;
	if (Fromfile && getyn("Same file? "))
		strcpy(buf, Fromfile);
	else {
over:
		ans = GetpromptedInput ("File: ");
		if (!ans)
		    return FALSE;
		strcpy (buf, ans);
		sp = buf + strlen (buf);
	}

	/*
	 * check for existing files, and confirm overwrite if needed
	 */

	if (sp == buf || (!Fromfile && stat(buf, &junk) > -1
	    && getyn("Overwrite File? ") == FALSE))
		return FALSE;

	if ((outf = creat(buf, 0644)) < 0) {
		error(sys_errlist[errno]);
		return FALSE;
	}
	Error (buf);
	time(tp);			/* get current time		*/
	strcpy(buf, ctime(tp));
	for (sp = buf; *sp != '\n'; sp++)
		continue;
	*sp = '\0';
	varpush(outf, write);
	close(outf);
	return TRUE;
}

/*
 *	This does the actual restoring.  It returns TRUE if the
 * backup was made on exiting, in which case certain things must
 * be cleaned up before the game starts.
 */
rest_f(file)
reg char	*file; {

	reg char	*sp;
	reg int		inf;
	char		buf[80];
	Stat		sbuf;

	if ((inf = open(file, 0)) < 0) {
		perror(file);
		exit(1);
	}
	if (fstat(inf, &sbuf) < 0) {		/* get file stats	*/
		perror(file);
		exit(1);
	}
	varpush(inf, read);
	close(inf);
	strcpy(buf, ctime(&sbuf.st_mtime));
	for (sp = buf; *sp != '\n'; sp++)
		continue;
	*sp = '\0';
	/*
	 * initialize some necessary values
	 */
	sprintf(Initstr, "%s [%s]\n", file, buf);
	Fromfile = file;
	return !On_exit;
}
