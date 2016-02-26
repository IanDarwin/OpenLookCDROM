/*
** Created By:
**
**      Jeremy Huxtable (jh@ist.co.uk)
**
**      Mon Aug 15 12:33:25 BST 1988
**
** mh@wlbr.imsd.contel.com: spruced up a bit.
**
*/

/*
**LIBS: -lcps
*/

/*

newsbiff.c:

    Usage: newsbiff name@host@port name@host@port.....

    Inspired by the program posted by Josh Siegel (siegel@hc.dspo.gov),
    this program will notify you via your NeWS server when new mail
    arrives.  The mail item should be given as standard input.  The
    program opens a connection to the NeWS server (if any) on
    <host.port>, checks that the user is the same as <username>, and
    pops up an icon shaped like an envelope. You can click on this icon
    to get a window displaying the whole mail item.

    To use this you must change two files:

    1) in your "user.ps" you need a line of the form:

	UserProfile /UserName (<name>) put

    and you probably need to disable network security:

	systemdict /NetSecurityWanted false put

    2) in your ".forward" file in your home directory you need a line
    of the form:

	<user>,|"newsbiff user@host@port"

    (you can leave off @host@port part, in which case it will use
    "@localhost@2000" as a default)

    mine's like this:

 mh,|"/nbin/newsbiff mh@awds26@2000"

    NOTE: if you work on many different machines that all have the same
    physical home directory (via NFS) put something like this in your
    .forward file.

 mh@awds26,mh-biff@awds26

    and then put mh-biff in /etc/aliases like so...

 mh-biff: "|/nbin/newsbiff mh@awds26@2000","|/nbin/newsbiff mh@awds14@2000",...

    this way you'll get biff-ed on each machine that you are running a
    NeWS server on. If someone else is running a NeWS server it will
    not biff them as long as they do not have the same UserName in
    their UserProfile PostScript dictionary as you.

    Of course, this is only a "biff" type program. It merely tells you
    that mail has arrived and lets you read it, nothing else.


*/

#ifdef SYSVREF
#ifdef INTERLANTCP
#include <interlan/il_types.h>
#include <interlan/netdb.h>
#include <interlan/in.h>
#else
#include <sys/types.h>
#endif
#else
#include <sys/types.h>
#include <netdb.h>
#include <netinet/in.h>
#endif

#include <psio.h>
#include "newsbiff.h"

#define MAXSTR  512

main(argc, argv)
    int             argc;
    char          **argv;
{
    for(argc--, argv++; argc >= 1; argc--, argv++)
	biffit(*argv);
    exit(0);
}

biffit(spec)
    char *spec;
{
    extern char    *strchr();
    extern long     atol();
    struct hostent *hp;
    char            from[MAXSTR];
    char            line[MAXSTR];
    char            server[MAXSTR];
    char            host[MAXSTR];
    char            username[MAXSTR];
    char           *p;
    int             port = 2000;
    int             count = 0;

    strcpy(username, spec);
    if ((p = strchr(username, '@')))
    {
	*p++ = '\0';
	strcpy(host, p);
    }
    else
	strcpy(host, "localhost");
    if ((p = strchr(host, '@')))
    {
	*p++ = '\0';
	port = atol(p);
    }
    else
	port = 2000;
    if (!(hp = gethostbyname(host)))
    {
	return (1);		/* Host unknown */
    }


    /*
     * * There ought to be a better way of doing this rather than *
     * putting NEWSSERVER into the environment.
     */
    sprintf(server,
      "NEWSSERVER=%lu.2000;%s\n", ntohl(*(u_long *) hp->h_addr), host);
    putenv(server);

    if (!ps_open_PostScript())
	return (0);		/* Can't contact NeWS server */

    ps_username(line);
    if (strcmp(line, username) != 0)
    {
	ps_close_PostScript();
	return (0);		/* User name doesn't match */
    }


    strcpy(from, "Anon");
    ps_begin_list();
    while (fgets(line, 255, stdin))
    {
	line[strlen(line) - 1] = 0;
	if (count++ > 100)
	{
	    ps_string("More follows......");
	    break;
	}
	ps_string(line);
	sscanf(line, "From: %s", from);
    }
    ps_end_list();

    /*- shorten the label on put on the envelope.  just use the
    user@host portion of the mailing address, can the domain stuff -*/

    if ((p = strchr(from, '@')) && (p = strchr(p, '.')))
	*p = '\0';
    ps_mailwindow(from);
    ps_close_PostScript();
    return (0);
}
