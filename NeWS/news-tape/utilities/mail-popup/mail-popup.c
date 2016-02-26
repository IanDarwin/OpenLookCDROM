/* Written by Josh Siegel */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>

main (argc, argv)
    int             argc;
    char           *argv[];
{
    char            from[255],
                    subject[255],
                    cc[255],
                    to[255],
                    tmp[255],
                    buff[255];
    int             s,
                    n;

    if (argc != 3 && argc != 4)
	exit (0);

    bzero (cc, sizeof (cc));

    strcpy (from, "From: \\(null\\)\n");
    strcpy (subject, "Subject: \\(null\\)\n");
    strcpy (to, "To: \\(null\\)\n");

    while (fgets (buff, 255, stdin) != NULL)
    {
	if (!strncmp (buff, "From:", 5))
	    strcpy (from, buff);
	if (!strncmp (buff, "Cc:", 3))
	    strcpy (cc, buff);
	if (!strncmp (buff, "To:", 3))
	    strcpy (to, buff);
	if (!strncmp (buff, "Subject:", 8))
	    strcpy (subject, buff);
    }

    if (argc == 4)
	s = phone (atoi (argv[3]), argv[1]);
    else
	s = phone (2000, argv[1]);

    if (s < 0)
	exit (0);

    write (s, "UserName ==\n", 12);
    read (s, buff, sizeof (buff));
    if (strncmp (buff, argv[2], strlen (argv[2])))
	exit (0);
    srandom (time ((long *) 0));
    if (cc[0])
    	sprintf (tmp, "(%s) (%s) (%s) (%s)", from, to, subject, cc);
    else
    	sprintf (tmp, "(%s) (%s) (%s)", from, to, subject);

    sprintf (buff, "%d %d [%s] popmsg pop\n", random () % 400, (random () % 700) + 100, tmp);
    n = strlen (buff);
    write (s, buff, n);
    close (s);
    exit (0);
}
phone (service, host)
    int             service;
    char           *host;
{

    struct sockaddr_in sin;
    struct hostent *hp;
    int             s;
    bzero ((char *) &sin, sizeof (sin));

    sin.sin_port = htons (service);

    hp = gethostbyname (host);
    if (hp == NULL)
	return (-1);
    bcopy (hp->h_addr, (char *) &sin.sin_addr, hp->h_length);
    sin.sin_family = hp->h_addrtype;
    s = socket (AF_INET, SOCK_STREAM, 0);
    if (s < 0)
	return (-1);

    if (connect (s, &sin, sizeof (sin)) < 0)
	return (-1);

    return (s);
}
