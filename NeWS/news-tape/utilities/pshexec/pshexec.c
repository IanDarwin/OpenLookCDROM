
/*
 * Written by Josh Siegel
 * siegel@hc.dspo.gov
 *
 * Mon Mar 27, 1989
 *
 * Does ${0-9} substitution as well as $@
 *
 */


#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

char cwd[MAXPATHLEN];

main(argc, argv)
	int             argc;
	char           *argv[];
{
	int             ns;


	ns = open_news();
	if (ns < 0) {
		perror("phone");
		exit(0);
	}


	dump_out(ns, argv[1],argv,argc);

	dup2(ns, 1);
	dup2(ns, 0);
	execvp(argv[2], &argv[2]);
}
dump_out(news, file,argv,argc)
	int             news,argc;
	char           *file;
	char *argv[];
{
	struct stat     fbuf;
	int             n, fd;
	register char           *buf,*p,*p2;

	getwd(cwd);

	n = stat(file, &fbuf);	/* st_size */

	if (n != 0) {
		perror("stat");
		exit(0);
	}
	buf = (char *) malloc(fbuf.st_size);

	fd = open(file, O_RDONLY);
	if (fd < 0) {
		perror(file);
		exit(0);
	}
	n=read(fd, buf, fbuf.st_size);
	buf[n] = '\0';
	close(fd);

	p2 = p = buf;
	while(1) {
		while(*p2!='$' && *p2!='\0')
			p2++;
		write(news, p, p2-p);
		if(*p2=='\0')
			break;
		p2++;
		if(*p2>='0' && *p2 <= '9') {
			if(*p2-'0'+2 < argc)
				write(news,argv[*p2-'0'+2],strlen(argv[*p2-'0'+2]));
		}
		else
			switch(*p2) {
				case '@':
					write(news,cwd,strlen(cwd));
					break;
				default:
					write(news,p2-1,2);
					break;
			}
		p2++;
		p = p2;
	}

	free(buf);
}
/*
	Phone:  Connects to a established port setup by accept().
*/
open_news()
{
	char           *p, *p2, *p3;
	int             news;

	p = (char *) getenv("NEWSSERVER");

	if (p) {
		p2 = (char *) index(p, '.') + 1;
		p3 = (char *) index(p2, ';');
		*p3++ = '\0';
		news = phone(atoi(p2), p3);
	} else
		news = phone(2000, "localhost");

	if (news < 0) {
		perror("phone");
		exit(0);
	}
	return (news);
}
phone(port, host)
	int             port;
	char            host[];
{

	struct sockaddr_in sin;
	struct servent *sp;
	struct hostent *hp;
	int             s, *p;
	char            buff[255];

	bzero((char *) &sin, sizeof(sin));

	sin.sin_port = port;

	hp = gethostbyname(host);

	if (hp == NULL)
		return (-1);

	bcopy(hp->h_addr, (char *) &sin.sin_addr, hp->h_length);
	sin.sin_family = hp->h_addrtype;
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0)
		return (-1);

	if (connect(s, &sin, sizeof(sin)) < 0) {
		close(s);
		return (-1);
	}
	return (s);
}
