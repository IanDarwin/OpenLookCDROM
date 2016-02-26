/*
 * Written by Josh Siegel
 * siegel@hc.dspo.gov
 *
 * Mon Mar 27, 1989
 */

main(argc,argv)
int argc;
char *argv[];
{
	int fildes[2],n;
	char buff[4096];

	pipe(fildes);

	if(fork()) {
		close(fildes[0]);
		while(n=read(0,buff,sizeof(buff))) {
			write(1,buff,n);
			write(fildes[1],buff,n);
		}
	} else {
		close(fildes[1]);
		dup2(fildes[0],0);
		execvp(argv[1],&argv[1]);
	}
}
