/*
 * notify_input.c -- use notify_set_input_func to monitor the state of
 * a file.  The notifier is running and checking the file descriptors
 * of the opened files associated with the command line args.  The
 * routine installed by notify_set_input_func() is called whenever
 * there is data to be read.  When there is no more data to be read
 * for that file, the input function is unregistered.  When all files
 * have been read, notify_start() returns and the program exits.
 */
#include <stdio.h>
#ifdef SVR4
#include <sys/stat.h>
#else
#include <sys/ioctl.h>
#endif /* SVR4 */
#include <xview/notify.h>

main(argc, argv)
char *argv[];
{
    Notify_value   read_it();
    Notify_client  client = (Notify_client)10101; /* arbitrary */
    FILE           *fp;

    while (*++argv)
        if (!(fp = fopen(*argv, "r")))
            perror(*argv);
        else {
            (void) notify_set_input_func(client, read_it, fileno(fp));
            client++; /* next client is new/unique */
        }

    /* loops continuously */
    notify_start();
}

/*
 * read_it() is called whenever there is input to be read.  Actually,
 * it's called continuously, so check to see if there is input to be
 * read first.
 */
Notify_value
read_it(client, fd)
Notify_client   client;
int fd;
{
    char buf[BUFSIZ];
    int bytes, i;

    if (input_available(fd) < 1)
        (void) notify_set_input_func(client, NOTIFY_FUNC_NULL, fd);
    else
        do
            if ((i = read(fd, buf, sizeof buf)) > 0)
                (void) write(1, buf, i);
        while (i > 0 && (bytes -= i) > 0);
    return NOTIFY_DONE;
}

int input_available(fd)
int fd;
{
 
   int bytes = -1;
 
#ifdef SVR4
   struct stat statbuf;
   int retval;
 
   if ( ( retval = fstat( fd, &statbuf ) ) >= 0 ) {
      if ( S_ISREG( statbuf.st_mode ) ) {
         if ( ( retval = lseek( fd, 0, SEEK_CUR ) ) >= 0 )
            bytes = statbuf.st_size - retval;
      }  
   }
#else
   if (ioctl(fd, FIONREAD, &bytes) == -1 || bytes == 0)
      bytes = -1;
#endif
 
   return( bytes );
}
