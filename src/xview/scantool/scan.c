
/*  @(#)scan.c 1.4 90/04/04
 *
 *  Program which will read a scanned image from a Microtek
 *  MS-300A scanner and convert it to a Sun rasterfile.
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me, then an attempt will be made to fix them.
 */

#include <stdio.h>
#include <strings.h>
#include <signal.h>
#include <sgtty.h>
#include <rasterfile.h>
#include <sys/time.h>
#include <sys/types.h>
#include "patchlevel.h"
#include "scan.h"

char cksum ;               /* Packets checksum. */
char cntbytes ;            /* Number of count bytes received. */
char finalimage[MAXLINE] ; /* Name of uncompressed image file. */
char from_rs[BUFSIZE] ;    /* Data received from the RS232. */
char line[MAXLINE] ;
char picname[MAXLINE] ;    /* Picture name for rasterfile output. */
char progname[MAXLINE] ;   /* Name of this program. */
char rscksum ;             /* RS232 read packet checksum. */
char rsdata[BUFSIZE] ;     /* Latest packet to/from the scanner. */
char rsrhold = XON ;       /* RS232 host hand shaking state. */
char rsrstate = RSSOR ;    /* State of RS232 read automation. */
char rsrtype ;             /* Record type received. */
char rswstate ;            /* RS232 write acknowledgement state. */
char scantype[MAXLINE] ;   /* Scanner details. */
char temphead[MAXLINE] ;   /* Temporary filename for header file. */
char tempimage[MAXLINE] ;  /* Temporary filename for saved scanned data. */

int brightness ;           /* Brightness value. */
int cancel ;               /* Indicates if should cancel current scan. */
int complete ;             /* Indicates if scanning is completed. */
int contrast ;             /* Contrast value. */
int error ;                /* Has an error report has been received? */
int fd = -1 ;              /* File descriptor for RS232 line. */
int finished ;             /* Indicates if we have finished uncompressing. */
int fwidth ;               /* Final width of rasterfile. */
int height ;               /* Height in scan lines of raster file image. */
int pic_compressed = 0 ;   /* Whether any picture data was compressed. */
int pkt_started = 0 ;      /* Is a data packet coming from the scanner? */
int pktdone ;              /* Indicates if a complete packet received. */
int rc ;                   /* Current character from scanned file. */
int rcount ;               /* Count of bit position within read character. */
int rscnt ;                /* Number of data bytes to read. */
int rsptr ;                /* Character buffer pointer. */
int wc ;                   /* Current character to write to final image file. */
int wcount ;               /* Count of bit position within write character. */
int width ;                /* Width in pixels of rasterfile image. */
int grain ;                /* Grain value. */
int resolution ;           /* Resolution value. */
int retval ;               /* This programs exit status. */
int rf ;                   /* File descriptor for raster image. */

int switches[4] =
      {
        0,                 /* Mode (Line Art). */
        1,                 /* Data Transfer (Compressed). */
        0,                 /* Serial Port (A). */
        1                  /* Baud Rate (19200). */
      } ;

int framevals[4] =            /* Initial frame in 1/8th inch intervals. */
      {
        16,                /* X1. */
        16,                /* Y1. */
        48,                /* X2. */
        48,                /* Y2. */
     } ;

FILE *rd ;                 /* File descriptor for input files. */
FILE *wd ;                 /* File descriptor for final image. */

#ifdef NO_4.3SELECT
int readmask ;                /* File descriptors with outstanding reads. */
#else
fd_set readmask ;             /* Readmask used in select call. */
#endif NO_4.3SELECT

struct timeval timeout = { MAXTIME, 0 } ;

struct code *whites[16] ;   /* White initial starting pointers. */
struct code *blacks[4] ;    /* Black initial starting pointers. */


/*  The scan program can exit is a variety of ways. These are:
 *
 *  0          - successful   - image saved in picname.
 *  1          - unsuccessful - cannot open ttyline.
 *  2          - unsuccessful - cannot open temporary image file.
 *  3          - unsuccessful - cannot open raster header file.
 *  4          - unsuccessful - scanner not responding. Aborting this scan.
 *  5          - unsuccessful - invalid command line argument.
 *  100 + 0xnn - unsuccessful - scanning error 0xnn received. Scan aborted.
 *  200 + n    - unsuccessful - scan program terminated with signal n.
 *
 *  It is upto the calling program or shell script to display the
 *  appropriate message back to the user.
 */

main(argc, argv)
int argc ;
char *argv[] ;
{
  retval = 0 ;
  set_signals() ;              /* Set up a signal handler. */
  STRCPY(progname, argv[0]) ;  /* Save program name. */
  get_options(argc, argv) ;    /* Extract scanning parameters. */
  do_scan() ;                  /* Perform a scan using given parameters. */
  exit(retval) ;
}


SIGRET
do_exit(sig)   /* Signal received; exit gracefully. */
int sig ;
{
  char outval ;

  if (fd != -1)     /* Terminate the scan properly, if started. */
    {
      outval = ETX ;
      WRITE(fd, &outval, 1) ;
      CLOSE(fd) ;
    }
  exit(200 + sig) ;
}


do_handshake(value)            /* Send RS232 handshake XON/XOFF. */
char value ;
{
  WRITE(fd, &value, 1) ;
  rsrhold = value ;
}


do_scan()                      /* Perform a scan using given parameters. */
{
  char c ;                     /* Next character for RS232 output. */

  pic_compressed = 0 ;
  height = 0 ;
  error = 0 ;                           /* Set to 1 on error. */
  if (open_temp_file() < 0) return ;    /* Open file for raster image. */
  if (init_rs_port(SERIAL_PORT, BAUD_RATE) < 0) return ;
 
/* Set scanning parameters. */
  if (make_packet('!', 1, 0) < 0) return ;    /* Get scanner model number. */
  if (make_packet('X', 1, 0) < 0) return ; ;  /* Reset scanning parameters. */

  if (make_packet('C', 2, DATA_TRANSFER) < 0) return ;

  if (MODE)
    {
      if (make_packet('H', 2, 0) < 0) return ;
    }
  else if (make_packet('T', 2, 0) < 0) return ;

  if (make_packet('G', 2, grain) < 0) return ;       
  if (make_packet('B', 2, brightness) < 0) return ; 
  if (make_packet('K', 2, contrast) < 0) return ;  
  if (make_packet('R', 2, resolution+16) < 0) return ;

  if (make_frame_packet() < 0) return ;
  if (make_packet('S', 1, 0) < 0) return ;     /* Send a start scan packet. */ 
  complete = 0 ;
  cancel = 0 ;
  for (;;)
    {
 
#ifdef NO_4.3SELECT
      readmask = 1 << fd ;
      SELECT(32, &readmask, (int *) 0,(int *) 0, &timeout) ;
      if (readmask & (1 << fd))
#else
      FD_ZERO(&readmask) ;
      FD_SET(fd, &readmask) ;
      SELECT(FD_SETSIZE, &readmask, (fd_set *) 0, (fd_set *) 0, &timeout) ;
      if (FD_ISSET(fd, &readmask))
#endif NO_4.3SELECT

        read_rs232() ;
      else
        { 
          if (rsrhold == XOFF) do_handshake(XON) ;
          if (pkt_started)
            {
              c = NAK ;
              WRITE(fd, &c, 1) ;
              rsrstate = RSSOR ;
              pkt_started = 0 ;
            }
        }    
      if (complete || error || cancel) break ;
    }
  CLOSE(rf) ;                     /* Close temporary image file. */
  if (!error)
    {
      save_picture() ;            /* Create Sun rasterfile picture. */
      retval = 0 ;                /* Successful exit - image saved. */
    }
  else UNLINK(tempimage) ;
  CLOSE(fd) ;
}


get_options(argc, argv)        /* Extract command line options. */
int argc ;
char *argv[] ;
{
  char next[MAXLINE] ;    /* The next command line parameter. */

  INC ;
  while (argc > 0)
    {
      if (argv[0][0] == '-')
        switch (argv[0][1])
          {
            case 'c' : INC ;                         /* Contrast. */
                       getparam(next, argv, "-c needs contrast value") ;
                       contrast = atoi(next) ;
                       break ;
            case 'b' : INC ;                         /* Brightness. */
                       getparam(next, argv, "-b needs brightness value") ;
                       brightness = atoi(next) ;
                       break ;
            case 'd' : INC ;                         /* Data transfer. */
                       getparam(next, argv, "-d needs data transfer value") ;
                       switches[DATA_TRANSFER] = atoi(next) ;
                       break ;
            case 'f' : INC ;                         /* Frame. */
                       getparam(next, argv, "-f needs X1 coordinate") ;
                       framevals[X1] = atoi(next) ;
                       INC ;
                       getparam(next, argv, "-f needs Y1 coordinate") ;
                       framevals[Y1] = atoi(next) ;
                       INC ;
                       getparam(next, argv, "-f needs X2 coordinate") ;
                       framevals[X2] = atoi(next) ;
                       INC ;
                       getparam(next, argv, "-f needs Y2 coordinate") ;
                       framevals[Y2] = atoi(next) ;
                       break ;
            case 'g' : INC ;                         /* Grain. */
                       getparam(next, argv, "-g needs grain value") ;
                       grain = atoi(next) ;
                       break ;
            case 'm' : INC ;                         /* Mode. */
                       getparam(next, argv, "-m needs mode value") ;
                       switches[MODE] = atoi(next) ;
                       break ;
            case 'p' : INC ;                         /* Picture name. */
                       getparam(picname, argv, "-p needs picture name") ;
                       break ;
            case 'r' : INC ;                         /* Resolution. */
                       getparam(next, argv, "-r needs resolution value") ;
                       resolution = atoi(next);
                       break ;
            case 's' : INC ;              /* Speed of RS232 connection. */
                       getparam(next, argv, "-s needs speed value") ;
                       switches[BAUD_RATE] = atoi(next) ;
                       break ;
            case 't' : INC ;                         /* Tty port. */
                       getparam(next, argv, "-t needs tty port") ;
                       switches[SERIAL_PORT] = atoi(next) ;
                       break ;
            case 'v' : FPRINTF(stderr, "%s version 1.4.%1d\n",
                                       progname, PATCHLEVEL) ;
                       break ;
            case '?' : usage() ;
          }
      INC ;
    }
}


getparam(s, argv, errmes)
char *s, *argv[], *errmes ;
{
  if (*argv != NULL && argv[0][0] != '-') STRCPY(s, *argv) ;
  else
    { 
      FPRINTF(stderr,"%s: %s as next argument.\n", progname, errmes) ;
      exit(5) ;
    }
}


init_rs_port(port, speed)              /* Initialise RS232 port. */
int port, speed ;
{
  char ttyline[MAXLINE] ;
  struct sgttyb sgttyb ;
 
  if (port) STRCPY(ttyline, "/dev/ttyb") ;
  else      STRCPY(ttyline, "/dev/ttya") ;
  if ((fd = open(ttyline,2)) < 0)
    {
      retval = 1 ;      /* Cannot open ttyline. */
      return(-1) ;
    }
  GTTY(fd, &sgttyb) ;
  sgttyb.sg_flags |= RAW ;
  sgttyb.sg_flags &= ~(ECHO | CRMOD) ;
  if (speed) sgttyb.sg_ispeed = sgttyb.sg_ospeed = EXTA ;
  else       sgttyb.sg_ispeed = sgttyb.sg_ospeed = B9600 ;
  STTY(fd, &sgttyb) ;             /* Implement tty line setup changes. */
  return(0) ;
}


make_frame_packet()        /* Construct and send scanning frame packet. */
{
  char cksum ;             /* For creating the packet checksum. */
  int i ;

  rsdata[0] = '\\' ;       /* Construct packet. */
  rsdata[1] = 0x80 ;
  rsdata[2] = 0x00 ;
  rsdata[3] = 5 ;
  rsdata[4] = 'F' ;
  rsdata[5] = framevals[X1] ;
  rsdata[6] = framevals[Y1] ;
  rsdata[7] = framevals[X2] ;
  rsdata[8] = framevals[Y2] ;
  cksum = 0 ;
  for (i = 1; i < 9; i++) cksum += rsdata[i] ;
  rsdata[9] = (~cksum) + 1 ;
  return(send_packet('F', 10)) ;
}


make_header(width, height)       /* Make Sun rasterfile header. */
int width, height ;
{
  struct rasterfile header ;
  FILE *hd ;                    /* File descriptor for header file. */

  SPRINTF(temphead, "/usr/tmp/%s.header", mktemp("XXXXXX")) ;
  if ((hd = fopen(temphead,"w")) == NULL)
    {
      retval = 3 ;              /* Can't open raster header file. */
      return(-1) ;
    }
  header.ras_magic = 0x59a66a95 ;        /* Generate rasterfile header. */
  header.ras_width = width ;
  header.ras_height = height ;
  header.ras_depth = 1 ;
  header.ras_length = width * height ;
  header.ras_type = RT_STANDARD ;
  header.ras_maptype = RMT_RAW ;
  header.ras_maplength = 0 ;
  FWRITE((char *) &header, sizeof(struct rasterfile), 1, hd) ;
  FCLOSE(hd) ;
  return(0) ;
}


make_packet(ptype, count, value)   /* Sent a packet to the scanner. */
char ptype, count, value ;
{
  char cksum ;                     /* For creating the packet checksum. */
  int i ;
  int len ;                        /* Length of this scanner packet. */

  len = count + 5 ;
  rsdata[0] = '\\' ;               /* Construct packet. */
  rsdata[1] = 0x80 ;
  rsdata[2] = 0x00 ;
  rsdata[3] = count ;
  rsdata[4] = ptype ;
  if (count == 2) rsdata[5] = value ;
  cksum = 0 ;
  for (i = 1; i < len-1; i++) cksum += rsdata[i] ;
  rsdata[len-1] = (~cksum) + 1 ;
  return(send_packet(ptype, len)) ;
}


open_temp_file()          /* Open a temporary file for the scanned image. */
{
  SPRINTF(tempimage, "/usr/tmp/%s.image", mktemp("XXXXXX")) ;
  if ((rf = open(tempimage, 2)) < 0)
    if ((rf = creat(tempimage, 0777)) < 0)
      {
        retval = 2 ;   /* Can't open temporary image file. */
        return(-1) ;
      }
  return(0) ;
}


process_packet(ptype)     /* Process RS232 packet received. */
char ptype ;
{
  int errorval ;          /* To extract error value. */
  char outval ;           /* ACK or NAK value to be sent. */

  if (rscksum == cksum)
    {
      outval = ACK ;
      switch (ptype)
        {
          case 'D' : if ((rsrtype >> 5) & 1) pic_compressed++ ;
                     height++ ;
                     if (!((rsrtype >> 5) & 1)) rsptr = ((rsptr >> 2) << 2) ;
                     WRITE(rf, from_rs, rsptr) ;
                     width = rsptr * 8 ;
                     break ;
          case 'E' : outval = ETX ;    /* Complete scanning and eject paper. */
                     WRITE(fd, &outval, 1) ;
                     complete = 1 ;
                     break ;
          case '!' : STRNCPY(scantype, from_rs, rsptr) ;
                     break ;
          case '?' : errorval = (from_rs[1] & 0xFF) ;
                     retval = 100 + errorval ;  /* Scanning error received. */
                     printf("Error=0x%x",errorval);
                     outval = ETX ;
                     WRITE(fd, &outval, 1) ;
                     error = 1 ;
        }
    }    
  else outval = NAK ;
  WRITE(fd, &outval, 1) ;        /* Send ACK or NAK. */
  pktdone = 1 ;
}


read_rs232()      /* Read upto end of next packet. */
{
  char c ;        /* Next character read from RS232. */
  int count ;     /* Number of RS232 characters to read. */
  int i ;

  IOCTL(fd,(int) FIONREAD,(char *) &count) ;   /* Any data to read. */
  if (count > MAXREAD) do_handshake(XOFF) ;
  else if (count < 20 && rsrhold == XOFF) do_handshake(XON) ;
  if (!count) return ;                         /* NO: exit routine. */
  for (i = 0; i < count; i++)
    {
      READ(fd, &c, 1) ;                        /* Read next character. */
      if (rsrstate == RSSOR && (c == ACK || c == NAK)) rswstate = c ;
      else
        switch (rsrstate)
          {
            case RSSOR   : rscksum = 0 ;
                           if (c == SOR) rsrstate = RSRTYPE ;
                           break ;
            case RSRTYPE : rsrtype = c ;
                           rscksum += c ;
                           cntbytes = 0 ;
                           rscnt = 0 ;
                           rsrstate = RSCOUNT ;
                           break ;
            case RSCOUNT : rscksum += c ;
                           rscnt = rscnt*256 + c ;
                           if (++cntbytes == 2)
                             {
                               rsrstate = RSDATA ;
                               rsptr = 0 ;
                             }
                           pkt_started = 1 ;
                           break ;
            case RSDATA  : from_rs[rsptr++] = c ;  /* Save data character. */
                           rscksum += c ;
                           if (!--rscnt) rsrstate = RSCKSUM ;
                           break ;
            case RSCKSUM : rscksum = (~rscksum) + 1 ;
                           cksum = c ;
                           if (rsrtype >> 7) process_packet(from_rs[0]) ;
                           else process_packet('D') ;
                           pkt_started = 0 ;
                           rsrstate = RSSOR ;
                           return ;
          }
    }
}


save_picture()    /* Combine header file and image to make a Sun rasterfile. */
{
  char line[MAXLINE] ;          /* For constructing the system command. */

  if (pic_compressed)
    {
      initialise_white("white.codes") ;
      initialise_black("black.codes") ;
      uncompress(tempimage) ;
    }
  else
    { 
      if (!make_header(width, height))   /* Write out Sun rasterfile header. */
        {
          SPRINTF(line, "cat %s %s > %s", temphead, tempimage, picname) ;
          SYSTEM(line) ;                 /* Create Sun rasterfile. */
          UNLINK(temphead) ;             /* Remove header file. */
        }
      UNLINK(tempimage) ;                /* Remove image file. */
    }
}


send_packet(ptype, len)
char ptype,len ;
{
  int done ;        /* Packet read or ACK/NAK found. */
  int i ;

  for (i = 0; i < MAXRETRIES; i++)
    {
      WRITE(fd, rsdata, len) ;
      rswstate = NOANSWER ;
      pktdone = 0 ;
      done = 0 ;
      do
        {
#ifdef NO_4.3SELECT
          readmask = 1 << fd ;
          SELECT(32, &readmask, (int *) 0, (int *) 0, &timeout) ;
          if (readmask & (1 << fd))
#else
          FD_ZERO(&readmask) ;
          FD_SET(fd, &readmask) ;
          SELECT(FD_SETSIZE, &readmask, (fd_set *) 0, (fd_set *) 0, &timeout) ;
          if (FD_ISSET(fd, &readmask))
#endif NO_4.3SELECT
            {
              read_rs232() ;
              switch (ptype)
                {
                  case '!' : if (pktdone) done = 1 ;
                             break ;
                  default  : if (rswstate == ACK || rswstate == NAK) done = 1 ;
                }
              if (done) break ;
            }
          else break ;
        }
      while (!done) ;
      if (rswstate == ACK) return(0) ;
    }
  retval = 4 ;   /* Scanner not responding. Aborting this scan. */
  CLOSE(rf) ;
  return(-1) ;
}


set_signals()   /* Setup a signal handler for a range of signals. */
{
  SIGNAL(SIGHUP,  do_exit) ;
  SIGNAL(SIGQUIT, do_exit) ;
  SIGNAL(SIGILL,  do_exit) ;
  SIGNAL(SIGTRAP, do_exit) ;
  SIGNAL(SIGIOT,  do_exit) ;
  SIGNAL(SIGEMT,  do_exit) ;
  SIGNAL(SIGFPE,  do_exit) ;
  SIGNAL(SIGBUS,  do_exit) ;
  SIGNAL(SIGSEGV, do_exit) ;
}


usage()
{
  FPRINTF(stderr, "Usage: %s: [-b brightness] [-c contrast] ", progname) ;
  FPRINTF(stderr, "[-d] [-f x1 y1 x2 y2] [-g grain] [-m] [-p picname]\n") ;
  FPRINTF(stderr, "[-s] [-t] [-v] [-?] [-Wi] [-Wp x y] [-WP x y]\n") ;
  exit(5) ;
}
