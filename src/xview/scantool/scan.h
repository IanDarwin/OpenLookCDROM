
/*  @(#)scan.h 1.3 90/04/03
 *
 *  Definitions used by scan.
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

#define  CLOSE       (void) close      /* To make lint happy. */
#define  FCLOSE      (void) fclose
#define  FPRINTF     (void) fprintf
#define  FWRITE      (void) fwrite
#define  GTTY        (void) gtty
#define  IOCTL       (void) ioctl
#define  PUTC        (void) putc
#define  READ        (void) read
#define  SELECT      (void) select
#define  SIGNAL      (void) signal
#define  SSCANF      (void) sscanf
#define  SPRINTF     (void) sprintf
#define  STRCPY      (void) strcpy
#define  STRNCPY     (void) strncpy
#define  STTY        (void) stty
#define  SYSTEM      (void) system
#define  UNLINK      (void) unlink
#define  WRITE       (void) write

char *malloc(), *mktemp() ;

#define  BUFSIZE        512     /* RS232 data buffer size. */
#define  ETX            3       /* End of scanning code for scanner. */
#define  INC            argc-- ; argv++ ;
#define  MAXLINE        80      /* Length of character strings. */
#define  MAXREAD        100     /* Maximum amount of outstanding data. */
#define  MAXRETRIES     5       /* Number of attempts to send packet. */
#define  MAXTIME        2       /* RS232 read timeout in seconds. */
#define  SOR            '\\'    /* Start of record character. */
#define  XOFF           19      /* Stop sending data. */
#define  XON            17      /* Start sending data again. */
#define  X1             0       /* Coordinate values within framevals. */
#define  Y1             1
#define  X2             2
#define  Y2             3

/* RS232 read automation states. */
#define  RSSOR       0            /* Start of packet. */
#define  RSRTYPE     1            /* Record type. */
#define  RSCOUNT     2            /* Data count. */
#define  RSDATA      3            /* Packet data. */
#define  RSCKSUM     4            /* Packet checksum. */

/* States for packet acknowledgement. */
#define  NOANSWER    '-'        /* No acknowledgement received. */
#define  ACK         6          /* Positive acknowledgement. */
#define  NAK         21         /* Negative acknowledgement. */

/* Abbreviations for box switch values. */
#define  MODE           switches[0]
#define  DATA_TRANSFER  switches[1]
#define  SERIAL_PORT    switches[2]
#define  BAUD_RATE      switches[3]

/* Color definitions used with uncompressing. */
#define  WHITE          4
#define  BLACK          2

#ifndef  SIGRET
#define  SIGRET         void
#endif /* SIGRET */

#ifndef LINT_CAST
#ifdef lint
#define LINT_CAST(arg)  (arg ? 0 : 0)
#else
#define LINT_CAST(arg)  (arg)
#endif lint
#endif LINT_CAST

struct code                     /* Huffman code record. */
         {
           struct code *next[2] ;
           int value[2] ;
         } ;
