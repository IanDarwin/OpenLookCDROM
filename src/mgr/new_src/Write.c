/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/Write.c,v 1.4 91/03/01 11:05:41 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/Write.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/Write.c,v $$Revision: 1.4 $";

/* Long writes to ptty's don't always work */

#include <errno.h>

#define MAX_RETRY	3		/* max retries after EWOULDBLOCK */
#define TTYMAX		100		/* max chunk size in write */
#define Min(x,y)	((x)<(y)?(x):(y))

extern errno;

int
Write(fd,buff,len)
register int fd, len;
register char *buff;
   {
   register int count = 0;
   register int code;
   register int retry=0;

   while (count < len) {
      code = write(fd,buff+count,Min(TTYMAX,len-count));
      if (code > 0)
         count += code;
      else if (errno == EWOULDBLOCK) {
         if (retry++ > MAX_RETRY)
            break;
         sleep(1);
         continue;
         }
      else 
         break;
      }
   return(count);
   }
