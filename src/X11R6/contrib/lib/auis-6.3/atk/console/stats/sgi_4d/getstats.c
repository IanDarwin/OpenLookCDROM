/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/sgi_4d/RCS/getstats.c,v 1.1 1994/02/28 05:09:48 rr2b Exp $";
#endif

/* **********************************************************************
*   This code is designed to read what might be priveledged (setuid) 
*   information regarding both Disk Statistics (% full) and a host of 
*   stats from /dev/kmem (including but not limited to, CPU, I/O, and VM)
*
*   When retriving the data - this program will print out to stdout
*   a string in the form of either "%d:%d\n" or "%d:%d:%s\n"
*   The latter case is for passing the name of where a disk is mounted
*   back to the parent program.
*
*   The parent program (Console, or any other program which wishes to get
*   at this information) is responsible for setting up a pipe, binding the
*   child's (this program) stdout to one end of a pipe, and parsing the
*   strings which are passed back.
*
*   The basic string format is an ID (int), a colon, a value (int), and
*   optionally another colon followed by a string.  The ID is coded from
*   the included file "getstats.h" - ID values 50 and over represent 
*   ERRORS as documented in the above mentioned inclued file.  When an 
*   ERROR or the optional string is passed, the value (second parameter)
*   can be safely ignored, and is usually set to 0.
*
*   The arguments to be passed to this program are the effective UID from
*   the parent program, a polling frequency (# of seconds) for checking
*   /dev/kmem (usually between 1 and 5, must be > 0), and a polling
*   frequency for checking how full the local disks are (generally higher
*   than the value for /dev/kmem, but could vary greatly).  Thus the call
*   is:
*
*   execvp("getstats", argv)
*
*   with argv as:
*
*   argv[0]="getstats";
*   argv[1]=~ ("%d", UID);
*   argv[2]=~ ("%d", kmempollfrequency);
*   argv[3]=~ ("%d", diskpollfrequency);
*   argv[4]=NULL;
*
********************************************************************** */
#include <andrewos.h>
#include <sitevars.h>

#include <stdio.h>
#include <sys/param.h>

#include <getstats.h>
#define sendval(text) {printf text ;fflush(stdout);}

GetGVMStats(UsersID)
int UsersID;
{
   
    sendval(("%d:%d\n", LOADCPU, 0));
    sendval(("%d:%d\n", LOADIO, 0));
    sendval(("%d:%d\n", LOADUSER, 0));
    sendval(("%d:%d\n", LOADSYS, 0));
    sendval(("%d:%d\n", LOADIDLE,0));
    sendval(("%d:%d\n", VM, 0));
    sendval(("%d:%d\n", PAGEIN, 0));
    sendval(("%d:%d\n", PAGEOUT, 0));
    sendval(("%d:%d\n", PAGEREPLACABLE, 0));
    sendval(("%d:%d\n", PAGEDEFICIT, 0));
    sendval(("%d:%d\n", MEMACTIVE, 0));
    sendval(("%d:%d\n", MEMFREE, 0));
    sendval(("%d:%d\n", QUEUERUN, 0));
    sendval(("%d:%d\n", QUEUEBLOCK, 0));
    sendval(("%d:%d\n", QUEUEMEM, 0));
    sendval(("%d:%d\n", INTSIO, 0));
    sendval(("%d:%d\n", INTSSYS, 0));
    sendval(("%d:%d\n", INTSSWAP, 0));
   
    sendval(("%d:%d\n", PROCSUSER, 0));
    sendval(("%d:%d\n", PROCSTOTAL, 0));
    sendval(("%d:%d\n", PROCSOTHER, 0));
}





InitGVMStats()
{
   
}

GetDiskStats(Init)
int Init;
{
}

