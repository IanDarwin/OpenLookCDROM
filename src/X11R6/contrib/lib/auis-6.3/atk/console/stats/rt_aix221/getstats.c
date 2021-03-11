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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/rt_aix221/RCS/getstats.c,v 1.6 1992/12/15 21:31:51 rr2b R6tape $";
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

/*
 * Additional information about the RT/PC AIX implementation.
 * Paul G. Crumley, pgc@andrew.cmu.edu
 *
 * Most of the numbers returned by previous implementations of
 * getstats are pretty meaningless.  There are no conventions
 * for determining the full-scale values for items that are
 * reported by value rather than as a ratio. In addition, the
 * exact datastream is no well specified as in whether file
 * system names may be sent as often as desired or can only be
 * sent once, when the program is initialized.  Finally, the 
 * previous implentations of getstats have to run SUID root or
 * the device /dev/kmem must be set to allow any process read
 * access.
 * 
 * In this implementation I have decided to report only those 
 * statistics that are ratios, report file system names often
 * (since such mappings can change with time) and have used
 * available facilities so that getstats needs no special 
 * protections and system security is not compromised.
 * 
 * The command line is slightly different for the AIX implementation.
 * The command arguments are:
 * 
 *   getstats UID kernel_reporting_rate filesystem_reporting_rate
 *
 * getstats uses the arguments as follows:
 * 
 *   argv[0]="getstats"
 *   argv[1]= IGNORED
 *   argv[2]= reporting rate in seconds
 *   argv[3]=IGNORED
 *
 * 
 * In order to allow getstats to run with out special protections
 * I have chosen to use the SAR facilities to collect kernel 
 * statistics and the "df" command to get information about
 * the filesystems.  These are known system specific commands
 * and facilities.   Just thought you should know....
 */



#include <getstats.h>

#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/times.h>
#include <sys/sysinfo.h>


/*
 * The following information is from manual
 * "Managing the AIX Operating System" SC23-2008.
 */
struct paginginfo {
    unsigned int freeslots;
    unsigned int cycles;
    unsigned int faults;
    unsigned int otherdiskios;
};

struct sa {
    struct sysinfo si;
    struct paginginfo pi;
    int szinode;
    int szfile;
    int sztext;
    int szproc;
    int mszinode;
    int mszfile;
    int msztext;
    int mszproc;
    long inodeovf;
    long fileofv;
    long textofv;
    long procofv;
    time_t ts;
};

/*
 * The above structure (sa) is what is emitted by
 * the sadc command.  When sadc is first invoked
 * two words (32 bits) of zeroes are emmitted to
 * mark the beginning of a new set of data.
 */



#define	SADC_COMMAND	"/usr/lib/sa/sadc"
#define	DF_COMMAND	"/bin/df"

#define	SAMPLES		10000000	    /* run SADC 10,000,000 times (forever) */


/* 
 * Used to emit values for console.
 */
#define sendval(text) {printf text ;fflush(stdout);}



/*
 * global data
 */

FILE *sadc_pipe;	/* pipe kernel stat info is read from */
FILE *df_pipe;		/* pipe filesystem info is read from */
struct sa last_sainfo;	/* last snapshot of various system counters */
struct sa this_sainfo;	/* latest snapshot of various system counters */


/*
 * Parse command line and start up the data collection command.
 */
static void
Initialize(argc, argv)
int argc;
char *argv[];

{
int interval;
int rc;
char sadc_command[1000];

    /*
     * check and parse command line
     */
    if (argc != 4){
    sendval(("%d:%d\n",	PARSE_ERR_1, 0));	/* make sure we have the right # args */
	exit(-1);
    }
    if ((interval = atoi(argv[2])) < 1){	/* get the collection interval */
	sendval(("%d:%d\n", PARSE_ERR_2, 0));
	exit(-1);
    }

    /*
     * start up the data collection process
     */
    (void) sprintf(sadc_command, "%s %d %d", SADC_COMMAND, interval, (SAMPLES+1)); /* one extra for baseline */
    sadc_pipe = popen(sadc_command, "r");
    if (sadc_pipe == NULL) {
	fprintf(stderr, "error starting sadc command\n");
	exit(-1);
    }

    rc = fread(&last_sainfo, sizeof(time_t), 2, sadc_pipe); /* special flag at beginning of data */
    if (rc != 2) {
	fprintf(stderr, "error reading flag\n");
	exit(-1);
    }

    rc = fread(&last_sainfo, sizeof(last_sainfo), 1, sadc_pipe); /* get initial counters */
    if (rc != 1) {
	fprintf(stderr, "error reading sadc output\n");
	exit(-1);
    }
}


/*
 * Get a new set of kernel info, see what has happened since the last
 * time we looked, and report what changed.
 */
static void
EmitKernelStats()
{
long total_time_ticks;
long idle_time_ticks;
long usr_time_ticks;
long kernel_time_ticks;
long wait_time_ticks;


    /*
     * get the newest info
     */
    if (1 != fread(&this_sainfo, sizeof(this_sainfo), 1, sadc_pipe)) {	/* get initial counters */
	fprintf(stderr, "error reading sadc output\n");
	exit(-1);
    }

    /*
     * calculate stats
     */
    idle_time_ticks = this_sainfo.si.cpu[CPU_IDLE] - last_sainfo.si.cpu[CPU_IDLE]; 
    usr_time_ticks = this_sainfo.si.cpu[CPU_USER] - last_sainfo.si.cpu[CPU_USER]; 
    kernel_time_ticks = this_sainfo.si.cpu[CPU_KERNEL] - last_sainfo.si.cpu[CPU_KERNEL]; 
    wait_time_ticks = this_sainfo.si.cpu[CPU_WAIT] - last_sainfo.si.cpu[CPU_WAIT]; 
    total_time_ticks = idle_time_ticks + usr_time_ticks + kernel_time_ticks + wait_time_ticks;

    /*
     * emit results
     */
    sendval(("%d:%d\n", LOADUSER, 100 * usr_time_ticks / total_time_ticks));
    sendval(("%d:%d\n", LOADSYS, 100 * kernel_time_ticks / total_time_ticks));
    sendval(("%d:%d\n", LOADIO, 100 * wait_time_ticks / total_time_ticks));
    sendval(("%d:%d\n", LOADIDLE, 100 * idle_time_ticks / total_time_ticks));
    sendval(("%d:%d\n", LOADCPU, 100 - (100 * idle_time_ticks / total_time_ticks))); /* ??? */

    sendval(("%d:%d\n", PROCSTOTAL, this_sainfo.szproc));

    /*
     * save current info for next time
     */
    (void) bcopy(&this_sainfo, &last_sainfo, sizeof(this_sainfo));
}


/*
 * Invoke the df command to get the current stats on the mounted filesystems.
 */
static void
EmitFilesystemStats()
{
int id;	    /* each filesystem has a number in the datastream */
int i;	    /* % of filesystem that is used */
char name[1000];    /* name of filesystem mount point */

    df_pipe = popen(DF_COMMAND, "r");
    if (df_pipe == NULL) {
	sendval(("%d:%d\n", DISK_ERR_3,	0));	/* some generic, non-fatal error */
	return;
    }

    if (EOF == fscanf(df_pipe, "%*[^\n]\n")) {	/* toss heading */
	(void) pclose(df_pipe);
	sendval(("%d:%d\n", DISK_ERR_3,	0));	/* some generic, non-fatal error */
	return;
    }

    id = DISK1;
    while (EOF != fscanf(df_pipe, "%*s %s %*d %*d %d%*[^\n]\n", name, &i)  && i < MAXGETSTATSCOUNTERS) {
	sendval(("%d:%d:%s\n", id, 0, name));
	sendval(("%d:%d\n", id, i));
	id++;
    }

    (void) pclose(df_pipe);	/* close this to recover resources */
}

/*
 *
 */
int
main(argc, argv)
int argc;
char *argv[];
{
int count; 

    Initialize(argc, argv);

    for (count = 0; count < SAMPLES; count++) {
	EmitKernelStats();
	EmitFilesystemStats();
    }
    exit(0);
}

