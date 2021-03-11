/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
static char *getstats_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/i386_Linux/RCS/getstats.c,v 1.1 1993/10/05 22:14:29 gk5g Exp $";

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/i386_Linux/RCS/getstats.c,v 1.1 1993/10/05 22:14:29 gk5g Exp $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/i386_Linux/RCS/getstats.c,v $ */

#ifndef lint
static char *getstats_c_id = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/i386_Linux/RCS/getstats.c,v 1.1 1993/10/05 22:14:29 gk5g Exp $";
#endif /* lint */

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
#include <fcntl.h>

#define	DF_COMMAND	"/bin/df"


/* 
 * Used to emit values for console.
 */
#define sendval(text) {printf text ;fflush(stdout);}



/*
 * global data
 */

FILE *df_pipe;		/* pipe filesystem info is read from */


/*
 * Invoke the df command to get the current stats on the mounted filesystems.
 * NOTE: This is probably a bad move, as it everytime df runs, it syncs.
 * oh well.
 */

GetDiskStats(Init)
int Init;
{
int id;	    /* each filesystem has a number in the datastream */
int i;	    /* % of filesystem that is used */
char name[1000];    /* name of filesystem mount point */

    df_pipe = popen(DF_COMMAND, "r");
    if (df_pipe == NULL) {
	sendval(("%d:%d\n", DISK_ERR_3,	0));	/* some generic, */
						/* non-fatal error */ 
	return;
    }

    if (EOF == fscanf(df_pipe, "%*[^\n]\n")) {	/* toss heading */
	(void) pclose(df_pipe);
	sendval(("%d:%d\n", DISK_ERR_3,	0));	/* some generic, */
						/* non-fatal error */ 
	return;
    }

    id = DISK1;
    while (EOF != fscanf(df_pipe, "%*s %*d %*d %*d %d%% %s%*[^\n]\n",
			 &i, name
			 )  && id < MAXGETSTATSCOUNTERS) { 
	sendval(("%d:%d:%s\n", id, 0, name));
	sendval(("%d:%d\n", id, i));
	id++;
    }

    (void) pclose(df_pipe);	/* close this to recover resources */
}

GetGVMStats(UsersID)
int UsersID;
{
  float a1, a5, a15;
  char loadav_str[250];
  int fd;

  fd = open("/proc/loadavg", O_RDONLY);
  if (fd >=  0) {
    lseek(fd, 0, SEEK_SET);
    read(fd, loadav_str, 249);
    loadav_str[249] = 0;
  
    sscanf(loadav_str, "%f %f %f", &a1,&a5, &a15);
    sendval(("%d:%d\n", LOADCPU, (int) (100.0 * a1) ));
    close(fd);
  }
  top_main();
}

InitGVMStats()
{
  top_main();
}
