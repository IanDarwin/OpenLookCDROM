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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/rs_aix3/RCS/getstats.c,v 1.2 1993/07/06 20:51:23 gk5g Exp $";
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

#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <math.h>
#include <nlist.h>
#include <time.h>
#include <sys/time.h>
#include <sys/sysinfo.h>
#include <sys/vminfo.h>
#include <sys/iostat.h>
#include <sys/mntctl.h>
#include <sys/vmount.h>
#include <sys/statfs.h>

#include <getstats.h>

extern int errno, sys_nerr;
extern char *sys_errlist[];

#define sendval(text) {printf text ;fflush(stdout);}

/* vmker struct is kernelstruct (undocumented) */
/* vmker seems to hold some kernels virtual memeory variables */
struct vmker {
  uint n0,n1,n2,n3,n4,n5,n6,n7,n8;
  uint totalmem;
  uint n10;
  uint freemem;
  uint n12,n13;
  uint totalvmem,freevmem;
};

int dk_cnt;  /* number of disks in system  */

#define TICK_D 60.0
#define	INIT_BUF_SIZE 2000

#define NUMBER_OF_KNSTRUCTS 4
#define NLIST_SYSINFO 0
#define NLIST_VMKER 1
#define NLIST_VMINFO 2
#define NLIST_IOSTAT 3
struct nlist kernelnames[] = {
    {"sysinfo", 0, 0, 0, 0, 0},
    {"vmker", 0, 0, 0, 0, 0},
    {"vmminfo", 0, 0, 0, 0, 0},
    {"iostat", 0, 0, 0, 0, 0},
    {NULL, 0, 0, 0, 0, 0},
    };
#define N_VALUE(index) (kernelnames[index].n_value)

int sleep_sec=5;
int dk_skip_interval;
double realtime();

/*
 * Parse command line and start up the data collection command.
 */
static void
Initialize(argc, argv)
    int argc;
    char *argv[];
{
    int dk_sec;

    /*
     * check and parse command line
     */
    if (argc != 4){
    sendval(("%d:%d\n",	PARSE_ERR_1, 0));	/* make sure we have the right # args */
	exit(-1);
    }
    if ((sleep_sec = atoi(argv[2])) < 1){	/* get the collection interval */
	sendval(("%d:%d\n", PARSE_ERR_2, 0));
	exit(-1);
    }
    if ((dk_sec = atoi(argv[3])) < 1){	/* get the disk collection interval */
	sendval(("%d:%d\n", PARSE_ERR_2, 0));
	exit(-1);
    }
    dk_skip_interval = dk_sec / sleep_sec;		/* Close enough for us. */
}

main(argc, argv)
int argc;
char **argv;
{
    struct sysinfo si1,si2;
    struct vmker vmk;
    struct vminfo vm1,vm2;
    double time1,time2;

    Initialize(argc, argv);

    if (knlist(kernelnames,NUMBER_OF_KNSTRUCTS,sizeof(struct nlist)) == -1){
	perror("knlist, entry not found");
    }

    get_sys_vm_info(&si2,&vmk);
    time2 = realtime();
    sleep(1);

    while (1) {
	get_sys_vm_info(&si1,&vmk,&vm1);
	time1 = realtime();
	print_sysinfo(time1-time2,&si1,&si2,&vmk,&vm1,&vm2);
	print_dkstat();
	sleep(sleep_sec);

	get_sys_vm_info(&si2,&vmk,&vm2);
	time2 = realtime();
	print_sysinfo(time2-time1,&si2,&si1,&vmk,&vm2,&vm1);
	print_dkstat();
	sleep(sleep_sec);
    }
    exit(0);
}

double realtime()
{
  struct timeval tp;
  gettimeofday(&tp,0);
  return((double)tp.tv_sec+tp.tv_usec*1.0e-6);
}

/****************************************************************************/
/* get_sys_vm_info(struct sysinfo *si,struct vker *vmk, struct vminfo *vm); */

get_sys_vm_info(si,vmk,vm)
    struct sysinfo *si;
    struct vmker *vmk;
    struct vminfo *vm;
{
    /*
    ** Get the system info structure from the running kernel.
    ** and update loadavg-values 
    ** Get the kernel virtual memory vmker structure
    ** Get the kernel virtual memory info structure
    */
    getkmemdata(si,sizeof(struct sysinfo),N_VALUE(NLIST_SYSINFO));
    getkmemdata(vmk,sizeof(struct vmker),N_VALUE(NLIST_VMKER));
    getkmemdata(vm,sizeof(struct vminfo),N_VALUE(NLIST_VMINFO));
}

/*********************************************************************/
getkmemdata(buf,bufsize,n_value)
char *buf;      /* buffer for kernel data*/
int bufsize;    /* buffers size */
long n_value;    /* seek address to kernels data */
{
    static int fd;
    static initted = 0;
    /*
    ** Do stuff we only need to do once per invocation, like opening
    ** the kmem file and fetching the parts of the symbol table.
    */
    if (!initted) {
	initted = 1;
	fd = open("/dev/kmem", O_RDONLY);
	if (fd < 0) {
	    perror("kmem");
	    exit(1);
	}
    }
    /*
    ** Get the structure from the running kernel.
    */
    lseek(fd, n_value, SEEK_SET);
    read(fd, buf, bufsize);
}

#define SIDELTA(a) (si->a - si2->a)
#define VMDELTA(a) (vm->a - vm2->a)

print_sysinfo(refresh_time,si,si2,vmk,vm,vm2)
    double refresh_time;
    struct sysinfo *si,*si2;
    struct vmker *vmk;
    struct vminfo *vm,*vm2;
{
    long cpuTime, idleTime, userTime, kernalTime, waitTime;
    int swp_proc = 0;

    idleTime = SIDELTA(cpu[CPU_IDLE]);
    userTime = SIDELTA(cpu[CPU_USER]);
    kernalTime = SIDELTA(cpu[CPU_KERNEL]);
    waitTime = SIDELTA(cpu[CPU_WAIT]);
    cpuTime = idleTime + userTime + kernalTime + waitTime;

    /*
     * emit results
     */
    sendval(("%d:%d\n", LOADUSER, 100 * userTime / cpuTime));
    sendval(("%d:%d\n", LOADSYS, 100 * kernalTime / cpuTime));
    sendval(("%d:%d\n", LOADIO, 100 * waitTime / cpuTime));
    sendval(("%d:%d\n", LOADIDLE, 100 * idleTime / cpuTime));
    sendval(("%d:%d\n", LOADCPU, 100 - (100 * idleTime / cpuTime)));
    sendval(("%d:%d\n", VM, 100 * vmk->freevmem / vmk->totalvmem));
    sendval(("%d:%d\n", PAGEIN, VMDELTA(pgspgins)/refresh_time));
    sendval(("%d:%d\n", PAGEOUT, VMDELTA(pgspgouts)/refresh_time));
    sendval(("%d:%d\n", INTSSYS, SIDELTA(syscall)/refresh_time));
    sendval(("%d:%d\n", INTSSWAP, SIDELTA(pswitch)/refresh_time));
    sendval(("%d:%d\n", INTSIO, SIDELTA(devintrs)/refresh_time));
    if(SIDELTA(runocc) > 0) {
	sendval(("%d:%d\n", QUEUERUN, (SIDELTA(runque)/SIDELTA(runocc))-1));
    }
    else {
	sendval(("%d:%d\n", QUEUERUN, 0));
    }
    if (SIDELTA(swpocc) == 0) swp_proc = 0;
    else swp_proc = (int)((double)SIDELTA(swpque)/(double)SIDELTA(swpocc));
    sendval(("%d:%d\n", QUEUEMEM, swp_proc));
}

print_dkstat()
{
    char *buf = NULL;    /* buffer that will hold vmount structs */
    int status = 0, bufSize = INIT_BUF_SIZE;
    struct vmount *vmt = NULL;
    int id = DISK1, dkIndex = 0;
    int count;
   static int dk_skip_count = 9999;

    /* We only do this every "dk_skip_count" time we are called because
     * it is very expensive.
     */
    if (dk_skip_count < dk_skip_interval) {
	dk_skip_count++;
	return;
    }
    dk_skip_count = 0;

    if((buf = (char*) calloc(bufSize, sizeof(char))) == NULL) {
	perror("calloc");
	exit(1);
    }

    while(1) {
	if((status = mntctl(MCTL_QUERY,	bufSize, buf)) < 0) {	/* failure */
	    perror("mntctl");
	    if (buf) free(buf);
	    return(-1);
	}
	else if(status == 0) {	/* Not enough room in buf for all vmounts structs */
	    bufSize = *(int*)buf;
	    if((buf = (char*) realloc(buf, bufSize * sizeof(char))) == NULL) {
		perror("realloc");
		exit(1);
	    }
	}
	else break; /* success */
    }

    vmt = (struct vmount*) buf;
    count = status;
    for (dkIndex = 0; dkIndex < count; dkIndex++) {
	struct statfs StatusBuf;
	char *mountOver;

	mountOver = (char*) calloc(vmt2datasize(vmt,VMT_STUB) + 1, sizeof(char*));
	strncpy(mountOver, vmt2dataptr(vmt, VMT_STUB), vmt2datasize(vmt,VMT_STUB));
	if (strcmp(mountOver, "/afs") != 0) {
	    if(statfs(mountOver, &StatusBuf) < 0) {
		perror("statfs");
		if (buf) free(buf);
		if (mountOver) free(mountOver);
		return(-1);
	    }
	    else {
		int pct = ((double)StatusBuf.f_bfree / (double)StatusBuf.f_blocks) * 100.0;
		sendval(("%d:%d:%s\n", id, 0, mountOver));
		sendval(("%d:%d\n", id, pct));
		id++;
	    }
	}
	vmt = (struct vmount *)((char*)vmt + vmt->vmt_length); 
	if(mountOver) free(mountOver);
    }
    if (buf) free(buf);
    return(0);
}
