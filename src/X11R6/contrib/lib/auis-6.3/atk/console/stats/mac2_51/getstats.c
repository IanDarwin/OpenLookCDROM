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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/mac2_51/RCS/getstats.c,v 1.7 1993/09/23 20:35:09 gk5g Exp $";
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
#include <stdio.h>
#include <sys/param.h>
#include <sys/user.h> 
#include <sys/proc.h>
#include <sys/vm.h>
#include <sys/dk.h>
#include <sys/map.h>
#include <nlist.h>
#include <netinet/in.h>
#include <errno.h>
#include <sys/stat.h>
#include <fstab.h>
#include <mtab.h>
#include <sys/fs.h>
#include <mach.h>
#include <sys/table.h>
#include <sys/msg_type.h>
#include "sitevars.h"
#define VMMON_DODECL
#include "getstats.h"


struct mtab mtab[NMOUNT];
char	root[32];
int	fi;
daddr_t	alloc();
char	*strcpy();

struct vm_statistics	vm_stat, last;
int	percent;
int	load_average;
unsigned short	nUserProcs;

char	*pgmname;

int MemoryFile /* file descriptor for unix memory */ ;
extern struct nlist RawStatistics[];
long stime[CPUSTATES], s1time[CPUSTATES];
long TotalTime;

#define sendval(text) {printf text ;fflush(stdout);}
#define min(a, b) (((a) <= (b)) ? (a) : (b))

union {
    struct fs iu_fs;
    char dummy[SBSIZE];
} sb;

#define sblock sb.iu_fs

get_cpuload () 
{
    int i, j;
    long t;

    lseek(MemoryFile,(long) RawStatistics[X_CPTIME].n_value, 0);
    read(MemoryFile, stime, sizeof(stime));

    for (i = 0; i < CPUSTATES; i++) {
	t = stime[i];
	stime[i] -= s1time[i];
	s1time[i] = t;
    }
    stime[1] += stime[0];
    TotalTime = 0;
    for (i = 1; i < CPUSTATES; i++)
	TotalTime += stime[i];
    if (TotalTime == 0)
	TotalTime = 1;
}

get_stats(stat, userId)
	struct vm_statistics	*stat;
	int userId;
{
        struct tbl_loadavg	avenrun;
	double scale;
	double run;

	(void) table(TBL_LOADAVG, 0, (char *)&avenrun, 1, sizeof(avenrun));
	(void) table(TBL_MAXUPRC,
		     0, (char *)&nUserProcs, 1, sizeof(nUserProcs));

	if (vm_statistics(current_task(), stat) != KERN_SUCCESS) {
		fprintf(stderr, "%s: failed to get statistics.\n", pgmname);
		exit(2);
	}
	if (stat->lookups == 0)
		percent = 0;
	else
		percent = (stat->hits*100)/stat->lookups;
}

InitGVMStats() 
{
   time_t bootime;
   int code = 0;

   /* 
    set up Unix interface: scan name list for current system
    addresses and then open a file which is the memory image
    of the system.
    */
   code = nlist(_SITE_VMUNIX, RawStatistics);
   if (code == -1){
       sendval(("%d:%d\n", GVM_ERR_2, 0));
       exit(-1);
   }
   else{
       if (RawStatistics[0].n_type == 0){
	   sendval(("%d:%d\n", GVM_ERR_3, 0));
	   exit(-1);
       }
   }
   MemoryFile = open(_SITE_DEV_KMEM, 0);
   if (MemoryFile < 0){
       sendval(("%d:%d\n", GVM_ERR_4, 0));
       exit(-1);
   }
    bzero(&last, sizeof(last));
}



GetGVMStats(UsersID)
int UsersID;
{
    get_cpuload();
    get_stats(&vm_stat, UsersID);
    sendval(("%d:%d\n", PAGEIN, vm_stat.pageins - last.pageins));
    sendval(("%d:%d\n", PAGEOUT, vm_stat.pageouts - last.pageouts));
    sendval(("%d:%d\n", MEMACTIVE, vm_stat.active_count));
    sendval(("%d:%d\n", PAGEREPLACABLE, vm_stat.inactive_count));
    sendval(("%d:%d\n", MEMFREE, vm_stat.free_count));

    sendval(("%d:%d\n", PAGESIZE, vm_stat.pagesize));
    sendval(("%d:%d\n", PAGESWIRED, vm_stat.wire_count));
    sendval(("%d:%d\n", PAGEFAULTS, vm_stat.faults - last.faults));
    sendval(("%d:%d\n", PAGECOWFAULTS, vm_stat.cow_faults - last.cow_faults));
    sendval(("%d:%d\n", PAGEZEROFILLS, 
	     vm_stat.zero_fill_count - last.zero_fill_count));
    sendval(("%d:%d\n", PAGEREACTIVATES, 
	     vm_stat.reactivations - last.reactivations));
    sendval(("%d:%d\n", PAGECACHEHITS, vm_stat.hits));
    sendval(("%d:%d\n", PAGELOOKUPS, vm_stat.lookups));
    sendval(("%d:%d\n", PAGEHITRATE, percent));

    sendval(("%d:%d\n", LOADCPU, (stime[1] + stime[2]) * 100 / TotalTime));
    sendval(("%d:%d\n", LOADUSER, stime[1] * 100 / TotalTime));
    sendval(("%d:%d\n", LOADSYS, stime[2] * 100 / TotalTime));
    sendval(("%d:%d\n", LOADIDLE, stime[3] * 100 / TotalTime));
    sendval(("%d:%d\n", PROCSUSER, nUserProcs));
    last = vm_stat;

#if 0
    sendval(("%d:%d\n", LOADIO, myval));
    sendval(("%d:%d\n", LOADUSER, stime[1] * 100 / TotalTime));
    sendval(("%d:%d\n", LOADSYS, stime[2] * 100 / TotalTime));
    sendval(("%d:%d\n", LOADIDLE, stime[3] * 100 / TotalTime));
    sendval(("%d:%d\n", VM, nSwapBlks ? CurrentSwapUsed*100 / nSwapBlks: -1));
    sendval(("%d:%d\n", PAGEDEFICIT, deficit));
    sendval(("%d:%d\n", QUEUERUN, total.t_rq));
    sendval(("%d:%d\n", QUEUEBLOCK, total.t_dw + total.t_pw));
    sendval(("%d:%d\n", QUEUEMEM, total.t_sw));
    sendval(("%d:%d\n", INTSIO, rate.v_intr));
    sendval(("%d:%d\n", INTSSYS, rate.v_syscall));
    sendval(("%d:%d\n", INTSSWAP, rate.v_swtch));

    sendval(("%d:%d\n", PROCSUSER, userprocesses * 100 / MAXUPRC));
    sendval(("%d:%d\n", PROCSTOTAL, nproc ? totalprocesses * 100 / nproc : -1));
    sendval(("%d:%d\n", PROCSOTHER, otherprocs));
#endif
}

void Sleep_Msecs(msecs)
    unsigned int    msecs;
{
    static sleepPort = PORT_NULL;
    msg_header_t    m;
    msg_return_t    mr;

    if (sleepPort == PORT_NULL) {
	port_allocate(task_self(),&sleepPort);
    }

    m.msg_size = sizeof(msg_header_t);
    m.msg_local_port = sleepPort;
    mr = msg_receive(&m,RCV_TIMEOUT,msecs);
}

/* the DeviceTable keeps a list of all the devices (and their mounted directory file name) that we should watch */

extern int getmnt();

GetDiskStats(Init)
int Init;
{
    int i = 0;
    struct fstab *fsp;

    i = open(_SITE_MTAB, 0);
    if (i >= 0) {
	(void) read(i, (char *)mtab, sizeof (mtab));
	(void) close(i);
    }
    sync();
    if (setfsent() == 0){
	sendval(("%d:%d\n", DISK_ERR_5, 0));
	exit(1); 
    }
    i = DISK1 - 1; /* figuratively 0 */
    while (fsp = getfsent() && i < MAXGETSTATSCOUNTERS) {
	i++;
	if (strcmp(fsp->fs_type, FSTAB_RW) &&
	    strcmp(fsp->fs_type, FSTAB_RO) &&
	    strcmp(fsp->fs_type, FSTAB_RQ))
	    continue;
	if (root[0] == 0){
	    (void) strcpy(root, fsp->fs_spec);
	}
	dfree1(i, fsp->fs_spec, 1, Init);
    }
    endfsent();
}

int bread(fi, bno, buf, cnt)
int fi;
daddr_t bno;
char *buf;
int cnt;
{
    register int n;
    extern int errno;

    (void) lseek(fi, (long)(bno * DEV_BSIZE), 0);
#ifndef sun
    if ((n=read(fi, buf, cnt)) != cnt)
#else sun
	if ((n = read(fi, buf, (unsigned) cnt)) < 0)
#endif sun
	{
	    /* probably a dismounted disk if errno == EIO */
	    if (errno != EIO) { 
		sendval(("%d:%d\n", DISK_ERR_5, 0));
	    }
	    return (0);
	}
    return (1);
}


/*
 * Given a name like /dev/rrp0h, returns the mounted path, like /usr.
 */
char *mpath(file)
char *file;
{
    register struct mtab *mp;

    if (eq(file, root)){
	return ("/");
    }
    for (mp = mtab; mp < mtab + NMOUNT; mp++){
	if (eq(file, mp->m_dname)){
	    return (mp->m_path);
	}
    }
    return "";
}

eq(f1, f2)
char *f1, *f2;
{
    if (strncmp(f1, "/dev/", 5) == 0)
	f1 += 5;
    if (strncmp(f2, "/dev/", 5) == 0)
	f2 += 5;
    if (!strcmp(f1, f2))
	return (1);
    if (*f1 == 'r' && !strcmp(f1+1, f2))
	return (1);
    if (*f2 == 'r' && !strcmp(f1, f2+1))
	return (1);
    if (*f1 == 'r' && *f2 == 'r' && strcmp(f1+1, f2+1) == 0)
	return (1);
    return (0);
}

int round(num)
double num;
{
    int inum = (int) num;
    return(((num - inum) >= 0.5) ? (inum + 1) : inum);
}


dfree1(id, file, infsent, Init)
int id;
char *file;
int infsent;
int Init;
{
    long totalblks, availblks, avail, free, used;
    int fi;

    struct stat stbuf;
    struct fstab *fsp;

    if (stat(file, &stbuf) == 0 &&
	 (stbuf.st_mode&S_IFMT) != S_IFCHR &&
	 (stbuf.st_mode&S_IFMT) != S_IFBLK) {
	if (infsent) { 
	    sendval(("%d:%d\n", DISK_ERR_3, 0));
	    return;
	}
	setfsent();
	while (fsp = getfsent()) {
	    struct stat stb;

	    if (stat(fsp->fs_spec, &stb) == 0 &&
		stb.st_rdev == stbuf.st_dev) {
		file = fsp->fs_spec;
		endfsent();
		goto found;
	    }
	}
	endfsent();
	sendval(("%d:%d\n", DISK_ERR_4, 0));
	return;
    }
    found:
      fi = open(file, 0);
    if (fi < 0){
	return;
    }
    if (bread(fi, SBLOCK, (char *)&sblock, SBSIZE) == 0) {
	(void) close(fi);
	return;
    }
    totalblks = sblock.fs_dsize;
    free = sblock.fs_cstotal.cs_nbfree * sblock.fs_frag +
      sblock.fs_cstotal.cs_nffree;
    used = totalblks - free;
    availblks = totalblks * (100 - sblock.fs_minfree) / 100;
    avail = availblks > used ? availblks - used : 0;
    if(Init){
	sendval(("%d:%d:%s\n", id, 0, mpath(file)));
    }
    else{
	sendval(("%d:%d\n", id, availblks == 0 ? 0 : round((double) used / (double) availblks * 100.0)));
    }
    (void) close(fi);
}
