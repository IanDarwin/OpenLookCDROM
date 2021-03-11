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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/stats/hp700/RCS/getstats.c,v 1.4 1993/09/23 20:33:56 gk5g Exp $";
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
#include <sys/signal.h>
#include <sys/user.h> 
/* Because otherwise proc.h will try to include fss.h, 
 * which isn't there.
 */
#undef __hp9000s700
#include <sys/proc.h>
#define __hp9000s700
#include <sys/vm.h>
#include <sys/dk.h>
#include <sys/map.h>

#include <sys/buf.h>

#include <netinet/in.h>
#include <errno.h>
#include <sys/stat.h>

#include <sys/socket.h>
#include <net/if.h>
#include <netinet/if_ether.h>
#include <checklist.h>
#include <mntent.h>
#include <sys/vfs.h>
#include <sys/fs.h>
#include <sys/dk.h>

#define DK_NDRIVE 8

#undef MAXUPRC

long maxuprc = 0;    /* we have to nlist in HP-UX  */

#define VMMON_DODECL
#include <nlist.h>
#include <getstats.h>
extern struct nlist RawStatistics[];

char	root[32];

int TotalTime;
int deficit;
int MemoryFile /* file descriptor for unix memory */ ;
int nproc;
off_t procp;
struct proc proc[8];/* 8 = a few, for fewer syscalls */
struct proc *mproc;
extern char *malloc();


struct{
   long time[CPUSTATES];
   long xfer[DK_NDRIVE];
   struct vmmeter Rate;
   struct vmtotal Total;
   long  dk_xfer[DK_NDRIVE];
}s, s1;

union {
    struct fs iu_fs;
    char dummy[SBSIZE];
} sb;

#define sendval(text) {printf text ;fflush(stdout);}




GetGVMStats(UsersID)
int UsersID;
{
    register int   i;
    long  t;
    struct mapent *sp;
    int myval = 0;

    lseek(MemoryFile,(long) RawStatistics[X_CPTIME].n_value, 0);
    read(MemoryFile, s.time, sizeof(s.time));

    lseek(MemoryFile,(long) RawStatistics[X_DKXFER].n_value, 0);
    read(MemoryFile, s.dk_xfer, sizeof(s.dk_xfer));

    lseek(MemoryFile,(long) RawStatistics[X_RATE].n_value, 0);
    read(MemoryFile, &s.Rate, sizeof(s.Rate));

    lseek(MemoryFile,(long) RawStatistics[X_TOTAL].n_value, 0);
    read(MemoryFile, &s.Total, sizeof(s.Total));

    lseek(MemoryFile,(long) RawStatistics[X_DEFICIT].n_value, 0);
    read(MemoryFile, &deficit, sizeof(deficit));
    for (i = 0; i < CPUSTATES; i++) {
	t = s.time[i];
	s.time[i] -= s1.time[i];
	s1.time[i] = t;
    }
    s.time[1] += s.time[0];
    TotalTime = 0;
    for (i = 1; i < CPUSTATES; i++)
	TotalTime += s.time[i];
    if (TotalTime == 0)
	TotalTime = 1;
    sendval(("%d:%d\n", LOADCPU, (s.time[1] + s.time[2]) * 100 / TotalTime));
    for (i = 1; i < DK_NDRIVE - 1; i++)
	s.dk_xfer[0] += s.dk_xfer[i];
    if (s1.dk_xfer[0] == 0){
	s1.dk_xfer[0] = s.dk_xfer[0];
    }
    myval = s.dk_xfer[0] - s1.dk_xfer[0];
    if (myval > 100) myval = 100;
    sendval(("%d:%d\n", LOADIO, myval));
    s1.dk_xfer[0] = s.dk_xfer[0];
    sendval(("%d:%d\n", LOADUSER, s.time[1] * 100 / TotalTime));
    sendval(("%d:%d\n", LOADSYS, s.time[2] * 100 / TotalTime));
    sendval(("%d:%d\n", LOADIDLE, s.time[3] * 100 / TotalTime));
    sendval(("%d:%d\n", VM, (int)((double)s.Total.t_avm/(double)s.Total.t_vm * 100)));
    sendval(("%d:%d\n", PAGEIN, s.Rate.v_pgpgin / 2));
    sendval(("%d:%d\n", PAGEOUT, s.Rate.v_pgpgout / 2));
    sendval(("%d:%d\n", PAGEREPLACABLE, s.Rate.v_scan));
    sendval(("%d:%d\n", PAGEDEFICIT, deficit));
    sendval(("%d:%d\n", MEMACTIVE, s.Total.t_avm / 2));
    sendval(("%d:%d\n", MEMFREE, s.Total.t_free / 2));
    sendval(("%d:%d\n", QUEUERUN, s.Total.t_rq));
    sendval(("%d:%d\n", QUEUEBLOCK, s.Total.t_dw + s.Total.t_pw));
    sendval(("%d:%d\n", QUEUEMEM, s.Total.t_sw));
    sendval(("%d:%d\n", INTSIO, s.Rate.v_intr));
    sendval(("%d:%d\n", INTSSYS, s.Rate.v_syscall));
    sendval(("%d:%d\n", INTSSWAP, s.Rate.v_swtch));
    if (1) {/* DoPROCESSES */
	int i, j, userprocesses, totalprocesses, otherprocs;
	off_t tmpprocp;

	userprocesses = 0;
	totalprocesses = 1;
	otherprocs = 0;
	tmpprocp = procp;

	for (i = 0; i < nproc; i += 8) {
	    lseek(MemoryFile, (long) tmpprocp, 0);
	    j = nproc - i;
	    if (j > 8)
		j = 8;
	    j *= sizeof(struct proc);
	    if (read(MemoryFile, (char *) proc, j) != j) {
		sendval(("%d:%d\n", GVM_ERR_1, 0));
		exit(-1);
	    }
	    tmpprocp += j;
	    for (j = j / sizeof(struct proc) - 1; j >= 0; j--) {
		mproc = &proc[j];
		if (mproc->p_pid != 0) {
		    totalprocesses++;
		    if (UsersID == mproc->p_uid) {
			userprocesses ++;
		    } else if (mproc->p_uid) {
			otherprocs++;
		    }
		}
	    }
	}
	sendval(("%d:%d\n", PROCSUSER, maxuprc ? (userprocesses * 100) / maxuprc : -1));
	sendval(("%d:%d\n", PROCSTOTAL, nproc ? totalprocesses * 100 / nproc : -1));
	sendval(("%d:%d\n", PROCSOTHER, otherprocs));
    }
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
   lseek(MemoryFile,(long) RawStatistics[X_BOOTIME].n_value, 0);
   read(MemoryFile, &bootime, sizeof(bootime));

   lseek(MemoryFile, (long) RawStatistics[X_PROC].n_value, 0);
   read(MemoryFile,(char *) &procp, sizeof(procp));

   lseek(MemoryFile, (long) RawStatistics[X_NPROC].n_value, 0);
   read(MemoryFile,(char *) &nproc, sizeof(nproc));

#ifndef MAXUPRC
    lseek(MemoryFile, (long) RawStatistics[X_MAXUPRC].n_value, 0);
    read(MemoryFile,(char *) &maxuprc, sizeof(maxuprc));
#endif /* MAXUPRC */
}


/* the DeviceTable keeps a list of all the devices (and their mounted directory file name) that we should watch */

extern int getmnt();

GetDiskStats(Init)
int Init;
{
    int i = 0;
    struct stat statb;
    char tmpname[1024];
    register FILE *mtabp;
    register struct mntent *mnt;

    sync();
    if ((mtabp = setmntent(MNT_MNTTAB, "r")) == NULL) {
	sendval(("%d:%d\n", DISK_ERR_1, 0));
	exit(1);
    }
    i = DISK1 - 1; /* figuratively 0 */
    while ((mnt = getmntent(mtabp)) != NULL) {
	i++;
	if (strcmp(mnt->mnt_type, MNTTYPE_IGNORE) == 0 ||
	    strcmp(mnt->mnt_type, MNTTYPE_SWAP) == 0)
	    continue;
	if (strcmp(mnt->mnt_type, MNTTYPE_HFS) == 0 &&
	    (stat(mnt->mnt_fsname, &statb) >= 0) &&
	    (((statb.st_mode & S_IFMT) == S_IFBLK) ||
	     ((statb.st_mode & S_IFMT) == S_IFCHR))) {
	    (void) strcpy(tmpname, mnt->mnt_fsname);
	    dfree1(i, tmpname, 0, Init);
	} else {
	    dfree2(i, mnt->mnt_dir, mnt, Init);
	}
    }
    (void) endmntent(mtabp);
}

int bread(fi, bno, buf, cnt)
int fi;
daddr_t bno;
char *buf;
int cnt;
{
    extern int errno;

    (void) lseek(fi, (long)(bno * DEV_BSIZE), 0);
    if (read(fi, buf, (unsigned) cnt) < 0)
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
    FILE *mntp;
    register struct mntent *mnt;


    if ((mntp = setmntent(MNT_MNTTAB, "r")) == 0) {
	sendval(("%d:%d\n", DISK_ERR_1, 0));
	exit(1);
    }
    while ((mnt = getmntent(mntp)) != 0) {
	if (strcmp(file, mnt->mnt_fsname) == 0) {
	    (void) endmntent(mntp);
	    return (mnt->mnt_dir);
	}
    }
    (void) endmntent(mntp);
    return "";
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
    long totalblks, availblks, free, used;
    int fi;
    struct stat stbuf;
    struct checklist *fsp;

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
    if (bread(fi, SBLOCK, (char *)&sb.iu_fs, SBSIZE) == 0) {
	(void) close(fi);
	return;
    }
    totalblks = sb.iu_fs.fs_dsize;
    free = sb.iu_fs.fs_cstotal.cs_nbfree * sb.iu_fs.fs_frag +
      sb.iu_fs.fs_cstotal.cs_nffree;
    used = totalblks - free;
    availblks = totalblks * (100 - sb.iu_fs.fs_minfree) / 100;
    if(Init){
	sendval(("%d:%d:%s\n", id, 0, mpath(file)));
    }
    else{
	sendval(("%d:%d\n", id, availblks == 0 ? 0 : round((double) used / (double) availblks * 100.0)));
    }
    (void) close(fi);
}


dfree2(id, file, mnt, Init)
int id;
char *file;
struct mntent *mnt;
int Init;
{
    struct statfs fs;
    long totalblks, avail, free, used, reserved;

    if (statfs(file, &fs) < 0) {
	return;
    }
    totalblks = fs.f_blocks;
    free = fs.f_bfree;
    used = totalblks - free;
    avail = fs.f_bavail;
    reserved = free - avail;
    if (avail < 0)
	avail = 0;
    totalblks -= reserved;
    if(Init){
	sendval(("%d:%d:%s\n",id, 0, mnt->mnt_dir));
    }
    else{
	sendval(("%d:%d\n", id, round((double) used / (double) totalblks * 100.0)));
    }
}
