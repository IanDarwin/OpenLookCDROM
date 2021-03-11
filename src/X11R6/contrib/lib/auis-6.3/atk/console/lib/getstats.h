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


 


#define LOADCPU 1
#define LOADIO 2
#define LOADUSER 3
#define LOADSYS 4
#define LOADIDLE 5
#define VM 6
#if defined(sys_telmat)
#define PAGE_IN 7
#define PAGE_OUT 8
#else /* sys_telmat */
#define PAGEIN 7
#define PAGEOUT 8
#endif /* sys_telmat */
#define PAGEREPLACABLE 9
#define PAGEDEFICIT 10
#define MEMACTIVE 11
#define MEMFREE 12
#define QUEUERUN 13
#define QUEUEBLOCK 14
#define QUEUEMEM 15
#define INTSIO 16
#define INTSSYS 17
#define INTSSWAP 18
#define NDSTATIN 19
#define NDSTATOUT 20
#define NDSTATERR 21
#define PROCSUSER 22
#define PROCSTOTAL 23
#define PROCSOTHER 24
#define DISK1 25 /* first disk */
#define DISK2 26
#define DISK3 27
#define DISK4 28
#define DISK5 29
#define DISK6 30
#define DISK7 31
#define DISK8 32
#define DISK9 33
#define DISK10 34
#define DISK11 35
#define DISK12 36
#define DISK13 37
#define DISK14 38
#define DISK15 39
#define DISK16 40
#define DISK17 41
#define DISK18 42
#define DISK19 43
#define DISK20 44
#define DISK21 45
#define DISK22 46
#define DISK23 47
#define DISK24 48
#define DISK25 49
#define DISK26 50
#define DISK27 51
#define DISK28 52
#define DISK29 53
#define DISK30 54
#define DISK31 55
#define DISK32 56
#define DISK33 57
#define DISK34 58
#define DISK35 59
#define DISK36 60
#define DISK37 61
#define DISK38 62
#define DISK39 63
#define DISK40 64
#define DISK41 65
#define DISK42 66
#define DISK43 67
#define DISK44 68
#define DISK45 69
#define DISK46 70
#define DISK47 71
#define DISK48 72
#define DISK49 73
#define DISK50 74
#define MAXGETSTATSCOUNTERS 74 /* must agree with last DISK */

#define	PARSE_ERR_1 100	/* "Usage: uid poll-period" */
#define	PARSE_ERR_2 101	/* "Invalid Polling Frequency" */
#define	GVM_ERR_1 110	/* "Cannot read proc table" */ 
#define	GVM_ERR_2 111	/* "nlist (/vmunix) failed" */
#define	GVM_ERR_3 112	/* "No (/vmunix) namelist" */
#define	GVM_ERR_4 113	/* "Cannot open /dev/kmem" */
#define	DISK_ERR_1 120	/* "setmntent failed" */
#define	DISK_ERR_2 121	/* "setfsent failed" */
#define	DISK_ERR_3 123	/* "screwy /etc/fstab entry" NON-FATAL */
#define	DISK_ERR_4 124	/* "disk mounted on unknown device" NON-FATAL */
#define DISK_ERR_5 125 	/* "Dismounted disk? (EIO)" NON-FATAL */
#define	DISK_ERR_6 126	/* "Cannot get mount information" */

#ifdef MACH_ENV
#define PAGESIZE 150
#define PAGESWIRED 151
#define PAGEFAULTS 152
#define PAGECOWFAULTS 153
#define PAGEZEROFILLS 154
#define PAGEREACTIVATES 155
#define PAGECACHEHITS 156
#define PAGELOOKUPS 157
#define PAGEHITRATE 158
#endif /* MACH_ENV */

#if defined(sys_telmat)
#ifdef VMMON_DODECL
struct nlist   RawStatistics[] =
{
#endif
#define	X_CPTIME	0
#ifdef VMMON_DODECL
   {
      "sysinfo"
   },
#endif
#define	X_RATE		1
#ifdef VMMON_DODECL
   {
      "vminfo"
   },
#endif
#define X_TOTAL		2
#ifdef VMMON_DODECL
   {
      "total"
   },
#endif
#define	X_DEFICIT	3
#ifdef VMMON_DODECL
   {
      "deficit"
   },
#endif
#define	X_PROC		4
#ifdef VMMON_DODECL
   {
      "curproc"
   },
#endif
#define	X_VAR		5
#ifdef VMMON_DODECL
   {
      "v"
   },
#endif
#define	X_BOOTIME	6
#ifdef VMMON_DODECL
   {
      "boottime"
   },
#endif
#ifdef VMMON_DODECL
   {
      0
   }
};
#endif


#else /* sys_telmat */

#if defined(bsdi)
#ifdef VMMON_DODECL
struct nlist   RawStatistics[] =
{
#endif
#define	X_CPTIME	0
#ifdef VMMON_DODECL
   {
      "_cpustats"
   },
#endif
#define	X_RATE		1
#ifdef VMMON_DODECL
   {
      "_cnt"
   },
#endif
#define X_TOTAL		2
#ifdef VMMON_DODECL
   {
      "_total"
   },
#endif
#define	X_PROC		3
#ifdef VMMON_DODECL
   {
      "_curproc"
   },
#endif
#define	X_BOOTIME	4
#ifdef VMMON_DODECL
   {
      "_boottime"
   },
#endif
#define X_NSWAPMAP	5
#ifdef VMMON_DODECL
   {
       "_nswapmap"
   },
#endif
#define X_SWAPMAP	6
#ifdef VMMON_DODECL
   {
       "_swapmap"
   },
#endif
#define X_DMMAX		7
#ifdef VMMON_DODECL
   {
       "_dmmax"
   },
#endif
#define	X_PROC		8
#ifdef VMMON_DODECL
   {
       "_proc0"
   },
#endif
#define	X_NPROC		9
#ifdef VMMON_DODECL
   {
       "_maxproc"
   },
#endif
#define X_MAXUPRC	10
#ifdef VMMON_DODECL
   {
       "_maxproc"
   },
#endif
#define X_NSWAPBLKS	11
#ifdef VMMON_DODECL
   {
       "_vmtotal"
   },
#endif
#ifdef VMMON_DODECL
   {
      0
   }
};
#endif


#else /* bsdi */


#ifdef VMMON_DODECL
struct nlist   RawStatistics[] =
{
#endif
#define	X_CPTIME	0
#ifdef VMMON_DODECL
   {
#ifdef MAXCPU			/* for Ultrix 4.* */
      "_cpudata"
#else
#ifdef hpux
	"cp_time"
#else
	"_cp_time"
#endif
#endif
   },
#endif
#define	X_RATE		1
#ifdef VMMON_DODECL
   {
#ifdef hpux
       "rate"
#else
       "_rate"
#endif
   },
#endif
#define X_TOTAL		2
#ifdef VMMON_DODECL
   {
#ifdef hpux
       "total"
#else
      "_total"
#endif
   },
#endif
#define	X_DEFICIT	3
#ifdef VMMON_DODECL
   {
#ifdef hpux
       "deficit"
#else
      "_deficit"
#endif
   },
#endif
#define	X_BOOTIME	4
#ifdef VMMON_DODECL
   {
#ifdef hpux
       "boottime"
#else
      "_boottime"
#endif
   },
#endif
#define	X_DKXFER	5
#ifdef VMMON_DODECL
   {
#ifdef hpux
       "dk_xfer"
#else
      "_dk_xfer"
#endif
   },
#endif
#define X_PGIN		6
#ifdef VMMON_DODECL
   {
      "_pgintime"
   },
#endif
#define X_NDSTAT	7
#ifdef VMMON_DODECL
   {
      "_ndstat"
   },
#endif
#define X_NSWAPMAP	8
#ifdef VMMON_DODECL
   {
       "_nswapmap"
   },
#endif
#define X_SWAPMAP	9
#ifdef VMMON_DODECL
   {
       "_swapmap"
   },
#endif
#define X_NSWAPBLKS	10
#ifdef VMMON_DODECL
   {
#ifdef hpyx
       "nswap"
#else
       "_nswap"
#endif
   },
#endif
#define X_DMMAX		11
#ifdef VMMON_DODECL
   {
#ifdef hpux
       "dmmax"
#else
       "_dmmax"
#endif
   },
#endif
#define	X_PROC		12
#ifdef VMMON_DODECL
   {
#ifdef hpux
       "proc"
#else
      "_proc"
#endif
   },
#endif
#define	X_NPROC		13
#ifdef VMMON_DODECL
   {
#ifdef hpux
       "nproc"
#else
      "_nproc"
#endif
   },
#endif
#ifndef MAXUPRC
#define X_MAXUPRC	14
#ifdef VMMON_DODECL
   {
#ifdef hpux
       "maxuprc"
#else
	"_maxuprc"
#endif
   },
#endif
#endif
#ifdef VMMON_DODECL
   {
      0
   },
};
#endif
#endif /* bsdi */
#endif /* sys_telmat */

