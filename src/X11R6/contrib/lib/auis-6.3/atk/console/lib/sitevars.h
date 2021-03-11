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


 


/* this is a set of defines for directories that are used within the console program - these should be tailored to the specific site that plans to use console */

#define _SITE_CONSOLELIB "/usr/andrew/lib/consoles"
#define _SITE_QUEUEMAIL "/usr/andrew/etc/queuemail"
#define _SITE_PRINT "/usr/andrew/bin/print"

#if defined(hpux) || defined(sys_telmat)
#define _SITE_NON_ANDREW_MAIL "/usr/mail/"
#else /* hpux */
#if defined(bsdi) || defined(__386BSD__)
#define _SITE_NON_ANDREW_MAIL "/var/mail"
#else /* bsdi */
#define _SITE_NON_ANDREW_MAIL "/usr/spool/mail/"
#endif /* bsdi || __386BSD__ */
#endif /* hpux */
#define _SITE_MAILBOX "Mailbox"
#ifdef hpux
#define _SITE_NON_ANDREW_PRINTDIR "/usr/spool/lp/request"
#else /* hpux */
#if defined(M_UNIX) || defined(sys_telmat)
#define _SITE_NON_ANDREW_PRINTDIR "/usr/spool/lp/requests"
#else /* M_UNIX */
#if defined(bsdi)
#define _SITE_NON_ANDREW_PRINTDIR "/var/spool/output"
#else /* bsdi */
#define _SITE_NON_ANDREW_PRINTDIR "/usr/spool/lpd"
#endif /* bsdi */
#endif /* M_UNIX */
#endif /* hpux */
#define _SITE_PRINTDIR "PrintDir"
#ifdef sys_telmat
#define _SITE_LOGFILE "/var/tmp/ConsoleLog"
#else /* sys_telmat */
#define _SITE_LOGFILE "/tmp/ConsoleLog"
#endif /* sys_telmat */

#ifdef hpux
#define _SITE_MTAB "/etc/mnttab"
#define _SITE_FSTAB "/etc/checklist"
#else /* hpux */
#ifdef sys_telmat
#define _SITE_MTAB "/etc/mnttab"
#define _SITE_FSTAB "/etc/vfstab"
#else /* sys_telmat */
#define _SITE_MTAB "/etc/mtab"
#define _SITE_FSTAB "/etc/fstab"
#endif /* sys_telmat */
#endif /* hpux */

#define _SITE_BIN_SH "/bin/sh"

#ifdef sys_telmat
#define _SITE_DEV_TTY "/dev/term"
#define _SITE_DEV_PTYP "/dev/ptmx"
#else /* sys_telmat */
#define _SITE_DEV_TTY "/dev/tty"
#define _SITE_DEV_PTYP "/dev/ptyp"
#endif /* sys_telmat */

#define _SITE_DEV_CONSOLE "/dev/console"
#define _SITE_DEV_KMEM "/dev/kmem"

#ifdef hpux
#define _SITE_VMUNIX "/hp-ux"
#else /* hpux */
#ifdef _IBMR2
#define _SITE_VMUNIX "/unix"
#else /* ! _IBMR2 */
#ifdef sys_telmat
#define _SITE_VMUNIX "/stand/unix"
#else /* sys_telmat */
#if defined(bsdi)
#define _SITE_VMUNIX "/bsd"
#else /* bsdi */
#define _SITE_VMUNIX "/vmunix"
#endif /* bsdi */
#endif /* sys_telmat */
#endif /* _IBMR2 */
#endif /* hpux */

#define _SITE_CONSOLE_SOCKET 2018 /* udp */
#define _SITE_VENUS_ITC_SOCKET 2106 /* tcp */
#define _SITE_VENUS_ITC_SOCKET_ALT 2107 /* tcp */
#define _SITE_MARINER_SOCKET 2106 /* udp *//* was venus socket */

#define _SITE_SCM "cluster1.fs.andrew.cmu.edu"
/* above for vopcon */

#if (defined(vax) || defined(MIPSEL))
#define _SITE_INTERCEPT "/dev/xcons"
/* used to be /dev/smscreen */
#endif /* vax || MIPSEL */

