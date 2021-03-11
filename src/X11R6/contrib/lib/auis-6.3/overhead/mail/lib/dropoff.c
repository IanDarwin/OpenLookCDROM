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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/dropoff.c,v 2.30 1993/12/02 22:08:19 gk5g Exp $";
#endif

/*
		dropoff.c -- Subroutines for performing dropoff
			     of a piece of user mail.
*/


/*

********    Documentation for user-callable routines.	************



int dropoff(tolist, mesgfile, returnpath, home, flags)

    char *tolist[], *mesgfile, *returnpath, *home;
    long flags;


This is the routine to be used to drop off a piece of mail for
delivery.  The return values for this routine are defined in
"dropoff.h".  If the return value is not D_OK, then a printable
explanation of the error can be found in Dropoff_ErrMsg;

    tolist	    Null-terminated list of addresses.  Each
		    address is a null-terminated string.  There
		    must be at least one address.

    mesgfile	    Name of a file containing the message to
		    deliver.  The message is assumed to be
		    preceded by headers (at least From, To,
		    Subject, and Date headers) followed by a blank
		    line.  The headers should obey RFC822 syntax.

    returnpath	    Return path to be used.  This should be a legal
		    RFC822 return path header value, surrounded by
		    <>, although none of this is checked.  If this
		    parameter is null, it will be defined as (roughly)
			<getpwuid(getuid())->pw_name+@this-domain>
		    Such a computation might be slower than you like,
		    since it involves a passwd or White Pages lookup.

    home	    This is the home directory of the current user.
		    This is a null-terminated string.  If this
		    pointer is null, then a value will be determined
		    by doing getpwuid(getuid())->pw_dir.  Beware
		    that this is slow, so if you know the value you
		    should pass it in.

    flags	    Bit mask of flags controlling some actions of
		    the dropoff process.  (See dropoff.h for details.)

int dropoff_auth(tolist, mesgfile, returnpath, home, flags, auth)

    char *tolist[], *mesgfile, *returnpath, *home, *auth;
    long flags;

This is the same as the older dropoff() routine described above, but includes
   an authentication string to be passed to the qmail() routine.


void dropoff_Reset()
The dropoff() routine allocates one file descriptor and leaves it open.
This routine, dropoff_Reset, can be used to reclaim that file descriptor.


int test_dropoff()
Returns an integer (one of the DT_xxx codes defined in dropoff.h) that says how dropoff will be doing its work.  (Clients will want to know if they can give formatted mail to dropoff, whether dropoff will work at all, and whether to warn users about potential slownesses in delivery.)

*/

#ifdef _IBMR2
#ifndef _BSD
#define _BSD 43
#endif
#endif

#include <andrewos.h>
#include <fdplumb.h>
#include <stdio.h>
#include <pwd.h>
#include <errno.h>
#ifndef MAXPATHLEN
#include <sys/param.h>
#endif /* MAXPATHLEN */
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>

#ifdef AFS_ENV
#include <afs/param.h>
#include <sys/ioctl.h>
#include <afs/vice.h>
#include <afs/venus.h>
#include <afs/prs_fs.h>
#include <rx/xdr.h>
#include <afs/afsint.h>

#if defined(AFS30_ENV) && !defined(AFS31_ENV) 
#include <afs/comauth.h>
#endif /* defined(AFS30_ENV) && !defined(AFS31_ENV) */

#include <afs/auth.h>
#endif /* AFS_ENV */

#include <util.h>
#include <mail.h>
#include <mailconf.h>
#include <qmail.h>

#ifdef AMS_DELIVERY_ENV
#include <tokens.h>
#define _DROPOFF_SYS

/* External declarations */
extern int qmail();
extern char Qmail_ErrMsg[];
extern char *UnixError();
#endif /* AMS_DELIVERY_ENV */

#include <dropoff.h>

extern int errno;

typedef unsigned char bool;

#define FALSE	0
#define TRUE	1

char Dropoff_ErrMsg[3*MAXPATHLEN];

static int DropoffStyle = DT_UNK;
#ifdef AMS_DELIVERY_ENV
static char MyDomain[100] = "";
#endif /* AMS_DELIVERY_ENV */

int test_dropoff()
{/* Set a DropoffStyle and return it. */

    CheckAMSConfiguration();
    if (DropoffStyle == DT_UNK) {
#ifndef AMS_DELIVERY_ENV
	DropoffStyle = DT_NONAMS;
#else /* AMS_DELIVERY_ENV */
	struct CellAuth *ca;
	char *oneDom;
	int amsd, myerr;

	ca = NULL;
	FindAMSHomeCell(&ca);
	oneDom = (ca != NULL ? ca->CellName : WorkstationCell);
	if (MyDomain[0] == '\0') strncpy(MyDomain, oneDom, sizeof(MyDomain));
	if (ULstrcmp(MyDomain, oneDom) != 0) {	/* leave as DT_UNK */
		strncpy(MyDomain, oneDom, sizeof(MyDomain));
	} else {
	    amsd = 0;
	    if (ca != NULL) amsd = ca->UsesAMSDelivery;
	    if (amsd == 0) amsd = CheckAMSDelivery(MyDomain);
	    myerr = errno;
	    if (amsd == 0 && ULstrcmp(MyDomain, WorkstationCell) == 0)
		amsd = (AMS_DeliverySystem ? 1 : -1);
	    if (amsd == 0) {
		sprintf(Dropoff_ErrMsg,
			"Can't determine if home cell %s runs AMS delivery: %s",
			MyDomain, UnixError(myerr));	/* leave as DT_UNK */
	    } else {
		if (amsd < 0) {
			sprintf(Dropoff_ErrMsg, "OK: no AMS delivery for %s.", MyDomain);
			DropoffStyle = DT_NONAMS;	/* home cell doesn't want it. */
		} else {		/* amsd > 0 */
		    if (ULstrcmp(WorkstationCell, MyDomain) == 0) {
			sprintf(Dropoff_ErrMsg, "OK: AMS delivery for %s.", MyDomain);
			DropoffStyle = DT_AMS;	/* good non-cross-cell case. */
		    } else {
/* We're guests in a foreign cell.  Foreign (home) cell needs AMS delivery.  Does this workstation provide it? */
			if (!AMS_DeliverySystem) {
			    sprintf(Dropoff_ErrMsg,
				"Delivery will be delayed; your home cell %s uses AMS delivery, but this workstation doesn't provide it.",
				MyDomain);
			    DropoffStyle = DT_AMSWAIT;
			} else if (CheckAMSDelivery(WorkstationCell) < 0) {
			    sprintf(Dropoff_ErrMsg,
				"Delivery will be delayed; your home cell %s uses AMS delivery, but this workstation's cell, %s, doesn't provide it.",
				MyDomain, WorkstationCell);
			    DropoffStyle = DT_AMSWAIT;
			} else {
			    sprintf(Dropoff_ErrMsg,
				"OK: AMS delivery in both AMS-home (%s) and workstation (%s) cells.",
				MyDomain, WorkstationCell);
			    DropoffStyle = DT_AMS;	/* good cross-cell case */
			}
			if (DropoffStyle == DT_AMSWAIT && AMS_WSRunsQueuemail) {
			    sprintf(Dropoff_ErrMsg,
				"OK: Your AMS home cell (%s) uses AMS delivery, your WS cell (%s) doesn't provide it, but you say you can queue mail.",
				MyDomain, WorkstationCell);
			    DropoffStyle = DT_AMS;
			}
		    }
		}
	    }
	}
#endif /* AMS_DELIVERY_ENV */
    }
    return DropoffStyle;
}

#ifdef AMS_DELIVERY_ENV
static int getuserinfo(uid, returnpath, inhome, outhome, myname)
    register int *uid;
    char **returnpath, **inhome, **outhome, **myname;
{
    struct CellAuth *ca;
    static char r[300], h[MAXPATHLEN+1], usern[200], cellN[200];
    int Ret;
    struct passwd *PW = (struct passwd *)NULL;

    Ret = geteuid();
    if (Ret == 0) Ret = getuid();
    *uid = Ret;	/* The local workstation user-id */
   /*  I dare you to tell me how geteuid or getuid could possibly fail...
    POSIX says it can't. -rr2b
    if (Ret < 0) {
	sprintf(Dropoff_ErrMsg, "Can't get uid: %s", UnixError(errno));
	return D_OSERR;
    }
*/
/* Find out what identity we're claiming.  If there's a home dir, use it, else get what we can. */
    Ret = 2;	/* assume unauthenticated */
    *outhome = NULL;
    if (*inhome != NULL) {
	Ret = GetCellFromFileName(*inhome, cellN, sizeof(cellN));
	if (Ret == 0) {
		Ret = FindCell(cellN, &ca);
		*outhome = *inhome;
	}
    }
    if (Ret != 0) {	/* Just get some cell authentication on any earlier error. */
	Ret = FindAMSHomeCell(&ca);
	if (Ret == 1) Ret = FindAnyCell(&ca);
    }
    if (Ret < 0) {
	sprintf(Dropoff_ErrMsg, "Temporary failure finding my identity: %d", Ret);
	return D_TEMP_FAIL;
    } else if (Ret == 0 || Ret == 1) {	/* Use that cell's WP */
	PW = NULL;
	FillInCell(ca);
	if (ca->WpError > 0) Ret = 1;	/* admit failure */
    }
    if (Ret > 0) {		/* Look for our /etc/passwd identity */
	ca = NULL;
	PW = getpwuid(*uid);
	if (PW == NULL) {
		sprintf(Dropoff_ErrMsg, "Can't find pw entry for uid: %d", *uid);
		return D_OSERR;
	}
    }

    if (*outhome == NULL) {	/* Fill this in. */
	strcpy(h, (ca == NULL || ca->homeDir == NULL ? PW->pw_dir : ca->homeDir));
	*outhome = h;
    }

    if (*myname == NULL) {
	strcpy(usern, (ca == NULL || ca->UserName == NULL ?
				PW->pw_name : ca->UserName));
	*myname = usern;
    }

    if (*returnpath == NULL) {
	Ret = GetRetPath(*outhome, returnpath);
	if (Ret < 0 || *returnpath == NULL || *returnpath[0] == '\0') {
		sprintf(Dropoff_ErrMsg, "Temp failure getting ret path: %d", Ret);
		return D_TEMP_FAIL;
	}
	strcpy(r, *returnpath);
	*returnpath = r;
    }

    return D_OK;
}
#endif /* AMS_DELIVERY_ENV */

#ifdef AMS_DELIVERY_ENV
static int rewind_fd(f)
    int f;
{
    if (lseek(f, 0, 0) < 0) {
	sprintf(Dropoff_ErrMsg, "lseek failed: %s", UnixError(errno));
	return D_BAD_MESGFILE;
    } else
	return D_OK;
}
#endif /* AMS_DELIVERY_ENV */

#if defined(AMS_DELIVERY_ENV) && defined(AFS_ENV)
static bool setprotection(dirname, who, homecell)
    char *dirname, *who, *homecell;
{
    register int rights;
    struct ViceIoctl blob;
    auto char space[2000], prot[1000];
    char *PMName;

    PMName = CheckAMSPMName(homecell);
    if (PMName == NULL) PMName = PostmasterName;    /* dead-lost backup */
    rights = PRSFS_READ | PRSFS_LOOKUP | PRSFS_INSERT |
	     PRSFS_DELETE | PRSFS_LOCK | PRSFS_WRITE;
    sprintf(prot, "2\n0\n%s %d\n%s %d", who, rights, PMName, rights);
    blob.in = prot;
    blob.in_size = strlen(prot)+1;
    blob.out = space;
    blob.out_size = sizeof space;
    if (AMS_ViceIsRunning && pioctl(dirname, VIOCSETAL, &blob, 1) < 0) {
	sprintf(Dropoff_ErrMsg,
		"FS SA on \"%s\" failed for \"%s\"", dirname, who);
	return FALSE;
    } else
	return TRUE;
}
#endif /* defined(AMS_DELIVERY_ENV) && defined(AFS_ENV) */

#ifdef AMS_DELIVERY_ENV
static bool createoutgoing(dirname, whoname, homecell)
    char *dirname, *whoname, *homecell;
{
    if (mkdir(dirname, 0700) < 0) {
	sprintf(Dropoff_ErrMsg,
		"Can't create \"%s\": %s", dirname, UnixError(errno));
	return FALSE;
    }

#ifdef AFS_ENV
    return setprotection(dirname, whoname, homecell);
#else /* AFS_ENV */
    return TRUE;
#endif /* AFS_ENV */
}
#endif /* AMS_DELIVERY_ENV */

#if defined(AMS_DELIVERY_ENV) && defined(AFS_ENV)
static bool onvice(name)
    char *name;
{
    int fd, res;

    fd = open(name, O_RDONLY);
    if (fd < 0) {
	return vdown(errno);
    } else {
	res = IsOnVice(fd);
	close(fd);
	return res;
    }
}
#endif /* defined(AMS_DELIVERY_ENV) && defined(AFS_ENV) */

#ifdef AMS_DELIVERY_ENV
static bool drop_virgin = TRUE;
static int drop_s;
static struct sockaddr_in drop_addr;

static bool blipdaemon(uid, flags, dirname, homecell)
    int uid;
    long flags;
    char *dirname, *homecell;
{
    extern unsigned long getaddr();
    char *Pkt, *NmTkPtr;
    int PktLen, PktMax, RC;

    if (drop_virgin) {	/* 1-time initialization */
	unsigned long myaddr;

	drop_s = socket(AF_INET, SOCK_DGRAM, 0);
	if (drop_s < 0) {
	    sprintf(Dropoff_ErrMsg, "[Socket failed: %s] ", UnixError(errno));
	    return FALSE;
	}
	myaddr = getaddr();	/* Nichols' hack to get address quickly */
	if (myaddr == 0) {
	    strcpy(Dropoff_ErrMsg, "[Can't get this machine's IP address] ");
	    close(drop_s);
	    return FALSE;
	}
	bzero(&drop_addr, sizeof drop_addr);
	drop_addr.sin_family = AF_INET;
	drop_addr.sin_port = htons(MAILDAEMON_PORT);
	drop_addr.sin_addr.s_addr = myaddr;
	drop_virgin = FALSE;
    }

    /* Construct packet.  Guess an initial packet size. */
    PktMax = MAXPATHLEN + 3*sizeof(long int) + DROPOFF_PKT_LEN * 2;
    Pkt = (char *) malloc(PktMax);
    if (Pkt == NULL) {
	strcpy(Dropoff_ErrMsg, "[No memory for a packet] ");
	return FALSE;
    }
    PktLen = 0;

    flags |= DF_NEW_FMT;
    * (long int *) (&Pkt[PktLen]) = htonl(flags);	/* First, the flags */
    PktLen += sizeof(long int);
    * (long int *) (&Pkt[PktLen]) = htonl(uid);	/* then the workstation UID */
    PktLen += sizeof(long int);
    NmTkPtr = (&Pkt[PktLen]);		/* then the number of token pairs following */
    PktLen += sizeof(long int);			/* (filled in later) */
    if (tok_AddStr(&Pkt, &PktLen, &PktMax, dirname) == 0) {
	strcpy(Dropoff_ErrMsg, "[No memory for a packet] ");
	if (Pkt != NULL) free(Pkt);
	return FALSE;
    }
    RC = GetAndPackAllTokens_Prim(&Pkt, &PktLen, &PktMax, 0, homecell);
    if (RC <= 0) {
	if (Pkt != NULL) free(Pkt);
	sprintf(Dropoff_ErrMsg, "[Can't get all tokens: %s] ",
		(RC == -1 ? "Out of memory" :
			(RC == -2 ? "Server down" : "Unknown failure")));
	return FALSE;
    }
    * (long int *) NmTkPtr = htonl(RC);	/* number of token pairs */
    
    /* Send the packet */
    if (sendto(drop_s, Pkt, PktLen, 0, (struct sockaddr*) &drop_addr, sizeof drop_addr) < 0) {
	sprintf(Dropoff_ErrMsg, "[Sendto failed: %s] ", UnixError(errno));
	free(Pkt);
	return FALSE;
    }

    free(Pkt);
    return TRUE;
}
#endif /* AMS_DELIVERY_ENV */

#ifdef AMS_DELIVERY_ENV
static int tryoutgoing(uid, home, tolist, f, returnpath, flags, whoname, auth, homecell)
    int uid;
    char *home, *tolist[];
    int f;
    char *returnpath;
    long flags;
    char *whoname, *auth, *homecell;
{
    char outgoing[MAXPATHLEN+1];
    register bool created;
    register int tries;
    static int fixedOne = 0;

    strcpy(outgoing, home);
    strcat(outgoing, "/");
    strcat(outgoing, OutgoingName);
#ifdef AFS_ENV
/* Try correcting the protections, gratuitously, from time to time. */
    if (fixedOne == 0 && (time(0) & 07) == 0) {
	fixedOne = 1;
	(void) setprotection(outgoing, whoname, homecell);
    }
#endif /* AFS_ENV */
    created = FALSE;  /* This will become TRUE if .Outgoing is created */
    for (tries=0; tries<2; ++tries) {
	if (qmail(outgoing, tolist, f, returnpath, auth) == Q_OK) {
	    register int rc;
	    Dropoff_ErrMsg[0] = '\0';	/* Blipdaemon may have a message */
	    rc = D_OK;
	    if ((flags & DF_NOBLIP) == 0 && !blipdaemon(uid, flags, outgoing, homecell))
		rc = D_OK_WARN;
	    if (created) {
		strcat(Dropoff_ErrMsg, "[Created \"");
		strcat(Dropoff_ErrMsg, outgoing);
		strcat(Dropoff_ErrMsg, "\"] ");
		strcat(Dropoff_ErrMsg, Qmail_ErrMsg);
		return D_OK_WARN;
	    } else {
		strcat(Dropoff_ErrMsg, Qmail_ErrMsg);
		return rc;
	    }
	}

	/* Couldn't queue to .Outgoing, see why */
	if (tries == 0) {
	    /* It failed just once, maybe .Outgoing needs to be created */
	    if (access(outgoing, F_OK) == 0)
		break;	    /* The directory is there */
	    else
		if (vdown(errno)) break;    /* Can't find it, but Vice is down */
#ifdef AFS_ENV
	    if (AMS_ViceIsRunning && !onvice(home)) {
		sprintf(Dropoff_ErrMsg, "Won't create .Outgoing on \"%s\"", home);
		return D_CANT_QUEUE;
	    }
#endif /* AFS_ENV */
	    if (!createoutgoing(outgoing, whoname, homecell)) return D_CANT_QUEUE;
	    created = TRUE;
	}
    }
    strcpy(Dropoff_ErrMsg, Qmail_ErrMsg);
    return D_CANT_QUEUE;
}
#endif /* AMS_DELIVERY_ENV */

#ifdef AMS_DELIVERY_ENV
static int trytoqueue(f, tolist, returnpath, inhome, flags, auth, homecell)
    register int f;
    register char *tolist[];
    char *returnpath, *inhome;
    long flags;
    char *auth, *homecell;
{
    int uid;
    register int rc;
    char *myname, *outhome;

    /* First place to try is ~/.Outgoing */

    outhome = myname = NULL;
    rc = getuserinfo(&uid, &returnpath, &inhome, &outhome, &myname);
    if (rc != D_OK && rc != D_TEMP_FAIL) {
	/* If there is an error, getuserinfo will fill Dropoff_ErrMsg */
	return rc;
    }

    if ((flags & DF_NOLOCALDELIVERY) == 0) {
      if (rc == D_OK) {
	/* If delivering with home directory on / (e.g. for root), skip attempt to create /.Outgoing */
	if (uid != 0 && strcmp(outhome, "/") != 0) {
		rc = tryoutgoing(uid, outhome, tolist, f, returnpath, flags, myname, auth, homecell);
		if (rc == D_OK || rc == D_OK_WARN) return rc;

		/* Couldn't write to ~/.Outgoing; rewind_fd to be safe */
		rc = rewind_fd(f);
		if (rc != D_OK) return rc;
	} else {
		strcpy(Dropoff_ErrMsg, "No .Outgoing for home on /");
	}
      }
    }

    if (Dropoff_ErrMsg[0] != '\0') strcat(Dropoff_ErrMsg, "; ");
    if (tryvicequeues(tolist, f, returnpath, auth) == Q_OK) {
	strcat(Dropoff_ErrMsg, Qmail_ErrMsg);
	return D_OK_WARN;
    }
    strcat(Dropoff_ErrMsg, Qmail_ErrMsg);
    strcat(Dropoff_ErrMsg, "; ");

    /* Try local queue if allowed */
    if ((flags & DF_NOLOCALQ) == 0) {
	rc = rewind_fd(f);
	if (rc != D_OK) return rc;
	rc = qmail(LocalQueue, tolist, f, returnpath, auth);
	strcat(Dropoff_ErrMsg, Qmail_ErrMsg);
	return (rc == Q_OK ? D_LOCALQ : D_CANT_QUEUE);
    } else {
	strcat(Dropoff_ErrMsg, "local queueing inhibited");
	return D_CANT_QUEUE;
    }
}
#endif /* AMS_DELIVERY_ENV */

int dropoff_auth(tolist, mesgfile, returnpath, home, flags, auth)
    register char *tolist[];
    char *mesgfile, *returnpath, *home, *auth;
    long flags;
{
    register int f;
#ifdef AMS_DELIVERY_ENV
    register int rc;
    struct CellAuth *ca;
    char *oneDom;
#endif /* AMS_DELIVERY_ENV */

    /* Check parameters */
    if (tolist == NULL || tolist[0] == NULL || mesgfile == NULL) {
	strcpy(Dropoff_ErrMsg, "Empty `tolist' or `mesgfile'");
	return D_BAD_PARMS;
    }
    if ((flags&DF_NOBLIP) && (flags&DF_FORCE)) {
	strcpy(Dropoff_ErrMsg, "Inconsistent flags set");
	return D_BAD_PARMS;
    }

    /* Try to open file */
    f = open(mesgfile, O_RDONLY, 0);
    if (f < 0) {
	if (vdown(errno)) {
	    sprintf(Dropoff_ErrMsg,
		    "Temp fail on open for \"%s\": %s",
		    mesgfile, UnixError(errno));
	    return D_TEMP_FAIL;
	} else {
	    sprintf(Dropoff_ErrMsg,
		    "Can't open \"%s\": %s",
		    mesgfile, UnixError(errno));
	    return D_BAD_MESGFILE;
	}
    }
    Dropoff_ErrMsg[0] = '\0';
    CheckAMSConfiguration();
#ifdef AMS_DELIVERY_ENV
    ca = NULL;
    FindAMSHomeCell(&ca);
    oneDom = (ca != NULL ? ca->CellName : WorkstationCell);
    if (MyDomain[0] == '\0') strncpy(MyDomain, oneDom, sizeof(MyDomain));
    if (ULstrcmp(MyDomain, oneDom) != 0) {
	strncpy(MyDomain, oneDom, sizeof(MyDomain));
	DropoffStyle = DT_UNK;
    }
    if (DropoffStyle == DT_UNK) {
	test_dropoff();
	if (DropoffStyle == DT_UNK) return D_TEMP_FAIL;	/* Dropoff_ErrMsg already set */
    }
    if (DropoffStyle == DT_CANNOT) return D_OSERR;	/* Dropoff_ErrMsg already set */
    if (DropoffStyle == DT_AMS || DropoffStyle == DT_AMSWAIT) {
	/* Run through all of the queues */
	rc = trytoqueue(f, tolist, returnpath, home,
		(DropoffStyle == DT_AMSWAIT
			? ((flags | (DF_NOBLIP | DF_NOLOCALQ)) & ~DF_FORCE)
			: flags),
		auth, MyDomain);
	close(f);
	return rc;
    } else 
#endif /* AMS_DELIVERY_ENV */
        {    /* This clause is potentially attached the the above else. */
	/* Standard UNIX system: call old sendmail program. */
	FILE *fp;
	char **SMVec, **list, ReadBuf[4096];
	int argct, i, ct;

	argct = 0; list = tolist;
	while (list[argct]) ++argct;
	SMVec = (char **) malloc(sizeof(char*) * (5+argct));
	if (!SMVec) {
	    close(f);
	    strcpy(Dropoff_ErrMsg, "Out of storage building sendmail arg list");
	    return(D_TEMP_FAIL);
	}
	SMVec[0] = oldsendmail;
	SMVec[1] = "-oem"; /* mail back error messages */
	SMVec[2] = "-oi"; /* Do not interpret a period on a line by
                             itself as a message terminator */
	SMVec[3] = "-m"; /* include sender is alias expansion */
	for(i=0; i<argct; ++i) {
	    SMVec[i+4] = tolist[i];
	}
	SMVec[i+4] = NULL;
	fp = (FILE *) qopen(oldsendmail, SMVec, "w");
	if (!fp) {
	    sprintf(Dropoff_ErrMsg, "Can't qopen ``%s'': %s", oldsendmail, UnixError(errno));
	    close(f);
	    free(SMVec);
	    return(D_OSERR);
	}
	while ((ct = read(f, ReadBuf, sizeof(ReadBuf)-1)) > 0) {
	    if (fwriteallchars(ReadBuf, ct, fp) != ct) {
		sprintf(Dropoff_ErrMsg, "Can't copy message to %s: %s",
				oldsendmail, UnixError(errno));
		close(f);
		qclose(fp);
		free(SMVec);
		return(D_OSERR);
	    }
	}
	close(f);
	if (ct < 0) {
	    sprintf(Dropoff_ErrMsg, "Can't read message to %s: %s",
				oldsendmail, UnixError(errno));
	    qclose(fp);
	    free(SMVec);
	    return(D_OSERR);
	}
	if (qclose(fp) != 0) {
	    sprintf(Dropoff_ErrMsg, "Can't write message to %s: %s",
				oldsendmail, UnixError(errno));
	    free(SMVec);
	    return(D_OSERR);
	}
	free(SMVec);
	strcpy(Dropoff_ErrMsg, "All OK in sendmail dropoff.");
	return(D_OK);
    }
}

int dropoff(tolist, mesgfile, returnpath, home, flags)
register char *tolist[];
char *mesgfile, *returnpath, *home;
long flags;
{
    return(dropoff_auth(tolist, mesgfile, returnpath, home, flags, NULL));
}

void dropoff_Reset()
{/* Close the socket used for sending things to the daemon */
#ifdef AMS_DELIVERY_ENV
	if (!drop_virgin) {
		close(drop_s);
		drop_virgin = TRUE;
	}
#endif /* AMS_DELIVERY_ENV */
}

#ifdef TESTINGONLYTESTING
main () 
{
    char Dest[200], FileName[1000], Temp1[1000];
    char *list[2]; int RC, Flags;

    printf("Destination: "); fflush(stdout);
    gets(Dest);
    printf("Filename: "); fflush(stdout);
    gets(FileName);
    printf("Flags: "); fflush(stdout);
    gets(Temp1);
    Flags = atoi(Temp1);
    list[0] = Dest;
    list[1] = NULL;
    printf("Sending file ``%s'' to address ``%s'', with flags %d.\n", FileName, Dest, Flags);
    RC = dropoff(list, FileName, NULL, NULL, Flags);	/* no ret-path or home dir */
    printf("dropoff() returns %d (%s).\n", RC, Dropoff_ErrMsg);
}
#endif /* TESTINGONLYTESTING */
