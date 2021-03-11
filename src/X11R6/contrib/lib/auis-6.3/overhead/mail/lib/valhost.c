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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/valhost.c,v 2.18 1993/08/25 20:34:47 susan Exp $";
#endif

#include <system.h>
#include <andyenv.h>
#include <stdio.h>
#include <util.h>
#include <mail.h>
#include <mailconf.h>
#include <sys/param.h>
#include <signal.h>
#include <errno.h>
#include <ctype.h>
#include <andrewos.h> /* sys/types.h */
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/stat.h>
#include <netdb.h>
#ifdef RESOLVER_ENV
#include <arpa/nameser.h>
#include <resolv.h>
extern int h_errno;
#endif /* RESOLVER_ENV */

extern int errno;

#ifdef RESOLVER_ENV
typedef union {
	HEADER qb1;
	char qb2[PACKETSZ*3];
} querybuf;

/* Return values from GetRec */
#define NoDomain 1	/* Definite negative */
#define TryAgain 2	/* Trying the request later might help */
#define NoRecovery 3	/* No information, but trying again later won't help. */
#define NoAnswers 4	/* No answers of the desired type. */
#define IsCName 5	/* What you found was a CNAME: ask again with that name. */
#define DestOK 6		/* You got what you wanted. */

#if (defined(RES_DEFNAMES) || defined(RES_DNSRCH))
/* Define myRES_OPTS to be as many of the bits in (RES_DNSRCH | RES_DEFNAMES) as are defined in this build, if any of them are defined.  These are the bits that specify name searching/defaulting, so we don't want them to apply if we're following a CNAME or other record (in which all names are returned in fully-qualified form). */
#if (defined(RES_DEFNAMES))

#if (defined(RES_DNSRCH))
#define myRES_OPTS (RES_DNSRCH | RES_DEFNAMES)
#else /* (defined(RES_DNSRCH)) */
#define myRES_OPTS (RES_DEFNAMES)
#endif /* (defined(RES_DNSRCH)) */

#else /* (defined(RES_DEFNAMES)) */
#define myRES_OPTS (RES_DNSRCH)
#endif /* (defined(RES_DEFNAMES)) */
#endif /* (defined(RES_DEFNAMES) || defined(RES_DNSRCH)) */

static u_short get_ushort(ptr)
char *ptr;
{
    unsigned char *P = (unsigned char *) ptr;
    u_short Res;

    Res = *P++ << 8;
    Res |= *P;
    return (Res);
}

static int GetRec(InName, wantedType, sawA, sawMX, OutN, sizeOutN, OutFwd, sizeOutFwd)
char *InName; int wantedType, *sawA, *sawMX;
char *OutN; int sizeOutN; char *OutFwd; int sizeOutFwd;
{/* Get the address and MX records describing the desired mail destination.  Expand domain abbreviations the same way that gethostname() does, if the BIND distribution supports it. */
    HEADER *hp;
    char *eom, *cp;
    querybuf answer;
    int n, ancount, qdcount;
    u_short type, class;
    char nbuf[BUFSIZ];
    int first, AnyAnswers, thisPref, minPref, n1, ancountOrig;

#ifndef res_SEARCH
#define res_SEARCH cptres_search
#endif

    hp = (HEADER *) &answer;
    hp->rcode = 14; /* Tag that we have never received a packet here */
    h_errno = 0;
    n = res_SEARCH(InName, C_IN, wantedType, (char *)&answer, sizeof(answer));
    if (n < 0) return TryAgain;
    eom = (char *)&answer + n;
    /*
      * find first satisfactory answer
      */
    ancount = ntohs(hp->ancount);
    qdcount = ntohs(hp->qdcount);
    if (hp->rcode != NOERROR || ancount == 0) {
#ifdef DEBUG
	if (_res.options & RES_DEBUG)
	    printf("rcode = %d, ancount=%d\n", hp->rcode, ancount);
#endif /* DEBUG */
	switch (hp->rcode) {
	    case NXDOMAIN:  /* Check if it's an authoritive answer */
		if (hp->aa) return NoDomain;
		else return TryAgain;
	    case SERVFAIL:
		return TryAgain;
	    case NOERROR:  /* Check if it's an authoritive answer */
		if (hp->aa) return NoAnswers;
		else return TryAgain;
	    case FORMERR:
	    case NOTIMP:
	    case REFUSED:
		return NoRecovery;
	    default:
		return TryAgain;
	}
    }
    cp = (char *)&answer + sizeof(HEADER);
    if (qdcount) {
#ifndef dn_SKIPNAME
#ifdef CQUERYM
	/* Version 4.7.3 of the Bind distribution changed the function name dn_skip to dn_skipname.
	 Another thing that changed at that point was whether the old CQUERYM flag was defined in /usr/include/arpa/nameser.h; it became STATUS with 4.7.3.
	   We hope that one of [CQUERYM/dn_skip] and [STATUS/dn_skipname] is the pairing on your system.  But if it isn't, you need only define dn_SKIPNAME to be whichever is appropriate. */
#define dn_SKIPNAME dn_skip(cp)
#else /* CQUERYM */
#define dn_SKIPNAME dn_skipname(cp, eom)
#endif /* CQUERYM */
#endif /* dn_SKIPNAME */
	cp += dn_SKIPNAME + QFIXEDSZ;
	while (--qdcount > 0)
	    cp += dn_SKIPNAME + QFIXEDSZ;
    }
    first = 1; AnyAnswers = 0; minPref = 100000;
    ancountOrig = ancount;
    while (--ancount >= 0 && cp < eom) {
	if ((n = dn_expand((char *)&answer, eom, cp, nbuf,
			   sizeof(nbuf))) < 0) break;
	cp += n;
	type = get_ushort(cp);
	cp += sizeof(u_short);
	class = get_ushort(cp);
	cp += sizeof(u_short) + sizeof(u_long);
	n = get_ushort(cp);
	cp += sizeof(u_short);
	if (class != C_IN && class != C_ANY) {cp += n; continue;}
	if (first) {
	    (void)strncpy(OutN, nbuf, sizeOutN);
	    OutN[sizeOutN - 1] = '\0';
	    first = 0;
	}
	if (type == T_CNAME &&
	    (ancountOrig == 1
#ifdef myRES_OPTS
	     || (_res.options & myRES_OPTS)
#endif /* myRES_OPTS */
	     )) {
	    /* The first CNAME found, which should be the only one, is good enough. */
	    if ((n = dn_expand((char *)&answer, eom, cp, nbuf,
			       sizeof(nbuf))) < 0) break;
	    (void)strncpy(OutN, nbuf, sizeOutN);
	    OutN[sizeOutN - 1] = '\0';
	    return IsCName;
	}
	if (wantedType == T_ANY || wantedType == type) ++AnyAnswers;
	if (type == T_A) *sawA = 1;
	if (type == T_MX) *sawMX = 1;
	if (type == T_MX && OutFwd != NULL && sizeOutFwd > 10) {
	    /* Load the forwarder name, if any given */
	    thisPref = get_ushort(cp);
	    if (thisPref < minPref) {
		cp += sizeof(u_short);	/* advance pointer to the MX host */
		n1 = dn_expand((char *)&answer, eom, cp, nbuf, sizeof(nbuf));
		cp -= sizeof(u_short);	/* put the pointer back */
		if (n1 < 0) break;
		n1 = sizeOutFwd;
		if (n1 > sizeof(nbuf)) n1 = sizeof(nbuf);
		strncpy(OutFwd, nbuf, n1);
		OutFwd[sizeOutFwd - 1] = '\0';
		minPref = thisPref;	/* This is the new minimum preference. */
	    }
	}
	cp += n;
    }
    return (AnyAnswers ? DestOK : NoAnswers);
}
#endif /* RESOLVER_ENV */

static struct hostent host;
static char *host_aliases[30];
static char HOSTDB[] = "/etc/hosts";
static FILE *hostf = NULL;
static char line[512];
static char hostaddr[10];
static char *host_addrs[2];

static char *
anychar(cp, match)
register char *cp;
char *match;
{
    register char *mp, c;

    while (c = *cp) {
	for (mp = match; *mp; mp++)
	    if (*mp == c)
		return (cp);
	cp++;
    }
    return ((char *)0);
}


static struct hostent *
nexthtent()
{
    char *p;
    register char *cp, **q;

    if (hostf == NULL && (hostf = fopen(HOSTDB, "r" )) == NULL)
	return (NULL);
    again:
      if ((p = fgets(line, BUFSIZ, hostf)) == NULL)
	  return (NULL);
    if (*p == '#')
	goto again;
    cp = anychar(p, "#\n");
    if (cp == NULL)
	goto again;
    *cp = '\0';
    cp = anychar(p, " \t");
    if (cp == NULL)
	goto again;
    *cp++ = '\0';
    /* THIS STUFF IS INTERNET SPECIFIC */
#ifdef h_addr
    /* when BSD > 43, h_addr is defined as h_addr_list[0]. */
    host.h_addr_list = host_addrs;
#endif /* h_addr */
    host.h_addr = hostaddr;
    *((u_long *)host.h_addr) = inet_addr(p);
    host.h_length = sizeof (u_long);
    host.h_addrtype = AF_INET;
    while (*cp == ' ' || *cp == '\t')
	cp++;
    host.h_name = cp;
    q = host.h_aliases = host_aliases;
    cp = anychar(cp, " \t");
    if (cp != NULL) 
	*cp++ = '\0';
    while (cp && *cp) {
	if (*cp == ' ' || *cp == '\t') {
	    cp++;
	    continue;
	}
	if (q < &host_aliases[29])
	    *q++ = cp;
	cp = anychar(cp, " \t");
	if (cp != NULL)
	    *cp++ = '\0';
    }
    *q = NULL;
    return (&host);
}

static struct hostent *
gettblbyname(name)
char *name;
{
    register struct hostent *p;
    register char **cp;
    char lowname[128];
    register char *lp = lowname;

    while (*name)
	if (isupper(*name))
	    *lp++ = tolower(*name++);
	else
	    *lp++ = *name++;
    *lp = '\0';


    if (hostf == NULL)
	hostf = fopen(HOSTDB, "r" );
    else
	rewind(hostf);
    while (p = nexthtent()) {
	if (strcmp(p->h_name, lowname) == 0)
	    break;
	for (cp = p->h_aliases; *cp != 0; cp++)
	    if (strcmp(*cp, lowname) == 0)
		goto found;
    }
    found:
      if (hostf) {
	  (void) fclose(hostf);
	  hostf = NULL;
      }
    return (p);
}

static int
GetDomainTranslation(domain, name, namelen)
char *name;
int namelen;
char *domain;
{
    FILE *translationfile;
    char *p, buf[1024];
    int prefixlen;

    prefixlen = strlen(name) - strlen(domain);

    strcpy(buf, CellCommonPrefix);
    LCappend(buf, ThisDomain);
    strcat(buf, CellCommonSuffix);
    strcat(buf, "domain/");
    strcat(buf, domain+1);
    strcat(buf, ".translation");

    translationfile = fopen(buf, "r");
    if (!translationfile) return 0;

    while (fgets(buf, sizeof(buf)-1, translationfile)) {
	if (!ULstrncmp(name, buf, prefixlen) && isspace(buf[prefixlen])) {
	    p = buf + prefixlen;
	    while (*p && isspace(*p)) p++;
	    if (!*p) return 0;
	    while (namelen-- && *p && !isspace(*p)) *name++ = *p++;
	    if (namelen > 0) *name++ = '\0';
	    return 1;
	}
    }
    return 0;
}

enum MailHostQuality
ValidateDomainMail(InName, OutName, OutNameLen, FwdName, FwdNameLen, TolerableDelay)
char *InName, *OutName, *FwdName;
int OutNameLen, FwdNameLen, TolerableDelay;
{/* Validates InName as a destination domain for mail, returning the rewritten name in OutName (which is storage of size OutNameLen owned by the caller).  Possibly returns a forwarding host (if we're to do MX mail forwarding checks) in FwdName (which is storage of size FwdNameLen owned by the caller), if FwdName is not NULL.  Returns a code saying whether the given name was known-OK, known-bad, or unvalidatable in the given interval.  TolerableDelay is the caller's suggestion for how long this procedure should wait for an answer from name resolvers.

    The idea behind all the configuration options is that the behavior of this procedure should mimic how the underlying mail delivery will handle a destination mail domain insofar as determining whether a host is good, bad, or indeterminate.
    */
    int Rslt, AddrDum, k, Len = 0;
    enum MailHostQuality BestAnswer;

    CheckAMSConfiguration();

    if (AMS_ValidateDestHosts == 0) {
	strncpy(OutName, InName, OutNameLen);
	return mailhost_good;
    }

    if (numAMS_ValidDomainSuffixes > 0) Len = strlen(InName);
    for (k = 0; k < numAMS_ValidDomainSuffixes; ++k) {
	Rslt = strlen(AMS_ValidDomainSuffixes[k]);
	if (Rslt <= Len)
	    if (ULstrcmp(AMS_ValidDomainSuffixes[k], &InName[Len - Rslt]) == 0){
		strncpy(OutName, InName, OutNameLen);
		if (GetDomainTranslation(AMS_ValidDomainSuffixes[k],
					 OutName, OutNameLen)) {
		    return mailhost_good;
		}
		if (k < numAMS_ValidDomainRelays)
		    strncpy(FwdName, AMS_ValidDomainRelays[k], FwdNameLen);
		return mailhost_good;
	    }
    }

    if (*InName == '[') {	/* Check for direct IP address. */
	Rslt = sscanf(InName, "[ %d . %d . %d . %d ] %[^ ]",
		      &AddrDum, &AddrDum, &AddrDum, &AddrDum, OutName);
	if (Rslt == 4) {
	    strncpy(OutName, InName, OutNameLen);
	    return mailhost_good;
	} else {
	    OutName[0] = '\0';
	    return (AMS_HardHostValidationErrors ?
		    mailhost_bad : mailhost_indeterminate);
	}
    }

    BestAnswer = mailhost_bad;
    errno = 0;
    OutName[0] = '\0';
    if (FwdName != NULL) FwdName[0] = '\0';

#ifdef RESOLVER_ENV
    if (AMS_DeliveryViaDomainAddress || AMS_DeliveryViaDomainMXAddress) {
	int sawA, sawMX;
#ifdef myRES_OPTS
	long int OldResOpts;
#endif /* myRES_OPTS */
	int OldRetrans, OldRetry;
	int DelayPerNS, NumQueries, Timeout;

	sawA = sawMX = 0;
	if ((_res.options & RES_INIT) == 0) res_init();
	DelayPerNS = TolerableDelay / _res.nscount;
	NumQueries = (DelayPerNS < 9 ? 2 :
		      (DelayPerNS < 20 ? 3 : 4));
	Timeout = (2 * TolerableDelay) / (NumQueries * NumQueries);
	if (Timeout <= 0) Timeout = 1;
	OldRetrans = _res.retrans; OldRetry = _res.retry;
	_res.retrans = Timeout; _res.retry = NumQueries;
#ifdef myRES_OPTS
	OldResOpts = (_res.options & myRES_OPTS);
#endif /* myRES_OPTS */

	if (AMS_DeliveryViaDomainMXAddress == 0) { /* Just using IP addresses */
	    Rslt = GetRec(InName, T_A, &sawA, &sawMX,
			  OutName, OutNameLen, FwdName, FwdNameLen);
	    while (Rslt == IsCName) {
#ifdef myRES_OPTS
		_res.options &= (~myRES_OPTS);
#endif /* myRES_OPTS */
		Rslt = GetRec(OutName, T_A, &sawA, &sawMX,
			      OutName, OutNameLen, FwdName, FwdNameLen);
	    }
	    if (Rslt == NoDomain || Rslt == NoAnswers) BestAnswer = mailhost_bad;
	    else if (Rslt == TryAgain || Rslt == NoRecovery) BestAnswer = mailhost_indeterminate;
	    else if (Rslt == DestOK) BestAnswer = mailhost_good;
	} else {	/* Uses both MX and addresses */
	    Rslt = GetRec(InName, T_ANY, &sawA, &sawMX,
			  OutName, OutNameLen, FwdName, FwdNameLen);
	    while (Rslt == IsCName) {
#ifdef myRES_OPTS
		_res.options &= (~myRES_OPTS);
#endif /* myRES_OPTS */
		Rslt = GetRec(OutName, T_ANY, &sawA, &sawMX,
			      OutName, OutNameLen, FwdName, FwdNameLen);
	    }
	    if (Rslt == NoDomain || Rslt == NoAnswers) BestAnswer = mailhost_bad;
	    else if (Rslt == TryAgain || Rslt == NoRecovery) BestAnswer = mailhost_indeterminate;
	    else if (Rslt == DestOK && (sawA || sawMX)) BestAnswer = mailhost_good;
	    else if (Rslt == DestOK) {
#ifdef myRES_OPTS
		_res.options &= (~myRES_OPTS);
#endif /* myRES_OPTS */
		Rslt = GetRec(OutName, T_MX, &sawA, &sawMX,
			      OutName, OutNameLen, FwdName, FwdNameLen);
		while (Rslt == IsCName)
		    Rslt = GetRec(OutName, T_MX, &sawA, &sawMX,
				  OutName, OutNameLen, FwdName, FwdNameLen);
		if (Rslt == NoDomain) BestAnswer = mailhost_bad;
		else if (Rslt == TryAgain) BestAnswer = mailhost_indeterminate;
		else if (sawA || sawMX) BestAnswer = mailhost_good;
		else if (Rslt == DestOK) BestAnswer = mailhost_good;
		else {	/* No MX; keep looking... */
		    Rslt = GetRec(OutName, T_A, &sawA, &sawMX,
				  OutName, OutNameLen, FwdName, FwdNameLen);
		    while (Rslt == IsCName)
			Rslt = GetRec(OutName, T_A, &sawA, &sawMX,
				      OutName, OutNameLen, FwdName, FwdNameLen);
		    if (Rslt == NoDomain || Rslt == NoAnswers) BestAnswer = mailhost_bad;
		    else if (Rslt == TryAgain || Rslt == NoRecovery)
			BestAnswer = mailhost_indeterminate;
		    else if (Rslt == DestOK || sawA || sawMX) BestAnswer = mailhost_good;
		}
	    }
	}

	_res.retrans = OldRetrans; _res.retry = OldRetry;
#ifdef myRES_OPTS
	_res.options &= (~myRES_OPTS);
	_res.options |= OldResOpts;
#endif /* myRES_OPTS */
    }
#endif /* RESOLVER_ENV */

    if (BestAnswer != mailhost_good) {
	if (AMS_DeliveryViaHostTable
#ifdef RESOLVER_ENV
	    || ((AMS_DeliveryViaDomainAddress || AMS_DeliveryViaDomainMXAddress)
		&& (errno == ECONNREFUSED))
#endif /* RESOLVER_ENV */
	    ) {
	    struct hostent *Host;

	    Host = gettblbyname(InName);
	    if (Host != NULL) {
		Len = OutNameLen;
		if (Len > (strlen(Host->h_name)+1)) Len = strlen(Host->h_name)+1;
		strncpy(OutName, Host->h_name, Len);
		return mailhost_good;
	    }
#ifdef RESOLVER_ENV
	    if (BestAnswer == mailhost_bad && ! AMS_DeliveryViaHostTable)
		BestAnswer = mailhost_indeterminate;
#endif /* RESOLVER_ENV */
	}
    }

    if (BestAnswer != mailhost_good) {
	if (AMS_DeliveryViaGethostbyname) {
	    struct hostent *Host;

	    Host = gethostbyname(InName);
	    if (Host != NULL) {
		Len = OutNameLen;
		if (Len > (strlen(Host->h_name)+1)) Len = strlen(Host->h_name)+1;
		strncpy(OutName, Host->h_name, Len);
		return mailhost_good;
	    }
	}
    }

    if (BestAnswer == mailhost_bad)
	if (! AMS_HardHostValidationErrors) BestAnswer = mailhost_indeterminate;

    return BestAnswer;
}

enum MailHostQuality
ValidateMailHostName(InName, OutName, OutNameLen, TolerableDelay)
char *InName, *OutName;
int OutNameLen, TolerableDelay;
{
    return ValidateDomainMail(InName, OutName, OutNameLen, NULL, 0, TolerableDelay);
}

#ifdef TESTINGONLYTESTING
main() {
    char *p;
    char InLine[400], OutDomain[400], FwdDomain[400];
    enum MailHostQuality mhq;

#ifdef RESOLVER_ENV
    _res.options |= RES_DEBUG;
#endif /* RESOLVER_ENV */
    printf("Test your favorite destination mail domain names.  One at a time, please.\n");
    for (;;) {
	printf("domain: "); fflush(stdout);
	p = gets(InLine);
	if (p == NULL) exit(0);
	OutDomain[0] = FwdDomain[0] = '\0';
	mhq = ValidateDomainMail(InLine, OutDomain, sizeof(OutDomain),
				 FwdDomain, sizeof(FwdDomain), 10);
	printf("Input: ``%s''; output: ``%s'' (%s)", InLine, OutDomain,
	       (mhq == mailhost_good ? "good" :
		(mhq == mailhost_bad ? "bad" : "indeterminate")));
	if (FwdDomain[0] != '\0') printf("; fwd: ``%s''", FwdDomain);
	printf("\n");
    }
}
#endif /* TESTINGONLYTESTING */

