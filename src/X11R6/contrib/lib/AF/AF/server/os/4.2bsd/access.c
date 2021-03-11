/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include "Aos.h"
#include "audio.h"
#include "audioproto.h"
#include "misc.h"
#include "site.h"
#include <errno.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

# include <net/if.h>

#include <netdb.h>
#ifdef TCPCONN
#include <netinet/in.h>
#endif
#ifdef DNETCONN
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif
#undef NULL
#include <stdio.h>
#include "diastruct.h"
#include "osdep.h"

#define acmp(a1, a2, len) bcmp((char *)(a1), (char *)(a2), len)
#define acopy(a1, a2, len) bcopy((char *)(a1), (char *)(a2), len)
#define addrEqual(fam, address, length, host) \
			 ((fam) == (host)->family &&\
			  (length) == (host)->len &&\
			  !acmp (address, (host)->addr, length))

#if	!defined(sgi) && !defined(ultrix) && !defined(__osf__)
extern char	*index();
#endif

static int XFamily(), UnixFamily();
static int ConvertAddr(), CheckAddr();
static void NewHost();

typedef struct _host {
	short		family;
	short		len;
#ifdef DNETCONN
	unsigned char	addr[sizeof(struct dn_naddr)];
#else
	unsigned char	addr[4];	/* will need to be bigger eventually */
#endif
	struct _host *next;
} HOST;

static HOST *selfhosts = NULL;
static HOST *validhosts = NULL;
static int AccessEnabled = DEFAULT_ACCESS_CONTROL;
static int LocalHostEnabled = FALSE;
static int UsingXdmcp = FALSE;

typedef struct {
    int af, xf;
} FamilyMap;

static FamilyMap familyMap[] = {
#ifdef     AF_DECnet
#ifdef     DNETCONN
    {AF_DECnet, AFamilyDECnet},
#endif
#endif
#ifdef     AF_CHAOS
#ifdef     CHAOSCONN
    {AF_CHAOS, AFamilyChaos},
#endif
#endif
#ifdef    AF_INET
#ifdef    TCPCONN
    {AF_INET, AFamilyInternet}
#endif
#endif
};

/*
 * called when authorization is not enabled to add the
 * local host to the access list
 */

EnableLocalHost ()
{
    if (!UsingXdmcp)
    {
	LocalHostEnabled = TRUE;
	AddLocalHosts ();
    }
}

/*
 * called at init time when XDMCP will be used; xdmcp always
 * adds local hosts manually when needed
 */

AccessUsingXdmcp ()
{
    UsingXdmcp = TRUE;
    LocalHostEnabled = FALSE;
}

#define FAMILIES ((sizeof familyMap)/(sizeof familyMap[0]))

/* Define this host for access control.  Find all the hosts the OS knows about 
 * for this fd and add them to the selfhosts list.
 */
DefineSelf (fd)
    int fd;
{
    char		buf[2048];
    struct ifconf	ifc;
    register int	n;
    int 		len;
    pointer 		addr;
    int 		family;
    register HOST 	*host;
    register struct ifreq *ifr;
    
#ifdef DNETCONN
    struct dn_naddr *dnaddr = getnodeadd(0);
    /*
     * AF_DECnet may not be listed in the interface list.  Instead use
     * the supported library call to find out the local address (if any).
     */
    if (dnaddr)
    {    
	addr = (pointer) dnaddr;
	len = dnaddr->a_len + sizeof(dnaddr->a_len);
	family = AF_DECnet;
	for (host = selfhosts;
	     host && !addrEqual (family, addr, len, host);
	     host = host->next)
	    ;
        if (!host)
	{
	    host = (HOST *) xalloc (sizeof (HOST));
	    if (host)
	    {
		host->family = family;
		host->len = len;
		acopy(addr, host->addr, len);
		host->next = selfhosts;
		selfhosts = host;
	    }
	}
    }
#endif
    ifc.ifc_len = sizeof (buf);
    ifc.ifc_buf = buf;
    if (ioctl (fd, (int) SIOCGIFCONF, (pointer) &ifc) < 0)
        Error ("Getting interface configuration");
    for (ifr = ifc.ifc_req, n = ifc.ifc_len / sizeof (struct ifreq); --n >= 0;
     ifr++)
    {
	len = sizeof(ifr->ifr_addr);
#ifdef DNETCONN
	/*
	 * DECnet was handled up above.
	 */
	if (ifr->ifr_addr.sa_family == AF_DECnet)
	    continue;
#endif
        if ((family = ConvertAddr (&ifr->ifr_addr, &len, &addr)) <= 0)
	    continue;
        for (host = selfhosts;
 	     host && !addrEqual (family, addr, len, host);
	     host = host->next)
	    ;
        if (host)
	    continue;
        host = (HOST *) xalloc (sizeof (HOST));
	if (host)
	{
	    host->family = family;
	    host->len = len;
	    acopy(addr, host->addr, len);
	    host->next = selfhosts;
	    selfhosts = host;
	}
#ifdef XDMCP
	{
	    struct sockaddr broad_addr;

	    /*
	     * If this isn't an Internet Address, don't register it.
	     */
	    if (family != AF_INET)
		continue;

	    XdmcpRegisterConnection (AFamilyInternet, (char *)addr, len);
	    broad_addr = ifr->ifr_addr;
	    ((struct sockaddr_in *) &broad_addr)->sin_addr.s_addr =
		htonl (INADDR_BROADCAST);
#ifdef SIOCGIFBRDADDR
	    {
	    	struct ifreq    broad_req;
    
	    	broad_req = *ifr;
		if (ioctl (fd, SIOCGIFFLAGS, (char *) &broad_req) != -1 &&
		    (broad_req.ifr_flags & IFF_BROADCAST) &&
		    (broad_req.ifr_flags & IFF_UP)
		    )
		{
		    broad_req = *ifr;
		    if (ioctl (fd, SIOCGIFBRDADDR, &broad_req) != -1)
			broad_addr = broad_req.ifr_addr;
		    else
			continue;
		}
		else
		    continue;
	    }
#endif
	    XdmcpRegisterBroadcastAddress ((struct sockaddr_in *) &broad_addr);
	}
#endif
    }
}

#ifdef XDMCP
void
AugmentSelf(fd)
    int fd;
{
    int len;
    struct sockaddr from;
    int family;
    pointer addr;
    register HOST *host;

    len = sizeof(from);
    if (getpeername(fd, &from, &len))
	return;
    family = ConvertAddr(&from, &len, &addr);
    if (family <= 0)
	return;
    for (host = selfhosts; host; host = host->next)
    {
	if (addrEqual(family, addr, len, host))
	    return;
    }
    host = (HOST *)xalloc(sizeof(HOST));
    if (!host)
	return;
    host->family = family;
    host->len = len;
    acopy(addr, host->addr, len);
    host->next = selfhosts;
    selfhosts = host;
}
#endif

AddLocalHosts ()
{
    HOST    *self;

    for (self = selfhosts; self; self = self->next)
	NewHost (self->family, self->addr, self->len);
}

/* Reset access control list to initial hosts */

ResetHosts (device)
    char *device;
{
    register HOST	*host;
    char 		hostname[120];
    char		fname[32];
    FILE		*fd;
    char		*ptr;
    union {
        struct sockaddr	sa;
#ifdef TCPCONN
        struct sockaddr_in in;
#endif
#ifdef DNETCONN
        struct sockaddr_dn dn;
#endif
    } 			saddr;
#ifdef DNETCONN
    struct nodeent 	*np;
    struct dn_naddr 	dnaddr, *dnaddrp, *dnet_addr();
#endif
    int			family;
    int			len;
    pointer		addr;
    register struct hostent *hp;

    AccessEnabled = DEFAULT_ACCESS_CONTROL;
    LocalHostEnabled = FALSE;
    while ( (host = validhosts) != NULL)
    {
        validhosts = host->next;
        xfree (host);
    }
    strcpy (fname, "/etc/audio");
    strcat (fname, device);
    strcat (fname, ".hosts");
    if ( (fd = fopen (fname, "r")) != NULL) 
    {
        while (fgets (hostname, sizeof (hostname), fd))
	{
    	if ( (ptr = index (hostname, '\n')) != NULL)
    	    *ptr = 0;
#ifdef DNETCONN
    	if ((ptr = index (hostname, ':')) && (*(ptr + 1) == ':'))
	{
    	    /* node name (DECnet names end in "::") */
    	    *ptr = 0;
	    dnaddrp = dnet_addr(hostname);
    	    if (!dnaddrp && (np = getnodebyname (hostname)))
	    {
		/* node was specified by name */
		saddr.sa.sa_family = np->n_addrtype;
		len = sizeof(saddr.sa);
		if (ConvertAddr (&saddr.sa, &len, &addr) == AF_DECnet)
		{
		    bzero ((char *) &dnaddr, sizeof (dnaddr));
		    dnaddr.a_len = np->n_length;
		    acopy (np->n_addr, dnaddr.a_addr, np->n_length);
		    dnaddrp = &dnaddr;
		}
    	    }
	    if (dnaddrp)
		NewHost((short)AF_DECnet, (pointer)dnaddrp,
			(int)(dnaddrp->a_len + sizeof(dnaddrp->a_len)));
    	}
	else
	{
#endif
#ifdef TCPCONN
    	    /* host name */
    	    if ( (hp = gethostbyname (hostname)) != NULL)
	    {
    		saddr.sa.sa_family = hp->h_addrtype;
		len = sizeof(saddr.sa);
    		if ((family = ConvertAddr (&saddr.sa, &len, &addr)) > 0)
#ifdef h_addr				/* new 4.3bsd version of gethostent */
		{
		    char **list;

		    /* iterate over the addresses */
		    for (list = hp->h_addr_list; *list; list++)
			NewHost (family, (pointer)*list, len);
		}
#else
    		    NewHost (family, (pointer)hp->h_addr, len);
#endif

    	    }
#endif
#ifdef DNETCONN
    	}	
#endif
        }
        fclose (fd);
    }
}

static ABool
AuthorizedClient(client)
    ClientPtr client;
{
    int    		alen, family;
    struct sockaddr	from;
    pointer		addr;
    register HOST	*host;

    alen = sizeof (from);
    if (!getpeername (((OsCommPtr)client->osPrivate)->fd, &from, &alen))
    {
        if ((family = ConvertAddr (&from, &alen, &addr)) >= 0)
	{
	    if (family == 0)
		return TRUE;
	    for (host = selfhosts; host; host = host->next)
	    {
		if (addrEqual (family, addr, alen, host))
		    return TRUE;
	    }
	}
    }
    return FALSE;
}

/* Add a host to the access control list.  This is the external interface
 * called from the dispatcher */

int
AddHost (client, family, length, pAddr)
    ClientPtr		client;
    int                 family;
    unsigned            length;        /* of bytes in pAddr */
    pointer             pAddr;
{
    int			len;
    register HOST	*host;
    int                 unixFamily;

    if (!AuthorizedClient(client))
	return(ABadAccess);
    unixFamily = UnixFamily(family);
    if (unixFamily < 0)
    {
	client->errorValue = family;
	return (ABadValue);
    }
    if ((len = CheckAddr (unixFamily, pAddr, length)) < 0)
    {
	client->errorValue = length;
        return (ABadValue);
    }
    for (host = validhosts; host; host = host->next)
    {
        if (addrEqual (unixFamily, pAddr, len, host))
    	    return (ASuccess);
    }
    host = (HOST *) xalloc (sizeof (HOST));
    if (!host)
	return(ABadAlloc);
    host->family = unixFamily;
    host->len = len;
    acopy(pAddr, host->addr, len);
    host->next = validhosts;
    validhosts = host;
    return (ASuccess);
}

/* Add a host to the access control list. This is the internal interface 
 * called when starting or resetting the server */
static void
NewHost (family, addr, len)
    short	family;
    pointer	addr;
    int		len;
{
    register HOST *host;

    for (host = validhosts; host; host = host->next)
    {
        if (addrEqual (family, addr, len, host))
	    return;
    }
    host = (HOST *) xalloc (sizeof (HOST));
    if (host)
    {
	host->family = family;
	host->len = len;
	acopy(addr, host->addr, len);
	host->next = validhosts;
	validhosts = host;
    }
}

/* Remove a host from the access control list */

int
RemoveHost (client, family, length, pAddr)
    ClientPtr		client;
    int                 family;
    unsigned            length;        /* of bytes in pAddr */
    pointer             pAddr;
{
    int			len,
                        unixFamily;
    register HOST	*host, **prev;

    if (!AuthorizedClient(client))
	return(ABadAccess);
    unixFamily = UnixFamily(family);
    if (unixFamily < 0)
    {
	client->errorValue = family;
        return(ABadValue);
    }
    if ((len = CheckAddr (unixFamily, pAddr, length)) < 0)
    {
	client->errorValue = length;
        return(ABadValue);
    }
    /*EMPTY*/
    for (prev = &validhosts;
         (host = *prev) && (!addrEqual (unixFamily, pAddr, len, host));
         prev = &host->next)
        ;
    if (host)
    {
        *prev = host->next;
        xfree (host);
    }
    return (ASuccess);
}

/* Get all hosts in the access control list */
int
GetHosts (data, pnHosts, pLen, pEnabled)
    pointer		*data;
    int			*pnHosts;
    int			*pLen;
    BOOL		*pEnabled;
{
    int			len;
    register int 	n = 0;
    register pointer	ptr;
    register HOST	*host;
    int			nHosts = 0;
    int			*lengths = (int *) NULL;
    int			*newlens;

    *pEnabled = AccessEnabled ? AEnableAccess : ADisableAccess;
    for (host = validhosts; host; host = host->next)
    {
	newlens = (int *) xrealloc(lengths, (nHosts + 1) * sizeof(int));
	if (!newlens)
	{
	    xfree(lengths);
	    return(ABadAlloc);
	}
	lengths = newlens;
	lengths[nHosts++] = host->len;
	n += (((host->len + 3) >> 2) << 2) + sizeof(aHostEntry);
    }
    if (n)
    {
        *data = ptr = (pointer) xalloc (n);
	if (!ptr)
	{
	    xfree(lengths);
	    return(ABadAlloc);
	}
	nHosts = 0;
        for (host = validhosts; host; host = host->next)
	{

	    len = lengths[nHosts++];
	    ((aHostEntry *)ptr)->family = XFamily(host->family);
	    ((aHostEntry *)ptr)->length = len;
	    ptr += sizeof(aHostEntry);
	    acopy (host->addr, ptr, len);
	    ptr += ((len + 3) >> 2) << 2;
        }
    } else {
	*data = NULL;
    }
    *pnHosts = nHosts;
    *pLen = n;
    xfree(lengths);
    return(ASuccess);
}

/* Check for valid address family and length, and return address length. */

/*ARGSUSED*/
static int
CheckAddr (family, pAddr, length)
    int			family;
    pointer		pAddr;
    unsigned		length;
{
    int	len;

    switch (family)
    {
#ifdef TCPCONN
      case AF_INET:
	if (length == sizeof (struct in_addr))
	    len = length;
	else
	    len = -1;
        break;
#endif 
#ifdef DNETCONN
      case AF_DECnet:
        {
	    struct dn_naddr *dnaddr = (struct dn_naddr *) pAddr;

	    if ((length < sizeof(dnaddr->a_len)) ||
		(length < dnaddr->a_len + sizeof(dnaddr->a_len)))
		len = -1;
	    else
		len = dnaddr->a_len + sizeof(dnaddr->a_len);
	    if (len > sizeof(struct dn_naddr))
		len = -1;
	}
        break;
#endif
      default:
        len = -1;
    }
    return (len);
}

/* Check if a host is not in the access control list. 
 * Returns 1 if host is invalid, 0 if we've found it. */

InvalidHost (saddr, len)
    register struct sockaddr	*saddr;
    int				len;
{
    int 			family;
    pointer			addr;
    register HOST 		*selfhost, *host;

    if (!AccessEnabled)   /* just let them in */
        return(0);    
    if ((family = ConvertAddr (saddr, &len, &addr)) < 0)
        return (1);
    if (family == 0)
    {
	if (!LocalHostEnabled)
 	{
	    /*
	     * check to see if any local address is enabled.  This 
	     * implicitly enables local connections.
	     */
	    for (selfhost = selfhosts; selfhost; selfhost=selfhost->next)
 	    {
		for (host = validhosts; host; host=host->next)
		{
		    if (addrEqual (selfhost->family, selfhost->addr,
				   selfhost->len, host))
			return 0;
		}
	    }
	    return 1;
	} else
	    return (0);
    }
    for (host = validhosts; host; host = host->next)
    {
        if (addrEqual (family, addr, len, host))
    	    return (0);
    }
    return (1);
}

static int
ConvertAddr (saddr, len, addr)
    register struct sockaddr	*saddr;
    int				*len;
    pointer			*addr;
{
    if (*len == 0)
        return (0);
    switch (saddr->sa_family)
    {
      case AF_UNSPEC:
#ifdef UNIXCONN
      case AF_UNIX:
#endif
        return (0);

#ifdef TCPCONN
      case AF_INET:
        *len = sizeof (struct in_addr);
        *addr = (pointer) &(((struct sockaddr_in *) saddr)->sin_addr);
        return (AF_INET);
#endif

#ifdef DNETCONN
      case AF_DECnet:
	{
	    struct sockaddr_dn *sdn = (struct sockaddr_dn *) saddr;
	    *len = sdn->sdn_nodeaddrl + sizeof(sdn->sdn_nodeaddrl);
	    *addr = (pointer) &(sdn->sdn_add);
	}
        return (AF_DECnet);
#endif

      default:
        break;
    }
    return (-1);
}

int
ChangeAccessControl(client, fEnabled)
    ClientPtr client;
    int fEnabled;
{
    if (!AuthorizedClient(client))
	return ABadAccess;
    AccessEnabled = fEnabled;
    return ASuccess;
}

static int XFamily(af)
    int af;
{
    int i;
    for (i = 0; i < FAMILIES; i++)
        if (familyMap[i].af == af)
            return familyMap[i].xf;
    return -1;
}

static int UnixFamily(xf)
    int xf;
{
    int i;
    for (i = 0; i < FAMILIES; i++)
        if (familyMap[i].xf == xf)
            return familyMap[i].af;
    return -1;
}

