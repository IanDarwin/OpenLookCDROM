/*

Copyright 1985, 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
M.I.T. makes no representations about the suitability of
this software for any purpose.  It is provided "as is"
without express or implied warranty.

*/
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

#ifndef lint
static char *rcsid_ahost_c = "$XConsortium: xhost.c,v 11.36 89/12/16 20:42:36 rws Exp $";
#endif
 
#ifdef TCPCONN
#define NEEDSOCKETS
#endif
#ifdef UNIXCONN
#define NEEDSOCKETS
#endif
#ifdef DNETCONN
#define NEEDSOCKETS
#endif
#ifdef STREAMSCONN
#define NEEDSOCKETS
#endif

#include <stdio.h>
#include <stdlib.h>				/* exit proto. 		*/
#include <unistd.h>				/* alarm proto...	*/
#include <AF/Aos.h>
#include <AF/AFlib.h>
#include <AF/audioproto.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#ifdef NEEDSOCKETS
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#endif

#ifdef notdef
#include <arpa/inet.h>
	bogus definition of inet_makeaddr() in BSD 4.2 and Ultrix
#else
extern unsigned long inet_makeaddr();
extern unsigned int inet_addr(char *);
#endif
#ifdef DNETCONN
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif
#ifdef STREAMSCONN
#include <X11/Xstreams.h>
extern char _XsTypeOfStream[];
#endif

int change_host(AFAudioConn *aud, char *name, ABool add);
static int local_aferror(AFAudioConn *aud, AFErrorEvent *rep);
static char *get_hostname(AFHostAddress *ha);


#ifdef STREAMSCONN
static char *get_streams_hostname();
static Bool get_streams_address();
#endif

#ifdef SIGNALRETURNSINT
#define signal_t int
#else
#define signal_t void
#endif
static signal_t nameserver_lost();

#define NAMESERVER_TIMEOUT 5	/* time to wait for nameserver */

int nameserver_timedout;
 
char *ProgramName;

#ifdef NEEDSOCKETS
static int AFamily(int af)
{
    int i;
    static struct _familyMap {
	int af, xf;
    } familyMap[] = {
#ifdef	AF_DECnet
        { AF_DECnet, AFamilyDECnet },
#endif
#ifdef	AF_CHAOS
        { AF_CHAOS, AFamilyChaos },
#endif
#ifdef	AF_INET
        { AF_INET, AFamilyInternet },
#endif
};

#define FAMILIES ((sizeof familyMap)/(sizeof familyMap[0]))

    for (i = 0; i < FAMILIES; i++)
	if (familyMap[i].af == af) return familyMap[i].xf;
    return -1;
}
#endif

AFAudioConn *aud;

main(int argc, char **argv)
{
	register char *arg;
	int i, nhosts;
	char *hostname;
	AFHostAddress *list;
	ABool enabled = AFalse;
#ifdef DNETCONN
	char *dnet_htoa();
	struct nodeent *np;
	struct dn_naddr *nlist, dnaddr, *dnaddrp, *dnet_addr();
	char *cp;
#endif
 
	ProgramName = argv[0];

	if ((aud = AFOpenAudioConn(NULL)) == NULL) {
	    fprintf(stderr, "%s:  unable to open display \"%s\"\n",
		    ProgramName, AFAudioConnName(NULL));
	    exit(1);
	}

	(void) AFSetErrorHandler(local_aferror);
  
	if (argc == 1) {
#ifdef DNETCONN
		setnodeent(1); /* keep the database accessed */
#endif
		sethostent(1); /* don't close the data base each time */
		list = AFListHosts(aud, &nhosts, &enabled);
		printf ("access control %s\n", 
			(enabled ? 
			 "enabled (only the following hosts are allowed)": 
			 "disabled (any host is allowed)"));
		if (nhosts != 0) {
		    for (i = 0; i < nhosts; i++ )  {
		      hostname = get_hostname(&list[i]);
		      if (hostname) {
			  printf ("%s", hostname);
		      } else {
#ifdef STREAMSCONN
			  print_streams_hostnames (list, nhosts);
#else
			  printf ("<unknown address in family %d>",
				  list[i].family);
#endif
		      }
		      if (nameserver_timedout)
			printf("\t(no nameserver response within %d seconds)\n",
			        NAMESERVER_TIMEOUT);
		      else printf("\n");
		    }
		    free(list);
		    endhostent();
		}
		exit(0);
	}
 
	if (argc == 2 && !strcmp(argv[1], "-help")) {
	    fprintf(stderr, "usage: %s [[+-]hostname ...]\n", argv[0]);
	    exit(1);
	}

	for (i = 1; i < argc; i++) {
	    arg = argv[i];
	    if (*arg == '-') {
	    
	        if (!argv[i][1] && ((i+1) == argc)) {
		    printf ("all hosts being restricted (access control enabled)\n");
		    AFEnableAccessControl(aud);
		} else {
		    arg = argv[i][1]? &argv[i][1] : argv[++i];
		    if (!change_host (aud, arg, AFalse)) {
			fprintf (stderr, "%s:  bad hostname \"%s\"\n",
				 ProgramName, arg);
		    }
		}
	    } else {
	        if (*arg == '+' && !argv[i][1] && ((i+1) == argc)) {
		    printf ("all hosts being allowed (access control disabled)\n");
		    AFDisableAccessControl(aud);
		} else {
		    if (*arg == '+') {
		      arg = argv[i][1]? &argv[i][1] : argv[++i];
		    }
		    if (!change_host (aud, arg, ATrue)) {
			fprintf (stderr, "%s:  bad hostname \"%s\"\n",
				 ProgramName, arg);
		    }
		}
	    }
	}
	AFCloseAudioConn(aud);
	exit(0);
	/*NOTREACHED*/
}

 

/*
 * change_host - edit the list of hosts that may connect to the server;
 * it parses DECnet names (expo::), Internet addresses (18.30.0.212), or
 * Internet names (expo.lcs.mit.edu); if 4.3bsd macro h_addr is defined
 * (from <netdb.h>), it will add or remove all addresses with the given
 * address.
 */

int
change_host(AFAudioConn *haud, char *name, ABool add)
{
  struct hostent *hp;
  AFHostAddress ha;
  static struct in_addr addr;	/* so we can point at it */
#ifdef DNETCONN
  struct dn_naddr *dnaddrp;
  struct nodeent *np;
  char *cp;
  static struct dn_naddr dnaddr;
#endif
  static char *add_msg = "being added to access control list";
  static char *remove_msg = "being removed from access control list";

#ifdef DNETCONN
  if ((cp = index (name, ':')) && (*(cp + 1) == ':')) {
    *cp = '\0';
    ha.family = AFamilyDECnet;
    if (dnaddrp = dnet_addr(name)) {
      dnaddr = *dnaddrp;
    } else {
      if ((np = getnodebyname (name)) == NULL) {
	  fprintf (stderr, "%s:  unble to get node name for \"%s::\"\n",
		   ProgramName, name);
	  return 0;
      }
      dnaddr.a_len = np->n_length;
      bcopy (np->n_addr, dnaddr.a_addr, np->n_length);
    }
    ha.length = sizeof(struct dn_naddr);
    ha.address = (char *)&dnaddr;
    if (add) {
	AFAddHost (haud, &ha);
	printf ("%s:: %s\n", name, add_msg);
    } else {
	AFRemoveHost (haud, &ha);
	printf ("%s:: %s\n", name, remove_msg);
    }
    return 1;
  }
#endif
#ifdef STREAMSCONN
  if (get_streams_address (name, &ha)) {
    if (add) {
	AFAddHost (haud, &ha);
	printf ("%s %s\n", name, add_msg);
    } else {
	AFRemoveHost (haud, &ha);
	printf ("%s %s\n", name, remove_msg);
    }
    return 1;
  }
#endif
#ifdef NEEDSOCKETS
  /*
   * First see if inet_addr() can grok the name; if so, then use it.
   */
  if (((int) (addr.s_addr = inet_addr(name))) != -1) {
    ha.family = AFamilyInternet;
    ha.length = sizeof(addr.s_addr);
    ha.address = (char *)&addr.s_addr;
    if (add) {
	AFAddHost (haud, &ha);
	printf ("%s %s\n", name, add_msg);
    } else {
	AFRemoveHost (haud, &ha);
	printf ("%s %s\n", name, remove_msg);
    }
    return 1;
  } 
  /*
   * Is it in the namespace?
   */
  else if (((hp = gethostbyname(name)) == (struct hostent *)NULL)
       || hp->h_addrtype != AF_INET) {
    return 0;
  } else {
    ha.family = AFamily(hp->h_addrtype);
    ha.length = hp->h_length;
#ifdef h_addr				/* new 4.3bsd version of gethostent */
    {
	char **list;

	/* iterate over the hosts */
	for (list = hp->h_addr_list; *list; list++) {
	    ha.address = *list;
	    if (add) {
		AFAddHost (haud, &ha);
	    } else {
		AFRemoveHost (haud, &ha);
	    }
	}
    }
#else
    ha.address = hp->h_addr;
    if (add) {
	AFAddHost (haud, &ha);
    } else {
	AFRemoveHost (haud, &ha);
    }
#endif
    printf ("%s %s\n", name, add ? add_msg : remove_msg);
    return 1;
  }
#endif
}


/*
 * get_hostname - Given an internet address, return a name (CHARON.MIT.EDU)
 * or a string representing the address (18.58.0.13) if the name cannot
 * be found.
 */

jmp_buf env;

static
char *get_hostname(AFHostAddress *ha)
{
#ifdef TCPCONN
  struct hostent *hp = NULL;
  char *inet_ntoa();
#endif
#ifdef DNETCONN
  struct nodeent *np;
  static char nodeaddr[16];
#endif

#ifdef TCPCONN
  if (ha->family == AFamilyInternet) {
    /* gethostbyaddr can take a LONG time if the host does not exist.
       Assume that if it does not respond in NAMESERVER_TIMEOUT seconds
       that something is wrong and do not make the user wait.
       gethostbyaddr will continue after a signal, so we have to
       jump out of it. 
       */
    nameserver_timedout = 0;
    signal(SIGALRM, nameserver_lost);
    (void) alarm(4);
    if (setjmp(env) == 0) {
      hp = gethostbyaddr (ha->address, ha->length, AF_INET);
    }
    (void) alarm(0);
    if (hp)
      return (hp->h_name);
    else return (inet_ntoa(*((struct in_addr *)(ha->address))));
  }
#endif
#ifdef DNETCONN
  if (ha->family == AFamilyDECnet) {
    if (np = getnodebyaddr(ha->address, ha->length, AF_DECnet)) {
      sprintf(nodeaddr, "%s::", np->n_name);
    } else {
      sprintf(nodeaddr, "%s::", dnet_htoa(ha->address));
    }
    return(nodeaddr);
  }
#endif
#ifdef STREAMSCONN
  return get_streams_hostname (ha);
#else
  return (NULL);
#endif
}

static signal_t nameserver_lost()
{
  nameserver_timedout = 1;
  longjmp(env, -1);
}

/* This should not be used! */
extern int _APrintDefaultError (AFAudioConn *, AFErrorEvent *, FILE *);
/*
 * local_aferror - local non-fatal error handling routine. If the error was
 * that an AFGetHosts request for an unknown address format was received, just
 * return, otherwise print the normal error message and continue.
 */
static int
local_aferror(AFAudioConn *haud, AFErrorEvent *rep)
{
    if ((rep->error_code == ABadAccess)
	&& (rep->request_code == A_ChangeHosts)) {
	fprintf (stderr, 
		 "%s:  must be on local machine to add or remove hosts.\n",
		 ProgramName);
	return 1;
    } else if ((rep->error_code == ABadAccess) && 
	       (rep->request_code == A_SetAccessControl)) {
	fprintf (stderr, 
	"%s:  must be on local machine to enable or disable access control.\n",
		 ProgramName);
	return 1;
    } else if ((rep->error_code == ABadValue) && 
	       (rep->request_code == A_ListHosts)) {
	return 1;
    }

    (void) _APrintDefaultError(haud, rep, stderr);
    return 0;
}


#ifdef STREAMSCONN
static Bool get_streams_address (name, hap) 
    char *name;
    AfHostAddress *hap;
{
  static char buf[128];
  char	 *ptr, *packet, *retptr, pktbuf[128];
  int	 n;


  if(_AFsTypeOfStream[ConnectionNumber(aud)]  == AF_LOCAL_STREAM)
  {
	hap->family = FamilyUname;
	hap->length = strlen(name) +1;
	hap->address = name;
	return True;
  }

  packet = pktbuf;
  ptr = &packet[2*sizeof(int)];

  n = strlen(name) + 1;
  ((xHostEntry *) ptr)->length = n;
  ptr += sizeof(xHostEntry);
  memcpy(ptr, name, n);

  retptr = packet;
   *(int *) retptr = n+sizeof(xHostEntry);
   *(int *) (retptr + sizeof(int)) = 1;

  if(GetNetworkInfo (ConnectionNumber(aud), NULL, ConvertNameToNetAddr, &packet, &retptr, NULL)<0)
           {
		return True;
           }
   hap->family = ((xHostEntry *) retptr)->family;
   hap->length = ((xHostEntry *) retptr)->length;
   hap->address = buf;
  
   if(hap->length > 127)
   	hap->length = 127;

   ptr = &retptr[sizeof(xHostEntry)];
   ptr[hap->length] = '\0';
   memcpy(buf, ptr, hap->length +1);
   return True;
}


print_streams_hostnames (list, nhosts)
    XHostAddress *list;
    int nhosts;
{
    int  m, n, i;
    char *ptr, *retptr;
    static char *packet = NULL;
    static int buflen = 0;
 
    if(buflen == 0)
		buflen = 512;

    m = 2 * sizeof(int);
    packet = (char *) malloc (buflen);
    if(packet == NULL){
	fprintf(stderr, "Cannot malloc %d chars \n", buflen);
	return;
	}
    ptr = &packet[m];

    for (i=0; i< nhosts; i++)
    {
	n = (((list[i].length + 3) >> 2) << 2) + sizeof(xHostEntry);
	m += n;
	if(m > buflen){
		buflen = m + 128;
		packet = (char *) realloc(packet, buflen);
    		if(packet == NULL){
			fprintf(stderr, "Cannot realloc %d chars \n", buflen);
			return;
			}
		}
	ptr = &packet[m - n];
	((xHostEntry *) ptr)->length  = list[i].length;
	((xHostEntry *) ptr)->family  = list[i].family;
	ptr += sizeof(xHostEntry);
	bcopy (list[i].address, ptr, list[i].length);
    }
    *(int *) packet = m;
    *(int *) (packet + sizeof(int)) = nhosts;
    if(_XsTypeOfStream[ConnectionNumber(aud)] != X_LOCAL_STREAM){
    	n =
 GetNetworkInfo (ConnectionNumber(aud), NULL,ConvertNetAddrToName, &packet, &retptr, &nhosts);
	if( n <= 0){
		fprintf(stderr, "No reply from the nameserver\n");
		return;
		}
	}
  	else retptr = &packet[2*sizeof(int)];
     m = 0;
     for(i=0; i<nhosts; i++){
	ptr = &retptr[m];
     	n = ((xHostEntry *) ptr)->length;
	n = (((n + 3) >> 2) << 2) + sizeof(xHostEntry);
	m += n;
     	ptr += sizeof(xHostEntry);
 	fprintf(stderr, "%s\n", ptr);	
 	}		
     free(retptr);
}

char *get_streams_hostname (ha)
    XHostAddress *ha;
{
  static char buf[128];
  char	 *ptr, *packet, pktbuf[128], *retptr;
  int	 n;

   if(_XsTypeOfStream[ConnectionNumber(aud)] == X_LOCAL_STREAM || ha->family == FamilyUname){
	return(ha->address);
  }

  packet = pktbuf;
  ptr = &packet[2*sizeof(int)];

  ((xHostEntry *) ptr)->length = ha->length;
  ((xHostEntry *) ptr)->family = ha->family;

  ptr += sizeof(xHostEntry);
  memcpy(ptr, ha->address, ha->length);

   retptr = packet;
   *(int *) retptr = ha->length+sizeof(xHostEntry);
   *(int *) (retptr + sizeof(int)) = 1;

  if(GetNetworkInfo (ConnectionNumber(aud), NULL, ConvertNetAddrToName, &packet, &retptr, NULL)<0)
           {
		ha->address[ha->length] = '\0';
		return(ha->address);
           }
   ptr = &retptr[sizeof(xHostEntry)];
   ptr[((xHostEntry *) retptr)->length] = '\0';

   memcpy(buf, ptr, ((xHostEntry *) retptr)->length +1);
   return(buf);
}

#endif
