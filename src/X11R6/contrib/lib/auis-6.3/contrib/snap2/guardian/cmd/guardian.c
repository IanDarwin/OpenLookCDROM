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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/guardian/cmd/RCS/guardian.c,v 2.109 1994/03/29 19:38:54 rr2b Exp $";
#endif

/*
  guardian.c -- daemon for SNAP servers
  Written: 17 September 1985
*/

static char GuardianVersion[] = "$Revision: 2.109 $ $Date: 1994/03/29 19:38:54 $";
#define GUARDIAN_PREIX_SKIP sizeof("xRevision")
extern char *AndrewDir();
#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */
extern char *inet_ntoa();

#include <andrewos.h>
#include <amsenv.h>
#include <errno.h>
#include <stdio.h>

#if SY_AIX221
#include <sys/signal.h>
#else /* SY_AIX221 */
#include <signal.h>
#endif /* SY_AIX221 */

#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */

#include <sys/stat.h>
#include <sys/param.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/wait.h>
#include <pwd.h>
#include <arpa/telnet.h>
#include <sys/ioctl.h>
#include <util.h>

#include <errprntf.h>
#include <snap.h>
#include <gasp.h>
#include <tokens.h>
#include <gstats.h>
#include <dropoff.h>
#include <sys/resource.h>

/* No Vice/authentication */
#define AFS_AUTH_DEFAULT   FALSE

#ifdef AFS_ENV
#include <afs/param.h>
#include <afs/cellconfig.h>
#include <rx/xdr.h>
/* New (Rx) authentication */
#include <afs/auth.h>
#undef AFS_AUTH_DEFAULT
#define AFS_AUTH_DEFAULT   TRUE
#define AUTH_KEYSIZE	    (sizeof(auth_EncryptionKey))
#endif /* AFS_ENV */

#ifdef M_UNIX
#define	SecureWare
#include <sys/security.h>
#include <prot.h>
#endif /* M_UNIX */

#include <fwddecl.h> /* forward declarations for this source file */
typedef unsigned char KEY[SNAP_KEYLEN];

#define NIL	(0L)

#define imin(xx1,xx2) (((xx1)<(xx2))?(xx1):(xx2))
#define imax(xx1,xx2) (((xx1)>(xx2))?(xx1):(xx2))

#define FALSE	0
#define TRUE	1
#define SERVICES_FILE "/etc/guardian.svc"

extern int errno, sys_nerr;
extern int SNAP_debugmask;
static int SNAP_serv_port;
extern struct passwd *getpwnam();
extern char *crypt(), *sys_errlist[];
extern long time();
extern double getla();

#define StrEql(s1, s2)	(strcmp(s1, s2) == 0)

#define DebugFlags  (variables[DEBUG_INDEX].value)

#ifdef DEBUG
/*static int DebugFlags;	/* Bit settings for debugging */
#define debug(value, msg) ((DebugFlags&(value)) ? printf msg : 0)

#else /* DEBUG */
#define debug(value, msg)

#endif /* DEBUG */

static bool RunAsUser;

#define AcceptRemoteRequests	(variables[REMOTE_INDEX].value)
#define ViceAuth		(variables[VAUTH_INDEX].value)

static bool RebootFlag;
#define RebootTime		(variables[REBOOT_INDEX].value)
#define Verbose 		(variables[VERBOSE_INDEX].value)

/*
 in statistics messages say logins are off if total users in all
 servers aboved TryLimitMaxUsers.
*/
long TryLimitMaxUsers = -1;
/* refuse new connections if serving this many*/
long LimitMaxUsers = -1;
/*time at which to start limiting to TryLimitMaxUsers*/
long LimitHardTime=0;

long MaxServers;
long Maxusers; 
long Users;

/* what uid guardian is authenticated as*/
long current_auth_uid = -1;
long current_auth_time;

static char *Permits;	    /* File name containing uids to accept */
static char *ServicesFile = SERVICES_FILE;  /* Additional services */

/*
	Settings for DebugFlags (or together for cumulative affect):

		1	Don't fork to become demon
		2	Debug SetAddress
		4	Debug NextRequest
		8	Debug CheckServers
		16	Debug fuseki
		32	Set SNAP_debugmask to a large #
		64	Debug ForkServerHead
		128	Debug authenticate
		256	Debug ServerDied
		512	Debug count
		1024	Debug ExecuteRequest
		2048	Debug BindtoPort
		4096	Debug GetServerRequest
		8192	Debug SameHost
		16384	Debug Token unpacking
*/

typedef struct sockaddr_in ADDRESS;	/* Our form of address */

typedef enum { AUTH_ONLY, UNAUTH_ONLY, BOTH } CONN_TYPE;

/* Service structure */
typedef struct {
    char	*name;		/* Name of service */
    CONN_TYPE	type;		/* Type of connections handled */
    bool	setuid; 	/* TRUE if guardian should setuid to user */
    int 	maxclients;	/* Maximum # clients serviced per server */
    bool	predefined;	/* TRUE if a predefined service */
    char	*exec;		/* String to exec service */
} SERVICE;

#define MAX_SERVICES 25

/*
Exec fields are sprintf strings.  They may contain at most one %s which will be replaced by AndrewDir
*/

static SERVICE services[MAX_SERVICES] =
	{ /* Predefined services */

	    { "BIOYA",          AUTH_ONLY,      TRUE,   200,    TRUE,
 				"/usr/snap/services/bioya" },

	    { "pcserver",       AUTH_ONLY,      TRUE,   200,    TRUE,
				"/usr/snap/services/pcserver" },
            /*for pc to authenticate only*/
	    { "messageserver",  AUTH_ONLY,      TRUE,   200,    TRUE,
				"/usr/snap/services/messageserver"},

	    { "MessageServer",  AUTH_ONLY,      TRUE,   200,    TRUE,
				"/usr/snap/services/MessageServer" },

	    { "TestMS",         AUTH_ONLY,      TRUE,   200,    TRUE,
				"/usr/snap/services/TestMS" },

	/* Always leave this entry at end */
	    { NIL }

	};

static int maxservices;

/* Request packets -- internal form */

typedef struct {
    ADDRESS	from;	/* Address of sender */
    int 	type;	/* Type of request */

	/* Rest of packet depends on type */

    int 		count;	/* Count for COUNT_RQ */

} REQUEST;

static struct {
    char    *name;
    long    value;	    /* Everything's an integer */
    int     (*inaction)();
    void     (*outaction)();
    char    *description;
} variables[] = {

    { "debug",      0,  assignhex,      printhex,
#define DEBUG_INDEX 0
				    "debug bit mask (hex)" },

    { "reboot",     0,  assignreboot,   printreboot,
#define REBOOT_INDEX 1
				    "reboot time (HH:MM, now, off)" },

    { "remote",     0,  assignbool,     printbool,
#define REMOTE_INDEX 2
				    "accept remote connections (T, F)" },

    { "time",       0,  NIL,            printtime,

				    "current time (read-only)" },
    { "verbose",    0,  assignbool,     printbool,
#define VERBOSE_INDEX 4
				    "verbose message output (T, F)" },

    { "version",    0,  NIL,            printversion,

				    "Guardian version (read-only)" },

    { "vice",       0,  assignbool,     printbool,
#define VAUTH_INDEX 6
				    "use Vice authentication (T, F)" },
    { "limit",       0,  assignlimit,     printlimit,
#define LIMIT_INDEX 7
			    "limit total users, try limit e.g. 7,2 off=-1,-1" },
    { "log",       0,  assignlog,     printlog,
#define LOG_INDEX 8
			    "log (off,filename)" },

};

#define NVARS	(sizeof(variables)/sizeof(variables[0]))

/* Structure with user info */
#define MAX_NAME 120
#define MAX_PASSWORD 400
struct USERINFO_R {
  bool valid;		/*this cache entry is valid*/
  bool want_auth;       /* Is this a cell guest uid */
  bool bad_password_rc; /*non zero if cache for bad password*/
    int 	uid;		/* User's Unix uid */
  long timeout;         /*when this entry times out*/
  char home[MAXPATHLEN+1]; /*home path*/
  int len;
  /*must be aligned*/
  char password[MAX_PASSWORD];
  char name[MAX_NAME];
  int type;
};
typedef struct USERINFO_R USERINFO,*USERINFO_pt;

#ifndef NUM_CACHE
#define NUM_CACHE 6
#endif /* NUM_CACHE */
USERINFO auth_cache[NUM_CACHE];

/* Name of this application */
static char GuardianName[] = "SNAP.GUARDIAN";

/* Global string for doing sprintf's */
static char ErrorText[2*MAXPATHLEN];

/* File descriptors for clients, servers, debugging & Leong statistics */
static int clientfd,serverfd, debugfd, statfd;

/* For saving debug connect descriptor before changing */
static int savedebug;

/* Client descriptor */
#define MAX_CLIENT_NAME 	100
#define NCONNECTIONS		10

typedef struct {
    char	name[MAX_CLIENT_NAME];	/* His Unix name */
    int 	nconns; 		/* # connections to remember */
    int 	nextconn;		/* Next slot to use */
    ADDRESS	connections[NCONNECTIONS];

} CLIENT;

/* Server description type */
typedef struct {
    SERVICE	*service;	/* Service this server provides */
    bool	authenticated;	/* Is this an authenticated server */
    bool	guestid;	/* Does this server have a cell guest uid ? */
    int 	count;		/* # clients guardian believes being served */
    KEY 	key;		/* Encryption key used for server */
    int 	pid;		/* Pid of server */
    long        start;          /*unix time of vfork*/
    CLIENT	client; 	/* Info about client(s) I am serving */
    ADDRESS	address;	/* IPC address of this server */
} SERVER;

/* Maximum possible # of servers that can be running */
#define MAXSERVERS	85

static SERVER servers[MAXSERVERS];


/* Some stuff for statistics collection */

static unsigned long start_time;

#define GCOUNT(xx_index) {st_cnt[xx_index]++;}

char *st_cnt_names[]={
#define STN_nextserverslot 0
#define NextServerSlot (st_cnt[STN_nextserverslot])
  "now_servers",
#define STN_connect_new 1
  "new_conn",
#define STN_connect_old 2
  "old_conn",
#define STN_log_connection 3
#define CONN_NUM st_cnt[STN_log_connection]
  "try_conn",
};
#define STN_MAX 3
long st_cnt[STN_MAX+1];

#include <demonlog.h>
/*time(0) to do next checkpoint*/
long log_checkpoint_time;
/*checkpoint log file this often (seconds)*/
#define LOG_CHECK_POINT_INTERVAL (10*60)

/*use current authorization for this long*/
#define AUTH_CACHE_TIMEOUT (30)

struct start_client_errors_R {
 int num_errors;
 int errno;
 int error;
 /*flag to say if this is a duplicate connection*/
 int is_duplicate;
 int pid;	/*pid of child*/
 int start;	/*child start time*/
 char *where;   /*text saying what when wrong*/
};
typedef struct start_client_errors_R start_client_errors;
start_client_errors sce;

static void clear_client_start_error()
{
    errno=0;
    sce.is_duplicate=FALSE;
    sce.num_errors=0;
    sce.errno=0;
    sce.error=0;
    sce.start=0;
    sce.pid= -1;
    sce.where="";
}

int set_gerror(gwhat,where)
int gwhat;  /*guardian error code*/
char *where;
{
    if(gwhat==0)gwhat=1;
    /*only keep first, most accurate error*/
    if(sce.num_errors++ != 0)
	return sce.error;
    /*remember the error code*/
    if(gwhat==0)gwhat=1;
    sce.errno=errno;
    sce.error=gwhat;
    sce.where=where;
    return sce.error;
}

static void child_sys(ecode,where)
int ecode;
char *where;
{
    if(ecode>=0)return;
    set_gerror(GASP_NO_FORK,where);
    _exit(EX_OSERR);
}

#ifdef AFS_ENV

static char *UidStorage;
static char **AuthorizedUsers;	    /* Array of permitted uid's */
static int NAuthorizedUsers;	    /* # authorized users */

static SetAuthorizedUsers()
{
    register int f, len, i;
    struct stat buf;
    register char *c;

    f = open(Permits, O_RDONLY);
    if (f < 0) {
	sprintf(ErrorText, "Can't open permits file: \"%s\": %%s", Permits);
	ReportSystemError(ERR_CRITICAL, NIL, ErrorText, errno);
    }
    if (fstat(f, &buf) < 0) {
	sprintf(ErrorText, "Can't stat permits file: \"%s\": %%s", Permits);
	ReportSystemError(ERR_CRITICAL, NIL, ErrorText, errno);
    }
    if (buf.st_size <= 1) {
	sprintf(ErrorText, "Permits file is too small: \"%s\"", Permits);
	ReportError(ERR_CRITICAL, NIL, ErrorText, NIL);
    }
    UidStorage = malloc(buf.st_size);
    if (UidStorage == NIL) {
	ReportNumError(ERR_CRITICAL, NIL,
		       "Out of storage for permits: %s", buf.st_size);
	exit(EX_UNAVAILABLE);
    }

    /* Read names into UidStorage */
    if (read(f, UidStorage, buf.st_size) < buf.st_size) {
	sprintf(ErrorText, "Read error on permits file: \"%s\": %%s", Permits);
	ReportSystemError(ERR_CRITICAL, NIL, ErrorText, errno);
    }
    close(f);

    /* Replace \n's with \0's & count uids */
    for (NAuthorizedUsers=0, c=UidStorage, len=buf.st_size; len>0; c++, len--)
	if (*c == '\n') {
	    *c = '\0';
	    NAuthorizedUsers++;
	}

    /* Point to uids from array */
    AuthorizedUsers = (char **) calloc(NAuthorizedUsers, sizeof(char *));
    if (AuthorizedUsers == NIL) {
	ReportNumError(ERR_CRITICAL, NIL,
		       "Out of storage for uid indexes: %d", NAuthorizedUsers);
	exit(EX_UNAVAILABLE);
    }
    AuthorizedUsers[0] = UidStorage;
    for (i=1, c=UidStorage; i<NAuthorizedUsers; c++)
	if (*c == '\0') AuthorizedUsers[i++] = (c+1);
}

static bool UserPermitted(uid)
register char *uid;
{
    register int i;

    for (i=0; i<NAuthorizedUsers; i++)
	if (StrEql(AuthorizedUsers[i], uid)) return TRUE;
    return FALSE;
}

#if 0
struct all_tokens_R {
    long bashme;
    ClearToken ctoken;
    long bashme2;
    SecretToken stoken;
    long bashmetoo;
};
typedef struct all_tokens_R gtokens,*gtokens_pt;
#endif /* 0 */

static void gclearauth()
{
    setpag();
}
#if 0
/*
   set tokens from clear and secret
*/
static int SetTokens(u,toks)
USERINFO *u;
gtokens_pt toks;
{
    int rc;
    if (!ViceAuth)return(0);

    /* 1st become the user */
    if (setreuid(0, u->uid)<0)
	return set_gerror(GASP_CANT_SETUID,"setreuid_become_user");
    rc = U_SetLocalTokens(1, &toks->ctoken, &toks->stoken);
    if (setreuid(0, 0)) exit(EX_OSERR);
    if (rc != 0)
	return set_gerror(GASP_NO_AUTHORIZATION,"u_setlocaltokens");
    return 0;
}

static int CellPwdAuth(u)
USERINFO_pt u;
{
    register int rc;
    register char *pwd, *cell;
    gtokens toks;
    int len=u->len;

    /* Format of auth is "<pwd>\0<cell>\0" */
    for (pwd=u->password; len>0 && *pwd!='\0'; len--, pwd++) ;
    /* Oops, bad format */
    if (len <= 0) return set_gerror(GASP_AUTH_FORMAT,"malformed_password");
    for (cell = ++pwd; len>0 && *cell!='\0'; len--, cell++) ;
    /* Oops, bad format */
    if (len <= 0) return set_gerror(GASP_AUTH_FORMAT,"malformed_cell_password");
    cell = pwd;
    pwd = u->password;

    if (!homeCell(cell))
	return set_gerror(GASP_NO_GUEST_ACCT,"no_home_cell");

    /* Try to authenticate */
    rc = InitializeRPC();
    if (rc != 0) return set_gerror(GASP_RPC_INIT,"no_rpc_init");
    rc = U_CellAuthenticate(u->name, pwd, cell, &toks.ctoken, &toks.stoken);
    if (rc != AUTH_SUCCESS)
	return set_gerror(GASP_NO_AUTHORIZATION,"u_cell_authenticate");
    return SetTokens(u,&toks);
}
static int VTokensAuth(u)
USERINFO *u;
{
    int code;
    gtokens toks;
    code = unpacktokens(u->password, &toks.ctoken, &toks.stoken,
			 (DebugFlags&16384), 0);
    if(!code)
	return set_gerror(GASP_NO_AUTHORIZATION,"unpacktokens");
    return SetTokens(u,&toks);
}
#endif /* 0 */

static int PwdAuth(u)
USERINFO *u;
{
    int rc;
    char err_text[1024];
    char userid[1024];
    char *p;
    debug(128,("[ViceAuthenticate(%s, ?, 0x%x)...\n",u->name, u));
    strcpy(userid, u->name);
    p=strchr(userid, '/');
    if(p) *p='\0';
    if(ka_UserAuthenticate(userid,
			   /*inst*/(char *)0,
			   /*realm*/(char *)0,
			   u->password,
			   /*setpag*/1,
			   err_text))
      return set_gerror(GASP_NO_AUTHORIZATION,"authentication");
    return 0;
}

static int MultiTokensAuth(u)
USERINFO_pt u;
{
    register char *ThisCell;
    register int tokens_id;

    ThisCell = GetCellName();
    if (ThisCell == NIL) return set_gerror(GASP_NO_CELL,"no_this_cell");
    tokens_id = ExtractCellID(u->password, u->len, ThisCell, 0 /*debug*/);
    if (tokens_id == -1)
	return set_gerror(GASP_NO_GUEST_ACCT,"extract_cell_id");
    if(u->uid != tokens_id)
	return set_gerror(GASP_NO_AUTHORIZATION,"uid_neq_viceid");

    /* Now, see if tokens are valid */
    if (setreuid(u->uid, 0)<0) /* make getuid() be roughly correct for old ktc.c */
	return set_gerror(GASP_CANT_SETUID,"setreuid_become_user");
    if (UnpackAndSetTokens(u->password,u->len, 0 /*debug*/, 1) < 0) {
	if (setreuid(0, 0)) exit(EX_OSERR);
	return set_gerror(GASP_NO_AUTHORIZATION,"unpackandsettokens");
    }
    if (setreuid(0, 0)) exit(EX_OSERR);
    return 0;
}

static int really_authenticate(u,from, remote)
USERINFO *u;
ADDRESS *from;
bool remote;
{
    debug(128, ("[authenticate(%s, (?, ?, %d), <%d,%d,%s>, %d,...)...\n",
		 u->name,
		 u->type,
		 from->sin_family,
		 ntohs(from->sin_port),
		 inet_ntoa(from->sin_addr.s_addr),
		 (int) remote));

    if (!u->valid && remote && Permits != NIL && !UserPermitted(u->name))
	return set_gerror(GASP_USER_PERMITS,"not_in_user_permits");

    /*
      set tokens
      */
    switch (u->type) {
	case 0:
	case GASP_PWD_STRING:
	    return PwdAuth(u);
#if 0
	case GASP_PWD_CELL:
	    return CellPwdAuth(u);
	case GASP_PWD_VTOKENS:
	    return VTokensAuth(u);
#endif /* Take this out when you're sure it's correct to remove it. :) */
	case GASP_PWD_MULTI_TOKENS:
	    return MultiTokensAuth(u);
	default:
	    return set_gerror(GASP_NO_AUTHORIZATION,"bad_auth_type");
    }
}

static int authenticate_if_requested(u,from,remote)
    USERINFO_pt u;
ADDRESS *from;
bool remote;
{
    int rc;
    if(!u->want_auth) {
	setpag();
	current_auth_time=0;
	return 0;
    }
    /*perhaps we are already authenticated as desired
      This allows a user to start a message server right after
      starting a pcserver without authenticating again.   
      */
    if((u->valid)&&
	(current_auth_uid==u->uid)&&
	((current_auth_time+AUTH_CACHE_TIMEOUT)>time(0))) {
	debug(16, (" using previous authentication\n"));
	return 0;
    }

    debug(16, (" Attempting authentication\n"));
    rc = really_authenticate(u,from,remote);
    u->valid=TRUE;
    u->bad_password_rc=rc;
    if (rc != 0) {
	set_gerror(rc,"authenticate");
	debug(16, (" Authentication failed\n"));
	current_auth_uid = -1;
    }
    else {
	debug(16, (" Authentication succeeded\n"));
	current_auth_uid = u->uid;
	current_auth_time = time(0L);
    }
    return rc;
}
#else /* AFS_ENV */
static int authenticate_if_requested(u,from,remote)
    USERINFO_pt u;
ADDRESS *from;
bool remote;
{
    return 0;
}
#endif /* AFS_ENV */

char *ghost()
{
#define NAMELEN 512
    static char name[NAMELEN];
    static bool virgin = TRUE;

    if (virgin)
	if (gethostname(name, NAMELEN) < 0)
	  ReportSystemError(ERR_CRITICAL, NIL, "gethostname", errno);
	else {
	    virgin = FALSE;
	    name[NAMELEN-1] = '\0';
	}
    return name;
}

static char MyAddress[4];	/* My machine address */

static bool GetMyAddress()
{
    static bool virgin = TRUE;
    register char *name;
    register struct hostent *me;

    if (!virgin) return TRUE;

    /* Get my host entry */
    name = ghost();
    if (name == NIL) {
	ReportError(ERR_CRITICAL, NIL,
		    "Can't find my name (in GetMyAddress)", NIL);
	return FALSE;
    }
    debug(8192, ("my name is %s\n", name));
    me = gethostbyname(name);
    if (me == NIL) {
	ReportError(ERR_CRITICAL, NIL,
		    "Can't find my host table entry: %s", name);
	return FALSE;
    }

    /* This is really anal -- check length of address */
    if (me->h_length != 4) {
	ReportNumError(ERR_CRITICAL, NIL, "Bad address length: %s", me->h_length);
	return FALSE;
    }
    debug(8192, ("my address is 0x%02x%02x%02x%02x\n",
		  (unsigned char) me->h_addr[0],
		  (unsigned char) me->h_addr[1],
		  (unsigned char) me->h_addr[2],
		  (unsigned char) me->h_addr[3]));

    /* Copy address into static storage */
    bcopy(me->h_addr, MyAddress, 4);
    virgin = FALSE;
    return TRUE;
}

char bool_to_char(abool)
int abool;
{
    if(abool)
	return 't';
    else
	return 'f';
}

void init_auth_cache()
{
    register int i;
    for(i=0;i<NUM_CACHE;i++)
	auth_cache[i].valid=FALSE;
}

void set_socket_reusable(fd)
int fd;
{int status;
 /* Allow socket to be reused if guardian goes away */
 status = setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, NIL, 0);
 if (status < 0 && errno == EINVAL)
 {
  /* Make the 4.3 call */
  int on = 1;
#ifndef sys_mac2
  status = setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on), 0);
#endif
 }
 if(status<0)
   ReportSystemError(ERR_CRITICAL, NIL, "set socket resuable", errno);
}

struct port_name_R {
  char *name;
  char *sproto;
  int iproto;
};
typedef struct port_name_R port_name,*port_name_pt;

#define PT_size 4
port_name udp_port_names[PT_size]={
#define PT_connect 0
  {"snap.guardian","udp",SOCK_DGRAM},
#define PT_server 1
  {"snap.guardian01", "udp", SOCK_DGRAM},
#define PT_debug 2
  {"snap.guardian02", "udp", SOCK_STREAM},
#define PT_stat 3
  {"snap.guardian03", "udp", SOCK_DGRAM}
};

port_name tcp_port_names[PT_size]={
  {"snap.guardiant", "tcp", SOCK_STREAM},
  {"snap.guardiant.debug", "tcp", SOCK_DGRAM},
  {"snap.guardiant.debug", "tcp", SOCK_STREAM},
  {"snap.guardiant.stat", "udp", SOCK_DGRAM}
};

port_name_pt port_names;

void main(argc, argv)
int argc;
char *argv[];
{
    int result, i;
    char *aDir;

#ifdef M_UNIX
    set_auth_parameters(argc,argv);
#endif
    /*protect log files and core dumps*/
    umask(077);
    /* Go somewhere we can dump core if necessary */
    chdir("/tmp");

    aDir = AndrewDir(NULL);

    init_auth_cache();

    for (i = 0; services[i].name != NULL; i++)  {
	char *t;

	t = malloc(strlen(services[i].exec) + strlen(aDir));
	sprintf(t, services[i].exec, aDir);
	if (strcmp(t, services[i].exec) != 0)  {
	    services[i].exec = t;
	}
	else  {
	    free(t);
	}
    }

    /* Check the machine-dependent configuration */
    amsconfig(argc, argv, "guardian");

    /* Process arguments */
    ProcessArgs(argc, argv);

    if (!RunAsUser) {
	register int uid;
	/*clear current authentication to start spawning users*/
#ifdef AFS_ENV 
	gclearauth();
#endif /* AFS_ENV  */
	/* Make sure I'm running as root */
	uid = geteuid();
	if (uid < 0)
	    ReportSystemError(ERR_CRITICAL, NIL, "Geteuid failed: %s", errno);
	if (uid != 0) {
	    ReportNumError(ERR_CRITICAL, NIL,
			   "Guardian not running as root: %s", uid);
	    exit(EX_OSERR);
	}
    }

    /* Initialize stuff */
    Initialize();

    if(SNAP_istcp())
      port_names=tcp_port_names;
    else
      port_names=udp_port_names;

    /* Bind to my address */
    clientfd = SetAddress(port_names+PT_connect);
    serverfd = SetAddress(port_names+PT_server);
    debugfd = SetAddress(port_names+PT_debug);
    statfd = SetAddress(port_names+PT_stat);

    DEALOG(("genrl,gstart\n"));

    /* Become a demon */
    Demonize();

    savedebug = -1;

    /* Initialize SNAP stuff */
    result = SNAP_GuardianInit();
    if (result != SNAP_SUCCESS) {
	ReportSnapError(ERR_CRITICAL, NIL,
			"SNAP_GuardianInit failed: %s", result);
	exit(EX_UNAVAILABLE);
    }

    /* Just loop & execute requests */
    Loop();
}

static void ProcessOption(arg)
register char *arg;
{
    extern int atoi();

    switch (*++arg) {
	case 'D':       /* Debugging */
#ifdef DEBUG
	    DebugFlags |= atoi(++arg);
	    if (DebugFlags == 0) DebugFlags = 0xffffffff;
#else /* DEBUG */
	    ReportError(ERR_WARNING, NIL,
			"Debugging not compiled", NIL);
#endif /* DEBUG */
	    break;
	case 'a':       SetReboot(arg+1);
	    break;
	case 'l':       ViceAuth = FALSE;
	    break;
	case 'p':       Permits = (*++arg != '\0' ? arg : "/etc/user.permits");
	    break;
	case 's':       ServicesFile = (*++arg != '\0' ? arg : SERVICES_FILE);
	    break;
	case 'r':       AcceptRemoteRequests = TRUE;
	    break;
	case 'R':       AcceptRemoteRequests = FALSE;
	    break;
	case 'u':       RunAsUser = TRUE;
	    break;
	case 'v':       Verbose = TRUE;
	    break;
	case 'n':
	    SetLimits(arg+1);
	    break;
	case 'q':
	    SetLog(arg+1);
	    break;
	default:	ReportError(ERR_WARNING, NIL,
				    "Unknown option: \'%c\'", *arg);
    }
}

static ProcessArgs(argc, argv)
int argc;
char *argv[];
{
    register int i;

    /* Initialize flags */
    DebugFlags = 0;
    ViceAuth = AFS_AUTH_DEFAULT;
#if SY_AIX221
    AcceptRemoteRequests = TRUE;
    Verbose = FALSE;
#else /* SY_AIX221 */
    AcceptRemoteRequests = FALSE;
    Verbose = TRUE;
#endif /* SY_AIX221 */
    RunAsUser = FALSE;
    RebootFlag = FALSE;
    Permits = NIL;

    for (i=1; i<argc; i++)
	switch (*argv[i]) {
	    case '-':
		ProcessOption(argv[i]);
		break;
	    default:
		errprintf(GuardianName, ERR_WARNING, NIL, NIL,
				  "Unknown argument: \"%s\"", argv[i]);
	}

#ifdef DEBUG
    /* Special hack for SNAP debugging */
    if ((DebugFlags & 32) != 0) SNAP_debugmask = 0xFFFF;
#endif /* DEBUG */

    /* See if permits file desired */
#ifdef AFS_ENV
    /* either this belongs within an ifdef - or SAU does not?? --ghoti 2/13/89 */
    if (Permits != NIL) SetAuthorizedUsers();
#endif /* AFS_ENV */
}

static char lc(c)
register char c;
{
    return (c >= 'A' && c <= 'Z') ? (c+('a'-'A')) : c;
}

static bool IgnoreCaseEql(s, t)
register char *s, *t;
{
    for (; *s!='\0' && *t!='\0'; s++, t++)
	if (lc(*s) != lc(*t)) return FALSE;

    return (*s == *t ? TRUE : FALSE);
}

static char * FindNextLine (b)
char *b;
{
    if (b == NULL)
	return (NULL);
    while (TRUE) {
	if (*b == '\n') {
	    *b = 0;
	    b++;
	    if (*b)
		return (b);
	    else
		return (NULL);
	}
	if (*b == 0) {
	    return (NULL);
	}
	b++;
    }
}

static char *GetToken (p)	/* Points *p to the next char after */
char **p;			/* the token and returns a pointer  */
{				/* to the start of the token.	    */
    char *t = *p;
    char *e;

    while (TRUE) {
	if (*t == 0) {
	    *p = t;
	    return (NULL);
	}

	if ((*t != '\t') && (*t != ' ')) {
	    e = t+1;
	    while ((*e != 0) && (*e != '\t') && (*e != ' '))
		e++;
	    if (*e != 0) {
		*e = 0;
		e++;
	    }
	    *p = e;
	    return (t);
	}

	t++;
    }
}

static char *SvcsStorage;
static void SetServicesTable()
{
    register int f;
    struct stat buf;
    int lineno, nextsvc;
    char *buffer;
    char *nextline;
    char *t;

    f = open(ServicesFile, O_RDONLY);
    if (f < 0)				/* Additional services are optional */
	return;
    if (fstat(f, &buf) < 0) {
	sprintf(ErrorText, "Can't stat \"%s\": %%s", ServicesFile);
	ReportSystemError(ERR_CRITICAL, NIL, ErrorText, errno);
    }
    if (buf.st_size <= 1) {
	sprintf(ErrorText, "File is too small: \"%s\"", ServicesFile);
	ReportError(ERR_CRITICAL, NIL, ErrorText, NIL);
    }
    SvcsStorage = malloc(buf.st_size+1);
    if (SvcsStorage == NIL) {
	ReportNumError(ERR_CRITICAL, NIL,
		       "Out of storage for services, needed %s bytes", buf.st_size);
    }
    SvcsStorage[buf.st_size] = 0;

    /* Read services into SvcsStorage */
    if (read(f, SvcsStorage, buf.st_size) < buf.st_size) {
	sprintf(ErrorText, "Read error on file: \"%s\": %%s", ServicesFile);
	ReportSystemError(ERR_CRITICAL, NIL, ErrorText, errno);
    }
    close(f);

    nextsvc = 0;
    while (services[nextsvc].name != NIL)
	nextsvc++;
    lineno = 1;
    buffer = SvcsStorage;

    while (buffer) {

	if (nextsvc >= MAX_SERVICES) {
	    sprintf (ErrorText, "%s line %d: table overflow",
		     ServicesFile, lineno);
	    ReportError (ERR_CRITICAL, NIL, ErrorText, NIL);
	    exit (EX_NOINPUT);
	}

	nextline = FindNextLine (buffer);

	if ((services[nextsvc].name = GetToken (&buffer)) != NULL) {

	    if ((t = GetToken (&buffer)) == NULL) {
		sprintf (ErrorText, "%s line %d: type field missing",
			 ServicesFile, lineno);
		ReportError (ERR_CRITICAL, NIL, ErrorText, NIL);
		exit (EX_NOINPUT);
	    }

	    if (IgnoreCaseEql (t, "AUTH_ONLY"))
		services[nextsvc].type = AUTH_ONLY;
	    else if (IgnoreCaseEql (t, "UNAUTH_ONLY"))
		services[nextsvc].type = UNAUTH_ONLY;
	    else if (IgnoreCaseEql (t, "BOTH"))
		services[nextsvc].type = BOTH;
	    else {
		sprintf (ErrorText, "%s line %d: invalid type field \"%s\"",
			 ServicesFile, lineno, t);
		ReportError (ERR_CRITICAL, NIL, ErrorText, NIL);
		exit (EX_NOINPUT);
	    }

	    if ((t = GetToken (&buffer)) == NULL) {
		sprintf (ErrorText, "%s line %d: setuid field missing",
			 ServicesFile, lineno);
		ReportError (ERR_CRITICAL, NIL, ErrorText, NIL);
		exit (EX_NOINPUT);
	    }

	    if (IgnoreCaseEql (t, "TRUE"))
		services[nextsvc].setuid = TRUE;
	    else if (IgnoreCaseEql (t, "FALSE"))
		services[nextsvc].setuid = FALSE;
	    else {
		sprintf (ErrorText, "%s line %d: invalid setuid field \"%s\"",
			 ServicesFile, lineno, t);
		ReportError (ERR_CRITICAL, NIL, ErrorText, NIL);
		exit (EX_NOINPUT);
	    }

	    if ((t = GetToken (&buffer)) == NULL) {
		sprintf (ErrorText, "%s line %d: maxclients field missing",
			 ServicesFile, lineno);
		ReportError (ERR_CRITICAL, NIL, ErrorText, NIL);
		exit (EX_NOINPUT);
	    }

	    if ((services[nextsvc].maxclients = atoi (t)) == 0) {
		sprintf (ErrorText, "%s line %d: invalid maxclients field \"%s\"",
			 ServicesFile, lineno, t);
		ReportError (ERR_CRITICAL, NIL, ErrorText, NIL);
		exit (EX_NOINPUT);
	    }

	    services[nextsvc].predefined = FALSE;

	    if ((services[nextsvc].exec = GetToken (&buffer)) == NULL) {
		sprintf (ErrorText, "%s line %d: exec field missing",
			 ServicesFile, lineno);
		ReportError (ERR_CRITICAL, NIL, ErrorText, NIL);
		exit (EX_NOINPUT);
	    }

	}

	buffer = nextline;
	lineno++;
	nextsvc++;
	maxservices = nextsvc;
    }
}

/********************\
*		     *
*  Error reporting   *
*		     *
  \********************/

static char GuardianAddress[] = "guardian";

static void MailError(msg, arg)
char *msg, *arg;
{
    register int rc;
    register FILE *f;
    extern char *arpadate();
    static char errorfile[] = "/tmp/gerrorXXXXXX";
    char *to[2];
#if SY_AIX221
    return;
#else /* SY_AIX221 */

    /* Construct message in temporary file */
    mktemp(errorfile);
    f = fopen(errorfile, "w");
    if (f == NIL) {
	safeperror("[SNAP.GUARDIAN] Can't open mail error file: %s");
	return;
    }

    /* Write mail header */
    fprintf(f, "From: SNAP Guardian <%s>\n", GuardianAddress);
    fprintf(f, "To: Guardian Maintainer <%s>\n", GuardianAddress);
    fprintf(f, "Date: %s", arpadate());
    fputs("Subject: ", f);
    fprintf(f, msg, arg);
    fputs("\n\n", f);

    fprintf(f, "Error from guardian running on host %s\n", ghost());
    fclose(f);

    /* Now send the mail */
    to[0] = GuardianAddress;
    to[1] = NIL;
    rc = dropoff(to, errorfile, NIL, NIL, 0);
    if (rc!=D_OK && rc!=D_OK_WARN) {
	errprintf(GuardianName, ERR_CRITICAL, NIL, NIL,
		  "Dropoff failed: %s", Dropoff_ErrMsg);
    }
#endif /* SY_AIX221 */
}

static ReportError(level, id, msg, arg)
int level;
char *id, *msg, *arg;
{
    errprintf(GuardianName, level, NIL, id, msg, arg);
    MailError(msg, arg);
}

static ReportSystemError(level, id, msg, err)
int level;
char *id, *msg;
int err;
{   char unknown[20];

if (err <= sys_nerr)
    ReportError(level, id, msg, sys_errlist[err]);
else {
    sprintf(unknown, "%d", err);
    ReportError(level, id, msg, unknown);
}
    exit(1);
}

static safeperror(msg)
char *msg;
{
    char unknown[20];

    if (errno <= sys_nerr)
	errprintf(GuardianName, ERR_CRITICAL, NIL, NIL, msg, sys_errlist[errno]);
    else {
	sprintf(unknown, "%d", errno);
	errprintf(GuardianName, ERR_CRITICAL, NIL, NIL, msg, unknown);
    }
}

static ReportNumError(level, id, msg, n)
int level;
char *id, *msg;
int n;
{
    char num[20];

    sprintf(num, "%d", n);
    ReportError(level, id, msg, num);
}

static ReportSnapError(level, id, msg, code)
int level;
char *id, *msg;
int code;
{
    ReportNumError(level, id, msg, code);
}

#if 0
#ifdef AFS_ENV
static ReportAuthError(level, id, msg, code)
int level;
char *id, *msg;
int code;
{
    ReportNumError(level, id, msg, code);
}
#endif /* AFS_ENV */
#endif /* Remove this when you're sure it's history. */

static void InitServices()
{
    for (maxservices=0; services[maxservices].name!=NIL; maxservices++) ;
}

static Initialize()
{
    struct osi_Times tp;

    /* Ignore broken pipes, CheckServers will pick up death */
    signal(SIGPIPE, SIG_IGN);

    /* Disassociate this process from

      /* No servers yet */
      NextServerSlot = 0;

      /* Initialize services structure */
      InitServices();
      SetServicesTable();

      /* Seed random # generator */
      srand((int) time(0));

      /* Statistics collection */
      (void) GetMyAddress();
      MaxServers = 0;
      Users = 0;
      Maxusers = 0;
      if (osi_GetTimes(&tp) < 0)
	  ReportSystemError(ERR_CRITICAL, NIL, "Gettimeofday failed: %s", errno);
      start_time = (unsigned long) tp.Secs;
}

static void Demonize()
{
    register int pid, fd;
#ifdef DEBUG
    if ((DebugFlags & 1)!=0) {
	puts("[SNAP.GUARDIAN] Not forking");
	return;
    }
#endif /* DEBUG */
    pid = fork();
    if (pid < 0)
	ReportSystemError(ERR_CRITICAL, NIL,
			  "Can't fork to become demon: %s", errno);
    if (pid > 0) exit(EX_OK);
    NEWPGRP();
    /* Redirect stdout & stderr */
    if (RunAsUser) return;
    fd = open("/dev/console", O_WRONLY, 0644);
    if (fd>=0) { /* If I could not open /dev/console, I just hope for the best */
	dup2(fd, 1);
	dup2(fd, 2);
	setlinebuf(fdopen(1, "w"));
	setlinebuf(fdopen(2, "w"));
	close(fd);
    } else
	ReportSystemError(ERR_CRITICAL, NIL, "can't open /dev/console: %s", errno);
#if SY_AIX221
    if ((fd = open("/dev/tty")) > 0) {
	ioctl(fd, TIOCNOTTY, 0);
	close(fd);
    }
#endif
}

static bool GetServerRequest(fd, request)
int fd;
register REQUEST *request;
{
    int len;
    SERVER_PACKET packet;

    len = sizeof request->from;
    if (recvfrom(fd, &packet, SERVER_PACKET_SIZE, 0, &request->from, &len) < 0)
	ReportSystemError(ERR_CRITICAL, NIL, "RECVFROM servers failed: %s", errno);

    debug(4096,
	   ("[GetServerRequest: <%d, %d>]\n",
	    packet.type, packet.count));

    switch (packet.type) {
	case COUNT_RQ:	request -> count = packet.count;
	    break;
	default:	return FALSE;
    }
    request -> type = packet.type;
    return TRUE;
}

static char *ParseReboot(value)
register char *value;
{
    int hour, min;
    long now;
    register struct tm *tm;

    /* First check format -- `now', `off' or HH:MM */
    if (StrEql(value, "now")) {
	RebootFlag = TRUE;
	return NIL;
    }
    if (StrEql(value, "off")) {
	RebootFlag = FALSE;
	RebootTime = 0;
	return NIL;
    }
    if (strlen(value) != 5 || value[2] != ':')
	return "Bad time format";

    /* Extract hour & minutes */
    sscanf(value, "%2d:%2d", &hour, &min);
    if (hour > 24 || min >= 60) return "Bad time";
    now = time(0);
    tm = localtime(&now);
    if (hour < tm->tm_hour || (hour == tm->tm_hour && min < tm->tm_min))
	hour += 24;	/* It's tomorrow */

    /* (hour, min) = (hour, min) - (tm_hour, tm_min) */
    if (min < tm->tm_min) {
	min += 60;
	hour--;
    }
    min -= tm->tm_min;
    hour -= tm->tm_hour;
    now += (min*60) + (hour*3600);
    RebootTime = now;
    return NIL;
}

static SetReboot(arg)
char *arg;
{
    register char *answer;

    answer = ParseReboot(arg);
    if (answer != NIL) {
	errprintf(GuardianName, ERR_CRITICAL, NIL, NIL,
		  "Fatal error, invalid value to -a: %s", answer);
	exit(1);
    }
}

static void kill_children()
{register int i;
for (i=0; i<NextServerSlot; i++)
    kill(servers[i].pid,SIGTERM);
}

static void reboot()
{
    static char *reboots[] = { "/etc/reboot", "/bin2/reboot" };
#define NREBOOTS    (sizeof(reboots)/sizeof(reboots[0]))
    register int i;

    errprintf(GuardianName, ERR_CRITICAL, NIL, NIL,
	       "Rebooting by request in 45 seconds");
    DEALOG(("genrl,reboot1\n"));
    kill_children();
    sleep(45);
    CheckServers();
    DEALOG(("genrl,reboot2,%d\n",NextServerSlot));
    deal_flush();
    for (i=0; i<NREBOOTS; i++) {
	execl(reboots[i], reboots[i], 0);
	errprintf(GuardianName, ERR_CRITICAL, NIL, NIL,
		  "Can't execute requested service %s: %d", reboots[i], errno);
    }
    ReportError(ERR_CRITICAL, NIL, "Can't reboot", NIL);
}

static bool SigChildRcvd;
static struct timeval SelectTime;

static ChildAction()
{
    SigChildRcvd = TRUE;
    SelectTime.tv_sec = 0;
    SelectTime.tv_usec = 0;
}

#define INF	99999999

long get_reboot_time()
{
  return RebootTime;
}

static void CheckReboot()
{
    if (!RebootFlag && RebootTime > 0) {
	long now;
	now = time(0);
	if (now >= RebootTime) {
	    RebootFlag = TRUE;
	    RebootTime = 0;
	} else {
	    SelectTime.tv_sec = RebootTime - now;
	    SelectTime.tv_usec = 0;
	    return;
	}
    }

    /* Check reboot flag */
    if (RebootFlag) {
	reboot();		    /* Might not return */
	RebootFlag = FALSE;	    /* Just in case it does */
    }

    SelectTime.tv_sec = INF;
    SelectTime.tv_usec = 0;
}

static void CheckLog()
{
    long now=time(0);
    if(now>=log_checkpoint_time) {
	register int i;
	for(i=0;i<=STN_MAX;i++)
	    DEALOG(("genrl,stat,%s,%ld\n",st_cnt_names[i],st_cnt[i]));
	DEALOG(("genrl,load,%11.3e\n",getla(2)));
	deal_log_your_self(); /*write log package logging about itself*/
	deal_flush();
	log_checkpoint_time=now+LOG_CHECK_POINT_INTERVAL;
    }
    SelectTime.tv_sec = imin((log_checkpoint_time-now),SelectTime.tv_sec);
    SelectTime.tv_sec = imax(SelectTime.tv_sec,1);
}


static void NextRequest(request)
REQUEST *request;
{
    debug(4, ("[NextRequest\n"));

    for (;;) {
	int readfds, exceptfds, nfds, omask;

	/* Wait for something on any port */
	readfds = (1<<clientfd) | (1<<serverfd) | (1<<debugfd) | (1<<statfd);
	exceptfds = readfds;

	omask = sigblock(1<<(SIGCHLD-1));	/* P() */
	if (SigChildRcvd) {
	    /* See if any servers have died */
	    debug(4, ("\tChecking servers\n"));
	    CheckServers();
	    SigChildRcvd = FALSE;
	}

	CheckReboot();				/* Also sets timeout */
	sigsetmask(omask);			/* V() */

	CheckLog();
	debug(4, ("\tSelect, mask=0x%x, timeout=<%d,%d>...\n",
		  readfds, SelectTime.tv_sec, SelectTime.tv_usec));
	errno=0;	/*clear for logging this request*/
#ifdef M_UNIX
	nfds = select(8, &readfds, NIL, NIL, &SelectTime);
	exceptfds = 0;
#else
	nfds = select(32, &readfds, NIL, &exceptfds, &SelectTime);
#endif

	/* See if child signal received */
	if (SigChildRcvd || (nfds < 0 && errno == EINTR) || (nfds == 0))
	    continue;	/* Top of loop will get it */

	if(nfds < 0)
	    ReportSystemError(ERR_CRITICAL, NIL, "Select failed: %s", errno);
	debug(4, ("\t...nfds=%d\n", nfds));


	{
	    int action;

	    /* Check for exceptional condition */
	    if (exceptfds != 0) {
		errprintf(GuardianName, ERR_CRITICAL, NIL, NIL,
			  "EXCEPTFDS: 0x%x", exceptfds);
		abort();
	    }

	    /* See who action is for */
	    action = readfds;

	    if (action & (1<<clientfd)) {
		SNAP_CPARMS parms;
		debug(4,("client startup"));
		parms.maxtime = 20;
		parms.timeout = 1;
		parms.encryptlevel = 0;
		SNAP_GetAuthReq(clientfd,&parms, fuseki);
	    } else

		if (action & (1<<serverfd)) {
		    debug(4, ("\tAction from a server]\n"));
		    if (!GetServerRequest(serverfd, request))
			;	/* It was bogus, ignore it */
		    else
			return;
		} else

		    if (action & (1<<debugfd)) {
			DebugSocket();
		    } else
			if (action & (1<<statfd)) {
			    debug(4, ("\tStatistics port]\n"));
			    SendStats();
			} else	/* Impossible */ {
			    ReportNumError(ERR_CRITICAL, NIL, "Impossible select result: %d", action);
			    exit(EX_OSERR);
			}
	}
    }
}

/* Execute requests */

static Loop()
{
#ifdef POSIX_ENV
    struct sigaction act;
    sigemptyset(&act.sa_mask);
    act.sa_handler=ChildAction;
    sigaction(SIGCHLD, &act, NULL);
#else
    SigChildRcvd = FALSE;
    signal(SIGCHLD, ChildAction);	/* For children dying */
#endif
    for (;;) {
	REQUEST request;

	NextRequest(&request);
	ExecuteRequest(&request);
    }
}

static ExecuteRequest(request)
REQUEST *request;
{
    debug(1024, ("[ExecuteRequest, type = %d\n", request->type));
    /* See what type of packet */
    switch (request->type) {
	case COUNT_RQ:	count(request);
	default:	break;	/* Bogus, ignore it */
    }
    debug(1024, ("]\n"));
}


static SERVICE *FindService(name)
register char *name;
{
    register int i;

    for (i=0; i<maxservices; i++)
	if (StrEql(name, services[i].name))
	    return &services[i];
    return NIL;
}

static SERVER *FindServerByName(service, u)
register SERVICE *service;
USERINFO_pt u;
{
    register int i;
    if(SNAP_istcp())
      return NIL;

    for (i=0; i<NextServerSlot; i++)
	if (StrEql(servers[i].service->name, service->name) &&
	    servers[i].authenticated == u->want_auth &&
	    (!u->want_auth || StrEql(servers[i].client.name, u->name)))

	    return &servers[i];

    return NIL;
}

static bool AddrEql(a1, a2)
register struct sockaddr_in *a1, *a2;
{
    return (a1->sin_port == a2->sin_port) &&
      (a1->sin_addr.s_addr == a2->sin_addr.s_addr);
}

static SERVER *FindServerByAddress(address)
register ADDRESS *address;
{
    register int i;

    for (i=0; i<NextServerSlot; i++)
	if (AddrEql(&servers[i].address, address))
	    return &servers[i];

    return NIL;
}

static bool OkToCreateNewServer(service, u)
SERVICE *service;
USERINFO_pt u;
{
    /* if really no free server slots doesn't matter who you are*/
    if (NextServerSlot >= MAXSERVERS)
	return FALSE;
    /*unauth servers are small and inportant, always allow*/
    if((!u->want_auth)||(LimitMaxUsers == -1))
	return TRUE;
    /*if above hard limit then we are full*/
    if (NextServerSlot >= LimitMaxUsers)
	return FALSE;
    /*if below target limit number allow new server*/
    if ((TryLimitMaxUsers == -1) ||
	 (NextServerSlot <= TryLimitMaxUsers))
	return TRUE;
    /*This request is above try limit but below hard limit.
      Only allow it while giving the loadbalanceing servers
	  time to notice we are full.
	  */
    if(time(0)>=LimitHardTime)
	return FALSE;
    return TRUE;
}

static bool CanAcceptNewClient(server)
SERVER *server;
{
    return (server->count) < (server->service->maxclients);
}

struct stat_stuff_R {
#ifndef sys_telmat
    struct rusage usage;
#endif
    int pid;
    int start;       /*time process started*/
    char *user_name;
    int s_termsig;
    int s_coredump;
    int s_retcode;
    int s_stopval;
    int s_stopsig;
};
typedef struct stat_stuff_R stat_stuff,*stat_stuff_pt;

void log_death(st)
stat_stuff_pt st;
{
#if defined(sys_telmat)
    DEALOG(("death,%d,%d,%d,%d,%d,%d,%s,%ld\n",
	     st->pid,
	     st->s_termsig,
	     st->s_retcode,
	     st->s_stopval,
	     st->s_stopsig,
	     st->user_name,
	     st->start));
#else /* sys_telmat */
 DEALOG(("death,%d,%d,%d,%d,%d,%d,%s,%ld,%ld,%ld,%ld,%ld,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n",
	     st->pid,
	     st->s_termsig,
	     st->s_retcode,
	     st->s_stopval,
	     st->s_stopsig,
	     st->user_name,
	     st->start,
	     st->usage.ru_utime.tv_sec,
	     st->usage.ru_utime.tv_usec,
	     st->usage.ru_stime.tv_sec,
	     st->usage.ru_stime.tv_usec,
	     st->usage.ru_maxrss,
	     st->usage.ru_ixrss,
	     st->usage.ru_idrss,
	     st->usage.ru_isrss,
	     st->usage.ru_minflt,
	     st->usage.ru_majflt,
	     st->usage.ru_nswap,
	     st->usage.ru_inblock,
	     st->usage.ru_oublock,
	     st->usage.ru_msgsnd,
	     st->usage.ru_msgrcv,
	     st->usage.ru_nsignals,
	     st->usage.ru_nvcsw,
	     st->usage.ru_nivcsw));
#endif
}

static void AServerDied(server,st)
SERVER *server;
stat_stuff_pt st;
{
    char name[MAX_CLIENT_NAME];
    st->start=server->start;
    st->user_name=server->client.name;
    log_death(st);

    /* Save name to see if last process for user */
    strcpy(name, server->client.name);

    /* Free up its slot -- by copying server in last slot down */
    if (&servers[--NextServerSlot] != server)
	*server = servers[NextServerSlot];
    if (NewUser(name)) Users--;
}

static void ServerDied(st)
stat_stuff_pt st;
{
    register int i;

    debug(256, ("[ServerDied: %d...",st->pid));
    /* Find this server */
    for (i=0; i<NextServerSlot; i++)
	if (servers[i].pid == st->pid) {
	    debug(256, ("found at %d]\n", i));
	    AServerDied(&servers[i],st);
	    return;
	}
    /*unknown child died*/
    st->start=0;
    st->user_name="!unknown!";
    log_death(st);
}

static int BindtoPort(sin)
register struct sockaddr_in *sin;
{
    int fd;
    int sock_len;

    debug(2048, ("[BindtoPort..."));
    if (!GetMyAddress())
	return (set_gerror(GASP_NO_PORT,"getmyaddress"),-1);
    fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0)
	return (set_gerror(GASP_NO_PORT,"get_local_socket"),-1);

    bzero(sin, sizeof *sin);
    sin -> sin_family = AF_INET;
    bcopy(MyAddress,&sin->sin_addr, 4);
    sin -> sin_port = 0;
    if ((bind(fd,sin, sizeof *sin)) < 0) {
	close(fd);
	set_gerror(GASP_NO_PORT,"bind_client_socket");
	return -1;
    }
    sock_len=sizeof(*sin);
    if((getsockname(fd,sin,&sock_len))<0) {
	close(fd);
	set_gerror(GASP_NO_PORT,"getsocketname");
	return -1;
    }
    debug(2048,
	   ("succeeded => <%d, %d,%s>]\n",
	    sin->sin_family, ntohs(sin->sin_port),
	    inet_ntoa(sin->sin_addr.s_addr)));
    return fd;
}

/*  Read & write indices of pipe */
#define RSIDE	0
#define WSIDE	1

static bool ForkServerHead(s,u, key, code)
int s;
USERINFO *u;
KEY key;
int *code;
{
    int pid, fds[2], sfd;
    int pipe_broke=0;

    debug(64, ("[ForkServerHead(%d, %s, 0x%x, %d, 0x%x, 0x%x)\n",
		s,u->name, u, u->want_auth, key, code));
    /* 1st, create pipe */
    if (pipe(fds) < 0) {
	*code = set_gerror(GASP_NO_PIPE,"pipe_create");
	return FALSE;
    } else
	debug(64, ("\tCreated pipe: %d, %d\n", fds[0], fds[1]));

    /* Bind to available port */
    if(SNAP_istcp())
      sfd = SNAP_serv_port;
    else
      sfd = BindtoPort(&servers[s].address);
    if (sfd < 0) {
	close(fds[0]);
	close(fds[1]);
	*code = set_gerror(GASP_NO_PORT,"bind_to_port");
	return FALSE;
    } else
	debug(64, ("\tBound to port, socket=%d\n", sfd));

    /* Then fork */
    deal_willfork();	    /*log chached pid may change*/
    /*warning:child logging code depends on this being vfork not fork*/
    pid = osi_vfork();
    sce.pid=pid;
    if (pid < 0) {
        if(!SNAP_istcp())close(sfd);
	*code = set_gerror(GASP_NO_FORK,"vfork_call");
	return FALSE;
    }
    if (pid == 0)		/* I am child */
	ServerHead(servers[s].service, u, fds, sfd);
    /* Call never returns */

    /* I'm the parent ... */

    /* Close read side of pipe & server socket */
    close(fds[RSIDE]);
    if(!SNAP_istcp())close(sfd);

    /* Save pid */
    servers[s].pid = pid;

    /* Send key to new server over pipe */
    pipe_broke=(write(fds[WSIDE], key, SNAP_KEYLEN)!=SNAP_KEYLEN);
    if((close(fds[WSIDE])!=0))pipe_broke=TRUE;
    if(pipe_broke) {
	set_gerror(GASP_NO_FORK,"pipe_broke");
	return FALSE;
    }

    debug(64, ("]\n"));
    return TRUE;
}

static void NewClient(server, from)
register SERVER *server;
ADDRESS *from;
{
    register int i, next;
    register CLIENT *c;

    /* See if this REALLY is a new client */
    c = &server -> client;
    for (i=0; i<c->nconns; i++)
	if (AddrEql(from, &c->connections[i]))
	    return;	/* We've seen this guy */

    /* It is a new connection */
    server -> count++;
    if (server->client.nconns < NCONNECTIONS) ++server->client.nconns;
    next = server->client.nextconn;
    server->client.connections[next] = *from;	/* Use next slot */
    if (++next >= NCONNECTIONS) next = 0;	/* Point to next */
    server->client.nextconn = next;
}

static SERVER *NewServer(service,u, code, key)
register SERVICE *service;
USERINFO *u;
int *code;
KEY key;
{
    register int i;
    /* Find vacant server entry */
    /* Initialize entry */
    if (strlen(u->name) >= MAX_CLIENT_NAME) {
	*code = set_gerror(GASP_CLIENT_TOO_LONG,"client_name_to_big");
	return NIL;
    }
    if (NextServerSlot >= MAXSERVERS) {
	/* No room left */
	*code = set_gerror(GASP_TOO_MANY_SERVERS,"server_slots_full");
	return NIL;
    }
    if (NewUser(u->name) && ++Users>Maxusers) Maxusers = Users;

    i = NextServerSlot++;	/* Reserve this one */
    if (NextServerSlot > MaxServers) MaxServers = NextServerSlot;
    servers[i].guestid = !u->want_auth;
    strcpy(servers[i].client.name,u->name);	/* Fill in user's name */
    servers[i].client.nconns = 0;
    servers[i].client.nextconn = 0;
    servers[i].service = service;
    servers[i].count = 0;
    servers[i].authenticated = u->want_auth;
    servers[i].start=time(0);
    sce.start=servers[i].start;
    MakeKey(key, u);
    bcopy(key, servers[i].key, SNAP_KEYLEN);

    LimitHardTime=time(0)+(60);

    if (ForkServerHead(i,u, key, code))
	return &servers[i];
    else {
	NextServerSlot--;
	if (NewUser(u->name)) Users--;
	return NIL;
    }
}

static bool AuthenticationRequested(password)
char *password;
{
    return password != NIL;
}

/*
  return an autherization slot to use
  */
int auth_use_next()
{static int next_to_use;
if((!auth_cache[next_to_use].valid) ||
    (!auth_cache[next_to_use].want_auth))
    return next_to_use;
next_to_use++;
if(next_to_use>=NUM_CACHE)
    next_to_use=0;
return next_to_use;
}

int lookup_auth_cache(result,service,name,password,len,type)
USERINFO_pt *result;
SERVICE *service;
char *name;
char *password;
int len;
int type;
{
    USERINFO_pt cur;
    struct passwd *pw;
    int i;
    long now=time(0);
#ifdef M_UNIX
    struct pr_passwd *prp;
#endif

    /*Will this request be authenticated?*/
    if((service->type==UNAUTH_ONLY)||
	((service->type==BOTH)&& (!AuthenticationRequested(password)))) {
	cur= &auth_cache[auth_use_next()];
	cur->valid=TRUE;
	cur->bad_password_rc=0;
	strcpy(cur->name,"guest");
	cur->type=GASP_PWD_STRING;
	cur->len=2;
	strcpy(cur->password,"X");
	cur->want_auth=FALSE;
	cur->uid=1;
	cur->timeout=0;
	goto found_entry;
    }

    /*user wants guest, server wants auth?*/
    if(!AuthenticationRequested(password))
	return set_gerror(GASP_MISMATCH,"authmismatch");

    for(cur=auth_cache,i=0;i<NUM_CACHE;i++,cur++)
	if(cur->valid)
	    if(cur->timeout<now)
		cur->valid=FALSE;
	    else if(StrEql(name,cur->name)&&
		    (type==cur->type)&&
		    (len==cur->len) &&
		    (bcmp(password,cur->password,len)==0))
		goto found_entry;

    /*not found make a new entry*/
    cur= &auth_cache[auth_use_next()];
    cur->valid=FALSE;
    i=strlen(name);
    if((i>=MAX_NAME)||(i<=0))
	return set_gerror(GASP_CANT_FIND_USER,"bad_user_name");
    strcpy(cur->name,name);
    /* the name might be name/cell which getpwnam doesn't like*/
    {char pure_name[MAX_NAME];
    register char *scn_in=cur->name;
    register char *scn_out=pure_name;
    register char ch;
    while(((ch=(*scn_in++))!=0)&&
	   (ch!='/'))
	*scn_out++ = ch;
    *scn_out=0;
    pw = getpwnam(pure_name);
#ifdef M_UNIX
    prp = getprpwnam(pure_name);
#endif
    }
#ifdef M_UNIX
    if (pw == NIL || prp == NIL)
#else
	if (pw == NIL)
#endif
	    return set_gerror(GASP_CANT_FIND_USER,"user_not_in_passwd");

if((len==0)||(len>=MAX_PASSWORD))
    return set_gerror(GASP_CANT_FIND_USER,"bogus_passwd_password");

    bcopy(password,cur->password,len);
    bzero(&cur->password[len],MAX_PASSWORD-len);
    cur->len=len;
    cur->type=type;
#ifdef M_UNIX
    cur->uid=prp->ufld.fd_uid;
#else
    cur->uid=pw->pw_uid;
#endif
    {
      int slen=strlen(pw->pw_dir);
      if((slen<=0)||(slen>=MAXPATHLEN))
	return set_gerror(GASP_CANT_FIND_USER,"home_path_syntax_error");
      strcpy(cur->home,pw->pw_dir);
    }
    cur->want_auth=TRUE;
    cur->bad_password_rc=0;
    cur->timeout=now+AUTH_CACHE_TIMEOUT;

    /* perhaps there is a password to check*/
    /* Check his password */
    if (!ViceAuth) {
#ifdef M_UNIX
	  if (!StrEql(bigcrypt(cur->password, prp->ufld.fd_encrypt), prp->ufld.fd_encrypt))
#else
	      if (!StrEql(crypt(cur->password, pw->pw_passwd), pw->pw_passwd))
#endif
	{
	    debug(128, ("failed: bad password]\n"));
	    cur->bad_password_rc=set_gerror(GASP_NO_AUTHORIZATION,"bad_local_pass");
	}
	cur->valid=TRUE;    /*password is known good or bad*/
    }

    found_entry:
      *result=cur;
    if((cur->bad_password_rc)!=0)
	set_gerror(cur->bad_password_rc,"cache_bad_password");
    return cur->bad_password_rc;
}

char *guard_errs[]={
    "noerr",
    "unknown_request",
    "unknown_service",
    "make_server",
    "too_many_clients",
    "auth_failed",
    "too_many_servers",
    "no_addr",
    "no_pipe",
    "no_fork",
    "no_port",
    "cliname_big",
    "no_remote",
    "no_unauth",
    "no_setuid",
    "vice_crocked",
    "bogus_auth",
    "no_rpc_init",
    "no_rpc",
    "not_in_permits",
    "not_in_passwd",
    "conn_mismatch",
    "no_guests",
    "whats_vice",
    "no_cell",
};

char *guard_error_to_txt(anerr)
int anerr;
{
    static char buf[30];
    if((anerr<=0)||
	(anerr>=sizeof(guard_errs))) {
	if(anerr==0)return "gnone";
	sprintf(buf,"gerr=%d",anerr);
	return buf;
    }
    else
	return guard_errs[anerr];
}

/*
  convert an errno to text
  */
char *errno_to_text(anerr)
int anerr;
{
    static char buf[30];
    if((anerr<=0)||
	(anerr>sys_nerr)) {
	if(anerr==0)return "no_sys_error";
	sprintf(buf,"serr%d",anerr);
	return buf;
    }
    else
	return sys_errlist[anerr];
}

static int fuseki(name, password, len, type, servername,key,to,from,errbuf,versinfo,servfd)
char *name, *password;
int len, type;
char *servername;
KEY key;
ADDRESS *to, *from;
char *errbuf;
char *versinfo;
int servfd;
{
    int rc;
    USERINFO_pt u;
    SERVICE *service;
    *errbuf=0;
    SNAP_serv_port=servfd;
    clear_client_start_error();
    debug(16, ("[fuseki(%s, ?, %d, %d, %s, 0x%x, 0x%x, <%d,%d,%s>)\n",
		name, len, type, servername, key, to,
		from->sin_family, ntohs(from->sin_port),
		inet_ntoa(from->sin_addr.s_addr)));
    GCOUNT(STN_log_connection);
    /* See if known service */
    service = FindService(servername);
    if (service == NIL)
	rc=set_gerror(GASP_UNKNOWN_SERVICE,"no_such_service");
    else
	rc=lookup_auth_cache(&u,service,name,password,len,type);
    if(rc==0)
	rc=Connect(u,service, key, to, from);

    /* in case startup error isn't set*/
    if(rc!=0)
	set_gerror(rc,"unknown");
    if(sce.error!=0) {
	DEALOG(("conn,%d,%d,%d,%d,%d,%d,%d,%d,%c,%s,%s,%s,%s,%s\n",
		CONN_NUM,
		sce.start,
		type,
		sce.pid,
		(ntohs(from->sin_port)),
		sce.num_errors,
		sce.errno,
		sce.error,
		bool_to_char(sce.is_duplicate),
		deal_fixfield(name),
		deal_fixfield(servername),
		deal_fixfield(inet_ntoa(from->sin_addr.s_addr)),
		deal_fixfield(sce.where),
		deal_fixfield(versinfo)))
	  if(sce.errno==0)
	    sprintf(errbuf,"Guardian:Login failed:%s",sce.where);
	else
	    sprintf(
		    errbuf,
		    "Guardian:Login failed:%s:%s",
		    sce.where,
		    errno_to_text(sce.errno));
    }
    else
	DEALOG(("cony,%d,%d,%d,%d,%d,%c,%s,%s,%s,%s\n",
		CONN_NUM,
		sce.start,
		type,
		sce.pid,
		(ntohs(from->sin_port)),
		bool_to_char(sce.is_duplicate),
		deal_fixfield(name),
		deal_fixfield(servername),
		deal_fixfield(inet_ntoa(from->sin_addr.s_addr)),
		deal_fixfield(versinfo)));
    if(Verbose) {
	if(sce.error!=0)
	    errprintf(
		      GuardianName,ERR_WARNING,NIL,NIL,
		      "%sconnfail:%s,%s,%s,%s,%s,%s,pid%d host:%s-%d vers:%s",
		      (sce.is_duplicate?"re":""),
		      name,
		      servername,
		      (sce.is_duplicate?"dup":"new"),
		      guard_error_to_txt(sce.error),
		      errno_to_text(sce.errno),
		      sce.where,
		      sce.pid,
		      inet_ntoa(from->sin_addr.s_addr),
		      (ntohs(from->sin_port)),
		      versinfo);
	else
	    errprintf(
		      GuardianName,ERR_WARNING,NIL,NIL,
		      "%sconn:%s,%s,pid%d host:%s-%d vers:%s",
		      (sce.is_duplicate?"re":""),
		      name,
		      servername,
		      sce.pid,
		      inet_ntoa(from->sin_addr.s_addr),
		      (ntohs(from->sin_port)),
		      versinfo);
    }
    return sce.error;
}

static bool SameHost(from)
ADDRESS *from;
{
    int result;

    debug(8192, ("[SameHost(<%d,%d,%s>)\n",
		  from->sin_family,
		  ntohs(from->sin_port),
		  inet_ntoa(from->sin_addr.s_addr)));

    /* Compare host addresses */
    if (!GetMyAddress()) return FALSE;
    result = (bcmp(MyAddress, &from->sin_addr.s_addr, 4) == 0);
    debug(8192, ("result: %d]\n", result));
    return result;
}

static int  Connect(u,service, key, to, from)
USERINFO_pt u;
SERVICE *service;
KEY key;
ADDRESS *to, *from;
{
    bool remote;
    SERVER *server;
    int code;

    /* See if remote request & we're accepting them */
    remote = !SameHost(from);
    if (!u->valid && remote && (!AcceptRemoteRequests)) {
	debug(16, ("Can't accept remote requests\n"));
	return set_gerror(GASP_NO_REMOTE_REQUESTS,"no_remote_logins");
    }

    /* See if existing server for this <service, name> tuple */
    server = FindServerByName(service, u);

    /*
      Check for special case -- found old server but new request
	  is guest id & old isn't (or vice versa).
		  */
    if (server != NIL && server->guestid != (!u->want_auth))
	return set_gerror(GASP_MISMATCH,"guest_vs_user");

    /* See if need to create a server */
    if (server == NIL) {
	GCOUNT(STN_connect_new);
	code=authenticate_if_requested(u,from,remote);
	if(code!=0)return code;

	/* Must create new server */
	if (OkToCreateNewServer(service,u)) {
	    server = NewServer(service,u, &code, key);
	    if (server == NIL) return code;
	} else
	    /* GASP_TOO_MANY_SERVERS;  GASP_CANT_MAKE_SERVER; */
	    /*Silly pc, this is the least bad sounding error from login*/
	    return set_gerror(GASP_NO_REMOTE_REQUESTS,"server_full");
    } else {
	sce.pid=server->pid;
	sce.is_duplicate=TRUE;
	if(!u->valid) {
	    code=authenticate_if_requested(u,from,remote);
	    if(code!=0)return code;
	}
	GCOUNT(STN_connect_old);
    }

    bcopy(server->key, key, SNAP_KEYLEN);

    /* See if server can accept another client */
    if (!CanAcceptNewClient(server)) {
printf("***TOO MANY CLIENTS\n"); fflush(stdout);
	return set_gerror(GASP_TOO_MANY_CLIENTS,"too_many_clients");
}

    /* Update server's client info */
    NewClient(server, from);

    /* Set address for client */
    *to = server->address;
    debug(16, ("\tConnect, address is <%d, %d,%s>\n",
		to->sin_family,
		ntohs(to->sin_port),
		inet_ntoa(to->sin_addr.s_addr)));

    return 0;
}

/*
  static NotCompiled(what)
  char *what;
  {
  errprintf(GuardianName, ERR_WARNING, NIL, NIL,
	     "Guardian not compiled for %s", what);
  exit(EX_SOFTWARE);
  }
  */
/*
  Construct an encryption key & store it in key.
  Use information in u, unless it is NIL.  In
  that case, there is nothing useful there, do
      without it.
	*/

static MakeKey(key, u)
KEY key;
USERINFO *u;
{
    extern int rand();
    register int i;
    register unsigned char *k;

    /* Fill with random #s */
    k = key;
    for (i=sizeof(long); i<=SNAP_KEYLEN; i+=sizeof(long)) {
	long n;
	n = rand();
	bcopy(&n, k, sizeof(long));
	k += sizeof(long);
    }

    /* Catch any bytes at end */
    if (k < key+SNAP_KEYLEN) {
	/* There are bytes left */
	long n;
	n = rand();
	bcopy(&n, k, key+SNAP_KEYLEN-k);
    }

#ifdef DEBUG
    if (DebugFlags & 128) {
	printf("[Key: 0x");
	for (i=0; i<SNAP_KEYLEN; i++) printf("%02x", (unsigned char) key[i]);
	puts("]");
    }
#endif /* DEBUG */
}

static void count(request)
register REQUEST *request;
{
    register SERVER *server;

    debug(512, ("[Count\n"));
    /* Find out who is sending */
    server = FindServerByAddress(&request->from);
    if (server == NIL) {
	debug(512, ("\tCan't find server by address: <%d, %d,%s>]\n",
		    request->from.sin_family,
		    ntohs(request->from.sin_port),
		    inet_ntoa(request->from.sin_addr.s_addr)));
	return;
    }

    debug(512, ("\tFound server, old count = %d", server->count));
    server -> count = request -> count;
    debug(512, (", new count = %d]\n", server->count));

    /* Here's the place to take action dependent on new count */
}

char **set_child_env(u)
USERINFO *u;
{
  static char *newenv[30];
  static char env0[MAXPATHLEN];
  static char env1[MAXPATHLEN];
  static char env2[MAXPATHLEN];
  char *adir, *ldir;

  newenv[0] = env0;
  newenv[1] = env1;
  newenv[2] = env2;
  newenv[3] = 0;

  adir = (char *)AndrewDir(NULL);
  if (!adir) adir = "/usr/andrew";
  ldir = (char *)LocalDir(NULL);
  if (!ldir) ldir = "/usr/local";
  sprintf(newenv[0], "HOME=%s",u->home);
  sprintf(newenv[1],
	"PATH=%s/bin:%s/bin:%s/bin:/usr/local/bin:/usr/ucb:/usr/bin:/bin", 
	u->home, adir, ldir);
  sprintf(newenv[2], "ANDREWDIR=%s", adir);

  return newenv;
}

static ServerHead(service,u,
		   fds, sfd)
SERVICE *service;
USERINFO *u;
int fds[2], sfd;
{
    char *argv[6], keytemplate[10], clienttemplate[10];
    int i;

    /* close everything but stdin stderr and stdout*/
    for(i=getdtablesize()-1;i>2;i--)
	if((i!=sfd)&&(i!=fds[RSIDE]))
	    close(i);

    /* Exec service program */
    argv[0] = service->name;
    argv[1] = u->name;
    /* Send file descriptors as args 2 & 3 */
    sprintf(keytemplate, "%d", fds[RSIDE]);
    argv[2] = keytemplate;
    sprintf(clienttemplate, "%d", sfd);
    argv[3] = clienttemplate;
    if (u->want_auth) {
	argv[4] = "-a";         /* Authenticated flag */
	argv[5] = NIL;
    } else
	argv[4] = NIL;

#ifdef DEBUG
    if ((DebugFlags & 128) != 0) {
	register int i;
	printf("[Serverhead, exec string: %s", service->exec);
	for (i=0; argv[i]!=NIL; i++)
	    printf(" %s", argv[i]);
	puts("]");
	fflush(stdout);
    }
#endif /* DEBUG */

    /* Become the client */
    if (u->want_auth)
	child_sys(setuid(u->uid),"setuid_become_user");
    
    child_sys(execve(service->exec, argv,set_child_env(u)),"execv_cant_start");
}

static int SetAddress(portn)
port_name_pt portn;
{
    struct servent *sv;
    struct sockaddr_in sin;
    register int fd;	/* Socket fd */

    debug(2, ("[SetAddress: %s, %s, %d\n", portn->name, portn->sproto, portn->iproto));
    sv = getservbyname(portn->name, portn->sproto);
    if (sv == NIL) {
	debug(2, ("Can't find name %s\n",portn->name));
	return -1;
    }

    fd = socket(AF_INET, portn->iproto, 0);
    if (fd < 0) {
	debug(2, ("\tSocket failed]\n"));
	return -1;
    } else
	debug(2, ("\tSocket succeeded: %d\n", fd));

    bzero(&sin, sizeof sin);
    sin.sin_family = AF_INET;
    sin.sin_port = sv -> s_port;
    sin.sin_addr.s_addr = INADDR_ANY;
    if(portn->iproto==SOCK_STREAM)
      set_socket_reusable(fd);
    if (bind(fd, &sin, sizeof sin) < 0)
	ReportSystemError(ERR_CRITICAL, NIL,
			  "Can't bind to address: ", errno);
    if(portn->iproto==SOCK_STREAM) {
      if (listen(fd, 3) < 0)
	ReportSystemError(ERR_CRITICAL, NIL, "Debug LISTEN failed: %s", errno);
    }

    debug(2, ("\tBind succeeded]\n"));
    return fd;
}

static CheckServers()
{
#ifdef POSIX_ENV
    stat_stuff st;
    int status;

    debug(8, ("[CheckServers\n"));

    do {
	st.pid = waitpid(-1, &status, WNOHANG);
	if (st.pid > 0) {
	    /*unpack machine dependant fields*/
	    st.s_termsig=WTERMSIG(status);
	    st.s_retcode=WEXITSTATUS(status);
	    st.s_stopval=WIFSTOPPED(status);
	    st.s_stopsig=WSTOPSIG(status);

	    /* Got a child, check its status */
	    debug(8, ("\tChecking status of pid %d...",st.pid));

	    if (WIFEXITED(status) || WIFSIGNALED(status)) {
		debug(8, ("it died\n"));
		ServerDied(&st);
	    } else
		debug(8, ("it didn't die"));
	}
    } while (st.pid > 0);
#else
    stat_stuff st;
    union wait status;

    debug(8, ("[CheckServers\n"));

    do {
	st.pid = wait3(&status, WNOHANG, &st.usage);
	if (st.pid > 0) {
	    /*unpack machine dependant fields*/
	    st.s_termsig=status.w_termsig;
	    st.s_coredump=status.w_coredump;
	    st.s_retcode=status.w_retcode;
	    st.s_stopval=status.w_stopval;
	    st.s_stopsig=status.w_stopsig;

	    /* Got a child, check its status */
	    debug(8, ("\tChecking status of pid %d...",st.pid));
	    if (st.s_stopval!=WSTOPPED) {
		debug(8, ("it died\n"));
		ServerDied(&st);
	    } else
		debug(8, ("it didn't die"));
	}
    } while (st.pid > 0);
#if defined(M_UNIX)
    signal(SIGCHLD, ChildAction);
#endif
#endif /* POSIX */
    debug(8, ("]\n"));
}

/*******************************\
*				*
*  Interactive debugging code	*
*				*
\*******************************/

static FILE *dout;

static int saveout, saveerr;

static bool WaitingForPassword; /* FALSE means we got & accepted the password */

#define DEBUGSIZE	200

static char DebugRequest[DEBUGSIZE];
static int DebugPos;	/* Index of next character in debug command */

static void DebugSocket()
{
    /* See if 1st time in for this connection */

    if (savedebug < 0) {
	struct sockaddr_in sin;
	int len, newfd;

	len = sizeof sin;
	newfd = accept(debugfd, &sin, &len);
	if (newfd < 0) {
	    safeperror("[SNAP.GUARDIAN] ACCEPT failed: %s");
	    return;
	}
	dout = fdopen(newfd, "w");
	if (dout == NIL) {
	    safeperror("[SNAP.GUARDIAN] FDOPEN failed: %s");
	    close(newfd);
	    return;
	}

	/* Redirect stdout & stderr to telnet socket */
	fflush(stdout);
	fflush(stderr);
	saveout = dup(1);	/* stdout */
	saveerr = dup(2);	/* stderr */
	if (dup2(newfd, 1) < 0) {
	    safeperror("[SNAP.GUARDIAN] DUP2 of stdout failed: %s");
	    close(newfd);
	    return;
	}
	if (dup2(newfd, 2) < 0) {
	    safeperror("[SNAP.GUARDIAN] DUP2 of stderr failed: %s");
	    close(newfd);
	    return;
	}
	/* Don't check results of following setlinebufs, they may fail */
	setlinebuf(stdout);
	setlinebuf(stderr);

	/* Change things so select done on new descriptor */
	savedebug = debugfd;
	debugfd = newfd;

	DebugPos = 0;
	WaitingForPassword = TRUE;
	{
	    static char willecho[] = { IAC, WILL, TELOPT_ECHO, '\0' };
	    fputs(willecho, dout);
	}
	fputs("\npassword: ", dout);
	fflush(dout);
	return;
    }

    /* Not 1st time, this must be a request from existing connection */
    ProcessDebugRequest();
}

enum DebugAction { DEBUG_EOF, DEBUG_COMMAND, DEBUG_CONT };

static enum DebugAction GetDebugRequest()
{
    register int len;
    char c;

    len = read(debugfd, &c, 1);
    if (len < 0) {
	safeperror("[SNAP.GUARDIAN] Read error in debug: %s");
	return DEBUG_EOF;
    }
    if (len == 0) return DEBUG_EOF;

    /* Got a character, see if room in buffer */
    if (DebugPos >= DEBUGSIZE) {
	fprintf(dout, "[SNAP.GUARDIAN] Debug line too long: %d\n", DEBUGSIZE);
	DebugRequest[0] = '\0';
	DebugPos = 0;
	return DEBUG_COMMAND;	/* Null command */
    }

    switch (c) {
	    case '\r':  /* Carriage return during password */
			fputs("\r\n", dout);
			/* Fall through */
	    case '\n':  DebugRequest[DebugPos] = '\0';
			DebugPos = 0;
			return DEBUG_COMMAND;
	    case 022:	if (WaitingForPassword) return DEBUG_CONT;
			DebugRequest[DebugPos] = '\0';  /* ^R */
			fputc('\n', dout);
			fputs(DebugRequest, dout);
			fflush(dout);
			return DEBUG_CONT;
	    case 025:	DebugRequest[0] = '\0';         /* ^U */
			if (WaitingForPassword) fputs("^U\r", dout);
			fputc('\n', dout);
			fflush(dout);
			DebugPos = 0;
			return DEBUG_COMMAND;	/* Null command */
	    case 010:					/* ^H */
	    case 0177:	DebugPos--;			/* DEL */
			return DEBUG_CONT;
	    case IAC:	/* Throw away telnet response */
			{
			    char answer[2];
			    len = read(debugfd, answer, 2);
			    if (len < 0) {
			    safeperror("[SNAP.GUARDIAN] Error in telnet answer: %s");
				return DEBUG_EOF;
			    }
			    return DEBUG_CONT;
			}
	    default:	DebugRequest[DebugPos++] = c;
			return DEBUG_CONT;
    }
}

static void ProcessDebugRequest()
{
    switch (GetDebugRequest()) {

	case DEBUG_EOF:
	    /* Restore stdout & stderr */
	    if (dup2(saveout, 1) < 0)
		safeperror("[SNAP.GUARDIAN] Can't restore stdout: %s");
	    else
		close(saveout);
	    if (dup2(saveerr, 2) < 0)
		safeperror("[SNAP.GUARDIAN] Can't restore stderr: %s");
	    else
		close(saveerr);
	    fclose(dout);
	    debugfd = savedebug;
	    savedebug = -1;

	case DEBUG_CONT:
	    return;

	case DEBUG_COMMAND:
	    /* Got a request, now execute it */
	    if (WaitingForPassword)
		if (DebugRequest[0] != '\0') { /* Only if not null */
		    CheckPassword(DebugRequest);
		    if (!WaitingForPassword) {
			static char wontecho[] =
			  { IAC, WONT, TELOPT_ECHO, '\0' };
			  fputs(wontecho, dout);
		    }
		} else
		    ;
	    else
		ExecuteDebugRequest(DebugRequest);
	    fputs(WaitingForPassword ? "password: " : "DEBUG> ", dout);
	    fflush(dout);
    }
}

/**********************\
*		       *
*  Debugging commands  *
*		       *
\**********************/

static char *SkipWord(line)
register char *line;
{
    for (; *line != ' ' && *line != '\0'; line++) ;
    return line;
}

static char *SkipBlanks(line)
register char *line;
{
    for (; *line == ' '; line++) ;
    return line;
}

#define SkipCommand(line)	(SkipBlanks(SkipWord(line)))

static void PrintServer(s)
register SERVER *s;
{
    fputs(s->service->name, dout);
    if (strlen(s->service->name) < 8) fputc('\t', dout);
    fprintf(dout, "\t%d\t%d\t%d\t%s\t",
	     s->authenticated, s->count, s->pid, s->client.name);
    if (strlen(s->client.name) < 8) fputc('\t', dout);
    fprintf(dout, "<%u,%u,%s>\n",
	     s->address.sin_family,
	     ntohs(s->address.sin_port),
	     inet_ntoa(s->address.sin_addr.s_addr));
}

static void ServerCommand(command)
register char *command;
{
    register int i;
    register char *name, *client;
    register bool first, any;

    /* Quick check */
    if (NextServerSlot == 0) {
	fputs("There are no active servers\n", dout);
	return;
    }

    command = SkipCommand(command);

    /*
      There are 3 cases:
      (1) No arguments: step through	all servers
      (2) 1 argument: print servers with that service name
      (3) 2 arguments: print servers with that service &
      client name
      */

    name = NIL;
    client = NIL;
    if (*command != '\0') {
	name = command; 	/* Will be service name */
	command = SkipWord(command);
	if (*command != '\0') {
	    *command++ = '\0';  /* Terminate name field */
	    command = SkipBlanks(command);
	    if (*command != '\0') {
		client = command;
		command = SkipWord(command);
		*command = '\0';
	    }
	}
    }

    /* Now service & client names have been set */
    first = TRUE;
    any = FALSE;
    for (i=0; i<NextServerSlot; i++)
	if ((name == NIL || StrEql(servers[i].service->name, name)) &&
	    (client == NIL || StrEql(servers[i].client.name, client))) {

	    any = TRUE;
	    if (first) {
		fputs("Service\t\tAuth\tCnt\tPid\tClient\t\tAddr\n", dout);
		fputs("==============================================", dout);
		fputs("==============\n", dout);
		first = FALSE;
	    }
	    PrintServer(&servers[i]);
	}

    if (!any) fputs("No services found\n", dout);
}

static char execbuffer[1025];

/*
  ex service-name path
  */

static void ExecCommand(command)
register char *command;
{
    register char *name, *exec;
    register SERVICE *service;

    command = SkipCommand(command);

    /* Extract args if there */
    name = NIL;
    exec = NIL;
    if (*command != '\0') {
	name = command; 	/* Point to service name */
	command = SkipWord(command);
	if (*command != '\0') {
	    *command++ = '\0';  /* Terminate name */
	    command = SkipBlanks(command);
	    if (*command != '\0') {
		exec = command;
		command = SkipWord(command);
		*command = '\0';
	    }
	}
    }

    /* See if args where there */
    if (name == NIL || exec == NIL) {
	fputs("Command is\n\tex service-name path\n", dout);
	return;
    }

    /* Try to find service */
    service = FindService(name);

    /* Was it there? */
    if (service == NIL) {
	fprintf(dout, "Can't find service \"%s\"\n", name);
	return;
    }

    /* Modify exec field */
    fprintf(dout, "Old exec field: \"%s\"\n", service->exec);
    strncpy(execbuffer, exec, sizeof(execbuffer));
    execbuffer[sizeof(execbuffer)-1] = '\0';
    service -> exec = execbuffer;
    fprintf(dout, "New exec field: \"%s\"\n", service->exec);
}

static void KillCommand(command)
register char *command;
{
    register int pid, i;
    register char *start;
    char answer;

    command = SkipCommand(command);

    /* Extract pid */
    if (*command != '\0') {
	start = command;
	command = SkipWord(command);
	*command = '\0';
	pid = atoi(start);
    } else
	pid = -1;

    /* Check the pid */
    if (pid < 0) {
	fputs("Bad/missing <pid>\n", dout);
	return;
    }
    for (i=0; i<NextServerSlot; i++)
	if (servers[i].pid == pid) break;

    if (i >= NextServerSlot)
	/* Not a server, make sure it's okay */
	if(pid==0) {
	    pid=getpid();
	    fprintf(dout, "Kill guardian");
	} else
	    fprintf(dout, "Process %d is not a server", pid);
    else
	/* It's a server, give information */
	fprintf(dout, "Process %d is server \"%s\" for user \"%s\"",
		pid, servers[i].service->name, servers[i].client.name);

    fputs(", are you sure [n] ? ", dout);
    fflush(dout);
    read(debugfd, &answer, 1);
    if (answer != '\n') fputc('\n', dout);
    fflush(dout);
    if (answer != 'Y' && answer != 'y') {
	fputs("ABORTED\n", dout);
	fflush(dout);
	return;
    }

    /* Kill the process */
    kill(pid, SIGKILL);
#ifdef LET_WAIT_GET_IT
    if (i < NextServerSlot) ServerDied(pid);
#endif /* LET_WAIT_GET_IT */
}

static int assignhex(loc, value)
long *loc;
char *value;
{
    sscanf(value, "%x", loc);
    return(0);
}

static void printhex(f, value)
FILE *f;
long value;
{
    fprintf(f, "0x%x", value);
}

static int assignbool(loc, value)
long *loc;
char *value;
{
    switch (*value) {
	case 't':
	case 'T':
	case '1':   *loc = TRUE;
	    break;
	case 'f':
	case 'F':
	case '0':   *loc = FALSE;
	    break;
	default:    fputs("Bad boolean value\n", dout);
    }
    return(0);
}

static int SetLog(value)
char *value;
{
    if(deal_open("guardian",value))
	printf("open of log file failed");
    return 0;
}

static void printlog(f, value)
FILE *f;
long value;
{
    fprintf(f,"%s",deal_get_log_name());
}

static int assignlog(loc, value)
long *loc;
char *value;
{
    return SetLog(value);
}

static int SetLimits(value)
char *value;
{
    long new_hard,new_try;
    if(sscanf(value,"%ld,%ld",&new_hard,&new_try)!=2)
	printf("can't parse login limit, e.g -1,-1 -> off, 5,2 ->hard 5 try 2");
    LimitMaxUsers=new_hard;
    TryLimitMaxUsers=new_try;
    return 0;
}

static int assignlimit(loc, value)
long *loc;
char *value;
{
    return SetLimits(value);
}

static void printlimit(f, value)
FILE *f;
long value;
{
    fprintf(f,"%d,%d",LimitMaxUsers,TryLimitMaxUsers);
}

static void printbool(f, value)
FILE *f;
long value;
{
    fputs(value ? "T" : "F", f);
}

static void printtime(f, value)
FILE *f;
long value;
{
    long now;
    register struct tm *tm;

    now = time(0);
    tm = localtime(&now);
    fprintf(dout, "%02d:%02d", tm->tm_hour, tm->tm_min, tm->tm_sec);
}

static void printversion(f, value)
FILE *f;
long value;
{
    fputs(GuardianVersion, f);
}

static int assignreboot(loc, value)
long *loc;
char *value;
{
    register char *answer;

    answer = ParseReboot(value);
    if (answer != NIL) fprintf(dout, "%s\n", answer);
    return(0);
}

static void printreboot(f, value)
FILE *f;
long value;
{
    if (RebootFlag)
	fputs("NOW", f);
    else
	if (RebootTime == 0)
	    fputs("OFF", f);
	else {
	    register struct tm *tm;
	    tm = localtime(&RebootTime);
	    fprintf(f, "%02d:%02d", tm->tm_hour, tm->tm_min);
	}
}

static void VarCommand(command)
char *command;
{
    register int i;
    register char *name;

    command = SkipCommand(command);
    if (*command == '\0') {

	/* No args, show all variables */
	fputs("\nname\tvalue\tdescription\n", dout);
	fputs("----\t-----\t-----------\n", dout);
	for (i=0; i<NVARS; i++) {
	    fprintf(dout, "%s\t", variables[i].name);
	    (*variables[i].outaction)(dout, variables[i].value);
	    fprintf(dout, "\t%s\n", variables[i].description);
	}
	fputc('\n', dout);
	return;
    }

    /* Have a specific variable */
    name = command;
    command = SkipWord(command);
    if (*command != '\0') *command++ = '\0';
    for (i=0; i<NVARS; i++)
	if (StrEql(variables[i].name, name)) {
	    register char *value;

	    command = SkipBlanks(command);
	    if (*command == '\0') {
		/* No new value, just show current value */
		fprintf(dout, "%s = ", name);
		(*variables[i].outaction)(dout, variables[i].value);
		fputc('\n', dout);
		return;
	    }

	    /* Assign a new value */
	    if (variables[i].inaction == NIL) {
		fputs("Read-only variable\n", dout);
		return;
	    }
	    value = command;
	    command = SkipWord(command);
	    *command = '\0';
	    (*variables[i].inaction)(&variables[i].value, value);
	    return;
	}

    fprintf(dout, "Variable \"%s\" not found\n", name);
    return;
}

static void ServiceCommand(command)
register char *command;
{
    register char *name;
    register bool first, any;
    register int i;

    command = SkipCommand(command);

    /* Extract arg if there */
    name = NIL;
    if (*command != '\0') {
	name = command; 	/* Point to service name */
	command = SkipWord(command);
	*command = '\0';        /* Terminate name */
    }

    first = TRUE;
    any = FALSE;
    for (i=0; i<maxservices; i++) {
	if (name == NIL || StrEql(services[i].name, name)) {
	    char *type;
	    register SERVICE *s;

	    any = TRUE;
	    if (first) {
		/* Print header */
		fputs("Name\t\tType\tSuid\tMax\tPre\tExec\n", dout);
		fputs("====================================================\n", dout);
		first = FALSE;
	    }
	    s = &services[i];
	    switch (s->type) {
		case AUTH_ONLY:
		    type = "A"; break;
		case UNAUTH_ONLY:
		    type = "U"; break;
		case BOTH:
		    type = "B"; break;
		default:
		    type = "?"; break;
	    }
	    fputs(s->name, dout);
	    if (strlen(s->name) < 8) fputc('\t', dout);
	    fprintf(dout, "\t%s\t%d\t%d\t%d\t\"%s\"\n",
		    type, s->setuid, s->maxclients, s->predefined, s->exec);
	}
    }
    if (!any) fputs("No services found\n", dout);
    return;
}

static struct {
    char	*name;
    void 	(*proc)();
    char	*help;
} commands[] = {

    { "?",          HelpCommand,
    "?                      Print this text" },

    { "=",          VarCommand,
    "= [[<var>] <value>]    Set variable/show vars" },

    { "ex",         ExecCommand,
    "ex <service> <path>    Change exec file for service" },

    { "kl",         KillCommand,
    "kl <pid>               Kill process" },

    { "sc",         ServiceCommand,
    "sc [<service>]         Show selected services" },

    { "sr",         ServerCommand,
    "sr [<service> [<usr>]] Show active servers" },
};

#define NCOMMANDS	(sizeof commands / sizeof commands[0])

static void HelpCommand(request)
char *request;
{
    register int i;

    for (i=0; i<NCOMMANDS; i++) {
	fputs(commands[i].help, dout);
	fputc('\n', dout);
    }

    fputc('\n', dout);
    fputs("^U           Delete line\n", dout);
    fputs("^R           Show current line\n", dout);
    fputs("<del>, ^H    Delete last character\n", dout);
}

static void CheckPassword(password)
char *password;
{
    static char wizard[] = "root";
    register struct passwd *pw;

    /* Find password entry */
#if defined(M_UNIX) || defined(sys_telmat)
    pw = getpwnam(wizard);
#else
    pw = getpwnam(wizard, dout, debugfd);
#endif
    if (pw == NIL) {
	ReportError(ERR_WARNING, NIL, "Can't find passwd entry: %s", wizard);
	return;
    }

    /* Check the password */
    if (!StrEql(crypt(password, pw->pw_passwd), pw->pw_passwd)) {
	fputs("Sorry\n\r", dout);
	return;
    }

    WaitingForPassword = FALSE;
    return;
}

static void ExecuteDebugRequest(request)
register char *request;
{
    register int i;

    /* Ignore blank lines */
    if (request[0] == '\0') return;

    for (i=0; i<NCOMMANDS; i++)
	if (strncmp(request,
		    commands[i].name,
		    strlen(commands[i].name)) == 0) {
	    /* Recognized a command */
	    (*commands[i].proc)(request);
	    return;
	}

    /* Unknown command */
    fprintf(dout, "Unrecognized command: %s\n", request);
}

/* Send out a statistics packet in response to a request. */

static void SendStats()
{
    int len;
    ADDRESS from;
    char dummy[4];
    stat_packet_t stats;

    /* Read request to get address */
    len = sizeof from;
    if (recvfrom(statfd, dummy, sizeof dummy, 0, &from, &len) < 0)
	ReportSystemError(ERR_WARNING, NIL,
			  "RECVFROM from stats failed: %s", errno);

    build_stat_packet(&stats);
    if (sendto(statfd, &stats, sizeof stats, 0, &from, sizeof from) < 0)
	ReportSystemError(ERR_WARNING, NIL,
			  "SENDTO to stats failed: %s", errno);
}

bool want_more_users()
{
    return (AcceptRemoteRequests &&
	     ((TryLimitMaxUsers == -1) ||
	      (NextServerSlot<TryLimitMaxUsers)));
}

static build_stat_packet(packet)
register stat_packet_t *packet;
{
    struct osi_Times Tm;

    packet->gstat_version = htonl((u_long) GSTAT_VER_1);
    if (osi_GetTimes(&Tm) == 0) {
	packet->timestamp = htonl((u_long) Tm.Secs);
    }
    packet->time_up = htonl(start_time);
    bcopy(MyAddress, &packet->ipAddress, 4);
    packet->ipAddress = htonl(packet->ipAddress);
    packet->servers = htonl((u_long) NextServerSlot);
    packet->max_servers = htonl((u_long) MaxServers);
    packet->users = htonl((u_long) Users);
    packet->max_users = htonl((u_long) Maxusers);
    packet->load_avg[0] = htonl((u_long) (getla(0)*10.0));
    packet->load_avg[1] = htonl((u_long) (getla(1)*10.0));
    packet->load_avg[2] = htonl((u_long) (getla(2)*10.0));

    /* Guardian state */
    strncpy(packet->g_version, GuardianVersion+GUARDIAN_PREIX_SKIP, sizeof(packet->g_version)-1);
    packet->vice_auth = (u_char) ViceAuth;
    packet->accept_remote_req = (u_char) want_more_users();
    packet->permits_file = (u_char) (Permits!=NIL);
    packet->run_as_user = (u_char) RunAsUser;
    packet->verbose = (u_char) Verbose;
    packet->debug_flags = htonl((u_long) DebugFlags);
    packet->reboot_time = htonl((u_long) RebootTime);
}

static bool NewUser(name)
register char *name;
{
    register int i;

    for (i=0; i<NextServerSlot; i++)
	if (StrEql(name, servers[i].client.name))
	    return FALSE;

    return TRUE;
}
#ifdef RAUTH

#ifdef AFS_ENV
static char *GetCellName()
{
    static bool virgin = TRUE;
    static char thiscell[MAXCELLCHARS+1];

    if (virgin) {
	if (GetCurrentWSCell(thiscell, MAXCELLCHARS) != 0)
	    return NIL;
	virgin = FALSE;
    }
    return thiscell;
}

#if 0
static bool homeCell(name)
register char *name;
{
    register char *thiscell;
    thiscell = GetCellName();
    if (thiscell == NIL) return FALSE;	/* Looks just like there was no error */
    /* Now do string comparison */
    return IgnoreCaseEql(name, thiscell);
}

static int InitializeRPC()
{
    extern long r_nPackets;
    static bool virgin = TRUE;
    register int rc;

    if (virgin) {
	r_nPackets = 5;
	rc = U_InitRPC();
	if (rc != 0)
	    ReportAuthError(ERR_CRITICAL, NIL, "RPC initialization failed: %s", rc);
	else
	    virgin = FALSE;
	return rc;
    }
    return 0;
}
#endif /* Remove this when you're sure it's history.*/
#endif /* AFS_ENV */

#endif /* RAUTH */

