
#ifdef _IBMR2
#ifndef _BSD
#define _BSD 43
#endif
#endif

#define SEC (1L)
#define MINS (60L*(SEC))
#define HOUR (60L*(MINS))
#define DAY (24L*HOUR)

/*
  ***********************************************************
  Configuration constants.  Feel free to asjust as needed.
  ***********************************************************
  */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/nametime/RCS/nmtimeg.c,v 2.37 1993/05/04 01:49:29 susan Exp $";
#endif

/* nametimeg program versions*/
#define VER_MAJ (0)
#define VER_MIN (9)

/*time to live for domain name responses (A responses not CNAME)*/
#define TIME_TO_LIVE (HOUR)
/*ask each guardian its loadaverage*/
#define AL_DELAY_ask (18*SEC)
/*if we don't here from a guardian in this amount of time it is dead*/
#define AL_DELAY_dead (AL_DELAY_ask*2L)
/*if no answer from guardian ask again*/
#define AL_DELAY_retry (4L*SEC)
/* max number of times to use AL_DELAY_retry fast time*/
#define ASK_FAST_MAX (5)
/*retry gethostname this often*/
#define AL_DELAY_noname (10L*MINS)
/*look for changed file*/
#define AL_DELAY_reread (4L*MINS)
/*if a time for an event to happen is found to be more than
    this amount of time in the future it means the clock changed
    so recompute the delay
    */
#define AL_DELAY_max (20L*MINS)
/*never sleep for less than this amount of time*/
#define MIN_SLEEP (2L*SEC)

/* maximum number of guardians to poll*/
#define NUM_GUARDS (30)
/*maximum length of a host name*/
#define MAX_HOST_NAME (60)

/*name of the service to get guardian statistics*/
#define GUARD_STAT_NAME "snap.guardian03"
/*where to dump all internal tables in a text file*/
#define INTERNAL_DUMP_LOCATION "/tmp/nmtimeg.int"

#define MAX_INT ((-1)^(1<<((sizeof(int)*8)-1)))

#include <andrewos.h> /* sys/types.h sys/time.h sys/file.h */
#include <stdio.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <netdb.h>
#include <gasp.h>
#include <gstats.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#include <setjmp.h>
#include <arpa/nameser.h>

#define	IPNAME_NAMESERVER   "name"

#ifndef IPPORT_NAMESERVER
#define	IPPORT_NAMESERVER	42
#endif /* IPPORT_NAMESERVER */

#define NETADDR struct sockaddr_in

#define TRUE (0==0)
#define FALSE (0==1)
#define NIL (0L)
#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */
extern int sys_nerr;
extern char *sys_errlist[];
extern int errno;

#define min(xx1,xx2) (((xx1)<(xx2))?(xx1):(xx2))
#define max(xx1,xx2) (((xx1)>(xx2))?(xx1):(xx2))

#define fatal_error(xx_arg) \
{printf("nametimeg:");printf xx_arg;fflush(stdout);perror(":sys err");exit(1);}

#define debug(xx_arg) {if(gl.debugging!=0){printf xx_arg;fflush(stdout);}}

typedef void (*ACTOR_FUNC_PT)();

/*
  guardian entry, one per gaurdian
  */
struct guard_entry_R {
    int addr_valid;             /*true if addr is set*/
    NETADDR addr;               /*adrress of this guardian*/
    char hname[MAXDNAME];  /*this hosts text net name*/
    long actor_activations;     /*number of times doing same actor*/
    long actor_time;            /*next time to run actor_func*/
    ACTOR_FUNC_PT actor_func;   /*routine to handle time event*/
    long weight;                /*extra load for each time we use this one first*/
    long rcv_time;              /*last time this load av recieved or zero*/
    stat_packet_t load_dat;     /*last loadav from this host*/
};
typedef struct guard_entry_R guard_entry,*guard_entry_pt;

/*
  names for last errno for each type of failure
*/
char *er_names[]={
#define ER_recvfrom_errno 0	/*errno of last recvfrom failure*/
    "recvfrom",
#define ER_sendto_errno 1	/*errno of last sendto failure*/
    "sendto",
#define ER_hname_errno 2	/*errno of last gethostbyname failure*/
    "gethostbyname",
};
#define ER_MAX 2

/*
  names for global statistics table
*/
char *st_names[]={
#define ST_recvfrom 0	/*recvfrom failures*/
    "recvfrom fails",
#define ST_recvshort 1	/*packets too short*/
    "rcv siz short",
#define ST_recvlong 2	/*packets too long*/
    "rcv siz long",
#define ST_recvright 3  /*packets length right*/
    "rcv pak siz ok",
#define ST_recvwho 4	/*can't match packet with guard entry*/
    "rcv from martian",
#define ST_sendtofail 5 /*number of sendto failures*/
    "snd failed",
#define ST_sendok 6	/*number of packets sent*/
    "snd ok",
#define ST_hname 7	/*number gethostbyname failures*/
    "gethhostbyname fails",
};
#define ST_MAX 7

/*
  domain name lookup globals and error processing
*/
#define domain_error(xx_arg) {(*gl.nh.catch) xx_arg;}
struct  nh_t_R {
    int dsock;		  /*domain name socket*/
    void (*catch)();	  /*throw errors to here*/
    jmp_buf dthrow;	  /*error catch for domain name*/
    NETADDR from;	  /*where lookup came from*/
    int look_size;	  /*size of lookup packet*/
    char io_buf[PACKETSZ*2];
    HEADER *ih;		  /*pointer to request packet cast as a header*/
    char *dnptrs[10];	  /*previously compressed names*/
    char **lastdnptr;	  /*last name compressed*/
};
typedef struct nh_t_R nh_t;

/*
   maximum legal pointer into io_buf
*/
#define MAX_INP (&gl.nh.io_buf[PACKETSZ])

/*
  global data
  */
struct gl_R {
    nh_t nh;            /*domain name variables*/
    int testing;	/*true if pretending to be an authority*/
    int nsock;          /*public socket for name server*/
    int wana_data_dump; /*set by signal handler to dump data file*/
    int debugging;      /*non zero if printing debugging info*/
    int load_socket;	/*unix socket for asking gaurdian questions*/
    int guard_stat_port; /*ip port number guardian listens on*/
    long time_started;	/*what time this server was started*/
    long time_wake;	/*time wake up the next actor*/
    long time_watch;	/*time to check for changed files*/
    long time_now;	/*very recent result of time(0) call*/
    int free_guard;	/*index of next free entry*/
    guard_entry guard_table[NUM_GUARDS];
    int ask_buf[2];	/*request to send to guardian*/
    long st_stats[ST_MAX+1]; /*interesting records about us*/
    int errno_tab[ER_MAX+1]; /*errnos for recent errors*/
    NETADDR sendto_addr;  /*netaddr of last sendto failure*/
    int number_of_ien116_real_names;
    int number_of_ien116_pool_names;
    int number_of_domain_requests;
}gl;

/*
  sun3_34 sprintf doesn't return a result properly
*/
#define dst_printf(xx_arg) \
(((long)sprintf xx_arg),((long)(dst=dst+strlen(dst))))

/*
  Is the current guardian alive?
  Used in scheduling packet delays and considering guardians alive
  to hand out to new users.
  */
int is_guard_alive(aguard)
guard_entry_pt aguard;
{
    return(((aguard->addr_valid))&&
            (aguard->rcv_time!=0)&&
            ((gl.time_now-AL_DELAY_dead)<aguard->rcv_time));
}

/*
  Is this guardian being nice?
  */
int is_guard_refused(aguard)
guard_entry_pt aguard;
{
    return (!(aguard->load_dat.accept_remote_req));
}

/*
  timing of events in a gaurd entry is done with an
  actor like mechanisim.  Each function called from
  a periodic routine must set there replacement function
  by calling set_guard_actor.
  */
void set_guard_actor(aguard,aactor,actor_delay)
guard_entry_pt aguard;
ACTOR_FUNC_PT aactor;
long actor_delay;
{
    long actor_newtime;
    actor_newtime=gl.time_now+actor_delay;
    if(aguard->actor_func!=aactor)
      aguard->actor_activations=0L;
    aguard->actor_func=aactor;
    aguard->actor_time=actor_newtime;
    if(actor_newtime<gl.time_wake)
        gl.time_wake=actor_newtime;
}

/*
  send packet to a guardian to request a stat packet
  */
void send_req_packet(aguard)
guard_entry *aguard;
{
    if(sendto(gl.load_socket,gl.ask_buf,sizeof(gl.ask_buf),0,
               &aguard->addr,sizeof(NETADDR))<0) {
        gl.errno_tab[ER_sendto_errno]=errno; 
        bcopy(&aguard->addr,&gl.sendto_addr,sizeof(NETADDR));
        gl.st_stats[ST_sendtofail]++;
    }
    else
        gl.st_stats[ST_sendok]++;  
}

/*
  actor to send a request to guardian requesting it to return its statistics
  */
void retry_load(aguard)
guard_entry_pt aguard;
{
    long my_delay=AL_DELAY_retry;
    send_req_packet(aguard); 
    if((aguard->actor_activations>ASK_FAST_MAX)||
        (!is_guard_alive(aguard)))
        my_delay=AL_DELAY_ask;
    set_guard_actor(aguard,retry_load,my_delay);
}


/*
  actor to send a request to guardian requesting it to return its statistics
  */
void ask_load(aguard)
guard_entry *aguard;
{
    send_req_packet(aguard);
    set_guard_actor(aguard,retry_load,AL_DELAY_retry);
}


/*
  set guardians host address actor
  */
void set_guard_addr(aguard)
guard_entry_pt aguard;
{
    struct  hostent *hp;
    hp=gethostbyname(aguard->hname);
    /* if gethostbyname didn't work try again later*/
    if(hp==0) {
        gl.errno_tab[ER_hname_errno]=errno;
        gl.st_stats[ST_hname]++;
        set_guard_actor(aguard,set_guard_addr,AL_DELAY_noname);
        return;
    }
    aguard->addr.sin_family = AF_INET; 	/* Complete network address */
    aguard->addr.sin_port = gl.guard_stat_port;
    bcopy(hp->h_addr,&aguard->addr.sin_addr.s_addr,hp->h_length);
    {
        int i;
        for (i=0;i<8;i++)
            aguard->addr.sin_zero[i] = 0;
    }
    aguard->addr_valid=TRUE;
    if(strlen(hp->h_name)<MAXDNAME)
        strcpy(aguard->hname,hp->h_name);
    set_guard_actor(aguard,ask_load,0);
}

/*
  allocate and return a guard table entry
  */
guard_entry_pt aloc_guard()
{
    switch(gl.free_guard) {
        case (NUM_GUARDS-2):
            debug(("nametimeg:server table full with %d entries\n",NUM_GUARDS));
            break;
        case (NUM_GUARDS-1):
            return 0;
        default:
            break;
    }
    return &gl.guard_table[gl.free_guard++];
}

/*
  new_now - to keep from calling time() too frequently
  a current time is cached
  */
void new_now()
{
    gl.time_now=time(0);
}

/*
  return true iff time is after the passed time
  */
int not_yet(atime)
long atime;
{
    return((gl.time_now<atime)&&
            (atime<(AL_DELAY_max+gl.time_now)));
}


/*
  recieve an old style packet to a guard table entry
  */
void recv_old_aguard(aguard,astat)
guard_entry_pt aguard;
stat_packet_t *astat;
{
    long *num_user_pt=(long *)astat;
    long num_user;
    num_user = ntohl(*num_user_pt);
    bzero(&aguard->load_dat,sizeof(stat_packet_t));
    aguard->load_dat.users=num_user;
    aguard->load_dat.servers=num_user;
    aguard->load_dat.load_avg[1]=((30*num_user+15)/40);
    aguard->load_dat.accept_remote_req=1;
    aguard->rcv_time=gl.time_now;
    aguard->weight=0;
    set_guard_actor(aguard,ask_load,AL_DELAY_ask);
}

/*
  recieve a packet to a guard table entry
  */
void recv_aguard(aguard,astat)
guard_entry_pt aguard;
stat_packet_t *astat;
{
    bcopy(astat,&aguard->load_dat,sizeof(stat_packet_t));
    aguard->rcv_time=gl.time_now;
    aguard->weight=0;
    set_guard_actor(aguard,ask_load,AL_DELAY_ask);
}

/*
  we recieved a packet, find the guardian entry to match it with
  */
void recv_find_guard(from,astat,rcv_func)
NETADDR *from;
stat_packet_t *astat;
ACTOR_FUNC_PT rcv_func;
{
    register int i;
    register guard_entry_pt scan;
    for(i=0,scan=gl.guard_table;i<gl.free_guard;i++,scan++)
        if(memcmp(from,&scan->addr,sizeof(NETADDR))==0) {
            (*rcv_func)(scan,astat);
            return;
        }
    gl.st_stats[ST_recvwho]++;
}

/*
  recieve a load average packet
  */
void recieve_load()
{
    int fromlen=sizeof(NETADDR);	/*size of recieved packet*/
    int result;
    NETADDR from;
    union {
        stat_packet_t recieve_me;
        char bigbuf[MAXPATHLEN];
    }recv_buf;
    result=recvfrom(gl.load_socket,&recv_buf,sizeof(recv_buf),
                     0,&from,&fromlen);
    if(result<0) {
        gl.st_stats[ST_recvfrom]++;
        gl.errno_tab[ER_recvfrom_errno]=errno;
        return;
    }
    if(result==sizeof(recv_buf.recieve_me)) {
        gl.st_stats[ST_recvright]++;
        recv_find_guard(&from,&recv_buf.recieve_me,recv_aguard);
    }
    else if (result==sizeof(long)) {
        gl.st_stats[ST_recvright]++;
        recv_find_guard(&from,&recv_buf.recieve_me,recv_old_aguard);
    }
    else if (result > sizeof(recv_buf.recieve_me))
        gl.st_stats[ST_recvlong]++;
    else
        gl.st_stats[ST_recvshort]++;
    return;
}

/*
  create our udp socket for asking/recieving load average packets
      */
void create_load_socket()
{
    gl.load_socket=socket(AF_INET,SOCK_DGRAM,PF_UNSPEC);
    if(gl.load_socket<0)
        fatal_error(("can't open udp socket to talk to guardians"));
}

/*
  find out what port guardian is giving out statistics on
  */
void lookup_guard_stat_port()
{
    struct  servent *sp;
    sp=getservbyname(GUARD_STAT_NAME,"udp");
    if(sp==0)
        fatal_error(("can't look up server '%s'\n",GUARD_STAT_NAME));
    gl.guard_stat_port=sp->s_port;
}

/*
  run actors that are ready to run
  */
void run_actors()
{
    register int i;
    register guard_entry_pt scan;
    if(not_yet(gl.time_wake))
        return;
    /*any actor that runs will lower this time wake up time*/
    gl.time_wake=gl.time_now+AL_DELAY_max;
    for(i=0,scan=gl.guard_table;i<gl.free_guard;i++,scan++) {
        if(!not_yet(scan->actor_time)) {
            scan->actor_activations++;	/*number of times same actor run*/
            (*scan->actor_func)(scan);	/*run actor, get replacment func*/
        }
        if(scan->actor_time<gl.time_wake)
            gl.time_wake=scan->actor_time;
    }
}

/* print a delta time in seconds in human readable form
  */
void pr_delta_time(fdst,atime)
FILE *fdst;
long atime;
{
    int days,hours,mins,secs;
    if(atime<0) {
        atime = -atime;
        fprintf(fdst,"-");
    }
    days  = atime / (DAY);
    atime -= days * DAY;
    hours = atime / (HOUR);
    atime -= hours * HOUR;
    mins = atime / MINS;
    atime -= mins * MINS;
    secs = atime;
    if(days!=0)
        fprintf(fdst,"%d days %d hrs",days,hours);
    else if(hours!=0)
        fprintf(fdst,"%d hrs %d min",hours,mins);
    else if(mins!=0)
        fprintf(fdst,"%d min %d sec",mins,secs);
    else
        fprintf(fdst,"%d sec",secs);
}

/*
  convert a boolean to it text form for printing
      */
char prbool(abool)
int abool;
{
    return ((abool)?'f':'t');
}

/*
  print all information we know about this guardian
  */
void print_guard_detailed(dfile,aguard)
FILE *dfile;
guard_entry_pt aguard;
{
    fprintf(dfile,"%s",aguard->hname);
    if(!aguard->addr_valid) {
        fprintf(dfile," can't find ip address\n");
        return;
    }
    fprintf(dfile," retries=%d wakeup=",
             aguard->actor_activations);
    pr_delta_time(dfile,(aguard->actor_time-gl.time_now));
    fprintf(dfile," %s  ",inet_ntoa(aguard->addr.sin_addr.s_addr));
    if(aguard->rcv_time==0) {
        fprintf(dfile," never replied\n");
        return;
    }
    fprintf(dfile,"\n last up ");
    pr_delta_time(dfile,(gl.time_now-aguard->rcv_time));
    fprintf(dfile," ago weight=%d\n",ntohl(aguard->weight));
    fprintf(dfile," last rcv pkt transmit time %s",
             ctime(&aguard->load_dat.timestamp));
    fprintf(dfile,
             " stats vers=%d %s up=",
             ntohl(aguard->load_dat.gstat_version),
             inet_ntoa(ntohl(aguard->load_dat.ipAddress)));
    pr_delta_time(dfile,ntohl(aguard->load_dat.time_up));
    fprintf(dfile," child=%d/%d\n",
             ntohl(aguard->load_dat.servers),ntohl(aguard->load_dat.max_servers));
    aguard->load_dat.g_version[31]=0; /*make sure string is terminated*/
    fprintf(dfile,
             " users=%ld/%ld load=%ld/%ld/%ld vers='%s' logins=%c\n",
             ntohl(aguard->load_dat.users),
             ntohl(aguard->load_dat.max_users),
             ntohl(aguard->load_dat.load_avg[0]),
             ntohl(aguard->load_dat.load_avg[1]),
             ntohl(aguard->load_dat.load_avg[2]),
             aguard->load_dat.g_version,
             prbool(aguard->load_dat.accept_remote_req));
    fprintf(dfile,
             " user.permits=%c setuid=%c verbose err=%c debug=%lx reboot time=%d\n\n",
             prbool(aguard->load_dat.permits_file),
             prbool(aguard->load_dat.run_as_user),
             prbool(aguard->load_dat.verbose),
             ntohl(aguard->load_dat.debug_flags),
             ntohl(aguard->load_dat.reboot_time));
}   

/*
  print all the gaurd table entries
  in detailed format
  */
void print_guards_detailed(dfile)
FILE *dfile;
{
    register int i;
    register guard_entry_pt scan;
    if(gl.free_guard==0)
        fprintf(dfile,"Server pool not defined\n");
    else
        for(i=0,scan=gl.guard_table;i<gl.free_guard;i++,scan++)
            print_guard_detailed(dfile,scan);
}

/*
  convert an errno to text
  */
void print_errno(dfile,anerr)
FILE *dfile;
int anerr;
{
    if((anerr<0)||
        (anerr>sys_nerr))
        fprintf(dfile,"unknown error %d",anerr);
    else
        fprintf(dfile,"%s",sys_errlist[anerr]);
}

/*
  print all the globals for debugging purposes
      */
void print_globals(dfile)
FILE *dfile;
{
    register int i;
    fprintf(dfile,"nametimeg %d.%d internal table dump\n",VER_MAJ,VER_MIN);
    fprintf(dfile,"nametimeg $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/nametime/RCS/nmtimeg.c,v 2.37 1993/05/04 01:49:29 susan Exp $\n");
    fprintf(dfile,"nametimeg $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/nametime/RCS/nmtimeg.c,v $\n");
    fprintf(dfile,"this nametimeg stated at %s",ctime(&gl.time_started));
    fprintf(dfile,"we handled %d name requests\n",gl.number_of_ien116_real_names);
    fprintf(dfile,"we handled %d ien-116 pool requests\n",gl.number_of_ien116_pool_names);
    fprintf(dfile,"we handled %d domain pool requests\n",gl.number_of_domain_requests);
    fprintf(dfile,"polling %d guadians\n",gl.free_guard);
    fprintf(dfile,"debugging=%d\n",gl.debugging);
    fprintf(dfile,"our udp socket %d\n",gl.load_socket);
    fprintf(dfile,"guardian stat port %d\n",gl.guard_stat_port);
    fprintf(dfile,"wakeup at %s",ctime(&gl.time_wake));
    fprintf(dfile,"time_now %s",ctime(&gl.time_now));
    for(i=0;i<=ST_MAX;i++)
        fprintf(dfile," %s:%d\n",st_names[i],gl.st_stats[i]);
    fprintf(dfile,"last system failure codes:\n");
    for(i=0;i<=ER_MAX;i++) {
        fprintf(dfile," %s:",er_names[i]);
        print_errno(dfile,gl.errno_tab[i]);
        fprintf(dfile,"\n");
    }
    fprintf(dfile," addr of last sendto failure:%s\n",inet_ntoa(gl.sendto_addr.sin_addr.s_addr));
}

/*
  print everything in the world for debugging
      */
void print_all(dfile)
FILE *dfile;
{
    print_globals(dfile);
    print_guards_detailed(dfile);
}

/*
  spill our guts to a fill return a short message about how it went
  */
void print_all_to_file()
{
    FILE *outfile;
    unlink(INTERNAL_DUMP_LOCATION);
    outfile=fopen(INTERNAL_DUMP_LOCATION,"w");
    if(outfile==0) return;
    print_all(outfile);
    fclose(outfile);
    printf("Internal data dumped to %s\n",INTERNAL_DUMP_LOCATION);
}

/*
  debugging routine, call it from a debugger
  to dump everything we know to a file
  */
void dumpall()
{
    gl.wana_data_dump=0;
    print_all_to_file();
}

/*
  return a value of how nice it would be for a user to use this host
      the smaller the value the better the host is
      */
int guard_value(aguard)
guard_entry_pt aguard;
{/*if we can't get the ip adress host not very usefull*/
    if(!(aguard->addr_valid))
        return 1000000;
    /*next least bad are dead hosts*/
    if(!(is_guard_alive(aguard)))
        return 999999;
    {
        int going_down=(ntohl(aguard->load_dat.reboot_time)!=0);
        long down_how_soon=(ntohl(aguard->load_dat.reboot_time)-gl.time_now);
        int down_soon=(going_down && (down_how_soon < (15*MINS)));
        int load_av=(ntohl(aguard->load_dat.load_avg[1])+aguard->weight);
        int host_value=load_av*100+ntohl(aguard->load_dat.servers);

        /*then hosts with logins off
          still consider the load since
          although guardian logins are off
          some other kind may be on
          */
        if(is_guard_refused(aguard))
            host_value += 200000;

        if(down_soon < (15*MINS))
            host_value += 10000;

        return host_value;
    }
}

/*
  return the best host
  */
guard_entry_pt get_best_guard(namept)
char *namept;
{
    register int i;
    register guard_entry_pt scan;
    long best_value=MAX_INT;
    register guard_entry_pt best_guard=NIL;
    char ch;
    int name_len;
    {char *s;
    s=index(namept,'.');
    if(s!=NIL)
        name_len=(s-namept);
    else
        name_len=strlen(namept);
    }
    if(name_len==0)return best_guard;
    /*build up the list of matching machines*/
    for(i=0,scan=gl.guard_table;i<gl.free_guard;i++,scan++) {
        int this_value;
        if(!lcstrneq(namept,scan->hname,name_len)) /*is this a pool machine?*/
            continue;     /*no*/
        /*make sure that after match server number is next in host name*/
        ch=scan->hname[name_len];
        if((ch<'0')||(ch>'9'))
            continue;
        this_value=guard_value(scan);
        if(this_value>best_value)
            continue;
        best_value=this_value;
        best_guard=scan;
    }
    /*
      since we are sending someone to this host inflate the load so we
      don't send too many users to the same host all at once
      */
    if(best_guard!=NIL)
        best_guard->weight++;
    return best_guard;
}

/*
  initialize pool entries for a given pool
      */
void init_pool(pool_name,num_ent)
char *pool_name;
int num_ent;
{
    register int i;
    register guard_entry_pt aguard;
    for(i=0;i<num_ent;i++) {
        aguard=aloc_guard();
        if(aguard==0L)return;
        sprintf(aguard->hname,"%s%d",pool_name,(i+1));
        set_guard_addr(aguard);
    }
}

/*
  initilize pool entries for all pools
      */
void init_pools(argc,argv)
int argc;
char *argv[];
{
    char *pool_name;
    char *pool_size_s;
    int pool_size_n;
    argc--; /*program name is not interesting*/;
    if((argc<2)||((argc % 2)!=0))
        fatal_error(("must have list of pool name and sizesf on command line\n example: nametimeg pcs 5 unix 8"));
    while(argc>0) {
        pool_size_s = argv[argc--];
        pool_name= argv[argc--];
        if(sscanf(pool_size_s,"%d",&pool_size_n)!=1)
            fatal_error(("can't parse pool size for pool:%s",pool_name));
        init_pool(pool_name,pool_size_n);
    }
}

int lcstreq(s1,s2)
char *s1;
char *s2;
{register int s1len=strlen(s1);
 return ((s1len==strlen(s2))&&lcstrneq(s1,s2,s1len));
}

/*
  case insensitive string compare with length
      */
int lcstrneq(s1,s2,len)
register char *s1;
register char *s2;
register int len;
{
    register char ch1,ch2;
    while (--len >=0) {
        ch1= *s1++;ch2 = *s2++;
        if(isupper(ch1))ch1=tolower(ch1);
        if(isupper(ch2))ch1=tolower(ch2);
        if(ch1!=ch2)
            return FALSE;
    }
    return TRUE;
}

/*
  error catch for domain name errors
      */
void dn_error_catch(str,a1,a2,a3,a4,a5,a6,a7,a8)
char *str;
long a1,a2,a3,a4,a5,a6,a7,a8;
{
    debug(("domain request from:%s size:%d errno:%d\n",
            inet_ntoa(gl.nh.from.sin_addr.s_addr),
            gl.nh.look_size,
            errno));
    debug((str,a1,a2,a3,a4,a5,a6,a7,a8));
    osi_longjmp(gl.nh.dthrow,1);
}

/*
  unpack a domain name from request
  */
char *dn_unpack(inp,ioutname)
char *inp;
char *ioutname;
{
    int dnamelen;		  /*length of name in dnamebuf (not incl null)*/
    register char *outname=ioutname;
    *outname=0;
    while (TRUE) {
        dnamelen= *inp++;
        if(dnamelen==0)
            break;
        if((dnamelen&INDIR_MASK)!=0)
            domain_error(("we don't implement indirect strings\n"));
        if((dnamelen<0)||(dnamelen>MAXDNAME))
            domain_error(("length of %d is bogus\n"));
        if((inp-MAX_INP)>=0)
            domain_error(("logical packet length %d longer than physical\n",inp-gl.nh.io_buf));

        /*slurp the name out of the input stream*/
        bcopy(inp,outname,dnamelen);
        inp +=dnamelen;
        outname +=dnamelen;
        *outname++ = '.';
    }
    if(outname!=ioutname) {
        /*remove the trailing dot*/
        outname--;
        *outname=0;
    }
    return inp;
}

void dn_check_header()
{
  if(gl.nh.look_size<sizeof(HEADER))
    domain_error(("request packet of %d too small\n",gl.nh.look_size));
  if(gl.nh.ih->qr!=0)
    domain_error(("expected query packet got response\n"));
  if(gl.nh.ih->opcode!=QUERY)
    domain_error(("don't know about opcode %d\n",gl.nh.ih->opcode));
  if(((ntohs(gl.nh.ih->qdcount))!=1)||
    (gl.nh.ih->ancount!=0)||
    (gl.nh.ih->nscount!=0)||
    (gl.nh.ih->arcount!=0))
    domain_error(("counts in query out of wack\n"));
}

#define PUT16(xx_dst,xx_var) \
{putshort(xx_var,xx_dst);xx_dst+=sizeof(u_short);}

#define PUT32(xy_dst,xy_var) \
{putlong(xy_var,xy_dst);xy_dst+=sizeof(u_long);}

/*
  emit a domain name string
  */
char *emit_string(inp,name)
char *inp;
char *name;
{
  register int n;
  if((n=dn_comp(name,inp,(MAX_INP-inp),gl.nh.dnptrs,gl.nh.lastdnptr))<0)
    domain_error(("dn_comp failed\n"));
  return inp+n;
}

/*
  emit a resource header
  */
char *emit_header(inp,name,etype,eclass,ettl)
char *inp;
char *name;
int etype;
int eclass;
long ettl;
{
    inp=emit_string(inp,name);
    PUT16(inp,etype);	  /*answer type*/
    PUT16(inp,eclass);	  /*class is internet*/
    PUT32(inp,ettl);	  /*time to live of zero*/
    return inp;
}

char *emit_cname(inp,official_name,dname)
char *inp;
char *official_name;
char *dname;
{
    int len;
    char *s;
    inp=emit_header(inp,dname,T_CNAME,C_IN,0L);
    s=inp;
    PUT16(inp,0);         /*fake length for now*/
    inp=emit_string(inp,official_name);
    len=(inp-s)-2;
    PUT16(s,len);         /*backpatch the real name*/
    return inp;
}

char *emit_a(inp,iaddr,dname)
char *inp;
long iaddr;
char *dname;
{
    inp=emit_header(inp,dname,T_A,C_IN,TIME_TO_LIVE);
    PUT16(inp,4);	  /*four bytes of resource data*/
    bcopy(&iaddr,inp,4); /*put out the ip address answer*/
    inp+=4;
    return inp;
}


char *dn_unpack_query(inp,rqtype,rclass)
char *inp;
int *rqtype;
int *rclass;
{   register int qclass,qtype;
#define SLURP16(xinto,xfrom) \
    {xinto=(((*xfrom++)&0xff)<<8); \
    xinto += ((*xfrom++)&0xff);}
/*not defined on sun3    {xinto= _getshort(xfrom);xfrom+=sizeof(u_short);} */
    SLURP16(qtype,inp);
    SLURP16(qclass,inp);
#undef SLURP16
#ifdef DREW_SAYS_SO
    if((qtype!=T_A)&&(qtype!=T_CNAME))
        domain_error(("query type %d not implemented\n",qtype));
#endif
    if((qclass!=C_IN)&&
        (qclass!=C_ANY))
        domain_error(("unknown class %d\n",qclass));
    *rclass = qclass;
    *rqtype = qtype;
    return inp;
}

void send_dn_reply(inp)
char *inp;
{
    if(sendto(gl.nh.dsock,
               gl.nh.io_buf,
               inp-gl.nh.io_buf,
               0,
               &gl.nh.from,
               sizeof(NETADDR))<0) {
        gl.st_stats[ST_sendtofail]++;
        gl.errno_tab[ER_sendto_errno]=errno;
        domain_error(("sendto failed\n"))
    }

}

/*
  get official name and ip_address
*/
long lookup_official_name(alias_name,real_name)
char *alias_name;
char *real_name;
{
 long iaddr;
 register struct hostent *hp;
 if((!gl.testing)||((hp=gethostbyname(alias_name))==0))
   domain_error(("no hosts matched requested name\n"));
 bcopy(hp->h_addr,&iaddr,4);
 strcpy(real_name,hp->h_name);
 return iaddr;
}

/*
  get official name and ip_address
*/
long get_official_name(alias_name,real_name)
char *alias_name;
char *real_name;
{
 guard_entry_pt best;
 if((best=get_best_guard(alias_name))==NIL)
   return lookup_official_name(alias_name,real_name);
 strcpy(real_name,best->hname);
 return best->addr.sin_addr.s_addr;
}

/*
  process the recieved domain lookup request
  */
void do_domain_lookup()
{
      char *inp;		  /*scan input message*/
      int qtype;		  /*query type*/
      int qclass;		  /*query class*/
      char dnamebuf[MAXDNAME+4]; /*the name being looked up*/
      char official_name[MAXDNAME+4]; /*the name being looked up*/
      long iaddr;		/*ip address of real machine*/

      dn_check_header();

      /*parse the variable length question*/
      inp=gl.nh.io_buf+sizeof(HEADER);

      {register int n;
       if((n=dn_expand(gl.nh.io_buf,MAX_INP,inp,dnamebuf,MAXDNAME))<=0)
        domain_error(("can't expand name\n"));
       inp+=n;
       }

      debug(("name lookup %s from %s\n",
	     dnamebuf,
	     inet_ntoa(gl.nh.from.sin_addr.s_addr)));

      inp=dn_unpack_query(inp,&qtype,&qclass);

      if((inp-gl.nh.io_buf)>gl.nh.look_size)
          domain_error(("physical packet not long enough for logical packet\n"));

      /*prepare answer*/
      gl.nh.ih->qr=1;           /*packet is now a response*/
      gl.nh.ih->aa=0;		/*it is not authorative*/
      gl.nh.ih->rcode=NOERROR;    /*response type is successfull*/
      gl.nh.ih->ancount=ntohs(1);

      iaddr=get_official_name(dnamebuf,official_name);

      if(gl.testing) {
	if(!lcstreq(official_name,dnamebuf)) {
	  inp=emit_cname(inp,official_name,dnamebuf);
	  gl.nh.ih->ancount=ntohs(2);
	}
        inp=emit_a(inp,iaddr,official_name);
      }else /*not testing, just put out cname*/
	inp=emit_cname(inp,official_name,dnamebuf);

      if(inp>=MAX_INP)
          domain_error(("overflowed output packet\n"));

      send_dn_reply(inp);
  }

/*
 reset global stat for dn compression routines
*/
void init_dn_comp()
{gl.nh.lastdnptr = gl.nh.dnptrs + sizeof(gl.nh.dnptrs)/sizeof(gl.nh.dnptrs[0]);
}

void get_and_do_domain_lookup()
  {
      int fromlen=sizeof(NETADDR);	/*size of recieved packet*/
      gl.nh.ih=(HEADER*)gl.nh.io_buf;

      gl.nh.catch=dn_error_catch;	/*error catcher*/

      if(osi_setjmp(gl.nh.dthrow)!=0)
          return;

      init_dn_comp();

      gl.number_of_domain_requests++;
      if((gl.nh.look_size=recvfrom(gl.nh.dsock,
                                    gl.nh.io_buf,
                                    sizeof(gl.nh.io_buf),
                                    0,
                                    &gl.nh.from,
                                    &fromlen))<0) {
          gl.st_stats[ST_recvfrom]++;
          gl.errno_tab[ER_recvfrom_errno]=errno;
          domain_error(("recvfrom failed\n"))
      }
      do_domain_lookup();
      return;		  /*success*/
  }

/*
  find_addr - lookup a hostname return
  true if it is found
      */
int find_addr(namept,ans)
char *namept;
NETADDR *ans;
{
    register struct hostent *hp;
    guard_entry_pt best;
    if((best=get_best_guard(namept))!=NIL) {
        bcopy(&best->addr,ans,sizeof(NETADDR));
        gl.number_of_ien116_pool_names++;
        return TRUE;
    }

    gl.number_of_ien116_real_names++;
    hp=gethostbyname(namept);
    if(hp==0) {
        debug(("name lookup on '%s' failed",namept));
        return FALSE;
    }
    if((hp->h_addrtype!=AF_INET)||((hp->h_length)!=4)) {
        debug(("address of '%s' not on internet",namept));
        return FALSE;
    }
    bcopy(hp->h_addr,&ans->sin_addr.s_addr,hp->h_length);
    return TRUE;
}

/*
  handle an ien116 name lookup request
  */
void name_lookup()
{
    char io_buf[100];
    int fromlen=sizeof(NETADDR);	/*size of recieved packet*/
    NETADDR from;
    NETADDR ans_addr;
    int look_size;
    int name_length;
    if((look_size=recvfrom(gl.nsock,io_buf,sizeof(io_buf),
                            0,&from,&fromlen))<0) {
        gl.st_stats[ST_recvfrom]++;
        gl.errno_tab[ER_recvfrom_errno]=errno;
        return;
    }
    { char *scan_buf=io_buf;
    if((look_size<3)||
        (*scan_buf++ != 1)||
        ((name_length= *scan_buf++)!=(look_size-2))) {
        debug(("recieved name lookup is to small"));
        /*??? add an error counter here*/
        return;
    }
    scan_buf[name_length]=0; /*make it null terminated*/
    if (find_addr(scan_buf,&ans_addr)) {
        unsigned long myaddr=ans_addr.sin_addr.s_addr;
        scan_buf+=name_length;
        *scan_buf++ =2;
        *scan_buf++ =4;
        *scan_buf++ =((myaddr>>24)&0xff);
        *scan_buf++ =((myaddr>>16)&0xff);
        *scan_buf++ =((myaddr>> 8)&0xff);
        *scan_buf++ =((myaddr    )&0xff);
        name_length += 8;
    }
    else {
        scan_buf+=name_length;
        *scan_buf++ =3;
        *scan_buf++ =2;
        *scan_buf++ =1;
        *scan_buf++ ='?';
        name_length+=6;
    }
    }
    if(sendto(gl.nsock,io_buf,name_length,0,&from,sizeof(NETADDR))<0) {
        gl.st_stats[ST_sendtofail]++;
        gl.errno_tab[ER_sendto_errno]=errno;
        return;
    }

}

/*
  init the name server sockets
  */
void nh_init()
{
    struct sockaddr_in client;
    struct servent *servent;
    gl.nsock=socket (AF_INET,SOCK_DGRAM,0);
    if(gl.nsock<0)fatal_error(("can't open name server socket"));
    client.sin_addr.s_addr=INADDR_ANY;
    client.sin_family=AF_INET;
    if(servent = getservbyname(IPNAME_NAMESERVER,(char*)0)) {
	client.sin_port = servent->s_port;
    }
    else {
	fprintf(stderr,"nh_init(): Could not find '%s' service in /etc/services.  Using default port '%d'.\n", IPNAME_NAMESERVER, IPPORT_NAMESERVER);
	client.sin_port = IPPORT_NAMESERVER;
    }
    if(bind(gl.nsock,&client,sizeof client)<0)
        fatal_error(("can't bind name server port"));

    gl.nh.dsock=socket (AF_INET,SOCK_DGRAM,0);
    if(gl.nh.dsock<0)fatal_error(("can't open time server socket"));
    client.sin_addr.s_addr=INADDR_ANY;
    client.sin_family=AF_INET;
    client.sin_port=NAMESERVER_PORT;
    if (bind(gl.nh.dsock,&client,sizeof client) < 0)
        fatal_error(("can't bind time server port"));
}

void wait_for_something_to_do_then_do_it()
  {
      long rfd,nfds;
      struct timeval timeout;
      long sleep_seconds;
      new_now();
      sleep_seconds = gl.time_wake - gl.time_now;
      sleep_seconds = max(MIN_SLEEP,sleep_seconds);
      sleep_seconds = min(sleep_seconds,AL_DELAY_max);
      timeout.tv_sec = sleep_seconds;
      timeout.tv_usec = 0;
      rfd = (1 << gl.load_socket);
      rfd|= (1 << gl.nsock);
      rfd|= (1 << gl.nh.dsock);
#ifdef LOTS_OF_OUTPUT
      debug(("select rfd=%x, timeot=%d\n",rfd,sleep_seconds));
#endif
      nfds = select((sizeof(rfd)*8),&rfd,0,0,&timeout);
      if(nfds < 0)
          if(errno==EINTR) 	/*Can you say bogus operating system designer?*/
              nfds=0;		/*There I new you could!*/
          else
              fatal_error(("select failed"));
      if(gl.wana_data_dump!=0)
          dumpall();
      new_now();
      if(nfds != 0)
          if(rfd & (1<<gl.load_socket))
              recieve_load();
          else if (rfd & (1 << gl.nsock))
              name_lookup();
          else if (rfd & (1 << gl.nh.dsock))
              get_and_do_domain_lookup();
          else fatal_error(("select on fd"));
      run_actors();
  }

/*
  redirect our stdout and stderr output to a pipescript
  */
void pipe_on()
{
    FILE* apipe;
    extern FILE *popen();
    gl.debugging=1;
    apipe=popen("/usr/andrew/bin/pipescript -t 'nametimeg debugging'", "w");
    if(apipe==0){
        debug(("couldn't open pipescripte\n"));
        return;
    }
    if(dup2(fileno(apipe),1)==(-1)) {
        debug(("couldn't redirect stdout\n"))
          return;
    }
    if(dup2(fileno(apipe),2)==(-1)) {
        debug(("couldn't redirect stderr\n"))
          return;
    }
    debug(("nametimeg %d.%d debugging on\n",VER_MAJ,VER_MIN));
    debug(("testing=%d\n",gl.testing));
}

/*
  see if nametimeg is being debugged
      */
void check_for_debugging()
{
    int cfd;
    if (!access("/debug.nametimeg", F_OK)) /*print debugging info?*/
        pipe_on();
    else if(cfd=open("/dev/console",O_WRONLY,0644)<0)
        printf("nametimeg:can't open console");
    else { 
        dup2(cfd,1);
        dup2(cfd,2);
        setlinebuf(fdopen(1,"w"));
        setlinebuf(fdopen(2,"w"));
        close(cfd);
    }
}

void dump_signal_handler(signum)
int signum;
{
    gl.wana_data_dump++;
}

main(argc,argv)
int argc;
char **argv;
{
    bzero(&gl,sizeof(gl));
    /*see if we are talking to a real domain name server*/
    gl.testing=FALSE;
    if(((argc>1)&&(strcmp(argv[1],"-t")==0))) {
      gl.testing=TRUE;
      argc--;
      argv++;
    }
    signal(SIGUSR1,dump_signal_handler);
    gl.time_started=time(0);
    check_for_debugging();
    nh_init();
    lookup_guard_stat_port();
    create_load_socket();
    init_pools(argc,argv);
    new_now();
    while(TRUE)
        wait_for_something_to_do_then_do_it();
}
