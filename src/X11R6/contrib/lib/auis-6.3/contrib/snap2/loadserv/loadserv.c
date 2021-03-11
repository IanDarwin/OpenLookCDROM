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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/loadserv/RCS/loadserv.c,v 2.17 1993/01/15 19:09:03 gk5g Exp $";
#endif


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
/* loadserv program versions*/

#define VER_MAJ (0)
#define VER_MIN (12)

/*how big a message of the day can be */
#define MOTD_SIZE (4000)

/*ask each guardian its loadaverage*/
#define AL_DELAY_ask (1L*MINS)
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
/*how long to wait for snap*/
#define AL_DELAY_snap (14L*SEC)
/*how long to wait for some data to exit before answering the first time*/
#define AL_DELAY_snapon (30L*SEC)

/* maximum number of guardians to poll*/
#define NUM_GUARDS (30)
/*maximum length of a host name*/
#define MAX_HOST_NAME (40)
/*file with list of guardians to watch*/
#define LOADSERV_FILE "/etc/loadserv.ini"
/*the maximum length line in LOADSERV_FILE */
#define LINE_BUF_SIZE 1200
/*name of the service to get guardian statistics*/
#define GUARD_STAT_NAME "snap.guardian03"
/*maximum size of a token in the output text (ip addr, dela time) */
#define OUTPUT_TOKEN_MAX (70)
/*where to dump all internal tables in a text file*/
#define INTERNAL_DUMP_LOCATION "/tmp/loadserv.int"
/* default number of users to print*/
#define ITOPN (10)

#include <andrewos.h> /* sys/types.h sys/time.h sys/file.h */
#include <stdio.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/socket.h>
#include <gasp.h>
#include <gstats.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <loadserv.h>

#define TRUE (0==0)
#define FALSE (0==1)
#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */
extern int sys_nerr;
extern char *sys_errlist[];
extern int errno;

#define imin(xx1,xx2) (((xx1)<(xx2))?(xx1):(xx2))
#define imax(xx1,xx2) (((xx1)>(xx2))?(xx1):(xx2))
#define fatal_error(xx_arg) {printf xx_arg;fflush(stdout);perror("sys err");exit(1);}
#define debug(xx_arg) {if(gl.debugging!=0){printf xx_arg;fflush(stdout);}}
typedef void (*ACTOR_FUNC_PT)();

/*
  guardian entry, one per gaurdian we watch
*/
struct guard_entry_R {
    int addr_valid;	/*true if addr is set*/
    NETADDR addr;		/*adrress of this guardian*/
    char hname[MAX_HOST_NAME]; /*this hosts text net name*/
    long actor_activations; /*number of times doing same actor*/
    long actor_time;	/*next time to run actor_func*/
    ACTOR_FUNC_PT actor_func; /*routine to handle time event*/
    long weight;	/*extra load for each time we print this one first*/
    long rcv_time;	/*last time this load av recieved or zero*/
    stat_packet_t load_dat; /*last loadav from this host*/
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
#define ER_ini_io 3		/*read/close on init file failed*/
    "io error on ini file",
};
#define ER_MAX 3

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
#define ST_snap_reply 8
    "snapreply",
#define ST_ini_io_err 9
    "read error on init file",
};
#define ST_MAX 9

/* watcher for changes in W_INI_FILE*/
#define W_INI_FILE 0
/* watcher for chanes in motd file*/
#define W_MOTD_FILE 1
#define NUM_FILE_WATCHES (2)

/*
  info for a file that may change
      stattime is the inode write time the last time we read it
      */
struct watch_file_t_R {
    long stattime;		/*time from last stat call or zero*/
    FILE *ffile;		/*this file when it is open*/
    ACTOR_FUNC_PT factor; /*actor to handle file changes*/ 
    char fname[MAXPATHLEN]; /*this files name*/
};
typedef struct watch_file_t_R watch_file_t,*watch_file_t_pt;

struct sn_data_R {
    int fd;		/*file descriptor for snap connection*/
    int auth;		/*0-unauthenticated 1-authenticated*/
    char *cliname;	/*client name*/
    SNAP_CPARMS parms;	/*snap connection paramaters*/
    long start_time;	/*don't read snap packets before this or zero*/
};
typedef struct sn_data_R sn_data;

/*
  global data
  */
struct gl_R {
    int wana_data_dump;     /*set by signal handler to dump data file*/
    watch_file_t wfiles[NUM_FILE_WATCHES];
    char motd_buf[MOTD_SIZE]; /*the contents of the motd file*/ 
    sn_data snp;		/*snap globals*/
    int debugging;        /*non zero if printing debugging info*/
    int load_socket;	/*unix socket for asking gaurdian questions*/
    int guard_stat_port;	/*ip port number guardian listens on*/
    long time_started;	/*what time this server was started*/
    long time_wake;	/*time wake up the next actor*/
    long time_watch;	/*time to check for changed files*/
    long time_now;	/*very recent result of time(0) call*/
    char par_ch;		/*current character*/
    char *par_str;	/*rest of current line being parsed*/
    int par_lineno;	/*line number for error messages*/
    int par_linerr;	/*last line in file with a parse error or 0*/
    int free_guard;	/*index of next free entry*/
    guard_entry guard_table[NUM_GUARDS];
    int ask_buf[2];	/*request to send to guardian*/
    long st_stats[ST_MAX+1]; /*interesting records about us*/
    int errno_tab[ER_MAX+1]; /*errnos for recent errors*/
    NETADDR sendto_addr;  /*netaddr of last sendto failure*/
    int snap_is_running;  /*false if running under debugger w/o guadian*/
    int topn;             /*how many users to print*/
    long number_of_requests_processed;
}gl;

#define dst_printf(xx_arg) \
(((long)sprintf xx_arg),((long)(dst=dst+strlen(dst))))
#ifdef RUBBISH
/*
  working sprintf - on sun computers the bogons don't return
			 the number of characters printed from sprintf
			 */
#include <varargs.h>
/*VARAGS*/
int wsprintf(va_alist)
va_dcl
{
    va_list args;
    char *dst,*fmt;
    va_start(args);
    dst=va_arg(args,char *);
    fmt=va_arg(args,char *);
    vsprintf (dst,fmt,args);
    va_end(args);
    return strlen(dst);
}
#endif /* RUBBISH */
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
    if(aguard->actor_func!=aactor);
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
    bcopy (hp->h_addr,&aguard->addr.sin_addr.s_addr,hp->h_length);
    {
	int i;
	for (i=0;i<8;i++)
	    aguard->addr.sin_zero[i] = 0;
    }
    aguard->addr_valid=TRUE;
    set_guard_actor(aguard,ask_load,0);
}

/*
  allocate and return a guard table entry
  */
guard_entry_pt aloc_guard()
{
    switch(gl.free_guard) {
	case (NUM_GUARDS-2):
	    debug(("loadserv:server table full with %d entries\n",NUM_GUARDS));
	    gl.par_linerr=gl.par_lineno;
	    break;
	case (NUM_GUARDS-1):
	    return 0;
	default:
	    break;
    }
    return &gl.guard_table[gl.free_guard++];
}

/*
  return the length of the current input token
  */
int toklen(astr)
register char *astr;
{
    register int result=0;
    register char ch;
    while(((ch= *astr++)>0x20)&&
	   (ch < 0x7f))
	result++;
    return result;
}

/*
  advance parsing an init file by one character
  */
void par_getch()
{
    gl.par_ch= *++gl.par_str; /*get the next char*/
    if(gl.par_ch==0) gl.par_str--; /*don't step past the ending null*/
}

/*
  advance parsing an init file until the current character
  isn't white space
				     */
void skip_white_space()
{
    while((gl.par_ch==' ')||(gl.par_ch=='\t'))
	par_getch();
}


/*
  called when the the message of the day has changed
  */
void motd_changed(awatch)
watch_file_t_pt awatch;
{
    int new_motd_size;
    /*a file open for us to read in?*/
    if(awatch->ffile==0) {
	gl.motd_buf[0]=0;	/*no it went away then*/
	return;
    }
    new_motd_size=fread(gl.motd_buf,1,MOTD_SIZE-1,awatch->ffile);
    gl.motd_buf[new_motd_size]=0;
    return;
}

/*
  initialize a file watcher
  */
void init_watch(which_file,new_name,change_actor)
int which_file;
char *new_name;
ACTOR_FUNC_PT change_actor;
{
    gl.wfiles[which_file].stattime=0;
    gl.wfiles[which_file].ffile=0;
    gl.wfiles[which_file].factor=change_actor;
    strncpy(gl.wfiles[which_file].fname,new_name,MAXPATHLEN-1);
    gl.wfiles[which_file].fname[MAXPATHLEN-1]=0;
    gl.time_watch=0;
}


/*
  parse a new motd line
  */
void parse_motd()
{
    int motd_len;
    par_getch();
    skip_white_space();
    if(gl.par_ch==0)return;
    motd_len=toklen(gl.par_str);
    if(motd_len==0)return;
    motd_len=imin(motd_len,MAXPATHLEN-1);
    *(gl.par_str+motd_len)=0;
    init_watch(W_MOTD_FILE,gl.par_str,motd_changed);
}

/*
  parse topn - top number of users to print
  */
void parse_topn()
{
    int glen;
    par_getch();
    skip_white_space();
    if(gl.par_ch==0)return;
    glen=toklen(gl.par_str);
    if(glen==0)return;
    *(gl.par_str+glen)=0;
    gl.topn=atoi(gl.par_str);
}

/*
  parse the definition of a new guardian to watch
  */
void parse_guardian()
{
    guard_entry_pt aguard;
    int glen;
    par_getch();
    skip_white_space();
    if(gl.par_ch==0)return;
    glen=toklen(gl.par_str);
    if(glen==0)return;
    *(gl.par_str+glen)=0;
    aguard=aloc_guard();
    if(aguard==0L)return;
    strncpy(aguard->hname,gl.par_str,MAX_HOST_NAME-1);
    aguard->hname[MAX_HOST_NAME-1]=0;
    set_guard_addr(aguard);
}

/*
  parse a guardian init file
  */
void parse_line()
{
    skip_white_space();
    switch(gl.par_ch) {
	case 0:
	case ';':
	    return;
	case 'n':
	case 'N':
	    parse_topn();
	    break;
	case 'g':
	case 'G':
	    parse_guardian();
	    break;
	case 'm':
	case 'M':
	    parse_motd();
	    break;
	default:
	    debug(("loadserv:bogus line '%s' in '%s'\n",gl.par_str,LOADSERV_FILE));
	    gl.par_linerr=gl.par_lineno;
	    break;
    }
}

/*
  read a line from a file
  return true iff end of file
  */
int read_a_line(afile,aline)
FILE *afile;
char *aline;
{
    char *nlpos;
    if(fgets(aline,LINE_BUF_SIZE-1,afile)==NULL) {
	if(feof(afile))return TRUE;
	gl.wfiles[W_INI_FILE].stattime=0; /*try again later*/
	gl.st_stats[ST_ini_io_err]++;
	gl.errno_tab[ER_ini_io]=errno;
	return TRUE;
    }
    /*change a possible end of line into an end of string*/
    if((nlpos=strchr(aline,'\n'))!=0)
	*nlpos=0;
    return FALSE;
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
  read in the list of guardians we watch and a motd
  */
void read_guards(afile)
FILE *afile;
{
    char linebuf[LINE_BUF_SIZE];
    gl.par_linerr=0;
    gl.par_lineno=0;
    while (TRUE) {
	gl.par_lineno++;
	new_now();
	if(read_a_line(afile,linebuf))
	    return;
	gl.par_str = linebuf -1;
	par_getch();
	parse_line();
    }
}

/*
  The watched file doesn't exist, see if this is news
      */
int stat_gone(awatch)
watch_file_t_pt awatch;
{
    /*doesn't exist, did it?*/
    if(awatch->stattime==0)return FALSE; /*no so no change*/
    awatch->stattime=0; /*did exist now doesn't*/
    return TRUE;	/*say the file is new*/
}

/*
  see if there is a new one of these to slurp in
      returns:false - no change
      returns:true
      check awatch->ffile==0 the file was just deleted
      check awitch->ffile!=0 ffile is the fopened file
      */
int fopen_if_changed(awatch)
    watch_file_t_pt awatch;
{
    struct stat stat_buf;
    /* get stat see if file exists*/
    if(stat(awatch->fname,&stat_buf)!=0)
	return stat_gone(awatch);
    /*if it exists and same time then no change*/
    if(stat_buf.st_mtime==awatch->stattime)
	return FALSE;
    /* the file exists, see if we can access it*/
    awatch->ffile=fopen(awatch->fname,"r");
    if(awatch->ffile==0)
	return stat_gone(awatch);
    debug(("reading a new '%s'\n",awatch->fname));
    awatch->stattime=stat_buf.st_mtime;
    return TRUE;
}

/*
  delay reading from the snap descriptor until after enough time has
  elapsed to get some statistics
  */
void snap_delay_startup()
{
    gl.snp.start_time=gl.time_now+AL_DELAY_snapon;
}

/*
  the guardian config file changed
  */
void ini_changed(awatch)
watch_file_t_pt awatch;
{
    /*have a new file to read in?
      if(awatch->ffile==0)
	  return;	/*no, so stick with the old one, if any*/
      gl.free_guard=0;
      bzero(gl.guard_table,sizeof(gl.guard_table));
      bzero(&gl.wfiles[W_MOTD_FILE],sizeof(gl.wfiles[W_MOTD_FILE]));
      gl.time_wake=gl.time_now;
      read_guards(awatch->ffile);
      snap_delay_startup();
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
    /*any actor that runs will set this time down*/
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

/*
  print a NETADDR in human readable from
  for now just prints ip addresses
      */
char *pr_netaddr(dst,aaddr)
register char *dst;
NETADDR *aaddr;
{
    unsigned long myaddr=aaddr->sin_addr.s_addr;
    dst_printf((dst,"ip=%d.%d.%d.%d",
		 ((myaddr>>24)&0xff),
		 ((myaddr>>16)&0xff),
		 ((myaddr>> 8)&0xff),
		 ((myaddr    )&0xff)));
    return dst;
}

/* print a delta time in seconds in human readable form
  */
char *pr_delta_time(dst,atime)
register char *dst;
long atime;
{
    int days,hours,mins,secs;
    if(atime<0) {
	atime = -atime;
	dst_printf((dst,"-"));
    }
    days  = atime / (DAY);
    atime -= days * DAY;
    hours = atime / (HOUR);
    atime -= hours * HOUR;
    mins = atime / MINS;
    atime -= mins * MINS;
    secs = atime;
    if(days!=0)
	dst_printf((dst,"%d days %d hrs",days,hours));
    else if(hours!=0)
	dst_printf((dst,"%d hrs %d min",hours,mins));
    else if(mins!=0)
	dst_printf((dst,"%d min %d sec",mins,secs));
    else
	dst_printf((dst,"%d sec",secs));
    return dst;
}

/*
  print an ip address
  */
char *pr_ipaddr(dst,an_ip)
register char *dst;
unsigned long an_ip;
{
    NETADDR as_net;
    as_net.sin_family = AF_INET; 	/* Complete network address */
    as_net.sin_port = 0;
    bcopy(&an_ip,&as_net.sin_addr.s_addr,sizeof(unsigned long));
    {
	int i;
	for (i=0;i<8;i++)
	    as_net.sin_zero[i] = 0;
    }
    return(pr_netaddr(dst,&as_net));
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
    char otbuf[OUTPUT_TOKEN_MAX];
    fprintf(dfile,"%s",aguard->hname);
    if(!aguard->addr_valid) {
	fprintf(dfile," can't find ip address\n");
	return;
    }
    fprintf(dfile," retries=%d wakeup=",
	     aguard->actor_activations);
    pr_delta_time(otbuf,(aguard->actor_time-gl.time_now));
    fprintf(dfile,"%s  ",otbuf);
    pr_netaddr(otbuf,&aguard->addr);
    fputs(otbuf,dfile);
    if(aguard->rcv_time==0) {
	fprintf(dfile," never replied\n");
	return;
    }
    fprintf(dfile," last up ");
    pr_delta_time(otbuf,(gl.time_now-aguard->rcv_time));
    fprintf(dfile,"%s ago weight=%d\n",otbuf,ntohl(aguard->weight));
    fprintf(dfile," last rcv pkt transmit time %s",
	     ctime(&aguard->load_dat.timestamp));
    fprintf(dfile,
	     " stats vers=%d ",ntohl(aguard->load_dat.gstat_version));
    pr_ipaddr(otbuf,ntohl(aguard->load_dat.ipAddress));
    fprintf(dfile,"%s up=",otbuf);
    pr_delta_time(otbuf,ntohl(aguard->load_dat.time_up));
    fprintf(dfile,"%s child=%d/%d\n",otbuf,
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
    char otbuf[OUTPUT_TOKEN_MAX];
    fprintf(dfile,"loadserv %d.%d internal table dump\n",VER_MAJ,VER_MIN);
    fprintf(dfile,"loadserv $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/loadserv/RCS/loadserv.c,v 2.17 1993/01/15 19:09:03 gk5g Exp $\n");
    fprintf(dfile,"loadserv $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/loadserv/RCS/loadserv.c,v $\n");
    fprintf(dfile,"this loadserv stated at %s",ctime(&gl.time_started));
    fprintf(dfile,"we have handled %ld requests\n",
	     gl.number_of_requests_processed);
    fprintf(dfile,"polling %d guadians\n",gl.free_guard);
    fprintf(dfile,"print the top %d best servers\n",gl.topn);
    fprintf(dfile,"last parse err in ini file or 0=%d\n",gl.par_linerr);
    fprintf(dfile,"debugging=%d\n",gl.debugging);
    for(i=0;i<NUM_FILE_WATCHES;i++) {
	fprintf(dfile,"watch '%s' stattime ",gl.wfiles[i].fname);
	if(gl.wfiles[i].stattime==0)
	    fprintf(dfile,"0\n");
	else
	    fprintf(dfile,"%s",ctime(&gl.wfiles[i].stattime));
    }
    fprintf(dfile,
	     "snap:fd=%d auth=%d client=%s maxtimeout=%d\n",
	     gl.snp.fd,gl.snp.auth,(((gl.snp.cliname)==0)?"nil":gl.snp.cliname),
	     gl.snp.parms.maxtime);
    fprintf(dfile,"timeout=%d maxmsgsize=%d encrypt=%c\n",
	     gl.snp.parms.timeout,gl.snp.parms.maxmsgsize,
	     ((gl.snp.parms.encryptlevel==SNAP_NOENCRYPT)?'N':'Y'));
    fprintf(dfile,"our udp socket %d\n",gl.load_socket);
    fprintf(dfile,"guardian stat port %d\n",gl.guard_stat_port);
    fprintf(dfile,"wakeup at %s",ctime(&gl.time_wake));
    fprintf(dfile,"check files at %s",ctime(&gl.time_watch));
    fprintf(dfile,"time_now %s",ctime(&gl.time_now));
    for(i=0;i<=ST_MAX;i++)
	fprintf(dfile," %s:%d\n",st_names[i],gl.st_stats[i]);
    fprintf(dfile,"last system failure codes:\n");
    for(i=0;i<=ER_MAX;i++) {
	fprintf(dfile," %s:",er_names[i]);
	print_errno(dfile,gl.errno_tab[i]);
	fprintf(dfile,"\n");
    }
    fprintf(dfile," addr of last sendto failure:");
    pr_netaddr(otbuf,&gl.sendto_addr);
    fprintf(dfile,"%s\n\n",otbuf);
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
  This guardian is being presented to the average user.
  Give a short sweet description.
  */
char *usr_print_guard(dst,aguard)
register char *dst;
guard_entry_pt aguard;
{
    /* we always know the name*/
    dst_printf((dst,"%s (",aguard->hname));
    if(!aguard->addr_valid) {
	dst_printf((dst,"can't locate host)"));
	return dst;
    }
    if(!is_guard_alive(aguard)) {
	dst_printf((dst,"dead)"));
	return dst;
    }
    if(is_guard_refused(aguard))
	dst_printf((dst,"refused "));
    dst_printf((dst,"load=%d users=%d",
		 (ntohl(aguard->load_dat.load_avg[1])+aguard->weight),
		 ntohl(aguard->load_dat.servers)));
    /* if going down say so */
    if(ntohl(aguard->load_dat.reboot_time)) {
	dst_printf((dst," down in "));
	dst=pr_delta_time(dst,ntohl(aguard->load_dat.reboot_time)-gl.time_now);
    }
    dst_printf((dst,")"));
    return dst;
}

/*
  return a value of how nice it would be for a user to use this host
      the smaller the value the better the host is
      */
int guard_value(aguard)
guard_entry_pt aguard;
{
    /*if we can't get the ip adress host not very usefull*/
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
	    return 200000+host_value;

	if(down_soon < (15*MINS))
	    return 10000+host_value;

	return host_value;
    }
}

/*
  qsort predicate for sorting guardians by desirability
      */
int guard_pred(aguard,bguard)
guard_entry_pt *aguard;
guard_entry_pt *bguard;
{
    register int aval=guard_value(*aguard);
    register int bval=guard_value(*bguard);
    if(aval<bval)return -1;
    if(aval>bval)return 1;
    return 0;
}

/*
  return a sorted list of guardians
  */
void get_sorted_guards(some_guards)
guard_entry_pt *some_guards;
{
    register int i;
    register guard_entry_pt scan;
    register guard_entry_pt *out_list;
    out_list=some_guards;
    /*build up the list*/
    for(i=0,scan=gl.guard_table;i<gl.free_guard;i++,scan++)
	*out_list++=scan;
    qsort(some_guards,gl.free_guard,sizeof(guard_entry_pt),guard_pred);
}

/*
  print guardian entries from a sorted list in short format
  */
char *print_sorted_guards(dst,guard_list,limit)
char *dst;
guard_entry_pt guard_list[];
int limit;
{
    register int i;
    limit=imin(limit,gl.free_guard);
    for(i=0;i<limit;i++) {
	dst=usr_print_guard(dst,guard_list[i]);
	dst_printf((dst,"\n"));
    }
    return dst;
}

/*
  print an ordered list of the first n servers
  Order them by how likely a user is to want to go there
  */
char *print_ordered(dst,limit)
register char *dst;
int limit;	/*maximum number of entries to print*/
{
    guard_entry_pt sorted_list[NUM_GUARDS];
    if(gl.free_guard==0) {
	dst_printf((dst,"No servers defined\n"));
	return dst;
    }
    get_sorted_guards(sorted_list);
    if(limit==0)limit=gl.free_guard;
    dst=print_sorted_guards(dst,sorted_list,limit);
    sorted_list[0]->weight++;
    return dst;
}


/*
  spill our guts to a fill return a short message about how it went
  */
char *print_all_to_file(dst)
register char *dst;
{
    FILE *outfile;
    unlink(INTERNAL_DUMP_LOCATION);
    outfile=fopen(INTERNAL_DUMP_LOCATION,"w");
    if(outfile==0) {
	dst_printf((dst,"couldn't open %s output file\n",INTERNAL_DUMP_LOCATION));
	return;}
    print_all(outfile);
    fclose(outfile);
    dst_printf((dst,"Internal data dumped to %s\n",INTERNAL_DUMP_LOCATION));
    return dst;
}

/*
  debugging routine, call it from a debugger
  to dump everything we know to a file
  */
void dumpall()
{
    char otbuf[OUTPUT_TOKEN_MAX];
    gl.wana_data_dump=0;
    print_all_to_file(otbuf);
    fprintf(stdout,"%s",otbuf);
}

/*
  wf_look_for_change - see if one file has changed
      */
void wf_look_for_change(awatch)
    watch_file_t_pt awatch;
{
    if(!fopen_if_changed(awatch)) /*any change?*/
	return;	/*no*/
    (*awatch->factor)(awatch); /*send changed message to actor*/
    if(awatch->ffile!=0)	/*clean up the file*/
	fclose(awatch->ffile);
}

/*
  watch files for changes
      */
void watch_files()
{
    register int i;
    register watch_file_t_pt scan;
    /*if not time yet, then nothing to do*/
    if(not_yet(gl.time_watch))
	return;
    gl.time_watch=gl.time_now+AL_DELAY_reread;
    for(i=0,scan=gl.wfiles;i<NUM_FILE_WATCHES;i++,scan++)
	if(scan->factor!=0)
	    wf_look_for_change(scan);
}

/*
  read a snap request from the client
  */
void snap_read()
{
    int len,mtype,cid;
    int result,length;
    char *request;
    char *ptr;
    char *options;
    int verbose;
    int prall;
    SNAP_INTEGER opcode;
    char send_buf[SNAP_BUF_SIZE];	/*answers we send back*/
    char tmpbuf[SNAP_BUF_SIZE];
    char *dst=tmpbuf;
    int climajver;
    int climinver;

    len=SNAP_Accept(&request,&mtype,&cid,1);
    debug(("got request, len=%d, mtype=%d\n",len,mtype))
      if(len<0)return;
    switch (mtype) {
	case SNAP_ENDCONV:
	    return;
	case SNAP_SENDWITHREPLY:
	    break;
	default:
	    return;
    }
    gl.number_of_requests_processed++;
    ptr=request;
    ptr = getint(ptr,&climajver);
    ptr = getint(ptr,&climinver);
    ptr = getint(ptr,&opcode);
    ptr = getstr(ptr,&options);
    verbose=(strcmp(options,"-d")==0);
    prall=(strcmp(options,"-l")==0);

#define RVAL (24)
    ptr = putint(send_buf,(SNAP_INTEGER)RVAL);
    /*number of strings following to print*/
    ptr = putint(ptr,(SNAP_INTEGER)2);
    if((climajver<VER_MAJ)||
	((climajver==VER_MAJ)&&(climinver<(VER_MIN-1)))) {
	dst_printf((dst,"Your loadav version %d.%d is no longer supported.\n",climajver,climinver));
	dst_printf((dst,"Please upgrade to %d.%d.\n",VER_MAJ,VER_MIN));
    }
    if(verbose)
	dst=print_all_to_file(dst);
    if(climajver<VER_MAJ) {
	ptr = putstr(ptr,tmpbuf);
	ptr = putstr(ptr,"");
    }
    else {
	if(gl.snp.start_time!=0) {
	    dst_printf((dst,"Loadserv has just started up.\n"));
	    dst_printf((dst,
			"Insufficient information at this time for a load report.\n"));
	    dst_printf((dst,"Try again in %d seconds.\n",
			imax((gl.snp.start_time-gl.time_now),2)));
	}
	else
	    dst=print_ordered(dst,(prall?0:gl.topn));
	ptr = putstr(ptr,tmpbuf);
	ptr = putstr(ptr,(prall?"":gl.motd_buf));
    }
    length = ptr - send_buf;
    result=SNAP_Reply(send_buf,length,cid);
    debug(("snap reply says %d\n",result));
    if(result<0) gl.st_stats[ST_snap_reply]++;
}

/*
  start us up as a snap server
  */
void snap_init(argc,argv)
int argc;
char *argv[];
{
    int result;
    gl.snp.parms.maxtime=AL_DELAY_snap;
    gl.snp.parms.timeout=1;
    gl.snp.parms.maxmsgsize=SNAP_BUF_SIZE;
    gl.snp.parms.encryptlevel=SNAP_ENCRYPT;
    result=GASP_ServerInit(argc,argv,
			    &gl.snp.parms,&gl.snp.cliname,&gl.snp.fd,&gl.snp.auth);
    if(result!=0)
	fatal_error(("snap init failed %d",result));
    /*schedule snap startup for after we have some data*/
    snap_delay_startup();
}

void wait_for_something_to_do_then_do_it()
  {
      long rfd,nfds;
      struct timeval timeout;
      long sleep_seconds;
      new_now();
      sleep_seconds = imin(gl.time_wake,gl.time_watch) - gl.time_now;
      sleep_seconds = imax(MIN_SLEEP,sleep_seconds);
      if(gl.snp.start_time!=0)
	  if(not_yet(gl.snp.start_time))
	      sleep_seconds=imin(sleep_seconds,(gl.snp.start_time-gl.time_now));
	  else {
	      sleep_seconds=MIN_SLEEP;
	      gl.snp.start_time=0;
	  }
      sleep_seconds = imin(sleep_seconds,AL_DELAY_max);
      timeout.tv_sec = sleep_seconds;
      timeout.tv_usec = 0;
      rfd = (1 << gl.load_socket);
      rfd|= (1 << gl.snp.fd);
      debug(("select rfd=%x, timeot=%d\n",rfd,sleep_seconds));
      nfds = select((sizeof(rfd)*8),&rfd,0,0,&timeout);
      if(nfds < 0)
	  if(errno==EINTR) 	/*Can you say shithead operating system designer?*/
	      nfds=0;		/*There I new you could!*/
	  else
	      fatal_error(("select failed"));
      if(gl.wana_data_dump!=0)
	  dumpall();
      new_now();
      if(nfds != 0)
	  if(rfd & (1<<gl.load_socket))
	      recieve_load();
	  else if (gl.snap_is_running &&(rfd & (1 <<gl.snp.fd)))
	      snap_read();
	  else fatal_error(("select on fd"));
      run_actors();
      watch_files();
  }

/*
  redirect our stdout and stderr output to a pipescript
  */
void pipe_on()
{
    FILE* apipe;
    extern FILE *popen();
    gl.debugging=1;
    apipe=popen("/usr/andrew/bin/pipescript -t 'loadserv debugging'", "w");
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
    debug(("loadserv %d.%d debugging on\n",VER_MAJ,VER_MIN));
}

/*
  see if loadserv is being debugged
      */
void check_for_debugging()
{
    if (access("/debug.loadserv", F_OK)) /*print debugging info?*/
	return;                       /*no*/ 
    if (!access("/debug.loadserv.snap", F_OK)) /*drivel everywhere?*/
	SNAP_debugmask = 0xffff;       /*yes*/
    pipe_on();
}

void dump_signal_handler(signum)
int signum;
{
    gl.wana_data_dump++;
}

int main(argc,argv)
int argc;
char *argv[];
{
    bzero(&gl,sizeof(gl));
    signal(SIGUSR1,dump_signal_handler);
    gl.time_started=time(0);
    gl.snap_is_running=(argc>2);
    check_for_debugging();
    lookup_guard_stat_port();
    create_load_socket();
    new_now();
    /*Start watching our configuration file
      hopefully it will be there and load up right away.
      Anyway, the watch stays on and new config files get loaded
      as we notice them.  If we have an init file and it goes away
      don't clear state, wait for a new init file.
	  */
    init_watch(W_INI_FILE,LOADSERV_FILE,ini_changed);
    if(gl.snap_is_running)
	snap_init(argc,argv);
    while(TRUE)
	wait_for_something_to_do_then_do_it();
}
