/*
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * Copyright (c) 1994 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of Texas System.\n\
 All rights reserved.\n";
static char rcsid[] =
"$Id: callftp.c,v 1.2 1994/05/26 22:04:29 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.2/RCS/callftp.c,v $\n";
#endif

/* $Log: callftp.c,v $
 * Revision 1.2  1994/05/26  22:04:29  jones
 * More meaning full connection close message.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include	"machine.h"
#include	"defs.h"
#include        "myfgets.h"
#include	"callrec.h"
#include        <sys/types.h>
#include        <sys/socket.h>
#include 	<netinet/in.h>
#include	<netdb.h>
#include	<arpa/telnet.h>
#include	<sys/signal.h>
#include 	<errno.h>

/*
 * Connect global data.
 */
#define PORT_COMMAND    21      /* command connection port */

#if defined(__osf__)&&defined(__alpha)
#define H_ADDR u_int
#else
#define H_ADDR u_long
#endif

struct _connect {
	struct _request *request;
	int		s;
	char    	*host;
	H_ADDR  	host_address;
	XtInputId       id;
        XtIntervalId    timeout;
	time_t		start;	
	time_t		write;	
} conn;

/*
 * List data struct.
 */
struct _listen {
	time_t  start;
	int	s;
	int	input;
	FILE	*output;
	XtInputId       id;
        int    (*convert)();
        char  *command;
        int    echo;
        int    flag;
	void (*status)();
        void (*finish)();
	CB     *cb;
	int    tag;
	int    wait;
        char  *file;
        int    rw;
	int    chars;
	int    last_chars;
        char   buff_in[2*4096];
        char   buff_out[4*4096];
	int    n;
	char   *cp;
} list;

struct _next {
    struct _next *next;
    struct _request *request;
};

#include 	"proto/callftp.h"

static int      wait    = 0;            /* Wait flag */
static char     *prompt            = "ftp> ";
static struct   _output  *output   = NULL;
static char     *next_string       = NULL;  
       int	aborting           = 0;		/* Global abort flag */
       int	ct_out  	   = 120;  	/* Conection timeout */
static int	loop_count         = 0;
static int      listen_fd	   = -1; 
       int      update_s = 0;
static char    *temp_s = NULL;
static char    *temp_ss = NULL;
static struct _next *start = NULL;
static struct _next *current = NULL;

static struct _buffs buffftp;
static struct _buffs buffdata;

int    myfgets();

#define NO  0
#define YES 1
#define BUFF_SIZE (4*4096)

void
Abort_Ftp()
{
    if (aborting) return;
    aborting = 1;

    /*
     * Make sure we are not running through the update_screen subroutine.
     */
    XtAppAddTimeOut(App_context,
                    (unsigned long)(1*1000),
                    (XtTimerCallbackProc)finish_abort,
                    (XtPointer)NULL);
}

void
Close_Ftp()
{
    if (aborting) return;
    aborting = 1;

    if (update_s)  {
        /*
         * Make sure we are not running through the update_screen subroutine.
         */
        XtAppAddTimeOut(App_context,
                        (unsigned long)(1*1000),
                        (XtTimerCallbackProc)finish_close,
                        (XtPointer)NULL);
    } else {
        finish_close(NULL, NULL);
    }
}

void 
Connect_Ftp(host, status, finish, hide, tag,  echo, data)
char *host;
void (*status)();
void (*finish)();
char *hide;
int  tag;
int  echo;
XtPointer data;
{ 
    struct _request *request;
 

    request = (struct _request *)XtMalloc(sizeof(struct _request));
    request->hide         = hide;
    request->status       = status;
    request->finish       = finish;
    request->echo         = echo;
    request->tag          = tag;
    request->command      = NULL;
    request->callback     = NULL;
    request->stuff        = NULL;
    request->data	  = data;
  
    conn.request      = request;
    conn.host         = host;
    conn.host_address = 0;
    conn.id           = 0;

    /*
     * It is possible to blow the server away with enough file transfers.
     * We will attempt to recover form this once!
     * Start a timeout daemon.
     */
    conn.timeout =  XtAppAddTimeOut(App_context,
                                    (unsigned long)(ct_out*1000),
                                    (XtTimerCallbackProc)timout_connect,
                                    (XtPointer)NULL);

    time(&conn.start);
    conn.write = 0;
    get_host_address(&conn);
}

void
Do_Prompt(xx, flag)
XtPointer xx;
int flag;
{
    struct _next *x = (struct _next *)xx;
    int echo = FTP_ECHO;

    if (prompt == NULL) return;

    if (x) {
	echo = x->request->echo;
    } else if (current) {
	echo = current->request->echo;
    }

    if (flag || (!(echo & FTP_PROMPT_NO) && (echo & (FTP_ECHO|FTP_PROMPT)))) {
	Add_Status_Text(prompt);
    }
}

int
Get_Chars()
{
    if (list.chars >= 0) return list.chars;
    return 0;
}

void 
Init_Ftp()
{
    prompt = XtNewString("ftp> ");
    list.id = 0;
    list.output = NULL;
    list.input = -1;
    list.s = -1;
    conn.s = -1;
    conn.id = 0;
    add_func_do(do_abort_listen, DO_ABORT|DO_DISCONNECT|DO_RESTART);
    add_func_do(do_abort_connect, DO_ABORT|DO_DISCONNECT|DO_RESTART);
}

void
Start_Listen(command, status, finish, tag, echo, flag, cb, file, rw)
char  *command;
void (*status)();
void (*finish)();
int    tag;
int    echo;
int    flag;
CB     *cb;
char  *file;
int    rw;
{
    struct sockaddr_in a;
    struct sockaddr_in c;
    int alen = sizeof(a);
    int clen = sizeof(c);
    char buf[64];

    list.command = XtNewString(command);
    list.echo  = echo;
    list.flag  = flag;
    list.tag   = tag;
    list.status = status;
    list.finish = finish;
    list.file = file;
    list.rw  = rw;
    list.convert  = NULL;
    list.id = 0;
    list.output = NULL;
    list.input = -1;
    list.s = -1;
    list.cb = cb;
    list.wait = 0;
    list.chars = 0;
    list.n = 0;
    list.cp = NULL;
    list.last_chars = 0;
    list.s = socket(AF_INET, SOCK_STREAM, 0);

    NONBLOCK(list.s);

    a.sin_family = AF_INET;
    a.sin_addr.s_addr = (u_int)INADDR_ANY;
    a.sin_port = 0;
    ZERO((char *)(a.sin_zero), sizeof(a.sin_zero));
    if( bind(list.s, (struct sockaddr *)&a, sizeof(a)) < 0) {
	abort_listen(&list, 0);
	return;
    }

    if (listen(list.s, 1) < 0) {
	abort_listen(&list, 0);
	return;
    }

    if (getsockname(list.s, (struct sockaddr *)&a, &alen) < 0) {
	abort_listen(&list, 0);
	return;
    }

    if (getsockname(conn.s, (struct sockaddr *)&c, &clen) < 0) {
	abort_listen(&list, 0);
	return;
    }
    sprintf(buf, "PORT %u,%u,%u,%u,%u,%u",
                 (htonl((u_int)(c.sin_addr.s_addr)) >> (3 * 8)) & 0xFF,
                 (htonl((u_int)(c.sin_addr.s_addr)) >> (2 * 8)) & 0xFF,
                 (htonl((u_int)(c.sin_addr.s_addr)) >> (1 * 8)) & 0xFF,
                 (htonl((u_int)(c.sin_addr.s_addr)) >> (0 * 8)) & 0xFF,
                 (htons((u_int)(a.sin_port)) >> (1 * 8)) & 0xFF,
                 (htons((u_int)(a.sin_port)) >> (0 * 8)) & 0xFF);

    write_ftp(buf, list_status, list_finish, NULL, 0,  0, (XtPointer)&list);
}

void
Update_Screen()
{
    while(XtAppPending(App_context) & XtIMXEvent)  {
	XtAppProcessEvent(App_context, XtIMXEvent);
    }
}

static void
abort_connect(conn, restart)
struct _connect *conn;
int    restart;
{
    struct _output *old_output = output;
    struct _next *next = start, *prev;

    free_output(old_output);
    output = NULL;
    if (restart) concat_output("777 Error restart");
    else         concat_output("776 Error reconnect");

    if (conn->request) {
        if (conn->request->finish) {
	    conn->request->finish(output, 
		                  conn->request->tag, 
			          conn->request->data);
	}
        XtFree((char *)conn->request);
	conn->request = NULL;
    }


    if(current) {
	prev = current;
	current = NULL;
	if (prev->request->finish) {
	    prev->request->finish(output, 
			          prev->request->tag, 
			          prev->request->data);
	}
        if (prev->request->hide) XtFree((char *)prev->request->hide);
        if (prev->request->command) XtFree((char *)prev->request->command);
        XtFree((char *)prev->request);
        XtFree((char *)prev);
    }

    while(next) {
	prev->request->finish(NULL, 
			      prev->request->tag, 
			      prev->request->data);
	prev = next;
	next = next->next;
	if (prev->request->hide) XtFree((char *)prev->request->hide);
	if (prev->request->command) XtFree((char *)prev->request->command);
	XtFree((char *)prev->request);
	XtFree((char *)prev);
    }

    start = NULL;
    free_output(output);
    output = NULL;
    if (conn->id) XtRemoveInput(conn->id);
    if (conn->timeout) XtRemoveTimeOut(conn->timeout);
    conn->start = 0;
    conn->write = 0;
    conn->timeout = 0;
    conn->id = 0;
    conn->s = -1;
    connected = 0;
}

static void
abort_ftp_con()
{
     aborting = 1;
     do_all(DO_DISCONNECT);
}

static void
abort_listen(l, restart)
struct _listen *l;
int    restart;
{
    struct _output *old_output = output;
    struct _next *next = start, *prev;

    free_output(old_output);
    output = NULL;

    if (restart) concat_output("777 Error restart");
    else         concat_output("776 Error reconnect");

    if (l->id) XtRemoveInput(l->id);
    l->id = 0;
    if (l->s >= 0) close(l->s);
    l->s = -1;
    if (l->input >= 0) close(l->input);
    l->input = -1;
    if (l->output) fclose(l->output);
    l->output = NULL;
    if (l->finish) l->finish(output, l->tag, (DATA)l->cb);
    /*
     * There ore too many places to worry about.
     */
    if(current) {
        prev = current;
        current = NULL;
        if (prev->request->finish) {
            prev->request->finish(output,
                                  prev->request->tag,
                                  prev->request->data);
        }
        if (prev->request->hide) XtFree((char *)prev->request->hide);
        if (prev->request->command) XtFree((char *)prev->request->command);
        XtFree((char *)prev->request);
        XtFree((char *)prev);
    }
    if (output) free_output(output);
    output = NULL;
    l->finish = NULL;
    l->convert = NULL;
    l->finish = NULL;
    l->start = 0;
}

static int
blocked(e)
int e;
{
#if defined(EAGAIN)
	if (e == EAGAIN) return 1;
#endif
#if defined(EWOULDBLOCK)
	if (e == EWOULDBLOCK) return 1;
#endif
	return 0;
}

char *
concat(s1, s2)
char *s1;
char *s2;
{
    if (s2) {
        if (s1 == NULL) {
            s1 = XtMalloc(strlen(s2)+1);
            strcpy(s1, s2);
        } else {
            s1 = XtRealloc(s1, strlen(s1)+strlen(s2)+2);
            strcat(s1, s2);
        }
    }
    if (s1 == NULL) {
	fprintf(stderr, "Ran out of memory\n", s2);
	exit(1);
	
    }
    return (s1);
}

char *
concat_output(s1)
char *s1;
{
    struct _output *next = output;
    struct _output *prev = NULL;

    if (output) {
        while(next && (next->lines == OUTPUT_LINES))  {
	    prev = next;
	    next = next->next;
	}
	if (next == NULL) {
	    next = prev->next = 
		(struct _output *)XtMalloc(sizeof(struct _output));
	    ZERO((char *)next, sizeof(struct _output));
	}
    } else {
	next = output = 
	      (struct _output *)XtMalloc(sizeof(struct _output));
	ZERO((char *)next, sizeof(struct _output));
    }
	
    next->output[next->lines] = XtNewString(s1);
    next->lines++;
}

char *
concatdir(s1, dir)
char *s1;
char *dir;
{
     int n;
     char *cp1, *cp2;

     if (dir == NULL) return s1;
     if (*dir == '\0') return s1;
     cp1 = concat(s1, dir);
     n = strlen(dir);
     if (dir[n-1] != '/') {
         cp2 = concat(cp1, "/");
	 return cp2;
     }
     return cp1;
}

static void
connect_to_host(conn)
struct _connect *conn;
{
    struct sockaddr_in a;
    int n;
    extern errno;

    a.sin_family = AF_INET;
    a.sin_port = htons(PORT_COMMAND);
    a.sin_addr.s_addr = conn->host_address;

    conn->s = socket(AF_INET, SOCK_STREAM, 0);

    NONBLOCK(conn->s);

    if (conn->request &&  conn->request->status) 
		conn->request->status(NULL, 0);

    connect(conn->s, (struct sockaddr *) &a, sizeof(a));
    conn->id =  XtAppAddInput(App_context,
		              conn->s, 
		              (XtPointer)(XtInputWriteMask|XtInputExceptMask),
                              (XtInputCallbackProc)finish_connect,
                              (XtPointer)conn);
}

static void
do_abort_connect(flag)
int flag;
{
    if (flag&DO_RESTART) {
	abort_connect(&conn, 1);
    } else {
	abort_connect(&conn, 0);
    }
}

static void
do_abort_listen(flag)
int flag;
{
    if (flag&DO_RESTART) {
	abort_listen(&list, 1);
    } else {
	abort_listen(&list, 0);
    }
}

static void
do_next()
{
    if (wait || current || !start) {
    	conn.start = 0;
	return;
    }

    current = start;
    start = start->next;
    if (current->request->echo & FTP_ECHO &&
	!(current->request->echo & FTP_COMMAND_NO)) {
	if (current->request->hide == NULL) {
	    Add_Status_Text(current->request->command);
	    Add_Status_Text("\n");
	} else {
	    Add_Status_Text(current->request->hide);
	    Add_Status_Text("\n");
	}
    } 
    loop_count = 0;
    write(conn.s, 
	  current->request->command, 
	  strlen(current->request->command));
    write(conn.s, "\r\n", 2);
    time(&conn.start);
    if (current->request->status) 
	current->request->status(current->request->command, 
			         current->request->tag);
}

static void
finish_abort(client_data, id)
caddr_t client_data;
XtIntervalId *id;
{
    if (update_s)  {
        XtAppAddTimeOut(App_context,
                        (unsigned long)(1*1000),
                        (XtTimerCallbackProc)finish_abort,
                        (XtPointer)NULL);
	return;
    }
    /*
     * Close connection and start a new one.
     */
    strcpy(last_response, "Aborting ftp session");
    abort_listen(&list, 0);
    Set_Reconnect(1);
    no_recconect_list++;
    Start_Reconnect(NULL);
}

static void
finish_close(client_data, id)
caddr_t client_data;
XtIntervalId *id;
{
    extern char *ver;
    extern int transfer_mode;
    extern int default_transfer_mode;
    
    strcpy(last_response, "Closing ftp session");
    Init_Close();
    do_all(DO_ABORT);
    transfer_mode = MODE_T_ASCII;
    default_transfer_mode =  MODE_T_ASCII;
    Clean_Up(0, NULL);
    Clear_Noop(NOOP_CLEAR_ALL);
    Set_Noop(NOOP_NOTCONN);
    Set_Logged_Out();
    aborting = 0;
    update_s = 0;
}

static void 
finish_connect( closure, source, id)
XtPointer closure;
int      *source;
XtInputId *id;
{
    struct _connect *conn = (struct _connect *)closure;
    int n; 
    char buff[1];

#ifdef KEEPALIVE
    {
	int on = 1;
        setsockopt(conn->s, SOL_SOCKET,  SO_KEEPALIVE, (char *)&on, sizeof(int));
    }
#endif

    /*
     * Make sure we have a connection.
     */
    n = write(conn->s, (char*)buff, 0);
    if (n < 0) {
	abort_connect(conn, 0);
	return;
    }

    XtRemoveInput(conn->id);
    conn->id = 0;
    current = (struct _next *)XtMalloc(sizeof(struct _next));
    ZERO((char *)current, sizeof(struct _next));
    current->request = conn->request;
    conn->request = NULL;
    connected = 1;
    conn->id = XtAppAddInput(App_context, 
			     conn->s,
                             (XtPointer)XtInputReadMask,
                             (XtInputCallbackProc)read_ftp, 
			     NULL);
    conn->start = 0;
}

static void
free_output(output)
struct _output *output;
{
    struct _output *next = output;
    struct _output *prev;
    int i;

    while(next) {
	if (next->lines > OUTPUT_LINES) {
	    fprintf(stderr, "%d is too many lines for struct _output\n", 
		    next->lines);
	    exit(1);
	}
	for (i=0; i<next->lines; i++) {
	    XtFree((char *)next->output[i]);
	}
	prev = next;
	next = next->next;
	XtFree((char *)prev);
    }
}

static void
get_host_address(conn)
struct _connect *conn;
{
    struct hostent *hp;
    int n;

    hp = gethostbyname(conn->host);

    if (hp == NULL) {
	Set_Status("Could not resolve host name");
	abort_connect(conn, 0);
	return;
    }


#if defined(CRAY)
#define OFFSET 4
#else
#define OFFSET 0
#endif

    COPY(hp->h_addr, ((char *)&conn->host_address)+ OFFSET, 4);
    connect_to_host(conn);
}

static void
list_finish(output, tag, data)
struct _output *output;
int  tag;
DATA data;
{
    int code;
    struct _listen *l = (struct _listen *)data;
    void (*temp)();
    
    code =  ftp_status(output);
    if (ftp_response[code].status & FTP_ERROR) {
        if (l->finish) l->finish(output, l->tag, (DATA)l->cb);
	l->finish = NULL;
	abort_listen(l, 0);
    } else {
	/*
	 * Don't call finish twice on a abort.
	 */
	temp = l->finish;
	l->finish = NULL;
        write_ftp(list.command, l->status, temp, NULL, l->tag, 
		      l->echo, (DATA)l->cb);
	XtFree(list.command);
    }
}

static void
list_status(command, tag)
char *command;
int   tag;
{
     char *cp = NULL;

     cp = concat(cp, "Connecting to ");
     cp = concat(cp, command);
     Set_Status(cp);
     XtFree(cp);
}

static int 
net2unix(ia, il, oa)
char *ia;
int il;
char *oa;
{
    register char *loa = oa;
    static int wait_lf = NO;

    if (il == -1) {     /* initialize for next file */
        wait_lf = NO;
        return(0);
    }
    if (il == -2) {     /* terminate current file */
        if (wait_lf) {
            *oa = '\r';
            wait_lf = NO;
            return(1);
        } else
            return(0);
    }
    for (; il > 0; ia++, il--) switch (*ia) {
        case '\r':
            wait_lf = YES;
            continue;
        case '\n':
            *loa++ = '\n';
            wait_lf = NO;
            continue;
        default:
            if (wait_lf) {
                *loa++ = '\r';
                wait_lf = NO;
            }
            *loa++ = *ia;
            continue;
    } /* for switch */
    return(loa - oa);
} /* net2unix */

static void
read_data( closure, source, id)
XtPointer closure;
int      *source;
XtInputId *id;
{
    struct _listen *l = (struct _listen *)closure;
    char buff_in[BUFF_SIZE];
    char buff_out[BUFF_SIZE];
    int n;

    Update_Screen();

    n = read(*source, buff_in, sizeof(buff_in));

    if (n < 0) {
	if (blocked(errno)) return;
	Set_Status("Error reading data");
	strcpy(last_response, "Error reading data");
	Set_Reconnect(0);
	return;
    }

    if (n == 0) {
	if (l->convert) {
	    n = l->convert((char *)0, -2, buff_out);
	    if (n) {
	    	fwrite(buff_out, n, 1, l->output);
	    }
	}
        conn.id = XtAppAddInput(App_context, 
			        conn.s,
                                (XtPointer)XtInputReadMask,
                                (XtInputCallbackProc)read_ftp, 
			        NULL);
        close(l->s);
	l->s = -1;
	l->input = -1;
	fflush(l->output);
	fclose(l->output);
	l->finish = NULL;
	l->output = NULL;
	l->convert = NULL;
        XtRemoveInput(l->id);
	l->id = 0;
        read_ftp(NULL, &conn.s, &conn.id);
	return;
    }
 
    l->chars += n;
    if (l->convert) {
	n = l->convert(buff_in, n, buff_out);
    	fwrite(buff_out, n, 1, l->output);
    } else {
    	fwrite(buff_in, n, 1, l->output);
    }
}

static void
read_dir_data( closure, source, id)
XtPointer closure;
int      *source;
XtInputId *id;
{
    struct _listen *l = (struct _listen *)closure;
    char s[LINESIZ+1];
    int more;
    int n = 0;
    int nl = 0;
    int chars = 0;
    struct _output *old_output = NULL;

    while ((more = myfgets(s, LINESIZ, *source, &buffdata)) > 0) {
	nl = 0;
	chars = n = strlen(s);
        /*
         *  Delete trailing CR/LF.
         */
        if (n > 0 && s[n - 1] == '\n') {
            s[n - 1] = '\0';
            n--;
	    nl++;
        }
        if (n > 0 && s[n - 1] == '\r') {
            s[n - 1] = '\0';
            n--;
        }
	if (nl) {
	    s[n] = '\n';
	    n++;
	}
        if (!nl) {
	    temp_ss = concat(temp_ss, s);
	} else if (temp_ss) {
	    temp_ss = concat(temp_ss, s);
	    concat_output(temp_ss);
	    XtFree(temp_ss);
	    temp_ss = NULL;
	} else {
	    concat_output(s);
        }
    }

    l->chars += chars;

    if (more < 0) {
	if (blocked(errno)) return;
 	Set_Status("Error getting directory list");
	strcpy(last_response, "Error getting directory list");
	Set_Reconnect(0);
	return;
    }

    if (more == 0) {
        if (temp_ss) {
	    temp_ss = concat(temp_ss, "\n");
	    concat_output(temp_ss);
	    XtFree(temp_ss);
	    temp_ss = NULL;
      	}
        conn.id = XtAppAddInput(App_context, 
		        	conn.s,
                                (XtPointer)XtInputReadMask,
                                (XtInputCallbackProc)read_ftp, 
			        NULL);
	close(*source);
	close(l->s);
	l->finish = NULL;
	l->s = -1;
	l->output = NULL;
        XtRemoveInput(l->id);
	l->id = 0;
        read_ftp(NULL, &conn.s, &conn.id);
    }
}

static void 
read_ftp(master, source, id)
XtPointer master;
int       *source;
XtInputId *id;
{
    int		    more;
    int		    code;
    char            s[LINESIZ];
    struct _next   *next;
    void          (*func)();
    struct _next   *temp = NULL;
    struct _output *old_output = NULL;
    int             command_echo       = FTP_ECHO;
    int		    len;
    int		    nl;

    if (current) command_echo = current->request->echo;
/*
    command_echo       = FTP_ECHO;
*/
    /*
     * Check for left overs.
     */
    if (next_string) {
	if(strncmp(next_string, "150 ", sizeof("150 ") - 1) == 0) {
	    get_peek_150(next_string);
	}
	if (command_echo & FTP_ECHO) Add_Status_Text(next_string);
	XtFree((char *)next_string);
	next_string = NULL;
	free_output(output);
        output = NULL;
    }
    while ((more = telnetfgets(s, LINESIZ, *source, &buffftp)) > 0) {
	nl = 0;
        loop_count++;
	len = strlen(s);
        /*
         *  Delete trailing CR/LF.
         */
        if (len > 0 && s[len - 1] == '\n') {
            s[len - 1] = '\0';
            len--;
	    nl++;
        }
        if (len > 0 && s[len - 1] == '\r') {
            s[len - 1] = '\0';
            len--;
        }
	if (nl) {
	    s[len] = '\n';
	    len++;
	}
	/*
         * Echo text and add it to output buffer.
	 */
	if (command_echo & FTP_ECHO) Add_Status_Text(s);
	if (INDEX(s, '\n') == NULL) {
	    temp_s = concat(temp_s, s);
	    return;
	} else if (temp_s) {
	    temp_s = concat(temp_s, s);
	    concat_output(temp_s);
	    /*
	     * Check lenght of string.
	     */
	    strcpy(s, temp_s);
	    len = strlen(s);
	    XtFree(temp_s);
	    temp_s = NULL;
	} else {
	    concat_output(s);
	}
	/*
         * Give get command a peek out the 150 ftp responses.
	 */
	if(strncmp(s, "150 ", sizeof("150 ") - 1) == 0) {
		get_peek_150(s);
	}
	if(loop_count > 20 && (XtAppPending(App_context) & XtIMXEvent)) {
	    loop_count = 0;
	    Update_Screen();
        }

	/*
	 * Shutdown command port till transfer is complete.
	 */
        /* XXX needs work */
	if((strncmp(s, "150", sizeof("150") - 1) == 0  ||
	    strncmp(s, "125", sizeof("125") - 1) == 0) &&
	   list.s >= 0) {
           XtRemoveInput(conn.id);
	   conn.id = 0;
           list.id = XtAppAddInput(App_context,
                                   list.s,
                                   (XtPointer)XtInputReadMask,
                                   (XtInputCallbackProc)start_data,
                                   (XtPointer)&list);
	   return;
	}
	
	if (len > 2 && (isdigit(s[0]) && isdigit(s[1]) && isdigit(s[2])) &&
	    (s[0] != '1') &&((len <= 4) || s[3] == ' ')) {
	    /*
	     * Process output from ftpd
	     */
	    if (wait) {
		/*
		 * Throw spurious output away.
		 */
		free_output(output);	
		output = NULL;
		return;
	    }
	    /*
	     * Pass output to output command processor.
	     */
	    wait = 1;
	    old_output = output;
	    output     = NULL;
  	    temp       = current;
  	    current    = NULL;
	    Do_Prompt((XtPointer)temp, 0);
	    if (temp != NULL) {
		update_s = 1;
		conn.start = 0;
		temp->request->finish(old_output, 
			              temp->request->tag, 
			              temp->request->data);
		update_s = 0;
		if (temp->request->command != NULL) 
			XtFree((char *)temp->request->command);
		if (temp->request->hide != NULL) XtFree(temp->request->hide);
		XtFree((char *)temp->request);
		XtFree((char *)temp);
	    } else {
	        code =  ftp_status(old_output);
	    }
	    if(old_output != NULL) {
		free_output(old_output);
		old_output = NULL;
	    }
	    wait = 0;
	    /*
	     * Start next command.
	     */
	    do_next();
	    return;
	}
    }
    if ((more < 0 && !blocked(errno)) ||
	more == 0) {
 	Devine_Last_Response(output, "Connection closed by peer");	
	Set_Status("Lost connection with remote host");
	Set_Reconnect(0);
	return;
    }
}

static void
start_data(closure, source, id)
XtPointer closure;
int      *source;
XtInputId *id;
{
    struct _listen *l = (struct _listen *)closure;
    struct sockaddr_in a;
    int len = sizeof(a);
    int s;
    char buff[1];
    int n;

    XtRemoveInput(l->id);
    l->id = 0;

    s = l->s;
    l->s = accept(s, (struct sockaddr *)&a, &len);
    if (l->s < 0) {
	abort_listen(l, 0);
	return;
    }
    close(s);

    NONBLOCK(l->s);

#ifdef KEEPALIVE
    {
	int on = 1;
        setsockopt(l->s, SOL_SOCKET,  SO_KEEPALIVE, (char *)&on, sizeof(int));
    }
#endif

   if (l->file == NULL) {
        buffdata.count = 0;
        temp_ss = NULL;
	l->input  = -1;
	l->convert = NULL;
        l->id = XtAppAddInput(App_context, 
			      l->s,
                              (XtPointer)XtInputReadMask,
                              (XtInputCallbackProc)read_dir_data, 
                              (XtPointer)l);
   } else  if (l->rw) {
	l->convert = NULL;
	switch (transfer_mode) {
		case MODE_T_ASCII:
		    l->convert = unix2net;
		    unix2net(NULL, -1, NULL);
		    break;
	}
	l->input  = open(l->file, O_RDONLY);
        l->id = XtAppAddInput(App_context, 
			      l->s,
                              (XtPointer)XtInputWriteMask,
                              (XtInputCallbackProc)write_data, 
                              (XtPointer)l);
    } else {
	l->convert = NULL;
	switch (transfer_mode) {
		case MODE_T_ASCII:
		    l->convert = net2unix;
		    net2unix(NULL, -1, NULL);
		    break;
	}

	l->output  = fopen(l->file, "w");
        l->id = XtAppAddInput(App_context, 
			      l->s,
                              (XtPointer)XtInputReadMask,
                              (XtInputCallbackProc)read_data, 
                              (XtPointer)l);
   }
}

static void
timout_connect(client_data, id)
caddr_t client_data;
XtIntervalId *id;
{
    time_t now;
    int t;


    if (list.start) {
	time(&now);
	t = now - list.start;
	if (t > (3*60)) {
	}
    }

    if (conn.write) {
	time(&now);
	t = now - conn.write;
	if (t > (10*60)) {
	}
    }

    if (list.chars != list.last_chars) {
	list.last_chars = list.chars;
	time(&conn.start);
    }

    if (conn.start) {
	time(&now);
	t = now - conn.start;
	if (t > (2*60)) {
            conn.timeout = 0;
	    Set_Status("Connection timed out");
	    strcpy(last_response, "Connection timed out");
	    if (connected) {
	    	Set_Reconnect(1);
	    } else {
	    	Set_Reconnect(0);
	    }
	    return;
	}
    }

    conn.timeout =  XtAppAddTimeOut(App_context,
                                    (unsigned long)(ct_out*1000),
                                    (XtTimerCallbackProc)timout_connect,
                                    (XtPointer)NULL);
}

static int 
unix2net(ia, il, oa)
char *ia;
int il;
char *oa;
{
    register char *loa = oa;

    if (ia == NULL) return(0);
    for (; il > 0; ia++, il--) switch (*ia) {
        case '\n':  *loa++ = '\r'; /* fall into... */
        default:    *loa++ = *ia; break;
    } /* for switch */
    return(loa - oa);
} /* unix2net */

static void
write_data( closure, source, id)
XtPointer closure;
int      *source;
XtInputId *id;
{
    struct _listen *l = (struct _listen *)closure;
    int m = 0;
    char *cp;

    if (l->n == 0) {
	l->cp = l->buff_out;
	if (l->convert) {	
             l->n = read(l->input, l->buff_in, sizeof(l->buff_in));
	     if (l->n > 0) 
                 l->n = l->convert(l->buff_in, l->n, l->buff_out);
	} else {
             l->n = read(l->input, l->buff_out, sizeof(l->buff_out));
	}
    }

    if (l->n <= 0) {
        conn.id = XtAppAddInput(App_context, 
			         conn.s,
                                 (XtPointer)XtInputReadMask,
                                 (XtInputCallbackProc)read_ftp, 
			         NULL);
        XtRemoveInput(l->id);
        close(l->s);
	l->s = -1;
	close(l->input);
	l->input = -1;
	l->output = NULL;
	l->finish = NULL;
	l->id = 0;
	l->n = 0;
	return;
    }
 
    m = write(l->s, l->cp, l->n);
    if (m < 0) {
	if (blocked(errno)) {
	    m = 0;
	} else {
	    Set_Status("Error writting data");
	    strcpy(last_response, "Error writeing data");
	    Set_Reconnect(0);
	    return;
	}
    }
    l->chars += m;
    l->cp += m;
    l->n = l->n - m;
}

void 
write_ftp(s, status, finish, hide, tag,  echo, data)
char      *s;
void      (*status)();
void      (*finish)();
char      *hide;
int       tag;
int       echo;
XtPointer data;
{ 
    struct _next *next, *end;
    char *cp;

    if (aborting || !connected) {
	finish(NULL, tag, data);
        return;
    }
    if (start != NULL ) {
        end = start;
        while (end->next) end = end->next;
        next = (struct _next *)XtMalloc(sizeof(struct _next));
        end->next = next;
    } else {
        start = (struct _next *)XtMalloc(sizeof(struct _next));
        next  = start;
    }
    next->next                 = NULL;

    next->request = (struct _request *)XtMalloc(sizeof(struct _request));

    next->request->hide         = NULL;
    next->request->status       = status;
    next->request->finish       = finish;
    next->request->echo         = echo;
    next->request->tag          = tag;
    next->request->command      = XtNewString(s);
    next->request->callback     = NULL;
    next->request->data         = data;
    if (hide != NULL) {
        next->request->hide = XtNewString(hide);
    } 
    do_next();
}
