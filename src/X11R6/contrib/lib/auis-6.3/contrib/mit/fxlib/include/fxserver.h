/*
 * The FX (File Exchange) Server
 *
 * $Author: susan $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/include/RCS/fxserver.h,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/include/RCS/fxserver.h,v 1.3 1993/05/17 17:09:50 susan Exp $
 *
 * Copyright 1989, 1990 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

/*
 * Definitions internal to the FX server.
 */

#ifndef _fxserver_h_

#include <fxserver_err.h>
#include <fx-internal.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <rpc/rpc.h>
#include <fx_prot.h>
#include <sys/param.h>
#ifdef KERBEROS
#include <krb.h>
#endif /* KERBEROS */

/*
 * These are the names of the various directories and files which will
 * used to locate and store files.
 */

#if defined(_IBMR2)
#define ROOT_DIR	"/usr/lpp/exchange/files"
#define BACKUP_ROOT_DIR	"/usr/lpp/exchange/files.new" /* XXX */
#else /* defined(_IBMR2) */
#if defined(ultrix)
#define ROOT_DIR	"/var/exchange/files"
#define BACKUP_ROOT_DIR	"/var/exchange/files.new" /* XXX */
#else
#define ROOT_DIR	"/site/exchange/files"
#define BACKUP_ROOT_DIR	"/site/exchange/files.new" /* XXX */
#endif /* defined(ultrix) */
#endif /* defined(_IBMR2) */

#define INDEX_FILE	"INDEX"
#define COURSE_INDEX	"COURSE_INDEX"
#define DB_VERS_FILE	"DB_VERSION"

/*
 * Structures and variables that need to be referenced by all sections
 * of the server.
 */

#define IS_SENDING 1
#define IS_RECEIVING 2

struct _Connection {
    int inited;
    int authed;
#ifdef KERBEROS
    AUTH_DAT auth;
    char authname[ANAME_SZ+INST_SZ+REALM_SZ+3];
#else
    char authname[128];
#endif
    char course[COURSE_NAME_LEN];
    char coursepath[MAXPATHLEN];
    int index;
    int sendrecv;
    Contents sendcont;
    FILE *sendrecvfp;
    int linecount;
    int server_num;
    int tokenstate;
};

extern struct _Connection Connection[NOFILE], *curconn;
extern int curconn_num;
extern server_stats stats;

/*
 * Debugging and lint aides.
 */

#ifdef DEBUG
#define Debug(x) printf x
#else
#define Debug(x)
#endif /* DEBUG */

#ifdef DEBUGDB
#define DebugDB(x) printf x
#else
#define DebugDB(x)
#endif /* DEBUGDB */

#ifdef DEBUGMULTI
#define DebugMulti(x) printf x
#else
#define DebugMulti(x)
#endif /* DEBUGMULTI */

#ifdef lint
#define TOUCH(var) var = var
#else
#define TOUCH(var)
#endif /* lint */

#ifdef MALLOC_LEAK
#define xmalloc(x) malloc(x)
#define xfree(x) free(x)
#define xrealloc(x,y) realloc(x,y)
char *malloc(), *realloc();
void free();
#endif /* MALLOC_LEAK */

/*
 * Miscellaneous definitions.
 */

#define SETUP_CURCONN curconn_num = rqstp->rq_xprt->xp_sock; \
    curconn = &Connection[curconn_num]

#define CHECK_INIT(var,str) if (!curconn->server_num && !curconn->inited) \
    { var = ERR_NOT_INITED; Debug(("ERROR CHECK_INIT: %s\n", error_message(var))); return &str; }

#define CHECK_AUTH(var,str) if (!curconn->server_num && !curconn->authed) \
    { var = ERR_NOT_AUTHED; Debug(("ERROR CHECK_AUTH: %s\n", error_message(var))); return &str; }

#define CHECK_ACCESS(x,var,str) if (!curconn->server_num && !check_access(x)) \
    { var = ERR_ACCESS_DENIED; Debug(("ERROR CHECK_ACCESS: %s\n", error_message(var))); return &str; }

#define CHECK_SENDRECV(var,str) if (curconn->sendrecv) \
    { var = ERR_SEND_RECV_PROGRESS; Debug(("ERROR CHECK_SENDRECV: %s\n", error_message(var))); return &str; }

#ifdef MULTI
#define CHECK_WRITE(var,str) if (sync_site != server_me && !curconn->server_num) \
    { var = ERR_NOT_SYNC; Debug(("ERROR CHECK_WRITE: %s\n", error_message(var))); return &str; } \
    if (update_server_npids || updating_database && updating_from_server != curconn->server_num-1) \
    { var = ERR_DATABASE_LOCKED; Debug(("ERROR CHECK_INIT: %s\n", error_message(var))); return &str; }

#else
#define CHECK_WRITE(var,str)
#endif /* MULTI */

#ifdef MULTI
#define CHECK_DB(var,str) if (!curconn->server_num && (!in_write_quorum || !database_uptodate)) \
    { var = ERR_OLD_DATABASE; return &str; }
/*#define CHECK_DB(var,str)*/
#else
#define CHECK_DB(var,str)
#endif /* MULTI */
   

/*
 * Database entry formats.
 */

#define MAKEKEY(bfr, c) sprintf((bfr), \
				"%s\001%04d\001%s\001%06ld\001%010ld\001%s", \
				(c)->p.author, (c)->p.assignment, \
				(c)->p.filename, \
				(c)->p.location.time.tv_sec, \
				(c)->p.location.time.tv_usec, \
				(c)->p.location.host)

#define MAKEDATA(bfr, c) sprintf((bfr), \
				 "%d\001%s\001%s\001%ld\001%ld\001%ld\001%ld\001%d\001%d\001%d\001%d\001%s", \
				 (c)->p.type, (c)->p.owner, \
				 (c)->p.desc, \
				 (c)->p.created.tv_sec, \
				 (c)->p.created.tv_usec, \
				 (c)->p.modified.tv_sec, \
				 (c)->p.modified.tv_usec, \
				 (c)->p.size, (c)->p.words, (c)->p.lines, \
				 (c)->p.flags, (c)->ptrfile)

/*
 * Definitions for multi server support.
 */

#ifdef MULTI

#define PINGINTERVAL 30 /* how often to check the ping list */
#define SERVER_RETRIES 3
#define SERVER_TIME_BIG 300
#define SERVER_TIME_SMALL 120

struct _servers {
    char *name;
    u_long inet_addr;
    CLIENT *cl;
    int sockfd;
    int maybe_up;
    int retries;
    int vote_weight;
    DBVers vers;
};

extern int in_write_quorum, sync_site, server_me;
extern int database_uptodate, nservers;
extern int updating_database, updating_from_server;
extern int update_server_npids;
extern struct _servers *servers;

#define UPDATING_EXTENSION ((updating_database && updating_from_server == curconn->server_num-1) ? "_new" : "")

#else

#define UPDATING_EXTENSION ""

#endif /* MULTI */

/*
 * "errno" variable.
 */

extern int errno;

/*
 * Random internal definitions.
 */

extern char my_hostname[256], my_canonhostname[256];
char *xmalloc(), *xsave_string();
Contents *db_firstcontents(), *db_nextcontents(),
  *db_firstkey(), *db_nextkey(), *db_fullcontents();
extern DBVers db_vers;
extern char *root_dir;

#endif /* _fxserver_h_ */
