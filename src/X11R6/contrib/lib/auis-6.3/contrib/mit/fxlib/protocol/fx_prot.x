/*
 * The FX (File Exchange) Library and Server
 *
 * $Author: wdc $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/protocol/RCS/fx_prot.x,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/protocol/RCS/fx_prot.x,v 1.1 1992/02/13 20:32:47 wdc R6tape $
 *
 * Copyright 1989, 1990 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

/*
 * This file contains the FX protocol definition in RPCGEN format.
 */

%#include <mit-copyright.h>
#include <mit-copyright.h>
%#include <krb.h>
%#include <fxserver_err.h>
%#include <sys/time.h>

/*
 * String length restrictions - always keep these in sync.
 */

%#define PTRFILELEN 64
#define PTRFILELEN 64
    
%#define COURSE_NAME_LEN 32
#define COURSE_NAME_LEN 32

%#define MAX_BURST_SIZE 10240
#define MAX_BURST_SIZE 10240

/*
 * Definition of a "Paper", the structure used to describe a Paper's
 * attributes.
 */

enum PaperType { TYPE_WILDCARD, TURNEDIN, TAKEN, GRADED, PICKEDUP, HANDOUT,
		     EXCHANGE, TEACHERS_ARCHIVE, TEACHERS_HANDOUT };

%#define ASSIGNMENT_WILDCARD 0
%#define AUTHOR_WILDCARD "*"
%#define OWNER_WILDCARD "*"
%#define FILENAME_WILDCARD "*"
%#define ID_WILDCARD "*"
%#define PAPER_EXECUTABLE 1

/*
 * Define a thing very similar to the UNIX timeval that exists on most
 * systems for our own time value exchange in our protocol.
 */

struct fx_timeval {
    long tv_sec;
    long tv_usec;
};

struct PaperID {
    string host<>;
    struct fx_timeval time;
};
	
struct Paper {
    PaperID location;
    PaperType type;
    int assignment;
    string author<>;
    string owner<>;
    string filename<>;
    string desc<>;
    struct fx_timeval created;
    struct fx_timeval modified;
    int size;
    int words;
    int lines;
    int flags;
};

struct Contents {
    Paper p;
    char ptrfile[PTRFILELEN];
};

/*
 * Aggregate arguments for various calls.
 */

struct init_data {
    string course<>;
/* #ifdef KERBEROS */
    KTEXT_ST auth;
/* #else
    string auth<>;
#endif */ /* KERBEROS */
};

struct acl_maint {
    string aclname<>;
    string aclparam<>;
};

struct burst_data {
    int size;
    opaque data[MAX_BURST_SIZE];
};

struct TwoPaper {
    Paper src;
    Paper dest;
};

struct portionspec {
    Paper p;
    int start;
    int end;
};

enum SyncStat { NO_SYNC, WANT_SYNC, AM_SYNC };

struct quorumstat {
    SyncStat sync;
};

struct DBVers {
    long synctime;
    long commit;
};

/*
 * Random data definitions for use in aggregate return values.
 */

union init_res switch (long errno) {
case ERR_NOT_SYNC:
    string sync<>;
default:
    void;
};

/*
 * A "stringlist" is a linked list of "string"s.
 */

typedef struct stringnode *stringlist;
struct stringnode {
    string s<>;
    stringlist next;
};

union stringlist_res switch (long errno) {
case 0:
    stringlist list;
default:
    void;
};     
    
/*
 * A "Paperlist" is a linked list of "Paper"s.
 */

typedef struct PaperNode *Paperlist;
struct PaperNode {
    Paper p;
    Paperlist next;
};

union Paperlist_res switch (long errno) {
case 0:
    Paperlist list;
default:
    void;
};

/*
 * A "retrieve_res" is a data burst from a RETRIEVE_BURST call.
 */

union retrieve_res switch (long errno) {
case 0:
    burst_data burst;
default:
    void;
};

/*
 * A "krb_info_res" is a set of Kerberos information.
 */

struct krb_info {
    string service<>;
    string instance<>;
    string realm<>;
};

union krb_info_res switch (long errno) {
case 0:
    krb_info info;
default:
    void;
};

/*
 * A quorum_res is returned from by server_quorum.
 */

struct quorum_res {
    int vote;
    DBVers vers;
};

/*
 * A server_stats is a set of server statistics.
 */

struct server_stats {
    long start_time;
    long uptime;
    int num_requests;
    int bytes_sent;
    int bytes_recv;
    int n_init;
    int n_list_acl;
    int n_add_acl;
    int n_delete_acl;
    int n_create_course;
    int n_delete_course;
    int n_list_courses;
    int n_list;
    int n_send_file;
    int n_send_burst;
    int n_end_send;
    int n_retrieve_file;
    int n_retrieve_burst;
    int n_copy;
    int n_move;
    int n_delete;
    int n_portion;
    int n_krb_info;
    int n_kill_server;
    int n_server_stats;
    int n_server_quorum;
    int n_server_store;
    int n_server_delete;
    int n_server_commit;
    int n_server_end_course;
    int n_server_requpdate;
    int n_server_start_upd;
    int n_server_end_upd;
    DBVers vers;
};

/*
 * The actual protocol!
 */

program FXSERVER {
    version FXVERS {
	/* RPC call 0 is NOOP by default */
	init_res INIT(init_data) = 1;		/* Init connection */
	stringlist_res LIST_ACL(string) = 2;	/* Return ACL */
	long ADD_ACL(acl_maint) = 3;		/* Add to ACL */
	long DELETE_ACL(acl_maint) = 4;		/* Delete from ACL */
	long CREATE_COURSE(string) = 10;	/* Create course */
	long DELETE_COURSE(string) = 11;	/* Delete course */
	stringlist_res LIST_COURSES(int) = 12;	/* List courses */
	Paperlist_res LIST(Paper) = 15;		/* Return matches */
	long SEND_FILE(Paper) = 16;		/* Send file */
	long SEND_BURST(burst_data) = 17;	/* Send a burst to the file */
	long END_SEND(int) = 18;			/* End the file */
	long RETRIEVE_FILE(Paper) = 19;		/* Retrieve file */
	retrieve_res RETRIEVE_BURST(int) = 20;	/* Retrieve a burst */
	long COPY(TwoPaper) = 21;		/* Copy a paper */
	long MOVE(TwoPaper) = 22;		/* Move a paper */
	long DELETE(Paper) = 23;			/* Delete a paper */
	long PORTION(portionspec) = 24;		/* Retrieve portion */
	krb_info_res KRB_INFO(int) = 25;	/* Get Kerberos info */
	long KILL_SERVER(int) = 30;		/* Kill server */
        server_stats SERVER_STATS(int) = 31;	/* Get server stats */

	quorum_res SERVER_QUORUM(quorumstat) = 50;	/* Talk to a server */
	long SERVER_STORE(Contents) = 51;	/* Store in DB */
	long SERVER_DELETE(Contents) = 52;	/* Delete from DB */
	long SERVER_COMMIT(DBVers) = 53;	/* Commit a change */
	long SERVER_END_COURSE(int) = 54;	/* Finished with a course */
	long SERVER_REQUPDATE(int) = 55;	/* Request an update */
	long SERVER_START_UPD(int) = 56;	/* Start an update */
	long SERVER_END_UPD(DBVers) = 57;	/* End an update */
    } = 1;
} = 100;
