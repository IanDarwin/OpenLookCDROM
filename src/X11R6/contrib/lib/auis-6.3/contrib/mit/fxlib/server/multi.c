/*
 * The FX (File Exchange) Server
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/multi.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/multi.c,v 1.3 1992/12/15 21:55:23 rr2b R6tape $
 *
 * Copyright 1989, 1990 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/multi.c,v 1.3 1992/12/15 21:55:23 rr2b R6tape $";
#endif

#include <mit-copyright.h>

/*
 * This file contains routines used to support multiple, synchronized
 * servers.  Some of this code was borrowed (in sprit) from the CMU
 * Ubik database system, although this code is much simpler.
 */

 

#include <fxserver.h>

#ifdef MULTI

#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <com_err.h>

#ifdef HESIOD
#include <hesiod.h>
#endif /* HESIOD */

struct _servers *servers;
int nservers = 0;

/*
 * Server state variables
 */

int sync_site = -1;
u_long sync_site_until = 0;
u_long sync_site_since = 0;

int in_write_quorum = 0;

u_long time_last_voted = 0;
int who_last_voted_for = -1;

int updating_database = 0, updating_from_server = -1;
int database_uptodate = 0;

int update_server_npids = 0;
int update_server_pids[64]; /* XXX */

int server_wentdown = 0;

int server_me;

DBVers new_db_vers;

static struct timeval TIMEOUT = { 25, 0 };

/*
 * Initialize multiple server support.  This includes finding the
 * names and addresses of the other servers.  Mark each server as
 * initially being up (but not heard from in about 20 years) until we
 * decide otherwise.
 */

multi_init()
{
  int i, weight;
#ifdef HESIOD
  char **hesres, **hesptr;
  struct hostent *hent;

  DebugMulti(("multi_init:\n"));
  
  servers = NULL;
  hesres = hes_resolve(HES_NAME, HES_TYPE);
  if (!hesres || !*hesres) {
    DebugMulti(("  No servers found\n"));
    return;
  }
  for (hesptr = hesres; *hesptr; hesptr++) {
    hent = gethostbyname(*hesptr);
    if (!hent)
      fatal("Can't resolve host %s!\n", *hesptr);
#else
    FILE *fp;
    char bfr[MAXHOSTNAMELEN];
    struct hostent *hent;

    fp = fopen(SERVER_LIST_FILE, "r");
    if (!fp) {
      servers = NULL;
      return;
    }
    DebugMulti(("My name is %s\n", my_canonhostname));
    while (fgets(bfr, sizeof bfr, fp) != NULL) {
      bfr[strlen(bfr)-1] = '\0';
      hent = gethostbyname(bfr);
      if (!hent)
	fatal("Can't resolve host %s!\n", bfr);
#endif /* HESIOD */	
      if (nservers)
	servers = (struct _servers *)xrealloc(servers,
					      (nservers+1)*
					      sizeof(struct _servers));
      else
	servers = (struct _servers *)xmalloc(sizeof(struct _servers));
      bzero((char*)&servers[nservers], sizeof(struct _servers));
      servers[nservers].name = xsave_string(hent->h_name);
      servers[nservers].inet_addr = *(long *)hent->h_addr_list[0];
      servers[nservers].cl = (CLIENT*)NULL;
      servers[nservers].maybe_up = 1;

      /* "Canonical" hostnames are case-insensitive.
       * I have seen gethostbyname() return OSAGE.MIT.EDU
       * and osage.MIT.EDU in consecutive calls from the
       * same process.  Thus the use of strcasecmp below:
       */
       
      if (!strcasecmp(hent->h_name, my_canonhostname)) {
	server_me = nservers;
      }
      servers[nservers].vote_weight = 2;
      DebugMulti(("Server %s at %s\n", hent->h_name,
		  inet_ntoa(servers[nservers].inet_addr)));
      nservers++;
#ifdef HESIOD
    }
#else
  }
#endif /* HESIOD */

  if (nservers > 0)
    servers[0].vote_weight = 3;
}

/*
 * Try to connect to any server that we think is up and that we don't
 * have a connection to.  We don't need to try any servers that we
 * think are down because if they come up they'll try to contact us.
 * This means that we never have to try to contact another server
 * unless we knew it was up and we think it's gone down.
 */

multi_ping_servers()
{
  int i;
  quorum_res *res;
  quorumstat qs;
  long curtime;

  DebugMulti(("multi_ping_servers: at %d\n", time(0)));

  /*
   * While some other server is updating our database, don't do any
   * ping-type stuff.
   */
  if (updating_database)
    return;

  /*
   * If the sync site hasn't reasserted itself recently, punt it.
   */
  if (time_last_voted+SERVER_TIME_SMALL < time(0) && sync_site != -1 &&
      who_last_voted_for == sync_site) {
    DebugMulti(("Sync site %s being punted\n", servers[sync_site].name));
    sync_site = -1;
  }

  /*
   * If there's no sync site, should we periodically try all the
   * hosts again? XXX
   */
  
  for (i=0; i<nservers; i++) {
    /* We know we're up - don't ping ourselves */
    if (i == server_me)
      continue;
    if (servers[i].maybe_up && !servers[i].cl) {
      multi_open_connection(i);
      if (!servers[i].cl) { /* Must be down! */
	servers[i].retries++;
	if (servers[i].retries > SERVER_RETRIES) {
	  servers[i].retries = 0;
	  servers[i].maybe_up = 0;
	  DebugMulti(("Server %s is down!\n", servers[i].name));
	}
	else
	  DebugMulti(("Server %s may be down!\n", servers[i].name));
	server_wentdown = 1;
      }
      else {
	log_warning("Server %s back up", servers[i].name);
	DebugMulti(("Server %s is up!\n", servers[i].name));
      }
    }
  }
  /*
   * If we may no longer be the sync site in a little while, ask for
   * reconfirmation.  If we aren't the sync site or a server went
   * down, recompute the quorum and maybe try to be the new sync site.
   */
  if ((sync_site == server_me &&
       time(0)+SERVER_TIME_SMALL/2 > sync_site_until) ||
      sync_site != server_me || server_wentdown)
    multi_ask_votes();

  server_wentdown = 0;
  
#ifdef DEBUGMULTI
  DebugMulti(("----\nServer statistics: (%d)\n", time(0)));
  if (sync_site == server_me)
    DebugMulti(("I am the sync site!\n"));
  else
    if (sync_site == -1)
      DebugMulti(("No sync site!\n"));
    else
      DebugMulti(("Sync site is %s\n", servers[sync_site].name));
  if (in_write_quorum)
    DebugMulti(("I'm in a quorum!\n"));
  if (updating_database)
    DebugMulti(("My database is being updated!\n"));
  for (i=0; i<nservers; i++)
    DebugMulti(("Server %s, up %d, retries %d, connection %s\n",
		servers[i].name, servers[i].maybe_up, servers[i].retries,
		servers[i].cl ? "open" : "none"));
#endif /* DEBUGMULTI */
}

/*
 * Try to open a connection to a server.
 */

multi_open_connection(num)
     int num;
{
  struct sockaddr_in sin;
  quorum_res *res;
  quorumstat qs;
  int sockdummy;
  
  DebugMulti(("Opening connection to server %s\n", servers[num].name));

  qs.sync = (sync_site == server_me) ? AM_SYNC : NO_SYNC;
  
  sin.sin_family = AF_INET;
  sin.sin_port = 0;
  sin.sin_addr.s_addr = servers[num].inet_addr;
  servers[num].sockfd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (servers[num].sockfd < 0)
    fatal("Unable to allocate socket!\n");
  sockdummy = 1;
  if (setsockopt(servers[num].sockfd, SOL_SOCKET, SO_KEEPALIVE,
		 &sockdummy, sizeof(int)) < 0)
    fatal("Unable to set SO_KEEPALIVE on socket\n");
  servers[num].cl = clnttcp_create(&sin, FXSERVER, FXVERS,
				   &servers[num].sockfd, 0, 0);
  if (servers[num].cl) {
    (void)bindresvport(servers[num].sockfd, (struct sockaddr_in *)0);
    if (connect(servers[num].sockfd, (struct sockaddr *)&sin,
		sizeof(struct sockaddr_in)) < 0) {
      clnt_destroy(servers[num].cl);
      servers[num].cl = (CLIENT*)NULL;
    }
    else {
      clnt_control(servers[num].cl, CLSET_TIMEOUT, &TIMEOUT);
      res = (quorum_res*)_server_quorum_1(&qs, servers[num].cl);
      if (!res) {
	clnt_destroy(servers[num].cl);
	servers[num].cl = (CLIENT*)NULL;
      }
    }
  }
  if (!servers[num].cl)
    close(servers[num].sockfd);
}

/*
 * Recompute the quorum.  If we're in a quorum, perhaps we want to be
 * the sync site.  If we're already the sync site, reassert that we're
 * the sync site.
 */

multi_ask_votes()
{
  quorumstat qs;
  int i, dummy, uptodate_server;
  int avail_votes, possible_votes;
  quorum_res *res;
  long *commitres;
  DBVers best_db, vers;
  
  /*
   * First check to see if we are talking to enough hosts to make a quorum.
   * avail_votes will always be _odd_, possible_votes may be odd or even.
   */

  avail_votes = possible_votes = 0;
  for (i=0; i<nservers; i++) {
    avail_votes += servers[i].vote_weight;
    if (servers[i].cl || i == server_me)
      possible_votes += servers[i].vote_weight;
  }

  if (possible_votes*2 < avail_votes) {
    /*
     * We're not in a quorum...oh well.
     */
    in_write_quorum = 0;
    sync_site = -1;
    database_uptodate = 0;
    DebugMulti(("Not in write quorum: %d possible out of %d\n",
		possible_votes, avail_votes));
    return;
  }

  /*
   * Well, we're in a write quorum -- this is a good sign.
   */
  in_write_quorum = 1;
  
  /*
   * We're in contact with enough sites that we could be a sync site
   * if we wanted to.  Do we have a chance?
   */
  for (i=0; i<nservers; i++)
    if (i == server_me || servers[i].maybe_up)
      break;
  if (i != server_me) {
    /*
     * We don't want to be the sync site.
     */
    DebugMulti(("I don't want to be the new sync site - %s may be up\n",
		servers[i].name));
    return;
  }
  /*
   * We want to be the new sync site! Ask for votes from everyone.
   */
  DebugMulti(("Asking for votes...\n"));
  for (i=0; i<nservers; i++) {
    if (servers[i].cl) {
      qs.sync = WANT_SYNC;
      res = (quorum_res*)_server_quorum_1(&qs, servers[i].cl);
      if (!res) {
	/*
	 * Oops! We got an RPC error - kill the connection and
	 * start everything over again.  This is very paranoid.
	 */
	DebugMulti(("Server %s might have gone down.  Aborting.\n",
		    servers[i].name));
	multi_conn_dropped(i);
	return;
      }
      if (!res->vote) {
	/*
	 * Someone didn't vote for us.  Oh well, maybe next time.
	 */
	DebugMulti(("Server %s didn't vote for me.  Aborting.\n",
		    servers[i].name));
	return;
      }
    }
  }
  /*
   * Everyone must have voted for us!  We're the new sync site!
   * Tell them all, and get db version numbers.
   */
  uptodate_server = -1;
  best_db.synctime = -1;
  best_db.commit = -1;
  for (i=0; i<nservers; i++) {
    if (servers[i].cl) {
      qs.sync = AM_SYNC;
      res = (quorum_res*)_server_quorum_1(&qs, servers[i].cl);
      if (!res) {
	/*
	 * Sigh...someone went down in the middle of this.
	 * Abort everything...this will cause inconsistencies for
	 * the next few minutes, but not forever.
	 */
	multi_conn_dropped(i);
	return;
      }
      DebugMulti(("Server %s synctime %ld commit %ld\n",
		  servers[i].name, res->vers.synctime,
		  res->vers.commit));
      servers[i].vers = res->vers;
      if (res->vers.synctime > best_db.synctime ||
	  (res->vers.synctime == best_db.synctime &&
	   res->vers.commit > best_db.commit)) {
	best_db = res->vers;
	uptodate_server = i;
      }
    }
  }
  /*
   * We take priority if we have a good version so we don't have to
   * update ourselves.
   */
  if (db_vers.synctime > best_db.synctime ||
      (db_vers.synctime == best_db.synctime &&
       db_vers.commit >= best_db.commit)) {
    best_db = db_vers;
    uptodate_server = server_me;
  }

  /* Only one server? Always up to date... */
  if (nservers != 1 && !best_db.synctime && !best_db.commit) {
    DebugMulti(("Nobody has a valid database!\n"));
    database_uptodate = 0;
    return;
  }
  
  /*
   * Update ourselves to the most up-to-date version.
   */
  if (uptodate_server != server_me) {
    DebugMulti(("Updating database from %s\n",
		servers[uptodate_server].name));
    log_info("Requesting database update from %s",
	     servers[uptodate_server].name);
    commitres = (long*)_server_requpdate_1(&dummy,
					   servers[uptodate_server].cl);
    return;
  }
  DebugMulti(("I have the most up-to-date database!\n"));
  database_uptodate = 1;
  
  multi_update_everyone();
}

multi_update_everyone()
{
  DBVers vers;
  int i;
  long *commitres;

  /*
   * Update everybody to the most up-to-date version, which is the
   * one I currently have.
   */
  if (sync_site != server_me) {
    new_db_vers.synctime = time(0);
    new_db_vers.commit = 0;
  }
  else
    new_db_vers = db_vers;
  db_set_vers();
  for (i=0; i<nservers; i++)
    if (servers[i].cl && (db_vers.synctime != servers[i].vers.synctime ||
			  db_vers.commit != servers[i].vers.commit))
      multi_update_server(i);
    else
      if (servers[i].cl) {
	commitres = (long*)_server_end_upd_1(&db_vers, servers[i].cl);
	if (!commitres)
	  multi_conn_dropped(i);
      }

  db_vers = new_db_vers;
  sync_site = server_me;
  sync_site_until = time(0)+SERVER_TIME_SMALL;
  DebugMulti(("I'm now the sync site!  Until %d\n", sync_site_until));
}

/*
 * Update a server to the current database.
 */

char *multi_acls[] = { ACL_TURNIN, ACL_GRADER, ACL_MAINT, 0 };

multi_update_server(num)
     int num;
{
  char fnbuf[1024], coursename[1024], aclbuf[1024], *ptr;
  init_data init;
  acl_maint acl_params;
  init_res *init_r;
  int i, dbidx, pid, dummy;
  long *res;
  FILE *fpcourse, *fpacl;
  Contents *cont;
  struct _Connection connfoo; /* XXX */

  if (!(pid = fork())) {
    sprintf(fnbuf, "%s/%s", root_dir, COURSE_INDEX);

    fpcourse = fopen(fnbuf, "r");
    if (!fpcourse)
      fatal("Can't open course file!\n");

    if (!servers[num].cl) {
      multi_open_connection(num);
      if (!servers[num].cl)
	exit(1);
    }

    res = (long*)_server_start_upd_1(&dummy, servers[num].cl);
    if (!res || *res) {
      DebugMulti(("ERROR starting %s update: %s\n",
		  servers[num].name, res ? error_message(*res)
		  : "RPC error"));
      exit(1);
    }
    while (fgets(coursename, sizeof(coursename), fpcourse)) {
      coursename[strlen(coursename)-1] = '\0';
      DebugMulti(("Updating server %s, course %s\n", servers[num].name,
		  coursename));
      init.course = coursename;
      bzero(&init.auth, sizeof(init.auth));
      init_r = (init_res*)_init_1(&init, servers[num].cl);
      if (!init_r) {
	exit(1);
      }
      if (init_r->errno == ERR_COURSE_NOT_FOUND) {
	ptr = coursename;
	res = (long*)_create_course_1(&ptr, servers[num].cl);
	if (!res || *res) {
	  if (res)
	    DebugMulti(("ERROR creating %s on %s: %s\n",
			ptr, servers[num].name,
			error_message(*res)));
	  exit(1);
	}
	init_r = (init_res*)_init_1(&init, servers[num].cl);
	if (!init_r || init_r->errno) {
	  DebugMulti(("ERROR initializing %s on %s: %s\n",
		      ptr, servers[num].name, init_r ?
		      error_message(*res) : "RPC error"));
	  exit(1);
	}
      }		
      for (i=0; multi_acls[i]; i++) {
	sprintf(fnbuf, "%s/%s/ACL-%s", root_dir, coursename,
		multi_acls[i]);
	fpacl = fopen(fnbuf, "r");
	if (!fpacl)
	  exit(1);
	while (fgets(aclbuf, sizeof(aclbuf), fpacl)) {
	  aclbuf[strlen(aclbuf)-1] = '\0';
	  acl_params.aclname = multi_acls[i];
	  acl_params.aclparam = aclbuf;
	  res = (long*)_add_acl_1(&acl_params, servers[num].cl);
	  if (!res /* || *res */) { /* XXX Bleh - bug in acl */
	    exit(1);
	  }
	}
	fclose(fpacl);
      }
      sprintf(fnbuf, "%s/%s/%s", root_dir, coursename, INDEX_FILE);
      curconn = &connfoo;
      connfoo.index = dbidx = db_open(fnbuf); /* XXX */
      for (cont = db_firstcontents(); cont; cont = db_nextcontents()) {
	res = (long*)_server_store_1(cont, servers[num].cl);
	if (!res || *res) {
	  if (res)
	    DebugMulti(("Error %ld\n", *res));
	  exit(1);
	}
      }
      db_close(dbidx);
      res = (long*)_server_end_course_1(&dummy, servers[num].cl);
      if (!res || *res) {
	if (res)
	  DebugMulti(("Error %ld\n", *res));
	exit(1);
      }
    }
    fclose(fpcourse);
    res = (long*)_server_end_upd_1(&new_db_vers, servers[num].cl);
    if (!res || *res)
      exit(1);
    exit(0);
  }
  if (pid == -1)
    fatal("Can't fork!");
  DebugMulti(("Child pid %d created!\n", pid));
  update_server_pids[update_server_npids++] = pid;
}

/*
 * Find the server making this request.
 */

multi_find_server(rqstp)
     struct svc_req *rqstp;
{
  int i;

  for (i=0; i<nservers; i++)
    if (servers[i].inet_addr ==
	svc_getcaller(rqstp->rq_xprt)->sin_addr.s_addr)
      return i;
  return -1;
}

/*
 * Server connection was dropped.
 */

multi_conn_dropped(snum)
     int snum;
{
  log_warning("Lost connection to server %s", servers[snum].name);
  
  DebugMulti(("Server %s dropped connection!\n", servers[snum].name));
  if (servers[snum].cl) {
    clnt_destroy(servers[snum].cl);
    close(servers[snum].sockfd);
  }
  servers[snum].cl = (CLIENT*)NULL;
  servers[snum].retries = 0;
  servers[snum].maybe_up = 1;
  server_wentdown = 1;

  if (snum == updating_from_server) {
    updating_database = 0; /* XXX */
    DebugMulti(("Updating server has gone down!\n"));
  }
}

/*
 * Initialize the current course on all servers in this quorum.
 */

multi_set_course()
{
  int i;
  init_data params;
  init_res *res;

  params.course = curconn->course;
  bzero(&params.auth, sizeof(params.auth));
  for (i=0; i<nservers; i++) {
    if (servers[i].cl) {
      res = (init_res *)_init_1(&params, servers[i].cl);
      if (!res || res->errno)
	multi_conn_dropped(i);
    }
  }
}

/*
 * Send a database commit to the workstations in this quorum.
 */

multi_commit()
{
  int i;
  init_res *res;

  db_inc_vers();
  for (i=0; i<nservers; i++) {
    if (servers[i].cl) {
      res = (init_res *)_server_commit_1(&db_vers, servers[i].cl);
      if (!res || res->errno) /* XXX */
	multi_conn_dropped(i);
    }
  }
}

/*
 * Another server is talking to us.  Update our information about that
 * server, etc.
 */

quorum_res *server_quorum_1(quorum, rqstp)
     quorumstat *quorum;
     struct svc_req *rqstp;
{
  int snum;
  static quorum_res ret;

  stats.n_server_quorum++;
  
  SETUP_CURCONN;

  ret.vers = db_vers;
  snum = multi_find_server(rqstp);
  if (snum == -1) {
    DebugMulti(("Pinged by a non-server!\n"));
    ret.vote = 0;
    return &ret;
  } 
  DebugMulti(("server_quorum_1: from %s, qs %d\n", servers[snum].name,
	      quorum->sync));
  curconn->server_num = snum+1;
  servers[snum].maybe_up = 1;
  servers[snum].retries = 0;
  if (quorum->sync == NO_SYNC) {
    ret.vote = 0;
    return &ret;
  }
  if (quorum->sync == AM_SYNC) { /* XXX */
    sync_site = snum;
    ret.vote = 1;
    return &ret;
  }
  /*
   * If we've voted for someone else in the last SERVER_TIME_BIG
   * seconds, we can't vote again.
   */
  if (who_last_voted_for != snum &&
      time_last_voted + SERVER_TIME_BIG > time(0)) {
    DebugMulti(("Not voting for %s\n", servers[snum].name));
    ret.vote = 0;
    return &ret;
  }
  who_last_voted_for = snum;
  time_last_voted = time(0);
  DebugMulti(("Voting for %s at time %d\n", servers[snum].name,
	      time_last_voted));
  ret.vote = 1;
  return &ret;
}

long *server_store_1(contents, rqstp)
     Contents *contents;
     struct svc_req *rqstp;
{
  static long res;

  stats.n_server_store++;

  Debug(("server_store_1:\n"));
  
  SETUP_CURCONN;
  CHECK_INIT(res, res);
  CHECK_DB(res, res);

  res = 0L;
  if (!curconn->server_num) {
    res = ERR_ACCESS_DENIED;
    return &res;
  }

  if (db_store(&curconn->sendcont)) res = ERR_INTERNAL_FAILURE;
  return &res;
}

long *server_delete_1(contents, rqstp)
     Contents *contents;
     struct svc_req *rqstp;
{
  static long res;

  stats.n_server_delete++;

  Debug(("server_delete_1:\n"));
  
  SETUP_CURCONN;
  CHECK_INIT(res, res);
  CHECK_DB(res, res);

  if (!curconn->server_num) {
    res = ERR_ACCESS_DENIED;
    return &res;
  }

  db_delete(contents);
  res = 0;
  return &res;
}

long *server_commit_1(vers, rqstp)
     DBVers *vers;
     struct svc_req *rqstp;
{
  static long res;

  stats.n_server_commit++;

  Debug(("server_commit_1: synctime %ld, commit %ld\n", vers->synctime,
	 vers->commit));
  
  SETUP_CURCONN;
  CHECK_INIT(res, res);
  CHECK_DB(res, res);

  if (!curconn->server_num) {
    res = ERR_ACCESS_DENIED;
    return &res;
  }

  db_vers = *vers;

  db_set_vers();

  res = 0;
  return &res;
}

long *server_end_course_1(param, rqstp)
     int param;
     struct svc_req *rqstp;
{
  static long res;
  char bfr1[1024], bfr2[1024]; /* XXX */

  stats.n_server_end_course++;

  Debug(("server_end_course_1:\n"));

  SETUP_CURCONN;
  CHECK_INIT(res, res);

  if (!curconn->server_num || !updating_database) {
    res = ERR_ACCESS_DENIED;
    return &res;
  }

  sprintf(bfr1, "%s%s_new.dir", curconn->coursepath, INDEX_FILE);
  sprintf(bfr2, "%s%s.dir", curconn->coursepath, INDEX_FILE);
  unlink(bfr2);
  rename(bfr1, bfr2);
  sprintf(bfr1, "%s%s_new.pag", curconn->coursepath, INDEX_FILE);
  sprintf(bfr2, "%s%s.pag", curconn->coursepath, INDEX_FILE);
  unlink(bfr2);
  rename(bfr1, bfr2);
  sprintf(bfr1, "%sACL-%s_new", curconn->coursepath, ACL_TURNIN);
  sprintf(bfr2, "%sACL-%s", curconn->coursepath, ACL_TURNIN);
  unlink(bfr2);
  rename(bfr1, bfr2);
  sprintf(bfr1, "%sACL-%s_new", curconn->coursepath, ACL_GRADER);
  sprintf(bfr2, "%sACL-%s", curconn->coursepath, ACL_GRADER);
  unlink(bfr2);
  rename(bfr1, bfr2);
  sprintf(bfr1, "%sACL-%s_new", curconn->coursepath, ACL_MAINT);
  sprintf(bfr2, "%sACL-%s", curconn->coursepath, ACL_MAINT);
  unlink(bfr2);
  rename(bfr1, bfr2);

  res = 0;
  return &res;
}

long *server_requpdate_1(dummy, rqstp)
     int *dummy;
     struct svc_req *rqstp;
{
  static long res;

  stats.n_server_requpdate++;

  Debug(("server_requpdate_1:\n"));
  
  SETUP_CURCONN;

  if (!curconn->server_num) {
    res = ERR_ACCESS_DENIED;
    return &res;
  }

  new_db_vers = db_vers;
  
  multi_update_server(curconn->server_num-1);
  
  res = 0;
  return &res;
}

long *server_start_upd_1(dummy, rqstp)
     int *dummy;
     struct svc_req *rqstp;
{
  static long res;
  char cmdbfr[1024];
  
  stats.n_server_start_upd++;

  DebugMulti(("server_start_upd_1:\n"));
  
  SETUP_CURCONN;

  if (!curconn->server_num) {
    res = ERR_ACCESS_DENIED;
    return &res;
  }

  log_info("Receiving database update from %s",
	   servers[curconn->server_num-1].name);

  if (updating_database) {
    res = ERR_ALREADY_UPDATING;
    return &res;
  }

  updating_database = 1;
  updating_from_server = curconn->server_num-1;

  /*
   * Make sure that if the update dies in the middle, we'll have the
   * worst database possible.
   */

  db_vers.synctime = 0;
  db_vers.commit = 0;
  db_set_vers();
  
  res = 0;
  return &res;
}

long *server_end_upd_1(vers, rqstp)
     DBVers *vers;
     struct svc_req *rqstp;
{
  static long res;
  char cmdbfr[1024];
  Contents *cont;
  
  stats.n_server_end_upd++;

  DebugMulti(("server_end_upd_1: %d/%d\n", vers->synctime, vers->commit));
  
  SETUP_CURCONN;

  if (!curconn->server_num) {
    res = ERR_ACCESS_DENIED;
    return &res;
  }

  if (updating_database)
    log_info("Finished update from %s",
	     servers[curconn->server_num-1].name);
  
  db_vers = *vers;
  db_set_vers();

  updating_database = 0;
  database_uptodate = 1;
  
  res = 0;
  return &res;
}

#else

quorum_res *server_quorum_1(quorum, rqstp)
     quorumstat *quorum;
     struct svc_req *rqstp;
{
  static quorum_res ret;

  stats.n_server_quorum++;

  Debug(("server_quorum_1: not supported\n"));
  
  ret.vers.synctime = -1;
  ret.vers.commit = -1;
  ret.vote = -1;
  return &ret;
}

long *server_store_1(contents, rqstp)
     Contents *contents;
     struct svc_req *rqstp;
{
  static long res;

  stats.n_server_store++;

  Debug(("server_store_1: not supported\n"));
  
  res = ERR_ACCESS_DENIED;
  return &res;
}

long *server_delete_1(contents, rqstp)
     Contents *contents;
     struct svc_req *rqstp;
{
  static long res;

  stats.n_server_delete++;

  Debug(("server_delete_1: not supported\n"));
  
  res = ERR_ACCESS_DENIED;
  return &res;
}

long *server_commit_1(contents, rqstp)
     Contents *contents;
     struct svc_req *rqstp;
{
  static long res;

  stats.n_server_commit++;

  Debug(("server_commit_1: not supported\n"));
  
  res = ERR_ACCESS_DENIED;
  return &res;
}

#endif /* MULTI */
