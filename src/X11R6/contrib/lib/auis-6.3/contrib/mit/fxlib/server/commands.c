/*
 * The FX (File Exchange) Server
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/commands.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/commands.c,v 1.3 1992/12/15 21:55:23 rr2b R6tape $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/commands.c,v 1.3 1992/12/15 21:55:23 rr2b R6tape $";
#endif

#include <mit-copyright.h>

/*
 * This file contains routines used to execute client commands.
 */

 

#include <com_err.h>
#include <fxserver.h>
#include <sys/file.h>

static long rpc_ret_int = 0;

/*
 * Initialize a session.  This includes making sure the course exists, etc.
 */

init_res *init_1(params, rqstp)
     init_data *params;
     struct svc_req *rqstp;
{
  char indexpath[MAXPATHLEN];
  static init_res res;

  Debug(("init_1: %s\n", params->course));

  stats.n_init++;
  
  SETUP_CURCONN;
  CHECK_SENDRECV(res.errno, res);
  
  /*
   * NULL course means that we're just authenticating, not
   * initializing a course.
   */
  if (*params->course) {
    /*
     * Check the arguments for validity.
     */
    if (strlen(params->course) > COURSE_NAME_LEN) {
      res.errno = ERR_COURSE_NAME_LEN;
      Debug(("ERROR init_1: %s\n", error_message(res.errno)));
      return &res;
    }
    if (!valid_course_name(params->course)) {
      res.errno = ERR_COURSE_NAME_INVAL;
      Debug(("ERROR init_1: %s\n", error_message(res.errno)));
      return &res;
    }

    if (strcmp(curconn->course, params->course)) {
      if (curconn->index)
	db_close(curconn->index);
      curconn->inited = 0;

      /*
       * Now try to open the index file.
       */
      sprintf(curconn->coursepath, "%s/%s/", root_dir, params->course);

      sprintf(indexpath, "%s%s%s", curconn->coursepath, INDEX_FILE,
	      UPDATING_EXTENSION);
      Debug(("Opening index file %s\n", indexpath));
      curconn->index = db_open(indexpath);
      if (!curconn->index) {
	res.errno = ERR_COURSE_NOT_FOUND;
	Debug(("ERROR init_1: %s\n", error_message(res.errno)));
	return &res;
      }

      /*
       * We've successfully opened the index file, so we can consider
       * this connection properly initialized.
       */
      curconn->inited = 1;
    }
  }
  strcpy(curconn->course, params->course);
  
#ifdef KERBEROS
  /* 
   * Deal with Kerberos authentication
   */
  if (krb_rd_req(&params->auth, KRB_SERVICE, my_hostname,
		 svc_getcaller(rqstp->rq_xprt)->sin_addr.s_addr,
		 &curconn->auth, "") != RD_AP_OK) {
    res.errno = ERR_WONT_BE_AUTHED;
    Debug(("ERROR init_1: %s\n", error_message(res.errno)));
    curconn->authed = 0;
  }
  else {
    sprintf(curconn->authname, "%s%s%s@%s", curconn->auth.pname,
	    *curconn->auth.pinst ? "." : "",
	    curconn->auth.pinst, curconn->auth.prealm);
    curconn->authed = 1;
    Debug(("Authenticated user %s\n", curconn->authname));
    res.errno = 0;
  }
#else
  /*
   * No Kerberos authentication available - just make user always
   * authenticated.
   */
  curconn->authed = 1;
  strcpy(curconn->authname, params->auth);
  res.errno = 0;
#endif /* KERBEROS */

#ifdef MULTI
  if (sync_site != server_me) {
    if (sync_site != -1) {
      res.errno = ERR_NOT_SYNC;
      Debug(("ERROR init_1: %s\n", error_message(res.errno)));
      res.init_res_u.sync = servers[sync_site].name;
    }
    else {
      res.errno = ERR_NO_QUORUM;
      Debug(("ERROR init_1: %s\n", error_message(res.errno)));
    }
  }
  if (curconn->server_num) {/* XXX */
    res.errno = 0;
    curconn->authed = 1;
  }
#endif /* MULTI */
  
  return &res;
}

/*
 * List the specified access control list for the current course.
 * Anyone can do this.  Is this a good thing?
 */

stringlist_res *list_acl_1(aclname, rqstp)
     char **aclname;
     struct svc_req *rqstp;
{
  FILE *fp;
  char aclfile[MAXPATHLEN], aclbfr[1024]; /* XXX */
  char *dotptr;
  stringlist *next;
  static stringlist_res res;
  
  Debug(("list_acl_1: %s\n", *aclname));

  stats.n_list_acl++;
  
  SETUP_CURCONN;
  CHECK_SENDRECV(res.errno, res);
  CHECK_DB(res.errno, res);
  
  CHECK_INIT(res.errno, res);

  if (!valid_filename(*aclname)) {
    res.errno = ERR_INVALID_FILENAME;
    Debug(("ERROR list_acl_1: %s\n", error_message(res.errno)));
    return &res;
  }
  
  if (strlen(*aclname) + strlen(curconn->coursepath) + 5 >
      MAXPATHLEN) {
    res.errno = ERR_ACL_NAME_LEN;
    Debug(("ERROR list_acl_1: %s\n", error_message(res.errno)));
    return &res;
  }

  sprintf(aclfile, "%sACL-%s", curconn->coursepath, *aclname);
  
  if (!(fp = fopen(aclfile, "r"))) {
    res.errno = ERR_ACL_NOT_FOUND;
    Debug(("ERROR list_acl_1: %s\n", error_message(res.errno)));
    return &res;
  }

  /*
   * Free previous result.   XXX Memory leak?
   */
  xdr_free(xdr_stringlist_res, &res);

  next = &res.stringlist_res_u.list;
  while (fgets(aclbfr, sizeof(aclbfr), fp)) {
    if (*aclbfr && aclbfr[strlen(aclbfr)-1] < ' ')
      aclbfr[strlen(aclbfr)-1] = '\0';
    /* Eliminate silly ".@" in <user>.@<realm> */
    dotptr = (char*)index(aclbfr, '.');
    if (dotptr && dotptr[1] == '@')
      strcpy(dotptr, dotptr+1);
    *next = (stringnode *)xmalloc(sizeof(stringnode));
    (*next)->s = xsave_string(aclbfr);
    next = &(*next)->next;
  }
  *next = 0;
  fclose(fp);
  res.errno = 0;
  return &res;
}

/*
 * Utility routine to do an ACL add or delete.
 */

long *acl_do_add_del(params, rqstp, func)
     acl_maint *params;
     struct svc_req *rqstp;
     int (*func)();
{
  char aclname[MAXPATHLEN];
  int acl_add();

  Debug(("acl_do_add_del: name=%s entry=%s\n", params->aclname,
	 params->aclparam));
  
  SETUP_CURCONN;
  
  CHECK_INIT(rpc_ret_int, rpc_ret_int);
  CHECK_SENDRECV(rpc_ret_int, rpc_ret_int);
  CHECK_WRITE(rpc_ret_int, rpc_ret_int);
  CHECK_AUTH(rpc_ret_int, rpc_ret_int);
  /* GOD access implies acl maint access. */
  if (!is_god()) CHECK_ACCESS(ACL_MAINT, rpc_ret_int, rpc_ret_int);
  
  if (!valid_filename(params->aclname)) {
    rpc_ret_int = ERR_INVALID_FILENAME;
    return &rpc_ret_int;
  }
  
  if (strlen(params->aclname) + strlen(curconn->coursepath) +
      strlen(UPDATING_EXTENSION) + 5 > MAXPATHLEN) {
    rpc_ret_int = ERR_ACL_NAME_LEN;
    return &rpc_ret_int;
  }

  sprintf(aclname, "%sACL-%s%s", curconn->coursepath, params->aclname,
	  UPDATING_EXTENSION);

  Debug(("ACL filename: %s\n", aclname));

  if (access(aclname, 0) == -1) {
    if (acl_initialize(aclname, 0600)) {
      rpc_ret_int = ERR_ACL_ERROR;
      return &rpc_ret_int;
    }
  }
  
  if ((*func)(aclname, params->aclparam))
    rpc_ret_int = ERR_ACL_ERROR;
  else
    rpc_ret_int = 0;

#ifdef MULTI
  if (!rpc_ret_int && !curconn->server_num) {
    int i;
    long *res;
    multi_set_course();
    for (i=0; i<nservers; i++) {
      if (servers[i].cl) {
	DebugMulti(("Calling add_acl_1 for server %s\n",
		    servers[i].name));
	if (func == acl_add)
	  res = (long *)_add_acl_1(params, servers[i].cl);
	else
	  res = (long *)_delete_acl_1(params, servers[i].cl);
	/* XXX */
	if (!res)
	  multi_conn_dropped(i);
      }
    }
    multi_commit();
  }
#endif /* MULTI */
  
  return &rpc_ret_int;
}

/*
 * Add an entry to the specified access control list for the current
 * course.  This command requires "maint" access.
 */

long *add_acl_1(params, rqstp)
     acl_maint *params;
     struct svc_req *rqstp;
{
  int acl_add();

  stats.n_add_acl++;
  
  return acl_do_add_del(params, rqstp, acl_add);
}

/*
 * Delete an entry to the specified access control list for the current
 * course.  This command requires "maint" access.
 */

long *delete_acl_1(params, rqstp)
     acl_maint *params;
     struct svc_req *rqstp;
{
  int acl_delete();

  stats.n_delete_acl++;
  
  return acl_do_add_del(params, rqstp, acl_delete);
}

/*
 * Create a new course.  This requires GOD access.
 */

long *create_course_1(coursename, rqstp)
     char **coursename;
     struct svc_req *rqstp;
{
  char coursedir[MAXPATHLEN];
  FILE *fp;
#ifdef MULTI
  int i;
  long *res;
#endif /* MULTI */

  TOUCH(rqstp);

  stats.n_create_course++;

  Debug(("create_course_1: %s\n", *coursename));
  
  CHECK_SENDRECV(rpc_ret_int, rpc_ret_int);
  CHECK_WRITE(rpc_ret_int, rpc_ret_int);
  CHECK_AUTH(rpc_ret_int, rpc_ret_int);

  if (!curconn->server_num && !is_god()) { /* XXX sync site! */
    rpc_ret_int = ERR_ACCESS_DENIED;
    return &rpc_ret_int;
  }

  if (strlen(*coursename) > COURSE_NAME_LEN) {
    rpc_ret_int = ERR_COURSE_NAME_LEN;
    return &rpc_ret_int;
  }

  if (!valid_course_name(*coursename)) {
    rpc_ret_int = ERR_INVALID_FILENAME;
    return &rpc_ret_int;
  }
  
  sprintf(coursedir, "%s/%s", root_dir, *coursename);

  if (mkdir(coursedir, 0700) == -1) {
    rpc_ret_int = ERR_COURSE_DIR_ERR;
    return &rpc_ret_int;
  }

  sprintf(coursedir, "%s/%s/ACL-%s%s", root_dir, *coursename,
	  ACL_TURNIN, UPDATING_EXTENSION);
  if (acl_initialize(coursedir, 0600))
    goto we_lost;
  if (!curconn->server_num && acl_add(coursedir, curconn->authname))
    goto we_lost;
  sprintf(coursedir, "%s/%s/ACL-%s%s", root_dir, *coursename,
	  ACL_GRADER, UPDATING_EXTENSION);
  if (acl_initialize(coursedir, 0600))
    goto we_lost;
  if (!curconn->server_num && acl_add(coursedir, curconn->authname))
    goto we_lost;
  sprintf(coursedir, "%s/%s/ACL-%s%s", root_dir, *coursename,
	  ACL_MAINT, UPDATING_EXTENSION);
  if (acl_initialize(coursedir, 0600))
    goto we_lost;
  if (!curconn->server_num && acl_add(coursedir, curconn->authname))
    goto we_lost;

  /*
   * Note: INDEX files will be created when someone first init's to
   * this course...don't need to do it now.
   */
#ifdef MULTI
  if (!curconn->server_num) {
    for (i=0; i<nservers; i++) {
      if (servers[i].cl) {
	DebugMulti(("Calling create_course_1 for server %s\n",
		    servers[i].name));
	/* XXX Who owns it?? */
	res = (long *)_create_course_1(coursename, servers[i].cl);
	/* XXX */
	if (!res)
	  multi_conn_dropped(i);
      }
    }
    multi_commit();
  }
#endif /* MULTI */

  /* XXX */
  sprintf(coursedir, "%s/%s", root_dir, COURSE_INDEX);
  if (!(fp = fopen(coursedir, "a")))
    fatal("Can't append to %s", coursedir);
  fprintf(fp, "%s\n", *coursename);
  fclose(fp);
  
  rpc_ret_int = 0;
  return &rpc_ret_int;

 we_lost:
  sprintf(coursedir, "%s/%s/ACL-%s", root_dir, *coursename, ACL_TURNIN);
  unlink(coursedir);
  sprintf(coursedir, "%s/%s/ACL-%s", root_dir, *coursename, ACL_GRADER);
  unlink(coursedir);
  sprintf(coursedir, "%s/%s/ACL-%s", root_dir, *coursename, ACL_MAINT);
  unlink(coursedir);
  sprintf(coursedir, "%s/%s", root_dir, *coursename);
  rmdir(coursedir);
  rpc_ret_int = ERR_COURSE_DIR_ACL;
  return &rpc_ret_int;
}

/*
 * Delete a course.  This requires GOD access.
 */

long *delete_course_1(coursename, rqstp)
     char **coursename;
     struct svc_req *rqstp;
{
  char rmbuf[1024], buf2[1024], inbuf[1024], dbfilename[MAXPATHLEN];
  FILE *fpin, *fpout;
#ifdef MULTI
  int i;
  long *res;
#endif /* MULTI */

  TOUCH(rqstp);

  stats.n_delete_course++;

  Debug(("delete_course_1: %s\n", *coursename));
  
  CHECK_SENDRECV(rpc_ret_int, rpc_ret_int);
  CHECK_WRITE(rpc_ret_int, rpc_ret_int);
  CHECK_AUTH(rpc_ret_int, rpc_ret_int);

  if (!curconn->server_num && !is_god()) { /* XXX sync site! */
    rpc_ret_int = ERR_ACCESS_DENIED;
    return &rpc_ret_int;
  }

  if (strlen(*coursename) > COURSE_NAME_LEN) {
    rpc_ret_int = ERR_COURSE_NAME_LEN;
    return &rpc_ret_int;
  }

  if (!valid_course_name(*coursename)) {
    rpc_ret_int = ERR_INVALID_FILENAME;
    return &rpc_ret_int;
  }
  
  sprintf(rmbuf, "rm -rf %s/%s", root_dir, *coursename);
  
  system(rmbuf);

  /* remove name from course index */
  /* XXX */
  sprintf(rmbuf, "%s/%s", root_dir, COURSE_INDEX);
  fpin = fopen(rmbuf, "r");
  sprintf(buf2, "%s/%s.new", root_dir, COURSE_INDEX);
  fpout = fopen(buf2, "w");
  while (fgets(inbuf, sizeof(inbuf), fpin)) {
    inbuf[strlen(inbuf)-1] = '\0';
    if (strcmp(inbuf, *coursename))
      fprintf(fpout, "%s\n", inbuf);
  }
  fclose(fpin);
  fclose(fpout);
  rename(buf2, rmbuf);

  /* flush course database from cache */
  sprintf(dbfilename, "%s/%s/%s%s", root_dir,
	  *coursename, INDEX_FILE, UPDATING_EXTENSION);
  db_flush(dbfilename);

#ifdef MULTI
  if (!curconn->server_num) {
    for (i=0; i<nservers; i++) {
      if (servers[i].cl) {
	DebugMulti(("Calling delete_course_1 for server %s\n",
		    servers[i].name));
	res = (long *)_delete_course_1(coursename, servers[i].cl);
	/* XXX */
	if (!res)
	  multi_conn_dropped(i);
      }
    }
    multi_commit();
  }
#endif /* MULTI */

  rpc_ret_int = 0;
  return &rpc_ret_int;
}

/*
 * List available courses.
 */

stringlist_res *list_courses_1(dummy, rqstp)
     int *dummy;
     struct svc_req *rqstp;
{
  FILE *fp;
  char indexpath[MAXPATHLEN], coursebfr[COURSE_NAME_LEN];
  stringlist *next;
  static stringlist_res res;

  TOUCH(dummy);
  
  Debug(("list_courses_1:\n"));

  stats.n_list_courses++;
  
  SETUP_CURCONN;
  CHECK_SENDRECV(res.errno, res);
  CHECK_DB(res.errno, res);
  
  sprintf(indexpath, "%s/%s", root_dir, COURSE_INDEX);
  
  if (!(fp = fopen(indexpath, "r"))) {
    res.errno = ERR_INTERNAL_FAILURE;
    Debug(("ERROR list_courses_1: %s\n", error_message(res.errno)));
    return &res;
  }

  /*
   * Free previous result.   XXX Memory leak?
   */
  xdr_free(xdr_stringlist_res, &res);

  next = &res.stringlist_res_u.list;
  while (fgets(coursebfr, sizeof(coursebfr), fp)) {
    if (*coursebfr && coursebfr[strlen(coursebfr)-1] < ' ')
      coursebfr[strlen(coursebfr)-1] = '\0';
    *next = (stringnode *)xmalloc(sizeof(stringnode));
    (*next)->s = xsave_string(coursebfr);
    next = &(*next)->next;
  }
  *next = 0;
  fclose(fp);
  res.errno = 0;
  return &res;
}

/*
 * List files that match a given Paper descriptor.  Apply standard
 * security to make sure people can't look at each other's papers, etc.
 */

Paperlist_res *list_1(paper, rqstp)
     Paper *paper;
     struct svc_req *rqstp;
{
  static Paperlist_res res;
  Paperlist *next;
  int wild_type, wild_assignment, wild_author, wild_owner;
  int wild_filename;
  int is_grader;
  Contents db_criterion, *contents;
  Debug(("list_1: %s", print_paper(paper)));

  stats.n_list++;
  
  SETUP_CURCONN;
  CHECK_INIT(res.errno, res);
  CHECK_DB(res.errno, res);
  CHECK_AUTH(res.errno, res);
  CHECK_SENDRECV(res.errno, res);
  is_grader = check_access(ACL_GRADER);

  /*
   * Free previous result.  XXX Memory leak?
   */
  
  xdr_free(xdr_Paperlist_res, &res);

  next = &res.Paperlist_res_u.list;

  wild_type = wild_assignment = wild_author = wild_owner =
    wild_filename = 0;
  bzero((char *) &db_criterion, sizeof(Contents));

  if (paper->type == TYPE_WILDCARD)
    wild_type = 1;
  if (paper->assignment == ASSIGNMENT_WILDCARD) {
    wild_assignment = 1;
  } else db_criterion.p.assignment = paper->assignment;
  if (!strcmp(paper->author, AUTHOR_WILDCARD)) {
    wild_author = 1;
    db_criterion.p.author = "";
  } else db_criterion.p.author = paper->author;
  if (!strcmp(paper->owner, OWNER_WILDCARD))
    wild_owner = 1;
  if (!strcmp(paper->filename, FILENAME_WILDCARD)) {
    wild_filename = 1;
    db_criterion.p.filename = "";
  } else db_criterion.p.filename = paper->filename;

  for (contents = db_firstkey(&db_criterion); contents;
       contents = db_nextkey()) {

    if (!wild_author) {
      if (strcmp(contents->p.author, paper->author)) break;
      if (!wild_assignment) {
	if (contents->p.assignment != paper->assignment) break;
	if (!wild_filename) {
	  if (strcmp(contents->p.filename, paper->filename)) break;
	}
      }
    }

    /* Students can't list what others have turned in. */
    if (paper->type != HANDOUT && paper->type != EXCHANGE
	&& strcmp(contents->p.author, curconn->authname)
	&& !is_grader)
      continue;

    if (!wild_assignment && contents->p.assignment != paper->assignment)
      continue;
    if (!wild_author && strcmp(contents->p.author, paper->author))
      continue;
    if (!wild_filename && strcmp(contents->p.filename, paper->filename))
      continue;

    contents = db_fullcontents();
    if (!wild_type && contents->p.type != paper->type)
      continue;
    if (!wild_owner && strcmp(contents->p.owner, paper->owner))
      continue;

    *next = (PaperNode *)xmalloc(sizeof(PaperNode));
    /* XXX VERY LARGE memory leak here? */
    copy_paper(&contents->p, &(*next)->p);
    next = &(*next)->next;
  }
  *next = 0;
  res.errno = 0;
  return &res;
}

/*
 * Start a file transfer from the client for this connection.
 */

long *send_file_1(paper, rqstp)
     Paper *paper;
     struct svc_req *rqstp;
{
  char filepath[MAXPATHLEN];
  
  Debug(("send_file_1: %s", print_paper(paper)));

  stats.n_send_file++;
  
  SETUP_CURCONN;
  CHECK_INIT(rpc_ret_int, rpc_ret_int);
  CHECK_WRITE(rpc_ret_int, rpc_ret_int);
  CHECK_AUTH(rpc_ret_int, rpc_ret_int);

  if (paper->type == TURNEDIN ||
      paper->type == EXCHANGE) {
    CHECK_ACCESS(ACL_TURNIN, rpc_ret_int, rpc_ret_int);
  }
  else {
    CHECK_ACCESS(ACL_GRADER, rpc_ret_int, rpc_ret_int);
  }
  
  if (curconn->sendrecv) {
    rpc_ret_int = ERR_SEND_RECV_PROGRESS;
    return &rpc_ret_int;
  }
  
  if (contains_wildcard(paper)) {
    rpc_ret_int = ERR_NO_WILDCARDS;
    return &rpc_ret_int;
  }
  if (!valid_filename(paper->filename)) {
    rpc_ret_int = ERR_INVALID_FILENAME;
    return &rpc_ret_int;
  }

  copy_paper(paper, &curconn->sendcont.p);
  if (!check_access(ACL_GRADER)) {
    xfree(curconn->sendcont.p.author);
    curconn->sendcont.p.author = xsave_string(curconn->authname);
  }
  xfree(curconn->sendcont.p.owner);
  curconn->sendcont.p.owner = xsave_string(curconn->authname);
  xfree(curconn->sendcont.p.location.host);
  curconn->sendcont.p.location.host = xsave_string(my_canonhostname);
  gettimeofday(&curconn->sendcont.p.location.time, (struct timezone *)NULL);
  paper = &curconn->sendcont.p;
  paper->created = paper->location.time;
  paper->modified = paper->location.time;
  paper->location.time.tv_usec--;
  do {
    paper->location.time.tv_usec++;
    sprintf(curconn->sendcont.ptrfile, "file%08d%08d",
	    paper->location.time.tv_sec,
	    paper->location.time.tv_usec);
    strcpy(filepath, curconn->coursepath);
    strcat(filepath, curconn->sendcont.ptrfile);
  } while(!access(filepath, F_OK));

  Debug(("Sending file to %s\n", filepath));
  
  curconn->sendrecvfp = fopen(filepath, "w+");
  if (!curconn->sendrecvfp) {
    rpc_ret_int = ERR_INTERNAL_FAILURE;
    return &rpc_ret_int;
  }

  curconn->sendcont.p.size = 0;
  curconn->sendcont.p.words = 0;
  curconn->sendcont.p.lines = 0;
  curconn->tokenstate = 0;
  
  curconn->sendrecv = IS_SENDING;
  rpc_ret_int = 0;
  return &rpc_ret_int;
}

/*
 * Store a burst of data into the currently open file.
 */

long *send_burst_1(params, rqstp)
     burst_data *params;
     struct svc_req *rqstp;
{
  Debug(("send_burst_1: %d bytes\n", params->size));

  stats.n_send_burst++;
  
  SETUP_CURCONN;
  
  CHECK_INIT(rpc_ret_int, rpc_ret_int);
  CHECK_AUTH(rpc_ret_int, rpc_ret_int);
  CHECK_WRITE(rpc_ret_int, rpc_ret_int);

  if (curconn->sendrecv != IS_SENDING) {
    rpc_ret_int = ERR_NOT_SENDING;
    return &rpc_ret_int;
  }

  if (params->size > 0 && params->size <= MAX_BURST_SIZE &&
      fwrite(params->data, params->size, 1, curconn->sendrecvfp) != 1) {
    rpc_ret_int = ERR_INTERNAL_FAILURE;
    fclose(curconn->sendrecvfp);
    curconn->sendrecvfp = NULL;
    curconn->sendrecv = 0;
    unlink(curconn->sendcont.ptrfile);
    return &rpc_ret_int;
  }

  stats.bytes_recv += params->size;
  curconn->sendcont.p.size += params->size;
  do_wc(params->data, params->size, &curconn->tokenstate,
	&curconn->sendcont.p.words, &curconn->sendcont.p.lines);
  
  rpc_ret_int = 0;
  return &rpc_ret_int;
}

/*
 * Finish a file send and make the database entry.
 */

long *end_send_1(params, rqstp)
     int params;
     struct svc_req *rqstp;
{
  Debug(("end_send_1:\n"));

  stats.n_end_send++;
  
  TOUCH(params);
  
  SETUP_CURCONN;
  
  CHECK_INIT(rpc_ret_int, rpc_ret_int);
  CHECK_AUTH(rpc_ret_int, rpc_ret_int);
  CHECK_WRITE(rpc_ret_int, rpc_ret_int);

  if (curconn->sendrecv != IS_SENDING) {
    rpc_ret_int = ERR_NOT_SENDING;
    return &rpc_ret_int;
  }

  fclose(curconn->sendrecvfp);
  curconn->sendrecvfp = NULL;
  curconn->sendrecv = 0;

  if (db_store(&curconn->sendcont)) rpc_ret_int = ERR_INTERNAL_FAILURE;

  xfree(curconn->sendcont.p.author);
  xfree(curconn->sendcont.p.owner);
  xfree(curconn->sendcont.p.desc);
  xfree(curconn->sendcont.p.filename);
  xfree(curconn->sendcont.p.location.host);

  rpc_ret_int = 0;
  return &rpc_ret_int;
}

/*
 * Retrieve a file from the local server.
 */

long *retrieve_file_1(paper, rqstp)
     Paper *paper;
     struct svc_req *rqstp;
{
  char filepath[MAXPATHLEN];
  Contents contents;
  
  Debug(("retrieve_file_1: %s", print_paper(paper)));

  stats.n_retrieve_file++;
  
  SETUP_CURCONN;
  CHECK_INIT(rpc_ret_int, rpc_ret_int);
  CHECK_AUTH(rpc_ret_int, rpc_ret_int);
  CHECK_DB(rpc_ret_int, rpc_ret_int);

  if (curconn->sendrecv) {
    rpc_ret_int = ERR_SEND_RECV_PROGRESS;
    return &rpc_ret_int;
  }
  
  if (contains_wildcard(paper)) {
    rpc_ret_int = ERR_NO_WILDCARDS;
    return &rpc_ret_int;
  }

  contents.p = *paper;
  if (db_fetch(&contents)) {
    printf("Couldn't find it!\n");
    rpc_ret_int = ERR_FILE_NOT_FOUND;
    return &rpc_ret_int;
  }

  if (strcasecmp(contents.p.location.host, my_canonhostname)) {
    rpc_ret_int = ERR_FILE_WRONG_SERVER;
    return &rpc_ret_int;
  }

  if (contents.p.type != HANDOUT &&
      contents.p.type != EXCHANGE &&
      !check_access(ACL_GRADER) &&
      strcmp(contents.p.author, curconn->authname)) {
    rpc_ret_int = ERR_ACCESS_DENIED;
    return &rpc_ret_int;
  }
  
  strcpy(filepath, curconn->coursepath);
  strcat(filepath, contents.ptrfile);
  curconn->sendrecvfp = fopen(filepath, "r");
  if (!curconn->sendrecvfp) {
    rpc_ret_int = ERR_INTERNAL_FAILURE;
    return &rpc_ret_int;
  }

  curconn->linecount = (int) (((unsigned) ~0) >> 1); /* i.e. MAXINT */
  
  curconn->sendrecv = IS_RECEIVING;
  rpc_ret_int = 0;
  return &rpc_ret_int;
}

/*
 * Retrieve a portion of a file from the local server.
 */

long *portion_1(portion, rqstp)
     portionspec *portion;
     struct svc_req *rqstp;
{
  char filepath[MAXPATHLEN];
  Contents contents;
  int line, c;
  
  Debug(("portion_1: %s", print_paper(&portion->p)));

  stats.n_portion++;
  
  SETUP_CURCONN;
  CHECK_INIT(rpc_ret_int, rpc_ret_int);
  CHECK_AUTH(rpc_ret_int, rpc_ret_int);
  CHECK_DB(rpc_ret_int, rpc_ret_int);

  if (curconn->sendrecv) {
    rpc_ret_int = ERR_SEND_RECV_PROGRESS;
    return &rpc_ret_int;
  }
  
  if (contains_wildcard(&portion->p)) {
    rpc_ret_int = ERR_NO_WILDCARDS;
    return &rpc_ret_int;
  }

  contents.p = portion->p;
  if (db_fetch(&contents)) {
    rpc_ret_int = ERR_FILE_NOT_FOUND;
    return &rpc_ret_int;
  }

  /* XXX 6.4R has a new idea of what is "canonical" */
  if (strcasecmp(contents.p.location.host, my_canonhostname)) {
    rpc_ret_int = ERR_FILE_WRONG_SERVER;
    return &rpc_ret_int;
  }

  if (contents.p.type != HANDOUT &&
      contents.p.type != EXCHANGE &&
      !check_access(ACL_GRADER) &&
      strcmp(contents.p.author, curconn->authname)) {
    rpc_ret_int = ERR_ACCESS_DENIED;
    return &rpc_ret_int;
  }
  
  strcpy(filepath, curconn->coursepath);
  strcat(filepath, contents.ptrfile);
  curconn->sendrecvfp = fopen(filepath, "r");
  if (!curconn->sendrecvfp) {
    rpc_ret_int = ERR_INTERNAL_FAILURE;
    return &rpc_ret_int;
  }

  if (portion->start < 1 || portion->start > contents.p.lines ||
      portion->end < 1 || portion->end > contents.p.lines ||
      portion->end < portion->start) {
    rpc_ret_int = ERR_BAD_PORTION;
    return &rpc_ret_int;
  }
  
  for (line=1; line < portion->start; line++) {
    while ((c = getc(curconn->sendrecvfp)) != EOF && c != '\n')
      ;
    if (c == EOF)
      break;
  }

  curconn->linecount = portion->end - portion->start + 1;
  
  curconn->sendrecv = IS_RECEIVING;
  rpc_ret_int = 0;
  return &rpc_ret_int;
}

/*
 * Retrieve a "burst" of data from the currently open file.
 */

retrieve_res *retrieve_burst_1(params, rqstp)
     int *params;
     struct svc_req *rqstp;
{
  static retrieve_res res;
  char *ptr;
  
  Debug(("retrieve_burst_1:\n"));

  stats.n_retrieve_burst++;
  
  TOUCH(params);
  
  SETUP_CURCONN;
  
  CHECK_INIT(res.errno, res);
  CHECK_AUTH(res.errno, res);
  CHECK_DB(res.errno, res);

  if (curconn->sendrecv != IS_RECEIVING) {
    res.errno = ERR_NOT_RECEIVING;
    Debug(("ERROR retrieve_burst_1: %s\n", error_message(res.errno)));
    return &res;
  }

  res.retrieve_res_u.burst.size = fread(res.retrieve_res_u.burst.data, 1,
					MAX_BURST_SIZE, curconn->sendrecvfp);
  if (res.retrieve_res_u.burst.size < 0) {
    fclose(curconn->sendrecvfp);
    curconn->sendrecvfp = NULL;
    curconn->sendrecv = 0;
    res.errno = ERR_INTERNAL_FAILURE;
    Debug(("ERROR retrieve_burst_1: %s\n", error_message(res.errno)));
    return &res;
  }

  if (res.retrieve_res_u.burst.size != MAX_BURST_SIZE) {
    fclose(curconn->sendrecvfp);
    curconn->sendrecvfp = NULL;
    curconn->sendrecv = 0;
  }
  stats.bytes_sent += res.retrieve_res_u.burst.size;

  for (ptr=res.retrieve_res_u.burst.data;
       ptr != res.retrieve_res_u.burst.data +
       res.retrieve_res_u.burst.size; ptr++) {
    if (*ptr == '\n') {
      curconn->linecount--;
      if (!curconn->linecount) {
	res.retrieve_res_u.burst.size = ptr -
	  res.retrieve_res_u.burst.data + 1;
	if (curconn->sendrecv) {
	  fclose(curconn->sendrecvfp);
	  curconn->sendrecvfp = NULL;
	  curconn->sendrecv = 0;
	}
	break;
      }
    }
  }
  
  res.errno = 0;
  return &res;
}

/*
 * Do a copy or move operation.
 */

long *do_copy_move(params, rqstp, move)
     TwoPaper *params;
     struct svc_req *rqstp;
     int move;
{
  Contents src, dest;
  char filepathsrc[MAXPATHLEN], filepathdest[MAXPATHLEN];
  
  Debug(("do_copy_move: %s ", print_paper(&params->src)));
  Debug(("to %s\n", print_paper(&params->dest)));
  
  SETUP_CURCONN;

  if (contains_wildcard(&params->src) ||
      contains_wildcard(&params->dest)) {
    rpc_ret_int = ERR_NO_WILDCARDS;
    return &rpc_ret_int;
  }

  src.p = params->src;
  dest.p = params->dest;
  
  CHECK_INIT(rpc_ret_int, rpc_ret_int);
  CHECK_AUTH(rpc_ret_int, rpc_ret_int);
  CHECK_SENDRECV(rpc_ret_int, rpc_ret_int);
  CHECK_WRITE(rpc_ret_int, rpc_ret_int);
  /* Students have limited permission to do copies or moves */
  if ((dest.p.type != PICKEDUP &&
       dest.p.type != EXCHANGE &&
       dest.p.type != TURNEDIN)
      || (strcmp(src.p.owner, curconn->authname) &&
	  strcmp(src.p.author, curconn->authname))) {
    CHECK_ACCESS(ACL_GRADER, rpc_ret_int, rpc_ret_int);
  }
  else {
    CHECK_ACCESS(ACL_TURNIN, rpc_ret_int, rpc_ret_int);
  }

  rpc_ret_int = 0;
  if (db_fetch(&src)) {
    rpc_ret_int = ERR_PAPER_NOT_FOUND;
    return &rpc_ret_int;
  }
  /* XXX Should we allow creation time to be changed? */
  gettimeofday(&dest.p.modified, (struct timezone *)NULL);
  dest.p.location.host = my_canonhostname;
  dest.p.location.time = dest.p.modified;
  dest.p.owner = curconn->authname;
  dest.p.location = src.p.location;
  dest.p.size = src.p.size;
  dest.p.words = src.p.words;
  dest.p.lines = src.p.lines;
  
  if (move) {
    copy_paper(&src.p, &src.p);
    db_delete(&src);
    free_paper(&src.p);
    strcpy(dest.ptrfile, src.ptrfile);
  }
  else {
    dest.p.location.host = my_canonhostname;
    dest.p.location.time = dest.p.modified;
    /* XXX Copy - set creation time? */
    dest.p.location.time.tv_usec--;
    do {
      dest.p.location.time.tv_usec++;
      sprintf(dest.ptrfile, "file%08d%08d",
	      dest.p.location.time.tv_sec,
	      dest.p.location.time.tv_usec);
      strcpy(filepathdest, curconn->coursepath);
      strcat(filepathdest, dest.ptrfile);
    } while(!access(filepathdest, F_OK));
    strcpy(filepathsrc, curconn->coursepath);
    strcat(filepathsrc, src.ptrfile);
    if (copy_file(filepathsrc, filepathdest)) {
      rpc_ret_int = ERR_INTERNAL_FAILURE;
      return &rpc_ret_int;
    }
  }
  if (db_store(&dest)) rpc_ret_int = ERR_INTERNAL_FAILURE;
  return &rpc_ret_int;
}

/*
 * Copy a paper to another paper.  Requires "grader" access.
 */

long *copy_1(params, rqstp)
     TwoPaper *params;
     struct svc_req *rqstp;
{
  stats.n_copy++;
  
  return do_copy_move(params, rqstp, 0);
}

/*
 * Move a paper to another paper.  Requires "grader" access.
 */

long *move_1(params, rqstp)
     TwoPaper *params;
     struct svc_req *rqstp;
{
  stats.n_move++;
  
  return do_copy_move(params, rqstp, 1);
}

/*
 * Delete a paper.  Requires "grader" access.
 */

long *delete_1(paper, rqstp)
     Paper *paper;
     struct svc_req *rqstp;
{
  Contents cont;
  char filepath[MAXPATHLEN];
  
  Debug(("delete_1: %s", print_paper(paper)));

  stats.n_delete++;
  
  SETUP_CURCONN;
  
  if (contains_wildcard(paper)) {
    rpc_ret_int = ERR_NO_WILDCARDS;
    return &rpc_ret_int;
  }

  CHECK_INIT(rpc_ret_int, rpc_ret_int);
  CHECK_AUTH(rpc_ret_int, rpc_ret_int);
  CHECK_WRITE(rpc_ret_int, rpc_ret_int);

  /* Students can delete their own exchange papers */
  if (paper->type != EXCHANGE ||
      (strcmp(paper->owner, curconn->authname) &&
       strcmp(paper->author, curconn->authname)))
    CHECK_ACCESS(ACL_GRADER, rpc_ret_int, rpc_ret_int);

  cont.p = *paper;
  
  if (db_fetch(&cont)) {
    rpc_ret_int = ERR_PAPER_NOT_FOUND;
    return &rpc_ret_int;
  }
  
  copy_paper(&cont.p, &cont.p);
  db_delete(&cont);
  free_paper(&cont.p);
  (void) strcpy(filepath, curconn->coursepath);
  (void) strcat(filepath, cont.ptrfile);
  unlink(filepath);
  rpc_ret_int = 0;
  return &rpc_ret_int;
}

/*
 * Return information about Kerberos usage: service, instance, and
 * realm.  Error if not compiled to use Kerberos.
 */

krb_info_res *krb_info_1(dummy, rqstp)
     int *dummy;
     struct svc_req *rqstp;
{
  static krb_info_res res;
  static char realm[256];
  
  Debug(("krb_info_1:\n"));

  stats.n_krb_info++;
  
  SETUP_CURCONN;

  TOUCH(dummy);
  TOUCH(rqstp);
  
#ifdef KERBEROS
  krb_get_lrealm(realm, 1); /* XXX Return value */
  res.errno = 0;
  res.krb_info_res_u.info.service = KRB_SERVICE;
  res.krb_info_res_u.info.instance = my_hostname;
  res.krb_info_res_u.info.realm = realm;

  return &res;
#else
  res.errno = ERR_NO_KERBEROS;
  Debug(("ERROR krb_info_1: %s\n", error_message(res.errno)));
  return &res;
#endif /* KERBEROS */
}

/*
 * Kill the server - must be GOD to do this.  This (obviously) won't
 * return an RPC value if it succeeds.
 */

long *kill_server_1(dummy, rqstp)
     int *dummy;
     struct svc_req *rqstp;
{
  Debug(("kill_server_1:\n"));

  TOUCH(dummy);
  TOUCH(rqstp);

  stats.n_kill_server++;
  
  SETUP_CURCONN;
  CHECK_SENDRECV(rpc_ret_int, rpc_ret_int);
  CHECK_AUTH(rpc_ret_int, rpc_ret_int);

  if (!is_god()) {
    rpc_ret_int = ERR_ACCESS_DENIED;
    return &rpc_ret_int;
  }

  /*
   * Bye!
   */
  exit(0);
}

/*
 * Return server statistics.
 */

server_stats *server_stats_1(dummy, rqstp)
     int *dummy;
     struct svc_req *rqstp;
{
  Debug(("server_stats_1:\n"));

  TOUCH(dummy);
  TOUCH(rqstp);

  stats.n_server_stats++;
  
  SETUP_CURCONN;

  stats.uptime = time(0) - stats.start_time;
  stats.vers = db_vers;
  
  return &stats;
}
