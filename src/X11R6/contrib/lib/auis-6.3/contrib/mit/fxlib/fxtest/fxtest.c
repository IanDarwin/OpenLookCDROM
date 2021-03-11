/*
 * The FX (File Exchange) Server
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/fxtest/RCS/fxtest.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/fxtest/RCS/fxtest.c,v 1.3 1992/12/15 21:52:15 rr2b R6tape $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/fxtest/RCS/fxtest.c,v 1.3 1992/12/15 21:52:15 rr2b R6tape $";
#endif

#include <mit-copyright.h>

/*
 * This is a simple client test program that allows the user to
 * specify commands explicitly and send them to a particular server.
 */

 

#include <stdio.h>
#include <rpc/rpc.h>
#include <fx_prot.h>
#include <fxserver_err.h>
#include <rpc_err.h>
#include <fx-internal.h>
#include <ss/ss.h>
#include <krb.h>

#define CLNTSTAT clnt_call_status_gross_hack

enum clnt_stat clnt_call_status_gross_hack;

int sci_idx;
extern ss_request_table fxtest_cmds;
CLIENT *cl;
char *host;

#define CHECK_TIMEOUT(a) if (!(a)) { fprintf(stderr, "RPC timeout\n"); return; }

main(argc, argv)
    int argc;
    char *argv[];
{
    int code;

    if (argc < 2) {
	fprintf(stderr, "Usage: fxtest <host>\n");
	exit(1);
    }

    init_fxsv_err_tbl();
    init_rpc_err_tbl();
    
    sci_idx = ss_create_invocation("fxtest", "", 0, &fxtest_cmds, &code);
    if (code) {
	ss_perror(sci_idx, code, "while creating ss invocation");
	exit(1);
    }

    host = argv[1];
    cl = clnt_create(argv[1], FXSERVER, FXVERS, "tcp");
    if (!cl) {
	perror("clnt_create");
	exit(1);
    }

    ss_listen(sci_idx, &code);
    exit(0);
}

cmd_init(argc, argv)
    int argc;
    char *argv[];
{
    init_data params;
    init_res *res;
    
    if (argc != 2) {
	fprintf(stderr, "Usage: init <course>\n");
	return;
    }
    params.course = argv[1];
    if (krb_mk_req(&params.auth, KRB_SERVICE, host, "ATHENA.MIT.EDU", 0)) {
	fprintf(stderr, "Can't make authenticator!\n");
	return;
    }
    
    res = init_1(&params, cl);
    if (!res) {
	show_rpc_error(cl);
	return;
    }
    if (res->errno)
	ss_perror(sci_idx, res->errno, "calling init_1");
    if (res->errno == ERR_NOT_SYNC)
	fprintf(stderr, "Sync site is: %s\n", res->init_res_u.sync);
}

cmd_krb_info(argc, argv)
    int argc;
    char *argv[];
{
    krb_info_res *res;
    int dummy;

    res = krb_info_1(&dummy, cl);
    if (!res) {
	show_rpc_error(cl);
	return;
    }
    if (res->errno) {
	ss_perror(sci_idx, res->errno, "calling krb_info_1");
	return;
    }
    printf("Service: %s\nInstance: %s\nRealm: %s\n",
	   res->krb_info_res_u.info.service,
	   res->krb_info_res_u.info.instance,
	   res->krb_info_res_u.info.realm);
}
	   
cmd_list_acl(argc, argv)
    int argc;
    char *argv[];
{
    stringlist_res *res;
    stringnode *ptr;

    if (argc != 2) {
	fprintf(stderr, "Usage: list_acl <aclname>\n");
	return;
    }
    res = list_acl_1(&argv[1], cl);
    if (!res) {
	show_rpc_error(cl);
	return;
    }
    if (res->errno) {
	ss_perror(sci_idx, res->errno, "calling list_acl_1");
	return;
    }
    for (ptr = res->stringlist_res_u.list; ptr; ptr = ptr->next)
	printf("%s\n", ptr->s);
    xdr_free(xdr_stringlist_res, res);
}

cmd_acl_add(argc, argv)
    int argc;
    char *argv[];
{
    long *retval;
    acl_maint maint;
    
    if (argc != 3) {
	fprintf(stderr, "Usage: acl_add <aclname> <person>\n");
	return;
    }

    maint.aclname = argv[1];
    maint.aclparam = argv[2];
    
    retval = add_acl_1(&maint, cl);
    if (!retval) {
	show_rpc_error(cl);
	return;
    }
    if (*retval)
	ss_perror(sci_idx, *retval, "calling add_acl_1");
}

cmd_acl_del(argc, argv)
    int argc;
    char *argv[];
{
    long *retval;
    acl_maint maint;
    
    if (argc != 3) {
	fprintf(stderr, "Usage: acl_del <aclname> <person>\n");
	return;
    }

    maint.aclname = argv[1];
    maint.aclparam = argv[2];
    
    retval = delete_acl_1(&maint, cl);
    if (!retval) {
	show_rpc_error(cl);
	return;
    }
    if (*retval)
	ss_perror(sci_idx, *retval, "calling delete_acl_1");
}

cmd_create_course(argc, argv)
    int argc;
    char *argv[];
{
    long *retval;
    acl_maint maint;
    
    if (argc != 2) {
	fprintf(stderr, "Usage: create_course <name>\n");
	return;
    }

    retval = create_course_1(&argv[1], cl);
    if (!retval) {
	show_rpc_error(cl);
	return;
    }
    if (*retval)
	ss_perror(sci_idx, *retval, "calling create_course_1");
}

cmd_delete_course(argc, argv)
    int argc;
    char *argv[];
{
    long *retval;
    acl_maint maint;
    
    if (argc != 2) {
	fprintf(stderr, "Usage: delete_course <name>\n");
	return;
    }

    retval = delete_course_1(&argv[1], cl);
    if (!retval) {
	show_rpc_error(cl);
	return;
    }
    if (*retval)
	ss_perror(sci_idx, *retval, "calling delete_course_1");
}

cmd_list_courses(argc, argv)
    int argc;
    char *argv[];
{
    stringlist_res *res;
    stringnode *ptr;

    res = list_courses_1(&argv[1], cl);
    if (!res) {
	show_rpc_error(cl);
	return;
    }
    if (res->errno) {
	ss_perror(sci_idx, res->errno, "calling list_courses_1");
	return;
    }
    for (ptr = res->stringlist_res_u.list; ptr; ptr = ptr->next)
	printf("%s\n", ptr->s);
    xdr_free(xdr_stringlist_res, res);
}

cmd_list(argc, argv)
    int argc;
    char *argv[];
{
    Paperlist_res *res;
    Paper p;
    PaperNode *ptr;

    if (argc != 6) {
	fprintf(stderr, "Usage: list <type> <assgn> <author> <owner> <file>\n");
	return;
    }
    p.type = (PaperType)atoi(argv[1]);
    p.assignment = atoi(argv[2]);
    p.author = argv[3];
    p.owner = argv[4];
    p.filename = argv[5];
    p.location.host = ID_WILDCARD;
    p.desc = "";
    
    res = list_1(&p, cl);
    if (!res) {
	show_rpc_error(cl);
	return;
    }
    if (res->errno) {
	ss_perror(sci_idx, res->errno, "list_1");
	return;
    }
    for (ptr = res->Paperlist_res_u.list; ptr; ptr = ptr->next) {
	printf(PRINTPAPER(&ptr->p));
    }
    xdr_free(xdr_Paperlist_res, res);
}

select_paper(ret)
    Paper *ret;
{
    Paperlist_res *res;
    Paper p;
    PaperNode *ptr;
    int cnt, sel;
    char bfr[256];

    bzero(&p, sizeof(p));
    p.type = TYPE_WILDCARD;
    p.assignment = ASSIGNMENT_WILDCARD;
    p.author = AUTHOR_WILDCARD;
    p.owner = OWNER_WILDCARD;
    p.filename = FILENAME_WILDCARD;
    p.location.host = ID_WILDCARD;
    
    res = list_1(&p, cl);
    if (!res) {
	show_rpc_error(cl);
	return;
    }
    if (res->errno) {
	ss_perror(sci_idx, res->errno, "list_1");
	return;
    }
    cnt = 0;
    for (ptr = res->Paperlist_res_u.list; ptr; ptr = ptr->next) {
	printf("Selection %d:\n", cnt);
	printf(PRINTPAPER(&ptr->p));
	cnt++;
    }
    do {
	printf("Selection: ");
	gets(bfr);
	sel = atoi(bfr);
    } while (sel < 0 || sel > cnt-1);
    for (ptr = res->Paperlist_res_u.list, cnt=0; cnt < sel;
	 cnt++, ptr = ptr->next)
	;
    *ret = ptr->p;
/* XXX    xdr_free(xdr_Paperlist_res, res);*/
}

cmd_send(argc, argv)
    int argc;
    char *argv[];
{
    FILE *fp;
    long *retval;
    int dummy;
    Paper p;
    burst_data data;
    
    if (argc != 7) {
	fprintf(stderr, "Usage: send <localfile> <type> <assgn> <author> <owner> <file>\n");
	return;
    }
    p.type = (PaperType)atoi(argv[2]);
    p.assignment = atoi(argv[3]);
    p.author = argv[4];
    p.owner = argv[5];
    p.filename = argv[6];
    p.location.host = ID_WILDCARD;
    
    if (!(fp = fopen(argv[1], "r"))) {
	fprintf(stderr, "Can't open file %s\n", argv[1]);
	return;
    }
    
    retval = send_file_1(&p, cl);
    if (!retval) {
	show_rpc_error(cl);
	fclose(fp);
	return;
    }
    if (*retval) {
	fclose(fp);
	ss_perror(sci_idx, *retval, "send_file_1");
	return;
    }

    do {
	data.size = fread(data.data, 1, MAX_BURST_SIZE, fp);
	retval = send_burst_1(&data, cl);
	if (!retval) {
	    show_rpc_error(cl);
	    fclose(fp);
	    return;
	}
	if (*retval) {
	    fclose(fp);
	    ss_perror(sci_idx, *retval, "send_burst_1");
	    return;
	}
    } while (data.size == MAX_BURST_SIZE);
    
    retval = end_send_1(&dummy, cl);
    if (!retval) {
	show_rpc_error(cl);
	fclose(fp);
	return;
    }
    if (*retval) {
	fclose(fp);
	ss_perror(sci_idx, *retval, "end_file_1");
	return;
    }

    fclose(fp);
}

cmd_retrieve(argc, argv)
    int argc;
    char *argv[];
{
    FILE *fp;
    long *retval;
    int dummy;
    Paper p;
    retrieve_res *res;
    
    if (argc != 10 && argc != 2) {
	fprintf(stderr, "Usage: get <localfile> [<type> <assgn> <author> <owner> <file> <host> <sec> <usec>]\n");
	return;
    }
    if (argc == 10) {
	p.type = (PaperType)atoi(argv[2]);
	p.assignment = atoi(argv[3]);
	p.author = argv[4];
	p.owner = argv[5];
	p.filename = argv[6];
	p.location.host = argv[7];
	p.location.time.tv_sec = atoi(argv[8]);
	p.location.time.tv_usec = atoi(argv[9]);
    }
    else
	select_paper(&p);
    
    if (!(fp = fopen(argv[1], "w"))) {
	fprintf(stderr, "Can't open file %s\n", argv[1]);
	return;
    }
    
    retval = retrieve_file_1(&p, cl);
    if (!retval) {
	show_rpc_error(cl);
	fclose(fp);
	return;
    }
    if (*retval) {
	fclose(fp);
	ss_perror(sci_idx, *retval, "retrieve_file_1");
	return;
    }

    do {
	res = retrieve_burst_1(&dummy, cl);
	if (!res) {
	    show_rpc_error(cl);
	    fclose(fp);
	    return;
	}
	if (res->errno) {
	    fclose(fp);
	    ss_perror(sci_idx, res->errno, "retrieve_burst_1");
	    return;
	}
	fwrite(res->retrieve_res_u.burst.data, 1,
	       res->retrieve_res_u.burst.size, fp);
    } while (res->retrieve_res_u.burst.size == MAX_BURST_SIZE);

    fclose(fp);
}

cmd_portion(argc, argv)
    int argc;
    char *argv[];
{
    FILE *fp;
    int dummy;
    long *retval;
    portionspec portion;
    retrieve_res *res;
    
    if (argc != 12 && argc != 4) {
	fprintf(stderr, "Usage: get <localfile> <start> <end> [<type> <assgn> <author> <owner> <file>\n        <host> <sec> <usec>]\n");
	return;
    }
    if (argc == 12) {
	portion.p.type = (PaperType)atoi(argv[4]);
	portion.p.assignment = atoi(argv[5]);
	portion.p.author = argv[6];
	portion.p.owner = argv[7];
	portion.p.filename = argv[8];
	portion.p.location.host = argv[9];
	portion.p.location.time.tv_sec = atoi(argv[10]);
	portion.p.location.time.tv_usec = atoi(argv[11]);
    }
    else
	select_paper(&portion.p);
    
    if (!(fp = fopen(argv[1], "w"))) {
	fprintf(stderr, "Can't open file %s\n", argv[1]);
	return;
    }

    portion.start = atoi(argv[2]);
    portion.end = atoi(argv[3]);
    
    retval = portion_1(&portion, cl);
    if (!retval) {
	show_rpc_error(cl);
	fclose(fp);
	return;
    }
    if (*retval) {
	fclose(fp);
	ss_perror(sci_idx, *retval, "retrieve_file_1");
	return;
    }

    do {
	res = retrieve_burst_1(&dummy, cl);
	if (!res) {
	    show_rpc_error(cl);
	    fclose(fp);
	    return;
	}
	if (res->errno) {
	    fclose(fp);
	    ss_perror(sci_idx, res->errno, "retrieve_burst_1");
	    return;
	}
	fwrite(res->retrieve_res_u.burst.data, 1,
	       res->retrieve_res_u.burst.size, fp);
    } while (res->retrieve_res_u.burst.size == MAX_BURST_SIZE);

    fclose(fp);
}

cmd_copy(argc, argv)
    int argc;
    char *argv[];
{
    Paper src, dest;
    TwoPaper p;
    long *retval;
    

    if (argc != 9) {
	fprintf(stderr, "Usage: copy {dest:} <type> <assgn> <author> <owner> <file> <host> <sec> <usec>]\n");
	return;
    }
    dest.type = (PaperType)atoi(argv[1]);
    dest.assignment = atoi(argv[2]);
    dest.author = argv[3];
    dest.owner = argv[4];
    dest.filename = argv[5];
    dest.location.host = argv[6];
    dest.location.time.tv_sec = atoi(argv[7]);
    dest.location.time.tv_usec = atoi(argv[8]);
    
    printf("Source:\n");
    select_paper(&src);

    p.src = src;
    p.dest = dest;
    
    retval = copy_1(&p, cl);
    if (!retval) {
	show_rpc_error(cl);
	return;
    }
    if (*retval) {
	ss_perror(sci_idx, *retval, "copy_1");
	return;
    }
}

cmd_move(argc, argv)
    int argc;
    char *argv[];
{
    Paper src, dest;
    TwoPaper p;
    long *retval;
    

    if (argc != 9) {
	fprintf(stderr, "Usage: move {dest:} <type> <assgn> <author> <owner> <file> <host> <sec> <usec>]\n");
	return;
    }
    dest.type = (PaperType)atoi(argv[1]);
    dest.assignment = atoi(argv[2]);
    dest.author = argv[3];
    dest.owner = argv[4];
    dest.filename = argv[5];
    dest.location.host = argv[6];
    dest.location.time.tv_sec = atoi(argv[7]);
    dest.location.time.tv_usec = atoi(argv[8]);
    
    printf("Source:\n");
    select_paper(&src);

    p.src = src;
    p.dest = dest;
    
    retval = move_1(&p, cl);
    if (!retval) {
	show_rpc_error(cl);
	return;
    }
    if (*retval) {
	ss_perror(sci_idx, *retval, "move_1");
	return;
    }
}

cmd_delete(argc, argv)
    int argc;
    char *argv[];
{
    long *retval;
    Paper p;
    
    if (argc != 9 && argc != 1) {
	fprintf(stderr, "Usage: delete <type> <assgn> <author> <owner> <file> <host> <sec> <usec>\n");
	return;
    }
    if (argc == 9) {
	p.type = (PaperType)atoi(argv[1]);
	p.assignment = atoi(argv[2]);
	p.author = argv[3];
	p.owner = argv[4];
	p.filename = argv[5];
	p.location.host = argv[6];
	p.location.time.tv_sec = atoi(argv[7]);
	p.location.time.tv_usec = atoi(argv[8]);
    }
    else
	select_paper(&p);
    
    retval = delete_1(&p, cl);
    if (!retval) {
	show_rpc_error(cl);
	return;
    }
    if (*retval) {
	ss_perror(sci_idx, *retval, "delete_1");
	return;
    }
}

cmd_kill(argc, argv)
    int argc;
    char *argv[];
{
    long *retval;
    int dummy;

    retval = kill_server_1(&dummy, cl);
    if (!retval) {
	show_rpc_error(cl);
	return;
    }
    if (*retval)
	ss_perror(sci_idx, *retval, "calling kill_server_1");
}

cmd_stats(argc, argv)
    int argc;
    char *argv[];
{
    server_stats *retval;
    int dummy;
    struct tm *tim;
    
    retval = server_stats_1(&dummy, cl);
    if (!retval) {
	show_rpc_error(cl);
	return;
    }
    printf("Start time:     %s", ctime(&retval->start_time));
    tim = (struct tm *)gmtime(&retval->uptime);
    printf("Uptime:         %d days, %02d:%02d:%02d\n", tim->tm_yday,
	   tim->tm_hour, tim->tm_min, tim->tm_sec);
    printf("Total requests: %-10d\n", retval->num_requests);
    printf("Bytes sent:     %-10d  ", retval->bytes_sent);
    printf("Bytes received: %-10d\n", retval->bytes_recv);
    printf("init:           %-10d  ", retval->n_init);
    printf("list_acl:       %-10d\n", retval->n_list_acl);
    printf("add_acl:        %-10d  ", retval->n_add_acl);
    printf("delete_acl:     %-10d\n", retval->n_delete_acl);
    printf("create_course:  %-10d  ", retval->n_create_course);
    printf("delete_course:  %-10d\n", retval->n_delete_course);
    printf("list:           %-10d  ", retval->n_list);
    printf("send_file:      %-10d\n", retval->n_send_file);
    printf("send_burst:     %-10d  ", retval->n_send_burst);
    printf("end_send:       %-10d\n", retval->n_end_send);
    printf("retrieve_file:  %-10d  ", retval->n_retrieve_file);
    printf("retrieve_burst: %-10d\n", retval->n_retrieve_burst);
    printf("copy:           %-10d  ", retval->n_copy);
    printf("move:           %-10d\n", retval->n_move);
    printf("delete:         %-10d  ", retval->n_delete);
    printf("krb_info:       %-10d\n", retval->n_krb_info);
    printf("kill_server:    %-10d  ", retval->n_kill_server);
    printf("server_stats:   %-10d\n", retval->n_server_stats);
    printf("server_quorum:  %-10d  ", retval->n_server_quorum);
    printf("server_store:   %-10d\n", retval->n_server_store);
    printf("server_delete:  %-10d  ", retval->n_server_delete);
    printf("server_commit:  %-10d\n", retval->n_server_commit);
    printf("server_end_crs: %-10d  ", retval->n_server_end_course);
    printf("server_requpdat:%-10d\n", retval->n_server_requpdate);
    printf("server_startupd:%-10d  ", retval->n_server_start_upd);
    printf("server_end_upd: %-10d\n", retval->n_server_end_upd);
    printf("DB vers:        %ld/%ld\n", retval->vers.synctime,
	   retval->vers.commit);
}

show_rpc_error(cl)
    CLIENT *cl;
{
    struct rpc_err err;

    CLNT_GETERR(cl, &err);
    ss_perror(sci_idx, (long)err.re_status+(long)rpc_err_base, "making RPC call");
}
