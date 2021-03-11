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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/alquery.c,v 1.20 1993/07/13 20:21:06 Zarf Exp $";
#endif


#ifndef TESTINGONLYTESTING
/*
#define DEBUG   1
#define DEBUG_1 1
*/
#endif /* TESTINGONLYTESTING */

 

/* Notes:

  Need to add support for "System:Administrator".

  The names of groups owned by System can have their prefixes omitted.  
  Thus "System:AllStudents" has the alias "AllStudents."  To avoid 
  ambiguity, usernames must be distinct from the suffixes of the 
  groups owned by System.
  
  In afs/prserver, prclient.c and prs.c both use pruser.c
  
  Assume user is authenticatable in usercell (unless anonymous).
  
  A group cannot contain groups as members.
      
*/



/**********************
 *                    *
 * Included Headers   *
 *                    *
 **********************/

#include <stdio.h>
#include <andrewos.h>		/* sys/time.h */
#include <svcconf.h>
#include <errno.h>
extern int errno;
#ifdef AFS30_ENV
#include <netinet/in.h>
#include <afs/acl.h>
#ifdef CMUCS
#define ERROR_TABLE_BASE ERROR_TABLE_BASE_pt
#include <afs/ptclient.h>
#include <afs/pterror.h>
#include <afs/ptint.h>
#else
#define ERROR_TABLE_BASE ERROR_TABLE_BASE_pr
#include <afs/prclient.h>
#include <afs/prerror.h>
#include <afs/print.h>
#endif
#include <afs/venus.h>
/* #include <afs/prs.h> */
#include <afs/cellconfig.h>
#include <afs/prs_fs.h>
#define ALLRIGHTS (PRSFS_READ | PRSFS_WRITE | PRSFS_INSERT | PRSFS_LOOKUP | PRSFS_DELETE | PRSFS_LOCK | PRSFS_ADMINISTER)
#else /* AFS30_ENV */
#include <sys/types.h>
#endif /* AFS30_ENV */
#include <sys/param.h>
#include <util.h>


/**********************
 *                    *
 * Macro Definitions  *
 *                    *
 **********************/

#define ALQ_Decode_Error(x) \
  ((EH_module(x)==EH_module_alq)?(-EH_code(x)): \
   (EH_module(x)==EH_module_prs)?-(ALQ_EPRS_BASE + EH_code(x)):-ALQ_ERRNO)
   

/**********************
 *                    *
 * Internal Datatypes *
 *                    *
 **********************/

#ifdef AFS30_ENV

typedef struct id_and_name {
    char *name;			/* these two fields are lazy evaluated */
    long id;			/* (provided you give one or the other) */
    int is_id;
    int is_name;
    char *cell;
    /* accessor functions: */
#define GET_ID(x) (((x)->is_id)?(x)->id:((x)->is_id=1,(x)->id=NameToID((x)->name,(x)->cell)))
#define SET_ID(x,y) (((x)->is_id = 1), ((x)->id = (y)))

#define GET_NAME(x) (((x)->is_name)?(x)->name:((x)->is_name=1,(x)->name=IDToName((x)->id,(x)->cell)))
#define SET_NAME(x,y) (((x)->is_name = 1), ((x)->name = (y)))

#define GET_CELL(x) (((x)->cell)?(x)->cell:NULL)
#define SET_CELL(x,y) ((x)->cell = (y))
} *id_and_name_t;


typedef struct access_for_user {
    id_and_name_t user;
    long rights;
} *access_for_user_t;

typedef struct access_list {
    access_for_user_t pos_rights, neg_rights;
    int num_pos, num_neg;
} *access_list_t;



/**********************
 *                    *
 * Internal Globals   *
 *                    *
 **********************/
static char *ALQ_Error_Message;



/**********************
 *                    *
 * Internal Functions *
 *                    *
 **********************/

static id_and_name_t New_id_and_name()
{
  /* Returns a new, initialized to empty, id_and_name_t type. 
     Will not return NULL.
     Failures are signalled. */
    id_and_name_t ret;

    ret = (id_and_name_t)emalloc(sizeof(*ret));
    ret->cell = NULL;
    ret->name = NULL;
    ret->id = 0;
    ret->is_id = 0;
    ret->is_name = 0;

    return(ret);
}

static void Free_id_and_name(in)
id_and_name_t in;
{
    if (in == NULL) return;
    if (in->cell) free(in->cell);
    if (in->name) free(in->name);
    free(in);
}

static id_and_name_t New_ID(id, cell)
long id;
char *cell;
{
  /* Builds a new id_and_name_t element, with the id and cell
     fields initialized.
     Passes errors up. */
    id_and_name_t ret;

    ret = New_id_and_name();
    SET_ID(ret, id);
    SET_CELL(ret, CopyString(cell));

    return(ret);
}

static id_and_name_t New_Name(name, cell)
char *name, *cell;
{
  /* Builds a new id_and_name_t element, with the name and cell
     fields initialized.
     Passes errors up. */
    id_and_name_t ret;

    ret = New_id_and_name();
    SET_NAME(ret, CopyString(name));
    SET_CELL(ret, CopyString(cell));

    return(ret);
}

static long NameToID(name, cell)
char *name, *cell;
{
  /* Returns the id of a name in a cell.
     Will not return NULL.
     Errors are signalled. */
    long id;
    int code;

    EH_cond_error_on((name==NULL),
		     EH_ret_code(EH_module_alq, ALQ_ENONAME), 
		     "Cannot translate null name to ID");
    EH_cond_error_on 
      (
       code =  pr_Initialize(0,AFSCONF_CLIENTNAME,cell),
       EH_ret_code(EH_module_prs, (code - ERROR_TABLE_BASE)),
       (sprintf(EH_Error_Msg, "pr_Initialize(%d, %s, %s) failed, code %d '%s'",
		 0, AFSCONF_CLIENTNAME, cell, code, pr_ErrorMsg(code)),
	EH_Error_Msg)
       );
    
    EH_cond_error_on
      (
       code = pr_SNameToId(name, &id),
       EH_ret_code(EH_module_prs, (code - ERROR_TABLE_BASE)),
       (sprintf(EH_Error_Msg, "pr_SNameToId(%s, &id) failed, code %d '%s'",
		 name, code, pr_ErrorMsg(code)),
	EH_Error_Msg)
       );

#ifdef AQ_FLAGNONUSERS
    if (id == ANONYMOUSID) {
      if (strcmp(name, IDToName(ANONYMOUSID, cell)) != 0) {
	EH_err(EH_ret_code(EH_module_prs, (PRNOENT - ERROR_TABLE_BASE)), 
	       (sprintf(EH_Error_Msg, "No such user or group as '%s'", name),
		EH_Error_Msg));
      }
    }
#endif /* AQ_FLAGNONUSERS */

    return(id);
}

static char *IDToName(id, cell)
long id;
char *cell;
{
  /* Returns the name of an id in a cell.
     Errors are signalled. */

    char name[PR_MAXNAMELEN+1];
    int code;

    EH_cond_error_on
      (
       code = pr_Initialize(0,AFSCONF_CLIENTNAME,cell),
       EH_ret_code(EH_module_prs, (code - ERROR_TABLE_BASE)),
       (sprintf(EH_Error_Msg, "pr_Initialize(%d, %s, %s) failed, code %d '%s'",
		 0, AFSCONF_CLIENTNAME, cell, code, pr_ErrorMsg(code)),
	EH_Error_Msg)
       );

    EH_cond_error_on
      (
       code = pr_SIdToName(id, name),
       EH_ret_code(EH_module_prs, (code - ERROR_TABLE_BASE)),
       (sprintf(EH_Error_Msg, "pr_SIdToName(%ld, name) failed, code %d '%s'",
		 id, code, pr_ErrorMsg(code)), 
	EH_Error_Msg)
       );

    return(CopyString(name));
}

static access_list_t New_access_list(pos, neg)
int pos, neg;
{
    access_list_t ret;
    int i;

    ret = (access_list_t)emalloc(sizeof(*ret));
    ret->num_pos = pos;
    ret->num_neg = neg;

    if (pos>0) {
      EH_begin
	ret->pos_rights = (access_for_user_t)emalloc((pos+1)*sizeof(struct access_for_user));
      EH_handle
	free(ret);
	EH_propagate_error("while allocating memory for set of positive rights");
      EH_end;
    } else {
      ret->pos_rights = NULL;
    }

    if (neg>0) {
      EH_begin
	ret->neg_rights = (access_for_user_t)emalloc((neg+1)*sizeof(struct access_for_user));
      EH_handle
	   if (ret->pos_rights) free(ret->pos_rights);
	   free(ret);
	   EH_propagate_error("while allocating memory for set of negative rights");
      EH_end;
    } else {
      ret->neg_rights = NULL;
    }
    
    for(i=0; i<pos; ++i){
	int j;
	ret->pos_rights[i].rights = 0L;
	EH_begin
	  ret->pos_rights[i].user = New_id_and_name();
	EH_handle
	  for(j=0; j<i; ++j) Free_id_and_name(ret->pos_rights[j].user);
	  if (ret->pos_rights) free(ret->pos_rights);
	  if (ret->neg_rights) free(ret->neg_rights);
	  free(ret);
	  EH_propagate_error("while allocating memory for a user element of positive rights");
	EH_end;
    }
    for(i=0; i<neg; ++i){
	int j;
	ret->neg_rights[i].rights = 0L;
	EH_begin
	  ret->neg_rights[i].user = New_id_and_name();
	EH_handle
	    for(j=0; j<pos; ++j) Free_id_and_name(ret->pos_rights[j].user);
	    for(j=0; j<i; ++j) Free_id_and_name(ret->neg_rights[j].user);
	    if (ret->pos_rights) free(ret->pos_rights);
	    if (ret->neg_rights) free(ret->neg_rights);
	    free(ret);
	    EH_propagate_error("allocating memory for a user element of negative rights");
	EH_end;
    }
    return(ret);
}

static void Free_access_list(al)
access_list_t al;
{
    int i;

    if (al == NULL) return;

    for(i=0; i<al->num_pos; ++i) Free_id_and_name(al->pos_rights[i].user);
    for(i=0; i<al->num_neg; ++i) Free_id_and_name(al->neg_rights[i].user);
    if (al->pos_rights) free(al->pos_rights);
    if (al->neg_rights) free(al->neg_rights);
    free(al);

    return;
}

static long Pos_Rights_Of(id, rights)
long id;
access_list_t rights;
{
  /* Return the positive rights of the id in the access list.
     May encounter errors (via GET_ID) which are not caught and
     are passed up. */
    int i;

    for(i=0; i < rights->num_pos; ++i) {
	if (id == GET_ID(rights->pos_rights[i].user))
	    return(rights->pos_rights[i].rights);
    }
    return(0L);
}

static long Neg_Rights_Of(id, rights)
long id;
access_list_t rights;
{
  /* Return the negative rights of the id in the access list.
     May encounter errors (via GET_ID) which are not caught and
     are passed up. */
    int i;

    for(i=0; i < rights->num_neg; ++i) {
	if (id == GET_ID(rights->neg_rights[i].user))
	    return(rights->neg_rights[i].rights);
    }
    return(0L);
}

static int Group_P(name)
id_and_name_t name;
{
  /* Returns true if the name is a group name (its numeric id is negative),
     else 0, implying a user name.
     May encounter errors (via GET_ID) which are not caught and 
     are passed up. */

    return(GET_ID(name)<0L);
}

static char *GroupMembers(name)
id_and_name_t name;
{
  /* Returns a newline delimited list of members of the group.
     Signals errors. */
    namelist out;
    int i, len;
    char *ret;
    int code;

    out.namelist_len = 0;
    out.namelist_val = (prname *) 0;
    EH_cond_error_on(code = pr_Initialize(0,AFSCONF_CLIENTNAME,GET_CELL(name)),
		     EH_ret_code(EH_module_prs, (code - ERROR_TABLE_BASE)),
		     (sprintf(EH_Error_Msg, "pr_Initialize(%d, %s, %s) failed, code %d '%s'", 0, AFSCONF_CLIENTNAME, GET_CELL(name), code, pr_ErrorMsg(code)),
		      EH_Error_Msg));

    EH_cond_error_on(code = pr_IDListMembers(GET_ID(name), &out),
		     EH_ret_code(EH_module_prs, (code - ERROR_TABLE_BASE)),
		     (sprintf(EH_Error_Msg, "pr_IDListMembers(%ld, &out) failed, code %d '%s'", GET_ID(name), code, pr_ErrorMsg(code)),
		      EH_Error_Msg));

#ifdef DEBUG
    fprintf(stderr,
	     CARDINALIZE(out.namelist_len,
			 "There are no group members.\n",
			 "There is one group member.\n",
			 "There are %d group members.\n"),
	     out.namelist_len);
#endif /* DEBUG */
    for (len = 0, i=0; i<out.namelist_len; ++i)
	len += strlen(out.namelist_val[i]);

    EH_begin
      ret = (char *)emalloc(len+out.namelist_len+1);
    EH_handle
      if (out.namelist_val) free(out.namelist_val);
      EH_propagate_error("while allocating memory for group members");
    EH_end;

    ret[0] = '\0';
    for (i=0; i<out.namelist_len; ++i) {
#ifdef DEBUG
	fprintf(stderr, "The %d%s member is ``%s''.\n", 
		i+1, ORDINALIZE(i+1), out.namelist_val[i]);
#endif /* DEBUG */
	if (i != 0) strcat(ret, "\n");
	strcat(ret, out.namelist_val[i]);
    }
    if (out.namelist_val) free(out.namelist_val);
    return(ret);
}

static int Member_of_Group_P(user, group)
id_and_name_t user, group;
{
  /* Returns 1 if name is a member of group, 0 if not.
     Signals errors. */
    namelist out;
    int i;
    int code;

    out.namelist_len = 0;
    out.namelist_val = (prname *) 0;
    EH_cond_error_on(code = pr_Initialize(0,AFSCONF_CLIENTNAME,GET_CELL(group)),
		     EH_ret_code(EH_module_prs, (code - ERROR_TABLE_BASE)),
		     (sprintf(EH_Error_Msg, "pr_Initialize(%d, %s, %s) failed, code %d '%s'", 0, AFSCONF_CLIENTNAME, GET_CELL(group), code, pr_ErrorMsg(code)),
		      EH_Error_Msg));
    EH_cond_error_on(code = pr_IDListMembers(GET_ID(group), &out),
		     EH_ret_code(EH_module_prs, (code - ERROR_TABLE_BASE)),
		     (sprintf(EH_Error_Msg, "pr_IDListMembers(%ld, &out) failed, code %d 's'", GET_ID(group), code, pr_ErrorMsg(code)),
		      EH_Error_Msg));
    EH_begin
      if (out.namelist_len > 0) {
	for (i=0; i<out.namelist_len; ++i)
	  if ((strcmp(GET_NAME(user), out.namelist_val[i])) == 0)
		return(1);
	free(out.namelist_val);
      }
    EH_handle
				/* GET_NAME may signal an error */
      if ((out.namelist_len > 0) && (out.namelist_val != NULL)) free(out.namelist_val);
      EH_propagate_error("while checking membership in a group");
    EH_end;
    return(0);
}

static char *GetACL(pathname)
char *pathname;
{
  /* Returns the access control string of the directory pathname.
     Errors are signalled. */
    struct ViceIoctl blob;
    char al[2*(ACL_MAXENTRIES+1)*(PR_MAXNAMELEN+1)];

    blob.in = NULL;
    blob.in_size = 0;
    blob.out_size = sizeof(al);
    blob.out = al;
    EH_cond_error_on(pioctl(pathname, _VICEIOCTL(2) /*VIOCGETAL*/, &blob, 1),
		     EH_ret_code(EH_module_system, errno), 
		     "VIOCGETAL error");
    return(CopyString(al));
}

static access_list_t ParseACL(acl, cell)
char *acl, *cell;
{
  /* Parses an access control list (see GetACL), for cell, returning an
     access list.
     Errors are signalled. */
    access_list_t ret;
    char namebuf[1+PR_MAXNAMELEN];
    int pos, neg;
    int i;

    EH_cond_error_on(sscanf(acl,"%d\n%d\n",&pos,&neg) < 2,
		     EH_ret_code(EH_module_alq, ALQ_EPARSEACL), 
		     "Bad ACL format (parsing num_pos, num_neg)");
    ret = New_access_list(pos, neg);
    while((*acl) && (*(++acl) != '\n')); /* skip to first newline */
    while((*acl) && (*(++acl) != '\n'));	/* skip to second newline */

    for(i = 0; i < pos; ++i) {
      if(sscanf(acl, "\n%s\t%ld", namebuf, &(ret->pos_rights[i].rights)) < 2) {
	Free_access_list(ret);
	EH_err(EH_ret_code(EH_module_alq, ALQ_EPARSEACL), 
	       "Bad ACL format (parsing pos elem)");
      }
      SET_NAME(ret->pos_rights[i].user, CopyString(namebuf));
      SET_CELL(ret->pos_rights[i].user, CopyString(cell));
      while((*acl) && (*(++acl) != '\n'));	/* skip to next newline */
    }

    for(i = 0; i < neg; ++i) {
      if(sscanf(acl, "\n%s\t%ld", namebuf, &(ret->neg_rights[i].rights)) < 2) {
	Free_access_list(ret);
	EH_err(EH_ret_code(EH_module_alq, ALQ_EPARSEACL), 
	       "Bad ACL format (parsing neg elem)");
      }
      SET_NAME(ret->neg_rights[i].user, CopyString(namebuf));
      SET_CELL(ret->neg_rights[i].user, CopyString(cell));
      while((*acl) && (*(++acl) != '\n'));	/* skip to next newline */
    }

    return(ret);
}

#ifdef DEBUG_1
static void DumpAL(f, al)
FILE *f;
access_list_t al;
{
    int i;
    char *gm;

    for(i = 0; i<al->num_pos; ++i) {
	fprintf(f, "'%s@%s' (id %ld) access 0x%lx\n",
		GET_NAME(al->pos_rights[i].user),
		GET_CELL(al->pos_rights[i].user),
		GET_ID(al->pos_rights[i].user),
		al->pos_rights[i].rights);
	if (Group_P(al->pos_rights[i].user)) {
	    gm = GroupMembers(al->pos_rights[i].user);
	    fprintf(f,"\tGroup members: '%s'.\n", gm?gm:"<none>");
	    if (gm) free(gm);
	}
    }
    for(i = 0; i<al->num_neg; ++i) {
	fprintf(f, "'%s@%s' (id %ld) negative access 0x%lx\n",
		GET_NAME(al->pos_rights[i].user),
		GET_CELL(al->pos_rights[i].user),
		GET_ID(al->neg_rights[i].user),
		al->neg_rights[i].rights);
	if (Group_P(al->pos_rights[i].user)) {
	    gm = GroupMembers(al->pos_rights[i].user);
	    fprintf(f,"\tGroup members: '%s'.\n", gm?gm:"<none>");
	    if (gm) free(gm);
	}
    }
}
#endif /* DEBUG_1 */

static long int Do_Negative_Rights(rights_list, testrights, username, dircell)
access_list_t rights_list;
long int testrights;
id_and_name_t username;
char *dircell;
{/* do_negative: calculate negative rights */

    long int neg_userrights = 0;
    int i;

    /* done? == if (-userrights == testrights) 
	--> return(-userrights) */
#define DONEP if (neg_userrights == testrights) return(neg_userrights);

    /* if (empty -rights) --> return(-userrights); */
    if (rights_list->num_neg == 0)
	return(neg_userrights);

    /* if (not (equal dircell usercell)), 
	  --> -userrights = (rightsof "system:anyuser" -rights)
	  | (rightsof "anonymous", -rights), 
	  return(-userrights); */
    if (ULstrcmp(GET_CELL(username), dircell)!=0) {
	neg_userrights = Neg_Rights_Of(ANYUSERID, rights_list)
	  | Neg_Rights_Of(ANONYMOUSID, rights_list);
	return(neg_userrights);
    }

    /* -userrights = (rightsof "system:anyuser" -rights), done? */
    neg_userrights = Neg_Rights_Of(ANYUSERID, rights_list);
    DONEP;

    /* if (not (equal user "anonymous")) 
	--> -userrights |= (rightsof "system:authuser" -rights), done? */
    if (GET_ID(username) != ANONYMOUSID) {
	neg_userrights |= Neg_Rights_Of(AUTHUSERID, rights_list);
	DONEP;
    }

    /* if (member user -rights),
	  --> -userrights |= (rightsof user -rights), done? */
    neg_userrights |= Neg_Rights_Of(GET_ID(username), rights_list);
    DONEP;

    /* foreach (group in (groupsin -rights))
      if (member user (membersof group))
	  --> -userrights |= (rightsof group -rights), done? */
    for (i=0; i < rights_list->num_neg; ++i) {
	if (Group_P(rights_list->neg_rights[i].user)) {
	    if (Member_of_Group_P(username, rights_list->neg_rights[i].user)) {
		neg_userrights |= rights_list->neg_rights[i].rights;
		DONEP;
	    }
	}
    }
#undef DONEP
    return(neg_userrights);
}

static long Do_Positive_Rights(rights_list, neg_userrights, testrights, anyflag, username, dircell)
access_list_t rights_list;
long int neg_userrights, testrights;
int anyflag;
id_and_name_t username;
char *dircell;
{

    long int pos_userrights = 0;
    int i;

    /*
      do_positive: calculate positive rights
	    enough? == if (anyflag)
		--> if (((+userrights & ~(-userrights)) & testrights) > 0)
		    --> return(+userrights).
		else if ((+userrights & ~(-userrights)) == testrights)
		    --> return(+userrights). */
#define ENOUGHP \
    if (anyflag) { \
      if (((pos_userrights & ~(neg_userrights)) & testrights) > 0) { \
	return(pos_userrights); \
      } \
    } else { \
      if ((pos_userrights & ~(neg_userrights)) == testrights) \
	return(pos_userrights); \
    }
    
    /*  20- if (not (equal dircell usercell))  ;no cross-cell memberships (yet)
	--> +userrights = (rightsof "system:anyuser" +rights)
	| (rightsof "anonymous", +rights), 
	return(+userrights). */
    if (ULstrcmp(GET_CELL(username), dircell)!=0) {
      pos_userrights = Pos_Rights_Of(ANYUSERID, rights_list)
	| Pos_Rights_Of(ANONYMOUSID, rights_list);
      return(pos_userrights);
    }
    
    /*  21- +userrights = (rightsof "system:anyuser" +rights), enough?
	;everyone gets anyuser rights */
    pos_userrights = Pos_Rights_Of(ANYUSERID, rights_list);
    ENOUGHP;
    
    /*  22- if (not (equal user "anonymous")) ;anonymous doesn't get authuser rights
	--> +userrights |= (rightsof "system:authuser" +rights), enough? */
    if (GET_ID(username) != ANONYMOUSID) {
      pos_userrights |= Pos_Rights_Of(AUTHUSERID, rights_list);
      ENOUGHP;
    }
    
    /*  23- if (member user +rights),                              ;explict mention?
	--> +userrights |= (rightsof user +rights), enough? */
    pos_userrights |= Pos_Rights_Of(GET_ID(username), rights_list);
    ENOUGHP;
    
    /*  24- foreach (group in (groupsin +rights))                ;implicit by group?
	;what to do about groups as members of groups?
	if (member user (membersof group))
	--> +userrights |= (rightsof group +rights), enough?
	*/
    for (i=0; i < rights_list->num_pos; ++i) {
      if (Group_P(rights_list->pos_rights[i].user)) {
	if (Member_of_Group_P(username, rights_list->pos_rights[i].user)) {
	  pos_userrights |= rights_list->pos_rights[i].rights;
	  ENOUGHP;
	}
      }
    }
#undef ENOUGHP
    return(pos_userrights);
}

static long int CheckRights(user, usercell, dir, testrights, anyflag)
char *user, *usercell, *dir;
long int testrights;
int anyflag;
{ /* Generalized version of User*RightsToDir, returns rights user has,
     modulo testrights, and (possibly) abbreviated if anyflag.
     Errors are signalled. */

    long pos_userrights = 0;
    long neg_userrights = 0;
    char *acl = NULL;
    access_list_t aclist = NULL;
    id_and_name_t userid = NULL;
    char dircell[MAXPATHLEN+1];

    EH_begin
      EH_cond_error_on(GetCellFromFileName(dir, dircell, sizeof(dircell)),
		       EH_ret_code(EH_module_system, errno),
		       "GetCellFromFileName failed");
      acl = GetACL(dir);
      aclist = ParseACL(acl, dircell);
      free(acl);
      acl = NULL;
      userid = New_Name(user, usercell);
      neg_userrights = Do_Negative_Rights(aclist, testrights, userid, dircell);
      if (neg_userrights != testrights) {
	pos_userrights = Do_Positive_Rights(aclist, neg_userrights, testrights, anyflag, userid, dircell);
      }
      Free_access_list(aclist);
      Free_id_and_name(userid);
      /*
	done:  subtract off negative rights
	30- return((+userrights & ~(-userrights)) & testrights)
	*/
      EH_return_val((pos_userrights & ~(neg_userrights)) & testrights);
    EH_handle
      if (acl) free(acl);
      if (aclist) Free_access_list(aclist);
      if (userid) Free_id_and_name(userid);
      EH_propagate_error("while determining rights of user on directory");
    EH_end;
}


/**********************
 *                    *
 * Exported Functions *
 *                    *
 **********************/

int aq_GroupP(group, groupcell)
char *group, *groupcell;
{ /* Returns 1 if the name "group" is a groupname, 0 if not, <0 on errors. */
    id_and_name_t user = NULL;
    int ret;

    EH_begin
      user = New_Name(group, groupcell);
      ret = Group_P(user);
      Free_id_and_name(user);
      EH_return_val(ret?1:0);
    EH_handle
      if (user) Free_id_and_name(user);
      ALQ_Error_Message = EH_Error_Msg;
      EH_return_val(ALQ_Decode_Error(_err_code));
    EH_end;
}

int aq_GetGroupMembers(group, groupcell, outBuf)
char *group, *groupcell, **outBuf;
{/* Return the members of the given group as a newline-separated list in outBuf, which is modified to point to a malloc'd string.  The return value is 0 if all is OK and negative on errors: a return value of -1 means to look in errno. */
    id_and_name_t user = NULL;
    char *ret = NULL;
    int GroupID;

    EH_begin
      user = New_Name(group, groupcell);
      if (!Group_P(user)) {
	  Free_id_and_name(user);
	  ALQ_Error_Message = "Given name not an AFS group name";
	  EH_return_val(-ALQ_NOT_GROUP);
      }
      GroupID = GET_ID(user);
      if (GroupID == ANYUSERID || GroupID == AUTHUSERID) {
	  Free_id_and_name(user);
	  ALQ_Error_Message = "Cannot enumerate built-in System: groups";
	  EH_return_val(-ALQ_SYSTEM_GROUP);
      }
      ret = GroupMembers(user);
      Free_id_and_name(user);
      *outBuf = ret;
      EH_return_val(0);
    EH_handle
      if (user) Free_id_and_name(user);
      if (ret) free(ret);
      ALQ_Error_Message = EH_Error_Msg;
      EH_return_val(ALQ_Decode_Error(_err_code));
    EH_end;
}

int aq_UserInGroup(user, usercell, group, groupcell)
char *user, *usercell, *group, *groupcell;
{/* Return whether the given user is in the given group in the given cell.  1 means YES, 0 means NO, negative numbers are error codes; -1 means to look in errno. */

    id_and_name_t the_user = NULL, the_group = NULL;
    int ret;

    EH_begin
      if (ULstrcmp(usercell, groupcell) != 0) return 0;
      the_group = New_Name(group, groupcell);
      if (GET_ID(the_group) == ANYUSERID) {
	  Free_id_and_name(the_group);
	  EH_return_val(1);
      }
      the_user = New_Name(user, usercell);
      if (GET_ID(the_group) == AUTHUSERID) {
	  ret = (GET_ID(the_user) != ANONYMOUSID ? 1 : 0);
	  Free_id_and_name(the_user);
	  Free_id_and_name(the_group);
	  EH_return_val(ret);
      }
      ret = Member_of_Group_P(the_user, the_group);
      Free_id_and_name(the_user);
      Free_id_and_name(the_group);
      EH_return_val(ret);
    EH_handle
      if (the_user) Free_id_and_name(the_user);
      if (the_group) Free_id_and_name(the_group);
      ALQ_Error_Message = EH_Error_Msg;
      EH_return_val(ALQ_Decode_Error(_err_code));
    EH_end;
}

long int aq_UserRightsToDir(user, usercell, dir)
char *user, *usercell, *dir;
{/* Return the access rights that the given user has to the given dir.  Negative numbers are error codes; -1 means to look in errno. */
  long int ret;

  EH_begin
    ret = CheckRights(user, usercell, dir, ALLRIGHTS, 0);
    EH_return_val(ret);
  EH_handle
    ALQ_Error_Message = EH_Error_Msg;
    EH_return_val(ALQ_Decode_Error(_err_code));
  EH_end;
}

int aq_CheckUserAllRightsToDir(user, usercell, dir, rights)
char *user, *usercell, *dir; long int rights;
{/* Check whether the given user has all of a collection of rights to the given directory.  Return 1 if YES, 0 if NO; negative numbers are error codes, and -1 means to look in errno. */

  long int retrights;
  
  EH_begin
    retrights = CheckRights(user, usercell, dir, rights, 0);
    EH_return_val((retrights>0)?1:0);
  EH_handle
    ALQ_Error_Message = EH_Error_Msg;
    EH_return_val(ALQ_Decode_Error(_err_code));
  EH_end;
}

int aq_CheckUserAnyRightToDir(user, usercell, dir, rights)
char *user, *usercell, *dir; long int rights;
{/* Check whether the given user has any of a collection of rights to the given directory.  Return 1 if YES, 0 if NO; negative numbers are error codes, and -1 means to look in errno. */

  long int retrights;
  
  EH_begin
    retrights = CheckRights(user, usercell, dir, rights, 1);
    EH_return_val((retrights>0)?1:0);
  EH_handle
    ALQ_Error_Message = EH_Error_Msg;
    EH_return_val(ALQ_Decode_Error(_err_code));
  EH_end;
}

char *aq_GetLastErrorMessage()
{/* Returns the text string of the last error message.  Points to static 
   storage, do NOT free. */

  return(ALQ_Error_Message);
}

#endif /* AFS30_ENV */


/**********************
 *                    *
 * Debugging Tests    *
 *                    *
 **********************/

#ifdef DEBUG_1

main(argc, argv)
int argc;
char *argv[];
{
				/* Tests internal data structures */
    char *acl, *dir;
    access_list_t al;
    char dircell[MAXPATHLEN+1];

    if (argc != 2) {
	fprintf(stderr, "Usage:  alquery dirname\n");
	exit(1);
    }
#ifdef ANDREW_MALLOC_ENV
    (void) SetMallocCheckLevel(3);
#endif /* ANDREW_MALLOC_ENV */
    dir = argv[1];

    if ((GetCellFromFileName(dir, dircell, sizeof(dircell))) != 0) {
	/* error! */
	exit(1);
    }
    EH_begin
      acl = GetACL(dir);
      printf("Access for '%s' in cell '%s' is:\n%s.\n", dir, dircell, acl);
      al = ParseACL(acl, dircell);
      free(acl);
      DumpAL(stdout, al);
      Free_access_list(al);
    EH_handle
      fprintf(stderr, "Bad news: '%s' (module %d, code %d).\n", EH_Error_Msg, EH_module(_err_code), EH_code(_err_code));
      exit(2);
    EH_end;
}
#endif /* DEBUG_1 */

#ifdef TESTINGONLYTESTING

main()
{
				/* Tests exported interfaces */
    char User[1000], Dir[1000], RightB[1000];
    char *Members;
    long int Rights, RC;

#ifdef ANDREW_MALLOC_ENV
    (void) SetMallocCheckLevel(3);
#endif /* ANDREW_MALLOC_ENV */
    CheckServiceConfiguration();
    for (;;) {
	printf("User: "); fflush(stdout);
	if (fgets(User, sizeof(User), stdin) == NULL) break;
	if (User[strlen(User)-1] == '\n') User[strlen(User)-1] = '\0';
	if (User[0] != '\0') {
	    printf("Dir: "); fflush(stdout);
	    if (fgets(Dir, sizeof(Dir), stdin) == NULL) break;
	    if (Dir[strlen(Dir)-1] == '\n') Dir[strlen(Dir)-1] = '\0';
	    printf("Right: "); fflush(stdout);
	    if (fgets(RightB, sizeof(RightB), stdin) == NULL) break;
	    if (RightB[strlen(RightB)-1] == '\n') RightB[strlen(RightB)-1] = '\0';
	    Rights = atol(RightB);
	    RC = aq_CheckUserAnyRightToDir(User, ThisDomain, Dir, Rights);
	    printf("aq_CheckUserAnyRightToDir(%s, %s, %s, %#lo) returns %d (errno %d)\n", User, ThisDomain, Dir, Rights, RC, errno);
	    if (RC >= 0) printf("Thus, %s@%s has %s of the rights %#lo on %s.\n", User, ThisDomain, (RC == 0 ? "none" : "at least one"), Rights, Dir);
	    else printf("Error message was: '%s'.\n", aq_GetLastErrorMessage());
	}
	printf("Group: "); fflush(stdout);
	if (fgets(Dir, sizeof(Dir), stdin) == NULL) break;
	if (Dir[strlen(Dir)-1] == '\n') Dir[strlen(Dir)-1] = '\0';
	if (Dir[0] != '\0') {
	    Members = NULL;
	    RC = aq_GetGroupMembers(Dir, ThisDomain, &Members);
	    if (RC == 0) {
		printf("aq_GetGroupMembers(%s, %s...) returns ``%s''.\n", Dir, ThisDomain, Members);
	    } else {
		printf("aq_GetGroupMembers(%s, %s...) gives error %d (errno %d).\n", Dir, ThisDomain, RC, errno);
		printf("Error message was: '%s'.\n", aq_GetLastErrorMessage());
	    }
	    if (Members != NULL) free(Members);
	}
    }
}
#endif /* TESTINGONLYTESTING */
