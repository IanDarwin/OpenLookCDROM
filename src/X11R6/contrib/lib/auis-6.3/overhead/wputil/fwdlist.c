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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/wputil/RCS/fwdlist.c,v 1.7 1994/03/01 23:00:39 rr2b Exp $";
#endif

/*

  fwdlist.c -- Forward File Lister
    This program generates a list of all users with $HOME/.foward files.

*/

#include <fcntl.h>
#include <stdio.h>
#include <wp.h>
#include <svcconf.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <util.h>

#define WHITESPACE " \b\f\n\r\t\v"

#ifndef DONTLOGPROGRESS
#define LOGTHRESHHOLD 50	/* the period of the progress message, in users */
#endif /* DONTLOGPROGRESS */

#define NoState 0
#define OpenedFile 1
#define FailedToOpenFile 2
#define StatedFile 3
#define FailedToStatFile 4
#define ReadFile 5
#define FailedToReadFile 6

#define NOERR 0
#define CHECKFAILED 1
#define TOOMANYTEMPFAILURES 4
#define HARDERROR 5		/* errors >= HARDERROR cause program to stop */
#define OUTOFMEM 9
#define MYMAXERR 10

#define USER "ID"
#define ENTRYKIND "EK"
#define HOMEDIR "HD"

extern int errno;

#ifndef DONTRETRYTEMPFAILURES
static struct tempfail {
  char *name, *home;
  struct tempfail *next;
} *temp_failures = NULL;
static int retry_count = 0;

#define MAXRETRYCOUNT 10
#define WAITBEFORERETRY 300	/* wait 5 mins before retrying temp failures*/

#endif /* DONTRETRYTEMPFAILURES */

static void logerror(code)
int code;
{
  char *msg;

  switch(code) {
  case NOERR:
    return;
  case CHECKFAILED:
    if (errno == 0)
      msg = "Couldn't check .forward file";
    else
      msg = UnixError(errno);
    break;
  case TOOMANYTEMPFAILURES:
    msg = "There was at least one entry which could not authoritatively be examined.  Please check the error log";
    break;
  case OUTOFMEM:
    msg = "Out of memory";
    break;
  default:
    fprintf(stderr, "WP ");
    msg = wp_ErrorString(code-MYMAXERR);
  }
  fprintf(stderr, "Error: %s.\n", msg?msg:"<NULL MESSAGE>");
}

static void quit(code)
int code;
{
  logerror(code);
  exit(code);
}

static int InitWP(cdp)
struct wp_cd **cdp;
{
  /* initialize the local white pages, put wp_cd into cdp */
  wp_ErrorCode wp_err;

  CheckServiceConfiguration();
  if ((wp_err = wp_InitializeCell(ThisDomain, cdp)) != wperr_NoError)
    return(((int)wp_err+MYMAXERR));

  return(NOERR);
}

static void ReportError(user, state)
char *user;
int state;
{
  char *msg;

  switch(state) {
  case NoState:
    msg = "No state yet";
    break;
  case StatedFile:
    msg = "Successfully stat'ed .forward file";
    break;
  case FailedToStatFile:
    msg = "Couldn't stat .forward file";
    break;
  case OpenedFile:
    msg = "Successfully opened .forward file";
    break;
  case FailedToOpenFile:
    msg = "Couldn't open .forward file";
    break;
  case ReadFile:
    msg = "Successfully read contents of .forward file";
    break;
  case FailedToReadFile:
    msg = "Couldn't read contents of .forward file";
    break;
  default:
    msg = "Unknown state";
  }
  printf("|error|%s|%s|%s|\n", user, msg, UnixError(errno));
}

static char *Trim(s, set)
char *s, *set;
{
  /* Trim the characters in set from the end of s, and
     return a pointer to the first character in s not in set.
     Destructively modifies the end of s, in trimming. */
  int i;

  /* trim the chars in set from the end of s*/
  for(i=strlen(s)-1;(i>=0) && strchr(set,s[i]);s[i--]='\0');

  /* turn the chars in set from the middle of s into the first char in set */
  for(i=strlen(s)-1;i>=0;--i)
    if(strchr(set,s[i]))
      s[i]=set[0];

  /* return a pointer to the first character not in set */
  return(s+strspn(s,set));
}

static int PrintFwd(name, home)
char *name, *home;
{
  /* compare contents of home/.forward with fwd (which can be NULL), if
     different, print 'em */
  int state = NoState;
  char filename[MAXPATHLEN];
  int f;
  struct stat stblk;
  static char *buf = NULL;
  static bufsize = 0;
  char *trimmed = "";
#ifndef DONTLOGPROGRESS
  static int countusers = LOGTHRESHHOLD;
#endif /* DONTLOGPROGRESS */

#ifndef DONTLOGPROGRESS
  if (++countusers >= LOGTHRESHHOLD) {
    countusers = 0;
    fprintf(stderr, "Checking $ID=%s, $HD=%s.\n",
	    name, home);
    fsync(fileno(stdout));	/* make sure our progress is recorded */
    fsync(fileno(stderr));
  }
#endif /* DONTLOGPROGRESS */

  (void)sprintf(filename, "%s/.forward", home);
  errno = 0;
  if ((f = open(filename, O_RDONLY)) == -1) {
    state = FailedToOpenFile;
    
    if (errno == ENOENT) {
	return(NOERR);
    } else {
      ReportError(name, state);
      return(CHECKFAILED);
    }
  } else {
    state = OpenedFile;
    
    errno = 0;
    if (fstat(f, &stblk)) {
      state = FailedToStatFile;

      close(f);
      VenusFlush(filename);
      VenusFlush(home);
      ReportError(name, state);
      return(CHECKFAILED);
    } else {
      state = StatedFile;
      
      if (buf == NULL) {
	/* buf not yet allocated */
	if ((buf = (char *)malloc(stblk.st_size+1)) == NULL) {
	  close(f);
	  VenusFlush(filename);
	  VenusFlush(home);
	  return(OUTOFMEM);
	} else {
	  bufsize = stblk.st_size+1;
	}
      } else if (bufsize < stblk.st_size+1) {
	/* buf isn't big enough to read file */
	free(buf);
	if ((buf = (char *)malloc(stblk.st_size+1)) == NULL) {
	  close(f);
	  VenusFlush(filename);
	  VenusFlush(home);
	  return(OUTOFMEM);
	} else {
	  bufsize = stblk.st_size+1;
	}
      }
      *buf = '\0';
      
      errno = 0;
      if (read(f, buf, stblk.st_size) <  (stblk.st_size)) {
	state = FailedToReadFile;
	
	ReportError(name, state);
	free(buf);
	close(f);
	VenusFlush(filename);
	VenusFlush(home);
	return(CHECKFAILED);
      } else {
	state = ReadFile;
	
	close(f);
	VenusFlush(filename);
	VenusFlush(home);

        buf[stblk.st_size] = '\0';
	trimmed = Trim(buf, WHITESPACE);
	printf("|.forward|%s|%s|%ld|\n", name, trimmed, stblk.st_ctime);
	return(NOERR);
      } /* if (read... */
    } /* if (stat... */
  } /* if ((f = open... */
}

#ifndef DONTRETRYTEMPFAILURES
static void RecordTempFail(name, home)
char *name, *home;
{
  /* record this entry for later processing */
  struct tempfail *new;

  if ((new = (struct tempfail *)malloc(sizeof(*new))) == NULL)
    return;
  if ((new->name = (char *)malloc(strlen(name)+1)) == NULL)
    return;
  strcpy(new->name, name);
  if ((new->home = (char *)malloc(strlen(home)+1)) == NULL)
    return;
  strcpy(new->home, home);
  new->next = temp_failures;
  temp_failures = new;
}
#endif /* DONTRETRYTEMPFAILURES */

static int CheckEntry(cd, key)
struct wp_cd *cd;
wp_PrimeKey key;
{
  /* check the entry against $HD/.forward, if appropriate */
  int my_err;
  wp_ErrorCode wp_err;
  wp_FieldIndex EKidx = wp_FieldNameToIndex(ENTRYKIND);
  wp_FieldIndex HDidx = wp_FieldNameToIndex(HOMEDIR);
  wp_FieldIndex IDidx = wp_FieldNameToIndex(USER);
  char *entrykind, *homedir, *user;


  if ((wp_err = cwp_Read(cd, key, EKidx, &entrykind)) != wperr_NoError)
    return((int)wp_err+MYMAXERR);

  if ((atoi(entrykind) & 1) != 0) {
    if ((wp_err = cwp_Read(cd, key, HDidx, &homedir)) != wperr_NoError)
      return((int)wp_err+MYMAXERR);
    if ((wp_err = cwp_Read(cd, key, IDidx, &user)) != wperr_NoError)
      return((int)wp_err+MYMAXERR);
    if ((my_err = PrintFwd(user, homedir)) != NOERR)
#ifndef DONTRETRYTEMPFAILURES
      if ((my_err < HARDERROR) && tfail(errno))
	RecordTempFail(user, homedir);
#endif /* DONTRETRYTEMPFAILURES */
      return(my_err);
  }
}

main(argc, argv)
int argc;
char *argv[];
{
  int my_err;
  wp_ErrorCode wp_err;
  struct wp_cd *cd = NULL;
  wp_PrimeKey PrimeKey = NULL;

  if ((my_err = InitWP(&cd)) != NOERR)
    quit(my_err);

  for(my_err = NOERR, 
      argv[1] ? (wp_err=cwp_GetUIDOnly(cd, argv[1], &PrimeKey))
              : (wp_err = cwp_Generate(cd, &PrimeKey));
      (wp_err == wperr_NoError)
      && (PrimeKey != NULL)
      && (my_err < HARDERROR);
      wp_err = cwp_Generate(cd, &PrimeKey))
    logerror(my_err = CheckEntry(cd, PrimeKey));

#ifndef DONTRETRYTEMPFAILURES
  my_err = NOERR;
  while((temp_failures != NULL) && (retry_count < MAXRETRYCOUNT) && (my_err < HARDERROR)) {
    struct tempfail *t, *o;
    
    sleep(WAITBEFORERETRY);
    ++retry_count;
    t = temp_failures;
    o = NULL;
    temp_failures = NULL;	/* build list anew on new temp errors */
    
    while((my_err < HARDERROR) && t) {
      o = t;
      if ((my_err = PrintFwd(t->name, t->home)) != NOERR) {
	if ((my_err < HARDERROR) && tfail(errno)) {
	  t = t->next;
	  o->next = temp_failures; /* put entry back on retry list */
	  temp_failures = o;
	} /* if ((my_err < HARDERROR ... */
      } else {			/* sucessfully processed entry */
	t = t->next;
	free(o->name);
	free(o->home);
	free(o);
      } /* if ((my_err = PrintFwd ... */
    } /* for(; t; t=... */
  } /* while(temp_failures ... */

  if (retry_count >= MAXRETRYCOUNT) {
    struct tempfail *t;

    for(t = temp_failures; t; t=t->next)
      printf("|Too many temporary failures|%s|\n", t->name);
    my_err = TOOMANYTEMPFAILURES;
  }
#endif /* DONTRETRYTEMPFAILURES */

  cwp_Terminate(cd);
  quit((wp_err!=wperr_NoError)?wp_err+MYMAXERR:my_err);

}

