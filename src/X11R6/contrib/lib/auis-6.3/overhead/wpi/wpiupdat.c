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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/wpi/RCS/wpiupdat.c,v 1.19 1992/12/15 21:12:21 rr2b R6tape $";
#endif

/*========================================================================*\
 *                                                                        *
 * implementation of wpiupdat, the WP update filter                       *
 *                                                                        *
\*========================================================================*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/param.h>
#include <svcconf.h>
#include <util.h>
#include <wpi.h>

/* Error code convention for PROVIDE-MAIL-BASED-SERVICE:
   Bit:  Integer: Meaning:
    0       1     Send success report to requestor, audittrail
    1       2     Send authorization request to administrator
    2       4     Send failure (possible forgery) to failure, requestor
    3       8     Temporary failure during some part of the request, requeue (part)
    4      16     Entire operation failed, requeue all
*/   

#define Success 1

#define HardFail_* 2,4	/* persistent failure -- needs human intervention to fix */
#define HardFail_TooManyArgs     4
#define HardFail_TooFewArgs      4
#define HardFail_BadDSVersion    4
#define HardFail_WrongCell       4
#define HardFail_Syntax          4
#define HardFail_BadWPIreturn    4
#define HardFail_BadCommand      4
#define HardFail_PossibleForgery 4

#define HardFail_AuthFailure     2

#define TempFail_* 8,16		/* transient failure -- should go away if tried again */
#define TempFail_CouldNotCloseOutfile  16
#define TempFail_CouldNotRenameOutfile 16
#define TempFail_Validation 16
#define TempFail_CouldNotOpenOutfile   8
#define TempFail_NotYetSupported       8

#define Warn_* 0		/* not a failure, just bitching */
#define Warn_MiscGarbage 0

#define MAXLINELENGTH 255

static int max_err = 0;
static char *outdir;		/* the output directory, passed in to main */

/* here's how the changes happen:

   if do_change finds an empty static global FILE *outfile, it opens one in 
   char *outdir, with name strcat(char *TEMPPREFIX,char *ams_genid(1)), 
   failing if the open fails (set outfile to NULL).

   on exit, if the close fails, return a TempFail_CouldNotCloseOutfile, which means
   that none of the processing was recorded.

   after the close, atomically rename the file to be strcat(char *DONEPREFIX, 
   ams_genid(1)), and report TempFail_CouldNotRenameOutfile if that fails.  
   Otherwise, return max_err.
*/
#define TEMPPREFIX "/t"		/* POLICY -- indicates file not finished*/
#define DONEPREFIX "/f"		/* POLICY -- atomic rename to this on completion */
static char outfname[MAXPATHLEN];
static FILE *outfile = NULL;

static void error(errcode, msg)
int errcode;
char *msg;
{				/* record the error, OR'ing the status  */
  if (errcode==0) fprintf(stderr,"> Warning");
  if ((errcode & 2) || (errcode & 4)) fprintf(stderr,"> Error");
  if ((errcode & 8) || (errcode & 16)) fprintf(stderr, "> Transient failure ");

  if (errno)
    fprintf(stderr, "(probable errno %d ``%s'')",
	    errno, UnixError(errno));
  if (msg)
    fprintf(stderr,": ``%s'' (status %d).\n", msg, errcode);
  else
    fprintf(stderr,": no message (status %d).\n", errcode);

  max_err |= errcode;
}


static void success(msg)
char *msg;
{				/* record a success */
  fprintf(stdout, "Successfully processed: ``%s''.\n",msg);
  max_err |= Success;
}


static char *quote(s)
char *s;
{				/* Quotes output for passwd.chg format. */
  char *q, *r;

  if ((r=(char *)malloc(2*(s?strlen(s)+1:3)))==NULL)
    return(NULL);
  q=r;

  if ((s==NULL)||(*s == '\0')) {
    strcpy(r,"+ ");
  } else {
    for(; *s; ++s) {
      switch(*s) {
      case ':' : *r++ = '+'; *r++ = '='; break;
      case '+' : *r++ = '+'; *r++ = '+'; break;
      default: *r++ = *s;
      }
    }
    *r = '\0';
  }
  return(q);
}


static char *unquote(s)
char *s;
{				/* strip the string s of the passwd.chg quotting */
  char *q,*r;

  if(s==NULL)
    return(NULL);

  if((r=(char *)malloc(strlen(s)+1))==NULL)
    return(NULL);

  if (!strcmp(s,"+ ")) {	/* the empty string */
    strcpy(r,"");
    return(r);
  }

  q=r;
  for(;*s;++s,++r)
    *r=((*s=='+')?(++s,((*s=='=')?':':*s)):(*s));
  *r='\0';

  return(q);
}


#ifdef AMS_ENV
#define genid(x) ams_genid(x)
#else /* AMS_ENV */
static char *genid(x)
     int x;
{
  static long counter=0;
  static char *template="genid000000";

  counter = (counter + 1)%1000000;
  sprintf(template, "genid%06d", counter);
  return(template);
}
#endif /* AMS_ENV */

static int do_change(user,field,timestamp,record)
char *user, *field;
int timestamp;
WPI_entry_t record;
{				/* write the change out */
  char *newvalue;

  if (outfile==NULL) {		/* need to open the file */
    strcpy(outfname, outdir);
    strcat(outfname, TEMPPREFIX);
    strcat(outfname,genid(1));

    if ((outfile=fopen(outfname,"w"))==NULL)
      return(0);
  }

  newvalue = quote(WPI_Value(field,record));
  return(fprintf(outfile,"%s:%s:*:%s:%d\n",
		 user,field,newvalue,timestamp)!=EOF);
}


static void close_outfile()
{				/* close and rename the outfile */
  char newname[MAXPATHLEN];

  if(outfile!=NULL) {
    if(vfclose(outfile)==EOF)
      error(TempFail_CouldNotCloseOutfile,
	    "Failed to close file--start all over");
    strcpy(newname, outdir);
    strcat(newname, DONEPREFIX);
    strcat(newname,genid(1));

    if(rename(outfname,newname))
      error(TempFail_CouldNotRenameOutfile,
	    "Failed to rename file--start all over");
  }
  return;
}

static int blank_p(l)
char *l;
{				/* is this a blank line? */
  for(;(*l) && (isspace(*l));++l);
  return(!((*l) && (!isspace(*l))));
}


static int comment_p(l)
char *l;
{				/* is this a comment line? */
  return((*l) == '>');
}


#define adduser 0
#define rmuser 1
#define change 2
static int which_cmd(command)
char *command;
{				/* which command request is this? */
  static char *cmd_tbl[] =  {"adduser", "rmuser", "change", NULL};
  int i;

  for(i=0;cmd_tbl[i] && strcmp(cmd_tbl[i], command); ++i);
  return(cmd_tbl[i] ? i : -1);
}

static void stripNL(s)
char *s;
{				/* remove the trailing newline from input */
  if (s==NULL)
    return;

  for(;*s;++s)
    if(*s=='\n') {
      *s='\0';
      return;
    }
}

#define readln(stream, buf) freadline((stream),(buf), sizeof(buf))

static int freadline(stream,buf,bufsize)
FILE *stream;
char *buf;
int bufsize;
{				/* get a line */
  if (!fgets(buf,bufsize,stream))
    return(0);
  stripNL(buf);
  return(1);
}

static int parse(line, args)
char *line, *args[];
{				/* parse the command into pieces (broken by ":") */
  char *newline;
  char *colonpos;
  int i;

  if((newline=(char *)malloc(strlen(line)+1))==NULL)
    return(0);
  strcpy(newline,line);

  colonpos=newline;
  args[0]=colonpos;
  for(i=1; colonpos=strchr(colonpos,':'); ++i) {
    *colonpos++='\0';
    args[i]=colonpos;
  }
  args[i]=NULL;

  return(i);
}

main(argc, argv)
int argc;
char *argv[];
{
  char *auth;
  boolx_t admin;
  char buf[MAXLINELENGTH],
       err[2*MAXLINELENGTH],
       *args[MAXLINELENGTH];	/* arglist from parse */
  int nargs;

  errno = 0;			/* reset errno */
  if (argc>4) {
    error(HardFail_TooManyArgs, "Too Many Arguments -- pass an AuthUser, an output directory, and optionally the admin flag");
  } else if (argc<3) {
    error(HardFail_TooFewArgs, "Too Few Arguments -- need an AuthUser, an output directory, and optionally the admin flag");
  } else {
    auth = argv[1];
    outdir = argv[2];
    admin = ((argc==4)?true:false); /* if there is a third argument, then assume auth is administrator */
    
    while(!feof(stdin) 
	  && readln(stdin,buf)
	  && !((parse(buf,args)==2)
	       && (strcmp(args[0],"version")==0)))
      {
      if (!(blank_p(buf))
	  && !(comment_p(buf))) 
	{
	  sprintf(err,"Unexpected text: %s", buf);
	  errno = 0;
	  error(Warn_MiscGarbage, err);
	} /* if (!(blank_p(buf))) */
      } /* while (!feof(stdin)) */

    if (atoi(args[1])!=WPI_DS_VERSION) { 
      errno = 0;
      error(HardFail_BadDSVersion, "Bad Version Number");
    } else {
    
      while(!feof(stdin)
	    && readln(stdin,buf)
	    && !((parse(buf,args)==2)
		 && (strcmp(args[0],"cell")==0)))
	{
	  if (!(blank_p(buf))
	      && !(comment_p(buf))) 
	    {
	      sprintf(err,"Unexpected text: %s", buf);
	      errno = 0;
	      error(Warn_MiscGarbage, err);
	    } /* if (!(blank_p(buf))) */
	} /* while (!feof(stdin)) */
      
      CheckServiceConfiguration();
      if (strcmp(args[1],ThisDomain)) {
	errno = 0;
	error(HardFail_WrongCell, "Bad Cellname");
      } else {
	
	while(!feof(stdin)) {
	  if (readln(stdin, buf))
	    if (blank_p(buf) || comment_p(buf)) {
	    /* ignore the line */
	    } else {
	      nargs=parse(buf,args);
	      if ((nargs!=2)&&(nargs!=6)) {
		fprintf(stdout, "Could not understand ``%s''.\n", buf);
		sprintf(err,"Syntax error: %s",buf);
		errno = 0;
		error(HardFail_Syntax, err);
	      }
	      else switch(which_cmd(args[0])) {
	      case adduser:
	      case rmuser:
		/* what does it mean to add a user??? */
		sprintf(err,"Not yet supported: %s",buf);
		errno = 0;
		error(TempFail_NotYetSupported, err);
		break;
	      case change:
		if(nargs!=6) {
		  fprintf(stdout, "Could not understand ``%s''.\n", buf);
		  sprintf(err,"Syntax error: %s",buf);
		  errno = 0;
		  error(HardFail_Syntax, err);
		} else {
		  if(admin || (strcmp(auth,args[1])==0)) {
		    char *newvalue;
		    int timestamp;
		    WPI_entry_t record;
		    
		    timestamp = atoi(args[5]);
		    newvalue = unquote(args[4]);
		    
		    if (!(record = WPI_Lookup(args[1],admin)))
		      {
			/* (BUG) should try to determine is WPI_error_code is signalling a temp fail */
			error(HardFail_BadWPIreturn, WPI_error_msg);
		      } else {
			validate_t result;

			WPI_error_code = WPI_OK;
			result = WPI_Validate(args[2],newvalue,record);
			if (WPI_error_code < WPI_OK) {
			  /* (BUG) should try to determine is WPI_error_code is signalling a temp fail */
			  error(HardFail_BadWPIreturn, WPI_error_msg);
			} else {
			
			  switch(result) {
			  case cool:
			    if (do_change(args[1],args[2],timestamp,record)) {
			      char msg[255];
			      
			      sprintf(msg, "change field `%s' of user `%s' to `%s'",
				      WPI_Nice(args[2]), args[1], 
				      WPI_Value(args[2], record));
			      success(msg);
			    } else {
			      sprintf(err,"Could not open output file for: %s",buf);
			      error(TempFail_CouldNotOpenOutfile, err);
			    }
			    break;
			  case drag:
			    if (admin) {
			      if (do_change(args[1],args[2],timestamp,record)) {
				char msg[255];
				
				sprintf(msg, "change field `%s' of user `%s' to `%s'",
					WPI_Nice(args[2]), args[1], 
					WPI_Value(args[2], record));
				success(msg);
			      } else {
				sprintf(err,"Could not open output file for: %s",
					buf);
				error(TempFail_CouldNotOpenOutfile, err);
			      }
			    } else {
			      fprintf(stdout, "Request requires approval: change field `%s' of user `%s' to `%s'.\n",
				      WPI_Nice(args[2]), args[1], newvalue);
			      sprintf(err,"Not authorized to make change (user %s): %s",
				      auth,buf);
			      errno = 0;
			      error(HardFail_AuthFailure, err);
			    }
			    break;
			  case uncool:
			    if (WPI_error_code == WPI_TEMP_UNCERTAINVALID) {
			      error(TempFail_Validation, WPI_error_msg);
			    } else {
			      fprintf(stdout, "Illegal request: change field `%s' of user `%s' to `%s'.\n",
				      WPI_Nice(args[2]), args[1], newvalue);
			      sprintf(err,"Possible forgery here: %s", buf);
			      errno = 0;
			      error(HardFail_PossibleForgery, err);
			    }
			    break;
			  default:
			    sprintf(err,"Bad WPI lib return!! (%s)", buf);
			    error(HardFail_BadWPIreturn, err);
			    break;
			  } /* switch(WPI_Validate(...)) */
			} /* if (WPI_error_code...) */
		      } /* if (!record...) */
		  } else {
		    fprintf(stdout, "This request can only be made by an administrator:  change field `%s' of user `%s' to `%s'.\n",
			    WPI_Nice(args[2]), args[1], args[4]);
		    sprintf(err,"Not an administrator (authuser %s): %s",
			    auth,buf);
		    errno = 0;
		    error(HardFail_PossibleForgery, err);
		  } /* if(admin || ...) */
		} /* if(!sscanf(args...)) */
		break;
	      default:
		fprintf(stdout, "Could not understand ``%s''.\n", buf);
		sprintf(err,"Bad Command: %s", buf);
		errno = 0;
		error(HardFail_BadCommand, err);
		break;
	      } /* switch(which_cmd(...)) */
	    } /* if(blank_p(buf) ...) */
	} /* while(!feof(stdin)) */
      } /* if (cellname...) */
    } /* if (dsversion...) */ 
  } /* if (argc...) */
  close_outfile();

  exit(max_err);
}
