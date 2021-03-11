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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/wpi/RCS/wpi.c,v 1.30 1993/06/22 20:49:20 gk5g Exp $";
#endif

/*========================================================================*\
 *                                                                        *
 * implementation of wpi, the UNIX, command-line interface for            *
 * interactively requesting changes to their WP entries.                  *
 *                                                                        *
\*========================================================================*/

#include <andrewos.h>
#include <wpi.h>
#include <stdio.h>
#include <svcconf.h>
#include <sys/param.h>
#ifdef AMS_ENV
#include <mail.h>
#include <dropoff.h>
#endif /* AMS_ENV */

#ifndef _STD_C
#define remove(x) unlink(x)
#endif

#define NULL_TO_EMPTY(x) (((x)!=NULL) ? (x) : "")

static char *WPAdministrators[] = { /* Mail to these addresses to effect request */
  NULL,NULL
};
static char *progname = NULL;	/* name of program */
static boolx_t emulate_forward = false; /* If true, act like the forward program */
static boolx_t emulate_setfrom = false; /* If true, act like the setfrom program */
static boolx_t force_warn = false;	/* If true, don't exit, but continue */
static boolx_t changes_made = false; /* if true, some fields were modified */
static boolx_t req_auth = false;	/* if true, some authorization needed */
static boolx_t quiet = false; /* if true, suppress messages */

static void
warn(msg)
char *msg;
{				/* print a warning message */
  if (!quiet) {
    fputs("Warning:  ",stderr);
    fputs(msg,stderr);
    fputs("\n",stderr);
    fflush(stderr);
  }
  return;
}

static void
error(msg, code)
char *msg;
int code;
{				/* print an error message */
  if (!quiet) {
    fputs("ERROR:  ",stderr);
    fputs(msg,stderr);
    fputs("\n",stderr);
    fflush(stderr);
  }
  exit(code);
}

static void
fail(msg, code)
char *msg;
int code;
{				/* print a warning or error, depending */
  if (force_warn)
    warn(msg);
  else
    error(msg,code);
}

static enum {
  print_staged_address,
  remaining_opts_are_address,
  specify_cell,
  just_validate_address,
  print_old_address,
  shutup,
  test_old_address_first,
  zero_address,
  assume_admin,
  override_failures, 
  specify_username, 
  command_line_field,
  bad_option,
  not_an_option
}
which_arg(arg)
char *arg;
{				/* parse the argument list */
  if (arg[0] == '-' && arg[2] == '\0') {
    if (emulate_forward || emulate_setfrom) {
      switch (arg[1]) {
      case 'R': return(print_staged_address);
      case 'a': return(remaining_opts_are_address);
      case 'c': return(specify_cell);
      case 'n': return(emulate_forward ? just_validate_address : bad_option);
      case 'o': return(test_old_address_first);
      case 'r': return(print_old_address);
      case 's': return(shutup);
      case 'u': return(specify_username);
      case 'z': return(zero_address);
      default: return(bad_option);
      }
    } else {
      switch (arg[1]) {
      case 'A': return(assume_admin);
      case 'F': return(override_failures);
      case 'c': return(specify_cell);
      case 'f': return(command_line_field);
      case 's': return(shutup);
      case 'u': return(specify_username);
      default: return(bad_option);
      }
    }
  }
  return(not_an_option);
}

static char *
quote_wp_chg_field(s)
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

static char *
format_mail(entry,user,requestor,reqdomain,explain)
WPI_entry_t entry;
char *user, *requestor, *reqdomain;
int explain;
{				/* Format the mail into a file suitable 
				   for mailing to wpid,
				   returns the name of the file */
  int i;
  char filename[MAXPATHLEN];
  FILE *fd;
  long t;

  sprintf(filename,"/tmp/wpi.%d",getpid());
  if ((fd = fopen(filename, "w"))==NULL)
    error("Couldn't open file for mailing.",9);

  time(&t);
  fprintf(fd,"Date: %s",arpadate());
  fprintf(fd,"From: %s+@%s\n",requestor,reqdomain);
  fprintf(fd,"To: %s",WPAdministrators[0]);
  for(i=1; WPAdministrators[i];++i) {
    fprintf(fd, ", %s", WPAdministrators[i]);
  }
  fprintf(fd,"\n");

  fprintf(fd,"Subject:  Request for WP Update from '%s'. (from %s v%d)\n\n",
	  requestor, progname, WPI_DS_VERSION);
  fprintf(fd,"version:%d\n", WPI_DS_VERSION);
  fprintf(fd,"cell:%s\n", WPI_GetWorkingDomain());
  for(i=0; entry[i].fieldnum != -1; ++i)
    if (entry[i].changed)
      fprintf(fd,"change:%s:%s:*:%s:%ld\n",
	      user,
	      entry[i].fieldname,
	      quote_wp_chg_field(entry[i].value),
	      t);

  if(explain) {
    char buf[255];

    fprintf(fd,"\n>Reason given by '%s+@%s' for change:\n", requestor,reqdomain);
    puts("\nSince you've requested changes that will need administrative approval,");
    puts("please enter some explanatory text to aid evaluation of your request");
    puts("(end your text with a line containing the single character '.'):");
    *buf = '\0';
    while((!feof(stdin))&&(*buf!='.')) {
      if (*buf) fprintf(fd,">%s\n", buf); /* write line out */
      printf(">> ");
      gets(buf);
    }
  }

  if(fclose(fd))
    error("Error on close of mail file.",10);
  return(filename);
}

static void
checkout(fieldname, newvalue, entry)
char *fieldname;
char *newvalue;
WPI_entry_t entry;
{				/* check (and make) the requested change */
  validate_t result;
  
  if(strlen(newvalue)>0) {
    if(!(strcmp("NONE",newvalue))) strcpy(newvalue, "");
    WPI_error_code = WPI_OK;
    result = WPI_Validate(fieldname, newvalue, entry);
    if (WPI_error_code < WPI_OK) {
      fprintf(stderr, "WPI library error: %s\n",WPI_error_msg);
      exit(WPI_error_code);
    } else if (WPI_error_code == WPI_WARN) {
      fprintf(stderr, "WPI library warning: %s\n",WPI_error_msg);
    }
    switch (result) {
    case cool: 
      changes_made = true;
      break;
    case drag:
      changes_made = true;
      req_auth = true;
      warn("Will need approval from WP Administrator.");
      break;
    case uncool:
      fail("Sorry, that value is not allowed.",3);
      WPI_Update(fieldname, newvalue, entry); 
      changes_made = true;
      warn("Change made forcibly.  Beware.");
      break;
    }
  }
}


static void
usage()
{
  if (emulate_forward)
    fputs("usage: forward [-s] [-n] [-o oldForward] [-c cellName] [-u userName] {-r|-R|-z|[-a] forwardTo}\n", stderr);
  else if (emulate_setfrom)
    fputs("usage: setfrom [-s] [-o oldFrom] [-c cellName] [-u userName] {-r|-R|-z|[-a] fromaddr|namesep}\n", stderr);
  else
    fputs("usage: wpi [-A] [-F] [-s] [-c cell] [-u user] [-f field [value]]* \n",stderr);
  exit(4);
}

main(argc, argv)
int argc;
char *argv[];
{
  char *arg;
  boolx_t command_line_fields_used = false;
  WPI_entry_t entry = NULL;
  char *self, *username, *fieldname,
       *selfdomain, *mailfile, newvalue[255];
  boolx_t adminp = false;
  int i;
  /* forward emulation */
  boolx_t just_print = false;
  boolx_t just_validate = false;
  char *old_addr = NULL;
  char *address = "";

  /* test argv[0]:t and see if we're forward or wpi */
  (progname = rindex(argv[0],'/')) ? ++progname : (progname=argv[0]);
  if (!strcmp(progname,"forward"))
    emulate_forward = true;
  if (!strcmp(progname,"setfrom"))
    emulate_setfrom = true;

  CheckServiceConfiguration();
  CheckAMSConfiguration();

  selfdomain = ThisDomain; /* By default, overridable */
  WPI_SetWorkingDomain(ThisDomain);

  switch(test_dropoff()){	/* is dropoff working? */
  case DT_AMS: 
    break;
  case DT_AMSWAIT: 
    warn("Possible delays."); break;
  default: 
    fail("Can't send authenticated mail.  Try again later.",1);
    break;
  }

  WPI_error_code = WPI_OK;
  self = username = WPI_Self();	/* in ThisDomain, at first... */
  if (WPI_error_code < WPI_OK) {
      fprintf(stderr, "WPI library error: %s\n",WPI_error_msg);
      exit(WPI_error_code);
  }
  for(i=1;i<argc;i++){
    arg=argv[i];
    switch (which_arg(arg)) {
    case assume_admin:
      if (entry)
	fail("Specify admin flag before fieldnames.",13);
      adminp = true; 
      break;
    case override_failures:
      force_warn = true; break;
    case specify_cell:
      if (entry)
	fail("Specify cellname before fieldnames.",14);
      if (!(argv[++i]))
	error("Missing cellname argument",11);
      else
	WPI_SetWorkingDomain(argv[i]);
      break;
    case shutup:
      quiet = true;
      break;
    case specify_username:
      if (!(username = argv[++i])) {
	error("Missing username argument.", 7);
	break;
      }
      if (!emulate_forward && !emulate_setfrom && !adminp && strcmp(username,self)) {
	fail("Can't modify other's WP entry.", 2);
	warn("Attempting to modify forcibly.  Beware.");
      }      
      if (entry) {
	fail("Specify username before fieldnames.",15);
	warn("Discarding previous changes.");
	free(entry);
	entry = NULL;
      }
      break;
    case command_line_field:
      if (!(fieldname = argv[++i]))
	error("Missing fieldname argument.", 6);
      switch (WPI_CanIChange(fieldname))  {
      case ALLOW_MODIFY:
	break;
      case PRIVILEDGED_MODIFY:
	if (adminp) break;
	error("Not allowed to modify that field.  You are not an administrator.",22);
	break;
      case GENERATED_FIELD:
	error("Not allowed to modify that field.  It is generated programmatically.",23);
	break;
      case UNKNOWN_FIELD:
      default:
	error("No such field.",24);
	break;
      }
      command_line_fields_used = true;
      if (!entry) {
	if (self == username) {	/* defaulting to self */
	  if ((self = username = WPI_Self())==NULL) {	/* in new domain */
	    error("could not find you in other cell",12);
	  } 
	  selfdomain = WPI_GetWorkingDomain();
	}
	if (!(entry = WPI_Lookup(username, adminp))) {
	  fprintf(stderr, "WPI library error: %s\n",WPI_error_msg);
	  exit(WPI_error_code);
	}
      }
      if ((argv[i+1]) && (which_arg(argv[i+1]) == not_an_option))
	strcpy(newvalue, argv[++i]);
      else {
	char *example;
	
	example = WPI_Example(fieldname);
	if (!strcmp(example,"")) {
	  printf("%s [%s]: ", 
		 WPI_Nice(fieldname), 
		 NULL_TO_EMPTY(WPI_Value(fieldname, entry)));
	} else {
	  printf("%s (e.g. %s) [%s]: ", 
		 WPI_Nice(fieldname), example,
		 NULL_TO_EMPTY(WPI_Value(fieldname, entry)));
	}
	gets(newvalue);
      }
      checkout(fieldname,newvalue,entry);
      break;

      /* forward emulation */
    case print_staged_address:
				/* *BUG* not yet implemented, fall through */
    case print_old_address:
      just_print = true;
      break;
    case just_validate_address:
      just_validate = true;
      break;
    case test_old_address_first:
      if ((old_addr = argv[++i])==NULL)
	error("missing old address",16);
      break;
    case zero_address:
      if (strcmp(address,""))
	error("Can't specify an address and zero address.",21);
      address = NULL;
      break;
    case remaining_opts_are_address:
      if (!address)
	error("Can't specify zero address and an address.",18);
      while((++i) < argc) {
	char *temp;

	arg = argv[i];
	if ((temp = (char *)malloc(strlen(arg)+strlen(address)+2))==NULL)
	  error("Couldn't malloc space for address.",17);
	strcpy(temp,address);
	strcat(temp," ");
	strcat(temp,arg);
	address = temp;
      }
      break;
    case not_an_option:
      if (emulate_forward || emulate_setfrom) {
	/* snarf this option as an address */
	if (!address)
	  error("Can't specify zero address and an address.",18);
	{	char *temp;
		
		if ((temp = (char *)malloc(strlen(argv[i])+strlen(address)+2))==NULL)
		  error("Couldn't malloc space for address.",17);
		strcpy(temp,address);
		strcat(temp," ");
		strcat(temp,argv[i]);
		address = temp;
	}
	break;
      } /* for wpi, fall through */
    case bad_option:			/* fall through */
    default:
      { char errbuf[255];
        sprintf(errbuf,"Bad command line argument '%s'.",arg);
	warn(errbuf);
	usage();			/* exits */
      }
    }
  }
  if (!emulate_forward && !emulate_setfrom && !command_line_fields_used) {
    if (!entry) {
	if (self == username) {	/* defaulting to self */
	  if ((self = username = WPI_Self())==NULL) {	/* in new domain */
	    error("could not find you in other cell",12);
	  } 
	  selfdomain = WPI_GetWorkingDomain();
	}
	if (!(entry = WPI_Lookup(username, adminp))) {
	  fprintf(stderr, "WPI library error: %s\n",WPI_error_msg);
	  exit(WPI_error_code);
	}
    }
    printf("Enter changes for %s@%s (<CR> for no change, 'NONE' for empty string).\n", username, WPI_GetWorkingDomain());
    for(fieldname=entry[i=0].fieldname;
	fieldname;
	fieldname=entry[++i].fieldname){
      char *example = WPI_Example(fieldname);
      change_t access = WPI_CanIChange(fieldname);

      /* Just ask about the relevant fields */
      if ((access != ALLOW_MODIFY) &&
	  (!adminp || (access!=PRIVILEDGED_MODIFY))) {
	  continue;
      }

      if (!strcmp(example,"")) {
	printf("%s [%s]: ", 
	       WPI_Nice(fieldname), 
	       NULL_TO_EMPTY(WPI_Value(fieldname, entry)));
      } else {
	printf("%s (e.g. %s) [%s]: ", 
	       WPI_Nice(fieldname), example,
	       NULL_TO_EMPTY(WPI_Value(fieldname, entry)));
      }
      gets(newvalue);
      checkout(fieldname,newvalue,entry);
    }
  } else if (emulate_forward) {
    /* do forward processing here */

    if (just_validate) {
      char *out;

      strcpy(fwdvalid_msgbuf,"");
      if (ValidateFwdAddr(address?address:"", &out)) {
	fputs(fwdvalid_msgbuf, stderr);
	fputs("\n", stderr);
	exit(3);
      } else {
	puts(out);
	exit(0);
      }
    }
    if (!entry) {
	if (self == username) {	/* defaulting to self */
	  if ((self = username = WPI_Self())==NULL) {	/* in new domain */
	    error("could not find you in other cell",12);
	  } 
	  selfdomain = WPI_GetWorkingDomain();
	}
	if (!(entry = WPI_Lookup(username, false))) {
	  fprintf(stderr, "WPI library error: %s\n",WPI_error_msg);
	  exit(WPI_error_code);
	}
    }

    if (just_print) {
	puts(NULL_TO_EMPTY(WPI_Value("Fwd", entry)));
	exit(0);
    }

    if (address && !strcmp(address,"")) {
      /* no address given */
      if (!quiet) fprintf(stderr, "No address to which to forward mail;\n");
      usage();
    }

    if (old_addr && !strcmp(old_addr, NULL_TO_EMPTY(WPI_Value("Fwd", entry)))) {
      error("Old address does not match command line spec.",20);
    }
    if (WPI_Validate("Fwd", address?address:"", entry) != uncool) {
      changes_made = true;
    }
  } else if (emulate_setfrom) {
    /* do from address processing here */

    /* Can't do validation */

    if (!entry) {
	if (self == username) {	/* defaulting to self */
	  if ((self = username = WPI_Self())==NULL) {	/* in new domain */
	    error("could not find you in other cell",12);
  }
	  selfdomain = WPI_GetWorkingDomain();
	}
	if (!(entry = WPI_Lookup(username, false))) {
	  fprintf(stderr, "WPI library error: %s\n",WPI_error_msg);
	  exit(WPI_error_code);
	}
    }

    if (just_print) {
	puts(NULL_TO_EMPTY(WPI_Value("CAF", entry)));
	exit(0);
    }

    if (address && !strcmp(address,"")) {
      /* no address given */
      if (!quiet) fprintf(stderr, "No address to which to set canonical address format;\n");
      usage();
    }

    if (old_addr && !strcmp(old_addr, NULL_TO_EMPTY(WPI_Value("CAF", entry)))) {
      error("Old address does not match command line spec.",20);
    }
    if (WPI_Validate("CAF", address?address:"", entry) == cool) {
      changes_made = true;
    }
    else {
      /* Make sure we don't send off administrator-only change requests. */
      error("Sorry, that value is not allowed.",3);
    }
  }

  if (!changes_made) {
    fail("No changes made",5);
    warn("Mailing unchanged entry forcibly.  Beware.");
  }

  if ((WPAdministrators[0] = CheckAMSWPIAddr(WPI_GetWorkingDomain()))==NULL)
    fail("Don't have an address to address request to. ",9);

  mailfile = format_mail(entry, username, self, selfdomain, req_auth);
  if (dropoff(WPAdministrators, mailfile, NULL,NULL,NULL) > D_LOCALQ) {
    error("Bummer, dropoff failed.",8);
  } else if (!quiet) {
    if (emulate_forward) {
      fprintf(stdout, "The forwarding address for user %s in cell %s will be established as ``%s''.\n",username,WPI_GetWorkingDomain(),NULL_TO_EMPTY(WPI_Value("Fwd",entry)));
    } else if (emulate_setfrom) {
      fprintf(stdout, "The canonical address format for user %s in cell %s will be established as ``%s''.\n",username,WPI_GetWorkingDomain(),NULL_TO_EMPTY(WPI_Value("CAF",entry)));
    } else {
      fprintf(stdout,"Request sent to %s.\n", WPAdministrators[0]);
    }
  }
  remove(mailfile);
  exit(0);
}

