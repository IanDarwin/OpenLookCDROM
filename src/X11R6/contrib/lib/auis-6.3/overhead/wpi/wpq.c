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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/wpi/RCS/wpq.c,v 1.6 1994/03/01 23:02:55 rr2b Exp $";
#endif

/*========================================================================*\
 * wpq -- White Pages Query                                               *
 *   The "ultimate" WP command line user interface                        *
 *   See the usage() function for calling information                     *
 *                                                                        *
\*========================================================================*/

/*---------------*/
/* Includes      */
/*---------------*/
#include <ctype.h>
#include <stdio.h>
#include <wp.h>
#include <wpi.h>
#include <system.h>
#include <sys/types.h>
#include <sys/param.h>
#include <svcconf.h>
#include <util.h>
#include <string.h>

/*---------------*/
/* Macros        */
/*---------------*/
/* WPCALL checks the return code of a WP routine and calls error if
   there is a problem.  Useful if you don't expect the wp call to fail. */
#define WPCALL(x) {wp_ErrorCode err; if ((err=(x))!=wperr_NoError) error(wp_ErrorString(err));}

/* Downcase the char if upcase, else leave it alone, evals c twice */
#define DOWNCASECHAR(c) (isupper(c) ? tolower(c) : (c))

/*---------------*/
/* Constants     */
/*---------------*/
#define MAXKEYS 100		/* Max. number of keys to get from search */


/*---------------*/
/* Globals       */
/*---------------*/
/* *BUG* really shouldn't need any of these as globals, but
   it makes maintaining the state while parsing easier */
static int pretty = 1;		/* Whether to pretty print or not */
static int keysonly = 0;	/* Whether to show just keys or not */
static int showempty = 0;	/* Whether to print empty fields or not */
static int showmultiple = 0;	/* Whether to use Lookup or Search */
static int showmatchqual = 0;	/* Whether to print match quality */

#define FULLNAME 0
#define TOKEN 1
#define LOGIN 2
#define UID 3
#define PRIMARYKEY 4
static int searchtype = LOGIN;	/* The kind of lookup/search to perform */

static int num_fields;		/* The number of fields in the WP */

static char **field_names, **entry_values;

static struct wp_cd *wpdir;	/* the current WP */


/*---------------*/
/* Functions     */
/*---------------*/
static void
error(s)
char *s;
{ /* General Error Routine */
  fputs("\nErred with message:  ", stderr);
  fputs(s,stderr);
  fputs("\n", stderr);
  fflush(stderr);
  exit(1);
}

static void
show_pretty(tog)
int tog;
{ /* print in human readable format, if true */
  pretty = tog;
}

static void
show_matchqual(tog)
int tog;
{ /* print in human readable format, if true */
  showmatchqual = tog;
}

static void
keys_only(tog)
int tog;
{ /* print just the keys, don't fetch the field, if true */
  keysonly = tog;
}

static void
show_empty(tog)
int tog;
{ /* print all fields (including empty ones), if true */
  showempty = tog;
}

static void
show_multiple(tog)
int tog;
{ /* Use search to get multiple keys, if true (see MAXKEYS) */
  showmultiple = tog;
}

static char *
downcase(s)
char *s;
{ /* map the string to all lower case, non-destructively */

  char *t, *p;

  p = t = (char *)malloc(strlen(s)+1);
  for(; *s; ++s, ++t) *t = DOWNCASECHAR(*s);
  *t = '\0';

  return(p);
}

static void
use_read_write(tog)
int tog;
{ /* If true, check the read/write (current, but less available) 
     version of the WP */
  struct CellAuth *ca;
  char buf[MAXPATHLEN];
  
  if (FindCell(ThisDomain, &ca))
    error("Couldn't FindCell(ThisDomain).");
    
  if (tog) {
    strcpy(buf,CellCommonPrefix);
    strcat(buf,CellCommonRWSuffix);
    strcat(buf,downcase(ca->CellName));
    strcat(buf,CellCommonSuffix);
    strcat(buf,CellCommonWPDirSuffix);
    WPCALL(wp_InitializeDir(buf, &wpdir));
  } else {
    WPCALL(wp_InitializeCell(ca->CellName, &wpdir));
  }
}

static void
set_cellname(arg)
char *arg;
{ /* Switch to cell arg */
  WPCALL(wp_InitializeCell(arg, &wpdir));
}

static void
set_wpdir(arg)
char *arg;
{ /* use arg as the path to the WP dir
     (usuallly /afs/<cellname>/service/wp) */
  WPCALL(wp_InitializeDir(arg, &wpdir));
}

static void
set_search_key(arg)
char *arg;
{ /* set the search "mode" apropriately */
  static struct search_table {
    char *name;
    int type;
  } tbl[] = {
    {"fullname",FULLNAME},
    {"token",TOKEN},
    {"login",LOGIN},
    {"uid",UID},
    {"primarykey",PRIMARYKEY},
    {NULL, 0}
  };
  int i;

  for(i=0; tbl[i].name!=NULL; ++i) {
    if (
#ifndef DISALLOWSINGLECHAROPTIONS
	((arg[0]==tbl[i].name[0]) &&
	 (arg[1]=='\0')) ||
#endif /* DISALLOWSINGLECHAROPTIONS */
	(strcmp(arg, tbl[i].name)==0)) {
      searchtype = tbl[i].type;
      break;
    }
  }
  if(tbl[i].name==NULL) error("Bad key type.");
}


static void
describe_fields()
{ /* Prints a description of all the fields we know about */
  int i;

  for(i=0; i<num_fields; ++i) {
    fputs(WPI_Nice(field_names[i]), stdout);
    fputs(" -- ", stdout);
    puts(WPI_Description(field_names[i]));
  }
  exit(0);
}


static void
usage()
{ /* Print usage information for the user. */
  static char *helpmsg[] = {
    "Usage: wpq [options] [user[@cell]]...",
    "Options:",
    "	-help		describe self and options",
    "	-describefields	output descriptions of the fields",
    "",
    "	-pretty		output in format suitable for wp.add (quote)",
    "	+pretty		output in human readable format [default]",
    "	-justkeys	print full entries [default]",
    "	+justkeys	prints all the primary keys matching the probe,",
    "			feed to -key primarykey",
    "	-empty		don't print empty fields [default]",
    "	+empty		do print empty fields",
    "	-multiple	print only best match [default]",
    "	+multiple	print all matches",
    "	-readwrite	don't use read/write volume (use readonly) [default]",
    "	+readwrite	insist on read/write volume",
    "	-quality	don't show match quality [default]",
    "	+quality	do show match quality result from search",
    "",
    "	-cell		use the next arg as the new cell (and wpdir)",
    "			[defaults to current cell]",
    "	-wpdir		use the next arg as the new wpdir",
    "			[defaults to current cell's wp]",
    "	-key		use the next arg as the search type",
    "			possible keys:",
    "				fullname (LookupUIDOverPhonetics,MatchAnyName)",
    "				token (LookupUIDLast, MatchAll)",
    "				login (LookupUIDOnly) [default]",
    "				uid (LookupNIDOnly)",
    "				primarykey",
#ifndef DISALLOWSINGLECHAROPTIONS
    " (options can be abbreviated to their first character)",
#endif /* DISALLOWSINGLECHAROPTIONS */
    NULL
  };
  int i;
  
  for(i=0; helpmsg[i]; ++i)
    puts(helpmsg[i]);
  exit(0);
}

char *
xlate_matchqual(mq)
int mq;
{ /* translate the matchquality integer to a string */

  static struct matchq_tbl {
    int qual;
    char *name;
  } mq_xlate[] = {
    {MatchIDExact, "MatchIDExact"},
    {MatchNameExact, "MatchNameExact"},
    {MatchIDNameExact, "MatchIDNameExact"},
    {MatchNamePart, "MatchNamePart"},
    {MatchIDNamePart, "MatchIDNamePart"},
    {MatchFirstNameAbbrev, "MatchFirstNameAbbrev"},
    {MatchIDFirstNameAbbrev, "MatchIDFirstNameAbbrev"},
    {MatchLastNameAbbrev, "MatchLastNameAbbrev"},
    {MatchAnyName, "MatchAnyName"},
    {MatchAnyEx, "MatchAnyEx"},
    {MatchNameAbbrev, "MatchNameAbbrev"},
    {MatchAnyAb, "MatchAnyAb"},
    {MatchIDNameAbbrev, "MatchIDNameAbbrev"},
    {MatchNoHeuristics, "MatchNoHeuristics"},
    {MatchExOv, "MatchExOv"},
    {MatchAbOv, "MatchAbOv"},
    {MatchExPh, "MatchExPh"},
    {MatchAbPh, "MatchAbPh"},
    {MatchExPA, "MatchExPA"},
    {MatchAbPA, "MatchAbPA"},
    {MatchPhEx, "MatchPhEx"},
    {MatchPhAb, "MatchPhAb"},
    {MatchPhOv, "MatchPhOv"},
    {MatchPhPh, "MatchPhPh"},
    {MatchPhPA, "MatchPhPA"},
    {MatchAnyOv, "MatchAnyOv"},
    {MatchAnyPh, "MatchAnyPh"},
    {MatchPAEx, "MatchPAEx"},
    {MatchPAAb, "MatchPAAb"},
    {MatchPAOv, "MatchPAOv"},
    {MatchPAPh, "MatchPAPh"},
    {MatchPAPA, "MatchPAPA"},
    {MatchAnyPA, "MatchAnyPA"},
    {MatchIDPhonetics, "MatchIDPhonetics"},
    {MatchIDHeuristic, "MatchIDHeuristic"},
    {MatchAll, "MatchAll"},
    {0, NULL}
  };
  int i;

  for(i=0; mq_xlate[i].name != NULL; ++i)
    if (mq_xlate[i].qual == mq)
      return(mq_xlate[i].name);

  return("Unknown match quality");
}

static void
setup_fields()
{ /* enumerates and captures the fieldnames */
  wp_ErrorCode err;
  char *temp, *buf;
  int i;

  for(num_fields=0,err=wperr_NoError; err==wperr_NoError; ++num_fields)
    err=wp_AllFields(num_fields, &temp);
  if (err!=wperr_FieldIndexOutOfBounds)
    error(wp_ErrorString(err));

  if ((field_names = (char **)malloc(num_fields*sizeof(char *)))==NULL)
    error("Couldn't malloc memory for field_names.");
  if ((entry_values=(char **)malloc(num_fields*sizeof(char *)))==NULL)
    error("Couldn't malloc memory for entry_values.");

  --num_fields;

  for(i=0; i < num_fields; ++i) {
    if((err=wp_AllFields(i, &temp))!=wperr_NoError)
      error(wp_ErrorString(err));
    buf = (char *)malloc(strlen(temp)+1);
    strcpy(buf,temp);
    field_names[i] = buf;
  }

  return;
}


static void
fill_entry(wpd,key)
struct wp_cd *wpd;
wp_PrimeKey key;
{ /* Fetches the data for key from WP wpd */
  /* (barf) *BUG* stores result in the global entry_values */
  /* (minor) *BUG* uses the global num_fields */
  int i;
  char *fldvalue, *buf;
  wp_ErrorCode err;

  for(i=0;i<num_fields;++i){
    switch(err=cwp_Read(wpd,key, i, &fldvalue)) {
    case wperr_NoError:
      buf = (char *)malloc(strlen(fldvalue)+1);
      strcpy(buf,fldvalue);
      entry_values[i] = buf;
      break;
    case wperr_NoSuchField:
      entry_values[i] = NULL;
      break;
    default:
      cwp_Terminate(wpd);
      error(wp_ErrorString(err));
    }
  }
  return;
}

static void
print_raw_entry()
{ /* Prints the data in entry_values in wp.add format */
  int i;

  for(i=0; i<num_fields; ++i) {
    if (showempty || (entry_values[i] != NULL)) {
      putchar('$');
      fputs(field_names[i], stdout);
      putchar(' ');
      if (entry_values[i]!=NULL)
	fputs(entry_values[i], stdout);
      putchar('\n');
    }
  }
  fputs("$$\n",stdout);
}

static void
print_pretty_entry()
{ /* prints the data in entry_values in human readable format */
  int i;
  char *fldname, *fldvalue;
#ifndef DONTALIGNCOLS		/* #define DONTALIGNCOLS if you
				 don't want the output to be aligned */
  int k, maxlen = 0;
  char *fullfieldname;

				/* Try to find the widest field label */
  for(i=0; i<num_fields; ++i) {
    fldname = field_names[i];
    fldvalue = entry_values[i];
    if (showempty || fldvalue != NULL) {
      fullfieldname = WPI_Nice(fldname);
      maxlen = MAX(maxlen, strlen(fullfieldname));
    }
  }
#endif /* DONTALIGNCOLS */

  for(i=0; i<num_fields; ++i) {
    fldname = field_names[i];
    fldvalue = entry_values[i];
    if (showempty || fldvalue != NULL) {
      if (fldvalue == NULL) fldvalue = "";
      fullfieldname = WPI_Nice(fldname);
      fputs(fullfieldname,stdout);
      fputs(":  ", stdout);
#ifndef DONTALINGCOLS
      for(k=maxlen-strlen(fullfieldname); k>0; --k)
	putchar(' ');
#endif /* DONTALINGCOLS */
      fputs("``", stdout);			       
      fputs(fldvalue, stdout);
      fputs("''\n", stdout);
    }
  }
  fputs("\n",stdout);
}

static void
print_entry()
{ /* print the global entry_value in a format 
     depending upon the global pretty */

  if (pretty)
    print_pretty_entry();
  else
    print_raw_entry();
}

static int
getnid()
{ /* Find this user's Network ID, 
     (*BUG* doesn't do this->) failing that, try the EUI. */

  int nid;
  struct CellAuth *ca;

  if (FindCell(ThisDomain, &ca))
    error("Couldn't FindCell(ThisDomain).");
  if (ca->ViceID < 0) FillInCell(ca);
  if (ca->ViceID < 0)
    error("Couldn't FillInCell(ThisDomain).");
  nid = ca->ViceID;

  return(nid);
}

static void
lookup_user(wpdir,user)
struct wp_cd *wpdir;
char *user;
{ /* Use "user" as a probe into the WP.  Do a different kind
     of search depending upon the global searchtype.  Print the
     results or just the key(s) depending upon the global
     keysonly.  Do a Lookup (one key) or Search (multiple keys)
     depending upon the global showmultiple. If "user" has an
     at-sign ("@") in it, treat the string after the at-sign as
     a (temporary) cell change, and the part before the at-sign
     as the probe */

  wp_PrimeKey primekey;
  wp_PrimeKeySetPtr pkset;
  wp_SearchToken search;
  struct wp_cd *wpd;
  int minmatches, matchqual;
  char *cellname;
  int i;
  
#ifdef DEBUG
  fputs("Looking up key ``", stdout);
  fputs(user, stdout);
  fputs("''...\n", stdout);
#endif

  if ((cellname=strchr(user,'@'))!=NULL) {
				/* flesh out the cellname */
    *cellname = '\0';
    ++cellname;
    WPCALL(wp_InitializeCell(cellname,&wpd));
  } else {
    wpd = wpdir;
  }

  primekey = NULL;
  pkset = NULL;

  switch(searchtype){
  case FULLNAME:
    WPCALL(wp_SetUp(user,LookupUIDOverPhonetics,&search));
    if (!showmultiple) {
      WPCALL(cwp_Lookup(wpd,search,&minmatches,
			MatchAnyName,&matchqual,&primekey));
      if (showmatchqual)
	printf("Match quality was %d (%s).\n", 
	       matchqual, xlate_matchqual(matchqual));
    } else {
      WPCALL(cwp_Search(wpd,search,MAXKEYS,
			MatchAnyName,&matchqual,&pkset));
      if (showmatchqual)
	printf("Found %d keys, match quality was %d (%s).\n", 
	       pkset->KeyCount,
	       matchqual, xlate_matchqual(matchqual));
    }
    break;
  case TOKEN:
    WPCALL(wp_SetUp(user,LookupUIDLast,&search));
    if (!showmultiple) {
      WPCALL(cwp_Lookup(wpd,search,&minmatches,
			MatchAll,&matchqual,&primekey));
      if (showmatchqual)
	printf("Match quality was %d (%s).\n", 
	       matchqual, xlate_matchqual(matchqual));
    } else {
      WPCALL(cwp_Search(wpd,search,MAXKEYS,
			MatchAll,&matchqual,&pkset));
      if (showmatchqual)
	printf("Found %d keys, match quality was %d (%s).\n", 
	       pkset->KeyCount,
	       matchqual, xlate_matchqual(matchqual));
    }
    break;
  case LOGIN:
    WPCALL(cwp_GetUIDOnly(wpd,user, &primekey));
    break;
  case UID:
    WPCALL(cwp_GetNIDOnly(wpd,atoi(user), &primekey));
    break;
  case PRIMARYKEY:
    primekey = user;
    break;
  default:
    error("Bad search type.");
  }

  if(pkset!=NULL) { 
				/* We have a set of keys, iterate */
    for(i=0; i<pkset->KeyCount; ++i) {
      primekey = pkset->Keys[i];
      if (!keysonly) {
	fill_entry(wpd,primekey);
	print_entry();
      } else {
	puts((char *)primekey);
      }
    }
  } else {
    if(primekey==NULL) error("Couldn't get key.");
				/* Just one key */
    if (!keysonly) {
      fill_entry(wpd,primekey);
      print_entry();
    } else {
      puts((char *)primekey);
    }
  }
}

static void
lookup_self(wpd)
struct wp_cd *wpd;
{ /* Just lookup the current user in the WP */
  wp_PrimeKey primekey;

  WPCALL(cwp_GetNIDOnly(wpd, getnid(), &primekey));
  if (!keysonly) {
    fill_entry(wpd,primekey);
    print_entry();
  } else {
    puts((char *)primekey);
  }
}

static void
parse_args(argc, argv)
int argc;
char *argv[];
{
#define OPTION 0
#define TOGGLE 1
#define ONEARG 2

  static struct wpq_optionmap {
    char *option;
    int type;
    void (*func)();
  } map_table[] = {		/* The symbol-table */
    {"help", OPTION, usage},
    {"describefields", OPTION, describe_fields},
    {"pretty", TOGGLE, show_pretty},
    {"justkeys", TOGGLE, keys_only},
    {"empty", TOGGLE, show_empty},
    {"multiple", TOGGLE, show_multiple},
    {"readwrite", TOGGLE, use_read_write},
    {"quality", TOGGLE, show_matchqual},
    {"cell", ONEARG, set_cellname},
    {"wpdir", ONEARG, set_wpdir},
    {"key", ONEARG, set_search_key},
    { NULL, 0, 0}
  };
  int i, j, parsed, users = 0;
  char *s;

  for(i=1; i<argc; ++i) {
    s = argv[i];
    switch (*s) {
    case '-':
    case '+':
      parsed = 0;
      for(j=0; map_table[j].option != NULL; ++j) {
	if (!((*s == '+') && (map_table[j].type != TOGGLE))) {
	  if(
#ifndef DISALLOWSINGLECHAROPTIONS
	     ((*(map_table[j].option) == *(s+1)) &&
	      (*(s+2)=='\0')) ||
#endif /* DISALLOWSINGLECHAROPTIONS */
	    (strcmp(s+1, map_table[j].option) == 0)) {
	    parsed = 1;
	    switch(map_table[j].type) {
	    case OPTION:
	      (map_table[j].func)();
	      break;
	    case TOGGLE:
	      (map_table[j].func)(*s == '+');
	      break;
	    case ONEARG:
	      if (++i >= argc) error("Ran out of arguments.");
	      (map_table[j].func)(argv[i]);
	      break;
	    default:
	      error("Bad option type.");
	    }
	  }
	}
      }
      if (parsed == 0) {
	fprintf(stderr,"Bad option '%s'.\n", s);
	usage();
      }
      break;
    default:			/* a probe */
      ++users;
      lookup_user(wpdir,s);
      break;
    }
  }
				/* if no probes, check self */
  if (users == 0) lookup_self(wpdir);

  return;
}

main(argc, argv)
int argc;
char *argv[];
{
  struct CellAuth *ca;

  CheckServiceConfiguration();
  if (FindCell(ThisDomain, &ca))
    error("Couldn't FindCell(ThisDomain).");
  
  WPCALL(wp_InitializeCell(ca->CellName, &wpdir));
  setup_fields();
  parse_args(argc, argv);
  WPCALL(cwp_Terminate(wpdir));
}

