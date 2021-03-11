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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/dlistc/RCS/dlc.c,v 1.16 1993/05/05 19:49:43 susan Exp $";
#endif

/*

  dlc.c -- Distribution List "Compiler"

  Takes a dlist.protodl file, which may contain aliases, and produces a
  dlist.dl file.  Basically, copies the dlist.protodl file to the
  dlist.dl file, expanding aliases as found in $HOME/.AMS_aliases.

*/

#include <andrewos.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>		/* this may cause portability problems */
#include <sys/param.h>
#include <pwd.h>
#include <util.h>
#include <readalias.h>


/* Format of the dlist.dl file:

-- comments are lines starting with a semi-colon
-- blank lines are ignored
-- addresses can be spread across lines, separated by commas
-- Several fields:
      Distribution-errors-to:
      Distribution-content:
      Distribution-if-type-unsupported:
      Distribution-errors-to-originator:


*/

#define WHITESPACE "\b\f\n\r\t\v "

extern int errno;

#define USAGE "[-d][-v][-a aliasfile][-u username] [infile [outfile]]"

struct processed_arguments {
  char *program_name;
  char tildeuser[MAXPATHLEN];	/* bogus--I don't want to allocate space for copy */
  char aliasfilename[MAXPATHLEN];
  int validate_mode;
  int debug_mode;
  FILE *infile, *outfile;
};

static char *GetNextLine(f)
FILE *f;
{
  static char *buf = NULL;
  static long bufsize = 0L;
  long count = 0L;
  int c;

  if (feof(f))
    return(NULL);

  if (buf == NULL) {
    bufsize = 255L;
    if ((buf = (char *) malloc(bufsize)) == NULL) {
      buf = NULL;
      bufsize = 0L;
      return(NULL);
    }
  }

  while(!(feof(f)) && ((c=fgetc(f))>EOF)) {
    if (count+1>bufsize) {
      bufsize = (bufsize + 1)*2;
      if ((buf = (char *)realloc(buf,bufsize))==NULL) {
	buf = NULL;
	bufsize = 0L;
	return(NULL);
      }
    }

    if (c=='\n') {
      buf[count++] = '\0';
      return(buf);
    } else {
      buf[count++] = c;
    }
  }
  buf[count] = '\0';

  if (count>0)
    return(buf);
  else
    return(NULL);
}

static void ProcessArguments(argc,argv, out)
int argc;
char **argv;
struct processed_arguments *out;
{
  char *progname;
  int argcounter;

  ((progname = strrchr(argv[0], '/'))!=NULL)?++progname:(progname=argv[0]);
  out->program_name = progname;

  out->infile = stdin;
  out->outfile = stdout;
  out->validate_mode = 0;
  out->debug_mode = 0;
  {
    struct passwd *pwent;
    
    if ((pwent = getpwuid(geteuid()))== NULL) {
      fprintf(stderr, "Couldn't find your pwent.\n");
      exit(3);
    }
    strcpy(out->aliasfilename, pwent->pw_dir);
    strcat(out->aliasfilename, DEFAULTALIASFILE);
    strcpy(out->tildeuser, pwent->pw_name);
  }

  ++argv;
  argcounter = 0;
  while(argv[0]) {
    if (argv[0][0] == '-') {
      switch(argv[0][1]) {
      case 'a':
	if (argv[1] != NULL) {
	  strncpy(out->aliasfilename,argv[1],MAXPATHLEN);
	  ++argv;
	} else {
	  fprintf(stderr, "%s:  option %s requires an alias file name.\n",
		  progname, argv[0]);
	  exit(1);
	}
	break;
      case 'd':
	out->debug_mode = 1;
	break;
      case 'u':
	if (argv[1] != NULL) {
	  struct passwd *pwent;
	  
	  if ((pwent = getpwnam(argv[1])) == NULL) {
	    fprintf(stderr, "Couldn't find user %s.\n", argv[1]);
	    exit(3);
	  }
	  strcpy(out->aliasfilename, pwent->pw_dir);
	  strcat(out->aliasfilename, DEFAULTALIASFILE);
	  strcpy(out->tildeuser, pwent->pw_name);
	  ++argv;
	} else {
	  fprintf(stderr, "%s:  option %s requires a user name.\n",
		  progname, argv[0]);
	  exit(1);
	}
	break;
      case 'v':
	out->validate_mode = 1;
	break;
      default:
	fprintf(stderr, "%s:  unknown option %s.\n", progname, argv[0]);
	fprintf(stderr, "Usage:  %s %s.\n", progname, USAGE);
	exit(1);
      }
    } else {
      switch(++argcounter) {
      case 1:
	if ((out->infile = fopen(argv[0], "r")) == NULL) {
	  fprintf(stderr,"Couldn't open input dlist proto file '%s' (error %d, '%s').\n", 
		  argv[0], errno, UnixError(errno));
	  exit(2);
	}
	break;
      case 2:
	if ((out->outfile = fopen(argv[0], "w")) == NULL) {
	  fprintf(stderr,"Couldn't open output dlist file '%s' (error %d, '%s').\n", 
		  argv[0], errno, UnixError(errno));
	  exit(2);
	}
	break;
      default:
	fprintf(stderr, "%s:  unknown argument %s.\n", progname, argv[0]);
	fprintf(stderr, "Usage:  %s %s.\n", progname, USAGE);
	exit(1);
      }
    }
    ++argv;
  }
  return;
}

static void ExpandSimpleAddress(a, aliases, outfile, validatep)
PARSED_ADDRESS *a;
alias_set_t aliases;
FILE *outfile;
int validatep;
{
  extern void ExpandAliases();

  if (aliases) {
    if (a->Hosts->Next == a->Hosts) {
      alias_t refalias;
    
      if ((refalias = FindAlias(a->LocalPart, aliases)) != NULL) {
        if (!(refalias->circular)) {
	  ExpandAliases(ALIASMEMBERS(refalias), aliases, outfile, validatep);
	  return;
        }
      }
    } else {
      char *longname;
      /* resolve hostname aliases */
      FOR_ALL_REVERSE_HOSTS
        (h, a,
       
         if ((longname = FindHost(h->Name, aliases)) != NULL) {
	   h->Name = longname;
         }
         );
    }
  }

  if (validatep) {
    char *unparsed, *validated;
    
    if ((unparsed = FlatUnparseAddress(a)) == NULL) {
      fprintf(stderr, "Warning: unparse failed!\n");
    } else {
      if (ValidateFwdAddr(unparsed, &validated) != 0) {
	fprintf(stderr, 
		"Warning: validation failed on address '%s' because\n\t``%s'',\n\tinserted anyway.\n",
		unparsed, fwdvalid_msgbuf);
	fprintf(outfile, "%s,\n", unparsed);
      } else {
	fprintf(outfile, "%s,\n", validated);
      }
    }
  } else {
    fprintf(outfile, "%s,\n", FlatUnparseAddress(a));
  }
}

static void ExpandAliases(pa, aliases, outfile, validatep)
PARSED_ADDRESS *pa;
alias_set_t aliases;
FILE *outfile;
int validatep;
{
  FOR_ALL_ADDRESSES
    (a, pa,
     switch(a->Kind) {
     case SIMPLE_ADDRESS:
       ExpandSimpleAddress(a, aliases, outfile, validatep);
       break;
     case GROUP_ADDRESS:
       ExpandAliases(a->Members, aliases, outfile, validatep);
       break;
     }
     );
}

static void ProcessInput(infile,outfile,aliases,validatep)
FILE *infile, *outfile;
alias_set_t aliases;
int validatep;
{
  static char *DistContent = "Distribution-Content:";
  char *line;
  char *DistContBuf = NULL;
  int lineno = 0;
  char *p;

  while (((line = GetNextLine(infile)) != NULL)
	 && (ULstrncmp(line,DistContent, strlen(DistContent)) != 0)) {
    lineno++;
    fprintf(outfile, "%s\n", line); /* pass any lines before DistContent */
  }

  if (line != NULL) {
    fprintf(outfile, "%s\n", DistContent);
    line = line + strlen(DistContent);
    do {
	if (p = strchr(line, ';')) *p = '\0';  /* Strip comment */
	if (DistContBuf == NULL) {
	  if ((DistContBuf = (char *)malloc(strlen(line)+2)) == NULL) {
	    fprintf(stderr, "Out of memory.\n");
	    exit(5);
	  }
	  strcpy(DistContBuf, line);
	} else {
	  if ((DistContBuf = (char *)realloc(DistContBuf, strlen(DistContBuf)+strlen(line)+1)) == NULL) {
	    fprintf(stderr, "Out of memory.\n");
	    exit(5);
	  }
	}
	strcat(DistContBuf, line);
	strcat(DistContBuf, "\n");
    } while ((line = GetNextLine(infile)) != NULL);
    if (DistContBuf != NULL) {
      int code;
      PARSED_ADDRESS *pa;

      switch(code = ParseAddressList(DistContBuf, &pa)){
      case PA_OK:
	ExpandAliases(pa, aliases, outfile, validatep);
	break;
      case PA_SYNTAX_ERROR:
	/* Try to find out where */
	for (p = strchr(DistContBuf, '\n'); p; p = strchr(p+1, '\n')) {
	    lineno++;
	    *p = '\0';
	    if (ParseAddressList(DistContBuf, &pa) != PA_OK) break;
	    FreeAddressList(pa);
	    *p = '\n';
	}
	if (!p) lineno++;
	    
	fprintf(stderr, "Syntax error in distribution contents, probably around line %d.\n", lineno);
	exit(8);
      case PA_NO_MEM:
	fprintf(stderr, "Out of memory.\n");
	exit(5);
      default:
	fprintf(stderr, "Bad parse of distribution contents (%d).\n", code);
	exit(8);
      }
    }
  }
}

main(argc,argv)
int argc;
char **argv;
{
  alias_set_t aliasset = NULL;
  struct processed_arguments params;
  FILE *aliasfile;

  ProcessArguments(argc,argv, &params);
  fwdvalid_SetTildeUser(params.tildeuser);
  if ((aliasfile = fopen(params.aliasfilename, "r")) == NULL && errno != ENOENT) {
    fprintf(stderr, "Couldn't open alias file %s (error %d, '%s').\n",
	    params.aliasfilename, errno, UnixError(errno));
    exit(4);
  }
  if (aliasfile && (aliasset = ReadAliases(aliasfile)) == NULL) {
    fprintf(stderr, "Couldn't read aliases.\n");
    exit(4);
  }
  if (aliasset && params.debug_mode)
    DumpAliases(stderr, aliasset);
  ProcessInput(params.infile, params.outfile, aliasset, params.validate_mode);

  return(0);
}
