/**********************************************************************
 * File Exchange fxcreate client
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/fxcreate.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/fxcreate.c,v 1.3 1992/12/15 21:51:52 rr2b R6tape $
 *
 * Copyright 1989, 1990 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 **********************************************************************/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/fxcreate.c,v 1.3 1992/12/15 21:51:52 rr2b R6tape $";
#endif

#include <mit-copyright.h>

 

#include <stdio.h>
#include <fxcl.h>

FX *
create_course(fxp, module, course)
     FX *fxp;
     char *module, *course;
{
  long code;

  code = fx_create(fxp, course);
  fx_close(fxp);
  if (code) {
    com_err(module, code, "trying to create %s", course);
    exit(1);
  }
  if ((fxp = fx_open(course, &code)) == NULL) {
    com_err(module, code, "while connecting to new course.");
    exit(1);
  }
  /* "*.*" works and "*" doesn't because the current acl lib is weird */
  if (code = fx_acl_add(fxp, ACL_TURNIN, "*.*"))
    com_err(module, code, "allowing public turnin access.");
  return(fxp);
}

main(argc, argv)
  int argc;
  char *argv[];
{
  FX *fxp;
  long code, code2;
  int i, verbose = 1, specified = 0, created = 0;
  char *course = NULL;
  static char USAGE[] = "Usage: %s [ options ] course [ grader ... ]\n";

  if (argc < 2) {
    fprintf(stderr, USAGE, argv[0]);
    exit(1);
  }

  /* First authenticate to the fxserver */
  if ((fxp = fx_open("", &code)) == NULL) {
    com_err(argv[0], code, "while connecting.");
    exit(1);
  }

  for(i=1; i<argc; i++) {
    if (argv[i][0] == '-') {
      switch(argv[i][1]) {
      case 'q':
        verbose = 0;
        break;
      case 'v':
        verbose = 1;
        break;
      case 'c':
	course = argv[++i];
	specified++;
	fxp = create_course(fxp, argv[0], course);
	if (verbose) printf("Created %s file exchange.\n", course);
	break;
      default:
	fprintf(stderr, USAGE, argv[0]);
	exit(1);
      }
      continue;
    }

    if (!specified) {
      course = argv[i];
      specified++;
      fxp = create_course(fxp, argv[0], course);
      if (verbose) printf("Created %s file exchange.\n", course);
      continue;
    }

    if (code = fx_acl_add(fxp, ACL_GRADER, argv[i]))
      com_err(argv[0], code, "giving %s grader access.", argv[i]);
    if (code2 = fx_acl_add(fxp, ACL_MAINT, argv[i]))
      com_err(argv[0], code2, "giving %s maintainer access.", argv[i]);
    if (verbose && !code && !code2) {
      printf("\tAdded %s to access control lists for %s.\n",
	     full_name(argv[i]), course);
    }
  }
  fx_close(fxp);
  exit(0);
}
