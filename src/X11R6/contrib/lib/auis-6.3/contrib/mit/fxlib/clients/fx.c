/**********************************************************************
 * File Exchange client routines
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/fx.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/fx.c,v 1.3 1992/12/15 21:51:52 rr2b R6tape $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/fx.c,v 1.3 1992/12/15 21:51:52 rr2b R6tape $";
#endif

#include <mit-copyright.h>

 

#include <stdio.h>
#include <fxcl.h>
#include <ss/ss.h>
extern char *ctime();   /* #include <time.h> */
#include <ctype.h>

/*** Global variables ***/

extern char Course[];
extern Paperlist_res *Plist;
extern Paper Criterion;
static char *Types[] = { "*", "new", "taken", "graded", "back",
			   "handout", "exchange", "archive", "t-hand", NULL };
static PaperType Typep[] = {TYPE_WILDCARD, TURNEDIN, TAKEN, GRADED,
			      PICKEDUP, HANDOUT, EXCHANGE, TEACHERS_ARCHIVE,
			      TEACHERS_HANDOUT};

#define TYPE_INVAL ((PaperType) 9)
char *string_type();

open_course(argc, argv)
  int argc;
  char *argv[];
{
  long code;
  FX *fxp;

  if (argc != 2) {
    fprintf(stderr, "Usage: %s <course name>\n", argv[0]);
    return;
  }
  (void) strcpy(Course, argv[1]);

  fxp = fx_open(Course, &code);

  if (code)
    com_err(argv[0], code, "(%s)", Course);
  else
    printf("Connected to %s for subject %s\n", fxp->host, fxp->name);

  fx_close(fxp);
  return;
}

list_papers(argc, argv)
     int argc;
     char *argv[];
{
  FX *fxp;
  long code;
  int i = 0;
  Paperlist node;
  int host, lines, words, chars;

  host = lines = words = chars = 0;

  for (i = 1; i < argc; i++)
    if (argv[i][0] == '-')
      switch(argv[i][1]) {
      case 'h':
	host = 1;
	break;
      case 'l':
	lines = 1;
	break;
      case 'w':
	words = 1;
	break;
      case 'c':
	chars = 1;
	break;
      }

  if ((fxp = fx_open(Course, &code)) == NULL) {
    com_err(argv[0], code, "(%s)", Course);
    return;
  }

  fx_list_destroy(&Plist);
  code = fx_list(fxp, &Criterion, &Plist);
  if (code) {
    com_err(argv[0], code, "while getting list");
    fx_close(fxp);
    return;
  }

  i = 0;
  printf(" No. type   asgn author   x filename         date\n");
  for (node = Plist->Paperlist_res_u.list; node != NULL; node = node->next) {
    i++;
    printf("%3d: %-8s %2d %-8s %c %-16s %-16.16s", i,
	   string_type(node->p.type), node->p.assignment,
	   node->p.author, (node->p.flags & PAPER_EXECUTABLE) ? 'x' : ' ',
	   node->p.filename, ctime(&node->p.created.tv_sec));
    if (strcmp(node->p.author, node->p.owner)) printf(" %s", node->p.owner);
    if (host) printf(" %s", node->p.location.host);
    if (lines) printf(" %d", node->p.lines);
    if (words) printf(" %d", node->p.words);
    if (chars) printf(" %d", node->p.size);
    newline(1);
  }
  fx_close(fxp);
}

char *
string_type(type)
     PaperType type;
{
  int i;

  for(i=0; Types[i] != NULL; i++)
    if (type == Typep[i]) return(Types[i]);

  return("unknown");
}

spec_type(argc, argv)
     int argc;
     char *argv[];
{
  int i;

  if (argc == 2) {
    for(i=0; Types[i] != NULL; i++)
      if (strcmp(argv[1], Types[i]) == 0) {
	Criterion.type = Typep[i];
	return;
      }
  }
  printf("Usage: %s <type>\nwhere <type> is one of", argv[0]);
  for(i=0; Types[i] != NULL; i++)
    printf(" %s", Types[i]);
  newline(1);
  return;
}

spec_assignment(argc, argv)
     int argc;
     char *argv[];
{
  if (argc == 2) {
    if (isdigit(argv[1][0])) {
      Criterion.assignment = atoi(argv[1]);
      return;
    }
    if (argv[1][0] == '*') {
      Criterion.assignment = ASSIGNMENT_WILDCARD;
      return;
    }
  }
  printf("Usage: %s <asgn>\n", argv[0]);
  printf("where <asgn> is a number or * for any assignment\n");
}

spec_author(argc, argv)
     int argc;
     char *argv[];
{
  static char author[90];

  if (argc == 2) {
    if (strlen(argv[1]) < 90) {
      strcpy(author, argv[1]);
      Criterion.author = author;
      return;
    }
    else {
      printf("Author should be at most 89 characters.\n");
      return;
    }
  }
  printf("Usage: %s <author>\n", argv[0]);
  return;
}

spec_filename(argc, argv)
     int argc;
     char *argv[];
{
  static char filename[33];

  if (argc == 2) {
    if (strlen(argv[1]) < 33) {
      (void) strcpy(filename, argv[1]);
      Criterion.filename = filename;
      return;
    }
    else {
      printf("Filename should be at most 32 characters.\n");
      return;
    }
  }
  printf("Usage: %s <filename>\n", argv[0]);
  return;
}

clear_specs(argc, argv)
     int argc;
     char *argv[];
{
  if (argc == 1) {
    Criterion.type = TYPE_WILDCARD;
    Criterion.assignment = ASSIGNMENT_WILDCARD;
    Criterion.author = AUTHOR_WILDCARD;
    Criterion.filename = FILENAME_WILDCARD;
    return;
  }
  printf("Usage: %s\n", argv[0]);
  return;
}

acl_list(argc, argv)
     int argc;
     char *argv[];
{
  FX *fxp;
  long code;
  stringlist_res *acl;
  stringlist node;

  if (argc == 2) {
    if ((fxp = fx_open(Course, &code)) == NULL) {
      com_err(argv[0], code, "(%s)", Course);
      return;
    }
    if (code = fx_acl_list(fxp, argv[1], &acl)) {
      com_err(argv[0], code, NULL);
      return;
    }
    for (node = acl->stringlist_res_u.list; node; node = node->next)
      printf("%s\n", node->s);
    fx_close(fxp);
    fx_acl_list_destroy(&acl);
    return;
  }
  printf("Usage: %s <acl>\n", argv[0]);
  printf("where <acl> is %s %s or %s\n", ACL_TURNIN, ACL_GRADER, ACL_MAINT);
  return;
}

acl_add(argc, argv)
     int argc;
     char *argv[];
{
  FX *fxp;
  long code;
  int i;

  if (argc > 2) {
    if ((fxp = fx_open(Course, &code)) == NULL) {
      com_err(argv[0], code, "(%s)", Course);
      return;
    }
    for (i = 2; i<argc; i++)
      if (code = fx_acl_add(fxp, argv[1], argv[i])) {
	com_err(argv[0], code, "(%s)", argv[i]);
	fx_close(fxp);
	return;
      }
    fx_close(fxp);
    printf("Successfully updated %s\n", argv[1]);
    return;
  }
  printf("Usage: %s <acl> <user1> <user2> ...\n", argv[0]);
  printf("where <acl> is %s %s or %s\n", ACL_TURNIN, ACL_GRADER, ACL_MAINT);
  return;
}

acl_del(argc, argv)
     int argc;
     char *argv[];
{
  FX *fxp;
  long code;
  int i;

  if (argc > 2) {
    if ((fxp = fx_open(Course, &code)) == NULL) {
      com_err(argv[0], code, "(%s)", Course);
      return;
    }
    for (i = 2; i<argc; i++)
      if (code = fx_acl_del(fxp, argv[1], argv[i])) {
	com_err(argv[0], code, "(%s)", argv[i]);
	fx_close(fxp);
	return;
      }
    fx_close(fxp);
    printf("Successfully updated %s\n", argv[1]);
    return;
  }
  printf("Usage: %s <acl> <user1> <user2> ...\n", argv[0]);
  printf("where <acl> is %s %s or %s\n", ACL_TURNIN, ACL_GRADER, ACL_MAINT);
  return;
}

cat_papers(argc, argv)
     int argc;
     char *argv[];
{
  int i, j, hi, lo;
  Paperlist node;
  FX *fxp;
  long code;
  int anything = 0;

  if ((fxp = fx_open(Course, &code)) == NULL) {
    com_err(argv[0], code, "(%s)", Course);
    return;
  }

  for (i=1; i<argc; i++) {
    hi = high_bound(argv[i]);
    lo = low_bound(argv[i]);
    j = 0;
    for (node = Plist->Paperlist_res_u.list; node; node = node->next) {
      j++;
      if (j < lo || j > hi) continue;
      anything = 1;
      code = fx_retrieve(fxp, &(node->p), stdout);
      if (code) com_err(argv[0], code, "(# %d)", j);
    }
  }
  fx_close(fxp);
  if (!anything) fprintf(stderr, "%s:  No files specified.\n", argv[0]);
  return;
}

delete_papers(argc, argv)
     int argc;
     char *argv[];
{
  int i, j, hi, lo;
  Paperlist node;
  FX *fxp;
  long code;
  int anything = 0;

  if ((fxp = fx_open(Course, &code)) == NULL) {
    com_err(argv[0], code, "(%s)", Course);
    return;
  }

  for (i=1; i<argc; i++) {
    hi = high_bound(argv[i]);
    lo = low_bound(argv[i]);
    j = 0;
    for (node = Plist->Paperlist_res_u.list; node; node = node->next) {
      j++;
      if (j < lo || j > hi) continue;
      anything = 1;
      code = fx_delete(fxp, &(node->p));
      if (code) com_err(argv[0], code, "(# %d)", j);
    }
  }
  fx_close(fxp);
  if (!anything) fprintf(stderr, "%s:  No files specified.\n", argv[0]);
  return;
}

fake_grade(argc, argv)
     int argc;
     char *argv[];
{
  int i, j, hi, lo;
  Paperlist node;
  FX *fxp;
  long code;
  int anything = 0;
  Paper gpaper;

  if ((fxp = fx_open(Course, &code)) == NULL) {
    com_err(argv[0], code, "(%s)", Course);
    return;
  }

  for (i=1; i<argc; i++) {
    hi = high_bound(argv[i]);
    lo = low_bound(argv[i]);
    j = 0;
    for (node = Plist->Paperlist_res_u.list; node; node = node->next) {
      j++;
      if (j < lo || j > hi) continue;
      anything = 1;
      paper_copy(&node->p, &gpaper);
      gpaper.type = GRADED;
      code = fx_move(fxp, &node->p, &gpaper);
      if (code) com_err(argv[0], code, "(# %d)", j);
    }
  }
  fx_close(fxp);
  if (!anything) fprintf(stderr, "%s:  No files specified.\n", argv[0]);
  return;
}

handout_papers(argc, argv)
     int argc;
     char *argv[];
{
  int i, j, hi, lo;
  Paperlist node;
  FX *fxp;
  long code;
  int anything = 0;
  Paper hpaper;

  if ((fxp = fx_open(Course, &code)) == NULL) {
    com_err(argv[0], code, "(%s)", Course);
    return;
  }

  for (i=1; i<argc; i++) {
    hi = high_bound(argv[i]);
    lo = low_bound(argv[i]);
    j = 0;
    for (node = Plist->Paperlist_res_u.list; node; node = node->next) {
      j++;
      if (j < lo || j > hi) continue;
      anything = 1;
      paper_copy(&node->p, &hpaper);
      hpaper.type = HANDOUT;
      code = fx_copy(fxp, &node->p, &hpaper);
      if (code) com_err(argv[0], code, "(# %d)", j);
    }
  }
  fx_close(fxp);
  if (!anything) fprintf(stderr, "%s:  No files specified.\n", argv[0]);
  return;
}

grade_papers(argc, argv)
     int argc;
     char *argv[];
{
  int i, j, hi, lo;
  Paperlist node;
  FX *fxp;
  long code;
  int anything = 0;
  char filename[256];
  Paper taken;
  Paperlist_res *plist;
  FILE *fp;

  if ((fxp = fx_open(Course, &code)) == NULL) {
    com_err(argv[0], code, "(%s)", Course);
    return;
  }

  for (i=1; i<argc; i++) {
    hi = high_bound(argv[i]);
    lo = low_bound(argv[i]);
    j = 0;
    for (node = Plist->Paperlist_res_u.list; node; node = node->next) {
      j++;
      if (j < lo || j > hi) continue;
      anything = 1;
      bcopy(&(node->p), &taken, sizeof(Paper));
      taken.type = TAKEN;
      code = fx_move(fxp, &(node->p), &taken);
      if (code) com_err(argv[0], code, "(# %d)", j);
    }
  }
  if (!anything) {
    fprintf(stderr, "%s:  No files specified.\n", argv[0]);
    fx_close(fxp);
    return;
  }
  anything = j = 0;

  bzero(&taken, sizeof(Paper));
  taken.location.host = ID_WILDCARD;
  taken.author = AUTHOR_WILDCARD;
  taken.owner = fxp->owner;
  taken.filename = FILENAME_WILDCARD;
  taken.assignment = ASSIGNMENT_WILDCARD;
  taken.type = TAKEN;

  code = fx_list(fxp, &taken, &plist);
  if (code) {
    com_err(argv[0], code, "retrieving list");
    goto GRADE_PAPERS_CLEANUP;
  }

  for (node = plist->Paperlist_res_u.list; node; node = node->next) {
    j++;
    (void) sprintf(filename, "/tmp/%s:%d:%d:%s", argv[0],
		   getpid(), j, node->p.filename);
    if ((fp = fopen(filename, "w")) == NULL) {
      perror(filename);
      goto GRADE_PAPERS_CLEANUP;
    }
    code = fx_retrieve(fxp, &(node->p), fp);
    fclose(fp);
    if (code) {
      com_err(argv[0], code, "(%s by %s for asgn %d)",
	      node->p.filename, node->p.author, node->p.assignment);
      (void) unlink(filename);
      goto GRADE_PAPERS_CLEANUP;
    }
  }
    
 GRADE_PAPERS_CLEANUP:
  fx_list_destroy(&plist);
  fx_close(fxp);
  return;
}

quit(argc, argv)
     int argc;
     char *argv[];
{
  if (argc == 2) exit(atoi(argv[1]));
  else exit(0);
}

newline(count)
     int count;
{
  int i;

  for(i=0; i<count; i++)
    (void) putchar('\n');
}

describe_papers(argc, argv)
     int argc;
     char *argv[];
{
  int i, j, hi, lo;
  Paperlist node;
  int anything = 0;
  char buf[1024];

  for (i=1; i<argc; i++) {
    hi = high_bound(argv[i]);
    lo = low_bound(argv[i]);
    j = 0;
    for (node = Plist->Paperlist_res_u.list; node; node = node->next) {
      j++;
      if (j < lo || j > hi) continue;
      anything = 1;
      strcpy(buf, node->p.desc);
      for (i = 0; buf[i] != '\0'; i++)
	if (buf[i] == (char)1) buf[i] = '\n';
      printf("%s\n", buf);
    }
  }
  if (!anything) fprintf(stderr, "%s:  No files specified.\n", argv[0]);
  return;
}

chdesc(argc, argv)
     int argc;
     char *argv[];
{
  int i, j, hi, lo;
  Paperlist node;
  FX *fxp;
  long code;
  int anything = 0;
  Paper newpaper;
  char buf[1024];

  if (argc < 3) {
    printf("Usage: %s <paper> description\n", argv[0]);
    return;
  }

  /* first argument is group of papers to change desc */
  hi = high_bound(argv[1]);
  lo = low_bound(argv[1]);

  /* remaining arguments are description */
  (void) strcpy(buf, argv[2]);
  for(i=3; i<argc; i++) (void) strcat(strcat(buf, " "), argv[i]);

  if ((fxp = fx_open(Course, &code)) == NULL) {
    com_err(argv[0], code, "(%s)", Course);
    return;
  }

  j = 0;
  for (node = Plist->Paperlist_res_u.list; node; node = node->next) {
    j++;
    if (j < lo || j > hi) continue;
    anything = 1;
    paper_copy(&node->p, &newpaper);
    newpaper.desc = buf;
    code = fx_move(fxp, &node->p, &newpaper);
    if (code) com_err(argv[0], code, "(# %d)", j);
  }

  fx_close(fxp);
  return;
}
