/**********************************************************************
 * File Exchange turnin client
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/fxbash.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/fxbash.c,v 1.3 1992/12/15 21:51:52 rr2b R6tape $
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/fxbash.c,v 1.3 1992/12/15 21:51:52 rr2b R6tape $";
#endif

#include <mit-copyright.h>

 

#include <stdio.h>
#include <fxcl.h>
#include <sys/types.h>
#include <strings.h>

log(msg)
     char *msg;
{
  static FILE *fp = NULL;
  time_t t;

  if (!fp) {
    fp = popen("/bin/mail brlewis", "w");
    if (!fp) {
      perror("fxbash");
      exit(1);
    }
    fprintf(fp, "To: brlewis@ATHENA.MIT.EDU\nSubject: fxbash test\n");
  }
  if (!msg) {			/* passing NULL means we're done */
    pclose(fp);
    return;
  }
  t = time(NULL);
  fprintf(fp, "%24.24s  %s\n", (char *) ctime(&t), msg);
  return;
}

main(argc, argv)
     int argc;
     char *argv[];
{
  FX *fxp;
  long code;
  Paper p;
  Paperlist_res *plist;
  char *course = "bash";
  char msg[256], host[32];

  paper_clear(&p);
  p.type = TURNEDIN;

  gethostname(host, 32);
  sprintf(msg, "%s started by %s@%s", argv[0], getenv("USER"), host);
  log(msg);

  fxp = fx_open(course, &code);
  sprintf(msg, "After fx_open: %s", error_message(code));
  log(msg);
  if (!fxp) goto BAIL_OUT;

  code = fx_send_file(fxp, &p, "/mit/eos/man/cat1/dump.1");
  sprintf(msg, "After fx_send_file: %s", error_message(code));
  log(msg);

  code = fx_list(fxp, &p, &plist);
  sprintf(msg, "After fx_list: %s", error_message(code));
  log(msg);

  fx_close(fxp);
  strcpy(msg, "done");
  log(msg);

 BAIL_OUT:
  log(NULL);
  exit(0);
}
