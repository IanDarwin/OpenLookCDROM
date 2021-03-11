

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/clients/RCS/example.c,v 1.3 1992/12/15 21:51:52 rr2b R6tape $";
#endif
#include <stdio.h>
#include <fxcl.h>

main(argc, argv)
     int argc;
     char *argv[];
{
  FX *fxp;
  long code;
  Paper p;
  FILE *pipe;

  /* Check command line usage */
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <exchange>\n", argv[0]);
    exit(1);
  }
  /* Open the file exchange */
  fxp = fx_open(argv[1], &code);
  if (!fxp) {
    com_err(argv[0], code, "while opening %s", argv[1]);
    exit(1);
  }
  /* Send the essay.dvi file */
  paper_clear(&p);
  code = fx_send_file(fxp, &p, "essay.dvi");
  if (code) {
    com_err(argv[0], code, "while sending essay.dvi");
    exit(1);
  }
  /* Open a pipe to the dvi/Postscript converter */
  pipe = popen("dvi2ps -r essay.dvi", "r");
  if (!pipe) {
    fprintf(stderr, "%s: Could not run dvi2ps\n", argv[0]);
    exit(1);
  }
  /* Send the essay.PS file */
  p.filename = "essay.PS";
  code = fx_send(fxp, &p, pipe);
  if (code) {
    com_err(argv[0], code, "while sending essay.PS");
    exit(1);
  }
  fx_close(fxp);
  (void) pclose(pipe);
  exit(0);
}
