/* Copyright 1992 NEC Corporation, Tokyo, Japan.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of NEC
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  NEC Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
 * NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
 * OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE. 
 */

#ifndef lint
static char rcs_id[] = "$Id: canlisp.c,v 1.4 1993/04/02 02:39:50 kon Exp $";
#endif

#include <canna/jrkanji.h>
#include <stdio.h>

static void
usage()
{
  fprintf(stderr, 
	  "Usage: canlisp [{-cs|-cannaserver} servername] [-f .canna-file]\n");
  exit(1);
}

main(argc, argv)
int argc;
char *argv[];
{
  register int i;

  for (i = 1 ; i < argc ; i++) {
    if ((!strcmp(argv[i], "-cs") || !strcmp(argv[i], "-cannaserver")) &&
	++i < argc) {
      wcKanjiControl(0, KC_SETSERVERNAME, argv[i]);
    }
    else if (!strcmp(argv[i], "-f") && ++i < argc) {
      wcKanjiControl(0, KC_SETINITFILENAME, argv[i]);
    }
    else {
      usage();
      /* NOTREACHED */
    }
  }
  wcKanjiControl(0, KC_INITIALIZE, 0);
  wcKanjiControl(0, KC_LISPINTERACTION, 0);
  wcKanjiControl(0, KC_FINALIZE, 0);
  exit(0);
}
