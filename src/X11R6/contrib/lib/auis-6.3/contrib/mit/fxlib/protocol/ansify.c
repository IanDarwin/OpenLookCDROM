/**********************************************************************
 * ansify.c -- utility to correct rpcgen's bad habit of constructing
 *		{ foo, bar, baz, }; <-- extra comma
 *
 * $Author: susan $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/protocol/RCS/ansify.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/protocol/RCS/ansify.c,v 1.1 1993/05/17 17:02:35 susan Exp $
 *
 * Copyright 1992 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 **********************************************************************/
#include <mit-copyright.h>

#include <stdio.h>

#define PUTC(c) if (putc((char)c, stdout)==EOF) { perror(argv[0]); exit(1); }

main(argc, argv)
     int argc;
     char *argv[];
{
  register int ch;
  int ch2, ch3;

  while((ch=getc(stdin)) != EOF) {
    if ((char)ch == ',') {
      ch2 = getc(stdin);
      ch3 = getc(stdin);
      if (ch3 != '}') PUTC(ch);
      PUTC(ch2);
      PUTC(ch3);
    } else {
      PUTC(ch);
    }
  }
  exit(0);
}
