#if !defined(lint) && !defined(__clipper__)
static char *rcsid = "parser.c,v 1.2 1994/05/27 06:21:26 me Exp";
#endif

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/*
 * These are the main entry points for the parser section.
 *
 * Author: Jordan K. Hubbard
 * Date: October 7th, 1990.
 * Description: General parser init code and switch table point for
 * all hard-coded parsers.
 *
 * Revision History:
 *
 * parser.c,v
 * Revision 1.2  1994/05/27  06:21:26  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:45  me
 * Initial import into CVS
 *
 * Revision 1.10  92/10/16  16:51:52  me
 * More fixes by Steve Crooks
 * 
 * Revision 1.8  92/02/26  11:42:14  me
 * Steve Crooks' clix port and general code cleanup
 * 
 * Revision 1.4  91/01/28  10:56:30  jkh
 * Changed EIO handling for sun. I did have it wrong. Double sigh.
 * 
 * Revision 1.3  91/01/28  10:42:57  jkh
 * Added EIO as "reasonable" error for Suns who seem to like to generate
 * that one when reading from a non-blocking fd that has no output ready
 * yet. I hope I don't come to regret this..
 */

#include "TermP.h"

Import void parseDumb(TermWidget, caddr_t, caddr_t);
Import void parseGeneric(TermWidget, caddr_t, caddr_t);
Import void parseHEmu(TermWidget, caddr_t, caddr_t);

Local ParserTable parseTab[] = {
     {  "emu",		parseHEmu,	NULL	},
     {	"dumb",		parseDumb,	NULL	},
     {	"tty",		parseDumb,	NULL	},
     {	NULL,		parseGeneric,	NULL	},
};

/* Used to both add and delete callback entries */
Local void
modify_table_callbacks(TermWidget w, int idx, void (*fptr)())
{
     if (parseTab[idx].in_parser)
	  (*fptr)(w, XpNinParser, parseTab[idx].in_parser, w->term.master);
     if (parseTab[idx].out_parser)
	  (*fptr)(w, XpNoutParser, parseTab[idx].out_parser, w->term.master);

     /*
      * If we're referring to the generic input parser, make sure we
      * initialize it first.
      */
     if (parseTab[idx].in_parser == parseGeneric)
	  parse_init(w);
}

/*
 * Look for and install a parser for the terminal type specified in the
 * term widget.
 */
Export Boolean
parser_install(TermWidget w)
{
     int i, added_hard = 0;
     String ops, term = w->term.term_type;

     for (i = 0; parseTab[i].name; i++) {
	  if (!strcmp(term, parseTab[i].name)) {
	       modify_table_callbacks(w, i, XtAddCallback);
	       added_hard = 1;
	       break;
	  }
     }

     if (!added_hard) {
	  /* Add default if we fell off the end and it's never been added */
	  if (XtHasCallbacks((Widget)w, XpNinParser) == XtCallbackHasNone)
	       modify_table_callbacks(w, i, XtAddCallback);
     
#ifndef NO_GENERIC_PARSER
	  if (ops = get_sub_resource_string((Widget)w, term, "ops", "Ops")) {
	       ops = backslash_convert(ops);
	       parse_add(w, ops);
	  }
	  else 
	       return FALSE;
#endif
     }

     /* Get ROPS either way */
     if ((ops = get_sub_resource_string((Widget)w, term, "rops", "Rops"))) {
	  ops = backslash_convert(ops);
	  rparse_add(w, ops);
     }
     return TRUE;
}

Export void
parser_remove(TermWidget w, String term)
{
     int i;

     for (i = 0; parseTab[i].name; i++) {
	  if (!strcmp(term, parseTab[i].name)) {
	       modify_table_callbacks(w, i, XtRemoveCallback);
	       break;
	  }
     }

     /* Remove default if we fell off the end */
     if (!parseTab[i].name)
	  modify_table_callbacks(w, i, XtRemoveCallback);
}

/* The input parser */
/*ARGSUSED*/
Export void
iparse(TermWidget w, int *fdp, XtInputId id)
{
     Import int errno;
     Import int read();
     int nread, fd = *fdp;

     if (fd < 0)
	  fatal("parse called with negative file descriptor (%d).", fd);
     do {
	  char *inbuf = ((char *)(w->term.buf + w->term.chars_to_parse));

	  nread = read(fd, inbuf, w->term.read_size);
     } while ((nread == -1) && ((errno == EAGAIN) || (errno == EINTR)));
     if (nread == -1) {
#if 0  /* I don't open the slave side until I fork, so this will
	  result in errors until that happens. */
	  warn("read error on fd %d in parse, errno = %d.", fd, errno);
#endif
	  return;
     }
     w->term.chars_to_parse += nread;
#ifdef DEBUG
     debug("iparse: read %d bytes", nread);
#endif

     /* Call everyone on what we read */
     XtCallCallbackList((Widget)w, w->term.in_parse, (XtPointer)w->term.buf);
}

/* Rarely used, but where we dispatch output parsing callbacks */
/*ARGSUSED*/
Export void
oparse(XtPointer client_data, int *fdp, XtInputId *id)
{
     XtCallCallbackList((Widget)client_data,
			((TermWidget)client_data)->term.out_parse,
			(XtPointer)*fdp);
}
