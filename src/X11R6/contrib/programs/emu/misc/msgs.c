#ifndef lint
static char *rcsid = "msgs.c,v 1.2 1994/05/26 21:01:47 me Exp";
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
 * Error and debugging message routines.
 *
 * Author: Jordan K. Hubbard
 * Date: April 24th, 1990.
 * Description: Miscellaneous error and debugging message routines.
 *		I don't use varargs() since I end up passing the results
 *		to the toolkit routines, and vsprintf() is unfortunately
 *		not available a lot of the time.  Sometimes I hate Unix.
 *
 * Revision History:
 *
 * msgs.c,v
 * Revision 1.2  1994/05/26  21:01:47  me
 * New copyright
 *
 * Revision 1.1.1.1  1994/05/22  11:22:41  me
 * Initial import into CVS
 */

#include "common.h"

/* print an error and toss our cookies */
/*VARARGS1*/
Export void
_VA_DCL(fatal, String fmt)
{
     char errmsg[1024];
     va_list ap;

#ifdef _USE_STDARGS
     va_start(ap, fmt);
#else
     String fmt;

     va_start(ap);
     fmt = va_arg(ap, String);
#endif
     vsprintf(errmsg, fmt, ap);
     strcat(errmsg, "\n");
     XtError(errmsg);
}

/* print a warning message */
Export void
_VA_DCL(warn, String fmt)
{
     char warning[1024];
     va_list ap;

#ifdef _USE_STDARGS
     va_start(ap, fmt);
#else
     String fmt;

     va_start(ap);
     fmt = va_arg(ap, String);
#endif

     strcpy(warning, "Emu: ");
     vsprintf(&warning[strlen(warning)], fmt, ap);
     strcat(warning, "\n");
     XtWarning(warning);
}

/* print a general information message */
Export void
_VA_DCL(pmessage, String fmt)
{
     va_list ap;

#ifdef _USE_STDARGS
     va_start(ap, fmt);
#else
     String fmt;

     va_start(ap);
     fmt = va_arg(ap, String);
#endif
     fprintf(stderr, "Emu: ");
     vfprintf(stderr, fmt, ap);
     fputc('\n', stderr);
}

Export void
_VA_DCL(debug, String fmt)
{
     FILE *dout;
     va_list ap;
#ifdef _USE_STDARGS
     va_start(ap, fmt);
#else
     String fmt;

     va_start(ap);
     fmt = va_arg(ap, String);
#endif

     /* Open and close each time so that en-masse fd closes won't affect us */
     if ((dout = fopen(DEBUGFILE, "a+")) == NULL)
	  fatal("Can't open debug file %s!", DEBUGFILE);
     vfprintf(dout, fmt, ap);
     fputc('\n', dout);
     fclose(dout);
}

/* Return the type name for a type id */
Export String
reg_type_name(int n)
{
     switch (n) {
     case CB_INT_TYPE:
	  return "Int";

     case CB_STR_TYPE:
	  return "String";

     case CB_CHAR_TYPE:
	  return "Char";

     case CB_ANY_TYPE:
	  return "Any";
     }
     return "???";
}
