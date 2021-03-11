#ifndef lint
static char *rcsid = "rdb.c,v 1.2 1994/05/26 21:01:49 me Exp";
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
 * Useful functions for dealing with the resource Database
 *
 * Author: Michael Elbel
 * Date: 19. Juni 1990
 * Description: This module contains some routines for dealing with the
 * 		resource database.
 *
 * Revision History:
 *
 * rdb.c,v
 * Revision 1.2  1994/05/26  21:01:49  me
 * New copyright
 *
 * Revision 1.1.1.1  1994/05/22  11:22:41  me
 * Initial import into CVS
 *
 * Revision 1.5  92/02/26  11:35:56  me
 * Steve Crooks clix port and general code cleanup
 * 
 * Revision 1.3  90/10/04  00:31:18  jkh
 * Removed warning message; it's not always wanted.
 */

#include "common.h"
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

/*
 * Get Subresources for the given emulation as string
 *
 * The Subresource path starts at the parent of the current widget, using
 * the current emulation's name (term_type), the different parts of
 * the Subresource are delimited by dashes.
 *
 * So Resources would look like *.term.<term_type>-<name>
 */
Export String get_sub_resource_string(Widget w, String term_type,
				      String name, String class)
{
     String retval;
     char nbuffer[100];
     char cbuffer[100];
     
     XtResource res;
     
     res.resource_name = nbuffer;
     res.resource_class = cbuffer;
     res.resource_type = XtRString;
     res.resource_size = sizeof(String);
     res.resource_offset = 0;
     res.default_type = XtRImmediate;
     res.default_addr = NULL;
	  
     if (term_type) {
	  strcpy(nbuffer, term_type);
	  strcpy(cbuffer, term_type);
	  
	  strcat(nbuffer, "-");
	  strcat(cbuffer, "-");
	  
	  strcat(nbuffer, name);
	  strcat(cbuffer, class);
     }
     else {
	  strcpy(nbuffer, name);
	  strcpy(cbuffer, class);
	  
     }
     
     XtGetSubresources(XtParent(w), &retval, "term", "Term",
		       &res, 1, NULL, 0);
     
     if (!retval) {
	  warn("get_sub_resource_string: Couldn't get resource '*term.%s' or '*Term.%s",
	       nbuffer, cbuffer);
     }
     return retval;
}
