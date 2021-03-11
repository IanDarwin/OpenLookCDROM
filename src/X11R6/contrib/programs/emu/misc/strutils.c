/* strutils.c,v 1.2 1994/05/26 21:01:54 me Exp */

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
 * General string-oriented utility functions.
 *
 * Author: Jordan K. Hubbard
 * Date: April 23rd, 1990.
 * Description: Various "general purpose" string functions to support
 *		xterm+
 *
 * Revision History:
 *
 * strutils.c,v
 * Revision 1.2  1994/05/26  21:01:54  me
 * New copyright
 *
 * Revision 1.1.1.1  1994/05/22  11:22:41  me
 * Initial import into CVS
 *
 * Revision 1.5  92/05/16  06:28:19  jkh
 * Synchronization checkin.
 * 
 * Revision 1.4  92/02/26  11:36:38  me
 * Steve Crooks clix port and general code cleanup
 * 
 * Revision 1.3  90/10/19  00:21:55  jkh
 * Added strcomp().
 * 
 */

#include "common.h"

/* Return the basename of a filename */
Export String
basename(String name)
{
     String cp;

     return((cp = rindex(name, '/')) ? cp + 1 : name);
}

/* malloc and conditionally zero a region */
Export Generic
malloc_and_clear(int nbytes)
{
     Generic ret;

     if ((ret = (Generic)XtMalloc(nbytes)) != NULL)
	  bzero(ret, nbytes);
     return ret;
}

/* safely compare two strings; works even if one or both are NULL */
Export int
strcomp(String s1, String s2)
{
     if (s1 && s2)
	  return(strcmp(s1, s2));
     else if (!s1 && !s2)
	  return 0;
     else if (!s1 && s2)
	  return -1;
     else
	  return 1;
}

/*
 * Convert backslashed constants in a string in-place (string will never
 * get longer).
 */

/* This will lose on non-ascii architectures. I don't really care. */
#define DIGIT(x) \
     (isdigit(x) ? (x) - '0' : islower(x) ? (x) + 10 - 'a' : (x) + 10 - 'A')

Export String
backslash_convert(String str)
{
     String keep, str2;
     int val = 0;

     /*
      * Convert backslashed character forms into their native formats
      * (I.E.: "\n" and "\t" into '\n' and '\t' characters, etc).
      */
     keep = str2 = str;

     while (*str) {
	  if (*str != '\\' && *str != '^')
	       *(str2++) = *(str++);
	  else if (*str == '^') {
	       if (*(++str) == '^')
		    *(str2++) = '^';
	       else
		    *(str2++) = (islower(*str) ? *str - 96 : *str - 64);
	       ++str;
	  }
	  else switch (*(++str)) {
	  case '\\':
	       *(str2++) = '\\';
	       ++str;
	       break;

	  case 'e':
	  case 'E':
	       *(str2++) = '\033';
	       ++str;
	       break;
	       
	  case 'n':
	       *(str2++) = '\n';
	       ++str;
	       break;
	       
	  case 't':
	       *(str2++) = '\t';
	       ++str;
	       break;
	       
	  case 's':
	       *(str2++) = ' ';
	       ++str;
	       break;
	       
	  case 'r':
	       *(str2++) = '\r';
	       ++str;
	       break;
	       
	  case 'f':
	       *(str2++) = '\f';
	       ++str;
	       break;
	       
	  case 'b':
	       *(str2++) = '\b';
	       ++str;
	       break;
	       
	  case 'v':
	       *(str2++) = '\13';
	       ++str;
	       break;
	       
	  case 'z':
	       ++str;
	       break;
	       
	  case '0': case '1': case '2': case '3': case '4':
	  case '5': case '6': case '7': case '8': case '9':
	       /* Three digit octal constant */
	       if (*str >= '0' && *str <= '3'
		   && *(str + 1) >= '0' && *(str + 1) <= '7'
		   && *(str + 2) >= '0' && *(str + 2) <= '7') {
		    
		    val = (DIGIT(*str) << 6) + (DIGIT(*(str + 1)) << 3)
			 + (DIGIT(*(str + 2)));
		    
		    /* if val is zero, you may lose big */
		    *(str2++) = val;
		    str += 3;
		    break;
	       }
	       
	       /*
		* One or two digit hex constant. If two are there
		* they will both be taken. Use \z to split them up if
		* this behaviour is not desired.
		*/
	       if (*str == '0' && (*(str + 1) == 'x' || *(str + 1) == 'X')
		   && isxdigit(*(str + 2))) {
		    val = DIGIT(*(str + 2));
		    if (isxdigit(*(str + 3))) {
			 val = (val << 4) + DIGIT(*(str + 3));
			 str += 4;
		    }
		    else
			 str += 3;
		    *(str2++) = val;
		    break;
	       }
	       
	       /* Two or three decimal digits */
	       if (isdigit(*(str + 1))) {
		    val = DIGIT(*str) * 10 + DIGIT(*(str + 1));
		    if (isdigit(*(str + 2))) {
			 val = 10 * val + DIGIT(*(str + 2));
			 str += 3;
		    }
		    else
			 str += 2;
		    *(str2++) = val;
		    break;
	       }
	       
	       /* Otherwise it's a single decimal digit */
	       *(str2++) = val;
	       str++;
	       break;
	       
	  default:
	       /* default is to leave the sequence alone */
	       *(str2++) = '\\';
	       *(str2++) = *(str++);
	       break;
	  }
     }
     *str2 = '\0';
     return(keep);
}
