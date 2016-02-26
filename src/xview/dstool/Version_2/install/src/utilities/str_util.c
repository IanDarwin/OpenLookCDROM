/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#include <stdio.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif

#include <ctype.h>
#include <constants.h>

/* 
  copy characters from  the leading token of source_str to
  dest_str. Assumes dest_str can hold up to maxlen characters.
  Returns a pointer to the first char of dest_str.
*/
char
    *leading_token(dest_str,source_str,maxlen)
char 
    *dest_str,*source_str;

int
    maxlen;

{
    char
	*first_char = dest_str;
    while (isspace(*source_str))
	source_str++; /* skip blanks */
    
    while ((isalnum(*source_str) || (*source_str == '_')) && maxlen > 0) {
	*dest_str++ = *source_str++;
	maxlen--;
    }
    
    *dest_str = '\0';

    return first_char;
}
