/*
 * This file is provided for unrestricted use
 * provided that this legend is included on all tape media
 * and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 *
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#include <stdio.h>
#include <xview/xview.h>
#include <xview/textsw.h>
#include "xvnews.h"

int textsw_find_exp(textsw, exp, first, last)
Textsw  textsw;
char    *exp;
Textsw_index     *first, *last;
{
        Textsw_index    textsize;
        char            *contents, *ptr, *c, *str;

        xv_set((Textsw)textsw, TEXTSW_INSERTION_POINT, *last, NULL);
        textsize = xv_get((Textsw)textsw, TEXTSW_INSERTION_POINT);
        ptr = (char *)malloc((textsize + 1) - *first);

        if (ptr == NULL)
                return -1;

	contents = ptr;

        memset(ptr, '\0', textsize + 1 - *first);
        xv_get((Textsw)textsw,
	       TEXTSW_CONTENTS, first, ptr, textsize - *first, NULL);

	if (xvnews_comp(exp)) {
                free(ptr);
                return -1;
        }

        c = strchr(contents, '\n');

        while (c != NULL) {
                *c = '\0';
		if (xvnews_exec(contents)) {
                        *last = *first + strlen(contents);
                        c = contents;
			while(xvnews_exec(c)) {
                                ++c;
                                *first += 1;
                        }
                        *first -= 1;
                        c--;
                        str = c + strlen(c);
			while (xvnews_exec(c)) {
                                str--;
                                *str = '\0';
                                *last -= 1;
                        }
                        *last += 1;
                        c = NULL;
                } else {
                        *first += strlen(contents) + 1;
                        contents = ++c;
                        c = strchr(contents, '\n');
                }
	}
	free(ptr);
		
        return 1;
}

extern int textsw_file_line_count(txt, buf)
Textsw	txt;
char *buf;
{
	int		i = 0, mem = 0;
	char            *textbuffer = NULL, *c;
	Textsw_index	textsize, next_pos;

	if (buf == NULL) {
        	xv_set((Textsw)txt, TEXTSW_INSERTION_POINT, TEXTSW_INFINITY, 0);
        	textsize= (Textsw_index)xv_get((Textsw)txt, TEXTSW_INSERTION_POINT);
        	textbuffer = (char *) malloc(textsize + 1);
        	if (textbuffer == NULL) {
                	printf("Malloc failed!\n");
                	return(-1);
        	}
        	memset(textbuffer, '\0', textsize);
        	next_pos = (Textsw_index) xv_get((Textsw)txt, TEXTSW_CONTENTS, 0,
            	textbuffer, textsize);
        	if (next_pos < 1) {
                	free(textbuffer);
                	return(-1);
        	}
		mem = 1;
		c = textbuffer;
	} else
		c = buf;	

	while(*c++ != '\0') {
		if (*c == '\n')
			++i;
	}	
	if (mem)
		free(textbuffer);
	
	return i;
}
