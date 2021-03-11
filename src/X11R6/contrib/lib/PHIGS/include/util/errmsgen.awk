##
# $XConsortium: errmsgen.awk,v 5.4 94/04/17 20:42:00 rws Exp $
##
## 
## Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. and the X Consortium.
## 
##                         All Rights Reserved
## 
## Permission to use, copy, modify, and distribute this software and its 
## documentation for any purpose and without fee is hereby granted, 
## provided that the above copyright notice appear in all copies and that
## both that copyright notice and this permission notice appear in 
## supporting documentation, and that the names of Sun Microsystems
## and the X Consortium not be used in advertising or publicity 
## pertaining to distribution of the software without specific, written 
## prior permission.  
## 
## SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
## INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
## EVENT SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
## CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
## USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
## OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
## PERFORMANCE OF THIS SOFTWARE.


# Generate error message file from the phigs error file.
# Lines starting with non-numerics are ignored.
#usage:
# awk -f errmsggen.awk phigserr.h > PHIGSerr.txt

BEGIN {
    FS = " ";\
    endcomment = 1;
}

(NR == 1) {	# First line of file
    printf "#\n# Copyright (c) 1988-1991 by Sun Microsystems, Inc.\n"
    printf "# Automatically generated file, do not edit.\n\n\n"
}

"#define" == $1	{
    errnum = $3;
    errtext = substr ($0, index ($0, "/*")+2);
    endcomment = index (errtext, "*/");
    if (endcomment) {
	# entire  message fits on one line
	errtext = substr (errtext, 1, endcomment-1);

	# strip trailing spaces (at EOL) before printing
	while (substr (errtext, length (errtext), 1) == " ")
	    errtext = substr(errtext, 1, length (errtext) - 1)

	format ="%d:%s\n"
    } else {
	# handle hyphens, else add a space between words
	if (substr (errtext, length (errtext), 1) == "-")
	    errtext = substr (errtext, 1, length(errtext)-1);
	else if (substr (errtext, length (errtext), 1) != " ")
	    errtext = errtext " "

	format ="%d:%s"
    }
    # print start of line
    printf (format, errnum, errtext);
}

"#define" != $1	{
    # if it's outside of the comment after the error define,
    # ignore it.
    if (endcomment == 0) {
	errtext = substr ($0, index ($0, $1)); # strip blanks
	endcomment = index (errtext, "*/");
	if (endcomment) {
	    errtext = substr (errtext, 1, endcomment-1);

	    # strip trailing spaces (at EOL) before printing
	    while (substr (errtext, length (errtext), 1) == " ")
		errtext = substr(errtext, 1, length (errtext) - 1)

	    format ="%s\n"
	} else {
	    # handle hyphens, else add a space between words
	    if (substr (errtext, length (errtext), 1) == "-")
		errtext = substr (errtext, 1, length(errtext)-1);
	    else if (substr (errtext, length (errtext), 1) != " ")
		errtext = errtext " "

	    format ="%s"
	}
	# print continuation of line
	printf (format, errtext);
    }
}
