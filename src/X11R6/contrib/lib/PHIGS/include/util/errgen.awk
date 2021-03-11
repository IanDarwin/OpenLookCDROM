##
# $XConsortium: errgen.awk,v 5.4 94/04/17 20:42:00 rws Exp $
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


# Generate internal error numbers from the phigs error file.
#usage:
# awk -f errgen.awk phigserr.h > errnum.h

BEGIN {FS = " ";\
 } 
(NR == 1) {	# First line of file
    printf "\n/*\n * Copyright (c) 1988-1991 by Sun Microsystems, Inc.\n"
    printf " * Automatically generated file, do not edit.\n */\n\n"
}
$1 == "#define" {
    if (substr($3,1,1) == "-")
	printf "#define\tERRN%d\t%4s\n", -$3, $3
    else 
	printf "#define\tERR%s\t%4s\n", $3, $3
}
