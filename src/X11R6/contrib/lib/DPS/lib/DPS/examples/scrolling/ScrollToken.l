%{
/*
 * $RCSfile: ScrollToken.l,v $
 *
 * (c) Copyright 1992-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

#ifndef _SCROLL_TOKEN_H
#define _SCROLL_TOKEN_H

/* operands */
#define dis_int			1
#define dis_real		2
#define dis_string		3
#define dis_literal		4
#define dis_name		5
#define dis_array		6

/* path rendering operators */
#define dis_f			7
#define dis_s			8			
#define dis_clip		9

/* text showing operators */
#define dis_T			10
#define dis_A			11
#define dis_W			12
#define dis_AW			13

/* path construction operators */
#define dis_m			14
#define dis_lineto		15
#define dis_L			16
#define dis_r			17
#define dis_R			18
#define dis_l			19
#define dis_x			20
#define dis_y			21
#define dis_X			22
#define dis_Y			23
#define dis_c			24
#define dis_cp			25

/* gstate parameter setting operators */
#define dis_w			26
#define dis_g			27
#define dis_j			28
#define dis_d			29
#define dis_miter		30
#define dis_cap			31
#define dis_RGB			32

/* font operators */
#define dis_F			33
#define dis_FF			34
#define dis_DF			35
#define dis_MF			36

/* image operators */
#define dis_IMASK		37
#define dis_IMAGE		38

#define dis_BPAGE		39
#define dis_EPAGE		40
#define dis_REMAP		41
#define dis_RECODE		42
#define dis_initclip		43

#endif /* _SCROLL_TOKEN_H -- Add nothing below this line! */

#include "ScrollToken.h"

extern char	igetc();
#undef input
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):igetc())==10?(yylineno++,EOF):yytchar)==EOF?0:yytchar)
%}
%o 5000
PSALPHA		[^0-9 \t\n\r\(\)<>\[\]\{\}\/\%]
PSNUM		[0-9]
PSALPHANUM	({PSALPHA}|{PSNUM})
PSNAME		{PSALPHANUM}*{PSALPHA}+{PSALPHANUM}*			
%%
"%".*\n		;
\(.*\)		return  dis_string; 
-?{PSNUM}+	return  dis_int; 
(-?{PSNUM}+\.?{PSNUM}*)|(-?{PSNUM}*\.{PSNUM}+)	return  dis_real; 
m		return dis_m; 
moveto		return dis_m; 
l		return dis_l; 
lineto		return dis_lineto; 
x		return dis_x; 
y		return dis_y; 
X		return dis_X; 
Y		return dis_Y; 
c		return dis_c; 
cp		return dis_cp; 
f 		return dis_f; 
s		return dis_s; 
clip		return dis_clip; 
T		return dis_T; 
A		return dis_A; 
W		return dis_W; 
AW		return dis_AW; 
R 		return dis_R; 
r		return dis_r; 
L 		return dis_L; 
w		return dis_w; 
g		return dis_g; 
j 		return dis_j; 
d		return dis_d; 
setmiterlimit	return dis_miter;
setlinecap	return dis_cap;
RGB		return dis_RGB; 
MF		return dis_MF; 
FF		return dis_FF; 
DF		return dis_DF; 
F		return dis_F; 
IMAGEMASK	return dis_IMASK; 
IMAGE 		return dis_IMAGE; 
BEGINPAGE	return dis_BPAGE;
ENDPAGE		return dis_EPAGE;
REMAP		return dis_REMAP;
RECODE		return dis_RECODE;
initclip	return dis_initclip;
\/{PSNAME}	return dis_literal;  
{PSNAME}	return dis_name; 
.|\n		;

%%
yywrap() { return (1);}
