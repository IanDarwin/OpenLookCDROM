/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/


#ifndef _parse_h_
#define _parse_h_




/*
 * $Log: parse.h,v $
 * Revision 1.5  1993/05/04  01:23:33  susan
 * RCS Tree Split
 *
 * Revision 1.4.1.1  1993/02/02  02:57:32  rr2b
 * new R6tape branch
 *
 * Revision 1.4  1992/12/14  20:48:33  rr2b
 * disclaimerization
 *
 * Revision 1.3  1992/11/26  02:33:17  wjh
 * converted to bison/parse and tlex
 * chtest: added -t switch to dump parse transitions
 *
 * .
 *
 * Revision 1.2  91/09/12  19:45:13  bobg
 * Update copyright notice
 * 
 * Revision 1.1  1989/08/22  15:29:44  wjh
 * Initial revision
 *
 */


union yystype {
	struct toksym *tok;
	struct type *type;
	char *str;
	long num;
	operation op;
	};

#define YYSTYPE union yystype

YYSTYPE yylval;

#endif   /*  _parse_h_  */
