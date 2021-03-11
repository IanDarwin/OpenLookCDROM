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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/rofftext/RCS/roffchrs.c,v 2.8 1992/12/15 21:40:52 rr2b R6tape $";
#endif


 

/* special chars for rofftext.c
 *
 */

#include <class.h>
#include <rofftext.ih>
#include <hash.ih>

#define Mangle(self,name,def) hash_Store(self->SpecialChars,name,StrDup(def))

InitChars(self)
struct rofftext *self;
{
    Mangle(self,"em","-");
    Mangle(self,"hy","-");
    Mangle(self,"bu","o");
    Mangle(self,"14","1/4");
    Mangle(self,"12","1/2");
    Mangle(self,"34","3/4");
    Mangle(self,"fi","fi");
    Mangle(self,"fl","fl");
    Mangle(self,"ff","ff");
    Mangle(self,"Fi","ffi");
    Mangle(self,"Fl","ffl");
    Mangle(self,"**","*");
    Mangle(self,"pl","+");
    Mangle(self,"mi","-");
    Mangle(self,"eq","=");
    Mangle(self,"aa","'");
    Mangle(self,"ga","`");
    Mangle(self,"ul","_");
    Mangle(self,"sl","/");
    Mangle(self,"br","|");
    Mangle(self,"or","|");
    Mangle(self,"ci","O");
    Mangle(self,"fm","'");

    /* greek letters */
    Mangle(self,"*a","A");
    Mangle(self,"*b","B");
    Mangle(self,"*g","/");
    Mangle(self,"*d","D");
    Mangle(self,"*e","S");
    Mangle(self,"*z","Q");
    Mangle(self,"*y","N");
    Mangle(self,"*h","O");
    Mangle(self,"*i","i");

    /* math */
    Mangle(self,">=",">=");
    Mangle(self,"<=","<=");
    Mangle(self,"!=","!=");
    Mangle(self,"==","==");
    Mangle(self,"ap","~");
    Mangle(self,"->","->");
    Mangle(self,"<-","<-");
    Mangle(self,"ua","^");
    Mangle(self,"da","v");
    Mangle(self,"mu","x");
    Mangle(self,"di","/");
    Mangle(self,"+-","+-");
    Mangle(self,"no","!");

    /* more to come */
}
