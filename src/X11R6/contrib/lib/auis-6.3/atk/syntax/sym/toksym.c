/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
\* ********************************************************************** */
#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/sym/RCS/toksym.c,v 1.11 1993/05/04 01:34:45 susan Exp $";
#endif

/* toksym.c		

	Code for the toksym object
*/
/*
 *    $Log: toksym.c,v $
 * Revision 1.11  1993/05/04  01:34:45  susan
 * RCS Tree Split
 *
 * Revision 1.10.1.1  1993/02/02  04:13:33  rr2b
 * new R6tape branch
 *
 * Revision 1.10  1992/12/14  20:57:48  rr2b
 * disclaimerization
 *
 * Revision 1.9  1992/12/02  20:00:53  rr2b
 * fixed to not use xgetchar.h, and to include text.ih so it can use getunsignedchar
 * on the text
 * .
 *
 * Revision 1.8  1992/11/26  02:03:49  wjh
 * updated header
 * .
 *
. . .
 * Revision 1.0  88/07/14  13:22:05 WJHansen
 * Copied from sym.c and discarded EVERYTHING
 */


#include <class.h>
#include <toksym.eh>
#include <text.ih>


	boolean
toksym__InitializeObject(ClassID, self)
	struct classhdr *ClassID;
	struct toksym *self;
{
	self->loc = 0;	/* (because error terminator set uses -3 as flag) */
	self->len = 0;
	self->info.obj = NULL;
	return TRUE;
}


	void
toksym__FinalizeObject(ClassID, self)
	struct classhdr *ClassID;
	struct toksym *self;
{
}

/* toksym__ToC(t, buf, maxlen)
	copies the token from the text t to the buffer buf, up to maxlen bytes
	returns buf 
*/
	char *
toksym__ToC(self, t, buf, maxlen)
	struct toksym *self;
	register struct text *t;
	register char *buf; 
	long maxlen;
{
	register char *cx;
	register long loc, len;
	cx = buf;
	loc = self->loc;
	len = self->len;
	if (len > maxlen-1) 
		len = maxlen - 1;
	for ( ;  len > 0;  len--, loc++)
		*cx++ = text_GetUnsignedChar(t, loc);
	*cx = '\0';
	return buf;
}
