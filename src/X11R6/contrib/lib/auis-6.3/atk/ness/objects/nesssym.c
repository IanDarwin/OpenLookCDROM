/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/nesssym.c,v 1.14 1994/02/25 00:26:17 wjh Exp $";
#endif





/*
 *    $Log: nesssym.c,v $
 * Revision 1.14  1994/02/25  00:26:17  wjh
 * fixed infinite loop when recompile
 * 	(forgot to detach mark from text)
 *
 * Revision 1.13  1994/02/01  22:08:25  wjh
 * fixed type error in call.c
 * fixed to unsderstand "wraplook"
 * first fields of eventnode now match funcnode
 *
 * Revision 1.12  1993/05/04  01:23:55  susan
 * RCS Tree Split
 *
 * Revision 1.10.1.1  1993/02/02  03:03:35  rr2b
 * new R6tape branch
 *
 * Revision 1.10  1992/12/15  21:38:20  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.9  1992/12/14  20:49:20  rr2b
 * disclaimerization
 *
 * Revision 1.8  1992/11/26  02:38:01  wjh
 * converted CorrectGetChar to GetUnsignedChar
 * moved ExtendShortSign to interp.h
 * remove xgetchar.h; use simpletext_GetUnsignedChar
 * nessrun timing messages go to stderr
 * replaced curNess with curComp
 * replaced genPush/genPop with struct compilation
 * created compile.c to handle compilation
 * moved scope routines to compile.c
 * converted from lex to tlex
 * convert to use lexan_ParseNumber
 * truncated logs to 1992 only
 * use bison and gentlex instead of yacc and lexdef/lex
 *
 * .
 *
 * Revision 1.7  92/06/05  16:39:31  rr2b
 * added code to ensure proper deallocation of ness symbols
 * 
 * Revision 1.6  1991/09/12  16:26:37  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.5  1989/06/01  16:01:17  wjh
 * campus release version
 *
 * Revision 1.1  88/10/21  11:01:09  wjh
 * Initial revision
 *  
 * Revision 1.0  88/07/14  13:22:05WJHansen
 * Copied from sym.c and discarded EVERYTHING
 */

/*****************************************************************************\
\*****************************************************************************/



#include <class.h>
#include <observe.ih>	/* for DeleteRecipient */
#include <text.ih>		/* for RemoveMark */

#include <nesssym.eh>
#include <nodeclss.h>
#include <compdefs.h>	/* for Tunk */
#include <nevent.h>

	boolean
nesssym__InitializeObject(ClassID, self)
	struct classhdr *ClassID;
	struct nesssym *self;
{
	self->next = NULL;
	self->parent.ness = NULL;
	self->flags = 0;
	self->type = Tunk;
	return TRUE;
}

	void
nesssym__FinalizeObject(ClassID, self)
	struct classhdr *ClassID;
	struct nesssym *self;
{
	struct mark *m;
	switch(self->type) {
	    case Tunk:
		break;
	    case Tfunc:
		if(self->flags&flag_function) {
			struct funcnode *fnode = nesssym_NGetINode(self, funcnode);
			m = fnode->where;
			funcnode_Destroy(fnode);
		}
		else if(self->flags&flag_event) {
			struct eventnode *n = nesssym_NGetINode(self, eventnode);
			m = n->where;
			if (n->TriggerHolder != NULL)
				observable_DeleteRecipient(n->TriggerHolder,
					atom_Intern(n->spec), n->parentness);
			if(n->spec!=NULL) {
				free(n->spec);
				n->spec=NULL;
			}
			if(n->rock!=NULL) {
				free(n->rock);
				n->rock=NULL;
			}
			if(n->meptr) *(n->meptr)=n->next;
			if(n->next) n->next->meptr=n->meptr;
			eventnode_Destroy( self->header.toksym.info.node);
		}
		else break;

		/* delete m in the two cases covered above */
		if (m) {
			struct text *tx = (struct text *)mark_GetObject(m);
			if (tx) text_RemoveMark(tx, m);
			mark_Destroy(m);
		}

		break;
	    case Tptr:
		if(self->flags&flag_xobj) {
			objnode_Destroy(self->header.toksym.info.node);
		}
	    default:
		break;
	}
	self->header.toksym.info.node=NULL;	
}
