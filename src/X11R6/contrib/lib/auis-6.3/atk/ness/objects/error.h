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


/* error.h  --  declarations for error handling routines
*/
#ifndef _ness_error_h_
#define _ness_error_h_



#define freeze(msg) ((char *)strcpy((char *)malloc(strlen(msg)+1), msg))


struct errornode {
	struct mark *where;
	long execloc;	/* object code location of error */
	unsigned char *msg;	/* the error message */
	boolean ownmsg;	/* TRUE if this node is the only ptr to the msg */
	struct errornode *next;
};

struct errornode *errornode_New();
struct errornode *errornode_Create(/* long loc, long len, long execloc, unsigned char *msg,
	boolean ownmsg, struct errornode *next */);
void errornode_Destory(/* struct errornode *enode */);

void codelocStore(/* struct nesssym *fness */);		/* record code location */
void codelocForget(/* struct nesssym *fness */);	/* forget it */
struct nesssym *codelocFind(/* long loc */);		/* find nesssym for loc */

void   SaveError(/* unsigned char *msg, long loc, long len */);	/* add to error list */
void   ReportError(/* unsigned char *msg,  long index */); /* SaveError at the loc of 'index'th token */
void   ExprError(/* unsigned char *msg, struct exprnode *expr */); /* SaveError at the loc of 'expr' */
void   SetupErrorHandling(/*  */);		/* init compilation recovery */
void   errorfromparse(/* struct parse *parser, int severity, unsigned char *msg */);	/* capture parser error */
long   errorsynch(/* int index */);		/* "... restart with token" */
boolean   isFuncStart(/* struct nesssym *tok */);	/* ? 0 indent for this or next two tokens */

struct errornode *LocateErrorFunc(/* loc, base, msg, ness */);  /* see which ness has the error */
void   MapRunErrors(/* struct ness *ness */);

#endif /* _ness_error_h_ */

