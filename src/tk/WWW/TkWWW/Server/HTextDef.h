/*	Specialities of GridText as subclass of HText
**      
**      This file has been modified for use with tkWWW
*/
#ifndef HTEXTDEF_H
#define HTEXTDEF_H

#include "HText.h"		/* Superclass */
#include "HTChunk.h"
#include "HTStream.h"
#include "tcl.h"

EXTERN Tcl_Interp *HtTclInterp;

/*	Notes on struct _Htext:
**	next_line is valid iff state is false.
**	top_of_screen line means the line at the top of the screen
**			or just under the title if there is one.
*/
struct _HText {
	HTParentAnchor *	node_anchor;
	HTChunk *               output;
        int                     execute_pointer;
	HTStream*		target;			/* Output stream */
	HTStreamClass		targetClass;		/* Output routines */
	int                     error_code;
};

#ifdef SHORT_NAMES
#define HText_executeTCL HTTxEt
#define HText_puts HTTxPs
#define HText_putc HTTxPc
#endif

extern void HText_executeTCL PARAMS((HText *));
extern void HText_puts PARAMS((HText *, CONST char *));
extern void HText_putc PARAMS((HText *, CONST char));
extern int HtTclErrorCode;

#endif
