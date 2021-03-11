/* -*-C-*-
********************************************************************************
*
* File:         t_trans.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/t_trans.c,v 2.1 1994/06/06 15:41:12 npm Exp $
* Description:  Interfaces to Xtango procs dealing with type TANGO_TRANS
* Author:       Niels P. Mayer
* Created:      Sat May 15 22:22:03 1993
* Modified:     Sun Jun  5 14:22:10 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/t_trans.c,v 2.1 1994/06/06 15:41:12 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <Xm/Xm.h>
#include "winterp.h"
#include "tango.h"


/******************************************************************************
 * This is called from xlisp/xldmem.c:cv_tangotrans() -- it puts the TANGO_TRANS
 * object onto WINTERP_TANGO_CONTEXT's list of TANGO_TRANS nodes that can be
 * referenced by XLISP.
 ******************************************************************************/
void Xtango_Context_Add_TANGO_TRANS(lval)
     LVAL lval;			/* XLTYPE_TANGO_TRANS */
{
  WINTERP_TANGO_CONTEXT context;
  WINTERP_TANGO_CONTEXT_REF ref;

  context = get_tangotrans_context(lval);

  ref = (WINTERP_TANGO_CONTEXT_REF) XtMalloc(sizeof(TANGO_CONTEXT_Ref_Node));

  ref->lval = lval;

  if (context->trans_reflist)
    ref->next = context->trans_reflist;
  else
    ref->next = (WINTERP_TANGO_CONTEXT_REF) NULL; /* elt is the only one on the alist */

  context->trans_reflist = ref;	/* make elt the new head of the list */
}


/******************************************************************************
 * Call this from garbage collector xlisp/xldmem.c:sweep() to remove the
 * XLTYPE_TANGO_TRANS arg from the set of TANGO_TRANS's that are still
 * referenced by XLISP. This proc also deallocates the C TANGO_TRANS structure
 * via TANGOtrans_free().
 ******************************************************************************/
void Xtango_Context_Remove_TANGO_TRANS(lval)
     LVAL lval;			/* XLTYPE_TANGO_TRANS */
{
  TANGO_TRANS trans;
  WINTERP_TANGO_CONTEXT context;
  WINTERP_TANGO_CONTEXT_REF prev, cur;

  if ((context = get_tangotrans_context(lval)) != (WINTERP_TANGO_CONTEXT) NULL) {
    /* remove the lval from the context */
    if (!(context->trans_reflist)) /* ASSERT(context->trans_reflist!=NULL): calling Xtango_Context_Remove_TANGO_TRANS() while context->trans_reflist==NULL indicates a programming error -- this call is meant to bracket Xtango_Context_Add_TANGO_TRANS(). */
      xlfatal("Internal error 0 in Xtango_Context_Remove_TANGO_TRANS().");
    else {
      prev = (WINTERP_TANGO_CONTEXT_REF) NULL;
      cur  = context->trans_reflist;
      while (cur && (cur->lval != lval)) {
	prev = cur;
	cur = cur->next;
      }
      if (!prev) {		/* context->trans_reflist == cur == lval */
	context->trans_reflist = context->trans_reflist->next; /* pop the first elt */
	XtFree((char*) cur);
      }
      else if (cur) {		/* cur == lval */
	prev->next = cur->next;	/* remove cur from alist */
	XtFree((char*) cur);
      }
      else			/* ASSERT(cur!=NULL): something has gone awry if Xtango_Context_Remove_TANGO_TRANS() got called but can't find 'lval' in context->trans_reflist -- this call is meant to bracket Xtango_Context_Add_TANGO_TRANS(). */
	xlfatal("Internal error 1 in Xtango_Context_Remove_TANGO_TRANS().");

      /* context will go away soon, NULL the field to prevent ref to dealloc'd memory */
      set_tangotrans_context(lval, (WINTERP_TANGO_CONTEXT) NULL);
    }
  }

  if ((trans = get_tangotrans(lval)) != (TANGO_TRANS) NULL) {
    /* Note assumption that transition_free() doesn't need
     * Xtango_Save_Set_Context...() and Xtango_Restore_Context()
     * since no tango globals are affected by transition_free() */
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
    set_tangotrans(lval, (TANGO_TRANS) NULL);
  }
}


/******************************************************************************
 * This is called from t_utils.c:Xtango_Remove_WINTERP_TANGO_CONTEXT() when
 * a tango-widget gets destroyed via wc_Xtango.c:Xtango_Widget_Destroy_Callback()
 *
 * context->trans_reflist is list of type LVAL(XLTYPE_TANGO_TRANS) which refer to this
 * WINTERP_TANGO_CONTEXT ptr. When proc Xtango_Remove_WINTERP_TANGO_CONTEXT()
 * destroys the WINTERP_TANGO_CONTEXT, all LVAL(XLTYPE_TANGO_TRANS)s that reference
 * this structure must be set to NULL to prevent subesquent reference to freed
 * memory by values that are still referenceable by XLISP -- an xlerror() will
 * get signalled instead, informing user of the attempted use of an
 * LVAL(XLTYPE_TANGO_TRANS) corresponding to a destroyed tango-widget. 
 *
 * LVALs are added to context->trans_reflist whenever a TANGO_TRANS object
 * is created via xlisp/xldmem.c:cv_tangotrans() -- the proc which adds the elt
 * is Xtango_Context_Add_TANGO_TRANS().
 *
 * Note that upon execution of the garbage collector xlisp/xldmem.c:sweep(),
 * Xtango_Context_Remove_TANGO_TRANS() is called to remove the
 * appropriate LVAL from context->trans_reflist, therefore, the code below
 * only frees&NULLifies TANGO_TRANS nodes which still have a reference.
 ******************************************************************************/
void Xtango_Free_All_TANGO_TRANS_Assocd_With_Context(context)
     WINTERP_TANGO_CONTEXT context;
{
  WINTERP_TANGO_CONTEXT_REF ref, next_ref;
  TANGO_TRANS trans;

  ref = context->trans_reflist;
  while (ref) {
    if (!tangotrans_p(ref->lval))
      xlfatal("Internal error 0 in Xtango_Free_All_TANGO_TRANS_Assocd_With_Context().");
    if (get_tangotrans_context(ref->lval) != context)
      xlfatal("Internal error 1 in Xtango_Free_All_TANGO_TRANS_Assocd_With_Context().");

    if ((trans = get_tangotrans(ref->lval)) != (TANGO_TRANS) NULL) {
      /* Note assumption that transition_free() doesn't need
       * Xtango_Save_Set_Context...() and Xtango_Restore_Context()
       * since no tango globals are affected by transition_free() */
      transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
      set_tangotrans(ref->lval, (TANGO_TRANS) NULL); /* set tango_trans==NULL s.t. subsequent XLISP refs to this LVAL generate error */
    }
    set_tangotrans_context(ref->lval, (WINTERP_TANGO_CONTEXT) NULL); /* set context to NULL since pointer will get freed by Xtango_Remove_WINTERP_TANGO_CONTEXT() */
    next_ref = ref->next;
    XtFree((char*) ref);
    ref = next_ref;
  }
}


/******************************************************************************
 *
 ******************************************************************************/
static void Xtango_Error_Too_Many_TANGO_TRANS_Args(lval_trans)
     LVAL lval_trans;
{
  xlerror("Maximum number of TANGO_TRANS arguments exceeded.", lval_trans);
}


/******************************************************************************
 *
 ******************************************************************************/
static void Xtango_Error_Trans_Not_In_Same_Context(lval_trans)
     LVAL lval_trans;
{
  xlerror("Transitions for concatenation/composition must all be in the same Xtango widget.",
	  lval_trans);
}


/******************************************************************************
 *
 ******************************************************************************/
static void Xtango_Error_Freed_TANGO_TRANS(lval_trans)
     LVAL lval_trans;
{
  xlerror("Attempt to use deallocated transition caused by either:\n\t (1) :DESTROY'd instance of TANGO:WIDGET_CLASS,\n\t (2) previous call to TANGO:TX_FREE.",
	  lval_trans);
}


/******************************************************************************
 * Retrieves a <tango_trans> typed argument from the argument stack, signals
 * error if (1) too few args, (2) arg of wrong type, (3) <tango_trans> has been
 * freed.
 *
 * if get_tangotrans() slot has been set to NULL, then the tango-widget assoc'd
 * w/ the trans has been destroy'd by Xtango_Free_All_TANGO_TRANS_Assocd_With_Context()
 * which is called from t_utils.c:Xtango_Remove_WINTERP_TANGO_CONTEXT() when
 * a tango-widget gets destroyed via wc_Xtango.c:Xtango_Widget_Destroy_Callback(()
 *
 * Alternately, the get_tangotrans() slot may be NULL if a transition has been
 * explicitly freed via TANGO:TX_FREE.
 ******************************************************************************/
static TANGO_TRANS Xtango_Get_Trans_Arg_Returning_Validated_TANGO_TRANS(lval_trans_return)
     LVAL *lval_trans_return;	/* return ptr to XLTYPE_TANGO_TRANS */
{
  TANGO_TRANS trans;

  if ((trans = get_tangotrans(*lval_trans_return = xlga_tangotrans())) == (TANGO_TRANS) NULL)
    Xtango_Error_Freed_TANGO_TRANS(*lval_trans_return);
  else
    return (trans);
}


/******************************************************************************
 * (TANGO:TX_ITERATE [:PERFORM] <tango_trans> <num-iterations>)
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 *
 * This routine returns a transition which corresponds to performing the given
 * <tango_trans> <num> times, one immediately after the other.  One example
 * use of this routine is to make an image repeat a given behavior.
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The <tango_trans> argument is of type TANGO_TRANS, which is produced 
 * by one of the :TX_* methods on TANGO:IMAGE_CLASS and it's subclasses.
 *
 * The <num-iterations> argument is a FIXNUM representing the number of times
 * to iterate..
 *
 * ----------------------------------------------------------------------------
 * TANGO_TRANS
 * TANGOtrans_iterate (trans,num) 
 *    TANGO_TRANS trans;
 *    int num;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_TX_ITERATE()
{
  LVAL lval_trans;
  TANGO_TRANS trans;
  int num;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean perform_p;

  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  trans		= Xtango_Get_Trans_Arg_Returning_Validated_TANGO_TRANS(&lval_trans); /* get <tango_trans> */
  num		= Xtango_Get_Positive_FIXNUM_Arg();
  xllastarg();

  contxt = get_tangotrans_context(lval_trans);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();

  trans = TANGOtrans_iterate(trans, num);

  if (perform_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(trans, contxt));
}


#define TANGO_TRANS_ARG_SEQUENCE_SIZE 50 /* see transitions[50] decl in TANGOtrans_concatenate() and TANGOtrans_compose() */


/******************************************************************************
 * (TANGO:TX_CONCATENATE [:PERFORM] <trans-1> [<trans-2> [<trans-3> [...]]])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 * 
 * This routine returns a transition which corresponds to the concatenation of
 * the transitions in the <trans-i> parameters. The order of the incoming
 * transitions corresponds to the order that they will be performed in the
 * concatenation with transition <trans-1> being first.  This one logical
 * transition can then be composed, iterated, etc., with other transitions to
 * achieve various desired animations.
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * Each <trans-i> argument is of type TANGO_TRANS, which is produced 
 * by one of the :TX_* methods on TANGO:IMAGE_CLASS and it's subclasses.
 *
 * ----------------------------------------------------------------------------
 * TANGO_TRANS
 * TANGOtrans_concatenate (num,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) 
 *    int num;
 *    TANGO_TRANS t1,t2,t3,t4,t5,t6,t7,t8,t9,t10;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_TX_CONCATENATE()
{
  TANGO_TRANS contrans;
  TANGO_TRANS trans[TANGO_TRANS_ARG_SEQUENCE_SIZE];
  LVAL arg;
  int i;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean perform_p;

  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  trans[0]	= Xtango_Get_Trans_Arg_Returning_Validated_TANGO_TRANS(&arg); /* get <trans-0> */
  contxt	= get_tangotrans_context(arg);
  i		= 0;

  while (moreargs()) {		/* retrieve <trans-1> ... <trans-49> */
    arg = nextarg();		/* retrieve an arg from argument stack */
    i++;
    if (tangotrans_p(arg)) {	/* check to make sure it's a TANGO_TRANS */
      if (i >= TANGO_TRANS_ARG_SEQUENCE_SIZE)
	Xtango_Error_Too_Many_TANGO_TRANS_Args(arg);
      if ((trans[i] = get_tangotrans(arg)) == (TANGO_TRANS) NULL)
	Xtango_Error_Freed_TANGO_TRANS(arg);
      if (get_tangotrans_context(arg) != contxt)
	Xtango_Error_Trans_Not_In_Same_Context(arg);
    }
    else
      xlbadtype(arg);
  }
  xllastarg();

  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();

  switch (i) {			/* varargs suck! ... but gnu-emacs keyboard macros rule */
  case 0:			/* only gets exec'd if while() loop above never got called */
    contrans = TANGOtrans_concatenate(1,
       trans[0]);
    break;
  case 1:
    contrans = TANGOtrans_concatenate(2,
       trans[0], trans[1]);
    break;
  case 2:
    contrans = TANGOtrans_concatenate(3,
       trans[0], trans[1], trans[2]);
    break;
  case 3:
    contrans = TANGOtrans_concatenate(4,
       trans[0], trans[1], trans[2], trans[3]);
    break;
  case 4:
    contrans = TANGOtrans_concatenate(5,
       trans[0], trans[1], trans[2], trans[3], trans[4]);
    break;
  case 5:
    contrans = TANGOtrans_concatenate(6,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5]);
    break;
  case 6:
    contrans = TANGOtrans_concatenate(7,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6]);
    break;
  case 7:
    contrans = TANGOtrans_concatenate(8,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7]);
    break;
  case 8:
    contrans = TANGOtrans_concatenate(9,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8]);
    break;
  case 9:
    contrans = TANGOtrans_concatenate(10,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9]);
    break;
  case 10:
    contrans = TANGOtrans_concatenate(11,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10]);
    break;
  case 11:
    contrans = TANGOtrans_concatenate(12,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11]);
    break;
  case 12:
    contrans = TANGOtrans_concatenate(13,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12]);
    break;
  case 13:
    contrans = TANGOtrans_concatenate(14,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13]);
    break;
  case 14:
    contrans = TANGOtrans_concatenate(15,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14]);
    break;
  case 15:
    contrans = TANGOtrans_concatenate(16,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15]);
    break;
  case 16:
    contrans = TANGOtrans_concatenate(17,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16]);
    break;
  case 17:
    contrans = TANGOtrans_concatenate(18,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17]);
    break;
  case 18:
    contrans = TANGOtrans_concatenate(19,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18]);
    break;
  case 19:
    contrans = TANGOtrans_concatenate(20,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19]);
    break;
  case 20:
    contrans = TANGOtrans_concatenate(21,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20]);
    break;
  case 21:
    contrans = TANGOtrans_concatenate(22,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21]);
    break;
  case 22:
    contrans = TANGOtrans_concatenate(23,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22]);
    break;
  case 23:
    contrans = TANGOtrans_concatenate(24,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23]);
    break;
  case 24:
    contrans = TANGOtrans_concatenate(25,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24]);
    break;
  case 25:
    contrans = TANGOtrans_concatenate(26,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25]);
    break;
  case 26:
    contrans = TANGOtrans_concatenate(27,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26]);
    break;
  case 27:
    contrans = TANGOtrans_concatenate(28,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27]);
    break;
  case 28:
    contrans = TANGOtrans_concatenate(29,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28]);
    break;
  case 29:
    contrans = TANGOtrans_concatenate(30,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29]);
    break;
  case 30:
    contrans = TANGOtrans_concatenate(31,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30]);
    break;
  case 31:
    contrans = TANGOtrans_concatenate(32,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31]);
    break;
  case 32:
    contrans = TANGOtrans_concatenate(33,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32]);
    break;
  case 33:
    contrans = TANGOtrans_concatenate(34,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33]);
    break;
  case 34:
    contrans = TANGOtrans_concatenate(35,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34]);
    break;
  case 35:
    contrans = TANGOtrans_concatenate(36,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35]);
    break;
  case 36:
    contrans = TANGOtrans_concatenate(37,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36]);
    break;
  case 37:
    contrans = TANGOtrans_concatenate(38,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37]);
    break;
  case 38:
    contrans = TANGOtrans_concatenate(39,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38]);
    break;
  case 39:
    contrans = TANGOtrans_concatenate(40,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39]);
    break;
  case 40:
    contrans = TANGOtrans_concatenate(41,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40]);
    break;
  case 41:
    contrans = TANGOtrans_concatenate(42,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41]);
    break;
  case 42:
    contrans = TANGOtrans_concatenate(43,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42]);
    break;
  case 43:
    contrans = TANGOtrans_concatenate(44,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43]);
    break;
  case 44:
    contrans = TANGOtrans_concatenate(45,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44]);
    break;
  case 45:
    contrans = TANGOtrans_concatenate(46,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44],
       trans[45]);
    break;
  case 46:
    contrans = TANGOtrans_concatenate(47,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44],
       trans[45], trans[46]);
    break;
  case 47:
    contrans = TANGOtrans_concatenate(48,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44],
       trans[45], trans[46], trans[47]);
    break;
  case 48:
    contrans = TANGOtrans_concatenate(49,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44],
       trans[45], trans[46], trans[47], trans[48]);
    break;
  case 49:
    contrans = TANGOtrans_concatenate(50,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44],
       trans[45], trans[46], trans[47], trans[48], trans[49]);
    break;
  default:
    Xtango_Restore_Context();
    xlfail("");
    break;
  }

  if (perform_p) {
    TANGOtrans_perform(contrans);
    transition_free(contrans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(contrans, contxt));
}


/******************************************************************************
 * (TANGO:TX_COMPOSE [:PERFORM] <trans-1> [<trans-2> [<trans-3> [...]]])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 *
 * This routine is very important because it provides the ability to have
 * many images moving and changing around the screen at the same time. It
 * returns a transition which is the composition of the <trans-i>
 * arguments. By composition, we mean the "concurrent" execution of all
 * the transitions involved.  When transitions are composed, individual
 * frames of the transition are combined, e.g., the first frames, the
 * second frames, etc.  For transitions, each offset in the path utilized
 * corresponds to one frame.  Transitions of unequal "length" can be
 * composed.  Transitions shorter than the longest transition will simply
 * have null action frames added to their tails.  Consequently, all the
 * action will start together but will finish according to how many
 * frames are in the transition. Note: to compose more than 50
 * transitions, you need to call this routine twice with the result of
 * the first call as a parameter to the second call.
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * Each <trans-i> argument is of type TANGO_TRANS, which is produced 
 * by one of the :TX_* methods on TANGO:IMAGE_CLASS and it's subclasses.
 *
 * ----------------------------------------------------------------------------
 * TANGO_TRANS
 * TANGOtrans_compose (num,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) 
 *    int num;
 *    TANGO_TRANS t1,t2,t3,t4,t5,t6,t7,t8,t9,t10;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_TX_COMPOSE()
{
  TANGO_TRANS comtrans;
  TANGO_TRANS trans[TANGO_TRANS_ARG_SEQUENCE_SIZE];
  LVAL arg;
  int i;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean perform_p;

  perform_p	= Xtango_Get_Optional_PERFORM_Kwd_Arg(); /* get optional [:PERFORM] keyword */
  trans[0]	= Xtango_Get_Trans_Arg_Returning_Validated_TANGO_TRANS(&arg); /* get <trans-0> */
  contxt	= get_tangotrans_context(arg);
  i		= 0;

  while (moreargs()) {		/* retrieve <trans-1> ... <trans-49> */
    arg = nextarg();		/* retrieve an arg from argument stack */
    i++;
    if (tangotrans_p(arg)) {	/* check to make sure it's a TANGO_TRANS */
      if (i >= TANGO_TRANS_ARG_SEQUENCE_SIZE)
	Xtango_Error_Too_Many_TANGO_TRANS_Args(arg);
      if ((trans[i] = get_tangotrans(arg)) == (TANGO_TRANS) NULL)
	Xtango_Error_Freed_TANGO_TRANS(arg);
      if (get_tangotrans_context(arg) != contxt)
	Xtango_Error_Trans_Not_In_Same_Context(arg);
    }
    else
      xlbadtype(arg);
  }
  xllastarg();

  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();

  switch (i) {			/* varargs suck! ... but gnu-emacs keyboard macros rule */
  case 0:			/* only gets exec'd if while() loop above never got called */
    comtrans = TANGOtrans_compose(1,
       trans[0]);
    break;
  case 1:
    comtrans = TANGOtrans_compose(2,
       trans[0], trans[1]);
    break;
  case 2:
    comtrans = TANGOtrans_compose(3,
       trans[0], trans[1], trans[2]);
    break;
  case 3:
    comtrans = TANGOtrans_compose(4,
       trans[0], trans[1], trans[2], trans[3]);
    break;
  case 4:
    comtrans = TANGOtrans_compose(5,
       trans[0], trans[1], trans[2], trans[3], trans[4]);
    break;
  case 5:
    comtrans = TANGOtrans_compose(6,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5]);
    break;
  case 6:
    comtrans = TANGOtrans_compose(7,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6]);
    break;
  case 7:
    comtrans = TANGOtrans_compose(8,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7]);
    break;
  case 8:
    comtrans = TANGOtrans_compose(9,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8]);
    break;
  case 9:
    comtrans = TANGOtrans_compose(10,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9]);
    break;
  case 10:
    comtrans = TANGOtrans_compose(11,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10]);
    break;
  case 11:
    comtrans = TANGOtrans_compose(12,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11]);
    break;
  case 12:
    comtrans = TANGOtrans_compose(13,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12]);
    break;
  case 13:
    comtrans = TANGOtrans_compose(14,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13]);
    break;
  case 14:
    comtrans = TANGOtrans_compose(15,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14]);
    break;
  case 15:
    comtrans = TANGOtrans_compose(16,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15]);
    break;
  case 16:
    comtrans = TANGOtrans_compose(17,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16]);
    break;
  case 17:
    comtrans = TANGOtrans_compose(18,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17]);
    break;
  case 18:
    comtrans = TANGOtrans_compose(19,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18]);
    break;
  case 19:
    comtrans = TANGOtrans_compose(20,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19]);
    break;
  case 20:
    comtrans = TANGOtrans_compose(21,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20]);
    break;
  case 21:
    comtrans = TANGOtrans_compose(22,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21]);
    break;
  case 22:
    comtrans = TANGOtrans_compose(23,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22]);
    break;
  case 23:
    comtrans = TANGOtrans_compose(24,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23]);
    break;
  case 24:
    comtrans = TANGOtrans_compose(25,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24]);
    break;
  case 25:
    comtrans = TANGOtrans_compose(26,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25]);
    break;
  case 26:
    comtrans = TANGOtrans_compose(27,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26]);
    break;
  case 27:
    comtrans = TANGOtrans_compose(28,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27]);
    break;
  case 28:
    comtrans = TANGOtrans_compose(29,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28]);
    break;
  case 29:
    comtrans = TANGOtrans_compose(30,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29]);
    break;
  case 30:
    comtrans = TANGOtrans_compose(31,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30]);
    break;
  case 31:
    comtrans = TANGOtrans_compose(32,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31]);
    break;
  case 32:
    comtrans = TANGOtrans_compose(33,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32]);
    break;
  case 33:
    comtrans = TANGOtrans_compose(34,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33]);
    break;
  case 34:
    comtrans = TANGOtrans_compose(35,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34]);
    break;
  case 35:
    comtrans = TANGOtrans_compose(36,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35]);
    break;
  case 36:
    comtrans = TANGOtrans_compose(37,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36]);
    break;
  case 37:
    comtrans = TANGOtrans_compose(38,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37]);
    break;
  case 38:
    comtrans = TANGOtrans_compose(39,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38]);
    break;
  case 39:
    comtrans = TANGOtrans_compose(40,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39]);
    break;
  case 40:
    comtrans = TANGOtrans_compose(41,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40]);
    break;
  case 41:
    comtrans = TANGOtrans_compose(42,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41]);
    break;
  case 42:
    comtrans = TANGOtrans_compose(43,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42]);
    break;
  case 43:
    comtrans = TANGOtrans_compose(44,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43]);
    break;
  case 44:
    comtrans = TANGOtrans_compose(45,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44]);
    break;
  case 45:
    comtrans = TANGOtrans_compose(46,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44],
       trans[45]);
    break;
  case 46:
    comtrans = TANGOtrans_compose(47,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44],
       trans[45], trans[46]);
    break;
  case 47:
    comtrans = TANGOtrans_compose(48,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44],
       trans[45], trans[46], trans[47]);
    break;
  case 48:
    comtrans = TANGOtrans_compose(49,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44],
       trans[45], trans[46], trans[47], trans[48]);
    break;
  case 49:
    comtrans = TANGOtrans_compose(50,
       trans[0], trans[1], trans[2], trans[3], trans[4],
       trans[5], trans[6], trans[7], trans[8], trans[9],
       trans[10], trans[11], trans[12], trans[13], trans[14],
       trans[15], trans[16], trans[17], trans[18], trans[19],
       trans[20], trans[21], trans[22], trans[23], trans[24],
       trans[25], trans[26], trans[27], trans[28], trans[29],
       trans[30], trans[31], trans[32], trans[33], trans[34],
       trans[35], trans[36], trans[37], trans[38], trans[39],
       trans[40], trans[41], trans[42], trans[43], trans[44],
       trans[45], trans[46], trans[47], trans[48], trans[49]);
    break;
  default:
    Xtango_Restore_Context();
    xlfail("");
    break;
  }

  if (perform_p) {
    TANGOtrans_perform(comtrans);
    transition_free(comtrans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_p ? NIL : cv_tangotrans(comtrans, contxt));
}


/******************************************************************************
 * (TANGO:TX_PERFORM <tango_trans>)
 *	==> returns NIL
 *
 * TANGO:TX_PERFORM actually carries out the given transition which has
 * been created via one of the transition creation routines (:TX_* methods).
 * All the involved graphical images are animated in the ways specified in the
 * transition when it was created.
 *
 * The <tango_trans> argument is of type TANGO_TRANS, which is produced 
 * by one of the :TX_* methods on TANGO:IMAGE_CLASS and it's subclasses.
 * 
 * Note that simple transitions may be achieved via the :PERFORM keyword
 * on the TANGO:IMAGE_CLASS :TX_* methods -- the :PERFORM keyword allows
 * :TX_* method to directly invoke the functionality of TANGO:TX_PERFORM.
 * More complex transitions require calls to TANGO:TX_PERFORM; these
 * transitions are stored in the user's program and performed later;
 * alternately the transitions may be the result of functions like
 * TANGO:TX_ITERATE, TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_TX_PERFORM()
{
  LVAL lval_trans;
  TANGO_TRANS trans;

  trans = Xtango_Get_Trans_Arg_Returning_Validated_TANGO_TRANS(&lval_trans);
  xllastarg();

  Xtango_Save_Set_Context(get_tangotrans_context(lval_trans));
  Xtango_Check_Initialized();

  TANGOtrans_perform(trans);

  Xtango_Restore_Context();

  return (NIL);
}


/******************************************************************************
 * (TANGO:TX_FREE <tango_trans>)
 *	==> returns NIL.
 *
 * This routine frees the space associated with <tango_trans>. Calling this
 * routine is not absolutely necessary, since WINTERP's garbage collector will
 * automatically free up any TANGO_TRANS data. However, if you are creating
 * many transitions which are briefly used, then discarded, you may be able to
 * prevent un-necessary memory-size growth by explicitly freeing TANGO_TRANS
 * objects -- in the worst case, where the only "garbage" being generated is
 * TANGO_TRANS objects, you may end up allocating 2000 (default) nodes before
 * any of the space gets freed by the garbage collector. The XLISP ALLOC
 * function allows the 2000 default to be changed.
 *
 * The <tango_trans> argument is of type TANGO_TRANS, which is produced 
 * by one of the :TX_* methods on TANGO:IMAGE_CLASS and it's subclasses.
 *
 * ----------------------------------------------------------------------------
 * TANGOtrans_free (num,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) 
 *    int num;
 *    TANGO_TRANS t1,t2,t3,t4,t5,t6,t7,t8,t9,t10;
 ******************************************************************************/
LVAL Xtango_Prim_TANGO_TX_FREE()
{
  LVAL lval_trans;
  TANGO_TRANS trans;

  trans	= Xtango_Get_Trans_Arg_Returning_Validated_TANGO_TRANS(&lval_trans); /* get <tango_trans> */
  xllastarg();

  Xtango_Save_Set_Context(get_tangotrans_context(lval_trans));
  Xtango_Check_Initialized();

  Xtango_Context_Remove_TANGO_TRANS(lval_trans);

  Xtango_Restore_Context();

  return (NIL);
}


/******************************************************************************
 * (send <t-image-obj> :TX_MOVE [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 *
 * The MOVE transition moves the given <t-image-obj> along the given <path> (see
 * below).  The first movement of the image corresponds to the first relative
 * offset in the path. All these relative offsets are with respect to the
 * image's previous position on the screen.
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TX_MOVE()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_MOVE, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <t-image-obj> :TX_VISIBLE [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 *
 * The VISIBLE transition switches  the visibility of the given <t-image-obj>
 * for each offset in the given <path>.  At each offset in the path, if the 
 * image is visible, it will become invisible, and vice-versa. Note that the
 * actual <x>,<y> offsets in the path are ignored for this transition, therefore
 * the optional <path> argument could be ommitted (to create a "null" path of
 * length 1) or a FIXNUM (to create a "null" path of length of the FIXNUM).
 * 
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TX_VISIBLE()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_VISIBLE, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <t-image-obj> :TX_COLOR [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 *
 * The COLOR transition changes the given <t-image-obj> to the color indicated
 * by the given <path>.  See the routine TANGO:PATH_COLOR to create a special
 * path which will change the image to a certain color. Using an arbitrary
 * TANGO_PATH type argument, or using one of the other alternatives for the
 * <path> argument list will generate undefined results.
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 *----------------------------------------------------------------------------- 
 * NOTE: Message :TX_COLOR applies to almost all subclasses of
 * TANGO:IMAGE_CLASS, with the exception of TANGO:BITMAP_IMAGE_CLASS, thus
 * can't add it to TANGO:IMAGE_CLASS metaclass...
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TX_COLOR()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_COLOR, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <t-image-obj> :TX_RAISE [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 * 
 * The RAISE transition brings the given <t-image-obj> to the viewing plane
 * closest to the viewer. The image's position is not changed, only its
 * relative ordering (top to bottom) with respect to other images. Note that
 * the actual <x>,<y> offsets in the path are ignored for this transition,
 * therefore the optional <path> argument could be ommitted (to create a
 * "null" path of length 1) or a FIXNUM (to create a "null" path of length of
 * the FIXNUM).
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TX_RAISE()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_RAISE, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <t-image-obj> :TX_LOWER [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 * 
 * The LOWER transition pushes the given <t-image-obj> to the viewing plane
 * farthest from the viewer.  The image's position is not changed, only its
 * relative ordering (top to bottom) with respect to other images.  It will
 * possibly be obscured by every other image. Note that the actual <x>,<y>
 * offsets in the path are ignored for this transition, therefore the optional
 * <path> argument could be ommitted (to create a "null" path of length 1) or
 * a FIXNUM (to create a "null" path of length of the FIXNUM).
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TX_LOWER()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_LOWER, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <t-image-obj> :TX_DELAY [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 *
 * The DELAY transition creates a transition which has a null frame for each
 * offset in the path.  By null frame, we mean that no action or modification
 * is performed.  This transition is helpful when combined with concatenation
 * in order to produce actions that commence at varying times.  The image used
 * in this transition does not matter.  Note that the actual <x>,<y> offsets
 * in the path are ignored for this transition, therefore the optional <path>
 * argument could be ommitted (to create a "null" path of length 1) or a
 * FIXNUM (to create a "null" path of length of the FIXNUM).
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TX_DELAY()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_DELAY, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <t-image-obj> :TX_REFRESH [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 * 
 * The REFRESH transition redraws the entire animation window, restoring it to
 * a pristine state.  This is useful when running TANGO in fast mode
 * (non-total frame by frame refresh) and you would like to clean up damage to
 * the screen.  A refresh is done for each offset in the path, i.e., there's
 * no need for more than one offset in the path.  The image used in this
 * transition does not matter. Note that the actual <x>,<y> offsets in the
 * path are ignored for this transition, therefore the optional <path>
 * argument could be ommitted (to create a "null" path of length 1) or a
 * FIXNUM (to create a "null" path of length of the FIXNUM).
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TX_REFRESH()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_REFRESH, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <t-image-obj> :TX_DELETE [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 *
 * The DELETE transition makes an image invisible (if not already) and removes
 * it from any possible later usage.  Any attempts to use this image as a
 * parameter in subsequent TANGO calls will result in error messages.  Also,
 * pre-existing transitions with this image as a parameter should not be
 * performed. Note that the actual <x>,<y> offsets in the path are ignored for
 * this transition, therefore the optional <path> argument may be ommitted, to
 * create a "null" path of length 1.
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TX_DELETE()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_DELETE, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <t-image-obj> :TX_ZOOM [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 * 
 * The ZOOM transition zooms the window in or out on the positional location
 * of the image argument <t-image-obj>.  The <x> and <y> components of a path
 * element #C(<x> <y>) determine how the zooming should occur.  They should be
 * between 0.0 and 1.0.  Values close to 1.0 will zoom in very tight.  Values
 * near 0.0 do a slow zoom.  To get a nice slow, smooth zoom in on an object,
 * use a path with quite a few offsets all of equal ``small'' offset values,
 * such as #C(0.1 0.1).  Positive offset values zoom the display in, negative
 * offset values pull the display back out.
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TX_ZOOM()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_ZOOM, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <t-image-obj> :TX_FILL [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 * 
 * The FILL transition changes the "fill" component of the given image
 * <t-image-obj>.  This works differently for different types of images.  For
 * instances of TANGO:RECTANGLE_IMAGE_CLASS, TANGO:CIRCLE_IMAGE_CLASS,
 * TANGO:ELLIPSE_IMAGE_CLASS, and TANGO:POLYGON_IMAGE_CLASS, this transition
 * alters the image's fill style value by the <x> value of the offsets in
 * <path>.  The <x> value is simply added to the existing fill value of
 * <t-image-obj>, the <y> value is ignored.  If the image's fill value goes
 * below 0.0, it is automatically set back to 0.0.  If it goes above 1.0, it
 * is set back to 1.0.  Actually, the full range of values between 0.0 and 1.0
 * is not available.  We currently implement 40 different fills that range
 * between the two extremes.
 * 
 * For instances of TANGO:LINE_IMAGE_CLASS, TANGO:POLYLINE_IMAGE_CLASS, and
 * TANGO:SPLINE_IMAGE_CLASS, the <x> value of each offset in <path> is added
 * to the line's <width> parameter value, and the <y> value is added to the
 * line's <style> parameter value (see the documentation accompanying the :NEW
 * method on the associated tango image classes for further information on
 * <width> and <style>).  Extreme values are reset to 0.0 and 1.0 as in
 * rectangles and circles.
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 *-----------------------------------------------------------------------------
 * NOTE: Message :TX_FILL applies to almost all subclasses of TANGO:IMAGE_CLASS,
 *  with the exception of TANGO:TEXT_IMAGE_CLASS and TANGO:BITMAP_IMAGE_CLASS,
 * thus can't add it to TANGO:IMAGE_CLASS metaclass...
 ******************************************************************************/
LVAL Tango_Image_Class_Method_TX_FILL()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_FILL, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <t-image-obj> :TX_RESIZE [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 * 
 * The RESIZE transition resizes the given <t-image-obj> along the given
 * <path>. The various types of images each have a ``method'' in which they 
 * are resized:
 * 
 * + TANGO:LINE_IMAGE_CLASS instances can have positive or negative sizes,
 * they are resized by altering the line's size for each offset in the path.
 * 
 * + TANGO:RECTANGLE_IMAGE_CLASS instances can have only positive sizes, so
 * resizing a rectangle corresponds to ``dragging'' the lower right corner of
 * the rectangle along the given path. If one of the rectangle's dimensions
 * would become negative, it is set to 0.
 * 
 * + TANGO:CIRCLE_IMAGE_CLASS instances are resized by modifying the circle's
 * radius by the amount given in the <x> component of each offset in the
 * <path>. The <y> component of <path> is ignored.
 * 
 * + TANGO:ELLIPSE_IMAGE_CLASS instances are resized by adding the <x> part of
 * <path> value to the ellipse's <x> radius and the <y> part of <path> to the
 * <y> radius.
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 *-----------------------------------------------------------------------------
 * NOTE: Message :TX_RESIZE applies only to LINE, RECTANGLE, CIRCLE, ELLIPSE,
 * and COMPOSITE image classes. For POLYLINE, POLYGON or SPLINE image classes
 * use messages :TX_RESIZE1 ... :TX_RESIZE7. BITMAP and TEXT images cannot
 * be resized.
 ******************************************************************************/
LVAL Tango_Non_Poly_Image_Class_Method_TX_RESIZE()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_RESIZE, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <poly-image> :TX_RESIZE1 [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 * ...
 * (send <poly-image> :TX_RESIZE7 [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 *
 * For <poly-image> instance of class TANGO:POLYLINE_IMAGE_CLASS,
 * TANGO:POLYGON_IMAGE_CLASS, or TANGO:SPLINE_IMAGE_CLASS, the transitions
 * RESIZE1-RESIZE7 modify the relative positions of the respective vertex
 * number, plus all others after it in numerical order, by the relative <x>
 * <y> offsets of <path>.  These transitions are useful, for example, with a
 * forward arrow polyline that has many of its edges compressed down to start.
 * They can subsequently be grown out in all different directions, one at a
 * time.
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 ******************************************************************************
 * NOTE: Methods :TX_RESIZE1 thru :TX_RESIZE7 apply only to POLYLINE, POLYGON,
 * SPLINE, and COMPOSITE image classes.
 ******************************************************************************/
LVAL Tango_Poly_Image_Class_Method_TX_RESIZE1()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_RESIZE1, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_RESIZE2()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_RESIZE2, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_RESIZE3()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_RESIZE3, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_RESIZE4()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_RESIZE4, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_RESIZE5()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_RESIZE5, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_RESIZE6()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_RESIZE6, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_RESIZE7()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_RESIZE7, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <poly-image> :TX_GRAB1 [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 * ...
 * (send <poly-image> :TX_GRAB7 [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 * 
 * For <poly-image> instance of class TANGO:POLYLINE_IMAGE_CLASS,
 * TANGO:POLYGON_IMAGE_CLASS, or TANGO:SPLINE_IMAGE_CLASS, the transitions
 * GRAB1-GRAB7 modify the relative position of a particular vertex on the
 * image (except the one denoting the image's position) by the relative
 * offsets of the path.  As opposed to RESIZE, the transitions GRAB1-GRAB7
 * alter only one particular vertex in the image's definition. Think of
 * grabbing that vertex and swinging it around while all the other points stay
 * anchored. 
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 ******************************************************************************
 * NOTE: Methods :TX_GRAB1 thru :TX_GRAB7 apply only to POLYLINE, POLYGON,
 * SPLINE, and COMPOSITE image classes.
 ******************************************************************************/
LVAL Tango_Poly_Image_Class_Method_TX_GRAB1()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_GRAB1, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_GRAB2()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_GRAB2, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_GRAB3()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_GRAB3, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_GRAB4()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_GRAB4, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_GRAB5()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_GRAB5, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_GRAB6()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_GRAB6, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}

LVAL Tango_Poly_Image_Class_Method_TX_GRAB7()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_GRAB7, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}


/******************************************************************************
 * (send <bitmap-image> :TX_SHUFFLE [:PERFORM] [<path>])
 *	==> returns <tango_trans> object, or NIL if :PERFORM keyword given
 *
 * The SHUFFLE transition affects only the TANGO:BITMAP_IMAGE_CLASS.  For
 * each offset in the <path> argument, the next 2-d bitmap in the image's
 * series will be displayed.  The series is considered circular, i.e.,
 * the successor of the last bitmap is the first bitmap.  This transition
 * is useful to get a certain effect such as a door opening.  To do this,
 * define a BITMAP image with a series of individual bitmaps showing the
 * various stages of the door opening.  Then run a SHUFFLE transition to
 * animate that occurrance. Note that the actual <x>,<y> offsets in the
 * path are ignored for this transition, therefore the optional <path>
 * argument could be ommitted (to create a "null" path of length 1) or a
 * FIXNUM (to create a "null" path of length of the FIXNUM).
 *
 * The optional :PERFORM keyword argument specifies that the transition is to
 * be performed immediately (see TANGO:TRANS_PERFORM). If :PERFORM is omitted,
 * then a <tango_trans>  object is returned (useful for storing- or
 * operating-on- complex transitions via TANGO:TX_ITERATE,
 * TANGO:TX_CONCATENATE, TANGO:TX_COMPOSE.
 *
 * The [<path>] argument is optional, and may be of multiple types:
 *
 *	* if the <path> argument is omitted, then a path of length one with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type TANGO_PATH, then that is used as the path
 *		for the transition to take. TANGO:PATH_CREATE, and other
 *		TANGO:PATH_* functions return such TANGO_PATH typed results;
 *	* else, if <path> is of type FIXNUM, then a path of length <path> with
 *		both x and y offsets set to 0.0 is used as the path for the
 *		transition (see TANGOpath_null() in doc/xtangodoc.tex);
 *	* else, if <path> is of type COMPLEX (e.g. #C(<x> <y>), then a path of
 *		length one is used with x and y offsets set to <x> and <y>.
 *	* else, if <path> is a sequence (LIST or ARRAY) of COMPLEX #C(<x> <y>),
 *		then a path of the same length as the sequence is created,
 *		with the x and y offsets set to the corresponding <x> <y>.
 * 
 * Note: all <path> arg types other than TANGO_PATH are used here for convenience
 * in creating transitions over simple, unique paths. "Simple cases"
 * constitute paths which don't need to be be re-used, or altered via calls to
 * TANGO:PATH_* functions.
 ******************************************************************************/
LVAL Tango_Bitmap_Image_Class_Method_TX_SHUFFLE()
{
  LVAL o_image;
  TANGO_IMAGE image;
  TANGO_TRANS trans;
  TANGO_PATH path;
  WINTERP_TANGO_CONTEXT contxt;
  Boolean tangopath_needs_freeing_p, perform_trans_p;

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  perform_trans_p = Xtango_Get_Optional_PERFORM_Kwd_Arg();
  path = Xtango_Get_TANGO_PATH_Args(&tangopath_needs_freeing_p);
  if (xlargc != 0) {		/* was xllastarg(), but need to free 'path' prior to signaling error */
    if (tangopath_needs_freeing_p)
      path_free(path);
    xltoomany();
  }

  contxt = get_tangoimageobj_context(o_image);
  Xtango_Save_Set_Context(contxt);
  Xtango_Check_Initialized();
  
  trans = TANGOtrans_create(TANGO_TRANS_TYPE_SHUFFLE, image, path);

  if (tangopath_needs_freeing_p)
    path_free(path);

  if (perform_trans_p) {
    TANGOtrans_perform(trans);
    transition_free(trans);	/* single arg version of TANGOtrans_free(...) */
  }

  Xtango_Restore_Context();

  return (perform_trans_p ? NIL : cv_tangotrans(trans, contxt));
}
