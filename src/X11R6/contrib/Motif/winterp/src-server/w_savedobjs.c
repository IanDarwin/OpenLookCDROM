/* -*-C-*-
********************************************************************************
*
* File:         w_savedobjs.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_savedobjs.c,v 2.5 1994/06/06 15:40:54 npm Exp $
* Description:  Hashtable of LVAL's to be protected against garbage coll.
* Author:       Niels Mayer
* Created:      Sun Sep 24 22:31:43 1989
* Modified:     Sun Jun  5 14:51:09 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_savedobjs.c,v 2.5 1994/06/06 15:40:54 npm Exp $";

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
#include <Xm/Xm.h>		/* Xm/Xm.h only needed for "winterp.h"*/
#include "winterp.h"

/*
  We put an initializer for LVAL v_savedobjs in xlsym:xlsinit(), and add
  this to the list of structures that gets marked by gc. v_savedobjs is a
  hashtable for storing LVAL's that need to persist across garbage
  collections.

  v_savedobjs is typically used for storing callback-objects,
  timeout-objects, etc.  These objects need to persist even though they are
  not directly referenced by any user-space global variables because a
  callback or timout may occur at any time and we must not allow the code or
  lexical environment associated with one of these delayed calls to be gc'd.

  v_savedobjs is used to store LVAL's. The hash function removes the bits
  from the LVAL address used for adressing within the LVAL (i.e. rightshift
  by the # of bits in an LVAL struct. Then we take that value modulo
  VSAVEDOBJS_SIZE to come up with the index into the hashtable.
*/

#define HASHTAB_ADDR_MASK 0xffL	/* value must be (2^x - 1) for any x */
#define HASHTAB_SIZE ((int) HASHTAB_ADDR_MASK + 1)
static int LVAL_ADDRESS_WIDTH;	/* must be initialized by Wso_Init(). */

/******************************************************************************
 * Given an LVAL, returns the hash index into v_savedobjs for that object.
 * This is done quite sleazily/simply by using the adress of the lisp object
 * right shifted by the number of bits used to index inside a LVAL structure
 * Then masked by the size of the power-of-2-sized hashtable. The result is
 * a value ranging from 0 to HASH_MASK.
 ******************************************************************************/
int Wso_Hash(object)
     LVAL object;
{
  unsigned long i;
  i = ((unsigned long) object >> LVAL_ADDRESS_WIDTH) & HASHTAB_ADDR_MASK;
  return ((int) i);
}


/******************************************************************************
 *
 ******************************************************************************/
Wso_Init()
{
  extern LVAL v_savedobjs;	/* xlglob.c */
  int i;
  LVAL sym;

  /* compute the number of bits used to index within an LVAL structure */
  i = sizeof(struct node);	/* size of an LVAL* in bytes */
  LVAL_ADDRESS_WIDTH = 1;
  while (i >= 2) {
    i /= 2;
    ++LVAL_ADDRESS_WIDTH;
  }

  sym = xlenter("*SAVED_OBJS*");
  v_savedobjs = newvector((unsigned) HASHTAB_SIZE);
  defconstant(sym, v_savedobjs); /* allow lisp access to v_savedobjs for debugging */
}
