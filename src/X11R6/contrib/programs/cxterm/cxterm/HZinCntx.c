/*
 *	$Id: HZinCntx.c,v 3.0 1994/06/04 07:39:38 ygz Exp $
 */

/***********************************************************
Copyright 1992, 1994 by Yongguang Zhang.  All Rights Reserved.

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the names of the authors not
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

THE AUTHOR(S) DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN
NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
USE OR PERFORMANCE OF THIS SOFTWARE.

******************************************************************/

/* HZinCntx.c		The Input Context module
 * 
 * Input context is a data structure for incremental input conversion.
 */

#include "HZinput.h"

static int HZbuildIC();
static int HZmatchIC();
static HZInputContext *AllocIC(), *NewIC(), *DupIC();


HZInputContext *HZinputSearch (pinbuf, iblen, im, ic)
    char *pinbuf;		/* the input key-stroke buffer */
    int iblen;			/* length of the input key-stroke buffer */
    HZInputMethod *im;		/* the input method */
    HZInputContext *ic;		/* the input context */
{
  HZInputContext *new_ic;

    if (ic)
	new_ic = DupIC (ic);
    else
	new_ic = NewIC (ic_type_None, im);

    if (HZbuildIC (pinbuf, iblen, &new_ic, IC_PREFER) != IC_OK) {
	FreeIC (new_ic);
	return (NULL);
    }
    if (HZmatchIC (new_ic) != IC_OK) {
	FreeIC (new_ic);
	return (NULL);
    }

    return (new_ic);
}

/*
 * Search for associations
 */
HZInputContext *HZassocSearch (hzc, im, assocList)
    XChar2b *hzc;		/* the last input hanzi */
    HZInputMethod *im;		/* the input method */
    AssocList *assocList;	/* the association list */
{
  HZInputContext *ic;
  int offset = assocList->idxtbl[ (hzc->byte1 & 0x7f) * 256
				  + (unsigned char)(hzc->byte2) ];

    if (offset == 0)
	return(NULL);

    ic = NewIC (ic_type_Assoc, im);
    ic->aBeginList = assocList->phrases + offset;	/* match this one */
    ic->aPtrList = ic->aBeginList;
    return (ic);
}

/*
 * Build a input context tree, save the new input key(s).
 */
static int HZbuildIC (pinbuf, iblen, pic, prefer)
    char *pinbuf;		/* the input key-stroke buffer */
    int iblen;			/* length of the input key-stroke buffer */
    HZInputContext **pic;	/* the input context */
    HZicType prefer;		/* what kind of context you prefer */
{
  HZInputContext *ic = *pic;
  HZInputMethod *im = ic->im;

    /* normally there is only one input key each time, but we save
     * some room for possible non-incremental HZ input conversion in
     * future, e.g. batch conversions */

    if (iblen == 1) {
	switch (ic->type) {

	 case ic_type_None:		/* initial it has no type */
	 case ic_type_Assoc:		/* nothing to do with Assoc type */
	    ic->keysq = pinbuf;
	    ic->matched = 0;
	    ic->pending = 1;
	    switch (im->type) {
	     case im_type_Simple:
		ic->type = ic_type_Trie;
		ic->tHZtbl = im->im_hztbl;
		ic->tWcCntx = NULL;
	        return (IC_OK);
		break;
	    }
	    return (IC_FAIL);		/* otherwise: invalid type */
	    break;

	 case ic_type_Trie:
	    ic->pending++;
	    return (IC_OK);

	}
	return (IC_FAIL);		/* otherwise: invalid type */

    } /* iblen == 1 */

    /* iblen >= 1 */
    while (iblen >= 1) {
	if (HZbuildIC (pinbuf, 1, pic, prefer) != IC_OK)
	    return (IC_FAIL);
	iblen--;
	pinbuf++;
    }
    return (IC_OK);
}

/*
 * According to the new input key, check if it match at least one HZ choices.
 */
static int HZmatchIC (ic)
    HZInputContext *ic;	/* the input context */
{
    if (ic->pending == 0)
	return (IC_OK);		/* no new matching */

    switch (ic->type) {
     case ic_type_None:
	ic->matched += ic->pending;
	ic->pending = 0;
	return (IC_OK);

     case ic_type_Trie:
	return (Trie_Match (ic));

     case ic_type_Assoc:
	return (IC_OK);		/* always match */

    }
    return (IC_FAIL);
}

/*
 * Reset the input context, so that in the next call of HZgetNextIC(),
 * the search will restart from beginning and return the first candidate.
 */
int HZrestartIC (ic)
    HZInputContext *ic;	/* the input context */
{
    if (ic == NULL)
	return (0);

    switch (ic->type) {
     case ic_type_Trie:
	return Trie_Restart(ic);

     case ic_type_Assoc:
	ic->aPtrList = ic->aBeginList;
	return (-1);	/* I don't know how many associations */

    }
    return (0);
}

/*
 * Try to find next candidate.  Return the number of HZ in the candidate.
 */
short int HZgetNextIC (ic, phzc)
    HZInputContext *ic;
    XChar2b **phzc;		/* return address of the HZ (phrase) choice */
{
    if (ic == NULL)
	return (-1);

    switch (ic->type) {
     case ic_type_Trie:
	return (Trie_GetNext (ic, phzc));

     case ic_type_Assoc:
	if (ic->aPtrList->byte1 == '\0') {
	    return (-1);	/* no more association */
	} else if (ic->aPtrList->byte1 == HZ_PHRASE_TAG) {
	    short int nhz = ic->aPtrList->byte2;

	    *phzc = ++(ic->aPtrList);
	    ic->aPtrList += nhz;
	    return (nhz);
	} else {
	    *phzc = ic->aPtrList++;
	    return (1);
	}
	break;

    }
    return (-1);
}


/********************* IC management subroutines *********************/

/* we have a pool of input contexts ready */

#define	MAX_IC_POOL	256

static HZInputContext ic_pool [MAX_IC_POOL];
static HZInputContext *ic_free;

void InitICpool ()
{
  int i;
    for (i = 0; i < MAX_IC_POOL-1; i++)
	ic_pool[i].next_free = &ic_pool[i+1];
    ic_pool[MAX_IC_POOL-1].next_free = NULL;
    ic_free = ic_pool;
}

static HZInputContext *AllocIC ()
{
  HZInputContext *ic;

    if (! ic_free)
	return ((HZInputContext *) NULL);
    ic = ic_free;
    ic_free = ic_free->next_free;
    return (ic);
}

void FreeIC (ic)
    HZInputContext *ic;
{
    if (ic->type == ic_type_Trie)
	if (ic->tWcCntx)
	    XtFree((char *)(ic->tWcCntx));
    ic->type = ic_type_None;
    ic->next_free = ic_free;
    ic_free = ic;
}

/* allocate an input context and set the initial values */
static HZInputContext *NewIC (type, im)
    HZicType type;
    HZInputMethod *im;
{
  HZInputContext *ic = AllocIC ();

    if (!ic)
	return (NULL);
    ic->type = type;
    ic->im = im;
    ic->keysq = NULL;
    ic->matched = 0;
    ic->pending = 0;
    if (type == ic_type_Trie)
	ic->tWcCntx = NULL;
    return (ic);
}

/* deep copy of an input context tree */
static HZInputContext *DupIC (ic)
    HZInputContext *ic;
{
  HZInputContext *nic, *new_next_free;

    if (! ic)
	return (NULL);
    nic = AllocIC ();

    /* The field "next_free" cannot be changed!	*/
    /* Save it before memmove and restore after.	*/
    new_next_free = nic->next_free;
    memmove (nic, ic, sizeof(HZInputContext));
    nic->next_free = new_next_free;

    if (ic->type == ic_type_Trie) {
	if (ic->tWcCntx) {
	    nic->tWcCntx = XtNew(HZwildcardContext);
	    memmove(nic->tWcCntx, ic->tWcCntx, sizeof(HZwildcardContext));
	}
    }
    return (nic);
}
