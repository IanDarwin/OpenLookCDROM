/*
 *	$Id: HZtrie.c,v 3.0 1994/06/04 07:39:38 ygz Exp $
 */

/***********************************************************
Copyright 1991,1992,1994 by Yongguang Zhang.  All Rights Reserved.

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of the authors not
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

THE AUTHOR(S) DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
THE AUTHOR(S) BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

******************************************************************/

/*
 * Trie matching routines
 */

#include "HZinput.h"

static short int NextChoice ();
static int Trie_wcStartSearch ();
static int Trie_wcSearch ();
static int Trie_wcForward();
static int WildcardMatch ();


int Trie_Match (ic)
    HZInputContext *ic;
{
  HZinputTable *hztbl = ic->tHZtbl;
  char *kptr = ic->keysq + ic->matched;

    if (ic->matched == 0)
	ic->tCurTNptr = hztbl->trieList;

    while (ic->pending > 0) {
	int j;
	trieNode *tnptr;
	Boolean match;

	/* check the wildcards first */
	if (ic->tWcCntx || (hztbl->keytype[*kptr] & HZ_KEY_WILD_MASK)) {

	    if (! ic->tWcCntx)
		ic->tWcCntx = XtNew(HZwildcardContext);

	    strncpy(ic->tWcCntx->wildcard, kptr, ic->pending);
	    ic->tWcCntx->wildcard[ ic->pending ] = '\0';

	    if (Trie_wcStartSearch (ic->tCurTNptr, ic)) {
		/* at least one hit */
		/* not to change ic->matched and ic->pending */
		/* always start from here for further wildcard matching */
		return (IC_OK);
	    } else
		return (IC_FAIL);		/*  failed to match any node */

	    break;
	}

	match = False;
	tnptr = &(hztbl->trieList[ic->tCurTNptr->tn_nextKeys]);  j = 0;
	while (j < ic->tCurTNptr->tn_numNextKeys) {
	    if (*kptr == tnptr->tn_key) {
		match = True;
		break;
	    }
	    j++;  tnptr++;
	}
	if (! match) {	/* fail to match a valid keystroke sequence */
	    return (IC_FAIL);
	}

	/* now we got it */
	ic->tCurTNptr = tnptr;

	ic->pending--;  ic->matched++;  kptr++;
    }
    ic->tCurHZptr = &(ic->tHZtbl->hzList[ic->tCurTNptr->tn_hzidx]);
    ic->totalChoice = ic->tCurTNptr->tn_numHZchoice;
    ic->availChoice = 0;
    return (IC_OK);
}

int Trie_Restart (ic)
    HZInputContext *ic;
{
    if (! ic->tWcCntx) {			/* normal case */
	ic->tCurHZptr = &(ic->tHZtbl->hzList[ic->tCurTNptr->tn_hzidx]);
	ic->totalChoice = ic->tCurTNptr->tn_numHZchoice;
	ic->availChoice = 0;
	return (ic->totalChoice);
    } else {					/* with wildcard */
	(void) Trie_wcStartSearch (ic->tCurTNptr, ic);
	return (-1);		/* I don't know how many choices */
    }
}

short int Trie_GetNext (ic, phzc)
    HZInputContext *ic;
    XChar2b **phzc;
{
    if (! ic->tWcCntx) {	/* the normal case */
	return NextChoice (&(ic->availChoice), ic->totalChoice,
			   &(ic->tCurHZptr), phzc);
    } else {
	/* we got some wildcards here */
	while (1) {
	    short int nhz;

	    nhz = NextChoice (&(ic->availChoice), ic->totalChoice,
			      &(ic->tCurHZptr), phzc);
	    if (nhz >= 0)
		return nhz;

	    /* no more choice for this match, search for next one */
	    if (Trie_wcSearch(ic) == IC_FAIL)
		return (-1);
	}
	/*NOTREACHED*/
    }
    /*NOTREACHED*/
}

static short int NextChoice (pavailChoice, totalChoice, pcHZptr, phzc)
    int *pavailChoice;		/* call by reference */
    int totalChoice;
    XChar2b **pcHZptr;		/* call by reference */
    XChar2b **phzc;		/* call by reference */
{
#define availChoice	(*pavailChoice)
#define cHZptr		(*pcHZptr)

    if (availChoice < totalChoice) {
	short int nhz;

	availChoice++ ;
	if (cHZptr->byte1 == HZ_PHRASE_TAG) {	/* phrase! */
	    *phzc = cHZptr + 1;
	    nhz = cHZptr->byte2;
	    cHZptr += 1 + nhz;
	    return (nhz);
	} else {
	    *phzc = cHZptr++ ;
	    return (1);
	}
    } else
	return (-1);

#undef availChoice
#undef cHZptr
}

/*********************** Wildcard Search in Trie ***********************/

/*
 * Search in the trie for the given suffix in the input sequent
 * that contains wildcard(s).  The "top" node matches the longest
 * prefix in the input sequent that contains no wildcard.
 * The traversal is a Deep-First-Search starting from "top".
 * But it visits the node before visiting any child under the node.
 *
 * WildcardMatch() returns 3 values.  WILD_MATCH is for an exact match.
 * WILD_PREFIX means the current traversal path (from "top" to the current
 * node) matches a prefix of the wildcard pattern, which might suggest a
 * possible match if the traversal goes deeper.  WILD_UNMATCH is a total
 * mismatch, in which case the traversal should stop go any deeper.
 */

#define WILD_MATCH	0		/* exact match */
#define WILD_PREFIX	1		/* not match, but if go deeper ... */
#define WILD_UNMATCH	2		/* completely mismatch */

/*
 * Search the Trie under "top" from the very beginning
 */
static int Trie_wcStartSearch(top, ic)
    trieNode *top;
    HZInputContext *ic;
{
  HZwildcardContext *pwc = ic->tWcCntx;

    pwc->depth = 0;
    pwc->tNstack[0] = top;
    pwc->tNnumSb[0] = 0;
    bzero ((char *)(pwc->repcode), sizeof(pwc->repcode));
    return Trie_wcSearch(ic) ;
}

/*
 * Search the Trie from the leftover point
 */
static int Trie_wcSearch(ic)
    HZInputContext *ic;
{
  HZwildcardContext *pwc = ic->tWcCntx;
  HZinputTable *hztbl = ic->tHZtbl;

    if (! pwc->tNstack[0])
	return (IC_FAIL);	/* the traversal has been exhausted */

    while (1) {
	trieNode *tnptr;

	switch( WildcardMatch( pwc->repcode, pwc->wildcard, hztbl->keytype )) {

	  case WILD_MATCH:
	    /* Good.  The matching trieNode is at pwc->tNstack[pwc->depth] */

	    tnptr = pwc->tNstack[pwc->depth];
	    ic->tCurHZptr = &(hztbl->hzList[ tnptr->tn_hzidx ]);
	    ic->totalChoice = tnptr->tn_numHZchoice;
	    ic->availChoice = 0;

	    /* move to next step */
	    (void) Trie_wcForward(pwc);

	    return(IC_OK);

	  case WILD_PREFIX:
	    /* go down */

	    tnptr = pwc->tNstack[pwc->depth];
	    if (tnptr->tn_numNextKeys > 0) {
		trieNode *new_tnptr = &(hztbl->trieList[ tnptr->tn_nextKeys ]);

		pwc->depth++ ;
		pwc->tNnumSb[ pwc->depth ] = tnptr->tn_numNextKeys - 1;
		pwc->tNstack[ pwc->depth ] = new_tnptr;

		pwc->repcode[ pwc->depth - 1 ] = new_tnptr->tn_key;
		break;
	    }

	    /* No more additional key, hence no match for this node. */
	    /* Don't go down, move forward */

	    if ( Trie_wcForward(pwc) == IC_FAIL )
		return (IC_FAIL);
	    break;

	  case WILD_UNMATCH :

	    /* go to the next sibling */
	    if ( Trie_wcForward(pwc) == IC_FAIL )
		return (IC_FAIL);
	    break;
	}
    }
}

/*
 * Move to next sibling, if no more sibling, move up to parent's sibling
 */
static int Trie_wcForward(pwc)
    HZwildcardContext *pwc;
{
    while (pwc->tNnumSb[ pwc->depth ] == 0) {
	/* no more sibling, go up */
	if (pwc->depth == 0) {
	    /* now at the topmost; we've tried everything! */
	    pwc->tNstack[ 0 ] = NULL;
	    return(IC_FAIL);
	} else {
	    pwc->depth--;
	    pwc->repcode[pwc->depth] = '\0';
	}
    }
    pwc->tNnumSb[ pwc->depth ]-- ;
    pwc->tNstack[ pwc->depth ]++ ; 
    pwc->repcode[ pwc->depth-1 ] = pwc->tNstack[ pwc->depth ]->tn_key;
    return(IC_OK);
}

/********************** WILDCARD STRING MATCH ************************/

/* use global variable just save some time in the recursive match */
static unsigned short *wildkeytype;	/* for checking wildcards */
static int match();

/*
 * match string with pattern, under the given keytype definition
 */
static int WildcardMatch( string, pattern, keytype )
    char *string;
    char *pattern;
    unsigned short *keytype;
{
    wildkeytype = keytype;
    return ( match( string, pattern ) );
}


/*
 * recursively match string (with no wildcard) against pattern (with wildcard)
 */
static int match(string, pattern)
  char *string;
  char *pattern;
{
#define car(s)		(*(s))
#define cdr(s)		((s)+1)
#define empty(s)	(!(*(s)))

#define iswildcard(c)	(wildkeytype[c] == HZ_KEY_WILDCARD)
#define iswildchar(c)	(wildkeytype[c] == HZ_KEY_WILDCHAR)

    if (empty(pattern))
	return (empty(string) ? WILD_MATCH : WILD_UNMATCH);
    else
    if (iswildcard(car(pattern))) {
	int x = match(string, cdr(pattern));
	if (x == WILD_UNMATCH)
	    return (match(cdr(string), pattern));
	else
	    return x;
    } else
	if (empty(string))
	    return WILD_PREFIX;
        else if (iswildchar(car(pattern)) || car(pattern) == car(string))
	    return match(cdr(string), cdr(pattern));
	else
	    return WILD_UNMATCH;
}
