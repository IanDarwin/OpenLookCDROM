/*
 *	$Id: HZchList.c,v 3.0 1994/06/04 07:39:38 ygz Exp $
 */

/***********************************************************
Copyright 1992 by Yongguang Zhang.  All Rights Reserved

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
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/* HZchList.c		The Choice List management module
 *
 * A choice list is a list of pointers to HZ (phrase) candidates.
 * A continuous segment of the list is call the on-screen selection window.
 * It is a sliding window of the choice list, which is displayed in the
 * input area.  A user can move it forward (to the right) or backward
 * (to the left) using some keys.  The size of the selection window
 * depends on the width of the input area and the length of each choice.
 * When the selection window reaches the right end of the choice list,
 * cxterm tries to expend the choice list by getting new choices from
 * the current input context.  A choice list is exhausted if there
 * will be no more choice in the current input context.
 */

#include "HZinput.h"		/* X headers included here. */

static int HZclAddChoice();

/*
 * Initialize the Choice List  
 */
void HZclInit (cl)
    HZChoiceList *cl;
{
    cl->num_alloc = 0;
    cl->choices = (struct hz_choice *)NULL;	/* allocated upon request */
    HZclReset (cl);
}

/*
 * Reset the Choice List.  Make it empty but don't dealloc the space.
 */
void HZclReset (cl)
    HZChoiceList *cl;
{
    cl->numChoices = 0;
    cl->exhaust = False;
    cl->selPos = 0;
    cl->selNum = 0;
}

/*
 * Free the space occupied the Choice List
 */
void HZclCleanUp (cl)
    HZChoiceList *cl;
{
    if (cl->choices)
	free ((char *)(cl->choices));
}

/*
 * Pick the n-th selection in the on-screen selection window of the
 * choice list.  Return the number of HZ in the selected candidate.
 */
short int HZclSelect (cl, n, phzc)
     HZChoiceList *cl;		/* the choice list */
     int n;			/* the n-th selection (n >= 0) */
     XChar2b **phzc;		/* the return address of the HZ choice */
{
    if (cl->selNum == 0)		/* nothing to select */
	return (0);
    if (n >= cl->selNum)		/* no such selection */
	return (-1);
    *phzc = cl->choices[ cl->selPos + n ].hzc;
    return (cl->choices[ cl->selPos + n ].nhz);
}

/*
 * Make the selection window in the choice list, given the start position.
 * Try to get new choices from the input context and expand the list
 * when the selection window reach the right end of the list.
 *
 * cl->selPos   must be already set to the new start position.
 */
int HZclMakeSelection (cl, ia, ic)
    HZChoiceList *cl;
    HZInputArea *ia;
    HZInputContext *ic;
{
    int len;		/* length of the selection window in input area */

    len = 3;		/* 3: '<' ' ' before the list, and '>' after it */
    cl->selNum = 0;

    /* First, see if there are undisplay choices in the choice list */
    while (cl->selPos + cl->selNum < cl->numChoices) {
	len += SelWidthOnScr( cl->choices[ cl->selPos + cl->selNum ].nhz );
	if (OverflowSelStr(len,ia,cl))		/* selection window too wide */
	    return (cl->selNum);

	cl->selNum ++;
	if (cl->selNum >= ia->maxchoice)	/* too many choices */
	    return (cl->selNum);
    }
    if (cl->exhaust)
	return (cl->selNum);	/* no more choice possible */

    /* Then, check the input context for possible new choices */
    while (1) {
	XChar2b *hzc;
	short int nhz;

	nhz = HZgetNextIC (ic, &hzc);
	if (nhz < 0) {
	    cl->exhaust = True;
	    break;
	}

	if (HZclAddChoice(cl, hzc, nhz) < 0)
	    return (cl->selNum);
	len += SelWidthOnScr(nhz);
	if (OverflowSelStr(len,ia,cl))		/* window too wide */
	    break;

	cl->selNum ++;
	if (cl->selNum >= ia->maxchoice)	/* too many choices */
	    break;
    }
    return (cl->selNum);
}

/*
 * Move the selection window forward in the choice list.
 */
int HZclMoveForward (cl, ia, ic)
    HZChoiceList *cl;
    HZInputArea *ia;
    HZInputContext *ic;
{
    int save_selPos = cl->selPos;
    int save_selNum = cl->selNum;

    cl->selPos += cl->selNum;
    if (HZclMakeSelection (cl, ia, ic) == 0) {
	/* no choice to the right, restore the old position of the window */
	cl->selPos = save_selPos;
	cl->selNum = save_selNum;
	return (0);
    } else
	return (cl->selNum);
}

/*
 * Move the selection window backward in the choice list.
 */
int HZclMoveBackward (cl, ia, ic)
    HZChoiceList *cl;
    HZInputArea *ia;
    HZInputContext *ic;
{
    int len = 0;

    if (cl->selPos == 0)	/* no choice to the left */
	return (0);

    cl->selNum = 0;
    while (1) {
	len += SelWidthOnScr (cl->choices[ cl->selPos - 1 ].nhz);
	if (OverflowSelStr(len,ia,cl))		/* too wide */
	    break;

	cl->selNum ++ ;
	cl->selPos -- ;

	if ((cl->selNum >= ia->maxchoice) || (cl->selPos <= 0))
		break;
    }
    return (cl->selNum);
}



/* add a HZ (phrase) candidate into the choice list */
static int HZclAddChoice (cl, hzc, nhz)
     HZChoiceList *cl;
     XChar2b *hzc;
     short int nhz;
{
#define	ALLOC_AMOUNT	256	/* allocated 256 choices each time */

    if (cl->choices == NULL) {
	cl->choices = (struct hz_choice *) calloc (ALLOC_AMOUNT,
				sizeof (struct hz_choice));
	if (! cl->choices) {
	    TScreen *screen = cxtermInput.screen;
	    HZiaShowMesg (screen, "No memory to expand the choice list");
	    return (-1);
	}
	cl->num_alloc = ALLOC_AMOUNT;
    } else if (cl->numChoices >= cl->num_alloc) {
	cl->choices = (struct hz_choice *) realloc ((char *)(cl->choices),
			(unsigned) ((cl->num_alloc + ALLOC_AMOUNT) *
				    sizeof (struct hz_choice)));
	if (! cl->choices) {
	    TScreen *screen = cxtermInput.screen;
	    HZiaShowMesg (screen, "No memory to expand the choice list");
	    return (-1);
	}
	cl->num_alloc += ALLOC_AMOUNT;
    }

    cl->choices[ cl->numChoices   ].hzc = hzc;
    cl->choices[ cl->numChoices++ ].nhz = nhz;
    return (0);

#undef	ALLOC_AMOUNT
}
