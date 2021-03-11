/*
 * $RCSfile: hdshowany.c,v $
 *
 * (c) Copyright 1991-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

/***************************************************************
**
** INCLUDE FILES
**
***************************************************************/

#include	"Text.h"

/***************************************************************
**
** FUNCTION:	hd_showstr
**
** DESCRIPTION:	Shows a section of a line of text up to a
**		kern, track, or space.
**
** PARAMETERS:	start		start of text
**		end		end of text
**		show		pointer to show structure
**		spaceval	amount to space string from last
**		trackval 	amount to track chars in string
**
** RETURN:	None
**
***************************************************************/

static void hd_showstr(start, end, show, spaceval, trackval)
    int		start;
    int		end;
    ShowStruct	*show;
    float	spaceval;
    float	trackval;
{
    if (start == end) return;

    /*
    ** Decide whether to use show, ashow, widthshow or awidthshow
    */
    if (spaceval == 0.0) {
	if (trackval == 0.0) {
	    PSWshow((char *)((show->text)+start), (end-start));
	} else {
	    PSWashow(trackval, 0.0,  (char *)((show->text)+start), (end-start));
	}
    } else {
	if (trackval == 0.0) {
	    PSWwidthshow(spaceval, 0.0, 32,
			 (char *)((show->text)+start), (end-start));
	} else {
	    PSWawidthshow(spaceval, 0.0, 32, trackval,
			  0.0, (char *)((show->text)+start), (end-start));
	}
    }
}

/***************************************************************
**
** FUNCTION:	ShowAny
**
** DESCRIPTION:	Breaks the current line of text into blocks
**		of normally spaced text and sends them to
**		hd_showstr with the tracking, kerning, and
**		extra spacing neccessary
**
** PARAMETERS:	show	the showstruct.
**
** RETURN:	None
**
***************************************************************/
void ShowAny(show)
    ShowStruct	*show;
{
    int lastshow = 0;	/* index of first char in last "show" block */

    /* indexes into ShowStruct arrays */
    int textptr			= 0;	/* current character in show->text[] */
    int prkernptr		= 0;	/* pair kern */
    int absmovptr		= 0;	/* absolute moveto array index */
    int spaceadjptr		= 0;	/* space adjust for justification */
    int trackkernptr	= 0;	/* track kern */

    float spaceval		= 0.0;
    float trackval		= 0.0;

    /*
    ** Loop through all the characters on the text line
    */
    while (textptr < show->textlen) {
	/*
	** If no attributes, just increment show pointer and skip to loop end
	*/
	if (show->attr [textptr] == SA_NOATTR) {
	    textptr++;
	    continue;
	}

	/*
	** If some characters skipped over since last "show", show the
	** section of the text line up to the kern, track, or space
	*/
	if (lastshow != textptr) {
	    hd_showstr (lastshow, textptr, show, spaceval, trackval);
	} 

	/*
	** If absolute move attribute for character, "moveto" the point
	*/
	if (show->attr [textptr] & SA_ABSMOV) {
	    PSmoveto (show->absmov [absmovptr].x, show->absmov [absmovptr].y);
	    absmovptr++;
	}

	/*
	** If pair kern attribute for character, "rmoveto" the point
	*/
	if (show->attr [textptr] & SA_PRKERN) {
	    PSrmoveto(show->prkern[prkernptr++], 0.0);
	}

	/*
	** If track kern value change for character, set new track value
	*/
	if (show->attr [textptr] & SA_TRACKADJ) {
	    trackval = (show->trackkernlen
			? show->trackkern [trackkernptr++] : 0.0);
	}

	/*
	** If space adjustment value change for character, set new value
	*/
	if (show->attr [textptr] & SA_SPACEADJ) {
	    spaceval = (show->spaceadjlen
			? show->spaceadj [spaceadjptr++] : 0.0);
	}

	/*
	** Mark last show and increment show pointer
	*/
	lastshow = textptr++;
    }

    /*
    ** Show the remainder of the text line
    */
    hd_showstr (lastshow, textptr, show, spaceval, trackval);
} /* end ShowAny () */
