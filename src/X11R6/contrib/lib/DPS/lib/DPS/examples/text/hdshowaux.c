/*
 * $RCSfile: hdshowaux.c,v $
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
** FUNCTION:	ResetShowStruct
**
** DESCRIPTION:	Resets the index and text fields of the show
**		structure to 0.
**
** PARAMETERS:	show	show structure
**
** RETURN:	None.
**
***************************************************************/
void ResetShowStruct (show)
    ShowStruct	*show;
{
    /*
    ** Initialize the text pointer
    */
    show->text = NULL;

    /*
    ** Initialize the character attribute array
    */
    memset ((void *) show->attr, 0, show->textmax * sizeof(unsigned int));

    /*
    ** Initialize the index fields
    */
    show->textlen      = 0;
    show->prkernlen    = 0;
    show->absmovlen    = 0;
    show->spaceadjlen  = 0;
    show->trackkernlen = 0;
}


/***************************************************************
**
** FUNCTION:	AllocShowStruct
**
** DESCRIPTION:	Allocates the initial space for the show
**		structure.
**
** PARAMETERS:	show	show structure
**
** RETURN:	None.
**
***************************************************************/
void AllocShowStruct (show)
    ShowStruct	*show;
{
    /*
    ** Set initial text string maximum size
    */
    show->textmax = 128;

    /*
    ** Set initial maximum array sizes
    */
    show->prkernmax = 16;
    show->absmovmax = 16;
    show->spaceadjmax = 16;
    show->trackkernmax = 16;

    /*
    ** Allocate text string attribute array of initial size
    */
    show->attr = (unsigned int *)
	    XtMalloc(show->textmax * sizeof(unsigned int));

    /*
    ** Allocate character spacing arrays of initial sizes
    */
    show->prkern = (float *) XtMalloc (show->prkernmax    * sizeof(float));
    show->absmov = (Point *) XtMalloc (show->absmovmax    * sizeof(Point));
    show->spaceadj = (float *) XtMalloc (show->spaceadjmax  * sizeof(float));
    show->trackkern = (float *) XtMalloc (show->trackkernmax * sizeof(float));

    ResetShowStruct(show);
}


/***************************************************************
**
** FUNCTION:	FreeShowStruct
**
** DESCRIPTION:	Frees the memory used by the show structure.
**
** PARAMETERS:	show	show structure
**
** RETURN:	None.
**
***************************************************************/
void FreeShowStruct (show)
    ShowStruct	*show;
{
    /*
    ** Free text string attribute array
    */
    XtFree((char *) show->attr);

    /*
    ** Free character spacing arrays
    */
    XtFree((char *) show->prkern);
    XtFree((char *) show->absmov);
    XtFree((char *) show->spaceadj);
    XtFree((char *) show->trackkern);
}


/***************************************************************
**
** FUNCTION:	AddString
**
** DESCRIPTION:	Increases the size of the attribute array
**		to accomodate every character in string if
**		necessary.
**
** PARAMETERS:	show	show structure
**		string	null terminated string
**
** RETURN:	None.
**
***************************************************************/
void AddString (show, string)
    ShowStruct	*show;
    char	*string;
{
    /*
    ** Set text sting length in show structure
    */
    show->textlen = strlen(string);

    /*
    ** Set text sting pointer in show structure
    */
    show->text = string;

    /*
    ** Check to see if text attribute array is large enough
    */
    if (show->textlen >= show->textmax) {
	/*
	** Double the size of the text attribute array
	*/
	show->textmax = show->textmax * 2;
	show->attr = (unsigned int *)
		XtRealloc ((char *) show->attr,
			   show->textmax * sizeof(unsigned int));
    }
}


/***************************************************************
**
** FUNCTION:	AddMoveto
**
** DESCRIPTION:	Forces a moveto PS operator into the show
**		structure.
**
** PARAMETERS:	show	show structure
**		index	character after the moveto
**		x	X coord of moveto (PS coords)
**		y	Y coord of moveto (PS coords)
**
** RETURN:	None.
**
***************************************************************/
void AddMoveto (show, index, x, y)
    ShowStruct *show;
    int index;
    float x;
    float y;
{
    /*
    ** Check to see if absolute moveto array is large enough
    */
    if (show->absmovlen >= show->absmovmax) {
	/*
	** Double the size of the absolute moveto array
	*/
	show->absmovmax = show->absmovmax * 2;
	show->absmov = (Point *) XtRealloc((char *) show->absmov,
					   show->absmovmax * sizeof(Point));
    }	

    /*
    ** Set the absolute moveto array parameters
    */
    show->attr[index] |= SA_ABSMOV;
    show->absmov[show->absmovlen].x = x;
    show->absmov[show->absmovlen].y = y;
    show->absmovlen++;
}

/***************************************************************
**
** FUNCTION:	AddPairKern
**
** DESCRIPTION:	Forces a kerning value into the show
**		structure at character index.
**
** PARAMETERS:	show	show structure
**		index	second char of kern pair
**		value	amount to kern
**
** RETURN:	None
**
***************************************************************/
void AddPairKern (show, index, value)
    ShowStruct *show;
    int index;
    float value;
{
    /*
    ** Check to see if pair kern array is large enough
    */
    if (show->prkernlen >= show->prkernmax) {
	/*
	** Double the size of the pair kern array
	*/
	show->prkernmax = show->prkernmax * 2;
	show->prkern = (float *) XtRealloc((char *) show->prkern,
					   show->prkernmax * sizeof(float));
    }	

    /*
    ** Set the pair kern array parameters
    */
    show->attr[index] |= SA_PRKERN;
    show->prkern[show->prkernlen] = value;
    show->prkernlen++;
}

/***************************************************************
**
** FUNCTION:	AddTracking
**
** DESCRIPTION:	Forces a tracing value into the show
**		structure after char at index.
**
** PARAMETERS:	show	show structure
**		index	character after track
**		value	amount to track
**
** RETURN:	None
**
***************************************************************/
void AddTracking (show, index, value)
    ShowStruct	*show;
    int		index;
    float	value;
{
    /*
    ** Check to see if track kern adjust array is large enough
    */
    if (show->trackkernlen >= show->trackkernmax) {
	/*
	** Double the size of the track kern adjust array
	*/
	show->trackkernmax = show->trackkernmax * 2;
	show->trackkern = (float *) XtRealloc((char *) show->trackkern,
					   show->trackkernmax * sizeof(float));
    }	

    /*
    ** Set the track kern adjust array parameters
    */
    show->attr[index] |= SA_TRACKADJ;
    show->trackkern[show->trackkernlen] = value;
    show->trackkernlen++;
}

/***************************************************************
**
** FUNCTION:	AddSpaceAdj
**
** DESCRIPTION:	Forces extra space into the show structure
**		after char at index for justification.
**
** PARAMETERS:	show	show structure
**		index	index of char after extra space
**		value	amount of extra space
**
** RETURN:	None
**
***************************************************************/
void AddSpaceAdj (show, index, value)
    ShowStruct	*show;
    int		index;
    float	value;
{
    /*
    ** Check to see if space adjust is large enough
    */
    if (show->spaceadjlen >= show->spaceadjmax) {
	/*
	** Double the size of the space adjust array
	*/
	show->spaceadjmax = show->spaceadjmax * 2;
	show->spaceadj = (float *) XtRealloc((char *) show->spaceadj,
					    show->spaceadjmax * sizeof(Point));
    }	

    /*
    ** Set the space adjust array parameters
    */
    show->attr[index] |= SA_SPACEADJ;
    show->spaceadj[show->spaceadjlen] = value;
    show->spaceadjlen++;
}
