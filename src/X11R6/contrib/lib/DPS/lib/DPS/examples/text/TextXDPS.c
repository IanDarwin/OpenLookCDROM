/*
 * $RCSfile: TextXDPS.c,v $
 *
 * (c) Copyright 1992-1994 Adobe Systems Incorporated.
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

#include "Text.h"

/***************************************************************
**
** FUNCTION:    initBitmapWidths
**
** DESCRIPTION: Initialize the array of bitmap widths for each
**		font size for the named font.
**
** PARAMETERS:  font	-	name of the font to be used
**
** RETURN:      None.
**
***************************************************************/
static void initBitmapWidths(font)
    char *font;
{
    unsigned char *buf, *ch;
    int	num, c, size;


    /*
    ** Set up string of all characters
    */
    buf = (unsigned char *) XtMalloc(AppData.metrics.maxChar + 2);
    ch = buf;

    for (c = 1; c < AppData.metrics.maxChar+1; c++) *ch++ = c;

    *ch++ = '\0';
    size = strlen((char *) buf);

    /*
    ** Loop through the different font sizes
    */
    for (num = 0; num < NUM_SIZES; num++) {
	/*
	** Set the font size
	*/
	PSselectfont(font, (float) FontSizes[num]);

	/*
	** Allocate space for the bitmap widths array for this font size
	*/
	AppData.metrics.bitmapWidths[num] =
		(float *) XtCalloc (size, sizeof (float));
	AppData.metrics.bitmapWidths[num][0] = 0;

	PSWGetStringWidth((char *) buf, size,
			  &AppData.metrics.bitmapWidths[num][1]);
    } /* end for loop */

    XtFree((XtPointer) buf);
} /* end initBitmapWidths() */

/***************************************************************
**
** FUNCTION:	initBuffer
**
** DESCRIPTION:	Creates the buffer 
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

static void initBuffer()
{	
    Display *dpy = XtDisplay(AppData.drawingArea);
    Window win = XtWindow(AppData.drawingArea);
    int depth;

    XtVaGetValues(AppData.drawingArea, XtNdepth, &depth, NULL);

    /*
    ** Create pixmap buffer
    */
    AppData.buf = XCreatePixmap(dpy, win,
		AppData.drawingWidth, AppData.drawingHeight, depth);

    /*
    ** Clear pixmap
    */
    XFillRectangle(dpy, AppData.buf, AppData.gc, 0, 0,
		   AppData.drawingWidth, AppData.drawingHeight);

    (void) XDPSSetContextDrawable(AppData.dpsCtxt,
				  AppData.buf, AppData.drawingHeight);
} /* end initBuffer() */

/***************************************************************
**
** FUNCTION:	initDPSContext
**
** DESCRIPTION:	Handle post-Realize initialization: 
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

void initDPSContext()
{
    Display *dpy = XtDisplay(AppData.drawingArea);
    Point point;
    XPoint xpoint;
    float width, height;
    int i;

    /*
    ** Get height and width of drawing window
    */
    XtVaGetValues(AppData.drawingArea, XtNheight, &AppData.drawingHeight,
		  XtNwidth, &AppData.drawingWidth, NULL);

    /*
    ** Create the DPSContext in which rendering will occur
    */
    AppData.dpsCtxt = XDPSGetSharedContext(dpy);
    (void) XDPSSetEventDelivery(dpy, dps_event_pass_through);

    if (AppData.dpsCtxt == NULL) {
	printf("Couldn't create a Display PostScript context.\n");
	exit(1);
    }

    if (XDPSSetContextDrawable(AppData.dpsCtxt, XtWindow(AppData.drawingArea),
                               AppData.drawingHeight) != dps_status_success) {
        printf ("Couldn't set Display PostScript context drawable.\n");
        exit (1);
    }
    XDPSChainTextContext (AppData.dpsCtxt, AppData.trace);

    /*
    ** Set the default DPSContext
    */
    DPSSetContext(AppData.dpsCtxt);

    /*
    ** Get the transformation matrices
    */
    PSWGetTransform(AppData.ctm, AppData.invctm,
		    &AppData.xOffset, &AppData.yOffset);
    for (i = 0; i < 6; i++) AppData.origInvctm[i] = AppData.invctm[i];

    xpoint.x = AppData.drawingWidth;
    xpoint.y = 0;
    convertToDPS(&xpoint, &point);
    width = point.x;
    height = point.y;

    /*
    ** Intialize the buffer
    */
    initBuffer();

    /*
    ** Compute how large a page would be needed to draw the whole thing
    */

    AppData.scaledWidth = PAGE_WIDTH * (float) AppData.drawingWidth / width;
    AppData.scaledHeight = PAGE_HEIGHT * (float) AppData.drawingHeight / height;

    /*
    ** Position the drawing area so the center is in the center of the window
    */
    positionDrawingArea(PAGE_WIDTH / 2, PAGE_HEIGHT / 2,
			AppData.drawingWidth / 2, AppData.drawingHeight / 2);

    /*
    ** Save the initial cache parameters
    */
    PSWCurrentcacheparams(&AppData.size, &AppData.lower, &AppData.upper);

    /*
    ** Screen font uses bitmap font when possible with bitmap widths
    */
    PSWCopyFont(FONT_BASE, FONT_SCREEN, True, True, 1, 1, 0);

    /*
    ** Printer font uses bitmap font when possible with outline widths
    */
    PSWCopyFont(FONT_BASE, FONT_PRINTER, False, True, 1, 1, 0);

    /*
    ** Outline font uses outline characters and widths
    */
    PSWCopyFont(FONT_BASE, FONT_OUTLINE, False, True, 0, 0, 0);

    /*
    ** Create copies without UniqueID.  These will not evaluate to
    ** the same fonts when determining cache hits, so they will
    ** never be found in the cache.  They also never will use bitmaps.
    */
    PSWCopyFont(FONT_BASE, FONT_SCREEN_UNCACHED, True, False, 1, 1, 0);
    PSWCopyFont(FONT_BASE, FONT_PRINTER_UNCACHED, False, False, 1, 1, 0);
    PSWCopyFont(FONT_BASE, FONT_OUTLINE_UNCACHED, False, False, 0, 0, 0);

    /*
    ** Initialize the bitmap width arrays for the screen font
    */
    initBitmapWidths(FONT_SCREEN);
} /* end initDPSContexts() */

/***************************************************************
**
** FUNCTION:	fillTrackWidths
**
** DESCRIPTION:	Fills the array or showstruct with the tracking
**		spaces between the characters in the string.
**
** PARAMETERS:	str	the string (displayable line)
**		array	array to hold track spaces (may be NULL)
**		s	a showstruct (may be NULL)
**		value	tracking value for all chars
**
** RETURN:	total amount of tracking width
**
***************************************************************/
static float fillTrackWidths (str, array, s, value)
    char	*str;
    float	*array;
    ShowStruct	*s;
    float	value;
{
    int i, len;
    float trackwidth = 0.0;

    if (str && value) {
	len = strlen (str);

	if (array) {
	    /*
	    ** Add tracking width to each character space in array
	    */
	    for (i = 0; i < len; i++) array[i] += value;
	} else if (s) {
	    /*
	    ** Add tracking width to each character space in show structure
	    */
	    AddTracking (s, 0, value);
	}

	trackwidth = len * value;
    }
    return trackwidth;
} /* end fillTrackWidths () */

/***************************************************************
**
** FUNCTION:	fillSpaceWidths
**
** DESCRIPTION:	Increases the width of a space character in the
**		array or showstruct.
**
** PARAMETERS:	str	the string (displayable line)
**		array	array of char widths (may be NULL)
**		s	a showstruct (may be NULL)
**		value	extra width for a space char
**
** RETURN:	None
**
***************************************************************/
static void fillSpaceWidths (str, array, s, value)
    char	*str;
    float	*array;
    ShowStruct	*s;
    float	value;
{
    int i, len;

    if (str && value) {
	if (array) {
	    len = strlen (str);

	    /*
	    ** Add extra width to each space character in array
	    */
	    for (i = 0; i < len; i++) {
		if (str[i] == ' ') array[i] += value;
	    }
	} else if (s) {
	    /*
	    ** Add extra width to each space character in show structure
	    */
	    AddSpaceAdj(s, 0, value);
	}
    }
} /* end fillSpaceWidths () */

/***************************************************************
**
** FUNCTION:	fillStringWidths
**
** DESCRIPTION:	If an array is passed in then place the
**		character width for each character in string
**		into it (used when displaying with xshow). Also
**		keep track of the total width and the number of
**		spaces and return the average additional width
**		of a space to be added to space characters
**		(used when displaying with full justification).
**
** PARAMETERS:	str		string (displayable line)
**		array		array to hold the char widths
**		kernwidth	total kern space on the line
**		trackwidth	total tracking width on the line
**
** RETURN:	average additional width to be added to space characters
**
***************************************************************/
static float fillStringWidths (str, array, kernwidth, trackwidth)
    char	*str;
    float	*array;
    float	kernwidth;
    float	trackwidth;
{
    Boolean	nospacing = False;
    int		i, len, spaces;
    float	value, linewidth;

    /*
    ** Initialize space character count and line width
    */
    spaces = 0;
    linewidth = kernwidth + trackwidth;

    /*
    ** Check for displayable text line
    */
    if (str) {
	len = strlen(str);

	/*
	** Loop through for each character in the text line
	*/
	for (i = 0; i < len; i++) {
	    /*
	    ** Get the screen width (bitmap width from the server)
	    ** or the printer width (AFM font metrics from the AFM
	    ** file) as requested for the character
	    */
	    if (AppData.screen) {
		value = AppData.metrics.bitmapWidths[AppData.fontNum]
			                            [(unsigned char) str[i]];
	    } else {
		value = AppData.metrics.widths[(unsigned char) str[i]] *
			AppData.fontSize;
	    }
	    /*
	    ** Increment the line width by the character width
	    */
	    linewidth += value;

	    /*
	    ** Place the character width into the character width array
	    */
	    if (array) array[i] += value;

	    /*
	    ** Check for space, return, linefeed, etc.
	    */
			
	    if (isspace(str[i])) {
		if (str[i] == ' ') spaces++;
		else nospacing = True;
	    }
	}
    }

    /*
    ** Determine the average additional width for a space character
    ** for the line
    */
    if (spaces == 0) value = 0.0;
    else {
	if (trackwidth) value = (LINE_LENGTH_TR - linewidth) / spaces;
	else value = (LINE_LENGTH - linewidth) / spaces;

	if (nospacing && (value > 0)) value = 0.0;
    }

    return value;
} /* end fillStringWidths () */

/***************************************************************
**
** FUNCTION:	getKernValue
**
** DESCRIPTION:	Look up the kern pairs for the first character
**		and see if there is an entry for the second
**		character. Returns the value to kern if any.
**
** PARAMETERS:	char1	first char in the pair
**		char2	second char in the pair
**
** RETURN:	amount to kern the two chars
**
***************************************************************/
static float getKernValue (char1, char2)
    unsigned char char1;
    unsigned char char2;
{
    int i, kindex, klen;

    /*
    ** Initialize the kern pair index and array length
    */
    kindex = AppData.metrics.kernIndex[char1];
    klen   = AppData.metrics.numKernPairs[char1];

    /*
    ** Loop through the kern pair array looking for the second character
    */
    for (i = kindex; i < kindex + klen; i++) {
	/*
	** If a match is found, return the kern value
	*/
	if (AppData.metrics.kernPairs[i].code == char2) {
	    return AppData.metrics.kernPairs[i].dx;
	}
    }
    return 0.0;
} /* end getKernValue () */

/***************************************************************
**
** FUNCTION:	fillKernWidths
**
** DESCRIPTION:	Fills the array or showstruct with the amount
**		to kern character pairs in the string.
**
** PARAMETERS:	str		string (displayable line)
**		array		array to hold the kern values
**		s		showstruct
**		fontsize	effective font size
**		kerns		pointer to a kern counter
**
** RETURN:	total amount of kern space for the line
**
***************************************************************/
static float fillKernWidths (str, array, s, fontsize, kerns)
    char	*str;
    float	*array;
    ShowStruct	*s;
    float	fontsize;
    int		*kerns;
{
    int i, len;
    float value, kernwidth;

    /*
    ** Initialize the line kern width
    */
    kernwidth = 0.0;

    /*
    ** Check for displayable text line
    */
    if (str) {
	len = strlen(str) - 1;

	/*
	** Loop through for each character in the text line
	*/
	for (i = 0; i < len; i++) {
	    /*
	    ** Check to see if the current character and the next
	    ** have a kern pair value
	    */
	    if ((value = getKernValue((unsigned char) str[i],
				      (unsigned char) str[i+1])) != 0) {
		/*
		** Scale the kern value to the font size
		*/
		value = value * fontsize;

		if (array) {
		    /*
		    ** Add kern width to character space in array
		    */
		    array[i] += value;
		} else if (s) {
		    /*
		    ** Add kern width to character space in show structure
		    */
		    AddPairKern(s, i+1, value);
		}			
		/*
		** Increment the line kern width by the kern width
		*/
		kernwidth += value;
		(*kerns)++;
	    }
	}
    }

    return kernwidth;
} /* end fillKernWidths() */

/***************************************************************
**
** FUNCTION:	characterSpacing
**
** DESCRIPTION:	Calculate character positioning on the line.  Go
**		through the line and insert the appropriate
**		value into the array. The value is dependent on
**		whether kerning, tracking, and xshow are on.
**
** PARAMETERS:	str	string (displayable line)
**		array	array to hold the character widths
**		justify	to justify or not to justify
**		kern	to kern or not to kern
**		track	to track or not to track
**		xshow	to xshow or not to xshow
**
** RETURN:	number of kerning pairs on the line
**
***************************************************************/
static int  characterSpacing (str, array, justify, kern, track, xshow)
    char	*str;
    float	*array;
    Boolean	justify;
    Boolean	kern;
    Boolean	track;
    Boolean	xshow;
{
    int		kerns = 0;
    float	fontsize;
    float	kernwidth, spacewidth, trackwidth;
    register int i;

    /*
    ** Initialize character width array, fontsize, and width parameters.
    */
    for (i = 0; i < MAX_XSHOW; i++) array[i] = 0.0;

    fontsize = AppData.fontSize;

    kernwidth = spacewidth = trackwidth = 0.0;

    /*
    ** If kerning, then look up each pair in the AFM kerning array.
    */
    if (kern) kernwidth = fillKernWidths(str, array, NULL, fontsize, &kerns);

    /*
    ** If using tracking, then include the tracking width in the array.
    */
    if (track) trackwidth = fillTrackWidths(str, array, NULL, TRACKVAL);

    /*
    ** If using xshow, then include the character width in the array as
    ** well (xshow takes the displacement from the previously shown
    ** character hence the character width).
    */
    if (xshow) spacewidth = fillStringWidths(str, array, 
					     kernwidth, trackwidth);
	
    /*
    ** If justifying the line, determine the value necessary for spacing
    ** and insert it into the array.
    */
    if (justify) {
	if (!xshow) spacewidth = fillStringWidths(str, NULL, 
						  kernwidth, trackwidth);

	fillSpaceWidths(str, array, NULL, spacewidth);
    }

    return kerns;
} /* end characterSpacing() */

/***************************************************************
**
** FUNCTION:	showStructure
**
** DESCRIPTION:	Build the showStructure spacing for this line.
**		Go through the line and insert the appropriate
**		value into the array. The value is dependent on
**		whether kerning and tracking are turned on.
**
** PARAMETERS:	str	the string (displayable line)
**		s	the showstruct
**		justify	to justify or not to justify
**		kern	to kern or not to kern
**		track	to track or not to track
**
** RETURN:	number of kerning pairs on the line
**
***************************************************************/
static int  showStructure (str, s, justify, kern, track)
    char	*str;
    ShowStruct	*s;
    Boolean	justify;
    Boolean	kern;
    Boolean	track;
{
    int		kerns = 0;
    float	fontsize;
    float	kernwidth, spacewidth, trackwidth;

    /*
    ** Initialize fontsize and width parameters.
    */
    fontsize = AppData.fontSize;

    kernwidth = spacewidth = trackwidth = 0.0;

    /*
    ** If kerning, then look up each pair in the AFM kerning array.
    */
    if (kern) kernwidth = fillKernWidths(str, NULL, s, fontsize, &kerns);
	
    /*
    ** If using tracking, then include the tracking width in the array.
    */
    if (track) trackwidth = fillTrackWidths(str, NULL, s, TRACKVAL);

    /*
    ** If justifying the line, determine the value necessary for spacing
    ** and insert it into the array.
    */
    if (justify) {
	spacewidth = fillStringWidths(str, NULL, kernwidth, trackwidth);

	fillSpaceWidths(str, NULL, s, spacewidth);
    }

    return kerns;
} /* end showStructure() */

/***************************************************************
**
** FUNCTION:    markStartTime
**
** DESCRIPTION: routine to set the start time of the DPS drawing method
**
** PARAMETERS:
**	startTime - pointer to struct timeval where current time is stored
**
** RETURN:      None.
**
***************************************************************/
static void markStartTime (startTime)
    struct timeval *startTime;
{
    struct timezone timeZone;

    gettimeofday (startTime, &timeZone);
}

/***************************************************************
**
** FUNCTION:    getElapsedTime
**
** DESCRIPTION: Returns milliseconds since startTime
**
** PARAMETERS:
**	startTime - pointer to struct timeval where the start time is kept
**
** RETURN:
**	long - the elapsed time since the start in milliseconds
**
***************************************************************/
static long getElapsedTime (startTime)
    struct timeval *startTime;
{
    struct timezone timeZone;
    struct timeval finishTime;
    long elapsedSeconds, elapsedMicroseconds;

    gettimeofday (&finishTime, &timeZone);
    elapsedSeconds = finishTime.tv_sec - startTime->tv_sec;
    elapsedMicroseconds = finishTime.tv_usec - startTime->tv_usec;

    return ((long)(elapsedSeconds * 1000 + (elapsedMicroseconds/1000)));
}
/***************************************************************
**
** FUNCTION:	showText
**
** DESCRIPTION:	Set the font to outline or bitmap using either
**		screen widths or printer widths. Cycle through
**		the lines of text and draw it according to the
**		values set in the interface - kerning or no
**		kerning, screen widths or printer widths, xshow
**		or rmoveto/show, etc. 
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/
static void showText ()
{
    float		x;
    int			i, j, last;
    int			chars;
    char		*fontName;

    /*
    ** Set the font based on switch settings
    */
    if (AppData.issues & CACHE) {
	if (AppData.issues & OUTLINE) fontName = FONT_OUTLINE;
	else if (AppData.screen) fontName = FONT_SCREEN;
	else fontName = FONT_PRINTER;
    } else {
	if (AppData.issues & OUTLINE) fontName = FONT_OUTLINE_UNCACHED;
	else if (AppData.screen) fontName = FONT_SCREEN_UNCACHED;
	else fontName = FONT_PRINTER_UNCACHED;
    }

    PSselectfont(fontName, AppData.fontSize);

    /*
    ** Loop through the text one line at a time
    */
    for (i = 0; i < NUM_LINES; i++) {
	/*
	** Get the number of characters in the line of text
	*/
	chars = strlen (textstrings[i]);
	AppData.timingInfo.chars += chars;

	/*
	** Get the character spacing for 'xshow' and 'rmoveto/show' cases
	*/
	if (AppData.show == show_xshow || AppData.show == show_rmshow) {
	    AppData.timingInfo.kerns +=
		    characterSpacing(textstrings[i], AppData.charspace,
				     AppData.justify,
				     AppData.spacing & KERNING,
				     AppData.spacing & TRACKING,
				     (AppData.show == show_xshow));

	    /*
	    ** Move to starting position for the line
	    */
	    PSmoveto (textxy[i][0], textxy[i][1]);
	}

	if (AppData.show == show_xshow) {
	    /*
	    ** Display the text using 'xshow'
	    */
	    PSxshow (textstrings[i], AppData.charspace, chars);

	} else if (AppData.show == show_rmshow) {
	    /*
	    ** Display the text using 'rmoveto' and 'show'
	    **
	    ** Loop through the characters on the line collecting all
	    ** that can be drawn with one 'show' operator
	    */
	    for (j = 0, last = -1, x = 0.0; j < chars; j++) {
		if (AppData.charspace[j] != 0) {
		    if (x == 0.0) PSWshow(&textstrings[i][last+1], j - last);
		    else PSWrmovetoShow(x, &textstrings[i][last+1], j - last);

		    last = j;
		    x = AppData.charspace[j];
		}
	    }

	    /*
	    ** Draw any remaining characters on the line
	    */
	    if (x == 0.0) {
		if (j-last-1 > 0) {
		    PSWshow(&textstrings[i][last+1], j - last - 1);
		}
	    } else {
		if (j-last-1 > 0) {
		    PSWrmovetoShow(x, &textstrings[i][last+1], j - last -1);
		}
	    }

	/*
	** Display the text using 'show' variants
	*/
	} else {  /* (AppData.show == show_varshow) */
	    
	    /*
	    ** Reset the index fields of the showstruct to 0
	    */
	    ResetShowStruct(&AppData.s);

	    /*
	    ** Increases the size of the attribute array to accomodate
	    ** every character in string if necessary
	    */
	    AddString(&AppData.s, textstrings[i]);

	    /*
	    ** Force a moveto PS operator into the showstruct
	    */
	    AddMoveto(&AppData.s, 0, textxy[i][0], textxy[i][1]);

	    /*
	    ** Build the showStructure spacing for this line
	    */
	    AppData.timingInfo.kerns +=
		    showStructure(textstrings[i], &AppData.s, AppData.justify,
				  AppData.spacing & KERNING,
				  AppData.spacing & TRACKING);

	    /*
	    ** Draw the line as blocks of normally spaced text using
	    ** 'show' variants and 'moveto' for each block to accomodate
	    ** kerning, tracking, and extra spacing
	    */
	    ShowAny(&AppData.s);
	}
    }
} /* end showText () */

/***************************************************************
**
** FUNCTION:    drawSelf
**
** DESCRIPTION: Redraw the page with the current parameters
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/
void drawSelf ()
{
    int temp1, temp2;
    ShowType tempShow;
    struct timeval	startTime;

    eraseFields();
    AppData.timingInfo.time = 0;

    /*
    ** Set the wait cursor
    */
    if (AppData.waitCursor != None) {
	XDefineCursor(XtDisplay(AppData.drawingArea),
		      XtWindow(AppData.drawingArea), AppData.waitCursor);
	XDefineCursor(XtDisplay(AppData.optionBox),
		      XtWindow(AppData.optionBox), AppData.waitCursor);
    }

    /*
    ** Draw the frame around the page
    */
    PSWdrawFrame(0, 0, (int) PAGE_WIDTH, (int) PAGE_HEIGHT);

    /*
    ** If the spacing includes tracking, adjust the page origin
    */
    if (AppData.spacing & TRACKING) PStranslate(-45.0, 0.0);

    /*
    ** Comparison with and without kerning
    */
    if (AppData.comp == COMPARE_KERNS) {
	/*
	** Output text using kerning
	*/
        temp1 = AppData.spacing;
        AppData.spacing |= KERNING;
	showText();

	/*
	** Translate origin and output text without kerning
	*/
        AppData.spacing &= ~KERNING;
        PStranslate(0.0, -10.0);
	showText();
        AppData.spacing = temp1;
        PStranslate(0.0, 10.0);
	DPSWaitContext(AppData.dpsCtxt);

    /*
    ** Comparison of printer widths and screen widths.  Must use xshow
    ** to do this
    */
    } else if (AppData.comp == COMPARE_WIDTHS) {
	/*
	** Output text with printer character widths
	*/
        temp1 = AppData.screen;
	tempShow = AppData.show;
        AppData.screen = False;
	AppData.show = show_xshow;
	showText();

	/*
	** Translate origin and output text with screen character widths
	*/
        AppData.screen = True;
        PStranslate(0.0, -10.0);
	showText();
        AppData.screen = temp1;
	AppData.show = tempShow;
        PStranslate(0.0, 10.0);
	DPSWaitContext(AppData.dpsCtxt);

    /*
    ** Comparison of outline font and bitmap font
    */
    } else if (AppData.comp == COMPARE_BITMAPS) {
	/*
	** Output text with outlines
	*/
        temp1 = AppData.screen;
        temp2 = AppData.issues;
	tempShow = AppData.show;
	AppData.screen = False;
	AppData.issues |= OUTLINE;
	AppData.show = show_xshow;
	showText();

	/*
	** Translate origin and output text with bitmaps widths
	*/
        AppData.screen = True;
	AppData.issues &= ~OUTLINE;
        PStranslate(0.0, -10.0);
	showText();
        AppData.screen = temp1;
	AppData.issues = temp2;
	AppData.show = tempShow;
        PStranslate(0.0, 10.0);
	DPSWaitContext(AppData.dpsCtxt);

    /*
    ** Non-comparison text output.  This is the only time we show time,
    ** since it's the only time that's meaningful
    */
    } else {
	/*
	** Mark the start time for rendering
	*/
	DPSWaitContext(AppData.dpsCtxt);
	markStartTime(&startTime);

	showText();

	/*
	** Obtain the elapsed time of the rendering
	*/
	DPSWaitContext(AppData.dpsCtxt);
	AppData.timingInfo.time = getElapsedTime(&startTime);
    }
    /*
    ** If the spacing includes tracking, adjust the page origin
    */
    if (AppData.spacing & TRACKING) PStranslate(45.0, 0.0);

    /*
    ** Update the timing and status fields
    */
    displayFields();

    /*
    ** Reset cursor
    */
    if (AppData.waitCursor != None) {
	XUndefineCursor(XtDisplay(AppData.drawingArea),
			XtWindow(AppData.drawingArea));
	XUndefineCursor(XtDisplay(AppData.optionBox),
			XtWindow(AppData.optionBox));
    }			
} /* end drawSelf () */

/***************************************************************
**
** FUNCTION:	drawSelfAndUpdate
**
** DESCRIPTION:	Draw page, then copy the buffer into
**		the drawing area window
**
** PARAMETERS:	None
**
** RETURN:	None
**
***************************************************************/

void drawSelfAndUpdate()
{
    drawSelf();
    XCopyArea(XtDisplay(AppData.drawingArea), AppData.buf,
	      XtWindow(AppData.drawingArea), AppData.gc,
	      0, 0, AppData.drawingWidth, AppData.drawingHeight, 0, 0);
}

/***************************************************************
**
** FUNCTION:	convertToX
**
** DESCRIPTION:	Convert user space to X coordinates. 
**
** PARAMETERS:	pXPt 	points to the target XPoint struct;
**		pUPt	points to the target Point struct;
**
** RETURN:	None
**
***************************************************************/

void convertToX (pXPt, pUPt)
    XPoint		*pXPt;
    Point		*pUPt;
{
    pXPt->x = AppData.ctm[A_COEFF] * pUPt->x + AppData.ctm[C_COEFF] * pUPt->y +
	    AppData.ctm[TX_CONS] + AppData.xOffset;
    pXPt->y = AppData.ctm[B_COEFF] * pUPt->x + AppData.ctm[D_COEFF] * pUPt->y +
	    AppData.ctm[TY_CONS] + AppData.yOffset;
} /* end convertToX() */   

/***************************************************************
**
** FUNCTION:	convertToDPS
**
** DESCRIPTION:	Convert X coordinates to user space
**
** PARAMETERS:	pXPt	points to the target XPoint struct;
**		pUPt	points to the target Point struct;
**
** RETURN:	None
**
***************************************************************/
void convertToDPS(pXPt, pUPt)
    XPoint		*pXPt;
    Point		*pUPt;
{
    int ix, iy;

    ix = pXPt->x - AppData.xOffset;
    iy = pXPt->y - AppData.yOffset;

    pUPt->x = AppData.invctm[A_COEFF] * ix + AppData.invctm[C_COEFF] * iy +
	    AppData.invctm[TX_CONS];
    pUPt->y = AppData.invctm[B_COEFF] * ix + AppData.invctm[D_COEFF] * iy +
	    AppData.invctm[TY_CONS];
} /* end convertToDPS() */   

/***************************************************************
**
** FUNCTION:	convertToOrigDPS
**
** DESCRIPTION:	Convert X coordinates to user space using the original
**		transformation matrix
**
** PARAMETERS:	pXPt	points to the target XPoint struct;
**		pUPt	points to the target Point struct;
**
** RETURN:	None
**
***************************************************************/

void convertToOrigDPS(pXPt, pUPt)
    XPoint	*pXPt;
    Point	*pUPt;
{
    int ix, iy;

    ix = pXPt->x - AppData.xOffset;
    iy = pXPt->y - AppData.yOffset;

    pUPt->x = AppData.origInvctm[A_COEFF] * ix +
	    AppData.origInvctm[C_COEFF] * iy + AppData.origInvctm[TX_CONS];
    pUPt->y = AppData.origInvctm[B_COEFF] * ix +
	    AppData.origInvctm[D_COEFF] * iy + AppData.origInvctm[TY_CONS];
} /* end convertToOrigDPS() */
