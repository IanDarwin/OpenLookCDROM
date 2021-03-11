/*
 * $RCSfile: text.c,v $
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

char *textstrings[ ] = {	
	"Mark Twain's The Prince and the Pauper\r",
	"The dull work went tediously on. Petitions were read, and",
	"proclamations, patents, and all manner of wordy, repetitious, and",
	"wearisome papers relating to the public business; and at last Tom",
	"sighed pathetically and murmured to himself, \252In what have I",
	"offended, that the good God should take me away from the fields",
	"and the free air and the sunshine to shut me up here and make me a",
	"king and afflict me so?\272 Then his poor muddled head nodded awhile",
	"and presently drooped to his shoulder, and the business of the",
	"empire came to a standstill for want of that august factor the",
	"ratifying power. Silence ensued around the slumbering child, and",
	"the sages of the realm ceased from their deliberations.\r",
	"The paragraph above has 29 kern pairs out of 724 character pairs.",
	"This paragraph has 78 kern pairs. UPPERCASE LETTERS OFTEN",
	"HAVE MORE KERNING PAIRS THAN LOWERCASE LETTERS.",
	"The kern pairs in Times-Roman for the letter A are AC AG AO AW",
	"AT AU AV AW Av Aw Ay and A\047. The kern pairs for the letter T",
	"are TA TO Ta Te Th Ti To Tr Tu Tw Ty T. T, T: T; T-. An example",
	"often used to illustrate kerning is the word AWAY.\r"
};

float	textxy[ ] [2]= {
	{200, 650},
	{130, 620},
	{130, 595},
	{130, 570},
	{130, 545},
	{130, 520},
	{130, 495},
	{130, 470}, 
	{130, 445}, 
	{130, 420}, 
	{130, 395}, 
	{130, 370}, 
	{130, 320}, 
	{130, 295}, 
	{130, 270}, 
	{130, 245}, 
	{130, 220}, 
	{130, 195}, 
	{130, 170}, 
};
